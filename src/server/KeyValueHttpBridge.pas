unit KeyValueHttpBridge;

interface

uses
  System.Types, System.Classes, System.SysUtils, IdCustomHTTPServer,
  HttpServer, KeyValueStore, IdStack, System.SyncObjs, System.Generics.Collections,
  SynCommons, SeqLogger;

type
  TKeyValueHTTPBridge = class
  private
    FHTTPThread: THTTPServerThread;
    FKV: IKeyValueStore;
    FLongPollContexts: TThreadList;
    procedure HTTPGetHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const BodyVariant: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
    procedure HTTPPostHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const BodyVariant: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
    procedure AddLongPoll(LastId: Int64; const ClientId: string; var ResponseText: string; var ResponseCode: Integer);
    procedure HandleValueChanged(Sender: TObject; const Key: string; const Value: variant; const SourceId: string);
    procedure NotifyLongPoll;
    function ChangesToJSON(const Changes: TArray<TChangeRecord>): string;

    function GetHTTPServer: THTTPServer;
  public
    constructor Create(APort: Integer; AKV: IKeyValueStore = nil);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property KeyValueStore: IKeyValueStore read FKV;
    property HTTPServer: THTTPServer read GetHTTPServer;
  end;

implementation

uses
  System.DateUtils, System.JSON, System.NetEncoding;

type
  TLongPollContext = class
    LastId: Int64;
    WaitEvent: TEvent;
    RequestedAt: TDateTime;
    ClientId: string;
  end;

constructor TKeyValueHTTPBridge.Create(APort: Integer; AKV: IKeyValueStore = nil);
begin
  inherited Create;
  if AKV = nil then
    FKV := CreateDefaultKeyValueStore
  else
    FKV := AKV;
  FHTTPThread := THTTPServerThread.Create(APort);
  FHTTPThread.FHttpServer.OnGet := HTTPGetHandler;
  FHTTPThread.FHttpServer.OnPost := HTTPPostHandler;
  FLongPollContexts := TThreadList.Create;
  FKV.OnValueChanged := HandleValueChanged;
end;

destructor TKeyValueHTTPBridge.Destroy;
begin
  Stop; // Ensure server is stopped
  FLongPollContexts.Free;
  FHTTPThread.Free;
  inherited;
end;

function TKeyValueHTTPBridge.GetHTTPServer: THTTPServer;
begin
  Result := FHTTPThread.FHttpServer;
end;

procedure TKeyValueHTTPBridge.Start;
begin
  FHTTPThread.Start;
end;

procedure TKeyValueHTTPBridge.Stop;
var
  L: TList;
  i: Integer;
  Ctx: TLongPollContext;
begin
  // First notify all long polling clients to disconnect
  L := FLongPollContexts.LockList;
  try
    for i := 0 to L.Count-1 do
    begin
      Ctx := TLongPollContext(L[i]);
      Ctx.WaitEvent.SetEvent;
    end;
  finally
    FLongPollContexts.UnlockList;
  end;

  // Give clients a moment to disconnect
  Sleep(100);

  // Now stop the server thread
  if Assigned(FHTTPThread) then
  begin
    FHTTPThread.Terminate;
    FHTTPThread.WaitFor;
  end;
end;

// JSON encode array of changes
function TKeyValueHTTPBridge.ChangesToJSON(const Changes: TArray<TChangeRecord>): string;
var
  Doc: TDocVariantData;
  Change: TChangeRecord;
  Value: Variant;
begin
  Doc.InitArray([]);
  for Change in Changes do
  begin
    Value := Change.Value;
      Doc.AddItem(_Obj([
        'id', Change.ChangeId,
        'key', Change.Key,
        'value', Value,
        'timestamp', DateToISO8601(Change.Timestamp, False)
      ]));
  end;
  Result := string(Doc.ToJSON);
end;

procedure TKeyValueHTTPBridge.HTTPGetHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const BodyVariant: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
var
  Key: string;
  Value: Variant;
  LastId: Int64;
  Changes: TArray<TChangeRecord>;
  ClientId: string;
  Guid: TGUID;
  Doc: TDocVariantData;
  AllKeys: TArray<TPair<string, variant>>;
  EqPos: Integer;
  Path: string;
begin
  // Get client ID from query parameter or generate one if not present
  ClientId := Args.RequestInfo.Params.Values['clientId'];
  if ClientId = '' then
  begin
    CreateGUID(Guid);
    ClientId := GUIDToString(Guid);
  end;

  // Extract path without query parameters
  Path := URL;
  EqPos := Pos('?', Path);
  if EqPos > 0 then
    Path := Copy(Path, 1, EqPos - 1);

  TSeqLogger.Logger.Log(Information, 'HTTP GET request from client {ClientId}: {URL}', ['ClientId', ClientId, 'URL', URL]);

  // /values/all - get all key/value pairs
  if SameText(Path, '/values/all') then
  begin
    Doc.InitArray([]);
    FKV.GetAll(AllKeys);
    TSeqLogger.Logger.Log(Information, 'Client {ClientId} requested all keys ({Count} keys)', ['ClientId', ClientId, 'Count', Length(AllKeys)]);
    for var i := 0 to High(AllKeys) do
    begin
      Doc.AddItem(_Obj([
        'key', AllKeys[i].Key,
        'value', AllKeys[i].Value
      ]));
    end;
    ResponseText := string(Doc.ToJSON);
    ResponseCode := 200;
    Exit;
  end;

  // /changes/latest - get latest change ID
  if SameText(Path, '/changes/latest') then
  begin
    var LatestId := FKV.GetLatestChangeId;
    TSeqLogger.Logger.Log(Information, 'Client {ClientId} requested latest change ID: {LatestId}', ['ClientId', ClientId, 'LatestId', LatestId]);
    Doc.InitObject([
      'id', LatestId
    ]);
    ResponseText := string(Doc.ToJSON);
    ResponseCode := 200;
    Exit;
  end;

  // /values?key=foo - regular GET
  if SameText(Path, '/values') then
  begin
    Key := Args.RequestInfo.Params.Values['key'];
    if Key = '' then
    begin
      TSeqLogger.Logger.Log(Warning, 'Client {ClientId} GET request missing key parameter', ['ClientId', ClientId]);
      ResponseText := '{"error":"missing key"}';
      ResponseCode := 400;
      Exit;
    end;

    if FKV.GetValue(Key, Value) then
    begin
      TSeqLogger.Logger.Log(Information, 'Client {ClientId} read key {Key} = {Value}', ['ClientId', ClientId, 'Key', Key, 'Value', VariantSaveJSON(Value)]);
      Doc.InitObject([
        'key', Key,
        'value', Value
      ]);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 200;
    end
    else
    begin
      TSeqLogger.Logger.Log(Information, 'Client {ClientId} attempted to read non-existent key {Key}', ['ClientId', ClientId, 'Key', Key]);
      Doc.InitObject(['error', 'not found']);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 404;
    end;
    Exit;
  end;

  // /changes?since=<id>
  if SameText(Path, '/changes') then
  begin
    if not TryStrToInt64(Args.RequestInfo.Params.Values['since'], LastId) then
    begin
      TSeqLogger.Logger.Log(Warning, 'Client {ClientId} long poll request has invalid since parameter', ['ClientId', ClientId]);
      ResponseText := '{"error":"invalid since parameter"}';
      ResponseCode := 400;
      Exit;
    end;

    TSeqLogger.Logger.Log(Information, 'Client {ClientId} started long poll from ID {LastId}', ['ClientId', ClientId, 'LastId', LastId]);
    FKV.GetChangesSince(LastId, ClientId, Changes);
    if Length(Changes) > 0 then
    begin
      ResponseText := ChangesToJSON(Changes);
      ResponseCode := 200;
    end
    else
      AddLongPoll(LastId, ClientId, ResponseText, ResponseCode);
    Exit;
  end;

  Doc.InitObject(['error', 'not found']);
  ResponseText := string(Doc.ToJSON);
  ResponseCode := 404;
end;

procedure TKeyValueHTTPBridge.HTTPPostHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const BodyVariant: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
var
  Key: string;
  Value: Variant;
  ClientId: string;
  Doc: TDocVariantData;
  BatchDoc: TDocVariantData;
  i: Integer;
  Path: string;
  EqPos: Integer;
begin
  ClientId := Args.RequestInfo.Params.Values['clientId'];

  // Extract path without query parameters
  Path := URL;
  EqPos := Pos('?', Path);
  if EqPos > 0 then
    Path := Copy(Path, 1, EqPos - 1);

  TSeqLogger.Logger.Log(Information, 'HTTP POST request from client {ClientId}: {URL}', ['ClientId', ClientId, 'URL', URL]);

  if SameText(Path, '/values') then
  begin
    if (BodyVariant <> null) and (BodyVariant <> null) then
    begin
      Key := TDocVariantData(BodyVariant).S['key'];
      Value := TDocVariantData(BodyVariant).GetValueOrNull('value');
    end
    else
    begin
      Key := Args.RequestInfo.Params.Values['key'];
      Value := Args.RequestInfo.Params.Values['value'];
    end;

    if Key = '' then
    begin
      TSeqLogger.Logger.Log(Error, 'Client {ClientId}: key is missing', ['ClientId', ClientId]);
      Doc.InitObject(['error', 'missing key']);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 400;
      Exit;
    end;

    TSeqLogger.Logger.Log(Information, 'Client {ClientId} wrote key {Key} = {Value}', ['ClientId', ClientId, 'Key', Key, 'Value', VariantSaveJSON(Value)]);
    FKV.SetValue(Key, Value, ClientId);
    ResponseText := '{"status":"ok"}';
    ResponseCode := 200;
    Exit;
  end;

  if SameText(Path, '/values/batch') then
  begin
    BatchDoc.InitJSON(RawUTF8(Body));
    if BatchDoc.Kind = dvArray then
    begin
      for i := 0 to BatchDoc.Count-1 do
      begin
        Key := BatchDoc.Values[i].S['key'];
        Value := BatchDoc.Values[i].GetValueOrNull('value');
        
        if Key <> '' then
          FKV.SetValue(Key, Value, ClientId);
      end;
      Doc.InitObject(['status', 'ok']);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 200;
    end
    else
    begin
      Doc.InitObject(['error', 'invalid batch format']);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 400;
    end;
    Exit;
  end;

  Doc.InitObject(['error', 'not found']);
  ResponseText := string(Doc.ToJSON);
  ResponseCode := 404;
end;

procedure TKeyValueHTTPBridge.AddLongPoll(LastId: Int64; const ClientId: string; var ResponseText: string; var ResponseCode: Integer);
var
  Ctx: TLongPollContext;
  Changes: TArray<TChangeRecord>;
  L: TList;
  i: Integer;
begin
  // Clean up any expired contexts first
  L := FLongPollContexts.LockList;
  try
    for i := L.Count-1 downto 0 do
    begin
      Ctx := TLongPollContext(L[i]);
      if SecondsBetween(Now, Ctx.RequestedAt) > 30 then
      begin
        TSeqLogger.Logger.Log(Information, 'Removing expired long poll context for client {ClientId}', ['ClientId', Ctx.ClientId]);
        L.Delete(i);
        Ctx.WaitEvent.Free;
        Ctx.Free;
      end;
    end;
  finally
    FLongPollContexts.UnlockList;
  end;

  // Just before waiting, check again in case of race
  FKV.GetChangesSince(LastId, ClientId, Changes);
  if Length(Changes) > 0 then
  begin
    ResponseText := ChangesToJSON(Changes);
    ResponseCode := 200;
    Exit;
  end;

  Ctx := TLongPollContext.Create;
  Ctx.LastId := LastId;
  Ctx.ClientId := ClientId;
  Ctx.RequestedAt := Now;
  Ctx.WaitEvent := TEvent.Create(nil, True, False, '');
  FLongPollContexts.Add(Ctx);

  TSeqLogger.Logger.Log(Information, 'Added long poll context for client {ClientId} waiting for changes after ID {LastId}', 
    ['ClientId', ClientId, 'LastId', LastId]);

  // Wait for up to 30 seconds for change
  if Ctx.WaitEvent.WaitFor(30000) = wrSignaled then
  begin
    FKV.GetChangesSince(Ctx.LastId, Ctx.ClientId, Changes);
    if Length(Changes) > 0 then
    begin
      ResponseText := ChangesToJSON(Changes);
      ResponseCode := 200;
      TSeqLogger.Logger.Log(Information, 'Long poll for client {ClientId} returned {Count} changes', 
        ['ClientId', ClientId, 'Count', Length(Changes)]);
    end
    else
    begin
      ResponseText := '[]';
      ResponseCode := 204;
      TSeqLogger.Logger.Log(Information, 'Long poll for client {ClientId} signaled but no changes found', ['ClientId', ClientId]);
    end;
  end
  else
  begin
    ResponseText := '[]';
    ResponseCode := 204;
    TSeqLogger.Logger.Log(Information, 'Long poll for client {ClientId} timed out', ['ClientId', ClientId]);
  end;

  // Remove our context
  L := FLongPollContexts.LockList;
  try
    i := L.IndexOf(Ctx);
    if i >= 0 then
      L.Delete(i);
  finally
    FLongPollContexts.UnlockList;
  end;

  Ctx.WaitEvent.Free;
  Ctx.Free;
end;

// Called on any change (manual, HTTP, etc)
procedure TKeyValueHTTPBridge.HandleValueChanged(Sender: TObject; const Key: string; const Value: variant; const SourceId: string);
begin
  TSeqLogger.Logger.Log(Information, 'Value changed: key={Key} value={Value} source={Source}', 
    ['Key', Key, 'Value', VariantSaveJSON(Value), 'Source', SourceId]);
  NotifyLongPoll;
end;

procedure TKeyValueHTTPBridge.NotifyLongPoll;
var
  L: TList;
  i: Integer;
  Ctx: TLongPollContext;
  ToSignal: TList;
begin
  ToSignal := TList.Create;
  try
    // First, get the list of contexts to signal
    L := FLongPollContexts.LockList;
    try
      for i := 0 to L.Count-1 do
        ToSignal.Add(L[i]);
    finally
      FLongPollContexts.UnlockList;
    end;

    // Now signal each context
    for i := 0 to ToSignal.Count-1 do
    begin
      Ctx := TLongPollContext(ToSignal[i]);
      TSeqLogger.Logger.Log(Information, 'Notifying long poll client {ClientId}', ['ClientId', Ctx.ClientId]);
      Ctx.WaitEvent.SetEvent;
    end;
  finally
    ToSignal.Free;
  end;
end;

end.


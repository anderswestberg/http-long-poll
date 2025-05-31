unit KeyValueHTTPBridgeUnit;

interface

uses
  System.Types, System.Classes, System.SysUtils, IdCustomHTTPServer,
  HTTPServerUnit, KeyValueServerUnit, IdStack, System.SyncObjs, System.Generics.Collections,
  SynCommons, SeqloggerClass;

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
  AllKeys: TArray<string>;
  KeyValue: string;
  EqPos: Integer;
  Path: string;
begin
  // Get client ID from header or generate one if not present
  ClientId := Args.RequestInfo.RawHeaders.Values['X-Client-ID'];
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

  TSeqLogger.Logger.Log(Information, Format('HTTP GET request from client %s: %s', [ClientId, URL]));

  // /all - get all key/value pairs
  if SameText(Path, '/all') then
  begin
    Doc.InitArray([]);
    FKV.GetAll(AllKeys);
    TSeqLogger.Logger.Log(Information, Format('Client %s requested all keys (%d keys)', [ClientId, Length(AllKeys)]));
    for var i := 0 to High(AllKeys) do
    begin
      KeyValue := AllKeys[i];
      EqPos := Pos('=', KeyValue);
      if EqPos > 0 then
      begin
        Key := Copy(KeyValue, 1, EqPos - 1);
        if FKV.GetValue(Key, Value) then
          Doc.AddItem(_Obj([
            'key', Key,
            'value', Value
          ]));
      end;
    end;
    ResponseText := string(Doc.ToJSON);
    ResponseCode := 200;
    Exit;
  end;

  // /latest-change-id - get latest change ID
  if SameText(Path, '/latest-change-id') then
  begin
    var LatestId := FKV.GetLatestChangeId;
    TSeqLogger.Logger.Log(Information, Format('Client %s requested latest change ID: %d', [ClientId, LatestId]));
    Doc.InitObject([
      'id', LatestId
    ]);
    ResponseText := string(Doc.ToJSON);
    ResponseCode := 200;
    Exit;
  end;

  // /data?key=foo - regular GET
  if SameText(Path, '/data') then
  begin
    Key := Args.RequestInfo.Params.Values['key'];
    if Key = '' then
    begin
      TSeqLogger.Logger.Log(Warning, Format('Client %s GET request missing key parameter', [ClientId]));
      ResponseText := '{"error":"missing key"}';
      ResponseCode := 400;
      Exit;
    end;

    if FKV.GetValue(Key, Value) then
    begin
      TSeqLogger.Logger.Log(Information, Format('Client %s read key %s = %s', [ClientId, Key, VariantSaveJSON(Value)]));
      Doc.InitObject([
        'key', Key,
        'value', Value
      ]);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 200;
    end
    else
    begin
      TSeqLogger.Logger.Log(Information, Format('Client %s attempted to read non-existent key %s', [ClientId, Key]));
      Doc.InitObject(['error', 'not found']);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 404;
    end;
    Exit;
  end;

  // /longpoll?since=<id>
  if SameText(Path, '/longpoll') then
  begin
    if not TryStrToInt64(Args.RequestInfo.Params.Values['since'], LastId) then
    begin
      TSeqLogger.Logger.Log(Warning, Format('Client %s long poll request has invalid since parameter', [ClientId]));
      ResponseText := '{"error":"invalid since parameter"}';
      ResponseCode := 400;
      Exit;
    end;

    TSeqLogger.Logger.Log(Information, Format('Client %s started long poll from ID %d', [ClientId, LastId]));
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
  ClientId := Args.RequestInfo.RawHeaders.Values['X-Client-ID'];

  // Extract path without query parameters
  Path := URL;
  EqPos := Pos('?', Path);
  if EqPos > 0 then
    Path := Copy(Path, 1, EqPos - 1);

  TSeqLogger.Logger.Log(Information, Format('HTTP POST request from client %s: %s', [ClientId, URL]));

  if SameText(Path, '/data') then
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
      TSeqLogger.Logger.Log(Error, Format('Client %s: key is missing', [ClientId]));
      Doc.InitObject(['error', 'missing key']);
      ResponseText := string(Doc.ToJSON);
      ResponseCode := 400;
      Exit;
    end;

    TSeqLogger.Logger.Log(Information, Format('Client %s wrote key %s = %s',
      [ClientId, Key, VariantToString(Value)]));
    FKV.SetValue(Key, Value, ClientId);
    ResponseText := '{"status":"ok"}';
    ResponseCode := 200;
    Exit;
  end;

  if SameText(Path, '/batch') then
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
begin
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

  // Wait for up to 30 seconds for change
  if Ctx.WaitEvent.WaitFor(30000) = wrSignaled then
  begin
    FKV.GetChangesSince(Ctx.LastId, Ctx.ClientId, Changes);
    ResponseText := ChangesToJSON(Changes);
    ResponseCode := 200;
  end
  else
  begin    ResponseText := '[]'; // no changes
    ResponseCode := 204;
  end;

  FLongPollContexts.Remove(Ctx);
  Ctx.WaitEvent.Free;
  Ctx.Free;
end;

// Called on any change (manual, HTTP, etc)
procedure TKeyValueHTTPBridge.HandleValueChanged(Sender: TObject; const Key: string; const Value: variant; const SourceId: string);
begin
  TSeqLogger.Logger.Log(Information, Format('Value changed: key=%s value=%s source=%s', [Key, VariantSaveJSON(Value), SourceId]));
  NotifyLongPoll;
end;

procedure TKeyValueHTTPBridge.NotifyLongPoll;
var
  L: TList;
  i: Integer;
  Ctx: TLongPollContext;
begin
  L := FLongPollContexts.LockList;
  try
    for i := L.Count-1 downto 0 do
    begin
      Ctx := TLongPollContext(L[i]);
      Ctx.WaitEvent.SetEvent;
      L.Delete(i);
    end;
  finally
    FLongPollContexts.UnlockList;
  end;
end;

end.


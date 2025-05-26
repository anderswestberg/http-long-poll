unit KeyValueHTTPBridgeUnit;

interface

uses
  System.Classes, System.SysUtils, IdCustomHTTPServer,
  HTTPServerUnit, KeyValueServerUnit, IdStack, System.SyncObjs, System.Generics.Collections;

type
  TKeyValueHTTPBridge = class
  private
    FHTTP: THTTPServer;
    FKV: TKeyValueServer;
    FLongPollContexts: TThreadList;
    procedure HTTPGetHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const Json: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
    procedure HTTPPutHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const Json: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
    procedure AddLongPoll(LastId: Int64; const ClientId: string; var ResponseText: string; var ResponseCode: Integer);
    procedure HandleValueChanged(Sender: TObject; const Key, Value: string; const SourceId: string);
    procedure NotifyLongPoll;
    function ChangesToJSON(const Changes: TArray<TChangeRecord>): string;
  public
    constructor Create(APort: Integer; AKV: TKeyValueServer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property KeyValueStore: TKeyValueServer read FKV;
    property HTTPServer: THTTPServer read FHTTP;
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

constructor TKeyValueHTTPBridge.Create(APort: Integer; AKV: TKeyValueServer);
begin
  inherited Create;
  FKV := AKV;
  FHTTP := THTTPServer.Create(APort);
  FHTTP.OnGet := HTTPGetHandler;
  FHTTP.OnPost := HTTPPutHandler;
  FLongPollContexts := TThreadList.Create;
  FKV.OnValueChanged := HandleValueChanged;
end;

destructor TKeyValueHTTPBridge.Destroy;
begin
  Stop; // Ensure server is stopped
  FLongPollContexts.Free;
  FHTTP.Free;
  inherited;
end;

procedure TKeyValueHTTPBridge.Start;
begin
  FHTTP.Active := True;
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

  // Now stop the server
  try
    FHTTP.Active := False;
  except
    on E: EAbort do
      ; // Ignore abort during shutdown
    on E: EIdSocketError do
      ; // Ignore socket errors during shutdown
  end;
end;

// JSON encode array of changes
function TKeyValueHTTPBridge.ChangesToJSON(const Changes: TArray<TChangeRecord>): string;
var
  JArr: TJSONArray;
  JObj: TJSONObject;
  Change: TChangeRecord;
begin
  JArr := TJSONArray.Create;
  try
    for Change in Changes do
    begin
      JObj := TJSONObject.Create;
      JObj.AddPair('id', TJSONNumber.Create(Change.ChangeId));
      JObj.AddPair('key', Change.Key);
      JObj.AddPair('value', Change.Value);
      JObj.AddPair('timestamp', DateToISO8601(Change.Timestamp, False));
      JArr.AddElement(JObj);
    end;
    Result := JArr.ToJSON;
  finally
    JArr.Free;
  end;
end;

procedure TKeyValueHTTPBridge.HTTPGetHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const Json: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
var
  Key, Value: string;
  LastId: Int64;
  Changes: TArray<TChangeRecord>;
  ClientId: string;
  Guid: TGUID;
begin
  // Get client ID from header or generate one if not present
  ClientId := Args.RequestInfo.RawHeaders.Values['X-Client-ID'];
  if ClientId = '' then
  begin
    CreateGUID(Guid);
    ClientId := GUIDToString(Guid);
  end;

  // /data?key=foo - regular GET
  if SameText(URL, '/data') then
  begin
    Key := Args.RequestInfo.Params.Values['key'];
    if Key = '' then
    begin
      ResponseText := '{"error":"missing key"}';
      ResponseCode := 400;
      Exit;
    end;

    if FKV.GetValue(Key, Value) then
    begin
      ResponseText := Format('{"key":"%s","value":%s}', [Key, QuotedStr(Value)]);
      ResponseCode := 200;
    end
    else
    begin
      ResponseText := '{"error":"not found"}';
      ResponseCode := 404;
    end;
    Exit;
  end;

  // /longpoll?since=<id>
  if SameText(URL, '/longpoll') then
  begin
    LastId := StrToInt64Def(Args.RequestInfo.Params.Values['since'], 0);
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

  ResponseText := '{"error":"not found"}';
  ResponseCode := 404;
end;

procedure TKeyValueHTTPBridge.HTTPPutHandler(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const Json: variant; const Args: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer);
var
  Key, Value: string;
  ClientId: string;
  Guid: TGUID;
begin
  if SameText(URL, '/data') then
  begin
    Key := Args.RequestInfo.Params.Values['key'];
    Value := Args.RequestInfo.Params.Values['value'];
    if Key = '' then
    begin
      ResponseText := '{"error":"missing key"}';
      ResponseCode := 400;
      Exit;
    end;

    // Get client ID from header or generate one if not present
    ClientId := Args.RequestInfo.RawHeaders.Values['X-Client-ID'];
    if ClientId = '' then
    begin
      CreateGUID(Guid);
      ClientId := GUIDToString(Guid);
    end;

    FKV.SetValue(Key, Value, ClientId);
    ResponseText := '{"result":"ok"}';
    ResponseCode := 200;
    Exit;
  end;

  ResponseText := '{"error":"not found"}';
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
  begin
    ResponseText := '[]'; // no changes
    ResponseCode := 204;
  end;

  FLongPollContexts.Remove(Ctx);
  Ctx.WaitEvent.Free;
  Ctx.Free;
end;

// Called on any change (manual, HTTP, etc)
procedure TKeyValueHTTPBridge.HandleValueChanged(Sender: TObject; const Key, Value: string; const SourceId: string);
begin
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


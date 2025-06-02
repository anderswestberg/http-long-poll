unit KeyValueHttpClient;

interface

uses
  System.Classes, System.SysUtils, IdHTTP, IdComponent, IdStack, IdExceptionCore,
  IdIOHandlerStack, System.JSON, NetEncoding, SynCommons, Generics.Collections,
  System.Variants, SeqLogger;

type
  TChangeItem = record
    Id: Int64;
    Key: string;
    Value: Variant;
    Timestamp: string;
  end;

  TKVHttpClient = class
  private
    FBaseUrl: string;
    FIdHTTP: TIdHTTP;
    FIdHTTP_LongPoll: TIdHTTP;
    FClientId: string;
    procedure AddClientIdHeader(HTTP: TIdHTTP);
  public
    constructor Create(const BaseUrl: string);
    destructor Destroy; override;

    function GetValue(const Key: string): Variant;
    procedure PostValue(const Key: string; const Value: Variant);
    procedure PostValues(const KeyValues: array of TPair<string, Variant>);
    function LongPoll(SinceId: Int64; out Changes: TArray<TChangeItem>): Boolean;
    procedure CancelLongPoll;
    function GetLatestChangeId: Int64;
    function GetAll: TArray<TPair<string, Variant>>;

    property BaseUrl: string read FBaseUrl;
    property ClientId: string read FClientId write FClientId;
  end;

implementation

{ TKVHttpClient }

constructor TKVHttpClient.Create(const BaseUrl: string);
begin
  inherited Create;
  FBaseUrl := BaseUrl;
  
  // Create and configure regular HTTP client
  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.IOHandler := TIdIOHandlerStack.Create(FIdHTTP);
  FIdHTTP.ConnectTimeout := 5000;  // Increased for reliability
  FIdHTTP.ReadTimeout := 10000;    // Increased for reliability
  FIdHTTP.IOHandler.MaxLineLength := 16384;
  FIdHTTP.AllowCookies := False;   // Don't need cookies
  FIdHTTP.HandleRedirects := False; // Don't need redirects
  FIdHTTP.ProtocolVersion := pv1_1; // Use HTTP/1.1 for keep-alive
  FIdHTTP.Request.Connection := 'keep-alive';
  
  // Create and configure long polling HTTP client
  FIdHTTP_LongPoll := TIdHTTP.Create(nil);
  FIdHTTP_LongPoll.IOHandler := TIdIOHandlerStack.Create(FIdHTTP_LongPoll);
  FIdHTTP_LongPoll.ConnectTimeout := 5000;  // Increased for reliability
  FIdHTTP_LongPoll.ReadTimeout := 32000;    // Keep long poll timeout
  FIdHTTP_LongPoll.IOHandler.MaxLineLength := 16384;
  FIdHTTP_LongPoll.AllowCookies := False;   // Don't need cookies
  FIdHTTP_LongPoll.HandleRedirects := False; // Don't need redirects
  FIdHTTP_LongPoll.ProtocolVersion := pv1_1; // Use HTTP/1.1 for keep-alive
  FIdHTTP_LongPoll.Request.Connection := 'keep-alive';
  
  FClientId := ''; // Empty by default
  TSeqLogger.Logger.Log(Information, 'HTTP client created for {BaseUrl}', ['BaseUrl', BaseUrl]);
end;

destructor TKVHttpClient.Destroy;
begin
  TSeqLogger.Logger.Log(Information, 'HTTP client for {BaseUrl} destroyed', ['BaseUrl', BaseUrl]);
  CancelLongPoll;
  FIdHTTP.Free;
  FIdHTTP_LongPoll.Free;
  inherited;
end;

procedure TKVHttpClient.AddClientIdHeader(HTTP: TIdHTTP);
begin
  HTTP.Request.ContentType := 'application/json';
end;

function TKVHttpClient.GetValue(const Key: string): Variant;
var
  Doc: TDocVariantData;
  Response: string;
  Url: string;
begin
  TSeqLogger.Logger.Log(Information, 'Client {ClientId} reading key {Key}', ['ClientId', FClientId, 'Key', Key]);
  AddClientIdHeader(FIdHTTP);  // Use regular client
  try
    Url := FBaseUrl + '/values?key=' + TNetEncoding.URL.Encode(Key);
    if FClientId <> '' then
      Url := Url + '&clientId=' + TNetEncoding.URL.Encode(FClientId);
    Response := FIdHTTP.Get(Url);
    Doc.InitJSON(RawUTF8(Response));
    Result := Doc.GetValueOrNull('value');
    TSeqLogger.Logger.Log(Information, 'Client {ClientId} read key {Key} = {Value}', ['ClientId', FClientId, 'Key', Key, 'Value', VarToStr(Result)]);
  except
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, 'Client {ClientId} failed to read key {Key}: {Error}', ['ClientId', FClientId, 'Key', Key, 'Error', E.Message]);
      raise;
    end;
  end;
end;

procedure TKVHttpClient.PostValue(const Key: string; const Value: Variant);
var
  Doc: TDocVariantData;
  Source: TStringStream;
  Url: string;
begin
  TSeqLogger.Logger.Log(Information, 'Client {ClientId} writing key {Key} = {Value}', ['ClientId', FClientId, 'Key', Key, 'Value', VarToStr(Value)]);
  AddClientIdHeader(FIdHTTP);
  
  Doc.InitObject([
    'key', Key,
    'value', Value
  ]);
  Source := TStringStream.Create(string(Doc.ToJSON), TEncoding.UTF8);
  try
    Url := FBaseUrl + '/values';
    if FClientId <> '' then
      Url := Url + '?clientId=' + TNetEncoding.URL.Encode(FClientId);
    FIdHTTP.Post(Url, Source);
    TSeqLogger.Logger.Log(Information, 'Client {ClientId} successfully wrote key {Key}', ['ClientId', FClientId, 'Key', Key]);
  except
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, 'Client {ClientId} failed to write key {Key}: {Error}', ['ClientId', FClientId, 'Key', Key, 'Error', E.Message]);
      raise;
    end;
  end;
  Source.Free;
end;

procedure TKVHttpClient.PostValues(const KeyValues: array of TPair<string, Variant>);
var
  Doc: TDocVariantData;
  Pair: TPair<string, Variant>;
  Source: TStringStream;
  Url: string;
begin
  TSeqLogger.Logger.Log(Information, 'Client {ClientId} starting batch write of {Count} values', ['ClientId', FClientId, 'Count', Length(KeyValues)]);
  AddClientIdHeader(FIdHTTP);

  Doc.InitArray([]);
  for Pair in KeyValues do
    Doc.AddItem(_Obj([
      'key', Pair.Key,
      'value', Pair.Value
    ]));
  Source := TStringStream.Create(string(Doc.ToJSON), TEncoding.UTF8);
  try
    try
      Url := FBaseUrl + '/values/batch';
      if FClientId <> '' then
        Url := Url + '?clientId=' + TNetEncoding.URL.Encode(FClientId);
      FIdHTTP.Post(Url, Source);
      TSeqLogger.Logger.Log(Information, 'Client {ClientId} successfully completed batch write', ['ClientId', FClientId]);
    except
      on E: Exception do
      begin
        TSeqLogger.Logger.Log(Error, 'Client {ClientId} failed batch write: {Error}', ['ClientId', FClientId, 'Error', E.Message]);
        raise;
      end;
    end;
  finally
    Source.Free;
  end;
end;

procedure TKVHttpClient.CancelLongPoll;
begin
  if Assigned(FIdHTTP_LongPoll) then
  begin
    TSeqLogger.Logger.Log(Information, 'Client {ClientId} cancelling long poll', ['ClientId', FClientId]);
    try
      FIdHTTP_LongPoll.Disconnect;
    except
      on E: EIdNotConnected do
        ; // Ignore - already disconnected
    end;
  end;
end;

// Returns True if changes received, False if timeout (no new data)
function TKVHttpClient.LongPoll(SinceId: Int64; out Changes: TArray<TChangeItem>): Boolean;
var
  Resp: string;
  Doc: TDocVariantData;
  i: Integer;
  Url: string;
begin
  TSeqLogger.Logger.Log(Information, 'Client {ClientId} starting long poll from ID {SinceId}', ['ClientId', FClientId, 'SinceId', SinceId]);
  SetLength(Changes, 0);
  try
    AddClientIdHeader(FIdHTTP_LongPoll);
    Url := FBaseUrl + '/changes?since=' + IntToStr(SinceId);
    if FClientId <> '' then
      Url := Url + '&clientId=' + TNetEncoding.URL.Encode(FClientId);
    Resp := FIdHTTP_LongPoll.Get(Url);
    if Resp <> '' then
    begin
      Doc.InitJSON(RawUTF8(Resp));
      if Doc.Kind = dvArray then
      begin
        SetLength(Changes, Doc.Count);
        for i := 0 to Doc.Count-1 do
        begin
          var Item := TDocVariantData(Doc.Values[i]);
          Changes[i].Id := Item.I['id'];
          Changes[i].Key := Item.S['key'];
          Changes[i].Value := Item.GetValueOrNull('value');
          Changes[i].Timestamp := Item.S['timestamp'];
          TSeqLogger.Logger.Log(Information, 'Client {ClientId} received change: id={ChangeId} key={Key} value={Value} time={Time}',
            ['ClientId', FClientId, 'ChangeId', Changes[i].Id, 'Key', Changes[i].Key, 'Value', VarToStr(Changes[i].Value), 'Time', Changes[i].Timestamp]);
        end;
        Result := Doc.Count > 0;
      end 
      else
        Result := False;
    end 
    else
      Result := False;

    if not Result then
      TSeqLogger.Logger.Log(Information, 'Client {ClientId} long poll timeout (no changes)', ['ClientId', FClientId]);
  except
    on E: EIdHTTPProtocolException do
    begin
      // 204 No Content = timeout
      if E.ErrorCode = 204 then
      begin
        TSeqLogger.Logger.Log(Information, 'Client {ClientId} long poll timeout (204)', ['ClientId', FClientId]);
        Result := False;
      end
      else
      begin
        TSeqLogger.Logger.Log(Error, 'Client {ClientId} long poll HTTP error: {Error}', ['ClientId', FClientId, 'Error', E.Message]);
        raise;
      end;
    end;
    on E: EIdSocketError do
    begin
      // Socket error (e.g., connection closed) = cancelled
      TSeqLogger.Logger.Log(Warning, 'Client {ClientId} long poll socket error: {Error}', ['ClientId', FClientId, 'Error', E.Message]);
      Result := False;
    end;
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, 'Client {ClientId} long poll error: {Error}', ['ClientId', FClientId, 'Error', E.Message]);
      raise;
    end;
  end;
end;

function TKVHttpClient.GetLatestChangeId: Int64;
var
  Doc: TDocVariantData;
  Url: string;
begin
  TSeqLogger.Logger.Log(Information, 'Client {ClientId} requesting latest change ID', ['ClientId', FClientId]);
  AddClientIdHeader(FIdHTTP);
  try
    Url := FBaseUrl + '/changes/latest';
    if FClientId <> '' then
      Url := Url + '?clientId=' + TNetEncoding.URL.Encode(FClientId);
    Doc.InitJSON(RawUTF8(FIdHTTP.Get(Url)));
    Result := Doc.I['id'];
    TSeqLogger.Logger.Log(Information, 'Client {ClientId} received latest change ID: {ChangeId}', ['ClientId', FClientId, 'ChangeId', Result]);
  except
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, 'Client {ClientId} failed to get latest change ID: {Error}', ['ClientId', FClientId, 'Error', E.Message]);
      raise;
    end;
  end;
end;

function TKVHttpClient.GetAll: TArray<TPair<string, Variant>>;
var
  Doc: TDocVariantData;
  i: Integer;
  Url: string;
begin
  TSeqLogger.Logger.Log(Information, 'Client {ClientId} requesting all values', ['ClientId', FClientId]);
  AddClientIdHeader(FIdHTTP);
  try
    Url := FBaseUrl + '/values/all';
    if FClientId <> '' then
      Url := Url + '?clientId=' + TNetEncoding.URL.Encode(FClientId);
    Doc.InitJSON(RawUTF8(FIdHTTP.Get(Url)));
    if Doc.Kind = dvArray then
    begin
      SetLength(Result, Doc.Count);
      TSeqLogger.Logger.Log(Information, 'Client {ClientId} received {Count} values', ['ClientId', FClientId, 'Count', Doc.Count]);
      for i := 0 to Doc.Count-1 do
      begin
        Result[i].Key := Doc.Values[i].S['key'];
        Result[i].Value := Doc.Values[i].GetValueOrNull('value');
        TSeqLogger.Logger.Log(Information, 'Client {ClientId} received value: {Key} = {Value}',
          ['ClientId', FClientId, 'Key', Result[i].Key, 'Value', VarToStr(Result[i].Value)]);
      end;
    end
    else
      SetLength(Result, 0);
  except
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, 'Client {ClientId} failed to get all values: {Error}', ['ClientId', FClientId, 'Error', E.Message]);
      raise;
    end;
  end;
end;

end.


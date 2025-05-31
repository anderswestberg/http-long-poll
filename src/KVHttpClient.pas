unit KVHttpClient;

interface

uses
  System.Classes, System.SysUtils, IdHTTP, IdComponent, IdStack, IdExceptionCore, System.JSON, NetEncoding,
  SynCommons, Generics.Collections, System.Variants, SeqloggerClass;

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
  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.ConnectTimeout := 3000;
  FIdHTTP_LongPoll := TIdHTTP.Create(nil);
  FIdHTTP_LongPoll.ConnectTimeout := 3000;
  FIdHTTP_LongPoll.ReadTimeout := 32000; // For long polling
  FClientId := ''; // Empty by default
  TSeqLogger.Logger.Log(Information, Format('HTTP client created for %s', [BaseUrl]));
end;

destructor TKVHttpClient.Destroy;
begin
  TSeqLogger.Logger.Log(Information, Format('HTTP client for %s destroyed', [BaseUrl]));
  CancelLongPoll;
  FIdHTTP.Free;
  FIdHTTP_LongPoll.Free;
  inherited;
end;

procedure TKVHttpClient.AddClientIdHeader(HTTP: TIdHTTP);
begin
  if FClientId <> '' then
    HTTP.Request.CustomHeaders.AddValue('X-Client-ID', FClientId);
  HTTP.Request.ContentType := 'application/json';
end;

function TKVHttpClient.GetValue(const Key: string): Variant;
var
  Doc: TDocVariantData;
begin
  TSeqLogger.Logger.Log(Information, Format('Client %s reading key %s', [FClientId, Key]));
  AddClientIdHeader(FIdHTTP);  // Use regular client
  try
    Doc.InitJSON(RawUTF8(FIdHTTP.Get(FBaseUrl + '/data?key=' + TNetEncoding.URL.Encode(Key))));
    Result := Doc.GetValueOrNull('value');
    TSeqLogger.Logger.Log(Information, Format('Client %s read key %s = %s', [FClientId, Key, VarToStr(Result)]));
  except
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, Format('Client %s failed to read key %s: %s', [FClientId, Key, E.Message]));
      raise;
    end;
  end;
end;

procedure TKVHttpClient.PostValue(const Key: string; const Value: Variant);
var
  Doc: TDocVariantData;
  Source: TStringStream;
begin
  TSeqLogger.Logger.Log(Information, Format('Client %s writing key %s = %s', [FClientId, Key, VarToStr(Value)]));
  AddClientIdHeader(FIdHTTP);
  Doc.InitObject([
    'key', Key,
    'value', Value
  ]);
  try
    Source := TStringStream.Create(string(Doc.ToJSON), TEncoding.UTF8);
    try
      FIdHTTP.Post(FBaseUrl + '/data', Source);
      TSeqLogger.Logger.Log(Information, Format('Client %s successfully wrote key %s', [FClientId, Key]));
    except
      on E: Exception do
      begin
        TSeqLogger.Logger.Log(Error, Format('Client %s failed to write key %s: %s', [FClientId, Key, E.Message]));
        raise;
      end;
    end;
  finally
    Source.Free;
  end;
end;

procedure TKVHttpClient.PostValues(const KeyValues: array of TPair<string, Variant>);
var
  Doc: TDocVariantData;
  Pair: TPair<string, Variant>;
  Source: TStringStream;
begin
  TSeqLogger.Logger.Log(Information, Format('Client %s starting batch write of %d values', [FClientId, Length(KeyValues)]));
  AddClientIdHeader(FIdHTTP);
  Doc.InitArray([]);
  for Pair in KeyValues do
  begin
    Doc.AddItem(_Obj([
      'key', Pair.Key,
      'value', Pair.Value
    ]));
    TSeqLogger.Logger.Log(Information, Format('Client %s batch write key %s = %s', [FClientId, Pair.Key, VarToStr(Pair.Value)]));
  end;
  try
    Source := TStringStream.Create(string(Doc.ToJSON), TEncoding.UTF8);
    try
      FIdHTTP.Post(FBaseUrl + '/batch', Source);
      TSeqLogger.Logger.Log(Information, Format('Client %s successfully completed batch write', [FClientId]));
    except
      on E: Exception do
      begin
        TSeqLogger.Logger.Log(Error, Format('Client %s failed batch write: %s', [FClientId, E.Message]));
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
    TSeqLogger.Logger.Log(Information, Format('Client %s cancelling long poll', [FClientId]));
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
begin
  TSeqLogger.Logger.Log(Information, Format('Client %s starting long poll from ID %d', [FClientId, SinceId]));
  SetLength(Changes, 0);
  try
    AddClientIdHeader(FIdHTTP_LongPoll);
    Resp := FIdHTTP_LongPoll.Get(FBaseUrl + '/longpoll?since=' + IntToStr(SinceId));
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
          TSeqLogger.Logger.Log(Information, Format('Client %s received change: id=%d key=%s value=%s time=%s', 
            [FClientId, Changes[i].Id, Changes[i].Key, VarToStr(Changes[i].Value), Changes[i].Timestamp]));
        end;
        Result := Doc.Count > 0;
      end 
      else
        Result := False;
    end 
    else
      Result := False;

    if not Result then
      TSeqLogger.Logger.Log(Information, Format('Client %s long poll timeout (no changes)', [FClientId]));
  except
    on E: EIdHTTPProtocolException do
    begin
      // 204 No Content = timeout
      if E.ErrorCode = 204 then
      begin
        TSeqLogger.Logger.Log(Information, Format('Client %s long poll timeout (204)', [FClientId]));
        Result := False;
      end
      else
      begin
        TSeqLogger.Logger.Log(Error, Format('Client %s long poll HTTP error: %s', [FClientId, E.Message]));
        raise;
      end;
    end;
    on E: EIdSocketError do
    begin
      // Socket error (e.g., connection closed) = cancelled
      TSeqLogger.Logger.Log(Warning, Format('Client %s long poll socket error: %s', [FClientId, E.Message]));
      Result := False;
    end;
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, Format('Client %s long poll error: %s', [FClientId, E.Message]));
      raise;
    end;
  end;
end;

function TKVHttpClient.GetLatestChangeId: Int64;
var
  Doc: TDocVariantData;
begin
  TSeqLogger.Logger.Log(Information, Format('Client %s requesting latest change ID', [FClientId]));
  AddClientIdHeader(FIdHTTP);
  try
    Doc.InitJSON(RawUTF8(FIdHTTP.Get(FBaseUrl + '/latest-change-id')));
    Result := Doc.I['id'];
    TSeqLogger.Logger.Log(Information, Format('Client %s received latest change ID: %d', [FClientId, Result]));
  except
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, Format('Client %s failed to get latest change ID: %s', [FClientId, E.Message]));
      raise;
    end;
  end;
end;

function TKVHttpClient.GetAll: TArray<TPair<string, Variant>>;
var
  Doc: TDocVariantData;
  i: Integer;
begin
  TSeqLogger.Logger.Log(Information, Format('Client %s requesting all values', [FClientId]));
  AddClientIdHeader(FIdHTTP);
  try
    Doc.InitJSON(RawUTF8(FIdHTTP.Get(FBaseUrl + '/all')));
    if Doc.Kind = dvArray then
    begin
      SetLength(Result, Doc.Count);
      TSeqLogger.Logger.Log(Information, Format('Client %s received %d values', [FClientId, Doc.Count]));
      for i := 0 to Doc.Count-1 do
      begin
        Result[i].Key := Doc.Values[i].S['key'];
        Result[i].Value := Doc.Values[i].GetValueOrNull('value');
        TSeqLogger.Logger.Log(Information, Format('Client %s received value: %s = %s', 
          [FClientId, Result[i].Key, VarToStr(Result[i].Value)]));
      end;
    end
    else
      SetLength(Result, 0);
  except
    on E: Exception do
    begin
      TSeqLogger.Logger.Log(Error, Format('Client %s failed to get all values: %s', [FClientId, E.Message]));
      raise;
    end;
  end;
end;

end.


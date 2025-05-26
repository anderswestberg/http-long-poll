unit KVHttpClient;

interface

uses
  System.Classes, System.SysUtils, IdHTTP, IdComponent, IdStack, IdExceptionCore, System.JSON, NetEncoding,
  SynCommons;

type
  TChangeItem = record
    Id: Int64;
    Key: string;
    Value: string;
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

    function GetValue(const Key: string): string;
    procedure PutValue(const Key, Value: string);
    procedure PutValues(const KeyValues: array of TPair<string, string>);
    function LongPoll(SinceId: Int64; out Changes: TArray<TChangeItem>): Boolean;
    procedure CancelLongPoll;

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
end;

destructor TKVHttpClient.Destroy;
begin
  CancelLongPoll;
  FIdHTTP.Free;
  FIdHTTP_LongPoll.Free;
  inherited;
end;

procedure TKVHttpClient.AddClientIdHeader(HTTP: TIdHTTP);
begin
  if FClientId <> '' then
    HTTP.Request.CustomHeaders.AddValue('X-Client-ID', FClientId);
end;

function TKVHttpClient.GetValue(const Key: string): string;
var
  Doc: TDocVariantData;
begin
  AddClientIdHeader(FIdHTTP_LongPoll);
  Doc.InitJSON(FIdHTTP_LongPoll.Get(FBaseUrl + '/data?key=' + TNetEncoding.URL.Encode(Key)));
  Result := Doc.S['value'];
end;

procedure TKVHttpClient.PutValue(const Key, Value: string);
var
  Source: TStringStream;
begin
  AddClientIdHeader(FIdHTTP);
  Source := TStringStream.Create(Value, TEncoding.UTF8);
  try
    FIdHTTP.Post(FBaseUrl + '/data?key=' + TNetEncoding.URL.Encode(Key) + '&value=' + TNetEncoding.URL.Encode(Value), Source);
  finally
    Source.Free;
  end;
end;

procedure TKVHttpClient.PutValues(const KeyValues: array of TPair<string, string>);
var
  Doc: TDocVariantData;
  Pair: TPair<string, string>;
  Source: TStringStream;
begin
  AddClientIdHeader(FIdHTTP);
  Doc.InitArray([]);
  for Pair in KeyValues do
    Doc.AddItem(_Obj([
      'key', Pair.Key,
      'value', Pair.Value
    ]));
  Source := TStringStream.Create(Doc.ToJSON, TEncoding.UTF8);
  try
    FIdHTTP.Post(FBaseUrl + '/batch', Source);
  finally
    Source.Free;
  end;
end;

procedure TKVHttpClient.CancelLongPoll;
begin
  if Assigned(FIdHTTP_LongPoll) then
    try
      FIdHTTP_LongPoll.Disconnect;
    except
      on E: EIdNotConnected do
        ; // Ignore - already disconnected
    end;
end;

// Returns True if changes received, False if timeout (no new data)
function TKVHttpClient.LongPoll(SinceId: Int64; out Changes: TArray<TChangeItem>): Boolean;
var
  Resp: string;
  Doc: TDocVariantData;
  i: Integer;
begin
  SetLength(Changes, 0);
  try
    AddClientIdHeader(FIdHTTP_LongPoll);
    Resp := FIdHTTP_LongPoll.Get(FBaseUrl + '/longpoll?since=' + IntToStr(SinceId));
    Doc.InitJSON(Resp);
    if (Doc.VarType = DocVariantType.VarType) and (Doc.Kind = dvArray) then
    begin
      SetLength(Changes, Doc.Count);
      for i := 0 to Doc.Count-1 do
      begin
        var Item := TDocVariantData(Doc.Values[i]);
        Changes[i].Id := Item.I['id'];
        Changes[i].Key := Item.S['key'];
        Changes[i].Value := Item.S['value'];
        Changes[i].Timestamp := Item.S['timestamp'];
      end;
      Result := True;
    end else
      Result := False;
  except
    on E: EIdHTTPProtocolException do
    begin
      // 204 No Content = timeout
      if E.ErrorCode = 204 then
        Result := False
      else
        raise;
    end;
    on E: EIdSocketError do
    begin
      // Socket error (e.g., connection closed) = cancelled
      Result := False;
    end;
    on E: Exception do
      raise;
  end;
end;

end.


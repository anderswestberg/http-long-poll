unit KVHttpClient;

interface

uses
  System.Classes, System.SysUtils, IdHTTP, IdComponent, IdStack, System.JSON, NetEncoding;

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
begin
  AddClientIdHeader(FIdHTTP_LongPoll);
  Result := FIdHTTP_LongPoll.Get(FBaseUrl + '/data?key=' + TNetEncoding.URL.Encode(Key));
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

procedure TKVHttpClient.CancelLongPoll;
begin
  if Assigned(FIdHTTP_LongPoll) then
    FIdHTTP_LongPoll.Disconnect;
end;

// Returns True if changes received, False if timeout (no new data)
function TKVHttpClient.LongPoll(SinceId: Int64; out Changes: TArray<TChangeItem>): Boolean;
var
  Resp: string;
  Arr: TJSONArray;
  Obj: TJSONObject;
  i: Integer;
begin
  SetLength(Changes, 0);
  try
    AddClientIdHeader(FIdHTTP_LongPoll);
    Resp := FIdHTTP_LongPoll.Get(FBaseUrl + '/longpoll?since=' + IntToStr(SinceId));
    Arr := TJSONObject.ParseJSONValue(Resp) as TJSONArray;
    try
      if Assigned(Arr) then
      begin
        SetLength(Changes, Arr.Count);
        for i := 0 to Arr.Count-1 do
        begin
          Obj := Arr.Items[i] as TJSONObject;
          Changes[i].Id := Obj.GetValue('id').Value.ToInt64;
          Changes[i].Key := Obj.GetValue('key').Value;
          Changes[i].Value := Obj.GetValue('value').Value;
          Changes[i].Timestamp := Obj.GetValue('timestamp').Value;
        end;
        Result := True;
      end else
        Result := False;
    finally
      Arr.Free;
    end;
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


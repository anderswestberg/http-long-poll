unit HTTPServerUnit;

interface

uses
  System.Classes, IdHTTPServer, IdContext, IdCustomHTTPServer, IdCustomTCPServer,
  System.SysUtils, System.NetEncoding, System.Variants, SynCommons;

type
  THTTPCommandArgs = record
    ContentType: string;
    Context: TIdContext;
    RequestInfo: TIdHTTPRequestInfo;
    ResponseInfo: TIdHTTPResponseInfo
  end;

  THTTPCommandEvent = procedure(Sender: TObject; const URL: string; const Params: TStrings;
    const Body: string; const Json: variant; const HTTPCommandArgs: THTTPCommandArgs; var ResponseText: string; var ResponseCode: Integer) of object;


  THTTPServer = class
  private
    FHTTPServer: TIdHTTPServer;
    FPort: Integer;

    // Events for different HTTP methods
    FOnGet: THTTPCommandEvent;
    FOnPost: THTTPCommandEvent;
    FOnPut: THTTPCommandEvent;
    FOnDelete: THTTPCommandEvent;
    FActive: boolean;

    procedure HandleCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure SetActive(const Value: boolean);
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;

    procedure StopServer;
    property Active: boolean read FActive write SetActive;
    // Properties for events
    property OnGet: THTTPCommandEvent read FOnGet write FOnGet;
    property OnPost: THTTPCommandEvent read FOnPost write FOnPost;
    property OnPut: THTTPCommandEvent read FOnPut write FOnPut;
    property OnDelete: THTTPCommandEvent read FOnDelete write FOnDelete;
end;

  THTTPServerThread = class(TThread)
  protected
    procedure Execute; override;
  public
    FHttpServer: THttpServer;
    constructor Create(Port: Integer);
    destructor Destroy; override;
  end;

implementation

{ THTTPServer }

constructor THTTPServer.Create(Port: Integer);
begin
  inherited Create;
  FPort := Port;

  // Create and configure the HTTP server
  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.DefaultPort := FPort;
  FHTTPServer.OnCommandGet := HandleCommand;
end;

destructor THTTPServer.Destroy;
begin
  StopServer;
  FHTTPServer.Free;
  inherited;
end;

procedure THTTPServer.SetActive(const Value: boolean);
begin
  FActive := Value;
  FHTTPServer.Active := Value;
end;

procedure THTTPServer.StopServer;
begin
  if Assigned(FHTTPServer) and FHTTPServer.Active then
    FHTTPServer.Active := False;
end;

{ THTTPServerThread }

constructor THTTPServerThread.Create(Port: Integer);
begin
  inherited Create(True); // Create suspended
  FHttpServer := THTTPServer.Create(Port);
end;

destructor THTTPServerThread.Destroy;
begin
  FHTTPServer.StopServer;
  FHTTPServer.Free;
  inherited;
end;

procedure THTTPServerThread.Execute;
begin
  try
    // Start the HTTP server
    FHTTPServer.FHTTPServer.Active := True;

    // Keep the thread alive while the server is running
    while not Terminated do
    begin
      Sleep(100); // Prevent high CPU usage
    end;

  except
    on E: Exception do
      // Log or handle server errors here
      raise Exception.Create('HTTP Server Error: ' + E.Message);
  end;
end;

procedure THTTPServer.HandleCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  URL: string;
  Params: TStrings;
  Body, s: string;
  ResponseText: string;
  ResponseCode: Integer;
  Json: variant;
  ContentType: string;
  Buffer: TBytes;
  HTTPCommandArgs: THTTPCommandArgs;
begin
  URL := ARequestInfo.Document;
  Params := TStringList.Create;
  try
    try
      Params.Delimiter := '&';
      Params.StrictDelimiter := True;
      s := TNetEncoding.URL.Decode(ARequestInfo.QueryParams);
      Params.DelimitedText := s;
      ContentType := ARequestInfo.ContentType;
      Body := '';
      Json := null;
      HTTPCommandArgs.ContentType := ContentType;
      HTTPCommandArgs.Context := AContext;
      HTTPCommandArgs.RequestInfo := ARequestInfo;
      HTTPCommandArgs.ResponseInfo := AResponseInfo;

      if Assigned(ARequestInfo.PostStream) and (ARequestInfo.PostStream.Size > 0) then
      begin
        SetLength(Buffer, ARequestInfo.PostStream.Size);
        ARequestInfo.PostStream.Position := 0;
        ARequestInfo.PostStream.ReadBuffer(Buffer[0], Length(Buffer));
        Body := TEncoding.UTF8.GetString(Buffer);
        if (ContentType = 'application/json') and (Body <> '') and ((Body[1] = '{') or (Body[1] = '[')) then
          Json := _Json(RawUTF8(Body), [dvoAllowDoubleValue]);
      end;

      ResponseText := '';
      ResponseCode := 200;

      // Dispatch based on HTTP command
      if ARequestInfo.Command = 'GET' then
      begin
        if Assigned(FOnGet) then
          FOnGet(Self, URL, Params, Body, Json, HTTPCommandArgs, ResponseText, ResponseCode);
      end
      else if ARequestInfo.Command = 'POST' then
      begin
        if Assigned(FOnPost) then
          FOnPost(Self, URL, Params, Body, Json, HTTPCommandArgs, ResponseText, ResponseCode);
      end
      else if ARequestInfo.Command = 'PUT' then
      begin
        if Assigned(FOnPut) then
          FOnPut(Self, URL, Params, Body, Json, HTTPCommandArgs, ResponseText, ResponseCode);
      end
      else if ARequestInfo.Command = 'DELETE' then
      begin
        if Assigned(FOnDelete) then
          FOnDelete(Self, URL, Params, Body, Json, HTTPCommandArgs, ResponseText, ResponseCode);
      end
      else
      begin
        ResponseCode := 405; // Method Not Allowed
        ResponseText := 'Method Not Allowed';
      end;

      if (ARequestInfo.Connection = 'close') or (ARequestInfo.Version = 'HTTP/1.0') then
      begin
        // Close connection explicitly
        AResponseInfo.CloseConnection := True;
        AResponseInfo.Connection := 'close';
      end
      else
      begin
        // Default to Keep-Alive for HTTP/1.1
        AResponseInfo.CloseConnection := False;
        AResponseInfo.Connection := 'keep-alive';
      end;

      // Set the response
      AResponseInfo.ContentText := ResponseText;
      AResponseInfo.ResponseNo := ResponseCode;
    except
    end;

  finally
    Params.Free;
  end;
end;

end.


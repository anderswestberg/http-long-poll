unit SeqLogger;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections, System.Variants, IdHTTP, IdSSLOpenSSL, SyncObjs, System.DateUtils;

const
  DEFAULT_FLUSH_TIMEOUT = 1000; // 1 second in milliseconds

type
  TLogLevel = (Verbose, Debug, Information, Warning, Error, Fatal);

  TSeqLogger = class
  private
    FEndpoint: string;
    FApiKey: string;
    FHttp: TIdHTTP;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FLogQueue: TQueue<TJSONObject>;
    FLock: TCriticalSection;
    FBatchSize: Integer;
    FRetryCount: Integer;
    FTimer: TThread;
    FFlushTimeout: Integer;
    FEnabled: Boolean;
    FMinLogLevel: TLogLevel;

    procedure SendBatch; // Sends logs in batches
    function FormatMessage(const Template: string; const Values: array of Variant): string;
    class function GetHostIP(aDefault: string): string;
    class function GetApiKey(aDefault: string): string; static;
  public
    constructor Create(const Endpoint: string; const ApiKey: string = ''; BatchSize: Integer = 1000; RetryCount: Integer = 3; FlushTimeout: Integer = DEFAULT_FLUSH_TIMEOUT);
    destructor Destroy; override;

    class function LogLevelToString(Level: TLogLevel): string;
    class function StringToLogLevel(const S: string): TLogLevel;
    procedure Log(Level: TLogLevel; const MessageText: string; const Properties: TJSONObject = nil); overload;
    procedure Log(Level: TLogLevel; const Template: string; const Values: array of Variant); overload;
    procedure Flush; // Forces flushing logs immediately

    class function Logger: TSeqLogger; static;
    property Enabled: Boolean read FEnabled write FEnabled;
    property MinLogLevel: TLogLevel read FMinLogLevel write FMinLogLevel;
  end;

implementation

var
  GlobalLogger: TSeqLogger = nil;

{ TSeqLogger }

constructor TSeqLogger.Create(const Endpoint: string; const ApiKey: string = ''; BatchSize: Integer = 1000; RetryCount: Integer = 3; FlushTimeout: Integer = DEFAULT_FLUSH_TIMEOUT);
begin
  inherited Create;
  FEndpoint := Endpoint;
  FApiKey := ApiKey;
  FBatchSize := BatchSize;
  FRetryCount := RetryCount;
  FFlushTimeout := FlushTimeout;
  FEnabled := True; // Default to enabled
  FMinLogLevel := Information; // Default to Information level

  // Initialize HTTP client
  FHttp := TIdHTTP.Create(nil);
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHttp.IOHandler := FSSLHandler;
  FHttp.Request.ContentType := 'application/json';
  FHttp.Request.Accept := 'application/json';
  FHttp.Request.UserAgent := 'DelphiLogger';

  FHttp.ConnectTimeout := 5000; // 5 seconds connection timeout
  FHttp.ReadTimeout := 5000;   // 5 seconds read timeout

  if FApiKey <> '' then
    FHttp.Request.CustomHeaders.Values['X-Seq-ApiKey'] := FApiKey;

  // Initialize threading and queue
  FLogQueue := TQueue<TJSONObject>.Create;
  FLock := TCriticalSection.Create;

  // Start a background thread for timeout-based flushing
  FTimer := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        while not TThread.CurrentThread.CheckTerminated do
        begin
          try
            Sleep(FFlushTimeout);
            Flush;
          except
            on E: Exception do
            begin
              // Log exceptions to console or local log
              //Writeln('Logging error: ' + E.Message);
            end;
          end;
        end;
      except
        on E: Exception do
        begin
          //Writeln('Critical thread error: ' + E.Message);
        end;
      end;
    end);
  FTimer.FreeOnTerminate := False; // Do not free automatically
  FTimer.Start;

  // Log program start information
  Log(Information, 'Program started: Name: {AppName}, Version: {Version}, User: {User}', ['AppName', ExtractFileName(ParamStr(0)), 'Version', '1.0.0', 'User', GetEnvironmentVariable('USERNAME')]);
end;

// Destructor to clean up resources
destructor TSeqLogger.Destroy;
begin
  // Signal thread to terminate and wait for it to finish
  if Assigned(FTimer) then
  begin
    FTimer.Terminate;
    FTimer.WaitFor;
    FTimer.Free; // Free the thread explicitly
  end;

  // Log program stop information
  Log(Information, 'Program stopped Name: {AppName}, Version: {Version}, User: {User}', ['AppName', ExtractFileName(ParamStr(0)), 'Version', '1.0.0', 'User', GetEnvironmentVariable('USERNAME')]);
  Flush; // Flush remaining logs before destruction
  FLogQueue.Free;
  FLock.Free;
  FHttp.Free;
  FSSLHandler.Free;
  inherited;
end;

// Converts TLogLevel to string
class function TSeqLogger.LogLevelToString(Level: TLogLevel): string;
begin
  case Level of
    Verbose: Result := 'Verbose';
    Debug: Result := 'Debug';
    Information: Result := 'Information';
    Warning: Result := 'Warning';
    Error: Result := 'Error';
    Fatal: Result := 'Fatal';
  else
    Result := 'Information';
  end;
end;

// Formats the message template with values
function TSeqLogger.FormatMessage(const Template: string; const Values: array of Variant): string;
var
  i: Integer;
  Key: string;
begin
  Result := Template;
  i := 0;
  while i < High(Values) do
  begin
    Key := '{' + VarToStr(Values[i]) + '}';
    Result := StringReplace(Result, Key, VarToStr(Values[i + 1]), [rfReplaceAll]);
    Inc(i, 2);
  end;
end;

// Adds a log entry to the queue
procedure TSeqLogger.Log(Level: TLogLevel; const MessageText: string; const Properties: TJSONObject = nil);
var
  LogEntry: TJSONObject;
  Pair: TJSONPair;
begin
  if not FEnabled or (Level < FMinLogLevel) then
    Exit;

  LogEntry := TJSONObject.Create;
  try
    LogEntry.AddPair('@t', DateToISO8601(Now));
    LogEntry.AddPair('@l', LogLevelToString(Level));
    LogEntry.AddPair('@m', MessageText);

    // Flatten properties
    if Assigned(Properties) then
    begin
      for Pair in Properties do
        LogEntry.AddPair(Pair.JsonString.Clone as TJSONString, Pair.JsonValue.Clone as TJSONValue);
    end;

    // Thread-safe addition to the queue
    FLock.Enter;
    try
      if FLogQueue.Count < 10000 then
        FLogQueue.Enqueue(LogEntry);
    finally
      FLock.Leave;
    end;

    // Flush if batch size is reached
    if FLogQueue.Count >= FBatchSize then
      Flush;
  except
    LogEntry.Free;
    raise;
  end;
end;

// Sends a batch of logs to Seq
procedure TSeqLogger.SendBatch;
var
  Item: TJSONObject;
  Retry: Integer;
  Payload: TStringList;
  ResponseBody: string;
begin
  FLock.Enter;
  try
    if FLogQueue.Count = 0 then Exit;

    Payload := TStringList.Create;
    Payload.LineBreak := #10;
    try
      while (Payload.Count < FBatchSize) and (FLogQueue.Count > 0) do
      begin
        Item := FLogQueue.Dequeue;
        Payload.Add(Item.ToJSON);
      end;

      //Writeln('Payload: ' + Payload.Text); // Output payload for debugging

      Retry := 0;
      while Retry <= FRetryCount do
      begin
        try
          FHttp.Post(FEndpoint, TStringStream.Create(Payload.Text));
          Break;
        except
          on E: EIdHTTPProtocolException do
          begin
            ResponseBody := E.ErrorMessage;
            ResponseBody := E.ErrorMessage;
            //Writeln('HTTP Error ' + IntToStr(E.ErrorCode) + ': ' + ResponseBody);
          end;
          on E: Exception do
          begin
            Inc(Retry);
            //Writeln('Retry ' + IntToStr(Retry) + ': ' + E.Message);
            if Retry > FRetryCount then
              raise;
          end;
        end;
      end;

    finally
      Payload.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

// Flush remaining logs
procedure TSeqLogger.Flush;
begin
  SendBatch;
end;

// Returns a global instance of the logger
procedure TSeqLogger.Log(Level: TLogLevel; const Template: string; const Values: array of Variant);
var
  MessageText: string;
  Properties: TJSONObject;
  i: Integer;
begin
  MessageText := FormatMessage(Template, Values);
  Properties := TJSONObject.Create;
  try
    i := 0;
    while i < High(Values) do
    begin
      Properties.AddPair(VarToStr(Values[i]), TJSONString.Create(VarToStr(Values[i + 1])));
      Inc(i, 2);
    end;
    Log(Level, MessageText, Properties);
  finally
    Properties.Free;
  end;
end;

class function TSeqLogger.GetHostIP(aDefault: string): string;
begin
  Result := GetEnvironmentVariable('SEQ_LOGGER_URL');
  if Result = '' then
    Result := aDefault;
end;

class function TSeqLogger.GetApiKey(aDefault: string): string;
begin
  Result := GetEnvironmentVariable('SEQ_API_KEY');
  if Result = '' then
    Result := aDefault;
end;

class function TSeqLogger.Logger: TSeqLogger;
begin
  if not Assigned(GlobalLogger) then
  begin
    GlobalLogger := TSeqLogger.Create(GetHostIP('http://localhost:5341') + '/api/events/raw?clef',
      GetApiKey('f34Ps2tVcCDqQB36R2jD'), 1000, 3, DEFAULT_FLUSH_TIMEOUT);
  end;
  Result := GlobalLogger;
end;

class function TSeqLogger.StringToLogLevel(const S: string): TLogLevel;
begin
  if SameText(S, 'Verbose') then Result := Verbose
  else if SameText(S, 'Debug') then Result := Debug
  else if SameText(S, 'Information') then Result := Information
  else if SameText(S, 'Warning') then Result := Warning
  else if SameText(S, 'Error') then Result := Error
  else if SameText(S, 'Fatal') then Result := Fatal
  else Result := Information;
end;

initialization

finalization

if Assigned(GlobalLogger) then
  GlobalLogger.Free;

end.

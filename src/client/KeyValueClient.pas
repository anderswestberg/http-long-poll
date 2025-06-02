unit KeyValueClient;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Generics.Collections,
  System.Variants, KeyValueHttpClient, SeqLogger;

type
  TKeyValueClientState = (
    kvsDisconnected,
    kvsConnecting,
    kvsConnected
  );

  TKeyValueClientStateEvent = procedure(Sender: TObject; NewState: TKeyValueClientState) of object;
  TKeyValueChangeEvent = procedure(Sender: TObject; const Key: string; const Value: Variant) of object;

  TKeyValueClient = class
  private
    FClient: TKVHttpClient;
    FLock: TCriticalSection;
    FRetryThread: TThread;
    FLongPollThread: TThread;
    FState: TKeyValueClientState;
    FLastSeenId: Int64;
    FOnStateChange: TKeyValueClientStateEvent;
    FOnValueChange: TKeyValueChangeEvent;
    FBaseUrl: string;
    FClientId: string;
    FRetryInterval: Integer;
    FTerminating: Boolean;
    procedure SetState(NewState: TKeyValueClientState);
    procedure DoStateChange(NewState: TKeyValueClientState);
    procedure DoValueChange(const Key: string; const Value: Variant);
    procedure StartLongPoll;
    procedure StopLongPoll;
    procedure LongPollThreadProc;
    procedure RetryThreadProc;
  protected
    procedure Initialize; virtual;
  public
    constructor Create(const ABaseUrl: string);
    destructor Destroy; override;

    // Thread-safe operations
    function GetValue(const Key: string): Variant;
    procedure SetValue(const Key: string; const Value: Variant);
    procedure SetValues(const Updates: array of TPair<string, Variant>);
    function GetAll: TArray<TPair<string, Variant>>;
    function GetLatestChangeId: Int64;

    // Properties
    property State: TKeyValueClientState read FState;
    property BaseUrl: string read FBaseUrl;
    property ClientId: string read FClientId;
    property RetryInterval: Integer read FRetryInterval write FRetryInterval;
    property OnStateChange: TKeyValueClientStateEvent read FOnStateChange write FOnStateChange;
    property OnValueChange: TKeyValueChangeEvent read FOnValueChange write FOnValueChange;
  end;

implementation

type
  TLongPollThread = class(TThread)
  private
    FClient: TKeyValueClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TKeyValueClient);
  end;

  TRetryThread = class(TThread)
  private
    FClient: TKeyValueClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TKeyValueClient);
  end;

{ TKeyValueClient }

constructor TKeyValueClient.Create(const ABaseUrl: string);
var
  Guid: TGUID;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FBaseUrl := ABaseUrl;
  FRetryInterval := 1000; // 1 second default
  FTerminating := False;

  // Generate client ID
  CreateGUID(Guid);
  FClientId := GUIDToString(Guid);

  // Create HTTP client
  FClient := TKVHttpClient.Create(FBaseUrl);
  FClient.ClientId := FClientId;

  TSeqLogger.Logger.Log(Information, 'KeyValueClient created for {BaseUrl} with ID {ClientId}', ['BaseUrl', FBaseUrl, 'ClientId', FClientId]);

  SetState(kvsDisconnected);
  Initialize;
end;

destructor TKeyValueClient.Destroy;
begin
  FTerminating := True;
  
  // Stop and cleanup retry thread
  if Assigned(FRetryThread) then
  begin
    TRetryThread(FRetryThread).Terminate;
    FRetryThread.WaitFor;
    FreeAndNil(FRetryThread);
  end;

  // Stop and cleanup long poll thread
  StopLongPoll;

  FClient.Free;
  FLock.Free;
  TSeqLogger.Logger.Log(Information, 'KeyValueClient destroyed for {BaseUrl}', ['BaseUrl', FBaseUrl]);
  inherited;
end;

procedure TKeyValueClient.Initialize;
begin
  // Start retry thread
  if not Assigned(FRetryThread) then
    FRetryThread := TRetryThread.Create(Self);
end;

procedure TKeyValueClient.SetState(NewState: TKeyValueClientState);
begin
  if FState <> NewState then
  begin
    FState := NewState;
    TThread.Queue(nil, procedure
    begin
      DoStateChange(NewState);
    end);
  end;
end;

procedure TKeyValueClient.DoStateChange(NewState: TKeyValueClientState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, NewState);
end;

procedure TKeyValueClient.DoValueChange(const Key: string; const Value: Variant);
begin
  if Assigned(FOnValueChange) then
    TThread.Queue(nil, procedure
    begin
      FOnValueChange(Self, Key, Value);
    end);
end;

procedure TKeyValueClient.RetryThreadProc;
begin
  while not FTerminating do
  begin
    if FState = kvsDisconnected then
    begin
      SetState(kvsConnecting);
      TSeqLogger.Logger.Log(Information, 'KeyValueClient attempting connection to {BaseUrl}', ['BaseUrl', FBaseUrl]);

      try
        FLock.Acquire;
        try
          // Test connection by getting latest change ID
          FLastSeenId := FClient.GetLatestChangeId;
          
          // Start long polling
          StartLongPoll;
          
          // Small delay to ensure everything is initialized
          Sleep(100);
          
          SetState(kvsConnected);
          TSeqLogger.Logger.Log(Information, 'KeyValueClient connected to {BaseUrl}', ['BaseUrl', FBaseUrl]);
        finally
          FLock.Release;
        end;
      except
        on E: Exception do
        begin
          TSeqLogger.Logger.Log(Warning, 'KeyValueClient connection failed: {Error}', ['Error', E.Message]);
          SetState(kvsDisconnected);
        end;
      end;
    end;

    // Wait for retry interval
    Sleep(FRetryInterval);
  end;
end;

procedure TKeyValueClient.StartLongPoll;
begin
  if not Assigned(FLongPollThread) and not FTerminating then
  begin
    FLongPollThread := TLongPollThread.Create(Self);
    TSeqLogger.Logger.Log(Information, 'KeyValueClient started long polling from ID {LastSeenId}', ['LastSeenId', FLastSeenId]);
  end;
end;

procedure TKeyValueClient.StopLongPoll;
begin
  if Assigned(FLongPollThread) then
  begin
    TLongPollThread(FLongPollThread).Terminate;
    FClient.CancelLongPoll;
    FLongPollThread.WaitFor;
    FreeAndNil(FLongPollThread);
    TSeqLogger.Logger.Log(Information, 'KeyValueClient stopped long polling');
  end;
end;

procedure TKeyValueClient.LongPollThreadProc;
var
  Changes: TArray<TChangeItem>;
  Change: TChangeItem;
begin
  while not FTerminating and (FState = kvsConnected) do
  begin
    try
      if FClient.LongPoll(FLastSeenId, Changes) then
      begin
        for Change in Changes do
        begin
          if Change.Id > FLastSeenId then
            FLastSeenId := Change.Id;
          DoValueChange(Change.Key, Change.Value);
        end;
      end;
    except
      on E: Exception do
      begin
        TSeqLogger.Logger.Log(Error, 'KeyValueClient long poll error: {Error}', ['Error', E.Message]);
        if not FTerminating then
        begin
          SetState(kvsDisconnected);
          Break;
        end;
      end;
    end;
  end;
end;

function TKeyValueClient.GetValue(const Key: string): Variant;
begin
  FLock.Acquire;
  try
    Result := FClient.GetValue(Key);
  finally
    FLock.Release;
  end;
end;

procedure TKeyValueClient.SetValue(const Key: string; const Value: Variant);
begin
  FLock.Acquire;
  try
    FClient.PostValue(Key, Value);
  finally
    FLock.Release;
  end;
end;

procedure TKeyValueClient.SetValues(const Updates: array of TPair<string, Variant>);
begin
  FLock.Acquire;
  try
    FClient.PostValues(Updates);
  finally
    FLock.Release;
  end;
end;

function TKeyValueClient.GetAll: TArray<TPair<string, Variant>>;
begin
  FLock.Acquire;
  try
    Result := FClient.GetAll;
  finally
    FLock.Release;
  end;
end;

function TKeyValueClient.GetLatestChangeId: Int64;
begin
  FLock.Acquire;
  try
    Result := FClient.GetLatestChangeId;
  finally
    FLock.Release;
  end;
end;

{ TLongPollThread }

constructor TLongPollThread.Create(AClient: TKeyValueClient);
begin
  inherited Create(False);
  FClient := AClient;
  FreeOnTerminate := False;
end;

procedure TLongPollThread.Execute;
begin
  FClient.LongPollThreadProc;
end;

{ TRetryThread }

constructor TRetryThread.Create(AClient: TKeyValueClient);
begin
  inherited Create(False);
  FClient := AClient;
  FreeOnTerminate := False;
end;

procedure TRetryThread.Execute;
begin
  FClient.RetryThreadProc;
end;

end. 
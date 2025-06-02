unit MainClientUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.DateUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, KeyValueClient, Generics.Collections, SeqLogger, LoggerSettings, VariantUtils;

type
  TStressTestThread = class(TThread)
  private
    FClient: TKeyValueClient;
    FStartTime: TDateTime;
    FOperations: Integer;
    FErrors: Integer;
    FLog: TStrings;
    FWrittenKeys: TDictionary<string, Boolean>;
    FClientId: string;
    FTerminating: Boolean;
    procedure LogMessage(const Msg: string);
    procedure UpdateStats;
  public
    constructor Create(AClient: TKeyValueClient; ALog: TStrings; const AClientId: string);
    destructor Destroy; override;
    procedure Execute; override;
    procedure SafeTerminate;
    property Operations: Integer read FOperations;
    property Errors: Integer read FErrors;
  end;

  TMainForm = class(TForm)
    MemoLog: TMemo;
    EditKey: TEdit;
    EditValue: TEdit;
    BtnWrite: TButton;
    BtnRead: TButton;
    LabelKey: TLabel;
    LabelValue: TLabel;
    EditPort: TEdit;
    LabelPort: TLabel;
    BtnConnect: TButton;
    BtnDisconnect: TButton;
    EditClientId: TEdit;
    LabelClientId: TLabel;
    CheckBoxLogging: TCheckBox;
    ComboBoxLogLevel: TComboBox;
    LabelLogLevel: TLabel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    BtnStartStressTest: TButton;
    BtnStopStressTest: TButton;
    LabelStatus: TLabel;
    LabelInstanceInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnWriteClick(Sender: TObject);
    procedure BtnReadClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBoxLoggingClick(Sender: TObject);
    procedure ComboBoxLogLevelChange(Sender: TObject);
    procedure BtnStartStressTestClick(Sender: TObject);
    procedure BtnStopStressTestClick(Sender: TObject);
  private
    FClient: TKeyValueClient;
    FStressTest: TStressTestThread;
    FClientId: string;
    procedure Log(const Msg: string);
    procedure HandleStateChange(Sender: TObject; NewState: TKeyValueClientState);
    procedure HandleValueChange(Sender: TObject; const Key: string; const Value: Variant);
    procedure UpdateStatusLabel;
    procedure InitializeLogLevels;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{ TStressTestThread }

constructor TStressTestThread.Create(AClient: TKeyValueClient; ALog: TStrings; const AClientId: string);
begin
  inherited Create(True); // Create suspended
  FClient := AClient;
  FLog := ALog;
  FClientId := AClientId;
  FWrittenKeys := TDictionary<string, Boolean>.Create;
  FStartTime := 0;
  FOperations := 0;
  FErrors := 0;
  FTerminating := False;
  FreeOnTerminate := False;
end;

destructor TStressTestThread.Destroy;
begin
  FWrittenKeys.Free;
  inherited;
end;

procedure TStressTestThread.LogMessage(const Msg: string);
begin
  TThread.Queue(nil, procedure
  begin
    FLog.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + Msg);
  end);
end;

procedure TStressTestThread.UpdateStats;
var
  ElapsedSecs: Double;
  OpsPerSec: Double;
begin
  ElapsedSecs := SecondsBetween(Now, FStartTime);
  if ElapsedSecs > 0 then
  begin
    OpsPerSec := FOperations / ElapsedSecs;
    LogMessage(Format('Stress Test Stats: %d ops in %.1f seconds (%.1f ops/sec), %d errors (%.1f%% success rate)',
      [FOperations, ElapsedSecs, OpsPerSec, FErrors,
       100 * (FOperations - FErrors) / FOperations]));
  end;
end;

procedure TStressTestThread.SafeTerminate;
begin
  FTerminating := True;
  Terminate;
end;

procedure TStressTestThread.Execute;
var
  Key: string;
  Value: Variant;
  i: Integer;
  LastStatsUpdate: TDateTime;
  BatchSize: Integer;
  BatchValues: array of TPair<string, Variant>;
  WriteMode: Boolean;
begin
  // Wait for client to be connected
  while not FTerminating and not Terminated and (FClient.State <> kvsConnected) do
  begin
    LogMessage('[Stress Test] Waiting for client to connect...');
    Sleep(1000);
  end;

  if FTerminating or Terminated then
    Exit;

  FStartTime := Now;
  LastStatsUpdate := FStartTime;
  LogMessage('Starting stress test...');
  BatchSize := 20; // Increased batch size
  WriteMode := True; // Start with writes

  // Initialize first 5 keys with client-specific prefix
  SetLength(BatchValues, 5);
  for i := 0 to 4 do
  begin
    if FTerminating or Terminated then
      Exit;

    Key := Format('%s.stress.test.%d', [FClientId, i]);
    Value := Format('initial-value-%d', [i]);
    BatchValues[i].Key := Key;
    BatchValues[i].Value := Value;
    FWrittenKeys.Add(Key, True);
  end;

  try
    if not FTerminating and not Terminated then
    begin
      FClient.SetValues(BatchValues);
      Inc(FOperations, Length(BatchValues));
      LogMessage(Format('[Stress Test] Initial batch write: %d values', [Length(BatchValues)]));
    end;
  except
    on E: Exception do
    begin
      Inc(FErrors);
      LogMessage('[Stress Test Error] ' + E.Message);
    end;
  end;

  // Main test loop
  SetLength(BatchValues, BatchSize);
  while not FTerminating and not Terminated do
  begin
    if WriteMode then
    begin
      // Prepare batch of writes
      for i := 0 to BatchSize - 1 do
      begin
        Key := Format('%s.stress.test.%d', [FClientId, Random(5)]);
        Value := Format('value-%d', [Random(1000)]);
        BatchValues[i].Key := Key;
        BatchValues[i].Value := Value;
      end;

      // Execute batch write
      try
        if not FTerminating and not Terminated then
        begin
          FClient.SetValues(BatchValues);
          Inc(FOperations, BatchSize);
        end;
      except
        on E: Exception do
        begin
          Inc(FErrors);
          LogMessage('[Stress Test Error] ' + E.Message);
        end;
      end;
    end
    else
    begin
      // Do a batch of reads (can't actually batch them, but we'll do them in sequence)
      for i := 0 to BatchSize - 1 do
      begin
        if FTerminating or Terminated then
          Break;
          
        Key := Format('%s.stress.test.%d', [FClientId, Random(5)]);
        try
          Value := FClient.GetValue(Key);
          Inc(FOperations);
        except
          on E: Exception do
          begin
            Inc(FErrors);
            LogMessage('[Stress Test Error] ' + E.Message);
          end;
        end;
      end;
    end;

    // Toggle between read and write modes
    WriteMode := not WriteMode;

    // Update stats every 5 seconds
    if SecondsBetween(Now, LastStatsUpdate) >= 5 then
    begin
      UpdateStats;
      LastStatsUpdate := Now;
    end;
  end;

  // Final stats update
  UpdateStats;
  LogMessage('Stress test stopped.');
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  LogEnabled: Boolean;
  LogLevel: TLogLevel;
  Params: TStringList;
  ServerPort: Integer;
begin
  // Initialize logging controls
  InitializeLogLevels;
  
  // Load saved settings
  TLoggerSettings.LoadSettings(LogEnabled, LogLevel);
  CheckBoxLogging.Checked := LogEnabled;
  TSeqLogger.Logger.Enabled := LogEnabled;
  ComboBoxLogLevel.ItemIndex := Ord(LogLevel);
  TSeqLogger.Logger.MinLogLevel := LogLevel;

  // Parse command line parameters
  Params := TStringList.Create;
  try
    Params.Delimiter := ' ';
    Params.DelimitedText := GetCommandLine;
    
    // Get client ID from command line or generate one
    if (Params.Count >= 2) and (Params[1] <> '') then
      FClientId := Params[1]
    else
    begin
      var Guid: TGUID;
      CreateGUID(Guid);
      FClientId := Copy(GUIDToString(Guid), 2, 8); // Use first 8 chars of GUID
    end;

    // Get server port from command line or use default
    if (Params.Count >= 3) and TryStrToInt(Params[2], ServerPort) then
      ServerPort := StrToInt(Params[2])
    else
      ServerPort := 8868;

    EditPort.Text := IntToStr(ServerPort);
    EditClientId.Text := FClientId;
    Caption := Format('Key-Value Client Demo - Instance %s', [FClientId]);
    LabelInstanceInfo.Caption := Format('Instance ID: %s, Server Port: %d', [FClientId, ServerPort]);
  finally
    Params.Free;
  end;

  TSeqLogger.Logger.Log(Information, 'KeyValue Client Demo started - Instance {ClientId}', ['ClientId', FClientId]);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // First log that we're stopping
  TSeqLogger.Logger.Log(Information, 'KeyValue Client stopping...');

  // Stop stress test first if running
  if Assigned(FStressTest) then
  begin
    FStressTest.SafeTerminate;
    FStressTest.WaitFor;
    FreeAndNil(FStressTest);
  end;
  
  // Now disconnect the client
  if Assigned(FClient) then
  begin
    TSeqLogger.Logger.Log(Information, 'Disconnecting client...');
    FreeAndNil(FClient);
  end;
    
  // Save settings before we disable logging
  TLoggerSettings.SaveSettings(
    CheckBoxLogging.Checked,
    TSeqLogger.StringToLogLevel(ComboBoxLogLevel.Text)
  );
  
  // Final log message
  TSeqLogger.Logger.Log(Information, 'KeyValue Client stopped');
end;

procedure TMainForm.Log(const Msg: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + Msg);
end;

procedure TMainForm.UpdateStatusLabel;
begin
  if not Assigned(FClient) then
    LabelStatus.Caption := 'Status: Not Created'
  else
    case FClient.State of
      kvsDisconnected: LabelStatus.Caption := 'Status: Disconnected';
      kvsConnecting: LabelStatus.Caption := 'Status: Connecting...';
      kvsConnected: LabelStatus.Caption := 'Status: Connected (Long polling active)';
    end;
end;

procedure TMainForm.HandleStateChange(Sender: TObject; NewState: TKeyValueClientState);
begin
  case NewState of
    kvsDisconnected:
    begin
      Log('Client disconnected');
      BtnWrite.Enabled := False;
      BtnRead.Enabled := False;
      BtnStartStressTest.Enabled := False;
      BtnStopStressTest.Enabled := False;
      BtnConnect.Enabled := True;
      BtnDisconnect.Enabled := False;
    end;
    kvsConnecting:
      Log('Client connecting...');
    kvsConnected:
    begin
      Log('Client connected - Long polling started');
      BtnWrite.Enabled := True;
      BtnRead.Enabled := True;
      BtnStartStressTest.Enabled := True;
      BtnStopStressTest.Enabled := False;
      BtnConnect.Enabled := False;
      BtnDisconnect.Enabled := True;
    end;
  end;
  UpdateStatusLabel;
end;

procedure TMainForm.HandleValueChange(Sender: TObject; const Key: string; const Value: Variant);
begin
  Log(Format('Long poll update: %s = %s', [Key, VariantToStringWithType(Value)]));
  // If this is the key we're currently viewing, update the value field
  if SameText(Key, Trim(EditKey.Text)) then
    EditValue.Text := VarToStr(Value);
end;

procedure TMainForm.BtnConnectClick(Sender: TObject);
var
  Port: Integer;
begin
  if not TryStrToInt(EditPort.Text, Port) then
  begin
    ShowMessage('Invalid port number');
    Exit;
  end;

  if Assigned(FClient) then
    FreeAndNil(FClient);

  FClientId := EditClientId.Text;
  FClient := TKeyValueClient.Create(Format('http://localhost:%d', [Port]));
  FClient.OnStateChange := HandleStateChange;
  FClient.OnValueChange := HandleValueChange;
  Log('Client created, waiting for connection...');
  UpdateStatusLabel;
end;

procedure TMainForm.BtnDisconnectClick(Sender: TObject);
begin
  if Assigned(FClient) then
  begin
    FClient.Free;
    FClient := nil;
    UpdateStatusLabel;
  end;
end;

procedure TMainForm.BtnWriteClick(Sender: TObject);
var
  Key, ValueStr, ConversionError: string;
  Value: Variant;
begin
  if Trim(EditKey.Text) = '' then
  begin
    ShowMessage('Key must not be empty');
    Exit;
  end;
  
  Key := Trim(EditKey.Text);
  ValueStr := EditValue.Text;

  Value := StringToTypedVariant(ValueStr, ConversionError);
  if ConversionError <> '' then
    Log('[Warning] ' + ConversionError);

  try
    FClient.SetValue(Key, Value);
    Log(Format('PUT %s = %s', [Key, VariantToStringWithType(Value)]));
  except
    on E: Exception do
      Log('[Error] ' + E.Message);
  end;
end;

procedure TMainForm.BtnReadClick(Sender: TObject);
var
  Value: Variant;
begin
  if Trim(EditKey.Text) = '' then
  begin
    ShowMessage('Key must not be empty');
    Exit;
  end;
  try
    Value := FClient.GetValue(Trim(EditKey.Text));
    if not VarIsNull(Value) then
    begin
      EditValue.Text := VarToStr(Value);
      Log(Format('GET %s = %s', [Trim(EditKey.Text), VarToStr(Value)]));
    end
    else
      Log(Format('GET %s = <not found>', [Trim(EditKey.Text)]));
  except
    on E: Exception do
      Log('[Error] ' + E.Message);
  end;
end;

procedure TMainForm.BtnStartStressTestClick(Sender: TObject);
begin
  if not Assigned(FStressTest) then
  begin
    FStressTest := TStressTestThread.Create(FClient, MemoLog.Lines, FClientId);
    FStressTest.Start;
    BtnStartStressTest.Enabled := False;
    BtnStopStressTest.Enabled := True;
  end;
end;

procedure TMainForm.BtnStopStressTestClick(Sender: TObject);
begin
  if Assigned(FStressTest) then
  begin
    FStressTest.SafeTerminate;
    FStressTest.WaitFor;
    FreeAndNil(FStressTest);
    BtnStartStressTest.Enabled := True;
    BtnStopStressTest.Enabled := False;
  end;
end;

procedure TMainForm.InitializeLogLevels;
begin
  ComboBoxLogLevel.Items.Clear;
  ComboBoxLogLevel.Items.Add('Verbose');
  ComboBoxLogLevel.Items.Add('Debug');
  ComboBoxLogLevel.Items.Add('Information');
  ComboBoxLogLevel.Items.Add('Warning');
  ComboBoxLogLevel.Items.Add('Error');
  ComboBoxLogLevel.Items.Add('Fatal');
  ComboBoxLogLevel.ItemIndex := 2; // Default to Information
end;

procedure TMainForm.CheckBoxLoggingClick(Sender: TObject);
begin
  TSeqLogger.Logger.Enabled := CheckBoxLogging.Checked;
end;

procedure TMainForm.ComboBoxLogLevelChange(Sender: TObject);
begin
  TSeqLogger.Logger.MinLogLevel := TSeqLogger.StringToLogLevel(ComboBoxLogLevel.Text);
end;

end. 
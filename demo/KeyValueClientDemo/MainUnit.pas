unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.DateUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  KeyValueClient, Generics.Collections, SeqLogger, VariantUtils;

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
    procedure LogMessage(const Msg: string);
    procedure UpdateStats;
  public
    constructor Create(AClient: TKeyValueClient; ALog: TStrings; const AClientId: string);
    destructor Destroy; override;
    procedure Execute; override;
    property Operations: Integer read FOperations;
    property Errors: Integer read FErrors;
  end;

  TMainForm = class(TForm)
    MemoLog: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    LabelKey: TLabel;
    LabelValue: TLabel;
    EditKey: TEdit;
    EditValue: TEdit;
    BtnWrite: TButton;
    BtnRead: TButton;
    BtnStartStressTest: TButton;
    BtnStopStressTest: TButton;
    LabelStatus: TLabel;
    LabelInstanceInfo: TLabel;
    procedure BtnWriteClick(Sender: TObject);
    procedure BtnReadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnStartStressTestClick(Sender: TObject);
    procedure BtnStopStressTestClick(Sender: TObject);
  private
    FClient: TKeyValueClient;
    FStressTest: TStressTestThread;
    FClientId: string;
    procedure Log(const S: string);
    procedure OnClientStateChange(Sender: TObject; NewState: TKeyValueClientState);
    procedure OnClientValueChange(Sender: TObject; const Key: string; const Value: Variant);
    procedure UpdateStatusLabel;
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

procedure TStressTestThread.Execute;
var
  Key: string;
  Value: Variant;
  i: Integer;
  LastStatsUpdate: TDateTime;
begin
  // Wait for client to be connected
  while not Terminated and (FClient.State <> kvsConnected) do
  begin
    LogMessage('[Stress Test] Waiting for client to connect...');
    Sleep(1000);
  end;

  if Terminated then
    Exit;

  FStartTime := Now;
  LastStatsUpdate := FStartTime;
  LogMessage('Starting stress test...');

  // Initialize first 5 keys with client-specific prefix
  for i := 0 to 4 do
  begin
    if Terminated then
      Exit;

    Key := Format('%s.stress.test.%d', [FClientId, i]);
    Value := Format('initial-value-%d', [i]);
    try
      FClient.SetValue(Key, Value);
      FWrittenKeys.Add(Key, True);
      Inc(FOperations);
      LogMessage(Format('[Stress Test] Initial write: %s = %s', [Key, VarToStr(Value)]));
    except
      on E: Exception do
      begin
        Inc(FErrors);
        LogMessage('[Stress Test Error] ' + E.Message);
      end;
    end;
  end;

  // Main test loop
  while not Terminated do
  begin
    // Randomly choose between read and write
    if Random(2) = 0 then
    begin
      // Read operation
      Key := Format('%s.stress.test.%d', [FClientId, Random(5)]);
      try
        Value := FClient.GetValue(Key);
        Inc(FOperations);
        LogMessage(Format('[Stress Test] Read %s = %s', [Key, VarToStr(Value)]));
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
      // Write operation
      Key := Format('%s.stress.test.%d', [FClientId, Random(5)]);
      Value := Format('value-%d', [Random(1000)]);
      try
        FClient.SetValue(Key, Value);
        FWrittenKeys.AddOrSetValue(Key, True);
        Inc(FOperations);
        LogMessage(Format('[Stress Test] Write %s = %s', [Key, VarToStr(Value)]));
      except
        on E: Exception do
        begin
          Inc(FErrors);
          LogMessage('[Stress Test Error] ' + E.Message);
        end;
      end;
    end;

    // Update stats every 5 seconds
    if SecondsBetween(Now, LastStatsUpdate) >= 5 then
    begin
      UpdateStats;
      LastStatsUpdate := Now;
    end;

    // Small delay to prevent overwhelming the server
    Sleep(5);
  end;

  // Final stats update
  UpdateStats;
  LogMessage('Stress test stopped.');
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Params: TStringList;
  ServerPort: Integer;
begin
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

    Caption := Format('Key-Value Client Demo - Instance %s', [FClientId]);
    LabelInstanceInfo.Caption := Format('Instance ID: %s, Server Port: %d', [FClientId, ServerPort]);
  finally
    Params.Free;
  end;

  TSeqLogger.Logger.Log(Information, 'KeyValue Client Demo started - Instance {ClientId}', ['ClientId', FClientId]);
  FClient := TKeyValueClient.Create(Format('http://localhost:%d', [ServerPort]));
  FClient.OnStateChange := OnClientStateChange;
  FClient.OnValueChange := OnClientValueChange;
  Log('Client created, waiting for connection...');
  UpdateStatusLabel;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FStressTest) then
  begin
    FStressTest.Terminate;
    FStressTest.WaitFor;
    FreeAndNil(FStressTest);
  end;
  FClient.Free;
end;

procedure TMainForm.Log(const S: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);
end;

procedure TMainForm.UpdateStatusLabel;
begin
  case FClient.State of
    kvsDisconnected: LabelStatus.Caption := 'Status: Disconnected';
    kvsConnecting: LabelStatus.Caption := 'Status: Connecting...';
    kvsConnected: LabelStatus.Caption := 'Status: Connected (Long polling active)';
  end;
end;

procedure TMainForm.OnClientStateChange(Sender: TObject; NewState: TKeyValueClientState);
begin
  case NewState of
    kvsDisconnected:
    begin
      Log('Client disconnected');
      BtnWrite.Enabled := False;
      BtnRead.Enabled := False;
      BtnStartStressTest.Enabled := False;
      BtnStopStressTest.Enabled := False;
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
    end;
  end;
  UpdateStatusLabel;
end;

procedure TMainForm.OnClientValueChange(Sender: TObject; const Key: string; const Value: Variant);
begin
  Log(Format('Long poll update: %s = %s', [Key, VariantToStringWithType(Value)]));
  // If this is the key we're currently viewing, update the value field
  if SameText(Key, Trim(EditKey.Text)) then
    EditValue.Text := VarToStr(Value);
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
    FStressTest.Terminate;
    FStressTest.WaitFor;
    FreeAndNil(FStressTest);
    BtnStartStressTest.Enabled := True;
    BtnStopStressTest.Enabled := False;
  end;
end;

end.



unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.DateUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  KeyValueClientUnit, Generics.Collections, SeqloggerClass;

type
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
    StressTestTimer: TTimer;
    procedure BtnWriteClick(Sender: TObject);
    procedure BtnReadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnStartStressTestClick(Sender: TObject);
    procedure BtnStopStressTestClick(Sender: TObject);
    procedure StressTestTimerTimer(Sender: TObject);
  private
    FClient: TKeyValueClient;
    FStressTestStartTime: TDateTime;
    FStressTestOps: Integer;
    FStressTestErrors: Integer;
    FWrittenKeys: TDictionary<string, Boolean>;  // Track which keys have been written
    FInitialWrites: Integer;  // Track initial writes
    procedure Log(const S: string);
    procedure UpdateStressTestStats;
    procedure OnClientStateChange(Sender: TObject; NewState: TKeyValueClientState);
    procedure OnClientValueChange(Sender: TObject; const Key: string; const Value: Variant);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TSeqLogger.Logger.Log(Information, 'KVClientTest started');
  FClient := TKeyValueClient.Create('http://localhost:8868');
  FClient.OnStateChange := OnClientStateChange;
  FClient.OnValueChange := OnClientValueChange;
  FWrittenKeys := TDictionary<string, Boolean>.Create;
  Log('Client created, waiting for connection...');
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FWrittenKeys.Free;
  FClient.Free;
end;

procedure TMainForm.Log(const S: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);
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
      Log('Client connected');
      BtnWrite.Enabled := True;
      BtnRead.Enabled := True;
      BtnStartStressTest.Enabled := True;
      BtnStopStressTest.Enabled := False;
    end;
  end;
end;

procedure TMainForm.OnClientValueChange(Sender: TObject; const Key: string; const Value: Variant);
begin
  Log(Format('Value changed: %s = %s', [Key, VarToStr(Value)]));
end;

procedure TMainForm.BtnWriteClick(Sender: TObject);
begin
  if Trim(EditKey.Text) = '' then
  begin
    ShowMessage('Key must not be empty');
    Exit;
  end;
  try
    FClient.SetValue(Trim(EditKey.Text), EditValue.Text);
    Log(Format('PUT %s = %s', [Trim(EditKey.Text), EditValue.Text]));
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
  FStressTestStartTime := Now;
  FStressTestOps := 0;
  FStressTestErrors := 0;
  FWrittenKeys.Clear;
  FInitialWrites := 0;
  
  // Start with a small delay to ensure connection is fully established
  Sleep(200);
  
  StressTestTimer.Interval := 50; // Change from 10ms to 50ms for better stability
  StressTestTimer.Enabled := True;
  BtnStartStressTest.Enabled := False;
  BtnStopStressTest.Enabled := True;
  Log('Starting stress test...');
end;

procedure TMainForm.BtnStopStressTestClick(Sender: TObject);
begin
  StressTestTimer.Enabled := False;
  BtnStartStressTest.Enabled := True;
  BtnStopStressTest.Enabled := False;
  UpdateStressTestStats;
  Log('Stress test stopped.');
end;

procedure TMainForm.UpdateStressTestStats;
var
  ElapsedSecs: Double;
  OpsPerSec: Double;
begin
  ElapsedSecs := SecondsBetween(Now, FStressTestStartTime);
  if ElapsedSecs > 0 then
  begin
    OpsPerSec := FStressTestOps / ElapsedSecs;
    Log(Format('Stress Test Stats: %d ops in %.1f seconds (%.1f ops/sec), %d errors (%.1f%% success rate)',
      [FStressTestOps, ElapsedSecs, OpsPerSec, FStressTestErrors,
       100 * (FStressTestOps - FStressTestErrors) / FStressTestOps]));
  end;
end;

procedure TMainForm.StressTestTimerTimer(Sender: TObject);
var
  Op: Integer;
  Key: string;
  Value: Variant;
  i: Integer;
  BatchUpdates: TArray<TPair<string, Variant>>;
  BatchSize: Integer;
  ReadCount: Integer;
begin
  // Only perform operations when client is connected
  if FClient.State <> kvsConnected then
  begin
    Log('[Stress Test] Waiting for client to connect...');
    Exit;
  end;

  try
    // Initial phase: Write to first 5 keys to ensure we have data
    if FInitialWrites < 5 then
    begin
      SetLength(BatchUpdates, 5 - FInitialWrites);
      BatchSize := 0;
      
      for i := 0 to 4 do
      begin
        Key := Format('stress.test.%d', [i]);
        if not FWrittenKeys.ContainsKey(Key) then
        begin
          Value := Format('initial-value-%d-%d', [i, FStressTestOps]);
          BatchUpdates[BatchSize] := TPair<string, Variant>.Create(Key, Value);
          Inc(BatchSize);
          FWrittenKeys.AddOrSetValue(Key, True);
          Inc(FInitialWrites);
        end;
      end;
      
      if BatchSize > 0 then
      begin
        SetLength(BatchUpdates, BatchSize);
        try
          FClient.SetValues(BatchUpdates);
          Inc(FStressTestOps, BatchSize);
          Log(Format('[Stress Test] Initial batch write: %d keys', [BatchSize]));
        except
          on E: Exception do
          begin
            Inc(FStressTestErrors);
            Log('[Stress Test Error] Initial batch write failed: ' + E.Message);
            Dec(FInitialWrites, BatchSize); // Rollback the writes that failed
            for i := 0 to BatchSize - 1 do
              FWrittenKeys.Remove(BatchUpdates[i].Key);
          end;
        end;
      end;
      
      if (FStressTestOps mod 100) = 0 then
        UpdateStressTestStats;
      Exit;
    end;

    // After initial phase, do multiple operations per timer tick
    SetLength(BatchUpdates, 3); // Reduce from 5 to 3 operations per tick
    BatchSize := 0;
    ReadCount := 0;
    
    for i := 1 to 3 do // Reduce from 5 to 3 iterations
    begin
      Key := Format('stress.test.%d', [Random(10)]);

      // Ensure we do some reads (at least 1 per batch)
      if (not FWrittenKeys.ContainsKey(Key)) or 
         ((Random(2) = 1) and (ReadCount < 2)) then // Allow max 2 reads per batch
      begin
        // Write operation
        Value := Format('value-%d-%d', [Random(1000), FStressTestOps]);
        BatchUpdates[BatchSize] := TPair<string, Variant>.Create(Key, Value);
        Inc(BatchSize);
        FWrittenKeys.AddOrSetValue(Key, True);
      end
      else
      begin
        // Read operation
        try
          Value := FClient.GetValue(Key);
          Inc(ReadCount);
        except
          on E: Exception do
          begin
            Inc(FStressTestErrors);
            Log('[Stress Test Error] Read failed: ' + E.Message);
          end;
        end;
      end;
      Inc(FStressTestOps);
    end;

    // Send batch updates if any
    if BatchSize > 0 then
    begin
      SetLength(BatchUpdates, BatchSize);
      try
        FClient.SetValues(BatchUpdates);
      except
        on E: Exception do
        begin
          Inc(FStressTestErrors);
          Log('[Stress Test Error] Batch write failed: ' + E.Message);
          // Remove the keys that failed to write
          for i := 0 to BatchSize - 1 do
            FWrittenKeys.Remove(BatchUpdates[i].Key);
        end;
      end;
    end;

    if (FStressTestOps mod 100) = 0 then
      UpdateStressTestStats;
  except
    on E: Exception do
    begin
      Inc(FStressTestErrors);
      Log('[Stress Test Error] ' + E.Message);
    end;
  end;
end;

end.



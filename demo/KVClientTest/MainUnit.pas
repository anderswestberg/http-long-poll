unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  KVHttpClient, KVLongPollThread, Generics.Collections;

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
    BtnStartLongPoll: TButton;
    BtnStopLongPoll: TButton;
    BtnStartStressTest: TButton;
    BtnStopStressTest: TButton;
    StressTestTimer: TTimer;
    procedure BtnWriteClick(Sender: TObject);
    procedure BtnReadClick(Sender: TObject);
    procedure BtnStartLongPollClick(Sender: TObject);
    procedure BtnStopLongPollClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnStartStressTestClick(Sender: TObject);
    procedure BtnStopStressTestClick(Sender: TObject);
    procedure StressTestTimerTimer(Sender: TObject);
  private
    FClient: TKVHttpClient;
    FPollThread: TKVLongPollThread;
    FLastSeenId: Int64;
    FClientId: string;
    FStressTestStartTime: TDateTime;
    FStressTestOps: Integer;
    FStressTestErrors: Integer;
    procedure OnLongPollUpdate(const Batch: TArray<TChangeItem>);
    procedure Log(const S: string);
    procedure UpdateStressTestStats;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  Guid: TGUID;
  AllValues: TArray<TPair<string, Variant>>;
  Pair: TPair<string, Variant>;
begin
  FClient := TKVHttpClient.Create('http://localhost:8868');
  FPollThread := nil;
  
  // Generate a GUID as client ID
  CreateGUID(Guid);
  FClientId := GUIDToString(Guid);
  FClient.ClientId := FClientId;
  Log('Client ID: ' + FClientId);

  // Get latest change ID to start from
  try
    FLastSeenId := FClient.GetLatestChangeId;
    Log('Starting from change ID: ' + IntToStr(FLastSeenId));

    // Get all current values
    AllValues := FClient.GetAll;
    Log('Initial values:');
    for Pair in AllValues do
      Log(Format('  %s = %s', [Pair.Key, VarToStr(Pair.Value)]));
  except
    on E: Exception do
    begin
      FLastSeenId := 0;
      Log('[Error during initialization] ' + E.Message);
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BtnStopLongPollClick(nil);
  FClient.Free;
end;

procedure TMainForm.Log(const S: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);
end;

procedure TMainForm.OnLongPollUpdate(const Batch: TArray<TChangeItem>);
var
  i: Integer;
begin
  if Length(Batch) = 0 then
    Log('[Timeout/No changes]')
  else
    for i := 0 to High(Batch) do
    begin
      Log(Format('Change: id=%d key=%s value=%s time=%s', [
        Batch[i].Id, Batch[i].Key, Batch[i].Value, Batch[i].Timestamp
      ]));
      if Batch[i].Id > FLastSeenId then
        FLastSeenId := Batch[i].Id;
    end;
end;

procedure TMainForm.BtnWriteClick(Sender: TObject);
begin
  if Trim(EditKey.Text) = '' then
  begin
    ShowMessage('Key must not be empty');
    Exit;
  end;
  try
    FClient.PostValue(Trim(EditKey.Text), EditValue.Text);
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

procedure TMainForm.BtnStartLongPollClick(Sender: TObject);
begin
  BtnStopLongPollClick(nil); // ensure old thread stopped
  FPollThread := TKVLongPollThread.Create(FClient, FLastSeenId, OnLongPollUpdate);
  Log('Started long polling from id: ' + IntToStr(FLastSeenId));
end;

procedure TMainForm.BtnStopLongPollClick(Sender: TObject);
begin
  if Assigned(FPollThread) then
  begin
    FPollThread.Stop;
    FPollThread.Terminate;
    FPollThread.WaitFor;
    FreeAndNil(FPollThread);
    Log('Stopped long poll.');
  end;
end;

procedure TMainForm.BtnStartStressTestClick(Sender: TObject);
begin
  FStressTestStartTime := Now;
  FStressTestOps := 0;
  FStressTestErrors := 0;
  StressTestTimer.Interval := 100; // 100ms between operations
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
begin
  try
    Op := Random(2); // 0 = read, 1 = write
    Key := Format('stress.test.%d', [Random(10)]); // Use 10 different keys

    if Op = 0 then
    begin
      // Read operation
      Value := FClient.GetValue(Key);
      Inc(FStressTestOps);
      if (FStressTestOps mod 100) = 0 then
        UpdateStressTestStats;
    end
    else
    begin
      // Write operation
      Value := Format('value-%d-%d', [Random(1000), FStressTestOps]);
      FClient.PostValue(Key, Value);
      Inc(FStressTestOps);
      if (FStressTestOps mod 100) = 0 then
        UpdateStressTestStats;
    end;
  except
    on E: Exception do
    begin
      Inc(FStressTestErrors);
      Log('[Stress Test Error] ' + E.Message);
    end;
  end;
end;

end.


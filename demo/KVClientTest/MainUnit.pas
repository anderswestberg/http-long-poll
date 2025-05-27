unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  KVHttpClient, KVLongPollThread;

type
  TMainForm = class(TForm)
    MemoLog: TMemo;
    LabelKey: TLabel;
    EditKey: TEdit;
    LabelValue: TLabel;
    EditValue: TEdit;
    BtnWrite: TButton;
    BtnStartLongPoll: TButton;
    BtnStopLongPoll: TButton;
    procedure BtnWriteClick(Sender: TObject);
    procedure BtnStartLongPollClick(Sender: TObject);
    procedure BtnStopLongPollClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FClient: TKVHttpClient;
    FPollThread: TKVLongPollThread;
    FLastSeenId: Int64;
    FClientId: string;
    procedure OnLongPollUpdate(const Batch: TArray<TChangeItem>);
    procedure Log(const S: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  Guid: TGUID;
begin
  FClient := TKVHttpClient.Create('http://localhost:8080');
  FPollThread := nil;
  FLastSeenId := 0;
  // Generate a GUID as client ID
  CreateGUID(Guid);
  FClientId := GUIDToString(Guid);
  FClient.ClientId := FClientId;
  Log('Client ID: ' + FClientId);
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

end.


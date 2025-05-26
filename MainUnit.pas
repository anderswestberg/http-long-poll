unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, KeyValueServerUnit, KeyValueHTTPBridgeUnit,
  SynCommons;

type
  TMainForm = class(TForm)
    MemoLog: TMemo;
    BtnStart: TButton;
    BtnStop: TButton;
    Timer1: TTimer;
    EditKey: TEdit;
    EditValue: TEdit;
    BtnWriteKV: TButton;
    LabelKey: TLabel;
    LabelValue: TLabel;
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnWriteKVClick(Sender: TObject);
  private
    FKV: TKeyValueServer;
    FBridge: TKeyValueHTTPBridge;
    procedure Log(const S: string);
    procedure UpdateKVDisplay;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Log(const S: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);
end;

procedure TMainForm.UpdateKVDisplay;
var
  Output: TArray<string>;
  S: string;
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add('');
    MemoLog.Lines.Add('Current Key/Value Store:');
    if Assigned(FKV) then
    begin
      FKV.GetAll(Output);
      for S in Output do
        MemoLog.Lines.Add('  ' + S);
    end;
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TMainForm.BtnStartClick(Sender: TObject);
begin
  if Assigned(FBridge) then Exit;
  FKV := TKeyValueServer.Create;
  FBridge := TKeyValueHTTPBridge.Create(8080, FKV);
  FBridge.Start;
  Timer1.Enabled := True;
  Log('Server started on port 8080.');
  UpdateKVDisplay;
end;

procedure TMainForm.BtnStopClick(Sender: TObject);
begin
  Timer1.Enabled := False;
  if Assigned(FBridge) then
  begin
    FBridge.Stop;
    FBridge.Free;
    FBridge := nil;
    FKV.Free;
    FKV := nil;
    Log('Server stopped.');
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BtnStopClick(nil);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  UpdateKVDisplay;
end;

procedure TMainForm.BtnWriteKVClick(Sender: TObject);
var
  Key, ValueStr: string;
  Value: Variant;
  IntValue: Int64;
  DoubleValue: Double;
  BoolValue: Boolean;
begin
  if not Assigned(FKV) then
    Exit;
  Key := Trim(EditKey.Text);
  ValueStr := EditValue.Text;
  if Key = '' then
  begin
    ShowMessage('Key must not be empty');
    Exit;
  end;

  // Try to parse as different types
  if TryStrToInt64(ValueStr, IntValue) then
    Value := IntValue
  else if TryStrToFloat(ValueStr, DoubleValue) then
    Value := DoubleValue
  else if SameText(ValueStr, 'true') then
    Value := True
  else if SameText(ValueStr, 'false') then
    Value := False
  else if ValueStr.StartsWith('{') or ValueStr.StartsWith('[') then
    // Try to parse as JSON
    try
      Value := _JSON(ValueStr);
    except
      Value := ValueStr;
    end
  else
    Value := ValueStr;

  FKV.SetValue(Key, Value);
  Log(Format('Manually wrote: %s=%s', [Key, ValueStr]));
  UpdateKVDisplay;
end;

end.


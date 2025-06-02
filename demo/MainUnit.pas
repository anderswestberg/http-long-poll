unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, KeyValueStore, KeyValueHttpBridge,
  SynCommons, Seqlogger, Generics.Collections, VariantUtils;

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
    Button1: TButton;
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnWriteKVClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FKV: IKeyValueStore;
    FBridge: TKeyValueHTTPBridge;
    LogStrings: TStringList;
    lastLog: string;
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
  Output: TArray<TPair<string, variant>>;
  S: TPair<string, variant>;
begin
  if not Assigned(LogStrings) then
    Exit;
  MemoLog.Lines.BeginUpdate;
  try
    FKV.GetAll(Output);
    LogStrings.Clear;
    for S in Output do
      LogStrings.Add('  ' + S.Key + '=' + VariantToString(S.Value));
    if LogStrings.Text <> LastLog then
    begin
      LastLog := LogStrings.Text;
      MemoLog.Lines.Add('Current Key/Value Store:');
      if Assigned(FKV) then
      begin
        for S in Output do
          MemoLog.Lines.Add('  ' + S.Key + '=' + VariantToString(S.Value));
      end;
    end;
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TMainForm.BtnStartClick(Sender: TObject);
begin
  if Assigned(FBridge) then Exit;
  FKV := CreateDefaultKeyValueStore;
  FBridge := TKeyValueHTTPBridge.Create(8868, FKV);
  FBridge.Start;
  Timer1.Enabled := True;
  TSeqLogger.Logger.Log(Information, 'HTTP server started on port 8868');
  Log('Server started on port 8868.');
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
    FKV := nil; // Interface will be released automatically
    TSeqLogger.Logger.Log(Information, 'HTTP server stopped');
    Log('Server stopped.');
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BtnStopClick(nil);
  Timer1.Enabled := False;
  LogStrings.Free;
  TSeqLogger.Logger.Log(Information, 'KeyValue Server stopped');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TSeqLogger.Logger.Log(Information, 'KeyValue Server started');
  LogStrings := TStringList.Create;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  //UpdateKVDisplay;
end;

procedure TMainForm.BtnWriteKVClick(Sender: TObject);
var
  Key, ValueStr, ConversionError: string;
  Value: Variant;
begin
  if not Assigned(FKV) then
    Exit;
  Key := System.SysUtils.Trim(EditKey.Text);
  ValueStr := EditValue.Text;
  if Key = '' then
  begin
    ShowMessage('Key must not be empty');
    Exit;
  end;

  Value := StringToTypedVariant(ValueStr, ConversionError);
  if ConversionError <> '' then
    Log('[Warning] ' + ConversionError);

  FKV.SetValue(Key, Value);
  TSeqLogger.Logger.Log(Information, Format('Manual key/value write: %s=%s', [Key, VariantToStringWithType(Value)]));
  Log(Format('Manually wrote: %s=%s', [Key, VariantToStringWithType(Value)]));
  UpdateKVDisplay;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  v: variant;
  arr: TArray<TPair<string, variant>>;
begin
  FKV.SetValue('a', 123);
  FKV.GetValue('a', v);
  v := _Obj(['pi', 3.14]);
  FKV.SetValue('b', v);
  FKV.GetValue('b', v);
  FKV.GetAll(arr);
  v := v;
end;

end.


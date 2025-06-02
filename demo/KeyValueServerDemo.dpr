program KeyValueServerDemo;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  HttpServer in '..\src\server\HttpServer.pas',
  KeyValueHttpBridge in '..\src\server\KeyValueHttpBridge.pas',
  KeyValueStore in '..\src\server\KeyValueStore.pas',
  SeqLogger in '..\src\common\SeqLogger.pas',
  VariantUtils in '..\src\common\VariantUtils.pas',
  ChangeRecordPool in '..\src\common\ChangeRecordPool.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

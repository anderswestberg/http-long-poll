program KeyValueClientDemo;

uses
  Vcl.Forms,
  MainClientUnit in 'MainClientUnit.pas' {MainForm},
  SeqLogger in '..\..\src\common\SeqLogger.pas',
  KeyValueClient in '..\..\src\client\KeyValueClient.pas',
  KeyValueHttpClient in '..\..\src\client\KeyValueHttpClient.pas',
  VariantUtils in '..\..\src\common\VariantUtils.pas',
  ChangeRecordPool in '..\..\src\common\ChangeRecordPool.pas',
  LoggerSettings in '..\..\src\common\LoggerSettings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end. 
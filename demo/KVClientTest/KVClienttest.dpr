program KVClientTest;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  SeqLoggerClass in '..\..\src\SeqLoggerClass.pas',
  KeyValueClientUnit in '..\..\src\KeyValueClientUnit.pas',
  KVHttpClient in '..\..\src\KVHttpClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

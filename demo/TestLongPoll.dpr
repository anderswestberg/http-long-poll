program TestLongPoll;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  HttpServerUnit in '..\src\HttpServerUnit.pas',
  KeyValueHTTPBridgeUnit in '..\src\KeyValueHTTPBridgeUnit.pas',
  KeyValueServerUnit in '..\src\KeyValueServerUnit.pas',
  SeqLoggerClass in '..\src\SeqLoggerClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

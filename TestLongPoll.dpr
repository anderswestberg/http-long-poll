program TestLongPoll;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form3},
  HttpServerUnit in 'HttpServerUnit.pas',
  KeyValueHTTPBridgeUnit in 'KeyValueHTTPBridgeUnit.pas',
  KeyValueServerUnit in 'KeyValueServerUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

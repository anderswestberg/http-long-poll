program KVClientTest;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  KVLongPollThread in 'KVLongPollThread.pas',
  KVHttpClient in 'KVHttpClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

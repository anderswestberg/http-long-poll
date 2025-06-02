program KeyValueServerDemo;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  HttpServer in '..\src\server\HttpServer.pas',
  KeyValueHttpBridge in '..\src\server\KeyValueHttpBridge.pas',
  KeyValueStore in '..\src\server\KeyValueStore.pas',
  SeqLogger in '..\src\common\SeqLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

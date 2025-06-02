unit LoggerSettings;

interface

uses
  System.IniFiles, System.SysUtils, SeqLogger;

type
  TLoggerSettings = class
  private
    class var FIniFile: TIniFile;
    class function GetIniFilePath: string;
  public
    class procedure Initialize;
    class procedure Finalize;
    
    class procedure SaveSettings(Enabled: Boolean; LogLevel: TLogLevel);
    class procedure LoadSettings(var Enabled: Boolean; var LogLevel: TLogLevel);
  end;

implementation

class procedure TLoggerSettings.Initialize;
begin
  FIniFile := TIniFile.Create(GetIniFilePath);
end;

class procedure TLoggerSettings.Finalize;
begin
  FIniFile.Free;
end;

class function TLoggerSettings.GetIniFilePath: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

class procedure TLoggerSettings.SaveSettings(Enabled: Boolean; LogLevel: TLogLevel);
begin
  if not Assigned(FIniFile) then Exit;
  
  FIniFile.WriteBool('Logging', 'Enabled', Enabled);
  FIniFile.WriteString('Logging', 'LogLevel', TSeqLogger.LogLevelToString(LogLevel));
  FIniFile.UpdateFile;
end;

class procedure TLoggerSettings.LoadSettings(var Enabled: Boolean; var LogLevel: TLogLevel);
begin
  if not Assigned(FIniFile) then Exit;
  
  Enabled := FIniFile.ReadBool('Logging', 'Enabled', True);
  LogLevel := TSeqLogger.StringToLogLevel(
    FIniFile.ReadString('Logging', 'LogLevel', 'Information')
  );
end;

initialization
  TLoggerSettings.Initialize;

finalization
  TLoggerSettings.Finalize;

end. 
unit AutoUpdater;

interface

uses
  Process,
  SysUtils;


procedure StartAutoUpdater;

var
  UpdaterProcess: TProcess;


implementation

procedure StartAutoUpdater;
var
  i: Integer;
begin
  UpdaterProcess := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    UpdaterProcess.Executable := ExtractFilePath(ParamStr(0)) + 'solupd.exe';
    {$ELSE}
    UpdaterProcess.Executable := ExtractFilePath(ParamStr(0)) + 'solupd';
    {$ENDIF}
    UpdaterProcess.CurrentDirectory := ExtractFilePath(ParamStr(0));
    UpdaterProcess.Parameters.Add('--waitpid');
    UpdaterProcess.Parameters.Add(IntToStr(System.GetProcessID));
    for i := 1 to GetEnvironmentVariableCount do
      UpdaterProcess.Environment.Add(GetEnvironmentString(i));
    UpdaterProcess.Options := UpdaterProcess.Options + [poUsePipes];

    UpdaterProcess.Execute;
  except
    on E: Exception do
    begin
    end;
  end;
end;

end.

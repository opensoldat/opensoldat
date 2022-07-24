{*******************************************************}
{                                                       }
{       LogFile Unit for OPENSOLDAT                     }
{                                                       }
{       Copyright (c) 2002 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit LogFile;

interface

uses
  SyncObjs, Classes;

procedure NewLogFile(var F: TStringList; Name: string);
procedure WriteLogFile(var F: TStringList; Name: string);
procedure AddLineToLogFile(var F: TStringList; S: string; Name: string; WithDate: Boolean = True);
procedure NewLogFiles;

var
  GameLog: TStringList;
  ConsoleLogFileName: String;
  {$IFDEF SERVER}
  KillLog: TStringList;
  KillLogFileName: String;
  {$ENDIF}
  LogLock: TCriticalSection;

implementation

uses
  {$IFDEF SERVER}
  Server,
  {$ELSE}
  Util, Client,
  {$ENDIF}
  SysUtils, Constants, TraceLog, Cvar;

procedure NewLogFile(var F: TStringList; Name: string);
var
  i: Integer;
  LogFile: TextFile;
begin
  if not log_enable.Value then
    Exit;

  LogLock.Acquire;
  try
    if Assigned(F) then
      F.Clear
    else
      F := TStringList.Create;
  finally
    LogLock.Release;
  end;

  try
    {$I-}
    AssignFile(LogFile, Name);
    FileMode := fmOpenReadWrite;
    {$I+}

    i := IOResult;
    if i <> 0 then
    begin
      {$IFDEF SERVER}
      WriteLn('File logging error (N): ' + IntToStr(i));
      {$ELSE}
      MainConsole.Console('File logging error (N): ' + IntToStr(i),
        DEBUG_MESSAGE_COLOR);
      {$ENDIF}
      Exit;
    end;

    {$I-}
    Rewrite(LogFile);
    {$I+}

    i := IOResult;
    if i <> 0 then
    begin
      {$IFDEF SERVER}
      WriteLn('File logging error (N2): ' + IntToStr(i));
      {$ELSE}
      MainConsole.Console('File logging error (N2): ' + IntToStr(i),
        DEBUG_MESSAGE_COLOR);
      {$ENDIF}
    end;
    CloseFile(LogFile);
  except
  end;
end;

procedure AddLineToLogFile(var F: TStringList; S: string; Name: string;
  WithDate: Boolean = True);
var
  S2: string;
begin
  Trace(S);

  if not Assigned(F) then
    Exit;

  if not log_enable.Value then
    Exit;

  if Length(S) < 1 then
    Exit;

  if log_level.Value = LEVEL_OFF then
    Exit;

  S2 := FormatDateTime('yy/mm/dd', Date);
  S2 := S2 + ' ' + FormatDateTime('hh:nn:ss', Time);

  LogLock.Acquire;
  try
    if WithDate then
      F.Add(S2 + ' ' + S)
    else
      F.Add(S);
  finally
    LogLock.Release;
  end;

  if log_level.Value >= LEVEL_TRACE then
    WriteLogFile(F, Name);
end;

procedure WriteLogFile(var F: TStringList; Name: string);
var
  i, io: Integer;
  LogFile: TextFile;
begin
  if not log_enable.Value then
    Exit;

  if not Assigned(F) then
    Exit;

  try
    {$I-}
    AssignFile(LogFile, Name);
    FileMode := fmOpenReadWrite;
    {$I+}

    io := IOResult;
    if io <> 0 then
    begin
      {$IFDEF SERVER}
      Writeln('File logging error (W1): ' + IntToStr(io));
      {$ELSE}
      MainConsole.Console('File logging error (W1): ' + IntToStr(io),
        DEBUG_MESSAGE_COLOR);
      {$ENDIF}
      Exit;
    end;

    {$I-}
    Append(LogFile);
    {$I+}

    io := IOResult;
    if io <> 0 then
    begin
      {$IFDEF SERVER}
      Writeln('File logging error (W2 - wrong directory names?): ' +
        IntToStr(io));
      {$ELSE}
      MainConsole.Console('File logging error (W2 - wrong directory names?): ' +
        IntToStr(io), DEBUG_MESSAGE_COLOR);
      {$ENDIF}
    end;

  except
    Exit;
  end;

  if (F.Count > 1000000) then
    Exit;

  {$I-}
  LogLock.Acquire;
  try
    for i := 0 to F.Count - 1 do
    begin
      Writeln(LogFile, F.Strings[i]);
    end;
  finally
    LogLock.Release;
  end;
  {$I+}

  io := IOResult;
  if io <> 0 then
  begin
    {$IFDEF SERVER}
    WriteLn('File logging error (W3): ' + IntToStr(io));
    {$ELSE}
    MainConsole.Console('File logging error (W3): ' + IntToStr(io),
      DEBUG_MESSAGE_COLOR);
    {$ENDIF}
  end;

  CloseFile(LogFile);
  LogLock.Acquire;
  try
    F.Clear;
  finally
    LogLock.Release;
  end;
end;

procedure NewLogFiles;
var
  j: Integer;
  S2: string;
begin
  {$IFNDEF SERVER}
  // avoid I/O 103 erros because of missing folder
  if not CreateDirIfMissing(UserDirectory + 'logs') then
  begin
    ShowMessage('Could not create Logs folder - disabled logging');
    log_enable.SetValue(False);
    Exit;
  end;
  {$ENDIF}
  S2 := FormatDateTime('yy-mm-dd', Now);

  ConsoleLogFileName := Format('%slogs/consolelog-%s-01.txt', [UserDirectory, S2]);
  j := 1;
  while FileExists(ConsoleLogFileName) do
  begin
    Inc(j);
    ConsoleLogFileName := Format('%slogs/consolelog-%s-%.2d.txt', [UserDirectory, S2, j]);
  end;
  if log_level.Value = LEVEL_OFF then
    ConsoleLogFileName := UserDirectory + 'logs/consolelog.txt';

  NewLogFile(GameLog, ConsoleLogFileName);
  AddLineToLogFile(GameLog, '   Console Log Started', ConsoleLogFileName);

  {$IFDEF SERVER}
  KillLogFileName := Format('%slogs/kills/killlog-%s-01.txt', [UserDirectory, S2]);
  j := 1;
  while FileExists(KillLogFileName) do
  begin
    Inc(j);
    KillLogFileName := Format('%slogs/kills/killlog-%s-%.2d.txt', [UserDirectory, S2, j]);
  end;
  NewLogFile(KillLog, KillLogFileName);
  AddLineToLogFile(KillLog, '   Kill Log Started', KillLogFileName);
  {$ENDIF}
end;

initialization
  LogLock := TCriticalSection.Create;

finalization
  FreeAndNil(LogLock);

end.

{*******************************************************}
{                                                       }
{       Main Unit for OPENSOLDAT                        }
{                                                       }
{       Copyright (c) 2012 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Main;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  Baseunix,
  {$ENDIF}

  {$IFDEF SCRIPT}ScriptDispatcher,{$ENDIF}

  Server, Constants, SysUtils, Classes, ServerHelper, ServerLoop;

procedure RunServer;

implementation

var
  CtrlCHit: Boolean = False;

{$IFDEF MSWINDOWS}
// The windows server needs a hook to make OpenSoldatServer exit normally
function ConsoleHandlerRoutine(CtrlType: DWORD): BOOL; stdcall;
begin
  Result := False;
  if CtrlType = CTRL_C_EVENT then
  begin
    Result := True;
    if not CtrlCHit then
    begin
      ProgReady := False;
      CtrlCHit := True;
      WriteLn('Control-C hit, shutting down');
    end
    else
    begin
      WriteLn('OK, OK, exiting immediately');
      Halt(1);
    end;
  end;
end;

procedure SetSigHooks;
begin
  SetConsoleCtrlHandler(@ConsoleHandlerRoutine, True);
end;

procedure ClearSigHooks;
begin
  // for some reason, under windows if we caught CTRL+C, it kept looping
  // My guess is that there are some threads that aren't cleaned up and
  // windows is waiting for them, so for now we just quit again
  SetConsoleCtrlHandler(@ConsoleHandlerRoutine, False);
  GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
end;
{$ELSE}
// The linux server can be killed with
// 'kill -TERM(15) <pid>' or 'kill -QUIT(3) <pid>' and
// it will clean itself up, instead of forcing you to use 'KILL -KILL(9) <pid>'
var
  FSIGTERM, FSIGTERMOLD: SigActionRec; // Place holders for the SIG Actions
  FSIGQUIT, FSIGQUITOLD: SigActionRec;
  FSIGINT,  FSIGINTOLD:  SigActionRec;
  FSIGPIPE, FSIGPIPEOLD: SigActionRec;

procedure HandleSig(Signal: Longint); cdecl;
begin
  if Signal = SIGINT then
  begin
    WriteLn('');
    if not CtrlCHit then
    begin
      WriteLn('Control-C hit, shutting down');
      CtrlCHit := True;
      ProgReady := False;
    end
    else
    begin
      WriteLn('OK, OK, exiting immediately');
      Halt(1);
    end;
  end;

  if (Signal = SIGTERM) or (Signal = SIGQUIT) then
  begin
    MainConsole.Console('', GAME_MESSAGE_COLOR);
    MainConsole.Console('Signal received, shutting down', GAME_MESSAGE_COLOR);
    ProgReady := False;
  end;
end;

procedure SetSigHooks;
begin
  // Get the SIGTERM signal
  FillChar(FSIGTERM.Sa_Mask, SizeOf(FSIGTERM.sa_mask), #0);
  FillChar(FSIGTERMOLD.Sa_Mask, SizeOf(FSIGTERMOLD.sa_mask), #0);

  FSIGTERM.sa_Handler := SigActionHandler(@HandleSig);
  FSIGTERM.Sa_Flags := 0;

  {$IFDEF Linux}  // Linux specific
  FSIGTERM.Sa_Restorer := nil;
  {$ENDIF}

  if fpSigAction(SIGTERM, @FSIGTERM, @FSIGTERMOLD) <> 0 then
    raise Exception.Create('SIGAction failed');

  // Get the SIGINT signal
  FillChar(FSIGINT.Sa_Mask, SizeOf(FSIGINT.Sa_Mask), #0);
  FillChar(FSIGINTOLD.Sa_Mask, SizeOf(FSIGINTOLD.Sa_Mask), #0);

  FSIGINT.sa_Handler := SigActionHandler(@HandleSig);
  FSIGINT.Sa_Flags := 0;

  {$IFDEF Linux}  // Linux specific
  FSIGINT.Sa_Restorer := nil;
  {$ENDIF}

  if fpSigAction(SIGINT, @FSIGINT, @FSIGINTOLD) <> 0 then
    raise Exception.Create('SIGAction failed');

  // Get the SIGQUIT signal
  FillChar(FSIGQUIT.Sa_Mask, SizeOf(FSIGQUIT.sa_mask), #0);
  FillChar(FSIGQUITOLD.Sa_Mask, SizeOf(FSIGQUITOLD.sa_mask), #0);

  FSIGQUIT.sa_Handler := SigActionHandler(@HandleSig);
  FSIGQUIT.Sa_Flags := 0;

  {$IFDEF Linux}  // Linux specific
  FSIGQUIT.Sa_Restorer := nil;
  {$ENDIF}

  if fpSigAction(SIGQUIT, @FSIGQUIT, @FSIGQUITOLD) <> 0 then
    raise Exception.Create('SIGAction failed');

  // Get the SIGPIPE signal
  FillChar(FSIGPIPE.Sa_Mask, SizeOf(FSIGPIPE.sa_mask), #0);
  FillChar(FSIGPIPEOLD.Sa_Mask, SizeOf(FSIGPIPEOLD.sa_mask), #0);

  FSIGPIPE.sa_Handler := SigActionHandler(@HandleSig);
  FSIGPIPE.Sa_Flags := 0;

  {$IFDEF Linux}  // Linux specific
  FSIGPIPEOLD.Sa_Restorer := nil;
  {$ENDIF}

  if fpSigAction(SIGPIPE, @FSIGPIPE, @FSIGPIPEOLD) <> 0 then
    raise Exception.Create('SIGAction failed');
end;

procedure ClearSigHooks;
begin
  if fpSigAction(SIGTERM, @FSIGTERMOLD, nil) <> 0 then
    raise Exception.Create('SIGAction failed');

  if fpSigAction(SIGINT, @FSIGINTOLD, nil) <> 0 then
    raise Exception.Create('SIGAction failed');

  if fpSigAction(SIGQUIT, @FSIGQUITOLD, nil) <> 0 then
    raise Exception.Create('SIGAction failed');

  if fpSigAction(SIGPIPE, @FSIGPIPEOLD, nil) <> 0 then
    raise Exception.Create('SIGAction failed');
end;
{$ENDIF}

function IsRoot: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := False;  // Ignore for Windows users
  {$ELSE}
  Result := (FpGeteuid() = 0);
  {$ENDIF}
end;

procedure RunServer;
begin
  if IsRoot then
  begin
    WriteLn('You are running OpenSoldatServer as root! Don''t do that! ' +
      'There are not many valid' + #10 +
      'reasons for this and it can, in theory, cause great damage!');
    Exit;

    // TODO: Add -r and --allow-root commandline option
    {if not AllowRoot then
      Exit;

    WriteLn('You have been warned.' + #10 +
      'Hit CTRL+C now if you don''t want to run OpenSoldatServer as root.' + #10 +
      'OpenSoldatServer will start in 30 seconds.');
    Sleep(30000);}
  end;

  SetSigHooks;

  try
    {$IFNDEF DEBUG}
    try
    {$ENDIF}
      ActivateServer;
      WritePID;

      {$IFDEF SCRIPT}
      if sc_enable.Value then
        ScrptDispatcher.Prepare;
      {$ENDIF}

      WriteLn(
        '----------------------------------------------------------------');

      if ProgReady then
        StartServer;
      while ProgReady do
      begin
        AppOnIdle;
        Sleep(1);
      end;
    {$IFNDEF DEBUG}
    except
      on E: Exception do
      begin
        ProgReady := False;
        MainConsole.Console('Server Encountered an error:', GAME_MESSAGE_COLOR);
        MainConsole.Console(E.Message, GAME_MESSAGE_COLOR);
      end;
    end;
    {$ENDIF NOT DEBUG}
  finally
    // Any needed cleanup code here
    ShutDown;
    ClearSigHooks;
  end;
end;

end.

unit ClientCommands;
{$push}
{$warn 5024 off}

interface

procedure InitClientCommands();

implementation

uses
  {$IFDEF STEAM}Steam,{$ENDIF}
  Client, Command, Util, strutils, Game, ClientGame, Sound,
  GameRendering, NetworkClientMessages, Demo, GameStrings, Net,
  Sprites, Constants, classes, sysutils, Input;

var
  ScreenShotsInARow: Byte = 0;

procedure CommandBind(Args: array of AnsiString; Sender: Byte);
var
  BindKeyName: AnsiString;
  CommandString: AnsiString;
  Modifier: Word;
begin
  if Length(Args) < 3 then
  begin
    MainConsole.Console('Usage: bind "key" "command"', GAME_MESSAGE_COLOR);
    Exit;
  end;

  BindKeyName := LowerCase(Args[1]);
  CommandString := Args[2];
  Modifier := KM_NONE;

  if AnsiContainsStr(BindKeyName, '+') then
  begin
    if AnsiContainsText(BindKeyName, 'ctrl') then
      Modifier := Modifier or KM_CTRL;
    if AnsiContainsText(BindKeyName, 'shift') then
      Modifier := Modifier or KM_SHIFT;
    if AnsiContainsText(BindKeyName, 'alt') then
      Modifier := Modifier or KM_ALT;
    BindKeyName := StringsReplace(BindKeyName, ['ctrl', 'shift', 'alt', '+'], ['', '', '', ''], [rfReplaceAll]);
  end;

  if Args[2][1] = '+' then
    BindKey(BindKeyName, CommandString, CommandString, Modifier)
  else
    BindKey(BindKeyName, '+bind', CommandString, Modifier);
end;

procedure CommandConnect(Args: array of AnsiString; Sender: Byte);
var
  S: String;
begin
  if Length(Args) <= 1 then
  begin
    MainConsole.Console('Usage: connect ip port password', GAME_MESSAGE_COLOR);
    Exit;
  end;
  ExitToMenu;
  if Args[0] = 'joinurl' then
  begin
    S := Args[1];
    JoinIP := GetPiece(S, '//', 2);
    JoinIP := GetPiece(JoinIP, ':', 1);

    JoinPort := GetPiece(S, ':', 3);
    JoinPort := GetPiece(JoinPort, '/', 1);
    JoinPort := AnsiReplaceStr(JoinPort, '/', '');

    JoinPassword := GetPiece(S, '/', 4);
  end else
  begin
    JoinIP := Args[1];
    if Length(Args) = 2 then
      JoinPort := '23073'
    else
      JoinPort := Args[2];
    if Length(Args) > 3 then
      JoinPassword := Args[3];
  end;
  JoinServer();
end;

procedure CommandRetry(Args: array of AnsiString; Sender: Byte);
begin
  ExitToMenu;
  JoinServer();
end;

procedure CommandDisconnect(Args: array of AnsiString; Sender: Byte);
begin
  ExitToMenu;
end;

procedure CommandSay(Args: array of AnsiString; Sender: Byte);
begin
  if Length(Args) <= 1 then
  begin
    MainConsole.Console('Usage: say "text"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  ClientSendStringMessage(WideString(Args[1]), MSGTYPE_PUB);
end;

procedure CommandSayTeam(Args: array of AnsiString; Sender: Byte);
begin
  if Length(Args) <= 1 then
  begin
    MainConsole.Console('Usage: say_team "text"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  ClientSendStringMessage(WideString(Args[1]), MSGTYPE_TEAM);
end;

procedure CommandRecord(Args: array of AnsiString; Sender: Byte);
var
  Str: String;
begin
  if (Length(Args) = 2) then
    Str := Args[1]
  else
    Str := FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now()) + Map.Name;

  DemoRecorder.StopRecord;
  DemoRecorder.StartRecord(UserDirectory + 'demos/' + Str + '.sdm');
end;

procedure CommandMute(Args: array of AnsiString; Sender: Byte);
var
  Str: String;
  i: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Str := Args[1];

  if Str = 'all' then
  begin
    MuteAll := not MuteAll;

    if MuteAll then
      MainConsole.Console(WideString('Everyone is muted'), CLIENT_MESSAGE_COLOR)
    else
      MainConsole.Console(WideString('Everyone is unmuted'), CLIENT_MESSAGE_COLOR);

    Exit;
  end;

  Targets := CommandTarget(Str, Sender);
  for i := 0 to High(Targets) do
  begin
    Sprite[Targets[i]].Muted := True;
    MainConsole.Console(WideFormat(_('%s is muted'), [Sprite[Targets[i]].Player.Name]),
      CLIENT_MESSAGE_COLOR);
  end;
end;

procedure CommandUnbindall(Args: array of AnsiString; Sender: Byte);
begin
  UnbindAll;
  MainConsole.Console('Unbinded all binds', GAME_MESSAGE_COLOR);
end;

procedure CommandUnmute(Args: array of AnsiString; Sender: Byte);
var
  Str: String;
  i: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Str := Args[1];

  Targets := CommandTarget(Str, Sender);
  for i := 0 to High(Targets) do
  begin
    Sprite[Targets[i]].Muted := False;
    MainConsole.Console(WideFormat(_('%s is unmuted'), [Sprite[Targets[i]].Player.Name]),
      CLIENT_MESSAGE_COLOR);
  end;
end;

procedure CommandStop(Args: array of AnsiString; Sender: Byte);
begin
  DemoRecorder.StopRecord;
end;

procedure CommandShutdown(Args: array of AnsiString; Sender: Byte);
begin
  ExitToMenu;
end;

procedure CommandScreenshot(Args: array of AnsiString; Sender: Byte);
var
  ScreenFile: String;
begin
  if (ScreenShotsInARow < 3) then
  begin
    Inc(ScreenShotsInARow);
    {$IFDEF STEAM}
    if cl_steam_screenshots.Value then
    begin
      SteamAPI.Screenshots.TriggerScreenshot();
      Exit;
    end;
    {$ENDIF}
    ScreenFile := UserDirectory + 'screens/' +
    FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now()) +
    Map.Name + '_screenshot.png';

    MainConsole.Console(WideString('Screenshot saved to ' + ScreenFile),
    DEBUG_MESSAGE_COLOR);

    TakeScreenShot(ScreenFile);

    PlaySound(SFX_SNAPSHOT);

    if ShowScreen then
    begin
      ShowScreen := False;
    end;
  end;
end;

procedure CommandSwitchCam(Args: array of AnsiString; Sender: Byte);
var
  InputId: LongInt;
begin
  if Length(Args) <= 1 then
  begin
    MainConsole.Console('Usage: switchcam "id"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  if not Sprite[MySprite].IsSpectator then
  begin
    MainConsole.Console('You are not a spectator', DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  InputId := StrToIntDef(Args[1], -1);
  if (InputId < 0) or (InputId > MAX_SPRITES) then
  begin
    MainConsole.Console('Invalid id value. Must be in range [0, ' + IntToStr(MAX_SPRITES) + ']',
    DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  CameraFollowSprite := InputId;
end;

procedure CommandSwitchCamFlag(Args: array of AnsiString; Sender: Byte);
var
  i: Integer;
begin
  if Length(Args) <= 1 then
  begin
    MainConsole.Console('Usage: switchcamflag "id"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  if Sprite[MySprite].IsSpectator then
  begin
    for i := 1 to MAX_THINGS do
    begin
        if (Thing[i].Style = StrToIntDef(Args[1], 0)) then
        begin
          CameraFollowSprite := 0;
          CameraX := Thing[i].Skeleton.Pos[1].X;
          CameraY := Thing[i].Skeleton.Pos[1].Y;
        end;
    end;
  end;
end;

procedure CommandDemoTick(Args: array of AnsiString; Sender: Byte);
begin
  if Length(Args) <= 1 then
  begin
    MainConsole.Console('Usage: ' + Args[0] + ' "tick"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  if not DemoPlayer.Active then
  begin
    MainConsole.Console('You are not playing a demo', DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  if Args[0] = 'demo_tick' then
    DemoPlayer.Position(StrToIntDef(Args[1], 0))
  else
    DemoPlayer.Position(MainTickCounter + StrToIntDef(Args[1], 0));
end;

procedure InitClientCommands();
begin
  CommandAdd('bind', CommandBind, 'Binds command to key', []);
  CommandAdd('connect', CommandConnect, 'connect to server', [CMD_DEFERRED]);
  CommandAdd('join', CommandConnect, 'connect to server', [CMD_DEFERRED]);
  CommandAdd('joinurl', CommandConnect, 'connect to server using url', [CMD_DEFERRED]);
  CommandAdd('disconnect', CommandDisconnect, 'disconnect from server', [CMD_DEFERRED]);
  CommandAdd('retry', CommandRetry, 'retry connect to last server', [CMD_DEFERRED]);
  CommandAdd('screenshot', CommandScreenshot, 'take a screenshot of game', [CMD_DEFERRED]);
  CommandAdd('say', CommandSay, 'send chat message', []);
  CommandAdd('say_team', CommandSayTeam, 'send team chat message', []);
  CommandAdd('record', CommandRecord, 'record demo', []);
  CommandAdd('mute', CommandMute, 'mute specific nick or id', []);
  CommandAdd('unbindall', CommandUnbindall, 'Unbinds all binds', []);
  CommandAdd('unmute', CommandUnmute, 'unmute specific nick or id', []);
  CommandAdd('stop', CommandStop, 'stop recording demo', []);
  CommandAdd('shutdown', CommandShutdown, 'shutdown game', []);
  CommandAdd('switchcam', CommandSwitchCam, 'switches camera to specific player', []);
  CommandAdd('switchcamflag', CommandSwitchCamFlag, 'switches camera to specific flag', []);
  CommandAdd('demo_tick', CommandDemoTick, 'skips to a tick in demo', []);
  CommandAdd('demo_tick_r', CommandDemoTick, 'skips to a tick (relatively) in demo', []);
end;
{$pop}
end.

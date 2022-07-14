unit ServerCommands;

interface

procedure InitServerCommands();

implementation

uses
  // system and delphi units
  SysUtils, Classes, dateutils,

  // helper units
  Vector, Util, Version,

  // scripting units
  {$IFDEF SCRIPT}
  ScriptDispatcher,
  {$ENDIF}

  Server, Command, Weapons, Net, NetworkUtils, NetworkServerMessages,
  NetworkServerFunctions, NetworkServerConnection, NetworkServerGame,
  Sprites, PolyMap, Game, Things, Constants, ServerHelper, BanSystem, Demo, Steam;

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
procedure CommandAddbot(Args: array of AnsiString; Sender: Byte);
var
  Name, TempStr: String;
  TeamSet: Integer;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  if PlayersNum = MAX_PLAYERS then
    Exit;

  TempStr := Args[0];
  if Length(TempStr) < 7 then
    TeamSet := 0
  else
    TeamSet := StrToIntDef(TempStr[7], 0);
  AddBotPlayer(Name, TeamSet);
end;

procedure CommandNextmap(Args: array of AnsiString; Sender: Byte);
begin
  NextMap;
end;

procedure CommandMap(Args: array of AnsiString; Sender: Byte);
var
  Status: TMapInfo;
begin
  if Length(Args) = 1 then
    Exit;


  if Length(Args[1]) < 1 then
    Exit;

  if GetMapInfo(Args[1], UserDirectory, Status) then
  begin
    PrepareMapChange(Args[1]);
  end else
  begin
    {$IFDEF STEAM}
    if Status.WorkshopID > 0 then
    begin
      MapChangeItemID := Status.WorkshopID;
      if SteamAPI.UGC.DownloadItem(MapChangeItemID, True) then
        MainConsole.Console('[Steam] Workshop map ' + IntToStr(MapChangeItemID) + ' not found in cache, downloading.', WARNING_MESSAGE_COLOR, Sender)
      else
        MainConsole.Console('[Steam] Workshop map ' + IntToStr(MapChangeItemID) + ' is invalid', WARNING_MESSAGE_COLOR, Sender)
    end else
    {$ENDIF}
      MainConsole.Console('Map not found (' + Args[1] + ')', WARNING_MESSAGE_COLOR, Sender);
  end;

  //if not MapExists(MapChangeName, UserDirectory) then
  //begin
  //  MainConsole.Console('Map not found (' + MapChangeName + ')', WARNING_MESSAGE_COLOR, Sender);
  //  Exit;
  //end;

end;

procedure CommandPause(Args: array of AnsiString; Sender: Byte);
begin
  MapChangeCounter := 999999999;
  ServerSyncMsg;
end;

procedure CommandUnpause(Args: array of AnsiString; Sender: Byte);
begin
  MapChangeCounter := -60;
  ServerSyncMsg;
end;

procedure CommandRestart(Args: array of AnsiString; Sender: Byte);
begin
  PrepareMapChange(Map.Name);
end;

procedure CommandKick(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  Targets := CommandTarget(Name, Sender);
  for i := 0 to High(Targets) do
    KickPlayer(Targets[i], False, KICK_CONSOLE, 0);
end;

procedure CommandKicklast(Args: array of AnsiString; Sender: Byte);
begin
  if (LastPlayer > 0) and (LastPlayer < MAX_SPRITES + 1) then
    if Sprite[LastPlayer].Active then
      KickPlayer(LastPlayer, False, KICK_CONSOLE, 0);
end;

procedure CommandBan(Args: array of AnsiString; Sender: Byte);
var
  Name, TempStr: String;
  i: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  if Sender = 255 then
    TempStr := 'an admin'
  else
    TempStr := Sprite[Sender].Player.Name;

  Targets := CommandTarget(Name, Sender);
  for i := 0 to High(Targets) do
    KickPlayer(Targets[i], True, KICK_CONSOLE, (DAY * 30), 'Banned by ' + TempStr);
end;

procedure CommandBaniphw(Args: array of AnsiString; Sender: Byte);
var
  Name, TempStr: String;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  if Sender = 255 then
    TempStr := 'an admin'
  else
    TempStr := Sprite[Sender].Player.Name;

  if Args[0] = 'banhw' then
  begin
    AddBannedHW(Name, 'Banned by ' + TempStr, (DAY * 30));
    MainConsole.Console('HWID ' + Name + ' banned', CLIENT_MESSAGE_COLOR, Sender);
  end else
  begin
    AddBannedIP(Name, 'Banned by ' + TempStr, DAY * 30);
    MainConsole.Console('IP number ' + Name + ' banned', CLIENT_MESSAGE_COLOR, Sender);
  end;

  SaveTxtLists;
end;

procedure CommandUnban(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  if DelBannedIP(Name) then
    MainConsole.Console('IP number ' + Name + ' unbanned', CLIENT_MESSAGE_COLOR, Sender);

  if DelBannedHW(Name) then
    MainConsole.Console('HWID ' + Name + ' unbanned', CLIENT_MESSAGE_COLOR, Sender);

  SaveTxtLists;
end;

procedure CommandUnbanlast(Args: array of AnsiString; Sender: Byte);
begin
  if DelBannedIP(LastBan) then
    MainConsole.Console('IP number ' + LastBan + ' unbanned', CLIENT_MESSAGE_COLOR, Sender);

  if DelBannedHW(LastBanHW) then
    MainConsole.Console('HWID ' + LastBanHW + ' unbanned', CLIENT_MESSAGE_COLOR, Sender);

  SaveTxtLists;
end;

procedure CommandAdm(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  Targets := CommandTarget(Name, Sender);
  for i := 0 to High(Targets) do
    if not IsRemoteAdminIP(Sprite[Targets[i]].Player.IP) then
    begin
      RemoteIPs.Add(Sprite[Targets[i]].Player.IP);
      MainConsole.Console('IP number ' + Sprite[Targets[i]].Player.IP +
        ' added to Remote Admins', CLIENT_MESSAGE_COLOR, Sender);
      SaveTxtLists;
    end;
end;

procedure CommandAdmip(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  if not IsRemoteAdminIP(Name) then
  begin
    RemoteIPs.Add(Name);
    MainConsole.Console('IP number ' + Name + ' added to Remote Admins',
      CLIENT_MESSAGE_COLOR, Sender);
    SaveTxtLists;
  end;
end;

procedure CommandUnadm(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  j: Integer;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  if IsRemoteAdminIP(Name) then
  begin
    j := RemoteIPs.IndexOf(Name);
    RemoteIPs.Delete(j);
    MainConsole.Console('IP number ' + Name +
      ' removed from Remote Admins', CLIENT_MESSAGE_COLOR, Sender);
    SaveTxtLists;
  end;
end;

procedure CommandSetteam(Args: array of AnsiString; Sender: Byte);
var
  Name, TempStr: String;
  i: Integer;
  TeamSet: Byte;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  TempStr := Args[0];
  TeamSet := StrToIntDef(TempStr[8], 1);

  Targets := CommandTarget(Name, Sender);
  for i := 0 to High(Targets) do
  begin
    Sprite[Targets[i]].ChangeTeam(TeamSet, True);
  end;
end;

procedure CommandSay(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  ServerSendStringMessage(WideString(Name), ALL_PLAYERS, 255, MSGTYPE_PUB);
end;

procedure CommandKill(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  a: TVector2;
  i: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];
  a := Default(TVector2);

  Targets := CommandTarget(Name, Sender);
  for i := 0 to High(Targets) do
  begin
    Sprite[Targets[i]].Vest := 0;
    Sprite[Targets[i]].HealthHit(3430, Targets[i], 1, -1, a);
    MainConsole.Console(Sprite[Targets[i]].Player.Name + ' killed by admin', CLIENT_MESSAGE_COLOR, Sender);
  end;
end;

procedure CommandLoadwep(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i: Integer;
begin
  if Length(Args) = 1 then
  begin
    if sv_realisticmode.Value then
      Name := 'weapons_realistic'
    else
      Name := 'weapons';
  end else
    Name := Args[1];

  LastWepMod := Name;
  LoadWeapons(Name);

  for i := 1 to MAX_PLAYERS do
    if Sprite[i].Active then
      if Sprite[i].Player.ControlMethod = HUMAN then
        ServerVars(i);
end;

procedure CommandLoadcon(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i: Integer;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if sv_lockedmode.Value then
  begin
    MainConsole.Console('Locked Mode is enabled. Settings can''t be changed mid-game.'
      , SERVER_MESSAGE_COLOR, Sender);
    Exit;
  end;

  MapChangeCounter := -60;
  ServerDisconnect;
  for i := 1 to MAX_BULLETS do
    Bullet[i].Kill;
  for i := 1 to MAX_THINGS do
    Thing[i].Kill;
  for i := 1 to MAX_PLAYERS do
  begin
    if Sprite[i].Active then
    begin
      Sprite[i].Player.Team := FixTeam(Sprite[i].Player.Team);
      Sprite[i].Respawn;
      Sprite[i].Player.Kills := 0;
      Sprite[i].Player.Deaths := 0;
      Sprite[i].Player.Flags := 0;

      Sprite[i].Player.TKWarnings := 0;
      Sprite[i].Player.ChatWarnings := 0;
      Sprite[i].Player.KnifeWarnings := 0;

      Sprite[i].Player.ScoresPerSecond := 0;
      Sprite[i].Player.GrabsPerSecond := 0;
    end;
  end;

  LoadConfig(Name);
  MainConsole.Console('Config reloaded ' + Name, CLIENT_MESSAGE_COLOR, Sender);
  StartServer;
end;

procedure CommandLoadlist(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
begin
  if Length(Args) > 1 then
    Name := Args[1]
  else
    Name := sv_maplist.Value; // We only set it for better feedback in console.
  if LoadMapsList(Name) then
    MainConsole.Console('Loaded maps list: ' + Name, CLIENT_MESSAGE_COLOR, Sender)
  else
    MainConsole.Console('Could not find maps list: ' + Name, DEBUG_MESSAGE_COLOR, Sender);
end;

procedure CommandPm(Args: array of AnsiString; Sender: Byte);
var
  PMToID, PMMessage: String;
  i: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) <= 2 then
    Exit;

  PMToID := Args[1];

  Targets := CommandTarget(PMToID, Sender);
  for i := 0 to High(Targets) do
  begin
    PMMessage := Args[2];
    MainConsole.Console('Private Message sent to ' + IDToName(Targets[i]),
      SERVER_MESSAGE_COLOR, Sender);
    MainConsole.Console('(PM) To: ' + IDToName(Targets[i]) + ' From: ' +
      IDToName(Sender) + ' Message: ' + PMMessage, SERVER_MESSAGE_COLOR);
    ServerSendStringMessage('(PM) ' + WideString(PMMessage), Targets[i], 255, MSGTYPE_PUB);
  end;
end;

procedure CommandGmute(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i, j: Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  Targets := CommandTarget(Name, Sender);
  for i := 0 to High(Targets) do
  begin
    Sprite[Targets[i]].Player.Muted := 1;
    for j := 1 to MAX_PLAYERS do
      if Trim(MuteList[j]) = '' then
      begin
        MuteList[j] := Sprite[Targets[i]].Player.IP;
        mutename[j] := Sprite[Targets[i]].Player.Name;
        Break;
      end;
    MainConsole.Console(Sprite[Targets[i]].Player.Name + ' has been muted.',
      CLIENT_MESSAGE_COLOR, Sender);
  end;
end;

procedure CommandUngmute(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i,j : Integer;
  Targets: TCommandTargets;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  Targets := CommandTarget(Name, Sender);
  for i := 0 to High(Targets) do
  begin
    Sprite[Targets[i]].Player.Muted := 0;
    for j := 1 to MAX_PLAYERS do
      if Trim(MuteList[j]) = Sprite[Targets[i]].Player.IP then
      begin
        MuteList[j] := '';
        Break;
      end;
    MainConsole.Console(Sprite[Targets[i]].Player.Name + ' has been unmuted.',
      CLIENT_MESSAGE_COLOR, Sender);
  end;
end;

procedure CommandAddmap(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  //if not (MapExists(Name, UserDirectory)) then
  //begin
  //  MainConsole.Console('Map not found (' + Name + ')',
  //    SERVER_MESSAGE_COLOR, Sender);
  //  Exit;
  //end;

  MapsList.Add(Name);
  MainConsole.Console(Name + ' has been added to the map list.',
    SERVER_MESSAGE_COLOR, Sender);
  SaveMapList;
end;

procedure CommandDelmap(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  TempInt: Integer;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  for TempInt := 0 to (MapsList.Count - 1) do
    if UpperCase(MapsList[TempInt]) = UpperCase(Name) then
    begin
      MainConsole.Console(Name + ' has been removed from the map list.',
        SERVER_MESSAGE_COLOR, Sender);
      MapsList.Delete(TempInt);
      Break;
    end;
  SaveMapList;
end;

procedure CommandTempban(Args: array of AnsiString; Sender: Byte);
var
  Name, TempStr: String;
begin
  if Length(Args) <= 2 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  TempStr := Args[2];
  // *BAN*
  AddBannedIP(TempStr, 'Temporary Ban by an Admin', StrToIntDef(Name, 1) * MINUTE);
  MainConsole.Console('IP number ' + TempStr + ' banned for ' + Name + ' minutes.',
    CLIENT_MESSAGE_COLOR, Sender);
  SaveTxtLists;
end;

procedure CommandWeaponon(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i, j: Integer;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
    begin
      j := StrToIntDef(Name, -1);
      if (j > -1) and (j < 15) then
        SetWeaponActive(i, j, True);
    end;
end;

procedure CommandWeaponoff(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
  i, j: Integer;
begin
  if Length(Args) = 1 then
    Exit;

  Name := Args[1];

  if Length(Name) < 1 then
    Exit;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
    begin
      j := StrToIntDef(Name, -1);
      if (j > -1) and (j < 15) then
        SetWeaponActive(i, j, False);
    end;
end;

procedure CommandBanlist(Args: array of AnsiString; Sender: Byte);
var
  i: Integer;
  BanDuration: TDateTime;
  BanDurationText: String;
begin
  MainConsole.Console(Format('%-15s | %-9s | %s', ['HWID', 'Duration', 'Reason']), SERVER_MESSAGE_COLOR, Sender);
  for i := 1 to High(BannedHWList) do
  begin
    if BannedHWList[i].Time = PERMANENT then
      BanDurationText := 'PERMANENT'
    else
    begin
      BanDuration := IncSecond(Now, BannedHWList[i].Time div 60) - Now;
      BanDurationText := Format('%dd%s', [Trunc(BanDuration), FormatDateTime('h"h"n"m"', BanDuration)]);
    end;
    MainConsole.Console(Format('%-15s | %-9s | %s', [
        BannedHWList[i].HW,
        BanDurationText,
        BannedHWList[i].Reason]),
      SERVER_MESSAGE_COLOR, Sender);
  end;

  MainConsole.Console(Format('%-15s | %-9s | %s', ['IP', 'Duration', 'Reason']), SERVER_MESSAGE_COLOR, Sender);
  for i := 1 to High(BannedIPList) do
  begin
    if BannedIPList[i].Time = PERMANENT then
      BanDurationText := 'PERMANENT'
    else
    begin
      BanDuration := IncSecond(Now, BannedIPList[i].Time div 60) - Now;
      BanDurationText := Format('%dd%s', [Trunc(BanDuration), FormatDateTime('h"h"n"m"', BanDuration)]);
    end;
    MainConsole.Console(Format('%-15s | %-9s | %s', [
        BannedIPList[i].IP,
        BanDurationText,
        BannedIPList[i].Reason]),
      SERVER_MESSAGE_COLOR, Sender);
  end;
end;

procedure CommandNetStats(Args: array of AnsiString; Sender: Byte);
var
  StatsText: array[0..2048] of Char;
  DstPlayer: TPlayer;
begin
  if UDP.NetworkingSocket.GetDetailedConnectionStatus(1, StatsText, 2048) = 0 then
    WriteLn(StatsText);
  for DstPlayer in Players do
  begin
    if UDP.NetworkingSocket.GetDetailedConnectionStatus(DstPlayer.Peer, StatsText, 2048) = 0 then
    begin
        WriteLn('----------------');
        WriteLn(DstPlayer.Name);
        WriteLn(DstPlayer.Peer);
        WriteLn(StatsText);
        WriteLn('----------------');
    end;
  end;
end;

procedure CommandPlayerCommand(Args: array of AnsiString; Sender: Byte);
var
  Command: String;
  a: TVector2;
begin
  a := Default(TVector2);
  Command := Args[0];

  if Command = 'tabac' then
  begin
    Sprite[Sender].IdleRandom := 0;
    Sprite[Sender].IdleTime := 1;
  end
  else if Command = 'smoke' then
  begin
    Sprite[Sender].IdleRandom := 1;
    Sprite[Sender].IdleTime := 1;
  end
  else if Command = 'takeoff' then
  begin
    Sprite[Sender].IdleRandom := 4;
    Sprite[Sender].IdleTime := 1;
  end
  else if Command = 'victory' then
  begin
    Sprite[Sender].IdleRandom := 5;
    Sprite[Sender].IdleTime := 1;
  end
  else if Command = 'piss' then
  begin
    Sprite[Sender].IdleRandom := 6;
    Sprite[Sender].IdleTime := 1;
  end
  else if Command = 'mercy' then
  begin
    Sprite[Sender].IdleRandom := 7;
    Sprite[Sender].IdleTime := 1;
    if Sprite[Sender].Player.Kills > 0 then
      Dec(Sprite[Sender].Player.Kills);
  end
  else if Command = 'pwn' then
  begin
    Sprite[Sender].IdleRandom := 8;
    Sprite[Sender].IdleTime := 1;
  end
  else if Command = 'kill' then
  begin
    Sprite[Sender].Vest := 0;
    Sprite[Sender].HealthHit(150, Sender, 1, -1, a);
    if Sprite[Sender].Player.Kills > 0 then
      Dec(Sprite[Sender].Player.Kills);
  end
  else if Command = 'brutalkill' then
  begin
    Sprite[Sender].Vest := 0;
    Sprite[Sender].HealthHit(3423, Sender, 1, -1, a);
    if Sprite[Sender].Player.Kills > 0 then
      Dec(Sprite[Sender].Player.Kills);
  end;
end;

procedure CommandInfo(Args: array of AnsiString; Sender: Byte);
var
  gametype: WideString = '';
  {$IFDEF SCRIPT}
  i: ShortInt;
  ScriptList: TStringList;
  {$ENDIF}
begin
  if Length(sv_greeting.Value) > 0 then
    ServerSendStringMessage(WideString(sv_greeting.Value), Sender, 255, MSGTYPE_PUB);
  if Length(sv_greeting2.Value) > 0 then
    ServerSendStringMessage(WideString(sv_greeting2.Value), Sender, 255, MSGTYPE_PUB);
  if Length(sv_greeting3.Value) > 0 then
    ServerSendStringMessage(WideString(sv_greeting3.Value), Sender, 255, MSGTYPE_PUB);

  ServerSendStringMessage('Server: ' + WideString(sv_hostname.Value), Sender, 255, MSGTYPE_PUB);
  ServerSendStringMessage('Address: ' + WideString(ServerIP) + ':' +
    WideString(IntToStr(ServerPort)), Sender, 255, MSGTYPE_PUB);
  ServerSendStringMessage('Version: ' + DEDVERSION, Sender, 255, MSGTYPE_PUB);

  case sv_gamemode.Value of
    GAMESTYLE_DEATHMATCH: gametype := 'Deathmatch';
    GAMESTYLE_POINTMATCH: gametype := 'Pointmatch';
    GAMESTYLE_TEAMMATCH:  gametype := 'Teammatch';
    GAMESTYLE_CTF:        gametype := 'Capture the Flag';
    GAMESTYLE_RAMBO:      gametype := 'Rambomatch';
    GAMESTYLE_INF:        gametype := 'Infiltration';
    GAMESTYLE_HTF:        gametype := 'Hold the Flag';
  end;

  ServerSendStringMessage('Gamemode: ' + gametype, Sender, 255, MSGTYPE_PUB);
  ServerSendStringMessage('Timelimit: ' +
    WideString(IntToStr(sv_timelimit.Value div 3600)), Sender, 255, MSGTYPE_PUB);
  ServerSendStringMessage('Nextmap: ' + WideString(CheckNextMap), Sender, 255, MSGTYPE_PUB);

  {$IFDEF SCRIPT}
  ServerSendStringMessage('Scripting: ' +
    iif(sc_enable.Value, 'Enabled', 'Disabled'), Sender, 255, MSGTYPE_PUB);
  if sc_enable.Value then
  begin
    gametype := '';
    ScriptList := ScrptDispatcher.ScriptList;
    for i := 0 to ScriptList.Count - 1 do
      gametype := gametype + iif(i <> 0, ', ', '') + ScriptList.Strings[i];
    ServerSendStringMessage('Scripts: ' + gametype, Sender, 255, MSGTYPE_PUB);
    ScriptList.Free();
  end;
  {$ENDIF}

  if LoadedWMChecksum <> DefaultWMChecksum then
    ServerSendStringMessage('Server uses weapon mod "' +
      WideString(WMName) + ' v' + WideString(WMVersion) + '" (checksum ' +
      WideString(IntToStr(LoadedWMChecksum)) + ')', Sender, 255, MSGTYPE_PUB);
end;

procedure CommandAdminlog(Args: array of AnsiString; Sender: Byte);
var
  Adminlog: String;
begin
  if Length(Args) = 1 then
    Exit;

  Adminlog := Args[1];
  if Length(sv_adminpassword.Value) > 0 then
  begin
    if Adminlog = sv_adminpassword.Value then
    begin
      if not IsAdminIP(Sprite[Sender].Player.IP) then
        AdminIPs.Add(Sprite[Sender].Player.IP);
      MainConsole.Console(Sprite[Sender].Player.Name + ' added to Game Admins',
        SERVER_MESSAGE_COLOR, Sender);
    end
    else
    begin
      MainConsole.Console(Sprite[Sender].Player.Name + ' tried to login as Game Admin with bad password',
        SERVER_MESSAGE_COLOR, Sender);
    end;
  end;
end;

procedure CommandVotemap(Args: array of AnsiString; Sender: Byte);
var
  MapName: String;
begin
  if Length(Args) = 1 then
    Exit;

  MapName := Args[1];

  if VoteActive then
  begin
    // check if the vote target is actually the target
    if VoteTarget <> string(MapName) then
      Exit;

    // check if he already voted
    if VoteHasVoted[Sender] then
      Exit;

    {$IFDEF SCRIPT}
    ScrptDispatcher.OnVoteMap(Sender, MapName);
    {$ENDIF}
    CountVote(Sender);
  end
  else
  begin
    if (MapsList.IndexOf(MapName) <> -1) {and (MapExists(MapName, UserDirectory))} then
    begin
      if VoteCooldown[Sender] < 0 then
      begin
        if not VoteActive then
        begin
          {$IFDEF SCRIPT}
            if ScrptDispatcher.OnVoteMapStart(Sender, MapName) then
              Exit;
          {$ENDIF}
          StartVote(Sender, VOTE_MAP, MapName, '---');
          ServerSendVoteOn(VoteType, Sender, MapName, '---');
        end;
      end
      else
        ServerSendStringMessage('Can''t vote for 2:00 minutes after joining game or last vote', Sender, 255, MSGTYPE_PUB);
    end
    else
      ServerSendStringMessage('Map not found (' + WideString(MapName) + ')', Sender, 255, MSGTYPE_PUB);
  end;
end;

{$IFDEF SCRIPT}
procedure CommandRecompile(Args: array of AnsiString; Sender: Byte);
var
  Name: String;
begin
  if Length(Args) = 1 then
  begin
    if not sc_enable.Value then
      MainConsole.Console('Scripting is currently disabled.', CLIENT_MESSAGE_COLOR, Sender)
    else
    begin
      ScrptDispatcher.Prepare();
      ScrptDispatcher.Launch();
    end;
  end else
  begin
    Name := Args[1];
    if not sc_enable.Value then
      MainConsole.Console('Scripting is currently disabled.', CLIENT_MESSAGE_COLOR, Sender)
    else
      ScrptDispatcher.Launch(Name);
  end;
end;
{$ENDIF}

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

procedure CommandStop(Args: array of AnsiString; Sender: Byte);
begin
  DemoRecorder.StopRecord;
end;

{$POP}

procedure InitServerCommands();
begin
  CommandAdd('addbot', CommandAddbot, 'Add specific bot to game', [CMD_ADMINONLY]);
  CommandAdd('addbot1', CommandAddbot, 'Add specific bot to alpha team', [CMD_ADMINONLY]);
  CommandAdd('addbot2', CommandAddbot, 'Add specific bot to blue team', [CMD_ADMINONLY]);
  CommandAdd('addbot3', CommandAddbot, 'Add specific bot to charlie team', [CMD_ADMINONLY]);
  CommandAdd('addbot4', CommandAddbot, 'Add specific bot to delta team', [CMD_ADMINONLY]);
  CommandAdd('addbot5', CommandAddbot, 'Add specific bot to spectators', [CMD_ADMINONLY]);
  CommandAdd('nextmap', CommandNextmap, 'Change map to next in maplist', [CMD_ADMINONLY]);
  CommandAdd('map', CommandMap, 'Change map to specified mapname', [CMD_ADMINONLY]);
  CommandAdd('pause', CommandPause, 'Pause game', [CMD_ADMINONLY]);
  CommandAdd('unpause', CommandUnpause, 'Unpause game', [CMD_ADMINONLY]);
  CommandAdd('restart', CommandRestart, 'Restart current map', [CMD_ADMINONLY]);
  CommandAdd('kick', CommandKick, 'Kick player with specified nick or id', [CMD_ADMINONLY]);
  CommandAdd('kicklast', CommandKicklast, 'Kick last connected player', [CMD_ADMINONLY]);
  CommandAdd('ban', CommandBan, 'Ban player with specified nick or id', [CMD_ADMINONLY]);
  CommandAdd('banip', CommandBaniphw, 'Ban specified IP address', [CMD_ADMINONLY]);
  CommandAdd('banhw', CommandBaniphw, 'Ban specified hwid', [CMD_ADMINONLY]);
  CommandAdd('unban', CommandUnban, 'Unban specified ip or hwid', [CMD_ADMINONLY]);
  CommandAdd('unbanlast', CommandUnban, 'Unban last player', [CMD_ADMINONLY]);
  CommandAdd('adm', CommandAdm, 'Give admin to specified nick or id', [CMD_ADMINONLY]);
  CommandAdd('admip', CommandAdmip, 'add the IP number to the Remote Admins list', [CMD_ADMINONLY]);
  CommandAdd('unadm', CommandUnadm, 'remove the IP number from the admins list', [CMD_ADMINONLY]);
  CommandAdd('setteam1', CommandSetteam, 'move specified id or nick to alpha team', [CMD_ADMINONLY]);
  CommandAdd('setteam2', CommandSetteam, 'move specified id or nick to bravo team', [CMD_ADMINONLY]);
  CommandAdd('setteam3', CommandSetteam, 'move specified id or nick to charlie team', [CMD_ADMINONLY]);
  CommandAdd('setteam4', CommandSetteam, 'move specified id or nick to delta team', [CMD_ADMINONLY]);
  CommandAdd('setteam5', CommandSetteam, 'move specified id or nick to spectators', [CMD_ADMINONLY]);
  CommandAdd('say', CommandSay, 'Send chat message', [CMD_ADMINONLY]);
  CommandAdd('pkill', CommandKill, 'Kill specified id or nick', [CMD_ADMINONLY]);
  CommandAdd('loadwep', CommandLoadwep, 'Load weapons config', [CMD_ADMINONLY]);
  CommandAdd('loadcon', CommandLoadcon, 'Load server config', [CMD_ADMINONLY]);
  CommandAdd('loadlist', CommandLoadlist, 'Load maplist', [CMD_ADMINONLY]);
  CommandAdd('pm', CommandPm, 'Send private message to other player', [CMD_ADMINONLY]);
  CommandAdd('gmute', CommandGmute, 'Mute player on server', [CMD_ADMINONLY]);
  CommandAdd('ungmute', CommandUngmute, 'Unmute player on server', [CMD_ADMINONLY]);
  CommandAdd('addmap', CommandAddmap, 'Add map to the maplist', [CMD_ADMINONLY]);
  CommandAdd('delmap', CommandDelmap, 'Remove map from the maplist', [CMD_ADMINONLY]);
  CommandAdd('weaponon', CommandWeaponon, 'Enable specific weapon on the server', [CMD_ADMINONLY]);
  CommandAdd('weaponoff', CommandWeaponoff, 'Disable specific weapon on the server', [CMD_ADMINONLY]);
  CommandAdd('banlist', CommandBanlist, 'Show banlist', [CMD_ADMINONLY]);
  CommandAdd('net_stats', CommandNetStats, 'Show network stats', [CMD_ADMINONLY]);

  CommandAdd('tabac', CommandPlayerCommand, 'tabac', [CMD_PLAYERONLY]);
  CommandAdd('smoke', CommandPlayerCommand, 'smoke', [CMD_PLAYERONLY]);
  CommandAdd('takeoff', CommandPlayerCommand, 'takeoff', [CMD_PLAYERONLY]);
  CommandAdd('victory', CommandPlayerCommand, 'victory', [CMD_PLAYERONLY]);
  CommandAdd('piss', CommandPlayerCommand, 'piss', [CMD_PLAYERONLY]);
  CommandAdd('mercy', CommandPlayerCommand, 'mercy', [CMD_PLAYERONLY]);
  CommandAdd('pwn', CommandPlayerCommand, 'pwn', [CMD_PLAYERONLY]);
  CommandAdd('kill', CommandPlayerCommand, 'kill', [CMD_PLAYERONLY]);
  CommandAdd('brutalkill', CommandPlayerCommand, 'brutalkill', [CMD_PLAYERONLY]);
  CommandAdd('info', CommandInfo, 'info', [CMD_PLAYERONLY]);
  CommandAdd('adminlog', CommandAdminlog, 'adminlog', [CMD_PLAYERONLY]);
  CommandAdd('votemap', CommandVotemap, 'votemap', [CMD_PLAYERONLY]);
  CommandAdd('record', CommandRecord, 'record demo', [CMD_ADMINONLY]);
  CommandAdd('stop', CommandStop, 'stop recording demo', [CMD_ADMINONLY]);

  {$IFDEF SCRIPT}
  CommandAdd('recompile', CommandRecompile, 'Recompile all or specific script', [CMD_ADMINONLY]);
  {$ENDIF}
end;

end.

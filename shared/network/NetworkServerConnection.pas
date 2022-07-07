unit NetworkServerConnection;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // helper units
  Vector, Util, Cvar, BitStream,

  {$IFDEF SCRIPT}ScriptDispatcher,{$ENDIF}

  // opensoldat units
  PolyMap, {$IFDEF SERVER}Steam,{$ENDIF} Net, Sprites, Weapons, Constants;

  {$IFDEF SERVER}
  procedure ServerHandleRequestGame(NetMessage: PSteamNetworkingMessage_t);
  procedure ServerHandlePlayerInfo(NetMessage: PSteamNetworkingMessage_t);
  {$ENDIF}
  procedure ServerSendPlayList({$IFDEF SERVER}Peer: HSteamNetConnection{$ENDIF});
  procedure ServerSendNewPlayerInfo(Num: Byte; JoinType: Byte);
  {$IFDEF SERVER}
  function GetBanStrForIndex(BanIndex: Integer; BanHW: Boolean = False): string; // TODO move?
  procedure ServerSendUnAccepted(Peer: HSteamNetConnection; State: Byte; Message: string = '');
  procedure ServerDisconnect;
  procedure ServerPlayerDisconnect(Num, Why: Byte);
  procedure ServerPing(ToNum: Byte);
  {$ENDIF}
  procedure ServerSyncCvars({$IFDEF SERVER}ToNum: Byte; peer: HSteamNetConnection;{$ENDIF} FullSync: Boolean = False);
  procedure ServerVars({$IFDEF SERVER}ToNum: Byte{$ENDIF});
  {$IFDEF SERVER}
  procedure ServerHandlePong(NetMessage: PSteamNetworkingMessage_t);
  {$ENDIF}

implementation

uses
    Game, NetworkUtils, Demo
    {$IFDEF SERVER}, Server, ServerHelper, Sha1, NetworkServerGame, NetworkServerMessages,
    NetworkServerThing, BanSystem, Things, Version, LogFile, TraceLog{$ELSE}, Client {$ENDIF}
    {$IFDEF ENABLE_FAE}, NetworkServerFae{$ENDIF}
    ;

{$IFDEF SERVER}
var
  PingTime: array[1..MAX_PLAYERS, -1..9] of Integer;
  PingSendCount: array[1..MAX_PLAYERS] of Byte;
{$ENDIF}

{$IFDEF SERVER}
procedure ServerHandleRequestGame(NetMessage: PSteamNetworkingMessage_t);
var
  Player: TPlayer;
  RequestMsg: TMsg_RequestGame;
  State: Cardinal;
  BanIndex: Integer;
  ID: Integer;
  BanReason: string;
  BanHW: Boolean;
begin
  if not VerifyPacketLargerOrEqual(sizeof(TMsg_RequestGame), NetMessage^.m_cbSize, MsgID_RequestGame) then
    Exit;

  //Player := TPlayer(NetMessage^.m_pData);
  Player := TPlayer(NetMessage^.m_nConnUserData);
  BanIndex := 0;
  BanHW := False;
  BanReason := '';

  // Fancy packet filter
  ID := UpdateAntiFlood(Player.IP);
  if IsFloodID(ID) then
    Exit;

  RequestMsg := PMsg_RequestGame(NetMessage^.m_pData)^;

  // Begin by checking the client's version
  if IsWrongGameVersion(RequestMsg.Version) then
  begin
    State := WRONG_VERSION;
    ServerSendUnAccepted(NetMessage^.m_conn, State);
    Exit;
  end;

  {$IFDEF ENABLE_FAE}
  // Reject clients without anti-cheat if this server requires it
  if (ac_enable.Value) and (RequestMsg.HaveAntiCheat <> ACTYPE_FAE) then
  begin
    State := ANTICHEAT_REQUIRED;
    ServerSendUnAccepted(NetMessage^.m_conn, State);
    Exit;
  end;
  {$ENDIF}

  if IsServerTotallyFull then
    State := SERVER_FULL
  else if IsRemoteAdminIP(Player.IP) then
    State := OK
  else if IsAdminPassword(PChar(@PMsg_RequestGame(NetMessage^.m_pData)^.Password)) then
  begin
    if AddIPToRemoteAdmins(Player.IP) then
      MainConsole.Console(Player.IP +
        ' added to Game Admins via Request Password', SERVER_MESSAGE_COLOR);
    State := OK;
  end
  else
  begin
    BanIndex := FindBan(Player.IP);

    if BanIndex > 0 then
    begin
      State := BANNED_IP;
      BanReason := ' (' + BannedIPList[BanIndex].Reason + ')';
    end
    else
    begin
      {$IFDEF STEAM}
      BanIndex := FindBanHW(IntToStr(CSteamID(Player.SteamID).m_unAccountID));
      {$ELSE}
      BanIndex := FindBanHW(RequestMsg.HardwareID);
      {$ENDIF}
      if BanIndex > 0 then
      begin
        State := BANNED_IP;
        BanHW := True;
        BanReason := ' (' + BannedHWList[BanIndex].Reason + ')';
      end
      else
      if IsWrongGamePassword(PChar(@PMsg_RequestGame(NetMessage^.m_pData)^.Password)) then
        State := WRONG_PASSWORD
      else if IsServerFull then
        State := SERVER_FULL
      else
        State := OK;
    end;
  end;

  {$IFDEF STEAM}
  if sv_steamonly.Value then
    if UInt64(Player.SteamID) = 0 then
      State := STEAM_ONLY;
  {$ENDIF}

  MainConsole.Console(Player.IP + ':' + IntToStr(Player.Port) +
    '|' + {$IFDEF STEAM}SteamID3(CSteamID(Player.SteamID)){$ELSE}RequestMsg.HardwareID{$ENDIF} +
    ' requesting game' + BanReason + '...', SERVER_MESSAGE_COLOR);

  {$IFDEF SCRIPT}
  if sc_enable.Value then
  begin
    State := ScrptDispatcher.OnRequestGame(Player.IP,
      {$IFDEF STEAM}
      SteamID3(CSteamID(Player.SteamID))
      {$ELSE}
      RequestMsg.HardwareID
      {$ENDIF},
      Player.Port, State, Boolean(RequestMsg.Forwarded),
      PChar(@PMsg_RequestGame(NetMessage^.m_pData)^.Password));
  end;
  {$ENDIF}

  if State = OK then
  begin
    Player.GameRequested := True;
    {$IFDEF ENABLE_FAE}
    if ac_enable.Value then
      ServerSendFaeChallenge(NetMessage^.m_conn, True);
    {$ENDIF}
    ServerSyncCvars(0, NetMessage^.m_conn, True);
    ServerSendPlayList(NetMessage^.m_conn);
  end
  else
  begin
    Debug('Request rejected: ' + IntToStr(State));
    ServerSendUnAccepted(NetMessage^.m_conn, State, GetBanStrForIndex(BanIndex, BanHw));
  end;
end;

procedure ServerHandlePlayerInfo(NetMessage: PSteamNetworkingMessage_t);
var
  PlayerInfoMsg: PMsg_PlayerInfo;
  Player: TPlayer;
  FixedPlayerName, FinalPlayerName: AnsiString;
  PlayerNameUnused: Boolean;
  SuffixLen: Integer;
  i, j: Integer;
  a, b: TVector2;
  {$IFDEF SCRIPT}NewTeam: ShortInt;{$ENDIF}
begin
  if not VerifyPacket(sizeof(TMsg_PlayerInfo), NetMessage^.m_cbSize, MsgID_PlayerInfo) then
    Exit;

  PlayerInfoMsg := PMsg_PlayerInfo(NetMessage^.m_pData);
  Player := TPlayer(NetMessage^.m_nConnUserData);
  Assert(Player.SpriteNum = 0);

  {$IFDEF ENABLE_FAE}
  if Player.FaeKicked then
  begin
    Debug('Rejecting, FaeKicked flag is set');
    Exit; // already sent unaccept message in NetworkServerFae
  end;

  // We expect the client to send a Fae response prior to sending its player info.
  // Note that in case of Fae being disabled that boolean is always false, so no need to check the cvar.
  // This is guaranteed to arrive in order due to GameNetworkingSockets magic, thus we can just check the pending bool:
  if Player.FaeResponsePending then
  begin
    Debug('Rejecting, no Fae response');
    ServerSendUnAccepted(Event.peer, INVALID_HANDSHAKE);
    Exit;
  end;
  {$ENDIF}

  if sv_pure.Value and
     ((not Sha1Match(TSHA1Digest(PlayerInfoMsg.GameModChecksum), GameModChecksum)) or
     (not Sha1Match(TSHA1Digest(PlayerInfoMsg.CustomModChecksum), CustomModChecksum))) then
  begin
    ServerSendUnAccepted(Player.peer, WRONG_CHECKSUM);
    Exit;
  end;

  if IsServerTotallyFull or (((PlayersNum - BotsNum) >= sv_maxplayers.Value) and (not IsRemoteAdminIP(Player.IP))) then
  begin
    ServerSendUnAccepted(Player.peer, SERVER_FULL);
    Exit;
  end;

  if PlayerInfoMsg.Team > TEAM_SPECTATOR then
  begin
    ServerSendUnAccepted(Player.peer, INVALID_HANDSHAKE);
    Exit;
  end;

  // change name if is already taken
  FixedPlayerName := FixPlayerName(PlayerInfoMsg.Name);
  FinalPlayerName := FixedPlayerName;
  j := 0;
  repeat
    PlayerNameUnused := True;
    for i := 1 to MAX_PLAYERS do
      if (Sprite[i].Active) and (Sprite[i].Player.Name = FinalPlayerName) then
      begin
        SuffixLen := Length(IntToStr(j)) + 2;
        Inc(j);

        // Truncate nick too long to append (NN) for duplicate names
        if Length(FinalPlayerName) + SuffixLen <= 24 then
          FinalPlayerName := FixedPlayerName + '(' + IntToStr(j) + ')'
        else
          FinalPlayerName := LeftStr(FixedPlayerName, 22 - Length(IntToStr(j))) + '(' + IntToStr(j) + ')';
        PlayerNameUnused := False;
      end;
  until PlayerNameUnused;

  MainConsole.Console(FinalPlayerName + ' joining game (' + Player.IP +
    ':' + IntToStr(Player.Port) + ') HWID:' +
    {$IFDEF STEAM}SteamID3(Player.SteamID){$ELSE}Player.HWID{$ENDIF},
    SERVER_MESSAGE_COLOR);

  // Set a network name for debugging purposes
  UDP.SetConnectionName(Player.peer, FinalPlayerName);

  Player.Name := FinalPlayerName;
  Player.ShirtColor := PlayerInfoMsg.ShirtColor and $00FFFFFF;
  Player.PantsColor := PlayerInfoMsg.PantsColor and $00FFFFFF;
  Player.SkinColor := PlayerInfoMsg.SkinColor and $00FFFFFF;
  Player.HairColor := PlayerInfoMsg.HairColor;
  Player.JetColor := PlayerInfoMsg.JetColor;
  Player.ControlMethod := HUMAN;

  // select team and fix an impossible team selection
  Player.Team := PlayerInfoMsg.Team;
  case sv_gamemode.Value of
    // no teams
    GAMESTYLE_DEATHMATCH, GAMESTYLE_POINTMATCH, GAMESTYLE_RAMBO:
      if (Player.Team <> TEAM_NONE) and (Player.Team <> TEAM_SPECTATOR) then
        Player.Team := TEAM_SPECTATOR;
    // two teams
    GAMESTYLE_CTF, GAMESTYLE_INF, GAMESTYLE_HTF:
      if (Player.Team < TEAM_ALPHA) or (Player.Team > TEAM_BRAVO) then
        Player.Team := TEAM_SPECTATOR;
    // four teams
    GAMESTYLE_TEAMMATCH:
      if (Player.Team < TEAM_ALPHA) or (Player.Team > TEAM_DELTA) then
        Player.Team := TEAM_SPECTATOR;
  end;

  // enforce spectator limit
  if (PlayerInfoMsg.Team = TEAM_SPECTATOR) and (SpectatorsNum >= sv_maxspectators.Value) then
  begin
    if not IsRemoteAdminIP(Player.IP) then
    begin
      ServerSendUnAccepted(Player.Peer, SERVER_FULL);
      Exit;
    end;
  end;

  // restore warnings across sessions
  for j := 1 to MAX_PLAYERS do
    if Trim(mutelist[j]) = Player.IP then
    begin
      Player.Muted := 1;
      Player.Name := mutename[j];
      Break;
    end;
  Player.TKWarnings := 0;
  for j := 1 to MAX_PLAYERS do
    if Trim(TKList[j]) = Player.IP then
    begin
      Player.TKWarnings := TKListKills[j];
      Break;
    end;

  Player.PlayTime := 0;

  Player.HairStyle := 0;
  if PlayerInfoMsg.Look and B1 = B1 then Player.HairStyle := 1;
  if PlayerInfoMsg.Look and B2 = B2 then Player.HairStyle := 2;
  if PlayerInfoMsg.Look and B3 = B3 then Player.HairStyle := 3;
  if PlayerInfoMsg.Look and B4 = B4 then Player.HairStyle := 4;

  Player.HeadCap := 0;
  if PlayerInfoMsg.Look and B5 = B5 then Player.HeadCap := GFX_GOSTEK_HELM;
  if PlayerInfoMsg.Look and B6 = B6 then Player.HeadCap := GFX_GOSTEK_KAP;

  Player.Chain := 0;
  if PlayerInfoMsg.Look and B7 = B7 then Player.Chain := 1;
  if PlayerInfoMsg.Look and B8 = B8 then Player.Chain := 2;

  {$IFDEF SCRIPT}
  NewTeam := ScrptDispatcher.OnBeforeJoinTeam(Player.SpriteNum, Player.Team, 255);
  if (NewTeam >= TEAM_NONE) and (NewTeam <= TEAM_SPECTATOR) then
    Player.Team := NewTeam
  else
  begin
    Sprite[Player.SpriteNum].Kill;
    ServerSendUnAccepted(Player.Peer, SERVER_FULL);
    Exit;
  end;
  {$ENDIF}

  Player.ApplyShirtColorFromTeam;

  // ------------------------
  // NOTE we're done fixing up the Player structure now -- don't change it below this comment
  // next the player's sprite is initialized

  // create sprite and assign it our player object
  a := Default(TVector2);
  b := Default(TVector2);
  RandomizeStart(a, PlayerInfoMsg.Team);
  CreateSprite(a, b, 1, 255, Player, False); // assigns Player.SpriteNum

  // respawn holded thing if is
  // FIXME english
  // TODO do a good bit of these things in CreateSprite instead?
  if Sprite[Player.SpriteNum].HoldedThing > 0 then
    if (Thing[Sprite[Player.SpriteNum].HoldedThing].Style < OBJECT_USSOCOM) then
    begin
      Thing[Sprite[Player.SpriteNum].HoldedThing].Respawn;
      Sprite[Player.SpriteNum].HoldedThing := 0;
    end;
  for i := 1 to MAX_THINGS do
    if Thing[Player.SpriteNum].HoldingSprite = Player.SpriteNum then
      Thing[Player.SpriteNum].Respawn;
  Sprite[Player.SpriteNum].HasPack := False;
  Sprite[Player.SpriteNum].Respawn; // FIXME do this later?

  if sv_survivalmode.Value then
  begin
    Sprite[Player.SpriteNum].HealthHit(150, Player.SpriteNum, 1, -1, a);
    Dec(Sprite[Player.SpriteNum].Player.Deaths);
  end;

  // reset legacy-ish counters that are stored under the sprite ID
  // TODO: most of this can be moved to TPlayer
  NoClientupdateTime[Player.SpriteNum] := 0;
  MessagesASecNum[Player.SpriteNum] := 0;
  FloodWarnings[Player.SpriteNum] := 0;
  PingWarnings[Player.SpriteNum] := 0;
  BulletTime[Player.SpriteNum] := -1000;
  GrenadeTime[Player.SpriteNum] := -1000;
  KnifeCan[Player.SpriteNum] := True;
  BulletWarningCount[Player.SpriteNum] := 0;
  CheatTag[Player.SpriteNum] := 0;
  VoteCooldown[Player.SpriteNum] := DEFAULT_VOTE_TIME;
  LastPlayer := Player.SpriteNum; // for /kicklast command

  ServerSendNewPlayerInfo(Player.SpriteNum, JOIN_NORMAL);
  ServerThingMustSnapshotOnConnect(Player.SpriteNum);
  ServerVars(Player.SpriteNum);
  ServerSyncMsg(Player.SpriteNum);

  // greetings message
  if Length(sv_greeting.Value) > 0 then
    ServerSendStringMessage(WideString(sv_greeting.Value), Player.SpriteNum, 255, MSGTYPE_PUB);
  if Length(sv_greeting2.Value) > 0 then
    ServerSendStringMessage(WideString(sv_greeting2.Value), Player.SpriteNum, 255, MSGTYPE_PUB);
  if Length(sv_greeting3.Value) > 0 then
    ServerSendStringMessage(WideString(sv_greeting3.Value), Player.SpriteNum, 255, MSGTYPE_PUB);

  case Player.Team of
    TEAM_NONE: MainConsole.Console(Player.Name + ' has joined the game.', ENTER_MESSAGE_COLOR);
    TEAM_ALPHA: MainConsole.Console(Player.Name + ' has joined alpha team.', ALPHAJ_MESSAGE_COLOR);
    TEAM_BRAVO: MainConsole.Console(Player.Name + ' has joined bravo team.', BRAVOJ_MESSAGE_COLOR);
    TEAM_CHARLIE: MainConsole.Console(Player.Name + ' has joined charlie team.', CHARLIEJ_MESSAGE_COLOR);
    TEAM_DELTA: MainConsole.Console(Player.Name + ' has joined delta team.', DELTAJ_MESSAGE_COLOR);
    TEAM_SPECTATOR: MainConsole.Console(Player.Name + ' has joined as spectator.', DELTAJ_MESSAGE_COLOR);
  end;

  // check if map change is in progress
  if (MapChangeCounter > -60) and (MapChangeCounter < 99999999) then
    ServerMapChange(Player.SpriteNum);

  {$IFDEF SCRIPT}
  ScrptDispatcher.OnJoinTeam(Sprite[Player.SpriteNum].Num, Player.Team, Player.Team, True);
  {$ENDIF}

  {$IFDEF STEAMSTATS}
  RequestUserStats(Player.SteamID);
  {$ENDIF}

  if Sprite[Player.SpriteNum].Active then // FIXME like above, it's always active is it not?
  begin
    DoBalanceBots(0, Player.Team);
  end;

  // flood from IP prevention
  // TODO eek, this should rather be done with enet (reject on connect, or using a built-in feature)
  j := 0;
  for i := 1 to MAX_FLOODIPS do
    if FloodIP[i] = Player.IP then
    begin
      j := i;
      Break;
    end;
  if j > 0 then
  begin
    Inc(FloodNum[j]);
    if FloodNum[j] > FLOODIP_MAX then
      KickPlayer(Player.SpriteNum, True, KICK_FLOODING, TWENTY_MINUTES, 'Join game flood');
  end;
  if j = 0 then
    for i := 1 to MAX_FLOODIPS do
      if FloodIP[i] = ' ' then
      begin
        FloodIP[i] := Player.IP;
        Break;
      end;
end;
{$ENDIF}

procedure ServerSendPlayList({$IFDEF SERVER}Peer: HSteamNetConnection{$ENDIF});
var
  PlayersList: TMsg_PlayersList;
  i: Integer;
begin
  PlayersList.Header.ID := MsgID_PlayersList;

  StringToArray(PlayersList.ModName, fs_mod.Value);
  PlayersList.ModChecksum := CustomModChecksum;

  StringToArray(PlayersList.MapName, Map.Name);
  PlayersList.MapChecksum := MapChecksum;

  PlayersList.Players := PlayersNum;
  PlayersList.CurrentTime := TimeLimitCounter;

  {$IFDEF SERVER}
  PlayersList.ServerTicks := ServerTickCounter;
  {$ELSE}
  PlayersList.ServerTicks := MainTickCounter;
  {$ENDIF}


  {$IFDEF ENABLE_FAE}
  PlayersList.AntiCheatRequired := ac_enable.Value;
  {$ELSE}
  PlayersList.AntiCheatRequired := False;
  {$ENDIF}

  for i := 1 to MAX_SPRITES do
  begin
    if (Sprite[i].Active) and (not Sprite[i].Player.DemoPlayer) then
    begin
      StringToArray(PlayersList.Name[i], Sprite[i].Player.Name);
      PlayersList.ShirtColor[i] := $FF000000 or Sprite[i].Player.ShirtColor;
      PlayersList.PantsColor[i] := $FF000000 or Sprite[i].Player.PantsColor;
      PlayersList.SkinColor[i] := $FF000000 or Sprite[i].Player.SkinColor;
      PlayersList.HairColor[i] := $FF000000 or Sprite[i].Player.HairColor;
      PlayersList.JetColor[i] := Sprite[i].Player.JetColor;
      PlayersList.Team[i] := Sprite[i].Player.Team;
      PlayersList.PredDuration[i] := iif(Sprite[i].BonusStyle = BONUS_PREDATOR,
        (Sprite[i].BonusTime / 60), 0);

      PlayersList.Look[i] := 0;
      if Sprite[i].Player.HairStyle = 1 then
        PlayersList.Look[i] := PlayersList.Look[i] or B1;
      if Sprite[i].Player.HairStyle = 2 then
        PlayersList.Look[i] := PlayersList.Look[i] or B2;
      if Sprite[i].Player.HairStyle = 3 then
        PlayersList.Look[i] := PlayersList.Look[i] or B3;
      if Sprite[i].Player.HairStyle = 4 then
        PlayersList.Look[i] := PlayersList.Look[i] or B4;
      if Sprite[i].Player.HeadCap = GFX_GOSTEK_HELM then
        PlayersList.Look[i] := PlayersList.Look[i] or B5;
      if Sprite[i].Player.HeadCap = GFX_GOSTEK_KAP then
        PlayersList.Look[i] := PlayersList.Look[i] or B6;
      if Sprite[i].Player.Chain = 1 then
        PlayersList.Look[i] := PlayersList.Look[i] or B7;
      if Sprite[i].Player.Chain = 2 then
        PlayersList.Look[i] := PlayersList.Look[i] or B8;

      PlayersList.Pos[i] := SpriteParts.Pos[Sprite[i].Num];
      PlayersList.Vel[i] := SpriteParts.Velocity[Sprite[i].Num];
      PlayersList.SteamID[i] := {$IFDEF STEAM}UInt64(Sprite[i].Player.SteamID){$ELSE}0{$ENDIF};
    end else
    begin
      StringToArray(PlayersList.Name[i], '0 ');
      PlayersList.ShirtColor[i] := 0;
      PlayersList.PantsColor[i] := 0;
      PlayersList.SkinColor[i] := 0;
      PlayersList.HairColor[i] := 0;
      PlayersList.JetColor[i] := 0;
      PlayersList.Team[i] := TEAM_NONE;
      PlayersList.PredDuration[i] := 0;
      PlayersList.Look[i] := 0;
      PlayersList.Pos[i] := Vector2(0, 0);
      PlayersList.Vel[i] := Vector2(0, 0);
      PlayersList.SteamID[i] := 0;
    end;
  end;

  {$IFDEF SERVER}
  UDP.SendData(PlayersList, sizeof(PlayersList), Peer, k_nSteamNetworkingSend_Reliable);
  {$ELSE}
  DemoRecorder.SaveRecord(PlayersList, sizeof(PlayersList));
  {$ENDIF}
end;

{$IFDEF SERVER}
function GetBanStrForIndex(BanIndex: Integer; BanHW: Boolean = False): string;
var
  BanTimeStr: string;
begin
  // check if has ban description
  if BanIndex < 0 then
  begin
    BanTimeStr := '';
  end else
  begin
    if BanHW then
    begin
      // get ban time
      if ((BannedHWList[BanIndex].Time + 1) div 3600 >= MinsPerDay) then
      begin
        BanTimeStr := '24+ Hrs';
      end else
      begin
        if ((BannedHWList[BanIndex].Time div 3600) >= MinsPerHour) then
          BanTimeStr := IntToStr((BannedHWList[BanIndex].Time + 1) div
            3600 div MinsPerHour) + 'h'
        else
          BanTimeStr := IntToStr((BannedHWList[BanIndex].Time + 1) div
            3600) + 'm'
      end;
      // add ban reason
      BanTimeStr := BannedHWList[BanIndex].Reason + ' (' + BanTimeStr + ')';
    end
    else
    begin
      // get ban time
      if ((BannedIPList[BanIndex].Time + 1) div 3600 >= MinsPerDay) then
      begin
        BanTimeStr := '24+ Hrs';
      end else
      begin
        if ((BannedIPList[BanIndex].Time div 3600) >= MinsPerHour) then
          BanTimeStr := IntToStr((BannedIPList[BanIndex].Time + 1) div
            3600 div MinsPerHour) + 'h'
        else
          BanTimeStr := IntToStr((BannedIPList[BanIndex].Time + 1) div
            3600) + 'm'
      end;
      // add ban reason
      BanTimeStr := BannedIPList[BanIndex].Reason + ' (' + BanTimeStr + ')';
    end;
  end;
  Result := BanTimeStr;
end;

procedure ServerSendUnAccepted(Peer: HSteamNetConnection; State: Byte; Message: string = '');
var
  UnAccepted: PMsg_UnAccepted;
  Size: Integer;
  SendBuffer: TCharArray;
begin
  SendBuffer := Default(TCharArray);
  // request memory
  Size := SizeOf(TMsg_UnAccepted) + Length(Message) + 1;
  SetLength(SendBuffer, Size);  // can throw out of memory exception
  UnAccepted := PMsg_UnAccepted(SendBuffer);

  // fill memory
  UnAccepted.Header.ID := MsgID_UnAccepted;
  UnAccepted.State := State;
  UnAccepted.Version := OPENSOLDAT_VERSION;
  StrPCopy(UnAccepted.Text, Message);

  UDP.SendData(UnAccepted^, Size, Peer, k_nSteamNetworkingSend_Reliable);

  UDP.NetworkingSocket.CloseConnection(Peer, 0, '', true);
end;
{$ENDIF}

procedure ServerSendNewPlayerInfo(Num, JoinType: Byte);
var
  NewPlayer: TMsg_NewPlayer;
  {$IFDEF SERVER}
  DstPlayer: TPlayer;
  {$ENDIF}
begin
  NewPlayer.Header.ID := MsgID_NewPlayer;
  NewPlayer.Num := Num;
  NewPlayer.JoinType := JoinType;

  StringToArray(NewPlayer.Name, Sprite[Num].Player.Name);
  NewPlayer.ShirtColor := Sprite[Num].Player.ShirtColor and $00FFFFFF;
  NewPlayer.PantsColor := Sprite[Num].Player.PantsColor and $00FFFFFF;
  NewPlayer.SkinColor := Sprite[Num].Player.SkinColor and $00FFFFFF;
  NewPlayer.HairColor := Sprite[Num].Player.HairColor and $00FFFFFF;
  NewPlayer.JetColor := Sprite[Num].Player.JetColor;
  NewPlayer.Team := Sprite[Num].Player.Team;
  NewPlayer.Pos := SpriteParts.Pos[Num];
  NewPlayer.SteamID := {$IFDEF STEAM}UInt64(Sprite[Num].Player.SteamID){$ELSE}0{$ENDIF};

  if NewPlayer.Team = TEAM_SPECTATOR then
    NewPlayer.ShirtColor := ColorToHex($FFFFFF);

  NewPlayer.Look := 0;
  if Sprite[Num].Player.HairStyle = 1 then
    NewPlayer.Look := NewPlayer.Look or B1;
  if Sprite[Num].Player.HairStyle = 2 then
    NewPlayer.Look := NewPlayer.Look or B2;
  if Sprite[Num].Player.HairStyle = 3 then
    NewPlayer.Look := NewPlayer.Look or B3;
  if Sprite[Num].Player.HairStyle = 4 then
    NewPlayer.Look := NewPlayer.Look or B4;
  if Sprite[Num].Player.HeadCap = GFX_GOSTEK_HELM then
    NewPlayer.Look := NewPlayer.Look or B5;
  if Sprite[Num].Player.HeadCap = GFX_GOSTEK_KAP then
    NewPlayer.Look := NewPlayer.Look or B6;
  if Sprite[Num].Player.Chain = 1 then
    NewPlayer.Look := NewPlayer.Look or B7;
  if Sprite[Num].Player.Chain = 2 then
    NewPlayer.Look := NewPlayer.Look or B8;

  {$IFDEF SERVER}
  // NOTE we also send to pending players to avoid desynchronization of the players list
  if not Sprite[Num].Player.DemoPlayer then
  begin
    for DstPlayer in Players do
    begin
      NewPlayer.AdoptSpriteID := Byte(Num = DstPlayer.SpriteNum);
      UDP.SendData(NewPlayer, sizeof(NewPlayer), DstPlayer.Peer, k_nSteamNetworkingSend_Reliable);
    end;
  end else
  begin
    if DemoRecorder.Active then
    begin
      NewPlayer.AdoptSpriteID := Byte(Sprite[Num].Player.DemoPlayer = True);
      DemoRecorder.SaveRecord(NewPlayer, sizeof(NewPlayer));
    end;
  end;

  AddLineToLogFile(GameLog, ' Net - ' + Sprite[Num].Player.Name + ' connected ' +
    DateToStr(Date) + ' ' + TimeToStr(Time), ConsoleLogFileName);
  {$ELSE}
  NewPlayer.AdoptSpriteID := 1;
  DemoRecorder.SaveRecord(NewPlayer, sizeof(NewPlayer));
  {$ENDIF}
end;

{$IFDEF SERVER}
procedure ServerDisconnect;
var
  ServerMsg: TMsg_ServerDisconnect;
  i: Integer;
  DstPlayer: TPlayer;
begin
  ServerMsg.Header.ID := MsgID_ServerDisconnect;

  // NOTE send to pending like above
  for DstPlayer in Players do
    UDP.SendData(ServerMsg, sizeof(ServerMsg), DstPlayer.peer, k_nSteamNetworkingSend_Reliable);

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
    begin
      Sprite[i].Kill;
    end;
end;

procedure ServerPlayerDisconnect(Num, Why: Byte);
var
  PlayerMsg: TMsg_PlayerDisconnect;
  DstPlayer: TPlayer;
begin
  PlayerMsg.Header.ID := MsgID_PlayerDisconnect;
  PlayerMsg.Num := Num;
  PlayerMsg.Why := Why;

  // NOTE send to pending like above
  for DstPlayer in Players do
    UDP.SendData(PlayerMsg, sizeof(PlayerMsg), DstPlayer.peer, k_nSteamNetworkingSend_Reliable);

  AddLineToLogFile(GameLog, ' Net - ' + Sprite[Num].Player.Name +
    ' disconnected ' + DateToStr(Date) + ' ' + TimeToStr(Time), ConsoleLogFileName);
end;

procedure ServerPing(ToNum: Byte);
var
  PingMsg: TMsg_Ping;
begin
  PingMsg.Header.ID := MsgID_Ping;
  PingMsg.PingTicks := Sprite[ToNum].Player.PingTicks;

  if PingSendCount[ToNum] < 8 then
    Inc(PingSendCount[ToNum])
  else
    PingSendCount[ToNum] := 1;
  PingTime[ToNum, PingSendCount[ToNum]] := MainTickCounter;

  PingMsg.PingNum := PingSendCount[ToNum];

  UDP.SendData(PingMsg, sizeof(PingMsg), Sprite[ToNum].Player.Peer, k_nSteamNetworkingSend_Reliable);
end;
{$ENDIF}

procedure ServerSyncCvars({$IFDEF SERVER}ToNum: Byte; peer: HSteamNetConnection;{$ENDIF} FullSync: Boolean = False);
var
  VarsMsg: PMsg_ServerSyncCvars;
  i: Integer;
  FieldCount: Byte = 0;
  PacketStream: TBitWriter;
  Buffer: PByte;
  BufferSize: LongWord;
begin
  PacketStream := TBitWriter.Create(100);

  for i := 0 to CvarsSync.Count - 1 do
  begin
    if (not (CVAR_TOSYNC in TCvarBase(CvarsSync.Items[i]).Flags)) and (not FullSync) then
      Continue;

    if TObject(CvarsSync[i]) is TIntegerCvar then
    begin
      Inc(FieldCount);
      PacketStream.WriteUInt8(i);
      PacketStream.WriteInt32(TIntegerCvar(CvarsSync.Items[i]).Value);
    end;

    if TObject(CvarsSync[i]) is TSingleCvar then
    begin
      Inc(FieldCount);
      PacketStream.WriteUInt8(i);
      PacketStream.WriteSingle(TSingleCvar(CvarsSync.Items[i]).Value);
    end;

    if TObject(CvarsSync[i]) is TBooleanCvar then
    begin
      Inc(FieldCount);
      PacketStream.WriteUInt8(i);
      PacketStream.WriteBoolean(TBooleanCvar(CvarsSync.Items[i]).Value);
    end;

    if TObject(CvarsSync[i]) is TStringCvar then
    begin
      Inc(FieldCount);
      PacketStream.WriteUInt8(i);
      PacketStream.WriteString(TStringCvar(CvarsSync.Items[i]).Value);
    end;

    if not FullSync then
      TCvarBase(CvarsSync.Items[i]).SyncUpdate(False);
  end;

  PacketStream.CloneBuffer(Buffer, BufferSize);
  GetMem(VarsMsg, SizeOf(TMsg_ServerSyncCvars) + BufferSize);
  VarsMsg^.ItemCount := FieldCount;
  VarsMsg^.Header.ID := MsgID_SyncCvars;
  Move(Buffer[0], VarsMsg^.Data, BufferSize);

  {$IFDEF SERVER}
  CvarsNeedSyncing := False;
  if peer = 0 then
  begin
    for i := 1 to MAX_PLAYERS do
      if (ToNum = 0) or (i = ToNum) then
        if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
          UDP.SendData(VarsMsg^, SizeOf(TMsg_ServerSyncCvars) + BufferSize, Sprite[i].Player.Peer, k_nSteamNetworkingSend_Reliable);
  end else
    UDP.SendData(VarsMsg^, SizeOf(TMsg_ServerSyncCvars) + BufferSize, peer, k_nSteamNetworkingSend_Reliable);
  {$ELSE}
  DemoRecorder.SaveRecord(VarsMsg^, sizeof(VarsMsg) + BufferSize);
  {$ENDIF}
  FreeMem(VarsMsg);
  FreeMem(Buffer);
  PacketStream.Free;
end;

procedure ServerVars({$IFDEF SERVER}ToNum: Byte{$ENDIF});
var
  VarsMsg: TMsg_ServerVars;
  i: Integer;
  Gun: ^TGun;
  WeaponIndex: Integer;
begin
  VarsMsg.Header.ID := MsgID_ServerVars;

  for i := 1 to MAIN_WEAPONS do
    VarsMsg.WeaponActive[i] := WeaponActive[i];

  for WeaponIndex := 1 to ORIGINAL_WEAPONS do
  begin
    Gun := @Guns[WeaponIndex];
    VarsMsg.Damage[WeaponIndex]            := Gun.HitMultiply;
    VarsMsg.Ammo[WeaponIndex]              := Gun.Ammo;
    VarsMsg.ReloadTime[WeaponIndex]        := Gun.ReloadTime;
    VarsMsg.Speed[WeaponIndex]             := Gun.Speed;
    VarsMsg.BulletStyle[WeaponIndex]       := Gun.BulletStyle;
    VarsMsg.StartUpTime[WeaponIndex]       := Gun.StartUpTime;
    VarsMsg.Bink[WeaponIndex]              := Gun.Bink;
    VarsMsg.FireInterval[WeaponIndex]      := Gun.FireInterval;
    VarsMsg.MovementAcc[WeaponIndex]       := Gun.MovementAcc;
    VarsMsg.BulletSpread[WeaponIndex]      := Gun.BulletSpread;
    VarsMsg.Recoil[WeaponIndex]            := Gun.Recoil;
    VarsMsg.Push[WeaponIndex]              := Gun.Push;
    VarsMsg.InheritedVelocity[WeaponIndex] := Gun.InheritedVelocity;
    VarsMsg.ModifierHead[WeaponIndex]      := Gun.ModifierHead;
    VarsMsg.ModifierChest[WeaponIndex]     := Gun.ModifierChest;
    VarsMsg.ModifierLegs[WeaponIndex]      := Gun.ModifierLegs;
    VarsMsg.NoCollision[WeaponIndex]       := Gun.NoCollision;
  end;

  {$IFDEF SERVER}
  UDP.SendData(VarsMsg, sizeof(VarsMsg), Sprite[ToNum].Player.Peer, k_nSteamNetworkingSend_Reliable);
  {$ELSE}
  DemoRecorder.SaveRecord(VarsMsg, sizeof(VarsMsg));
  {$ENDIF}
end;

{$IFDEF SERVER}
procedure ServerHandlePong(NetMessage: PSteamNetworkingMessage_t);
var
  PongMsg: TMsg_Pong;
  Player: TPlayer;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_Pong), NetMessage^.m_cbSize, MsgID_Pong) then
    Exit;

  PongMsg := PMsg_Pong(NetMessage^.m_pData)^;
  Player := TPlayer(NetMessage^.m_nConnUserData);
  i := Player.SpriteNum;

  Inc(MessagesASecNum[i]);

  if (PongMsg.PingNum < 1) or (PongMsg.PingNum > 8) then
    Exit;

  Sprite[i].Player.PingTicks := MainTickCounter - PingTime[i, PongMsg.PingNum];
  Sprite[i].Player.PingTime := Sprite[i].Player.PingTicks * 1000 div 60;

  NoClientupdateTime[i] := 0;
end;
{$ENDIF}

end.

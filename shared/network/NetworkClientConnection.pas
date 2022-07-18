unit NetworkClientConnection;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // helper units
  Vector, Util, Version, BitStream,

  // anti-cheat units
  {$IFDEF ENABLE_FAE}FaeClient,{$ENDIF}

  // opensoldat units
  LogFile, Steam, Net, Sprites, Weapons, Constants, GameStrings,
  Cvar, PhysFS;

procedure ClientRequestGame;
procedure ClientSendPlayerInfo;
procedure ClientDisconnect;
procedure ClientPong(PingNum: Byte);
procedure ClientHandlePlayersList(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleUnAccepted(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleServerDisconnect(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandlePing(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleServerVars(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleSyncCvars(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  GameRendering, Client, Game, Demo, ClientGame, GameMenus, strutils,
  NetworkUtils, NetworkClientSprite, FileClient, Sha1, Anims, Sound;

// REQUEST GAME FROM SERVER
procedure ClientRequestGame;
var
  RequestMsg: PMsg_RequestGame;
  Size: Integer;
  SendBuffer: TCharArray;
begin
  SendBuffer := Default(TCharArray);
  Size := SizeOf(TMsg_RequestGame) + Length(JoinPassword) + 1;

  SetLength(SendBuffer, Size);

  RequestMsg := PMsg_RequestGame(SendBuffer);

  RequestMsg.Header.ID := MsgID_RequestGame;
  RequestMsg.Version := OPENSOLDAT_VERSION;

  RequestMsg.HaveAntiCheat := ACTYPE_NONE;

  {$IFDEF ENABLE_FAE}
  if FaeIsEnabled then
    RequestMsg.HaveAntiCheat := ACTYPE_FAE;
  {$ENDIF}

  if RedirectIP <> '' then
  begin
    RequestMsg.Forwarded := 1;
    RedirectIP := '';
    RedirectPort := 0;
    RedirectMsg := '';
  end;
  RequestMsg.HardwareID := HWID;

  StrPCopy(RequestMsg.Password, JoinPassword);
  UDP.SendData(RequestMsg^, Size, k_nSteamNetworkingSend_Reliable);
  RequestingGame := True;
  ReceivedUnAccepted := False;
end;

// SEND INFO ABOUT NAME, COLOR, PASS etc. TO SERVER OR CHANGE TEAM
procedure ClientSendPlayerInfo;
var
  PlayerInfo: TMsg_PlayerInfo;
  ChangeMsg: TMsg_ChangeTeam;
begin
  if (Spectator = 1) and (SelTeam = 0) then
    SelTeam := TEAM_SPECTATOR;
  Spectator := 0;  // allow joining other teams after first join
  if (sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF) or
     (sv_gamemode.Value = GAMESTYLE_HTF) then
    if (SelTeam < TEAM_ALPHA) or
       ((SelTeam > TEAM_BRAVO) and (SelTeam < TEAM_SPECTATOR)) then
    begin
      GameMenuShow(TeamMenu);
      Exit;
    end;
  if sv_gamemode.Value = GAMESTYLE_TEAMMATCH then
    if SelTeam < TEAM_ALPHA then
    begin
      GameMenuShow(TeamMenu);
      Exit;
    end;

  // sent a team change request instead if we're already ingame
  if MySprite > 0 then
  begin
    ChangeMsg.Header.ID := MsgID_ChangeTeam;
    ChangeMsg.Team := SelTeam;
    UDP.SendData(ChangeMsg, sizeof(ChangeMsg), k_nSteamNetworkingSend_Reliable);
    Exit;
  end;

  PlayerInfo.Header.ID := MsgID_PlayerInfo;
  StringToArray(PlayerInfo.Name, cl_player_name.Value);
  PlayerInfo.ShirtColor := (255 shl 24) + cl_player_shirt.Value;
  PlayerInfo.PantsColor := (255 shl 24) + cl_player_pants.Value;
  PlayerInfo.SkinColor := (255 shl 24) + cl_player_skin.Value;
  PlayerInfo.HairColor := (255 shl 24) + cl_player_hair.Value;
  PlayerInfo.JetColor := (cl_player_jet.Value and
    $00FFFFFF) +
    {$IFDEF DEVELOPMENT}
    COLOR_TRANSPARENCY_SPECIAL
    {$ELSE}
    COLOR_TRANSPARENCY_REGISTERED
    {$ENDIF};
  PlayerInfo.Team := SelTeam;
  PlayerInfo.Look := 0;
  if cl_player_hairstyle.Value = 1 then
    PlayerInfo.Look := PlayerInfo.Look or B1;
  if cl_player_hairstyle.Value = 2 then
    PlayerInfo.Look := PlayerInfo.Look or B2;
  if cl_player_hairstyle.Value = 3 then
    PlayerInfo.Look := PlayerInfo.Look or B3;
  if cl_player_hairstyle.Value = 4 then
    PlayerInfo.Look := PlayerInfo.Look or B4;
  if cl_player_headstyle.Value = HEADSTYLE_HELMET then
    PlayerInfo.Look := PlayerInfo.Look or B5;
  if cl_player_headstyle.Value = HEADSTYLE_HAT then
    PlayerInfo.Look := PlayerInfo.Look or B6;
  if cl_player_chainstyle.Value = 1 then
    PlayerInfo.Look := PlayerInfo.Look or B7;
  if cl_player_chainstyle.Value = 2 then
    PlayerInfo.Look := PlayerInfo.Look or B8;

  PlayerInfo.GameModChecksum := GameModChecksum;
  PlayerInfo.CustomModChecksum := CustomModChecksum;

  UDP.SendData(PlayerInfo, sizeof(PlayerInfo), k_nSteamNetworkingSend_Reliable);
  ClientPlayerSent := true;
  ClientPlayerReceivedCounter := CLIENTPLAYERRECIEVED_TIME;
end;

procedure ClientDisconnect;
var
  PlayerMsg: TMsg_PlayerDisconnect;
begin
  if MySprite > 0 then
  begin  // send disconnection info to server
    PlayerMsg.Header.ID := MsgID_PlayerDisconnect;
    PlayerMsg.Num := MySprite;

    UDP.SendData(PlayerMsg, sizeof(PlayerMsg), k_nSteamNetworkingSend_Reliable);

    AddLineToLogFile(GameLog, 'Client Disconnect from ' + UDP.GetStringAddress(@UDP.Address, True), ConsoleLogFileName);
    UDP.ProcessLoop;
    UDP.Disconnect(false);
  end else
  begin
    UDP.Disconnect(true);
    ExitToMenu;
  end;
end;

procedure ClientPong(PingNum: Byte);
var
  PongMsg: TMsg_Pong;
begin
  PongMsg.Header.ID := MsgID_Pong;
  PongMsg.PingNum := PingNum;

  UDP.SendData(PongMsg, sizeof(PongMsg), k_nSteamNetworkingSend_Reliable);
end;

procedure ClientHandlePlayersList(NetMessage: PSteamNetworkingMessage_t);
var
  PlayersListMsg: TMsg_PlayersList;
  i: Integer;
  Pos, Vel, b: TVector2;
  NewPlayer: TPlayer;
  DownloadURL: AnsiString;
  Checksum: TSHA1Digest;
  ForceGraphicsReload: Boolean = False;
  ModName: AnsiString;
  MapName: AnsiString;
  MapStatus: TMapInfo;
begin
  if not VerifyPacket(sizeof(PlayersListMsg), NetMessage^.m_cbSize, MsgID_PlayersList) then
    Exit;

  if not (RequestingGame or DemoPlayer.Active) then
    Exit;

  RequestingGame := False;

  PlayersListMsg := PMsg_PlayersList(NetMessage^.m_pData)^;

  if not DemoPlayer.Active then
    MainConsole.Console(_('Connection accepted to') + ' ' + WideString(UDP.GetStringAddress(@UDP.Address, True)),
      CLIENT_MESSAGE_COLOR);

  ServerIP := UDP.GetStringAddress(@UDP.Address, False);

  MapName := AnsiReplaceStr(Trim(PlayersListMsg.MapName), '..', '');
  ModName := AnsiReplaceStr(Trim(PlayersListMsg.ModName), '..', '');

  if sv_downloadurl.Value <> '' then
    DownloadURL := IncludeTrailingPathDelimiter(sv_downloadurl.Value)
  else
    DownloadURL := 'http://' + ServerIP + ':' + IntToStr(ServerPort + 10) + '/';

  if cl_servermods.Value then
    if ModName <> '' then
    begin
      if FileExists(UserDirectory + 'mods/' + ModName + '.smod') then
        Checksum := Sha1File(UserDirectory + 'mods/' + ModName + '.smod', 4096)
      else
        Checksum := Default(TSHA1Digest);

      if Sha1Match(TSHA1Digest(PlayersListMsg.ModChecksum), Checksum) then
      begin
        if not PhysFS_mount(PChar(UserDirectory + 'mods/' + ModName + '.smod'),
          PChar('mods/' + ModName + '/'), False) then
        begin
          ShowMessage(_('Could not load mod archive') + WideString(' (' + ModName + ').'));
          Exit;
        end;
        ModDir := 'mods/' + ModName + '/';
        CustomModChecksum := Checksum;
        // TODO: Replace with LoadMod
        LoadWeaponNames();
        InitGameMenus();
        LoadAnimObjects(ModDir);
        LoadSounds(ModDir);
        ForceGraphicsReload := True;
        UsesServerMod := True;
        MainConsole.Console(_('Loading server mod:') + WideString(' ' + ModName), MODE_MESSAGE_COLOR);
      end
      else
      begin
        DownloadThread := TDownloadThread.Create(DownloadURL + 'mods/' + ModName + '.smod',
          UserDirectory + 'mods/' + ModName + '.smod', TSHA1Digest(PlayersListMsg.ModChecksum));
        Exit;
      end;
    end else
    begin
      if UsesServerMod then // reset to original mod
      begin
        ModDir := fs_mod.Value;
        LoadWeaponNames();
        LoadAnimObjects(ModDir);
        LoadSounds(ModDir);
        ForceGraphicsReload := True;
        UsesServerMod := False;
      end;
    end;

  // Initialize Map

  if GetMapInfo(MapName, UserDirectory, MapStatus) {and VerifyMapChecksum(MapStatus, PlayersListMsg.MapChecksum)} then
  begin
    if not Map.LoadMap(MapStatus, r_forcebg.Value, r_forcebg_color1.Value, r_forcebg_color2.Value) then
    begin
      RenderGameInfo(_('Could not load map:') + ' ' + WideString(MapName));
      ExitToMenu;
      Exit;
    end;
  end
  else
  begin
    {$IFDEF STEAM}
    if MapStatus.WorkshopID > 0 then
    begin
      RenderGameInfo(_('Downloading workshop item:') + ' ' + WideString(IntToStr(MapStatus.WorkshopID)));
      SteamAPI.UGC.DownloadItem(MapStatus.WorkshopID, True);
      MapChangeItemID := MapStatus.WorkshopID;
      ForceReconnect := True;
      Exit;
    end else
    {$ENDIF}
    begin
      DownloadThread := TDownloadThread.Create(DownloadURL + 'maps/' + MapName + '.smap',
        UserDirectory + 'maps/' + MapName + '.smap', PlayersListMsg.MapChecksum);
      Exit;
    end;
  end;

  DownloadRetry := 0;

  if ForceGraphicsReload then
  begin
    ReloadGraphics;
    ForceGraphicsReload := False;
  end;

  // Sync cvars
  PlayersNum := PlayersListMsg.Players;
  TimeLimitCounter := PlayersListMsg.CurrentTime;

  b.x := 0;
  b.y := 0;
  for i := 1 to MAX_SPRITES do
    if PlayersListMsg.Name[i] <> '0 ' then
    begin
      NewPlayer := Sprite[i].Player; // reuse object
      NewPlayer.Name := ReturnFixedPlayerName(PlayersListMsg.Name[i]);
      NewPlayer.ShirtColor := PlayersListMsg.ShirtColor[i] or $FF000000;
      NewPlayer.PantsColor := PlayersListMsg.PantsColor[i] or $FF000000;
      NewPlayer.SkinColor := PlayersListMsg.SkinColor[i] or $FF000000;
      NewPlayer.HairColor := PlayersListMsg.HairColor[i] or $FF000000;
      NewPlayer.JetColor := PlayersListMsg.JetColor[i];
      NewPlayer.Team := PlayersListMsg.Team[i];

      {$IFDEF STEAM}
      NewPlayer.SteamID := CSteamID(PlayersListMsg.SteamID[i]);
      {$ENDIF}

      NewPlayer.SecWep := 0;
      Pos := PlayersListMsg.Pos[i];
      Vel := PlayersListMsg.Vel[i];

      NewPlayer.HairStyle := 0;
      if PlayersListMsg.Look[i] and B1 = B1 then NewPlayer.HairStyle := 1;
      if PlayersListMsg.Look[i] and B2 = B2 then NewPlayer.HairStyle := 2;
      if PlayersListMsg.Look[i] and B3 = B3 then NewPlayer.HairStyle := 3;
      if PlayersListMsg.Look[i] and B4 = B4 then NewPlayer.HairStyle := 4;

      NewPlayer.HeadCap := 0;
      if PlayersListMsg.Look[i] and B5 = B5 then NewPlayer.HeadCap := GFX_GOSTEK_HELM;
      if PlayersListMsg.Look[i] and B6 = B6 then NewPlayer.HeadCap := GFX_GOSTEK_KAP;

      NewPlayer.Chain := 0;
      if PlayersListMsg.Look[i] and B7 = B7 then NewPlayer.Chain := 1;
      if PlayersListMsg.Look[i] and B8 = B8 then NewPlayer.Chain := 2;

      CreateSprite(Pos, b, 1, i, NewPlayer, False);

      SpriteParts.Velocity[Sprite[i].Num] := Vel;

      Sprite[i].CeaseFireCounter := 0;
      if (PlayersListMsg.PredDuration[i] > 0) then
      begin
        Sprite[i].Alpha := PREDATORALPHA;
        Sprite[i].BonusTime := PlayersListMsg.PredDuration[i] * 60;
        Sprite[i].BonusStyle := BONUS_PREDATOR;
      end;
    end;

  if not DemoPlayer.Active then
    RenderGameInfo(_('Waiting to join game...'));

  MySprite := 0;
  CameraFollowSprite := 0;
  GameThingTarget := 0;
  SelTeam := 0;
  MenuTimer := 0;
  SurvivalEndRound := False;
  CameraX := 0;
  CameraY := 0;

  if not DemoPlayer.Active then
  begin
    GoalTicks := DEFAULT_GOALTICKS;
    NoTexts := 0;
  end;

  ClientVarsRecieved := False;
  MainTickCounter := 0;
  ClientTickCount := PlayersListMsg.ServerTicks;
  NoHeartbeatTime := 0;
  MapChangeCounter := -60;
  GameMenuShow(EscMenu, False);
  LimboLock := False;

  if VoteActive then
    StopVote;

  ResetWeaponStats;

  ClientPlayerReceived := False;
  ClientPlayerSent := False;
  ClientPlayerReceivedCounter := CLIENTPLAYERRECIEVED_TIME;

  // Begin rendering so that the team menu selection is visible
  if not (DemoPlayer.Active and (DemoPlayer.SkipTo = -1)) then
    ShouldRenderFrames := True;

  if cl_player_team.Value > 0 then
    begin
      // Bypass Team Select Menu if team cvar is set
      SelTeam := cl_player_team.Value;
      ClientSendPlayerInfo;
    end
  else if Spectator = 1 then
    ClientSendPlayerInfo
  else
  begin
    if (sv_gamemode.Value = GAMESTYLE_DEATHMATCH) or
       (sv_gamemode.Value = GAMESTYLE_POINTMATCH) or
       (sv_gamemode.Value = GAMESTYLE_RAMBO) then
      ClientSendPlayerInfo;

    if sv_gamemode.Value = GAMESTYLE_TEAMMATCH then
      GameMenuShow(TeamMenu);

    if (sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF) or
       (sv_gamemode.Value = GAMESTYLE_HTF) then
    begin
      GameMenuShow(TeamMenu);
      if sv_balanceteams.Value then
      begin
        SelTeam := 0;
        if PlayersTeamNum[1] < PlayersTeamNum[2] then
          SelTeam := 1;
        if PlayersTeamNum[2] < PlayersTeamNum[1] then
          SelTeam := 2;
        if SelTeam > 0 then
          ClientSendPlayerInfo;
      end;
    end;
  end;

  if ui_sniperline.Value and not sv_sniperline.Value then
    MainConsole.Console(_('Sniper Line disabled on this server'), WARNING_MESSAGE_COLOR);

  StartHealth := DEFAULT_HEALTH;
  if sv_realisticmode.Value then
  begin
    StartHealth := REALISTIC_HEALTH;
    MainConsole.Console(_('Realistic Mode ON'), MODE_MESSAGE_COLOR);
  end;
  if sv_survivalmode.Value then
    MainConsole.Console(_('Survival Mode ON'), MODE_MESSAGE_COLOR);
  if sv_advancemode.Value then
    MainConsole.Console(_('Advance Mode ON'), MODE_MESSAGE_COLOR);

  // newby stuff
  if cl_runs.Value < 3 then
  begin
    if Random(3) = 0 then
      MainConsole.Console(_('(!) Jet fuel is map specific. There can be more or less on certain maps.'), INFO_MESSAGE_COLOR)
    else
    begin
      MainConsole.Console(_('(!) To leave your current weapon after respawn'), INFO_MESSAGE_COLOR);
      MainConsole.Console('    ' + _('click anywhere outside the weapons menu.'), INFO_MESSAGE_COLOR);
    end;

    if sv_realisticmode.Value then
      MainConsole.Console(_('(!) To prevent weapon recoil fire single shots or short bursts.'), INFO_MESSAGE_COLOR);
  end;

  mx := GameWidthHalf;
  my := GameHeightHalf;
  MousePrev.x := mx;
  MousePrev.y := my;
  WindowReady := true;
end;

procedure ClientHandleUnAccepted(NetMessage: PSteamNetworkingMessage_t);
var
  UnAcceptedMsg: PMsg_UnAccepted;
  Text: String;
  TextLen: Integer;
begin
  if not VerifyPacketLargerOrEqual(sizeof(UnAcceptedMsg), NetMessage^.m_cbSize, MsgID_UnAccepted) then
    Exit;

  UnAcceptedMsg := PMsg_UnAccepted(NetMessage^.m_pData);
  TextLen := NetMessage^.m_cbSize - SizeOf(TMsg_UnAccepted);

  if (TextLen > 0) and (PChar(UnAcceptedMsg.Text)[TextLen-1] = #0) then
    Text := PChar(UnAcceptedMsg.Text)
  else
    Text := '';

  AddLineToLogFile(GameLog, '*UA ' + IntToStr(UnAcceptedMsg.State), ConsoleLogFileName);

  case UnAcceptedMsg.State of
    WRONG_VERSION:
      RenderGameInfo(_('Wrong game versions. Your version:') + ' ' + OPENSOLDAT_VERSION +
        ' ' + _('Server Version:') + ' ' + UnAcceptedMsg.Version);

    WRONG_PASSWORD:
      RenderGameInfo(_('Wrong server password'));

    BANNED_IP:
      RenderGameInfo(_('You have been banned on this server. Reason:') + ' ' + WideString(Text));

    SERVER_FULL:
      RenderGameInfo(_('Server is full'));

    INVALID_HANDSHAKE:
      RenderGameInfo(_('Unspecified internal protocol error'));

    WRONG_CHECKSUM:
      RenderGameInfo(_('This server requires a different smod file.'));

    STEAM_ONLY:
      RenderGameInfo(_('This server accepts only Steam players.'));

    ANTICHEAT_REJECTED:
      RenderGameInfo(_('Rejected by Anti-Cheat:') + ' ' + WideString(Text));
  end;

  ReceivedUnAccepted := True;
  ClientDisconnect;
  ExitToMenu;
end;

procedure ClientHandleServerDisconnect(NetMessage: PSteamNetworkingMessage_t);
begin
  if not VerifyPacket(sizeof(TMsg_ServerDisconnect), NetMessage^.m_cbSize, MsgID_ServerDisconnect) then
    Exit;

  ShowMapChangeScoreboard();

  if not DemoPlayer.Active then
    MainConsole.Console(_('Server disconnected'), SERVER_MESSAGE_COLOR)
  else
    DemoPlayer.StopDemo;
end;

procedure ClientHandlePing(NetMessage: PSteamNetworkingMessage_t);
begin
  if not VerifyPacket(sizeof(TMsg_Ping), NetMessage^.m_cbSize, MsgID_Ping) then
    Exit;

  if DemoPlayer.Active then
    Exit;

  if MySprite <> 0 then
  begin
    Sprite[MySprite].Player.PingTicks := PMsg_Ping(NetMessage^.m_pData)^.PingTicks;
    Sprite[MySprite].Player.PingTime :=
      Sprite[MySprite].Player.PingTicks * 1000 div 60;
  end;

  ClientPong(PMsg_Ping(NetMessage^.m_pData)^.PingNum);

  ClientStopMovingCounter := CLIENTSTOPMOVE_RETRYS;
  NoHeartbeatTime := 0;
end;

procedure ClientHandleServerVars(NetMessage: PSteamNetworkingMessage_t);
var
  VarsMsg: TMsg_ServerVars;
  i: Integer;
  Gun: ^TGun;
  WeaponIndex: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerVars), NetMessage^.m_cbSize, MsgID_ServerVars) then
    Exit;

  VarsMsg := PMsg_ServerVars(NetMessage^.m_pData)^;

  ClientVarsRecieved := true;

  WeaponsInGame := 0;

  for i := 1 to MAIN_WEAPONS do
  begin
    WeaponActive[i] := VarsMsg.WeaponActive[i];
    LimboMenu.Button[i - 1].Active := Boolean(WeaponActive[i]);
    if WeaponActive[i] = 1 then
      Inc(WeaponsInGame);
  end;

  if MySprite > 0 then
  begin
    SelectDefaultWeapons(MySprite);
    NewPlayerWeapon;
  end;

  CreateDefaultWeapons(sv_realisticmode.Value);
  DefaultWMChecksum := CreateWMChecksum();

  for WeaponIndex := 1 to ORIGINAL_WEAPONS do
  begin
    Gun := @Guns[WeaponIndex];
    Gun.HitMultiply       := VarsMsg.Damage[WeaponIndex];
    Gun.Ammo              := VarsMsg.Ammo[WeaponIndex];
    Gun.ReloadTime        := VarsMsg.ReloadTime[WeaponIndex];
    Gun.Speed             := VarsMsg.Speed[WeaponIndex];
    Gun.BulletStyle       := VarsMsg.BulletStyle[WeaponIndex];
    Gun.StartUpTime       := VarsMsg.StartUpTime[WeaponIndex];
    Gun.Bink              := VarsMsg.Bink[WeaponIndex];
    Gun.FireInterval      := VarsMsg.FireInterval[WeaponIndex];
    Gun.MovementAcc       := VarsMsg.MovementAcc[WeaponIndex];
    Gun.BulletSpread      := VarsMsg.BulletSpread[WeaponIndex];
    Gun.Recoil            := VarsMsg.Recoil[WeaponIndex];
    Gun.Push              := VarsMsg.Push[WeaponIndex];
    Gun.InheritedVelocity := VarsMsg.InheritedVelocity[WeaponIndex];
    Gun.ModifierHead      := VarsMsg.ModifierHead[WeaponIndex];
    Gun.ModifierChest     := VarsMsg.ModifierChest[WeaponIndex];
    Gun.ModifierLegs      := VarsMsg.ModifierLegs[WeaponIndex];
    Gun.NoCollision       := VarsMsg.NoCollision[WeaponIndex];
  end;

  BuildWeapons();

  if MySprite > 0 then
  begin
    Sprite[MySprite].ApplyWeaponByNum(Sprite[MySprite].Weapon.Num, 1);
    Sprite[MySprite].ApplyWeaponByNum(Sprite[MySprite].SecondaryWeapon.Num, 2);
    if not Sprite[MySprite].DeadMeat then
      ClientSpriteSnapshot;
  end;

  LoadedWMChecksum := CreateWMChecksum();

  if LoadedWMChecksum <> DefaultWMChecksum then
    if not DemoPlayer.Active then
      MainConsole.Console(_('Server uses weapon mod (checksum') + ' ' +
        WideString(IntToStr(LoadedWMChecksum)) + ')', SERVER_MESSAGE_COLOR)
end;

procedure ClientHandleSyncCvars(NetMessage: PSteamNetworkingMessage_t);
var
  VarsMsg: TMsg_ServerSyncCvars;
  i, CvarID: Byte;
  PacketStreamReader: TBitReader;
  Size: Integer;
begin
  if not VerifyPacketLargerOrEqual(sizeof(TMsg_ServerSyncCvars), NetMessage^.m_cbSize, MsgID_SyncCvars) then
    Exit;

  VarsMsg := PMsg_ServerSyncCvars(NetMessage^.m_pData)^;
  Size := NetMessage^.m_cbSize - SizeOf(VarsMsg.Header) + SizeOf(VarsMsg.ItemCount);
  PacketStreamReader := TBitReader.Create(@PMsg_ServerSyncCvars(NetMessage^.m_pData)^.Data, Size);

  for i := 1 to VarsMsg.ItemCount do
  begin
    if PacketStreamReader.BitPos >= (Size * 8) then
      Continue;

    CvarID := PacketStreamReader.ReadUInt8;

    if CvarID > CvarsSync.Count then
      Continue;

    if TObject(CvarsSync[CvarID]) is TIntegerCvar then
      TIntegerCvar(CvarsSync.Items[CvarID]).SetValue(PacketStreamReader.ReadInt32)
    else if TObject(CvarsSync[CvarID]) is TSingleCvar then
      TSingleCvar(CvarsSync.Items[CvarID]).SetValue(PacketStreamReader.ReadSingle)
    else if TObject(CvarsSync[CvarID]) is TBooleanCvar then
      TBooleanCvar(CvarsSync.Items[CvarID]).SetValue(PacketStreamReader.ReadBoolean)
    else if TObject(CvarsSync[CvarID]) is TStringCvar then
      TStringCvar(CvarsSync.Items[CvarID]).SetValue(PacketStreamReader.ReadString);
  end;
end;

end.

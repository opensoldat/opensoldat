unit NetworkClientGame;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // helper units
  Vector,

  // OpenSoldat units
  Steam, Net, Sprites, Weapons, Sound,
  Constants, GameStrings;

procedure ClientHandleNewPlayer(NetMessage: PSteamNetworkingMessage_t);
procedure ClientVoteKick(Num: Byte; Ban: Boolean; Reason: string);
procedure ClientVoteMap(MapID: Word);
procedure ClientHandleVoteResponse(NetMessage: PSteamNetworkingMessage_t);
procedure ClientFreeCamTarget;
procedure ClientHandlePlayerDisconnect(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleMapChange(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleFlagInfo(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleIdleAnimation(NetMessage: PSteamNetworkingMessage_t);
{$IFDEF STEAM}
procedure ClientSendVoiceData(Data: Pointer; DataSize: Word);
procedure ClientHandleVoiceData(NetMessage: PSteamNetworkingMessage_t);
{$ENDIF}

implementation

uses
  Client, NetworkUtils, Game, Demo, ClientGame, Sparks, GameMenus,
  InterfaceGraphics;

procedure ClientHandleNewPlayer(NetMessage: PSteamNetworkingMessage_t);
var
   NewPlayerMsg: TMsg_NewPlayer;
   Player: TPlayer;
   a, b: TVector2;
   i, d: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_NewPlayer), NetMessage^.m_cbSize, MsgID_NewPlayer) then
    Exit;

  NewPlayerMsg := PMsg_NewPlayer(NetMessage^.m_pData)^;
  i := NewPlayerMsg.Num;
  if (i < 1) or (i > MAX_SPRITES) then
    Exit;

  b.x := 0;
  b.y := 0;
  Player := Sprite[i].Player; // reuse object
  Player.Name := ReturnFixedPlayerName(NewPlayerMsg.Name);
  Player.ShirtColor := NewPlayerMsg.ShirtColor and $00FFFFFF;
  Player.PantsColor := NewPlayerMsg.PantsColor and $00FFFFFF;
  Player.SkinColor := NewPlayerMsg.SkinColor and $00FFFFFF;
  Player.HairColor := NewPlayerMsg.HairColor and $00FFFFFF;
  Player.JetColor := NewPlayerMsg.JetColor;
  Player.Team := NewPlayerMsg.Team;

  {$IFDEF STEAM}
  Player.SteamID := CSteamID(NewPlayerMsg.SteamID);
  Player.SteamFriend := SteamAPI.Friends.HasFriend(Player.SteamID, Ord(k_EFriendFlagImmediate));
  {$ENDIF}

  Player.ControlMethod := HUMAN;

  Player.HairStyle := 0;
  if NewPlayerMsg.Look and B1 = B1 then
    Player.HairStyle := 1;
  if NewPlayerMsg.Look and B2 = B2 then
    Player.HairStyle := 2;
  if NewPlayerMsg.Look and B3 = B3 then
    Player.HairStyle := 3;
  if NewPlayerMsg.Look and B4 = B4 then
    Player.HairStyle := 4;

  Player.HeadCap := 0;
  if NewPlayerMsg.Look and B5 = B5 then
    Player.HeadCap := GFX_GOSTEK_HELM;
  if NewPlayerMsg.Look and B6 = B6 then
    Player.HeadCap := GFX_GOSTEK_KAP;

  Player.Chain := 0;
  if NewPlayerMsg.Look and B7 = B7 then
    Player.Chain := 1;
  if NewPlayerMsg.Look and B8 = B8 then
    Player.Chain := 2;

  a := NewPlayerMsg.Pos;
  i := CreateSprite(a, b, 1, i, Player, False);
  d := 0;

  // The NewPlayer message doubles as confirmation that a player object was
  // allocated for the client. So far the following happened:
  // Client                        Server
  // Msg_RequestGame -->           (ServerHandleRequestGame) "Requesting game..." console message
  // ...                       <-- ... (ServerSyncCvars)
  // (ClientHandlePlayersList) <-- Msg_PlayersList (ServerSendPlayList)
  // Msg_PlayerInfo -->            (ServerHandlePlayerInfo) "...joining game..." console message
  //                               CreateSprite() -- note: also called when switching teams!
  // You are here!             <-- Msg_NewPlayer with AdoptSpriteID=1 (ServerSendNewPlayerInfo)
  if (NewPlayerMsg.AdoptSpriteID = 1) then
  begin
    d := 1;
    MySprite := i;

    if DemoPlayer.Active then
      Sprite[MySprite].Player.DemoPlayer := True;

    // TODO wat?
    Sprite[MySprite].BulletCount := Random(High(Word));

    if Player.Team = TEAM_SPECTATOR then
    begin
      CameraFollowSprite := 0;
      CameraFollowSprite := GetCameraTarget();
      GameMenuShow(LimboMenu, False);
    end
    else
      CameraFollowSprite := MySprite;

    GameMenuShow(TeamMenu, False);
    ClientPlayerReceived := true;
    ClientPlayerReceivedCounter := -1;
    BadMapIDCount := 2;
    HeartbeatTime := MainTickCounter;
    HeartbeatTimeWarnings := 0;

    r_zoom.SetValue(0.0);  // Reset zoom

    if MapChangeCounter < 999999999 then
      MapChangeCounter := -60;
    FragsMenuShow := False;
    StatsMenuShow := False;
  end;

  SpriteParts.OldPos[i] := NewPlayerMsg.Pos;
  SpriteParts.Pos[i] := NewPlayerMsg.Pos;

  Sprite[i].Respawn;

  if d = 1 then
  begin
    Sprite[i].Weapon := Guns[NOWEAPON];
    Sprite[i].SecondaryWeapon := Guns[NOWEAPON];

    if MySprite > 0 then
    begin
      GameMenuShow(LimboMenu);
      NewPlayerWeapon;
    end;
  end;

  if NewPlayerMsg.JoinType <> JOIN_SILENT then
    case NewPlayerMsg.Team of
      TEAM_NONE: MainConsole.Console(WideFormat(_('%s has joined the game'),
          [Player.Name]), ENTER_MESSAGE_COLOR);
      TEAM_ALPHA: MainConsole.Console(WideFormat(_('%s has joined alpha team'),
          [Player.Name]), ALPHAJ_MESSAGE_COLOR);
      TEAM_BRAVO: MainConsole.Console(WideFormat(_('%s has joined bravo team'),
          [Player.Name]), BRAVOJ_MESSAGE_COLOR);
      TEAM_CHARLIE: MainConsole.Console(WideFormat(_('%s has joined charlie team'),
          [Player.Name]), CHARLIEJ_MESSAGE_COLOR);
      TEAM_DELTA: MainConsole.Console(WideFormat(_('%s has joined delta team'),
          [Player.Name]), DELTAJ_MESSAGE_COLOR);
      TEAM_SPECTATOR: MainConsole.Console(WideFormat(_('%s has joined as spectator'),
          [Player.Name]), DELTAJ_MESSAGE_COLOR);
    end;
end;

procedure ClientVoteKick(Num: Byte; Ban: Boolean; Reason: string);
var
  VoteMsg: TMsg_VoteKick;
begin
  VoteMsg.Header.ID := MsgID_VoteKick;
  VoteMsg.Ban := Byte(Ban);
  VoteMsg.Num := Num;
  StringToArray(VoteMsg.Reason, Reason);
  UDP.SendData(VoteMsg, sizeof(VoteMsg), k_nSteamNetworkingSend_Reliable);
end;

procedure ClientVoteMap(MapID: Word);
var
  VoteMsg: TMsg_VoteMap;
begin
  VoteMsg.Header.ID := MsgID_VoteMap;
  VoteMsg.MapID := MapID;
  UDP.SendData(VoteMsg, sizeof(VoteMsg), k_nSteamNetworkingSend_Reliable);
end;

procedure ClientHandleVoteResponse(NetMessage: PSteamNetworkingMessage_t);
var
  VoteMsgReply: TMsg_VoteMapReply;
begin
  if not VerifyPacket(sizeof(TMsg_VoteMapReply), NetMessage^.m_cbSize, MsgID_VoteMapReply) then
    Exit;

  VoteMsgReply := PMsg_VoteMapReply(NetMessage^.m_pData)^;
  VoteMapName := VoteMsgReply.MapName;
  VoteMapCount := VoteMsgReply.Count;
end;

procedure ClientFreeCamTarget;
var
  FreeCamMsg: TMsg_ClientFreeCam;
begin
  FreeCamMsg.Header.ID := MsgID_ClientFreeCam;
  FreeCamMsg.FreeCamOn := 0;
  FreeCamMsg.TargetPos.x := camerax;
  FreeCamMsg.TargetPos.y := cameray;

  UDP.SendData(FreeCamMsg, sizeof(FreeCamMsg), k_nSteamNetworkingSend_Reliable);
end;

procedure ClientHandlePlayerDisconnect(NetMessage: PSteamNetworkingMessage_t);
var
  PlayerMsg: TMsg_PlayerDisconnect;
begin
  if not VerifyPacket(sizeof(TMsg_PlayerDisconnect), NetMessage^.m_cbSize, MsgID_PlayerDisconnect) then
    Exit;

  PlayerMsg := PMsg_PlayerDisconnect(NetMessage^.m_pData)^;
  if (PlayerMsg.Num < 1) or (PlayerMsg.Num > MAX_SPRITES) then
    Exit;

  if (PlayerMsg.Why = KICK_UNKNOWN) or (PlayerMsg.Why >= _KICK_END) or
    (PlayerMsg.Why = KICK_LEFTGAME) then
    case Sprite[PlayerMsg.Num].Player.Team of
      0: MainConsole.Console(WideFormat(_('%s has left the game'),
          [Sprite[PlayerMsg.Num].Player.Name]), ENTER_MESSAGE_COLOR);
      1: MainConsole.Console(WideFormat(_('%s has left alpha team'),
          [Sprite[PlayerMsg.Num].Player.Name]), ALPHAJ_MESSAGE_COLOR);
      2: MainConsole.Console(WideFormat(_('%s has left bravo team'),
          [Sprite[PlayerMsg.Num].Player.Name]), BRAVOJ_MESSAGE_COLOR);
      3: MainConsole.Console(WideFormat(_('%s has left charlie team'),
          [Sprite[PlayerMsg.Num].Player.Name]), CHARLIEJ_MESSAGE_COLOR);
      4: MainConsole.Console(WideFormat(_('%s has left delta team'),
          [Sprite[PlayerMsg.Num].Player.Name]), DELTAJ_MESSAGE_COLOR);
      5: MainConsole.Console(WideFormat(_('%s has left spectators'),
          [Sprite[PlayerMsg.Num].Player.Name]), DELTAJ_MESSAGE_COLOR);
    end;

  case PlayerMsg.Why of
    KICK_NORESPONSE: MainConsole.Console(WideFormat(_('%s has disconnected'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_NOCHEATRESPONSE: MainConsole.Console(WideFormat(_('%s has been disconnected'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_CHANGETEAM: MainConsole.Console(WideFormat(_('%s is changing teams'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_PING: MainConsole.Console(WideFormat(_('%s has been ping kicked (for 15 minutes)'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_FLOODING: MainConsole.Console(WideFormat(_('%s has been flood kicked (for 5 minutes)'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_CONSOLE: MainConsole.Console(WideFormat(_('%s has been kicked from console'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_CONNECTCHEAT: MainConsole.Console(WideFormat(_('%s has been ''connect cheat'' kicked'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_CHEAT: MainConsole.Console(WideFormat(_('%s has been kicked for possible cheat'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_VOTED: MainConsole.Console(WideFormat(_('%s has been voted to leave the game'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_AC: MainConsole.Console(WideFormat(_('%s has been kicked for Anti-Cheat violation'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
    KICK_STEAMTICKET: MainConsole.Console(WideFormat(_('%s has been kicked for invalid Steam ticket'),
        [Sprite[PlayerMsg.Num].Player.Name]), CLIENT_MESSAGE_COLOR);
  end;
  if VoteActive then
    case PlayerMsg.Why of
      KICK_NORESPONSE: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_NOCHEATRESPONSE: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_PING: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_FLOODING: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_CONSOLE: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_CONNECTCHEAT: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_CHEAT: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_VOTED: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_AC: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
      KICK_SILENT: if (VoteTarget = IntToStr(PlayerMsg.Num)) then
          StopVote;
    end;

  if (PlayerMsg.Why <> KICK_CHANGETEAM) then
    Sprite[PlayerMsg.Num].Kill;

  SortPlayers;

  if (PlayerMsg.Num = MySprite) and (MapChangeCounter < 1) then
  begin
    ShowMapChangeScoreboard();
    GameMenuShow(TeamMenu, False);
  end;

  if (PlayerMsg.Why <> KICK_CHANGETEAM) and
    (PlayerMsg.Why <> KICK_LEFTGAME) then
  begin
    FragsMenuShow := False;
    StatsMenuShow := False;
  end;
end;

procedure ClientHandleMapChange(NetMessage: PSteamNetworkingMessage_t);
var
  MapChange: TMsg_MapChange;
  I: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_MapChange), NetMessage^.m_cbSize, MsgID_MapChange) then
    Exit;

  MapChange := PMsg_MapChange(NetMessage^.m_pData)^;

  MapChangeName := MapChange.MapName;
  MapChangeCounter := MapChange.Counter;
  MapChangeChecksum := MapChange.MapChecksum;
  FragsMenuShow := True;
  StatsMenuShow := False;
  GameMenuShow(LimboMenu, False);
  HeartbeatTime := MainTickCounter;
  HeartbeatTimeWarnings := 0;

  if cl_endscreenshot.Value then
    ScreenTaken := True;

  for I := 1 to MAX_PLAYERS do
    if Sprite[I].Active then
    begin
      StopSound(Sprite[I].ReloadSoundChannel);
      StopSound(Sprite[I].JetsSoundChannel);
      StopSound(Sprite[I].GattlingSoundChannel);
      StopSound(Sprite[I].GattlingSoundChannel2);
    end;

  if DemoPlayer.Active then
  begin
    ShowMapChangeScoreboard();

    DemoPlayer.StopDemo;
    Exit;
  end;

  MainConsole.Console(_('Next map:') + ' ' + WideString(MapChangeName),
    GAME_MESSAGE_COLOR);

  if not sv_survivalmode.Value then
    if (sv_gamemode.Value = GAMESTYLE_DEATHMATCH) or
      (sv_gamemode.Value = GAMESTYLE_POINTMATCH) or
      (sv_gamemode.Value = GAMESTYLE_RAMBO) then
    begin
      if SortedPlayers[1].PlayerNum > 0 then
        CameraFollowSprite := SortedPlayers[1].PlayerNum;
      if not EscMenu.Active then
      begin
        mx := GameWidthHalf;
        my := GameHeightHalf;
      end;
    end;
end;

procedure ClientHandleFlagInfo(NetMessage: PSteamNetworkingMessage_t);
var
  j: Integer;
  a, b: TVector2;
begin
  if not VerifyPacket(sizeof(TMsg_ServerFlagInfo), NetMessage^.m_cbSize, MsgID_FlagInfo) then
    Exit;

  a := Default(TVector2);
  b := Default(TVector2);

  if (PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who < 1) or
    (PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who > MAX_SPRITES) then
    Exit;

  if PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Style = RETURNRED then
    if sv_gamemode.Value = GAMESTYLE_CTF then
    begin
      PlaySound(SFX_CAPTURE);
      BigMessage(_('Red Flag returned!'), CAPTUREMESSAGEWAIT,
        ALPHA_MESSAGE_COLOR);

      MainConsole.Console(WideFormat(_('%s returned the Red Flag'),
        [Sprite[PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who].Player.Name]),
        ALPHA_MESSAGE_COLOR);
      if TeamFlag[1] > 0 then
        Thing[TeamFlag[1]].Respawn;
    end;
  if PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Style = RETURNBLUE then
    if sv_gamemode.Value = GAMESTYLE_CTF then
    begin
      PlaySound(SFX_CAPTURE);
      BigMessage(_('Blue Flag returned!'), CAPTUREMESSAGEWAIT,
        ALPHA_MESSAGE_COLOR);

      MainConsole.Console(WideFormat(_('%s returned the Blue Flag'),
        [Sprite[PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who].Player.Name]),
        BRAVO_MESSAGE_COLOR);
      if TeamFlag[2] > 0 then
        Thing[TeamFlag[2]].Respawn;
    end;
  if PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Style = CAPTURERED then
  begin
    BigMessage(_('Alpha Team Scores!'), CAPTURECTFMESSAGEWAIT,
      ALPHA_MESSAGE_COLOR);
    MainConsole.Console(WideFormat(_('%s scores for Alpha Team'),
      [Sprite[PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who].Player.Name]),
      ALPHA_MESSAGE_COLOR);

    if sv_gamemode.Value = GAMESTYLE_INF then
    begin
      PlaySound(SFX_INFILTMUS);

      // flame it
      for j := 1 to 10 do
      begin
        a.x := Thing[TeamFlag[1]].Skeleton.Pos[2].x - 10 + Random(20);
        a.y := Thing[TeamFlag[1]].Skeleton.Pos[2].y - 10 + Random(20);
        b.x := 0;
        b.y := 0;
        CreateSpark(a, b, 36, 0, 35);
        if Random(2) = 0 then
          CreateSpark(a, b, 37, 0, 75);
      end;
    end
    else
      PlaySound(SFX_CTF);
    if TeamFlag[2] > 0 then
      Thing[TeamFlag[2]].Respawn;

    // cap spark
    CreateSpark(Thing[TeamFlag[1]].Skeleton.Pos[2], b, 61,
      PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who, 18);

    if sv_survivalmode.Value then
      SurvivalEndRound := true;
  end;
  if PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Style = CAPTUREBLUE then
  begin
    BigMessage(_('Bravo Team Scores!'), CAPTURECTFMESSAGEWAIT,
      BRAVO_MESSAGE_COLOR);
    MainConsole.Console(WideFormat(_('%s scores for Bravo Team'),
      [Sprite[PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who].Player.Name]),
      BRAVO_MESSAGE_COLOR);
    PlaySound(SFX_CTF);
    if TeamFlag[1] > 0 then
      Thing[TeamFlag[1]].Respawn;

    // cap spark
    CreateSpark(Thing[TeamFlag[2]].Skeleton.Pos[2], b, 61,
      PMsg_ServerFlagInfo(NetMessage^.m_pData)^.Who, 18);

    if sv_survivalmode.Value then
      SurvivalEndRound := true;
  end;
end;

procedure ClientHandleIdleAnimation(NetMessage: PSteamNetworkingMessage_t);
var
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_IdleAnimation), NetMessage^.m_cbSize, MsgID_IdleAnimation) then
    Exit;

  i := PMsg_IdleAnimation(NetMessage^.m_pData)^.Num;

  if not Sprite[i].Active then
    Exit;

  Sprite[i].IdleTime := 1;
  Sprite[i].IdleRandom := PMsg_IdleAnimation(NetMessage^.m_pData)^.IdleRandom;
end;

{$IFDEF STEAM}
procedure ClientSendVoiceData(Data: Pointer; DataSize: Word);
var
  VoiceMsg: PMsg_VoiceData;
  Size: Integer;
  SendBuffer: array of Byte = Nil;
begin
  Size := SizeOf(TMsg_VoiceData) + DataSize;
  SetLength(SendBuffer, Size);

  VoiceMsg := PMsg_VoiceData(SendBuffer);
  VoiceMsg.Header.ID := MsgID_VoiceData;

  Move(Data^, VoiceMsg.Data, DataSize);

  Sprite[MySprite].Player.LastReceiveVoiceTime := MainTickCounter + 10;

  UDP.SendData(VoiceMsg^, Size, k_nSteamNetworkingSend_NoDelay);
end;

procedure ClientHandleVoiceData(NetMessage: PSteamNetworkingMessage_t);
var
  VoiceMsg: PMsg_VoiceData;
  TextLen: Word;
  VoiceResult: EVoiceResult;
  AudioData: array of Byte = Nil;
  AudioLength: Cardinal;
begin
  if not cl_voicechat.Value then
    Exit;

  if not VerifyPacketLargerOrEqual(sizeof(VoiceMsg), NetMessage^.m_cbSize, MsgID_VoiceData) then
    Exit;

  VoiceMsg := PMsg_VoiceData(NetMessage^.m_pData);

  if (VoiceMsg.Speaker > 0) and (VoiceMsg.Speaker < MAX_PLAYERS) then
  begin
    if not Sprite[VoiceMsg.Speaker].Active then
      Exit;

    if Sprite[VoiceMsg.Speaker].Player.Muted = 1 then
      Exit;

    SetLength(AudioData, 20480);

    TextLen := NetMessage^.m_cbSize - SizeOf(TMsg_VoiceData);
    VoiceResult := SteamAPI.User.DecompressVoice(@VoiceMsg^.Data, TextLen, AudioData, Length(AudioData), @AudioLength, 44100);

    if VoiceResult = k_EVoiceResultBufferTooSmall then
    begin
      SetLength(AudioData, AudioLength);
      VoiceResult := SteamAPI.User.DecompressVoice(@VoiceMsg^.Data, TextLen, AudioData, Length(AudioData), @AudioLength, 44100);
    end;

    if (VoiceResult = k_EVoiceResultOK) and (AudioLength > 0) then
    begin
      Sprite[VoiceMsg.Speaker].Player.LastReceiveVoiceTime := MainTickCounter + 30;
      PlayVoiceData(@AudioData[0], AudioLength, VoiceMsg.Speaker);
    end;
  end;
end;
{$ENDIF}

end.

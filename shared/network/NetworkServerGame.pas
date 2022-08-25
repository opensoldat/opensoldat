unit NetworkServerGame;

interface

uses
  // delphi and system units
  SysUtils, Classes,
  {$IFDEF SCRIPT}
  // Script
  ScriptDispatcher,
  {$ENDIF}

  // OpenSoldat units
  Steam, Net, Sprites, Constants;

procedure ServerHandlePlayerDisconnect(NetMessage: PSteamNetworkingMessage_t);
procedure ServerMapChange(ID: Byte);
procedure ServerFlagInfo(Style, Who: Byte);
procedure ServerIdleAnimation(Num: Byte; Style: SmallInt);
procedure ServerSendVoteOn(VoteStyle: Byte; Voter: Integer;
  TargetName: string; Reason: string);
procedure ServerSendVoteOff;
procedure ServerHandleVoteKick(NetMessage: PSteamNetworkingMessage_t);
procedure ServerHandleVoteMap(NetMessage: PSteamNetworkingMessage_t);
procedure ServerHandleChangeTeam(NetMessage: PSteamNetworkingMessage_t);
procedure ServerSyncMsg(ToNum: Integer = 0);
{$IFDEF STEAM}
procedure ServerHandleVoiceData(NetMessage: PSteamNetworkingMessage_t);
{$ENDIF}

implementation

uses
  Server, Game, Util, ServerHelper,
  NetworkUtils, NetworkServerConnection, NetworkServerMessages;

procedure ServerHandlePlayerDisconnect(NetMessage: PSteamNetworkingMessage_t);
var
  PlayerMsg: TMsg_PlayerDisconnect;
  Player: TPlayer;
begin
  if not VerifyPacket(sizeof(TMsg_PlayerDisconnect), NetMessage^.m_cbSize, MsgID_PlayerDisconnect) then
    Exit;

  PlayerMsg := PMsg_PlayerDisconnect(NetMessage^.m_pData)^;
  Player := TPlayer(NetMessage^.m_nConnUserData);
  if PlayerMsg.Num <> Player.SpriteNum then
    Exit;

  ServerPlayerDisconnect(Player, KICK_LEFTGAME, True);
end;

procedure ServerMapChange(ID: Byte);
var
  MapChangeMsg: TMsg_MapChange;
  i: Integer;
  DstPlayer: TPlayer;
begin
  MapChangeMsg.Header.ID := MsgID_MapChange;
  MapChangeMsg.Counter := MapChangeCounter;
  MapChangeMsg.MapName := MapChange.Name;
  MapCheckSum := GetMapChecksum(MapChange);
  MapChangeMsg.MapChecksum := MapCheckSum;

  for i := 1 to MAX_PLAYERS do
    if Sprite[i].Active then
    begin
      Sprite[i].Player.TKWarnings := 0;
      TKList[i] := '';
      TKListKills[i] := 0;
    end;

  if ID = 0 then
  begin
    // NOTE we send to pending players too, otherwise there is a small window where they miss the map change
    // NOTE also that we're using the CONNECTION channel, which is required for all packets that can be sent
    // before a sprite is assigned to the player for proper sequencing with encryption commands.
    for DstPlayer in Players do
      UDP.SendData(MapChangeMsg, sizeof(MapChangeMsg), DstPlayer.peer, k_nSteamNetworkingSend_Reliable);
    end else
      if (Sprite[ID].Active) and (Sprite[ID].Player.ControlMethod = HUMAN) then
        UDP.SendData(MapChangeMsg, sizeof(MapChangeMsg), Sprite[ID].Player.Peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerFlagInfo(Style, Who: Byte);
var
  FlagMsg: TMsg_ServerFlagInfo;
  i: Integer;
begin
  FlagMsg.Header.ID := MsgID_FlagInfo;
  FlagMsg.Style := Style;
  FlagMsg.Who := Who;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(FlagMsg, sizeof(FlagMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerIdleAnimation(Num: Byte; Style: SmallInt);
var
  IdleMsg: TMsg_IdleAnimation;
  i: Integer;
begin
  IdleMsg.Header.ID := MsgID_IdleAnimation;
  IdleMsg.Num := Num;
  IdleMsg.IdleRandom := Style;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(IdleMsg, sizeof(IdleMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerSendVoteOn(VoteStyle: Byte; Voter: Integer;
  TargetName: string; Reason: string);
var
  VoteMsg: TMsg_VoteOn;
  i: Integer;
begin
  VoteMsg.Header.ID := MsgID_VoteOn;
  VoteMsg.VoteType := VoteStyle;
  VoteMsg.Timer := VoteTimeRemaining;
  VoteMsg.Who := Voter;
  StringToArray(VoteMsg.TargetName, TargetName);
  StringToArray(VoteMsg.Reason, Reason);

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(VoteMsg, sizeof(VoteMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerSendVoteOff;
var
  VoteMsg: TMsg_VoteOff;
  i: Integer;
begin
  VoteMsg.Header.ID := MsgID_VoteOff;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(VoteMsg, sizeof(VoteMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerHandleVoteKick(NetMessage: PSteamNetworkingMessage_t);
var
  VoteKickMsg: TMsg_VoteKick;
  Player: TPlayer;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_VoteKick), NetMessage^.m_cbSize, MsgID_votekick) then
    Exit;

  VoteKickMsg := PMsg_VoteKick(NetMessage^.m_pData)^;
  Player := TPlayer(NetMessage^.m_nConnUserData);
  i := Player.SpriteNum;

  if VoteActive then
  begin
    // if a vote against a player is in progress,
    // don't allow that player to vote against himself.
    if VoteType <> VOTE_KICK then
      Exit;
    if StrToInt(VoteTarget) = i then
    begin
      ServerSendStringMessage('A vote has been cast against you. You can not vote.', i, 255, MSGTYPE_PUB);
      Exit;
    end;

    // check if he already voted
    if VoteHasVoted[i] then
      Exit;

    // check if the vote target is actually the target
    if VoteTarget <> IntToStr(VoteKickMsg.Num) then
      Exit;

    {$IFDEF SCRIPT}
    ScrptDispatcher.OnVoteKick(i, VoteKickMsg.Num);
    {$ENDIF}
    CountVote(i);
  end
  else
  begin
    if VoteCooldown[i] < 0 then
    begin
      // only allow valid votes
      if (VoteKickMsg.Num < 1) or (VoteKickMsg.Num > MAX_PLAYERS) then
        Exit;
      if (Sprite[i].Player.Muted = 1) then
      begin
        WriteConsole(i, 'You are muted. You can''t cast a vote kick.',
          SERVER_MESSAGE_COLOR);
        Exit;
      end;

      {$IFDEF SCRIPT}
      if ScrptDispatcher.OnVoteKickStart(i, VoteKickMsg.Num, string(VoteKickMsg.Reason)) then
        Exit;
      {$ENDIF}

      StartVote(i, VOTE_KICK, IntToStr(VoteKickMsg.Num), string(VoteKickMsg.Reason));
      ServerSendVoteOn(VoteType, i, IntToStr(VoteKickMsg.Num), string(VoteKickMsg.Reason));
      // Show started votekick in admin console
      MainConsole.Console(Sprite[i].Player.Name + ' started votekick against ' +
        Sprite[VoteKickMsg.Num].Player.Name + ' - Reason:' +
        string(VoteKickMsg.Reason), VOTE_MESSAGE_COLOR);
    end;
  end;
end;

procedure ServerHandleVoteMap(NetMessage: PSteamNetworkingMessage_t);
var
  VoteMapMsg: TMsg_VoteMap;
  VoteMapReplyMsg: TMsg_VoteMapReply;
  Player: TPlayer;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_VoteMap), NetMessage^.m_cbSize, MsgID_VoteMap) then
    Exit;

  VoteMapMsg := PMsg_VoteMap(NetMessage^.m_pData)^;
  Player := TPlayer(NetMessage^.m_nConnUserData);
  i := Player.SpriteNum;

  if VoteMapMsg.MapID > MapsList.Count - 1 then
    Exit;

  VoteMapReplyMsg.Header.ID := MsgID_VoteMapReply;
  VoteMapReplyMsg.Count := MapsList.Count;
  VoteMapReplyMsg.MapName := MapsList[VoteMapMsg.MapID];

  UDP.SendData(VoteMapReplyMsg, sizeof(VoteMapReplyMsg), Sprite[I].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerHandleChangeTeam(NetMessage: PSteamNetworkingMessage_t);
var
  ChangeTeamMsg: TMsg_ChangeTeam;
  Player: TPlayer;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ChangeTeam), NetMessage^.m_cbSize, MsgID_ChangeTeam) then
    Exit;
  ChangeTeamMsg := PMsg_ChangeTeam(NetMessage^.m_pData)^;
  Player := TPlayer(NetMessage^.m_nConnUserData);
  i := Player.SpriteNum;
  Sprite[i].ChangeTeam(ChangeTeamMsg.Team);
end;

procedure ServerSyncMsg(ToNum: Integer = 0);
var
  SyncMsg: TMsg_ServerSyncMsg;
  i: Integer;
begin
  SyncMsg.Header.ID := MsgID_ServerSyncMsg;
  SyncMsg.Time := TimeLimitCounter;
  if MapChangeCounter = 999999999 then
    SyncMsg.Pause := 1
  else
    SyncMsg.Pause := 0;

  for i := 1 to MAX_PLAYERS do
    if (ToNum = 0) or (i = ToNum) then
      if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
        UDP.SendData(SyncMsg, sizeof(SyncMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

{$IFDEF STEAM}
procedure ServerHandleVoiceData(NetMessage: PSteamNetworkingMessage_t);
var
  VoiceMsg: PMsg_VoiceData;
  i: Byte;
  Player: TPlayer;
begin
  if not sv_voicechat.Value then
    Exit;

  if not VerifyPacketLargerOrEqual(sizeof(VoiceMsg), NetMessage^.m_cbSize, MsgID_VoiceData) then
    Exit;

  Player := TPlayer(NetMessage^.m_nConnUserData);

  if Player.Muted = 1 then
    Exit;

  VoiceMsg := PMsg_VoiceData(NetMessage^.m_pData);

  VoiceMsg.Speaker := Player.SpriteNum;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      if (Player.SpriteNum <> i) and (UInt64(Sprite[i].Player.SteamID) > 0) then
        if sv_voicechat_alltalk.Value or Sprite[Player.SpriteNum].IsInSameTeam(Sprite[i]) then
          UDP.SendData(VoiceMsg^, NetMessage^.m_cbSize, Sprite[i].Player.peer, k_nSteamNetworkingSend_NoDelay);
end;
{$ENDIF}

end.

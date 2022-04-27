unit NetworkClientFunctions;

interface

uses
  // delphi and system units
  SysUtils, Classes, sockets,

  // helper units
  Vector,

  // Sound unit
  Sound,

  // soldat units
  LogFile, Steam, Net, Sprites, Weapons, Constants;

procedure ClientHandleVoteOn(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleVoteOff;
procedure ClientHandleServerSyncMsg(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleForcePosition(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleForceVelocity(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleForceWeapon(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleWeaponActiveMessage(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleClientFreeCam(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleJoinServer(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandlePlaySound(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  Client, Game, Demo, GameMenus, NetworkClientConnection, PhysFS,
  InterfaceGraphics, NetworkUtils;

procedure ClientHandleVoteOn(NetMessage: PSteamNetworkingMessage_t);
var
  VoteOnMsg: TMsg_VoteOn;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_VoteOn), NetMessage^.m_cbSize, MsgID_VoteOn) then
    Exit;

  VoteOnMsg := PMsg_VoteOn(NetMessage^.m_pData)^;

  i := VoteOnMsg.Who;

  StatsMenuShow := false;

  StartVote(i, VoteOnMsg.VoteType, VoteOnMsg.TargetName, VoteOnMsg.Reason);
end;

procedure ClientHandleVoteOff;
begin
  StopVote;
end;

procedure ClientHandleServerSyncMsg(NetMessage: PSteamNetworkingMessage_t);
var
  SyncMsg: TMsg_ServerSyncMsg;
  I: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSyncMsg), NetMessage^.m_cbSize, MsgID_ServerSyncMsg) then
    Exit;

  SyncMsg := PMsg_ServerSyncMsg(NetMessage^.m_pData)^;

  TimeLimitCounter := SyncMsg.Time;
  if SyncMsg.Pause = 1 then
  begin
    MapChangeCounter := 999999999;
    MapChangeName := 'PAUSE*!*';
    for I := 1 to MAX_PLAYERS do
      if Sprite[I].Active then
      begin
        StopSound(Sprite[I].ReloadSoundChannel);
        StopSound(Sprite[I].JetsSoundChannel);
        StopSound(Sprite[I].GattlingSoundChannel);
        StopSound(Sprite[I].GattlingSoundChannel2);
      end;
  end
  else if MapChangeCounter = 999999999 then
    MapChangeCounter := -60;
end;

procedure ClientHandleForcePosition(NetMessage: PSteamNetworkingMessage_t);
var
  ForcePosition: TMsg_ForcePosition;
begin
  if not VerifyPacket(sizeof(TMsg_ForcePosition), NetMessage^.m_cbSize, MsgID_ForcePosition) then
    Exit;

  ForcePosition := PMsg_ForcePosition(NetMessage^.m_pData)^;

  SpriteParts.Pos[ForcePosition.PlayerID] := ForcePosition.Pos;
  SpriteParts.OldPos[ForcePosition.PlayerID] := SpriteParts.Pos[ForcePosition.PlayerID];
end;

procedure ClientHandleForceVelocity(NetMessage: PSteamNetworkingMessage_t);
var
  ForceVelocity: TMsg_ForceVelocity;
begin
  if not VerifyPacket(sizeof(TMsg_ForceVelocity), NetMessage^.m_cbSize, MsgID_ForceVelocity) then
    Exit;

  ForceVelocity := PMsg_ForceVelocity(NetMessage^.m_pData)^;

  SpriteParts.Velocity[ForceVelocity.PlayerID] := ForceVelocity.Vel;
end;

procedure ClientHandleForceWeapon(NetMessage: PSteamNetworkingMessage_t);
var
  ForceWeapon: TMsg_ForceWeapon;
begin
  if not VerifyPacket(sizeof(TMsg_ForceWeapon), NetMessage^.m_cbSize, MsgID_ForceWeapon) then
    Exit;

  ForceWeapon := PMsg_ForceWeapon(NetMessage^.m_pData)^;

  if (MySprite > 0) then
  begin
    Sprite[MySprite].ApplyWeaponByNum(ForceWeapon.WeaponNum, 1);
    Sprite[MySprite].ApplyWeaponByNum(ForceWeapon.SecondaryWeaponNum, 2);
    Sprite[MySprite].Weapon.AmmoCount := ForceWeapon.AmmoCount;
    Sprite[MySprite].SecondaryWeapon.AmmoCount := ForceWeapon.SecAmmoCount;
  end;
end;

procedure ClientHandleWeaponActiveMessage(NetMessage: PSteamNetworkingMessage_t);
var
  WActiveMessage: TMsg_WeaponActiveMessage;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_WeaponActiveMessage), NetMessage^.m_cbSize, MsgID_WeaponActiveMessage) then
    Exit;

  WActiveMessage := PMsg_WeaponActiveMessage(NetMessage^.m_pData)^;

  if (WActiveMessage.Weapon > 0) and (WActiveMessage.Weapon <= MAIN_WEAPONS) then
  begin
    WeaponSel[MySprite][WActiveMessage.Weapon] := WActiveMessage.Active;

    for i := 1 to MAIN_WEAPONS do
      if WeaponActive[i] = 1 then
        LimboMenu.Button[i - 1].Active := Boolean(WeaponSel[MySprite][i]);
  end;
end;

procedure ClientHandleClientFreeCam(NetMessage: PSteamNetworkingMessage_t);
var
  FreeCamMsg: TMsg_ClientFreeCam;
begin
  if not VerifyPacket(sizeof(TMsg_ClientFreeCam), NetMessage^.m_cbSize, MsgID_ClientFreeCam) then
    Exit;

  FreeCamMsg := PMsg_ClientFreeCam(NetMessage^.m_pData)^;

  if (MySprite > 0) then
  begin
    if FreeCamMsg.FreeCamOn = 1 then
    begin
      CameraFollowSprite := 0;
      TargetMode := true;
    end
    else
    begin
      CameraFollowSprite := MySprite;
      TargetMode := false;
    end;

    if (FreeCamMsg.TargetPos.x <> 0.0) and
       (FreeCamMsg.TargetPos.y <> 0.0) then
    begin
      CameraX := FreeCamMsg.TargetPos.x;
      CameraY := FreeCamMsg.TargetPos.y;
    end;
  end;
end;

// Server tells client to join another server
procedure ClientHandleJoinServer(NetMessage: PSteamNetworkingMessage_t);
var
  JoinServerMsg: TMsg_JoinServer;
begin
  if not VerifyPacket(sizeof(TMsg_JoinServer), NetMessage^.m_cbSize, MsgID_JoinServer) then
    Exit;

  JoinServerMsg := PMsg_JoinServer(NetMessage^.m_pData)^;
  MainConsole.Console('Redirecting to... ' + NetAddrToStr(in_addr(JoinServerMsg.IP)) + ':' +
    IntToStr(JoinServerMsg.Port), SERVER_MESSAGE_COLOR);

  ClientDisconnect;

  RedirectToServer := true;
  RedirectIP := NetAddrToStr(in_addr(JoinServerMsg.IP));
  RedirectPort := JoinServerMsg.Port;
  RedirectMsg := Trim(PChar(@PMsg_JoinServer(NetMessage^.m_pData)^.ShowMsg));

  ExitToMenu;
end;

procedure ClientHandlePlaySound(NetMessage: PSteamNetworkingMessage_t);
var
  PlaySoundMsg: TMsg_PlaySound;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_PlaySound), NetMessage^.m_cbSize, MsgID_PlaySound) then
    Exit;

  PlaySoundMsg := PMsg_PlaySound(NetMessage^.m_pData)^;

  if PHYSFS_exists(PChar(ModDir + 'sfx/' + PlaySoundMsg.Name)) then
  begin
    // Name to ID, for easy use for scripters
    i := SoundNameToID(PlaySoundMsg.Name);

    // Sound downloaded, but not initialized. So intialize it
    if i = -1 then
    begin
      SetLength(ScriptSamp, High(ScriptSamp) + 2);
      i := High(ScriptSamp);
      ScriptSamp[i].Name := PlaySoundMsg.Name;
      ScriptSamp[i].Samp := LoadSample(PChar(ModDir +
        'sfx/' + PlaySoundMsg.Name), ScriptSamp[i].Samp);
    end;
    if PlaySoundMsg.Emitter.x = 0 then
      PlaySoundMsg.Emitter.x := SpriteParts.Pos[MySprite].X;
    if PlaySoundMsg.Emitter.y = 0 then
      PlaySoundMsg.Emitter.y := SpriteParts.Pos[MySprite].y;
    PlaySound(i, PlaySoundMsg.Emitter);
  end;
end;

end.

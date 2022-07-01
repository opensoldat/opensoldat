unit NetworkServerFunctions;

interface

uses
  // delphi and system units
  SysUtils, Classes, sockets,

  // helper units
  Vector, Util,

  // soldat units
  Steam, Net, Sprites, Weapons, Constants;

procedure SetWeaponActive(ID, WeaponNum: Byte; State: Boolean);
procedure ForceWeapon(ID, Primary, Secondary, Ammo, SecAmmo: Byte);
procedure MovePlayer(ID: Byte; X, Y: Single);
procedure ModifyPlayerVelocity(ID: Byte; VelX, VelY: Single);
procedure ForwardClient(ID: Byte; TargetIP: string; TargetPort: Integer; ShowMsg: String);
procedure PlaySound(ID: Byte; Name: string; X, Y: Single);
procedure ServerHandleClientFreeCam(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  Server, Game, NetworkUtils;

procedure ServerSendFreeCam(ToNum: Byte; FreeCam: Boolean; Pos: TVector2);
var
  FreeCamMsg: TMsg_ClientFreeCam;
begin
  FreeCamMsg.Header.ID := MsgID_ClientFreeCam;
  FreeCamMsg.FreeCamOn := Byte(FreeCam);
  FreeCamMsg.TargetPos := Pos;

  UDP.SendData(FreeCamMsg, sizeof(FreeCamMsg), Sprite[ToNum].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure SetWeaponActive(ID, WeaponNum: Byte; State: Boolean);
var
  WepMsg: TMsg_WeaponActiveMessage;
  i: Integer;
begin
  WepMsg.Header.ID := MsgID_WeaponActiveMessage;
  WepMsg.Weapon := WeaponNum;
  WepMsg.Active := iif(State, 1, 0);

  if ID = 0 then
  begin
    for i := 1 to MAX_PLAYERS do
      if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
        UDP.SendData(WepMsg, sizeof(WepMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
  end
  else
    if (Sprite[ID].Active) and (Sprite[ID].Player.ControlMethod = HUMAN) then
      UDP.SendData(WepMsg, sizeof(WepMsg), Sprite[ID].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ForceWeapon(ID, Primary, Secondary, Ammo, SecAmmo: Byte);
var
  WepMsg: TMsg_ForceWeapon;
begin
  Sprite[ID].ApplyWeaponByNum(Primary, 1);
  Sprite[ID].ApplyWeaponByNum(Secondary, 2);

  if Ammo > Guns[WeaponNumToIndex(Primary)].Ammo then
    Ammo :=  Guns[WeaponNumToIndex(Primary)].Ammo;

  if SecAmmo > Guns[WeaponNumToIndex(Secondary)].Ammo then
    SecAmmo := Guns[WeaponNumToIndex(Secondary)].Ammo;

  Sprite[ID].Weapon.Ammo := Ammo;
  Sprite[ID].SecondaryWeapon.Ammo := SecAmmo;

  if Sprite[ID].Player.ControlMethod = HUMAN then
  begin
    WepMsg.Header.ID := MsgId_ForceWeapon;
    WepMsg.WeaponNum := Primary;
    WepMsg.SecondaryWeaponNum := Secondary;
    WepMsg.AmmoCount := Ammo;
    WepMsg.SecAmmoCount := SecAmmo;

    UDP.SendData(WepMsg, sizeof(WepMsg), Sprite[ID].Player.peer, k_nSteamNetworkingSend_Reliable);
  end;

  {$IFDEF SCRIPT}
  // In some places in code, where we apply weapons (sprie snapshot, object collisions),
  // we need to check whenever weapons are already changed, hence this ugly workaround
  // with boolean variable.
  ForceWeaponCalled := True;
  {$ENDIF}
end;

procedure MovePlayer(ID: Byte; X, Y: Single);
var
  MoveMsg: TMsg_ForcePosition;
  i: Byte;
begin
  if not Sprite[ID].Active then
    Exit;

  SpriteParts.Pos[ID].X := X;
  SpriteParts.Pos[ID].Y := Y;
  SpriteParts.OldPos[ID] := SpriteParts.Pos[ID];

  MoveMsg.Header.ID := MsgID_ForcePosition;

  MoveMsg.Pos := SpriteParts.Pos[ID];
  MoveMsg.PlayerID := ID;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[I].Active) and (Sprite[I].Player.ControlMethod = HUMAN) then
      UDP.SendData(MoveMsg, sizeof(MoveMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ModifyPlayerVelocity(ID: Byte; VelX, VelY: Single);
var
  VelMsg: TMsg_ForceVelocity;
  I: Byte;
begin
  if not Sprite[ID].Active then
    Exit;

  SpriteParts.Velocity[ID].X := VelX;
  SpriteParts.Velocity[ID].Y := VelY;

  VelMsg.Header.ID := MsgID_ForceVelocity;

  VelMsg.Vel := SpriteParts.Velocity[ID];
  VelMsg.PlayerID := ID;

  for I := 1 to MAX_PLAYERS do
    if (Sprite[I].Active) and (Sprite[I].Player.ControlMethod = HUMAN) then
      UDP.SendData(VelMsg, sizeof(VelMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ForwardClient(ID: Byte; TargetIP: string; TargetPort: Integer; ShowMsg: string);
var
  JoinServerMsg: TMsg_JoinServer;
begin
  JoinServerMsg.Header.ID := MsgID_JoinServer;
  JoinServerMsg.Port := TargetPort;
  JoinServerMsg.IP := LongWord(StrToNetAddr(TargetIP));
  StringToArray(JoinServerMsg.ShowMsg, ShowMsg);

  if (Sprite[ID].Active) and (Sprite[ID].Player.ControlMethod = HUMAN) then
    UDP.SendData(JoinServerMsg, sizeof(JoinServerMsg), Sprite[ID].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure PlaySound(ID: Byte; Name: string; X, Y: Single);
var
  PlaySoundMsg: TMsg_PlaySound;
  Pos: TVector2;
begin
  PlaySoundMsg.Header.ID := MsgID_PlaySound;
  Pos.x := X;
  Pos.y := Y;
  PlaySoundMsg.Emitter := Pos;
  StringToArray(PlaySoundMsg.Name, Name);
  // PlaySoundMsg.Name := Name;

  if ID = 0 then
  begin
    for ID := 1 to MAX_PLAYERS do
      if (Sprite[ID].Active) and (Sprite[ID].Player.ControlMethod = HUMAN) then
        UDP.SendData(PlaySoundMsg, sizeof(PlaySoundMsg), Sprite[ID].Player.peer, k_nSteamNetworkingSend_Reliable);
  end else
    if (Sprite[ID].Active) and (Sprite[ID].Player.ControlMethod = HUMAN) then
      UDP.SendData(PlaySoundMsg, sizeof(PlaySoundMsg), Sprite[ID].Player.peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerHandleClientFreeCam(NetMessage: PSteamNetworkingMessage_t);
var
  FreeCamMsg: TMsg_ClientFreeCam;
  Player: TPlayer;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ClientFreeCam), NetMessage^.m_cbSize, MsgID_ClientFreeCam) then
    Exit;
  FreeCamMsg := PMsg_ClientFreeCam(NetMessage^.m_pData)^;
  Player := TPlayer(NetMessage^.m_nConnUserData);
  i := Player.SpriteNum;
  Sprite[i].TargetX := FreeCamMsg.TargetPos.X;
  Sprite[i].TargetY := FreeCamMsg.TargetPos.Y;
end;

end.

unit NetworkClientSprite;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // helper units
  Vector,

  // opensoldat units
  Steam, Net, Sprites, Weapons, Sound,
  Constants, GameStrings;

procedure ClientHandleServerSpriteSnapshot(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleServerSpriteSnapshot_Major(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleServerSkeletonSnapshot(NetMessage: PSteamNetworkingMessage_t);
procedure ClientSpriteSnapshot;
procedure ClientSpriteSnapshotMov;
procedure ClientSpriteSnapshotDead;
procedure ClientHandleSpriteDeath(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleDelta_Movement(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleDelta_MouseAim(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleDelta_Weapons(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleDelta_Helmet(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleClientSpriteSnapshot_Dead(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  Client, NetworkUtils, Game, Demo, ClientGame, Sparks, GameMenus, TraceLog;

var
  OldClientSnapshotMsg: TMsg_ClientSpriteSnapshot;
  OldClientSnapshotMovMsg: TMsg_ClientSpriteSnapshot_Mov;

procedure ClientHandleServerSpriteSnapshot(NetMessage: PSteamNetworkingMessage_t);
var
  SpriteSnap: TMsg_ServerSpriteSnapshot;
  i, j: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSpriteSnapshot), NetMessage^.m_cbSize, MsgID_ServerSpriteSnapshot) then
    Exit;

  SpriteSnap := PMsg_ServerSpriteSnapshot(NetMessage^.m_pData)^;

  // assign received sprite info to sprite
  i := SpriteSnap.Num;

  if (i < 1) or (i > MAX_SPRITES) then
    Exit;
  if not Sprite[i].Active then
    Exit;

  ClientTickCount := SpriteSnap.ServerTicks;
  LastHeartBeatCounter := SpriteSnap.ServerTicks;

  // CLIENT RESPAWN
  if Sprite[i].DeadMeat then
  begin
    SpriteParts.OldPos[i] := SpriteParts.Pos[i];
    SpriteParts.Pos[i] := SpriteSnap.Pos;
    SpriteParts.Velocity[i] := SpriteSnap.Velocity;
    Sprite[i].Respawn;
    Sprite[i].OldDeadMeat := Sprite[i].DeadMeat;
    SpriteParts.Pos[i] := SpriteSnap.Pos;
  end;

  Sprite[i].DeadMeat := false;

  if i <> MySprite then
  begin
    if Sprite[i].Health = SpriteSnap.Health then
    begin
      SpriteParts.OldPos[i] := SpriteParts.Pos[i];
      SpriteParts.Pos[i] := SpriteSnap.Pos;
      SpriteParts.Velocity[i] := SpriteSnap.Velocity;
    end;

    Sprite[i].Control.MouseAimY := SpriteSnap.MouseAimY;
    Sprite[i].Control.MouseAimX := SpriteSnap.MouseAimX;

    DecodeKeys(Sprite[i], SpriteSnap.Keys16);

    if Sprite[i].Weapon.Num <> SpriteSnap.WeaponNum then
      Sprite[i].ApplyWeaponByNum(SpriteSnap.WeaponNum, 1);
    if Sprite[i].SecondaryWeapon.Num <> SpriteSnap.SecondaryWeaponNum then
      Sprite[i].ApplyWeaponByNum(SpriteSnap.SecondaryWeaponNum, 2);
    Sprite[i].Weapon.AmmoCount := SpriteSnap.AmmoCount;

    if Sprite[i].Weapon.Num = Guns[KNIFE].Num then
      Sprite[i].Player.SecWep := 1;
    if Sprite[i].Weapon.Num = Guns[CHAINSAW].Num then
      Sprite[i].Player.SecWep := 2;
    if Sprite[i].Weapon.Num = Guns[LAW].Num then
      Sprite[i].Player.SecWep := 3;

    // Toggle prone if it was activated or deactivated
    Sprite[i].Control.Prone :=
      (SpriteSnap.Position = POS_PRONE) xor (Sprite[i].Position = POS_PRONE);
  end;

  // kill the bow
  if (Sprite[i].Weapon.Num = Guns[BOW].Num) or
    (Sprite[i].Weapon.Num = Guns[BOW2].Num) then
    for j := 1 to MAX_THINGS do
      if (Thing[j].Active) and (Thing[j].Style = OBJECT_RAMBO_BOW) then
      begin
        GameThingTarget := 0;
        Thing[j].Kill;
      end;

  Sprite[i].WearHelmet := 1;
  if SpriteSnap.Look and B1 = B1 then
    Sprite[i].WearHelmet := 0;
  if SpriteSnap.Look and B4 = B4 then
    Sprite[i].WearHelmet := 2;
  if (Sprite[i].BodyAnimation.ID <> Cigar.ID) and
     (Sprite[i].BodyAnimation.ID <> Smoke.ID) and not
     ((Sprite[i].IdleRandom = 1) and
      (Sprite[i].BodyAnimation.ID = Stand.ID)) then
  begin
    Sprite[i].HasCigar := 0;
    if SpriteSnap.Look and B2 = B2 then
      Sprite[i].HasCigar := 5;
    if SpriteSnap.Look and B3 = B3 then
      Sprite[i].HasCigar := 10;
  end;

  Sprite[i].TertiaryWeapon.AmmoCount := SpriteSnap.GrenadeCount;

  Sprite[i].Health := SpriteSnap.Health;
  Sprite[i].Vest := SpriteSnap.Vest;
  if Sprite[i].Vest > DEFAULTVEST then
    Sprite[i].Vest := DEFAULTVEST;

  if i = MySprite then
  begin
    if (not TargetMode) then
    begin
      CameraFollowSprite := MySprite;
      Sprite[i].Player.Camera := MySprite;
    end;
  end;

end;

procedure ClientHandleServerSpriteSnapshot_Major(NetMessage: PSteamNetworkingMessage_t);
var
  SpriteSnapMajor: TMsg_ServerSpriteSnapshot_Major;
  i, j: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSpriteSnapshot_Major), NetMessage^.m_cbSize, MsgID_ServerSpriteSnapshot_Major) then
    Exit;

  SpriteSnapMajor := PMsg_ServerSpriteSnapshot_Major(NetMessage^.m_pData)^;

  // assign received sprite info to sprite
  i := SpriteSnapMajor.Num;

  if (i < 1) or (i > MAX_SPRITES) then
    Exit;

  if not Sprite[i].Active then
  begin
    Debug('[ClientSprite] Warning: Received snapshot for inactive player ' + IntToStr(i));
    Exit;
  end;

  ClientTickCount := SpriteSnapMajor.ServerTicks;
  LastHeartBeatCounter := SpriteSnapMajor.ServerTicks;

  // CLIENT RESPAWN
  if Sprite[i].DeadMeat then
  begin
    SpriteParts.OldPos[i] := SpriteParts.Pos[i];
    SpriteParts.Pos[i] := SpriteSnapMajor.Pos;
    SpriteParts.Velocity[i] := SpriteSnapMajor.Velocity;
    Sprite[i].Respawn;
    Sprite[i].OldDeadMeat := Sprite[i].DeadMeat;
    SpriteParts.Pos[i] := SpriteSnapMajor.Pos;
  end;

  Sprite[i].DeadMeat := false;

  if i <> MySprite then
  begin
    if Sprite[i].Health = SpriteSnapMajor.Health then
    begin
      SpriteParts.OldPos[i] := SpriteParts.Pos[i];
      SpriteParts.Pos[i] := SpriteSnapMajor.Pos;
      SpriteParts.Velocity[i] := SpriteSnapMajor.Velocity;
    end;

    Sprite[i].Control.MouseAimY := SpriteSnapMajor.MouseAimY;
    Sprite[i].Control.MouseAimX := SpriteSnapMajor.MouseAimX;

    DecodeKeys(Sprite[i], SpriteSnapMajor.Keys16);

    // Toggle prone if it was activated or deactivated
    Sprite[i].Control.Prone :=
      (SpriteSnapMajor.Position = POS_PRONE) xor (Sprite[i].Position = POS_PRONE);
  end;

  // kill the bow
  if (Sprite[i].Weapon.Num = Guns[BOW].Num) or
    (Sprite[i].Weapon.Num = Guns[BOW2].Num) then
    for j := 1 to MAX_THINGS do
      if (Thing[j].Active) and (Thing[j].Style = OBJECT_RAMBO_BOW) then
      begin
        GameThingTarget := 0;
        Thing[j].Kill;
      end;

  Sprite[i].Health := SpriteSnapMajor.Health;

  if i = MySprite then
  begin
    if (not TargetMode) then
    begin
      CameraFollowSprite := MySprite;
      Sprite[i].Player.Camera := MySprite;
    end;
  end;

end;

procedure ClientHandleServerSkeletonSnapshot(NetMessage: PSteamNetworkingMessage_t);
var
  SkeletonSnap: TMsg_ServerSkeletonSnapshot;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSkeletonSnapshot), NetMessage^.m_cbSize, MsgID_ServerSkeletonSnapshot) then
    Exit;

  SkeletonSnap := PMsg_ServerSkeletonSnapshot(NetMessage^.m_pData)^;

  // assign received Skeleton info to skeleton
  i := SkeletonSnap.Num;

  if (i < 1) or (i > MAX_SPRITES) then
    Exit;
  if not Sprite[i].Active then
    Exit;

  Sprite[i].DeadMeat := true;
  Sprite[i].RespawnCounter := SkeletonSnap.RespawnCounter;
  Sprite[i].Weapon := Guns[NOWEAPON];

end;

procedure ClientSpriteSnapshot;
var
  ClientMsg: TMsg_ClientSpriteSnapshot;
begin
  ClientMsg.Header.ID := MsgID_ClientSpriteSnapshot;

  ClientMsg.AmmoCount := Sprite[MySprite].Weapon.AmmoCount;
  ClientMsg.SecondaryAmmoCount := Sprite[MySprite].SecondaryWeapon.AmmoCount;
  ClientMsg.WeaponNum := Sprite[MySprite].Weapon.Num;
  ClientMsg.SecondaryWeaponNum := Sprite[MySprite].SecondaryWeapon.Num;
  ClientMsg.Position := Sprite[MySprite].Position;

  if (ClientMsg.AmmoCount = OldClientSnapshotMsg.AmmoCount) and
     (ClientMsg.WeaponNum = OldClientSnapshotMsg.WeaponNum) and
     (ClientMsg.SecondaryWeaponNum = OldClientSnapshotMsg.SecondaryWeaponNum) and
     (ClientMsg.Position = OldClientSnapshotMsg.Position) then
    Exit;

  OldClientSnapshotMsg := ClientMsg;

  UDP.SendData(ClientMsg, sizeof(ClientMsg), k_nSteamNetworkingSend_Unreliable);
end;

// CLIENT SPRITE SNAPSHOT MOV
procedure ClientSpriteSnapshotMov;
var
  ClientMsg: TMsg_ClientSpriteSnapshot_Mov;
  PosDiff, VelDiff: TVector2;
begin
  ClientMsg.Header.ID := MsgID_ClientSpriteSnapshot_Mov;

  ClientMsg.Pos := SpriteParts.Pos[MySprite];
  ClientMsg.Velocity := SpriteParts.Velocity[MySprite];
  ClientMsg.MouseAimX := Sprite[MySprite].Control.MouseAimX;
  ClientMsg.MouseAimY := Sprite[MySprite].Control.MouseAimY;

  EncodeKeys(Sprite[MySprite], ClientMsg.Keys16);

  if Sprite[MySprite].DontDrop then
    ClientMsg.Keys16 := ClientMsg.Keys16 and not B9;

  PosDiff := Vec2Subtract(ClientMsg.Pos, OldClientSnapshotMovMsg.Pos);
  VelDiff := Vec2Subtract(ClientMsg.Velocity, OldClientSnapshotMovMsg.Velocity);

  if (Vec2Length(PosDiff) > POSDELTA) or
     (Vec2Length(VelDiff) > VELDELTA) or
     (ClientMsg.Keys16 <> OldClientSnapshotMovMsg.Keys16) or
     (ClientMsg.Keys16 and B6 = B6) or
     not (((Sprite[MySprite].Weapon.FireInterval <= FIREINTERVAL_NET) and
           (Sprite[MySprite].Weapon.AmmoCount > 0) and
           (Round(mx) = OldClientSnapshotMovMsg.MouseAimX) and
           (Round(my) = OldClientSnapshotMovMsg.MouseAimY)) or
          ((Abs(mx - OldClientSnapshotMovMsg.MouseAimX) < MOUSEAIMDELTA) and
           (Abs(my - OldClientSnapshotMovMsg.MouseAimY) < MOUSEAIMDELTA))) then
  begin
    OldClientSnapshotMovMsg := ClientMsg;
    OldClientSnapshotMovMsg.MouseAimX := Round(mx);
    OldClientSnapshotMovMsg.MouseAimY := Round(my);

    UDP.SendData(ClientMsg, sizeof(ClientMsg), k_nSteamNetworkingSend_Unreliable);
  end;
end;

// CLIENT SPRITE SNAPSHOT DEAD
procedure ClientSpriteSnapshotDead;
var
  ClientMsg: TMsg_ClientSpriteSnapshot_Dead;
begin
  ClientMsg.Header.ID := MsgID_ClientSpriteSnapshot_Dead;
  ClientMsg.CameraFocus := CameraFollowSprite;

  UDP.SendData(ClientMsg, sizeof(ClientMsg), k_nSteamNetworkingSend_Unreliable);
end;

procedure ClientHandleSpriteDeath(NetMessage: PSteamNetworkingMessage_t);
var
  DeathSnap: TMsg_SpriteDeath;
  i, d, j, k: Integer;
  b: TVector2;
  col, col2: Cardinal;
  hm: Single = 0.0;
begin
  if not VerifyPacket(sizeof(TMsg_SpriteDeath), NetMessage^.m_cbSize, MsgID_SpriteDeath) then
    Exit;

  DeathSnap := PMsg_SpriteDeath(NetMessage^.m_pData)^;

  i := DeathSnap.Num;

  if (i < 1) or (i > MAX_SPRITES) then
    Exit;
  if not Sprite[i].Active then
    Exit;

  for d := 1 to 16 do
    if (Round(DeathSnap.Pos[d].X) <> 0) and
      (Round(DeathSnap.Pos[d].Y) <> 0) and
      (Round(DeathSnap.OldPos[d].X) <> 0) and
      (Round(DeathSnap.OldPos[d].Y) <> 0) then
    begin
      Sprite[i].Skeleton.Pos[d].X := DeathSnap.Pos[d].X;
      Sprite[i].Skeleton.Pos[d].Y := DeathSnap.Pos[d].Y;
      Sprite[i].Skeleton.OldPos[d].X := DeathSnap.OldPos[d].X;
      Sprite[i].Skeleton.OldPos[d].Y := DeathSnap.OldPos[d].Y;

      if d = 1 then
      begin
        Sprite[i].Skeleton.Pos[17].X := DeathSnap.Pos[1].X;
        Sprite[i].Skeleton.Pos[17].Y := DeathSnap.Pos[1].Y;
        Sprite[i].Skeleton.OldPos[17].X := DeathSnap.OldPos[1].X;
        Sprite[i].Skeleton.OldPos[17].Y := DeathSnap.OldPos[1].Y;
      end;
      if d = 2 then
      begin
        Sprite[i].Skeleton.Pos[18].X := DeathSnap.Pos[2].X;
        Sprite[i].Skeleton.Pos[18].Y := DeathSnap.Pos[2].Y;
        Sprite[i].Skeleton.OldPos[18].X := DeathSnap.OldPos[2].X;
        Sprite[i].Skeleton.OldPos[18].Y := DeathSnap.OldPos[2].Y;
      end;
      if d = 15 then
      begin
        Sprite[i].Skeleton.Pos[19].X := DeathSnap.Pos[15].X;
        Sprite[i].Skeleton.Pos[19].Y := DeathSnap.Pos[15].Y;
        Sprite[i].Skeleton.OldPos[19].X := DeathSnap.OldPos[15].X;
        Sprite[i].Skeleton.OldPos[19].Y := DeathSnap.OldPos[15].Y;
      end;
      if d = 16 then
      begin
        Sprite[i].Skeleton.Pos[20].X := DeathSnap.Pos[16].X;
        Sprite[i].Skeleton.Pos[20].Y := DeathSnap.Pos[16].Y;
        Sprite[i].Skeleton.OldPos[20].X := DeathSnap.OldPos[16].X;
        Sprite[i].Skeleton.OldPos[20].Y := DeathSnap.OldPos[16].Y;
      end;
    end;

  b.x := 0;
  b.y := 0;
  Sprite[i].Health := DeathSnap.Health;

  // death!
  if (Sprite[i].Health < 1) and
    (Sprite[i].Health > HEADCHOPDEATHHEALTH) then
    Sprite[i].Die(NORMAL_DEATH, Deathsnap.Killer,
      DeathSnap.Where, Deathsnap.KillBullet, b)
  else if (Sprite[i].Health<(HEADCHOPDEATHHEALTH + 1)) and
    (Sprite[i].Health > BRUTALDEATHHEALTH) then
    Sprite[i].Die(HEADCHOP_DEATH, Deathsnap.Killer, DeathSnap.Where,
      Deathsnap.KillBullet, b)
  else if (Sprite[i].Health<(BRUTALDEATHHEALTH + 1)) then
    Sprite[i].Die(BRUTAL_DEATH, Deathsnap.Killer, DeathSnap.Where,
      Deathsnap.KillBullet, b);

  Sprite[i].Skeleton.Constraints[2].Active := true;
  Sprite[i].Skeleton.Constraints[4].Active := true;
  Sprite[i].Skeleton.Constraints[20].Active := true;
  Sprite[i].Skeleton.Constraints[21].Active := true;
  Sprite[i].Skeleton.Constraints[23].Active := true;
  if Deathsnap.Constraints and B1 = B1 then
    Sprite[i].Skeleton.Constraints[2].Active := false;
  if Deathsnap.Constraints and B2 = B2 then
    Sprite[i].Skeleton.Constraints[4].Active := false;
  if Deathsnap.Constraints and B3 = B3 then
    Sprite[i].Skeleton.Constraints[20].Active := false;
  if Deathsnap.Constraints and B4 = B4 then
    Sprite[i].Skeleton.Constraints[21].Active := false;
  if Deathsnap.Constraints and B5 = B5 then
    Sprite[i].Skeleton.Constraints[23].Active := false;

  Sprite[i].Weapon := Guns[NOWEAPON];
  Sprite[i].RespawnCounter := DeathSnap.RespawnCounter;
  Sprite[i].OnFire := DeathSnap.OnFire;

  // mulitkill count
  if Deathsnap.Killer <> i then
  begin
    Sprite[Deathsnap.Killer].MultiKillTime := MULTIKILLINTERVAL;
    Inc(Sprite[Deathsnap.Killer].MultiKills);
  end;

  if i = MySprite then
  begin
    BigMessage(WideFormat(_('Killed by %s'),
      [Sprite[Deathsnap.Killer].Player.Name]),
      KILLMESSAGEWAIT, DIE_MESSAGE_COLOR);
    if not LimboLock then
      GameMenuShow(LimboMenu);
    MenuTimer := MENU_TIME;
    PlaySound(SFX_PLAYERDEATH);
  end;

  if Deathsnap.Killer = MySprite then
  begin
    BigMessage(WideFormat(_('You killed %s'), [Sprite[i].Player.Name]),
      KILLMESSAGEWAIT, KILL_MESSAGE_COLOR);

    if (Sprite[Deathsnap.Killer].MultiKills > 1) and
      (Sprite[Deathsnap.Killer].MultiKills < 18) then
      BigMessage(MULTIKILLMESSAGE[Sprite[Deathsnap.Killer].MultiKills],
        KILLMESSAGEWAIT, KILL_MESSAGE_COLOR);
    if (Sprite[Deathsnap.Killer].MultiKills > 17) then
      BigMessage(MULTIKILLMESSAGE[9], KILLMESSAGEWAIT,
        KILL_MESSAGE_COLOR);

    if (ShotDistance > -1) and (Deathsnap.Killer <> i) then
    begin
      ShotDistanceShow := KILLMESSAGEWAIT - 30;
      ShotDistance := Deathsnap.ShotDistance;
      ShotRicochet := Deathsnap.ShotRicochet;
      ShotLife := Deathsnap.ShotLife;
    end;
  end;

  if (Deathsnap.Killer = MySprite) and (i = MySprite) then
    BigMessage(_('You killed yourself'), KILLMESSAGEWAIT, DIE_MESSAGE_COLOR);

  // This k seems to go to the rendering code through KillConsole.NumMessage,
  // where it was used as IntTexture[11 + k] basically, so I'm replacing with
  // texture constants instead, without that 11 offset.
  // Similar code can be found in TSprite.Die (Sprites.pas)

  case Deathsnap.KillBullet of
    0:     k := GFX_INTERFACE_GUNS_SOCOM;
    1..10: k := GFX_INTERFACE_GUNS_DEAGLES + Deathsnap.KillBullet - 1;
    205:   k := GFX_INTERFACE_GUNS_FLAMER;
    206:   k := GFX_INTERFACE_GUNS_FIST;
    207:   k := GFX_INTERFACE_GUNS_BOW;
    208:   k := GFX_INTERFACE_GUNS_BOW;
    210:   k := GFX_INTERFACE_CLUSTER_NADE;
    211:   k := GFX_INTERFACE_GUNS_KNIFE;
    212:   k := GFX_INTERFACE_GUNS_CHAINSAW;
    222:   k := GFX_INTERFACE_NADE;
    224:   k := GFX_INTERFACE_GUNS_LAW;
    225:   k := GFX_INTERFACE_GUNS_M2;
    else   k := -255;
  end;

  col := 0;
  col2 := 0;
  case Sprite[Deathsnap.Killer].Player.Team of
    TEAM_NONE: col := KILLER_MESSAGE_COLOR;
    TEAM_ALPHA: col := ALPHA_K_MESSAGE_COLOR;
    TEAM_BRAVO: col := BRAVO_K_MESSAGE_COLOR;
    TEAM_CHARLIE: col := CHARLIE_K_MESSAGE_COLOR;
    TEAM_DELTA: col := DELTA_K_MESSAGE_COLOR;
  end;
  case Sprite[i].Player.Team of
    TEAM_NONE: col2 := DEATH_MESSAGE_COLOR;
    TEAM_ALPHA: col2 := ALPHA_D_MESSAGE_COLOR;
    TEAM_BRAVO: col2 := BRAVO_D_MESSAGE_COLOR;
    TEAM_CHARLIE: col2 := CHARLIE_D_MESSAGE_COLOR;
    TEAM_DELTA: col2 := DELTA_D_MESSAGE_COLOR;
  end;

  if Deathsnap.Killer <> i then
  begin
    KillConsole.consoleNum(WideString(Sprite[Deathsnap.Killer].Player.Name) + ' (' +
      WideString(IntToStr(Sprite[Deathsnap.Killer].Player.Kills)) + ')', col, k);
    KillConsole.consoleNum(WideString(Sprite[i].Player.Name), col2, -255);
  end
  else
  begin
    KillConsole.consoleNum(WideString(Sprite[Deathsnap.Killer].Player.Name) + ' (' +
      WideString(IntToStr(Sprite[Deathsnap.Killer].Player.Kills)) + ')',
      SPECTATOR_D_MESSAGE_COLOR, k);
  end;

  // Explode - lag compensate
  if Deathsnap.KillBullet = 7 then // M79
    for j := MAX_BULLETS downto 1 do
      if (Bullet[j].Active) and (Bullet[j].Owner = Deathsnap.Killer) and
        (Bullet[j].Style = Guns[M79].BulletStyle) then
      begin
        Bulletparts.OldPos[j] := Sprite[i].Skeleton.Pos[8];
        Bulletparts.Pos[j] := Sprite[i].Skeleton.Pos[8];
        Bullet[j].Hit(3);
        Bullet[j].Kill;
        break;
      end;

  if Deathsnap.KillBullet = 224 then {LAW}
    for j := MAX_BULLETS downto 1 do
      if (Bullet[j].Active) and (Bullet[j].Owner = Deathsnap.Killer) and
        (Bullet[j].Style = Guns[LAW].BulletStyle) then
      begin
        Bulletparts.OldPos[j] := Sprite[i].Skeleton.Pos[8];
        Bulletparts.Pos[j] := Sprite[i].Skeleton.Pos[8];
        Bullet[j].Hit(3);
        Bullet[j].Kill;
        break;
      end;

  if Deathsnap.KillBullet = 222 then {grenade}
    for j := MAX_BULLETS downto 1 do
      if (Bullet[j].Active) and (Bullet[j].Owner = Deathsnap.Killer) and
        (Bullet[j].Style = Guns[FRAGGRENADE].BulletStyle) then
      begin
        Map.RayCast(Bulletparts.Pos[j], Sprite[i].Skeleton.Pos[8], hm, 351);
        if hm < AFTER_EXPLOSION_RADIUS then
        begin
          Bulletparts.OldPos[j] := Spriteparts.Pos[i];
          Bulletparts.Pos[j] := Spriteparts.Pos[i];
          Bullet[j].Hit(4);
          Bullet[j].Kill;
        end;
      end;
end;

procedure ClientHandleDelta_Movement(NetMessage: PSteamNetworkingMessage_t);
var
  DeltaMov: TMsg_ServerSpriteDelta_Movement;
  i: Integer;
  //a: TVector2;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSpriteDelta_Movement), NetMessage^.m_cbSize, MsgID_Delta_Movement) then
    Exit;

  DeltaMov := PMsg_ServerSpriteDelta_Movement(NetMessage^.m_pData)^;

  // Older than Heartbeat Drop the Packet
  if not DemoPlayer.Active and (DeltaMov.ServerTick < LastHeartBeatCounter) then
  begin
    Exit;
  end;
  i := DeltaMov.Num;

  if (i < 1) or (i > MAX_SPRITES) then
    Exit;
  if not Sprite[i].Active then
    Exit;

  //a := Vec2Subtract(SpriteParts.Pos[i], DeltaMov.Pos);

  SpriteParts.Pos[i] := DeltaMov.Pos;
  SpriteParts.Velocity[i] := DeltaMov.Velocity;

  Sprite[i].Control.MouseAimY := DeltaMov.MouseAimY;
  Sprite[i].Control.MouseAimX := DeltaMov.MouseAimX;

  DecodeKeys(Sprite[i], DeltaMov.Keys16);
end;

procedure ClientHandleDelta_MouseAim(NetMessage: PSteamNetworkingMessage_t);
var
  i: Integer;
  DeltaMouse: TMsg_ServerSpriteDelta_MouseAim;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSpriteDelta_MouseAim), NetMessage^.m_cbSize, MsgID_Delta_MouseAim) then
    Exit;

  DeltaMouse := PMsg_ServerSpriteDelta_MouseAim(NetMessage^.m_pData)^;

  i := DeltaMouse.Num;
  if (i < 1) or (i > MAX_SPRITES) then
    Exit;
  if not Sprite[i].Active then
    Exit;

  Sprite[i].Control.MouseAimY := DeltaMouse.MouseAimY;
  Sprite[i].Control.MouseAimX := DeltaMouse.MouseAimX;

  if Sprite[i].Position = POS_PRONE then
    Sprite[i].BodyApplyAnimation(Prone, 1)
  else
    Sprite[i].BodyApplyAnimation(Aim, 1);

  Sprite[i].Weapon.FireIntervalPrev := 0;
  Sprite[i].Weapon.FireIntervalCount := 0;
end;

procedure ClientHandleDelta_Weapons(NetMessage: PSteamNetworkingMessage_t);
var
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSpriteDelta_Weapons), NetMessage^.m_cbSize, MsgID_Delta_Weapons) then
    Exit;

  i := PMsg_ServerSpriteDelta_Weapons(NetMessage^.m_pData)^.Num;

  if (i < 1) or (i > MAX_SPRITES) then
    Exit;
  if not Sprite[i].Active then
    Exit;

  Sprite[i].ApplyWeaponByNum(
    PMsg_ServerSpriteDelta_Weapons(NetMessage^.m_pData)^.WeaponNum, 1);
  Sprite[i].ApplyWeaponByNum(
    PMsg_ServerSpriteDelta_Weapons(NetMessage^.m_pData)^.SecondaryWeaponNum, 2);
  Sprite[i].Weapon.AmmoCount :=
    PMsg_ServerSpriteDelta_Weapons(NetMessage^.m_pData)^.AmmoCount;

  if (i = MySprite) and not Sprite[MySprite].DeadMeat then
    ClientSpriteSnapshot;
end;

procedure ClientHandleDelta_Helmet(NetMessage: PSteamNetworkingMessage_t);
var
  DeltaHelmet: TMsg_ServerSpriteDelta_Helmet;
  i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerSpriteDelta_Helmet), NetMessage^.m_cbSize, MsgID_Delta_Helmet) then
    Exit;

  DeltaHelmet := PMsg_ServerSpriteDelta_Helmet(NetMessage^.m_pData)^;

  i := DeltaHelmet.Num;

  if (i < 1) or (i > MAX_SPRITES) then
    Exit;
  if not Sprite[i].Active then
    Exit;

  // helmet chop
  if (DeltaHelmet.WearHelmet = 0) then
  begin
    CreateSpark(Sprite[i].Skeleton.Pos[12],
      SpriteParts.Velocity[i], 6, i, 198);
    PlaySound(SFX_HEADCHOP, Sprite[i].Skeleton.Pos[12]);
  end;

  Sprite[i].WearHelmet := DeltaHelmet.WearHelmet;
end;

procedure ClientHandleClientSpriteSnapshot_Dead(NetMessage: PSteamNetworkingMessage_t);
begin
  if not VerifyPacket(sizeof(TMsg_ClientSpriteSnapshot_Dead), NetMessage^.m_cbSize, MsgID_ClientSpriteSnapshot_Dead) then
    Exit;
  if FreeCam = 0 then
    CameraFollowSprite := PMsg_ClientSpriteSnapshot_Dead(NetMessage^.m_pData)^.CameraFocus;
end;

end.

unit NetworkServerBullet;

interface

{$IFDEF SERVER}
uses
  Steam;
{$ENDIF}

procedure ServerBulletSnapshot(i: Byte; {$IFDEF SERVER}ToNum: Byte;{$ENDIF} Forced: Boolean);
{$IFDEF SERVER}
procedure ServerHandleBulletSnapshot(NetMessage: PSteamNetworkingMessage_t);
{$ENDIF}

implementation

uses
  {$IFDEF SERVER}
  Server, Vector, Math, PolyMap, Calc, Sprites, Weapons, Constants, NetworkUtils, 
  {$ELSE}Client,{$ENDIF} 
  Demo, Net, Game, Bullets;


procedure ServerBulletSnapshot(i: Byte; {$IFDEF SERVER}ToNum: Byte;{$ENDIF} Forced: Boolean);
var
  BulletMsg: TMsg_BulletSnapshot;
  {$IFDEF SERVER}
  j: Integer;
  {$ENDIF}
begin
  // SERVER BULLETS SNAPSHOT
  BulletMsg.Header.ID := MsgID_BulletSnapshot;
  BulletMsg.Owner := Bullet[i].Owner;
  BulletMsg.WeaponNum := Bullet[i].OwnerWeapon;
  BulletMsg.Pos := BulletParts.Pos[i];
  BulletMsg.Velocity := BulletParts.Velocity[i];
  BulletMsg.Seed := Bullet[i].Seed;
  BulletMsg.Forced := Forced;

  {$IFDEF SERVER}
  if not Forced then
    if (Sprite[BulletMsg.owner].Weapon.AmmoCount > 0) and
      (Bullet[i].Style <> BULLET_STYLE_FRAGNADE) and
      (Bullet[i].Style <> BULLET_STYLE_CLUSTERNADE) and
      (Bullet[i].Style <> BULLET_STYLE_CLUSTER) then
        Dec(Sprite[BulletMsg.owner].Weapon.AmmoCount);

  for j := 1 to MAX_SPRITES do
    if Sprite[j].Active and
       (Sprite[j].Player.ControlMethod = HUMAN) and ((j <> Bullet[i].Owner) or Forced) then
       if (ToNum = 0) or (j = ToNum) then
        if BulletCanSend(Bulletparts.Pos[i].X, Bulletparts.Pos[i].Y,
         Sprite[j].Player.Camera, Bulletparts.Velocity[i].X) or Forced then
      begin
        UDP.SendData(BulletMsg, sizeof(BulletMsg), Sprite[j].Player.peer, k_nSteamNetworkingSend_Unreliable);
      end;
  {$ELSE}
  DemoRecorder.SaveRecord(BulletMsg, sizeof(BulletMsg));
  {$ENDIF}
end;

{$IFDEF SERVER}
procedure ServerHandleBulletSnapshot(NetMessage: PSteamNetworkingMessage_t);
var
  BulletSnap: TMsg_ClientBulletSnapshot;
  Player: TPlayer;
  p, d: Integer;
  k: Single;
  i: Integer;
  OnStatGun: Boolean;
  FailedBulletCheck: Boolean;
  WeaponIndex: SmallInt;
  Style: Byte;
  a, b, bx: TVector2;
  BStraight: TVector2;
  BNorm: TVector2;
  BulletSpread: Single;
  RandSeedSave: Cardinal;
begin
  if not VerifyPacket(sizeof(TMsg_ClientBulletSnapshot), NetMessage^.m_cbSize, MsgID_BulletSnapshot) then
    Exit;

  BulletSnap := PMsg_ClientBulletSnapshot(NetMessage^.m_pData)^;
  Player := TPlayer(NetMessage^.m_nConnUserData);
  p := Player.SpriteNum;

  Inc(MessagesASecNum[p]);

  Sprite[p].Player.PingTicksB := ServerTickCounter - BulletSnap.ClientTicks;
  if Sprite[p].Player.PingTicksB < 0 then
    Sprite[p].Player.PingTicksB := 0;
  if Sprite[p].Player.PingTicksB > MAX_OLDPOS then
    Sprite[p].Player.PingTicksB := MAX_OLDPOS;

  WeaponIndex := WeaponNumToIndex(BulletSnap.WeaponNum);
  if WeaponIndex = -1 then
    Exit;

  Style := Guns[WeaponIndex].BulletStyle;

  // Check for duplicated bullets
  // Using a ringbuffer of saved references of old ones
  FailedBulletCheck := False;
  for i := 0 to Sprite[p].BulletCheckAmount do
  begin
    if Sprite[p].BulletCheck[i] = BulletSnap.Seed then
    begin
      FailedBulletCheck := True;
      Break;
    end;
  end;
  if FailedBulletCheck then
  begin
    // ignore duplicate packet
    Exit;
  end
  else
  begin
    if Sprite[p].BulletCheckIndex > BULLETCHECKARRAYSIZE - 1 then
    begin
      Sprite[p].BulletCheckIndex := 0;
    end;
    if Sprite[p].BulletCheckAmount < BULLETCHECKARRAYSIZE - 1 then
    begin
      Inc(Sprite[p].BulletCheckAmount);
    end;
    Sprite[p].BulletCheck[Sprite[p].BulletCheckIndex] := BulletSnap.Seed;
    Inc(Sprite[p].BulletCheckIndex);
  end;

  // spec kill: spectators NEVER send bullet snapshots
  if Sprite[p].IsSpectator() then
    Exit;

  // Disable knife-cheat kick for now, until we have timestamped
  // packets, and will do a time-based comparison to sprite snapshot
  // with info about switching primary to secondary
  {if (BulletSnap.Style <> Sprite[p].Weapon.BulletStyle) then
    if ((BulletSnap.Style = 13) and
      (Sprite[p].Weapon.BulletStyle <> 11) and
      (Sprite[p].Weapon.Num <> NoWeapon.Num)) then
    begin
      KickPlayer(p, True, KICK_CHEAT, DAY, 'Knife Cheat');
      Exit
    end;}

  if (Style = BULLET_STYLE_THROWNKNIFE) and (WeaponActive[KNIFE] = 0) then
  begin
    if KickPlayer(p, True, KICK_CHEAT, DAY, 'Knife-Spawn Cheat') then
      Exit;
  end;

  if Style = BULLET_STYLE_M2 then
  begin
    OnStatGun := False;
    for i := 1 to MAX_THINGS do
      if (Thing[i].Style = OBJECT_STATIONARY_GUN) {Stat Gun} and (OnStatGun = False) then
        if Distance(Sprite[p].Skeleton.Pos[1].x, Sprite[p].Skeleton.Pos[1].y,
           Thing[i].Skeleton.pos[1].x, Thing[i].Skeleton.pos[1].y) < STAT_RADIUS * 2 then
          OnStatGun := True;
    if not OnStatGun then
    begin
      // KickPlayer(p, True, KICK_CHEAT, DAY, 'StatGun Cheat');
      Exit;
    end;
  end;

  CheckOutOfBounds(BulletSnap.Pos.X, BulletSnap.Pos.Y);

  if (Style < BULLET_STYLE_PLAIN) or (Style > BULLET_STYLE_M2) then
    Exit;

  if (Style <> BULLET_STYLE_FRAGNADE) and (Style <> BULLET_STYLE_PUNCH) and
     (Style <> BULLET_STYLE_CLUSTERNADE) and (Style <> BULLET_STYLE_CLUSTER) and
     (Style <> BULLET_STYLE_THROWNKNIFE) and (Style <> BULLET_STYLE_M2) and
     (Sprite[p].LastWeaponStyle <> Style) then
    Exit;

  if (OldBulletSnapshotMsg[p].WeaponNum = BulletSnap.WeaponNum) and
     (OldBulletSnapshotMsg[p].Pos.X = BulletSnap.Pos.X) and
     (OldBulletSnapshotMsg[p].Pos.Y = BulletSnap.Pos.Y) and
     (OldBulletSnapshotMsg[p].Velocity.X = BulletSnap.Velocity.X) and
     (OldBulletSnapshotMsg[p].Velocity.Y = BulletSnap.Velocity.Y) then
    Exit;

  if (Style <> BULLET_STYLE_FRAGNADE) and (Style <> BULLET_STYLE_CLUSTERNADE) and
     (Style <> BULLET_STYLE_CLUSTER) and (Style <> BULLET_STYLE_THROWNKNIFE) and
     (Style <> BULLET_STYLE_M2) then
    if Vec2Length(BulletSnap.Velocity) >
       Sprite[p].LastWeaponSpeed + 10 * Guns[WeaponIndex].InheritedVelocity then
      Exit;

  a.x := Sprite[p].Skeleton.Pos[15].X - (BulletSnap.Velocity.X / 1.33);
  a.y := Sprite[p].Skeleton.Pos[15].Y - 2 - (BulletSnap.Velocity.Y / 1.33);
  b := Vec2Subtract(a, BulletSnap.Pos);

  if (Style <> BULLET_STYLE_FRAGNADE) and (Style <> BULLET_STYLE_FLAME) and
     (Style <> BULLET_STYLE_CLUSTERNADE) and (Style <> BULLET_STYLE_CLUSTER) and
     (Style <> BULLET_STYLE_THROWNKNIFE) and (Style <> BULLET_STYLE_M2) then
    if Vec2Length(b) > 366 then
      Exit;

  if MapChangeCounter = 999999999 then
    Exit;

  if BulletTime[p] > MainTickCounter then
    BulletTime[p] := 0;

  if (Style <> BULLET_STYLE_FRAGNADE) and (Style <> BULLET_STYLE_PUNCH) and
     (Style <> BULLET_STYLE_CLUSTERNADE) and (Style <> BULLET_STYLE_CLUSTER) and
     (Style <> BULLET_STYLE_THROWNKNIFE) and (Style <> BULLET_STYLE_M2) then
  begin
    if Sprite[p].Weapon.Ammo > 1 then
      if (MainTickCounter - BulletTime[p]) <
        ((Sprite[p].LastWeaponFire) * 0.85) then
        Inc(BulletWarningCount[p])
      else
        BulletWarningCount[p] := 0;

    if Sprite[p].Weapon.Ammo = 1 then
      if (MainTickCounter - BulletTime[p]) <
        ((Sprite[p].LastWeaponReload) * 0.9) then
        Inc(BulletWarningCount[p])
      else
        BulletWarningCount[p] := 0;

    if BulletWarningCount[p] > 2 then
      Exit;

    BulletTime[p] := MainTickCounter;
  end;

  if GrenadeTime[p] > MainTickCounter then
    GrenadeTime[p] := 0;

  if (Style = BULLET_STYLE_FRAGNADE) or (Style = BULLET_STYLE_CLUSTERNADE) then
  begin
    if MainTickCounter - GrenadeTime[p] < 6 then
      Exit;

    GrenadeTime[p] := MainTickCounter;
  end;

  if Style = BULLET_STYLE_THROWNKNIFE then
  begin
    if sv_warnings_knifecheat.Value = 69 then
    begin
      if (not KnifeCan[p]) or (Sprite[p].Weapon.BulletStyle <> BULLET_STYLE_THROWNKNIFE) or
        (Sprite[p].Weapon.BulletStyle <> BULLET_STYLE_PUNCH) then
      begin
        Inc(Sprite[p].Player.KnifeWarnings);
        if Sprite[p].Player.KnifeWarnings = 3 then
        begin
          MainConsole.Console('** DETECTED KNIFE CHEATING FROM ' +
            Sprite[p].Player.Name + ' **', SERVER_MESSAGE_COLOR);
          KickPlayer(p, True, KICK_CHEAT, DAY, 'Knife Throw Cheat');
        end;
      end;
    end;
    KnifeCan[p] := False;
  end;

  a := BulletSnap.Pos;
  b := BulletSnap.Velocity;
  k := Sprite[p].LastWeaponHM;

  if Style = BULLET_STYLE_PUNCH then
    k := Guns[NOWEAPON].HitMultiply;

  if Style = BULLET_STYLE_CLUSTER then
    if Sprite[p].TertiaryWeapon.AmmoCount = 0 then
      Exit;

  if (Style = BULLET_STYLE_FRAGNADE) or (Style = BULLET_STYLE_CLUSTERNADE) then
  begin
    k := Guns[FRAGGRENADE].HitMultiply;
    if Sprite[p].TertiaryWeapon.AmmoCount = 0 then
      Exit;
    if Sprite[p].TertiaryWeapon.AmmoCount > 0 then
      Dec(Sprite[p].TertiaryWeapon.AmmoCount);
  end;

  if Style = BULLET_STYLE_THROWNKNIFE then
  begin
    k := Guns[THROWNKNIFE].HitMultiply;
    Sprite[p].BodyApplyAnimation(Stand, 1);
  end;

  if Style = BULLET_STYLE_M2 then
    k := Guns[M2].HitMultiply;

  CreateBullet(a, b, BulletSnap.WeaponNum, p, 255, k, True, True, BulletSnap.Seed);

  BulletSpread := Guns[WeaponIndex].BulletSpread;

  if BulletSnap.WeaponNum = Guns[EAGLE].Num then  // Desert Eagle pellets
  begin
    // Undo the bullet spread used on the first pellet that was sent in order to
    // get the "straight" bullet vector. Then re-apply the the same randomness
    RandSeedSave := RandSeed;
    RandSeed := BulletSnap.Seed;

    BStraight.x := b.x - (Random * 2 - 1) * BulletSpread;
    BStraight.y := b.y - (Random * 2 - 1) * BulletSpread;

    bx.x := BStraight.x + (Random * 2 - 1) * BulletSpread;
    bx.y := BStraight.y + (Random * 2 - 1) * BulletSpread;

    RandSeed := RandSeedSave;

    Vec2Normalize(BNorm, BStraight);
    a.x := a.x - Sign(BStraight.x) * Abs(BNorm.y) * 3.0;
    a.y := a.y + Sign(BStraight.y) * Abs(BNorm.x) * 3.0;

    CreateBullet(a, bx, BulletSnap.WeaponNum, p, 255, k, False, True);
  end
  else if Style = BULLET_STYLE_SHOTGUN then  // SPAS-12 pellets
  begin
    // Undo the bullet spread used on the first pellet that was sent in order to
    // get the "straight" bullet vector. Then re-apply the the same randomness
    RandSeedSave := RandSeed;
    RandSeed := BulletSnap.Seed;
    BStraight.x := b.x - (Random * 2 - 1) * BulletSpread;
    BStraight.y := b.y - (Random * 2 - 1) * BulletSpread;

    for d := 0 to 4 do  // Remaining 5 pellets
    begin
      bx.x := BStraight.x + (Random * 2 - 1) * BulletSpread;
      bx.y := BStraight.y + (Random * 2 - 1) * BulletSpread;
      CreateBullet(a, bx, BulletSnap.WeaponNum, p, 255, k, False, True);
    end;

    RandSeed := RandSeedSave;
  end;

  if (Style <> BULLET_STYLE_FRAGNADE) and (Style <> BULLET_STYLE_CLUSTERNADE) and
     (Style <> BULLET_STYLE_CLUSTER) and (Style <> BULLET_STYLE_THROWNKNIFE) and
     (Style <> BULLET_STYLE_M2) then
  begin
    Sprite[p].Weapon.FireIntervalPrev := 1;
    Sprite[p].Weapon.FireIntervalCount := 1;
    Sprite[p].Control.Fire := True;
  end;
end;
{$ENDIF}

end.

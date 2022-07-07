unit NetworkClientBullet;

interface

uses
  // delphi and system units
  SysUtils, Classes, Math,

  // helper units
  Vector,

  // opensoldat units
  Steam, Net, Sprites, Weapons, Constants, NetworkServerBullet, Demo;

procedure ClientSendBullet(i: Byte);
procedure ClientHandleBulletSnapshot(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  Client, Game, Bullets, NetworkUtils;

var
  OldBulletSnapshotMsg: array[1..MAX_SPRITES] of TMsg_BulletSnapshot;

procedure ClientSendBullet(i: Byte);
var
  BulletMsg: TMsg_ClientBulletSnapshot;
begin
  BulletMsg.Header.ID := MsgID_BulletSnapshot;
  BulletMsg.WeaponNum := Bullet[i].OwnerWeapon;
  BulletMsg.Pos := BulletParts.Pos[i];
  BulletMsg.Velocity := BulletParts.Velocity[i];
  BulletMsg.ClientTicks := ClientTickCount;
  BulletMsg.Seed := Bullet[i].Seed;

  UDP.SendData(BulletMsg, sizeof(BulletMsg), k_nSteamNetworkingSend_Unreliable);

  if DemoRecorder.Active then
    ServerBulletSnapshot(i, False);
end;

procedure ClientHandleBulletSnapshot(NetMessage: PSteamNetworkingMessage_t);
var
  BulletSnap: TMsg_BulletSnapshot;
  a, b, bx: TVector2;
  BStraight: TVector2;
  BNorm: TVector2;
  hm: Single;
  i, k, pa, c, d: Integer;
  WeaponIndex: SmallInt;
  Style: Byte;
  BulletSpread: Single;
begin
  if not VerifyPacket(sizeof(BulletSnap), NetMessage^.m_cbSize, MsgID_BulletSnapshot) then
    Exit;

  BulletSnap := PMsg_BulletSnapshot(NetMessage^.m_pData)^;

  if (BulletSnap.Owner < 1) or (BulletSnap.Owner > MAX_SPRITES) then
    Exit;

  if not BulletSnap.Forced then
    if (OldBulletSnapshotMsg[BulletSnap.Owner].WeaponNum = BulletSnap.WeaponNum) and
       (OldBulletSnapshotMsg[BulletSnap.Owner].Pos.X = BulletSnap.Pos.X) and
       (OldBulletSnapshotMsg[BulletSnap.Owner].Pos.Y = BulletSnap.Pos.Y) and
       (OldBulletSnapshotMsg[BulletSnap.Owner].Velocity.X = BulletSnap.Velocity.X) and
       (OldBulletSnapshotMsg[BulletSnap.Owner].Velocity.Y = BulletSnap.Velocity.Y) then
      Exit;

  WeaponIndex := WeaponNumToIndex(BulletSnap.WeaponNum);
  if WeaponIndex = -1 then
    Exit;

  Style := Guns[WeaponIndex].BulletStyle;

  a := BulletSnap.Pos;
  b := BulletSnap.Velocity;

  //FIXME (falcon): Also serialize HitMultiply for CreateBullet()
  // on the other side, how the hell it works now? (because it does)
  hm := Sprite[BulletSnap.Owner].Weapon.HitMultiply;
  if Style = BULLET_STYLE_FRAGNADE then
    hm := Guns[FRAGGRENADE].HitMultiply;

  i := CreateBullet(a, b, BulletSnap.WeaponNum, BulletSnap.Owner, 255, hm, false, true);

    Bullet[i].OwnerPingTick := Sprite[BulletSnap.Owner].Player.PingTicks +
      PingTicksAdd;
    pa := Sprite[MySprite].Player.PingTicks + Bullet[i].OwnerPingTick;
    Bullet[i].PingAdd := pa;
    Bullet[i].PingAddStart := pa;
    if not BulletSnap.Forced then
    begin
      BulletSpread := Guns[WeaponIndex].BulletSpread;

      if BulletSnap.WeaponNum = Guns[EAGLE].Num then  // Desert Eagle pellets
      begin
        // Undo the bullet spread used on the first pellet that was sent in order to
        // get the "straight" bullet vector. Then re-apply the the same randomness
        RandSeed := BulletSnap.Seed;
        BStraight.x := b.x - (Random * 2 - 1) * BulletSpread;
        BStraight.y := b.y - (Random * 2 - 1) * BulletSpread;

        bx.x := BStraight.x + (Random * 2 - 1) * BulletSpread;
        bx.y := BStraight.y + (Random * 2 - 1) * BulletSpread;

        Vec2Normalize(BNorm, BStraight);
        a.x := a.x - Sign(BStraight.x) * Abs(BNorm.y) * 3.0;
        a.y := a.y + Sign(BStraight.y) * Abs(BNorm.x) * 3.0;

        k := CreateBullet(a, bx, BulletSnap.WeaponNum, BulletSnap.Owner, 255, i, false, true);

        if (MySprite > 0) and (BulletSnap.Owner > 0) then
          for c := 1 to pa do
            if Bullet[k].Active then
            begin
              BulletParts.DoEulerTimeStepFor(k);
              Bullet[k].Update;
              if not Bullet[k].Active then
                break;
            end;
      end
      else if Style = BULLET_STYLE_SHOTGUN then  // SPAS-12 pellets
      begin
        // Undo the bullet spread used on the first pellet that was sent in order to
        // get the "straight" bullet vector. Then re-apply the the same randomness
        RandSeed := BulletSnap.Seed;
        BStraight.x := b.x - (Random * 2 - 1) * BulletSpread;
        BStraight.y := b.y - (Random * 2 - 1) * BulletSpread;

        for d := 0 to 4 do  // Remaining 5 pellets
        begin
          bx.x := BStraight.x + (Random * 2 - 1) * BulletSpread;
          bx.y := BStraight.y + (Random * 2 - 1) * BulletSpread;
          k := CreateBullet(a, bx, BulletSnap.WeaponNum, BulletSnap.Owner, 255, hm, False, True);

          if (MySprite > 0) and (BulletSnap.Owner > 0) then
            for c := 1 to pa do
              if Bullet[k].Active then
              begin
                BulletParts.DoEulerTimeStepFor(k);
                Bullet[k].Update;
                if not Bullet[k].Active then
                  break;
              end;
        end;
      end;

    if (Style <> BULLET_STYLE_FRAGNADE) and (Style <> BULLET_STYLE_CLUSTERNADE) and
       (Style <> BULLET_STYLE_CLUSTER) and (Style <> BULLET_STYLE_THROWNKNIFE) and
       (Style <> BULLET_STYLE_M2) then
    begin
      Sprite[BulletSnap.Owner].Fire();
    end;
  end;

  if Bullet[i].Active then
    if (MySprite > 0) and (BulletSnap.Owner > 0) then
      for c := 1 to pa do begin
        BulletParts.DoEulerTimeStepFor(i);
        Bullet[i].Update;
        if not Bullet[i].Active then
          break;
      end;

  // stat gun
  if not BulletSnap.Forced then
    if Style = BULLET_STYLE_M2 then
        for i := 1 to MAX_THINGS do
          if (Thing[i].Active) and (Thing[i].Style = OBJECT_STATIONARY_GUN) then
            Thing[i].CheckStationaryGunCollision(true);

  OldBulletSnapshotMsg[BulletSnap.Owner] := BulletSnap;
end;

end.

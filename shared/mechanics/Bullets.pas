unit Bullets;

interface

uses
  Parts, MapFile, PolyMap, Net, Weapons, Constants,
  Vector, Sprites, Things;

type TBullet = object
    Active: Boolean;
    {$IFNDEF SERVER}
    HasHit: Boolean;
    {$ENDIF}
    Style: Byte;
    Num: SmallInt;
    Owner: Byte;
    OwnerWeapon: Byte;
    TimeOutReal: Single;
    TimeOut, TimeOutPrev: SmallInt;
    HitMultiply, HitMultiplyPrev: Single;
    VelocityPrev: TVector2;
    Whizzed: Boolean;
    OwnerPingTick: Byte;
    HitBody: Byte;
    HitSpot: TVector2;
    Tracking: Byte;
    ImageStyle: Byte;
    Initial: TVector2;
    StartUpTime, RicochetCount, DegradeCount: Integer;
    Seed: Word;
    ThingCollisions: array of TThingCollision;
    SpriteCollisions: Set of 1..32;
    {$IFDEF SERVER}
    DontCheat: Boolean;
    {$ELSE}
    PingAdd, PingAddStart: SmallInt;
    {$ENDIF}
  public
    procedure Update;
    {$IFNDEF SERVER}
    procedure Render(TimeElapsed: Extended);
    {$ENDIF}
    procedure Kill;
    function CheckMapCollision(X, Y: Single): TVector2;
    function CheckSpriteCollision(lasthitdist: Single): TVector2;
    function CheckThingCollision(lasthitdist: Single): TVector2;
    function CheckColliderCollision(lasthitdist: Single): TVector2;
    procedure Hit(T: Integer; SpriteHit: Integer = 0; Where: Integer = 0);
    procedure ExplosionHit(Typ, SpriteHit, Where: Integer);
    procedure CheckOutOfBounds;
    function FilterSpritesByDistance(var SpriteIndexes: TSpriteIndexes): Integer;
    function TargetableSprite(i: Integer): Boolean;
    function GetComparableSpriteDistance(i: Integer): Single;
    function GetSpriteCollisionPoint(i: Integer): TVector2;
    function GetWeaponIndex: Byte;
  end;

const
  HIT_TYPE_WALL        = 1;
  HIT_TYPE_BLOOD       = 2;
  HIT_TYPE_EXPLODE     = 3;
  HIT_TYPE_FRAGNADE    = 4;
  HIT_TYPE_THING       = 5;
  HIT_TYPE_CLUSTERNADE = 6;
  HIT_TYPE_CLUSTER     = 7;
  HIT_TYPE_FLAK        = 8;
  HIT_TYPE_BODYHIT     = 9;
  HIT_TYPE_RICOCHET    = 10;

  function CreateBullet(sPos, sVelocity: TVector2; sNum: Byte;
    sOwner: Integer; N: Byte; HitM: Single; Net, MustCreate: Boolean; Seed: LongInt = -1): Integer;
  {$IFDEF SERVER}
  function ServerCreateBullet(sPos, sVelocity: TVector2; sNum: Byte;
    sOwner: Integer; N: Byte; HitM: Single; Net: Boolean): Integer;
  {$ELSE}
  function CanHitSpray(Victim: Integer; Attacker: Integer): Boolean;
  procedure CalculateRecoil(px: Single; py: Single; var cx: Single;
    var cy: Single; da: Single);
  procedure HitSpray();
  {$ENDIF}
  function BulletCanSend(X, Y: Single; i: Integer; vX: Single): Boolean;

  implementation

  uses
    {$IFNDEF SERVER}
    Gfx, Sound, GameRendering, Sparks, Util,
    NetworkClientBullet, Demo, ClientGame,
    {$ELSE}
    NetworkServerBullet,
    {$ENDIF}
    {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} SysUtils, Calc, Math, Game, TraceLog;

function CreateBullet(sPos, sVelocity: TVector2; sNum: Byte;
  sOwner: Integer; N: Byte; HitM: Single; Net, MustCreate: Boolean; Seed: LongInt = -1): Integer;
var
  i, j: Integer;
  Mass: Single;
  WeaponIndex: SmallInt;
  sStyle: Byte;
begin
  Result := -1;

  {$IFDEF SERVER}
  Trace('CreateBullet ' + IntToStr(sNum) + ' ' + IntToStr(sOwner) + ' ' + IntToStr(N));
  {$ENDIF}

  {$IFNDEF SERVER}
  if DemoPlayer.Active then
    if not MustCreate then
      Exit;
  {$ENDIF}

  WeaponIndex := WeaponNumToIndex(sNum);
  sStyle := Guns[WeaponIndex].BulletStyle;

  if not MustCreate and (sOwner > 0) then
  begin
    if {$IFNDEF SERVER}(sOwner <> MySprite) and {$ENDIF} (Sprite[sOwner].Player.ControlMethod <> BOT) and
       ((Sprite[sOwner].Weapon.FireInterval > FIREINTERVAL_NET)
        or (((sStyle = BULLET_STYLE_FRAGNADE) or (sStyle = BULLET_STYLE_CLUSTERNADE)) {$IFNDEF SERVER}and False{$ENDIF})) then
    begin
      {$IFNDEF SERVER}if BulletCanSend(sPos.X, sPos.Y, CameraFollowSprite, sVelocity.X) then{$ENDIF}
      begin
        Exit;
      end;
    end;
  end;

  {$IFDEF SERVER}
  Trace('CreateBullet 2');
  {$ENDIF}

  if N = 255 then
  begin
    for i := 1 to MAX_BULLETS + 1 do
    begin
      if i = MAX_BULLETS + 1 then
      begin
        Result := -1;
        Exit;
      end;
      if not Bullet[i].Active then
        Break;
    end;
  end
  else
  begin
    i := N;
  end;

  // i is now the active sprite
  {$IFDEF SERVER}
  Trace('CreateBullet 3');
  {$ENDIF}
  // activate sprite
  Bullet[i].Active := True;
  {$IFNDEF SERVER}
  Bullet[i].HasHit := False;
  {$ENDIF}
  Bullet[i].Style := sStyle;
  Bullet[i].Num := i;
  Bullet[i].Owner := sOwner;
  Bullet[i].TimeOut := Guns[WeaponIndex].Timeout;
  {$IFNDEF SERVER} // TODO: Check if this should be used also in server
  Bullet[i].TimeOutPrev := Guns[WeaponIndex].Timeout;
  {$ENDIF}
  Bullet[i].HitMultiply := HitM;
  {$IFNDEF SERVER} // TODO: Check if this should be used also in server
  Bullet[i].HitMultiplyPrev := HitM;
  {$ENDIF}
  Bullet[i].Whizzed := False;

  if {$IFNDEF SERVER}(sOwner = CameraFollowSprite) or{$ENDIF}
    (sStyle = BULLET_STYLE_FLAMEARROW) or (sStyle = BULLET_STYLE_FLAME) then
    Bullet[i].Whizzed := True;

  if Sprite[sOwner].Player.ControlMethod = HUMAN then
    Bullet[i].OwnerPingTick := Sprite[sOwner].Player.PingTicksB {$IFDEF SERVER} + PingTicksAdd{$ENDIF}
  else
    Bullet[i].OwnerPingTick := 0;

  Bullet[i].OwnerWeapon := sNum;
  Bullet[i].HitBody := 0;
  Bullet[i].HitSpot.X := 0;
  Bullet[i].HitSpot.Y := 0;
  Bullet[i].Tracking := 0;

  {$IFNDEF SERVER} // TODO: Check if this should be used also in server
  if Sprite[sOwner].AimDistCoef < DEFAULTAIMDIST then
    Bullet[i].Tracking := 255;

  Bullet[i].ImageStyle := Sprite[sOwner].Weapon.BulletImageStyle;
  {$ELSE}
  Bullet[i].Initial := sPos;
  {$ENDIF}
  Bullet[i].StartUpTime := MainTickCounter;
  Bullet[i].RicochetCount := 0;
  {$IFNDEF SERVER} // TODO: Check if this should be used also in server
  Bullet[i].DegradeCount := 0;
  Bullet[i].PingAdd := 0;
  Bullet[i].PingAddStart := 0;
  {$ENDIF}

  if Seed = -1 then
  begin
    if Sprite[sOwner].BulletCount = High(Word) then
      Sprite[sOwner].BulletCount := 0
    else
      Inc(Sprite[sOwner].BulletCount);
    Seed := Sprite[sOwner].BulletCount;
  end;
  Bullet[i].Seed := Seed;

  {$IFDEF SERVER}
  Trace('CreateBullet 4');
  {$ENDIF}

  Mass := 1.0;

  if not MustCreate then
    if sStyle = BULLET_STYLE_FLAME then
    begin
      sPos.x := sPos.x + sVelocity.X;
      sPos.y := sPos.y + sVelocity.y;
    end;

  Bullet[i].Initial := sPos;

  {$IFDEF SERVER}
  Trace('CreateBullet 5');
  {$ENDIF}
  // activate sprite part
  BulletParts.CreatePart(sPos, sVelocity, Mass, i);

  {$IFNDEF SERVER}

  // SEND BULLLET THROUGH NETWORK
  if Net then
  begin
    if (sOwner = MySprite) and (ClientStopMovingCounter > 0) then
    begin
      if (Sprite[sOwner].Weapon.FireInterval > FIREINTERVAL_NET) or MustCreate or
         (Bullet[i].Style = BULLET_STYLE_FRAGNADE) or
         (Bullet[i].Style = BULLET_STYLE_CLUSTERNADE) or
         (Bullet[i].Style = BULLET_STYLE_CLUSTER) then
      begin
        ClientSendBullet(i);
        // Damage multiplier hack was here, they recalled ClientSendBullet
      end
      else if (Sprite[sOwner].Weapon.FireInterval <= FIREINTERVAL_NET) and
              (Sprite[sOwner].BurstCount = 0) and
              (MainTickCounter > LastForceClientSpriteSnapshotMovTick + FIREINTERVAL_NET) then
      begin
        ForceClientSpriteSnapshotMov := True;
        LastForceClientSpriteSnapshotMovTick := MainTickCounter;
      end;
    end;
  end;

  if (sOwner = MySprite) then
  begin
    if Bullet[i].Style = BULLET_STYLE_FRAGNADE then
    begin
      if WepStats[18].Shots = 0 then
        WepStats[18].TextureID := GFX_INTERFACE_NADE;
      WepStats[18].Shots := WepStats[18].Shots + 1;
      if WepStatsNum = 0 then
        WepStatsNum := 1;
      if WepStats[18].Name = '' then
        WepStats[18].Name := 'Grenade';
    end else if (Bullet[i].Style = BULLET_STYLE_CLUSTER) or
      (Bullet[i].Style = BULLET_STYLE_CLUSTERNADE) then
    begin
      if WepStats[19].Shots = 0 then
        WepStats[19].TextureID := GFX_INTERFACE_CLUSTER_NADE;
      WepStats[19].Shots := WepStats[19].Shots + 1;
      if WepStatsNum = 0 then
        WepStatsNum := 1;
      if WepStats[19].Name = '' then
        WepStats[19].Name := 'Clusters';
    end else if Bullet[i].Style = BULLET_STYLE_M2 then
    begin
      if WepStats[20].Shots = 0 then
        WepStats[20].TextureID := GFX_INTERFACE_GUNS_M2;
      WepStats[20].Shots := WepStats[20].Shots + 1;
      if WepStatsNum = 0 then
        WepStatsNum := 1;
      if WepStats[20].Name = '' then
        WepStats[20].Name := 'Stationary gun';
    end else if Bullet[i].Style = BULLET_STYLE_PUNCH then
    begin
      if WepStats[17].Shots = 0 then
        WepStats[17].TextureID := GFX_INTERFACE_GUNS_FIST;
      WepStats[17].Shots := WepStats[17].Shots + 1;
      if WepStatsNum = 0 then
        WepStatsNum := 1;
      if WepStats[17].Name = '' then
        WepStats[17].Name := 'Hands';
    end else
    begin
      j := Sprite[MySprite].Weapon.Num;
      if j = Guns[NOWEAPON].Num then
        j := 17;

      if WepStats[j].Shots = 0 then
      begin
        case j of
          0:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_SOCOM;
          1:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_DEAGLES;
          2:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_MP5;
          3:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_AK74;
          4:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_STEYR;
          5:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_SPAS;
          6:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_RUGER;
          7:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_M79;
          8:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_BARRETT;
          9:  WepStats[j].TextureID := GFX_INTERFACE_GUNS_MINIMI;
          10: WepStats[j].TextureID := GFX_INTERFACE_GUNS_MINIGUN;
          11: WepStats[j].TextureID := GFX_INTERFACE_GUNS_KNIFE;
          12: WepStats[j].TextureID := GFX_INTERFACE_GUNS_CHAINSAW;
          13: WepStats[j].TextureID := GFX_INTERFACE_GUNS_LAW;
          14: WepStats[j].TextureID := GFX_INTERFACE_GUNS_FLAMER;
          15: WepStats[j].TextureID := GFX_INTERFACE_GUNS_BOW;
          16: WepStats[j].TextureID := GFX_INTERFACE_GUNS_BOW;
        end;
      end;
      WepStats[j].Shots := WepStats[j].Shots + 1;
      if WepStatsNum = 0 then
        WepStatsNum := 1;
      if WepStats[j].Name = '' then
        WepStats[j].Name := Sprite[MySprite].Weapon.Name;
    end;
  end;
  {$ELSE}
  if Net then
  begin
    Trace('CreateBullet 6');

    if Sprite[sOwner].Weapon.FireInterval > FIREINTERVAL_NET then
      ServerBulletSnapshot(i, 0, False)
    else
      for j := 1 to MAX_SPRITES do
        if Sprite[j].Active and
           (Sprite[j].Player.ControlMethod = HUMAN) and (j <> Bullet[i].Owner) and
           (Sprite[j].Player.Port = 0) and Sprite[j].IsSpectator() then
          ServerBulletSnapshot(i, j, False);
  end;
  {$ENDIF}

  // Bullet[i].Update;
  {$IFDEF SERVER}
  Trace('CreateBullet 7');
  {$ENDIF}

  Result := i;
end;
{$IFDEF SERVER}
function ServerCreateBullet(sPos, sVelocity: TVector2; sNum: Byte;
  sOwner: Integer; N: Byte; HitM: Single; Net: Boolean): Integer;
var
  i: Integer;
begin
  Result := -1;

  if (sOwner <= 0) or (sOwner >= MAX_SPRITES) then
    Exit;

  i := CreateBullet(sPos, sVelocity, sNum, sOwner, N, HitM, Net, True, 0);

  Bullet[i].OwnerPingTick := 0;
  Bullet[i].DontCheat := True;

  if Net then
    ServerBulletSnapshot(i, 0, True);

  Result := i;
end;
{$ENDIF}

function BulletCanSend(X, Y: Single; i: Integer; vX: Single): Boolean;
var
  SX, SY: Single;
begin
  Trace('BulletCanSend');
  Result := False;

  if (i > MAX_PLAYERS) or (i < 0) then
    Exit;

  if i = 0 then
  begin
    Result := True;
    Exit;
  end;

  SX := Spriteparts.Pos[i].X -
    ((Spriteparts.Pos[i].X - Sprite[i].Control.MouseAimX) / 2);
  SY := Spriteparts.Pos[i].Y -
    ((Spriteparts.Pos[i].Y - Sprite[i].Control.MouseAimY) / 2);
  {$IFDEF SERVER}
  if (X > (SX - MAX_GAME_WIDTH)) and (X < (SX + MAX_GAME_WIDTH)) and
    (Y > (SY - 480)) and (Y < (SY + 480)) then
    Result := True;
  if (X > (SX - 1640)) and (X < (SX + 1640)) and (Y > (SY - 680)) and
    (Y < (SY + 680)) then
    if ((SX < X) and (vX < 0)) or ((SX > X) and (vX > 0)) then
      Result := True;
  {$ELSE}
  if (X > (SX - GameWidth)) and (X < (SX + GameWidth)) and
    (Y > (SY - GameHeight)) and (Y < (SY + GameHeight))
    then Result := True;

  if (X > (SX - 1640)) and (X < (SX + 1640)) and
    (Y > (SY - 680)) and (Y < (SY + 680)) then
    if ((SX < X) and (vX < 0)) or ((SX > X) and (vX > 0))
      then Result := True;
  {$ENDIF}
end;
{$IFNDEF SERVER}
function CanHitSpray(Victim: Integer; Attacker: Integer): Boolean;
begin
  Result := False;

  if Victim = MySprite then
  begin
    if (Victim = Attacker) or (sv_friendlyfire.Value) or
       Sprite[Victim].IsSolo() or
       Sprite[Victim].IsNotInSameTeam(Sprite[Attacker]) then
    begin
      Result := True;
    end;
  end;
end;

procedure HitSpray();
var
  Bink: SmallInt;
begin
  Bink := Sprite[MySprite].Weapon.Bink;
  if Bink > 0 then
    HitSprayCounter := CalculateBink(HitSprayCounter, Bink);
end;

procedure CalculateRecoil(px: Single; py: Single; var cx: Single;
  var cy: Single; da: Single);
var
  dx, dy, Radius, Alpha, DisplacementX: Single;
  //M: TVector2;
begin
  DisplacementX := 0;
  // If cursor is on right side displace recoil circle to left,
  // otherwise to right
  if (cx > px) then  // Test to find out best Result for displacement_x
  begin
    px := px - DisplacementX;
  end
  else
  begin
    px := px + DisplacementX;
  end;

  // Calculate delta x/y and radius
  dx := cx - px;
  dy := cy - py;
  Radius := Sqrt(dx * dx + dy * dy);

  // Depending on is cursor is on left/right side do different things
  if (dx > 0) then
  begin
    Alpha := ArcTan(dy / dx);
    Alpha := Alpha + da;

    if Alpha > pi / 2 then
    begin
      Alpha := pi / 2;
    end;
  end
  else
  begin
    Alpha := ArcTan(dy / dx) + pi;
    Alpha := Alpha - da;
    if (Alpha < pi / 2) then
    begin
      Alpha := pi / 2;
    end;
  end;

  // Calculate new x/y for cursor
  // cx := (px + Cos(alpha) * radius);

  // If OpenSoldat uses a coordinate system where y < 0 at top instead of
  // bottom, this needs subtraction instead of addition
  // cy := (py - Sin(alpha) * radius);

  //M.X := (dx / Sprite[CameraFollowSprite].AimDistCoef) * 3;
  //M.Y := (dy / Sprite[CameraFollowSprite].AimDistCoef) * 3;

  cx := px + ((dx + Cos(alpha) * radius) /
    Sprite[CameraFollowSprite].AimDistCoef) *
    (Sprite[CameraFollowSprite].AimDistCoef / 2);
  cy := py + ((dy + Sin(alpha) * radius) /
    Sprite[CameraFollowSprite].AimDistCoef) *
    (Sprite[CameraFollowSprite].AimDistCoef / 2);


  // Lastly make sure that cursor is still at the same side as it was first
  if (dx > 0) then
  begin
    if (cx <= px) then
    begin
      cx := (px + 0.0001); // We add a really small value like 0.001 to
                           // distinguish that the cursos is on right side
                           // (if it was right in the middle it'd be possible to
                           // interpret as it being on left or right side)
    end;
  end
  else
  begin
    if (cx >= px) then
    begin
      cx := (px - 0.0001);
    end;
  end;
end;
{$ENDIF}

// TBULLET
procedure TBullet.Update;
var
  OldV, a: TVector2;
  dist: Single;
  OldP, OldOP: TVector2;
  HitP, HitP2, HitP3: TVector2;
begin
  {$IFDEF SERVER}
  Trace('TBullet.Update');
  {$ENDIF}

  TimeOutPrev := TimeOut;
  HitMultiplyPrev := HitMultiply;
  VelocityPrev := BulletParts.Velocity[Num];

  OldV := BulletParts.Velocity[Num];
  OldP := BulletParts.Pos[Num];
  OldOP := BulletParts.OldPos[Num];
  dist := -1;
  HitP2.x := 0;
  HitP3.x := 0;

  CheckOutOfBounds;

  // check collision with map
  if Style <> BULLET_STYLE_FRAGNADE then
    HitP := CheckMapCollision(BulletParts.Pos[Num].X, BulletParts.Pos[Num].Y)
  else
  begin
    HitP := CheckMapCollision(BulletParts.Pos[Num].X, BulletParts.Pos[Num].Y - 2);
    HitP := CheckMapCollision(BulletParts.Pos[Num].X, BulletParts.Pos[Num].Y);
  end;

  if not Active then
  begin
    a := Vec2Subtract(HitP, OldOP);
    dist := Vec2Length(a);

    BulletParts.Velocity[Num] := OldV;
    Dec(RicochetCount);
    BulletParts.Pos[Num] := OldP;
    BulletParts.OldPos[Num] := OldOP;
  end;

  // check if hit collider
  HitP2 := CheckColliderCollision(dist);

  if not Active then
  begin
    if HitP2.x = 0 then
      a := Vec2Subtract(HitP, OldOP)
    else
      a := Vec2Subtract(HitP2, OldOP);

    dist := Vec2Length(a);
    BulletParts.Velocity[Num] := OldV;
    BulletParts.Pos[Num] := OldP;
    BulletParts.OldPos[Num] := OldOP;
  end;

  // check if hit sprites
  HitP3 := CheckSpriteCollision(dist);

  if not Active then
  begin
    if HitP3.x = 0 then
    begin
      if HitP2.x = 0 then
        a := Vec2Subtract(HitP, OldOP)
      else
        a := Vec2Subtract(HitP2, OldOP);
    end
    else
      a := Vec2Subtract(HitP3, OldOP);

    dist := Vec2Length(a);
  end;

  // check if hit things
  HitP := CheckThingCollision(dist);

  // count Time Out
  Dec(TimeOut);
  if TimeOut = 0 then
  begin
    case Style of
      BULLET_STYLE_PLAIN, BULLET_STYLE_SHOTGUN, BULLET_STYLE_FLAME,
      BULLET_STYLE_PUNCH, BULLET_STYLE_ARROW, BULLET_STYLE_CLUSTERNADE,
      BULLET_STYLE_KNIFE, BULLET_STYLE_THROWNKNIFE:
        Kill;
      BULLET_STYLE_FRAGNADE, BULLET_STYLE_M79, BULLET_STYLE_FLAMEARROW, BULLET_STYLE_LAW:
        begin
          Hit(HIT_TYPE_FRAGNADE);
          Kill;
        end;
      BULLET_STYLE_CLUSTER:
        begin
          Hit(HIT_TYPE_CLUSTER);
          Kill;
        end;
      BULLET_STYLE_M2:
        begin
          Hit(HIT_TYPE_FLAK);
          Kill;
        end;
    end;  // case
  end;  // TimeOut = 0

  // lose power on distance
  if TimeOut mod 6 = 0 then
  begin
    if (OwnerWeapon <> Guns[BARRETT].Num) and
       (OwnerWeapon <> Guns[M79].Num) and
       (OwnerWeapon <> Guns[KNIFE].Num) and
       (OwnerWeapon <> Guns[LAW].Num) then
    begin
      a := Vec2Subtract(Initial, BulletParts.Pos[Num]);
      dist := Vec2Length(a);

      if degradecount = 0 then
      begin
        if dist > 500 then
        begin
          HitMultiply := HitMultiply * 0.5;
          Inc(degradecount);
        end;
      end
      else if degradecount = 1 then
      begin
        if dist > 900 then
        begin
          HitMultiply := HitMultiply * 0.5;
          Inc(degradecount);
        end;
      end;
    end;
  end;

  {$IFNDEF SERVER}
  // Bullet Tracking
  if Owner = MySprite then
    if Tracking = 255 then
      Tracking := Owner;

  if Tracking = MySprite then
  begin
    if Sprite[Owner].Position <> POS_STAND then
    begin
      CameraX := Bulletparts.Pos[Num].X + 5 * Bulletparts.Velocity[Num].X;
      CameraY := Bulletparts.Pos[Num].Y + 5 * Bulletparts.Velocity[Num].Y;
    end else
    begin
      Tracking := 0;
    end;
  end;

  // his sound
  if (TimeOut = BULLET_TIMEOUT - 25) and (Style <> BULLET_STYLE_SHOTGUN) then
    PlaySound(SFX_BULLETBY, BulletParts.Pos[Num]);

  // whiizz above head
  if not Whizzed then
    if Style <> BULLET_STYLE_PUNCH then
      if CameraFollowSprite > 0 then
        if (BulletParts.Pos[Num].X > Spriteparts.Pos[CameraFollowSprite].X - 200) and
           (BulletParts.Pos[Num].X < Spriteparts.Pos[CameraFollowSprite].X + 200) and
           (BulletParts.Pos[Num].Y > Spriteparts.Pos[CameraFollowSprite].Y - 350) and
           (BulletParts.Pos[Num].Y < Spriteparts.Pos[CameraFollowSprite].Y + 100) then
        begin
          PlaySound(SFX_BULLETBY2 + Random(4), BulletParts.Pos[Num]);
          Whizzed := True;
        end;

  // fire for flaming arrow
  if Style = BULLET_STYLE_FLAMEARROW then
  begin
    // smoke
    if Random(2) = 0 then
      CreateSpark(BulletParts.Pos[Num], Vector2(0, -0.5), 37, Num, 40);

    if Random(2) = 0 then
      CreateSpark(BulletParts.Pos[Num], Vector2(0, -0.5), 36, Num, 40);
  end;

  // law missile smoke
  if Style = BULLET_STYLE_LAW then
  begin
    // smoke
    CreateSpark(BulletParts.Pos[Num], Vector2(0, -1.5), 59, Num, 50);
    if Random(2) = 0 then
      CreateSpark(BulletParts.Pos[Num], BulletParts.Velocity[Num], 2, Num, 5);
  end;
  {$ENDIF}

  // flame
  if Style = BULLET_STYLE_FLAME then
    Bulletparts.Forces[Num].Y := Bulletparts.Forces[Num].Y - 0.15;

  {$IFNDEF SERVER}
  // bleed
  if HitBody > 0 then
    if Random(5) = 0 then
    begin
      CreateSpark(BulletParts.Pos[Num],
        Vector2(BulletParts.Velocity[Num].X * 0.5,
        BulletParts.Velocity[Num].Y * 0.5), 4, Owner, 90);
    end;
  {$ENDIF}
end;

{$IFNDEF SERVER}
procedure TBullet.Render(TimeElapsed: Extended);
var
  T: ^TGfxSpriteArray;
  BulletPos, BulletVel: TVector2;
  _p, _p2, _Scala, a, b: TVector2;
  roto, alfa: Single;
  grenvel: Single = 0.0;
  ox, oy: Single;
  SinusVar: Single;
begin
  T := @Textures;
  BulletPos := BulletParts.Pos[Num];

  if sv_realisticmode.Value then
    if (Owner > 0) and (Owner < MAX_SPRITES + 1) then
      if Sprite[Owner].Active then
        if Sprite[Owner].Visible = 0 then
          if Map.RayCast(BulletPos, Sprite[MySprite].Skeleton.Pos[9], grenvel, GameWidth, True) then
            Exit;

  BulletVel := BulletParts.Velocity[Num];
  SinusVar := Sin(TimeOutReal + 5.1 * TimeElapsed);

  case Style of
    BULLET_STYLE_PLAIN:  // bullet
      if TimeOutReal < BULLET_TIMEOUT - 2 then
      begin
        if (ImageStyle <> Guns[EAGLE].BulletImageStyle) and
           (ImageStyle <> Guns[MP5].BulletImageStyle) and
           (ImageStyle <> Guns[AK74].BulletImageStyle) and
           (ImageStyle <> Guns[STEYRAUG].BulletImageStyle) and
           (ImageStyle <> Guns[RUGER77].BulletImageStyle) and
           (ImageStyle <> Guns[BARRETT].BulletImageStyle) and
           (ImageStyle <> Guns[M249].BulletImageStyle) and
           (ImageStyle <> Guns[MINIGUN].BulletImageStyle) and
           (ImageStyle <> Guns[COLT].BulletImageStyle) then
          ImageStyle := Guns[COLT].BulletImageStyle;

        _p.x := BulletPos.X + BulletVel.X;
        _p.y := BulletPos.Y + BulletVel.Y;
        _p2.x := _p.x - BulletVel.X;
        _p2.y := _p.y - BulletVel.Y;
        _scala.x := Vec2Length(BulletVel) / BULLETTRAIL;
        _scala.y := 1;
        Roto := -Angle2Points(_p, _p2);

        alfa := ((HitMultiply * _scala.x * _scala.x) / 4.63) * 255;

        if alfa > 230 then
          alfa := 230;
        if alfa < 50 then
          alfa := 50;

        if PingAdd < 1 then
          GfxDrawSprite(T^[ImageStyle], _p.x, _p.y, _scala.x, _scala.y, 0, 0,
            Roto, RGBA($FFFFFF, Round(alfa)));

        if PingAdd > 0 then
        begin
          a.x := BulletPos.X - Initial.X;
          a.y := BulletPos.Y - Initial.Y;
          b.x := Vec2Length(a) * Min(1 / BULLETLENGTH, ((PingAdd + 2) / PingAddStart) / BULLETTRAIL);
          b.y := 1;

          if Active then
            alfa := alfa / 6
          else
            alfa := alfa / 4;
          GfxDrawSprite(T^[ImageStyle], _p.x, _p.y, b.x, b.y, 0, 0, Roto,
            RGBA($FFFFFF, Round(alfa)));
        end;

        if HitBody > 0 then
        begin
          if Trails = 1 then
            if TimeOutReal < BULLET_TIMEOUT - 7 then
            begin
              _p.x := BulletPos.X;
              _p.y := BulletPos.Y;
              _scala.x := Abs(Vec2Length(BulletVel) / 4);
              _scala.y := 1;
              GfxDrawSprite(T^[ImageStyle], _p.x, _p.y, _scala.x, _scala.y, 0, 0,
                Roto, RGBA($FFDDDD, BULLETALPHA div 2));
            end;
        end else
        begin
          if Trails = 1 then
          begin
            if TimeOutReal < BULLET_TIMEOUT - 7 then
            begin
              _p.x := BulletPos.X;
              _p.y := BulletPos.Y;
              _scala.x := Abs(Vec2Length(BulletVel) / 3.5);
              _scala.y := 1;
              GfxDrawSprite(T^[ImageStyle], _p.x, _p.y, _scala.x, _scala.y, 0, 0,
                Roto, RGBA($FFFFFF, BULLETALPHA div 2));
            end;
          end;
        end;
      end;

    BULLET_STYLE_FRAGNADE:
      begin
        grenvel := Vec2Length(BulletVel);
        if Trails = 1 then
          if TimeOutReal < GRENADE_TIMEOUT - 3 then
          begin
            if BulletVel.y > 0 then
              ox := -1
            else
              ox := 1;
            if BulletVel.x > 0 then
              oy := 1
            else
              oy := -1;

            _p.x := BulletPos.X + ox;
            _p.y := BulletPos.Y - 3 + oy;
            _p2.x := _p.x - BulletVel.X;
            _p2.y := _p.y - BulletVel.Y;
            Roto := -Angle2Points(_p, _p2);
            _scala.x := grenvel / 3;
            _scala.y := 1;
            GfxDrawSprite(T^[GFX_WEAPONS_BULLET], _p.x, _p.y, _scala.x, _scala.y,
              0, 0, Roto, RGBA($64FF64, Round(BULLETALPHA * 0.75)));
          end;

        _p.x := BulletPos.X - 1;
        _p.y := BulletPos.Y - 4;
        _p2.x := BulletParts.OldPos[Num].X - 1;
        _p2.y := BulletParts.OldPos[Num].Y - 4;
        GfxDrawSprite(T^[GFX_WEAPONS_FRAG_GRENADE], _p.x, _p.y);
      end;

    BULLET_STYLE_SHOTGUN:
      if TimeOutReal < BULLET_TIMEOUT - 2 then
      begin
        _p.x := BulletPos.X + BulletVel.X;
        _p.y := BulletPos.Y + BulletVel.Y;
        _p2.x := _p.x - BulletVel.X;
        _p2.y := _p.y - BulletVel.Y;
        Roto := -Angle2Points(_p, _p2);
        GfxDrawSprite(T^[GFX_WEAPONS_SPAS_BULLET], _p.x, _p.y, 0, 0, Roto,
          RGBA($FFFFFF, 150));

        if Trails = 1 then
          if TimeOutReal < BULLET_TIMEOUT - 3 then
          begin
            _p.x := BulletPos.X;
            _p.y := BulletPos.Y;
            _scala.x := Abs(Vec2Length(BulletVel) / 9);
            _scala.y := 1;
            GfxDrawSprite(T^[GFX_WEAPONS_BULLET], _p.x, _p.y, _scala.x, _scala.y,
              0, 0, Roto, RGBA($FFFFFF, BULLETALPHA div 5));
          end;
      end;

    BULLET_STYLE_M79:
      if TimeOutReal < BULLET_TIMEOUT - 2 then
      begin
        _p.x := BulletPos.X;
        _p.y := BulletPos.Y + 1;
        _p2.x := _p.x - BulletVel.X;
        _p2.y := _p.y - BulletVel.Y;
        _scala.x := 1;
        _scala.y := 1;
        Roto := -Angle2Points(_p, _p2);
        GfxDrawSprite(T^[GFX_WEAPONS_M79_BULLET], _p.x, _p.y, _scala.x, _scala.y,
          0, 0, degtorad(TimeOutReal * 6), RGBA($FFFFFF, 252));

        if Trails = 1 then
          if TimeOutReal < BULLET_TIMEOUT - 4 then
          begin
            if BulletVel.y > 0 then
              ox := -1
            else
              ox := 1;
            if BulletVel.x > 0 then
              oy := 1
            else
              oy := -1;

            _p.x := BulletPos.X + ox;
            _p.y := BulletPos.Y + oy;
            _scala.x := Abs(Vec2Length(BulletVel) / 4);
            _scala.y := 1.3;
            GfxDrawSprite(T^[GFX_WEAPONS_BULLET], _p.x, _p.y, _scala.x, _scala.y,
              0, 0, Roto, RGBA($FFFF55, BULLETALPHA));
          end;
      end;

    BULLET_STYLE_FLAME:
      begin
        _p.x := BulletPos.X - 8;
        _p.y := BulletPos.Y - 17;
        if (TimeOutReal > 0) and (TimeOutReal <= FLAMER_TIMEOUT) then
          GfxDrawSprite(T^[GFX_SPARKS_FLAMES_EXPLODE16 - Trunc(TimeOutReal / 2)],
            _p.x, _p.y);
      end;

    BULLET_STYLE_ARROW:
      if TimeOutReal < BULLET_TIMEOUT - 2 then
      begin
        _p.x := BulletPos.X + BulletVel.X;
        _p.y := BulletPos.Y + BulletVel.Y;
        _p2.x := _p.x - BulletVel.X;
        _p2.y := _p.y - BulletVel.Y;
        Roto := -Angle2Points(_p, _p2);
        GfxDrawSprite(T^[GFX_WEAPONS_ARROW], _p.x, _p.y, 0, 0, Roto);

        if Trails = 1 then
          if TimeOutReal > ARROW_RESIST then
          begin
            _p.x := BulletPos.X;
            _p.y := BulletPos.Y;
            _scala.x := Abs(Vec2Length(BulletVel) / 3);
            _scala.y := 1;
            GfxDrawSprite(T^[GFX_WEAPONS_BULLET], _p.x, _p.y, _scala.x, _scala.y,
              0, 0, Roto, RGBA($FFFFFF, BULLETALPHA div 7));
          end;
      end;

    BULLET_STYLE_FLAMEARROW:
      if TimeOutReal < BULLET_TIMEOUT - 2 then
      begin
        _p.x := BulletPos.X + BulletVel.X;
        _p.y := BulletPos.Y + BulletVel.Y;
        _p2.x := _p.x - BulletVel.X;
        _p2.y := _p.y - BulletVel.Y;
        Roto := -Angle2Points(_p, _p2);
        GfxDrawSprite(T^[GFX_WEAPONS_ARROW], _p.x, _p.y, 0, 0, Roto);
      end;

    BULLET_STYLE_CLUSTERNADE:
      begin
        _p.x := BulletPos.X;
        _p.y := BulletPos.Y - 3;
        GfxDrawSprite(T^[GFX_WEAPONS_CLUSTER_GRENADE], _p.x, _p.y, 0, 0,
          degtorad(TimeOutReal * 5) * iif(BulletVel.X < 0, -1, 1));
      end;

    BULLET_STYLE_CLUSTER:
      begin
        _p.x := BulletPos.X;
        _p.y := BulletPos.Y - 2;
        GfxDrawSprite(T^[GFX_WEAPONS_CLUSTER], _p.x, _p.y);
      end;

    BULLET_STYLE_LAW:
      if TimeOutReal < BULLET_TIMEOUT - 2 then
      begin
        _p.x := BulletPos.X + BulletVel.X;
        _p.y := BulletPos.Y + BulletVel.Y;
        _p2.x := _p.x - BulletVel.X;
        _p2.y := _p.y - BulletVel.Y;
        Roto := -Angle2Points(_p, _p2);
        GfxDrawSprite(T^[GFX_WEAPONS_MISSILE], _p.x, _p.y, 0, 0, Roto);

        if Trails = 1 then
        begin
          if TimeOutReal < BULLET_TIMEOUT - 7 then
          begin
            _p.x := BulletPos.X;
            _p.y := BulletPos.Y;
            _scala.x := Abs(Vec2Length(BulletVel) / 3);
            _scala.y := 1;
            GfxDrawSprite(T^[GFX_WEAPONS_BULLET], _p.x, _p.y, _scala.x, _scala.y,
              0, 0, Roto, RGBA($FFFFFF, BULLETALPHA div 5));
          end;
        end;
      end;

    BULLET_STYLE_THROWNKNIFE:
      begin
        _p.x := BulletPos.X + BulletVel.X;
        _p.y := BulletPos.Y + BulletVel.Y;
        Roto := TimeOutReal / PI;

        if BulletVel.X >= 0 then
          GfxDrawSprite(T^[GFX_WEAPONS_KNIFE], _p.x, _p.y, 4, 1, Roto)
        else
          GfxDrawSprite(T^[GFX_WEAPONS_KNIFE2], _p.x, _p.y, 4, 1, -Roto);
      end;

    BULLET_STYLE_M2:
      if TimeOutReal < M2BULLET_TIMEOUT - 2 then
      begin
        _p.x := BulletPos.X + BulletVel.X;
        _p.y := BulletPos.Y + BulletVel.Y;
        _p2.x := _p.x - BulletVel.X;
        _p2.y := _p.y - BulletVel.Y;
        _scala.x := Abs(Vec2Length(BulletVel) / BULLETTRAIL);
        _scala.y := 1.2;
        Roto := -Angle2Points(_p, _p2);
        GfxDrawSprite(T^[GFX_WEAPONS_BULLET], _p.x, _p.y, _scala.x, _scala.y,
          0, 0, Roto, RGBA($FFBF77, BULLETALPHA * 2));

        if Trails = 1 then
        begin
          if TimeOutReal < M2BULLET_TIMEOUT - 13 then
          begin
            _p.x := BulletPos.X;
            _p.y := BulletPos.Y;
            _scala.x := Abs(Vec2Length(BulletVel) / 3);
            _scala.y := 1;
            GfxDrawSprite(T^[GFX_WEAPONS_BULLET], _p.x, _p.y, _scala.x, _scala.y,
              0, 0, Roto, RGBA($FFFFFF, BULLETALPHA div 5));

            _scala.x := Abs(Vec2Length(BulletVel) / (SinusVar + 2.5));
            _scala.y := SinusVar;
            GfxDrawSprite(T^[GFX_WEAPONS_SMUDGE], _p.x, _p.y, _scala.x, _scala.y,
              0, 0, Roto, RGBA($FFFFFF, BULLETALPHA div 6));
          end;

        end;
      end;
  end;
end;
{$ENDIF}

procedure TBullet.Kill;
begin
  {$IFDEF SERVER}
  Trace('TBullet.Kill');
  {$ENDIF}

  Active := False;
  if Num > 0 then
    BulletParts.Active[Num] := False;
  SetLength(ThingCollisions, 0);
  SpriteCollisions := [];
end;

function TBullet.CheckMapCollision(X, Y: Single): TVector2;
var
  LargestVelocityComponent: Single;
  j, b, w, k, w2: Integer;
  Pos, Perp, Step, Temp, Temp2: TVector2;
  D: Single = 0.0;
  DetAcc: Integer;
  kx, ky: Integer;
  tempInt: Integer = 0;
  teamcol: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TBullet.CheckMapCollision');
  {$ENDIF}

  Pos := Default(TVector2);
  Perp := Default(TVector2);

  Result := Pos;

  // make step
  LargestVelocityComponent := Max(Abs(BulletParts.Velocity[Num].x),
                                  Abs(BulletParts.Velocity[Num].y));

  DetAcc := Trunc(LargestVelocityComponent / 2.5);

  if DetAcc = 0 then
    DetAcc := 1;
  Vec2Scale(Step, BulletParts.Velocity[Num], 1 / DetAcc);

  // make steps for accurate collision detection
  for b := 0 to DetAcc - 1 do
  begin
    Pos.X := X + b * Step.X;
    Pos.Y := Y + b * Step.Y;

    // iterate through maps sector polygons
    kx := Round(Pos.X / Map.SectorsDivision);
    ky := Round(Pos.Y / Map.SectorsDivision);
    if (kx < -Map.SectorsNum) or (kx > Map.SectorsNum) or
      (ky < -Map.SectorsNum) or (ky > Map.SectorsNum) then
    begin
      Kill;
      Exit;
    end;

    if High(Map.Sectors[kx, ky].Polys) > 0 then
      for j := 1 to High(Map.Sectors[kx, ky].Polys) do
      begin
        w := Map.Sectors[kx, ky].Polys[j];
        teamcol := TeamCollides(w, Sprite[Owner].Player.Team, True);
        if teamcol then
          if (Map.PolyType[w] <> POLY_TYPE_ONLY_PLAYER) and
            (Map.PolyType[w] <> POLY_TYPE_DOESNT) and
            (Map.PolyType[w] <> POLY_TYPE_ONLY_FLAGGERS) and
            (Map.PolyType[w] <> POLY_TYPE_NOT_FLAGGERS) and
            (Map.PolyType[w] <> POLY_TYPE_BACKGROUND) and
            (Map.PolyType[w] <> POLY_TYPE_BACKGROUND_TRANSITION) then
            if Map.PointInPolyEdges(Pos.X, Pos.y, w) then
            begin
              case Style of
                BULLET_STYLE_PLAIN, BULLET_STYLE_SHOTGUN, BULLET_STYLE_PUNCH,
                BULLET_STYLE_KNIFE, BULLET_STYLE_M2:
                  begin
                    BulletParts.OldPos[Num] := BulletParts.Pos[Num];
                    BulletParts.Pos[Num] := Vec2Subtract(Pos, BulletParts.Velocity[Num]);
                    Temp := BulletParts.Pos[Num];
                    Temp2 := BulletParts.Velocity[Num];

                    Perp := Vec2Subtract(BulletParts.Pos[Num], HitSpot);
                    D := Vec2Length(Perp);
                    // ricochet!
                    if D > 50.0 then
                    begin
                      Inc(RicochetCount);
                      Perp := Map.ClosestPerpendicular(w, BulletParts.Pos[Num], D, tempInt);
                      D := Vec2Length(Bulletparts.Velocity[Num]);
                      Vec2Normalize(Perp, Perp);
                      Vec2Scale(Perp, Perp, -D);

                      BulletParts.Velocity[Num].X :=
                        BulletParts.Velocity[Num].X * (25 / 35) + Perp.X * (10 / 35);
                      BulletParts.Velocity[Num].Y :=
                        BulletParts.Velocity[Num].Y * (25 / 35) + Perp.Y * (10 / 35);
                      BulletParts.Pos[Num] := Pos;
                      HitSpot := BulletParts.Pos[Num];

                      Vec2Normalize(Perp, BulletParts.Velocity[Num]);
                      Vec2Scale(Perp, Perp, D / 6);
                      BulletParts.OldPos[Num] := BulletParts.Pos[Num];
                      Pos.X := BulletParts.Pos[Num].X + Perp.X;
                      Pos.Y := BulletParts.Pos[Num].Y + Perp.Y;
                      kx := Round(Pos.X / Map.SectorsDivision);
                      ky := Round(Pos.Y / Map.SectorsDivision);
                      if (kx > -Map.SectorsNum) and (kx < Map.SectorsNum) and
                         (ky > -Map.SectorsNum) and (ky < Map.SectorsNum) then
                      begin
                        for k := 1 to High(Map.Sectors[kx, ky].Polys) do
                        begin
                          w2 := Map.Sectors[kx, ky].Polys[k];
                          if (Map.PolyType[w2] <> POLY_TYPE_ONLY_PLAYER) and
                             (Map.PolyType[w2] <> POLY_TYPE_DOESNT) and
                             (Map.PolyType[w2] <> POLY_TYPE_ONLY_FLAGGERS) and
                             (Map.PolyType[w2] <> POLY_TYPE_NOT_FLAGGERS) and
                             (Map.PolyType[w2] <> POLY_TYPE_BACKGROUND) and
                             (Map.PolyType[w2] <> POLY_TYPE_BACKGROUND_TRANSITION) and
                            TeamCollides(w2, Sprite[Owner].Player.Team, True) then
                            if Map.PointInPolyEdges(Pos.X, Pos.y, w2) then
                            begin
                              Kill;
                              Break;
                            end;
                        end;
                      end;
                    end
                    else
                    begin
                      Kill;
                    end;

                    if Active then
                    begin
                      BulletParts.Pos[Num] := Temp;
                      Perp := BulletParts.Velocity[Num];
                      BulletParts.Velocity[Num] := Temp2;
                      Hit(HIT_TYPE_RICOCHET);
                      BulletParts.Pos[Num] := HitSpot;
                      BulletParts.Velocity[Num] := Perp;
                    end
                    else
                    begin
                      BulletParts.Pos[Num] := Temp;
                      Perp := BulletParts.Velocity[Num];
                      BulletParts.Velocity[Num] := Temp2;
                      Hit(HIT_TYPE_WALL);
                      BulletParts.Pos[Num] := HitSpot;
                      BulletParts.Velocity[Num] := Perp;
                    end;
                  end;
                BULLET_STYLE_ARROW:
                  begin
                    BulletParts.Pos[Num] :=
                      Vec2Subtract(Pos, BulletParts.Velocity[Num]);
                    BulletParts.Forces[Num].Y :=
                      BulletParts.Forces[Num].Y - BulletParts.Gravity;
                    if TimeOut > ARROW_RESIST then
                      TimeOut := ARROW_RESIST;
                    if TimeOut < 20 then
                      BulletParts.Forces[Num].Y :=
                        BulletParts.Forces[Num].Y + BulletParts.Gravity;
                  end;
                BULLET_STYLE_FRAGNADE, BULLET_STYLE_FLAME:
                  begin
                    {$IFNDEF SERVER}
                    // bounce sound
                    if Style = BULLET_STYLE_FRAGNADE then
                      if Vec2Length(BulletParts.Velocity[Num]) > 1.5 then
                        PlaySound(SFX_GRENADE_BOUNCE, SpriteParts.Pos[Num]);
                    {$ENDIF}

                    Perp := Map.ClosestPerpendicular(w, BulletParts.Pos[Num], D,
                      tempInt);

                    Vec2Normalize(Perp, Perp);
                    Vec2Scale(Perp, Perp, D);

                    BulletParts.Pos[Num] := Pos;
                    BulletParts.Velocity[Num] :=
                      Vec2Subtract(BulletParts.Velocity[Num], Perp);

                    Vec2Scale(BulletParts.Velocity[Num],
                      BulletParts.Velocity[Num], GRENADE_SURFACECOEF);

                    if Style = BULLET_STYLE_FLAME then
                      if TimeOut > 16 then
                        TimeOut := 16;
                  end;
                BULLET_STYLE_M79, BULLET_STYLE_FLAMEARROW, BULLET_STYLE_LAW:
                  begin
                    BulletParts.OldPos[Num] := BulletParts.Pos[Num];
                    BulletParts.Pos[Num] :=
                      Vec2Subtract(Pos, BulletParts.Velocity[Num]);
                    Temp := BulletParts.Pos[Num];
                    Temp2 := BulletParts.Velocity[Num];

                    Perp := Vec2Subtract(BulletParts.Pos[Num], HitSpot);
                    D := Vec2Length(Perp);
                    // ricochet!
                    if D > 50.0 then
                    begin
                      Inc(RicochetCount);
                      Perp := Map.ClosestPerpendicular(w, BulletParts.Pos[Num], D, tempInt);
                      D := Vec2Length(Bulletparts.Velocity[Num]);
                      Vec2Normalize(Perp, Perp);
                      Vec2Scale(Perp, Perp, -D);

                      BulletParts.Velocity[Num].X :=
                        BulletParts.Velocity[Num].X * (25 / 35) + Perp.X * (10 / 35);
                      BulletParts.Velocity[Num].Y :=
                        BulletParts.Velocity[Num].Y * (25 / 35) + Perp.Y * (10 / 35);
                      BulletParts.Pos[Num] := Pos;
                      HitSpot := BulletParts.Pos[Num];

                      Vec2Normalize(Perp, BulletParts.Velocity[Num]);
                      Vec2Scale(Perp, Perp, D / 6);

                      Pos.X := BulletParts.Pos[Num].X + Perp.X;
                      Pos.Y := BulletParts.Pos[Num].Y + Perp.Y;
                      kx := Round(Pos.X / Map.SectorsDivision);
                      ky := Round(Pos.Y / Map.SectorsDivision);
                      if (kx > -Map.SectorsNum) and (kx < Map.SectorsNum) and
                         (ky > -Map.SectorsNum) and (ky < Map.SectorsNum) then
                      begin
                        for k := 1 to High(Map.Sectors[kx, ky].Polys) do
                        begin
                          w2 := Map.Sectors[kx, ky].Polys[k];
                          if (Map.PolyType[w2] <> POLY_TYPE_ONLY_PLAYER) and
                             (Map.PolyType[w2] <> POLY_TYPE_DOESNT) and
                             (Map.PolyType[w2] <> POLY_TYPE_ONLY_FLAGGERS) and
                             (Map.PolyType[w2] <> POLY_TYPE_NOT_FLAGGERS) and
                             (Map.PolyType[w2] <> POLY_TYPE_BACKGROUND) and
                             (Map.PolyType[w2] <> POLY_TYPE_BACKGROUND_TRANSITION) and
                            TeamCollides(w2, Sprite[Owner].Player.Team, True) then
                            if Map.PointInPolyEdges(Pos.X, Pos.y, w2) then
                            begin
                              Kill;
                              Break;
                            end;
                        end;
                      end;
                    end
                    else
                    begin
                      Kill;
                    end;

                    if Active then
                    begin
                      BulletParts.Pos[Num] := Temp;
                      Perp := BulletParts.Velocity[Num];
                      BulletParts.Velocity[Num] := Temp2;
                      Hit(HIT_TYPE_RICOCHET);
                      BulletParts.Pos[Num] := HitSpot;
                      BulletParts.Velocity[Num] := Perp;
                    end
                    else
                    begin
                      BulletParts.Pos[Num] := Temp;
                      Perp := BulletParts.Velocity[Num];
                      BulletParts.Velocity[Num] := Temp2;
                      Hit(HIT_TYPE_EXPLODE);
                      BulletParts.Pos[Num] := HitSpot;
                      BulletParts.Velocity[Num] := Perp;
                    end;

                  end;
                BULLET_STYLE_CLUSTERNADE:
                  begin
                    Hit(HIT_TYPE_CLUSTERNADE);
                    Kill;
                  end;
                BULLET_STYLE_CLUSTER:
                  begin
                    Hit(HIT_TYPE_CLUSTER);
                    Kill;
                  end;
                BULLET_STYLE_THROWNKNIFE:
                  begin
                    BulletParts.Pos[Num] :=
                      Vec2Subtract(Pos, BulletParts.Velocity[Num]);

                    // create knife thing
                    {$IFDEF SERVER}
                    CreateThing(BulletParts.Pos[Num], Owner, OBJECT_COMBAT_KNIFE, 255);
                    {$ENDIF}

                    Hit(HIT_TYPE_WALL);
                    Kill;
                  end;
              end;  // case

              Result := Pos;
              Exit;
            end;  // PointinPolyEdges
      end;  // for j
  end;  // for b
end;

function TBullet.CheckSpriteCollision(lasthitdist: Single): TVector2;
const
  BodyPartsPriority: array[0..6] of Integer = (12, 11, 10, 6, 5, 4, 3);
var
  SpritesByDistance: TSpriteIndexes;
  SpriteCount, SpriteCounter: Integer;
  j: Integer;
  Pos, Norm, ColPos, a, Col: TVector2;
  BulletVelocity: TVector2;
  CandidateSkeleton: ^ParticleSystem;
  BodyPartPriorityIndex: Integer;
  BodyPartId: Integer;
  BodyPartOffset: TVector2;
  ButtstockPositionOffset, StartPoint, EndPoint: TVector2;
  Where: Byte;
  WeaponIndex: Byte;
  Dist, MinDist: Single;
  r: Integer;
  BulletPush: TVector2;
  PushTick: Integer;
  HitboxModifier: Single;
  Speed: Single;
  WasDead: Boolean;
  NoCollision: Byte;
  {$IFNDEF SERVER}
  ClothesShreadStyle: Byte;
  i: Integer;
  srv: Integer;
  {$ENDIF}
begin
  {$IFDEF SERVER}
  Trace('TBullet.CheckSpriteCollision ' + IntToStr(Style) + ' ' + IntToStr(Owner) + ' ' +
    IntToStr(OwnerWeapon) + ' ' + IntToStr(TimeOut) + ' ' + ' ' + IntToStr(Tracking));
  {$ENDIF}
  Result := Default(TVector2);

  if (Style = BULLET_STYLE_ARROW) and (TimeOut <= ARROW_RESIST) then Exit;

  a := Default(TVector2);
  Pos.x := 0;
  Result := Pos;

  {$IFNDEF SERVER}
  srv := 0;
  {$ENDIF}

  BulletVelocity := BulletParts.Velocity[Num];

  {$IFDEF SERVER}
  Trace('TBullet.CheckSpriteCollision 2');
  {$ENDIF}
  // Iterate through sprites
  if Style <> BULLET_STYLE_CLUSTERNADE then
  begin
    SpritesByDistance := Default(TSpriteIndexes);
    SpriteCount := FilterSpritesByDistance(SpritesByDistance);
    SpriteCounter := 0;

    while SpriteCounter < SpriteCount do
    begin
      SpriteCounter := SpriteCounter + 1;
      j := SpritesByDistance[SpriteCounter];

      Col := GetSpriteCollisionPoint(j);

      Where := 0;
      if Style <> BULLET_STYLE_FRAGNADE then
        r := PART_RADIUS
      else
        r := PART_RADIUS + 1;

      {$IFDEF SERVER}
      Trace('TBullet.CheckSpriteCollision 3 -- ' + IntToStr(j));
      {$ENDIF}

      CandidateSkeleton := @Sprite[j].Skeleton;

      // Pre-calculate some points if it's a melee weapon
      if (Style = BULLET_STYLE_PUNCH) or (Style = BULLET_STYLE_KNIFE) then
      begin
        Pos := BulletParts.Pos[Num];

        ButtstockPositionOffset := Sprite[Owner].GetHandsAimDirection();
        Vec2Scale(ButtstockPositionOffset, ButtstockPositionOffset, 4);

        StartPoint := Vec2Add(Sprite[Owner].Skeleton.Pos[15], ButtstockPositionOffset);
        EndPoint := Vec2Add(Pos, BulletVelocity);
      end
      else
      begin
        StartPoint := BulletParts.Pos[Num];
        EndPoint := Vec2Add(StartPoint, BulletVelocity);
      end;

      // Check for collision with the body parts
      MinDist := MaxSingle;

      for BodyPartPriorityIndex := 0 to High(BodyPartsPriority) do
      begin
        BodyPartId := BodyPartsPriority[BodyPartPriorityIndex];

        BodyPartOffset := Vec2Subtract(CandidateSkeleton.Pos[BodyPartId], SpriteParts.Pos[j]);
        ColPos := Vec2Add(Col, BodyPartOffset);

        // FIXME(skoskav): Offset player sprite 2px to the left because sprites are glitchy
        // like that (don't need to do it for melee weapons though because their offsets
        // cancel each other out)
        if (Style <> BULLET_STYLE_PUNCH) and (Style <> BULLET_STYLE_KNIFE) then
          ColPos.x := ColPos.x - 2;

        if LineCircleCollision(StartPoint, EndPoint, ColPos, r, Pos) then
        begin
          Dist := SqrDist(StartPoint, Pos);

          if Dist < MinDist then
          begin
            Where := BodyPartId;
            MinDist := Dist;
          end;
        end;
      end;

      {$IFDEF SERVER}
      Trace('TBullet.CheckSpriteCollision 4 ');
      {$ENDIF}

      if ((Style <> BULLET_STYLE_PUNCH) and
        (Style <> BULLET_STYLE_KNIFE)) or (j <> Owner) then
      begin
        if Where > 0 then
        begin
          // order collision
          if lasthitdist > -1 then
          begin
            a := Vec2Subtract(Pos, BulletParts.OldPos[Num]);
            dist := Vec2Length(a);

            if dist > lasthitdist then
            begin
              Break;
            end;
          end;

          Sprite[j].Brain.PissedOff := Owner;

          Norm := Vec2Subtract(Pos, Sprite[j].Skeleton.Pos[Where]);
          Vec2Scale(Norm, Norm, 1.3);
          Norm.y := -Norm.y;

          {$IFDEF SERVER}
          Trace('TBullet.CheckSpriteCollision 5 ' + IntToStr(Where));
          {$ENDIF}

          Result := Pos;

          NoCollision := Guns[WeaponNumToIndex(OwnerWeapon)].NoCollision;

          if (NoCollision and WEAPON_NOCOLLISION_ENEMY <> 0) and
              (Sprite[j].IsNotInSameTeam(Sprite[Owner]) ) then
            Continue;

          if (NoCollision and WEAPON_NOCOLLISION_TEAM <> 0) and
             (Sprite[j].IsInSameTeam(Sprite[Owner])) and (j <> Owner) then
            Continue;

          if (NoCollision and WEAPON_NOCOLLISION_SELF <> 0) and
             (Owner = j) then
            Continue;

          if Sprite[j].CeaseFireCounter < 0 then
          begin
            WeaponIndex := GetWeaponIndex();

            // Collision respond
            if not Sprite[j].DeadMeat then
            begin
              if (Style <> BULLET_STYLE_FRAGNADE) and
                (Style <> BULLET_STYLE_FLAME) and (Style <> BULLET_STYLE_ARROW) then
              begin
                Vec2Scale(BulletPush, BulletVelocity, Guns[WeaponIndex].Push);
                PushTick := Sprite[j].Player.PingTicks div 2 +
                  OwnerPingTick + 1;
                if PushTick > MAX_PUSHTICK then
                  PushTick := MAX_PUSHTICK;
                Sprite[j].NextPush[PushTick] := Vec2Add(
                  Sprite[j].NextPush[PushTick], BulletPush);
              end;
            end;

            case Style of
              BULLET_STYLE_PLAIN, BULLET_STYLE_SHOTGUN, BULLET_STYLE_PUNCH,
                BULLET_STYLE_KNIFE, BULLET_STYLE_M2:
                begin
                  BulletParts.Pos[Num] := Pos;

                  {$IFNDEF SERVER}
                  // Blood spark
                  if (sv_friendlyfire.Value) or
                     Sprite[Owner].IsSolo() or
                     Sprite[Owner].IsNotInSameTeam(Sprite[j]) or
                     (j = Owner) then
                  begin
                    Hit(HIT_TYPE_BLOOD);
                  end;

                  // Shake screen
                  if (Owner = MySprite) and (OwnerWeapon = Guns[CHAINSAW].Num) then
                  begin
                    CameraX := CameraX - 3 + Random(7);
                    CameraY := CameraY - 3 + Random(7);
                  end;

                  // Puff
                  Vec2Normalize(a, BulletVelocity);
                  Vec2Scale(a, a, 3);
                  a := Vec2Add(BulletParts.Pos[Num], a);
                  if r_maxsparks.Value > (MAX_SPARKS - 10) then
                    CreateSpark(a, a, 50, j, 31);

                  // Shread clothes
                  if r_maxsparks.Value > (MAX_SPARKS - 10) then
                  begin
                    if Where <= 4 then
                      ClothesShreadStyle := 49  // Piece of clothing with pants
                    else if Where <= 11 then
                      ClothesShreadStyle := 48  // Piece of clothing
                    else
                      ClothesShreadStyle := 0;

                    if ClothesShreadStyle > 0 then
                    begin
                      for i := 1 to 2 do
                      begin
                        if Random(8) = 0 then
                        begin
                          a.x := Sin(Random(100));
                          a.y := Cos(Random(100));
                          CreateSpark(Pos, a, ClothesShreadStyle, j, 120);
                        end;
                      end;
                    end;
                  end;

                  // play hit sound
                  if Sprite[j].Vest < 1 then
                  begin
                    if not Sprite[j].DeadMeat then
                      PlaySound(SFX_HIT_ARG + Random(3), BulletParts.Pos[Num])
                    else
                      PlaySound(SFX_DEAD_HIT, BulletParts.Pos[Num]);
                  end
                  else
                    PlaySound(SFX_VESTHIT, BulletParts.Pos[Num]);
                  {$ENDIF}

                  // Head, torso or leg hitbox modifier
                  if Where <= 4 then
                    HitboxModifier := Guns[WeaponNumToIndex(OwnerWeapon)].ModifierLegs
                  else if Where <= 11 then
                    HitboxModifier := Guns[WeaponNumToIndex(OwnerWeapon)].ModifierChest
                  else
                    HitboxModifier := Guns[WeaponNumToIndex(OwnerWeapon)].ModifierHead;

                  Speed := Vec2Length(BulletVelocity);

                  WasDead := Sprite[j].DeadMeat;

                  Sprite[j].HealthHit(
                    {$IFNDEF SERVER}srv * {$ENDIF}Speed * HitMultiply * HitboxModifier,
                    Owner, Where, Num, Norm);

                  {$IFNDEF SERVER}
                  // hit Spray
                  if CanHitSpray(j, Owner) then
                    HitSpray();
                  {$ENDIF}

                  {$IFDEF SERVER}
                  Trace('TBullet.CheckSpriteCollision a ');
                  {$ENDIF}

                  // drop weapon when punched
                  if Style = BULLET_STYLE_PUNCH then
                    if Sprite[j].IsSolo() or
                       (Sprite[j].IsNotSolo() and Sprite[j].IsNotInSameTeam(Sprite[Owner])) then
                      if (Sprite[j].Weapon.Num <> Guns[BOW].Num) and
                         (Sprite[j].Weapon.Num <> Guns[BOW2].Num) then
                        Sprite[j].BodyApplyAnimation(ThrowWeapon, 11);

                  HitBody := j;

                  // Pierce check and break to next sprite
                  if WasDead then
                  begin
                    Vec2Scale(BulletParts.Velocity[Num], BulletVelocity, 0.9);
                    BulletVelocity := BulletParts.Velocity[Num];
                    Hit(HIT_TYPE_BODYHIT);
                    Continue;
                  end;

                  if Sprite[j].DeadMeat or (Speed > 23) then
                  begin
                    Vec2Scale(BulletParts.Velocity[Num], BulletVelocity, 0.75);
                    BulletVelocity := BulletParts.Velocity[Num];
                    Hit(HIT_TYPE_BODYHIT);
                    Continue;
                  end;

                  if (Speed > 5) and (Speed / Guns[WeaponIndex].Speed >= 0.9) then
                  begin
                    Vec2Scale(BulletParts.Velocity[Num], BulletVelocity, 0.66);
                    BulletVelocity := BulletParts.Velocity[Num];
                    Hit(HIT_TYPE_BODYHIT);
                    Continue;
                  end;

                  // Destroy bullet
                  Kill;
                end;

              BULLET_STYLE_FRAGNADE:
                begin
                  if not Sprite[j].DeadMeat then
                  begin
                    Hit(HIT_TYPE_FRAGNADE, j, Where);
                    Kill;
                  end;
                end;

              BULLET_STYLE_ARROW:
                if TimeOut > ARROW_RESIST then
                begin
                  BulletParts.Pos[Num] := Vec2Subtract(Pos, BulletParts.Velocity[Num]);
                  BulletParts.Forces[Num].Y := BulletParts.Forces[Num].Y - BulletParts.Gravity;
                  if ((not sv_friendlyfire.Value) and
                    Sprite[Owner].IsNotSolo() and
                    Sprite[Owner].IsInSameTeam(Sprite[j])
                    {$IFNDEF SERVER}and (Num <> MySprite){$ENDIF}) or
                    (Sprite[j].BonusStyle = BONUS_FLAMEGOD) then
                  else
                    Hit(HIT_TYPE_BLOOD);
                  {$IFNDEF SERVER}
                  // play hit sound
                  if Sprite[j].Vest < 1 then
                  begin
                    if not Sprite[j].DeadMeat then
                      PlaySound(SFX_HIT_ARG + Random(3), BulletParts.Pos[Num])
                    else
                      PlaySound(SFX_DEAD_HIT, BulletParts.Pos[Num]);
                  end else
                    PlaySound(SFX_VESTHIT, BulletParts.Pos[Num]);
                  {$ENDIF}

                  {$IFDEF SERVER}
                  Trace('TBullet.CheckSpriteCollision b ');
                  {$ENDIF}

                  // Head, torso or leg hitbox modifier
                  if Where <= 4 then
                    HitboxModifier := Guns[WeaponNumToIndex(OwnerWeapon)].ModifierLegs
                  else if Where <= 11 then
                    HitboxModifier := Guns[WeaponNumToIndex(OwnerWeapon)].ModifierChest
                  else
                    HitboxModifier := Guns[WeaponNumToIndex(OwnerWeapon)].ModifierHead;

                  Speed := Vec2Length(BulletParts.Velocity[Num]);

                  Sprite[j].HealthHit({$IFNDEF SERVER}srv *{$ENDIF}
                    Speed *
                    HitMultiply * HitboxModifier, Owner, Where, Num, Norm);

                  if not Sprite[j].DeadMeat then
                    Sprite[j].Skeleton.Pos[Where] := a;

                  Kill;
                end;

              BULLET_STYLE_M79, BULLET_STYLE_FLAMEARROW, BULLET_STYLE_LAW:
                if not Sprite[j].DeadMeat then
                begin
                  Hit(HIT_TYPE_EXPLODE, j, Where);
                  BulletParts.Pos[Num] := Pos;
                  Kill;

                  Sprite[j].HealthHit({$IFNDEF SERVER}srv *{$ENDIF}
                    Vec2Length(BulletParts.Velocity[Num]) *
                    HitMultiply, Owner, Where, Num, Norm);

                  {$IFDEF SERVER}
                  Trace('TBullet.CheckSpriteCollision c ');
                  {$ENDIF}

                  if not Sprite[j].DeadMeat then
                    Sprite[j].Skeleton.Pos[Where] := a;
                end;

              BULLET_STYLE_FLAME:
                if Owner <> j then
                begin
                  BulletParts.Pos[Num] := Sprite[j].Skeleton.Pos[Where];
                  if not Sprite[j].DeadMeat then
                    BulletParts.Velocity[Num] := SpriteParts.Velocity[j]
                  else
                  begin
                    BulletParts.Velocity[Num].X := 0;
                    BulletParts.Velocity[Num].Y := 0;
                  end;

                  {$IFDEF SERVER}
                  if (TimeOut < 3) and (RicochetCount < 2) then
                  {$ELSE}
                  if (TimeOut < 2) and (RicochetCount < 1) then
                  {$ENDIF}
                  begin
                    if HitMultiply >= Guns[FLAMER].HitMultiply / 3 then
                    begin
                      TimeOut := FLAMER_TIMEOUT - 1;
                      Inc(RicochetCount);
                      a.x := -Spriteparts.Velocity[j].x;
                      a.y := -Spriteparts.Velocity[j].y;
                      CreateBullet(Sprite[j].Skeleton.Pos[Where], a, Guns[FLAMER].Num,
                        Owner, 255, 2 * HitMultiply / 3,
                        False, {$IFDEF SERVER}False{$ELSE}True{$ENDIF});
                    end;

                    if (Sprite[j].Health > -1) then
                      Sprite[j].HealthHit({$IFNDEF SERVER}srv *{$ENDIF}
                        HitMultiply, Owner, Where, Num, Norm);
                  end;
                end;

              BULLET_STYLE_CLUSTER:
                begin
                  {$IFDEF SERVER}
                  Trace('TBullet.CheckSpriteCollision f ');
                  {$ENDIF}

                  Hit(HIT_TYPE_CLUSTER, j, Where);

                  if not Sprite[j].DeadMeat then
                    Sprite[j].Skeleton.Pos[Where] := a;

                  Kill;
                end;

              BULLET_STYLE_THROWNKNIFE:
                begin
                  {$IFNDEF SERVER}
                  // Blood spark
                  if (sv_friendlyfire.Value) or
                     Sprite[Owner].IsSolo() or
                     Sprite[Owner].IsNotInSameTeam(Sprite[j]) or
                     (j = Owner) then
                  begin
                    Hit(HIT_TYPE_BLOOD);
                  end;

                  // Puff
                  Vec2Normalize(a, BulletParts.Velocity[Num]);
                  Vec2Scale(a, a, 3);
                  a := Vec2Add(BulletParts.Pos[Num], a);
                  if r_maxsparks.Value > (MAX_SPARKS - 10) then
                    CreateSpark(a, a, 50, j, 31);

                  // Shread clothes
                  if r_maxsparks.Value > (MAX_SPARKS - 10) then
                  begin
                    if Where <= 4 then
                      ClothesShreadStyle := 49  // Piece of clothing with pants
                    else if Where <= 11 then
                      ClothesShreadStyle := 48  // Piece of clothing
                    else
                      ClothesShreadStyle := 0;

                    if ClothesShreadStyle > 0 then
                    begin
                      for i := 1 to 2 do
                      begin
                        if Random(8) = 0 then
                        begin
                          a.x := Sin(Random(100));
                          a.y := Cos(Random(100));
                          CreateSpark(Pos, a, ClothesShreadStyle, j, 120);
                        end;
                      end;
                    end;
                  end;

                  // play hit sound
                  if not (Sprite[j].Num in SpriteCollisions) then
                  begin
                    Include(SpriteCollisions, Sprite[j].Num);

                    if Sprite[j].Vest < 1 then
                    begin
                      if not Sprite[j].DeadMeat then
                        PlaySound(SFX_HIT_ARG + Random(3), BulletParts.Pos[Num])
                      else
                        PlaySound(SFX_DEAD_HIT, BulletParts.Pos[Num]);
                    end else
                      PlaySound(SFX_VESTHIT, BulletParts.Pos[Num]);
                  end;
                  {$ENDIF}

                  {$IFDEF SERVER}
                  Trace('TBullet.CheckSpriteCollision g ');
                  {$ENDIF}

                  WasDead := Sprite[j].DeadMeat;

                  Sprite[j].HealthHit({$IFNDEF SERVER}srv *{$ENDIF}
                    Vec2Length(BulletParts.Velocity[Num]) *
                    HitMultiply * 0.01, Owner, Where, Num, Norm);

                  if not Sprite[j].DeadMeat then
                    Sprite[j].Skeleton.Pos[Where] := a;

                  if (not WasDead) or (sv_realisticmode.Value) then
                  begin
                    {$IFDEF SERVER}
                    // create knife thing
                    CreateThing(BulletParts.Pos[Num], Owner, OBJECT_COMBAT_KNIFE, 255);
                    {$ENDIF}
                    Kill;
                  end;
                end;
            end;  // case

            // Bullet is destroyed, so exit
            Exit;
          end;
        end;
      end;

      {$IFDEF SERVER}
      Trace('TBullet.CheckSpriteCollision 7 ');
      {$ENDIF}
    end;  // while SpriteCounter < SpriteCount
  end;
end;

function TBullet.CheckThingCollision(lasthitdist: Single): TVector2;
var
  i, j: Integer;
  StartPoint, EndPoint, Pos, ColPos, a: TVector2;
  Where: Byte;
  dist: Single;
  ThingVel: TVector2;
  VelDiff: TVector2;
  BulletPush: TVector2;
  NewIndex: Byte;
  SkipCollision: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TBullet.CheckThingCollision');
  {$ENDIF}

  Pos.x := 0;
  Result := Pos;

  // iterate through Things
  if (Style <> BULLET_STYLE_FRAGNADE) then
    for j := 1 to MAX_THINGS do
      if Thing[j].Active and
         (TimeOut < BULLET_TIMEOUT - 1) and
         Thing[j].CollideWithBullets and
         (Owner <> Thing[j].HoldingSprite) and
         (((sv_gamemode.Value = GAMESTYLE_INF) and (Thing[j].Style = OBJECT_BRAVO_FLAG)) or
          (sv_gamemode.Value <> GAMESTYLE_INF)) and
         (Thing[j].Style <> OBJECT_STATIONARY_GUN) then
      begin
        StartPoint := BulletParts.Pos[Num];
        EndPoint := Vec2Add(StartPoint, BulletParts.Velocity[Num]);

        Where := 0;

        for i := 1 to 2 do
        begin
          ColPos := Thing[j].Skeleton.Pos[i];

          if LineCircleCollision(StartPoint, EndPoint, ColPos, FLAG_PART_RADIUS, Pos) then
          begin
            Where := i;
            Break;
          end;
        end;

        if (Where = 1) or (Where = 2) then
        begin
          // order collision
          if lasthitdist > -1 then
          begin
            a := Vec2Subtract(Pos, BulletParts.OldPos[Num]);
            dist := Vec2Length(a);
            if dist > lasthitdist then
              Break;
          end;

          // Thing push cooldown from this bullet
          SkipCollision := False;

          for i := 0 to High(ThingCollisions) do
          begin
            if ThingCollisions[i].ThingNum = Thing[j].Num then
            begin
              if MainTickCounter < ThingCollisions[i].CooldownEnd then
              begin
                SkipCollision := True;
                Break;
              end;
            end;
          end;

          if SkipCollision then
            Break;

          // TODO: has high performance impact? -> do alloc like std vector
          NewIndex := Length(ThingCollisions);
          SetLength(ThingCollisions, NewIndex + 1);
          ThingCollisions[NewIndex] :=
            ThingCollision(Thing[j].Num, MainTickCounter + THING_COLLISION_COOLDOWN);

          // collision respond
          ThingVel := Vec2Subtract(Thing[j].Skeleton.Pos[Where], Thing[j].Skeleton.OldPos[Where]);
          VelDiff := Vec2Subtract(BulletParts.Velocity[Num], ThingVel);

          Vec2Scale(BulletPush, VelDiff, Guns[GetWeaponIndex()].Push * THING_PUSH_MULTIPLIER);

          Thing[j].Skeleton.Pos[Where] := Vec2Add(Thing[j].Skeleton.Pos[Where], BulletPush);

          Result := Pos;
          Thing[j].StaticType := False;

          case Style of
            BULLET_STYLE_PLAIN, BULLET_STYLE_FRAGNADE, BULLET_STYLE_SHOTGUN:
              begin
                Hit(HIT_TYPE_THING);
              end;
          end;

          Break;
        end;
      end;  // for j
end;

function TBullet.CheckColliderCollision(lasthitdist: Single): TVector2;
var
  j: Integer;
  {$IFNDEF SERVER}i: Integer;{$ENDIF}
  StartPoint, EndPoint, Pos, ColPos, a: TVector2;
  dist: Single;
begin
  {$IFDEF SERVER}
  Trace('TBullet.CheckColliderCollision');
  {$ENDIF}

  Pos.x := 0;
  Result := Pos;

  // iterate through colliders
  for j := 1 to 128 do
    if Map.Collider[j].Active then
    begin
      StartPoint := BulletParts.Pos[Num];
      EndPoint := Vec2Add(StartPoint, BulletParts.Velocity[Num]);
      // make steps for accurate collision detection

      ColPos.X := Map.Collider[j].X;
      ColPos.Y := Map.Collider[j].Y;


      if LineCircleCollision(StartPoint, EndPoint, ColPos, Map.Collider[j].Radius / 1.7, Pos) then
      begin
        // order collision
        if lasthitdist > -1 then
        begin
          a := Vec2Subtract(Pos, BulletParts.OldPos[Num]);
          dist := Vec2Length(a);
          if dist > lasthitdist then
          begin
            Break;
          end;
        end;

        case Style of
          BULLET_STYLE_PLAIN, BULLET_STYLE_SHOTGUN, BULLET_STYLE_PUNCH,
          BULLET_STYLE_KNIFE, BULLET_STYLE_THROWNKNIFE, BULLET_STYLE_M2:
            begin
              BulletParts.Pos[Num] :=
                Vec2Subtract(Pos, BulletParts.Velocity[Num]);

              {$IFNDEF SERVER}
              // dirt
              if r_maxsparks.Value > (MAX_SPARKS - 10) then
                for i := 1 to 2 do
                  if Random(4) = 0 then
                  begin
                    a.x := sin(Random(100));
                    a.y := cos(Random(100));
                    CreateSpark(Pos, a, 44 + Random(4), j, 120);
                  end;
              {$ENDIF}

              // create knife thing
              {$IFDEF SERVER}
              if Style = BULLET_STYLE_THROWNKNIFE then
                CreateThing(BulletParts.Pos[Num], Owner, OBJECT_COMBAT_KNIFE, 255);
              {$ENDIF}

              {$IFNDEF SERVER}
              PlaySound(SFX_COLLIDERHIT, BulletParts.Pos[Num]);
              {$ENDIF}

              Hit(HIT_TYPE_WALL);
              Kill;
            end;
          BULLET_STYLE_FRAGNADE:  // frag grenade exploded
            if TimeOut < GRENADE_TIMEOUT - 2 then
            begin
              Hit(HIT_TYPE_FRAGNADE);
              Kill;
            end;
          BULLET_STYLE_FLAME:
            begin
              Kill;
            end;
          BULLET_STYLE_ARROW:
            if TimeOut > ARROW_RESIST then
            begin
              BulletParts.Forces[Num].Y :=
                BulletParts.Forces[Num].Y - BulletParts.Gravity;
              Hit(HIT_TYPE_WALL);

              Kill;
            end;
          BULLET_STYLE_M79, BULLET_STYLE_FLAMEARROW, BULLET_STYLE_LAW:
            begin
              Hit(HIT_TYPE_EXPLODE);  // plays m79 explosion sound
              Kill;
            end;
          BULLET_STYLE_CLUSTERNADE:
            begin
              Hit(HIT_TYPE_CLUSTERNADE);
              Kill;
            end;
          BULLET_STYLE_CLUSTER:
            begin
              Hit(HIT_TYPE_CLUSTER);
              Kill;
            end;
        end;  // case

        Result := Pos;
        Exit;
      end;
    end;  // for j
end;


procedure TBullet.Hit(T: Integer; SpriteHit: Integer = 0; Where: Integer = 0);
var
  a, b: TVector2;
  i: Integer;
begin
  {$IFDEF SERVER}
  Trace('TBullet.Hit');
  {$ENDIF}

  case T of
    HIT_TYPE_WALL:  // wall hit
      begin
        {$IFNDEF SERVER}
        b := BulletParts.Velocity[Num];
        a := Vec2Add(BulletParts.Pos[Num], BulletParts.Velocity[Num]);
        Vec2Scale(b, b, -0.06);
        b.y := b.y - 1.0;

        b.x := b.x * (0.6 + Random(8) / 10);
        b.y := b.y * (0.8 + Random(4) / 10);
        CreateSpark(a, b, 3, Owner, 60);

        b.x := b.x * (0.8 + Random(4) / 10);
        b.y := b.y * (0.6 + Random(8) / 10);
        CreateSpark(a, b, 3, Owner, 65);

        Vec2Scale(b, b, 0.4 + Random(4) / 10);
        CreateSpark(a, b, 1, Owner, 60);

        b.x := b.x * (0.5 + Random(4) / 10);
        b.y := b.y * (0.7 + Random(8) / 10);
        CreateSpark(a, b, 3, Owner, 50);

        b.x := 0;
        b.y := 0;
        if r_maxsparks.Value > (MAX_SPARKS - 5) then
          CreateSpark(a, b, 56, Owner, 22);

        if TimeOut < BULLET_TIMEOUT - 5 then
        begin
          PlaySound(SFX_RIC + Random(4), BulletParts.Pos[Num]);
        end;
        {$ENDIF}
      end;

    HIT_TYPE_BLOOD:  // body hit -blood
      begin
        {$IFNDEF SERVER}
        b := BulletParts.Velocity[Num];
        a := BulletParts.Pos[Num];
        Vec2Scale(b, b, 0.025);
        // Vec3Normalize(b, b);
        b.x := b.x * 1.2;
        b.y := b.y * 0.85;
        CreateSpark(a, b, 4, Owner, 70);

        b.x := b.x * 0.745;
        b.y := b.y * 1.1;
        CreateSpark(a, b, 4, Owner, 75);
        b.x := b.x * 0.9;
        b.y := b.y * 0.85;
        if Random(2) = 0 then
          CreateSpark(a, b, 4, Owner, 75);

        b.x := b.x * 1.2;
        b.y := b.y * 0.85;
        CreateSpark(a, b, 5, Owner, 80);

        b.x := b.x * 1;
        b.y := b.y * 1;
        CreateSpark(a, b, 5, Owner, 85);

        b.x := b.x * 0.5;
        b.y := b.y * 1.05;
        if Random(2) = 0 then
          CreateSpark(a, b, 5, Owner, 75);

        for i := 1 to 7 do
          if Random(6) = 0 then
          begin
            b.x := sin(Random(100)) * 1.6;
            b.y := cos(Random(100)) * 1.6;
            CreateSpark(a, b, 4, Owner, 55);
          end;
        {$ENDIF}
      end;

    HIT_TYPE_EXPLODE:  // 40mm grenade explode
      begin
        {$IFNDEF SERVER}
        a := Vector2(0.0, 0.0);

        if r_maxsparks.Value > (MAX_SPARKS - 10) then
          CreateSpark(BulletParts.Pos[Num], a, 60, Owner, 255);

        if r_maxsparks.Value > (MAX_SPARKS - 10) then
          CreateSpark(BulletParts.Pos[Num], a, 54, Owner, SMOKE_ANIMS * 4 + 10);

        CreateSpark(BulletParts.Pos[Num], a, 12, Owner, EXPLOSION_ANIMS * 3);
        PlaySound(SFX_M79_EXPLOSION, BulletParts.Pos[Num]);
        {$ENDIF}

        ExplosionHit(HIT_TYPE_EXPLODE, SpriteHit, Where);
      end;

    HIT_TYPE_FRAGNADE:
      begin
        {$IFNDEF SERVER}
        a := Vector2(0.0, 0.0);

        if r_maxsparks.Value > (MAX_SPARKS - 10) then
          CreateSpark(BulletParts.Pos[Num], a, 60, Owner, 190);

        if r_maxsparks.Value > (MAX_SPARKS - 10) then
          CreateSpark(BulletParts.Pos[Num], a, 54, Owner, SMOKE_ANIMS * 4 + 10);

        CreateSpark(BulletParts.Pos[Num], a, 17, Owner, EXPLOSION_ANIMS * 3);
        PlaySound(SFX_GRENADE_EXPLOSION, BulletParts.Pos[Num]);
        {$ENDIF}

        ExplosionHit(HIT_TYPE_FRAGNADE, SpriteHit, Where);
      end;

    HIT_TYPE_THING:
      begin
        {$IFNDEF SERVER}
        b := BulletParts.Velocity[Num];
        a := Vec2Add(BulletParts.Pos[Num], BulletParts.Velocity[Num]);
        Vec2Scale(b, b, -0.02);
        Vec2Scale(b, b, 0.4 + Random(4) / 10);
        CreateSpark(a, b, 1, Owner, 70);

        PlaySound(SFX_BODYFALL, BulletParts.Pos[Num]);
        {$ENDIF}
      end;

    HIT_TYPE_CLUSTERNADE:
      begin
        {$IFNDEF SERVER}
        b.x := 0;
        b.y := 0;
        CreateSpark(BulletParts.Pos[Num], b, 29, Owner, 55);

        PlaySound(SFX_CLUSTERGRENADE, BulletParts.Pos[Num]);
        {$ENDIF}

        a := Vec2Subtract(BulletParts.Pos[Num], BulletParts.Velocity[Num]);

        for i := 1 to 5 do
        begin
          b := BulletParts.Velocity[Num];
          Vec2Scale(b, b, -0.75);
          b.x := -b.x - 2.5 + (Random(50) / 10);
          b.y := b.y - 2.5 + (Random(25) / 10);
          CreateBullet(a, b, Guns[CLUSTER].Num, Owner, 255,
            Guns[FRAGGRENADE].HitMultiply / 2, True, False);
        end;
      end;

    HIT_TYPE_CLUSTER:
      begin
        {$IFNDEF SERVER}
        a := Vector2(0.0, 0.0);
        CreateSpark(BulletParts.Pos[Num], a, 28, Owner, EXPLOSION_ANIMS * 3);
        PlaySound(SFX_CLUSTER_EXPLOSION, BulletParts.Pos[Num]);
        {$ENDIF}

        ExplosionHit(HIT_TYPE_CLUSTER, SpriteHit, Where);
      end;

    HIT_TYPE_FLAK:
      begin
        {$IFNDEF SERVER}
        a := Vector2(0.0, 0.0);
        CreateSpark(BulletParts.Pos[Num], a, 29, Owner, 55);
        PlaySound(SFX_M2EXPLODE, BulletParts.Pos[Num]);
        {$ENDIF}

        ExplosionHit(HIT_TYPE_FLAK, SpriteHit, Where);
      end;

    {$IFNDEF SERVER}
    HIT_TYPE_BODYHIT:
      begin
        if r_maxsparks.Value < (MAX_SPARKS - 10) then
          Exit;

        b := BulletParts.Velocity[Num];
        a := BulletParts.Pos[Num];
        Vec2Scale(b, b, 0.075);

        b.x := b.x * 1.2;
        b.y := b.y * 0.85;
        CreateSpark(a, b, 4, Owner, 60);
        b.x := b.x * 0.745;
        b.y := b.y * 1.1;
        CreateSpark(a, b, 4, Owner, 65);
        b.x := b.x * 1.5;
        b.y := b.y * 0.4;
        CreateSpark(a, b, 5, Owner, 70);
        b.x := b.x * 1;
        b.y := b.y * 1;
        CreateSpark(a, b, 5, Owner, 75);
        b.x := b.x * 0.4;
        b.y := b.y * 1.15;

        for i := 1 to 4 do
          if Random(6) = 0 then
          begin
            b.x := Sin(Random(100)) * 1.2;
            b.y := Cos(Random(100)) * 1.2;
            CreateSpark(a, b, 4, Owner, 50);
          end;
      end;

    HIT_TYPE_RICOCHET:  // ricochet hit
      begin
        a := Vec2Add(BulletParts.Pos[Num], BulletParts.Velocity[Num]);

        b.x := (-2.0 + Random(40) / 10);
        b.y := (-2.0 + Random(40) / 10);
        CreateSpark(a, b, 26, Owner, 35);
        b.x := (-2.0 + Random(40) / 10);
        b.y := (-2.0 + Random(40) / 10);
        CreateSpark(a, b, 26, Owner, 35);
        b.x := (-3.0 + Random(60) / 10);
        b.y := (-3.0 + Random(60) / 10);
        CreateSpark(a, b, 26, Owner, 35);
        b.x := (-3.0 + Random(60) / 10);
        b.y := (-3.0 + Random(60) / 10);
        CreateSpark(a, b, 26, Owner, 35);
        b.x := (-3.0 + Random(60) / 10);
        b.y := (-3.0 + Random(60) / 10);
        CreateSpark(a, b, 26, Owner, 35);
        b.x := (-3.0 + Random(60) / 10);
        b.y := (-3.0 + Random(60) / 10);
        CreateSpark(a, b, 27, Owner, 35);

        PlaySound(SFX_RIC5 + Random(3), BulletParts.Pos[Num]);
      end;
      {$ENDIF}
  end;  // case
end;

procedure TBullet.ExplosionHit(Typ, SpriteHit, Where: Integer);
const
  AFTER_EXPLOSION_RADIUS2: Single = AFTER_EXPLOSION_RADIUS*AFTER_EXPLOSION_RADIUS;
  BodyParts: array[0..6] of Integer = (12, 11, 10, 6, 5, 4, 3);
{$IFNDEF SERVER}
  srv: Integer = 0;
{$ENDIF}
var
  a, Col: TVector2;
  i, j, w, iGun, PushTick: Integer;
  PartHit: Boolean;
  s, ExplosionRadius, ExplosionRadius2, HitboxModifier: Single;
  NoCollision: Byte;
  s2: Single = 0.0;
  {$IFNDEF SERVER}
  b: TVector2;
  n, rnd: Integer;
  {$ENDIF}
begin
  case Typ of
    HIT_TYPE_FRAGNADE: begin
      iGun := FRAGGRENADE;
      ExplosionRadius := FRAGGRENADE_EXPLOSION_RADIUS
    end;
    HIT_TYPE_EXPLODE: begin
      iGun := M79;
      ExplosionRadius := M79GRENADE_EXPLOSION_RADIUS;
    end;
    HIT_TYPE_CLUSTER, HIT_TYPE_FLAK: begin
      iGun := FRAGGRENADE;
      ExplosionRadius := CLUSTERGRENADE_EXPLOSION_RADIUS;
    end;
    else Exit;
  end;

  ExplosionRadius2 := ExplosionRadius * ExplosionRadius;

  // check explosion collision with sprites
  for i := 1 to MAX_SPRITES do
  begin
    if not Sprite[i].Active or Sprite[i].IsSpectator() then
      Continue;

    NoCollision := Guns[WeaponNumToIndex(OwnerWeapon)].NoCollision;

    if (NoCollision and WEAPON_NOCOLLISION_EXP_ENEMY <> 0) and
        (Sprite[i].IsNotInSameTeam(Sprite[Owner])) then
      Continue;

    if (NoCollision and WEAPON_NOCOLLISION_EXP_TEAM <> 0) and
        (Sprite[i].IsInSameTeam(Sprite[Owner])) and (i <> Owner) then
      Continue;

    if (NoCollision and WEAPON_NOCOLLISION_EXP_SELF <> 0) and
        (Owner = i) then
      Continue;

    if not Sprite[i].DeadMeat then
    begin
      Col := GetSpriteCollisionPoint(i);
      HitboxModifier := 1.0;

      // if hitpoint is not given find closest one
      w := Where;
      if (i <> SpriteHit) or (Where = 0) then
      begin
        s := MaxSingle;
        for j := Low(BodyParts) to High(BodyParts) do
        begin
          a.x := Col.x + (Sprite[i].Skeleton.Pos[BodyParts[j]].x - SpriteParts.Pos[i].x);
          a.y := Col.y + (Sprite[i].Skeleton.Pos[BodyParts[j]].y - SpriteParts.Pos[i].y);
          a := Vec2Subtract(BulletParts.Pos[Num], a);
          s2 := Vec2Length2(a);

          if s2 < s then
          begin
            s := s2;            // squared distance
            w := BodyParts[j];  // hitpoint index
          end;
        end;
      end;

      if w <= 4 then
        HitboxModifier := Guns[iGun].ModifierLegs
      else if w <= 11 then
        HitboxModifier := Guns[iGun].ModifierChest
      else
        HitboxModifier := Guns[iGun].ModifierHead;

      Col.x := Col.x + (Sprite[i].Skeleton.Pos[w].x - SpriteParts.Pos[i].x);
      Col.y := Col.y + (Sprite[i].Skeleton.Pos[w].y - SpriteParts.Pos[i].y);

      a := Vec2Subtract(BulletParts.Pos[Num], Col);
      s := Vec2Length2(a);

      if s < ExplosionRadius2 then
      begin
        s := Sqrt(s);

        {$IFNDEF SERVER}
        CreateSpark(SpriteParts.Pos[i], Vector2(0, -0.01), 5, Owner, 80);
        PlaySound(SFX_EXPLOSION_ERG, SpriteParts.Pos[i]);
        {$ENDIF}

        // collision respond
        a.x := (a.x * (1 / (s + 1)) * EXPLOSION_IMPACT_MULTIPLY);
        a.y := (a.y * (1 / (s + 1)) * EXPLOSION_IMPACT_MULTIPLY);

        if Typ in [HIT_TYPE_FRAGNADE, HIT_TYPE_EXPLODE] then
          a.y *= 2.0
        else
          HitboxModifier *= 0.5; // cluster/flak is halved

        PushTick := Sprite[i].Player.PingTicks div 2 + OwnerPingTick + 1;
        PushTick := Min(PushTick, MAX_PUSHTICK);
        Sprite[i].NextPush[PushTick].x -= a.x;
        Sprite[i].NextPush[PushTick].y -= a.y;

        if Sprite[i].CeaseFireCounter < 0 then
        begin
          s := (1 / (s + 1)) * Guns[iGun].HitMultiply * HitboxModifier;
          Sprite[i].HealthHit({$IFNDEF SERVER}srv *{$ENDIF}s, Owner, 1, Num, a);
        end;

        {$IFNDEF SERVER}
        if CanHitSpray(i, Owner) then
          HitSpray();
        {$ENDIF}
      end;  // s < explosion radius
    end;  // not DeadMeat

    if Sprite[i].DeadMeat then
    begin
      PartHit := False;

      for j := 1 to 16 do
      begin
        a := Vec2Subtract(BulletParts.Pos[Num], Sprite[i].Skeleton.Pos[j]);
        s := Vec2Length2(a);

        if s < ExplosionRadius2 then
        begin
          s := Sqrt(s);
          Vec2Scale(a, a, (1 / (s + 1)) * EXPLOSION_DEADIMPACT_MULTIPLY);
          Sprite[i].Skeleton.Oldpos[j].x += a.x;
          Sprite[i].Skeleton.Oldpos[j].y += a.y;

          PartHit := True;
          s2 := s;
        end;
      end;

      if PartHit then
      begin
        HitboxModifier := 1.0;

        if Typ = HIT_TYPE_EXPLODE then
          s2 := Max(s2, 20.0000001)
        else if Typ in [HIT_TYPE_CLUSTER, HIT_TYPE_FLAK] then
          HitboxModifier := 0.5;

        s2 := (1 / (s2 + 1)) * Guns[iGun].HitMultiply * HitboxModifier;
        Sprite[i].HealthHit({$IFNDEF SERVER}srv *{$ENDIF}s2, Owner, 1, Num, a);
      end;
    end;
  end;  // for Sprite[i]

  // check explosion collision with things
  for i := 1 to MAX_THINGS do
  begin
    if not Thing[i].Active or not Thing[i].CollideWithBullets then
      Continue;

    for j := 1 to 4 do
    begin
      a := Vec2Subtract(BulletParts.Pos[Num], Thing[i].Skeleton.Pos[j]);
      s := Vec2Length2(a);

      if s < ExplosionRadius2 then
      begin
        s := Sqrt(s);
        Vec2Scale(a, a, 0.5 * (1 / (s + 1)) * EXPLOSION_IMPACT_MULTIPLY);
        Thing[i].Skeleton.Oldpos[j].x += a.x;
        Thing[i].Skeleton.Oldpos[j].y += a.y;
        Thing[i].StaticType := False;
      end;
    end;
  end;  // for Thing[i]

  if not Typ in [HIT_TYPE_FRAGNADE, HIT_TYPE_EXPLODE] then
    Exit;

  // check explosion collision with bullets
  Active := False;
  for i := 1 to MAX_BULLETS do
  begin
    if (i <> Num) and Bullet[i].Active and (Bullet[i].Style in
      [BULLET_STYLE_FRAGNADE, BULLET_STYLE_M79, BULLET_STYLE_LAW]) then
    begin
      a := Vec2Subtract(BulletParts.Pos[Num], BulletParts.Pos[i]);
      s := Vec2Length2(a);

      if s < AFTER_EXPLOSION_RADIUS2 then
      begin
        case Bullet[i].Style of
          BULLET_STYLE_FRAGNADE: Bullet[i].Hit(HIT_TYPE_FRAGNADE);
          BULLET_STYLE_M79: Bullet[i].Hit(HIT_TYPE_EXPLODE);
          BULLET_STYLE_LAW: Bullet[i].Hit(HIT_TYPE_EXPLODE);
        end;

        Bullet[i].Kill;
      end;
    end;
  end;

  {$IFNDEF SERVER}
  // Grenade Effect
  if snd_effects_explosions.Value then
  begin
    if MySprite > 0 then
    begin
      if Sprite[MySprite].Health > -50 then
      begin
        if Distance(BulletParts.Pos[Num], SpriteParts.Pos[MySprite]) <
          GRENADEEFFECT_DIST then
        begin
          GrenadeEffectTimer := 320;
          PlaySound(SFX_HUM);
        end;
      end;
    end;
  end;

  a := Vec2Subtract(BulletParts.Pos[Num], BulletParts.Velocity[Num]);

  // dirt fly
  if r_maxsparks.Value > (MAX_SPARKS - 10) then
  begin
    n := iif(Typ = HIT_TYPE_FRAGNADE, 6, 7);
    s := iif(Typ = HIT_TYPE_FRAGNADE, -0.2, -0.15);

    for i := 1 to n do
    begin
      Vec2Scale(b, BulletParts.Velocity[Num], s);
      b.x := -b.x - 3.5 + (Random(70) / 10);
      b.y := b.y - 3.5 + (Random(65) / 10);
      if Random(4) = 0 then
        CreateSpark(a, b, 40, Owner, 180 + Random(50));
      if Random(4) = 0 then
        CreateSpark(a, b, 41, Owner, 180 + Random(50));
      if Random(4) = 0 then
        CreateSpark(a, b, 42, Owner, 180 + Random(50));
      if Random(4) = 0 then
        CreateSpark(a, b, 43, Owner, 180 + Random(50));
    end;
  end;

  // smaller dirt fly
  if r_maxsparks.Value > (MAX_SPARKS - 10) then
  begin
    n := iif(Typ = HIT_TYPE_FRAGNADE, 7, 5);
    s := iif(Typ = HIT_TYPE_FRAGNADE, -0.2, -0.15);
    rnd := iif(Typ = HIT_TYPE_FRAGNADE, 4, 3);

    for i := 1 to n do
    begin
      Vec2Scale(b, BulletParts.Velocity[Num], s);
      b.x := -b.x - 3.5 + (Random(70) / 10);
      b.y := b.y - 3.5 + (Random(65) / 10);
      if Random(rnd) = 0 then
        CreateSpark(a, b, 44, Owner, 120);
      if Random(rnd) = 0 then
        CreateSpark(a, b, 45, Owner, 120);
      if Random(rnd) = 0 then
        CreateSpark(a, b, 46, Owner, 120);
      if Random(rnd) = 0 then
        CreateSpark(a, b, 47, Owner, 120);
    end;
  end;

  // iskry fly
  if r_maxsparks.Value > (MAX_SPARKS - 10) then
  begin
    n := iif(Typ = HIT_TYPE_FRAGNADE, 3, 4);
    rnd := iif(Typ = HIT_TYPE_FRAGNADE, 23, 22);

    for i := 1 to n do
    begin
      Vec2Scale(b, BulletParts.Velocity[Num], -0.3);
      b.x := -b.x - 3.5 + (Random(70) / 10);
      b.y := b.y - 3.5 + (Random(65) / 10);
      if Random(rnd) = 0 then
        CreateSpark(a, b, 2, Owner, 120);
      if Random(rnd) = 0 then
        CreateSpark(a, b, 2, Owner, 120);
      if Random(rnd) = 0 then
        CreateSpark(a, b, 2, Owner, 120);
    end;
  end;

  // plomyki
  if r_maxsparks.Value > (MAX_SPARKS - 10) then
  begin
    n := iif(Typ = HIT_TYPE_FRAGNADE, 3, 4);
    j := iif(Typ = HIT_TYPE_FRAGNADE, 25, 20);
    rnd := iif(Typ = HIT_TYPE_FRAGNADE, 50, 40);
    s := iif(Typ = HIT_TYPE_FRAGNADE, -0.05, -0.1);

    for i := 1 to n do
    begin
      a.x := a.x - j + Random(rnd);
      a.y := a.y - j + Random(rnd);
      Vec2Scale(b, BulletParts.Velocity[Num], s);
      b.x := -b.x - 3.5 + (Random(70) / 10);
      b.y := b.y - 3.5 + (Random(65) / 10);
      CreateSpark(a, b, 64, Owner, 35);
    end;
  end;
  {$ENDIF}
end;

procedure TBullet.CheckOutOfBounds;
var
  Bound: Integer;
  BulletPartsPos: ^TVector2;
begin
  {$IFDEF SERVER}
  Trace('TBullet.CheckOutOfBounds');
  {$ENDIF}

  Bound := Map.SectorsNum * Map.SectorsDivision - 10;
  BulletPartsPos := @BulletParts.Pos[Num];

  if (Abs(BulletPartsPos.X) > Bound) or
     (Abs(BulletPartsPos.Y) > Bound) then
    Kill;
end;

function TBullet.FilterSpritesByDistance(var SpriteIndexes: TSpriteIndexes): Integer;
var
  i, j, SpriteCount: Integer;
  RoughDistance: Single;
  Distances: TSpriteDistances;
begin
  SpriteCount := 0;
  Distances := Default(TSpriteDistances);

  for i := 1 to MAX_SPRITES do
  begin
    if TargetableSprite(i) then
    begin
      // Get a representation of the distance between this bullet and the sprite
      RoughDistance := GetComparableSpriteDistance(i);

      // Add the sprite index at the appropriate place using a variant of Insertion sort
      SpriteCount := SpriteCount + 1;
      j := SpriteCount;

      while (j > 1) and (RoughDistance < Distances[j - 1]) do
        j := j - 1;
      Move(Distances[j], Distances[j + 1], (SpriteCount - j) * SizeOf(Single));
      Distances[j] := RoughDistance;

      Move(SpriteIndexes[j], SpriteIndexes[j + 1], (SpriteCount - j) * SizeOf(Integer));
      SpriteIndexes[j] := i;
    end;
  end;

  Result := SpriteCount;
end;

function TBullet.TargetableSprite(i: Integer): Boolean;
var
  OwnerVulnerableTime: Integer;
begin
  if Style = BULLET_STYLE_FRAGNADE then
    OwnerVulnerableTime := GRENADE_TIMEOUT - 50
  else if Style = BULLET_STYLE_M2 then
    OwnerVulnerableTime := M2BULLET_TIMEOUT - 20
  else if Style = BULLET_STYLE_FLAME then
    OwnerVulnerableTime := FLAMER_TIMEOUT
  else
    OwnerVulnerableTime := BULLET_TIMEOUT - 20;

  // Check whether a sprite can be hit by this bullet
  Result := Sprite[i].Active and
            ((Owner <> i) or (TimeOut < OwnerVulnerableTime)) and
            (HitBody <> i) and Sprite[i].IsNotSpectator();
end;

function TBullet.GetComparableSpriteDistance(i: Integer): Single;
var
  SpriteCol, Distance: TVector2;
begin
  SpriteCol := GetSpriteCollisionPoint(i);
  Distance := Vec2Subtract(BulletParts.Pos[Num], SpriteCol);

  // Faster Euclidean distance calc used for comparisons.
  // Sqrt can be skipped because if "Sqrt[a] < Sqrt[b]" then "a < b"
  Result := Distance.x * Distance.x + Distance.y * Distance.y;
end;

function TBullet.GetSpriteCollisionPoint(i: Integer): TVector2;
begin
  // Why is this an exception to the usual rule??
  {$IFNDEF SERVER}
  if (Style = BULLET_STYLE_FLAME) and (TimeOut > FLAMER_TIMEOUT - 2) then
  begin
    Result := SpriteParts.Pos[i];
    Exit;
  end;
  {$ENDIF}

  // Ping compensation to get a more accurate collision point
  {$IFNDEF SERVER}
  if (Sprite[i].Player.ControlMethod <> BOT) then
  begin
    if (OwnerPingTick < MAX_OLDPOS) then
      Result := OldSpritePos[i, OwnerPingTick]
    else
      Result := OldSpritePos[i, MAX_OLDPOS - 1];
  end
  else
  {$ENDIF}
    Result := SpriteParts.Pos[i];
end;

function TBullet.GetWeaponIndex: Byte;
var
  WeaponIndex: Byte;
begin
  for WeaponIndex := 1 to High(Guns) do
  begin
    if OwnerWeapon = Guns[WeaponIndex].Num then
    begin
      Result := WeaponIndex;
      Exit;
    end;
  end;
  Result := 0;  // Not possible
end;

end.

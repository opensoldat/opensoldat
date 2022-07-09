{*******************************************************}
{                                                       }
{       Sprites Unit for OPENSOLDAT                     }
{       Based on Strike of the Dragon Unit              }
{       by Michal Marcinkowski                          }
{                                                       }
{       Copyright (c) 2002 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Sprites;

interface

uses
  Parts, Anims, MapFile, PolyMap, Net, Weapons, Constants, Vector;

const
  MAX_SPRITES = MAX_PLAYERS;
  MAX_BULLETS = 254;
  MAX_SPARKS  = 558;
  MAX_THINGS  =  90;

  SURFACECOEFX           = 0.970;
  SURFACECOEFY           = 0.970;
  CROUCHMOVESURFACECOEFX = 0.850;
  CROUCHMOVESURFACECOEFY = 0.970;
  STANDSURFACECOEFX      = 0.000;
  STANDSURFACECOEFY      = 0.000;
  GRENADE_SURFACECOEF    = 0.880;
  SPARK_SURFACECOEF      = 0.700;

  PART_RADIUS                     =  7;
  FLAG_PART_RADIUS                = 10;
  SPRITE_RADIUS                   = 16;
  M79GRENADE_EXPLOSION_RADIUS     = 64;
  FRAGGRENADE_EXPLOSION_RADIUS    = 85;
  AFTER_EXPLOSION_RADIUS          = 50;
  CLUSTERGRENADE_EXPLOSION_RADIUS = 35;
  BASE_RADIUS                     = 75;
  TOUCHDOWN_RADIUS                = 28;
  SPRITE_COL_RADIUS               =  3;

  FLAG_HOLDING_FORCEUP = -14;
  FLAG_STAND_FORCEUP   = -16;

  BULLETALPHA  = 110;
  MAXPATHCOUNT =  50;

  SLIDELIMIT = 0.2;
  MAX_VELOCITY = 11;
  BULLETTIME_MINDISTANCE = 320;
  FLAGTHROW_POWER = 4.225;

  NORMAL_DEATH   = 1;
  BRUTAL_DEATH   = 2;
  HEADCHOP_DEATH = 3;

  POS_STAND  = 1;
  POS_CROUCH = 2;
  POS_PRONE  = 4;

type
  TSpriteIndexes = array[1..MAX_SPRITES] of Integer;
  TSpriteDistances = array[1..MAX_SPRITES] of Single;

  TControl = record
    Left, Right, Up, Down,
    Fire, Jetpack, ThrowNade, ChangeWeapon,
    ThrowWeapon, Reload, Prone, FlagThrow: Boolean;
    MouseAimX, MouseAimY, MouseDist: SmallInt;
  end;

  TBotData = record
    FavWeapon: Integer;
    Friend: string;
    Accuracy, GrenadeFreq: Integer;
    DeadKill: Byte;
    WaypointTimeoutCounter, WaypointTimeout: Integer;
    ChatFreq: Integer;
    ChatKill, ChatDead, ChatLowHealth, ChatSeeEnemy, ChatWinning: string;
    PissedOff: Byte;
    PathNum: Byte;
    TargetNum: Byte;
    GoThing: Boolean;
    CurrentWaypoint, NextWaypoint, OldWaypoint, WaypointTime,
      LastWaypoint: Integer;
    Use: Byte;
    OnePlaceCount: Integer;
    Camper: Byte;
    CampTimer: Integer;
    FallSave: Byte;
  end;

  TBackgroundState = object
    BackgroundStatus: Byte;
    BackgroundPoly: SmallInt;
    BackgroundTestResult: Boolean;
    function BackgroundTest(Poly: Word): Boolean;
    procedure BackgroundTestBigPolyCenter(Pos: TVector2);
    function BackgroundFindCurrentPoly(Pos: TVector2): SmallInt;
    procedure BackgroundTestPrepare();
    procedure BackgroundTestReset();
  end;

  PSprite = ^TSprite;
  TSprite = object
    Active, DeadMeat, Dummy: Boolean;
    Style: Byte;
    Num: Byte;
    Visible: Byte;
    OnGround, OnGroundForLaw: Boolean;
    OnGroundLastFrame: Boolean;
    OnGroundPermanent: Boolean;
    Direction, OldDirection: SmallInt;
    Health: Single;
    HoldedThing: Byte;
    FlagGrabCooldown: Integer;
    AimDistCoef: Single;
    Fired: Byte;
    Alpha: Byte;
    JetsCountReal: Single;
    JetsCount, JetsCountPrev: SmallInt;
    WearHelmet: Byte;
    HasCigar: Byte;
    CanMercy: Boolean;
    RespawnCounter, CeaseFireCounter: SmallInt;
    SelWeapon: Byte;
    BonusStyle, BonusTime: Integer;
    MultiKillTime, MultiKills: Integer;
    Vest: Single;
    IdleTime: Integer;
    IdleRandom: ShortInt;
    BurstCount: Byte;
    Position: Byte;
    OnFire: Byte;
    ColliderDistance: Byte;
    DeadCollideCount: Integer;
    DeadTime: Integer;
    Para, Stat: Byte;
    UseTime: SmallInt;
    HalfDead: Boolean;
    LastWeaponHM, LastWeaponSpeed: Single;
    LastWeaponStyle: Byte;
    LastWeaponFire, LastWeaponReload: Word;
    Skeleton: ParticleSystem;
    LegsAnimation, BodyAnimation: TAnimation;
    Control: TControl;
    Weapon, SecondaryWeapon, TertiaryWeapon: TGun;
    GrenadeCanThrow: Boolean;
    Brain: TBotData;
    Player: TPlayer;
    IsPlayerObjectOwner: Boolean;
    Typing: Boolean;
    AutoReloadWhenCanFire: Boolean;
    CanAutoReloadSpas: Boolean;
    BGState: TBackgroundState;
    {$IFDEF SERVER}
    HasPack: Boolean;
    TargetX, TargetY: Single;
    {$ELSE}
    GattlingSoundChannel2, ReloadSoundChannel, JetsSoundChannel, GattlingSoundChannel: LongInt;
    OldDeadMeat: Boolean;
    Muted: Boolean;
    {$ENDIF}
    DontDrop: Boolean;
    NextPush: array [0..MAX_PUSHTICK] of TVector2;
    BulletCount: Word;
    {$IFDEF SERVER}
    BulletCheck: array [0..BULLETCHECKARRAYSIZE] of Word;
    BulletCheckIndex: Integer;
    BulletCheckAmount: Integer;
    {$ENDIF}
  public
    procedure Update;
    procedure Kill;
    procedure Die(How, Who, Where, What: Integer; Impact: TVector2);
    function DropWeapon(): Integer;
    procedure LegsApplyAnimation(Anim: TAnimation; Curr: Integer);
    procedure BodyApplyAnimation(Anim: TAnimation; Curr: Integer);
    procedure MoveSkeleton(x1, y1: Single; FromZero: Boolean);
    function CheckMapCollision(X, Y: Single; Area: Integer): Boolean;
    function CheckRadiusMapCollision(X, Y: Single; HasCollided: Boolean): Boolean;
    function CheckMapVerticesCollision(X, Y: Single; R: Single; HasCollided: Boolean): Boolean;
    function CheckSkeletonMapCollision(i: Integer; X, Y: Single): Boolean;
    procedure HandleSpecialPolyTypes(PolyType: Integer; Pos: TVector2);
    procedure ApplyWeaponByNum(WNum: Byte; Gun: Byte; Ammo: Integer = -1;
      RestorePrimaryState: Boolean = False);
    procedure HealthHit(Amount: Single; Who, Where, What: Integer;
      Impact: TVector2);
    procedure FreeControls;
    procedure CheckOutOfBounds;
    procedure CheckSkeletonOutOfBounds;
    procedure Respawn;
    {$IFDEF SERVER}
    procedure ResetSpriteOldPos;
    {$ENDIF}
    procedure Parachute(a: TVector2);
    procedure ChangeTeam(Team: Integer{$IFDEF SERVER};
      AdminChange: Boolean = False; JoinType: Byte = JOIN_NORMAL{$ENDIF});
    procedure Fire();
    procedure ThrowFlag();
    procedure ThrowGrenade();
    function GetMoveacc(): Single;
    function GetCursorAimDirection(): TVector2;
    function GetHandsAimDirection(): TVector2;
    function IsSolo(): Boolean;
    function IsNotSolo(): Boolean;
    function IsInTeam(): Boolean;
    function IsSpectator(): Boolean;
    function IsNotSpectator(): Boolean;
    function IsInSameTeam(const OtherPlayer: TSprite): Boolean;
    function IsNotInSameTeam(const OtherPlayer: TSprite): Boolean;
    function CanRespawn(DeadMeatBeforeRespawn: Boolean): Boolean;
  end;

  function CreateSprite(sPos, sVelocity: TVector2; sStyle, N: Byte; Player: TPlayer; TransferOwnership: Boolean): Integer;
  function TeamCollides(Poly, Team: Integer; Bullet: Boolean): Boolean;

var
  SpriteMapColCount: Integer;
  {$IFNDEF SERVER}
  wasReloading: Boolean = False;
  procedure SelectDefaultWeapons(MySprite: Byte);
  {$ENDIF}

implementation

uses
  {$IFDEF SERVER}
    {$IFDEF SCRIPT}ScriptDispatcher,{$ENDIF}
    ServerHelper, LogFile,
    NetworkServerSprite, NetworkServerMessages, NetworkServerConnection, NetworkServerGame, NetworkServerThing,
  {$ELSE}
    Sound, Demo, GameStrings, ClientGame, GameMenus, Sparks,
    NetworkClientSprite,
  {$ENDIF}
  Bullets, {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} Util, SysUtils, Calc, Math, TraceLog, Game, Control, Things, Cvar;

function CreateSprite(sPos, sVelocity: TVector2; sStyle, N: Byte; Player: TPlayer; TransferOwnership: Boolean): Integer;
var
  i, j: Integer;
  SecWep: Integer;
begin
  Trace('CreateSprite');

  if N = 255 then
  begin
    for i := 1 to MAX_SPRITES + 1 do
    begin
      if i = MAX_SPRITES + 1 then
      begin
        Result := -1;
        Exit;
      end;
      if not Sprite[i].Active then
        Break;
    end;
  end
  else
    i := N; // i is now the active sprite

  Result := i;

  // replace player object
  if Sprite[i].Player <> nil then
  begin
    Sprite[i].Player.SpriteNum := 0;
    if Sprite[i].IsPlayerObjectOwner then
      Sprite[i].Player.Free;
  end;
  Sprite[i].Player := Player;
  Sprite[i].Player.SpriteNum := i;
  Sprite[i].IsPlayerObjectOwner := TransferOwnership;

  Sprite[i].Active := True;
  Sprite[i].Style := sStyle;
  Sprite[i].Num := i;
  Sprite[i].DeadMeat := False;
  Sprite[i].RespawnCounter := 0;
  Sprite[i].CeaseFireCounter := CeaseFireTime;

  if sv_survivalmode.Value then
    Sprite[i].CeaseFireCounter := Sprite[i].CeaseFireCounter * 3;

  Sprite[i].Alpha := 255;
  Sprite[i].Brain.PissedOff := 0;
  Sprite[i].Vest := 0;
  Sprite[i].BonusStyle := BONUS_NONE;
  Sprite[i].BonusTime := 0;
  Sprite[i].MultiKills := 0;
  Sprite[i].MultiKillTime := 0;
  Sprite[i].TertiaryWeapon := Guns[FRAGGRENADE];
  Sprite[i].HasCigar := 0;
  Sprite[i].IdleTime := DEFAULT_IDLETIME;
  Sprite[i].IdleRandom := -1;
  Sprite[i].Position := POS_STAND;
  Sprite[i].BodyAnimation := Stand;
  Sprite[i].LegsAnimation := Stand;
  Sprite[i].OnFire := 0;
  Sprite[i].HoldedThing := 0;
  Sprite[i].SelWeapon := 0;
  Sprite[i].Stat := 0;

  {$IFNDEF SERVER}
  Sprite[i].OldDeadMeat := False;
  Sprite[i].HalfDead := False;
  {$ENDIF}

  Sprite[i].BGState.BackgroundStatus := BACKGROUND_TRANSITION;
  Sprite[i].BGState.BackgroundPoly := BACKGROUND_POLY_UNKNOWN;

  sVelocity.x := 0;
  sVelocity.y := 0;

  if Sprite[i].Player.Team = TEAM_SPECTATOR then
  begin
    sPos.X := MIN_SECTORZ * Map.SectorsDivision * 0.8;
    sPos.Y := MIN_SECTORZ * Map.SectorsDivision * 0.8;
  end;

  // activate sprite part
  SpriteParts.CreatePart(sPos, sVelocity, 1, i);

  // create skeleton
  Sprite[i].Skeleton.TimeStep := 1;
  Sprite[i].Skeleton.Gravity := 1.06 * GRAV;
  Sprite[i].Skeleton := GostekSkeleton;
  Sprite[i].Skeleton.VDamping := 0.9945;

  Sprite[i].Health := STARTHEALTH;
  Sprite[i].AimDistCoef := DEFAULTAIMDIST;

  Sprite[i].Weapon := Guns[NOWEAPON];

  SecWep := Sprite[i].Player.SecWep + 1;
  if (SecWep >= 1) and (SecWep <= SECONDARY_WEAPONS) and
     (WeaponActive[PRIMARY_WEAPONS + SecWep] = 1) then
    Sprite[i].SecondaryWeapon := Guns[PRIMARY_WEAPONS + SecWep]
  else
    Sprite[i].SecondaryWeapon := Guns[NOWEAPON];

  Sprite[i].JetsCount := Map.StartJet;
  {$IFNDEF SERVER}
  Sprite[i].JetsCountPrev := Map.StartJet;
  {$ENDIF}
  Sprite[i].TertiaryWeapon.AmmoCount := sv_maxgrenades.Value div 2;

  Sprite[i].WearHelmet := 1;
  if Sprite[i].Player.HeadCap = 0 then
    Sprite[i].WearHelmet := 0;

  Sprite[i].Brain.TargetNum := 1;
  Sprite[i].Brain.WaypointTimeoutCounter := WAYPOINTTIMEOUT;

  Sprite[i].DeadCollideCount := 0;

  {$IFNDEF SERVER}
  Sprite[i].ReloadSoundChannel := i - 1;
  Sprite[i].JetsSoundChannel := 1 * MAX_SPRITES + i - 1;
  Sprite[i].GattlingSoundChannel := 2 * MAX_SPRITES + i - 1;
  Sprite[i].GattlingSoundChannel2 := 3 * MAX_SPRITES + i - 1;
  Sprite[i].MoveSkeleton(sPos.X, sPos.Y, False);
  {$ELSE}
  Sprite[i].MoveSkeleton(0, 0, False);
  {$ENDIF}

  // clear push wait list
  for j := 0 to MAX_PUSHTICK do
  begin
    Sprite[i].NextPush[j].X := 0;
    Sprite[i].NextPush[j].Y := 0;
  end;

  Sprite[i].BulletCount := Random(High(Word)); // FIXME wat?
  Sprite[i].FreeControls;

  SortPlayers; // sort the players frag list
end;

function TeamCollides(Poly, Team: Integer; Bullet: Boolean): Boolean;
begin
  Result := True;
  if Bullet then
  begin
    if (Map.PolyType[Poly] = POLY_TYPE_RED_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_RED_PLAYER) then
      if (Team = TEAM_ALPHA) and (Map.PolyType[Poly] = POLY_TYPE_RED_BULLETS) then
        Result := True
      else
        Result := False
    else if (Map.PolyType[Poly] = POLY_TYPE_BLUE_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_BLUE_PLAYER) then
      if (Team = TEAM_BRAVO) and (Map.PolyType[Poly] = POLY_TYPE_YELLOW_BULLETS) then
        Result := True
      else
        Result := False
    else if (Map.PolyType[Poly] = POLY_TYPE_YELLOW_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_YELLOW_PLAYER) then
      if (Team = TEAM_CHARLIE) and (Map.PolyType[Poly] = POLY_TYPE_YELLOW_BULLETS) then
        Result := True
      else
        Result := False
    else if (Map.PolyType[Poly] = POLY_TYPE_GREEN_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_GREEN_PLAYER) then
      if (Team = TEAM_DELTA) and (Map.PolyType[Poly] = POLY_TYPE_GREEN_BULLETS) then
        Result := True
      else
        Result := False;
  end
  else
  begin
    if ((Map.PolyType[Poly] = POLY_TYPE_RED_BULLETS) and (Team = TEAM_ALPHA)) or
      (((Map.PolyType[Poly] = POLY_TYPE_RED_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_RED_PLAYER)) and
      (Team <> TEAM_ALPHA)) then
      Result := False
    else if ((Map.PolyType[Poly] = POLY_TYPE_BLUE_BULLETS) and (Team = TEAM_BRAVO)) or
      (((Map.PolyType[Poly] = POLY_TYPE_BLUE_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_BLUE_PLAYER)) and
      (Team <> TEAM_BRAVO)) then
      Result := False
    else if ((Map.PolyType[Poly] = POLY_TYPE_YELLOW_BULLETS) and (Team = TEAM_CHARLIE)) or
      (((Map.PolyType[Poly] = POLY_TYPE_YELLOW_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_YELLOW_PLAYER)) and
      (Team <> TEAM_CHARLIE)) then
      Result := False
    else if ((Map.PolyType[Poly] = POLY_TYPE_GREEN_BULLETS) and (Team = TEAM_DELTA)) or
      (((Map.PolyType[Poly] = POLY_TYPE_GREEN_BULLETS) or
      (Map.PolyType[Poly] = POLY_TYPE_GREEN_PLAYER)) and
      (Team <> TEAM_DELTA)) then
      Result := False;
  end;
  if (Map.PolyType[Poly] = POLY_TYPE_NON_FLAGGER_COLLIDES) then
    Result := False;
end;

procedure TSprite.Update;
var
  i: Integer;
  {$IFNDEF SERVER}
  k: Integer;
  RND: Integer;
  M3, M4: TVector2;
  WeaponReloadSound: Integer;
  {$ENDIF}
  MouseAim, P, M: TVector2;
  // rotation vars
  RNorm, LegVector: TVector2;
  BodyY, ArmS: Single;
  LegDistance: Single = 0.0;
begin
  {$IFDEF SERVER}
  Trace('TSprite.Update');
  {$ENDIF}

  JetsCountPrev := JetsCount;
  Weapon.ReloadTimePrev := Weapon.ReloadTimeCount;
  Weapon.FireIntervalPrev := Weapon.FireIntervalCount;

  BodyY := 0;

  SpriteParts.Velocity[Num] := Vec2Add(SpriteParts.Velocity[Num],
    NextPush[0]);
  {$IFNDEF SERVER}
  for i := 0 to MAX_PUSHTICK - 1 do
    NextPush[i] := NextPush[i + 1];
  {$ENDIF}
  NextPush[MAX_PUSHTICK].X := 0;
  NextPush[MAX_PUSHTICK].Y := 0;

  // reload spas after shooting delay is over
  if Sprite[Num].AutoReloadWhenCanFire and
     ((Sprite[Num].Weapon.Num <> Guns[SPAS12].Num) or
      (Sprite[Num].Weapon.FireIntervalCount = 0)) then
  begin
    Sprite[Num].AutoReloadWhenCanFire := False;

    if (Sprite[Num].Weapon.Num = Guns[SPAS12].Num) and
       (Sprite[Num].BodyAnimation.ID <> Roll.ID) and
       (Sprite[Num].BodyAnimation.ID <> RollBack.ID) and
       (Sprite[Num].BodyAnimation.ID <> Change.ID) and
       (Sprite[Num].Weapon.AmmoCount <> Sprite[Num].Weapon.Ammo) then
    begin
      Sprite[Num].BodyApplyAnimation(Reload, 1);
    end;
  end;

  if {$IFNDEF SERVER}(ClientStopMovingCounter > 0) {$ELSE}
     (((Player.ControlMethod = HUMAN) and (NoClientupdateTime[Num] < CLIENTSTOPMOVE_RETRYS)) or
      (Player.ControlMethod = BOT)) {$ENDIF} then
    ControlSprite(Sprite[Num]);

  if IsSpectator() then
  begin
    DeadMeat := True;

    {$IFNDEF SERVER}
    if Num = MySprite then
    begin
      RespawnCounter := 19999;
      GameMenuShow(LimboMenu, False);
    end;
    {$ENDIF}
  end;

  Skeleton.OldPos[21] := Skeleton.Pos[21];
  Skeleton.OldPos[23] := Skeleton.Pos[23];
  Skeleton.OldPos[25] := Skeleton.Pos[25];
  Skeleton.Pos[21] := Skeleton.Pos[9];
  Skeleton.Pos[23] := Skeleton.Pos[12];
  Skeleton.Pos[25] := Skeleton.Pos[5];
  if not DeadMeat then
  begin
    Vec2Add(Skeleton.Pos[21], SpriteParts.Velocity[Num]);
    Vec2Add(Skeleton.Pos[23], SpriteParts.Velocity[Num]);
    Vec2Add(Skeleton.Pos[25], SpriteParts.Velocity[Num]);
  end;

  case Position of
    POS_STAND: BodyY := 8;
    POS_CROUCH: BodyY := 9;
    POS_PRONE:
      begin
        if BodyAnimation.ID = Prone.ID then
        begin
          if BodyAnimation.CurrFrame > 9 then
            BodyY := -2
          else
            BodyY := 14 - BodyAnimation.CurrFrame;
        end
        else
          BodyY := 9;

        if BodyAnimation.ID = ProneMove.ID then
          BodyY := 0;
      end;
  end;

  if BodyAnimation.ID = GetUp.ID then
    if BodyAnimation.CurrFrame > 18 then
      BodyY := 8
    else
      BodyY := 4;

  if FlagGrabCooldown > 0 then
    Dec(FlagGrabCooldown);

  // Reset the background poly test before collision checks on the corpse
  if DeadMeat then
    BGState.BackgroundTestPrepare();

  if Control.MouseAimX >= SpriteParts.Pos[Num].X then
    Direction := 1
  else
    Direction := -1;

  for i := 1 to 20 do
  begin
    if Skeleton.Active[i] and not DeadMeat then
    begin
      Skeleton.OldPos[i] := Skeleton.Pos[i];

      if not HalfDead then
        // legs
        if (i = 1) or (i = 4) or (i = 2) or (i = 3) or (i = 5) or (i = 6) or
          (i = 17) or (i = 18) then
        begin
          Skeleton.Pos[i].X := SpriteParts.Pos[Num].X + Direction *
            LegsAnimation.Frames[LegsAnimation.CurrFrame].Pos[i].X;
          Skeleton.Pos[i].Y := SpriteParts.Pos[Num].Y +
            LegsAnimation.Frames[LegsAnimation.CurrFrame].Pos[i].Y;
        end;

      // body
      if (i = 7) or (i = 8) or (i = 9) or (i = 10) or (i = 11) or (i = 12) or
         (i = 13) or (i = 14) or (i = 15) or (i = 16) or (i = 19) or
         (i = 20) then
      begin
        Skeleton.Pos[i].X := SpriteParts.Pos[Num].X + Direction *
          BodyAnimation.Frames[BodyAnimation.CurrFrame].Pos[i].X;
        if not HalfDead then
          Skeleton.Pos[i].Y := (Skeleton.Pos[6].Y -
            (SpriteParts.Pos[Num].Y - BodyY)) + SpriteParts.Pos[Num].Y +
            BodyAnimation.Frames[BodyAnimation.CurrFrame].Pos[i].Y
        else
          Skeleton.Pos[i].Y := 9 + SpriteParts.Pos[Num].Y +
            BodyAnimation.Frames[BodyAnimation.CurrFrame].Pos[i].Y;
      end;
    end;
  end;

  if not DeadMeat then
  begin
    // Rotate parts
    // head
    i := 12;
    begin
      P.X := Skeleton.Pos[i].X;
      P.Y := Skeleton.Pos[i].Y;
      MouseAim.X := Control.MouseAimX;
      MouseAim.Y := Control.MouseAimY;
      RNorm := Vec2Subtract(P, MouseAim);
      Vec2Normalize(RNorm, RNorm);
      Vec2Scale(RNorm, RNorm, 0.1);
      Skeleton.Pos[i].X := Skeleton.Pos[9].X - Direction * RNorm.Y;
      Skeleton.Pos[i].Y := Skeleton.Pos[9].Y + Direction * RNorm.X;

      Vec2Scale(RNorm, RNorm, 50);
      Skeleton.Pos[23].X := Skeleton.Pos[9].X - Direction * RNorm.Y;
      Skeleton.Pos[23].Y := Skeleton.Pos[9].Y + Direction * RNorm.X;
    end;

    if BodyAnimation.ID = Throw.ID then
      ArmS := -5
    else
      ArmS := -7;

    // arm
    i := 15;
    if (BodyAnimation.ID <> Reload.ID) and
       (BodyAnimation.ID <> ReloadBow.ID) and
       (BodyAnimation.ID <> ClipIn.ID) and
       (BodyAnimation.ID <> ClipOut.ID) and
       (BodyAnimation.ID <> SlideBack.ID) and
       (BodyAnimation.ID <> Change.ID) and
       (BodyAnimation.ID <> ThrowWeapon.ID) and
       (BodyAnimation.ID <> WeaponNone.ID) and
       (BodyAnimation.ID <> Punch.ID) and
       (BodyAnimation.ID <> Roll.ID) and
       (BodyAnimation.ID <> RollBack.ID) and
       (BodyAnimation.ID <> Cigar.ID) and
       (BodyAnimation.ID <> Match.ID) and
       (BodyAnimation.ID <> Smoke.ID) and
       (BodyAnimation.ID <> Wipe.ID) and
       (BodyAnimation.ID <> TakeOff.ID) and
       (BodyAnimation.ID <> Groin.ID) and
       (BodyAnimation.ID <> Piss.ID) and
       (BodyAnimation.ID <> Mercy.ID) and
       (BodyAnimation.ID <> Mercy2.ID) and
       (BodyAnimation.ID <> Victory.ID) and
       (BodyAnimation.ID <> Own.ID) and
       (BodyAnimation.ID <> Melee.ID) then
    begin
      P.X := Skeleton.Pos[i].X;
      P.Y := Skeleton.Pos[i].Y;
      MouseAim.X := Control.MouseAimX;
      MouseAim.Y := Control.MouseAimY;
      RNorm := Vec2Subtract(P, MouseAim);
      Vec2Normalize(RNorm, RNorm);
      Vec2Scale(RNorm, RNorm, ArmS);
      M.X := Skeleton.Pos[16].X;
      M.Y := Skeleton.Pos[16].Y;
      P := Vec2Add(M, RNorm);
      Skeleton.Pos[i].X := P.X;
      Skeleton.Pos[i].Y := P.Y;
    end;

    if BodyAnimation.ID = Throw.ID then
      ArmS := -6
    else
      ArmS := -8;

    // arm
    i := 19;
    if (BodyAnimation.ID <> Reload.ID) and
       (BodyAnimation.ID <> ReloadBow.ID) and
       (BodyAnimation.ID <> ClipIn.ID) and
       (BodyAnimation.ID <> ClipOut.ID) and
       (BodyAnimation.ID <> SlideBack.ID) and
       (BodyAnimation.ID <> Change.ID) and
       (BodyAnimation.ID <> ThrowWeapon.ID) and
       (BodyAnimation.ID <> WeaponNone.ID) and
       (BodyAnimation.ID <> Punch.ID) and
       (BodyAnimation.ID <> Roll.ID) and
       (BodyAnimation.ID <> RollBack.ID) and
       (BodyAnimation.ID <> Cigar.ID) and
       (BodyAnimation.ID <> Match.ID) and
       (BodyAnimation.ID <> Smoke.ID) and
       (BodyAnimation.ID <> Wipe.ID) and
       (BodyAnimation.ID <> TakeOff.ID) and
       (BodyAnimation.ID <> Groin.ID) and
       (BodyAnimation.ID <> Piss.ID) and
       (BodyAnimation.ID <> Mercy.ID) and
       (BodyAnimation.ID <> Mercy2.ID) and
       (BodyAnimation.ID <> Victory.ID) and
       (BodyAnimation.ID <> Own.ID) and
       (BodyAnimation.ID <> Melee.ID) then
    begin
      P.X := Skeleton.Pos[i].X;
      P.Y := Skeleton.Pos[i].Y;
      MouseAim.X := Control.MouseAimX;
      MouseAim.Y := Control.MouseAimY;
      RNorm := Vec2Subtract(P, MouseAim);
      Vec2Normalize(RNorm, RNorm);
      Vec2Scale(RNorm, RNorm, ArmS);
      M.X := Skeleton.Pos[16].X;
      M.Y := Skeleton.Pos[16].Y - 4;
      P := Vec2Add(M, RNorm);
      Skeleton.Pos[i].X := P.X;
      Skeleton.Pos[i].Y := P.Y;
    end;
  end;

  for i := 1 to 20 do
  begin
    // dead part
    if DeadMeat or HalfDead and
       IsNotSpectator() then
    begin
      if (i <> 17) and (i <> 18) and (i <> 19) and (i <> 20) and (i <> 8) and
         (i <> 7) and (i < 21) then
      begin
        OnGround := CheckSkeletonMapCollision(i, Skeleton.Pos[i].X, Skeleton.Pos[i].Y);
      end;

      {$IFNDEF SERVER}
      // bleed
      // check where constraints are cut then BLEED
      for k := 1 to Skeleton.ConstraintCount do
      begin
        if not Skeleton.Constraints[k].Active and
           ((Skeleton.Constraints[k].PartA = i) or
            (Skeleton.Constraints[k].PartB = i)) then
        begin
          M4 := Skeleton.Pos[i];
          M4.Y := M4.Y + 2;
          M3 := Vec2Subtract(Skeleton.Pos[i], Skeleton.OldPos[i]);
          Vec2Scale(M3, M3, 0.35);

          if SparksCount > 300 then
            RND := BLOOD_RANDOM_LOW
          else if SparksCount > 50 then
            RND := BLOOD_RANDOM_NORMAL
          else
            RND := BLOOD_RANDOM_HIGH;

          if DeadTime > LESSBLEED_TIME then
            RND := 2 * RND;
          if DeadTime > NOBLEED_TIME then
            RND := 100 * RND;

          if r_maxsparks.Value < (MAX_SPARKS - 10) then
            RND := 2 * RND;

          if (k <> 10) and (k <> 11) then
            if Random(RND) = 0 then
              CreateSpark(M4, M3, 5, Num, 85 - Random(25))
            else if Random(RND div 3) = 0 then
                CreateSpark(M4, M3, 4, Num, 85 - Random(25));
        end;
      end;  // bleed

      // fire
      if DeadTime < ONFIRE_TIME then
        if OnFire > 0 then
          if i mod OnFire = 0 then
          begin
            M4 := Skeleton.Pos[i];
            M4.Y := M4.Y + 3;
            M3 := Vec2Subtract(Skeleton.Pos[i], Skeleton.OldPos[i]);
            Vec2Scale(M3, M3, 0.3);

            RND := FIRE_RANDOM_NORMAL;
            if SparksCount > 170 then
              RND := FIRE_RANDOM_LOW;
            if SparksCount < 17 then
              RND := FIRE_RANDOM_HIGH;

            if r_maxsparks.Value < (MAX_SPARKS - 10) then
              RND := 2 * RND;

            if Random(RND) = 0 then
            begin
              CreateSpark(M4, M3, 36, Num, 35);
              if Random(8) = 0 then
                PlaySound(SFX_ONFIRE, SpriteParts.Pos[Num]);
              if Random(2) = 0 then
                PlaySound(SFX_FIRECRACK, SpriteParts.Pos[Num]);
            end else
              if Random(RND div 3) = 0 then
                CreateSpark(M4, M3, 37, Num, 75);
          end;
      {$ENDIF}
    end;
  end;

  // If no background poly contact in CheckSkeletonMapCollision() then reset any background poly status
  if DeadMeat then
    BGState.BackgroundTestReset();

  {$IFDEF SERVER}
  Trace('TSprite.Update 2');
  {$ENDIF}

  if not DeadMeat then
  begin
    case Style of
      1:
        begin
          BodyAnimation.DoAnimation;
          LegsAnimation.DoAnimation;

          CheckOutOfBounds;

          OnGround := False;

          {$IFNDEF SERVER}
          if OldDeadMeat then
          begin
            Respawn;
            OldDeadMeat := DeadMeat;
          end;
          {$ENDIF}

          // Reset the background poly test before collision checks
          BGState.BackgroundTestPrepare();

          // head
          CheckMapCollision(SpriteParts.Pos[Num].X - 3.5,
            SpriteParts.Pos[Num].Y - 12, 1);

          CheckMapCollision(SpriteParts.Pos[Num].X + 3.5,
            SpriteParts.Pos[Num].Y - 12, 1);

          BodyY := 0;
          ArmS := 0;

          // Walking either left or right (though only one can be active at once)
          if Control.Left xor Control.Right then
          begin
            // If walking in facing direction
            if Control.Left xor (Direction = 1) then
              ArmS := 0.25
            else  // Walking backwards
              BodyY := 0.25;
          end;

          // If a leg is inside a polygon, caused by the modification of ArmS and
          // BodyY, this is there to not lose contact to ground on slope polygons
          if BodyY = 0 then
          begin
            LegVector := Vector2(SpriteParts.Pos[Num].X + 2,
              SpriteParts.Pos[Num].Y + 1.9);
            if Map.RayCast(LegVector, LegVector, LegDistance, 10) then
              BodyY := 0.25;
          end;
          if ArmS = 0 then
          begin
            LegVector := Vector2(SpriteParts.Pos[Num].X - 2,
              SpriteParts.Pos[Num].Y + 1.9);
            if Map.RayCast(LegVector, LegVector, LegDistance, 10) then
              ArmS := 0.25;
          end;

          // Legs collison check. If collided then don't check the other side as a possible double
          // CheckMapCollision collision would result in too much of a ground repelling force.
          OnGround := CheckMapCollision(SpriteParts.Pos[Num].X + 2,
            SpriteParts.Pos[Num].Y + 2 - BodyY, 0);

          OnGround := OnGround or CheckMapCollision(SpriteParts.Pos[Num].X - 2,
            SpriteParts.Pos[Num].Y + 2 - ArmS, 0);

          // radius collison check
          OnGroundForLaw := CheckRadiusMapCollision(SpriteParts.Pos[Num].X, SpriteParts.Pos[Num].Y - 1,
            OnGround);

          OnGround :=
            CheckMapVerticesCollision(SpriteParts.Pos[Num].X, SpriteParts.Pos[Num].Y, 3,
              OnGround or OnGroundForLaw) or OnGround;

          // Change the permanent state if the player has had the same OnGround state for two frames in a row
          if not (OnGround xor OnGroundLastFrame) then
            OnGroundPermanent := OnGround;

          OnGroundLastFrame := OnGround;

          // If no background poly contact then reset any background poly status
          BGState.BackgroundTestReset();

          // WEAPON HANDLING
          {$IFNDEF SERVER}
          if (Num = MySprite) or
             (Weapon.FireInterval <= FIREINTERVAL_NET) or
             not PointVisible(SpriteParts.Pos[Num].X, SpriteParts.Pos[Num].Y, CameraFollowSprite) then
          {$ENDIF}
            if (Weapon.FireIntervalCount > 0) and
               ((Weapon.AmmoCount > 0) or
                (Weapon.Num = Guns[SPAS12].Num)) then
            begin
              Weapon.FireIntervalPrev := Weapon.FireIntervalCount;
              Dec(Weapon.FireIntervalCount);
            end;

          // If fire button is released, then the reload can begin
          if not Sprite[Num].Control.Fire then
            CanAutoReloadSpas := True;

          // reload
          if (Weapon.AmmoCount = 0) and
             ((Weapon.Num = Guns[CHAINSAW].Num) or
              ((BodyAnimation.ID <> Roll.ID) and
               (BodyAnimation.ID <> RollBack.ID) and
               (BodyAnimation.ID <> Melee.ID) and
               (BodyAnimation.ID <> Change.ID) and
               (BodyAnimation.ID <> Throw.ID) and
               (BodyAnimation.ID <> ThrowWeapon.ID))) then
          begin
            {$IFNDEF SERVER}
            if ReloadSoundChannel > -2 then
              SetSoundPaused(ReloadSoundChannel, False);
            {$ENDIF}

            if BodyAnimation.ID <> GetUp.ID then
            begin
              //spas is unique - it does the fire interval delay AND THEN reloads. all other weapons do the opposite.
              if Weapon.Num = Guns[SPAS12].Num then
              begin
                if (Weapon.FireIntervalCount = 0) and CanAutoReloadSpas then
                  BodyApplyAnimation(Reload, 1)
              end
              else
                if (Weapon.Num = Guns[BOW].Num) or (Weapon.Num = Guns[BOW2].Num) then
                  BodyApplyAnimation(ReloadBow, 1)
                else
                  if (BodyAnimation.ID <> ClipIn.ID) and
                     (BodyAnimation.ID <> SlideBack.ID) then
                  begin
                    // Don't show reload animation for chainsaw if one of these
                    // animations are already ongoing
                    if (Weapon.Num <> Guns[CHAINSAW].Num) or
                       ((BodyAnimation.ID <> Roll.ID) and
                        (BodyAnimation.ID <> RollBack.ID) and
                        (BodyAnimation.ID <> Melee.ID) and
                        (BodyAnimation.ID <> Change.ID) and
                        (BodyAnimation.ID <> Throw.ID) and
                        (BodyAnimation.ID <> ThrowWeapon.ID)) then
                      BodyApplyAnimation(ClipOut, 1);
                  end;

              BurstCount := 0;
            end;

            {$IFNDEF SERVER}
            // play reload sound
            if Weapon.ReloadTimeCount = Weapon.ReloadTime then
            begin

              if Weapon.Num = Guns[EAGLE].Num then
                WeaponReloadSound := SFX_DESERTEAGLE_RELOAD
              else if Weapon.Num = Guns[MP5].Num then
                WeaponReloadSound := SFX_MP5_RELOAD
              else if Weapon.Num = Guns[AK74].Num then
                WeaponReloadSound := SFX_AK74_RELOAD
              else if Weapon.Num = Guns[STEYRAUG].Num then
                WeaponReloadSound := SFX_STEYRAUG_RELOAD
              else if Weapon.Num = Guns[RUGER77].Num then
                WeaponReloadSound := SFX_RUGER77_RELOAD
              else if Weapon.Num = Guns[M79].Num then
                WeaponReloadSound := SFX_M79_RELOAD
              else if Weapon.Num = Guns[BARRETT].Num then
                WeaponReloadSound := SFX_BARRETM82_RELOAD
              else if Weapon.Num = Guns[M249].Num then
                WeaponReloadSound := SFX_M249_RELOAD
              else if Weapon.Num = Guns[MINIGUN].Num then
                WeaponReloadSound := SFX_MINIGUN_RELOAD
              else if Weapon.Num = Guns[COLT].Num then
                WeaponReloadSound := SFX_COLT1911_RELOAD
              else
                WeaponReloadSound := -1;

              if WeaponReloadSound <> -1 then
                PlaySound(WeaponReloadSound, SpriteParts.Pos[Num], ReloadSoundChannel);
            end;

            M3.X := Skeleton.Pos[15].X;
            M3.Y := Skeleton.Pos[15].Y + 6;
            M4.X := Spriteparts.Velocity[Num].X;
            M4.Y := Spriteparts.Velocity[Num].Y - 0.001;
            if Weapon.ReloadTimeCount = Weapon.ClipOutTime then
            begin
              if Weapon.Num = Guns[EAGLE].Num then
              begin
                CreateSpark(M3, M4, 18, Num, 255);
                M3.X := Skeleton.Pos[15].X - 2;
                M3.Y := Skeleton.Pos[15].Y + 7;
                M4.X := Spriteparts.Velocity[Num].X + 0.3;
                M4.Y := Spriteparts.Velocity[Num].Y - 0.003;
                CreateSpark(M3, M4, 18, Num, 255);
              end
              else if Weapon.Num = Guns[MP5].Num then
                CreateSpark(M3, M4, 11, Num, 255)
              else if Weapon.Num = Guns[AK74].Num then
                CreateSpark(M3, M4, 9, Num, 255)
              else if Weapon.Num = Guns[STEYRAUG].Num then
                CreateSpark(M3, M4, 19, Num, 255)
              else if Weapon.Num = Guns[BARRETT].Num then
                CreateSpark(M3, M4, 20, Num, 255)
              else if Weapon.Num = Guns[M249].Num then
                CreateSpark(M3, M4, 10, Num, 255)
              else if Weapon.Num = Guns[COLT].Num then
                CreateSpark(M3, M4, 23, Num, 255);
            end;
            {$ENDIF}

            if Weapon.Num <> Guns[SPAS12].Num then
            begin
              //Spas doesn't use the reload time.
              //If it ever does, be sure to put this back outside.
              Weapon.ReloadTimePrev := Weapon.ReloadTimeCount;
              if Weapon.ReloadTimeCount > 0 then
                Dec(Weapon.ReloadTimeCount);

              //spas waits for fire interval to hit 0.
              //doing this next line for the spas would cause it to never reload when empty.
              Weapon.FireIntervalPrev := Weapon.FireInterval;
              Weapon.FireIntervalCount := Weapon.FireInterval;

              if Weapon.ReloadTimeCount < 1 then
              begin
                Weapon.ReloadTimePrev := Weapon.ReloadTime;
                Weapon.FireIntervalPrev := Weapon.FireInterval;
                Weapon.ReloadTimeCount := Weapon.ReloadTime;
                Weapon.FireIntervalCount := Weapon.FireInterval;
                Weapon.StartUpTimeCount := Weapon.StartUpTime;
                Weapon.AmmoCount := Weapon.Ammo;
              end;
            end;
          end;

          // weapon jam fix?
          // TODO: check if server or client do stuff wrong here...
          if Weapon.AmmoCount = 0 then
          begin
            {$IFDEF SERVER}
            if Weapon.ReloadTimeCount < 1 then
            begin
              Weapon.ReloadTimeCount := Weapon.ReloadTime;
              Weapon.FireIntervalCount := Weapon.FireInterval;
              Weapon.StartUpTimeCount := Weapon.StartUpTime;
              Weapon.AmmoCount := Weapon.Ammo;
            end;
            if Weapon.ReloadTimeCount > Weapon.ReloadTime then
              Weapon.ReloadTimeCount := Weapon.ReloadTime;
            {$ENDIF}

            if Weapon.Num <> Guns[SPAS12].Num then
            begin
              if Weapon.ReloadTimeCount < 1 then
              begin
                {$IFDEF SERVER}
                BodyApplyAnimation(Change, 36);
                {$ENDIF}
                Weapon.ReloadTimePrev := Weapon.ReloadTime;
                Weapon.FireIntervalPrev := Weapon.FireInterval;
                Weapon.ReloadTimeCount := Weapon.ReloadTime;
                Weapon.FireIntervalCount := Weapon.FireInterval;
                Weapon.StartUpTimeCount := Weapon.StartUpTime;
                Weapon.AmmoCount := Weapon.Ammo;
              end;

              {$IFNDEF SERVER}
              if Weapon.ReloadTimeCount > Weapon.ReloadTime then
              begin
                Weapon.ReloadTimePrev := Weapon.ReloadTime;
                Weapon.ReloadTimeCount := Weapon.ReloadTime;
              end;

              // didn't we just do this right above? :S
              if Weapon.Num <> Guns[SPAS12].Num then
                if Weapon.ReloadTimeCount < 1 then
                begin
                  BodyApplyAnimation(Change, 36);
                  Weapon.ReloadTimePrev := Weapon.ReloadTime;
                  Weapon.FireIntervalPrev := Weapon.FireInterval;
                  Weapon.ReloadTimeCount := Weapon.ReloadTime;
                  Weapon.FireIntervalCount := Weapon.FireInterval;
                  Weapon.StartUpTimeCount := Weapon.StartUpTime;
                  Weapon.AmmoCount := Weapon.Ammo;
                end;
              {$ENDIF}
            end;
          end;

          {$IFNDEF SERVER}
          // chainsaw smoke
          if (Weapon.Num = Guns[CHAINSAW].Num) and (Stat = 0) then
          begin
            if r_maxsparks.Value > (MAX_SPARKS - 10) then
            begin
              if MainTickCounter mod 15 = 0 then
              begin
                M3 := Skeleton.Pos[9]; M3.y := M3.Y - 2;
                M3.X := M3.X + Direction * 3;
                M4.x := 0; M4.y := -0.25;
                CreateSpark(M3, M4, 1, Num, 20);
                if Weapon.AmmoCount = 0 then
                  PlaySound(SFX_CHAINSAW_O, SpriteParts.Pos[Num], GattlingSoundChannel)
                else
                  PlaySound(SFX_CHAINSAW_M, SpriteParts.Pos[Num], DefaultChannel);
              end;
            end;

            if Control.Fire then
            begin
              if Weapon.AmmoCount > 0 then
                PlaySound(SFX_CHAINSAW_R, SpriteParts.Pos[Num], GattlingSoundChannel);
            end;
          end;

          // LAW, chansaw smoke
          if (Weapon.Num = Guns[LAW].Num) or (Weapon.Num = Guns[CHAINSAW].Num) then
          begin
            if Weapon.AmmoCount = 0 then
              if r_maxsparks.Value > (MAX_SPARKS - 10) then
                if Random(4) = 0 then
                begin
                  M3 := Skeleton.Pos[9];
                  M3.y := M3.Y - 2 - 1 + Random(60) / 10;
                  M3.X := M3.X + Direction * 3 - 8 + Random(80) / 10;
                  M4.x := 0; M4.y := -0.3;
                  CreateSpark(M3, M4, 1, Num, 20);
                end;
          end;

          // flame arrow on fire
          if (Weapon.Num = Guns[BOW2].Num) then
            if Random(10) = 0 then
            begin
              M3.X := Skeleton.Pos[15].X + Direction * 6;
              M3.Y := Skeleton.Pos[15].Y - 5;
              CreateSpark(M3, Vector2(0, -0.5), 36, Num, 40);
            end;
          {$ENDIF}

          // JETS
          if {$IFNDEF SERVER}(ClientStopMovingCounter > 0){$ELSE}
             (((Player.ControlMethod = HUMAN) and (NoClientupdateTime[Num] < CLIENTSTOPMOVE_RETRYS)) or
              (Player.ControlMethod = BOT)) {$ENDIF} then
            if (JetsCount < Map.StartJet) and not Control.Jetpack then
            begin
              if OnGround or (MainTickCounter mod 2 = 0) then
                Inc(JetsCount);
            end;

          if CeaseFireCounter > -1 then
          begin
            CeaseFireCounter := CeaseFireCounter - 1;
            Alpha := Round(Abs(100 + 70 * sin(SinusCounter)));
          end else
          begin
            Alpha := 255;
          end;

          if BonusStyle = BONUS_PREDATOR then
            Alpha := PREDATORALPHA;

          {$IFNDEF SERVER}
          // bleed when BERSERKER
          if BonusStyle = BONUS_BERSERKER then
          begin
            M4 := Skeleton.Pos[19];
            M4.X := M4.X - 5 + Random(11);
            M4.Y := M4.Y - 5 + Random(11);
            M3 := Vec2Subtract(Skeleton.Pos[19], Skeleton.OldPos[19]);
            M3.Y := M3.y - 1.38;
            RND := BLOOD_RANDOM_HIGH;

            if r_maxsparks.Value < (MAX_SPARKS - 10) then
              RND := 2 * RND;

            if Random(RND) = 0 then
              CreateSpark(M4, M3, 5, Num, 55 - Random(20));
          end;

          if BonusStyle = BONUS_FLAMEGOD then
          begin
            M4 := Skeleton.Pos[19];
            M4.X := M4.X - 5 + Random(11);
            M4.Y := M4.Y - 5 + Random(11);
            M3 := Vec2Subtract(Skeleton.Pos[19], Skeleton.OldPos[19]);
            M3.Y := M3.y - 1.38;
            RND := BLOOD_RANDOM_HIGH;

            if r_maxsparks.Value < (MAX_SPARKS - 10) then
              RND := 2 * RND;

            if Random(RND) = 0 then
              CreateSpark(M4, M3, 36, Num, 40 - Random(10));
          end;

          // bleed when hurt
          if Health < HURT_HEALTH then
          begin
            M4 := Skeleton.Pos[5];
            M4.X := M4.X + 2;
            M4.Y := M4.Y;
            M3 := Vec2Subtract(Skeleton.Pos[5], Skeleton.OldPos[5]);
            RND := BLOOD_RANDOM_NORMAL;

            if r_maxsparks.Value < (MAX_SPARKS - 10) then RND := 2 * RND;

            if Random(RND) = 0 then
              CreateSpark(M4, M3, 4, Num, 65 - Random(10));
          end;
          {$ENDIF}

          // BONUS time
          if BonusTime > -1 then
          begin
            BonusTime := BonusTime - 1;
            if BonusTime < 1 then
            begin
              case BonusStyle of
                BONUS_PREDATOR:
                  Alpha := 255;
              end;
              BonusStyle := BONUS_NONE;
            end;
          end
          else
            BonusStyle := BONUS_NONE;

          // MULITKILL TIMER

          if MultiKillTime > -1 then
          begin
            MultiKillTime := MultiKillTime - 1;
          end else
          begin
            MultiKills := 0;
          end;

          // gain health from bow
          if (MainTickCounter mod 3 = 0) and
             ((Weapon.Num = Guns[BOW].Num) or
              (Weapon.Num = Guns[BOW2].Num)) and (Health < (STARTHEALTH)) then
            Health := Health + 1;

          {$IFNDEF SERVER}
          // smoke
          if HasCigar = 10 then
            if MainTickCounter mod 160 = 0 then
            begin
              begin
                M3 := Skeleton.Pos[9];
                M3.y := M3.Y - 2;
                M3.X := M3.X + Direction * 4;
                M4.x := 0; M4.y := -0.75;
                CreateSpark(M3, M4, 31, Num, 55);
                if Random(2) = 0 then
                begin
                  M3 := Skeleton.Pos[9];
                  M3.y := M3.Y - 2;
                  M3.X := M3.X + Direction * 4.1;
                  M4.x := 0; M4.y := -0.69;
                  CreateSpark(M3, M4, 31, Num, 55);
                  PlaySound(SFX_SMOKE, SpriteParts.Pos[Num]);
                  if Random(2) = 0 then
                  begin
                    M3 := Skeleton.Pos[9];
                    M3.y := M3.Y - 2;
                    M3.X := M3.X + Direction * 3.9;
                    M4.x := 0; M4.y := -0.81;
                    CreateSpark(M3, M4, 31, Num, 55);
                  end;
                end;
              end;
            end;

          // winter breath
          if Map.Weather = 3 then
            if r_maxsparks.Value > (MAX_SPARKS - 10) then
              if MainTickCounter mod 160 = 0 then
              begin
                begin
                  M3 := Skeleton.Pos[9];
                  M3.y := M3.Y - 2;
                  M3.X := M3.X + Direction * 4;
                  M4.x := 0; M4.y := -0.75;
                  CreateSpark(M3, M4, 31, Num, 55);
                end;
              end;
          {$ENDIF}

          // parachuter
          Para := 0;
          if (HoldedThing > 0) and (HoldedThing < MAX_THINGS + 1) then
            if Thing[HoldedThing].Style = OBJECT_PARACHUTE then
              Para := 1;

          if Para = 1 then
          begin
            Spriteparts.Forces[Num].Y := PARA_SPEED;
            {$IFDEF SERVER}
            if CeaseFireCounter < 1 then
            {$ELSE}
            if ((sv_survivalmode.Value) and (CeaseFireCounter < CeaseFireTime * 3 - 30)) or
               (CeaseFireCounter < CeaseFireTime - 30) then
            {$ENDIF}
              if OnGround or Control.Jetpack then
                if (HoldedThing > 0) and (HoldedThing < MAX_THINGS + 1) then
                begin
                  Thing[HoldedThing].HoldingSprite := 0;
                  Dec(Thing[HoldedThing].Skeleton.ConstraintCount);
                  Thing[HoldedThing].TimeOut := 3 * 60;
                  HoldedThing := 0;
                end;
          end;

          {$IFDEF SERVER}
          Trace('TSprite.Update 3e');
          {$ENDIF}

          Skeleton.DoVerletTimeStepFor(22, 29);
          Skeleton.DoVerletTimeStepFor(24, 30);

          {$IFNDEF SERVER}
          // Ping Impr
          for i := MAX_OLDPOS downto 1 do
            OldSpritePos[Num, i] := OldSpritePos[Num, i - 1];

          OldSpritePos[Num, 0] := Spriteparts.Pos[Num];
          {$ENDIF}
        end;  // 1
    end;  // case
  end;

  if DeadMeat then
    if IsNotSpectator() then
    begin
      // physically integrate skeleton particles
      Skeleton.DoVerletTimeStep;

      SpriteParts.Pos[Num] := Skeleton.Pos[12];

      // Ping Impr
      for i := MAX_OLDPOS downto 1 do
        OldSpritePos[Num, i] := OldSpritePos[Num, i - 1];

      OldSpritePos[Num, 0] := Spriteparts.Pos[Num];

      CheckSkeletonOutOfBounds;

      // Respawn Countdown
      {$IFDEF SERVER}
      if RespawnCounter < 1 then
      begin
        Respawn;
        ServerSpriteSnapshotMajorSingle(Num, NETW);
      end;
      {$ENDIF}

      RespawnCounter := RespawnCounter - 1;

      {$IFNDEF SERVER}
      if RespawnCounter < -360 then
      begin
        RespawnCounter := 0;
        Respawn;
      end;
      {$ENDIF}

      {$IFDEF SERVER}
      if sv_survivalmode.Value then
        if RespawnCounter = 1 then
          if not SurvivalEndRound then
            Inc(RespawnCounter, 2)
          else
          begin
            if RespawnCounter < 3 then
              for i := 1 to MAX_SPRITES do
                if Sprite[i].Active and not Sprite[i].DeadMeat then
                begin
                  P := Default(TVector2);
                  Sprite[i].HealthHit(4000, i, 1, -1, P);
                  Dec(Sprite[i].Player.Deaths);
                end;

            if (sv_gamemode.Value <> GAMESTYLE_HTF) then
              if (TeamFlag[1] > 0) and (TeamFlag[2] > 0) then
              begin
                if (not Thing[TeamFlag[1]].InBase) then
                  Thing[TeamFlag[1]].Respawn;
                if (not Thing[TeamFlag[2]].InBase) then
                  Thing[TeamFlag[2]].Respawn;
              end;
          end;
      {$ENDIF}

      // parachuter
      Para := 0;
      if (HoldedThing > 0) and (HoldedThing < MAX_THINGS + 1) then
        if Thing[HoldedThing].Style = OBJECT_PARACHUTE then
          Para := 1;

      if Para = 1 then
      begin
        Skeleton.Forces[12].Y := 25 * PARA_SPEED;
        if OnGround then
          if (HoldedThing > 0) and (HoldedThing < MAX_THINGS + 1) then
          begin
            Thing[HoldedThing].HoldingSprite := 0;
            Dec(Thing[HoldedThing].Skeleton.ConstraintCount);
            Thing[HoldedThing].TimeOut := 3 * 60;
            HoldedThing := 0;
          end;
      end;

      Inc(DeadTime);
    end;  // DeadMeat

  // Safety
  if SpriteParts.Velocity[Num].X > MAX_VELOCITY then
    SpriteParts.Velocity[Num].X := MAX_VELOCITY;
  if SpriteParts.Velocity[Num].X < -MAX_VELOCITY then
    SpriteParts.Velocity[Num].X := -MAX_VELOCITY;
  if SpriteParts.Velocity[Num].Y > MAX_VELOCITY then
    SpriteParts.Velocity[Num].Y := MAX_VELOCITY;
  if SpriteParts.Velocity[Num].Y < -MAX_VELOCITY then
    SpriteParts.Velocity[Num].Y := -MAX_VELOCITY;
end;

procedure TSprite.Kill;
var
  i: Integer;
  Left: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TSprite.Kill');
  {$ENDIF}

  //Debug('[Sprite] Deactivate ' + IntToStr(Num));
  Active := False;
  {$IFNDEF SERVER}
  Muted := False;
  {$ENDIF}

  {$IFNDEF SERVER}
  StopSound(ReloadSoundChannel);
  StopSound(JetsSoundChannel);
  StopSound(GattlingSoundChannel);
  StopSound(GattlingSoundChannel2);
  {$ENDIF}

  if Num > 0 then
  begin
    Sprite[Num].Skeleton.Destroy;
    SpriteParts.Active[Num] := False;
  end;

  if (HoldedThing > 0) and (HoldedThing < MAX_THINGS + 1) then
    if (Thing[HoldedThing].Style < OBJECT_USSOCOM) then
    begin
      Thing[HoldedThing].HoldingSprite := 0;
      HoldedThing := 0;
    end;

  if Stat > 0 then
  begin
    Thing[Stat].StaticType := False;
    Stat := 0;
  end;

  if IsNotSolo() then
  begin
    Left := False;
    for i := 1 to MAX_PLAYERS do
      if Sprite[i].Active and IsInSameTeam(Sprite[i]) and (i <> Num) then
        Left := True;

    if Left = False then
      TeamScore[Player.Team] := TEAM_NONE;
  end;

  {$IFDEF SERVER}
  if num > 0 then
  begin
    NoClientupdateTime[num] := 0;
    MessagesASecNum[num] := 0;
    FloodWarnings[num] := 0;
    PingWarnings[num] := 0;
  end;
  {$ENDIF}

  // sort the players frag list
  SortPlayers;
end;

{$IFNDEF SERVER}
// TODO move into Sprite
procedure SelectDefaultWeapons(MySprite: Byte);
var
  i, j, k: Integer;
begin
  i := 0;
  for j := 1 to PRIMARY_WEAPONS do
    if WeaponActive[j] = 1 then
      Inc(i);

  if i = 1 then
  begin
    for j := 1 to PRIMARY_WEAPONS do
    begin
      if WeaponActive[j] = 1 then
      begin
        WeaponSel[MySprite][j] := 1;
        LimboMenu.Button[j - 1].Active := True;
        Sprite[MySprite].SelWeapon := j;

        if LimboMenu.Active and not Sprite[MySprite].DeadMeat then begin
          Sprite[MySprite].ApplyWeaponByNum(Sprite[MySprite].SelWeapon, 1);
          ClientSpriteSnapshot;
        end;
        Break;
      end;
    end;
  end;

  k := 0;
  for j := 1 to SECONDARY_WEAPONS do
    if WeaponActive[j + PRIMARY_WEAPONS] = 1 then
      Inc(k);

  if k = 1 then
  begin
    for j := PRIMARY_WEAPONS + 1 to MAIN_WEAPONS do
    begin
      if WeaponActive[j] = 1 then
      begin
        WeaponSel[MySprite][j] := 1;
        LimboMenu.Button[j - 1].Active := True;
        Sprite[MySprite].Player.SecWep := j - PRIMARY_WEAPONS - 1;

        cl_player_secwep.SetValue(Sprite[MySprite].Player.SecWep);

        if LimboMenu.Active and not Sprite[MySprite].DeadMeat then
          Sprite[MySprite].ApplyWeaponByNum(Guns[j].Num, 2);
          ClientSpriteSnapshot;
        Break;
      end;
    end;
  end;
end;

function deg2rad(deg: Single): Single;
begin
  Result := deg / (180 / pi);
end;
{$ENDIF}

procedure TSprite.Die(How, Who, Where, What: Integer; Impact: TVector2);
var
  i, j: Integer;
  k: Single;
  S: String;
  a: TVector2;
  {$IFNDEF SERVER}
  b: TVector2;
  {$ELSE}
  S2: String;
  {$ENDIF}
begin
  {$IFDEF SERVER}
  Trace('TSprite.Die');
  {$ENDIF}

  if (Who < 1) or (Who > MAX_SPRITES) then
    Exit;
  if (What > MAX_BULLETS) then
    Exit;

  if not DeadMeat then
  begin
    // bullet time
    if sv_bullettime.Value then
      if GOALTICKS = DEFAULT_GOALTICKS then
      begin
        k := 0;
        for i := 1 to MAX_SPRITES do
          if Sprite[i].Active and (i <> Who) and
            (not Sprite[i].Player.DemoPlayer) and
            Sprite[i].IsNotSpectator() then
            if Distance(SpriteParts.Pos[i],
              SpriteParts.Pos[Who]) > BULLETTIME_MINDISTANCE then
              k := 1;

        if k < 1 then
        begin
          ToggleBulletTime(True);
        end;
      end;

    {$IFDEF SERVER}
      if (sv_gamemode.Value = GAMESTYLE_INF) or (sv_gamemode.Value = GAMESTYLE_TEAMMATCH) or
         (sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_HTF) then
        RespawnCounter := WaveRespawnCounter + sv_respawntime_minwave.Value
      else
        RespawnCounter := sv_respawntime.Value;
    {$ENDIF}
    Inc(Player.Deaths);

    {$IFDEF SERVER}
    if What > 0 then
      if ((Bullet[What].Style = BULLET_STYLE_ARROW) and (sv_gamemode.Value <> GAMESTYLE_RAMBO)) or
         ((Bullet[What].Style = BULLET_STYLE_FLAMEARROW) and (sv_gamemode.Value <> GAMESTYLE_RAMBO)) then
        if (Bullet[What].DontCheat = False) then
        begin
          KickPlayer(Sprite[Who].Num, True, KICK_CHEAT, DAY, 'Not allowed weapon');
          Exit;
        end;

    // Anti-Team Killer Protection
    if sv_punishtk.Value then
    begin
      if IsInSameTeam(Sprite[Who]) and
         not (sv_gamemode.Value = GAMESTYLE_DEATHMATCH) and
         not (sv_gamemode.Value = GAMESTYLE_RAMBO) and
         not (Player.Name = Sprite[Who].Player.Name) then
      begin
        Inc(Sprite[Who].Player.TKWarnings);
        MainConsole.Console(Sprite[Who].Player.Name +
          ' Team Killed ' + Player.Name + ' (Warning #' +
          IntToStr(Sprite[Who].Player.TKWarnings) + ')', GAME_MESSAGE_COLOR);
        ServerSendStringMessage('TK Warning #' +
          WideString(IntToStr(Sprite[Who].Player.TKWarnings)) + '. Max Warnings: ' +
          WideString(IntToStr(sv_warnings_tk.Value)), Who, 255, MSGTYPE_PUB);
        if Sprite[Who].Player.TKWarnings > (sv_warnings_tk.Value div 2) then
        begin
          a := Default(TVector2);
          Sprite[Who].Vest := 0;
          Sprite[Who].HealthHit(200, Who, 1, 1, a);
          ServerSendStringMessage(WideString(Sprite[Who].Player.Name) +
            ' has been punished for TeamKilling. (' +
            WideString(IntToStr(Sprite[Who].Player.TKWarnings)) + '/' +
            WideString(IntToStr(sv_warnings_tk.Value)) + ')', 0, 255, MSGTYPE_PUB);
        end;
        if Sprite[Who].Player.TKWarnings > (sv_warnings_tk.Value - 1) then
          KickPlayer(Who, True, KICK_CONSOLE, 3600 * 15, 'Team Killing');
      end;
    end;
    {$ENDIF}

    if Who <> Num then
    begin
      if sv_gamemode.Value = GAMESTYLE_DEATHMATCH then
      begin
        Inc(Sprite[Who].Player.Kills);

        // mulitkill count
        {$IFDEF SERVER}
        Sprite[Who].MultiKillTime := MULTIKILLINTERVAL;
        Inc(Sprite[Who].MultiKills);
        {$ENDIF}
      end;
      if sv_gamemode.Value = GAMESTYLE_POINTMATCH then
      begin
        // add point for kill
        i := 1;

        // add another point for holding the flag
        if (Sprite[Who].HoldedThing > 0) and
          (Sprite[Who].HoldedThing < MAX_THINGS + 1) then
          if Thing[Sprite[Who].HoldedThing].Style = OBJECT_POINTMATCH_FLAG then
            i := i * 2;

        // add points for multikill
        {$IFDEF SERVER}
        if Sprite[Who].MultiKillTime > 0 then
        begin
          if Sprite[Who].MultiKills = 2 then i := i * 2;
          if Sprite[Who].MultiKills = 3 then i := i * 4;
          if Sprite[Who].MultiKills = 4 then i := i * 8;
          if Sprite[Who].MultiKills = 5 then i := i * 16;
          if Sprite[Who].MultiKills > 5 then i := i * 32;
        end;
        {$ENDIF}

        Inc(Sprite[Who].Player.Kills, i);

        // mulitkill count
        {$IFDEF SERVER}
        Sprite[Who].MultiKillTime := MULTIKILLINTERVAL;
        Inc(Sprite[Who].MultiKills);
        {$ENDIF}
      end;
      if sv_gamemode.Value = GAMESTYLE_TEAMMATCH then
      begin
        if IsNotInSameTeam(Sprite[Who]) then
        begin
          Inc(Sprite[Who].Player.Kills);
          Inc(TeamScore[Sprite[Who].Player.Team]);
          {$IFDEF SERVER}
          // mulitkill count
          Sprite[Who].MultiKillTime := MULTIKILLINTERVAL;
          Inc(Sprite[Who].MultiKills);
          {$ENDIF}
        end;
      end;
      if sv_gamemode.Value = GAMESTYLE_CTF then
      begin
        if IsNotInSameTeam(Sprite[Who]) then
        begin
          Inc(Sprite[Who].Player.Kills);
          {$IFDEF SERVER}
          // mulitkill count
          Sprite[Who].MultiKillTime := MULTIKILLINTERVAL;
          Inc(Sprite[Who].MultiKills);
          {$ENDIF}
        end;
      end;
      if sv_gamemode.Value = GAMESTYLE_INF then
      begin
        if IsNotInSameTeam(Sprite[Who]) then
        begin
          Inc(Sprite[Who].Player.Kills);
          {$IFDEF SERVER}
          // mulitkill count
          Sprite[Who].MultiKillTime := MULTIKILLINTERVAL;
          Inc(Sprite[Who].MultiKills);
          {$ENDIF}
        end;
      end;
      if sv_gamemode.Value = GAMESTYLE_HTF then
      begin
        if IsNotInSameTeam(Sprite[Who]) then
        begin
          Inc(Sprite[Who].Player.Kills);
          {$IFDEF SERVER}
          // mulitkill count
          Sprite[Who].MultiKillTime := MULTIKILLINTERVAL;
          Inc(Sprite[Who].MultiKills);
          {$ENDIF}
        end;
      end;
      if sv_gamemode.Value = GAMESTYLE_RAMBO then
      begin
        if What > 0 then
          i := Bullet[What].OwnerWeapon
        else
          i := -1;
        if (i = Guns[BOW].Num) or  // Shooter is Rambo
           (i = Guns[BOW2].Num) or
           (Weapon.Num = Guns[BOW].Num) or  // Shootee is Rambo
           (Weapon.Num = Guns[BOW2].Num) then
        begin
          Inc(Sprite[Who].Player.Kills);
          {$IFDEF SERVER}
          // mulitkill count
          Sprite[Who].MultiKillTime := MULTIKILLINTERVAL;
          Inc(Sprite[Who].MultiKills);
          {$ENDIF}
        end
        else
        begin
          // Punish for killing non-Rambos when someone is Rambo
          for i := 1 to MAX_PLAYERS do
            if (Sprite[i].Weapon.Num = Guns[BOW].Num) or
               (Sprite[i].Weapon.Num = Guns[BOW2].Num) then
              if Sprite[Who].Player.Kills > 0 then
              begin
                Dec(Sprite[Who].Player.Kills);
                Break;
              end;
        end;
      end;
    end;

    if IdleRandom = 7 then
    begin
      if Weapon.Num = Guns[NOWEAPON].Num then
        How := BRUTAL_DEATH;
    end;

    BodyAnimation.CurrFrame := 0;

    // console message about kill
    // game log
    if (What > 0) then
    begin
      {$IFDEF SERVER}
      S := WeaponNameByNum(Bullet[What].OwnerWeapon);
      if Bullet[What].OwnerWeapon = 0 then S := 'USSOCOM';
      if Bullet[What].Style = BULLET_STYLE_FRAGNADE then S := 'Grenade';
      if Bullet[What].Style = BULLET_STYLE_CLUSTER then S := 'Cluster Grenades';
      if Bullet[What].Style = BULLET_STYLE_PUNCH then S := Guns[NOWEAPON].Name;
      if Bullet[What].Style = BULLET_STYLE_M2 then S := 'Stationary gun';
      //if Bullet[What].OwnerWeapon = Guns[NOWEAPON].Num then S := 'Selfkill';
      {$ELSE}
      S := WeaponNameByNum(What);
      if What = 222 then S := 'Grenade';
      if What = 210 then S := 'Clusters';
      if What = 211 then S := Guns[KNIFE].Name;
      if What = 212 then S := Guns[CHAINSAW].Name;
      if What = 224 then S := Guns[LAW].Name;
      if What = 225 then S := 'Stationary gun';
      if What = 205 then S := Guns[FLAMER].Name;
      if What = 207 then S := Guns[BOW].Name;
      if What = 208 then S := Guns[BOW2].Name;
      if What = 206 then S := Guns[NOWEAPON].Name;
      if What = 250 then S := 'Selfkill';
      {$ENDIF}
    end else
    begin
      S := 'Selfkill';
    end;

    {$IFNDEF SERVER}
    if (Who <> Num) and (Who = MySprite) then
      for i := 0 to 20 do
        if WepStats[i].Name = S then
        begin
          WepStats[i].Kills := WepStats[i].Kills + 1;
          if Where = 12 {head} then
            WepStats[i].Headshots := WepStats[i].Headshots + 1;
        end;
    {$ENDIF}

    {$IFDEF SERVER}
    // console message for kills
    if (sv_echokills.Value) and not (Sprite[Who].Player.Name = Player.Name) then
    begin
      MainConsole.Console('(' + IntToStr(Sprite[Who].Player.Team) + ') ' +
        Sprite[Who].Player.Name + ' killed (' + IntToStr(Player.Team) +
        ') ' + Player.Name + ' with ' + S, 0);
    end;
    {$ENDIF}

    {$IFDEF SCRIPT}
    // COMMENT: Sprite[Num].Num = Num?
    ScrptDispatcher.OnPlayerKill(Sprite[Who].Num, Sprite[Num].Num, Byte(What));
    {$ENDIF}

    // console message about kill
    // game log
    {$IFDEF SERVER}
    begin
      if (log_enable.Value) then
      begin
        S2 := FormatDateTime('yy/mm/dd', Date);
        S2 := S2 + ' ' + FormatDateTime('hh:nn:ss', Time);
        AddLineToLogFile(KillLog, '--- ' + S2, KillLogFileName, False);
        AddLineToLogFile(KillLog, Sprite[Who].Player.Name, KillLogFileName, False);
        AddLineToLogFile(KillLog, Sprite[Num].Player.Name, KillLogFileName, False);
        AddLineToLogFile(KillLog, S, KillLogFileName, False);
      end;

      // Bot Chat
      if bots_chat.Value then
      begin
        if Player.ControlMethod = BOT then
          if Random(Brain.ChatFreq div 2) = 0 then
            ServerSendStringMessage(WideString(Brain.ChatDead),
              ALL_PLAYERS, Num, MSGTYPE_PUB);
        if (Who <> Num) and (Sprite[Who].Player.ControlMethod = BOT) then
          if Random(Sprite[Who].Brain.ChatFreq div 3) = 0 then
            ServerSendStringMessage(WideString(Sprite[Who].Brain.ChatKill),
              ALL_PLAYERS, Who, MSGTYPE_PUB);
      end;

      k := Weapon.HitMultiply;

      LastWeaponHM := Weapon.HitMultiply;
      LastWeaponStyle := Weapon.BulletStyle;
      LastWeaponSpeed := Weapon.Speed;
      LastWeaponFire := Weapon.FireInterval;
      LastWeaponReload := Weapon.ReloadTime;

      i := DropWeapon();
      Weapon.HitMultiply := k;

      if (i > 0) and
         (Weapon.Num <> Guns[FLAMER].Num) and
         (Weapon.Num <> Guns[NOWEAPON].Num) then
      begin
        Thing[i].Skeleton.Forces[2] := Impact;
      end;

      FreeControls;
    end;
    {$ENDIF}

    {$IFNDEF SERVER}
    if ((Who = MySprite) or (Num = MySprite)) and
       (What > 0) and IsPointOnScreen(Skeleton.Pos[9]) then
      if (Who = MySprite) and (Num = MySprite) then
      else
      begin
        ScreenCounter := 5;
        CapScreen := 4;
      end;
    {$ENDIF}
  end;

  {$IFDEF SERVER}
  if What > 0 then
  begin
    if ((What = 1) and (Where = 1)) or (Bullet[What].Style = BULLET_STYLE_FLAME) or DeadMeat then
    else
      begin
        a := Vec2Subtract(BulletParts.Pos[What], Bullet[What].Initial);
        ShotDistance := Vec2Length(a) / 14;
        ShotRicochet := Bullet[What].RicochetCount;
        ShotLife := (MainTickCounter - Bullet[What].StartUpTime) / 60;
      end;
  end;

  Trace('TSprite.Die 2');
  {$ENDIF}

  {$IFNDEF SERVER}
  if What > 0 then
    if (Where = 12) and ((Bullet[What].OwnerWeapon = Guns[RUGER77].Num)) then
      How := HEADCHOP_DEATH;
  {$ENDIF}
  case How of
    NORMAL_DEATH:
      begin
        {$IFNDEF SERVER}
        // the sound of death...
        if not DeadMeat then
          PlaySound(SFX_DEATH + Random(3), SpriteParts.Pos[Num]);
        {$ENDIF}
      end;

    HEADCHOP_DEATH:
      begin
        {$IFNDEF SERVER}if DeadMeat then{$ENDIF}
        begin
          if Where = 12 then
            Skeleton.Constraints[20].Active := False;
          if Where = 3 then
            Skeleton.Constraints[2].Active := False;
          if Where = 4 then
            Skeleton.Constraints[4].Active := False;
        end;

        {$IFNDEF SERVER}
        if What > 0 then
          if not DeadMeat and (Where = 12) and
             ((Bullet[What].OwnerWeapon = Guns[BARRETT].Num) or
              (Bullet[What].OwnerWeapon = Guns[RUGER77].Num)) then
          begin
            Randomize;
            if Random(100) > 50 then
            begin
              Skeleton.Constraints[20].Active := True; // Keep head attached to corpse
              for i := 0 to 50 do
              begin
                a.x := Skeleton.Pos[9].x + (cos(deg2rad(360 / 50 * i)) * 2);
                a.y := Skeleton.Pos[9].y + (sin(deg2rad(360 / 50 * i)) * 2);
                Randomize;
                //FIXME: Causes range check error
                //RandSeed := RandSeed * i;
                b.x := (cos(deg2rad(360 / 50 * i)) * RandomRange(1, 3));
                b.y := (sin(deg2rad(360 / 50 * i)) * RandomRange(1, 3));
                CreateSpark(a, b, util.iif(i < 25, RandomRange(4, 5), 5), Num,
                  100 - Random(20));
              end;
            end;

            if (Bullet[What].OwnerWeapon = Guns[BARRETT].Num) then
              // corpse explode
              PlaySound(SFX_BRYZG, Sprite[Num].Skeleton.Pos[12]);

            if Who = MySprite then
              PlaySound(SFX_BOOMHEADSHOT);
          end;

        // siup leb!
        if not DeadMeat then
        begin
          PlaySound(SFX_HEADCHOP, Sprite[Num].Skeleton.Pos[12]);
        end;
        {$ENDIF}
      end;

    BRUTAL_DEATH:
      begin
        {$IFNDEF SERVER}if DeadMeat then{$ENDIF}
        begin
          Skeleton.Constraints[2].Active := False;
          Skeleton.Constraints[4].Active := False;
          Skeleton.Constraints[20].Active := False;
          Skeleton.Constraints[21].Active := False;
          Skeleton.Constraints[23].Active := False;
        end;

        {$IFNDEF SERVER}
        // play bryzg sound!
        PlaySound(SFX_BRYZG, Sprite[Num].Skeleton.Pos[12]);
        {$ENDIF}
      end;
  end;  // case

  {$IFNDEF SERVER}
  if DeadMeat then
  {$ENDIF}
    if Sprite[Who].BonusStyle = BONUS_BERSERKER then
    begin
      Skeleton.Constraints[2].Active := False;
      Skeleton.Constraints[4].Active := False;
      Skeleton.Constraints[20].Active := False;
      Skeleton.Constraints[21].Active := False;
      Skeleton.Constraints[23].Active := False;

      {$IFNDEF SERVER}
      PlaySound(SFX_KILLBERSERK, Sprite[Num].Skeleton.Pos[12]);
      {$ENDIF}
    end;

  {$IFNDEF SERVER}
  if not DeadMeat and (What > 0) then
    if Bullet[What].OwnerWeapon = Guns[FLAMER].Num then
      PlaySound(SFX_BURN, Sprite[Num].Skeleton.Pos[12]);
  {$ENDIF}

  if not DeadMeat and (HasCigar = 10) then
  begin
    {$IFNDEF SERVER}
    CreateSpark(Skeleton.Pos[12], Impact, 34, Num, 245);
    {$ENDIF}
    HasCigar := 0;
  end;

  // Survival Mode
  if sv_survivalmode.Value then
    if not DeadMeat then
    begin
      if (sv_gamemode.Value = GAMESTYLE_DEATHMATCH) or
        (sv_gamemode.Value = GAMESTYLE_RAMBO) then
      begin
        AliveNum := 0;

        for i := 1 to MAX_SPRITES do
          if Sprite[i].Active and not Sprite[i].DeadMeat and
             Sprite[i].IsNotSpectator() then
            Inc(AliveNum);

        Dec(AliveNum);

        if AliveNum < 2 then
        begin
          for i := 1 to MAX_SPRITES do
            if (Sprite[i].Active) then
              Sprite[i].RespawnCounter := SURVIVAL_RESPAWNTIME;

          SurvivalEndRound := True;

          {$IFNDEF SERVER}
          for i := 1 to MAX_SPRITES do
            if Sprite[i].Active and
               not Sprite[i].DeadMeat and
               (Num <> i) and //not the current player
               Sprite[i].IsNotSpectator() then
            begin
              PlaySound(SFX_ROAR, SpriteParts.Pos[i]);
            end;
          {$ENDIF}
        end;

        {$IFNDEF SERVER}
        if not Sprite[MySprite].DeadMeat then
        {$ENDIF}
          MainConsole.Console(
            {$IFDEF SERVER}
            'Players left: ' +
            {$ELSE}
            _('Players left:') + ' ' +
            {$ENDIF}
            WideString(IntToStr(AliveNum)), GAME_MESSAGE_COLOR);
      end;

      if (sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF) or
         (sv_gamemode.Value = GAMESTYLE_HTF) or (sv_gamemode.Value = GAMESTYLE_TEAMMATCH) then
      begin
        TeamAliveNum[1] := 0;
        TeamAliveNum[2] := 0;
        TeamAliveNum[3] := 0;
        TeamAliveNum[4] := 0;

        for i := 1 to MAX_SPRITES do
        begin
          if Sprite[i].Active and not Sprite[i].DeadMeat and
             (Sprite[i].Player.Team = TEAM_ALPHA) then
            Inc(TeamAliveNum[TEAM_ALPHA]);
          if Sprite[i].Active and not Sprite[i].DeadMeat and
             (Sprite[i].Player.Team = TEAM_BRAVO) then
            Inc(TeamAliveNum[TEAM_BRAVO]);
          if Sprite[i].Active and not Sprite[i].DeadMeat and
             (Sprite[i].Player.Team = TEAM_CHARLIE) then
            Inc(TeamAliveNum[TEAM_CHARLIE]);
          if Sprite[i].Active and not Sprite[i].DeadMeat and
             (Sprite[i].Player.Team = TEAM_DELTA) then
            Inc(TeamAliveNum[TEAM_DELTA]);
        end;

        Dec(TeamAliveNum[Player.Team]);

        AliveNum := TeamAliveNum[1] + TeamAliveNum[2] + TeamAliveNum[3] +
          TeamAliveNum[4];

        if ((TeamAliveNum[1] > 0) and (TeamAliveNum[2] < 1) and
            (TeamAliveNum[3] < 1) and (TeamAliveNum[4] < 1)) or
           ((TeamAliveNum[2] > 0) and (TeamAliveNum[1] < 1) and
            (TeamAliveNum[3] < 1) and (TeamAliveNum[4] < 1)) or
           ((TeamAliveNum[3] > 0) and (TeamAliveNum[1] < 1) and
            (TeamAliveNum[2] < 1) and (TeamAliveNum[4] < 1)) or
           ((TeamAliveNum[4] > 0) and (TeamAliveNum[1] < 1) and
            (TeamAliveNum[2] < 1) and (TeamAliveNum[3] < 1)) or
           ((TeamAliveNum[1] < 1) and (TeamAliveNum[2] < 1) and
            (TeamAliveNum[3] < 1) and (TeamAliveNum[4] < 1)) then
        begin
          for i := 1 to MAX_SPRITES do
            if Sprite[i].Active then
              Sprite[i].RespawnCounter := SURVIVAL_RESPAWNTIME;

          if not SurvivalEndRound then
            if sv_gamemode.Value = GAMESTYLE_CTF then
            begin
              if TeamAliveNum[1] > 0 then
                Inc(TeamScore[1], 1);
              if TeamAliveNum[2] > 0 then
                Inc(TeamScore[2], 1);
            end;
          if not SurvivalEndRound then
            if sv_gamemode.Value = GAMESTYLE_INF then
            begin
              if TeamAliveNum[1] > 0 then
                Inc(TeamScore[1], sv_inf_redaward.Value);

              // penalty
              if (PlayersTeamNum[1] > PlayersTeamNum[2]) then
                Dec(TeamScore[1], 5 * (PlayersTeamNum[1] - PlayersTeamNum[2]));
              if (TeamScore[1] < 0) then
                TeamScore[1] := 0;
            end;

          SurvivalEndRound := True;

          for i := 1 to MAX_SPRITES do
            if Sprite[i].Active and not Sprite[i].DeadMeat then
            begin
              Sprite[i].IdleRandom := 5;
              Sprite[i].IdleTime := 1;
            end;
        end;

        {$IFNDEF SERVER}
        if MySprite > 0 then
          if IsInSameTeam(Sprite[MySprite]) then
            if not Sprite[MySprite].DeadMeat then
              MainConsole.Console(_('Players left on your team:') + ' ' +
                WideString(IntToStr(TeamAliveNum[Sprite[MySprite].Player.Team])),
                GAME_MESSAGE_COLOR);
        {$ENDIF}
      end;
    end;

  {$IFDEF SERVER}
  // Fire on from bullet
  if What > 0 then
  begin
    if Bullet[What].Style = BULLET_STYLE_FRAGNADE then
      if Random(12) = 0 then
        OnFire := 4;

    if Bullet[What].Style = BULLET_STYLE_M79 then
      if Random(8) = 0 then
        OnFire := 2;

    if Bullet[What].Style = BULLET_STYLE_FLAME then
      OnFire := 1;

    if Bullet[What].Style = BULLET_STYLE_FLAMEARROW then
      if Random(4) = 0 then
        OnFire := 1;

    if Bullet[What].Style = BULLET_STYLE_CLUSTER then
      if Random(3) = 0 then
        OnFire := 3;
  end;
  {$ENDIF}

  for i := 1 to MAX_THINGS do
  begin
    if Thing[i].HoldingSprite = Num then
      if Thing[i].Style < OBJECT_USSOCOM then
      begin
        Thing[i].HoldingSprite := 0;
        HoldedThing := 0;
        {$IFNDEF SERVER}
        if (Thing[i].Style = OBJECT_ALPHA_FLAG) or (Thing[i].Style = OBJECT_BRAVO_FLAG) then
        begin
          MainConsole.Console(WideFormat(_('%s dropped the %s Flag'),
            [Player.Name,
              iif(Player.Team = TEAM_ALPHA, _('Blue'), _('Red'))]),
            iif(Player.Team = TEAM_ALPHA, Longint(BRAVO_MESSAGE_COLOR), Longint(ALPHA_MESSAGE_COLOR)));

          if IsInSameTeam(Sprite[MySprite]) then
          begin
            if Thing[i].Style = OBJECT_ALPHA_FLAG then  // Alpha
              BigMessage(WideFormat(_('%s Flag dropped!'), [_('Red')]),
                CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR)
            else if Thing[i].Style = OBJECT_BRAVO_FLAG then  // Bravo
              BigMessage(WideFormat(_('%s Flag dropped!'), [_('Blue')]),
                CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);

            PlaySound(SFX_INFILT_POINT);
          end;
        end;
        {$ENDIF}
        {$IFDEF SCRIPT}
        ScrptDispatcher.OnFlagDrop(Num, Thing[i].Style, False);
        {$ENDIF}
      end;

    if Thing[i].Owner = Num then
      Thing[i].Owner := 255;

    if Stat = Num then
    begin
      Stat := 0;
      Thing[i].StaticType := True;
    end;
  end;

  // send net info, so the death is smooth
  {$IFDEF SERVER}
  if not DeadMeat then
    ServerSpriteDeath(Num, Who, What, Where);

  Trace('TSprite.Die 3');
  {$ENDIF}

  {$IFNDEF SERVER}
  StopSound(ReloadSoundChannel);
  {$ENDIF}

  // BREAD
  {$IFDEF SERVER}
  if not sv_advancemode.Value then
    if not DeadMeat and (Num <> Who) then
  {$ELSE}
  if sv_advancemode.Value then
    if not DeadMeat then
  {$ENDIF}
    begin
      i := sv_advancemode_amount.Value;

      {$IFNDEF SERVER}
      if (Num <> Who) and
         (Sprite[Num].IsNotInSameTeam(Sprite[Who]) or Sprite[Num].IsSolo()) then
      {$ENDIF}
      begin
        if (Sprite[Who].Player.Kills mod i) = 0 then
        begin
          j := 0;
          for i := 1 to PRIMARY_WEAPONS do
            if (WeaponSel[Who][i] = 0) and (WeaponActive[i] = 1) then
              j := 1;

          if j = 1 then
          begin
            repeat
              j := Random(PRIMARY_WEAPONS) + 1;
            until (WeaponSel[Who][j] = 0) and (WeaponActive[j] = 1);
            WeaponSel[Who][j] := 1;
          end;
        end;
      end;

      i := sv_advancemode_amount.Value;

      if (Sprite[Num].Player.Deaths mod i) = 0 then
      begin
        j := 0;
        for i := 1 to PRIMARY_WEAPONS do
          if (WeaponSel[Num][i] = 1) then
            j := 1;

        if j = 1 then
        begin
          repeat
            j := Random(PRIMARY_WEAPONS) + 1;
          until (WeaponSel[Num][j] = 1);
          WeaponSel[Num][j] := 0;
        end;
      end;

      {$IFNDEF SERVER}
      if (Num = MySprite) or (Who = MySprite) then
        for i := 1 to PRIMARY_WEAPONS do
          if WeaponActive[i] = 1 then
            LimboMenu.Button[i - 1].Active := Boolean(WeaponSel[MySprite][i]);
      {$ENDIF}
    end;

  DeadMeat := True;
  {$IFDEF SERVER}
  HoldedThing := 0;
  {$ENDIF}
  Alpha := 255;
  Vest := 0;
  BonusStyle := BONUS_NONE;
  BonusTime := 0;
  if (DeadTime > 0) and (OnFire = 0) then
    DeadTime := DeadTime div 2
  else
    DeadTime := 0;

  SpriteParts.Velocity[Num].X := 0;
  SpriteParts.Velocity[Num].Y := 0;
  Sprite[Who].Brain.PissedOff := 0;

  // sort the players frag list
  SortPlayers;
end;

function TSprite.DropWeapon(): Integer;
begin
  Result := -1;

  {$IFDEF SERVER}
  Trace('TSprite.DropWeapon');
  {$ENDIF}

  WeaponsCleaned := False;
  // drop weapon
  {$IFDEF SERVER}
  if Weapon.Num = Guns[COLT].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_USSOCOM,      255)
  else if Weapon.Num = Guns[EAGLE].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_DESERT_EAGLE, 255)
  else if Weapon.Num = Guns[MP5].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_HK_MP5,       255)
  else if Weapon.Num = Guns[AK74].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_AK74,         255)
  else if Weapon.Num = Guns[STEYRAUG].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_STEYR_AUG,    255)
  else if Weapon.Num = Guns[SPAS12].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_SPAS12,       255)
  else if Weapon.Num = Guns[RUGER77].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_RUGER77,      255)
  else if Weapon.Num = Guns[M79].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_M79,          255)
  else if Weapon.Num = Guns[BARRETT].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_BARRET_M82A1, 255)
  else if Weapon.Num = Guns[M249].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_MINIMI,       255)
  else if Weapon.Num = Guns[MINIGUN].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_MINIGUN,      255)
  else if Weapon.Num = Guns[KNIFE].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_COMBAT_KNIFE, 255)
  else if Weapon.Num = Guns[CHAINSAW].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_CHAINSAW,     255)
  else if Weapon.Num = Guns[LAW].Num then
    Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_LAW,          255);

  if sv_gamemode.Value = GAMESTYLE_RAMBO then
  begin
    if (Weapon.Num = Guns[BOW].Num) or
        (Weapon.Num = Guns[BOW2].Num) then
    begin
      Result := CreateThing(Skeleton.Pos[16], Num, OBJECT_RAMBO_BOW, 255);
      {$IFNDEF SERVER}
      GameThingTarget := Result;
      {$ENDIF}
    end;
  end;

  if Result > 0 then
    Thing[Result].AmmoCount := Weapon.AmmoCount;

  // This should be called before weapon is actually applied
  // so that sprite still holds old values
  {$IFDEF SCRIPT}
  if Result > 0 then
  begin
    // event must be before actual weapon apply.
    // script might've called ForceWeapon, which we should check.
    // if it did, we don't apply snapshot weapon's as they were already applied
    // by force weapon.
    ForceWeaponCalled := False;
    ScrptDispatcher.OnWeaponChange(Num, Guns[NOWEAPON].Num, SecondaryWeapon.Num,
      Guns[NOWEAPON].AmmoCount, SecondaryWeapon.AmmoCount);
  end;

  if not ForceWeaponCalled then
  {$ENDIF}
    ApplyWeaponByNum(Guns[NOWEAPON].Num, 1);
  {$ENDIF}
end;

procedure TSprite.LegsApplyAnimation(Anim: TAnimation; Curr: Integer);
begin
  {$IFDEF SERVER}
  Trace('TSprite.LegsApplyAnimation');
  {$ENDIF}

  if (LegsAnimation.ID = Prone.ID) or
     (LegsAnimation.ID = ProneMove.ID) then
    Exit;

  if Anim.ID <> LegsAnimation.ID then
  begin
    LegsAnimation := Anim;
    LegsAnimation.CurrFrame := Curr;
  end;
end;

procedure TSprite.BodyApplyAnimation(Anim: TAnimation; Curr: Integer);
begin
  {$IFDEF SERVER}
  Trace('TSprite.BodyApplyAnimation');
  {$ENDIF}

  {$IFNDEF SERVER}
  if Anim.ID = Stand.ID then
    if wasReloading then
    begin
      BodyApplyAnimation(Reload, 1);
      wasReloading := False;
      Exit;
    end;
  {$ENDIF}

  if Anim.ID <> BodyAnimation.ID then
  begin
    BodyAnimation := Anim;
    BodyAnimation.CurrFrame := Curr;
  end;
end;

procedure TSprite.MoveSkeleton(x1, y1: Single; FromZero: Boolean);
var
  i: Integer;
begin
  {$IFDEF SERVER}
  Trace('TSprite.MoveSkeleton');
  {$ENDIF}

  if not FromZero then
    for i := 1 to NUM_PARTICLES do
      if Skeleton.Active[i] then
      begin
        Skeleton.Pos[i].X := Skeleton.Pos[i].X + x1;
        Skeleton.Pos[i].Y := Skeleton.Pos[i].Y + y1;
        Skeleton.OldPos[i] := Skeleton.Pos[i];
      end;

  if FromZero then
    for i := 1 to NUM_PARTICLES do
      if Skeleton.Active[i] then
      begin
        Skeleton.Pos[i].X := x1;
        Skeleton.Pos[i].Y := y1;
        Skeleton.OldPos[i] := Skeleton.Pos[i];
      end;
end;

function TSprite.CheckRadiusMapCollision(X, Y: Single; HasCollided: Boolean): Boolean;
var
  j, w, k, z, PolyType: Integer;
  b: Integer = 0;
  SPos, Pos, Perp, Step: TVector2;
  Norm: TVector2;
  P1, P2, P3: TVector2;
  D: Single = 0.0;
  DetAcc: Integer;
  rx, ry: Integer;
  teamcol: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TSprite.CheckRadiusMapCollision');
  {$ENDIF}

  Result := False;
  P1 := Default(TVector2);
  P2 := Default(TVector2);
  SPos.X := X;
  SPos.Y := Y - 3;

  // make step
  DetAcc := Trunc(Vec2Length(SpriteParts.Velocity[Num]));
  if DetAcc = 0 then
    DetAcc := 1;
  Vec2Scale(Step, SpriteParts.Velocity[Num], 1 / DetAcc);

  // make steps for accurate collision detection
  for z := 0 to DetAcc - 1 do
  begin
    SPos.X := SPos.X + Step.X;
    SPos.Y := SPos.Y + Step.Y;

    // iterate through maps sector polygons
    rx := Round(SPos.X / Map.SectorsDivision);
    ry := Round(SPos.Y / Map.SectorsDivision);
    if (rx > -Map.SectorsNum) and (rx < Map.SectorsNum) and
      (ry > -Map.SectorsNum) and (ry < Map.SectorsNum) then
    begin
      for j := 1 to High(Map.Sectors[rx, ry].Polys) do
      begin
        w := Map.Sectors[rx, ry].Polys[j];
        PolyType := Map.PolyType[w];

        teamcol := TeamCollides(w, Player.Team, False);

        if ((HoldedThing = 0) and (PolyType = POLY_TYPE_ONLY_FLAGGERS)) or
           ((HoldedThing <> 0) and (PolyType = POLY_TYPE_NOT_FLAGGERS)) then
          teamcol := False;
        if teamcol and
           (PolyType <> POLY_TYPE_DOESNT) and (PolyType <> POLY_TYPE_ONLY_BULLETS) then
        begin
          for k := 1 to 3 do
          begin
            Norm := Map.Perp[w][k];
            Vec2Scale(Norm, Norm, -SPRITE_COL_RADIUS);

            Pos := Vec2Add(SPos, Norm);

            if Map.PointInPolyEdges(Pos.X, Pos.Y, w) then
            begin
              if BGState.BackgroundTest(w) then
                Continue;

            if not HasCollided then
              HandleSpecialPolyTypes(PolyType, Pos);

              Perp := Map.ClosestPerpendicular(w, SPos, D, b);

              case b of
                1:
                  begin
                    P1.X := Map.Polys[w].Vertices[1].X;
                    P1.Y := Map.Polys[w].Vertices[1].Y;
                    P2.X := Map.Polys[w].Vertices[2].X;
                    P2.Y := Map.Polys[w].Vertices[2].Y;
                  end;
                2:
                  begin
                    P1.X := Map.Polys[w].Vertices[2].X;
                    P1.Y := Map.Polys[w].Vertices[2].Y;
                    P2.X := Map.Polys[w].Vertices[3].X;
                    P2.Y := Map.Polys[w].Vertices[3].Y;
                  end;
                3:
                  begin
                    P1.X := Map.Polys[w].Vertices[3].X;
                    P1.Y := Map.Polys[w].Vertices[3].Y;
                    P2.X := Map.Polys[w].Vertices[1].X;
                    P2.Y := Map.Polys[w].Vertices[1].Y;
                  end;
              end;

              P3 := Pos;
              D := PointLineDistance(P1, P2, P3);
              Vec2Scale(Perp, Perp, D);

              SpriteParts.Pos[Num] := SpriteParts.OldPos[Num];
              SpriteParts.Velocity[Num] := Vec2Subtract(SpriteParts.Forces[Num], Perp);

              Result := True;
              Exit;
            end;  // PointInPolyEdges
          end;
        end;
      end;  // for j
    end;
  end;  // n
end;

function TSprite.CheckMapCollision(X, Y: Single; Area: Integer): Boolean;
var
  j, w, PolyType: Integer;
  k: Integer = 0;
  SPos, Pos, Perp, Step: TVector2;
  D: Single = 0.0;
  rx, ry: Integer;
  teamcol: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TSprite.CheckMapCollision');
  {$ENDIF}

  Result := False;
  SPos.X := X;
  SPos.Y := Y;

  Pos.X := SPos.X + SpriteParts.Velocity[Num].X;
  Pos.Y := SPos.Y + SpriteParts.Velocity[Num].Y;

  // iterate through maps sector polygons
  rx := Round(Pos.X / Map.SectorsDivision);
  ry := Round(Pos.Y / Map.SectorsDivision);
  if (rx > -Map.SectorsNum) and (rx < Map.SectorsNum) and
    (ry > -Map.SectorsNum) and (ry < Map.SectorsNum) then
  begin
    BGState.BackgroundTestBigPolyCenter(Pos);

    for j := 1 to High(Map.Sectors[rx, ry].Polys) do
    begin
      w := Map.Sectors[rx, ry].Polys[j];
      PolyType := Map.PolyType[w];

      teamcol := TeamCollides(w, Player.Team, False);

      if ((PolyType <> POLY_TYPE_DOESNT) and (PolyType <> POLY_TYPE_ONLY_BULLETS) and teamcol and
          (PolyType <> POLY_TYPE_ONLY_FLAGGERS) and (PolyType <> POLY_TYPE_NOT_FLAGGERS)) or
         ((HoldedThing <> 0) and (PolyType = POLY_TYPE_ONLY_FLAGGERS)) or
         ((HoldedThing = 0) and (PolyType = POLY_TYPE_NOT_FLAGGERS)) then
      begin
        if Map.PointInPoly(Pos, Map.Polys[w]) then
        begin
          if BGState.BackgroundTest(w) then
            Continue;

          {$IFDEF SERVER}
          Sprite[Num].Player.StandingPolyType := PolyType;
          {$ENDIF}

          HandleSpecialPolyTypes(PolyType, Pos);

          {$IFNDEF SERVER}
          if (Abs(SpriteParts.Velocity[Num].Y) > 2.2) and
             (Abs(SpriteParts.Velocity[Num].Y) < 3.4) and
             (PolyType <> POLY_TYPE_BOUNCY) then
            PlaySound(SFX_FALL, SpriteParts.Pos[Num]);
          {$ENDIF}

          if Abs(SpriteParts.Velocity[Num].Y) > 3.5 then
          begin
            {$IFNDEF SERVER}
            PlaySound(SFX_FALL_HARD, SpriteParts.Pos[Num]);
            {$ENDIF}

            // Hit ground
            if sv_realisticmode.Value then
              if (SpriteParts.Velocity[Num].Y > 3.5) and (PolyType <> POLY_TYPE_BOUNCY) then
              begin
                HealthHit(SpriteParts.Velocity[Num].Y * 5, Num, 12, -1, SPos);
                {$IFNDEF SERVER}
                PlaySound(SFX_FALL, SpriteParts.Pos[Num]);
                {$ENDIF}
              end;
          end;

          {$IFNDEF SERVER}
          // Run
          if ((LegsAnimation.ID = Run.ID) or
              (LegsAnimation.ID = RunBack.ID)) and
             ((LegsAnimation.CurrFrame = 16) or
              (LegsAnimation.CurrFrame = 32)) then
          begin
            if r_maxsparks.Value > (MAX_SPARKS - 10) then
              if Abs(SpriteParts.Velocity[Num].X) > 1.0 then
              begin
                SPos.X := SpriteParts.Velocity[Num].X / 4;
                SPos.Y := -0.8;
                Vec2Scale(SPos, SPos, 0.4 + Random(4) / 10);
                CreateSpark(Pos, SPos, 1, Num, 70);
              end;

            if r_maxsparks.Value > (MAX_SPARKS - 10) then
              if (((Direction = 1) and (SpriteParts.Velocity[Num].X < 0.01)) or
                  ((Direction = -1) and (SpriteParts.Velocity[Num].X > 0.01))) and
                 (LegsAnimation.ID = Run.ID) then
              begin
                SPos.X := SpriteParts.Velocity[Num].X / 4;
                SPos.Y := -1.3;
                Vec2Scale(SPos, SPos, 0.4 + Random(4) / 10);
                CreateSpark(Pos, SPos, 1, Num, 70);
              end;

            if Map.Steps = 0 then
              PlaySound(SFX_STEP + Random(4), SpriteParts.Pos[Num]);
            if Map.Steps = 1 then
              PlaySound(SFX_STEP5 + Random(4), SpriteParts.Pos[Num]);


            if Map.Weather = 1 then
              PlaySound(SFX_WATER_STEP, SpriteParts.Pos[Num]);
          end;

          // Crouch
          if ((LegsAnimation.ID = CrouchRun.ID) or
              (LegsAnimation.ID = CrouchRunBack.ID)) and
             ((LegsAnimation.CurrFrame = 15) or
              (LegsAnimation.CurrFrame = 1)) and
             (LegsAnimation.Count = 1) then
          begin
            if Random(2) = 0 then
              PlaySound(SFX_CROUCH_MOVE, SpriteParts.Pos[Num])
            else if Random(2) = 0 then
              PlaySound(SFX_CROUCH_MOVEL, SpriteParts.Pos[Num]);
          end;

          // Prone
          if (LegsAnimation.ID = ProneMove.ID) and
             (LegsAnimation.CurrFrame = 8) and
             (LegsAnimation.Count = 1) then
          begin
            PlaySound(SFX_PRONE_MOVE, SpriteParts.Pos[Num]);
          end;

          if (Abs(SpriteParts.Velocity[Num].X) > 2.4) and
             (LegsAnimation.ID <> Run.ID) and
             (LegsAnimation.ID <> RunBack.ID) and
             (Random(4) = 0) then
          begin
            SPos.X := SpriteParts.Velocity[Num].X / 4;
            SPos.Y := -0.9;
            Vec2Scale(SPos, SPos, 0.4 + Random(4) / 10);
            CreateSpark(Pos, SPos, 1, Num, 70);
          end;
          {$ENDIF}

          Perp := Map.ClosestPerpendicular(w, Pos, D, k);
          Step := Perp;

          Vec2Normalize(Perp, Perp);
          Vec2Scale(Perp, Perp, D);

          D := Vec2Length(SpriteParts.Velocity[Num]);
          if Vec2Length(Perp) > D then
          begin
            Vec2Normalize(Perp, Perp);
            Vec2Scale(Perp, Perp, D);
          end;

          if (Area = 0) or
             ((Area = 1) and
              ((SpriteParts.Velocity[Num].Y < 0) or
               (SpriteParts.Velocity[Num].X > SLIDELIMIT) or
               (SpriteParts.Velocity[Num].X < -SLIDELIMIT))) then
          begin
            SpriteParts.OldPos[Num] := SpriteParts.Pos[Num];
            SpriteParts.Pos[Num] := Vec2Subtract(SpriteParts.Pos[Num], Perp);
            if PolyType = POLY_TYPE_BOUNCY then  // bouncy polygon
            begin
              Vec2Normalize(Perp, Perp);
              Vec2Scale(Perp, Perp, Map.Bounciness[w] * D);
              {$IFNDEF SERVER}
              if Vec2Length(Perp) > 1 then
              begin
                PlaySound(SFX_BOUNCE, SpriteParts.Pos[Num]);
              end;
              {$ENDIF}
            end;
            SpriteParts.Velocity[Num] := Vec2Subtract(SpriteParts.Velocity[Num], Perp);
          end;

          if Area = 0 then
          begin
            if (LegsAnimation.ID = Stand.ID) or
               (LegsAnimation.ID = Crouch.ID) or
               (LegsAnimation.ID = Prone.ID) or
               (LegsAnimation.ID = ProneMove.ID) or
               (LegsAnimation.ID = GetUp.ID) or
               (LegsAnimation.ID = Fall.ID) or
               (LegsAnimation.ID = Mercy.ID) or
               (LegsAnimation.ID = Mercy2.ID) or
               (LegsAnimation.ID = Own.ID) then
            begin
              if (SpriteParts.Velocity[Num].X < SLIDELIMIT) and
                 (SpriteParts.Velocity[Num].X > -SLIDELIMIT) and
                 (Step.Y > SLIDELIMIT) then
              begin
                SpriteParts.Pos[Num] := SpriteParts.OldPos[Num];
                SpriteParts.Forces[Num].Y := SpriteParts.Forces[Num].Y - GRAV;
              end
              else
              begin
                {$IFNDEF SERVER}
                if r_maxsparks.Value > (MAX_SPARKS - 10) then
                  if Random(15) = 0 then
                  begin
                    SPos.X := SpriteParts.Velocity[Num].X * 3;
                    SPos.Y := -SpriteParts.Velocity[Num].Y * 2;
                    Vec2Scale(SPos, SPos, 0.4 + Random(4) / 10);
                    CreateSpark(Pos, SPos, 1, Num, 70);
                  end;
                {$ENDIF}
              end;

              if (Step.Y > SLIDELIMIT) and (PolyType <> POLY_TYPE_ICE) and (PolyType <> POLY_TYPE_BOUNCY) then
              begin
                if (LegsAnimation.ID = Stand.ID) or
                   (LegsAnimation.ID = Fall.ID) or
                   (LegsAnimation.ID = Crouch.ID) then
                begin
                  SpriteParts.Velocity[Num].X := SpriteParts.Velocity[Num].X * STANDSURFACECOEFX;
                  SpriteParts.Velocity[Num].Y := SpriteParts.Velocity[Num].Y * STANDSURFACECOEFY;
                  SpriteParts.Forces[Num].X := SpriteParts.Forces[Num].X - SpriteParts.Velocity[Num].X;
                end
                else if LegsAnimation.ID = Prone.ID then
                begin
                  if LegsAnimation.CurrFrame > 24 then
                  begin
                    if not (Control.Down and (Control.Left or Control.Right)) then
                    begin
                      SpriteParts.Velocity[Num].X := SpriteParts.Velocity[Num].X * STANDSURFACECOEFX;
                      SpriteParts.Velocity[Num].Y := SpriteParts.Velocity[Num].Y * STANDSURFACECOEFY;
                      SpriteParts.Forces[Num].X := SpriteParts.Forces[Num].X - SpriteParts.Velocity[Num].X;
                    end;
                  end
                  else
                  begin
                    SpriteParts.Velocity[Num].X := SpriteParts.Velocity[Num].X * SURFACECOEFX;
                    SpriteParts.Velocity[Num].Y := SpriteParts.Velocity[Num].Y * SURFACECOEFY;
                  end;
                end
                else if LegsAnimation.ID = GetUp.ID then
                begin
                  SpriteParts.Velocity[Num].X := SpriteParts.Velocity[Num].X * SURFACECOEFX;
                  SpriteParts.Velocity[Num].Y := SpriteParts.Velocity[Num].Y * SURFACECOEFY;
                end
                else if LegsAnimation.ID = ProneMove.ID then
                begin
                  SpriteParts.Velocity[Num].X := SpriteParts.Velocity[Num].X * STANDSURFACECOEFX;
                  SpriteParts.Velocity[Num].Y := SpriteParts.Velocity[Num].Y * STANDSURFACECOEFY;
                end;
              end;
            end
            else
            begin
              if (LegsAnimation.ID = CrouchRun.ID) or
                 (LegsAnimation.ID = CrouchRunBack.ID) then
              begin
                SpriteParts.Velocity[Num].X := SpriteParts.Velocity[Num].X * CROUCHMOVESURFACECOEFX;
                SpriteParts.Velocity[Num].Y := SpriteParts.Velocity[Num].Y * CROUCHMOVESURFACECOEFY;
              end
              else
              begin
                SpriteParts.Velocity[Num].X := SpriteParts.Velocity[Num].X * SURFACECOEFX;
                SpriteParts.Velocity[Num].Y := SpriteParts.Velocity[Num].Y * SURFACECOEFY;
              end;
            end;
          end;
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

function TSprite.CheckMapVerticesCollision(X, Y: Single; R: Single; HasCollided: Boolean): Boolean;
var
  i, j, w, PolyType: Integer;
  Pos, Dir, Vert: TVector2;
  D: Single;
  rx, ry: Integer;
  teamcol: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TSprite.CheckMapVerticesCollision');
  {$ENDIF}

  Result := False;
  Pos.X := X;
  Pos.Y := Y;

  // iterate through maps sector polygons
  rx := Round(Pos.X / Map.SectorsDivision);
  ry := Round(Pos.Y / Map.SectorsDivision);
  if (rx > -Map.SectorsNum) and (rx < Map.SectorsNum) and
    (ry > -Map.SectorsNum) and (ry < Map.SectorsNum) then
  begin
    for j := 1 to High(Map.Sectors[rx, ry].Polys) do
    begin
      w := Map.Sectors[rx, ry].Polys[j];
      PolyType := Map.PolyType[w];

      teamcol := TeamCollides(w, Player.Team, False);

      if ((PolyType <> POLY_TYPE_DOESNT) and (PolyType <> POLY_TYPE_ONLY_BULLETS) and teamcol and
          (PolyType <> POLY_TYPE_ONLY_FLAGGERS) and (PolyType <> POLY_TYPE_NOT_FLAGGERS)) or
         ((HoldedThing <> 0) and (PolyType = POLY_TYPE_ONLY_FLAGGERS)) or
         ((HoldedThing = 0) and (PolyType = POLY_TYPE_NOT_FLAGGERS)) then
      begin
        for i := 1 to 3 do
        begin
          Vert.X := Map.Polys[w].Vertices[i].X;
          Vert.Y := Map.Polys[w].Vertices[i].Y;
          D := Distance(Vert, Pos);
          if D < R then  // collision
          begin
            if BGState.BackgroundTest(w) then
              Continue;

            if not HasCollided then
              HandleSpecialPolyTypes(PolyType, Pos);

            Dir := Vec2Subtract(Pos, Vert);
            Vec2Normalize(Dir, Dir);
            SpriteParts.Pos[Num] := Vec2Add(SpriteParts.Pos[Num], Dir);

            Result := True;
            Exit;
          end;  // D < R
        end;  // i
      end;  // if (PolyType...)
    end;  // j
  end;
end;

function TSprite.CheckSkeletonMapCollision(i: Integer; X, Y: Single): Boolean;
var
  j, w: Integer;
  b: Integer = 0;
  Pos, Perp: TVector2;
  {$IFNDEF SERVER}
  a: TVector2;
  {$ENDIF}
  D: Single = 0.0;
  rx, ry: Integer;
  teamcol: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TSprite.CheckSkeletonMapCollision');
  {$ENDIF}

  Result := False;
  Pos.X := X - 1;
  Pos.Y := Y + 4;

  // iterate through map polygons
  rx := Round(Pos.X / Map.SectorsDivision);
  ry := Round(Pos.Y / Map.SectorsDivision);
  if (rx > -Map.SectorsNum) and (rx < Map.SectorsNum) and
    (ry > -Map.SectorsNum) and (ry < Map.SectorsNum) then
  begin
    BGState.BackgroundTestBigPolyCenter(Pos);

    for j := 1 to High(Map.Sectors[rx, ry].Polys) do
    begin
      w := Map.Sectors[rx, ry].Polys[j];

      teamcol := TeamCollides(w, Player.Team, False);

      if ((Map.PolyType[w] <> POLY_TYPE_DOESNT) and
          (Map.PolyType[w] <> POLY_TYPE_ONLY_BULLETS) and teamcol and
          (Map.PolyType[w] <> POLY_TYPE_ONLY_FLAGGERS) and
          (Map.PolyType[w] <> POLY_TYPE_NOT_FLAGGERS)) or
          ((HoldedThing <> 0) and (Map.PolyType[w] = POLY_TYPE_ONLY_FLAGGERS)) or
          ((HoldedThing = 0) and (Map.PolyType[w] = POLY_TYPE_NOT_FLAGGERS)) then
      begin
        if Map.PointInPolyEdges(Pos.X, Pos.y, w) then
        begin
          if BGState.BackgroundTest(w) then
            Continue;

          Perp := Map.ClosestPerpendicular(w, Pos, D, b);

          Vec2Normalize(Perp, Perp);
          Vec2Scale(Perp, Perp, D);

          Skeleton.Pos[i] := Skeleton.OldPos[i];
          Skeleton.Pos[i] := Vec2Subtract(Skeleton.Pos[i], Perp);

          {$IFNDEF SERVER}
          a := Vec2Subtract(Skeleton.Pos[i], Skeleton.OldPos[i]);

          if (Abs(a.y) > 0.8) and (DeadCollideCount < 13) then
            PlaySound(SFX_BODYFALL, Skeleton.Pos[i]);

          if (Abs(a.y) > 2.1) and (DeadCollideCount < 4) then
            PlaySound(SFX_BONECRACK, Skeleton.Pos[i]);
          {$ENDIF}

          Inc(DeadCollideCount);

          Result := True;
        end;
      end;
    end;
  end;

  if Result then
  begin
    Pos.X := X;
    Pos.Y := Y + 1;

    // iterate through map polygons
    rx := Round(Pos.X / Map.SectorsDivision);
    ry := Round(Pos.Y / Map.SectorsDivision);

    if (rx > -Map.SectorsNum) and (rx < Map.SectorsNum) and
      (ry > -Map.SectorsNum) and (ry < Map.SectorsNum) then
    begin
      BGState.BackgroundTestBigPolyCenter(Pos);

      for j := 1 to High(Map.Sectors[rx, ry].Polys) do
      begin
        w := Map.Sectors[rx, ry].Polys[j];

        if (Map.PolyType[w] <> POLY_TYPE_DOESNT) and (Map.PolyType[w] <> POLY_TYPE_ONLY_BULLETS) then
        begin
          if Map.PointInPolyEdges(Pos.X, Pos.y, w) then
          begin
            if BGState.BackgroundTest(w) then
              Continue;

            Perp := Map.ClosestPerpendicular(w, Pos, D, b);

            Vec2Normalize(Perp, Perp);
            Vec2Scale(Perp, Perp, D);

            Skeleton.Pos[i] := Skeleton.OldPos[i];
            Skeleton.Pos[i] := Vec2Subtract(Skeleton.Pos[i], Perp);

            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSprite.HandleSpecialPolyTypes(PolyType: Integer; Pos: TVector2);
var
  A, B: TVector2;
begin
  case PolyType of
    POLY_TYPE_DEADLY:
      begin
        {$IFDEF SERVER}
        HealthHit(50 + Health, Num, 12, -1, SpriteParts.Velocity[Num]);
        {$ENDIF}
      end;
    POLY_TYPE_BLOODY_DEADLY:
      begin
        {$IFDEF SERVER}
        HealthHit(450 + Health, Num, 12, -1, SpriteParts.Velocity[Num]);
        {$ENDIF}
      end;
    POLY_TYPE_HURTS, POLY_TYPE_LAVA:  // hurts
      begin
        if not DeadMeat then
        begin
          if Random(10) = 0 then
          begin
            {$IFDEF SERVER}
            Health := Health - 5;
            {$ELSE}
            if PolyType = POLY_TYPE_HURTS then
              PlaySound(SFX_ARG, SpriteParts.Pos[Num])
            else if PolyType = POLY_TYPE_LAVA then
              PlaySound(SFX_LAVA, SpriteParts.Pos[Num]);
            {$ENDIF}
          end;
          {$IFDEF SERVER}
          if Health < 1 then
            HealthHit(10, Num, 12, -1, SpriteParts.Velocity[Num]);
          {$ENDIF}
        end;

        // lava
        if Random(3) = 0 then
          if PolyType = POLY_TYPE_LAVA then
          begin
            A := Pos;
            A.Y := A.Y - 3.0;
            {$IFNDEF SERVER}
            CreateSpark(A, Vector2(0, -1.3), 36, Num, 40);
            {$ENDIF}

            if Random(3) = 0 then
            begin
              B.x := -Spriteparts.Velocity[Num].x;
              B.y := -Spriteparts.Velocity[Num].y;
              CreateBullet(A, B, Guns[FLAMER].Num, Num, 255,
                Guns[FLAMER].HitMultiply, False, True);
            end;
          end;
      end;
    POLY_TYPE_REGENERATES:
      begin
        if Health < STARTHEALTH then
          if MainTickCounter mod 12 = 0 then
          begin
            {$IFDEF SERVER}
            HealthHit(-2, Num, 12, -1, SpriteParts.Velocity[Num]);
            {$ELSE}
            PlaySound(SFX_REGENERATE, SpriteParts.Pos[Num]);
            {$ENDIF}
          end;
      end;
    POLY_TYPE_EXPLODES:
      begin
        if not DeadMeat then
        begin
          A := Pos;
          A.y := A.Y - 3.0;
          B.x := 0;
          B.y := 0;
          {$IFNDEF SERVER}
          CreateSpark(A, Vector2(0, -1.3), 36, Num, 40);
          {$ELSE}
          ServerCreateBullet(A, B, Guns[M79].Num, Num, 255, Guns[M79].HitMultiply, True);
          HealthHit(4000, Num, 12, -1, SpriteParts.Velocity[Num]);
          Health := -600;
          {$ENDIF}
        end;
      end;
    POLY_TYPE_HURTS_FLAGGERS:
      begin
        if not DeadMeat and
           (HoldedThing > 0) and (Thing[HoldedThing].Style < OBJECT_USSOCOM) then
          if Random(10) = 0 then
          begin
            {$IFDEF SERVER}
            Health := Health - 10;
            {$ELSE}
            PlaySound(SFX_ARG, SpriteParts.Pos[Num]);
            {$ENDIF}
          end;
        {$IFDEF SERVER}
        if Health < 1 then
          HealthHit(10, Num, 12, -1, SpriteParts.Velocity[Num]);
        {$ENDIF}
      end;
  end;
end;

function TBackgroundState.BackgroundTest(Poly: Word): Boolean;
var
  PolyType: Byte;
begin
  Result := False;

  PolyType := Map.PolyType[Poly];

  if (PolyType = POLY_TYPE_BACKGROUND) and (BackgroundStatus = BACKGROUND_TRANSITION) then
  begin
    BackgroundTestResult := True;
    BackgroundPoly := Poly;
    BackgroundStatus := BACKGROUND_TRANSITION;

    Result := True;
  end
  else if PolyType = POLY_TYPE_BACKGROUND_TRANSITION then
  begin
    BackgroundTestResult := True;
    if BackgroundStatus = BACKGROUND_NORMAL then
      BackgroundStatus := BACKGROUND_TRANSITION;

    Result := True;
  end;
end;

procedure TBackgroundState.BackgroundTestBigPolyCenter(Pos: TVector2);
begin
  if BackgroundStatus = BACKGROUND_TRANSITION then
  begin
    if BackgroundPoly = BACKGROUND_POLY_UNKNOWN then
    begin
      BackgroundPoly := BackgroundFindCurrentPoly(Pos);
      if BackgroundPoly <> BACKGROUND_POLY_NONE then
        BackgroundTestResult := True;
    end
    else if (BackgroundPoly <> BACKGROUND_POLY_NONE) and
      (Map.PointInPoly(Pos, Map.Polys[BackgroundPoly])) then
    begin
      BackgroundTestResult := True;
    end;
  end;
end;

function TBackgroundState.BackgroundFindCurrentPoly(Pos: TVector2): SmallInt;
var
  i: Integer;
begin
  for i := 1 to Map.BackPolyCount do
  begin
    if Map.PointInPoly(Pos, Map.BackPolys[i]^) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := BACKGROUND_POLY_NONE;
end;

procedure TBackgroundState.BackgroundTestPrepare();
begin
  BackgroundTestResult := False;
end;

procedure TBackgroundState.BackgroundTestReset();
begin
  if not BackgroundTestResult then
  begin
    BackgroundStatus := BACKGROUND_NORMAL;
    BackgroundPoly := BACKGROUND_POLY_NONE;
  end;
end;

procedure TSprite.ApplyWeaponByNum(WNum: Byte; Gun: Byte; Ammo: Integer = -1;
  RestorePrimaryState: Boolean = False);
var
  WeaponIndex: Integer;
begin
  {$IFDEF SERVER}
  Trace('TSprite.ApplyWeaponByNum');

  if Player.KnifeWarnings > 0 then
    Dec(Player.KnifeWarnings);
  {$ENDIF}

  if RestorePrimaryState and (Gun = 2) then
  begin
    SecondaryWeapon := Weapon;
  end else
  begin
    WeaponIndex := WeaponNumToIndex(WNum);
    if Gun = 1 then
      Weapon := Guns[WeaponIndex]
    else
      SecondaryWeapon := Guns[WeaponIndex];
  end;

  if Ammo > -1 then
    Weapon.AmmoCount := Ammo;

  {$IFNDEF SERVER}
  Weapon.StartUpTimeCount := Weapon.StartUpTime;
  {$ENDIF}

  {$IFDEF SERVER}
  if Weapon.Num = Guns[KNIFE].Num then
    KnifeCan[Num] := True;
  {$ENDIF}

  if WNum <> Guns[NOWEAPON].Num then
  begin
    LastWeaponHM := Weapon.HitMultiply;
    LastWeaponStyle := Weapon.BulletStyle;
    LastWeaponSpeed := Weapon.Speed;
    LastWeaponFire := Weapon.FireInterval;
    LastWeaponReload := Weapon.ReloadTime;
  end;

  {$IFDEF SERVER}
  Trace('TSprite.ApplyWeaponByNum 2');
  {$ENDIF}
end;

procedure TSprite.HealthHit(Amount: Single; Who, Where, What: Integer;
  Impact: TVector2);
var
  T: TVector2;
  HM: Single;
  j: Integer;
  {$IFNDEF SERVER}
  S: string;
  {$ENDIF}
begin
  {$IFDEF SERVER}
  Trace('TSprite.HealthHit');
  {$ENDIF}
  // Friendly Fire
  if (not sv_friendlyfire.Value) and IsNotSolo() and
     IsInSameTeam(Sprite[Who]) and (Num <> Who) then
    Exit;

  {$IFDEF SERVER}
  if Sprite[Who].IsSpectator() and (Sprite[Who].Player.ControlMethod = HUMAN) then
    Exit;
  {$ENDIF}

  if BonusStyle = BONUS_FLAMEGOD then
    Exit;

  // no health hit if someone is rambo
  if sv_gamemode.Value = GAMESTYLE_RAMBO then
    if Num <> Who then
    begin
      for j := 1 to MAX_PLAYERS do
        if (Sprite[j].Active) and (Who <> j) and (num <> j) then
          if (Sprite[j].Weapon.Num = Guns[BOW].Num) or
            (Sprite[j].Weapon.Num = Guns[BOW2].Num) then
            Exit;
    end;

  HM := Amount;

  if Vest > 0 then
  begin
    HM := Round(0.33 * Amount);
    Vest := Vest - HM;
    HM := Round(0.25 * Amount);
  end;

  if (Sprite[Who].BonusStyle = BONUS_BERSERKER)
    {$IFNDEF SERVER}
    and (Who <> Num)
    {$ENDIF}
    then
    HM := 4 * Amount;

  {$IFDEF SCRIPT}
  if not Self.DeadMeat then
    HM := ScrptDispatcher.OnPlayerDamage(Self.Num, Who, HM, Byte(What));
  {$ENDIF}

  Health := Health - HM;

  {$IFNDEF SERVER}
  if (What > 0) and (Self.Num <> MySprite) then
  begin
    case Bullet[What].Style of
      BULLET_STYLE_FRAGNADE:   S := 'Grenade';
      BULLET_STYLE_CLUSTER:  S := 'Clusters';
      BULLET_STYLE_M2:  S := 'Stationary gun';
      else S := WeaponNameByNum(Bullet[What].OwnerWeapon);
    end;
  end else
  begin
    S := 'Selfkill';
  end;

  if not self.DeadMeat and (What > 0) then
    // Check to prevent one AoE explosion counting as multiple hits on one bullet
    if (Who <> Self.Num) and (not Bullet[What].HasHit) and (Who = MySprite) then
      for j := 0 to 20 do
      begin
        if WepStats[j].Name = S then
        begin
          WepStats[j].Hits := WepStats[j].Hits + 1;
          Bullet[What].HasHit := True;
        end;
      end;
  {$ENDIF}

  // helmet fall off
  if (Health < HELMETFALLHEALTH) and (WearHelmet = 1) and (Where = 12) and
    (Weapon.Num <> Guns[BOW].Num) and (Weapon.Num <> Guns[BOW2].Num) and
    (Player.HeadCap > 0) then
  begin
    WearHelmet := 0;
    {$IFNDEF SERVER}
    CreateSpark(Skeleton.Pos[12], SpriteParts.Velocity[Num], 6, Num, 198);
    PlaySound(SFX_HEADCHOP, Sprite[Num].Skeleton.Pos[Where]);
    {$ENDIF}
  end;

  // safety precautions
  if Health < (BRUTALDEATHHEALTH - 1) then
    Health := BRUTALDEATHHEALTH;
  if Health > STARTHEALTH then
    Health := STARTHEALTH;

  // death!
  T := Impact;
  if (Health < 1) and (Health > HEADCHOPDEATHHEALTH) then
    Die(NORMAL_DEATH, Who, Where, What, T)
  else if (Health < HEADCHOPDEATHHEALTH + 1) and (Health > BRUTALDEATHHEALTH) then
    Die(HEADCHOP_DEATH, Who, Where, What, T)
  else if Health < BRUTALDEATHHEALTH + 1 then
    Die(BRUTAL_DEATH, Who, Where, What, T);

  Brain.TargetNum := Who;

  // Bot Chat
  {$IFDEF SERVER}
  if bots_chat.Value then
    if (Health < HURT_HEALTH) and
      (Player.ControlMethod = BOT) then
    begin
      if Random(10 * Brain.ChatFreq) = 0 then
        ServerSendStringMessage(WideString(Brain.ChatLowHealth), ALL_PLAYERS, Num, MSGTYPE_PUB);
    end;
  {$ENDIF}
end;

procedure TSprite.FreeControls;
begin
  {$IFDEF SERVER}
  Trace('TSprite.FreeControls');
  {$ENDIF}

  Control.Left := False;
  Control.Right := False;
  Control.Up := False;
  Control.Down := False;
  Control.Fire := False;
  Control.Jetpack := False;
  Control.ThrowNade := False;
  Control.ChangeWeapon := False;
  Control.ThrowWeapon := False;
  Control.Reload := False;
  Control.Prone := False;
  {$IFNDEF SERVER}Control.MouseDist := 150;{$ENDIF}
  Control.FlagThrow := False;
end;

procedure TSprite.CheckOutOfBounds;
var
  Bound: Integer;
  SpritePartsPos: ^TVector2;
begin
  {$IFDEF SERVER}
  Trace('TSprite.CheckOutOfBounds');
  {$ENDIF}

  if SurvivalEndRound then
    Exit;

  Bound := Map.SectorsNum * Map.SectorsDivision - 50;
  SpritePartsPos := @SpriteParts.Pos[Num];

  if (Abs(SpritePartsPos.X) > Bound) or
     (Abs(SpritePartsPos.Y) > Bound) then
  begin
    {$IFNDEF SERVER}
    RandomizeStart(SpriteParts.Pos[Num], Player.Team);
    {$ENDIF}
    Respawn;
  end;
end;

procedure TSprite.CheckSkeletonOutOfBounds;
var
  i: Integer;
  Bound: Integer;
  SkeletonPos: ^TVector2;
begin
  {$IFDEF SERVER}
  Trace('TSprite.CheckSkeletonOutOfBounds');
  {$ENDIF}

  if SurvivalEndRound then
    Exit;

  Bound := Map.SectorsNum * Map.SectorsDivision - 50;

  for i := 1 to 20 do
  begin
    SkeletonPos := @Skeleton.Pos[i];

    if (Abs(SkeletonPos.X) > Bound) or
       (Abs(SkeletonPos.Y) > Bound) then
    begin
      {$IFNDEF SERVER}
      RandomizeStart(SpriteParts.Pos[Num], Player.Team);
      {$ENDIF}
      Respawn;
      Break;
    end;
  end;
end;

procedure TSprite.Respawn;
var
  J: Integer;
  {$IFDEF SERVER}
  k: Integer;
  WeaponIndex: Integer;
  FavWeaponIndex: SmallInt;
  {$ENDIF}
  SecWep: Integer;
  DeadMeatBeforeRespawn: Boolean;
  SurvivalCheckEndRound: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TSprite.Respawn');
  {$ENDIF}
  if sv_survivalmode_clearweapons.Value then
    if SurvivalEndRound and not WeaponsCleaned then
    begin
      for J := 1 to MAX_THINGS do
      begin
        if Thing[J].Active and
           ((Thing[J].Style in [OBJECT_USSOCOM..OBJECT_MINIGUN]) or
            (Thing[J].Style in [OBJECT_COMBAT_KNIFE..OBJECT_LAW])) then
        begin
          Thing[J].Kill;
        end;
      end;
      WeaponsCleaned := True;
    end;

  if IsSpectator() then
    Exit;

  {$IFNDEF SERVER}
  if (Player.Name = '') or (Player.DemoPlayer) then
    Exit;

  if Num = MySprite then
    PlaySound(SFX_WERMUSIC, SpriteParts.Pos[Num]);
  {$ENDIF}

  {$IFDEF SERVER}
  RandomizeStart(SpriteParts.Pos[Num], Player.Team);
  {$ENDIF}

  {$IFDEF SCRIPT}
  SpriteParts.Pos[Num] := ScrptDispatcher.OnBeforePlayerRespawn(Num);
  {$ENDIF}

  DeadMeatBeforeRespawn := DeadMeat;
  DeadMeat := False;
  HalfDead := False;
  Health := STARTHEALTH;
  WearHelmet := 1;

  if Player.HeadCap = 0 then
    WearHelmet := 0;
  Skeleton.Constraints := GostekSkeleton.Constraints;
  SpriteParts.Velocity[Num].X := 0;
  SpriteParts.Velocity[Num].Y := 0;
  SpriteParts.Forces[Num].X := 0;
  SpriteParts.Forces[Num].Y := 0;
  JetsCount := Map.StartJet;
  JetsCountPrev := Map.StartJet;
  CeaseFireCounter := CeaseFireTime;
  if sv_advancemode.Value then
    CeaseFireCounter := CeaseFireCounter * 3;
  Brain.PissedOff := 0;
  Brain.GoThing := False;
  Vest := 0;
  BonusStyle := BONUS_NONE;
  BonusTime := 0;
  MultiKills := 0;
  MultiKillTime := 0;
  TertiaryWeapon := Guns[FRAGGRENADE];
  TertiaryWeapon.AmmoCount := sv_maxgrenades.Value div 2;
  HasCigar := 0;
  CanMercy := True;
  IdleTime := DEFAULT_IDLETIME;
  IdleRandom := -1;

  {$IFNDEF SERVER}
  if Num = MySprite then
  begin
    CameraFollowSprite := MySprite;
    GrenadeEffectTimer := 0;
    HitSprayCounter := 0;
  end;
  {$ENDIF}

  BodyAnimation := Stand;
  LegsAnimation := Stand;
  Position := POS_STAND;
  OnFire := 0;
  DeadCollideCount := 0;
  Brain.CurrentWaypoint := 0;
  RespawnCounter := 0;
  Player.Camera := Num;
  OnGround := False;
  OnGroundLastFrame := False;
  OnGroundPermanent := False;

  BGState.BackgroundStatus := BACKGROUND_TRANSITION;
  BGState.BackgroundPoly := BACKGROUND_POLY_UNKNOWN;

  {$IFDEF SERVER}
  BulletTime[Num] := MainTickCounter - 10;
  GrenadeTime[Num] := MainTickCounter - 10;
  KnifeCan[Num] := True;
  {$ENDIF}

  if (HoldedThing > 0) and (HoldedThing < MAX_THINGS + 1) then
    if Thing[HoldedThing].Style <> OBJECT_PARACHUTE then
      Thing[HoldedThing].Respawn
    else
      Thing[HoldedThing].Kill;

  HoldedThing := 0;

  {$IFNDEF SERVER}
  if SelWeapon > 0 then
    if WeaponSel[Num][SelWeapon] = 0 then
      SelWeapon := 0;
  {$ENDIF}

  Weapon := Guns[NOWEAPON];

  if SelWeapon > 0 then
    {$IFNDEF SERVER}
    if (WeaponActive[SelWeapon] = 1) and
       (WeaponSel[Num][SelWeapon] = 1) then
    begin
    {$ENDIF}
      ApplyWeaponByNum(SelWeapon, 1);
    {$IFNDEF SERVER}
    if Num = MySprite then
      ClientSpriteSnapshot;
    end;
    {$ENDIF}


  SecWep := Player.SecWep + 1;

  if (SecWep >= 1) and (SecWep <= SECONDARY_WEAPONS) and
     (WeaponActive[PRIMARY_WEAPONS + SecWep] = 1) and
     (WeaponSel[Num][PRIMARY_WEAPONS + SecWep] = 1) then
    SecondaryWeapon := Guns[PRIMARY_WEAPONS + SecWep]
  else
    SecondaryWeapon := Guns[NOWEAPON];

  {$IFDEF SERVER}if sv_advancemode.Value then{$ENDIF}
    if (SelWeapon > 0) and
      ((WeaponActive[SelWeapon] = 0) or
       (WeaponSel[Num][SelWeapon] = 0)) then
    begin
      Weapon := SecondaryWeapon;
      SecondaryWeapon := Guns[NOWEAPON];
    end;

  {$IFDEF SERVER}
  if Player.ControlMethod = BOT then
  begin
    Brain.CurrentWaypoint := 0;

    if (sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF) or
        (sv_gamemode.Value = GAMESTYLE_HTF) then
      Brain.PathNum := Player.Team;

    {$IFDEF SERVER}
    Trace('TSprite.Respawn 2');
    {$ENDIF}

    // randomize bot weapon
    if (Brain.FavWeapon <> Guns[NOWEAPON].Num) and
        (Brain.FavWeapon <> Guns[KNIFE].Num) and
        (Brain.FavWeapon <> Guns[CHAINSAW].Num) and
        (Brain.FavWeapon <> Guns[LAW].Num) and
        not Dummy then
    begin
      if ((WeaponActive[1] = 1) and (WeaponSel[Num][1] = 1)) or
          ((WeaponActive[2] = 1) and (WeaponSel[Num][2] = 1)) or
          ((WeaponActive[3] = 1) and (WeaponSel[Num][3] = 1)) or
          ((WeaponActive[4] = 1) and (WeaponSel[Num][4] = 1)) or
          ((WeaponActive[5] = 1) and (WeaponSel[Num][5] = 1)) or
          ((WeaponActive[6] = 1) and (WeaponSel[Num][6] = 1)) or
          ((WeaponActive[7] = 1) and (WeaponSel[Num][7] = 1)) or
          ((WeaponActive[8] = 1) and (WeaponSel[Num][8] = 1)) or
          ((WeaponActive[9] = 1) and (WeaponSel[Num][9] = 1)) or
          ((WeaponActive[10] = 1) and (WeaponSel[Num][10] = 1)) then
      begin
        repeat
          if Random(2) = 0 then
            ApplyWeaponByNum(Brain.FavWeapon, 1)
          else
          begin
            k := Random(9) + 1;
            Weapon := Guns[k];
          end;

          if (WeaponsInGame < 6) and (WeaponActive[MINIGUN] = 1) and (WeaponSel[Num][MINIGUN] = 1) then
            Weapon := Guns[MINIGUN];

          if sv_advancemode.Value then
          begin
            for j := 1 to PRIMARY_WEAPONS do
              if WeaponSel[Num][j] = 1 then
                Break;
            ApplyWeaponByNum(j, 1);
          end;
        until (WeaponActive[Weapon.Num] = 1) or (sv_advancemode.Value);
      end;
    end;

    if (WeaponActive[1] = 0) and (WeaponActive[2] = 0) and
        (WeaponActive[3] = 0) and (WeaponActive[4] = 0) and
        (WeaponActive[5] = 0) and (WeaponActive[6] = 0) and
        (WeaponActive[7] = 0) and (WeaponActive[8] = 0) and
        (WeaponActive[9] = 0) and (WeaponActive[10] = 0) and
        (WeaponSel[Num][1] = 0) and (WeaponSel[Num][2] = 0) and
        (WeaponSel[Num][3] = 0) and (WeaponSel[Num][4] = 0) and
        (WeaponSel[Num][5] = 0) and (WeaponSel[Num][6] = 0) and
        (WeaponSel[Num][7] = 0) and (WeaponSel[Num][8] = 0) and
        (WeaponSel[Num][9] = 0) and (WeaponSel[Num][10] = 0) then
      Weapon := Guns[NOWEAPON];

    FavWeaponIndex := WeaponNumToIndex(Brain.FavWeapon);
    if (Brain.FavWeapon = NOWEAPON_NUM) or
        IsSecondaryWeaponIndex(FavWeaponIndex) or Dummy then
    begin
      Weapon := Guns[FavWeaponIndex];
      SecondaryWeapon := Guns[NOWEAPON];
    end;

    if Brain.Use <> 255 then
    begin
      if Brain.Use = 1 then
      begin
        IdleTime := 0;
        IdleRandom := 1;
      end;
      if Brain.Use = 2 then
      begin
        IdleTime := 0;
        IdleRandom := 0;
      end;
    end;

    // Disarm bot if the primary weapon isn't allowed and selectable
    WeaponIndex := Weapon.Num;
    if (WeaponIndex >= 1) and (WeaponIndex <= PRIMARY_WEAPONS) then
      if (WeaponActive[WeaponIndex] = 0) or (WeaponSel[Num][WeaponIndex] = 0) then
        Weapon := Guns[NOWEAPON];
  end;
  {$ENDIF}

  if WeaponsInGame = 0 then
    Weapon := Guns[NOWEAPON];

  Parachute(SpriteParts.Pos[Num]);

  {$IFDEF SCRIPT}
  ScrptDispatcher.OnAfterPlayerRespawn(Num);
  {$ENDIF}

  {$IFDEF SERVER}
  ResetSpriteOldPos;
  {$ENDIF}

  // clear push wait list
  for j := 0 to MAX_PUSHTICK do
  begin
    NextPush[j].X := 0;
    NextPush[j].Y := 0;
  end;

  {$IFNDEF SERVER}
  // Spawn sound
  if Num <> MySprite then
    PlaySound(SFX_SPAWN, SpriteParts.Pos[Num]);

  // spawn spark
  CreateSpark(SpriteParts.Pos[Num], SpriteParts.Velocity[Num], 25, Num, 33);
  {$ENDIF}

  FreeControls;

  LegsApplyAnimation(Stand, 1);
  BodyApplyAnimation(Stand, 1);

  if CanRespawn(DeadMeatBeforeRespawn) then
  begin
    if SurvivalEndRound then
    begin
      SurvivalCheckEndRound := False;
      for J := 1 to MAX_SPRITES do
        if Sprite[J].Active then
          if Sprite[J].Player.Team <> TEAM_SPECTATOR then
            if Sprite[J].DeadMeat then
            begin
              SurvivalCheckEndRound := True;
              Break;
            end;
      SurvivalEndRound := SurvivalCheckEndRound;
    end;
  end else
  begin
    // CheckSkeletonOutOfBounds would trigger infinitely
    // Respawn if this is not done
    for J := 1 to 20 do
    begin
      Skeleton.Pos[J].X := SpriteParts.Pos[Num].X;
      Skeleton.Pos[J].Y := SpriteParts.Pos[Num].Y;
      Skeleton.OldPos[J] := Skeleton.Pos[J];
    end;
    // TODO: Fix this shouldn't change wepstats
    Die(NORMAL_DEATH, Num, 1, -1, Skeleton.Pos[12]);
    Dec(Player.Deaths);
  end;
end;

{$IFDEF SERVER}
procedure TSprite.ResetSpriteOldPos;
var
  i: Integer;
begin
  for i := MAX_OLDPOS downto 1 do
    OldSpritePos[Num, i] := SpriteParts.Pos[Num];
end;
{$ENDIF}

procedure TSprite.Parachute(a: TVector2);
var
  b: TVector2;
  n, i: Integer;
  D: Single = 0.0;
begin
  {$IFDEF SERVER}
  Trace('Parachute');
  {$ENDIF}
  if HoldedThing > 0 then
    Exit;
  if IsSpectator() then
    Exit;

  for i := 1 to MAX_THINGS do
  begin
    if Thing[i].HoldingSprite = Num then
    begin
      Thing[i].HoldingSprite := 0;
      Thing[i].Kill;
    end;
  end;

  b := a;
  b.y := b.y + PARA_DISTANCE;
  if not Map.RayCast(a, b, D, PARA_DISTANCE + 50, True, False, False, False, Player.Team) then
    if D > PARA_DISTANCE - 10 then
    begin
      a.y := a.y + 70;
      n := CreateThing(a, Num, OBJECT_PARACHUTE, 255);
      Thing[n].HoldingSprite := Num;
      {$IFNDEF SERVER}
      Thing[n].Color := Player.ShirtColor;
      {$ENDIF}
      HoldedThing := n;
    end;
end;

procedure TSprite.ChangeTeam(Team: Integer{$IFDEF SERVER};
  AdminChange: Boolean; JoinType: Byte = JOIN_NORMAL{$ENDIF});
var
  i: Integer;
  a, b: TVector2;
  {$IFDEF SERVER}
  TeamsCount: array[0..5] of Integer;
  {$ENDIF}
  {$IFDEF SCRIPT}
  SpriteOldTeam: Byte;
  {$ENDIF}
begin
  if Team > TEAM_SPECTATOR then
    Exit;

  if Active then
  begin
    {$IFDEF SERVER}
    for i := TEAM_NONE to TEAM_SPECTATOR do
      TeamsCount[i] := 0;

    for i := 1 to MAX_SPRITES do
      if Sprite[i].Active and Sprite[i].IsNotSpectator() then
        TeamsCount[Sprite[i].Player.Team] := TeamsCount[Sprite[i].Player.Team] + 1;

    // Check for uneven teams
    if (sv_balanceteams.Value) and (AdminChange = False) then
    begin
      if IsSpectator() and
        (Team = FindLowestTeam(TeamsCount)) then
      else
        if (TeamsCount[Team] >= TeamsCount[Player.Team]) and
           (Team < TEAM_SPECTATOR) then
        begin
          WriteConsole(Num, string(choose(Team - 1,
            ['Alpha', 'Bravo', 'Charlie', 'Delta'])) +
            ' team is full', RGB(0, 0, 255));
          Exit;
        end;
    end;

    if (TeamsCount[TEAM_SPECTATOR] >= sv_maxspectators.Value) and
       (Team = TEAM_SPECTATOR) and
       (AdminChange = False) then
    begin
      WriteConsole(Num, 'Spectators are full', RGB(0, 0, 255));
      Exit;
    end;

    if (sv_gamemode.Value <> GAMESTYLE_TEAMMATCH) and
       ((Team = TEAM_CHARLIE) or (Team = TEAM_DELTA)) and
       (AdminChange = False) then
      Exit;

    {$IFDEF SCRIPT}
    SpriteOldTeam := Player.Team;
    Team := ScrptDispatcher.OnBeforeJoinTeam(Num, Team, SpriteOldTeam);
    if (Team < TEAM_NONE) or (Team > TEAM_SPECTATOR) then
      Exit;
    {$ENDIF}
    {$ENDIF SERVER}

    DropWeapon();

    Player.Team := Team;
    Player.ApplyShirtColorFromTeam;
    a := Default(TVector2);
    b := Default(TVector2);
    Num := CreateSprite(a, b, 1, Num, Player, IsPlayerObjectOwner);

    if Sprite[Num].HoldedThing > 0 then
      if (Thing[Sprite[Num].HoldedThing].Style < OBJECT_USSOCOM) then
      begin
        Thing[Sprite[Num].HoldedThing].Respawn;
        Sprite[Num].HoldedThing := 0;
      end;

    for i := 1 to MAX_THINGS do
    begin
      if Thing[i].HoldingSprite = Num then
        Thing[i].Respawn;
    end;
    Sprite[Num].Respawn;

    {$IFDEF SERVER}
    BulletTime[Num] := MainTickCounter - 10;
    GrenadeTime[Num] := MainTickCounter - 10;
    KnifeCan[Num] := True;

    if not Sprite[Num].Player.DemoPlayer then
      ServerSendNewPlayerInfo(Num, JoinType);
    SortPlayers;
    Debug('Player switched teams');
    {$ENDIF}

    case Player.Team of
      {$IFDEF SERVER}
      TEAM_NONE: MainConsole.Console(Player.Name + ' has joined the game.',
        ENTER_MESSAGE_COLOR);
      TEAM_ALPHA: MainConsole.Console(Player.Name + ' has joined alpha team.',
        ALPHAJ_MESSAGE_COLOR);
      TEAM_BRAVO: MainConsole.Console(Player.Name + ' has joined bravo team.',
        BRAVOJ_MESSAGE_COLOR);
      TEAM_CHARLIE: MainConsole.Console(Player.Name + ' has joined charlie team.',
        CHARLIEJ_MESSAGE_COLOR);
      TEAM_DELTA: MainConsole.Console(Player.Name + ' has joined delta team.',
        DELTAJ_MESSAGE_COLOR);
      TEAM_SPECTATOR: MainConsole.Console(Player.Name + ' has joined spectators.',
        DELTAJ_MESSAGE_COLOR);
      {$ELSE}
      TEAM_NONE: MainConsole.Console(WideFormat(_('%s has joined the game.'), [Player.Name]), ENTER_MESSAGE_COLOR);
      TEAM_ALPHA: MainConsole.Console(WideFormat(_('%s has joined alpha team'), [Player.Name]), ALPHAJ_MESSAGE_COLOR);
      TEAM_BRAVO: MainConsole.Console(WideFormat(_('%s has joined bravo team'), [Player.Name]), BRAVOJ_MESSAGE_COLOR);
      TEAM_CHARLIE: MainConsole.Console(WideFormat(_('%s has joined charlie team'), [Player.Name]), CHARLIEJ_MESSAGE_COLOR);
      TEAM_DELTA: MainConsole.Console(WideFormat(_('%s has joined delta team'), [Player.Name]), DELTAJ_MESSAGE_COLOR);
      {$ENDIF}
    end;

    // prevent players from joining alive midround in survival mode
    if (sv_survivalmode.Value) and (Player.Team <> TEAM_SPECTATOR) then
    begin
      // TODO: Fix this shouldn't change wepstats
      Sprite[Num].HealthHit(4000, Num, 1, 1, a);
      Dec(Sprite[Num].Player.Deaths);
    end;
    {$IFNDEF SERVER}
    if Num = MySprite then
    begin
      if Player.Team = TEAM_SPECTATOR then
      begin
        CameraFollowSprite := 0;
        CameraFollowSprite := GetCameraTarget;
        GameMenuShow(LimboMenu, False);
      end else
        CameraFollowSprite := MySprite;
    end;
    {$ENDIF}

    // Check if map change is in progress
    {$IFDEF SERVER}
    if (MapChangeCounter > -60) and (MapChangeCounter < 99999999) then
      ServerMapChange(Num);
    {$ENDIF}

    {$IFDEF SCRIPT}
    ScrptDispatcher.OnJoinTeam(Sprite[Num].Num, Team, SpriteOldTeam, False);
    {$ENDIF}
  end;
end;

// SPRITE FIRE
procedure TSprite.Fire();
var
  a, b, d, m: TVector2;
  AimDirection: TVector2;
  i, bn: Integer;
  Inaccuracy: Single;
  MaxDeviation: Single;
  CollisionTestPerpendicular: TVector2;
  BNorm: TVector2;
  {$IFNDEF SERVER}
  MuzzleSmokeVector: TVector2;
  c: TVector2;
  rc: Single;
  col: Boolean;
  {$ENDIF}
begin
  bn := 0;
  Inaccuracy := 0;
  {$IFDEF SERVER}
  Trace('SpriteFire');
  {$ENDIF}

  // Create a normalized directional vector
  if (Weapon.BulletStyle = BULLET_STYLE_KNIFE) or
     (BodyAnimation.ID = Mercy.ID) or
     (BodyAnimation.ID = Mercy2.ID) then
    AimDirection := GetHandsAimDirection()
  else
    AimDirection := GetCursorAimDirection();

  b := AimDirection;

  a.x := Skeleton.Pos[15].X - (b.x * 4);
  a.y := Skeleton.Pos[15].Y - (b.y * 4) - 2;

  {$IFNDEF SERVER}
  // TODO(skoskav): Make bink and self-bink sprite-specific so bots can also use it
  if Num = MySprite then
  begin
    // Bink & self-bink
    if HitSprayCounter > 0 then
    begin
      Inaccuracy := Inaccuracy + HitSprayCounter * 0.01;
    end;
  end;
  {$ENDIF}

  // Moveacc
  Inaccuracy := Inaccuracy + GetMoveacc();

  // Bullet spread
  if (Weapon.Num <> Guns[EAGLE].Num) and
     (Weapon.Num <> Guns[SPAS12].Num) and
     (Weapon.BulletStyle <> BULLET_STYLE_SHOTGUN) then
  begin
    if Weapon.BulletSpread > 0 then
    begin
      if (LegsAnimation.ID = ProneMove.ID) or
         ((LegsAnimation.ID = Prone.ID) and (LegsAnimation.CurrFrame > 23)) then
      begin
        Inaccuracy := Inaccuracy + Weapon.BulletSpread / 1.625;
      end
      else if (LegsAnimation.ID = CrouchRun.ID) or
              (LegsAnimation.ID = CrouchRunBack.ID) or
              ((LegsAnimation.ID = Crouch.ID) and (LegsAnimation.CurrFrame > 13)) then
      begin
        Inaccuracy := Inaccuracy + Weapon.BulletSpread / 1.3;
      end
      else
      begin
        Inaccuracy := Inaccuracy + Weapon.BulletSpread;
      end;
    end;
  end;

  // FIXME(skoskav): Inaccuracy decreased due to altered way of acquiring the directional vector.
  // This should be solved more elegantly.
  Inaccuracy := Inaccuracy * 0.25;

  if Inaccuracy > MAX_INACCURACY then
    Inaccuracy := MAX_INACCURACY;

  // Calculate the maximum bullet deviation between 0 and MAX_INACCURACY.
  // The scaling is modeled after Sin(x) where x = 0 -> Pi/2 to gracefully reach
  // the maximum. Then multiply by a float between -1.0 and 1.0.
  MaxDeviation := MAX_INACCURACY * Sin((Inaccuracy / MAX_INACCURACY) * (PI / 2));
  d.x := (Random * 2 - 1) * MaxDeviation;
  d.y := (Random * 2 - 1) * MaxDeviation;

  // Add inaccuracies to directional vector and re-normalize
  b := Vec2Add(b, d);
  Vec2Normalize(b, b);

  // Multiply with the weapon speed
  Vec2Scale(b, b, Weapon.Speed);

  // Add some of the player's velocity to the bullet
  Vec2Scale(m, SpriteParts.Velocity[Num], Weapon.InheritedVelocity);
  b := Vec2Add(b, m);

  CollisionTestPerpendicular := Default(TVector2);
  // Check for immediate collision (could just be head in polygon), if so then
  // offset the bullet origin downward slightly
  if Map.CollisionTest(a, CollisionTestPerpendicular) then
  begin
    a.y := a.y + 2.5;
  end;

    if ((Weapon.Num <> Guns[EAGLE].Num) and
        (Weapon.Num <> Guns[SPAS12].Num) and
        (Weapon.Num <> Guns[FLAMER].Num) and
        (Weapon.Num <> Guns[NOWEAPON].Num) and
        (Weapon.Num <> Guns[KNIFE].Num) and
        (Weapon.Num <> Guns[CHAINSAW].Num) and
        (Weapon.Num <> Guns[LAW].Num)) or
       (BodyAnimation.ID = Mercy.ID) or
       (BodyAnimation.ID = Mercy2.ID) then
    begin
      bn := CreateBullet(a, b, Weapon.Num, Num, 255, Weapon.HitMultiply, True, False);
    end;


  {$IFDEF SERVER}
  Trace('SpriteFire 10');
  {$ENDIF}

  if Weapon.Num = Guns[EAGLE].Num then  // Eagles
  begin
    Inc(BulletCount);
    RandSeed := BulletCount;

    d.x := b.x + (Random * 2 - 1) * Weapon.BulletSpread;
    d.y := b.y + (Random * 2 - 1) * Weapon.BulletSpread;

    bn := CreateBullet(a, d, Weapon.Num, Num, 255, Weapon.HitMultiply, True, False, BulletCount);

    d.x := b.x + (Random * 2 - 1) * Weapon.BulletSpread;
    d.y := b.y + (Random * 2 - 1) * Weapon.BulletSpread;

    Vec2Normalize(BNorm, b);
    a.x := a.x - Sign(b.x) * Abs(BNorm.y) * 3.0;
    a.y := a.y + Sign(b.y) * Abs(BNorm.x) * 3.0;

    CreateBullet(a, d, Weapon.Num, Num, 255, Weapon.HitMultiply, False, False);
  end;

  if Weapon.BulletStyle = BULLET_STYLE_SHOTGUN then  // Shotgun
  begin
    Inc(BulletCount);
    RandSeed := BulletCount;

    d.x := b.x + (Random * 2 - 1) * Weapon.BulletSpread;
    d.y := b.y + (Random * 2 - 1) * Weapon.BulletSpread;

    bn := CreateBullet(a, d, Weapon.Num, Num, 255, Weapon.HitMultiply, True, False, BulletCount);

    for i := 0 to 4 do  // Remaining 5 pellets
    begin
      d.x := b.x + (Random * 2 - 1) * Weapon.BulletSpread;
      d.y := b.y + (Random * 2 - 1) * Weapon.BulletSpread;
      CreateBullet(a, d, Weapon.Num, Num, 255, Weapon.HitMultiply, False, False);
    end;

    d.x := b.x * 0.0412;
    d.y := b.y * 0.041;
    SpriteParts.Velocity[Num] := Vec2Subtract(SpriteParts.Velocity[Num], d);
  end;

  {$IFDEF SERVER}
  Trace('SpriteFire 11');
  {$ENDIF}

  if Weapon.Num = Guns[MINIGUN].Num then  // Minigun
  begin
    if Control.Jetpack and (JetsCount > 0) then
    begin
      d.x := b.x * 0.0012;
      d.y := b.y * 0.0009;
    end
    else
    begin
      d.x := b.x * 0.0082;
      d.y := b.y * 0.0078;
    end;

    if HoldedThing > 0 then
    begin
      d.x := d.x * 0.5;
      d.y := d.y * 0.7;
    end;
    d.x := d.x * 0.6;

    SpriteParts.Velocity[Num] := Vec2Subtract(SpriteParts.Velocity[Num], d);
  end;

  if Weapon.Num = Guns[FLAMER].Num then  // Flamer
  begin
    a.x := a.x + b.x * 2;
    a.y := a.y + b.y * 2;
    bn := CreateBullet(a, b, Weapon.Num, Num, 255, Weapon.HitMultiply, True, False);
    {$IFNDEF SERVER}
    PlaySound(SFX_FLAMER, SpriteParts.Pos[Num], GattlingSoundChannel);
    {$ENDIF}
  end;

  if Weapon.Num = Guns[CHAINSAW].Num then  // Chainsaw
  begin
    a.x := a.x + b.x * 2;
    a.y := a.y + b.y * 2;
    bn := CreateBullet(a, b, Weapon.Num, Num, 255, Weapon.HitMultiply, True, False);
  end;

  if Weapon.Num = Guns[LAW].Num then  // LAW
    if (OnGround or OnGroundPermanent or OnGroundForLaw) and
       (((LegsAnimation.ID = Crouch.ID) and
         (LegsAnimation.CurrFrame > 13)) or
        (LegsAnimation.ID = CrouchRun.ID) or
        (LegsAnimation.ID = CrouchRunBack.ID) or
        ((LegsAnimation.ID = Prone.ID) and
         (LegsAnimation.CurrFrame > 23))) then
    begin
      bn := CreateBullet(a, b, Weapon.Num, Num, 255, Weapon.HitMultiply, True, False);
    end
    else
    begin
      Exit;
    end;

  // Mercy animation
  if (BodyAnimation.ID = Mercy.ID) or
     (BodyAnimation.ID = Mercy2.ID) then
  begin
    if (bn > 0) and (bn < MAX_BULLETS + 1) then
      if Bullet[bn].Active then
      begin
        a := BulletParts.Velocity[bn];
        Vec2Normalize(BulletParts.Velocity[bn], BulletParts.Velocity[bn]);
        Vec2Scale(BulletParts.Velocity[bn], BulletParts.Velocity[bn], 70);
        Bullet[bn].Hit(2);
        Bullet[bn].Hit(9);
        // couple more - not sure why
        Bullet[bn].Hit(2);
        Bullet[bn].Hit(9);
        Bullet[bn].Hit(2);
        Bullet[bn].Hit(9);
        Bullet[bn].HitBody := Bullet[bn].Owner;
        BulletParts.Velocity[bn] := a;
      end;
  end;

  // Shouldn't we dec on server too?
  {$IFNDEF SERVER}
  if Weapon.AmmoCount > 0 then
    Dec(Weapon.AmmoCount);
  {$ENDIF}

  if Weapon.Num = Guns[SPAS12].Num then
    CanAutoReloadSpas := False;

  Weapon.FireIntervalPrev := Weapon.FireInterval;
  Weapon.FireIntervalCount := Weapon.FireInterval;

  Fired := Weapon.FireStyle;

  {$IFNDEF SERVER}
  // Spent bullet shell vectors
  c.x := SpriteParts.Velocity[Num].x + Direction * AimDirection.y * (Random * 0.5 + 0.8);
  c.y := SpriteParts.Velocity[Num].y - Direction * AimDirection.x * (Random * 0.5 + 0.8);
  a.x := Skeleton.Pos[15].x + 2 - Direction * 0.015 * b.x;
  a.y := Skeleton.Pos[15].y - 2 - Direction * 0.015 * b.y;

  Col := Map.CollisionTest(a, b);

  if r_maxsparks.Value < (MAX_SPARKS - 10) then
    if Random(2) = 0 then
      Col := True;
  {$ENDIF}

  {$IFDEF SERVER}
  Trace('SpriteFire 13');
  {$ENDIF}

  // play fire sound
  if Weapon.Num = Guns[AK74].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_AK74_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
    {$IFNDEF SERVER}
    if not Col then
      CreateSpark(a, c, 68, Num, 255);  // shell
    {$ENDIF}
  end;
  if Weapon.Num = Guns[M249].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_M249_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
    {$IFNDEF SERVER}
    if not Col then
      CreateSpark(a, c, 72, Num, 255);  // shell
    {$ENDIF}
  end;
  if Weapon.Num = Guns[RUGER77].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_RUGER77_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(Recoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
    {$IFNDEF SERVER}
    if not Col then
      CreateSpark(a, c, 70, Num, 255);  // shell
    {$ENDIF}
  end;
  if Weapon.Num = Guns[MP5].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_MP5_FIRE, SpriteParts.Pos[Num]);
    a.x := Skeleton.Pos[15].x + 2 - 0.2 * b.x;
    a.y := Skeleton.Pos[15].y - 2 - 0.2 * b.y;
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
    {$IFNDEF SERVER}
    if not Col then
      CreateSpark(a, c, 67, Num, 255);  // shell
    {$ENDIF}
  end;
  if Weapon.Num = Guns[SPAS12].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_SPAS12_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position <> POS_PRONE) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(Shotgun, 1);

    //make sure firing interrupts reloading when prone
    if (Position = POS_PRONE) and
      (BodyAnimation.ID = Reload.ID) then
      BodyAnimation.CurrFrame := BodyAnimation.NumFrames;
  end;
  if Weapon.Num = Guns[M79].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_M79_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position <> POS_PRONE) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
  end;
  if Weapon.Num = Guns[EAGLE].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_DESERTEAGLE_FIRE, SpriteParts.Pos[Num]);
    a.x := Skeleton.Pos[15].x + 3 - 0.17 * b.x;
    a.y := Skeleton.Pos[15].y - 2 - 0.15 * b.y;
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
    {$IFNDEF SERVER}
    if not Col then
      CreateSpark(a, c, 66, Num, 255);  // shell
    if not Col then
    begin
      a.x := Skeleton.Pos[15].x - 3 - 0.25 * b.x;
      a.y := Skeleton.Pos[15].y - 3 - 0.3 * b.y;
      c.x := SpriteParts.Velocity[Num].x + Direction * AimDirection.y * (Random * 0.5 + 0.8);
      c.y := SpriteParts.Velocity[Num].y - Direction * AimDirection.x * (Random * 0.5 + 0.8);
      CreateSpark(a, c, 66, Num, 255);  // shell
    end;
    {$ENDIF}
  end;
  if Weapon.Num = Guns[STEYRAUG].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_STEYRAUG_FIRE, SpriteParts.Pos[Num]);
    if not Col then
      CreateSpark(a, c, 69, Num, 255);  // shell
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
  end;
  if Weapon.Num = Guns[BARRETT].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_BARRETM82_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(Barret, 1);
    {$IFNDEF SERVER}
    if not Col then
      CreateSpark(a, c, 71, Num, 255);  // shell
    {$ENDIF}
  end;
  if Weapon.Num = Guns[MINIGUN].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_MINIGUN_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 2);
    {$IFNDEF SERVER}
    if not Col then
      CreateSpark(a, c, 73, Num, 255);  // shell
    {$ENDIF}
  end;
  if Weapon.Num = Guns[COLT].Num then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_COLT1911_FIRE, SpriteParts.Pos[Num]);
    a.x := Skeleton.Pos[15].x + 2 - 0.2 * b.x;
    a.y := Skeleton.Pos[15].y - 2 - 0.2 * b.y;
    if not Col then
      CreateSpark(a, c, 65, Num, 255);  // shell
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
  end;
  if (Weapon.Num = Guns[BOW].Num) or (Weapon.Num = Guns[BOW2].Num) then
  begin
    {$IFNDEF SERVER}
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_BOW_FIRE, SpriteParts.Pos[Num]);
    {$ENDIF}
    if (BodyAnimation.ID <> Throw.ID) and
      (Position = POS_STAND) and
      (BodyAnimation.ID <> GetUp.ID) and
      (BodyAnimation.ID <> Melee.ID) then
      BodyApplyAnimation(SmallRecoil, 1);
    if Position = POS_CROUCH then
    begin
      if BodyAnimation.ID = HandsUpAim.ID then
        BodyApplyAnimation(HandsUpRecoil, 1)
      else
        BodyApplyAnimation(AimRecoil, 1);
    end;
  end;
  {$IFNDEF SERVER}
  if Weapon.Num = Guns[LAW].Num then
  begin
    if BonusStyle <> BONUS_PREDATOR then
      PlaySound(SFX_LAW, SpriteParts.Pos[Num]);
  end;

  // smoke from muzzle
  MuzzleSmokeVector := b;
  Vec2Scale(MuzzleSmokeVector, MuzzleSmokeVector, 0.5);
  a := Vec2Add(a, MuzzleSmokeVector);
  Vec2Scale(MuzzleSmokeVector, MuzzleSmokeVector, 0.2);
  CreateSpark(a, MuzzleSmokeVector, 35, Num, 10);
  {$ENDIF}

  if BurstCount < 255 then
    Inc(BurstCount);

  // TODO(skoskav): Make bink and self-bink sprite-specific so bots can also use it
  {$IFNDEF SERVER}
  if Num = MySprite then
  begin
    // Increase self-bink for next shot
    if Weapon.Bink < 0 then
    begin
      if (LegsAnimation.ID = Crouch.ID) or
         (LegsAnimation.ID = CrouchRun.ID) or
         (LegsAnimation.ID = CrouchRunBack.ID) or
         (LegsAnimation.ID = Prone.ID) or
         (LegsAnimation.ID = ProneMove.ID) then
        HitSprayCounter := CalculateBink(HitSprayCounter, Round(-Weapon.Bink / 2))
      else
      begin
        HitSprayCounter := CalculateBink(HitSprayCounter, -Weapon.Bink);
      end;
    end;
  end;

  // Screen shake
if ((MySprite > 0) and (CameraFollowSprite <> 0)) and (not ((Num <> MySprite) and not cl_screenshake.Value)) then
  begin
    if Weapon.Num <> Guns[CHAINSAW].Num then
    begin
      if PointVisible(Spriteparts.Pos[Num].X, Spriteparts.Pos[Num].Y, CameraFollowSprite) then
      begin
        if (Weapon.Num = Guns[M249].Num) or
           (Weapon.Num = Guns[SPAS12].Num) or
           (Weapon.Num = Guns[BARRETT].Num) or
           (Weapon.Num = Guns[MINIGUN].Num) then
        begin
          CameraX := CameraX - 3 + Random(7);
          CameraY := CameraY - 3 + Random(7);
        end
        else
        begin
          CameraX := CameraX - 1 + Random(3);
          CameraY := CameraY - 1 + Random(3);
        end;
      end;
    end;
  end;

  // Recoil!
  if MySprite = Num then
  begin
    rc := BurstCount / 10;
    rc := rc * Weapon.Recoil;

    // less recoil on crouch
    if (OnGround) then
    begin
      if (LegsAnimation.ID = Crouch.ID) and
        (LegsAnimation.CurrFrame > 13) then
        rc := rc / 2;

      if (LegsAnimation.ID = Prone.ID) and
        (LegsAnimation.CurrFrame > 23) then
        rc := rc / 3;
    end;

    if (rc > 0) then
    begin
      rc := sin(degtorad((Weapon.Speed * Weapon.FireInterval / 364) * rc));

      CalculateRecoil(GameWidthHalf - camerax + SpriteParts.Pos[Num].X,
        GameHeightHalf - cameray + SpriteParts.Pos[Num].Y, mx, my,
        -(pi / 1) * rc);
    end;
  end;
  {$ENDIF}
end;

procedure TSprite.ThrowFlag();
var
  i, j: Integer;
  b: TVector2;
  d: Single = 0.0;
  CursorDirection: TVector2;
  BOffset, BPerp: TVector2;
  LookPoint1, LookPoint2, LookPoint3: TVector2;
  NewPosDiff: TVector2;
  FuturePoint1, FuturePoint2, FuturePoint3, FuturePoint4: TVector2;
begin
  BPerp := Default(TVector2);
  if (BodyAnimation.ID <> Roll.ID) and
     (BodyAnimation.ID <> RollBack.ID) then
  begin
    if Control.FlagThrow then
    begin
      if HoldedThing > 0 then
      begin
        for i := 1 to MAX_THINGS do
        begin
          if Thing[i].HoldingSprite = Num then
          begin
            if Thing[i].Style < 4 then
            begin
              // Create start velocity vector
              CursorDirection := GetCursorAimDirection();
              Vec2Scale(CursorDirection, CursorDirection, FLAGTHROW_POWER);

              // FIXME: Offset it away from the player so it isn't instantly re-grabbed,
              // it makes it look like lag though
              Vec2Scale(BOffset, CursorDirection, 5);

              // Add velocity
              b := Vec2Add(CursorDirection, SpriteParts.Velocity[Num]);

              // Don't throw if the flag would collide in the upcoming frame
              NewPosDiff := Vec2Add(BOffset, b);
              LookPoint1 := Vec2Add(Thing[i].Skeleton.Pos[1], NewPosDiff);

              FuturePoint1 := Vec2Add(LookPoint1, Vector2(-10, -8));
              FuturePoint2 := Vec2Add(LookPoint1, Vector2(10, -8));
              FuturePoint3 := Vec2Add(LookPoint1, Vector2(-10, 8));
              FuturePoint4 := Vec2Add(LookPoint1, Vector2(10, 8));

              LookPoint1 := Vec2Add(Thing[i].Skeleton.Pos[2], NewPosDiff);
              LookPoint2 := Vec2Add(Thing[i].Skeleton.Pos[3], NewPosDiff);
              LookPoint3 := Vec2Add(Thing[i].Skeleton.Pos[4], NewPosDiff);

              if not Map.RayCast(Skeleton.Pos[15], LookPoint1, d, 200, False, True, False) and
                 not Map.RayCast(Skeleton.Pos[15], LookPoint2, d, 200, False, True, False) and
                 not Map.RayCast(Skeleton.Pos[15], LookPoint3, d, 200, False, True, False) and
                 not Map.CollisionTest(FuturePoint1, BPerp, True) and
                 not Map.CollisionTest(FuturePoint2, BPerp, True) and
                 not Map.CollisionTest(FuturePoint3, BPerp, True) and
                 not Map.CollisionTest(FuturePoint4, BPerp, True) then
              begin
                for j := 1 to 4 do
                begin
                  // Apply offset from flagger
                  Thing[i].Skeleton.Pos[j] := Vec2Add(Thing[i].Skeleton.Pos[j], BOffset);

                  // Apply velocities
                  Thing[i].Skeleton.Pos[j] := Vec2Add(Thing[i].Skeleton.Pos[j], b);
                  Thing[i].Skeleton.OldPos[j] := Vec2Subtract(Thing[i].Skeleton.Pos[j], b);
                end;

                // Add some spin for visual effect
                BPerp := Vector2(-b.y, b.x);
                Vec2Normalize(BPerp, BPerp);
                Vec2Scale(BPerp, BPerp, Direction);
                Thing[i].Skeleton.Pos[1] := Vec2Subtract(Thing[i].Skeleton.Pos[1], BPerp);
                Thing[i].Skeleton.Pos[2] := Vec2Add(Thing[i].Skeleton.Pos[2], BPerp);

                // Release the flag
                Thing[i].HoldingSprite := 0;
                HoldedThing := 0;
                FlagGrabCooldown := SECOND div 4;

                {$IFDEF SCRIPT}
                ScrptDispatcher.OnFlagDrop(Num, Thing[i].Style, True);
                {$ENDIF}

                Thing[i].BGState.BackgroundStatus := BACKGROUND_TRANSITION;
                Thing[i].BGState.BackgroundPoly := BACKGROUND_POLY_UNKNOWN;

                Thing[i].StaticType := False;
                {$IFDEF SERVER}
                ServerThingMustSnapshot(i);
                {$ENDIF}
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSprite.ThrowGrenade();
var
  a, B, c, e: TVector2;
  f: Single = 0.0;
  GrenadeArcSize, GrenadeArcX, GrenadeArcY: Single;
  PlayerVelocity: TVector2;
begin
  c := Default(TVector2);
  // Start throw animation
  if not Control.ThrowNade then
    GrenadeCanThrow := True;

  if GrenadeCanThrow and
     Control.ThrowNade and
     (BodyAnimation.ID <> Roll.ID) and
     (BodyAnimation.ID <> RollBack.ID) then
  begin
    BodyApplyAnimation(Throw, 1);
    {$IFNDEF SERVER}
    SetSoundPaused(ReloadSoundChannel, True);
    {$ENDIF}
  end;

  {$IFNDEF SERVER}
  // Pull pin
  if (BodyAnimation.ID = Throw.ID) and
     (BodyAnimation.CurrFrame = 15) and
     (TertiaryWeapon.AmmoCount > 0) and (CeaseFireCounter < 0) then
  begin
    B := GetHandsAimDirection();
    Vec2Scale(B, B, BodyAnimation.CurrFrame / Guns[FRAGGRENADE].Speed);
    if BodyAnimation.CurrFrame < 24 then
      Vec2Scale(B, B, 0.65);
    B := Vec2Add(B, SpriteParts.Velocity[Num]);
    a.x := Skeleton.Pos[15].X + b.x * 3;
    a.y := Skeleton.Pos[15].Y - 2 + b.y * 3;
    if not Map.CollisionTest(a, c) then
    begin
      B := GetHandsAimDirection();
      B.X := B.X * 0.5;
      B.Y := B.Y + 0.4;
      CreateSpark(a, b, 30, Num, 255);  // Pin
      PlaySound(SFX_GRENADE_PULLOUT, a);
    end;
  end;
  {$ENDIF}

  if (BodyAnimation.ID = Throw.ID) and
     (not Control.ThrowNade or (BodyAnimation.CurrFrame = 36)) then
  begin
    // Grenade throw
    if (BodyAnimation.CurrFrame > 14) and
       (BodyAnimation.CurrFrame < 37) and
       (TertiaryWeapon.AmmoCount > 0) and
       (CeaseFireCounter < 0) then
    begin
      B := GetCursorAimDirection();

      // Add a few degrees of arc to the throw. The arc approaches zero as you aim up or down
      GrenadeArcSize := Sign(b.x) / 8 * (1 - Abs(b.y));
      GrenadeArcX := Sin(b.y * Pi / 2) * GrenadeArcSize;
      GrenadeArcY := Sin(b.x * Pi / 2) * GrenadeArcSize;
      B.x := B.x + GrenadeArcX;
      B.y := B.y - GrenadeArcY;
      Vec2Normalize(B, B);

      Vec2Scale(B, B, BodyAnimation.CurrFrame / Guns[FRAGGRENADE].Speed);
      if BodyAnimation.CurrFrame < 24 then
        Vec2Scale(B, B, 0.65);

      Vec2Scale(PlayerVelocity, SpriteParts.Velocity[Num],
        Guns[FRAGGRENADE].InheritedVelocity);

      B := Vec2Add(B, PlayerVelocity);
      a.x := Skeleton.Pos[15].X + b.x * 3;
      a.y := Skeleton.Pos[15].Y - 2 + (b.y * 3);
      e.x := SpriteParts.Pos[Num].X;
      e.y := SpriteParts.Pos[Num].Y - 12;
      if not Map.CollisionTest(a, c) and
         not Map.RayCast(e, a, f, 50, False, False, True, False, Player.Team) then
      begin
        CreateBullet(a, b, TertiaryWeapon.Num, Num, 255,
          Guns[FRAGGRENADE].HitMultiply, True, False);
        if {$IFNDEF SERVER}((Player.ControlMethod = HUMAN) and (Num = MySprite)) or{$ENDIF}
           (Player.ControlMethod = BOT) then
          Dec(TertiaryWeapon.AmmoCount);

        {$IFNDEF SERVER}
        if (Num = MySprite) and (Guns[FRAGGRENADE].Bink < 0) then
          HitSprayCounter := CalculateBink(HitSprayCounter, -Guns[FRAGGRENADE].Bink);

        PlaySound(SFX_GRENADE_THROW, a);
        {$ENDIF}
      end;
    end;

    if Control.ThrowNade then
      GrenadeCanThrow := False;

    if Weapon.AmmoCount = 0 then
    begin
      if Weapon.ReloadTimeCount > Weapon.ClipOutTime then
        BodyApplyAnimation(ClipOut, 1);
      if Weapon.ReloadTimeCount < Weapon.ClipOutTime then
        BodyApplyAnimation(ClipIn, 1);
      if (Weapon.ReloadTimeCount < Weapon.ClipInTime) and
         (Weapon.ReloadTimeCount > 0) then
        BodyApplyAnimation(SlideBack, 1);
      {$IFNDEF SERVER}
      SetSoundPaused(ReloadSoundChannel, False);
      {$ENDIF}
    end;
  end;
end;

function TSprite.GetMoveacc(): Single;
var
  Moveacc: Single;
begin
  Result := 0;

  // No moveacc for bots on harder difficulties
  if (Player.ControlMethod = BOT) {$IFDEF SERVER}and (bots_difficulty.Value < 50){$ENDIF} then
    Moveacc := 0
  else
    Moveacc := Weapon.MovementAcc;

  if Moveacc > 0 then
  begin
    if (Control.Jetpack and (JetsCount > 0)) or
       (LegsAnimation.ID = Jump.ID) or
       (LegsAnimation.ID = JumpSide.ID) or
       (LegsAnimation.ID = Run.ID) or
       (LegsAnimation.ID = RunBack.ID) or
       (LegsAnimation.ID = Roll.ID) or
       (LegsAnimation.ID = RollBack.ID) then
    begin
      Result := Moveacc * 7;
    end
    else if (not OnGroundPermanent and
             (LegsAnimation.ID <> Prone.ID) and
             (LegsAnimation.ID <> ProneMove.ID) and
             (LegsAnimation.ID <> Crouch.ID) and
             (LegsAnimation.ID <> CrouchRun.ID) and
             (LegsAnimation.ID <> CrouchRunBack.ID)) or
             (LegsAnimation.ID = GetUp.ID) or
             ((LegsAnimation.ID = Prone.ID) and
             (LegsAnimation.CurrFrame < LegsAnimation.NumFrames)) then
    begin
      Result := Moveacc * 3;
    end;
  end;
end;

function TSprite.GetCursorAimDirection(): TVector2;
var
  MouseAim: TVector2;
  AimDirection: TVector2;
begin
  MouseAim.x := Control.MouseAimX;
  MouseAim.y := Control.MouseAimY;

  AimDirection := Vec2Subtract(MouseAim, Skeleton.Pos[15]);
  Vec2Normalize(AimDirection, AimDirection);

  Result := AimDirection;
end;

function TSprite.GetHandsAimDirection(): TVector2;
var
  AimDirection: TVector2;
begin
  AimDirection := Vec2Subtract(Skeleton.Pos[15], Skeleton.Pos[16]);
  Vec2Normalize(AimDirection, AimDirection);

  Result := AimDirection;
end;

function TSprite.IsSolo(): Boolean;
begin
  Result := Player.Team = TEAM_NONE;
end;

function TSprite.IsNotSolo(): Boolean;
begin
  Result := Player.Team <> TEAM_NONE;
end;

function TSprite.IsInTeam(): Boolean;
begin
  case Player.Team of
    TEAM_ALPHA, TEAM_BRAVO, TEAM_CHARLIE, TEAM_DELTA:
      Result := True;
    else
      Result := False;
  end;
end;

function TSprite.IsSpectator(): Boolean;
begin
  Result := Player.Team = TEAM_SPECTATOR;
end;

function TSprite.IsNotSpectator(): Boolean;
begin
  Result := Player.Team <> TEAM_SPECTATOR;
end;

function TSprite.IsInSameTeam(const OtherPlayer: TSprite): Boolean;
begin
  Result := Player.Team = OtherPlayer.Player.Team;
end;

function TSprite.IsNotInSameTeam(const OtherPlayer: TSprite): Boolean;
begin
  Result := Player.Team <> OtherPlayer.Player.Team;
end;

function TSprite.CanRespawn(DeadMeatBeforeRespawn: Boolean): Boolean;
begin
  Result := (sv_survivalmode.Value = False) or
            (SurvivalEndRound) or
            (not DeadMeatBeforeRespawn);
end;

end.

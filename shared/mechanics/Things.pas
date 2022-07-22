unit Things;

interface

uses
  Parts, Anims, MapFile, PolyMap, Net, Weapons, Constants, Vector, Sprites;

  type TThingCollision = object
      ThingNum: Byte;
      CooldownEnd: LongInt;
  end;

  type TThing = object
      Active: Boolean;
      Style, Num, Owner: Byte;
      HoldingSprite: Byte;
      AmmoCount: Byte;
      Radius: Single;
      TimeOut: Integer;
      StaticType: Boolean;
      Interest: Integer;
      CollideWithBullets: Boolean;
      InBase: Boolean;
      LastSpawn, Team: Byte;
      Skeleton: ParticleSystem;
      CollideCount: array[1..4] of Byte;
      Polys: array[1..2] of TMapPolygon;
      BGState: TBackgroundState;
      {$IFNDEF SERVER}
      Tex1, Tex2: Integer;
      Texture: LongInt;
      Color: LongWord;
      {$ENDIF}
    public
      procedure Update;
      {$IFNDEF SERVER}
      procedure Render(TimeElapsed: Extended);
      procedure PolygonsRender;
      {$ENDIF}
      function CheckMapCollision(i: Integer; X, Y: Single): Boolean;
      procedure Kill;
      procedure CheckOutOfBounds;
      procedure Respawn;
      procedure MoveSkeleton(x1, y1: Single; FromZero: Boolean);
      {$IFDEF SERVER}
      function CheckSpriteCollision: Integer;
      {$ENDIF}
      function CheckStationaryGunCollision
        {$IFNDEF SERVER}(ClientCheck: Boolean){$ENDIF}: Integer;
    end;

  function CreateThing(sPos: TVector2; owner, sStyle, N: Byte): Integer;
  function ThingCollision(ThingNum: Byte; CooldownEnd: LongInt): TThingCollision;
  function SpawnBoxes(var Start: TVector2; Team: Byte; Num: Byte): Boolean;
  function RandomizeStart(var Start: TVector2; Team: Byte): Boolean;

  implementation

  uses
    {$IFNDEF SERVER}
    Gfx, Sound, GameRendering, Sparks,
    {$ELSE}
      {$IFDEF SCRIPT}
      ScriptDispatcher,
      {$ENDIF}
    {$ENDIF}
    {$IFDEF SERVER}
    NetworkServerThing, NetworkServerMessages, NetworkServerGame,
    {$ENDIF}
    {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} SysUtils, Calc, TraceLog, Game, Bullets;

function CreateThing(sPos: TVector2; owner, sStyle, N: Byte):
  Integer;
var
  i, k, s: Integer;
  a: TVector2;
  {$IFDEF SERVER}
  b: TVector2;
  WeaponThrowSpeedPos1, WeaponThrowSpeedPos2: Single;
  WeaponThrowVelocity: TVector2;
  {$ENDIF}
begin
  Trace('CreateThing');

  i := 0;
  // Remove flag if a new one is created
  if sStyle < OBJECT_USSOCOM then
    for k := 1 to MAX_THINGS do
      if (Thing[k].Active) and (Thing[k].Style = sStyle) then
        Thing[k].Kill;

  if N = 255 then
  begin
    s := 1;
    // FIXME (helloer): Check if this should be synced
    {$IFNDEF SERVER}
    if sStyle = OBJECT_PARACHUTE then
      s := MAX_THINGS div 2;
    {$ENDIF}

    for i := s to MAX_THINGS + 1 do
    begin
      if i = MAX_THINGS + 1 then
      begin
        Result := -1;
        Exit;
      end;
      if not Thing[i].Active then
        Break;
    end;
  end else
    i := N; // i is now the active sprite

  Assert((i <> 0), 'thing id must not be 0');

  // activate sprite
  Thing[i].Active := True;
  Thing[i].Style := sStyle;
  Thing[i].Num := i;
  Thing[i].HoldingSprite := 0;
  Thing[i].Owner := Owner;
  Thing[i].Timeout := 0;
  Thing[i].Skeleton.Destroy;
  Thing[i].Skeleton.TimeStep := 1;
  Thing[i].StaticType := False;
  Thing[i].InBase := False;
  {$IFNDEF SERVER}
  Thing[i].Tex1 := 0;
  Thing[i].Tex2 := 0;
  {$ENDIF}

  Thing[i].BGState.BackgroundStatus := BACKGROUND_TRANSITION;
  Thing[i].BGState.BackgroundPoly := BACKGROUND_POLY_UNKNOWN;

  for k := 1 to 4 do
    Thing[i].CollideCount[k] := 0;

  if Owner <> 255 then
  begin
    if Sprite[Owner].Direction = 1 then
      k := 0
    else
      k := 1;
  end else
    k := 0;

  case sStyle of  // specific style creation
    OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG:  // Flag
      begin
        Thing[i].Skeleton.VDamping := 0.991;
        Thing[i].Skeleton.Gravity := 1.0 * GRAV;
        Thing[i].Skeleton.Clone(FlagSkeleton);
        // A and B flags face eachother.
        if sStyle = OBJECT_ALPHA_FLAG then
        begin
          Thing[i].Skeleton.Pos[3].X := 12;
          Thing[i].Skeleton.Pos[4].X := 12;
          Thing[i].Skeleton.OldPos[3].X := 12;
          Thing[i].Skeleton.OldPos[4].X := 12;
        end;
        Thing[i].Radius := 19;
        if sStyle <> OBJECT_POINTMATCH_FLAG then
          Thing[i].InBase := True;

        {$IFNDEF SERVER}
        if sv_gamemode.Value = GAMESTYLE_INF then
          Thing[i].Texture := GFX_OBJECTS_INFFLAG
        else
          Thing[i].Texture := GFX_OBJECTS_FLAG;
        {$ENDIF}

        Thing[i].Timeout := FLAG_TIMEOUT;
        Thing[i].Interest := FLAG_INTEREST_TIME;

        Thing[i].CollideWithBullets := True;

        if (sv_gamemode.Value = GAMESTYLE_INF) and (sStyle = OBJECT_ALPHA_FLAG) then
          Thing[i].CollideWithBullets := False;
      end;  // Flag

    OBJECT_USSOCOM:  // Socom
      begin
        Thing[i].Skeleton.VDamping := 0.994;
        Thing[i].Skeleton.Gravity := 1.05 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton10);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := GFX_WEAPONS_N_SOCOM + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_DESERT_EAGLE:  // Deagle
      begin
        Thing[i].Skeleton.VDamping := 0.996;
        Thing[i].Skeleton.Gravity := 1.09 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton11);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := GFX_WEAPONS_N_DEAGLES + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_HK_MP5:  // Mp5
      begin
        Thing[i].Skeleton.VDamping := 0.995;
        Thing[i].Skeleton.Gravity := 1.11 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton22);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[MP5].TextureNum + k;
        Thing[i].Tex2 := Guns[MP5].ClipTextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_AK74:  // Ak74
      begin
        Thing[i].Skeleton.VDamping := 0.994;
        Thing[i].Skeleton.Gravity := 1.16 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton37);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[AK74].TextureNum + k;
        Thing[i].Tex2 := Guns[AK74].ClipTextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_STEYR_AUG:  // SteyrAug
      begin
        Thing[i].Skeleton.VDamping := 0.994;
        Thing[i].Skeleton.Gravity := 1.16 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton37);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[STEYRAUG].TextureNum + k;
        Thing[i].Tex2 := Guns[STEYRAUG].ClipTextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_SPAS12:  // Spas
      begin
        Thing[i].Skeleton.VDamping := 0.993;
        Thing[i].Skeleton.Gravity := 1.15 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton36);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[SPAS12].TextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_RUGER77:  // Ruger
      begin
        Thing[i].Skeleton.VDamping := 0.993;
        Thing[i].Skeleton.Gravity := 1.13 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton36);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[RUGER77].TextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_M79:  // M79
      begin
        Thing[i].Skeleton.VDamping := 0.994;
        Thing[i].Skeleton.Gravity := 1.15 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton28);
        //FIXME (helloer): Check why Tex1 is different
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[M79].TextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_BARRET_M82A1:  // Barrett
      begin
        Thing[i].Skeleton.VDamping := 0.993;
        Thing[i].Skeleton.Gravity := 1.18 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton43);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[BARRETT].TextureNum + k;
        Thing[i].Tex2 := Guns[BARRETT].ClipTextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_MINIMI:  // M249
      begin
        Thing[i].Skeleton.VDamping := 0.993;
        Thing[i].Skeleton.Gravity := 1.2 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton39);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[M249].TextureNum + k;
        Thing[i].Tex2 := Guns[M249].ClipTextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_MINIGUN:  // Minigun
      begin
        Thing[i].Skeleton.VDamping := 0.991;
        Thing[i].Skeleton.Gravity := 1.4 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton55);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[MINIGUN].TextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_RAMBO_BOW:  // Bow
      begin
        Thing[i].Skeleton.VDamping := 0.996;
        Thing[i].Skeleton.Gravity := 0.65 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton50);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := GFX_WEAPONS_N_BOW + k;
        {$ENDIF}
        Thing[i].Radius := BOW_RADIUS;
        Thing[i].TimeOut := FLAG_TIMEOUT;
        Thing[i].Interest := BOW_INTEREST_TIME;
        Thing[i].CollideWithBullets := True;
      end;
    OBJECT_MEDICAL_KIT:  // medikit
      begin
        Thing[i].Skeleton := BoxSkeleton;
        Thing[i].Skeleton.VDamping := 0.989;
        Thing[i].Skeleton.Gravity := 1.05 * GRAV;
        Thing[i].Radius := KIT_RADIUS;
        Thing[i].TimeOut := sv_respawntime.Value * GUNRESISTTIME;
        Thing[i].Interest := DEFAULT_INTEREST_TIME;
        Thing[i].CollideWithBullets := sv_kits_collide.Value;
        {$IFNDEF SERVER}
        Thing[i].Texture := GFX_OBJECTS_MEDIKIT;
        {$ENDIF}
      end;
    OBJECT_GRENADE_KIT:  // grenadekit
      begin
        Thing[i].Skeleton := BoxSkeleton;
        Thing[i].Skeleton.VDamping := 0.989;
        Thing[i].Skeleton.Gravity := 1.07 * GRAV;
        Thing[i].Radius := KIT_RADIUS;
        Thing[i].TimeOut := FLAG_TIMEOUT;
        Thing[i].Interest := DEFAULT_INTEREST_TIME;
        Thing[i].CollideWithBullets := sv_kits_collide.Value;
        {$IFNDEF SERVER}
        Thing[i].Texture := GFX_OBJECTS_GRENADEKIT;
        {$ENDIF}
      end;
    OBJECT_FLAMER_KIT:  // flamerkit
      begin
        Thing[i].Skeleton := BoxSkeleton;
        Thing[i].Skeleton.VDamping := 0.989;
        Thing[i].Skeleton.Gravity := 1.17 * GRAV;
        Thing[i].Radius := KIT_RADIUS;
        Thing[i].TimeOut := FLAG_TIMEOUT;
        Thing[i].Interest := DEFAULT_INTEREST_TIME;
        Thing[i].CollideWithBullets := sv_kits_collide.Value;
        {$IFNDEF SERVER}
        Thing[i].Texture := GFX_OBJECTS_FLAMERKIT;
        {$ENDIF}
      end;
    OBJECT_PREDATOR_KIT:  // predatorkit
      begin
        Thing[i].Skeleton := BoxSkeleton;
        Thing[i].Skeleton.VDamping := 0.989;
        Thing[i].Skeleton.Gravity := 1.17 * GRAV;
        Thing[i].Radius := KIT_RADIUS;
        Thing[i].TimeOut := FLAG_TIMEOUT;
        Thing[i].Interest := DEFAULT_INTEREST_TIME;
        Thing[i].CollideWithBullets := sv_kits_collide.Value;
        {$IFNDEF SERVER}
        Thing[i].Texture := GFX_OBJECTS_PREDATORKIT;
        {$ENDIF}
      end;
    OBJECT_VEST_KIT:  // vestkit
      begin
        Thing[i].Skeleton := BoxSkeleton;
        Thing[i].Skeleton.VDamping := 0.989;
        Thing[i].Skeleton.Gravity := 1.17 * GRAV;
        Thing[i].Radius := KIT_RADIUS;
        Thing[i].TimeOut := FLAG_TIMEOUT;
        Thing[i].Interest := DEFAULT_INTEREST_TIME;
        Thing[i].CollideWithBullets := sv_kits_collide.Value;
        {$IFNDEF SERVER}
        Thing[i].Texture := GFX_OBJECTS_VESTKIT;
        {$ENDIF}
      end;
    OBJECT_BERSERK_KIT:  // berserkerkit
      begin
        Thing[i].Skeleton := BoxSkeleton;
        Thing[i].Skeleton.VDamping := 0.989;
        Thing[i].Skeleton.Gravity := 1.17 * GRAV;
        Thing[i].Radius := KIT_RADIUS;
        Thing[i].TimeOut := FLAG_TIMEOUT;
        Thing[i].Interest := DEFAULT_INTEREST_TIME;
        Thing[i].CollideWithBullets := sv_kits_collide.Value;
        {$IFNDEF SERVER}
        Thing[i].Texture := GFX_OBJECTS_BERSERKERKIT;
        {$ENDIF}
      end;
    OBJECT_CLUSTER_KIT:  // clusterkit
      begin
        Thing[i].Skeleton := BoxSkeleton;
        Thing[i].Skeleton.VDamping := 0.989;
        Thing[i].Skeleton.Gravity := 1.07 * GRAV;
        Thing[i].Radius := KIT_RADIUS;
        Thing[i].TimeOut := FLAG_TIMEOUT;
        Thing[i].Interest := DEFAULT_INTEREST_TIME;
        Thing[i].CollideWithBullets := sv_kits_collide.Value;
        {$IFNDEF SERVER}
        Thing[i].Texture := GFX_OBJECTS_CLUSTERKIT;
        {$ENDIF}
      end;
    OBJECT_PARACHUTE:  // para
      begin
        Thing[i].Skeleton.VDamping := 0.993;
        Thing[i].Skeleton.Gravity := 1.15 * GRAV;
        Thing[i].Skeleton.Clone(ParaSkeleton);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := GFX_GOSTEK_PARA_ROPE;
        Thing[i].Tex2 := GFX_GOSTEK_PARA;
        {$ENDIF}
        Thing[i].TimeOut := 3600;
      end;
    OBJECT_COMBAT_KNIFE:  // Knife
      begin
        Thing[i].Skeleton.VDamping := 0.994;
        Thing[i].Skeleton.Gravity := 1.15 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton18);

        a := Thing[i].Skeleton.Pos[2];
        Thing[i].Skeleton.Pos[2] := Thing[i].Skeleton.Pos[1];
        Thing[i].Skeleton.OldPos[2] := Thing[i].Skeleton.Pos[1];

        Thing[i].Skeleton.Pos[1] := a;
        Thing[i].Skeleton.OldPos[1] := a;

        Thing[i].Skeleton.Pos[1].X := Thing[i].Skeleton.Pos[1].X + Random(100) / 100;
        Thing[i].Skeleton.Pos[2].X := Thing[i].Skeleton.Pos[2].X - Random(100) / 100;

        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[KNIFE].TextureNum + k;
        Thing[i].Tex2 := 0;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS * 1.5;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_CHAINSAW:  // Chainsaw
      begin
        Thing[i].Skeleton.VDamping := 0.994;
        Thing[i].Skeleton.Gravity := 1.15 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton28);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[CHAINSAW].TextureNum + k;
        Thing[i].Tex2 := 0;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_LAW:  // LAW
      begin
        Thing[i].Skeleton.VDamping := 0.994;
        Thing[i].Skeleton.Gravity := 1.15 * GRAV;
        Thing[i].Skeleton.Clone(RifleSkeleton28);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := Guns[LAW].TextureNum + k;
        {$ENDIF}
        Thing[i].Radius := GUN_RADIUS;
        Thing[i].TimeOut := GUNRESISTTIME;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := sv_guns_collide.Value;
      end;
    OBJECT_STATIONARY_GUN:  // stationary gun
      begin
        Thing[i].Skeleton.VDamping := 0.99;
        Thing[i].Skeleton.Gravity := 0.2 * GRAV;
        Thing[i].Skeleton.Clone(StatSkeleton);
        {$IFNDEF SERVER}
        Thing[i].Tex1 := 0;
        Thing[i].Tex2 := 0;
        {$ENDIF}
        Thing[i].TimeOut := 60;
        Thing[i].Radius := STAT_RADIUS;
        Thing[i].StaticType := False;
        Thing[i].Interest := 0;
        Thing[i].CollideWithBullets := False;
      end;
  end;  // case

  Thing[i].Owner := Owner;
  Thing[i].MoveSkeleton(sPos.X, sPos.Y, False);

  {$IFDEF SERVER}
  // Throw weapon
  if ((sStyle > OBJECT_POINTMATCH_FLAG) and
      (sStyle < OBJECT_MEDICAL_KIT)) or
      (sStyle = OBJECT_LAW) or
      (sStyle = OBJECT_CHAINSAW) then
    if (Owner > 0) and (Owner < MAX_SPRITES + 1) then
    begin
      // Add player velocity
      Thing[i].Skeleton.Pos[1] := Vec2Add(Thing[i].Skeleton.Pos[1], SpriteParts.Velocity[Owner]);
      Thing[i].Skeleton.Pos[2] := Vec2Add(Thing[i].Skeleton.Pos[2], SpriteParts.Velocity[Owner]);

      // Add throw velocity
      b := Sprite[Owner].GetCursorAimDirection();

      if not Sprite[Owner].DeadMeat then
      begin
        WeaponThrowSpeedPos1 := 0.01;
        WeaponThrowSpeedPos2 := 3;
      end
      else
      begin
        WeaponThrowSpeedPos1 := 0.02;
        WeaponThrowSpeedPos2 := 0.64;
      end;

      Vec2Scale(WeaponThrowVelocity, b, WeaponThrowSpeedPos1);
      Thing[i].Skeleton.Pos[1] := Vec2Add(Thing[i].Skeleton.Pos[1], WeaponThrowVelocity);
      Vec2Scale(WeaponThrowVelocity, b, WeaponThrowSpeedPos2);
      Thing[i].Skeleton.Pos[2] := Vec2Add(Thing[i].Skeleton.Pos[2], WeaponThrowVelocity);
    end;

  // send net info
  if sStyle <> OBJECT_PARACHUTE then
    ServerThingMustSnapshot(i);
  {$ENDIF}
  Result := i;
end;

function ThingCollision(ThingNum: Byte; CooldownEnd: LongInt): TThingCollision;
begin
  Result.ThingNum := ThingNum;
  Result.CooldownEnd := CooldownEnd;
end;

function SpawnBoxes(var Start: TVector2; Team: Byte; Num: Byte): Boolean;
var
  i: Integer;
  spawnscount: Integer;
  Spawns: array[1..255] of Integer;
  PreviousSpawn: Integer;
begin
  Trace('SpawnBoxes');
  Result := True;

  Start.X := 0;
  Start.Y := 0;

  for i := 1 to 255 do
    Spawns[i] := -1;

  spawnscount := 0;
  PreviousSpawn := 0;

  for i := 1 to 255 do
    if Map.SpawnPoints[i].Active then
      if Map.SpawnPoints[i].Team = Team then
        if Thing[Num].LastSpawn <> i then
          begin
            Inc(spawnscount);
            Spawns[spawnscount] := i;
          end
          else
            PreviousSpawn := i;

  if spawnscount = 0 then
  begin
    if PreviousSpawn <> 0 then
    begin
      Inc(spawnscount);
      Spawns[spawnscount] := PreviousSpawn;
    end
    else
    begin
      Result := False;
      for i := 1 to 255 do
        if Map.SpawnPoints[i].Active then
        begin
          Inc(spawnscount);
          Spawns[spawnscount] := i;
        end;
    end;
  end;

  if spawnscount > 0 then
  begin
    i := Random(spawnscount) + 1;
    Start.x := Map.SpawnPoints[Spawns[i]].X - 4 + Random(8);
    Start.y := Map.SpawnPoints[Spawns[i]].Y - 4 + Random(4);
    Thing[Num].LastSpawn := Spawns[i];
  end;
end;

function RandomizeStart(var Start: TVector2; Team: Byte): Boolean;
var
  i: Integer;
  spawnscount: Integer;
  Spawns: array[1..255] of Integer;
begin
  Trace('RandomizeStart');

  Result := True;

  Start.X := 0;
  Start.Y := 0;

  for i := 1 to MAX_SPAWNPOINTS do
    Spawns[i] := -1;

  spawnscount := 0;

  for i := 1 to MAX_SPAWNPOINTS do
    if Map.SpawnPoints[i].Active then
      if Map.SpawnPoints[i].Team = Team then
      begin
        Inc(spawnscount);
        Spawns[spawnscount] := i;
      end;

  if spawnscount = 0 then
  begin
    Result := False;
    for i := 1 to MAX_SPAWNPOINTS do
      if Map.SpawnPoints[i].Active then
      begin
        Inc(spawnscount);
        Spawns[spawnscount] := i;
      end;
  end;

  if spawnscount > 0 then
  begin
    i := Random(spawnscount) + 1;
    Start.x := Map.SpawnPoints[Spawns[i]].X - 4 + Random(8);
    Start.y := Map.SpawnPoints[Spawns[i]].Y - 4 + Random(4);
  end;
end;

procedure TThing.Update;
var
  i{$IFDEF SERVER}, j{$ENDIF}: Integer;
  Collided, Collided2: Boolean;
  a, b: TVector2;
  WasStatic: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TThing.Update');
  {$ENDIF}

  WasStatic := StaticType;

  if not StaticType then
  begin
    Collided := False;
    Collided2 := False;

    // Reset the background poly test before collision checks
    BGState.BackgroundTestPrepare();

    for i := 1 to 4 do
    begin
      if Skeleton.Active[i] and
         ((HoldingSprite = 0) or ((HoldingSprite > 0) and (i = 2))) then
      begin
        if Style < OBJECT_USSOCOM then
        begin
          if i = 1 then
          begin
            if CheckMapCollision(i, Skeleton.Pos[i].X - 10, Skeleton.Pos[i].Y - 8) or
               CheckMapCollision(i, Skeleton.Pos[i].X + 10, Skeleton.Pos[i].Y - 8) or
               CheckMapCollision(i, Skeleton.Pos[i].X - 10, Skeleton.Pos[i].Y) or
               CheckMapCollision(i, Skeleton.Pos[i].X + 10, Skeleton.Pos[i].Y) then
            begin
              if Collided then
                Collided2 := True;
              Collided := True;

              if Collided then
                Skeleton.Forces[2].Y := Skeleton.Forces[2].Y + FLAG_STAND_FORCEUP * GRAV;
            end;
          end
          else
          begin
            if CheckMapCollision(i, Skeleton.Pos[i].X, Skeleton.Pos[i].Y) then
            begin
              if Collided then
                Collided2 := True;
              Collided := True;
            end;
          end;
        end
        else if Style >= OBJECT_USSOCOM then
        begin
          if CheckMapCollision(i, Skeleton.Pos[i].X, Skeleton.Pos[i].Y) then
          begin
            if Collided then
              Collided2 := True;
            Collided := True;
          end;
        end;
      end;  // Skeleton.Active[i]
    end;

    // If no background poly contact then reset any background poly status
    BGState.BackgroundTestReset();

    Skeleton.DoVerletTimeStep;

    if (Style = OBJECT_STATIONARY_GUN) and (TimeOut < 0) then
    begin
      Skeleton.Pos[2] := Skeleton.OldPos[2];
      Skeleton.Pos[3] := Skeleton.OldPos[3];
    end;

    // Make the thing static if not moving much
    a := Vec2Subtract(Skeleton.Pos[1], Skeleton.OldPos[1]);
    b := Vec2Subtract(Skeleton.Pos[2], Skeleton.OldPos[2]);
    if Style <> OBJECT_STATIONARY_GUN then
      if Collided and Collided2 then
        if (Vec2Length(a) + Vec2Length(b)) / 2 < MINMOVEDELTA then
          StaticType := True;

    // Sprite is Holding this Flag
    if Style < OBJECT_USSOCOM then
      if (HoldingSprite > 0) and (HoldingSprite < MAX_SPRITES + 1) then
      begin
        Skeleton.Pos[1] := Sprite[HoldingSprite].Skeleton.Pos[8];
        Skeleton.Forces[2].Y := Skeleton.Forces[2].Y + FLAG_HOLDING_FORCEUP * GRAV;
        Interest := DEFAULT_INTEREST_TIME;

        Interest := FLAG_INTEREST_TIME;

        Sprite[HoldingSprite].HoldedThing := Num;
        TimeOut := FLAG_TIMEOUT;

        if BGState.BackgroundStatus <> BACKGROUND_TRANSITION then
        begin
          BGState.BackgroundStatus := Sprite[HoldingSprite].BGState.BackgroundStatus;
          BGState.BackgroundPoly := Sprite[HoldingSprite].BGState.BackgroundPoly;
        end;
      end;  // HoldingSprite > 0
  end;

  {$IFDEF SERVER}
  Trace('TThing.Update 1');
  {$ENDIF}

  // check if flag is in base
  case Style of
    OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG:
      begin
        a.x := Map.SpawnPoints[Map.FlagSpawn[Style]].X;
        a.y := Map.SpawnPoints[Map.FlagSpawn[Style]].Y;

        if Distance(Skeleton.Pos[1].X, Skeleton.Pos[1].Y, a.X, a.Y) < BASE_RADIUS then
        begin
          InBase := True;
          TimeOut := FLAG_TIMEOUT;
          Interest := FLAG_INTEREST_TIME;

          {$IFDEF SERVER}
          if (HoldingSprite > 0) and (HoldingSprite < MAX_SPRITES + 1) then
            if Sprite[HoldingSprite].Player.Team = Style then
              Respawn;
          {$ENDIF}
        end else
        begin
          InBase := False;
        end;

        TeamFlag[Style] := Num;
      end;
    OBJECT_POINTMATCH_FLAG:
      begin
        {$IFNDEF SERVER}
        TeamFlag[1] := Num;
        {$ENDIF}
      end;
  end;

  {$IFDEF SERVER}
  Trace('TThing.Update 2');
  {$ENDIF}

  // check if flag is touchdown
{$IFDEF SERVER}
    if (Style = OBJECT_ALPHA_FLAG) or (Style = OBJECT_BRAVO_FLAG) then
      if (HoldingSprite > 0) and (HoldingSprite < MAX_SPRITES + 1) then
        if Sprite[HoldingSprite].Player.Team <> Style then
        begin  // check if other flag is inbase
          for i := 1 to MAX_THINGS do
            if (Thing[i].Active) and (Thing[i].InBase) and (i <> Num) and
              (Thing[i].HoldingSprite = 0) then  // check if flags are close
              if Distance(Skeleton.Pos[1], Thing[i].Skeleton.Pos[1]) <
                TOUCHDOWN_RADIUS then
              begin
                if Sprite[HoldingSprite].Player.Team = TEAM_ALPHA then
                begin
                  {$IFNDEF SERVER}
                  if sv_gamemode.IntValue = GAMESTYLE_INF then
                    PlaySound(SFX_INFILTMUS)
                  else
                    PlaySound(SFX_CTF);
                  {$ENDIF}

                  Sprite[HoldingSprite].Player.Flags := Sprite[HoldingSprite].Player.Flags + 1;
                  Inc(TeamScore[1], 1);

                  b.x := 0;
                  b.y := 0;
                  if sv_gamemode.Value = GAMESTYLE_INF then
                  begin
                    Inc(TeamScore[1], sv_inf_redaward.Value - 1);
                    // penalty
                    if (PlayersTeamNum[1] > PlayersTeamNum[2]) then
                      Dec(TeamScore[1], 5 * (PlayersTeamNum[1] - PlayersTeamNum[2]));
                    if (TeamScore[1] < 0) then TeamScore[1] := 0;

                    {$IFNDEF SERVER}
                    // flame it
                    for j := 1 to 10 do
                    begin
                      a.x := Thing[i].Skeleton.Pos[2].x - 10 + Random(20);
                      a.y := Thing[i].Skeleton.Pos[2].y - 10 + Random(20);
                      CreateSpark(a, b, 36, 0, 35);
                      if Random(2) = 0 then
                        CreateSpark(a, b, 37, 0, 75);
                    end;
                    {$ENDIF}
                  end;

                  {$IFNDEF SERVER}
                  // cap spark
                  CreateSpark(Thing[i].Skeleton.Pos[2], b, 61, HoldingSprite, 18);
                  {$ENDIF}

                  SortPlayers;
                  {$IFDEF SERVER}
                  mainconsole.console(Sprite[HoldingSprite].Player.Name + ' ' +
                    'scores for Alpha Team', ALPHA_MESSAGE_COLOR);
                  Inc(Sprite[HoldingSprite].Player.ScoresPerSecond);
                  {$ELSE}
                  BigMessage(_('Alpha Team Scores!'), CAPTURECTFMESSAGEWAIT,
                    ALPHA_MESSAGE_COLOR);
                  mainconsole.console(WideFormat(_('%s scores for Alpha Team'),
                    [Sprite[HoldingSprite].Player.Name]),
                    ALPHA_MESSAGE_COLOR);
                  {$ENDIF}

                  ServerFlagInfo(CAPTURERED, HoldingSprite);
                end;
                if Sprite[HoldingSprite].Player.Team = TEAM_BRAVO then
                begin
                  {$IFNDEF SERVER}
                  PlaySound(SFX_CTF);
                  {$ENDIF}
                  Sprite[HoldingSprite].Player.Flags :=
                    Sprite[HoldingSprite].Player.Flags + 1;
                  Inc(TeamScore[2], 1);

                  {$IFNDEF SERVER}
                  // cap spark
                  b.x := 0;
                  b.y := 0;
                  CreateSpark(Thing[i].Skeleton.Pos[2], b, 61, HoldingSprite, 15);
                  {$ENDIF}

                  SortPlayers;
                  {$IFDEF SERVER}
                  mainconsole.console(Sprite[HoldingSprite].Player.Name + ' ' +
                    'scores for Bravo Team', BRAVO_MESSAGE_COLOR);
                  Inc(Sprite[HoldingSprite].Player.ScoresPerSecond);
                  {$ELSE}
                  BigMessage(_('Bravo Team Scores!'), CAPTURECTFMESSAGEWAIT,
                    BRAVO_MESSAGE_COLOR);
                  mainconsole.console(WideFormat(_('%s scores for Bravo Team'),
                    [Sprite[HoldingSprite].Player.Name]), BRAVO_MESSAGE_COLOR);
                  {$ENDIF}
                  ServerFlagInfo(CAPTUREBLUE, HoldingSprite);
                end;
                {$IFDEF SCRIPT}
                ScrptDispatcher.OnFlagScore(HoldingSprite, Style);
                {$ENDIF}

                if bots_chat.Value then
                begin
                  if Sprite[HoldingSprite].Player.ControlMethod = BOT then
                    if Random(Sprite[HoldingSprite].Brain.ChatFreq div 3) = 0 then
                      ServerSendStringMessage(WideString(Sprite[HoldingSprite].Brain.ChatWinning),
                        ALL_PLAYERS, HoldingSprite, MSGTYPE_PUB);
                end;

                Respawn;

                if sv_survivalmode.Value then
                begin
                  SurvivalEndRound := True;

                  // Everyone should die in realistic after cap
                  // because if nobody is dead the round will not end
                  for j := 1 to MAX_SPRITES do
                    if Sprite[j].Active and not Sprite[j].DeadMeat then
                    begin
                      Sprite[j].HealthHit(
                        {$IFDEF SERVER}4000{$ELSE}150{$ENDIF},
                        j, 1, -1, Sprite[j].Skeleton.Pos[12]);
                      Dec(Sprite[j].Player.Deaths);
                    end;
                end;
              end;
        end;
  {$ENDIF}
  {$IFDEF SERVER}
  Trace('TThing.Update 3');
  {$ENDIF}

  if Style = OBJECT_STATIONARY_GUN then
  begin
    CheckStationaryGunCollision{$IFNDEF SERVER}(False){$ENDIF};
  end;

  // check if sprite grabs thing
  {$IFDEF SERVER}
  if Style <> OBJECT_STATIONARY_GUN then
    CheckSpriteCollision;
  {$ENDIF}

  if Style = OBJECT_RAMBO_BOW then
  begin
    for i := 1 to MAX_SPRITES do
      if Sprite[i].Active then
        if (Sprite[i].Weapon.Num = Guns[BOW].Num) or
           (Sprite[i].Weapon.Num = Guns[BOW2].Num) then
        begin
          {$IFNDEF SERVER}
          GameThingTarget := 0;
          {$ENDIF}
          Kill;
        end;
  end;

  {$IFNDEF SERVER}
  // flag on wind sound - and para
  if (Style < OBJECT_USSOCOM) or (Style = OBJECT_PARACHUTE) then
    if Random(75) = 0 then
      if Vec2Length(Vec2Subtract(Skeleton.Pos[2], Skeleton.OldPos[2])) > 1.0 then
        PlaySound(SFX_FLAG + Random(2), Skeleton.Pos[2]);
  {$ENDIF}

  // Parachute
  if Style = OBJECT_PARACHUTE then
    if (HoldingSprite > 0) and (HoldingSprite < MAX_SPRITES + 1) then
    begin
      Skeleton.Pos[4] := Sprite[HoldingSprite].Skeleton.Pos[12];
      Skeleton.Forces[1].Y := -SpriteParts.Velocity[HoldingSprite].Y;
      Sprite[HoldingSprite].HoldedThing := Num;

      if Skeleton.Pos[3].X < Skeleton.Pos[4].x then
      begin
        a := Skeleton.Pos[4];
        Skeleton.Pos[4] := Skeleton.Pos[3];
        Skeleton.OldPos[4] := Skeleton.Pos[3];
        Skeleton.Pos[3] := a;
        Skeleton.OldPos[3] := a;
        SpriteParts.Forces[HoldingSprite].Y := GRAV;
      end;
    end else
    begin
      {$IFNDEF SERVER}
      if TimeOut > 180 then
        TimeOut := 180;
      {$ENDIF}
    end;

  {$IFDEF SERVER}
  Trace('TThing.Update 4');
  {$ENDIF}

  // count Time Out
  TimeOut := TimeOut - 1;
  if TimeOut < -1000 then TimeOut := -1000;
  if TimeOut = 0 then
  begin
    case Style of
      OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG,
      OBJECT_RAMBO_BOW:
        begin
          {$IFDEF SERVER}
          if HoldingSprite > 0 then
            TimeOut := FLAG_TIMEOUT else
            Respawn;
          {$ENDIF}
        end;
      OBJECT_USSOCOM, OBJECT_DESERT_EAGLE, OBJECT_HK_MP5, OBJECT_AK74,
      OBJECT_STEYR_AUG, OBJECT_SPAS12, OBJECT_RUGER77, OBJECT_M79,
      OBJECT_BARRET_M82A1, OBJECT_MINIMI, OBJECT_MINIGUN, OBJECT_COMBAT_KNIFE,
      OBJECT_CHAINSAW, OBJECT_LAW: Kill;
      OBJECT_FLAMER_KIT, OBJECT_PREDATOR_KIT, OBJECT_VEST_KIT,
      OBJECT_BERSERK_KIT, OBJECT_CLUSTER_KIT, OBJECT_PARACHUTE: Kill;
    end;  // case
  end;  // TimeOut = 0

  CheckOutOfBounds;

  if (not WasStatic) and StaticType then
    Move(Skeleton.Pos[1], Skeleton.OldPos[1], 4 * sizeof(TVector2));
end;

{$IFNDEF SERVER}
procedure TThing.Render(TimeElapsed: Extended);
var
  T: ^TGfxSpriteArray;
  _p, a: TVector2;
  _Scala, _ra: TVector2;
  Roto: Single;
  Pos1, Pos2, Pos3, Pos4: ^TVector2;
begin
  T := @Textures;

  if sv_realisticmode.Value then
    if (Owner > 0) and (Owner < MAX_SPRITES + 1) then
      if Sprite[Owner].Active then
        if Sprite[Owner].Visible = 0 then
          Exit;

  Pos1 := @Skeleton.Pos[1];
  Pos2 := @Skeleton.Pos[2];
  Pos3 := @Skeleton.Pos[3];
  Pos4 := @Skeleton.Pos[4];

  // Iluminate the target thing
  if GameThingTarget = Num then
  begin
    _p.x := Pos1.X + (Pos2.X - Pos1.X) / 2 - 12.5;
    _p.y := Pos1.Y + (Pos2.Y - Pos1.Y) / 2 - 12.5;
    GfxDrawSprite(T^[GFX_OBJECTS_ILUM], _p.x, _p.y, RGBA($FFFFFF,
      Round(Abs(5 + 20 * Sin(5.1 * TimeElapsed)))));
  end;

  case Style of
    OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG:  // Flag
      begin
        if (sv_realisticmode.Value) and (HoldingSprite > 0) then
          if Sprite[HoldingSprite].Visible = 0 then
            Exit;

        // fade out (sort of)
        if TimeOut < 300 then
          if (TimeOut mod 6) < 3 then
            Exit;

        _p.x := Pos1.X;
        _p.y := Pos1.Y;
        _ra.x := 0;
        _ra.y := 0;
        Roto := Angle2Points(Pos1^, Pos2^);

        GfxDrawSprite(T^[GFX_OBJECTS_FLAG_HANDLE], _p.x, _p.y, _ra.x, _ra.y, -Roto);

        if InBase then
        begin
          _p.x := Pos1.X + (Pos2.X - Pos1.X) / 2 - 12.5;
          _p.y := Pos1.Y + (Pos2.Y - Pos1.Y) / 2 - 12.5;
          GfxDrawSprite(T^[GFX_OBJECTS_ILUM], _p.x, _p.y, RGBA($FFFFFF,
            Round(Abs(5 + 20 * Sin(5.1 * TimeElapsed)))));
        end;
      end;  // 1,2,3

    OBJECT_USSOCOM, OBJECT_DESERT_EAGLE, OBJECT_HK_MP5, OBJECT_AK74,
    OBJECT_STEYR_AUG, OBJECT_SPAS12, OBJECT_RUGER77, OBJECT_M79,
    OBJECT_BARRET_M82A1, OBJECT_MINIMI, OBJECT_MINIGUN, OBJECT_RAMBO_BOW,
    OBJECT_COMBAT_KNIFE, OBJECT_CHAINSAW, OBJECT_LAW:
      begin
        // fade out (sort of)
        if TimeOut < 300 then
          if (TimeOut mod 6) < 3 then
            Exit;

        _p.x := Pos1.X;
        _p.y := Pos1.Y - 3;
        _ra.x := 0;
        _ra.y := 2;
        Roto := Angle2Points(Pos1^, Pos2^);

        if Tex1 = 0 then
          Exit;

        GfxDrawSprite(T^[Tex1], _p.x, _p.y, _ra.x, _ra.y, -Roto);

        if Tex2 > 0 then
          GfxDrawSprite(T^[Tex2], _p.x, _p.y, _ra.x, _ra.y, -Roto);
      end;  // 4,5,6,7,8,9,10,11,12,13,14

    OBJECT_PARACHUTE:  // para
      begin
        // rope 1
        _p.x := Pos4.X;
        _p.y := Pos4.Y - 0.55;
        _ra.x := 0;
        _ra.y := 0.55;
        Roto := Angle2Points(Pos4^, Pos2^);
        GfxDrawSprite(T^[Tex1], _p.x, _p.y, _ra.x, _ra.y, -Roto);

        // rope 2
        _p.x := Pos4.X;
        _p.y := Pos4.Y - 0.55;
        _ra.x := 0;
        _ra.y := 0.55;
        Roto := Angle2Points(Pos4^, Pos3^) - (5 * Pi/180);
        GfxDrawSprite(T^[Tex1], _p.x, _p.y, _ra.x, _ra.y, -Roto);

        // rope 3
        _p.x := Pos4.X;
        _p.y := Pos4.Y - 0.55;
        _ra.x := 0;
        _ra.y := 0.55;
        Roto := Angle2Points(Pos4^, Pos1^);
        GfxDrawSprite(T^[Tex1], _p.x, _p.y, _ra.x, _ra.y, -Roto);

        a := Vec2Subtract(Pos2^, Pos3^);
        _Scala.Y := Vec2Length(a) / 45.83;
        _Scala.X := _Scala.Y;
        if _Scala.Y > 2 then
          Exit;

        // chute 2
        _p.x := Pos3.X;
        _p.y := Pos3.Y;
        _ra.x := 0;
        _ra.y := 0;
        Roto := Angle2Points(Pos3^, Pos1^);

        GfxDrawSprite(T^[Tex2 + 1], _p.x, _p.y, _Scala.x, _Scala.y, _ra.x, _ra.y,
          -Roto, RGBA(Color));

        // chute 1
        _p.x := Pos1.X;
        _p.y := Pos1.Y;
        _ra.x := 0;
        _ra.y := 0;
        Roto := Angle2Points(Pos1^, Pos2^);

        GfxDrawSprite(T^[Tex2], _p.x, _p.y, _Scala.x, _Scala.y, _ra.x, _ra.y,
          -Roto, RGBA(Color));
      end;  // 23

    OBJECT_STATIONARY_GUN:  // stat gun
      begin
        _p.x := Pos3.X;
        _p.y := Pos3.Y - 20;
        _ra.x := 0;
        _ra.y := 0;
        Roto := Angle2Points(Pos3^, Pos2^);
        GfxDrawSprite(T^[GFX_WEAPONS_M2_STAT], _p.x, _p.y, _ra.x, _ra.y, -Roto);

        _p.x := Pos1.X;
        _p.y := Pos1.Y - 13;
        _ra.x := 5;
        _ra.y := 4;
        Roto := Angle2Points(Pos4^, Pos1^);
        if Pos4.X >= Pos1.X then
          GfxDrawSprite(T^[GFX_WEAPONS_M2_2], _p.x, _p.y, _ra.x, _ra.y, Roto,
            RGBA(255, 255 - 10 * Interest, 255 - 13 * Interest))
        else
          GfxDrawSprite(T^[GFX_WEAPONS_M2], _p.x, _p.y, _ra.x, _ra.y, Roto,
            RGBA(255, 255 - 10 * Interest, 255 - 13 * Interest));
      end;  // 27
  end;  // case
end;

procedure TThing.PolygonsRender;
var
  Pos1, Pos2, Pos3, Pos4: TVector2;
  a: TVector2;
  ColorBase, ColorTop, ColorLow: TGfxColor;
  tc: PGfxRect;
  v: array[0..3] of TGfxVertex;
begin
  if Texture = 0 then
    Exit;

  case Style of
    OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG:  // Flags
      begin
        // fade out (sort of)
        if (TimeOut < 300) then
          if (TimeOut mod 6) < 3 then
            Exit;

        if (sv_realisticmode.Value) and (HoldingSprite > 0) then
          if Sprite[HoldingSprite].Visible = 0 then
            Exit;
      end;
  end;

  Pos1 := Skeleton.Pos[1];
  Pos2 := Skeleton.Pos[2];
  Pos3 := Skeleton.Pos[3];
  Pos4 := Skeleton.Pos[4];

  ColorBase := Default(TGfxColor);
  ColorLow := Default(TGfxColor);
  ColorTop := Default(TGfxColor);

  case Style of
    OBJECT_ALPHA_FLAG:  // Red Flag
      begin
        if sv_gamemode.Value = GAMESTYLE_INF then
        begin
          ColorBase := RGBA($EEEEEE);
          ColorLow  := RGBA($DDEEEE);
          ColorTop  := RGBA($FFEEEE);
        end
        else
        begin
          ColorBase := RGBA($AD1515);
          ColorLow  := RGBA($951515);
          ColorTop  := RGBA($B51515);
        end;
      end;
    OBJECT_BRAVO_FLAG:  // Blue Flag
      begin
        if sv_gamemode.Value = GAMESTYLE_INF then
        begin
          ColorBase := RGBA($333333);
          ColorLow  := RGBA($333322);
          ColorTop  := RGBA($333344);
        end
        else
        begin
          ColorBase := RGBA($0510AD);
          ColorLow  := RGBA($051095);
          ColorTop  := RGBA($0510B5);
        end;
      end;
    OBJECT_POINTMATCH_FLAG:  // Yellow Flag
      begin
        ColorBase := RGBA($ADAD15);
        ColorLow  := RGBA($959515);
        ColorTop  := RGBA($B5B515);
      end;
    OBJECT_MEDICAL_KIT, OBJECT_GRENADE_KIT, OBJECT_FLAMER_KIT, OBJECT_PREDATOR_KIT,
    OBJECT_VEST_KIT, OBJECT_BERSERK_KIT, OBJECT_CLUSTER_KIT:  // Kits
      begin
        ColorBase := RGBA($FFFFFF);
        ColorLow  := RGBA($FFFFFF);
        ColorTop  := RGBA($FFFFFF);
      end;
  end;

  case Style of
    OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG:  // Flags
      begin
        // Move up the position at the handle to halfway between it and the flag tip
        a := Vec2Subtract(Pos2, Pos1);
        Vec2Scale(a, a, 0.5);
        Pos1 := Vec2Add(Pos1, a);
      end;
  end;

  tc := @Textures[Texture].TexCoords;

  v[0] := GfxVertex(Pos2.x, Pos2.y, tc.Left, tc.Top, ColorBase);
  v[1] := GfxVertex(Pos1.x, Pos1.y, tc.Left, tc.Bottom, ColorTop);
  v[2] := GfxVertex(Pos4.x, Pos4.y, tc.Right, tc.Bottom, ColorBase);
  v[3] := GfxVertex(Pos3.x, Pos3.y, tc.Right, tc.Top, ColorLow);

  GfxDrawQuad(Textures[Texture].Texture, v);
end;
{$ENDIF}

function TThing.CheckMapCollision(i: Integer; X, Y: Single): Boolean;
var
  j, w: Integer;
  b: Integer = 0;
  Pos, Perp, PosDiff, PosDiffPerp: TVector2;
  D: Single = 0.0;
  PosDiffLen: Single;
  rx, ry: Integer;
  teamcol: Boolean;
begin
  {$IFDEF SERVER}
  Trace('TThing.CheckMapCollision');
  {$ENDIF}

  Result := False;
  Pos.X := X;
  Pos.Y := Y - 0.5;

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

      teamcol := True;

      if (Owner > 0) and (Owner < MAX_SPRITES + 1) then
      begin
        teamcol := TeamCollides(w, Sprite[Owner].Player.Team, False);
      end;

      if (Style < OBJECT_USSOCOM) and (Map.PolyType[w] > POLY_TYPE_LAVA) and (Map.PolyType[w] < POLY_TYPE_BOUNCY) then
        teamcol := False;

      if teamcol and
         (Map.PolyType[w] <> POLY_TYPE_ONLY_BULLETS) and
         (Map.PolyType[w] <> POLY_TYPE_ONLY_PLAYER) and
         (Map.PolyType[w] <> POLY_TYPE_DOESNT) and
         (Map.PolyType[w] <> POLY_TYPE_ONLY_FLAGGERS) and
         (Map.PolyType[w] <> POLY_TYPE_NOT_FLAGGERS) then
      begin
        if Map.PointInPolyEdges(Pos.X, Pos.y, w) then
        begin
          if BGState.BackgroundTest(w) then
            Continue;

          Perp := Map.ClosestPerpendicular(w, Pos, D, b);

          Vec2Normalize(Perp, Perp);
          Vec2Scale(Perp, Perp, D);

          case Style of
            OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG:
              begin
                if i = 1 then
                begin
                  Skeleton.Pos[i] := Skeleton.OldPos[i];
                end
                else
                begin
                  // FIXME(skoskav): For more accurate bounce, it should be the sum of the object's
                  // momentum and the intrusion on the polygon's perpendicular

                  // Bounce back the Pos with Perp and move the OldPos behind the new Pos, so it now
                  // travels in the direction of Perp
                  PosDiff := Vec2Subtract(Skeleton.Pos[i], Skeleton.OldPos[i]);
                  PosDiffLen := Vec2Length(PosDiff);
                  Vec2Normalize(PosDiffPerp, Perp);
                  Vec2Scale(PosDiffPerp, PosDiffPerp, PosDiffLen);

                  Skeleton.Pos[i] := Vec2Subtract(Skeleton.Pos[i], Perp);
                  Skeleton.OldPos[i] := Vec2Add(Skeleton.Pos[i], PosDiffPerp);

                  if (i = 2) and (HoldingSprite = 0) then
                    Skeleton.Forces[i].Y := Skeleton.Forces[i].Y - 1;
                end;
              end;

            OBJECT_USSOCOM, OBJECT_DESERT_EAGLE, OBJECT_HK_MP5, OBJECT_AK74, OBJECT_STEYR_AUG,
            OBJECT_SPAS12, OBJECT_RUGER77, OBJECT_M79, OBJECT_BARRET_M82A1, OBJECT_MINIMI,
            OBJECT_MINIGUN, OBJECT_COMBAT_KNIFE, OBJECT_CHAINSAW, OBJECT_LAW:
              begin
                Skeleton.Pos[i] := Skeleton.OldPos[i];
                Skeleton.Pos[i] := Vec2Subtract(Skeleton.Pos[i], Perp);

                {$IFNDEF SERVER}
                if (CollideCount[i] = 0) or
                   ((Vec2Length(Vec2Subtract(Skeleton.Pos[i], Skeleton.OldPos[i])) > 1.5) and
                    (CollideCount[i] < 30)) then
                  PlaySound(SFX_WEAPONHIT, Skeleton.Pos[i]);
                {$ENDIF}
              end;

            OBJECT_RAMBO_BOW, OBJECT_MEDICAL_KIT, OBJECT_GRENADE_KIT, OBJECT_FLAMER_KIT,
            OBJECT_PREDATOR_KIT, OBJECT_VEST_KIT, OBJECT_BERSERK_KIT, OBJECT_CLUSTER_KIT:
              begin
                Skeleton.Pos[i] := Skeleton.OldPos[i];
                Skeleton.Pos[i] := Vec2Subtract(Skeleton.Pos[i], Perp);

                {$IFNDEF SERVER}
                if (CollideCount[i] = 0) or
                   ((Vec2Length(Vec2Subtract(Skeleton.Pos[i], Skeleton.OldPos[i])) > 1.5) and
                    (CollideCount[i] < 3)) then
                  PlaySound(SFX_KIT_FALL + Random(2), Skeleton.Pos[i]);
                {$ENDIF}
              end;

            OBJECT_PARACHUTE:
              begin
                Skeleton.Pos[i] := Skeleton.OldPos[i];
                Skeleton.Pos[i] := Vec2Subtract(Skeleton.Pos[i], Perp);

                {$IFNDEF SERVER}
                if (CollideCount[i] = 0) or
                   ((Vec2Length(Vec2Subtract(Skeleton.Pos[i], Skeleton.OldPos[i])) > 1.5) and
                    (CollideCount[i] < 3)) then
                  PlaySound(SFX_FLAG + Random(2), Skeleton.Pos[i]);
                {$ENDIF}
              end;

            OBJECT_STATIONARY_GUN:
              begin
                Skeleton.Pos[i] := Skeleton.OldPos[i];
                Skeleton.Pos[i] := Vec2Subtract(Skeleton.Pos[i], Perp);
              end;
          end;

          // avoid overflow error
          CollideCount[i] := Byte(CollideCount[i] + 1);

          Result := True;
        end;
      end;
    end;
  end;
end;

procedure TThing.Kill;
begin
  {$IFDEF SERVER}
  Trace('TThing.Kill');
  {$ENDIF}

  if (Num <= 0) then  // skip uninited Things
    Exit;
  Thing[Num].Skeleton.Destroy;
  Active := False;
  {$IFNDEF SERVER}
  Texture := 0;
  {$ENDIF}
end;

procedure TThing.CheckOutOfBounds;
var
  i: Integer;
  Bound: Integer;
  SkeletonPos: ^TVector2;
begin
  {$IFDEF SERVER}
  Trace('TThing.CheckOutOfBounds');
  {$ENDIF}

  Bound := Map.SectorsNum * Map.SectorsDivision - 10;

  for i := 1 to 4 do
  begin
    SkeletonPos := @Skeleton.Pos[i];

    if (Abs(SkeletonPos.X) > Bound) or
       (Abs(SkeletonPos.Y) > Bound) then
    begin
      case Style of
        OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG,
        OBJECT_RAMBO_BOW, OBJECT_MEDICAL_KIT, OBJECT_GRENADE_KIT,
        OBJECT_FLAMER_KIT, OBJECT_PREDATOR_KIT, OBJECT_VEST_KIT,
        OBJECT_BERSERK_KIT, OBJECT_CLUSTER_KIT:
          begin
            Respawn;
            {$IFNDEF SERVER}
            Kill;
            {$ENDIF}
          end;

        OBJECT_USSOCOM, OBJECT_DESERT_EAGLE, OBJECT_HK_MP5, OBJECT_AK74,
        OBJECT_STEYR_AUG, OBJECT_SPAS12, OBJECT_RUGER77, OBJECT_M79,
        OBJECT_BARRET_M82A1, OBJECT_MINIMI, OBJECT_MINIGUN, OBJECT_COMBAT_KNIFE,
        OBJECT_CHAINSAW, OBJECT_LAW, OBJECT_STATIONARY_GUN:
          begin
            Kill;
          end;

        OBJECT_PARACHUTE:
          begin
            {$IFNDEF SERVER}
            if (HoldingSprite > 0) and (HoldingSprite < MAX_SPRITES + 1) then
              Sprite[HoldingSprite].HoldedThing := 0;
            HoldingSprite := 0;
            Kill;
            {$ENDIF}
          end;
      end
    end;
  end;
end;

procedure TThing.Respawn;
var
  a: TVector2;
  i: Integer;
begin
  {$IFDEF SERVER}
  Trace('TThing.Respawn');
  {$ENDIF}

  if (HoldingSprite > 0) and (HoldingSprite < MAX_SPRITES + 1) then
  begin
    Sprite[HoldingSprite].HoldedThing := 0;
    if Sprite[HoldingSprite].Player.Team = TEAM_ALPHA then
      Sprite[HoldingSprite].Brain.PathNum := 1;
    if Sprite[HoldingSprite].Player.Team = TEAM_BRAVO then
      Sprite[HoldingSprite].Brain.PathNum := 2;
  end;

  Kill;
  a := Default(TVector2);
  RandomizeStart(a, 0);

  case Style of
    OBJECT_ALPHA_FLAG:      RandomizeStart(a, 5);
    OBJECT_BRAVO_FLAG:      RandomizeStart(a, 6);
    OBJECT_POINTMATCH_FLAG: RandomizeStart(a, 14);
    OBJECT_RAMBO_BOW:       RandomizeStart(a, 15);
    OBJECT_MEDICAL_KIT:     SpawnBoxes(a, 8, Num);
    OBJECT_GRENADE_KIT:     SpawnBoxes(a, 7, Num);
    OBJECT_FLAMER_KIT:      RandomizeStart(a, 11);
    OBJECT_PREDATOR_KIT:    RandomizeStart(a, 13);
    OBJECT_VEST_KIT:        RandomizeStart(a, 10);
    OBJECT_BERSERK_KIT:     RandomizeStart(a, 12);
    OBJECT_CLUSTER_KIT:     RandomizeStart(a, 9);
  end;

  CreateThing(a, 255, Style, Num);
  Thing[Num].TimeOut := FLAG_TIMEOUT;
  Thing[Num].Interest := DEFAULT_INTEREST_TIME;
  Thing[Num].StaticType := False;

  for i := 1 to 4 do
    Thing[Num].CollideCount[i] := 0;


  if Style = OBJECT_RAMBO_BOW then
    Thing[Num].Interest := BOW_INTEREST_TIME;
  if Style < OBJECT_USSOCOM then
    Thing[Num].Interest := FLAG_INTEREST_TIME;

  // send net info
  {$IFDEF SERVER}
  ServerThingMustSnapshot(Num);
  {$ENDIF}
end;

procedure TThing.MoveSkeleton(x1, y1: Single; FromZero: Boolean);
var
  i: Integer;
begin
  {$IFDEF SERVER}
  Trace('TThing.MoveSkeleton');
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

{$IFDEF SERVER}
function TThing.CheckSpriteCollision: Integer;
var
  j, closestplayer: Integer;
  Pos, Norm, ColPos, a: TVector2;
  k, closestdist, dist: Single;
  WeaponIndex: Byte;
  CapColor: Cardinal;
  {$IFDEF SERVER}
  SmallCapTextStr: string;
  {$ELSE}
  BigCapText: WideString = '';
  SmallCapText: WideString = '';
  {$ENDIF}
begin
  {$IFDEF SERVER}
  Trace('TThing.CheckSpriteCollision');
  {$ENDIF}

  Result := -1;

  a := Vec2Subtract(Skeleton.Pos[1], Skeleton.Pos[2]);
  k := Vec2Length(a) / 2;
  Vec2Normalize(a, a);
  Vec2Scale(a, a, -k);
  Pos := Vec2Add(Skeleton.Pos[1], a);

  // iterate through sprites
  closestdist := 9999999;
  closestplayer := -1;
  for j := 1 to MAX_SPRITES do
    if Sprite[j].Active and not Sprite[j].DeadMeat and Sprite[j].IsNotSpectator() then
    begin
      ColPos := SpriteParts.Pos[j];
      Norm := Vec2Subtract(Pos, ColPos);
      if Vec2Length(Norm) >= Radius then
      begin
        Pos := Skeleton.Pos[1];
        ColPos := SpriteParts.Pos[j];
        Norm := Vec2Subtract(Pos, ColPos);

        if Vec2Length(Norm) >= Radius then
        begin
          Pos := Skeleton.Pos[2];
          ColPos := SpriteParts.Pos[j];
          Norm := Vec2Subtract(Pos, ColPos);
        end;
      end;

      dist := Vec2Length(Norm);
      if dist < Radius then
        if dist < closestdist then
          {$IFDEF SERVER}
          if not ((Style = OBJECT_MEDICAL_KIT) and (Sprite[j].Health = STARTHEALTH)) then
            if not ((Style = OBJECT_GRENADE_KIT) and (Sprite[j].TertiaryWeapon.AmmoCount = sv_maxgrenades.Value) and
              (Sprite[j].TertiaryWeapon.Num = Guns[FRAGGRENADE].Num)) then
          {$ENDIF}
              if not ((Style < OBJECT_USSOCOM) and (Sprite[j].CeaseFireCounter > 0)) then
              begin
                closestdist := dist;
                closestplayer := j;
              end;
    end;  // for j

  j := closestplayer;

  if j > 0 then  // collision
  begin
    if ((((Style > OBJECT_POINTMATCH_FLAG) and (Style < OBJECT_RAMBO_BOW) and
      (Sprite[j].BodyAnimation.ID <> Change.ID)) or ((Style > OBJECT_PARACHUTE) and
      (Sprite[j].BodyAnimation.ID <> Change.ID))) and
      (Sprite[j].Weapon.Num = Guns[NOWEAPON].Num) and
      (Sprite[j].Brain.FavWeapon <> Guns[NOWEAPON].Num) and
      (TimeOut < GUNRESISTTIME - 30)) or ((Style = 15) and
      (Sprite[j].Weapon.Num = Guns[NOWEAPON].Num) and
      (TimeOut < (sv_respawntime.Value * FLAG_TIMEOUT - 100))) or
      (((Style = OBJECT_MEDICAL_KIT) and (Sprite[j].Health < STARTHEALTH)
      {$IFDEF SERVER}and (Sprite[j].HasPack = False){$ENDIF}) or
      (Style = OBJECT_GRENADE_KIT) and (Sprite[j].TertiaryWeapon.AmmoCount < sv_maxgrenades.Value) and
      ((Sprite[j].TertiaryWeapon.Num <> Guns[CLUSTERGRENADE].Num) or (Sprite[j].TertiaryWeapon.AmmoCount = 0))) or
      ((((Style = OBJECT_FLAMER_KIT) and (Sprite[j].Weapon.Num <> Guns[BOW].Num) and
      (Sprite[j].Weapon.Num <> Guns[BOW2].Num)) or (Style = OBJECT_PREDATOR_KIT) or
      (Style = OBJECT_BERSERK_KIT)) and (Sprite[j].BonusStyle = BONUS_NONE) and
      (Sprite[j].CeaseFireCounter < 1)) or ((Style = OBJECT_VEST_KIT)
      {$IFDEF SERVER}and (Sprite[j].Vest < DEFAULTVEST){$ENDIF}) or
      ((Style = OBJECT_CLUSTER_KIT)
      {$IFDEF SERVER} and
      ((Sprite[j].TertiaryWeapon.Num = Guns[FRAGGRENADE].Num) or
      (Sprite[j].TertiaryWeapon.AmmoCount = 0))
      {$ENDIF}) then
    begin
      {$IFNDEF SERVER}
      if ((Style > OBJECT_POINTMATCH_FLAG) and
          (Style < OBJECT_RAMBO_BOW)) or
         (Style > OBJECT_PARACHUTE) then  // take sound
        PlaySound(SFX_TAKEGUN, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_RAMBO_BOW then  // rambo sound
        PlaySound(SFX_TAKEBOW, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_MEDICAL_KIT then  // take medikit sound
        PlaySound(SFX_TAKEMEDIKIT, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_GRENADE_KIT then  // take grenade kit sound
        PlaySound(SFX_PICKUPGUN, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_FLAMER_KIT then  // take flamer kit sound
        PlaySound(SFX_GODFLAME, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_PREDATOR_KIT then  // take predator kit sound
        PlaySound(SFX_PREDATOR, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_VEST_KIT then  // take vest kit sound
        PlaySound(SFX_VESTTAKE, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_BERSERK_KIT then  // take berserker kit sound
        PlaySound(SFX_BERSERKER, SpriteParts.Pos[Sprite[j].Num])
      else if Style = OBJECT_CLUSTER_KIT then  // take cluster kit sound
        PlaySound(SFX_PICKUPGUN, SpriteParts.Pos[Sprite[j].Num]);
      {$ENDIF}

      {$IFDEF SERVER}
      // Send thing take info through NET
      ServerThingTaken(Num, j);
      {$ENDIF}
      if Style <> OBJECT_RAMBO_BOW then
        Kill;
    end;

    {$IFDEF SERVER}KnifeCan[j] := True;{$ENDIF}

    case Style of
      OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG:
        begin
          if (sv_gamemode.Value = GAMESTYLE_INF) and (Style = OBJECT_ALPHA_FLAG) then
            Exit;

          // Dont allow flag cap when round has ended
          if SurvivalEndRound and
            ((Style = OBJECT_ALPHA_FLAG) or
             (Style = OBJECT_BRAVO_FLAG) or
             (Style = OBJECT_POINTMATCH_FLAG)) then
            Exit;

          StaticType := False;
          TimeOut := FLAG_TIMEOUT;
          Interest := FLAG_INTEREST_TIME;

          if (Sprite[j].Player.Team <> Style) or not InBase then
          begin
            if (HoldingSprite = 0) and (Sprite[j].FlagGrabCooldown < 1) then
            begin
              {$IFNDEF SERVER}
              // capture sound
              PlaySound(SFX_CAPTURE, Skeleton.Pos[1]);
              {$ENDIF}
              HoldingSprite := j;
              {$IFDEF SERVER}
              ServerThingTaken(Num, j);
              {$ENDIF}

              CapColor := CAPTURE_MESSAGE_COLOR;
              case sv_gamemode.Value of
                GAMESTYLE_HTF, GAMESTYLE_CTF:
                  case Sprite[j].Player.Team of
                    TEAM_ALPHA: CapColor := ALPHA_MESSAGE_COLOR;
                    TEAM_BRAVO: CapColor := BRAVO_MESSAGE_COLOR;
                  end;
              end;

              {$IFDEF SERVER}
              SmallCapTextStr := '';
              {$ELSE}
              SmallCapText := '';
              {$ENDIF}

              case sv_gamemode.Value of
                GAMESTYLE_POINTMATCH, GAMESTYLE_HTF:
                begin
                  {$IFDEF SERVER}
                  SmallCapTextStr := Sprite[j].Player.Name + ' got the Yellow Flag';
                  if sv_gamemode.Value = GAMESTYLE_HTF then
                  begin
                    Sprite[j].Player.GrabbedInBase := Thing[2].InBase;
                    Inc(Sprite[j].Player.GrabsPerSecond);
                  end;
                  {$ELSE}
                  BigCapText   := iif(j = MySprite, _('You got the Flag!'), _('Yellow Flag captured!'));
                  SmallCapText := _('%s got the Yellow Flag');
                  {$ENDIF}
                  {$IFDEF SCRIPT}
                  ScrptDispatcher.OnFlagGrab(j, Style, Sprite[j].Player.GrabbedInBase);
                  {$ENDIF}
                end;
                GAMESTYLE_CTF:
                  if Sprite[j].Player.Team = Style then
                  begin
                    case Sprite[j].Player.Team of
                      TEAM_ALPHA:
                      begin
                        {$IFDEF SERVER}
                        SmallCapTextStr := Sprite[j].Player.Name + ' returned the Red Flag';
                        {$ELSE}
                        BigCapText   := _('Red flag returned!');
                        SmallCapText := _('%s returned the Red Flag');
                        {$ENDIF}
                      end;
                      TEAM_BRAVO:
                      begin
                        {$IFDEF SERVER}
                        SmallCapTextStr := Sprite[j].Player.Name + ' returned the Blue Flag';
                        {$ELSE}
                        BigCapText   := _('Blue Flag returned!');
                        SmallCapText := _('%s returned the Blue Flag');
                        {$ENDIF}
                      end;
                    end;
                    Respawn;
                    {$IFDEF SCRIPT}
                    ScrptDispatcher.OnFlagReturn(j, Style);
                    {$ENDIF}
                  end else
                  begin
                    case Sprite[j].Player.Team of
                      TEAM_ALPHA:
                      begin
                        {$IFDEF SERVER}
                        SmallCapTextStr := Sprite[j].Player.Name + ' captured the Blue Flag';
                        Sprite[j].Player.GrabbedInBase := Thing[2].InBase;
                        Inc(Sprite[j].Player.GrabsPerSecond);
                        {$ELSE}
                        BigCapText   := iif(j = MySprite, _('You got the Blue Flag!'), _('Blue Flag captured!'));
                        SmallCapText := _('%s captured the Blue Flag');
                        {$ENDIF}
                      end;
                      TEAM_BRAVO:
                      begin
                        {$IFDEF SERVER}
                        SmallCapTextStr := Sprite[j].Player.Name + ' captured the Red Flag';
                        Sprite[j].Player.GrabbedInBase := Thing[1].InBase;
                        Inc(Sprite[j].Player.GrabsPerSecond);
                        {$ELSE}
                        BigCapText   := iif(j = MySprite, _('You got the Red Flag!'), _('Red Flag captured!'));
                        SmallCapText := _('%s captured the Red Flag');
                        {$ENDIF}
                      end;
                    end;
                    {$IFDEF SCRIPT}
                    ScrptDispatcher.OnFlagGrab(j, Style, Sprite[j].Player.GrabbedInBase);
                    {$ENDIF}
                  end;
                GAMESTYLE_INF:
                  if Sprite[j].Player.Team = Style then
                  begin
                    if Sprite[j].Player.Team = TEAM_BRAVO then
                    begin
                      {$IFDEF SERVER}
                      SmallCapTextStr := Sprite[j].Player.Name + ' returned the Objective';
                      Sprite[j].Player.GrabbedInBase := Thing[2].InBase;
                      Inc(Sprite[j].Player.GrabsPerSecond);
                      {$ELSE}
                      BigCapText   := iif(j = MySprite, _('You returned the Objective!'), _('Objective returned!'));
                      SmallCapText := _('%s returned the Objective');
                      {$ENDIF}
                    end;
                    {$IFDEF SCRIPT}
                    ScrptDispatcher.OnFlagReturn(j, Style);
                    {$ENDIF}
                    Respawn;
                  end else
                  begin
                    if Sprite[j].Player.Team = TEAM_ALPHA then
                    begin
                      {$IFDEF SERVER}
                      SmallCapTextStr := Sprite[j].Player.Name + ' captured the Objective';
                      Sprite[j].Player.GrabbedInBase := Thing[2].InBase;
                      Inc(Sprite[j].Player.GrabsPerSecond);
                      {$ELSE}
                      BigCapText   := iif(j = MySprite, _('You got the Objective!'), _('Objective captured!'));
                      SmallCapText := _('%s captured the Objective');
                      {$ENDIF}
                    end;
                    {$IFDEF SCRIPT}
                    ScrptDispatcher.OnFlagGrab(j, Style, Sprite[j].Player.GrabbedInBase);
                    {$ENDIF}
                  end;
              end;

              {$IFDEF SERVER}
              if SmallCapTextStr <> '' then
                mainconsole.console(SmallCapTextStr, CapColor);
              {$ELSE}
              if SmallCapText <> '' then
              begin
                BigMessage(BigCapText, CAPTUREMESSAGEWAIT, CapColor);
                mainconsole.console(WideFormat(SmallCapText, [Sprite[j].Player.Name]), CapColor);
              end;
              {$ENDIF}
            end;
          end;
        end;
      OBJECT_USSOCOM, OBJECT_DESERT_EAGLE, OBJECT_HK_MP5, OBJECT_AK74, OBJECT_STEYR_AUG,
      OBJECT_SPAS12, OBJECT_RUGER77, OBJECT_M79, OBJECT_BARRET_M82A1, OBJECT_MINIMI, OBJECT_MINIGUN:
        if Sprite[j].Weapon.Num = Guns[NOWEAPON].Num then
          if Sprite[j].Brain.FavWeapon <> Guns[NOWEAPON].Num then
            if Sprite[j].BodyAnimation.ID <> Change.ID then
              if TimeOut < GUNRESISTTIME - 30 then
              begin
                // Objects 1-3 are flags, so we need for WeaponIndex subtract by flags+1
                WeaponIndex := WeaponNumToIndex(Style - (OBJECT_NUM_FLAGS + 1));
                {$IFDEF SCRIPT}
                // event must be before actual weapon apply.
                // script might've called ForceWeapon, which we should check.
                // if it did, we don't apply snapshot weapon's as they were already applied
                // by force weapon.
                ForceWeaponCalled := False;
                ScrptDispatcher.OnWeaponChange(j, Guns[WeaponIndex].Num, Sprite[j].SecondaryWeapon.Num,
                  AmmoCount, Sprite[j].SecondaryWeapon.AmmoCount);

                if not ForceWeaponCalled then
                begin;
                {$ENDIF}
                  Sprite[j].ApplyWeaponByNum(Guns[WeaponIndex].Num, 1);
                  Sprite[j].Weapon.AmmoCount := AmmoCount;
                  Sprite[j].Weapon.FireIntervalPrev := Sprite[j].Weapon.FireInterval;
                  Sprite[j].Weapon.FireIntervalCount := Sprite[j].Weapon.FireInterval;
                {$IFNDEF SERVER}
                  if j = MySprite then
                    ClientSpriteSnapshot;
                {$ENDIF}
                {$IFDEF SCRIPT}
                end;
                {$ENDIF}
              end;
      OBJECT_RAMBO_BOW:
        if Sprite[j].Weapon.Num = Guns[NOWEAPON].Num then
          if Sprite[j].BodyAnimation.ID <> Change.ID then
            if TimeOut < FLAG_TIMEOUT - 100 then
            begin
              {$IFDEF SCRIPT}
              // event must be before actual weapon apply.
              // script might've called ForceWeapon, which we should check.
              // if it did, we don't apply snapshot weapon's as they were already applied
              // by force weapon.
              ForceWeaponCalled := False;
              ScrptDispatcher.OnWeaponChange(j, Guns[BOW].Num, Guns[BOW2].Num,
                1, 1);

              if not ForceWeaponCalled then
              begin
              {$ENDIF}
                Sprite[j].ApplyWeaponByNum(Guns[BOW].Num, 1);
                Sprite[j].ApplyWeaponByNum(Guns[BOW2].Num, 2);
                //BUG: shouldn't this be Guns[BOW].Ammo? Somebody might've set more than one
                Sprite[j].Weapon.AmmoCount := 1;
                Sprite[j].Weapon.FireIntervalPrev := Sprite[j].Weapon.FireInterval;
                Sprite[j].Weapon.FireIntervalCount := Sprite[j].Weapon.FireInterval;
                Sprite[j].WearHelmet := 1;
                {$IFNDEF SERVER}
                  if j = MySprite then
                    ClientSpriteSnapshot;
                {$ENDIF}
              {$IFDEF SCRIPT}
              end;
              {$ENDIF}

              {$IFNDEF SERVER}
              if j = MySprite then
              begin
                BigMessage(_('You got the Bow!'), CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);
                if not LimboLock then
                  GameMenuShow(LimboMenu, False);
              end else
                BigMessage(WideFormat(_('%s got the Bow!'),
                  [Sprite[j].Player.Name]), CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);
              {$ENDIF}
            end;
      OBJECT_MEDICAL_KIT: begin
          if Sprite[j].Health < STARTHEALTH then
          begin
            // pickup health pack
            {$IFDEF SERVER}if Sprite[j].HasPack = False then{$ENDIF}
            begin
              Team := Sprite[j].Player.Team;
              {$IFDEF SERVER}
              if sv_healthcooldown.Value > 0 then
                Sprite[j].HasPack := True;
              {$ENDIF}
              Sprite[j].Health := STARTHEALTH;
              Respawn;

              {$IFDEF SCRIPT}
              ScrptDispatcher.OnKitPickup(j, Num);
              {$ENDIF}
            end;
          end;
        end;
      OBJECT_GRENADE_KIT: begin
          if (Sprite[j].TertiaryWeapon.AmmoCount < sv_maxgrenades.Value) and
             ((Sprite[j].TertiaryWeapon.Num <> Guns[CLUSTERGRENADE].Num) or
              (Sprite[j].TertiaryWeapon.AmmoCount = 0)) then
          begin
            Team := Sprite[j].Player.Team;
            Sprite[j].TertiaryWeapon := Guns[FRAGGRENADE];
            Sprite[j].TertiaryWeapon.AmmoCount := sv_maxgrenades.Value;
            Respawn;

            {$IFDEF SCRIPT}
            ScrptDispatcher.OnKitPickup(j, Num);
            {$ENDIF}
          end;
        end;
      OBJECT_FLAMER_KIT: if (Sprite[j].BonusStyle = BONUS_NONE) and
             (Sprite[j].CeaseFireCounter < 1) then
          begin
            if (Sprite[j].Weapon.Num <> Guns[BOW].Num) and
               (Sprite[j].Weapon.Num <> Guns[BOW2].Num) then
            begin
              {$IFDEF SCRIPT}
              // event must be before actual weapon apply.
              // script might've called ForceWeapon, which we should check.
              // if it did, we don't apply snapshot weapon's as they were already applied
              // by force weapon.
              ForceWeaponCalled := False;
              ScrptDispatcher.OnWeaponChange(j, Guns[FLAMER].Num, Sprite[j].SecondaryWeapon.Num,
                AmmoCount, Sprite[j].SecondaryWeapon.AmmoCount);
              ScrptDispatcher.OnKitPickup(j, Num);

              if not ForceWeaponCalled then
              begin;
              {$ENDIF}
                Sprite[j].ApplyWeaponByNum(Sprite[j].Weapon.Num, 2
                  {$IFNDEF SERVER}, -1, True{$ENDIF});
                Sprite[j].ApplyWeaponByNum(Guns[FLAMER].Num, 1);
                Sprite[j].BonusTime := FLAMERBONUSTIME;
                Sprite[j].BonusStyle := BONUS_FLAMEGOD;

                {$IFNDEF SERVER}
                if j = MySprite then begin
                  BigMessage(_('Flame God Mode!'), CAPTUREMESSAGEWAIT, BONUS_MESSAGE_COLOR);
                  ClientSpriteSnapshot;
                end;
                {$ENDIF}

                Sprite[j].Health := STARTHEALTH;
              {$IFDEF SCRIPT}
              end;
              {$ENDIF}
            end;
          end;
      OBJECT_PREDATOR_KIT: if (Sprite[j].BonusStyle = BONUS_NONE) and
             (Sprite[j].CeaseFireCounter < 1) then
        begin
          Sprite[j].Alpha := PREDATORALPHA;
          Sprite[j].BonusTime := PREDATORBONUSTIME;
          Sprite[j].BonusStyle := BONUS_PREDATOR;

          {$IFNDEF SERVER}
          if j = MySprite then
            BigMessage(_('Predator Mode!'), CAPTUREMESSAGEWAIT, BONUS_MESSAGE_COLOR);
          {$ENDIF}

          Sprite[j].Health := STARTHEALTH;

          {$IFDEF SCRIPT}
          ScrptDispatcher.OnKitPickup(j, Num);
          {$ENDIF}
        end;
      OBJECT_VEST_KIT:
        begin
          Sprite[j].Vest := DEFAULTVEST;

          {$IFNDEF SERVER}
          if j = MySprite then
            BigMessage(_('Bulletproof Vest!'), CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);
          {$ENDIF}

          {$IFDEF SCRIPT}
          ScrptDispatcher.OnKitPickup(j, Num);
          {$ENDIF}
        end;
      OBJECT_BERSERK_KIT: if (Sprite[j].BonusStyle = 0) and
                             (Sprite[j].CeaseFireCounter < 1) then
        begin
          Sprite[j].BonusStyle := BONUS_BERSERKER;
          Sprite[j].BonusTime := BERSERKERBONUSTIME;

          {$IFNDEF SERVER}
          if j = MySprite then
            BigMessage(_('Berserker Mode!'), CAPTUREMESSAGEWAIT, BONUS_MESSAGE_COLOR);
          {$ENDIF}

          Sprite[j].Health := STARTHEALTH;

          {$IFDEF SCRIPT}
          ScrptDispatcher.OnKitPickup(j, Num);
          {$ENDIF}
        end;
      OBJECT_CLUSTER_KIT: {$IFDEF SERVER}if (Sprite[j].TertiaryWeapon.Num = Guns[FRAGGRENADE].Num) or
                                            (Sprite[j].TertiaryWeapon.AmmoCount = 0) then{$ENDIF}
        begin
          Sprite[j].TertiaryWeapon := Guns[CLUSTERGRENADE];
          Sprite[j].TertiaryWeapon.AmmoCount := CLUSTER_GRENADES;

          {$IFNDEF SERVER}
          if j = MySprite then
            BigMessage(_('Cluster grenades!'), CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);
          {$ENDIF}

          {$IFDEF SCRIPT}
          ScrptDispatcher.OnKitPickup(j, Num);
          {$ENDIF}
        end;
      OBJECT_COMBAT_KNIFE, OBJECT_CHAINSAW, OBJECT_LAW:
        if Sprite[j].Weapon.Num = Guns[NOWEAPON].Num then
          if Sprite[j].Brain.FavWeapon <> Guns[NOWEAPON].Num then
            if Sprite[j].BodyAnimation.ID <> Change.ID then
              if TimeOut < GUNRESISTTIME - 30 then
              begin
                // There are in total OBJECT_NUM_NONWEAPON non-weapon objects before the
                // knife so we need to subtract it+1 for the WeaponIndex (like before)
                WeaponIndex := WeaponNumToIndex(Style - (OBJECT_NUM_NONWEAPON + 1));
                {$IFDEF SCRIPT}
                // event must be before actual weapon apply.
                // script might've called ForceWeapon, which we should check.
                // if it did, we don't apply snapshot weapon's as they were already applied
                // by force weapon.
                ForceWeaponCalled := False;
                ScrptDispatcher.OnWeaponChange(j, Guns[WeaponIndex].Num, Sprite[j].SecondaryWeapon.Num,
                  AmmoCount, Sprite[j].SecondaryWeapon.AmmoCount);

                if not ForceWeaponCalled then
                begin
                {$ENDIF}
                  Sprite[j].ApplyWeaponByNum(Guns[WeaponIndex].Num, 1);
                  Sprite[j].Weapon.AmmoCount := AmmoCount;
                  Sprite[j].Weapon.FireIntervalPrev := Sprite[j].Weapon.FireInterval;
                  Sprite[j].Weapon.FireIntervalCount := Sprite[j].Weapon.FireInterval;
                {$IFNDEF SERVER}
                  if j = MySprite then
                    ClientSpriteSnapshot;
                {$ENDIF}
                {$IFDEF SCRIPT}
                end;
                {$ENDIF}
              end;
    end;

    Result := j;
  end;
end;
{$ENDIF}

function TThing.CheckStationaryGunCollision
  {$IFNDEF SERVER}(ClientCheck: Boolean){$ENDIF}: Integer;
var
  i, j, k: Integer;
  Pos, Norm, ColPos: TVector2;
begin
  {$IFDEF SERVER}
  Trace('TThing.CheckStationaryGunCollision');
  {$ENDIF}

  Result := -1;
  if TimeOut > 0 then
    Exit;

  // Stat overheat less
  if Interest > M2GUN_OVERHEAT + 1 then
    Interest := 0;

  if Interest > 0 then
    if MainTickCounter mod 8 = 0 then
      Dec(Interest);

  Pos := Skeleton.Pos[1];

  for i := 1 to MAX_SPRITES do
    if Sprite[i].Active and not Sprite[i].DeadMeat and Sprite[i].IsNotSpectator() then
      if Sprite[i].Stat = Num then
      begin
        ColPos := SpriteParts.Pos[i];
        Norm := Vec2Subtract(Pos, ColPos);
        if Vec2Length(Norm) < Radius then  // collision
        begin
          if Sprite[i].Player.ControlMethod = BOT then
            if Sprite[i].Brain.Camper > 0 then
              if Sprite[i].HoldedThing = 0 then
              begin
                Sprite[i].Control.Right := False;
                Sprite[i].Control.Left := False;
                Sprite[i].Control.Up := False;
                Sprite[i].Control.Down := False;
                if Sprite[i].LegsAnimation.ID = Prone.ID then
                  Sprite[i].Control.Prone := True;
              end;

          StaticType := True;

          Pos.X := Sprite[i].Control.MouseAimX;
          Pos.Y := Sprite[i].Control.MouseAimY;
          Norm := Vec2Subtract(Pos, Sprite[i].Skeleton.Pos[15]);
          Vec2Normalize(Norm, Norm);
          Vec2Scale(Norm, Norm, 3);
          Norm.X := -Norm.X;
          Skeleton.OldPos[4] := Skeleton.Pos[4];
          Skeleton.Pos[4] := Vec2Add(Skeleton.Pos[1], Norm);

          Interest := Sprite[i].UseTime;

          if Sprite[i].Control.Fire then
            if Sprite[i].LegsAnimation.ID = Stand.ID then
              if MainTickCounter mod Guns[M2].FireInterval = 0 then
              begin
                if Sprite[i].UseTime > M2GUN_OVERHEAT then
                begin
                  {$IFNDEF SERVER}
                  PlaySound(SFX_M2OVERHEAT, SpriteParts.Pos[i]);
                  {$ENDIF}
                  Exit;
                end;

                k := 0;
                if Sprite[i].UseTime > M2GUN_OVERAIM then
                begin
                  k := (Sprite[i].UseTime div 11);
                  k := -k + Random(2 * k);
                end;

                Pos.x := Skeleton.Pos[4].X; Pos.y := Skeleton.Pos[4].Y;
                Norm := Vec2Subtract(Pos, Skeleton.Pos[1]);
                Vec2Normalize(Norm, Norm);
                Vec2Scale(Norm, Norm, Guns[M2].Speed);
                Norm.X := -Norm.X;
                Norm.x := Norm.x + k;
                Norm.y := Norm.y + k;
                Pos.x := Skeleton.Pos[4].X + 4;
                Pos.y := Skeleton.Pos[4].Y - 10;

                CreateBullet(Pos, Norm, Guns[M2].Num, i, 255,
                  Guns[M2].HitMultiply, True, False);

                {$IFNDEF SERVER}
                Pos := Vec2Add(Pos, Norm);
                Vec2Scale(Norm, Norm, 0.1);
                CreateSpark(Pos, Norm, 35, i, 15);  // smoke

                ColPos := Norm;
                Norm.y := -Sprite[i].Direction * Norm.x;
                norm.x := Sprite[i].Direction * Colpos.y;
                Vec2Scale(Norm, Norm, 0.2);
                Pos.x := Skeleton.Pos[3].X + 18;
                Pos.y := Skeleton.Pos[3].Y - 20;
                CreateSpark(Pos, Norm, 22, i, 255);  // hull

                PlaySound(SFX_M2FIRE, SpriteParts.Pos[i]);
                {$ENDIF}

                Inc(Sprite[i].UseTime);
              end;
          Exit;
        end else
        begin
          Sprite[i].Stat := 0;
          StaticType := False;
          Exit;
        end;
      end;

  // iterate through sprites
  {$IFNDEF SERVER}if ClientCheck then{$ENDIF}
    if not StaticType then
      for j := 1 to MAX_SPRITES do
        if Sprite[j].Active and not Sprite[j].DeadMeat and Sprite[j].IsNotSpectator() then
        begin
          ColPos := SpriteParts.Pos[j];
          Norm := Vec2Subtract(Pos, ColPos);
          if Vec2Length(Norm) < Radius then  // collision
          begin
            case Style of
              OBJECT_STATIONARY_GUN:
                begin
                  if Sprite[j].Player.ControlMethod = BOT then
                    if Sprite[j].Brain.Camper > 0 then
                    begin
                      Sprite[j].Control.Right := False;
                      Sprite[j].Control.Left := False;
                      Sprite[j].Control.Up := False;
                      Sprite[j].Control.Down := False;
                      Sprite[j].LegsApplyAnimation(Stand, 1);
                    end;

                  if Sprite[j].LegsAnimation.ID = Stand.ID then
                  begin
                    {$IFNDEF SERVER}
                    PlaySound(SFX_M2USE, SpriteParts.Pos[j]);
                    {$ENDIF}

                    StaticType := True;
                    Sprite[j].Stat := Num;
                    Sprite[j].Brain.OnePlaceCount := 0;
                    {$IFDEF SERVER}
                    ServerThingTaken(Num, j);
                    {$ENDIF}
                  end;
                end;
            end;

            Result := j;
            Exit;
          end else
            if Sprite[j].Stat = Num then
              Sprite[j].Stat := 0;

          StaticType := False;
        end;  // for j
end;

end.

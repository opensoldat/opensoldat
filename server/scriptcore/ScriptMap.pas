{*******************************************************}
{                                                       }
{       ScriptMap unit for OPENSOLDAT                   }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}
unit ScriptMap;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  Vector,
  PascalCompiler,
  PascalExec,
  PolyMap,
  ScriptCore3Api,
  ScriptObject,
  ScriptBullet,
  ScriptSpawnPoint,
  ScriptPlayer,
  Sprites,
  Things,
  SysUtils,
  Server,
  Game,
  Weapons;

type

  TOnBeforeMapChange = procedure(Next: string) of object;
  TOnAfterMapChange = procedure(Next: string) of object;

  TScriptMap = class(TObject)
  private
    FObjects: array [1..MAX_THINGS] of TScriptActiveObject;
    FBullets: array [1..MAX_BULLETS] of TScriptActiveBullet;
    FSpawnpoints: array [1..MAX_SPAWNPOINTS] of TScriptActiveSpawnPoint;
    FLastFlagObjs: array [1..3] of TScriptActiveFlag;
    FOnBeforeMapChange: TOnBeforeMapChange;
    FOnAfterMapChange: TOnAfterMapChange;
    function GetObject(ID: Byte): TScriptActiveObject;
    function GetBullet(ID: Byte): TScriptActiveBullet;
    function GetSpawn(ID: Byte): TScriptSpawnPoint;
    procedure SetSpawn(ID: Byte; const Spawn: TScriptSpawnPoint);
    function GetName: string;
  public
    constructor Create;
    function GetFlag(ID: Integer): TScriptActiveFlag;
    function RayCast(x1, y1, x2, y2: Single; Player: Boolean = False;
      Flag: Boolean = False; Bullet: Boolean = True; CheckCollider: Boolean = False;
      Team: Byte = 0): Boolean;
    function RayCastVector(A, B: TVector2; Player: Boolean = False;
      Flag: Boolean = False; Bullet: Boolean = True; CheckCollider: Boolean = False;
      Team: Byte = 0): Boolean;
    function CreateBulletVector(A, B: TVector2; HitM: Single; sStyle: Byte; Owner: TScriptActivePlayer): Integer;
    function CreateBullet(X, Y, VelX, VelY, HitM: Single; sStyle: Byte; Owner: TScriptActivePlayer): Integer;
    function AddObject(Obj: TScriptNewObject): TScriptActiveObject;
    function AddSpawnPoint(Spawn: TScriptNewSpawnPoint): TScriptActiveSpawnPoint;
    procedure NextMap;
    procedure SetMap(NewMap: string);
    property Objects[ID: Byte]: TScriptActiveObject read GetObject;
    property Bullets[ID: Byte]: TScriptActiveBullet read GetBullet;
    property Spawns[ID: Byte]: TScriptSpawnPoint read GetSpawn write SetSpawn;
    property Name: string read GetName;
    property RedFlag: TScriptActiveFlag index 1 read GetFlag;
    property BlueFlag: TScriptActiveFlag index 2 read GetFlag;
    property YellowFlag: TScriptActiveFlag index 3 read GetFlag;
    property OnBeforeMapChange: TOnBeforeMapChange
      read FOnBeforeMapChange write FOnBeforeMapChange;
    property OnAfterMapChange: TOnAfterMapChange
      read FOnAfterMapChange write FOnAfterMapChange;
  end;

  TScriptMapAPI = class(TScriptCore3API)
  private
    FMap: TScriptMap;
  public
    //constructor Create(ScriptCore3: TScript);
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
    procedure RuntimeRegisterVariables(Exec: TPascalExec); override;
    property Map: TScriptMap read FMap;
  end;

implementation

uses
  Bullets,
  Constants;

constructor TScriptMap.Create;
var
  I: Byte;
begin
  for I := 1 to MAX_THINGS do
    Self.FObjects[I] := TScriptActiveObject.CreateActive(I, Thing[I]);
  for I := 1 to MAX_BULLETS do
    Self.FBullets[I] := TScriptActiveBullet.CreateActive(I, Bullet[I]);
  for I := 1 to MAX_SPAWNPOINTS do
    Self.FSpawnpoints[I] :=
      TScriptActiveSpawnPoint.CreateActive(I, Map.SpawnPoints[I]);
end;

function TScriptMap.GetObject(ID: Byte): TScriptActiveObject;
begin
  if (ID > MAX_THINGS) or (ID = 0) then
    raise Exception.Create('ID must be from 1 to ' + IntToStr(MAX_THINGS));
  Result := FObjects[ID];
end;

function TScriptMap.GetBullet(ID: Byte): TScriptActiveBullet;
begin
  if (ID > MAX_BULLETS) or (ID = 0) then
    raise Exception.Create('ID must be from 1 to ' + IntToStr(MAX_BULLETS));
  Result := FBullets[ID];
end;

function TScriptMap.GetSpawn(ID: Byte): TScriptSpawnPoint;
begin
  if ID = 0 then
    raise Exception.Create('ID must be from 1 to ' + IntToStr(MAX_SPAWNPOINTS));
  Result := FSpawnpoints[ID];
end;

procedure TScriptMap.SetSpawn(ID: Byte; const Spawn: TScriptSpawnPoint);
begin
  FSpawnpoints[ID].Active := Spawn.Active;
  FSpawnpoints[ID].X := Spawn.X;
  FSpawnpoints[ID].Y := Spawn.Y;
  FSpawnpoints[ID].Style := Spawn.Style;
end;

function TScriptMap.GetName: string;
begin
  Result := Map.Name;
end;

function TScriptMap.GetFlag(ID: Integer): TScriptActiveFlag;
var
  I: Byte;
begin
  Result := Self.FLastFlagObjs[ID];
  if (Result = nil) or (Result.Style <> ID) then
  begin
    Self.FLastFlagObjs[ID].Free;
    Self.FLastFlagObjs[ID] := nil;
    for I := 1 to MAX_THINGS do
      if Thing[I].Style = ID then
      begin
        Self.FLastFlagObjs[ID] := TScriptActiveFlag.CreateActive(I, Thing[I]);
        Result := Self.FLastFlagObjs[ID];
        break;
      end;
  end;
end;

function TScriptMap.RayCast(x1, y1, x2, y2: Single; Player: Boolean = False;
  Flag: Boolean = False; Bullet: Boolean = True; CheckCollider: Boolean = False;
  Team: Byte = 0): Boolean;
var
  A, B: TVector2;
  Distance: Single = 0.0;
begin
  A.x := x1;
  A.y := y1;
  B.x := x2;
  B.y := y2;
  Result := Map.RayCast(a, b, Distance, 99999, Player, Flag, Bullet,
    CheckCollider, Team);
end;

function TScriptMap.RayCastVector(A, B: TVector2; Player: Boolean = False;
  Flag: Boolean = False; Bullet: Boolean = True; CheckCollider: Boolean = False;
  Team: Byte = 0): Boolean;
var
  Distance: Single = 0.0;
begin
  Result := Map.RayCast(A, B, Distance, 99999, Player, Flag, Bullet,
    CheckCollider, Team);
end;

function TScriptMap.CreateBulletVector(A, B: TVector2; HitM: Single; sStyle: Byte; Owner: TScriptActivePlayer): Integer;
var
  i: Integer;
  FoundWeaponIndex: Integer;
begin
  FoundWeaponIndex := -1;
  for i := Low(Guns) to High(Guns) do
  begin
    if Guns[i].BulletStyle = SStyle then
    begin
      FoundWeaponIndex := i;
      Break;
    end;
  end;

  if FoundWeaponIndex >= 0 then
    Result := ServerCreateBullet(A, B, Guns[FoundWeaponIndex].Num,
      Owner.ID, 255, HitM, True)
  else
    Result := -1;
end;

function TScriptMap.CreateBullet(X, Y, VelX, VelY, HitM: Single; sStyle: Byte; Owner: TScriptActivePlayer): Integer;
var
  SPos, SVel: TVector2;
begin
  SPos.x := X;
  SPos.y := Y;
  SVel.x := VelX;
  SVel.y := VelY;
  Result := CreateBulletVector(SPos, SVel, HitM, sStyle, Owner);
end;

function TScriptMap.AddObject(Obj: TScriptNewObject): TScriptActiveObject;
var
  SPos: TVector2;
  I: Shortint;
  RealWeapon: Byte;
begin
  Result := nil;
  if (Obj.Style > OBJECT_STATIONARY_GUN) or
     (Obj.Style < OBJECT_ALPHA_FLAG) then
    Exit;
  SPos.x := Obj.X;
  SPos.y := Obj.Y;
  I := CreateThing(SPos, 255, Obj.Style, 255);
  Result := Self.Objects[I];
  if ((Obj.Style > OBJECT_POINTMATCH_FLAG) and
      (Obj.Style < OBJECT_MEDICAL_KIT)) or
     ((Obj.Style > OBJECT_PARACHUTE) and
      (Obj.Style < OBJECT_STATIONARY_GUN + 1)) then
  begin
    case Obj.Style of
      OBJECT_USSOCOM:        RealWeapon := COLT;      // USSOCOM
      OBJECT_DESERT_EAGLE:   RealWeapon := EAGLE;     // Desert Eagle
      OBJECT_HK_MP5:         RealWeapon := MP5;       // HK MP5
      OBJECT_AK74:           RealWeapon := AK74;      // AK 74
      OBJECT_STEYR_AUG:      RealWeapon := STEYRAUG;  // Steyr AUG
      OBJECT_SPAS12:         RealWeapon := SPAS12;    // Spas 12
      OBJECT_RUGER77:        RealWeapon := RUGER77;   // Ruger77
      OBJECT_M79:            RealWeapon := M79;       // M79
      OBJECT_BARRET_M82A1:   RealWeapon := BARRETT;   // Barrett M82A1
      OBJECT_MINIMI:         RealWeapon := M249;      // Minimi
      OBJECT_MINIGUN:        RealWeapon := MINIGUN;   // Minigun
      OBJECT_RAMBO_BOW:      RealWeapon := BOW;       // Rambo Bow
      OBJECT_COMBAT_KNIFE:   RealWeapon := CHAINSAW;  // Chainsaw
      OBJECT_CHAINSAW:       RealWeapon := KNIFE;     // Combat Knife
      OBJECT_LAW:            RealWeapon := LAW;       // LAW
      OBJECT_STATIONARY_GUN: RealWeapon := M2;        // Stationary Gun
      else
        Exit;
    end;
    Thing[I].AmmoCount := Round(Guns[RealWeapon].Ammo);
  end;
end;


function TScriptMap.AddSpawnPoint(Spawn: TScriptNewSpawnPoint): TScriptActiveSpawnPoint;
var
  i: Byte;
begin
  Result := nil;
  for i := 1 to MAX_SPAWNPOINTS do
    if not Map.SpawnPoints[i].Active then
    begin
      Map.SpawnPoints[i].X := Spawn.X;
      Map.SpawnPoints[i].Y := Spawn.Y;
      Map.SpawnPoints[i].Team := Spawn.Style;
      Map.SpawnPoints[i].Active := True;
      // Map.SpawnPointsTeam doesn't seem to be used anywhere so I removed it
      {if (TeamBase[1] > 0) and (TeamBase[2] > 0) then
      begin
        if Distance(Map.SpawnPoints[i].X, Map.SpawnPoints[i].Y,
          Map.SpawnPoints[TeamBase[1]].X, Map.SpawnPoints[TeamBase[1]].Y) >
          Distance(Map.SpawnPoints[i].X, Map.SpawnPoints[i].Y,
          Map.SpawnPoints[TeamBase[2]].X, Map.SpawnPoints[TeamBase[2]].Y) then
          Map.SpawnPointsTeam[i] := 2
        else
          Map.SpawnPointsTeam[i] := 1;
      end;}
      Result := TScriptActiveSpawnPoint(FSpawnpoints[i]);
      break;
    end;
end;

procedure TScriptMap.NextMap;
begin
  Server.NextMap;
end;

procedure TScriptMap.SetMap(NewMap: string);
begin
  if not PrepareMapChange(NewMap) then begin
    MainConsole.Console('Map not found (' + NewMap + ')'
    , WARNING_MESSAGE_COLOR);
    Exit;
  end;
end;

procedure ObjectReadHelper(Self: TScriptMap; var Result: TScriptActiveObject;
  const ID: Byte);
begin
  Result := Self.Objects[ID];
end;

procedure BulletReadHelper(Self: TScriptMap; var Result: TScriptActiveBullet;
  const ID: Byte);
begin
  Result := Self.Bullets[ID];
end;

procedure SpawnReadHelper(Self: TScriptMap; var Result: TScriptSpawnPoint;
  const ID: Byte);
begin
  Result := Self.Spawns[ID];
end;

procedure SpawnWriteHelper(Self: TScriptMap; const Result: TScriptSpawnPoint;
  const ID: Byte);
begin
  Self.Spawns[ID] := Result;
end;

procedure NameReadHelper(Self: TScriptMap; var Result: string);
begin
  Result := Self.Name;
end;

procedure RedFlagReadHelper(Self: TScriptMap; var Result: TScriptActiveFlag);
begin
  Result := Self.RedFlag;
end;

procedure BlueFlagReadHelper(Self: TScriptMap; var Result: TScriptActiveFlag);
begin
  Result := Self.BlueFlag;
end;

procedure YellowFlagReadHelper(Self: TScriptMap; var Result: TScriptActiveFlag);
begin
  Result := Self.YellowFlag;
end;

procedure OnBeforeMapChangeReadHelper(Self: TScriptMap; var Result: TOnBeforeMapChange);
begin
  Result := Self.OnBeforeMapChange;
end;

procedure OnBeforeMapWriteHelper(Self: TScriptMap; const Result: TOnBeforeMapChange);
begin
  Self.OnBeforeMapChange := Result;
end;

procedure OnAfterMapChangeReadHelper(Self: TScriptMap; var Result: TOnAfterMapChange);
begin
  Result := Self.OnAfterMapChange;
end;

procedure OnAfterMapWriteHelper(Self: TScriptMap; const Result: TOnAfterMapChange);
begin
  Self.OnAfterMapChange := Result;
end;

procedure TScriptMapAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  AClass: TPascalCompiletimeClass;
begin
  Compiler.AddType('TOnBeforeMapChangeEvent', 'procedure(Next: string)');
  Compiler.AddType('TOnAfterMapChangeEvent', 'procedure(Next: string)');

  AClass := Compiler.AddClass(nil, 'TMap');
  with AClass do
  begin
    RegisterMethod(
      'function RayCast(x1, y1, x2, y2: Single; Player, Flag, Bullet, CheckCollider: Boolean; Team: Byte): Boolean');
    RegisterMethod(
      'function RayCastVector(A, B: TVector; Player, Flag, Bullet, CheckCollider: Boolean; Team: Byte): Boolean');
    RegisterMethod(
      'function CreateBulletVector(A, B: TVector; HitM: Single; sStyle: Byte; Owner: TActivePlayer): Integer');
    RegisterMethod(
      'function CreateBullet(X, Y, VelX, VelY, HitM: Single; sStyle: Byte; Owner: TActivePlayer): Integer');
    RegisterMethod('function AddObject(Obj: TNewMapObject): TActiveMapObject');
    RegisterMethod('function AddSpawnPoint(Spawn: TNewSpawnPoint): TActiveSpawnPoint');
    RegisterMethod('procedure NextMap');
    RegisterMethod('procedure SetMap(NewMap: string)');
    RegisterProperty('Objects', 'TActiveMapObject Byte', iptR);
    RegisterProperty('Bullets', 'TActiveMapBullet Byte', iptR);
    RegisterProperty('Spawns', 'TActiveSpawnPoint Byte', iptR);
    RegisterProperty('RedFlag', 'TActiveFlag', iptR);
    RegisterProperty('BlueFlag', 'TActiveFlag', iptR);
    RegisterProperty('YellowFlag', 'TActiveFlag', iptR);
    RegisterProperty('OnBeforeMapChange', 'TOnBeforeMapChangeEvent', iptRW);
    RegisterProperty('OnAfterMapChange', 'TOnAfterMapChangeEvent', iptRW);
  end;
  Compiler.AddPtrVariable('Map', AClass.aType);
end;

procedure TScriptMapAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptMap, 'TMap') do
  begin
    RegisterMethod(@TScriptMap.RayCast, 'RayCast');
    RegisterMethod(@TScriptMap.RayCastVector, 'RayCastVector');
    RegisterMethod(@TScriptMap.CreateBulletVector, 'CreateBulletVector');
    RegisterMethod(@TScriptMap.CreateBullet, 'CreateBullet');
    RegisterMethod(@TScriptMap.AddObject, 'AddObject');
    RegisterMethod(@TScriptMap.AddSpawnPoint, 'AddSpawnPoint');
    RegisterMethod(@TScriptMap.NextMap, 'NextMap');
    RegisterMethod(@TScriptMap.SetMap, 'SetMap');
    RegisterPropertyHelper(@ObjectReadHelper, nil, 'Objects');
    RegisterPropertyHelper(@BulletReadHelper, nil, 'Bullets');
    RegisterPropertyHelper(@SpawnReadHelper, @SpawnWriteHelper, 'Spawns');
    RegisterPropertyHelper(@RedFlagReadHelper, nil, 'RedFlag');
    RegisterPropertyHelper(@BlueFlagReadHelper, nil, 'BlueFlag');
    RegisterPropertyHelper(@YellowFlagReadHelper, nil, 'YellowFlag');
    RegisterEventPropertyHelper(@OnBeforeMapChangeReadHelper,
      @OnBeforeMapWriteHelper, 'OnBeforeMapChange');
    RegisterEventPropertyHelper(@OnAfterMapChangeReadHelper,
      @OnAfterMapWriteHelper, 'OnAfterMapChange');
  end;
end;

procedure TScriptMapAPI.RuntimeRegisterVariables(Exec: TPascalExec);
begin
  Self.FMap := TScriptMap.Create;
  Exec.SetPointerToData('Map', @Self.FMap, Exec.FindType(btClass));
end;

end.

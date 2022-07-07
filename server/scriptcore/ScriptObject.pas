{*******************************************************}
{                                                       }
{       ScriptObject unit for OPENSOLDAT                }
{                                                       }
{       Copyright (c) 2013 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}
unit ScriptObject;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  ScriptCore3Api,
  Things,
  SysUtils,
  Server,
  Game;

type

  PThing = ^TThing;

  TScriptObject = class(TObject)
  protected
    FObj: PThing;
    function GetStyle: Byte; virtual; abstract;
    function GetX: Single;
    function GetY: Single;
  public
    property Style: Byte read GetStyle;
    property X: Single read GetX;
    property Y: Single read GetY;
  end;

  TScriptNewObject = class(TScriptObject)
  protected
    function GetStyle: Byte; override;
    procedure SetStyle(Style: Byte);
    procedure SetX(X: Single);
    procedure SetY(Y: Single);
  public
    constructor Create;
    destructor Destroy; override;
    property Style: Byte read GetStyle write SetStyle;
    property X: Single read GetX write SetX;
    property Y: Single read GetY write SetY;
  end;

  TScriptActiveObject = class(TScriptObject)
  protected
    FID: Byte;
    function GetActive: Boolean;
    function GetID: Byte;
    function GetStyle: Byte; override;
  public
    constructor CreateActive(ID: Byte; var Obj: TThing);
    procedure Kill;
    property Active: Boolean read GetActive;
    property ID: Byte read GetID;
  end;

  TScriptActiveFlag = class(TScriptActiveObject)
  private
    function GetInBase: Boolean;
  public
    property InBase: Boolean read GetInBase;
  end;

  TScriptObjectAPI = class(TScriptCore3API)
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

uses
  NetworkServerThing, Parts, Constants;

function TScriptObject.GetX: Single;
begin
  Result := Self.FObj^.Skeleton.Pos[1].X;
end;

function TScriptObject.GetY: Single;
begin
  Result := Self.FObj^.Skeleton.Pos[1].Y;
end;

function TScriptNewObject.GetStyle: Byte;
begin
  Result := Self.FObj^.Style;
end;

constructor TScriptNewObject.Create;
begin
  New(Self.FObj);
  FillChar(Self.FObj^, SizeOf(Self.FObj), #0);
end;

destructor TScriptNewObject.Destroy;
begin
  Freemem(Self.FObj, SizeOf(TThing));
end;

procedure TScriptNewObject.SetStyle(Style: Byte);
begin
  Self.FObj^.Style := Style;
end;

procedure TScriptNewObject.SetX(X: Single);
var
  i: Integer;
begin
  for i := 1 to NUM_PARTICLES do
    Self.FObj^.Skeleton.Pos[i].X := X;
end;

procedure TScriptNewObject.SetY(Y: Single);
var
  i: Integer;
begin
  for i := 1 to NUM_PARTICLES do
    Self.FObj^.Skeleton.Pos[i].Y := Y;
end;

function TScriptActiveObject.GetActive: Boolean;
begin
  Result := Self.FObj^.Active;
end;

function TScriptActiveObject.GetID: Byte;
begin
  Result := Self.FID;
end;

function TScriptActiveObject.GetStyle: Byte;
begin
  Result := Self.FObj^.Style;
end;

constructor TScriptActiveObject.CreateActive(ID: Byte; var Obj: TThing);
begin
  Self.FID := ID;
  Self.FObj := @Obj;
end;

procedure TScriptActiveObject.Kill;
begin
  if not Thing[Self.FID].Active then
    Exit;
  Thing[Self.FID].Kill;
  ServerThingTaken(Self.FID, 255);
end;

function TScriptActiveFlag.GetInBase: Boolean;
begin
  Result := Self.FObj^.InBase;
end;

procedure IDReadHelper(Self: TScriptActiveObject; var Result: Byte);
begin
  Result := Self.ID;
end;

procedure ActiveReadHelper(Self: TScriptActiveObject; var Result: Boolean);
begin
  Result := Self.Active;
end;

procedure StyleReadHelper(Self: TScriptObject; var Result: Byte);
begin
  Result := Self.Style;
end;

procedure StyleWriteHelper(Self: TScriptNewObject; const Result: Byte);
begin
  Self.Style := Result;
end;

procedure XReadHelper(Self: TScriptObject; var Result: Single);
begin
  Result := Self.X;
end;

procedure XWriteHelper(Self: TScriptNewObject; const Result: Single);
begin
  Self.X := Result;
end;

procedure YReadHelper(Self: TScriptObject; var Result: Single);
begin
  Result := Self.Y;
end;

procedure YWriteHelper(Self: TScriptNewObject; const Result: Single);
begin
  Self.Y := Result;
end;

procedure InBaseReadHelper(Self: TScriptActiveFlag; var Result: Boolean);
begin
  Result := Self.InBase;
end;

procedure TScriptObjectAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  MapObject, NewMapObject, ActiveMapObject: TPascalCompiletimeClass;
begin
  Compiler.AddConstant('OBJECT_ALPHA_FLAG',      'Byte').SetInt(OBJECT_ALPHA_FLAG);
  Compiler.AddConstant('OBJECT_BRAVO_FLAG',      'Byte').SetInt(OBJECT_BRAVO_FLAG);
  Compiler.AddConstant('OBJECT_POINTMATCH_FLAG', 'Byte').SetInt(OBJECT_POINTMATCH_FLAG);
  Compiler.AddConstant('OBJECT_USSOCOM',         'Byte').SetInt(OBJECT_USSOCOM);
  Compiler.AddConstant('OBJECT_DESERT_EAGLE',    'Byte').SetInt(OBJECT_DESERT_EAGLE);
  Compiler.AddConstant('OBJECT_HK_MP5',          'Byte').SetInt(OBJECT_HK_MP5);
  Compiler.AddConstant('OBJECT_AK74',            'Byte').SetInt(OBJECT_AK74);
  Compiler.AddConstant('OBJECT_STEYR_AUG',       'Byte').SetInt(OBJECT_STEYR_AUG);
  Compiler.AddConstant('OBJECT_SPAS12',          'Byte').SetInt(OBJECT_SPAS12);
  Compiler.AddConstant('OBJECT_RUGER77',         'Byte').SetInt(OBJECT_RUGER77);
  Compiler.AddConstant('OBJECT_M79',             'Byte').SetInt(OBJECT_M79);
  Compiler.AddConstant('OBJECT_BARRET_M82A1',    'Byte').SetInt(OBJECT_BARRET_M82A1);
  Compiler.AddConstant('OBJECT_MINIMI',          'Byte').SetInt(OBJECT_MINIMI);
  Compiler.AddConstant('OBJECT_MINIGUN',         'Byte').SetInt(OBJECT_MINIGUN);
  Compiler.AddConstant('OBJECT_RAMBO_BOW',       'Byte').SetInt(OBJECT_RAMBO_BOW);
  Compiler.AddConstant('OBJECT_MEDICAL_KIT',     'Byte').SetInt(OBJECT_MEDICAL_KIT);
  Compiler.AddConstant('OBJECT_GRENADE_KIT',     'Byte').SetInt(OBJECT_GRENADE_KIT);
  Compiler.AddConstant('OBJECT_FLAMER_KIT',      'Byte').SetInt(OBJECT_FLAMER_KIT);
  Compiler.AddConstant('OBJECT_PREDATOR_KIT',    'Byte').SetInt(OBJECT_PREDATOR_KIT);
  Compiler.AddConstant('OBJECT_VEST_KIT',        'Byte').SetInt(OBJECT_VEST_KIT);
  Compiler.AddConstant('OBJECT_BERSERK_KIT',     'Byte').SetInt(OBJECT_BERSERK_KIT);
  Compiler.AddConstant('OBJECT_CLUSTER_KIT',     'Byte').SetInt(OBJECT_CLUSTER_KIT);
  Compiler.AddConstant('OBJECT_PARACHUTE',       'Byte').SetInt(OBJECT_PARACHUTE);
  Compiler.AddConstant('OBJECT_COMBAT_KNIFE',    'Byte').SetInt(OBJECT_COMBAT_KNIFE);
  Compiler.AddConstant('OBJECT_CHAINSAW',        'Byte').SetInt(OBJECT_CHAINSAW);
  Compiler.AddConstant('OBJECT_LAW',             'Byte').SetInt(OBJECT_LAW);
  Compiler.AddConstant('OBJECT_STATIONARY_GUN',  'Byte').SetInt(OBJECT_STATIONARY_GUN);

  MapObject := Compiler.AddClass(nil, 'TMapObject');
  with MapObject do
  begin
    RegisterProperty('Style', 'Byte', iptR);
    RegisterProperty('X', 'Single', iptR);
    RegisterProperty('Y', 'Single', iptR);
  end;

  NewMapObject := Compiler.AddClass(MapObject, 'TNewMapObject');
  with NewMapObject do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Free');
    RegisterProperty('Style', 'Byte', iptRW);
    RegisterProperty('X', 'Single', iptRW);
    RegisterProperty('Y', 'Single', iptRW);
  end;

  ActiveMapObject := Compiler.AddClass(MapObject, 'TActiveMapObject');
  with ActiveMapObject do
  begin
    RegisterMethod('procedure Kill');
    RegisterProperty('ID', 'Byte', iptR);
    RegisterProperty('Active', 'Boolean', iptR);
  end;

  with Compiler.AddClass(ActiveMapObject, 'TActiveFlag') do
  begin
    RegisterProperty('InBase', 'Boolean', iptR);
  end;
end;

procedure TScriptObjectAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptObject, 'TMapObject') do
  begin
    RegisterPropertyHelper(@StyleReadHelper, nil, 'Style');
    RegisterPropertyHelper(@XReadHelper, nil, 'X');
    RegisterPropertyHelper(@YReadHelper, nil, 'Y');
  end;

  with Exec.AddClass(TScriptNewObject, 'TNewMapObject') do
  begin
    RegisterConstructor(@TScriptNewObject.Create, 'Create');
    RegisterMethod(@TScriptNewObject.Free, 'Free');
    RegisterPropertyHelper(@StyleReadHelper, @StyleWriteHelper, 'Style');
    RegisterPropertyHelper(@XReadHelper, @XWriteHelper, 'X');
    RegisterPropertyHelper(@YReadHelper, @YWriteHelper, 'Y');
  end;

  with Exec.AddClass(TScriptActiveObject, 'TActiveMapObject') do
  begin
    RegisterMethod(@TScriptActiveObject.Kill, 'Kill');
    RegisterPropertyHelper(@IDReadHelper, nil, 'ID');
    RegisterPropertyHelper(@ActiveReadHelper, nil, 'Active');
  end;

  with Exec.AddClass(TScriptActiveFlag, 'TActiveFlag') do
  begin
    RegisterPropertyHelper(@InBaseReadHelper, nil, 'InBase');
  end;
end;

end.

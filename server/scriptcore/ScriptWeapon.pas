{*******************************************************}
{                                                       }
{       ScriptWeapon unit for OPENSOLDAT                }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}
unit ScriptWeapon;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  NetworkServerFunctions,
  PascalCompiler,
  PascalExec,
  Sprites,
  ScriptCore3Api,
  SysUtils,
  Weapons;

type
  PGun = ^TGun;
  PSprite = ^TSprite;

  TScriptWeapon = class(TObject)
  protected
    FWeapon: PGun;
    function GetGun: TGun;
    function GetType: Byte;
    function GetName: string;
    function GetBulletStyle: Byte;
    function GetAmmo: Byte;
    procedure SetAmmo(Ammo: Byte); virtual; abstract;
  public
    property Gun: TGun read GetGun;

    property WType: Byte read GetType;
    property Name: string read GetName;
    property BulletStyle: Byte read GetBulletStyle;
    property Ammo: Byte read GetAmmo write SetAmmo;
  end;

  TScriptPlayerWeapon = class(TScriptWeapon)
  protected
    FSprite: PSprite;
  public
    constructor Create(var Sprite: TSprite); virtual;
  end;

  TScriptPrimaryPlayerWeapon = class(TScriptPlayerWeapon)
  protected
    procedure SetAmmo(Ammo: Byte); override;
  public
    constructor Create(var Sprite: TSprite); override;
  end;

  TScriptSecondaryPlayerWeapon = class(TScriptPlayerWeapon)
  protected
    procedure SetAmmo(Ammo: Byte); override;
  public
    constructor Create(var Sprite: TSprite); override;
  end;

  TScriptNewWeapon = class(TScriptWeapon)
  private
    procedure SetType(WType: Byte);
  protected
    procedure SetAmmo(Ammo: Byte); override;
  public
    constructor Create;
    destructor Destroy; override;
    property WType: Byte read GetType write SetType;
  end;

  // Not exported, only used for OnWeaponChange workaround
  TScriptWeaponChange = class(TScriptPlayerWeapon)
  protected
    procedure SetAmmo(Ammo: Byte); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure SetGun(Weapon, Ammo: Byte);
  end;

  TScriptWeaponAPI = class(TScriptCore3API)
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

constructor TScriptNewWeapon.Create;
begin
  New(Self.FWeapon);
  FillChar(Self.FWeapon^, SizeOf(TGun), #0);
  Self.FWeapon^.Num := NOWEAPON;
end;

destructor TScriptNewWeapon.Destroy;
begin
  Dispose(Self.FWeapon);
end;

constructor TScriptWeaponChange.Create;
begin
  New(Self.FWeapon);
  FillChar(Self.FWeapon^, SizeOf(TGun), #0);
  Self.FWeapon^ := Guns[NOWEAPON];
end;

destructor TScriptWeaponChange.Destroy;
begin
  Dispose(Self.FWeapon);
end;

procedure TScriptWeaponChange.SetAmmo(Ammo: Byte);
begin
  Self.FWeapon^.AmmoCount := Ammo;
end;

constructor TScriptPlayerWeapon.Create(var Sprite: TSprite);
begin
  Self.FSprite := @Sprite;
end;

constructor TScriptPrimaryPlayerWeapon.Create(var Sprite: TSprite);
begin
  inherited;
  Self.FWeapon := @Sprite.Weapon;
end;

constructor TScriptSecondaryPlayerWeapon.Create(var Sprite: TSprite);
begin
  inherited;
  Self.FWeapon := @Sprite.SecondaryWeapon;
end;

function TScriptWeapon.GetGun: TGun;
begin
  Result := Self.FWeapon^;
end;

function TScriptWeapon.GetType: Byte;
begin
  Result := Self.FWeapon.Num;
end;

function TScriptWeapon.GetName: string;
begin
  Result := Self.FWeapon.Name;
end;

function TScriptWeapon.GetBulletStyle: Byte;
begin
  Result := Self.FWeapon.BulletStyle;
end;

function TScriptWeapon.GetAmmo: Byte;
begin
  Result := Self.FWeapon.AmmoCount;
end;

procedure TScriptPrimaryPlayerWeapon.SetAmmo(Ammo: Byte);
begin
  ForceWeapon(Self.FSprite^.Num, Self.WType,
     Self.FSprite^.SecondaryWeapon.Num,
    Ammo, Self.FSprite^.SecondaryWeapon.AmmoCount);
end;

procedure TScriptSecondaryPlayerWeapon.SetAmmo(Ammo: Byte);
begin
  ForceWeapon(Self.FSprite^.Num, Self.WType,
    Self.FSprite^.SecondaryWeapon.Num,
    Self.FSprite^.Weapon.AmmoCount, Ammo);
end;

procedure TScriptNewWeapon.SetAmmo(Ammo: Byte);
begin
  Self.FWeapon^.AmmoCount := Ammo;
end;

procedure TScriptNewWeapon.SetType(WType: Byte);
begin
  Self.FWeapon^.Num := WType;
end;

procedure TScriptWeaponChange.SetGun(Weapon, Ammo: Byte);
var
  RealWeapon: Byte;
begin
  case Weapon of
    EAGLE_NUM:    RealWeapon := EAGLE;     // Desert Eagle
    MP5_NUM:      RealWeapon := MP5;       // HK MP5
    AK74_NUM:     RealWeapon := AK74;      // AK 74
    STEYRAUG_NUM: RealWeapon := STEYRAUG;  // Steyr AUG
    SPAS12_NUM:   RealWeapon := SPAS12;    // Spas 12
    RUGER77_NUM:  RealWeapon := RUGER77;   // Ruger77
    M79_NUM:      RealWeapon := M79;       // M79
    BARRETT_NUM:  RealWeapon := BARRETT;   // Barrett M82A1
    M249_NUM:     RealWeapon := M249;      // Minimi
    MINIGUN_NUM:  RealWeapon := MINIGUN;   // Minigun
    COLT_NUM:     RealWeapon := COLT;      // USSOCOM
    KNIFE_NUM:    RealWeapon := KNIFE;     // Combat Knife
    CHAINSAW_NUM: RealWeapon := CHAINSAW;  // Chainsaw
    LAW_NUM:      RealWeapon := LAW;       // LAW
    FLAMER_NUM:   RealWeapon := FLAMER;    // Flamer
    BOW_NUM:      RealWeapon := BOW;       // Rambo Bow
    BOW2_NUM:     RealWeapon := BOW2;      // Flame Bow
    else
      RealWeapon := NOWEAPON;
  end;
  Self.FWeapon^ := Guns[RealWeapon];
  Self.FWeapon^.AmmoCount := Ammo;
end;

procedure ScriptWeaponGetWType(Self: TScriptWeapon; var Result: Byte);
begin
  Result := Self.WType;
end;

procedure ScriptWeaponGetName(Self: TScriptWeapon; var Result: string);
begin
  Result := Self.Name;
end;

procedure ScriptWeaponGetBulletStyle(Self: TScriptWeapon; var Result: Byte);
begin
  Result := Self.BulletStyle;
end;

procedure ScriptWeaponGetAmmo(Self: TScriptWeapon; var Result: Byte);
begin
  Result := Self.Ammo;
end;

procedure ScriptWeaponSetAmmo(Self: TScriptWeapon; const Result: Byte);
begin
  Self.Ammo := Result;
end;

procedure ScriptNewWeaponSetWType(Self: TScriptNewWeapon; const Result: Byte);
begin
  Self.WType := Result;
end;

procedure TScriptWeaponAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  WeaponClass: TPascalCompiletimeClass;
begin
  Compiler.AddConstant('WTYPE_EAGLE',          'Byte').SetInt(EAGLE_NUM);
  Compiler.AddConstant('WTYPE_MP5',            'Byte').SetInt(MP5_NUM);
  Compiler.AddConstant('WTYPE_AK74',           'Byte').SetInt(AK74_NUM);
  Compiler.AddConstant('WTYPE_STEYRAUG',       'Byte').SetInt(STEYRAUG_NUM);
  Compiler.AddConstant('WTYPE_SPAS12',         'Byte').SetInt(SPAS12_NUM);
  Compiler.AddConstant('WTYPE_RUGER77',        'Byte').SetInt(RUGER77_NUM);
  Compiler.AddConstant('WTYPE_M79',            'Byte').SetInt(M79_NUM);
  Compiler.AddConstant('WTYPE_BARRETT',        'Byte').SetInt(BARRETT_NUM);
  Compiler.AddConstant('WTYPE_M249',           'Byte').SetInt(M249_NUM);
  Compiler.AddConstant('WTYPE_MINIGUN',        'Byte').SetInt(MINIGUN_NUM);
  Compiler.AddConstant('WTYPE_USSOCOM',        'Byte').SetInt(COLT_NUM);
  Compiler.AddConstant('WTYPE_KNIFE',          'Byte').SetInt(KNIFE_NUM);
  Compiler.AddConstant('WTYPE_CHAINSAW',       'Byte').SetInt(CHAINSAW_NUM);
  Compiler.AddConstant('WTYPE_LAW',            'Byte').SetInt(LAW_NUM);
  Compiler.AddConstant('WTYPE_FLAMER',         'Byte').SetInt(FLAMER_NUM);
  Compiler.AddConstant('WTYPE_BOW',            'Byte').SetInt(BOW_NUM);
  Compiler.AddConstant('WTYPE_BOW2',           'Byte').SetInt(BOW2_NUM);
  Compiler.AddConstant('WTYPE_M2',             'Byte').SetInt(M2_NUM);
  Compiler.AddConstant('WTYPE_NOWEAPON',       'Byte').SetInt(NOWEAPON_NUM);
  Compiler.AddConstant('WTYPE_FRAGGRENADE',    'Byte').SetInt(FRAGGRENADE_NUM);
  Compiler.AddConstant('WTYPE_CLUSTERGRENADE', 'Byte').SetInt(CLUSTERGRENADE_NUM);
  Compiler.AddConstant('WTYPE_CLUSTER',        'Byte').SetInt(CLUSTER_NUM);
  Compiler.AddConstant('WTYPE_THROWNKNIFE',    'Byte').SetInt(THROWNKNIFE_NUM);

  WeaponClass := Compiler.AddClass(nil, 'TWeapon');
  with WeaponClass do
  begin
    IsAbstract := True;
    RegisterProperty('WType', 'Byte', iptR);
    RegisterProperty('Name', 'String', iptR);
    RegisterProperty('BulletStyle', 'Byte', iptR);
    RegisterProperty('Ammo', 'Byte', iptRW);
  end;

  with Compiler.AddClass(WeaponClass, 'TNewWeapon') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Free');
    RegisterProperty('WType', 'Byte', iptRW);
  end;

  WeaponClass := Compiler.AddClass(WeaponClass, 'TPlayerWeapon');
  Compiler.AddClass(WeaponClass, 'TPrimaryPlayerWeapon');
  Compiler.AddClass(WeaponClass, 'TSecondaryPlayerWeapon');
end;

procedure TScriptWeaponAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptWeapon, 'TWeapon') do
  begin
    RegisterPropertyHelper(@ScriptWeaponGetWType, nil, 'WType');
    RegisterPropertyHelper(@ScriptWeaponGetName, nil, 'Name');
    RegisterPropertyHelper(@ScriptWeaponGetBulletStyle, nil, 'BulletStyle');
    RegisterPropertyHelper(@ScriptWeaponGetAmmo, @ScriptWeaponSetAmmo, 'Ammo');
  end;

  with Exec.AddClass(TScriptNewWeapon, 'TNewWeapon') do
  begin
    RegisterConstructor(@TScriptNewWeapon.Create, 'Create');
    RegisterMethod(@TScriptNewWeapon.Free, 'Free');
    RegisterPropertyHelper(@ScriptWeaponGetWType, @ScriptNewWeaponSetWType, 'WType');
  end;

  Exec.AddClass(TScriptPlayerWeapon, 'TPlayerWeapon');
  Exec.AddClass(TScriptPrimaryPlayerWeapon, 'TPrimaryPlayerWeapon');
  Exec.AddClass(TScriptPrimaryPlayerWeapon, 'TSecondaryPlayerWeapon');
end;

end.

{*******************************************************}
{                                                       }
{       Weapons Unit for OPENSOLDAT                     }
{                                                       }
{       Copyright (c) 2012 Daniel Forssten              }
{                                                       }
{*******************************************************}

unit Weapons;

interface

type
  TGun = record
    // Scrambled 1.6, to piss off gamehackers.
    Ammo:              Byte;
    AmmoCount:         Byte;
    Num:               Byte;
    MovementAcc:       Single;
    Bink:              SmallInt;
    Recoil:            Word;
    FireInterval:      Word;
    FireIntervalPrev:  Word;
    FireIntervalCount: Word;
    FireIntervalReal:  Single;
    StartUpTime:       Word;
    StartUpTimeCount:  Word;
    ReloadTime:        Word;
    ReloadTimePrev:    Word;
    ReloadTimeCount:   Word;
    ReloadTimeReal:    Single;
    TextureNum:        Byte;
    ClipTextureNum:    Byte;
    ClipReload:        Boolean;
    ClipInTime:        Word;
    ClipOutTime:       Word;
    Name:              string;
    IniName:           string;
    Speed:             Single;
    HitMultiply:       Single;
    BulletSpread:      Single;
    Push:              Single;
    InheritedVelocity: Single;
    ModifierLegs:      Single;
    ModifierChest:     Single;
    ModifierHead:      Single;
    NoCollision:       Byte;
    FireMode:          Byte;
    Timeout:           Word;
    BulletStyle:       Byte;
    FireStyle:         Byte;
    BulletImageStyle:  Byte;
  end;

const
  EAGLE          = 1;
  MP5            = 2;
  AK74           = 3;
  STEYRAUG       = 4;
  SPAS12         = 5;
  RUGER77        = 6;
  M79            = 7;
  BARRETT        = 8;
  M249           = 9;
  MINIGUN        = 10;
  COLT           = 11;
  KNIFE          = 12;
  CHAINSAW       = 13;
  LAW            = 14;
  BOW2           = 15;
  BOW            = 16;
  FLAMER         = 17;
  M2             = 18;
  NOWEAPON       = 19;
  FRAGGRENADE    = 20;
  CLUSTERGRENADE = 21;
  CLUSTER        = 22;
  THROWNKNIFE    = 23;

  PRIMARY_WEAPONS   = 10;
  SECONDARY_WEAPONS = 4;
  BONUS_WEAPONS     = 3;
  MAIN_WEAPONS      = PRIMARY_WEAPONS + SECONDARY_WEAPONS;
  EXTENDED_WEAPONS  = MAIN_WEAPONS + BONUS_WEAPONS;

  ORIGINAL_WEAPONS  = 20;
  TOTAL_WEAPONS     = 23;

  // FIXME(skoskav): Normalize weapons' num with their index
  EAGLE_NUM          = 1;
  MP5_NUM            = 2;
  AK74_NUM           = 3;
  STEYRAUG_NUM       = 4;
  SPAS12_NUM         = 5;
  RUGER77_NUM        = 6;
  M79_NUM            = 7;
  BARRETT_NUM        = 8;
  M249_NUM           = 9;
  MINIGUN_NUM        = 10;
  COLT_NUM           = 0;
  KNIFE_NUM          = 11;
  CHAINSAW_NUM       = 12;
  LAW_NUM            = 13;
  BOW2_NUM           = 16;
  BOW_NUM            = 15;
  FLAMER_NUM         = 14;
  M2_NUM             = 30;
  NOWEAPON_NUM       = 255;
  FRAGGRENADE_NUM    = 50;
  CLUSTERGRENADE_NUM = 51;
  CLUSTER_NUM        = 52;
  THROWNKNIFE_NUM    = 53;

  // BulletStyle types
  BULLET_STYLE_PLAIN       = 1;
  BULLET_STYLE_FRAGNADE    = 2;
  BULLET_STYLE_SHOTGUN     = 3;
  BULLET_STYLE_M79         = 4;
  BULLET_STYLE_FLAME       = 5;
  BULLET_STYLE_PUNCH       = 6;
  BULLET_STYLE_ARROW       = 7;
  BULLET_STYLE_FLAMEARROW  = 8;
  BULLET_STYLE_CLUSTERNADE = 9;
  BULLET_STYLE_CLUSTER     = 10;
  BULLET_STYLE_KNIFE       = 11;
  BULLET_STYLE_LAW         = 12;
  BULLET_STYLE_THROWNKNIFE = 13;
  BULLET_STYLE_M2          = 14;

  // Used for NoCollision attribute
  WEAPON_NOCOLLISION_ENEMY     = 1 shl 0;
  WEAPON_NOCOLLISION_TEAM      = 1 shl 1;
  WEAPON_NOCOLLISION_SELF      = 1 shl 2;
  WEAPON_NOCOLLISION_EXP_ENEMY = 1 shl 3;
  WEAPON_NOCOLLISION_EXP_TEAM  = 1 shl 4;
  WEAPON_NOCOLLISION_EXP_SELF  = 1 shl 5;

var
  Guns: array[1..TOTAL_WEAPONS] of TGun;
  DefaultGuns: array[1..TOTAL_WEAPONS] of TGun;
  DefaultWMChecksum, LoadedWMChecksum: LongWord;

procedure CreateWeapons(RealisticMode: Boolean);
procedure CreateDefaultWeapons(RealisticMode: Boolean);
procedure CreateWeaponsBase;
procedure CreateNormalWeapons;
procedure CreateRealisticWeapons;
procedure BuildWeapons;
function CreateWMChecksum: LongWord;
function WeaponNumToIndex(Num: Byte): SmallInt;
function WeaponNameToNum(Name: string): Integer;
function WeaponNumToName(Num: Integer): string;
function WeaponNameByNum(Num: Integer): string;
function IsMainWeaponIndex(WeaponIndex: SmallInt): Boolean;
function IsSecondaryWeaponIndex(WeaponIndex: SmallInt): Boolean;
function IsExtendedWeaponIndex(WeaponIndex: SmallInt): Boolean;
function CalculateBink(Accumulated: Word; Bink: Word): Word;
function WeaponNumInternalToExternal(Num: Byte): Byte;
function WeaponNumExternalToInternal(Num: Byte): Byte;

implementation

uses
  TraceLog, Constants, SysUtils;

procedure CreateWeapons(RealisticMode: Boolean);
begin
  CreateWeaponsBase();
  CreateDefaultWeapons(RealisticMode);
end;

procedure CreateDefaultWeapons(RealisticMode: Boolean);
var
  Gun: ^TGun;
  DefaultGun: ^TGun;
  WeaponIndex: Integer;
begin
  if RealisticMode then
    CreateRealisticWeapons()
  else
    CreateNormalWeapons();

  // Set defaults for weapon menu selection comparisons
  for WeaponIndex := 1 to High(Guns) do
  begin
    Gun        := @Guns[WeaponIndex];
    DefaultGun := @DefaultGuns[WeaponIndex];

    DefaultGun.HitMultiply       := Gun.HitMultiply;
    DefaultGun.FireInterval      := Gun.FireInterval;
    DefaultGun.Ammo              := Gun.Ammo;
    DefaultGun.ReloadTime        := Gun.ReloadTime;
    DefaultGun.Speed             := Gun.Speed;
    DefaultGun.BulletStyle       := Gun.BulletStyle;
    DefaultGun.StartUpTime       := Gun.StartUpTime;
    DefaultGun.Bink              := Gun.Bink;
    DefaultGun.MovementAcc       := Gun.MovementAcc;
    DefaultGun.BulletSpread      := Gun.BulletSpread;
    DefaultGun.Recoil            := Gun.Recoil;
    DefaultGun.Push              := Gun.Push;
    DefaultGun.InheritedVelocity := Gun.InheritedVelocity;
    DefaultGun.ModifierLegs      := Gun.ModifierLegs;
    DefaultGun.ModifierChest     := Gun.ModifierChest;
    DefaultGun.ModifierHead      := Gun.ModifierHead;
  end;

  BuildWeapons();
end;

procedure CreateWeaponsBase;
var
  Gun: ^TGun;
begin
  // Desert Eagle
  Gun := @Guns[EAGLE];
  Gun.Name    := 'Desert Eagles';
  Gun.IniName := Gun.Name;
  Gun.Num := EAGLE_NUM;
  Gun.TextureNum := GFX_WEAPONS_DEAGLES;
  Gun.ClipTextureNum := GFX_WEAPONS_DEAGLES_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := GFX_WEAPONS_DEAGLES_BULLET;
  Gun.FireStyle := GFX_WEAPONS_DEAGLES_FIRE;
  Gun.FireMode := 2;

  // MP5
  Gun := @Guns[MP5];
  Gun.Name    := 'HK MP5';
  Gun.IniName := Gun.Name;
  Gun.Num := MP5_NUM;
  Gun.TextureNum := GFX_WEAPONS_MP5;
  Gun.ClipTextureNum := GFX_WEAPONS_MP5_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := GFX_WEAPONS_MP5_BULLET;
  Gun.FireStyle := GFX_WEAPONS_MP5_FIRE;
  Gun.FireMode := 0;

  // AK-74
  Gun := @Guns[AK74];
  Gun.Name    := 'Ak-74';
  Gun.IniName := Gun.Name;
  Gun.Num := AK74_NUM;
  Gun.TextureNum := GFX_WEAPONS_AK74;
  Gun.ClipTextureNum := GFX_WEAPONS_AK74_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := GFX_WEAPONS_AK74_BULLET;
  Gun.FireStyle := GFX_WEAPONS_AK74_FIRE;
  Gun.FireMode := 0;

  // Steyr AUG
  Gun := @Guns[STEYRAUG];
  Gun.Name    := 'Steyr AUG';
  Gun.IniName := Gun.Name;
  Gun.Num := STEYRAUG_NUM;
  Gun.TextureNum := GFX_WEAPONS_STEYR;
  Gun.ClipTextureNum := GFX_WEAPONS_STEYR_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := GFX_WEAPONS_STEYR_BULLET;
  Gun.FireStyle := GFX_WEAPONS_STEYR_FIRE;
  Gun.FireMode := 0;

  // SPAS-12
  Gun := @Guns[SPAS12];
  Gun.Name    := 'Spas-12';
  Gun.IniName := Gun.Name;
  Gun.Num := SPAS12_NUM;
  Gun.TextureNum := GFX_WEAPONS_SPAS;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_SPAS_FIRE;
  Gun.FireMode := 2;

  // Ruger 77
  Gun := @Guns[RUGER77];
  Gun.Name    := 'Ruger 77';
  Gun.IniName := Gun.Name;
  Gun.Num := RUGER77_NUM;
  Gun.TextureNum := GFX_WEAPONS_RUGER;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := GFX_WEAPONS_RUGER_BULLET;
  Gun.FireStyle := GFX_WEAPONS_RUGER_FIRE;
  Gun.FireMode := 2;

  // M79 grenade launcher
  Gun := @Guns[M79];
  Gun.Name    := 'M79';
  Gun.IniName := Gun.Name;
  Gun.Num := M79_NUM;
  Gun.TextureNum := GFX_WEAPONS_M79;
  Gun.ClipTextureNum := GFX_WEAPONS_M79_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_M79_FIRE;
  Gun.FireMode := 0;

  // Barrett M82A1
  Gun := @Guns[BARRETT];
  Gun.Name    := 'Barrett M82A1';
  Gun.IniName := 'Barret M82A1';
  Gun.Num := BARRETT_NUM;
  Gun.TextureNum := GFX_WEAPONS_BARRETT;
  Gun.ClipTextureNum := GFX_WEAPONS_BARRETT_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := GFX_WEAPONS_BARRETT_BULLET;
  Gun.FireStyle := GFX_WEAPONS_BARRETT_FIRE;
  Gun.FireMode := 2;

  // M249
  Gun := @Guns[M249];
  Gun.Name    := 'FN Minimi';
  Gun.IniName := Gun.Name;
  Gun.Num := M249_NUM;
  Gun.TextureNum := GFX_WEAPONS_MINIMI;
  Gun.ClipTextureNum := GFX_WEAPONS_MINIMI_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := GFX_WEAPONS_MINIMI_BULLET;
  Gun.FireStyle := GFX_WEAPONS_MINIMI_FIRE;
  Gun.FireMode := 0;

  // Minigun
  Gun := @Guns[MINIGUN];
  Gun.Name    := 'XM214 Minigun';
  Gun.IniName := Gun.Name;
  Gun.Num := MINIGUN_NUM;
  Gun.TextureNum := GFX_WEAPONS_MINIGUN;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := GFX_WEAPONS_MINIGUN_BULLET;
  Gun.FireStyle := GFX_WEAPONS_MINIGUN_FIRE;
  Gun.FireMode := 0;

  // Colt 1911
  Gun := @Guns[COLT];
  Gun.Name    := 'USSOCOM';
  Gun.IniName := Gun.Name;
  Gun.Num := COLT_NUM;
  Gun.TextureNum := GFX_WEAPONS_SOCOM;
  Gun.ClipTextureNum := GFX_WEAPONS_SOCOM_CLIP;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := GFX_WEAPONS_COLT_BULLET;
  Gun.FireStyle := GFX_WEAPONS_SOCOM_FIRE;
  Gun.FireMode := 2;

  // Knife
  Gun := @Guns[KNIFE];
  Gun.Name    := 'Combat Knife';
  Gun.IniName := Gun.Name;
  Gun.Num := KNIFE_NUM;
  Gun.TextureNum := GFX_WEAPONS_KNIFE;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := 0;
  Gun.FireMode := 0;

  // Chainsaw
  Gun := @Guns[CHAINSAW];
  Gun.Name    := 'Chainsaw';
  Gun.IniName := Gun.Name;
  Gun.Num := CHAINSAW_NUM;
  Gun.TextureNum := GFX_WEAPONS_CHAINSAW;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_CHAINSAW_FIRE;
  Gun.FireMode := 0;

  // M72 LAW
  Gun := @Guns[LAW];
  Gun.Name    := 'LAW';
  Gun.IniName := 'M72 LAW';
  Gun.Num := LAW_NUM;
  Gun.TextureNum := GFX_WEAPONS_LAW;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := True;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_LAW_FIRE;
  Gun.FireMode := 0;

  // Rambo Bow with flame
  Gun := @Guns[BOW2];
  Gun.Name    := 'Flame Bow';
  Gun.IniName := 'Flamed Arrows';
  Gun.Num := BOW2_NUM;
  Gun.TextureNum := GFX_WEAPONS_BOW;
  Gun.ClipTextureNum := GFX_WEAPONS_BOW_S;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_BOW_FIRE;
  Gun.FireMode := 0;

  // Rambo Bow
  Gun := @Guns[BOW];
  Gun.Name    := 'Bow';
  Gun.IniName := 'Rambo Bow';
  Gun.Num := BOW_NUM;
  Gun.TextureNum := GFX_WEAPONS_BOW;
  Gun.ClipTextureNum := GFX_WEAPONS_BOW_S;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_BOW_FIRE;
  Gun.FireMode := 0;

  // Flamethrower
  Gun := @Guns[FLAMER];
  Gun.Name    := 'Flamer';
  Gun.IniName := Gun.Name;
  Gun.Num := FLAMER_NUM;
  Gun.TextureNum := GFX_WEAPONS_FLAMER;
  Gun.ClipTextureNum := GFX_WEAPONS_FLAMER;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_FLAMER_FIRE;
  Gun.FireMode := 0;

  // M2
  Gun := @Guns[M2];
  Gun.Name    := 'M2 MG';
  Gun.IniName := 'Stationary Gun';
  Gun.Num := M2_NUM;
  Gun.TextureNum := GFX_WEAPONS_MINIGUN;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := 0;
  Gun.FireMode := 0;

  // No weapon
  Gun := @Guns[NOWEAPON];
  Gun.Name    := 'Hands';
  Gun.IniName := 'Punch';
  Gun.Num := NOWEAPON_NUM;
  Gun.TextureNum := 0;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := 0;
  Gun.FireMode := 0;

  // Frag grenade
  Gun := @Guns[FRAGGRENADE];
  Gun.Name    := 'Frag Grenade';
  Gun.IniName := 'Grenade';
  Gun.Num := FRAGGRENADE_NUM;
  Gun.TextureNum := GFX_WEAPONS_FRAG_GRENADE;
  Gun.ClipTextureNum := GFX_WEAPONS_FRAG_GRENADE;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_AK74_FIRE;
  Gun.FireMode := 0;

  // TODO(skoskav): Add a proper entry for cluster nade and thrown knife
  // Cluster grenade
  Gun := @Guns[CLUSTERGRENADE];
  Gun.Name    := 'Frag Grenade';
  Gun.IniName := '';
  Gun.Num := CLUSTERGRENADE_NUM;
  Gun.TextureNum := GFX_WEAPONS_FRAG_GRENADE;
  Gun.ClipTextureNum := GFX_WEAPONS_FRAG_GRENADE;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_AK74_FIRE;
  Gun.FireMode := 0;

  // Cluster
  Gun := @Guns[CLUSTER];
  Gun.Name    := 'Frag Grenade';
  Gun.IniName := '';
  Gun.Num := CLUSTER_NUM;
  Gun.TextureNum := GFX_WEAPONS_FRAG_GRENADE;
  Gun.ClipTextureNum := GFX_WEAPONS_FRAG_GRENADE;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := GFX_WEAPONS_AK74_FIRE;
  Gun.FireMode := 0;

  // Thrown knife
  Gun := @Guns[THROWNKNIFE];
  Gun.Name    := 'Combat Knife';
  Gun.IniName := '';
  Gun.Num := THROWNKNIFE_NUM;
  Gun.TextureNum := GFX_WEAPONS_KNIFE;
  Gun.ClipTextureNum := 0;
  Gun.ClipReload := False;
  Gun.BulletImageStyle := 0;
  Gun.FireStyle := 0;
  Gun.FireMode := 0;
end;

procedure CreateNormalWeapons;
var
  Gun: ^TGun;
begin
  // Desert Eagle
  Gun := @Guns[EAGLE];
  Gun.HitMultiply       := 1.81;
  Gun.FireInterval      := 24;
  Gun.Ammo              := 7;
  Gun.ReloadTime        := 87;
  Gun.Speed             := 19;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.009;
  Gun.BulletSpread      := 0.15;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0176;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // MP5
  Gun := @Guns[MP5];
  Gun.HitMultiply       := 1.01;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 30;
  Gun.ReloadTime        := 105;
  Gun.Speed             := 18.9;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0.14;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0112;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // AK-74
  Gun := @Guns[AK74];
  Gun.HitMultiply       := 1.004;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 35;
  Gun.ReloadTime        := 165;
  Gun.Speed             := 24.6;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := -12;
  Gun.MovementAcc       := 0.011;
  Gun.BulletSpread      := 0.025;
  Gun.Recoil            := 0;
  Gun.Push              := 0.01376;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // Steyr AUG
  Gun := @Guns[STEYRAUG];
  Gun.HitMultiply       := 0.71;
  Gun.FireInterval      := 7;
  Gun.Ammo              := 25;
  Gun.ReloadTime        := 125;
  Gun.Speed             := 26;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0.075;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0084;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // SPAS-12
  Gun := @Guns[SPAS12];
  Gun.HitMultiply       := 1.22;
  Gun.FireInterval      := 32;
  Gun.Ammo              := 7;
  Gun.ReloadTime        := 175;
  Gun.Speed             := 14;
  Gun.BulletStyle       := BULLET_STYLE_SHOTGUN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0.8;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0188;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // Ruger 77
  Gun := @Guns[RUGER77];
  Gun.HitMultiply       := 2.49;
  Gun.FireInterval      := 45;
  Gun.Ammo              := 4;
  Gun.ReloadTime        := 78;
  Gun.Speed             := 33;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.03;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.012;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.2;
  Gun.ModifierChest     := 1.05;
  Gun.ModifierLegs      := 1;

  // M79 grenade launcher
  Gun := @Guns[M79];
  Gun.HitMultiply       := 1550;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 178;
  Gun.Speed             := 10.7;
  Gun.BulletStyle       := BULLET_STYLE_M79;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.036;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // Barrett M82A1
  Gun := @Guns[BARRETT];
  Gun.HitMultiply       := 4.45;
  Gun.FireInterval      := 225;
  Gun.Ammo              := 10;
  Gun.ReloadTime        := 70;
  Gun.Speed             := 55;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 19;
  Gun.Bink              := 65;
  Gun.MovementAcc       := 0.05;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.018;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 1;

  // M249
  Gun := @Guns[M249];
  Gun.HitMultiply       := 0.85;
  Gun.FireInterval      := 9;
  Gun.Ammo              := 50;
  Gun.ReloadTime        := 250;
  Gun.Speed             := 27;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.013;
  Gun.BulletSpread      := 0.064;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0128;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // Minigun
  Gun := @Guns[MINIGUN];
  Gun.HitMultiply       := 0.468;
  Gun.FireInterval      := 3;
  Gun.Ammo              := 100;
  Gun.ReloadTime        := 480;
  Gun.Speed             := 29;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 25;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.0625;
  Gun.BulletSpread      := 0.3;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0104;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // Colt 1911
  Gun := @Guns[COLT];
  Gun.HitMultiply       := 1.49;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 14;
  Gun.ReloadTime        := 60;
  Gun.Speed             := 18;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.02;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // Knife
  Gun := @Guns[KNIFE];
  Gun.HitMultiply       := 2150;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 3;
  Gun.Speed             := 6;
  Gun.BulletStyle       := BULLET_STYLE_KNIFE;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.12;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // Chainsaw
  Gun := @Guns[CHAINSAW];
  Gun.HitMultiply       := 50;
  Gun.FireInterval      := 2;
  Gun.Ammo              := 200;
  Gun.ReloadTime        := 110;
  Gun.Speed             := 8;
  Gun.BulletStyle       := BULLET_STYLE_KNIFE;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0028;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // M72 LAW
  Gun := @Guns[LAW];
  Gun.HitMultiply       := 1550;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 300;
  Gun.Speed             := 23;
  Gun.BulletStyle       := BULLET_STYLE_LAW;
  Gun.StartUpTime       := 13;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.028;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // Rambo Bow with flame
  Gun := @Guns[BOW2];
  Gun.HitMultiply       := 8;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 39;
  Gun.Speed             := 18;
  Gun.BulletStyle       := BULLET_STYLE_FLAMEARROW;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // Rambo Bow
  Gun := @Guns[BOW];
  Gun.HitMultiply       := 12;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 25;
  Gun.Speed             := 21;
  Gun.BulletStyle       := BULLET_STYLE_ARROW;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0148;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // Flamethrower
  Gun := @Guns[FLAMER];
  Gun.HitMultiply       := 19;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 200;
  Gun.ReloadTime        := 5;
  Gun.Speed             := 10.5;
  Gun.BulletStyle       := BULLET_STYLE_FLAME;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.016;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // M2
  Gun := @Guns[M2];
  Gun.HitMultiply       := 1.8;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 100;
  Gun.ReloadTime        := 366;
  Gun.Speed             := 36;
  Gun.BulletStyle       := BULLET_STYLE_M2;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0088;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 0.95;
  Gun.ModifierLegs      := 0.85;

  // No weapon
  Gun := @Guns[NOWEAPON];
  Gun.HitMultiply       := 330;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 3;
  Gun.Speed             := 5;
  Gun.BulletStyle       := BULLET_STYLE_PUNCH;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.15;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.9;

  // Frag grenade
  Gun := @Guns[FRAGGRENADE];
  Gun.HitMultiply       := 1500;
  Gun.FireInterval      := 80;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 20;
  Gun.Speed             := 5;
  Gun.BulletStyle       := BULLET_STYLE_FRAGNADE;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0;
  Gun.InheritedVelocity := 1;
  Gun.ModifierHead      := 1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 1;
end;

procedure CreateRealisticWeapons;
var
  Gun: ^TGun;
begin
  // Desert Eagle
  Gun := @Guns[EAGLE];
  Gun.HitMultiply       := 1.66;
  Gun.FireInterval      := 27;
  Gun.Ammo              := 7;
  Gun.ReloadTime        := 106;
  Gun.Speed             := 19;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.02;
  Gun.BulletSpread      := 0.1;
  Gun.Recoil            := 55;
  Gun.Push              := 0.0164;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // MP5
  Gun := @Guns[MP5];
  Gun.HitMultiply       := 0.94;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 30;
  Gun.ReloadTime        := 110;
  Gun.Speed             := 18.9;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := -10;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0.03;
  Gun.Recoil            := 9;
  Gun.Push              := 0.0164;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // AK-74
  Gun := @Guns[AK74];
  Gun.HitMultiply       := 1.08;
  Gun.FireInterval      := 11;
  Gun.Ammo              := 35;
  Gun.ReloadTime        := 158;
  Gun.Speed             := 24;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := -10;
  Gun.MovementAcc       := 0.02;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 13;
  Gun.Push              := 0.0132;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Steyr AUG
  Gun := @Guns[STEYRAUG];
  Gun.HitMultiply       := 0.68;
  Gun.FireInterval      := 7;
  Gun.Ammo              := 30;
  Gun.ReloadTime        := 126;
  Gun.Speed             := 26;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := -9;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 11;
  Gun.Push              := 0.012;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // SPAS-12
  Gun := @Guns[SPAS12];
  Gun.HitMultiply       := 1.2;
  Gun.FireInterval      := 35;
  Gun.Ammo              := 7;
  Gun.ReloadTime        := 175;
  Gun.Speed             := 13.2;
  Gun.BulletStyle       := BULLET_STYLE_SHOTGUN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0.8;
  Gun.Recoil            := 65;
  Gun.Push              := 0.0224;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Ruger 77
  Gun := @Guns[RUGER77];
  Gun.HitMultiply       := 2.22;
  Gun.FireInterval      := 52;
  Gun.Ammo              := 4;
  Gun.ReloadTime        := 104;
  Gun.Speed             := 33;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 14;
  Gun.MovementAcc       := 0.03;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 54;
  Gun.Push              := 0.0096;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // M79 grenade launcher
  Gun := @Guns[M79];
  Gun.HitMultiply       := 1600;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 173;
  Gun.Speed             := 11.4;
  Gun.BulletStyle       := BULLET_STYLE_M79;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 45;
  Gun.MovementAcc       := 0.03;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 420;
  Gun.Push              := 0.024;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Barrett M82A1
  Gun := @Guns[BARRETT];
  Gun.HitMultiply       := 4.95;
  Gun.FireInterval      := 200;
  Gun.Ammo              := 10;
  Gun.ReloadTime        := 170;
  Gun.Speed             := 55;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 16;
  Gun.Bink              := 80;
  Gun.MovementAcc       := 0.07;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 0;
  Gun.Push              := 0.0056;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // M249
  Gun := @Guns[M249];
  Gun.HitMultiply       := 0.81;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 50;
  Gun.ReloadTime        := 261;
  Gun.Speed             := 27;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := -8;
  Gun.MovementAcc       := 0.02;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 8;
  Gun.Push              := 0.0116;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Minigun
  Gun := @Guns[MINIGUN];
  Gun.HitMultiply       := 0.43;
  Gun.FireInterval      := 4;
  Gun.Ammo              := 100;
  Gun.ReloadTime        := 320;
  Gun.Speed             := 29;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 33;
  Gun.Bink              := -2;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0.1;
  Gun.Recoil            := 4;
  Gun.Push              := 0.0108;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Colt 1911
  Gun := @Guns[COLT];
  Gun.HitMultiply       := 1.30;
  Gun.FireInterval      := 12;
  Gun.Ammo              := 12;
  Gun.ReloadTime        := 72;
  Gun.Speed             := 18;
  Gun.BulletStyle       := BULLET_STYLE_PLAIN;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.02;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 28;
  Gun.Push              := 0.0172;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Knife
  Gun := @Guns[KNIFE];
  Gun.HitMultiply       := 2250;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 3;
  Gun.Speed             := 6;
  Gun.BulletStyle       := BULLET_STYLE_KNIFE;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 10;
  Gun.Push              := 0.028;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Chainsaw
  Gun := @Guns[CHAINSAW];
  Gun.HitMultiply       := 21;
  Gun.FireInterval      := 2;
  Gun.Ammo              := 200;
  Gun.ReloadTime        := 110;
  Gun.Speed             := 7.6;
  Gun.BulletStyle       := BULLET_STYLE_KNIFE;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 1;
  Gun.Push              := 0.0028;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // M72 LAW
  Gun := @Guns[LAW];
  Gun.HitMultiply       := 1500;
  Gun.FireInterval      := 30;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 495;
  Gun.Speed             := 23;
  Gun.BulletStyle       := BULLET_STYLE_LAW;
  Gun.StartUpTime       := 12;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 9;
  Gun.Push              := 0.012;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Rambo Bow with flame
  Gun := @Guns[BOW2];
  Gun.HitMultiply       := 8;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 39;
  Gun.Speed             := 18;
  Gun.BulletStyle       := BULLET_STYLE_FLAMEARROW;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 10;
  Gun.Push              := 0;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Rambo Bow
  Gun := @Guns[BOW];
  Gun.HitMultiply       := 12;
  Gun.FireInterval      := 10;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 25;
  Gun.Speed             := 21;
  Gun.BulletStyle       := BULLET_STYLE_ARROW;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 10;
  Gun.Push              := 0.0148;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Flamethrower
  Gun := @Guns[FLAMER];
  Gun.HitMultiply       := 12;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 200;
  Gun.ReloadTime        := 5;
  Gun.Speed             := 12.5;
  Gun.BulletStyle       := BULLET_STYLE_FLAME;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 10;
  Gun.Push              := 0.016;
  Gun.InheritedVelocity := 0.5;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // M2
  Gun := @Guns[M2];
  Gun.HitMultiply       := 1.55;
  Gun.FireInterval      := 14;
  Gun.Ammo              := 100;
  Gun.ReloadTime        := 366;
  Gun.Speed             := 36;
  Gun.BulletStyle       := BULLET_STYLE_M2;
  Gun.StartUpTime       := 21;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 10;
  Gun.Push              := 0.0088;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // No weapon
  Gun := @Guns[NOWEAPON];
  Gun.HitMultiply       := 330;
  Gun.FireInterval      := 6;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 3;
  Gun.Speed             := 5;
  Gun.BulletStyle       := BULLET_STYLE_PUNCH;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 10;
  Gun.Push              := 0;
  Gun.InheritedVelocity := 0;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;

  // Frag grenade
  Gun := @Guns[FRAGGRENADE];
  Gun.HitMultiply       := 1500;
  Gun.FireInterval      := 80;
  Gun.Ammo              := 1;
  Gun.ReloadTime        := 20;
  Gun.Speed             := 5;
  Gun.BulletStyle       := BULLET_STYLE_FRAGNADE;
  Gun.StartUpTime       := 0;
  Gun.Bink              := 0;
  Gun.MovementAcc       := 0.01;
  Gun.BulletSpread      := 0;
  Gun.Recoil            := 10;
  Gun.Push              := 0;
  Gun.InheritedVelocity := 1;
  Gun.ModifierHead      := 1.1;
  Gun.ModifierChest     := 1;
  Gun.ModifierLegs      := 0.6;
end;

procedure BuildWeapons;
var
  Gun: ^TGun;
  WeaponIndex: Integer;
begin
  // TODO(skoskav): Add a proper entry for cluster nade and thrown knife
  // Cluster grenade
  Gun := @Guns[CLUSTERGRENADE];
  Gun.HitMultiply := Guns[FRAGGRENADE].HitMultiply;
  Gun.FireInterval := Guns[FRAGGRENADE].FireInterval;
  Gun.Ammo := Guns[FRAGGRENADE].Ammo;
  Gun.ReloadTime := Guns[FRAGGRENADE].ReloadTime;
  Gun.Speed := Guns[FRAGGRENADE].Speed;
  Gun.BulletStyle := BULLET_STYLE_CLUSTERNADE;
  Gun.StartUpTime := Guns[FRAGGRENADE].StartUpTime;
  Gun.Bink := Guns[FRAGGRENADE].Bink;
  Gun.MovementAcc := Guns[FRAGGRENADE].MovementAcc;
  Gun.BulletSpread := Guns[FRAGGRENADE].BulletSpread;
  Gun.Recoil := Guns[FRAGGRENADE].Recoil;
  Gun.Push := Guns[FRAGGRENADE].Push;
  Gun.InheritedVelocity := Guns[FRAGGRENADE].InheritedVelocity;

  // Cluster
  Gun := @Guns[CLUSTER];
  Gun.HitMultiply := Guns[CLUSTERGRENADE].HitMultiply;
  Gun.FireInterval := Guns[CLUSTERGRENADE].FireInterval;
  Gun.Ammo := Guns[CLUSTERGRENADE].Ammo;
  Gun.ReloadTime := Guns[CLUSTERGRENADE].ReloadTime;
  Gun.Speed := Guns[CLUSTERGRENADE].Speed;
  Gun.BulletStyle := BULLET_STYLE_CLUSTER;
  Gun.StartUpTime := Guns[CLUSTERGRENADE].StartUpTime;
  Gun.Bink := Guns[CLUSTERGRENADE].Bink;
  Gun.MovementAcc := Guns[CLUSTERGRENADE].MovementAcc;
  Gun.BulletSpread := Guns[CLUSTERGRENADE].BulletSpread;
  Gun.Recoil := Guns[CLUSTERGRENADE].Recoil;
  Gun.Push := Guns[CLUSTERGRENADE].Push;
  Gun.InheritedVelocity := Guns[CLUSTERGRENADE].InheritedVelocity;

  // Thrown knife
  Gun := @Guns[THROWNKNIFE];
  Gun.HitMultiply := Guns[KNIFE].HitMultiply;
  Gun.FireInterval := Guns[KNIFE].FireInterval;
  Gun.Ammo := Guns[KNIFE].Ammo;
  Gun.ReloadTime := Guns[KNIFE].ReloadTime;
  Gun.Speed := Guns[KNIFE].Speed;
  Gun.BulletStyle := BULLET_STYLE_THROWNKNIFE;
  Gun.StartUpTime := Guns[KNIFE].StartUpTime;
  Gun.Bink := Guns[KNIFE].Bink;
  Gun.MovementAcc := Guns[KNIFE].MovementAcc;
  Gun.BulletSpread := Guns[KNIFE].BulletSpread;
  Gun.Recoil := Guns[KNIFE].Recoil;
  Gun.Push := Guns[KNIFE].Push;
  Gun.InheritedVelocity := Guns[KNIFE].InheritedVelocity;

  for WeaponIndex := 1 to High(Guns) do
  begin
    Gun := @Guns[WeaponIndex];

    Gun.FireIntervalPrev  := Gun.FireInterval;
    Gun.FireIntervalCount := Gun.FireInterval;
    Gun.AmmoCount         := Gun.Ammo;
    Gun.ReloadTimePrev    := Gun.ReloadTime;
    Gun.ReloadTimeCount   := Gun.ReloadTime;
    Gun.StartUpTimeCount  := Gun.StartUpTime;

    // Set timings for when to let out and in a magazine, if at all
    if Gun.ClipReload then
    begin
      Gun.ClipOutTime := Trunc(Gun.ReloadTime * 0.8);
      Gun.ClipInTime  := Trunc(Gun.ReloadTime * 0.3);
    end else
    begin
      Gun.ClipOutTime := 0;
      Gun.ClipInTime  := 0;
    end;

    // Set bullet lifetime
    case Gun.BulletStyle of
      BULLET_STYLE_FRAGNADE, BULLET_STYLE_CLUSTERNADE:
        Gun.Timeout := GRENADE_TIMEOUT;
      BULLET_STYLE_FLAME:
        Gun.Timeout := FLAMER_TIMEOUT;
      BULLET_STYLE_PUNCH, BULLET_STYLE_KNIFE:
        Gun.Timeout := MELEE_TIMEOUT;
      BULLET_STYLE_M2:
        Gun.Timeout := M2BULLET_TIMEOUT;
    else
      Gun.Timeout := BULLET_TIMEOUT;
    end;
  end;

  // Force M79 reload on spawn
  Guns[M79].AmmoCount := 0;
end;

{$IFOPT Q+}{$Q-}{$DEFINE NoOverflowCheck}{$ENDIF}
{$IFOPT R+}{$R-}{$DEFINE NoRangeCheck}{$ENDIF}
function CreateWMChecksum: LongWord;
var
  Hash: LongWord;
  WeaponIndex: Integer;
  Gun: TGun;
begin
  Trace('CreateWMChecksum');

  // djb2 hashing algorithm
  Hash := 5381;

  for WeaponIndex := 1 to ORIGINAL_WEAPONS do
  begin
    Gun := Guns[WeaponIndex];

    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.HitMultiply));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.FireInterval));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.Ammo));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.ReloadTime));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.Speed));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.BulletStyle));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.StartUpTime));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.Bink));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.MovementAcc));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.BulletSpread));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.Recoil));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.Push));
    Inc(Hash, Hash shl 5 + Round(1000.0 * Gun.InheritedVelocity));
  end;

  Result := Hash;
end;
{$IFDEF NoRangeCheck}{$R+}{$UNDEF NoRangeCheck}{$ENDIF}
{$IFDEF NoOverflowCheck}{$Q+}{$UNDEF NoOverflowCheck}{$ENDIF}

function WeaponNumToIndex(Num: Byte): SmallInt;
var
  WeaponIndex: Byte;
begin
  for WeaponIndex := 1 to High(Guns) do
  begin
    if Num = Guns[WeaponIndex].Num then
    begin
      Result := WeaponIndex;
      Exit;
    end;
  end;
  Result := -1;
end;

function WeaponNameToNum(Name: string): Integer;
var
  i: Byte;
begin
  for i := 1 to High(Guns) do
  begin
    if Name = Guns[i].Name then
    begin
      Result := Guns[i].Num;
      Exit;
    end;
  end;
  Result := -1;
end;

function WeaponNumToName(Num: Integer): string;
begin
  case Num of
    EAGLE_NUM          : Result := Guns[EAGLE].Name;
    MP5_NUM            : Result := Guns[MP5].Name;
    AK74_NUM           : Result := Guns[AK74].Name;
    STEYRAUG_NUM       : Result := Guns[STEYRAUG].Name;
    SPAS12_NUM         : Result := Guns[SPAS12].Name;
    RUGER77_NUM        : Result := Guns[RUGER77].Name;
    M79_NUM            : Result := Guns[M79].Name;
    BARRETT_NUM        : Result := Guns[BARRETT].Name;
    M249_NUM           : Result := Guns[M249].Name;
    MINIGUN_NUM        : Result := Guns[MINIGUN].Name;
    COLT_NUM           : Result := Guns[COLT].Name;
    KNIFE_NUM          : Result := Guns[KNIFE].Name;
    CHAINSAW_NUM       : Result := Guns[CHAINSAW].Name;
    LAW_NUM            : Result := Guns[LAW].Name;
    BOW2_NUM           : Result := Guns[BOW2].Name;
    BOW_NUM            : Result := Guns[BOW].Name;
    FLAMER_NUM         : Result := Guns[FLAMER].Name;
    M2_NUM             : Result := Guns[M2].Name;
    NOWEAPON_NUM       : Result := Guns[NOWEAPON].Name;
    FRAGGRENADE_NUM    : Result := Guns[FRAGGRENADE].Name;
    CLUSTERGRENADE_NUM : Result := Guns[CLUSTERGRENADE].Name;
    CLUSTER_NUM        : Result := Guns[CLUSTER].Name;
    THROWNKNIFE_NUM    : Result := Guns[THROWNKNIFE].Name;
  else
    Result := '';
  end;
end;

function WeaponNumInternalToExternal(Num: Byte): Byte;
begin
  case Num of
    KNIFE_NUM:    Result := 14;
    CHAINSAW_NUM: Result := 15;
    LAW_NUM:      Result := 16;
    FLAMER_NUM:   Result := 11;
    BOW_NUM:      Result := 12;
    BOW2_NUM:     Result := 13;
    else          Result := Num;
  end;
end;

function WeaponNumExternalToInternal(Num: Byte): Byte;
begin
  case Num of
    11:  Result := FLAMER_NUM;
    12:  Result := BOW_NUM;
    13:  Result := BOW2_NUM;
    14:  Result := KNIFE_NUM;
    15:  Result := CHAINSAW_NUM;
    16:  Result := LAW_NUM;
    else Result := Num;
  end;
end;

function WeaponNameByNum(Num: Integer): string;
var
  WeaponIndex: Integer;
begin
  Result := '';

  for WeaponIndex := Low(Guns) to High(Guns) do
  begin
    if Num = Guns[WeaponIndex].Num then
    begin
      Result := Guns[WeaponIndex].Name;
      Break;
    end;
  end;
end;

function IsMainWeaponIndex(WeaponIndex: SmallInt): Boolean;
begin
  Result := (WeaponIndex >= 1) and (WeaponIndex <= MAIN_WEAPONS);
end;

function IsSecondaryWeaponIndex(WeaponIndex: SmallInt): Boolean;
begin
  Result := (WeaponIndex >= PRIMARY_WEAPONS + 1) and (WeaponIndex <= MAIN_WEAPONS);
end;

function IsExtendedWeaponIndex(WeaponIndex: SmallInt): Boolean;
begin
  Result := (WeaponIndex >= 1) and (WeaponIndex <= EXTENDED_WEAPONS);
end;

function CalculateBink(Accumulated: Word; Bink: Word): Word;
begin
  // Adding bink has diminishing returns as more gets accumulated
  Result := Accumulated + Bink - Round(Accumulated * (Accumulated / ((10 * Bink) + Accumulated)));
end;

end.

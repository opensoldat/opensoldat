{*******************************************************}
{                                                       }
{       ScriptBullet unit for OPENSOLDAT                }
{                                                       }
{       Copyright (c) 2014 Tomasz Kolosowski            }
{                          and  Umut Karakas            }
{                                                       }
{*******************************************************}
unit ScriptBullet;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  ScriptCore3Api,
  Bullets,
  SysUtils,
  Server,
  Game;

type

  PBullet = ^TBullet;

  TScriptActiveBullet = class(TObject)
  protected
    FBul: PBullet;
    FID: Byte;
    function GetActive: Boolean;
    function GetID: Byte;
    function GetStyle: Byte;
    function GetX: Single;
    function GetY: Single;
    function GetVelX: Single;
    function GetVelY: Single;
    function GetOwner: Byte;
  public
    constructor CreateActive(ID: Byte; var Bul: TBullet);
    function GetOwnerWeaponId: Integer;
    property Active: Boolean read GetActive;
    property ID: Byte read GetID;
    property Style: Byte read GetStyle;
    property X: Single read GetX;
    property Y: Single read GetY;
    property VelX: Single read GetVelX;
    property VelY: Single read GetVelY;
    property Owner: Byte read GetOwner;
  end;

  TScriptBulletAPI = class(TScriptCore3API)
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

uses
  Parts;

function TScriptActiveBullet.GetOwnerWeaponId: Integer;
begin
  Result := Bullet[Self.FID].OwnerWeapon;
end;

function TScriptActiveBullet.GetActive: Boolean;
begin
  Result := Self.FBul^.Active;
end;

function TScriptActiveBullet.GetID: Byte;
begin
  Result := Self.FID;
end;

function TScriptActiveBullet.GetStyle: Byte;
begin
  Result := Self.FBul^.Style;
end;

function TScriptActiveBullet.GetX: Single;
begin
  Result := BulletParts.Pos[Self.FID].X;
end;

function TScriptActiveBullet.GetY: Single;
begin
  Result := BulletParts.Pos[Self.FID].Y;
end;

function TScriptActiveBullet.GetVelX: Single;
begin
  Result := BulletParts.Velocity[Self.FID].X;
end;

function TScriptActiveBullet.GetVelY: Single;
begin
  Result := BulletParts.Velocity[Self.FID].Y;
end;

function TScriptActiveBullet.GetOwner: Byte;
begin
  Result := Self.FBul^.Owner;
end;

constructor TScriptActiveBullet.CreateActive(ID: Byte; var Bul: TBullet);
begin
  Self.FID := ID;
  Self.FBul := @Bul;
end;


procedure IDReadHelper(Self: TScriptActiveBullet; var Result: Byte);
begin
  Result := Self.ID;
end;

procedure ActiveReadHelper(Self: TScriptActiveBullet; var Result: Boolean);
begin
  Result := Self.Active;
end;

procedure StyleReadHelper(Self: TScriptActiveBullet; var Result: Byte);
begin
  Result := Self.Style;
end;

procedure XReadHelper(Self: TScriptActiveBullet; var Result: Single);
begin
  Result := Self.X;
end;

procedure YReadHelper(Self: TScriptActiveBullet; var Result: Single);
begin
  Result := Self.Y;
end;

procedure VelXReadHelper(Self: TScriptActiveBullet; var Result: Single);
begin
  Result := Self.VelX;
end;

procedure VelYReadHelper(Self: TScriptActiveBullet; var Result: Single);
begin
  Result := Self.VelY;
end;

procedure OwnerReadHelper(Self: TScriptActiveBullet; var Result: Byte);
begin
  Result := Self.Owner;
end;


procedure TScriptBulletAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  ActiveMapBullet: TPascalCompiletimeClass;
begin
  ActiveMapBullet := Compiler.AddClass(nil, 'TActiveMapBullet');
  with ActiveMapBullet do
  begin
    RegisterMethod('function GetOwnerWeaponId: Integer;');
    RegisterProperty('ID', 'Byte', iptR);
    RegisterProperty('Active', 'Boolean', iptR);
    RegisterProperty('Style', 'Byte', iptR);
    RegisterProperty('X', 'Single', iptR);
    RegisterProperty('Y', 'Single', iptR);
    RegisterProperty('VelX', 'Single', iptR);
    RegisterProperty('VelY', 'Single', iptR);
    RegisterProperty('Owner', 'Byte', iptR);
  end;
end;

procedure TScriptBulletAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptActiveBullet, 'TActiveMapBullet') do
  begin
    RegisterMethod(@TScriptActiveBullet.GetOwnerWeaponId, 'GetOwnerWeaponId');
    RegisterPropertyHelper(@IDReadHelper, nil, 'ID');
    RegisterPropertyHelper(@ActiveReadHelper, nil, 'Active');
    RegisterPropertyHelper(@StyleReadHelper, nil, 'Style');
    RegisterPropertyHelper(@XReadHelper, nil, 'X');
    RegisterPropertyHelper(@YReadHelper, nil, 'Y');
    RegisterPropertyHelper(@VelXReadHelper, nil, 'VelX');
    RegisterPropertyHelper(@VelYReadHelper, nil, 'VelY');
    RegisterPropertyHelper(@OwnerReadHelper, nil, 'Owner');
  end;
end;

end.

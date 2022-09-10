unit ScriptSpawnPoint;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  MapFile,
  ScriptCore3Api,
  SysUtils;

type

  TScriptSpawnPoint = class(TObject)
  protected
    FSpawnPoint: PMapSpawnPoint;
    function GetActive: Boolean;
    procedure SetActive(Active: Boolean);
    function GetX: Longint;
    procedure SetX(X: Longint);
    function GetY: Longint;
    procedure SetY(Y: Longint);
    function GetStyle: Byte;
    procedure SetStyle(Style: Byte);
  public
    property Active: Boolean read GetActive write SetActive;
    property X: Longint read GetX write SetX;
    property Y: Longint read GetY write SetY;
    property Style: Byte read GetStyle write SetStyle;
  end;

  TScriptNewSpawnPoint = class(TScriptSpawnPoint)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TScriptActiveSpawnPoint = class(TScriptSpawnPoint)
  private
    FID: Byte;
  public
    constructor CreateActive(ID: Byte; var Spawn: TMapSpawnPoint);
    property ID: Byte read FID;
  end;

  TScriptSpawnPointAPI = class(TScriptCore3API)
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;


implementation

function TScriptSpawnPoint.GetActive: Boolean;
begin
  Result := Self.FSpawnPoint^.Active;
end;

procedure TScriptSpawnPoint.SetActive(Active: Boolean);
begin
  Self.FSpawnPoint^.Active := Active;
end;

function TScriptSpawnPoint.GetX: Longint;
begin
  Result := Self.FSpawnPoint^.X;
end;

procedure TScriptSpawnPoint.SetX(X: Longint);
begin
  Self.FSpawnPoint^.X := X;
end;

function TScriptSpawnPoint.GetY: Longint;
begin
  Result := Self.FSpawnPoint^.Y;
end;

procedure TScriptSpawnPoint.SetY(Y: Longint);
begin
  Self.FSpawnPoint^.Y := Y;
end;

function TScriptSpawnPoint.GetStyle: Byte;
begin
  Result := Self.FSpawnPoint^.Team;
end;

procedure TScriptSpawnPoint.SetStyle(Style: Byte);
begin
  Self.FSpawnPoint^.Team := Style;
end;

constructor TScriptNewSpawnPoint.Create;
begin
  New(Self.FSpawnPoint);
  FillChar(Self.FSpawnPoint^, SizeOf(TMapSpawnPoint), #0);
end;

destructor TScriptNewSpawnPoint.Destroy;
begin
  Dispose(Self.FSpawnPoint);
end;

constructor TScriptActiveSpawnPoint.CreateActive(ID: Byte; var Spawn: TMapSpawnPoint);
begin
  Self.FID := ID;
  Self.FSpawnPoint := @Spawn;
end;

procedure ActiveReadHelper(Self: TScriptSpawnPoint; var Result: Boolean);
begin
  Result := Self.Active;
end;

procedure ActiveWriteHelper(Self: TScriptSpawnPoint; const Result: Boolean);
begin
  Self.Active := Result;
end;

procedure XReadHelper(Self: TScriptSpawnPoint; var Result: Longint);
begin
  Result := Self.X;
end;

procedure XWriteHelper(Self: TScriptSpawnPoint; const Result: Longint);
begin
  Self.X := Result;
end;

procedure YReadHelper(Self: TScriptSpawnPoint; var Result: Longint);
begin
  Result := Self.Y;
end;

procedure YWriteHelper(Self: TScriptSpawnPoint; const Result: Longint);
begin
  Self.Y := Result;
end;

procedure StyleReadHelper(Self: TScriptSpawnPoint; var Result: Byte);
begin
  Result := Self.Style;
end;

procedure IDReadHelper(Self: TScriptActiveSpawnPoint; var Result: Byte);
begin
  Result := Self.ID;
end;

procedure StyleWriteHelper(Self: TScriptSpawnPoint; const Result: Byte);
begin
  Self.Style := Result;
end;

procedure TScriptSpawnPointAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  SpawnPoint: TPascalCompiletimeClass;
begin
  SpawnPoint := Compiler.AddClass(nil, 'TSpawnPoint');
  with SpawnPoint do
  begin
    RegisterProperty('Active', 'Boolean', iptRW);
    RegisterProperty('X', 'LongInt', iptRW);
    RegisterProperty('Y', 'LongInt', iptRW);
    RegisterProperty('Style', 'Byte', iptRW);
  end;

  with Compiler.AddClass(SpawnPoint, 'TNewSpawnPoint') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Free');
  end;

  with Compiler.AddClass(SpawnPoint, 'TActiveSpawnPoint') do
  begin
    RegisterProperty('ID', 'Byte', iptR);
  end;
end;

procedure TScriptSpawnPointAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptSpawnPoint, 'TSpawnPoint') do
  begin
    RegisterPropertyHelper(@ActiveReadHelper, @ActiveWriteHelper, 'Active');
    RegisterPropertyHelper(@XReadHelper, @XWriteHelper, 'X');
    RegisterPropertyHelper(@YReadHelper, @YWriteHelper, 'Y');
    RegisterPropertyHelper(@StyleReadHelper, @StyleWriteHelper, 'Style');
  end;

  with Exec.AddClass(TScriptNewSpawnPoint, 'TNewSpawnPoint') do
  begin
    RegisterConstructor(@TScriptNewSpawnPoint.Create, 'Create');
    RegisterMethod(@TScriptNewSpawnPoint.Free, 'Free');
  end;

  with Exec.AddClass(TScriptActiveSpawnPoint, 'TActiveSpawnPoint') do
  begin
    RegisterPropertyHelper(@IDReadHelper, nil, 'ID');
  end;
end;


end.

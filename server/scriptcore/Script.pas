{*******************************************************}
{                                                       }
{       Abstract script unit for OPENSOLDAT             }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

unit Script;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes, syncobjs, SysUtils, Vector;

type
  // Abstract script class.
  // Extended by each concrete script and the script dispatcher itself.
  // Defines all the functions and variables required for script to function
  // and all the events.
  //
  // The reason to define all events here (an not as abstarct) is so that
  // concrete instances doesn't have to implement them.
  // They can choose whatever they want to intercept nad process.
  TScript = class
  protected
    // name of the script
    FName: string;
    // directory the script reside in
    FDir: string;
    // script's main lock
    FLock: TSynchroObject;
  public

    constructor Create;
    destructor Destroy; override;
    // In prepare function scripts should set-up all the things
    // that need to be done before launching (for instance compile/load bytecode)
    function Prepare: Boolean; virtual; abstract;

    // Launch function.
    // Concrete implementation will most likely use it to compile itself
    procedure Launch; virtual; abstract;

    // Used by CrossFunc
    //
    // @param Params array of parameters for the foreign script's function
    // @param FuncName name of foreign script and function in format:
    //        "ScriptName.FunctionName"
    // @param DefaultReturn default return value in case script or
    //        function is not found
    // @return Value returned by foreign function or DefaultReturn if not found
    function CallFunc(const Params: array of Variant; FuncName: string;
      DefaultReturn: Variant): Variant; virtual; abstract;

    // EVENTS
    procedure OnActivateServer; virtual;
    procedure OnClockTick; virtual;
    procedure OnIdle; virtual;
    // procedure OnScriptShutdown(ServerShutdown: Boolean); virtual;

    function OnRequestGame(Ip, Hw: string; Port: Word; State: Byte;
      Forwarded: Boolean; Password: string): Integer; virtual;
    function OnBeforeJoinTeam(Id, Team, OldTeam: Byte): ShortInt; virtual;
    procedure OnJoinTeam(Id, Team, OldTeam: Byte; JoinGame: Boolean); virtual;
    procedure OnLeaveGame(Id: Byte; Kicked: Boolean); virtual;

    procedure OnBeforeMapChange(Map: string); virtual;
    procedure OnAfterMapChange(Map: string); virtual;

    procedure OnAdminConnect(Ip: string; Port: Word); virtual;
    procedure OnAdminDisconnect(Ip: string; Port: Word); virtual;
    procedure OnAdminMessage(Ip: string; Port: Word; Message: string); virtual;

    procedure OnFlagGrab(Id, TeamFlag: Byte; GrabbedInBase: Boolean); virtual;
    procedure OnFlagScore(Id, TeamFlag: Byte); virtual;
    procedure OnFlagReturn(Id, TeamFlag: Byte); virtual;
    procedure OnFlagDrop(Id, TeamFlag: Byte; Thrown: Boolean); virtual;

    procedure OnKitPickup(Id, KitId: Byte); virtual;

    function OnBeforePlayerRespawn(Id: Byte): TVector2; virtual;
    procedure OnAfterPlayerRespawn(Id: Byte); virtual;
    function OnPlayerDamage(Victim, Shooter: Byte; Damage: Single;
      BulletID: Byte): Single; virtual;
    procedure OnPlayerKill(Killer, Victim, BulletID: Byte); virtual;
    procedure OnWeaponChange(Id, Primary, Secondary,
      PrimaryAmmo, SecondaryAmmo: Byte); virtual;

    function OnVoteMapStart(Id: Byte; Map: string): Boolean; virtual;
    function OnVoteKickStart(Id, Victim: Byte; Reason: string): Boolean; virtual;
    procedure OnVoteMap(Id: Byte; Map: string); virtual;
    procedure OnVoteKick(Id, Victim: Byte); virtual;

    procedure OnPlayerSpeak(Id: Byte; Text: string); virtual;
    function OnPlayerCommand(Id: Byte; Command: string): Boolean; virtual;
    function OnConsoleCommand(Ip: string; Port: Word; Command: string): Boolean;
      virtual;

    property Name: string read FName;
    property Dir: string read FDir;
    property Lock: TSynchroObject read FLock;
  end;

implementation

uses
  Game;

constructor TScript.Create;
begin
  inherited;
  Self.FLock := TCriticalSection.Create;
end;

destructor TScript.Destroy;
begin
  inherited;
  Self.FLock.Free;
end;

procedure TScript.OnActivateServer;
begin
end;

procedure TScript.OnClockTick;
begin
end;

procedure TScript.OnIdle;
begin
end;
{$PUSH}
{$HINTS OFF}
function TScript.OnRequestGame(Ip, Hw: string; Port: Word; State: Byte;
  Forwarded: Boolean; Password: string): Integer;
begin
  Result := State;
end;

function TScript.OnBeforeJoinTeam(Id, Team, OldTeam: Byte): ShortInt;
begin
  Result := Team;
end;

procedure TScript.OnJoinTeam(Id, Team, OldTeam: Byte; JoinGame: Boolean);
begin
end;

procedure TScript.OnLeaveGame(Id: Byte; Kicked: Boolean);
begin
end;


procedure TScript.OnBeforeMapChange(Map: string);
begin
end;

procedure TScript.OnAfterMapChange(Map: string);
begin
end;


procedure TScript.OnAdminConnect(Ip: string; Port: Word);
begin
end;

procedure TScript.OnAdminDisconnect(Ip: string; Port: Word);
begin
end;

procedure TScript.OnAdminMessage(Ip: string; Port: Word; Message: string);
begin
end;


procedure TScript.OnFlagGrab(Id, TeamFlag: Byte; GrabbedInBase: Boolean);
begin
end;

procedure TScript.OnFlagScore(Id, TeamFlag: Byte);
begin
end;

procedure TScript.OnFlagReturn(Id, TeamFlag: Byte);
begin
end;

procedure TScript.OnFlagDrop(Id, TeamFlag: Byte; Thrown: Boolean);
begin
end;


procedure TScript.OnKitPickup(Id, KitId: Byte);
begin
end;


function TScript.OnBeforePlayerRespawn(Id: Byte): TVector2;
begin
  result := SpriteParts.Pos[Id];
end;

procedure TScript.OnAfterPlayerRespawn(Id: Byte);
begin
end;

function TScript.OnPlayerDamage(Victim, Shooter: Byte; Damage: Single;
  BulletID: Byte): Single;
begin
  Result := Damage;
end;

procedure TScript.OnPlayerKill(Killer, Victim, BulletID: Byte);
begin
end;

procedure TScript.OnWeaponChange(Id, Primary, Secondary,
  PrimaryAmmo, SecondaryAmmo: Byte);
begin
end;


function TScript.OnVoteMapStart(Id: Byte; Map: string): Boolean;
begin
  Result := False;
end;

function TScript.OnVoteKickStart(Id, Victim: Byte; Reason: string): Boolean;
begin
  Result := False;
end;

procedure TScript.OnVoteMap(Id: Byte; Map: string);
begin
end;

procedure TScript.OnVoteKick(Id, Victim: Byte);
begin
end;


procedure TScript.OnPlayerSpeak(Id: Byte; Text: string);
begin
end;

function TScript.OnPlayerCommand(Id: Byte; Command: string): Boolean;
begin
  Result := False;
end;

function TScript.OnConsoleCommand(Ip: string; Port: Word; Command: string): Boolean;
begin
  Result := False;
end;
{$POP}
end.



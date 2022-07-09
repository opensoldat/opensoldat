{******************************************************************}
{                                                                  }
{             Script dispatcher unit for OPENSOLDAT                }
{                                                                  }
{             Copyright (c) 2012 Tomasz Kolosowski                 }
{                                                                  }
{ This is an entry scriptcore unit.                                }
{ No other script unit should be imported in the rest of the code. }
{ I've tried to implement it using slots & signals pattern         }
{ but I've failed miserabley while trying to port it to pascal.    }
{ Implemnted as observer instead, resulting in a big code          }
{ redundancy. The good news though is that most of the errors will }
{ be most likely caught during compilation time.                   }
{                                                                  }
{******************************************************************}

unit ScriptDispatcher;

{$IFDEF FPC}{$mode delphi}{$M+}{$ENDIF}

interface

uses
  Classes,
  Script,
  Vector,
  SysUtils;

type
  TCheckFunction = function(Dir: string): TScript;

  // Script dispatcher class, singletone.
  TScriptDispatcher = class(TScript)
  private
    // list of registered scripts
    FScripts: TList;
    // list of scripts to unregister at the end of current pass
    FScriptsToUnregister: TList;
    // list of functions used to check script folders if they're files of
    // given scriptcore version
    // they're created in concrete script units and registered here in
    // initialization section.
    // I've tried to register them in each script's unit initialization
    // section but these are called too early
    // (ScriptDispatcher is not created yet)
    FCheckFunctions: TList;
    // used by /info command, published in porperty below to make sure it's
    //   read only
    function GetScriptNames: TStringList;

    // private constructor
    {$push}{$warn 3018 off} // Hide "Constructor should be public"
    constructor Instantiate;
    {$pop}
    procedure ProcessUnregisterQueue;
    procedure DoUnregisterScript(Script: TScript);
    procedure DoLock;
    procedure DoUnlock;
  public
    // for backward compatibility
    SafeMode: Boolean;
    // directory where scripts reside in
    // COMMENT: perhaps that should be a const?
    Dir: string;
    // used by /info command to display runnig scripts
    property ScriptList: TStringList read GetScriptNames;

    // overwrite constructor with a function that will always return same
    // instance.
    // that's the best attempt to make class singletone i found
    class procedure Create;

    // a shorthand for concrete scripts and dispathcer itself to display info
    // in main console in a unified format
    //
    // @param Text text to write
    procedure WriteInfo(Text: string);

    // Searches scripts folder and assigns each subfolder to a concrete
    // script instance trough registered check function
    //
    // @param Name If specifed, only one subfolder is searched
    //       (used by /recompile <name>)
    procedure FindScripts(Name: string = '');

    // Scans the scripts folder and calls prepare function in all registered
    // script instances.
    function Prepare: Boolean; override;

    // Triggers Launch function in all concrete script instances.
    // This will (most likely) make them compile and trigger
    // ActivateServer function
    procedure Launch; overload; override;

    // Triggers Launch function only in one script instance.
    // Used by /recompile <name>
    //
    // @param Name name of the script.
    // Note that it doesn't have to be name of the folder that script reside in,
    // as scripts set their names themselve, using their own algorithms.
    procedure Launch(Name: string); reintroduce; overload;

    // Destructor. Nothing special, just cleans the stuff.
    destructor Destroy; override;

    // Used by concrete script instances to register their check functions.
    // called in this unit's initialization section. For explanation why see
    // FCheckFunctions description
    //
    // @param Func check function to register
    procedure RegisterCheckFunction(Func: TCheckFunction);

    // Used by concrete script instances to unregister themselve from dispatcher.
    // It might be used if script failed to compile or malfunctioned in runtime.
    //
    // @param Script script instance to unregister
    procedure UnregisterScript(Script: TScript);
    // EVENTS

    // used by CrossFunc function
    //
    // @param Params array of parameters for the foreign script's function
    // @param FuncName name of foreign script and function in format:
    //        "ScriptName.FunctionName"
    // @param DefaultReturn default return value in case script or function is not found
    // @return Value returned by foreign function or DefaultReturn if not found
    function CallFunc(const Params: array of Variant; FuncName: string;
      DefaultReturn: Variant): Variant; override;
    // rest of the functions are scriptcore events. Not gonna bother to document it.

    procedure OnActivateServer; override;
    procedure OnClockTick; override;
    // procedure OnScriptShutdown(ServerShutdown: Boolean);

    function OnRequestGame(Ip, Hw: string; Port: Word; State: Byte;
      Forwarded: Boolean; Password: string): Integer; override;
    function OnBeforeJoinTeam(Id, Team, OldTeam: Byte): ShortInt; override;
    procedure OnJoinTeam(Id, Team, OldTeam: Byte; JoinGame: Boolean); override;
    procedure OnLeaveGame(Id: Byte; Kicked: Boolean); override;

    procedure OnBeforeMapChange(Map: string); override;
    procedure OnAfterMapChange(Map: string); override;

    procedure OnAdminConnect(Ip: string; Port: Word); override;
    procedure OnAdminDisconnect(Ip: string; Port: Word); override;
    procedure OnAdminMessage(Ip: string; Port: Word; Msg: string); override;

    procedure OnFlagGrab(Id, TeamFlag: Byte; GrabbedInBase: Boolean); override;
    procedure OnFlagScore(Id, TeamFlag: Byte); override;
    procedure OnFlagReturn(Id, TeamFlag: Byte); override;
    procedure OnFlagDrop(Id, TeamFlag: Byte; Thrown: Boolean); override;

    procedure OnKitPickup(Id, KitId: Byte); override;

    function OnBeforePlayerRespawn(Id: Byte): TVector2; override;
    procedure OnAfterPlayerRespawn(Id: Byte); override;
    function OnPlayerDamage(Victim, Shooter: Byte; Damage: Single;
      Weapon: Byte): Single; override;
    procedure OnPlayerKill(Killer, Victim, BulletID: Byte); override;
    procedure OnWeaponChange(Id, Primary, Secondary,
      PrimaryAmmo, SecondaryAmmo: Byte); override;

    function OnVoteMapStart(Id: Byte; Map: string): Boolean; override;
    function OnVoteKickStart(Id, Victim: Byte; Reason: string): Boolean; override;
    procedure OnVoteMap(Id: Byte; Map: string); override;
    procedure OnVoteKick(Id, Victim: Byte); override;

    procedure OnPlayerSpeak(Id: Byte; Text: string); override;
    function OnPlayerCommand(Id: Byte; Command: string): Boolean; override;
    function OnConsoleCommand(Ip: string; Port: Word; Command: string): Boolean;
      override;
  end;

var
  ScrptDispatcher: TScriptDispatcher;

implementation

uses
  Constants,
  ScriptCore,
  ScriptCore3,
  StrUtils,
  Game,
  Server;

constructor TScriptDispatcher.Instantiate;
begin
  inherited Create;
  Self.FCheckFunctions := TList.Create;
  Self.FScripts := TList.Create;
  Self.FScriptsToUnregister := TList.Create;
  SafeMode := True;
  Dir := 'scripts';
end;

procedure TScriptDispatcher.DoUnregisterScript(Script: TScript);
begin
  if Self.FScripts.Remove(Script) <> -1 then
    Self.WriteInfo('Disabling ' + Script.Name);
  if Assigned(Script) then
    Script.Destroy;
end;

procedure TScriptDispatcher.ProcessUnregisterQueue;
var
  i: Integer;
begin;
  for i := 0 to Self.FScriptsToUnregister.Count - 1 do
    DoUnregisterScript(Self.FScriptsToUnregister[i]);
  Self.FScriptsToUnregister.Clear;
end;

procedure TScriptDispatcher.DoLock;
begin
  if GetThreadID <> MainThreadID then
    Self.Lock.Acquire;
end;

procedure TScriptDispatcher.DoUnlock;
begin
  if GetThreadID <> MainThreadID then
    Self.Lock.Release;
end;

class procedure TScriptDispatcher.Create;
begin
  if ScrptDispatcher = nil then
    ScrptDispatcher := TScriptDispatcher.Instantiate;
end;

destructor TScriptDispatcher.Destroy;
var
  I: Integer;
begin
  for I := 0 to Self.FScripts.Count - 1 do
    TScript(Self.FScripts[I]).Free;
  FScripts.Free;
  FScriptsToUnregister.Free;
  FCheckFunctions.Free;

  inherited;
end;

function TScriptDispatcher.GetScriptNames: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Self.FScripts.Count - 1 do
    Result.Add(TScript(Self.FScripts[I]).Name);
end;

procedure TScriptDispatcher.WriteInfo(Text: string);
begin
  MainConsole.Console(' [*] ' + Text, SERVER_MESSAGE_COLOR);
end;


// TODO: split into two overloads, as the behavior should be in fact different
procedure TScriptDispatcher.FindScripts(Name: string = '');
var
  I, J: Integer;
  DirInfo: TSearchRec;
  Script: TScript;
  Folders: TStringList;
begin
  Folders := TStringList.Create;
  try
    Self.DoLock;
    // reset the list
    // TODO: perhaps this could be done in some better place
    if Name = '' then
    begin
      if Assigned(Self.FScripts) then
      begin
        for I := 0 to Self.FScripts.Count - 1 do
        begin
          if GetThreadID <> MainThreadID then
            TScript(Self.FScripts[i]).Lock.Acquire;
          TScript(Self.FScripts[i]).Free;
        end;
      end;
      Self.FScripts.Free;
      Self.FScripts := TList.Create;
    end
    else
    begin
      for I := 0 to Self.FScripts.Count - 1 do
        if TScript(Self.FScripts[I]).Name = Name then
        begin
          TScript(Self.FScripts[I]).Free;
          Self.FScripts.Delete(I);
          break;
        end;
    end;
    if FindFirst(Self.Dir + '/*', FaDirectory, DirInfo) = 0 then
    begin
      repeat
        if (((DirInfo.Attr and FaDirectory) = FaDirectory) and
          (DirInfo.Name <> '.') and (DirInfo.Name <> '..')) then
        begin
          Folders.Add(DirInfo.Name);
        end;
      until FindNext(DirInfo) <> 0;
    end;
    FindClose(DirInfo);

    Folders.Sort;
    for I := Folders.Count - 1 downto 0 do
    begin
      for J := 0 to Self.FCheckFunctions.Count - 1 do
      begin
        Script := TCheckFunction(Self.FCheckFunctions[J])(Self.Dir +
          '/' + Folders[I]);
        if (Script <> nil) and ((Script.Name = Name) or (Name = '')) then
        begin
          Self.FScripts.Add(Script);
          Break;
        end;
      end;
    end;
  finally
    Folders.Free;
  end;
end;

function TScriptDispatcher.Prepare: Boolean;
var
  I: Integer;
begin
  Result := True;
  Self.WriteInfo('Preparing scripts to be launched');
  Self.FindScripts;
  for I := Self.FScripts.Count - 1 downto 0 do
    if not TScript(FScripts[I]).Prepare then
      Self.DoUnregisterScript(FScripts[i]);
  Self.ProcessUnregisterQueue;
  Self.WriteInfo('Done');
end;

procedure TScriptDispatcher.Launch;
var
  I: Integer;
begin
  for I := Self.FScripts.Count - 1 downto 0 do
    TScript(FScripts[I]).Launch;
  Self.ProcessUnregisterQueue;
  Self.DoUnlock;
end;

procedure TScriptDispatcher.Launch(Name: string);
var
  I: Integer;
  Script: TScript;
begin
  Self.FindScripts(Name);
  for I := Self.FScripts.Count - 1 downto 0 do
  begin
    Script := TScript(FScripts[I]);
    if Script.Name = Name then
    begin
      try
        if GetThreadID <> MainThreadID then
          Script.Lock.Acquire;
        if Script.Prepare then
          Script.Launch
        else
          Self.DoUnregisterScript(Script);
      finally
        if GetThreadID <> MainThreadID then
          Script.Lock.Release;
        Self.DoUnlock;
      end;
      Exit;
    end;
  end;
  Self.WriteInfo('Script not found: ' + Name);
  Self.ProcessUnregisterQueue;
  Self.DoUnlock;
end;

procedure TScriptDispatcher.RegisterCheckFunction(Func: TCheckFunction);
begin
  FCheckFunctions.Add(@Func);
end;

procedure TScriptDispatcher.UnregisterScript(Script: TScript);
begin
  Self.FScriptsToUnregister.Add(Script);
end;

//EVENTS

function TScriptDispatcher.CallFunc(const Params: array of Variant;
  FuncName: string; DefaultReturn: Variant): Variant;
var
  I: Integer;
  Name, Func: string;
  Pos: Byte;
begin
  Result := DefaultReturn;
  Pos := PosEx('.', FuncName);
  Name := Self.Dir + '/' + Copy(FuncName, 1, Pos - 1) + '/';
  Func := Copy(FuncName, Pos + 1, Length(FuncName));
  for I := Self.FScripts.Count - 1 downto 0 do
    if TScript(Self.FScripts[I]).Dir = Name then
    begin
      Result := TScript(Self.FScripts[I]).CallFunc(Params, Func, DefaultReturn);
      Break;
    end;
  Self.ProcessUnregisterQueue;
end;

procedure TScriptDispatcher.OnActivateServer;
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnActivateServer;
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnClockTick;
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnClockTick;
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

function TScriptDispatcher.OnRequestGame(Ip, Hw: string; Port: Word;
  State: Byte; Forwarded: Boolean; Password: string): Integer;
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      State := TScript(Self.FScripts[I]).OnRequestGame(Ip, Hw, Port,
        State, Forwarded, Password);
    Result := State;
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

function TScriptDispatcher.OnBeforeJoinTeam(Id, Team, OldTeam: Byte): ShortInt;
var
  I: Integer;
begin
  Result := Team;
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
    begin
      if Result = -1 then
        TScript(Self.FScripts[I]).OnBeforeJoinTeam(Id, Team, OldTeam)
      else
      begin
        Result := TScript(Self.FScripts[I]).OnBeforeJoinTeam(Id, Team, OldTeam);
        if Result <> -1 then
          Team := Result;
      end;
    end;
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnJoinTeam(Id, Team, OldTeam: Byte; JoinGame: Boolean);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnJoinTeam(Id, Team, OldTeam, JoinGame);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnLeaveGame(Id: Byte; Kicked: Boolean);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnLeaveGame(Id, Kicked);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;


procedure TScriptDispatcher.OnBeforeMapChange(Map: string);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnBeforeMapChange(Map);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnAfterMapChange(Map: string);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnAfterMapChange(Map);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;


procedure TScriptDispatcher.OnAdminConnect(Ip: string; Port: Word);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnAdminConnect(Ip, Port);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnAdminDisconnect(Ip: string; Port: Word);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnAdminDisconnect(Ip, Port);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnAdminMessage(Ip: string; Port: Word; Msg: string);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnAdminMessage(Ip, Port, Msg);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;


procedure TScriptDispatcher.OnFlagGrab(Id, TeamFlag: Byte; GrabbedInBase: Boolean);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnFlagGrab(Id, TeamFlag, GrabbedInBase);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnFlagScore(Id, TeamFlag: Byte);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnFlagScore(Id, TeamFlag);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnFlagReturn(Id, TeamFlag: Byte);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnFlagReturn(Id, TeamFlag);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnFlagDrop(Id, TeamFlag: Byte; Thrown: Boolean);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnFlagDrop(Id, TeamFlag, Thrown);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnKitPickup(Id, KitId: Byte);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnKitPickup(Id, KitId);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

function TScriptDispatcher.OnBeforePlayerRespawn(Id: Byte): TVector2;
var
  I: Integer;
begin
  Result := SpriteParts.Pos[Id];
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
    begin
      Result := TScript(Self.FScripts[I]).OnBeforePlayerRespawn(Id);
      SpriteParts.Pos[Id] := Result;
    end;
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnAfterPlayerRespawn(Id: Byte);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnAfterPlayerRespawn(Id);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

function TScriptDispatcher.OnPlayerDamage(Victim, Shooter: Byte;
  Damage: Single; Weapon: Byte): Single;
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      Damage := TScript(Self.FScripts[I]).OnPlayerDamage(Victim,
        Shooter, Damage, Weapon);
    Result := Damage;
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnPlayerKill(Killer, Victim, BulletID: Byte);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnPlayerKill(Killer, Victim, BulletID);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnWeaponChange(Id, Primary, Secondary,
  PrimaryAmmo, SecondaryAmmo: Byte);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnWeaponChange(Id, Primary, Secondary,
        PrimaryAmmo, SecondaryAmmo);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;


function TScriptDispatcher.OnVoteMapStart(Id: Byte; Map: string): Boolean;
var
  I: Integer;
begin
  try
    Self.DoLock;
    Result := False;
    for I := Self.FScripts.Count - 1 downto 0 do
      Result := Result or TScript(Self.FScripts[I]).OnVoteMapStart(Id, Map);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

function TScriptDispatcher.OnVoteKickStart(Id, Victim: Byte; Reason: string): Boolean;
var
  I: Integer;
begin
  try
    Self.DoLock;
    Result := False;
    for I := Self.FScripts.Count - 1 downto 0 do
      Result := Result or TScript(Self.FScripts[I]).OnVoteKickStart(Id, Victim, Reason);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnVoteMap(Id: Byte; Map: string);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnVoteMap(Id, Map);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

procedure TScriptDispatcher.OnVoteKick(Id, Victim: Byte);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnVoteKick(Id, Victim);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;


procedure TScriptDispatcher.OnPlayerSpeak(Id: Byte; Text: string);
var
  I: Integer;
begin
  try
    Self.DoLock;
    for I := Self.FScripts.Count - 1 downto 0 do
      TScript(Self.FScripts[I]).OnPlayerSpeak(Id, Text);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

function TScriptDispatcher.OnPlayerCommand(Id: Byte; Command: string): Boolean;
var
  I: Integer;
begin
  try
    Self.DoLock;
    Result := False;
    for I := Self.FScripts.Count - 1 downto 0 do
      Result := Result or TScript(Self.FScripts[I]).OnPlayerCommand(Id, Command);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

function TScriptDispatcher.OnConsoleCommand(Ip: string; Port: Word;
  Command: string): Boolean;
var
  I: Integer;
begin
  try
    Self.DoLock;
    Result := False;
    for I := Self.FScripts.Count - 1 downto 0 do
      Result := Result or TScript(Self.FScripts[I]).OnConsoleCommand(Ip, Port, Command);
  finally
    Self.ProcessUnregisterQueue;
    Self.DoUnlock;
  end;
end;

initialization
  ScrptDispatcher := TScriptDispatcher.Instantiate;
  ScrptDispatcher.registerCheckFunction(@ScriptCore.CheckFunction);
  ScrptDispatcher.registerCheckFunction(@ScriptCore3.CheckFunction);
end.

{*******************************************************}
{                                                       }
{       ScriptCore script unit for OPENSOLDAT           }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

unit ScriptCore;

{$IFDEF FPC}{$mode delphi}{$M+}{$ENDIF}

interface

uses
  Classes, PascalCore, ScriptCoreInterface, syncobjs,
  // pascal script units
  SysUtils,
  // critical section
  uPSComponent, uPSRuntime, uPSUtils;
  // scriptcore units
type
  // ScriptCore class
  TScriptCore = class(TPascalCore)
  private
    // used to synchronize all the events between themselve and with
    // compilation (Launch) function
    // appOnIdleTimer allows scripts to set how often to call AppOnIdle event
    FAppOnIdleTimer: LongInt;
    // TODO: implement those three
    FErrorCount: Byte;
    FErrorTime: Word;
    FDisabled: Boolean;
    // called by TPSScript when the script is being compiled.
    // used to register all the API functions
    procedure OnCompile(Script: TPSScript);
    //function _onUses(Compiler: TPSPascalCompiler; const Name: string):
    //Boolean;
    // called by TPSScript when any event is being called.
    // used to refresh all the global API variables
    procedure OnExecute(Script: TPSScript);
    // called after successful compilation, registers all pointer variables
    procedure OnAfterCompile;
    {$push}{$warn 3018 off} // Hide "Constructor should be public"
    constructor Create(Dir: string);
    {$pop}
  public
    destructor Destroy; override;
    // Compiles ScriptCore instance
    function Prepare: Boolean; override;
    // Does nothing, yet
    procedure Launch; override;
    // CrossFunc implementation, used also by all the event calls
    function CallFunc(const Params: array of Variant; FuncName: string;
      DefaultReturn: Variant): Variant; override;
    procedure OnClockTick; override;
    procedure OnIdle; override;
    //procedure OnScriptShutdown(ServerShutdown: Boolean);

    function OnRequestGame(Ip, Hw: string; Port: Word; State: Byte;
      Forwarded: Boolean; Password: string): Integer; override;
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

    procedure OnAfterPlayerRespawn(Id: Byte); override;
    function OnPlayerDamage(Victim, Shooter: Byte; Damage: Single;
      BulletID: Byte): Single; override;
    procedure OnPlayerKill(Killer, Victim, BulletID: Byte); override;
    procedure OnWeaponChange(Id, Primary, Secondary,
      PrimaryAmmo, SecondaryAmmo: Byte); override;

    function OnVoteMapStart(Id: Byte; Map: string): Boolean; override;
    function OnVoteKickStart(Id, Victim: Byte; Reason: string): Boolean;
      override;
    procedure OnVoteMap(Id: Byte; Map: string); override;
    procedure OnVoteKick(Id, Victim: Byte); override;
    procedure OnPlayerSpeak(Id: Byte; Text: string); override;
    function OnPlayerCommand(Id: Byte; Command: string): Boolean; override;
    function OnConsoleCommand(Ip: string; Port: Word; Command: string): Boolean;
      override;
  end;

// ScriptCore's check function. Will simply check if Includes.txt is present

// @param dir Directory to check
// @return TScriptCore instance if Includes.txt is found, null otherwise
function CheckFunction(Dir: string): TScriptCore;


implementation

uses
  Net,
  NetworkUtils,
  // OnScriptCrash var
  ScriptDispatcher, strutils,
  // Main tick counter
  Server,
  Game,
  Command,
  Constants,
  //OnPlayerKill
  Weapons;

constructor TScriptCore.Create(Dir: string);
var
  DllPlugin: TPSPlugin;
begin
  inherited Create(Dir);
  Self.FName := Copy(Dir, 9, Length(Dir));
  Self.FErrorCount := 0;
  Self.FErrorTime := 0;
  Self.FDisabled := False;
  if not ScrptDispatcher.SafeMode then
  begin
    DllPlugin := TPSDllPlugin.Create(Self.PascalScript);
    TPSPluginItem(Self.PascalScript.Plugins.Add).Plugin := DllPlugin;
  end;
  Self.FAppOnIdleTimer := 60;
  //Self.PascalScript.Comp.OnUses := OnUses;
  Self.PascalScript.OnCompile := Self.OnCompile;
  Self.PascalScript.OnExecute := Self.OnExecute;
end;

destructor TScriptCore.Destroy;
begin
  Self.CallFunc([], 'DeactivateServer', 0);
  inherited;
end;

function CheckFunction(Dir: string): TScriptCore;
begin
  Result := nil;
  if FileExists(Dir + '/Includes.txt') then
    Result := TScriptCore.Create(Dir);
end;

function TScriptCore.Prepare: Boolean;
var
  S: TStringList;
  I: Integer;
  Buff: string;
  Compiled: Boolean;
begin
  Result := False;
  Self.Lock.Acquire;
  Buff := '';
  S := TStringList.Create;
  S.LoadFromFile(Self.FDir + 'Includes.txt');
  for I := 0 to S.Count - 1 do
    if (Copy(S.Strings[I], 1, 2) <> '//') and (S.Strings[I] <> '') then
    begin
      S.Strings[I] := Copy(S.Strings[I], PosEx(S.Strings[I], '//'),
        Length(S.Strings[I]));
      Buff := Buff + ReadFile(Self.FDir + S.Strings[I], True) + Chr(13) + Chr(10);
      ScrptDispatcher.WriteInfo('Compiling ' + Self.FName + ' -> ' +
        S.Strings[I] + '...');
      if not (FileExists(Self.FDir + S.Strings[I])) then
      begin
        ScrptDispatcher.WriteInfo('Included File not found: ' +
          ExtractFileName(S.Strings[I]));
        Exit;
      end;
    end;
  S.Text := Buff + 'begin end.';
  Buff := '';
  Self.PascalScript.Script := S;
  S.Free;
  Compiled := Self.PascalScript.Compile;
  if Self.PascalScript.CompilerMessageCount > 0 then
    for I := 0 to Self.PascalScript.CompilerMessageCount - 1 do
      ScrptDispatcher.WriteInfo(Self.FName + ' -> ' +
        Self.PascalScript.CompilerErrorToStr(I));
  if not Compiled then
  begin
    if Self.PascalScript.ExecErrorCode <> ErNoError then
      ScrptDispatcher.WriteInfo(Self.FName + ' -> ' +
        Self.PascalScript.ExecErrorToString);
    Exit;
  end;
  try
    ScrptDispatcher.WriteInfo(Self.Name + ' compiled');
    Self.OnAfterCompile;
    Result := True;
  except
  end;
  Self.Lock.Release;
end;

procedure TScriptCore.Launch;
begin
  Self.CallFunc([], 'ActivateServer', 0);
end;

procedure TScriptCore.OnCompile(Script: TPSScript);
begin
  RegisterFunctions(Self, Script);
end;

procedure TScriptCore.OnExecute(Script: TPSScript);
begin
  SetVariables(Self, Script);
end;

procedure TScriptCore.OnAfterCompile;
begin
  Self.PascalScript.SetPointerToData('AppOnIdleTimer', @Self.FAppOnIdleTimer,
    Self.PascalScript.FindBaseType(btU32));
  Self.PascalScript.SetPointerToData('DisableScript', @Self.FDisabled,
    Self.PascalScript.FindBaseType(btU8));
end;

function TScriptCore.CallFunc(const Params: array of Variant;
  FuncName: string; DefaultReturn: Variant): Variant;
begin
  Result := DefaultReturn;
  if Self.PascalScript.Exec.GetProc(FuncName) <> InvalidVal then
  begin
    Self.Lock.Acquire;
    try
      Result := Self.PascalScript.ExecuteFunction(Params, FuncName
        {$IFDEF LEGACY_PASCALSCRIPT},0{$ENDIF}
        );
    except
      on E: Exception do
      begin
        if Self.PascalScript.ExecErrorCode <> ErNoError then
          ScrptDispatcher.WriteInfo('[Error] ' + Self.Name +
            ' -> ' + FuncName + '(' +
            IntToStr(Self.PascalScript.ExecErrorRow) + ':' +
            IntToStr(Self.PascalScript.ExecErrorCol) + ') ' +
            Self.PascalScript.ExecErrorToString)
        else
          ScrptDispatcher.WriteInfo('[Error] ' + Self.Name +
            ' -> (' + FuncName + '): ' + E.Message);
        Self.FErrorCount := Self.FErrorCount + 1;
        if Self.FErrorCount > 10 then
        begin
          if sc_onscriptcrash.Value = 'shutdown' then
          begin
            ProgReady := False;
            ScrptDispatcher.WriteInfo(Self.Name + ' crashed, shutting down');
            ParseInput('say Script' + Self.Name +
              ' crashed, server is shutting down', 255);
          end
          else if sc_onscriptcrash.Value = 'recompile' then
          begin
            ScrptDispatcher.WriteInfo(Self.Name + ' crashed, recompiling...');
            ParseInput('say Script' + Self.Name +
              ' crashed, recompiling...', 255);
            Self.Lock.Release;
            Self.Prepare;
            Self.OnActivateServer;
          end
          else if sc_onscriptcrash.Value = 'disable' then
          begin
            ScrptDispatcher.WriteInfo(Self.Name + ' crashed, disaling');
            ParseInput('say Script' + Self.Name + ' crashed, disabling it', 255);
            ScrptDispatcher.UnregisterScript(Self);
          end;
        end;
      end;
    end;
    Self.Lock.Release;
  end;
end;

procedure TScriptCore.OnClockTick;
begin
  if not Self.FDisabled then
  begin
    if Self.FAppOnIdleTimer > 0 then
      if MainTickCounter mod Self.FAppOnIdleTimer = 0 then
        Self.CallFunc([MainTickCounter], 'AppOnIdle', 0);
    if MainTickCounter mod SECOND = 0 then
      if Self.FErrorCount > 0 then
        Self.FErrorCount := Self.FErrorCount - 1;
  end;
end;

procedure TScriptCore.OnIdle;
begin
  if not Self.FDisabled then
    Self.CallFunc([], 'OnIdle', 0);
end;

//procedure OnScriptShutdown(ServerShutdown: Boolean);

function TScriptCore.OnRequestGame(Ip, Hw: string; Port: Word; State: Byte;
  Forwarded: Boolean; Password: string): Integer;
{$push}{$warn 5024 off}
begin
  if not Self.FDisabled then
  begin
    SetVal(Self.PascalScript, 'ReqPort', Port);
    State := Self.CallFunc([Ip, State], 'OnRequestGame', State);
    State := Self.CallFunc([Ip, Port, State, Forwarded, Password],
      'OnRequestGameEx', State);
  end;
  Result := State;
end;
{$pop}

procedure TScriptCore.OnJoinTeam(Id, Team, OldTeam: Byte; JoinGame: Boolean);
{$push}{$warn 5024 off}
begin
  if not Self.FDisabled then begin
    if JoinGame and (Sprite[Id].Player.ControlMethod = Net.HUMAN) then
      Self.CallFunc([Id, Team], 'OnJoinGame', 0);
    Self.CallFunc([Id, Team], 'OnJoinTeam', 0);
  end;
end;
{$pop}

procedure TScriptCore.OnLeaveGame(Id: Byte; Kicked: Boolean);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, Sprite[Id].Player.Team, Kicked], 'OnLeaveGame', 0);
end;

procedure TScriptCore.OnBeforeMapChange(Map: string);
{$push}{$warn 5024 off}
begin
  if not Self.FDisabled then
    Self.CallFunc([], 'OnGameEnd', 0);
end;
{$pop}

procedure TScriptCore.OnAfterMapChange(Map: string);
begin
  if not Self.FDisabled then
    Self.CallFunc([Map], 'OnMapChange', 0);
end;

procedure TScriptCore.OnAdminConnect(Ip: string; Port: Word);
{$push}{$warn 5024 off}
begin
  if not Self.FDisabled then
    Self.CallFunc([Ip], 'OnAdminConnect', 0);
end;
{$pop}

procedure TScriptCore.OnAdminDisconnect(Ip: string; Port: Word);
{$push}{$warn 5024 off}
begin
  if not Self.FDisabled then
    Self.CallFunc([Ip], 'OnAdminDisconnect', 0);
end;
{$pop}

procedure TScriptCore.OnAdminMessage(Ip: string; Port: Word; Msg: string);
begin
{$push}{$warn 5024 off}
  if not Self.FDisabled then
    Self.CallFunc([Ip, Msg], 'OnAdminMessage', 0);
end;
{$pop}

procedure TScriptCore.OnFlagGrab(Id, TeamFlag: Byte; GrabbedInBase: Boolean);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, TeamFlag, GrabbedInBase], 'OnFlagGrab', 0);
end;

procedure TScriptCore.OnFlagScore(Id, TeamFlag: Byte);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, TeamFlag], 'OnFlagScore', 0);
end;

procedure TScriptCore.OnFlagReturn(Id, TeamFlag: Byte);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, TeamFlag], 'OnFlagReturn', 0);
end;

procedure TScriptCore.OnAfterPlayerRespawn(Id: Byte);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id], 'OnPlayerRespawn', 0);
end;

function TScriptCore.OnPlayerDamage(Victim, Shooter: Byte; Damage: Single;
  BulletID: Byte): Single;
begin
  if not Self.FDisabled then
  begin
    Damage := Self.CallFunc([Victim, Shooter, Integer(Round(Damage))], 'OnPlayerDamage', Damage);
    Damage := Self.CallFunc([Victim, Shooter, Integer(Round(Damage)), WeaponNumInternalToExternal(Bullet[BulletID].OwnerWeapon)],
      'OnPlayerDamageEx', Damage);
  end;
  Result := Damage;
end;

procedure TScriptCore.OnPlayerKill(Killer, Victim, BulletID: Byte);
begin
  if not Self.FDisabled then
  begin
    if Killer = Victim then
      BulletID := 100
    else
      BulletID := Bullet[BulletID].OwnerWeapon;
    Self.CallFunc([Killer, Victim, WeaponNameByNum(BulletID)],
      'OnPlayerKill', 0);
    Self.CallFunc([Killer, Victim, WeaponNumInternalToExternal(BulletID)],
      'OnPlayerKillEx', 0);
  end;
end;

procedure TScriptCore.OnWeaponChange(Id, Primary, Secondary,
  PrimaryAmmo, SecondaryAmmo: Byte);
{$push}{$warn 5024 off}
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, WeaponNumInternalToExternal(Primary), WeaponNumInternalToExternal(Secondary)], 'OnWeaponChange', 0);
end;
{$pop}

function TScriptCore.OnVoteMapStart(Id: Byte; Map: string): Boolean;
begin
  Result := False;
  if not Self.FDisabled then
    Result := Self.CallFunc([Id, Map], 'OnVoteMapStart', False);
end;

function TScriptCore.OnVoteKickStart(Id, Victim: Byte; Reason: string): Boolean;
begin
  Result := False;
  if not Self.FDisabled then
    Result := Self.CallFunc([Id, Victim, Reason], 'OnVoteKickStart', False);
end;

procedure TScriptCore.OnVoteMap(Id: Byte; Map: string);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, Map], 'OnVoteMap', 0);
end;

procedure TScriptCore.OnVoteKick(Id, Victim: Byte);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, Victim], 'OnVoteKick', 0);
end;

procedure TScriptCore.OnPlayerSpeak(Id: Byte; Text: string);
begin
  if not Self.FDisabled then
    Self.CallFunc([Id, Text], 'OnPlayerSpeak', 0);
end;

function TScriptCore.OnPlayerCommand(Id: Byte; Command: string): Boolean;
begin
  Result := False;
  if not Self.FDisabled then
  begin
    if IsRemoteAdminIP(Sprite[Id].Player.IP) or
       IsAdminIP(Sprite[Id].Player.IP) then
      Result := Self.CallFunc([Id, Command], 'OnCommand', False);
    Result := Result or Self.CallFunc([Id, Command], 'OnPlayerCommand', False);
  end;
end;

function TScriptCore.OnConsoleCommand(Ip: string; Port: Word; Command: string): Boolean;
{$push}{$warn 5024 off}
begin
  Result := False;
  if not Self.FDisabled then
    Result := Self.CallFunc([255, Command], 'OnCommand', False);
end;
{$pop}

end.

unit Command;

interface

uses
  classes, contnrs, sysutils, variants, Constants;

procedure CommandInit();
procedure CommandCleanup();
function ParseInput(Input: String; Sender: Byte = 0): Boolean; overload;
function LoadConfig(ConfigName: AnsiString): Boolean;

const
  MAX_COMMANDS = 1024;

type
  TCommandFlag = (CMD_INIT, CMD_ALIAS, CMD_SCRIPT, CMD_DEFERRED, CMD_ADMINONLY, CMD_PLAYERONLY);
  TCommandFlags = set of TCommandFlag;
  PCommand = ^TCommand;
  TCommandFunction = procedure(Args: array of AnsiString; Sender: Byte);
  TCommand = record
    Name: AnsiString;
    FunctionPtr: TCommandFunction;
    Description: AnsiString;
    Flags: TCommandFlags;
  end;
  TCommandTargets = array of byte;
  TDeferredCommand = record
    Command: String;
    Values: TStringList;
  end;

  function CommandAdd(CommandNameVar: AnsiString; CommandPtr: TCommandFunction; Description: AnsiString; Flags: TCommandFlags): PCommand;
  function CommandFind(Name: AnsiString): PCommand;
  procedure CommandExecuteAlias(Args: array of AnsiString; Sender: Byte);
  procedure ParseCommandLine();
  procedure RunDeferredCommands();
  function CommandTarget(Target: AnsiString; Sender: Byte): TCommandTargets;

var
  Commands: TFPHashList;
  DeferredCommands: array of TDeferredCommand;
  DeferredInitialized: Boolean = False;

implementation
  uses {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF}Cvar, strutils, Game, Net
    {$IFDEF DEVELOPMENT}, Steam, ctypes, TraceLog{$ENDIF}
    {$IFDEF SERVER}, NetworkUtils{$ENDIF};

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
{$WARN 5026 OFF : Value parameter "$1" is assigned but never used}
procedure CommandExec(Args: array of AnsiString; Sender: Byte = 255);
begin
  if Length(Args) = 1 then
  begin
    MainConsole.Console('Usage: exec "filename.cfg"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  LoadConfig(Args[1]);
end;

procedure CommandToggle(Args: array of AnsiString; Sender: Byte);
var
  ACvar: TCvarBase;
begin
  if Length(Args) < 4 then
  begin
    MainConsole.Console('Usage: toggle "cvarname" "value" "value2"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  ACvar := TCvarBase.Find(Args[1]);
  if not Assigned(ACvar) then
  begin
    MainConsole.Console('Toggle: Cvar ' + Args[1] + ' not found', DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  if ACvar.ValueAsString = Args[2] then
    ACvar.ParseAndSetValue(Args[3])
  else
    ACvar.ParseAndSetValue(Args[2]);
end;

procedure CommandAlias(Args: array of AnsiString; Sender: Byte);
var
  AliasName: AnsiString;
begin
  if Length(Args) < 3 then
  begin
    MainConsole.Console('Usage: alias "name" "command"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  AliasName := Args[1];
  if (TCvarBase.Find(AliasName) <> nil) or (CommandFind(AliasName) <> nil) then
  begin
    MainConsole.Console('Cannot use this alias name because it''s already used', DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  CommandAdd(AliasName, CommandExecuteAlias, Args[2], [CMD_ALIAS]);
  MainConsole.Console('New alias: ' + Args[1] + ' with command ' + Args[2], GAME_MESSAGE_COLOR);
end;

procedure CommandExecuteAlias(Args: array of AnsiString; Sender: Byte);
var
  CommandPtr: PCommand;
  InputParse: TStringList;
  i: Integer;
begin
  CommandPtr := CommandFind(Args[0]);
  if not Assigned(CommandPtr) then
  begin
    MainConsole.Console('Cannot find alias command', DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  InputParse := TStringList.Create;
  InputParse.Delimiter := ';';
  InputParse.StrictDelimiter := True;
  InputParse.DelimitedText := CommandPtr.Description;
  for i:=0 To InputParse.Count-1 Do
    ParseInput(InputParse[i]);
  InputParse.Free;
end;

procedure CommandEcho(Args: array of AnsiString; Sender: Byte);
begin
  if Length(Args) = 1 then
  begin
    MainConsole.Console('Usage: echo "text"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  MainConsole.Console(Args[1], GAME_MESSAGE_COLOR);
end;

procedure CommandReset(Args: array of AnsiString; Sender: Byte);
var
  CvarName: AnsiString;
  ACvar: TCvarBase;
begin
  if Length(Args) = 1 then
  begin
    MainConsole.Console('Usage: reset "cvarname"', GAME_MESSAGE_COLOR);
    Exit;
  end;
  CvarName := Args[1];
  ACvar := TCvarBase.Find(CvarName);
  if not Assigned(ACvar) then
  begin
    MainConsole.Console('Reset: Cvar ' + CvarName + ' not found', DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  ACvar.Reset();
  MainConsole.Console('Reset: ' + CvarName + ' set to: ' + ACvar.ValueAsString, DEBUG_MESSAGE_COLOR);
end;

procedure CommandCmdlist(Args: array of AnsiString; Sender: Byte);
var
  i: Integer;
  CommandPtr: PCommand;
begin
  for i:=0 to Commands.Count-1 do
  begin
    CommandPtr := Commands.Items[i];
    MainConsole.Console(CommandPtr.Name + ' - ' + CommandPtr.Description, GAME_MESSAGE_COLOR);
  end;
end;

procedure CommandCvarlist(Args: array of AnsiString; Sender: Byte);
var
  i: Integer;
  ACvar: TCvarBase;
begin
  for i:=0 to Cvars.Count-1 do
  begin
    ACvar := Cvars.Items[i];
    {$IFNDEF SERVER}
    {$IFNDEF DEVELOPMENT}
    // Hide temporary sync cvars in client
    if (CVAR_SERVER in ACvar.Flags) or (CVAR_SYNC in ACvar.Flags) then
      Continue;
    {$ENDIF}
    {$ENDIF}
    if Length(Args) = 2 then
      if not AnsiContainsStr(Acvar.Name, Args[1]) then
        Continue;
    MainConsole.Console(PadRight(ACvar.Name, 30) + ' : ' + ACvar.ValueAsString + ' : [' + DumpFlags(Acvar) + ' ] : ' + ACvar.Description, GAME_MESSAGE_COLOR);
  end;
end;

procedure CommandInc(Args: array of AnsiString; Sender: Byte);
var
  CvarName: AnsiString;
  ACvar: TCvarBase;
  FloatCvar: TSingleCvar;
  IntegerCvar: TIntegerCvar;
  NewFloatValue: Single;
  NewIntegerValue: Integer;
begin
  if Length(Args) <= 4 then
  begin
    MainConsole.Console('Usage: inc "cvarname" "min" "max" "delta"', GAME_MESSAGE_COLOR);
    Exit;
  end;

  CvarName := Args[1];
  ACvar := TCvarBase.Find(CvarName);
  if not Assigned(ACvar) then
  begin
    MainConsole.Console('Inc: Cvar ' + CvarName + ' not found', DEBUG_MESSAGE_COLOR);
    Exit;
  end;

  if ACvar is TSingleCvar then
  begin
    FloatCvar := TSingleCvar(ACvar);
    NewFloatValue := FloatCvar.Value + StrToFloatDef(Args[4], 0.0);
    if (NewFloatValue >= StrToFloatDef(Args[2], 0.0)) and (NewFloatValue <= StrToFloatDef(Args[3], 1.0)) then
      FloatCvar.SetValue(NewFloatValue);
  end;

  if ACvar is TIntegerCvar then
  begin
    IntegerCvar := TIntegerCvar(ACvar);
    NewIntegerValue := IntegerCvar.Value + StrToIntDef(Args[4], 0);
    if (NewIntegerValue >= StrToIntDef(Args[2], 0)) and (NewIntegerValue <= StrToIntDef(Args[3], 1)) then
      IntegerCvar.SetValue(NewIntegerValue);
  end;
end;

{$IFDEF DEVELOPMENT}
{$PUSH}
{$WARN 5027 OFF}
procedure CommandNetConfig(Args: array of AnsiString; Sender: Byte);
var
  ConfigName: PChar = '';
  OutDataType: ESteamNetworkingConfigDataType;
  OutScope: ESteamNetworkingConfigScope;
  FloatValue: Single = 0.0;
  IntegerValue: Integer = 0;
  cbResult: csize_t = 0;
  SetResult: Boolean = False;
  ConfigID: Integer;
begin
  if Length(Args) < 3 then
  begin
    MainConsole.Console('Usage: netconfig "id" "value"', GAME_MESSAGE_COLOR);
    Exit;
  end;

  ConfigID := StrToIntDef(Args[1], -1);
  ConfigName := UDP.NetworkingUtil.GetConfigValueInfo(ESteamNetworkingConfigValue(ConfigID), @OutDataType, @OutScope);
  if ConfigName <> Nil then
  begin
    if OutDataType = k_ESteamNetworkingConfig_Int32 then
    begin
      cbResult := SizeOf(Integer);
      IntegerValue := StrToIntDef(Args[2], 0);
      SetResult := UDP.NetworkingUtil.SetConfigValue(ESteamNetworkingConfigValue(StrToInt(Args[1])), k_ESteamNetworkingConfig_Global, 0, OutDataType, @IntegerValue);
      MainConsole.Console(Format('[NET] NetConfig: Set %S to %D, result: %S', [AnsiString(ConfigName), IntegerValue, SetResult.ToString(TUseBoolStrs.True)]), DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
    end
    else if OutDataType = k_ESteamNetworkingConfig_Float then
    begin
      cbResult := SizeOf(Single);
      FloatValue := StrToFloatDef(Args[2], 0.0);
      SetResult := UDP.NetworkingUtil.SetConfigValue(ESteamNetworkingConfigValue(StrToInt(Args[1])), k_ESteamNetworkingConfig_Global, 0, OutDataType, @FloatValue);
      MainConsole.Console(Format('[NET] NetConfig: Set %S to %F, result: %S', [AnsiString(ConfigName), FloatValue, SetResult.ToString(TUseBoolStrs.True)]), DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
    end;
  end
  else
  begin
    MainConsole.Console(Format('[NET] NetConfig: Couldn''t find config value: %S', [Args[1]]), DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
  end;
end;

procedure CommandNetConfigList(Args: array of AnsiString; Sender: Byte);
var
  ConfigName: PChar = '';
  OutDataType: ESteamNetworkingConfigDataType = k_ESteamNetworkingConfig_Int32;
  OutScope: ESteamNetworkingConfigScope;
  CurrentConfigValue: ESteamNetworkingConfigValue;
  FloatValue: Single = 0.0;
  IntegerValue: Integer = 0;
  cbResult: csize_t = 0;
  IterDev: Boolean = {$IFDEF DEVELOPMENT}True;{$ELSE}False;{$ENDIF}
begin
  CurrentConfigValue := UDP.NetworkingUtil.IterateGenericEditableConfigValues(k_ESteamNetworkingConfig_Invalid, IterDev);

  while CurrentConfigValue <> k_ESteamNetworkingConfig_Invalid do
  begin
    ConfigName := UDP.NetworkingUtil.GetConfigValueInfo(CurrentConfigValue, @OutDataType, @OutScope);
    if ConfigName = Nil then
      Break;

    if OutDataType = k_ESteamNetworkingConfig_Int32 then
    begin
      cbResult := SizeOf(Integer);

      if UDP.NetworkingUtil.GetConfigValue(CurrentConfigValue, k_ESteamNetworkingConfig_Global, 0, @OutDataType, @IntegerValue, @cbResult) = k_ESteamNetworkingGetConfigValue_OK then
        MainConsole.Console(Format('[NET] NetConfig: %S is %D', [AnsiString(ConfigName), IntegerValue]), DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
    end
    else if OutDataType = k_ESteamNetworkingConfig_Float then
    begin
      cbResult := SizeOf(Single);
      if UDP.NetworkingUtil.GetConfigValue(CurrentConfigValue, k_ESteamNetworkingConfig_Global, 0, @OutDataType, @FloatValue, @cbResult) = k_ESteamNetworkingGetConfigValue_OK then
        MainConsole.Console(Format('[NET] NetConfig: %S is %F', [AnsiString(ConfigName), FloatValue]), DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
    end;

    CurrentConfigValue := UDP.NetworkingUtil.IterateGenericEditableConfigValues(CurrentConfigValue, IterDev);
  end;
end;

procedure CommandNetLogLevel(Args: array of AnsiString; Sender: Byte);
begin
  if Length(Args) = 1 then
  begin
    MainConsole.Console('Usage: netconfig_loglevel "level"', DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
    Exit;
  end;

  UDP.SetDebugLevel(ESteamNetworkingSocketsDebugOutputType(StrToIntDef(Args[1], 4)));
  MainConsole.Console('[NET] GNS log level set to ' + Args[1], DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
end;
{$POP}
{$ENDIF}

function CommandFind(Name: AnsiString): PCommand;
var
  CommandPtr: PCommand;
begin
  Result := nil;
  CommandPtr := Commands.Find(name);
  if Assigned(CommandPtr) then
    Result := CommandPtr;
end;

function CommandAdd(CommandNameVar: AnsiString; CommandPtr: TCommandFunction; Description: AnsiString; Flags: TCommandFlags): PCommand;
var
  NewCommand : PCommand;
  CommandName: AnsiString;
begin
  Result := nil;
  CommandName := LowerCase(CommandNameVar);
  if CommandFind(CommandName) <> nil then
  begin
    {$IFDEF DEVELOPMENT}
    Debug('CommandAdd: ' + CommandName + ' is already set');
    {$ENDIF}
    Exit;
  end;
  {$IFDEF DEVELOPMENT}
  Debug('CommandAdd: ' + CommandName + ' Description: ' + Description);
  {$ENDIF}
  New(NewCommand);
  NewCommand^.Name := CommandName;
  NewCommand^.FunctionPtr := CommandPtr;
  NewCommand^.Description := Description;
  NewCommand^.Flags := Flags;
  Commands.Add(CommandName, NewCommand);
  Result := NewCommand;
end;
{$POP}

function ParseInput(Command: String; Values: TStringList; Sender: Byte = 0): Boolean; overload;
var
  CommandPtr: PCommand;
  CommandFunction: TCommandFunction;
  ACvar: TCvarBase;
  i: Integer;
  ValuesAsArray: array of AnsiString = nil;
begin
  CommandPtr := CommandFind(Command);

  if CommandPtr <> nil then
  begin
    if (CMD_DEFERRED in CommandPtr.Flags) and (not DeferredInitialized) then
    begin
      SetLength(DeferredCommands, Length(DeferredCommands) + 1);
      DeferredCommands[High(DeferredCommands)].Command := Command;
      DeferredCommands[High(DeferredCommands)].Values := TStringList.Create;
      for i := 0 to Values.Count - 1 do
      begin
        DeferredCommands[High(DeferredCommands)].Values.Add(Values[i]);
      end;
    end
    else
    begin
      {$IFDEF SERVER}
      if CMD_ADMINONLY in CommandPtr.Flags then
        if not ((Sender = 255) or (IsRemoteAdminIP(Sprite[Sender].Player.IP) or IsAdminIP(Sprite[Sender].Player.IP))) then
          Exit;
      if CMD_PLAYERONLY in CommandPtr.Flags then
        if (Sender = 0) or (Sender > MAX_PLAYERS + 1) then
          Exit;
      {$ENDIF}
      SetLength(ValuesAsArray, Values.Count + 1);
      ValuesAsArray[0] := Command;
      for i := 0 to Values.Count - 1 do
      begin
        ValuesAsArray[i + 1] := Values[i];
      end;
      CommandFunction := CommandPtr.FunctionPtr;
      CommandFunction(ValuesAsArray, Sender);
    end;
    Result := True;
    Exit;
  end;

  ACvar := TCvarBase.Find(Command);
  if ACvar <> nil then
  begin
    if CVAR_SERVER in ACvar.Flags then
      {$IFNDEF SERVER}
      // Ignore server-related cvars on client.
      Exit;
      {$ELSE}
      // On server, only allow accessing/editing server-related
      // cvars when the player is an admin, or if input is coming
      // from outside the game (from remote connection like ARSSE,
      // from scripts, or when reading config file).
      if (Sender >= 1) and (Sender <= MAX_PLAYERS) then
        if not (IsRemoteAdminIP(Sprite[Sender].Player.IP) or IsAdminIP(Sprite[Sender].Player.IP)) then
          Exit;
      {$ENDIF}

    if Values.Count = 0 then
    begin
      MainConsole.Console(Format('%s is "%s" (%s)',
        [ACvar.Name, ACvar.ValueAsString, ACvar.Description]),
        DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
    end
    else if Values.Count = 1 then
    begin
      if not ACvar.ParseAndSetValue(Values[0]) then
      begin
        MainConsole.Console(Format('Unable to set %s: %s',
          [ACvar.Name, ACvar.GetErrorMessage]),
          DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
      end else
      begin
        MainConsole.Console(Format('%s is now set to: "%s"',
          [ACvar.Name, ACvar.ValueAsString]),
          DEBUG_MESSAGE_COLOR{$IFDEF SERVER}, Sender{$ENDIF});
      end;
    end;
    Result := True;
  end;
end;

function ParseInput(Input: String; Sender: Byte = 0): Boolean; overload;
var
  Command: String;
  InputParse: TStringList;
begin
  Result := False;

  if Length(Input) = 0 then
    Exit;

  InputParse := TStringList.Create;
  InputParse.Delimiter := ' ';
  InputParse.DelimitedText := TrimLeft(Input);

  Command := InputParse[0];
  InputParse.Delete(0);

  Result := ParseInput(Command, InputParse, Sender);
  InputParse.Free;
end;

function LoadConfig(ConfigName: AnsiString): Boolean;
var
  Path: string;
  ConfigFile: TextFile;
  s: string;
begin
  Result := False;
  Path := UserDirectory + 'configs/' + ConfigName;
  if not FileExists(Path) then
  begin
    MainConsole.Console('No such config file: ' + ConfigName, WARNING_MESSAGE_COLOR);
    Exit;
  end;
  AssignFile(ConfigFile, Path);
  try
    Reset(ConfigFile);
    while not Eof(ConfigFile) do
    begin
      ReadLn(ConfigFile, s);
      s := TrimLeft(s);
      if AnsiStartsStr('//', s) then
        Continue;
      ParseInput(s);
    end;
    CloseFile(ConfigFile);
    Result := True;
  except
    on E: EInOutError do
    begin
      Result := False;
      MainConsole.Console('Failed to load config file: ' + ConfigName, WARNING_MESSAGE_COLOR);
    end;
  end;
end;

procedure ParseCommandLine();
var
  CurrentCommand: AnsiString = '';
  CurrentCommandValues: TStringList;
  i: Integer;
begin
  CurrentCommandValues := TStringList.Create;
  for i := 1 to argc do
  begin
    if (argv[i] <> '') and (argv[i][0] <> '-') then
    begin
      CurrentCommandValues.Add(argv[i]);
      continue;
    end;

    if CurrentCommand <> '' then
    begin
      ParseInput(copy(CurrentCommand, 2), CurrentCommandValues);
      CurrentCommandValues.Clear;
    end;

    CurrentCommand := argv[i];
  end;
  CurrentCommandValues.Free;
end;

function CommandTarget(Target: AnsiString; Sender: Byte): TCommandTargets;
var
  Players: array of byte;
  j: integer;
  TargetID: integer;

  procedure AddPlayer(Id: Byte);
  begin
    SetLength(Players, Length(Players) + 1);
    Players[High(Players)] := Sprite[Id].Num;
  end;
begin
  TargetID := StrToIntDef(Target, 0);
  SetLength(Players, 0);

  for j := 1 to MAX_PLAYERS do
  begin
    if Sprite[j].Active then
    begin
      if (TargetID <> 0) and (j = TargetID) then
      begin
        AddPlayer(j);
        Break;
      end;

      if Sprite[j].Player.Name = Target then
      begin
        AddPlayer(Sprite[j].Num);
        Break;
      end;

      if Target = '@all' then
      begin
        AddPlayer(Sprite[j].Num);
      end
      else if Target = '@bots' then
      begin
        if Sprite[j].Player.ControlMethod = BOT then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@humans' then
      begin
        if Sprite[j].Player.ControlMethod = HUMAN then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@alive' then
      begin
        if not Sprite[j].DeadMeat then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@dead' then
      begin
        if Sprite[j].DeadMeat then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@aim' then
      begin
        if (Sender > 0) and (Sender <= MAX_PLAYERS) then
          if Sprite[Sender].Player.Camera = Sprite[j].Num then
            AddPlayer(Sprite[j].Num);
      end
      else if Target = '@me' then
      begin
        if (Sender > 0) and (Sender <= MAX_PLAYERS) then
          if (Sprite[Sender].Num = Sprite[j].Num) then
            AddPlayer(Sprite[Sender].Num);
      end
      else if Target = '@!me' then
      begin
        if (Sender > 0) and (Sender <= MAX_PLAYERS) then
          if not (Sprite[Sender].Num = Sprite[j].Num) then
            AddPlayer(Sprite[j].Num);
      end
      else if Target = '@none' then
      begin
        if Sprite[j].Player.Team = TEAM_NONE then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@alpha' then
      begin
        if Sprite[j].Player.Team = TEAM_ALPHA then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@bravo' then
      begin
        if Sprite[j].Player.Team = TEAM_BRAVO then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@charlie' then
      begin
        if Sprite[j].Player.Team = TEAM_CHARLIE then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@delta' then
      begin
        if Sprite[j].Player.Team = TEAM_DELTA then
          AddPlayer(Sprite[j].Num);
      end
      else if Target = '@spec' then
      begin
        if Sprite[j].Player.Team = TEAM_SPECTATOR then
          AddPlayer(Sprite[j].Num);
      end;
    end;
  end;
  Result := Players;
end;

procedure RunDeferredCommands();
var
  i: Integer;
begin
  DeferredInitialized := True;
  if Length(DeferredCommands) = 0 then
    Exit;
  for i := Low(DeferredCommands) To High(DeferredCommands) do
  begin
    ParseInput(DeferredCommands[i].Command, DeferredCommands[i].Values);
    DeferredCommands[i].Values.Free;
  end;
  SetLength(DeferredCommands, 0);
end;

procedure CommandInit();
begin
  Commands := TFPHashList.Create;
  SetLength(DeferredCommands, 0);
  CommandAdd('echo', CommandEcho, 'echo text', []);
  CommandAdd('exec', CommandExec, 'executes fileconfig', []);
  CommandAdd('cmdlist', CommandCmdlist, 'list of commands', []);
  CommandAdd('cvarlist', CommandCvarlist, 'list of cvars', []);
  CommandAdd('reset', CommandReset, 'resets cvar to default value', []);
  CommandAdd('alias', CommandAlias, 'creates alias', []);
  CommandAdd('toggle', CommandToggle, 'toggles cvar between two values', []);
  CommandAdd('inc', CommandInc, 'increments cvar value', []);
  {$IFDEF DEVELOPMENT}
  CommandAdd('netconfig', CommandNetConfig, 'Set GNS config', []);
  CommandAdd('netconfig_conn', CommandNetConfig, 'Set GNS config for specific connection handle', []);
  CommandAdd('netconfig_list', CommandNetConfigList, 'List GNS cvars', []);

  CommandAdd('netconfig_loglevel', CommandNetLogLevel, 'Set GNS log level', []);

  {$ENDIF}
end;

procedure CommandCleanup();
var
  i: Integer;
begin
  for i := Low(DeferredCommands) to High(DeferredCommands) do
    DeferredCommands[i].Values.Free;
  SetLength(DeferredCommands, 0);
  if Commands <> Nil then
    for i := 0 to Commands.Count - 1 do
      Dispose(PCommand(Commands[i]));
  FreeAndNil(Commands);
end;

end.

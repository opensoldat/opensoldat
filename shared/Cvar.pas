unit Cvar;

interface

uses
  Classes, Contnrs, Sysutils, Variants, Command, Constants, Util;
{
  Cvar tags
  sv_ - server cvar
  cl_ - client cvar
  mp_ - multiplayer cvars
  r_  - renderer settings
  ui_ - interface settings
  fs_ - filesystem settings
}

const
    MAX_CVARS = 1024;

type
  {
    CVAR_IMMUTABLE       - can't be changed after set
    CVAR_ARCHIVE         - save cvar to cfg file
    CVAR_SPONLY          - only in singleplayer mode
    CVAR_NOTIFY          - notify players after change
    CVAR_MODIFIED        - this flag is set after cvar changed initial value
    CVAR_CLIENT          - client cvar
    CVAR_SERVER          - server cvar
    CVAR_SYNC            - sync cvar to client cvar
    CVAR_SCRIPT          - cvar set by script
    CVAR_INITONLY        - cvar can be changed only at startup
    CVAR_SERVER_INITONLY - cvar can be changed only at startup by the server
  }
  TCvarFlag = (CVAR_IMMUTABLE, CVAR_ARCHIVE, CVAR_SPONLY, CVAR_NOTIFY,
      CVAR_MODIFIED, CVAR_CLIENT, CVAR_SERVER, CVAR_SYNC, CVAR_SCRIPT,
      CVAR_INITONLY, CVAR_SERVER_INITONLY, CVAR_TOSYNC);
  TCvarFlags = set of TCvarFlag;

  TCvarBase = class
  private
    FName: AnsiString;
    FFlags: TCvarFlags;
    FDescription: AnsiString;
    FErrorMessage: AnsiString;
  public
    function ParseAndSetValue(Value: string): Boolean; virtual; abstract;
    function ValueAsString: AnsiString; virtual; abstract;
    function GetErrorMessage: AnsiString; virtual; abstract;
    procedure Reset(); virtual; abstract;
    procedure SyncUpdate(ToSync: Boolean);
    class function Find(Name: AnsiString): TCvarBase;
    property Name: AnsiString read FName;
    property Flags: TCvarFlags read FFlags;
    property Description: AnsiString read FDescription;
  end;

  TCvar<T> = class(TCvarBase)
  type
    TCallback = function(Cvar: TCvar<T>; NewValue: T): Boolean;
  private
    FValue: T;
    FDefaultValue: T;
    FOnChange: TCallback;
  public
    constructor Create(Name, Description: AnsiString; DefaultValue: T; Flags: TCvarFlags; OnChange: TCallback);
    procedure Reset(); override;
    function SetValue(Value: T): Boolean; virtual; abstract;
    class function Find(Name: AnsiString): TCvar<T>;
    property Value: T read FValue;
    property DefaultValue: T read FDefaultValue;
    property OnChange: TCallback read FOnChange write FOnChange;
  end;

  TIntegerCvar = class(TCvar<Integer>)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
  public
    constructor Create(Name, Description: AnsiString; DefaultValue: Integer; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Integer);
    function SetValue(Value: Integer): Boolean; override;
    function GetErrorMessage: AnsiString; override;
    function ParseAndSetValue(Value: string): Boolean; override;
    function ValueAsString: AnsiString; override;
    class function Add(Name, Description: AnsiString; DefaultValue: Integer; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Integer): TIntegerCvar;
    property MinValue: Integer read FMinValue;
    property MaxValue: Integer read FMaxValue;
  end;

  TSingleCvar = class(TCvar<Single>)
  private
    FMinValue: Single;
    FMaxValue: Single;
  public
    constructor Create(Name, Description: AnsiString; DefaultValue: Single; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Single);
    function SetValue(Value: Single): Boolean; override;
    function GetErrorMessage: AnsiString; override;
    function ParseAndSetValue(Value: string): Boolean; override;
    function ValueAsString: AnsiString; override;
    class function Add(Name, Description: AnsiString; DefaultValue: Single; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Single): TSingleCvar;
    property MinValue: Single read FMinValue;
    property MaxValue: Single read FMaxValue;
  end;

  TBooleanCvar = class(TCvar<Boolean>)
  public
    function SetValue(Value: Boolean): Boolean; override;
    function GetErrorMessage: AnsiString; override;
    function ParseAndSetValue(Value: string): Boolean; override;
    function ValueAsString: AnsiString; override;
    class function Add(Name, Description: AnsiString; DefaultValue: Boolean; Flags: TCvarFlags; OnChange: TCallback): TBooleanCvar;
  end;

  TColorCvar = class(TCvar<TColor>)
  public
    function SetValue(Value: TColor): Boolean; override;
    function GetErrorMessage: AnsiString; override;
    function ParseAndSetValue(Value: string): Boolean; override;
    function ValueAsString: AnsiString; override;
    class function Add(Name, Description: AnsiString; DefaultValue: TColor; Flags: TCvarFlags; OnChange: TCallback): TColorCvar;
  end;

  TStringCvar = class(TCvar<AnsiString>)
  private
    FMinLength: Integer;
    FMaxLength: Integer;
  public
    constructor Create(Name, Description: AnsiString; DefaultValue: AnsiString; Flags: TCvarFlags; OnChange: TCallback; MinLength, MaxLength: Integer);
    function SetValue(Value: AnsiString): Boolean; override;
    function GetErrorMessage: AnsiString; override;
    function ParseAndSetValue(Value: string): Boolean; override;
    function ValueAsString: AnsiString; override;
    class function Add(Name, Description: AnsiString; DefaultValue: AnsiString; Flags: TCvarFlags; OnChange: TCallback; MinLength, MaxLength: Integer): TStringCvar;
    property MinLength: Integer read FMinLength;
    property MaxLength: Integer read FMaxLength;
  end;

procedure CvarInit();
procedure CvarCleanup();
function DumpFlags(Cvar: TCvarBase): AnsiString;
procedure ResetSyncCvars;

var
  Cvars: TFPHashList;
  CvarsSync: TFPHashList;
  CvarsNeedSyncing: Boolean = False;
  CvarsInitialized: Boolean = False;

implementation
  uses {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} TraceLog, Math, Game {$IFNDEF SERVER}, Sound, Demo {$ENDIF};

{$IFNDEF SERVER}
function snd_volumeChange(Cvar: TCvarBase; NewValue: Integer): Boolean;
begin
  VolumeInternal := ScaleVolumeSetting(NewValue);
  SetVolume(-1, VolumeInternal);
  Cvar.FErrorMessage := '';
  Result := True;
end;

function r_zoomChange(Cvar: TCvarBase; NewValue: Single): Boolean;
begin
  if (not Sprite[MySprite].IsSpectator) and (not IsZero(NewValue)) then
  begin
    Cvar.FErrorMessage := 'You need to be in the spectators team';
    Result := False;
  end else
    Result := True;
end;

function cl_player_wepChange(Cvar: TCvarBase; NewValue: Integer): Boolean;
begin
  if WeaponActive[NewValue] = 1 then
    Sprite[MySprite].SelWeapon := NewValue;
  Cvar.FErrorMessage := '';
  Result := True;
end;

function demo_speedChange(Cvar: TCvarBase; NewValue: Single): Boolean;
begin
  if not DemoPlayer.Active then
  begin
    Cvar.FErrorMessage := 'This command works only with demos';
    Result := False;
  end else
  begin
    GOALTICKS := Round(DEFAULT_GOALTICKS * NewValue);
    Result := True;
  end;
end;
{$ELSE}
function killlimitChange(Cvar: TStringCvar; NewValue: Integer): Boolean;
begin
  sv_killlimit.SetValue(NewValue);
  Cvar.FErrorMessage := '';
  Result := True;
end;

function sv_maplistChange(Cvar: TCvarBase; NewValue: String): Boolean;
begin
  Result := False;

  if MapsList = Nil then
  begin
    Result := True;
    Exit;
  end;

  if LoadMapsList(NewValue) then
    Result := True
  else
    Cvar.FErrorMessage := 'Maps list file not found';
end;
{$ENDIF}

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
function CommandLineOnlyChange(Cvar: TCvarBase; NewValue: Boolean): Boolean;
begin
  if (UserDirectory <> '') or (BaseDirectory <> '') then
  begin
    Cvar.FErrorMessage := Cvar.Name + ' must be set from the command line';
    Result := False;
    Exit;
  end;

  Result := True;
end;
{$POP}

function sv_gravityChange(Cvar: TCvarBase; NewValue: Single): Boolean;
begin
  Cvar := Cvar;
  GRAV := NewValue;
  SpriteParts.Gravity := GRAV;
  GostekSkeleton.Gravity := 1.06 * GRAV;
  BulletParts.Gravity := GRAV * 2.25;
  SparkParts.Gravity := GRAV / 1.4;
  Result := True;
end;

procedure ResetSyncCvars;
var
  i: Integer;
begin
 for i := 0 to CvarsSync.Count - 1 do
    TCvarBase(CvarsSync.Items[i]).Reset;
end;

function CheckIfCvarExists(Name: AnsiString): Boolean;
begin
  Result := False;
  if Assigned(Cvars.Find(Name)) then
  begin
    Debug('[CVAR] CvarAdd: ' + Name + ' is already set');
    Exit;
  end;
  Result := True;
end;

procedure DumpCvar(Cvar: TCvarBase; DefaultValue: Variant);
begin
  Debug('[CVAR] CvarAdd: ' + Cvar.Name + ' DefaultValue: '
      + VarToStr(DefaultValue) +
      ' FLAGS: 0 ' + ' Description: ' + Cvar.Description);
end;

function DumpFlags(Cvar: TCvarBase): AnsiString;
var
  CvarFlags: AnsiString = '';
begin
  if CVAR_IMMUTABLE in Cvar.FFlags then
    CvarFlags := CvarFlags + ' I';
  if CVAR_ARCHIVE in Cvar.FFlags then
    CvarFlags := CvarFlags + ' A';
  if CVAR_SPONLY in Cvar.FFlags then
    CvarFlags := CvarFlags + ' SP';
  if CVAR_NOTIFY in Cvar.FFlags then
    CvarFlags := CvarFlags + ' N';
  if CVAR_MODIFIED in Cvar.FFlags then
    CvarFlags := CvarFlags + ' M';
  if CVAR_CLIENT in Cvar.FFlags then
    CvarFlags := CvarFlags + ' CL';
  if CVAR_SERVER in Cvar.FFlags then
    CvarFlags := CvarFlags + ' SV';
  if CVAR_SYNC in Cvar.FFlags then
    CvarFlags := CvarFlags + ' SYNC';
  if CVAR_SCRIPT in Cvar.FFlags then
    CvarFlags := CvarFlags + ' SC';
  if CVAR_INITONLY in Cvar.FFlags then
    CvarFlags := CvarFlags + ' INITONLY';

  Result := CvarFlags;
end;

class function TCvarBase.Find(Name: AnsiString): TCvarBase;
begin
  Result := Cvars.Find(Name);
end;

procedure TCvarBase.SyncUpdate(ToSync: Boolean);
begin
  if ToSync then
  begin
    Include(FFlags, CVAR_TOSYNC);
    CvarsNeedSyncing := True;
  end else
    Exclude(Self.FFlags, CVAR_TOSYNC);
end;

constructor TCvar<T>.Create(Name, Description: AnsiString; DefaultValue: T; Flags: TCvarFlags; OnChange: TCallback);
begin
  Self.FName := Name;
  Self.FDescription := Description;
  Self.FValue := DefaultValue;
  Self.FDefaultValue := DefaultValue;
  Self.FFlags := Flags;
  Self.FOnChange := OnChange;
end;

procedure TCvar<T>.Reset();
begin
  SetValue(FDefaultValue);
end;


class function TCvar<T>.Find(Name: AnsiString): TCvar<T>;
var
  Cvar: TCvarBase;
begin
  Result := nil;
  Cvar := TCvarBase.Find(Name);
  if not Assigned(Cvar) then Exit;
  if not (Cvar is TCvar<T>) then Exit;
  Result := TCvar<T>(Cvar);
end;

constructor TIntegerCvar.Create(Name, Description: AnsiString; DefaultValue: Integer; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Integer);
begin
  inherited Create(Name, Description, DefaultValue, Flags, OnChange);
  Self.FMinValue := MinValue;
  Self.FMaxValue:= MaxValue;
end;

class function TIntegerCvar.Add(Name, Description: AnsiString; DefaultValue: Integer; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Integer): TIntegerCvar;
var
  NewCvar: TIntegerCvar;
begin
  Result := nil;
  if not CheckIfCvarExists(Name) then Exit;

  NewCvar := TIntegerCvar.Create(Name, Description, DefaultValue, Flags, OnChange, MinValue, MaxValue);
  {$IFDEF DEVELOPMENT}
  DumpCvar(NewCvar, DefaultValue);
  {$ENDIF}
  Cvars.Add(Name, NewCvar);
  if CVAR_SYNC in Flags then
    CvarsSync.Add(Name, NewCvar);
  Result := NewCvar;
end;

function TIntegerCvar.SetValue(Value: Integer): Boolean;
begin
  if (CVAR_INITONLY in FFlags) and CvarsInitialized then
  begin
    Result := False;
    FErrorMessage := 'Can be set only at startup';
    Exit;
  end;

  {$IFDEF SERVER}
  if (CVAR_SERVER_INITONLY in FFlags) and CvarsInitialized then
  begin
    Result := False;
    FErrorMessage := 'Can be set only by server at startup';
    Exit;
  end;
  {$ENDIF}

  if (Value >= FMinValue) and (Value <= FMaxValue) then
  begin
    if Assigned(FOnChange) then
    begin
      if not FOnChange(Self, Value) then
      begin
        Result := False;
        Exit;
      end;
    end;

    if Value <> FDefaultValue then
      Include(FFlags, CVAR_MODIFIED)
    else
      Exclude(FFlags, CVAR_MODIFIED);

    {$IFDEF SERVER}
    if Value <> FValue then
      SyncUpdate(True);
    {$ENDIF}

    FValue := Value;
    Result := True;
  end
  else
  begin
    FErrorMessage := 'Value must be between ' + IntToStr(MinValue) + ' and ' + IntToStr(MaxValue);
    Result := False;
  end;
end;

function TIntegerCvar.GetErrorMessage: AnsiString;
begin
  Result := FErrorMessage;
  FErrorMessage := '';
end;

function TIntegerCvar.ParseAndSetValue(Value: String): Boolean;
begin
  Result := SetValue(StrToIntDef(Value, 0));
end;

function TIntegerCvar.ValueAsString: AnsiString;
begin
  Result := IntToStr(FValue);
end;

constructor TSingleCvar.Create(Name, Description: AnsiString; DefaultValue: Single; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Single);
begin
  inherited Create(Name, Description, DefaultValue, Flags, OnChange);
  Self.FMinValue := MinValue;
  Self.FMaxValue := MaxValue;
end;

class function TSingleCvar.Add(Name, Description: AnsiString; DefaultValue: Single; Flags: TCvarFlags; OnChange: TCallback; MinValue, MaxValue: Single): TSingleCvar;
var
  NewCvar: TSingleCvar;
  CvarName: AnsiString;
begin
  Result := nil;
  CvarName := LowerCase(Name);
  if not CheckIfCvarExists(CvarName) then Exit;

  NewCvar := TSingleCvar.Create(CvarName, Description, DefaultValue, Flags, OnChange, MinValue, MaxValue);
  {$IFDEF DEVELOPMENT}
  DumpCvar(NewCvar, DefaultValue);
  {$ENDIF}
  Cvars.Add(CvarName, NewCvar);
  if CVAR_SYNC in Flags then
    CvarsSync.Add(Name, NewCvar);
  Result := NewCvar;
end;

function TSingleCvar.SetValue(Value: Single): Boolean;
begin
  if (CVAR_INITONLY in FFlags) and CvarsInitialized then
  begin
    Result := False;
    FErrorMessage := 'Can be set only at startup';
    Exit;
  end;

  if (Value >= FMinValue) and (Value <= FMaxValue) then
  begin
    if Assigned(FOnChange) then
    begin
      if not FOnChange(Self, Value) then
      begin
        Result := False;
        Exit;
      end;
    end;

    if Value <> FDefaultValue then
      Include(FFlags, CVAR_MODIFIED)
    else
      Exclude(FFlags, CVAR_MODIFIED);

    {$IFDEF SERVER}
    if Value <> FValue then
      SyncUpdate(True);
    {$ENDIF}

    FValue := Value;
    Result := True;
  end
  else
  begin
    FErrorMessage := 'Value must be between ' + FormatFloat('0.##', MinValue) + ' and ' + FormatFloat('0.##', MaxValue);
    Result := False;
  end;
end;

function TSingleCvar.GetErrorMessage: AnsiString;
begin
  Result := FErrorMessage;
  FErrorMessage := '';
end;

function TSingleCvar.ParseAndSetValue(Value: String): Boolean;
begin
  Result := SetValue(StrToFloatDef(Value, 0));
end;

function TSingleCvar.ValueAsString: AnsiString;
begin
  Result := FloatToStr(FValue);
end;

class function TBooleanCvar.Add(Name, Description: AnsiString; DefaultValue: Boolean; Flags: TCvarFlags; OnChange: TCallback): TBooleanCvar;
var
  NewCvar: TBooleanCvar;
  CvarName: AnsiString;
begin
  Result := nil;
  CvarName := LowerCase(Name);
  if not CheckIfCvarExists(CvarName) then Exit;

  NewCvar := TBooleanCvar.Create(CvarName, Description, DefaultValue, Flags, OnChange);
  {$IFDEF DEVELOPMENT}
  DumpCvar(NewCvar, DefaultValue);
  {$ENDIF}
  if CVAR_SYNC in Flags then
    CvarsSync.Add(Name, NewCvar);
  Cvars.Add(CvarName, NewCvar);
  Result := NewCvar;
end;

function TBooleanCvar.SetValue(Value: Boolean): Boolean;
begin
  if (CVAR_INITONLY in FFlags) and CvarsInitialized then
  begin
    Result := False;
    FErrorMessage := 'Can be set only at startup';
    Exit;
  end;

  if Assigned(FOnChange) then
  begin
    if not FOnChange(Self, Value) then
    begin
      Result := False;
      Exit;
    end;
  end;

  if Value <> FDefaultValue then
    Include(FFlags, CVAR_MODIFIED)
  else
    Exclude(FFlags, CVAR_MODIFIED);

  {$IFDEF SERVER}
  if Value <> FValue then
    SyncUpdate(True);
  {$ENDIF}

  FValue := Value;
  Result := True;
end;

function TBooleanCvar.GetErrorMessage: AnsiString;
begin
  Result := FErrorMessage;
  FErrorMessage := '';
end;

function TBooleanCvar.ParseAndSetValue(Value: String): Boolean;
begin
  Result := SetValue(StrToBoolDef(Value, False));
end;

function TBooleanCvar.ValueAsString: AnsiString;
begin
  Result := BoolToStr(FValue, '1', '0');
end;

class function TColorCvar.Add(Name, Description: AnsiString; DefaultValue: TColor; Flags: TCvarFlags; OnChange: TCallback): TColorCvar;
var
  NewCvar: TColorCvar;
  CvarName: AnsiString;
begin
  Result := nil;
  CvarName := LowerCase(Name);
  if not CheckIfCvarExists(CvarName) then Exit;

  NewCvar := TColorCvar.Create(CvarName, Description, DefaultValue, Flags, OnChange);
  {$IFDEF DEVELOPMENT}
  DumpCvar(NewCvar, DefaultValue);
  {$ENDIF}
  Cvars.Add(CvarName, NewCvar);
  Result := NewCvar;
end;

function TColorCvar.SetValue(Value: TColor): Boolean;
begin
  if (CVAR_INITONLY in FFlags) and CvarsInitialized then
  begin
    Result := False;
    FErrorMessage := 'Can be set only at startup';
    Exit;
  end;

  if Assigned(FOnChange) then
  begin
    if not FOnChange(Self, Value) then
    begin
      Result := False;
      Exit;
    end;
  end;

  if Value <> FDefaultValue then
    Include(FFlags, CVAR_MODIFIED)
  else
    Exclude(FFlags, CVAR_MODIFIED);

  FValue := Value;
  Result := True;
end;

function TColorCvar.GetErrorMessage: AnsiString;
begin
  Result := FErrorMessage;
  FErrorMessage := '';
end;

function TColorCvar.ParseAndSetValue(Value: String): Boolean;
begin
  Result := SetValue(StrToIntDef(Value, $000000));
end;

function TColorCvar.ValueAsString: AnsiString;
begin
  Result := '$' + HexStr(FValue, 8);
end;

constructor TStringCvar.Create(Name, Description: AnsiString; DefaultValue: AnsiString; Flags: TCvarFlags; OnChange: TCallback; MinLength, MaxLength: Integer);
begin
  inherited Create(Name, Description, DefaultValue, Flags, OnChange);
  Self.FMinLength := MinLength;
  Self.FMaxLength := MaxLength;
end;

class function TStringCvar.Add(Name, Description: AnsiString; DefaultValue: AnsiString; Flags: TCvarFlags; OnChange: TCallback; MinLength, MaxLength: Integer): TStringCvar;
var
  NewCvar: TStringCvar;
  CvarName: AnsiString;
begin
  Result := nil;
  CvarName := LowerCase(Name);
  if not CheckIfCvarExists(CvarName) then Exit;

  NewCvar := TStringCvar.Create(CvarName, Description, DefaultValue, Flags, OnChange, MinLength, MaxLength);
  {$IFDEF DEVELOPMENT}
  DumpCvar(NewCvar, DefaultValue);
  {$ENDIF}
  if CVAR_SYNC in Flags then
    CvarsSync.Add(Name, NewCvar);
  Cvars.Add(CvarName, NewCvar);
  Result := NewCvar;
end;

function TStringCvar.SetValue(Value: AnsiString): Boolean;
var
  Len: Integer;
begin
  if (CVAR_INITONLY in FFlags) and CvarsInitialized then
  begin
    Result := False;
    FErrorMessage := 'Can be set only at startup';
    Exit;
  end;

  Len := Length(Value);
  if (Len >= MinLength) and (Len <= MaxLength) then
  begin
    if Assigned(FOnChange) then
    begin
      if not FOnChange(Self, Value) then
      begin
        Result := False;
        Exit;
      end;
    end;

    if Value <> FDefaultValue then
      Include(FFlags, CVAR_MODIFIED)
    else
      Exclude(FFlags, CVAR_MODIFIED);

    {$IFDEF SERVER}
    if Value <> FValue then
      SyncUpdate(True);
    {$ENDIF}

    FValue := Value;
    Result := True;
  end
  else
  begin
    Result := False;
    FErrorMessage := 'Value must be longer than ' + IntToStr(MinLength) + ' and shorter than ' + IntToStr(MaxLength) + ' characters';
  end;
end;

function TStringCvar.GetErrorMessage: AnsiString;
begin
  Result := FErrorMessage;
  FErrorMessage := '';
end;

function TStringCvar.ParseAndSetValue(Value: String): Boolean;
begin
  Result := SetValue(Value);
end;

function TStringCvar.ValueAsString: AnsiString;
begin
  Result := FValue;
end;

procedure CvarInit();
begin
  Cvars := TFPHashList.Create;
  CvarsSync := TFPHashList.Create;

  log_level := TIntegerCvar.Add('log_level', 'Sets log level. 0 = Off, 1 = Debug/Noteworthy events, 2 = Trace function entries', LEVEL_OFF, [], nil, LEVEL_OFF, LEVEL_TRACE);
  log_enable := TBooleanCvar.Add('log_enable', 'Enables logging to file', True, [], nil);
  log_filesupdate := TIntegerCvar.Add('log_filesupdate', 'How often the log files should be updated in ticks (60 ticks = 1 second)', 3600, [], nil, 0, MaxInt);
  {$IFDEF SERVER}
  log_timestamp := TBooleanCvar.Add('log_timestamp', 'Enables/Disables timestamps in console', False, [CVAR_SERVER], nil);
  {$ENDIF}
  {$IFDEF SERVER}
    {$IFDEF ENABLE_FAE}
  ac_enable := TBooleanCvar.Add('ac_enable', 'Enables/Disables anti-cheat checks via Fae', True, [CVAR_SERVER], nil);
    {$ENDIF}
  {$ENDIF}

  fs_localmount := TBooleanCvar.Add('fs_localmount', 'Mount game directory as game mod', False, [CVAR_CLIENT, CVAR_INITONLY], @CommandLineOnlyChange);
  fs_mod := TStringCvar.Add('fs_mod', 'File name of mod placed in mods directory (without .smod extension)', '', [CVAR_INITONLY], nil, 0, 255);
  fs_portable := TBooleanCvar.Add('fs_portable', 'Enables portable mode', True, [CVAR_CLIENT, CVAR_INITONLY], @CommandLineOnlyChange);
  fs_basepath := TStringCvar.Add('fs_basepath', 'Path to base game directory', '', [CVAR_INITONLY], @CommandLineOnlyChange, 0, 255);
  fs_userpath := TStringCvar.Add('fs_userpath', 'Path to user game directory', '', [CVAR_INITONLY], @CommandLineOnlyChange, 0, 255);

  demo_autorecord := TBooleanCvar.Add('demo_autorecord', 'Auto record demos', False, [], nil);

  // Launcher cvars
  launcher_ipc_enable := TBooleanCvar.Add('launcher_ipc_enable', 'Enables inter-process communication with launcher', False, [CVAR_INITONLY], nil);
  launcher_ipc_port := TIntegerCvar.Add('launcher_ipc_port', 'Port of TCP server used for inter-process communication with launcher', 23093, [CVAR_INITONLY], nil, 0, 65535);
  launcher_ipc_reconnect_rate := TIntegerCvar.Add('launcher_ipc_reconnect_rate', 'How often (in ticks) the game tries to reconnect to launcher', 300, [], nil, 60, 54000);

  {$IFNDEF SERVER}
  // Render Cvars
  r_fullscreen := TIntegerCvar.Add('r_fullscreen', 'Set mode of fullscreen', 0, [CVAR_CLIENT], nil, 0, 2);
  r_weathereffects := TBooleanCvar.Add('r_weathereffects', 'Weather effects', True, [CVAR_CLIENT], nil);
  r_dithering := TBooleanCvar.Add('r_dithering', 'Dithering', False, [CVAR_CLIENT], nil);
  r_swapeffect := TIntegerCvar.Add('r_swapeffect', 'Swap interval, 0 for immediate updates, 1 for updates synchronized with the vertical retrace, -1 for late swap tearing', 0, [CVAR_CLIENT], nil, -1, 1);
  r_compatibility := TBooleanCvar.Add('r_compatibility', 'OpenGL compatibility mode (use fixed pipeline)', False, [CVAR_CLIENT], nil);
  r_texturefilter := TIntegerCvar.Add('r_texturefilter', 'Texture filter (1 = nearest, 2 = linear)', 2, [CVAR_CLIENT], nil, 1, 2);
  r_optimizetextures := TBooleanCvar.Add('r_optimizetextures', 'Optimize textures (for older graphics card)', False, [CVAR_CLIENT], nil);
  r_mipmapping := TBooleanCvar.Add('r_mipmapping', '', True, [CVAR_CLIENT], nil);
  r_mipmapbias := TSingleCvar.Add('r_mipmapbias', '', -0.5, [CVAR_CLIENT], nil, -1.0, 1.0);
  r_glfinish := TBooleanCvar.Add('r_glfinish', '', False, [CVAR_CLIENT], nil);
  r_smoothedges := TBooleanCvar.Add('r_smoothedges', '', False, [CVAR_CLIENT], nil);
  r_scaleinterface := TBooleanCvar.Add('r_scaleinterface', '', True, [CVAR_CLIENT], nil);
  r_maxsparks := TIntegerCvar.Add('r_maxsparks', '', 557, [CVAR_CLIENT], nil, 0, 557);
  r_animations := TBooleanCvar.Add('r_animations', '', True, [CVAR_CLIENT], nil);
  r_renderbackground := TBooleanCvar.Add('r_renderbackground', '', True, [CVAR_CLIENT], nil);
  r_maxfps := TIntegerCvar.Add('r_maxfps', '', 60, [CVAR_CLIENT], nil, 0, 9999);
  r_fpslimit := TBooleanCvar.Add('r_fpslimit', '', True, [CVAR_CLIENT], nil);
  r_resizefilter := TIntegerCvar.Add('r_resizefilter', '', 2, [CVAR_CLIENT], nil, 0, 2);
  r_sleeptime := TIntegerCvar.Add('r_sleeptime', '', 0, [CVAR_CLIENT], nil, 0, 100);
  r_screenwidth := TIntegerCvar.Add('r_screenwidth', '', 0, [CVAR_CLIENT], nil, 0, MaxInt);
  r_screenheight := TIntegerCvar.Add('r_screenheight', '', 0, [CVAR_CLIENT], nil, 0, MaxInt);
  r_renderwidth := TIntegerCvar.Add('r_renderwidth', '', 0, [CVAR_CLIENT, CVAR_INITONLY], nil, 0, MaxInt);
  r_renderheight := TIntegerCvar.Add('r_renderheight', '', 0, [CVAR_CLIENT, CVAR_INITONLY], nil, 0, MaxInt);
  r_forcebg := TBooleanCvar.Add('r_forcebg', '', False, [CVAR_CLIENT], nil);
  r_forcebg_color1 := TColorCvar.Add('r_forcebg_color1', 'Force bg first color', $00FF0000, [CVAR_CLIENT], nil);
  r_forcebg_color2 := TColorCvar.Add('r_forcebg_color2', 'Force bg second color', $00FF0000, [CVAR_CLIENT], nil);
  r_renderui := TBooleanCvar.Add('r_renderui', 'Enables interface rendering', True, [CVAR_CLIENT], nil);
  r_zoom := TSingleCvar.Add('r_zoom', 'Sets rendering zoom (only for spectators)', 0.0, [CVAR_CLIENT], @r_zoomChange, -5.0, 5.0);
  r_msaa := TIntegerCVar.Add('r_msaa', 'Sets the number of samples for anti-aliasing (MSAA).', 0, [CVAR_CLIENT, CVAR_INITONLY], nil, 0, 32);

  // Ui Cvars
  ui_playerindicator := TBooleanCvar.Add('ui_playerindicator', 'Enables player indicator', True, [CVAR_CLIENT], nil);
  ui_minimap_transparency := TIntegerCvar.Add('ui_minimap_transparency', 'Transparency of minimap', 230, [CVAR_CLIENT], nil, 0, 255);
  ui_minimap_posx := TIntegerCvar.Add('ui_minimap_posx', 'Horizontal position of minimap', 285, [CVAR_CLIENT], nil, 0, 640);
  ui_minimap_posy := TIntegerCvar.Add('ui_minimap_posy', 'Vertical position of minimap', 5, [CVAR_CLIENT], nil, 0, 480);
  ui_bonuscolors := TBooleanCvar.Add('ui_bonuscolors', '', True, [CVAR_CLIENT], nil);
  ui_style := TStringCvar.Add('ui_style', '', 'Default', [CVAR_CLIENT], nil, 0, 50);
  ui_status_transparency := TIntegerCvar.Add('ui_status_transparency', 'Transparency of ui', 200, [CVAR_CLIENT], nil, 0, 255);
  ui_console := TBooleanCvar.Add('ui_console', 'Enables chat', True, [CVAR_CLIENT], nil);
  ui_console_length := TIntegerCvar.Add('ui_console_length', 'Sets length of main console', 6, [CVAR_CLIENT], nil, 0, 50);
  ui_killconsole := TBooleanCvar.Add('ui_killconsole', 'Enables kill console', True, [CVAR_CLIENT], nil);
  ui_killconsole_length := TIntegerCvar.Add('ui_killconsole_length', 'Sets length of kill console', 15, [CVAR_CLIENT], nil, 0, 50);
  ui_hidespectators := TBooleanCvar.Add('ui_hidespectators', 'Hides spectators from the fragsmenu', False, [CVAR_CLIENT], nil);
  ui_sniperline := TBooleanCvar.Add('ui_sniperline', 'Draws a line between the player and the cursor', False, [CVAR_CLIENT], nil);

  // Client cvars
  cl_sensitivity := TSingleCvar.Add('cl_sensitivity', 'Mouse sensitivity', 1.0, [CVAR_CLIENT], nil, 0.0, 1.0);
  cl_endscreenshot := TBooleanCvar.Add('cl_endscreenshot', 'Take screenshot when game ends', False, [CVAR_CLIENT], nil);
  cl_actionsnap := TBooleanCvar.Add('cl_actionsnap', 'Enables action snap', False, [CVAR_CLIENT], nil);
  cl_screenshake := TBooleanCvar.Add('cl_screenshake', 'Enables screen shake from enemy fire', True, [CVAR_CLIENT], nil);
  cl_servermods := TBooleanCvar.Add('cl_servermods', 'Enables server mods feature', True, [CVAR_CLIENT], nil);

  {$IFDEF STEAM}
  cl_steam_screenshots := TBooleanCvar.Add('cl_steam_screenshots', 'Use steam screenshots functionality', False, [CVAR_CLIENT], nil);
  cl_voicechat := TBooleanCvar.Add('cl_voicechat', 'Enables voice chat', True, [CVAR_CLIENT], nil);
  fs_workshop_mod := TIntegerCvar.Add('fs_workshop_mod', 'Workshop mod ID', 0, [CVAR_CLIENT], nil, 0, MaxInt);
  fs_workshop_interface := TIntegerCvar.Add('fs_workshop_interface', 'Workshop interface ID', 0, [CVAR_CLIENT], nil, 0, MaxInt);
  {$ENDIF}

  // Player cvars
  cl_player_name := TStringCvar.Add('cl_player_name', 'Player nickname', 'Major', [CVAR_CLIENT], nil, 1, 24);
  cl_player_team := TIntegerCvar.Add('cl_player_team', 'Player team ID', 0, [CVAR_CLIENT], nil, 0, 5);
  cl_player_shirt := TColorCvar.Add('cl_player_shirt', 'Player shirt color', $00304289, [CVAR_CLIENT], nil);
  cl_player_pants := TColorCvar.Add('cl_player_pants', 'Player pants color', $00ff0000, [CVAR_CLIENT], nil);
  cl_player_hair := TColorCvar.Add('cl_player_hair', 'Player hair color', $00000000, [CVAR_CLIENT], nil);
  cl_player_jet := TColorCvar.Add('cl_player_jet', 'Player jet color', $0000008B, [CVAR_CLIENT], nil);
  cl_player_skin := TColorCvar.Add('cl_player_skin', 'Player skin color', $00E6B478, [CVAR_CLIENT], nil);

  cl_player_hairstyle := TIntegerCvar.Add('cl_player_hairstyle', 'Player hair style', 0, [CVAR_CLIENT], nil, 0, 4);
  cl_player_headstyle := TIntegerCvar.Add('cl_player_headstyle', 'Player head style', 0, [CVAR_CLIENT], nil, 0, 2);
  cl_player_chainstyle := TIntegerCvar.Add('cl_player_chainstyle', 'Player chain style', 0, [CVAR_CLIENT], nil, 0, 2);
  cl_player_secwep := TIntegerCvar.Add('cl_player_secwep', 'Player secondary weapon', 1, [CVAR_CLIENT], nil, 0, 3);
  cl_player_wep := TIntegerCvar.Add('cl_player_wep', 'Player primary weapon', 1, [CVAR_CLIENT], @cl_player_wepChange, 1, 10);

  cl_runs := TIntegerCvar.Add('cl_runs', 'Game runs', 0, [CVAR_CLIENT], nil, 0, 32);
  cl_lang := TStringCvar.Add('cl_lang', 'Game language', '', [CVAR_CLIENT, CVAR_INITONLY], nil, 0, 32);


  // Demo cvars
  demo_speed := TSingleCvar.Add('demo_speed', 'Demo speed', 1.0, [CVAR_CLIENT], @demo_speedChange, 0.0, 10.0);
  demo_rate := TIntegerCvar.Add('demo_rate', 'Rate of demo recording', 1, [CVAR_CLIENT], nil, 1, 20);
  demo_showcrosshair := TBooleanCvar.Add('demo_showcrosshair', 'Enables rendering crosshair in demos', True, [CVAR_CLIENT], nil);

  // Sound cvars
  snd_volume := TIntegerCvar.Add('snd_volume', 'Sets sound volume', 50, [CVAR_CLIENT], @snd_volumeChange, 0, 100);
  snd_effects_battle := TBooleanCvar.Add('snd_effects_battle', 'Enables battle sound effects', False, [CVAR_CLIENT], nil);
  snd_effects_explosions := TBooleanCvar.Add('snd_effects_explosions', 'Enables sound explosions effects', False, [CVAR_CLIENT], nil);

  // TODO: Remove
  sv_respawntime := TIntegerCvar.Add('sv_respawntime', 'Respawn time in ticks (60 ticks = 1 second)', 180, [CVAR_SERVER], nil, 0, 9999);
  sv_inf_redaward := TIntegerCvar.Add('sv_inf_redaward', 'Infiltration: Points awarded for a flag capture', 30, [CVAR_SERVER], nil, 0, 9999);
  net_allowdownload := TBooleanCvar.Add('net_allowdownload', 'Enables/Disables file transfers', True, [CVAR_SERVER], nil);

  font_1_name := TStringCvar.Add('font_1_name', 'First font name', 'Play', [CVAR_CLIENT], nil, 0, 100);
  font_1_filename := TStringCvar.Add('font_1_filename', 'First font filename', 'play-regular.ttf', [CVAR_CLIENT], nil, 0, 100);
  font_1_scale := TIntegerCvar.Add('font_1_scalex', 'First font scale', 150, [CVAR_CLIENT], nil, 0, MaxInt);

  font_2_name := TStringCvar.Add('font_2_name', 'Second font name', 'Lucida Console', [CVAR_CLIENT], nil, 0, 100);
  font_2_filename := TStringCvar.Add('font_2_filename', 'Second font filename', 'play-regular.ttf', [CVAR_CLIENT], nil, 0, 100);
  font_2_scale := TIntegerCvar.Add('font_2_scalex', 'Second font scale', 125, [CVAR_CLIENT], nil, 0, MaxInt);

  font_menusize := TIntegerCvar.Add('font_menusize', 'Menu font size', 12, [CVAR_CLIENT], nil, 0, MaxInt);
  font_consolesize := TIntegerCvar.Add('font_consolesize', 'Console font size', 9, [CVAR_CLIENT], nil, 0, MaxInt);
  font_consolesmallsize := TIntegerCvar.Add('font_consolesmallsize', 'Console small font size', 7, [CVAR_CLIENT], nil, 0, MaxInt);
  font_consolelineheight := TSingleCvar.Add('font_consolelineheight', 'Console line height', 1.5, [CVAR_CLIENT], nil, 0, MaxSingle);
  font_bigsize := TIntegerCvar.Add('font_bigsize', 'Big message font size', 28, [CVAR_CLIENT], nil, 0, MaxInt);
  font_weaponmenusize := TIntegerCvar.Add('font_weaponmenusize', 'Weapon menu font size', 8, [CVAR_CLIENT], nil, 0, MaxInt);
  font_killconsolenamespace := TIntegerCvar.Add('font_killconsolenamespace', 'Kill console namespace size', 8, [CVAR_CLIENT], nil, 0, MaxInt);

  {$ELSE}
  // Server cvars
  sv_respawntime := TIntegerCvar.Add('sv_respawntime', 'Respawn time in ticks (60 ticks = 1 second)', 180, [CVAR_SERVER], nil, 0, 9999);
  sv_respawntime_minwave := TIntegerCvar.Add('sv_respawntime_minwave', 'Min wave respawn time in ticks (60 ticks = 1 second)', 120, [CVAR_SERVER], nil, 0, 9999);
  sv_respawntime_maxwave := TIntegerCvar.Add('sv_respawntime_maxwave', 'Max wave respawn time in ticks (60 ticks = 1 second)', 240, [CVAR_SERVER], nil, 0, 9999);

  sv_dm_limit := TIntegerCvar.Add('sv_dm_limit', 'Deathmatch point limit', 30, [CVAR_SERVER], @killlimitChange, 0, 9999);
  sv_pm_limit := TIntegerCvar.Add('sv_pm_limit', 'Pointmatch point limit', 30, [CVAR_SERVER], @killlimitChange, 0, 9999);
  sv_tm_limit := TIntegerCvar.Add('sv_tm_limit', 'Teammatch point limit', 60, [CVAR_SERVER], @killlimitChange, 0, 9999);
  sv_rm_limit := TIntegerCvar.Add('sv_rm_limit', 'Rambomatch point limit', 30, [CVAR_SERVER], @killlimitChange, 0, 9999);

  sv_inf_limit := TIntegerCvar.Add('sv_inf_limit', 'Infiltration point limit', 90, [CVAR_SERVER], @killlimitChange, 0, 9999);
  sv_inf_bluelimit := TIntegerCvar.Add('sv_inf_bluelimit', 'Infiltration: Time for blue team to get points in seconds', 5, [CVAR_SERVER], nil, 0, 9999);

  sv_htf_limit := TIntegerCvar.Add('sv_htf_limit', 'Hold the Flag point limit', 80, [CVAR_SERVER], @killlimitChange, 0, 9999);
  sv_htf_pointstime := TIntegerCvar.Add('sv_htf_pointstime', 'Hold The Flag points time', 5, [CVAR_SERVER], nil, 0, 9999);

  sv_ctf_limit := TIntegerCvar.Add('sv_ctf_limit', 'Capture the Flag point limit', 10, [CVAR_SERVER], @killlimitChange, 0, 9999);

  sv_bonus_frequency := TIntegerCvar.Add('sv_bonus_frequency', 'The interval of bonuses occurring ingame.', 0, [CVAR_SERVER], nil, 0, 5);
  sv_bonus_flamer := TBooleanCvar.Add('sv_bonus_flamer', 'Flamer bonus availability', False, [CVAR_SERVER], nil);
  sv_bonus_predator := TBooleanCvar.Add('sv_bonus_predator', 'Predator bonus availability', False, [CVAR_SERVER], nil);
  sv_bonus_berserker := TBooleanCvar.Add('sv_bonus_berserker', 'Berserker bonus availability', False, [CVAR_SERVER], nil);
  sv_bonus_vest := TBooleanCvar.Add('sv_bonus_vest', 'Bulletproof Vest bonus availability', False, [CVAR_SERVER], nil);
  sv_bonus_cluster := TBooleanCvar.Add('sv_bonus_cluster', 'Cluster Grenades bonus availability', False, [CVAR_SERVER], nil);

  sv_stationaryguns := TBooleanCvar.Add('sv_stationaryguns', 'Enables/disables Stationary Guns ingame.', False, [CVAR_SERVER], nil);

  sv_password := TStringCvar.Add('sv_password', 'Sets game password', '', [CVAR_SERVER], nil, 0, 32);
  sv_adminpassword := TStringCvar.Add('sv_adminpassword', 'Sets admin password', '', [CVAR_SERVER, CVAR_INITONLY], nil, 0, 32);
  sv_maxplayers := TIntegerCvar.Add('sv_maxplayers', 'Max number of players that can play on server', 24, [CVAR_SERVER], nil, 1, 32);
  sv_maxspectators := TIntegerCvar.Add('sv_maxspectators', 'Sets the limit of spectators', 10, [CVAR_SERVER], nil, 0, 32);
  sv_spectatorchat := TBooleanCvar.Add('sv_spectatorchat', 'Enables/disables spectators chat', True, [CVAR_SERVER], nil);
  sv_greeting := TStringCvar.Add('sv_greeting', 'First greeting message', 'Welcome', [CVAR_SERVER], nil, 0, 100);
  sv_greeting2 := TStringCvar.Add('sv_greeting2', 'Second greeting message', '', [CVAR_SERVER], nil, 0, 100);
  sv_greeting3 := TStringCvar.Add('sv_greeting3', 'Third greeting message', '', [CVAR_SERVER], nil, 0, 100);
  sv_minping := TIntegerCvar.Add('sv_minping', 'The minimum ping a player must have to play in your server', 0, [CVAR_SERVER], nil, 0, 9999);
  sv_maxping := TIntegerCvar.Add('sv_maxping', 'The maximum ping a player can have to play in your server', 400, [CVAR_SERVER], nil, 0, 9999);
  sv_votepercent := TIntegerCvar.Add('sv_votepercent', 'Percentage of players in favor of a map/kick vote to let it pass', 60, [CVAR_SERVER], nil, 0, 200);
  sv_lockedmode := TBooleanCvar.Add('sv_lockedmode', 'When Locked Mode is enabled, admins will not be able to type /loadcon, /password or /maxplayers', False, [CVAR_SERVER], nil);
  sv_pidfilename := TStringCvar.Add('sv_pidfilename', 'Sets the Process ID file name', 'opensoldatserver.pid', [CVAR_SERVER], nil, 1, 256);
  sv_maplist := TStringCvar.Add('sv_maplist', 'Sets the name of maplist file', 'mapslist.txt', [CVAR_SERVER], @sv_maplistChange, 1, 256);
  sv_lobby := TBooleanCvar.Add('sv_lobby', 'Enables/Disables registering in lobby', False, [CVAR_SERVER], nil);
  sv_lobbyurl := TStringCvar.Add('sv_lobbyurl', 'URL of the lobby server', 'http://api.soldat.pl:443', [CVAR_SERVER], nil, 1, 256);

  sv_steamonly := TBooleanCvar.Add('sv_steamonly', 'Enables/Disables steam only mode', False, [CVAR_SERVER], nil);

  {$IFDEF STEAM}
  sv_voicechat := TBooleanCvar.Add('sv_voicechat', 'Enables voice chat', True, [CVAR_SERVER], nil);
  sv_voicechat_alltalk := TBooleanCvar.Add('sv_voicechat_alltalk', 'Enables voice chat from enemy team and spectators', False, [CVAR_SERVER], nil);
  sv_setsteamaccount := TStringCvar.Add('sv_setsteamaccount', 'Set game server account token to use for logging in to a persistent game server account', '', [CVAR_SERVER], nil, 32, 32);
  {$ENDIF}

  sv_warnings_flood := TIntegerCvar.Add('sv_warnings_flood', 'How many warnings someone who is flooding the server gets before getting kicked for 20 minutes', 4, [CVAR_SERVER], nil, 0, 100);
  sv_warnings_ping := TIntegerCvar.Add('sv_warnings_ping', 'How many warnings someone who has a ping outside the required values above gets before being kicked for 15 minutes', 10, [CVAR_SERVER], nil, 0, 100);
  sv_warnings_votecheat := TIntegerCvar.Add('sv_warnings_votecheat', 'How many warnings someone gets before determining that they are automatically vote kicked', 8, [CVAR_SERVER], nil, 0, 100);
  sv_warnings_knifecheat := TIntegerCvar.Add('sv_warnings_knifecheat', 'How many warnings someone gets before determining that they are using a knife cheat', 14, [CVAR_SERVER], nil, 0, 100);
  sv_warnings_tk := TIntegerCvar.Add('sv_warnings_tk', 'Number of teamkills that needs to be done before a temporary ban is handed out', 5, [CVAR_SERVER], nil, 0, 100);

  sv_anticheatkick := TBooleanCvar.Add('sv_anticheatkick', 'Enables/Disables anti cheat kicks', False, [CVAR_SERVER], nil);
  sv_punishtk := TBooleanCvar.Add('sv_punishtk', 'Enables/disables the built-in TK punish feature', False, [CVAR_SERVER], nil);
  sv_botbalance := TBooleanCvar.Add('sv_botbalance', 'Whether or not bots should count as players in the team balance', False, [CVAR_SERVER], nil);
  sv_echokills := TBooleanCvar.Add('sv_echokills', 'Echoes kills done to the admin console', False, [CVAR_SERVER], nil);
  sv_antimassflag := TBooleanCvar.Add('sv_antimassflag', '', True, [CVAR_SERVER], nil);
  sv_healthcooldown := TIntegerCvar.Add('sv_healthcooldown', 'Amount of time (in seconds) a player needs to wait before he''s able to pick up a second medikit. Use 0 to disable', 2, [CVAR_SERVER], nil, 0, 100);
  sv_teamcolors := TBooleanCvar.Add('sv_teamcolors', 'Overwrites shirt color in team games', True, [CVAR_SERVER], nil);
  sv_pauseonidle := TBooleanCvar.Add('sv_pauseonidle', 'Pauses the server when no human players are connected', True, [CVAR_SERVER], nil);

  // Network cvars
  net_port := TIntegerCvar.Add('net_port', 'The port your server runs on, and player have to connect to', 23073, [CVAR_SERVER], nil, 0, 65535);
  net_ip := TStringCvar.Add('net_ip', 'Binds server ports to specific ip address', '0.0.0.0', [CVAR_SERVER], nil, 0, 15);
  net_adminip := TStringCvar.Add('net_adminip', 'Binds admin port to specific ip address', '0.0.0.0', [CVAR_SERVER], nil, 0, 15);
  net_lan := TIntegerCvar.Add('net_lan', 'Set to 1 to set server to LAN mode', 0, [CVAR_SERVER], nil, 0, 1);
  net_allowdownload := TBooleanCvar.Add('net_allowdownload', 'Enables/Disables file transfers', True, [CVAR_SERVER], nil);
  net_maxadminconnections := TIntegerCvar.Add('net_maxadminconnections', 'Maximum number of admin connections', 20, [CVAR_SERVER], nil, 0, MaxInt);
  net_rcon_limit := TIntegerCvar.Add('net_rcon_limit', 'Limits the rate of admin connection attempts per second', 5, [CVAR_SERVER], nil, 0, MaxInt);
  net_rcon_burst := TIntegerCvar.Add('net_rcon_burst', 'Limits the burst rate of admin connection attempts per second', 10, [CVAR_SERVER], nil, 0, MaxInt);

  net_floodingpacketslan := TIntegerCvar.Add('net_floodingpacketslan', 'When running on a LAN, controls how many packets should be considered flooding', 80, [CVAR_SERVER], nil, 0, 100);
  net_floodingpacketsinternet := TIntegerCvar.Add('net_floodingpacketsinternet', 'When running on the Internet, controls how many packets should be considered flooding', 42, [CVAR_SERVER], nil, 0, 100);

  net_t1_snapshot := TIntegerCvar.Add('net_t1_snapshot', 'How often to send sprite snapshot packets on the internet in ticks (60 ticks = 1 second)', 35, [CVAR_SERVER], nil, 1, 1000);
  net_t1_majorsnapshot := TIntegerCvar.Add('net_t1_majorsnapshot', 'How often to send major sprite snapshot packets on the internet in ticks (60 ticks = 1 second)', 19, [CVAR_SERVER], nil, 1, 1000);
  net_t1_deadsnapshot := TIntegerCvar.Add('net_t1_deadsnapshot', 'How often to send dead sprite snapshot packets on the internet in ticks (60 ticks = 1 second)', 50, [CVAR_SERVER], nil, 1, 1000);
  net_t1_heartbeat := TIntegerCvar.Add('net_t1_heartbeat', 'How often to send heartbeat packets on the internet in ticks (60 ticks = 1 second)', 135, [CVAR_SERVER], nil, 1, 1000);
  net_t1_delta := TIntegerCvar.Add('net_t1_delta', 'How often to send bot sprite deltas on the internet in ticks (60 ticks = 1 second)', 4, [CVAR_SERVER], nil, 1, 1000);
  net_t1_ping := TIntegerCvar.Add('net_t1_ping', 'How often to send ping packets on the internet in ticks (60 ticks = 1 second)', 21, [CVAR_SERVER], nil, 1, 1000);
  net_t1_thingsnapshot := TIntegerCvar.Add('net_t1_thingsnapshot', 'How often to send thing snapshot packets on the internet in ticks (60 ticks = 1 second)', 31, [CVAR_SERVER], nil, 1, 1000);

  // Bots cvars
  bots_random_noteam := TIntegerCvar.Add('bots_random_noteam', 'Number of bots in DM, PM and RM modes', 0, [CVAR_SERVER], nil, 0, 32);
  bots_random_alpha := TIntegerCvar.Add('bots_random_alpha', 'Number of bots on Alpha in INF, CTF, HTF, PM and TM', 0, [CVAR_SERVER], nil, 0, 32);
  bots_random_bravo := TIntegerCvar.Add('bots_random_bravo', 'Number of bots on Bravo in INF, CTF, HTF, PM and TM', 0, [CVAR_SERVER], nil, 0, 32);
  bots_random_charlie := TIntegerCvar.Add('bots_random_charlie', 'Number of bots on Charlie in INF, CTF, HTF, PM and TM.', 0, [CVAR_SERVER], nil, 0, 32);
  bots_random_delta := TIntegerCvar.Add('bots_random_delta', 'Number of bots on Delta in INF, CTF, HTF, PM and TM', 0, [CVAR_SERVER], nil, 0, 32);
  bots_difficulty := TIntegerCvar.Add('bots_difficulty', 'Sets the skill level of the bots: 300=stupid, 200=poor, 100=normal, 50=hard, 10=impossible', 100, [CVAR_SERVER], nil, 0, 300);
  bots_chat := TBooleanCvar.Add('bots_chat', 'Enables/disables bots chatting', True, [CVAR_SERVER], nil);

  // ScriptCore cvars
  sc_enable := TBooleanCvar.Add('sc_enable', 'Enables/Disables scripting', True, [CVAR_SERVER, CVAR_INITONLY], nil);
  sc_onscriptcrash := TStringCvar.Add('sc_onscriptcrash', 'What action to take when a script crashes. Available parameters are recompile, shutdown, ignore and disable', 'ignore', [CVAR_SERVER, CVAR_INITONLY], nil, 0, 10);
  sc_safemode := TBooleanCvar.Add('sc_safemode', 'Enables/Disables Safe Mode for Scripts', True, [CVAR_SERVER, CVAR_INITONLY], nil);
  sc_allowdlls := TBooleanCvar.Add('sc_allowdlls', 'Enables/Disables loading external dlls', False, [CVAR_SERVER, CVAR_INITONLY], nil);
  sc_sandboxed := TIntegerCvar.Add('sc_sandboxed', 'ScriptCore global sandbox level ', 2, [CVAR_SERVER, CVAR_INITONLY], nil, 0, 2);
  sc_defines := TStringCvar.Add('sc_defines', 'ScriptCore global defines (comma separated)', '', [CVAR_SERVER, CVAR_INITONLY], nil, 0, 255);
  sc_searchpaths := TStringCvar.Add('sc_searchpaths', 'ScriptCore global search paths (comma separated)', '', [CVAR_SERVER, CVAR_INITONLY], nil, 0, 255);

  // Fileserver cvars
  fileserver_enable := TBooleanCvar.Add('fileserver_enable', 'Enables/Disables built-in fileserver', True, [CVAR_SERVER, CVAR_INITONLY], nil);
  fileserver_port := TIntegerCvar.Add('fileserver_port', 'Binds fileserver to specific port', 0, [CVAR_SERVER, CVAR_INITONLY], nil, 0, 65535);
  fileserver_ip := TStringCvar.Add('fileserver_ip', 'Binds fileserver to specific ip address', '0.0.0.0', [CVAR_SERVER, CVAR_INITONLY], nil, 0, 15);
  fileserver_maxconnections := TIntegerCvar.Add('fileserver_maxconnections', 'Maximum number of simultaneous file transfer connections', 32, [CVAR_SERVER], nil, 0, 100);

  {$ENDIF}

  // Sync vars (todo);

  sv_gamemode := TIntegerCvar.Add('sv_gamemode', 'Sets the gamemode', GAMESTYLE_CTF, [CVAR_SERVER, CVAR_SYNC,CVAR_SERVER_INITONLY], nil, 0, 6); // Restart server
  sv_friendlyfire := TBooleanCvar.Add('sv_friendlyfire', 'Enables friendly fire', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_timelimit := TIntegerCvar.Add('sv_timelimit', 'Time limit of map', 36000, [CVAR_SERVER, CVAR_SYNC], nil, 0, MaxInt);
  sv_maxgrenades := TIntegerCvar.Add('sv_maxgrenades', 'Sets the max number of grenades a player can carry', 2, [CVAR_SERVER, CVAR_SYNC], nil, 0, 5);
  sv_bullettime := TBooleanCvar.Add('sv_bullettime', 'Enables/disables the Bullet Time effect on server', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_sniperline := TBooleanCvar.Add('sv_sniperline', 'Enables/disables the Sniper Line on server', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_balanceteams := TBooleanCvar.Add('sv_balanceteams', 'Enables/disables team balancing', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_guns_collide := TBooleanCvar.Add('sv_guns_collide', 'Enables colliding guns', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_kits_collide := TBooleanCvar.Add('sv_kits_collide', 'Enables colliding kits', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_survivalmode := TBooleanCvar.Add('sv_survivalmode', 'Enables survival mode', False, [CVAR_SERVER, CVAR_SYNC,CVAR_SERVER_INITONLY], nil); // Restart server
  sv_survivalmode_antispy := TBooleanCvar.Add('sv_survivalmode_antispy', 'Enables anti spy chat in survival mode', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_survivalmode_clearweapons := TBooleanCvar.Add('sv_survivalmode_clearweapons', 'Clear weapons in between survivalmode rounds', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_realisticmode := TBooleanCvar.Add('sv_realisticmode', 'Enables realistic mode', False, [CVAR_SERVER, CVAR_SYNC,CVAR_SERVER_INITONLY], nil); // Restart server
  sv_advancemode := TBooleanCvar.Add('sv_advancemode', 'Enables advance mode', False, [CVAR_SERVER, CVAR_SYNC,CVAR_SERVER_INITONLY], nil); // Restart server
  sv_advancemode_amount := TIntegerCvar.Add('sv_advancemode_amount', 'Number of kills required in Advance Mode to gain a weapon.', 2, [CVAR_SERVER, CVAR_SYNC], nil, 1, 9999);
  sv_minimap_locations := TBooleanCvar.Add('sv_minimap_locations', 'Enables/disables drawing player and object location indicators on minimap', True, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_advancedspectator := TBooleanCvar.Add('sv_advancedspectator', 'Enables/disables advanced spectator mode', True, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_radio := TBooleanCvar.Add('sv_radio', 'Enables/disables radio chat', False, [CVAR_SERVER, CVAR_SYNC], nil);
  sv_info := TStringCvar.Add('sv_info', 'A website or e-mail address, or any other short text describing your server', '', [CVAR_SERVER, CVAR_SYNC], nil, 0, 60);
  sv_gravity := TSingleCvar.Add('sv_gravity', 'Gravity', 0.06, [CVAR_SERVER, CVAR_SYNC], @sv_gravityChange, MinSingle, MaxSingle);
  sv_hostname := TStringCvar.Add('sv_hostname', 'Name of the server', 'OpenSoldat Server', [CVAR_SERVER, CVAR_SYNC], nil, 0, 24);
  sv_website := TStringCvar.Add('sv_website', 'Server website', '', [CVAR_SERVER, CVAR_SYNC], nil, 0, 255);

  sv_killlimit := TIntegerCvar.Add('sv_killlimit', 'Game point limit', 10, [CVAR_SERVER, CVAR_SYNC], nil, 0, 9999);
  sv_downloadurl := TStringCvar.Add('sv_downloadurl', 'URL from which clients can download missing assets', '', [CVAR_SERVER, CVAR_SYNC], nil, 0, 100);
  sv_pure := TBooleanCvar.Add('sv_pure', 'Requires clients to use the same game files (.smod) as the server', False, [CVAR_SERVER, CVAR_SYNC], nil);

  CommandInit();
end;

procedure CvarCleanup();
var
  i: Integer;
begin
  FreeAndNil(CvarsSync);
  if Cvars <> Nil then
    for i := 0 to Cvars.Count - 1 do
      TCvarBase(Cvars[i]).Free;
  FreeAndNil(Cvars);
end;

end.

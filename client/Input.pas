{*************************************************************}
{                                                             }
{       Input Unit for OpenSoldat                             }
{                                                             }
{       Copyright (c) 2016      Paweł Drzazga                 }
{       Copyright (c) 2020-2023 OpenSoldat contributors       }
{                                                             }
{*************************************************************}

unit Input;

interface

uses
  // System units
  Classes,
  StrUtils,
  SysUtils,

  // Library units
  SDL2,

  // Project units
  Constants,
  Weapons;


type
  {$scopedenums on}
  TAction = (None, Left, Right, Jump, Crouch, Fire, Jet, Reload, ChangeWeapon, VoiceChat,
    ThrowGrenade, DropWeapon, Prone, FlagThrow, StatsMenu, GameStats,
    MiniMap, PlayerName, FragsList, SniperLine, Radio, RecordDemo, VolumeUp, VolumeDown,
    MouseSensitivityUp, MouseSensitivityDown, Cmd, Chat, TeamChat, Snap, Weapons, Bind);

  TKeyMods = UInt32;

  PBind = ^TBind;
  TBind = record
    Action: TAction;
    keyId: LongWord;
    keyMod: TKeyMods;
    Command: WideString;
    Specificity: UInt32; // Used to sort/prioritize binds.
  end;

const
  // Following modifiers, up to KM_RALT, are compatible with SDL_Keymod.
  KM_NONE   = $00000;
  KM_LSHIFT = $00001;
  KM_RSHIFT = $00002;
  KM_LCTRL  = $00040;
  KM_RCTRL  = $00080;
  KM_LALT   = $00100;
  KM_RALT   = $00200;
  // These are our own custom keymod flags, to support either left or right
  // ctrl/shift/alt.
  KM_CTRL   = $10000;
  KM_ALT    = $20000;
  KM_SHIFT  = $40000;

  KM_ALL    = KM_LSHIFT or KM_RSHIFT or KM_LCTRL or KM_RCTRL or KM_LALT or KM_RALT or KM_CTRL or KM_ALT or KM_SHIFT;

  // Custom "KeyIDs" to allow for mousewheel controls.
  KEYID_MOUSEWHEEL_UP   = 513;
  KEYID_MOUSEWHEEL_DOWN = 514;

function  BindKey(key, action, command: string; KeyMods: TKeyMods): Boolean;
function  FindKeyBind(KeyMods: TKeyMods; KeyCode: TSDL_ScanCode; Exact: Boolean = False): PBind;
function  KeyModsMatch(BindKeyMods, KeyMods: TKeyMods; Exclusive: Boolean = False): Boolean;
procedure StartInput;
procedure UnbindAll;

var
  KeyStatus: array [0..514] of Boolean;
  Binds: array of TBind;
  GameWindow : PSDL_Window;
  MouseWheelUpCounter: Integer = 0;
  MouseWheelDownCounter: Integer = 0;


implementation

uses
  // System units
  TypInfo,

  // Helper units
  TraceLog,

  // Project units
  {$IFDEF SERVER}
  Server,
  {$ELSE}
  Client,
  {$ENDIF}
  GameRendering;


// Used to determine the order binds are tried in, so for example
// "ctrl+shift+x" is run instead of "x" if "ctrl+shift" are held.
function SpecificityScore(KeyMods: TKeyMods): UInt32;
begin
  if (KeyMods and KM_SHIFT) = KM_SHIFT then
    KeyMods := KeyMods and not (KM_LSHIFT or KM_RSHIFT);
  if (KeyMods and KM_ALT) = KM_ALT then
    KeyMods := KeyMods and not (KM_LALT or KM_RALT);
  if (KeyMods and KM_CTRL) = KM_CTRL then
    KeyMods := KeyMods and not (KM_LCTRL or KM_RCTRL);

  // Most important component is number of modifier keys.
  Result := PopCnt(KeyMods) shl 19;

  // One modifier key is more specific than either modifier key.
  Result := Result or ((KeyMods and (KM_CTRL or KM_ALT or KM_SHIFT)) shr 16);
  Result := Result or ((KeyMods and not (KM_CTRL or KM_ALT or KM_SHIFT)) shl 4);
end;

function BindKey(key, action, command: string; KeyMods: TKeyMods): Boolean;
var
  b: TBind;
  id: Integer;
  i: Integer;
begin
  Result := False;

  // Clear unused modifier bits...
  KeyMods := KeyMods and KM_ALL;

  if key = 'mousewheel up' then
    b.keyId := KEYID_MOUSEWHEEL_UP
  else if key = 'mousewheel down' then
    b.keyId := KEYID_MOUSEWHEEL_DOWN
  else if AnsiContainsStr(key, 'mouse') = true then
    b.keyId := 300 + StrToIntDef(Copy(Key, 6, 1), 1)
  else
    b.keyId := SDL_GetScancodeFromName(Pchar(key));

  if b.keyId = 0 then
  begin
    Debug('[INPUT] Key ' + key + ' is invalid');
    Exit;
  end;

  if Assigned(FindKeyBind(KeyMods, b.keyId, True)) then
  begin
    Debug('[INPUT] Key ' + key + ' is already binded');
    Exit;
  end;

  for i := Ord(Low(TAction)) to Ord(High(TAction)) do
  begin
     if LowerCase(action) = '+' + LowerCase(GetEnumName(TypeInfo(TAction), Ord(i))) then
       b.Action := TAction(Ord(i));
  end;

  b.Command := WideString(Command);
  b.keyMod := KeyMods;
  b.Specificity := SpecificityScore(b.keyMod);

  SetLength(Binds, Length(Binds) + 1);
  id := High(Binds);
  for i := Low(Binds) to High(Binds) - 1 do
    if Binds[i].Specificity <= b.Specificity then
    begin
      id := i;
      Break;
    end;

  for i := High(Binds) - 1 downto id do
    Binds[i + 1] := Binds[i];
  Binds[id] := b;

  Debug('[INPUT] BindKey: Key: ' + key + ' (' +
        IntToStr(Binds[id].keyId) + '), Mod: ' + IntToStr(Binds[id].keyMod) +
        ' Command: ' + command);
  Result := True;
end;

function FindKeyBind(KeyMods: TKeyMods; KeyCode: TSDL_ScanCode; Exact: Boolean = False): PBind;
var
  i: Integer;
begin
  Result := nil;

  // Clear unused modifier bits...
  KeyMods := KeyMods and KM_ALL;

  for i := Low(Binds) to High(Binds) do
  begin
    if (Binds[i].KeyId = KeyCode) and Exact and (Binds[i].keymod = KeyMods) then
    begin
      Result := @Binds[i];
      Exit;
    end;

    if (not Exact) and (Binds[i].KeyId = KeyCode) and KeyModsMatch(Binds[i].keymod, KeyMods, False) then
    begin
      Result := @Binds[i];
      Exit;
    end;
  end;
end;

function KeyModsMatch(BindKeyMods, KeyMods: TKeyMods; Exclusive: Boolean = False): Boolean;
begin
  Result := True;

  // Clear unused modifier bits...
  BindKeyMods := BindKeyMods and KM_ALL;
  KeyMods := KeyMods and KM_ALL;

  if (BindKeyMods and KM_SHIFT) = KM_SHIFT then
  begin
    if (KeyMods and (KM_LSHIFT or KM_RSHIFT)) = 0 then
    begin
      Result := False;
      Exit;
    end;

    BindKeyMods := BindKeyMods or KM_LSHIFT or KM_RSHIFT;
    KeyMods := KeyMods or KM_LSHIFT or KM_RSHIFT;
  end;

  if (BindKeyMods and KM_CTRL) = KM_CTRL then
  begin
    if (KeyMods and (KM_LCTRL or KM_RCTRL)) = 0 then
    begin
      Result := False;
      Exit;
    end;

    BindKeyMods := BindKeyMods or KM_LCTRL or KM_RCTRL;
    KeyMods := KeyMods or KM_LCTRL or KM_RCTRL;
  end;

  if (BindKeyMods and KM_ALT) = KM_ALT then
  begin
    if (KeyMods and (KM_LALT or KM_RALT)) = 0 then
    begin
      Result := False;
      Exit;
    end;

    BindKeyMods := BindKeyMods or KM_LALT or KM_RALT;
    KeyMods := KeyMods or KM_LALT or KM_RALT;
  end;

  if Exclusive then
    Result := Result and ((BindKeyMods and $FFFF) = KeyMods)
  else
    Result := Result and ((Keymods and BindKeyMods) = (BindKeyMods and $ffff));
end;

procedure UnbindAll;
begin
  SetLength(Binds, 0);
end;

procedure StartInput;
begin
  SDL_SetRelativeMouseMode(SDL_TRUE);
  SDL_StopTextInput;
end;

end.

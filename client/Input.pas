{*******************************************************}
{                                                       }
{       Input Unit for OPENSOLDAT                       }
{                                                       }
{       Copyright (c) 2016 Paweł Drzazga                }
{                                                       }
{*******************************************************}

unit Input;

interface

uses
  sysutils, Constants, Weapons, Classes, Strutils, SDL2;

type
  {$scopedenums on}
  TAction = (None, Left, Right, Jump, Crouch, Fire, Jet, Reload, ChangeWeapon, VoiceChat,
    ThrowGrenade, DropWeapon, Prone, FlagThrow, StatsMenu, GameStats,
    MiniMap, PlayerName, FragsList, SniperLine, Radio, RecordDemo, VolumeUp, VolumeDown,
    MouseSensitivityUp, MouseSensitivityDown, Cmd, Chat, TeamChat, Snap, Weapons, Bind);

  PBind = ^TBind;
  TBind = record
    Action: TAction;
    keyId: LongWord;
    keyMod: Word;
    Command: WideString;
  end;

const
  KM_NONE  = 0;
  KM_ALT   = 1 shl 0;
  KM_CTRL  = 1 shl 1;
  KM_SHIFT = 1 shl 2;

function BindKey(key, action, command: string; Modifier: Word): Boolean;
function FindKeyBind(KeyMods: Word; KeyCode: TSDL_ScanCode): PBind;
procedure StartInput;
procedure UnbindAll;

var
  KeyStatus: array [0..512] of Boolean;
  Binds: array of TBind;
  GameWindow : PSDL_Window;

implementation

uses {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} GameRendering, typinfo, TraceLog;

function BindKey(key, action, command: string; Modifier: Word): Boolean;
var
  id: Integer;
  i: Integer;
begin
  Result := False;
  if Length(Binds) = 0 then
    SetLength(Binds, 1);
  id := Length(Binds) - 1;

  if AnsiContainsStr(key, 'mouse') = true then
    Binds[id].keyId := 300 + StrToIntDef(Copy(Key, 6, 1), 1)
  else
    Binds[id].keyId := SDL_GetScancodeFromName(Pchar(key));

  if Binds[id].keyId = 0 then
  begin
    Debug('[INPUT] Key ' + key + ' is invalid');
    Exit;
  end;

  if Assigned(FindKeyBind(Modifier, Binds[id].keyId)) then
  begin
    Debug('[INPUT] Key ' + key + ' is already binded');
    Exit;
  end;

  for i := Ord(Low(TAction)) to Ord(High(TAction)) do
  begin
     if LowerCase(action) = '+' + LowerCase(GetEnumName(TypeInfo(TAction), Ord(i))) then
       Binds[id].Action := TAction(Ord(i));
  end;

  Binds[id].Command := WideString(Command);
  Binds[id].keyMod := Modifier;

  Debug('[INPUT] BindKey id: ' + IntToStr(id) + ' Key: ' + key + ' (' +
        IntToStr(Binds[id].keyId) + '), Mod: ' + IntToStr(Binds[id].keyMod) +
        ' Command: ' + command);
  SetLength(Binds, Length(Binds) + 1);
  Result := True;
end;

function FindKeyBind(KeyMods: Word; KeyCode: TSDL_ScanCode): PBind;
var
  i: Integer;
begin
  Result := nil;

  for i := Low(Binds) to High(Binds) - 1 do
  begin
    if (Binds[i].KeyId = KeyCode) and ((Binds[i].keyMod and KeyMods <> 0) or
      (Binds[i].keyMod = KeyMods)) then
    begin
      Result := @Binds[i];
      Exit;
    end;
  end;
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

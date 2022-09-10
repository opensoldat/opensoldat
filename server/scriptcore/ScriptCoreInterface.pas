{*******************************************************}
{                                                       }
{       ScriptCoreInterface unit for OPENSOLDAT         }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{ ScriptCoreInterface unit holds all the old            }
{ scriptcore's API functions.                           }
{ I've put them all here to keep ScriptCore class more  }
{ or less tidy.                                         }
{                                                       }
{*******************************************************}

unit ScriptCoreInterface;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  // general
  Classes, SysUtils, Util,
  // other
  Vector,
  // pascal script
  uPSComponent, uPSRuntime,
  // OpenSoldat
  PascalCore;

// Scriptcore functions
function ReadFile(FileName: String; UseEncryption: Boolean): String;
function SRGB(R, G, B: Byte): TColor;
function ExistsFile(FileName: String): Boolean;
//function ScriptGetURL(URL: String): String;
function Decrypt(const S: Ansistring; Key: Word): Ansistring;
function Encrypt(const S: Ansistring; Key: Word): Ansistring;
function MySplitStr(const Source: String; const delimiter: String): TStringArray;
function SafeSplit(Source, Delimiter: String): TStringArray;
procedure DrawText(Id: Byte; Text: String; Delay: Integer; Colour: Longint;
  Scale: Single; X, Y: Integer);

function SetVal(Script: TPSScript; Variable: String; Value: Integer): Boolean;
procedure SetVariables(ScriptCore: TPascalCore; Script: TPSScript);
procedure RegisterFunctions(ScriptCore: TPascalCore; Script: TPSScript);

function SArrayHigh(Arr: array of String): Integer;
procedure WriteLn(const Data: String);
function MyExp(Number: Extended): Extended;
function SRand(Min, Max: Integer): Integer;
function SCreateBullet(X, Y, VelX, VelY, HitM: Single; sStyle, Owner: Byte): Integer;
procedure SGiveBonus(Id, Bonus: Byte);
procedure ServerModifier(var Name: String; Value: Single);
function SRayCast(P1X, P1Y, P2X, P2Y: Single; var Distance: Single;
  MaxDist: Single): Boolean;
function SRayCastEx(P1X, P1Y, P2X, P2Y: Single; var Distance: Single;
  MaxDist: Single; Player, Flag, Bullet, Collider: Boolean; Team: Byte): Boolean;
procedure SetTeamScore(Team: Byte; Score: Integer);
function GetObjectStat(Id: Byte; ScriptStat: String): Variant;
function CrossFunc(const Params: array of Variant; ProcName: String): Variant;
procedure ResetTarget(Id: Byte);
function GetKeyPress(Id: Byte; ScriptKey: String): Boolean;
function GetSpawnStat(Id: Byte; ScriptStat: String): Variant;
procedure SetSpawnStat(Id: Byte; ScriptStat: String; Value: Variant);
procedure SBotChat(Id: Byte; Text: String);
function RegExpMatch(RegularExpression, Source: String): Boolean;
function RegExpReplace(RegularExpression, Source, ReplaceText: String;
  UseSubstitution: Boolean): String;
function MyRoundTo(const AValue: Extended; ADigit: Integer): Extended;
function MyDate(Format: String): String;
procedure DrawTextEx(Id, Num: Byte; Text: String; Delay: Integer;
  Colour: Longint; Scale: Single; X, Y: Integer);
function GetSystem: String;
function SWeaponNameByNum(Num: Integer): string;
function SGetTickCount: Cardinal;
function IDToIP(Id: Byte): String;
function IDToHW(Id: Byte): String;
procedure SKickPlayer(Num: Byte);
procedure MyStartVoteKick(Target: Byte; Reason: String);
procedure MyStartVoteMap(Mapname, Reason: String);
procedure SBanPlayer(Num: Byte; Time: Integer);
procedure SBanPlayerReason(Num: Byte; Time: Integer; Reason: String);
function SCommand(Cmd: String): Variant;
procedure DoDamage(Id: Byte; Damage: Integer);
procedure DoDamageBy(Id, Shooter: Byte; Damage: Integer);
procedure SForceWeapon(A, B, C, D: Byte);
procedure SForceWeaponEx(A, B, C, D, E: Byte);
procedure SetScore(Id: Byte; Score: Integer);
function SCreateThing(X, Y: Single; BType: Byte): Integer;
procedure KillThing(Id: Integer);
procedure GetPlayerXY(Id: Byte; var X, Y: Single);
function GetPlayerStat(Id: Byte; ScriptStat: String): Variant;
procedure GetFlagsXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: Single);
procedure GetFlagsSpawnXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: Single);
procedure SSLeep(Milliseconds: Cardinal);
procedure SSendStringToPlayer(Id: Byte; Text: String);
function MyReadFile(FileName: String): String;
function WriteFile(FileName: String; SData: String): Boolean;
function AppendFile(FileName: String; Data: String): Boolean;
function shell_exec(Command: String): Integer;
function ReadINI(FileName, Section, Value, Default: string): string;

implementation

uses
  {$IFNDEF MSWINDOWS}
    Unix,
  {$ELSE}
    ShellApi,
  {$ENDIF}
  Net, NetworkUtils, NetworkServerFunctions, NetworkServerThing,
  NetworkServerMessages, NetworkServerGame,
  Server, Game, Calc, IniFiles, Sprites, fpmasks,
  Math, ScriptDispatcher, Weapons, Things, Command, Bullets,
  TraceLog, Constants, strutils, Version, RegExpr, Cvar, ServerHelper {$IFDEF RCON}, Rcon{$ENDIF};

function ExistsFile(FileName: String): Boolean;
var
  Name: String;
begin
  Name := AnsiReplaceStr(FileName, '..', '');
  Result := FileExists(UserDirectory + Name);
end;

{function ScriptGetURL(Url: String): String;
begin
  if ScrptDispatcher.SafeMode then
  begin
    Result := '';
    MainConsole.Console('[Error] GetURL: Function Disabled (SafeMode = ON)',
      DEBUG_MESSAGE_COLOR);
    Exit;
  end;

  Result := GetURL(Url);
end;}

function MySplitStr(const Source: String; const Delimiter: String): TStringArray;
begin
  Result := SafeSplit(Source, Delimiter);
end;

function SafeSplit(Source, Delimiter: String): TStringArray;
var
  Count: Integer;
  Position: Integer;
  Len: Integer;
  Temp: String;
  Split, Ret: TStringArray;
begin
  Ret := Default(TStringArray);
  Split := Default(TStringArray);
  Trace('|| Split();');

  Temp := Source;
  Count := 0;
  Len := Length(Delimiter) - 1;
  repeat
    Position := Pos(Delimiter, Temp);
    if Position = 0 then
    begin
      Break;
    end
    else
    begin
      Inc(Count, 1);
      SetLength(Split, Count);
      Split[Count - 1] := Copy(Temp, 1, Position - 1);
      Delete(Temp, 1, Position + Len);
    end;
  until False;
  if Length(Temp) > 0 then
  begin
    Inc(Count, 1);
    SetLength(Split, Count);
    Split[Count - 1] := Temp;
  end;

  SetLength(Ret, Count + 2);

  for Position := 0 to Count - 1 do
  begin
    Ret[Position] := Split[Position];
  end;
  Result := Ret;
end;

procedure DrawTextEx(Id, Num: Byte; Text: String; Delay: Integer;
  Colour: Longint; Scale: Single; X, Y: Integer);
var
  SCRX, SCRY: Single;
begin
  if Id > MAX_PLAYERS then
    Exit;

  // X = LEFT RIGHT
  // Y = UP DOWN
  // Draw text on the screen of ALL Players
  SCRX := X;  // (X / 320) - 1;
  SCRY := Y;  // (Y / 160);
  // Scale := Scale - 0.80;
  ServerSendSpecialMessage(Text, 1, Num, Delay, Scale, Colour, SCRX, SCRY, Id);
end;

procedure DrawText(Id: Byte; Text: String; Delay: Integer; Colour: Longint;
  Scale: Single; X, Y: Integer);
begin
  DrawTextEx(Id, 1, Text, Delay, Colour, Scale, X, Y);
end;

function ReadFile(FileName: String; UseEncryption: Boolean): String;
var
  VarFile: TextFile;
  Buff: String;
  FoundKey: String;
  Name: String;
  TempBuff, TempStr: String;
  NumLines, NumLinesEnc: Integer;
begin
  Result := '';
  Trace('|| ScriptCore.ReadFile(' + FileName + ');');

  Name := AnsiReplaceStr(FileName, '..', '');
  assignfile(VarFile, UserDirectory + Name);
  FileMode := fmOpenRead;
  Reset(VarFile);
  NumLines := 0;
  NumLinesEnc := 0;
  Buff := '';
  FoundKey := '';
  // S.Text := AnsiReplaceStr(S.Text, 'Ø', chr(13) + chr(10));
  if not EOF(VarFile) and (ProgReady = True) then
    repeat
      ReadLn(VarFile, TempBuff);
      // TempBuff := AnsiReplaceStr(TempBuff, 'Ø', chr(13) + chr(10));

      if (Copy(TempBuff, 1, 1) = '$') and (UseEncryption) then
      begin
        TempBuff := Decrypt(Copy(TempBuff, 2, Length(TempBuff) - 1),
          StrToInt(FoundKey));
        NumLinesEnc := NumLinesEnc + 1;
      end;

      if (Copy(TempBuff, 1, 1) = '%') and (UseEncryption) then
      begin
        if StrToInt(Decrypt(Copy(GetPiece(TempBuff, '@', 1), 2,
          Length(GetPiece(TempBuff, '@', 1)) - 1), 54931)) > 0 then
        begin
          FoundKey := Decrypt(Copy(GetPiece(TempBuff, '@', 1), 2,
            Length(GetPiece(TempBuff, '@', 1)) - 1), 54931);
          // WriteLn('Key: ' + FoundKey);
          NumLines := StrToInt(Decrypt(Copy(GetPiece(TempBuff, '@', 2),
            1, Length(GetPiece(TempBuff, '@', 2))), StrToInt(FoundKey)));
          // WriteLn('Num: ' + inttostr(NumLines));
          TempBuff := '';
        end;
      end;

      if (UseEncryption) and (Copy(TempBuff, 1, 10) = '{$VERSION ') then
      begin
        // Compiler version check
        TempStr := AnsiReplaceStr(Copy(TempBuff, 11, 5), '.', '');
        if TempStr > AnsiReplaceStr(DEDVERSION, '.', '') then
        begin
          WriteLn('');
          MainConsole.Console(' [*] Script Error: Script Version: ' +
            Copy(TempBuff, 11, 5) + ' Server Version: ' + DEDVERSION,
            SERVER_MESSAGE_COLOR);
          WriteLn('     Please upgrade to ' + Copy(TempBuff, 11, 5) +
            ' to use this script. Server must now shutdown.');
          WriteLn('');
          ProgReady := False;
          Exit;
        end;
      end;
      // if (UseEncryption) and
      // (TempBuff = 'procedure OnCommand(Id: integer; Text: string);') then
      // begin
      // MainConsole.Console(
      // ' [*] ERROR: You are using an invalid Script (For Soldat 1.3.1).',
      // DEBUG_MESSAGE_COLOR);
      // Sleep(1000);
      // end;

      TempBuff := TempBuff + Chr(13) + Chr(10);
      Buff := Buff + TempBuff;
    until (EOF(VarFile));

  CloseFile(VarFile);
  Result := Buff;

  if (((NumLines - NumLinesEnc) > 1) or ((NumLines - NumLinesEnc) < -1)) and
    (FoundKey <> '') then
  begin
    sc_enable.SetValue(False);
  end;
end;

function SRGB(R, G, B: Byte): TColor;
begin
  Result := (B or (G shl 8) or (R shl 16));
end;

// ENCRYPTION CODE

const
  C1 = 52845;
  C2 = 22719;

function Decode(const S: Ansistring): Ansistring;
const
  Map: array[Char] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 0, 63, 52, 53,
    54, 55, 56, 57, 58, 59, 60, 61, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2,
    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 0, 0, 0, 0, 0, 0, 26, 27, 28, 29, 30,
    31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
    46, 47, 48, 49, 50, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0);
var
  I: Longint;
begin
  Result := '';
  case Length(S) of
    2:
    begin
      I := Map[S[1]] + (Map[S[2]] shl 6);
      SetLength(Result, 1);
      Move(I, Result[1], Length(Result));
    end;
    3:
    begin
      I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12);
      SetLength(Result, 2);
      Move(I, Result[1], Length(Result));
    end;
    4:
    begin
      I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12) +
        (Map[S[4]] shl 18);
      SetLength(Result, 3);
      Move(I, Result[1], Length(Result));
    end
  end;
end;

function PreProcess(const S: Ansistring): Ansistring;
var
  SS: Ansistring;
begin
  SS := S;
  Result := '';
  while SS <> '' do
  begin
    Result := Result + Decode(Copy(SS, 1, 4));
    Delete(SS, 1, 4);
  end;
end;

function InternalDecrypt(const S: Ansistring; Key: Word): Ansistring;
var
  I: Word;
  Seed: Word;
begin
  Result := S;
  Seed := Key;
  for I := 1 to Length(Result) do
  begin
    Result[I] := Char(Byte(Result[I]) xor (Seed shr 8));
    Seed := (Byte(S[I]) + Seed) * Word(C1) + Word(C2);
  end;
end;

function Decrypt(const S: Ansistring; Key: Word): Ansistring;
begin
  Result := InternalDecrypt(PreProcess(S), Key);
end;

function Encode(const S: Ansistring): Ansistring;
const
  Map: array[0..63] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'abcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I: Longint;
begin
  Result := '';
  I := 0;
  Move(S[1], I, Length(S));
  case Length(S) of
    1:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64];
    2:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] + Map[(I shr 12) mod 64];
    3:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] +
        Map[(I shr 12) mod 64] + Map[(I shr 18) mod 64]
  end;
end;

function PostProcess(const S: Ansistring): Ansistring;
var
  SS: Ansistring;
begin
  SS := S;
  Result := '';
  while SS <> '' do
  begin
    Result := Result + Encode(Copy(SS, 1, 3));
    Delete(SS, 1, 3);
  end;
end;

function InternalEncrypt(const S: Ansistring; Key: Word): Ansistring;
var
  I: Word;
  Seed: Word;
begin
  Result := S;
  Seed := Key;
  for I := 1 to Length(Result) do
  begin
    Result[I] := Char(Byte(Result[I]) xor (Seed shr 8));
    Seed := (Byte(Result[I]) + Seed) * Word(C1) + Word(C2);
  end;
end;

function Encrypt(const S: Ansistring; Key: Word): Ansistring;
begin
  Result := PostProcess(InternalEncrypt(S, Key));
end;

function AppendFile(FileName: String; Data: String): Boolean;
var
  VarFile: TextFile;
begin
  Trace('|| ScriptCore.AppendFile(' + FileName + ');');

  FileName := AnsiReplaceStr(FileName, '..', '');
  try
    if not FileExists(UserDirectory + FileName) then
    begin
      AssignFile(VarFile, UserDirectory + FileName);
      FileMode := fmOpenWrite;
      ReWrite(VarFile);
      Write(VarFile, '');
      CloseFile(VarFile);
    end;

    AssignFile(VarFile, UserDirectory + FileName);
    FileMode := fmOpenWrite;
    Append(VarFile);
    System.WriteLn(VarFile, Data);
    CloseFile(VarFile);

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function SRayCast(P1X, P1Y, P2X, P2Y: Single; var Distance: Single;
  MaxDist: Single): Boolean;
var
  A, B: TVector2;
begin
  A.x := P1X;
  A.y := P1Y;
  B.x := P2X;
  B.y := P2Y;
  Result := not Map.RayCast(A, B, Distance, MaxDist, True);
end;

function SRayCastEx(P1X, P1Y, P2X, P2Y: Single; var Distance: Single;
  MaxDist: Single; Player, Flag, Bullet, Collider: Boolean; Team: Byte): Boolean;
var
  A, B: TVector2;
begin
  A.x := P1X;
  A.y := P1Y;
  B.x := P2X;
  B.y := P2Y;
  Result := not Map.RayCast(a, b, Distance, MaxDist, Player, Flag,
    Bullet, Collider, Team);
end;

function SArrayHigh(Arr: array of String): Integer;
begin
  Result := High(Arr);
end;

function SWeaponNameByNum(Num: Integer): string;
begin
  Result := WeaponNameByNum(WeaponNumExternalToInternal(Num));
end;

procedure SForceWeapon(A, B, C, D: Byte);
begin
  ForceWeapon(A, WeaponNumExternalToInternal(B), WeaponNumExternalToInternal(C), D, 0);
end;

procedure SForceWeaponEx(A, B, C, D, E: Byte);
begin
  ForceWeapon(A, WeaponNumExternalToInternal(B), WeaponNumExternalToInternal(C), D, E);
end;

procedure SetScore(Id: Byte; Score: Integer);
begin
  Sprite[Id].Player.Kills := Score;
  SortPlayers;
end;

procedure SetTeamScore(Team: Byte; Score: Integer);
begin
  if (Team < 1) or (Team > 4) then
    Exit;
  TeamScore[Team] := Score;
  SortPlayers;
end;

procedure SKickPlayer(Num: Byte);
begin
  KickPlayer(Num, False, KICK_CONSOLE, 0);
end;

function SGetTickCount: Cardinal;
begin
  // Result := idGlobal.GetTickCount;
  Result := MainTickCounter;
end;

// function ServerCreateBullet(SPos, SVelocity: TVector2; SStyle: Byte;
// SOwner: Integer;
// STimeOut: Smallint; N: Byte; HitM: Single; Net: Boolean): Integer;

function SCreateBullet(X, Y, VelX, VelY, HitM: Single; sStyle, Owner: Byte): Integer;
var
  SPos, SVel: TVector2;
  i: Integer;
  FoundWeaponIndex: Integer;
begin
  SPos.x := X;
  SPos.y := Y;
  SVel.x := VelX;
  SVel.y := VelY;

  FoundWeaponIndex := -1;
  for i := Low(Guns) to High(Guns) do
  begin
    if Guns[i].BulletStyle = SStyle then
    begin
      FoundWeaponIndex := i;
      Break;
    end;
  end;

  if FoundWeaponIndex >= 0 then
    Result := ServerCreateBullet(SPos, SVel, Guns[FoundWeaponIndex].Num,
      Owner, 255, HitM, True)
  else
    Result := -1;
end;

function SCreateThing(X, Y: Single; BType: Byte): Integer;
var
  SPos: TVector2;
  OBType: Byte;
begin
  SPos.x := X;
  SPos.y := Y;
  Result := 255;
  OBType := BType;
  case BType of
    1: BType := 5;    // Desert Eagle
    2: BType := 6;    // HK MP5
    3: BType := 7;    // AK 74
    4: BType := 8;    // Steyr AUG
    5: BType := 9;    // Spas 12
    6: BType := 10;   // Ruger77
    7: BType := 11;   // M79
    8: BType := 12;   // Barrett M82A1
    9: BType := 13;   // Minimi
    10: BType := 14;  // Minigun
    11: BType := 4;   // USSOCOM
    12: BType := 24;  // Combat Knife
    13: BType := 25;  // Chainsaw
    14: BType := 26;  // LAW
    15: BType := 27;  // Stationary Gun
    16: BType := 16;  // Medical Kit
    17: BType := 17;  // Grenade Kit
    18: BType := 18;  // Flamer Kit
    19: BType := 20;  // Vest Kit
    20: BType := 19;  // Predator Kit
    21: BType := 21;  // Berserk Kit
    22: BType := 22;  // Cluster Kit
    else
      Exit;
  end;
  Result := CreateThing(SPos, 255, BType, 255);
  Thing[Result].Active := True;
  if (OBType > 0) and (OBType < 15) then
    Thing[Result].AmmoCount := Round(DefaultGuns[OBType].Ammo);
end;

procedure KillThing(Id: Integer);
begin
  try
    if not Thing[Id].Active then
      Exit;
    Thing[Id].Kill;
    Thing[Id].Active := False;

    ServerThingTaken(Id, 255);
  except
    on E: Exception do
      Exit;
  end;
end;

procedure SGiveBonus(Id, Bonus: Byte);
var
  PStyle: Byte;
  I: Integer;
begin
  if (Id < 1) or (Id > MAX_PLAYERS) or (not Sprite[Id].Active) then
    Exit;

  case Bonus of
    1:
    begin  // Predator
      Sprite[Id].Alpha := PREDATORALPHA;
      Sprite[Id].BonusTime := PREDATORBONUSTIME;
      Sprite[Id].BonusStyle := BONUS_PREDATOR;
      Sprite[Id].Health := STARTHEALTH;
      PStyle := 20;
    end;
    2:
    begin  // Berserker
      Sprite[Id].BonusStyle := BONUS_BERSERKER;
      Sprite[Id].BonusTime := BERSERKERBONUSTIME;
      Sprite[Id].Health := STARTHEALTH;
      PStyle := 21;
    end;
    3:
    begin  // Vest
      Sprite[Id].Vest := DEFAULTVEST;
      PStyle := 19;
    end;
    4:
    begin  // Grenades
      Sprite[Id].TertiaryWeapon := Guns[FRAGGRENADE];
      Sprite[Id].TertiaryWeapon.AmmoCount := sv_maxgrenades.Value;
      PStyle := 17;
    end;
    5:
    begin  // Clusters
      Sprite[Id].TertiaryWeapon := Guns[CLUSTERGRENADE];
      Sprite[Id].TertiaryWeapon.AmmoCount := CLUSTER_GRENADES;
      PStyle := 22;
    end;
    6:
    begin  // Flame god
      Sprite[Id].BonusStyle := BONUS_FLAMEGOD;
      Sprite[Id].BonusTime := FLAMERBONUSTIME;
      Sprite[Id].Health := STARTHEALTH;
      PStyle := 18;
    end;
    else
      Exit;
  end;

  I := SCreateThing(Sprite[Id].Skeleton.pos[1].x, Sprite[Id].Skeleton.pos[1].y +
    800, PStyle);

  ServerThingTaken(I, Id);
  KillThing(I);
end;

procedure ServerModifier(var Name: String; Value: Single);
begin
  Name := Name;
  sv_gravity.SetValue(Value);
end;

function SRand(Min, Max: Integer): Integer;
begin
  Result := RandomRange(Min, Max);
end;

function RegExpMatch(RegularExpression, Source: String): Boolean;
begin
  Result := RegExpr.ExecRegExpr(RegularExpression, Source);
end;

function RegExpReplace(RegularExpression, Source, ReplaceText: String;
  UseSubstitution: Boolean): String;
begin
  Result := RegExpr.ReplaceRegExpr(RegularExpression, Source,
    ReplaceText, UseSubstitution);
end;

function MyDate(Format: String): String;
begin
  Result := FormatDateTime(Format, Now);
end;

function IDToIP(Id: Byte): String;
begin
  Result := Iif(Id = 255, '255.255.255.255', Sprite[Id].Player.IP);
end;

function IDToHW(Id: Byte): String;
begin
  Result := Iif(Id = 255, '0', Sprite[Id].Player.hwid);
end;

procedure SBanPlayer(Num: Byte; Time: Integer);
begin
  if Time = 0 then
    Time := PERMANENT;
  KickPlayer(Num, True, KICK_CONSOLE, Iif(Time = PERMANENT, PERMANENT, Time * 3600),
    'Script Ban');
end;

procedure SBanPlayerReason(Num: Byte; Time: Integer; Reason: String);
begin
  if Time = 0 then
    Time := PERMANENT;
  KickPlayer(num, True, KICK_CONSOLE, Iif(time = PERMANENT, PERMANENT, Time * 3600),
    '(Script)' + Reason);
end;

function SCommand(Cmd: String): Variant;
begin
  Result := ParseInput(Cmd, 255);
end;

procedure SSendStringToPlayer(Id: Byte; Text: String);
begin
  ServerSendStringMessage(WideString(Text), Id, 255, MSGTYPE_PUB);
end;

procedure SBotChat(Id: Byte; Text: String);
begin
  if (Id < 1) or (Id > MAX_PLAYERS) then
    Exit;
  if Sprite[Id].Active = False then
    Exit;

  ServerSendStringMessage(WideString(Text), Id, 255, MSGTYPE_PUB);
  MainConsole.Console('[' + Sprite[Id].Player.Name + '] ' + Text,
    CHAT_MESSAGE_COLOR);
end;

procedure SSLeep(Milliseconds: Cardinal);
begin
  // aPSCE[1].FLock.Leave;
  Sleep(Milliseconds);
  // aPSCE[1].FLock.Enter;
end;

function GetSystem: String;
begin
  {$IFDEF MSWINDOWS}
  Result := 'windows';
    {$ELSE}
        {$IFDEF LINUX}
        Result := 'linux';
        {$ELSE}
            {$IFDEF DARWIN}
            Result := 'osx';
            {$ELSE}
            Result := 'unknown';
            {$ENDIF}
        {$ENDIF}
    {$ENDIF}
end;

function GetPlayerStat(Id: Byte; ScriptStat: String): Variant;
var
  Stat: String;
begin
  Result := 0;  // return on error
  if (Id > MAX_PLAYERS) or (Id < 1) then
    Exit;

  Stat := UpperCase(ScriptStat);
  if Stat = 'KILLS' then
    Result := Sprite[Id].Player.Kills  // Integer
  else if Stat = 'DEATHS' then
    Result := Sprite[Id].Player.Deaths  // Integer
  else if Stat = 'PING' then
    Result := Sprite[Id].Player.RealPing  // Integer
  else if Stat = 'TEAM' then
    Result := Sprite[Id].Player.Team  // Byte
  else if Stat = 'ACTIVE' then
    Result := Sprite[Id].Active  // Boolean
  else if Stat = 'IP' then
    Result := Sprite[Id].Player.IP  // string
  else if Stat = 'HWID' then
    Result := Sprite[Id].Player.hwid  // string
  else if Stat = 'PORT' then
    Result := Sprite[Id].Player.Port  // Integer
  else if Stat = 'NAME' then
    Result := Sprite[Id].Player.Name  // string
  else if Stat = 'ALIVE' then
    Result := not Sprite[Id].DeadMeat // Boolean
  else if Stat = 'HEALTH' then
    Result := Sprite[Id].Health  // Integer
  else if Stat = 'PRIMARY' then
    Result := WeaponNumInternalToExternal(Sprite[Id].Weapon.Num)  // Byte
  else if Stat = 'SECONDARY' then
    Result := WeaponNumInternalToExternal(Sprite[Id].SecondaryWeapon.Num)  // Byte
  else if Stat = 'AMMO' then
    Result := Sprite[Id].Weapon.AmmoCount  // Byte
  else if Stat = 'SECAMMO' then
    Result := Sprite[Id].SecondaryWeapon.AmmoCount
  else if Stat = 'JETS' then
    Result := Sprite[Id].JetsCount  // Integer
  else if Stat = 'GRENADES' then
    Result := Sprite[Id].TertiaryWeapon.AmmoCount  // Byte
  else if Stat = 'X' then
    Result := Sprite[Id].Skeleton.Pos[1].x  // Single
  else if Stat = 'Y' then
    Result := Sprite[Id].Skeleton.Pos[1].y  // Single
  else if Stat = 'FLAGGER' then
    Result := (Sprite[Id].HoldedThing > 0) and
      (Thing[Sprite[Id].HoldedThing].Style < 4) // Boolean
  else if Stat = 'TIME' then
    Result := Sprite[Id].Player.PlayTime  // Integer
  else if Stat = 'GROUND' then
    Result := Sprite[Id].OnGround  // Boolean
  else if Stat = 'HUMAN' then
    // Boolean
    Result := Sprite[Id].Player.ControlMethod = HUMAN
  else if Stat = 'VELX' then
    Result := SpriteParts.Velocity[Id].x  // Single
  else if Stat = 'VELY' then
    Result := SpriteParts.Velocity[Id].y  // Single
  else if Stat = 'VEST' then
    Result := Sprite[Id].Vest  // Integer
  else if Stat = 'TIME' then
    Result := Sprite[Id].Player.PlayTime  // Integer
  else if Stat = 'DIRECTION' then
    Result := Iif(Sprite[Id].Direction = -1, '<', '>')  // Char
  else if Stat = 'FLAGS' then
    Result := Sprite[Id].Player.Flags  // Byte
  else if Stat = 'MUTE' then
    Result := Sprite[Id].Player.Muted;  // Byte
end;

function GetKeyPress(Id: Byte; ScriptKey: String): Boolean;
var
  Key: String;
begin
  Result := False;  // return on error
  if (Id > MAX_PLAYERS) or (Id < 1) then
    Exit;

  Key := UpperCase(ScriptKey);
  if Key = 'LEFT' then
    Result := Sprite[Id].Control.Left
  else if Key = 'RIGHT' then
    Result := Sprite[Id].Control.Right
  else if Key = 'UP' then
    Result := Sprite[Id].Control.Up
  else if Key = 'SHOOT' then
    Result := Sprite[Id].Control.Fire
  else if Key = 'JETPACK' then
    Result := Sprite[Id].Control.Jetpack
  else if Key = 'GRENADE' then
    Result := Sprite[Id].Control.ThrowNade
  else if Key = 'CHANGEWEP' then
    Result := Sprite[Id].Control.ChangeWeapon
  else if Key = 'THROW' then
    Result := Sprite[Id].Control.ThrowWeapon
  else if Key = 'RELOAD' then
    Result := Sprite[Id].Control.Reload
  else if (Key = 'CROUCH') or (Key = 'DOWN') then
    Result := Sprite[Id].Control.Down
  else if Key = 'PRONE' then
    Result := Iif(Sprite[Id].Position = POS_PRONE, True, False);
end;

function GetObjectStat(Id: Byte; ScriptStat: String): Variant;
var
  Stat: String;
begin
  Result := 0;  // error

  if (Id < 1) or (Id > MAX_THINGS) then
    Exit;

  Stat := UpperCase(ScriptStat);
  if Stat = 'STYLE' then
    Result := Thing[Id].Style  // Byte
  else if Stat = 'ACTIVE' then
    Result := Thing[Id].Active  // Boolean
  else if Stat = 'X' then
    Result := Thing[Id].Skeleton.Pos[1].X  // Single
  else if Stat = 'Y' then
    Result := Thing[Id].Skeleton.Pos[1].Y  // Single
  else if Stat = 'INBASE' then
    Result := Thing[Id].InBase;  // Boolean
end;

function GetSpawnStat(Id: Byte; ScriptStat: String): Variant;
var
  Stat: String;
begin
  Stat := UpperCase(ScriptStat);
  if Stat = 'ACTIVE' then
    Result := Map.SpawnPoints[Id].Active  // Boolean
  else if Stat = 'STYLE' then
    Result := Map.SpawnPoints[Id].Team  // Integer
  else if Stat = 'X' then
    Result := Map.SpawnPoints[Id].X  // Single
  else if Stat = 'Y' then
    Result := Map.SpawnPoints[Id].Y  // Single
  else
    Result := 0;  // error
end;

procedure SetSpawnStat(Id: Byte; ScriptStat: String; Value: Variant);
var
  Stat: String;
begin
  Stat := UpperCase(ScriptStat);
  if Stat = 'ACTIVE' then
    Map.SpawnPoints[Id].Active := Value  // Boolean
  else if Stat = 'STYLE' then
    Map.SpawnPoints[Id].Team := Value  // Integer
  else if Stat = 'X' then
    Map.SpawnPoints[Id].X := Value  // Single
  else if Stat = 'Y' then
    Map.SpawnPoints[Id].Y := Value;  // Single
end;

procedure DoDamage(Id: Byte; Damage: Integer);
var
  A: TVector2;
begin
  A := Default(TVector2);
  Sprite[Id].HealthHit(Damage, Id, 1, 1, A);
end;

procedure DoDamageBy(Id, Shooter: Byte; Damage: Integer);
var
  A: TVector2;
begin
  A := Default(TVector2);
  Sprite[Id].HealthHit(Damage, Shooter, 1, 1, A);
end;

procedure GetPlayerXY(Id: Byte; var X, Y: Single);
begin
  X := Sprite[Id].Skeleton.pos[1].X;
  Y := Sprite[Id].Skeleton.pos[1].Y;
end;

procedure GetFlagsXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: Single);
var
  I: Integer;
begin
  for I := 0 to MAX_THINGS do
    case Thing[I].Style of
      1:
      begin
        RedFlagX := Thing[I].Skeleton.Pos[1].x;
        RedFlagY := Thing[I].Skeleton.Pos[1].Y;
      end;
      2:
      begin
        BlueFlagX := Thing[I].Skeleton.Pos[1].x;
        BlueFlagY := Thing[I].Skeleton.Pos[1].Y;
      end;
      3:
      begin
        RedFlagX := Thing[I].Skeleton.Pos[1].x;
        RedFlagY := Thing[I].Skeleton.Pos[1].Y;
      end;
    end;
end;

procedure GetFlagsSpawnXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: Single);
begin
  BlueFlagX := Map.SpawnPoints[Map.FlagSpawn[2]].X;
  BlueFlagy := Map.SpawnPoints[Map.FlagSpawn[2]].Y;
  RedFlagX := Map.SpawnPoints[Map.FlagSpawn[1]].X;
  RedFlagy := Map.SpawnPoints[Map.FlagSpawn[1]].Y;
end;

procedure MyStartVoteKick(Target: Byte; Reason: String);
begin
  StartVote(255, VOTE_KICK, IntToStr(Target), Reason);
  ServerSendVoteOn(1, 255, IntToStr(Target), Reason);
end;

procedure MyStartVoteMap(Mapname, Reason: String);
begin
  StartVote(255, VOTE_MAP, Mapname, Reason);
  ServerSendVoteOn(0, 255, Mapname, Reason);
end;

function MyExp(Number: Extended): Extended;
begin
  Result := Exp(Number);
end;

function MyRound(const Number: Variant): Int64;
begin
  Result := Round(ValReal(Number));
end;

function MyRoundTo(const AValue: Extended; ADigit: Integer): Extended;
var
  X: Extended;
  I: Integer;
begin
  X := 1.0;
  ADigit := -ADigit;
  for I := 1 to Abs(ADigit) do
    X := X * 10;
  if ADigit < 0 then
    Result := Round(AValue * X) / X
  else
    Result := Round(AValue / X) * X;
end;  // R2

function WriteFile(FileName: String; SData: String): Boolean;
var
  VarFile: TextFile;
begin
  Trace('|| ScriptCore.WriteFile(' + FileName + ');');

  FileName := AnsiReplaceStr(FileName, '..', '');
  try
    AssignFile(VarFile, UserDirectory + FileName);
    FileMode := fmOpenWrite;
    ReWrite(VarFile);
    Write(VarFile, sData);
    CloseFile(VarFile);
    Result := True;
  except
    Result := False;
  end;
end;

procedure WriteLn(const Data: String);
begin
  mainconsole.Console(Data, SERVER_MESSAGE_COLOR);
end;

function shell_exec(Command: String): Integer;
begin
  if ScrptDispatcher.SafeMode then
  begin
    Result := -1;
    MainConsole.Console('[Error] shell_exec: Function Disabled (SafeMode = ON)',
      DEBUG_MESSAGE_COLOR);
    Exit;
  end;
  {$IFNDEF MSWINDOWS}
    Result := fpSystem(Command);
  {$ELSE}
  Result := ShellExecute(0, 'open',
      // PChar('command.com'),  // doesn't work on Window 64bit
      PChar('cmd.exe'),  // Since Windows NT
      PChar('/c ' + Command), nil, 0)
  {$ENDIF}
end;

function MyReadFile(FileName: String): String;
begin
  Result := ReadFile(FileName, False);
end;

function CrossFunc(const Params: array of Variant; ProcName: String): Variant;
begin
  Result := ScrptDispatcher.CallFunc(Params, ProcName, 0);
end;

procedure ResetTarget(Id: Byte);
begin
  Sprite[Id].TargetX := 0;
  Sprite[Id].TargetY := 0;
end;

function SetVal(Script: TPSScript; Variable: String; Value: Integer): Boolean;
begin
  Result := False;
  if not sc_enable.Value then
    Exit;
  // Trace('|| ScriptCore.SetVal('+Variable+', '+inttostr(Value)+');');

  VSetInt(Script.GetVariable(UpperCase(Variable)), Value);
  Result := True;
end;

function SetString(Script: TPSScript; Variable: String; Value: String): Boolean;
begin
  Result := False;
  if not sc_enable.Value then
    Exit;
  // Trace('|| ScriptCore.SetString('+Variable+','+Value+');');

  VSetString(Script.GetVariable(UpperCase(Variable)), Value);
  Result := True;
end;

function MyArctan(A: Extended): Extended;
begin
  Result := arctan(A);
end;

function ReadINI(FileName, Section, Value, Default: string): string;
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(UserDirectory + FileName);
  try
    Result := ini.ReadString(Section, Value, Default)
  except
  end;
  ini.Free;
end;

procedure SetVariables(ScriptCore: TPascalCore; Script: TPSScript);
var
  I: Integer;
  TeamsCount: array[0..5] of Byte;
begin
  try
    for I := 0 to 5 do
      TeamsCount[I] := 0;

    for I := 1 to MAX_SPRITES do
      if (Sprite[I].Active) and (Sprite[I].Player.Team <= TEAM_SPECTATOR) then
        TeamsCount[Sprite[I].Player.Team] :=
          TeamsCount[Sprite[I].Player.Team] + 1;

    SetString(Script, 'CoreVersion', COREVERSION);
    SetVal(Script, 'SafeMode', iif(ScrptDispatcher.SafeMode, 1, 0));

    SetVal(Script, 'DeathmatchPlayers', TeamsCount[0]);
    SetVal(Script, 'AlphaPlayers', TeamsCount[1]);
    SetVal(Script, 'BravoPlayers', TeamsCount[2]);
    SetVal(Script, 'CharliePlayers', TeamsCount[3]);
    SetVal(Script, 'DeltaPlayers', TeamsCount[4]);
    SetVal(Script, 'Spectators', TeamsCount[5]);

    SetVal(Script, 'AlphaScore', TeamScore[1]);
    SetVal(Script, 'BravoScore', TeamScore[2]);
    SetVal(Script, 'CharlieScore', TeamScore[3]);
    SetVal(Script, 'DeltaScore', TeamScore[4]);

    SetVal(Script, 'MaxPlayers', sv_maxplayers.Value);
    SetVal(Script, 'NumPlayers', PlayersNum);
    SetVal(Script, 'NumBots', BotsNum);

    SetString(Script, 'CurrentMap', Map.Name);
    SetString(Script, 'NextMap', CheckNextMap);
    SetVal(Script, 'TimeLimit', sv_timelimit.Value div 60 div 60);
    SetVal(Script, 'TimeLeft', TimeLimitCounter div 60);
    SetVal(Script, 'ScoreLimit', sv_killlimit.Value);
    SetVal(Script, 'GameStyle', Byte(sv_gamemode.Value));

    SetString(Script, 'ServerName', sv_hostname.Value);
    SetString(Script, 'Version', OPENSOLDAT_VERSION);
    SetString(Script, 'ServerVersion', DEDVERSION);
    SetVal(Script, 'ServerPort', net_port.Value);
    SetString(Script, 'ServerIP', ServerIP);
    SetString(Script, 'ScriptName', ScriptCore.Name);
    SetString(Script, 'Password', sv_password.Value);
    SetVal(Script, 'Paused', iif(MapChangeCounter = 999999999, 1, 0));

  except
    // on E: Exception do
    // WriteLn(' [*] Exception raised on SetVariables(' + E.Message + ')');
  end;
end;

procedure RegisterFunctions(ScriptCore: TPascalCore; Script: TPSScript);
begin
  Script.Comp.AddTypeS('TStringArray', 'array of string');
  // Exit;

  // Script.AddFunction(@GetStrikeTarget,
  // 'procedure GetStrikeTarget(A: Byte; var B, C: Single);');
  Script.AddFunction(@PlaySound,
    'procedure PlaySound(Id: Byte; Name: string; X, Y: Single)');

  Script.AddFunction(@FormatFloat,
    'function FormatFloat(A: string; B: extended): string');
  Script.AddFunction(@SArrayHigh, 'function ArrayHigh(A: array of string): Integer');
  Script.AddFunction(@WriteLn, 'procedure WriteLn(Data: string)');
  Script.AddFunction(@SRand, 'function Random(A,B:Integer): Integer');

  Script.AddFunction(@SCreateBullet,
    'function CreateBullet(A, B, C, D, E:Single; F, G: Byte): Integer');
  Script.AddFunction(@SBotChat, 'procedure BotChat(A: Byte; B: string);');
  Script.AddFunction(@SGiveBonus, 'procedure GiveBonus(A, B: Byte)');
  Script.AddFunction(@ServerModifier,
    'procedure ServerModifier(A: string; B: Variant)');

  Script.AddFunction(@SRayCast,
    'function RayCast(A, B, C, D: Single; var E: Single; F: Single): Boolean');
  Script.AddFunction(@SRayCastEx,
    'function RayCastEx(A, B, C, D: Single; var E: Single; F: Single; G, H, I, J: Boolean; K: Byte): Boolean');
  Script.AddFunction(@SetTeamScore,
    'procedure SetTeamScore(A: Byte; B: Integer)');
  Script.AddFunction(@MD5StringHelper,
    'function MD5String(A: string): string');

  Script.AddFunction(@MyArctan, 'function arctan(A: Extended): Extended;');
  Script.AddFunction(@LogN, 'function LogN(A, B: Extended): Extended;');

  Script.AddFunction(@MovePlayer, 'procedure MovePlayer(A: Byte; X, Y: Single)');
  Script.AddFunction(@GetObjectStat,
    'function GetObjectStat(A: Byte; B: string): Variant');
  Script.AddFunction(@ForwardClient,
    'procedure ForwardClient(A: string; B: Integer; C: string; D: Integer; E: string)');
  Script.AddFunction(@CrossFunc,
    'function CrossFunc(const A: array of Variant; B: string): Variant;');

  Script.AddFunction(@ResetTarget, 'procedure ResetTarget(A: Byte);');
  Script.AddFunction(@GetKeyPress,
    'function GetKeyPress(Id: Byte; Key: string): Boolean');

  Script.AddFunction(@GetSpawnStat,
    'function GetSpawnStat(A: Byte; B: string): Variant');
  Script.AddFunction(@SetSpawnStat,
    'procedure SetSpawnStat(A: Byte; B: string; C: Variant)');

  Script.AddFunction(@ReadINI, 'function ReadINI(A, B, C, D: string): string');
  Script.AddFunction(@RegExpMatch,
    'function RegExpMatch(A, B: string): Boolean');
  Script.AddFunction(@RegExpReplace,
    'function RegExpReplace(A, B, C: string; D: Boolean): string');

  Script.AddFunction(@MyRoundTo,
    'function RoundTo(A: Extended; B: Integer): Extended');
  Script.AddFunction(@MyDate, 'function FormatDate(A: string): string');

  //Script.AddMethod(ScriptCore, @TPascalCore.ThreadFunc,
  //  'procedure ThreadFunc(Params: array of Variant; FuncName: string)');
  Script.AddFunction(@WriteConsole,
    'procedure WriteConsole(A: Byte; B: string; C: LongInt)');
  Script.AddFunction(@DrawText,
    'procedure DrawText(A: Byte; B: string; C: Integer; D: LongInt; E: Single; F, G: Integer)');
  Script.AddFunction(@DrawTextEx,
    'procedure DrawTextEx(Id, Num: Byte; Text: String; Delay: Integer; Colour: Longint; Scale: Single; X, Y: Integer)');
  Script.AddFunction(@GetSystem, 'function GetSystem: string');
  Script.AddFunction(@SGetTickCount, 'function GetTickCount: Cardinal');

  Script.AddFunction(@SRGB, 'function RGB(R, G, B: Byte): LongInt');

  Script.AddFunction(@IDtoName, 'function IDToName(A: Integer): string');
  Script.AddFunction(@NameToID, 'function NameToID(A: string): Integer');
  Script.AddFunction(@NameToHW, 'function NameToHW(A: string): string');
  Script.AddFunction(@IDToIP, 'function IDToIP(Id: Byte): string');
  Script.AddFunction(@IDToHW, 'function IDToHW(Id: Byte): string');
  {$IFDEF RCON}
  Script.AddFunction(@SendMessageToAdmin, 'procedure TCPAdminPM(A, B: string)');
  {$ENDIF}
  Script.AddFunction(@SWeaponNameByNum,
    'function WeaponNameByNum(A: Integer): string');
  Script.AddFunction(@RandomBot, 'function RandomBot: string');
  Script.AddFunction(@SKickPlayer, 'procedure KickPlayer(A: Byte)');
  Script.AddFunction(@MyStartVoteKick,
    'procedure StartVoteKick(A: Byte; B: string)');

  Script.AddFunction(@MyStartVoteMap,
    'procedure StartVoteMap(A, B: string)');
  Script.AddFunction(@SBanPlayer, 'procedure BanPlayer(A: Byte; B: Integer)');
  Script.AddFunction(@SBanPlayerReason,
    'procedure BanPlayerReason(A: Byte; B: Integer; C: string)');

  Script.AddFunction(@CheckWeaponNotAllowed,
    'function CheckWeaponAllowed(A: Byte): Boolean');
  Script.AddFunction(@SCommand, 'function Command(A: string): Variant');

  Script.AddFunction(@StartServer, 'procedure StartServer');
  Script.AddFunction(@GetPID, 'function GetPID: Integer');
  Script.AddFunction(@Shutdown, 'procedure Shutdown');
  Script.AddFunction(@UpdateGameStats, 'procedure UpdateGameStats');
  Script.AddFunction(@DoDamage, 'procedure DoDamage(A: Byte; B: Integer)');
  Script.AddFunction(@DoDamageBy,
    'procedure DoDamageBy(A, B: Byte; C: Integer)');

  Script.AddFunction(@SForceWeapon, 'procedure ForceWeapon(A, B, C, D: Byte)');
  Script.AddFunction(@SForceWeaponEx,
    'procedure ForceWeaponEx(A, B, C, D, E: Byte)');
  Script.AddFunction(@SetWeaponActive,
    'procedure SetWeaponActive(A, B: Byte; C: Boolean)');
  Script.AddFunction(@SetScore, 'procedure SetScore(A: Byte; B: Integer)');
  Script.AddFunction(@Distance, 'function Distance(A, B, C, D: Single): Single');
  Script.AddFunction(@SCreateThing,
    'function SpawnObject(A, B: Single; D: Byte): Integer');
  Script.AddFunction(@KillThing, 'procedure KillObject(A: Integer)');

  Script.AddFunction(@GetPlayerXY,
    'procedure GetPlayerXY(A: Byte; var X, Y: Single)');
  Script.AddFunction(@GetPlayerStat,
    'function GetPlayerStat(A: Byte; B: string): Variant');

  Script.AddFunction(@GetFlagsXY,
    'procedure GetFlagsXY(var A, B, C, D: Single)');
  Script.AddFunction(@GetFlagsSpawnXY,
    'procedure GetFlagsSpawnXY(var A, B, C, D: Single)');
  Script.AddFunction(@SSendStringToPlayer,
    'procedure SayToPlayer(A: Byte; B: string)');
  Script.AddFunction(@SSleep, 'procedure Sleep(A: Cardinal)');
  // Script.AddFunction(@SetVariables, 'procedure UpdateVars');

  Script.AddFunction(@MatchesMask,
    'function MaskCheck(const A: string; const B: string):Boolean');
  Script.AddFunction(@util.Iif,
    'function Iif(const A: Boolean; const B: Variant; const C: Variant): Variant');
  Script.AddFunction(@GetPiece,
    'function GetPiece(const A, B: string; const C: Integer): string');

  Script.AddFunction(@MyReadFile, 'function ReadFile(A: string): string');
  Script.AddFunction(@WriteFile, 'function WriteFile(A, B: string): Boolean');
  Script.AddFunction(@AppendFile,
    'function WriteLnFile(A: string; B: string): Boolean');
  Script.AddFunction(@ExistsFile, 'function FileExists(F: string): Boolean');
  Script.AddFunction(@AnsiContainsStr,
    'function ContainsString(const A, B: string): Boolean');
  Script.AddFunction(@AnsiIndexStr,
    'function GetStringIndex(const A: string; const B: array of string): Integer');
  Script.AddFunction(@AnsiPos, 'function StrPos(const A, S: string): Integer');
  Script.AddFunction(@AnsiReplaceStr,
    'function StrReplace(const A, B, C: string): string');
  //Script.AddFunction(@HTTPEncode, 'function HTTPEncode(A: string): string');
  //Script.AddFunction(@HTTPDecode, 'function HTTPDecode(A: string): string');

  // Safe Mode Functions
  Script.AddFunction(@shell_exec, 'function shell_exec(A: string): Integer');
  //Script.AddFunction(@ScriptGetURL, 'function GetURL(A: string): string');

  // Variables
  Script.AddRegisteredVariable('CoreVersion', 'string');
  Script.AddRegisteredVariable('ScriptName', 'string');
  Script.AddRegisteredVariable('SafeMode', 'Byte');
  Script.AddRegisteredVariable('MaxPlayers', 'Byte');
  Script.AddRegisteredVariable('NumPlayers', 'Byte');
  Script.AddRegisteredVariable('NumBots', 'Byte');
  Script.AddRegisteredVariable('CurrentMap', 'string');
  Script.AddRegisteredVariable('NextMap', 'string');
  Script.AddRegisteredVariable('TimeLimit', 'Integer');
  Script.AddRegisteredVariable('TimeLeft', 'Integer');
  Script.AddRegisteredVariable('ScoreLimit', 'Integer');
  Script.AddRegisteredVariable('GameStyle', 'Byte');
  Script.AddRegisteredVariable('Version', 'string');
  Script.AddRegisteredVariable('ServerVersion', 'string');
  Script.AddRegisteredVariable('ServerName', 'string');
  Script.AddRegisteredVariable('ServerIP', 'string');
  Script.AddRegisteredVariable('ServerPort', 'Integer');
  Script.AddRegisteredVariable('DeathmatchPlayers', 'Byte');
  Script.AddRegisteredVariable('Spectators', 'Byte');
  Script.AddRegisteredVariable('AlphaPlayers', 'Byte');
  Script.AddRegisteredVariable('BravoPlayers', 'Byte');
  Script.AddRegisteredVariable('CharliePlayers', 'Byte');
  Script.AddRegisteredVariable('DeltaPlayers', 'Byte');
  Script.AddRegisteredVariable('AlphaScore', 'Byte');
  Script.AddRegisteredVariable('BravoScore', 'Byte');
  Script.AddRegisteredVariable('CharlieScore', 'Byte');
  Script.AddRegisteredVariable('DeltaScore', 'Byte');
  Script.AddRegisteredVariable('ReqPort', 'Word');

  Script.AddRegisteredVariable('Paused', 'Boolean');
  Script.AddRegisteredVariable('Password', 'string');

  Script.AddRegisteredPTRVariable('DisableScript', 'Boolean');
  Script.AddRegisteredPTRVariable('AppOnIdleTimer', 'LongWord');
  with Script.Comp.AddConstantN('SCRIPT_NAME', 'String') do
  begin
    SetString(ScriptCore.Name);
  end;
end;

end.

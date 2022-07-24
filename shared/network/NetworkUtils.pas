// helper functions

unit NetworkUtils;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // helper units
  Version, StrUtils,

  // opensoldat units
  {$IFNDEF SERVER}
  Sound,
  {$ENDIF}
  Sprites, Weapons, Constants;

{$IFDEF SERVER}
function IsRemoteAdminIP(IP: string): Boolean;
function IsAdminIP(IP: string): Boolean;
function IsAdminPassword(Password: string): Boolean;
{$ENDIF}
function IsWrongGameVersion(RequestVersion: string): Boolean;
function VerifyPacket(ValidSize, ReceiveSize, PacketId: Integer): Boolean;
function VerifyPacketLargerOrEqual(ValidSize, ReceiveSize, PacketId: Integer): Boolean;
{$IFDEF SERVER}
function IsServerTotallyFull: Boolean;
function IsServerFull: Boolean;
function IsWrongGamePassword(GamePassword: string): Boolean;
{$ENDIF}
function FixPlayerName(Name: array of char): string;

{$IFNDEF SERVER}
procedure PlayRadioSound(RadioID: Byte);
procedure NewPlayerWeapon;
function ReturnFixedPlayerName(name: string): string;
{$ENDIF}
{$IFDEF SERVER}
function CheckWeaponNotAllowed(i: Byte): Boolean;
{$ENDIF}

procedure EncodeKeys(var SpriteC: TSprite; out Keys16: Word);
procedure DecodeKeys(var SpriteC: TSprite; var Keys16: Word);
{$IFDEF SERVER}
function FindFloodID(SrcIP: string): Integer;
function AddFloodIP(SrcIP: string): Cardinal;
function UpdateAntiFlood(SrcIP: string): Cardinal;
function IsFloodID(ID: Cardinal): Boolean;

function AddIPToRemoteAdmins(SrcIP: string): Boolean;
{$ENDIF}
procedure StringToArray(var c: array of Char; s: string);
implementation

uses
  {$IFDEF SERVER}
  Server, BanSystem,
  {$ELSE}
  Client, GameMenus,
  {$ENDIF}
  Net, Game, TraceLog;

{$IFNDEF SERVER}
procedure PlayRadioSound(RadioID: Byte);
begin
  if (RadioCooldown > 0) or (not sv_radio.Value) then
    Exit;
  if (sv_gamemode.Value <> GAMESTYLE_CTF) and (sv_gamemode.Value <> GAMESTYLE_HTF) and
     (sv_gamemode.Value <> GAMESTYLE_INF) then
    Exit;

  RadioCooldown := 3;
  case RadioID of
    11: PlaySound(SFX_RADIO_EFCUP, SpriteParts.Pos[MySprite]);
    12: PlaySound(SFX_RADIO_EFCMID, SpriteParts.Pos[MySprite]);
    13: PlaySound(SFX_RADIO_EFCDOWN, SpriteParts.Pos[MySprite]);
    21: PlaySound(SFX_RADIO_FFCUP, SpriteParts.Pos[MySprite]);
    22: PlaySound(SFX_RADIO_FFCMID, SpriteParts.Pos[MySprite]);
    23: PlaySound(SFX_RADIO_FFCDOWN, SpriteParts.Pos[MySprite]);
    31: PlaySound(SFX_RADIO_ESUP, SpriteParts.Pos[MySprite]);
    32: PlaySound(SFX_RADIO_ESMID, SpriteParts.Pos[MySprite]);
    33: PlaySound(SFX_RADIO_ESDOWN, SpriteParts.Pos[MySprite]);
  end;
end;
{$ENDIF}

procedure EncodeKeys(var SpriteC: TSprite; out Keys16: Word);
var
  Controls: ^TControl;
begin
  Controls := @SpriteC.Control;

  Keys16 := 0;
  if Controls.Left          then Keys16 := Keys16 or B1;
  if Controls.Right         then Keys16 := Keys16 or B2;
  if Controls.Up            then Keys16 := Keys16 or B3;
  if Controls.Down          then Keys16 := Keys16 or B4;
  if Controls.Fire          then Keys16 := Keys16 or B5;
  if Controls.Jetpack       then Keys16 := Keys16 or B6;
  if Controls.ThrowNade     then Keys16 := Keys16 or B7;
  if Controls.ChangeWeapon  then Keys16 := Keys16 or B8;
  if Controls.ThrowWeapon   then Keys16 := Keys16 or B9;
  if Controls.Reload        then Keys16 := Keys16 or B10;
  if Controls.FlagThrow     then Keys16 := Keys16 or B11;

  if SpriteC.BodyAnimation.ID = Change.ID then
    Keys16 := Keys16 or B8;
  if SpriteC.BodyAnimation.ID = ThrowWeapon.ID then
    Keys16 := Keys16 or B9;
end;

procedure DecodeKeys(var SpriteC: TSprite; var Keys16: Word);
var
  Controls: ^TControl;
begin
  Controls := @SpriteC.Control;

  Controls.Left         := Keys16 and B1  = B1;
  Controls.Right        := Keys16 and B2  = B2;
  Controls.Up           := Keys16 and B3  = B3;
  Controls.Down         := Keys16 and B4  = B4;
  Controls.Fire         := Keys16 and B5  = B5;
  Controls.Jetpack      := Keys16 and B6  = B6;
  Controls.ThrowNade    := Keys16 and B7  = B7;
  Controls.ChangeWeapon := Keys16 and B8  = B8;
  Controls.ThrowWeapon  := Keys16 and B9  = B9;
  Controls.Reload       := Keys16 and B10 = B10;
  Controls.FlagThrow    := Keys16 and B11 = B11;
end;

// Sets the player name to Major if it is invalid
function FixPlayerName(Name: array of char): string;
begin
  if (Trim(Name) = '') or
    (Name = #12) or
    (UpperCase(Name) = 'SERVER MESSAGE') or
    AnsiContainsStr(Name, #10) or
    AnsiContainsStr(Name, #13) then
    Result := 'Major'
  else
    Result := Name;
end;

function VerifyPacket(ValidSize, ReceiveSize, PacketId: Integer): Boolean;
var
  Dropped: String = '';
begin
  Result := True;
  if ValidSize <> ReceiveSize then
  begin
    Dropped := ' - DROPPED (wrong size <> ' + IntToStr(ValidSize) + ')';
    Result := False;
  end;
  if log_level.Value >= LEVEL_TRACE then
    MainConsole.Console('[NET] Received Packet (' + IntToStr(PacketId) +
      ') Size:' + IntToStr(ReceiveSize) + Dropped, DEBUG_MESSAGE_COLOR);
end;

function VerifyPacketLargerOrEqual(ValidSize, ReceiveSize, PacketId: Integer): Boolean;
var
  Dropped: String = '';
begin
  Result := True;
  if ValidSize > ReceiveSize then
  begin
    Dropped := ' - DROPPED (wrong size, expected at least ' + IntToStr(ValidSize) + ')';
    Result := False;
  end;
  if log_level.Value >= LEVEL_TRACE then
    MainConsole.Console('[NET] Received Packet (' + IntToStr(PacketId) +
      ') Size:' + IntToStr(ReceiveSize) + Dropped, DEBUG_MESSAGE_COLOR);
end;

{$IFDEF SERVER}
// Checks if the IP string is inside the remote IPs list
function IsRemoteAdminIP(IP: string): Boolean;
begin

  if RemoteIPs.IndexOf(IP) > -1 then
    Result := True
  else
    Result := False;
end;

// Checks if the IP string is inside the admin IPs list
function IsAdminIP(IP: string): Boolean;
begin
  if AdminIPs.IndexOf(IP) > -1 then
    Result := True
  else
    Result := False;
end;

// Retruns true if the password is not empty and equal to the Admin password
// Server passwords are not allowed to be empty else everyone could login
function IsAdminPassword(Password: string): Boolean;
begin
  Result := (sv_adminpassword.Value <> '') and (Password = sv_adminpassword.Value);
end;

// Checks if the given passwords match
// If the password is not set then this returns false
function IsWrongGamePassword(GamePassword: string): Boolean;
begin
  Result := (sv_password.Value <> '') and (GamePassword <> sv_password.Value);
end;

// Checks if server has MAX_PLAYERS slots taken and adding even and admin
// wouldn't work
function IsServerTotallyFull: Boolean;
begin
  Result := PlayersNum >= MAX_PLAYERS;
end;

// Checks if allowed server slots are taken
// If MaxPlayers slots is lower than MAX_PLAYERS there are still slots for
// admins to join
function IsServerFull: Boolean;
begin
  Result := ((PlayersNum - BotsNum) >= sv_maxplayers.Value) or
    (IsServerTotallyFull);
end;
{$ENDIF}

// Checks if the Requested and the current OpenSoldat version are the same
function IsWrongGameVersion(RequestVersion: string): Boolean;
begin
  Result := RequestVersion <> OPENSOLDAT_VERSION;
end;

{$IFNDEF SERVER}
function ReturnFixedPlayerName(name: string): string;
var
  i: Integer;
  r: string;
begin
  r := '';
  for i := 1 to Length(name) do
    r := r + name[i];

  if Length(r) > 24 then
    delete(r, 25, Length(r) - 24);

  result := r;
end;
{$ENDIF}

{$IFNDEF SERVER}
procedure NewPlayerWeapon;
var
  i, j: Integer;
  SecWep: Integer;
begin
  if Sprite[MySprite].Weapon.Num = Guns[NOWEAPON].Num then
    GameMenuShow(LimboMenu);

  i := MySprite;

  Sprite[i].Player.SecWep := cl_player_secwep.Value;

  for j := 1 to MAIN_WEAPONS do
    WeaponSel[i][j] := 1;

  if sv_advancemode.Value then
    for j := 1 to PRIMARY_WEAPONS do
      WeaponSel[i][j] := 0;

  for j := 1 to MAIN_WEAPONS do
    if WeaponActive[j] = 1 then
      LimboMenu.Button[j - 1].Active := Boolean(WeaponSel[i][j]);

  SecWep := Sprite[i].Player.SecWep + 1;

  if (SecWep >= 1) and (SecWep <= SECONDARY_WEAPONS) and
      (WeaponActive[PRIMARY_WEAPONS + SecWep] = 1) then
    Sprite[i].SecondaryWeapon := Guns[PRIMARY_WEAPONS + SecWep]
  else
    Sprite[i].SecondaryWeapon := Guns[NOWEAPON];
end;
{$ENDIF}
{$IFDEF SERVER}
function CheckWeaponNotAllowed(i: Byte): Boolean;
var
  WeaponIndex: Integer;
begin
  Trace('CheckWeaponNotAllowed');

  Result := True;

  WeaponIndex := WeaponNumToIndex(Sprite[i].Weapon.Num);
  if IsMainWeaponIndex(WeaponIndex) and (WeaponActive[WeaponIndex] = 0) then
    Exit;

  if ((Sprite[i].Weapon.Num = Guns[BOW].Num)    and (sv_gamemode.Value <> GAMESTYLE_RAMBO)) or
      ((Sprite[i].Weapon.Num = Guns[BOW2].Num)   and (sv_gamemode.Value <> GAMESTYLE_RAMBO)) or
      ((Sprite[i].Weapon.Num = Guns[FLAMER].Num) and (sv_bonus_flamer.Value)) then
    Exit;

  Result := False;
end;

// Searches for the flood ip in the flood ips array
// Returns 0 when nothing was found
function FindFloodID(SrcIP: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to MAX_FLOODIPS do
    if FloodIP[i] = SrcIP then
    begin
      Result := i;
      Break;
    end;
end;

// Adds a flooding ip to the Flood ips array
// If the array is full the flood ip will not be added
function AddFloodIP(SrcIP: string): Cardinal;
var
  i: Cardinal;
const
  FLOOD_ID_NOT_FOUND = 0;
begin
  Result := FLOOD_ID_NOT_FOUND;
  for i := 1 to MAX_FLOODIPS do
    if FloodIP[i] = ' ' then
    begin
      FloodIP[i] := SrcIP;
      Result := i;
      Break;
    end;
end;

function UpdateAntiFlood(SrcIP: string): Cardinal;
var
  FloodID: Integer;
const
  FLOOD_ID_NOT_FOUND = 0;
begin
  {$IFDEF SERVER}
  LastReqIP[LastReqID] := SrcIP;
  LastReqID := (LastReqID + 1) mod 4;
  {$ENDIF}

  FloodID := FindFloodID(SrcIP);

  if FloodID = FLOOD_ID_NOT_FOUND then
  begin
    FloodID := AddFloodIP(SrcIP);
  end
  else
  begin
    Inc(FloodNum[FloodID]);

    if FloodNum[FloodID] > FLOODIP_MAX then
    begin
      AddBannedIP(SrcIP, 'Flooding', TWENTY_MINUTES);
      MainConsole.Console('IP number' + ' ' + SrcIP + ' ' +
        'banned for flooding', CLIENT_MESSAGE_COLOR);
    end;
  end;
  Result := FloodID;
end;

function IsFloodID(ID: Cardinal): Boolean;
const
  FLOOD_ID_NOT_FOUND = 0;
begin
  Result := (ID <> FLOOD_ID_NOT_FOUND) and (FloodNum[ID] > FLOODIP_MAX);
end;

function AddIPToRemoteAdmins(SrcIP: string): Boolean;
begin
  Result := False;
  if SrcIP = ' ' then
    Exit;

  if not IsAdminIP(SrcIP) then
  begin
    AdminIPs.Add(SrcIP);
    Result := True;
  end;
end;
{$ENDIF}

procedure StringToArray(var c: array of Char; s: string);
var
  i: Integer;
begin
  FillChar(c, sizeOf(c), 0);
  if Length(s) < 1 then
    Exit;
  if Length(c) < 1 then
    Exit;
  if Length(s) > Length(c) then
    Exit;
  for i := 0 to Length(s) - 1 do
  begin
    c[i] := s[i + 1];
  end;
  if Length(s) < Length(c) then
    c[Length(s)] := #0;
end;

end.

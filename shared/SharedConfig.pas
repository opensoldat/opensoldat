{*******************************************************}
{                                                       }
{       SharedConfig Unit for OPENSOLDAT                }
{                                                       }
{       Copyright (c) 2012 Daniel Forssten              }
{                                                       }
{*******************************************************}

unit SharedConfig;

interface

uses
  Sprites;

{$IFDEF SERVER}
function LoadBotConfig(const FilePath: string; var SpriteC: TSprite): Boolean;
{$ENDIF}
function LoadWeaponsConfig(const FilePath: string): Boolean;

implementation

uses
  Game, IniFiles, Classes, SysUtils, StrUtils, Math,
  Util, Server, Net, Weapons, Constants;

procedure ReadConfColor(conf: TStringList; const SectionName: string; var VarName: LongWord); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := LongWord(ColorToHex(StringToColor(conf.Values[SectionName])))
    else
      raise Exception.Create('Value "' + SectionName + '" not found');
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;

procedure ReadConfMagicColor(conf: TStringList; const SectionName: string; var VarName: LongWord); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := StringToColor(conf.Values[SectionName])
    else
      raise Exception.Create('Value "' + SectionName + '" not found');
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;

procedure ReadConf(conf: TStringList; const SectionName: string; var VarName: Integer); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := StrToInt(conf.Values[SectionName])
    else
      raise Exception.Create('Value "' + SectionName + '" not found');
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;

procedure ReadConf(conf: TStringList; const SectionName: string; var VarName: Byte); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := StrToInt(conf.Values[SectionName])
    else
      raise Exception.Create('Value "' + SectionName + '" not found');
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;

procedure ReadConf(conf: TStringList; const SectionName: string; var VarName: string;
  AllowBlank: Boolean = False); overload;
begin
  if (conf.Values[SectionName] <> '') or AllowBlank then
    VarName := conf.Values[SectionName]
  else
    raise Exception.Create('Value "' + SectionName + '" not found');
end;

procedure ReadConf(conf: TStringList; const SectionName: string; var VarName: Boolean;
  AllowBlank: Boolean = False); overload;
begin
  if (conf.Values[SectionName] <> '') or AllowBlank then
    VarName := conf.Values[SectionName] = '1'
  else
    raise Exception.Create('Value "' + SectionName + '" not found');
end;

procedure ReadWMConf(conf: TStringList; const SectionName: string; var VarName: Word); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := StrToInt(conf.Values[SectionName]);
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;

procedure ReadWMConf(conf: TStringList; const SectionName: string; var VarName: SmallInt); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := StrToInt(conf.Values[SectionName]);
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;

procedure ReadWMConf(conf: TStringList; const SectionName: string; var VarName: Byte); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := StrToInt(conf.Values[SectionName]);
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;

procedure ReadWMConf(conf: TStringList; const SectionName: string; var VarName: Single); overload;
begin
  try
    if conf.Values[SectionName] <> '' then
      VarName := StrToFloat(conf.Values[SectionName]);
  except
    raise Exception.Create('Value "' + SectionName + '" is not a number');
  end;
end;
{$IFDEF SERVER}
function LoadBotConfig(const FilePath: string; var SpriteC: TSprite): Boolean;
var
  ini: TMemIniFile;
  conf: TStringList;
  Filename: string;
  FavWeaponName: string = '';
  Headgear: Byte = 0;
begin
  Result := False;
  ini := nil;
  conf := nil;

  try
    if not FileExists(FilePath) then
      raise Exception.Create('Bot file not found');

    conf := TStringList.Create;
    ini := TMemIniFile.Create(FilePath);

    if ini.SectionExists('BOT') then
    begin
      ini.ReadSectionValues('BOT', conf);

      ReadConf(conf, 'Favourite_Weapon', FavWeaponName);
      SpriteC.Brain.FavWeapon := WeaponNameToNum(FavWeaponName);
      ReadConf(conf, 'Secondary_Weapon', SpriteC.Player.SecWep);
      ReadConf(conf, 'Friend', SpriteC.Brain.Friend, True);
      ReadConf(conf, 'Accuracy', SpriteC.Brain.Accuracy);
      SpriteC.Brain.Accuracy := Trunc(SpriteC.Brain.Accuracy * (bots_difficulty.Value / 100));
      ReadConf(conf, 'Shoot_Dead', SpriteC.Brain.DeadKill);
      ReadConf(conf, 'Grenade_Frequency', SpriteC.Brain.GrenadeFreq);
      ReadConf(conf, 'OnStartUse', SpriteC.Brain.Use);

      ReadConf(conf, 'Chat_Frequency', SpriteC.Brain.ChatFreq);
      SpriteC.Brain.ChatFreq := Round(2.5 * SpriteC.Brain.ChatFreq);
      ReadConf(conf, 'Chat_Kill', SpriteC.Brain.ChatKill, True);
      ReadConf(conf, 'Chat_Dead', SpriteC.Brain.ChatDead, True);
      ReadConf(conf, 'Chat_LowHealth', SpriteC.Brain.ChatLowHealth, True);
      ReadConf(conf, 'Chat_SeeEnemy', SpriteC.Brain.ChatSeeEnemy, True);
      ReadConf(conf, 'Chat_Winning', SpriteC.Brain.ChatWinning, True);

      ReadConf(conf, 'Camping', SpriteC.Brain.Camper);

      ReadConf(conf, 'Name', SpriteC.Player.Name);
      SetLength(SpriteC.Player.Name, Min(Length(SpriteC.Player.Name), PLAYERNAME_CHARS));

      if (SpriteC.Player.Team = TEAM_NONE) then
        ReadConfColor(conf, 'Color1', SpriteC.Player.ShirtColor);
      ReadConfColor(conf, 'Color2', SpriteC.Player.PantsColor);
      ReadConfMagicColor(conf, 'Skin_Color', SpriteC.Player.SkinColor);
      ReadConfColor(conf, 'Hair_Color', SpriteC.Player.HairColor);
      SpriteC.Player.JetColor := (DEFAULT_JETCOLOR and $00FFFFFF) + COLOR_TRANSPARENCY_BOT;
      ReadConf(conf, 'Hair', SpriteC.Player.HairStyle);

      ReadConf(conf, 'Headgear', Headgear);
      if Headgear = 0 then
        SpriteC.Player.HeadCap := 0
      else if Headgear = 2 then
        SpriteC.Player.HeadCap := GFX_GOSTEK_KAP
      else
        SpriteC.Player.HeadCap := GFX_GOSTEK_HELM;

      if SpriteC.Player.HeadCap = 0 then
        SpriteC.WearHelmet := 0
      else
        SpriteC.WearHelmet := 1;

      ReadConf(conf, 'Chain', SpriteC.Player.Chain);
      ReadConf(conf, 'Dummy', SpriteC.Dummy, True);

      SpriteC.Player.ControlMethod := BOT;
      SpriteC.FreeControls;
    end
    else
      raise Exception.Create('Section "[Bot]" not found');

    Result := True;
  except
    on e : Exception do
    begin
      Filename := RightStr(FilePath, Length(FilePath) - LastDelimiter('\', FilePath));
      MainConsole.console(Filename + ': ' + e.message, WARNING_MESSAGE_COLOR);
    end;
  end;

  ini.Free;
  conf.Free;
end;
{$ENDIF}
function LoadWeaponsConfig(const FilePath: string): Boolean;
var
  ini: TMemIniFile;
  conf: TStringList;
  WeaponIndex: Integer;
  Gun: ^TGun;
  Filename: string;
begin
  Result := False;
  ini := nil;
  conf := nil;

  if not FileExists(FilePath) then
    Exit;

  try
    conf := TStringList.Create;
    ini := TMemIniFile.Create(FilePath);

    if ini.SectionExists('Info') then
    begin
      ini.ReadSectionValues('Info', conf);
      ReadConf(conf, 'Name', WMName);
      ReadConf(conf, 'Version', WMVersion);
    end
    else
      raise Exception.Create('Section "[Info]" not found');

    for WeaponIndex := 1 to ORIGINAL_WEAPONS do
    begin
      Gun := @Guns[WeaponIndex];

      if ini.SectionExists(Gun.IniName) then
      begin
        ini.ReadSectionValues(Gun.IniName, conf);

        ReadWMConf(conf, 'Damage',            Gun.HitMultiply);
        ReadWMConf(conf, 'FireInterval',      Gun.FireInterval);
        ReadWMConf(conf, 'Ammo',              Gun.Ammo);
        ReadWMConf(conf, 'ReloadTime',        Gun.ReloadTime);
        ReadWMConf(conf, 'Speed',             Gun.Speed);
        ReadWMConf(conf, 'BulletStyle',       Gun.BulletStyle);
        ReadWMConf(conf, 'StartUpTime',       Gun.StartUpTime);
        ReadWMConf(conf, 'Bink',              Gun.Bink);
        ReadWMConf(conf, 'MovementAcc',       Gun.MovementAcc);
        ReadWMConf(conf, 'BulletSpread',      Gun.BulletSpread);
        ReadWMConf(conf, 'Recoil',            Gun.Recoil);
        ReadWMConf(conf, 'Push',              Gun.Push);
        ReadWMConf(conf, 'InheritedVelocity', Gun.InheritedVelocity);
        ReadWMConf(conf, 'ModifierLegs',      Gun.ModifierLegs);
        ReadWMConf(conf, 'ModifierChest',     Gun.ModifierChest);
        ReadWMConf(conf, 'ModifierHead',      Gun.ModifierHead);
        ReadWMConf(conf, 'NoCollision',       Gun.NoCollision);
      end
    end;

    BuildWeapons();

    Result := True;
  except
    on e : Exception do
    begin
      Filename := RightStr(FilePath, Length(FilePath) - LastDelimiter('\', FilePath));
      MainConsole.console(Filename + ': ' + e.message, WARNING_MESSAGE_COLOR);
    end;
  end;

  ini.Free;
  conf.Free;
end;

end.

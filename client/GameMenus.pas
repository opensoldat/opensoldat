{*******************************************************}
{                                                       }
{       Game Menus Unit for OPENSOLDAT                  }
{                                                       }
{       Copyright (c) 2002 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit GameMenus;

interface

type
  PGameButton = ^TGameButton;
  TGameButton = record
    Active: Boolean;
    x1, y1, x2, y2: Integer;
    Caption: WideString;
  end;

  PGameMenu = ^TGameMenu;
  TGameMenu = record
    Active: Boolean;
    x, y: Integer;
    w, h: Integer;
    Button: array of TGameButton;
  end;

var
  GameMenu: array of TGameMenu;
  HoveredMenu: PGameMenu;
  HoveredButton: PGameButton;
  HoveredButtonIndex: Integer;
  EscMenu:   PGameMenu;
  TeamMenu:  PGameMenu;
  LimboMenu: PGameMenu;
  KickMenu:  PGameMenu;
  MapMenu:   PGameMenu;
  KickMenuIndex: Integer = 0;
  MapMenuIndex: Integer = 0;

procedure InitGameMenus;
procedure GameMenuShow(Menu: PGameMenu; Show: Boolean = True);
function GameMenuAction(Menu: PGameMenu; ButtonIndex: Integer): Boolean;
procedure GameMenuMouseMove();
function GameMenuClick(): Boolean;

implementation

uses
  SDL2, SysUtils, Client, Weapons, Game, GameStrings, ClientGame, Sound, InterfaceGraphics,
  Constants, MainMenuRendering, Net, NetworkClientConnection, NetworkClientSprite, Sprites, Cvar,
  NetworkClientGame, NetworkClientMessages{$IFDEF STEAM}, Steam{$ENDIF};

var
  LimboWasActive: Boolean;

procedure InitButton(Menu: PGameMenu; Button: Integer; Caption: WideString;
  x, y, w, h: Integer; Active: Boolean = True);
begin
  Menu.Button[Button].Active := Active;
  Menu.Button[Button].x1 := Menu.x + x;
  Menu.Button[Button].y1 := Menu.y + y;
  Menu.Button[Button].x2 := Menu.x + x + w;
  Menu.Button[Button].y2 := Menu.y + y + h;
  Menu.Button[Button].Caption := Caption;
end;

procedure InitGameMenus;
var
  i: Integer;
  s: WideString;
begin
  HoveredMenu := nil;
  HoveredButton := nil;
  HoveredButtonIndex := 0;

  SetLength(GameMenu, 5);
  EscMenu   := @GameMenu[0];
  TeamMenu  := @GameMenu[1];
  LimboMenu := @GameMenu[2];
  KickMenu  := @GameMenu[3];
  MapMenu   := @GameMenu[4];

  // esc menu

  EscMenu.w := 300;
  EscMenu.h := 200;

  if r_scaleinterface.Value then
  begin
    EscMenu.x := Round((GameWidth - EscMenu.w) / 2);
    EscMenu.y := Round((GameHeight - EscMenu.h) / 2);
  end
  else
  begin
    EscMenu.x := Round((RenderWidth - EscMenu.w) / 2);
    EscMenu.y := Round((RenderHeight - EscMenu.h) / 2);
  end;

  SetLength(EscMenu.Button, {$IFDEF STEAM}5{$ELSE}4{$ENDIF});
  InitButton(EscMenu, 0, '1 ' + _('Exit to menu'), 5, 1 * 25, 240, 25);
  InitButton(EscMenu, 1, '2 ' + _('Change map'), 5, 2 * 25, 240, 25);
  InitButton(EscMenu, 2, '3 ' + _('Kick player'), 5, 3 * 25, 240, 25);
  InitButton(EscMenu, 3, '4 ' + _('Change team'), 5, 4 * 25, 240, 25);
  {$IFDEF STEAM}
  InitButton(EscMenu, 4, _('Server Website'), 5, 7 * 25, 240, 15);
  {$ENDIF}

  // team menu

  TeamMenu.w := 0;
  TeamMenu.h := 0;
  TeamMenu.x := 0;
  TeamMenu.y := 0;

  SetLength(TeamMenu.Button, 6);
  InitButton(TeamMenu, 0, '0 ' + _('0 Player'), 40, 140 + 40 * 1, 215, 35);
  InitButton(TeamMenu, 1, '1 ' + _('Alpha Team'), 40, 140 + 40 * 1, 215, 35);
  InitButton(TeamMenu, 2, '2 ' + _('Bravo Team'), 40, 140 + 40 * 2, 215, 35);
  InitButton(TeamMenu, 3, '3 ' + _('Charlie Team'), 40, 140 + 40 * 3, 215, 35);
  InitButton(TeamMenu, 4, '4 ' + _('Delta Team'), 40, 140 + 40 * 4, 215, 35);
  InitButton(TeamMenu, 5, '5 ' + _('Spectator'), 40, 140 + 40 * 5, 215, 35);

  // limbo menu

  LimboMenu.w := 0;
  LimboMenu.h := 0;
  LimboMenu.x := 0;
  LimboMenu.y := 0;

  SetLength(LimboMenu.Button, MAIN_WEAPONS);

  for i := 0 to MAIN_WEAPONS - 1 do
  begin
    if i < PRIMARY_WEAPONS then
      s := WideString(IntToStr((i + 1) mod 10)) + ' ' + WideString(GunDisplayName[Guns[i + 1].Num])
    else
      s := WideString(GunDisplayName[Guns[i + 1].Num]);

    InitButton(LimboMenu, i, s, 35, 154 + 18 * (i + Ord(i >= PRIMARY_WEAPONS)), 235, 16);
  end;

  // kick menu

  KickMenu.w := 370;
  KickMenu.h := 90;
  KickMenu.x := 125;
  KickMenu.y := 355;

  SetLength(KickMenu.Button, 4);
  InitButton(KickMenu, 0, '<<<<',  15, 35, 90, 25);
  InitButton(KickMenu, 1, '>>>>', 265, 35, 90, 25);
  InitButton(KickMenu, 2, _('Kick'), 105, 55, 90, 25);
  InitButton(KickMenu, 3, _('Ban'), 195, 55, 80, 25);

  KickMenu.Button[3].Active := False;  // TODO: ban not supported for now

  // map menu

  MapMenu.w := 370;
  MapMenu.h := 90;
  MapMenu.x := 125;
  MapMenu.y := 355;

  SetLength(MapMenu.Button, 3);
  InitButton(MapMenu, 0, '<<<<',  15, 35, 90, 25);
  InitButton(MapMenu, 1, '>>>>', 265, 35, 90, 25);
  InitButton(MapMenu, 2, _('Select'), 120, 55, 90, 25);
end;

procedure HideAll();
var
  i: Integer;
begin
  for i := Low(GameMenu) to High(GameMenu) do
    GameMenu[i].Active := False;
end;

procedure GameMenuShow(Menu: PGameMenu; Show: Boolean = True);
var
  i: Integer;
begin
  if Menu = EscMenu then
  begin
    if Show then
    begin
      if LimboMenu.Active then
        LimboWasActive := True;

      HideAll;
      FragsMenuShow := False;
      StatsMenuShow := False;

      for i := 1 to MAX_PLAYERS do
      begin
        if Sprite[i].Active then
        begin
          StopSound(Sprite[i].ReloadSoundChannel);
          StopSound(Sprite[i].JetsSoundChannel);
          StopSound(Sprite[i].GattlingSoundChannel);
          StopSound(Sprite[i].GattlingSoundChannel2);
        end;
      end;

      if cl_runs.Value < 3 then
        NoobShow := True;

      // TODO: stop playing weather in escmenu
    end
    else
    begin
      HideAll;
      NoobShow := False;
      if LimboWasActive then
        LimboMenu.Active := True;
    end;
  end
  else if (Menu = TeamMenu) and Show then
  begin
    HideAll;

    if Show then case sv_gamemode.Value of
      GAMESTYLE_CTF, GAMESTYLE_INF, GAMESTYLE_HTF: begin
        Menu.Button[0].Active := False;
        Menu.Button[1].Active := True;
        Menu.Button[2].Active := True;
        Menu.Button[3].Active := False;
        Menu.Button[4].Active := False;
      end;
      GAMESTYLE_TEAMMATCH: begin
        Menu.Button[0].Active := False;
        Menu.Button[1].Active := True;
        Menu.Button[2].Active := True;
        Menu.Button[3].Active := True;
        Menu.Button[4].Active := True;
      end;
      else begin
        Menu.Button[0].Active := True;
        Menu.Button[1].Active := False;
        Menu.Button[2].Active := False;
        Menu.Button[3].Active := False;
        Menu.Button[4].Active := False;
      end;
    end;
  end
  else if (Menu = MapMenu) and Show then
  begin
    ClientVoteMap(MapMenuIndex);
    KickMenu.Active := False;
  end
  else if (Menu = KickMenu) and Show then
  begin
    KickMenuIndex := 1;
    MapMenu.Active := False;

    if PlayersNum < 1 then
      Menu := nil;
  end
  else if Menu = LimboMenu then
  begin
    Menu.Active := False;

    if not Show then
      LimboWasActive := False
    else if WeaponsInGame = 0 then
      Menu := nil;
  end;

  if Menu <> nil then
    Menu.Active := Show;

  GameMenuMouseMove();
end;

function GameMenuAction(Menu: PGameMenu; ButtonIndex: Integer): Boolean;
var
  i, Count: Integer;
begin
  Result := False;

  if (ButtonIndex >= 0) and (ButtonIndex < Length(Menu.Button)) and
    Menu.Button[ButtonIndex].Active then
  begin
    if Menu = EscMenu then
    begin
      Result := True;

      case ButtonIndex of
        0: begin
          ClientDisconnect;
          GameLoopRun := False;
        end;
        1: GameMenuShow(MapMenu, not MapMenu.Active);
        2: GameMenuShow(KickMenu, not KickMenu.Active);
        3: begin
          Result := (MySprite > 0) and (MapChangeCounter < 0);

          if Result then
          begin
            GameMenuShow(TeamMenu);
            MapChangeCounter := -60;
            SelTeam := 0;
          end
          else if (MySprite = 0) and IsTeamGame then
          begin
            Result := True;
            GameMenuShow(TeamMenu);
          end;
        end;
        {$IFDEF STEAM}
        4: begin
          if sv_website.Value <> '' then
            SteamAPI.Friends.ActivateGameOverlayToWebPage(PChar(sv_website.Value), k_EActivateGameOverlayToWebPageMode_Default);
        end;
        {$ENDIF}
      end;
    end
    else if Menu = TeamMenu then
    begin
      Result := True;
      GameMenuShow(Menu, False);
      SelTeam := ButtonIndex;

      if (MySprite = 0) or (ButtonIndex <> Sprite[MySprite].Player.Team) then
      begin
        // NOTE this actually sends a change team request
        ClientSendPlayerInfo;
      end;
    end
    else if Menu = KickMenu then
    begin
      i := KickMenuIndex;

      if PlayersNum < 1 then
      begin
        GameMenuShow(KickMenu, False);
      end
      else case ButtonIndex of
        0: begin  // prev
          KickMenuIndex := ((MAX_SPRITES + KickMenuIndex - 2) mod MAX_SPRITES) + 1;
          while not (Sprite[KickMenuIndex].Active or Sprite[KickMenuIndex].Player.DemoPlayer) do
            KickMenuIndex := ((MAX_SPRITES + KickMenuIndex - 2) mod MAX_SPRITES) + 1;

          Result := (KickMenuIndex <> i);
        end;

        1: begin  // next
          KickMenuIndex := (KickMenuIndex mod MAX_SPRITES) + 1;
          while not (Sprite[KickMenuIndex].Active or Sprite[KickMenuIndex].Player.DemoPlayer) do
            KickMenuIndex := (KickMenuIndex mod MAX_SPRITES) + 1;

          Result := (KickMenuIndex <> i);
        end;

        2: begin  // kick
          Result := (KickMenuIndex <> MySprite);

          if Result then
          begin
            GameMenuShow(EscMenu, False);
            ChatText := ' ';
            ChatChanged := True;
            VoteKickReasonType := True;
            SDL_StartTextInput;
          end;
        end;
      end;
    end
    else if Menu = MapMenu then
    begin
      if PlayersNum < 1 then
      begin
        GameMenuShow(KickMenu, False);
      end
      else case ButtonIndex of
        0: begin  // prev
          if MapMenuIndex > 0 then
          begin
            MapMenuIndex := MapMenuIndex - 1;
            ClientVoteMap(MapMenuIndex);
          end;

          Result := (KickMenuIndex <> 0);
        end;

        1: begin  // next
          if MapMenuIndex < VoteMapCount - 1 then
          begin
            MapMenuIndex := MapMenuIndex + 1;
            ClientVoteMap(MapMenuIndex);
          end;

          Result := (MapMenuIndex <= VoteMapCount - 1);
        end;

        2: begin  // vote map
          GameMenuShow(EscMenu, False);
          ClientSendStringMessage('votemap ' + WideString(VoteMapName), MSGTYPE_CMD);
        end;
      end;
    end
    else if (Menu = LimboMenu) and (MySprite > 0) then
    begin
      Result := True;
      i := ButtonIndex + 1;

      if (WeaponActive[i] = 1) and (WeaponSel[MySprite][i] = 1) then
      begin
        if i <= 10 then
        begin
          if (WeaponActive[i] = 1) and (WeaponSel[MySprite][i] = 1) then
            Sprite[MySprite].SelWeapon := Guns[i].Num;

          if Sprite[MySprite].SelWeapon > 0 then
          begin
            GameMenuShow(LimboMenu, False);

            if not Sprite[MySprite].DeadMeat and
              not (Sprite[MySprite].Weapon.Num in [Guns[BOW].Num, Guns[BOW2].Num]) then
            begin
              Sprite[MySprite].ApplyWeaponByNum(Sprite[MySprite].SelWeapon, 1);
              ClientSpriteSnapshot;
            end;
          end;
        end
        else
        begin
          cl_player_secwep.SetValue(i - 11);
          Sprite[MySprite].Player.SecWep := i - 11;
          Sprite[MySprite].ApplyWeaponByNum(Guns[i].Num, 2);

          Count := 0;
          for i := 1 to PRIMARY_WEAPONS do
            Inc(Count, WeaponActive[i]);

          if Count = 0 then
          begin
            GameMenuShow(LimboMenu, False);
            Sprite[MySprite].Weapon := Sprite[MySprite].SecondaryWeapon;
            Sprite[MySprite].SecondaryWeapon := Guns[NOWEAPON];
          end;

          if not Sprite[MySprite].DeadMeat then
            ClientSpriteSnapshot;
        end;
      end;
    end;

    if Result then
      PlaySound(SFX_MENUCLICK);
  end;
end;

procedure GameMenuMouseMove();
var
  i, j: Integer;
  x, y: Single;
  Btn: PGameButton;
begin
  HoveredMenu := nil;
  HoveredButton := nil;
  HoveredButtonIndex := 0;

  x := mx * _rscala.x;
  y := my * _rscala.y;

  for i := Low(GameMenu) to High(GameMenu) do
  begin
    if GameMenu[i].Active then
    begin
      for j := Low(GameMenu[i].Button) to High(GameMenu[i].Button) do
      begin
        Btn := @GameMenu[i].Button[j];

        if Btn.Active and (x > Btn.x1) and (x < Btn.x2) and
          (y > Btn.y1) and (y < Btn.y2) then
        begin
          HoveredMenu := @GameMenu[i];
          HoveredButton := Btn;
          HoveredButtonIndex := j;
          Exit;
        end;
      end;
    end;
  end;
end;

function GameMenuClick(): Boolean;
begin
  Result := False;

  if HoveredButton <> nil then
    Result := GameMenuAction(HoveredMenu, HoveredButtonIndex);
end;

end.

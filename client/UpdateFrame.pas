{*******************************************************}
{                                                       }
{       UpdateFrame Unit for OPENSOLDAT                 }
{                                                       }
{       Copyright (c) 2003 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit UpdateFrame;

interface

uses
  {$IFDEF STEAM}Steam,{$ENDIF}
  Sound, Demo, Classes, GameStrings, GameRendering, Sprites, Vector, Weapons, Net,
  NetworkClientConnection, Constants,
  Polymap, Game, Client, Util, SysUtils, Calc, LogFile, WeatherEffects, Sparks
  {$IFDEF ENABLE_FAE}, FaeClient{$ENDIF}
  ;

procedure Update_Frame;

implementation

uses
  ClientGame, InterfaceGraphics, GameMenus;

var
  IdleCounter, OldMouseX: Integer;

procedure Update_Frame;
var
  j: Integer;
  Norm, CamV, S, M, P: TVector2;
  DisplayRatio: Single;
  ScreenFile: String;
begin
  CheckSynchronize;

  {$IFDEF ENABLE_FAE}
  FaeOnTick;
  {$ENDIF}

  CameraPrev.x := CameraX;
  CameraPrev.y := CameraY;
  MousePrev.x := mx;
  MousePrev.y := my;

  if MapChangeCounter < 0 then
  begin
    if DemoPlayer.Active and EscMenu.Active then
      Exit;

    for j := 1 to MAX_SPRITES do
      if Sprite[j].Active then
        if Sprite[j].IsNotSpectator() then
          if (ClientStopMovingCounter > 0) then
            SpriteParts.DoEulerTimeStepFor(j);  // integrate sprite particles

    for j := 1 to MAX_SPRITES do
      if Sprite[j].Active then
        Sprite[j].Update;  // update sprite

    // Bullets update
    for j := 1 to MAX_BULLETS do
    begin
      if Bullet[j].Active then
        Bullet[j].Update;

      if Bullet[j].PingAdd > 0 then
        Dec(Bullet[j].PingAdd, 4);
    end;

    BulletParts.DoEulerTimeStep;

    SparksCount := 0;
    for j := 1 to MAX_SPARKS do
      if Spark[j].Active then
      begin
        Spark[j].Update;
        Inc(SparksCount);
      end;

    // update Things
    for j := 1 to MAX_THINGS do
      if Thing[j].Active then
        Thing[j].Update;

    if MainTickCounter mod SECOND = 0 then
      if ScreenCounter <> 255 then
        // TODO: don't rely on underflow
        ScreenCounter := $FF and (ScreenCounter - 1);

    // Change spectate target away from dead player
    if MainTickCounter mod (SECOND * 5) = 0 then
      if (CameraFollowSprite > 0) and Sprite[CameraFollowSprite].DeadMeat and
        (sv_realisticmode.Value) and (sv_survivalmode.Value) and not SurvivalEndRound then
      begin
        CameraFollowSprite := GetCameraTarget();
      end;

    // Weather effects
    if r_weathereffects.Value then
      case Map.Weather of
        1: MakeRain;
        2: MakeSandStorm;
        3: MakeSnow;
      end;
  end  // mapchangecounter < 0
  else
  begin
    // allow camera switching in demos while paused
    //if DemoPlay then
    //  for j := 1 to MAX_SPRITES do
    //   if Sprite[j].Active then
    //    ControlSprite(Sprite[j]);
  end;

  {$IFDEF STEAM}
  if VoiceSpeakingNow then
    GetMicData;
  {$ENDIF}

  // >> cursor on player <<
  CursorText := '';
  CursorFriendly := False;

  // TODO(helloer): While watching demos this code needs to use SpectNumber instead of MySprite
  if (MySprite > 0) and (not DemoPlayer.Active) then
    for j := 1 to MAX_SPRITES do
      if Sprite[j].Active and Sprite[j].IsNotSpectator() and
          (j <> MySprite) and (Sprite[j].BonusStyle <> BONUS_PREDATOR) and
          ((Sprite[j].Position = POS_STAND) or
          (Sprite[j].IsNotSolo() and Sprite[j].IsInSameTeam(Sprite[MySprite])) or
          Sprite[MySprite].DeadMeat or Sprite[j].DeadMeat) and
          ((Sprite[j].Visible > 40) or (not sv_realisticmode.Value)) then
      begin
        if Distance(-GameWidthHalf + camerax + mx, -GameHeightHalf + cameray + my,
          Spriteparts.Pos[j].X, Spriteparts.Pos[j].Y) <
          CURSORSPRITE_DISTANCE then
        begin
          CursorText := Sprite[j].Player.Name;
          if IsTeamGame() then
            if Sprite[j].IsInSameTeam(Sprite[MySprite]) then
            begin
              CursorText := CursorText + ' ' +
                IntToStr(Round((Sprite[j].Health / STARTHEALTH) * 100)) + '%';
              CursorFriendly := True;
            end;

          Break;
        end;
      end;
  CursorTextLength := Length(CursorText);

  // bullet timer
  if BulletTimeTimer > -1 then
    Dec(BulletTimeTimer);

  if BulletTimeTimer = 0 then
  begin
    ToggleBulletTime(False);
    BulletTimeTimer := -1;
  end
  else if BulletTimeTimer < 1 then
  begin
    // MapChange counter update
    if (MapChangeCounter > -60) and (MapChangeCounter < 99999999) then
      MapChangeCounter := MapChangeCounter - 1;
    if (MapChangeCounter < 0) and (MapChangeCounter > -59) then
      if MapChangeName <> 'EXIT*!*' then
      begin
        ChangeMap;
        ResetWeaponStats;
      end;

      // Game Stats save
    if MainTickCounter mod log_filesupdate.Value = 0 then
    begin
      if log_enable.Value then
      begin
        if (CheckFileSize(ConsoleLogFileName) > MAX_LOGFILESIZE) then
          NewLogFiles;

        WriteLogFile(GameLog, ConsoleLogFileName);
      end;
    end;

    if MainTickCounter mod (SECOND * 6) = 0 then
    begin
      if PlayersNum = 0 then
        if MapChangeCounter > 99999999 then
          MapChangeCounter := -60;
    end;

    SinusCounter := SinusCounter + ILUMINATESPEED;

    if GrenadeEffectTimer > -1 then
      GrenadeEffectTimer := GrenadeEffectTimer - 1;

    // Spray counter
    if HitSprayCounter > 0 then
      Dec(HitSprayCounter);

    // Idle counter
    if MySprite > 0 then
      if MapChangeCounter < 99999999 then
        if Sprite[MySprite].IsNotSpectator() and
            (not Sprite[MySprite].Player.DemoPlayer) then
        begin
          if OldMouseX - Round(mx) = 0 then
            Inc(IdleCounter)
          else
            IdleCounter := 0;

          if IdleCounter > IDLE_KICK then
          begin
            RenderGameInfo(_('Idle kick (didn''t move mouse for > 3 minutes)'));
            ClientDisconnect;
            ExitToMenu;
          end;

          OldMouseX := Round(mx);
        end;

    // Time Limit decrease
    if (MapChangeCounter < 99999999) then
      if TimeLimitCounter > 0 then
        TimeLimitCounter := TimeLimitCounter - 1;

    TimeLeftMin := TimeLimitCounter div 3600;
    TimeLeftSec := (TimeLimitCounter - TimeLeftMin * 3600) div 60;

    if TimeLimitCounter > 0 then
      if TimeLimitCounter < 601 then
      begin
        if TimeLimitCounter mod 60 = 0 then
          if MapChangeCounter = -60 then
          begin
            MainConsole.Console(_('Time Left:') + ' ' +
              WideString(IntToStr(TimeLimitCounter div 60)) + ' ' + _('seconds'),
                GAME_MESSAGE_COLOR);
            PlaySound(SFX_SIGNAL);
          end;
      end
      else if TimeLimitCounter < 3601 then
      begin
        if TimeLimitCounter mod 600 = 0 then
        begin
          MainConsole.Console(_('Time Left:') + ' ' +
            WideString(IntToStr(TimeLimitCounter div 60)) + ' ' + _('seconds'),
            GAME_MESSAGE_COLOR);
          PlaySound(SFX_SIGNAL);
        end;
      end
      else if TimeLimitCounter < 18001 then
      begin
        if TimeLimitCounter mod 3600 = 0 then
        begin
          MainConsole.Console(_('Time Left:') + ' ' +
            WideString(IntToStr(TimeLimitCounter div 3600)) + ' ' + _('minutes'),
            GAME_MESSAGE_COLOR);
          PlaySound(SFX_SIGNAL);
        end;
      end
      else if TimeLimitCounter mod 18000 = 0 then
      begin
        MainConsole.Console(_('Time Left:') + ' ' +
          WideString(IntToStr(TimeLimitCounter div 3600)) + ' ' + _('minutes'),
          GAME_MESSAGE_COLOR);
        PlaySound(SFX_SIGNAL);
      end;

    // Map voting timer
    TimerVote;

    // Chat Update
    for j := 1 to MAX_SPRITES do
      if ChatDelay[j] > 0 then
        ChatDelay[j] := ChatDelay[j] - 1;

    // Big and World Message update
    for j := 0 to MAX_BIG_MESSAGES do
    begin
      if BigDelay[j] > 0 then
        BigDelay[j] := BigDelay[j] - 1;
      if WorldDelay[j] > 0 then
        WorldDelay[j] := WorldDelay[j] - 1;
    end;

    // Shot dist update
    if ShotDistanceShow > 0 then
      ShotDistanceShow := ShotDistanceShow - 1;

    // Consoles Update
    MainConsole.ScrollTick := MainConsole.ScrollTick + 1;
    if MainConsole.ScrollTick = MainConsole.ScrollTickMax then
      MainConsole.ScrollConsole;

    if MainConsole.AlphaCount > 0 then
      MainConsole.AlphaCount := MainConsole.AlphaCount - 1;

    KillConsole.ScrollTick := KillConsole.ScrollTick + 1;
    if KillConsole.ScrollTick = KillConsole.ScrollTickMax then
    begin
      KillConsole.ScrollConsole;
      if (KillConsole.Count > 0) and
        (KillConsole.NumMessage[KillConsole.Count] = -255) then
        KillConsole.ScrollConsole;
    end;

    if ChatTimeCounter > 0 then
      ChatTimeCounter := ChatTimeCounter - 1;
  end;  // bullettime off

  // MOVE -=CAMERA=-
  if (CameraFollowSprite > 0) and (CameraFollowSprite < MAX_SPRITES + 1) then
  begin
    if Sprite[CameraFollowSprite].Active and Sprite[CameraFollowSprite].IsNotSpectator() then
    begin
      // FIXME(skoskav): Scope zoom and non-default resolution makes this a bit complicated. Why
      // does the magic number ~6.8 work so well?

      M.X := exp(r_zoom.Value) * ((mx - GameWidthHalf) / Sprite[CameraFollowSprite].AimDistCoef *
        ((2 * 640 / GameWidth - 1) +
        (GameWidth - 640) / GameWidth * (DEFAULTAIMDIST - Sprite[CameraFollowSprite].AimDistCoef) / 6.8));

      M.Y := exp(r_zoom.Value) * ((my - GameHeightHalf) / Sprite[CameraFollowSprite].AimDistCoef);
      CamV.X := CameraX;
      CamV.Y := CameraY;
      P.X := SpriteParts.Pos[CameraFollowSprite].X;
      P.Y := SpriteParts.Pos[CameraFollowSprite].Y;
      Norm := Vec2Subtract(P, CamV);
      Vec2Scale(S, Norm, CAMSPEED);
      CamV := Vec2Add(CamV, S);
      CamV := Vec2Add(CamV, M);
      CameraX := CamV.X;
      CameraY := CamV.Y;
    end
    else
      CameraFollowSprite := 0;
  end
  else if CameraFollowSprite = 0 then
  begin
    DisplayRatio := GameWidth / 640;

    if (mx > 310 * DisplayRatio) and (mx < 330 * DisplayRatio) and
       (my > 230) and (my < 250) then
    begin
      M.X := 0;
      M.Y := 0;
    end
    else
    begin
      M.X := (mx - GameWidthHalf) / SPECTATORAIMDIST;
      M.Y := (my - GameHeightHalf) / SPECTATORAIMDIST;
    end;
    CamV.X := CameraX;
    CamV.Y := CameraY;
    CamV := Vec2Add(CamV, M);
    CameraX := CamV.X;
    CameraY := CamV.Y;
  end;

  // safety
  if (MySprite > 0) and (Sprite[MySprite].IsSpectator) then
    if (CameraX > MAX_SECTORZ * Map.SectorsDivision) or
        (CameraX < MIN_SECTORZ * Map.SectorsDivision) or
        (CameraY > MAX_SECTORZ * Map.SectorsDivision) or
        (CameraY < MIN_SECTORZ * Map.SectorsDivision) then
    begin
      CameraX := 0;
      CameraY := 0;
      TargetMode := False;
    end;

  // end game screen
  if ScreenTaken then
    if MapChangeCounter < (DEFAULT_MAPCHANGE_TIME / 3) then
    begin
      ScreenTaken := False;
      {$IFDEF STEAM}
      if cl_steam_screenshots.Value then
      begin
        SteamAPI.Screenshots.TriggerScreenshot();
        Exit;
      end;
      {$ENDIF}
      ScreenFile := UserDirectory + 'screens/' +
        FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now()) + Map.Name +
        '_endgame.png';
      TakeScreenShot(ScreenFile);
    end;

  if ((demo_autorecord.Value) and (DemoRecorder.Active = False) and (Map.Name <> '')) then
  begin
    DemoRecorder.StartRecord(UserDirectory + 'demos/'  +
      FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now()) + Map.Name + '.sdm');
  end;
end;

end.

{*******************************************************}
{                                                       }
{       Control Unit for OPENSOLDAT                     }
{                                                       }
{       Copyright (c) 2003 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Control;

interface

uses
  {$IFNDEF SERVER}
  SDL2, Sound, Demo, ClientGame, Input, GameMenus, Sparks,
  {$ENDIF}
  {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} Util, SysUtils, Calc, Math, Game, Sprites, Bullets,
  Vector, Weapons, Constants, Net
  {$IFDEF SERVER}, NetworkServerGame, NetworkServerSprite, AI {$ENDIF}
  {$IFNDEF SERVER}, NetworkClientMessages, NetworkClientSprite, NetworkClientGame{$ENDIF};
  procedure ControlSprite(var SpriteC: TSprite);

implementation

{$IFNDEF SERVER}
var
  WasRunningLeft:     Boolean;
  WasJumping:         Boolean;
  WasThrowingGrenade: Boolean;
  WasChangingWeapon:  Boolean;
  WasThrowingWeapon:  Boolean;
  WasReloadingWeapon: Boolean;
  FreeCamPressed: Boolean;
{$ENDIF}

function CheckSpriteLineOfSightVisibility(var LookSprite: TSprite; var SpriteToCheck: TSprite): Boolean;
var
  StartPoint, LookPoint: TVector2;
  D2: Single = 0.0;
begin
  Result := False;
  // Do we look in the right direction?
  StartPoint.X := SpriteToCheck.Skeleton.Pos[7].X - LookSprite.Skeleton.Pos[7].X;
  StartPoint.Y := SpriteToCheck.Skeleton.Pos[7].Y - (LookSprite.Skeleton.Pos[7].Y - 2);
  LookPoint.X := LookSprite.Control.MouseAimX - LookSprite.Skeleton.Pos[7].X;
  LookPoint.Y := LookSprite.Control.MouseAimY - (LookSprite.Skeleton.Pos[7].Y - 2);
  Vec2Normalize(StartPoint, StartPoint);
  Vec2Normalize(LookPoint, LookPoint);
  if Vec2Dot(StartPoint, LookPoint) > 0.0 then  // 0.5 = 90 fov, 0.0 = 180 fov, -0.5 = 270 fov
  begin
    LookPoint.X := LookSprite.Skeleton.Pos[7].X;
    LookPoint.Y := LookSprite.Skeleton.Pos[7].Y - 2;
    StartPoint := SpriteToCheck.Skeleton.Pos[7];
    // Is it even possible to see the player
    if not Map.RayCast(LookPoint, StartPoint, D2, 1001, False, False, False) then
      Result := True;
  end;

end;

function AreConflictingKeysPressed(var SpriteC: TSprite): Boolean;
begin
  // True if more than one of the keys are pressed
  Result := Integer(SpriteC.Control.ThrowNade) + Integer(SpriteC.Control.ChangeWeapon) +
            Integer(SpriteC.Control.ThrowWeapon) + Integer(SpriteC.Control.Reload) > 1;
end;

procedure ControlSprite(var SpriteC: TSprite);
var
  a, b, lookpoint, startpoint: TVector2;
  i, j: Integer;
  {$IFNDEF SERVER}
  CameraTarget: Integer;
  {$ENDIF}
  D: Single;
  TempGun: TGun;
  PlayerVelocity: TVector2;
  PlayerPressedLeftRight: Boolean = False;
  Unprone: Boolean;
begin
  case SpriteC.Style of
    1:  // Gostek
      begin
        // safety
        if (SpriteC.Weapon.AmmoCount > SpriteC.Weapon.Ammo) or
          (SpriteC.Weapon.FireIntervalCount > SpriteC.Weapon.FireInterval) or
          (SpriteC.Weapon.ReloadTimeCount > SpriteC.Weapon.ReloadTime) then
        begin
          SpriteC.ApplyWeaponByNum(SpriteC.Weapon.Num, 1);
          SpriteC.Weapon.AmmoCount := 0;
          {$IFNDEF SERVER}
          if (SpriteC.Num = MySprite) and not SpriteC.DeadMeat then
          ClientSpriteSnapshot;
          {$ENDIF}
        end;

        if (SpriteC.LegsAnimation.Speed < 1) then
          SpriteC.LegsAnimation.Speed := 1;
        if (SpriteC.BodyAnimation.Speed < 1) then
          SpriteC.BodyAnimation.Speed := 1;
          {$IFNDEF SERVER}
          if (SpriteC.Num = MySprite) and not EscMenu.Active then
          begin
            SpriteC.FreeControls;

            Sprite[MySprite].Control.MouseAimX := Round(mx - GameWidthHalf + camerax);
            Sprite[MySprite].Control.MouseAimY := Round(my - GameHeightHalf + cameray);

            if not TeamMenu.Active and not LimboMenu.Active then
            begin
              if ChatText = '' then
              begin
                for i := Low(Binds) to High(Binds) - 1 do
                begin
                  if KeyStatus[Binds[i].keyId] and ((Binds[i].keymod = 0) or (Binds[i].keyMod and SDL_GetModState() <> 0)) then
                  begin
                    if Binds[i].action = TAction.Left then SpriteC.Control.Left := True;
                    if Binds[i].action = TAction.Right then SpriteC.Control.Right := True;
                    if Binds[i].action = TAction.Jump then SpriteC.Control.Up := True;
                    if Binds[i].action = TAction.Crouch then SpriteC.Control.Down := True;
                    if Binds[i].action = TAction.Fire then SpriteC.Control.Fire := True;
                    if Binds[i].action = TAction.Jet then SpriteC.Control.Jetpack := True;
                    if Binds[i].action = TAction.Reload then SpriteC.Control.Reload := True;
                    if Binds[i].action = TAction.ChangeWeapon then SpriteC.Control.ChangeWeapon := True;
                    if Binds[i].action = TAction.ThrowGrenade then SpriteC.Control.ThrowNade := True;
                    if Binds[i].action = TAction.DropWeapon then SpriteC.Control.ThrowWeapon := True;
                    if Binds[i].action = TAction.Prone then SpriteC.Control.Prone := True;
                    if Binds[i].action = TAction.FlagThrow then
                    begin
                        SpriteC.Control.FlagThrow := True;
                    end else
                    begin
                      SpriteC.Control.FlagThrow := SpriteC.Control.Up and SpriteC.Control.Down;
                    end;
                  end;
                end;

                // If both left and right directions are pressed, then decide which direction to go in
                if SpriteC.Control.Left and SpriteC.Control.Right then
                begin
                  // Remember that both directions were pressed, as it's useful for some moves
                  PlayerPressedLeftRight := True;

                  if WasJumping then
                  begin
                    // If jumping, keep going in the old direction
                    if WasRunningLeft then
                      SpriteC.Control.Right := False
                    else
                      SpriteC.Control.Left := False;
                  end
                  else
                  begin
                    // If not jumping, instead go in the new direction
                    if WasRunningLeft then
                      SpriteC.Control.Left := False
                    else
                      SpriteC.Control.Right := False;
                  end;
                end
                else
                begin
                  WasRunningLeft := SpriteC.Control.Left;
                  WasJumping := SpriteC.Control.Up;
                end;

                // Handle simultaneous key presses that would conflict
                if AreConflictingKeysPressed(SpriteC) then
                begin
                  // At least two buttons pressed, so deactivate any previous one
                  if WasThrowingGrenade then
                    SpriteC.Control.ThrowNade := False
                  else if WasChangingWeapon then
                    SpriteC.Control.ChangeWeapon := False
                  else if WasThrowingWeapon then
                    SpriteC.Control.ThrowWeapon := False
                  else if WasReloadingWeapon then
                    SpriteC.Control.Reload := False;

                  // If simultaneously pressing two or more new buttons, then deactivate them in order
                  // of least prefecence
                  while AreConflictingKeysPressed(SpriteC) do
                  begin
                    if SpriteC.Control.Reload then
                      SpriteC.Control.Reload := False
                    else if SpriteC.Control.ChangeWeapon then
                      SpriteC.Control.ChangeWeapon := False
                    else if SpriteC.Control.ThrowWeapon then
                      SpriteC.Control.ThrowWeapon := False
                    else if SpriteC.Control.ThrowNade then
                      SpriteC.Control.ThrowNade := False;
                  end;
                end
                else
                begin
                  // At most one of these will be true
                  WasThrowingGrenade := SpriteC.Control.ThrowNade;
                  WasChangingWeapon  := SpriteC.Control.ChangeWeapon;
                  WasThrowingWeapon  := SpriteC.Control.ThrowWeapon;
                  WasReloadingWeapon := SpriteC.Control.Reload;
                end;
              end;
            end;

            if (Length(ChatText) > 0) and KeyStatus[301] then
            begin
              SDL_StopTextInput;
              FireChatType := ChatType;
              FireChatText := ChatText;
              ChatText := '';
            end;

            if FreeCamPressed and
               not (SpriteC.Control.Fire or SpriteC.Control.Jetpack or SpriteC.Control.Up) then
              FreeCamPressed := False;

            // change camera when dead
            if MenuTimer < 1 then
              if not LimboMenu.Active then
                if SpriteC.DeadMeat then
                begin
                  if SpriteC.Control.Fire or SpriteC.Control.Jetpack or SpriteC.Control.Up then
                    if not DemoPlayer.Active or (FreeCam = 1) then
                    begin
                      if PlayersNum < 1 then
                        Exit;

                      i := 0;
                      for j := 1 to MAX_SPRITES do
                        if Sprite[j].Active then
                          if Sprite[j].IsNotSpectator() and
                             (Sprite[j].IsInSameTeam(Sprite[MySprite]) or
                              Sprite[MySprite].IsSpectator()) then
                          begin
                            i := 1;
                            Break;
                          end;

                      CameraTarget := iif(i = 1, GetCameraTarget(SpriteC.Control.JetPack), 0);

                      if (CameraTarget > 0) or not FreeCamPressed then
                      begin
                        CameraFollowSprite := CameraTarget;

                        mx := GameWidthHalf;
                        my := GameHeightHalf;
                        MousePrev.x := mx;
                        MousePrev.y := my;
                        SpriteC.Control.Fire := False;
                        SpriteC.Control.Jetpack := False;
                        MenuTimer := 10;
                        FreeCamPressed := CameraTarget = 0;
                      end;
                    end;
                end;

          // Fog of War
          if sv_realisticmode.Value then
          begin
            for i := 1 to MAX_SPRITES do
              if Sprite[i].Visible > 0 then
                Dec(Sprite[i].Visible);

            LookPoint.X := SpriteC.Skeleton.Pos[7].X;
            LookPoint.Y := SpriteC.Skeleton.Pos[7].Y - 2;
            SpriteC.Visible := 45;

            for i := 1 to MAX_SPRITES do
              if Sprite[i].Active then
              begin
                // Following sprites
                if (SpriteC.Num <> CameraFollowSprite) and
                   SpriteC.IsNotInSameTeam(Sprite[i]) and
                   SpriteC.IsNotSpectator() then
                begin
                  if CheckSpriteLineOfSightVisibility(Sprite[CameraFollowSprite], Sprite[i]) then
                    Sprite[i].Visible := 45;
                end else
                begin
                  if SpriteC.DeadMeat or
                     ((IsTeamGame() and Sprite[i].IsInSameTeam(SpriteC)) or
                      ((not IsTeamGame) and Sprite[i].IsNotInSameTeam(SpriteC))) then
                  begin
                    Sprite[i].Visible := 45
                  end else
                  begin
                    if CheckSpriteLineOfSightVisibility(SpriteC, Sprite[i]) then
                      Sprite[i].Visible := 45;
                  end;
                end;
              end;
          end;
        end;
        {$ENDIF}
        {$IFDEF SERVER}
        ControlBot(SpriteC);
        {$ENDIF}

        if SpriteC.DeadMeat then
          SpriteC.FreeControls;
        if MapChangeCounter > 0 then
          SpriteC.FreeControls;

        SpriteC.Fired := 0;

        SpriteC.Control.MouseAimX := Round(SpriteC.Control.MouseAimX + SpriteParts.Velocity[SpriteC.Num].X);
        SpriteC.Control.MouseAimY := Round(SpriteC.Control.MouseAimY + SpriteParts.Velocity[SpriteC.Num].Y);

        // use weapons
        b.x := 0;
        b.y := 0;

        if SpriteC.Control.Jetpack and
           (((SpriteC.LegsAnimation.ID = JumpSide.ID) and
             (((SpriteC.Direction = -1) and SpriteC.Control.Right) or
              ((SpriteC.Direction = 1) and SpriteC.Control.Left) or
              PlayerPressedLeftRight)) or
            ((SpriteC.LegsAnimation.ID = RollBack.ID) and SpriteC.Control.Up)) then
        begin
          SpriteC.BodyApplyAnimation(RollBack, 1);
          SpriteC.LegsApplyAnimation(RollBack, 1);
        end
        else
          if SpriteC.Control.Jetpack and (SpriteC.JetsCount > 0) then
          begin
            if SpriteC.OnGround then
              Spriteparts.Forces[SpriteC.Num].Y :=
                -2.5 * iif(GRAV > 0.05, JETSPEED, Grav * 2);

            if not SpriteC.OnGround then
            begin
              if SpriteC.Position <> POS_PRONE then
                Spriteparts.Forces[SpriteC.Num].Y :=
                  Spriteparts.Forces[SpriteC.Num].Y - iif(GRAV > 0.05, JETSPEED, Grav * 2)
              else
                Spriteparts.Forces[SpriteC.Num].X := Spriteparts.Forces[SpriteC.Num].X +
                  (SpriteC.Direction * iif(GRAV > 0.05, JETSPEED, Grav * 2) / 2);
            end;

            if (SpriteC.LegsAnimation.ID <> GetUp.ID) and
               (SpriteC.BodyAnimation.ID <> Roll.ID) and
               (SpriteC.BodyAnimation.ID <> RollBack.ID) then
              SpriteC.LegsApplyAnimation(Fall, 1);
            {$IFNDEF SERVER}
            a.x := SpriteC.Skeleton.Pos[1].X - 1;
            a.y := SpriteC.Skeleton.Pos[1].Y + 3;

            b := Vec2Subtract(SpriteC.Skeleton.Pos[5], SpriteC.Skeleton.Pos[4]);
            Vec2Normalize (b, b);
            Vec2Scale(b, b, -0.5);

            // smoke
            if Random(8) = 0 then
              CreateSpark(a, SpriteParts.Velocity[SpriteC.Num], 1, SpriteC.Num, 75);
            // sparks
            if Random(7) = 0 then
              CreateSpark(a, b, 62, SpriteC.Num, 40);

            a.x := SpriteC.Skeleton.Pos[2].X - 1;
            a.y := SpriteC.Skeleton.Pos[2].Y + 3;
            b := Vec2Subtract(SpriteC.Skeleton.Pos[6], SpriteC.Skeleton.Pos[3]);
            Vec2Normalize(b, b);
            Vec2Scale(b, b, -0.5);

            // smoke
            if Random(8) = 0 then
              CreateSpark(a, SpriteParts.Velocity[SpriteC.Num], 1, SpriteC.Num, 75);
            // sparks
            if Random(7) = 0 then
              CreateSpark(a, b, 62, SpriteC.Num, 40);

            {$ENDIF}
            Dec(SpriteC.JetsCount);
            {$IFNDEF SERVER}
            if (SpriteC.JetsCount = 1) and SpriteC.Control.Jetpack then
              SpriteC.JetsCount := 0;

            // play rockets sound
            PlaySound(SFX_ROCKETZ, SpriteParts.Pos[SpriteC.Num], SpriteC.JetsSoundChannel);
            {$ENDIF}
          end
          else
          begin
           {$IFNDEF SERVER}
           StopSound(SpriteC.JetsSoundChannel);
           {$ENDIF}
          end;
        // Jets

        // KOLBA
        if SpriteC.Stat = 0 then
          if SpriteC.Position = POS_STAND then
            if (SpriteC.Control.Fire) and (SpriteC.CeaseFireCounter < 0) then
              if (SpriteC.Weapon.Num <> Guns[NOWEAPON].Num) and
                 (SpriteC.Weapon.Num <> Guns[KNIFE].Num) and
                 (SpriteC.Weapon.Num <> Guns[CHAINSAW].Num) then
              begin
                for i := 1 to MAX_SPRITES do
                  if Sprite[i].Active and not Sprite[i].DeadMeat and
                     (Sprite[i].Position = POS_STAND) and (i <> SpriteC.Num) and
                     Sprite[i].IsNotSpectator() then
                    if Distance(Spriteparts.Pos[SpriteC.Num], Spriteparts.Pos[i]) < MELEE_DIST then
                      SpriteC.BodyApplyAnimation(Melee, 1);
              end;

        // FIRE!!!!
        // (not TargetMode or (SpriteC.Num <> MySprite))
        if (SpriteC.Stat = 0)  then
        begin
          if (SpriteC.Weapon.Num = Guns[CHAINSAW].Num) or
             ((SpriteC.BodyAnimation.ID <> Roll.ID) and
              (SpriteC.BodyAnimation.ID <> RollBack.ID) and
              (SpriteC.BodyAnimation.ID <> Melee.ID) and
              (SpriteC.BodyAnimation.ID <> Change.ID)) then
          begin
            if ((SpriteC.BodyAnimation.ID = HandsUpAim.ID) and
                (SpriteC.BodyAnimation.CurrFrame = 11)) or
               (SpriteC.BodyAnimation.ID <> HandsUpAim.ID) then
            begin
              if SpriteC.Control.Fire and (SpriteC.CeaseFireCounter < 0) then
              begin
                if (SpriteC.Weapon.Num = Guns[NOWEAPON].Num) or
                   (SpriteC.Weapon.Num = Guns[KNIFE].Num) then
                  SpriteC.BodyApplyAnimation(Punch, 1)
                else
                begin
                  if (SpriteC.Weapon.FireIntervalCount = 0) and (SpriteC.Weapon.AmmoCount > 0) then
                  begin
                    if SpriteC.Weapon.StartUpTime > 0 then
                    begin
                      {$IFNDEF SERVER}
                      StopSound(SpriteC.GattlingSoundChannel2);
                      {$ENDIF}

                      if SpriteC.Weapon.StartUpTimeCount > 0 then
                      begin
                        {$IFNDEF SERVER}
                        if SpriteC.Weapon.StartUpTimeCount = SpriteC.Weapon.StartUpTime then
                        begin
                          // Barrett wind up
                          if SpriteC.Weapon.Num = Guns[BARRETT].Num then
                            PlaySound(SFX_LAW_START, SpriteParts.Pos[SpriteC.Num], SpriteC.GattlingSoundChannel)

                          // Minigun wind up
                          else if SpriteC.Weapon.Num = Guns[MINIGUN].Num then
                            PlaySound(SFX_MINIGUN_START, SpriteParts.Pos[SpriteC.Num], SpriteC.GattlingSoundChannel)

                          // LAW wind up
                          else if SpriteC.Weapon.Num = Guns[LAW].Num then
                          begin
                              if SpriteC.OnGround and
                                (((SpriteC.LegsAnimation.ID = Crouch.ID) and
                                (SpriteC.LegsAnimation.CurrFrame > 13)) or
                                (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
                                (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) or
                                ((SpriteC.LegsAnimation.ID = Prone.ID) and
                                (SpriteC.LegsAnimation.CurrFrame > 23))) then
                                  PlaySound(SFX_LAW_START, SpriteParts.Pos[SpriteC.Num], SpriteC.GattlingSoundChannel)
                          end;
                        end;
                        {$ENDIF}
                        if (SpriteC.Weapon.Num <> Guns[LAW].Num) or
                           ((SpriteC.OnGround or SpriteC.OnGroundPermanent) and
                            (((SpriteC.LegsAnimation.ID = Crouch.ID) and
                              (SpriteC.LegsAnimation.CurrFrame > 13)) or
                             (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
                             (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) or
                             ((SpriteC.LegsAnimation.ID = Prone.ID) and
                              (SpriteC.LegsAnimation.CurrFrame > 23)))) then
                          Dec(SpriteC.Weapon.StartUpTimeCount);
                      end
                      else
                        SpriteC.Fire();
                    end
                    else
                      SpriteC.Fire();
                  end;
                end;
              end
              else
              begin
                {$IFNDEF SERVER}
                StopSound(SpriteC.GattlingSoundChannel);

                if SpriteC.Weapon.StartUpTimeCount < SpriteC.Weapon.StartUpTime then
                begin
                  if (SpriteC.Weapon.Num = Guns[MINIGUN].Num) then
                      // gattling end sound
                      PlaySound(SFX_MINIGUN_END, SpriteParts.Pos[SpriteC.Num], SpriteC.GattlingSoundChannel2);
                  if (SpriteC.Weapon.Num = Guns[LAW].Num) then
                    if (SpriteC.OnGround) and
                      (((SpriteC.LegsAnimation.ID = Crouch.ID) and
                      (SpriteC.LegsAnimation.CurrFrame > 13)) or
                      (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
                      (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) or
                      ((SpriteC.LegsAnimation.ID = Prone.ID) and
                      (SpriteC.LegsAnimation.CurrFrame > 23))) then
                        // LAW wind down sound
                        PlaySound(SFX_LAW_END, SpriteParts.Pos[SpriteC.Num], SpriteC.GattlingSoundChannel2);
                end;

                SpriteC.Weapon.StartUpTimeCount := SpriteC.Weapon.StartUpTime;
                {$ENDIF}
              end;
            end;
          end
          else
          begin
            if SpriteC.Weapon.StartUpTimeCount < SpriteC.Weapon.StartUpTime then
            begin
              SpriteC.Weapon.StartUpTimeCount := SpriteC.Weapon.StartUpTime;
            end;

            SpriteC.BurstCount := 0;
          end;
        end;

        if SpriteC.Player.ControlMethod = HUMAN then
          if not SpriteC.Control.Fire then
            SpriteC.BurstCount := 0;

        // Fire Mode styles
        case SpriteC.Weapon.FireMode of
          2:  // Single shot
            begin

              if (SpriteC.Player.ControlMethod = HUMAN) then
                if SpriteC.Control.Fire then
                begin
                  if ((SpriteC.BurstCount > 0) or SpriteC.Control.Reload) and
                     (SpriteC.Weapon.FireIntervalCount < 2) then
                    Inc(SpriteC.Weapon.FireIntervalCount);
                end;
            end;

        end;

        // TARGET MODE
        {$IFNDEF SERVER}
        if (SpriteC.Num = MySprite) and
           TargetMode and SpriteC.Control.Fire then
        begin
          ClientFreeCamTarget;
          TargetMode := False;
          CameraFollowSprite := MySprite;
          SpriteC.Control.Fire := False;
          SpriteC.Weapon.FireIntervalPrev := SpriteC.Weapon.FireInterval;
          SpriteC.Weapon.FireIntervalCount := SpriteC.Weapon.FireInterval;
        end;
        {$ELSE}
          SpriteC.ThrowFlag;
        {$ENDIF}

        SpriteC.ThrowGrenade;

        // change weapon animation
        if (SpriteC.BodyAnimation.ID <> Roll.ID) and
           (SpriteC.BodyAnimation.ID <> RollBack.ID) and
           (SpriteC.BonusStyle <> BONUS_FLAMEGOD) then
          if SpriteC.Control.ChangeWeapon then
          begin
            SpriteC.BodyApplyAnimation(Change, 1);
            {$IFNDEF SERVER}
            SetSoundPaused(SpriteC.ReloadSoundChannel, True);
            {$ENDIF}
          end;

        // clear dont drop flag if needed
        {$IFNDEF SERVER}
        if SpriteC.DontDrop then
          if not SpriteC.Control.ThrowWeapon or (SpriteC.Weapon.Num = Guns[KNIFE].Num) then
            SpriteC.DontDrop := False;
        {$ENDIF}
        // throw weapon animation
        if SpriteC.Control.ThrowWeapon and
           not SpriteC.Control.ThrowNade and
           not SpriteC.DontDrop and
           (SpriteC.BodyAnimation.ID <> Roll.ID) and
           (SpriteC.BodyAnimation.ID <> RollBack.ID) and
           ((SpriteC.BodyAnimation.ID <> Change.ID) or
            (SpriteC.BodyAnimation.CurrFrame > 25)) and
           (SpriteC.BonusStyle <> BONUS_FLAMEGOD) and
           (SpriteC.Weapon.Num <> Guns[BOW].Num) and
           (SpriteC.Weapon.Num <> Guns[BOW2].Num) and
           (SpriteC.Weapon.Num <> Guns[NOWEAPON].Num) then
        begin
          SpriteC.BodyApplyAnimation(ThrowWeapon, 1);

          if SpriteC.Weapon.Num = Guns[KNIFE].Num then
            SpriteC.BodyAnimation.Speed := 2;

        {$IFNDEF SERVER}
        StopSound(SpriteC.ReloadSoundChannel);
        {$ENDIF}
        end;

        // reload
        if (SpriteC.Weapon.Num = Guns[CHAINSAW].Num) or
           ((SpriteC.BodyAnimation.ID <> Roll.ID) and
            (SpriteC.BodyAnimation.ID <> RollBack.ID) and
            (SpriteC.BodyAnimation.ID <> Change.ID)) then
        begin
          if SpriteC.Control.Reload then
          begin
            if SpriteC.Weapon.AmmoCount <> SpriteC.Weapon.Ammo then
            begin
              if SpriteC.Weapon.Num = Guns[SPAS12].Num then
              begin
                if SpriteC.Weapon.AmmoCount < SpriteC.Weapon.Ammo then
                begin
                  if SpriteC.Weapon.FireIntervalCount = 0 then
                    SpriteC.BodyApplyAnimation(Reload, 1)
                  else
                    SpriteC.AutoReloadWhenCanFire := True;
                end;
              end
              else
              begin
                SpriteC.Weapon.AmmoCount := 0;
                SpriteC.Weapon.FireIntervalPrev := SpriteC.Weapon.FireInterval;
                SpriteC.Weapon.FireIntervalCount := SpriteC.Weapon.FireInterval;
              end;
              SpriteC.BurstCount := 0;
            end;
          end;
        end;

        // reload shotgun / reload spas
        if (SpriteC.BodyAnimation.ID = Reload.ID) and
           (SpriteC.BodyAnimation.CurrFrame = 7) then
        begin
          {$IFNDEF SERVER}
          PlaySound(SFX_SPAS12_RELOAD, SpriteParts.Pos[SpriteC.Num], SpriteC.ReloadSoundChannel);
          {$ENDIF}
          Inc(SpriteC.BodyAnimation.CurrFrame);
        end;

        if not SpriteC.Control.Fire or (SpriteC.Weapon.AmmoCount = 0) then
          if (SpriteC.BodyAnimation.ID = Reload.ID) and
             (SpriteC.BodyAnimation.CurrFrame = 14) then
          begin
            SpriteC.Weapon.AmmoCount := SpriteC.Weapon.AmmoCount + 1;
            if SpriteC.Weapon.AmmoCount < SpriteC.Weapon.Ammo then
              SpriteC.BodyAnimation.CurrFrame := 1;
          end;

        // Change Weapon
        // sound
        if (SpriteC.BodyAnimation.ID = Change.ID) and
           (SpriteC.BodyAnimation.CurrFrame = 2) then
        begin
          {$IFNDEF SERVER}
          if SpriteC.SecondaryWeapon.Num = Guns[COLT].Num then
            PlaySound(SFX_CHANGESPIN, SpriteParts.Pos[SpriteC.Num])
          else if SpriteC.SecondaryWeapon.Num = Guns[KNIFE].Num then
            PlaySound(SFX_KNIFE, SpriteParts.Pos[SpriteC.Num])
          else if SpriteC.SecondaryWeapon.Num = Guns[CHAINSAW].Num then
            PlaySound(SFX_CHAINSAW_D, SpriteParts.Pos[SpriteC.Num])
          else
            PlaySound(SFX_CHANGEWEAPON, SpriteParts.Pos[SpriteC.Num]);
          {$ENDIF}
          Inc(SpriteC.BodyAnimation.CurrFrame);
        end;

        if (SpriteC.BodyAnimation.ID = Change.ID) and
           (SpriteC.BodyAnimation.CurrFrame = 25) and
           (SpriteC.BonusStyle <> BONUS_FLAMEGOD) then
        begin
          if {$IFNDEF SERVER}(SpriteC.Num = MySprite) or {$ENDIF} (SpriteC.Player.ControlMethod = BOT) then
          begin
            TempGun := SpriteC.Weapon;
            SpriteC.Weapon := SpriteC.SecondaryWeapon;
            SpriteC.SecondaryWeapon := TempGun;

            SpriteC.LastWeaponHM := SpriteC.Weapon.HitMultiply;
            SpriteC.LastWeaponStyle := SpriteC.Weapon.BulletStyle;
            SpriteC.LastWeaponSpeed := SpriteC.Weapon.Speed;
            SpriteC.LastWeaponFire := SpriteC.Weapon.FireInterval;
            SpriteC.LastWeaponReload := SpriteC.Weapon.ReloadTime;

            SpriteC.Weapon.StartUpTimeCount := SpriteC.Weapon.StartUpTime;
            SpriteC.Weapon.ReloadTimePrev := SpriteC.Weapon.ReloadTimeCount;

            SpriteC.BurstCount := 0;
          end;
        end;

        if ((SpriteC.BodyAnimation.ID = Change.ID) and
            (SpriteC.BodyAnimation.CurrFrame = Change.NumFrames)) and
            (SpriteC.BonusStyle <> BONUS_FLAMEGOD) and
            (SpriteC.Weapon.AmmoCount = 0) then
        begin
          SpriteC.BodyApplyAnimation(Stand, 1);
          {$IFNDEF SERVER}
          SetSoundPaused(SpriteC.ReloadSoundChannel, False);
          {$ENDIF}
        end;

        // Throw away weapon
        {$IFNDEF SERVER}
        if (SpriteC.BodyAnimation.ID = ThrowWeapon.ID) and
           (SpriteC.BodyAnimation.CurrFrame = 2) then
         PlaySound(SFX_THROWGUN, SpriteParts.Pos[SpriteC.Num]);
        {$ENDIF}
        if SpriteC.Weapon.Num <> Guns[KNIFE].Num then
          if (SpriteC.BodyAnimation.ID = ThrowWeapon.ID) and
             (SpriteC.BodyAnimation.CurrFrame = 19) and
             (SpriteC.Weapon.Num <> Guns[NOWEAPON].Num) then
          begin
            SpriteC.DropWeapon();
            SpriteC.BodyApplyAnimation(Stand, 1);
          end;

        // Throw knife
        if (SpriteC.Weapon.Num = Guns[KNIFE].Num) and
           (SpriteC.BodyAnimation.ID = ThrowWeapon.ID) and
           (not SpriteC.Control.ThrowWeapon or (SpriteC.BodyAnimation.CurrFrame = 16)) then
        begin
          if (SpriteC.Player.ControlMethod = BOT) {$IFNDEF SERVER} or (SpriteC.Num = MySprite) {$ENDIF} then
          begin
            // Set the dont drop flag so ThrowWeapon will not be sent to the server after knife is thrown
            {$IFNDEF SERVER}
            SpriteC.DontDrop := True;
            // Force a snapshot to be sent
            ForceClientSpriteSnapshotMov := True;
            LastForceClientSpriteSnapshotMovTick := MainTickCounter;
            {$ENDIF}
            B := SpriteC.GetCursorAimDirection();
            Vec2Scale(PlayerVelocity, SpriteParts.Velocity[SpriteC.Num],
              Guns[THROWNKNIFE].InheritedVelocity);

            D := Min(Max(SpriteC.BodyAnimation.CurrFrame, 8), 16) / 16;
            Vec2Scale(B, B, Guns[THROWNKNIFE].Speed * 1.5 * D);
            B := Vec2Add(B, PlayerVelocity);
            a := SpriteC.Skeleton.Pos[16];
            CreateBullet(a, b, Guns[THROWNKNIFE].Num, SpriteC.Num, 255,
              Guns[THROWNKNIFE].HitMultiply, True, False);
            SpriteC.ApplyWeaponByNum(Guns[NOWEAPON].Num, 1);
            SpriteC.BodyApplyAnimation(Stand, 1);
            {$IFNDEF SERVER}
            if (SpriteC.Num = MySprite) and not SpriteC.DeadMeat then
              ClientSpriteSnapshot;
            {$ENDIF}
          end;
        end;

        // Punch!
        if not SpriteC.DeadMeat then
          if (SpriteC.BodyAnimation.ID = Punch.ID) and
             (SpriteC.BodyAnimation.CurrFrame = 11) and
             (SpriteC.Weapon.Num <> Guns[LAW].Num) and
             (SpriteC.Weapon.Num <> Guns[M79].Num) then
          begin
            a.x := SpriteC.Skeleton.Pos[16].X + 2 * SpriteC.Direction;
            a.y := SpriteC.Skeleton.Pos[16].Y + 3;
            b.x := SpriteC.Direction * 0.1;
            b.y := 0;
            CreateBullet(a, b, SpriteC.Weapon.Num, SpriteC.Num, 255,
              SpriteC.Weapon.HitMultiply, True, False);

            {$IFNDEF SERVER}
            if SpriteC.Weapon.Num = Guns[KNIFE].Num then
              PlaySound(SFX_SLASH, SpriteParts.Pos[SpriteC.Num]);
            {$ENDIF}

            Inc(SpriteC.BodyAnimation.CurrFrame);
          end;

        // Buttstock!
        if not SpriteC.DeadMeat then
          if (SpriteC.BodyAnimation.ID = Melee.ID) and
             (SpriteC.BodyAnimation.CurrFrame = 12) then
          begin
            a.x := SpriteC.Skeleton.Pos[16].X + 2 * SpriteC.Direction;
            a.y := SpriteC.Skeleton.Pos[16].Y + 3;
            b.x := SpriteC.Direction * 0.1;
            b.y := 0;
            CreateBullet(a, b, Guns[NOWEAPON].Num, SpriteC.Num, 255,
              Guns[NOWEAPON].HitMultiply, True, True);

            {$IFNDEF SERVER}
            PlaySound(SFX_SLASH, SpriteParts.Pos[SpriteC.Num]);
            {$ENDIF}
          end;

        if SpriteC.BodyAnimation.ID = Melee.ID then
          if SpriteC.BodyAnimation.CurrFrame > 20 then
            SpriteC.BodyApplyAnimation(Stand, 1);

        // Shotgun luska
        if (SpriteC.BodyAnimation.ID = Shotgun.ID) and
           (SpriteC.BodyAnimation.CurrFrame = 24) then
        begin
          {$IFNDEF SERVER}
          B := SpriteC.GetHandsAimDirection();
          Vec2Scale(B, B, SpriteC.Weapon.Speed);
          b.x := SpriteC.Direction * 0.025 * b.y + SpriteParts.Velocity[SpriteC.Num].X;
          b.y := -SpriteC.Direction * 0.025 * b.x + SpriteParts.Velocity[SpriteC.Num].Y;
          a.x := SpriteC.Skeleton.Pos[15].x + 2 - SpriteC.Direction * 0.015 * b.x;
          a.y := SpriteC.Skeleton.Pos[15].y - 2 - SpriteC.Direction * 0.015 * b.y;

          CreateSpark(a, b, 51, SpriteC.Num, 255);  // czerwona luska
          {$ENDIF}
          Inc(SpriteC.BodyAnimation.CurrFrame);
        end;

        // M79 luska
        if (SpriteC.Weapon.Num = Guns[M79].Num) and
           (SpriteC.Weapon.ReloadTimeCount = SpriteC.Weapon.ClipOutTime) then
        begin
          {$IFNDEF SERVER}
          B := SpriteC.GetHandsAimDirection();
          Vec2Scale(B, B, SpriteC.Weapon.Speed);
          b.x := SpriteC.Direction * 0.08 * b.y + SpriteParts.Velocity[SpriteC.Num].X;
          b.y := -SpriteC.Direction * 0.08 * b.x + SpriteParts.Velocity[SpriteC.Num].Y;
          a.x := SpriteC.Skeleton.Pos[15].x + 2 - SpriteC.Direction * 0.015 * b.x;
          a.y := SpriteC.Skeleton.Pos[15].y - 2 - SpriteC.Direction * 0.015 * b.y;

          CreateSpark(a, b, 52, SpriteC.Num, 255); {m79 luska}
          {$ENDIF}
          if SpriteC.Weapon.ReloadTimeCount > 0 then
            Dec(SpriteC.Weapon.ReloadTimeCount);
        end;

        // Prone
        if SpriteC.Control.Prone then
        begin
          if (SpriteC.LegsAnimation.ID <> GetUp.ID) and
             (SpriteC.LegsAnimation.ID <> Prone.ID) and
             (SpriteC.LegsAnimation.ID <> ProneMove.ID) then
          begin
            {$IFNDEF SERVER}
            PlaySound(SFX_GOPRONE, SpriteParts.Pos[SpriteC.Num]);
            {$ENDIF}

            SpriteC.LegsApplyAnimation(Prone, 1);
            if (SpriteC.BodyAnimation.ID <> Reload.ID) and
               (SpriteC.BodyAnimation.ID <> Change.ID) and
               (SpriteC.BodyAnimation.ID <> ThrowWeapon.ID) then
              SpriteC.BodyApplyAnimation(Prone, 1);

            SpriteC.OldDirection := SpriteC.Direction;
            SpriteC.Control.Prone := False;
          end;
        end;

        // Get up
        if SpriteC.Position = POS_PRONE then
          if SpriteC.Control.Prone or
             (SpriteC.Direction <> SpriteC.OldDirection) then
            if ((SpriteC.LegsAnimation.ID = Prone.ID) and
                (SpriteC.LegsAnimation.CurrFrame > 23)) or
               (SpriteC.LegsAnimation.ID = ProneMove.ID) then
            begin
              if SpriteC.LegsAnimation.ID <> GetUp.ID then
              begin
                SpriteC.LegsAnimation := GetUp;
                SpriteC.LegsAnimation.CurrFrame := 9;
                SpriteC.Control.Prone := False;
                {$IFNDEF SERVER}
                PlaySound(SFX_STANDUP, SpriteParts.Pos[SpriteC.Num]);
                {$ENDIF}
              end;
              if (SpriteC.BodyAnimation.ID <> Reload.ID) and
                 (SpriteC.BodyAnimation.ID <> Change.ID) and
                 (SpriteC.BodyAnimation.ID <> ThrowWeapon.ID) then
                SpriteC.BodyApplyAnimation(GetUp, 9);
            end;

        Unprone := False;
        // Immediately switch from unprone to jump/sidejump, because the end of the unprone
        // animation can be seen as the "wind up" for the jump
        if (SpriteC.LegsAnimation.ID = GetUp.ID) and
           (SpriteC.LegsAnimation.CurrFrame > 23 - (4 - 1)) and  // Possible during the last 4 frames
           SpriteC.OnGround and SpriteC.Control.Up and
           (SpriteC.Control.Right or SpriteC.Control.Left) then
        begin
          // Set sidejump frame 1 to 4 depending on which unprone frame we're in
          SpriteC.LegsApplyAnimation(JumpSide, SpriteC.LegsAnimation.CurrFrame - (23 - (4 - 1)));
          Unprone := True;
        end
        else if (SpriteC.LegsAnimation.ID = GetUp.ID) and
                (SpriteC.LegsAnimation.CurrFrame > 23 - (4 - 1)) and  // Possible during the last 4 frames
                SpriteC.OnGround and SpriteC.Control.Up and
                not (SpriteC.Control.Right or SpriteC.Control.Left) then
        begin
          // Set jump frame 6 to 9 depending on which unprone frame we're in
          SpriteC.LegsApplyAnimation(Jump, SpriteC.LegsAnimation.CurrFrame - (23 - (9 - 1)));
          Unprone := True;
        end
        else if (SpriteC.LegsAnimation.ID = GetUp.ID) and
                (SpriteC.LegsAnimation.CurrFrame > 23) then
        begin
          if SpriteC.Control.Right or SpriteC.Control.Left then
          begin
            // Run back or forward depending on facing direction and direction key pressed
            if (SpriteC.Direction = 1) xor SpriteC.Control.Left then
              SpriteC.LegsApplyAnimation(Run, 1)
            else
              SpriteC.LegsApplyAnimation(RunBack, 1);
          end
          else if not SpriteC.OnGround and SpriteC.Control.Up then
            SpriteC.LegsApplyAnimation(Run, 1)
          else
            SpriteC.LegsApplyAnimation(Stand, 1);

          Unprone := True;
        end;

        if Unprone then
        begin
          SpriteC.Position := POS_STAND;

          if (SpriteC.BodyAnimation.ID <> Reload.ID) and
             (SpriteC.BodyAnimation.ID <> Change.ID) and
             (SpriteC.BodyAnimation.ID <> ThrowWeapon.ID) then
            SpriteC.BodyApplyAnimation(Stand, 1);
        end;

        // Stat overheat less
        if not SpriteC.Control.Fire then
        begin
          if SpriteC.UseTime > M2GUN_OVERHEAT + 1 then
            SpriteC.UseTime := 0;
          if SpriteC.UseTime > 0 then
            if MainTickCounter mod 8 = 0 then
              Dec(SpriteC.UseTime);
        end;

        // Fondle Barrett?!
        if (SpriteC.Weapon.Num = Guns[BARRETT].Num) and
           (SpriteC.Weapon.FireIntervalCount > 0) then
          if (SpriteC.BodyAnimation.ID = Stand.ID) or
             (SpriteC.BodyAnimation.ID = Crouch.ID) or
             (SpriteC.BodyAnimation.ID = Prone.ID) then
            SpriteC.BodyApplyAnimation(Barret, 1);

        // IDLE
        if SpriteC.Stat = 0 then
        begin
          if ((SpriteC.BodyAnimation.ID = Stand.ID) and
              (SpriteC.LegsAnimation.ID = Stand.ID) and
              not SpriteC.DeadMeat and
              (SpriteC.IdleTime > 0)) or
             (SpriteC.IdleTime > DEFAULT_IDLETIME) then
          begin
            {$IFNDEF SERVER}if (SpriteC.IdleRandom >= 0) then{$ENDIF}
            begin
              Dec(SpriteC.IdleTime);
            end;
          end
          else
          begin
            SpriteC.IdleTime := DEFAULT_IDLETIME;
          end;
          {$IFDEF SERVER}
            if (SpriteC.IdleTime = 1) and (SpriteC.IdleRandom < 0) then
            begin
              SpriteC.IdleTime := 0;
              SpriteC.IdleRandom := Random(4);
            end;
          {$ENDIF}
        end;

        if SpriteC.IdleRandom = 0 then  // STUFF
        begin
          if SpriteC.IdleTime = 0 then
          begin
            SpriteC.BodyApplyAnimation(Smoke, 1);
            {$IFDEF SERVER}
            ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
            {$ENDIF}
            SpriteC.IdleTime := DEFAULT_IDLETIME;
          end;

          if (SpriteC.BodyAnimation.ID = Smoke.ID) and
             (SpriteC.BodyAnimation.CurrFrame = 17) then
          begin
            {$IFNDEF SERVER}
            PlaySound(SFX_STUFF, SpriteParts.Pos[SpriteC.Num]);
            {$ENDIF}
            Inc(SpriteC.BodyAnimation.CurrFrame);
          end;

          if not SpriteC.DeadMeat then
          begin
            if (SpriteC.IdleTime = 1) and
               (SpriteC.BodyAnimation.ID <> Smoke.ID) and
               (SpriteC.LegsAnimation.ID = Stand.ID) then
            begin
              {$IFNDEF SERVER}
              a := SpriteC.Skeleton.Pos[12];
              b := SpriteC.GetHandsAimDirection();
              Vec2Scale(b, b, 2);
              CreateSpark(a, b, 32, SpriteC.Num, 245);
              PlaySound(SFX_SPIT, SpriteParts.Pos[SpriteC.Num]);
              {$ENDIF}
              SpriteC.IdleTime := DEFAULT_IDLETIME;
              SpriteC.IdleRandom := -1;
            end;
          end;
        end
        else if SpriteC.IdleRandom = 1 then  // CIGAR
        begin
          if not SpriteC.DeadMeat then
          begin
            if SpriteC.IdleTime = 0 then
            begin
              if SpriteC.HasCigar = 0 then
              begin
                if SpriteC.BodyAnimation.ID = Stand.ID then
                begin
                  // Step 1/8
                  SpriteC.BodyApplyAnimation(Cigar, 1);
                  {$IFDEF SERVER}
                  ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
                  {$ENDIF}
                  SpriteC.IdleTime := DEFAULT_IDLETIME;
                end;
              end
              else if SpriteC.HasCigar = 5 then
              begin
                if (SpriteC.BodyAnimation.ID <> Smoke.ID) and
                   (SpriteC.BodyAnimation.ID <> Cigar.ID) then
                begin
                  // Step 4.5/8 (only occurrs if interrupted between step 2 and 5, so redo step 1)
                  SpriteC.HasCigar := 0;
                  SpriteC.BodyApplyAnimation(Cigar, 1);
                  SpriteC.IdleTime := DEFAULT_IDLETIME;
                end;
              end
              else if SpriteC.HasCigar = 10 then
              begin
                if SpriteC.BodyAnimation.ID <> Smoke.ID then
                begin
                  // Step 6/8
                  SpriteC.BodyApplyAnimation(Smoke, 1);
                  SpriteC.IdleTime := DEFAULT_IDLETIME;
                end;
              end;
            end;

            if (SpriteC.BodyAnimation.ID = Cigar.ID) and
              (SpriteC.BodyAnimation.CurrFrame = 37) then
              if SpriteC.HasCigar = 5 then
              begin
                // Step 3/8
                SpriteC.BodyApplyAnimation(Stand, 1);
                SpriteC.BodyApplyAnimation(Cigar, 1);
              end;

            if (SpriteC.BodyAnimation.ID = Cigar.ID) and
              (SpriteC.BodyAnimation.CurrFrame = 9) then
              if SpriteC.HasCigar = 5 then
              begin
                // Step 4/8
                {$IFNDEF SERVER}
                PlaySound(SFX_MATCH, SpriteParts.Pos[SpriteC.Num]);
                {$ENDIF}
                Inc(SpriteC.BodyAnimation.CurrFrame);
              end;

            if (SpriteC.BodyAnimation.ID = Cigar.ID) and
              (SpriteC.BodyAnimation.CurrFrame = 26) then
            begin
              if SpriteC.HasCigar = 5 then
              begin
                // Step 5/8
                SpriteC.HasCigar := 10;
                {$IFNDEF SERVER}
                a := SpriteC.Skeleton.Pos[12];
                a.X := a.X + SpriteC.Direction * 4;
                b.x := 0;
                b.y := -0.7;
                CreateSpark(a, b, 31, SpriteC.Num, 65);
                PlaySound(SFX_SMOKE, SpriteParts.Pos[SpriteC.Num]);

                a := SpriteC.Skeleton.Pos[15];
                b.x := SpriteC.Direction / 2;
                b.y := 0.15;
                CreateSpark(a, b, 33, SpriteC.Num, 245);
                {$ENDIF}
                Inc(SpriteC.BodyAnimation.CurrFrame);
                SpriteC.IdleTime := LONGER_IDLETIME;
              end
              else if SpriteC.HasCigar = 0 then
              begin
                // Step 2/8
                SpriteC.HasCigar := 5;
                Inc(SpriteC.BodyAnimation.CurrFrame);
              end;
            end;

            if (SpriteC.BodyAnimation.ID = Smoke.ID) and
              ((SpriteC.BodyAnimation.CurrFrame = 17) or
              (SpriteC.BodyAnimation.CurrFrame = 37)) then
            begin
              // Step 7/8
              {$IFNDEF SERVER}
              a := SpriteC.Skeleton.Pos[12];
              a.X := a.X + SpriteC.Direction * 4;
              b.x := 0;
              b.y := -0.7;
              CreateSpark(a, b, 31, SpriteC.Num, 65);
              PlaySound(SFX_SMOKE, SpriteParts.Pos[SpriteC.Num]);
              {$ENDIF}
              Inc(SpriteC.BodyAnimation.CurrFrame);
            end;

            if (SpriteC.BodyAnimation.ID = Smoke.ID) and
              (SpriteC.BodyAnimation.CurrFrame = 38) then
            begin
              // Step 8/8
              SpriteC.HasCigar := 0;
              {$IFNDEF SERVER}
              a := SpriteC.Skeleton.Pos[15];
              b.x := SpriteC.Direction / 1.5;
              b.y := 0.1;
              CreateSpark(a, b, 34, SpriteC.Num, 245);
              {$ENDIF}
              Inc(SpriteC.BodyAnimation.CurrFrame);
              SpriteC.IdleTime := DEFAULT_IDLETIME;
              SpriteC.IdleRandom := -1;
            end;
          end;
        end
        else if SpriteC.IdleRandom = 2 then  // WIPE
        begin
          if SpriteC.IdleTime = 0 then
          begin
            SpriteC.BodyApplyAnimation(Wipe, 1);
            {$IFDEF SERVER}
            ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
            {$ENDIF}
            SpriteC.IdleTime := DEFAULT_IDLETIME;
            SpriteC.IdleRandom := -1;
          end;
        end
        else if SpriteC.IdleRandom = 3 then  // EGGS
        begin
          if SpriteC.IdleTime = 0 then
          begin
            SpriteC.BodyApplyAnimation(Groin, 1);
            {$IFDEF SERVER}
            ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
            {$ENDIF}
            SpriteC.IdleTime := DEFAULT_IDLETIME;
            SpriteC.IdleRandom := -1;
          end;
        end
        else if SpriteC.IdleRandom = 4 then  // TAKE OFF HELMET
        begin
          if (SpriteC.Weapon.Num <> Guns[BOW].Num) and
             (SpriteC.Weapon.Num <> Guns[BOW2].Num) then
          begin
            if SpriteC.IdleTime = 0 then
            begin
              if SpriteC.WearHelmet = 1 then
                SpriteC.BodyApplyAnimation(TakeOff, 1);
              if SpriteC.WearHelmet = 2 then
                SpriteC.BodyApplyAnimation(TakeOff, 10);
              {$IFDEF SERVER}
              ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
              {$ENDIF}
              SpriteC.IdleTime := DEFAULT_IDLETIME;
            end;

            if SpriteC.WearHelmet = 1 then
            begin
              if (SpriteC.BodyAnimation.ID = TakeOff.ID) and
                (SpriteC.BodyAnimation.CurrFrame = 15) then
              begin
                SpriteC.WearHelmet := 2;
                Inc(SpriteC.BodyAnimation.CurrFrame);
              end;
            end
            else if SpriteC.WearHelmet = 2 then
            begin
              if (SpriteC.BodyAnimation.ID = TakeOff.ID) and
                (SpriteC.BodyAnimation.CurrFrame = 22) then
              begin
                SpriteC.BodyApplyAnimation(Stand, 1);
                SpriteC.IdleRandom := -1;
              end;

              if (SpriteC.BodyAnimation.ID = TakeOff.ID) and
                (SpriteC.BodyAnimation.CurrFrame = 15) then
              begin
                SpriteC.WearHelmet := 1;
                Inc(SpriteC.BodyAnimation.CurrFrame);
              end;
            end;
          end;
        end
        else if SpriteC.IdleRandom = 5 then  // VICTORY
        begin
          if SpriteC.IdleTime = 0 then
          begin
            SpriteC.BodyApplyAnimation(Victory, 1);
            {$IFDEF SERVER}
            ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
            {$ENDIF}
            SpriteC.IdleTime := DEFAULT_IDLETIME;
            SpriteC.IdleRandom := -1;

            {$IFNDEF SERVER}
            PlaySound(SFX_ROAR, SpriteParts.Pos[SpriteC.Num]);
            {$ENDIF}
          end;
        end
        else if SpriteC.IdleRandom = 6 then  // PISS...
        begin
          if SpriteC.IdleTime = 0 then
          begin
            SpriteC.BodyApplyAnimation(Piss, 1);
            {$IFDEF SERVER}
            ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
            {$ENDIF}
            SpriteC.IdleTime := DEFAULT_IDLETIME;

            {$IFNDEF SERVER}
            PlaySound(SFX_PISS, SpriteParts.Pos[SpriteC.Num]);
            {$ENDIF}
          end;

          if (SpriteC.BodyAnimation.ID = Piss.ID) then
          begin
            if (SpriteC.BodyAnimation.CurrFrame > 8) and
               (SpriteC.BodyAnimation.CurrFrame < 22) then
            begin
              if Random(2) = 0 then
              begin
                {$IFNDEF SERVER}
                a := SpriteC.Skeleton.Pos[20];
                lookpoint.x := SpriteC.Control.MouseAimX;
                lookpoint.y := SpriteC.Control.MouseAimY;
                B := Vec2Subtract(SpriteC.Skeleton.Pos[20], LookPoint);
                Vec2Normalize(B, B);
                Vec2Scale(B, B, -1.3);
                CreateSpark(a, b, 57, SpriteC.Num, 165);
                {$ENDIF}
              end;
            end
            else if (SpriteC.BodyAnimation.CurrFrame > 21) and
                    (SpriteC.BodyAnimation.CurrFrame < 34) then
            begin
              if Random(3) = 0 then
              begin
                {$IFNDEF SERVER}
                a := SpriteC.Skeleton.Pos[20];
                lookpoint.x := SpriteC.Control.MouseAimX;
                lookpoint.y := SpriteC.Control.MouseAimY;
                B := Vec2Subtract(SpriteC.Skeleton.Pos[20], LookPoint);
                Vec2Normalize(B, B);
                Vec2Scale(B, B, -1.9);
                CreateSpark(a, b, 57, SpriteC.Num, 120);
                {$ENDIF}
              end;
            end
            else if (SpriteC.BodyAnimation.CurrFrame > 33) and
                    (SpriteC.BodyAnimation.CurrFrame < 35) then
            begin
              if Random(4) = 0 then
              begin
                {$IFNDEF SERVER}
                a := SpriteC.Skeleton.Pos[20];
                lookpoint.x := SpriteC.Control.MouseAimX;
                lookpoint.y := SpriteC.Control.MouseAimY;
                B := Vec2Subtract(SpriteC.Skeleton.Pos[20], LookPoint);
                Vec2Normalize(B, B);
                Vec2Scale(B, B, -1.3);

                CreateSpark(a, b, 57, SpriteC.Num, 120);
                {$ENDIF}
              end;
            end
            else if SpriteC.BodyAnimation.CurrFrame = 37 then
            begin
              SpriteC.IdleRandom := -1;
            end;
          end;
        end
        else if SpriteC.IdleRandom = 7 then  // SELFKILL
        begin
          if SpriteC.IdleTime = 0 then
          begin
            if SpriteC.CanMercy then
            begin
              if (SpriteC.Weapon.Num = Guns[M79].Num) or
                 (SpriteC.Weapon.Num = Guns[M249].Num) or
                 (SpriteC.Weapon.Num = Guns[SPAS12].Num) or
                 (SpriteC.Weapon.Num = Guns[LAW].Num) or
                 (SpriteC.Weapon.Num = Guns[CHAINSAW].Num) or
                 (SpriteC.Weapon.Num = Guns[BARRETT].Num) or
                 (SpriteC.Weapon.Num = Guns[MINIGUN].Num) then
              begin
                SpriteC.BodyApplyAnimation(Mercy2, 1);
                SpriteC.LegsApplyAnimation(Mercy2, 1);
              end
              else if (SpriteC.Weapon.Num <> Guns[MINIGUN].Num) then
              begin
                  SpriteC.BodyApplyAnimation(Mercy, 1);
                  SpriteC.LegsApplyAnimation(Mercy, 1);
              end;

              {$IFNDEF SERVER}
              PlaySound(SFX_MERCY, SpriteParts.Pos[SpriteC.Num]);
              if (SpriteC.Weapon.Num = Guns[MINIGUN].Num) then
              PlaySound(SFX_MINIGUN_START, SpriteParts.Pos[SpriteC.Num]);
              {$Else}
              ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
              {$ENDIF}
              SpriteC.IdleTime := DEFAULT_IDLETIME;

              SpriteC.CanMercy := False;
            end
            else
            begin
              SpriteC.IdleRandom := -1;
              SpriteC.CanMercy := True;
            end;
          end;

          if (SpriteC.BodyAnimation.ID = Mercy.ID) or
             (SpriteC.BodyAnimation.ID = Mercy2.ID) then
          begin
            if (SpriteC.BodyAnimation.CurrFrame = 20) then
            begin
              SpriteC.Fire();
              {$IFNDEF SERVER}
              if SpriteC.Weapon.Num = Guns[KNIFE].Num then
                PlaySound(SFX_SLASH, SpriteParts.Pos[SpriteC.Num], Sprite[SpriteC.Num].GattlingSoundChannel);
              if SpriteC.Weapon.Num = Guns[CHAINSAW].Num then
                PlaySound(SFX_CHAINSAW_R, SpriteParts.Pos[SpriteC.Num], Sprite[SpriteC.Num].GattlingSoundChannel);
              if SpriteC.Weapon.Num = Guns[NOWEAPON].Num then
                PlaySound(SFX_DEAD_HIT, SpriteParts.Pos[SpriteC.Num], Sprite[SpriteC.Num].GattlingSoundChannel);

              if SpriteC.Num = MySprite then
                ClientSendStringMessage('kill', MSGTYPE_CMD);
              {$ENDIF}
              Inc(SpriteC.BodyAnimation.CurrFrame);

              SpriteC.IdleRandom := -1;
            end;
          end;
        end
        else if SpriteC.IdleRandom = 8 then  // PWN!
        begin
          if SpriteC.IdleTime = 0 then
          begin
            SpriteC.BodyApplyAnimation(Own, 1);
            SpriteC.LegsApplyAnimation(Own, 1);
            {$IFDEF SERVER}
            ServerIdleAnimation(SpriteC.Num, SpriteC.IdleRandom);
            {$ENDIF}
            SpriteC.IdleTime := DEFAULT_IDLETIME;
            SpriteC.IdleRandom := -1;
          end;
        end;


        // *CHEAT*
        if SpriteC.LegsAnimation.Speed > 1 then
        begin
          if (SpriteC.LegsAnimation.ID = Jump.ID) or
             (SpriteC.LegsAnimation.ID = JumpSide.ID) or
             (SpriteC.LegsAnimation.ID = Roll.ID) or
             (SpriteC.LegsAnimation.ID = RollBack.ID) or
             (SpriteC.LegsAnimation.ID = Prone.ID) or
             (SpriteC.LegsAnimation.ID = Run.ID) or
             (SpriteC.LegsAnimation.ID = RunBack.ID) then
          begin
            Spriteparts.Velocity[SpriteC.Num].x :=
              (Spriteparts.Velocity[SpriteC.Num].x / SpriteC.LegsAnimation.Speed);
            Spriteparts.Velocity[SpriteC.Num].y :=
              (Spriteparts.Velocity[SpriteC.Num].y / SpriteC.LegsAnimation.Speed);
          end;

          if SpriteC.LegsAnimation.Speed > 2 then
            if (SpriteC.LegsAnimation.ID = ProneMove.ID) or
               (SpriteC.LegsAnimation.ID = CrouchRun.ID) then
            begin
              Spriteparts.Velocity[SpriteC.Num].x :=
                (Spriteparts.Velocity[SpriteC.Num].x / SpriteC.LegsAnimation.Speed);
              Spriteparts.Velocity[SpriteC.Num].y :=
                (Spriteparts.Velocity[SpriteC.Num].y / SpriteC.LegsAnimation.Speed);
            end;
        end;

        // stat gun deactivate if needed
        if SpriteC.Control.Up or SpriteC.Control.Jetpack then
          if SpriteC.Stat > 0 then
          begin
            Thing[SpriteC.Stat].StaticType := False;
            SpriteC.Stat := 0;
          end;

        // AimDistCoef (Sniper view)
        if SpriteC.Weapon.Num = Guns[BARRETT].Num then
        begin
          if (SpriteC.Weapon.FireIntervalCount = 0) and
             ((SpriteC.BodyAnimation.ID = Prone.ID) or
              (SpriteC.BodyAnimation.ID = Aim.ID)) then
          begin
            if (Abs(SpriteC.Control.MouseAimX - SpriteParts.Pos[SpriteC.Num].X) >= 640 / 1.035) or
               (Abs(SpriteC.Control.MouseAimY - SpriteParts.Pos[SpriteC.Num].Y) >= 480 / 1.035) then
            begin
              if SpriteC.AimDistCoef = DEFAULTAIMDIST then
              begin
                {$IFNDEF SERVER}
                PlaySound(SFX_SCOPE, SpriteParts.Pos[SpriteC.Num]);
                {$ELSE}
                ServerSpriteDeltasMouse(SpriteC.Num);
                {$ENDIF}
              end;

              if SpriteC.BodyAnimation.ID = Prone.ID then
                if SpriteC.AimDistCoef > SNIPERAIMDIST then
                begin
                  SpriteC.AimDistCoef := SpriteC.AimDistCoef - AIMDISTINCR;
                  if MainTickCounter mod 27 = 0 then
                  begin
                    {$IFNDEF SERVER}
                    PlaySound(SFX_SCOPERUN, SpriteParts.Pos[SpriteC.Num]);
                    {$ELSE}
                    ServerSpriteDeltasMouse(SpriteC.Num);
                    {$ENDIF}
                  end;
                end;

              if SpriteC.BodyAnimation.ID = Aim.ID then
                if SpriteC.AimDistCoef > CROUCHAIMDIST then
                begin
                  SpriteC.AimDistCoef := SpriteC.AimDistCoef - 2 * AIMDISTINCR;
                  if MainTickCounter mod 27 = 0 then
                  begin
                    {$IFNDEF SERVER}
                    PlaySound(SFX_SCOPERUN, SpriteParts.Pos[SpriteC.Num]);
                    {$ELSE}
                    ServerSpriteDeltasMouse(SpriteC.Num);
                    {$ENDIF}
                  end;
                end;
            end;

            if (Abs(SpriteC.Control.MouseAimX - SpriteParts.Pos[SpriteC.Num].X) < 640 / 1.5) and
               (Abs(SpriteC.Control.MouseAimY - SpriteParts.Pos[SpriteC.Num].Y) < 480 / 1.5) then
            begin
              if SpriteC.AimDistCoef < DEFAULTAIMDIST then
              begin
                SpriteC.AimDistCoef := SpriteC.AimDistCoef + AIMDISTINCR;
                {$IFNDEF SERVER}
                if SpriteC.AimDistCoef = DEFAULTAIMDIST then
                  PlaySound(SFX_SCOPE, SpriteParts.Pos[SpriteC.Num]);
                {$ENDIF}
                  if MainTickCounter mod 27 = 0 then
                  begin
                    {$IFNDEF SERVER}
                    PlaySound(SFX_SCOPERUN, SpriteParts.Pos[SpriteC.Num]);
                    {$ELSE}
                    ServerSpriteDeltasMouse(SpriteC.Num);
                    {$ENDIF}
                  end;
              end;
            end;
          end else
          begin
            if SpriteC.AimDistCoef <> DEFAULTAIMDIST then
            begin
              {$IFNDEF SERVER}
              PlaySound(SFX_SCOPEBACK, SpriteParts.Pos[SpriteC.Num]);
              {$ELSE}
              ServerSpriteDeltasMouse(SpriteC.Num);
              {$ENDIF}
            end;

            SpriteC.AimDistCoef := DEFAULTAIMDIST;
            SpriteC.Control.MouseDist := 150;
          end;
        end else
        begin
          SpriteC.AimDistCoef := DEFAULTAIMDIST;
          SpriteC.Control.MouseDist := 150;
        end;

        // Check if near collider
        if MainTickCounter mod 10 = 0 then
        begin
          SpriteC.ColliderDistance := 255;  // not near

          for j := 1 to 128 do
            if Map.Collider[j].Active then
            begin
              a.X := Map.Collider[j].X;
              a.Y := Map.Collider[j].Y;

              B := Vec2Subtract(SpriteC.Skeleton.Pos[15],
                SpriteC.Skeleton.Pos[16]);
              Vec2Normalize(B, B);
              Vec2Scale(B, B, 8);
              startpoint.x := SpriteC.Skeleton.Pos[12].x;
              startpoint.y := SpriteC.Skeleton.Pos[12].y - 5;
              lookpoint := Vec2Add(startpoint, B);

              b := Vec2Subtract(lookpoint, a);
              d := Vec2Length(b);

              if d < Map.Collider[j].Radius then
              begin
                SpriteC.ColliderDistance := 1;

                if SpriteC.ColliderDistance = 1 then
                begin
                  if d > 253 then d := 253;
                  SpriteC.ColliderDistance := Round(d);
                end;

                Break;
              end;
            end;

          // raise weapon above teammate when crouching
          for j := 1 to MAX_SPRITES do
            if IsTeamGame() then
              if Sprite[j].Active and
                 Sprite[j].IsInSameTeam(SpriteC) and
                 (Sprite[j].Position = POS_CROUCH) and (j <> SpriteC.Num) and
                 SpriteC.IsNotSpectator() then
              begin
                a := Spriteparts.Pos[j];

                B := Vec2Subtract(SpriteC.Skeleton.Pos[15],
                  SpriteC.Skeleton.Pos[16]);
                Vec2Normalize(B, B);
                Vec2Scale(B, B, 8);
                startpoint.x := SpriteC.Skeleton.Pos[12].x;
                startpoint.y := SpriteC.Skeleton.Pos[12].y - 5;
                lookpoint := Vec2Add(startpoint, B);

                b := Vec2Subtract(lookpoint, a);
                d := Vec2Length(b);

                if d < SPRITE_RADIUS then
                begin
                  SpriteC.ColliderDistance := 1;

                  if SpriteC.ColliderDistance = 1 then
                  begin
                    if d > 253 then
                      d := 253;
                    SpriteC.ColliderDistance := Round(d);
                  end;

                  Break;
                end;
              end;
        end;
        {$IFNDEF SERVER}
        if TargetMode and (SpriteC.Num = MySprite) then
        begin
          SpriteC.FreeControls;
        end;
        {$ENDIF}
        // End any ongoing idle animations if a key is pressed
        if (SpriteC.BodyAnimation.ID = Cigar.ID) or
           (SpriteC.BodyAnimation.ID = Match.ID) or
           (SpriteC.BodyAnimation.ID = Smoke.ID) or
           (SpriteC.BodyAnimation.ID = Wipe.ID) or
           (SpriteC.BodyAnimation.ID = Groin.ID) then
        begin
          if SpriteC.Control.Left   or SpriteC.Control.Right or
             SpriteC.Control.Up     or SpriteC.Control.Down or
             SpriteC.Control.Fire or SpriteC.Control.Jetpack or
             SpriteC.Control.ThrowNade or SpriteC.Control.ChangeWeapon or
             SpriteC.Control.ThrowWeapon or SpriteC.Control.Reload or
             SpriteC.Control.Prone then
          begin
            SpriteC.BodyAnimation.CurrFrame := SpriteC.BodyAnimation.NumFrames;
          end;
        end;

        // make anims out of controls
        // rolling
        if (SpriteC.BodyAnimation.ID <> TakeOff.ID) and
           (SpriteC.BodyAnimation.ID <> Piss.ID) and
           (SpriteC.BodyAnimation.ID <> Mercy.ID) and
           (SpriteC.BodyAnimation.ID <> Mercy2.ID) and
           (SpriteC.BodyAnimation.ID <> Victory.ID) and
           (SpriteC.BodyAnimation.ID <> Own.ID) then
        begin
          if (SpriteC.BodyAnimation.ID = Roll.ID) or
             (SpriteC.BodyAnimation.ID = RollBack.ID) then
          begin
            if SpriteC.LegsAnimation.ID = Roll.ID then
            begin
              if SpriteC.OnGround then  // if staying on ground
                Spriteparts.Forces[SpriteC.Num].X := SpriteC.Direction * ROLLSPEED
              else
                Spriteparts.Forces[SpriteC.Num].X := SpriteC.Direction * 2 * FLYSPEED;
            end
            else if SpriteC.LegsAnimation.ID = RollBack.ID then
            begin
              if SpriteC.OnGround then  // if staying on ground
                Spriteparts.Forces[SpriteC.Num].X := -SpriteC.Direction * ROLLSPEED
              else
                Spriteparts.Forces[SpriteC.Num].X := -SpriteC.Direction * 2 * FLYSPEED;

              // if appropriate frames to move
              if (SpriteC.LegsAnimation.CurrFrame > 1) and
                 (SpriteC.LegsAnimation.CurrFrame < 8) then
              begin
                if SpriteC.Control.Up then
                begin
                  Spriteparts.Forces[SpriteC.Num].Y := Spriteparts.Forces[SpriteC.Num].Y - JUMPDIRSPEED * 1.5;
                  Spriteparts.Forces[SpriteC.Num].X := Spriteparts.Forces[SpriteC.Num].X * 0.5;
                  Spriteparts.Velocity[SpriteC.Num].X := Spriteparts.Velocity[SpriteC.Num].X * 0.8;
                end;
              end;
            end;
          end
          // downright
          else if SpriteC.Control.Right and SpriteC.Control.Down then
          begin
            if SpriteC.OnGround then  // if staying on ground
            begin
              // roll to the side
              if (SpriteC.LegsAnimation.ID = Run.ID) or
                 (SpriteC.LegsAnimation.ID = RunBack.ID) or
                 (SpriteC.LegsAnimation.ID = Fall.ID) or
                 (SpriteC.LegsAnimation.ID = ProneMove.ID) or
                 ((SpriteC.LegsAnimation.ID = Prone.ID) and
                  (SpriteC.LegsAnimation.CurrFrame >= 24)) then
              begin
                if (SpriteC.LegsAnimation.ID = ProneMove.ID) or
                   ((SpriteC.LegsAnimation.ID = Prone.ID) and
                    (SpriteC.LegsAnimation.CurrFrame = SpriteC.LegsAnimation.NumFrames)) then
                begin
                  SpriteC.Control.Prone := False;
                  SpriteC.Position := POS_STAND;
                end;
                {$IFNDEF SERVER}
                if (SpriteC.LegsAnimation.ID <> RollBack.ID) and
                  (SpriteC.LegsAnimation.ID <> Roll.ID) then
                    PlaySound(SFX_ROLL, SpriteParts.Pos[SpriteC.Num]);

                SetSoundPaused(SpriteC.ReloadSoundChannel, True);
                {$ENDIF}

                if SpriteC.Direction = 1 then
                begin
                  SpriteC.BodyApplyAnimation(Roll, 1);
                  SpriteC.LegsAnimation := Roll;
                  SpriteC.LegsAnimation.CurrFrame := 1;
                end
                else
                begin
                  SpriteC.BodyApplyAnimation(RollBack, 1);
                  SpriteC.LegsAnimation := RollBack;
                  SpriteC.LegsAnimation.CurrFrame := 1;
                end;
              end
              else
              begin
                if SpriteC.Direction = 1 then
                  SpriteC.LegsApplyAnimation(CrouchRun, 1)
                else
                  SpriteC.LegsApplyAnimation(CrouchRunBack, 1);
              end;

              if (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
                 (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) then
                Spriteparts.Forces[SpriteC.Num].X := CROUCHRUNSPEED
              else if (SpriteC.LegsAnimation.ID = Roll.ID) or
                      (SpriteC.LegsAnimation.ID = RollBack.ID) then
                Spriteparts.Forces[SpriteC.Num].X := 2 * CROUCHRUNSPEED;
            end;
          end
          // downleft
          else if SpriteC.Control.Left and SpriteC.Control.Down then
          begin
            if SpriteC.OnGround then  // if staying on ground
            begin
              // roll to the side
              if (SpriteC.LegsAnimation.ID = Run.ID) or
                 (SpriteC.LegsAnimation.ID = RunBack.ID) or
                 (SpriteC.LegsAnimation.ID = Fall.ID) or
                 (SpriteC.LegsAnimation.ID = ProneMove.ID) or
                 ((SpriteC.LegsAnimation.ID = Prone.ID) and
                  (SpriteC.LegsAnimation.CurrFrame >= 24)) then
              begin
                if (SpriteC.LegsAnimation.ID = ProneMove.ID) or
                   ((SpriteC.LegsAnimation.ID = Prone.ID) and
                    (SpriteC.LegsAnimation.CurrFrame = SpriteC.LegsAnimation.NumFrames)) then
                begin
                  SpriteC.Control.Prone := False;
                  SpriteC.Position := POS_STAND;
                end;
                {$IFNDEF SERVER}
                if (SpriteC.LegsAnimation.ID <> RollBack.ID) and
                   (SpriteC.LegsAnimation.ID <> Roll.ID) then
                    PlaySound(SFX_ROLL, SpriteParts.Pos[SpriteC.Num]);

                SetSoundPaused(SpriteC.ReloadSoundChannel, True);
                {$ENDIF}

                if SpriteC.Direction = 1 then
                begin
                  SpriteC.BodyApplyAnimation(RollBack, 1);
                  SpriteC.LegsAnimation := RollBack;
                  SpriteC.LegsAnimation.CurrFrame := 1;
                end
                else
                begin
                  SpriteC.BodyApplyAnimation(Roll, 1);
                  SpriteC.LegsAnimation := Roll;
                  SpriteC.LegsAnimation.CurrFrame := 1;
                end;
              end
              else
              begin
                if SpriteC.Direction = 1 then
                  SpriteC.LegsApplyAnimation(CrouchRunBack, 1)
                else
                  SpriteC.LegsApplyAnimation(CrouchRun, 1);
              end;

              if (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
                 (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) then
                Spriteparts.Forces[SpriteC.Num].X := -CROUCHRUNSPEED;
            end;
          end
          // Proning
          // FIXME(skoskav): The "and Body <> Throw|Punch" check is to keep the grenade tap and
          // punch/stab prone cancel bugs
          else if (SpriteC.LegsAnimation.ID = Prone.ID) or
                  (SpriteC.LegsAnimation.ID = ProneMove.ID) or
                  ((SpriteC.LegsAnimation.ID = GetUp.ID) and
                   (SpriteC.BodyAnimation.ID <> Throw.ID) and
                   (SpriteC.BodyAnimation.ID <> Punch.ID)) then
          begin
            if SpriteC.OnGround then
            begin
              if ((SpriteC.LegsAnimation.ID = Prone.ID) and
                  (SpriteC.LegsAnimation.CurrFrame > 25)) or
                 (SpriteC.LegsAnimation.ID = ProneMove.ID) then
              begin
                if SpriteC.Control.Left or SpriteC.Control.Right then
                begin
                  if (SpriteC.LegsAnimation.CurrFrame < 4) or
                     (SpriteC.LegsAnimation.CurrFrame > 14) then
                    Spriteparts.Forces[SpriteC.Num].X :=
                      iif(SpriteC.Control.Left, -PRONESPEED, PRONESPEED);

                  SpriteC.LegsApplyAnimation(ProneMove, 1);
                  if (SpriteC.BodyAnimation.ID <> ClipIn.ID) and
                     (SpriteC.BodyAnimation.ID <> ClipOut.ID) and
                     (SpriteC.BodyAnimation.ID <> SlideBack.ID) and
                     (SpriteC.BodyAnimation.ID <> Reload.ID) and
                     (SpriteC.BodyAnimation.ID <> Change.ID) and
                     (SpriteC.BodyAnimation.ID <> Throw.ID) and
                     (SpriteC.BodyAnimation.ID <> ThrowWeapon.ID) then
                    SpriteC.BodyApplyAnimation(ProneMove, 1);

                  if SpriteC.LegsAnimation.ID <> ProneMove.ID then
                    SpriteC.LegsAnimation := ProneMove;
                end
                else
                begin
                  if SpriteC.LegsAnimation.ID <> Prone.ID then
                    SpriteC.LegsAnimation := Prone;
                  SpriteC.LegsAnimation.CurrFrame := 26;
                end;
              end;
            end;
          end
          // upright
          else if SpriteC.Control.Right and SpriteC.Control.Up then
          begin
            if SpriteC.OnGround then  // if staying on ground
            begin
              // jump to the side
              if (SpriteC.LegsAnimation.ID = Run.ID) or
                 (SpriteC.LegsAnimation.ID = RunBack.ID) or
                 (SpriteC.LegsAnimation.ID = Stand.ID) or
                 (SpriteC.LegsAnimation.ID = Crouch.ID) or
                 (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
                 (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) then
              begin
                SpriteC.LegsApplyAnimation(JumpSide, 1);
                {$IFNDEF SERVER}
                PlaySound(SFX_JUMP, SpriteParts.Pos[SpriteC.Num]);
                {$ENDIF}
              end;

              if SpriteC.LegsAnimation.CurrFrame = SpriteC.LegsAnimation.NumFrames then
                SpriteC.LegsApplyAnimation(Run, 1);
            end
            else if (SpriteC.LegsAnimation.ID = Roll.ID) or
                    (SpriteC.LegsAnimation.ID = RollBack.ID) then
            begin
              if SpriteC.Direction = 1 then
                SpriteC.LegsApplyAnimation(Run, 1)
              else
                SpriteC.LegsApplyAnimation(RunBack, 1);
            end;

            if SpriteC.LegsAnimation.ID = Jump.ID then
            begin
              if SpriteC.LegsAnimation.CurrFrame < 10 then
              begin
                SpriteC.LegsApplyAnimation(JumpSide, 1);
              end;
            end;

            if SpriteC.LegsAnimation.ID = JumpSide.ID then
              // if appropriate frames to move
              if (SpriteC.LegsAnimation.CurrFrame > 3) and
                 (SpriteC.LegsAnimation.CurrFrame < 11) then
              begin
                Spriteparts.Forces[SpriteC.Num].X := JUMPDIRSPEED;
                Spriteparts.Forces[SpriteC.Num].Y := -JUMPDIRSPEED / 1.2;
              end;
          end
          // upleft
          else if SpriteC.Control.Left and SpriteC.Control.Up then
          begin
            if SpriteC.OnGround then  // if staying on ground
            begin
              // jump to the side
              if (SpriteC.LegsAnimation.ID = Run.ID) or
                 (SpriteC.LegsAnimation.ID = RunBack.ID) or
                 (SpriteC.LegsAnimation.ID = Stand.ID) or
                 (SpriteC.LegsAnimation.ID = Crouch.ID) or
                 (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
                 (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) then
              begin
                SpriteC.LegsApplyAnimation(JumpSide, 1);
                {$IFNDEF SERVER}
                PlaySound(SFX_JUMP, SpriteParts.Pos[SpriteC.Num]);
                {$ENDIF}
              end;

              if SpriteC.LegsAnimation.CurrFrame = SpriteC.LegsAnimation.NumFrames then
                SpriteC.LegsApplyAnimation(Run, 1);
            end
            else if (SpriteC.LegsAnimation.ID = Roll.ID) or
                    (SpriteC.LegsAnimation.ID = RollBack.ID) then
            begin
              if SpriteC.Direction = -1 then
                SpriteC.LegsApplyAnimation(Run, 1)
              else
                SpriteC.LegsApplyAnimation(RunBack, 1);
            end;

            if SpriteC.LegsAnimation.ID = Jump.ID then
            begin
              if SpriteC.LegsAnimation.CurrFrame < 10 then
              begin
                SpriteC.LegsApplyAnimation(JumpSide, 1);
              end;
            end;

            if SpriteC.LegsAnimation.ID = JumpSide.ID then
              // if appropriate frames to move
              if (SpriteC.LegsAnimation.CurrFrame > 3) and
                 (SpriteC.LegsAnimation.CurrFrame < 11) then
              begin
                Spriteparts.Forces[SpriteC.Num].X := -JUMPDIRSPEED;
                Spriteparts.Forces[SpriteC.Num].Y := -JUMPDIRSPEED / 1.2;
              end;
          end
          // up
          else if SpriteC.Control.Up then
          begin
            if SpriteC.OnGround then  // if staying on ground
            begin
              if SpriteC.LegsAnimation.ID <> Jump.ID then
              begin
                SpriteC.LegsApplyAnimation(Jump, 1);
                {$IFNDEF SERVER}
                PlaySound(SFX_JUMP, SpriteParts.Pos[SpriteC.Num]);
                {$ENDIF}
              end;

              if SpriteC.LegsAnimation.CurrFrame = SpriteC.LegsAnimation.NumFrames then
                SpriteC.LegsApplyAnimation(Stand, 1);
            end;

            if SpriteC.LegsAnimation.ID = Jump.ID then
            begin
              // if appropriate frames to move
              if (SpriteC.LegsAnimation.CurrFrame > 8) and
                 (SpriteC.LegsAnimation.CurrFrame < 15) then
                Spriteparts.Forces[SpriteC.Num].Y := -JUMPSPEED;

              if SpriteC.LegsAnimation.CurrFrame = SpriteC.LegsAnimation.NumFrames then
                SpriteC.LegsApplyAnimation(Fall, 1);
            end;
          end
          // down
          else if SpriteC.Control.Down then
          begin
            if SpriteC.OnGround then  // if staying on ground
            begin
              {$IFNDEF SERVER}
              if (SpriteC.LegsAnimation.ID <> CrouchRun.ID) and
                 (SpriteC.LegsAnimation.ID <> CrouchRunBack.ID) and
                 (SpriteC.LegsAnimation.ID <> Crouch.ID) then
                   PlaySound(SFX_CROUCH, SpriteParts.Pos[SpriteC.Num]);
              {$ENDIF}

              SpriteC.LegsApplyAnimation(Crouch, 1);
            end;
          end
          // right
          else if SpriteC.Control.Right then
          begin
            if SpriteC.Para = 0 then
            begin
              if SpriteC.Direction = 1 then
                SpriteC.LegsApplyAnimation(Run, 1)
              else
                SpriteC.LegsApplyAnimation(RunBack, 1);
            end
            else if SpriteC.HoldedThing <> 0 then
            begin
              // parachute bend
              Thing[SpriteC.HoldedThing].Skeleton.Forces[3].Y :=
                Thing[SpriteC.HoldedThing].Skeleton.Forces[3].Y - 0.5;
              Thing[SpriteC.HoldedThing].Skeleton.Forces[2].Y :=
                Thing[SpriteC.HoldedThing].Skeleton.Forces[2].Y + 0.5;
            end;

            if SpriteC.OnGround then  // if staying on ground
            begin
              Spriteparts.Forces[SpriteC.Num].X := RUNSPEED;
              Spriteparts.Forces[SpriteC.Num].Y := -RUNSPEEDUP;
            end
            else
              Spriteparts.Forces[SpriteC.Num].X := FLYSPEED;
          end
          // left
          else if SpriteC.Control.Left then
          begin
            if SpriteC.Para = 0 then
            begin
              if SpriteC.Direction = -1 then
                SpriteC.LegsApplyAnimation(Run, 1)
              else
                SpriteC.LegsApplyAnimation(RunBack, 1);
            end
            else if SpriteC.HoldedThing <> 0 then
            begin
              // parachute bend
              Thing[SpriteC.HoldedThing].Skeleton.Forces[2].Y :=
                Thing[SpriteC.HoldedThing].Skeleton.Forces[2].Y - 0.5;
              Thing[SpriteC.HoldedThing].Skeleton.Forces[3].Y :=
                Thing[SpriteC.HoldedThing].Skeleton.Forces[3].Y + 0.5;
            end;

            if SpriteC.OnGround then  // if staying on ground
            begin
              Spriteparts.Forces[SpriteC.Num].X := -RUNSPEED;
              Spriteparts.Forces[SpriteC.Num].Y := -RUNSPEEDUP;
            end
            else
              Spriteparts.Forces[SpriteC.Num].X := -FLYSPEED;
          end
          // else all keys not pressed
          else
          begin
            if SpriteC.OnGround then  // if staying on ground
            begin
              {$IFNDEF SERVER}
              if not SpriteC.DeadMeat then
                if SpriteC.LegsAnimation.ID <> Stand.ID then
                  PlaySound(SFX_STOP, SpriteParts.Pos[SpriteC.Num]);
              {$ENDIF}
              SpriteC.LegsApplyAnimation(Stand, 1);
            end
            else
              SpriteC.LegsApplyAnimation(Fall, 1);
          end;
        end;

        // Body animations

        // reloading
        if (SpriteC.Weapon.ReloadTimeCount = SpriteC.Weapon.ClipOutTime) and
           (SpriteC.BodyAnimation.ID <> Reload.ID) and
           (SpriteC.BodyAnimation.ID <> ReloadBow.ID) and
           (SpriteC.BodyAnimation.ID <> Roll.ID) and
           (SpriteC.BodyAnimation.ID <> RollBack.ID) then
          SpriteC.BodyApplyAnimation(ClipIn, 1);
        if SpriteC.Weapon.ReloadTimeCount = SpriteC.Weapon.ClipInTime then
          SpriteC.BodyApplyAnimation(SlideBack, 1);

        // this piece of code fixes the infamous crouch bug
        // how you ask? well once upon time opensoldat's code decided that
        // randomly the animation for roll for the body and legs will magically
        // go out of sync, which is causing the crouch bug. so this piece of
        // awesome code simply syncs them when they go out of sync <3
        if (SpriteC.LegsAnimation.ID = Roll.ID) and
           (SpriteC.BodyAnimation.ID <> Roll.ID) then
          SpriteC.BodyApplyAnimation(Roll, 1);
        if (SpriteC.BodyAnimation.ID = Roll.ID) and
           (SpriteC.LegsAnimation.ID <> Roll.ID) then
          SpriteC.LegsApplyAnimation(Roll, 1);
        if (SpriteC.LegsAnimation.ID = RollBack.ID) and
           (SpriteC.BodyAnimation.ID <> RollBack.ID) then
          SpriteC.BodyApplyAnimation(Rollback, 1);
        if (SpriteC.BodyAnimation.ID = RollBack.ID) and
           (SpriteC.LegsAnimation.ID <> RollBack.ID) then
          SpriteC.LegsApplyAnimation(Rollback, 1);

        if (SpriteC.BodyAnimation.ID = Roll.ID) or
           (SpriteC.BodyAnimation.ID = RollBack.ID) then
        begin
          if SpriteC.LegsAnimation.CurrFrame <> SpriteC.BodyAnimation.CurrFrame then
          begin
            if SpriteC.LegsAnimation.CurrFrame > SpriteC.BodyAnimation.CurrFrame then
              SpriteC.BodyAnimation.CurrFrame := SpriteC.LegsAnimation.CurrFrame
            else
              SpriteC.LegsAnimation.CurrFrame := SpriteC.BodyAnimation.CurrFrame;
          end;
        end;

        // Gracefully end a roll animation
        if ((SpriteC.BodyAnimation.ID = Roll.ID) or
            (SpriteC.BodyAnimation.ID = RollBack.ID)) and
           (SpriteC.BodyAnimation.CurrFrame = SpriteC.BodyAnimation.NumFrames) then
        begin
          // Was probably a roll
          if SpriteC.OnGround then
          begin
            if SpriteC.Control.Down then
            begin
              if SpriteC.Control.Left or SpriteC.Control.Right then
              begin
                if SpriteC.BodyAnimation.ID = Roll.ID then
                  SpriteC.LegsApplyAnimation(CrouchRun, 1)
                else
                  SpriteC.LegsApplyAnimation(CrouchRunBack, 1);
              end else
                SpriteC.LegsApplyAnimation(Crouch, 15);
            end;
          end
          // Was probably a backflip
          else if (SpriteC.BodyAnimation.ID = RollBack.ID) and SpriteC.Control.Up then
          begin
            if SpriteC.Control.Left or SpriteC.Control.Right then
            begin
              // Run back or forward depending on facing direction and direction key pressed
              if (SpriteC.Direction = 1) xor SpriteC.Control.Left then
                SpriteC.LegsApplyAnimation(Run, 1)
              else
                SpriteC.LegsApplyAnimation(RunBack, 1);
            end else
              SpriteC.LegsApplyAnimation(Fall, 1);
          end
          // Was probably a roll (that ended mid-air)
          else if SpriteC.Control.Down then
          begin
            if SpriteC.Control.Left or SpriteC.Control.Right then
            begin
              if SpriteC.BodyAnimation.ID = Roll.ID then
                SpriteC.LegsApplyAnimation(CrouchRun, 1)
              else
                SpriteC.LegsApplyAnimation(CrouchRunBack, 1);
            end else
              SpriteC.LegsApplyAnimation(Crouch, 15);
          end;

          SpriteC.BodyApplyAnimation(Stand, 1);
        end;

        if SpriteC.Weapon.AmmoCount > 0 then
          if (not SpriteC.Control.ThrowNade and
              (SpriteC.BodyAnimation.ID <> Recoil.ID) and
              (SpriteC.BodyAnimation.ID <> SmallRecoil.ID) and
              (SpriteC.BodyAnimation.ID <> AimRecoil.ID) and
              (SpriteC.BodyAnimation.ID <> HandsUpRecoil.ID) and
              (SpriteC.BodyAnimation.ID <> Shotgun.ID) and
              (SpriteC.BodyAnimation.ID <> Barret.ID) and
              (SpriteC.BodyAnimation.ID <> Change.ID) and
              (SpriteC.BodyAnimation.ID <> ThrowWeapon.ID) and
              (SpriteC.BodyAnimation.ID <> WeaponNone.ID) and
              (SpriteC.BodyAnimation.ID <> Punch.ID) and
              (SpriteC.BodyAnimation.ID <> Roll.ID) and
              (SpriteC.BodyAnimation.ID <> RollBack.ID) and
              (SpriteC.BodyAnimation.ID <> ReloadBow.ID) and
              (SpriteC.BodyAnimation.ID <> Cigar.ID) and
              (SpriteC.BodyAnimation.ID <> Match.ID) and
              (SpriteC.BodyAnimation.ID <> Smoke.ID) and
              (SpriteC.BodyAnimation.ID <> Wipe.ID) and
              (SpriteC.BodyAnimation.ID <> TakeOff.ID) and
              (SpriteC.BodyAnimation.ID <> Groin.ID) and
              (SpriteC.BodyAnimation.ID <> Piss.ID) and
              (SpriteC.BodyAnimation.ID <> Mercy.ID) and
              (SpriteC.BodyAnimation.ID <> Mercy2.ID) and
              (SpriteC.BodyAnimation.ID <> Victory.ID) and
              (SpriteC.BodyAnimation.ID <> Own.ID) and
              (SpriteC.BodyAnimation.ID <> Reload.ID) and
              (SpriteC.BodyAnimation.ID <> Prone.ID) and
              (SpriteC.BodyAnimation.ID <> GetUp.ID) and
              (SpriteC.BodyAnimation.ID <> ProneMove.ID) and
              (SpriteC.BodyAnimation.ID <> Melee.ID)) or
             ((SpriteC.BodyAnimation.CurrFrame = SpriteC.BodyAnimation.NumFrames) and
              (SpriteC.BodyAnimation.ID <> Prone.ID)) or
             ((SpriteC.Weapon.FireIntervalCount = 0) and
              (SpriteC.BodyAnimation.ID = Barret.ID)) then
          begin
            if SpriteC.Position <> POS_PRONE then
            begin
              if SpriteC.Position = POS_STAND then
                SpriteC.BodyApplyAnimation(Stand, 1);

              if SpriteC.Position = POS_CROUCH then
              begin
                if SpriteC.ColliderDistance < 255 then
                begin
                  if SpriteC.BodyAnimation.ID = HandsUpRecoil.ID then
                    SpriteC.BodyApplyAnimation(HandsUpAim, 11)
                  else
                    SpriteC.BodyApplyAnimation(HandsUpAim, 1)
                end
                else
                begin
                  if SpriteC.BodyAnimation.ID = AimRecoil.ID then
                    SpriteC.BodyApplyAnimation(Aim, 6)
                  else
                    SpriteC.BodyApplyAnimation(Aim, 1);
                end;
              end;
            end
            else
              SpriteC.BodyApplyAnimation(Prone, 26);
          end;

        if (SpriteC.LegsAnimation.ID = Crouch.ID) or
           (SpriteC.LegsAnimation.ID = CrouchRun.ID) or
           (SpriteC.LegsAnimation.ID = CrouchRunBack.ID) then
          SpriteC.Position := POS_CROUCH
        else
          SpriteC.Position := POS_STAND;

        if (SpriteC.LegsAnimation.ID = Prone.ID) or
           (SpriteC.LegsAnimation.ID = ProneMove.ID) then
          SpriteC.Position := POS_PRONE;

        {$IFNDEF SERVER}
        if (ClientStopMovingCounter < 1) then
          SpriteC.FreeControls;
        {$ENDIF}
      end;
  end;
end;

end.

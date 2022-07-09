{*******************************************************}
{                                                       }
{       AI Unit for OPENSOLDAT                          }
{                                                       }
{       Copyright (c) 2002 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit AI;

interface

uses
  Sprites;

const
  DIST_AWAY       = 731;
  DIST_TOO_FAR    = 730;
  DIST_VERY_FAR   = 500;
  DIST_FAR        = 350;
  DIST_ROCK_THROW = 180;
  DIST_CLOSE      =  95;
  DIST_VERY_CLOSE =  55;
  DIST_TOO_CLOSE  =  35;

  DIST_COLLIDE    =  20;
  DIST_STOP_PRONE =  25;

function CheckDistance(PosA, PosB: Single): Integer;
procedure SimpleDecision(SNum: Byte);
procedure GoToThing(SNum, TNum: Byte);
procedure ControlBot(var SpriteC: TSprite);

implementation

uses
  Vector, Net, Server, Game, Weapons, Constants, Bullets,Waypoints,
  NetworkServerMessages, Calc;

// Checks the distance on one axis
function CheckDistance(PosA, PosB: Single): Integer;
var
  Distance: Single;
begin
  Result := DIST_AWAY;

  Distance := Abs(PosA - PosB);

  if Distance <= DIST_TOO_CLOSE then
    Result := DIST_TOO_CLOSE

  else if Distance <= DIST_VERY_CLOSE then
    Result := DIST_VERY_CLOSE

  else if Distance <= DIST_CLOSE then
    Result := DIST_CLOSE

  else if Distance <= DIST_ROCK_THROW then
    Result := DIST_ROCK_THROW

  else if Distance <= DIST_FAR then
    Result := DIST_FAR

  else if Distance <= DIST_VERY_FAR then
    Result := DIST_VERY_FAR

  else if Distance <= DIST_TOO_FAR then
    Result := DIST_TOO_FAR
end;

procedure SimpleDecision(SNum: Byte);
var
  m, t, tv: TVector2;
  DistToTargetX, DistToTargetY, Dist: Integer;
  GR, i: Integer;
begin
  with Sprite[SNum] do
  begin
    m := SpriteParts.Pos[SNum];
    t := SpriteParts.Pos[Brain.TargetNum];

    if not Sprite[SNum].Brain.GoThing then
    begin
      Control.Right := False;
      Control.Left  := False;
      if t.x > m.x then
        Control.Right := True;
      if t.x < m.x then
        Control.Left  := True;
    end;

    // X - Distance
    DistToTargetX := CheckDistance(m.x, t.x);

    if DistToTargetX = DIST_TOO_CLOSE then
    begin
      if not Sprite[SNum].Brain.GoThing then
      begin
        Control.Right := False;
        Control.Left  := False;
        if t.x < m.x then
          Control.Right := True;
        if t.x > m.x then
          Control.Left  := True;
      end;
      Control.Fire := True;
    end

    else if DistToTargetX = DIST_VERY_CLOSE then
    begin
      if not Sprite[SNum].Brain.GoThing then
      begin
        Control.Right := False;
        Control.Left  := False;
      end;
      Control.Fire := True;

      // if reloading
      if Weapon.AmmoCount = 0 then
      begin
        if not Sprite[SNum].Brain.GoThing then
        begin
          Control.Right := False;
          Control.Left  := False;
          if t.x < m.x then
            Control.Right := True;
          if t.x > m.x then
            Control.Left  := True;
        end;
        Control.Fire := False;
      end;
    end

    else if DistToTargetX = DIST_CLOSE then
    begin
      if not Sprite[SNum].Brain.GoThing then
      begin
        Control.Right := False;
        Control.Left  := False;
      end;
      Control.Down   := True;
      Control.Fire := True;

      // if reloading
      if Weapon.AmmoCount = 0 then
      begin
        if not Sprite[SNum].Brain.GoThing then
        begin
          Control.Right := False;
          Control.Left  := False;
          if t.x < m.x then
            Control.Right := true;
          if t.x > m.x then
            Control.Left  := True;
        end;
        Control.Down   := False;
        Control.Fire := False;
      end;
    end

    else if DistToTargetX = DIST_ROCK_THROW then
    begin
      Control.Down   := True;
      Control.Fire := True;

      // if reloading
      if Weapon.AmmoCount = 0 then
      begin
        if not Sprite[SNum].Brain.GoThing then
        begin
          Control.Right := False;
          Control.Left  := False;
          if t.x < m.x then
            Control.Right := True;
          if t.x > m.x then
            Control.Left  := True;
        end;
        Control.Down   := False;
        Control.Fire := False;
      end;
    end

    else if DistToTargetX = DIST_FAR then
    begin
      Control.Fire := True;

      if Brain.Camper > 127 then
      begin
        if not Sprite[SNum].Brain.GoThing then
        begin
          Control.Up   := False;
          Control.Down := True;
        end;
      end;
    end

    else if DistToTargetX = DIST_VERY_FAR then
    begin
      Control.Up := True;
      if (Random(2) = 0) or (Weapon.Num = Guns[MINIGUN].Num) then
        Control.Fire := True;

      if Brain.Camper > 0 then
      begin
        if Random(250) = 0 then
          if BodyAnimation.ID <> Prone.ID then
            control.Prone := True;

        if not Sprite[SNum].Brain.GoThing then
        begin
          Control.Right := False;
          Control.Left  := False;
          Control.Up    := False;
          Control.Down  := True;
        end;
      end;
    end

    else if DistToTargetX = DIST_TOO_FAR then
    begin
      if (Random(4) = 0) or (Weapon.Num = Guns[MINIGUN].Num) then
        Control.Fire := True;

      if Brain.Camper > 0 then
      begin
        if Random(300) = 0 then
          if BodyAnimation.ID <> Prone.ID then
            control.Prone := True;

        if not Sprite[SNum].Brain.GoThing then
        begin
          Control.Right := False;
          Control.Left  := False;
          Control.Up    := False;
          Control.Down  := True;
        end;
      end;
    end;

    // move when other player camps
    if not Sprite[SNum].Brain.GoThing then
      if (Sprite[Brain.TargetNum].Brain.CurrentWaypoint > 0) and
        (BotPath.Waypoint[Sprite[Brain.TargetNum].Brain.CurrentWaypoint].Action <>
        TWaypointAction.None) then
      begin
        Control.Right := False;
        Control.Left  := False;
        if t.x > m.x then
          Control.Right := True;
        if t.x < m.x then
          Control.Left  := True;
      end;

    // hide yourself behind collider
    if bots_difficulty.Value < 101 then
      if ColliderDistance < 255 then
      begin
        Control.Down := True;

        if Brain.Camper > 0 then
        begin
          Control.Left  := False;
          Control.Right := False;

          // shoot!
          if (Random(4) = 0) or (Weapon.Num = Guns[MINIGUN].Num) then
            Control.Fire := True;
        end;

        if BodyAnimation.ID = HandsUpAim.ID then
          if BodyAnimation.CurrFrame <> 11 then
            Control.Fire := false;

        {if Brain.Camper > 128 then
        if ColliderDistance < DIST_COLLIDE then
          control.Prone := true;}
      end;

    // get up if not behind collider
    {if ColliderDistance > DIST_STOP_PRONE then
      if BodyAnimation.Name = Prone.Name then
        control.Prone := True;}

    // if target behind collider and bot doesn't escape
    if bots_difficulty.Value < 201 then
      if (Sprite[Brain.TargetNum].ColliderDistance < 255) and
         (ColliderDistance > 254) then
        if Brain.Camper > 0 then
        begin
          if t.x < m.x then
            Control.Right := True;
          if t.x > m.x then
            Control.Left  := True;
        end;

    // go prone
    // Fists!
    if ((Sprite[SNum].Weapon.Num = Guns[NOWEAPON].Num)  or
        (Sprite[SNum].Weapon.Num = Guns[KNIFE].Num)     or
        (Sprite[SNum].Weapon.Num = Guns[CHAINSAW].Num)) and
       (((Sprite[Brain.TargetNum].Weapon.Num <> Guns[NOWEAPON].Num)  and
         (Sprite[Brain.TargetNum].Weapon.Num <> Guns[KNIFE].Num)     and
         (Sprite[Brain.TargetNum].Weapon.Num <> Guns[CHAINSAW].Num)) or
        (SpriteParts.Pos[Brain.TargetNum].Y > SpriteParts.Pos[SNum].Y)) then
    begin
      Control.Right  := False;
      Control.Left   := False;
      Control.Down   := False;
      Control.Fire := True;
      if t.x > m.x then
        Control.Right := True;
      if t.x < m.x then
        Control.Left  := True;
    end;

    // Y - Distance
    DistToTargetY := CheckDistance(m.y, t.y);

    if not Sprite[SNum].Brain.GoThing then
      if (DistToTargetY >= DIST_ROCK_THROW) and (m.y > t.y) then
        Control.Jetpack := True;

    // Flame god see
    if Sprite[Brain.TargetNum].BonusStyle = BONUS_FLAMEGOD then
    begin
      Control.Right := False;
      Control.Left  := False;
      if t.x < m.x then
        Control.Right := True;
      if t.x > m.x then
        Control.Left  := True;
    end;

    // Change weapon if reloading long
    {if Difficulty < 201 then
      if (DistToTargetX < DIST_CLOSE) and (DistToTargetY < DIST_CLOSE) then
        if ((Weapon.AmmoCount = 0) and (Weapon.ReloadTimeCount > 185)) or
            (Weapon.FireIntervalCount > 185) then
          Control.ChangeWeapon := True;}

    // Realistic Mode - Burst Fire
    if sv_realisticmode.Value then
    begin
      if Weapon.Num <> Guns[MINIGUN].Num then
        if BurstCount > 3 then
        begin
          Control.Fire := False;
          if MainTickCounter mod SECOND = 0 then
            BurstCount := 0;
        end;

      if Weapon.Num = Guns[MINIGUN].Num then
        if BurstCount > 30 then
        begin
          Control.Fire := False;
          if MainTickCounter mod SECOND = 0 then
            BurstCount := 0;
        end;
    end;

    if Sprite[SNum].Stat > 0 then
    begin
      Control.Right  := False;
      Control.Left   := False;
      Control.Up     := False;
      Control.Down   := False;
      Control.Fire := True;
    end;

    // Grenade throw
    if Brain.GrenadeFreq > -1 then
    begin
      GR := Brain.GrenadeFreq;
      if (Weapon.AmmoCount = 0) or (Weapon.FireIntervalCount > 125) then
        GR := GR div 2;
      if  (Brain.CurrentWaypoint > 0) and
          (BotPath.Waypoint[Brain.CurrentWaypoint].Action <> TWaypointAction.None) then
        GR := GR div 2;
      if bots_difficulty.Value < 100 then
        GR := GR div 2;

      if bots_difficulty.Value < 201 then
        if  (Random(GR) = 0)                    and
            (DistToTargetX < DIST_FAR)          and
            (TertiaryWeapon.AmmoCount > 0)      and
            (((DistToTargetY < DIST_VERY_CLOSE) and
            (m.y > t.y)) or (m.y < t.y))        then
          Control.ThrowNade := True;
    end;

    // Knife Throw
    if (Sprite[SNum].CeaseFireCounter < 30)             and
       (Sprite[SNum].Weapon.Num = Guns[KNIFE].Num)      and
       (Sprite[SNum].Brain.FavWeapon = Guns[KNIFE].Num) then
    begin
      Control.Fire := False;
      Control.ThrowWeapon := True;
    end;

    vec2scale(tv, SpriteParts.Velocity[Brain.TargetNum], 10);
    vec2add(t, tv);

    Control.MouseAimX := Round(t.x);
    if DistToTargetX < DIST_FAR then
      Control.MouseAimY := Round(t.y - (0.5 * DistToTargetX /
        (Weapon.Speed)) - Brain.Accuracy + Random(Brain.Accuracy))
    else
      Control.MouseAimY := Round(t.y - (1.75 * DistToTargetX /
        (Weapon.Speed)) - Brain.Accuracy + Random(Brain.Accuracy));

    if Sprite[SNum].Stat > 0 then
      Control.MouseAimY := Round(t.y - (0.5 * DistToTargetX / (30)) -
        Brain.Accuracy + Random(Brain.Accuracy));

    // impossible
    if bots_difficulty.Value < 60 then
      if  (Sprite[Brain.TargetNum].Weapon.Num = Guns[BARRETT].Num) or
          (Sprite[Brain.TargetNum].Weapon.Num = Guns[RUGER77].Num) then
      begin
        Dist := Round(Sqrt(Sqr(m.x - t.x) + Sqr(m.y - t.y)));
        Control.MouseAimX := Round(t.X);
        Control.MouseAimY := Round(t.y);

        for i := 1 to
            Round((Dist / Sprite[Brain.TargetNum].Weapon.Speed) * 1.0) do
        begin
          Control.MouseAimX := Control.MouseAimX +
            Round(SpriteParts.Velocity[Brain.TargetNum].X);
          Control.MouseAimY := Control.MouseAimY +
            Round(SpriteParts.Velocity[Brain.TargetNum].Y);
        end;

        if Weapon.FireIntervalCount < 3 then
        begin
          FreeControls;
          Control.Fire := True;
          Control.Down   := True;

          if  (BodyAnimation.ID <> Stand.ID)         and
              (BodyAnimation.ID <> Recoil.ID)        and
              (BodyAnimation.ID <> Prone.ID)         and
              (BodyAnimation.ID <> Shotgun.ID)       and
              (BodyAnimation.ID <> Barret.ID)        and
              (BodyAnimation.ID <> SmallRecoil.ID)   and
              (BodyAnimation.ID <> AimRecoil.ID)     and
              (BodyAnimation.ID <> HandsUpRecoil.ID) and
              (BodyAnimation.ID <> Aim.ID)           and
              (BodyAnimation.ID <> HandsUpAim.ID)    then
            Control.Fire := False;
        end;
      end;

    if sv_realisticmode.Value then
      Control.MouseAimY := Control.MouseAimY - BurstCount * 3;
  end;
end;

procedure GoToThing(SNum, TNum: Byte);
var
  m, t: TVector2;
  DistToTargetX, DistToTargetY: Integer;
begin
  with Sprite[SNum] do
  begin
    m  := SpriteParts.Pos[SNum];
    t  := Thing[TNum].Skeleton.Pos[2];

    if  (Thing[TNum].Skeleton.Pos[2].x > Thing[TNum].Skeleton.Pos[1].x) and
        (m.x < Thing[TNum].Skeleton.Pos[2].x) then
      t := Thing[TNum].Skeleton.Pos[2];
    if  (Thing[TNum].Skeleton.Pos[2].x > Thing[TNum].Skeleton.Pos[1].x) and
        (m.x > Thing[TNum].Skeleton.Pos[1].x) then
      t := Thing[TNum].Skeleton.Pos[1];
    if  (Thing[TNum].Skeleton.Pos[2].x < Thing[TNum].Skeleton.Pos[1].x) and
        (m.x < Thing[TNum].Skeleton.Pos[1].x) then
      t := Thing[TNum].Skeleton.Pos[1];
    if  (Thing[TNum].Skeleton.Pos[2].x < Thing[TNum].Skeleton.Pos[1].x) and
        (m.x > Thing[TNum].Skeleton.Pos[2].x) then
      t := Thing[TNum].Skeleton.Pos[2];

    if Thing[TNum].HoldingSprite > 0 then
      t.y := t.y + 5;

    if t.x >= m.x then
      Control.Right := True;
    if t.x < m.x  then
      Control.Left  := True;

    if  (Thing[TNum].HoldingSprite > 0) and
        (TeamFlag[Player.Team] > TEAM_NONE) then
      if  (Player.Team = Sprite[Thing[TNum].HoldingSprite].Player.Team) and
          (not Thing[TNum].InBase) then
      begin
        // X - Distance
        DistToTargetX := CheckDistance(m.x, t.x);

        if  (DistToTargetX = DIST_TOO_CLOSE) or
            (DistToTargetX = DIST_VERY_CLOSE) then
        begin
          Control.Right := False;
          Control.Left  := False;
          Control.Down  := True;
        end;

        if Sprite[Thing[TNum].HoldingSprite].Control.Jetpack then
          Control.Jetpack := True
        else
          Control.Jetpack := False;
      end;

    // Y - Distance
    DistToTargetY := CheckDistance(m.y, t.y);
    if (DistToTargetY >= DIST_VERY_CLOSE) and (m.y > t.y) then
      Control.Jetpack := True;
  end;
end;

procedure ControlBot(var SpriteC: TSprite);
var
  b, lookpoint, startpoint: TVector2;
  k, i: Integer;
  SeeClosest, SeeThing, RunAway: Boolean;
  D, D2, Dt: Single;
  tempb: Boolean;
begin
  if (SpriteC.Player.ControlMethod = BOT) and
      not SpriteC.DeadMeat and not SpriteC.Dummy then
  // if (MainTickCounter mod (SECOND * 2) = 0) then
  begin
    tempb := SpriteC.Control.ThrowNade;

    SpriteC.FreeControls;

    if SpriteC.BodyAnimation.ID = Throw.ID then
      SpriteC.Control.ThrowNade := tempb
    else
      SpriteC.Control.ThrowNade := False;

    LookPoint.X := SpriteC.Skeleton.Pos[12].X;
    LookPoint.Y := SpriteC.Skeleton.Pos[12].Y - 2;

    // >see?
    // See := False;
    SeeClosest := False;
    D := 999999;
    D2 := 0.0;
    for i := 1 to MAX_SPRITES do
      if Sprite[i].Active and (i <> SpriteC.Num) and
          (Sprite[i].Player.Name <> SpriteC.Brain.Friend) and
          ((Sprite[i].Alpha = 255) or (Sprite[i].HoldedThing > 0)) and
          Sprite[i].IsNotSpectator() then
        if not Sprite[i].DeadMeat or
            (Sprite[i].DeadMeat and
            ((SpriteC.Brain.DeadKill = 1) and (Sprite[i].DeadTime < 180))) then
        begin
          StartPoint := Sprite[i].Skeleton.Pos[12];
          // check if ray startpoint is not in map
          b := Default(TVector2);
          if Map.CollisionTest(StartPoint, b) then
            StartPoint.Y := StartPoint.Y + 6;

          if not Map.RayCast(LookPoint, StartPoint, D2, 651) then
          begin
            if sv_gamemode.Value = GAMESTYLE_RAMBO then
              if (Sprite[i].Weapon.Num = Guns[BOW].Num) or
                  (Sprite[i].Weapon.Num = Guns[BOW2].Num) then
              begin
                SpriteC.Brain.TargetNum := i;
                SeeClosest := True;
                Break;
              end;

            if D > D2 then
            begin
              SpriteC.Brain.TargetNum := i;

              Dt := D;

              if not Sprite[i].DeadMeat then
                D := D2;
              SeeClosest := True;

              // stop throwing grenades and weapons if it's dead
              if Sprite[i].DeadMeat then
              begin
                SpriteC.Control.ThrowNade := False;
                SpriteC.Control.ThrowWeapon := False;
              end;

              if (sv_gamemode.Value = GAMESTYLE_RAMBO) and
                  (SpriteC.Weapon.Num <> Guns[BOW].Num) and
                  (SpriteC.Weapon.Num <> Guns[BOW2].Num) then
              begin
                SeeClosest := False;
                D := dt;
              end;
              if IsTeamGame() and SpriteC.IsInSameTeam(Sprite[i]) then
              begin
                SeeClosest := False;
                //D := dt;
              end;
            end;
          end;  // if see
        end;
    // <see?

    if SpriteC.Brain.TargetNum > 0 then
      if (Sprite[SpriteC.Brain.TargetNum].Weapon.Num = Guns[BOW].Num) or
          (Sprite[SpriteC.Brain.TargetNum].Weapon.Num = Guns[BOW2].Num) then
        SpriteC.Brain.PissedOff := 0;

    if SpriteC.Brain.PissedOff = SpriteC.Num then
      SpriteC.Brain.PissedOff := 0;

    if SpriteC.Brain.PissedOff > 0 then
      if IsTeamGame() and (not sv_friendlyfire.Value) and
          Sprite[SpriteC.Brain.PissedOff].IsInSameTeam(SpriteC) then
        SpriteC.Brain.PissedOff := 0;

    if SpriteC.Brain.TargetNum > 0 then
      if IsTeamGame() and (sv_friendlyfire.Value) and
          Sprite[SpriteC.Brain.TargetNum].IsNotInSameTeam(SpriteC) then
        SpriteC.Brain.PissedOff := 0;

    if SpriteC.Brain.PissedOff > 0 then
    begin
      LookPoint.X := SpriteC.Skeleton.Pos[12].X;
      LookPoint.Y := SpriteC.Skeleton.Pos[12].Y - 2;
      StartPoint := Sprite[SpriteC.Brain.PissedOff].Skeleton.Pos[12];
      if not Map.RayCast(LookPoint, StartPoint, D2, 651) then
      begin
        SpriteC.Brain.TargetNum := SpriteC.Brain.PissedOff;
        SeeClosest := True;
      end
      else
        SpriteC.Brain.PissedOff := 0;
    end;

    // have flag and not hurt, runaway!!!
    RunAway := False;
    if SeeClosest then
      if SpriteC.HoldedThing > 0 then
        if (Thing[SpriteC.HoldedThing].Style = OBJECT_ALPHA_FLAG) or
            (Thing[SpriteC.HoldedThing].Style = OBJECT_BRAVO_FLAG) then
          if Sprite[SpriteC.Brain.TargetNum].HoldedThing = 0 then
          begin
            SeeClosest := False;
            RunAway := True;
          end;

    // GO WITH WAYPOINTS
    if not SeeClosest then  // it doesn't see any target
    begin
      if not SpriteC.Brain.GoThing then
        if SpriteC.Stat = 0 then
        begin
          if SpriteC.Brain.CurrentWaypoint = 0 then
            i := 350
          else
            i := WAYPOINTSEEKRADIUS;  // Radius of waypoint seeking

            k := BotPath.FindClosest(Spriteparts.Pos[SpriteC.Num].X,
              Spriteparts.Pos[SpriteC.Num].Y, i,
              SpriteC.Brain.CurrentWaypoint);

          SpriteC.Brain.OldWaypoint := SpriteC.Brain.CurrentWaypoint;

          // current pathnum
          // FIXME set an initial waypoint. this previously did an out-of-bounds read, so the
          // next assignment doesn't make it worse...
          if SpriteC.Brain.NextWaypoint = 0 then
            SpriteC.Brain.NextWaypoint := 1;
          SpriteC.Brain.PathNum := BotPath.Waypoint[SpriteC.Brain.NextWaypoint].PathNum;

          // pathnum for CTF
          if sv_gamemode.Value = GAMESTYLE_CTF then
          begin
            SpriteC.Brain.PathNum := SpriteC.Player.Team;

            // i have the flag!
            if SpriteC.HoldedThing > 0 then
              if (Thing[SpriteC.HoldedThing].Style = OBJECT_ALPHA_FLAG) or
                  (Thing[SpriteC.HoldedThing].Style = OBJECT_BRAVO_FLAG) then
              begin
                if SpriteC.Player.Team = TEAM_ALPHA then
                  SpriteC.Brain.PathNum := 2;
                if SpriteC.Player.Team = TEAM_BRAVO then
                  SpriteC.Brain.PathNum := 1;
              end;
          end;

          // pathnum for HTF
          if sv_gamemode.Value = GAMESTYLE_HTF then
          begin
            SpriteC.Brain.PathNum := SpriteC.Player.Team;

            // i have the flag!
            if SpriteC.HoldedThing > 0 then
              if Thing[SpriteC.HoldedThing].Style = OBJECT_POINTMATCH_FLAG then
              begin
                if SpriteC.Player.Team = TEAM_ALPHA then
                  SpriteC.Brain.PathNum := 2;
                if SpriteC.Player.Team = TEAM_BRAVO then
                  SpriteC.Brain.PathNum := 1;
              end;
          end;

          // pathnum for Infiltration
          if sv_gamemode.Value = GAMESTYLE_INF then
          begin
            if SpriteC.Player.Team = TEAM_ALPHA then
              SpriteC.Brain.PathNum := 1;
            if SpriteC.Player.Team = TEAM_BRAVO then
              SpriteC.Brain.PathNum := 2;

            if not Thing[TeamFlag[2]].InBase then
              if SpriteC.Player.Team = TEAM_BRAVO then
                SpriteC.Brain.PathNum := 2;

            // i have the flag!
            if SpriteC.HoldedThing > 0 then
              if (Thing[SpriteC.HoldedThing].Style = OBJECT_ALPHA_FLAG) or
                  (Thing[SpriteC.HoldedThing].Style = OBJECT_BRAVO_FLAG) then
              begin
                if SpriteC.Player.Team = TEAM_ALPHA then
                  SpriteC.Brain.PathNum := 2;
                if SpriteC.Player.Team = TEAM_BRAVO then
                  SpriteC.Brain.PathNum := 1;
              end;
          end;

          if (SpriteC.Brain.CurrentWaypoint = 0) or (k > 0) then
            if (SpriteC.Brain.PathNum = BotPath.Waypoint[k].PathNum) or
                (SpriteC.Brain.CurrentWaypoint = 0) then
            begin
              SpriteC.Brain.CurrentWaypoint := k;
            end;

          if (SpriteC.Brain.CurrentWaypoint > 0) and
              (SpriteC.Brain.CurrentWaypoint < MAX_WAYPOINTS) then
          begin
            if (SpriteC.Brain.OldWaypoint <> SpriteC.Brain.CurrentWaypoint)
              {and (BotPath.Waypoint[SpriteC.CurrentWaypoint].ConnectionsNum > 0)} then
            begin
              k := Random(BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].ConnectionsNum) + 1;
              if (k > 0) and (k < MAX_WAYPOINTS) and
                  (BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Connections[k] > 0) and
                  (BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Connections[k] < MAX_WAYPOINTS) then
              begin
                SpriteC.Brain.NextWaypoint := BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Connections[k];

                // face target
                SpriteC.Control.MouseAimX := Round(BotPath.Waypoint[SpriteC.Brain.NextWaypoint].X);
                SpriteC.Control.MouseAimY := Round(BotPath.Waypoint[SpriteC.Brain.NextWaypoint].Y);
              end;
            end;

            // apply waypoint movements to sprite
            SpriteC.Control.Left := BotPath.Waypoint[SpriteC.Brain.NextWaypoint].Left;
            SpriteC.Control.Right := BotPath.Waypoint[SpriteC.Brain.NextWaypoint].Right;
            SpriteC.Control.Up := BotPath.Waypoint[SpriteC.Brain.NextWaypoint].Up;
            SpriteC.Control.Down := BotPath.Waypoint[SpriteC.Brain.NextWaypoint].Down;
            SpriteC.Control.Jetpack := BotPath.Waypoint[SpriteC.Brain.NextWaypoint].Jetpack;

            // Special waypoint
            if ((sv_gamemode.Value = GAMESTYLE_INF) and
                (SpriteC.Player.Team = TEAM_BRAVO) and
                (Thing[TeamFlag[2]].InBase) and
                (SpriteC.HoldedThing = 0)) or
                ((sv_gamemode.Value = GAMESTYLE_CTF) and
                (SpriteC.HoldedThing = 0)) or
                ((sv_gamemode.Value <> GAMESTYLE_INF) and
                (sv_gamemode.Value <> GAMESTYLE_CTF) and
                (sv_gamemode.Value <> GAMESTYLE_HTF)) then
              // not infiltration escape path
              if (BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.StopAndCamp) or
                  ((BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.Wait1Second) and
                  (SpriteC.Brain.OnePlaceCount < 60)) or
                  ((BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.Wait5Seconds) and
                  (SpriteC.Brain.OnePlaceCount < 300)) or
                  ((BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.Wait10Seconds) and
                  (SpriteC.Brain.OnePlaceCount < 600)) or
                  ((BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.Wait15Seconds) and
                  (SpriteC.Brain.OnePlaceCount < 900)) or
                  ((BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.Wait20Seconds) and
                  (SpriteC.Brain.OnePlaceCount < 1200)) then
              begin
                SpriteC.Control.Left := False;
                SpriteC.Control.Right := False;
                SpriteC.Control.Up := False;
                SpriteC.Control.Down := False;
                SpriteC.Control.Jetpack := False;

                if SpriteC.Stat = 0 then
                  if SpriteC.Brain.Camper > 0 then
                    if SpriteC.Brain.OnePlaceCount > 180 then
                      SpriteC.Control.Down := True;
              end;

            // fire at guy that is shooting me while running away
            if RunAway then
              if SpriteC.Brain.PissedOff > 0 then
              begin
                SpriteC.Control.MouseAimX := Round(Spriteparts.Pos[SpriteC.Brain.PissedOff].x);
                SpriteC.Control.MouseAimY := Round(Spriteparts.Pos[SpriteC.Brain.PissedOff].y -
                  (1.75 * 100 / SpriteC.Weapon.Speed) -
                  SpriteC.Brain.Accuracy + Random(SpriteC.Brain.Accuracy));
                SpriteC.Control.Fire := True;
              end;

            if SpriteC.Brain.LastWaypoint = SpriteC.Brain.CurrentWaypoint then
              Inc(SpriteC.Brain.WaypointTime)
            else
              SpriteC.Brain.WaypointTime := 0;
            SpriteC.Brain.LastWaypoint := SpriteC.Brain.CurrentWaypoint;

            // check if standing in place because stuck or sth
            if SpriteC.Brain.CurrentWaypoint > 0 then
              if BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.None then
              begin
                if (SpriteC.Control.Left or SpriteC.Control.Right) and
                    not SpriteC.Control.Down then
                begin
                  if Distance(Spriteparts.Pos[SpriteC.Num], Spriteparts.OldPos[SpriteC.Num]) < 3 then
                  begin
                    Inc(SpriteC.Brain.OnePlaceCount);
                  end else
                    SpriteC.Brain.OnePlaceCount := 0;
                end else
                  SpriteC.Brain.OnePlaceCount := 0;
              end else
                Inc(SpriteC.Brain.OnePlaceCount);

            if BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.None then
              if SpriteC.Brain.OnePlaceCount > 90 then
              begin
                if SpriteC.Control.Left and SpriteC.Control.Right then
                  SpriteC.Control.Right := False;
                SpriteC.Control.Up := True;
              end;

            // change weapon back
            if bots_difficulty.Value < 201 then
              if ((SpriteC.Weapon.Num = Guns[COLT].Num) or
                  (SpriteC.Weapon.Num = Guns[NOWEAPON].Num) or
                  (SpriteC.Weapon.Num = Guns[KNIFE].Num) or
                  (SpriteC.Weapon.Num = Guns[CHAINSAW].Num) or
                  (SpriteC.Weapon.Num = Guns[LAW].Num)) and
                  (SpriteC.SecondaryWeapon.Num <> Guns[NOWEAPON].Num) then
                SpriteC.Control.ChangeWeapon := True;

            // reload if low ammo
            if bots_difficulty.Value < 201 then
              if (SpriteC.Weapon.AmmoCount < 4) and (SpriteC.Weapon.Ammo > 3) then
                SpriteC.Control.Reload := True;

            // get up if prone
            if Random(150) = 0 then
              if (SpriteC.BodyAnimation.ID = Prone.ID) or
                  (SpriteC.BodyAnimation.ID = ProneMove.ID) then
                SpriteC.Control.Prone := True;
          end;  // SpriteC.CurrentWaypoint>0
        end;  // gothing
    end
    else
    begin
      if (SpriteC.Brain.CurrentWaypoint <> 0) and
          (BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.None) then
        SpriteC.Brain.CurrentWaypoint := 0;

      SimpleDecision(SpriteC.Num);

      // Camp
      if (SpriteC.Brain.CurrentWaypoint > 0) and
          (((sv_gamemode.Value = GAMESTYLE_INF) and (SpriteC.Player.Team = TEAM_BRAVO)) or
          ((sv_gamemode.Value = GAMESTYLE_CTF) and (SpriteC.HoldedThing = 0)) or
          ((sv_gamemode.Value <> GAMESTYLE_INF) and (sv_gamemode.Value <> GAMESTYLE_CTF)))
           then
        // not infiltration escape path
        if BotPath.Waypoint[SpriteC.Brain.CurrentWaypoint].Action = TWaypointAction.StopAndCamp then
        begin
          SpriteC.Control.Left := False;
          SpriteC.Control.Right := False;
          SpriteC.Control.Up := False;
          SpriteC.Control.Down := False;
          SpriteC.Control.Jetpack := False;
        end;

      if bots_chat.Value then
      begin
        if Random(115 * SpriteC.Brain.ChatFreq) = 0 then
          ServerSendStringMessage(WideString(SpriteC.Brain.ChatSeeEnemy),
            ALL_PLAYERS, SpriteC.Num, MSGTYPE_PUB);

        if Random(790 * SpriteC.Brain.ChatFreq) = 0 then
          ServerSendStringMessage('Die ' + WideString(Sprite[SpriteC.Brain.TargetNum].Player.Name) +
            '!', ALL_PLAYERS, SpriteC.Num, MSGTYPE_PUB);
      end;

      SpriteC.Brain.WaypointTime := 0;
    end;

    SeeThing := False;
    // ThingHolded := False;
    LookPoint.X := SpriteC.Skeleton.Pos[12].X;
    LookPoint.Y := SpriteC.Skeleton.Pos[12].Y - 4;
    // look for flag or bow
    for i := 1 to MAX_THINGS do
      if not SeeThing and Thing[i].Active and (Thing[i].HoldingSprite <> SpriteC.Num) and
          ((Thing[i].Style = OBJECT_ALPHA_FLAG) or (Thing[i].Style = OBJECT_BRAVO_FLAG) or
          (Thing[i].Style = OBJECT_POINTMATCH_FLAG) or (Thing[i].Style = OBJECT_RAMBO_BOW) or
          (Thing[i].Style = OBJECT_FLAMER_KIT) or (Thing[i].Style = OBJECT_PREDATOR_KIT) or
          (Thing[i].Style = OBJECT_VEST_KIT) or (Thing[i].Style = OBJECT_BERSERK_KIT) or
          (Thing[i].Style = OBJECT_COMBAT_KNIFE) or
          ((Thing[i].Style = OBJECT_MEDICAL_KIT) and (SpriteC.Health < STARTHEALTH)) or
          ((Thing[i].Style = OBJECT_GRENADE_KIT) and
          (SpriteC.TertiaryWeapon.AmmoCount < sv_maxgrenades.Value) and
            ((SpriteC.TertiaryWeapon.Num <> Guns[CLUSTERGRENADE].Num) or
            (SpriteC.TertiaryWeapon.AmmoCount = 0)))) then
      begin
        StartPoint.X := Thing[i].Skeleton.Pos[2].X;
        StartPoint.Y := Thing[i].Skeleton.Pos[2].Y - 5;

        if not Map.RayCast(LookPoint, StartPoint, D2, 651) then
          if D2 < DIST_FAR then
          begin  // i see the flag! or bow or sth
            SeeThing := True;

            // dont take it if is my flag in base
            if ((sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF)) and
                (Thing[i].Style = SpriteC.Player.Team) and Thing[i].InBase then
            begin
              SeeThing := False;
              if (SpriteC.HoldedThing > 0) and (i <> SpriteC.HoldedThing) then
                if Thing[SpriteC.HoldedThing].HoldingSprite = SpriteC.Num then
                  SeeThing := True;
            end;
            // dont follow this flag if my flag is not inbase
            if ((sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF)) and
                (Thing[i].Style <> SpriteC.Player.Team) and
                not Thing[TeamFlag[SpriteC.Player.Team]].InBase then
              SeeThing := False;
            // dont take it if is flag in base
            if ((sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF)) and
                (Thing[i].Style <> SpriteC.Player.Team) and
                (Thing[i].Style < OBJECT_USSOCOM) and Thing[i].InBase and
                (D2 > DIST_CLOSE) then
              SeeThing := False;
            // or better take it if hurt and medikit is close
            if (Thing[i].Style = OBJECT_MEDICAL_KIT) and
                (SpriteC.Health < HURT_HEALTH) and
                (D2 < DIST_VERY_CLOSE) then
              SeeThing := True;
            // dont take it when running away with flag
            if ((Thing[i].Style = OBJECT_MEDICAL_KIT) or (Thing[i].Style = OBJECT_GRENADE_KIT) or
                (Thing[i].Style = OBJECT_FLAMER_KIT) or (Thing[i].Style = OBJECT_PREDATOR_KIT) or
                (Thing[i].Style = OBJECT_BERSERK_KIT)) and RunAway then
              SeeThing := False;
            if ((Thing[i].Style = OBJECT_FLAMER_KIT) or (Thing[i].Style = OBJECT_PREDATOR_KIT) or
                (Thing[i].Style = OBJECT_BERSERK_KIT)) and
                (SpriteC.BonusStyle > BONUS_NONE) then
              SeeThing := False;
            if Thing[i].Style = OBJECT_COMBAT_KNIFE then
              SeeThing := True;

            // throw away weapon
            if (D2 < 30) and (Thing[i].Style = OBJECT_RAMBO_BOW) then
              SpriteC.Control.ThrowWeapon := True;

            if SeeThing then
            begin
              if Thing[i].HoldingSprite = 0 then
                Dec(Thing[i].Interest);

              if Thing[i].Interest > 0 then
              begin
                if bots_chat.Value then
                begin
                  if Thing[i].Style < OBJECT_POINTMATCH_FLAG then
                    if Random(400 * SpriteC.Brain.ChatFreq) = 0 then
                      ServerSendStringMessage('Flag!', ALL_PLAYERS, SpriteC.Num, MSGTYPE_PUB);
                end;

                SpriteC.Brain.GoThing := True;
                GoToThing(SpriteC.Num, i);
              end
              else
                SpriteC.Brain.GoThing := False;

              // Pickup knife!
              if ((Thing[i].Style = OBJECT_COMBAT_KNIFE) and
                  (SpriteC.Weapon.Num = Guns[NOWEAPON].Num) and
                  (SpriteC.Brain.FavWeapon = Guns[KNIFE].Num)) then
              begin
                SpriteC.Control.Fire := False;
                SpriteC.Brain.TargetNum := 0;
                //ServerSendStringMessage(' Looking for knife...', SpriteC.Num);
                SpriteC.Brain.GoThing := True;
                GoToThing(SpriteC.Num, i);
              end;
            end;
          end;
      end;
    // <see flag?

    if not SeeThing then
      SpriteC.Brain.GoThing := False;

    // Runaway from grenade!
    if bots_difficulty.Value < 201 then
      for i := 1 to MAX_BULLETS do
        if Bullet[i].Active and (Bullet[i].Style = BULLET_STYLE_FRAGNADE) and
            (Distance(BulletParts.Pos[i].X, BulletParts.Pos[i].Y,
            SpriteParts.Pos[SpriteC.Num].X, SpriteParts.Pos[SpriteC.Num].Y) <
            (FRAGGRENADE_EXPLOSION_RADIUS * 1.4)) then
        begin
          if BulletParts.Pos[i].X > SpriteParts.Pos[SpriteC.Num].X then
          begin
            SpriteC.Control.Left := True;
            SpriteC.Control.Right := False;
          end else
          begin
            SpriteC.Control.Right := True;
            SpriteC.Control.Left := False;
          end;
        end;

    // release grenade
    if (SpriteC.BodyAnimation.ID = Throw.ID) and
        (SpriteC.BodyAnimation.CurrFrame > 35) then
      SpriteC.Control.ThrowNade := False;

    Dec(SpriteC.Brain.WaypointTimeoutCounter);
    if SpriteC.Brain.WaypointTimeoutCounter < 0 then
    begin
      SpriteC.Brain.CurrentWaypoint := SpriteC.Brain.OldWaypoint;
      SpriteC.Brain.WaypointTimeoutCounter := WAYPOINTTIMEOUT;
      SpriteC.FreeControls;
      SpriteC.Control.Up := True;
    end;

    // waypoint is shit
    if SpriteC.Brain.WaypointTime > WAYPOINT_TIMEOUT then
    begin
      SpriteC.FreeControls;
      SpriteC.Brain.CurrentWaypoint := 0;
      SpriteC.Brain.GoThing := False;
      SpriteC.Brain.WaypointTime := 0;
    end;

    // fall damage save
    D := SpriteParts.Velocity[SpriteC.Num].Y;
    if D > 3.35 then
      SpriteC.Brain.FallSave := 1;
    if D < 1.35 then
      SpriteC.Brain.FallSave := 0;
    if SpriteC.Brain.FallSave > 0 then
      SpriteC.Control.Jetpack := True;

    // Bot Chat
    if bots_chat.Value then
      if Random(SpriteC.Brain.ChatFreq * 150) = 0 then
      begin
        if SortedPlayers[1].PlayerNum = SpriteC.Num then
          ServerSendStringMessage(WideString(SpriteC.Brain.ChatWinning),
            ALL_PLAYERS, SpriteC.Num, MSGTYPE_PUB);
      end;

    if Random(190) = 0 then
      SpriteC.Brain.PissedOff := 0;

    if SpriteC.Stat > 0 then
    begin
      Inc(SpriteC.Brain.OnePlaceCount);
      if  ((SpriteC.Brain.OnePlaceCount >  120) and (SpriteC.Brain.OnePlaceCount <  220)) or
          ((SpriteC.Brain.OnePlaceCount >  350) and (SpriteC.Brain.OnePlaceCount <  620)) or
          ((SpriteC.Brain.OnePlaceCount >  700) and (SpriteC.Brain.OnePlaceCount <  740)) or
          ((SpriteC.Brain.OnePlaceCount >  900) and (SpriteC.Brain.OnePlaceCount < 1100)) or
          ((SpriteC.Brain.OnePlaceCount > 1300) and (SpriteC.Brain.OnePlaceCount < 1500)) then
      begin
        SpriteC.Control.Fire := True;
        if Random(2) = 0 then
          SpriteC.Control.MouseAimY := SpriteC.Control.MouseAimY + Random(4)
        else
          SpriteC.Control.MouseAimY := SpriteC.Control.MouseAimY - Random(4);
      end;

      if SpriteC.Brain.OnePlaceCount > 1500 then
        SpriteC.Brain.OnePlaceCount := 0;
    end;

    // destroy weapon if fav weapon hands
    {if NoWeapon.Name = SpriteC.Brain.FavWeapon then
        if SpriteC.Weapon.Num <> Guns[NOWEAPON].Num then
          SpriteC.Weapon := Guns[NOWEAPON];}
  end;  // Bot
end;

end.

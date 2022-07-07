unit NetworkClientThing;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // helper units
  Vector, Util,

  // opensoldat units
  Calc, LogFile, Steam, Net, Sprites, Weapons, Sound,
  Constants, GameStrings;

procedure ClientHandleServerThingSnapshot(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleServerThingMustSnapshot(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleThingTaken(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  Client, NetworkUtils, NetworkClientSprite, Game,
  Demo, ClientGame, Things;

procedure ClientHandleServerThingSnapshot(NetMessage: PSteamNetworkingMessage_t);
var
  ThingSnap: TMsg_ServerThingSnapshot;
  i, d: Integer;
  a: TVector2;
begin
  if not VerifyPacket(sizeof(TMsg_ServerThingSnapshot), NetMessage^.m_cbSize, MsgID_ServerThingSnapshot) then
    Exit;

  ThingSnap := PMsg_ServerThingSnapshot(NetMessage^.m_pData)^;

  // assign received Thing info to thing
  i := ThingSnap.Num;

  if (i < 1) or (i > MAX_THINGS) then
    Exit;

  a.x := ThingSnap.Pos[1].X;
  a.y := ThingSnap.Pos[1].Y;

  if (not Thing[i].Active) or (Thing[i].Style <> ThingSnap.Style) then
  begin
    CreateThing(a, ThingSnap.Owner, ThingSnap.Style, i);

    for d := 1 to 4 do
    begin
      Thing[i].Skeleton.Pos[d].X := ThingSnap.Pos[d].X;
      Thing[i].Skeleton.Pos[d].Y := ThingSnap.Pos[d].Y;
      Thing[i].Skeleton.OldPos[d].X := ThingSnap.OldPos[d].X;
      Thing[i].Skeleton.OldPos[d].Y := ThingSnap.OldPos[d].Y;
    end;
  end;

  Thing[i].HoldingSprite := ThingSnap.HoldingSprite;

  // is not holded anymore
  if Thing[i].HoldingSprite = 0 then
  begin
    for d := 1 to MAX_SPRITES do
      if Sprite[d].Active then
      begin
        if Sprite[d].HoldedThing = i then
          Sprite[d].HoldedThing := 0;
      end;
  end;

  if (ThingSnap.Owner > 0) and (ThingSnap.Owner < MAX_SPRITES + 1) then
  begin
    if Thing[i].HoldingSprite > 0 then
    begin
      Sprite[ThingSnap.Owner].HoldedThing := i;
      Sprite[ThingSnap.Owner].OnGround := false;
    end;

    Thing[i].Color := Sprite[ThingSnap.Owner].Player.ShirtColor;
  end;

  if (Thing[i].HoldingSprite = 0) and (Thing[i].Style <> OBJECT_STATIONARY_GUN) then
    if (Distance(Thing[i].Skeleton.Pos[1], ThingSnap.Pos[1]) > 10)
      and (Distance(Thing[i].Skeleton.Pos[2], ThingSnap.Pos[2]) > 10) then
      for d := 1 to 4 do
      begin
        Thing[i].Skeleton.Pos[d].X := ThingSnap.Pos[d].X;
        Thing[i].Skeleton.Pos[d].Y := ThingSnap.Pos[d].Y;
        Thing[i].Skeleton.OldPos[d].X := ThingSnap.OldPos[d].X;
        Thing[i].Skeleton.OldPos[d].Y := ThingSnap.OldPos[d].Y;
      end;

  if (Thing[i].HoldingSprite > 0) and (Thing[i].Style <> OBJECT_PARACHUTE) then
    if Distance(Thing[i].Skeleton.Pos[1],
      Spriteparts.Pos[Thing[i].HoldingSprite]) > 330 then
      for d := 1 to 4 do
      begin
        Thing[i].Skeleton.Pos[d].X := ThingSnap.Pos[d].X;
        Thing[i].Skeleton.Pos[d].Y := ThingSnap.Pos[d].Y;
        Thing[i].Skeleton.OldPos[d].X := ThingSnap.OldPos[d].X;
        Thing[i].Skeleton.OldPos[d].Y := ThingSnap.OldPos[d].Y;
      end;

  Thing[i].StaticType := false;

  if Thing[i].Style = OBJECT_RAMBO_BOW then
    GameThingTarget := i;
end;

procedure ClientHandleServerThingMustSnapshot(NetMessage: PSteamNetworkingMessage_t);
var
  ThingMustSnap: TMsg_ServerThingMustSnapshot;
  i, d: Integer;
  a: TVector2;
  SpriteThingOwner: ^TSprite;
  WeaponThing: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_ServerThingMustSnapshot), NetMessage^.m_cbSize, MsgID_ServerThingMustSnapshot) then
    Exit;

  ThingMustSnap := PMsg_ServerThingMustSnapshot(NetMessage^.m_pData)^;

  // assign received Thing info to thing
  i := ThingMustSnap.Num;

  if (i < 1) or (i > MAX_THINGS) then
    Exit;

  if (ThingMustSnap.Owner > 0) and (ThingMustSnap.Owner < MAX_SPRITES + 1) then
    SpriteThingOwner := @Sprite[ThingMustSnap.Owner]
  else
    SpriteThingOwner := nil;

  if (not Thing[i].Active) or (Thing[i].Style <> ThingMustSnap.Style) then
  begin
    a.x := ThingMustSnap.Pos[1].X;
    a.y := ThingMustSnap.Pos[1].Y;
    CreateThing(a, ThingMustSnap.Owner, ThingMustSnap.Style, i);

    for d := 1 to 4 do
    begin
      Thing[i].Skeleton.Pos[d].X := ThingMustSnap.Pos[d].X;
      Thing[i].Skeleton.Pos[d].Y := ThingMustSnap.Pos[d].Y;
      Thing[i].Skeleton.OldPos[d].X := ThingMustSnap.OldPos[d].X;
      Thing[i].Skeleton.OldPos[d].Y := ThingMustSnap.OldPos[d].Y;
    end;

    if SpriteThingOwner <> nil then
    begin
      case ThingMustSnap.Style of
        OBJECT_USSOCOM:      WeaponThing := COLT_NUM;
        OBJECT_DESERT_EAGLE: WeaponThing := EAGLE_NUM;
        OBJECT_HK_MP5:       WeaponThing := MP5_NUM;
        OBJECT_AK74:         WeaponThing := AK74_NUM;
        OBJECT_STEYR_AUG:    WeaponThing := STEYRAUG_NUM;
        OBJECT_SPAS12:       WeaponThing := SPAS12_NUM;
        OBJECT_RUGER77:      WeaponThing := RUGER77_NUM;
        OBJECT_M79:          WeaponThing := M79_NUM;
        OBJECT_BARRET_M82A1: WeaponThing := BARRETT_NUM;
        OBJECT_MINIMI:       WeaponThing := M249_NUM;
        OBJECT_MINIGUN:      WeaponThing := MINIGUN_NUM;
      //OBJECT_COMBAT_KNIFE: WeaponThing := KNIFE_NUM;
        OBJECT_CHAINSAW:     WeaponThing := CHAINSAW_NUM;
        OBJECT_LAW:          WeaponThing := LAW_NUM;
        else WeaponThing := -1;
      end;

      if WeaponThing > -1 then
      begin
        if SpriteThingOwner.Weapon.Num = WeaponThing then
          SpriteThingOwner.ApplyWeaponByNum(NOWEAPON_NUM, 1)
        else if SpriteThingOwner.SecondaryWeapon.Num = WeaponThing then
          SpriteThingOwner.ApplyWeaponByNum(NOWEAPON_NUM, 2);
        if (SpriteThingOwner.Num = MySprite) and not SpriteThingOwner.DeadMeat then
          ClientSpriteSnapshot;
      end;
    end;
  end;

  Thing[i].Owner := ThingMustSnap.Owner;
  Thing[i].HoldingSprite := ThingMustSnap.HoldingSprite;

  // is not holded anymore
  if Thing[i].HoldingSprite = 0 then
  begin
    for d := 1 to MAX_SPRITES do
      if Sprite[d].Active then
      begin
        if Sprite[d].HoldedThing = i then
          Sprite[d].HoldedThing := 0;
      end;
  end;

  if (ThingMustSnap.Owner > 0) and
     (ThingMustSnap.Owner < MAX_SPRITES + 1) then
  begin
    if Thing[i].HoldingSprite > 0 then
    begin
      SpriteThingOwner.HoldedThing := i;
      SpriteThingOwner.OnGround := False;
    end;

    Thing[i].Color := SpriteThingOwner.Player.ShirtColor;
  end;

  if (Thing[i].HoldingSprite = 0) and (not Thing[i].Style = OBJECT_STATIONARY_GUN) then
    for d := 1 to 4 do
    begin
      Thing[i].Skeleton.Pos[d].X := ThingMustSnap.Pos[d].X;
      Thing[i].Skeleton.Pos[d].Y := ThingMustSnap.Pos[d].Y;
      Thing[i].Skeleton.OldPos[d].X := ThingMustSnap.OldPos[d].X;
      Thing[i].Skeleton.OldPos[d].Y := ThingMustSnap.OldPos[d].Y;
    end;

  Thing[i].Timeout := ThingMustSnap.Timeout;
  Thing[i].StaticType := false;
  if Thing[i].Style = OBJECT_RAMBO_BOW then
    GameThingTarget := i;
end;

procedure ClientHandleThingTaken(NetMessage: PSteamNetworkingMessage_t);
var
  ThingTakenSnap: TMsg_ServerThingTaken;
  i, j, n: Integer;
  RequestThingMsg: TMsg_RequestThing;
  WeaponIndex: Byte;
  CapColor: Cardinal = CAPTURE_MESSAGE_COLOR;
  BigCapText: WideString = '';
  SmallCapText: WideString = '';
begin
  if not VerifyPacket(sizeof(TMsg_ServerThingTaken), NetMessage^.m_cbSize, MsgID_ThingTaken) then
    Exit;

  ThingTakenSnap := PMsg_ServerThingTaken(NetMessage^.m_pData)^;

  i := ThingTakenSnap.Num;
  if (i < 1) or (i > MAX_THINGS) then
    Exit;

  if (ThingTakenSnap.Who = 255) then
    Thing[i].Kill;

  if (ThingTakenSnap.Who < 1) or (ThingTakenSnap.Who > MAX_SPRITES) then
    Exit;

  if not Sprite[ThingTakenSnap.Who].Active then
    Exit;

  if (not Thing[i].Active) and ((ThingTakenSnap.Style = OBJECT_STATIONARY_GUN) or
    (ThingTakenSnap.Style < OBJECT_USSOCOM)) then
  begin
    // request the new thing if not active
    RequestThingMsg.Header.ID := MsgID_RequestThing;
    RequestThingMsg.ThingID := i;
    UDP.SendData(RequestThingMsg, sizeof(RequestThingMsg), k_nSteamNetworkingSend_Unreliable);
    Exit;
  end;

  Thing[i].Style := ThingTakenSnap.Style;

  j := ThingTakenSnap.Who;
  if Sprite[j].Weapon.Num = Guns[NOWEAPON].Num then
    n := 1
  else
    n := 2;

  case Thing[i].Style of
    OBJECT_ALPHA_FLAG, OBJECT_BRAVO_FLAG, OBJECT_POINTMATCH_FLAG:
      begin
        // capture sound
        PlaySound(SFX_CAPTURE, Thing[i].Skeleton.Pos[1]);
        Thing[i].HoldingSprite := ThingTakenSnap.Who;
        Thing[i].StaticType := False;

        j := ThingTakenSnap.Who;

        case sv_gamemode.Value of
          GAMESTYLE_POINTMATCH, GAMESTYLE_INF:
            CapColor := CAPTURE_MESSAGE_COLOR;
          GAMESTYLE_HTF, GAMESTYLE_CTF:
            case Sprite[j].Player.Team of
              TEAM_ALPHA: CapColor := ALPHA_MESSAGE_COLOR;
              TEAM_BRAVO: CapColor := BRAVO_MESSAGE_COLOR;
            end;
        end;

        SmallCapText := '';

        case sv_gamemode.Value of
          GAMESTYLE_POINTMATCH, GAMESTYLE_HTF:
          begin
            BigCapText   := iif(j = MySprite, _('You got the Flag!'), _('Yellow Flag captured!'));
            SmallCapText := _('%s got the Yellow Flag');
          end;
          GAMESTYLE_CTF:
            if Sprite[j].Player.Team = Thing[i].Style then
            begin
              case Sprite[j].Player.Team of
                TEAM_ALPHA:
                begin
                  BigCapText   := _('Red Flag returned!');
                  SmallCapText := _('%s returned the Red Flag');
                end;
                TEAM_BRAVO:
                begin
                  BigCapText   := _('Blue Flag returned!');
                  SmallCapText := _('%s returned the Blue Flag');
                end;
              end;
              Thing[i].Respawn;
            end else
            begin
              case Sprite[j].Player.Team of
                TEAM_ALPHA:
                begin
                  BigCapText   := iif(j = MySprite, _('You got the Blue Flag!'), _('Blue Flag captured!'));
                  SmallCapText := _('%s captured the Blue Flag');
                end;
                TEAM_BRAVO:
                begin
                  BigCapText   := iif(j = MySprite, _('You got the Red Flag!'), _('Red Flag captured!'));
                  SmallCapText := _('%s captured the Red Flag');
                end;
              end;
            end;
          GAMESTYLE_INF:
            if Sprite[j].Player.Team = Thing[i].Style then
            begin
              if Sprite[j].Player.Team = TEAM_BRAVO then
              begin
                BigCapText   := iif(j = MySprite, _('You returned the Objective!'), _('Objective returned!'));
                SmallCapText := _('%s returned the Objective');
              end;
              Thing[i].Respawn;
            end else
            begin
              if Sprite[j].Player.Team = TEAM_ALPHA then
              begin
                BigCapText   := iif(j = MySprite, _('You got the Objective!'), _('Objective captured!'));
                SmallCapText := _('%s captured the Objective');
              end;
            end;
        end;

        if SmallCapText <> '' then
        begin
          BigMessage(BigCapText, CAPTUREMESSAGEWAIT, CapColor);
          MainConsole.Console(WideFormat(SmallCapText, [Sprite[j].Player.Name]), CapColor);
        end;
      end;
    OBJECT_USSOCOM, OBJECT_DESERT_EAGLE, OBJECT_HK_MP5, OBJECT_AK74, OBJECT_STEYR_AUG,
    OBJECT_SPAS12, OBJECT_RUGER77, OBJECT_M79, OBJECT_BARRET_M82A1, OBJECT_MINIMI, OBJECT_MINIGUN:
      begin
        // Objects 1-3 are flags, so we need for WeaponIndex subtract by flags+1
        WeaponIndex := WeaponNumToIndex(Thing[i].Style - (OBJECT_NUM_FLAGS + 1));
        Sprite[ThingTakenSnap.Who].ApplyWeaponByNum(Guns[WeaponIndex].Num, n, ThingTakenSnap.AmmoCount);
        if (ThingTakenSnap.Who = MySprite) and not Sprite[MySprite].DeadMeat then
          ClientSpriteSnapshot;
      end;
    OBJECT_RAMBO_BOW:
      begin
        PlaySound(SFX_TAKEBOW, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].ApplyWeaponByNum(Guns[BOW].Num, 1, 1);
        Sprite[ThingTakenSnap.Who].ApplyWeaponByNum(Guns[BOW2].Num, 2, 1);
        Sprite[ThingTakenSnap.Who].Weapon.AmmoCount := 1;
        Sprite[ThingTakenSnap.Who].Weapon.FireInterval := 10;
        Sprite[ThingTakenSnap.Who].WearHelmet := 1;
        Thing[i].Kill;
        GameThingTarget := 0;

        if (ThingTakenSnap.Who = MySprite) and not Sprite[MySprite].DeadMeat then
          ClientSpriteSnapshot;

        if ThingTakenSnap.Who = MySprite then
          BigMessage(_('You got the Bow!'), CAPTUREMESSAGEWAIT,
            CAPTURE_MESSAGE_COLOR)
        else
          BigMessage(WideFormat(_('%s got the Bow!'),
            [Sprite[ThingTakenSnap.Who].Player.Name]),
            CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);
      end;
    OBJECT_MEDICAL_KIT:
      begin
        PlaySound(SFX_TAKEMEDIKIT, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].Health := STARTHEALTH;
        Thing[i].Kill;
      end;
    OBJECT_GRENADE_KIT:
      begin
        PlaySound(SFX_PICKUPGUN, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].TertiaryWeapon := Guns[FRAGGRENADE];
        Sprite[ThingTakenSnap.Who].TertiaryWeapon.AmmoCount := sv_maxgrenades.Value;
        Thing[i].Kill;
      end;
    OBJECT_FLAMER_KIT:
      begin
        PlaySound(SFX_GODFLAME, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].BonusTime := FLAMERBONUSTIME;
        Sprite[ThingTakenSnap.Who].BonusStyle := BONUS_FLAMEGOD;
        Sprite[ThingTakenSnap.Who].ApplyWeaponByNum(
          Sprite[ThingTakenSnap.Who].Weapon.Num, 2, -1, True);
        Sprite[ThingTakenSnap.Who].ApplyWeaponByNum(Guns[FLAMER].Num, 1);
        if ThingTakenSnap.Who = MySprite then begin
          BigMessage(_('Flame God Mode!'), CAPTUREMESSAGEWAIT, BONUS_MESSAGE_COLOR);
          if not Sprite[MySprite].DeadMeat then
            ClientSpriteSnapshot;
        end;
        Thing[i].Kill;
      end;
    OBJECT_PREDATOR_KIT:
      begin
        PlaySound(SFX_PREDATOR, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].Alpha := PREDATORALPHA;
        Sprite[ThingTakenSnap.Who].BonusTime := PREDATORBONUSTIME;
        Sprite[ThingTakenSnap.Who].BonusStyle := BONUS_PREDATOR;
        if ThingTakenSnap.Who = MySprite then
          BigMessage(_('Predator Mode!'), CAPTUREMESSAGEWAIT, BONUS_MESSAGE_COLOR);
        Thing[i].Kill;
      end;
    OBJECT_VEST_KIT:
      begin
        PlaySound(SFX_VESTTAKE, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].Vest := DEFAULTVEST;
        if ThingTakenSnap.Who = MySprite then
          BigMessage(_('Bulletproof Vest!'), CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);
        Thing[i].Kill;
      end;
    OBJECT_BERSERK_KIT:
      begin
        PlaySound(SFX_BERSERKER, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].BonusStyle := BONUS_BERSERKER;
        Sprite[ThingTakenSnap.Who].BonusTime := BERSERKERBONUSTIME;
        if ThingTakenSnap.Who = MySprite then
          BigMessage(_('Berserker Mode!'), CAPTUREMESSAGEWAIT, BONUS_MESSAGE_COLOR);
        Thing[i].Kill;
      end;
    OBJECT_CLUSTER_KIT:
      begin
        PlaySound(SFX_PICKUPGUN, Thing[i].Skeleton.Pos[1]);
        Sprite[ThingTakenSnap.Who].TertiaryWeapon := Guns[CLUSTERGRENADE];
        Sprite[ThingTakenSnap.Who].TertiaryWeapon.AmmoCount := CLUSTER_GRENADES;
        if ThingTakenSnap.Who = MySprite then
          BigMessage(_('Cluster Grenades!'), CAPTUREMESSAGEWAIT, CAPTURE_MESSAGE_COLOR);
        Thing[i].Kill;
      end;
    OBJECT_COMBAT_KNIFE, OBJECT_CHAINSAW, OBJECT_LAW:
      begin
        // There are in total OBJECT_NUM_NONWEAPON non-weapon objects before the
        // knife so we need to subtract it+1 for the WeaponIndex (like before)
        WeaponIndex := WeaponNumToIndex(Thing[i].Style - (OBJECT_NUM_NONWEAPON + 1));
        Sprite[ThingTakenSnap.Who].ApplyWeaponByNum(Guns[WeaponIndex].Num, n, ThingTakenSnap.AmmoCount);
        if (ThingTakenSnap.Who = MySprite) and not Sprite[MySprite].DeadMeat then
          ClientSpriteSnapshot;
      end;
    OBJECT_STATIONARY_GUN:
      begin
        Thing[i].StaticType := True;
        Sprite[ThingTakenSnap.Who].Stat := i;
        PlaySound(SFX_M2USE, SpriteParts.Pos[ThingTakenSnap.Who]);
      end;
  end;

  if ((Thing[i].Style > OBJECT_POINTMATCH_FLAG) and (Thing[i].Style < OBJECT_RAMBO_BOW)) or
     ((Thing[i].Style > OBJECT_PARACHUTE) and (Thing[i].Style < OBJECT_STATIONARY_GUN)) then
  begin
    PlaySound(SFX_PICKUPGUN, Thing[i].Skeleton.Pos[1]);
    Sprite[ThingTakenSnap.Who].Weapon.FireIntervalPrev :=
      Sprite[ThingTakenSnap.Who].Weapon.FireInterval;
    Sprite[ThingTakenSnap.Who].Weapon.FireIntervalCount :=
      Sprite[ThingTakenSnap.Who].Weapon.FireInterval;
    Thing[i].Kill;
  end;
end;

end.

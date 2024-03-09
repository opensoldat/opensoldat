{*************************************************************}
{                                                             }
{       GostekGraphics Unit for OpenSoldat                    }
{                                                             }
{       Copyright (c) 2020-2023 OpenSoldat contributors       }
{                                                             }
{*************************************************************}

unit GostekGraphics;

{$hints off}
interface

uses
  // System units
  Classes,

  // Project units
  Sprites;


procedure RenderGostek(var Soldier: TSprite);
procedure LoadGostekData(Data: TStringList);
procedure ApplyGostekConstraints;


implementation

uses
  // System units
  Math,
  SysUtils,

  // Helper units
  Vector,

  // Project units
  Client,
  ClientGame,
  Constants,
  Game,
  GameRendering,
  Gfx,
  Parts,
  Weapons;


const
  {$DEFINE IDS}
  {$INCLUDE GostekGraphics.inc}
  {$UNDEF IDS}

  COLOR_NONE      = 0;
  COLOR_MAIN      = 1;
  COLOR_PANTS     = 2;
  COLOR_SKIN      = 3;
  COLOR_HAIR      = 4;
  COLOR_CYGAR     = 5;
  COLOR_HEADBLOOD = 6;

  ALPHA_BASE  = 0;
  ALPHA_BLOOD = 1;
  ALPHA_NADES = 2;

type
  TGostekSpriteSet = set of GOSTEK_FIRST..GOSTEK_LAST;

  TGostekSprite = record
    ID: string;
    Image: Word;
    p1, p2: Byte;
    cx, cy: Single;
    Flex: Single;
    Flip: Boolean;
    Team: Boolean;
    Color: Byte;
    Alpha: Byte;
  end;

var
  GostekSprites: array[GOSTEK_FIRST..GOSTEK_LAST] of TGostekSprite;
  GostekBase: TGostekSpriteSet;


procedure LoadDefaults();
var
  DefIndex: Integer;
  procedure Def(ID: string; Image: Word; p1, p2: Byte; cx, cy: Single;
    Visible, Flip, Team: Byte; Flex: Single; Color, Alpha: Byte);
  begin
    GostekSprites[DefIndex].ID    := ID;
    GostekSprites[DefIndex].Image := Image;
    GostekSprites[DefIndex].p1    := p1;
    GostekSprites[DefIndex].p2    := p2;
    GostekSprites[DefIndex].cx    := cx;
    GostekSprites[DefIndex].cy    := cy;
    GostekSprites[DefIndex].Flex  := Flex;
    GostekSprites[DefIndex].Flip  := Flip = 1;
    GostekSprites[DefIndex].Team  := Team = 1;
    GostekSprites[DefIndex].Color := Color;
    GostekSprites[DefIndex].Alpha := Alpha;

    if Visible = 1 then
      GostekBase := GostekBase + [DefIndex];

    Inc(DefIndex);
  end;
begin
  DefIndex := GOSTEK_FIRST;
  GostekBase := [];
  {$INCLUDE GostekGraphics.inc}
end;

procedure LoadGostekData(Data: TStringList);
var
  i: Integer;
  cx, cy: string;
begin
  LoadDefaults();

  for i := GOSTEK_FIRST to GOSTEK_LAST do
  begin
    cx := Data.Values[GostekSprites[i].ID + '_CenterX'];
    cy := Data.Values[GostekSprites[i].ID + '_CenterY'];
    GostekSprites[i].cx := StrToFloatDef(cx, GostekSprites[i].cx);
    GostekSprites[i].cy := StrToFloatDef(cy, GostekSprites[i].cy);
  end;
end;

procedure ApplyGostekConstraints();
var
  i, t2: Integer;
  w, h, cx, cy: Single;
  gs: ^TGostekSprite;

  function TexWidth(Index: Integer): Single;
  begin
    Result := Abs(Textures[Index].Width * Textures[Index].Scale);
  end;

  function TexHeight(Index: Integer): Single;
  begin
    Result := Abs(Textures[Index].Height * Textures[Index].Scale);
  end;
begin
  t2 := GFX_GOSTEK_TEAM2_STOPA - GFX_GOSTEK_STOPA;

  for i := GOSTEK_FIRST to GOSTEK_LAST do
  begin
    gs := @GostekSprites[i];

    if gs.Image <> 0 then
    begin
      w := TexWidth(gs.Image);
      w := Max(w, TexWidth(gs.Image + Ord(gs.Flip)));
      w := Max(w, TexWidth(gs.Image + t2 * Ord(gs.Team)));
      w := Max(w, TexWidth(gs.Image + t2 * Ord(gs.Team) + Ord(gs.Flip)));

      h := TexHeight(gs.Image);
      h := Max(h, TexHeight(gs.Image + Ord(gs.Flip)));
      h := Max(h, TexHeight(gs.Image + t2 * Ord(gs.Team)));
      h := Max(h, TexHeight(gs.Image + t2 * Ord(gs.Team) + Ord(gs.Flip)));

      cx := w * Abs(gs.cx + 0.5);
      cy := h * Abs(gs.cy + 0.5);

      if cx > (w + GOS_RESTRICT_WIDTH) then
        gs.cx := 0.5 + Sign(gs.cx + 0.5) * ((w + GOS_RESTRICT_WIDTH) / w);

      if cy > (h + GOS_RESTRICT_HEIGHT) then
        gs.cy := 0.5 + Sign(gs.cy + 0.5) * ((h + GOS_RESTRICT_HEIGHT) / h);
    end;
  end;
end;

procedure DrawGostekSprite(Sprite: PGfxSprite; x, y, sx, sy, cx, cy, r: Single;
  Color: TGfxColor);
var
  m: TGfxMat3;
  c, s, w, h, u0, v0, u1, v1: Single;
  v: array[0..3] of TGfxVertex;
  p: array[0..3] of TVector2;
begin
  c := Cos(r);
  s := Sin(r);

  m[0] := c * sx;  m[3] := -s * sy;  m[6] := x - cy * m[3] - cx * m[0];
  m[1] := s * sx;  m[4] :=  c * sy;  m[7] := y - cy * m[4] - cx * m[1];
  m[2] := 0;       m[5] := 0;        m[8] := 1;

  w := Sprite.Scale * Sprite.Width;
  h := Sprite.Scale * Sprite.Height;

  u0 := Sprite.TexCoords.Left;
  v0 := Sprite.TexCoords.Top;
  u1 := Sprite.TexCoords.Right;
  v1 := Sprite.TexCoords.Bottom;

  p[0] := GfxMat3Mul(m, 0, 0);
  p[1] := GfxMat3Mul(m, w, 0);
  p[2] := GfxMat3Mul(m, w, h);
  p[3] := GfxMat3Mul(m, 0, h);

  v[0] := GfxVertex(p[0].x, p[0].y, u0, v0, Color);
  v[1] := GfxVertex(p[1].x, p[1].y, u1, v0, Color);
  v[2] := GfxVertex(p[2].x, p[2].y, u1, v1, Color);
  v[3] := GfxVertex(p[3].x, p[3].y, u0, v1, Color);

  GfxDrawQuad(Sprite.Texture, v);
end;

procedure RenderGostek(var Soldier: TSprite);
var
  i, n, Index: Integer;
  ShowClip, Grabbed: Boolean;
  Visible: TGostekSpriteSet;
  Color: array[COLOR_NONE..COLOR_HEADBLOOD] of TGfxColor;
  Alpha: array[ALPHA_BASE..ALPHA_NADES] of Byte;
  m: TGfxMat3;
  v: TVector2;
  x1, y1, x2, y2, cx, cy, r, sx, sy: Single;
  Tex, Team2Offset: Integer;
  gs: ^TGostekSprite;
begin
  if (Soldier.Style <> 1) or
    (Soldier.CeaseFireCounter > CeaseFireTime - 5) or
    ((sv_realisticmode.Value) and (Soldier.Visible = 0)) or
    (Soldier.IsSpectator) or
    (Soldier.Player.Name = '') or
    (Soldier.Player.DemoPlayer) then
    Exit;

  Visible := GostekBase;
  m := Default(TGfxMat3);

  // setup colors

  Color[COLOR_NONE]      := RGBA($FFFFFF);
  Color[COLOR_MAIN]      := RGBA(Soldier.Player.ShirtColor);
  Color[COLOR_PANTS]     := RGBA(Soldier.Player.PantsColor);
  Color[COLOR_SKIN]      := RGBA(Soldier.Player.SkinColor);
  Color[COLOR_HAIR]      := RGBA(Soldier.Player.HairColor);
  Color[COLOR_CYGAR]     := RGBA($FFFFFF);
  Color[COLOR_HEADBLOOD] := RGBA($ACA9A8);

  if Soldier.HasCigar = 5 then
    Color[COLOR_CYGAR] := RGBA($616161);

  Alpha[ALPHA_BASE] := Soldier.Alpha;
  Alpha[ALPHA_BLOOD] := Max(0, Min(255, 200 - Round(Soldier.Health)));

  if Soldier.Health > (90 - 40 * Ord(sv_realisticmode.Value)) then
    Alpha[ALPHA_BLOOD] := 0;

  if (sv_realisticmode.Value) and (Soldier.Visible > 0) and (Soldier.Visible < 45) and
    (Soldier.Alpha > 60) then
  begin
    Soldier.Alpha := 3 * Soldier.Visible;
    Alpha[ALPHA_BASE] := Soldier.Alpha;
    Alpha[ALPHA_BLOOD] := 0;
  end;

  Alpha[ALPHA_NADES] := Trunc(0.75 * Alpha[ALPHA_BASE]);

  // blood
  if Alpha[ALPHA_BLOOD] > 0 then
  begin
    Visible := Visible + [
      GOSTEK_LEFT_THIGH_DMG, GOSTEK_LEFT_LOWERLEG_DMG,
      GOSTEK_LEFT_FOREARM_DMG, GOSTEK_LEFT_ARM_DMG,
      GOSTEK_CHEST_DMG, GOSTEK_HIP_DMG, GOSTEK_HEAD_DMG,
      GOSTEK_RIGHT_THIGH_DMG, GOSTEK_RIGHT_LOWERLEG_DMG,
      GOSTEK_RIGHT_FOREARM_DMG, GOSTEK_RIGHT_ARM_DMG
    ];
  end;

  // jets
  if (Soldier.Control.Jetpack) and (Soldier.JetsCount > 0) then
  begin
    Visible := Visible - [GOSTEK_LEFT_FOOT, GOSTEK_RIGHT_FOOT];
    Visible := Visible + [GOSTEK_LEFT_JETFOOT, GOSTEK_RIGHT_JETFOOT];
  end;

  // vest
  if Soldier.Vest > 0 then
    Include(Visible, GOSTEK_VEST);

  // grenades
  if Soldier.TertiaryWeapon.Num = Guns[FRAGGRENADE].Num then
    Index := GOSTEK_FRAG_GRENADE1
  else
    Index := GOSTEK_CLUSTER_GRENADE1;

  n := Soldier.TertiaryWeapon.AmmoCount - Ord(Soldier.BodyAnimation.ID = Throw.ID);

  for i := 0 to Min(5, n) - 1 do
    Include(Visible, Index + i);

  // chain
  case Soldier.Player.Chain of
    1: Visible := Visible +
      [GOSTEK_SILVER_LCHAIN, GOSTEK_SILVER_RCHAIN, GOSTEK_SILVER_PENDANT];
    2: Visible := Visible +
      [GOSTEK_GOLDEN_LCHAIN, GOSTEK_GOLDEN_RCHAIN, GOSTEK_GOLDEN_PENDANT];
  end;

  // cygar
  if (Soldier.HasCigar = 5) or (Soldier.HasCigar = 10) then
    Include(Visible, GOSTEK_CIGAR);

  // head & hair
  if Soldier.DeadMeat then
  begin
    Visible := Visible - [GOSTEK_HEAD, GOSTEK_HEAD_DMG];
    Visible := Visible + [GOSTEK_HEAD_DEAD, GOSTEK_HEAD_DEAD_DMG];
  end;

  if Soldier.Weapon.Num in [Guns[BOW].Num, Guns[BOW2].Num] then
  begin
    Include(Visible, GOSTEK_RAMBO_BADGE);
  end
  else
  begin
    Grabbed := (Soldier.BodyAnimation.ID in [Wipe.ID, TakeOff.ID]) and
      (Soldier.BodyAnimation.CurrFrame > 4);

    if Soldier.WearHelmet = 1 then
    begin
      case Soldier.Player.HeadCap of
        GFX_GOSTEK_HELM: case Grabbed of
          True:  Include(Visible, GOSTEK_GRABBED_HELMET);
          False: Include(Visible, GOSTEK_HELMET);
        end;
        GFX_GOSTEK_KAP: case Grabbed of
          True:  Include(Visible, GOSTEK_GRABBED_HAT);
          False: Include(Visible, GOSTEK_HAT);
        end;
      end;
    end;

    if Grabbed or (Soldier.WearHelmet <> 1) or (Soldier.Player.HairStyle = 3) then
    begin
      case Soldier.Player.HairStyle of
        1: for i := 0 to 5 do
          Include(Visible, GOSTEK_HAIR_DREADLOCKS + i);
        2: Include(Visible, GOSTEK_HAIR_PUNK);
        3: Include(Visible, GOSTEK_MR_T);
        4: Include(Visible, GOSTEK_HAIR_NORMAL);
      end;
    end;
  end;

  // secondary weapon (on the back)

  Index := WeaponNumToIndex(Soldier.SecondaryWeapon.Num);

  if (Index >= EAGLE) and (Index <= FLAMER) then
    Include(Visible, GOSTEK_SECONDARY_FIRST + Index - EAGLE);

  // primary weapon

  if Soldier.Weapon.Num = Guns[MINIGUN].Num then
  begin
    Include(Visible, GOSTEK_PRIMARY_MINIGUN);

    ShowClip := (Soldier.Weapon.AmmoCount > 0) or ((Soldier.Weapon.AmmoCount = 0) and
      (Soldier.Weapon.ReloadTimeCount < 65));

    if ShowClip then
      Include(Visible, GOSTEK_PRIMARY_MINIGUN_CLIP);

    if Soldier.Fired > 0 then
      Include(Visible, GOSTEK_PRIMARY_MINIGUN_FIRE);
  end
  else if Soldier.Weapon.Num in [Guns[BOW].Num, Guns[BOW2].Num] then
  begin
    if Soldier.Weapon.AmmoCount = 0 then
      Include(Visible, GOSTEK_PRIMARY_BOW_ARROW_RELOAD)
    else
      Include(Visible, GOSTEK_PRIMARY_BOW_ARROW);

    if Soldier.BodyAnimation.ID = ReloadBow.ID then
      Visible := Visible + [GOSTEK_PRIMARY_BOW_RELOAD,
        GOSTEK_PRIMARY_BOW_STRING_RELOAD]
    else
      Visible := Visible + [GOSTEK_PRIMARY_BOW,
        GOSTEK_PRIMARY_BOW_STRING];

    if Soldier.Fired > 0 then
      Include(Visible, GOSTEK_PRIMARY_BOW_FIRE);
  end
  else if not Soldier.DeadMeat then
  begin
    Index := WeaponNumToIndex(Soldier.Weapon.Num);

    if (Index >= EAGLE) and (Index <= FLAMER) then
    begin
      if Index = FLAMER then
        Index := GOSTEK_PRIMARY_FLAMER - GOSTEK_PRIMARY_FIRST
      else
        Index := 3 * (Index - EAGLE);

      Include(Visible, GOSTEK_PRIMARY_FIRST + Index);

      ShowClip := (Soldier.Weapon.ClipTextureNum > 0) and
        ((Soldier.Weapon.AmmoCount > 0) or ((Soldier.Weapon.AmmoCount = 0) and
        ((Soldier.Weapon.ReloadTimeCount < Soldier.Weapon.ClipInTime) or
        (Soldier.Weapon.ReloadTimeCount > Soldier.Weapon.ClipOutTime))));

      if ShowClip then
        Include(Visible, GOSTEK_PRIMARY_FIRST + Index + 1);

      if Soldier.Fired > 0 then
        Include(Visible, GOSTEK_PRIMARY_FIRST + Index + 2);
    end;
  end;

  // draw

  Team2Offset := 0;

  if Soldier.Player.Team in [TEAM_BRAVO, TEAM_DELTA] then
    Team2Offset := GFX_GOSTEK_TEAM2_STOPA - GFX_GOSTEK_STOPA;

  if GOSTEK_HAIR_DREADLOCKS in Visible then
  begin
    x1 := Soldier.Skeleton.Pos[GostekSprites[GOSTEK_HEAD].p1].x;
    y1 := Soldier.Skeleton.Pos[GostekSprites[GOSTEK_HEAD].p1].y;
    x2 := Soldier.Skeleton.Pos[GostekSprites[GOSTEK_HEAD].p2].x;
    y2 := Soldier.Skeleton.Pos[GostekSprites[GOSTEK_HEAD].p2].y;
    r := ArcTan2(y2 - y1, x2 - x1) - Pi / 2;
    m := GfxMat3Rot(r);
  end;

  for i := GOSTEK_FIRST to GOSTEK_LAST do
  begin
    gs := @GostekSprites[i];

    if (i in Visible) and (gs.Image <> 0) then
    begin
      Tex := gs.Image;

      if gs.Team then
        Tex := Tex + Team2Offset;

      x1 := Soldier.Skeleton.Pos[gs.p1].x;
      y1 := Soldier.Skeleton.Pos[gs.p1].y;
      x2 := Soldier.Skeleton.Pos[gs.p2].x;
      y2 := Soldier.Skeleton.Pos[gs.p2].y;
      r := ArcTan2(y2 - y1, x2 - x1);
      cx := gs.cx;
      cy := gs.cy;
      sx := 1;
      sy := 1;

      if Soldier.Direction <> 1 then
      begin
        if gs.Flip then
        begin
          cy := 1 - gs.cy;
          Tex := Tex + 1;
        end
        else
          sy := -1;
      end;

      cx := cx * Textures[Tex].Width * Textures[Tex].Scale;
      cy := cy * Textures[Tex].Height * Textures[Tex].Scale;
      Color[gs.Color].a := Alpha[gs.Alpha];

      if (i >= GOSTEK_HAIR_DREADLOCK1) and (i <= GOSTEK_HAIR_DREADLOCK5) then
      begin
        v := GfxMat3Mul(m, -cy * Soldier.Direction, cx);
        x1 := x1 + v.x;
        y1 := y1 + v.y;
        cx := 0;
        cy := 0.5 * Textures[Tex].Height * Textures[Tex].Scale;
        sx := 0.75 + (1 - 0.75) / 5 * (i - GOSTEK_HAIR_DREADLOCK1);
      end
      else if gs.Flex > 0 then
        sx := Min(1.5, Sqrt(Sqr(x2 - x1) + Sqr(y2 - y1)) / gs.Flex);

      DrawGostekSprite(Textures[Tex], x1, y1 + 1, sx, sy, cx, cy, r, Color[gs.Color]);
    end;
  end;
end;

initialization
  LoadDefaults();

end.

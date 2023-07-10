unit Sparks;

interface

uses
  // Helper units
  Vector,

  // Project units
  Constants,
  MapFile,
  Net,
  Parts,
  PolyMap,
  Sprites;

type TSpark = object
    Active: Boolean;
    Num: SmallInt;
    LifeReal: Single;
    Life, LifePrev: Byte;
    Style, Owner: Byte;
    CollideCount: Byte;
  public
    procedure Update;
    procedure Render;
    function CheckMapCollision(X, Y: Single): Boolean;
    procedure Kill;
    procedure CheckOutOfBounds;
  end;
  function CreateSpark(sPos, sVelocity: TVector2; sStyle, sOwner: Byte;
    Life: Integer): Integer;

var
  SparksCount: Integer;


implementation

uses
  // System units
  Math,
  SysUtils,

  // Helper units
  Util,

  // Project units
  Client,
  Demo,
  Game,
  GameRendering,
  Gfx,
  Sound;


function CreateSpark(sPos, sVelocity: TVector2; sStyle, sOwner: Byte; Life: Integer): Integer;
var
  i: Integer;
  M: Single;
begin
  Result := 0;

  if CameraFollowSprite > 0 then
  begin
    if CameraFollowSprite = MySprite then
      if not PointVisible(sPos.X, sPos.Y, CameraFollowSprite) and (sStyle <> 38) then
      begin
        Result := 0;
        Exit;
      end;

    if CameraFollowSprite <> MySprite then
      if not PointVisible2(sPos.X, sPos.Y, CameraFollowSprite) and (sStyle <> 38) then
      begin
        Result := 0;
        Exit;
      end;
  end;

  for i := 1 to r_maxsparks.Value + 1 do
  begin
    if (SparksCount > r_maxsparks.Value - 50) and ((sStyle = 3) or (sStyle = 4) or
        (sStyle = 26) or (sStyle = 27) or (sStyle = 59) or (sStyle = 2)) then
      Exit;
    if (SparksCount > r_maxsparks.Value - 40) and (sStyle = 1) then
      Exit;
    if (SparksCount > r_maxsparks.Value - 30) and (sStyle = 24) then
      Exit;

    if i = r_maxsparks.Value then
    begin
      Result := Random(r_maxsparks.Value div 3) + 1;
      Break;
    end;
    if not Spark[i].Active and (Spark[i].Style = 0) and
        not SparkParts.Active[i] then
    begin
      Result := i;
      Break;
    end;
  end;
  // i is now the active sprite
  i := Result;

  // activate sprite
  Spark[i].Active := True;
  Spark[i].Life := Life;
  Spark[i].Style := sStyle;
  Spark[i].Num := i;
  Spark[i].Owner := sOwner;
  Spark[i].CollideCount := 0;

  M := 1;

  // activate sprite part
  SparkParts.CreatePart(sPos, sVelocity, M, i);

  Result := i;
end;

// TSPARK
procedure TSpark.Update;
const
  NONEULER_STYLE = [12, 13, 14, 15, 17, 24, 25, 28, 29, 31, 36, 37, 50, 54, 56, 60];

  COLLIDABLE_STYLE = [2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 16, 18, 19, 20, 21, 22,
    23, 30, 32, 33, 34, 40, 41, 42, 43, 48, 49, 51, 52, 57, 62, 64, 65, 66, 67,
    68, 69, 70, 71, 72, 73];
var
  Wobble, WobbleX, WobbleY: Integer;
begin
  if not (Style in NONEULER_STYLE) then
    SparkParts.DoEulerTimeStepFor(Num);

  CheckOutOfBounds;

  // check collision with map
  if Style in COLLIDABLE_STYLE then
    CheckMapCollision(SparkParts.Pos[Num].X, SparkParts.Pos[Num].Y);

  // wobble the screen when explosion
  if (MySprite > 0) and (CameraFollowSprite > 0) and (not DemoPlayer.Active) then
    if (Style = 17) or (Style = 12) or (Style = 14) or (Style = 15) or (Style = 28) then
      if PointVisible(Sparkparts.Pos[Num].X, Sparkparts.Pos[Num].Y, CameraFollowSprite) then
        if Life > EXPLOSION_ANIMS * 2.3 then
          // if ((Style = 17) and (Life > EXPLOSION_ANIMS * 2.5)) or
            // ((Style <> 17) and (Life > EXPLOSION_ANIMS * 2.4)) then
        begin
          Wobble := Life div 6;
          WobbleX := Random(2 * Wobble + 1);
          WobbleY := Random(2 * Wobble);
          CameraX := CameraX - Wobble + WobbleX;
          CameraY := CameraY - Wobble + WobbleY;
        end;

  // smoke luska
  if (r_maxsparks.Value > (MAX_SPARKS - 10)) and (Style > 64) and  (Life > 235) and
     (Random(32) = 0) then
    CreateSpark(Sparkparts.Pos[Num], Sparkparts.Velocity[Num], 31, Owner, 40);

  // smoke m79 luska
  if (r_maxsparks.Value > (MAX_SPARKS - 10)) and (Style = 52) then
  begin
    if (Life > 235) and (Random(6) = 0) then
      CreateSpark(Sparkparts.Pos[Num], Sparkparts.Velocity[Num], 31, Owner, 40);

    if (Life > 85) and (Life < 235) and (Random(15) = 0) then
      CreateSpark(Sparkparts.Pos[Num], Sparkparts.Velocity[Num], 31, Owner, 35);

    if (Life < 85) and (Random(24) = 0) then
      CreateSpark(Sparkparts.Pos[Num], Sparkparts.Velocity[Num], 31, Owner, 30);
  end;

  // iskry
  if (r_maxsparks.Value > (MAX_SPARKS - 10)) and (Style = 2) and (Random(8) = 0) then
    CreateSpark(Sparkparts.Pos[Num], Vector2(0, 0), 26, Owner, 35);

  LifePrev := Life;
  Life := Life - 1;
  if Life = 0 then
    Kill;
end;

procedure TSpark.Render;
var
  T: ^TGfxSpriteArray;
  _p: TVector2;
  _Scala: TVector2;
  grenvel: Single = 0.0;
  l: Single;
  i: Integer;
begin
  T := @Textures;
  if sv_realisticmode.Value then
    if (Owner > 0) and (Owner < MAX_SPRITES + 1) then
      if Sprite[Owner].Active then
        if Sprite[Owner].Visible = 0 then
          if Map.RayCast(SparkParts.Pos[Num], Sprite[MySprite].Skeleton.Pos[9],
            grenvel, GameWidth, True) or (Sprite[Owner].Visible = 0) then
            Exit;

  _p.x := SparkParts.Pos[Num].X;
  _p.y := SparkParts.Pos[Num].Y;
  l := LifeReal;

  case Style of
    1: GfxDrawSprite(T^[GFX_SPARKS_SMOKE], _p.x, _p.y, RGBA($FFFFFF, l + 10));
    2: GfxDrawSprite(T^[GFX_SPARKS_LILFIRE], _p.x, _p.y, RGBA($FFFFFF, l));
    3: GfxDrawSprite(T^[GFX_SPARKS_ODPRYSK], _p.x, _p.y, RGBA($FFFFFF, l * 3 + 10));
    4: GfxDrawSprite(T^[GFX_SPARKS_LILBLOOD], _p.x, _p.y, 0.75, 0.75, 0, 0,
      degtorad(l * 10), RGBA($FFFFFF, l * 2 + 65));
    5:
      begin
        _scala.x := iif(l > 10, 0.33 + 10 / l, 1);
        _scala.y := _scala.x;
        GfxDrawSprite(T^[GFX_SPARKS_BLOOD], _p.x, _p.y, _scala.x, _scala.y, 0, 0,
          degtorad(l * 2), RGBA($FFFFFF, l * 2 + 85));
      end;
    6:
      if Sprite[Owner].Player.HeadCap > 0 then
        GfxDrawSprite(T^[Sprite[Owner].Player.HeadCap], _p.x, _p.y, 0, 0,
          degtorad(l * 2), RGBA(Sprite[Owner].Player.ShirtColor));
    7: GfxDrawSprite(T^[GFX_WEAPONS_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    8: GfxDrawSprite(T^[GFX_WEAPONS_SHELL], _p.x, _p.y, 1.1, 1.2, 0, 0,
      degtorad(l * 3.5), RGBA($FF3333));
    9:  GfxDrawSprite(T^[GFX_WEAPONS_AK74_CLIP], _p.x + 8, _p.y, 0, 0, Pi);
    10: GfxDrawSprite(T^[GFX_WEAPONS_MINIMI_CLIP], _p.x + 8, _p.y, 0, 0, Pi);
    11: GfxDrawSprite(T^[GFX_WEAPONS_MP5_CLIP], _p.x + 8, _p.y, 0, 0, Pi);
    12:
      begin
        _p.x := _p.x - 19;
        _p.y := _p.y - 38;

        i := GFX_SPARKS_EXPLOSION_EXPLODE16 - Round(l / 4);

        if (i - 1) >= GFX_SPARKS_EXPLOSION_EXPLODE1 then
          GfxDrawSprite(T^[i - 1], _p.x, _p.y, 0.75, RGBA($ADADAD, 100));

        GfxDrawSprite(T^[i], _p.x, _p.y, 0.75,
          RGBA($FFFFFF, 255 - (EXPLOSION_ANIMS * 5) + l));
      end;
    13:
      begin
        _p.x := _p.x - 8;
        _p.y := _p.y - 17;
        GfxDrawSprite(T^[GFX_SPARKS_EXPLOSION_EXPLODE16 - Round(l / 3)],
          _p.x, _p.y, 0.3, RGBA($FFFFFF, 255 - 2 * l));
      end;
    14:
      begin
        _p.x := _p.x - 50;
        _p.y := _p.y - 100;

        i := GFX_SPARKS_EXPLOSION_EXPLODE16 - Round(l / 3);

        if (i - 1) >= GFX_SPARKS_EXPLOSION_EXPLODE1 then
          GfxDrawSprite(T^[i - 1], _p.x, _p.y, 2, RGBA($ADADAD, 100));

        GfxDrawSprite(T^[i], _p.x, _p.y, 2, RGBA($FFFFFF, 255 - 2 * l));
      end;
    15:
      begin
        _p.x := _p.x - 75;
        _p.y := _p.y - 150;

        i := GFX_SPARKS_EXPLOSION_EXPLODE16 - Round(l / 3);

        if (i - 1) >= GFX_SPARKS_EXPLOSION_EXPLODE1 then
          GfxDrawSprite(T^[i - 1], _p.x, _p.y, 3, RGBA($ADADAD, 100));

        GfxDrawSprite(T^[i], _p.x, _p.y, 3, RGBA($FFFFFF, 255 - 2 * l));
      end;
    16: GfxDrawSprite(T^[GFX_WEAPONS_SHELL], _p.x, _p.y, 1, 2, 0, 0,
      degtorad(l * 3.5), RGBA($8877FF));
    17:
      begin
        _p.x := _p.x - 25;
        _p.y := _p.y - 50;

        i := GFX_SPARKS_EXPLOSION_EXPLODE16 - Round(l / 4);

        if (i - 1) >= GFX_SPARKS_EXPLOSION_EXPLODE1 then
          GfxDrawSprite(T^[i - 1], _p.x, _p.y, RGBA($AAAAAA, 100));

        GfxDrawSprite(T^[i], _p.x, _p.y, RGBA($FFFFFF,
          255 - (EXPLOSION_ANIMS * 5) + l));
      end;
    18: GfxDrawSprite(T^[GFX_WEAPONS_DEAGLES_CLIP], _p.x + 8, _p.y, 0, 0, Pi);
    19: GfxDrawSprite(T^[GFX_WEAPONS_STEYR_CLIP], _p.x + 8, _p.y, 0, 0, Pi);
    20: GfxDrawSprite(T^[GFX_WEAPONS_BARRETT_CLIP], _p.x + 8, _p.y, 0, 0, Pi);
    21: GfxDrawSprite(T^[GFX_WEAPONS_SHELL], _p.x, _p.y, 1.1, 1, 0, 0,
      degtorad(l * 3.77));
    22: GfxDrawSprite(T^[GFX_WEAPONS_SHELL], _p.x, _p.y, 1.3, 1, 0, 0,
      degtorad(l * 3.5));
    23: GfxDrawSprite(T^[GFX_WEAPONS_SOCOM_CLIP], _p.x + 8, _p.y, 0, 0, Pi);
    24:
      begin
        _scala.x := 0.6 + (75 / l) / 126;
        _scala.y := 0.6 + (75 / l) / 120;
        _p.x := _p.x - 22 * _scala.x;
        _p.y := _p.y - 64 + l / 2;
        GfxDrawSprite(T^[GFX_SPARKS_BIGSMOKE], _p.x, _p.y, _scala.x, _scala.y,
          RGBA($FFFFFF, Trunc(3 * l)));
      end;
    25: GfxDrawSprite(T^[GFX_SPARKS_SPAWNSPARK], _p.x - 20, _p.y - 20, 0, 0,
      degtorad(l), RGBA(Sprite[Owner].Player.ShirtColor, min(6 * l, 255.0)));
    26: GfxDrawSprite(T^[GFX_SPARKS_ODPRYSK], _p.x, _p.y, RGBA($FFFE35, min(l * 3 + 154, 255.0)));
    27: GfxDrawSprite(T^[GFX_SPARKS_ODPRYSK], _p.x, _p.y, RGBA($AAAAAA, min(l * 3 + 154, 255.0)));
    28: GfxDrawSprite(T^[GFX_SPARKS_EXPLOSION_EXPLODE16 - Round(l / 3)],
      _p.x - 15, _p.y - 37, 0.5, RGBA($FFFFFF, 255 - 2 * l));
    29:
      begin
        _scala.x := 0.5 * (0.6 + (75 / l) / 96);
        _scala.y := 0.5 * (0.6 + (75 / l) / 90);
        _p.x := _p.x - 22 * _scala.x;
        _p.y := _p.y - 48 + l / 1.5;
        GfxDrawSprite(T^[GFX_SPARKS_BIGSMOKE], _p.x, _p.y, _scala.x, _scala.y,
          RGBA($FFFFFF, Trunc(2.5 * l)));
      end;
    30: GfxDrawSprite(T^[GFX_SPARKS_PIN], _p.x, _p.y, 0, 0, degtorad(l * 4));
    31: GfxDrawSprite(T^[GFX_SPARKS_LILSMOKE], _p.x, _p.y, RGBA($FFFFFF, l + 10));
    32: GfxDrawSprite(T^[GFX_SPARKS_STUFF], _p.x, _p.y, RGBA($FFFFFF, l + 10));
    33: GfxDrawSprite(T^[GFX_WEAPONS_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4),
      RGBA($BBAAA9));
    34: GfxDrawSprite(T^[GFX_SPARKS_CYGARO], _p.x, _p.y, 0, 0, degtorad(l * 4));
    35: GfxDrawSprite(T^[GFX_SPARKS_LILSMOKE], _p.x, _p.y, RGBA($FFFFFF, l * 13));
    36:
      begin
        _Scala.X := l / 35;
        _Scala.Y := l / 35;
        _p.y := _p.y - (1 / _Scala.Y);
        GfxDrawSprite(T^[GFX_SPARKS_PLOMYK], _p.x, _p.y, _Scala.x, _Scala.y,
          RGBA($FFFFFF, min(l * 2 + 185, 255.0)));
      end;
    37:
      begin
        _Scala.X := l / 75;
        _Scala.Y := l / 75;
        _p.y := _p.y - (1 / _Scala.Y);
        GfxDrawSprite(T^[GFX_SPARKS_BLACKSMOKE], _p.x, _p.y, _Scala.x, _Scala.y,
          RGBA($FFFFFF, l * 3));
      end;
    38: GfxDrawSprite(T^[GFX_SPARKS_RAIN], _p.x, _p.y, RGBA($FFFFFF, 105));
    39: GfxDrawSprite(T^[GFX_SPARKS_SAND], _p.x, _p.y, RGBA($FFFFFF, 105));
    40: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK1], _p.x, _p.y, 0, 0, degtorad(l * 8),
      RGBA($FFFFFF, Trunc(l + 10)));
    41: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK2], _p.x, _p.y, 0, 0, degtorad(l * 8),
      RGBA($FFFFFF, Trunc(l + 10)));
    42: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK3], _p.x, _p.y, 0, 0, degtorad(l * 8),
      RGBA($FFFFFF, Trunc(l + 10)));
    43: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK4], _p.x, _p.y, 0, 0, degtorad(l * 8),
      RGBA($FFFFFF, Trunc(l + 10)));
    44: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK1], _p.x, _p.y, 0.7,
      RGBA($FFFFFF, Trunc(l * 2) + 15));
    45: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK2], _p.x, _p.y, 0.7,
      RGBA($FFFFFF, Trunc(l * 2) + 15));
    46: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK3], _p.x, _p.y, 0.7,
      RGBA($FFFFFF, Trunc(l * 2) + 15));
    47: GfxDrawSprite(T^[GFX_SPARKS_ODLAMEK4], _p.x, _p.y, 0.7,
      RGBA($FFFFFF, Trunc(l * 2) + 15));
    48: GfxDrawSprite(T^[GFX_SPARKS_SKRAWEK],_p.x, _p.y, 0, 0, degtorad(l * 5),
      RGBA(Sprite[Owner].Player.ShirtColor, Trunc(l * 2) + 15));
    49: GfxDrawSprite(T^[GFX_SPARKS_SKRAWEK], _p.x, _p.y, 0, 0, degtorad(l * 5),
      RGBA(Sprite[Owner].Player.PantsColor, Trunc(l * 2) + 15));
    50: GfxDrawSprite(T^[GFX_SPARKS_PUFF], _p.x, _p.y, RGBA($FFFFFF, Trunc(l) + 5));
    51: GfxDrawSprite(T^[GFX_WEAPONS_SPAS_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 3.77));
    52: GfxDrawSprite(T^[GFX_WEAPONS_M79_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 3.77));
    53: GfxDrawSprite(T^[GFX_SPARKS_SNOW], _p.x, _p.y, RGBA($FFFFFF, 105));
    54:
      if l <= SMOKE_ANIMS * 4 then
      begin
        _p.x := _p.x - 26;
        _p.y := _p.y - 48;

        i := GFX_SPARKS_MINISMOKE - Round(l / 4);

        if (i - 1) >= GFX_SPARKS_EXPLOSION_SMOKE1 then
          GfxDrawSprite(T^[i - 1], _p.x, _p.y, RGBA($CCCCCC, 2 * l + 10));

        GfxDrawSprite(T^[i], _p.x, _p.y, RGBA($DDDDDD, 3 * l + 10));
      end;
    55:
      begin
        _scala.x := iif(l > 20, 0.63 + 10 / l, 1);
        _scala.y := _scala.x;
        GfxDrawSprite(T^[GFX_SPARKS_SPLAT], _p.x, _p.y, _scala.x, _scala.y, 0, 0,
          degtorad(l), RGBA($FFFFFF, min(l * 2 + 55, 255.0)));
      end;
    56: GfxDrawSprite(T^[GFX_SPARKS_MINISMOKE], _p.x - 3, _p.y - 3,
      RGBA($FFFFFF, Trunc(2.5 * l)));
    57: GfxDrawSprite(T^[GFX_SPARKS_ODPRYSK], _p.x, _p.y, RGBA($FFFF00, l * 2 + 10));
    58: GfxDrawSprite(T^[GFX_SPARKS_ODPRYSK], _p.x, _p.y, RGBA($FFFF00, l * 3 + 10));
    59:
      begin
        _p.y := _p.y - (50 - l);
        _scala.x := 1.5 + (620 / l) / 50;
        _scala.y := _scala.x;
        GfxDrawSprite(T^[GFX_SPARKS_SMOKE], _p.x, _p.y, _scala.x, _scala.y,
          RGBA($FFFFFF, l * 2));
      end;
    60:
      begin
        _scala.x := 0.5 + (16 / (l + 50));
        _scala.y := _scala.x;
        _p.x := _p.x - 14 * _scala.x;
        _p.y := _p.y - 30;
        GfxDrawSprite(T^[GFX_SPARKS_BIGSMOKE], _p.x, _p.y, _scala.x, _scala.y,
          RGBA($FFFFFF, Trunc(l / 3.3)));

        if l > 30 then
          GfxDrawSprite(T^[GFX_SPARKS_BIGSMOKE2], _p.x, _p.y, _scala.x, _scala.y,
            RGBA($666666, Trunc((255 - l) / 9)))
        else
          GfxDrawSprite(T^[GFX_SPARKS_BIGSMOKE2], _p.x, _p.y, _scala.x, _scala.y,
            RGBA($BFBFBF, Trunc(l * 1.0)));
      end;
    61: GfxDrawSprite(T^[GFX_SPARKS_SPAWNSPARK], _p.x - 10, _p.y - 10, 0.5, 0.5,
      0, 0, degtorad(l), RGBA(Sprite[Owner].Player.ShirtColor, 14 * l));
    62: GfxDrawSprite(T^[GFX_SPARKS_JETFIRE], _p.x, _p.y, 0, 0, degtorad(l),
      RGBA(Sprite[Owner].Player.JetColor, l * 5));
    64:
      begin
        _scala.x := l / 35;
        _scala.y := l / 35;
        _p.y := _p.y - (1 / _scala.y);
        GfxDrawSprite(T^[GFX_SPARKS_PLOMYK], _p.x, _p.y, _scala.x, _scala.y,
          RGBA($FFFFFF, l * 2 + 185));
      end;
    65: GfxDrawSprite(T^[GFX_WEAPONS_COLT_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    66: GfxDrawSprite(T^[GFX_WEAPONS_DEAGLES_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    67: GfxDrawSprite(T^[GFX_WEAPONS_MP5_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    68: GfxDrawSprite(T^[GFX_WEAPONS_AK74_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    69: GfxDrawSprite(T^[GFX_WEAPONS_STEYR_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    70: GfxDrawSprite(T^[GFX_WEAPONS_RUGER_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    71: GfxDrawSprite(T^[GFX_WEAPONS_BARRETT_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 3.5));
    72: GfxDrawSprite(T^[GFX_WEAPONS_MINIMI_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
    73: GfxDrawSprite(T^[GFX_WEAPONS_MINIGUN_SHELL], _p.x, _p.y, 0, 0, degtorad(l * 4));
  end;  // case
end;

function TSpark.CheckMapCollision(X, Y: Single): Boolean;
var
  j, w: Integer;
  b: Integer = 0;
  Pos, Perp: TVector2;
  D: Single = 0.0;
  kx, ky: Integer;
  teamcol: Boolean;
begin
  Result := False;

  Pos.X := X - 8;
  Pos.Y := Y - 1;

  {iterate through maps sector polygons}
  kx := Round(Pos.X / Map.SectorsDivision);
  ky := Round(Pos.Y / Map.SectorsDivision);
  if (kx < -Map.SectorsNum) or (kx > Map.SectorsNum) or
    (ky < -Map.SectorsNum) or (ky > Map.SectorsNum) then
    Exit;

  if High(Map.Sectors[kx, ky].Polys) > 0 then
    for j := 1 to High(Map.Sectors[kx, ky].Polys) do
    begin
      w := Map.Sectors[kx, ky].Polys[j];

      if (Owner < 1) or (Owner > 32) then Exit;

      teamcol := TeamCollides(w, Sprite[Owner].Player.Team, False);

      if teamcol then
        if not ((Map.PolyType[w] = POLY_TYPE_BOUNCY) and (Sprite[Owner].HoldedThing = 0)) then
          if (Map.PolyType[w] <> POLY_TYPE_ONLY_BULLETS) and
            (Map.PolyType[w] <> POLY_TYPE_ONLY_PLAYER) and
            (Map.PolyType[w] <> POLY_TYPE_DOESNT) and
            (Map.PolyType[w] <> POLY_TYPE_BACKGROUND) and
            (Map.PolyType[w] <> POLY_TYPE_BACKGROUND_TRANSITION) then
            if Map.PointInPolyEdges(Pos.X, Pos.y, w) then
            begin
              Perp := Map.ClosestPerpendicular(w, Pos, D, b);

              Vec2Normalize(Perp, Perp);
              Vec2Scale(Perp, Perp, D);

              SparkParts.Velocity[Num] := Vec2Subtract(SparkParts.Velocity[Num], Perp);

              Vec2Scale(SparkParts.Velocity[Num], SparkParts.Velocity[Num], SPARK_SURFACECOEF);

              case Style of
                2, 62:
                  begin
                    Vec2Scale(Perp, Perp, 2.5);
                    Perp.X := Perp.X - 0.5 + Random(11) / 10;
                    Perp.Y := -Perp.Y;
                    if Random(2) = 0 then
                    begin
                      if Random(2) = 0 then
                        CreateSpark(Pos, Perp, 26, Owner, 35) else
                        CreateSpark(Pos, Perp, 27, Owner, 35);

                      PlaySound(SFX_TS, SparkParts.Pos[Num]);
                    end;
                  end;
                33, 34:
                  begin
                    Vec2Scale(Perp, Perp, 2.5);
                    Perp.X := Perp.X - 0.5 + Random(11) / 10;
                    Perp.Y := -Perp.Y;
                    if Random(7) = 0 then
                      CreateSpark(Pos, Perp, 26, Owner, 35) else
                      CreateSpark(Pos, Perp, 27, Owner, 35);

                    if CollideCount > 4 then
                      Kill;
                  end;
                4, 5:
                  begin
                    if Style = 5 then
                      CreateSpark(Sparkparts.Pos[Num], Sparkparts.Velocity[Num], 55, Owner, 30);

                    if CollideCount > 1 then
                      Kill;
                  end;
                6:
                  begin
                    if (CollideCount = 0) or (CollideCount = 2) or (CollideCount = 4) then
                      PlaySound(SFX_CLIPFALL, SparkParts.Pos[Num]);

                    if CollideCount > 4 then
                      Kill;
                  end;
                7, 21, 22, 16, 30, 52, 65, 66, 67, 68, 69, 70, 71, 72, 73:
                  begin
                    if (CollideCount = 0) or (CollideCount = 2) or (CollideCount = 4) then
                      PlaySound(SFX_SHELL + Random(2), SparkParts.Pos[Num]);
                    if CollideCount > 4 then Kill;
                  end;
                51:
                  begin
                    PlaySound(SFX_GAUGESHELL, SparkParts.Pos[Num]);
                    if CollideCount > 4 then Kill;
                  end;
                32, 48, 49:
                  begin
                    if CollideCount > 2 then
                      Kill;
                  end;
                9, 10, 11, 18, 19, 20, 23:
                  begin
                    if (CollideCount = 0) or (CollideCount = 4) then
                      PlaySound(SFX_CLIPFALL, SparkParts.Pos[Num]);

                    if CollideCount > 4 then Kill;
                  end;
                57:
                  begin
                    Vec2Scale(Perp, Perp, 0.75);
                    Perp.X := Perp.X - 0.5 + Random(11) / 10;
                    Perp.Y := -Perp.Y;
                    if Random(2) = 0 then
                      CreateSpark(Pos, Perp, 58, Owner, 50) else
                      CreateSpark(Pos, Perp, 58, Owner, 50);
                  end;
              end;

              Inc(CollideCount);

              Result := True;
              Exit;
            end;  // PointinPolyEdges
    end;  // for j
end;

procedure TSpark.Kill;
begin
  Active := False;
  Style := 0;
  if Num > 0 then
    SparkParts.Active[Num] := False;
end;

procedure TSpark.CheckOutOfBounds;
var
  Bound: Integer;
  SparkPartsPos: ^TVector2;
begin
  Bound := Map.SectorsNum * Map.SectorsDivision - 10;
  SparkPartsPos := @SparkParts.Pos[Num];

  if (Abs(SparkPartsPos.X) > Bound) or
     (Abs(SparkPartsPos.Y) > Bound) then
    Kill;
end;

end.

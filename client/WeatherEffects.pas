{*******************************************************}
{                                                       }
{       WeatherEffects Unit for OPENSOLDAT              }
{                                                       }
{       Copyright (c) 2003 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit WeatherEffects;

interface

uses
  Game, Weapons, Vector, Sprites, Net, Sparks;

procedure MakeRain;
procedure MakeSandStorm;
procedure MakeSnow;

implementation

uses
  Client, Cvar, Sound, Constants;

// Backgorund animation and sound for rain
procedure MakeRain;
var
  i: Integer;
  a, b: TVector2;
  Modder: Integer;
begin
  if r_maxsparks.Value < (MAX_SPARKS - 10) then
    Modder := 34
  else
    Modder := 17;

  if MainTickCounter mod Modder = 0 then
  begin
    a.x := CameraX - GameWidthHalf - 128;

    b.x := 0;
    b.y := 12;

    for i := 1 to 8 do
    begin
      a.x := a.x + 128 - 50 + Random(90);
      a.y := CameraY - GameHeightHalf - 128 - 60 + Random(150);

      CreateSpark(a, b, 38, 255, 60);
    end;
  end;

  PlaySound(SFX_WIND, CHANNEL_WEATHER);
end;

// Backgorund animation and sound for sandstorm
procedure MakeSandStorm;
var
  i: Integer;
  a, b: TVector2;
  Modder: Integer;
begin
  if r_maxsparks.Value < (MAX_SPARKS - 10) then
    Modder := 34
  else
    Modder := 17;

  if MainTickCounter mod Modder = 0 then
  begin
    a.x := CameraX - GameWidthHalf - 1.5 * 512;

    b.x := 10;
    b.y := 7;

    for i := 1 to 8 do
    begin
      a.x := a.x + 128 - 50 + Random(90);
      a.y := CameraY - GameHeightHalf - 256 - 60 + Random(150);

      CreateSpark(a, b, 39, 255, 80);
    end;
  end;

  PlaySound(SFX_WIND, CHANNEL_WEATHER);
end;

// Backgorund animation and sound for snow
procedure MakeSnow;
var
  i: Integer;
  a, b: TVector2;
  Modder: Integer;
begin
  if r_maxsparks.Value < (MAX_SPARKS - 10) then
    Modder := 34
  else
    Modder := 17;

  if MainTickCounter mod Modder = 0 then
  begin
    a.x := CameraX - GameWidthHalf - 256;

    b.x := 1;
    b.y := 2;

    for i := 1 to 8 do
    begin
      a.x := a.x + 128 - 50 + Random(90);
      a.y := CameraY - GameHeightHalf - 60 + Random(150);

      CreateSpark(a, b, 53, 255, 80);
    end;
  end;

  PlaySound(SFX_WIND, CHANNEL_WEATHER);
end;

end.

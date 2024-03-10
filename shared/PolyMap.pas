{*************************************************************}
{                                                             }
{       PolyMap Unit for OpenSoldat                           }
{                                                             }
{       Copyright (c) 2001-2020 Michal Marcinkowski           }
{       Copyright (c) 2020-2023 OpenSoldat contributors       }
{                                                             }
{*************************************************************}

unit PolyMap;

interface

uses
  // Helper units
  Util,
  Vector,

  // Project units
  MapFile,
  Waypoints;


const
  MAX_POLYS = 5000;
  MIN_SECTOR  = -25;
  MAX_SECTOR  = 25;
  MIN_SECTORZ = -35;
  MAX_SECTORZ = 35;
  TILESECTOR  = 3;
  MIN_TILE = MIN_SECTOR * TILESECTOR;
  MAX_TILE = MAX_SECTOR * TILESECTOR;
  MAX_PROPS = 500;
  MAX_TEXTURES = 8;
  MAX_SPAWNPOINTS = 255;
  MAX_COLLIDERS = 128;

  // Polygon constants go here
  // ...
  POLY_TYPE_NORMAL                = 0;
  POLY_TYPE_ONLY_BULLETS          = 1;
  POLY_TYPE_ONLY_PLAYER           = 2;
  POLY_TYPE_DOESNT                = 3;
  POLY_TYPE_ICE                   = 4;
  POLY_TYPE_DEADLY                = 5;
  POLY_TYPE_BLOODY_DEADLY         = 6;
  POLY_TYPE_HURTS                 = 7;
  POLY_TYPE_REGENERATES           = 8;
  POLY_TYPE_LAVA                  = 9;
  POLY_TYPE_RED_BULLETS           = 10;
  POLY_TYPE_RED_PLAYER            = 11;
  POLY_TYPE_BLUE_BULLETS          = 12;
  POLY_TYPE_BLUE_PLAYER           = 13;
  POLY_TYPE_YELLOW_BULLETS        = 14;
  POLY_TYPE_YELLOW_PLAYER         = 15;
  POLY_TYPE_GREEN_BULLETS         = 16;
  POLY_TYPE_GREEN_PLAYER          = 17;
  POLY_TYPE_BOUNCY                = 18;
  POLY_TYPE_EXPLODES              = 19;
  POLY_TYPE_HURTS_FLAGGERS        = 20;
  POLY_TYPE_ONLY_FLAGGERS         = 21;
  POLY_TYPE_NOT_FLAGGERS          = 22;
  POLY_TYPE_NON_FLAGGER_COLLIDES  = 23;
  POLY_TYPE_BACKGROUND            = 24;
  POLY_TYPE_BACKGROUND_TRANSITION = 25;

  // The poly will interact as an "only players collide" poly
  BACKGROUND_NORMAL       = 0;

  // The poly will interact as a "doesn't collide" poly
  BACKGROUND_TRANSITION   = 1;

  // Whether a background poly is being interacted with is currently unknown and
  // must be found out
  BACKGROUND_POLY_UNKNOWN = -2;

  // No background poly is currently being interacted with
  BACKGROUND_POLY_NONE    = -1;

type
  TLoadMapGraphics = procedure(var MapFile: TMapFile; BgForce: Boolean;
    BgColorTop, BgColorBtm: TMapColor);

  TPolyMap = object
  private
    procedure Initialize();
    procedure LoadData(var MapFile: TMapFile);
  public
    MapID:           LongWord;
    MapInfo:         TMapInfo;
    Name:            string[64];
    Filename:        string;
    SectorsDivision: Integer;
    SectorsNum:      Integer;
    StartJet:        Integer;
    Grenades:        Byte;
    Medikits:        Byte;
    Weather:         Byte;
    Steps:           Byte;
    PolyCount:       Integer;
    BackPolyCount:   Integer;
    ColliderCount:   Integer;
    Polys:           array[1..MAX_POLYS] of TMapPolygon;
    BackPolys:       array[1..MAX_POLYS] of PMapPolygon;
    PolyType:        array[1..MAX_POLYS] of Byte;
    Perp:            array[1..MAX_POLYS, 1..3] of TVector2;
    Bounciness:      array[1..MAX_POLYS] of Single;
    Sectors:         array[MIN_SECTORZ..MAX_SECTORZ, MIN_SECTORZ..MAX_SECTORZ] of TMapSector;
    Spawnpoints:     array[1..MAX_SPAWNPOINTS] of TMapSpawnpoint;
    Collider:        array[1..MAX_COLLIDERS] of TMapCollider;
    FlagSpawn:       array[1..2] of Integer;
    LoadGraphics:    TLoadMapGraphics;
    function LoadMap(Map: TMapInfo): Boolean; overload;
    {$IFNDEF SERVER}
    function LoadMap(Map: TMapInfo; BgForce: Boolean; BgColorTop, BgColorBtm: LongInt): Boolean; overload;
    {$ENDIF}
    function LineInPoly(const a, b: TVector2; Poly: Integer; var v: TVector2): Boolean;
    function PointInPolyEdges(x, y: Single; i: Integer): Boolean;
    function PointInPoly(const p: TVector2; const Poly: TMapPolygon): Boolean;
    function ClosestPerpendicular(j: Integer; Pos: TVector2; var d: Single;
      var n: Integer): TVector2;
    function CollisionTest(Pos: TVector2; var PerpVec: TVector2;
      IsFlag: Boolean = False): Boolean;
    function CollisionTestExcept(Pos: TVector2; var PerpVec: TVector2;
      c: Integer): Boolean;
    function RayCast(const a, b: TVector2; var Distance: Single; MaxDist: Single;
      Player: Boolean = False; Flag: Boolean = False; Bullet: Boolean = True;
      CheckCollider: Boolean = False; Team: Byte = 0): Boolean;
  end;

procedure CheckOutOfBounds(var x: Single; var y: Single); overload;
procedure CheckOutOfBounds(var x: SmallInt; var y: SmallInt); overload;


implementation

uses
  // System units
  {$IFNDEF SERVER}
    Classes,
    SysUtils,
  {$ENDIF}
  Math,

  // Helper units
  Calc,

  // Project units
  {$IFDEF SERVER}
    Server,
  {$ELSE}
    Client,
  {$ENDIF}
  Constants,
  Game;


procedure TPolyMap.Initialize();
var
  i, j: Integer;
begin
  Self.MapID := 0;
  Self.Name := '';
  Self.Filename := '';
  Self.SectorsDivision := 0;
  Self.SectorsNum := 0;
  Self.StartJet := 0;
  Self.Grenades := 0;
  Self.Medikits := 0;
  Self.Weather := 0;
  Self.Steps := 0;
  Self.PolyCount := 0;
  Self.BackPolyCount := 0;
  Self.ColliderCount := 0;

  FillChar(Self.Polys[1],       sizeof(Self.Polys),       0);
  FillChar(Self.BackPolys[1],   sizeof(Self.BackPolys),   0);
  FillChar(Self.PolyType[1],    sizeof(Self.PolyType),    0);
  FillChar(Self.Perp[1, 1],     sizeof(Self.Perp),        0);
  FillChar(Self.Bounciness[1],  sizeof(Self.Bounciness),  0);
  FillChar(Self.Spawnpoints[1], sizeof(Self.Spawnpoints), 0);
  FillChar(Self.Collider[1],    sizeof(Self.Collider),    0);
  FillChar(Self.FlagSpawn[1],   sizeof(Self.FlagSpawn),   0);

  for i := Low(Self.Sectors) to High(Self.Sectors) do
  begin
    for j := Low(Self.Sectors[i]) to High(Self.Sectors[i]) do
      Self.Sectors[i, j].Polys := nil;
  end;

  // BotPath (TWaypoints) defined in Game.pas
  FillChar(BotPath.Waypoint[1], sizeof(BotPath.Waypoint), 0);
end;

procedure TPolyMap.LoadData(var MapFile: TMapFile);
var
  i, j, k: Integer;
begin
  Self.MapID := MapFile.Hash;
  Self.SectorsDivision := MapFile.SectorsDivision;
  Self.SectorsNum := MapFile.SectorsNum;
  Self.StartJet := 119 * MapFile.StartJet div 100;  // quickfix
  Self.Grenades := MapFile.GrenadePacks;
  Self.Medikits := MapFile.Medikits;
  Self.Weather := MapFile.Weather;
  Self.Steps := MapFile.Steps;
  Self.PolyCount := Length(MapFile.Polygons);
  Self.ColliderCount := Length(MapFile.Colliders);

  if Self.PolyCount > 0 then
    Move(MapFile.Polygons[0], Self.Polys[1],
      sizeof(TMapPolygon) * Self.PolyCount);

  if Self.ColliderCount > 0 then
    Move(MapFile.Colliders[0], Self.Collider[1],
      sizeof(TMapCollider) * Self.ColliderCount);

  if Length(MapFile.Spawnpoints) > 0 then
    Move(MapFile.Spawnpoints[0], Self.Spawnpoints[1],
      sizeof(TMapSpawnpoint) * Length(MapFile.Spawnpoints));

  if Length(MapFile.Waypoints) > 0 then
    Move(MapFile.Waypoints[0], BotPath.Waypoint[1],
      sizeof(TWaypoint) * Length(MapFile.Waypoints));

  for i := 1 to Self.PolyCount do
  begin
    Self.PolyType[i] := Self.Polys[i].PolyType;

    Self.Perp[i][1].x := Self.Polys[i].Normals[1].x;
    Self.Perp[i][1].y := Self.Polys[i].Normals[1].y;
    Self.Perp[i][2].x := Self.Polys[i].Normals[2].x;
    Self.Perp[i][2].y := Self.Polys[i].Normals[2].y;
    Self.Perp[i][3].x := Self.Polys[i].Normals[3].x;
    Self.Perp[i][3].y := Self.Polys[i].Normals[3].y;

    Self.Bounciness[i] := Vec2Length(Self.Perp[i][3]);  // gg

    Vec2Normalize(Self.Perp[i][1], Self.Perp[i][1]);
    Vec2Normalize(Self.Perp[i][2], Self.Perp[i][2]);
    Vec2Normalize(Self.Perp[i][3], Self.Perp[i][3]);

    if (Self.PolyType[i] = POLY_TYPE_BACKGROUND) or
      (Self.PolyType[i] = POLY_TYPE_BACKGROUND_TRANSITION) then
    begin
      Inc(Self.BackPolyCount);
      Self.BackPolys[Self.BackPolyCount] := @Self.Polys[i];
    end;
  end;

  k := 0;

  for i := -Self.SectorsNum to Self.SectorsNum do
  begin
    for j := -Self.SectorsNum to Self.SectorsNum do
    begin
      if Length(MapFile.Sectors[k].Polys) > 0 then
      begin
        SetLength(Self.Sectors[i, j].Polys, Length(MapFile.Sectors[k].Polys) + 1);
        Move(MapFile.Sectors[k].Polys[0], Self.Sectors[i, j].Polys[1],
          sizeof(Word) * Length(MapFile.Sectors[k].Polys));
      end;

      Inc(k);
    end;
  end;

  for i := 1 to Length(MapFile.Spawnpoints) do
  begin
    if (Abs(Self.Spawnpoints[i].x) >= 2000000) or
      (Abs(Self.Spawnpoints[i].y) >= 2000000) then
      Self.Spawnpoints[i].Active := False;

    if Self.Spawnpoints[i].Active then
    begin
      if (FlagSpawn[1] = 0) and (Self.SpawnPoints[i].Team = 5) then
        FlagSpawn[1] := i;

      if (FlagSpawn[2] = 0) and (Self.SpawnPoints[i].Team = 6) then
        FlagSpawn[2] := i;
    end;
  end;

  for i := 1 to Length(MapFile.Waypoints) do
  begin
    if (Abs(BotPath.Waypoint[i].x) >= 2000000) or
      (Abs(BotPath.Waypoint[i].y) >= 2000000) then
      BotPath.Waypoint[i].Active := False;
  end;
end;

function TPolyMap.LoadMap(Map: TMapInfo): Boolean;
var
  MapFile: TMapFile;
begin
  Result := False;
  Self.Initialize();
  MapFile := Default(TMapFile);

  if LoadMapFile(Map, MapFile) then
  begin
    Self.Filename := Map.Name;
    Self.LoadData(MapFile);
    Self.Name := Map.Name;
    Self.MapInfo := Map;
    MainConsole.console(MapFile.MapName, GAME_MESSAGE_COLOR);
    Result := True;
  end;
end;
{$IFNDEF SERVER}
function TPolyMap.LoadMap(Map: TMapInfo; BgForce: Boolean; BgColorTop, BgColorBtm: LongInt): Boolean;
var
  MapFile: TMapFile;
begin
  if Self.Filename = Map.Name then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  Self.Initialize();
  MapFile := Default(TMapFile);

  if LoadMapFile(Map, MapFile) then
  begin
    Self.Filename := Map.Name;
    Self.LoadData(MapFile);
    Self.Name := Map.Name;
    Self.MapInfo := Map;

    {$IFNDEF SERVER}
    if Assigned(Self.LoadGraphics) then
      Self.LoadGraphics(MapFile, BgForce, MapColor(BgColorTop), MapColor(BgColorBtm));
    {$ENDIF}

    MainConsole.console(MapFile.MapName, GAME_MESSAGE_COLOR);
    Result := True;
  end;
end;
{$ENDIF}
function TPolyMap.LineInPoly(const a, b: TVector2; Poly: Integer;
  var v: TVector2): Boolean;
var
  i, j: Integer;
  p, q: ^TMapVertex;
  ak, am, bk, bm: Single;
begin
  Result := False;

  for i := 1 to 3 do
  begin
    if i = 3 then
      j := 1
    else
      j := i + 1;

    p := @Polys[Poly].Vertices[i];
    q := @Polys[Poly].Vertices[j];

    if (b.x <> a.x) or (q.x <> p.x) then
    begin
      if b.x = a.x then
      begin
        bk := (q.y - p.y) / (q.x - p.x);
        bm := p.y - bk * p.x;
        v.x := a.x;
        v.y := bk * v.x + bm;

        if (v.x > Min(p.x, q.x)) and (v.x < Max(p.x, q.x)) and
          (v.y > Min(a.y, b.y)) and (v.y < Max(a.y, b.y)) then
        begin
          Result := True;
          Exit;
        end;
      end
      else if q.x = p.x then
      begin
        ak := (b.y - a.y) / (b.x - a.x);
        am := a.y - ak * a.x;
        v.x := p.x;
        v.y := ak * v.x + am;

        if (v.y > Min(p.y, q.y)) and (v.y < Max(p.y, q.y)) and
          (v.x > Min(a.x, b.x)) and (v.x < Max(a.x, b.x)) then
        begin
          Result := True;
          Exit;
        end;
      end
      else
      begin
        ak := (b.y - a.y) / (b.x - a.x);
        bk := (q.y - p.y) / (q.x - p.x);

        if ak <> bk then
        begin
          am := a.y - ak * a.x;
          bm := p.y - bk * p.x;
          v.x := (bm - am) / (ak - bk);
          v.y := ak * v.x + am;

          if (v.x > Min(p.x, q.x)) and (v.x < Max(p.x, q.x)) and
            (v.x > Min(a.x, b.x)) and (v.x < Max(a.x, b.x)) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TPolyMap.PointInPolyEdges(x, y: Single; i: Integer): Boolean;
var
  u: TVector2;
  d: Single;
begin
  Result := False;

  u.x := x - Polys[i].Vertices[1].x;
  u.y := y - Polys[i].Vertices[1].y;
  d := Perp[i][1].x * u.x + Perp[i][1].y * u.y;
  if d < 0 then
    Exit;

  u.x := x - Polys[i].Vertices[2].x;
  u.y := y - Polys[i].Vertices[2].y;
  d := Perp[i][2].x * u.x + Perp[i][2].y * u.y;
  if d < 0 then
    Exit;

  u.x := x - Polys[i].Vertices[3].x;
  u.y := y - Polys[i].Vertices[3].y;
  d := Perp[i][3].x * u.x + Perp[i][3].y * u.y;
  if d < 0 then
    Exit;

  Result := True;
end;

function TPolyMap.PointInPoly(const p: TVector2; const Poly: TMapPolygon): Boolean;
var
  a, b, c: ^TMapVertex;
  ap_x: Single;
  ap_y: Single;
  p_ab: Boolean;
  p_ac: Boolean;
begin
  Result := False;

  {
    FIXME(skoskav): Explain what is going on here. Description from StackOverflow:

    Is the point p to the left of or to the right of both the lines AB and AC?
    If true, it can't be inside. If false, it is at least inside the "cones"
    that satisfy the condition. Now since we know that a point inside a trigon
    (triangle) must be to the same side of AB as BC (and also CA), we check if
    they differ. If they do, p can't possibly be inside, otherwise p must be
    inside.

    Some keywords in the calculations are line half-planes and the determinant
    (2x2 cross product).
    Perhaps a more pedagogical way is probably to think of it as a point being
    inside iff it's to the same side (left or right) to each of the lines AB,
    BC and CA.
  }

  a := @Poly.Vertices[1];
  b := @Poly.Vertices[2];
  c := @Poly.Vertices[3];

  ap_x := p.x - a.x;
  ap_y := p.y - a.y;

  p_ab := (b.x - a.x) * ap_y - (b.y - a.y) * ap_x > 0;
  p_ac := (c.x - a.x) * ap_y - (c.y - a.y) * ap_x > 0;

  if p_ac = p_ab then
    Exit;

  // p_bc <> p_ab
  if ((c.x - b.x) * (p.y - b.y) - (c.y - b.y) * (p.x - b.x) > 0) <> p_ab then
    Exit;

  Result := True;
end;

function TPolyMap.ClosestPerpendicular(j: Integer; Pos: TVector2; var d: Single;
  var n: Integer): TVector2;
var
  px, py: array[1..3] of Single;
  p1, p2: TVector2;
  d1, d2, d3: Single;
  EdgeV1, EdgeV2: Integer;
begin
  Result := Default(TVector2);
  px[1] := Polys[j].Vertices[1].x;
  py[1] := Polys[j].Vertices[1].y;

  px[2] := Polys[j].Vertices[2].x;
  py[2] := Polys[j].Vertices[2].y;

  px[3] := Polys[j].Vertices[3].x;
  py[3] := Polys[j].Vertices[3].y;

  // find closest edge
  p1.x := px[1];
  p1.y := py[1];

  p2.x := px[2];
  p2.y := py[2];

  d1 := PointLineDistance(p1, p2, Pos);
  d := d1;
  EdgeV1 := 1;
  EdgeV2 := 2;

  p1.x := px[2];
  p1.y := py[2];

  p2.x := px[3];
  p2.y := py[3];

  d2 := PointLineDistance(p1, p2, Pos);

  if d2 < d1 then
  begin
    EdgeV1 := 2;
    EdgeV2 := 3;
    d := d2;
  end;

  p1.x := px[3];
  p1.y := py[3];

  p2.x := px[1];
  p2.y := py[1];

  d3 := PointLineDistance(p1, p2, Pos);

  if (d3 < d2) and (d3 < d1) then
  begin
    EdgeV1 := 3;
    EdgeV2 := 1;
    d := d3;
  end;

  if (EdgeV1 = 1) and (EdgeV2 = 2) then
  begin
    Result := Perp[j][1];
    n := 1;
  end;

  if (EdgeV1 = 2) and (EdgeV2 = 3) then
  begin
    Result := Perp[j][2];
    n := 2;
  end;

  if (EdgeV1 = 3) and (EdgeV2 = 1) then
  begin
    Result := Perp[j][3];
    n := 3;
  end;
end;

function TPolyMap.CollisionTest(Pos: TVector2; var PerpVec: TVector2;
  IsFlag: Boolean = False): Boolean;
const
  EXCLUDED1 = [1, 2, 3, 11, 13, 15, 17, 24, 25];
  EXCLUDED2 = [21, 22, 23];
var
  j, w: Integer;
  b: Integer = 0;
  d: Single = 0.0;
  kx, ky: Integer;
begin
  Result := False;
  kx := Round(Pos.x / SectorsDivision);
  ky := Round(Pos.y / SectorsDivision);

  if (kx > -SectorsNum) and (kx < SectorsNum) and
     (ky > -SectorsNum) and (ky < SectorsNum) then
  begin
    for j := 1 to High(Sectors[kx, ky].Polys) do
    begin
      w := Sectors[kx, ky].Polys[j];

      if not (PolyType[w] in EXCLUDED1) and (IsFlag or not (PolyType[w] in EXCLUDED2)) then
      begin
        if PointInPoly(Pos, Polys[w]) then
        begin
          PerpVec := ClosestPerpendicular(w, Pos, d, b);
          Vec2Scale(PerpVec, PerpVec, 1.5 * d);
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function TPolyMap.CollisionTestExcept(Pos: TVector2; var PerpVec: TVector2;
  c: Integer): Boolean;
const
  EXCLUDED = [1, 2, 3, 11, 24, 25];
var
  j, w: Integer;
  b: Integer = 0;
  d: Single = 0.0;
  kx, ky: Integer;
begin
  Result := False;
  kx := Round(Pos.x / SectorsDivision);
  ky := Round(Pos.y / SectorsDivision);

  if (kx > -SectorsNum) and (kx < SectorsNum) and
     (ky > -SectorsNum) and (ky < SectorsNum) then
  begin
    for j := 1 to High(Sectors[kx, ky].Polys) do
    begin
      w := Sectors[kx, ky].Polys[j];

      if (w <> c) and not (PolyType[w] in EXCLUDED) then
      begin
        if PointInPoly(Pos, Polys[w]) then
        begin
          PerpVec := ClosestPerpendicular(w, Pos, d, b);
          Vec2Scale(PerpVec, PerpVec, 1.5 * d);
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;
{$IFDEF NoOverflowCheck}{$Q+}{$UNDEF NoOverflowCheck}{$ENDIF}

function TPolyMap.RayCast(const a, b: TVector2; var Distance: Single; MaxDist: Single;
  Player: Boolean = False; Flag: Boolean = False; Bullet: Boolean = True;
  CheckCollider: Boolean = False; Team: Byte = 0): Boolean;
var
  i, j, ax, ay, bx, by, p, w: Integer;
  c, d: TVector2;
  testcol: Boolean;
  npCol, nbCol: Boolean;
  e, f, g, h, r: Single;
begin
  Result := False;
  d := Default(TVector2);
  Distance := Vec2Length(Vec2Subtract(a, b));
  if Distance > MaxDist then
  begin
    Distance := 9999999;
    Result := True;
    Exit;
  end;

  ax := Round(Min(a.x, b.x) / SectorsDivision);
  ay := Round(Min(a.y, b.y) / SectorsDivision);
  bx := Round(Max(a.x, b.x) / SectorsDivision);
  by := Round(Max(a.y, b.y) / SectorsDivision);

  if (ax > MAX_SECTORZ) or (bx < MIN_SECTORZ) or
     (ay > MAX_SECTORZ) or (by < MIN_SECTORZ) then
    Exit;

  ax := Max(MIN_SECTORZ, ax);
  ay := Max(MIN_SECTORZ, ay);
  bx := Min(MAX_SECTORZ, bx);
  by := Min(MAX_SECTORZ, by);

  npCol := not Player;
  nbCol := not Bullet;

  for i := ax to bx do
  begin
    for j := ay to by do
    begin
      for p := 1 to High(Sectors[i, j].Polys) do
      begin
        w := Sectors[i, j].Polys[p];

        testcol := True;
        if ((PolyType[w] = POLY_TYPE_RED_BULLETS) and ((Team <> TEAM_ALPHA) or nbCol)) or
           ((PolyType[w] = POLY_TYPE_RED_PLAYER) and ((Team <> TEAM_ALPHA) or npCol)) then
          testcol := False;
        if ((PolyType[w] = POLY_TYPE_BLUE_BULLETS) and ((Team <> TEAM_BRAVO) or nbCol)) or
           ((PolyType[w] = POLY_TYPE_BLUE_PLAYER) and ((Team <> TEAM_BRAVO) or npCol)) then
          testcol := False;
        if ((PolyType[w] = POLY_TYPE_YELLOW_BULLETS) and ((Team <> TEAM_CHARLIE) or nbCol)) or
           ((PolyType[w] = POLY_TYPE_YELLOW_PLAYER) and ((Team <> TEAM_CHARLIE) or npCol)) then
          testcol := False;
        if ((PolyType[w] = POLY_TYPE_GREEN_BULLETS) and ((Team <> TEAM_DELTA) or nbCol)) or
           ((PolyType[w] = POLY_TYPE_GREEN_PLAYER) and ((Team <> TEAM_DELTA) or npCol)) then
          testcol := False;
        if ((not Flag or npCol) and (PolyType[w] = POLY_TYPE_ONLY_FLAGGERS)) or
           ((Flag or npCol) and (PolyType[w] = POLY_TYPE_NOT_FLAGGERS)) then
          testcol := False;
        if ((not Flag or npCol or nbCol) and (PolyType[w] = POLY_TYPE_NON_FLAGGER_COLLIDES)) then
          testcol := False;
        if ((PolyType[w] = POLY_TYPE_ONLY_BULLETS) and nbCol) or ((PolyType[w] = POLY_TYPE_ONLY_PLAYER) and npCol) or
           (PolyType[w] = POLY_TYPE_DOESNT) or (PolyType[w] = POLY_TYPE_BACKGROUND) or (PolyType[w] = POLY_TYPE_BACKGROUND_TRANSITION) then
          testcol := False;
        if testcol then
        begin
          if PointInPoly(a, Polys[w]) then
          begin
            Distance := 0;
            Result := True;
            Exit;
          end;
          if LineInPoly(a, b, w, d) then
          begin
            c := Vec2Subtract(d, a);
            Distance := Vec2Length(c);
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;

  if CheckCollider then
  begin
    // check if vector crosses any colliders
    // |A*x + B*y + C| / Sqrt(A^2 + B^2) < r
    e := a.y - b.y;
    f := b.x - a.x;
    g := a.x * b.y - a.y * b.x;
    h := Sqrt(e*e + f*f);
    for i := 1 to ColliderCount do
    begin
      if Collider[i].Active then
      begin
        if Abs(e * Collider[i].x + f * Collider[i].y + g) / h <= Collider[i].Radius then
        begin
          r := SqrDist(a.x, a.y, b.x, b.y) + Collider[i].Radius * Collider[i].Radius;
          if SqrDist(a.x, a.y, Collider[i].x, Collider[i].y) <= r then
            if SqrDist(b.x, b.y, Collider[i].x, Collider[i].y) <= r then
            begin
              Result := False;
              Break;
            end;
        end;
      end;
    end;
  end;
end;

// this should go inside TPolyMap, used only from Net.pas it seems

procedure CheckOutOfBounds(var x: Single; var y: Single);
begin
  if (x < (10 * (-Map.SectorsNum * Map.SectorsDivision) + 50)) then
    x := 1
  else if (x > (10 * (Map.SectorsNum * Map.SectorsDivision) - 50)) then
    x := 1;

  if (y < (10 * (-Map.SectorsNum * Map.SectorsDivision) + 50)) then
    y := 1
  else if (y > (10 * (Map.SectorsNum * Map.SectorsDivision) - 50)) then
    y := 1;
end;

procedure CheckOutOfBounds(var x: SmallInt; var y: SmallInt);
begin
  if (x < (10 * (-Map.SectorsNum * Map.SectorsDivision) + 50)) then
    x := 1
  else if (x > (10 * (Map.SectorsNum * Map.SectorsDivision) - 50)) then
    x := 1;

  if (y < (10 * (-Map.SectorsNum * Map.SectorsDivision) + 50)) then
    y := 1
  else if (y > (10 * (Map.SectorsNum * Map.SectorsDivision) - 50)) then
    y := 1;
end;

end.

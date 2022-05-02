unit MapGraphics;

interface

uses Gfx, MapFile, Vector, Util;

type
  TMapGraphics = record
    Filename: string;
    MapInfo: TMapInfo;
    BgForce: Boolean;
    BgForcedColor: array[0..1] of TMapColor;
    VertexBuffer: TGfxVertexBuffer;
    IndexBuffer: TGfxIndexBuffer;
    Textures: array of TGfxTexture;
    EdgeTextures: array of TGfxTexture;
    Spritesheet: TGfxSpritesheet;
    Animations: array of PGfxSprite;
    AnimationsCmd: array of PGfxDrawCommand;
    AnimationsBuffer: array of TGfxVertex;
    AnimDuration: array of Integer;
    Props: array[0..2] of array of TGfxDrawCommand;
    Minimap: TGfxSprite;
    MinimapScale: Single;
    MinimapOffset: TVector2;
    BgColorTop, BgColorBtm: TGfxColor;
    Background, BackgroundCount: Integer;
    Edges: array[0..1] of array of TGfxDrawCommand;
    Polys: array[0..1] of array of TGfxDrawCommand;
  end;

var
  MapGfx: TMapGraphics;

procedure LoadMapGraphics(var MapFile: TMapFile; BgForce: Boolean;
  BgColorTop, BgColorBtm: TMapColor);
procedure DestroyMapGraphics();
procedure UpdateProps(t: Extended);
procedure RenderProps(Level: Integer);
procedure RenderMinimap(x, y: Single; Alpha: Byte);
procedure WorldToMinimap(x, y: Single; var ox, oy: Single);
procedure SetTextureFilter(Texture: TGfxTexture; AllowMipmaps: Boolean);

implementation

uses
  Client, Math, SysUtils, PhysFS, GameRendering, Game, ClientGame, PolyMap;

function LoadMapTexture(TexName: string; ColorKey: TGfxColor): TGfxImage;
var
  s: array[1..3] of string;
  Filename: string;
  i: Integer;
begin
  Result := nil;

  s[1] := ModDir + 'textures/' + TexName;
  s[2] := 'current_map/textures/' + TexName;

  if not PHYSFS_exists(PChar(FindImagePath(s[2]))) then
    s[2] := 'textures/' + TexName;

  if Copy(TexName, 1, 6) = 'edges/' then
    s[3] := 'textures/edges/default.bmp'
  else
    s[3] := 'textures/default.bmp';

  for i := Low(s) to High(s) do
  begin
    Filename := FindImagePath(s[i]);

    if PHYSFS_exists(PChar(Filename)) then
    begin
      Result := TGfxImage.Create(Filename, ColorKey);

      if Result.GetImageData() = nil then
        FreeAndNil(Result)
      else
        Break;
    end;
  end;

  if Result = nil then
    Result := TGfxImage.Create(32, 32);

  Result.Premultiply();
end;

procedure SetTextureFilter(Texture: TGfxTexture; AllowMipmaps: Boolean);
var
  i: Integer;
  Filters: array[1..2] of TGfxTextureFilter;
begin
  if Texture = nil then
    Exit;

  i := Max(1, Min(2, r_texturefilter.Value));

  Filters[1] := GFX_NEAREST;  // "point"
  Filters[2] := GFX_LINEAR;   // "linear"

  if AllowMipmaps and r_mipmapping.Value then
  begin
    Filters[1] := GFX_MIPMAP_NEAREST;
    Filters[2] := GFX_MIPMAP_LINEAR;
  end;

  GfxTextureFilter(Texture, Filters[i], Filters[i]);

  if AllowMipmaps and r_mipmapping.Value then
    GfxGenerateMipmap(Texture);
end;

function GetTextureTargetScale(var MapFile: TMapFile; Image: TGfxImage): Single;
var
  i: Integer;
  Scale: TVector2;
  Alpha: Byte;
  Area, ResolutionX, ResolutionY: Single;
  a, b, c: TMapVertex;

  procedure UpdateScale(const p, q: TMapVertex);
  var
    dx, dy: Single;
    du, dv: Single;
    d1, d2: Single;
    sx, sy: Single;
  begin
    dx := q.x - p.x;
    dy := q.y - p.y;
    du := q.u - p.u;
    dv := q.v - p.v;
    d1 := Sqrt(dx * dx + dy * dy);
    d2 := Sqrt(du * du + dv * dv);
    sx := Abs(d1 * ResolutionX * du / d2) / (du * Image.Width);
    sy := Abs(d1 * ResolutionY * dv / d2) / (dv * Image.Height);

    if not IsNaN(sx) and not IsInfinite(sx) then
      Scale.x := Max(Scale.x, sx);

    if not IsNaN(sy) and not IsInfinite(sy) then
      Scale.y := Max(Scale.y, sy);
  end;
begin
  Scale.x := 0;
  Scale.y := 0;

  ResolutionX := RenderWidth / GameWidth;
  ResolutionY := RenderHeight / GameHeight;

  for i := Low(MapFile.Polygons) to High(MapFile.Polygons) do
  begin
    a := MapFile.Polygons[i].Vertices[1];
    b := MapFile.Polygons[i].Vertices[2];
    c := MapFile.Polygons[i].Vertices[3];

    Alpha := a.Color[3] or b.Color[3] or c.Color[3];
    Area := 0.5 * Abs((a.x - c.x) * (b.y - a.y) - (a.x - b.x) * (c.y - a.y));

    if (Alpha > 0) and (Area > 0) then
    begin
      UpdateScale(a, b);
      UpdateScale(b, c);
      UpdateScale(c, a);
    end;
  end;

  Result := Min(1, Max(Scale.x, Scale.y));
end;

{$PUSH}
{$WARN 4056 off : Conversion between ordinals and pointers is not portable}
procedure LoadMapGraphics(var MapFile: TMapFile; BgForce: Boolean;
  BgColorTop, BgColorBtm: TMapColor);
const
  BACKPOLY = [POLY_TYPE_BACKGROUND, POLY_TYPE_BACKGROUND_TRANSITION];
  IDX: array[0..5] of Integer = (0, 1, 2, 2, 3, 0);
type
  TEdge = record
    a, b: PMapVertex;
    Level: Integer;
    TextureIndex: Integer;
  end;
  TEdgeSprites = array of TGfxSprite;
  TEdges = array of TEdge;
  Tvb = array of TGfxVertex;
  Tib = array of Word;
  TSceneryCounters = array of Integer;
  TSceneryMaxSize = array of TVector2;
  TScenerySheetIndex = array of Integer;
var
  mg: ^TMapGraphics;
  i, j, k, n, d, Level: Integer;
  ImgWidth, ImgHeight: Integer;
  PropTotal, AnimTotal: Integer;
  AnimIndex, vbAnimIndex, vbPropIndex, vbIndex, ibIndex: Integer;
  Sheet: TGfxSpritesheet;
  Sprite: PGfxSprite;
  Image, EdgeImage: TGfxImage;
  EdgeSprites: TEdgeSprites;
  Color: TGfxColor;
  Prop: PMapProp;
  Poly: PMapPolygon;
  EdgeCount: Integer;
  Edge: TEdge;
  Edges: TEdges;
  Cmd: PGfxDrawCommand;
  AnimCount: array[0..2] of Integer = (0,0,0);
  PropCount: array[0..2] of Integer = (0,0,0);
  PropList: array[0..2] of array of PMapProp;
  vb: Tvb;
  ib: Tib;
  u, v, r, sx, w, h, Scale: Single;
  a, b: TVector2;
  Bounds: TGfxRect;
  Quad: array[0..3] of TGfxVertex;
  Texture: TGfxTexture;
  Str: string;
  PrevIsAnim: Boolean;
  SceneryCount: Integer;
  SceneryCounters: TSceneryCounters;
  SceneryMaxSize: TSceneryMaxSize;
  ScenerySheetIndex: TScenerySheetIndex;
begin
  mg := @MapGfx;
  DestroyMapGraphics();

  mg.MapInfo := MapFile.MapInfo;
  mg.Filename := MapFile.Filename;
  mg.BgForce := BgForce;
  mg.BgForcedColor[0] := BgColorTop;
  mg.BgForcedColor[1] := BgColorBtm;

  EdgeImage := Default(TGfxImage);
  Color := Default(TGfxColor);
  Sheet := Default(TGfxSpritesheet);
  Sprite := Default(PGfxSprite);
  Cmd := Default(PGfxDrawCommand);
  Edges := Default(TEdges);
  ScenerySheetIndex := Default(TSceneryCounters);
  SceneryMaxSize := Default(TSceneryMaxSize);
  SceneryCounters := Default(TScenerySheetIndex);
  // load map textures

  SetLength(mg.Textures, Length(MapFile.Textures));
  SetLength(mg.EdgeTextures, Length(MapFile.Textures));
  EdgeSprites := Default(TEdgeSprites);
  SetLength(EdgeSprites, Length(MapFile.Textures));

  for i := 0 to High(MapFile.Textures) do
  begin
    Image := LoadMapTexture(MapFile.Textures[i], RGBA(0, 0, 0, Byte(0)));

    ImgWidth := Image.Width;
    ImgHeight := Image.Height;

    if r_optimizetextures.Value then
    begin
      Scale := GetTextureTargetScale(MapFile, Image);
      ImgWidth := Max(2, Min(Image.Width, Round(Scale * Image.Width)));
      ImgHeight := Max(2, Min(Image.Height, Round(Scale * Image.Height)));
    end;

    mg.Textures[i] := GfxCreateTexture(ImgWidth, ImgHeight, 4, NIL);

    if mg.Textures[i].Components = 0 then
    begin
      ImgWidth := Npot(ImgWidth);
      ImgHeight := Npot(ImgHeight);

      GfxDeleteTexture(mg.Textures[i]);
      mg.Textures[i] := GfxCreateTexture(ImgWidth, ImgHeight, 4, NIL);
    end;

    if (ImgWidth <> Image.Width) or (ImgHeight <> Image.Height) then
      Image.Resize(ImgWidth, ImgHeight);

    GfxUpdateTexture(mg.Textures[i], 0, 0, ImgWidth, ImgHeight, Image.GetImageData());
    GfxTextureWrap(mg.Textures[i], GFX_REPEAT, GFX_REPEAT);
    SetTextureFilter(mg.Textures[i], True);
    FreeAndNil(Image);

    // load edge texture

    if r_smoothedges.Value then
    begin
      EdgeImage := LoadMapTexture('edges/' + MapFile.Textures[i], RGBA($00FF00));
      Image := EdgeImage;

      ImgWidth := Npot(EdgeImage.Width);
      ImgHeight := Npot(EdgeImage.Height);

      if (ImgWidth <> EdgeImage.Width) or (ImgHeight <> EdgeImage.Height) then
      begin
        Image := TGfxImage.Create(ImgWidth, ImgHeight);
        Image.Update(0, 0, EdgeImage.Width, EdgeImage.Height, EdgeImage.GetImageData());
      end;

      mg.EdgeTextures[i] := GfxCreateTexture(Image.Width, Image.Height, 4,
        Image.GetImageData());
      SetTextureFilter(mg.EdgeTextures[i], True);

      EdgeSprites[i].Scale := 1;
      EdgeSprites[i].Texture := mg.EdgeTextures[i];
      EdgeSprites[i].x := 0;
      EdgeSprites[i].y := 0;
      EdgeSprites[i].Width := EdgeImage.Width;
      EdgeSprites[i].Height := EdgeImage.Height;
      EdgeSprites[i].TexCoords.Left := 0;
      EdgeSprites[i].TexCoords.Top := 0;
      EdgeSprites[i].TexCoords.Right := EdgeImage.Width / mg.EdgeTextures[i].Width;
      EdgeSprites[i].TexCoords.Bottom := EdgeImage.Height / mg.EdgeTextures[i].Height;

      if Image <> EdgeImage then
        FreeAndNil(Image);

      FreeAndNil(EdgeImage);
    end;
  end;

  // filter unused scenery and calculate max size for each one

  SceneryCount := 0;

  if Length(MapFile.Scenery) > 0 then
  begin
    n := Length(MapFile.Scenery);

    SetLength(SceneryCounters, n);
    SetLength(SceneryMaxSize, n);
    SetLength(ScenerySheetIndex, n);

    FillChar(SceneryCounters[0], n * sizeof(Integer), 0);
    FillChar(SceneryMaxSize[0], n * sizeof(TVector2), 0);
    FillChar(ScenerySheetIndex[0], n * sizeof(Integer), 0);

    for i := Low(MapFile.Props) to High(MapFile.Props) do
    begin
      if IsPropActive(MapFile, i) then
      begin
        Prop := @MapFile.Props[i];
        Inc(SceneryCounters[Prop.Style - 1]);
        SceneryMaxSize[Prop.Style - 1].x := Max(SceneryMaxSize[Prop.Style - 1].x,
          Abs(Prop.ScaleX * Prop.Width) * (RenderHeight / GameHeight));
        SceneryMaxSize[Prop.Style - 1].y := Max(SceneryMaxSize[Prop.Style - 1].y,
          Abs(Prop.ScaleY * Prop.Height) * (RenderHeight / GameHeight));
      end;
    end;

    for i := Low(MapFile.Scenery) to High(MapFile.Scenery) do
    begin
      if SceneryCounters[i] > 0 then
      begin
        ScenerySheetIndex[i] := SceneryCount;
        Inc(SceneryCount);
      end;
    end;
  end;

  // load scenery spritesheet

  if SceneryCount > 0 then
  begin
    Color := RGBA($00FF00);
    Sheet := TGfxSpritesheet.Create(SceneryCount);

    for i := Low(MapFile.Scenery) to High(MapFile.Scenery) do
    begin
      if SceneryCounters[i] > 0 then
      begin
        Str := FindImagePath('current_map/scenery-gfx/' + MapFile.Scenery[i].Filename);

        if not PHYSFS_exists(PChar(Str)) then
          Str := FindImagePath('scenery-gfx/' + MapFile.Scenery[i].Filename);

        if r_optimizetextures.Value then
        begin
          SceneryMaxSize[i].x := 1.5 * SceneryMaxSize[i].x;
          SceneryMaxSize[i].y := 1.5 * SceneryMaxSize[i].y;
          Sheet.AddImage(Str, Color, SceneryMaxSize[i])
        end
        else
          Sheet.AddImage(Str, Color, 1);
      end;
    end;

    Sheet.Load();

    for i := 0 to SceneryCount - 1 do
      Sheet[i].Scale := 1;

    mg.Spritesheet := Sheet;
    Str := '';

    for i := 0 to Sheet.TextureCount - 1 do
    begin
      Str := Str + Format('%dx%d ', [Sheet.Texture[i].Width, Sheet.Texture[i].Height]);
      SetTextureFilter(Sheet.Texture[i], True);
    end;

    Str[Length(Str)] := ')';
    GfxLog('Loaded map spritesheet (' + Str);
  end;

  // calculate prop counts and filter out inactive/invalid ones

  FillChar(PropCount[0], sizeof(PropCount), 0);
  FillChar(AnimCount[0], sizeof(AnimCount), 0);

  SetLength(PropList[0], Length(MapFile.Props));
  SetLength(PropList[1], Length(MapFile.Props));
  SetLength(PropList[2], Length(MapFile.Props));

  for i := Low(MapFile.Props) to High(MapFile.Props) do
  begin
    if IsPropActive(MapFile, i) then
    begin
      Prop := @MapFile.Props[i];
      PropList[Prop.Level][PropCount[Prop.Level]] := Prop;
      Inc(PropCount[Prop.Level]);

      if Sheet[ScenerySheetIndex[Prop.Style - 1]].Next <> nil then
        Inc(AnimCount[Prop.Level]);
    end;
  end;

  PropTotal := PropCount[0] + PropCount[1] + PropCount[2];
  AnimTotal := AnimCount[0] + AnimCount[1] + AnimCount[2];

  // calculate edge count

  EdgeCount := 0;

  if r_smoothedges.Value then
  begin
    SetLength(Edges, 3 * Length(MapFile.Polygons));

    for i := Low(MapFile.Polygons) to High(MapFile.Polygons) do
    begin
      j := 3;

      for k := 1 to 3 do
      begin
        Edge.a := @MapFile.Polygons[i].Vertices[k];
        Edge.b := @MapFile.Polygons[i].Vertices[j];

        a.x := Edge.a.x + (Edge.b.x - Edge.a.x) * 0.5;
        a.y := Edge.a.y + (Edge.b.y - Edge.a.y) * 0.5;
        b := Default(TVector2);

        if (not Map.CollisionTestExcept(a, b, i + 1)) and
          (Min(Edge.a.Color[3], Edge.b.Color[3]) > 128) then
        begin
          Edges[EdgeCount] := Edge;
          Edges[EdgeCount].Level := 1 - Ord(MapFile.Polygons[i].PolyType in BACKPOLY);
          Edges[EdgeCount].TextureIndex := MapFile.Polygons[i].TextureIndex;
          Inc(EdgeCount);
        end;

        j := k;
      end;
    end;
  end;

  {-----------------------------------------------------------------------------

  Vertex buffer layout
  --------------------

  [              props               ][   edges   ][background][   polys   ]
  [    animated    ][     other      ][back][front]            [back][front]
  [back][mid][front][back][mid][front]

  Props & edges: 4 vertices per sprite (indexed by index buffer)
  Background:    6 vertices (2 triangles)
  Polys:         3 vertices per triangle

  Index buffer layout
  -------------------

  [     props      ][   edges   ]
  [back][mid][front][back][front]

  6 indices per sprite (2 triangles)

  -----------------------------------------------------------------------------}

  // initialize vertex/index buffers + animations list
  vb := Default(Tvb);
  ib := Default(Tib);
  SetLength(vb, 4 * PropTotal + 4 * EdgeCount + 6 + 3 * Length(MapFile.Polygons));
  SetLength(ib, 6 * PropTotal + 6 * EdgeCount);
  SetLength(mg.Animations, AnimTotal);
  SetLength(mg.AnimationsCmd, AnimTotal);
  SetLength(mg.AnimDuration, AnimTotal);
  SetLength(mg.AnimationsBuffer, 4 * AnimTotal);

  // setup props

  AnimIndex := 0;
  vbAnimIndex := 0;
  vbPropIndex := 4 * AnimTotal;
  ibIndex := 0;
  PrevIsAnim := False;

  // Next loop iterates all props in the order they should be drawn. The order
  // is defined by the index buffer (ib), which points to vertices in the vertex
  // buffer (vb). The reason I'm using this index buffer is so all animated
  // scenery can be kept at the beggining of the vertex buffer while preserving
  // the correct drawing order. Having all animated scenery together in the
  // vertex buffer is convenient because their vertices can be updated with one
  // single GfxUpdateBuffer() call every frame.

  for Level := Low(PropList) to High(PropList) do
  begin
    for i := 0 to PropCount[Level] - 1 do
    begin
      Prop := PropList[Level][i];
      Sprite := Sheet[ScenerySheetIndex[Prop.Style - 1]];
      n := Length(mg.Props[Level]);

      if Sprite.Next <> nil then
      begin
        vbIndex := vbAnimIndex;
        Inc(vbAnimIndex, 4);

        PrevIsAnim := True;
        SetLength(mg.Props[Level], n + 1);
        mg.Props[Level][n].Texture := Sprite.Texture;
        mg.Props[Level][n].Offset := ibIndex;
        mg.Props[Level][n].Count := 6;

        mg.Animations[AnimIndex] := Sprite;
        mg.AnimationsCmd[AnimIndex] := Pointer((Level shl 28) or (n and $0FFFFFFF));
        Inc(AnimIndex);
      end
      else
      begin
        vbIndex := vbPropIndex;
        Inc(vbPropIndex, 4);

        if PrevIsAnim or (n = 0) or
          (mg.Props[Level][n - 1].Texture <> Sprite.Texture) then
        begin
          PrevIsAnim := False;
          SetLength(mg.Props[Level], n + 1);
          mg.Props[Level][n].Texture := Sprite.Texture;
          mg.Props[Level][n].Offset := ibIndex;
          mg.Props[Level][n].Count := 6;
        end
        else
          Inc(mg.Props[Level][n - 1].Count, 6);
      end;

      Color := RGBA(Prop.Color[0], Prop.Color[1], Prop.Color[2], Prop.Alpha);

      GfxSpriteVertices(Sprite, Prop.x, Prop.y, Prop.Width, Prop.Height,
        Prop.ScaleX, Prop.ScaleY, 0, 1, -Prop.Rotation, Color, @vb[vbIndex]);

      for j := 0 to 5 do
        ib[ibIndex + j] := vbIndex + IDX[j];

      Inc(ibIndex, 6);
    end;
  end;

  // initialize animation data

  if AnimTotal > 0 then
    Move(vb[0], mg.AnimationsBuffer[0], 4 * AnimTotal * sizeof(TGfxVertex));

  for i := Low(mg.Animations) to High(mg.Animations) do
  begin
    mg.AnimationsCmd[i] := @mg.Props[LongWord(mg.AnimationsCmd[i]) shr 28]
      [LongWord(mg.AnimationsCmd[i]) and $0FFFFFFF];

    mg.AnimDuration[i] := 0;
    Sprite := mg.Animations[i];

    while Sprite <> nil do
    begin
      Inc(mg.AnimDuration[i], Sprite.Delay);
      Sprite := Sprite.Next;
    end;
  end;

  // edges

  if r_smoothedges.Value then
  begin
    n := 0;
    vbIndex := 4 * PropTotal;
    ibIndex := 6 * PropTotal;

    for Level := 0 to 1 do
    begin
      for i := 0 to EdgeCount - 1 do
      begin
        if Edges[i].Level = Level then
        begin
          if Length(mg.Edges[Level]) > 0 then
            Cmd := @mg.Edges[Level][High(mg.Edges[Level])];

          if (Length(mg.Edges[Level]) = 0) or
            (Cmd.Texture <> mg.EdgeTextures[Edges[i].TextureIndex]) then
          begin
            SetLength(mg.Edges[Level], Length(mg.Edges[Level]) + 1);
            Cmd := @mg.Edges[Level][High(mg.Edges[Level])];
            Cmd.Texture := mg.EdgeTextures[Edges[i].TextureIndex];
            Cmd.Offset := ibIndex;
            Cmd.Count := 0;
            Sprite := @EdgeSprites[Edges[i].TextureIndex];
          end;

          a.x := Edges[i].a.x;
          a.y := Edges[i].a.y;
          b.x := Edges[i].b.x;
          b.y := Edges[i].b.y;

          r := ArcTan2(b.y - a.y, b.x - a.x);
          sx := Sqrt(Sqr(b.x - a.x) + Sqr(b.y - a.y)) / 90;

          Move(Edges[i].a.Color[0], Color, 4);
          Color.a := Trunc(Color.a * 0.75);

          GfxSpriteVertices(Sprite, a.x, a.y, Sprite.Width, Sprite.Height, sx, 1,
            0, 0.5, r, Color, @vb[vbIndex]);

          Move(Edges[i].b.Color[0], Color, 4);
          Color.a := Trunc(Color.a * 0.75);

          vb[vbIndex + 1].Color := Color;
          vb[vbIndex + 2].Color := Color;

          for j := 0 to 5 do
            ib[ibIndex + j] := vbIndex + IDX[j];

          Inc(n);
          Inc(vbIndex, 4);
          Inc(ibIndex, 6);
          Inc(Cmd.Count, 6);
        end;
      end;
    end;
  end;

  // background

  i := 4 * (PropTotal + EdgeCount);
  h := GameHeight;
  d := MAX_SECTOR * Max(MapFile.SectorsDivision, Ceil(0.5 * h / MAX_SECTOR));

  if BgForce then
  begin
    Move(BgColorTop[0], mg.BgColorTop, 4);
    Move(BgColorBtm[0], mg.BgColorBtm, 4);
  end
  else
  begin
    Move(MapFile.BgColorTop[0], mg.BgColorTop, 4);
    Move(MapFile.BgColorBtm[0], mg.BgColorBtm, 4);
  end;

  mg.BgColorTop.a := 255;
  mg.BgColorBtm.a := 255;

  vb[i + 0] := GfxVertex(0, -d, 0, 0, mg.BgColorTop);
  vb[i + 1] := GfxVertex(1, -d, 0, 0, mg.BgColorTop);
  vb[i + 2] := GfxVertex(1,  d, 0, 0, mg.BgColorBtm);
  vb[i + 3] := GfxVertex(1,  d, 0, 0, mg.BgColorBtm);
  vb[i + 4] := GfxVertex(0,  d, 0, 0, mg.BgColorBtm);
  vb[i + 5] := GfxVertex(0, -d, 0, 0, mg.BgColorTop);

  mg.Background := i;
  mg.BackgroundCount := 6;

  // polygons

  vbIndex := mg.Background + mg.BackgroundCount;

  for Level := 0 to 1 do
  begin
    for i := Low(MapFile.Polygons) to  High(MapFile.Polygons) do
    begin
      Poly := @MapFile.Polygons[i];

      if Level = iif(Poly.PolyType in BACKPOLY, 0, 1) then
      begin
        if Length(mg.Polys[Level]) > 0 then
          Cmd := @mg.Polys[Level][High(mg.Polys[Level])];

        if (Length(mg.Polys[Level]) = 0) or (Cmd.Texture <> mg.Textures[Poly.TextureIndex]) then
        begin
          SetLength(mg.Polys[Level], Length(mg.Polys[Level]) + 1);
          Cmd := @mg.Polys[Level][High(mg.Polys[Level])];
          Cmd.Texture := mg.Textures[Poly.TextureIndex];
          Cmd.Offset := vbIndex;
          Cmd.Count := 0;
        end;

        for j := 1 to 3 do
        begin
          vb[vbIndex].x := Poly.Vertices[j].x;
          vb[vbIndex].y := Poly.Vertices[j].y;
          vb[vbIndex].u := Poly.Vertices[j].u;
          vb[vbIndex].v := Poly.Vertices[j].v;
          Move(Poly.Vertices[j].Color[0], vb[vbIndex].Color, 4);
          Inc(vbIndex);
        end;

        Inc(Cmd.Count, 3);
      end;
    end;
  end;

  // create gfx buffers

  mg.VertexBuffer := GfxCreateBuffer(Length(vb), True, @vb[0]);
  mg.IndexBuffer := GfxCreateIndexBuffer(Length(ib), True, @ib[0]);

  // calculate map bounds

  Bounds := Default(TGfxRect);

  if Length(MapFile.Polygons) > 0 then
  begin
    Bounds.Left   := MapFile.Polygons[0].Vertices[1].x;
    Bounds.Right  := MapFile.Polygons[0].Vertices[1].x;
    Bounds.Top    := MapFile.Polygons[0].Vertices[1].y;
    Bounds.Bottom := MapFile.Polygons[0].Vertices[1].y;
  end;

  for i := Low(MapFile.Polygons) to  High(MapFile.Polygons) do
  begin
    for j := 1 to 3 do
    begin
      Bounds.Left   := Min(MapFile.Polygons[i].Vertices[j].x, Bounds.Left);
      Bounds.Right  := Max(MapFile.Polygons[i].Vertices[j].x, Bounds.Right);
      Bounds.Top    := Min(MapFile.Polygons[i].Vertices[j].y, Bounds.Top);
      Bounds.Bottom := Max(MapFile.Polygons[i].Vertices[j].y, Bounds.Bottom);
    end;
  end;

  // calculate minimap size

  if r_scaleinterface.Value then
    w := 260 * (RenderWidth / GameWidth)
  else
    w := Round(130 / (0.5 * GameWidth / RenderWidth));

  sx := w / ((Bounds.Right - Bounds.Left) + (Bounds.Bottom - Bounds.Top));

  i := Round(sx * (Bounds.Right - Bounds.Left));  // width
  j := Round(sx * (Bounds.Bottom - Bounds.Top));  // height

  mg.Minimap.Texture := nil;
  mg.MinimapScale := sx;
  mg.MinimapOffset.x := -Bounds.Right;
  mg.MinimapOffset.y := -Bounds.Bottom;

  // create minimap sprite/texture

  if GfxFramebufferSupported then
  begin
    n := 4;  // supersampling
    Texture := GfxCreateRenderTarget(Npot(n * i), Npot(n * j));

    GfxTarget(Texture);
    GfxViewport(0, 0, n * i, n * j);
    GfxClear(0, 0, 0, 0);

    GfxTransform(GfxMat3Ortho(0, 1, Bounds.Top, Bounds.Bottom));
    GfxBegin();

    Quad[0] := GfxVertex(0, Min(-d, Bounds.Top), 0, 0, mg.BgColorTop);
    Quad[1] := GfxVertex(1, Min(-d, Bounds.Top), 0, 0, mg.BgColorTop);
    Quad[2] := GfxVertex(1, Bounds.Top, 0, 0, mg.BgColorTop);
    Quad[3] := GfxVertex(0, Bounds.Top, 0, 0, mg.BgColorTop);

    GfxDrawQuad(nil, Quad);

    Quad[0] := GfxVertex(0, Bounds.Bottom, 0, 0, mg.BgColorBtm);
    Quad[1] := GfxVertex(1, Bounds.Bottom, 0, 0, mg.BgColorBtm);
    Quad[2] := GfxVertex(1, Max(d, Bounds.Bottom), 0, 0, mg.BgColorBtm);
    Quad[3] := GfxVertex(0, Max(d, Bounds.Bottom), 0, 0, mg.BgColorBtm);

    GfxDrawQuad(nil, Quad);
    GfxEnd();

    GfxBindTexture(nil);
    GfxTransform(GfxMat3Ortho(0, 1, Bounds.Top, Bounds.Bottom));
    GfxDraw(mg.VertexBuffer, mg.Background, mg.BackgroundCount);

    GfxTransform(GfxMat3Ortho(Bounds.Left, Bounds.Right, Bounds.Top, Bounds.Bottom));

    if Length(mg.Polys[0]) > 0 then
      GfxDraw(mg.VertexBuffer, mg.Polys[0][0].Offset, vbIndex - mg.Polys[0][0].Offset)
    else if Length(mg.Polys[1]) > 0 then
      GfxDraw(mg.VertexBuffer, mg.Polys[1][0].Offset, vbIndex - mg.Polys[1][0].Offset);

    // downsample rendered minimap texture to a new texture with original size

    Sprite := @mg.Minimap;
    Sprite.Texture := GfxCreateRenderTarget(Npot(i), Npot(j));
    Sprite.x := 0;
    Sprite.y := 0;
    Sprite.Width := i;
    Sprite.Height := j;
    Sprite.Scale := 1;
    Sprite.TexCoords.Left := 0;
    Sprite.TexCoords.Top := 0;
    Sprite.TexCoords.Right := i / Sprite.Texture.Width;
    Sprite.TexCoords.Bottom := j / Sprite.Texture.Height;

    if r_scaleinterface.Value then
    begin
      Sprite.Scale := GameWidth / RenderWidth;
      mg.MinimapScale := mg.MinimapScale * Sprite.Scale;
    end;

    GfxTarget(Sprite.Texture);
    GfxViewport(0, 0, i, j);
    GfxClear(0, 0, 0, 0);
    GfxTransform(GfxMat3Ortho(0, 1, 0, 1));

    u := (n * i) / Texture.Width;
    v := (n * j) / Texture.Height;
    Color := RGBA($FFFFFF);

    Quad[0] := GfxVertex(0, 0, 0, 0, Color);
    Quad[1] := GfxVertex(1, 0, u, 0, Color);
    Quad[2] := GfxVertex(1, 1, u, v, Color);
    Quad[3] := GfxVertex(0, 1, 0, v, Color);

    GfxBegin();
    GfxDrawQuad(Texture, Quad);
    GfxEnd();
    GfxDeleteTexture(Texture);

    SetTextureFilter(Sprite.Texture, False);

    GfxTarget(nil);
    GfxViewport(0, 0, RenderWidth, RenderHeight);
  end;
end;
{$POP}

procedure UpdateProps(t: Extended);
var
  i, vbIndex, AnimTime, Accum: Integer;
  Duration: Single;
  mg: ^TMapGraphics;
  Sprite: PGfxSprite;
begin
  mg := @MapGfx;
  vbIndex := 0;

  for i := Low(mg.Animations) to High(mg.Animations) do
  begin
    if mg.AnimDuration[i] > 0 then
    begin
      Accum := 0;
      Duration := mg.AnimDuration[i] / 100;
      AnimTime := Trunc(100 * (t - Duration * Trunc(t / Duration)));
      Sprite := mg.Animations[i];

      while (Accum + Sprite.Delay) < AnimTime do
      begin
        if Sprite.Next = nil then
          Break;

        Accum := Accum + Sprite.Delay;
        Sprite := Sprite.Next;
      end;

      mg.AnimationsBuffer[vbIndex + 0].u := Sprite.TexCoords.Left;
      mg.AnimationsBuffer[vbIndex + 0].v := Sprite.TexCoords.Top;
      mg.AnimationsBuffer[vbIndex + 1].u := Sprite.TexCoords.Right;
      mg.AnimationsBuffer[vbIndex + 1].v := Sprite.TexCoords.Top;
      mg.AnimationsBuffer[vbIndex + 2].u := Sprite.TexCoords.Right;
      mg.AnimationsBuffer[vbIndex + 2].v := Sprite.TexCoords.Bottom;
      mg.AnimationsBuffer[vbIndex + 3].u := Sprite.TexCoords.Left;
      mg.AnimationsBuffer[vbIndex + 3].v := Sprite.TexCoords.Bottom;

      mg.AnimationsCmd[i].Texture := Sprite.Texture;
    end;

    Inc(vbIndex, 4);
  end;

  if vbIndex > 0 then
    GfxUpdateBuffer(mg.VertexBuffer, 0, vbIndex, @mg.AnimationsBuffer[0]);
end;

procedure RenderProps(Level: Integer);
var
  i: Integer;
  mg: ^TMapGraphics;
begin
  mg := @MapGfx;

  for i := 0 to High(mg.Props[Level]) do
  begin
    GfxBindTexture(mg.Props[Level][i].Texture);
    GfxDraw(mg.VertexBuffer, mg.IndexBuffer, mg.Props[Level][i].Offset,
      mg.Props[Level][i].Count);
  end;
end;

procedure RenderMinimap(x, y: Single; Alpha: Byte);
begin
  if MapGfx.Minimap.Texture <> nil then
    GfxDrawSprite(@MapGfx.Minimap, x, y, RGBA($FFFFFF, Alpha));
end;

procedure WorldToMinimap(x, y: Single; var ox, oy: Single);
begin
  ox := (x - MapGfx.MinimapOffset.x) * MapGfx.MinimapScale;
  oy := (y - MapGfx.MinimapOffset.y) * MapGfx.MinimapScale;
end;

procedure DestroyMapGraphics();
var
  i: Integer;
begin
  if MapGfx.VertexBuffer <> nil then
    GfxDeleteBuffer(MapGfx.VertexBuffer);

  if MapGfx.IndexBuffer <> nil then
    GfxDeleteIndexBuffer(MapGfx.IndexBuffer);

  if MapGfx.Spritesheet <> nil then
    FreeAndNil(MapGfx.Spritesheet);

  if MapGfx.Minimap.Texture <> nil then
    GfxDeleteTexture(MapGfx.Minimap.Texture);

  for i := 0 to High(MapGfx.Textures) do
  begin
    if MapGfx.Textures[i] <> nil then
      GfxDeleteTexture(MapGfx.Textures[i]);
  end;

  for i := 0 to High(MapGfx.EdgeTextures) do
  begin
    if MapGfx.EdgeTextures[i] <> nil then
      GfxDeleteTexture(MapGfx.EdgeTextures[i]);
  end;

  MapGfx.Filename := '';
  MapGfx.Animations := nil;
  MapGfx.AnimationsCmd := nil;
  MapGfx.AnimationsBuffer := nil;
  MapGfx.AnimDuration := nil;
  MapGfx.Props[0] := nil;
  MapGfx.Props[1] := nil;
  MapGfx.Props[2] := nil;
  MapGfx.Textures := nil;
  MapGfx.EdgeTextures := nil;
  MapGfx.Edges[0] := nil;
  MapGfx.Edges[1] := nil;
  MapGfx.Polys[0] := nil;
  MapGfx.Polys[1] := nil;

  FillChar(MapGfx, sizeof(MapGfx), 0);
end;

end.

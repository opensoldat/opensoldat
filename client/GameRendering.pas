unit GameRendering;

interface

uses
  Gfx, SDL2;

type
  TGameRenderingParams = record
    InterfaceName: string;
  end;

const
  FONT_BIG          = 0;
  FONT_SMALL        = 1;
  FONT_SMALL_BOLD   = 2;
  FONT_SMALLEST     = 3;
  FONT_MENU         = 4;
  FONT_WEAPONS_MENU = 5;
  FONT_WORLD        = 6;
  FONT_LAST         = FONT_WORLD;

var
  GameRenderingParams: TGameRenderingParams;
  Textures: TGfxSpriteArray;

function InitGameGraphics: Boolean;
procedure ReloadGraphics;
procedure DestroyGameGraphics();
procedure RenderFrame(TimeElapsed, FramePercent: Extended; Paused: Boolean);
procedure RenderGameInfo(TextString: WideString);
function DoTextureLoading(FinishLoading: Boolean = False): Boolean;
procedure SetFontStyle(Style: Integer); overload;
procedure SetFontStyle(Style: Integer; Scale: Single); overload;
function FontStyleSize(Style: Integer): Single;
procedure TakeScreenshot(Filename: string; Async: Boolean = True);
procedure FillCaseInsensitiveImageMap;
function FindImagePath(const Filename: string): string;

implementation

uses
  Client,
  Math, SysUtils, IniFiles, Classes, Contnrs,
  Constants, Sprites, Parts, Game, Weapons, PolyMap, MapFile, Vector, Util,
  InterfaceGraphics, ClientGame, GameStrings, GostekGraphics, Input,
  PhysFS, Cvar, MapGraphics, TraceLog {$IFDEF TESTING},  Version{$ENDIF};

type
  TTextureLoadData = record
    ID: Integer;
    Group: Integer;
    Path: string;
    ColorKey: LongWord;
  end;

  TInterpolationState = record
    Camera: TVector2;
    Mouse: TVector2;
    SpritePos: array[1..MAX_SPRITES] of array[1..24] of TVector2;
    BulletPos: array[1..MAX_BULLETS] of TVector2;
    BulletVel: array[1..MAX_BULLETS] of TVector2;
    BulletHitMul: array[1..MAX_BULLETS] of Single;
    SparkPos: array[1..MAX_SPARKS] of TVector2;
    ThingPos: array[1..MAX_THINGS] of array[1..4] of TVector2;
  end;

  TFontStyle = record
    Font: TGfxFont;
    TableIndex: Integer;
    Size: Single;
    Stretch: Single;
    Flags: LongWord;
  end;

const
  LOAD_DATA: array[1..GFXID_END] of TTextureLoadData = (
    {$DEFINE GFXDATA}
    {$INCLUDE gfx.inc}
    {$UNDEF GFXDATA}
  );

var
  Initialized: Boolean;
  LoadedInterfaceName: string;
  MainSpritesheet: TGfxSpritesheet;
  InterfaceSpritesheet: TGfxSpritesheet;
  Fonts: array[1..2] of TGfxFont;
  FontStyles: array[0..FONT_LAST] of TFontStyle;
  CaseInsensitiveImageMap: TFPStringHashTable;
  ActionSnapTexture: TGfxTexture;
  RenderTarget: TGfxTexture;
  RenderTargetAA: TGfxTexture;
  ScreenshotPath: string;
  ScreenshotAsync: Boolean;
  ImageScale: array[1..GFXID_END] of Single;
  GostekData: TStringList;
  ScaleData: record
    Root: TStringList;
    CurrentMod: TStringList;
    CustomInterface: TStringList;
  end;

procedure LoadModInfo();
var
  i: Integer;
  RootIni, ModIni, InterfaceIni: TIniFile;
  RootIniStream, ModIniStream, InterfaceIniStream: TStream;
  ModGostek: TStringList;
  Key: string;
begin
  // load required ini's

  RootIni := nil;
  ModIni := nil;
  InterfaceIni := nil;

  RootIniStream := PHYSFS_readAsStream(PChar('mod.ini'));

  if not Assigned(GostekData) or not Assigned(ScaleData.Root) then
  begin
    RootIni := TIniFile.Create(RootIniStream);
  end;

  ModIniStream := PHYSFS_readAsStream(PChar(ModDir + 'mod.ini'));

  if not Assigned(GostekData) or not Assigned(ScaleData.CurrentMod) then
    ModIni := TIniFile.Create(ModIniStream);

  InterfaceIniStream := PHYSFS_readAsStream(PChar('custom-interfaces/' + GameRenderingParams.InterfaceName + '/mod.ini'));

  if not Assigned(ScaleData.CustomInterface) then
    InterfaceIni := TIniFile.Create(InterfaceIniStream);

  // gostek

  if not Assigned(GostekData) then
  begin
    GostekData := TStringList.Create;
    RootIni.ReadSectionValues('GOSTEK', GostekData);

    ModGostek := TStringList.Create;
    ModIni.ReadSectionValues('GOSTEK', ModGostek);

    for i := 0 to ModGostek.Count - 1 do
    begin
      Key := ModGostek.Names[i];

      if Key <> '' then
        GostekData.Values[Key] := ModGostek.ValueFromIndex[i];
    end;

    FreeAndNil(ModGostek);
    LoadGostekData(GostekData);
  end;

  // scale

  if not Assigned(ScaleData.Root) then
  begin
    ScaleData.Root := TStringList.Create;
    RootIni.ReadSectionValues('SCALE', ScaleData.Root);
  end;

  if not Assigned(ScaleData.CurrentMod) then
  begin
    ScaleData.CurrentMod := TStringList.Create;
    ModIni.ReadSectionValues('SCALE', ScaleData.CurrentMod);
  end;

  if not Assigned(ScaleData.CustomInterface) then
  begin
    ScaleData.CustomInterface := TStringList.Create;
    InterfaceIni.ReadSectionValues('SCALE', ScaleData.CustomInterface);
  end;

  // cleanup
  RootIni.Free;
  ModIni.Free;
  InterfaceIni.Free;
  RootIniStream.Free;
  ModIniStream.Free;
  InterfaceIniStream.Free;
end;

function GetImageScale(ImagePath: String): Single;
var
  Data: TStringList;
  IntDir, Scale, Key: string;
  Path: String;
begin
  IntDir := 'custom-interfaces/' + LowerCase(GameRenderingParams.InterfaceName) + '/';

  Data := ScaleData.Root;
  Path := LowerCase(Copy(ImagePath, 1, Length(ImagePath)));

  if (ModDir <> '') and (LowerCase(ModDir) = Copy(Path, 1, Length(ModDir))) then
  begin
    Data := ScaleData.CurrentMod;
    Path := Copy(Path, Length(ModDir) + 1, Length(Path));
  end
  else if Copy(Path, 1, Length(IntDir)) = IntDir then
  begin
    Data := ScaleData.CustomInterface;
    Path := Copy(Path, Length(IntDir) + 1, Length(Path));
  end;

  Key := StringReplace(Path, '\', '/', [rfReplaceAll]);
  Scale := Data.Values[Key];

  if Scale = '' then
  begin
    Key := StringReplace(ExtractFileDir(Path), '\', '/', [rfReplaceAll]);
    Scale := Data.Values[Key];

    if Scale = '' then
      Scale := Data.Values['DefaultScale'];
  end;

  Result := StrToFloatDef(Scale, 1);
end;

procedure TakeScreenshot(Filename: string; Async: Boolean = True);
begin
  ScreenshotPath := Filename;
  ScreenshotAsync := Async;
end;

procedure FillCaseInsensitiveImageMap;
var
  ImageDir: String;
  ImageDirs: Array of String;
  Images: Array of String;
  Image: String;
begin
  if CaseInsensitiveImageMap = Nil then
    Exit;
  CaseInsensitiveImageMap.Clear();

  ImageDirs := TStringArray.Create(
    'scenery-gfx/',
    'current_map/scenery-gfx/',
    'textures/',
    'textures/edges/',
    'textures/objects/',
    'current_map/textures/',
    'current_map/textures/edges/',
    ModDir + 'textures/',
    ModDir + 'textures/edges/',
    ModDir + 'textures/objects/',
    'gostek-gfx/',
    'gostek-gfx/ranny/',
    'gostek-gfx/team2/',
    'gostek-gfx/team2/ranny/',
    ModDir + 'gostek-gfx/',
    ModDir + 'gostek-gfx/ranny/',
    ModDir + 'gostek-gfx/team2/',
    ModDir + 'gostek-gfx/team2/ranny/',
    'weapons-gfx/',
    ModDir + 'weapons-gfx/',
    'sparks-gfx/',
    'sparks-gfx/explosion/',
    'sparks-gfx/flames/',
    ModDir + 'sparks-gfx/',
    ModDir + 'sparks-gfx/explosion/',
    ModDir + 'sparks-gfx/flames/',
    'interface-gfx/',
    'interface-gfx/guns/',
    ModDir + 'interface-gfx/',
    ModDir + 'interface-gfx/guns/',
    'objects-gfx/',
    ModDir + 'objects-gfx/'
  );

  for ImageDir in ImageDirs do
  begin
    Images := PHYSFS_GetEnumeratedFiles(ImageDir);
    for Image in Images do
      if CaseInsensitiveImageMap[LowerCase(ImageDir + Image)] = '' then
        CaseInsensitiveImageMap.Add(LowerCase(ImageDir + Image), ImageDir + Image);
  end;
end;

// Handles .png override and case insensitivity.
function FindImagePath(const Filename: string): string;
var
  Orig, Png: String;
begin
  Result := Filename;
  if CaseInsensitiveImageMap = Nil then
    Exit;

  Orig := StringReplace(LowerCase(Filename), '\', '/', [rfReplaceAll]);
  Png := ChangeFileExt(Orig, '.png');

  if CaseInsensitiveImageMap[Png] <> '' then
    Result := CaseInsensitiveImageMap[Png]
  else if CaseInsensitiveImageMap[Orig] <> '' then
    Result := CaseInsensitiveImageMap[Orig]
end;

procedure LoadMainTextures();
var
  i, Count: Integer;
  Path: string;
  Color: TGfxColor;
  Scale: Single;
begin
  Count := 0;

  for i := Low(LOAD_DATA) to High(LOAD_DATA) do
  begin
    if LOAD_DATA[i].Group <> GFXG_INTERFACE then
      Inc(Count);
  end;

  MainSpritesheet := TGfxSpritesheet.Create(Count);
  Scale := 1.5 * RenderHeight / GameHeight;

  for i := Low(LOAD_DATA) to High(LOAD_DATA) do
  begin
    if LOAD_DATA[i].Group <> GFXG_INTERFACE then
    begin
      Color.r := (LOAD_DATA[i].ColorKey and $000000FF) shr 0;
      Color.g := (LOAD_DATA[i].ColorKey and $0000FF00) shr 8;
      Color.b := (LOAD_DATA[i].ColorKey and $00FF0000) shr 16;
      Color.a := (LOAD_DATA[i].ColorKey and $FF000000) shr 24;

      Path := FindImagePath(ModDir + LOAD_DATA[i].Path);

      if not PHYSFS_exists(PChar(Path)) then
        Path := FindImagePath(LOAD_DATA[i].Path);


      ImageScale[i] := GetImageScale(Path);

      if r_optimizetextures.Value then
        MainSpritesheet.AddImage(Path, Color, Scale / ImageScale[i])
      else
        MainSpritesheet.AddImage(Path, Color, 1);
    end;
  end;

  MainSpritesheet.StartLoading();
end;

procedure LoadInterfaceTextures(const InterfaceName: string);
const
  CUSTOM_FIRST = GFX_INTERFACE_CURSOR;
  CUSTOM_LAST  = GFX_INTERFACE_TITLE_R;
var
  i, Count: Integer;
  CutLength: Integer;
  Prefix: String = '';
  Path: String = '';
  Color: TGfxColor;
  Scale: Single;
  IsCustom: Boolean;
begin

  Count := 0;
  CutLength := 0;
  IsCustom := not IsDefaultInterface(InterfaceName);

  if IsCustom then
  begin
    CutLength := Length('interface-gfx/');
    Prefix := ModDir + 'custom-interfaces/' + InterfaceName + '/';
  end;

  for i := Low(LOAD_DATA) to High(LOAD_DATA) do
  begin
    if LOAD_DATA[i].Group = GFXG_INTERFACE then
      Inc(Count);
  end;

  if InterfaceSpritesheet <> nil then
    FreeAndNil(InterfaceSpritesheet);

  InterfaceSpritesheet := TGfxSpritesheet.Create(Count);

  if r_scaleinterface.Value then
    Scale := RenderHeight / GameHeight
  else
    Scale := 1;

  for i := Low(LOAD_DATA) to High(LOAD_DATA) do
  begin
    if LOAD_DATA[i].Group = GFXG_INTERFACE then
    begin
      Color.r := (LOAD_DATA[i].ColorKey and $000000FF) shr 0;
      Color.g := (LOAD_DATA[i].ColorKey and $0000FF00) shr 8;
      Color.b := (LOAD_DATA[i].ColorKey and $00FF0000) shr 16;
      Color.a := (LOAD_DATA[i].ColorKey and $FF000000) shr 24;

      if IsCustom and (i >= CUSTOM_FIRST) and (i <= CUSTOM_LAST) then
      begin
        Path := Prefix + Copy(LOAD_DATA[i].Path, CutLength + 1, MaxInt);
        Path := FindImagePath(Path);

       if not PHYSFS_exists(PChar(Path)) then
          Path := FindImagePath(LOAD_DATA[i].Path);
      end
      else
      begin
        Path := FindImagePath(ModDir + LOAD_DATA[i].Path);

        if not PHYSFS_exists(PChar(Path)) then
          Path := FindImagePath(LOAD_DATA[i].Path);
      end;

      ImageScale[i] := GetImageScale(Path);

      if r_optimizetextures.Value then
        InterfaceSpritesheet.AddImage(Path, Color, Scale / ImageScale[i])
      else
        InterfaceSpritesheet.AddImage(Path, Color, 1);
    end;
  end;

  InterfaceSpritesheet.StartLoading();
end;

procedure LoadInterface();
begin
  if LoadInterfaceData(GameRenderingParams.InterfaceName) then
    LoadInterfaceTextures(GameRenderingParams.InterfaceName)
  else
    LoadInterfaceTextures('');

  LoadedInterfaceName := GameRenderingParams.InterfaceName;
end;

function GetFontPath(FontFile: string): string; overload;
begin
  Result := '';
  if FileExists(BaseDirectory + FontFile) then
    Result := BaseDirectory + FontFile;
end;

function GetFontPath(Fallback: String; var FontFile: String): String; overload;
begin
  Result := '';

  if FontFile <> '' then
    Result := GetFontPath(FontFile);

  if Result = '' then
  begin
    Result := GetFontPath(FontFile);
  end;

  if Result = '' then
  begin
    FontFile := Fallback;
    Result := GetFontPath(FontFile);
  end;
end;

procedure LoadFonts();
var
  s: Single;
  i, w, h: Integer;
  FontFile, FontPath: array[1..2] of string;
begin
  FontFile[1] := font_1_filename.Value;
  FontFile[2] := font_2_filename.Value;

  FontPath[1] := GetFontPath(DEFAULT_FONT, FontFile[1]);
  FontPath[2] := GetFontPath(DEFAULT_FONT, FontFile[2]);

  if (FontPath[1] = '') or (FontPath[2] = '') then
  begin
    ShowMessage(_('One of the fonts cannot be found. Please check your installation directory.'));
    ShutDown;
  end;

  w := RenderWidth;
  h := RenderHeight;
  s := iif(r_scaleinterface.Value, RenderHeight / GameHeight, 1);

  Fonts[1] := GfxCreateFont(FontPath[1], Npot(w div 2), Npot(h div 2));
  Fonts[2] := GfxCreateFont(FontPath[2], Npot(w div 3), Npot(h div 3));

  FontStyles[FONT_SMALL].Font := Fonts[2];
  FontStyles[FONT_SMALL].Size := s * font_consolesize.Value;
  FontStyles[FONT_SMALL].Stretch := font_2_scale.Value / 100;
  FontStyles[FONT_SMALL].Flags := 0;

  // bold not supported for now so same as FONT_SMALL
  FontStyles[FONT_SMALL_BOLD].Font := Fonts[2];
  FontStyles[FONT_SMALL_BOLD].Size := s * font_consolesize.Value;
  FontStyles[FONT_SMALL_BOLD].Stretch := font_2_scale.Value / 100;
  FontStyles[FONT_SMALL_BOLD].Flags := 0;

  FontStyles[FONT_SMALLEST].Font := Fonts[2];
  FontStyles[FONT_SMALLEST].Size := s * font_consolesmallsize.Value;;
  FontStyles[FONT_SMALLEST].Stretch := font_2_scale.Value / 100;
  FontStyles[FONT_SMALLEST].Flags := 0;

  FontStyles[FONT_BIG].Font := Fonts[1];
  FontStyles[FONT_BIG].Size := font_bigsize.Value;
  FontStyles[FONT_BIG].Stretch := font_1_scale.Value / 100;
  FontStyles[FONT_BIG].Flags := 0;

  FontStyles[FONT_MENU].Font := Fonts[1];
  FontStyles[FONT_MENU].Size := s * font_menusize.Value;
  FontStyles[FONT_MENU].Stretch := font_1_scale.Value / 100;
  FontStyles[FONT_MENU].Flags := 0;

  FontStyles[FONT_WEAPONS_MENU].Font := Fonts[2];
  FontStyles[FONT_WEAPONS_MENU].Size := s * font_weaponmenusize.Value;;
  FontStyles[FONT_WEAPONS_MENU].Stretch := font_2_scale.Value / 100;
  FontStyles[FONT_WEAPONS_MENU].Flags := 0;

  FontStyles[FONT_WORLD].Font := Fonts[1];
  FontStyles[FONT_WORLD].Size := 128 * (RenderHeight / GameHeight);
  FontStyles[FONT_WORLD].Stretch := font_1_scale.Value / 100;
  FontStyles[FONT_WORLD].Flags := 0;

  for i := Low(FontStyles) to High(FontStyles) do
  begin
    if FontStyles[i].Size < 10 then
      FontStyles[i].Flags := GFX_MONOCHROME;

    FontStyles[i].TableIndex := GfxSetFont(FontStyles[i].Font,
      FontStyles[i].Size, FontStyles[i].Flags, FontStyles[i].Stretch);
  end;
end;

function InitGameGraphics: Boolean;
var
  WindowFlags: LongWord;
  IconFile: PSDL_RWops;
  IconFileSurface: PSDL_Surface;
  FileBuffer: PHYSFS_Buffer;
begin
  Result := True;

  CaseInsensitiveImageMap := TFPStringHashTable.Create();
  FillCaseInsensitiveImageMap();

  if Initialized then
  begin
    if GameRenderingParams.InterfaceName <> LoadedInterfaceName then
    begin
      if Assigned(ScaleData.CustomInterface) then
        FreeAndNil(ScaleData.CustomInterface);

      LoadModInfo;
      LoadInterface;
    end;

    Exit;
  end;

  WindowFlags := SDL_WINDOW_SHOWN or SDL_WINDOW_OPENGL;

  if r_fullscreen.Value = 2 then
    WindowFlags := WindowFlags or SDL_WINDOW_FULLSCREEN_DESKTOP
  else if r_fullscreen.Value = 1 then
    WindowFlags := WindowFlags or SDL_WINDOW_FULLSCREEN
  else
    WindowFlags := WindowFlags;

  // OPENGL ES3 TEST
  //SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);

  if r_msaa.Value > 0 then
  begin
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, r_msaa.Value);
  end;

  GameWindow := SDL_CreateWindow('Soldat'{$IFDEF TESTING} + ' build ' + SOLDAT_VERSION_LONG{$ENDIF},
    SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WindowWidth, WindowHeight, WindowFlags);

  FileBuffer := PHYSFS_readBuffer('icon.bmp');

  IconFile := SDL_RWFromMem(FileBuffer, Length(FileBuffer));
  IconFileSurface := SDL_LoadBMP_RW(IconFile, 1);
  SDL_SetWindowIcon(GameWindow, IconFileSurface);
  SDL_FreeSurface(IconFileSurface);

  if GameWindow = nil then
    begin
      ShowMessage('Error creating sdl2 window');
      Result := False;
      Exit;
     end;

  if not GfxInitContext(GameWindow, r_dithering.Value, r_compatibility.Value) then
  begin
    Result := False;
    Exit;
  end;

  StartInput();

  if SDL_GL_SetSwapInterval(r_swapeffect.Value) = -1 then
    GfxLog('Error while setting SDL_GL_SetSwapInterval:' + SDL_GetError());

  GfxViewport(0, 0, WindowWidth, WindowHeight);

  SetLength(Textures, GFXID_END + 1);
  LoadModInfo();

  LoadMainTextures();
  LoadInterface();
  LoadFonts();

  Map.LoadGraphics := @LoadMapGraphics;
  if not GfxFramebufferSupported then
    cl_actionsnap.SetValue(False);

  if cl_actionsnap.Value then
    ActionSnapTexture := GfxCreateRenderTarget(RenderWidth, RenderHeight, 4, True);

  if GfxFramebufferSupported then
  begin
    if (WindowWidth <> RenderWidth) or (WindowHeight <> RenderHeight) then
    begin
      RenderTarget := GfxCreateRenderTarget(RenderWidth, RenderHeight, 4, True);

      if RenderTarget.Samples > 0 then
      begin
        RenderTargetAA := GfxCreateRenderTarget(RenderWidth, RenderHeight, 4, False);

        if r_resizefilter.Value >= 2 then
          GfxTextureFilter(RenderTargetAA, GFX_LINEAR, GFX_LINEAR)
        else
          GfxTextureFilter(RenderTargetAA, GFX_NEAREST, GFX_NEAREST);
      end else
      begin
        if r_resizefilter.Value >= 2 then
          GfxTextureFilter(RenderTarget, GFX_LINEAR, GFX_LINEAR)
        else
          GfxTextureFilter(RenderTarget, GFX_NEAREST, GFX_NEAREST);
      end;
    end;
  end;

  Initialized := True;
end;

procedure ReloadGraphics;
var
  MapFile: TMapFile;
  MapInfo: TMapInfo;
  BgForce: Boolean;
  Color: array[0..1] of TMapColor;
  mg: ^TMapGraphics;
begin
  mg := @MapGfx;
  MapFile := Default(TMapFile);
  MapInfo := mg.MapInfo;
  BgForce := mg.BgForce;
  Color[0] := mg.BgForcedColor[0];
  Color[1] := mg.BgForcedColor[1];

  FreeAndNil(MainSpritesheet);
  FreeAndNil(InterfaceSpritesheet);
  DestroyMapGraphics;

  FreeAndNil(GostekData);
  FreeAndNil(ScaleData.Root);
  FreeAndNil(ScaleData.CurrentMod);
  FreeAndNil(ScaleData.CustomInterface);

  FillCaseInsensitiveImageMap();
  LoadModInfo();
  LoadMainTextures();
  LoadInterface();
  DoTextureLoading(True);

  LoadMapFile(MapInfo, MapFile);
  LoadMapGraphics(MapFile, BgForce, Color[0], Color[1]);
end;

procedure DestroyGameGraphics();
var
  i: Integer;
begin
  if Initialized = False then
    Exit;

  FreeAndNil(MainSpritesheet);
  FreeAndNil(InterfaceSpritesheet);

  for i := Low(Fonts) to High(Fonts) do
    GfxDeleteFont(Fonts[i]);

  if ActionSnapTexture <> nil then
    GfxDeleteTexture(ActionSnapTexture);

  if RenderTarget <> nil then
    GfxDeleteTexture(RenderTarget);

  if RenderTargetAA <> nil then
    GfxDeleteTexture(RenderTargetAA);

  FreeAndNil(CaseInsensitiveImageMap);
  DestroyMapGraphics();
  GfxDestroyContext();

  Initialized := False;
end;

function Lerp(a, b, x: Single): Single; overload;
begin
  Result := a + (b - a) * x;
end;

function Lerp(a, b: TVector2; x: Single): TVector2; overload;
begin
  Result.x := a.x + (b.x - a.x) * x;
  Result.y := a.y + (b.y - a.y) * x;
end;

procedure InterpolateState(p: Extended; var s: TInterpolationState; Paused: Boolean);
const
  KIT_STYLES = [
    OBJECT_MEDICAL_KIT,
    OBJECT_GRENADE_KIT,
    OBJECT_FLAMER_KIT,
    OBJECT_PREDATOR_KIT,
    OBJECT_VEST_KIT,
    OBJECT_BERSERK_KIT,
    OBJECT_CLUSTER_KIT
  ];
var
  i, j: Integer;
  sk: ^ParticleSystem;
  Gun: ^TGun;
begin
  s.Camera.x := CameraX;
  s.Camera.y := CameraY;
  s.Mouse.x := mx;
  s.Mouse.y := my;

  CameraX := Lerp(CameraPrev.x, CameraX, p);
  CameraY := Lerp(CameraPrev.y, CameraY, p);
  mx := Lerp(MousePrev.x, mx, p);
  my := Lerp(MousePrev.y, my, p);

  if Paused then
    p := 1.0;

  for i := 1 to MAX_SPRITES do
  begin
    if Sprite[i].Active then
    begin
      sk := @Sprite[i].Skeleton;
      Gun := @Sprite[i].Weapon;

      Move(sk.Pos[1], s.SpritePos[i][1], sizeof(TVector2) * Length(s.SpritePos[i]));

      for j := Low(s.SpritePos[i]) to High(s.SpritePos[i]) do
        sk.Pos[j] := Lerp(sk.OldPos[j], sk.Pos[j], p);

      Gun.ReloadTimeReal := Lerp(Gun.ReloadTimePrev, Gun.ReloadTimeCount, p);
      Gun.FireIntervalReal := Lerp(Gun.FireIntervalPrev, Gun.FireIntervalCount, p);
      Sprite[i].JetsCountReal := Lerp(Sprite[i].JetsCountPrev, Sprite[i].JetsCount, p);
    end;
  end;

  for i := 1 to MAX_BULLETS do
  begin
    if Bullet[i].Active or (Bullet[i].PingAdd > 0) then
    begin
      j := Bullet[i].Num;

      s.BulletPos[i] := BulletParts.Pos[j];
      s.BulletVel[i] := BulletParts.Velocity[j];
      s.BulletHitMul[i] := Bullet[i].HitMultiply;

      BulletParts.Pos[j] := Lerp(BulletParts.OldPos[j], BulletParts.Pos[j], p);
      BulletParts.Velocity[j] := Lerp(Bullet[i].VelocityPrev, BulletParts.Velocity[j], p);
      Bullet[i].HitMultiply := Lerp(Bullet[i].HitMultiplyPrev, Bullet[i].HitMultiply, p);
      Bullet[i].TimeOutReal := Lerp(Bullet[i].TimeOutPrev, Bullet[i].TimeOut, p);
    end;
  end;

  for i := 1 to MAX_SPARKS do
  begin
    if Spark[i].Active then
    begin
      j := Spark[i].Num;
      s.SparkPos[i] := SparkParts.Pos[j];
      SparkParts.Pos[j] := Lerp(SparkParts.OldPos[j], SparkParts.Pos[j], p);
      Spark[i].LifeReal := Lerp(Spark[i].LifePrev, Spark[i].Life, p);
    end;
  end;

  for i := 1 to MAX_THINGS do
  begin
    if Thing[i].Active then
    begin
      sk := @Thing[i].Skeleton;
      Move(sk.Pos[1], s.ThingPos[i][1], sizeof(TVector2) * Length(s.ThingPos[i]));

      for j := Low(s.ThingPos[i]) to High(s.ThingPos[i]) do
        sk.Pos[j] := Lerp(sk.OldPos[j], sk.Pos[j], p);

      if Thing[i].Style in KIT_STYLES then
        sk.SatisfyConstraints();
    end;
  end;
end;

procedure RestoreState(var s: TInterpolationState);
var
  i: Integer;
begin
  CameraX := s.Camera.x;
  CameraY := s.Camera.y;
  mx := s.Mouse.x;
  my := s.Mouse.y;

  for i := 1 to MAX_SPRITES do
  begin
    if Sprite[i].Active then
    begin
      Move(s.SpritePos[i][1], Sprite[i].Skeleton.Pos[1],
        sizeof(TVector2) * Length(s.SpritePos[i]));
    end;
  end;

  for i := 1 to MAX_BULLETS do
  begin
    if Bullet[i].Active or (Bullet[i].PingAdd > 0) then
    begin
      BulletParts.Pos[Bullet[i].Num] := s.BulletPos[i];
      BulletParts.Velocity[Bullet[i].Num] := s.BulletVel[i];
      Bullet[i].HitMultiply := s.BulletHitMul[i];
    end;
  end;

  for i := 1 to MAX_SPARKS do
  begin
    if Spark[i].Active then
      SparkParts.Pos[Spark[i].Num] := s.SparkPos[i];
  end;

  for i := 1 to MAX_THINGS do
  begin
    if Thing[i].Active then
    begin
      Move(s.ThingPos[i][1], Thing[i].Skeleton.Pos[1],
        sizeof(TVector2) * Length(s.ThingPos[i]));
    end;
  end;
end;

procedure RenderFrame(TimeElapsed, FramePercent: Extended; Paused: Boolean);
var
  mg: ^TMapGraphics;
  i: Integer;
  dx, dy: Single;
  w, h, s, u, v: Single;
  InterpolationState: TInterpolationState;
  GrabActionSnap: Boolean;
  Rc: TRect;
  Rt: TGfxTexture;
begin
  mg := @MapGfx;

  // graphics might be destroyed before end of game loop
  if mg.VertexBuffer = nil then
    Exit;

  if RenderTarget <> nil then
  begin
    GfxTarget(RenderTarget);
    GfxViewport(0, 0, RenderWidth, RenderHeight);
  end;

  if ScreenshotPath <> '' then
  begin
    GfxSaveScreen(ScreenshotPath, 0, 0, RenderWidth, RenderHeight, ScreenshotAsync);
    ScreenshotPath := '';
  end;

  if ShowScreen and ActionSnapTaken then
  begin
    Rc := Rect(0, RenderHeight, RenderWidth, 0);
    GfxBlit(ActionSnapTexture, RenderTarget, Rc, Rc, GFX_NEAREST);
    GfxTarget(RenderTarget);

    w := RenderWidth;
    h := RenderHeight;

    if r_scaleinterface.Value then
    begin
      w := GameWidth;
      h := GameHeight;
    end;

    GfxBegin();
    GfxTransform(GfxMat3Ortho(0, w, 0, h));
    GfxTextPixelRatio(Vector2(w / RenderWidth, h / RenderHeight));
    RenderActionSnapText(TimeElapsed);
    GfxEnd();
  end
  else
  begin
    GrabActionSnap := False;

    if (cl_actionsnap.Value) and (CapScreen = 0) then
    begin
      CapScreen := 255;
      GrabActionSnap := True;
      ActionSnapTaken := True;
      GfxTarget(ActionSnapTexture);
    end
    else if CapScreen <> 255 then
    begin
      Dec(CapScreen);
    end;

    InterpolationState := Default(TInterpolationState);
    InterpolateState(FramePercent, InterpolationState, Paused);

    w := exp(r_zoom.Value) * GameWidth;
    h := exp(r_zoom.Value) * GameHeight;

    dx := CameraX - w / 2;
    dy := CameraY - h / 2;

    if CameraY > 0 then
      GfxClear(MapGfx.BgColorBtm)
    else
      GfxClear(MapGfx.BgColorTop);

    if r_animations.Value then
      UpdateProps(TimeElapsed);

    GfxTransform(GfxMat3Ortho(0, 1, dy, h + dy));
    GfxBindTexture(nil);
    GfxDraw(mg.VertexBuffer, mg.Background, mg.BackgroundCount);

    GfxTransform(GfxMat3Ortho(dx, w + dx, dy, h + dy));

    if r_smoothedges.Value and (Length(mg.Edges[0]) > 0) then
      GfxDraw(mg.VertexBuffer, mg.IndexBuffer, @mg.Edges[0][0], Length(mg.Edges[0]));

    if Length(mg.Polys[0]) > 0 then
      GfxDraw(mg.VertexBuffer, @mg.Polys[0][0], Length(mg.Polys[0]));

    GfxSetMipmapBias(r_mipmapbias.Value);

    if r_renderbackground.Value then
      RenderProps(0);

    GfxBegin();

    for i := 1 to MAX_BULLETS do
      if Bullet[i].Active or (Bullet[i].PingAdd > 0) then
        Bullet[i].Render(TimeElapsed);

    for i := 1 to MAX_SPRITES do
      if Sprite[i].Active then
        RenderGostek(Sprite[i]);

    for i := 1 to MAX_THINGS do
      if Thing[i].Active then
        Thing[i].Render(TimeElapsed);

    for i := 1 to MAX_SPARKS do
      if Spark[i].Active then
        Spark[i].Render();

    GfxEnd();
    RenderProps(1);
    GfxBegin();

    for i := 1 to MAX_THINGS do
      if Thing[i].Active then
        Thing[i].PolygonsRender();

    GfxEnd();
    GfxSetMipmapBias(0);

    if r_smoothedges.Value and (Length(mg.Edges[1]) > 0) then
      GfxDraw(mg.VertexBuffer, mg.IndexBuffer, @mg.Edges[1][0], Length(mg.Edges[1]));

    if Length(mg.Polys[1]) > 0 then
      GfxDraw(mg.VertexBuffer, @mg.Polys[1][0], Length(mg.Polys[1]));

    GfxSetMipmapBias(r_mipmapbias.Value);
    RenderProps(2);
    GfxSetMipmapBias(0);

    if not r_scaleinterface.Value then
    begin
      w := RenderWidth;
      h := RenderHeight;
    end;

    if GrabActionSnap then
    begin
      Rc := Rect(0, RenderHeight, RenderWidth, 0);
      GfxBlit(ActionSnapTexture, RenderTarget, Rc, Rc, GFX_NEAREST);
      GfxTarget(RenderTarget);
    end;

    if r_renderui.Value then
    begin
      GfxBegin();
      GfxTransform(GfxMat3Ortho(0, w, 0, h));
      RenderInterface(TimeElapsed, w, h);
      GfxEnd();
    end;

    RestoreState(InterpolationState);
  end;

  if RenderTarget <> nil then
  begin
    Rt := RenderTarget;

    if RenderTargetAA <> nil then
    begin
      Rc := Rect(0, RenderHeight, RenderWidth, 0);
      GfxBlit(RenderTarget, RenderTargetAA, Rc, Rc, GFX_NEAREST);
      Rt := RenderTargetAA;
    end;

    if (ScreenWidth / ScreenHeight) >= (RenderWidth / RenderHeight) then
    begin
      w := ScreenHeight * (RenderWidth / RenderHeight);
      h := ScreenHeight;
    end
    else
    begin
      w := ScreenWidth;
      h := ScreenWidth * (RenderHeight / RenderWidth);
    end;

    dx := Floor(0.5 * (ScreenWidth - w));
    dy := Floor(0.5 * (ScreenHeight - h));

    if ScreenWidth <> WindowWidth then
    begin
      s := WindowWidth / ScreenWidth;
      w := w * s;
      dx := (dx - ScreenWidth / 2) * s + WindowWidth / 2;
    end;

    if ScreenHeight <> WindowHeight then
    begin
      s := WindowHeight / ScreenHeight;
      h := h * s;
      dy := (dy - ScreenHeight / 2) * s + WindowHeight / 2;
    end;

    GfxTarget(nil);
    GfxViewport(0, 0, WindowWidth, WindowHeight);
    GfxClear(RGBA(0));
    GfxTransform(GfxMat3Ortho(0, WindowWidth, 0, WindowHeight));

    u := RenderWidth / RenderTarget.Width;
    v := RenderHeight / RenderTarget.Height;

    GfxBegin();
    GfxDrawQuad(Rt,
      GfxVertex(dx + 0, dy + 0, 0, v, RGBA($FFFFFF)),
      GfxVertex(dx + w, dy + 0, u, v, RGBA($FFFFFF)),
      GfxVertex(dx + w, dy + h, u, 0, RGBA($FFFFFF)),
      GfxVertex(dx + 0, dy + h, 0, 0, RGBA($FFFFFF))
    );
    GfxEnd();
  end;

  GfxPresent(r_glfinish.Value);
end;

procedure RenderGameInfo(TextString: WideString);
var
  rc: TGfxRect;
begin
  GfxTarget(nil);
  GfxViewport(0, 0, WindowWidth, WindowHeight);
  GfxTransform(GfxMat3Ortho(0, WindowWidth, 0, WindowHeight));
  GfxClear(49, 61, 79, 255);
  SetFontStyle(FONT_MENU);
  GfxTextColor(RGBA($FFFFFF));
  GfxTextShadow(1, 1, RGBA(0));
  GfxTextPixelRatio(Vector2(1, 1));
  rc := GfxTextMetrics(TextString);
  GfxBegin();
  GfxDrawText((WindowWidth - RectWidth(rc)) / 2, (WindowHeight - RectHeight(rc)) / 2);
  SetFontStyle(FONT_SMALL);
  rc := GfxTextMetrics(_('Press ESC to quit the game'));
  GfxDrawText((WindowWidth - RectWidth(rc)) / 2, ((WindowHeight - RectHeight(rc)) / 2) + 100);
  GfxEnd();
  GfxPresent(True);
end;

function ArrayContains(const List: array of Integer; x: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := Low(List) to High(List) do
  begin
    if List[i] = x then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function GetSizeConstraint(id: Integer; var w, h: Integer): Boolean;
const
  WEAPONS_LIST: array[1..35] of Integer = (
    GFX_WEAPONS_AK74,        GFX_WEAPONS_AK74_2,
    GFX_WEAPONS_AK74_FIRE,   GFX_WEAPONS_MINIMI,
    GFX_WEAPONS_MINIMI_2,    GFX_WEAPONS_RUGER,
    GFX_WEAPONS_RUGER_2,     GFX_WEAPONS_MP5,
    GFX_WEAPONS_MP5_2,       GFX_WEAPONS_SPAS,
    GFX_WEAPONS_SPAS_2,      GFX_WEAPONS_M79,
    GFX_WEAPONS_M79_2,       GFX_WEAPONS_DEAGLES,
    GFX_WEAPONS_DEAGLES_2,   GFX_WEAPONS_N_DEAGLES,
    GFX_WEAPONS_N_DEAGLES_2, GFX_WEAPONS_STEYR,
    GFX_WEAPONS_STEYR_2,     GFX_WEAPONS_BARRETT,
    GFX_WEAPONS_BARRETT_2,   GFX_WEAPONS_MINIGUN,
    GFX_WEAPONS_MINIGUN_2,   GFX_WEAPONS_SOCOM,
    GFX_WEAPONS_SOCOM_2,     GFX_WEAPONS_N_SOCOM,
    GFX_WEAPONS_N_SOCOM_2,   GFX_WEAPONS_BOW,
    GFX_WEAPONS_BOW_S,       GFX_WEAPONS_FLAMER,
    GFX_WEAPONS_FLAMER_2,    GFX_WEAPONS_KNIFE,
    GFX_WEAPONS_KNIFE2,      GFX_WEAPONS_CHAINSAW,
    GFX_WEAPONS_CHAINSAW2
  );
begin
  Result := False;

  if (id >= GFX_GOSTEK_STOPA) and (id <= GFX_GOSTEK_TEAM2_LECISTOPA2) then
  begin
    w := GOS_RESTRICT_WIDTH;
    h := GOS_RESTRICT_HEIGHT;
    Result := True;
  end
  else if ArrayContains(WEAPONS_LIST, id) then
  begin
    w := WEP_RESTRICT_WIDTH;
    h := WEP_RESTRICT_HEIGHT;
    Result := True;
  end;
end;

function DoTextureLoading(FinishLoading: Boolean = False): Boolean;
var
  i, j: Integer;
  w: Integer = 0;
  h: Integer = 0;
  MainLoading, InterfaceLoading: Boolean;
  s: string;
begin
  Result := True; // return true when not loading

  if (MainSpritesheet = nil) or (InterfaceSpritesheet = nil) then
    Exit;

  MainLoading := MainSpritesheet.Loading;
  InterfaceLoading := InterfaceSpritesheet.Loading;

  if not (MainLoading or InterfaceLoading) then
    Exit;

  if FinishLoading then
  begin
    MainSpritesheet.FinishLoading();
    InterfaceSpritesheet.FinishLoading();
  end
  else
  begin
    if MainSpritesheet.Loading then
      MainSpritesheet.ContinueLoading()
    else if InterfaceSpritesheet.Loading then
      InterfaceSpritesheet.ContinueLoading();
  end;

  if MainSpritesheet.Loading <> MainLoading then
  begin
    j := 0;

    for i := Low(LOAD_DATA) to High(LOAD_DATA) do
    begin
      if LOAD_DATA[i].Group <> GFXG_INTERFACE then
      begin
        Textures[i] := MainSpritesheet[j];
        Textures[i].Scale := Textures[i].Scale * (1 / ImageScale[i]);

        if GetSizeConstraint(i, w, h) then
        begin
          if ((Textures[i].Width * Textures[i].Scale) > w) or
            ((Textures[i].Height * Textures[i].Scale) > h) then
          begin
            if (Textures[i].Width / Textures[i].Height) > (w / h) then
              Textures[i].Scale := w / Textures[i].Width
            else
              Textures[i].Scale := h / Textures[i].Height;
          end;
        end;

        Inc(j);
      end;
    end;

    s := '';

    for i := 0 to MainSpritesheet.TextureCount - 1 do
    begin
      s := s + Format('%dx%d ', [
        MainSpritesheet.Texture[i].Width,
        MainSpritesheet.Texture[i].Height
      ]);

      SetTextureFilter(MainSpritesheet.Texture[i], True);
    end;

    s[Length(s)] := ')';
    GfxLog('Loaded main spritesheet (' + s);

    ApplyGostekConstraints();
  end;

  if InterfaceSpritesheet.Loading <> InterfaceLoading then
  begin
    j := 0;

    for i := Low(LOAD_DATA) to High(LOAD_DATA) do
    begin
      if LOAD_DATA[i].Group = GFXG_INTERFACE then
      begin
        Textures[i] := InterfaceSpritesheet[j];
        Textures[i].Scale := Textures[i].Scale * (1 / ImageScale[i]);
        Inc(j);
      end;
    end;

    s := '';

    for i := 0 to InterfaceSpritesheet.TextureCount - 1 do
    begin
      s := s + Format('%dx%d ', [
        InterfaceSpritesheet.Texture[i].Width,
        InterfaceSpritesheet.Texture[i].Height
      ]);

      SetTextureFilter(InterfaceSpritesheet.Texture[i], False);
    end;

    s[Length(s)] := ')';
    GfxLog('Loaded interface spritesheet (' + s);
  end;

  Result := False;

  if not (MainSpritesheet.Loading or InterfaceSpritesheet.Loading) then
    Result := True;
end;

procedure SetFontStyle(Style: Integer);
begin
  GfxSetFontTable(FontStyles[Style].Font, FontStyles[Style].TableIndex);
end;

procedure SetFontStyle(Style: Integer; Scale: Single);
begin
  GfxSetFont(FontStyles[Style].Font, Scale * FontStyles[Style].Size,
    FontStyles[Style].Flags, FontStyles[Style].Stretch);
end;

function FontStyleSize(Style: Integer): Single;
begin
  Result := FontStyles[Style].Size;
end;

procedure GfxLogCallback(s: string);
begin
  Debug('[GFX] ' + s);
end;

initialization
  GfxLog := @GfxLogCallback;
end.

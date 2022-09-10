{*******************************************************}
{                                                       }
{       Main Unit for OPENSOLDAT                        }
{                                                       }
{       Copyright (c) 2003 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Client;

interface

uses
  // system and delphi units
  {$IFDEF MSWINDOWS}
  MMSystem,
  {$ENDIF}
  SysUtils, Classes, Math, GameStrings, Variants, FileClient,

  // graphics units
  Gfx,

  // helper units
  Version, Vector, Util, Sha1,

  // SDL2
  SDL2,

  {$IFDEF STEAM}
  // Steam
  Steam,
  {$ENDIF}

  // PhysFS
  PhysFS,

  // Cvar
  Cvar, Command, ClientCommands,

  // anti-cheat units
  {$IFDEF ENABLE_FAE}FaeClient,{$ENDIF}

  // OpenSoldat units
  Sprites, Anims, PolyMap, Net, LogFile, Sound, GetText,
  NetworkClientConnection, GameMenus, Demo, Console,
  Weapons, Constants, Game, GameRendering, ClientLauncherIPC;

procedure JoinServer;
procedure StartGame;
procedure ShutDown;
procedure ExitToMenu;
procedure RestartGraph;
procedure ShowMessage(MessageText: AnsiString); overload;
procedure ShowMessage(MessageText: WideString); overload;
// TODO: Make this part of a cleaner "LoadMod".
procedure LoadWeaponNames();

type
  TWeaponStat = record
    Name: string;
    TextureID: LongWord;
    Shots, Hits, Kills, Headshots: Cardinal;
    Accuracy: Byte;
  end;

var
  GameLoopRun: Boolean;
  ProgReady: Boolean;

  JoinPassword: String; // server password
  JoinPort: String; // join port to server
  JoinIP: String; // join ip to server

  WindowReady: Boolean = False;
  Initing: Byte;
  GraphicsInitialized: Boolean = False;

  BaseDirectory: string;
  UserDirectory: string;

  ModDir: string = '';
  UsesServerMod: Boolean;

  // Cvars
  log_enable: TBooleanCvar;
  log_level: TIntegerCvar;
  log_filesupdate: TIntegerCvar;

  fs_localmount: TBooleanCvar;
  fs_mod: TStringCvar;
  fs_portable: TBooleanCvar;
  fs_basepath: TStringCvar;
  fs_userpath: TStringCvar;

  r_fullscreen: TIntegerCvar;
  r_weathereffects: TBooleanCvar;
  r_dithering: TBooleanCvar;
  r_swapeffect: TIntegerCvar;
  r_compatibility: TBooleanCvar;
  r_texturefilter: TIntegerCvar;
  r_optimizetextures: TBooleanCvar;
  r_mipmapping: TBooleanCvar;
  r_mipmapbias: TSingleCvar;
  r_glfinish: TBooleanCvar;
  r_smoothedges: TBooleanCvar;
  r_scaleinterface: TBooleanCvar;
  r_maxsparks: TIntegerCvar;
  r_animations: TBooleanCvar;
  r_renderbackground: TBooleanCvar;
  r_maxfps: TIntegerCvar;
  r_fpslimit: TBooleanCvar;
  r_resizefilter: TIntegerCvar;
  r_sleeptime: TIntegerCvar;
  r_screenwidth: TIntegerCvar;
  r_screenheight: TIntegerCvar;
  r_renderwidth: TIntegerCvar;
  r_renderheight: TIntegerCvar;
  r_forcebg: TBooleanCvar;
  r_forcebg_color1: TColorCvar;
  r_forcebg_color2: TColorCvar;
  r_renderui: TBooleanCvar;
  r_zoom: TSingleCvar;
  r_msaa: TIntegerCVar;

  ui_playerindicator: TBooleanCvar;
  ui_minimap_transparency: TIntegerCvar;
  ui_minimap_posx: TIntegerCvar;
  ui_minimap_posy: TIntegerCvar;
  ui_bonuscolors: TBooleanCvar;
  ui_style: TStringCvar;
  ui_status_transparency: TIntegerCvar;
  ui_console: TBooleanCvar;
  ui_console_length: TIntegerCvar;
  ui_killconsole: TBooleanCvar;
  ui_killconsole_length: TIntegerCvar;
  ui_hidespectators: TBooleanCvar;
  ui_sniperline: TBooleanCvar;

  cl_sensitivity: TSingleCvar;
  cl_endscreenshot: TBooleanCvar;
  cl_actionsnap: TBooleanCvar;
  cl_screenshake: TBooleanCvar;
  cl_servermods: TBooleanCvar;

  {$IFDEF STEAM}
  cl_steam_screenshots: TBooleanCvar;
  cl_voicechat: TBooleanCvar;
  fs_workshop_mod: TIntegerCvar;
  fs_workshop_interface: TIntegerCvar;
  {$ENDIF}

  cl_player_name: TStringCvar;
  cl_player_team: TIntegerCvar;
  cl_player_shirt: TColorCvar;
  cl_player_pants: TColorCvar;
  cl_player_hair: TColorCvar;
  cl_player_jet: TColorCvar;
  cl_player_skin: TColorCvar;

  cl_player_hairstyle: TIntegerCvar;
  cl_player_headstyle: TIntegerCvar;
  cl_player_chainstyle: TIntegerCvar;
  cl_player_secwep: TIntegerCvar;
  cl_player_wep: TIntegerCvar;

  cl_runs: TIntegerCvar;
  cl_lang: TStringCvar;

  demo_speed: TSingleCvar;
  demo_rate: TIntegerCvar;
  demo_showcrosshair: TBooleanCvar;
  demo_autorecord: TBooleanCvar;

  snd_volume: TIntegerCvar;
  snd_effects_battle: TBooleanCvar;
  snd_effects_explosions: TBooleanCvar;

  font_1_name: TStringCvar;
  font_1_filename: TStringCvar;
  font_1_scale: TIntegerCvar;
  font_2_name: TStringCvar;
  font_2_filename: TStringCvar;
  font_2_scale: TIntegerCvar;

  font_menusize: TIntegerCvar;
  font_consolesize: TIntegerCvar;
  font_consolesmallsize: TIntegerCvar;
  font_consolelineheight: TSingleCvar;
  font_bigsize: TIntegerCvar;
  font_weaponmenusize: TIntegerCvar;
  font_killconsolenamespace: TIntegerCvar;

  launcher_ipc_enable: TBooleanCvar;
  launcher_ipc_port: TIntegerCvar;
  launcher_ipc_reconnect_rate: TIntegerCvar;

  sv_respawntime: TIntegerCvar; // TODO: Remove
  sv_inf_redaward: TIntegerCvar; // TODO: Remove
  net_contype: TIntegerCvar; // TODO: Remove
  net_allowdownload: TBooleanCvar;

  // syncable cvars
  sv_gamemode: TIntegerCvar;
  sv_friendlyfire: TBooleanCvar;
  sv_timelimit: TIntegerCvar;
  sv_maxgrenades: TIntegerCvar;
  sv_bullettime: TBooleanCvar;
  sv_sniperline: TBooleanCvar;
  sv_balanceteams: TBooleanCvar;
  sv_guns_collide: TBooleanCvar;
  sv_kits_collide: TBooleanCvar;
  sv_survivalmode: TBooleanCvar;
  sv_survivalmode_antispy: TBooleanCvar;
  sv_survivalmode_clearweapons: TBooleanCvar;
  sv_realisticmode: TBooleanCvar;
  sv_advancemode: TBooleanCvar;
  sv_advancemode_amount: TIntegerCvar;
  sv_minimap_locations: TBooleanCvar;
  sv_advancedspectator: TBooleanCvar;
  sv_radio: TBooleanCvar;
  sv_info: TStringCvar;
  sv_gravity: TSingleCvar;
  sv_hostname: TStringCvar;
  sv_killlimit: TIntegerCvar;
  sv_downloadurl: TStringCvar;
  sv_pure: TBooleanCvar;
  sv_website: TStringCvar;

  ServerIP: string = '127.0.0.1';
  ServerPort: Integer = 23073;

  Grav: Single = 0.06;
  Connection: Byte = INTERNET;

  WeaponActive: array[1..MAIN_WEAPONS] of Byte; // sync
  WeaponsInGame: Integer; // sync

  Trails: Byte = 1;
  Spectator: Byte = 0;  // TODO: Remove

  PacketAdjusting: Byte = 1;

  LimboLock: Boolean;
  SelTeam: Byte;

  MySprite: Byte;

  // Network
  UDP: TClientNetwork;
  LauncherIPC: TClientLauncherIPC;

  // Consoles
  MainConsole: TConsole;
  BigConsole: TConsole;
  KillConsole: TConsole;

  // Weapon Stats
  WepStats: array[0..20] of TWeaponStat;
  WepStatsNum: Byte = 0;

  GunDisplayName: array[0..16] of string;

  GameThingTarget: Byte;
  GrenadeEffectTimer: Integer = 0;

  BadMapIDCount: Byte;

  AbnormalTerminate: Boolean = False;

  HWID: string;

  HitSprayCounter: Word;
  ScreenTaken: Boolean;

  // bullet shot stats
  ShotDistanceShow: Integer;
  ShotDistance: Single;
  ShotLife: Single;
  ShotRicochet: Integer;

  TargetMode: Boolean = False;

  MuteAll: Boolean = False;

  RedirectToServer: Boolean = False;
  RedirectIP: Shortstring;
  RedirectPort: Integer;
  RedirectMsg: string;

  // Radio Menu
  RadioMenu: TStringList;
  RMenuState: array[0..1] of Char = '  ';
  ShowRadioMenu: Boolean = False;
  RadioCooldown: Byte = 3;

  // screen
  CameraPrev: TVector2;
  CameraX, CameraY: Single;  // camera x and y within world
  CameraFollowSprite: Byte;  // Tag number of object to follow

  DownloadThread: TDownloadThread;
  {$IFDEF STEAM}
  SteamAPI: TSteam;
  //SteamCallbacks: TSteamCallbacks;
  VoiceSpeakingNow: Boolean;
  ForceReconnect: Boolean;
  {$ENDIF}

implementation

uses
  IniFiles, TraceLog, ClientGame, ControlGame, InterfaceGraphics, Input;

procedure RestartGraph;
begin
  WindowReady := False;

  DoTextureLoading(True);

  // Load Map
  Map.LoadMap(MapChange, r_forcebg.Value, r_forcebg_color1.Value, r_forcebg_color2.Value);

  WindowReady := True;

  if not EscMenu.Active then
  begin
    mx := GameWidthHalf;
    my := GameHeightHalf;
    MousePrev.x := mx;
    MousePrev.y := my;
  end;

  MainConsole.Console(_('Graphics restart'), DEBUG_MESSAGE_COLOR);
end;

procedure LoadWeaponNames();
var
  TF: PHYSFS_File;
  i: Integer;
  Prefix: AnsiString;
begin
  if PHYSFS_Exists(PChar(ModDir + 'txt/weaponnames.txt')) then
    Prefix := ModDir
  else
    Prefix := '';

  MainConsole.Console(_('Loading Weapon Names from') + WideString(' ' + Prefix + 'txt/weaponnames.txt'), DEBUG_MESSAGE_COLOR);
  TF := PHYSFS_openRead(PChar(Prefix + 'txt/weaponnames.txt'));
  if TF <> nil then
    for i := 0 to EXTENDED_WEAPONS - 1 do
      PhysFS_ReadLN(TF, GunDisplayName[WeaponNumExternalToInternal(i)]);
  PHYSFS_close(TF);
end;

procedure RedirectDialog;
var
  Buttons: array[0..1] of TSDL_MessageBoxButtonData;
  Data: TSDL_MessageBoxData;
  Response: LongInt;
begin
  RenderGameInfo(_('Server Redirect'));
  Buttons[0].flags := SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;
  Buttons[0].buttonid := 0;
  Buttons[0].text := 'Yes';
  Buttons[1].flags := SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;
  Buttons[1].buttonid := 1;
  Buttons[1].text := 'No';

  Data.Flags := 0;
  Data.Window := GameWindow;
  Data.Title := 'Server Redirect';
  Data._message := PChar(RedirectMsg + Chr(13) + Chr(10) + Chr(13) + Chr(10) +
   'Redirect to server ' + RedirectIP + ':' + IntToStr(RedirectPort) + '?');
  Data.numbuttons := 2;
  Data.buttons := @Buttons;
  Data.colorScheme := nil;

  if SDL_ShowMessageBox(@Data, @Response) <> 0 then
    Exit;

  RedirectToServer := False;

  if Response = 0 then
  begin
    JoinIP := RedirectIP;
    JoinPort := IntToStr(RedirectPort);
    JoinServer();
  end else
  begin
    RedirectIP := '';
    RedirectPort := 0;
    RedirectMsg := '';
    ExitToMenu;
  end;
end;

procedure ExitToMenu;
var
  i: Integer;
begin
  GOALTICKS := DEFAULT_GOALTICKS;

  // Reset network state and show the status string (if any)
  ShouldRenderFrames := False;
  //NetEncActive := False;
  ResetSyncCvars;

  if DemoRecorder.Active then
    DemoRecorder.StopRecord;

  if DemoPlayer.Active then
    DemoPlayer.StopDemo;

  if MySprite > 0 then
    ClientDisconnect
  else
    if Assigned(UDP) then
      UDP.Disconnect(true);

  StopSound(CHANNEL_WEATHER);

  Map.Name := '';

  if Assigned(EscMenu) then
    GameMenuShow(EscMenu, False);

  Map.Filename := '';  // force reloading next time
  MapChangeCounter := -60;
  //WindowReady := False;

  for i := 1 to MAX_SPRITES do
    if Sprite[i].Active then
      Sprite[i].Kill;
  for i := 1 to MAX_BULLETS do
      Bullet[i].Kill;
  for i := 1 to MAX_SPARKS do
      Spark[i].Kill;
  for i := 1 to MAX_THINGS do
      Thing[i].Kill;

  // Reset World and Big Texts
  for I := 0 to MAX_BIG_MESSAGES do
  begin
    // Big Text
    BigText[I] := '';
    BigDelay[I] := 0;
    BigScale[I] := 0;
    BigColor[I] := 0;
    BigPosX[I] := 0;
    BigPosY[I] := 0;
    BigX[I] := 0;
    // World Text
    WorldText[I] := '';
    WorldDelay[I] := 0;
    WorldScale[I] := 0;
    WorldColor[I] := 0;
    WorldPosX[I] := 0;
    WorldPosY[I] := 0;
    WorldX[I] := 0;
  end;

  // Reset ABOVE CHAT MESSAGE
  for I := 1 to MAX_SPRITES do
  begin
    ChatDelay[I] := 0;
    ChatMessage[I] := '';
    ChatTeam[I] := False;
  end;

  MySprite := 0;
  CameraFollowSprite := 0;
  GameThingTarget := 0;

  if RedirectToServer then
    RedirectDialog;
end;


{$IFDEF STEAM}
procedure OnScreenshotReady(Event: PScreenshotReady_t); cdecl;
var
  i: Byte;
begin
  if Event.m_eResult = k_EResultOK then
  begin
    // Set map name as location
    SteamAPI.Screenshots.SetLocation(Event.m_hLocal, PChar(AnsiString(Map.Name)));
    SteamAPI.Screenshots.TagPublishedFile(Event.m_hLocal, Map.MapInfo.WorkshopID);
    // Tag all visible players
    for i := 1 to MAX_SPRITES do
    begin
      if Sprite[i].Active and (Sprite[i].Player.ControlMethod = HUMAN) and (UInt64(Sprite[i].Player.SteamID) > 0) then
        if PointVisible(SpriteParts.Pos[i].X, SpriteParts.Pos[i].Y, Sprite[MySprite].Player.Camera) then
          SteamAPI.Screenshots.TagUser(Event.m_hLocal, Sprite[i].Player.SteamID);
    end;
    MainConsole.Console(_('Succesfully saved screenshot on Steam'), DEBUG_MESSAGE_COLOR);
  end else
    MainConsole.Console(_('Failed to save screenshot on Steam'), DEBUG_MESSAGE_COLOR);
end;

procedure LoadWorkshopModArchives;
var
  SizeOnDisk: uint64;
  Path: array[0..4096] of Char;
  TimeStamp: Cardinal;
  Sr: TSearchRec;
  Name: String;
begin
  if SteamAPI.UGC.GetItemInstallInfo(fs_workshop_mod.Value, @SizeOnDisk, @Path, 1024, @TimeStamp) then
  begin
    if FindFirst(Path + '/*.smod', faAnyFile - faDirectory, sr) = 0 then
    begin
      Name := Sr.Name;
      Name := Name.SubString(0, Name.Length - 5);
      Debug('[PhysFS] Mounting mods/' + LowerCase(Sr.Name));
      if not PhysFS_mount(PChar(Path + '/' + Sr.Name),
        PChar('mods/' + LowerCase(Name) + '/'), False) then
      begin
        ShowMessage(_('Could not load mod archive (' + IntToStr(fs_workshop_mod.Value) + '/' + Sr.Name + ').'));
        Exit;
      end;
      ModDir := 'mods/' + LowerCase(Name) + '/';
      CustomModChecksum := Sha1File(Path + '/' + Sr.Name, 4096);
    end;
    FindClose(Sr);
  end else
    Debug('Failed to load mod archive from workshop - ' + IntToStr(fs_workshop_mod.Value));
end;

procedure LoadWorkshopInterfaceArchives;
var
  SizeOnDisk: uint64;
  Path: array[0..4096] of Char;
  TimeStamp: Cardinal;
begin
  if SteamAPI.UGC.GetItemInstallInfo(fs_workshop_interface.Value, @SizeOnDisk, @Path, 1024, @TimeStamp) then
    LoadInterfaceArchives(Path + '/', True)
  else
    Debug('Failed to load interface from workshop - ' + IntToStr(fs_workshop_interface.Value));
end;

procedure DownloadItemResult(Callback: PDownloadItemResult_t); cdecl;
begin
  if Callback.m_unAppID = STEAM_APPID then
  begin
    if Callback.m_eResult = k_EResultOK then
    begin
      if MapChangeItemID = Callback.m_nPublishedFileId then
      begin
        if ForceReconnect then
        begin
          ExitToMenu();
          JoinServer;
          ForceReconnect := False;
        end;
      end;
    end else
      Debug('[Steam] Failed to download workshop item, id:' + IntToStr(Callback.m_nPublishedFileId) + ' error: ' + IntToStr(Ord(Callback.m_eResult)));
  end;
end;

procedure SteamNetConnectionStatusChangedCallback(Callback: PSteamNetConnectionStatusChangedCallback_t); cdecl;
begin
  if Assigned(UDP) then
    UDP.ProcessEvents(Callback);
end;

{$ENDIF}

procedure StartGame();
var
  ini: TMemINIFile;
  fov: Single;
  currentDisplay: TSDL_DisplayMode;
  SystemLang: String = 'en_US';
  SystemFallbackLang: String = 'en_US';
  RadioMenuStream: TStream;
  BasePathSDL: PChar;
  UserPathSDL: PChar;
  i: Integer;
  s: String;
begin
  Randomize;

  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.DateSeparator := '-';

  UserPathSDL := SDL_GetPrefPath('OpenSoldat', 'OpenSoldat');
  BasePathSDL := SDL_GetBasePath();

  Debug('[PhysFS] Initializing system');

  if not PhysFS_Init(PChar(ParamStr(0))) then
  begin
    ShowMessage(_('Could not initialize PhysFS. Try to reinstall the game.'));
    Exit;
  end;

  CvarInit();
  InitClientCommands();
  ParseCommandLine();

  // NOTE: fs_basepath, fs_userpath, fs_portable and fs_localmount
  // must be set from command line, not in client.cfg.
  if fs_portable.Value then
  begin
    UserDirectory := BasePathSDL;
    BaseDirectory := BasePathSDL;
    Debug('[FS] Portable mode enabled.');
  end
  else
  begin
    UserDirectory := UserPathSDL;
    if fs_userpath.Value <> '' then
    begin
      s := ExpandFileName(fs_userpath.Value);
      if DirectoryExists(s) then
        UserDirectory := IncludeTrailingPathDelimiter(s)
      else
        Debug('[FS] Warning: Specified fs_userpath directory ''' + fs_userpath.Value + ''' does not exist.');
    end;

    BaseDirectory := BasePathSDL;
    if fs_basepath.Value <> '' then
    begin
      s := ExpandFileName(fs_basepath.Value);
      if DirectoryExists(s) then
        BaseDirectory := IncludeTrailingPathDelimiter(s)
      else
        Debug('[FS] Warning: Specified fs_basepath directory ''' + fs_basepath.Value + ''' does not exist.');
    end;
  end;

  Debug('[FS] UserDirectory: ' + UserDirectory);
  Debug('[FS] BaseDirectory: ' + BaseDirectory);

  Debug('[PhysFS] Mounting game archive');

  if fs_localmount.Value then
  begin
    if not PhysFS_mount(PChar(UserDirectory), '/', False) then
    begin
      ShowMessage(_('Could not load base game archive (game directory).'));
      Exit;
    end;
  end else
  begin
    if not PhysFS_mount(PChar(BaseDirectory + '/soldat.smod'), '/', False) then
    begin
      ShowMessage(_('Could not load base game archive (soldat.smod). Try to reinstall the game.'));
      Exit;
    end;
    GameModChecksum := Sha1File(BaseDirectory + '/soldat.smod', 4096);
  end;

  // Now that the game archive is mounted, and we have UserDirectory and BaseDirectory set,
  // we can create the basic directory structure and unpack the necessary config files.
  CreateDirIfMissing(UserDirectory + '/configs');
  CreateDirIfMissing(UserDirectory + '/screens');
  CreateDirIfMissing(UserDirectory + '/demos');
  CreateDirIfMissing(UserDirectory + '/logs');
  CreateDirIfMissing(UserDirectory + '/logs/kills');
  CreateDirIfMissing(UserDirectory + '/maps');
  CreateDirIfMissing(UserDirectory + '/mods');

  PHYSFS_CopyFileFromArchive('configs/bindings.cfg', UserDirectory + '/configs/bindings.cfg');
  PHYSFS_CopyFileFromArchive('configs/client.cfg', UserDirectory + '/configs/client.cfg');
  PHYSFS_CopyFileFromArchive('configs/controls.cfg', UserDirectory + '/configs/controls.cfg');
  PHYSFS_CopyFileFromArchive('configs/game.cfg', UserDirectory + '/configs/game.cfg');
  PHYSFS_CopyFileFromArchive('configs/graphics.cfg', UserDirectory + '/configs/graphics.cfg');
  PHYSFS_CopyFileFromArchive('configs/player.cfg', UserDirectory + '/configs/player.cfg');
  PHYSFS_CopyFileFromArchive('configs/sound.cfg', UserDirectory + '/configs/sound.cfg');

  LoadConfig('client.cfg');

  // NOTE: Code depending on CVars should be run after this line if possible.
  CvarsInitialized := True;

  if launcher_ipc_enable.Value then begin
    LauncherIPC := TClientLauncherIPC.Create;
    LauncherIPC.Connect(launcher_ipc_port.Value);
  end;

  NewLogFiles;

  // TODO remove HWIDs, replace by Fae auth tickets
  HWID := '00000000000';

  {$IFDEF MSWINDOWS}
  if r_sleeptime.Value > 0 then
    timeBeginPeriod(r_sleeptime.Value);
  {$ENDIF}

  Initing := 0;

  {$IFDEF STEAM}
  Debug('[Steam] Initializing system');
  // Initialize steam
  try
    SteamAPI := TSteam.Init;
  except
    on e: Exception do
    begin
      ShowMessage(_('Could not initialize Steam API. Try to run the game directly through Steam'));
      Exit;
    end;
  end;

  //SteamAPI.Utils.SetWarningMessageHook(SteamWarning);
  //SteamCallbackDispatcher.Create(1221, @SteamNetConnectionStatusChangedCallback, SizeOf(SteamNetConnectionStatusChangedCallback_t));
  //SteamCallbackDispatcher.Create(2301, @OnScreenshotReady, SizeOf(ScreenshotReady_t));
  //SteamCallbackDispatcher.Create(3406, @DownloadItemResult, SizeOf(DownloadItemResult_t));
  {$ENDIF}

  ModDir := '';

  {$IFDEF STEAM}
  if fs_workshop_mod.Value <> 0 then
    LoadWorkshopModArchives
  else
  {$ENDIF}if fs_mod.Value <> '' then
  begin
    Debug('[PhysFS] Mounting mods/' + fs_mod.Value + '.smod');
    if not PhysFS_mount(PChar(UserDirectory + 'mods/' + fs_mod.Value + '.smod'),
      PChar('mods/' + fs_mod.Value + '/'), False) then
    begin
      ShowMessage(_('Could not load mod archive (' + fs_mod.Value + ').'));
      Exit;
    end;
    ModDir := 'mods/' + fs_mod.Value + '/';
    CustomModChecksum := Sha1File(UserDirectory + 'mods/' + fs_mod.Value + '.smod', 4096);
  end;

  {$IFDEF STEAM}
  if fs_workshop_interface.Value <> 0 then
    LoadWorkshopInterfaceArchives;
  {$ENDIF}

  LoadInterfaceArchives(UserDirectory + 'custom-interfaces/');

  // these might change so keep a backup to avoid changing the settings file
  ScreenWidth := r_screenwidth.Value;
  ScreenHeight := r_screenheight.Value;
  RenderHeight := r_renderheight.Value;
  RenderWidth := r_renderwidth.Value;

  SDL_Init(SDL_INIT_VIDEO);
  SDL_GetCurrentDisplayMode(0, @currentDisplay);

  if (ScreenWidth = 0) or (ScreenHeight = 0) then
  begin
    ScreenWidth := currentDisplay.w;
    ScreenHeight := currentDisplay.h;
  end;

  if (RenderWidth = 0) or (RenderHeight = 0) then
  begin
    RenderWidth := ScreenWidth;
    RenderHeight := ScreenHeight;
  end;

  // Calculcate FOV to check for too high/low vision
  fov := RenderWidth / RenderHeight;
  if fov > MAX_FOV then
  begin
    RenderWidth := Ceil(RenderHeight * MAX_FOV);
    fov := MAX_FOV;
  end
  else if fov < MIN_FOV then
  begin
    RenderHeight := Ceil(RenderWidth / MIN_FOV);
    fov := MIN_FOV;
  end;

  // Calulcate internal game width based on the fov and internal height
  GameWidth := Round(fov * GameHeight);
  GameWidthHalf := GameWidth / 2;
  GameHeightHalf := GameHeight / 2;

  if r_fullscreen.Value = 0 then
  begin
    // avoid black bars in windowed mode
    if (ScreenWidth / ScreenHeight) >= (RenderWidth / RenderHeight) then
      ScreenWidth := Round(ScreenHeight * (RenderWidth / RenderHeight))
    else
      ScreenHeight := Round(ScreenWidth * (RenderHeight / RenderWidth));
  end;

  // window size equals "screen" size except in windowed fullscreen
  WindowWidth := ScreenWidth;
  WindowHeight := ScreenHeight;

  if r_fullscreen.Value = 2 then
  begin
  //  WindowWidth := Screen.Width;
  //  WindowHeight := Screen.Height;
  end;

  GfxLog(Format('Window size: %dx%d', [WindowWidth, WindowHeight]));
  GfxLog(Format('Target resolution: %dx%d', [ScreenWidth, ScreenHeight]));
  GfxLog(Format('Internal resolution: %dx%d', [RenderWidth, RenderHeight]));

  // even windowed mode can behave as fullscreen with the right size
  //IsFullscreen := (WindowWidth = Screen.Width) and (WindowHeight = Screen.Height);

  // interface is hard-coded to work on 4:3 aspect ratio,
  // but luckily for us the interface rendering code
  // translates the points using _RScala scale factor
  // above, so all we really need to do to make interace
  // work for widescreens is translate those points to a wider
  // area, which we can do by using the 640/480 as scale factors
  // even in widescreen resolutions. The interface code does NOT
  // use the _RScala to scale the interface, so this won't make
  // it look distorted.
  if r_scaleinterface.Value then
  begin
    _RScala.x := 1;
    _RScala.y := 1;

    _iscala.x := GameWidth / DEFAULT_WIDTH;
    _iscala.y := 1;

    fragx := floor(GameWidthHalf - 300) - 25;
  end
  else
  begin
    _RScala.x := RenderWidth / GameWidth;
    _RScala.y := RenderHeight / GameHeight;

    _iscala.x := RenderWidth / 640;
    _iscala.y := RenderHeight / 480;

    fragx := floor(RenderWidth / 2 - 300) - 25;

    if RenderHeight > GameHeight then
      fragy := Round(10 * _rscala.y);
  end;

  GameRenderingParams.InterfaceName := ui_style.Value;

  ResetFrameTiming();

  GfxLog('Loading game graphics');

  if not InitGameGraphics then
  begin
    ShowMessage('The required OpenGL functionality isn''t supported. ' +
      'Please, update your video drivers and try again.');
    //ExitButtonClick(nil);
    Exit;
  end;

  WindowReady := True;

  if cl_lang.Value <> '' then
  begin
    SystemLang := cl_lang.Value;
  end else
    GetLanguageIDs(SystemLang, SystemFallbackLang);

  if InitTranslation(ModDir + '/txt/' + SystemLang + '.mo') then
    Debug('Game captions loaded from ' + ModDir + '/txt/' + SystemLang)
  else
    Debug('Game captions not found');

  AddLineToLogFile(GameLog, 'Initializing Sound Library.', ConsoleLogFileName);
  // Init Sound Library
  if not InitSound() then
  begin
    AddLineToLogFile(GameLog, 'Failed to initialize Sound Library.', ConsoleLogFileName);
    // Let the player know that he has no sound (no popup window)
  end;

  LoadSounds('');
  if Length(ModDir) > 0 then
    LoadSounds(ModDir);

  AddLineToLogFile(GameLog, 'Creating network interface.', ConsoleLogFileName);

  // Create Consoles
  MainConsole.CountMax := Round(ui_console_length.Value * _rscala.y);
  MainConsole.ScrollTickMax := 150;
  MainConsole.NewMessageWait := 150;
  MainConsole.AlphaCount := 255;
  MainConsole.Count := 0;
  if MainConsole.CountMax > 254 then
    MainConsole.CountMax := 254;

  BigConsole.CountMax := Floor((0.85 * RenderHeight) /
    (font_consolelineheight.Value * FontStyleSize(FONT_SMALL)));
  BigConsole.ScrollTickMax := 1500000;
  BigConsole.NewMessageWait := 0;
  BigConsole.AlphaCount := 255;
  BigConsole.Count := 0;
  if BigConsole.CountMax > 254 then
    BigConsole.CountMax := 254;

  KillConsole.CountMax := Round(ui_killconsole_length.Value * _rscala.y);
  KillConsole.ScrollTickMax := 240;
  KillConsole.NewMessageWait := 70;

  // Create static player objects
  for i := 1 to MAX_SPRITES do
    Sprite[i].Player := TPlayer.Create;

  LoadAnimObjects('');
  if Length(ModDir) > 0 then
    LoadAnimObjects(ModDir);

  // greet!
  MainConsole.Console(_('Welcome to OpenSoldat') + ' ' + OPENSOLDAT_VERSION,
    DEFAULT_MESSAGE_COLOR);

  {$IFDEF ENABLE_FAE}
  if FaeIsEnabled then
    MainConsole.Console('Multi-player sessions are protected by Fae Anti-Cheat', AC_MESSAGE_COLOR);
  {$ENDIF}

  // Load weapon display names
  LoadWeaponNames();
  CreateWeaponsBase();

  MapChangeCounter := -60;

  PlayerNamesShow := True;

  CursorText := '';

  InitGameMenus();

  RadioMenu := TStringList.Create;
  RadioMenuStream := PHYSFS_readAsStream('txt/radiomenu-default.ini');
  ini := TMemIniFile.Create(RadioMenuStream);
  if Assigned(ini) then
  begin
    ini.ReadSectionValues('OPTIONS', RadioMenu);
    ini.Free;
  end;
  RadioMenuStream.Free;

  // Play demo
  FreeCam := 1;
  NoTexts := 0;
  ShotDistanceShow := -1;

  if r_compatibility.Value then
    cl_actionsnap.SetValue(False);

  WriteLogFile(GameLog, ConsoleLogFileName);
  RunDeferredCommands();
end;

procedure ShutDown;
begin
  ExitToMenu;

  {$IFDEF MSWINDOWS}
  if r_sleeptime.Value > 0 then
    timeEndPeriod(r_sleeptime.Value);
  {$ENDIF}

  if AbnormalTerminate then
    Exit;

  AddLineToLogFile(GameLog, 'Freeing sprites.', ConsoleLogFileName);

  // Free GFX
  DestroyGameGraphics();

  SDL_Quit();

  DeInitTranslation();

  AddLineToLogFile(GameLog, 'UDP closing.', ConsoleLogFileName);

  FreeAndNil(UDP);

  AddLineToLogFile(GameLog, 'Sound closing.', ConsoleLogFileName);

  CloseSound;

  AddLineToLogFile(GameLog, 'PhysFS closing.', ConsoleLogFileName);

  PhysFS_deinit();

  if launcher_ipc_enable.Value then begin
    AddLineToLogFile(GameLog, 'Launcher connection closing.', ConsoleLogFileName);
    FreeAndNil(LauncherIPC);
  end;

  {$IFDEF STEAM}
  AddLineToLogFile(GameLog, 'Steam API closing.', ConsoleLogFileName);
  SteamAPI_Shutdown();
  {$ENDIF}

  AddLineToLogFile(GameLog, '   End of Log.', ConsoleLogFileName);

  WriteLogFile(GameLog, ConsoleLogFileName);

  CommandCleanup();
  CvarCleanup();

  Halt(0);
end;

procedure StartGameLoop;
begin
  while GameLoopRun do
  begin
    UDP.ProcessLoop;
    GameInput();
    GameLoop();
  end;
end;

procedure JoinServer();
begin
  ResetFrameTiming;

  Inc(Initing);
  if Initing > 10 then
    Initing := 10;

  ServerIP := Trim(JoinIP);
  if not TryStrToInt(Trim(JoinPort), ServerPort) then
    Exit;

  InitGameGraphics();
  DoTextureLoading(True);

  {$IFDEF ENABLE_FAE}
  // Fae performs various initialization tasks (compute HWID, checksum DLLs, etc.) in a separate
  // thread. We must wait for this thread to become ready to accept messages. Otherwise, we cannot
  // authenticate with the game server's anti-cheat component.
  // NOTE That in case of an internal error Fae displays a message box here and quits.
  RenderGameInfo(_('Initializing'));
  FaePreflight; // no-op if Fae if called before or if Fae is disabled
  {$ENDIF}

  UDP := TClientNetwork.Create();
  // DEMO
  if JoinPort = '0' then
  begin
    DemoPlayer.OpenDemo(UserDirectory + 'demos/' + JoinIP + '.sdm');
    DemoPlayer.ProcessDemo;
    ProgReady := True;
    GameLoopRun := True;
    RenderGameInfo(_('Loading'));
    StartGameLoop();
  end else
  begin
    RenderGameInfo(_(WideFormat('Connecting to %s:%d', [ServerIP, ServerPort])));

    if UDP.Connect(ServerIP, ServerPort) then
    begin
      ProgReady := True;
      GameLoopRun := True;
      RenderGameInfo(_('Loading'));
      ClientRequestGame;
      StartGameLoop;
    end
    else
    begin
      RenderGameInfo(_('Connection timed out.'));
      Exit;
    end;
  end;
end;

procedure ShowMessage(MessageText: AnsiString); overload;
begin
  ShowMessage(WideString(MessageText));
end;

procedure ShowMessage(MessageText: WideString); overload;
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, PChar('Error'), PAnsiChar(AnsiString(MessageText)), nil);
end;

initialization
  // Mask exceptions on 32 and 64 bit fpc builds
  {$IF defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$ENDIF}

finalization
  ShutDown;
end.

{*******************************************************}
{                                                       }
{       Net Unit for SOLDAT                             }
{                                                       }
{       Copyright (c) 2002-03 Michal Marcinkowski       }
{                                                       }
{*******************************************************}

unit Net;

interface

uses
  // delphi and system units
  SysUtils, Classes, fgl, {$IFDEF DEVELOPMENT}Util,{$ENDIF}

  // helper units
  Vector, Version, Sha1,

  // anti-cheat
  {$IFDEF SERVER}
    {$IFDEF ENABLE_FAE}FaeBase, FaeRemoteAttestation, NetworkServerFae,{$ENDIF}
  {$ELSE}
    {$IFDEF ENABLE_FAE}FaeBase, FaeRemoteAttestation, NetworkClientFae,{$ENDIF}
    GameRendering,
  {$ENDIF}

  Steam,

  // soldat units
  Weapons, Constants;

const
  // Binary ops
  B1 = 1;
  B2 = 2;
  B3 = 4;
  B4 = 8;
  B5 = 16;
  B6 = 32;
  B7 = 64;
  B8 = 128;
  B9 = 256;
  B10 = 512;
  B11 = 1024;
  B12 = 2048;
  B13 = 4096;
  B14 = 8192;
  B15 = 16384;
  B16 = 32768;

  // MESSAGE IDs
  MsgID_Custom = 0;
  MsgID_HeartBeat = MsgID_Custom + 2;
  MsgID_ServerSpriteSnapshot = MsgID_Custom + 3;
  MsgID_ClientSpriteSnapshot = MsgID_Custom + 4;
  MsgID_BulletSnapshot = MsgID_Custom + 5;
  MsgID_ChatMessage = MsgID_Custom + 6;
  MsgID_ServerSkeletonSnapshot = MsgID_Custom + 7;
  MsgID_MapChange = MsgID_Custom + 8;
  MsgID_ServerThingSnapshot = MsgID_Custom + 9;
  MsgID_ThingTaken = MsgID_Custom + 12;
  MsgID_SpriteDeath = MsgID_Custom + 13;
  MsgID_PlayerInfo = MsgID_Custom + 15;
  MsgID_PlayersList = MsgID_Custom + 16;
  MsgID_NewPlayer = MsgID_Custom + 17;
  MsgID_ServerDisconnect = MsgID_Custom + 18;
  MsgID_PlayerDisconnect = MsgID_Custom + 19;
  MsgID_Delta_Movement = MsgID_Custom + 21;
  MsgID_Delta_Weapons = MsgID_Custom + 25;
  MsgID_Delta_Helmet = MsgID_Custom + 26;
  MsgID_Delta_MouseAim = MsgID_Custom + 29;
  MsgID_Ping = MsgID_Custom + 30;
  MsgID_Pong = MsgID_Custom + 31;
  MsgID_FlagInfo = MsgID_Custom + 32;
  MsgID_ServerThingMustSnapshot = MsgID_Custom + 33;
  MsgID_IdleAnimation = MsgID_Custom + 37;
  MsgID_ServerSpriteSnapshot_Major = MsgID_Custom + 41;
  MsgID_ClientSpriteSnapshot_Mov = MsgID_Custom + 42;
  MsgID_ClientSpriteSnapshot_Dead = MsgID_Custom + 43;
  MsgID_UnAccepted = MsgID_Custom + 44;
  MsgID_VoteOn = MsgID_Custom + 45;
  MsgID_VoteMap = MsgID_Custom + 46;
  MsgID_VoteMapReply = MsgID_Custom + 47;
  MsgID_VoteKick = MsgID_Custom + 48;
  MsgID_RequestThing = MsgID_Custom + 51;
  MsgID_ServerVars = MsgID_Custom + 52;
  MsgID_ServerSyncMsg = MsgID_Custom + 54;
  MsgID_ClientFreeCam = MsgID_Custom + 55;
  MsgID_VoteOff = MsgID_Custom + 56;
  MsgID_FaeData = MsgID_Custom + 57;
  MsgID_RequestGame = MsgID_Custom + 58;
  MsgID_ForcePosition = MsgID_Custom + 60;
  MsgID_ForceVelocity = MsgID_Custom + 61;
  MsgID_ForceWeapon = MsgID_Custom + 62;
  MsgID_ChangeTeam = MsgID_Custom + 63;
  MsgID_SpecialMessage = MsgID_Custom + 64;
  MsgID_WeaponActiveMessage = MsgID_Custom + 65;
  MsgID_JoinServer = MsgID_Custom + 68;
  MsgID_PlaySound = MsgID_Custom + 70;
  MsgID_SyncCvars = MsgID_Custom + 71;
  MsgID_VoiceData = MsgID_Custom + 72;

  MAX_PLAYERS = 32;

  // ControlMethod
  HUMAN = 1;
  BOT = 2;

  // Request Reply States
  OK = 1;
  WRONG_VERSION = 2;
  WRONG_PASSWORD = 3;
  BANNED_IP = 4;
  SERVER_FULL = 5;
  INVALID_HANDSHAKE = 8;
  WRONG_CHECKSUM = 9;
  ANTICHEAT_REQUIRED = 10;
  ANTICHEAT_REJECTED = 11;
  STEAM_ONLY = 12;

  LAN = 1;
  INTERNET = 0;

  // FLAG INFO
  RETURNRED = 1;
  RETURNBLUE = 2;
  CAPTURERED = 3;
  CAPTUREBLUE = 4;

  // Kick/Ban Why's
  KICK_UNKNOWN = 0;
  KICK_NORESPONSE = 1;
  KICK_NOCHEATRESPONSE = 2; // TOOD remove?
  KICK_CHANGETEAM = 3; // TODO remove?
  KICK_PING = 4;
  KICK_FLOODING = 5;
  KICK_CONSOLE = 6;
  KICK_CONNECTCHEAT = 7; // TODO remove?
  KICK_CHEAT = 8;
  KICK_LEFTGAME = 9;
  KICK_VOTED = 10;
  KICK_AC = 11;
  KICK_SILENT = 12;
  KICK_STEAMTICKET = 13;
  _KICK_END = 14;

  // Join types
  JOIN_NORMAL = 0;
  JOIN_SILENT = 1;

  // RECORD
  NETW = 0;
  REC = 1;

  CLIENTPLAYERRECIEVED_TIME = 3 * 60;

  FLOODIP_MAX = 18;
  MAX_FLOODIPS = 1000;
  MAX_BANIPS = 1000;

  PLAYERNAME_CHARS = 24;
  PLAYERHWID_CHARS = 11;
  MAPNAME_CHARS = 64;
  REASON_CHARS = 26;

  ACTYPE_NONE = 0;
  ACTYPE_FAE = 1;

  MSGTYPE_CMD = 0;
  MSGTYPE_PUB = 1;
  MSGTYPE_TEAM = 2;
  MSGTYPE_RADIO = 3;

type
  TNetwork = class
    private
      FInit: Boolean;
      FActive: Boolean;
      FAddress: SteamNetworkingIPAddr;

      FPeer: HSteamNetConnection;
      FHost: HSteamListenSocket;
      {$IFDEF SERVER}
      FPollGroup: HSteamNetPollGroup;
      {$ENDIF}
      NetworkingSockets: PISteamNetworkingSockets;
      NetworkingUtils: PISteamNetworkingUtils;
    public
      property Active: Boolean read FActive write FActive;
      constructor Create();
      destructor Destroy(); override;

      function Disconnect(Now: Boolean): Boolean;
      procedure FlushMsg();
      procedure ProcessEvents(pInfo: PSteamNetConnectionStatusChangedCallback_t); virtual; abstract;

      function GetDetailedConnectionStatus(hConn: HSteamNetConnection): String;
      function GetConnectionRealTimeStatus(hConn: HSteamNetConnection): SteamNetConnectionRealTimeStatus_t;
      procedure SetConnectionName(hConn: HSteamNetConnection; Name: AnsiString);
      function GetStringAddress(pAddress: PSteamNetworkingIPAddr; Port: Boolean): AnsiString;

      function SetGlobalConfigValueInt32(eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
      function SetGlobalConfigValueFloat(eValue: ESteamNetworkingConfigValue; val: Single): Boolean;
      function SetGlobalConfigValueString(eValue: ESteamNetworkingConfigValue; val: PChar): Boolean;
      function SetConnectionConfigValueInt32(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
      function SetConnectionConfigValueFloat(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
      function SetConnectionConfigValueString(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;

      procedure SetDebugLevel(Level: ESteamNetworkingSocketsDebugOutputType);

      property Host: HSteamListenSocket read FHost;
      property Peer: HSteamNetConnection read FPeer;
      property NetworkingSocket: PISteamNetworkingSockets read NetworkingSockets;
      property NetworkingUtil: PISteamNetworkingUtils read NetworkingUtils;

      property Port: Word read FAddress.m_port write FAddress.m_port;
      property Address: SteamNetworkingIPAddr read FAddress;
  end;

  {$IFDEF SERVER}
  TServerNetwork = class(TNetwork)
  public
    procedure ProcessEvents(pInfo: PSteamNetConnectionStatusChangedCallback_t); override;
    constructor Create(Host: String; Port: Word);
    destructor Destroy; override;
    procedure ProcessLoop;
    procedure HandleMessages(IncomingMsg: PSteamNetworkingMessage_t);
    function SendData(var Data; Size: Integer; peer: HSteamNetConnection; Flags: Integer): Boolean;
    procedure UpdateNetworkStats(Player: Byte);
  end;
  {$ELSE}
  TClientNetwork = class(TNetwork)
  public
    procedure ProcessEvents(pInfo: PSteamNetConnectionStatusChangedCallback_t); override;
    constructor Create();
    destructor Destroy; override;
    function Connect(Host: String; Port: Word): Boolean;
    procedure ProcessLoop;
    procedure HandleMessages(IncomingMsg: PSteamNetworkingMessage_t);
    function SendData(var Data; Size: Integer; Flags: Integer): Boolean;
  end;
  {$ENDIF}

  // Network Player Class
  // Client:
  //   Each sprite has a static instance of this class, allocated on startup.
  // Server:
  //   This class is attached to the GNS peer's data pointer. Its lifetime equals the lifetime of
  //   the GNS connection. The player's Sprite member MAY point to a valid sprite if the player has
  //   joined a game.
  TPlayer = class
  public
    // (!!!) When extending this class also extend its clone method, else ScriptCore breaks (maybe).

    // client/server shared stuff:
    // TODO stuff here that is relevant for the sprite (color, team, ...) should be moved to
    // a template object instead. When a sprite is created, then copy the template to it and apply
    // modifications depending on the game mode (eg. change the shirt color to match the team.)
    // That would allow switching game modes etc. without losing information about the player.
    Name: string;
    ShirtColor, PantsColor, SkinColor, HairColor, JetColor: LongWord;
    Kills, Deaths: Integer;
    Flags: Byte;
    PingTicks, PingTicksB, PingTime, Ping: Integer;
    RealPing: Word;
    ConnectionQuality: Byte;
    Team: Byte;
    ControlMethod: Byte;
    Chain, HeadCap, HairStyle: Byte;
    SecWep: Byte;
    Camera: Byte;
    Muted: Byte;
    SpriteNum: Byte; // 0 if no sprite exists yet
    DemoPlayer: Boolean;
    {$IFDEF STEAM}
    SteamID: CSteamID;
    SteamStats: Boolean;
    LastReceiveVoiceTime: Integer;
    SteamFriend: Boolean;
    {$ENDIF}

    // server only below this line:
    // -----
    {$IFDEF SERVER}
    IP: string;
    Port: Integer;

    // anti-cheat client handles and state
    {$IFDEF ENABLE_FAE}
    FaeResponsePending: Boolean;
    FaeKicked: Boolean;
    FaeTicks: Integer;
    FaeSecret: TFaeSecret;
    {$ENDIF}

    Peer: HSteamNetConnection;
    HWID: string;
    PlayTime: Integer;
    GameRequested: Boolean;

    // counters for warnings:
    ChatWarnings: Byte;
    TKWarnings: Byte;

    // anti mass flag counters:
    ScoresPerSecond: Integer;
    GrabsPerSecond: Integer;
    GrabbedInBase: Boolean;  // To prevent false accusations
    StandingPolyType: Byte;  // Testing
    KnifeWarnings: Byte;

    constructor Create();
    destructor Destroy(); override;
    {$ENDIF SERVER}
    procedure ApplyShirtColorFromTeam; // TODO remove, see comment before Name
    function Clone: TPlayer;
  end;

  TPlayers = TFPGObjectList<TPlayer>;

  TStatsString = array[0..2048] of Char;
  TIPString = array[0..128] of Char;

  PmsgHeader = ^TMsgHeader;
  TMsgHeader = packed record
    ID: Byte;
  end;

  PMsg_StringMessage = ^TMsg_StringMessage;
  TMsg_StringMessage = packed record
    Header: TMsgHeader;
    Num: Byte;
    MsgType: Byte;
    Text: array[0..0] of WideChar;
  end;

  PMsg_Ping = ^TMsg_Ping;
  TMsg_Ping = packed record
    Header: TMsgHeader;
    PingTicks: Byte;
    PingNum: Byte;
  end;

  PMsg_Pong = ^TMsg_Pong;
  TMsg_Pong = packed record
    Header: TMsgHeader;
    PingNum: Byte;
  end;

  // HEARTBEAT TYPE
  // - every while information about frags, server status etc.
  PMsg_HeartBeat = ^TMsg_HeartBeat;
  TMsg_HeartBeat = packed record
    Header: TMsgHeader;
    MapID: LongWord;
    TeamScore: array[1..4] of Word;
    Active: array[1..MAX_PLAYERS] of Boolean;
    Kills: array[1..MAX_PLAYERS] of Word;
    Caps: array[1..MAX_PLAYERS] of Byte;
    Team: array[1..MAX_PLAYERS] of Byte;
    Deaths: array[1..MAX_PLAYERS] of Word;
    Ping: array[1..MAX_PLAYERS] of Byte;
    RealPing: array[1..MAX_PLAYERS] of Word;
    ConnectionQuality: array[1..MAX_PLAYERS] of Byte;
    Flags: array[1..MAX_PLAYERS] of Byte;
  end;

  // SERVERSPRITESNAPSHOT TYPE
  // - servers object status - uses for all objects in game
  PMsg_ServerSpriteSnapshot = ^TMsg_ServerSpriteSnapshot;
  TMsg_ServerSpriteSnapshot = packed record
    Header: TMsgHeader;
    Num: Byte;
    Pos, Velocity: TVector2;
    MouseAimX, MouseAimY: SmallInt;
    Position: Byte;
    Keys16: Word;
    Look: Byte;
    Vest: Single;
    Health: Single;
    AmmoCount, GrenadeCount: Byte;
    WeaponNum, SecondaryWeaponNum: Byte;
    ServerTicks: LongInt;
  end;

  PMsg_ServerSpriteSnapshot_Major = ^TMsg_ServerSpriteSnapshot_Major;
  TMsg_ServerSpriteSnapshot_Major = packed record
    Header: TMsgHeader;
    Num: Byte;
    Pos, Velocity: TVector2;
    Health: Single;
    MouseAimX, MouseAimY: SmallInt;
    Position: Byte;
    Keys16: Word;
    ServerTicks: LongInt;
  end;

  PMsg_ServerVars = ^TMsg_ServerVars;
  TMsg_ServerVars = packed record
    Header: TMsgHeader;
    Damage:            array[1..ORIGINAL_WEAPONS] of Single;
    Ammo:              array[1..ORIGINAL_WEAPONS] of Byte;
    ReloadTime:        array[1..ORIGINAL_WEAPONS] of Word;
    Speed:             array[1..ORIGINAL_WEAPONS] of Single;
    BulletStyle:       array[1..ORIGINAL_WEAPONS] of Byte;
    StartUpTime:       array[1..ORIGINAL_WEAPONS] of Word;
    Bink:              array[1..ORIGINAL_WEAPONS] of SmallInt;
    FireInterval:      array[1..ORIGINAL_WEAPONS] of Word;
    MovementAcc:       array[1..ORIGINAL_WEAPONS] of Single;
    BulletSpread:      array[1..ORIGINAL_WEAPONS] of Single;
    Recoil:            array[1..ORIGINAL_WEAPONS] of Word;
    Push:              array[1..ORIGINAL_WEAPONS] of Single;
    InheritedVelocity: array[1..ORIGINAL_WEAPONS] of Single;
    ModifierHead:      array[1..ORIGINAL_WEAPONS] of Single;
    ModifierChest:     array[1..ORIGINAL_WEAPONS] of Single;
    ModifierLegs:      array[1..ORIGINAL_WEAPONS] of Single;
    NoCollision:       array[1..ORIGINAL_WEAPONS] of Byte;
    WeaponActive:      array[1..MAIN_WEAPONS] of Byte;
  end;

  PMsg_ServerSyncCvars = ^TMsg_ServerSyncCvars;
  TMsg_ServerSyncCvars = packed record
    Header: TMsgHeader;
    ItemCount: Byte;
    Data: array[0..0] of Byte;
  end;

  // CLIENTSPRITESNAPSHOT TYPE
  // - current players status
  PMsg_ClientSpriteSnapshot = ^TMsg_ClientSpriteSnapshot;
  TMsg_ClientSpriteSnapshot = packed record
    Header: TMsgHeader;
    AmmoCount, SecondaryAmmoCount: Byte;
    WeaponNum, SecondaryWeaponNum: Byte;
    Position: Byte;
  end;

  PMsg_ClientSpriteSnapshot_Mov = ^TMsg_ClientSpriteSnapshot_Mov;
  TMsg_ClientSpriteSnapshot_Mov = packed record
    Header: TMsgHeader;
    Pos, Velocity: TVector2;
    Keys16: Word;
    MouseAimX, MouseAimY: SmallInt;
  end;

  PMsg_ClientSpriteSnapshot_Dead = ^TMsg_ClientSpriteSnapshot_Dead;
  TMsg_ClientSpriteSnapshot_Dead = packed record
    Header: TMsgHeader;
    CameraFocus: Byte;
  end;

  // BULLETSNAPSHOT TYPE
  // - for server's bullet information
  PMsg_BulletSnapshot = ^TMsg_BulletSnapshot;
  TMsg_BulletSnapshot = packed record
    Header: TMsgHeader;
    Owner, WeaponNum: Byte;
    Pos, Velocity: TVector2;
    Seed: Word;
    Forced: Boolean; // CreateBullet() forced bullet?
  end;

  // BULLETSNAPSHOT TYPE
  // - for clients' bullet information
  PMsg_ClientBulletSnapshot = ^TMsg_ClientBulletSnapshot;
  TMsg_ClientBulletSnapshot = packed record
    Header: TMsgHeader;
    WeaponNum: Byte;
    Pos, Velocity: TVector2;
    Seed: Word;
    ClientTicks: LongInt;
  end;

  // SERVERSKELETONSNAPSHOT TYPE
  // - info on the sprites skeleton - used when sprite is DeadMeat
  PMsg_ServerSkeletonSnapshot = ^TMsg_ServerSkeletonSnapshot;
  TMsg_ServerSkeletonSnapshot = packed record
    Header: TMsgHeader;
    Num: Byte;
    // Constraints: byte;
    RespawnCounter: SmallInt;
  end;

  // MAPCHANGE TYPE
  PMsg_MapChange = ^TMsg_MapChange;
  TMsg_MapChange = packed record
    Header: TMsgHeader;
    Counter: SmallInt;
    MapName: string[MAPNAME_CHARS];
    MapChecksum: TSHA1Digest;
  end;

  // SERVERTHINGSNAPSHOT TYPE
  // - info on the things in world that move
  PMsg_ServerThingSnapshot = ^TMsg_ServerThingSnapshot;
  TMsg_ServerThingSnapshot = packed record
    Header: TMsgHeader;
    Num, Owner, Style, HoldingSprite: Byte;
    Pos, OldPos: array[1..4] of TVector2;
  end;

  // SERVERTHINGMUSTSNAPSHOT TYPE
  // - info on the things in world
  PMsg_ServerThingMustSnapshot = ^TMsg_ServerThingMustSnapshot;
  TMsg_ServerThingMustSnapshot = packed record
    Header: TMsgHeader;
    Num, Owner, Style, HoldingSprite: Byte;
    Pos, OldPos: array[1..4] of TVector2;
    Timeout: LongInt;
  end;

  // SERVERTHINGTAKENINFO TYPE
  // - sent when thing is taken
  PMsg_ServerThingTaken = ^TMsg_ServerThingTaken;
  TMsg_ServerThingTaken = packed record
    Header: TMsgHeader;
    Num, Who: Byte;
    Style, AmmoCount: Byte;
  end;

  // SPRITEDEATH TYPE
  // - if sprite dies this is sent
  PMsg_SpriteDeath = ^TMsg_SpriteDeath;
  TMsg_SpriteDeath = packed record
    Header: TMsgHeader;
    Num, Killer, KillBullet, Where: Byte;
    Constraints: Byte;
    Pos, OldPos: array[1..16] of TVector2;
    Health: Single;
    OnFire: Byte;
    RespawnCounter: SmallInt;
    ShotDistance, ShotLife: Single;
    ShotRicochet: Byte;
  end;

  // REQUEST GAME TYPE
  PMsg_RequestGame = ^TMsg_RequestGame;
  TMsg_RequestGame = packed record
    Header: TMsgHeader;
    Version: array[0..SOLDAT_VERSION_CHARS - 1] of char;
    Forwarded: Byte;
    HaveAntiCheat: Byte;
    HardwareID: string[PLAYERHWID_CHARS];
    Password: array[0..24] of char;
  end;

  // PLAYER INFO TYPE
  PMsg_PlayerInfo = ^TMsg_PlayerInfo;
  TMsg_PlayerInfo = packed record
    Header: TMsgHeader;
    Name: array[0..PLAYERNAME_CHARS - 1] of char;
    Look: Byte;
    Team: Byte;
    ShirtColor, PantsColor, SkinColor, HairColor, JetColor: LongWord;
    GameModChecksum: TSHA1Digest;
    CustomModChecksum: TSHA1Digest;
  end;

  // PLAYERS LIST TYPE
  PMsg_PlayersList = ^TMsg_PlayersList;
  TMsg_PlayersList = packed record
    Header: TMsgHeader;
    ModName: array[0..MAPNAME_CHARS - 1] of char;
    ModChecksum: TSHA1Digest;
    MapName: array[0..MAPNAME_CHARS - 1] of char;
    MapChecksum: TSHA1Digest;
    Players: Byte;
    Name: array[1..MAX_PLAYERS] of array[0..PLAYERNAME_CHARS - 1] of char;
    ShirtColor, PantsColor, SkinColor, HairColor, JetColor: array[1..MAX_PLAYERS]
      of LongWord;
    Team: array[1..MAX_PLAYERS] of Byte;
    PredDuration: array[1..MAX_PLAYERS] of Byte;
    Look: array[1..MAX_PLAYERS] of Byte;
    Pos: array[1..MAX_PLAYERS] of TVector2;
    Vel: array[1..MAX_PLAYERS] of TVector2;
    SteamID: array[1..MAX_PLAYERS] of UInt64;
    CurrentTime: Integer;
    ServerTicks: LongInt;
    AntiCheatRequired: Boolean;
  end;

  // REJECTED CONNECTION TYPE
  PMsg_UnAccepted = ^TMsg_UnAccepted;
  TMsg_UnAccepted = packed record
    Header: TMsgHeader;
    State: Byte;
    Version: array[0..SOLDAT_VERSION_CHARS - 1] of char;
    Text: array[0..0] of char;
  end;

  // NEW PLAYER TYPE
  PMsg_NewPlayer = ^TMsg_NewPlayer;
  TMsg_NewPlayer = packed record
    Header: TMsgHeader;
    Num: Byte;
    AdoptSpriteID: Byte;
    JoinType: Byte;
    Name: array[0..PLAYERNAME_CHARS - 1] of char;
    ShirtColor, PantsColor, SkinColor, HairColor, JetColor: LongWord;
    Team: Byte;
    Look: Byte;
    Pos: TVector2;
    SteamID: UInt64;
  end;

  // SERVER DISCONNECT TYPE
  PMsg_ServerDisconnect = ^TMsg_ServerDisconnect;
  TMsg_ServerDisconnect = packed record
    Header: TMsgHeader;
  end;

  // PLAYER DISCONNECT TYPE
  PMsg_PlayerDisconnect = ^TMsg_PlayerDisconnect;
  TMsg_PlayerDisconnect = packed record
    Header: TMsgHeader;
    Num: Byte;
    Why: Byte;
  end;

  // IDLE ANIMATION TYPE
  PMsg_IdleAnimation = ^TMsg_IdleAnimation;
  TMsg_IdleAnimation = packed record
    Header: TMsgHeader;
    Num: Byte;
    IdleRandom: SmallInt;
  end;

  PMsg_ClientFreeCam = ^TMsg_ClientFreeCam;
  TMsg_ClientFreeCam = packed record
    Header: TMsgHeader;
    FreeCamOn: Byte;
    TargetPos: TVector2;
  end;

  // DELTAS
  PMsg_ServerSpriteDelta_Movement = ^TMsg_ServerSpriteDelta_Movement;
  TMsg_ServerSpriteDelta_Movement = packed record
    Header: TMsgHeader;
    Num: Byte;
    Pos, Velocity: TVector2;
    Keys16: Word;
    MouseAimX, MouseAimY: SmallInt;
    ServerTick: LongInt;
  end;

  PMsg_ServerSpriteDelta_MouseAim = ^TMsg_ServerSpriteDelta_MouseAim;
  TMsg_ServerSpriteDelta_MouseAim = packed record
    Header: TMsgHeader;
    Num: Byte;
    MouseAimX, MouseAimY: SmallInt;
  end;

  PMsg_ServerSpriteDelta_Weapons = ^TMsg_ServerSpriteDelta_Weapons;
  TMsg_ServerSpriteDelta_Weapons = packed record
    Header: TMsgHeader;
    Num: Byte;
    WeaponNum, SecondaryWeaponNum: Byte;
    AmmoCount: Byte;
  end;

  PMsg_ServerSpriteDelta_Helmet = ^TMsg_ServerSpriteDelta_Helmet;
  TMsg_ServerSpriteDelta_Helmet = packed record
    Header: TMsgHeader;
    Num: Byte;
    WearHelmet: Byte;
  end;

  PMsg_ServerFlagInfo = ^TMsg_ServerFlagInfo;
  TMsg_ServerFlagInfo = packed record
    Header: TMsgHeader;
    Style, Who: Byte;
  end;

  PMsg_ServerSyncMsg = ^TMsg_ServerSyncMsg;
  TMsg_ServerSyncMsg = packed record
    Header: TMsgHeader;
    Time: Integer;
    Pause: Byte;
  end;

  {$IFDEF ENABLE_FAE}
  PMsg_FaeChallenge = ^TMsg_FaeChallenge;
  TMsg_FaeChallenge = packed record
    Header: TMsgHeader;
    InOrder: Byte;
    Challenge: TFaeChallenge;
  end;

  PMsg_FaeResponse = ^TMsg_FaeResponse;
  TMsg_FaeResponse = packed record
    Header: TMsgHeader;
    Response: TFaeResponseBox;
  end;
  {$ENDIF}

  PMsg_ForcePosition = ^TMsg_ForcePosition;
  TMsg_ForcePosition = packed record
    Header: TMsgHeader;
    Pos: TVector2;
    PlayerID: Byte;
  end;

  PMsg_ForceVelocity = ^TMsg_ForceVelocity;
  TMsg_ForceVelocity = packed record
    Header: TMsgHeader;
    Vel: TVector2;
    PlayerID: Byte;
  end;

  PMsg_ForceWeapon = ^TMsg_ForceWeapon;
  TMsg_ForceWeapon = packed record
    Header: TMsgHeader;
    WeaponNum, SecondaryWeaponNum: Byte;
    AmmoCount, SecAmmoCount: Byte;
  end;

  PMsg_ChangeTeam = ^TMsg_ChangeTeam;
  TMsg_ChangeTeam = packed record
    Header: TMsgHeader;
    Team: Byte;
  end;

  PMsg_RequestThing = ^TMsg_RequestThing;
  TMsg_RequestThing = packed record
    Header: TMsgHeader;
    ThingID: Byte;
  end;

  // Voting Messages

  // VOTING ON TYPE
  PMsg_VoteOn = ^TMsg_VoteOn;
  TMsg_VoteOn = packed record
    Header: TMsgHeader;
    VoteType: Byte;
    Timer: Word;
    Who: Byte;
    TargetName: array[0..MAPNAME_CHARS - 1] of char;
    Reason: array[0..REASON_CHARS - 1] of char;
  end;

  // VOTING OFF TYPE
  PMsg_VoteOff = ^TMsg_VoteOff;
  TMsg_VoteOff = packed record
    Header: TMsgHeader;
  end;

  // VOTING MAP LIST QUERY
  PMsg_VoteMap = ^TMsg_VoteMap;
  TMsg_VoteMap = packed record
    Header: TMsgHeader;
    MapID: Word;
  end;

  // VOTING MAP LIST RESPONSE
  PMsg_VoteMapReply = ^TMsg_VoteMapReply;
  TMsg_VoteMapReply = packed record
    Header: TMsgHeader;
    Count: Word;
    MapName: String[MAPNAME_CHARS];
  end;

  // VOTING KICK TYPE
  PMsg_VoteKick = ^TMsg_VoteKick;
  TMsg_VoteKick = packed record
    Header: TMsgHeader;
    Ban: Byte;
    Num: Byte;
    Reason: array[0..REASON_CHARS - 1] of char;
  end;

  // MESSAGE PACKET
  PMsg_ServerSpecialMessage = ^TMsg_ServerSpecialMessage;
  TMsg_ServerSpecialMessage = packed record
    Header: TMsgHeader;
    MsgType: Byte;  // 0 - console, 1 - big text, 2 - world text
    LayerId: Byte;  // only used for big text and world text
    Delay: Integer;
    Scale: Single;
    Color: UInt32;
    X, Y: Single;
    Text: array[0..0] of Char;
  end;

  // HIDE/SHOW WEAPON IN MENU FOR SPECIFIC PLAYER
  PMsg_WeaponActiveMessage = ^TMsg_WeaponActiveMessage;

  TMsg_WeaponActiveMessage = packed record
    Header: TMsgHeader;
    Active, Weapon: Byte;
  end;

  PMsg_JoinServer = ^TMsg_JoinServer;
  TMsg_JoinServer = packed record
    Header: TMsgHeader;
    IP: LongWord;
    Port: Word;
    ShowMsg: array[0..50] of char;
  end;

  PMsg_PlaySound = ^TMsg_PlaySound;
  TMsg_PlaySound = packed record  // Server -> Client
    Header: TMsgHeader;
    Name: array[0..26] of char;
    Emitter: TVector2;
  end;

  PMsg_VoiceData = ^TMsg_VoiceData;
  TMsg_VoiceData = packed record
    Header: TMsgHeader;
    Speaker: Byte;
    Data: array[0..0] of Char;
  end;

var
  MainTickCounter: Integer;
  // Stores all network-generated TPlayer objects
  Players: TPlayers;

  {$IFNDEF SERVER}
  ClientTickCount, LastHeartBeatCounter: LongInt;
  ClientPlayerReceivedCounter: Integer;
  ClientPlayerReceived, ClientPlayerSent: Boolean;
  ClientVarsRecieved: Boolean;
  RequestingGame: Boolean;
  NoHeartbeatTime: Integer = 0;
  ReceivedUnAccepted: Boolean;
  VoteMapName: String;
  VoteMapCount: Word;
  {$ELSE}
  // We're assigning a dummy player class to all sprites that are currently not being controlled
  // by a player. This avoids nasty surprises with older code that reads .Player despite .Active
  // being false. A player object is swapped in by CreateSprite as needed. For bots we simply leave
  // the bot object and free it when it is replaced.
  // Albeit this approach is very robust I'd prefer if we get rid of this and fix all .Active
  // checks (if any) later. Alternatively we could move a good bit if info from Player to Sprite.
  DummyPlayer: TPlayer;

  ServerTickCounter: Integer;
  NoClientUpdateTime: array[1..MAX_PLAYERS] of Integer;
  MessagesASecNum: array[1..MAX_PLAYERS] of Integer;
  FloodWarnings: array[1..MAX_PLAYERS] of Byte;
  PingWarnings: array[1..MAX_PLAYERS] of Byte;
  BulletTime: array[1..MAX_PLAYERS] of Integer;
  GrenadeTime: array[1..MAX_PLAYERS] of Integer;
  KnifeCan: array[1..MAX_PLAYERS] of Boolean;
  {$ENDIF}

  PlayersNum, BotsNum, SpectatorsNum: Integer;
  PlayersTeamNum: array[1..4] of Integer;

  PingTicksAdd: Integer = {$IFDEF SERVER} 0 {$ELSE} 2 {$ENDIF};

  {$IFDEF SCRIPT}
  ForceWeaponCalled: Boolean;
  {$ENDIF}

implementation

uses
  {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} Game, TraceLog, Demo,
  {$IFNDEF SERVER}
  NetworkClientSprite, NetworkClientConnection, NetworkClientThing,
  NetworkClientGame, NetworkClientFunctions, NetworkClientHeartbeat,
  NetworkClientMessages, NetworkClientBullet
  {$ELSE}
  NetworkServerSprite, NetworkServerThing, NetworkServerMessages,
  NetworkServerBullet, NetworkServerConnection, NetworkServerGame,
  NetworkServerFunctions
  {$IFDEF SCRIPT}, ScriptDispatcher{$ENDIF}
  {$ENDIF}
  ;

procedure ProcessEventsCallback(pInfo: PSteamNetConnectionStatusChangedCallback_t); cdecl;
begin
  UDP.ProcessEvents(pInfo);
end;

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
procedure DebugNet(nType: ESteamNetworkingSocketsDebugOutputType; pszMsg: PChar); cdecl;
begin
  Debug('[NET DEBUG] ' + pszMsg);
end;
{$POP}

constructor TNetwork.Create();
{$IFNDEF STEAM}
var
  ErrorMsg: SteamNetworkingErrMsg;
{$ENDIF}
begin
  FInit := True;

  {$IFNDEF STEAM}
  if not GameNetworkingSockets_Init(Nil, @ErrorMsg) then
    raise Exception.Create('GameNetworkingSockets_Init has failed: ' + PChar(ErrorMsg));
  {$ENDIF}

  {$IFDEF STEAM}
  {$IFNDEF SERVER}
  NetworkingSockets := SteamAPI_SteamNetworkingSockets_SteamAPI_v012();
  {$ELSE}
  NetworkingSockets := SteamAPI_SteamGameServerNetworkingSockets_SteamAPI_v012();
  {$ENDIF}
  {$ELSE}
  NetworkingSockets := SteamAPI_SteamNetworkingSockets_v009();
  {$ENDIF}
  if NetworkingSockets = Nil then
    raise Exception.Create('NetworkingSockets is null');

  {$IFDEF STEAM}
  NetworkingUtils := SteamAPI_SteamNetworkingUtils_SteamAPI_v004();
  {$ELSE}
  NetworkingUtils := SteamAPI_SteamNetworkingUtils_v003();
  {$ENDIF}
  if NetworkingUtils = Nil then
    raise Exception.Create('NetworkingUtils is null');

  NetworkingUtils.SetGlobalCallback_SteamNetConnectionStatusChanged(@ProcessEventsCallback);

  NetworkingUtils.SetDebugOutputFunction(k_ESteamNetworkingSocketsDebugOutputType_Msg, DebugNet);
  //NetworkingUtils.SetDebugOutputFunction(k_ESteamNetworkingSocketsDebugOutputType_Everything, DebugNet);
end;

destructor TNetwork.Destroy();
begin
  Disconnect(True);
  inherited Destroy();
end;


function TNetwork.Disconnect(Now: Boolean): Boolean;
{$IFDEF SERVER}
var
  DstPlayer: TPlayer;
{$ENDIF}
begin
  Result := False;
  {$IFDEF SERVER}
  if (FHost <> k_HSteamNetPollGroup_Invalid) then
  begin
    for DstPlayer in Players do
    begin
      if FPeer <> 0 then
        NetworkingSockets.CloseConnection(DstPlayer.Peer, 0, '', not Now)
    end;
  Result := True;
  end;
  {$ELSE}
  NetworkingSockets.CloseConnection(FPeer, 0, '', not Now)
  {$ENDIF}
end;

procedure TNetwork.FlushMsg();
begin
  if FPeer <> k_HSteamNetConnection_Invalid then
    NetworkingSockets.FlushMessagesOnConnection(FPeer);
end;

function TNetwork.GetDetailedConnectionStatus(hConn: HSteamNetConnection): String;
var
  StatsText: TStatsString;
begin
  StatsText := Default(TStatsString);
  if NetworkingSocket.GetDetailedConnectionStatus(hConn, StatsText, 2048) = 0 then
    Result := StatsText
  else
    Result := '';
end;

function TNetwork.GetConnectionRealTimeStatus(hConn: HSteamNetConnection): SteamNetConnectionRealTimeStatus_t;
begin
  Result := Default(SteamNetConnectionRealTimeStatus_t);
  NetworkingSocket.GetConnectionRealTimeStatus(hConn, @Result, 0, Nil);
end;

procedure TNetwork.SetConnectionName(hConn: HSteamNetConnection; Name: AnsiString);
begin
  NetworkingSocket.SetConnectionName(hConn, PChar(Name));
end;

function TNetwork.GetStringAddress(pAddress: PSteamNetworkingIPAddr; Port: Boolean): AnsiString;
var
  TempIP: array[0..128] of Char;
begin
  pAddress.ToString(@TempIP, 128, Port);
  Result := TempIP;
end;

function TNetwork.SetGlobalConfigValueInt32(eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
begin
   Result := NetworkingUtils.SetConfigValue(eValue, k_ESteamNetworkingConfig_Global, 0, k_ESteamNetworkingConfig_Int32, @val);
end;

function TNetwork.SetGlobalConfigValueFloat(eValue: ESteamNetworkingConfigValue; val: Single): Boolean;
begin
   Result := NetworkingUtils.SetConfigValue(eValue, k_ESteamNetworkingConfig_Global, 0, k_ESteamNetworkingConfig_Float, @val);
end;

function TNetwork.SetGlobalConfigValueString(eValue: ESteamNetworkingConfigValue; val: PChar): Boolean;
begin
   Result := NetworkingUtils.SetConfigValue(eValue, k_ESteamNetworkingConfig_Connection, 0, k_ESteamNetworkingConfig_String, @val);
end;
function TNetwork.SetConnectionConfigValueInt32(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
begin
   Result := NetworkingUtils.SetConfigValue(eValue, k_ESteamNetworkingConfig_Connection, hConn, k_ESteamNetworkingConfig_String, @val);
end;

function TNetwork.SetConnectionConfigValueFloat(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
begin
   Result := NetworkingUtils.SetConfigValue(eValue, k_ESteamNetworkingConfig_Connection, hConn, k_ESteamNetworkingConfig_String, @val);
end;

function TNetwork.SetConnectionConfigValueString(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
begin
   Result := NetworkingUtils.SetConfigValue(eValue, k_ESteamNetworkingConfig_Connection, hConn, k_ESteamNetworkingConfig_String, @val);
end;

procedure TNetwork.SetDebugLevel(Level: ESteamNetworkingSocketsDebugOutputType);
begin
  NetworkingUtils.SetDebugOutputFunction(Level, DebugNet);
end;

{$IFNDEF SERVER}
constructor TClientNetwork.Create();
begin
  inherited Create();
end;

destructor TClientNetwork.Destroy;
begin
  Inherited;
  //Self.Free;
end;

procedure TClientNetwork.ProcessLoop;
var
  NumMsgs: Integer;
  IncomingMsg: PSteamNetworkingMessage_t;
begin
  {$IFNDEF STEAM}
  NetworkingSockets.RunCallbacks();
  {$ENDIF}
  if FPeer = k_HSteamNetConnection_Invalid then
    Exit;

  NumMsgs := NetworkingSockets.ReceiveMessagesOnConnection(FPeer, @IncomingMsg, 1);

  if NumMsgs = 0 then
    Exit
  else if NumMsgs < 0 then
  begin
    WriteLn('[NET] Failed to poll messages');
    Exit;
  end else
    HandleMessages(IncomingMsg);
end;


procedure TClientNetwork.ProcessEvents(pInfo: PSteamNetConnectionStatusChangedCallback_t);
begin
  if pInfo.m_hConn = k_HSteamNetConnection_Invalid then
  begin
    WriteLn('[NET] Invalid connection handle');
    Exit;
  end;
  {$IFDEF DEVELOPMENT}
  Debug('[NET] Received SteamNetConnectionStatusChangedCallback_t ' + ToStr(pInfo^, TypeInfo(SteamNetConnectionStatusChangedCallback_t)));
  {$ENDIF}
  // Make sure it's for us
  if pInfo.m_hConn = FPeer then
  begin
    case pInfo.m_info.m_eState of
      k_ESteamNetworkingConnectionState_None:
      begin
        FPeer := k_HSteamNetConnection_Invalid;
      end;
      k_ESteamNetworkingConnectionState_ClosedByPeer, k_ESteamNetworkingConnectionState_ProblemDetectedLocally:
      begin
        if pInfo.m_eOldState = k_ESteamNetworkingConnectionState_Connecting then
          WriteLn('[NET] Connection error #1', pInfo.m_info.m_szEndDebug)
        else if pInfo.m_info.m_eState = k_ESteamNetworkingConnectionState_ProblemDetectedLocally then
          WriteLn('[NET] Connection error #2', pInfo.m_info.m_szEndDebug)
        else
          WriteLn('[NET] Connection error #3', pInfo.m_info.m_szEndDebug);

        // No need to inform players about closed connection when
        // they already received the UnAccepted packet.
        if not ReceivedUnAccepted then
          RenderGameInfo('Network error: ' + WideString(pInfo.m_info.m_szEndDebug));
        NetworkingSockets.CloseConnection(pInfo.m_hConn, 0, nil, false);
      end;
      k_ESteamNetworkingConnectionState_Connecting:
      begin
        WriteLn('[NET] Connecting to: ' + PChar(pInfo.m_info.m_szConnectionDescription));
      end;
      k_ESteamNetworkingConnectionState_Connected:
      begin
        WriteLn('[NET] Connected to the server');
        ClientRequestGame;
      end;
    else
      //break;
    end;
  end;
end;

function TClientNetwork.Connect(Host: String; Port: Word): Boolean;
var
  ServerAddress: SteamNetworkingIPAddr;
  InitSettings: SteamNetworkingConfigValue_t;
begin
  Debug('Connecting to: ' + Host + ':' + IntToStr(Port));

  Result := True;

  ServerAddress.Clear;
  ServerAddress.ParseString(PChar(Host + ':' + IntToStr(Port)));

  NetworkingSockets.InitAuthentication();

  InitSettings.m_eValue := k_ESteamNetworkingConfig_IP_AllowWithoutAuth;
  InitSettings.m_eDataType := k_ESteamNetworkingConfig_Int32;
  InitSettings.m_int32 := 1;

  FPeer := NetworkingSockets.ConnectByIPAddress(ServerAddress, 1, @InitSettings);

  if FPeer = k_HSteamNetConnection_Invalid then
  begin
    MainConsole.Console('[NET] Failed to connect to server: ' + UDP.GetStringAddress(@ServerAddress, True), WARNING_MESSAGE_COLOR);
    Result := False;
    Exit;
  end;

  FAddress := ServerAddress;
end;

procedure TClientNetwork.HandleMessages(IncomingMsg: PSteamNetworkingMessage_t);
var
  PacketHeader: PMsgHeader;
begin
  if IncomingMsg^.m_cbSize < SizeOf(TMsgHeader) then
    Exit; // truncated packet

  PacketHeader := PMsgHeader(IncomingMsg^.m_pData);

  if DemoRecorder.Active then
    DemoRecorder.SaveRecord(IncomingMsg^.m_pData^, IncomingMsg^.m_cbSize);

  case PacketHeader.ID of
    MsgID_PlayersList:
      ClientHandlePlayersList(IncomingMsg);

    MsgID_UnAccepted:
      ClientHandleUnAccepted(IncomingMsg);

    MsgID_NewPlayer:
      ClientHandleNewPlayer(IncomingMsg);

    {$IFDEF ENABLE_FAE}
    MsgID_FaeData:
      ClientHandleFaeChallenge(IncomingMsg);
    {$ENDIF}

    // PLAYING GAME MESSAGES

    MsgID_ServerSpriteSnapshot:
      ClientHandleServerSpriteSnapshot(IncomingMsg);

    MsgID_ServerSpriteSnapshot_Major:
      ClientHandleServerSpriteSnapshot_Major(IncomingMsg);

    MsgID_ServerSkeletonSnapshot:
      ClientHandleServerSkeletonSnapshot(IncomingMsg);

    MsgID_BulletSnapshot:
      ClientHandleBulletSnapshot(IncomingMsg);

    MsgID_HeartBeat:
      ClientHandleHeartBeat(IncomingMsg);

    MsgID_ServerThingSnapshot:
      ClientHandleServerThingSnapshot(IncomingMsg);

    MsgID_ServerThingMustSnapshot:
      ClientHandleServerThingMustSnapshot(IncomingMsg);

    MsgID_ThingTaken:
      ClientHandleThingTaken(IncomingMsg);

    MsgID_SpriteDeath:
      ClientHandleSpriteDeath(IncomingMsg);

    MsgID_ServerDisconnect:
      ClientHandleServerDisconnect(IncomingMsg);

    MsgID_PlayerDisconnect:
      ClientHandlePlayerDisconnect(IncomingMsg);

    MsgID_Delta_Movement:
      ClientHandleDelta_Movement(IncomingMsg);

    MsgID_Delta_MouseAim:
      ClientHandleDelta_MouseAim(IncomingMsg);

    MsgID_Delta_Weapons:
      ClientHandleDelta_Weapons(IncomingMsg);

    MsgID_Delta_Helmet:
      ClientHandleDelta_Helmet(IncomingMsg);

    MsgID_ChatMessage:
      ClientHandleChatMessage(IncomingMsg);

    MsgID_Ping:
      ClientHandlePing(IncomingMsg);

    MsgID_MapChange:
      ClientHandleMapChange(IncomingMsg);

    MsgID_FlagInfo:
      ClientHandleFlagInfo(IncomingMsg);

    MsgID_IdleAnimation:
      ClientHandleIdleAnimation(IncomingMsg);

    MsgID_VoteOn:
      ClientHandleVoteOn(IncomingMsg);

    MsgID_ClientSpriteSnapshot_Dead:
      ClientHandleClientSpriteSnapshot_Dead(IncomingMsg);

    MsgID_ServerVars:
      ClientHandleServerVars(IncomingMsg);

    MsgID_ServerSyncMsg:
      ClientHandleServerSyncMsg(IncomingMsg);

    MsgID_ForcePosition:
      ClientHandleForcePosition(IncomingMsg);

    MsgID_ForceVelocity:
      ClientHandleForceVelocity(IncomingMsg);

    MsgID_ForceWeapon:
      ClientHandleForceWeapon(IncomingMsg);

    MsgID_SpecialMessage:
      ClientHandleSpecialMessage(IncomingMsg);

    MsgID_WeaponActiveMessage:
      ClientHandleWeaponActiveMessage(IncomingMsg);

    MsgID_ClientFreeCam:
      ClientHandleClientFreeCam(IncomingMsg);

    MsgID_VoteOff:
      ClientHandleVoteOff;

    MsgID_VoteMapReply:
      ClientHandleVoteResponse(IncomingMsg);

    MsgID_JoinServer:
      ClientHandleJoinServer(IncomingMsg);

    MsgID_PlaySound:
      ClientHandlePlaySound(IncomingMsg);

    MsgID_SyncCvars:
      ClientHandleSyncCvars(IncomingMsg);

    {$IFDEF STEAM}
    MsgID_VoiceData:
      ClientHandleVoiceData(IncomingMsg);
    {$ENDIF}
  end;

  if not DemoPlayer.Active then
    IncomingMsg.Release();
end;

function TClientNetwork.SendData(var Data; Size: Integer; Flags: Integer): Boolean;
begin
  Result := False;

  if Size < SizeOf(TMsgHeader) then
    Exit; // truncated packet

  if FPeer = k_HSteamNetConnection_Invalid then
    Exit; // not connected

  NetworkingSockets.SendMessageToConnection(FPeer, @Data, Size, Flags, nil);
  Result := True;
end;

{$ELSE}
constructor TServerNetwork.Create(Host: String; Port: Word);
var
  ServerAddress: SteamNetworkingIPAddr;
  InitSettings: SteamNetworkingConfigValue_t;
  //TempIP: array[0..128] of Char;
begin
  inherited Create();
  if FInit then
  begin
    Players := TFPGObjectList<TPlayer>.Create;
    ServerAddress.Clear;
    ServerAddress.ParseString(PChar(Host + ':' + IntToStr(Port)));
    InitSettings.m_eValue := k_ESteamNetworkingConfig_IP_AllowWithoutAuth;
    InitSettings.m_eDataType := k_ESteamNetworkingConfig_Int32;
    InitSettings.m_int32 := 1;
    //{$IFDEF STEAM}
    //if sv_steamonly.Value then
    //  InitSettings.m_int32 := 0
    //else
    //{$ENDIF}
    FHost := NetworkingSockets.CreateListenSocketIP(ServerAddress, 1, @InitSettings);

    if FHost = k_HSteamListenSocket_Invalid then
      Exit;

    FPollGroup := NetworkingSockets.CreatePollGroup();
    if FPollGroup = k_HSteamNetPollGroup_Invalid then
    begin
      Debug('[NET] Failed to create poll group');
      Exit;
    end else
    begin
      NetworkingSockets.GetListenSocketAddress(FHost, @FAddress);
    end;
  end;

  if FHost <> k_HSteamNetPollGroup_Invalid then
    Active := True;

end;

procedure TServerNetwork.ProcessLoop;
var
  NumMsgs: Integer;
  IncomingMsg: PSteamNetworkingMessage_t;
begin
  {$IFNDEF STEAM}
  NetworkingSockets.RunCallbacks();
  {$ENDIF}

  NumMsgs := NetworkingSockets.ReceiveMessagesOnPollGroup(FPollGroup, @IncomingMsg, 1);

  if NumMsgs = 0 then
    Exit
  else if NumMsgs < 0 then
  begin
    Debug('[NET] Failed to poll messages');
    Exit;
  end else
    HandleMessages(IncomingMsg);
end;

procedure TServerNetwork.ProcessEvents(pInfo: PSteamNetConnectionStatusChangedCallback_t);
var
  info: array[0..128] of Char;
  Player: TPlayer;
  TempIP: TIPString;
begin
  TempIP := Default(TIPString);
  {$IFDEF DEVELOPMENT}
  Debug('[NET] Received SteamNetConnectionStatusChangedCallback_t ' + ToStr(pInfo^, TypeInfo(SteamNetConnectionStatusChangedCallback_t)));
  {$ENDIF}
  case pInfo.m_info.m_eState of
    k_ESteamNetworkingConnectionState_None:
    begin
      FPeer := k_HSteamNetConnection_Invalid;
      WriteLn('[Net] Destroying peer handle');
    end;
    k_ESteamNetworkingConnectionState_ClosedByPeer, k_ESteamNetworkingConnectionState_ProblemDetectedLocally:
    begin
      if pInfo.m_info.m_nUserData = 0 then
      begin
        NetworkingSockets.CloseConnection(pInfo.m_hConn, 0, '', False);
        Exit;
      end;
      // NOTE that this is not called for ordinary disconnects, where we use enet's disconnect_now,
      // which does not generate additional events. Cleanup of Player is still performed explicitly.
      Player := TPlayer(pInfo.m_info.m_nUserData);

      if Player = nil then
      begin
        NetworkingSockets.CloseConnection(pInfo.m_hConn, 0, '', False);
        Exit;
      end;

      // the sprite may be zero if we're still in the setup phase
      if Player.SpriteNum <> 0 then
      begin
        MainConsole.Console(Player.Name + ' could not respond', WARNING_MESSAGE_COLOR);
        ServerPlayerDisconnect(Player.SpriteNum, KICK_NORESPONSE);
        {$IFDEF SCRIPT}
        ScrptDispatcher.OnLeaveGame(Player.SpriteNum, False);
        {$ENDIF}
        Sprite[Player.SpriteNum].Kill;
        Sprite[Player.SpriteNum].Player := DummyPlayer;
      end;

      WriteLn('[NET] Connection lost: ' + IntToStr(pInfo.m_info.m_eEndReason)  + PChar(pInfo.m_info.m_szConnectionDescription));

      // call destructor; this releases any additional resources managed for the connection, such
      // as anti-cheat handles etc.
      Players.Remove(Player);

      NetworkingSockets.CloseConnection(pInfo.m_hConn, 0, '', false);
    end;
    k_ESteamNetworkingConnectionState_Connecting:
    begin
      if pInfo.m_info.m_hListenSocket <> 0 then
      begin
        WriteLn('[NET] Connection request from: ' + PChar(pInfo.m_info.m_szConnectionDescription));

        //  A new connection arrives on a listen socket. m_info.m_hListenSocket will be set, m_eOldState = k_ESteamNetworkingConnectionState_None,
        //and m_info.m_eState = k_ESteamNetworkingConnectionState_Connecting. See AcceptConnection.
        //  and (pInfo.m_info.m_eState = k_ESteamNetworkingConnectionState_Connecting)
        if (pInfo.m_eOldState = k_ESteamNetworkingConnectionState_None) then
        begin
          if not NetworkingSockets.SetConnectionPollGroup(pInfo.m_hConn, FPollGroup) then
          begin
            WriteLn('[NET] Failed to set poll group for user');
            NetworkingSockets.CloseConnection(pInfo.m_hConn, 0, nil, False);
            Exit;
          end;
          NetworkingSockets.AcceptConnection(pInfo.m_hConn);
          pInfo.m_info.m_identityRemote.ToString(@info, 1024);
          NetworkingSockets.SetConnectionUserData(pInfo.m_hConn, 0);
        end;
      end;
    end;
    k_ESteamNetworkingConnectionState_Connected:
    begin
      //if pInfo.m_eOldState = k_ESteamNetworkingConnectionState_Connecting then
      begin
        Player := TPlayer.Create;
        Player.peer := pInfo.m_hConn;
        pInfo.m_info.m_addrRemote.ToString(TempIP, 128, False);
        Player.IP := TempIP;
        Player.Port := pInfo.m_info.m_addrRemote.m_port;
        {$IFDEF STEAM}
        Player.SteamID := CSteamID(pInfo.m_info.m_identityRemote.GetSteamID64);
        {$ENDIF}
        {$HINTS OFF} // Conversion between ordinals and pointers is not portable
        NetworkingSockets.SetConnectionUserData(pInfo.m_hConn, PtrUint(Pointer(Player)));
        {$HINTS ON}
        WriteLn('[NET] Connection accepted: ' + PChar(pInfo.m_info.m_szConnectionDescription));
        Players.Add(Player);
      end;
    end;
  else
    //break;
  end;

end;

procedure TServerNetwork.HandleMessages(IncomingMsg: PSteamNetworkingMessage_t);
var
  Player: TPlayer;
  PacketHeader: PMsgHeader;
begin
  if IncomingMsg^.m_cbSize < SizeOf(TMsgHeader) then
    Exit; // truncated packet

  if IncomingMsg^.m_nConnUserData = -1 then
    Exit;

  Player := TPlayer(IncomingMsg^.m_nConnUserData);
  PacketHeader := PMsgHeader(IncomingMsg^.m_pData);

  case PacketHeader.ID of
    MsgID_RequestGame:
      // only allowed if the player has not yet joined the game
      if (Player.SpriteNum = 0) and (not Player.GameRequested) then
        ServerHandleRequestGame(IncomingMsg);

    MsgID_PlayerInfo:
      // allowed once after RequestGame was received, sets SpriteNum
      if (Player.SpriteNum = 0) and Player.GameRequested then
        ServerHandlePlayerInfo(IncomingMsg);

    {$IFDEF ENABLE_FAE}
    MsgID_FaeData:
      ServerHandleFaeResponse(IncomingMsg);
    {$ENDIF}
  end;

  // all the following commands can only be issued after the player has joined the game.
  if (Player.SpriteNum = 0) or (Sprite[Player.SpriteNum].Player <> Player) then
    Exit;

  case PacketHeader.ID of
    MsgID_ClientSpriteSnapshot:
      ServerHandleClientSpriteSnapshot(IncomingMsg);

    MsgID_ClientSpriteSnapshot_Mov:
      ServerHandleClientSpriteSnapshot_Mov(IncomingMsg);

    MsgID_ClientSpriteSnapshot_Dead:
      ServerHandleClientSpriteSnapshot_Dead(IncomingMsg);

    MsgID_PlayerDisconnect:
      ServerHandlePlayerDisconnect(IncomingMsg);

    MsgID_ChatMessage:
      ServerHandleChatMessage(IncomingMsg);

    MsgID_Pong:
      ServerHandlePong(IncomingMsg);

    MsgID_BulletSnapshot:
      ServerHandleBulletSnapshot(IncomingMsg);

    MsgID_RequestThing:
      ServerHandleRequestThing(IncomingMsg);

    MsgID_VoteKick:
      ServerHandleVoteKick(IncomingMsg);

    MsgID_VoteMap:
      ServerHandleVoteMap(IncomingMsg);

    MsgID_ChangeTeam:
      ServerHandleChangeTeam(IncomingMsg);

    MsgID_ClientFreeCam:
      ServerHandleClientFreeCam(IncomingMsg);

    {$IFDEF STEAM}
    MsgID_VoiceData:
      ServerHandleVoiceData(IncomingMsg);
    {$ENDIF}
  end;

  IncomingMsg.Release();
end;

destructor TServerNetwork.Destroy;
begin
  if FHost <> k_HSteamNetConnection_Invalid then
    NetworkingSockets.CloseListenSocket(FHost);

  if FPollGroup <> k_HSteamNetPollGroup_Invalid then
    NetworkingSockets.DestroyPollGroup(FPollGroup);

  Players.Clear;

  Inherited;
end;

function TServerNetwork.SendData(var Data; Size: Integer; Peer: HSteamNetConnection; Flags: Integer): Boolean;
begin
  Result := False;

  if Size < SizeOf(TMsgHeader) then
    Exit; // truncated packet

  if FHost = k_HSteamNetConnection_Invalid then
    Exit; // server not set up

  if DemoRecorder.Active then
    if Peer = High(LongWord) then
      DemoRecorder.SaveRecord(Data, Size);

  NetworkingSockets.SendMessageToConnection(Peer, @Data, Size, Flags, nil);

  Result := True;
end;

procedure TServerNetwork.UpdateNetworkStats(Player: Byte);
var
  Stats: SteamNetConnectionRealTimeStatus_t;
begin
  Stats := GetConnectionRealTimeStatus(Sprite[Player].Player.Peer);
  Sprite[Player].Player.RealPing := Stats.m_nPing;
  if Stats.m_flConnectionQualityLocal > 0.0 then
    Sprite[Player].Player.ConnectionQuality := Trunc(Stats.m_flConnectionQualityLocal * 100)
  else
    Sprite[Player].Player.ConnectionQuality := 0;

end;

constructor TPlayer.Create();
begin
end;

destructor TPlayer.Destroy();
begin
end;

{$ENDIF}

function TPlayer.Clone: TPlayer;
begin
  // NOTE that only fields used by TScriptNewPlayer really matter here, but we clone the whole
  // thing for consistency. Obviously don't clone handles etc. unless they can be duplicated.

  Result := TPlayer.Create;

  Result.Name := Self.Name;
  Result.ShirtColor := Self.ShirtColor;
  Result.PantsColor := Self.PantsColor;
  Result.SkinColor := Self.SkinColor;
  Result.HairColor := Self.HairColor;
  Result.JetColor := Self.JetColor;
  Result.Kills := Self.Kills;
  Result.Deaths := Self.Deaths;
  Result.Flags := Self.Flags;
  Result.PingTicks := Self.PingTicks;
  Result.PingTicksB := Self.PingTicksB;
  Result.PingTime := Self.PingTime;
  Result.RealPing := Self.RealPing;
  Result.ConnectionQuality := Self.ConnectionQuality;
  Result.Ping := Self.Ping;
  Result.Team := Self.Team;
  Result.ControlMethod := Self.ControlMethod;
  Result.Chain := Self.Chain;
  Result.HeadCap := Self.HeadCap;
  Result.HairStyle := Self.HairStyle;
  Result.SecWep := Self.SecWep;
  Result.Camera := Self.Camera;
  Result.Muted := Self.Muted;
  Result.SpriteNum := Self.SpriteNum;
  Result.DemoPlayer := Self.DemoPlayer;
  {$IFDEF STEAM}
  Result.SteamID := Self.SteamID;
  Result.SteamStats := Self.SteamStats;
  {$ENDIF}

  {$IFDEF SERVER}
  Result.IP := Self.IP;
  Result.Port := Self.Port;
  {$IFDEF ENABLE_FAE}
  Result.FaeResponsePending := Self.FaeResponsePending;
  Result.FaeKicked := Self.FaeKicked;
  Result.FaeTicks := Self.FaeTicks;
  Result.FaeSecret := Self.FaeSecret;
  {$ENDIF}
  Result.hwid := Self.hwid;
  Result.PlayTime := Self.PlayTime;
  Result.GameRequested := Self.GameRequested;
  Result.ChatWarnings := Self.ChatWarnings;
  Result.TKWarnings := Self.TKWarnings;
  Result.ScoresPerSecond := Self.ScoresPerSecond;
  Result.GrabsPerSecond := Self.GrabsPerSecond;
  Result.GrabbedInBase := Self.GrabbedInBase;
  Result.StandingPolyType := Self.StandingPolyType;
  Result.KnifeWarnings := Self.KnifeWarnings;
  {$ENDIF}
end;

procedure TPlayer.ApplyShirtColorFromTeam;
begin
  {$IFDEF SERVER}
  if sv_teamcolors.Value and IsTeamGame() then
    case Self.Team of
      1: Self.ShirtColor := $FFD20F05;
      2: Self.ShirtColor := $FF151FD9;
      3: Self.ShirtColor := $FFD2D205;
      4: Self.ShirtColor := $FF05D205;
    end;
  {$ENDIF}
end;

end.

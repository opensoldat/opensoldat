{*******************************************************}
{                                                       }
{       ScriptPlayer unit for SOLDAT                    }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit ScriptPlayer;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  ScriptCore3Api,
  ScriptObject,
  ScriptWeapon,
  Sprites,
  Things,
  SysUtils,
  Vector;

type

  PSprite = ^TSprite;

  TScriptActivePlayer = class;

  TJoinType = (TJoinNormal, TJoinSilent);

  TOnFlagGrab = procedure(Player: TScriptActivePlayer; TFlag: TScriptActiveFlag;
    Team: Byte; GrabbedInBase: Boolean) of object;
  TOnFlagReturn = procedure(Player: TScriptActivePlayer; Flag: TScriptActiveFlag;
    Team: Byte) of object;
  TOnFlagScore = procedure(Player: TScriptActivePlayer; Flag: TScriptActiveFlag;
    Team: Byte) of object;
  TOnFlagDrop = procedure(Player: TScriptActivePlayer; Flag: TScriptActiveFlag;
    Team: Byte; Thrown: Boolean) of object;
  TOnKitPickup = procedure(Player: TScriptActivePlayer;
    Kit: TScriptActiveObject) of object;
  TOnBeforeRespawn = function(Player: TScriptActivePlayer): TVector2 of object;
  TOnAfterRespawn = procedure(Player: TScriptActivePlayer) of object;
  TOnDamage = function(Shooter, Victim: TScriptActivePlayer; Damage: Single;
    BulletID: Byte): Single of object;
  TOnKill = procedure(Killer, Victim: TScriptActivePlayer;
    WeaponType: Byte) of object;
  TOnWeaponChange = procedure(Player: TScriptActivePlayer;
    Primary, Secondary: TScriptPlayerWeapon) of object;
  TOnVoteMapStart = function(Player: TScriptActivePlayer;
    Map: string): Boolean of object;
  TOnVoteKickStart = function(Player, Victim: TScriptActivePlayer;
    Reason: string): Boolean of object;
  TOnVoteMap = procedure(Player: TScriptActivePlayer; Map: string) of object;
  TOnVoteKick = procedure(Player, Victim: TScriptActivePlayer) of object;
  TOnSpeak = procedure(Player: TScriptActivePlayer; Text: string) of object;
  TOnCommand = function(Player: TScriptActivePlayer; Command: string): Boolean of object;

  TKickReason = (TKickNoResponse, TKickNoCheatResponse,
    TKickChangeTeam, TKickPing, TKickFlooding, TKickConsole,
    TKickConnectionCheat, TKickCheat, TKickLeft, TKickVoted,
    TKickAC, TKickSilent);

  TScriptPlayer = class(TObject)
  protected
    FSpritePtr: PSprite;
    FPrimary: TScriptWeapon;
    FSecondary: TScriptWeapon;
    function GetSprite: TSprite;
    function GetTeam: Byte;
    function GetName: string;
    function GetAlive: Boolean;
    function GetHealth: Single;
    procedure SetHealth(Health: Single);
    function GetVest: Single;
    procedure SetVest(Vest: Single);
    function GetPrimary: TScriptPrimaryPlayerWeapon;
    function GetSecondary: TScriptSecondaryPlayerWeapon;
    function GetShirtColor: Longword;
    function GetPantsColor: Longword;
    function GetSkinColor: Longword;
    function GetHairColor: Longword;
    function GetFavouriteWeapon: string;
    procedure SetFavouriteWeapon(Weapon: string);
    function GetChosenSecondaryWeapon: Byte;
    function GetFriend: string;
    procedure SetFriend(Friend: string);
    function GetAccuracy: Byte;
    procedure SetAccuracy(Accuracy: Byte);
    function GetShootDead: Boolean;
    procedure SetShootDead(ShootDead: Boolean);
    function GetGrenadeFrequency: Integer;
    procedure SetGrenadeFrequency(Frequency: Integer);
    function GetCamping: Boolean;
    procedure SetCamping(Camping: Boolean);
    function GetOnStartUse: Byte;
    procedure SetOnStartUse(Thing: Byte);
    function GetHairStyle: Byte;
    function GetHeadgear: Byte;
    function GetChain: Byte;
    function GetChatFrequency: Byte;
    procedure SetChatFrequency(Frequency: Byte);
    function GetChatKill: string;
    procedure SetChatKill(Message: string);
    function GetChatDead: string;
    procedure SetChatDead(Message: string);
    function GetChatLowHealth: string;
    procedure SetChatLowHealth(Message: string);
    function GetChatSeeEnemy: string;
    procedure SetChatSeeEnemy(Message: string);
    function GetChatWinning: string;
    procedure SetChatWinning(Message: string);
    function GetAdmin: Boolean;
    procedure SetAdmin(SetAsAdmin: Boolean);
    function GetDummy: Boolean;
    procedure SetDummy(Dummy: Boolean);
  public
    destructor Destroy; override;
    // Not exported
    property Sprite: TSprite read GetSprite;
    property Team: Byte read GetTeam;
    property Name: string read GetName;
    property Alive: Boolean read GetAlive;
    property Health: Single read GetHealth write SetHealth;
    property Vest: Single read GetVest write SetVest;
    property Primary: TScriptPrimaryPlayerWeapon read GetPrimary;
    property Secondary: TScriptSecondaryPlayerWeapon read GetSecondary;
    property ShirtColor: Longword read GetShirtColor;
    property PantsColor: Longword read GetPantsColor;
    property SkinColor: Longword read GetSkinColor;
    property HairColor: Longword read GetHairColor;
    property FavouriteWeapon: string read GetFavouriteWeapon write SetFavouriteWeapon;
    property ChosenSecondaryWeapon: Byte read GetChosenSecondaryWeapon;
    property Friend: string read GetFriend write SetFriend;
    property Accuracy: Byte read GetAccuracy write SetAccuracy;
    property ShootDead: Boolean read GetShootDead write SetShootDead;
    property GrenadeFrequency: Integer read GetGrenadeFrequency write SetGrenadeFrequency;
    property Camping: Boolean read GetCamping write SetCamping;
    property OnStartUse: Byte read GetOnStartUse write SetOnStartUse;
    property HairStyle: Byte read GetHairStyle;
    property Headgear: Byte read GetHeadgear;
    property Chain: Byte read GetChain;
    property ChatFrequency: Byte read GetChatFrequency write SetChatFrequency;
    property ChatKill: string read GetChatKill write SetChatKill;
    property ChatDead: string read GetChatDead write SetChatDead;
    property ChatLowHealth: string read GetChatLowHealth write SetChatLowHealth;
    property ChatSeeEnemy: string read GetChatSeeEnemy write SetChatSeeEnemy;
    property ChatWinning: string read GetChatWinning write SetChatWinning;
    property IsAdmin: Boolean read GetAdmin write SetAdmin;
    property Dummy: Boolean read GetDummy write SetDummy;
  end;

  TScriptNewPlayer = class(TScriptPlayer)
  protected
    procedure SetName(Name: string);
    procedure SetTeam(Team: Byte);
    procedure SetHealth(Health: Single);
    function GetPrimary: TScriptWeapon;
    procedure SetPrimary(Primary: TScriptWeapon);
    function GetSecondary: TScriptWeapon;
    procedure SetSecondary(Secondary: TScriptWeapon);
    procedure SetShirtColor(Color: Longword);
    procedure SetPantsColor(Color: Longword);
    procedure SetSkinColor(Color: Longword);
    procedure SetHairColor(Color: Longword);
    procedure SetHairStyle(Style: Byte);
    procedure SetHeadgear(Headgear: Byte);
    procedure SetChain(Chain: Byte);
    function GetDummy: Boolean;
  public
    constructor Create;
    property Name: string read GetName write SetName;
    property Team: Byte read GetTeam write SetTeam;
    property Health: Single read GetHealth write SetHealth;
    property Primary: TScriptWeapon read GetPrimary write SetPrimary;
    property Secondary: TScriptWeapon read GetSecondary write SetSecondary;
    property ShirtColor: Longword read GetShirtColor write SetShirtColor;
    property PantsColor: Longword read GetPantsColor write SetPantsColor;
    property SkinColor: Longword read GetSkinColor write SetSkinColor;
    property HairColor: Longword read GetHairColor write SetHairColor;
    property HairStyle: Byte read GetHairStyle write SetHairStyle;
    property Headgear: Byte read GetHeadgear write SetHeadgear;
    property Chain: Byte read GetChain write SetChain;
    property Dummy: Boolean read GetDummy write SetDummy;
  end;

  TScriptActivePlayer = class(TScriptPlayer)
  private
    FID: Byte;
    FOnFlagGrab: TOnFlagGrab;
    FOnFlagReturn: TOnFlagReturn;
    FOnFlagScore: TOnFlagScore;
    FOnFlagDrop: TOnFlagDrop;
    FOnKitPickup: TOnKitPickup;
    FOnBeforeRespawn: TOnBeforeRespawn;
    FOnAfterRespawn: TOnAfterRespawn;
    FOnDamage: TOnDamage;
    FOnKill: TOnKill;
    FOnWeaponChange: TOnWeaponChange;
    FOnVoteMapStart: TOnVoteMapStart;
    FOnVoteKickStart: TOnVoteKickStart;
    FOnVoteMap: TOnVoteMap;
    FOnVoteKick: TOnVoteKick;
    FOnSpeak: TOnSpeak;
    FOnCommand: TOnCommand;
  protected
    function GetKills: Integer;
    procedure SetKills(Kills: Integer);
    function GetDeaths: Integer;
    procedure SetDeaths(Deaths: Integer);
    function GetPing: Integer;
    procedure SetTeam(Team: Byte);
    function GetActive: Boolean;
    function GetIP: string;
    function GetPort: Word;
    function GetVelX: Single;
    function GetVelY: Single;
    function GetMuted: Boolean;
    procedure SetMuted(Muted: Boolean);
    function GetJets: Integer;
    function GetGrenades: Byte;
    function GetX: Single;
    function GetY: Single;
    function GetMouseAimX: SmallInt;
    procedure SetMouseAimX(AimX: SmallInt);
    function GetMouseAimY: SmallInt;
    procedure SetMouseAimY(AimY: SmallInt);
    function GetFlagger: Boolean;
    function GetTime: Integer;
    function GetOnGround: Boolean;
    function GetProne: Boolean;
    function GetHuman: Boolean;
    function GetDirection: Shortint;
    function GetFlags: Byte;
    function GetHWID: string;
    {$IFDEF STEAM}
    function GetSteamID: Int64;
    {$ENDIF}
    function GetKeyUp: Boolean;
    procedure SetKeyUp(Pressed: Boolean);
    function GetKeyLeft: Boolean;
    procedure SetKeyLeft(Pressed: Boolean);
    function GetKeyRight: Boolean;
    procedure SetKeyRight(Pressed: Boolean);
    function GetKeyShoot: Boolean;
    procedure SetKeyShoot(Pressed: Boolean);
    function GetKeyJetpack: Boolean;
    procedure SetKeyJetpack(Pressed: Boolean);
    function GetKeyGrenade: Boolean;
    procedure SetKeyGrenade(Pressed: Boolean);
    function GetKeyChangeWeap: Boolean;
    procedure SetKeyChangeWeap(Pressed: Boolean);
    function GetKeyThrow: Boolean;
    procedure SetKeyThrow(Pressed: Boolean);
    function GetKeyReload: Boolean;
    procedure SetKeyReload(Pressed: Boolean);
    function GetKeyCrouch: Boolean;
    procedure SetKeyCrouch(Pressed: Boolean);
    function GetKeyProne: Boolean;
    procedure SetKeyProne(Pressed: Boolean);
    function GetKeyFlagThrow: Boolean;
    procedure SetKeyFlagThrow(Pressed: Boolean);
    procedure SetWeaponActive(ID: Byte; Active: Boolean);
  public
    constructor Create(var Sprite: TSprite; ID: Byte); overload;
    function Ban(Time: Integer; Reason: string): Boolean;
    procedure Say(Text: string; MsgType: Byte);
    procedure Damage(Shooter: Byte; Damage: Single);
    procedure Kill();
    procedure BigText(Layer: Byte; Text: string; Delay: Integer;
      Color: Longint; Scale: Single; X, Y: Integer);
    procedure WorldText(Layer: Byte; Text: string; Delay: Integer;
      Color: Longint; Scale, X, Y: Single);
    procedure ForceWeapon(Primary, Secondary: TScriptWeapon);
    procedure ForwardTo(TargetIP: string; TargetPort: Word; Message: string);
    procedure GiveBonus(BType: Byte);
    function Kick(Reason: TKickReason): Boolean;
    procedure PlaySound(Name: string; X, Y: Single);
    procedure Move(X, Y: Single);
    procedure SetVelocity(VelX, VelY: Single);
    procedure Tell(Text: string);
    procedure WriteConsole(Text: string; Color: Longint);
    procedure ChangeTeam(NewTeam: Byte; JoinType: TJoinType);
    property ID: Byte read FID;
    property Team: Byte read GetTeam write SetTeam;
    property Alive: Boolean read GetAlive;
    property Kills: Integer read GetKills write SetKills;
    property Deaths: Integer read GetDeaths write SetDeaths;
    property Ping: Integer read GetPing;
    property Active: Boolean read GetActive;
    property IP: string read GetIP;
    property Port: Word read GetPort;
    property VelX: Single read GetVelX;
    property VelY: Single read GetVelY;
    property Muted: Boolean read GetMuted write SetMuted;
    property Jets: Integer read GetJets;
    property Grenades: Byte read GetGrenades;
    property X: Single read GetX;
    property Y: Single read GetY;
    property MouseAimX: SmallInt read GetMouseAimX write SetMouseAimX;
    property MouseAimY: SmallInt read GetMouseAimY write SetMouseAimY;
    property Flagger: Boolean read GetFlagger;
    property Time: Integer read GetTime;
    property OnGround: Boolean read GetOnGround;
    property IsProne: Boolean read GetProne;
    property Human: Boolean read GetHuman;
    property Direction: Shortint read GetDirection;
    property Flags: Byte read GetFlags;
    property HWID: string read GetHWID;
    {$IFDEF STEAM}
    property SteamID: Int64 read GetSteamID;
    {$ENDIF}
    property KeyUp: Boolean read GetKeyUp write SetKeyUp;
    property KeyLeft: Boolean read GetKeyLeft write SetKeyLeft;
    property KeyRight: Boolean read GetKeyRight write SetKeyRight;
    property KeyShoot: Boolean read GetKeyShoot write SetKeyShoot;
    property KeyJetpack: Boolean read GetKeyJetpack write SetKeyJetpack;
    property KeyGrenade: Boolean read GetKeyGrenade write SetKeyGrenade;
    property KeyChangeWeap: Boolean read GetKeyChangeWeap write SetKeyChangeWeap;
    property KeyThrow: Boolean read GetKeyThrow write SetKeyThrow;
    property KeyReload: Boolean read GetKeyReload write SetKeyReload;
    property KeyCrouch: Boolean read GetKeyCrouch write SetKeyCrouch;
    property KeyProne: Boolean read GetKeyProne write SetKeyProne;
    property KeyFlagThrow: Boolean read GetKeyFlagThrow write SetKeyFlagThrow;
    property WeaponActive[ID: Byte]: Boolean write SetWeaponActive;
    property OnFlagGrab: TOnFlagGrab read FOnFlagGrab write FOnFlagGrab;
    property OnFlagReturn: TOnFlagReturn read FOnFlagReturn write FOnFlagReturn;
    property OnFlagScore: TOnFlagScore read FOnFlagScore write FOnFlagScore;
    property OnFlagDrop: TOnFlagDrop read FOnFlagDrop write FOnFlagDrop;
    property OnKitPickup: TOnKitPickup read FOnKitPickup write FOnKitPickup;
    property OnBeforeRespawn: TOnBeforeRespawn
      read FOnBeforeRespawn write FOnBeforeRespawn;
    property OnAfterRespawn: TOnAfterRespawn read FOnAfterRespawn write FOnAfterRespawn;
    property OnDamage: TOnDamage read FOnDamage write FOnDamage;
    property OnKill: TOnKill read FOnKill write FOnKill;
    property OnWeaponChange: TOnWeaponChange read FOnWeaponChange write FOnWeaponChange;
    property OnVoteMapStart: TOnVoteMapStart read FOnVoteMapStart write FOnVoteMapStart;
    property OnVoteKickStart: TOnVoteKickStart
      read FOnVoteKickStart write FOnVoteKickStart;
    property OnVoteMap: TOnVoteMap read FOnVoteMap write FOnVoteMap;
    property OnVoteKick: TOnVoteKick read FOnVoteKick write FOnVoteKick;
    property OnSpeak: TOnSpeak read FOnSpeak write FOnSpeak;
    property OnCommand: TOnCommand read FOnCommand write FOnCommand;
  end;

  TScriptPlayerAPI = class(TScriptCore3API)
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

uses
  Constants,
  Net,
  NetworkServerFunctions,
  NetworkServerMessages,
  NetworkServerThing,
  NetworkUtils,
  ServerHelper,
  Server,
  Command,
  Game,
  Weapons;

constructor TScriptNewPlayer.Create;
begin
  New(Self.FSpritePtr);
  FillChar(Self.FSpritePtr^, SizeOf(TSprite), #0);
  Self.FPrimary := TScriptPrimaryPlayerWeapon.Create(Self.FSpritePtr^);
  Self.FSecondary := TScriptSecondaryPlayerWeapon.Create(Self.FSpritePtr^);
  Self.FSpritePtr^.Player := TPlayer.Create;
  Self.FSpritePtr^.Brain.TargetNum := 1;
  Self.FSpritePtr^.Brain.WaypointTimeoutCounter := WAYPOINTTIMEOUT;
end;

destructor TScriptPlayer.Destroy;
begin
  if Self.FSpritePtr <> nil then
    Self.FSpritePtr^.Player.Free;
  Self.FPrimary.Free;
  Self.FSecondary.Free;
  FreeMem(Self.FSpritePtr, SizeOf(TSprite));
end;

function TScriptPlayer.GetSprite: TSprite;
begin
  Result := Self.FSpritePtr^;
end;

function TScriptPlayer.GetTeam: Byte;
begin
  Result := Self.FSpritePtr^.Player.Team;
end;

function TScriptPlayer.GetName: string;
begin
  Result := Self.FSpritePtr^.Player.Name;
end;

function TScriptPlayer.GetAlive: Boolean;
begin
  Result := Self.FSpritePtr^.Active and not Self.FSpritePtr^.DeadMeat;
end;

function TScriptPlayer.GetHealth: Single;
begin
  Result := Self.FSpritePtr^.Health;
end;

procedure TScriptPlayer.SetHealth(Health: Single);
begin
  Self.FSpritePtr^.Health := Health;
end;

function TScriptPlayer.GetVest: Single;
begin
  Result := Self.FSpritePtr^.Vest;
end;

procedure TScriptPlayer.SetVest(Vest: Single);
begin
  Self.FSpritePtr^.Vest := Vest;
end;

function TScriptPlayer.GetPrimary: TScriptPrimaryPlayerWeapon;
begin
  Result := TScriptPrimaryPlayerWeapon(Self.FPrimary);
end;

function TScriptPlayer.GetSecondary: TScriptSecondaryPlayerWeapon;
begin
  Result := TScriptSecondaryPlayerWeapon(Self.FSecondary);
end;

function TScriptPlayer.GetShirtColor: Longword;
begin
  Result := Self.FSpritePtr^.Player.ShirtColor;
end;

function TScriptPlayer.GetPantsColor: Longword;
begin
  Result := Self.FSpritePtr^.Player.PantsColor;
end;

function TScriptPlayer.GetSkinColor: Longword;
begin
  Result := Self.FSpritePtr^.Player.SkinColor;
end;

function TScriptPlayer.GetHairColor: Longword;
begin
  Result := Self.FSpritePtr^.Player.HairColor;
end;

function TScriptPlayer.GetFavouriteWeapon: string;
begin
  Result := WeaponNumToName(Self.FSpritePtr^.Brain.FavWeapon);
end;

procedure TScriptPlayer.SetFavouriteWeapon(Weapon: string);
begin
  Self.FSpritePtr^.Brain.FavWeapon := WeaponNameToNum(Weapon);
end;

function TScriptPlayer.GetChosenSecondaryWeapon: Byte;
begin
  Result := Self.FSpritePtr^.Player.SecWep;
end;

function TScriptPlayer.GetFriend: string;
begin
  Result := Self.FSpritePtr^.Brain.Friend;
end;

procedure TScriptPlayer.SetFriend(Friend: string);
begin
  Self.FSpritePtr^.Brain.Friend := Friend;
end;

function TScriptPlayer.GetAccuracy: Byte;
begin
  Result := Self.FSpritePtr^.Brain.Accuracy;
end;

procedure TScriptPlayer.SetAccuracy(Accuracy: Byte);
begin
  Self.FSpritePtr^.Brain.Accuracy := Accuracy;
end;

function TScriptPlayer.GetShootDead: Boolean;
begin
  Result := Boolean(Self.FSpritePtr^.Brain.DeadKill);
end;

procedure TScriptPlayer.SetShootDead(ShootDead: Boolean);
begin
  Self.FSpritePtr^.Brain.DeadKill := Byte(ShootDead);
end;

function TScriptPlayer.GetGrenadeFrequency: Integer;
begin
  Result := Self.FSpritePtr^.Brain.GrenadeFreq;
end;

procedure TScriptPlayer.SetGrenadeFrequency(Frequency: Integer);
begin
  Self.FSpritePtr^.Brain.GrenadeFreq := Frequency;
end;

function TScriptPlayer.GetCamping: Boolean;
begin
  Result := Boolean(Self.FSpritePtr^.Brain.Camper);
end;

procedure TScriptPlayer.SetCamping(Camping: Boolean);
begin
  Self.FSpritePtr^.Brain.Camper := Byte(Camping);
end;

function TScriptPlayer.GetOnStartUse: Byte;
begin
  Result := Self.FSpritePtr^.Brain.Use;
end;

procedure TScriptPlayer.SetOnStartUse(Thing: Byte);
begin
  Self.FSpritePtr^.Brain.Use := Thing;
end;

function TScriptPlayer.GetHairStyle: Byte;
begin
  Result := Self.FSpritePtr^.Player.HairStyle;
end;

function TScriptPlayer.GetHeadgear: Byte;
begin
  Result := Self.FSpritePtr^.Player.HeadCap;
end;

function TScriptPlayer.GetChain: Byte;
begin
  Result := Self.FSpritePtr^.Player.Chain;
end;

function TScriptPlayer.GetChatFrequency: Byte;
begin
  Result := Self.FSpritePtr^.Brain.ChatFreq;
end;

procedure TScriptPlayer.SetChatFrequency(Frequency: Byte);
begin
  Self.FSpritePtr^.Brain.ChatFreq := Frequency;
end;

function TScriptPlayer.GetChatKill: string;
begin
  Result := Self.FSpritePtr^.Brain.ChatKill;
end;

procedure TScriptPlayer.SetChatKill(Message: string);
begin
  Self.FSpritePtr^.Brain.ChatKill := Message;
end;

function TScriptPlayer.GetChatDead: string;
begin
  Result := Self.FSpritePtr^.Brain.ChatDead;
end;

procedure TScriptPlayer.SetChatDead(Message: string);
begin
  Self.FSpritePtr^.Brain.ChatDead := Message;
end;

function TScriptPlayer.GetChatLowHealth: string;
begin
  Result := Self.FSpritePtr^.Brain.ChatLowHealth;
end;

procedure TScriptPlayer.SetChatLowHealth(Message: string);
begin
  Self.FSpritePtr^.Brain.ChatLowHealth := Message;
end;

function TScriptPlayer.GetChatSeeEnemy: string;
begin
  Result := Self.FSpritePtr^.Brain.ChatSeeEnemy;
end;

procedure TScriptPlayer.SetChatSeeEnemy(Message: string);
begin
  Self.FSpritePtr^.Brain.ChatSeeEnemy := Message;
end;

function TScriptPlayer.GetChatWinning: string;
begin
  Result := Self.FSpritePtr^.Brain.ChatWinning;
end;

procedure TScriptPlayer.SetChatWinning(Message: string);
begin
  Self.FSpritePtr^.Brain.ChatWinning := Message;
end;

function TScriptPlayer.GetAdmin: Boolean;
begin
  if not Self.FSpritePtr^.Active then
  begin
    Result := False;
    exit;
  end;
  if IsRemoteAdminIP(Self.FSpritePtr^.Player.IP) then
    Result := True
  else
    if IsAdminIP(Self.FSpritePtr^.Player.IP) then
      Result := True
    else
      Result := False;
end;

procedure TScriptPlayer.SetAdmin(SetAsAdmin: Boolean);
begin
  if SetAsAdmin then
    ParseInput('admip ' + Self.FSpritePtr^.Player.IP, 255)
  else
    ParseInput('unadm ' + Self.FSpritePtr^.Player.IP, 255);
end;

function TScriptPlayer.GetDummy: Boolean;
begin
  Result := Self.FSpritePtr^.Active and (Self.FSpritePtr^.Player.ControlMethod <> HUMAN) and Self.FSpritePtr^.Dummy;
end;

procedure TScriptPlayer.SetDummy(Dummy: Boolean);
begin
  Self.FSpritePtr^.Dummy := Dummy;
end;

constructor TScriptActivePlayer.Create(var Sprite: TSprite; ID: Byte);
begin
  Self.FSpritePtr := @Sprite;
  Self.FPrimary := TScriptPrimaryPlayerWeapon.Create(Sprite);
  Self.FSecondary := TScriptSecondaryPlayerWeapon.Create(Sprite);
  Self.FID := ID;
end;

function TScriptActivePlayer.GetKills: Integer;
begin
  Result := Self.FSpritePtr^.Player.Kills;
end;

procedure TScriptActivePlayer.SetKills(Kills: Integer);
begin
  Self.FSpritePtr^.Player.Kills := Kills;
  SortPlayers;
end;

function TScriptActivePlayer.GetDeaths: Integer;
begin
  Result := Self.FSpritePtr^.Player.Deaths;
end;

procedure TScriptActivePlayer.SetDeaths(Deaths: Integer);
begin
  Self.FSpritePtr^.Player.Deaths := Deaths;
  SortPlayers;
end;

function TScriptActivePlayer.GetPing: Integer;
begin
  Result := Self.FSpritePtr^.Player.RealPing;
end;

function TScriptActivePlayer.GetActive: Boolean;
begin
  Result := Self.FSpritePtr^.Active;
end;

function TScriptActivePlayer.GetIP: string;
begin
  Result := Self.FSpritePtr^.Player.IP;
end;

function TScriptActivePlayer.GetPort: Word;
begin
  Result := Self.FSpritePtr^.Player.Port;
end;

function TScriptActivePlayer.GetJets: Integer;
begin
  Result := Self.FSpritePtr^.JetsCount;
end;

function TScriptActivePlayer.GetGrenades: Byte;
begin
  Result := Self.FSpritePtr^.TertiaryWeapon.AmmoCount;
end;

function TScriptActivePlayer.GetX: Single;
begin
  Result := SpriteParts.Pos[Self.FID].X;
end;

function TScriptActivePlayer.GetY: Single;
begin
  Result := SpriteParts.Pos[Self.FID].Y;
end;

function TScriptActivePlayer.GetMouseAimX: SmallInt;
begin
  Result := Self.FSpritePtr^.Control.MouseAimX;
end;

procedure TScriptActivePlayer.SetMouseAimX(AimX: SmallInt);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.MouseAimX := AimX;
end;

function TScriptActivePlayer.GetMouseAimY: SmallInt;
begin
  Result := Self.FSpritePtr^.Control.MouseAimY;
end;

procedure TScriptActivePlayer.SetMouseAimY(AimY: SmallInt);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.MouseAimY := AimY;
end;

function TScriptActivePlayer.GetFlagger: Boolean;
begin
  Result := (Self.FSpritePtr^.HoldedThing > 0) and (Self.FSpritePtr^.HoldedThing < 4);
end;

function TScriptActivePlayer.GetTime: Integer;
begin
  Result := Self.FSpritePtr^.Player.PlayTime;
end;

function TScriptActivePlayer.GetOnGround: Boolean;
begin
  Result := Self.FSpritePtr^.OnGround;
end;

function TScriptActivePlayer.GetProne: Boolean;
begin
  Result := Self.FSpritePtr^.Position = POS_PRONE;
end;

function TScriptActivePlayer.GetHuman: Boolean;
begin
  Result := Self.FSpritePtr^.Active and (Self.FSpritePtr^.Player.ControlMethod = Net.HUMAN);
end;

function TScriptActivePlayer.GetDirection: Shortint;
begin
  Result := Self.FSpritePtr^.Direction;
end;

function TScriptActivePlayer.GetFlags: Byte;
begin
  Result := Self.FSpritePtr^.Player.Flags;
end;

function TScriptActivePlayer.GetHWID: string;
begin
  Result := Self.FSpritePtr^.Player.hwid;
end;

{$IFDEF STEAM}
function TScriptActivePlayer.GetSteamID: Int64;
begin
  Result := Int64(Self.FSpritePtr^.Player.SteamID);
end;
{$ENDIF}

function TScriptActivePlayer.GetKeyUp: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Up;
end;

procedure TScriptActivePlayer.SetKeyUp(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.Up := Pressed;
end;

function TScriptActivePlayer.GetKeyLeft: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Left;
end;

procedure TScriptActivePlayer.SetKeyLeft(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.Left := Pressed;
end;

function TScriptActivePlayer.GetKeyRight: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Right;
end;

procedure TScriptActivePlayer.SetKeyRight(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.Right := Pressed;
end;

function TScriptActivePlayer.GetKeyShoot: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Fire;
end;

procedure TScriptActivePlayer.SetKeyShoot(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.Fire := Pressed;
end;

function TScriptActivePlayer.GetKeyJetpack: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Jetpack;
end;

procedure TScriptActivePlayer.SetKeyJetpack(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.JetPack := Pressed;
end;

function TScriptActivePlayer.GetKeyGrenade: Boolean;
begin
  Result := Self.FSpritePtr^.Control.ThrowNade;
end;

procedure TScriptActivePlayer.SetKeyGrenade(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.ThrowNade := Pressed;
end;

function TScriptActivePlayer.GetKeyChangeWeap: Boolean;
begin
  Result := Self.FSpritePtr^.Control.ChangeWeapon;
end;

procedure TScriptActivePlayer.SetKeyChangeWeap(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.ChangeWeapon := Pressed;
end;

function TScriptActivePlayer.GetKeyThrow: Boolean;
begin
  Result := Self.FSpritePtr^.Control.ThrowWeapon;
end;

procedure TScriptActivePlayer.SetKeyThrow(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.ThrowWeapon := Pressed;
end;

function TScriptActivePlayer.GetKeyReload: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Reload;
end;

procedure TScriptActivePlayer.SetKeyReload(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.Reload := Pressed;
end;

function TScriptActivePlayer.GetKeyCrouch: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Down;
end;

procedure TScriptActivePlayer.SetKeyCrouch(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.Down := Pressed;
end;

function TScriptActivePlayer.GetKeyProne: Boolean;
begin
  Result := Self.FSpritePtr^.Control.Prone;
end;

procedure TScriptActivePlayer.SetKeyProne(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.Prone := Pressed;
end;

function TScriptActivePlayer.GetKeyFlagThrow: Boolean;
begin
  Result := Self.FSpritePtr^.Control.FlagThrow;
end;

procedure TScriptActivePlayer.SetKeyFlagThrow(Pressed: Boolean);
begin
  if not GetHuman then
    Self.FSpritePtr^.Control.FlagThrow := Pressed;
end;


function TScriptActivePlayer.Ban(Time: Integer; Reason: string): Boolean;
begin
  Result := KickPlayer(Self.ID, True, KICK_CONSOLE, Time * 3600, Reason);
end;

procedure TScriptActivePlayer.Say(Text: string; MsgType: Byte);
begin
  if not Self.Active then
    Exit;
  ServerSendStringMessage(WideString(Text), ALL_PLAYERS, Self.ID, MsgType);
  MainConsole.Console('[' + Self.Name + '] ' + Text, CHAT_MESSAGE_COLOR);
end;

procedure TScriptActivePlayer.Damage(Shooter: Byte; Damage: Single);
var
  A: TVector2;
begin
  if (Shooter < 1) or (Shooter > 32) then
    raise EArgumentException('Shooter must be between 1 and ' + IntToStr(MAX_SPRITES));
  if not Self.Active then
    Exit;
  A := Default(TVector2);
  Self.FSpritePtr^.HealthHit(Damage, Shooter, 1, 1, A);
end;

procedure TScriptActivePlayer.Kill();
begin
  Self.FSpritePtr^.Die(NORMAL_DEATH, Self.FSpritePtr^.Num, 1, -1, Self.FSpritePtr^.Skeleton.Pos[12]);
end;

procedure TScriptActivePlayer.BigText(Layer: Byte; Text: string;
  Delay: Integer; Color: Longint; Scale: Single; X, Y: Integer);
begin
  if not Self.Active then
    Exit;
  ServerSendSpecialMessage(Text, 1, Layer, Delay, Scale, Color, X, Y, Self.ID);
end;

procedure TScriptActivePlayer.WorldText(Layer: Byte; Text: string;
  Delay: Integer; Color: Longint; Scale, X, Y: Single);
begin
  if not Self.Active then
    Exit;
  ServerSendSpecialMessage(Text, 2, Layer, Delay, Scale, Color, X, Y, Self.ID);
end;

procedure TScriptActivePlayer.ForceWeapon(Primary, Secondary: TScriptWeapon);
begin
  if not Self.Active then
    Exit;
  NetworkServerFunctions.ForceWeapon(Self.ID, Primary.WType, Secondary.WType, Primary.Ammo, Secondary.Ammo);
end;

procedure TScriptActivePlayer.ForwardTo(TargetIP: string; TargetPort: Word;
  Message: string);
begin
  if not Self.Active then
    Exit;
  ForwardClient(Self.ID, TargetIP, TargetPort, Message);
end;

procedure TScriptActivePlayer.GiveBonus(BType: Byte);
var
  PStyle: Byte;
  I: Integer;
  SPos: TVector2;
  OPStyle: Byte;
begin
  if not Self.Active then
    Exit;
  case BType of
    1:
    begin  // Predator
      Self.FSpritePtr^.Alpha := PREDATORALPHA;
      Self.FSpritePtr^.BonusTime := PREDATORBONUSTIME;
      Self.FSpritePtr^.BonusStyle := BONUS_PREDATOR;
      Self.FSpritePtr^.Health := STARTHEALTH;
      PStyle := 20;
    end;
    2:
    begin  // Berserker
      Self.FSpritePtr^.BonusStyle := BONUS_BERSERKER;
      Self.FSpritePtr^.BonusTime := BERSERKERBONUSTIME;
      Self.FSpritePtr^.Health := STARTHEALTH;
      PStyle := 21;
    end;
    3:
    begin  // Vest
      Self.FSpritePtr^.Vest := DEFAULTVEST;
      PStyle := 19;
    end;
    4:
    begin  // Grenades
      Self.FSpritePtr^.TertiaryWeapon := Guns[FRAGGRENADE];
      Self.FSpritePtr^.TertiaryWeapon.AmmoCount := sv_maxgrenades.Value;
      PStyle := 17;
    end;
    5:
    begin  // Clusters
      Self.FSpritePtr^.TertiaryWeapon := Guns[CLUSTERGRENADE];
      Self.FSpritePtr^.TertiaryWeapon.AmmoCount := CLUSTER_GRENADES;
      PStyle := 22;
    end;
    6:
    begin  // Flame god
      Self.FSpritePtr^.BonusStyle := BONUS_FLAMEGOD;
      Self.FSpritePtr^.BonusTime := FLAMERBONUSTIME;
      Self.FSpritePtr^.Health := STARTHEALTH;
      PStyle := 18;
    end;
    else
      Exit;
  end;

  SPos.x := X;
  SPos.y := Y;
  OPStyle := PStyle;
  case PStyle of
    1: PStyle := 5;    // Desert Eagle
    2: PStyle := 6;    // HK MP5
    3: PStyle := 7;    // AK 74
    4: PStyle := 8;    // Steyr AUG
    5: PStyle := 9;    // Spas 12
    6: PStyle := 10;   // Ruger77
    7: PStyle := 11;   // M79
    8: PStyle := 12;   // Barrett M82A1
    9: PStyle := 13;   // Minimi
    10: PStyle := 14;  // Minigun
    11: PStyle := 4;   // USSOCOM
    12: PStyle := 25;  // Combat Knife
    13: PStyle := 24;  // Chainsaw
    14: PStyle := 26;  // LAW
    15: PStyle := 27;  // Stationary Gun
    16: PStyle := 16;  // Medical Kit
    17: PStyle := 17;  // Grenade Kit
    18: PStyle := 18;  // Flamer Kit
    19: PStyle := 20;  // Vest Kit
    20: PStyle := 19;  // Predator Kit
    21: PStyle := 21;  // Berserk Kit
    22: PStyle := 22;  // Cluster Kit
    else
      Exit;
  end;
  I := CreateThing(SPos, 255, PStyle, 255);
  Thing[I].Active := True;
  if (OPStyle > 0) and (OPStyle < 15) then
    Thing[I].AmmoCount := Round(DefaultGuns[OPStyle].Ammo);

  ServerThingTaken(I, Id);
  Thing[I].Kill;
  Thing[I].Active := False;

  ServerThingTaken(I, 255);
end;

function TScriptActivePlayer.Kick(Reason: TKickReason): Boolean;
begin
  Result := False;
  if not Self.Active then
  begin
    Result := False;
    Exit;
  end;
  {  TKickReason = (TKickNoResponse, TKickNoCheatResponse,
    TKickChangeTeam, TKickPing, TKickFlooding, TKickConsole,
    TKickConnectionCheat, TKickCheat, TKickLeft, TKickVoted,
    TKickAC); }
  case Reason of
    TKickNoResponse: Result := KickPlayer(Self.Id, False, KICK_NORESPONSE, 0);
    TKickNoCheatResponse: Result := KickPlayer(Self.Id, False, KICK_NOCHEATRESPONSE, 0);
    TKickChangeTeam: Result := KickPlayer(Self.Id, False, KICK_CHANGETEAM, 0);
    TKickPing: Result := KickPlayer(Self.Id, False, KICK_PING, 0);
    TKickFlooding: Result := KickPlayer(Self.Id, False, KICK_FLOODING, 0);
    TKickConsole: Result := KickPlayer(Self.Id, False, KICK_CONSOLE, 0);
    TKickConnectionCheat: Result := KickPlayer(Self.Id, False, KICK_CONNECTCHEAT, 0);
    TKickCheat: Result := KickPlayer(Self.Id, False, KICK_CHEAT, 0);
    TKickLeft: Result := KickPlayer(Self.Id, False, KICK_LEFTGAME, 0);
    TKickVoted: Result := KickPlayer(Self.Id, False, KICK_VOTED, 0);
    TKickAC: Result := KickPlayer(Self.Id, False, KICK_AC, 0);
    TKickSilent: Result := KickPlayer(Self.Id, False, KICK_SILENT, 0);
    //else raise EArgumentException.Create('Unknown reason ' + IntToStr(Integer(Reason)));
  end;
end;

procedure TScriptActivePlayer.PlaySound(Name: string; X, Y: Single);
begin
  NetworkServerFunctions.PlaySound(Self.ID, Name, X, Y);
end;

procedure TScriptActivePlayer.Move(X, Y: Single);
begin
  if not Self.Active then
    Exit;
  MovePlayer(Self.ID, X, Y);
end;

procedure TScriptActivePlayer.SetVelocity(VelX, VelY: Single);
begin
  if not Self.Active then
    Exit;
  ModifyPlayerVelocity(Self.ID, VelX, VelY);
end;

procedure TScriptActivePlayer.Tell(Text: string);
begin
  if not Self.Active then
    Exit;
  ServerSendStringMessage(WideString(Text), Self.ID, 255, MSGTYPE_PUB);
end;

procedure TScriptActivePlayer.WriteConsole(Text: string; Color: Longint);
begin
  if not Self.Active then
    Exit;
  ServerHelper.WriteConsole(Self.ID, Text, Color);
end;

procedure TScriptActivePlayer.ChangeTeam(NewTeam: Byte; JoinType: TJoinType);
begin
    if not Self.Active then
    Exit;
  if NewTeam > 5 then
    raise EArgumentException.Create('Team parameter must be 0-5');
  case JoinType of
    TJoinNormal: Self.FSpritePtr^.ChangeTeam(NewTeam, True, JOIN_NORMAL);
    TJoinSilent: Self.FSpritePtr^.ChangeTeam(NewTeam, True, JOIN_SILENT);
    //else raise EArgumentException.Create('Unknown join type');
  end;
end;

procedure TScriptNewPlayer.SetTeam(Team: Byte);
begin
  Self.FSpritePtr^.Player.Team := Team;
end;

procedure TScriptNewPlayer.SetName(Name: string);
begin
  Self.FSpritePtr^.Player.Name := Name;
end;

procedure TScriptNewPlayer.SetHealth(Health: Single);
begin
  Self.FSpritePtr^.Health := Health;
end;

function TScriptNewPlayer.GetPrimary: TScriptWeapon;
begin
  Result := Self.FPrimary;
end;

procedure TScriptNewPlayer.SetPrimary(Primary: TScriptWeapon);
begin
  Self.FPrimary := Primary;
end;

function TScriptNewPlayer.GetSecondary: TScriptWeapon;
begin
  Result := Self.FSecondary;
end;

procedure TScriptNewPlayer.SetSecondary(Secondary: TScriptWeapon);
begin
  Self.FSecondary := Secondary;
end;

procedure TScriptNewPlayer.SetShirtColor(Color: Longword);
begin
  Self.FSpritePtr^.Player.ShirtColor := Color;
end;

procedure TScriptNewPlayer.SetPantsColor(Color: Longword);
begin
  Self.FSpritePtr^.Player.PantsColor := Color;
end;

procedure TScriptNewPlayer.SetSkinColor(Color: Longword);
begin
  Self.FSpritePtr^.Player.SkinColor := Color;
end;

procedure TScriptNewPlayer.SetHairColor(Color: Longword);
begin
  Self.FSpritePtr^.Player.HairColor := Color;
end;

procedure TScriptNewPlayer.SetHairStyle(Style: Byte);
begin
  Self.FSpritePtr^.Player.HairStyle := Style;
end;

procedure TScriptNewPlayer.SetHeadgear(Headgear: Byte);
begin
  Self.FSpritePtr^.Player.HeadCap := Headgear;
end;

procedure TScriptNewPlayer.SetChain(Chain: Byte);
begin
  Self.FSpritePtr^.Player.Chain := Chain;
end;

function TScriptNewPlayer.GetDummy: Boolean;
begin
  Result := Self.FSpritePtr^.Dummy;
end;

procedure TScriptActivePlayer.SetTeam(Team: Byte);
begin
  if not Self.Active then
    Exit;
  if Team > 5 then
    raise EArgumentException.Create('Team parameter must be 0-5');
  Self.FSpritePtr^.ChangeTeam(Team, True);
end;

function TScriptActivePlayer.GetVelX: Single;
begin
  Result := SpriteParts.Velocity[Self.ID].x;
end;

function TScriptActivePlayer.GetVelY: Single;
begin
  Result := SpriteParts.Velocity[Self.ID].y;
end;

function TScriptActivePlayer.GetMuted: Boolean;
begin
  Result := Self.FSpritePtr^.Player.Muted <> 0;
end;

procedure TScriptActivePlayer.SetWeaponActive(ID: Byte; Active: Boolean);
begin
  if not Self.Active then
    Exit;
  NetworkServerFunctions.SetWeaponActive(Self.ID, ID, Active);
end;

procedure TScriptActivePlayer.SetMuted(Muted: Boolean);
var
  i: Byte;
begin
  if not Self.Active then
    Exit;
  if Muted and (Self.FSpritePtr^.Player.Muted = 0) then
  begin
    Self.FSpritePtr^.Player.Muted := 1;
    for i := 1 to MAX_PLAYERS do
      if Trim(mutelist[i]) = '' then
      begin
        mutelist[i] := Self.FSpritePtr^.Player.IP;
        mutename[i] := Self.FSpritePtr^.Player.Name;
        Break;
      end;
  end
  else if not Muted and (Self.FSpritePtr^.Player.Muted = 1) then
  begin
    Self.FSpritePtr^.Player.Muted := 0;
    for i := 1 to MAX_PLAYERS do
      if Trim(mutelist[i]) = Self.FSpritePtr^.Player.IP then
      begin
        mutelist[i] := '';
        Break;
      end;
  end;

end;

/////////////////////////////
// Player property helpers //
/////////////////////////////

procedure ScriptPlayerGetTeam(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.Team;
end;

procedure ScriptPlayerGetName(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.Name;
end;

procedure ScriptPlayerGetAlive(Self: TScriptPlayer; var Result: Boolean);
begin
  Result := Self.Alive;
end;

procedure ScriptPlayerGetHealth(Self: TScriptPlayer; var Result: Single);
begin
  Result := Self.Health;
end;

procedure ScriptPlayerSetHealth(Self: TScriptPlayer; const Result: Single);
begin
  Self.Health := Result;
end;

procedure ScriptPlayerGetVest(Self: TScriptPlayer; var Result: Single);
begin
  Result := Self.Vest;
end;

procedure ScriptPlayerSetVest(Self: TScriptPlayer; const Result: Single);
begin
  Self.Vest := Result;
end;

procedure ScriptPlayerGetPrimary(Self: TScriptPlayer;
  var Result: TScriptPrimaryPlayerWeapon);
begin
  Result := Self.Primary;
end;

procedure ScriptPlayerGetSecondary(Self: TScriptPlayer;
  var Result: TScriptSecondaryPlayerWeapon);
begin
  Result := Self.Secondary;
end;

procedure ScriptPlayerGetShirtColor(Self: TScriptPlayer; var Result: Longword);
begin
  Result := Self.ShirtColor;
end;

procedure ScriptPlayerGetPantsColor(Self: TScriptPlayer; var Result: Longword);
begin
  Result := Self.PantsColor;
end;

procedure ScriptPlayerGetSkinColor(Self: TScriptPlayer; var Result: Longword);
begin
  Result := Self.SkinColor;
end;

procedure ScriptPlayerGetHairColor(Self: TScriptPlayer; var Result: Longword);
begin
  Result := Self.HairColor;
end;

procedure ScriptPlayerGetFavouriteWeapon(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.FavouriteWeapon;
end;

procedure ScriptPlayerSetFavouriteWeapon(Self: TScriptPlayer; const Result: string);
begin
  Self.FavouriteWeapon := Result;
end;

procedure ScriptPlayerGetChosenSecondaryWeapon(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.ChosenSecondaryWeapon;
end;

procedure ScriptPlayerGetFriend(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.Friend;
end;

procedure ScriptPlayerSetFriend(Self: TScriptPlayer; const Result: string);
begin
  Self.Friend := Result;
end;

procedure ScriptPlayerGetAccuracy(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.Accuracy;
end;

procedure ScriptPlayerSetAccuracy(Self: TScriptPlayer; const Result: Byte);
begin
  Self.Accuracy := Result;
end;

procedure ScriptPlayerGetShootDead(Self: TScriptPlayer; var Result: Boolean);
begin
  Result := Self.ShootDead;
end;

procedure ScriptPlayerSetShootDead(Self: TScriptPlayer; const Result: Boolean);
begin
  Self.ShootDead := Result;
end;

procedure ScriptPlayerGetGrenadeFrequency(Self: TScriptPlayer; var Result: Integer);
begin
  Result := Self.GrenadeFrequency;
end;

procedure ScriptPlayerSetGrenadeFrequency(Self: TScriptPlayer; const Result: Integer);
begin
  Self.GrenadeFrequency := Result;
end;

procedure ScriptPlayerGetCamping(Self: TScriptPlayer; var Result: Boolean);
begin
  Result := Self.Camping;
end;

procedure ScriptPlayerSetCamping(Self: TScriptPlayer; const Result: Boolean);
begin
  Self.Camping := Result;
end;

procedure ScriptPlayerGetOnStartUse(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.OnStartUse;
end;

procedure ScriptPlayerSetOnStartUse(Self: TScriptPlayer; const Result: Byte);
begin
  Self.OnStartUse := Result;
end;

procedure ScriptPlayerGetHairStyle(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.HairStyle;
end;

procedure ScriptPlayerGetHeadgear(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.Headgear;
end;

procedure ScriptPlayerGetChain(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.Chain;
end;

procedure ScriptPlayerGetChatFrequency(Self: TScriptPlayer; var Result: Byte);
begin
  Result := Self.ChatFrequency;
end;

procedure ScriptPlayerSetChatFrequency(Self: TScriptPlayer; const Result: Byte);
begin
  Self.ChatFrequency := Result;
end;

procedure ScriptPlayerGetChatKill(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.ChatKill;
end;

procedure ScriptPlayerSetChatKill(Self: TScriptPlayer; const Result: string);
begin
  Self.ChatKill := Result;
end;

procedure ScriptPlayerGetChatDead(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.ChatDead;
end;

procedure ScriptPlayerSetChatDead(Self: TScriptPlayer; const Result: string);
begin
  Self.ChatDead := Result;
end;

procedure ScriptPlayerGetChatLowHealth(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.ChatLowHealth;
end;

procedure ScriptPlayerSetChatLowHealth(Self: TScriptPlayer; const Result: string);
begin
  Self.ChatLowHealth := Result;
end;

procedure ScriptPlayerGetChatSeeEnemy(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.ChatSeeEnemy;
end;

procedure ScriptPlayerSetChatSeeEnemy(Self: TScriptPlayer; const Result: string);
begin
  Self.ChatSeeEnemy := Result;
end;

procedure ScriptPlayerGetChatWinning(Self: TScriptPlayer; var Result: string);
begin
  Result := Self.ChatWinning;
end;

procedure ScriptPlayerSetChatWinning(Self: TScriptPlayer; const Result: string);
begin
  Self.ChatWinning := Result;
end;

procedure ScriptPlayerGetAdmin(Self: TScriptPlayer; var Result: Boolean);
begin
  Result := Self.IsAdmin;
end;

procedure ScriptPlayerSetAdmin(Self: TScriptPlayer; const Result: Boolean);
begin
  Self.IsAdmin := Result;
end;

procedure ScriptPlayerGetDummy(Self: TScriptPlayer; var Result: Boolean);
begin
  Result := Self.Dummy;
end;

procedure ScriptPlayerSetDummy(Self: TScriptPlayer; const Result: Boolean);
begin
  Self.Dummy := Result;
end;

/////////////////////////////////
// New player property helpers //
/////////////////////////////////

procedure ScriptNewPlayerGetName(Self: TScriptNewPlayer; var Result: string);
begin
  Result := Self.Name;
end;

procedure ScriptNewPlayerSetName(Self: TScriptNewPlayer; const Result: string);
begin
  Self.Name := Result;
end;

procedure ScriptNewPlayerGetTeam(Self: TScriptNewPlayer; var Result: Byte);
begin
  Result := Self.Team;
end;

procedure ScriptNewPlayerSetTeam(Self: TScriptNewPlayer; const Result: Byte);
begin
  Self.Team := Result;
end;

procedure ScriptNewPlayerGetAlive(Self: TScriptNewPlayer; var Result: Boolean);
begin
  Result := Self.Alive;
end;

procedure ScriptNewPlayerGetHealth(Self: TScriptNewPlayer; var Result: Single);
begin
  Result := Self.Health;
end;

procedure ScriptNewPlayerSetHealth(Self: TScriptNewPlayer; const Result: Single);
begin
  Self.Health := Result;
end;

procedure ScriptNewPlayerGetPrimary(Self: TScriptNewPlayer; var Result: TScriptWeapon);
begin
  Result := Self.Primary;
end;

procedure ScriptNewPlayerSetPrimary(Self: TScriptNewPlayer; const Result: TScriptWeapon);
begin
  Self.Primary := Result;
end;

procedure ScriptNewPlayerGetSecondary(Self: TScriptNewPlayer; var Result: TScriptWeapon);
begin
  Result := Self.Secondary;
end;

procedure ScriptNewPlayerSetSecondary(Self: TScriptNewPlayer;
  const Result: TScriptWeapon);
begin
  Self.Secondary := Result;
end;

procedure ScriptNewPlayerGetShirtColor(Self: TScriptNewPlayer; var Result: Longword);
begin
  Result := Self.ShirtColor;
end;

procedure ScriptNewPlayerSetShirtColor(Self: TScriptNewPlayer; const Result: Longword);
begin
  Self.ShirtColor := Result;
end;

procedure ScriptNewPlayerGetPantsColor(Self: TScriptNewPlayer; var Result: Longword);
begin
  Result := Self.PantsColor;
end;

procedure ScriptNewPlayerSetPantsColor(Self: TScriptNewPlayer; const Result: Longword);
begin
  Self.PantsColor := Result;
end;

procedure ScriptNewPlayerGetSkinColor(Self: TScriptNewPlayer; var Result: Longword);
begin
  Result := Self.SkinColor;
end;

procedure ScriptNewPlayerSetSkinColor(Self: TScriptNewPlayer; const Result: Longword);
begin
  Self.SkinColor := Result;
end;

procedure ScriptNewPlayerGetHairColor(Self: TScriptNewPlayer; var Result: Longword);
begin
  Result := Self.HairColor;
end;

procedure ScriptNewPlayerSetHairColor(Self: TScriptNewPlayer; const Result: Longword);
begin
  Self.HairColor := Result;
end;

procedure ScriptNewPlayerGetHairStyle(Self: TScriptNewPlayer; var Result: Byte);
begin
  Result := Self.HairStyle;
end;

procedure ScriptNewPlayerSetHairStyle(Self: TScriptNewPlayer; const Result: Byte);
begin
  Self.HairStyle := Result;
end;

procedure ScriptNewPlayerGetHeadgear(Self: TScriptNewPlayer; var Result: Byte);
begin
  Result := Self.Headgear;
end;

procedure ScriptNewPlayerSetHeadgear(Self: TScriptNewPlayer; const Result: Byte);
begin
  Self.Headgear := Result;
end;

procedure ScriptNewPlayerGetChain(Self: TScriptNewPlayer; var Result: Byte);
begin
  Result := Self.Chain;
end;

procedure ScriptNewPlayerSetChain(Self: TScriptNewPlayer; const Result: Byte);
begin
  Self.Chain := Result;
end;

procedure ScriptNewPlayerGetDummy(Self: TScriptNewPlayer; var Result: Boolean);
begin
  Result := Self.Dummy;
end;

procedure ScriptNewPlayerSetDummy(Self: TScriptNewPlayer; const Result: Boolean);
begin
  Self.Dummy := Result;
end;

////////////////////////////////////
// Active player property helpers //
////////////////////////////////////

procedure ScriptActivePlayerGetID(Self: TScriptActivePlayer; var Result: Byte);
begin
  Result := Self.ID;
end;

procedure ScriptActivePlayerGetTeam(Self: TScriptActivePlayer; var Result: Byte);
begin
  Result := Self.Team;
end;

procedure ScriptActivePlayerSetTeam(Self: TScriptActivePlayer; const Result: Byte);
begin
  Self.Team := Result;
end;

procedure ScriptActivePlayerGetAlive(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.Alive;
end;

procedure ScriptActivePlayerGetKills(Self: TScriptActivePlayer; var Result: Integer);
begin
  Result := Self.Kills;
end;

procedure ScriptActivePlayerSetKills(Self: TScriptActivePlayer; const Result: Integer);
begin
  Self.Kills := Result;
end;

procedure ScriptActivePlayerGetDeaths(Self: TScriptActivePlayer; var Result: Integer);
begin
  Result := Self.Deaths;
end;

procedure ScriptActivePlayerSetDeaths(Self: TScriptActivePlayer; const Result: Integer);
begin
  Self.Deaths := Result;
end;

procedure ScriptActivePlayerGetPing(Self: TScriptActivePlayer; var Result: Integer);
begin
  Result := Self.Ping;
end;

procedure ScriptActivePlayerGetActive(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.Active;
end;

procedure ScriptActivePlayerGetIP(Self: TScriptActivePlayer; var Result: string);
begin
  Result := Self.IP;
end;

procedure ScriptActivePlayerGetPort(Self: TScriptActivePlayer; var Result: Word);
begin
  Result := Self.Port;
end;

procedure ScriptActivePlayerGetVelX(Self: TScriptActivePlayer; var Result: Single);
begin
  Result := Self.VelX;
end;

procedure ScriptActivePlayerGetVelY(Self: TScriptActivePlayer; var Result: Single);
begin
  Result := Self.VelY;
end;

procedure ScriptActivePlayerGetMuted(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.Muted;
end;

procedure ScriptActivePlayerSetMuted(Self: TScriptActivePlayer; const Result: Boolean);
begin
  Self.Muted := Result;
end;

procedure ScriptActivePlayerGetJets(Self: TScriptActivePlayer; var Result: Byte);
begin
  Result := Self.Jets;
end;

procedure ScriptActivePlayerGetGrenades(Self: TScriptActivePlayer; var Result: Byte);
begin
  Result := Self.Grenades;
end;

procedure ScriptActivePlayerGetX(Self: TScriptActivePlayer; var Result: Single);
begin
  Result := Self.X;
end;

procedure ScriptActivePlayerGetY(Self: TScriptActivePlayer; var Result: Single);
begin
  Result := Self.Y;
end;

procedure ScriptActivePlayerGetMouseAimX(Self: TScriptActivePlayer; var Result: SmallInt);
begin
  Result := Self.MouseAimX;
end;

procedure ScriptActivePlayerSetMouseAimX(Self: TScriptActivePlayer;
  const Result: SmallInt);
begin
  Self.MouseAimX := Result;
end;

procedure ScriptActivePlayerGetMouseAimY(Self: TScriptActivePlayer; var Result: SmallInt);
begin
  Result := Self.MouseAimY;
end;

procedure ScriptActivePlayerSetMouseAimY(Self: TScriptActivePlayer;
  const Result: SmallInt);
begin
  Self.MouseAimY := Result;
end;

procedure ScriptActivePlayerGetFlagger(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.Flagger;
end;

procedure ScriptActivePlayerGetTime(Self: TScriptActivePlayer; var Result: Integer);
begin
  Result := Self.Time;
end;

procedure ScriptActivePlayerGetOnGround(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.OnGround;
end;

procedure ScriptActivePlayerGetProne(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.IsProne;
end;

procedure ScriptActivePlayerGetHuman(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.Human;
end;

procedure ScriptActivePlayerGetDirection(Self: TScriptActivePlayer;
  var Result: Shortint);
begin
  Result := Self.Direction;
end;

procedure ScriptActivePlayerGetFlags(Self: TScriptActivePlayer; var Result: Byte);
begin
  Result := Self.Flags;
end;

procedure ScriptActivePlayerGetHWID(Self: TScriptActivePlayer; var Result: string);
begin
  Result := Self.HWID;
end;

{$IFDEF STEAM}
procedure ScriptActivePlayerGetSteamID(Self: TScriptActivePlayer; var Result: Int64);
begin
  Result := Self.SteamID;
end;
{$ENDIF}

procedure ScriptActivePlayerGetKeyUp(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyUp;
end;

procedure ScriptActivePlayerSetKeyUp(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyUp := Result;
end;

procedure ScriptActivePlayerGetKeyLeft(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyLeft;
end;

procedure ScriptActivePlayerSetKeyLeft(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyLeft := Result;
end;

procedure ScriptActivePlayerGetKeyRight(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyRight;
end;

procedure ScriptActivePlayerSetKeyRight(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyRight := Result;
end;

procedure ScriptActivePlayerGetKeyShoot(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyShoot;
end;

procedure ScriptActivePlayerSetKeyShoot(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyShoot := Result;
end;

procedure ScriptActivePlayerGetKeyJetpack(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyJetpack;
end;

procedure ScriptActivePlayerSetKeyJetpack(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyJetpack := Result;
end;

procedure ScriptActivePlayerGetKeyGrenade(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyGrenade;
end;

procedure ScriptActivePlayerSetKeyGrenade(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyGrenade := Result;
end;

procedure ScriptActivePlayerGetKeyChangeWeap(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyChangeWeap;
end;

procedure ScriptActivePlayerSetKeyChangeWeap(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyChangeWeap := Result;
end;

procedure ScriptActivePlayerGetKeyThrow(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyThrow;
end;

procedure ScriptActivePlayerSetKeyThrow(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyThrow := Result;
end;

procedure ScriptActivePlayerGetKeyReload(Self: TScriptActivePlayer;
  var Result: Boolean);
begin
  Result := Self.KeyReload;
end;

procedure ScriptActivePlayerSetKeyReload(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyReload := Result;
end;

procedure ScriptActivePlayerGetKeyCrouch(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.KeyCrouch;
end;

procedure ScriptActivePlayerSetKeyCrouch(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyCrouch := Result;
end;

procedure ScriptActivePlayerGetKeyProne(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.KeyProne;
end;

procedure ScriptActivePlayerSetKeyProne(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyProne := Result;
end;

procedure ScriptActivePlayerGetKeyFlagThrow(Self: TScriptActivePlayer; var Result: Boolean);
begin
  Result := Self.KeyFlagThrow;
end;

procedure ScriptActivePlayerSetKeyFlagThrow(Self: TScriptActivePlayer;
  const Result: Boolean);
begin
  Self.KeyFlagThrow := Result;
end;

procedure ScriptActivePlayerSetWeaponActive(Self: TScriptActivePlayer;
  const Result: Boolean; const Num: Byte);
begin
  Self.WeaponActive[Num] := Result;
end;

procedure ScriptActivePlayerGetOnFlagGrab(Self: TScriptActivePlayer;
  var Result: TOnFlagGrab);
begin
  Result := Self.OnFlagGrab;
end;

procedure ScriptActivePlayerSetOnFlagGrab(Self: TScriptActivePlayer;
  const Result: TOnFlagGrab);
begin
  Self.OnFlagGrab := Result;
end;

procedure ScriptActivePlayerGetOnFlagReturn(Self: TScriptActivePlayer;
  var Result: TOnFlagReturn);
begin
  Result := Self.OnFlagReturn;
end;

procedure ScriptActivePlayerSetOnFlagReturn(Self: TScriptActivePlayer;
  const Result: TOnFlagReturn);
begin
  Self.OnFlagReturn := Result;
end;

procedure ScriptActivePlayerGetOnFlagScore(Self: TScriptActivePlayer;
  var Result: TOnFlagScore);
begin
  Result := Self.OnFlagScore;
end;

procedure ScriptActivePlayerSetOnFlagScore(Self: TScriptActivePlayer;
  const Result: TOnFlagScore);
begin
  Self.OnFlagScore := Result;
end;

procedure ScriptActivePlayerGetOnFlagDrop(Self: TScriptActivePlayer;
  var Result: TOnFlagDrop);
begin
  Result := Self.OnFlagDrop;
end;

procedure ScriptActivePlayerSetOnFlagDrop(Self: TScriptActivePlayer;
  const Result: TOnFlagDrop);
begin
  Self.OnFlagDrop := Result;
end;

procedure ScriptActivePlayerGetOnKitPickup(Self: TScriptActivePlayer;
  var Result: TOnKitPickup);
begin
  Result := Self.OnKitPickup;
end;

procedure ScriptActivePlayerSetOnKitPickup(Self: TScriptActivePlayer;
  const Result: TOnKitPickup);
begin
  Self.OnKitPickup := Result;
end;

procedure ScriptActivePlayerGetOnBeforeRespawn(Self: TScriptActivePlayer;
  var Result: TOnBeforeRespawn);
begin
  Result := Self.OnBeforeRespawn;
end;

procedure ScriptActivePlayerSetOnBeforeRespawn(Self: TScriptActivePlayer;
  const Result: TOnBeforeRespawn);
begin
  Self.OnBeforeRespawn := Result;
end;

procedure ScriptActivePlayerGetOnAfterRespawn(Self: TScriptActivePlayer;
  var Result: TOnAfterRespawn);
begin
  Result := Self.OnAfterRespawn;
end;

procedure ScriptActivePlayerSetOnAfterRespawn(Self: TScriptActivePlayer;
  const Result: TOnAfterRespawn);
begin
  Self.OnAfterRespawn := Result;
end;

procedure ScriptActivePlayerGetOnDamage(Self: TScriptActivePlayer;
  var Result: TOnDamage);
begin
  Result := Self.OnDamage;
end;

procedure ScriptActivePlayerSetOnDamage(Self: TScriptActivePlayer;
  const Result: TOnDamage);
begin
  Self.OnDamage := Result;
end;

procedure ScriptActivePlayerGetOnKill(Self: TScriptActivePlayer; var Result: TOnKill);
begin
  Result := Self.OnKill;
end;

procedure ScriptActivePlayerSetOnKill(Self: TScriptActivePlayer; const Result: TOnKill);
begin
  Self.OnKill := Result;
end;

procedure ScriptActivePlayerGetOnWeaponChange(Self: TScriptActivePlayer;
  var Result: TOnWeaponChange);
begin
  Result := Self.OnWeaponChange;
end;

procedure ScriptActivePlayerSetOnWeaponChange(Self: TScriptActivePlayer;
  const Result: TOnWeaponChange);
begin
  Self.OnWeaponChange := Result;
end;

procedure ScriptActivePlayerGetOnVoteMapStart(Self: TScriptActivePlayer;
  var Result: TOnVoteMapStart);
begin
  Result := Self.OnVoteMapStart;
end;

procedure ScriptActivePlayerSetOnVoteMapStart(Self: TScriptActivePlayer;
  const Result: TOnVoteMapStart);
begin
  Self.OnVoteMapStart := Result;
end;

procedure ScriptActivePlayerGetOnVoteKickStart(Self: TScriptActivePlayer;
  var Result: TOnVoteKickStart);
begin
  Result := Self.OnVoteKickStart;
end;

procedure ScriptActivePlayerSetOnVoteKickStart(Self: TScriptActivePlayer;
  const Result: TOnVoteKickStart);
begin
  Self.OnVoteKickStart := Result;
end;

procedure ScriptActivePlayerGetOnVoteMap(Self: TScriptActivePlayer;
  var Result: TOnVoteMap);
begin
  Result := Self.OnVoteMap;
end;

procedure ScriptActivePlayerSetOnVoteMap(Self: TScriptActivePlayer;
  const Result: TOnVoteMap);
begin
  Self.OnVoteMap := Result;
end;

procedure ScriptActivePlayerGetOnVoteKick(Self: TScriptActivePlayer;
  var Result: TOnVoteKick);
begin
  Result := Self.OnVoteKick;
end;

procedure ScriptActivePlayerSetOnVoteKick(Self: TScriptActivePlayer;
  const Result: TOnVoteKick);
begin
  Self.OnVoteKick := Result;
end;

procedure ScriptActivePlayerGetOnSpeak(Self: TScriptActivePlayer; var Result: TOnSpeak);
begin
  Result := Self.OnSpeak;
end;

procedure ScriptActivePlayerSetOnSpeak(Self: TScriptActivePlayer;
  const Result: TOnSpeak);
begin
  Self.OnSpeak := Result;
end;

procedure ScriptActivePlayerGetOnCommand(Self: TScriptActivePlayer;
  var Result: TOnCommand);
begin
  Result := Self.OnCommand;
end;

procedure ScriptActivePlayerSetOnCommand(Self: TScriptActivePlayer;
  const Result: TOnCommand);
begin
  Self.OnCommand := Result;
end;

procedure TScriptPlayerAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  Player: TPascalCompiletimeClass;
  ActivePlayer: TPascalCompiletimeClass;
begin
  Compiler.AddType('TJoinType', '(TJoinNormal, TJoinSilent)');

  Player := Compiler.AddClass(nil, 'TPlayer');
  with Player do
  begin
    RegisterProperty('Team', 'Byte', iptR);
    RegisterProperty('Name', 'string', iptR);
    RegisterProperty('Alive', 'boolean', iptR);
    RegisterProperty('Health', 'Single', iptRW);
    RegisterProperty('Vest', 'Single', iptRW);
    RegisterProperty('Primary', 'TPlayerWeapon', iptR);
    RegisterProperty('Secondary', 'TPlayerWeapon', iptR);
    RegisterProperty('ShirtColor', 'Longword', iptR);
    RegisterProperty('PantsColor', 'Longword', iptR);
    RegisterProperty('SkinColor', 'Longword', iptR);
    RegisterProperty('HairColor', 'Longword', iptR);
    RegisterProperty('FavouriteWeapon', 'string', iptRW);
    RegisterProperty('ChosenSecondaryWeapon', 'Byte', iptR);
    RegisterProperty('Friend', 'string', iptRW);
    RegisterProperty('Accuracy', 'Byte', iptRW);
    RegisterProperty('ShootDead', 'Boolean', iptRW);
    RegisterProperty('GrenadeFrequency', 'Integer', iptRW);
    RegisterProperty('Camping', 'Boolean', iptRW);
    RegisterProperty('OnStartUse', 'Byte', iptRW);
    RegisterProperty('HairStyle', 'Byte', iptR);
    RegisterProperty('Headgear', 'Byte', iptR);
    RegisterProperty('Chain', 'Byte', iptR);
    RegisterProperty('ChatFrequency', 'Byte', iptRW);
    RegisterProperty('ChatKill', 'string', iptRW);
    RegisterProperty('ChatDead', 'string', iptRW);
    RegisterProperty('ChatLowHealth', 'string', iptRW);
    RegisterProperty('ChatSeeEnemy', 'string', iptRW);
    RegisterProperty('ChatWinning', 'string', iptRW);
    RegisterProperty('IsAdmin', 'Boolean', iptRW);
    RegisterProperty('Dummy', 'Boolean', iptRW);
  end;

  with Compiler.AddClass(Player, 'TNewPlayer') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Free');
    RegisterProperty('Name', 'string', iptRW);
    RegisterProperty('Team', 'Byte', iptRW);
    RegisterProperty('Health', 'Single', iptRW);
    RegisterProperty('Primary', 'TWeapon', iptRW);
    RegisterProperty('Secondary', 'TWeapon', iptRW);
    RegisterProperty('ShirtColor', 'Longword', iptRW);
    RegisterProperty('PantsColor', 'Longword', iptRW);
    RegisterProperty('SkinColor', 'Longword', iptRW);
    RegisterProperty('HairColor', 'Longword', iptRW);
    RegisterProperty('HairStyle', 'Byte', iptRW);
    RegisterProperty('Headgear', 'Byte', iptRW);
    RegisterProperty('Chain', 'Byte', iptRW);
    RegisterProperty('Dummy', 'Boolean', iptRW);
  end;

  ActivePlayer := Compiler.AddClass(Player, 'TActivePlayer');

  //TODO: Move this somewhere more appropriate
  Compiler.AddType('TVector', 'record X, Y: Single; end;');

  Compiler.AddType('TOnFlagGrabEvent',
    'procedure(Player: TActivePlayer; TFlag: TActiveFlag; Team: Byte; GrabbedInBase: Boolean)');
  Compiler.AddType('TOnFlagReturnEvent',
    'procedure(Player: TActivePlayer; Flag: TActiveFlag; Team: Byte)');
  Compiler.AddType('TOnFlagScoreEvent',
    'procedure(Player: TActivePlayer; Flag: TActiveFlag; Team: Byte)');
  Compiler.AddType('TOnFlagDropEvent',
    'procedure(Player: TActivePlayer; Flag: TActiveFlag; Team: Byte; Thrown: Boolean)');
  Compiler.AddType('TOnKitPickupEvent',
    'procedure(Player: TActivePlayer; Kit: TActiveMapObject)');
  Compiler.AddType('TOnBeforeRespawnEvent', 'function(Player: TActivePlayer): TVector');
  Compiler.AddType('TOnAfterRespawnEvent', 'procedure(Player: TActivePlayer)');
  Compiler.AddType('TOnDamageEvent',
    'function(Shooter, Victim: TActivePlayer; Damage: Single; BulletID: Byte): Single');
  Compiler.AddType('TOnKillEvent',
    'procedure(Killer, Victim: TActivePlayer; WeaponType: Byte)');
  Compiler.AddType('TOnWeaponChangeEvent',
    'procedure (Player: TActivePlayer; Primary, Secondary: TPlayerWeapon)');
  Compiler.AddType('TOnVoteMapStartEvent',
    'function(Player: TActivePlayer; Map: string): Boolean');
  Compiler.AddType('TOnVoteKickStartEvent',
    'function(Player, Victim: TActivePlayer; Reason: string): Boolean');
  Compiler.AddType('TOnVoteMapEvent', 'procedure(Player: TActivePlayer; Map: string)');
  Compiler.AddType('TOnVoteKickEvent', 'procedure(Player, Victim: TActivePlayer)');
  Compiler.AddType('TOnSpeakEvent', 'procedure(Player: TActivePlayer; Text: string)');
  Compiler.AddType('TOnCommandEvent',
    'function(Player: TActivePlayer; Command: string): Boolean');

  Compiler.AddType('TKickReason',
    '(TKickNoResponse, TKickNoCheatResponse, TKickChangeTeam, TKickPing, ' +
    'TKickFlooding, TKickConsole, TKickConnectionCheat, TKickCheat, TKickLeft, ' +
    'TKickVoted, TKickAC, TKickSilent)');


  with ActivePlayer do
  begin
    RegisterMethod('function Ban(Time: Integer; Reason: string): Boolean');
    RegisterMethod('procedure Say(Text: string; MsgType: Byte)');
    RegisterMethod('procedure Damage(Shooter: Byte; Damage: Single)');
    RegisterMethod('procedure Kill()');
    RegisterMethod(
      'procedure BigText(Layer: Byte; Text: string; Delay: Integer; Color: Longint; Scale: Single; X, Y: Integer)');
    RegisterMethod(
      'procedure WorldText(Layer: Byte; Text: string; Delay: Integer; Color: Longint; Scale, X, Y: Single)');
    RegisterMethod('procedure ForceWeapon(Primary, Secondary: TWeapon)');
    RegisterMethod('procedure ForwardTo(TargetIP: string; TargetPort: Word; Message: string)');
    RegisterMethod('procedure GiveBonus(BType: Byte)');
    RegisterMethod('function Kick(Reason: TKickReason): Boolean');
    RegisterMethod('procedure PlaySound(Name: string; X, Y: Single)');
    RegisterMethod('procedure Move(X, Y: Single)');
    RegisterMethod('procedure SetVelocity(VelX, VelY: Single)');
    RegisterMethod('procedure Tell(Text: string)');
    RegisterMethod('procedure WriteConsole(Text: string; Color: Longint)');
    RegisterMethod('procedure ChangeTeam(NewTeam: Byte; JoinType: TJoinType)');
    RegisterProperty('ID', 'Byte', iptR);
    RegisterProperty('Team', 'Byte', iptRW);
    RegisterProperty('Alive', 'Boolean', iptR);
    RegisterProperty('Kills', 'Integer', iptRW);
    RegisterProperty('Deaths', 'Integer', iptRW);
    RegisterProperty('Ping', 'Integer', iptR);
    RegisterProperty('Active', 'Boolean', iptR);
    RegisterProperty('IP', 'string', iptR);
    RegisterProperty('Port', 'Word', iptR);
    RegisterProperty('VelX', 'Single', iptR);
    RegisterProperty('VelY', 'Single', iptR);
    RegisterProperty('Muted', 'Boolean', iptRW);
    RegisterProperty('Jets', 'Integer', iptR);
    RegisterProperty('Grenades', 'Byte', iptR);
    RegisterProperty('X', 'Single', iptR);
    RegisterProperty('Y', 'Single', iptR);
    RegisterProperty('MouseAimX', 'SmallInt', iptRW);
    RegisterProperty('MouseAimY', 'SmallInt', iptRW);
    RegisterProperty('Flagger', 'Boolean', iptR);
    RegisterProperty('Time', 'Integer', iptR);
    RegisterProperty('OnGround', 'Boolean', iptR);
    RegisterProperty('IsProne', 'Boolean', iptR);
    RegisterProperty('Human', 'Boolean', iptR);
    RegisterProperty('Direction', 'Shortint', iptR);
    RegisterProperty('Flags', 'Byte', iptR);
    RegisterProperty('HWID', 'string', iptR);
    {$IFDEF STEAM}
    RegisterProperty('SteamID', 'Integer', iptR);
    {$ENDIF}
    RegisterProperty('KeyUp', 'Boolean', iptRW);
    RegisterProperty('KeyLeft', 'Boolean', iptRW);
    RegisterProperty('KeyRight', 'Boolean', iptRW);
    RegisterProperty('KeyShoot', 'Boolean', iptRW);
    RegisterProperty('KeyJetpack', 'Boolean', iptRW);
    RegisterProperty('KeyGrenade', 'Boolean', iptRW);
    RegisterProperty('KeyChangeWeap', 'Boolean', iptRW);
    RegisterProperty('KeyThrow', 'Boolean', iptRW);
    RegisterProperty('KeyReload', 'Boolean', iptRW);
    RegisterProperty('KeyCrouch', 'Boolean', iptRW);
    RegisterProperty('KeyProne', 'Boolean', iptRW);
    RegisterProperty('KeyFlagThrow', 'Boolean', iptRW);
    RegisterProperty('WeaponActive', 'Boolean Byte', iptW);
    RegisterProperty('OnFlagGrab', 'TOnFlagGrabEvent', iptRW);
    RegisterProperty('OnFlagReturn', 'TOnFlagReturnEvent', iptRW);
    RegisterProperty('OnFlagScore', 'TOnFlagScoreEvent', iptRW);
    RegisterProperty('OnFlagDrop', 'TOnFlagDropEvent', iptRW);
    RegisterProperty('OnKitPickup', 'TOnKitPickupEvent', iptRW);
    RegisterProperty('OnBeforeRespawn', 'TOnBeforeRespawnEvent', iptRW);
    RegisterProperty('OnAfterRespawn', 'TOnAfterRespawnEvent', iptRW);
    RegisterProperty('OnDamage', 'TOnDamageEvent', iptRW);
    RegisterProperty('OnKill', 'TOnKillEvent', iptRW);
    RegisterProperty('OnWeaponChange', 'TOnWeaponChangeEvent', iptRW);
    RegisterProperty('OnVoteMapStart', 'TOnVoteMapStartEvent', iptRW);
    RegisterProperty('OnVoteKickStart', 'TOnVoteKickStartEvent', iptRW);
    RegisterProperty('OnVoteMap', 'TOnVoteMapEvent', iptRW);
    RegisterProperty('OnVoteKick', 'TOnVoteKickEvent', iptRW);
    RegisterProperty('OnSpeak', 'TOnSpeakEvent', iptRW);
    RegisterProperty('OnCommand', 'TOnCommandEvent', iptRW);
  end;
end;

procedure TScriptPlayerAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptPlayer, 'TPlayer') do
  begin
    RegisterPropertyHelper(@ScriptPlayerGetTeam, nil, 'Team');
    RegisterPropertyHelper(@ScriptPlayerGetName, nil, 'Name');
    RegisterPropertyHelper(@ScriptPlayerGetAlive, nil, 'Alive');
    RegisterPropertyHelper(@ScriptPlayerGetHealth, @ScriptPlayerSetHealth, 'Health');
    RegisterPropertyHelper(@ScriptPlayerGetVest, @ScriptPlayerSetVest, 'Vest');
    RegisterPropertyHelper(@ScriptPlayerGetPrimary, nil, 'Primary');
    RegisterPropertyHelper(@ScriptPlayerGetSecondary, nil, 'Secondary');
    RegisterPropertyHelper(@ScriptPlayerGetShirtColor, nil, 'ShirtColor');
    RegisterPropertyHelper(@ScriptPlayerGetPantsColor, nil, 'PantsColor');
    RegisterPropertyHelper(@ScriptPlayerGetSkinColor, nil, 'SkinColor');
    RegisterPropertyHelper(@ScriptPlayerGetHairColor, nil, 'HairColor');
    RegisterPropertyHelper(@ScriptPlayerGetFavouriteWeapon,
      @ScriptPlayerSetFavouriteWeapon, 'FavouriteWeapon');
    RegisterPropertyHelper(@ScriptPlayerGetChosenSecondaryWeapon, nil,
      'ChosenSecondaryWeapon');
    RegisterPropertyHelper(@ScriptPlayerGetFriend, @ScriptPlayerSetFriend, 'Friend');
    RegisterPropertyHelper(@ScriptPlayerGetAccuracy, @ScriptPlayerSetAccuracy,
      'Accuracy');
    RegisterPropertyHelper(@ScriptPlayerGetShootDead, @ScriptPlayerSetShootDead,
      'ShootDead');
    RegisterPropertyHelper(@ScriptPlayerGetGrenadeFrequency,
      @ScriptPlayerSetGrenadeFrequency, 'GrenadeFrequency');
    RegisterPropertyHelper(@ScriptPlayerGetCamping, @ScriptPlayerSetCamping, 'Camping');
    RegisterPropertyHelper(@ScriptPlayerGetOnStartUse, @ScriptPlayerSetOnStartUse,
      'OnStartUse');
    RegisterPropertyHelper(@ScriptPlayerGetHairStyle, nil, 'HairStyle');
    RegisterPropertyHelper(@ScriptPlayerGetHeadgear, nil, 'Headgear');
    RegisterPropertyHelper(@ScriptPlayerGetChain, nil, 'Chain');
    RegisterPropertyHelper(@ScriptPlayerGetChatFrequency,
      @ScriptPlayerSetChatFrequency, 'ChatFrequency');
    RegisterPropertyHelper(@ScriptPlayerGetChatKill, @ScriptPlayerSetChatKill,
      'ChatKill');
    RegisterPropertyHelper(@ScriptPlayerGetChatDead, @ScriptPlayerSetChatDead,
      'ChatDead');
    RegisterPropertyHelper(@ScriptPlayerGetChatLowHealth,
      @ScriptPlayerSetChatLowHealth, 'ChatLowHealth');
    RegisterPropertyHelper(@ScriptPlayerGetChatSeeEnemy,
      @ScriptPlayerSetChatSeeEnemy, 'ChatSeeEnemy');
    RegisterPropertyHelper(@ScriptPlayerGetChatWinning, @ScriptPlayerSetChatWinning,
      'ChatWinning');
    RegisterPropertyHelper(@ScriptPlayerGetAdmin, @ScriptPlayerSetAdmin, 'IsAdmin');
    RegisterPropertyHelper(@ScriptPlayerGetDummy, @ScriptPlayerSetDummy, 'Dummy');
  end;

  with Exec.AddClass(TScriptPlayer, 'TNewPlayer') do
  begin
    RegisterConstructor(@TScriptNewPlayer.Create, 'Create');
    RegisterMethod(@TScriptNewPlayer.Free, 'Free');
    RegisterPropertyHelper(@ScriptNewPlayerGetName, @ScriptNewPlayerSetName, 'Name');
    RegisterPropertyHelper(@ScriptNewPlayerGetTeam, @ScriptNewPlayerSetTeam, 'Team');
    RegisterPropertyHelper(@ScriptNewPlayerGetHealth,
      @ScriptNewPlayerSetHealth, 'Health');
    RegisterPropertyHelper(@ScriptNewPlayerGetPrimary, @ScriptNewPlayerSetPrimary,
      'Primary');
    RegisterPropertyHelper(@ScriptNewPlayerGetSecondary, @ScriptNewPlayerSetSecondary,
      'Secondary');
    RegisterPropertyHelper(@ScriptNewPlayerGetShirtColor,
      @ScriptNewPlayerSetShirtColor, 'ShirtColor');
    RegisterPropertyHelper(@ScriptNewPlayerGetPantsColor,
      @ScriptNewPlayerSetPantsColor, 'PantsColor');
    RegisterPropertyHelper(@ScriptNewPlayerGetSkinColor,
      @ScriptNewPlayerSetSkinColor, 'SkinColor');
    RegisterPropertyHelper(@ScriptNewPlayerGetHairColor,
      @ScriptNewPlayerSetHairColor, 'HairColor');
    RegisterPropertyHelper(@ScriptNewPlayerGetHairStyle,
      @ScriptNewPlayerSetHairStyle, 'HairStyle');
    RegisterPropertyHelper(@ScriptNewPlayerGetHeadgear,
      @ScriptNewPlayerSetHeadgear, 'Headgear');
    RegisterPropertyHelper(@ScriptNewPlayerGetChain, @ScriptNewPlayerSetChain, 'Chain');
    RegisterPropertyHelper(@ScriptNewPlayerGetDummy, @ScriptNewPlayerSetDummy, 'Dummy');
  end;

  with Exec.AddClass(TScriptPlayer, 'TActivePlayer') do
  begin
    RegisterMethod(@TScriptActivePlayer.Ban, 'Ban');
    RegisterMethod(@TScriptActivePlayer.Say, 'Say');
    RegisterMethod(@TScriptActivePlayer.Damage, 'Damage');
    RegisterMethod(@TScriptActivePlayer.Kill, 'Kill');
    RegisterMethod(@TScriptActivePlayer.BigText, 'BigText');
    RegisterMethod(@TScriptActivePlayer.WorldText, 'WorldText');
    RegisterMethod(@TScriptActivePlayer.ForceWeapon, 'ForceWeapon');
    RegisterMethod(@TScriptActivePlayer.ForwardTo, 'ForwardTo');
    RegisterMethod(@TScriptActivePlayer.GiveBonus, 'GiveBonus');
    RegisterMethod(@TScriptActivePlayer.Kick, 'Kick');
    RegisterMethod(@TScriptActivePlayer.PlaySound, 'PlaySound');
    RegisterMethod(@TScriptActivePlayer.Move, 'Move');
    RegisterMethod(@TScriptActivePlayer.SetVelocity, 'SetVelocity');
    RegisterMethod(@TScriptActivePlayer.Tell, 'Tell');
    RegisterMethod(@TScriptActivePlayer.WriteConsole, 'WriteConsole');
    RegisterMethod(@TScriptActivePlayer.ChangeTeam, 'ChangeTeam');
    RegisterPropertyHelper(@ScriptActivePlayerGetID, nil, 'ID');
    RegisterPropertyHelper(@ScriptActivePlayerGetTeam,
      @ScriptActivePlayerSetTeam, 'Team');
    RegisterPropertyHelper(@ScriptActivePlayerGetAlive,
      nil, 'Alive');
    RegisterPropertyHelper(@ScriptActivePlayerGetKills,
      @ScriptActivePlayerSetKills, 'Kills');
    RegisterPropertyHelper(@ScriptActivePlayerGetDeaths, @ScriptActivePlayerSetDeaths, 'Deaths');
    RegisterPropertyHelper(@ScriptActivePlayerGetPing, nil, 'Ping');
    RegisterPropertyHelper(@ScriptActivePlayerGetActive, nil, 'Active');
    RegisterPropertyHelper(@ScriptActivePlayerGetIP, nil, 'IP');
    RegisterPropertyHelper(@ScriptActivePlayerGetPort, nil, 'Port');
    RegisterPropertyHelper(@ScriptActivePlayerGetVelX, nil, 'VelX');
    RegisterPropertyHelper(@ScriptActivePlayerGetVelY, nil, 'VelY');
    RegisterPropertyHelper(@ScriptActivePlayerGetMuted,
      @ScriptActivePlayerSetMuted, 'Muted');
    RegisterPropertyHelper(@ScriptActivePlayerGetJets, nil, 'Jets');
    RegisterPropertyHelper(@ScriptActivePlayerGetGrenades, nil, 'Grenades');
    RegisterPropertyHelper(@ScriptActivePlayerGetX, nil, 'X');
    RegisterPropertyHelper(@ScriptActivePlayerGetY, nil, 'Y');
    RegisterPropertyHelper(@ScriptActivePlayerGetMouseAimX,
      @ScriptActivePlayerSetMouseAimX, 'MouseAimX');
    RegisterPropertyHelper(@ScriptActivePlayerGetMouseAimY,
      @ScriptActivePlayerSetMouseAimY, 'MouseAimY');
    RegisterPropertyHelper(@ScriptActivePlayerGetFlagger, nil, 'Flagger');
    RegisterPropertyHelper(@ScriptActivePlayerGetTime, nil, 'Time');
    RegisterPropertyHelper(@ScriptActivePlayerGetOnGround, nil, 'OnGround');
    RegisterPropertyHelper(@ScriptActivePlayerGetProne, nil, 'IsProne');
    RegisterPropertyHelper(@ScriptActivePlayerGetHuman, nil, 'Human');
    RegisterPropertyHelper(@ScriptActivePlayerGetDirection, nil, 'Direction');
    RegisterPropertyHelper(@ScriptActivePlayerGetFlags, nil, 'Flags');
    RegisterPropertyHelper(@ScriptActivePlayerGetHWID, nil, 'HWID');
    {$IFDEF STEAM}
    RegisterPropertyHelper(@ScriptActivePlayerGetSteamID, nil, 'SteamID');
    {$ENDIF}
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyUp,
      @ScriptActivePlayerSetKeyUp, 'KeyUp');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyLeft,
      @ScriptActivePlayerSetKeyLeft, 'KeyLeft');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyRight,
      @ScriptActivePlayerSetKeyRight, 'KeyRight');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyShoot,
      @ScriptActivePlayerSetKeyShoot, 'KeyShoot');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyJetpack,
      @ScriptActivePlayerSetKeyJetpack, 'KeyJetpack');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyGrenade,
      @ScriptActivePlayerSetKeyGrenade, 'KeyGrenade');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyChangeWeap,
      @ScriptActivePlayerSetKeyChangeWeap, 'KeyChangeWeap');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyThrow,
      @ScriptActivePlayerSetKeyThrow, 'KeyThrow');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyReload,
      @ScriptActivePlayerSetKeyReload, 'KeyReload');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyCrouch,
      @ScriptActivePlayerSetKeyCrouch, 'KeyCrouch');
    RegisterPropertyHelper(@ScriptActivePlayerGetKeyProne,
      @ScriptActivePlayerSetKeyProne, 'KeyProne');
        RegisterPropertyHelper(@ScriptActivePlayerGetKeyFlagThrow,
      @ScriptActivePlayerSetKeyFlagThrow, 'KeyFlagThrow');
    RegisterPropertyHelper(nil, @ScriptActivePlayerSetWeaponActive, 'WeaponActive');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnFlagGrab,
      @ScriptActivePlayerSetOnFlagGrab, 'OnFlagGrab');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnFlagReturn,
      @ScriptActivePlayerSetOnFlagReturn, 'OnFlagReturn');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnFlagScore,
      @ScriptActivePlayerSetOnFlagScore, 'OnFlagScore');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnFlagDrop,
      @ScriptActivePlayerSetOnFlagDrop, 'OnFlagDrop');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnKitPickup,
      @ScriptActivePlayerSetOnKitPickup, 'OnKitPickup');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnBeforeRespawn,
      @ScriptActivePlayerSetOnBeforeRespawn, 'OnBeforeRespawn');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnAfterRespawn,
      @ScriptActivePlayerSetOnAfterRespawn, 'OnAfterRespawn');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnDamage,
      @ScriptActivePlayerSetOnDamage, 'OnDamage');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnKill,
      @ScriptActivePlayerSetOnKill, 'OnKill');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnWeaponChange,
      @ScriptActivePlayerSetOnWeaponChange, 'OnWeaponChange');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnVoteMapStart,
      @ScriptActivePlayerSetOnVoteMapStart, 'OnVoteMapStart');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnVoteKickStart,
      @ScriptActivePlayerSetOnVoteKickStart, 'OnVoteKickStart');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnVoteMap,
      @ScriptActivePlayerSetOnVoteMap, 'OnVoteMap');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnVoteKick,
      @ScriptActivePlayerSetOnVoteKick, 'OnVoteKick');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnSpeak,
      @ScriptActivePlayerSetOnSpeak, 'OnSpeak');
    RegisterEventPropertyHelper(@ScriptActivePlayerGetOnCommand,
      @ScriptActivePlayerSetOnCommand, 'OnCommand');
  end;
end;

end.

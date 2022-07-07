{*******************************************************}
{                                                       }
{       ScriptPlayers unit for OPENSOLDAT               }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit ScriptPlayers;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  ScriptCore3Api,
  ScriptPlayer,
  Sprites,
  Things,
  fgl,
  SysUtils;

type

  TScriptPlayers = class(TObject)
  private
    FPlayers: array [1..MAX_SPRITES] of TScriptActivePlayer;
    FActivePlayers: TFPGList<TScriptActivePlayer>;
    function GetPlayer(ID: Byte): TScriptActivePlayer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Player: TScriptNewPlayer; JoinType: TJoinType): TScriptActivePlayer;
    procedure WriteConsole(Text: string; Color: Longint);
    procedure BigText(Layer: Byte; Text: string; Delay: Integer;
      Color: Longint; Scale: Single; X, Y: Integer);
    procedure WorldText(Layer: Byte; Text: string; Delay: Integer;
      Color: Longint; Scale, X, Y: Single);
    function GetByName(Name: string): TScriptActivePlayer;
    function GetByIP(IP: string): TScriptActivePlayer;
    procedure Tell(Text: string);

    property Player[ID: Byte]: TScriptActivePlayer read GetPlayer; default;
    property Active: TFPGList<TScriptActivePlayer> read FActivePlayers;
  end;

  TScriptPlayersAPI = class(TScriptCore3API)
  private
    FPlayers: TScriptPlayers;

  public
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
    procedure RuntimeRegisterVariables(Exec: TPascalExec); override;
    property Players: TScriptPlayers read FPlayers;
  end;

implementation

uses
  Constants,
  Vector,
  Net,
  NetworkServerMessages,
  NetworkServerConnection,
  ScriptDispatcher,
  ServerHelper,
  Server,
  Game;

constructor TScriptPlayers.Create;
var
  I: Byte;
begin
  for I := 1 to MAX_SPRITES do
    FPlayers[I] := TScriptActivePlayer.Create(Sprite[I], I);
  FActivePlayers := TFPGList<TScriptActivePlayer>.Create;
end;

destructor TScriptPlayers.Destroy;
var
  I: Byte;
begin
  for I := 1 to MAX_SPRITES do
    FPlayers[I].Free;
  FActivePlayers.Free;
end;

function TScriptPlayers.GetPlayer(ID: Byte): TScriptActivePlayer;
begin
  if (ID > MAX_PLAYERS) or (ID = 0) then
    raise Exception.Create('ID must be from 1 to ' + IntToStr(MAX_SPRITES));
  Result := FPlayers[ID];
end;

function TScriptPlayers.Add(Player: TScriptNewPlayer; JoinType: TJoinType): TScriptActivePlayer;
var
  a, b: TVector2;
  i: Byte;
  TempStr: string = '';
begin
  Result := nil;
  if PlayersNum = 32 then
    Exit;

  b := Default(TVector2);
  a := Default(TVector2);

  RandomizeStart(a, 0);
  i := CreateSprite(a, b, 1, 255, Player.Sprite.Player.Clone(), True);
  Result := Self.Player[i];

  Sprite[i].Player.ApplyShirtColorFromTeam;
  Sprite[i].Brain := Player.Sprite.Brain;
  Sprite[i].DeadMeat := Player.Alive;
  Sprite[i].Health := Player.Health;
  Sprite[i].Weapon := Player.Primary.Gun;
  Sprite[i].SecondaryWeapon := Player.Secondary.Gun;
  Sprite[i].Dummy := Player.Dummy;
  Sprite[i].Player.ControlMethod := BOT;
  Sprite[i].Player.JetColor := (Sprite[i].Player.JetColor and $00FFFFFF) + COLOR_TRANSPARENCY_BOT;

  Sprite[i].FreeControls;

  Sprite[i].Respawn;
  Sprite[i].Player.ControlMethod := BOT;
  Sprite[i].Player.ChatWarnings := 0;
  Sprite[i].Player.GrabsPerSecond := 0;

  {$IFDEF SCRIPT}
  ScrptDispatcher.OnJoinTeam(i, Sprite[i].Player.Team, Sprite[i].Player.Team, True);
  ScrptDispatcher.OnWeaponChange(i, Sprite[i].Weapon.Num, Sprite[i].SecondaryWeapon.Num,
    Sprite[i].Weapon.AmmoCount, Sprite[i].SecondaryWeapon.AmmoCount);
  {$ENDIF}

  case JoinType of
    TJoinNormal: begin
      case Player.Team of
        0: TempStr := 'the game';
        1: TempStr := 'alpha team';
        2: TempStr := 'bravo team';
        3: TempStr := 'charlie team';
        4: TempStr := 'delta team';
        5: TempStr := 'as spectator';
      end;

      MainConsole.console(Sprite[i].Player.Name + ' ' + 'has joined ' +
        TempStr + '.', ENTER_MESSAGE_COLOR);
      ServerSendNewPlayerInfo(i, JOIN_NORMAL);
    end;
    TJoinSilent: ServerSendNewPlayerInfo(i, JOIN_SILENT);
    //else raise EArgumentException.Create('Unknown JoinType ' + IntToStr(Integer(JoinType)));
  end;

  //Sortplayers;
end;

procedure TScriptPlayers.WriteConsole(Text: string; Color: Longint);
begin
  ServerHelper.WriteConsole(0, Text, Color);
end;

procedure TScriptPlayers.BigText(Layer: Byte; Text: string; Delay: Integer;
  Color: Longint; Scale: Single; X, Y: Integer);
begin
  NetworkServerMessages.ServerSendSpecialMessage(Text, 1, Layer, Delay, Scale, Color, X, Y, 0);
end;

procedure TScriptPlayers.WorldText(Layer: Byte; Text: string; Delay: Integer;
  Color: Longint; Scale, X, Y: Single);
begin
  NetworkServerMessages.ServerSendSpecialMessage(Text, 2, Layer, Delay, Scale, Color, X, Y, 0);
end;

function TScriptPlayers.GetByName(Name: string): TScriptActivePlayer;
var
  i: Byte;
begin
  Result := nil;
  for i := 1 to MAX_PLAYERS do
  begin
    if not Player[i].Active then
      Exit;
    if Player[i].Name = Name then
    begin
      Result := Player[i];
      Exit;
    end;
  end;
end;

function TScriptPlayers.GetByIP(IP: string): TScriptActivePlayer;
var
  i: Byte;
begin
  Result := nil;
  for i := 1 to MAX_PLAYERS do
  begin
    if not Player[i].Active then
      Exit;
    if Player[i].IP = IP then
    begin
      Result := Player[i];
      Exit;
    end;
  end;
end;

procedure TScriptPlayers.Tell(Text: string);
begin
  ServerSendStringMessage(WideString(Text), ALL_PLAYERS, 255, MSGTYPE_PUB);
end;

procedure ScriptPlayersGetPlayerHelper(Self: TScriptPlayers;
  var Result: TScriptPlayer; const ID: Byte);
begin
  Result := Self[ID];
end;

procedure ScriptPlayersGetActiveHelper(Self: TScriptPlayers;
  var Result: TFPGList<TScriptActivePlayer>);
begin
  Result := Self.FActivePlayers;
end;

procedure ActivePlayerListItemsHelper(Self: TFPGList<TScriptActivePlayer>;
  var Result: TScriptActivePlayer; const ID: Integer);
begin
  Result := Self[ID];
end;

procedure ActivePlayerListCountHelper(Self: TFPGList<TScriptActivePlayer>;
  var Result: Integer);
begin
  Result := Self.Count;
end;

procedure TScriptPlayersAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  AClass: TPascalCompiletimeClass;
begin

  with Compiler.AddClass(nil, 'TActivePlayerList') do
  begin
    RegisterProperty('Items', 'TActivePlayer Integer', iptR);
    RegisterProperty('Count', 'Integer', iptR);
    SetDefaultPropery('Items');
  end;

  AClass := Compiler.AddClass(nil, 'TPlayers');
  with AClass do
  begin
    RegisterMethod('function Add(Player: TNewPlayer; JoinType: TJoinType): TActivePlayer;');
    RegisterMethod('procedure WriteConsole(Text: string; Color: Longint)');
    RegisterMethod(
      'procedure BigText(Layer: Byte; Text: string; Delay: Integer; Color: Longint; Scale: Single; X, Y: Integer)');
    RegisterMethod(
      'procedure WorldText(Layer: Byte; Text: string; Delay: Integer; Color: Longint; Scale, X, Y: Single)');
    RegisterMethod('function GetByName(Name: string): TActivePlayer');
    RegisterMethod('function GetByIP(IP: string): TActivePlayer');
    RegisterMethod('procedure Tell(Text: string)');
    RegisterProperty('Player', 'TActivePlayer Byte', iptR);
    RegisterProperty('Active', 'TActivePlayerList', iptR);
    SetDefaultPropery('Player');
  end;
  Compiler.AddPtrVariable('Players', AClass.aType);
end;

procedure TScriptPlayersAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TFPGList<TScriptActivePlayer>, 'TActivePlayerList') do
  begin
    RegisterPropertyHelper(@ActivePlayerListItemsHelper, nil, 'Items');
    RegisterPropertyHelper(@ActivePlayerListCountHelper, nil, 'Count');
  end;

  with Exec.AddClass(TScriptPlayers, 'TPlayers') do
  begin
    RegisterMethod(@TScriptPlayers.Add, 'Add');
    RegisterMethod(@TScriptPlayers.WriteConsole, 'WriteConsole');
    RegisterMethod(@TScriptPlayers.BigText, 'BigText');
    RegisterMethod(@TScriptPlayers.WorldText, 'WorldText');
    RegisterMethod(@TScriptPlayers.GetByName, 'GetByName');
    RegisterMethod(@TScriptPlayers.GetByIP, 'GetByIP');
    RegisterMethod(@TScriptPlayers.Tell, 'Tell');
    RegisterPropertyHelper(@ScriptPlayersGetPlayerHelper, nil, 'Player');
    RegisterPropertyHelper(@ScriptPlayersGetActiveHelper, nil, 'Active');
  end;
end;

procedure TScriptPlayersAPI.RuntimeRegisterVariables(Exec: TPascalExec);
var
  i: Byte;
begin
  if FPlayers = nil then
    FPlayers := TScriptPlayers.Create;
  Exec.SetPointerToData('Players', @FPlayers, Exec.FindType(btClass));
  for i := 1 to MAX_PLAYERS do
    if FPlayers[i].Active then
      FPlayers.Active.Add(FPlayers[i]);
end;

end.

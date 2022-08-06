{*******************************************************}
{                                                       }
{       Demo Unit for OPENSOLDAT                        }
{                                                       }
{       Copyright (c) 2002 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Demo;

interface

uses
  {$IFNDEF SERVER}
  GameStrings, ClientGame,
  {$ENDIF}
  SysUtils, Classes, Vector, Sprites,
  {$IFNDEF SERVER}Steam,{$ENDIF}
  Net;


const
  DEMO_MAGIC: array[1..6] of Char = 'SOLDEM';

type

  TDemoHeader = record
    Header: array[1..6] of Char;
    Version: Word;
    MapName: array[0..160] of Char;
    StartDate: Integer;
    TicksNum: Integer;
  end;

  TDemo = class
  private
    FDemoFile: TMemoryStream;
    FDemoHeader: TDemoHeader;
    FActive: Boolean;
    FName: AnsiString;
    FOldCam: Byte;
  public
    property Active: Boolean read FActive write FActive;
    property Name: AnsiString read FName;
    property Header: TDemoHeader read FDemoHeader;
  end;

  TDemoRecorder = class(TDemo)
  private
    FTicksNum: Integer;
    function CreateDemoPlayer: Integer;
    {$IFNDEF SERVER}
    procedure SaveCamera;
    {$ENDIF}
  public
    function StartRecord(Filename: string): Boolean;
    procedure StopRecord;
    procedure SaveRecord(var R; Size: Integer);
    procedure SaveNextFrame;
    {$IFNDEF SERVER}
    procedure SavePosition;
    {$ENDIF}
    property Name: AnsiString read FName;
    property Active: Boolean read FActive write FActive;
    property TicksNum: Integer read FTicksNum;
  end;

  {$IFNDEF SERVER}
  TDemoPlayer = class(TDemo)
  private
    FSkipTo: Integer;
  public
    function OpenDemo(Filename: string): Boolean;
    procedure StopDemo;
    procedure ProcessDemo;
    procedure Position(Ticks: Integer);
    property SkipTo: Integer read FSkipTo;
  end;
  {$ENDIF}

var
  DemoRecorder: TDemoRecorder;
  {$IFNDEF SERVER}
  DemoPlayer: TDemoPlayer;
  {$ENDIF}
  RSize: Word;
  FreeCam: Byte;
  NoTexts: Byte = 0;


implementation

uses
  {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} Game, PolyMap, Constants, Version, NetworkUtils, DateUtils,
  NetworkServerConnection, NetworkServerThing {$IFNDEF SERVER}, InterfaceGraphics {$ENDIF};


function TDemoRecorder.StartRecord(Filename: string): Boolean;
var
  SpriteID: Integer;
begin
  Result := False;
  {$IFNDEF SERVER}
  if DemoPlayer.Active then
    Exit;
  {$ENDIF}

  FDemoFile := TMemoryStream.Create;
  MainConsole.Console(
    {$IFDEF SERVER}
    'Recording demo:' +
    {$ELSE}
    _('Recording demo:') +
    {$ENDIF}
    ' ' + WideString(ExtractFileName(Filename)), INFO_MESSAGE_COLOR);

  FName := ExtractFileName(Filename);
  FActive := True;
  FOldCam := 255;
  FTicksNum := 0;

  FDemoHeader.Header := 'SOLDEM';
  FDemoHeader.TicksNum := 0;
  FDemoHeader.StartDate := DateTimeToUnix(Now);
  FDemoHeader.Version := DEMO_VERSION;

  FillChar(FDemoHeader.MapName, SizeOf(FDemoHeader.MapName), #0);
  StringToArray(FDemoHeader.MapName, Map.Name);

  FDemoFile.WriteBuffer(FDemoHeader, SizeOf(FDemoHeader));

  SpriteID := CreateDemoPlayer;

  if SpriteID = MAX_SPRITES then
    Result := True;
end;

procedure TDemoRecorder.StopRecord;
begin
  if not Active then
    Exit;

  MainConsole.Console(
    {$IFDEF SERVER}
    'Demo stopped' +
    {$ELSE}
    _('Demo stopped') +
    {$ENDIF}
    ' (' + WideString(FName) + ')', INFO_MESSAGE_COLOR);

  Sprite[MAX_SPRITES].Kill;

  FDemoFile.Position := 0;

  FDemoHeader.Version := DEMO_VERSION;
  FDemoHeader.TicksNum := TicksNum;

  FillChar(FDemoHeader.MapName, SizeOf(FDemoHeader.MapName), #0);
  StringToArray(FDemoHeader.MapName, Map.Name);

  FDemoFile.WriteBuffer(FDemoHeader, SizeOf(FDemoHeader));

  try
    FDemoFile.SaveToFile(UserDirectory + 'demos/' + FName);
  except
    on e: Exception do
      MainConsole.Console('Failed to save demo file: ' + WideString(E.Message), INFO_MESSAGE_COLOR);
  end;

  FActive := False;
  FName := '';
  FDemoFile.Free;
end;

function TDemoRecorder.CreateDemoPlayer: Integer;
var
  P: Integer;
  Player: TPlayer;
  A: TVector2;
begin
  Result := -1;

  if Sprite[MAX_SPRITES].Active then
  begin
    MainConsole.Console('Failed to create Demo Recorder player. Demos can be recorded with up to 31 players', INFO_MESSAGE_COLOR);
    StopRecord;
    Exit;
  end;

  Player := TPlayer.Create;
  Player.DemoPlayer := True;
  Player.Name := 'Demo Recorder';
  Player.Team := TEAM_SPECTATOR;
  Player.ControlMethod := HUMAN;

  {$IFDEF SERVER}
  Player.peer := High(LongWord);
  {$ENDIF}

  A.x := MIN_SECTORZ * Map.SectorsDivision * 0.7;
  A.y := MIN_SECTORZ * Map.SectorsDivision * 0.7;

  P := CreateSprite(A, Vector2(0, 0), 1, MAX_SPRITES, Player, True);
  if (P > 0) and (P < MAX_SPRITES + 1) then
  begin
    {$IFDEF SERVER}
    ServerSyncCvars(P, Player.peer, True);
    ServerSendPlayList(Player.peer);
    {$ELSE}
    ServerSyncCvars({$IFDEF SERVER}P, 0,{$ENDIF} True);
    ServerSendPlayList({$IFDEF SERVER}0{$ENDIF});
    {$ENDIF}
    ServerVars({$IFDEF SERVER}P{$ENDIF});
    ServerSendNewPlayerInfo(P, JOIN_NORMAL);
    ServerThingMustSnapshotOnConnect({$IFDEF SERVER}P{$ENDIF});
    Sprite[P].Player.DemoPlayer := True;
    Spriteparts.Pos[P] := Vector2(0, 0);
    Result := P;
  end;

end;

procedure TDemoRecorder.SaveRecord(var R; Size: Integer);
begin
  if Size = 0 then
    Exit;

  if not Active then
    Exit;
  FDemoFile.Write(Size, SizeOf(RSize));
  FDemoFile.Write(R, Size);
end;

{$IFNDEF SERVER}
procedure TDemoRecorder.SaveCamera;
var
  Msg: TMsg_ClientSpriteSnapshot_Dead;
begin
  Msg.Header.ID := MsgID_ClientSpriteSnapshot_Dead;
  Msg.CameraFocus := CameraFollowSprite;
  SaveRecord(Msg, SizeOf(Msg));
end;

procedure TDemoRecorder.SavePosition;
var
  MovementMsg: TMsg_ServerSpriteDelta_Movement;
begin
  MovementMsg.Header.ID := MsgID_Delta_Movement;

  MovementMsg.Num := MySprite;
  MovementMsg.Velocity := SpriteParts.Velocity[MySprite];
  MovementMsg.Pos := SpriteParts.Pos[MySprite];
  MovementMsg.ServerTick := MainTickCounter;

  EncodeKeys(Sprite[MySprite], MovementMsg.Keys16);

  MovementMsg.MouseAimX := Sprite[MySprite].Control.MouseAimX;
  MovementMsg.MouseAimY := Sprite[MySprite].Control.MouseAimY;

  SaveRecord(MovementMsg, sizeof(MovementMsg));
end;
{$ENDIF}

procedure TDemoRecorder.SaveNextFrame;
begin
  if not FActive then
    Exit;

  // save record type
  RSize := 1;

  FDemoFile.Write(RSize, SizeOf(RSize));

  // save camera change
  {$IFNDEF SERVER}
  if FOldCam <> CameraFollowSprite then
  begin
    SaveCamera;
    FOldCam := CameraFollowSprite;
  end;
  {$ENDIF}

  Inc(FTicksNum);
end;

{$IFNDEF SERVER}
function TDemoPlayer.OpenDemo(Filename: string): Boolean;
begin
  Result := False;
  FDemoFile := TMemoryStream.Create;

  try
    FDemoFile.LoadFromFile(Filename);
  except
    on e: Exception do
    begin
      MainConsole.Console(_('Failed to load demo file: ') + WideString(E.Message), INFO_MESSAGE_COLOR);
      Exit;
    end;
  end;

  FDemoFile.ReadBuffer(FDemoHeader, SizeOf(FDemoHeader));
  if (FDemoHeader.Header <> 'SOLDEM') then
  begin
    MainConsole.Console(_('The provided file is not valid: ') + ' ' + WideString(FName), INFO_MESSAGE_COLOR);
    FDemoFile.Free;
  end
  else if (FDemoHeader.Version <> DEMO_VERSION) then
  begin
    MainConsole.Console(_(WideFormat('Wrong demo version: %d - %d', [DEMO_VERSION, FDemoHeader.Version])), INFO_MESSAGE_COLOR);
    FDemoFile.Free;
  end else
  begin
    FName := ExtractFileName(Filename);
    MainConsole.Console(_('Playing demo') + ' ' + WideString(FName), INFO_MESSAGE_COLOR);
    Spectator := 1;
    FActive := True;
    Result := True;
  end;
end;

procedure TDemoPlayer.StopDemo;
begin
  if not FActive then
    Exit;

  MainConsole.Console('Demo stopped', INFO_MESSAGE_COLOR);

  FDemoFile.Free;

  FActive := False;
end;

procedure TDemoPlayer.ProcessDemo;
var
  ReadBuf: TCharArray;
  packet: PSteamNetworkingMessage_t;
begin
  ReadBuf := Default(TCharArray);
  repeat
    if not FActive then
      Exit;

    if FDemoFile.Position = FDemoFile.Size then
    begin
      StopDemo;
      ExitToMenu;
      Exit;
    end;

    try
      FDemoFile.Read(RSize, SizeOf(RSize));
    except
      Exit;
    end;

    if (FSkipTo > 0) and (MainTickCounter >= FSkipTo) then
    begin
      FSkipTo := -1;
      ShouldRenderFrames := True;
      GOALTICKS := Round(demo_speed.Value * DEFAULT_GOALTICKS);
    end;

    if RSize = 0 then
    begin
      FDemoFile.Position := FDemoFile.Position + 2;
      Exit;
    end;

    if RSize = 1 then  // next frame
      Exit;

    try
    begin
      SetLength(ReadBuf, RSize);
      FDemoFile.Read(ReadBuf[0], RSize);
    end;
    except
      Exit;
    end;

    packet := UDP.NetworkingUtil.AllocateMessage(0);
    packet.m_pData := ReadBuf;
    packet.m_cbSize := RSize;
    UDP.HandleMessages(packet);
    packet^.Release();
    SetLength(ReadBuf, 0);

  until RSize = 1;
end;

procedure TDemoPlayer.Position(Ticks: Integer);
var
  i: Integer;
begin
  FSkipTo := Ticks;
  ShouldRenderFrames := False;

  if FSkipTo < MainTickCounter then
  begin
    FDemoFile.Seek(SizeOf(FDemoHeader), soFromBeginning);

    MainTickCounter := 0;

    for i := 1 to MAX_SPRITES do
      Sprite[i].Kill;
    for i := 1 to MAX_BULLETS do
      Bullet[i].Kill;
    for i := 1 to MAX_SPARKS do
      Spark[i].Kill;
    for i := 1 to MAX_THINGS do
      Thing[i].Kill;

    // Reset World and Big Texts
    for i := 0 to MAX_BIG_MESSAGES do
    begin
      // Big Text
      BigText[i] := '';
      BigDelay[i] := 0;
      BigScale[i] := 0;
      BigColor[i] := 0;
      BigPosX[i] := 0;
      BigPosY[i] := 0;
      BigX[i] := 0;
      // World Text
      WorldText[i] := '';
      WorldDelay[i] := 0;
      WorldScale[i] := 0;
      WorldColor[i] := 0;
      WorldPosX[i] := 0;
      WorldPosY[i] := 0;
      WorldX[i] := 0;
    end;

    // Reset ABOVE CHAT MESSAGE
    for I := 1 to MAX_SPRITES do
    begin
      ChatDelay[i] := 0;
      ChatMessage[i] := '';
      ChatTeam[i] := False;
    end;

    MainConsole.Count := 0;
    BigConsole.Count := 0;
  end;

  GOALTICKS := DEFAULT_GOALTICKS * 20;
end;
{$ENDIF}

initialization
begin
  DemoRecorder := TDemoRecorder.Create;
  {$IFNDEF SERVER}
  DemoPlayer := TDemoPlayer.Create;
  {$ENDIF}
end;

finalization
begin
  DemoRecorder.Free;
  {$IFNDEF SERVER}
  DemoPlayer.Free;
  {$ENDIF}
end;

end.

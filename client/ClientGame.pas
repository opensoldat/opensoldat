unit ClientGame;

interface

uses
  SDL2, Constants, Vector;

procedure ResetFrameTiming;
procedure GameLoop;
function GetGameFps: Integer;
function GetCurrentTime: Extended;
procedure TabComplete;
procedure ResetWeaponStats;
procedure BigMessage(Text: WideString; Delay: Integer; Col: Cardinal);
function GetCameraTarget(Backwards: Boolean = False): Byte;
{$IFDEF STEAM}
procedure GetMicData;
{$ENDIF}

var
  MousePrev: TVector2;
  mx, my: Single;
  MapChanged: Boolean = False;
  ChatChanged: Boolean = True;  // used for blinking chat input
  ShouldRenderFrames: Boolean;  // false during game request phase

  // used for action snap
  ActionSnap: Byte = 1;
  ActionSnapTaken: Boolean = False;
  CapScreen: Integer = 255;
  ShowScreen: Boolean = False;
  ScreenCounter: Byte = 255;

  // resolution
  IsFullscreen: Boolean;
  ScreenWidth: Integer = DEFAULT_WIDTH;
  ScreenHeight: Integer = DEFAULT_HEIGHT;
  RenderWidth: Integer = 0;
  RenderHeight: Integer = 0;
  WindowWidth: Integer = 0;
  WindowHeight: Integer = 0;

  // chat stuff
  ChatText, LastChatText, FireChatText: WideString;
  ChatType, LastChatType, FireChatType: Byte;
  CompletionBase: String = '';
  CompletionBaseSeparator: Integer;
  CurrentTabCompletePlayer: Byte = 0;
  CursorPosition: Byte = 0;
  TabCompletePressed: Boolean;
  ChatTimeCounter: Integer;

  ClientStopMovingCounter: Integer = 99999;
  ForceClientSpriteSnapshotMov: Boolean;
  LastForceClientSpriteSnapshotMovTick: LongInt;
  MenuTimer: Integer;


implementation

uses
  SysUtils, StrUtils, Math, Classes,
  Client, Game, Sprites, GameStrings, Demo,
  Net, NetworkClientSprite, NetworkClientConnection,
  {$IFDEF ENABLE_FAE}FaeBase, FaeClient, NetworkClientFae,{$ENDIF}
  {$IFDEF STEAM}Steam, NetworkClientGame,{$ENDIF}
  GameRendering, Gfx, UpdateFrame, GameMenus, Util, InterfaceGraphics;

type
  TFrameTiming = record
    Frequency: Int64;
    StartTime: Int64;
    PrevTime: Extended;
    PrevRenderTime: Extended;
    Accumulator: Extended;
    MinDeltaTime: Extended;
    Elapsed: Extended;
    Counter: Integer;
    Fps: Integer;
    FpsAccum: Extended;
  end;

var
  FrameTiming: TFrameTiming;

procedure ResetFrameTiming;
begin
  FrameTiming.Frequency := SDL_GetPerformanceFrequency;
  FrameTiming.StartTime := SDL_GetPerformanceCounter;

  FrameTiming.PrevTime := GetCurrentTime;
  FrameTiming.PrevRenderTime := FrameTiming.PrevTime;
  FrameTiming.Accumulator := 0;
  FrameTiming.MinDeltaTime := 0;
  FrameTiming.Elapsed := 0;

  FrameTiming.Counter := 0;
  FrameTiming.Fps := 0;
  FrameTiming.FpsAccum := 0;

  if r_fpslimit.Value then
    FrameTiming.MinDeltaTime := 1.0 / r_maxfps.Value;

  TickTime := 0;
  TickTimeLast := 0;
end;

function GetCurrentTime: Extended;
var
  x: Int64;
begin
  x := SDL_GetPerformanceCounter;
  Result := (x - FrameTiming.StartTime) / FrameTiming.Frequency;
end;

procedure BigMessage(Text: WideString; Delay: Integer; Col: Cardinal);
var
  w, s: Single;
begin
  GfxTextPixelRatio(Vector2(1, 1));
  SetFontStyle(FONT_BIG);

  w := RectWidth(GfxTextMetrics(Text));
  s := 4.8 * (RenderHeight / 480);

  BigX[1] := 0;
  BigText[1] := Text;
  BigDelay[1] := Delay;
  BigScale[1] := Min(1 / 4.8, (0.7 * RenderWidth / w) / s);
  BigColor[1] := Col;
  BigPosX[1] := (RenderWidth - s * w * BigScale[1]) / 2;
  BigPosY[1] := 420 * _iscala.y;

  if r_scaleinterface.Value then
    BigPosX[1] := BigPosX[1] * (GameWidth / RenderWidth);
end;

// In-game nickname tab completion
procedure TabComplete;
var
  i: Integer;
  ChatTextLen, CompletionBaseLen: Integer;
  Offset, LastSeparator: Integer;
  ContinuedTabCompletePlayer, Next, AvailableChatSpace: Integer;
  SpaceFittedName: WideString;
begin
  if MySprite < 1 then
    Exit;

  ChatTextLen := Length(ChatText);

  if (ChatTextLen > 1) and (ChatText[2] = '^') then
    Offset := 1
  else
    Offset := 0;

  // If not already tab-completing, save and use this base text for tab completetion
  if CurrentTabCompletePlayer = 0 then
  begin
    // Find where the current word starts
    LastSeparator := LastDelimiter(' ', AnsiString(ChatText));

    if LastSeparator < Offset then
      LastSeparator := Offset;

    CompletionBaseLen := ChatTextLen - LastSeparator;
    CompletionBase := AnsiMidStr(String(ChatText), LastSeparator + 1, CompletionBaseLen);
    CompletionBaseSeparator := LastSeparator;
  end;

  // Next potential match
  ContinuedTabCompletePlayer := (CurrentTabCompletePlayer + 1) mod MAX_PLAYERS;

  if ChatTextLen > Offset then  // Dont complete if chat is empty
  begin
    for i := ContinuedTabCompletePlayer to (ContinuedTabCompletePlayer + MAX_PLAYERS - 1) do
    begin
      Next := ((i - 1) mod MAX_PLAYERS) + 1;
      if Sprite[Next].Active and (not Sprite[Next].Player.DemoPlayer) and (Next <> MySprite) then
      begin
        if (CompletionBase = '') or AnsiContainsText(Sprite[Next].Player.Name, CompletionBase) then
        begin
          AvailableChatSpace := MAXCHATTEXT - CompletionBaseSeparator;
          SpaceFittedName := Copy(WideString(Sprite[Next].Player.Name), 0, AvailableChatSpace);
          ChatText := Copy(ChatText, 0, CompletionBaseSeparator) + SpaceFittedName;
          CurrentTabCompletePlayer := Next;
          CursorPosition := Length(AnsiString(ChatText));
          TabCompletePressed := True;
          Break;
        end;
      end;
    end;
  end;
end;

// Resets the stats of all weapons
procedure ResetWeaponStats;
var
  i: Byte;
begin
  for i := 0 to 20 do
  begin
    WepStats[i].Shots := 0;
    WepStats[i].Hits := 0;
    WepStats[i].Kills := 0;
    WepStats[i].Headshots := 0;
    WepStats[i].Accuracy := 0;
  end;
end;

function GetGameFps: Integer;
begin
    Result := FrameTiming.Fps;
end;

procedure GameLoop;
var
  MainControl: Integer;
  HeavySendersNum: Integer;
  Adjust: Single;
  CurrentTime, FrameTime, SimTime: Extended;
  FramePercent, dt: Extended;
  GamePaused: Boolean;
  {$IFDEF ENABLE_FAE}AsyncFaeResponse: TFaeResponseBox;{$ENDIF}
begin
  GamePaused := (MapChangeCounter >= 0);

  CurrentTime := GetCurrentTime;

  FrameTime := CurrentTime - FrameTiming.PrevTime;

  FrameTiming.FpsAccum := FrameTiming.FpsAccum + FrameTime;

  FrameTiming.PrevTime := CurrentTime;
  TickTimeLast := TickTime;

  if FrameTime > 2 then
    FrameTime := 0;

  dt := 1 / GOALTICKS;

  FrameTiming.Accumulator := FrameTiming.Accumulator + FrameTime;
  TickTime := TickTime + Trunc(FrameTiming.Accumulator / dt);

  SimTime := (TickTime - TickTimeLast) * dt;
  FrameTiming.Accumulator := FrameTiming.Accumulator - SimTime;
  FramePercent := Min(1, Max(0, FrameTiming.Accumulator / dt));

  {$IFDEF ENABLE_FAE}
  // Poll for authentication result from Fae background thread (cf. ClientHandleFaeChallenge)
  if (FaePendingAuth <> nil) and FaeAuthFetch(FaePendingAuth, AsyncFaeResponse, nil) then
  begin
    ClientSendFaeResponse(AsyncFaeResponse);
    FaePendingAuth := nil;
  end;
  {$ENDIF}

  for MainControl := 1 to (Ticktime - ticktimeLast) do
  begin  // frame rate independant code
    if not GamePaused then
      FrameTiming.Elapsed := FrameTiming.Elapsed + (1 / DEFAULT_GOALTICKS);

    Inc(Ticks);

    Inc(ClientTickCount);
    // Update main tick counter
    Inc(MainTickCounter);

    if MenuTimer > -1 then
      Dec(MenuTimer);

    {$IFDEF STEAM}
    SteamAPI_RunCallbacks();
    {$ENDIF}

    // General game updating
    Update_Frame;

    if DemoRecorder.Active and (MainTickCounter mod demo_rate.Value = 0) then
      DemoRecorder.SavePosition;

    if (MapChangeCounter < 0) and (not EscMenu.Active) then
    begin
      // DEMO
      if DemoRecorder.Active then
        DemoRecorder.SaveNextFrame;
      if DemoPlayer.Active then
        DemoPlayer.ProcessDemo;
    end;

    // Radio Cooldown
    if (MainTickCounter mod SECOND = 0) and
      (RadioCooldown > 0) and (sv_radio.Value) then
      Dec(RadioCooldown);

    // Packet rate send adjusting
    if PacketAdjusting = 1 then
    begin
      HeavySendersNum := PlayersNum - SpectatorsNum;

      if HeavySendersNum < 5 then
        Adjust := 0.75
      else if HeavySendersNum < 9 then
        Adjust := 0.87
      else
        Adjust := 1.0;
    end
    else
      Adjust := 1.0;

    if (MySprite > 0) and (not DemoPlayer.Active) then
    begin
      // connection problems
      if (MapChangeCounter < 0) and not EscMenu.Active then
        Inc(NoHeartbeatTime);

      if NoHeartbeatTime > CONNECTIONPROBLEM_TIME then
      begin
        if MainTickCounter mod 120 = 0 then
          if NoHeartbeatTime > DISCONNECTION_TIME then
            MainConsole.Console(_('Connection timeout'), WARNING_MESSAGE_COLOR)
          else
            MainConsole.Console(_('Connection problem'), WARNING_MESSAGE_COLOR);

        ClientStopMovingCounter := 0;
      end;

      if NoHeartbeatTime = DISCONNECTION_TIME then
      begin
        ShowMapChangeScoreboard;

        GameMenuShow(TeamMenu, False);

        MainConsole.Console(_('Connection timeout'), WARNING_MESSAGE_COLOR);

        ClientDisconnect;
      end;

      if NoHeartbeatTime < 0 then
        NoHeartbeatTime := 0;

      Dec(ClientStopMovingCounter);

      if Connection = Internet then
      begin
        if Sprite[MySprite].Active then
        begin
          if not Sprite[MySprite].DeadMeat then
          begin
            if (MainTickCounter mod Round(7 * Adjust) = 1) and
               (MainTickCounter mod Round(5 * Adjust) <> 0) then
              ClientSpriteSnapshot;
            if (MainTickCounter mod Round(5 * Adjust) = 0) or
               ForceClientSpriteSnapshotMov then
              ClientSpriteSnapshotMov;
          end
          else
            if MainTickCounter mod Round(30 * Adjust) = 0 then
              ClientSpriteSnapshotDead;
        end;
      end
      else if Connection = LAN then
      begin
        if not Sprite[MySprite].DeadMeat then
        begin
          if MainTickCounter mod Round(4 * Adjust) = 0 then
            ClientSpriteSnapshot;

          if (MainTickCounter mod Round(3 * Adjust) = 0) or
             ForceClientSpriteSnapshotMov then
            ClientSpriteSnapshotMov;
        end
        else
          if MainTickCounter mod Round(15 * Adjust) = 0 then
            ClientSpriteSnapshotDead;
      end;

      ForceClientSpriteSnapshotMov := False;
    end;  // playing

    //UDP.FlushMsg;
  end;  // Client

  // this shouldn't happen but still done for safety
  if FrameTiming.PrevRenderTime > CurrentTime then
    FrameTiming.PrevRenderTime := CurrentTime - FrameTiming.MinDeltaTime;

  if ShouldRenderFrames and
      ((CurrentTime - FrameTiming.PrevRenderTime) >= FrameTiming.MinDeltaTime) then
  begin
    FrameTiming.PrevRenderTime := CurrentTime;
    Inc(FrameTiming.Counter);

    if FrameTiming.Counter >= 30 then
    begin
      FrameTiming.Fps := Round(FrameTiming.Counter / FrameTiming.FpsAccum);
      FrameTiming.Counter := 0;
      FrameTiming.FpsAccum := 0;
    end;

    if GamePaused then
      RenderFrame(FrameTiming.Elapsed, FramePercent, True)
    else
      RenderFrame(FrameTiming.Elapsed - dt * (1 - FramePercent), FramePercent, False);
  end;

  if (MapChangeCounter < 0) and (MapChangeCounter > -59) then
    if MapChangeName = 'EXIT*!*' then
      ExitToMenu;

  if MapChanged then
  begin
    MapChanged := False;
    ResetFrameTiming;
  end;

  if r_sleeptime.Value > 0 then
    Sleep(r_sleeptime.Value);
end;

function GetCameraTarget(Backwards: Boolean = False): Byte;
var
  NewCam: ShortInt;
  NumLoops: Byte;
  ValidCam: Boolean;
begin
  ValidCam := False;
  NewCam := CameraFollowSprite;
  NumLoops := 0;

  repeat
    Inc(NumLoops);
    if NumLoops = 33 then
    begin  // Shit, way too many loops...
      NewCam := 0;
      ValidCam := True;
      Break;
    end;

    if not Backwards then
      Inc(NewCam)
    else
      Dec(NewCam);
    if NewCam > MAX_SPRITES then
      NewCam := 1
    else if NewCam < 1 then
      NewCam := MAX_SPRITES;

    if not Sprite[NewCam].Active then
      Continue;  // Sprite slot empty
    if Sprite[NewCam].DeadMeat then
      Continue;  // Sprite is dead
    if Sprite[NewCam].IsSpectator() then
      Continue;  // Sprite is a spectator

    if Sprite[MySprite].Control.Up and (not sv_realisticmode.Value) and
       Sprite[MySprite].IsNotSpectator() then
    begin
      NewCam := 0;
      ValidCam := True;
      Break;
    end;  // Freecam if not Realistic

    if Sprite[MySprite].IsSpectator() then
    begin
      if Sprite[MySprite].Control.Up then
      begin
        NewCam := 0;
        ValidCam := True;
        Break;
      end else
      begin  // Allow spectators to go into Free Cam
        ValidCam := True;
        Break;
      end;  // Let spectator view all players
    end;

    if Sprite[NewCam].IsNotInSameTeam(Sprite[MySprite]) then
      Continue;  // Dont swap camera to a player not on my team

    ValidCam := True;
  until ValidCam;

  Result := iif(ValidCam, NewCam, CameraFollowSprite);
end;

{$IFDEF STEAM}
procedure GetMicData;
var
  AvailableVoice: EVoiceResult;
  AvailableVoiceBytes: Cardinal;
  VoiceData: array of Byte = Nil;
begin
  AvailableVoice := SteamAPI.User.GetAvailableVoice(@AvailableVoiceBytes, nil, 0);

  if (AvailableVoice = k_EVoiceResultOK) and (AvailableVoiceBytes > 0) then
  begin
    SetLength(VoiceData, AvailableVoiceBytes);
    AvailableVoice := SteamAPI.User.GetVoice(True, VoiceData, AvailableVoiceBytes, @AvailableVoiceBytes, false, nil, 0, nil, 0);

    if (AvailableVoice = k_EVoiceResultOK) and (AvailableVoiceBytes > 0) then
      ClientSendVoiceData(VoiceData, AvailableVoiceBytes);
  end;
end;
{$ENDIF}

end.

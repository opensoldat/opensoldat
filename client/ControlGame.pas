unit ControlGame;

interface

procedure GameInput;

implementation

uses
  SDL2, SysUtils, Constants, Net,
  NetworkClientConnection, NetworkClientMessages,
  NetworkClientGame, Game, Weapons, Math, InterfaceGraphics,
  Input, Sound, Classes, GameStrings, GameMenus, Demo, Util,
  {$IFDEF STEAM} Steam, {$ENDIF}
  Client, Sprites, ClientGame, Command, Cvar;

procedure ClearChatText;
begin
  ChatText := '';
  FireChatText := '';
  CompletionBase := '';
  CurrentTabCompletePlayer := 0;
  CursorPosition := 1;
  VoteKickReasonType := False;
  SDL_StopTextInput;
end;

procedure StartChat;
begin
  if (Length(FireChatText) > 0) and (FireChatType = ChatType) then
    ChatText := FireChatText
  else if ChatType = MSGTYPE_CMD then
    ChatText := '/'
  else
    ChatText := ' ';

  ChatChanged := True;
  CursorPosition := Length(ChatText);
  SDL_StartTextInput;
end;

function FilterChatText(Str: WideString): WideString;
var
  i: Integer;
begin
  Result := '';
  For i := 1 to Length(Str) do
  begin
    if (Str[i] >= #32) and (Str[i] <> #127) then
      Result := Result + Str[i];
  end;
end;

function ChatKeyDown(KeyMods: Byte; KeyCode: TSDL_KeyCode): Boolean;
var
  Len: Integer;
  Str: WideString;
  ConsoleStr: String;
begin
  Result := False;

  if Length(ChatText) > 0 then
  begin
    if ((KeyMods = KM_CTRL) and (KeyCode = SDLK_v))
      or ((KeyMods = KM_SHIFT) and (KeyCode = SDLK_INSERT)) then
    begin
      Str := FilterChatText(WideString(UTF8String(SDL_GetClipboardText)));
      Len := Length(ChatText);
      Insert(Str, ChatText, CursorPosition + 1);
      Inc(CursorPosition, Length(ChatText) - Len);

      Len := iif(VoteKickReasonType, REASON_CHARS - 1, MAXCHATTEXT);

      if Length(ChatText) > Len then
      begin
        ChatText := Copy(ChatText, 1, Len);
        CursorPosition := Min(CursorPosition, Len);
      end;

      CurrentTabCompletePlayer := 0;
      ChatChanged := True;
      Result := True;
    end
    else if ((KeyMods = KM_CTRL) and (KeyCode = SDLK_c)) then
    begin
      if SDL_SetClipboardText(PChar(UTF8String(BigConsole.GetContentsAsPlainText))) = 0 then
        MainConsole.Console(_('Copied chat contents to clipboard'), GAME_MESSAGE_COLOR)
      else
        MainConsole.Console(WideFormat(_('Failed copying chat to clipboard: %s'), [SDL_GetError()]), DEBUG_MESSAGE_COLOR);
    end
    else if (KeyMods = KM_CTRL) then
    begin
      Result := True;

      case KeyCode of
        SDLK_HOME: begin
          ChatChanged := True;
          CursorPosition := 1;
        end;

        SDLK_END: begin
          ChatChanged := True;
          CursorPosition := Length(ChatText);
        end;

        SDLK_RIGHT: begin
          ChatChanged := True;
          Len := Length(ChatText);
          while (CursorPosition < Len) do
          begin
            Inc(CursorPosition);
            if (CursorPosition = Len) then
              break;
            if (ChatText[CursorPosition] = ' ')
              and (ChatText[CursorPosition + 1] <> ' ') then
              break;
          end;
        end;

        SDLK_LEFT: begin
          ChatChanged := True;
          while (CursorPosition > 1) do
          begin
            Dec(CursorPosition);
            if (CursorPosition = 0) then
              break;
            if (ChatText[CursorPosition] = ' ')
              and (ChatText[CursorPosition + 1] <> ' ') then
              break;
          end;
        end;
      else
        Result := False;
      end;
    end
    else if KeyMods = KM_NONE then
    begin
      Result := True;

      case KeyCode of
        SDLK_ESCAPE: begin
          ClearChatText;
        end;

        SDLK_BACKSPACE:
        begin
          ChatChanged := True;
          if (CursorPosition > 1) or (Length(ChatText) = 1) then
          begin
            CurrentTabCompletePlayer := 0;
            Delete(ChatText, CursorPosition, 1);
            Dec(CursorPosition);
            if Length(ChatText) = 0 then
            begin
              ClearChatText;
            end;
          end;
        end;

        SDLK_DELETE:
        begin
          ChatChanged := True;
          if Length(ChatText) > CursorPosition then
          begin
            Delete(ChatText, CursorPosition + 1, 1);
            CurrentTabCompletePlayer := 0;
          end;
        end;

        SDLK_HOME: begin
          ChatChanged := True;
          CursorPosition := 1;
        end;

        SDLK_END: begin
          ChatChanged := True;
          CursorPosition := Length(ChatText);
        end;

        SDLK_RIGHT: begin
          ChatChanged := True;
          if Length(ChatText) > CursorPosition then
            Inc(CursorPosition);
        end;

        SDLK_LEFT: begin
          ChatChanged := True;
          if CursorPosition > 1 then
            Dec(CursorPosition);
        end;

        SDLK_TAB:
          TabComplete;

        SDLK_RETURN, SDLK_KP_ENTER:
        begin
          if ChatText[1] = '/' then
          begin
              ChatType := MSGTYPE_CMD;
              ConsoleStr := Copy(String(ChatText), 2, Length(ChatText));
              if ParseInput(ConsoleStr) then
              begin
                LastChatType := ChatType;
                LastChatText := ChatText;
                ClearChatText;
                Exit;
              end;
          end;
          if MySprite > 0 then
          begin
            if VoteKickReasonType then
            begin
              if Length(ChatText) > 3 then
              begin
                ClientVoteKick(KickMenuIndex, False, String(ChatText));
                VoteKickReasonType := False;
              end;
            end
            else
            begin
              ClientSendStringMessage(Copy(ChatText, 2, Length(ChatText)), ChatType);
            end;
          end;

          LastChatType := ChatType;
          LastChatText := ChatText;
          ClearChatText;
        end;
      else
        Result := False;
      end;
    end;
  end;
end;

function MenuKeyDown(KeyMods: Byte; KeyCode: TSDL_ScanCode): Boolean;
begin
  Result := False;

  if (KeyMods = KM_NONE) and (KeyCode = SDL_SCANCODE_ESCAPE) then
  begin
    Result := True;

    if ShowRadioMenu then
    begin
      ShowRadioMenu := False;
      RMenuState := '  ';
    end
    else if KickMenu.Active or MapMenu.Active then
      GameMenuShow(EscMenu)
    else
      GameMenuShow(EscMenu, not EscMenu.Active);
  end
  else if (KeyCode >= SDL_SCANCODE_1) and (KeyCode <= SDL_SCANCODE_0) then
  begin
    if TeamMenu.Active then
    begin
      if KeyMods = KM_NONE then
        Result := GameMenuAction(TeamMenu, ((KeyCode - SDL_SCANCODE_1) + 1) mod 10);
    end
    else if EscMenu.Active then
    begin
      if KeyMods = KM_NONE then
        Result := GameMenuAction(EscMenu, KeyCode - SDL_SCANCODE_1);
    end
    else if LimboMenu.Active then
    begin
      case KeyMods of
        KM_NONE: Result := GameMenuAction(LimboMenu, KeyCode - SDL_SCANCODE_1);
        KM_CTRL: Result := GameMenuAction(LimboMenu, KeyCode - SDL_SCANCODE_1 + 10);
      end;
    end;
  end;
end;

function KeyDown(var KeyEvent: TSDL_KeyboardEvent): Boolean;
var
  i: Integer;
  KeyMods: Byte;
  KeyCode: TSDL_ScanCode;
  Bind: PBind;
  Action: TAction;
  PriCount, SecCount, PriNum, SecNum: Integer;
begin
  Result := True;
  KeyCode := KeyEvent.keysym.scancode;

  KeyMods :=
    (Ord(0 <> (KeyEvent.keysym._mod and KMOD_ALT)) shl 0) or
    (Ord(0 <> (KeyEvent.keysym._mod and KMOD_CTRL)) shl 1) or
    (Ord(0 <> (KeyEvent.keysym._mod and KMOD_SHIFT)) shl 2);
  if ShouldRenderFrames and ChatKeyDown(KeyMods, KeyEvent.keysym.sym) then
    Exit;

  if KeyEvent._repeat <> 0 then
  begin
    Result := False;
    Exit;
  end;

  if ShouldRenderFrames and MenuKeyDown(KeyMods, KeyCode) then
    Exit;

  // other hard coded key bindings

  if KeyMods = KM_NONE then case KeyCode of
    SDL_SCANCODE_ESCAPE: begin
      if not ShouldRenderFrames then
        GameLoopRun := False;
    end;
    SDL_SCANCODE_PAGEDOWN: begin
      if FragsMenuShow then
        Inc(FragsScrollLev, Ord(FragsScrollLev < FragsScrollMax));
    end;

    SDL_SCANCODE_PAGEUP: begin
      if FragsMenuShow then
        Dec(FragsScrollLev, Ord(FragsScrollLev > 0));
    end;

    SDL_SCANCODE_F11: begin
      Result := VoteActive;
      VoteActive := False;
    end;

    SDL_SCANCODE_F12:
    begin
      Result := VoteActive;

      if VoteActive then
      begin
        VoteActive := False;

        if VoteType = VOTE_MAP then
        begin
          ClientSendStringMessage('votemap ' + WideString(VoteTarget), MSGTYPE_CMD);
          MainConsole.Console(WideFormat(_('You have voted on %s'), [VoteTarget]), VOTE_MESSAGE_COLOR);
        end
        else if VoteType = VOTE_KICK then
        begin
          i := StrToInt(VoteTarget);
          ClientVoteKick(i, True, '');
          MainConsole.Console(WideFormat(_('You have voted to kick %s'), [Sprite[i].Player.Name]), VOTE_MESSAGE_COLOR);
        end;
      end;
    end;

    SDL_SCANCODE_F9: begin
      SDL_MinimizeWindow(GameWindow);
      Result := True;
    end;

    SDL_SCANCODE_F8: begin
      Result := False;

      if DemoPlayer.Active then
      begin
        Result := True;
        demo_speed.SetValue(iif(GoalTicks = DEFAULT_GOALTICKS, 8.0, 1.0))
      end;
    end;

    SDL_SCANCODE_F10: begin
      Result := False;

      if DemoPlayer.Active then
      begin
        Result := True;
        if (MapChangeCounter < 0) or (MapChangeCounter > 99999999) then
        begin
          if MapChangeCounter < 0 then
            MapChangeCounter := 999999999
          else
            MapChangeCounter := -60;
        end;
      end;
    end;

    SDL_SCANCODE_1, SDL_SCANCODE_2, SDL_SCANCODE_3:
    begin
      Result := False;

      if (ChatText = '') and (sv_radio.Value) and ShowRadioMenu then
      begin
        Result := True;
        i := Ord(RMenuState[0] <> ' ');

        case KeyCode of
          SDL_SCANCODE_1: RMenuState[i] := '1';
          SDL_SCANCODE_2: RMenuState[i] := '2';
          SDL_SCANCODE_3: RMenuState[i] := '3';
        end;

        if i = 1 then
        begin
          ChatText := WideString(
            RadioMenu.Values['Menu1' +
              choose(StrToInt(RMenuState[0]) - 1, ['EFC', 'FFC', 'ES'])
            ] + ' ' +
            RadioMenu.Values['Menu2' +
              choose(StrToInt(RMenuState[0]) - 1, ['EFC', 'FFC', 'ES']) +
              choose(StrToInt(RMenuState[1]) - 1, ['U', 'M', 'D'])
            ]);

          ChatText := '*' + RMenuState[0] + RMenuState[1] + ChatText;
          ClientSendStringMessage(ChatText, MSGTYPE_RADIO);
          ChatText := '';
          // RadioCooldown := 3;
          ShowRadioMenu := False;
          RMenuState := '  ';
        end;
      end;
    end;

    else
      Result := False;
  end
  else if KeyMods = KM_ALT then case KeyCode of
    SDL_SCANCODE_F4:
      ExitToMenu;

    SDL_SCANCODE_F9:
      ExitToMenu;
    else
      Result := False;
  end
  else if (KeyMods = KM_CTRL) or (KeyMods = KM_SHIFT) then
    Result := False;

  if Result then
    Exit;

  // bindings
  Bind := FindKeyBind(KeyMods, KeyCode);
  Result := Bind <> nil;

  if not Result then
    Exit;

  Action := Bind.Action;

  if Action = TAction.SniperLine then
  begin
    if sv_sniperline.Value then
      ui_sniperline.SetValue(not ui_sniperline.Value)
    else
      MainConsole.Console(_('Sniper Line disabled on this server'), WARNING_MESSAGE_COLOR);
  end
  else if Action = TAction.StatsMenu then
  begin
    if not EscMenu.Active then
    begin
      StatsMenuShow := not StatsMenuShow;
      if StatsMenuShow then
        FragsMenuShow := False;
    end;
  end
  else if Action = TAction.GameStats then
  begin
    ConInfoShow := not ConInfoShow;
  end
  else if Action = TAction.MiniMap then
  begin
    MiniMapShow := not MiniMapShow;
  end
  else if Action = TAction.PlayerName then
  begin
    PlayerNamesShow := not PlayerNamesShow;
  end
  else if Action = TAction.FragsList then
  begin
    if not EscMenu.Active then
    begin
      FragsScrollLev := 0;
      FragsMenuShow := not FragsMenuShow;
      if FragsMenuShow then
        StatsMenuShow := False;
    end;
  end
  else if Action = TAction.Radio then
  begin
    if (ChatText = '') and (sv_radio.Value) and (MySprite > 0) and
      (Sprite[MySprite].IsNotSpectator) then
    begin
      ShowRadioMenu := not ShowRadioMenu;
      RMenuState := '  ';
    end;
  end
  else if Action = TAction.RecordDemo then
  begin
    if not DemoPlayer.Active then
    begin
      if demo_autorecord.Value then
      begin
        DemoRecorder.StopRecord;
        DemoRecorder.StartRecord(UserDirectory + 'demos/'  +
            FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now()) + Map.Name + '.sdm');
      end
      else if DemoRecorder.Active then
        DemoRecorder.StopRecord
      else
        DemoRecorder.StartRecord(UserDirectory + 'demos/' +
          FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now()) + Map.Name + '.sdm');
    end;
  end
  else if (Action = TAction.VolumeUp) or (Action = TAction.VolumeDown) then
  begin
    if (ChatText = '') and not EscMenu.Active then
    begin
      i := snd_volume.Value;

      if Action = TAction.VolumeUp then
        snd_volume.SetValue(Min(snd_volume.Value + 10, 100));

      if Action = TAction.VolumeDown then
        snd_volume.SetValue(Max(snd_volume.Value - 10, 0));

      if snd_volume.Value <> i then
      begin
        VolumeInternal := ScaleVolumeSetting(snd_volume.Value);
        SetVolume(-1, VolumeInternal);
        Mainconsole.Console(WideString('Volume: ' + IntToStr(snd_volume.Value) + '%'),
          MUSIC_MESSAGE_COLOR);
      end;
    end;
  end
  else if (Action = TAction.MouseSensitivityUp) or (Action = TAction.MouseSensitivityDown) then
  begin
    if (ChatText = '') and not EscMenu.Active then
    begin
      i := iif(Action = TAction.MouseSensitivityDown, -5, 5);
      cl_sensitivity.SetValue(Max(0, i + Floor(100 * cl_sensitivity.Value)) / 100);
      Mainconsole.Console(_('Sensitivity:') +
        WideString(' ' + IntToStr(Floor(100 * cl_sensitivity.Value)) + '%'),
        MUSIC_MESSAGE_COLOR);
    end;
  end
  else if Action = TAction.Cmd then
  begin
    if ChatText = '' then
    begin
      ChatType := MSGTYPE_CMD;
      VoteKickReasonType := False;
      StartChat;
    end;
  end
  else if Action = TAction.Chat then
  begin
    if ChatText = '' then
    begin
      // force spectator chat to teamchat in survival mode when Round hasn't ended
      if (sv_survivalmode.Value) and Sprite[MySprite].IsSpectator() and
         not SurvivalEndRound and (sv_survivalmode_antispy.Value) then
        ChatType := MSGTYPE_TEAM
      else
        ChatType := MSGTYPE_PUB;

      StartChat;
    end;
  end
  else if Action = TAction.TeamChat then
  begin
    if (ChatText = '') and (MySprite > 0) and
      (Sprite[MySprite].IsSpectator() or IsTeamGame()) then
    begin
      ChatType := MSGTYPE_TEAM;
      StartChat;
    end;
  end
  else if Action = TAction.Snap then
  begin
    if (cl_actionsnap.Value) and (ScreenCounter < 255) and ActionSnapTaken then
    begin
      ShowScreen := not ShowScreen;

      if ShowScreen = False then
        ScreenCounter := 255
      else
        PlaySound(SFX_SNAPSHOT);
    end
    else
    begin
      ScreenCounter := 255;
      ShowScreen := False;
    end;
  end
  else if Action = TAction.Weapons then
  begin
    if (ChatText = '') and (MySprite > 0) and not EscMenu.Active and
      not Sprite[MySprite].IsSpectator then
    begin
      if Sprite[MySprite].DeadMeat then
      begin
        GameMenuShow(LimboMenu, not LimboMenu.Active);
        LimboLock := not LimboMenu.Active;
        MainConsole.Console(iif(LimboLock, _('Weapons menu disabled'), _('Weapons menu active')), GAME_MESSAGE_COLOR);
      end
      else
      begin
        PriCount := 0;
        SecCount := 0;
        PriNum := Sprite[MySprite].Weapon.Num;
        SecNum := Sprite[MySprite].SecondaryWeapon.Num;

        for i := 1 to PRIMARY_WEAPONS do
          Inc(PriCount, WeaponActive[i]);

        for i := 1 to SECONDARY_WEAPONS do
          Inc(SecCount, WeaponActive[i + PRIMARY_WEAPONS]);

        if not LimboMenu.Active or
          (((PriNum <> Guns[NOWEAPON].Num) or (PriCount = 0)) and
          ((SecNum <> Guns[NOWEAPON].Num) or (SecCount = 0))) then
        begin
          GameMenuShow(LimboMenu, False);
          LimboLock := not LimboLock;
          MainConsole.Console(iif(LimboLock, _('Weapons menu disabled'), _('Weapons menu active')), GAME_MESSAGE_COLOR);
        end;
      end;
    end;
  end
  else if action = TAction.Bind then
  begin
    if (ChatTimeCounter = 0) and (Bind.Command <> '') then
    begin
      if (ChatText = '') and not EscMenu.Active then
      begin
        if not ParseInput(String(Bind.Command)) then
          ClientSendStringMessage(Bind.Command, MSGTYPE_CMD);
      end;
    end;
  end
  else if action = TAction.VoiceChat then
  begin
    {$IFDEF STEAM}
    if not VoiceSpeakingNow then
    begin
      SteamAPI.User.StartVoiceRecording();
      VoiceSpeakingNow := True;
    end;
    {$ENDIF}
  end
  else
  begin
    Result := False;
  end;
end;

function KeyUp(var KeyEvent: TSDL_KeyboardEvent): Boolean;
var
  KeyMods: Byte;
  KeyCode: TSDL_ScanCode;
  Bind: PBind;
  Action: TAction;
begin
  Result := True;
  KeyCode := KeyEvent.keysym.scancode;

  KeyMods :=
    (Ord(0 <> (KeyEvent.keysym._mod and KMOD_ALT)) shl 0) or
    (Ord(0 <> (KeyEvent.keysym._mod and KMOD_CTRL)) shl 1) or
    (Ord(0 <> (KeyEvent.keysym._mod and KMOD_SHIFT)) shl 2);

  if KeyEvent._repeat <> 0 then
  begin
    Result := False;
    Exit;
  end;

  // bindings
  Bind := FindKeyBind(KeyMods, KeyCode);
  Result := Bind <> nil;

  if not Result then
    Exit;

  Action := Bind.Action;

  if Action = TAction.VoiceChat then
  begin
    {$IFDEF STEAM}
    if VoiceSpeakingNow then
    begin
      SteamAPI.User.StopVoiceRecording();
      VoiceSpeakingNow := False;
    end;
    {$ENDIF}
  end else
  begin
    Result := False;
  end;
end;

procedure GameInput;
var
  Event: TSDL_Event;
  Str: WideString;
  ChatEnabled: Boolean;
begin
  Event := Default(TSDL_Event);
  ChatEnabled := Length(ChatText) > 0;

  while SDL_PollEvent(@Event) = 1 do
  begin
    case Event.type_ of
      SDL_QUITEV: begin
        ClientDisconnect;
        Halt(0);
      end;

      SDL_KEYDOWN: begin
        if not KeyDown(Event.key) then
          KeyStatus[Event.key.keysym.scancode] := True;
      end;

      SDL_KEYUP: begin
        KeyStatus[Event.key.keysym.scancode] := False;
        KeyUp(Event.key);
      end;

      SDL_MOUSEBUTTONDOWN: begin
        if not GameMenuClick() then
          KeyStatus[Event.button.button + 300] := True;
      end;

      SDL_MOUSEBUTTONUP:
        KeyStatus[Event.button.button + 300] := False;

      SDL_TEXTINPUT:
      begin
        if ChatEnabled Then
        begin
          Str := WideString(UTF8String(RawByteString(PChar(@Event.text.text[0]))));
          Str := FilterChatText(Str);

          if (ChatText = '/') and (Str = '/') and (Length(LastChatText) > 1) then
          begin
            ChatChanged := True;
            CurrentTabCompletePlayer := 0;
            ChatType := LastChatType;
            ChatText := LastChatText;
            CursorPosition := Length(ChatText);
          end
          else if Length(ChatText) > 0 then
          begin
            if Length(ChatText) < iif(VoteKickReasonType, REASON_CHARS - 1, MAXCHATTEXT) then
            begin
              ChatChanged := True;
              CurrentTabCompletePlayer := 0;
              Insert(Str, ChatText, CursorPosition + 1);
              Inc(CursorPosition, Length(Str));
            end;
          end;
        end;
      end;

      SDL_MOUSEMOTION: begin
        if 0 <> (SDL_GetWindowFlags(GameWindow) and SDL_WINDOW_INPUT_FOCUS) then
        begin
          mx := Max(0, Min(GameWidth, mx + Event.motion.xrel * cl_sensitivity.Value));
          my := Max(0, Min(GameHeight, my + Event.motion.yrel * cl_sensitivity.Value));

          GameMenuMouseMove();
        end;
      end;
    end;
  end;
end;

end.

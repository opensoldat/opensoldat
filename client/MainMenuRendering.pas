unit MainMenuRendering;

interface

uses
  Gfx, SDL2, GameRendering, Command, StrUtils;

type
  TButtonFunction = procedure();

  PMainButton = ^TMainButton;
  TMainButton = record
    Active: Boolean;
    x, y, w, h: Integer;
    Caption: WideString;
    ButtonFunction: TButtonFunction;
  end;

  PMainTextLabel = ^TMainTextLabel;
  TMainTextLabel = record
    x, y: Integer;
    Caption: WideString;
  end;

  PMainTextInput = ^TMainTextInput;
  TMainTextInput = record
    Active: Boolean;
    x, y, w, h: Integer;
    MaxLength: Integer;
    Content: WideString;
  end;

function InitMainMenuGraphics: Boolean;
procedure StartMainMenuLoop;
procedure ShowMainMenu;
procedure MainMenuLoop(TimeElapsed: Extended);
procedure MainMenuInput;
//function KeyDown(var KeyEvent: TSDL_KeyboardEvent);
procedure ExitButtonClick;
procedure JoinButtonClick;


implementation

uses
  Client,
  Math, SysUtils, Classes,
  Constants, Weapons, Vector,
  InterfaceGraphics, ClientGame, GameStrings, GostekGraphics, Input,
  PhysFS, Cvar, MapGraphics, TraceLog {$IFDEF TESTING},  Version{$ENDIF};

const
  ButtonColorRGBA: DWORD = $3C8E37;
  BackgroundColorPrimaryRGBA: DWORD = $313D4F;
  BackgroundColorSecondaryRGBA: DWORD = $2D3444;
  TextInputBackgroundColorRGBA: DWORD = $3C4B60;
  TextColorRGBA: DWORD = $E4EAF6;

var
  Initialized: Boolean;
  MainMenuLoopRun: Boolean;
  //Renderer: PSDL_Renderer;
  Buttons: array of TMainButton;
  TextLabels: array of TMainTextLabel;
  TextInputs: array of TMainTextInput;
  ActiveTextInput: PMainTextInput;
  RenderTarget: TGfxTexture;

procedure InitButton(Button: Integer; Caption: WideString;
  x, y, w, h: Integer; ButtonFunction: TButtonFunction; Active: Boolean = True);
begin
  Buttons[Button].Active := Active;
  Buttons[Button].x := x;
  Buttons[Button].y := y;
  Buttons[Button].w := w;
  Buttons[Button].h := h;
  Buttons[Button].Caption := Caption;
  Buttons[Button].ButtonFunction := ButtonFunction;
end;

procedure InitTextLabel(TextLabel: Integer; Caption: WideString;
  x, y: Integer);
begin
  TextLabels[TextLabel].x := x;
  TextLabels[TextLabel].y := y;
  TextLabels[TextLabel].Caption := Caption;
end;

procedure InitTextInput(TextInput: Integer;
  x, y, MaxLength: Integer; Content: WideString = '');
var
  rc: TGfxRect;
begin
  TextInputs[TextInput].Active := False;
  TextInputs[TextInput].x := x;
  TextInputs[TextInput].y := y;
  TextInputs[TextInput].MaxLength := MaxLength;
  TextInputs[TextInput].Content := Content;

  SetFontStyle(FONT_SMALL);
  rc := GfxTextMetrics(DupeString('M', TextInputs[TextInput].MaxLength));
  TextInputs[TextInput].w := Trunc(RectWidth(rc)) + 4;
  TextInputs[TextInput].h := Trunc(RectHeight(rc)) + 15;
end;

function InitMainMenuGraphics: Boolean;
begin
  Result := True;

  if Initialized then
  begin
    Exit;
  end;

  SetLength(Buttons, 2);
  InitButton(0, _('Join Game'), Trunc(WindowWidth / 2) - 150, 300, 300, 50, JoinButtonClick);
  InitButton(1, _('Exit'), Trunc(WindowWidth / 2) - 150, 400, 300, 50, ExitButtonClick);

  SetLength(TextLabels, 2);
  InitTextLabel(0, _('Player Name'), Trunc(WindowWidth / 2) - 160, 110);
  InitTextLabel(1, _('Server IP'), Trunc(WindowWidth / 2) - 160, 210);

  SetLength(TextInputs, 2);
  InitTextInput(0, Trunc(WindowWidth / 2) - 150, 150, 24, cl_player_name.Value);
  InitTextInput(1, Trunc(WindowWidth / 2) - 150, 250, 15);
  ActiveTextInput := nil;

  Initialized := True;
end;

procedure ControlClick();
var
  i: Integer;
begin
  for i := Low(Buttons) to High(Buttons) do
  begin
    if (Buttons[i].x < mx) and (Buttons[i].x + Buttons[i].w > mx) and (Buttons[i].y < my) and (Buttons[i].y + Buttons[i].h > my) then
    begin
      Buttons[i].ButtonFunction();
    end;
  end;

  ActiveTextInput := nil;
  SDL_StopTextInput;
  for i := Low(TextInputs) to High(TextInputs) do
  begin
    TextInputs[i].Active := (TextInputs[i].x < mx) and (TextInputs[i].x + TextInputs[i].w > mx) and
                            (TextInputs[i].y < my) and (TextInputs[i].y + TextInputs[i].h > my);
    if (TextInputs[i].Active) then
    begin
      ActiveTextInput := @TextInputs[i];
      SDL_StartTextInput;
    end;
  end;
end;

procedure KeyDown(var KeyEvent: TSDL_KeyboardEvent);
var
  KeyCode: TSDL_ScanCode;
begin
  KeyCode := KeyEvent.keysym.sym;
  case KeyCode of
    SDLK_BACKSPACE: begin
      if (ActiveTextInput <> nil) then
      begin
        Delete(ActiveTextInput^.Content, Length(ActiveTextInput^.Content), 1);
        CursorPosition := Length(ActiveTextInput^.Content);
      end;
    end;
  end;
end;

procedure ExitButtonClick;
begin
  MainMenuLoopRun := False;
  ClientLoopRun := False;
end;

procedure JoinButtonClick;
begin
  cl_player_name.SetValue(TextInputs[0].Content);
  MainMenuLoopRun := False;
  ParseInput('join ' + TextInputs[1].Content);
end;

procedure MainMenuInput;
var
  Event: TSDL_Event;
  Str: WideString;
begin
  Event := Default(TSDL_Event);

  while SDL_PollEvent(@Event) = 1 do
  begin
    case Event.type_ of
      SDL_QUITEV: begin
        Halt(0);
      end;

      SDL_KEYDOWN: begin
        KeyDown(Event.key);
      end;

      SDL_MOUSEBUTTONDOWN: begin
        ControlClick();
      end;

      SDL_TEXTINPUT: begin
        if (ActiveTextInput <> nil) then
        begin
          Str := WideString(UTF8String(RawByteString(PChar(@Event.text.text[0]))));
          //Str := FilterChatText(Str);
          CursorPosition := Length(ActiveTextInput^.Content);
          if (CursorPosition < ActiveTextInput^.MaxLength) then
          begin
            Insert(Str, ActiveTextInput^.Content, CursorPosition + 1);
            CursorPosition := Length(ActiveTextInput^.Content);
          end;
        end;
      end;

      SDL_MOUSEMOTION: begin
        if 0 <> (SDL_GetWindowFlags(GameWindow) and SDL_WINDOW_INPUT_FOCUS) then
        begin
          mx := Max(0, Min(WindowWidth, mx + Event.motion.xrel * cl_sensitivity.Value));
          my := Max(0, Min(WindowHeight, my + Event.motion.yrel * cl_sensitivity.Value));
        end;
      end;
    end;
  end;
end;

procedure RenderButtons;
var
  i: Integer;
  rc: TGfxRect;
  ButtonColor: TGfxColor;
  TextColor: TGfxColor;
begin
  ButtonColor := RGBA(ButtonColorRGBA);
  TextColor := RGBA(TextColorRGBA);

  GfxBegin();
  for i := Low(Buttons) to High(Buttons) do
  begin
    GfxDrawQuad(RenderTarget,
      GfxVertex(Buttons[i].x, Buttons[i].y, 0, 1, ButtonColor),
      GfxVertex(Buttons[i].x + Buttons[i].w, Buttons[i].y, 1, 1, ButtonColor),
      GfxVertex(Buttons[i].x + Buttons[i].w, Buttons[i].y + Buttons[i].h, 1, 0, ButtonColor),
      GfxVertex(Buttons[i].x, Buttons[i].y + Buttons[i].h, 0, 0, ButtonColor)
    );
  end;
  GfxEnd();

  SetFontStyle(FONT_MENU);
  GfxTextColor(TextColor);
  GfxTextShadow(1, 1, RGBA(0));
  GfxTextVerticalAlign(GFX_BASELINE);
  GfxTextPixelRatio(Vector2(1, 1));
  GfxBegin();

  for i := Low(Buttons) to High(Buttons) do
  begin
    rc := GfxTextMetrics(Buttons[i].Caption);
    // This doesn't actually center text in the button, but pretty close.
    GfxDrawText(Buttons[i].x + (Buttons[i].w - RectWidth(rc)) / 2, Buttons[i].y + (Buttons[i].h + RectHeight(rc)) / 2);
  end;
  GfxEnd();
end;

procedure RenderTextLabels;
var
  i: Integer;
  rc: TGfxRect;
  BackgroundColorSecondary: TGfxColor;
  TextColor: TGfxColor;
  BackgroundWidth, BackgroundHeight: Integer;
begin
  BackgroundColorSecondary := RGBA(BackgroundColorSecondaryRGBA);
  TextColor := RGBA(TextColorRGBA);

  SetFontStyle(FONT_SMALL);
  GfxTextColor(TextColor);
  GfxTextShadow(1, 1, RGBA(0));
  GfxTextVerticalAlign(GFX_TOP);
  GfxTextPixelRatio(Vector2(1, 1));
  GfxBegin();

  for i := Low(TextLabels) to High(TextLabels) do
  begin
    rc := GfxTextMetrics(TextLabels[i].Caption);
    BackgroundWidth := Trunc(RectWidth(rc)) + 4;
    BackgroundHeight := Trunc(RectHeight(rc)) + 4;

    GfxDrawQuad(RenderTarget,
      GfxVertex(TextLabels[i].x, TextLabels[i].y, 0, 1, BackgroundColorSecondary),
      GfxVertex(TextLabels[i].x + BackgroundWidth, TextLabels[i].y, 1, 1, BackgroundColorSecondary),
      GfxVertex(TextLabels[i].x + BackgroundWidth, TextLabels[i].y + BackgroundHeight, 1, 0, BackgroundColorSecondary),
      GfxVertex(TextLabels[i].x, TextLabels[i].y + BackgroundHeight, 0, 0, BackgroundColorSecondary)
    );
    // This doesn't actually center text in the label, but pretty close.
    GfxDrawText(TextLabels[i].x + 2, TextLabels[i].y);
  end;
  GfxEnd();
end;

procedure RenderTextInputs(TimeElapsed: Extended);
var
  i: Integer;
  rc: TGfxRect;
  BackgroundColorSecondary: TGfxColor;
  TextColor: TGfxColor;
  x, y, w, h: Single;
begin
  BackgroundColorSecondary := RGBA(TextInputBackgroundColorRGBA);
  TextColor := RGBA(TextColorRGBA);

  SetFontStyle(FONT_SMALL);
  GfxTextColor(TextColor);
  GfxTextShadow(1, 1, RGBA(0));
  GfxTextVerticalAlign(GFX_TOP);
  GfxTextPixelRatio(Vector2(1, 1));
  GfxBegin();
  for i := Low(TextInputs) to High(TextInputs) do
  begin
    GfxDrawQuad(RenderTarget,
      GfxVertex(TextInputs[i].x, TextInputs[i].y, 0, 1, BackgroundColorSecondary),
      GfxVertex(TextInputs[i].x + TextInputs[i].w, TextInputs[i].y, 1, 1, BackgroundColorSecondary),
      GfxVertex(TextInputs[i].x + TextInputs[i].w, TextInputs[i].y + TextInputs[i].h, 1, 0, BackgroundColorSecondary),
      GfxVertex(TextInputs[i].x, TextInputs[i].y + TextInputs[i].h, 0, 0, BackgroundColorSecondary)
    );
    rc := GfxTextMetrics(TextInputs[i].Content);
    GfxDrawText(TextInputs[i].x + 4, TextInputs[i].y);
  end;
  GfxEnd();

  if (ActiveTextInput <> nil) then
  begin
    // cursor blinking
    if (TimeElapsed - Floor(TimeElapsed)) <= 0.5 then
    begin

      // for some reason GfxTextMetrics ignores the last trailing space while calculating rectangle width...
      x := ActiveTextInput^.x + RectWidth(GfxTextMetrics(ActiveTextInput^.Content)) + 5;
      y := ActiveTextInput^.y + 3;
      w := 2;
      h := ActiveTextInput^.h - 6;

      //x := PixelAlignX(5 + x) + 2 * PixelSize.x;
      //y := PixelAlignY(420 * _iscala.y - RectHeight(rc));
      //w := PixelSize.x;
      //h := PixelAlignY(1.4 * RectHeight(rc));

      // drawing cursor
      GfxEnd();
      GfxDrawQuad(nil,
        GfxVertex(x + 0, y + 0, 0, 0, RGBA(255, 230, 170)),
        GfxVertex(x + w, y + 0, 0, 0, RGBA(255, 230, 170)),
        GfxVertex(x + w, y + h, 0, 0, RGBA(255, 230, 170)),
        GfxVertex(x + 0, y + h, 0, 0, RGBA(255, 230, 170))
      );
      GfxEnd();
    end;
  end;
end;

procedure MainMenuLoop(TimeElapsed: Extended);
var
  BackgroundColorPrimary: TGfxColor;
  T: ^TGfxSpriteArray;
begin
  BackgroundColorPrimary := RGBA(BackgroundColorPrimaryRGBA);
  T := @Textures;

  GfxTarget(nil);
  GfxViewport(0, 0, WindowWidth, WindowHeight);
  GfxTransform(GfxMat3Ortho(0, WindowWidth, 0, WindowHeight));
  GfxClear(BackgroundColorPrimary.r, BackgroundColorPrimary.g, BackgroundColorPrimary.b, BackgroundColorPrimary.a);

  RenderButtons;
  RenderTextLabels;
  RenderTextInputs(TimeElapsed);

  if (T <> nil) then
  begin
    GfxBegin();
    GfxDrawSprite(T^[GFX_INTERFACE_MENUCURSOR], mx, my, RGBA($FFFFFF));
    GfxEnd();
  end;

  GfxPresent(True);

end;

procedure StartMainMenuLoop;
var
  i: Integer;
  InputParse: TStringList;
  StartTime, Frequency: Extended;
begin
  for i:=0 To DeferredCommands.Count-1 do
  begin
    InputParse := TStringList.Create;
    InputParse.Delimiter := ' ';
    InputParse.DelimitedText := TrimLeft(DeferredCommands[i]);
    if (InputParse[0] = 'join') or (InputParse[0] = 'joinurl') or (InputParse[0] = 'connect') then
    begin
      Debug('Already joining game, skipping main menu');
      InputParse.Free;
      Exit
    end;

    InputParse.Free;

  end;
  StartTime := SDL_GetPerformanceCounter;
  Frequency := SDL_GetPerformanceFrequency;
  while MainMenuLoopRun do
  begin
    MainMenuInput();
    MainMenuLoop((SDL_GetPerformanceCounter - StartTime) / Frequency);
  end;
end;

procedure ShowMainMenu;
begin
  MainMenuLoopRun := True;

  InitMainMenuGraphics();
  DoTextureLoading(True);

  StartMainMenuLoop();
end;

end.

//from "sdl_messagebox.h"

  {**
   *  SDL_MessageBox flags. If supported will display warning icon, etc.
   *}
 
const
  SDL_MESSAGEBOX_ERROR        = $00000010;   {**< error dialog *}
  SDL_MESSAGEBOX_WARNING      = $00000020;   {**< warning dialog *}
  SDL_MESSAGEBOX_INFORMATION  = $00000040;   {**< informational dialog *}

type
  TSDL_MessageBoxFlags = Byte;

  {**
   *  Flags for SDL_MessageBoxButtonData.
   *}
const
  SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = $00000001;  {**< Marks the default button when return is hit *}
  SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = $00000002;   {**< Marks the default button when escape is hit *}
type
  TSDL_MessageBoxButtonFlags = Byte;

  {**
   *   Individual button data.
   *}
type
  PSDL_MessageBoxButtonData = ^TSDL_MessageBoxButtonData;
  TSDL_MessageBoxButtonData = record
    flags: UInt32;     {**< ::SDL_MessageBoxButtonFlags *}
    buttonid: Integer; {**< User defined button id (value returned via SDL_ShowMessageBox) *}
    text: PAnsiChar;   {**< The UTF-8 button text *}
  end;

  {**
   *  RGB value used in a message box color scheme
   *}
type
  PSDL_MessageBoxColor = ^TSDL_MessageBoxColor;
  TSDL_MessageBoxColor = record
    r, g, b: UInt8;
  end;

  PSDL_MessageBoxColorType = ^TSDL_MessageBoxColorType;
  TSDL_MessageBoxColorType = (SDL_MESSAGEBOX_COLOR_BACKGROUND,
                              SDL_MESSAGEBOX_COLOR_TEXT,
                              SDL_MESSAGEBOX_COLOR_BUTTON_BORDER,
                              SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND,
                              SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED,
                              SDL_MESSAGEBOX_COLOR_MAX);

  {**
   *  A set of colors to use for message box dialogs
   *}
type
  PSDL_MessageBoxColorScheme = ^TSDL_MessageBoxColorScheme;
  TSDL_MessageBoxColorScheme = record
    //colors: array[0..SDL_MESSAGEBOX_COLOR_MAX-1] of TSDL_MessageBoxColor;
    colors: array[0..4] of TSDL_MessageBoxColor;   //right?!
  end;

  {**
   *   MessageBox structure containing title, text, window, etc.
   *}
type
  PSDL_MessageBoxData = ^TSDL_MessageBoxData;
  TSDL_MessageBoxData = record
    flags: UInt32;             {**< SDL_MessageBoxFlags *}
    window: PSDL_Window;       {**< Parent window, can be NULL *}
    title: PAnsiChar;          {**< UTF-8 title *}
    _message: PAnsiChar;       {**< UTF-8 message text *}

    numbuttons: Integer;
    buttons: PSDL_MessageBoxButtonData;

    colorScheme: PSDL_MessageBoxColorScheme;   {**< SDL_MessageBoxColorScheme, can be NULL to use system settings *}
  end;

  {**
   *   Create a modal message box.
   *
   *   messageboxdata The SDL_MessageBoxData structure with title, text, etc.
   *   buttonid The pointer to which user id of hit button should be copied.
   *
   *   -1 on error, otherwise 0 and buttonid contains user id of button
   *   hit or -1 if dialog was closed.
   *
   *   This function should be called on the thread that created the parent
   *   window, or on the main thread if the messagebox has no parent.  It will
   *   block execution of that thread until the user clicks a button or
   *   closes the messagebox.
   *}
function SDL_ShowMessageBox(messageboxdata: PSDL_MessageBoxData; buttonid: PInt): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ShowMessageBox' {$ENDIF} {$ENDIF};

  {**
   *   Create a simple modal message box
   *
   *   flags    SDL_MessageBoxFlags
   *   title    UTF-8 title text
   *   message  UTF-8 message text
   *   window   The parent window, or NULL for no parent
   *
   *   0 on success, -1 on error
   *
   *   SDL_ShowMessageBox
   *}
function SDL_ShowSimpleMessageBox(flags: UInt32; title: PAnsiChar; _message: PAnsiChar; window: PSDL_Window): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ShowSimpleMessageBox' {$ENDIF} {$ENDIF};


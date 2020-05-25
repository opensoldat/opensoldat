//from "sdl_keyboard.h"

type
  PKeyStateArr = ^TKeyStateArr;
  TKeyStateArr = array[0..65000] of UInt8;

  {**
   *  The SDL keysym structure, used in key events.
   *}
  PSDL_Keysym = ^TSDL_Keysym;
  TSDL_Keysym = record
    scancode: TSDL_ScanCode;      // SDL physical key code - see SDL_Scancode for details
    sym: TSDL_KeyCode;            // SDL virtual key code - see SDL_Keycode for details
    _mod: UInt16;                 // current key modifiers
    unicode: UInt32;              // (deprecated) use SDL_TextInputEvent instead
  end;

  {**
   *  Get the window which currently has keyboard focus.
   *}

  function SDL_GetKeyboardFocus: PSDL_Window cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetKeyboardFocus' {$ENDIF}{$ENDIF};

  {**
   *  Get a snapshot of the current state of the keyboard.
   *
   *  numkeys if non-nil, receives the length of the returned array.
   *  
   *  An array of key states. Indexes into this array are obtained by using SDL_Scancode values.
   *
   *}

  function SDL_GetKeyboardState(numkeys: PInt): PUInt8 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetKeyboardState' {$ENDIF}{$ENDIF};

  {**
   *  Get the current key modifier state for the keyboard.
   *}

  function SDL_GetModState: TSDL_KeyMod cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetModState' {$ENDIF}{$ENDIF};

  {**
   *  Set the current key modifier state for the keyboard.
   *  
   *  This does not change the keyboard state, only the key modifier flags.
   *}

  procedure SDL_SetModState(modstate: TSDL_KeyMod) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetModState' {$ENDIF}{$ENDIF};

  {**
   *  Get the key code corresponding to the given scancode according
   *         to the current keyboard layout.
   *
   *  See SDL_Keycode for details.
   *  
   *  SDL_GetKeyName()
   *}

  function SDL_GetKeyFromScancode(scancode: TSDL_ScanCode): TSDL_KeyCode cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetKeyFromScancode' {$ENDIF}{$ENDIF};

  {**
   *  Get the scancode corresponding to the given key code according to the
   *         current keyboard layout.
   *
   *  See SDL_Scancode for details.
   *
   *  SDL_GetScancodeName()
   *}

  function SDL_GetScancodeFromKey(key: TSDL_KeyCode): TSDL_ScanCode cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetScancodeFromKey' {$ENDIF}{$ENDIF};

  {**
   *  Get a human-readable name for a scancode.
   *
   *  A pointer to the name for the scancode.
   *
   *  If the scancode doesn't have a name, this function returns
   *  an empty string ("").
   *
   *}

  function SDL_GetScancodeName(scancode: TSDL_ScanCode): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetScancodeName' {$ENDIF}{$ENDIF};

  {**
   *  Get a scancode from a human-readable name
   *
   *  scancode, or SDL_SCANCODE_UNKNOWN if the name wasn't recognized
   *
   *  SDL_Scancode
   *}

  function SDL_GetScancodeFromName(const name: PAnsiChar): TSDL_ScanCode cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetScancodeFromName' {$ENDIF}{$ENDIF};

  {**
   *  Get a human-readable name for a key.
   *
   *  A pointer to a UTF-8 string that stays valid at least until the next
   *  call to this function. If you need it around any longer, you must
   *  copy it.  If the key doesn't have a name, this function returns an
   *  empty string ("").
   *  
   *  SDL_Key
   *}

  function SDL_GetKeyName(key: TSDL_KeyCode): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetKeyName' {$ENDIF}{$ENDIF};

  {**
   *  Get a key code from a human-readable name
   *
   *  key code, or SDLK_UNKNOWN if the name wasn't recognized
   *
   *  SDL_Keycode
   *}

  function SDL_GetKeyFromName(const name: PAnsiChar): TSDL_KeyCode cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetKeyFromName' {$ENDIF}{$ENDIF};

  {**
   *  Start accepting Unicode text input events.
   *  This function will show the on-screen keyboard if supported.
   *  
   *  SDL_StopTextInput()
   *  SDL_SetTextInputRect()
   *  SDL_HasScreenKeyboardSupport()
   *}

  procedure SDL_StartTextInput cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_StartTextInput' {$ENDIF}{$ENDIF};

  {**
   *  Return whether or not Unicode text input events are enabled.
   *
   *  SDL_StartTextInput()
   *  SDL_StopTextInput()
   *}

  function SDL_IsTextInputActive: TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_IsTextInputActive' {$ENDIF}{$ENDIF};

  {**
   *  Stop receiving any text input events.
   *  This function will hide the on-screen keyboard if supported.
   *  
   *  SDL_StartTextInput()
   *  SDL_HasScreenKeyboardSupport()
   *}

  procedure SDL_StopTextInput cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_StopTextInput' {$ENDIF}{$ENDIF};

  {**
   *  Set the rectangle used to type Unicode text inputs.
   *  This is used as a hint for IME and on-screen keyboard placement.
   *  
   *  SDL_StartTextInput()
   *}

  procedure SDL_SetTextInputRect(rect: PSDL_Rect) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetTextInputRect' {$ENDIF}{$ENDIF};

  {**
   *  Returns whether the platform has some screen keyboard support.
   *  
   *  SDL_TRUE if some keyboard support is available else SDL_FALSE.
   *
   *  Not all screen keyboard functions are supported on all platforms.
   *
   *  SDL_IsScreenKeyboardShown()
   *}

  function SDL_HasScreenKeyboardSupport: TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasScreenKeyboardSupport' {$ENDIF}{$ENDIF};

  {**
   *  Returns whether the screen keyboard is shown for given window.
   *
   *  window The window for which screen keyboard should be queried.
   *
   *  Result - SDL_TRUE if screen keyboard is shown else SDL_FALSE.
   *
   *  SDL_HasScreenKeyboardSupport()
   *}

  function SDL_IsScreenKeyboardShown(window: PSDL_Window): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_IsScreenKeyboardShown' {$ENDIF}{$ENDIF};

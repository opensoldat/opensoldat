//from "sdl_mouse.h"

type
  PSDL_Cursor = Pointer;

const

  {**
   *  Cursor types for SDL_CreateSystemCursor.
   *}

  SDL_SYSTEM_CURSOR_ARROW = 0;     // Arrow
  SDL_SYSTEM_CURSOR_IBEAM = 1;     // I-beam
  SDL_SYSTEM_CURSOR_WAIT = 2;      // Wait
  SDL_SYSTEM_CURSOR_CROSSHAIR = 3; // Crosshair
  SDL_SYSTEM_CURSOR_WAITARROW = 4; // Small wait cursor (or Wait if not available)
  SDL_SYSTEM_CURSOR_SIZENWSE = 5;  // Double arrow pointing northwest and southeast
  SDL_SYSTEM_CURSOR_SIZENESW = 6;  // Double arrow pointing northeast and southwest
  SDL_SYSTEM_CURSOR_SIZEWE = 7;    // Double arrow pointing west and east
  SDL_SYSTEM_CURSOR_SIZENS = 8;    // Double arrow pointing north and south
  SDL_SYSTEM_CURSOR_SIZEALL = 9;   // Four pointed arrow pointing north, south, east, and west
  SDL_SYSTEM_CURSOR_NO = 10;        // Slashed circle or crossbones
  SDL_SYSTEM_CURSOR_HAND = 11;      // Hand
  SDL_NUM_SYSTEM_CURSORS = 12;

type
  PSDL_SystemCursor = ^TSDL_SystemCursor;
  TSDL_SystemCursor = Word;

  {* Function prototypes *}

  {**
   *  Get the window which currently has mouse focus.
   *}

  function SDL_GetMouseFocus: PSDL_Window cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetMouseFocus' {$ENDIF}{$ENDIF};

  {**
   *  Retrieve the current state of the mouse.
   *  
   *  The current button state is returned as a button bitmask, which can
   *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
   *  mouse cursor position relative to the focus window for the currently
   *  selected mouse.  You can pass nil for either x or y.
   *
   * SDL_Button = 1 shl ((X)-1)
   *}

  function SDL_GetMouseState(x: PInt; y: PInt): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetMouseState' {$ENDIF}{$ENDIF};


  {**
   *  \brief Get the current state of the mouse, in relation to the desktop
   *
   *  This works just like SDL_GetMouseState(), but the coordinates will be
   *  reported relative to the top-left of the desktop. This can be useful if
   *  you need to track the mouse outside of a specific window and
   *  SDL_CaptureMouse() doesn't fit your needs. For example, it could be
   *  useful if you need to track the mouse while dragging a window, where
   *  coordinates relative to a window might not be in sync at all times.
   *
   *  \note SDL_GetMouseState() returns the mouse position as SDL understands
   *        it from the last pump of the event queue. This function, however,
   *        queries the OS for the current mouse position, and as such, might
   *        be a slightly less efficient function. Unless you know what you're
   *        doing and have a good reason to use this function, you probably want
   *        SDL_GetMouseState() instead.
   *
   *  \param x Returns the current X coord, relative to the desktop. Can be NULL.
   *  \param y Returns the current Y coord, relative to the desktop. Can be NULL.
   *  \return The current button state as a bitmask, which can be tested using the SDL_BUTTON(X) macros.
   *
   *  \sa SDL_GetMouseState
   *}
  function SDL_GetGlobalMouseState(x, y: PSInt32): UInt32; cdecl;
    external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetGlobalMouseState' {$ENDIF}{$ENDIF};

  {**
   *  Retrieve the relative state of the mouse.
   *
   *  The current button state is returned as a button bitmask, which can
   *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
   *  mouse deltas since the last call to SDL_GetRelativeMouseState().
   *}

  function SDL_GetRelativeMouseState(x: PInt; y: PInt): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRelativeMouseState' {$ENDIF}{$ENDIF};

  {**
   *  Moves the mouse to the given position within the window.
   *
   *   window The window to move the mouse into, or nil for the current mouse focus
   *   x The x coordinate within the window
   *   y The y coordinate within the window
   *
   *  This function generates a mouse motion event
   *}

  procedure SDL_WarpMouseInWindow(window: PSDL_Window; x: SInt32; y: SInt32) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WarpMouseInWindow' {$ENDIF}{$ENDIF};

  {**
   *  \brief Moves the mouse to the given position in global screen space.
   *
   *  \param x The x coordinate
   *  \param y The y coordinate
   *  \return 0 on success, -1 on error (usually: unsupported by a platform).
   *
   *  \note This function generates a mouse motion event
   *}
  Function SDL_WarpMouseGlobal(x, y: SInt32): SInt32; cdecl;
    external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WarpMouseGlobal' {$ENDIF}{$ENDIF};

  {**
   *  Set relative mouse mode.
   *
   *  enabled Whether or not to enable relative mode
   *
   *  0 on success, or -1 if relative mode is not supported.
   *
   *  While the mouse is in relative mode, the cursor is hidden, and the
   *  driver will try to report continuous motion in the current window.
   *  Only relative motion events will be delivered, the mouse position
   *  will not change.
   *
   *  This function will flush any pending mouse motion.
   *  
   *  SDL_GetRelativeMouseMode()
   *}

  function SDL_SetRelativeMouseMode(enabled: TSDL_Bool): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetRelativeMouseMode' {$ENDIF}{$ENDIF};

  {**
   *  \brief Capture the mouse, to track input outside an SDL window.
   *
   *  \param enabled Whether or not to enable capturing
   *
   *  Capturing enables your app to obtain mouse events globally, instead of
   *  just within your window. Not all video targets support this function.
   *  When capturing is enabled, the current window will get all mouse events,
   *  but unlike relative mode, no change is made to the cursor and it is
   *  not restrained to your window.
   *
   *  This function may also deny mouse input to other windows--both those in
   *  your application and others on the system--so you should use this
   *  function sparingly, and in small bursts. For example, you might want to
   *  track the mouse while the user is dragging something, until the user
   *  releases a mouse button. It is not recommended that you capture the mouse
   *  for long periods of time, such as the entire time your app is running.
   *
   *  While captured, mouse events still report coordinates relative to the
   *  current (foreground) window, but those coordinates may be outside the
   *  bounds of the window (including negative values). Capturing is only
   *  allowed for the foreground window. If the window loses focus while
   *  capturing, the capture will be disabled automatically.
   *
   *  While capturing is enabled, the current window will have the
   *  SDL_WINDOW_MOUSE_CAPTURE flag set.
   *
   *  \return 0 on success, or -1 if not supported.
   *}
   function SDL_CaptureMouse(enabled: TSDL_Bool): SInt32; cdecl;
     external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CaptureMouse' {$ENDIF}{$ENDIF};

  {**
   *  Query whether relative mouse mode is enabled.
   *  
   *  SDL_SetRelativeMouseMode()
   *}

  function SDL_GetRelativeMouseMode: TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRelativeMouseMode' {$ENDIF}{$ENDIF};

  {**
   *  Create a cursor, using the specified bitmap data and
   *  mask (in MSB format).
   *
   *  The cursor width must be a multiple of 8 bits.
   *
   *  The cursor is created in black and white according to the following:
   *  <table>
   *  <tr><td> data </td><td> mask </td><td> resulting pixel on screen </td></tr>
   *  <tr><td>  0   </td><td>  1   </td><td> White </td></tr>
   *  <tr><td>  1   </td><td>  1   </td><td> Black </td></tr>
   *  <tr><td>  0   </td><td>  0   </td><td> Transparent </td></tr>
   *  <tr><td>  1   </td><td>  0   </td><td> Inverted color if possible, black 
   *                                         if not. </td></tr>
   *  </table>
   *  
   *  SDL_FreeCursor()
   *}

  function SDL_CreateCursor(const data: PUInt8; const mask: PUInt8; w: SInt32; h: SInt32; hot_x: SInt32; hot_y: SInt32): PSDL_Cursor cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateCursor' {$ENDIF}{$ENDIF};

  {**
   *  Create a color cursor.
   *
   *  SDL_FreeCursor()
   *}

  function SDL_CreateColorCursor(surface: PSDL_Surface; hot_x: SInt32; hot_y: SInt32): PSDL_Cursor cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateColorCursor' {$ENDIF}{$ENDIF};

  {**
   *  Create a system cursor.
   *
   *  SDL_FreeCursor()
   *}

  function SDL_CreateSystemCursor(id: TSDL_SystemCursor): PSDL_Cursor cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateSystemCursor' {$ENDIF}{$ENDIF};

  {**
   *  Set the active cursor.
   *}

  procedure SDL_SetCursor(cursor: PSDL_Cursor) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetCursor' {$ENDIF}{$ENDIF};

  {**
   *  Return the active cursor.
   *}

  function SDL_GetCursor: PSDL_Cursor cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetCursor' {$ENDIF}{$ENDIF};
  
  {**
   * Return the default cursor.
   *}
   
   function SDL_GetDefaultCursor: PSDL_Cursor cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetDefaultCursor' {$ENDIF}{$ENDIF};

  {**
   *  Frees a cursor created with SDL_CreateCursor().
   *
   *  SDL_CreateCursor()
   *}

  procedure SDL_FreeCursor(cursor: PSDL_Cursor) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_FreeCursor' {$ENDIF}{$ENDIF};

  {**
   *  Toggle whether or not the cursor is shown.
   *
   *  toggle 1 to show the cursor, 0 to hide it, -1 to query the current
   *                state.
   *  
   *  1 if the cursor is shown, or 0 if the cursor is hidden.
   *}

  function SDL_ShowCursor(toggle: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ShowCursor' {$ENDIF}{$ENDIF};

  function SDL_Button(button: SInt32): SInt32; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}

const
  {**
   *  Used as a mask when testing buttons in buttonstate.
   *   - Button 1:  Left mouse button
   *   - Button 2:  Middle mouse button
   *   - Button 3:  Right mouse button
   *}

  SDL_BUTTON_LEFT	= 1;
  SDL_BUTTON_MIDDLE	= 2;
  SDL_BUTTON_RIGHT	= 3;
  SDL_BUTTON_X1	    = 4;
  SDL_BUTTON_X2	    = 5;
  SDL_BUTTON_LMASK  = 1 shl ((SDL_BUTTON_LEFT) - 1);
  SDL_BUTTON_MMASK  = 1 shl ((SDL_BUTTON_MIDDLE) - 1);
  SDL_BUTTON_RMASK  = 1 shl ((SDL_BUTTON_RIGHT) - 1);
  SDL_BUTTON_X1MASK = 1 shl ((SDL_BUTTON_X1) - 1);
  SDL_BUTTON_X2MASK = 1 shl ((SDL_BUTTON_X2) - 1);

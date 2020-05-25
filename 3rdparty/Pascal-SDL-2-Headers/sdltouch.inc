//from "sdl_touch.h"

type
  PSDL_TouchID  = ^TSDL_TouchID;
  TSDL_TouchID  = SInt64;

  PSDL_FingerID = ^TSDL_FingerID;
  TSDL_FingerID = SInt64;

  PSDL_Finger = ^TSDL_Finger;
  TSDL_Finger = record
    id: TSDL_FingerID;
    x: Float;
    y: Float;
    pressure: Float;
  end;

{* Used as the device ID for mouse events simulated with touch input *}
const
  SDL_TOUCH_MOUSEID = UInt32(-1);

  {* Function prototypes *}

  {**
   *  Get the number of registered touch devices.
   *}
function SDL_GetNumTouchDevices: SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetNumTouchDevices' {$ENDIF} {$ENDIF};

  {**
   *  Get the touch ID with the given index, or 0 if the index is invalid.
   *}
function SDL_GetTouchDevice(index: SInt32): TSDL_TouchID cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetTouchDevice' {$ENDIF} {$ENDIF};

  {**
   *  Get the number of active fingers for a given touch device.
   *}
function SDL_GetNumTouchFingers(touchID: TSDL_TouchID): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetNumTouchFingers' {$ENDIF} {$ENDIF};

  {**
   *  Get the finger object of the given touch, with the given index.
   *}
function SDL_GetTouchFinger(touchID: TSDL_TouchID; index: SInt32): PSDL_Finger cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetTouchFinger' {$ENDIF} {$ENDIF};


//from "sdl.h"

const

  SDL_INIT_TIMER          = $00000001;
  {$EXTERNALSYM SDL_INIT_TIMER}
  SDL_INIT_AUDIO          = $00000010;
  {$EXTERNALSYM SDL_INIT_AUDIO}
  SDL_INIT_VIDEO          = $00000020;   // SDL_INIT_VIDEO implies SDL_INIT_EVENTS
  {$EXTERNALSYM SDL_INIT_VIDEO}
  SDL_INIT_JOYSTICK       = $00000200;   // SDL_INIT_JOYSTICK implies SDL_INIT_EVENTS
  {$EXTERNALSYM SDL_INIT_JOYSTICK}
  SDL_INIT_HAPTIC         = $00001000;
  {$EXTERNALSYM SDL_INIT_HAPTIC}
  SDL_INIT_GAMECONTROLLER = $00002000;  //turn on game controller also implicitly does JOYSTICK
  {$EXTERNALSYM SDL_INIT_GAMECONTROLLER}// SDL_INIT_GAMECONTROLLER implies SDL_INIT_JOYSTICK
  SDL_INIT_EVENTS         = $00004000;
  {$EXTERNALSYM SDL_INIT_EVENTS}
  SDL_INIT_NOPARACHUTE    = $00100000;  //Don't catch fatal signals
  {$EXTERNALSYM SDL_INIT_NOPARACHUTE}   // compatibility; this flag is ignored.
  SDL_INIT_EVERYTHING     = SDL_INIT_TIMER    or
                              SDL_INIT_AUDIO    or
                              SDL_INIT_VIDEO    or
                              SDL_INIT_EVENTS   or
                              SDL_INIT_JOYSTICK or
                              SDL_INIT_HAPTIC   or
                              SDL_INIT_GAMECONTROLLER;
  {$EXTERNALSYM SDL_INIT_EVERYTHING}

{**
 *  This function initializes  the subsystems specified by flags
 *  Unless the SDL_INIT_NOPARACHUTE flag is set, it will install cleanup
 *  signal handlers for some commonly ignored fatal signals (like SIGSEGV).
 *}

function SDL_Init(flags: UInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Init' {$ENDIF} {$ENDIF};

{**
 *  This function initializes specific SDL subsystems
 *}

function SDL_InitSubSystem(flags: UInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_InitSubSystem' {$ENDIF} {$ENDIF};

{**
 *  This function cleans up specific SDL subsystems
 *}
 
procedure SDL_QuitSubSystem(flags: UInt32) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_QuitSubSystem' {$ENDIF} {$ENDIF};

{**
 *  This function returns a mask of the specified subsystems which have
 *  previously been initialized.
 *  
 *  If flags is 0, it returns a mask of all initialized subsystems.
 *}
 
function SDL_WasInit(flags: UInt32): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WasInit' {$ENDIF} {$ENDIF};

{**
 *  This function cleans up all initialized subsystems. You should
 *  call it upon all exit conditions.
 *}
 
procedure SDL_Quit() cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Quit' {$ENDIF} {$ENDIF};


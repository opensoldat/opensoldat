//from "sdl.h"

type
  TSDL_Init = type UInt32;

const
  SDL_INIT_TIMER          = TSDL_Init($00000001);
  {$EXTERNALSYM SDL_INIT_TIMER}
  SDL_INIT_AUDIO          = TSDL_Init($00000010);
  {$EXTERNALSYM SDL_INIT_AUDIO}
  SDL_INIT_VIDEO          = TSDL_Init($00000020);   // SDL_INIT_VIDEO implies SDL_INIT_EVENTS
  {$EXTERNALSYM SDL_INIT_VIDEO}
  SDL_INIT_JOYSTICK       = TSDL_Init($00000200);   // SDL_INIT_JOYSTICK implies SDL_INIT_EVENTS
  {$EXTERNALSYM SDL_INIT_JOYSTICK}
  SDL_INIT_HAPTIC         = TSDL_Init($00001000);
  {$EXTERNALSYM SDL_INIT_HAPTIC}
  SDL_INIT_GAMECONTROLLER = TSDL_Init($00002000);   //turn on game controller also implicitly does JOYSTICK
  {$EXTERNALSYM SDL_INIT_GAMECONTROLLER}            // SDL_INIT_GAMECONTROLLER implies SDL_INIT_JOYSTICK
  SDL_INIT_EVENTS         = TSDL_Init($00004000);
  {$EXTERNALSYM SDL_INIT_EVENTS}
  SDL_INIT_SENSOR         = TSDL_Init($00008000);
  {$EXTERNALSYM SDL_INIT_SENSOR}
  SDL_INIT_NOPARACHUTE    = TSDL_Init($00100000);   //Don't catch fatal signals
  {$EXTERNALSYM SDL_INIT_NOPARACHUTE}               // compatibility; this flag is ignored.
  SDL_INIT_EVERYTHING     = TSDL_Init(
                              SDL_INIT_TIMER          or
                              SDL_INIT_AUDIO          or
                              SDL_INIT_VIDEO          or
                              SDL_INIT_EVENTS         or
                              SDL_INIT_JOYSTICK       or
                              SDL_INIT_HAPTIC         or
                              SDL_INIT_GAMECONTROLLER or
                              SDL_INIT_SENSOR
                              );
  {$EXTERNALSYM SDL_INIT_EVERYTHING}

{**
 *  This function initializes  the subsystems specified by flags
 *  Unless the SDL_INIT_NOPARACHUTE flag is set, it will install cleanup
 *  signal handlers for some commonly ignored fatal signals (like SIGSEGV).
 *}

function SDL_Init(flags: TSDL_Init): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Init' {$ENDIF} {$ENDIF};

{**
 *  This function initializes specific SDL subsystems
 *}

function SDL_InitSubSystem(flags: TSDL_Init): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_InitSubSystem' {$ENDIF} {$ENDIF};

{**
 *  This function cleans up specific SDL subsystems
 *}
 
procedure SDL_QuitSubSystem(flags: TSDL_Init) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_QuitSubSystem' {$ENDIF} {$ENDIF};

{**
 *  This function returns a mask of the specified subsystems which have
 *  previously been initialized.
 *  
 *  If flags is 0, it returns a mask of all initialized subsystems.
 *}
 
function SDL_WasInit(flags: TSDL_Init): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WasInit' {$ENDIF} {$ENDIF};

{**
 *  This function cleans up all initialized subsystems. You should
 *  call it upon all exit conditions.
 *}
 
procedure SDL_Quit() cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Quit' {$ENDIF} {$ENDIF};


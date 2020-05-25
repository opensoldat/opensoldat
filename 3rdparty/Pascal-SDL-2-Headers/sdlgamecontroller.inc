//from sdl_gamecontroller.h

{**
 *   SDL_gamecontroller.h
 *
 *  In order to use these functions, SDL_Init() must have been called
 *  with the ::SDL_INIT_JOYSTICK flag.  This causes SDL to scan the system
 *  for game controllers, and load appropriate drivers.
 *
 *  If you would like to receive controller updates while the application
 *  is in the background, you should set the following hint before calling
 *  SDL_Init(): SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
 *}

{* The gamecontroller structure used to identify an SDL game controller *}
type
  PSDL_GameController = ^TSDL_GameController;
  TSDL_GameController = Pointer; //todo

  TSDL_GameControllerBindType = (SDL_CONTROLLER_BINDTYPE_NONE,
                                 SDL_CONTROLLER_BINDTYPE_BUTTON,
                                 SDL_CONTROLLER_BINDTYPE_AXIS,
                                 SDL_CONTROLLER_BINDTYPE_HAT);

  {**
   *  Get the SDL joystick layer binding for this controller button/axis mapping
   *}
  THat = record
    hat: Integer;
    hat_mask: Integer;
  end;

  TSDL_GameControllerButtonBind = record
    bindType: TSDL_GameControllerBindType;
    case Integer of
      0: ( button: Integer; );
      1: ( axis: Integer; );
      2: ( hat: THat; );
  end;

  {**
   *  To count the number of game controllers in the system for the following:
   *  int nJoysticks = SDL_NumJoysticks();
   *  int nGameControllers = 0;
   *  for ( int i = 0; i < nJoysticks; i++ ) {
   *      if ( SDL_IsGameController(i) ) {
   *          nGameControllers++;
   *
   *
   *
   *  Using the SDL_HINT_GAMECONTROLLERCONFIG hint or the SDL_GameControllerAddMapping you can add support for controllers SDL is unaware of or cause an existing controller to have a different binding. The format is:
   *  guid,name,mappings
   *
   *  Where GUID is the string value from SDL_JoystickGetGUIDString(), name is the human readable string for the device and mappings are controller mappings to joystick ones.
   *  Under Windows there is a reserved GUID of "xinput" that covers any XInput devices.
   *  The mapping format for joystick is:
   *      bX - a joystick button, index X
   *      hX.Y - hat X with value Y
   *      aX - axis X of the joystick
   *  Buttons can be used as a controller axis and vice versa.
   *
   *  This string shows an example of a valid mapping for a controller
   *  "341a3608000000000000504944564944,Afterglow PS3 Controller,a:b1,b:b2,y:b3,x:b0,start:b9,guide:b12,back:b8,dpup:h0.1,dpleft:h0.8,dpdown:h0.4,dpright:h0.2,leftshoulder:b4,rightshoulder:b5,leftstick:b10,rightstick:b11,leftx:a0,lefty:a1,rightx:a2,righty:a3,lefttrigger:b6,righttrigger:b7",
   *
   *}

  {**
   *  Add or update an existing mapping configuration
   *
   *  1 if mapping is added, 0 if updated, -1 on error
   *}
function SDL_GameControllerAddMapping( mappingString: PAnsiChar ): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerAddMapping' {$ENDIF} {$ENDIF};

  {**
   *  Load a set of mappings from a seekable SDL data stream (memory or file), filtered by the current SDL_GetPlatform()
   *  A community sourced database of controllers is available at https://raw.github.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt
   *
   *  If freerw is non-zero, the stream will be closed after being read.
   * 
   *  Returns number of mappings added, -1 on error
   *}
function SDL_GameControllerAddMappingsFromRW(rw: PSDL_RWops; freerw: SInt32):SInt32;
   cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerAddMappingsFromRW' {$ENDIF} {$ENDIF};

  {**
   *  Get a mapping string for a GUID
   *
   *   the mapping string.  Must be freed with SDL_free.  Returns NULL if no mapping is available
   *}
function SDL_GameControllerMappingForGUID( guid: TSDL_JoystickGUID ): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerMappingForGUID' {$ENDIF} {$ENDIF};

  {**
   *  Get a mapping string for an open GameController
   *
   *   the mapping string.  Must be freed with SDL_free.  Returns NULL if no mapping is available
   *}
function SDL_GameControllerMapping( gamecontroller: PSDL_GameController ): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerMapping' {$ENDIF} {$ENDIF};

  {**
   *  Is the joystick on this index supported by the game controller interface?
   *}
function SDL_IsGameController(joystick_index: Integer): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_IsGameController' {$ENDIF} {$ENDIF};

  {**
   *  Get the implementation dependent name of a game controller.
   *  This can be called before any controllers are opened.
   *  If no name can be found, this function returns NULL.
   *}
function SDL_GameControllerNameForIndex(joystick_index: Integer): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerNameForIndex' {$ENDIF} {$ENDIF};

  {**
   *  Open a game controller for use.
   *  The index passed as an argument refers to the N'th game controller on the system.
   *  This index is the value which will identify this controller in future controller
   *  events.
   *
   *   A controller identifier, or NULL if an error occurred.
   *}
function SDL_GameControllerOpen(joystick_index: Integer): PSDL_GameController cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerOpen' {$ENDIF} {$ENDIF};

  {**
   * Return the SDL_GameController associated with an instance id.
   *}
function SDL_GameControllerFromInstanceID(joyid: TSDL_JoystickID): PSDL_GameController; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerFromInstanceID' {$ENDIF} {$ENDIF};

  {**
   *  Return the name for this currently opened controller
   *}
function SDL_GameControllerName(gamecontroller: PSDL_GameController): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerName' {$ENDIF} {$ENDIF};

  {**
   *  Returns SDL_TRUE if the controller has been opened and currently connected,
   *  or SDL_FALSE if it has not.
   *}
function SDL_GameControllerGetAttached(gamecontroller: PSDL_GameController): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetAttached' {$ENDIF} {$ENDIF};

  {**
   *  Get the underlying joystick object used by a controller
   *}
function SDL_GameControllerGetJoystick(gamecontroller: PSDL_GameController): PSDL_Joystick cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetJoystick' {$ENDIF} {$ENDIF};

  {**
   *  Enable/disable controller event polling.
   *
   *  If controller events are disabled, you must call SDL_GameControllerUpdate()
   *  yourself and check the state of the controller when you want controller
   *  information.
   *
   *  The state can be one of ::SDL_QUERY, ::SDL_ENABLE or ::SDL_IGNORE.
   *}
function SDL_GameControllerEventState(state: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerEventState' {$ENDIF} {$ENDIF};

  {**
   *  Update the current state of the open game controllers.
   *
   *  This is called automatically by the event loop if any game controller
   *  events are enabled.
   *}
procedure SDL_GameControllerUpdate() cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerUpdate' {$ENDIF} {$ENDIF};

  {**
   *  The list of axes available from a controller
   *}

const
  SDL_CONTROLLER_AXIS_INVALID = -1;
  SDL_CONTROLLER_AXIS_LEFTX = 0;
  SDL_CONTROLLER_AXIS_LEFTY = 1;
  SDL_CONTROLLER_AXIS_RIGHTX = 2;
  SDL_CONTROLLER_AXIS_RIGHTY = 3;
  SDL_CONTROLLER_AXIS_TRIGGERLEFT = 4;
  SDL_CONTROLLER_AXIS_TRIGGERRIGHT = 5;
  SDL_CONTROLLER_AXIS_MAX = 6;
type
  TSDL_GameControllerAxis = Byte;

  {**
   *  turn this string into a axis mapping
   *}
function SDL_GameControllerGetAxisFromString(pchString: PAnsiChar): TSDL_GameControllerAxis cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetAxisFromString' {$ENDIF} {$ENDIF};

  {**
   *  turn this axis enum into a string mapping
   *}
function SDL_GameControllerGetStringForAxis(axis: TSDL_GameControllerAxis): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetStringForAxis' {$ENDIF} {$ENDIF};

  {**
   *  Get the SDL joystick layer binding for this controller button mapping
   *}
function SDL_GameControllerGetBindForAxis(gamecontroller: PSDL_GameController; axis: TSDL_GameControllerAxis): TSDL_GameControllerButtonBind cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetBindForAxis' {$ENDIF} {$ENDIF};

  {**
   *  Get the current state of an axis control on a game controller.
   *
   *  The state is a value ranging from -32768 to 32767.
   *
   *  The axis indices start at index 0.
   *}
function SDL_GameControllerGetAxis(gamecontroller: PSDL_GameController; axis: TSDL_GameControllerAxis): SInt16 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetAxis' {$ENDIF} {$ENDIF};

  {**
   *  The list of buttons available from a controller
   *}
const
  SDL_CONTROLLER_BUTTON_INVALID = -1;
  SDL_CONTROLLER_BUTTON_A = 0;
  SDL_CONTROLLER_BUTTON_B = 1;
  SDL_CONTROLLER_BUTTON_X = 2;
  SDL_CONTROLLER_BUTTON_Y = 3;
  SDL_CONTROLLER_BUTTON_BACK = 4;
  SDL_CONTROLLER_BUTTON_GUIDE = 5;
  SDL_CONTROLLER_BUTTON_START = 6;
  SDL_CONTROLLER_BUTTON_LEFTSTICK = 7;
  SDL_CONTROLLER_BUTTON_RIGHTSTICK = 8;
  SDL_CONTROLLER_BUTTON_LEFTSHOULDER = 9;
  SDL_CONTROLLER_BUTTON_RIGHTSHOULDER = 10;
  SDL_CONTROLLER_BUTTON_DPAD_UP = 11;
  SDL_CONTROLLER_BUTTON_DPAD_DOWN = 12;
  SDL_CONTROLLER_BUTTON_DPAD_LEFT = 13;
  SDL_CONTROLLER_BUTTON_DPAD_RIGHT = 14;
  SDL_CONTROLLER_BUTTON_MAX = 15;
type
  TSDL_GameControllerButton = Byte;

  {**
   *  turn this string into a button mapping
   *}
function SDL_GameControllerGetButtonFromString(pchString: PAnsiChar): TSDL_GameControllerButton cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetButtonFromString' {$ENDIF} {$ENDIF};

  {**
   *  turn this button enum into a string mapping
   *}
function SDL_GameControllerGetStringForButton(button: TSDL_GameControllerButton): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetStringForButton' {$ENDIF} {$ENDIF};

{**
 *  Get the SDL joystick layer binding for this controller button mapping
 *}
function SDL_GameControllerGetBindForButton(gamecontroller: PSDL_GameController; button: TSDL_GameControllerButton): TSDL_GameControllerButtonBind cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetBindForButton' {$ENDIF} {$ENDIF};


{**
 *  Get the current state of a button on a game controller.
 *
 *  The button indices start at index 0.
 *}
function SDL_GameControllerGetButton(gamecontroller: PSDL_GameController; button: TSDL_GameControllerButton): UInt8 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetButton' {$ENDIF} {$ENDIF};

{**
 *  Close a controller previously opened with SDL_GameControllerOpen().
 *}
procedure SDL_GameControllerClose(gamecontroller: PSDL_GameController) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerClose' {$ENDIF} {$ENDIF};


function SDL_GameControllerAddMappingsFromFile(Const FilePath:PAnsiChar):SInt32;

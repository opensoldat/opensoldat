// based on sdl_joystick.h (2.0.14)

{**
 *  \file SDL_joystick.h
 *
 *  Include file for SDL joystick event handling
 *
 * The term "device_index" identifies currently plugged in joystick devices between 0 and SDL_NumJoysticks(), with the exact joystick
 *   behind a device_index changing as joysticks are plugged and unplugged.
 *
 * The term "instance_id" is the current instantiation of a joystick device in the system, if the joystick is removed and then re-inserted
 *   then it will get a new instance_id, instance_id's are monotonically increasing identifiers of a joystick plugged in.
 *
 * The term JoystickGUID is a stable 128-bit identifier for a joystick device that does not change over time, it identifies class of
 *   the device (a X360 wired controller for example). This identifier is platform dependent.
 *
 *
 *}

 {**
  *  \file SDL_joystick.h
  *
  *  In order to use these functions, SDL_Init() must have been called
  *  with the ::SDL_INIT_JOYSTICK flag.  This causes SDL to scan the system
  *  for joysticks, and load appropriate drivers.
  *
  *  If you would like to receive joystick updates while the application
  *  is in the background, you should set the following hint before calling
  *  SDL_Init(): SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
  *}

type
  {* The joystick structure used to identify an SDL joystick *}
  TSDL_Joystick = record end;
  PSDL_Joystick = ^TSDL_Joystick;

  {* A structure that encodes the stable unique id for a joystick device *}
  TSDL_JoystickGUID = record
    data: array[0..15] of UInt8;
  end;

 {**
  * This is a unique ID for a joystick for the time it is connected to the system,
  * and is never reused for the lifetime of the application. If the joystick is
  * disconnected and reconnected, it will get a new ID.
  *
  * The ID value starts at 0 and increments from there. The value -1 is an invalid ID.
  *}
  TSDL_JoystickID = SInt32;

type
  TSDL_JoystickType = type SInt32;

const
  SDL_JOYSTICK_TYPE_UNKNOWN        = TSDL_JoystickType(0);
  SDL_JOYSTICK_TYPE_GAMECONTROLLER = TSDL_JoystickType(1);
  SDL_JOYSTICK_TYPE_WHEEL          = TSDL_JoystickType(2);
  SDL_JOYSTICK_TYPE_ARCADE_STICK   = TSDL_JoystickType(3);
  SDL_JOYSTICK_TYPE_FLIGHT_STICK   = TSDL_JoystickType(4);
  SDL_JOYSTICK_TYPE_DANCE_PAD      = TSDL_JoystickType(5);
  SDL_JOYSTICK_TYPE_GUITAR         = TSDL_JoystickType(6);
  SDL_JOYSTICK_TYPE_DRUM_KIT       = TSDL_JoystickType(7);
  SDL_JOYSTICK_TYPE_ARCADE_PAD     = TSDL_JoystickType(8);
  SDL_JOYSTICK_TYPE_THROTTLE       = TSDL_JoystickType(9);

type
  TSDL_JoystickPowerLevel = type SInt32;

const
  SDL_JOYSTICK_POWER_UNKNOWN = TSDL_JoystickPowerLevel(-1);
  SDL_JOYSTICK_POWER_EMPTY   = TSDL_JoystickPowerLevel(0);  {* <= 5% *}
  SDL_JOYSTICK_POWER_LOW     = TSDL_JoystickPowerLevel(1);  {* <= 20% *}
  SDL_JOYSTICK_POWER_MEDIUM  = TSDL_JoystickPowerLevel(2);  {* <= 70% *}
  SDL_JOYSTICK_POWER_FULL    = TSDL_JoystickPowerLevel(3);  {* <= 100% *}
  SDL_JOYSTICK_POWER_WIRED   = TSDL_JoystickPowerLevel(4);
  SDL_JOYSTICK_POWER_MAX     = TSDL_JoystickPowerLevel(5);

  {* Set max recognized G-force from accelerometer
     See src/joystick/uikit/SDL_sysjoystick.m for notes on why this is needed
   *}
const
  SDL_IPHONE_MAX_GFORCE = 5.0;

{* Function prototypes *}

{**
 * Locking for multi-threaded access to the joystick API
 *
 * If you are using the joystick API or handling events from multiple threads
 * you should use these locking functions to protect access to the joysticks.
 *
 * In particular, you are guaranteed that the joystick list won't change, so
 * the API functions that take a joystick index will be valid, and joystick
 * and game controller events will not be delivered.
 *}
procedure SDL_LockJoysticks(); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockJoysticks' {$ENDIF} {$ENDIF};

procedure SDL_UnlockJoysticks(); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UnlockJoysticks' {$ENDIF} {$ENDIF};

{**
 *  Count the number of joysticks attached to the system right now
 *}
function SDL_NumJoysticks(): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_NumJoysticks' {$ENDIF} {$ENDIF};

{**
 *  Get the implementation dependent name of a joystick.
 *  This can be called before any joysticks are opened.
 *  If no name can be found, this function returns NULL.
 *}
function SDL_JoystickNameForIndex(device_index: SInt32): PAnsiChar; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickNameForIndex' {$ENDIF} {$ENDIF};

{**
 *  Get the player index of a joystick, or -1 if it's not available
 *  This can be called before any joysticks are opened.
 *}
function SDL_JoystickGetDevicePlayerIndex(device_index: SInt32): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetDevicePlayerIndex' {$ENDIF} {$ENDIF};

{**
 *  Return the GUID for the joystick at this index
 *  This can be called before any joysticks are opened.
 *}
function SDL_JoystickGetDeviceGUID(device_index: SInt32): TSDL_JoystickGUID; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetDeviceGUID' {$ENDIF} {$ENDIF};

{**
 *  Get the USB vendor ID of a joystick, if available.
 *  This can be called before any joysticks are opened.
 *  If the vendor ID isn't available this function returns 0.
 *}
function SDL_JoystickGetDeviceVendor(device_index: SInt32): UInt16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetDeviceVendor' {$ENDIF} {$ENDIF};

{**
 *  Get the USB product ID of a joystick, if available.
 *  This can be called before any joysticks are opened.
 *  If the product ID isn't available this function returns 0.
 *}
function SDL_JoystickGetDeviceProduct(device_index: SInt32): UInt16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetDeviceProduct' {$ENDIF} {$ENDIF};

{**
 *  Get the product version of a joystick, if available.
 *  This can be called before any joysticks are opened.
 *  If the product version isn't available this function returns 0.
 *}
function SDL_JoystickGetDeviceProductVersion(device_index: SInt32): UInt16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetDeviceProductVersion' {$ENDIF} {$ENDIF};

{**
 *  Get the type of a joystick, if available.
 *  This can be called before any joysticks are opened.
 *}
function SDL_JoystickGetDeviceType(device_index: SInt32): TSDL_JoystickType; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetDeviceType' {$ENDIF} {$ENDIF};

{**
 *  Get the instance ID of a joystick.
 *  This can be called before any joysticks are opened.
 *  If the index is out of range, this function will return -1.
 *}
function SDL_JoystickGetDeviceInstanceID(device_index: SInt32): TSDL_JoystickID; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetDeviceInstanceID' {$ENDIF} {$ENDIF};

{**
 *  Open a joystick for use.
 *  The index passed as an argument refers to the N'th joystick on the system.
 *  This index is not the value which will identify this joystick in future
 *  joystick events.  The joystick's instance id (::SDL_JoystickID) will be used
 *  there instead.
 *
 *  \return A joystick identifier, or NULL if an error occurred.
 *}
function SDL_JoystickOpen(device_index: SInt32): PSDL_Joystick; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickOpen' {$ENDIF} {$ENDIF};

{**
 * Return the SDL_Joystick associated with an instance id.
 *}
function SDL_JoystickFromInstanceID(instance_id: TSDL_JoystickID): PSDL_Joystick; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickFromInstanceID' {$ENDIF} {$ENDIF};

{**
 * Return the SDL_Joystick associated with a player index.
 *}
function SDL_JoystickFromPlayerIndex(player_index: SInt32): PSDL_Joystick; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickFromPlayerIndex' {$ENDIF} {$ENDIF};

{**
 * Attaches a new virtual joystick.
 * Returns the joystick's device index, or -1 if an error occurred.
 *}
function SDL_JoystickAttachVirtual(type_: TSDL_JoystickType; naxes: SInt32; nbuttons: SInt32; nhats: SInt32): PSDL_Joystick; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickAttachVirtual' {$ENDIF} {$ENDIF};

{**
 * Detaches a virtual joystick
 * Returns 0 on success, or -1 if an error occurred.
 *}
function SDL_JoystickDetachVirtual(device_index: SInt32): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickDetachVirtual' {$ENDIF} {$ENDIF};

{**
 * Indicates whether or not a virtual-joystick is at a given device index.
 *}
function SDL_JoystickIsVirtual(device_index: SInt32): TSDL_Bool; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickIsVirtual' {$ENDIF} {$ENDIF};

{**
 * Set values on an opened, virtual-joystick's controls.
 * Please note that values set here will not be applied until the next
 * call to SDL_JoystickUpdate, which can either be called directly,
 * or can be called indirectly through various other SDL APIS,
 * including, but not limited to the following: SDL_PollEvent,
 * SDL_PumpEvents, SDL_WaitEventTimeout, SDL_WaitEvent.
 *
 * Returns 0 on success, -1 on error.
 *}
function SDL_JoystickSetVirtualAxis(joystick: PSDL_Joystick; axis: SInt32; value: SInt16): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickSetVirtualAxis' {$ENDIF} {$ENDIF};

function SDL_JoystickSetVirtualButton(joystick: PSDL_Joystick; button: SInt32; value: UInt8): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickSetVirtualButton' {$ENDIF} {$ENDIF};

function SDL_JoystickSetVirtualHat(joystick: PSDL_Joystick; hat: SInt32; value: UInt8): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickSetVirtualHat' {$ENDIF} {$ENDIF};

{**
 *  Return the name for this currently opened joystick.
 *  If no name can be found, this function returns NULL.
 *}
function SDL_JoystickName(joystick: PSDL_Joystick): PAnsiChar; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickName' {$ENDIF} {$ENDIF};

{**
 *  Get the player index of an opened joystick, or -1 if it's not available
 *
 *  For XInput controllers this returns the XInput user index.
 *}
function SDL_JoystickGetPlayerIndex(joystick: PSDL_Joystick): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetPlayerIndex' {$ENDIF} {$ENDIF};

{**
 *  Set the player index of an opened joystick
 *}
procedure SDL_JoystickSetPlayerIndex(joystick: PSDL_Joystick; player_index: SInt32); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickSetPlayerIndex' {$ENDIF} {$ENDIF};

{**
 *  Return the GUID for this opened joystick
 *}
function SDL_JoystickGetGUID(joystick: PSDL_Joystick): TSDL_JoystickGUID; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetGUID' {$ENDIF} {$ENDIF};

{**
 *  Get the USB vendor ID of an opened joystick, if available.
 *  If the vendor ID isn't available this function returns 0.
 *}
function SDL_JoystickGetVendor(joystick: PSDL_Joystick): UInt16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetVendor' {$ENDIF} {$ENDIF};

{**
 *  Get the USB product ID of an opened joystick, if available.
 *  If the product ID isn't available this function returns 0.
 *}
function SDL_JoystickGetProduct(joystick: PSDL_Joystick): UInt16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetProduct' {$ENDIF} {$ENDIF};

{**
 *  Get the product version of an opened joystick, if available.
 *  If the product version isn't available this function returns 0.
 *}
function SDL_JoystickGetProductVersion(joystick: PSDL_Joystick): UInt16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetProductVersion' {$ENDIF} {$ENDIF};

{**
 *  Get the serial number of an opened joystick, if available.
 *
 *  Returns the serial number of the joystick, or NULL if it is not available.
 *}
function SDL_JoystickGetSerial(joystick: PSDL_Joystick): PAnsiChar; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetSerial' {$ENDIF} {$ENDIF};

{**
 *  Get the type of an opened joystick.
 *}
function SDL_JoystickGetType(joystick: PSDL_Joystick): TSDL_JoystickType; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetType' {$ENDIF} {$ENDIF};

{**
 *  Return a string representation for this guid. pszGUID must point to at least 33 bytes
 *  (32 for the string plus a NULL terminator).
 *}
procedure SDL_JoystickGetGUIDString(guid: TSDL_JoystickGUID; pszGUID: PAnsiChar; cbGUID: SInt32); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetGUIDString' {$ENDIF} {$ENDIF};

{**
 *  convert a string into a joystick guid
 *}
function SDL_JoystickGetGUIDFromString(const pchGUID: PAnsiChar): TSDL_JoystickGUID; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetGUIDFromString' {$ENDIF} {$ENDIF};

{**
 *  Returns SDL_TRUE if the joystick has been opened and currently connected, or SDL_FALSE if it has not.
 *}
function SDL_JoystickGetAttached(joystick: PSDL_Joystick): TSDL_Bool; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetAttached' {$ENDIF} {$ENDIF};

{**
 *  Get the instance ID of an opened joystick or -1 if the joystick is invalid.
 *}
function SDL_JoystickInstanceID(joystick: PSDL_Joystick): TSDL_JoystickID; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickInstanceID' {$ENDIF} {$ENDIF};

{**
 *  Get the number of general axis controls on a joystick.
 *}
function SDL_JoystickNumAxes(joystick: PSDL_Joystick): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickNumAxes' {$ENDIF} {$ENDIF};

{**
 *  Get the number of trackballs on a joystick.
 *
 *  Joystick trackballs have only relative motion events associated
 *  with them and their state cannot be polled.
 *}
function SDL_JoystickNumBalls(joystick: PSDL_Joystick): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickNumBalls' {$ENDIF} {$ENDIF};

{**
 *  Get the number of POV hats on a joystick.
 *}
function SDL_JoystickNumHats(joystick: PSDL_Joystick): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickNumHats' {$ENDIF} {$ENDIF};

{**
 *  Get the number of buttons on a joystick.
 *}
function SDL_JoystickNumButtons(joystick: PSDL_Joystick): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickNumButtons' {$ENDIF} {$ENDIF};

{**
 *  Update the current state of the open joysticks.
 *
 *  This is called automatically by the event loop if any joystick
 *  events are enabled.
 *}
procedure SDL_JoystickUpdate(); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickUpdate' {$ENDIF} {$ENDIF};

{**
 *  Enable/disable joystick event polling.
 *
 *  If joystick events are disabled, you must call SDL_JoystickUpdate()
 *  yourself and check the state of the joystick when you want joystick
 *  information.
 *
 *  The state can be one of ::SDL_QUERY, ::SDL_ENABLE or ::SDL_IGNORE.
 *}
function SDL_JoystickEventState(state: SInt32): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickEventState' {$ENDIF} {$ENDIF};

const
  SDL_JOYSTICK_AXIS_MAX = 32767;
  SDL_JOYSTICK_AXIS_MIN = -32768;

{**
 *  Get the current state of an axis control on a joystick.
 *
 *  The state is a value ranging from -32768 to 32767.
 *
 *  The axis indices start at index 0.
 *}
function SDL_JoystickGetAxis(joystick: PSDL_Joystick; axis: SInt32): SInt16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetAxis' {$ENDIF} {$ENDIF};

{**
 *  Get the initial state of an axis control on a joystick.
 *
 *  The state is a value ranging from -32768 to 32767.
 *
 *  The axis indices start at index 0.
 *
 *  \return SDL_TRUE if this axis has any initial value, or SDL_FALSE if not.
 *}
function SDL_JoystickGetAxisInitialState(joystick: PSDL_Joystick; axis: SInt32; state: PSInt16): TSDL_Bool; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetAxisInitialState' {$ENDIF} {$ENDIF};

{**
 *  Hat positions
 *}
const
  SDL_HAT_CENTERED  = $00;
  SDL_HAT_UP        = $01;
  SDL_HAT_RIGHT     = $02;
  SDL_HAT_DOWN      = $04;
  SDL_HAT_LEFT      = $08;
  SDL_HAT_RIGHTUP   = SDL_HAT_RIGHT or SDL_HAT_UP;
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;
  SDL_HAT_LEFTUP    = SDL_HAT_LEFT or SDL_HAT_UP;
  SDL_HAT_LEFTDOWN  = SDL_HAT_LEFT or SDL_HAT_DOWN;

{**
 *  Get the current state of a POV hat on a joystick.
 *
 *  The hat indices start at index 0.
 *
 *  The return value is one of the following positions:
 *   - SDL_HAT_CENTERED
 *   - SDL_HAT_UP
 *   - SDL_HAT_RIGHT
 *   - SDL_HAT_DOWN
 *   - SDL_HAT_LEFT
 *   - SDL_HAT_RIGHTUP
 *   - SDL_HAT_RIGHTDOWN
 *   - SDL_HAT_LEFTUP
 *   - SDL_HAT_LEFTDOWN
 *}
function SDL_JoystickGetHat(joystick: PSDL_Joystick; hat: SInt32): UInt8; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetHat' {$ENDIF} {$ENDIF};

{**
 *  Get the ball axis change since the last poll.
 *
 *  0, or -1 if you passed it invalid parameters.
 *
 *  The ball indices start at index 0.
 *}
function SDL_JoystickGetBall(joystick: PSDL_Joystick; ball: SInt32; dx: PSInt32; dy: PSInt32): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetBall' {$ENDIF} {$ENDIF};

{**
 *  Get the current state of a button on a joystick.
 *
 *  The button indices start at index 0.
 *}
function SDL_JoystickGetButton(joystick: PSDL_Joystick; button: SInt32): UInt8; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickGetButton' {$ENDIF} {$ENDIF};

{**
 *  Start a rumble effect
 *  Each call to this function cancels any previous rumble effect, and calling it with 0 intensity stops any rumbling.
 *
 *  \param joystick The joystick to vibrate
 *  \param low_frequency_rumble The intensity of the low frequency (left) rumble motor, from 0 to 0xFFFF
 *  \param high_frequency_rumble The intensity of the high frequency (right) rumble motor, from 0 to 0xFFFF
 *  \param duration_ms The duration of the rumble effect, in milliseconds
 *
 *  \return 0, or -1 if rumble isn't supported on this joystick
 *}
function SDL_JoystickRumble(joystick: PSDL_Joystick; low_frequency_rumble: UInt16; high_frequency_rumble: UInt16; duration_ms: UInt32): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickRumble' {$ENDIF} {$ENDIF};

{**
 *  Start a rumble effect in the joystick's triggers
 *  Each call to this function cancels any previous trigger rumble effect, and calling it with 0 intensity stops any rumbling.
 *
 *  \param joystick The joystick to vibrate
 *  \param left_rumble The intensity of the left trigger rumble motor, from 0 to 0xFFFF
 *  \param right_rumble The intensity of the right trigger rumble motor, from 0 to 0xFFFF
 *  \param duration_ms The duration of the rumble effect, in milliseconds
 *
 *  \return 0, or -1 if trigger rumble isn't supported on this joystick
 *}
function SDL_JoystickRumbleTriggerse(joystick: PSDL_Joystick; left_rumble: UInt16; right_rumble: UInt16; duration_ms: UInt32): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickRumbleTriggers' {$ENDIF} {$ENDIF};

{**
 *  Return whether a joystick has an LED
 *
 *  \param joystick The joystick to query
 *
 *  \return SDL_TRUE, or SDL_FALSE if this joystick does not have a modifiable LED
 *}
function SDL_JoystickHasLED(joystick: PSDL_Joystick; button: SInt32): TSDL_Bool; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickHasLED' {$ENDIF} {$ENDIF};

{**
 *  Update a joystick's LED color.
 *
 *  \param joystick The joystick to update
 *  \param red The intensity of the red LED
 *  \param green The intensity of the green LED
 *  \param blue The intensity of the blue LED
 *
 *  \return 0, or -1 if this joystick does not have a modifiable LED
 *}
function SDL_JoystickSetLED(joystick: PSDL_Joystick; red: UInt8; green: UInt8; blue: UInt8): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickSetLED' {$ENDIF} {$ENDIF};

{**
 *  Close a joystick previously opened with SDL_JoystickOpen().
 *}
procedure SDL_JoystickClose(joystick: PSDL_Joystick); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickClose' {$ENDIF} {$ENDIF};

{**
 *  Return the battery level of this joystick
 *}
function SDL_JoystickCurrentPowerLevel(joystick: PSDL_Joystick): TSDL_JoystickPowerLevel; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickCurrentPowerLevel' {$ENDIF} {$ENDIF};

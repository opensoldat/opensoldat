// based on SDL_sensor.h


{**
 *  \brief SDL_sensor.h
 *
 *  In order to use these functions, SDL_Init() must have been called
 *  with the ::SDL_INIT_SENSOR flag.  This causes SDL to scan the system
 *  for sensors, and load appropriate drivers.
 *}
type
	TSDL_Sensor = record end;
	PSDL_Sensor = ^TSDL_Sensor;

{**
 * This is a unique ID for a sensor for the time it is connected to the system,
 * and is never reused for the lifetime of the application.
 *
 * The ID value starts at 0 and increments from there. The value -1 is an invalid ID.
 *}
type
	TSDL_SensorID = type SInt32;

{**
 * The different sensors defined by SDL
 *
 * Additional sensors may be available, using platform dependent semantics.
 *
 * Hare are the additional Android sensors:
 * https://developer.android.com/reference/android/hardware/SensorEvent.html#values
 *}
type
	TSDL_SensorType = type SInt32;

const
	SDL_SENSOR_INVALID = TSDL_SensorType(-1);    {**< Returned for an invalid sensor *}
	SDL_SENSOR_UNKNOWN = TSDL_SensorType(0);     {**< Unknown sensor type *}
	SDL_SENSOR_ACCEL   = TSDL_SensorType(1);     {**< Accelerometer *}
	SDL_SENSOR_GYRO    = TSDL_SensorType(2);     {**< Gyroscope *}

{**
 * Accelerometer sensor
 *
 * The accelerometer returns the current acceleration in SI meters per
 * second squared. This measurement includes the force of gravity, so
 * a device at rest will have an value of SDL_STANDARD_GRAVITY away
 * from the center of the earth.
 *
 * values[0]: Acceleration on the x axis
 * values[1]: Acceleration on the y axis
 * values[2]: Acceleration on the z axis
 *
 * For phones held in portrait mode and game controllers held in front of you,
 * the axes are defined as follows:
 * -X ... +X : left ... right
 * -Y ... +Y : bottom ... top
 * -Z ... +Z : farther ... closer
 *
 * The axis data is not changed when the phone is rotated.
 *
 * \sa SDL_GetDisplayOrientation()
 *}
const
	SDL_STANDARD_GRAVITY = 9.80665;

{**
 * Gyroscope sensor
 *
 * The gyroscope returns the current rate of rotation in radians per second.
 * The rotation is positive in the counter-clockwise direction. That is,
 * an observer looking from a positive location on one of the axes would
 * see positive rotation on that axis when it appeared to be rotating
 * counter-clockwise.
 *
 * values[0]: Angular speed around the x axis (pitch)
 * values[1]: Angular speed around the y axis (yaw)
 * values[2]: Angular speed around the z axis (roll)
 *
 * For phones held in portrait mode and game controllers held in front of you,
 * the axes are defined as follows:
 * -X ... +X : left ... right
 * -Y ... +Y : bottom ... top
 * -Z ... +Z : farther ... closer
 *
 * The axis data is not changed when the phone or controller is rotated.
 *
 * \sa SDL_GetDisplayOrientation()
 *}

{--- Function prototypes ---}

{**
 * Locking for multi-threaded access to the sensor API
 *
 * If you are using the sensor API or handling events from multiple threads
 * you should use these locking functions to protect access to the sensors.
 *
 * In particular, you are guaranteed that the sensor list won't change, so
 * the API functions that take a sensor index will be valid, and sensor
 * events will not be delivered.
 *}
procedure SDL_LockSensors(); cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockSensors' {$ENDIF} {$ENDIF};
procedure SDL_UnlockSensors(); cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UnlockSensors' {$ENDIF} {$ENDIF};

{**
 *  \brief Count the number of sensors attached to the system right now
 *}
function SDL_NumSensors(): SInt32; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_NumSensors' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the implementation dependent name of a sensor.
 *
 *  This can be called before any sensors are opened.
 *
 *  \return The sensor name, or NIL if device_index is out of range.
 *}
function SDL_SensorGetDeviceName(device_index: SInt32): PChar; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetDeviceName' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the type of a sensor.
 *
 *  This can be called before any sensors are opened.
 *
 *  \return The sensor type, or SDL_SENSOR_INVALID if device_index is out of range.
 *}
function SDL_SensorGetDeviceType(device_index: SInt32): TSDL_SensorType; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetDeviceType' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the platform dependent type of a sensor.
 *
 *  This can be called before any sensors are opened.
 *
 *  \return The sensor platform dependent type, or -1 if device_index is out of range.
 *}
function SDL_SensorGetDeviceNonPortableType(device_index: SInt32): SInt32; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetDeviceNonPortableType' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the instance ID of a sensor.
 *
 *  This can be called before any sensors are opened.
 *
 *  \return The sensor instance ID, or -1 if device_index is out of range.
 *}
function SDL_SensorGetDeviceInstanceID(device_index: SInt32): TSDL_SensorID; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetDeviceInstanceID' {$ENDIF} {$ENDIF};

{**
 *  \brief Open a sensor for use.
 *
 *  The index passed as an argument refers to the N'th sensor on the system.
 *
 *  \return A sensor identifier, or NIL if an error occurred.
 *}
function SDL_SensorOpen(device_index: SInt32): PSDL_Sensor; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorOpen' {$ENDIF} {$ENDIF};

{**
 * Return the SDL_Sensor associated with an instance id.
 *}
function SDL_SensorFromInstanceID(instance_id: TSDL_SensorID): PSDL_Sensor; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorFromInstanceID' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the implementation dependent name of a sensor.
 *
 *  \return The sensor name, or NIL if the sensor is NIL.
 *}
function SDL_SensorGetName(sensor: PSDL_Sensor): PChar; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetName' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the type of a sensor.
 *
 *  This can be called before any sensors are opened.
 *
 *  \return The sensor type, or SDL_SENSOR_INVALID if the sensor is NIL.
 *}
function SDL_SensorGetType(sensor: PSDL_Sensor): TSDL_SensorType; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetType' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the platform dependent type of a sensor.
 *
 *  This can be called before any sensors are opened.
 *
 *  \return The sensor platform dependent type, or -1 if the sensor is NIL.
 *}
function SDL_SensorGetNonPortableType(sensor: PSDL_Sensor): SInt32; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetNonPortableType' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the instance ID of a sensor.
 *
 *  This can be called before any sensors are opened.
 *
 *  \return The sensor instance ID, or -1 if the sensor is NIL.
 *}
function SDL_SensorGetInstanceID(sensor: PSDL_Sensor): TSDL_SensorID; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetInstanceID' {$ENDIF} {$ENDIF};

{**
 *  Get the current state of an opened sensor.
 *
 *  The number of values and interpretation of the data is sensor dependent.
 *
 *  \param sensor The sensor to query
 *  \param data A pointer filled with the current sensor state
 *  \param num_values The number of values to write to data
 *
 *  \return 0 or -1 if an error occurred.
 *}
function SDL_SensorGetData(sensor: PSDL_Sensor; data: PSingle; num_values: SInt32): SInt32; cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorGetData' {$ENDIF} {$ENDIF};

{**
 *  Close a sensor previously opened with SDL_SensorOpen()
 *}
procedure SDL_SensorClose(sensor: PSDL_Sensor); cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorClose' {$ENDIF} {$ENDIF};

{**
 *  Update the current state of the open sensors.
 *
 *  This is called automatically by the event loop if sensor events are enabled.
 *
 *  This needs to be called from the thread that initialized the sensor subsystem.
 *}
procedure SDL_SensorUpdate(); cdecl;
	external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SensorUpdate' {$ENDIF} {$ENDIF};

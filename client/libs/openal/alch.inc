(*
 * The ALCAPI, ALCAPIENTRY, and ALC_INVALID macros are deprecated, but are
 * included for applications porting code from AL 1.0
 *)

{ ALCAPI ALC_API
  ALCAPIENTRY ALC_APIENTRY
  ALC_INVALID                       = 0;


  ALC_VERSION_0_1                   = 1;}

type
  PALCdevice                        = ^ALCdevice;
  ALCdevice                         = record
  end;

  PALCcontext                       = ^ALCcontext;
  ALCcontext                        = record
  end;


(** 8-bit boolean *)
  ALCboolean                        = cbool;
  PALCboolean                       = pcbool;

(** character *)
  ALCchar                           = cchar;
  PALCchar                          = pcchar;

(** signed 8-bit 2's complement integer *)
  ALCbyte                           = cint8;
  PALCbyte                          = pcint8;

(** unsigned 8-bit integer *)
  ALCubyte                          = cuint8;
  PALCubyte                         = pcuint8;

(** signed 16-bit 2's complement integer *)
  ALCshort                          = cint16;
  PALCshort                         = pcint16;

(** unsigned 16-bit integer *)
  ALCushort                         = cuint16;
  PALCushort                        = pcuint16;

(** signed 32-bit 2's complement integer *)
  ALCint                            = cint32;
  PALCint                           = pcint32;

(** unsigned 32-bit integer *)
  ALCuint                           = cuint32;
  PALCuint                          = pcuint32;

(** non-negative 32-bit binary integer size *)
  ALCsizei                          = cint32;
  PALCsizei                         = pcint32;

(** enumerated 32-bit value *)
  ALCenum                           = cint32;
  PALCenum                          = pcint32;

(** 32-bit IEEE754 floating-point *)
  ALCfloat                          = cfloat;
  PALCfloat                         = pcfloat;

(** 64-bit IEEE754 floating-point *)
  ALCdouble                         = cdouble;
  PALCdouble                        = pcdouble;

(** void type (for opaque pointers only) *)
  PALCvoid                          = pointer;


(* Enumerant values begin at column 50. No tabs. *)
const

(* Boolean False. *)
  ALC_FALSE                         = 0;

(* Boolean True. *)
  ALC_TRUE                          = 1;

(**
 * followed by <int> Hz
 *)
  ALC_FREQUENCY                     = $1007;

(**
 * followed by <int> Hz
 *)
  ALC_REFRESH                       = $1008;

(**
 * followed by AL_TRUE, AL_FALSE
 *)
  ALC_SYNC                          = $1009;

(**
 * followed by <int> Num of requested Mono (3D) Sources
 *)
  ALC_MONO_SOURCES                  = $1010;

(**
 * followed by <int> Num of requested Stereo Sources
 *)
  ALC_STEREO_SOURCES                = $1011;

(**
 * errors
 *)

(**
 * No error
 *)
  ALC_NO_ERROR                      = ALC_FALSE;

(**
 * No device
 *)
  ALC_INVALID_DEVICE                = $A001;

(**
 * invalid context ID
 *)
  ALC_INVALID_CONTEXT               = $A002;

(**
 * bad enum
 *)
  ALC_INVALID_ENUM                  = $A003;

(**
 * bad value
 *)
  ALC_INVALID_VALUE                 = $A004;

(**
 * Out of memory.
 *)
  ALC_OUT_OF_MEMORY                 = $A005;


(**
 * The Specifier string for default device
 *)
  ALC_DEFAULT_DEVICE_SPECIFIER      = $1004;
  ALC_DEVICE_SPECIFIER              = $1005;
  ALC_EXTENSIONS                    = $1006;

  ALC_MAJOR_VERSION                 = $1000;
  ALC_MINOR_VERSION                 = $1001;

  ALC_ATTRIBUTES_SIZE               = $1002;
  ALC_ALL_ATTRIBUTES                = $1003;

(**
 * Capture extension
 *)
  ALC_CAPTURE_DEVICE_SPECIFIER      = $310;
  ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER = $311;
  ALC_CAPTURE_SAMPLES               = $312;


(*
 * Context Management
 *)
function alcCreateContext(device: PALCdevice; const attrlist: PALCint): PALCcontext; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

function alcMakeContextCurrent(context: PALCcontext): ALCboolean; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

procedure alcProcessContext(context: PALCcontext); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

procedure alcSuspendContext(context: PALCcontext); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

procedure alcDestroyContext(context: PALCcontext); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

function alcGetCurrentContext: PALCcontext; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

function alcGetContextsDevice(context: PALCcontext): PALCdevice; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};


(*
 * Device Management
 *)
function alcOpenDevice(const devicename: PALCchar): PALCdevice; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

function alcCloseDevice(device: PALCdevice): ALCboolean; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};


(*
 * Error support.
 * Obtain the most recent Context error
 *)
function alcGetError(device: PALCdevice): ALCenum; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};


(* 
 * Extension support.
 * Query for the presence of an extension, and obtain any appropriate
 * function pointers and enum values.
 *)
function alcIsExtensionPresent(device: PALCdevice; const extname: PALCchar): ALCboolean; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

function alcGetProcAddress(device: PALCdevice; const funcname: PALCchar): pointer; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

function alcGetEnumValue(device: PALCdevice; const enumname: PALCchar): ALCenum; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};


(*
 * Query functions
 *)
function alcGetString(device: PALCdevice; param: ALCenum): PALCchar; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

procedure alcGetIntegerv(device: PALCdevice; param: ALCenum; size: ALCsizei; data: PALCint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};


(*
 * Capture functions
 *)
function alcCaptureOpenDevice(const devicename: PALCchar; frequency: ALCuint; format: ALCenum; buffersize: ALCsizei): PALCdevice; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

function alcCaptureCloseDevice(device: PALCdevice): ALCboolean; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

procedure alcCaptureStart(device: PALCdevice); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

procedure alcCaptureStop(device: PALCdevice); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

procedure alcCaptureSamples(device: PALCdevice; buffer: PALCvoid; samples: ALCsizei); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

(*
 * Pointer-to-function types, useful for dynamically getting ALC entry points.
 *)
{
type
  LPALCCREATECONTEXT                = function(ALCdevice *device, const ALCint *attrlist): PALCcontext; cdecl;
  LPALCMAKECONTEXTCURRENT           = function( ALCcontext *context ): ALCboolean; cdecl;
  LPALCPROCESSCONTEXT               = procedure( ALCcontext *context ); cdecl;
  LPALCSUSPENDCONTEXT               = procedure( ALCcontext *context ); cdecl;
  LPALCDESTROYCONTEXT               = procedure( ALCcontext *context ); cdecl;
  LPALCGETCURRENTCONTEXT            = function( ALCvoid ): PALCcontext; cdecl;
  LPALCGETCONTEXTSDEVICE            = function( ALCcontext *context ): PALCdevice; cdecl;
  LPALCOPENDEVICE                   = function( const ALCchar *devicename ): PALCdevice; cdecl;
  LPALCCLOSEDEVICE                  = function( ALCdevice *device ): ALCboolean; cdecl;
  LPALCGETERROR                     = function( ALCdevice *device ): ALCenum; cdecl;
  LPALCISEXTENSIONPRESENT           = function( ALCdevice *device, const ALCchar *extname ): ALCboolean; cdecl;
  LPALCGETPROCADDRESS               = function(ALCdevice *device, const ALCchar *funcname ): pointer; cdecl;
  LPALCGETENUMVALUE                 = function(ALCdevice *device, const ALCchar *enumname ): ALCenum; cdecl;
  LPALCGETSTRING                    = function( ALCdevice *device, ALCenum param ): PALCchar; cdecl;
  LPALCGETINTEGERV                  = procedure(ALCdevice *device, ALCenum param, ALCsizei size, ALCint *dest ); cdecl;
  LPALCCAPTUREOPENDEVICE            = function( const ALCchar *devicename, ALCuint frequency, ALCenum format, ALCsizei buffersize ): PALCdevice; cdecl;
  LPALCCAPTURECLOSEDEVICE           = function( ALCdevice *device ): ALCboolean; cdecl;
  LPALCCAPTURESTART                 = procedure( ALCdevice *device ); cdecl;
  LPALCCAPTURESTOP                  = procedure( ALCdevice *device ); cdecl;
  LPALCCAPTURESAMPLES               = procedure( ALCdevice *device, ALCvoid *buffer, ALCsizei samples ); cdecl;
}
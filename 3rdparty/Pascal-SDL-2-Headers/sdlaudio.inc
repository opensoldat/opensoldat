//from sdl_audio.h
 {**
   *   Audio format flags.
   *
   *  These are what the 16 bits in SDL_AudioFormat currently mean...
   *  (Unspecified bits are always zero).
   *
   *
      ++-----------------------sample is signed if set
      ||
      ||       ++-----------sample is bigendian if set
      ||       ||
      ||       ||          ++---sample is float if set
      ||       ||          ||
      ||       ||          || +---sample bit size---+
      ||       ||          || |                     |
      15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
   *
   *  There are macros in SDL 2.0 and later to query these bits.
   *}
type
  TSDL_AudioFormat = UInt16;

  {**
   *   Audio flags
   *}
const
  SDL_AUDIO_MASK_BITSIZE      = ($FF);
  SDL_AUDIO_MASK_DATATYPE     = (1 shl 8);
  SDL_AUDIO_MASK_ENDIAN       = (1 shl 12);
  SDL_AUDIO_MASK_SIGNED       = (1 shl 15);

function SDL_AUDIO_BITSIZE(x: Cardinal): Cardinal;
function SDL_AUDIO_ISFLOAT(x: Cardinal): Cardinal;
function SDL_AUDIO_ISBIGENDIAN(x: Cardinal): Cardinal;
function SDL_AUDIO_ISSIGNED(x: Cardinal): Cardinal;
function SDL_AUDIO_ISINT(x: Cardinal): Cardinal;
function SDL_AUDIO_ISLITTLEENDIAN(x: Cardinal): Cardinal;
function SDL_AUDIO_ISUNSIGNED(x: Cardinal): Cardinal;

  {**
   *   Audio format flags
   *
   *  Defaults to LSB byte order.
   *}
const
  AUDIO_U8      = $0008;  {**< Unsigned 8-bit samples *}
  AUDIO_S8      = $8008;  {**< Signed 8-bit samples *}
  AUDIO_U16LSB  = $0010;  {**< Unsigned 16-bit samples *}
  AUDIO_S16LSB  = $8010;  {**< Signed 16-bit samples *}
  AUDIO_U16MSB  = $1010;  {**< As above, but big-endian byte order *}
  AUDIO_S16MSB  = $9010;  {**< As above, but big-endian byte order *}
  AUDIO_U16     = AUDIO_U16LSB;
  AUDIO_S16     = AUDIO_S16LSB;

  {**
   *   int32 support
   *}
const
  AUDIO_S32LSB  = $8020;  {**< 32-bit integer samples *}
  AUDIO_S32MSB  = $9020;  {**< As above, but big-endian byte order *}
  AUDIO_S32     = AUDIO_S32LSB;

  {**
   *   float32 support
   *}
const
  AUDIO_F32LSB  = $8120;  {**< 32-bit floating point samples *}
  AUDIO_F32MSB  = $9120;  {**< As above, but big-endian byte order *}
  AUDIO_F32     = AUDIO_F32LSB;

  {**
   *   Native audio byte ordering
   *}
{$IFDEF FPC}
   {$IF DEFINED(ENDIAN_LITTLE)}
      AUDIO_U16SYS = AUDIO_U16LSB;
      AUDIO_S16SYS = AUDIO_S16LSB;
      AUDIO_S32SYS = AUDIO_S32LSB;
      AUDIO_F32SYS = AUDIO_F32LSB;
   {$ELSEIF DEFINED(ENDIAN_BIG)}
      AUDIO_U16SYS = AUDIO_U16MSB;
      AUDIO_S16SYS = AUDIO_S16MSB;
      AUDIO_S32SYS = AUDIO_S32MSB;
      AUDIO_F32SYS = AUDIO_F32MSB;
   {$ELSE}
      {$FATAL Cannot determine endianness.}
   {$IFEND}
{$ENDIF}

  {**
   *   Allow change flags
   *
   *  Which audio format changes are allowed when opening a device.
   *}
const
  SDL_AUDIO_ALLOW_FREQUENCY_CHANGE  = $00000001;
  SDL_AUDIO_ALLOW_FORMAT_CHANGE     = $00000002;
  SDL_AUDIO_ALLOW_CHANNELS_CHANGE   = $00000004;
  SDL_AUDIO_ALLOW_ANY_CHANGE        = (SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or
                                       SDL_AUDIO_ALLOW_FORMAT_CHANGE or
                                                SDL_AUDIO_ALLOW_CHANNELS_CHANGE);

  {*Audio flags*}

  {**
   *  This function is called when the audio device needs more data.
   *
   *   userdata An application-specific parameter saved in
   *                  the SDL_AudioSpec structure
   *   stream A pointer to the audio data buffer.
   *   len    The length of that buffer in bytes.
   *
   *  Once the callback returns, the buffer will no longer be valid.
   *  Stereo samples are stored in a LRLRLR ordering.
   *}
type
  TSDL_AudioCallback = procedure(userdata: Pointer; stream: PUInt8; len: Integer) cdecl;

  {**
   *  The calculated values in this structure are calculated by SDL_OpenAudio().
   *}
type
  PSDL_AudioSpec = ^TSDL_AudioSpec;
  TSDL_AudioSpec = record
    freq: Integer;                {**< DSP frequency -- samples per second *}
    format: TSDL_AudioFormat;     {**< Audio data format *}
    channels: UInt8;              {**< Number of channels: 1 mono, 2 stereo *}
    silence: UInt8;               {**< Audio buffer silence value (calculated) *}
    samples: UInt16;              {**< Audio buffer size in samples (power of 2) *}
    padding: UInt16;              {**< Necessary for some compile environments *}
    size: UInt32;                 {**< Audio buffer size in bytes (calculated) *}
    callback: TSDL_AudioCallback;
    userdata: Pointer;
  end;

  PSDL_AudioCVT = ^TSDL_AudioCVT;
  TSDL_AudioFilter = procedure(cvt: PSDL_AudioCVT; format: TSDL_AudioFormat) cdecl;

  {**
   *  A structure to hold a set of audio conversion filters and buffers.
   *}
  TSDL_AudioCVT = record
    needed: Integer;                           {**< Set to 1 if conversion possible *}
    src_format: TSDL_AudioFormat;              {**< Source audio format *}
    dst_format: TSDL_AudioFormat;                {**< Target audio format *}
    rate_incr: Double;                        {**< Rate conversion increment *}
    buf: PUInt8;                             {**< Buffer to hold entire audio data *}
    len: Integer;                               {**< Length of original audio buffer *}
    len_cvt: Integer;                           {**< Length of converted audio buffer *}
    len_mult: Integer;                          {**< buffer must be len*len_mult big *}
    len_ratio: Double;                        {**< Given len, final size is len*len_ratio *}
    filters: array[0..9] of TSDL_AudioFilter; {**< Filter list *}
    filter_index: Integer;                      {**< Current audio conversion function *}
  end;


  {* Function prototypes *}

  {**
   *   Driver discovery functions
   *
   *  These functions return the list of built in audio drivers, in the
   *  order that they are normally initialized by default.
   *}

function SDL_GetNumAudioDrivers: Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetNumAudioDrivers' {$ENDIF} {$ENDIF};
function SDL_GetAudioDriver(index: Integer): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetAudioDriver' {$ENDIF} {$ENDIF};

  {**
   *   Initialization and cleanup
   *
   *  These functions are used internally, and should not be used unless
   *  you have a specific need to specify the audio driver you want to
   *  use.  You should normally use SDL_Init() or SDL_InitSubSystem().
   *}
 
function SDL_AudioInit(driver_name: PAnsiChar): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_AudioInit' {$ENDIF} {$ENDIF};
procedure SDL_AudioQuit cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_AudioQuit' {$ENDIF} {$ENDIF};

  {**
   *  This function returns the name of the current audio driver, or NULL
   *  if no driver has been initialized.
   *}
function SDL_GetCurrentAudioDriver: PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetCurrentAudioDriver' {$ENDIF} {$ENDIF};

  {**
   *  This function opens the audio device with the desired parameters, and
   *  returns 0 if successful, placing the actual hardware parameters in the
   *  structure pointed to by obtained.  If obtained is NULL, the audio
   *  data passed to the callback function will be guaranteed to be in the
   *  requested format, and will be automatically converted to the hardware
   *  audio format if necessary.  This function returns -1 if it failed
   *  to open the audio device, or couldn't set up the audio thread.
   *
   *  When filling in the desired audio spec structure,
   *    - desired->freq should be the desired audio frequency in samples-per-
   *      second.
   *    - desired->format should be the desired audio format.
   *    - desired->samples is the desired size of the audio buffer, in
   *      samples.  This number should be a power of two, and may be adjusted by
   *      the audio driver to a value more suitable for the hardware.  Good values
   *      seem to range between 512 and 8096 inclusive, depending on the
   *      application and CPU speed.  Smaller values yield faster response time,
   *      but can lead to underflow if the application is doing heavy processing
   *      and cannot fill the audio buffer in time.  A stereo sample consists of
   *      both right and left channels in LR ordering.
   *      Note that the number of samples is directly related to time by the
   *      following formula:  ms := (samples*1000)/freq;
   *    - desired->size is the size in bytes of the audio buffer, and is
   *      calculated by SDL_OpenAudio().
   *    - desired->silence is the value used to set the buffer to silence,
   *      and is calculated by SDL_OpenAudio().
   *    - desired->callback should be set to a function that will be called
   *      when the audio device is ready for more data.  It is passed a pointer
   *      to the audio buffer, and the length in bytes of the audio buffer.
   *      This function usually runs in a separate thread, and so you should
   *      protect data structures that it accesses by calling SDL_LockAudio()
   *      and SDL_UnlockAudio() in your code.
   *    - desired->userdata is passed as the first parameter to your callback
   *      function.
   *
   *  The audio device starts out playing silence when it's opened, and should
   *  be enabled for playing by calling SDL_PauseAudio(0) when you are ready
   *  for your audio callback function to be called.  Since the audio driver
   *  may modify the requested size of the audio buffer, you should allocate
   *  any local mixing buffers after you open the audio device.
   *}
function SDL_OpenAudio(desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_OpenAudio' {$ENDIF} {$ENDIF};

  {**
   *  SDL Audio Device IDs.
   *
   *  A successful call to SDL_OpenAudio() is always device id 1, and legacy
   *  SDL audio APIs assume you want this device ID. SDL_OpenAudioDevice() calls
   *  always returns devices >= 2 on success. The legacy calls are good both
   *  for backwards compatibility and when you don't care about multiple,
   *  specific, or capture devices.
   *}
type
  TSDL_AudioDeviceID = UInt32;

  {**
   *  Get the number of available devices exposed by the current driver.
   *  Only valid after a successfully initializing the audio subsystem.
   *  Returns -1 if an explicit list of devices can't be determined; this is
   *  not an error. For example, if SDL is set up to talk to a remote audio
   *  server, it can't list every one available on the Internet, but it will
   *  still allow a specific host to be specified to SDL_OpenAudioDevice().
   *
   *  In many common cases, when this function returns a value <= 0, it can still
   *  successfully open the default device (NULL for first argument of
   *  SDL_OpenAudioDevice()).
   *}
function SDL_GetNumAudioDevices(iscapture: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetNumAudioDevices' {$ENDIF} {$ENDIF};

  {**
   *  Get the human-readable name of a specific audio device.
   *  Must be a value between 0 and (number of audio devices-1).
   *  Only valid after a successfully initializing the audio subsystem.
   *  The values returned by this function reflect the latest call to
   *  SDL_GetNumAudioDevices(); recall that function to redetect available
   *  hardware.
   *
   *  The string returned by this function is UTF-8 encoded, read-only, and
   *  managed internally. You are not to free it. If you need to keep the
   *  string for any length of time, you should make your own copy of it, as it
   *  will be invalid next time any of several other SDL functions is called.
   *}
function SDL_GetAudioDeviceName(index: Integer; iscapture: Integer): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetAudioDeviceName' {$ENDIF} {$ENDIF};

  {**
   *  Open a specific audio device. Passing in a device name of NULL requests
   *  the most reasonable default (and is equivalent to calling SDL_OpenAudio()).
   *
   *  The device name is a UTF-8 string reported by SDL_GetAudioDeviceName(), but
   *  some drivers allow arbitrary and driver-specific strings, such as a
   *  hostname/IP address for a remote audio server, or a filename in the
   *  diskaudio driver.
   *
   *   0 on error, a valid device ID that is >= 2 on success.
   *
   *  SDL_OpenAudio(), unlike this function, always acts on device ID 1.
   *}
function SDL_OpenAudioDevice(device: PAnsiChar;
                             iscapture: Integer;
                             desired: PSDL_AudioSpec;
                             obtained: PSDL_AudioSpec;
                                  allowed_changes: Integer): TSDL_AudioDeviceID cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_OpenAudioDevice' {$ENDIF} {$ENDIF};

  {**
   *   Audio state
   *
   *  Get the current audio state.
   *}

type
  TSDL_AudioStatus = (SDL_AUDIO_STOPPED,SDL_AUDIO_PLAYING,SDL_AUDIO_PAUSED);

function SDL_GetAudioStatus: TSDL_AudioStatus cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetAudioStatus' {$ENDIF} {$ENDIF};

function SDL_GetAudioDeviceStatus(dev: TSDL_AudioDeviceID): TSDL_AudioStatus cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetAudioDeviceStatus' {$ENDIF} {$ENDIF};
  {*Audio State*}

  {**
   *   Pause audio functions
   *
   *  These functions pause and unpause the audio callback processing.
   *  They should be called with a parameter of 0 after opening the audio
   *  device to start playing sound.  This is so you can safely initialize
   *  data for your callback function after opening the audio device.
   *  Silence will be written to the audio device during the pause.
   *}

procedure SDL_PauseAudio(pause_on: Integer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_PauseAudio' {$ENDIF} {$ENDIF};
procedure SDL_PauseAudioDevice(dev: TSDL_AudioDeviceID; pause_on: Integer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_PauseAudioDevice' {$ENDIF} {$ENDIF};
  {*Pause audio functions*}

  {**
   *  This function loads a WAVE from the data source, automatically freeing
   *  that source if freesrc is non-zero.  For example, to load a WAVE file,
   *  you could do:
   *
   *      SDL_LoadWAV_RW(SDL_RWFromFile("sample.wav", "rb"), 1, ...);
   *
   *
   *  If this function succeeds, it returns the given SDL_AudioSpec,
   *  filled with the audio data format of the wave data, and sets
   *   *audio_buf to a malloc()'d buffer containing the audio data,
   *  and sets  *audio_len to the length of that audio buffer, in bytes.
   *  You need to free the audio buffer with SDL_FreeWAV() when you are
   *  done with it.
   *
   *  This function returns NULL and sets the SDL error message if the
   *  wave file cannot be opened, uses an unknown data format, or is
   *  corrupt.  Currently raw and MS-ADPCM WAVE files are supported.
   *}
function SDL_LoadWAV_RW(src: PSDL_RWops; freesrc: Integer; spec: PSDL_AudioSpec; audio_buf: PPUInt8; audio_len: PUInt32): PSDL_AudioSpec cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LoadWAV_RW' {$ENDIF} {$ENDIF};

  {**
   *  Loads a WAV from a file.
   *  Compatibility convenience function.
   *}

   function SDL_LoadWAV(_file: PAnsiChar; spec: PSDL_AudioSpec; audio_buf: PPUInt8; audio_len: PUInt32): PSDL_AudioSpec;

  {**
   *  This function frees data previously allocated with SDL_LoadWAV_RW()
   *}
  procedure SDL_FreeWAV(audio_buf: PUInt8) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_FreeWAV' {$ENDIF} {$ENDIF};

  {**
   *  This function takes a source format and rate and a destination format
   *  and rate, and initializes the cvt structure with information needed
   *  by SDL_ConvertAudio() to convert a buffer of audio data from one format
   *  to the other.
   *
   *   -1 if the format conversion is not supported, 0 if there's
   *  no conversion needed, or 1 if the audio filter is set up.
   *}
function SDL_BuildAudioCVT(cvt: PSDL_AudioCVT;
                           src_format: TSDL_AudioFormat;
                           src_channels: UInt8;
                           src_rate: Integer;
                           dst_format: TSDL_AudioFormat;
                           dst_channels: UInt8;
                           dst_rate: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_BuildAudioCVT' {$ENDIF} {$ENDIF};

{**
 *  Once you have initialized the cvt structure using SDL_BuildAudioCVT(),
 *  created an audio buffer cvt->buf, and filled it with cvt->len bytes of
 *  audio data in the source format, this function will convert it in-place
 *  to the desired format.
 *
 *  The data conversion may expand the size of the audio data, so the buffer
 *  cvt->buf should be allocated after the cvt structure is initialized by
 *  SDL_BuildAudioCVT(), and should be cvt->len*cvt->len_mult bytes long.
 *}
function SDL_ConvertAudio(cvt: PSDL_AudioCVT): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ConvertAudio' {$ENDIF} {$ENDIF};

const
  SDL_MIX_MAXVOLUME = 128;

  {**
   *  This takes two audio buffers of the playing audio format and mixes
   *  them, performing addition, volume adjustment, and overflow clipping.
   *  The volume ranges from 0 - 128, and should be set to ::SDL_MIX_MAXVOLUME
   *  for full audio volume.  Note this does not change hardware volume.
   *  This is provided for convenience -- you can mix your own audio data.
   *}
procedure SDL_MixAudio(dst: PUInt8; src: PUInt8; len: UInt32; volume: Integer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_MixAudio' {$ENDIF} {$ENDIF};

  {**
   *  This works like SDL_MixAudio(), but you specify the audio format instead of
   *  using the format of audio device 1. Thus it can be used when no audio
   *  device is open at all.
   *}
procedure SDL_MixAudioFormat(dst: PUInt8; src: PUInt8; format: TSDL_AudioFormat; len: UInt32; volume: Integer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_MixAudioFormat' {$ENDIF} {$ENDIF};

  {**
   *  Queue more audio on non-callback devices.
   *
   *  SDL offers two ways to feed audio to the device: you can either supply a
   *  callback that SDL triggers with some frequency to obtain more audio
   *  (pull method), or you can supply no callback, and then SDL will expect
   *  you to supply data at regular intervals (push method) with this function.
   *
   *  There are no limits on the amount of data you can queue, short of
   *  exhaustion of address space. Queued data will drain to the device as
   *  necessary without further intervention from you. If the device needs
   *  audio but there is not enough queued, it will play silence to make up
   *  the difference. This means you will have skips in your audio playback
   *  if you aren't routinely queueing sufficient data.
   *
   *  This function copies the supplied data, so you are safe to free it when
   *  the function returns. This function is thread-safe, but queueing to the
   *  same device from two threads at once does not promise which buffer will
   *  be queued first.
   *
   *  You may not queue audio on a device that is using an application-supplied
   *  callback; doing so returns an error. You have to use the audio callback
   *  or queue audio with this function, but not both.
   *
   *  You should not call SDL_LockAudio() on the device before queueing; SDL
   *  handles locking internally for this function.
   *
   *  \param dev The device ID to which we will queue audio.
   *  \param data The data to queue to the device for later playback.
   *  \param len The number of bytes (not samples!) to which (data) points.
   *  \return zero on success, -1 on error.
   *
   *  \sa SDL_GetQueuedAudioSize
   *  \sa SDL_ClearQueuedAudio
   *}
function SDL_QueueAudio(dev: TSDL_AudioDeviceID; data: Pointer; len: UInt32): SInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_QueueAudio' {$ENDIF} {$ENDIF};

  {**
   *  Dequeue more audio on non-callback devices.
   *
   *  (If you are looking to queue audio for output on a non-callback playback
   *  device, you want SDL_QueueAudio() instead. This will always return 0
   *  if you use it with playback devices.)
   *
   *  SDL offers two ways to retrieve audio from a capture device: you can
   *  either supply a callback that SDL triggers with some frequency as the
   *  device records more audio data, (push method), or you can supply no
   *  callback, and then SDL will expect you to retrieve data at regular
   *  intervals (pull method) with this function.
   *
   *  There are no limits on the amount of data you can queue, short of
   *  exhaustion of address space. Data from the device will keep queuing as
   *  necessary without further intervention from you. This means you will
   *  eventually run out of memory if you aren't routinely dequeueing data.
   *
   *  Capture devices will not queue data when paused; if you are expecting
   *  to not need captured audio for some length of time, use
   *  SDL_PauseAudioDevice() to stop the capture device from queueing more
   *  data. This can be useful during, say, level loading times. When
   *  unpaused, capture devices will start queueing data from that point,
   *  having flushed any capturable data available while paused.
   *
   *  This function is thread-safe, but dequeueing from the same device from
   *  two threads at once does not promise which thread will dequeued data
   *  first.
   *
   *  You may not dequeue audio from a device that is using an
   *  application-supplied callback; doing so returns an error. You have to use
   *  the audio callback, or dequeue audio with this function, but not both.
   *
   *  You should not call SDL_LockAudio() on the device before queueing; SDL
   *  handles locking internally for this function.
   *
   *  \param dev The device ID from which we will dequeue audio.
   *  \param data A pointer into where audio data should be copied.
   *  \param len The number of bytes (not samples!) to which (data) points.
   *  \return number of bytes dequeued, which could be less than requested.
   *
   *  \sa SDL_GetQueuedAudioSize
   *  \sa SDL_ClearQueuedAudio
   *}
function SDL_DequeueAudio(dev: TSDL_AudioDeviceID; data: Pointer; len:Uint32):Uint32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DequeueAudio' {$ENDIF} {$ENDIF};

  {**
   *  Get the number of bytes of still-queued audio.
   *
   *  This is the number of bytes that have been queued for playback with
   *  SDL_QueueAudio(), but have not yet been sent to the hardware.
   *
   *  Once we've sent it to the hardware, this function can not decide the exact
   *  byte boundary of what has been played. It's possible that we just gave the
   *  hardware several kilobytes right before you called this function, but it
   *  hasn't played any of it yet, or maybe half of it, etc.
   *
   *  You may not queue audio on a device that is using an application-supplied
   *  callback; calling this function on such a device always returns 0.
   *  You have to use the audio callback or queue audio with SDL_QueueAudio(),
   *  but not both.
   *
   *  You should not call SDL_LockAudio() on the device before querying; SDL
   *  handles locking internally for this function.
   *
   *  \param dev The device ID of which we will query queued audio size.
   *  \return Number of bytes (not samples!) of queued audio.
   *
   *  \sa SDL_QueueAudio
   *  \sa SDL_ClearQueuedAudio
   *}
function SDL_GetQueuedAudioSize(dev: TSDL_AudioDeviceID): UInt32; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetQueuedAudioSize' {$ENDIF} {$ENDIF};

  {**
   *  Drop any queued audio data waiting to be sent to the hardware.
   *
   *  Immediately after this call, SDL_GetQueuedAudioSize() will return 0 and
   *  the hardware will start playing silence if more audio isn't queued.
   *
   *  This will not prevent playback of queued audio that's already been sent
   *  to the hardware, as we can not undo that, so expect there to be some
   *  fraction of a second of audio that might still be heard. This can be
   *  useful if you want to, say, drop any pending music during a level change
   *  in your game.
   *
   *  You may not queue audio on a device that is using an application-supplied
   *  callback; calling this function on such a device is always a no-op.
   *  You have to use the audio callback or queue audio with SDL_QueueAudio(),
   *  but not both.
   *
   *  You should not call SDL_LockAudio() on the device before clearing the
   *  queue; SDL handles locking internally for this function.
   *
   *  This function always succeeds and thus returns void.
   *
   *  \param dev The device ID of which to clear the audio queue.
   *
   *  \sa SDL_QueueAudio
   *  \sa SDL_GetQueuedAudioSize
   *}
procedure SDL_ClearQueuedAudio(dev: TSDL_AudioDeviceID); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ClearQueuedAudio' {$ENDIF} {$ENDIF};

  {**
   *   Audio lock functions
   *
   *  The lock manipulated by these functions protects the callback function.
   *  During a SDL_LockAudio()/SDL_UnlockAudio() pair, you can be guaranteed that
   *  the callback function is not running.  Do not call these from the callback
   *  function or you will cause deadlock.
   *}

procedure SDL_LockAudio cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockAudio' {$ENDIF} {$ENDIF};
procedure SDL_LockAudioDevice(dev: TSDL_AudioDeviceID) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockAudioDevice' {$ENDIF} {$ENDIF};
procedure SDL_UnlockAudio cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Unlock' {$ENDIF} {$ENDIF};
procedure SDL_UnlockAudioDevice(dev: TSDL_AudioDeviceID) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UnlockAudioDevice' {$ENDIF} {$ENDIF};
  {*Audio lock functions*}

  {**
   *  This function shuts down audio processing and closes the audio device.
   *}
procedure SDL_CloseAudio cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CloseAudio' {$ENDIF} {$ENDIF};
procedure SDL_CloseAudioDevice(dev: TSDL_AudioDeviceID) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CloseAudioDevice' {$ENDIF} {$ENDIF};

  {**
   *  1 if audio device is still functioning, zero if not, -1 on error.
   *}
function SDL_AudioDeviceConnected(dev: TSDL_AudioDeviceID): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_AudioDeviceConnected' {$ENDIF} {$ENDIF};


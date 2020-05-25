(* AL_LOKI_IMA_ADPCM_format extension *)
const
  AL_FORMAT_IMA_ADPCM_MONO16_EXT            = $10000;
  AL_FORMAT_IMA_ADPCM_STEREO16_EXT          = $10001;

(* AL_LOKI_WAVE_format extension *)
const
  AL_FORMAT_WAVE_EXT                        = $10002;

(* AL_EXT_vorbis extension *)
const
  AL_FORMAT_VORBIS_EXT                      = $10003;

(* AL_LOKI_quadriphonic extension *)
const
  AL_FORMAT_QUAD8_LOKI                      = $10004;
  AL_FORMAT_QUAD16_LOKI                     = $10005;

(* AL_EXT_float32 extension *)
const
  AL_FORMAT_MONO_FLOAT32                    = $10010;
  AL_FORMAT_STEREO_FLOAT32                  = $10011;

(* AL_EXT_double extension *)
const
  AL_FORMAT_MONO_DOUBLE_EXT                 = $10012;
  AL_FORMAT_STEREO_DOUBLE_EXT               = $10013;

(* ALC_LOKI_audio_channel extension *)
const
  ALC_CHAN_MAIN_LOKI                        = $500001;
  ALC_CHAN_PCM_LOKI                         = $500002;
  ALC_CHAN_CD_LOKI                          = $500003;

(* ALC_ENUMERATE_ALL_EXT extension *)
const
  ALC_DEFAULT_ALL_DEVICES_SPECIFIER         = $1012;
  ALC_ALL_DEVICES_SPECIFIER                 = $1013;

(* AL_EXT_MCFORMATS extension *)
const
   AL_FORMAT_QUAD8                          = $1204;
   AL_FORMAT_QUAD16                         = $1205;
   AL_FORMAT_QUAD32                         = $1206;
   AL_FORMAT_REAR8                          = $1207;
   AL_FORMAT_REAR16                         = $1208;
   AL_FORMAT_REAR32                         = $1209;
   AL_FORMAT_51CHN8                         = $120A;
   AL_FORMAT_51CHN16                        = $120B;
   AL_FORMAT_51CHN32                        = $120C;
   AL_FORMAT_61CHN8                         = $120D;
   AL_FORMAT_61CHN16                        = $120E;
   AL_FORMAT_61CHN32                        = $120F;
   AL_FORMAT_71CHN8                         = $1210;
   AL_FORMAT_71CHN16                        = $1211;
   AL_FORMAT_71CHN32                        = $1212;

(* AL_EXT_IMA4 extension *)
const
   AL_FORMAT_MONO_IMA4                      = $1300;
   AL_FORMAT_STEREO_IMA4                    = $1301;

(* AL_EXT_buffer_sub_data extension *)
const
   AL_BYTE_RW_OFFSETS_EXT                   = $1031;
   AL_SAMPLE_RW_OFFSETS_EXT                 = $1032;
   AL_SEC_RW_OFFSETS_EXT                    = $1033;

procedure alBufferSubDataEXT(buffer :ALuint; format :ALenum; const data :PALvoid; offset :ALsizei; length :ALsizei); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

(* AL_EXT_STATIC_BUFFER extension *)
procedure alBufferDataStatic(buffer :ALint; format :ALenum; data :Pointer; size, freq :ALsizei); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

(* AL_EXT_sample_buffer_object extension *)
const
   AL_SAMPLE_SOURCE_EXT                     = $1040;
   AL_SAMPLE_SINK_EXT                       = $1041;
   AL_READ_ONLY_EXT                         = $1042;
   AL_WRITE_ONLY_EXT                        = $1043;
   AL_READ_WRITE_EXT                        = $1044;
   AL_STREAM_WRITE_EXT                      = $1045;
   AL_STREAM_READ_EXT                       = $1046;
   AL_STREAM_COPY_EXT                       = $1047;
   AL_STATIC_WRITE_EXT                      = $1048;
   AL_STATIC_READ_EXT                       = $1049;
   AL_STATIC_COPY_EXT                       = $104A;
   AL_DYNAMIC_WRITE_EXT                     = $104B;
   AL_DYNAMIC_READ_EXT                      = $104C;
   AL_DYNAMIC_COPY_EXT                      = $104D;

  procedure alGenDatabuffersEXT(n:ALsizei; puiBuffers:pALuint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alDeleteDatabuffersEXT(n:ALsizei; puiBuffers:pALuint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  function  alIsDatabufferEXT(uiBuffer:ALuint):ALboolean; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alDatabufferDataEXT(buffer:ALuint; data:pALvoid; size:ALsizei; usage:ALenum); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alDatabufferSubDataEXT(buffer:ALuint; start:ALuint; length:ALsizei; data:pALvoid); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alGetDatabufferSubDataEXT(buffer:ALuint; start:ALuint; length:ALsizei; data:pALvoid); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alDatabufferfEXT(buffer:ALuint; eParam:ALenum; flValue:ALfloat); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alDatabufferfvEXT(buffer:ALuint; eParam:ALenum; flValues:pALfloat); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alDatabufferiEXT(buffer:ALuint; eParam:ALenum; lValue:ALint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alDatabufferivEXT(buffer:ALuint; eParam:ALenum; plValues:pALint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alGetDatabufferfEXT(buffer:ALuint; eParam:ALenum; pflValue:pALfloat); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alGetDatabufferfvEXT(buffer:ALuint; eParam:ALenum; pflValues:pALfloat); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alGetDatabufferiEXT(buffer:ALuint; eParam:ALenum; plValue:pALint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alGetDatabufferivEXT(buffer:ALuint; eParam:ALenum; plValues:pALint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alSelectDatabufferEXT(target:ALenum; uiBuffer:ALuint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  function  alMapDatabufferEXT(uiBuffer:ALuint; start:ALuint; length:ALsizei; access:ALenum):pALvoid; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
  procedure alUnmapDatabufferEXT(uiBuffer:ALuint); cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

(* ALC_EXT_disconnect extension *)
const
   ALC_CONNECTED                            = $0313;

(* ALC_EXT_thread_local_context extension *)

function alcMakeCurrent(context :PALCcontext) :ALCboolean; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};
function alcGetThreadContext() :PALCcontext; cdecl; external {$IFDEF DYNLINK}openallib{$ENDIF};

(* AL_EXT_source_distance_model extension *)
const
   AL_SOURCE_DISTANCE_MODEL                 = $0200;

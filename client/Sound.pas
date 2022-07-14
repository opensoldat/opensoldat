{*******************************************************}
{                                                       }
{       Sound Unit for OPENSOLDAT                       }
{                                                       }
{       Copyright (c) 2002-2003 Michal Marcinkowski     }
{                                                       }
{*******************************************************}

unit Sound;

{$H+}
interface

uses
  {$IFDEF STEAM}fgl,{$ENDIF}
  SDL2, Vector, openal, PhysFS;

type
  TSoundSample = record
    Loaded: Boolean;
    Buffer: ALuint;
  end;

  TScriptSound = record
    Name: string[26];
    Samp: TSoundSample;
  end;

const
  MAX_SOURCES = 256;
  RESERVED_SOURCES = 128;
  MAX_SAMPLES = 163;
  CHANNEL_WEATHER = 127;

var
  Samp: array[1..MAX_SAMPLES] of TSoundSample;
  ScriptSamp: array of TScriptSound;
  VolumeInternal: Single;
  ALDevice: PALCdevice;
  ALContext: PALCcontext;
  Sources: array[0..MAX_SOURCES - 1] of ALuint;
  DefaultChannel: LongInt = -1;
  {$IFDEF STEAM}
  VoiceChatBuffer: array[0..64] of ALuint;
  VoiceBufferQueue: TFPGList<LongWord>;
  {$ENDIF}

function InitSound(): Boolean;
function SoundNameToID(Name: string): ShortInt;
function LoadSample(Name: PChar; samp: TSoundSample): TSoundSample;
function ScaleVolumeSetting(VolumeSetting: Byte): Single;
procedure LoadSounds(ModDir: string);
procedure CloseSound;
procedure FPlaySound(SampleNum: Integer; ListenerX, ListenerY, EmitterX,
  EmitterY: Single; Chan: Integer);
procedure PlaySound(Sample: Integer); overload;
procedure PlaySound(Sample: Integer; Channel: Integer); overload;
procedure PlaySound(Sample: Integer; var Emitter: TVector2); overload;
procedure PlaySound(Sample: Integer; var Emitter: TVector2; var Channel: Integer); overload;
function StopSound(Channel: Integer): Boolean;

function SetSoundPaused(Channel: Integer; Paused: Boolean): Boolean;

function SetVolume(Channel: Integer; Volume: Single): Boolean;
{$IFDEF STEAM}
procedure PlayVoiceData(Data: Pointer; DataLength: Word; SpriteNum: Byte);
{$ENDIF}

implementation

uses
  Client, SysUtils,
  LogFile, Math, Constants, Game;

function InitSound(): Boolean;
var
  I: Integer;
begin
  Result := False;

  ALDevice := alcOpenDevice(nil);
  if ALDevice = nil then
    Exit;
  ALContext := alcCreateContext(ALDevice, nil);
  if ALContext = nil then
    Exit;
  if not alcMakeContextCurrent(ALContext) then
    Exit;

  alDistanceModel(AL_NONE);

  alGenSources(MAX_SOURCES, @Sources);
  if alGetError <> AL_NO_ERROR then
    Exit;

  for I := 1 to MAX_SAMPLES do
    Samp[I].Loaded := False;

  Result := True;

  {$IFDEF STEAM}
  alGenBuffers(64, @VoiceChatBuffer);
  VoiceBufferQueue := TFPGLIST<LongWord>.Create;
  for i := 0 to High(VoiceChatBuffer) do
    VoiceBufferQueue.Add(VoiceChatBuffer[i]);
  {$ENDIF}
end;

function LoadSample(Name: PChar; Samp: TSoundSample): TSoundSample;
var
  Spec: TSDL_AudioSpec;
  DataBuffer: PUInt8;
  DataLength: UInt32;
  Format: ALenum = AL_FORMAT_MONO8;
  BadFormat: Boolean;
  WavFile: PSDL_RWops;
  FileBuffer: PHYSFS_Buffer;
begin
  if not Samp.Loaded then
    Result.Loaded := False
  else
    Result := Samp;

  FileBuffer := PHYSFS_readBuffer(Name);
  if FileBuffer = nil then
    Exit;

  WavFile := SDL_RWFromMem(FileBuffer, Length(FileBuffer));

  if SDL_LoadWAV_RW(WavFile, 1, @Spec, @DataBuffer, @DataLength) = nil then
    Exit;

  BadFormat := False;
  case Spec.channels of
    1:
      case Spec.format of
        AUDIO_U8:
          Format := AL_FORMAT_MONO8;
        AUDIO_S16SYS:
          Format := AL_FORMAT_MONO16;
        AUDIO_F32SYS:
          Format := AL_FORMAT_MONO_FLOAT32;
        else
          BadFormat := True;
      end;
    2:
      case Spec.format of
        AUDIO_U8:
          Format := AL_FORMAT_STEREO8;
        AUDIO_S16SYS:
          Format := AL_FORMAT_STEREO16;
        AUDIO_F32SYS:
          Format := AL_FORMAT_STEREO_FLOAT32;
        else
          BadFormat := True;
      end;
    else
      BadFormat := True;
  end;
  if not BadFormat then
  begin
    if Samp.Loaded then
      alDeleteBuffers(1, @Samp.Buffer);
    alGenBuffers(1, @Result.Buffer);
    alBufferData(Result.Buffer, Format, DataBuffer, DataLength, Spec.freq);
    Result.Loaded := True;
  end;
  SDL_FreeWAV(DataBuffer);
end;

function SoundNameToID(Name: string): ShortInt;
var
  i: Byte;
begin
  Result := -1;
  if High(ScriptSamp) < 0 then
    Exit;
  for i := 0 to High(ScriptSamp) do
    if UpperCase(ScriptSamp[i].Name) = UpperCase(Name) then
    begin
      Result := i;
      Break;
    end;
end;

{ Takes a volume percentage (0-100) and converts it for internal use (0-1).
  The result is exponentially scaled to improve volume control intuitiveness
  and sensitivity at lower decibels.

  VolumeSetting the volume percentage to scale
  return the volume scaled for internal use
}
function ScaleVolumeSetting(VolumeSetting: Byte): Single;
begin
  Result := (Power(1.0404, VolumeSetting) - 1) / (1.0404 - 1) / 1275;
end;

procedure LoadSounds(ModDir: string);
const
  SAMPLE_FILES: array[1..MAX_SAMPLES] of string =
  (
    'ak74-fire.wav',
    'rocketz.wav',
    'ak74-reload.wav',
    '',  // empty.wav - no longer used
    'm249-fire.wav',
    'ruger77-fire.wav',
    'ruger77-reload.wav',
    'm249-reload.wav',
    'mp5-fire.wav',
    'mp5-reload.wav',
    'spas12-fire.wav',
    'spas12-reload.wav',
    'standup.wav',
    'fall.wav',
    'spawn.wav',
    'm79-fire.wav',
    'm79-explosion.wav',
    'm79-reload.wav',
    'grenade-throw.wav',
    'grenade-explosion.wav',
    'grenade-bounce.wav',
    'bryzg.wav',
    'infiltmus.wav',
    'headchop.wav',
    'explosion-erg.wav',
    'water-step.wav',
    'bulletby.wav',
    'bodyfall.wav',
    'deserteagle-fire.wav',
    'deserteagle-reload.wav',
    'steyraug-fire.wav',
    'steyraug-reload.wav',
    'barretm82-fire.wav',
    'barretm82-reload.wav',
    'minigun-fire.wav',
    'minigun-reload.wav',
    'minigun-start.wav',
    'minigun-end.wav',
    'pickupgun.wav',
    'capture.wav',
    'colt1911-fire.wav',
    'colt1911-reload.wav',
    'changeweapon.wav',
    'shell.wav',
    'shell2.wav',
    'dead-hit.wav',
    'throwgun.wav',
    'bow-fire.wav',
    'takebow.wav',
    'takemedikit.wav',
    'wermusic.wav',
    'ts.wav',
    'ctf.wav',
    'berserker.wav',
    'godflame.wav',
    'flamer.wav',
    'predator.wav',
    'killberserk.wav',
    'vesthit.wav',
    'burn.wav',
    'vesttake.wav',
    'clustergrenade.wav',
    'cluster-explosion.wav',
    'grenade-pullout.wav',
    'spit.wav',
    'stuff.wav',
    'smoke.wav',
    'match.wav',
    'roar.wav',
    'step.wav',
    'step2.wav',
    'step3.wav',
    'step4.wav',
    'hum.wav',
    'ric.wav',
    'ric2.wav',
    'ric3.wav',
    'ric4.wav',
    'dist-m79.wav',
    'dist-grenade.wav',
    'dist-gun1.wav',
    'dist-gun2.wav',
    'dist-gun3.wav',
    'dist-gun4.wav',
    'death.wav',
    'death2.wav',
    'death3.wav',
    'crouch-move.wav',
    'hit-arg.wav',
    'hit-arg2.wav',
    'hit-arg3.wav',
    'goprone.wav',
    'roll.wav',
    'fall-hard.wav',
    'onfire.wav',
    'firecrack.wav',
    'scope.wav',
    'scopeback.wav',
    'playerdeath.wav',
    'changespin.wav',
    'arg.wav',
    'lava.wav',
    'regenerate.wav',
    'prone-move.wav',
    'jump.wav',
    'crouch.wav',
    'crouch-movel.wav',
    'step5.wav',
    'step6.wav',
    'step7.wav',
    'step8.wav',
    'stop.wav',
    'bulletby2.wav',
    'bulletby3.wav',
    'bulletby4.wav',
    'bulletby5.wav',
    'weaponhit.wav',
    'clipfall.wav',
    'bonecrack.wav',
    'gaugeshell.wav',
    'colliderhit.wav',
    'kit-fall.wav',
    'kit-fall2.wav',
    'flag.wav',
    'flag2.wav',
    'takegun.wav',
    'infilt-point.wav',
    'menuclick.wav',
    'knife.wav',
    'slash.wav',
    'chainsaw-d.wav',
    'chainsaw-m.wav',
    'chainsaw-r.wav',
    'piss.wav',
    'law.wav',
    'chainsaw-o.wav',
    'm2fire.wav',
    'm2explode.wav',
    'm2overheat.wav',
    'signal.wav',
    'm2use.wav',
    'scoperun.wav',
    'mercy.wav',
    'ric5.wav',
    'ric6.wav',
    'ric7.wav',
    'law-start.wav',
    'law-end.wav',
    'boomheadshot.wav',
    'snapshot.wav',
    'radio/efcup.wav',
    'radio/efcmid.wav',
    'radio/efcdown.wav',
    'radio/ffcup.wav',
    'radio/ffcmid.wav',
    'radio/ffcdown.wav',
    'radio/esup.wav',
    'radio/esmid.wav',
    'radio/esdown.wav',
    'bounce.wav',
    'sfx_rain.wav',
    'sfx_snow.wav',
    'sfx_wind.wav'
  );
var
  SfxPath: string;
  i: Integer;
begin
  SfxPath := ModDir + 'sfx/';

  // Sound effects
  MainConsole.Console('Loading sound effects', DEBUG_MESSAGE_COLOR);

  for i := Low(SAMPLE_FILES) to High(SAMPLE_FILES) do
  begin
    if SAMPLE_FILES[i] <> '' then
    begin
      Samp[i] := LoadSample(PChar(SfxPath + SAMPLE_FILES[i]), Samp[i]);
      if not Samp[i].Loaded then
        MainConsole.Console('Unable to load file ' + SfxPath + SAMPLE_FILES[i], DEBUG_MESSAGE_COLOR);
    end;
  end;
end;

procedure CloseSound;
var
  i: Integer;
begin
  for i := Low(Samp) to High(Samp) do
    if Samp[i].Loaded then
    begin
      alDeleteBuffers(1, @Samp[i].Buffer);
      Samp[i].Loaded := False;
    end;
  alDeleteSources(MAX_SOURCES, Sources);
  alcMakeContextCurrent(nil);
  alcDestroyContext(ALContext);
  alcCloseDevice(ALDevice);
end;

procedure FPlaySound(SampleNum: Integer; ListenerX, ListenerY, EmitterX,
  EmitterY: Single; Chan: Integer);
var
  Dist: Single;
  Volume: Single;
  PlayMode: Integer;
  i: Integer;
  State: Integer = 0;
  //Pan: Single = 0.0;
begin

  if not Samp[SampleNum].Loaded then
    Exit;

  if CameraFollowSprite > 0 then
  begin
    ListenerX := Spriteparts.Pos[CameraFollowSprite].X;
    ListenerY := Spriteparts.Pos[CameraFollowSprite].Y;
  end;

  Dist := Sqrt(Sqr(EmitterX - ListenerX) + Sqr(EmitterY - ListenerY)) / SOUND_MAXDIST;

  // play distant sounds
  if (Dist > 0.5) and (snd_effects_battle.Value) then
  begin
    case SampleNum of
      SFX_M79_EXPLOSION:
        FPlaySound(SFX_DIST_M79, ListenerX, ListenerY, EmitterX, EmitterY, Chan);
      SFX_GRENADE_EXPLOSION, SFX_CLUSTERGRENADE, SFX_CLUSTER_EXPLOSION:
        FPlaySound(SFX_DIST_GRENADE, ListenerX, ListenerY, EmitterX, EmitterY, Chan);
      SFX_AK74_FIRE, SFX_M249_FIRE, SFX_RUGER77_FIRE, SFX_SPAS12_FIRE,
        SFX_DESERTEAGLE_FIRE, SFX_STEYRAUG_FIRE, SFX_BARRETM82_FIRE,
        SFX_MINIGUN_FIRE, SFX_COLT1911_FIRE:
        FPlaySound(81 + Random(4), ListenerX, ListenerY, EmitterX, EmitterY, Chan);
      SFX_DIST_M79, SFX_DIST_GRENADE, SFX_DIST_GUN1, SFX_DIST_GUN2, SFX_DIST_GUN3, SFX_DIST_GUN4:
        if Dist > 1 then
          Dist := Dist - 1
        else
          Dist := 1 - 2 * Dist;
    end;
  end;

  // decrease volume if grenade effect
  if (GrenadeEffectTimer > 0) and (SampleNum <> SFX_HUM) then
    Dist := (Dist + 10) * (GrenadeEffectTimer div 7);

  if Dist > 1 then
    Exit;

  if (SampleNum = SFX_ROCKETZ) or (SampleNum = SFX_CHAINSAW_R) or (SampleNum = SFX_FLAMER) then
    PlayMode := AL_TRUE // loop
  else
    PlayMode := AL_FALSE; // one time

  if Chan >= RESERVED_SOURCES then
    Exit;
  if Chan = -1 then
    for i := RESERVED_SOURCES to MAX_SOURCES - 1 do
    begin
      alGetSourcei(Sources[i], AL_SOURCE_STATE, State);
      if State <> AL_PLAYING then
      begin
        Chan := i;
        Break;
      end;
    end;

  if Chan <> -1 then
  begin
    alSourcei(Sources[Chan], AL_LOOPING, PlayMode);
    Volume := VolumeInternal * (1 - Dist);
    alSourcef(Sources[Chan], AL_GAIN, Volume);
    //Pan := EmitterX - ListenerX;
    alSource3f(Sources[Chan], AL_POSITION,
      (EmitterX - ListenerX)/SOUND_METERLENGTH,
      (EmitterY - ListenerY)/SOUND_METERLENGTH,
      -SOUND_PANWIDTH/SOUND_METERLENGTH);
    alGetSourcei(Sources[Chan], AL_SOURCE_STATE, State);
    if State = AL_PLAYING then
      Exit;
    if State = AL_PAUSED then
      alSourceStop(Sources[Chan]);
    alSourcei(Sources[Chan], AL_BUFFER, Samp[SampleNum].Buffer);
    alSourcePlay(Sources[Chan]);
  end;

end;

procedure PlaySound(Sample: Integer);
begin
  FPlaySound(Sample,
    CameraX, CameraY,
    CameraX, CameraY,
    DefaultChannel);
end;

procedure PlaySound(Sample: Integer; Channel: Integer);
begin
  FPlaySound(Sample,
    CameraX,   CameraY,
    CameraX,   CameraY,
    Channel);
end;

procedure PlaySound(Sample: Integer; var Emitter: TVector2);
begin
  FPlaySound(Sample,
    CameraX,   CameraY,
    Emitter.x, Emitter.y,
    DefaultChannel);
end;

procedure PlaySound(Sample: Integer; var Emitter: TVector2; var Channel: Integer);
begin
  FPlaySound(Sample,
    CameraX,   CameraY,
    Emitter.x, Emitter.y,
    Channel);
end;

function StopSound(Channel: Integer): Boolean;
begin
  alSourceStop(Sources[Channel]);
  Result := False;
end;

function SetSoundPaused(Channel: Integer; Paused: Boolean): Boolean;
var
  State: Integer = 0;
begin
  alGetSourcei(Sources[Channel], AL_SOURCE_STATE, State);
  if (State = AL_PLAYING) and (Paused) then
    alSourcePause(Sources[Channel]);
  if (State = AL_PAUSED) and (not Paused) then
    alSourcePlay(Sources[Channel]);

  Result := False;
end;

function SetVolume(Channel: Integer; Volume: Single): Boolean;
var
  i: Integer;
begin
  if Channel = -1 then
    for i := 0 to MAX_SOURCES - 1 do
      alSourcef(Sources[i], AL_GAIN, Volume)
  else
    alSourcef(Sources[Channel], AL_GAIN, Volume);
  Result := True;
end;

{$IFDEF STEAM}
procedure PlayVoiceData(Data: Pointer; DataLength: Word; SpriteNum: Byte);
var
  State: Integer = 0;
  Chan: Integer;
  i: Byte;
  BuffersProcessed: LongInt = 0;
  VoiceBuffer: ALuint;
  BufferHolder: array[0..64] of ALuint;
begin
  Chan := 64 + SpriteNum; // use reserved chan

  alGetSourcei(Sources[Chan], AL_BUFFERS_PROCESSED, BuffersProcessed);
  if BuffersProcessed > 0 then
  begin
    alSourceUnqueueBuffers(Sources[Chan], BuffersProcessed, BufferHolder);
    for i:= 0 to BuffersProcessed - 1 do
      VoiceBufferQueue.Add(bufferHolder[i]);
  end;

  VoiceBuffer := VoiceBufferQueue.First;
  VoiceBufferQueue.Remove(VoiceBufferQueue.First);
  alBufferData(VoiceBuffer, AL_FORMAT_MONO16, Data, DataLength, 44100);
  alSourceQueueBuffers(Sources[Chan], 1, @VoiceBuffer);
  alGetSourcei(Sources[Chan], AL_SOURCE_STATE, State);

  if State = AL_PLAYING then
    Exit;

  alSourcePlay(Sources[Chan]);
end;
{$ENDIF}

end.

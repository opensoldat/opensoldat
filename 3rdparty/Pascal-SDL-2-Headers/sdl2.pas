unit sdl2;

{
  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

  Pascal-Header-Conversion
  Copyright (C) 2012-2017 Tim Blume aka End/EV1313

  SDL.pas is based on the files:
  "sdl.h",
  "sdl_audio.h",
  "sdl_blendmode.h",
  "sdl_clipboard.h",
  "sdl_cpuinfo.h",
  "sdl_events.h",
  "sdl_error.h",
  "sdl_filesystem.h",
  "sdl_gamecontroller.h",
  "sdl_gesture.h",
  "sdl_haptic.h",
  "sdl_hints.h",
  "sdl_joystick.h",
  "sdl_keyboard.h",
  "sdl_keycode.h",
  "sdl_loadso.h",
  "sdl_log.h",
  "sdl_pixels.h",
  "sdl_power.h",
  "sdl_main.h",
  "sdl_messagebox.h",
  "sdl_mouse.h",
  "sdl_mutex.h",
  "sdl_rect.h",
  "sdl_render.h",
  "sdl_rwops.h",
  "sdl_scancode.h",
  "sdl_shape.h",
  "sdl_stdinc.h",
  "sdl_surface.h",
  "sdl_system.h",
  "sdl_syswm.h",
  "sdl_thread.h",
  "sdl_timer.h",
  "sdl_touch.h",
  "sdl_version.h",
  "sdl_video.h",
  "sdl_types.h"

  I will not translate:
  "sdl_opengl.h",
  "sdl_opengles.h"
  "sdl_opengles2.h"

  cause there's a much better OpenGL-Header avaible at delphigl.com:

  the dglopengl.pas

  You'll find it nowadays here: https://github.com/SaschaWillems/dglOpenGL

  Parts of the SDL.pas are from the SDL-1.2-Headerconversion from the JEDI-Team,
  written by Domenique Louis and others.

  I've changed the names of the dll for 32 & 64-Bit, so theres no conflict
  between 32 & 64 bit Libraries.

  This software is provided 'as-is', without any express or implied
  warranty.  In no case will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Special Thanks to:

   - DelphiGL.com - Community
   - Domenique Louis and everyone else from the JEDI-Team
   - Sam Latinga and everyone else from the SDL-Team
}

{
  Changelog:
  ----------
               ? 31.01.2016: updated sdlevents.inc to SDL2 2.0.4, adressing issue #24 (thx to imantsg)
               ? 16.01.2016: Delphi 6+ bugfixes/compatibility. Thx to Peter Karpov for identifiying and testing.
  v.1.80-stable; 09.10.2014: added sdl_cpuinfo.h and sdl_clipboard.h
  v.1.74-stable; 10.11.2013: added sdl_gamecontroller.h
  v.1.73-stable; 08.11.2013: added sdl_hints.h and some keystate helpers
                             thx to Cybermonkey!
  v.1.72-stable; 23.09.2013: fixed bug with procedures without parameters
                             (they must have brakets)
  v.1.70-stable; 17.09.2013: added "sdl_messagebox.h" and "sdl_haptic.h"
  v.1.63-stable; 16.09.2013: added libs sdl2_image and sdl2_ttf and added sdl_audio.h
  v.1.62-stable; 03.09.2013: fixed.
  v.1.61-stable; 02.09.2013: now it should REALLY work with Mac...
  v.1.60-stable; 01.09.2013: now it should work with Delphi XE4 for Windows and
                            MacOS and of course Lazarus. thx to kotai :D
  v.1.55-Alpha; 24.08.2013: fixed bug with SDL_GetEventState thx to d.l.i.w.
  v.1.54-Alpha; 24.08.2013: added sdl_loadso.h
  v.1.53-Alpha; 24.08.2013: renamed *really* and fixed linux comp.
  v.1.52-Alpha; 24.08.2013: renamed sdl.pas to sdl2.pas
  v.1.51-Alpha; 24.08.2013: added sdl_platform.h
  v.1.50-Alpha; 24.08.2013: the header is now modular. thx for the hint from d.l.i.w.
  v.1.40-Alpha; 13.08.2013: Added MacOS compatibility (thx to stoney-fd)
  v.1.34-Alpha; 05.08.2013: Added missing functions from sdl_thread.h
  v.1.33-Alpha; 31.07.2013: Added missing units for Linux. thx to Cybermonkey
  v.1.32-Alpha; 31.07.2013: Fixed three bugs, thx to grieferatwork
  v.1.31-Alpha; 30.07.2013: Added "sdl_power.h"
  v.1.30-Alpha; 26.07.2013: Added "sdl_thread.h" and "sdl_mutex.h"
  v.1.25-Alpha; 29.07.2013: Added Makros for SDL_RWops
  v.1.24-Alpha; 28.07.2013: Fixed bug with RWops and size_t
  v.1.23-Alpha; 27.07.2013: Fixed two bugs, thx to GrieferAtWork
  v.1.22-Alpha; 24.07.2013: Added "sdl_shape.h" and TSDL_Window
                            (and ordered the translated header list ^^)
  v.1.21-Alpha; 23.07.2013: Added TSDL_Error
  v.1.20-Alpha; 19.07.2013: Added "sdl_timer.h"
  v.1.10-Alpha; 09.07.2013: Added "sdl_render.h"
  v.1.00-Alpha; 05.07.2013: Initial Alpha-Release.
}

{$DEFINE SDL}

{$I jedi.inc}

interface

  {$IFDEF WINDOWS}
    uses
      Windows;
  {$ENDIF}

  {$IF DEFINED(UNIX) AND NOT DEFINED(ANDROID)}
    uses
      {$IFDEF DARWIN}
      CocoaAll;
      {$ELSE}
      X,
      XLib;
      {$ENDIF}
  {$ENDIF}

const

  {$IFDEF WINDOWS}
    SDL_LibName = 'SDL2.dll';
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF DARWIN}
      SDL_LibName = 'libSDL2.dylib';
    {$ELSE}
      {$IFDEF FPC}
        SDL_LibName = 'libSDL2.so';
      {$ELSE}
        SDL_LibName = 'libSDL2.so.0';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MACOS}
    SDL_LibName = 'SDL2';
    {$IFDEF FPC}
      {$linklib libSDL2}
    {$ENDIF}
  {$ENDIF}

{$I sdltype.inc}
{$I sdlversion.inc}
{$I sdlerror.inc}
{$I sdlplatform.inc}
{$I sdlpower.inc}
{$I sdlthread.inc}
{$I sdlmutex.inc}
{$I sdltimer.inc}
{$I sdlpixels.inc}
{$I sdlrect.inc}
{$I sdlrwops.inc}
{$I sdlaudio.inc}
{$I sdlblendmode.inc}
{$I sdlsurface.inc}
{$I sdlshape.inc}
{$I sdlvideo.inc}
{$I sdlhints.inc}
{$I sdlloadso.inc}
{$I sdlmessagebox.inc}
{$I sdlrenderer.inc}
{$I sdlscancode.inc}
{$I sdlkeyboard.inc}
{$I sdlmouse.inc}
{$I sdljoystick.inc}
{$I sdlgamecontroller.inc}
{$I sdlhaptic.inc}
{$I sdltouch.inc}
{$I sdlgesture.inc}
{$I sdlsyswm.inc}
{$I sdlevents.inc}
{$I sdlclipboard.inc}
{$I sdlcpuinfo.inc}
{$I sdlfilesystem.inc}
{$I sdllog.inc}
{$I sdlsystem.inc}
{$I sdl.inc}

implementation

//from "sdl_version.h"
procedure SDL_VERSION(out x: TSDL_Version);
begin
  x.major := SDL_MAJOR_VERSION;
  x.minor := SDL_MINOR_VERSION;
  x.patch := SDL_PATCHLEVEL;
end;

function SDL_VERSIONNUM(X,Y,Z: UInt32): Cardinal;
begin
  Result := X*1000 + Y*100 + Z;
end;

function SDL_COMPILEDVERSION: Cardinal;
begin
  Result := SDL_VERSIONNUM(SDL_MAJOR_VERSION,
                           SDL_MINOR_VERSION,
                           SDL_PATCHLEVEL);
end;

function SDL_VERSION_ATLEAST(X,Y,Z: Cardinal): Boolean;
begin
  Result := SDL_COMPILEDVERSION >= SDL_VERSIONNUM(X,Y,Z);
end;

//from "sdl_mouse.h"
function SDL_Button(button: SInt32): SInt32;
begin
  Result := 1 shl (button - 1); 
end;

{$IFDEF WINDOWS}
//from "sdl_thread.h"

function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PAnsiChar; data: Pointer): PSDL_Thread; overload;
begin
  Result := SDL_CreateThread(fn,name,data,nil,nil);
end;

{$ENDIF}

//from "sdl_rect.h"
function SDL_RectEmpty(const r: PSDL_Rect): Boolean;
begin
  Result := (r^.w <= 0) or (r^.h <= 0);
end;

function SDL_RectEquals(const a, b: PSDL_Rect): Boolean;
begin
  Result := (a^.x = b^.x) and (a^.y = b^.y) and (a^.w = b^.w) and (a^.h = b^.h);
end;

function SDL_PointInRect(const p: PSDL_Point; const r: PSDL_Rect): Boolean;
begin
  Result := 
    (p^.x >= r^.x) and (p^.x < (r^.x + r^.w)) 
    and 
    (p^.y >= r^.y) and (p^.y < (r^.y + r^.h))
end;

//from "sdl_rwops.h"

function SDL_RWsize(ctx: PSDL_RWops): SInt64;
begin
  Result := ctx^.size(ctx);
end;

function SDL_RWseek(ctx: PSDL_RWops; offset: SInt64; whence: SInt32): SInt64;
begin
  Result := ctx^.seek(ctx,offset,whence);
end;

function SDL_RWtell(ctx: PSDL_RWops): SInt64;
begin
  Result := ctx^.seek(ctx, 0, RW_SEEK_CUR);
end;

function SDL_RWread(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t;
begin
  Result := ctx^.read(ctx, ptr, size, n);
end;

function SDL_RWwrite(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t;
begin
  Result := ctx^.write(ctx, ptr, size, n);
end;

function SDL_RWclose(ctx: PSDL_RWops): SInt32;
begin
  Result := ctx^.close(ctx);
end;

//from "sdl_audio.h"

function SDL_LoadWAV(_file: PAnsiChar; spec: PSDL_AudioSpec; audio_buf: PPUInt8; audio_len: PUInt32): PSDL_AudioSpec;
begin
  Result := SDL_LoadWAV_RW(SDL_RWFromFile(_file, 'rb'), 1, spec, audio_buf, audio_len);
end;
  
function SDL_AUDIO_BITSIZE(x: Cardinal): Cardinal;
begin
  Result := x and SDL_AUDIO_MASK_BITSIZE;
end;

function SDL_AUDIO_ISFLOAT(x: Cardinal): Cardinal;
begin
  Result := x and SDL_AUDIO_MASK_DATATYPE;
end;

function SDL_AUDIO_ISBIGENDIAN(x: Cardinal): Cardinal;
begin
  Result := x and SDL_AUDIO_MASK_ENDIAN;
end;

function SDL_AUDIO_ISSIGNED(x: Cardinal): Cardinal;
begin
  Result := x and SDL_AUDIO_MASK_SIGNED;
end;

function SDL_AUDIO_ISINT(x: Cardinal): Cardinal;
begin
  Result := not SDL_AUDIO_ISFLOAT(x);
end;

function SDL_AUDIO_ISLITTLEENDIAN(x: Cardinal): Cardinal;
begin
  Result := not SDL_AUDIO_ISLITTLEENDIAN(x);
end;

function SDL_AUDIO_ISUNSIGNED(x: Cardinal): Cardinal;
begin
  Result := not SDL_AUDIO_ISSIGNED(x);
end;

//from "sdl_pixels.h"

function SDL_PIXELFLAG(X: Cardinal): Cardinal;
begin
  Result := (X shr 28) and $0F;
end;

function SDL_PIXELTYPE(X: Cardinal): Cardinal;
begin
  Result := (X shr 24) and $0F;
end;

function SDL_PIXELORDER(X: Cardinal): Cardinal;
begin
  Result := (X shr 20) and $0F;
end;

function SDL_PIXELLAYOUT(X: Cardinal): Cardinal;
begin
  Result := (X shr 16) and $0F;
end;

function SDL_BITSPERPIXEL(X: Cardinal): Cardinal;
begin
  Result := (X shr 8) and $FF;
end;

function SDL_IsPixelFormat_FOURCC(format: Variant): Boolean;
begin
  {* The flag is set to 1 because 0x1? is not in the printable ASCII range *}
  Result := format and SDL_PIXELFLAG(format) <> 1;
end;

//from "sdl_surface.h"
function SDL_LoadBMP(_file: PAnsiChar): PSDL_Surface;
begin
  Result := SDL_LoadBMP_RW(SDL_RWFromFile(_file, 'rb'), 1);
end;

function SDL_SaveBMP(const surface: PSDL_Surface; const filename: AnsiString
  ): sInt32;
begin
   Result := SDL_SaveBMP_RW(surface, SDL_RWFromFile(PAnsiChar(filename), 'wb'), 1)
end;

{**
 *  Evaluates to true if the surface needs to be locked before access.
 *}
function SDL_MUSTLOCK(const S: PSDL_Surface): Boolean;
begin
  Result := ((S^.flags and SDL_RLEACCEL) <> 0)
end;

//from "sdl_sysvideo.h"

function FULLSCREEN_VISIBLE(W: PSDL_Window): Variant;
begin
  Result := ((W^.flags and SDL_WINDOW_FULLSCREEN) and (W^.flags and SDL_WINDOW_SHOWN) and not (W^.flags and SDL_WINDOW_MINIMIZED));
end;

//from "sdl_video.h"

function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X: Variant): Variant;
begin
  Result := (SDL_WINDOWPOS_UNDEFINED_MASK or X);
end;

function SDL_WINDOWPOS_ISUNDEFINED(X: Variant): Variant;
begin
  Result := (X and $FFFF0000) = SDL_WINDOWPOS_UNDEFINED_MASK;
end;

function SDL_WINDOWPOS_CENTERED_DISPLAY(X: Variant): Variant;
begin
  Result := (SDL_WINDOWPOS_CENTERED_MASK or X);
end;

function SDL_WINDOWPOS_ISCENTERED(X: Variant): Variant;
begin
  Result := (X and $FFFF0000) = SDL_WINDOWPOS_CENTERED_MASK;
end;

//from "sdl_events.h"

function SDL_GetEventState(type_: UInt32): UInt8;
begin
  Result := SDL_EventState(type_, SDL_QUERY);
end;

// from "sdl_timer.h"
function SDL_TICKS_PASSED(const A, B: UInt32): Boolean;
begin
   Result := ((Int64(B) - Int64(A)) <= 0)
end;

// from "sdl_gamecontroller.h"
  {**
   *  Load a set of mappings from a file, filtered by the current SDL_GetPlatform()
   *}
function SDL_GameControllerAddMappingsFromFile(const FilePath: PAnsiChar
  ): SInt32;
begin
  Result := SDL_GameControllerAddMappingsFromRW(SDL_RWFromFile(FilePath, 'rb'), 1)
end;

end.

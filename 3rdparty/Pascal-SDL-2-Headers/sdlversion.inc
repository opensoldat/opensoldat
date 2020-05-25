//from "sdl_version.h"

  {**
   *  Information the version of SDL in use.
   *
   *  Represents the library's version as three levels: major revision
   *  (increments with massive changes, additions, and enhancements),
   *  minor revision (increments with backwards-compatible changes to the
   *  major revision), and patchlevel (increments with fixes to the minor
   *  revision).
   *
   *  SDL_VERSION
   *  SDL_GetVersion
   *}
type
  PSDL_Version = ^TSDL_Version;
  TSDL_Version = record
    major,         {**< major version *}
    minor,         {**< minor version *}
    patch: UInt8;  {**< update version *}
  end;

{* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL
*}
const
  SDL_MAJOR_VERSION = 2;
  SDL_MINOR_VERSION = 0;
  SDL_PATCHLEVEL    = 4;

{**
 *  Macro to determine SDL version program was compiled against.
 *
 *  This macro fills in a SDL_version structure with the version of the
 *  library you compiled against. This is determined by what header the
 *  compiler uses. Note that if you dynamically linked the library, you might
 *  have a slightly newer or older version at runtime. That version can be
 *  determined with SDL_GetVersion(), which, unlike SDL_VERSION(),
 *  is not a macro.
 *
 *  x   An instance on TSDL_Version to fill with version data.
 *
 *  SDL_version
 *  SDL_GetVersion
 *}
procedure SDL_VERSION(Out x: TSDL_Version);

{**
 *  This macro turns the version numbers into a numeric value:
 *
 *  (1,2,3) -> (1203)
 *
 *
 *  This assumes that there will never be more than 100 patchlevels.
 *}
function SDL_VERSIONNUM(X,Y,Z: UInt32): Cardinal;

  {**
   *  This is the version number macro for the current SDL version.
   *}
function SDL_COMPILEDVERSION: Cardinal;

  {**
   *  This macro will evaluate to true if compiled with SDL at least X.Y.Z.
   *}
function SDL_VERSION_ATLEAST(X,Y,Z: Cardinal): Boolean;

  {**
   *  Get the version of SDL that is linked against your program.
   *
   *  If you are linking to SDL dynamically, then it is possible that the
   *  current version will be different than the version you compiled against.
   *  This function returns the current version, while SDL_VERSION() is a
   *  macro that tells you what version you compiled with.
   *
   *
   *  compiled: TSDL_Version;
   *  linked: TSDL_Version;
   *
   *  SDL_VERSION(@compiled);
   *  SDL_GetVersion(@linked);
   *  WriteLn('We compiled against SDL version: ' +
   *           IntToStr(compiled.major) +
   *           IntToStr(compiled.minor) +
   *           IntToStr(compiled.patch));
   *  WriteLn('But we linked against SDL version:' +
   *           IntToStr(compiled.major) +
   *           IntToStr(compiled.minor) +
   *           IntToStr(compiled.patch));
   *
   *
   *  This function may be called safely at any time, even before SDL_Init().
   *
   *  SDL_VERSION
   *}
procedure SDL_GetVersion(ver: PSDL_Version) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetVersion' {$ENDIF} {$ENDIF};

  {**
   *  Get the code revision of SDL that is linked against your program.
   *
   *  Returns an arbitrary string (a hash value) uniquely identifying the
   *  exact revision of the SDL library in use, and is only useful in comparing
   *  against other revisions. It is NOT an incrementing number.
   *}
function SDL_GetRevision: PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRevision' {$ENDIF} {$ENDIF};

  {**
   *  Get the revision number of SDL that is linked against your program.
   *
   *  Returns a number uniquely identifying the exact revision of the SDL
   *  library in use. It is an incrementing number based on commits to
   *  hg.libsdl.org.
   *}
function SDL_GetRevisionNumber: SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRevisionNumber' {$ENDIF} {$ENDIF};

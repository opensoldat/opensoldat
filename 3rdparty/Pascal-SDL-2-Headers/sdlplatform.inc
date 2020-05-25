//from sdl_platform.h

{**
 *  Gets the name of the platform.
 *}
function SDL_GetPlatform: PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetPlatform' {$ENDIF} {$ENDIF};

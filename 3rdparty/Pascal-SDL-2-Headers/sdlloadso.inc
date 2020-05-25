//from sdl_loadso.h

{**
 *  This function dynamically loads a shared object and returns a pointer
 *  to the object handle (or NULL if there was an error).
 *  The 'sofile' parameter is a system dependent name of the object file.
 *}
function SDL_LoadObject(Const sofile: PAnsiChar): Pointer; cdecl;
   external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LoadObject' {$ENDIF}{$ENDIF};

{**
 *  Given an object handle, this function looks up the address of the
 *  named function in the shared object and returns it.  This address
 *  is no longer valid after calling SDL_UnloadObject().
 *}
function SDL_LoadFunction(handle: Pointer; Const name: PAnsiChar): Pointer; cdecl;
   external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LoadFunction' {$ENDIF}{$ENDIF};

{**
 *  Unload a shared object from memory.
 *}
procedure SDL_UnloadObject(handle: Pointer); cdecl;
   external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UnloadObject' {$ENDIF}{$ENDIF};

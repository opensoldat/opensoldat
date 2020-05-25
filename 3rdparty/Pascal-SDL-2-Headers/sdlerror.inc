//from "sdl_error.h"
const
  ERR_MAX_STRLEN = 128;
  ERR_MAX_ARGS   = 5;

  {* Public functions *}

  {* SDL_SetError() unconditionally returns -1. *}
function SDL_SetError(const fmt: PAnsiChar): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetError' {$ENDIF} {$ENDIF};
function SDL_GetError: PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetError' {$ENDIF} {$ENDIF};
procedure SDL_ClearError cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ClearError' {$ENDIF} {$ENDIF};
  {*Internal error functions*}
  {**
   *  Internal error functions
   *
   *  Private error reporting function - used internally.
   *}

    {
#define SDL_OutOfMemory()   SDL_Error(SDL_ENOMEM)
#define SDL_Unsupported()   SDL_Error(SDL_UNSUPPORTED)
#define SDL_InvalidParamError(param)    SDL_SetError("Parameter '%s' is invalid", (param))
   }
type
  TSDL_ErrorCode = (SDL_ENOMEM,
                    SDL_EFREAD,
                    SDL_EFWRITE,
                    SDL_EFSEEK,
                    SDL_UNSUPPORTED,
                    SDL_LASTERROR);

  TSDL_Error = record
    {* This is a numeric value corresponding to the current error *}
    error: SInt32;

    {* This is a key used to index into a language hashtable containing
       internationalized versions of the SDL error messages.  If the key
       is not in the hashtable, or no hashtable is available, the key is
       used directly as an error message format string.
     *}
    key: String[ERR_MAX_STRLEN];

    {* These are the arguments for the error functions *}
    argc: SInt32;
    case SInt32 of
         {* What is a character anyway?  (UNICODE issues) *}
      0: (value_c: Byte;);
      1: (value_ptr: Pointer;);
      2: (value_i: SInt32;);
      3: (value_f: Double;);
      4: (buf: String[ERR_MAX_STRLEN];);
  end;

  {* SDL_Error() unconditionally returns -1. *}
function SDL_Error(code: TSDL_ErrorCode): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Error' {$ENDIF} {$ENDIF};

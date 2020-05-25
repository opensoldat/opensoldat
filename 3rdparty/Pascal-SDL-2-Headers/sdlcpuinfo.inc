
{* This is a guess for the cacheline size used for padding.
 * Most x86 processors have a 64 byte cache line.
 * The 64-bit PowerPC processors have a 128 byte cache line.
 * We'll use the larger value to be generally safe.
 *}
const
  SDL_CACHELINE_SIZE = 128;

{**
 *  This function returns the number of CPU cores available.
 *}
function SDL_GetCPUCount(): Integer; cdecl external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetCPUCount' {$ENDIF} {$ENDIF};

{**
 *  This function returns the L1 cache line size of the CPU
 *
 *  This is useful for determining multi-threaded structure padding
 *  or SIMD prefetch sizes.
 *}
function SDL_GetCPUCacheLineSize(): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetCPUCacheLineSize' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has the RDTSC instruction.
 *}
function SDL_HasRDTSC(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasRDTSC' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has AltiVec features.
 *}
function SDL_HasAltiVec(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasAltiVec' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has MMX features.
 *}
function SDL_HasMMX(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasMMX' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has 3DNow! features.
 *}
function SDL_Has3DNow(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Has3DNow' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has SSE features.
 *}
function SDL_HasSSE(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasSSE' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has SSE2 features.
 *}
function SDL_HasSSE2(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasSSE2' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has SSE3 features.
 *}
function SDL_HasSSE3(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasSSE3' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has SSE4.1 features.
 *}
function SDL_HasSSE41(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasSSE41' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has SSE4.2 features.
 *}
function SDL_HasSSE42(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasSSE42' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has AVX features.
 *}
function SDL_HasAVX(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasAVX' {$ENDIF} {$ENDIF};

{**
 *  This function returns true if the CPU has AVX2 features.
 *}
function SDL_HasAVX2(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasAVX2' {$ENDIF} {$ENDIF};

{**
 *  This function returns the amount of RAM configured in the system, in MB.
 *}
function SDL_GetSystemRAM(): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetSystemRAM' {$ENDIF} {$ENDIF};


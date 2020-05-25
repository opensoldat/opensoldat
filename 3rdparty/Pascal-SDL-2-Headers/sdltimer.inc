//from "sdl_timer.h"

  {**
   *  Get the number of milliseconds since the SDL library initialization.
   *
   *  This value wraps if the program runs for more than ~49 days.
   *}
function SDL_GetTicks: UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetTicks' {$ENDIF} {$ENDIF};

  {**
   *  Get the current value of the high resolution counter
   *}
function SDL_GetPerformanceCounter: UInt64 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetPerformanceCounter' {$ENDIF} {$ENDIF};

  {**
   *  Get the count per second of the high resolution counter
   *}
function SDL_GetPerformanceFrequency: UInt64 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetPerformanceFrequency' {$ENDIF} {$ENDIF};

  {**
   *  Wait a specified number of milliseconds before returning.
   *}
procedure SDL_Delay(ms: UInt32) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Delay' {$ENDIF} {$ENDIF};

  {**
   *  Function prototype for the timer callback function.
   *
   *  The callback function is passed the current timer interval and returns
   *  the next timer interval.  If the returned value is the same as the one
   *  passed in, the periodic alarm continues, otherwise a new alarm is
   *  scheduled.  If the callback returns 0, the periodic alarm is cancelled.
   *}

type
  TSDL_TimerCallback = function(interval: UInt32; param: Pointer): UInt32; cdecl;

  {**
   * Definition of the timer ID type.
   *}
  TSDL_TimerID = SInt32;

  {**
   *  Add a new timer to the pool of timers already running.
   *
   *  A timer ID, or NULL when an error occurs.
   *}
function SDL_AddTimer(interval: UInt32; callback: TSDL_TimerCallback; param: Pointer): TSDL_TimerID cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_AddTimer' {$ENDIF} {$ENDIF};

  {**
   *  Remove a timer knowing its ID.
   *
   *  A boolean value indicating success or failure.
   *
   *  It is not safe to remove a timer multiple times.
   *}
function SDL_RemoveTimer(id: TSDL_TimerID): Boolean cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RemoveTimer' {$ENDIF} {$ENDIF};

  {**
  *   Compare SDL ticks values, and return true if A has passed B.
  *}
function SDL_TICKS_PASSED(Const A, B:UInt32):Boolean;

//from "sdl_mutex.h"

  {**
   *  Synchronization functions which can time out return this value
   *  if they time out.
   *}
const
  SDL_MUTEX_TIMEDOUT = 1;

  {**
   *  This is the timeout value which corresponds to never time out.
   *}
  //SDL_MUTEX_MAXWAIT   (~(Uint32)0)


  {**
   *  Mutex functions
   *}
type
  {* The SDL mutex structure, defined in SDL_mutex.c *}
  PSDL_Mutex = Pointer; //todo!

  {**
   *  Create a mutex, initialized unlocked.
   *}
function SDL_CreateMutex: PSDL_Mutex cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateMutex' {$ENDIF} {$ENDIF};

  {**
   *  Lock the mutex.
   *
   *   0, or -1 on error.
   *}
//#define SDL_mutexP(m)   SDL_LockMutex(m)
function SDL_LockMutex(mutex: PSDL_Mutex): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockMutex' {$ENDIF} {$ENDIF};

  {**
   *  Try to lock the mutex
   *
   *   0, SDL_MUTEX_TIMEDOUT, or -1 on error
   *}
function SDL_TryLockMutex(mutex: PSDL_Mutex): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_TryLockMutex' {$ENDIF} {$ENDIF};

  {**
   *  Unlock the mutex.
   *
   *   0, or -1 on error.
   *
   *   It is an error to unlock a mutex that has not been locked by
   *   the current thread, and doing so results in undefined behavior.
   *}
//#define SDL_mutexV(m)   SDL_UnlockMutex(m)
function SDL_UnlockMutex(mutex: PSDL_Mutex): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UnlockMutex' {$ENDIF} {$ENDIF};

  {**
   *  Destroy a mutex.
   *}
procedure SDL_DestroyMutex(mutex: PSDL_Mutex) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DestroyMutex' {$ENDIF} {$ENDIF};

  {*Mutex functions*}

  {**
   *   Semaphore functions
   *}
type
  {* The SDL semaphore structure, defined in SDL_sem.c *}
  PSDL_Sem = Pointer; //todo!

  {**
   *  Create a semaphore, initialized with value, returns NULL on failure.
   *}
function SDL_CreateSemaphore(initial_value: UInt32): PSDL_sem cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateSemaphore' {$ENDIF} {$ENDIF};

  {**
   *  Destroy a semaphore.
   *}
procedure SDL_DestroySemaphore(sem: PSDL_Sem) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DestroySemaphore' {$ENDIF} {$ENDIF};

  {**
   *  This function suspends the calling thread until the semaphore pointed
   *  to by sem has a positive count. It then atomically decreases the
   *  semaphore count.
   *}
function SDL_SemWait(sem: PSDL_Sem): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SemWait' {$ENDIF} {$ENDIF};

  {**
   *  Non-blocking variant of SDL_SemWait().
   *
   *   0 if the wait succeeds, SDL_MUTEX_TIMEDOUT if the wait would
   *   block, and -1 on error.
   *}
function SDL_SemTryWait(sem: PSDL_Sem): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SemTryWait' {$ENDIF} {$ENDIF};

  {**
   *  Variant of SDL_SemWait() with a timeout in milliseconds.
   *
   *   0 if the wait succeeds, ::SDL_MUTEX_TIMEDOUT if the wait does not
   *   succeed in the allotted time, and -1 on error.
   *
   *   On some platforms this function is implemented by looping with a
   *   delay of 1 ms, and so should be avoided if possible.
   *}
function SDL_SemWaitTimeout(sem: PSDL_Sem; ms: UInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SemWaitTimeout' {$ENDIF} {$ENDIF};

  {**
   *  Atomically increases the semaphore'_S count (not blocking).
   *
   *   0, or -1 on error.
   *}
function SDL_SemPost(sem: PSDL_Sem): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SemPost' {$ENDIF} {$ENDIF};

  {**
   *  Returns the current count of the semaphore.
   *}
function SDL_SemValue(sem: PSDL_Sem): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SemValue' {$ENDIF} {$ENDIF};

  {*Semaphore functions*}

  {**
   *  Condition variable functions
   * }
type
  {* The SDL condition variable structure, defined in SDL_cond.c *}
  PSDL_Cond = Pointer; //todo!!

  {**
   *  Create a condition variable.
   *
   *  Typical use of condition variables:
   *
   *  Thread A:
   *    SDL_LockMutex(lock);
   *    while ( not condition )
   *    begin
   *      SDL_CondWait(cond, lock);
   *    end;
   *    SDL_UnlockMutex(lock);
   *
   *  Thread B:
   *    SDL_LockMutex(lock);
   *    ...
   *    condition := true;
   *    ...
   *    SDL_CondSignal(cond);
   *    SDL_UnlockMutex(lock);
   *
   *  There is some discussion whether to signal the condition variable
   *  with the mutex locked or not.  There is some potential performance
   *  benefit to unlocking first on some platforms, but there are some
   *  potential race conditions depending on how your code is structured.
   *
   *  In general it'_S safer to signal the condition variable while the
   *  mutex is locked.
   *}
function SDL_CreateCond: PSDL_Cond cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateCond' {$ENDIF} {$ENDIF};

  {**
   *  Destroy a condition variable.
   *}
procedure SDL_DestroyCond(cond: PSDL_Cond) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DestroyCond' {$ENDIF} {$ENDIF};

  {**
   *  Restart one of the threads that are waiting on the condition variable.
   *
   *   0 or -1 on error.
   *}
function SDL_CondSignal(cond: PSDL_Cond): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CondSignal' {$ENDIF} {$ENDIF};

  {**
   *  Restart all threads that are waiting on the condition variable.
   *
   *   0 or -1 on error.
   *}
function SDL_CondBroadcast(cond: PSDL_Cond): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CondBroadcast' {$ENDIF} {$ENDIF};

  {**
   *  Wait on the condition variable, unlocking the provided mutex.
   *
   *   The mutex must be locked before entering this function!
   *
   *  The mutex is re-locked once the condition variable is signaled.
   *
   *   0 when it is signaled, or -1 on error.
   *}
function SDL_CondWait(cond: PSDL_Cond; mutex: PSDL_Mutex): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CondWait' {$ENDIF} {$ENDIF};

  {**
   *  Waits for at most ms milliseconds, and returns 0 if the condition
   *  variable is signaled, SDL_MUTEX_TIMEDOUT if the condition is not
   *  signaled in the allotted time, and -1 on error.
   *
   *   On some platforms this function is implemented by looping with a
   *   delay of 1 ms, and so should be avoided if possible.
   *}
function SDL_CondWaitTimeout(cond: PSDL_Cond; mutex: PSDL_Mutex; ms: UInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CondWaitTimeout' {$ENDIF} {$ENDIF};

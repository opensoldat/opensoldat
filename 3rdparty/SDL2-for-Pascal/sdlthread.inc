//from "sdl_thread.h"

{* The SDL thread structure, defined in SDL_thread.c *}
//todo!
type
  {* The SDL thread priority
   *
   * Note: On many systems you require special privileges to set high priority.
   *}

  TSDL_ThreadPriority = (SDL_THREAD_PRIORITY_LOW,
                         SDL_THREAD_PRIORITY_NORMAL,
                         SDL_THREAD_PRIORITY_HIGH);

  {* The function passed to SDL_CreateThread()
     It is passed a void* user context parameter and returns an int.
   *}
  PSDL_ThreadFunction = ^TSDL_ThreadFunction;
  TSDL_ThreadFunction = function(data: Pointer): Integer; cdecl;

  {* The SDL thread ID *}
  TSDL_ThreadID = LongWord;
   {
  PSDL_Thread = Pointer;
     }

  PSDL_Thread = ^TSDL_Thread;
  TSDL_Thread = record
    threadid: TSDL_ThreadID;
    handle: THandle;
    status: SInt32;
    errbuf: TSDL_Error;
    name: PAnsiChar;
    data: Pointer;
  end;

  TSDL_TLSID = Cardinal;

{$IFDEF WINDOWS}
  {**
   *  SDL_thread.h
   *
   *  We compile SDL into a DLL. This means, that it's the DLL which
   *  creates a new thread for the calling process with the SDL_CreateThread()
   *  API. There is a problem with this, that only the RTL of the SDL.DLL will
   *  be initialized for those threads, and not the RTL of the calling
   *  application!
   *
   *  To solve this, we make a little hack here.
   *
   *  We'll always use the caller's _beginthread() and _endthread() APIs to
   *  start a new thread. This way, if it's the SDL.DLL which uses this API,
   *  then the RTL of SDL.DLL will be used to create the new thread, and if it's
   *  the application, then the RTL of the application will be used.
   *
   *  So, in short:
   *  Always use the _beginthread() and _endthread() of the calling runtime
   *  library!
   *}
{$DEFINE SDL_PASSED_BEGINTHREAD_ENDTHREAD}

type
  {$IFNDEF DELPHI16UP}
    TThreadID = Cardinal;
  {$ENDIF}

  TpfnSDL_CurrentBeginThread = function(SecurityAttributes: Pointer; StackSize: LongWord; ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord; var ThreadId: TThreadID): Integer;

  TpfnSDL_CurrentEndThread = procedure(ExitCode: Integer);

  {**
   *  Create a thread.
   *}
function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PAnsiChar; data: Pointer; pfnBeginThread: TpfnSDL_CurrentBeginThread; pfnEndThread: TpfnSDL_CurrentEndThread): PSDL_Thread; cdecl; overload; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateThread' {$ENDIF} {$ENDIF};

  {**
   *  Create a thread.
   *}
function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PAnsiChar; data: Pointer): PSDL_Thread; overload;

{$ELSE}

  {**
   *  Create a thread.
   *
   *   Thread naming is a little complicated: Most systems have very small
   *    limits for the string length (BeOS has 32 bytes, Linux currently has 16,
   *    Visual C++ 6.0 has nine!), and possibly other arbitrary rules. You'll
   *    have to see what happens with your system's debugger. The name should be
   *    UTF-8 (but using the naming limits of C identifiers is a better bet).
   *   There are no requirements for thread naming conventions, so long as the
   *    string is null-terminated UTF-8, but these guidelines are helpful in
   *    choosing a name:
   *
   *    http://stackoverflow.com/questions/149932/naming-conventions-for-threads
   *
   *   If a system imposes requirements, SDL will try to munge the string for
   *    it (truncate, etc), but the original string contents will be available
   *    from SDL_GetThreadName().
   *}
function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PAnsiChar; data: Pointer): PSDL_Thread; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateThread' {$ENDIF} {$ENDIF};

{$ENDIF}

  {**
   * Get the thread name, as it was specified in SDL_CreateThread().
   *  This function returns a pointer to a UTF-8 string that names the
   *  specified thread, or NULL if it doesn't have a name. This is internal
   *  memory, not to be free()'d by the caller, and remains valid until the
   *  specified thread is cleaned up by SDL_WaitThread().
   *}
function SDL_GetThreadName(thread: PSDL_Thread): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetThreadName' {$ENDIF}{$ENDIF};

  {**
   *  Get the thread identifier for the current thread.
   *}
function SDL_ThreadID: TSDL_ThreadID cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ThreadID' {$ENDIF}{$ENDIF};

  {**
   *  Get the thread identifier for the specified thread.
   *
   *  Equivalent to SDL_ThreadID() if the specified thread is NULL.
   *}
function SDL_GetThreadID(thread: PSDL_Thread): TSDL_ThreadID cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetThreadID' {$ENDIF}{$ENDIF};

  {**
   *  Set the priority for the current thread
   *}
function SDL_SetThreadPriority(priority: TSDL_ThreadPriority): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetThreadPriority' {$ENDIF}{$ENDIF};

  {**
   *  Wait for a thread to finish.
   *
   *  The return code for the thread function is placed in the area
   *  pointed to by status, if status is not NULL.
   *}
procedure SDL_WaitThread(thread: PSDL_Thread; status: PInt) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WaitThread' {$ENDIF}{$ENDIF};

  {**
   *  A thread may be "detached" to signify that it should not remain until
   *  another thread has called SDL_WaitThread() on it. Detaching a thread
   *  is useful for long-running threads that nothing needs to synchronize
   *  with or further manage. When a detached thread is done, it simply
   *  goes away.
   *
   *  There is no way to recover the return code of a detached thread. If you
   *  need this, don't detach the thread and instead use SDL_WaitThread().
   *
   *  Once a thread is detached, you should usually assume the SDL_Thread isn't
   *  safe to reference again, as it will become invalid immediately upon
   *  the detached thread's exit, instead of remaining until someone has called
   *  SDL_WaitThread() to finally clean it up. As such, don't detach the same
   *  thread more than once.
   *
   *  If a thread has already exited when passed to SDL_DetachThread(), it will
   *  stop waiting for a call to SDL_WaitThread() and clean up immediately.
   *  It is not safe to detach a thread that might be used with SDL_WaitThread().
   *
   *  You may not call SDL_WaitThread() on a thread that has been detached.
   *  Use either that function or this one, but not both, or behavior is
   *  undefined.
   *
   *  It is safe to pass NIL to this function; it is a no-op.
   *}
procedure SDL_DetachThread(thread:TSDL_Thread); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DetachThread' {$ENDIF}{$ENDIF};

  {**
   *  Create an identifier that is globally visible to all threads but refers to data that is thread-specific.
   *
   *   The newly created thread local storage identifier, or 0 on error
   *
   *  var tls_lock: TSDL_SpinLock;
   *    thread_local_storage: TSDL_TLSID;
   *
   *  procedure SetMyThreadData(value: Pointer)
   *
   *      if not (thread_local_storage) then
   *      begin
   *        SDL_AtomicLock(@tls_lock);
   *        if (!thread_local_storage)
   *              thread_local_storage = SDL_TLSCreate();
   *
   *        SDL_AtomicUnLock(@tls_lock);
   *
   *        SDL_TLSSet(thread_local_storage, value);
   *      end;
   *
   *  function GetMyThreadData(): Pointer;
   *  begin
   *     Result := SDL_TLSGet(thread_local_storage);
   *  end;
   *
   *   SDL_TLSGet()
   *   SDL_TLSSet()
   *}
function SDL_TLSCreate: TSDL_TLSID cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_TLSCreate' {$ENDIF} {$ENDIF};

  {**
   *  Get the value associated with a thread local storage ID for the current thread.
   *
   *   id The thread local storage ID
   *
   *   The value associated with the ID for the current thread, or NULL if no value has been set.
   *
   *   SDL_TLSCreate()
   *   SDL_TLSSet()
   *}
function SDL_TLSGet(id: TSDL_TLSID): Pointer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_TLSGet' {$ENDIF} {$ENDIF};

  {**
   *  Set the value associated with a thread local storage ID for the current thread.
   *
   *   id The thread local storage ID
   *   value The value to associate with the ID for the current thread
   *   destructor_ A function called when the thread exits, to free the value.
   *
   *   0 on success, -1 on error
   *
   *   SDL_TLSCreate()
   *   SDL_TLSGet()
   *}
function SDL_TLSSet(id: TSDL_TLSID; value: Pointer; destructor_: Pointer): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_TLSSet' {$ENDIF} {$ENDIF};

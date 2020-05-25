{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.13    03/16/05 11:15:42 AM  JSouthwell
  Named the IdNotify thread for simpler debugging.

  Rev 1.12    2004.04.13 10:22:52 PM  czhower
  Changed procedure to class method.

  Rev 1.11    4/12/2004 11:44:36 AM  BGooijen
  fix

  Rev 1.10    4/12/2004 11:36:56 AM  BGooijen
  NotifyThread can be cleaned up with procedure now

  Rev 1.9    2004.03.11 10:14:46 AM  czhower
  Improper cast fixed.

  Rev 1.8    2004.02.29 8:23:16 PM  czhower
  Fixed visibility mismatch.

  Rev 1.7    2004.02.25 10:11:42 AM  czhower
  Fixed visibility in notify

  Rev 1.6    2004.02.03 4:16:54 PM  czhower
  For unit name changes.

  Rev 1.5    1/1/2004 11:56:10 PM  PIonescu
  Fix for TIdNotifyMethod's constructor

  Rev 1.4    2003.12.31 7:33:20 PM  czhower
  Constructor bug fix.

  Rev 1.3    5/12/2003 9:17:42 AM  GGrieve
  compile fix

  Rev 1.2    2003.09.18 5:42:14 PM  czhower
  Removed TIdThreadBase

  Rev 1.1    05.6.2003 ã. 11:30:12  DBondzhev
  Mem leak fix for notifiers created in main thread. Also WaitFor for waiting
  notification to be executed.

  Rev 1.0    11/13/2002 09:00:10 AM  JPMugaas
}

unit IdSync;

// Author: Chad Z. Hower - a.k.a. Kudzu

interface

{$i IdCompilerDefines.inc}

{$IFDEF HAS_STATIC_TThread_ForceQueue}
  {$IFDEF BROKEN_TThread_ForceQueue}
    {$UNDEF HAS_STATIC_TThread_ForceQueue}
  {$ENDIF}
{$ENDIF}

{$UNDEF NotifyThreadNeeded}
{$IFNDEF HAS_STATIC_TThread_Synchronize}
  {$DEFINE NotifyThreadNeeded}
{$ENDIF}
{$IFNDEF HAS_STATIC_TThread_Queue}
  {$DEFINE NotifyThreadNeeded}
{$ELSE}
  {$IFNDEF HAS_STATIC_TThread_ForceQueue}
    {$DEFINE NotifyThreadNeeded}
  {$ENDIF}
{$ENDIF}

uses
  Classes,
  IdGlobal
  {$IFDEF NotifyThreadNeeded}
  , IdThread
  {$ENDIF}
  ;

type
  TIdSync = class(TObject)
  protected
    {$IFNDEF HAS_STATIC_TThread_Synchronize}
    FThread: TIdThread;
    {$ENDIF}
    //
    procedure DoSynchronize; virtual; abstract;
  public
    {$IFDEF HAS_STATIC_TThread_Synchronize}
    constructor Create; virtual;
    {$ELSE}
    constructor Create; overload; virtual;
    constructor Create(AThread: TIdThread); overload; virtual;
    {$ENDIF}
    procedure Synchronize;
    class procedure SynchronizeMethod(AMethod: TThreadMethod);
    //
    {$IFNDEF HAS_STATIC_TThread_Synchronize}
    property Thread: TIdThread read FThread;
    {$ENDIF}
  end
  {$IFDEF HAS_STATIC_TThread_Synchronize}
    // TODO: deprecate TIdSync only if anonymous procedures are supported?
    // Delphi's TThread.Synchronize() supports them, but FreePascal's does not...
    {.$IFDEF HAS_STATIC_TThread_Synchronize_AnonProc}
      //{$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use static TThread.Synchronize() with an anonymous procedure'{$ENDIF}{$ENDIF}
      {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use static TThread.Synchronize()'{$ENDIF}{$ENDIF}
    {.$ENDIF}
  {$ENDIF}
  ;

  TIdNotify = class(TObject)
  protected
    FMainThreadUsesNotify: Boolean;
    //
    procedure DoNotify; virtual; abstract;
    {$IFNDEF USE_OBJECT_ARC}
    procedure InternalDoNotify;
    {$ENDIF}
  public
    constructor Create; virtual; // here to make virtual
    procedure Notify;
    {$IFNDEF HAS_STATIC_TThread_Queue}
    procedure WaitFor; {$IFDEF HAS_DEPRECATED}deprecated;{$ENDIF}
    {$ENDIF}
    class procedure NotifyMethod(AMethod: TThreadMethod; AForceQueue: Boolean = False);
    //
    property MainThreadUsesNotify: Boolean read FMainThreadUsesNotify write FMainThreadUsesNotify;
  end
  {$IFDEF HAS_STATIC_TThread_Queue}
    {$IFDEF HAS_STATIC_TThread_ForceQueue}
      // TODO: deprecate TIdNotify only if anonymous procedures are available?
      // Delphi's TThread.(Force)Queue() supports them, but FreePascal's does not...
      {.$IFDEF HAS_STATIC_TThread_Queue_AnonProc}
        //{$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use static TThread.Queue() or TThread.ForceQueue() with an anonymous procedure'{$ENDIF}{$ENDIF}
        {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use static TThread.Queue() or TThread.ForceQueue()'{$ENDIF}{$ENDIF}
      {.$ENDIF}
    {$ENDIF}
  {$ENDIF}
  ;

  {$I IdSymbolDeprecatedOff.inc}
  TIdNotifyMethod = class(TIdNotify)
  protected
    FMethod: TThreadMethod;
    //
    procedure DoNotify; override;
  public
    constructor Create(AMethod: TThreadMethod); reintroduce; virtual;
  end {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} {$IFDEF HAS_STATIC_TThread_Queue}{$IFDEF HAS_STATIC_TThread_ForceQueue}'Use static TThread.Queue() or TThread.ForceQueue()'{$ELSE}'Use static TThread.Queue()'{$ENDIF}{$ELSE}'Use TIdNotify.NotifyMethod()'{$ENDIF}{$ENDIF}{$ENDIF};
  {$I IdSymbolDeprecatedOn.inc}

implementation

uses
  //facilitate inlining only.
  {$IFDEF DOTNET}
    {$IFDEF USE_INLINE}
  System.Threading,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF NotifyThreadNeeded}
    {$IFDEF HAS_UNIT_Generics_Collections}
  System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF VCL_2010_OR_ABOVE}
    {$IFDEF WINDOWS}
  Windows,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_VCL_POSIX}
  Posix.SysSelect,
  Posix.SysTime,
  {$ENDIF}
  SysUtils
  {$IFNDEF NotifyThreadNeeded}
  , IdThread
  {$ENDIF}
  ;

// TODO: there is a bug in FireMonkey prior to XE7 where FMX.TApplication does
// not assign a handler to the Classes.WakeMainThread callback (see QC #123579).
// Without that, TThread.Synchronize() and TThread.Queue() will not do anything
// if the main message queue is idle at the moment they are called!!!  If the
// main thread *happens* to receive a message at a later time, say from UI
// activity, then they will be processed.  But for a background process, we
// cannot rely on that.  Need an alternative solution for those versions of
// FireMonkey...

{$IFDEF NotifyThreadNeeded}
type
  // This is done with a NotifyThread instead of PostMessage because starting
  // with D6/Kylix Borland radically modified the mechanisms for .Synchronize.
  // This is a bit more code in the end, but its source compatible and does not
  // rely on Indy directly accessing any OS APIs and performance is still more
  // than acceptable, especially considering Notifications are low priority.

  {$IFDEF HAS_GENERICS_TThreadList}
  TIdNotifyThreadList = TThreadList<TIdNotify>;
  TIdNotifyList = TList<TIdNotify>;
  {$ELSE}
  // TODO: flesh out to match TThreadList<TIdNotify> and TList<TIdNotify> for non-Generics compilers...
  TIdNotifyThreadList = TThreadList;
  TIdNotifyList = TList;
  {$ENDIF}

  TIdNotifyThread = class(TIdThread)
  protected
    FEvent: TIdLocalEvent;
    FNotifications: TIdNotifyThreadList;
  public
    procedure AddNotification(ASync: TIdNotify);
    constructor Create; reintroduce;
    destructor Destroy; override;
    class procedure FreeThread;
    procedure Run; override;
  end;

var
  GNotifyThread: TIdNotifyThread = nil;

procedure CreateNotifyThread;
begin
  // TODO: this function has a race condition if it is called by multiple
  // threads at the same time and GNotifyThread has not been assigned yet!
  // Need to use something like InterlockedCompareExchangeObj() so any
  // duplicate threads can be freed...
  {
  Thread := TIdNotifyThread.Create(True);
  if InterlockedCompareExchangeObj(GNotifyThread, Thread, nil) <> nil then begin
    Thread.Free;
  end else begin
    Thread.Start;
  end;
  }
  if GNotifyThread = nil then begin
    GNotifyThread := TIdNotifyThread.Create;
  end;
end;
{$ENDIF}

{ TIdSync }

{$IFNDEF HAS_STATIC_TThread_Synchronize}
constructor TIdSync.Create(AThread: TIdThread);
begin
  inherited Create;
  FThread := AThread;
end;
{$ENDIF}

constructor TIdSync.Create;
begin
  {$IFDEF HAS_STATIC_TThread_Synchronize}
  inherited Create;
  {$ELSE}
    {$IFDEF DOTNET}
  inherited Create;
  CreateNotifyThread;
  FThread := GNotifyThread;
    {$ELSE}
  CreateNotifyThread;
  Create(GNotifyThread);
    {$ENDIF}
  {$ENDIF}
end;

procedure DoThreadSync(
  {$IFNDEF HAS_STATIC_TThread_Synchronize}
  AThread: TIdThread;
  {$ENDIF}
  SyncProc: TThreadMethod);
begin
  {
  if not Assigned(Classes.WakeMainThread) then
  begin
    // TODO: if WakeMainThread is not assigned, need to force a message into
    // the main message queue so TApplication.Idle() will be called so it can
    // call CheckSynchronize():
    //
    // on Windows, call PostMessage() to post a WM_NULL message to the TApplication window...
    //
    // on OSX (and iOS?), call NSApp.sendEvent(???), but with what kind of event?
    //
    // on Android, what to do???

    // We can't put the message in the queue before calling TThread.Synchronize(),
    // as it might get processed before Synchronize() can queue the procedure.
    // Might have to use TThread.Queue() instead and wait on a manual TEvent...
  end else
  begin
  }
    // RLebeau 6/7/2016: there are race conditions if multiple threads call
    // TThread.Synchronize() on the same TThread object at the same time
    // (such as this unit's GNotifyThread object)...
    {$IFDEF HAS_STATIC_TThread_Synchronize}
    // Fortunately, the static versions of TThread.Synchronize() can skip the
    // race conditions when the AThread parameter is nil, so we are safe here...
    TThread.Synchronize(nil, SyncProc);
    {$ELSE}
    // However, in Delphi 7 and later, the static versions of TThread.Synchronize()
    // call the non-static versions when AThread is not nil, and the non-static
    // versions are not even close to being thread-safe (see QualityPortal #RSP-15139).
    // They share a private FSynchronize variable that is not protected from
    // concurrent access.
    //
    // In Delphi 6, TThread.Synchronize() is thread-safe UNLESS a synch'ed method
    // raises an uncaught exception, then there is a race condition on a private
    // FSynchronizeException variable used to capture and re-raise the exception,
    // so multiple threads could potentially re-raise the same exception object,
    // or cancel out another thread's exception before it can be re-raised.
    //
    // In Delphi 5, there are race conditions on private FSynchronizeException and
    // FMethod variables, making Synchronize() basically not thread-safe at all.
    //
    // So, in Delphi 5 and 6 at least, we need a way for TIdSync to synch
    // a method more safely.  Thread.Queue() does not exist in those versions.
    //
    // At this time, I do not know if FreePascal's implementation of TThread
    // has any issues.
    //
    // TODO: We might need to expand TIdNotifyThread to handle both TIdSync and
    // TIdNotify from within its own context...
    //
    AThread.Synchronize(SyncProc);
    {$ENDIF}
  // end;
end;

procedure TIdSync.Synchronize;
begin
  DoThreadSync(
    {$IFNDEF HAS_STATIC_TThread_Synchronize}FThread,{$ENDIF}
    DoSynchronize
  );
end;

class procedure TIdSync.SynchronizeMethod(AMethod: TThreadMethod);
begin
  {$IFDEF HAS_STATIC_TThread_Synchronize}
  DoThreadSync(AMethod);
  {$ELSE}
  CreateNotifyThread;
  DoThreadSync(GNotifyThread, AMethod);
  {$ENDIF}
end;

{ TIdNotify }

constructor TIdNotify.Create;
begin
  inherited Create;
end;

{$UNDEF USE_DoThreadQueue}
{$IFDEF HAS_STATIC_TThread_Queue}
  {$DEFINE USE_DoThreadQueue}
{$ENDIF}
{$IFDEF HAS_STATIC_TThread_ForceQueue}
  {$DEFINE USE_DoThreadQueue}
{$ENDIF}

{$IFDEF USE_DoThreadQueue}
procedure DoThreadQueue(QueueProc: TThreadMethod
  {$IFDEF HAS_STATIC_TThread_ForceQueue}
  ; AForceQueue: Boolean = False
  {$ENDIF}
);
begin
  {
  if not Assigned(Classes.WakeMainThread) then
  begin
    // TODO: if WakeMainThread is not assigned, need to force a message into
    // the main message queue so TApplication.Idle() will be called so it can
    // call CheckSynchronize():
    //
    // on Windows, call PostMessage() to post a WM_NULL message to the TApplication window...
    //
    // on OSX (and iOS?), call NSApp.sendEvent(???), but with what kind of event?
    //
    // on Android, what to do???

    // We can't put the message in the queue before calling TThread.Queue(),
    // as it might get processed before Queue() can queue the procedure.
    // Might have to wait on a manual TEvent...
  end else
  begin
  }
    {$IFDEF HAS_STATIC_TThread_ForceQueue}
    if AForceQueue then begin
      TThread.ForceQueue(nil, QueueProc);
    end else begin
      TThread.Queue(nil, QueueProc);
    end;
    {$ELSE}

    // TODO: FreePascal/Lazarus has Application.QueueAsyncCall(), but it is in the Forms unit!
    {
    if AForceQueue then begin
      Application.QueueAsyncCall(NotifyAsync, @QueueProc);
    else
      TThread.Queue(nil, QueueProc);
    }

    TThread.Queue(nil, QueueProc);
    {$ENDIF}
  // end;
end;
{$ENDIF}

procedure TIdNotify.Notify;
begin
  {$IFDEF HAS_STATIC_TThread_ForceQueue}
  DoThreadQueue(
    {$IFNDEF USE_OBJECT_ARC}
    InternalDoNotify
    {$ELSE}
    DoNotify
    {$ENDIF}
    , MainThreadUsesNotify
  );
  {$ELSE}
  if InMainThread then
  begin
    // RLebeau 9/4/2010: MainThreadUsesNotify only has meaning now when
    // TThread.Queue() is not available, as it calls the specified method
    // immediately if invoked in the main thread!  To go back to the old
    // behavior, we would have to re-enable use of TIdNotifyThread, which is
    // another interface change...

    // RLebeau 6/21/2017: Delphi 10.2 Tokyo added TThread.ForceQueue() to let
    // the specified method be queued even if invoked by the main thread!  So
    // lets re-enable use of TIdNotifyThread in earlier versions, to maintain
    // consistent notification behavior...

    if not MainThreadUsesNotify then
    begin
      {$IFNDEF USE_OBJECT_ARC}
      InternalDoNotify;
      {$ELSE}
      DoNotify;
      {$ENDIF}
    end else
    begin
      // TODO: if available, use TThread.CreateAnonymousThread() to call TThread.Queue()?
      //TThread.CreateAnonymousThread(Notify).Start;

      // TODO: FreePascal/Lazarus has Application.QueueAsyncCall(), but it is in the Forms unit!
      {
      uses Forms;

      procedure TIdNotify.NotifyAsync(Data: PtrInt);
      begin
        ($IFNDEF USE_OBJECT_ARC)
        InternalDoNotify;
        ($ELSE)
        DoNotify;
        ($ENDIF)
      end;

      Application.QueueAsyncCall(@NotifyAsync, 0);
      }

      {$IFNDEF USE_OBJECT_ARC}
      try
      {$ENDIF}
        CreateNotifyThread;
        GNotifyThread.AddNotification(Self);
      {$IFNDEF USE_OBJECT_ARC}
      except
        Free;
        raise;
      end;
      {$ENDIF}
    end;
  end else begin
    {$IFNDEF USE_OBJECT_ARC}
    try
    {$ENDIF}
      {$IFDEF HAS_STATIC_TThread_Queue}
      DoThreadQueue(
        {$IFNDEF USE_OBJECT_ARC}
        InternalDoNotify
        {$ELSE}
        DoNotify
        {$ENDIF}
      );
      {$ELSE}

      // TODO: FreePascal/Lazarus has Application.QueueAsyncCall(), but it is in the Forms unit!
      {
      uses Forms;

      procedure TIdNotify.NotifyAsync(Data: PtrInt);
      begin
        ($IFNDEF USE_OBJECT_ARC)
        InternalDoNotify;
        ($ELSE)
        DoNotify;
        ($ENDIF)
      end;

      Application.QueueAsyncCall(@NotifyAsync, 0);
      }

      CreateNotifyThread;
      GNotifyThread.AddNotification(Self);
      {$ENDIF}
    {$IFNDEF USE_OBJECT_ARC}
    except
      Free;
      raise;
    end;
    {$ENDIF}
  end;
  {$ENDIF}
end;

{$IFNDEF USE_OBJECT_ARC}
procedure TIdNotify.InternalDoNotify;
begin
  try
    DoNotify;
  finally
    Free;
  end;
end;
{$ENDIF}

class procedure TIdNotify.NotifyMethod(AMethod: TThreadMethod; AForceQueue: Boolean = False);
begin
  {$IFDEF HAS_STATIC_TThread_ForceQueue}
  DoThreadQueue(AMethod, AForceQueue);
  {$ELSE}
  if InMainThread then begin
    if not AForceQueue then begin
      AMethod;
    end else begin
      {$I IdSymbolDeprecatedOff.inc}
      with TIdNotifyMethod.Create(AMethod) do begin
        MainThreadUsesNotify := True;
        Notify;
      end;
      {$I IdSymbolDeprecatedOn.inc}
    end;
  end else begin
    {$IFDEF HAS_STATIC_TThread_Queue}
    DoThreadQueue(AMethod);
    {$ELSE}
    {$I IdSymbolDeprecatedOff.inc}
    TIdNotifyMethod.Create(AMethod).Notify;
    {$I IdSymbolDeprecatedOn.inc}
    {$ENDIF}
  end;
  {$ENDIF}
end;

{$IFNDEF HAS_STATIC_TThread_Queue}
// RLebeau: this method does not make sense.  The Self pointer is not
// guaranteed to remain valid while this method is running since the
// notify thread frees the object.  Also, this makes the calling thread
// block, so TIdSync should be used instead...

{$I IdDeprecatedImplBugOff.inc}
procedure TIdNotify.WaitFor;
{$I IdDeprecatedImplBugOn.inc}
var
  LNotifyIndex: Integer;
  LList: TIdNotifyList;
begin
  repeat
    LList := GNotifyThread.FNotifications.LockList;
    try
      LNotifyIndex := LList.IndexOf(Self);
    finally
      GNotifyThread.FNotifications.UnlockList;
    end;
    if LNotifyIndex = -1 then begin
      Break;
    end;
    IndySleep(10);
  until False;
end;

{$ENDIF}

{$IFDEF NotifyThreadNeeded}

{ TIdNotifyThread }

procedure TIdNotifyThread.AddNotification(ASync: TIdNotify);
begin
  FNotifications.Add(ASync);
  FEvent.SetEvent;
end;

constructor TIdNotifyThread.Create;
begin
  FEvent := TIdLocalEvent.Create;
  FNotifications := TIdNotifyThreadList.Create;
  // Must be before - Thread starts running when we call inherited
  inherited Create(False, False, 'IdNotify'); {do not localize}
end;

destructor TIdNotifyThread.Destroy;
var
  {$IFNDEF USE_OBJECT_ARC}
  LNotify: TIdNotify;
  {$ENDIF}
  LList: TIdNotifyList;
begin
  // Free remaining Notifications if there is something that is still in
  // the queue after thread was terminated
  LList := FNotifications.LockList;
  try
    {$IFDEF USE_OBJECT_ARC}
    LList.Clear; // Items are auto-freed
    {$ELSE}
    while LList.Count > 0 do begin
      LNotify := {$IFDEF HAS_GENERICS_TList}LList.Items[0]{$ELSE}TIdNotify(LList.Items[0]){$ENDIF};
      LList.Delete(0);
      LNotify.Free;
    end;
    {$ENDIF}
  finally
    FNotifications.UnlockList;
  end;
  FreeAndNil(FNotifications);
  FreeAndNil(FEvent);
  inherited Destroy;
end;

class procedure TIdNotifyThread.FreeThread;
begin
  if GNotifyThread <> nil then begin
    GNotifyThread.Stop;
    GNotifyThread.FEvent.SetEvent;
    GNotifyThread.WaitFor;
    // Instead of FreeOnTerminate so we can set the reference to nil
    FreeAndNil(GNotifyThread);
  end;
end;

procedure TIdNotifyThread.Run;
// NOTE: Be VERY careful with making changes to this proc. It is VERY delicate and the order
// of execution is very important. Small changes can have drastic effects
var
  LNotifications: TIdNotifyList;
  LNotify: TIdNotify;
begin
  FEvent.WaitForEver;

  // TODO: If TThread.Queue() is available, just queue the entire
  // FNotifications list to the main thread and exit. No sense in
  // locking and unlocking the list on every notification since we
  // will not be waiting on them here.
  //
  // Unlocking and relocking the list should only be needed if we
  // have to resort to using TThread.Synchronize(), so we don't block
  // other threads from queuing new notifications while a notification
  // is running...
  {
  ($IFDEF Use_DoThreadQueue)
  if not Stopped then begin
    try
      LNotifications := FNotifications.LockList;
      try
        while (LNotifications.Count > 0) and (not Stopped) do
        begin
          LNotify := ($IFDEF HAS_GENERICS_TList)LNotifications.Items[0]($ELSE)TIdNotify(LNotifications.Items[0])($ENDIF);
          LNotifications.Delete(0);
          ($IFNDEF USE_OBJECT_ARC)
          try
            DoThreadQueue(LNotify.InternalDoNotify);
          except
            FreeAndNil(LNotify);
            raise;
          end;
          ($ELSE)
          try
            DoThreadQueue(LNotify.DoNotify);
          finally
            LNotify := nil;
          end;
          ($ENDIF)
        end;
      finally
        FNotifications.UnlockList;
      end;
    except // Catch all exceptions especially these which are raised during the application close
    end;
  end;
  ($ENDIF)
  }

  // If terminated while waiting on the event or during the loop
  while not Stopped do begin
    try
      LNotifications := FNotifications.LockList;
      try
        if LNotifications.Count = 0 then begin
          Break;
        end;
        LNotify := {$IFDEF HAS_GENERICS_TList}LNotifications.Items[0]{$ELSE}TIdNotify(LNotifications.Items[0]){$ENDIF};
        LNotifications.Delete(0);
      finally
        FNotifications.UnlockList;
      end;
      {$IFDEF USE_DoThreadQueue}
        {$IFNDEF USE_OBJECT_ARC}
      try
        DoThreadQueue(LNotify.InternalDoNotify);
      except
        FreeAndNil(LNotify);
        raise;
      end;
        {$ELSE}
      try
        DoThreadQueue(LNotify.DoNotify);
      finally
        LNotify := nil;
      end;
        {$ENDIF}
      {$ELSE}
      try
        DoThreadSync(
          {$IFNDEF HAS_STATIC_TThread_Synchronize}Self,{$ENDIF}
          LNotify.DoNotify);
      finally
        FreeAndNil(LNotify);
      end;
      {$ENDIF}
    except // Catch all exceptions especially these which are raised during the application close
    end;
  end;
end;

{$ENDIF} // NotifyThreadNeeded

{ TIdNotifyMethod }

{$I IdDeprecatedImplBugOff.inc}
constructor TIdNotifyMethod.Create(AMethod: TThreadMethod);
{$I IdDeprecatedImplBugOn.inc}
begin
  inherited Create;
  FMethod := AMethod;
end;

{$I IdDeprecatedImplBugOff.inc}
procedure TIdNotifyMethod.DoNotify;
{$I IdDeprecatedImplBugOn.inc}
begin
  FMethod;
end;

{$IFDEF NotifyThreadNeeded}
initialization
  //CreateNotifyThread; // created on demand
finalization
  TIdNotifyThread.FreeThread;
{$ENDIF}

end.


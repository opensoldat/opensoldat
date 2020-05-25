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
  Rev 1.12    2004.02.03 4:17:06 PM  czhower
  For unit name changes.

  Rev 1.11    2003.10.24 12:59:20 PM  czhower
  Name change

  Rev 1.10    2003.10.21 12:19:00 AM  czhower
  TIdTask support and fiber bug fixes.

  Rev 1.9    2003.10.11 5:49:50 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.8    2003.09.19 10:11:20 PM  czhower
  Next stage of fiber support in servers.

  Rev 1.7    2003.09.19 11:54:32 AM  czhower
  -Completed more features necessary for servers
  -Fixed some bugs

  Rev 1.6    2003.09.18 4:10:26 PM  czhower
  Preliminary changes for Yarn support.

  Rev 1.5    7/6/2003 8:04:08 PM  BGooijen
  Renamed IdScheduler* to IdSchedulerOf*

  Rev 1.4    7/5/2003 11:49:06 PM  BGooijen
  Cleaned up and fixed av in threadpool

  Rev 1.3    4/15/2003 10:56:08 PM  BGooijen
  fixes

  Rev 1.2    3/13/2003 10:18:34 AM  BGooijen
  Server side fibers, bug fixes

  Rev 1.1    1/23/2003 7:28:46 PM  BGooijen

  Rev 1.0    1/17/2003 03:29:58 PM  JPMugaas
  Renamed from ThreadMgr for new design.

  Rev 1.0    11/13/2002 09:01:46 AM  JPMugaas

  2002-06-23 -Andrew P.Rybin
    -2 deadlock fix (and also in IdThread)
}

unit IdSchedulerOfThreadPool;

interface

{$i IdCompilerDefines.inc}

uses
  {$IFDEF HAS_UNIT_Generics_Collections}
  System.Generics.Collections,
  {$ELSE}
  Classes,
  {$ENDIF}
  IdContext,
  IdScheduler,
  IdSchedulerOfThread,
  IdThread,
  //IdThreadSafe,
  IdYarn;

type
  {$IFDEF HAS_GENERICS_TThreadList}
  TIdPoolThreadList = TThreadList<TIdThreadWithTask>;
  TIdPoolList = TList<TIdThreadWithTask>;
  {$ELSE}
  // TODO: flesh out to match TThreadList<TIdThreadWithTask> and TList<TIdThreadWithTask> for non-Generics compilers
  TIdPoolThreadList = TThreadList;
  TIdPoolList = TList;
  {$ENDIF}

  TIdSchedulerOfThreadPool = class(TIdSchedulerOfThread)
  protected
    FPoolSize: Integer;
    FThreadPool: TIdPoolThreadList;
    procedure InitComponent; override;
  public
    destructor Destroy; override;
    function AcquireYarn: TIdYarn; override;
    procedure Init; override;
    function NewThread: TIdThreadWithTask; override;
    procedure ReleaseYarn(AYarn: TIdYarn); override;
    procedure TerminateAllYarns; override;
  published
    //TODO: Poolsize is only looked at during loading and when threads are
    // needed. Probably should add an Active property to schedulers like
    // servers have.
    property PoolSize: Integer read FPoolSize write FPoolSize default 0;
  End;

implementation

uses
  {$IFDEF VCL_2010_OR_ABOVE}
    {$IFDEF WINDOWS}
  Windows,
    {$ENDIF}
  {$ENDIF}
  IdGlobal, SysUtils;

type
  TIdYarnOfThreadAccess = class(TIdYarnOfThread)
  end;

destructor TIdSchedulerOfThreadPool.Destroy;
begin
  inherited Destroy;
  // Must be after, inherited calls TerminateThreads
  FreeAndNil(FThreadPool);
end;

function TIdSchedulerOfThreadPool.AcquireYarn: TIdYarn;
var
  LThread: TIdThreadWithTask;
  LList: TIdPoolList;
begin
  LList := FThreadPool.LockList;
  try
    if LList.Count > 0 then begin
      LThread := {$IFDEF HAS_GENERICS_TList}LList.Items[0]{$ELSE}TIdThreadWithTask(LList.Items[0]){$ENDIF};
      LList.Delete(0);
    end else begin
      LThread := nil;
    end;
  finally
    FThreadPool.UnlockList;
  end;

  if LThread = nil then begin
    LThread := NewThread;
  end;

  Result := NewYarn(LThread);
  ActiveYarns.Add(Result);
end;

procedure TIdSchedulerOfThreadPool.ReleaseYarn(AYarn: TIdYarn);
//only gets called from YarnOf(Fiber/Thread).Destroy
var
  LThread: TIdThreadWithTask;
  LList: TIdPoolList;
begin
  //take posession of the thread
  LThread := TIdYarnOfThread(AYarn).Thread;
  TIdYarnOfThreadAccess(AYarn).FThread := nil;
  //Currently LThread can =nil. Is that a valid condition?
  //Assert(LThread<>nil);

  // inherited removes from ActiveYarns list
  inherited ReleaseYarn(AYarn);

  if LThread <> nil then begin
    // need to redeposit the thread in the pool or destroy it
    LThread.Yarn := nil; // Yarn is being destroyed, de-couple it from the thread
    LList := FThreadPool.LockList;
    try
      if (LList.Count < PoolSize) and (not LThread.Terminated) then begin
        LList.Add(LThread);
        Exit;
      end;
    finally
      FThreadPool.UnlockList;
    end;
    LThread.Terminate;
    // RLebeau - ReleaseYarn() can be called in the context of
    // the yarn's thread (when TIdThread.Cleanup() destroys the
    // yarn between connnections), so have to check which context
    // we're in here so as not to deadlock the thread!
    if IsCurrentThread(LThread) then begin
      LThread.FreeOnTerminate := True;
    end else begin
      {$IFDEF DEPRECATED_TThread_SuspendResume}
      LThread.Suspended := False;
      {$ELSE}
      LThread.Resume;
      {$ENDIF}
      LThread.WaitFor;
      LThread.Free;
    end;
  end;
end;

procedure TIdSchedulerOfThreadPool.TerminateAllYarns;
var
  LThread: TIdThreadWithTask;
  LList: TIdPoolList;
begin
  // inherited will kill off ActiveYarns
  inherited TerminateAllYarns;
  // ThreadPool is nil if never Initted
  if FThreadPool <> nil then begin
    // Now we have to kill off the pooled threads
    LList := FThreadPool.LockList;
    try
      while LList.Count > 0 do begin
        LThread := {$IFDEF HAS_GENERICS_TList}LList.Items[0]{$ELSE}TIdThreadWithTask(LList.Items[0]){$ENDIF};
        LThread.Terminate;
        {$IFDEF DEPRECATED_TThread_SuspendResume}
        LThread.Suspended := False;
        {$ELSE}
        LThread.Resume;
        {$ENDIF}
        LThread.WaitFor;
        LThread.Free;
        LList.Delete(0);
      end;
    finally
      FThreadPool.UnlockList;
    end;
  end;
end;

procedure TIdSchedulerOfThreadPool.Init;
var
  LList: TIdPoolList;
begin
  inherited Init;
  Assert(FThreadPool<>nil);

  if not IsDesignTime then begin
    if PoolSize > 0 then begin
      LList := FThreadPool.LockList;
      try
        while LList.Count < PoolSize do begin
          LList.Add(NewThread);
        end;
      finally
        FThreadPool.UnlockList;
      end;
    end;
  end;
end;

function TIdSchedulerOfThreadPool.NewThread: TIdThreadWithTask;
begin
  Result := inherited NewThread;
  Result.StopMode := smSuspend;
end;

procedure TIdSchedulerOfThreadPool.InitComponent;
begin
  inherited;
  FThreadPool := TIdPoolThreadList.Create;
end;

end.

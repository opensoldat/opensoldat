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
  Rev 1.32    3/23/2005 8:20:18 AM  JPMugaas
  Temp fix for a double-free problem causing an AV.  I will explain on Core.

  Rev 1.31    6/11/2004 8:48:32 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.30    2004.03.01 5:12:38 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.29    2004.02.03 4:17:04 PM  czhower
  For unit name changes.

  Rev 1.28    2004.01.22 5:59:14 PM  czhower
  IdCriticalSection

  Rev 1.27    2004.01.20 10:03:32 PM  czhower
  InitComponent

  Rev 1.26    6/11/2003 8:28:42 PM  GGrieve
  remove wrong call to inherited StartYarn

  Rev 1.25    2003.10.24 12:59:18 PM  czhower
  Name change

  Rev 1.24    2003.10.21 12:19:00 AM  czhower
  TIdTask support and fiber bug fixes.

  Rev 1.23    10/15/2003 8:35:30 PM  DSiders
  Added resource string for exception raised in TIdSchedulerOfThread.NewYarn.

  Rev 1.22    2003.10.14 11:18:10 PM  czhower
  Fix for AV on shutdown and other bugs

  Rev 1.21    2003.10.11 5:49:32 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.20    2003.09.19 10:11:18 PM  czhower
  Next stage of fiber support in servers.

  Rev 1.19    2003.09.19 11:54:30 AM  czhower
  -Completed more features necessary for servers
  -Fixed some bugs

  Rev 1.18    2003.09.18 4:43:18 PM  czhower
  -Removed IdBaseThread
  -Threads now have default names

  Rev 1.17    2003.09.18 4:10:26 PM  czhower
  Preliminary changes for Yarn support.

  Rev 1.16    2003.07.17 1:08:04 PM  czhower
  Fixed warning

  Rev 1.15    7/6/2003 8:04:06 PM  BGooijen
  Renamed IdScheduler* to IdSchedulerOf*

  Rev 1.14    7/5/2003 11:49:06 PM  BGooijen
  Cleaned up and fixed av in threadpool

  Rev 1.13    2003.06.30 9:39:44 PM  czhower
  Comments and small change.

  Rev 1.12    6/25/2003 3:54:02 PM  BGooijen
  Destructor waits now until all threads are terminated

  Rev 1.11    2003.06.25 4:27:02 PM  czhower
  Fixed some formatting and fixed one line ifs.

  Rev 1.10    4/11/2003 6:35:28 PM  BGooijen

  Rev 1.9    3/27/2003 5:17:22 PM  BGooijen
  Moved some code to TIdScheduler, made ThreadPriority published

  Rev 1.8    3/22/2003 1:49:38 PM  BGooijen
  Fixed warnings (.ShouldStop)

  Rev 1.7    3/13/2003 10:18:30 AM  BGooijen
  Server side fibers, bug fixes

  Rev 1.6    1/23/2003 11:55:24 PM  BGooijen

  Rev 1.5    1/23/2003 8:32:40 PM  BGooijen
  Added termination handler

  Rev 1.3    1/23/2003 11:05:58 AM  BGooijen

  Rev 1.2    1-17-2003 23:22:16  BGooijen
  added MaxThreads property

  Rev 1.1    1/17/2003 03:43:04 PM  JPMugaas
  Updated to use new class.

  Rev 1.0    1/17/2003 03:29:50 PM  JPMugaas
  Renamed from ThreadMgr for new design.

  Rev 1.0    11/13/2002 09:01:32 AM  JPMugaas

  02 Oct 2001 - Allen O'Neill
  Added support for thread priority - new property Threadpriority,
  new line added to OnCreate
}

unit IdSchedulerOfThread;

interface
{$i IdCompilerDefines.inc}

uses
  Classes,
  IdException, IdBaseComponent, IdGlobal, IdScheduler, 
  IdThread, IdTask, IdYarn;

type
  TIdYarnOfThread = class(TIdYarn)
  protected
    // TODO: should these be [Weak] on ARC systems?
    FScheduler: TIdScheduler;
    FThread: TIdThreadWithTask;
  public
    constructor Create(AScheduler: TIdScheduler; AThread: TIdThreadWithTask); reintroduce;
    destructor Destroy; override;
    //
    property Thread: TIdThreadWithTask read FThread;
  end;

  TIdSchedulerOfThread = class(TIdScheduler)
  protected
    FMaxThreads: Integer;
    FThreadPriority: TIdThreadPriority;
    FThreadClass: TIdThreadWithTaskClass;
    //
    procedure InitComponent; override;
  public
    destructor Destroy; override;
    function NewThread: TIdThreadWithTask; virtual;
    function NewYarn(AThread: TIdThreadWithTask = nil): TIdYarnOfThread;
    procedure StartYarn(AYarn: TIdYarn; ATask: TIdTask); override;
    procedure TerminateYarn(AYarn: TIdYarn); override;
    property ThreadClass: TIdThreadWithTaskClass read FThreadClass write FThreadClass;
  published
    property MaxThreads: Integer read FMaxThreads write FMaxThreads;
    property ThreadPriority: TIdThreadPriority read FThreadPriority write FThreadPriority default tpNormal;
  end;

implementation

uses
  {$IFDEF KYLIXCOMPAT}
  Libc,
  {$ENDIF}
  IdResourceStringsCore, IdTCPServer, IdThreadSafe, IdExceptionCore, SysUtils;

{ TIdSchedulerOfThread }

destructor TIdSchedulerOfThread.Destroy;
begin
  TerminateAllYarns;
  inherited Destroy;
end;

procedure TIdSchedulerOfThread.StartYarn(AYarn: TIdYarn; ATask: TIdTask);
var
  LThread: TIdThreadWithTask;
begin
  LThread := TIdYarnOfThread(AYarn).Thread;
  LThread.Task := ATask;
  LThread.Start;
end;

function TIdSchedulerOfThread.NewThread: TIdThreadWithTask;
begin
  Assert(FThreadClass<>nil);
  if (FMaxThreads <> 0) and (not ActiveYarns.IsCountLessThan(FMaxThreads + 1)) then begin
    raise EIdSchedulerMaxThreadsExceeded.Create(RSchedMaxThreadEx);
  end;
  Result := FThreadClass.Create(nil, IndyFormat('%s User', [Name])); {do not localize}
  if ThreadPriority <> tpNormal then begin
    IndySetThreadPriority(Result, ThreadPriority);
  end;
end;

function TIdSchedulerOfThread.NewYarn(AThread: TIdThreadWithTask): TIdYarnOfThread;
begin
  if not Assigned(AThread) then begin
    raise EIdException.Create(RSThreadSchedulerThreadRequired);
  end;
  // Create Yarn
  Result := TIdYarnOfThread.Create(Self, AThread);
end;

procedure TIdSchedulerOfThread.TerminateYarn(AYarn: TIdYarn);
var
  LYarn: TIdYarnOfThread;
begin
  Assert(AYarn<>nil);
  LYarn := TIdYarnOfThread(AYarn);
  if (LYarn.Thread <> nil) and (not LYarn.Thread.Suspended) then begin
    // Is still running and will free itself
    LYarn.Thread.Stop;
    // Dont free the yarn. The thread frees it (IdThread.pas)
  end else
  begin
    // If suspended, was created but never started
    // ie waiting on connection accept

    // RLebeau: free the yarn here as well. This allows TIdSchedulerOfThreadPool
    // to put the suspended thread, if present, back in the pool.

    IdDisposeAndNil(LYarn);
  end;
end;

procedure TIdSchedulerOfThread.InitComponent;
begin
  inherited InitComponent;
  FThreadPriority := tpNormal;
  FMaxThreads := 0;
  FThreadClass := TIdThreadWithTask;
end;

{ TIdYarnOfThread }

constructor TIdYarnOfThread.Create(
  AScheduler: TIdScheduler;
  AThread: TIdThreadWithTask
  );
begin
  inherited Create;
  FScheduler := AScheduler;
  FThread := AThread;
  AThread.Yarn := Self;
end;

destructor TIdYarnOfThread.Destroy;
begin
  FScheduler.ReleaseYarn(Self);
  inherited Destroy;
end;

end.

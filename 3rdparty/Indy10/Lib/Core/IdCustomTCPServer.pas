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
  Rev 1.1    1/15/05 2:23:00 PM  RLebeau
  Comment added to SetScheduler()

  Rev 1.0    12/2/2004 3:26:32 PM  JPMugaas
  Moved most of TIdTCPServer here so we can use TIdTCPServer as an end point
  which requires an OnExecute event.

  Rev 1.68    11/29/04 11:50:26 PM  RLebeau
  Updated ContextDisconected() to call DoDisconnect()

  Rev 1.67    11/27/04 3:28:36 AM  RLebeau
  Updated to automatically set up the client IOHandler before calling
  DoConnect(), and to tear the IOHandler down before calling OnDisconnect().

  Rev 1.66    10/8/2004 10:11:02 PM  BGooijen
  uncommented intercept code

  Rev 1.65    2004.08.13 10:55:38  czhower
  Removed IFDEF

  Rev 1.64    08.08.2004 10:43:10  OMonien
  temporary Thread.priority fix for Kylix

  Rev 1.63    6/11/2004 12:41:52 PM  JPMugaas
  Reuse Address now reenabled.

    Rev 1.62    6/1/2004 1:22:28 PM  DSiders
  Added TODO for TerminateWaitTimeout.

  Rev 1.61    28/04/2004 15:54:40  HHariri
  Changed thread priority for scheduler

  Rev 1.60    2004.04.22 11:44:48 PM  czhower
  Boosted thread priority of listener thread.

  Rev 1.59    2004.03.06 10:40:34 PM  czhower
  Changed IOHandler management to fix bug in server shutdowns.

  Rev 1.58    2004.03.01 5:12:40 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.57    2004.02.03 4:16:56 PM  czhower
  For unit name changes.

  Rev 1.56    2004.01.20 10:03:36 PM  czhower
  InitComponent

  Rev 1.55    1/3/2004 11:49:30 PM  BGooijen
  the server creates a default binding for IPv6 now too, if IPv6 is supported

  Rev 1.54    2003.12.28 8:04:54 PM  czhower
  Shutdown fix for .net.

  Rev 1.53    2003.11.29 6:03:46 PM  czhower
  Active = True now works when set at design time.

  Rev 1.52    2003.10.21 12:19:02 AM  czhower
  TIdTask support and fiber bug fixes.

  Rev 1.51    2003.10.18 9:33:30 PM  czhower
  Boatload of bug fixes to command handlers.

  Rev 1.50    2003.10.18 8:04:28 PM  czhower
  Fixed bug with setting active at design time.

    Rev 1.49    10/15/2003 11:10:00 PM  DSiders
  Added localization comments.
  Added resource srting for exception raised in TIdTCPServer.SetScheduler.

  Rev 1.48    2003.10.15 4:34:38 PM  czhower
  Bug fix for shutdown.

  Rev 1.47    2003.10.14 11:18:12 PM  czhower
  Fix for AV on shutdown and other bugs

  Rev 1.46    2003.10.11 5:51:38 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.45    10/5/2003 9:55:26 PM  BGooijen
  TIdTCPServer works on D7 and DotNet now

  Rev 1.44    10/5/2003 03:07:48 AM  JPMugaas
  Should compile.

  Rev 1.43    2003.10.01 9:11:28 PM  czhower
  .Net

  Rev 1.42    2003.09.30 1:23:08 PM  czhower
  Stack split for DotNet

  Rev 1.41    2003.09.19 10:11:22 PM  czhower
  Next stage of fiber support in servers.

  Rev 1.40    2003.09.19 11:54:34 AM  czhower
  -Completed more features necessary for servers
  -Fixed some bugs

  Rev 1.39    2003.09.18 4:43:18 PM  czhower
  -Removed IdBaseThread
  -Threads now have default names

    Rev 1.37    7/6/2003 8:04:10 PM  BGooijen
  Renamed IdScheduler* to IdSchedulerOf*

  Rev 1.36    2003.06.30 9:41:06 PM  czhower
  Fix for AV during server shut down.

    Rev 1.35    6/25/2003 3:57:58 PM  BGooijen
  Disconnecting the context is now inside try...except

    Rev 1.34    6/8/2003 2:13:02 PM  BGooijen
  Made ContextClass public

    Rev 1.33    6/5/2003 12:43:26 PM  BGooijen
  changed short circuit fix code

  Rev 1.32    2003.06.04 10:14:08 AM  czhower
  Removed short circuit dependency and fixed some older irrelevant code.

    Rev 1.31    6/3/2003 11:49:38 PM  BGooijen
  removed AV in TIdTCPServer.DoExecute (hopefully)

  Rev 1.30    5/26/2003 04:29:58 PM  JPMugaas
  Removed GenerateReply and ParseReply.  Those are now obsolete duplicate
  functions in the new design.

  Rev 1.29    2003.05.26 10:35:26 PM  czhower
  Fixed spelling typo.

  Rev 1.28    5/26/2003 12:20:00 PM  JPMugaas

  Rev 1.27    2003.05.26 11:38:22 AM  czhower

  Rev 1.26    5/25/2003 03:38:04 AM  JPMugaas

  Rev 1.25    5/25/2003 03:26:38 AM  JPMugaas

    Rev 1.24    5/20/2003 12:43:52 AM  BGooijen
  changeable reply types

    Rev 1.23    5/13/2003 2:56:40 PM  BGooijen
  changed GetGreating to SendGreeting

    Rev 1.21    4/4/2003 8:09:46 PM  BGooijen
  moved some consts tidcmdtcpserver, changed DoExecute to return
  .connection.connected

    Rev 1.20    3/25/2003 9:04:06 PM  BGooijen
  Scheduler in IOHandler is now updated when the scheduler is removed

    Rev 1.19    3/23/2003 11:33:34 PM  BGooijen
  Updates the scheduler in the iohandler when scheduler/iohandler is changed

    Rev 1.18    3/22/2003 11:44:08 PM  BGooijen
  ServerIntercept now logs connects/disconnects

    Rev 1.17    3/22/2003 1:46:02 PM  BGooijen
  Better handling of exceptions in TIdListenerThread.Run (could cause mem leaks
  first (in non-paged-memory))

    Rev 1.16    3/21/2003 5:55:54 PM  BGooijen
  Added code for serverIntercept

  Rev 1.15    3/21/2003 11:44:00 AM  JPMugaas
  Updated with a OnBeforeConnect event for the TIdMappedPort components.

    Rev 1.14    3/20/2003 12:18:32 PM  BGooijen
  Moved ReplyExceptionCode from TIdTCPServer to TIdCmdTCPServer

    Rev 1.13    3/13/2003 10:18:26 AM  BGooijen
  Server side fibers, bug fixes

  Rev 1.12    2003.02.18 5:52:16 PM  czhower
  Fix for warnings and logic error.

    Rev 1.11    1/23/2003 8:33:16 PM  BGooijen

    Rev 1.10    1/23/2003 11:05:48 AM  BGooijen

    Rev 1.9    1/20/2003 12:50:44 PM  BGooijen
  Added a Contexts propperty, which contains all contexts for that server
  Moved the commandhandlers to TIdCmdTCPServer

  Rev 1.8    1-18-2003 0:00:30  BGooijen
  Removed TIdContext.OnCreate
  Added ContextCreated

  Rev 1.7    1-17-2003 23:44:32  BGooijen
  added support code for TIdContext.OnCreate

  Rev 1.6    1-17-2003 22:22:10  BGooijen
  new design

  Rev 1.5    1-10-2003 23:59:22  BGooijen
  Connection is now freed in destructor of TIdContext

  Rev 1.4    1-10-2003 19:46:22  BGooijen
  The context was not freed, now it is

  Rev 1.3    1-9-2003 11:52:28  BGooijen
  changed construction of TIdContext to Create(AServer: TIdTCPServer)
  added TIdContext property .Server

  Rev 1.2    1-3-2003 19:05:56  BGooijen
  added FContextClass:TIdContextClass to TIdTcpServer
  added Data:TObject to TIdContext

  Rev 1.1    1-1-2003 16:42:10  BGooijen
  Changed TIdThread to TIdYarn
  Added TIdContext

  Rev 1.0    11/13/2002 09:00:42 AM  JPMugaas

2002-01-01 - Andrew P.Rybin
 - bug fix (MaxConnections, SetActive(FALSE)), TerminateListenerThreads, DoExecute

2002-04-17 - Andrew P.Rybin
 - bug fix: if exception raised in OnConnect, Threads.Remove and ThreadMgr.ReleaseThread are not called
}

unit IdCustomTCPServer;

{
  Original Author and Maintainer:
  - Chad Z. Hower a.k.a Kudzu
}

interface

{$I IdCompilerDefines.inc}
//here to flip FPC into Delphi mode

uses
  Classes,
  {$IFDEF HAS_UNIT_Generics_Collections}
  System.Generics.Collections,
  {$ENDIF}
  IdBaseComponent,
  IdComponent,IdContext, IdGlobal, IdException,
  IdIntercept, IdIOHandler, IdIOHandlerStack,
  IdReply, IdScheduler, IdSchedulerOfThread, IdServerIOHandler,
  IdServerIOHandlerStack, IdSocketHandle, IdStackConsts, IdTCPConnection,
  IdThread, IdYarn, SysUtils;

const
  IdListenQueueDefault = 15;

type
  TIdCustomTCPServer = class;

  // This is the thread that listens for incoming connections and spawns
  // new ones to handle each one
  TIdListenerThread = class(TIdThread)
  protected
    FBinding: TIdSocketHandle;
    FServer: TIdCustomTCPServer;
    FOnBeforeRun: TIdNotifyThreadEvent;
    //
    procedure AfterRun; override;
    procedure BeforeRun; override;
    procedure Run; override;
  public
    constructor Create(AServer: TIdCustomTCPServer; ABinding: TIdSocketHandle); reintroduce;
    //
    property Binding: TIdSocketHandle read FBinding;
    property Server: TIdCustomTCPServer read FServer;
    property OnBeforeRun: TIdNotifyThreadEvent read FOnBeforeRun write FOnBeforeRun;
  End;

  {$IFDEF HAS_GENERICS_TThreadList}
  TIdListenerThreadList = TThreadList<TIdListenerThread>;
  TIdListenerList = TList<TIdListenerThread>;
  {$ELSE}
  // TODO: flesh out to match TThreadList<TIdListenerThread> and TList<TIdListenerThread> for non-Generics compilers
  TIdListenerThreadList = TThreadList;
  TIdListenerList = TList;
  {$ENDIF}

  TIdListenExceptionEvent = procedure(AThread: TIdListenerThread; AException: Exception) of object;
  TIdServerThreadExceptionEvent = procedure(AContext: TIdContext; AException: Exception) of object;
  TIdServerThreadEvent = procedure(AContext: TIdContext) of object;

  TIdServerContext = class(TIdContext)
  protected
    FServer: TIdCustomTCPServer;
  public
    property Server: TIdCustomTCPServer read FServer;
  end;

  TIdServerContextClass = class of TIdServerContext;

  TIdCustomTCPServer = class(TIdComponent)
  protected
    FActive: Boolean;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FScheduler: TIdScheduler;
    FBindings: TIdSocketHandles;
    FContextClass: TIdServerContextClass;
    FImplicitScheduler: Boolean;
    FImplicitIOHandler: Boolean;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FIntercept: TIdServerIntercept;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FIOHandler: TIdServerIOHandler;
    FListenerThreads: TIdListenerThreadList;
    FListenQueue: integer;
    FMaxConnections: Integer;
    FReuseSocket: TIdReuseSocket;
    FTerminateWaitTime: Integer;
    FContexts: TIdContextThreadList;
    FOnContextCreated: TIdServerThreadEvent;
    FOnConnect: TIdServerThreadEvent;
    FOnDisconnect: TIdServerThreadEvent;
    FOnException: TIdServerThreadExceptionEvent;
    FOnExecute: TIdServerThreadEvent;
    FOnListenException: TIdListenExceptionEvent;
    FOnBeforeBind: TIdSocketHandleEvent;
    FOnAfterBind: TNotifyEvent;
    FOnBeforeListenerRun: TIdNotifyThreadEvent;
    FUseNagle : Boolean;
    //
    procedure CheckActive;
    procedure CheckOkToBeActive;  virtual;
    procedure ContextCreated(AContext: TIdContext); virtual;
    procedure ContextConnected(AContext: TIdContext); virtual;
    procedure ContextDisconnected(AContext: TIdContext); virtual;
    function CreateConnection: TIdTCPConnection; virtual;
    procedure DoBeforeBind(AHandle: TIdSocketHandle); virtual;
    procedure DoAfterBind; virtual;
    procedure DoBeforeListenerRun(AThread: TIdThread); virtual;
    procedure DoConnect(AContext: TIdContext); virtual;
    procedure DoDisconnect(AContext: TIdContext); virtual;
    procedure DoException(AContext: TIdContext; AException: Exception); virtual;
    function DoExecute(AContext: TIdContext): Boolean; virtual;
    procedure DoListenException(AThread: TIdListenerThread; AException: Exception); virtual;
    procedure DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler); virtual;
    procedure DoTerminateContext(AContext: TIdContext); virtual;
    function GetDefaultPort: TIdPort;
    procedure InitComponent; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // This is needed for POP3's APOP authentication.  For that,
    // you send a unique challenge to the client dynamically.
    procedure SendGreeting(AContext: TIdContext; AGreeting: TIdReply); virtual;
    procedure SetActive(AValue: Boolean); virtual;
    procedure SetBindings(const AValue: TIdSocketHandles); virtual;
    procedure SetDefaultPort(const AValue: TIdPort); virtual;
    procedure SetIntercept(const AValue: TIdServerIntercept); virtual;
    procedure SetIOHandler(const AValue: TIdServerIOHandler); virtual;
    procedure SetScheduler(const AValue: TIdScheduler); virtual;
    procedure Startup; virtual;
    procedure Shutdown; virtual;
    procedure TerminateAllThreads;
    // Occurs in the context of the peer thread
    property OnExecute: TIdServerThreadEvent read FOnExecute write FOnExecute;

  public
    destructor Destroy; override;
    //
    procedure StartListening;
    procedure StopListening;
    //
    property Contexts: TIdContextThreadList read FContexts;
    property ContextClass: TIdServerContextClass read FContextClass write FContextClass;
    property ImplicitIOHandler: Boolean read FImplicitIOHandler;
    property ImplicitScheduler: Boolean read FImplicitScheduler;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Bindings: TIdSocketHandles read FBindings write SetBindings;
    property DefaultPort: TIdPort read GetDefaultPort write SetDefaultPort;
    property Intercept: TIdServerIntercept read FIntercept write SetIntercept;
    property IOHandler: TIdServerIOHandler read FIOHandler write SetIOHandler;
    property ListenQueue: integer read FListenQueue write FListenQueue default IdListenQueueDefault;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections default 0;
    // right before/after binding sockets
    property OnBeforeBind: TIdSocketHandleEvent read FOnBeforeBind write FOnBeforeBind;
    property OnAfterBind: TNotifyEvent read FOnAfterBind write FOnAfterBind;
    property OnBeforeListenerRun: TIdNotifyThreadEvent read FOnBeforeListenerRun write FOnBeforeListenerRun;
    property OnContextCreated: TIdServerThreadEvent read FOnContextCreated write FOnContextCreated;
    // Occurs in the context of the peer thread
    property OnConnect: TIdServerThreadEvent read FOnConnect write FOnConnect;
    // Occurs in the context of the peer thread
    property OnDisconnect: TIdServerThreadEvent read FOnDisconnect write FOnDisconnect;
    // Occurs in the context of the peer thread
    property OnException: TIdServerThreadExceptionEvent read FOnException write FOnException;
    property OnListenException: TIdListenExceptionEvent read FOnListenException write FOnListenException;
    property ReuseSocket: TIdReuseSocket read FReuseSocket write FReuseSocket default rsOSDependent; // {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use TIdSocketHandle.ReuseSocket'{$ENDIF};{$ENDIF}
//UseNagle should be set to true in most cases.
//See: http://tangentsoft.net/wskfaq/intermediate.html#disable-nagle and
//   http://tangentsoft.net/wskfaq/articles/lame-list.html#item19
//The Nagle algorithm reduces the amount of needless traffic.  Disabling Nagle
//program’s throughput to degrade.
    property UseNagle: boolean read FUseNagle write FUseNagle default true;
    property TerminateWaitTime: Integer read FTerminateWaitTime write FTerminateWaitTime default 5000;
    property Scheduler: TIdScheduler read FScheduler write SetScheduler;
  end;

  EIdTCPServerError = class(EIdException);
  EIdNoExecuteSpecified = class(EIdTCPServerError);
  EIdTerminateThreadTimeout = class(EIdTCPServerError);

implementation

uses
  {$IFDEF VCL_2010_OR_ABOVE}
    {$IFDEF WINDOWS}
  Windows,
    {$ENDIF}
  {$ENDIF}
  IdGlobalCore,
  IdResourceStringsCore, IdReplyRFC,
  IdSchedulerOfThreadDefault, IdStack,
  IdThreadSafe;

{ TIdCustomTCPServer }

procedure TIdCustomTCPServer.CheckActive;
begin
  if Active and (not IsDesignTime) and (not IsLoading) then begin
    raise EIdTCPServerError.Create(RSCannotPerformTaskWhileServerIsActive);
  end;
end;

procedure TIdCustomTCPServer.ContextCreated(AContext: TIdContext);
begin
  if Assigned(FOnContextCreated) then begin
    FOnContextCreated(AContext);
  end;
end;

destructor TIdCustomTCPServer.Destroy;
begin
  Active := False;

  SetIOHandler(nil);

  // Destroy bindings first
  FreeAndNil(FBindings);
  //
  FreeAndNil(FContexts);
  FreeAndNil(FListenerThreads);
  //
  inherited Destroy;
end;

procedure TIdCustomTCPServer.DoBeforeBind(AHandle: TIdSocketHandle);
begin
  if Assigned(FOnBeforeBind) then begin
    FOnBeforeBind(AHandle);
  end;
end;

procedure TIdCustomTCPServer.DoAfterBind;
begin
  if Assigned(FOnAfterBind) then begin
    FOnAfterBind(Self);
  end;
end;

procedure TIdCustomTCPServer.SendGreeting(AContext: TIdContext; AGreeting: TIdReply);
begin
  AContext.Connection.IOHandler.Write(AGreeting.FormattedReply);
end;

procedure TIdCustomTCPServer.ContextConnected(AContext: TIdContext);
var
  // under ARC, convert weak references to strong references before working with them
  LServerIntercept: TIdServerIntercept;
  LConnIntercept: TIdConnectionIntercept;
begin
  LServerIntercept := Intercept;
  if Assigned(LServerIntercept) then begin
    LConnIntercept := LServerIntercept.Accept(AContext.Connection);
    AContext.Connection.IOHandler.Intercept := LConnIntercept;
    if Assigned(LConnIntercept) then begin
      LConnIntercept.Connect(AContext.Connection);
    end;
  end;
  DoConnect(AContext);
end;

procedure TIdCustomTCPServer.ContextDisconnected(AContext: TIdContext);
var
  // under ARC, convert weak references to strong references before working with them
  LIOHandler: TIdIOHandler;
  LIntercept: TIdConnectionIntercept;
begin
  DoDisconnect(AContext);
  LIOHandler := AContext.Connection.IOHandler;
  if Assigned(LIOHandler) then begin
    LIntercept := LIOHandler.Intercept;
    if Assigned(LIntercept) then begin
      LIntercept.Disconnect;
      FreeAndNil(LIntercept);
      LIOHandler.Intercept := nil;
    end;
  end;
end;

function TIdCustomTCPServer.CreateConnection: TIdTCPConnection;
begin
  Result := TIdTCPConnection.Create(nil);
end;

procedure TIdCustomTCPServer.DoConnect(AContext: TIdContext);
begin
  if Assigned(OnConnect) then begin
    OnConnect(AContext);
  end;
end;

procedure TIdCustomTCPServer.DoDisconnect(AContext: TIdContext);
begin
  if Assigned(OnDisconnect) then begin
    OnDisconnect(AContext);
  end;
end;

procedure TIdCustomTCPServer.DoException(AContext: TIdContext; AException: Exception);
begin
  if Assigned(OnException) then begin
    OnException(AContext, AException);
  end;
end;

function TIdCustomTCPServer.DoExecute(AContext: TIdContext): Boolean;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LConn: TIdTCPConnection;
begin
  if Assigned(OnExecute) then begin
    OnExecute(AContext);
  end;
  Result := False;
  if AContext <> nil then begin
    LConn := AContext.Connection;
    if LConn <> nil then begin
      Result := LConn.Connected;
    end;
  end;
end;

procedure TIdCustomTCPServer.DoListenException(AThread: TIdListenerThread; AException: Exception);
begin
  if Assigned(FOnListenException) then begin
    FOnListenException(AThread, AException);
  end;
end;

function TIdCustomTCPServer.GetDefaultPort: TIdPort;
begin
  Result := FBindings.DefaultPort;
end;

procedure TIdCustomTCPServer.Loaded;
begin
  inherited Loaded;
  // Active = True must not be performed before all other props are loaded
  if Active then begin
    FActive := False;
    Active := True;
  end;
end;

// under ARC, all weak references to a freed object get nil'ed automatically
// so this is mostly redundant
procedure TIdCustomTCPServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Remove the reference to the linked components if they are deleted
  if (Operation = opRemove) then begin
    if (AComponent = FScheduler) then begin
      FScheduler := nil;
      FImplicitScheduler := False;
    end
    else if (AComponent = FIntercept) then begin
      FIntercept := nil;
    end
    else if (AComponent = FIOHandler) then begin
      FIOHandler := nil;
      FImplicitIOHandler := False;
    end;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TIdCustomTCPServer.SetActive(AValue: Boolean);
begin
  // At design time we just set the value and save it for run time.
  // During loading we ignore it till all other properties are set.
  // Loaded will recall it to toggle it
  if IsDesignTime or IsLoading then begin
    FActive := AValue;
  end
  else if FActive <> AValue then begin
    if AValue then begin
      CheckOkToBeActive;
      try
        Startup;
      except
        FActive := True;
        SetActive(False); // allow descendants to clean up
        raise;
      end;
      FActive := True;
    end else begin
      // Must set to False here. Shutdown() implementations call property setters that check this
      FActive := False;
      Shutdown;
    end;
  end;
end;

procedure TIdCustomTCPServer.SetBindings(const AValue: TIdSocketHandles);
begin
  FBindings.Assign(AValue);
end;

procedure TIdCustomTCPServer.SetDefaultPort(const AValue: TIdPort);
begin
  FBindings.DefaultPort := AValue;
end;

procedure TIdCustomTCPServer.SetIntercept(const AValue: TIdServerIntercept);
begin
  {$IFDEF USE_OBJECT_ARC}
  // under ARC, all weak references to a freed object get nil'ed automatically
  FIntercept := AValue;
  {$ELSE}
  if FIntercept <> AValue then
  begin
    // Remove self from the intercept's notification list
    if Assigned(FIntercept) then begin
      FIntercept.RemoveFreeNotification(Self);
    end;
    FIntercept := AValue;
    // Add self to the intercept's notification list
    if Assigned(FIntercept) then begin
      FIntercept.FreeNotification(Self);
    end;
  end;
  {$ENDIF}
end;

procedure TIdCustomTCPServer.SetScheduler(const AValue: TIdScheduler);
var
  // under ARC, convert weak references to strong references before working with them
  LScheduler: TIdScheduler;
  LIOHandler: TIdServerIOHandler;
begin
  LScheduler := FScheduler;

  if LScheduler <> AValue then
  begin
    // RLebeau - is this really needed?  What should happen if this
    // gets called by Notification() if the Scheduler is freed while
    // the server is still Active?
    if Active then begin
      raise EIdException.Create(RSTCPServerSchedulerAlreadyActive);
    end;

    // under ARC, all weak references to a freed object get nil'ed automatically

    // If implicit one already exists free it
    // Free the default Thread manager
    if FImplicitScheduler then begin
      // Under D8 notification gets called after .Free of FreeAndNil, but before
      // its set to nil with a side effect of IDisposable. To counteract this we
      // set it to nil first.
      // -Kudzu
      FScheduler := nil;
      FImplicitScheduler := False;
      IdDisposeAndNil(LScheduler);
    end;

    {$IFNDEF USE_OBJECT_ARC}
    // Ensure we will no longer be notified when the component is freed
    if LScheduler <> nil then begin
      LScheduler.RemoveFreeNotification(Self);
    end;
    {$ENDIF}

    FScheduler := AValue;

    {$IFNDEF USE_OBJECT_ARC}
    // Ensure we will be notified when the component is freed, even is it's on
    // another form
    if AValue <> nil then begin
      AValue.FreeNotification(Self);
    end;
    {$ENDIF}

    LIOHandler := FIOHandler;
    if LIOHandler <> nil then begin
      LIOHandler.SetScheduler(AValue);
    end;
  end;
end;

procedure TIdCustomTCPServer.SetIOHandler(const AValue: TIdServerIOHandler);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdServerIOHandler;
begin
  LIOHandler := FIOHandler;

  if LIOHandler <> AValue then begin

    // RLebeau - is this needed?  SetScheduler() does it, so should SetIOHandler()
    // also do it? What should happen if this gets called by Notification() if the
    // IOHandler is freed while the server is still Active?
    {
    if Active then begin
      raise EIdException.Create(RSTCPServerIOHandlerAlreadyActive);
    end;
    }

    if FImplicitIOHandler then begin
      FIOHandler := nil;
      FImplicitIOHandler := False;
      IdDisposeAndNil(LIOHandler);
    end;

    {$IFNDEF USE_OBJECT_ARC}
    // Ensure we will no longer be notified when the component is freed
    if Assigned(LIOHandler) then begin
      LIOHandler.RemoveFreeNotification(Self);
    end;
    {$ENDIF}

    FIOHandler := AValue;

    if AValue <> nil then begin
      {$IFNDEF USE_OBJECT_ARC}
      // Ensure we will be notified when the component is freed, even is it's on
      // another form
      AValue.FreeNotification(Self);
      {$ENDIF}
      AValue.SetScheduler(FScheduler);
    end;
  end;
end;

procedure TIdCustomTCPServer.StartListening;
var
  LListenerThreads: TIdListenerList;
  LListenerThread: TIdListenerThread;
  I: Integer;
  LBinding: TIdSocketHandle;
begin
  LListenerThreads := FListenerThreads.LockList;
  try
    // Set up any sockets that are not already listening
    I := LListenerThreads.Count;
    try
      while I < Bindings.Count do begin
        LBinding := Bindings[I];
        LBinding.AllocateSocket;
        // do not overwrite if the default. This allows ReuseSocket to be set per binding
        if FReuseSocket <> rsOSDependent then begin
          LBinding.ReuseSocket := FReuseSocket;
        end;
        DoBeforeBind(LBinding);
        LBinding.Bind;
        LBinding.UseNagle := FUseNagle;
        Inc(I);
      end;
    except
      Dec(I); // the one that failed doesn't need to be closed
      while I >= 0 do begin
        Bindings[I].CloseSocket;
        Dec(I);
      end;
      raise;
    end;

    if I > LListenerThreads.Count then begin
      DoAfterBind;
    end;

    // Set up any threads that are not already running
    for I := LListenerThreads.Count to Bindings.Count - 1 do
    begin
      LBinding := Bindings[I];
      LBinding.Listen(FListenQueue);
      LListenerThread := TIdListenerThread.Create(Self, LBinding);
      try
        LListenerThread.Name := Name + ' Listener #' + IntToStr(I + 1); {do not localize}
        LListenerThread.OnBeforeRun := DoBeforeListenerRun;
        //Todo: Implement proper priority handling for Linux
        //http://www.midnightbeach.com/jon/pubs/2002/BorCon.London/Sidebar.3.html
        LListenerThread.Priority := tpListener;
        LListenerThreads.Add(LListenerThread);
      except
        LBinding.CloseSocket;
        FreeAndNil(LListenerThread);
        raise;
      end;
      LListenerThread.Start;
    end;
  finally
    FListenerThreads.UnlockList;
  end;
end;

//APR-011207: for safe-close Ex: SQL Server ShutDown 1) stop listen 2) wait until all clients go out
procedure TIdCustomTCPServer.StopListening;
var
  LListenerThreads: TIdListenerList;
  LListener: TIdListenerThread;
begin
  LListenerThreads := FListenerThreads.LockList;
  try
    while LListenerThreads.Count > 0 do begin
      LListener := {$IFDEF HAS_GENERICS_TThreadList}LListenerThreads[0]{$ELSE}TIdListenerThread(LListenerThreads[0]){$ENDIF};
      // Stop listening
      LListener.Terminate;
      LListener.Binding.CloseSocket;
      // Tear down Listener thread
      LListener.WaitFor;
      LListener.Free;
      LListenerThreads.Delete(0); // RLebeau 2/17/2006
    end;
  finally
    FListenerThreads.UnlockList;
  end;
end;

{$IFDEF STRING_IS_UNICODE}
//This is an ugly hack that's required because a ShortString does not seem
//to be acceptable to D2009's Assert function.
procedure AssertClassName(const ABool : Boolean; const AString : String); inline;
begin
  Assert(ABool, AString);
end;
{$ENDIF}

procedure TIdCustomTCPServer.TerminateAllThreads;
var
  i: Integer;
  LContext: TIdContext;
  LList: TIdContextList;

  // under ARC, convert a weak reference to a strong reference before working with it
  LScheduler: TIdScheduler;
begin
  // TODO:  reimplement support for TerminateWaitTimeout

  //BGO: find out why TerminateAllThreads is sometimes called multiple times
  //Kudzu: Its because of notifications. It calls shutdown when the Scheduler is
  // set to nil and then again on destroy.
  if Contexts <> nil then begin
    LList := Contexts.LockList;
    try
      for i := 0 to LList.Count - 1 do begin
        LContext := {$IFDEF HAS_GENERICS_TList}LList.Items[i]{$ELSE}TIdContext(LList.Items[i]){$ENDIF};
        Assert(LContext<>nil);
        {$IFDEF STRING_IS_UNICODE}
        AssertClassName(LContext.Connection<>nil, LContext.ClassName);
        {$ELSE}
        Assert(LContext.Connection<>nil, LContext.ClassName);
        {$ENDIF}
        // RLebeau: allow descendants to perform their own cleanups before
        // closing the connection.  FTP, for example, needs to abort an
        // active data transfer on a separate asociated connection
        DoTerminateContext(LContext);
      end;
    finally
      Contexts.UnLockList;
    end;
  end;

  // Scheduler may be nil during destroy which calls TerminateAllThreads
  // This happens with explicit schedulers

  LScheduler := FScheduler;
  if Assigned(LScheduler) then begin
    LScheduler.TerminateAllYarns;
  end;
end;

procedure TIdCustomTCPServer.DoBeforeListenerRun(AThread: TIdThread);
begin
  if Assigned(OnBeforeListenerRun) then begin
    OnBeforeListenerRun(AThread);
  end;
end;

procedure TIdCustomTCPServer.DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler);
begin
//
end;

procedure TIdCustomTCPServer.DoTerminateContext(AContext: TIdContext);
begin
  // Dont call disconnect with true. Otherwise it frees the IOHandler and the thread
  // is still running which often causes AVs and other.
  AContext.Connection.Disconnect(False);
end;

procedure TIdCustomTCPServer.InitComponent;
begin
  inherited InitComponent;
  FBindings := TIdSocketHandles.Create(Self);
  FContexts := TIdContextThreadList.Create;
  FContextClass := TIdServerContext;
  //
  FTerminateWaitTime := 5000;
  FListenQueue := IdListenQueueDefault;
  FListenerThreads := TIdListenerThreadList.Create;
  //TODO: When reestablished, use a sleeping thread instead
//  fSessionTimer := TTimer.Create(self);
  FUseNagle := true; // default
end;

procedure TIdCustomTCPServer.Shutdown;
var
  // under ARC, convert the weak reference to a strong reference before working with it
  LIOHandler: TIdServerIOHandler;
begin
  // tear down listening threads
  StopListening;

  // Tear down ThreadMgr
  try
    TerminateAllThreads;
  finally
    {//bgo TODO: fix this: and Threads.IsCountLessThan(1)}
    // DONE -oAPR: BUG! Threads still live, Mgr dead ;-(
    if ImplicitScheduler then begin
      SetScheduler(nil);
    end;
  end;

  LIOHandler := IOHandler;
  if LIOHandler <> nil then begin
    LIOHandler.Shutdown;
  end;
end;

// Linux/Unix does not allow an IPv4 socket and an IPv6 socket
// to listen on the same port at the same time! Windows does not
// have that problem...
{$DEFINE CanCreateTwoBindings}
{$IFDEF LINUX} // should this be UNIX instead?
  {$UNDEF CanCreateTwoBindings}
{$ENDIF}
{$IFDEF ANDROID}
  {$UNDEF CanCreateTwoBindings}
{$ENDIF}
// TODO: Would this be solved by enabling the SO_REUSEPORT option on
// platforms that support it?

procedure TIdCustomTCPServer.Startup;
var
  LScheduler: TIdScheduler;
  LIOHandler: TIdServerIOHandler;
  {$IFDEF CanCreateTwoBindings}
  LBinding: TIdSocketHandle;
  {$ENDIF}
begin
  // Set up bindings
  if Bindings.Count = 0 then begin
    // TODO: on systems that support dual-stack sockets, create a single
    // Binding object that supports both IPv4 and IPv6 on the same socket...

    {$IFDEF CanCreateTwoBindings}LBinding := {$ENDIF}Bindings.Add; // IPv4 or IPv6 by default

    {$IFDEF CanCreateTwoBindings}
    // TODO: maybe add a property so the developer can switch this behavior on/off
    case LBinding.IPVersion of
      Id_IPv4: begin
        if GStack.SupportsIPv6 then begin
          Bindings.Add.IPVersion := Id_IPv6;
        end;
      end;
      Id_IPv6: begin
        if GStack.SupportsIPv4 then begin
          Bindings.Add.IPVersion := Id_IPv4;
        end;
      end;
    end;
    {$ENDIF}
  end;

  // Setup IOHandler
  LIOHandler := FIOHandler;
  if not Assigned(LIOHandler) then begin
    LIOHandler := TIdServerIOHandlerStack.Create(Self);
    SetIOHandler(LIOHandler);
    FImplicitIOHandler := True;
  end;
  LIOHandler.Init;

  // Set up scheduler
  LScheduler := FScheduler;
  if not Assigned(FScheduler) then begin
    LScheduler := TIdSchedulerOfThreadDefault.Create(Self);
    SetScheduler(LScheduler);
    FImplicitScheduler := True;
    // Useful in debugging and for thread names
    LScheduler.Name := Name + 'Scheduler';   {do not localize}
  end;
  LScheduler.Init;

  StartListening;
end;

procedure TIdCustomTCPServer.CheckOkToBeActive;
begin
  //nothing here.  Override in a descendant for an end-point
end;

{ TIdListenerThread }

procedure TIdListenerThread.AfterRun;
begin
  inherited AfterRun;
  // Close just own binding. The rest will be closed from their coresponding
  // threads
  FBinding.CloseSocket;
end;

procedure TIdListenerThread.BeforeRun;
begin
  inherited BeforeRun;
  if Assigned(FOnBeforeRun) then begin
    FOnBeforeRun(Self);
  end;
end;

constructor TIdListenerThread.Create(AServer: TIdCustomTCPServer; ABinding: TIdSocketHandle);
begin
  inherited Create;
  FBinding := ABinding;
  FServer := AServer;
end;

type
  TIdServerContextAccess = class(TIdServerContext)
  end;

procedure TIdListenerThread.Run;
var
  LContext: TIdServerContext;
  LIOHandler: TIdIOHandler;
  LPeer: TIdTCPConnection;
  LYarn: TIdYarn;
begin
  Assert(Server<>nil);
  Assert(Server.IOHandler<>nil);

  LContext := nil;
  LPeer := nil;
  LYarn := nil;
  try
    // GetYarn can raise exceptions
    LYarn := Server.Scheduler.AcquireYarn;

    // TODO: under Windows at least, use SO_CONDITIONAL_ACCEPT to allow
    // the user to reject connections before they are accepted.  Somehow
    // expose an event here for the user to decide with...
    
    LIOHandler := Server.IOHandler.Accept(Binding, Self, LYarn);
    if LIOHandler = nil then begin
      // Listening has finished
      Stop;
      Abort;
    end else begin
      // We have accepted the connection and need to handle it
      LPeer := TIdTCPConnection.Create(nil);
      {$IFDEF USE_OBJECT_ARC}
      // under ARC, the TIdTCPConnection.IOHandler property is a weak reference.
      // TIdServerIOHandler.Accept() returns an IOHandler with no Owner assigned,
      // so lets make the TIdTCPConnection become the Owner in order to keep the
      // IOHandler alive when this method exits.
      //
      // TODO: should we assign Ownership unconditionally on all platforms?
      //
      LPeer.InsertComponent(LIOHandler);
      {$ENDIF}
      LPeer.IOHandler := LIOHandler;
      LPeer.ManagedIOHandler := True;
    end;

    // LastRcvTimeStamp := Now;  // Added for session timeout support
    // ProcessingTimeout := False;

    // Check MaxConnections
    if (Server.MaxConnections > 0) and (not Server.Contexts.IsCountLessThan(Server.MaxConnections)) then begin
      FServer.DoMaxConnectionsExceeded(LIOHandler);
      LPeer.Disconnect;
      Abort;
    end;

    // Create and init context
    LContext := Server.FContextClass.Create(LPeer, LYarn, Server.Contexts);
    LContext.FServer := Server;
    // We set these instead of having the context call them directly
    // because they are protected methods. Also its good to keep
    // Context indepent of the server as well.
    LContext.OnBeforeRun := Server.ContextConnected;
    LContext.OnRun := Server.DoExecute;
    LContext.OnAfterRun := Server.ContextDisconnected;
    LContext.OnException := Server.DoException;
    //
    Server.ContextCreated(LContext);
    //
    // If all ok, lets start the yarn
    Server.Scheduler.StartYarn(LYarn, LContext);
  except
    on E: Exception do begin
      // RLebeau 1/11/07: TIdContext owns the Peer by default so
      // take away ownership here so the Peer is not freed twice
      if LContext <> nil then begin
        TIdServerContextAccess(LContext).FOwnsConnection := False;
      end;
      FreeAndNil(LContext);
      FreeAndNil(LPeer);
      // Must terminate - likely has not started yet
      if LYarn <> nil then begin
        Server.Scheduler.TerminateYarn(LYarn);
      end;
      // EAbort is used to kick out above and destroy yarns and other, but
      // we dont want to show the user
      if not (E is EAbort) then begin
        Server.DoListenException(Self, E);
      end;
    end;
  end;
end;

end.


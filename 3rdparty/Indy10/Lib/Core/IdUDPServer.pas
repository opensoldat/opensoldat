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
  Rev 1.14    11/12/2004 3:44:00 PM  JPMugaas
  Compiler error fix.  OOPPS!!!

  Rev 1.13    11/12/2004 11:30:20 AM  JPMugaas
  Expansions for IPv6.

  Rev 1.12    6/11/2004 11:48:34 PM  JPMugaas
  Fix for mistake I made.  UDPReceive should have been UDPException

  Rev 1.11    6/11/2004 4:05:34 PM  JPMugaas
  RecvFrom should now work in the UDP server with IPv6.
  An OnException event was added for logging purposes.

  Rev 1.10    09/06/2004 00:25:32  CCostelloe
  Kylix 3 patch

  Rev 1.9    2004.02.03 4:17:02 PM  czhower
  For unit name changes.

  Rev 1.8    2004.01.20 10:03:40 PM  czhower
  InitComponent

  Rev 1.7    2003.12.31 8:03:36 PM  czhower
  Matched visibility

  Rev 1.6    10/26/2003 6:01:44 PM  BGooijen
  Fixed binding problem

  Rev 1.5    10/24/2003 5:18:38 PM  BGooijen
  Removed boolean shortcutting from .GetActive

  Rev 1.4    10/22/2003 04:41:02 PM  JPMugaas
  Should compile with some restored functionality.  Still not finished.

  Rev 1.3    2003.10.11 9:58:50 PM  czhower
  Started on some todos

  Rev 1.2    2003.10.11 5:52:18 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.1    2003.09.30 1:23:10 PM  czhower
  Stack split for DotNet

  Rev 1.0    11/13/2002 09:02:30 AM  JPMugaas
}

unit IdUDPServer;

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  Classes,
  {$IFDEF HAS_UNIT_Generics_Collections}
  System.Generics.Collections,
  {$ENDIF}
  IdComponent,
  IdException,
  IdGlobal,
  IdSocketHandle,
  IdStackConsts,
  IdThread,
  IdUDPBase,
  IdStack;

type
  TIdUDPServer = class;

  TIdUDPListenerThread = class(TIdThread)
  protected
    FBinding: TIdSocketHandle;
    FAcceptWait: Integer;
    FBuffer: TIdBytes;
    FCurrentException: String;
    FCurrentExceptionClass: TClass;
    {$IFDEF USE_OBJECT_ARC}
    // When AutoRefCounting is enabled, object references MUST be valid objects.
    // It is common for users to store non-object values, though, so we will
    // provide separate properties for those purpose
    //
    // TODO; use TValue instead of separating them
    //
    FDataObject: TObject;
    FDataValue: PtrInt;
    {$ELSE}
    FData: TObject;
    {$ENDIF}
    FServer: TIdUDPServer;
    //
    procedure AfterRun; override;
    procedure Run; override;
  public
    //
    //[Error] IdUDPServer.pas(266): E2391 Potentially polymorphic constructor calls must be virtual
    constructor Create(AOwner: TIdUDPServer; ABinding: TIdSocketHandle); reintroduce; virtual;
    destructor Destroy; override;
    //
    procedure UDPRead;
    procedure UDPException;
    //
    property AcceptWait: integer read FAcceptWait write FAcceptWait;
    property Binding: TIdSocketHandle read FBinding;
    property Server: TIdUDPServer read FServer;
    {$IFDEF USE_OBJECT_ARC}
    property DataObject: TObject read FDataObject write FDataObject;
    property DataValue: PtrInt read FDataValue write FDataValue;
    {$ELSE}
    property Data: TObject read FData write FData;
    {$ENDIF}
  end;

  // TODO: use TIdThreadSafeObjectList instead?
  {$IFDEF HAS_GENERICS_TThreadList}
  TIdUDPListenerThreadList = TThreadList<TIdUDPListenerThread>;
  TIdUDPListenerList = TList<TIdUDPListenerThread>;
  {$ELSE}
  // TODO: flesh out TThreadList<TIdUDPListenerThread> and TList<TIdUDPListenerThread> for non-Generics compilers...
  TIdUDPListenerThreadList = TThreadList;
  TIdUDPListenerList = TList;
  {$ENDIF}

  TIdUDPListenerThreadClass = class of TIdUDPListenerThread;
  
  //Exception is used instead of EIdException because the exception could be from somewhere else
  TIdUDPExceptionEvent = procedure(AThread: TIdUDPListenerThread; ABinding: TIdSocketHandle; const AMessage : String; const AExceptionClass : TClass) of object;

  TUDPReadEvent = procedure(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle) of object;

  TIdUDPServer = class(TIdUDPBase)
  protected
    FBindings: TIdSocketHandles;
    FCurrentBinding: TIdSocketHandle;
    FListenerThreads: TIdUDPListenerThreadList;
    FThreadClass: TIdUDPListenerThreadClass;
    FThreadedEvent: boolean;
    //
    FOnBeforeBind: TIdSocketHandleEvent;
    FOnAfterBind: TNotifyEvent;
    FOnUDPRead: TUDPReadEvent;
    FOnUDPException : TIdUDPExceptionEvent;
    //
    procedure BroadcastEnabledChanged; override;
    procedure CloseBinding; override;
    procedure DoBeforeBind(AHandle: TIdSocketHandle); virtual;
    procedure DoAfterBind; virtual;
    procedure DoOnUDPException(AThread: TIdUDPListenerThread; ABinding: TIdSocketHandle; const AMessage : String; const AExceptionClass : TClass);  virtual;
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); virtual;
    function GetActive: Boolean; override;
    function GetBinding: TIdSocketHandle; override;
    function GetDefaultPort: TIdPort;
    procedure InitComponent; override;
    procedure SetBindings(const Value: TIdSocketHandles);
    procedure SetDefaultPort(const AValue: TIdPort);
  public
    destructor Destroy; override;
    property ThreadClass: TIdUDPListenerThreadClass read FThreadClass write FThreadClass;
  published
    property Bindings: TIdSocketHandles read FBindings write SetBindings;
    property DefaultPort: TIdPort read GetDefaultPort write SetDefaultPort;
    property ReuseSocket;
    property ThreadedEvent: boolean read FThreadedEvent write FThreadedEvent default False;
    //
    property OnBeforeBind: TIdSocketHandleEvent read FOnBeforeBind write FOnBeforeBind;
    property OnAfterBind: TNotifyEvent read FOnAfterBind write FOnAfterBind;
    property OnUDPRead: TUDPReadEvent read FOnUDPRead write FOnUDPRead;
    property OnUDPException : TIdUDPExceptionEvent read FOnUDPException write FOnUDPException;
  end;

  EIdUDPServerException = class(EIdUDPException);

implementation

uses
  {$IFDEF VCL_2010_OR_ABOVE}
    {$IFDEF WINDOWS}
  Windows,
    {$ENDIF}
  {$ENDIF}
  IdGlobalCore, SysUtils;

procedure TIdUDPServer.BroadcastEnabledChanged;
var
  i: Integer;
begin
  if Assigned(FCurrentBinding) then begin
    for i := 0 to Bindings.Count - 1 do begin
      Bindings[i].BroadcastEnabled := BroadcastEnabled;
    end;
  end;
end;

procedure TIdUDPServer.CloseBinding;
var
  LListenerThreads: TIdUDPListenerList;
  LListener: TIdUDPListenerThread;
begin
  // RLebeau 2/17/2006: TIdUDPBase.Destroy() calls CloseBinding()
  if Assigned(FListenerThreads) then
  begin
    LListenerThreads := FListenerThreads.LockList;
    try
      while LListenerThreads.Count > 0 do
      begin
        LListener := {$IFDEF HAS_GENERICS_TThreadList}LListenerThreads[0]{$ELSE}TIdUDPListenerThread(LListenerThreads[0]){$ENDIF};
        // Stop listening
        LListener.Stop;
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
  FCurrentBinding := nil;
end;

destructor TIdUDPServer.Destroy;
begin
  Active := False;
  FreeAndNil(FBindings);
  FreeAndNil(FListenerThreads);
  inherited Destroy;
end;

procedure TIdUDPServer.DoBeforeBind(AHandle: TIdSocketHandle);
begin
  if Assigned(FOnBeforeBind) then begin
    FOnBeforeBind(AHandle);
  end;
end;

procedure TIdUDPServer.DoAfterBind;
begin
  if Assigned(FOnAfterBind) then begin
    FOnAfterBind(Self);
  end;
end;

procedure TIdUDPServer.DoOnUDPException(AThread: TIdUDPListenerThread; ABinding: TIdSocketHandle; const AMessage : String; const AExceptionClass : TClass);
begin
  if Assigned(FOnUDPException) then begin
    OnUDPException(AThread, ABinding, AMessage, AExceptionClass);
  end;
end;

procedure TIdUDPServer.DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
begin
  if Assigned(OnUDPRead) then begin
    OnUDPRead(AThread, AData, ABinding);
  end;
end;

function TIdUDPServer.GetActive: Boolean;
begin
  // inherited GetActive keeps track of design-time Active property
  Result := inherited GetActive;
  if not Result then begin
    if Assigned(FCurrentBinding) then begin
      Result := FCurrentBinding.HandleAllocated;
    end;
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

function TIdUDPServer.GetBinding: TIdSocketHandle;
var
  LListenerThread: TIdUDPListenerThread;
  i: Integer;
  LBinding: TIdSocketHandle;
begin
  if FCurrentBinding = nil then begin
    if Bindings.Count = 0 then begin
      // TODO: on systems that support dual-stack sockets, create a single
      // Binding object that supports both IPv4 and IPv6 on the same socket...

      LBinding := Bindings.Add;
      LBinding.IPVersion := IPVersion; // IPv4 or IPv6 by default

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

    // Set up listener threads
    i := 0;
    try
      while i < Bindings.Count do begin
        LBinding := Bindings[i];
{$IFDEF LINUX}
        LBinding.AllocateSocket(Integer(Id_SOCK_DGRAM));
{$ELSE}
        LBinding.AllocateSocket(Id_SOCK_DGRAM);
{$ENDIF}
        // do not overwrite if the default. This allows ReuseSocket to be set per binding
        if FReuseSocket <> rsOSDependent then begin
          LBinding.ReuseSocket := FReuseSocket;
        end;
        DoBeforeBind(LBinding);
        LBinding.Bind;
        Inc(i);
      end;
    except
      Dec(i); // the one that failed doesn't need to be closed
      while i >= 0 do begin
        Bindings[i].CloseSocket;
        Dec(i);
      end;
      raise;
    end;

    DoAfterBind;

    for i := 0 to Bindings.Count - 1 do begin
      LListenerThread := FThreadClass.Create(Self, Bindings[i]);
      LListenerThread.Name := Name + ' Listener #' + IntToStr(i + 1); {do not localize}
      {$IFDEF DELPHI_CROSS}
        {$IFNDEF MACOSX}
      //Todo: Implement proper priority handling for Linux
      //http://www.midnightbeach.com/jon/pubs/2002/BorCon.London/Sidebar.3.html
      LListenerThread.Priority := tpListener;
        {$ENDIF}
      {$ENDIF}
      FListenerThreads.Add(LListenerThread);
      LListenerThread.Start;
    end;
    FCurrentBinding := Bindings[0];
    BroadcastEnabledChanged;
  end;
  Result := FCurrentBinding;
end;

function TIdUDPServer.GetDefaultPort: TIdPort;
begin
  Result := FBindings.DefaultPort;
end;

procedure TIdUDPServer.InitComponent;
begin
  inherited InitComponent;
  FBindings := TIdSocketHandles.Create(Self);
  FListenerThreads := TIdUDPListenerThreadList.Create;
  FThreadClass := TIdUDPListenerThread;
end;

procedure TIdUDPServer.SetBindings(const Value: TIdSocketHandles);
begin
  FBindings.Assign(Value);
end;

procedure TIdUDPServer.SetDefaultPort(const AValue: TIdPort);
begin
  FBindings.DefaultPort := AValue;
end;

{ TIdUDPListenerThread }

procedure TIdUDPListenerThread.AfterRun;
begin
  inherited AfterRun;
  // Close just own binding. The rest will be closed from their
  // coresponding threads
  FBinding.CloseSocket;
end;

constructor TIdUDPListenerThread.Create(AOwner: TIdUDPServer; ABinding: TIdSocketHandle);
begin
  inherited Create(True);
  FAcceptWait := 1000;
  FBinding := ABinding;
  FServer := AOwner;
  SetLength(FBuffer, 0);
end;

destructor TIdUDPListenerThread.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TIdUDPListenerThread.Run;
var
  PeerIP: string;
  PeerPort : TIdPort;
  PeerIPVersion: TIdIPVersion;
  ByteCount: Integer;
begin
  if FBinding.Select(AcceptWait) then try
    // Doublecheck to see if we've been stopped
    // Depending on timing - may not reach here if it is in ancestor run when thread is stopped
    if not Stopped then begin
      SetLength(FBuffer, FServer.BufferSize);
      ByteCount := FBinding.RecvFrom(FBuffer, PeerIP, PeerPort, PeerIPVersion);
      // RLebeau: some protocols make use of 0-length messages, so don't discard
      // them here. This is not connection-oriented, so recvfrom() only returns
      // 0 if a 0-length packet was actually received...
      if ByteCount >= 0 then
      begin
        SetLength(FBuffer, ByteCount);
        FBinding.SetPeer(PeerIP, PeerPort, PeerIPVersion);
        // TODO: figure out a way to let UDPRead() run in this thread context
        // and only synchronize the OnUDPRead event handler so that descendants
        // do not need to be synchronized unnecessarily. Probably just have
        // TIdUDPServer.DoUDPRead() use TIdSync when ThreadedEvent is false...
        if FServer.ThreadedEvent then begin
          UDPRead;
        end else begin
          Synchronize(UDPRead);
        end;
      end;
    end;
  except
    // exceptions should be ignored so that other clients can be served in case of a DOS attack
    on E : Exception do
    begin
      FCurrentException := E.Message;
      FCurrentExceptionClass := E.ClassType;
      if FServer.ThreadedEvent then begin
        UDPException;
      end else begin
        Synchronize(UDPException);
      end;
    end;
  end;
end;

procedure TIdUDPListenerThread.UDPRead;
begin
  FServer.DoUDPRead(Self, FBuffer, FBinding);
end;

procedure TIdUDPListenerThread.UDPException;
begin
  FServer.DoOnUDPException(Self, FBinding, FCurrentException, FCurrentExceptionClass);
end;

end.

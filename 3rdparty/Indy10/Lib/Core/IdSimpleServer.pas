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
   Rev 1.17    7/13/04 6:46:36 PM  RLebeau
 Added support for BoundPortMin/Max propeties
}
{
   Rev 1.16    6/6/2004 12:49:40 PM  JPMugaas
 Removed old todo's for things that have already been done.
}
{
   Rev 1.15    5/6/2004 6:04:44 PM  JPMugaas
 Attempt to reenable TransparentProxy.Bind.
}
{
   Rev 1.14    5/5/2004 2:08:40 PM  JPMugaas
 Reenabled Socks Listen for TIdSimpleServer.
}
{
   Rev 1.13    2004.02.03 4:16:52 PM  czhower
 For unit name changes.
}
{
   Rev 1.12    2004.01.20 10:03:34 PM  czhower
 InitComponent
}
{
   Rev 1.11    1/2/2004 12:02:16 AM  BGooijen
 added OnBeforeBind/OnAfterBind
}
{
   Rev 1.10    1/1/2004 10:57:58 PM  BGooijen
 Added IPv6 support
}
{
   Rev 1.9    10/26/2003 10:08:44 PM  BGooijen
 Compiles in DotNet
}
{
   Rev 1.8    10/20/2003 03:04:56 PM  JPMugaas
 Should now work without Transparant Proxy.  That still needs to be enabled.
}
{
   Rev 1.7    2003.10.14 9:57:42 PM  czhower
 Compile todos
}
{
   Rev 1.6    2003.10.11 5:50:12 PM  czhower
 -VCL fixes for servers
 -Chain suport for servers (Super core)
 -Scheduler upgrades
 -Full yarn support
}
{
   Rev 1.5    2003.09.30 1:23:02 PM  czhower
 Stack split for DotNet
}
{
    Rev 1.4    5/16/2003 9:25:36 AM  BGooijen
  TransparentProxy support
}
{
    Rev 1.3    3/29/2003 5:55:04 PM  BGooijen
  now calls AfterAccept
}
{
    Rev 1.2    3/23/2003 11:24:46 PM  BGooijen
  changed cast from TIdIOHandlerStack to TIdIOHandlerSocket
}
{
   Rev 1.1    1-6-2003 21:39:00  BGooijen
 The handle to the listening socket was not closed when accepting a
 connection. This is fixed by merging the responsible code from 9.00.11

  Rev 1.0    11/13/2002 08:58:40 AM  JPMugaas
}
unit IdSimpleServer;

interface

{$i IdCompilerDefines.inc}

uses
  Classes,
  IdException,
  IdGlobal,
  IdSocketHandle,
  IdTCPConnection,
  IdStackConsts,
  IdIOHandler;

const
  ID_ACCEPT_WAIT = 1000;

type
  TIdSimpleServer = class(TIdTCPConnection)
  protected
    FAbortedRequested: Boolean;
    FAcceptWait: Integer;
    FBoundIP: String;
    FBoundPort: TIdPort;
    FBoundPortMin: TIdPort;
    FBoundPortMax: TIdPort;
    FIPVersion: TIdIPVersion;
    FListenHandle: TIdStackSocketHandle;
    FListening: Boolean;
    FOnBeforeBind: TNotifyEvent;
    FOnAfterBind: TNotifyEvent;
    //
    procedure Bind;
    procedure DoBeforeBind; virtual;
    procedure DoAfterBind; virtual;
    function GetBinding: TIdSocketHandle;
    procedure InitComponent; override;
    procedure SetIOHandler(AValue: TIdIOHandler); override;
    procedure SetIPVersion(const AValue: TIdIPVersion);
  public
    procedure Abort; virtual;
    procedure BeginListen; virtual;
    procedure CreateBinding;
    procedure EndListen; virtual;
    procedure Listen(ATimeout: Integer = IdTimeoutDefault); virtual;
    //
    property AcceptWait: Integer read FAcceptWait write FAcceptWait default ID_ACCEPT_WAIT;
  published
    property BoundIP: string read FBoundIP write FBoundIP;
    property BoundPort: TIdPort read FBoundPort write FBoundPort;
    property BoundPortMin: TIdPort read FBoundPortMin write FBoundPortMin;
    property BoundPortMax: TIdPort read FBoundPortMax write FBoundPortMax;
    property Binding: TIdSocketHandle read GetBinding;
    property IPVersion: TIdIPVersion read FIPVersion write SetIPVersion;

    property OnBeforeBind: TNotifyEvent read FOnBeforeBind write FOnBeforeBind;
    property OnAfterBind: TNotifyEvent read FOnAfterBind write FOnAfterBind;
  end;

  EIdCannotUseNonSocketIOHandler = class(EIdException);

implementation

uses
  IdExceptionCore,
  IdIOHandlerStack,
  IdIOHandlerSocket,
  IdCustomTransparentProxy,
  IdResourceStringsCore,
  IdStack;

{ TIdSimpleServer }

procedure TIdSimpleServer.Abort;
begin
  FAbortedRequested := True;
end;

procedure TIdSimpleServer.BeginListen;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LProxy: TIdCustomTransparentProxy;
begin
  // Must be before IOHandler as it resets it
  EndListen;
  CreateBinding;
  LProxy := Socket.TransparentProxy;
  if LProxy.Enabled then begin
    Socket.Binding.IP := BoundIP;
    LProxy.Bind(FIOHandler, BoundPort);
  end else begin
    Bind;
    Binding.Listen(1);
  end;
  FListening := True;
end;

procedure TIdSimpleServer.Bind;
var
  LBinding: TIdSocketHandle;
begin
  LBinding := Binding;
  try
    DoBeforeBind;
    LBinding.IPVersion := FIPVersion;  // needs to be before AllocateSocket, because AllocateSocket uses this
    LBinding.AllocateSocket;
    FListenHandle := LBinding.Handle;
    LBinding.IP := BoundIP;
    LBinding.Port := BoundPort;
    LBinding.ClientPortMin := BoundPortMin;
    LBinding.ClientPortMax := BoundPortMax;
    LBinding.Bind;
    DoAfterBind;
  except
    FListenHandle := Id_INVALID_SOCKET;
    raise;
  end;
end;

procedure TIdSimpleServer.CreateBinding;
begin
  if not Assigned(IOHandler) then begin
    CreateIOHandler();
  end;
  IOHandler.Open;
end;

procedure TIdSimpleServer.DoBeforeBind;
begin
  if Assigned(FOnBeforeBind) then begin
    FOnBeforeBind(self);
  end;
end;

procedure TIdSimpleServer.DoAfterBind;
begin
  if Assigned(FOnAfterBind) then begin
    FOnAfterBind(self);
  end;
end;

procedure TIdSimpleServer.EndListen;
begin
  FAbortedRequested := False;
  FListening := False;
end;

function TIdSimpleServer.GetBinding: TIdSocketHandle;
begin
  if Assigned(Socket) then begin
    Result := Socket.Binding;
  end else begin
    Result := nil;
  end;
end;

procedure TIdSimpleServer.SetIOHandler(AValue: TIdIOHandler);
begin
  if Assigned(AValue) then begin
    if not (AValue is TIdIOHandlerSocket) then begin
      raise EIdCannotUseNonSocketIOHandler.Create(RSCannotUseNonSocketIOHandler);
    end;
  end;
  inherited SetIOHandler(AValue);
end;

procedure TIdSimpleServer.SetIPVersion(const AValue: TIdIPVersion);
begin
  FIPVersion := AValue;
  if Assigned(Socket) then begin
    Socket.IPVersion := AValue;
  end;
end;

procedure TIdSimpleServer.Listen(ATimeout: Integer = IdTimeoutDefault);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LProxy: TIdCustomTransparentProxy;
  LAccepted: Boolean;

  function DoListenTimeout(ALTimeout: Integer; AUseProxy: Boolean): Boolean;
  var
    LSleepTime: Integer;
  begin
    LSleepTime := AcceptWait;

    if ALTimeout = IdTimeoutDefault then begin
      ALTimeout := IdTimeoutInfinite;
    end;

    if ALTimeout = IdTimeoutInfinite then begin
      repeat
        if AUseProxy then begin
          Result := LProxy.Listen(IOHandler, LSleepTime);
        end else begin
          Result := Binding.Select(LSleepTime);
        end;
      until Result or FAbortedRequested;
      Exit;
    end;

    while ALTimeout > LSleepTime do begin
      if AUseProxy then begin
        Result := LProxy.Listen(IOHandler, LSleepTime);
      end else begin
        Result := Binding.Select(LSleepTime);
      end;

      if Result or FAbortedRequested then begin
        Exit;
      end;

      Dec(ALTimeout, LSleepTime);
    end;

    if AUseProxy then begin
      Result := LProxy.Listen(IOHandler, ALTimeout);
    end else begin
      Result := Binding.Select(ALTimeout);
    end;
  end;

begin
  if not FListening then begin
    BeginListen;
  end;

  LProxy := Socket.TransparentProxy;
  if LProxy.Enabled then begin
    LAccepted := DoListenTimeout(ATimeout, True);
  end else
  begin
    LAccepted := DoListenTimeout(ATimeout, False);
    if LAccepted then begin
      if Binding.Accept(Binding.Handle) then begin
        IOHandler.AfterAccept;
      end;
    end;

// This is now protected. Disconnect replaces it - but it also calls shutdown.
// Im not sure we want to call shutdown here? Need to investigate before fixing
// this.
    GStack.Disconnect(FListenHandle);
    FListenHandle := Id_INVALID_SOCKET;
  end;
  
  if not LAccepted then begin
    raise EIdAcceptTimeout.Create(RSAcceptTimeout);
  end;
end;

procedure TIdSimpleServer.InitComponent;
begin
  inherited InitComponent;
  FAcceptWait := ID_ACCEPT_WAIT;
  FListenHandle := Id_INVALID_SOCKET;
end;

end.

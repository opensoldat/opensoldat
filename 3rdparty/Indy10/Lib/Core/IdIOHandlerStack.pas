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
  Rev 1.53    3/10/05 3:23:16 PM  RLebeau
  Updated WriteDirect() to access the Intercept property directly.

  Rev 1.52    11/12/2004 11:30:16 AM  JPMugaas
  Expansions for IPv6.

  Rev 1.51    11/11/04 12:03:46 PM  RLebeau
  Updated DoConnectTimeout() to recognize IdTimeoutDefault

  Rev 1.50    6/18/04 1:06:58 PM  RLebeau
  Bug fix for ReadTimeout property

  Rev 1.49    5/4/2004 9:57:34 AM  JPMugaas
  Removed some old uncommented code and reenabled some TransparentProxy code
  since it compile in DotNET.

  Rev 1.48    2004.04.18 12:52:02 AM  czhower
  Big bug fix with server disconnect and several other bug fixed that I found
  along the way.

  Rev 1.47    2004.04.08 3:56:34 PM  czhower
  Fixed bug with Intercept byte count. Also removed Bytes from Buffer.

  Rev 1.46    2004.03.12 8:01:00 PM  czhower
  Exception update

  Rev 1.45    2004.03.07 11:48:42 AM  czhower
  Flushbuffer fix + other minor ones found

  Rev 1.44    2004.03.01 5:12:32 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.43    2/21/04 9:25:50 PM  RLebeau
  Fix for BBG #66

  Added FLastSocketError member to TIdConnectThread

  Rev 1.42    2004.02.03 4:16:48 PM  czhower
  For unit name changes.

  Rev 1.41    12/31/2003 9:51:56 PM  BGooijen
  Added IPv6 support

  Rev 1.40    2003.12.28 1:05:58 PM  czhower
  .Net changes.

  Rev 1.39    11/21/2003 12:05:18 AM  BGooijen
  Terminated isn't public in TThread any more, made it public here now

  Rev 1.38    10/28/2003 9:15:44 PM  BGooijen
  .net

  Rev 1.37    10/18/2003 1:42:46 PM  BGooijen
  Added include

  Rev 1.36    2003.10.14 1:26:56 PM  czhower
  Uupdates + Intercept support

  Rev 1.35    2003.10.11 5:48:36 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.34    10/9/2003 8:09:10 PM  SPerry
  bug fixes

  Rev 1.33    10/5/2003 11:02:36 PM  BGooijen
  Write buffering

  Rev 1.32    05/10/2003 23:01:02  HHariri
  Fix for connect problem when IP address specified as opposed to host

  Rev 1.31    2003.10.02 8:23:42 PM  czhower
  DotNet Excludes

  Rev 1.30    2003.10.02 10:16:28 AM  czhower
  .Net

  Rev 1.29    2003.10.01 9:11:18 PM  czhower
  .Net

  Rev 1.28    2003.10.01 5:05:14 PM  czhower
  .Net

  Rev 1.27    2003.10.01 2:46:38 PM  czhower
  .Net

  Rev 1.26    2003.10.01 2:30:38 PM  czhower
  .Net

  Rev 1.22    10/1/2003 12:14:14 AM  BGooijen
  DotNet: removing CheckForSocketError

  Rev 1.21    2003.10.01 1:37:34 AM  czhower
  .Net

  Rev 1.19    2003.09.30 1:22:58 PM  czhower
  Stack split for DotNet

  Rev 1.18    2003.07.14 1:57:22 PM  czhower
  -First set of IOCP fixes.
  -Fixed a threadsafe problem with the stack class.

  Rev 1.17    2003.07.14 12:54:32 AM  czhower
  Fixed graceful close detection if it occurs after connect.

  Rev 1.16    2003.07.10 4:34:58 PM  czhower
  Fixed AV, added some new comments

  Rev 1.15    7/4/2003 08:26:46 AM  JPMugaas
  Optimizations.

  Rev 1.14    7/1/2003 03:39:48 PM  JPMugaas
  Started numeric IP function API calls for more efficiency.

    Rev 1.13    6/30/2003 10:25:18 AM  BGooijen
  removed unnecessary assignment to FRecvBuffer.Size

    Rev 1.12    6/29/2003 10:56:28 PM  BGooijen
  Removed .Memory from the buffer, and added some extra methods

  Rev 1.11    2003.06.25 4:28:32 PM  czhower
  Formatting and fixed a short circuit clause.

    Rev 1.10    6/3/2003 11:43:52 PM  BGooijen
  Elimintated some code

    Rev 1.9    4/16/2003 3:31:26 PM  BGooijen
  Removed InternalCheckForDisconnect, added .Connected

    Rev 1.8    4/14/2003 11:44:20 AM  BGooijen
  CheckForDisconnect calls ReadFromSource now

    Rev 1.7    4/2/2003 3:24:56 PM  BGooijen
  Moved transparantproxy from ..stack to ..socket

    Rev 1.6    3/5/2003 11:04:32 PM  BGooijen
  Fixed Intercept, but the part in WriteBuffer doesn't look really nice yet

    Rev 1.5    3/3/2003 11:31:58 PM  BGooijen
  fixed stack overflow in .CheckForDisconnect

    Rev 1.4    2/26/2003 1:15:40 PM  BGooijen
  FBinding is now freed in IdIOHandlerSocket, instead of in IdIOHandlerStack

  Rev 1.3    2003.02.25 1:36:12 AM  czhower

  Rev 1.2    2002.12.06 11:49:34 PM  czhower

  Rev 1.1    12-6-2002 20:10:18  BGooijen
  Added IPv6-support

  Rev 1.0    11/13/2002 08:45:16 AM  JPMugaas
}

unit IdIOHandlerStack;

interface

{$i IdCompilerDefines.inc}

uses
  Classes,
  IdGlobal, IdSocketHandle, IdIOHandlerSocket, IdExceptionCore, IdStack,
  SysUtils;

type
  TIdIOHandlerStack = class(TIdIOHandlerSocket)
  protected
    procedure ConnectClient; override;
    function ReadDataFromSource(var VBuffer: TIdBytes): Integer; override;
    function WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer; override;
  public
    procedure CheckForDisconnect(ARaiseExceptionIfDisconnected: Boolean = True;
      AIgnoreBuffer: Boolean = False); override;
    function Connected: Boolean; override;
    function Readable(AMSec: Integer = IdTimeoutDefault): Boolean; override;
  published
    property ReadTimeout;
  end;

implementation

uses
  {$IFDEF USE_VCL_POSIX}
  Posix.SysSelect,
  Posix.SysTime,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  IdAntiFreezeBase, IdResourceStringsCore, IdResourceStrings, IdStackConsts, IdException,
  IdTCPConnection, IdComponent, IdIOHandler, IdCustomTransparentProxy;

type
  TIdConnectThread = class(TThread)
  protected
    FBinding: TIdSocketHandle;
    {$IFDEF HAS_AcquireExceptionObject}
    FConnectException: TObject;
    {$ELSE}
    FLastSocketError: Integer;
    FExceptionMessage: string;
    {$ENDIF}
    FExceptionOccured: Boolean;
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(ABinding: TIdSocketHandle); reintroduce;
    {$IFDEF HAS_AcquireExceptionObject}
    destructor Destroy; override;
    {$ENDIF}
    procedure CheckForConnectError;
    property Terminated;
  end;

{ TIdIOHandlerStack }

function TIdIOHandlerStack.Connected: Boolean;
begin
  ReadFromSource(False, 0, False);
  Result := inherited Connected;
end;

procedure TIdIOHandlerStack.ConnectClient;

  procedure DoConnectTimeout(ATimeout: Integer);
  var
    LSleepTime, LWaitTime: Integer;
    LThread: TIdConnectThread;
  begin
    // IndySleep
    if TIdAntiFreezeBase.ShouldUse then begin
      LSleepTime := IndyMin(GAntiFreeze.IdleTimeOut, 125);
    end else begin
      LSleepTime := 125;
    end;

    LThread := TIdConnectThread.Create(Binding);
    try
      if TIdAntiFreezeBase.ShouldUse then begin
        // TODO: we need to take the actual clock into account, not just
        // decrement by the sleep interval.  If IndySleep() runs longer then
        // requested, that would slow down the loop and exceed the original
        // timeout that was requested...
        {
        Start := Ticks64;
        repeat
          while (GetElapsedTicks(Start) < ATimeout) and (not LThread.Terminated) do begin
            LWaitTime := IndyMin(ATimeout - GetElapsedTicks(Start), LSleepTime);
            if LWaitTime <= 0 then Break;
            ($IFDEF WINDOWS)
            if WaitForSingleObject(LThread.Handle, LWaitTime) <> WAIT_TIMEOUT then begin
              Break;
            end;
            ($ELSE)
            // TODO: figure out what else can be used here...
            IndySleep(LWaitTime);
            ($ENDIF)
            TIdAntiFreezeBase.DoProcess;
          end;
        end;
        }
        while (ATimeout > 0) and (not LThread.Terminated) do begin
          LWaitTime := IndyMin(ATimeout, LSleepTime);
          {$IFDEF WINDOWS}
          if WaitForSingleObject(LThread.Handle, LWaitTime) <> WAIT_TIMEOUT then begin
            Break;
          end;
          {$ELSE}
          // TODO: figure out what else can be used here...
          IndySleep(LWaitTime);
          {$ENDIF}
          TIdAntiFreezeBase.DoProcess;
          Dec(ATimeout, LWaitTime);
        end;
      end else begin
        {$IFDEF WINDOWS}
        WaitForSingleObject(LThread.Handle, ATimeout);
        {$ELSE}
        // TODO: figure out what else can be used here...
        while (ATimeout > 0) and (not LThread.Terminated) do begin
          LWaitTime := IndyMin(ATimeout, LSleepTime);
          IndySleep(LWaitTime);
          Dec(ATimeout, LWaitTime);
        end;
        {$ENDIF}
      end;

      if LThread.Terminated then begin
        LThread.CheckForConnectError;
      end else begin
        LThread.Terminate;
        // TODO: before closing, maybe enable SO_DONTLINGER, or SO_LINGER with a 0 timeout...

        //Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_DONTLINGER, 1);

        {
        var l: linger;
        l.l_onoff := 1;
        l.l_linger := 0;
        Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_LINGER, Integer(@l));
        }

        Close;
        LThread.WaitFor;
        raise EIdConnectTimeout.Create(RSConnectTimeout);
      end;
    finally
      LThread.Free;
    end;
  end;

var
  LHost: String;
  LPort: Integer;
  LIP: string;
  LIPVersion : TIdIPVersion;
  // under ARC, convert a weak reference to a strong reference before working with it
  LProxy: TIdCustomTransparentProxy;
  LTimeout: Integer;
begin
  inherited ConnectClient;
  LProxy := FTransparentProxy;
  if Assigned(LProxy) then begin
    if LProxy.Enabled then begin
      LHost := LProxy.Host;
      LPort := LProxy.Port;
      LIPVersion := LProxy.IPVersion;
    end else begin
      LHost := Host;
      LPort := Port;
      LIPVersion := IPVersion;
    end;
  end else begin
    LHost := Host;
    LPort := Port;
    LIPVersion := IPVersion;
  end;
  if LIPVersion = Id_IPv4 then
  begin
    if not GStack.IsIP(LHost) then begin
      if Assigned(OnStatus) then begin
        DoStatus(hsResolving, [LHost]);
      end;
      LIP := GStack.ResolveHost(LHost, LIPVersion);
    end else begin
      LIP := LHost;
    end;
  end
  else
  begin  //IPv6
    LIP := MakeCanonicalIPv6Address(LHost);
    if LIP='' then begin  //if MakeCanonicalIPv6Address failed, we have a hostname
      if Assigned(OnStatus) then begin
        DoStatus(hsResolving, [LHost]);
      end;
      LIP := GStack.ResolveHost(LHost, LIPVersion);
    end else begin
      LIP := LHost;
    end;
  end;
  Binding.SetPeer(LIP, LPort, LIPVersion);
  // Connect
  //note for status events, we check specifically for them here
  //so we don't do a string conversion in Binding.PeerIP.
  if Assigned(OnStatus) then begin
    DoStatus(hsConnecting, [Binding.PeerIP]);
  end;

  LTimeout := ConnectTimeout;
  if (LTimeout = IdTimeoutDefault) or (LTimeout = 0) then begin
    LTimeout := IdTimeoutInfinite;
  end;
  if LTimeout = IdTimeoutInfinite then begin
    if TIdAntiFreezeBase.ShouldUse then begin
      DoConnectTimeout(120000); // 2 Min
    end else begin
      Binding.Connect;
    end;
  end else begin
    DoConnectTimeout(LTimeout);
  end;

  if Assigned(LProxy) then begin
    if LProxy.Enabled then begin
      LProxy.Connect(Self, Host, Port, IPVersion);
    end;
  end;
end;

function TIdIOHandlerStack.Readable(AMSec: integer): boolean;
begin
  Result := Binding.Readable(AMSec);
end;

function TIdIOHandlerStack.WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer;
begin
  Assert(Binding<>nil);
  Result := Binding.Send(ABuffer, AOffset, ALength);
end;

// Reads any data in tcp/ip buffer and puts it into Indy buffer
// This must be the ONLY raw read from Winsock routine
// This must be the ONLY call to RECV - all data goes thru this method
function TIdIOHandlerStack.ReadDataFromSource(var VBuffer: TIdBytes): Integer;
begin
  Assert(Binding<>nil);
  Result := Binding.Receive(VBuffer);
end;

procedure TIdIOHandlerStack.CheckForDisconnect(
  ARaiseExceptionIfDisconnected: Boolean; AIgnoreBuffer: Boolean);
var
  LDisconnected: Boolean;
begin
  // ClosedGracefully // Server disconnected
  // IOHandler = nil // Client disconnected
  if ClosedGracefully then begin
    if BindingAllocated then begin
      Close;
      // Call event handlers to inform the user that we were disconnected
      DoStatus(hsDisconnected);
      //DoOnDisconnected;
    end;
    LDisconnected := True;
  end else begin
    LDisconnected := not BindingAllocated;
  end;
  // Do not raise unless all data has been read by the user
  if LDisconnected then begin
    if (InputBufferIsEmpty or AIgnoreBuffer) and ARaiseExceptionIfDisconnected then begin
      RaiseConnClosedGracefully;
    end;
  end;
end;

{ TIdConnectThread }

constructor TIdConnectThread.Create(ABinding: TIdSocketHandle);
begin
  FBinding := ABinding;
  inherited Create(False);
end;

{$IFDEF HAS_AcquireExceptionObject}
destructor TIdConnectThread.Destroy;
begin
  FConnectException.Free;
  inherited;
end;
{$ENDIF}

procedure TIdConnectThread.Execute;
begin
  try
    FBinding.Connect;
  except
    {$IFDEF HAS_AcquireExceptionObject}
    // TThread has a FatalException property, but we can't take ownership of it
    // so we can re-raise it, so using AcquireExceptionObject() instead to take
    // ownership of the exception before it can be assigned to FatalException...
    FExceptionOccured := True;
    FConnectException := AcquireExceptionObject;
    {$ELSE}
    on E: Exception do begin
      FExceptionOccured := True;
      FExceptionMessage := E.Message;
      if E is EIdSocketError then begin
        if (EIdSocketError(E).LastError <> Id_WSAEBADF) and (EIdSocketError(E).LastError <> Id_WSAENOTSOCK) then begin
          FLastSocketError := EIdSocketError(E).LastError;
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

procedure TIdConnectThread.DoTerminate;
begin
  // Necessary as caller checks this
  Terminate;
  inherited;
end;

procedure TIdConnectThread.CheckForConnectError;
var
  LException: TObject;
begin
  if FExceptionOccured then begin
    {$IFDEF HAS_AcquireExceptionObject}
    LException := FConnectException;
    FConnectException := nil;
    if LException = nil then begin
      LException := EIdConnectException.Create(''); // TODO
    end;
    {$ELSE}
    if FLastSocketError <> 0 then begin
      LException := EIdSocketError.CreateError(FLastSocketError, FExceptionMessage);
    end else begin
      LException := EIdConnectException.Create(FExceptionMessage);
    end;
    {$ENDIF}
    raise LException;
  end;
end;

initialization
  TIdIOHandlerStack.SetDefaultClass;
end.

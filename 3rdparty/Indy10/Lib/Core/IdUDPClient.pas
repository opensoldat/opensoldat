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
  Rev 1.11    11/12/2004 11:30:20 AM  JPMugaas
  Expansions for IPv6.

  Rev 1.10    11/11/2004 10:25:26 PM  JPMugaas
  Added OpenProxy and CloseProxy so you can do RecvFrom and SendTo functions
  from the UDP client with SOCKS.  You must call OpenProxy  before using
  RecvFrom or SendTo.  When you are finished, you must use CloseProxy to close
  any connection to the Proxy.  Connect and disconnect also call OpenProxy and
  CloseProxy.

  Rev 1.9    11/10/2004 9:40:42 PM  JPMugaas
  Timeout error fix.  Thanks Bas.

  Rev 1.8    11/9/2004 8:18:00 PM  JPMugaas
  Attempt to add SOCKS support in UDP.

  Rev 1.7    11/8/2004 5:03:00 PM  JPMugaas
  Eliminated Socket property because we probably do not need it after all.
  Binding should work just as well.  I also made some minor refinements to
  Disconnect and Connect.

  Rev 1.6    11/7/2004 11:50:36 PM  JPMugaas
  Fixed a Send method I broke.   If FSocket is not assigned, it will call the
  inherited SendBuffer method.  That should prevent code breakage.  The connect
  method should be OPTIONAL because UDP may be used for simple one-packet
  query/response protocols.

  Rev 1.5    11/7/2004 11:33:30 PM  JPMugaas
  Now uses Connect, Disconnect, Send, and Receive similarly to the TCP Clients.
  This should prevent unneeded DNS name to IP address conversions that SendTo
  was doing.

  Rev 1.4    2004.02.03 4:17:02 PM  czhower
  For unit name changes.

  Rev 1.3    2004.01.21 2:35:40 PM  czhower
  Removed illegal characters from file.

  Rev 1.2    21.1.2004 ã. 12:31:02  DBondzhev
  Fix for Indy source. Workaround for dccil bug
  now it can be compiled using Compile instead of build

  Rev 1.1    10/22/2003 04:41:00 PM  JPMugaas
  Should compile with some restored functionality.  Still not finished.

  Rev 1.0    11/13/2002 09:02:16 AM  JPMugaas
}

unit IdUDPClient;

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  Classes,
  IdUDPBase,
  IdGlobal,
  IdSocketHandle,
  IdCustomTransparentProxy;

(*$HPPEMIT '#if defined(_VCL_ALIAS_RECORDS)' *)
(*$HPPEMIT '#if !defined(UNICODE)' *)
(*$HPPEMIT '#pragma alias "@Idudpclient@TIdUDPClient@SetPortA$qqrxus"="@Idudpclient@TIdUDPClient@SetPort$qqrxus"' *)
(*$HPPEMIT '#else' *)
(*$HPPEMIT '#pragma alias "@Idudpclient@TIdUDPClient@SetPortW$qqrxus"="@Idudpclient@TIdUDPClient@SetPort$qqrxus"' *)
(*$HPPEMIT '#endif' *)
(*$HPPEMIT '#endif' *)

type
  EIdMustUseOpenProxy = class(EIdUDPException);

  TIdUDPClient = class(TIdUDPBase)
  protected
    FBoundIP: string;
    FBoundPort: TIdPort;
    FBoundPortMin: TIdPort;
    FBoundPortMax: TIdPort;
    FProxyOpened : Boolean;
    FOnConnected : TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FConnected : Boolean;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FTransparentProxy: TIdCustomTransparentProxy;
    FImplicitTransparentProxy: Boolean;
    function UseProxy : Boolean;
    procedure RaiseUseProxyError;
    procedure DoOnConnected; virtual;
    procedure DoOnDisconnected; virtual;
    procedure InitComponent; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //property methods
    procedure SetIPVersion(const AValue: TIdIPVersion); override;
    procedure SetHost(const AValue : String); override;
    procedure SetPort(const AValue : TIdPort); override;
    procedure SetTransparentProxy(AProxy : TIdCustomTransparentProxy);
    function GetBinding: TIdSocketHandle; override;
    function GetTransparentProxy: TIdCustomTransparentProxy;
  public
    destructor Destroy; override;
    procedure OpenProxy;
    procedure CloseProxy;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    function Connected: Boolean;
    function ReceiveBuffer(var ABuffer : TIdBytes;
     const AMSec: Integer = IdTimeoutDefault): Integer; overload;  override;
    function ReceiveBuffer(var ABuffer : TIdBytes;
      var VPeerIP: string; var VPeerPort: TIdPort;
      AMSec: Integer = IdTimeoutDefault): integer; overload; override;
    function ReceiveBuffer(var ABuffer : TIdBytes;
      var VPeerIP: string; var VPeerPort: TIdPort; var VIPVersion: TIdIPVersion;
      const AMSec: Integer = IdTimeoutDefault): integer; overload; override;
    procedure Send(const AData: string; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure SendBuffer(const AHost: string; const APort: TIdPort; const ABuffer : TIdBytes); overload; override;
    procedure SendBuffer(const ABuffer: TIdBytes); reintroduce; overload;
    procedure SendBuffer(const AHost: string; const APort: TIdPort;
      const AIPVersion: TIdIPVersion; const ABuffer: TIdBytes);overload; override;
  published
    property BoundIP: string read FBoundIP write FBoundIP;
    property BoundPort: TIdPort read FBoundPort write FBoundPort default DEF_PORT_ANY;
    property BoundPortMin: TIdPort read FBoundPortMin write FBoundPortMin default DEF_PORT_ANY;
    property BoundPortMax: TIdPort read FBoundPortMax write FBoundPortMax default DEF_PORT_ANY;
    property IPVersion;
    property Host;
    property Port;
    property ReceiveTimeout;
    property ReuseSocket;
    property TransparentProxy: TIdCustomTransparentProxy read GetTransparentProxy write SetTransparentProxy;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

implementation

uses
  IdComponent, IdResourceStringsCore, IdSocks, IdStack, IdStackConsts,
  SysUtils;

{ TIdUDPClient }

procedure TIdUDPClient.CloseProxy;
begin
  if UseProxy and FProxyOpened then begin
    FTransparentProxy.CloseUDP(Binding);
    FProxyOpened := False;
  end;
end;

procedure TIdUDPClient.Connect;
var
  LIP : String;
  // under ARC, convert a weak reference to a strong reference before working with it
  LTransparentProxy: TIdCustomTransparentProxy;
begin
  if Connected then begin
    Disconnect;
  end;
  LTransparentProxy := FTransparentProxy;
  if Assigned(LTransparentProxy) then begin
    if LTransparentProxy.Enabled then begin
      //we don't use proxy open because we want to pass a peer's hostname and port
      //in case a proxy type in the future requires this.
      LTransparentProxy.OpenUDP(Binding, Host, Port);
      FProxyOpened := True;
      FConnected := True;
      Exit;  //we're done, the transparentProxy takes care of the work.
    end;
  end;

  if FIPVersion = Id_IPv4 then
  begin
    if not GStack.IsIP(Host) then begin
      if Assigned(OnStatus) then begin
        DoStatus(hsResolving, [Host]);
      end;
      LIP := GStack.ResolveHost(Host, FIPVersion);
    end else begin
      LIP := Host;
    end;
  end
  else
  begin  //IPv6
    LIP := MakeCanonicalIPv6Address(Host);
    if LIP = '' then begin  //if MakeCanonicalIPv6Address failed, we have a hostname
      if Assigned(OnStatus) then begin
        DoStatus(hsResolving, [Host]);
      end;
      LIP := GStack.ResolveHost(Host, FIPVersion);
    end else begin
      LIP := Host;
    end;
  end;
  Binding.SetPeer(LIP, Port, FIPVersion);
  Binding.Connect;

  DoStatus(hsConnected, [Host]);
  DoOnConnected;
  FConnected := True;
end;

function TIdUDPClient.Connected: Boolean;
begin
  Result := FConnected;
  if Result then begin
    if Assigned(FBinding) then begin
      Result := FBinding.HandleAllocated;
    end else begin
      Result := False;
    end;
  end;
end;

procedure TIdUDPClient.Disconnect;
begin
  if Connected then begin
    DoStatus(hsDisconnecting);
    if UseProxy and FProxyOpened then begin
      CloseProxy;
    end;
    FBinding.CloseSocket;
    DoOnDisconnected;
    DoStatus(hsDisconnected);
    FConnected := False;
  end;
end;

procedure TIdUDPClient.DoOnConnected;
begin
  if Assigned(OnConnected) then begin
    OnConnected(Self);
  end;
end;

procedure TIdUDPClient.DoOnDisconnected;
begin
  if Assigned(OnDisconnected) then begin
    OnDisconnected(Self);
  end;
end;

function TIdUDPClient.GetBinding: TIdSocketHandle;
begin
  if FBinding = nil then begin
    FBinding := TIdSocketHandle.Create(nil);
  end;
  if not FBinding.HandleAllocated then begin
    FBinding.IPVersion := FIPVersion;
    FBinding.AllocateSocket(Id_SOCK_DGRAM);
    FBinding.IP := FBoundIP;
    FBinding.Port := FBoundPort;
    FBinding.ClientPortMin := FBoundPortMin;
    FBinding.ClientPortMax := FBoundPortMax;
    FBinding.ReuseSocket := FReuseSocket;
    FBinding.Bind;
    BroadcastEnabledChanged;
  end;
  Result := FBinding;
end;

function TIdUDPClient.GetTransparentProxy: TIdCustomTransparentProxy;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LTransparentProxy: TIdCustomTransparentProxy;
begin
  LTransparentProxy := FTransparentProxy;
  // Necessary at design time for Borland SOAP support
  if LTransparentProxy = nil then begin
    LTransparentProxy := TIdSocksInfo.Create(Self); //default
    FTransparentProxy := LTransparentProxy;
    FImplicitTransparentProxy := True;
  end;
  Result := LTransparentProxy;
end;

procedure TIdUDPClient.InitComponent;
begin
  inherited InitComponent;
  FProxyOpened := False;
  FConnected := False;
  FBoundPort := DEF_PORT_ANY;
  FBoundPortMin := DEF_PORT_ANY;
  FBoundPortMax := DEF_PORT_ANY;
end;

// under ARC, all weak references to a freed object get nil'ed automatically
// so this is mostly redundant
procedure TIdUDPClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTransparentProxy) then begin
    FTransparentProxy := nil;
    FImplicitTransparentProxy := False;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TIdUDPClient.OpenProxy;
begin
  if UseProxy and (not FProxyOpened) then begin
    FTransparentProxy.OpenUDP(Binding);
    FProxyOpened := True;
  end;
end;

function TIdUDPClient.ReceiveBuffer(var ABuffer: TIdBytes;
  const AMSec: Integer): Integer;
var
  LMSec : Integer;
  LHost : String;
  LPort : TIdPort;
  LIPVersion: TIdIPVersion;
begin
  Result := 0;
  if AMSec = IdTimeoutDefault then begin
    if ReceiveTimeout = 0 then begin
      LMSec := IdTimeoutInfinite;
    end else begin
      LMSec := ReceiveTimeout;
    end;
  end else begin
    LMSec := AMSec;
  end;
  if UseProxy then begin
    if not FProxyOpened then begin
      RaiseUseProxyError;
    end;
    Result := FTransparentProxy.RecvFromUDP(Binding, ABuffer, LHost, LPort, LIPVersion, LMSec);
  end else
  begin
    if Connected then begin
      if FBinding.Readable(LMSec) then begin //Select(LMSec)  then
        Result := FBinding.Receive(ABuffer);
      end;
    end else begin
      Result := inherited ReceiveBuffer(ABuffer, LMSec);
    end;
  end;
end;

procedure TIdUDPClient.RaiseUseProxyError;
begin
  raise EIdMustUseOpenProxy.Create(RSUDPMustUseProxyOpen);
end;

function TIdUDPClient.ReceiveBuffer(var ABuffer: TIdBytes;
  var VPeerIP: string; var VPeerPort: TIdPort; AMSec: Integer): integer;
var
  VoidIPVersion: TidIPVersion;
begin
  Result := ReceiveBuffer(ABuffer, VPeerIP, VPeerPort, VoidIPVersion, AMSec);
end;

procedure TIdUDPClient.Send(const AData: string; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  Send(Host, Port, AData, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
end;

procedure TIdUDPClient.SendBuffer(const ABuffer : TIdBytes);
begin
  if UseProxy then begin
    if not FProxyOpened then begin
      RaiseUseProxyError;
    end;
    FTransparentProxy.SendToUDP(Binding, Host, Port, IPVersion, ABuffer);
  end else
  begin
    if Connected then begin
      FBinding.Send(ABuffer, 0, -1);
    end else begin
      inherited SendBuffer(Host, Port, IPVersion, ABuffer);
    end;
  end;
end;

procedure TIdUDPClient.SendBuffer(const AHost: string; const APort: TIdPort;
  const ABuffer: TIdBytes);
begin
  if UseProxy then begin
    if not FProxyOpened then begin
      RaiseUseProxyError;
    end;
    FTransparentProxy.SendToUDP(Binding, AHost, APort, IPVersion, ABuffer);
  end else begin
    inherited SendBuffer(AHost, APort, ABuffer);
  end;
end;

procedure TIdUDPClient.SetHost(const AValue: String);
begin
  if FHost <> AValue then begin
    Disconnect;
  end;
  inherited SetHost(AValue);
end;

procedure TIdUDPClient.SetIPVersion(const AValue: TIdIPVersion);
begin
  if FIPVersion <> AValue then begin
    Disconnect;
  end;
  inherited SetIPVersion(AValue);
end;

procedure TIdUDPClient.SetPort(const AValue: TIdPort);
begin
  if FPort <> AValue then begin
    Disconnect;
  end;
  inherited SetPort(AValue);
end;

procedure TIdUDPClient.SetTransparentProxy(AProxy: TIdCustomTransparentProxy);
var
  LClass: TIdCustomTransparentProxyClass;
  // under ARC, convert a weak reference to a strong reference before working with it
  LTransparentProxy: TIdCustomTransparentProxy;
begin
  LTransparentProxy := FTransparentProxy;

  if LTransparentProxy <> AProxy then
  begin
    // All this is to preserve the compatibility with old version
    // In the case when we have SocksInfo as object created in runtime without owner form it is treated as temporary object
    // In the case when the ASocks points to an object with owner it is treated as component on form.

    // under ARC, all weak references to a freed object get nil'ed automatically

    if Assigned(AProxy) then begin
      if not Assigned(AProxy.Owner) then begin
        if Assigned(LTransparentProxy) and (not FImplicitTransparentProxy) then begin
          {$IFNDEF USE_OBJECT_ARC}
          LTransparentProxy.RemoveFreeNotification(Self);
          {$ENDIF}
          LTransparentProxy := nil;
        end;
        LClass := TIdCustomTransparentProxyClass(AProxy.ClassType);
        if Assigned(LTransparentProxy) and (LTransparentProxy.ClassType <> LClass) then begin
          FTransparentProxy := nil;
          FImplicitTransparentProxy := False;
          IdDisposeAndNil(LTransparentProxy);
        end;
        if not Assigned(LTransparentProxy) then begin
          LTransparentProxy := LClass.Create(Self);
          FTransparentProxy := LTransparentProxy;
          FImplicitTransparentProxy := True;
        end;
        LTransparentProxy.Assign(AProxy);
      end else begin
        if Assigned(LTransparentProxy) then begin
          if FImplicitTransparentProxy then begin
            FTransparentProxy := nil;
            FImplicitTransparentProxy := False;
            IdDisposeAndNil(LTransparentProxy);
          end else begin
            {$IFNDEF USE_OBJECT_ARC}
            LTransparentProxy.RemoveFreeNotification(Self);
            {$ENDIF}
          end;
        end;

        FTransparentProxy := AProxy;

        {$IFNDEF USE_OBJECT_ARC}
        AProxy.FreeNotification(Self);
        {$ENDIF}
      end;
    end
    else if Assigned(LTransparentProxy) then begin
      if FImplicitTransparentProxy then begin
        FTransparentProxy := nil;
        FImplicitTransparentProxy := False;
        IdDisposeAndNil(LTransparentProxy);
      end else begin
        FTransparentProxy := nil; //remove link
        {$IFNDEF USE_OBJECT_ARC}
        LTransparentProxy.RemoveFreeNotification(Self);
        {$ENDIF}
      end;
    end;
  end;
end;

function TIdUDPClient.UseProxy: Boolean;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LTransparentProxy: TIdCustomTransparentProxy;
begin
  LTransparentProxy := FTransparentProxy;
  Result := Assigned(LTransparentProxy);
  if Result then begin
    Result := LTransparentProxy.Enabled;
  end;
end;

destructor TIdUDPClient.Destroy;
begin
  if UseProxy and FProxyOpened then begin
    CloseProxy;
  end;
  if Connected then begin
    Disconnect;
  end;
  inherited Destroy;
end;

function TIdUDPClient.ReceiveBuffer(var ABuffer: TIdBytes;
  var VPeerIP: string; var VPeerPort: TIdPort; var VIPVersion: TIdIPVersion;
  const AMSec: Integer): integer;
var
  LMSec : Integer;
begin
  if AMSec = IdTimeoutDefault then begin
    if ReceiveTimeout = 0 then begin
      LMSec := IdTimeoutInfinite;
    end else begin
      LMSec := ReceiveTimeout;
    end;
  end else begin
    LMSec := AMSec;
  end;
  if UseProxy then begin
    if not FProxyOpened then begin
      RaiseUseProxyError;
    end;
    Result := FTransparentProxy.RecvFromUDP(Binding, ABuffer, VPeerIP, VPeerPort, VIPVersion, LMSec);
  end else begin
    Result := inherited ReceiveBuffer(ABuffer, VPeerIP, VPeerPort, VIPVersion, LMSec);
  end;
end;

procedure TIdUDPClient.SendBuffer(const AHost: string; const APort: TIdPort;
  const AIPVersion: TIdIPVersion; const ABuffer: TIdBytes);
begin
  if UseProxy then begin
    if not FProxyOpened then begin
      RaiseUseProxyError;
    end;
    FTransparentProxy.SendToUDP(Binding, AHost, APort, AIPVersion, ABuffer);
  end else begin
    inherited SendBuffer(AHost, APort, AIPVersion, ABuffer);
  end;
end;

end.

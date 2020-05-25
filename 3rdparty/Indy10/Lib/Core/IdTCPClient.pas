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
  Rev 1.38    1/15/05 2:14:58 PM  RLebeau
  Removed virtual specifier from SetConnectTimeout() and SetReadTimeout(), not
  being used by any descendants.

  Rev 1.37    11/29/2004 11:49:24 PM  JPMugaas
  Fixes for compiler errors.

  Rev 1.36    11/29/04 10:38:58 AM  RLebeau
  Updated Connect() to release the IOHandler on error if implicitally created.

  Rev 1.35    11/28/04 2:28:22 PM  RLebeau
  Added 'const' to various property setter parameters.

  Removed redundant getter methods.

  Rev 1.34    11/27/2004 8:27:44 PM  JPMugaas
  Fix for compiler errors.

  Rev 1.33    11/26/04 3:46:10 PM  RLebeau
  Added support for BoundIP and BoundPort properties

  Rev 1.32    2004.11.05 10:58:34 PM  czhower
  Changed connect overloads for C#.

  Rev 1.31    8/8/04 12:32:08 AM  RLebeau
  Redeclared ReadTimeout and ConnectTimeout properties as public instead of
  protected in TIdTCPClientCustom

  Rev 1.30    8/4/2004 5:37:34 AM  DSiders
  Changed camel-casing on ReadTimeout to be consistent with ConnectTimeout.

  Rev 1.29    8/3/04 11:17:30 AM  RLebeau
  Added support for ReadTimeout property

  Rev 1.28    8/2/04 5:50:58 PM  RLebeau
  Added support for ConnectTimeout property

  Rev 1.27    2004.03.06 10:40:28 PM  czhower
  Changed IOHandler management to fix bug in server shutdowns.

  Rev 1.26    2004.02.03 4:16:54 PM  czhower
  For unit name changes.

  Rev 1.25    1/8/2004 8:22:54 PM  JPMugaas
  SetIPVersion now virtual so I can override in TIdFTP.  Other stuff may need
  the override as well.

  Rev 1.24    1/2/2004 12:02:18 AM  BGooijen
  added OnBeforeBind/OnAfterBind

  Rev 1.23    12/31/2003 9:52:04 PM  BGooijen
  Added IPv6 support

  Rev 1.20    2003.10.14 1:27:00 PM  czhower
  Uupdates + Intercept support

  Rev 1.19    2003.10.01 9:11:26 PM  czhower
  .Net

  Rev 1.18    2003.10.01 2:30:42 PM  czhower
  .Net

  Rev 1.17    2003.10.01 11:16:36 AM  czhower
  .Net

  Rev 1.16    2003.09.30 1:23:06 PM  czhower
  Stack split for DotNet

  Rev 1.15    2003.09.18 2:59:46 PM  czhower
  Modified port and host overrides to only override if values exist.

  Rev 1.14    6/3/2003 11:48:32 PM  BGooijen
  Undid change from version 1.12, is now fixed in iohandlersocket

  Rev 1.13    2003.06.03 7:27:56 PM  czhower
  Added overloaded Connect method

  Rev 1.12    5/23/2003 6:45:32 PM  BGooijen
  ClosedGracefully is now set if Connect failes.

  Rev 1.11    2003.04.10 8:05:34 PM  czhower
  removed unneeded self. reference

  Rev 1.10    4/7/2003 06:58:32 AM  JPMugaas
  Implicit IOHandler now created in virtual method

  function TIdTCPClientCustom.MakeImplicitClientHandler: TIdIOHandler;

  Rev 1.9    3/17/2003 9:40:16 PM  BGooijen
  Host and Port were not properly synchronised with the IOHandler, fixed that

  Rev 1.8    3/5/2003 11:05:24 PM  BGooijen
  Intercept

  Rev 1.7    2003.02.25 1:36:16 AM  czhower

  Rev 1.6    12-14-2002 22:52:34  BGooijen
  now also saves host and port settings when an explicit iohandler is used. the
  host and port settings are copied to the iohandler if the iohandler doesn't
  have them specified.

  Rev 1.5    12-14-2002 22:38:26  BGooijen
  The host and port settings were lost when the implicit iohandler  was created
  in .Connect, fixed that.

  Rev 1.4    2002.12.07 12:26:12 AM  czhower

  Rev 1.2    12/6/2002 02:11:42 PM  JPMugaas
  Protected Port and Host properties added to TCPClient because those are
  needed by protocol implementations.  Socket property added to TCPConnection.

  Rev 1.1    6/12/2002 4:08:34 PM  SGrobety

  Rev 1.0    11/13/2002 09:00:26 AM  JPMugaas
}

unit IdTCPClient;

{$i IdCompilerDefines.inc}

interface

uses
  Classes,
  IdGlobal, IdExceptionCore, IdIOHandler, IdTCPConnection;

(*$HPPEMIT '#if defined(_VCL_ALIAS_RECORDS)' *)
(*$HPPEMIT '#if !defined(UNICODE)' *)
(*$HPPEMIT '#pragma alias "@Idtcpclient@TIdTCPClientCustom@SetPortA$qqrxus"="@Idtcpclient@TIdTCPClientCustom@SetPort$qqrxus"' *)
(*$HPPEMIT '#else' *)
(*$HPPEMIT '#pragma alias "@Idtcpclient@TIdTCPClientCustom@SetPortW$qqrxus"="@Idtcpclient@TIdTCPClientCustom@SetPort$qqrxus"' *)
(*$HPPEMIT '#endif' *)
(*$HPPEMIT '#endif' *)

type

  TIdTCPClientCustom = class(TIdTCPConnection)
  protected
    FBoundIP: String;
    FBoundPort: TIdPort;
    FBoundPortMax: TIdPort;
    FBoundPortMin: TIdPort;
    FConnectTimeout: Integer;
    FDestination: string;
    FHost: string;
    FIPVersion: TIdIPVersion;
    FOnConnected: TNotifyEvent;
    FPassword: string;
    FPort: TIdPort;
    FReadTimeout: Integer;
    FUsername: string;
    FReuseSocket: TIdReuseSocket;
    FUseNagle: Boolean;
    //
    FOnBeforeBind: TNotifyEvent;
    FOnAfterBind: TNotifyEvent;
    FOnSocketAllocated: TNotifyEvent;
    //
    procedure DoOnConnected; virtual;
    function MakeImplicitClientHandler: TIdIOHandler; virtual;
    //
    procedure SetConnectTimeout(const AValue: Integer);
    procedure SetReadTimeout(const AValue: Integer);
    procedure SetReuseSocket(const AValue: TIdReuseSocket);
    procedure SetUseNagle(const AValue: Boolean);
    procedure SetBoundIP(const AValue: String);
    procedure SetBoundPort(const AValue: TIdPort);
    procedure SetBoundPortMax(const AValue: TIdPort);
    procedure SetBoundPortMin(const AValue: TIdPort);
    procedure SetHost(const AValue: string); virtual;
    procedure SetPort(const AValue: TIdPort); virtual;
    procedure SetIPVersion(const AValue: TIdIPVersion); virtual;
    //
    procedure SetOnBeforeBind(const AValue: TNotifyEvent);
    procedure SetOnAfterBind(const AValue: TNotifyEvent);
    procedure SetOnSocketAllocated(const AValue: TNotifyEvent);
    //
    procedure SetIOHandler(AValue: TIdIOHandler); override;
    procedure InitComponent; override;
    //
    function GetReadTimeout: Integer;
    function GetReuseSocket: TIdReuseSocket;
    function GetUseNagle: Boolean;
    //
    property Host: string read FHost write SetHost;
    property IPVersion: TIdIPVersion read FIPVersion write SetIPVersion;
    property Password: string read FPassword write FPassword;
    property Port: TIdPort read FPort write SetPort;
    property Username: string read FUsername write FUsername;
  public
    procedure Connect; overload; virtual;
    // This is overridden and not as default params so that descendants
    // do not have to worry about the arguments.
    // Also has been split further to allow usage from C# as it does not have optional
    // params
    procedure Connect(const AHost: string); overload;
    procedure Connect(const AHost: string; const APort: TIdPort); overload;
    function ConnectAndGetAll: string; virtual;
    //
    property BoundIP: string read FBoundIP write SetBoundIP;
    property BoundPort: TIdPort read FBoundPort write SetBoundPort default DEF_PORT_ANY;
    property BoundPortMax: TIdPort read FBoundPortMax write SetBoundPortMax default DEF_PORT_ANY;
    property BoundPortMin: TIdPort read FBoundPortMin write SetBoundPortMin default DEF_PORT_ANY;
    //
    property ConnectTimeout: Integer read FConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ReuseSocket: TIdReuseSocket read GetReuseSocket write SetReuseSocket default rsOSDependent;
    property UseNagle: Boolean read GetUseNagle write SetUseNagle default True;
    //
    property OnBeforeBind: TNotifyEvent read FOnBeforeBind write SetOnBeforeBind;
    property OnAfterBind: TNotifyEvent read FOnAfterBind write SetOnAfterBind;
    property OnSocketAllocated: TNotifyEvent read FOnSocketAllocated write SetOnSocketAllocated;
    //
  published
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
  end;

  TIdTCPClient = class(TIdTCPClientCustom)
  published
    property BoundIP;
    property BoundPort;
    property ConnectTimeout;
    property Host;
    property IPVersion;
    property Port;
    property ReadTimeout;
    property ReuseSocket;
    property UseNagle;

    property OnBeforeBind;
    property OnAfterBind;
    property OnSocketAllocated;
  end;
  //Temp IFDEF till we change aliaser
  // Temp - reversed it for code freeze - will rereverse later.

implementation

uses
  IdComponent, IdResourceStringsCore, IdIOHandlerSocket;

{ TIdTCPClientCustom }

procedure TIdTCPClientCustom.InitComponent;
begin
  inherited InitComponent;
  FReadTimeOut := IdTimeoutDefault;
  FBoundPort := DEF_PORT_ANY;
  FBoundPortMin := DEF_PORT_ANY;
  FBoundPortMax := DEF_PORT_ANY;
  FUseNagle := True;
end;

procedure TIdTCPClientCustom.Connect;
begin
  if Connected then begin
    raise EIdAlreadyConnected.Create(RSAlreadyConnected);
  end;

  if Host = '' then begin
    raise EIdHostRequired.Create('A Host is required'); {do not localize}
  end;
  if Port = 0 then begin
    raise EIdPortRequired.Create('A Port is required'); {do not localize}
  end;

  if IOHandler = nil then begin
    IOHandler := MakeImplicitClientHandler;
    ManagedIOHandler := True;

    // TODO: always assign the OnStatus event even if the IOHandler is not implicit?
    IOHandler.OnStatus := OnStatus;
  end;

  try
    // Bypass GetDestination
    if FDestination <> '' then begin
      IOHandler.Destination := FDestination;
    end;

{BGO: not any more, TIdTCPClientCustom has precedence now (for port protocols, and things like that)
    // We retain the settings that are in here (filled in by the user)
    // we only do this when the iohandler has no settings,
    // because the iohandler has precedence
    if (IOHandler.Port = 0) and (IOHandler.Host = '') then begin
      IOHandler.Port := FPort;
      IOHandler.Host := FHost;
    end;
}

    IOHandler.Port := FPort; //BGO: just to make sure
    IOHandler.Host := FHost;
    IOHandler.ConnectTimeout := FConnectTimeout;
    IOHandler.ReadTimeout := FReadTimeout;

    if Socket <> nil then begin
      Socket.BoundIP := FBoundIP;
      Socket.BoundPort := FBoundPort;
      Socket.BoundPortMin := FBoundPortMin;
      Socket.BoundPortMax := FBoundPortMax;
      Socket.IPVersion := FIPVersion;
      Socket.ReuseSocket := FReuseSocket;
      Socket.UseNagle := FUseNagle;
      Socket.OnBeforeBind := FOnBeforeBind;
      Socket.OnAfterBind := FOnAfterBind;
      Socket.OnSocketAllocated := FOnSocketAllocated;
    end;

    IOHandler.Open;
    if IOHandler.Intercept <> nil then begin
      IOHandler.Intercept.Connect(Self);
    end;

    DoStatus(hsConnected, [Host]);
    DoOnConnected;
  except
    if IOHandler <> nil then begin
      IOHandler.Close;
      if ManagedIOHandler then begin
        IOHandler := nil; // RLebeau - SetIOHandler() will free the IOHandler
      end;
    end;
    raise;
  end;
end;

function TIdTCPClientCustom.ConnectAndGetAll: string;
begin
  Connect; try
    Result := IOHandler.AllData;
  finally Disconnect; end;
end;

procedure TIdTCPClientCustom.DoOnConnected;
begin
  if Assigned(OnConnected) then begin
    OnConnected(Self);
  end;
end;

procedure TIdTCPClientCustom.SetConnectTimeout(const AValue: Integer);
begin
  FConnectTimeout := AValue;
  if IOHandler <> nil then begin
    IOHandler.ConnectTimeout := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetReadTimeout(const AValue: Integer);
begin
  FReadTimeout := AValue;
  if IOHandler <> nil then begin
    IOHandler.ReadTimeout := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetReuseSocket(const AValue: TIdReuseSocket);
begin
  FReuseSocket := AValue;
  if Socket <> nil then begin
    Socket.ReuseSocket := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetUseNagle(const AValue: Boolean);
begin
  FUseNagle := AValue;
  if Socket <> nil then begin
    Socket.UseNagle := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetBoundIP(const AValue: String);
begin
  FBoundIP := AValue;
  if Socket <> nil then begin
    Socket.BoundIP := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetBoundPort(const AValue: TIdPort);
begin
  FBoundPort := AValue;
  if Socket <> nil then begin
    Socket.BoundPort := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetBoundPortMax(const AValue: TIdPort);
begin
  FBoundPortMax := AValue;
  if Socket <> nil then begin
    Socket.BoundPortMax := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetBoundPortMin(const AValue: TIdPort);
begin
  FBoundPortMin := AValue;
  if Socket <> nil then begin
    Socket.BoundPortMin := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetHost(const AValue: string);
begin
  FHost := AValue;
  if IOHandler <> nil then begin
    IOHandler.Host := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetPort(const AValue: TIdPort);
begin
  FPort := AValue;
  if IOHandler <> nil then begin
    IOHandler.Port := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetIPVersion(const AValue: TIdIPVersion);
begin
  FIPVersion := AValue;
  if Socket <> nil then begin
    Socket.IPVersion := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetOnBeforeBind(const AValue: TNotifyEvent);
begin
  FOnBeforeBind := AValue;
  if Socket <> nil then begin
    Socket.OnBeforeBind := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetOnAfterBind(const AValue: TNotifyEvent);
begin
  FOnAfterBind := AValue;
  if Socket <> nil then begin
    Socket.OnAfterBind := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetOnSocketAllocated(const AValue: TNotifyEvent);
begin
  FOnSocketAllocated := AValue;
  if Socket <> nil then begin
    Socket.OnSocketAllocated := AValue;
  end;
end;

procedure TIdTCPClientCustom.SetIOHandler(AValue: TIdIOHandler);
begin
  inherited SetIOHandler(AValue);
  // TIdTCPClientCustom overrides settings in iohandler to initialize
  // protocol defaults.
  if IOHandler <> nil then begin
    IOHandler.Port := FPort;
    IOHandler.Host := FHost;
    IOHandler.ConnectTimeout := FConnectTimeout;
    IOHandler.ReadTimeout := FReadTimeout;
  end;
  if Socket <> nil then begin
    Socket.BoundIP := FBoundIP;
    Socket.BoundPort := FBoundPort;
    Socket.BoundPortMin := FBoundPortMin;
    Socket.BoundPortMax := FBoundPortMax;
    Socket.IPVersion := FIPVersion;
    Socket.ReuseSocket := FReuseSocket;
    Socket.UseNagle := FUseNagle;

    // TODO: use local event handlers that then trigger the user event handler if assigned
    Socket.OnBeforeBind := FOnBeforeBind;
    Socket.OnAfterBind := FOnAfterBind;
    Socket.OnSocketAllocated := FOnSocketAllocated;
  end;
end;

function TIdTCPClientCustom.MakeImplicitClientHandler: TIdIOHandler;
begin
  Result := TIdIOHandler.MakeDefaultIOHandler(Self);
end;

procedure TIdTCPClientCustom.Connect(const AHost: string);
begin
  Host := AHost;
  Connect;
end;

procedure TIdTCPClientCustom.Connect(const AHost: string; const APort: TIdPort);
begin
  Host := AHost;
  Port := APort;
  Connect;
end;

function TIdTCPClientCustom.GetReadTimeout: Integer;
begin
  if IOHandler <> nil then begin
    Result := IOHandler.ReadTimeout;
  end else begin
    Result := FReadTimeout;
  end;
end;

function TIdTCPClientCustom.GetReuseSocket: TIdReuseSocket;
begin
  if Socket <> nil then begin
    Result := Socket.ReuseSocket;
  end else begin
    Result := FReuseSocket;
  end;
end;

function TIdTCPClientCustom.GetUseNagle: Boolean;
begin
  if Socket <> nil then begin
    Result := Socket.UseNagle;
  end else begin
    Result := FUseNagle;
  end;
end;

end.

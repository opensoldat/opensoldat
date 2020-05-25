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
  Rev 1.15    11/12/2004 11:30:18 AM  JPMugaas
  Expansions for IPv6.

  Rev 1.14    11/11/04 12:05:32 PM  RLebeau
  Updated ReceiveBuffer() to set AMSec to IdTimeoutInfinite when the
  ReceiveTimeout property is 0

  Rev 1.13    11/7/2004 11:33:30 PM  JPMugaas
  Now uses Connect, Disconnect, Send, and Receive similarly to the TCP Clients.
  This should prevent unneeded DNS name to IP address conversions that SendTo
  was doing.

  Rev 1.12    7/21/04 3:33:10 PM  RLebeau
  Updated TIdUDPBase.ReceiveString() to use new BytesToString() parameters

  Rev 1.11    09/06/2004 00:29:56  CCostelloe
  Kylix 3 patch

  Rev 1.10    2004.02.03 4:17:00 PM  czhower
  For unit name changes.

  Rev 1.9    21.1.2004 ã. 12:31:00  DBondzhev
  Fix for Indy source. Workaround for dccil bug
  now it can be compiled using Compile instead of build

  Rev 1.7    10/26/2003 12:30:18 PM  BGooijen
  DotNet

  Rev 1.6    10/24/2003 5:18:36 PM  BGooijen
  Removed boolean shortcutting from .GetActive

  Rev 1.5    10/22/2003 04:40:58 PM  JPMugaas
  Should compile with some restored functionality.  Still not finished.

  Rev 1.4    10/19/2003 9:34:30 PM  BGooijen
  SetSocketOption

  Rev 1.3    2003.10.11 9:58:48 PM  czhower
  Started on some todos

  Rev 1.2    2003.10.11 5:52:10 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.1    2003.09.30 1:23:08 PM  czhower
  Stack split for DotNet

  Rev 1.0    11/13/2002 09:02:06 AM  JPMugaas
}

unit IdUDPBase;

interface

{$I IdCompilerDefines.inc}
//here to flip FPC into Delphi mode

uses
  IdComponent,
  IdGlobal,
  IdException,
  IdSocketHandle;

(*$HPPEMIT '#if defined(_VCL_ALIAS_RECORDS)' *)
(*$HPPEMIT '#if !defined(UNICODE)' *)
(*$HPPEMIT '#pragma alias "@Idudpbase@TIdUDPBase@SetPortA$qqrxus"="@Idudpbase@TIdUDPBase@SetPort$qqrxus"' *)
(*$HPPEMIT '#else' *)
(*$HPPEMIT '#pragma alias "@Idudpbase@TIdUDPBase@SetPortW$qqrxus"="@Idudpbase@TIdUDPBase@SetPort$qqrxus"' *)
(*$HPPEMIT '#endif' *)
(*$HPPEMIT '#endif' *)

const
  ID_UDP_BUFFERSIZE = 8192;

type
  TIdUDPBase = class(TIdComponent)
  protected
    FBinding: TIdSocketHandle;
    FBufferSize: Integer;
    FDsgnActive: Boolean;
    FHost: String;
    FPort: TIdPort;
    FReceiveTimeout: Integer;
    FReuseSocket: TIdReuseSocket;
    FIPVersion: TIdIPVersion;
    //
    FBroadcastEnabled: Boolean;
    procedure BroadcastEnabledChanged; dynamic;
    procedure CloseBinding; virtual;
    function GetActive: Boolean; virtual;
    procedure InitComponent; override;
    procedure SetActive(const Value: Boolean);
    procedure SetBroadcastEnabled(const AValue: Boolean);
    function GetBinding: TIdSocketHandle; virtual; abstract;
    procedure Loaded; override;

    function GetIPVersion: TIdIPVersion;  virtual;
    procedure SetIPVersion(const AValue: TIdIPVersion); virtual;

    function GetHost : String; virtual;
    procedure SetHost(const AValue : String); virtual;

    function GetPort : TIdPort; virtual;
    procedure SetPort(const AValue : TIdPort); virtual;

    property Host: string read GetHost write SetHost;
    property Port: TIdPort read GetPort write SetPort;
  public
    destructor Destroy; override;
    //
    property Binding: TIdSocketHandle read GetBinding;
    procedure Broadcast(const AData: string; const APort: TIdPort; const AIP: String = '';
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure Broadcast(const AData: TIdBytes; const APort: TIdPort; const AIP: String = ''); overload;
    function ReceiveBuffer(var ABuffer : TIdBytes; var VPeerIP: string; var VPeerPort: TIdPort;
      AMSec: Integer = IdTimeoutDefault): integer; overload; virtual;
    function ReceiveBuffer(var ABuffer : TIdBytes; var VPeerIP: string; var VPeerPort: TIdPort;
      var VIPVersion: TIdIPVersion; const AMSec: Integer = IdTimeoutDefault): integer; overload; virtual;
    function ReceiveBuffer(var ABuffer : TIdBytes;
     const AMSec: Integer = IdTimeoutDefault): Integer; overload;  virtual;
    function ReceiveString(const AMSec: Integer = IdTimeoutDefault; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; overload;
    function ReceiveString(var VPeerIP: string; var VPeerPort: TIdPort;
     const AMSec: Integer = IdTimeoutDefault; AByteEncoding: IIdTextEncoding = nil
     {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
     ): string; overload;
    procedure Send(const AHost: string; const APort: TIdPort; const AData: string;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      );
    procedure SendBuffer(const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion; const ABuffer : TIdBytes); overload; virtual;
    procedure SendBuffer(const AHost: string; const APort: TIdPort; const ABuffer: TIdBytes); overload; virtual;
    //
    property ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default IdTimeoutInfinite;
    property ReuseSocket: TIdReuseSocket read FReuseSocket write FReuseSocket default rsOSDependent;
  published
    property Active: Boolean read GetActive write SetActive Default False;
    property BufferSize: Integer read FBufferSize write FBufferSize default ID_UDP_BUFFERSIZE;
    property BroadcastEnabled: Boolean read FBroadcastEnabled
     write SetBroadcastEnabled default False;
    property IPVersion: TIdIPVersion read GetIPVersion write SetIPVersion default ID_DEFAULT_IP_VERSION;
  end;
  EIdUDPException = Class(EIdException);
  EIdUDPReceiveErrorZeroBytes = class(EIdUDPException);

implementation

uses
  IdStackConsts, IdStack, SysUtils;

{ TIdUDPBase }

procedure TIdUDPBase.Broadcast(const AData: string; const APort: TIdPort;
  const AIP: String = ''; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF});
begin
  Binding.Broadcast(AData, APort, AIP, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF});
end;

procedure TIdUDPBase.Broadcast(const AData: TIdBytes; const APort: TIdPort;
  const AIP: String = '');
begin
  Binding.Broadcast(AData, APort, AIP);
end;

procedure TIdUDPBase.BroadcastEnabledChanged;
begin
  if Assigned(FBinding) then begin
    FBinding.BroadcastEnabled := BroadcastEnabled;
  end;
end;

procedure TIdUDPBase.CloseBinding;
begin
  FreeAndNil(FBinding);
end;

destructor TIdUDPBase.Destroy;
begin
  Active := False;
  //double check that binding gets freed.
  //sometimes possible that binding is allocated, but active=false
  CloseBinding;
  inherited Destroy;
end;

function TIdUDPBase.GetActive: Boolean;
begin
  Result := FDsgnActive;
  if not Result then begin
    if Assigned(FBinding) then begin
      Result := FBinding.HandleAllocated;
    end;
  end;
end;

function TIdUDPBase.GetHost: String;
begin
  Result := FHost;
end;

function TIdUDPBase.GetIPVersion: TIdIPVersion;
begin
  Result := FIPVersion;
end;

function TIdUDPBase.GetPort: TIdPort;
begin
  Result := FPort;
end;

procedure TIdUDPBase.InitComponent;
begin
  inherited InitComponent;
  BufferSize := ID_UDP_BUFFERSIZE;
  FReceiveTimeout := IdTimeoutInfinite;
  FReuseSocket := rsOSDependent;
  FIPVersion := ID_DEFAULT_IP_VERSION;
end;

procedure TIdUDPBase.Loaded;
var
  b: Boolean;
begin
  inherited Loaded;
  b := FDsgnActive;
  FDsgnActive := False;
  Active := b;
end;

function TIdUDPBase.ReceiveBuffer(var ABuffer : TIdBytes;
  const AMSec: Integer = IdTimeoutDefault): Integer;
var
  VoidIP: string;
  VoidPort:  TIdPort;
  VoidIPVer: TIdIPVersion;
begin
  Result := ReceiveBuffer(ABuffer, VoidIP, VoidPort, VoidIPVer, AMSec);
end;

function TIdUDPBase.ReceiveBuffer(var ABuffer : TIdBytes;
  var VPeerIP: string; var VPeerPort: TIdPort;
  AMSec: Integer = IdTimeoutDefault): integer;
var
  VoidIPVer : TIdIPVersion;
begin
  Result := ReceiveBuffer(ABuffer, VPeerIP, VPeerPort, VoidIPVer, AMSec);
 // GBSDStack.CheckForSocketError(Result);
end;

function TIdUDPBase.ReceiveBuffer(var ABuffer : TIdBytes;
  var VPeerIP: string; var VPeerPort: TIdPort; var VIPVersion: TIdIPVersion;
  const AMSec: Integer = IdTimeoutDefault): integer;
var
  LMSec : Integer;
begin
  if AMSec = IdTimeoutDefault then begin
    if ReceiveTimeOut = 0 then begin
      LMSec := IdTimeoutInfinite;
    end else begin
      LMSec := ReceiveTimeOut;
    end;
  end else begin
    LMSec := AMSec;
  end;
  if not Binding.Readable(LMSec) then begin
    Result := 0;
    VPeerIP := '';    {Do not Localize}
    VPeerPort := 0;
    Exit;
  end;
  Result := Binding.RecvFrom(ABuffer, VPeerIP, VPeerPort, VIPVersion);
end;

function TIdUDPBase.ReceiveString(var VPeerIP: string; var VPeerPort: TIdPort;
  const AMSec: Integer = IdTimeoutDefault; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  i: Integer;
  LBuffer : TIdBytes;
begin
  SetLength(LBuffer, BufferSize);
  i := ReceiveBuffer(LBuffer, VPeerIP, VPeerPort, AMSec);
  Result := BytesToString(LBuffer, 0, i, AByteEncoding{$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF});
end;

function TIdUDPBase.ReceiveString(const AMSec: Integer = IdTimeoutDefault;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}): string;
var
  VoidIP: string;
  VoidPort: TIdPort;
begin
  Result := ReceiveString(VoidIP, VoidPort, AMSec, AByteEncoding{$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF});
end;

procedure TIdUDPBase.Send(const AHost: string; const APort: TIdPort; const AData: string;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  SendBuffer(AHost, APort, ToBytes(AData, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}));
end;

procedure TIdUDPBase.SendBuffer(const AHost: string; const APort: TIdPort; const ABuffer: TIdBytes);
begin
  SendBuffer(AHost, APort, IPVersion, ABuffer);
end;

procedure TIdUDPBase.SendBuffer(const AHost: string; const APort: TIdPort;
  const AIPVersion: TIdIPVersion; const ABuffer: TIdBytes);
var
  LIP : String;
begin
  LIP := GStack.ResolveHost(AHost, AIPVersion);
  Binding.SendTo(LIP, APort, ABuffer,AIPVersion);
end;

procedure TIdUDPBase.SetActive(const Value: Boolean);
begin
  if Active <> Value then begin
    if not (IsDesignTime or IsLoading) then begin
      if Value then begin
        GetBinding;
      end
      else begin
        CloseBinding;
      end;
    end
    else begin  // don't activate at designtime (or during loading of properties)    {Do not Localize}
      FDsgnActive := Value;
    end;
  end;
end;

procedure TIdUDPBase.SetBroadcastEnabled(const AValue: Boolean);
begin
  if FBroadCastEnabled <> AValue then begin
    FBroadcastEnabled := AValue;
    if Active then begin
      BroadcastEnabledChanged;
    end;
  end;
end;

procedure TIdUDPBase.SetHost(const AValue: String);
begin
  FHost := Avalue;
end;

procedure TIdUDPBase.SetIPVersion(const AValue: TIdIPVersion);
begin
  FIPVersion := AValue;
end;

procedure TIdUDPBase.SetPort(const AValue: TIdPort);
begin
  FPort := AValue;
end;

end.

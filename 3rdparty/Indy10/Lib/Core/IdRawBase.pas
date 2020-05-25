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
  Rev 1.15    7/9/04 4:26:28 PM  RLebeau
  Removed TIdBytes local variable from Send()

  Rev 1.14    09/06/2004 00:28:00  CCostelloe
  Kylix 3 patch

  Rev 1.13    4/25/2004 7:54:26 AM  JPMugaas
  Fix for AV.

  Rev 1.12    2/8/2004 12:58:42 PM  JPMugaas
  Should now compile in DotNET.

  Rev 1.11    2004.02.03 4:16:48 PM  czhower
  For unit name changes.

  Rev 1.10    2/1/2004 6:10:14 PM  JPMugaas
  Should compile better.

  Rev 1.9    2/1/2004 4:52:34 PM  JPMugaas
  Removed the rest of the Todo; items.

  Rev 1.8    2004.01.20 10:03:30 PM  czhower
  InitComponent

  Rev 1.7    2004.01.02 9:38:46 PM  czhower
  Removed warning

  Rev 1.6    2003.10.24 10:09:54 AM  czhower
  Compiles

  Rev 1.5    2003.10.20 12:03:08 PM  czhower
  Added IdStackBSDBase to make it compile again.

  Rev 1.4    10/19/2003 10:41:12 PM  BGooijen
  Compiles in DotNet and D7 again

  Rev 1.3    10/19/2003 9:34:28 PM  BGooijen
  SetSocketOption

  Rev 1.2    2003.10.11 5:48:58 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.1    2003.09.30 1:23:00 PM  czhower
  Stack split for DotNet

  Rev 1.0    11/13/2002 08:45:24 AM  JPMugaas
}

unit IdRawBase;

interface

{
  We need to selectively disable some functionality in DotNET with buffers as
  we don't want to impact anything else such as TIdICMPClient.
}

{$I IdCompilerDefines.inc}

uses
  IdComponent, IdGlobal, IdSocketHandle, IdStack,
  {$IFDEF WINDOWS}
  IdWship6,
  {$ENDIF}
  IdStackConsts;

const
  Id_TIdRawBase_Port = 0;
  Id_TIdRawBase_BufferSize = 8192;
  GReceiveTimeout = 0;
  GFTTL = 128;

type
  TIdRawBase = class(TIdComponent)
  protected
    FBinding: TIdSocketHandle;
    FHost: string;
    FPort: TIdPort;
    FReceiveTimeout: integer;
    FProtocol: TIdSocketProtocol;
    FProtocolIPv6 : TIdSocketProtocol;
    FTTL: Integer;
    FPkt : TIdPacketInfo;
    FConnected : Boolean;
    //
    function GetBinding: TIdSocketHandle;
    function GetIPVersion: TIdIPVersion;
    //
    procedure InitComponent; override;
    procedure SetIPVersion(const AValue: TIdIPVersion);
    procedure SetTTL(const Value: Integer);
    procedure SetHost(const AValue : String); virtual;
    //
    // TODO: figure out which ReceiveXXX functions we want
    //
    property IPVersion : TIdIPVersion read GetIPVersion write SetIPVersion;
    //
    property Port: TIdPort read FPort write FPort default Id_TIdRawBase_Port;
    property Protocol: TIdSocketProtocol read FProtocol write FProtocol default Id_IPPROTO_RAW;
    property ProtocolIPv6 : TIdSocketProtocol read FProtocolIPv6 write FProtocolIPv6;
    property TTL: Integer read FTTL write SetTTL default GFTTL;

  public
    destructor Destroy; override;

    function ReceiveBuffer(var VBuffer : TIdBytes; ATimeOut: Integer = -1): Integer;
    procedure Send(const AData: string; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload; virtual;
    procedure Send(const AData: TIdBytes); overload;  virtual;
    procedure Send(const AHost: string; const APort: TIdPort; const AData: string;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload; virtual;
    procedure Send(const AHost: string; const APort: TIdPort; const ABuffer : TIdBytes); overload; virtual;
    //
    property Binding: TIdSocketHandle read GetBinding;
    property ReceiveTimeout: integer read FReceiveTimeout write FReceiveTimeout Default GReceiveTimeout;
  published
    property Host: string read FHost write SetHost;
  end;


implementation

uses
  SysUtils;

{ TIdRawBase }

destructor TIdRawBase.Destroy;
begin
  FreeAndNil(FBinding);
  FreeAndNil(FPkt);
  inherited Destroy;
end;

function TIdRawBase.GetBinding: TIdSocketHandle;
begin
  if not FBinding.HandleAllocated then begin
    if FBinding.IPVersion = Id_IPv4 then
    begin
      FBinding.AllocateSocket(Id_SOCK_RAW, FProtocol);
    end else
    begin
      FBinding.AllocateSocket(Id_SOCK_RAW, FProtocolIPv6);
      {$IFDEF DOTNET}
        {$IFDEF DOTNET_2_OR_ABOVE}
      {
      Microsoft NET Framework 1.1 may actually have the packetinfo option but that
      will not do you any good because you need a RecvMsg function which is not
      in NET 1.1.  NET 2.0 does have a RecvMsg function, BTW.
      }
      //indicate we want packet information with RecvMsg calls
      FBinding.SetSockOpt(Id_SOL_IPv6, Id_IPV6_PKTINFO, 1);
        {$ENDIF}
      {$ELSE}
      //indicate we want packet information with RecvMsg WSARecvMsg calls
      FBinding.SetSockOpt(Id_SOL_IPv6, Id_IPV6_PKTINFO, 1);
      FBinding.SetSockOpt(Id_SOL_IPv6, Id_IPV6_HOPLIMIT, 1);
      {$ENDIF}
    end;
    //set hop limit (or TTL as it was called in IPv4
    FBinding.SetTTL(FTTL);
  end;
  Result := FBinding;
end;

function TIdRawBase.ReceiveBuffer(var VBuffer : TIdBytes; ATimeOut: Integer = -1): Integer;
var
  LIP : String;
  LPort : TIdPort;
  LIPVersion: TIdIPVersion;
begin
  Result := 0;
  // TODO: pass flags to recv()
  if ATimeOut < 0 then
  begin
    ATimeOut := FReceiveTimeout;
  end;
  if Length(VBuffer) > 0 then
  begin
    if Binding.Readable(ATimeOut) then begin
      if FBinding.IPVersion = Id_IPv4 then
      begin
        Result := Binding.RecvFrom(VBuffer, LIP, LPort, LIPVersion);
        FPkt.Reset;
        FPkt.SourceIP := LIP;
        FPkt.SourcePort := LPort;
        FPkt.SourceIPVersion := LIPVersion;
        FPkt.DestIPVersion := LIPVersion;
      end else
      begin
        {
        IMPORTANT!!!!

        Do NOT call GStack.ReceiveMsg unless it is absolutely necessary.
        The reasons are:

        1) WSARecvMsg is only supported on WindowsXP or later.  I think Linux
        might have a RecvMsg function as well but I'm not sure.
        2) GStack.ReceiveMsg is not supported in the Microsoft NET framework 1.1.
        It may be supported in later versions.

        For IPv4 and raw sockets, it usually isn't because we get the raw header itself.

        For IPv6 and raw sockets, we call this to get information about the destination
        IP address and hopefully, the TTL (hop count).
        }

        Result := GStack.ReceiveMsg(Binding.Handle, VBuffer, FPkt);
      end;
    end;
  end;
end;

procedure TIdRawBase.Send(const AHost: string; const APort: TIdPort; const AData: string;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  Send(AHost, APort, ToBytes(AData, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}));
end;

procedure TIdRawBase.Send(const AData: string;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  Send(ToBytes(AData, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}));
end;

procedure TIdRawBase.Send(const AData: TIdBytes);
begin
  Send(Host, Port, AData);
end;

procedure TIdRawBase.Send(const AHost: string; const APort: TIdPort; const ABuffer : TIdBytes);
var
  LIP : String;
begin
  LIP := GStack.ResolveHost(AHost, FBinding.IPVersion);
  Binding.SendTo(LIP, APort, ABuffer, FBinding.IPVersion);
end;

procedure TIdRawBase.SetTTL(const Value: Integer);
begin
  if FTTL <> Value then
  begin
    FTTL := Value;
    if FBinding.HandleAllocated then
    begin
      FBinding.SetTTL(FTTL);
    end;
  end;
end;

procedure TIdRawBase.InitComponent;
begin
  inherited InitComponent;
  FBinding := TIdSocketHandle.Create(nil);
  FBinding.IPVersion := Id_IPv4;
  FPkt := TIdPacketInfo.Create;
  ReceiveTimeout := GReceiveTimeout;
  FPort := Id_TIdRawBase_Port;
  FProtocol := Id_IPPROTO_RAW;
  FTTL := GFTTL;
end;

function TIdRawBase.GetIPVersion;
begin
  Result := FBinding.IPVersion;
end;

procedure TIdRawBase.SetIPVersion(const AValue: TIdIPVersion);
begin
  FBinding.IPVersion := AValue;
end;

procedure TIdRawBase.SetHost(const AValue: String);
begin
  FHost := AValue;
end;

end.

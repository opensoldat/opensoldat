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
  Rev 1.8    10/26/2004 8:12:30 PM  JPMugaas
  Now uses TIdStrings and TIdStringList for portability.

  Rev 1.7    12/06/2004 15:16:44  CCostelloe
  Restructured to remove inconsistencies with derived classes

  Rev 1.6    07/06/2004 21:30:48  CCostelloe
  Kylix 3 changes

  Rev 1.5    5/15/2004 3:32:30 AM  DSiders
  Corrected case in name of TIdIPAddressRec.

  Rev 1.4    4/18/04 10:29:24 PM  RLebeau
  Added TIdInt64Parts structure

  Rev 1.3    2004.04.18 4:41:40 PM  czhower
  RaiseSocketError

  Rev 1.2    2004.03.07 11:45:24 AM  czhower
  Flushbuffer fix + other minor ones found

  Rev 1.1    3/6/2004 5:16:24 PM  JPMugaas
  Bug 67 fixes.  Do not write to const values.

  Rev 1.0    2004.02.03 3:14:44 PM  czhower
  Move and updates

  Rev 1.22    2/1/2004 3:28:26 AM  JPMugaas
  Changed WSGetLocalAddress to GetLocalAddress and moved into IdStack since
  that will work the same in the DotNET as elsewhere.  This is required to
  reenable IPWatch.

  Rev 1.21    1/31/2004 1:13:00 PM  JPMugaas
  Minor stack changes required as DotNET does support getting all IP addresses
  just like the other stacks.

  Rev 1.20    12/4/2003 3:14:56 PM  BGooijen
  Added HostByAddress

  Rev 1.19    12/31/2003 9:52:00 PM  BGooijen
  Added IPv6 support

  Rev 1.18    10/26/2003 5:04:24 PM  BGooijen
  UDP Server and Client

  Rev 1.17    10/26/2003 09:10:24 AM  JPMugaas
  Calls necessary for IPMulticasting.

  Rev 1.16    10/22/2003 04:41:04 PM  JPMugaas
  Should compile with some restored functionality.  Still not finished.

  Rev 1.15    10/21/2003 06:24:24 AM  JPMugaas
  BSD Stack now have a global variable for refercing by platform specific
  things.  Removed corresponding var from Windows stack.

  Rev 1.14    10/19/2003 5:21:28 PM  BGooijen
  SetSocketOption

  Rev 1.13    2003.10.11 5:51:08 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.12    10/5/2003 9:55:28 PM  BGooijen
  TIdTCPServer works on D7 and DotNet now

  Rev 1.11    04/10/2003 22:32:02  HHariri
  moving of WSNXXX method to IdStack and renaming of the DotNet ones

  Rev 1.10    10/2/2003 7:36:28 PM  BGooijen
  .net

  Rev 1.9    2003.10.02 10:16:30 AM  czhower
  .Net

  Rev 1.8    2003.10.01 9:11:22 PM  czhower
  .Net

  Rev 1.7    2003.10.01 5:05:16 PM  czhower
  .Net

  Rev 1.6    2003.10.01 2:30:42 PM  czhower
  .Net

  Rev 1.3    10/1/2003 12:14:16 AM  BGooijen
  DotNet: removing CheckForSocketError

  Rev 1.2    2003.10.01 1:12:38 AM  czhower
  .Net

  Rev 1.1    2003.09.30 1:25:02 PM  czhower
  Added .inc file.

  Rev 1.0    2003.09.30 1:24:20 PM  czhower
  Initial Checkin

  Rev 1.10    2003.09.30 10:36:02 AM  czhower
  Moved stack creation to IdStack
  Added DotNet stack.

  Rev 1.9    9/8/2003 02:13:14 PM  JPMugaas
  SupportsIP6 function added for determining if IPv6 is installed on a system.

  Rev 1.8    2003.07.17 4:57:04 PM  czhower
  Added new exception type so it can be added to debugger list of ignored
  exceptions.

  Rev 1.7    2003.07.14 11:46:46 PM  czhower
  IOCP now passes all bubbles.

  Rev 1.6    2003.07.14 1:57:24 PM  czhower
  -First set of IOCP fixes.
  -Fixed a threadsafe problem with the stack class.

  Rev 1.5    7/1/2003 05:20:38 PM  JPMugaas
  Minor optimizations.  Illiminated some unnecessary string operations.

  Rev 1.4    7/1/2003 03:39:54 PM  JPMugaas
  Started numeric IP function API calls for more efficiency.

  Rev 1.3    7/1/2003 12:46:08 AM  JPMugaas
  Preliminary stack functions taking an IP address numerical structure instead
  of a string.

  Rev 1.2    5/10/2003 4:02:22 PM  BGooijen

  Rev 1.1    2003.05.09 10:59:26 PM  czhower

  Rev 1.0    11/13/2002 08:59:02 AM  JPMugaas
}

unit IdStackBSDBase;

interface

{$I IdCompilerDefines.inc}

{$IFDEF DOTNET}
Improper compile.
This unit must NOT be linked into DotNet applications.
{$ENDIF}

uses
  Classes,
  IdException, IdStack, IdStackConsts, IdGlobal;

type
  // RLebeau - for use with the HostToNetwork() and NetworkToHost()
  // methods under Windows and Linux since the Socket API doesn't
  // have native conversion functions for int64 values...
  TIdInt64Parts = packed record
    case Integer of
    0: (
       {$IFDEF ENDIAN_BIG}
      HighPart: UInt32;
      LowPart: UInt32);
       {$ELSE}
      LowPart: UInt32;
      HighPart: UInt32);
      {$ENDIF}
    1: (
      QuadPart: Int64);
  end;
  TIdUInt64Parts = packed record
    case Integer of
    0: (
       {$IFDEF ENDIAN_BIG}
      HighPart: UInt32;
      LowPart: UInt32);
       {$ELSE}
      LowPart: UInt32;
      HighPart: UInt32);
      {$ENDIF}
    1: (
      QuadPart: UInt64);
  end;

  TIdIPv6AddressRec = packed array[0..7] of UInt16;

  TIdIPAddressRec = packed record
    IPVer: TIdIPVersion;
    case Integer of
    0: (IPv4, Junk1, Junk2, Junk3: UInt32);
    2: (IPv6 : TIdIPv6AddressRec);
  end;

//procedure EmptyIPRec(var VIP : TIdIPAddress);

  TIdSunB = packed record
    s_b1, s_b2, s_b3, s_b4: UInt8;
  end;

  TIdSunW = packed record
    s_w1, s_w2: UInt16;
  end;

  PIdIn4Addr = ^TIdIn4Addr;
  TIdIn4Addr = packed record
    case integer of
        0: (S_un_b: TIdSunB);
        1: (S_un_w: TIdSunW);
        2: (S_addr: UInt32);
  end;

  PIdIn6Addr = ^TIdIn6Addr;
  TIdIn6Addr = packed record
    case Integer of
    0: (s6_addr: packed array [0..16-1] of UInt8);
    1: (s6_addr16: packed array [0..8-1] of UInt16);
  end;
  (*$HPPEMIT '#ifdef s6_addr'*)
  (*$HPPEMIT '  #undef s6_addr'*)
  (*$HPPEMIT '#endif'*)
  (*$HPPEMIT '#ifdef s6_addr16'*)
  (*$HPPEMIT '  #undef s6_addr16'*)
  (*$HPPEMIT '#endif'*)

  PIdInAddr = ^TIdInAddr;
  TIdInAddr = {$IFDEF IPv6} TIdIn6Addr; {$ELSE} TIdIn4Addr; {$ENDIF}

  //Do not change these structures or insist on objects
  //because these are parameters to IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP
  TIdIPMreq = packed record
    IMRMultiAddr : TIdIn4Addr;   // IP multicast address of group */
    IMRInterface : TIdIn4Addr;   // local IP address of interface */
  end;
  TIdIPv6Mreq = packed record
    ipv6mr_multiaddr : TIdIn6Addr;  //IPv6 multicast addr
    ipv6mr_interface : UInt32;      //interface index
  end;

  TIdStackBSDBase = class(TIdStack)
  protected
    function WSCloseSocket(ASocket: TIdStackSocketHandle): Integer; virtual; abstract;
    function WSRecv(ASocket: TIdStackSocketHandle; var ABuffer;
     const ABufferLength, AFlags: Integer): Integer; virtual; abstract;
    function WSSend(ASocket: TIdStackSocketHandle; const ABuffer;
     const ABufferLength, AFlags: Integer): Integer; virtual; abstract;
    function WSShutdown(ASocket: TIdStackSocketHandle; AHow: Integer): Integer;
     virtual; abstract;
    {$IFNDEF VCL_XE3_OR_ABOVE}
    procedure WSGetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; var AOptVal; var AOptLen: Integer); virtual; abstract;
    procedure WSSetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; const AOptVal; const AOptLen: Integer); virtual; abstract;
    {$ENDIF}
     //internal for multicast membership stuff
    procedure MembershipSockOpt(AHandle: TIdStackSocketHandle;
      const AGroupIP, ALocalIP : String; const ASockOpt : Integer;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
  public
    constructor Create; override;
    function CheckIPVersionSupport(const AIPVersion: TIdIPVersion): boolean; virtual; abstract;
    function Receive(ASocket: TIdStackSocketHandle; var VBuffer: TIdBytes): Integer; override;
    function Send(ASocket: TIdStackSocketHandle; const ABuffer: TIdBytes;
      const AOffset: Integer = 0; const ASize: Integer = -1): Integer; override;
    function ReceiveFrom(ASocket: TIdStackSocketHandle; var VBuffer: TIdBytes;
      var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion): Integer; override;
    function SendTo(ASocket: TIdStackSocketHandle; const ABuffer: TIdBytes;
      const AOffset: Integer; const ASize: Integer; const AIP: string;
      const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): Integer; override;
    procedure GetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; out AOptVal: Integer); overload; override;
    procedure GetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; var AOptVal; var AOptLen: Integer); {$IFDEF VCL_XE3_OR_ABOVE}overload; virtual; abstract;{$ELSE}reintroduce; overload;{$ENDIF}
    procedure SetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; AOptVal: Integer); overload; override;
    procedure SetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; const AOptVal; const AOptLen: Integer); {$IFDEF VCL_XE3_OR_ABOVE}overload; virtual; abstract;{$ELSE}reintroduce; overload;{$ENDIF}
    function TranslateTInAddrToString(var AInAddr; const AIPVersion: TIdIPVersion): string;
    procedure TranslateStringToTInAddr(const AIP: string; var AInAddr; const AIPVersion: TIdIPVersion);
    function WSGetServByName(const AServiceName: string): TIdPort; virtual; abstract;
    function WSGetServByPort(const APortNumber: TIdPort): TStrings; virtual; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use AddServByPortToList()'{$ENDIF};{$ENDIF}
    procedure AddServByPortToList(const APortNumber: TIdPort; AAddresses: TStrings); virtual; abstract;
    function RecvFrom(const ASocket: TIdStackSocketHandle; var ABuffer;
      const ALength, AFlags: Integer; var VIP: string; var VPort: TIdPort;
      var VIPVersion: TIdIPVersion): Integer; virtual; abstract;
    procedure WSSendTo(ASocket: TIdStackSocketHandle; const ABuffer;
      const ABufferLength, AFlags: Integer; const AIP: string; const APort: TIdPort;
      AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); virtual; abstract;
    function WSSocket(AFamily : Integer; AStruct : TIdSocketType; AProtocol: Integer;
      const AOverlapped: Boolean = False): TIdStackSocketHandle; virtual; abstract;
    procedure SetBlocking(ASocket: TIdStackSocketHandle;
     const ABlocking: Boolean); virtual; abstract;
    function WouldBlock(const AResult: Integer): Boolean; virtual; abstract;
    function NewSocketHandle(const ASocketType: TIdSocketType;
      const AProtocol: TIdSocketProtocol;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION;
      const AOverlapped: Boolean = False)
     : TIdStackSocketHandle; override;
    //multicast stuff Kudzu permitted me to add here.
    procedure SetMulticastTTL(AHandle: TIdStackSocketHandle;
      const AValue : Byte; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    procedure SetLoopBack(AHandle: TIdStackSocketHandle; const AValue: Boolean;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    procedure DropMulticastMembership(AHandle: TIdStackSocketHandle;
      const AGroupIP, ALocalIP : String;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    procedure AddMulticastMembership(AHandle: TIdStackSocketHandle;
      const AGroupIP, ALocalIP : String;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
  end;

  EIdInvalidServiceName = class(EIdException);
  EIdStackInitializationFailed = class (EIdStackError);
  EIdStackSetSizeExceeded = class (EIdStackError);


//for some reason, if GDBSDStack is in the same block as GServeFileProc then
//FPC gives a type declaration error.
var
  GBSDStack: TIdStackBSDBase = nil;

const
  IdIPFamily : array[TIdIPVersion] of Integer = (Id_PF_INET4, Id_PF_INET6);

implementation

uses
  //done this way so we can have a separate stack for the Unix systems in FPC
  {$IFDEF DOTNET}
  IdStackDotNet,
  {$ELSE}
    {$IFDEF WINDOWS}
  IdStackWindows,
    {$ELSE}
      {$IFDEF USE_VCL_POSIX}
  IdStackVCLPosix,
      {$ELSE}
        {$IFDEF UNIX}
          {$IFDEF KYLIXCOMPAT}
  IdStackLibc,
          {$ELSE}
            {$IFDEF USE_BASEUNIX}
  IdStackUnix,
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  IdResourceStrings,
  SysUtils;

{ TIdStackBSDBase }

function TIdStackBSDBase.TranslateTInAddrToString(var AInAddr;
  const AIPVersion: TIdIPVersion): string;
var
  i: Integer;
begin
  case AIPVersion of
    Id_IPv4: begin
      // TODO: use RtlIpv4AddressToString() on Windows when available...
      Result := IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b1) + '.'   {Do not Localize}
                + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b2) + '.' {Do not Localize}
                + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b3) + '.' {Do not Localize}
                + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b4);
    end;
    Id_IPv6: begin
      // TODO: use RtlIpv6AddressToString() on Windows when available...
      Result := '';
      for i := 0 to 7 do begin
        Result := Result + IntToHex(NetworkToHost(TIdIn6Addr(AInAddr).s6_addr16[i]), 1) + ':';
      end;
      SetLength(Result, Length(Result)-1);
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

procedure TIdStackBSDBase.TranslateStringToTInAddr(const AIP: string;
  var AInAddr; const AIPVersion: TIdIPVersion);
var
  LIP: String;
  LAddress: TIdIPv6Address;
begin
  case AIPVersion of
    Id_IPv4: begin
      // TODO: use RtlIpv4StringToAddress() on Windows when available...
      LIP := AIP;
      TIdIn4Addr(AInAddr).S_un_b.s_b1 := IndyStrToInt(Fetch(LIP, '.'));    {Do not Localize}
      TIdIn4Addr(AInAddr).S_un_b.s_b2 := IndyStrToInt(Fetch(LIP, '.'));    {Do not Localize}
      TIdIn4Addr(AInAddr).S_un_b.s_b3 := IndyStrToInt(Fetch(LIP, '.'));    {Do not Localize}
      TIdIn4Addr(AInAddr).S_un_b.s_b4 := IndyStrToInt(Fetch(LIP, '.'));    {Do not Localize}
    end;
    Id_IPv6: begin
      // TODO: use RtlIpv6StringToAddress() on Windows when available...
      IPv6ToIdIPv6Address(AIP, LAddress);
      TIdIPv6Address(TIdIn6Addr(AInAddr).s6_addr16) := HostToNetwork(LAddress);
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

function TIdStackBSDBase.NewSocketHandle(const ASocketType:TIdSocketType;
  const AProtocol: TIdSocketProtocol;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION;
  const AOverlapped: Boolean = False): TIdStackSocketHandle;
begin
  // RLebeau 04/17/2008: Don't use CheckForSocketError() here.  It expects
  // an Integer error code, not a TSocket handle.  When WSSocket() fails,
  // it returns Id_INVALID_SOCKET.  Although that is technically the same
  // value as Id_SOCKET_ERROR, passing Id_INVALID_SOCKET to CheckForSocketError()
  // causes a range check error to be raised.
  Result := WSSocket(IdIPFamily[AIPVersion], ASocketType, AProtocol, AOverlapped);
  if Result = Id_INVALID_SOCKET then begin
    RaiseLastSocketError;
  end;
end;

constructor TIdStackBSDBase.Create;
begin
  inherited Create;
  GBSDStack := Self;
end;

function TIdStackBSDBase.Receive(ASocket: TIdStackSocketHandle;
  var VBuffer: TIdBytes): Integer;
begin
  Result := CheckForSocketError(WSRecv(ASocket, VBuffer[0], Length(VBuffer) , 0));
end;

function TIdStackBSDBase.Send(ASocket: TIdStackSocketHandle; const ABuffer: TIdBytes;
  const AOffset: Integer = 0; const ASize: Integer = -1): Integer;
var
  Tmp: Byte;
begin
  Result := IndyLength(ABuffer, ASize, AOffset);
  if Result > 0 then begin
    Result := WSSend(ASocket, ABuffer[AOffset], Result, 0);
  end else begin
    // RLebeau: this is to allow UDP sockets to send 0-length packets.
    // Have to use a variable because the Buffer parameter is declared
    // as an untyped 'const'...
    //
    // TODO: check the socket type and only allow this for UDP sockets...
    //
    Result := WSSend(ASocket, Tmp, 0, 0);
  end;
end;

function TIdStackBSDBase.ReceiveFrom(ASocket: TIdStackSocketHandle;
  var VBuffer: TIdBytes; var VIP: string; var VPort: TIdPort;
  var VIPVersion: TIdIPVersion): Integer;
begin
   Result := CheckForSocketError(RecvFrom(ASocket, VBuffer[0], Length(VBuffer),
     0, VIP, VPort, VIPVersion));
end;

function TIdStackBSDBase.SendTo(ASocket: TIdStackSocketHandle;
  const ABuffer: TIdBytes; const AOffset: Integer; const ASize: Integer;
  const AIP: string; const APort: TIdPort;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): Integer;
var
  Tmp: Byte;
begin
  Result := IndyLength(ABuffer, ASize, AOffset);
  if Result > 0 then begin
    WSSendTo(ASocket, ABuffer[AOffset], Result, 0, AIP, APort, AIPVersion);
  end else begin
    // RLebeau: this is to allow UDP sockets to send 0-length packets.
    // Have to use a variable because the Buffer parameter is declared
    // as an untyped 'const'...
    //
    // TODO: check the socket type and only allow this for UDP sockets...
    //
    WSSendTo(ASocket, Tmp, 0, 0, AIP, APort, AIPVersion);
  end;
end;

procedure TIdStackBSDBase.GetSocketOption(ASocket: TIdStackSocketHandle;
  ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption; out AOptVal: Integer);
var
  LBuf, LLen: Integer;
begin
  LLen := SizeOf(LBuf);
  {$IFDEF VCL_XE3_OR_ABOVE}GetSocketOption{$ELSE}WSGetSocketOption{$ENDIF}(ASocket, ALevel, AOptName, LBuf, LLen);
  AOptVal := LBuf;
end;

{$IFNDEF VCL_XE3_OR_ABOVE}
procedure TIdStackBSDBase.GetSocketOption(ASocket: TIdStackSocketHandle;
  ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption; var AOptVal;
  var AOptLen: Integer);
begin
  WSGetSocketOption(ASocket, ALevel, AOptName, AOptVal, AOptLen);
end;
{$ENDIF}

procedure TIdStackBSDBase.SetSocketOption(ASocket: TIdStackSocketHandle;
  ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption; AOptVal: Integer);
begin
  {$IFDEF VCL_XE3_OR_ABOVE}SetSocketOption{$ELSE}WSSetSocketOption{$ENDIF}(ASocket, ALevel, AOptName, AOptVal, SizeOf(AOptVal));
end;

{$IFNDEF VCL_XE3_OR_ABOVE}
procedure TIdStackBSDBase.SetSocketOption(ASocket: TIdStackSocketHandle;
  ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption; const AOptVal;
  const AOptLen: Integer);
begin
  WSSetSocketOption(ASocket, ALevel, AOptName, AOptVal, AOptLen);
end;
{$ENDIF}

procedure TIdStackBSDBase.DropMulticastMembership(AHandle: TIdStackSocketHandle;
  const AGroupIP, ALocalIP : String; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
begin
  MembershipSockOpt(AHandle, AGroupIP, ALocalIP,
    iif(AIPVersion = Id_IPv4, Id_IP_DROP_MEMBERSHIP, Id_IPV6_DROP_MEMBERSHIP),
    AIPVersion);
end;

procedure TIdStackBSDBase.AddMulticastMembership(AHandle: TIdStackSocketHandle;
  const AGroupIP, ALocalIP : String; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
begin
  MembershipSockOpt(AHandle, AGroupIP, ALocalIP,
    iif(AIPVersion = Id_IPv4, Id_IP_ADD_MEMBERSHIP, Id_IPV6_ADD_MEMBERSHIP),
    AIPVersion);
end;

procedure TIdStackBSDBase.SetMulticastTTL(AHandle: TIdStackSocketHandle;
  const AValue: Byte; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LLevel, LOpt: Integer;
begin
  case AIPVersion of
    Id_IPv4: begin
      LLevel := Id_IPPROTO_IP;
      LOpt := Id_IP_MULTICAST_TTL;
    end;
    id_IPv6: begin
      LLevel := Id_IPPROTO_IPv6;
      LOpt := Id_IPV6_MULTICAST_HOPS;
    end;
    else begin
      // keep the compiler happy
      LLevel := 0;
      LOpt := 0;
      IPVersionUnsupported;
    end;
  end;
  SetSocketOption(AHandle, LLevel, LOpt, AValue);
end;

procedure TIdStackBSDBase.SetLoopBack(AHandle: TIdStackSocketHandle;
  const AValue: Boolean; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LLevel, LOpt: Integer;
begin
  case AIPVersion of
    Id_IPv4: begin
      LLevel := Id_IPPROTO_IP;
      LOpt := Id_IP_MULTICAST_LOOP;
    end;
    Id_IPv6: begin
      LLevel := Id_IPPROTO_IPv6;
      LOpt := Id_IPV6_MULTICAST_LOOP;
    end;
    else begin
      // keep the compiler happy
      LLevel := 0;
      LOpt := 0;
      IPVersionUnsupported;
    end;
  end;
  SetSocketOption(AHandle, LLevel, LOpt, Ord(AValue));
end;

procedure TIdStackBSDBase.MembershipSockOpt(AHandle: TIdStackSocketHandle;
  const AGroupIP, ALocalIP: String; const ASockOpt: Integer;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LIP4: TIdIPMreq;
  LIP6: TIdIPv6Mreq;
begin
  case AIPVersion of
    Id_IPv4: begin
      if IsValidIPv4MulticastGroup(AGroupIP) then
      begin
        TranslateStringToTInAddr(AGroupIP, LIP4.IMRMultiAddr, Id_IPv4);
        TranslateStringToTInAddr(ALocalIP, LIP4.IMRInterface, Id_IPv4);
        SetSocketOption(AHandle, Id_IPPROTO_IP, ASockOpt, LIP4, SizeOf(LIP4));
      end;
    end;
    Id_IPv6: begin
      if IsValidIPv6MulticastGroup(AGroupIP) then
      begin
        TranslateStringToTInAddr(AGroupIP, LIP6.ipv6mr_multiaddr, Id_IPv6);
        //this should be safe meaning any adaptor
        //we can't support a localhost address in IPv6 because we can't get that
        //and even if you could, you would have to convert it into a network adaptor
        //index - Yuk
        LIP6.ipv6mr_interface := 0;
        SetSocketOption(AHandle, Id_IPPROTO_IPv6, ASockOpt, LIP6, SizeOf(LIP6));
      end;
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdStackBSDBase.WSGetServByPort(const APortNumber: TIdPort): TStrings;
{$I IdDeprecatedImplBugOn.inc}
begin
  Result := TStringList.Create;
  try
    AddServByPortToList(APortNumber, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.

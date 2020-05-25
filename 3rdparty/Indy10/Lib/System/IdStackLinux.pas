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
  Rev 1.7    10/26/2004 8:20:04 PM  JPMugaas
  Fixed some oversights with conversion.  OOPS!!!

  Rev 1.6    10/26/2004 8:12:32 PM  JPMugaas
  Now uses TIdStrings and TIdStringList for portability.

  Rev 1.5    12/06/2004 15:17:20  CCostelloe
  Restructured to correspond with IdStackWindows, now works.

  Rev 1.4    07/06/2004 21:31:02  CCostelloe
  Kylix 3 changes

  Rev 1.3    4/18/04 10:43:22 PM  RLebeau
  Fixed syntax error

  Rev 1.2    4/18/04 10:29:46 PM  RLebeau
  Renamed Int64Parts structure to TIdInt64Parts

  Rev 1.1    4/18/04 2:47:28 PM  RLebeau
  Conversion support for Int64 values

  Removed WSHToNs(), WSNToHs(), WSHToNL(), and WSNToHL() methods, obsolete

  Rev 1.0    2004.02.03 3:14:48 PM  czhower
  Move and updates

  Rev 1.3    10/19/2003 5:35:14 PM  BGooijen
  SetSocketOption

  Rev 1.2    2003.10.01 9:11:24 PM  czhower
  .Net

  Rev 1.1    7/5/2003 07:25:50 PM  JPMugaas
  Added functions to the Linux stack which use the new TIdIPAddress record type
  for IP address parameters.  I also fixed a compile bug.

  Rev 1.0    11/13/2002 08:59:24 AM  JPMugaas
}

unit IdStackLinux;

interface

{$i IdCompilerDefines.inc}

uses
  Classes,
  Libc,
  IdStack,
  IdStackConsts,
  IdGlobal,
  IdStackBSDBase;

type

  TIdSocketListLinux = class (TIdSocketList)
  protected
    FCount: integer;
    FFDSet: TFDSet;
    //
    class function FDSelect(AReadSet: PFDSet; AWriteSet: PFDSet; AExceptSet: PFDSet;
     const ATimeout: Integer = IdTimeoutInfinite): integer;
    function GetItem(AIndex: Integer): TIdStackSocketHandle; override;
  public
    procedure Add(AHandle: TIdStackSocketHandle); override;
    procedure Remove(AHandle: TIdStackSocketHandle); override;
    function Count: Integer; override;
    procedure Clear; override;
    function Clone: TIdSocketList; override;
    function ContainsSocket(AHandle: TIdStackSocketHandle): boolean; override;
    procedure GetFDSet(var VSet: TFDSet);
    procedure SetFDSet(var VSet: TFDSet);
    class function Select(AReadList: TIdSocketList; AWriteList: TIdSocketList;
      AExceptList: TIdSocketList; const ATimeout: Integer = IdTimeoutInfinite): Boolean; override;
    function SelectRead(const ATimeout: Integer = IdTimeoutInfinite): Boolean;
      override;
    function SelectReadList(var VSocketList: TIdSocketList;
      const ATimeout: Integer = IdTimeoutInfinite): Boolean; override;
  End;//TIdSocketList

  TIdStackLinux = class(TIdStackBSDBase)
  private
    procedure WriteChecksumIPv6(s: TIdStackSocketHandle;
      var VBuffer: TIdBytes; const AOffset: Integer; const AIP: String;
      const APort: TIdPort);
  protected
    function GetLastError : Integer;
    procedure SetLastError(Const AError : Integer);
    function HostByName(const AHostName: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string; override;
    function ReadHostName: string; override;
    function WSCloseSocket(ASocket: TIdStackSocketHandle): Integer; override;
    function WSRecv(ASocket: TIdStackSocketHandle;
      var ABuffer; const ABufferLength, AFlags: Integer): Integer; override;
    function WSSend(ASocket: TIdStackSocketHandle; const ABuffer; const ABufferLength, AFlags: Integer): Integer; override;
    function WSShutdown(ASocket: TIdStackSocketHandle; AHow: Integer): Integer; override;
    {$IFNDEF VCL_XE3_OR_ABOVE}
    procedure WSGetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; var AOptVal; var AOptLen: Integer); override;
    procedure WSSetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; const AOptVal; const AOptLen: Integer); override;
    {$ENDIF}
  public
    procedure SetBlocking(ASocket: TIdStackSocketHandle;
        const ABlocking: Boolean); override;
    function WouldBlock(const AResult: Integer): Boolean; override;
    function WSTranslateSocketErrorMsg(const AErr: Integer): string; override;
    function Accept(ASocket: TIdStackSocketHandle; var VIP: string; var VPort: TIdPort;
      var VIPVersion: TIdIPVersion): TIdStackSocketHandle; override;
    procedure Bind(ASocket: TIdStackSocketHandle; const AIP: string;
     const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    procedure Connect(const ASocket: TIdStackSocketHandle; const AIP: string;
     const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function HostByAddress(const AAddress: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string; override;
    function WSGetLastError: Integer; override;
    procedure WSSetLastError(const AErr : Integer);  override;
    function WSGetServByName(const AServiceName: string): TIdPort; override;
    function WSGetServByPort(const APortNumber: TIdPort): TStrings; override;
    procedure GetPeerName(ASocket: TIdStackSocketHandle; var VIP: string;
     var VPort: TIdPort; var VIPVersion: TIdIPVersion); override;
    procedure GetSocketName(ASocket: TIdStackSocketHandle; var VIP: string;
     var VPort: TIdPort; var VIPVersion: TIdIPVersion); override;
    procedure Listen(ASocket: TIdStackSocketHandle; ABackLog: Integer); override;
    function HostToNetwork(AValue: UInt16): UInt16; override;
    function NetworkToHost(AValue: UInt16): UInt16; override;
    function HostToNetwork(AValue: UInt32): UInt32; override;
    function NetworkToHost(AValue: UInt32): UInt32; override;
    function HostToNetwork(AValue: TIdUInt64): TIdUInt64; override;
    function NetworkToHost(AValue: TIdUInt64): TIdUInt64; override;
    function RecvFrom(const ASocket: TIdStackSocketHandle; var VBuffer;
      const ALength, AFlags: Integer; var VIP: string; var VPort: TIdPort;
      var VIPVersion: TIdIPVersion): Integer; override;
    function ReceiveMsg(ASocket: TIdStackSocketHandle; var VBuffer: TIdBytes;
       APkt: TIdPacketInfo): UInt32; override;
    procedure WSSendTo(ASocket: TIdStackSocketHandle; const ABuffer;
      const ABufferLength, AFlags: Integer;
      const AIP: string; const APort: TIdPort; AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function WSSocket(AFamily : Integer; AStruct : TIdSocketType; AProtocol: Integer;
      const AOverlapped: Boolean = False): TIdStackSocketHandle; override;
    procedure Disconnect(ASocket: TIdStackSocketHandle); override;
    {$IFDEF VCL_XE3_OR_ABOVE}
    procedure GetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; var AOptVal; var AOptLen: Integer); override;
    procedure SetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; const AOptVal; const AOptLen: Integer); override;
    {$ENDIF}
    function SupportsIPv4: Boolean; overload; override;
    function SupportsIPv6: Boolean; overload; override;
    function CheckIPVersionSupport(const AIPVersion: TIdIPVersion): boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    //In Windows, this writes a checksum into a buffer.  In Linux, it would probably
    //simply have the kernal write the checksum with something like this (RFC 2292):
//
//    int  offset = 2;
//    setsockopt(fd, IPPROTO_IPV6, IPV6_CHECKSUM, &offset, sizeof(offset));
//
//  Note that this should be called
    //IMMEDIATELY before you do a SendTo because the Local IPv6 address might change

    procedure WriteChecksum(s : TIdStackSocketHandle;
      var VBuffer : TIdBytes;
      const AOffset : Integer;
      const AIP : String;
      const APort : TIdPort;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function IOControl(const s: TIdStackSocketHandle; const cmd: UInt32;
      var arg: UInt32): Integer; override;

    procedure GetLocalAddressList(AAddresses: TIdStackLocalAddressList); override;
  end;

  TLinger = record
    l_onoff: UInt16;
    l_linger: UInt16;
  end;
  TIdLinger = TLinger;

implementation

uses
  IdResourceStrings,
  IdResourceStringsKylixCompat,
  IdResourceStringsUnix,
  IdException,
  SysUtils;

type
  psockaddr_in6 = ^sockaddr_in6;

const
  Id_MSG_NOSIGNAL = MSG_NOSIGNAL;
  Id_WSAEPIPE = EPIPE;

constructor TIdStackLinux.Create;
begin
  inherited Create;
end;

destructor TIdStackLinux.Destroy;
begin
  inherited Destroy;
end;

function TIdStackLinux.GetLastError : Integer;
begin
  Result := errno;
end;

procedure TIdStackLinux.SetLastError(Const AError : Integer);
begin
  __errno_location^ := AError;
end;

procedure TIdStackLinux.WSSetLastError(const AErr : Integer);
begin
  SetLastError(AErr);
end;

function TIdStackLinux.Accept(ASocket: TIdStackSocketHandle;
  var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion): TIdStackSocketHandle;
var
  LN: UInt32;
  LAddr: sockaddr_in6;
begin
  LN := SizeOf(LAddr);
  Result := Libc.accept(ASocket, PSockAddr(@LAddr), @LN);
  if Result <> SOCKET_ERROR then begin
    case LAddr.sin6_family of
      Id_PF_INET4: begin
        with Psockaddr(@LAddr)^ do
        begin
          VIP := TranslateTInAddrToString(sin_addr, Id_IPv4);
          VPort := Ntohs(sin_port);
        end;
        VIPVersion := Id_IPV4;
      end;
      Id_PF_INET6: begin
        with LAddr do begin
          VIP := TranslateTInAddrToString(sin6_addr, Id_IPv6);
          VPort := ntohs(sin6_port);
        end;
        VIPVersion := Id_IPV6;
      end;
      else begin
        Libc.__close(Result);
        Result := Id_INVALID_SOCKET;
        IPVersionUnsupported;
      end;
    end;
  end else begin
    if GetLastError = EBADF then begin
      SetLastError(EINTR);
    end;
  end;
end;

procedure TIdStackLinux.Bind(ASocket: TIdStackSocketHandle;
  const AIP: string; const APort: TIdPort;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LAddr: sockaddr_in6;
  LSize: UInt32;
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  case AIPVersion of
    Id_IPv4: begin
        with Psockaddr(@LAddr)^ do begin
          sin_family := Id_PF_INET4;
          if AIP <> '' then begin
            TranslateStringToTInAddr(AIP, sin_addr, Id_IPv4);
          end;
          sin_port := htons(APort);
        end;
        LSize := SizeOf(sockaddr);
      end;
    Id_IPv6: begin
        with LAddr do
        begin
          sin6_family := Id_PF_INET6;
          if AIP <> '' then begin
            TranslateStringToTInAddr(AIP, sin6_addr, Id_IPv6);
          end;
          sin6_port := htons(APort);
        end;
        LSize := SizeOf(sockaddr_in6);
      end;
    else begin
      LSize := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;
  CheckForSocketError(Libc.bind(ASocket, Psockaddr(@LAddr), LSize);
end;

function TIdStackLinux.WSCloseSocket(ASocket: TIdStackSocketHandle): Integer;
begin
  Result := Libc.__close(ASocket);
end;

procedure TIdStackLinux.Connect(const ASocket: TIdStackSocketHandle;
 const AIP: string; const APort: TIdPort;
 const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LAddr: sockaddr_in6;
  LSize: UInt32;
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  case AIPVersion of
    Id_IPv4: begin
      with Psockaddr(@LAddr)^ do begin
        sin_family := Id_PF_INET4;
        TranslateStringToTInAddr(AIP, sin_addr, Id_IPv4);
        sin_port := htons(APort);
      end;
      LSize := SizeOf(sockaddr);
    end;
    Id_IPv6: begin
      with LAddr do begin
        sin6_family := Id_PF_INET6;
        TranslateStringToTInAddr(AIP, sin6_addr, Id_IPv6);
        sin6_port := htons(APort);
      end;
      LSize := SizeOf(sockaddr_in6);
    end;
    else begin
      LSize := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;
  CheckForSocketError(Libc.connect(
    ASocket,
    {$IFDEF KYLIX}Psockaddr(@LAddr)^{$ELSE}Psockaddr(@LAddr){$ENDIF},
    LSize));
end;

function TIdStackLinux.HostByName(const AHostName: string;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string;
var
  Lpa: PAnsiChar;
  Lsa: TInAddr;
  LHost: PHostEnt;
// ipv6
  LHints: TAddressInfo;
  {$IFDEF KYLIX}
  LAddrInfo: PAddressInfo;
  {$ELSE}
  LAddrInfo: PAddrInfo;
  {$ENDIF}
  LRetVal: Integer;
  {$IFDEF STRING_IS_UNICODE}
  LAStr: AnsiString;
  {$ENDIF}
begin
  case AIPVersion of
    Id_IPv4: begin
      {$IFDEF STRING_IS_UNICODE}
      LAStr := AnsiString(AHostName); // explicit convert to Ansi
      {$ENDIF}
      // TODO: use getaddrinfo() instead for IPv4 as well...
      LHost := Libc.gethostbyname(
        PAnsiChar({$IFDEF STRING_IS_UNICODE}LAStr{$ELSE}AHostName{$ENDIF}));
      if LHost <> nil then begin
        // TODO: gethostbyname() might return other things besides IPv4
        // addresses, so we should be validating the address type before
        // attempting the conversion...
        Lpa := LHost^.h_addr_list^;
        Lsa.S_un_b.s_b1 := Ord(Lpa[0]);
        Lsa.S_un_b.s_b2 := Ord(Lpa[1]);
        Lsa.S_un_b.s_b3 := Ord(Lpa[2]);
        Lsa.S_un_b.s_b4 := Ord(Lpa[3]);
        Result := TranslateTInAddrToString(Lsa, Id_IPv4);
      end else begin
        //RaiseSocketError(h_errno);
        RaiseLastSocketError;
      end;
    end;
    Id_IPv6: begin
      FillChar(LHints, SizeOf(LHints), 0);
      LHints.ai_family := IdIPFamily[AIPVersion];
      LHints.ai_socktype := Integer(SOCK_STREAM);
      LAddrInfo := nil;

      {$IFDEF STRING_IS_UNICODE}
      LAStr := AnsiString(AHostName); // explicit convert to Ansi
      {$ENDIF}
      LRetVal := getaddrinfo(
        PAnsiChar({$IFDEF STRING_IS_UNICODE}LAStr{$ELSE}AHostName{$ENDIF}),
        nil, @LHints, {$IFDEF KYLIX}LAddrInfo{$ELSE}@LAddrInfo{$ENDIF});
      if LRetVal <> 0 then begin
        if LRetVal = EAI_SYSTEM then begin
          IndyRaiseLastError;
        end else begin
          raise EIdResolveError.CreateFmt(RSResolveError, [AHostName, gai_strerror(LRetVal), LRetVal]);
        end;
      end;
      try
        Result := TranslateTInAddrToString(LAddrInfo^.ai_addr^.sin_zero, Id_IPv6);
      finally
        freeaddrinfo(LAddrInfo);
      end;
    end;
    else
      Result := ''; // avoid warning
      IPVersionUnsupported;
  end;
end;

function TIdStackLinux.ReadHostName: string;
var
  LStr: AnsiString;
begin
  SetLength(LStr, 250);
  Libc.gethostname(PAnsiChar(LStr), 250);
  Result := PAnsiChar(LStr);
end;

procedure TIdStackLinux.Listen(ASocket: TIdStackSocketHandle; ABackLog: Integer);
begin
  CheckForSocketError(Libc.listen(ASocket, ABacklog));
end;

function TIdStackLinux.WSRecv(ASocket: TIdStackSocketHandle; var ABuffer;
  const ABufferLength, AFlags: Integer): Integer;
begin
  //IdStackWindows is just: Result := Recv(ASocket, ABuffer, ABufferLength, AFlags);
  Result := Recv(ASocket, ABuffer, ABufferLength, AFlags or Id_MSG_NOSIGNAL);
end;

function TIdStackLinux.RecvFrom(const ASocket: TIdStackSocketHandle;
  var VBuffer; const ALength, AFlags: Integer; var VIP: string;
  var VPort: TIdPort; var VIPVersion: TIdIPVersion): Integer;
var
  LiSize: UInt32;
  LAddr: sockaddr_in6;
begin
  LiSize := SizeOf(sockaddr_in6);
  Result := Libc.recvfrom(ASocket, VBuffer, ALength, AFlags or Id_MSG_NOSIGNAL, PSockAddr(@LAddr), @LiSize);
  if Result >= 0 then
  begin
    case LAddr.sin6_family of
      Id_PF_INET4: begin
        with Psockaddr(@LAddr)^ do begin
          VIP := TranslateTInAddrToString(sin_addr, Id_IPv4);
          VPort := Ntohs(sin_port);
        end;
        VIPVersion := Id_IPV4;
      end;
      Id_PF_INET6: begin
        with LAddr do begin
          VIP := TranslateTInAddrToString(sin6_addr, Id_IPv6);
          VPort := ntohs(sin6_port);
        end;
        VIPVersion := Id_IPV6;
      end;
      else begin
        Result := 0;
        IPVersionUnsupported;
      end;
    end;
  end;
end;

function TIdStackLinux.ReceiveMsg(ASocket: TIdStackSocketHandle;
  var VBuffer: TIdBytes; APkt: TIdPacketInfo): UInt32;
{var
  LIP : String;
  LPort : TIdPort;
  LSize: UInt32;
  LAddr: SockAddr_In6;
  LMsg : msghdr;
  LMsgBuf : BUF;
  LControl : TIdBytes;
  LCurCmsg : CMSGHDR;   //for iterating through the control buffer
  LByte : PByte;
  LDummy, LDummy2 : UInt32;
      
begin
  //we call the macro twice because we specified two possible structures.
  //Id_IPV6_HOPLIMIT and Id_IPV6_PKTINFO
  LSize := CMSG_LEN(CMSG_LEN(Length(VBuffer)));
  SetLength( LControl,LSize);

  LMsgBuf.len := Length(VBuffer); // Length(VMsgData);
  LMsgBuf.buf := @VBuffer[0]; // @VMsgData[0];

  FillChar(LMsg,SizeOf(LMsg),0);

  LMsg.lpBuffers := @LMsgBuf;
  LMsg.dwBufferCount := 1;

  LMsg.Control.Len := LSize;
  LMsg.Control.buf := @LControl[0];

  LMsg.name := PSOCKADDR(@LAddr);
  LMsg.namelen := SizeOf(LAddr);

  CheckForSocketError(RecvMsg(ASocket, @LMsg, Result, @LDummy, LPwsaoverlapped_COMPLETION_ROUTINE(@LDummy2)));
  APkt.Reset;

  case LAddr.sin6_family of
    Id_PF_INET4: begin
      with Psockaddr(@LAddr)^ do
      begin
        APkt.SourceIP := TranslateTInAddrToString(sin_addr, Id_IPv4);
        APkt.SourcePort := ntohs(sin_port);
      end;
      APkt.SourceIPVersion := Id_IPv4;
    end;
    Id_PF_INET6: begin
      with LAddr do
      begin
        APkt.SourceIP := TranslateTInAddrToString(sin6_addr, Id_IPv6);
        APkt.SourcePort := ntohs(sin6_port);
      end;
      APkt.SourceIPVersion := Id_IPv6;
    end;
    else begin
      Result := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;

  LCurCmsg := nil;
  repeat
    LCurCmsg := CMSG_NXTHDR(@LMsg,LCurCmsg);
    if LCurCmsg=nil then
    begin
      break;
    end;
    case LCurCmsg^.cmsg_type of
      IP_PKTINFO :     //done this way because IPV6_PKTINF and  IP_PKTINFO
      //are both 19
      begin
        case LAddr.sin6_family of
          Id_PF_INET4: begin
            with Pin_pktinfo(WSA_CMSG_DATA(LCurCmsg))^ do
            begin
              APkt.DestIP := GWindowsStack.TranslateTInAddrToString(ipi_addr, Id_IPv4);
              APkt.DestIF := ipi_ifindex;
            end;
            APkt.DestIPVersion := Id_IPv4;
          end;
          Id_PF_INET6: begin
            with Pin6_pktinfo(WSA_CMSG_DATA(LCurCmsg))^ do
            begin
              APkt.DestIP := GWindowsStack.TranslateTInAddrToString(ipi6_addr, Id_IPv6);
              APkt.DestIF := ipi6_ifindex;
            end;
            APkt.DestIPVersion := Id_IPv6;
          end;
        end;
      end;
      Id_IPV6_HOPLIMIT :
      begin
        LByte :=  PByte(WSA_CMSG_DATA(LCurCmsg));
        APkt.TTL := LByte^;
      end;
    end;
  until False; }
begin
  APkt.Reset;
  Result := 0; // avoid warning
end;

function TIdStackLinux.WSSend(ASocket: TIdStackSocketHandle;
  const ABuffer; const ABufferLength, AFlags: Integer): Integer;
begin
  //CC: Should Id_MSG_NOSIGNAL be included?
  //  Result := Send(ASocket, ABuffer, ABufferLength, AFlags or Id_MSG_NOSIGNAL);
  Result := CheckForSocketError(Libc.send(ASocket, ABuffer, ABufferLength, AFlags));
end;

procedure TIdStackLinux.WSSendTo(ASocket: TIdStackSocketHandle;
  const ABuffer; const ABufferLength, AFlags: Integer; const AIP: string;
  const APort: TIdPort; AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LAddr: sockaddr_in6;
  LiSize: Integer;
begin
   FillChar(LAddr, SizeOf(LAddr), 0);
  case AIPVersion of
    Id_IPv4: begin
      with Psockaddr(@LAddr)^ do begin
        sin_family := Id_PF_INET4;
        TranslateStringToTInAddr(AIP, sin_addr, Id_IPv4);
        sin_port := htons(APort);
      end;
      LiSize := SizeOf(sockaddr);
    end;
    Id_IPv6: begin
      with LAddr do begin
        sin6_family := Id_PF_INET6;
        TranslateStringToTInAddr(AIP, sin6_addr, Id_IPv6);
        sin6_port := htons(APort);
      end;
      LiSize := SizeOf(sockaddr_in6);
    end;
    else begin
      LiSize := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;
  LiSize := Libc.sendto(
    ASocket, ABuffer, ABufferLength, AFlags or Id_MSG_NOSIGNAL,
    {$IFDEF KYLIX}Psockaddr(@LAddr)^{$ELSE}Psockaddr(@LAddr){$ENDIF},
    LiSize);
  end;
  if LiSize = Id_SOCKET_ERROR then begin
    // TODO: move this into RaiseLastSocketError directly
    if WSGetLastError() = Id_WSAEMSGSIZE then begin
      raise EIdPackageSizeTooBig.Create(RSPackageSizeTooBig);
    end else begin
      RaiseLastSocketError;
    end;
  end
  else if LiSize <> ABufferLength then begin
    raise EIdNotAllBytesSent.Create(RSNotAllBytesSent);
  end;
end;

procedure TIdStackLinux.{$IFDEF VCL_XE3_OR_ABOVE}GetSocketOption{$ELSE}WSGetSocketOption{$ENDIF}
  (ASocket: TIdStackSocketHandle; ALevel: TIdSocketProtocol; AOptName: TIdSocketOption;
  var AOptVal; var AOptLen: Integer);
var
  LLen: UInt32;
begin
  LLen := AOptLen;
  CheckForSocketError(Libc.getsockopt(ASocket, ALevel, AOptName, PAnsiChar(@AOptVal), LLen));
  AOptLen := LLen;
end;

procedure TIdStackLinux.{$IFDEF VCL_XE3_OR_ABOVE}SetSocketOption{$ELSE}WSSetSocketOption{$ENDIF}
  (ASocket: TIdStackSocketHandle; ALevel: TIdSocketProtocol; AOptName: TIdSocketOption;
  const AOptVal; const AOptLen: Integer);
begin
  CheckForSocketError(Libc.setsockopt(ASocket, ALevel, AOptName, PAnsiChar(@AOptVal), AOptLen));
end;

function TIdStackLinux.WSGetLastError: Integer;
begin
  //IdStackWindows just uses   result := WSAGetLastError;
  Result := GetLastError; //System.GetLastOSError; - FPC doesn't define it in System
  if Result = Id_WSAEPIPE then begin
    Result := Id_WSAECONNRESET;
  end;
end;

function TIdStackLinux.WSSocket(AFamily : Integer; AStruct : TIdSocketType; AProtocol: Integer;
      const AOverlapped: Boolean = False): TIdStackSocketHandle;
begin
  // TODO: enable this?
  {
  if AOverlapped then begin
    Result := Libc.socket(AFamily, AStruct or SOCK_NONBLOCK, AProtocol);
  end else begin
  }
    Result := Libc.socket(AFamily, AStruct, AProtocol);
  //end;
end;

function TIdStackLinux.WSGetServByName(const AServiceName: string): TIdPort;
var
  Lps: PServEnt;
  {$IFDEF STRING_IS_UNICODE}
  LAStr: AnsiString;
  {$ENDIF}
begin
  {$IFDEF STRING_IS_UNICODE}
  LAStr := AnsiString(AServiceName); // explicit convert to Ansi
  {$ENDIF}
  Lps := Libc.getservbyname(
    PAnsiChar({$IFDEF STRING_IS_UNICODE}LAStr{$ELSE}AServiceName{$ENDIF},
    nil);
  if Lps <> nil then begin
    Result := ntohs(Lps^.s_port);
  end else begin
    try
      Result := IndyStrToInt(AServiceName);
    except
      on EConvertError do begin
        IndyRaiseOuterException(EIdInvalidServiceName.CreateFmt(RSInvalidServiceName, [AServiceName]));
      end;
    end;
  end;
end;

function TIdStackLinux.WSGetServByPort(const APortNumber: TIdPort): TStrings;
type
  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = packed array[0..(Maxint div SizeOf(PAnsiChar))-1] of PAnsiChar;
var
  Lps: PServEnt;
  Li: Integer;
  Lp: PPAnsiCharArray;
begin
  Result := TStringList.Create;
  Lp := nil;
  try
    Lps := Libc.getservbyport(htons(APortNumber), nil);
    if Lps <> nil then begin
      Result.Add(Lps^.s_name);
      Li := 0;
      Lp := Pointer(Lps^.s_aliases);
      while Lp[Li] <> nil do begin
        Result.Add(Lp[Li]);
        Inc(Li);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TIdStackLinux.HostToNetwork(AValue: UInt16): UInt16;
begin
  Result := htons(AValue);
end;

function TIdStackLinux.NetworkToHost(AValue: UInt16): UInt16;
begin
  Result := ntohs(AValue);
end;

function TIdStackLinux.HostToNetwork(AValue: UInt32): UInt32;
begin
  Result := htonl(AValue);
end;

function TIdStackLinux.NetworkToHost(AValue: UInt32): UInt32;
begin
  Result := ntohl(AValue);
end;

{ RP - I'm not sure what endian Linux natively uses, thus the
check to see if the bytes need swapping or not ... }
function TIdStackLinux.HostToNetwork(AValue: TIdUInt64): TIdUInt64;
var
  LParts: TIdUInt64Parts;
  L: UInt32;
begin
  if (htonl(1) <> 1) then begin
    LParts.QuadPart := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
    L := htonl(LParts.HighPart);
    LParts.HighPart := htonl(LParts.LowPart);
    LParts.LowPart := L;
    Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := LParts.QuadPart;
  end else begin
    Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
  end;
end;

function TIdStackLinux.NetworkToHost(AValue: TIdUInt64): TIdUInt64;
var
  LParts: TIdUInt64Parts;
  L: UInt32;
begin
  if (ntohl(1) <> 1) then begin
    LParts.QuadPart := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
    L := ntohl(LParts.HighPart);
    LParts.HighPart := ntohl(LParts.LowPart);
    LParts.LowPart := L;
    Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := LParts.QuadPart;
  end else begin
    Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
  end;
end;

procedure TIdStackLinux.GetLocalAddressList(AAddresses: TIdStackLocalAddressList);
{$IFNDEF HAS_getifaddrs}
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
  TaPIn6Addr = array[0..250] of PIn6Addr;
  PaPIn6Addr = ^TaPIn6Addr;
{$ENDIF}
var
  {$IFDEF HAS_getifaddrs}
  LAddrList, LAddrInfo: pifaddrs;
  LSubNetStr: string;
  {$ELSE}
  Li: Integer;
  LAHost: PHostEnt;
  LPAdrPtr: PaPInAddr;
  LHostName: AnsiString;
  {$ENDIF}
begin
  // TODO: Using gethostname() and gethostbyname() like this may not always
  // return just the machine's IP addresses. Technically speaking, they will
  // return the local hostname, and then return the address(es) to which that
  // hostname resolves. It is possible for a machine to (a) be configured such
  // that its name does not resolve to an IP, or (b) be configured such that
  // its name resolves to multiple IPs, only one of which belongs to the local
  // machine. For better results, we should use getifaddrs() on platforms that
  // support it...

  {$IFDEF HAS_getifaddrs}

  if getifaddrs(@LAddrList) = 0 then // TODO: raise an exception if it fails
  try
    AAddresses.BeginUpdate;
    try
      LAddrInfo := LAddrList;
      repeat
        if (LAddrInfo^.ifa_addr <> nil) and ((LAddrInfo^.ifa_flags and IFF_LOOPBACK) = 0) then
        begin
          case LAddrInfo^.ifa_addr^.sa_family of
            Id_PF_INET4: begin
              if LAddrInfo^.ifa_netmask <> nil then begin
                LSubNetStr := TranslateTInAddrToString(PSockAddr_In(LAddrInfo^.ifa_netmask)^.sin_addr, Id_IPv4);
              end else begin
                LSubNetStr := '';
              end;
              TIdStackLocalAddressIPv4.Create(AAddresses, TranslateTInAddrToString(PSockAddr_In(LAddrInfo^.ifa_addr)^.sin_addr, Id_IPv4), LSubNetStr);
            end;
            Id_PF_INET6: begin
              TIdStackLocalAddressIPv6.Create(AAddresses, TranslateTInAddrToString(PSockAddr_In6(LAddrInfo^.ifa_addr)^.sin6_addr, Id_IPv6));
            end;
          end;
        end;
        LAddrInfo := LAddrInfo^.ifa_next;
      until LAddrInfo = nil;
    finally
      AAddresses.EndUpdate;
    end;
  finally
    freeifaddrs(LAddrList);
  end;

  {$ELSE}

  // this won't get IPv6 addresses as I didn't find a way
  // to enumerate IPv6 addresses on a linux machine

  LHostName := HostName;
  LAHost := Libc.gethostbyname(PAnsiChar(LHostName));
  if LAHost = nil then begin
    RaiseLastSocketError;
  end;

  // gethostbyname() might return other things besides IPv4 addresses, so we
  // need to validate the address type before attempting the conversion...

  case LAHost^.h_addrtype of
    Id_PF_INET4: begin
      LPAdrPtr := PAPInAddr(LAHost^.h_addr_list);
      Li := 0;
      if LPAdrPtr^[Li] <> nil then begin
        AAddresses.BeginUpdate;
        try
          repeat
            TIdStackLocalAddressIPv4.Create(AAddresses, TranslateTInAddrToString(LPAdrPtr^[Li]^, Id_IPv4), ''); // TODO: SubNet
            Inc(Li);
          until LPAdrPtr^[Li] = nil;
        finally
          AAddresses.EndUpdate;
        end;
      end;
    end;
    Id_PF_INET6: begin
      LPAdr6Ptr := PAPIn6Addr(LAHost^.h_addr_list);
      Li := 0;
      if LPAdr6Ptr^[Li] <> nil then begin
        AAddresses.BeginUpdate;
        try
          repeat
            TIdStackLocalAddressIPv6.Create(AAddresses, TranslateTInAddrToString(LPAdr6Ptr^[Li]^, Id_IPv6));
            Inc(Li);
          until LPAdr6Ptr^[Li] = nil;
        finally
          AAddresses.EndUpdate;
        end;
      end;
    end;
  end;

  {$ENDIF}
end;

function TIdStackLinux.HostByAddress(const AAddress: string;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string;
var
  LAddr: sockaddr_in6;
  LSize: UInt32;
  LHostName : array[0..NI_MAXHOST] of TIdAnsiChar;
  {$IFDEF USE_MARSHALLED_PTRS}
  LHostNamePtr: TPtrWrapper;
  {$ENDIF}
  LRet : Integer;
  {$IFDEF KYLIX}
  LHints: TAddressInfo;
  LAddrInfo: PAddressInfo;
  {$ELSE}
  LHints: AddrInfo; //The T is no omission - that's what I found in the header
  LAddrInfo: PAddrInfo;
  {$ENDIF}
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  case AIPVersion of
    Id_IPv4: begin
      with Psockaddr(@LAddr)^ do begin
        sin_family := Id_PF_INET4;
        TranslateStringToTInAddr(AAddress, sin_addr, Id_IPv4);
      end;
      LSize := SizeOf(sockaddr);
    end;
    Id_IPv6: begin
      with LAddr do begin
        sin6_family := Id_PF_INET6;
        TranslateStringToTInAddr(AAddress, sin6_addr, Id_IPv6);
      end;
      LSize := SizeOf(sockaddr_in6);
    end;
    else begin
      LSize := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;
  FillChar(LHostName[0],Length(LHostName),0);
  {$IFDEF USE_MARSHALLED_PTRS}
  LHostNamePtr := TPtrWrapper.Create(@LHostName[0]);
  {$ENDIF}
  LRet := getnameinfo(Psockaddr(@LAddr)^, LSize,
    {$IFDEF USE_MARSHALLED_PTRS}
    LHostNamePtr.ToPointer
    {$ELSE}
    LHostName
    {$ENDIF},
    NI_MAXHOST,nil,0,NI_NAMEREQD );
  if LRet <> 0 then begin
    if LRet = EAI_SYSTEM then begin
      RaiseLastOSError;
    end else begin
      raise EIdReverseResolveError.CreateFmt(RSReverseResolveError, [AAddress, gai_strerror(LRet), LRet]);
    end;
  end;
{
IMPORTANT!!!

getnameinfo can return either results from a numeric to text conversion or
results from a DNS reverse lookup.  Someone could make a malicous PTR record
such as

   1.0.0.127.in-addr.arpa. IN PTR  10.1.1.1

and trick a caller into beleiving the socket address is 10.1.1.1 instead of
127.0.0.1.  If there is a numeric host in LAddr, than this is the case and
we disregard the result and raise an exception.
}
  FillChar(LHints,SizeOf(LHints),0);
  LHints.ai_socktype := SOCK_DGRAM; //*dummy*/
  LHints.ai_flags := AI_NUMERICHOST;
  if getaddrinfo(
    {$IFDEF USE_MARSHALLED_PTRS}
    LHostNamePtr.ToPointer
    {$ELSE}
    LHostName
    {$ENDIF},
    '0', LHints, LAddrInfo) = 0 then
  begin
    freeaddrinfo(LAddrInfo^);
    Result := '';
    raise EIdMaliciousPtrRecord.Create(RSMaliciousPtrRecord);
  end;

  {$IFDEF USE_MARSHALLED_PTRS}
  Result := TMarshal.ReadStringAsAnsi(LHostNamePtr);
  {$ELSE}
  Result := String(LHostName);
  {$ENDIF}
(* JMB: I left this in here just in case someone
        complains, but the other code works on all
        linux systems for all addresses and is thread-safe

variables for it:
  Host: PHostEnt;
  LAddr: u_long;

    Id_IPv4: begin
      // GetHostByAddr is thread-safe in Linux.
      // It might not be safe in Solaris or BSD Unix
      LAddr := inet_addr(PAnsiChar(AAddress));
      Host := GetHostByAddr(@LAddr,SizeOf(LAddr),AF_INET);
      if (Host <> nil) then begin
        Result := Host^.h_name;
      end else begin
        RaiseSocketError(h_errno);
      end;
    end;
*)
end;

function TIdStackLinux.WSShutdown(ASocket: TIdStackSocketHandle; AHow: Integer): Integer;
begin
  Result := Libc.shutdown(ASocket, AHow);
end;

procedure TIdStackLinux.Disconnect(ASocket: TIdStackSocketHandle);
begin
  // Windows uses Id_SD_Send, Linux should use Id_SD_Both
  WSShutdown(ASocket, Id_SD_Both);
  // SO_LINGER is false - socket may take a little while to actually close after this
  WSCloseSocket(ASocket);
end;

procedure TIdStackLinux.GetPeerName(ASocket: TIdStackSocketHandle;
 var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion);
var
  LiSize: UInt32;
  LAddr: sockaddr_in6;
begin
  LiSize := SizeOf(LAddr);
  CheckForSocketError(Libc.getpeername(ASocket, Psockaddr(@LAddr)^, LiSize));
  case LAddr.sin6_family of
    Id_PF_INET4: begin
      with Psockaddr(@LAddr6)^ do begin
        VIP := TranslateTInAddrToString(sin_addr, Id_IPv4);
        VPort := ntohs(sin_port);
      end;
      VIPVersion := Id_IPV4;
    end;
    Id_PF_INET6: begin
      with LAddr do begin
        VIP := TranslateTInAddrToString(sin6_addr, Id_IPv6);
        VPort := Ntohs(sin6_port);
      end;
      VIPVersion := Id_IPV6;
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

procedure TIdStackLinux.GetSocketName(ASocket: TIdStackSocketHandle;
  var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion);
var
  LiSize: UInt32;
  LAddr: sockaddr_in6;
begin
  LiSize := SizeOf(LAddr);
  CheckForSocketError(Libc.getsockname(ASocket, Psockaddr(@LAddr)^, LiSize));
  case LAddr.sin6_family of
    Id_PF_INET4: begin
      with Psockaddr(@LAddr6)^ do begin
        VIP := TranslateTInAddrToString(sin_addr, Id_IPv4);
        VPort := ntohs(sin_port);
      end;
      VIPVersion := Id_IPV4;
    end;
    Id_PF_INET6: begin
      with LAddr do begin
        VIP := TranslateTInAddrToString(sin6_addr, Id_IPv6);
        VPort := ntohs(sin6_port);
      end;
      VIPVersion := Id_IPV6;
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

function TIdStackLinux.WouldBlock(const AResult: Integer): Boolean;
begin
  //non-blocking does not exist in Linux, always indicate things will block
  Result := True;

  // TODO: enable this:
  //Result := (AResult in [EAGAIN, EWOULDBLOCK, EINPROGRESS]);
end;

function TIdStackLinux.SupportsIPv4: Boolean;
begin
  //In Windows, this does something else.  It checks the LSP's installed.
  Result := CheckIPVersionSupport(Id_IPv4);
end;

function TIdStackLinux.SupportsIPv6: Boolean;
begin
  //In Windows, this does something else.  It checks the LSP's installed.
  Result := CheckIPVersionSupport(Id_IPv6);
end;

function TIdStackLinux.CheckIPVersionSupport(const AIPVersion: TIdIPVersion): Boolean;
var
  LTmpSocket: TIdStackSocketHandle;
begin
  // TODO: an alternative would be to check for the existance of the '/proc/net/if_inet6' kernel pseudo-file
  LTmpSocket := WSSocket(IdIPFamily[AIPVersion], Id_SOCK_STREAM, Id_IPPROTO_IP );
  Result := LTmpSocket <> Id_INVALID_SOCKET;
  if Result then begin
    WSCloseSocket(LTmpSocket);
  end;
end;

procedure TIdStackLinux.WriteChecksum(s: TIdStackSocketHandle;
  var VBuffer: TIdBytes; const AOffset: Integer; const AIP: String;
  const APort: TIdPort; const AIPVersion: TIdIPVersion);
begin
  case AIPVersion of
    Id_IPv4 : CopyTIdUInt16(HostToLittleEndian(CalcCheckSum(VBuffer)), VBuffer, AOffset);
    Id_IPv6 : WriteChecksumIPv6(s, VBuffer, AOffset, AIP, APort);
  else
    IPVersionUnsupported;
  end;
end;

procedure TIdStackLinux.WriteChecksumIPv6(s: TIdStackSocketHandle;
  var VBuffer: TIdBytes; const AOffset: Integer; const AIP: String;
  const APort: TIdPort);
begin
//we simply request that the kernal write the checksum when the data
//is sent.  All of the parameters required are because Windows is bonked
//because it doesn't have the IPV6CHECKSUM socket option meaning we have
//to querry the network interface in TIdStackWindows -- yuck!!
  SetSocketOption(s, IPPROTO_IPV6, IPV6_CHECKSUM, AOffset);
end;

function TIdStackLinux.IOControl(const s: TIdStackSocketHandle;
  const cmd: UInt32; var arg: UInt32): Integer;
begin
  Result := ioctl(s, cmd, @arg);
end;

{ TIdSocketListLinux }

procedure TIdSocketListLinux.Add(AHandle: TIdStackSocketHandle);
begin
  Lock;
  try
    if not FD_ISSET(AHandle, FFDSet) then begin
      if Count >= __FD_SETSIZE then begin
        raise EIdStackSetSizeExceeded.Create(RSSetSizeExceeded);
      end;
      FD_SET(AHandle, FFDSet);
      Inc(FCount);
    end;
  finally
    Unlock;
  end;
end;//

procedure TIdSocketListLinux.Clear;
begin
  Lock;
  try
    FD_ZERO(FFDSet);
    FCount := 0;
  finally
    Unlock;
  end;
end;

function TIdSocketListLinux.ContainsSocket(
  AHandle: TIdStackSocketHandle): boolean;
begin
  Lock;
  try
    Result := FD_ISSET(AHandle, FFDSet);
  finally
    Unlock;
  end;
end;

function TIdSocketListLinux.Count: Integer;
begin
  Lock;
  try
    Result := FCount;
  finally
    Unlock;
  end;
end;//

class function TIdSocketListLinux.FDSelect(AReadSet, AWriteSet,
  AExceptSet: PFDSet; const ATimeout: Integer): integer;
var
  LTime: TTimeVal;
  LTimePtr: PTimeVal;
begin
  if ATimeout = IdTimeoutInfinite then begin
    LTimePtr := nil;
  end else begin
    LTime.tv_sec := ATimeout div 1000;
    LTime.tv_usec := (ATimeout mod 1000) * 1000;
    LTimePtr := @LTime;
  end;
  // TODO: calculate the actual nfds value based on the Sets provided...
  Result := Libc.select(MaxLongint, AReadSet, AWriteSet, AExceptSet, LTimePtr);
end;

procedure TIdSocketListLinux.GetFDSet(var VSet: TFDSet);
begin
  Lock;
  try
    VSet := FFDSet;
  finally
    Unlock;
  end;
end;

function TIdSocketListLinux.GetItem(AIndex: Integer): TIdStackSocketHandle;
var
  LIndex, i: Integer;
begin
  Result := 0;
  Lock;
  try
    LIndex := 0;
    //? use FMaxHandle div x
    for i:= 0 to __FD_SETSIZE - 1 do begin
      if FD_ISSET(i, FFDSet) then begin
        if LIndex = AIndex then begin
          Result := i;
          Break;
        end;
        Inc(LIndex);
      end;
    end;
  finally
    Unlock;
  end;
end;//

procedure TIdSocketListLinux.Remove(AHandle: TIdStackSocketHandle);
begin
  Lock;
  try
    if FD_ISSET(AHandle, FFDSet) then begin
      Dec(FCount);
      FD_CLR(AHandle, FFDSet);
    end;
  finally
    Unlock;
  end;
end;//


function TIdStackLinux.WSTranslateSocketErrorMsg(const AErr: Integer): string;
begin
  //we override this function for the herr constants that
  //are returned by the DNS functions
  case AErr of
    Libc.HOST_NOT_FOUND: Result := RSStackHOST_NOT_FOUND;
    Libc.TRY_AGAIN: Result := RSStackTRY_AGAIN;
    Libc.NO_RECOVERY: Result := RSStackNO_RECOVERY;
    Libc.NO_DATA: Result := RSStackNO_DATA;
  else
    Result := inherited WSTranslateSocketErrorMsg(AErr);
  end;
end;

procedure TIdSocketListLinux.SetFDSet(var VSet: TFDSet);
begin
  Lock;
  try
    FFDSet := VSet;
  finally
    Unlock;
  end;
end;

class function TIdSocketListLinux.Select(AReadList: TIdSocketList; AWriteList: TIdSocketList;
  AExceptList: TIdSocketList; const ATimeout: Integer = IdTimeoutInfinite): Boolean;
var
  LReadSet: TFDSet;
  LWriteSet: TFDSet;
  LExceptSet: TFDSet;
  LPReadSet: PFDSet;
  LPWriteSet: PFDSet;
  LPExceptSet: PFDSet;

  procedure ReadSet(AList: TIdSocketList; var ASet: TFDSet; var APSet: PFDSet);
  begin
    if AList <> nil then begin
      TIdSocketListLinux(AList).GetFDSet(ASet);
      APSet := @ASet;
    end else begin
      APSet := nil;
    end;
  end;

begin
  ReadSet(AReadList, LReadSet, LPReadSet);
  ReadSet(AWriteList, LWriteSet, LPWriteSet);
  ReadSet(AExceptList, LExceptSet, LPExceptSet);
  //
  Result := FDSelect(LPReadSet, LPWriteSet, LPExceptSet, ATimeout) >0;
  //
  if AReadList <> nil then begin
    TIdSocketListLinux(AReadList).SetFDSet(LReadSet);
  end;
  if AWriteList <> nil then begin
    TIdSocketListLinux(AWriteList).SetFDSet(LWriteSet);
  end;
  if AExceptList <> nil then begin
    TIdSocketListLinux(AExceptList).SetFDSet(LExceptSet);
  end;
end;

function TIdSocketListLinux.SelectRead(const ATimeout: Integer): Boolean;
var
  LSet: TFDSet;
begin
  Lock;
  try
    LSet := FFDSet;
    // select() updates this structure on return,
    // so we need to copy it each time we need it
  finally
    Unlock;
  end;
  Result := FDSelect(@LSet, nil, nil, ATimeout) > 0;
end;

function TIdSocketListLinux.SelectReadList(var VSocketList: TIdSocketList;
  const ATimeout: Integer = IdTimeoutInfinite): Boolean;
var
  LSet: TFDSet;
begin
  Lock;
  try
    LSet := FFDSet;
    // select() updates this structure on return,
    // so we need to copy it each time we need it
  finally
    Unlock;
  end;
  Result := FDSelect(@LSet, nil, nil, ATimeout) > 0;
  if Result then begin
    if VSocketList = nil then begin
      VSocketList := TIdSocketList.CreateSocketList;
    end;
    TIdSocketListLinux(VSocketList).SetFDSet(LSet);
  end;
end;

procedure TIdStackLinux.SetBlocking(ASocket: TIdStackSocketHandle;
  const ABlocking: Boolean);
begin
  // TODO: enable this
  {
  LFlags := CheckForSocketError(Libc.fcntl(ASocket, F_GETFL, 0));
  if ABlocking then begin
    LFlags := LFlags and not O_NONBLOCK;
  end else begin
    LFlags := LFlags or O_NONBLOCK;
  end;
  CheckForSocketError(Libc.fcntl(ASocket, F_SETFL, LFlags));

or

  LValue := UInt32(not ABlocking);
  CheckForSocketError(Libc.ioctl(ASocket, FIONBIO, @LValue));
  }
  if not ABlocking then begin
    raise EIdNonBlockingNotSupported.Create(RSStackNonBlockingNotSupported);
  end;
end;

(*
Why did I remove this again?

 1) it sends SIGPIPE even if the socket is created with the no-sigpipe bit set
    that could be solved by blocking sigpipe within this thread
    This is probably a bug in the Linux kernel, but we could work around it
    by blocking that signal for the time of sending the file (just get the
    sigprocmask, see if pipe bit is set, if not set it and remove again after
    sending the file)

But the more serious reason is another one, which exists in Windows too:
 2) I think that ServeFile is misdesigned:
    ServeFile does not raise an exception if it didn't send all the bytes.
    Now what happens if I have OnExecute assigned like this
      AThread.Connection.ServeFile('...', True); // <-- true to send via kernel
    is that it will return 0, but notice that in this case I didn't ask for the
    result. Net effect is that the thread will loop in OnExecute even if the
    socket is long gone. This doesn't fit Indy semantics at all, exceptions are
    always raised if the remote end disconnects. Even if I would do
      AThread.Connection.ServeFile('...', False);
    then it would raise an exception.
    I think this is a big flaw in the design of the ServeFile function.
    Maybe GServeFile should only return the bytes sent, but then
    TCPConnection.ServeFile() should raise an exception if GServeFile didn't
    send all the bytes.

JM Berg, 2002-09-09

function ServeFile(ASocket: TIdStackSocketHandle; AFileName: string): UInt32;
var
  LFileHandle: integer;
  offset: integer;
  stat: _stat;
begin
  LFileHandle := open(PAnsiChar(AFileName), O_RDONLY);
  try
    offset := 0;
    fstat(LFileHandle, stat);
    Result := sendfile(ASocket, LFileHandle, offset, stat.st_size);
//**    if Result = UInt32(-1) then RaiseLastOSError;
  finally libc.__close(LFileHandle); end;
end;
*)
function TIdSocketListLinux.Clone: TIdSocketList;
begin
  Result := TIdSocketListLinux.Create;
  try
    Lock;
    try
      TIdSocketListLinux(Result).SetFDSet(FFDSet);
    finally
      Unlock;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

initialization
  GSocketListClass := TIdSocketListLinux;

end.


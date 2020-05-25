unit IdStackVCLPosix;

interface

{$I IdCompilerDefines.inc}

{IMPORTANT!!!

Platform warnings in this unit should be disabled because Indy we have no
intention of porting this unit to Windows or any non-Unix-like operating system.

Any differences between Unix-like operating systems have to dealt with in other
ways.
}

{$I IdSymbolPlatformOff.inc}
{$I IdUnitPlatformOff.inc}

uses
  Classes,
  IdCTypes,
  Posix.SysSelect,
  Posix.SysSocket,
  Posix.SysTime,
  IdStack,
  IdStackConsts,
  IdGlobal,
  IdStackBSDBase;

type
  {$IFDEF USE_VCL_POSIX}
    {$IFDEF ANDROID}
  EIdAccessWifiStatePermissionNeeded = class(EIdAndroidPermissionNeeded);
  EIdAccessNetworkStatePermissionNeeded = class(EIdAndroidPermissionNeeded);
    {$ENDIF}
  {$ENDIF}

  TIdSocketListVCLPosix = class (TIdSocketList)
  protected
    FCount: Integer;
    FFDSet: fd_set;
    //
    class function FDSelect(AReadSet, AWriteSet,
      AExceptSet: Pfd_set; const ATimeout: Integer): Integer;
    function GetItem(AIndex: Integer): TIdStackSocketHandle; override;
  public
    procedure Add(AHandle: TIdStackSocketHandle); override;
    procedure Remove(AHandle: TIdStackSocketHandle); override;
    function Count: Integer; override;
    procedure Clear; override;
    function Clone: TIdSocketList; override;
    function ContainsSocket(AHandle: TIdStackSocketHandle): Boolean; override;
    procedure GetFDSet(var VSet: fd_set);
    procedure SetFDSet(var VSet: fd_set);
    class function Select(AReadList: TIdSocketList; AWriteList: TIdSocketList;
      AExceptList: TIdSocketList; const ATimeout: Integer = IdTimeoutInfinite): Boolean; override;
    function SelectRead(const ATimeout: Integer = IdTimeoutInfinite): Boolean; override;
    function SelectReadList(var VSocketList: TIdSocketList;
      const ATimeout: Integer = IdTimeoutInfinite): Boolean; override;
  end;

  TIdStackVCLPosix = class(TIdStackBSDBase)
  protected
    procedure WriteChecksumIPv6(s: TIdStackSocketHandle; var VBuffer: TIdBytes;
      const AOffset: Integer; const AIP: String; const APort: TIdPort);
    function GetLastError: Integer;
    procedure SetLastError(const AError: Integer);
    function HostByName(const AHostName: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string; override;
    function ReadHostName: string; override;
    function WSCloseSocket(ASocket: TIdStackSocketHandle): Integer; override;
    function WSRecv(ASocket: TIdStackSocketHandle; var ABuffer;
      const ABufferLength, AFlags: Integer): Integer; override;
    function WSSend(ASocket: TIdStackSocketHandle; const ABuffer;
      const ABufferLength, AFlags: Integer): Integer; override;
    function WSShutdown(ASocket: TIdStackSocketHandle; AHow: Integer): Integer; override;
    {$IFNDEF VCL_XE3_OR_ABOVE}
    procedure WSGetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; var AOptVal; var AOptLen: Integer); override;
    procedure WSSetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; const AOptVal; const AOptLen: Integer); override;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetBlocking(ASocket: TIdStackSocketHandle; const ABlocking: Boolean); override;
    function WouldBlock(const AResult: Integer): Boolean; override;
    function Accept(ASocket: TIdStackSocketHandle; var VIP: string; var VPort: TIdPort;
      var VIPVersion: TIdIPVersion): TIdStackSocketHandle; override;
    procedure Bind(ASocket: TIdStackSocketHandle; const AIP: string;
     const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    procedure Connect(const ASocket: TIdStackSocketHandle; const AIP: string;
     const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function HostByAddress(const AAddress: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string; override;
    function WSGetLastError: Integer; override;
    procedure WSSetLastError(const AErr : Integer); override;
    function WSGetServByName(const AServiceName: string): TIdPort; override;
    procedure AddServByPortToList(const APortNumber: TIdPort; AAddresses: TStrings); override;
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
    function RecvFrom(const ASocket: TIdStackSocketHandle;
      var VBuffer; const ALength, AFlags: Integer; var VIP: string;
      var VPort: TIdPort; var VIPVersion: TIdIPVersion): Integer; override;
    function ReceiveMsg(ASocket: TIdStackSocketHandle;
      var VBuffer: TIdBytes; APkt: TIdPacketInfo): UInt32;  override;
    procedure WSSendTo(ASocket: TIdStackSocketHandle; const ABuffer;
      const ABufferLength, AFlags: Integer; const AIP: string; const APort: TIdPort;
      AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
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
    //In Windows, this writes a checksum into a buffer.  In Linux, it would probably
    //simply have the kernal write the checksum with something like this (RFC 2292):
//
//    int  offset = 2;
//    setsockopt(fd, IPPROTO_IPV6, IPV6_CHECKSUM, &offset, sizeof(offset));
//
//  Note that this should be called
    //IMMEDIATELY before you do a SendTo because the Local IPv6 address might change

    procedure WriteChecksum(s : TIdStackSocketHandle; var VBuffer : TIdBytes;
      const AOffset : Integer; const AIP : String; const APort : TIdPort;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function IOControl(const s: TIdStackSocketHandle; const cmd: UInt32;
      var arg: UInt32): Integer; override;

    procedure GetLocalAddressList(AAddresses: TIdStackLocalAddressList); override;
  end;

implementation

{$O-}

uses
  IdResourceStrings,
  IdResourceStringsUnix,
  IdResourceStringsVCLPosix,
  IdException,
  IdVCLPosixSupplemental,
  Posix.Base,
  Posix.ArpaInet,
  Posix.Errno,
  Posix.NetDB,
  {$IFDEF HAS_getifaddrs}
  Posix.NetIf,
  {$ENDIF}
  Posix.NetinetIn,
  Posix.StrOpts,
  Posix.SysTypes,
  Posix.SysUio,
  Posix.Unistd,
  SysUtils;

  {$UNDEF HAS_MSG_NOSIGNAL}
  {$IFDEF LINUX}  //this LINUX ifdef is deliberate
    {$DEFINE HAS_MSG_NOSIGNAL}
  {$ENDIF}


const
  {$IFDEF HAS_MSG_NOSIGNAL}
  //fancy little trick since OS X does not have MSG_NOSIGNAL
  Id_MSG_NOSIGNAL = MSG_NOSIGNAL;
  {$ELSE}
  Id_MSG_NOSIGNAL = 0;
  {$ENDIF}
  Id_WSAEPIPE = EPIPE;



//helper functions for some structs

{Note:  These hide an API difference in structures.

BSD 4.4 introduced a minor API change.  sa_family was changed from a 16bit
word to an 8 bit byteee and an 8 bit byte feild named sa_len was added.

}
procedure InitSockAddr_In(var VSock : SockAddr_In);
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  FillChar(VSock, SizeOf(SockAddr_In), 0);
  VSock.sin_family := PF_INET;
  {$IFDEF SOCK_HAS_SINLEN}
  VSock.sin_len := SizeOf(SockAddr_In);
  {$ENDIF}
end;

procedure InitSockAddr_in6(var VSock : SockAddr_in6);
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  FillChar(VSock, SizeOf(SockAddr_in6), 0);
  {$IFDEF SOCK_HAS_SINLEN}
  VSock.sin6_len := SizeOf(SockAddr_in6);
  {$ENDIF}
  VSock.sin6_family := PF_INET6;
end;
//

{ TIdSocketListVCLPosix }

procedure TIdSocketListVCLPosix.Add(AHandle: TIdStackSocketHandle);
begin
  Lock;
  try
    if not __FD_ISSET(AHandle, FFDSet) then begin
      if Count >= FD_SETSIZE then begin
        raise EIdStackSetSizeExceeded.Create(RSSetSizeExceeded);
      end;
      __FD_SET(AHandle, FFDSet);
      Inc(FCount);
    end;
  finally
    Unlock;
  end;
end;

procedure TIdSocketListVCLPosix.Clear;
begin
  Lock;
  try
    __FD_ZERO(FFDSet);
    FCount := 0;
  finally
    Unlock;
  end;
end;

function TIdSocketListVCLPosix.Clone: TIdSocketList;
begin
  Result := TIdSocketListVCLPosix.Create;
  try
    Lock;
    try
      TIdSocketListVCLPosix(Result).SetFDSet(FFDSet);
    finally
      Unlock;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TIdSocketListVCLPosix.ContainsSocket(
  AHandle: TIdStackSocketHandle): Boolean;
begin
  Lock;
  try
    Result := __FD_ISSET(AHandle, FFDSet);
  finally
    Unlock;
  end;
end;

function TIdSocketListVCLPosix.Count: Integer;
begin
  Lock;
  try
    Result := FCount;
  finally
    Unlock;
  end;
end;

class function TIdSocketListVCLPosix.FDSelect(AReadSet, AWriteSet,
  AExceptSet: Pfd_set; const ATimeout: Integer): Integer;
var
  LTime: TimeVal;
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
  Result := Posix.SysSelect.select(FD_SETSIZE, AReadSet, AWriteSet, AExceptSet, LTimePtr);
end;

procedure TIdSocketListVCLPosix.GetFDSet(var VSet: fd_set);
begin
  Lock;
  try
    VSet := FFDSet;
  finally
    Unlock;
  end;
end;

function TIdSocketListVCLPosix.GetItem(AIndex: Integer): TIdStackSocketHandle;
var
  LIndex, i: Integer;
begin
  Result := 0;
  Lock;
  try
    LIndex := 0;
    //? use FMaxHandle div x
    for i:= 0 to FD_SETSIZE - 1 do begin
      if __FD_ISSET(i, FFDSet) then begin
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
end;

procedure TIdSocketListVCLPosix.Remove(AHandle: TIdStackSocketHandle);
begin
  Lock;
  try
    if __FD_ISSET(AHandle, FFDSet) then begin
      Dec(FCount);
      __FD_CLR(AHandle, FFDSet);
    end;
  finally
    Unlock;
  end;
end;

class function TIdSocketListVCLPosix.Select(AReadList, AWriteList,
  AExceptList: TIdSocketList; const ATimeout: Integer): Boolean;

var
  LReadSet: fd_set;
  LWriteSet: fd_set;
  LExceptSet: fd_set;
  LPReadSet: Pfd_set;
  LPWriteSet: Pfd_set;
  LPExceptSet: Pfd_set;

  procedure ReadSet(AList: TIdSocketList; var ASet: fd_set; var APSet: Pfd_set);
  begin
    if AList <> nil then begin
      TIdSocketListVCLPosix(AList).GetFDSet(ASet);
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
    TIdSocketListVCLPosix(AReadList).SetFDSet(LReadSet);
  end;
  if AWriteList <> nil then begin
    TIdSocketListVCLPosix(AWriteList).SetFDSet(LWriteSet);
  end;
  if AExceptList <> nil then begin
    TIdSocketListVCLPosix(AExceptList).SetFDSet(LExceptSet);
  end;
end;

function TIdSocketListVCLPosix.SelectRead(const ATimeout: Integer): Boolean;
var
  LSet: fd_set;
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

function TIdSocketListVCLPosix.SelectReadList(var VSocketList: TIdSocketList;
  const ATimeout: Integer): Boolean;
var
  LSet: fd_set;
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
    TIdSocketListVCLPosix(VSocketList).SetFDSet(LSet);
  end;
end;

procedure TIdSocketListVCLPosix.SetFDSet(var VSet: fd_set);
begin
  Lock;
  try
    FFDSet := VSet;
  finally
    Unlock;
  end;
end;

{ TIdStackVCLPosix }

{
IMPORTANT!!!

Throughout much of this code, you will see stuff such as:

var
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;

This is just a fancy way to do typecasting with various types of address type.
Many functions take a sockaddr parameter but that parameter is typecast for various
address types.  The structures mentioned above are designed just for such
typecasting.  The reason we use sockaddr_storage instead of sockaddr is that
we need something that is guaranteed to be able to contain various address types
and sockaddr would be too short for some of them and we can't know what
someone else will add to Indy as time goes by.
}

function TIdStackVCLPosix.Accept(ASocket: TIdStackSocketHandle; var VIP: string;
  var VPort: TIdPort; var VIPVersion: TIdIPVersion): TIdStackSocketHandle;
var
  LN: socklen_t;
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;

begin
  LN := SizeOf(LAddrStore);
  Result := Posix.SysSocket.accept(ASocket, LAddr, LN);
  if Result <> -1 then begin
    {$IFDEF HAS_SOCKET_NOSIGPIPE}
    SetSocketOption(Result, SOL_SOCKET, SO_NOSIGPIPE, 1);
    {$ENDIF}
    case LAddrStore.ss_family of
      Id_PF_INET4: begin
        VIP := TranslateTInAddrToString( LAddrIPv4.sin_addr, Id_IPv4);
        VPort := ntohs(LAddrIPv4.sin_port);
        VIPVersion := Id_IPV4;
      end;
      Id_PF_INET6: begin
        VIP := TranslateTInAddrToString(LAddrIPv6.sin6_addr, Id_IPv6);
        VPort := ntohs(LAddrIPv6.sin6_port);
        VIPVersion := Id_IPV6;
      end
      else begin
        __close(Result);
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

{$IFDEF HAS_getifaddrs}
function getifaddrs(var ifap: pifaddrs): Integer; cdecl; external libc name _PU + 'getifaddrs'; {do not localize}
procedure freeifaddrs(ifap: pifaddrs); cdecl; external libc name _PU + 'freeifaddrs'; {do not localize}
{$ELSE}
  {$IFDEF ANDROID}
  // TODO: implement getifaddrs() manually using code from https://github.com/kmackay/android-ifaddrs
  {.$DEFINE HAS_getifaddrs}
  {$ENDIF}
{$ENDIF}

procedure TIdStackVCLPosix.GetLocalAddressList(AAddresses: TIdStackLocalAddressList);
var
  {$IFDEF HAS_getifaddrs}
  LAddrList, LAddrInfo: pifaddrs;
  LSubNetStr: String;
  {$ELSE}
  LRetVal: Integer;
  LHostName: string;
  Hints: AddrInfo;
  LAddrList, LAddrInfo: pAddrInfo;
    {$IFDEF USE_MARSHALLED_PTRS}
  M: TMarshaller;
    {$ENDIF}
  {$ENDIF}
begin
  // TODO: Using gethostname() and getaddrinfo() like this may not always return just
  // the machine's IP addresses. Technically speaking, they will return the local
  // hostname, and then return the address(es) to which that hostname resolves.
  // It is possible for a machine to (a) be configured such that its name does
  // not resolve to an IP, or (b) be configured such that its name resolves to
  // multiple IPs, only one of which belongs to the local machine. For better
  // results, we should use getifaddrs() on platforms that support it...

  {$IFDEF HAS_getifaddrs}

  if getifaddrs(LAddrList) = 0 then // TODO: raise an exception if it fails
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
                LSubNetStr := TranslateTInAddrToString( PSockAddr_In(LAddrInfo^.ifa_netmask)^.sin_addr, Id_IPv4);
              end else begin
                LSubNetStr := '';
              end;
              TIdStackLocalAddressIPv4.Create(AAddresses, TranslateTInAddrToString( PSockAddr_In(LAddrInfo^.ifa_addr)^.sin_addr, Id_IPv4), LSubNetStr);
            end;
            Id_PF_INET6: begin
              TIdStackLocalAddressIPv6.Create(AAddresses, TranslateTInAddrToString( PSockAddr_In6(LAddrInfo^.ifa_addr)^.sin6_addr, Id_IPv6));
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

  // TODO: on Android, either implement getifaddrs() (https://github.com/kmackay/android-ifaddrs)
  // or use the Java API to enumerate the local network interfaces and their IP addresses, eg:
  {
  var
    en, enumIpAddr: Enumeration;
    intf: NetworkInterface;
    inetAddress: InetAddress;
  begin
    try
      en := NetworkInterface.getNetworkInterfaces;
      if en.hasMoreElements then begin
        AAddresses.BeginUpdate;
        try
          repeat
            intf := en.nextElement;
            enumIpAddr := intf.getInetAddresses();
            while enumIpAddr.hasMoreElements do begin
              inetAddress := enumIpAddr.nextElement;
              if not inetAddress.isLoopbackAddress then begin
                if (inetAddress instanceof Inet4Address) then begin
                  TIdStackLocalAddressIPv4.Create(AAddresses, inetAddress.getHostAddress.toString, ''); // TODO: subnet mask
                end
                else if (inetAddress instanceof Inet6Address) then begin
                  TIdStackLocalAddressIPv6.Create(AAddresses, inetAddress.getHostAddress.toString);
                end;
              end;
            end;
          until not en.hasMoreElements;
        finally
          AAddresses.EndUpdate;
        end;
       end;
     except
       if not HasAndroidPermission('android.permission.ACCESS_NETWORK_STATE') then begin
         IndyRaiseOuterException(EIdAccessNetworkStatePermissionNeeded.CreateError(0, ''));
       end;
       if not HasAndroidPermission('android.permission.INTERNET') then begin
         IndyRaiseOuterException(EIdInternetPermissionNeeded.CreateError(0, ''));
       end;
       raise;
     end;
   end;

  Note that this requires the application to have ACCESS_NETWORK_STATE and INTERNET permissions.

  Or:

  uses
    if XE7+
      Androidapi.Helpers 
    else
      FMX.Helpers.Android 
    ;

  var
    wifiManager: WifiManager;
    ipAddress: Integer;
  begin
    try
      wifiManager := (WifiManager) GetActivityContext.getSystemService(WIFI_SERVICE);
      ipAddress := wifiManager.getConnectionInfo.getIpAddress;
    except
      if not HasAndroidPermission('android.permission.ACCESS_WIFI_STATE') then begin
        IndyRaiseOuterException(EIdAccessWifiStatePermissionNeeded.CreateError(0, ''));
      end;
      raise;
    end;

    // WiFiInfo only supports IPv4
    TIdStackLocalAddressIPv4.Create(AAddresses,
      Format('%d.%d.%d.%d', [ipAddress and $ff, (ipAddress shr 8) and $ff, (ipAddress shr 16) and $ff, (ipAddress shr 24) and $ff]),
      '' // TODO: subnet mask
    );
  end;

  This requires only ACCESS_WIFI_STATE permission.
  }

  //IMPORTANT!!!
  //
  //The Hints structure must be zeroed out or you might get an AV.
  //I've seen this in Mac OS X
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family := PF_UNSPEC; // returns both IPv4 and IPv6 addresses
  Hints.ai_socktype := SOCK_STREAM;

  LHostName := HostName;

  LRetVal := getaddrinfo(
    {$IFDEF USE_MARSHALLED_PTRS}
    M.AsAnsi(LHostName).ToPointer
    {$ELSE}
    PAnsiChar(
      {$IFDEF STRING_IS_ANSI}
      LHostName
      {$ELSE}
      AnsiString(LHostName) // explicit convert to Ansi
      {$ENDIF}
    )
    {$ENDIF},
    nil, Hints, LAddrList);
  if LRetVal <> 0 then begin
    if LRetVal = EAI_SYSTEM then begin
      RaiseLastOSError;
    end else begin
      raise EIdReverseResolveError.CreateFmt(RSReverseResolveError, [LHostName, gai_strerror(LRetVal), LRetVal]);
    end;
  end;
  try
    AAddresses.BeginUpdate;
    try
      LAddrInfo := LAddrList;
      repeat
        case LAddrInfo^.ai_addr^.sa_family of
        Id_PF_INET4 :
          begin
            TIdStackLocalAddressIPv4.Create(AAddresses, TranslateTInAddrToString( PSockAddr_In(LAddrInfo^.ai_addr)^.sin_addr, Id_IPv4), ''); // TODO: SubNet
          end;
        Id_PF_INET6 :
          begin
            TIdStackLocalAddressIPv6.Create(AAddresses, TranslateTInAddrToString( PSockAddr_In6(LAddrInfo^.ai_addr)^.sin6_addr, Id_IPv6));
          end;
        end;
        LAddrInfo := LAddrInfo^.ai_next;
      until LAddrInfo = nil;
    finally
      AAddresses.EndUpdate;
    end;
  finally
    freeaddrinfo(LAddrList^);
  end;

  {$ENDIF}
end;

procedure TIdStackVCLPosix.Bind(ASocket: TIdStackSocketHandle;
  const AIP: string; const APort: TIdPort; const AIPVersion: TIdIPVersion);
var
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;
begin
  case AIPVersion of
    Id_IPv4: begin
        InitSockAddr_In(LAddrIPv4);
        if AIP <> '' then begin
          TranslateStringToTInAddr(AIP, LAddrIPv4.sin_addr, Id_IPv4);
        end;
        LAddrIPv4.sin_port := htons(APort);
        CheckForSocketError(Posix.SysSocket.bind(ASocket, LAddr, SizeOf(LAddrIPv4)));
      end;
    Id_IPv6: begin
        InitSockAddr_in6(LAddrIPv6);
        if AIP <> '' then begin
          TranslateStringToTInAddr(AIP, LAddrIPv6.sin6_addr, Id_IPv6);
        end;
        LAddrIPv6.sin6_port := htons(APort);
        CheckForSocketError(Posix.SysSocket.bind(ASocket,LAddr, SizeOf(LAddrIPv6)));
      end;
    else begin
      IPVersionUnsupported;
    end;
  end;

end;

function TIdStackVCLPosix.CheckIPVersionSupport(
  const AIPVersion: TIdIPVersion): boolean;
var
  LTmpSocket: TIdStackSocketHandle;
begin
  // TODO: on nix systems (or maybe just Linux?), an alternative would be to
  // check for the existance of the '/proc/net/if_inet6' kernel pseudo-file
  LTmpSocket := WSSocket(IdIPFamily[AIPVersion], Id_SOCK_STREAM, Id_IPPROTO_IP );
  Result := LTmpSocket <> Id_INVALID_SOCKET;
  if Result then begin
    WSCloseSocket(LTmpSocket);
  end;
end;

procedure TIdStackVCLPosix.Connect(const ASocket: TIdStackSocketHandle;
  const AIP: string; const APort: TIdPort; const AIPVersion: TIdIPVersion);
var
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;
begin
  case AIPVersion of
    Id_IPv4: begin
      InitSockAddr_In(LAddrIPv4);
      TranslateStringToTInAddr(AIP, LAddrIPv4.sin_addr, Id_IPv4);
      LAddrIPv4.sin_port := htons(APort);
      CheckForSocketError(Posix.SysSocket.connect(ASocket, LAddr, SizeOf(LAddrIPv4)));
    end;
    Id_IPv6: begin
      InitSockAddr_in6(LAddrIPv6);
      TranslateStringToTInAddr(AIP, LAddrIPv6.sin6_addr, Id_IPv6);
      LAddrIPv6.sin6_port := htons(APort);
      CheckForSocketError(Posix.SysSocket.connect(ASocket, LAddr, SizeOf(LAddrIPv6)));
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;

end;

constructor TIdStackVCLPosix.Create;
begin
  inherited Create;
end;

destructor TIdStackVCLPosix.Destroy;
begin
  inherited Destroy;
end;

procedure TIdStackVCLPosix.Disconnect(ASocket: TIdStackSocketHandle);
begin
  // Windows uses Id_SD_Send, Linux should use Id_SD_Both
  WSShutdown(ASocket, Id_SD_Both);
  // SO_LINGER is false - socket may take a little while to actually close after this
  WSCloseSocket(ASocket);
end;

function TIdStackVCLPosix.GetLastError: Integer;
begin
  Result := errno;
end;

procedure TIdStackVCLPosix.GetPeerName(ASocket: TIdStackSocketHandle;
  var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion);
var
  i: socklen_t;
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;
begin
  i := SizeOf(LAddrStore);
  CheckForSocketError(Posix.SysSocket.getpeername(ASocket, LAddr, i));
  case LAddrStore.ss_family of
    Id_PF_INET4: begin
      VIP := TranslateTInAddrToString(LAddrIPv4.sin_addr, Id_IPv4);
      VPort := ntohs(LAddrIPv4.sin_port);
      VIPVersion := Id_IPV4;
    end;
    Id_PF_INET6: begin
      VIP := TranslateTInAddrToString(LAddrIPv6.sin6_addr, Id_IPv6);
      VPort := ntohs(LAddrIPv6.sin6_port);
      VIPVersion := Id_IPV6;
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

procedure TIdStackVCLPosix.GetSocketName(ASocket: TIdStackSocketHandle;
  var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion);
var
  LiSize: socklen_t;
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;
begin
  LiSize := SizeOf(LAddrStore);
  CheckForSocketError(getsockname(ASocket, LAddr, LiSize));
  case LAddrStore.ss_family of
    Id_PF_INET4: begin
      VIP := TranslateTInAddrToString(LAddrIPv4.sin_addr, Id_IPv4);
      VPort := ntohs(LAddrIPv4.sin_port);
      VIPVersion := Id_IPV4;
    end;
    Id_PF_INET6: begin
      VIP := TranslateTInAddrToString(LAddrIPv6.sin6_addr, Id_IPv6);
      VPort := ntohs(LAddrIPv6.sin6_port);
      VIPVersion := Id_IPV6;
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

function TIdStackVCLPosix.HostByAddress(const AAddress: string;
  const AIPVersion: TIdIPVersion): string;
var
  LiSize: socklen_t;
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;
  LHostName : array[0..NI_MAXHOST] of TIdAnsiChar;
  {$IFDEF USE_MARSHALLED_PTRS}
  LHostNamePtr: TPtrWrapper;
  {$ENDIF}
  LRet : Integer;
  LHints : addrinfo;
  LAddrInfo: pAddrInfo;
begin
  LiSize := 0;
  case AIPVersion of
    Id_IPv4 :
    begin
      InitSockAddr_In(LAddrIPv4);
      TranslateStringToTInAddr(AAddress,LAddrIPv4.sin_addr,Id_IPv4);
      LiSize := SizeOf(SockAddr_In);
    end;
    Id_IPv6 :
    begin
      InitSockAddr_In6(LAddrIPv6);
      TranslateStringToTInAddr(AAddress,LAddrIPv6.sin6_addr,Id_IPv6);
      LiSize := SizeOf(SockAddr_In6);
    end
  else
    IPVersionUnsupported;
  end;
  FillChar(LHostName[0],Length(LHostName),0);
  {$IFDEF USE_MARSHALLED_PTRS}
  LHostNamePtr := TPtrWrapper.Create(@LHostName[0]);
  {$ENDIF}
  LRet := getnameinfo(LAddr,LiSize,
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
  FillChar(LHints, SizeOf(LHints), 0);
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
end;

function TIdStackVCLPosix.HostByName(const AHostName: string;
  const AIPVersion: TIdIPVersion): string;
var
  LAddrInfo: pAddrInfo;
  LHints: AddrInfo;
  LRetVal: Integer;
  {$IFDEF USE_MARSHALLED_PTRS}
  M: TMarshaller;
  {$ENDIF}
begin
  if not (AIPVersion in [Id_IPv4, Id_IPv6]) then begin
    IPVersionUnsupported;
  end;
  //IMPORTANT!!!
  //
  //The Hints structure must be zeroed out or you might get an AV.
  //I've seen this in Mac OS X
  FillChar(LHints, SizeOf(LHints), 0);
  LHints.ai_family := IdIPFamily[AIPVersion];
  LHints.ai_socktype := SOCK_STREAM;
  LAddrInfo := nil;

  LRetVal := getaddrinfo(
    {$IFDEF USE_MARSHALLED_PTRS}
    M.AsAnsi(AHostName).ToPointer
    {$ELSE}
    PAnsiChar(
      {$IFDEF STRING_IS_ANSI}
      AHostName
      {$ELSE}
      AnsiString(AHostName) // explicit convert to Ansi
      {$ENDIF}
    )
    {$ENDIF},
    nil, LHints, LAddrInfo);
  if LRetVal <> 0 then begin
    if LRetVal = EAI_SYSTEM then begin
      RaiseLastOSError;
    end else begin
      raise EIdResolveError.CreateFmt(RSReverseResolveError, [AHostName, gai_strerror(LRetVal), LRetVal]);
    end;
  end;
  try
    if AIPVersion = Id_IPv4 then begin
      Result := TranslateTInAddrToString( PSockAddr_In( LAddrInfo^.ai_addr)^.sin_addr, AIPVersion);
    end else begin
      Result := TranslateTInAddrToString( PSockAddr_In6( LAddrInfo^.ai_addr)^.sin6_addr, AIPVersion);
    end;
  finally
    freeaddrinfo(LAddrInfo^);
  end;
end;

function TIdStackVCLPosix.HostToNetwork(AValue: UInt32): UInt32;
begin
 Result := htonl(AValue);
end;

function TIdStackVCLPosix.HostToNetwork(AValue: UInt16): UInt16;
begin
  Result := htons(AValue);
end;

function TIdStackVCLPosix.HostToNetwork(AValue: TIdUInt64): TIdUInt64;
var
  LParts: TIdUInt64Parts;
  L: UInt32;
begin
  if (htonl(1) <> 1) then begin
    LParts.QuadPart := AValue;
    L := htonl(LParts.HighPart);
    LParts.HighPart := htonl(LParts.LowPart);
    LParts.LowPart := L;
    Result := LParts.QuadPart;
  end else begin
    Result := AValue;
  end;
end;

function TIdStackVCLPosix.IOControl(const s: TIdStackSocketHandle;
  const cmd: UInt32; var arg: UInt32): Integer;
begin
  Result := ioctl(s, cmd, @arg);
end;

procedure TIdStackVCLPosix.Listen(ASocket: TIdStackSocketHandle;
  ABackLog: Integer);
begin
  CheckForSocketError(Posix.SysSocket.listen(ASocket, ABacklog));
end;

function TIdStackVCLPosix.NetworkToHost(AValue: UInt32): UInt32;
begin
  Result := ntohl(AValue);
end;

function TIdStackVCLPosix.NetworkToHost(AValue: TIdUInt64): TIdUInt64;
var
  LParts: TIdUInt64Parts;
  L: UInt32;
begin
  if (ntohl(1) <> 1) then begin
    LParts.QuadPart := AValue;
    L := ntohl(LParts.HighPart);
    LParts.HighPart := ntohl(LParts.LowPart);
    LParts.LowPart := L;
    Result := LParts.QuadPart;
  end else begin
    Result := AValue;
  end;
end;

function TIdStackVCLPosix.NetworkToHost(AValue: UInt16): UInt16;
begin
   Result := ntohs(AValue);
end;

function TIdStackVCLPosix.ReadHostName: string;
const
  sMaxHostSize = 250;
var
  LStr: array[0..sMaxHostSize] of TIdAnsiChar;
  {$IFDEF USE_MARSHALLED_PTRS}
  LStrPtr: TPtrWrapper;
  {$ENDIF}
begin
  {$IFDEF USE_MARSHALLED_PTRS}
  LStrPtr := TPtrWrapper.Create(@LStr[0]);
  {$ENDIF}
  gethostname(
    {$IFDEF USE_MARSHALLED_PTRS}
    LStrPtr.ToPointer
    {$ELSE}
    LStr
    {$ENDIF}, sMaxHostSize);
  LStr[sMaxHostSize] := TIdAnsiChar(0);
  {$IFDEF USE_MARSHALLED_PTRS}
  Result := TMarshal.ReadStringAsAnsi(LStrPtr);
  {$ELSE}
  Result := String(LStr);
  {$ENDIF}
end;

function TIdStackVCLPosix.ReceiveMsg(ASocket: TIdStackSocketHandle;
  var VBuffer: TIdBytes; APkt: TIdPacketInfo): UInt32;
var
  LSize: socklen_t;
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;
  LMsg : msghdr;
  LIOV : iovec;
  LControl : TIdBytes;
  LCurCmsg : Pcmsghdr;   //for iterating through the control buffer
  LByte : PByte;

begin
  //we call the macro twice because we specified two possible structures.
  //Id_IPV6_HOPLIMIT and Id_IPV6_PKTINFO
  LSize := CMSG_LEN(CMSG_LEN(Length(VBuffer)));
  SetLength( LControl,LSize);

  LIOV.iov_len := Length(VBuffer); // Length(VMsgData);
  LIOV.iov_base := @VBuffer[0]; // @VMsgData[0];

  FillChar(LMsg,SizeOf(LMsg),0);

  LMsg.msg_iov := @LIOV;//lpBuffers := @LMsgBuf;
  LMsg.msg_iovlen := 1;

  LMsg.msg_controllen := LSize;
  LMsg.msg_control := @LControl[0];

  LMsg.msg_name := @LAddr;
  LMsg.msg_namelen := SizeOf(LAddrStore);

  Result := 0;
  CheckForSocketError(RecvMsg(ASocket, LMsg, Result));
  APkt.Reset;

  case LAddrStore.ss_family of
    Id_PF_INET4: begin
      APkt.SourceIP := TranslateTInAddrToString(LAddrIPv4.sin_addr, Id_IPv4);
      APkt.SourcePort := ntohs(LAddrIPv4.sin_port);
      APkt.SourceIPVersion := Id_IPv4;
    end;
    Id_PF_INET6: begin
      APkt.SourceIP := TranslateTInAddrToString(LAddrIPv6.sin6_addr, Id_IPv6);
      APkt.SourcePort := ntohs(LAddrIPv6.sin6_port);
      APkt.SourceIPVersion := Id_IPv6;
    end;
    else begin
      Result := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;

  LCurCmsg := nil;
  repeat
    LCurCmsg := CMSG_NXTHDR(@LMsg, LCurCmsg);
    if LCurCmsg = nil then begin
      break;
    end;
    case LCurCmsg^.cmsg_type of
      IPV6_PKTINFO :     //done this way because IPV6_PKTINF and IP_PKTINFO are both 19
      begin
        case LAddrStore.ss_family of
          Id_PF_INET4: begin
            {$IFDEF IOS}
            ToDo('PKTINFO not implemented for IPv4 under iOS yet');
            {$ELSE}
              {$IFNDEF DARWIN}
              //This is not supported in OS X.
              with Pin_pktinfo(CMSG_DATA(LCurCmsg))^ do begin
                APkt.DestIP := TranslateTInAddrToString(ipi_addr, Id_IPv4);
                APkt.DestIF := ipi_ifindex;
              end;
              APkt.DestIPVersion := Id_IPv4;
              {$ENDIF}
            {$ENDIF}
          end;
          Id_PF_INET6: begin
            with pin6_pktinfo(CMSG_DATA(LCurCmsg))^ do begin
              APkt.DestIP := TranslateTInAddrToString(ipi6_addr, Id_IPv6);
              APkt.DestIF :=  ipi6_ifindex;
            end;
            APkt.DestIPVersion := Id_IPv6;
          end;
        end;
      end;
      Id_IPV6_HOPLIMIT :
      begin
        LByte :=  PByte(CMSG_DATA(LCurCmsg));
        APkt.TTL := LByte^;
      end;
    end;
  until False;
end;

function TIdStackVCLPosix.RecvFrom(const ASocket: TIdStackSocketHandle;
  var VBuffer; const ALength, AFlags: Integer; var VIP: string;
  var VPort: TIdPort; var VIPVersion: TIdIPVersion): Integer;
var
  LiSize: socklen_t;
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;

begin
  LiSize := SizeOf(LAddrStore);
  // TODO: only include MSG_NOSIGNAL if SO_NOSIGPIPE is not enabled?
  Result := Posix.SysSocket.recvfrom(ASocket,VBuffer, ALength, AFlags or Id_MSG_NOSIGNAL, LAddr, LiSize);
  if Result >= 0 then
  begin
    case LAddrStore.ss_family of
      Id_PF_INET4: begin
        VIP := TranslateTInAddrToString(LAddrIPv4.sin_addr, Id_IPv4);
        VPort := ntohs(LAddrIPv4.sin_port);
        VIPVersion := Id_IPV4;
      end;
      Id_PF_INET6: begin
        VIP := TranslateTInAddrToString(LAddrIPv6.sin6_addr, Id_IPv6);
        VPort := ntohs(LAddrIPv6.sin6_port);
        VIPVersion := Id_IPV6;
      end;
      else begin
        Result := 0;
        IPVersionUnsupported;
      end;
    end;
  end;
end;

procedure TIdStackVCLPosix.SetBlocking(ASocket: TIdStackSocketHandle;
  const ABlocking: Boolean);
{
var
  LFlags: Integer;
}
begin
  // TODO: enable this
  {
  LFlags := CheckForSocketError(Posix.SysSocket.fcntl(ASocket, F_GETFL, 0));
  if ABlocking then begin
    LFlags := LFlags and not O_NONBLOCK;
  end else begin
    LFlags := LFlags or O_NONBLOCK;
  end;
  CheckForSocketError(Posix.SysSocket.fcntl(ASocket, F_SETFL, LFlags));
  }
  if not ABlocking then begin
    raise EIdNonBlockingNotSupported.Create(RSStackNonBlockingNotSupported);
  end;
end;

procedure TIdStackVCLPosix.SetLastError(const AError: Integer);
begin
  __error^ := AError;
end;

procedure TIdStackVCLPosix.{$IFDEF VCL_XE3_OR_ABOVE}GetSocketOption{$ELSE}WSGetSocketOption{$ENDIF}
  (ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption;
  var AOptVal; var AOptLen: Integer);
var
  LLen : socklen_t;
begin
  LLen := AOptLen;
  CheckForSocketError(Posix.SysSocket.getsockopt(ASocket, ALevel, AOptName, AOptVal, LLen));
  AOptLen := LLen;
end;

procedure TIdStackVCLPosix.{$IFDEF VCL_XE3_OR_ABOVE}SetSocketOption{$ELSE}WSSetSocketOption{$ENDIF}
  (ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption;
  const AOptVal; const AOptLen: Integer);
begin
  CheckForSocketError(Posix.SysSocket.setsockopt(ASocket, ALevel, AOptName, AOptVal, AOptLen));
end;

function TIdStackVCLPosix.SupportsIPv4: Boolean;
begin
  {$IFDEF IOS}
  // TODO: iOS 9+ is IPv6-only...
  //Result := ([[[UIDevice currentDevice] systemVersion] compare:'9.0' options:NSNumericSearch] == NSOrderedAscending);
  {$ENDIF}
  //In Windows, this does something else.  It checks the LSP's installed.
  Result := CheckIPVersionSupport(Id_IPv4);
end;

function TIdStackVCLPosix.SupportsIPv6: Boolean;
begin
  //In Windows, this does something else.  It checks the LSP's installed.
  Result := CheckIPVersionSupport(Id_IPv6);
end;

function TIdStackVCLPosix.WouldBlock(const AResult: Integer): Boolean;
begin
  //non-blocking does not exist in Linux, always indicate things will block
  Result := True;

  // TODO: enable this:
  //Result := (AResult in [EAGAIN, EWOULDBLOCK, EINPROGRESS]);
end;

procedure TIdStackVCLPosix.WriteChecksum(s: TIdStackSocketHandle;
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

procedure TIdStackVCLPosix.WriteChecksumIPv6(s: TIdStackSocketHandle;
  var VBuffer: TIdBytes; const AOffset: Integer; const AIP: String;
  const APort: TIdPort);
begin
//we simply request that the kernal write the checksum when the data
//is sent.  All of the parameters required are because Windows is bonked
//because it doesn't have the IPV6CHECKSUM socket option meaning we have
//to querry the network interface in TIdStackWindows -- yuck!!
  SetSocketOption(s, Id_IPPROTO_IPV6, IPV6_CHECKSUM, AOffset);
end;

function TIdStackVCLPosix.WSCloseSocket(ASocket: TIdStackSocketHandle): Integer;
begin
  Result := __close(ASocket);
end;

function TIdStackVCLPosix.WSGetLastError: Integer;
begin
  //IdStackWindows just uses   result := WSAGetLastError;
  Result := GetLastError; //System.GetLastOSError; - FPC doesn't define it in System
  if Result = Id_WSAEPIPE then begin
    Result := Id_WSAECONNRESET;
  end;
end;

function TIdStackVCLPosix.WSGetServByName(const AServiceName: string): TIdPort;
var
  Lps: PServEnt;
  {$IFDEF USE_MARSHALLED_PTRS}
  M: TMarshaller;
  {$ENDIF}
begin
  Lps := Posix.NetDB.getservbyname(
    {$IFDEF USE_MARSHALLED_PTRS}
    M.AsAnsi(AServiceName).ToPointer
    {$ELSE}
    PAnsiChar(
      {$IFDEF STRING_IS_ANSI}
      AServiceName
      {$ELSE}
      AnsiString(AServiceName) // explicit convert to Ansi
      {$ENDIF}
    )
    {$ENDIF},
    nil);
  if Lps <> nil then begin
    Result := ntohs(Lps^.s_port);
  end else begin
    try
      Result := IndyStrToInt(AServiceName);
    except
      on EConvertError do begin
        Result := 0;
        IndyRaiseOuterException(EIdInvalidServiceName.CreateFmt(RSInvalidServiceName, [AServiceName]));
      end;
    end;
  end;
end;

procedure TIdStackVCLPosix.AddServByPortToList(const APortNumber: TIdPort; AAddresses: TStrings);
//function TIdStackVCLPosix.WSGetServByPort(const APortNumber: TIdPort): TStrings;
type
  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = packed array[0..(MaxInt div SizeOf(PIdAnsiChar))-1] of PIdAnsiChar;
var
  Lps: PServEnt;
  Li: Integer;
  Lp: PPAnsiCharArray;
begin
  Lps := Posix.NetDB.getservbyport(htons(APortNumber), nil);
  if Lps <> nil then begin
    AAddresses.BeginUpdate;
    try
      AAddresses.Add(String(Lps^.s_name));
      Li := 0;
      Lp := Pointer(Lps^.s_aliases);
      while Lp[Li] <> nil do begin
        AAddresses.Add(String(Lp[Li]));
        Inc(Li);
      end;
    finally
      AAddresses.EndUpdate;
    end;
  end;
end;

function TIdStackVCLPosix.WSRecv(ASocket: TIdStackSocketHandle; var ABuffer;
  const ABufferLength, AFlags: Integer): Integer;
begin
  //IdStackWindows is just: Result := Recv(ASocket, ABuffer, ABufferLength, AFlags);
  // TODO: only include MSG_NOSIGNAL if SO_NOSIGPIPE is not enabled?
  Result := Posix.SysSocket.Recv(ASocket, ABuffer, ABufferLength, AFlags or Id_MSG_NOSIGNAL);
end;

function TIdStackVCLPosix.WSSend(ASocket: TIdStackSocketHandle; const ABuffer;
  const ABufferLength, AFlags: Integer): Integer;
begin
  // TODO: only include MSG_NOSIGNAL if SO_NOSIGPIPE is not enabled?
  Result := CheckForSocketError(Posix.SysSocket.send(ASocket, ABuffer, ABufferLength, AFlags or Id_MSG_NOSIGNAL));
end;

procedure TIdStackVCLPosix.WSSendTo(ASocket: TIdStackSocketHandle;
  const ABuffer; const ABufferLength, AFlags: Integer; const AIP: string;
  const APort: TIdPort; AIPVersion: TIdIPVersion);
var
  LAddrStore: sockaddr_storage;
  LAddrIPv4 : SockAddr_In absolute LAddrStore;
  LAddrIPv6 : sockaddr_in6 absolute LAddrStore;
  LAddr : sockaddr absolute LAddrStore;
  LiSize: socklen_t;
  LBytesSent: Integer;
begin
  case AIPVersion of
    Id_IPv4: begin
      InitSockAddr_In(LAddrIPv4);
      TranslateStringToTInAddr(AIP, LAddrIPv4.sin_addr, Id_IPv4);
      LAddrIPv4.sin_port := htons(APort);
      LiSize := SizeOf(LAddrIPv4);
    end;
    Id_IPv6: begin
      InitSockAddr_in6(LAddrIPv6);
      TranslateStringToTInAddr(AIP, LAddrIPv6.sin6_addr, Id_IPv6);
      LAddrIPv6.sin6_port := htons(APort);
      LiSize := SizeOf(LAddrIPv6);
    end;
  else
    LiSize := 0; // avoid warning
    IPVersionUnsupported;
  end;
  // TODO: only include MSG_NOSIGNAL if SO_NOSIGPIPE is not enabled?
  LBytesSent := Posix.SysSocket.sendto(
    ASocket, ABuffer, ABufferLength, AFlags or Id_MSG_NOSIGNAL, LAddr, LiSize);
  if LBytesSent = Id_SOCKET_ERROR then begin
    // TODO: move this into RaiseLastSocketError directly
    if WSGetLastError() = Id_WSAEMSGSIZE then begin
      raise EIdPackageSizeTooBig.Create(RSPackageSizeTooBig);
    end else begin
      RaiseLastSocketError;
    end;
  end
  else if LBytesSent <> ABufferLength then begin
    raise EIdNotAllBytesSent.Create(RSNotAllBytesSent);
  end;

end;

procedure TIdStackVCLPosix.WSSetLastError(const AErr: Integer);
begin
  __error^ := AErr;
end;

function TIdStackVCLPosix.WSShutdown(ASocket: TIdStackSocketHandle;
  AHow: Integer): Integer;
begin
  Result := Posix.SysSocket.shutdown(ASocket, AHow);
end;

function TIdStackVCLPosix.WSSocket(AFamily : Integer; AStruct : TIdSocketType; AProtocol: Integer;
      const AOverlapped: Boolean = False): TIdStackSocketHandle;
begin
  Result := Posix.SysSocket.socket(AFamily, AStruct, AProtocol);
  {$IFDEF HAS_SOCKET_NOSIGPIPE}
  if Result <> INVALID_SOCKET then begin
    SetSocketOption(Result, SOL_SOCKET, SO_NOSIGPIPE, 1);
  end;
  {$ENDIF}
end;

{$I IdUnitPlatformOn.inc}
{$I IdSymbolPlatformOn.inc}
initialization
  GSocketListClass := TIdSocketListVCLPosix;
end.

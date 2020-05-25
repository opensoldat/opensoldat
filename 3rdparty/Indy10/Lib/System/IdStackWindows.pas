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


  $Log$


   Rev 1.8    10/26/2004 8:20:04 PM  JPMugaas
 Fixed some oversights with conversion.  OOPS!!!


   Rev 1.7    07/06/2004 21:31:24  CCostelloe
 Kylix 3 changes


   Rev 1.6    4/18/04 10:43:24 PM  RLebeau
 Fixed syntax error


   Rev 1.5    4/18/04 10:29:58 PM  RLebeau
 Renamed Int64Parts structure to TIdInt64Parts


   Rev 1.4    4/18/04 2:47:46 PM  RLebeau
 Conversion support for Int64 values


   Rev 1.3    2004.03.07 11:45:28 AM  czhower
 Flushbuffer fix + other minor ones found


   Rev 1.2    3/6/2004 5:16:34 PM  JPMugaas
 Bug 67 fixes.  Do not write to const values.


   Rev 1.1    3/6/2004 4:23:52 PM  JPMugaas
 Error #62 fix.  This seems to work in my tests.


   Rev 1.0    2004.02.03 3:14:48 PM  czhower
 Move and updates


   Rev 1.33    2/1/2004 6:10:56 PM  JPMugaas
 GetSockOpt.


   Rev 1.32    2/1/2004 3:28:36 AM  JPMugaas
 Changed WSGetLocalAddress to GetLocalAddress and moved into IdStack since
 that will work the same in the DotNET as elsewhere.  This is required to
 reenable IPWatch.


   Rev 1.31    1/31/2004 1:12:48 PM  JPMugaas
 Minor stack changes required as DotNET does support getting all IP addresses
 just like the other stacks.


   Rev 1.30    12/4/2003 3:14:52 PM  BGooijen
 Added HostByAddress


   Rev 1.29    1/3/2004 12:38:56 AM  BGooijen
 Added function SupportsIPv6


   Rev 1.28    12/31/2003 9:52:02 PM  BGooijen
 Added IPv6 support


   Rev 1.27    10/26/2003 05:33:14 PM  JPMugaas
 LocalAddresses should work.


   Rev 1.26    10/26/2003 5:04:28 PM  BGooijen
 UDP Server and Client


   Rev 1.25    10/26/2003 09:10:26 AM  JPMugaas
 Calls necessary for IPMulticasting.


   Rev 1.24    10/22/2003 04:40:52 PM  JPMugaas
 Should compile with some restored functionality.  Still not finished.


   Rev 1.23    10/21/2003 11:04:20 PM  BGooijen
 Fixed name collision


   Rev 1.22    10/21/2003 01:20:02 PM  JPMugaas
 Restore GWindowsStack because it was needed by SuperCore.


   Rev 1.21    10/21/2003 06:24:28 AM  JPMugaas
 BSD Stack now have a global variable for refercing by platform specific
 things.  Removed corresponding var from Windows stack.


   Rev 1.20    10/19/2003 5:21:32 PM  BGooijen
 SetSocketOption


   Rev 1.19    2003.10.11 5:51:16 PM  czhower
 -VCL fixes for servers
 -Chain suport for servers (Super core)
 -Scheduler upgrades
 -Full yarn support


   Rev 1.18    2003.10.02 8:01:08 PM  czhower
 .Net


   Rev 1.17    2003.10.02 12:44:44 PM  czhower
 Fix for Bind, Connect


   Rev 1.16    2003.10.02 10:16:32 AM  czhower
 .Net


   Rev 1.15    2003.10.01 9:11:26 PM  czhower
 .Net


   Rev 1.14    2003.10.01 12:30:08 PM  czhower
 .Net


   Rev 1.12    10/1/2003 12:14:12 AM  BGooijen
 DotNet: removing CheckForSocketError


   Rev 1.11    2003.10.01 1:12:40 AM  czhower
 .Net


   Rev 1.10    2003.09.30 1:23:04 PM  czhower
 Stack split for DotNet


   Rev 1.9    9/8/2003 02:13:10 PM  JPMugaas
 SupportsIP6 function added for determining if IPv6 is installed on a system.


   Rev 1.8    2003.07.14 1:57:24 PM  czhower
 -First set of IOCP fixes.
 -Fixed a threadsafe problem with the stack class.


   Rev 1.7    7/1/2003 05:20:44 PM  JPMugaas
 Minor optimizations.  Illiminated some unnecessary string operations.


   Rev 1.5    7/1/2003 03:39:58 PM  JPMugaas
 Started numeric IP function API calls for more efficiency.


   Rev 1.4    7/1/2003 12:46:06 AM  JPMugaas
 Preliminary stack functions taking an IP address numerical structure instead
 of a string.


    Rev 1.3    5/19/2003 6:00:28 PM  BGooijen
  TIdStackWindows.WSGetHostByAddr raised an ERangeError when the last number in
  the ip>127


    Rev 1.2    5/10/2003 4:01:28 PM  BGooijen


   Rev 1.1    2003.05.09 10:59:28 PM  czhower


   Rev 1.0    11/13/2002 08:59:38 AM  JPMugaas
}
unit IdStackWindows;

interface

{$I IdCompilerDefines.inc}

uses
  Classes,
  IdGlobal, IdException, IdStackBSDBase, IdStackConsts, IdWinsock2, IdStack,
  SysUtils, 
  Windows;

type
  EIdIPv6Unavailable = class(EIdException);

  TIdSocketListWindows = class(TIdSocketList)
  protected
    FFDSet: TFDSet;
    //
    class function FDSelect(AReadSet: PFDSet; AWriteSet: PFDSet; AExceptSet: PFDSet;
     const ATimeout: Integer = IdTimeoutInfinite): Boolean;
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
    function SelectRead(const ATimeout: Integer = IdTimeoutInfinite): Boolean; override;
    function SelectReadList(var VSocketList: TIdSocketList;
      const ATimeout: Integer = IdTimeoutInfinite): Boolean; override;
  end;

  TIdStackWindows = class(TIdStackBSDBase)
  protected
     procedure WSQuerryIPv6Route(ASocket: TIdStackSocketHandle;
       const AIP: String; const APort : UInt16; var VSource; var VDest);
    procedure WriteChecksumIPv6(s : TIdStackSocketHandle; var VBuffer : TIdBytes;
      const AOffset : Integer; const AIP : String; const APort : TIdPort);
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
    function Accept(ASocket: TIdStackSocketHandle; var VIP: string; var VPort: TIdPort;
      var VIPVersion: TIdIPVersion): TIdStackSocketHandle; override;
    function HostToNetwork(AValue: UInt16): UInt16; override;
    function HostToNetwork(AValue: UInt32): UInt32; override;
    function HostToNetwork(AValue: TIdUInt64): TIdUInt64; override;
    procedure Listen(ASocket: TIdStackSocketHandle; ABackLog: Integer); override;
    function NetworkToHost(AValue: UInt16): UInt16; override;
    function NetworkToHost(AValue: UInt32): UInt32; override;
    function NetworkToHost(AValue: TIdUInt64): TIdUInt64; override;
    procedure SetBlocking(ASocket: TIdStackSocketHandle; const ABlocking: Boolean); override;
    function WouldBlock(const AResult: Integer): Boolean; override;
    //
    function HostByAddress(const AAddress: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string; override;

    function WSGetServByName(const AServiceName: string): TIdPort; override;
    procedure AddServByPortToList(const APortNumber: TIdPort; AAddresses: TStrings); override;

    function RecvFrom(const ASocket: TIdStackSocketHandle; var VBuffer;
     const ALength, AFlags: Integer; var VIP: string; var VPort: TIdPort;
     var VIPVersion: TIdIPVersion): Integer; override;
   function ReceiveMsg(ASocket: TIdStackSocketHandle; var VBuffer: TIdBytes;
      APkt : TIdPacketInfo): UInt32; override;

    procedure WSSendTo(ASocket: TIdStackSocketHandle; const ABuffer;
      const ABufferLength, AFlags: Integer; const AIP: string; const APort: TIdPort; AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;

    function WSSocket(AFamily : Integer; AStruct : TIdSocketType; AProtocol: Integer;
      const AOverlapped: Boolean = False): TIdStackSocketHandle; override;
    function WSTranslateSocketErrorMsg(const AErr: integer): string; override;
    function WSGetLastError: Integer; override;
    procedure WSSetLastError(const AErr : Integer); override;
    //
    procedure Bind(ASocket: TIdStackSocketHandle; const AIP: string;
     const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    procedure Connect(const ASocket: TIdStackSocketHandle; const AIP: string;
     const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Disconnect(ASocket: TIdStackSocketHandle); override;
    procedure GetPeerName(ASocket: TIdStackSocketHandle; var VIP: string;
     var VPort: TIdPort; var VIPVersion: TIdIPVersion); override;
    procedure GetSocketName(ASocket: TIdStackSocketHandle; var VIP: string;
     var VPort: TIdPort; var VIPVersion: TIdIPVersion); override;
    {$IFDEF VCL_XE3_OR_ABOVE}
    procedure GetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; var AOptVal; var AOptLen: Integer); override;
    procedure SetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; const AOptVal; const AOptLen: Integer); override;
    {$ENDIF}
    function IOControl(const s:  TIdStackSocketHandle; const cmd: UInt32; var arg: UInt32): Integer; override;
    function SupportsIPv4: Boolean; override;
    function SupportsIPv6: Boolean; override;
    function CheckIPVersionSupport(const AIPVersion: TIdIPVersion): boolean; override;
    procedure WriteChecksum(s : TIdStackSocketHandle;
       var VBuffer : TIdBytes;
      const AOffset : Integer;
      const AIP : String;
      const APort : TIdPort;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    procedure GetLocalAddressList(AAddresses: TIdStackLocalAddressList); override;
    procedure SetKeepAliveValues(ASocket: TIdStackSocketHandle;
      const AEnabled: Boolean; const ATimeMS, AInterval: Integer); override;
  end;

var
//This is for the Win32-only package (SuperCore)
  GWindowsStack : TIdStackWindows = nil;

implementation

{$DEFINE USE_IPHLPAPI}

{$IFDEF USE_IPHLPAPI}
  {$IFDEF VCL_XE2_OR_ABOVE}
    {$DEFINE HAS_UNIT_IpTypes}
    {$DEFINE HAS_UNIT_IpHlpApi}
  {$ENDIF}
{$ENDIF}

uses
  IdIDN, IdResourceStrings, IdWship6
  {$IFDEF USE_IPHLPAPI}
    {$IFDEF HAS_UNIT_IpTypes}
  , IpTypes
    {$ENDIF}
    {$IFDEF HAS_UNIT_IpHlpApi}
  , IpHlpApi
    {$ENDIF}
  {$ENDIF}
  ;

{$IFNDEF WINCE}
type
  TGetFileSizeEx = function(hFile : THandle; var lpFileSize : LARGE_INTEGER) : BOOL; stdcall;
{$ENDIF}

const
  SIZE_HOSTNAME = 250;

var
  GStarted: Boolean = False;
  {$IFNDEF WINCE}
  GetFileSizeEx : TGetFileSizeEx = nil;
  {$ENDIF}

{ IPHLPAPI support }

{$IFDEF USE_IPHLPAPI}

const
  IPHLPAPI_DLL = 'iphlpapi.dll';
  {$IFNDEF HAS_UNIT_IpTypes}
  MAX_ADAPTER_DESCRIPTION_LENGTH  = 128;
  MAX_ADAPTER_NAME_LENGTH         = 256;
  MAX_ADAPTER_ADDRESS_LENGTH      = 8;
  MAX_DHCPV6_DUID_LENGTH          = 130;
  MAX_DNS_SUFFIX_STRING_LENGTH    = 256;
  GAA_FLAG_SKIP_UNICAST           = $0001;
  GAA_FLAG_SKIP_ANYCAST           = $0002;
  GAA_FLAG_SKIP_MULTICAST         = $0004;
  GAA_FLAG_SKIP_DNS_SERVER        = $0008;
  GAA_FLAG_INCLUDE_PREFIX         = $0010;
  GAA_FLAG_SKIP_FRIENDLY_NAME     = $0020;
  IP_ADAPTER_RECEIVE_ONLY         = $08;
  {$ENDIF}
  IF_TYPE_SOFTWARE_LOOPBACK       = 24;

type
  PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS = ^IP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
  IP_UNIDIRECTIONAL_ADAPTER_ADDRESS = record
    NumAdapters: ULONG;
    Address: array[0..0] of TInAddr;
  end;

  {$IFNDEF HAS_UNIT_IpTypes}
  {$MINENUMSIZE 4}

  time_t                  = TIdNativeInt;
  IFTYPE                  = ULONG;
  IF_INDEX                = ULONG;
  NET_IF_COMPARTMENT_ID   = UINT32;
  NET_IF_NETWORK_GUID     = TGUID;

  IP_PREFIX_ORIGIN = (
    IpPrefixOriginOther,
    IpPrefixOriginManual,
    IpPrefixOriginWellKnown,
    IpPrefixOriginDhcp,
    IpPrefixOriginRouterAdvertisement,
    {$IFNDEF HAS_ENUM_ELEMENT_VALUES}
    ippoUnused5,
    ippoUnused6,
    ippoUnused7,
    ippoUnused8,
    ippoUnused9,
    ippoUnused10,
    ippoUnused11,
    ippoUnused12,
    ippoUnused13,
    ippoUnused14,
    ippoUnused15,
    {$ENDIF}
    IpPrefixOriginUnchanged);

  IP_SUFFIX_ORIGIN = (
    IpSuffixOriginOther,
    IpSuffixOriginManual,
    IpSuffixOriginWellKnown,
    IpSuffixOriginDhcp,
    IpSuffixOriginLinkLayerAddress,
    IpSuffixOriginRandom,
    {$IFNDEF HAS_ENUM_ELEMENT_VALUES}
    ipsoUnued6,
    ipsoUnued7,
    ipsoUnued8,
    ipsoUnued9,
    ipsoUnued10,
    ipsoUnued11,
    ipsoUnued12,
    ipsoUnued13,
    ipsoUnued14,
    ipsoUnued15,
    {$ENDIF}
    IpSuffixOriginUnchanged);

  IP_DAD_STATE = (
    IpDadStateInvalid,
    IpDadStateTentative,
    IpDadStateDuplicate,
    IpDadStateDeprecated,
    IpDadStatePreferred);

  IF_OPER_STATUS = (
    {$IFNDEF HAS_ENUM_ELEMENT_VALUES}
    ifosUnused,
    IfOperStatusUp,
    {$ELSE}
    IfOperStatusUp = 1,
    {$ENDIF}
    IfOperStatusDown,
    IfOperStatusTesting,
    IfOperStatusUnknown,
    IfOperStatusDormant,
    IfOperStatusNotPresent,
    IfOperStatusLowerLayerDown);

  NET_IF_CONNECTION_TYPE = (
    {$IFNDEF HAS_ENUM_ELEMENT_VALUES}
    nictUnused,
    NetIfConnectionDedicated,
    {$ELSE}
    NetIfConnectionDedicated = 1,
    {$ENDIF}
    NetIfConnectionPassive,
    NetIfConnectionDemand,
    NetIfConnectionMaximum);

  TUNNEL_TYPE = (
    TunnelTypeNone,
    TunnelTypeOther,
    TunnelTypeDirect,
    TunnelType6To4,
    TunnelTypeIsatap,
    TunnelTypeTeredo,
    TunnelTypeIPHTTPS);

  IP_ADDRESS_STRING = record
    S: array [0..15] of AnsiChar;
  end;
  IP_MASK_STRING = IP_ADDRESS_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  IP_ADDR_STRING = record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  IP_ADAPTER_INFO = record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of AnsiChar;
    Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of AnsiChar;
    AddressLength: UINT;
    Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    Index: DWORD;
    Type_: UINT;
    DhcpEnabled: UINT;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: BOOL;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: time_t;
    LeaseExpires: time_t;
  end;

  SOCKET_ADDRESS = record
    lpSockaddr: IdWinsock2.LPSOCKADDR;
    iSockaddrLength: Integer;
  end;

  PIP_ADAPTER_UNICAST_ADDRESS = ^IP_ADAPTER_UNICAST_ADDRESS;
  IP_ADAPTER_UNICAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_UNICAST_ADDRESS;
    Address: SOCKET_ADDRESS;
    PrefixOrigin: IP_PREFIX_ORIGIN;
    SuffixOrigin: IP_SUFFIX_ORIGIN;
    DadState: IP_DAD_STATE;
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    LeaseLifetime: ULONG;

    // This structure member is only available on Windows Vista and later
    OnLinkPrefixLength: UCHAR;
  end;

  PIP_ADAPTER_ANYCAST_ADDRESS = ^IP_ADAPTER_ANYCAST_ADDRESS;
  IP_ADAPTER_ANYCAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_ANYCAST_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;

  PIP_ADAPTER_MULTICAST_ADDRESS = ^IP_ADAPTER_MULTICAST_ADDRESS;
  IP_ADAPTER_MULTICAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_MULTICAST_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;

  PIP_ADAPTER_DNS_SERVER_ADDRESS = ^IP_ADAPTER_DNS_SERVER_ADDRESS;
  IP_ADAPTER_DNS_SERVER_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Reserved: DWORD);
    end;
    Next: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;

  PIP_ADAPTER_PREFIX = ^IP_ADAPTER_PREFIX;
  IP_ADAPTER_PREFIX = record
    Union: record
    case Integer of
      0: (
        Alignment: ULONGLONG);
      1: (
        Length: ULONG;
        Flags: DWORD);
    end;
    Next: PIP_ADAPTER_PREFIX;
    Address: SOCKET_ADDRESS;
    PrefixLength: ULONG;
  end;

  PIP_ADAPTER_WINS_SERVER_ADDRESS_LH = ^IP_ADAPTER_WINS_SERVER_ADDRESS_LH;
  IP_ADAPTER_WINS_SERVER_ADDRESS_LH = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Reserved: DWORD);
    end;
    Next: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
    Address: SOCKET_ADDRESS;
  end;

  PIP_ADAPTER_GATEWAY_ADDRESS_LH = ^IP_ADAPTER_GATEWAY_ADDRESS_LH;
  IP_ADAPTER_GATEWAY_ADDRESS_LH = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Reserved: DWORD);
    end;
    Next: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
    Address: SOCKET_ADDRESS;
  end;

  IF_LUID = record
    case Integer of
      0: (
          Value: ULONG64);
      1: (
          Info: ULONG64);
  end;

  PIP_ADAPTER_DNS_SUFFIX = ^IP_ADAPTER_DNS_SUFFIX;
  IP_ADAPTER_DNS_SUFFIX = record
    Next: PIP_ADAPTER_DNS_SUFFIX;
    AString: array[0..MAX_DNS_SUFFIX_STRING_LENGTH - 1] of WCHAR;
  end;

  PIP_ADAPTER_ADDRESSES = ^IP_ADAPTER_ADDRESSES;
  IP_ADAPTER_ADDRESSES = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          IfIndex: DWORD);
    end;
    Next: PIP_ADAPTER_ADDRESSES;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
    FirstAnycastAddress: PIP_ADAPTER_ANYCAST_ADDRESS;
    FirstMulticastAddress: PIP_ADAPTER_MULTICAST_ADDRESS;
    FirstDnsServerAddress: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    DnsSuffix: PWCHAR;
    Description: PWCHAR;
    FriendlyName: PWCHAR;
    PhysicalAddress: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    PhysicalAddressLength: DWORD;
    Flags: DWORD;
    Mtu: DWORD;
    IfType: IFTYPE;
    OperStatus: IF_OPER_STATUS;
    Ipv6IfIndex: IF_INDEX;
    ZoneIndices: array [0..15] of DWORD;
    FirstPrefix: PIP_ADAPTER_PREFIX;
    TransmitLinkSpeed: ULONG64;
    ReceiveLinkSpeed: ULONG64;
    FirstWinsServerAddress: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
    FirstGatewayAddress: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: IF_LUID;
    Dhcpv4Server: SOCKET_ADDRESS;
    CompartmentId: NET_IF_COMPARTMENT_ID;
    NetworkGuid: NET_IF_NETWORK_GUID;
    ConnectionType: NET_IF_CONNECTION_TYPE;
    TunnelType: TUNNEL_TYPE;
    //
    // DHCP v6 Info.
    //
    Dhcpv6Server: SOCKET_ADDRESS;
    Dhcpv6ClientDuid: array [0..MAX_DHCPV6_DUID_LENGTH - 1] of Byte;
    Dhcpv6ClientDuidLength: ULONG;
    Dhcpv6Iaid: ULONG;
    FirstDnsSuffix: PIP_ADAPTER_DNS_SUFFIX;
  end;

  {$ENDIF}

  PMIB_IPADDRROW = ^MIB_IPADDRROW;
  MIB_IPADDRROW = record
    dwAddr: DWORD;
    dwIndex: DWORD;
    dwMask: DWORD;
    dwBCastAddr: DWORD;
    dwReasmSize: DWORD;
    unused1: Word;
    wType: Word;
  end;

  PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;
  MIB_IPADDRTABLE = record
    dwNumEntries: DWORD;
    table: array[0..0] of MIB_IPADDRROW;
  end;

  NETIO_STATUS = DWORD;

  TGetIpAddrTable = function(pIpAddrTable: PMIB_IPADDRTABLE; var pdwSize: ULONG; bOrder: BOOL): DWORD; stdcall;
  TGetUniDirectionalAdapterInfo = function(pIPIfInfo: PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS; var dwOutBufLen: ULONG): DWORD; stdcall;
  TGetAdaptersInfo = function(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall;
  TGetAdaptersAddresses = function(Family: ULONG; Flags: DWORD; Reserved: PVOID; pAdapterAddresses: PIP_ADAPTER_ADDRESSES; var OutBufLen: ULONG): DWORD; stdcall;
  TConvertLengthToIpv4Mask = function(MaskLength: ULONG; var Mask: ULONG): NETIO_STATUS; stdcall;

var
  hIpHlpApi: THandle = 0;
  GetIpAddrTable: TGetIpAddrTable = nil;
  GetUniDirectionalAdapterInfo: TGetUniDirectionalAdapterInfo = nil;
  GetAdaptersInfo: TGetAdaptersInfo = nil;
  GetAdaptersAddresses: TGetAdaptersAddresses = nil;
  ConvertLengthToIpv4Mask: TConvertLengthToIpv4Mask = nil;

function FixupIPHelperStub(const AName:{$IFDEF WINCE}TIdUnicodeString{$ELSE}string{$ENDIF}; DefImpl: Pointer): Pointer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := nil;
  if hIpHlpApi <> 0 then begin
    Result := Windows.GetProcAddress(hIpHlpApi, {$IFDEF WINCE}PWideChar{$ELSE}PChar{$ENDIF}(AName));
  end;
  if Result = nil then begin
    Result := DefImpl;
  end;
end;

function Impl_GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE; var pdwSize: ULONG; bOrder: BOOL): DWORD; stdcall;
begin
  pdwSize := 0;
  Result := ERROR_NOT_SUPPORTED;
end;

function Stub_GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE; var pdwSize: ULONG; bOrder: BOOL): DWORD; stdcall;
begin
  @GetIpAddrTable := FixupIPHelperStub('GetIpAddrTable', @Impl_GetIpAddrTable); {Do not localize}
  Result := GetIpAddrTable(pIpAddrTable, pdwSize, bOrder);
end;

function Impl_GetUniDirectionalAdapterInfo(pIPIfInfo: PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS; var dwOutBufLen: ULONG): DWORD; stdcall;
begin
  dwOutBufLen := 0;
  Result := ERROR_NOT_SUPPORTED;
end;

function Stub_GetUniDirectionalAdapterInfo(pIPIfInfo: PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS; var dwOutBufLen: ULONG): DWORD; stdcall;
begin
  @GetUniDirectionalAdapterInfo := FixupIPHelperStub('GetUniDirectionalAdapterInfo', @Impl_GetUniDirectionalAdapterInfo); {Do not localize}
  Result := GetUniDirectionalAdapterInfo(pIPIfInfo, dwOutBufLen);
end;

function Impl_GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall;
begin
  pOutBufLen := 0;
  Result := ERROR_NOT_SUPPORTED;
end;

function Stub_GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall;
begin
  @GetAdaptersInfo := FixupIPHelperStub('GetAdaptersInfo', @Impl_GetAdaptersInfo); {Do not localize}
  Result := GetAdaptersInfo(pAdapterInfo, pOutBufLen);
end;

function Impl_GetAdaptersAddresses(Family: ULONG; Flags: DWORD; Reserved: PVOID; pAdapterAddresses: PIP_ADAPTER_ADDRESSES; var OutBufLen: ULONG): DWORD; stdcall;
begin
  OutBufLen := 0;
  Result := ERROR_NOT_SUPPORTED;
end;

function Stub_GetAdaptersAddresses(Family: ULONG; Flags: DWORD; Reserved: PVOID; pAdapterAddresses: PIP_ADAPTER_ADDRESSES; var OutBufLen: ULONG): DWORD; stdcall;
begin
  @GetAdaptersAddresses := FixupIPHelperStub('GetAdaptersAddresses', @Impl_GetAdaptersAddresses); {Do not localize}
  Result := GetAdaptersAddresses(Family, Flags, Reserved, pAdapterAddresses, OutBufLen);
end;

function Impl_ConvertLengthToIpv4Mask(MaskLength: ULONG; var Mask: ULONG): NETIO_STATUS; stdcall;
begin
  // TODO: implement manually
  Mask := INADDR_NONE;
  if MaskLength > 32 then begin
    Result := ERROR_INVALID_PARAMETER;
  end else begin
    Result := ERROR_NOT_SUPPORTED;
  end;
end;

function Stub_ConvertLengthToIpv4Mask(MaskLength: ULONG; var Mask: ULONG): NETIO_STATUS; stdcall;
begin
  @ConvertLengthToIpv4Mask := FixupIPHelperStub('ConvertLengthToIpv4Mask', @Impl_ConvertLengthToIpv4Mask); {Do not localize}
  Result := ConvertLengthToIpv4Mask(MaskLength, Mask);
end;

procedure InitializeIPHelperStubs;
begin
  GetIpAddrTable := Stub_GetIpAddrTable;
  GetUniDirectionalAdapterInfo := Stub_GetUniDirectionalAdapterInfo;
  GetAdaptersInfo := Stub_GetAdaptersInfo;
  GetAdaptersAddresses := Stub_GetAdaptersAddresses;
  ConvertLengthToIpv4Mask := Stub_ConvertLengthToIpv4Mask;
end;

procedure InitializeIPHelperAPI;
begin
  if hIpHlpApi = 0 then begin
    hIpHlpApi := SafeLoadLibrary(IPHLPAPI_DLL);
  end;
end;

procedure UninitializeIPHelperAPI;
begin
  if hIpHlpApi <> 0 then
  begin
    FreeLibrary(hIpHlpApi);
    hIpHlpApi := 0;
  end;
  InitializeIPHelperStubs;
end;

{$ENDIF}

{ TIdStackWindows }

constructor TIdStackWindows.Create;
begin
  inherited Create;
  if not GStarted then begin
    try
      InitializeWinSock;
      IdWship6.InitLibrary;
      IdIDN.InitIDNLibrary;
      {$IFDEF USE_IPHLPAPI}
      InitializeIPHelperAPI;
      {$ENDIF}
    except
      on E: Exception do begin
        IndyRaiseOuterException(EIdStackInitializationFailed.Create(E.Message));
      end;
    end;
    GStarted := True;
  end;
  GWindowsStack := Self;
end;

destructor TIdStackWindows.Destroy;
begin
  //DLL Unloading and Cleanup is done at finalization
  inherited Destroy;
end;

function TIdStackWindows.Accept(ASocket: TIdStackSocketHandle;
  var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion): TIdStackSocketHandle;
var
  LSize: Integer;
  LAddr: SOCKADDR_STORAGE;
begin
  LSize := SizeOf(LAddr);
  Result := IdWinsock2.accept(ASocket, IdWinsock2.PSOCKADDR(@LAddr), @LSize);
  if Result <> INVALID_SOCKET then begin
    case LAddr.ss_family of
      Id_PF_INET4: begin
        VIP := TranslateTInAddrToString(PSockAddrIn(@LAddr)^.sin_addr, Id_IPv4);
        VPort := ntohs(PSockAddrIn(@LAddr)^.sin_port);
        VIPVersion := Id_IPv4;
      end;
      Id_PF_INET6: begin
        VIP := TranslateTInAddrToString(PSockAddrIn6(@LAddr)^.sin6_addr, Id_IPv6);
        VPort := ntohs(PSockAddrIn6(@LAddr)^.sin6_port);
        VIPVersion := Id_IPv6;
      end;
      else begin
        CloseSocket(Result);
        Result := INVALID_SOCKET;
        IPVersionUnsupported;
      end;
    end;
  end;
end;

procedure TIdStackWindows.Bind(ASocket: TIdStackSocketHandle;
  const AIP: string; const APort: TIdPort;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LAddr: SOCKADDR_STORAGE;
  LSize: Integer;
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  case AIPVersion of
    Id_IPv4: begin
      PSockAddrIn(@LAddr)^.sin_family := Id_PF_INET4;
      if AIP <> '' then begin
        TranslateStringToTInAddr(AIP, PSockAddrIn(@LAddr)^.sin_addr, Id_IPv4);
      end;
      PSockAddrIn(@LAddr)^.sin_port := htons(APort);
      LSize := SIZE_TSOCKADDRIN;
    end;
    Id_IPv6: begin
      PSockAddrIn6(@LAddr)^.sin6_family := Id_PF_INET6;
      if AIP <> '' then begin
        TranslateStringToTInAddr(AIP, PSockAddrIn6(@LAddr)^.sin6_addr, Id_IPv6);
      end;
      PSockAddrIn6(@LAddr)^.sin6_port := htons(APort);
      LSize := SIZE_TSOCKADDRIN6;
    end;
    else begin
      LSize := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;
  CheckForSocketError(IdWinsock2.bind(ASocket, IdWinsock2.PSOCKADDR(@LAddr), LSize));
end;

function TIdStackWindows.WSCloseSocket(ASocket: TIdStackSocketHandle): Integer;
begin
  Result := CloseSocket(ASocket);
end;

function TIdStackWindows.HostByAddress(const AAddress: string;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string;
var
  {$IFDEF UNICODE}
  Hints: TAddrInfoW;
  LAddrInfo: pAddrInfoW;
  {$ELSE}
  Hints: TAddrInfo;
  LAddrInfo: pAddrInfo;
  {$ENDIF}
  RetVal: Integer;
  {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp: TIdPlatformString;
  {$ENDIF}
begin
  if not (AIPVersion in [Id_IPv4, Id_IPv6]) then begin
    IPVersionUnsupported;
  end;

  // TODO: should this be calling getnameinfo() first and then getaddrinfo()
  // to check for a malicious PTR record, like the other TIdStack classes do?

  // TODO: use TranslateStringToTInAddr() instead of getaddrinfo() to convert
  // the IP address to a sockaddr struct for getnameinfo(), like other TIdStack
  // classes do.

  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family := IdIPFamily[AIPVersion];
  Hints.ai_socktype := Integer(SOCK_STREAM);
  Hints.ai_flags := AI_NUMERICHOST;
  LAddrInfo := nil;

  {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp := TIdPlatformString(AAddress); // explicit convert to Ansi/Unicode
  {$ENDIF}

  RetVal := getaddrinfo(
    {$IFDEF STRING_UNICODE_MISMATCH}PIdPlatformChar(LTemp){$ELSE}PChar(AAddress){$ENDIF},
    nil, @Hints, @LAddrInfo);
  if RetVal <> 0 then begin
    RaiseSocketError(gaiErrorToWsaError(RetVal));
  end;
  try
    SetLength(
      {$IFDEF STRING_UNICODE_MISMATCH}LTemp{$ELSE}Result{$ENDIF},
      NI_MAXHOST);
    RetVal := getnameinfo(
      LAddrInfo.ai_addr, LAddrInfo.ai_addrlen,
      {$IFDEF STRING_UNICODE_MISMATCH}PIdPlatformChar(LTemp){$ELSE}PChar(Result){$ENDIF},
      NI_MAXHOST, nil, 0, NI_NAMEREQD);
    if RetVal <> 0 then begin
      RaiseSocketError(gaiErrorToWsaError(RetVal));
    end;
    Result := {$IFDEF STRING_UNICODE_MISMATCH}PIdPlatformChar(LTemp){$ELSE}PChar(Result){$ENDIF};
  finally
    freeaddrinfo(LAddrInfo);
  end;
end;

function TIdStackWindows.ReadHostName: string;
var
  // Note that there is no Unicode version of gethostname.
  // Maybe use getnameinfo() instead?
  LStr: AnsiString;
begin
  SetLength(LStr, SIZE_HOSTNAME);
  gethostname(PAnsiChar(LStr), SIZE_HOSTNAME);
  //we have to specifically type cast a PAnsiChar to a string for D2009+.
  //otherwise, we will get a warning about implicit typecast from AnsiString
  //to string
  Result := String(PAnsiChar(LStr));
end;

procedure TIdStackWindows.Listen(ASocket: TIdStackSocketHandle; ABackLog: Integer);
begin
  CheckForSocketError(IdWinsock2.listen(ASocket, ABacklog));
end;

// RLebeau 12/16/09: MS Hotfix #971383 supposedly fixes a bug in Windows
// Server 2003 when client and server are running on the same machine.
// The bug can cause recv() to return 0 bytes prematurely even though data
// is actually pending.  Uncomment the below define if you do not want to
// rely on the Hotfix always being installed.  The workaround described by
// MS is to simply call recv() again to make sure data is really not pending.
//
{.$DEFINE IGNORE_KB971383_FIX}

function TIdStackWindows.WSRecv(ASocket: TIdStackSocketHandle; var ABuffer;
  const ABufferLength, AFlags: Integer) : Integer;
begin
  Result := recv(ASocket, ABuffer, ABufferLength, AFlags);
  {$IFDEF IGNORE_KB971383_FIX}
  if Result = 0 then begin
    Result := recv(ASocket, ABuffer, ABufferLength, AFlags);
  end;
  {$ENDIF}
end;

function TIdStackWindows.RecvFrom(const ASocket: TIdStackSocketHandle;
  var VBuffer; const ALength, AFlags: Integer; var VIP: string;
  var VPort: TIdPort; var VIPVersion: TIdIPVersion): Integer;
var
  LSize: Integer;
  LAddr: SOCKADDR_STORAGE;
begin
  LSize := SizeOf(LAddr);
  Result := IdWinsock2.recvfrom(ASocket, VBuffer, ALength, AFlags, IdWinsock2.PSOCKADDR(@LAddr), @LSize);
  if Result >= 0 then
  begin
    case LAddr.ss_family of
      Id_PF_INET4: begin
        VIP := TranslateTInAddrToString(PSockAddrIn(@LAddr)^.sin_addr, Id_IPv4);
        VPort := ntohs(PSockAddrIn(@LAddr)^.sin_port);
        VIPVersion := Id_IPv4;
      end;
      Id_PF_INET6: begin
        VIP := TranslateTInAddrToString(PSockAddrIn6(@LAddr)^.sin6_addr, Id_IPv6);
        VPort := ntohs(PSockAddrIn6(@LAddr)^.sin6_port);
        VIPVersion := Id_IPv6;
      end;
      else begin
        IPVersionUnsupported;
      end;
    end;
  end;
end;

function TIdStackWindows.WSSend(ASocket: TIdStackSocketHandle;
  const ABuffer; const ABufferLength, AFlags: Integer): Integer;
begin
  Result := CheckForSocketError(IdWinsock2.send(ASocket, ABuffer, ABufferLength, AFlags));
end;

procedure TIdStackWindows.WSSendTo(ASocket: TIdStackSocketHandle;
  const ABuffer; const ABufferLength, AFlags: Integer; const AIP: string;
  const APort: TIdPort; AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LAddr: SOCKADDR_STORAGE;
  LSize: Integer;
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  case AIPVersion of
    Id_IPv4: begin
      PSockAddrIn(@LAddr)^.sin_family := Id_PF_INET4;
      TranslateStringToTInAddr(AIP, PSockAddrIn(@LAddr)^.sin_addr, Id_IPv4);
      PSockAddrIn(@LAddr)^.sin_port := htons(APort);
      LSize := SIZE_TSOCKADDRIN;
    end;
    Id_IPv6: begin
      PSockAddrIn6(@LAddr)^.sin6_family := Id_PF_INET6;
      TranslateStringToTInAddr(AIP, PSockAddrIn6(@LAddr)^.sin6_addr, Id_IPv6);
      PSockAddrIn6(@LAddr)^.sin6_port := htons(APort);
      LSize := SIZE_TSOCKADDRIN6;
    end;
    else begin
      LSize := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;
  LSize := IdWinsock2.sendto(ASocket, ABuffer, ABufferLength, AFlags, IdWinsock2.PSOCKADDR(@LAddr), LSize);
  // TODO: call CheckForSocketError() here
  if LSize = Id_SOCKET_ERROR then begin
    // TODO: move this into RaiseLastSocketError() directly
    if WSGetLastError() = Id_WSAEMSGSIZE then begin
      raise EIdPackageSizeTooBig.Create(RSPackageSizeTooBig);
    end else begin
      RaiseLastSocketError;
    end;
  end
  else if LSize <> ABufferLength then begin
    raise EIdNotAllBytesSent.Create(RSNotAllBytesSent);
  end;
end;

function TIdStackWindows.WSGetLastError: Integer;
begin
  Result := WSAGetLastError;
  if Result = -1073741251{STATUS_HOST_UNREACHABLE} then begin
    Result := WSAEHOSTUNREACH;
  end
end;

procedure TIdStackWindows.WSSetLastError(const AErr : Integer);
begin
  WSASetLastError(AErr);
end;

function TIdStackWindows.WSSocket(AFamily : Integer; AStruct : TIdSocketType; AProtocol: Integer;
  const AOverlapped: Boolean = False): TIdStackSocketHandle;
begin
  if AOverlapped then begin
    Result := WSASocket(AFamily, AStruct, AProtocol, nil, 0, WSA_FLAG_OVERLAPPED);
  end else begin
    Result := IdWinsock2.socket(AFamily, AStruct, AProtocol);
  end;
end;

function TIdStackWindows.WSGetServByName(const AServiceName: string): TIdPort;
var
  ps: PServEnt;
  {$IFDEF STRING_IS_UNICODE}
  LTemp: AnsiString;
  {$ENDIF}
begin
  {$IFDEF STRING_IS_UNICODE}
  LTemp := AnsiString(AServiceName); // explicit convert to Ansi
  {$ENDIF}
  ps := getservbyname(
    PAnsiChar({$IFDEF STRING_IS_UNICODE}LTemp{$ELSE}AServiceName{$ENDIF}),
    nil);
  if ps <> nil then begin
    Result := ntohs(ps^.s_port);
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

procedure TIdStackWindows.AddServByPortToList(const APortNumber: TIdPort; AAddresses: TStrings);
type
  // Note that there is no Unicode version of getservbyport.
  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = packed array[0..(MaxInt div SizeOf(PAnsiChar))-1] of PAnsiChar;
var
  ps: PServEnt;
  i: integer;
  p: PPAnsiCharArray;
begin
  ps := getservbyport(htons(APortNumber), nil);
  if ps = nil then begin
    RaiseLastSocketError;
  end;
  AAddresses.BeginUpdate;
  try
    //we have to specifically type cast a PAnsiChar to a string for D2009+.
    //otherwise, we will get a warning about implicit typecast from AnsiString
    //to string
    AAddresses.Add(String(ps^.s_name));
    i := 0;
    p := Pointer(ps^.s_aliases);
    while p[i] <> nil do
    begin
      AAddresses.Add(String(p[i]));
      Inc(i);
    end;
  finally
    AAddresses.EndUpdate;
  end;
end;

function TIdStackWindows.HostToNetwork(AValue: UInt16): UInt16;
begin
  Result := htons(AValue);
end;

function TIdStackWindows.NetworkToHost(AValue: UInt16): UInt16;
begin
  Result := ntohs(AValue);
end;

function TIdStackWindows.HostToNetwork(AValue: UInt32): UInt32;
begin
  Result := htonl(AValue);
end;

function TIdStackWindows.NetworkToHost(AValue: UInt32): UInt32;
begin
  Result := ntohl(AValue);
end;

function TIdStackWindows.HostToNetwork(AValue: TIdUInt64): TIdUInt64;
var
  LParts: TIdUInt64Parts;
  L: UInt32;
begin
  // TODO: ARM is bi-endian, so if Windows is running on ARM instead of x86,
  // can it ever be big endian? Or do ARM manufacturers put it in little endian
  // for Windows installations?

  //if (htonl(1) <> 1) then begin
    LParts.QuadPart := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
    L := htonl(LParts.HighPart);
    LParts.HighPart := htonl(LParts.LowPart);
    LParts.LowPart := L;
    Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := LParts.QuadPart;
  //end else begin
  //  Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
  //end;
end;

function TIdStackWindows.NetworkToHost(AValue: TIdUInt64): TIdUInt64;
var
  LParts: TIdUInt64Parts;
  L: UInt32;
begin
  // TODO: ARM is bi-endian, so if Windows is running on ARM instead of x86,
  // can it ever be big endian? Or do ARM manufacturers put it in little endian
  // for Windows installations?

  //if (ntohl(1) <> 1) then begin
    LParts.QuadPart := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
    L := ntohl(LParts.HighPart);
    LParts.HighPart := ntohl(LParts.LowPart);
    LParts.LowPart := L;
    Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := LParts.QuadPart;
  //end else begin
  //  Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
  //end;
end;

procedure TIdStackWindows.GetLocalAddressList(AAddresses: TIdStackLocalAddressList);
{$IFNDEF USE_IPHLPAPI}
  {$IFNDEF WINCE}
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
  TaPIn6Addr = array[0..250] of PIn6Addr;
  PaPIn6Addr = ^TaPIn6Addr;
  {$ENDIF}
{$ENDIF}

  {$IFDEF USE_IPHLPAPI}

  function IPv4MaskLengthToString(MaskLength: ULONG): String;
  var
    Mask: ULONG;
  begin
    if ConvertLengthToIpv4Mask(MaskLength, Mask) = ERROR_SUCCESS then begin
      Result := TranslateTInAddrToString(Mask, Id_IPv4);
    end else begin
      Result := '';
    end;
  end;

  procedure GetIPv4SubNetMasks(ASubNetMasks: TStrings);
  var
    Ret: DWORD;
    BufLen: ULONG;
    Table: PMIB_IPADDRTABLE;
    pRow: PMIB_IPADDRROW;
    I: ULONG;
  begin
    BufLen := 0;
    Table := nil;
    try
      repeat
        // Alternatively, use WSAIoctl(SIO_GET_INTERFACE_LIST), but
        // I have noticed it does not always return IPv4 subnets!
        Ret := GetIpAddrTable(Table, BufLen, FALSE);
        case Ret of
          ERROR_SUCCESS:
          begin
            if BufLen = 0 then begin
              Exit;
            end;
            Break;
          end;
          ERROR_NOT_SUPPORTED:
            Exit;
          ERROR_INSUFFICIENT_BUFFER:
            ReallocMem(Table, BufLen);
        else
          SetLastError(Ret);
          IndyRaiseLastError;
        end;
      until False;

      if Ret = ERROR_SUCCESS then
      begin
        if Table^.dwNumEntries > 0 then
        begin
          pRow := @(Table^.table[0]);
          for I := 0 to Table^.dwNumEntries-1 do begin
            IndyAddPair(ASubNetMasks, TranslateTInAddrToString(pRow^.dwAddr, Id_IPv4), TranslateTInAddrToString(pRow^.dwMask, Id_IPv4));
            Inc(pRow);
          end;
        end;
      end;
    finally
      FreeMem(Table);
    end;
  end;

  function GetLocalAddressesByAdaptersAddresses: Boolean;
  var
    Ret: DWORD;
    BufLen: ULONG;
    Adapter, Adapters: PIP_ADAPTER_ADDRESSES;
    UnicastAddr: PIP_ADAPTER_UNICAST_ADDRESS;
    IPAddr: string;
    SubNetStr: String;
    SubNetMasks: TStringList;
  begin
    // assume True unless ERROR_NOT_SUPPORTED is reported...
    Result := True;

    // MSDN says:
    // The recommended method of calling the GetAdaptersAddresses function is
    // to pre-allocate a 15KB working buffer pointed to by the AdapterAddresses
    // parameter. On typical computers, this dramatically reduces the chances
    // that the GetAdaptersAddresses function returns ERROR_BUFFER_OVERFLOW,
    // which would require calling GetAdaptersAddresses function multiple times.

    BufLen := 1024*15;
    GetMem(Adapters, BufLen);
    try
      repeat
        // TODO: include GAA_FLAG_INCLUDE_PREFIX on XPSP1+?
        // TODO: include GAA_FLAG_INCLUDE_ALL_INTERFACES on Vista+?
        Ret := GetAdaptersAddresses(PF_UNSPEC, GAA_FLAG_SKIP_ANYCAST or GAA_FLAG_SKIP_MULTICAST or GAA_FLAG_SKIP_DNS_SERVER or GAA_FLAG_SKIP_FRIENDLY_NAME, nil, Adapters, BufLen);
        case Ret of
          ERROR_SUCCESS:
          begin
            // Windows CE versions earlier than 4.1 may return ERROR_SUCCESS and
            // BufLen=0 if no adapter info is available, instead of returning
            // ERROR_NO_DATA as documented...
            if BufLen = 0 then begin
              Exit;
            end;
            Break;
          end;
          ERROR_NOT_SUPPORTED:
          begin
            Result := False;
            Exit;
          end;
          ERROR_NO_DATA,
          ERROR_ADDRESS_NOT_ASSOCIATED:
            Exit;
          ERROR_BUFFER_OVERFLOW:
            ReallocMem(Adapters, BufLen);
        else
          SetLastError(Ret);
          IndyRaiseLastError;
        end;
      until False;

      if Ret = ERROR_SUCCESS then
      begin
        SubNetMasks := nil;
        try
          AAddresses.BeginUpdate;
          try
            Adapter := Adapters;
            repeat
              if (Adapter.IfType <> IF_TYPE_SOFTWARE_LOOPBACK) and
                ((Adapter.Flags and IP_ADAPTER_RECEIVE_ONLY) = 0) then
              begin
                UnicastAddr := Adapter^.FirstUnicastAddress;
                while UnicastAddr <> nil do
                begin
                  if UnicastAddr^.DadState = IpDadStatePreferred then
                  begin
                    case UnicastAddr^.Address.lpSockaddr.sin_family of
                      AF_INET: begin
                        IPAddr := TranslateTInAddrToString(PSockAddrIn(UnicastAddr^.Address.lpSockaddr)^.sin_addr, Id_IPv4);
                        // The OnLinkPrefixLength member is only available on Windows Vista and later
                        if IndyCheckWindowsVersion(6) then begin
                          SubNetStr := IPv4MaskLengthToString(UnicastAddr^.OnLinkPrefixLength);
                        end else
                        begin
                          // TODO: on XP SP1+, can the subnet mask be determined
                          // by analyzing the Adapter's Prefix list without resorting
                          // to reading the Registry?
                          if SubNetMasks = nil then
                          begin
                            SubNetMasks := TStringList.Create;
                            GetIPv4SubNetMasks(SubNetMasks);
                          end;
                          SubNetStr := SubNetMasks.Values[IPAddr];
                        end;
                        TIdStackLocalAddressIPv4.Create(AAddresses, IPAddr, SubNetStr);
                      end;
                      AF_INET6: begin
                        TIdStackLocalAddressIPv6.Create(AAddresses, TranslateTInAddrToString(PSockAddrIn6(UnicastAddr^.Address.lpSockaddr)^.sin6_addr, Id_IPv6));
                      end;
                    end;
                  end;
                  UnicastAddr := UnicastAddr^.Next;
                end;
              end;
              Adapter := Adapter^.Next;
            until Adapter = nil;
          finally
            AAddresses.EndUpdate;
          end;
        finally
          SubNetMasks.Free;
        end;
      end;
    finally
      FreeMem(Adapters);
    end;
  end;

  procedure GetUniDirAddresseses(AUniDirAddresses: TStrings);
  var
    Ret: DWORD;
    BufLen: ULONG;
    Adapters: PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
    pUniDirAddr: PInAddr;
    I: ULONG;
  begin
    BufLen := 1024*15;
    GetMem(Adapters, BufLen);
    try
      repeat
        Ret := GetUniDirectionalAdapterInfo(Adapters, BufLen);
        case Ret of
          ERROR_SUCCESS:
          begin
            if BufLen = 0 then begin
              Exit;
            end;
            Break;
          end;
          ERROR_NOT_SUPPORTED,
          ERROR_NO_DATA:
            Exit;
          ERROR_MORE_DATA:
            ReallocMem(Adapters, BufLen);
        else
          SetLastError(Ret);
          IndyRaiseLastError;
        end;
      until False;

      if Ret = ERROR_SUCCESS then
      begin
        if Adapters^.NumAdapters > 0 then
        begin
          pUniDirAddr := @(Adapters^.Address[0]);
          for I := 0 to Adapters^.NumAdapters-1 do begin
            AUniDirAddresses.Add(TranslateTInAddrToString(pUniDirAddr^, Id_IPv4));
            Inc(pUniDirAddr);
          end;
        end;
      end;
    finally
      FreeMem(Adapters);
    end;
  end;

  procedure GetLocalAddressesByAdaptersInfo;
  var
    Ret: DWORD;
    BufLen: ULONG;
    UniDirAddresses: TStringList;
    Adapter, Adapters: PIP_ADAPTER_INFO;
    IPAddr: PIP_ADDR_STRING;
    IPStr: String;
  begin
    BufLen := 1024*15;
    GetMem(Adapters, BufLen);
    try
      repeat
        Ret := GetAdaptersInfo(Adapters, BufLen);
        case Ret of
          ERROR_SUCCESS:
          begin
            // Windows CE versions earlier than 4.1 may return ERROR_SUCCESS and
            // BufLen=0 if no adapter info is available, instead of returning
            // ERROR_NO_DATA as documented...
            if BufLen = 0 then begin
              Exit;
            end;
            Break;
          end;
          ERROR_NOT_SUPPORTED,
          ERROR_NO_DATA:
            Exit;
          ERROR_BUFFER_OVERFLOW:
            ReallocMem(Adapters, BufLen);
        else
          SetLastError(Ret);
          IndyRaiseLastError;
        end;
      until False;

      if Ret = ERROR_SUCCESS then
      begin
        // on XP and later, GetAdaptersInfo() includes uni-directional adapters.
        // Need to use GetUniDirectionalAdapterInfo() to filter them out of the
        // list ...

        if IndyCheckWindowsVersion(5, 1) then begin
          UniDirAddresses := TStringList.Create;
        end else begin
          UniDirAddresses := nil;
        end;
        try
          if UniDirAddresses <> nil then begin
            GetUniDirAddresseses(UniDirAddresses);
          end;
          AAddresses.BeginUpdate;
          try
            Adapter := Adapters;
            repeat
              IPAddr := @(Adapter^.IpAddressList);
              repeat
                IPStr := String(IPAddr^.IpAddress.S);
                if (IPStr <> '') and (IPStr <> '0.0.0.0') then
                begin
                  if UniDirAddresses <> nil then begin
                    if UniDirAddresses.IndexOf(IPStr) <> -1 then begin
                      IPAddr := IPAddr^.Next;
                      Continue;
                    end;
                  end;
                  TIdStackLocalAddressIPv4.Create(AAddresses, IPStr, String(IPAddr^.IpMask.S));
                end;
                IPAddr := IPAddr^.Next;
              until IPAddr = nil;
              Adapter := Adapter^.Next;
            until Adapter = nil;
          finally
            AAddresses.EndUpdate;
          end;
        finally
          UniDirAddresses.Free;
        end;
      end;
    finally
      FreeMem(Adapters);
    end;
  end;

    {$ELSE}

var
    {$IFDEF UNICODE}
  Hints: TAddrInfoW;
  LAddrList, LAddrInfo: pAddrInfoW;
    {$ELSE}
  Hints: TAddrInfo;
  LAddrList, LAddrInfo: pAddrInfo;
    {$ENDIF}
  RetVal: Integer;
  LHostName: String;
    {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp: TIdPlatformString;
    {$ENDIF}

  {$ENDIF}
begin
  // Using gethostname() and gethostbyname/getaddrinfo() may not always return
  // just the machine's IP addresses. Technically speaking, they will return
  // the local hostname, and then return the address(es) to which that hostname
  // resolves. It is possible for a machine to (a) be configured such that its
  // name does not resolve to an IP, or (b) be configured such that its name
  // resolves to multiple IPs, only one of which belongs to the local machine.
  // For better results, we should use the Win32 API GetAdaptersInfo() and/or
  // GetAdaptersAddresses() functions instead.  GetAdaptersInfo() only supports
  // IPv4, but GetAdaptersAddresses() supports both IPv4 and IPv6...

  {$IFDEF USE_IPHLPAPI}

  // try GetAdaptersAddresses() first, then fall back to GetAdaptersInfo()...
  if not GetLocalAddressesByAdaptersAddresses then begin
    GetLocalAddressesByAdaptersInfo;
  end;

  {$ELSE}

  LHostName := HostName;

  ZeroMemory(@Hints, SIZE_TADDRINFO);
  Hints.ai_family := PF_UNSPEC; // returns both IPv4 and IPv6 addresses
  Hints.ai_socktype := SOCK_STREAM;
  LAddrList := nil;

  {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp := TIdPlatformString(LHostName); // explicit convert to Ansi/Unicode
  {$ENDIF}

  RetVal := getaddrinfo(
    {$IFDEF STRING_UNICODE_MISMATCH}PIdPlatformChar(LTemp){$ELSE}PChar(LHostName){$ENDIF},
    nil, @Hints, @LAddrList);
  if RetVal <> 0 then begin
    RaiseSocketError(gaiErrorToWsaError(RetVal));
  end;
  try
    AAddresses.BeginUpdate;
    try
      LAddrInfo := LAddrList;
      repeat
        case LAddrInfo^.ai_addr^.sa_family of
          Id_AF_INET: begin
            TIdStackLocalAddressIPv4.Create(AAddresses, TranslateTInAddrToString(PSockAddrIn(LAddrInfo^.ai_addr)^.sin_addr, Id_IPv4), ''); // TODO: SubNet
          end;
          Id_AF_INET6: begin
            TIdStackLocalAddressIPv6.Create(AAddresses, TranslateTInAddrToString(PSockAddrIn6(LAddrInfo^.ai_addr)^.sin6_addr, Id_IPv6));
          end;
        end;
        LAddrInfo := LAddrInfo^.ai_next;
      until LAddrInfo = nil;
    finally
      AAddresses.EndUpdate;
    end;
  finally
    freeaddrinfo(LAddrList);
  end;

  {$ENDIF}
end;

{ TIdStackVersionWinsock }

function TIdStackWindows.WSShutdown(ASocket: TIdStackSocketHandle; AHow: Integer): Integer;
begin
  Result := Shutdown(ASocket, AHow);
end;

procedure TIdStackWindows.GetSocketName(ASocket: TIdStackSocketHandle;
  var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion);
var
  LSize: Integer;
  LAddr: SOCKADDR_STORAGE;
begin
  LSize := SizeOf(LAddr);
  CheckForSocketError(getsockname(ASocket, IdWinsock2.PSOCKADDR(@LAddr), LSize));
  case LAddr.ss_family of
    Id_PF_INET4: begin
      VIP := TranslateTInAddrToString(PSockAddrIn(@LAddr)^.sin_addr, Id_IPv4);
      VPort := ntohs(PSockAddrIn(@LAddr)^.sin_port);
      VIPVersion := Id_IPv4;
    end;
    Id_PF_INET6: begin
      VIP := TranslateTInAddrToString(PSockAddrIn6(@LAddr)^.sin6_addr, Id_IPv6);
      VPort := Ntohs(PSockAddrIn6(@LAddr)^.sin6_port);
      VIPVersion := Id_IPv6;
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

{ TIdSocketListWindows }

procedure TIdSocketListWindows.Add(AHandle: TIdStackSocketHandle);
begin
  Lock;
  try
    if FFDSet.fd_count >= FD_SETSIZE then begin
      raise EIdStackSetSizeExceeded.Create(RSSetSizeExceeded);
    end;
    FFDSet.fd_array[FFDSet.fd_count] := AHandle;
    Inc(FFDSet.fd_count);
  finally
    Unlock;
  end;
end;

procedure TIdSocketListWindows.Clear;
begin
  Lock;
  try
    fd_zero(FFDSet);
  finally
    Unlock;
  end;
end;

function TIdSocketListWindows.ContainsSocket(AHandle: TIdStackSocketHandle): Boolean;
begin
  Lock;
  try
    Result := fd_isset(AHandle, FFDSet);
  finally
    Unlock;
  end;
end;

function TIdSocketListWindows.Count: Integer;
begin
  Lock;
  try
    Result := FFDSet.fd_count;
  finally
    Unlock;
  end;
end;

function TIdSocketListWindows.GetItem(AIndex: Integer): TIdStackSocketHandle;
begin
  Result := 0;
  Lock;
  try
    //We can't redefine AIndex to be a UInt32 because the libc Interface
    //and DotNET define it as a LongInt.  OS/2 defines it as a UInt16.
    if (AIndex >= 0) and (u_int(AIndex) < FFDSet.fd_count) then begin
      Result := FFDSet.fd_array[AIndex];
    end else begin
      raise EIdStackSetSizeExceeded.Create(RSSetSizeExceeded);
    end;
  finally
    Unlock;
   end;
end;

procedure TIdSocketListWindows.Remove(AHandle: TIdStackSocketHandle);
var
  i: Integer;
begin
  Lock;
  try
    {
    IMPORTANT!!!

    Sometimes, there may not be a member of the FDSET.  If you attempt to "remove"
    an item, the loop would execute once.
    }
    if FFDSet.fd_count > 0 then
    begin
      for i:= 0 to FFDSet.fd_count - 1 do
      begin
        if FFDSet.fd_array[i] = AHandle then
        begin
          Dec(FFDSet.fd_count);
          FFDSet.fd_array[i] := FFDSet.fd_array[FFDSet.fd_count];
          FFDSet.fd_array[FFDSet.fd_count] := 0; //extra purity
          Break;
        end;//if found
      end;
    end;
  finally
    Unlock;
  end;
end;

function TIdStackWindows.WSTranslateSocketErrorMsg(const AErr: Integer): string;
begin
  if AErr = WSAHOST_NOT_FOUND then begin
    Result := IndyFormat(RSStackError, [AErr, RSStackHOST_NOT_FOUND]);
  end else begin
    Result :=  inherited WSTranslateSocketErrorMsg(AErr);
  end;
end;

function TIdSocketListWindows.SelectRead(const ATimeout: Integer): Boolean;
var
  LSet: TFDSet;
begin
  // Windows updates this structure on return, so we need to copy it each time we need it
  GetFDSet(LSet);
  Result := FDSelect(@LSet, nil, nil, ATimeout);
end;

class function TIdSocketListWindows.FDSelect(AReadSet, AWriteSet,
  AExceptSet: PFDSet; const ATimeout: Integer): Boolean;
var
  LResult: Integer;
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
  LResult := IdWinsock2.select(0, AReadSet, AWriteSet, AExceptSet, LTimePtr);
  //TODO: Remove this cast
  Result := GStack.CheckForSocketError(LResult) > 0;
end;

function TIdSocketListWindows.SelectReadList(var VSocketList: TIdSocketList;
  const ATimeout: Integer): Boolean;
var
  LSet: TFDSet;
begin
  // Windows updates this structure on return, so we need to copy it each time we need it
  GetFDSet(LSet);
  Result := FDSelect(@LSet, nil, nil, ATimeout);
  if Result then
  begin
    if VSocketList = nil then begin
      VSocketList := TIdSocketList.CreateSocketList;
    end;
    TIdSocketListWindows(VSocketList).SetFDSet(LSet);
  end;
end;

class function TIdSocketListWindows.Select(AReadList, AWriteList,
  AExceptList: TIdSocketList; const ATimeout: Integer): Boolean;
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
      TIdSocketListWindows(AList).GetFDSet(ASet);
      APSet := @ASet;
    end else begin
      APSet := nil;
    end;
  end;

begin
  ReadSet(AReadList, LReadSet, LPReadSet);
  ReadSet(AWriteList, LWriteSet, LPWriteSet);
  ReadSet(AExceptList, LExceptSet, LPExceptSet);

  Result := FDSelect(LPReadSet, LPWriteSet, LPExceptSet, ATimeout);

  if AReadList <> nil then begin
    TIdSocketListWindows(AReadList).SetFDSet(LReadSet);
  end;
  if AWriteList <> nil then begin
    TIdSocketListWindows(AWriteList).SetFDSet(LWriteSet);
  end;
  if AExceptList <> nil then begin
    TIdSocketListWindows(AExceptList).SetFDSet(LExceptSet);
  end;
end;

procedure TIdSocketListWindows.SetFDSet(var VSet: TFDSet);
begin
  Lock;
  try
    FFDSet := VSet;
  finally
    Unlock;
  end;
end;

procedure TIdSocketListWindows.GetFDSet(var VSet: TFDSet);
begin
  Lock;
  try
    VSet := FFDSet;
  finally
    Unlock;
  end;
end;

procedure TIdStackWindows.SetBlocking(ASocket: TIdStackSocketHandle;
  const ABlocking: Boolean);
var
  LValue: UInt32;
begin
  LValue := UInt32(not ABlocking);
  CheckForSocketError(ioctlsocket(ASocket, FIONBIO, LValue));
end;

function TIdSocketListWindows.Clone: TIdSocketList;
begin
  Result := TIdSocketListWindows.Create;
  try
    Lock;
    try
      TIdSocketListWindows(Result).SetFDSet(FFDSet);
    finally
      Unlock;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TIdStackWindows.WouldBlock(const AResult: Integer): Boolean;
begin
  Result := (AResult = WSAEWOULDBLOCK);
end;

function TIdStackWindows.HostByName(const AHostName: string;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string;
{$IFNDEF WINCE}
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
{$ENDIF}
var
  {$IFDEF UNICODE}
  LAddrInfo: pAddrInfoW;
  Hints: TAddrInfoW;
  {$ELSE}
  LAddrInfo: pAddrInfo;
  Hints: TAddrInfo;
  {$ENDIF}
  RetVal: Integer;
  LHostName: String;
  {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp: TIdPlatformString;
  {$ENDIF}
begin
  if not (AIPVersion in [Id_IPv4, Id_IPv6]) then begin
    IPVersionUnsupported;
  end;

  ZeroMemory(@Hints, SIZE_TADDRINFO);
  Hints.ai_family := IdIPFamily[AIPVersion];
  Hints.ai_socktype := SOCK_STREAM;
  LAddrInfo := nil;

  if UseIDNAPI then begin
    LHostName := IDNToPunnyCode(
      {$IFDEF STRING_IS_UNICODE}
      AHostName
      {$ELSE}
      TIdUnicodeString(AHostName) // explicit convert to Unicode
      {$ENDIF}
    );
  end else begin
    LHostName := AHostName;
  end;

  {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp := TIdPlatformString(LHostName); // explicit convert to Ansi/Unicode
  {$ENDIF}

  RetVal := getaddrinfo(
    {$IFDEF STRING_UNICODE_MISMATCH}PIdPlatformChar(LTemp){$ELSE}PChar(LHostName){$ENDIF},
    nil, @Hints, @LAddrInfo);
  if RetVal <> 0 then begin
    RaiseSocketError(gaiErrorToWsaError(RetVal));
  end;
  try
    if AIPVersion = Id_IPv4 then begin
      Result := TranslateTInAddrToString(PSockAddrIn(LAddrInfo^.ai_addr)^.sin_addr, Id_IPv4)
    end else begin
      Result := TranslateTInAddrToString(PSockAddrIn6(LAddrInfo^.ai_addr)^.sin6_addr, Id_IPv6);
    end;
  finally
    freeaddrinfo(LAddrInfo);
  end;
end;

procedure TIdStackWindows.Connect(const ASocket: TIdStackSocketHandle;
  const AIP: string; const APort: TIdPort;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  LAddr: SOCKADDR_STORAGE;
  LSize: Integer;
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  case AIPVersion of
    Id_IPv4: begin
      PSockAddrIn(@LAddr)^.sin_family := Id_PF_INET4;
      TranslateStringToTInAddr(AIP, PSockAddrIn(@LAddr)^.sin_addr, Id_IPv4);
      PSockAddrIn(@LAddr)^.sin_port := htons(APort);
      LSize := SIZE_TSOCKADDRIN;
    end;
    Id_IPv6: begin
      PSockAddrIn6(@LAddr)^.sin6_family := Id_PF_INET6;
      TranslateStringToTInAddr(AIP, PSockAddrIn6(@LAddr)^.sin6_addr, Id_IPv6);
      PSockAddrIn6(@LAddr)^.sin6_port := htons(APort);
      LSize := SIZE_TSOCKADDRIN6;
    end;
    else begin
      LSize := 0; // avoid warning
      IPVersionUnsupported;
    end;
  end;
  CheckForSocketError(IdWinsock2.connect(ASocket, IdWinsock2.PSOCKADDR(@LAddr), LSize));
end;

procedure TIdStackWindows.GetPeerName(ASocket: TIdStackSocketHandle;
  var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion);
var
  LSize: Integer;
  LAddr: SOCKADDR_STORAGE;
begin
  LSize := SizeOf(LAddr);
  CheckForSocketError(IdWinsock2.getpeername(ASocket, IdWinsock2.PSOCKADDR(@LAddr), LSize));
  case LAddr.ss_family of
    Id_PF_INET4: begin
      VIP := TranslateTInAddrToString(PSockAddrIn(@LAddr)^.sin_addr, Id_IPv4);
      VPort := ntohs(PSockAddrIn(@LAddr)^.sin_port);
      VIPVersion := Id_IPv4;
    end;
    Id_PF_INET6: begin
      VIP := TranslateTInAddrToString(PSockAddrIn6(@LAddr)^.sin6_addr, Id_IPv6);
      VPort := ntohs(PSockAddrIn6(@LAddr)^.sin6_port);
      VIPVersion := Id_IPv6;
    end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

procedure TIdStackWindows.Disconnect(ASocket: TIdStackSocketHandle);
begin
  // Windows uses Id_SD_Send, Linux should use Id_SD_Both
  WSShutdown(ASocket, Id_SD_Send);
  // SO_LINGER is false - socket may take a little while to actually close after this
  WSCloseSocket(ASocket);
end;

procedure TIdStackWindows.{$IFDEF VCL_XE3_OR_ABOVE}GetSocketOption{$ELSE}WSGetSocketOption{$ENDIF}
  (ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption;
  var AOptVal; var AOptLen: Integer);
begin
  CheckForSocketError(
    getsockopt(ASocket, ALevel, AOptName,
      {$IFNDEF HAS_PAnsiChar}
      // TODO: use TPtrWrapper here?
      {PAnsiChar}@AOptVal
      {$ELSE}
      PAnsiChar(@AOptVal)
      {$ENDIF},
      AOptLen
    )
  );
end;

procedure TIdStackWindows.{$IFDEF VCL_XE3_OR_ABOVE}SetSocketOption{$ELSE}WSSetSocketOption{$ENDIF}
  (ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption;
  const AOptVal; const AOptLen: Integer);
begin
  CheckForSocketError(
    setsockopt(ASocket, ALevel, Aoptname,
      {$IFNDEF HAS_PAnsiChar}
      // TODO: use TPtrWrapper here?
      {PAnsiChar}@AOptVal
      {$ELSE}
      PAnsiChar(@AOptVal)
      {$ENDIF},
      AOptLen
    )
  );
end;

function TIdStackWindows.SupportsIPv4: Boolean;
var
  LLen : DWORD;
  LPInfo, LPCurPtr: LPWSAPROTOCOL_INFO;
  LCount : Integer;
  i : Integer;
begin
  // TODO: move this logic into CheckIPVersionSupport() instead...
  // Result := CheckIPVersionSupport(Id_IPv4);

  Result := False;
  LPInfo := nil;
  try
    LLen := 0;
    // Note: WSAEnumProtocols returns -1 when it is just called to get the needed Buffer Size!
    repeat
      LCount := IdWinsock2.WSAEnumProtocols(nil, LPInfo, LLen);
      if LCount = SOCKET_ERROR then
      begin
        if WSAGetLastError() <> WSAENOBUFS then begin
          Exit;
        end;
        ReallocMem(LPInfo, LLen);
      end else begin
        Break;
      end;
    until False;

    if LCount > 0 then
    begin
      LPCurPtr := LPInfo;
      for i := 0 to LCount-1 do
      begin
        if LPCurPtr^.iAddressFamily = AF_INET then
        begin
          Result := True;
          Exit;
        end;
        Inc(LPCurPtr);
      end;
    end;
  finally
    FreeMem(LPInfo);
  end;
end;

{
based on
http://groups.google.com/groups?q=Winsock2+Delphi+protocol&hl=en&lr=&ie=UTF-8&oe=utf-8&selm=3cebe697_2%40dnews&rnum=9
}
function TIdStackWindows.SupportsIPv6: Boolean;
var
  LLen : DWORD;
  LPInfo, LPCurPtr: LPWSAPROTOCOL_INFO;
  LCount : Integer;
  i : Integer;
begin
  // TODO: move this logic into CheckIPVersionSupport() instead...
  // Result := CheckIPVersionSupport(Id_IPv6);

  Result := False;
  LPInfo := nil;
  try
    LLen := 0;
    // Note: WSAEnumProtocols returns -1 when it is just called to get the needed Buffer Size!
    repeat
      LCount := IdWinsock2.WSAEnumProtocols(nil, LPInfo, LLen);
      if LCount = SOCKET_ERROR then
      begin
        if WSAGetLastError() <> WSAENOBUFS then begin
          Exit;
        end;
        ReallocMem(LPInfo, LLen);
      end else begin
        Break;
      end;
    until False;

    if LCount > 0 then
    begin
      LPCurPtr := LPInfo;
      for i := 0 to LCount-1 do
      begin
        if LPCurPtr^.iAddressFamily = AF_INET6 then
        begin
          Result := True;
          Exit;
        end;
        Inc(LPCurPtr);
      end;
    end;
  finally
    FreeMem(LPInfo);
  end;
end;

function TIdStackWindows.IOControl(const s: TIdStackSocketHandle;
  const cmd: UInt32; var arg: UInt32): Integer;
begin
  Result := IdWinsock2.ioctlsocket(s, cmd, arg);
end;

procedure TIdStackWindows.WSQuerryIPv6Route(ASocket: TIdStackSocketHandle;
  const AIP: String; const APort: TIdPort; var VSource; var VDest);
var
  Llocalif : TSockAddrIn6;
  LAddr : TSockAddrIn6;
  Bytes : DWORD;
begin
  //make our LAddrInfo structure
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin6_family := AF_INET6;
  TranslateStringToTInAddr(AIP, LAddr.sin6_addr, Id_IPv6);
  Move(LAddr.sin6_addr, VDest, SizeOf(in6_addr));
  LAddr.sin6_port := htons(APort);
  // Find out which local interface for the destination
  // RLebeau: in XE4+, PDWORD is NOT defined as ^DWORD, so we have to use a type-cast!
  CheckForSocketError(WSAIoctl(ASocket, SIO_ROUTING_INTERFACE_QUERY,
    @LAddr, SizeOf(LAddr), @Llocalif, SizeOf(Llocalif), PDWORD(@Bytes), nil, nil));
  Move(Llocalif.sin6_addr, VSource, SizeOf(in6_addr));
end;

procedure TIdStackWindows.WriteChecksum(s: TIdStackSocketHandle;
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

procedure TIdStackWindows.WriteChecksumIPv6(s: TIdStackSocketHandle;
  var VBuffer: TIdBytes; const AOffset: Integer; const AIP: String;
  const APort: TIdPort);
var 
  LSource : TIdIn6Addr;
  LDest : TIdIn6Addr;
  LTmp : TIdBytes;
  LIdx : Integer;
  LC : UInt32;
{
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               |
   +                                                               +
   |                                                               |
   +                         Source Address                        +
   |                                                               |
   +                                                               +
   |                                                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               |
   +                                                               +
   |                                                               |
   +                      Destination Address                      +
   |                                                               |
   +                                                               +
   |                                                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                   Upper-Layer Packet Length                   |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                      zero                     |  Next Header  |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}
begin
  WSQuerryIPv6Route(s, AIP, APort, LSource, LDest);
  SetLength(LTmp, 40+Length(VBuffer));

  //16
  Move(LSource, LTmp[0], SIZE_TSOCKADDRIN6);
  LIdx := SIZE_TSOCKADDRIN6;
  //32
  Move(LDest, LTmp[LIdx], SIZE_TSOCKADDRIN6);
  Inc(LIdx, SIZE_TSOCKADDRIN6);
  //use a word so you don't wind up using the wrong network byte order function
  LC := UInt32(Length(VBuffer));
  CopyTIdUInt32(HostToNetwork(LC), LTmp, LIdx);
  Inc(LIdx, 4);
  //36
  //zero the next three bytes
  FillChar(LTmp[LIdx], 3, 0);
  Inc(LIdx, 3);
  //next header (protocol type determines it
  LTmp[LIdx] := Id_IPPROTO_ICMPV6; // Id_IPPROTO_ICMP6;
  Inc(LIdx);
  //combine the two
  CopyTIdBytes(VBuffer, 0, LTmp, LIdx, Length(VBuffer));
  //zero out the checksum field
  CopyTIdUInt16(0, LTmp, LIdx+AOffset);

  CopyTIdUInt16(HostToLittleEndian(CalcCheckSum(LTmp)), VBuffer, AOffset);
end;

function TIdStackWindows.ReceiveMsg(ASocket: TIdStackSocketHandle; var VBuffer : TIdBytes;
  APkt: TIdPacketInfo): UInt32;
var
  LIP : String;
  LPort : TIdPort;
  LIPVersion : TIdIPVersion;
  {Windows CE does not have WSARecvMsg}
  {$IFNDEF WINCE}
  LSize: PtrUInt;
  LAddr: TIdBytes;
  PAddr: PSOCKADDR_STORAGE;
  LMsg : TWSAMSG;
  LMsgBuf : TWSABUF;
  LControl : TIdBytes;
  LCurCmsg : LPWSACMSGHDR;   //for iterating through the control buffer
  PPktInfo: PInPktInfo;
  PPktInfo6: PIn6PktInfo;
  {$ENDIF}
begin
  {$IFNDEF WINCE}
  //This runs only on WIndows XP or later
  // XP 5.1 at least, Vista 6.0
  if IndyCheckWindowsVersion(5, 1) then
  begin
    //we call the macro twice because we specified two possible structures.
    //Id_IPV6_HOPLIMIT and Id_IPV6_PKTINFO
    LSize := WSA_CMSG_LEN(WSA_CMSG_LEN(Length(VBuffer)));
    SetLength(LControl, LSize);

    LMsgBuf.len := Length(VBuffer); // Length(VMsgData);
    LMsgBuf.buf := PAnsiChar(Pointer(VBuffer)); // @VMsgData[0];

    FillChar(LMsg, SIZE_TWSAMSG, 0);

    LMsg.lpBuffers := @LMsgBuf;
    LMsg.dwBufferCount := 1;

    LMsg.Control.Len := LSize;
    LMsg.Control.buf := PAnsiChar(Pointer(LControl));

    // RLebeau: despite that we are not performing an overlapped I/O operation,
    // WSARecvMsg() does not like the SOCKADDR variable being allocated on the
    // stack, at least on my tests with Windows 7.  So we will allocate it on
    // the heap instead to keep WinSock happy...
    SetLength(LAddr, SizeOf(SOCKADDR_STORAGE));
    PAddr := PSOCKADDR_STORAGE(@LAddr[0]);

    LMsg.name := IdWinsock2.PSOCKADDR(PAddr);
    LMsg.namelen := Length(LAddr);

    CheckForSocketError(WSARecvMsg(ASocket, @LMsg, Result, nil, nil));
    APkt.Reset;

    case PAddr^.ss_family of
      Id_PF_INET4: begin
        APkt.SourceIP := TranslateTInAddrToString(PSockAddrIn(PAddr)^.sin_addr, Id_IPv4);
        APkt.SourcePort := ntohs(PSockAddrIn(PAddr)^.sin_port);
        APkt.SourceIPVersion := Id_IPv4;
      end;
      Id_PF_INET6: begin
        APkt.SourceIP := TranslateTInAddrToString(PSockAddrIn6(PAddr)^.sin6_addr, Id_IPv6);
        APkt.SourcePort := ntohs(PSockAddrIn6(PAddr)^.sin6_port);
        APkt.SourceIPVersion := Id_IPv6;
      end;
      else begin
        Result := 0; // avoid warning
        IPVersionUnsupported;
      end;
    end;

    LCurCmsg := nil;
    repeat
      LCurCmsg := WSA_CMSG_NXTHDR(@LMsg, LCurCmsg);
      if LCurCmsg = nil then begin
        Break;
      end;
      case LCurCmsg^.cmsg_type of
        IP_PKTINFO :     //done this way because IPV6_PKTINF and  IP_PKTINFO are both 19
        begin
          case PAddr^.ss_family of
            Id_PF_INET4: begin
              PPktInfo := PInPktInfo(WSA_CMSG_DATA(LCurCmsg));
              APkt.DestIP := TranslateTInAddrToString(PPktInfo^.ipi_addr, Id_IPv4);
              APkt.DestIF := PPktInfo^.ipi_ifindex;
              APkt.DestIPVersion := Id_IPv4;
            end;
            Id_PF_INET6: begin
              PPktInfo6 := PIn6PktInfo(WSA_CMSG_DATA(LCurCmsg));
              APkt.DestIP := TranslateTInAddrToString(PPktInfo6^.ipi6_addr, Id_IPv6);
              APkt.DestIF := PPktInfo6^.ipi6_ifindex;
              APkt.DestIPVersion := Id_IPv6;
            end;
          end;
        end;
        Id_IPV6_HOPLIMIT :
        begin
          APkt.TTL := WSA_CMSG_DATA(LCurCmsg)^;
        end;
      end;
    until False;
  end else
  begin
  {$ENDIF}
    Result := RecvFrom(ASocket, VBuffer, Length(VBuffer), 0, LIP, LPort, LIPVersion);
    APkt.Reset;
    APkt.SourceIP := LIP;
    APkt.SourcePort := LPort;
    APkt.SourceIPVersion := LIPVersion;
    APkt.DestIPVersion := LIPVersion;
  {$IFNDEF WINCE}
  end;
  {$ENDIF}
end;

function TIdStackWindows.CheckIPVersionSupport(const AIPVersion: TIdIPVersion): Boolean;
var
  LTmpSocket: TIdStackSocketHandle;
begin
  LTmpSocket := WSSocket(IdIPFamily[AIPVersion], Id_SOCK_STREAM, Id_IPPROTO_IP);
  Result := LTmpSocket <> Id_INVALID_SOCKET;
  if Result then begin
    WSCloseSocket(LTmpSocket);
  end;
end;

{$IFNDEF WINCE}
{
This is somewhat messy but I wanted to do things this way to support Int64
file sizes.
}
function ServeFile(ASocket: TIdStackSocketHandle; const AFileName: string): Int64;
var
  LFileHandle: THandle;
  LSize: LARGE_INTEGER;
  {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp: TIdPlatformString;
  {$ENDIF}
begin
  Result := 0;

  {$IFDEF STRING_UNICODE_MISMATCH}
  LTemp := TIdPlatformString(AFileName); // explicit convert to Ansi/Unicode
  {$ENDIF}

  LFileHandle := CreateFile(
    {$IFDEF STRING_UNICODE_MISMATCH}PIdPlatformChar(LTemp){$ELSE}PChar(AFileName){$ENDIF},
    GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);

  if LFileHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      if TransmitFile(ASocket, LFileHandle, 0, 0, nil, nil, 0) then
      begin
        if Assigned(GetFileSizeEx) then
        begin
          if not GetFileSizeEx(LFileHandle, LSize) then begin
            Exit;
          end;
        end else
        begin
          LSize.LowPart := GetFileSize(LFileHandle, @LSize.HighPart);
          if (LSize.LowPart = $FFFFFFFF) and (GetLastError() <> 0) then begin
            Exit;
          end;
        end;
        Result := LSize.QuadPart;
      end;
    finally
      CloseHandle(LFileHandle);
    end;
  end;
end;
{$ENDIF}

procedure TIdStackWindows.SetKeepAliveValues(ASocket: TIdStackSocketHandle;
  const AEnabled: Boolean; const ATimeMS, AInterval: Integer);
var
  ka: _tcp_keepalive;
  Bytes: DWORD;
begin
  // TODO: instead of doing an OS version check, always call SIO_KEEPALIVE_VALS
  // when AEnabled is True, and then fallback to SO_KEEPALIVE if WSAIoctl()
  // reports that SIO_KEEPALIVE_VALS is not supported...

  // SIO_KEEPALIVE_VALS is supported on Win2K+ and WinCE 4.x only
  if AEnabled and IndyCheckWindowsVersion({$IFDEF WINCE}4{$ELSE}5{$ENDIF}) then
  begin
    ka.onoff := 1;
    ka.keepalivetime := ATimeMS;
    ka.keepaliveinterval := AInterval;
    // RLebeau: in XE4+, PDWORD is NOT defined as ^DWORD, so we have to use a type-cast!
    WSAIoctl(ASocket, SIO_KEEPALIVE_VALS, @ka, SizeOf(ka), nil, 0, PDWORD(@Bytes), nil, nil);
  end else begin
    SetSocketOption(ASocket, Id_SOL_SOCKET, Id_SO_KEEPALIVE, iif(AEnabled, 1, 0));
  end;
end;

initialization
  GStarted := False;
  GSocketListClass := TIdSocketListWindows;
  // Check if we are running under windows NT
  {$IFNDEF WINCE}
  if IndyWindowsPlatform = VER_PLATFORM_WIN32_NT then begin
    GetFileSizeEx := Windows.GetProcAddress(GetModuleHandle('Kernel32.dll'), 'GetFileSizeEx');
    GServeFileProc := ServeFile;
  end;
  {$ENDIF}
  {$IFDEF USE_IPHLPAPI}
  InitializeIPHelperStubs;
  {$ENDIF}
finalization
  IdWship6.CloseLibrary;
  UninitializeWinSock;
  {$IFDEF USE_IPHLPAPI}
  UninitializeIPHelperAPI;
  {$ENDIF}
  GStarted := False;

end.

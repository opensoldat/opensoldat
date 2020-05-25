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
  Rev 1.0    2004.02.03 3:14:52 PM  czhower
  Move and updates

  Rev 1.2    10/15/2003 9:43:20 PM  DSiders
  Added localization comments.

  Rev 1.1    1-10-2003 19:44:28  BGooijen
  fixed leak in CloseLibrary()

  Rev 1.0    11/13/2002 09:03:24 AM  JPMugaas
}

unit IdWship6;

interface

{$I IdCompilerDefines.inc}

{$IFDEF FPC}
  {$IFDEF WIN32}
    {$ALIGN OFF}
  {$ELSE}
    //It turns out that Win64 and WinCE require record alignment
    {$PACKRECORDS C}
  {$ENDIF}
{$ELSE}
  {$IFDEF WIN64}
    {$ALIGN ON}
    {$MINENUMSIZE 4}
  {$ELSE}
    {$MINENUMSIZE 4}
    {$IFDEF REQUIRES_PROPER_ALIGNMENT}
      {$ALIGN ON}
    {$ELSE}
      {$ALIGN OFF}
      {$WRITEABLECONST OFF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF HAS_TInterlocked}
  syncobjs, //here to facilitate inlining with Delphi
  {$ENDIF}
  {$IFNDEF HAS_SIZE_T}
  IdGlobal,
  {$ENDIF}
  Windows,
  IdWinsock2;

const
  Wship6_dll =   'Wship6.dll';    {do not localize}
  iphlpapi_dll = 'iphlpapi.dll';  {do not localize}
  fwpuclnt_dll = 'Fwpuclnt.dll'; {Do not localize}

  // Error codes from getaddrinfo().

  //JPM
  //Note that I am adding a GIA_ prefix on my own because
  //some names here share some names defined in IdWinsock2 causing
  //an unpredictible problem. The values are not defined the same in IdWinsock2
  {$EXTERNALSYM GIA_EAI_ADDRFAMILY}
  GIA_EAI_ADDRFAMILY = 1  ; // Address family for nodename not supported.
  {$EXTERNALSYM GIA_EAI_AGAIN}
  GIA_EAI_AGAIN      = 2  ; // Temporary failure in name resolution.
  {$EXTERNALSYM GIA_EAI_BADFLAGS}
  GIA_EAI_BADFLAGS   = 3  ; // Invalid value for ai_flags.
  {$EXTERNALSYM GIA_EAI_FAIL}
  GIA_EAI_FAIL       = 4  ; // Non-recoverable failure in name resolution.
  {$EXTERNALSYM GIA_EAI_FAMILY}
  GIA_EAI_FAMILY     = 5  ; // Address family ai_family not supported.
  {$EXTERNALSYM GIA_EAI_MEMORY}
  GIA_EAI_MEMORY     = 6  ; // Memory allocation failure.
  {$EXTERNALSYM GIA_EAI_NODATA}
  GIA_EAI_NODATA     = 7  ; // No address associated with nodename.
  {$EXTERNALSYM GIA_EAI_NONAME}
  GIA_EAI_NONAME     = 8  ; // Nodename nor servname provided, or not known.
  {$EXTERNALSYM GIA_EAI_SERVICE}
  GIA_EAI_SERVICE    = 9  ; // Servname not supported for ai_socktype.
  {$EXTERNALSYM GIA_EAI_SOCKTYPE}
  GIA_EAI_SOCKTYPE   = 10 ; // Socket type ai_socktype not supported.
  {$EXTERNALSYM GIA_EAI_SYSTEM}
  GIA_EAI_SYSTEM     = 11 ; // System error returned in errno.

  {$EXTERNALSYM NI_MAXHOST}
  NI_MAXHOST  = 1025;      // Max size of a fully-qualified domain name.
  {$EXTERNALSYM NI_MAXSERV}
  NI_MAXSERV  =   32;      // Max size of a service name.

  // Flags for getnameinfo().

  {$EXTERNALSYM NI_NOFQDN}
  NI_NOFQDN       =   $1  ;  // Only return nodename portion for local hosts.
  {$EXTERNALSYM NI_NUMERICHOST}
  NI_NUMERICHOST  =   $2  ;  // Return numeric form of the host's address.
  {$EXTERNALSYM NI_NAMEREQD}
  NI_NAMEREQD     =   $4  ;  // Error if the host's name not in DNS.
  {$EXTERNALSYM NI_NUMERICSERV}
  NI_NUMERICSERV  =   $8  ;  // Return numeric form of the service (port #).
  {$EXTERNALSYM NI_DGRAM}
  NI_DGRAM        =   $10 ;  // Service is a datagram service.

  //JPM - These may not be supported in WinCE 4.2
  {$EXTERNALSYM PROTECTION_LEVEL_RESTRICTED}
  PROTECTION_LEVEL_RESTRICTED   = 30;  //* for Intranet apps      /*
  {$EXTERNALSYM PROTECTION_LEVEL_DEFAULT}
  PROTECTION_LEVEL_DEFAULT      = 20;  //* default level          /*
  {$EXTERNALSYM PROTECTION_LEVEL_UNRESTRICTED}
  PROTECTION_LEVEL_UNRESTRICTED = 10;  //* for peer-to-peer apps  /*

  {$EXTERNALSYM SOCKET_SETTINGS_GUARANTEE_ENCRYPTION}
  SOCKET_SETTINGS_GUARANTEE_ENCRYPTION = $00000001;
  {$EXTERNALSYM SOCKET_SETTINGS_ALLOW_INSECURE}
  SOCKET_SETTINGS_ALLOW_INSECURE = $00000002;
  
  {$EXTERNALSYM SOCKET_INFO_CONNECTION_SECURED}
  SOCKET_INFO_CONNECTION_SECURED =  $00000001;
  {$EXTERNALSYM SOCKET_INFO_CONNECTION_ENCRYPTED}
  SOCKET_INFO_CONNECTION_ENCRYPTED = $00000002;

type
  // RLebeau: find a better place for this
  {$IFNDEF HAS_UInt64}
  {$EXTERNALSYM UINT64}
  UINT64 = {$IFDEF HAS_QWord}QWord{$ELSE}Int64{$ENDIF};
  {$ENDIF}

  {$NODEFINE PPaddrinfo}
  PPaddrinfo = ^PAddrInfo;
  {$NODEFINE PPaddrinfoW}
  PPaddrinfoW = ^PAddrInfoW;

  {$IFNDEF WINCE}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL_DEFAULT}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL_IPSEC}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL_INVALID}
  SOCKET_SECURITY_PROTOCOL = (
    SOCKET_SECURITY_PROTOCOL_DEFAULT, SOCKET_SECURITY_PROTOCOL_IPSEC, SOCKET_SECURITY_PROTOCOL_INVALID
    );

  {$EXTERNALSYM SOCKET_SECURITY_SETTINGS_IPSEC}
  SOCKET_SECURITY_SETTINGS_IPSEC = record
    SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
    SecurityFlags : ULONG;
    IpsecFlags : ULONG;
    AuthipMMPolicyKey : TGUID;
    AuthipQMPolicyKey : TGUID;
    Reserved : TGUID;
    Reserved2 : UINT64;
    UserNameStringLen : ULONG;
    DomainNameStringLen : ULONG;
    PasswordStringLen : ULONG;
  //  wchar_t AllStrings[0];
  end;
  {$EXTERNALSYM PSOCKET_SECURITY_SETTINGS_IPSEC}
  PSOCKET_SECURITY_SETTINGS_IPSEC = ^SOCKET_SECURITY_SETTINGS_IPSEC;
  {$EXTERNALSYM SOCKET_SECURITY_SETTINGS}
  SOCKET_SECURITY_SETTINGS = record
    SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
    SecurityFlags : ULONG;
  end;
  {$EXTERNALSYM PSOCKET_SECURITY_SETTINGS}
  PSOCKET_SECURITY_SETTINGS = ^SOCKET_SECURITY_SETTINGS;
  {$EXTERNALSYM SOCKET_PEER_TARGET_NAME}
  SOCKET_PEER_TARGET_NAME = record
    SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
    PeerAddress : SOCKADDR_STORAGE;
    PeerTargetNameStringLen : ULONG;
    //wchar_t AllStrings[0];
  end;
  {$EXTERNALSYM PSOCKET_PEER_TARGET_NAME}
  PSOCKET_PEER_TARGET_NAME = ^SOCKET_PEER_TARGET_NAME;

  {$EXTERNALSYM SOCKET_SECURITY_QUERY_INFO}
  SOCKET_SECURITY_QUERY_INFO = record
     SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
     Flags : ULONG;
     PeerApplicationAccessTokenHandle : UINT64;
     PeerMachineAccessTokenHandle : UINT64;
  end;
  {$EXTERNALSYM PSOCKET_SECURITY_QUERY_INFO}
  PSOCKET_SECURITY_QUERY_INFO = ^SOCKET_SECURITY_QUERY_INFO;
  {$EXTERNALSYM SOCKET_SECURITY_QUERY_TEMPLATE}
  SOCKET_SECURITY_QUERY_TEMPLATE = record
    SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
    PeerAddress : SOCKADDR_STORAGE;
    PeerTokenAccessMask : ULONG;
  end;
  {$EXTERNALSYM PSOCKET_SECURITY_QUERY_TEMPLATE}
  PSOCKET_SECURITY_QUERY_TEMPLATE = ^SOCKET_SECURITY_QUERY_TEMPLATE;

//callback defs
type
  {$EXTERNALSYM LPLOOKUPSERVICE_COMPLETION_ROUTINE}
  LPLOOKUPSERVICE_COMPLETION_ROUTINE = procedure (const dwError, dwBytes : DWORD; lpOverlapped : LPWSAOVERLAPPED); stdcall;
{$ENDIF}

type
  {$EXTERNALSYM LPFN_GETADDRINFO}
  LPFN_GETADDRINFO = function(NodeName: PAnsiChar; ServiceName: PAnsiChar; Hints: Paddrinfo; ppResult: PPaddrinfo): Integer; stdcall;
  {$EXTERNALSYM LPFN_GETADDRINFOW}
  LPFN_GETADDRINFOW = function(NodeName: PWideChar; ServiceName: PWideChar; Hints: PaddrinfoW; ppResult: PPaddrinfoW): Integer; stdcall;
  {$EXTERNALSYM LPFN_GETNAMEINFO}
  //The IPv6 preview for Win2K defines hostlen and servelen as size_t but do not use them
  //for these definitions as the newer SDK's define those as DWORD.
  LPFN_GETNAMEINFO = function(sa: psockaddr; salen: u_int; host: PAnsiChar; hostlen: u_int; serv: PAnsiChar; servlen: u_int; flags: Integer): Integer; stdcall;
  {$EXTERNALSYM LPFN_GETNAMEINFOW}
  LPFN_GETNAMEINFOW = function(sa: psockaddr; salen: u_int; host: PWideChar; hostlen: u_int; serv: PWideChar; servlen: u_int; flags: Integer): Integer; stdcall;
  {$EXTERNALSYM LPFN_FREEADDRINFO}
  LPFN_FREEADDRINFO = procedure(ai: Paddrinfo); stdcall;
  {$EXTERNALSYM LPFN_FREEADDRINFOW}
  LPFN_FREEADDRINFOW = procedure(ai: PaddrinfoW); stdcall;

//function GetAdaptersAddresses( Family:ULONG; Flags:ULONG; Reserved:Pointer; pAdapterAddresses: PIP_ADAPTER_ADDRESSES; pOutBufLen:PULONG):ULONG;stdcall;  external iphlpapi_dll;

{ the following are not used, nor tested}
{function getipnodebyaddr(const src:pointer;  len:integer; af:integer;var error_num:integer) :phostent;stdcall; external Wship6_dll;
procedure freehostent(ptr:phostent);stdcall; external Wship6_dll;
function inet_pton(af:integer; const src:pchar; dst:pointer):integer;stdcall; external Wship6_dll;
function inet_ntop(af:integer; const src:pointer; dst:pchar;size:integer):pchar;stdcall; external Wship6_dll;
}
 {$IFNDEF WINCE}
  {$EXTERNALSYM LPFN_INET_PTON}
  LPFN_INET_PTON = function (af: Integer; const src: PAnsiChar; dst: Pointer): Integer; stdcall;
  {$EXTERNALSYM LPFN_INET_PTONW}
  LPFN_INET_PTONW = function (af: Integer; const src: PWideChar; dst: Pointer): Integer; stdcall;
  {$EXTERNALSYM LPFN_INET_NTOP}
  LPFN_INET_NTOP = function (af: Integer; const src: Pointer; dst: PAnsiChar; size: size_t): PAnsiChar; stdcall;
  {$EXTERNALSYM LPFN_INET_NTOPW}
  LPFN_INET_NTOPW = function (af: Integer; const src: Pointer; dst: PWideChar; size: size_t): PAnsiChar; stdcall;

{ end the following are not used, nor tested}
//These are provided in case we need them later
//Windows Vista
  {$EXTERNALSYM LPFN_GETADDRINFOEXA}
  LPFN_GETADDRINFOEXA = function(pName : PAnsiChar; pServiceName : PAnsiChar;
    const dwNameSpace: DWord; lpNspId : LPGUID; hints : PADDRINFOEXA;
    var ppResult : PADDRINFOEXA; timeout : Ptimeval; lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE;
    lpNameHandle : PHandle) : Integer; stdcall;
  {$EXTERNALSYM LPFN_GETADDRINFOEXW}
  LPFN_GETADDRINFOEXW = function(pName : PWideChar; pServiceName : PWideChar;
    const dwNameSpace: DWord; lpNspId : LPGUID;hints : PADDRINFOEXW;
    var ppResult : PADDRINFOEXW; timeout : Ptimeval; lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE;
    lpNameHandle : PHandle) : Integer; stdcall;
  {$EXTERNALSYM LPFN_SETADDRINFOEXA}
  LPFN_SETADDRINFOEXA= function(pName : PAnsiChar; pServiceName : PAnsiChar;
    pAddresses : PSOCKET_ADDRESS; const dwAddressCount : DWord; lpBlob : LPBLOB;
    const dwFlags : DWord; const dwNameSpace : DWord; lpNspId : LPGUID;
    timeout : Ptimeval;
    lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE; lpNameHandle : PHandle) : Integer; stdcall;
  {$EXTERNALSYM LPFN_SETADDRINFOEXW}
  LPFN_SETADDRINFOEXW= function(pName : PWideChar; pServiceName : PWideChar;
    pAddresses : PSOCKET_ADDRESS; const dwAddressCount : DWord; lpBlob : LPBLOB;
    const dwFlags : DWord; const dwNameSpace : DWord; lpNspId : LPGUID;
    timeout : Ptimeval;
    lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE; lpNameHandle : PHandle) : Integer; stdcall;

  {$EXTERNALSYM LPFN_FREEADDRINFOEX}
  LPFN_FREEADDRINFOEX = procedure(pAddrInfoEx : PADDRINFOEXA) ; stdcall;
  {$EXTERNALSYM LPFN_FREEADDRINFOEXW}
  LPFN_FREEADDRINFOEXW = procedure(pAddrInfoEx : PADDRINFOEXW) ; stdcall;

  {$EXTERNALSYM LPFN_GETADDRINFOEX}
  {$EXTERNALSYM LPFN_SETADDRINFOEX}
  {$IFDEF UNICODE}
  LPFN_GETADDRINFOEX = LPFN_GETADDRINFOEXW;
  LPFN_SETADDRINFOEX = LPFN_SETADDRINFOEXW;
  {$ELSE}
  LPFN_GETADDRINFOEX = LPFN_GETADDRINFOEXA;
  LPFN_SETADDRINFOEX = LPFN_SETADDRINFOEXA;
  {$ENDIF}

  //  Fwpuclnt.dll - API
  {$EXTERNALSYM LPFN_WSASetSocketSecurity}
  LPFN_WSASetSocketSecurity = function (socket : TSocket;
    SecuritySettings : PSOCKET_SECURITY_SETTINGS; const SecuritySettingsLen : ULONG;
    OVERLAPPED : LPWSAOVERLAPPED; CompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Integer; stdcall;
  {$EXTERNALSYM LPFN_WSADELETESOCKETPEERTARGETNAME}
  LPFN_WSADELETESOCKETPEERTARGETNAME = function (Socket : TSocket;
    PeerAddr : Psockaddr; PeerAddrLen : ULONG;
    Overlapped : LPWSAOVERLAPPED;  CompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;
  {$EXTERNALSYM LPFN_WSASETSOCKETPEERTARGETNAME}
  LPFN_WSASETSOCKETPEERTARGETNAME = function (Socket : TSocket;
    PeerTargetName : PSOCKET_PEER_TARGET_NAME; PeerTargetNameLen : ULONG;
    Overlapped : LPWSAOVERLAPPED; CompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Integer; stdcall;
  {$EXTERNALSYM LPFN_WSAIMPERSONATESOCKETPEER}
  LPFN_WSAIMPERSONATESOCKETPEER = function (Socket : TSocket;
    PeerAddress : Psockaddr;  peerAddressLen :  ULONG) : Integer; stdcall;
  {$EXTERNALSYM LPFN_WSAQUERYSOCKETSECURITY}
  LPFN_WSAQUERYSOCKETSECURITY = function (Socket : TSocket;
    SecurityQueryTemplate : PSOCKET_SECURITY_QUERY_TEMPLATE; const SecurityQueryTemplateLen : ULONG;
    SecurityQueryInfo : PSOCKET_SECURITY_QUERY_INFO; var SecurityQueryInfoLen : ULONG;
     Overlapped : LPWSAOVERLAPPED;  CompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Integer; stdcall;
  {$EXTERNALSYM LPFN_WSAREVERTIMPERSONATION}
  LPFN_WSAREVERTIMPERSONATION = function : Integer; stdcall;
{$ENDIF}

const
  {$NODEFINE fn_GetAddrInfo}
  {$NODEFINE fn_getnameinfo}
  {$NODEFINE fn_freeaddrinfo}
  {$IFNDEF WINCE}
  {$NODEFINE fn_GetAddrInfoEx}
  {$NODEFINE fn_SetAddrInfoEx}
  {$NODEFINE fn_FreeAddrInfoEx}
  {$NODEFINE fn_inet_pton}
  {$NODEFINE fn_inet_ntop}
  {$ENDIF}
  {$IFDEF UNICODE}
  // WinCE does not support GetAddrInfoW(), GetNameInfoW(), or FreeAddrInfoW().
  // To support IPv6 on WinCE when UNICODE is defined, we will use our own
  // wrappers that internally call WinCE's functions...
  fn_GetAddrInfo = {$IFDEF WINCE}'getaddrinfo'{$ELSE}'GetAddrInfoW'{$ENDIF};
  fn_getnameinfo = {$IFDEF WINCE}'getnameinfo'{$ELSE}'GetNameInfoW'{$ENDIF};
  fn_freeaddrinfo = {$IFDEF WINCE}'freeaddrinfo'{$ELSE}'FreeAddrInfoW'{$ENDIF};
    {$IFNDEF WINCE}
  fn_GetAddrInfoEx = 'GetAddrInfoExW';
  fn_SetAddrInfoEx = 'SetAddrInfoExW';
  fn_FreeAddrInfoEx = 'FreeAddrInfoExW';
  fn_inet_pton = 'InetPtonW';
  fn_inet_ntop = 'InetNtopW';
    {$ENDIF}
  {$ELSE}
  fn_GetAddrInfo = 'getaddrinfo';
  fn_getnameinfo = 'getnameinfo';
  fn_freeaddrinfo = 'freeaddrinfo';
    {$IFNDEF WINCE}
  fn_GetAddrInfoEx = 'GetAddrInfoExA';
  fn_SetAddrInfoEx = 'SetAddrInfoExA';
  fn_FreeAddrInfoEx = 'FreeAddrInfoEx';
  fn_inet_pton = 'inet_pton';
  fn_inet_ntop = 'inet_ntop';
    {$ENDIF}
  {$ENDIF}

{$UNDEF WINCE_UNICODE}
{$IFDEF WINCE}
  {$IFDEF UNICODE}
    {$DEFINE WINCE_UNICODE}
  {$ENDIF}
{$ENDIF}

var
  {$EXTERNALSYM getaddrinfo}
  {$EXTERNALSYM getnameinfo}
  {$EXTERNALSYM freeaddrinfo}
  {$IFNDEF WINCE}
  {$EXTERNALSYM inet_pton}
  {$EXTERNALSYM inet_ntop}
  {$ENDIF}
  {$IFDEF UNICODE}
    {$IFDEF WINCE}
  getaddrinfoCE: LPFN_GETADDRINFO = nil;
  getnameinfoCE: LPFN_GETNAMEINFO = nil;
  freeaddrinfoCE: LPFN_FREEADDRINFO = nil;
    {$ENDIF}
  getaddrinfo: LPFN_GETADDRINFOW = nil;
  getnameinfo: LPFN_GETNAMEINFOW = nil;
  freeaddrinfo: LPFN_FREEADDRINFOW = nil;
    {$IFNDEF WINCE}
  //These are here for completeness
  inet_pton : LPFN_inet_ptonW = nil;
  inet_ntop : LPFN_inet_ntopW = nil;
    {$ENDIF}
  {$ELSE}
  getaddrinfo: LPFN_GETADDRINFO = nil;
  getnameinfo: LPFN_GETNAMEINFO = nil;
  freeaddrinfo: LPFN_FREEADDRINFO = nil;
      {$IFNDEF WINCE}
  //These are here for completeness
  inet_pton : LPFN_inet_pton = nil;
  inet_ntop : LPFN_inet_ntop = nil;
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF WINCE}
  {
  IMPORTANT!!!

  These are Windows Vista functions and there's no guarantee that you will have
  them so ALWAYS check the function pointer before calling them.
  }
  {$EXTERNALSYM GetAddrInfoEx}
  GetAddrInfoEx :  LPFN_GETADDRINFOEX = nil;
  {$EXTERNALSYM SetAddrInfoEx}
  SetAddrInfoEx : LPFN_SETADDRINFOEX = nil;
  {$EXTERNALSYM FreeAddrInfoEx}
  //You can't alias the LPFN for this because the ASCII version of this
  //does not end with an "a"
  {$IFDEF UNICODE}
  FreeAddrInfoEx : LPFN_FREEADDRINFOEXW = nil;
  {$ELSE}
  FreeAddrInfoEx : LPFN_FREEADDRINFOEX = nil;
  {$ENDIF}

  //Fwpuclnt.dll available for Windows Vista and later
  {$EXTERNALSYM WSASetSocketSecurity}
  WSASetSocketSecurity : LPFN_WSASetSocketSecurity = nil;
  {$EXTERNALSYM WSASETSOCKETPEERTARGETNAME}
  WSASetSocketPeerTargetName : LPFN_WSASETSOCKETPEERTARGETNAME = nil;
  {$EXTERNALSYM WSADELETESOCKETPEERTARGETNAME}
  WSADeleteSocketPeerTargetName : LPFN_WSADELETESOCKETPEERTARGETNAME = nil;
  {$EXTERNALSYM WSAImpersonateSocketPeer}
  WSAImpersonateSocketPeer : LPFN_WSAIMPERSONATESOCKETPEER = nil;
  {$EXTERNALSYM WSAQUERYSOCKETSECURITY}
  WSAQUERYSOCKETSECURITY : LPFN_WSAQUERYSOCKETSECURITY = nil;
  {$EXTERNALSYM WSAREVERTIMPERSONATION}
  WSARevertImpersonation : LPFN_WSAREVERTIMPERSONATION = nil;
  {$ENDIF}

var
  GIdIPv6FuncsAvailable: Boolean = False{$IFDEF HAS_DEPRECATED}{$IFDEF USE_SEMICOLON_BEFORE_DEPRECATED};{$ENDIF} deprecated{$ENDIF};

function gaiErrorToWsaError(const gaiError: Integer): Integer;

//We want to load this library only after loading Winsock and unload immediately
//before unloading Winsock.
procedure InitLibrary;
procedure CloseLibrary;

implementation

uses
  SysUtils
  {$IFDEF HAS_SIZE_T}
  , IdGlobal
  {$ENDIF};

var
  hWship6Dll : THandle = 0; // Wship6.dll handle
  //Use this instead of hWship6Dll because this will point to the correct lib.
  hProcHandle : THandle = 0;
  {$IFNDEF WINCE}
  hfwpuclntDll : THandle = 0;
  {$ENDIF}

function gaiErrorToWsaError(const gaiError: Integer): Integer;
begin
  case gaiError of
    GIA_EAI_ADDRFAMILY: Result := 0; // TODO: find a decent error for here
    GIA_EAI_AGAIN:      Result := WSATRY_AGAIN;
    GIA_EAI_BADFLAGS:   Result := WSAEINVAL;
    GIA_EAI_FAIL:       Result := WSANO_RECOVERY;
    GIA_EAI_FAMILY:     Result := WSAEAFNOSUPPORT;
    GIA_EAI_MEMORY:     Result := WSA_NOT_ENOUGH_MEMORY;
    GIA_EAI_NODATA:     Result := WSANO_DATA;
    GIA_EAI_NONAME:     Result := WSAHOST_NOT_FOUND;
    GIA_EAI_SERVICE:    Result := WSATYPE_NOT_FOUND;
    GIA_EAI_SOCKTYPE:   Result := WSAESOCKTNOSUPPORT;
    GIA_EAI_SYSTEM:
      begin
        Result := 0; // avoid warning
        IndyRaiseLastError;
      end;
    else
      Result := gaiError;
  end;
end;

procedure CloseLibrary;
var
  h : THandle;
begin
  h := InterlockedExchangeTHandle(hWship6Dll, 0);
  if h <> 0 then begin
    FreeLibrary(h);
  end;
  {$IFNDEF WINCE}
  h := InterlockedExchangeTHandle(hfwpuclntDll, 0);
  if h <> 0 then begin
    FreeLibrary(h);
  end;
  {$ENDIF}

  {$I IdSymbolDeprecatedOff.inc}
  GIdIPv6FuncsAvailable := False;
  {$I IdSymbolDeprecatedOn.inc}

  {$IFDEF WINCE_UNICODE}
  getaddrinfoCE := nil;
  getnameinfoCE := nil;
  freeaddrinfoCE := nil;
  {$ENDIF}
  getaddrinfo := nil;
  getnameinfo := nil;
  freeaddrinfo := nil;
  {$IFNDEF WINCE}
  inet_pton := nil;
  inet_ntop := nil;
  GetAddrInfoEx := nil;
  SetAddrInfoEx := nil;
  FreeAddrInfoEx := nil;
  WSASetSocketPeerTargetName := nil;
  WSADeleteSocketPeerTargetName := nil;
  WSAImpersonateSocketPeer := nil;
  WSAQuerySocketSecurity := nil;
  WSARevertImpersonation := nil;
  {$ENDIF}
end;

{$IFDEF FPC} //{$IFDEF STRING_IS_ANSI}
  {$IFDEF UNICODE}

// FreePascal does not have PWideChar overloads of these functions

function StrComp(const Str1, Str2: PWideChar): Integer; overload;
var
  P1, P2: PWideChar;
begin
  P1 := Str1;
  P2 := Str2;
  while True do
  begin
    if (P1^ <> P2^) or (P1^ = #0) then
    begin
      Result := Ord(P1^) - Ord(P2^);
      Exit;
    end;
    Inc(P1);
    Inc(P2);
  end;
  Result := 0;
end;

function StrScan(const Str: PWideChar; Chr: WideChar): PWideChar; overload;
begin
  Result := Str;
  while Result^ <> #0 do
  begin
    if Result^ = Chr then begin
      Exit;
    end;
    Inc(Result);
  end;
  if Chr <> #0 then begin
    Result := nil;
  end;
end;

  {$ENDIF}
{$ENDIF}

// The IPv6 functions were added to the Ws2_32.dll on Windows XP and later.
// To execute an application that uses these functions on earlier versions of
// Windows, the functions are defined as inline functions in the Wspiapi.h file.
// At runtime, the functions are implemented in such a way that if the Ws2_32.dll
// or the Wship6.dll (the file containing the functions in the IPv6 Technology
// Preview for Windows 2000) does not include them, then versions are implemented
// inline based on code in the Wspiapi.h header file. This inline code will be
// used on older Windows platforms that do not natively support the functions.

// RLebeau: Wspiapi.h only defines Ansi versions of the legacy functions, but we
// need to handle Unicode as well...

function WspiapiMalloc(tSize: size_t): Pointer;
begin
  try
    GetMem(Result, tSize);
    ZeroMemory(Result, tSize);
  except
    Result := nil;
  end;
end;

procedure WspiapiFree(p: Pointer);
begin
  FreeMem(p);
end;

procedure WspiapiSwap(var a, b, c: PIdPlatformChar);
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  c := a;
  a := b;
  b := c;
end;

function WspiapiStrdup(const pszString: PIdPlatformChar): PIdPlatformChar; stdcall;
var
  pszMemory: PIdPlatformChar;
  cchMemory: size_t;
begin
  if pszString = nil then begin
    Result := nil;
    Exit;
  end;

  cchMemory := StrLen(pszString) + 1;
  pszMemory := PIdPlatformChar(WspiapiMalloc(cchMemory * SizeOf(TIdPlatformChar)));
  if pszMemory = nil then begin
    Result := nil;
    Exit;
  end;

  StrLCopy(pszMemory, pszString, cchMemory);
  Result := pszMemory;
end;

function WspiapiParseV4Address(const pszAddress: PIdPlatformChar; var pdwAddress: DWORD): BOOL; stdcall;
var
  dwAddress: DWORD;
  pcNext: PIdPlatformChar;
  iCount: Integer;
begin
  iCount := 0;

  // ensure there are 3 '.' (periods)
  pcNext := pszAddress;
  while pcNext^ <> TIdPlatformChar(0) do begin
    if pcNext^ = '.' then begin
      Inc(iCount);
    end;
    Inc(pcNext);
  end;
  if iCount <> 3 then begin
    Result := FALSE;
    Exit;
  end;

  // return an error if dwAddress is INADDR_NONE (255.255.255.255)
  // since this is never a valid argument to getaddrinfo.
  dwAddress := inet_addr(
    {$IFNDEF UNICODE}
    pszAddress
    {$ELSE}
    PAnsiChar(AnsiString(pszAddress))
    {$ENDIF}
  );
  if dwAddress = INADDR_NONE then begin
    Result := FALSE;
    Exit;
  end;

  pdwAddress := dwAddress;
  Result := TRUE;
end;

function WspiapiNewAddrInfo(iSocketType, iProtocol: Integer; wPort: WORD; dwAddress: DWORD): {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF}; stdcall;
var
  ptNew: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF};
  ptAddress: PSockAddrIn;
begin
  // allocate a new addrinfo structure.
  {$IFDEF UNICODE}
  ptNew := PaddrinfoW(WspiapiMalloc(SizeOf(addrinfoW)));
  {$ELSE}
  ptNew := Paddrinfo(WspiapiMalloc(SizeOf(addrinfo)));
  {$ENDIF}
  if ptNew = nil then begin
    Result := nil;
    Exit;
  end;

  ptAddress := PSockAddrIn(WspiapiMalloc(SizeOf(sockaddr_in)));
  if ptAddress = nil then begin
    WspiapiFree(ptNew);
    Result := nil;
    Exit;
  end;
  ptAddress^.sin_family       := AF_INET;
  ptAddress^.sin_port         := wPort;
  ptAddress^.sin_addr.s_addr  := dwAddress;

  // fill in the fields...
  ptNew^.ai_family            := PF_INET;
  ptNew^.ai_socktype          := iSocketType;
  ptNew^.ai_protocol          := iProtocol;
  ptNew^.ai_addrlen           := SizeOf(sockaddr_in);
  ptNew^.ai_addr              := Psockaddr(ptAddress);

  Result := ptNew;
end;

function WspiapiQueryDNS(const pszNodeName: PIdPlatformChar; iSocketType, iProtocol: Integer;
  wPort: WORD; pszAlias: PIdPlatformChar; var pptResult: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF}): Integer; stdcall;
var
  pptNext: {$IFDEF UNICODE}PPaddrinfoW{$ELSE}PPaddrinfo{$ENDIF};
  ptHost: Phostent;
  ppAddresses: ^PInAddr;
begin
  pptNext := @pptResult;

  pptNext^ := nil;
  pszAlias^ := TIdPlatformChar(0);

  ptHost := gethostbyname(
    {$IFNDEF UNICODE}
    pszNodeName
    {$ELSE}
    PAnsiChar(AnsiString(pszNodeName))
    {$ENDIF}
  );
  if ptHost <> nil then begin
    if (ptHost^.h_addrtype = AF_INET) and (ptHost^.h_length = SizeOf(in_addr)) then begin
      ppAddresses := Pointer(ptHost^.h_address_list);
      while ppAddresses^ <> nil do begin
        // create an addrinfo structure...
        pptNext^ := WspiapiNewAddrInfo(iSocketType, iProtocol, wPort, ppAddresses^^.s_addr);
        if pptNext^ = nil then begin
          Result := EAI_MEMORY;
          Exit;
        end;

        pptNext := @((pptNext^)^.ai_next);
        Inc(ppAddresses);
      end;
    end;

    // pick up the canonical name.
    StrLCopy(pszAlias,
      {$IFNDEF UNICODE}
      ptHost^.h_name
      {$ELSE}
      PIdPlatformChar(TIdPlatformString(ptHost^.h_name))
      {$ENDIF}
      , NI_MAXHOST);

    Result := 0;
    Exit;
  end;

  case WSAGetLastError() of
    WSAHOST_NOT_FOUND: Result := EAI_NONAME;
    WSATRY_AGAIN:      Result := EAI_AGAIN;
    WSANO_RECOVERY:    Result := EAI_FAIL;
    WSANO_DATA:        Result := EAI_NODATA;
  else
    Result := EAI_NONAME;
  end;
end;

function WspiapiLookupNode(const pszNodeName: PIdPlatformChar; iSocketType: Integer;
  iProtocol: Integer; wPort: WORD; bAI_CANONNAME: BOOL; var pptResult: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF}): Integer; stdcall;
var
  iError: Integer;
  iAliasCount: Integer;
  szFQDN1: array[0..NI_MAXHOST-1] of TIdPlatformChar;
  szFQDN2: array[0..NI_MAXHOST-1] of TIdPlatformChar;
  pszName: PIdPlatformChar;
  pszAlias: PIdPlatformChar;
  pszScratch: PIdPlatformChar;
begin
  iAliasCount := 0;

  ZeroMemory(@szFQDN1, SizeOf(szFQDN1));
  ZeroMemory(@szFQDN2, SizeOf(szFQDN2));
  pszName := @szFQDN1[0];
  pszAlias := @szFQDN2[0];
  pszScratch := nil;
  StrLCopy(pszName, pszNodeName, NI_MAXHOST);

  repeat
    iError := WspiapiQueryDNS(pszNodeName, iSocketType, iProtocol, wPort, pszAlias, pptResult);
    if iError <> 0 then begin
      Break;
    end;

    // if we found addresses, then we are done.
    if pptResult <> nil then begin
      Break;
    end;

    // stop infinite loops due to DNS misconfiguration.  there appears
    // to be no particular recommended limit in RFCs 1034 and 1035.
    if (StrLen(pszAlias) = 0) or (StrComp(pszName, pszAlias) = 0) then begin
      iError := EAI_FAIL;
      Break;
    end;
    Inc(iAliasCount);
    if iAliasCount = 16 then begin
      iError := EAI_FAIL;
      Break;
    end;

    // there was a new CNAME, look again.
    WspiapiSwap(pszName, pszAlias, pszScratch);
  until False;

  if (iError = 0) and bAI_CANONNAME then begin
    pptResult^.ai_canonname := WspiapiStrdup(pszAlias);
    if pptResult^.ai_canonname = nil then begin
      iError := EAI_MEMORY;
    end;
  end;

  Result := iError;
end;

function WspiapiClone(wPort: WORD; ptResult: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF}): Integer; stdcall;
var
  ptNext, ptNew: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF};
begin
  ptNext := ptResult;
  while ptNext <> nil do begin
    // create an addrinfo structure...
    ptNew := WspiapiNewAddrInfo(SOCK_DGRAM, ptNext^.ai_protocol, wPort, PSockAddrIn(ptNext^.ai_addr)^.sin_addr.s_addr);
    if ptNew = nil then begin
      Break;
    end;

    // link the cloned addrinfo
    ptNew^.ai_next  := ptNext^.ai_next;
    ptNext^.ai_next := ptNew;
    ptNext          := ptNew^.ai_next;
  end;

  if ptNext <> nil then begin
    Result := EAI_MEMORY;
    Exit;
  end;

  Result := 0;
end;

procedure WspiapiLegacyFreeAddrInfo(ptHead: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF}); stdcall;
var
 ptNext: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF};
begin
  ptNext := ptHead;
  while ptNext <> nil do
  begin
    if ptNext^.ai_canonname <> nil then begin
      WspiapiFree(ptNext^.ai_canonname);
    end;
    if ptNext^.ai_addr <> nil then begin
      WspiapiFree(ptNext^.ai_addr);
    end;
    ptHead := ptNext^.ai_next;
    WspiapiFree(ptNext);
    ptNext := ptHead;
  end;
end;

{$IFNDEF HAS_TryStrToInt}
// TODO: use the implementation already in IdGlobalProtocols...
function TryStrToInt(const S: string; out Value: Integer): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF}

function WspiapiLegacyGetAddrInfo(const pszNodeName: PIdPlatformChar; const pszServiceName: PIdPlatformChar;
  const ptHints: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF};
  var pptResult: {$IFDEF UNICODE}PaddrinfoW{$ELSE}Paddrinfo{$ENDIF}): Integer; stdcall;
var
  iError: Integer;
  iFlags: Integer;
  iSocketType: Integer;
  iProtocol: Integer;
  wPort: WORD;
  iTmp: Integer;
  dwAddress: DWORD;
  ptService: Pservent;
  bClone: BOOL;
  wTcpPort: WORD;
  wUdpPort: WORD;
begin
  iError := 0;
  iFlags := 0;
  iSocketType := 0;
  iProtocol := 0;
  wPort := 0;
  dwAddress := 0;
  bClone := FALSE;
  wTcpPort := 0;
  wUdpPort := 0;

  // initialize pptResult with default return value.
  pptResult := nil;

  ////////////////////////////////////////
  // validate arguments...
  //

  // both the node name and the service name can't be NULL.
  if (pszNodeName = nil) and (pszServiceName = nil) then begin
    Result := EAI_NONAME;
    Exit;
  end;

  // validate hints.
  if ptHints <> nil then
  begin
    // all members other than ai_flags, ai_family, ai_socktype
    // and ai_protocol must be zero or a null pointer.
    if (ptHints^.ai_addrlen <> 0) or
       (ptHints^.ai_canonname <> nil) or
       (ptHints^.ai_addr <> nil) or
       (ptHints^.ai_next <> nil) then
    begin
      Result := EAI_FAIL;
      Exit;
    end;

    // the spec has the "bad flags" error code, so presumably we
    // should check something here.  insisting that there aren't
    // any unspecified flags set would break forward compatibility,
    // however.  so we just check for non-sensical combinations.
    //
    // we cannot come up with a canonical name given a null node name.
    iFlags := ptHints^.ai_flags;
    if ((iFlags and AI_CANONNAME) <> 0) and (pszNodeName = nil) then begin
      Result := EAI_BADFLAGS;
      Exit;
    end;

    // we only support a limited number of protocol families.
    if (ptHints^.ai_family <> PF_UNSPEC) and (ptHints^.ai_family <> PF_INET) then begin
      Result := EAI_FAMILY;
      Exit;
    end;

    // we only support only these socket types.
    iSocketType := ptHints^.ai_socktype;
    if (iSocketType <> 0) and
       (iSocketType <> SOCK_STREAM) and
       (iSocketType <> SOCK_DGRAM) and
       (iSocketType <> SOCK_RAW) then
    begin
      Result := EAI_SOCKTYPE;
      Exit;
    end;

    // REVIEW: What if ai_socktype and ai_protocol are at odds?
    iProtocol := ptHints^.ai_protocol;
  end;

  ////////////////////////////////////////
  // do service lookup...

  if pszServiceName <> nil then begin
    if TryStrToInt(pszServiceName, iTmp) and (iTmp >= 0) then begin
      wPort := htons(WORD(iTmp));
      //wTcpPort := wPort; // never used
      wUdpPort := wPort;
      if iSocketType = 0 then begin
        bClone := TRUE;
        iSocketType := SOCK_STREAM;
      end;
    end else
    begin
      if (iSocketType = 0) or (iSocketType = SOCK_DGRAM) then begin
        ptService := getservbyname(
          {$IFNDEF UNICODE}
          pszServiceName
          {$ELSE}
          PAnsiChar(AnsiString(pszServiceName))
          {$ENDIF}
          , 'udp'); {do not localize}
        if ptService <> nil then begin
          wPort := ptService^.s_port;
          wUdpPort := wPort;
        end;
      end;

      if (iSocketType = 0) or (iSocketType = SOCK_STREAM) then begin
        ptService := getservbyname(
          {$IFNDEF UNICODE}
          pszServiceName
          {$ELSE}
          PAnsiChar(AnsiString(pszServiceName))
          {$ENDIF}
          , 'tcp'); {do not localize}
        if ptService <> nil then begin
          wPort := ptService^.s_port;
          wTcpPort := wPort;
        end;
      end;

      // assumes 0 is an invalid service port...
      if wPort = 0 then begin
        Result := iif(iSocketType <> 0, EAI_SERVICE, EAI_NONAME);
        Exit;
      end;

      if iSocketType = 0 then begin
        // if both tcp and udp, process tcp now & clone udp later.
        iSocketType := iif(wTcpPort <> 0, SOCK_STREAM, SOCK_DGRAM);
        bClone := (wTcpPort <> 0) and (wUdpPort <> 0);
      end;
    end;
  end;

  ////////////////////////////////////////
  // do node name lookup...

  // if we weren't given a node name,
  // return the wildcard or loopback address (depending on AI_PASSIVE).
  //
  // if we have a numeric host address string,
  // return the binary address.
  //
  if ((pszNodeName = nil) or WspiapiParseV4Address(pszNodeName, dwAddress)) then begin
    if pszNodeName = nil then begin
      dwAddress := htonl(iif((iFlags and AI_PASSIVE) <> 0, INADDR_ANY, INADDR_LOOPBACK));
    end;

    // create an addrinfo structure...
    pptResult := WspiapiNewAddrInfo(iSocketType, iProtocol, wPort, dwAddress);
    if pptResult = nil then begin
      iError := EAI_MEMORY;
    end;

    if (iError = 0) and (pszNodeName <> nil) then begin
      // implementation specific behavior: set AI_NUMERICHOST
      // to indicate that we got a numeric host address string.
      pptResult^.ai_flags := pptResult^.ai_flags or AI_NUMERICHOST;
      // return the numeric address string as the canonical name
      if (iFlags and AI_CANONNAME) <> 0 then begin
        pptResult^.ai_canonname := WspiapiStrdup(
          {$IFNDEF UNICODE}
          inet_ntoa(PInAddr(@dwAddress)^)
          {$ELSE}
          PWideChar(TIdUnicodeString(inet_ntoa(PInAddr(@dwAddress)^)))
          {$ENDIF}
          );
        if pptResult^.ai_canonname = nil then begin
          iError := EAI_MEMORY;
        end;
      end;
    end;
  end

  // if we do not have a numeric host address string and
  // AI_NUMERICHOST flag is set, return an error!
  else if ((iFlags and AI_NUMERICHOST) <> 0) then begin
    iError := EAI_NONAME;
  end

  // since we have a non-numeric node name,
  // we have to do a regular node name lookup.
  else begin
    iError := WspiapiLookupNode(pszNodeName, iSocketType, iProtocol, wPort, (iFlags and AI_CANONNAME) <> 0, pptResult);
  end;

  if (iError = 0) and bClone then begin
    iError := WspiapiClone(wUdpPort, pptResult);
  end;

  if iError <> 0 then begin
    WspiapiLegacyFreeAddrInfo(pptResult);
    pptResult := nil;
  end;

  Result := iError;
end;

function iif(ATest: Boolean; const ATrue, AFalse: PAnsiChar): PAnsiChar;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ATest then begin
    Result := ATrue;
  end else begin
    Result := AFalse;
  end;
end;

function WspiapiLegacyGetNameInfo(ptSocketAddress: Psockaddr;
  tSocketLength: u_int; pszNodeName: PIdPlatformChar; tNodeLength: size_t;
  pszServiceName: PIdPlatformChar; tServiceLength: size_t; iFlags: Integer): Integer; stdcall;
var
  ptService: Pservent;
  wPort: WORD;
  szBuffer: array[0..5] of TIdPlatformChar;
  pszService: PIdPlatformChar;
  ptHost: Phostent;
  tAddress: in_addr;
  pszNode: PIdPlatformChar;
  pc: PIdPlatformChar;
  {$IFDEF UNICODE}
  tmpService: TIdUnicodeString;
  tmpNode: TIdUnicodeString;
  {$ENDIF}
begin
  StrCopy(szBuffer, '65535');
  pszService := szBuffer;

  // sanity check ptSocketAddress and tSocketLength.
  if (ptSocketAddress = nil) or (tSocketLength < SizeOf(sockaddr)) then begin
    Result := EAI_FAIL;
    Exit;
  end;

  if ptSocketAddress^.sa_family <> AF_INET then begin
    Result := EAI_FAMILY;
    Exit;
  end;

  if tSocketLength < SizeOf(sockaddr_in) then begin
    Result := EAI_FAIL;
    Exit;
  end;

  if (not ((pszNodeName <> nil) and (tNodeLength > 0))) and (not ((pszServiceName <> nil) and (tServiceLength > 0))) then begin
    Result := EAI_NONAME;
    Exit;
  end;

  // the draft has the "bad flags" error code, so presumably we
  // should check something here.  insisting that there aren't
  // any unspecified flags set would break forward compatibility,
  // however.  so we just check for non-sensical combinations.
  if ((iFlags and NI_NUMERICHOST) <> 0) and ((iFlags and NI_NAMEREQD) <> 0) then begin
    Result := EAI_BADFLAGS;
    Exit;
  end;

  // translate the port to a service name (if requested).
  if (pszServiceName <> nil) and (tServiceLength > 0) then begin
    wPort := PSockAddrIn(ptSocketAddress)^.sin_port;

    if (iFlags and NI_NUMERICSERV) <> 0 then begin
      // return numeric form of the address.
      StrPLCopy(szBuffer, IntToStr(ntohs(wPort)), Length(szBuffer));
    end else
    begin
      // return service name corresponding to port.
      ptService := getservbyport(wPort, iif((iFlags and NI_DGRAM) <> 0, 'udp', nil));
      if (ptService <> nil) and (ptService^.s_name <> nil) then begin
        // lookup successful.
        {$IFNDEF UNICODE}
        pszService := ptService^.s_name;
        {$ELSE}
        tmpService := TIdUnicodeString(ptService^.s_name);
        pszService := PWideChar(tmpService);
        {$ENDIF}
      end else begin
        // DRAFT: return numeric form of the port!
        StrPLCopy(szBuffer, IntToStr(ntohs(wPort)), Length(szBuffer));
      end;
    end;

    if tServiceLength > size_t(StrLen(pszService)) then begin
      StrLCopy(pszServiceName, pszService, tServiceLength);
    end else begin
      Result := EAI_FAIL;
      Exit;
    end;
  end;

  // translate the address to a node name (if requested).
  if (pszNodeName <> nil) and (tNodeLength > 0) then begin
    // this is the IPv4-only version, so we have an IPv4 address.
    tAddress := PSockAddrIn(ptSocketAddress)^.sin_addr;

    if (iFlags and NI_NUMERICHOST) <> 0 then begin
      // return numeric form of the address.
      {$IFNDEF UNICODE}
      pszNode := inet_ntoa(tAddress);
      {$ELSE}
      tmpNode := TIdUnicodeString(inet_ntoa(tAddress));
      pszNode := PWideChar(tmpNode);
      {$ENDIF}
    end else
    begin
      // return node name corresponding to address.
      ptHost := gethostbyaddr(PAnsiChar(@tAddress), SizeOf(in_addr), AF_INET);
      if (ptHost <> nil) and (ptHost^.h_name <> nil) then begin
        // DNS lookup successful.
        // stop copying at a "." if NI_NOFQDN is specified.
        {$IFNDEF UNICODE}
        pszNode := ptHost^.h_name;
        {$ELSE}
        tmpNode := TIdUnicodeString(ptHost^.h_name);
        pszNode := PWideChar(tmpNode);
        {$ENDIF}
        if (iFlags and NI_NOFQDN) <> 0 then begin
          pc := StrScan(pszNode, '.');
          if pc <> nil then begin
            pc^ := TIdPlatformChar(0);
          end;
        end;
      end else
      begin
        // DNS lookup failed.  return numeric form of the address.
        if (iFlags and NI_NAMEREQD) <> 0 then begin
          case WSAGetLastError() of
            WSAHOST_NOT_FOUND: Result := EAI_NONAME;
            WSATRY_AGAIN:      Result := EAI_AGAIN;
            WSANO_RECOVERY:    Result := EAI_FAIL;
          else
            Result := EAI_NONAME;
          end;
          Exit;
        end else begin
          {$IFNDEF UNICODE}
          pszNode := inet_ntoa(tAddress);
          {$ELSE}
          tmpNode := TIdUnicodeString(inet_ntoa(tAddress));
          pszNode := PWideChar(tmpNode);
          {$ENDIF}
        end;
      end;
    end;

    if tNodeLength > size_t(StrLen(pszNode)) then begin
      StrLCopy(pszNodeName, pszNode, tNodeLength);
    end else begin
      Result := EAI_FAIL;
      Exit;
    end;
  end;

  Result := 0;
end;

{$IFDEF WINCE_UNICODE}

function IndyStrdupAToW(const pszString: PAnsiChar): PWideChar;
var
  szStr: TIdUnicodeString;
  pszMemory: PWideChar;
  cchMemory: size_t;
begin
  if pszString = nil then begin
    Result := nil;
    Exit;
  end;

  szStr := TIdUnicodeString(pszString);
  cchMemory := Length(szStr) + 1;

  pszMemory := PWideChar(WspiapiMalloc(cchMemory * SizeOf(WideChar)));
  if pszMemory = nil then begin
    Result := nil;
    Exit;
  end;

  StrLCopy(pszMemory, PWideChar(szStr), cchMemory);
  Result := pszMemory;
end;

procedure IndyFreeAddrInfoW(ptHead: PaddrinfoW); stdcall;
var
 ptNext: PaddrinfoW;
begin
  ptNext := ptHead;
  while ptNext <> nil do
  begin
    if ptNext^.ai_canonname <> nil then begin
      WspiapiFree(ptNext^.ai_canonname);
    end;
    if ptNext^.ai_addr <> nil then begin
      WspiapiFree(ptNext^.ai_addr);
    end;
    ptHead := ptNext^.ai_next;
    WspiapiFree(ptNext);
    ptNext := ptHead;
  end;
end;

function IndyAddrInfoConvert(AddrInfo: Paddrinfo): PaddrinfoW;
var
  ptNew: PaddrinfoW;
  ptAddress: Pointer;
begin
  Result := nil;

  if AddrInfo = nil then begin
    Exit;
  end;

  // allocate a new addrinfo structure.
  ptNew := PaddrinfoW(WspiapiMalloc(SizeOf(addrinfoW)));
  if ptNew = nil then begin
    WspiapiFree(ptNew);
    Exit;
  end;

  ptAddress := WspiapiMalloc(AddrInfo^.ai_addrlen);
  if ptAddress = nil then begin
    WspiapiFree(ptNew);
    Exit;
  end;
  Move(AddrInfo^.ai_addr^, ptAddress^, AddrInfo^.ai_addrlen);

  // fill in the fields...
  ptNew^.ai_flags             := AddrInfo^.ai_flags;
  ptNew^.ai_family            := AddrInfo^.ai_family;
  ptNew^.ai_socktype          := AddrInfo^.ai_socktype;
  ptNew^.ai_protocol          := AddrInfo^.ai_protocol;
  ptNew^.ai_addrlen           := AddrInfo^.ai_addrlen;
  ptNew^.ai_canonname         := nil;
  ptNew^.ai_addr              := Psockaddr(ptAddress);
  ptNew^.ai_next              := nil;

  if AddrInfo^.ai_canonname <> nil then begin
    ptNew^.ai_canonname := IndyStrdupAToW(AddrInfo^.ai_canonname);
    if ptNew^.ai_canonname = nil then begin
      IndyFreeAddrInfoW(ptNew);
      Exit;
    end;
  end;

  if AddrInfo^.ai_next <> nil then begin
    ptNew^.ai_next := IndyAddrInfoConvert(AddrInfo^.ai_next);
    if ptNew^.ai_next = nil then begin
      IndyFreeAddrInfoW(ptNew);
      Exit;
    end;
  end;

  Result := ptNew;
end;

function IndyGetAddrInfoW(const pszNodeName: PWideChar; const pszServiceName: PWideChar;
  const ptHints: PaddrinfoW; var pptResult: PaddrinfoW): Integer; stdcall;
var
  LNodeName: AnsiString;
  LPNodeName: PAnsiChar;
  LServiceName: AnsiString;
  LPServiceName: PAnsiChar;
  LHints: addrinfo;
  LPHints: Paddrinfo;
  LResult: Paddrinfo;
begin
  // initialize pptResult with default return value.
  pptResult := nil;

  if pszNodeName <> nil then begin
    LNodeName := AnsiString(pszNodeName);
    LPNodeName := PAnsiChar(LNodeName);
  end else begin
    LPNodeName := nil;
  end;

  if pszServiceName <> nil then begin
    LServiceName := AnsiString(pszServiceName);
    LPServiceName := PAnsiChar(LServiceName);
  end else begin
    LPServiceName := nil;
  end;

  if ptHints <> nil then begin
    ZeroMemory(@LHints, SizeOf(LHints));
    LHints.ai_flags     := ptHints^.ai_flags;
    LHints.ai_family    := ptHints^.ai_family;
    LHints.ai_socktype  := ptHints^.ai_socktype;
    LHints.ai_protocol  := ptHints^.ai_protocol;
    LPHints := @LHints;
  end else begin
    LPHints := nil;
  end;

  Result := getaddrinfoCE(LPNodeName, LPServiceName, LPHints, @LResult);
  if Result = 0 then begin
    try
      pptResult := IndyAddrInfoConvert(LResult);
    finally
      freeaddrinfoCE(LResult);
    end;
    if pptResult = nil then begin
      Result := EAI_MEMORY;
    end;
  end;
end;

function IndyGetNameInfoW(ptSocketAddress: Psockaddr; tSocketLength: u_int;
  pszNodeName: PWideChar; tNodeLength: size_t; pszServiceName: PWideChar;
  tServiceLength: size_t; iFlags: Integer): Integer; stdcall;
var
  LHost: array[0..NI_MAXHOST-1] of AnsiChar;
  LPHost: PAnsiChar;
  LHostLen: u_int;
  LServ: array[0..NI_MAXSERV-1] of AnsiChar;
  LPServ: PAnsiChar;
  LServLen: u_int;
begin
  if pszNodeName <> nil then
  begin
    LPHost := @LHost[0];
    LHostLen := Length(LHost);
  end else begin
    LPHost := nil;
    LHostLen := 0;
  end;

  if pszServiceName <> nil then
  begin
    LPServ := @LServ[0];
    LServLen := Length(LServ);
  end else begin
    LPServ := nil;
    LServLen := 0;
  end;

  Result := getnameinfoCE(ptSocketAddress, tSocketLength, LPHost, LHostLen, LPServ, LServLen, iFlags);
  if Result = 0 then begin
    if pszNodeName <> nil then begin
      StrPLCopy(pszNodeName, TIdUnicodeString(LPHost), tNodeLength);
    end;
    if pszServiceName <> nil then begin
      StrPLCopy(pszServiceName, TIdUnicodeString(LPServ), tServiceLength);
    end;
  end;
end;

{$ENDIF}

procedure InitLibrary;
var
  {$IFDEF WINCE_UNICODE}
  gai: LPFN_GETADDRINFO;
  gni: LPFN_GETNAMEINFO;
  fai: LPFN_FREEADDRINFO;
  {$ELSE}
  gai: {$IFDEF UNICODE}LPFN_GETADDRINFOW{$ELSE}LPFN_GETADDRINFO{$ENDIF};
  gni: {$IFDEF UNICODE}LPFN_GETNAMEINFOW{$ELSE}LPFN_GETNAMEINFO{$ENDIF};
  fai: {$IFDEF UNICODE}LPFN_FREEADDRINFOW{$ELSE}LPFN_FREEADDRINFO{$ENDIF};
  {$ENDIF}
begin
{
IMPORTANT!!!

I am doing things this way because the functions we want are probably in
the Winsock2 dll.  If they are not there, only then do you actually want
to try the Wship6.dll.   I know it's a mess but I found that the functions
may not load if they aren't in Wship6.dll (and they aren't there in some
versions of Windows).

hProcHandle provides a transparant way of managing the two possible library
locations.  hWship6Dll is kept so we can unload the Wship6.dll if necessary.
}
  //Winsock2 has to be loaded by IdWinsock first.
  if not IdWinsock2.Winsock2Loaded then
  begin
    IdWinsock2.InitializeWinSock;
  end;
  hProcHandle := IdWinsock2.WinsockHandle;

  gai := GetProcAddress(hProcHandle, fn_getaddrinfo);
  if not Assigned(gai) then
  begin
    hWship6Dll := SafeLoadLibrary(Wship6_dll);
    hProcHandle := hWship6Dll;
    gai := GetProcAddress(hProcHandle, fn_getaddrinfo);  {do not localize}
  end;

  if Assigned(gai) then
  begin
    gni := GetProcAddress(hProcHandle, fn_getnameinfo);  {do not localize}
    if Assigned(gni) then
    begin
      fai := GetProcAddress(hProcHandle, fn_freeaddrinfo);  {do not localize}
      if Assigned(fai) then
      begin
        {$IFDEF WINCE_UNICODE}
        getaddrinfoCE := gai;
        getnameinfoCE := gni;
        freeaddrinfoCE := fai;
        getaddrinfo := @IndyGetAddrInfoW;
        getnameinfo := @IndyGetNameInfoW;
        freeaddrinfo := @IndyFreeAddrInfoW;
        {$ELSE}
        getaddrinfo := gai;
        getnameinfo := gni;
        freeaddrinfo := fai;
        {$ENDIF}

        //Additional functions should be initialized here.
        {$IFNDEF WINCE}
        inet_pton := GetProcAddress(hProcHandle, fn_inet_pton);  {do not localize}
        inet_ntop := GetProcAddress(hProcHandle, fn_inet_ntop);  {do not localize}
        GetAddrInfoEx := GetProcAddress(hProcHandle, fn_GetAddrInfoEx); {Do not localize}
        SetAddrInfoEx := GetProcAddress(hProcHandle, fn_SetAddrInfoEx); {Do not localize}
        FreeAddrInfoEx := GetProcAddress(hProcHandle, fn_FreeAddrInfoEx); {Do not localize}
        hfwpuclntDll := SafeLoadLibrary(fwpuclnt_dll);
        if hfwpuclntDll <> 0 then
        begin
          WSASetSocketSecurity := GetProcAddress(hfwpuclntDll,
             'WSASetSocketSecurity');
          WSAQuerySocketSecurity := GetProcAddress(hfwpuclntDll, 'WSAQuerySocketSecurity'); {Do not localize}
          WSASetSocketPeerTargetName := GetProcAddress(hfwpuclntDll, 'WSASetSocketPeerTargetName'); {Do not localize}
          WSADeleteSocketPeerTargetName := GetProcAddress(hfwpuclntDll, 'WSADeleteSocketPeerTargetName');  {Do not localize}
          WSAImpersonateSocketPeer := GetProcAddress(hfwpuclntDll, 'WSAImpersonateSocketPeer'); {Do not localize}

          WSARevertImpersonation := GetProcAddress(hfwpuclntDll, 'WSARevertImpersonation'); {Do not localize}
        end;
        {$ENDIF}

        Exit;
      end;
    end;
  end;

  CloseLibrary;

  getaddrinfo := Addr(WspiapiLegacyGetAddrInfo);
  getnameinfo := Addr(WspiapiLegacyGetNameInfo);
  freeaddrinfo := Addr(WspiapiLegacyFreeAddrInfo);

  {$I IdSymbolDeprecatedOff.inc}
  GIdIPv6FuncsAvailable := True;
  {$I IdSymbolDeprecatedOn.inc}
end;

initialization
finalization
  CloseLibrary;

end.

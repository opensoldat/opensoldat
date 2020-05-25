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
  Rev 1.7    1/17/2005 7:25:48 PM  JPMugaas
  Moved some stack management code here to so that we can reuse it in
  non-TIdComponent classes.
  Made HostToNetwork and NetworkToHost byte order overload functions for IPv6
  addresses.

  Rev 1.6    10/26/2004 8:12:30 PM  JPMugaas
  Now uses TIdStrings and TIdStringList for portability.

  Rev 1.5    6/30/2004 12:41:14 PM  BGooijen
  Added SetStackClass

  Rev 1.4    6/11/2004 8:28:50 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.3    4/18/04 2:45:38 PM  RLebeau
  Conversion support for Int64 values

  Rev 1.2    2004.03.07 11:45:22 AM  czhower
  Flushbuffer fix + other minor ones found

  Rev 1.1    3/6/2004 5:16:20 PM  JPMugaas
  Bug 67 fixes.  Do not write to const values.

  Rev 1.0    2004.02.03 3:14:42 PM  czhower
  Move and updates

  Rev 1.39    2/1/2004 6:10:50 PM  JPMugaas
  GetSockOpt.

  Rev 1.38    2/1/2004 3:28:24 AM  JPMugaas
  Changed WSGetLocalAddress to GetLocalAddress and moved into IdStack since
  that will work the same in the DotNET as elsewhere.  This is required to
  reenable IPWatch.

  Rev 1.37    2/1/2004 1:54:56 AM  JPMugaas
  Missapplied fix.  IP 0.0.0.0 should now be accepted.

  Rev 1.36    1/31/2004 4:39:12 PM  JPMugaas
  Removed empty methods.

  Rev 1.35    1/31/2004 1:13:04 PM  JPMugaas
  Minor stack changes required as DotNET does support getting all IP addresses
  just like the other stacks.

  Rev 1.34    2004.01.22 5:59:10 PM  czhower
  IdCriticalSection

  Rev 1.33    1/18/2004 11:15:52 AM  JPMugaas
  IsIP was not handling "0" in an IP address.  This caused the address
  "127.0.0.1" to be treated as a hostname.

  Rev 1.32    12/4/2003 3:14:50 PM  BGooijen
  Added HostByAddress

  Rev 1.31    1/3/2004 12:21:44 AM  BGooijen
  Added function SupportsIPv6

  Rev 1.30    12/31/2003 9:54:16 PM  BGooijen
  Added IPv6 support

  Rev 1.29    2003.12.31 3:47:42 PM  czhower
  Changed to use TextIsSame

  Rev 1.28    10/21/2003 9:24:32 PM  BGooijen
  Started on SendTo, ReceiveFrom

  Rev 1.27    10/19/2003 5:21:28 PM  BGooijen
  SetSocketOption

  Rev 1.26    10/15/2003 7:21:02 PM  DSiders
  Added resource strings in TIdStack.Make.

  Rev 1.25    2003.10.11 5:51:02 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.24    10/5/2003 9:55:30 PM  BGooijen
  TIdTCPServer works on D7 and DotNet now

  Rev 1.23    04/10/2003 22:31:56  HHariri
  moving of WSNXXX method to IdStack and renaming of the DotNet ones

  Rev 1.22    10/2/2003 7:31:18 PM  BGooijen
  .net

  Rev 1.21    10/2/2003 6:05:16 PM  GGrieve
  DontNet

  Rev 1.20    2003.10.02 10:16:30 AM  czhower
  .Net

  Rev 1.19    2003.10.01 9:11:20 PM  czhower
  .Net

  Rev 1.18    2003.10.01 5:05:16 PM  czhower
  .Net

  Rev 1.17    2003.10.01 2:30:40 PM  czhower
  .Net

  Rev 1.16    2003.10.01 12:30:08 PM  czhower
  .Net

  Rev 1.14    2003.10.01 1:37:36 AM  czhower
  .Net

  Rev 1.12    9/30/2003 7:15:46 PM  BGooijen
  IdCompilerDefines.inc is included now

  Rev 1.11    2003.09.30 1:23:04 PM  czhower
  Stack split for DotNet
}

unit IdStack;

interface

{$I IdCompilerDefines.inc}

uses
  Classes,
  IdException, IdStackConsts, IdGlobal, SysUtils;

type
  EIdSocketError = class(EIdException)
  protected
    FLastError: Integer;
  public
    // Params must be in this order to avoid conflict with CreateHelp
    // constructor in CBuilder as CB does not differentiate constructors
    // by name as Delphi does
    constructor CreateError(const AErr: Integer; const AMsg: string); virtual;
    //
    property LastError: Integer read FLastError;
  end;

  { resolving hostnames }
  EIdStackError = class (EIdException);
  EIdIPVersionUnsupported = class (EIdStackError);
  {$IFDEF UNIX}
  EIdResolveError = class(EIdSocketError);
  EIdReverseResolveError = class(EIdSocketError);
  EIdMaliciousPtrRecord = class(EIdReverseResolveError);
  {$ELSE}
  EIdMaliciousPtrRecord = class(EIdSocketError);
  {$ENDIF}

  EIdNotASocket = class(EIdSocketError);

  // TODO: move this to IdStackVCLPosix...
  {$IFDEF USE_VCL_POSIX}
    {$IFDEF ANDROID}
  EIdAndroidPermissionNeeded = class(EIdSocketError);
  EIdInternetPermissionNeeded = class(EIdAndroidPermissionNeeded);
    {$ENDIF}
  {$ENDIF}

  TIdServeFile = function(ASocket: TIdStackSocketHandle; const AFileName: string): Int64;

  TIdPacketInfo = class
  protected
    FSourceIP: String;
    FSourcePort : TIdPort;
    FSourceIF: UInt32;
    FSourceIPVersion: TIdIPVersion;
    FDestIP: String;
    FDestPort : TIdPort;
    FDestIF: UInt32;
    FDestIPVersion: TIdIPVersion;
    FTTL: Byte;
  public
    procedure Reset;

    property TTL : Byte read FTTL write FTTL;
    //The computer that sent it to you
    property SourceIP : String read FSourceIP write FSourceIP;
    property SourcePort : TIdPort read FSourcePort write FSourcePort;
    property SourceIF : UInt32 read FSourceIF write FSourceIF;
    property SourceIPVersion : TIdIPVersion read FSourceIPVersion write FSourceIPVersion;
    //you, the receiver - this is provided for multihomed machines
    property DestIP : String read FDestIP write FDestIP;
    property DestPort : TIdPort read FDestPort write FDestPort;
    property DestIF : UInt32 read FDestIF write FDestIF;
    property DestIPVersion : TIdIPVersion read FDestIPVersion write FDestIPVersion;
  end;

  TIdSocketListClass = class of TIdSocketList;

  // Descend from only TObject. This objects is created a lot and should be fast
  // and small
  TIdSocketList = class(TObject)
  protected
    FLock: TIdCriticalSection;
    //
    function GetItem(AIndex: Integer): TIdStackSocketHandle; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AHandle: TIdStackSocketHandle); virtual; abstract;
    function Clone: TIdSocketList; virtual; abstract;
    function Count: Integer; virtual; abstract;
    class function CreateSocketList: TIdSocketList;
    property Items[AIndex: Integer]: TIdStackSocketHandle read GetItem; default;
    procedure Remove(AHandle: TIdStackSocketHandle); virtual; abstract;
    procedure Clear; virtual; abstract;
    function ContainsSocket(AHandle: TIdStackSocketHandle): boolean; virtual; abstract;
    procedure Lock;
    class function Select(AReadList: TIdSocketList; AWriteList: TIdSocketList;
     AExceptList: TIdSocketList; const ATimeout: Integer = IdTimeoutInfinite): Boolean; virtual;
    function SelectRead(const ATimeout: Integer = IdTimeoutInfinite): Boolean; virtual; abstract;
    function  SelectReadList(var VSocketList: TIdSocketList; const ATimeout: Integer = IdTimeoutInfinite): Boolean; virtual; abstract;
    procedure Unlock;
  end;

  TIdStackLocalAddress = class(TCollectionItem)
  protected
    FIPVersion: TIdIPVersion;
    FIPAddress: String;
  public
    constructor Create(ACollection: TCollection; const AIPVersion: TIdIPVersion; const AIPAddress: string); reintroduce;
    property IPVersion: TIdIPVersion read FIPVersion;
    property IPAddress: String read FIPAddress;
  end;

  TIdStackLocalAddressIPv4 = class(TIdStackLocalAddress)
  protected
    FSubNetMask: String;
  public
    constructor Create(ACollection: TCollection; const AIPAddress, ASubNetMask: string); reintroduce;
    property SubNetMask: String read FSubNetMask;
    // TODO: add BroadcastIP
  end;

  TIdStackLocalAddressIPv6 = class(TIdStackLocalAddress)
  public
    constructor Create(ACollection: TCollection; const AIPAddress: string); reintroduce;
  end;

  TIdStackLocalAddressList = class(TCollection)
  protected
    function GetAddress(AIndex: Integer): TIdStackLocalAddress;
  public
    constructor Create; reintroduce;
    function IndexOfIP(const AIP: String): Integer; overload;
    function IndexOfIP(const AIP: String; AIPVersion: TIdIPVersion): Integer; overload;
    property Addresses[AIndex: Integer]: TIdStackLocalAddress read GetAddress; default;
  end;

  TIdStack = class(TObject)
  protected
    FLocalAddresses: TStrings;
    //
    procedure IPVersionUnsupported;
    function HostByName(const AHostName: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string; virtual; abstract;
    function MakeCanonicalIPv6Address(const AAddr: string): string; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IdGlobal.MakeCanonicalIPv6Address()'{$ENDIF};{$ENDIF}
    function ReadHostName: string; virtual; abstract;
    function GetLocalAddress: string;
    function GetLocalAddresses: TStrings;
  public
    function Accept(ASocket: TIdStackSocketHandle; var VIP: string; var VPort: TIdPort): TIdStackSocketHandle; overload;
    function Accept(ASocket: TIdStackSocketHandle; var VIP: string; var VPort: TIdPort;
      var VIPVersion: TIdIPVersion): TIdStackSocketHandle; overload; virtual; abstract;
    procedure Bind(ASocket: TIdStackSocketHandle; const AIP: string;
              const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION ); virtual; abstract;
    procedure Connect(const ASocket: TIdStackSocketHandle; const AIP: string;
              const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Disconnect(ASocket: TIdStackSocketHandle); virtual; abstract;
    function IOControl(const s: TIdStackSocketHandle; const cmd: UInt32;
      var arg: UInt32): Integer; virtual; abstract;
    class procedure IncUsage; //create stack if necessary and inc counter
    class procedure DecUsage; //decrement counter and free if it gets to zero
    procedure GetPeerName(ASocket: TIdStackSocketHandle; var VIP: string;
      var VPort: TIdPort); overload;
    procedure GetPeerName(ASocket: TIdStackSocketHandle; var VIP: string;
      var VPort: TIdPort; var VIPVersion: TIdIPVersion); overload; virtual; abstract;
    procedure GetSocketName(ASocket: TIdStackSocketHandle; var VIP: string;
      var VPort: TIdPort); overload;
    procedure GetSocketName(ASocket: TIdStackSocketHandle; var VIP: string;
      var VPort: TIdPort; var VIPVersion: TIdIPVersion); overload; virtual; abstract;
    function HostByAddress(const AAddress: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string; virtual; abstract;
    function HostToNetwork(AValue: UInt16): UInt16; overload; virtual; abstract;
    function HostToNetwork(AValue: UInt32): UInt32; overload; virtual; abstract;
    function HostToNetwork(AValue: TIdUInt64): TIdUInt64; overload; virtual; abstract;
    function HostToNetwork(const AValue: TIdIPv6Address): TIdIPv6Address; overload; virtual;
    function IsIP(AIP: string): Boolean;
    procedure Listen(ASocket: TIdStackSocketHandle; ABackLog: Integer); virtual; abstract;
    function WSGetLastError: Integer; virtual; abstract;
    procedure WSSetLastError(const AErr : Integer); virtual; abstract;
    function WSTranslateSocketErrorMsg(const AErr: integer): string; virtual;
    function CheckForSocketError(const AResult: Integer): Integer; overload;
    function CheckForSocketError(const AResult: Integer; const AIgnore: array of Integer): Integer; overload;
    procedure RaiseLastSocketError;
    procedure RaiseSocketError(AErr: integer); virtual;
    function NewSocketHandle(const ASocketType: TIdSocketType; const AProtocol: TIdSocketProtocol;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION; const AOverlapped: Boolean = False)
      : TIdStackSocketHandle; virtual; abstract;
    function NetworkToHost(AValue: UInt16): UInt16; overload; virtual; abstract;
    function NetworkToHost(AValue: UInt32): UInt32; overload; virtual; abstract;
    function NetworkToHost(AValue: TIdUInt64): TIdUInt64; overload; virtual; abstract;
    function NetworkToHost(const AValue: TIdIPv6Address): TIdIPv6Address; overload; virtual;
    procedure GetSocketOption(ASocket: TIdStackSocketHandle;
      ALevel: TIdSocketOptionLevel; AOptName: TIdSocketOption;
      out AOptVal: Integer); overload; virtual; abstract;
    procedure SetSocketOption(ASocket: TIdStackSocketHandle; ALevel: TIdSocketOptionLevel;
      AOptName: TIdSocketOption; AOptVal: Integer); overload; virtual; abstract;
    function ResolveHost(const AHost: string;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string;

    // Result:
    // > 0: Number of bytes received
    //   0: Connection closed gracefully
    // Will raise exceptions in other cases
    function Receive(ASocket: TIdStackSocketHandle; var VBuffer: TIdBytes): Integer; virtual; abstract;
    function Send(ASocket: TIdStackSocketHandle; const ABuffer: TIdBytes;
      const AOffset: Integer = 0; const ASize: Integer = -1): Integer; virtual; abstract;

    function ReceiveFrom(ASocket: TIdStackSocketHandle; var VBuffer: TIdBytes;
      var VIP: string; var VPort: TIdPort; var VIPVersion: TIdIPVersion): Integer; virtual; abstract;
    function SendTo(ASocket: TIdStackSocketHandle; const ABuffer: TIdBytes;
      const AOffset: Integer; const AIP: string; const APort: TIdPort;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): Integer; overload;
    function SendTo(ASocket: TIdStackSocketHandle; const ABuffer: TIdBytes;
      const AOffset: Integer; const ASize: Integer; const AIP: string;
      const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION)
      : Integer; overload; virtual; abstract;
    function ReceiveMsg(ASocket: TIdStackSocketHandle; var VBuffer: TIdBytes;
      APkt: TIdPacketInfo): UInt32; virtual; abstract;
    function SupportsIPv4: Boolean; virtual; abstract;
    function SupportsIPv6: Boolean; virtual; abstract;

    //multicast stuff Kudzu permitted me to add here.
    function IsValidIPv4MulticastGroup(const Value: string): Boolean;
    function IsValidIPv6MulticastGroup(const Value: string): Boolean;
    procedure SetKeepAliveValues(ASocket: TIdStackSocketHandle;
      const AEnabled: Boolean; const ATimeMS, AInterval: Integer); virtual;
    procedure SetMulticastTTL(AHandle: TIdStackSocketHandle;
      const AValue : Byte; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); virtual; abstract;
    procedure SetLoopBack(AHandle: TIdStackSocketHandle; const AValue: Boolean;
      const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); virtual; abstract;
    procedure DropMulticastMembership(AHandle: TIdStackSocketHandle;
      const AGroupIP, ALocalIP : String; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); virtual; abstract;
    procedure AddMulticastMembership(AHandle: TIdStackSocketHandle;
      const AGroupIP, ALocalIP : String; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); virtual; abstract;
    //I know this looks like an odd place to put a function for calculating a
    //packet checksum.  There is a reason for it though.  The reason is that
    //you need it for ICMPv6 and in Windows, you do that with some other stuff
    //in the stack descendants
    function CalcCheckSum(const AData : TIdBytes): UInt16; virtual;
    //In Windows, this writes a checksum into a buffer.  In Linux, it would probably
    //simply have the kernal write the checksum with something like this (RFC 2292):
    //
    //    int  offset = 2;
    //    setsockopt(fd, IPPROTO_IPV6, IPV6_CHECKSUM, &offset, sizeof(offset));
    //
    //  Note that this should be called
    //IMMEDIATELY before you do a SendTo because the Local IPv6 address might change
    procedure WriteChecksum(s : TIdStackSocketHandle;
      var VBuffer : TIdBytes; const AOffset : Integer; const AIP : String;
      const APort : TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); virtual; abstract;
    //
    procedure AddLocalAddressesToList(AAddresses: TStrings); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'use GetLocalAddressList()'{$ENDIF};{$ENDIF}
    procedure GetLocalAddressList(AAddresses: TIdStackLocalAddressList); virtual; abstract;
    //
    // Properties
    //
    property HostName: string read ReadHostName;
    property LocalAddress: string read GetLocalAddress; // {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'use GetLocalAddressList()'{$ENDIF};{$ENDIF}
    property LocalAddresses: TStrings read GetLocalAddresses; // {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'use GetLocalAddressList()'{$ENDIF};{$ENDIF}
  end;

  TIdStackClass = class of TIdStack;

var
  GStack: TIdStack = nil;
  GServeFileProc: TIdServeFile = nil;
  GSocketListClass: TIdSocketListClass;

// Procedures
  procedure SetStackClass( AStackClass: TIdStackClass );

// TODO: move this to IdStackVCLPosix...
{$IFDEF USE_VCL_POSIX}
  {$IFDEF ANDROID}
function HasAndroidPermission(const Permission: string): Boolean;
  {$ENDIF}
{$ENDIF}

implementation

{$O-}

uses
  //done this way so we can have a separate stack for FPC under Unix systems
  {$IFDEF DOTNET}
  IdStackDotNet,
  {$ELSE}
    {$IFDEF WINDOWS}
      {$IFDEF USE_INLINE}
  Windows,
      {$ENDIF}
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

  // TODO: move this to IdStackVCLPosix...
  {$IFDEF USE_VCL_POSIX}
    {$IFDEF ANDROID}
      {$IFNDEF VCL_XE6_OR_ABOVE}
  // StringToJString() is here in XE5
  Androidapi.JNI.JavaTypes,
      {$ENDIF}
      {$IFNDEF VCL_XE7_OR_ABOVE}
  // SharedActivityContext() is here in XE5 and XE6
  FMX.Helpers.Android,
      {$ENDIF}
      {$IFDEF VCL_XE6_OR_ABOVE}
        {$IFDEF VCL_10_0_SEATTLE_OR_ABOVE}
  // StringToJString() is inline in Seattle and later, so we need JavaTypes again...
  Androidapi.JNI.JavaTypes,
        {$ENDIF}
  // StringToJString() was moved here in XE6
  // SharedActivityContext() was moved here in XE7
  // TAndroidHelper was added here in Seattle
  Androidapi.Helpers,
      {$ENDIF}
  Androidapi.JNI.GraphicsContentViewText,
    {$ENDIF}
  {$ENDIF}

  IdResourceStrings;

var
  GStackClass: TIdStackClass = nil;

var
  {$IFNDEF USE_OBJECT_ARC}
  GInstanceCount: UInt32 = 0;
  {$ENDIF}
  GStackCriticalSection: TIdCriticalSection = nil;

//for IPv4 Multicast address chacking
const
  IPv4MCastLo = 224;
  IPv4MCastHi = 239;

procedure SetStackClass(AStackClass: TIdStackClass);
begin
  GStackClass := AStackClass;
end;

procedure TIdPacketInfo.Reset;
begin
  FSourceIP := '';
  FSourcePort := 0;
  FSourceIF := 0;
  FSourceIPVersion := ID_DEFAULT_IP_VERSION;
  FDestIP := '';
  FDestPort:= 0;
  FDestIF := 0;
  FDestIPVersion := ID_DEFAULT_IP_VERSION;
  FTTL := 0;
end;

{ TIdSocketList }

constructor TIdSocketList.Create;
begin
  inherited Create;
  FLock := TIdCriticalSection.Create;
end;

class function TIdSocketList.CreateSocketList: TIdSocketList;
Begin
  Result := GSocketListClass.Create;
End;

destructor TIdSocketList.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TIdSocketList.Lock;
begin
  FLock.Acquire;
end;

class function TIdSocketList.Select(AReadList, AWriteList,
  AExceptList: TIdSocketList; const ATimeout: Integer): Boolean;
begin
  // C++ Builder cannot have abstract class functions thus we need this base
  Result := False;
end;

procedure TIdSocketList.Unlock;
begin
  FLock.Release;
end;

{ EIdSocketError }

constructor EIdSocketError.CreateError(const AErr: Integer; const AMsg: string);
begin
  inherited Create(AMsg);
  FLastError := AErr;
end;

{ TIdStackLocalAddressList }

constructor TIdStackLocalAddress.Create(ACollection: TCollection; const AIPVersion: TIdIPVersion; const AIPAddress: string);
begin
  inherited Create(ACollection);
  FIPVersion := AIPVersion;
  FIPAddress := AIPAddress;
end;

constructor TIdStackLocalAddressIPv4.Create(ACollection: TCollection; const AIPAddress, ASubNetMask: string);
begin
  inherited Create(ACollection, Id_IPv4, AIPAddress);
  FSubNetMask := ASubNetMask;
end;

constructor TIdStackLocalAddressIPv6.Create(ACollection: TCollection; const AIPAddress: string);
begin
  inherited Create(ACollection, Id_IPv6, AIPAddress);
end;

constructor TIdStackLocalAddressList.Create;
begin
  inherited Create(TIdStackLocalAddress);
end;

function TIdStackLocalAddressList.GetAddress(AIndex: Integer): TIdStackLocalAddress;
begin
  Result := TIdStackLocalAddress(inherited Items[AIndex]);
end;

function TIdStackLocalAddressList.IndexOfIP(const AIP: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count-1 do begin
    if Addresses[I].IPAddress = AIP then begin
      Result := I;
      Exit;
    end;
  end;
end;

function TIdStackLocalAddressList.IndexOfIP(const AIP: String; AIPVersion: TIdIPVersion): Integer;
var
  I: Integer;
  LAddr: TIdStackLocalAddress;
begin
  Result := -1;
  for I := 0 to Count-1 do begin
    LAddr := Addresses[I];
    if (LAddr.IPVersion = AIPVersion) and (LAddr.IPAddress = AIP) then begin
      Result := I;
      Exit;
    end;
  end;
end;

{ TIdStack }

constructor TIdStack.Create;
begin
  // Here for .net
  inherited Create;
end;

destructor TIdStack.Destroy;
begin
  FreeAndNil(FLocalAddresses);
  inherited Destroy;
end;

procedure TIdStack.IPVersionUnsupported;
begin
  raise EIdIPVersionUnsupported.Create(RSIPVersionUnsupported);
end;

function TIdStack.Accept(ASocket: TIdStackSocketHandle; var VIP: string;
  var VPort: TIdPort): TIdStackSocketHandle;
var
  LIPVersion: TIdIPVersion;
begin
  Result := Accept(ASocket, VIP, VPort, LIPVersion);
end;

procedure TIdStack.GetPeerName(ASocket: TIdStackSocketHandle; var VIP: string;
  var VPort: TIdPort);
var
  LIPVersion: TIdIPVersion;
begin
  GetPeerName(ASocket, VIP, VPort, LIPVersion);
end;

procedure TIdStack.GetSocketName(ASocket: TIdStackSocketHandle; var VIP: string;
  var VPort: TIdPort);
var
  LIPVersion: TIdIPVersion;
begin
  GetSocketName(ASocket, VIP, VPort, LIPVersion);
end;

{$I IdDeprecatedImplBugOff.inc}
procedure TIdStack.AddLocalAddressesToList(AAddresses: TStrings);
{$I IdDeprecatedImplBugOn.inc}
var
  LList: TIdStackLocalAddressList;
  I: Integer;
begin
  LList := TIdStackLocalAddressList.Create;
  try
    // for backwards compatibility, return only IPv4 addresses
    GetLocalAddressList(LList);
    if LList.Count > 0 then begin
      AAddresses.BeginUpdate;
      try
        for I := 0 to LList.Count-1 do begin
          if LList[I].IPVersion = Id_IPv4 then begin
            AAddresses.Add(LList[I].IPAddress);
          end;
        end;
      finally
        AAddresses.EndUpdate;
      end;
    end;
  finally
    LList.Free;
  end;
end;

function TIdStack.GetLocalAddresses: TStrings;
var
  LList: TIdStackLocalAddressList;
  I: Integer;
begin
  if FLocalAddresses = nil then begin
    FLocalAddresses := TStringList.Create;
  end;
  FLocalAddresses.BeginUpdate;
  try
    FLocalAddresses.Clear;
    LList := TIdStackLocalAddressList.Create;
    try
      // for backwards compatibility, return only IPv4 addresses
      GetLocalAddressList(LList);
      for I := 0 to LList.Count-1 do begin
        if LList[I].IPVersion = Id_IPv4 then begin
          FLocalAddresses.Add(LList[I].IPAddress);
        end;
      end;
    finally
      LList.Free;
    end;
  finally
    FLocalAddresses.EndUpdate;
  end;
  Result := FLocalAddresses;
end;

function TIdStack.GetLocalAddress: string;
var
  LList: TIdStackLocalAddressList;
  I: Integer;
begin
  // RLebeau: using a local list instead of the LocalAddresses
  // property so this method can be thread-safe...
  //
  // old code:
  // Result := LocalAddresses[0];

  Result := '';
  LList := TIdStackLocalAddressList.Create;
  try
    // for backwards compatibility, return only IPv4 addresses
    GetLocalAddressList(LList);
    for I := 0 to LList.Count-1 do begin
      if LList[I].IPVersion = Id_IPv4 then begin
        Result := LList[I].IPAddress;
        Exit;
      end;
    end;
  finally
    LList.Free;
  end;
end;

function TIdStack.IsIP(AIP: string): Boolean;
var
  i: Integer;
begin
  // TODO: support IPv6

//
//Result := Result and ((i > 0) and (i < 256));
//
  i := IndyStrToInt(Fetch(AIP, '.'), -1);    {Do not Localize}
  Result := (i > -1) and (i < 256);
  i := IndyStrToInt(Fetch(AIP, '.'), -1);    {Do not Localize}
  Result := Result and ((i > -1) and (i < 256));
  i := IndyStrToInt(Fetch(AIP, '.'), -1);    {Do not Localize}
  Result := Result and ((i > -1) and (i < 256));
  i := IndyStrToInt(Fetch(AIP, '.'), -1);    {Do not Localize}
  Result := Result and ((i > -1) and (i < 256)) and (AIP = '');
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdStack.MakeCanonicalIPv6Address(const AAddr: string): string;
{$I IdDeprecatedImplBugOn.inc}
begin
  Result := IdGlobal.MakeCanonicalIPv6Address(AAddr);
end;

function TIdStack.ResolveHost(const AHost: string;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): string;
begin
  Result := '';
  case AIPVersion of
    Id_IPv4: begin
        // Sometimes 95 forgets who localhost is
        if TextIsSame(AHost, 'LOCALHOST') then begin    {Do not Localize}
          Result := '127.0.0.1';    {Do not Localize}
        end else if IsIP(AHost) then begin
          Result := AHost;
        end else begin
          Result := HostByName(AHost, Id_IPv4);
        end;
      end;
    Id_IPv6: begin
        if TextIsSame(AHost, 'LOCALHOST') then begin    {Do not Localize}
          Result := '::1';    {Do not Localize}
        end else begin
          Result := IdGlobal.MakeCanonicalIPv6Address(AHost);
          if Result = '' then begin
            Result := HostByName(AHost, Id_IPv6);
          end;
        end;
      end;
    else begin
      IPVersionUnsupported;
    end;
  end;
end;

function TIdStack.SendTo(ASocket: TIdStackSocketHandle; const ABuffer: TIdBytes;
  const AOffset: Integer; const AIP: string; const APort: TIdPort;
  const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION): Integer;
begin
  Result := SendTo(ASocket, ABuffer, AOffset, -1, AIP, APort, AIPVersion);
end;

class procedure TIdStack.DecUsage;
var
  // under ARC, increment the lock's reference count before working with it
  LLock: TIdCriticalSection;
begin
  LLock := GStackCriticalSection;
  if not Assigned(LLock) then begin
    raise EIdStackError.Create('GStackCriticalSection is nil in TIdStack.DecUsage'); {do not localize}
  end;
  LLock.Acquire;
  try
    // This CS will guarantee that during the FreeAndNil nobody
    // will try to use or construct GStack
    {$IFDEF USE_OBJECT_ARC}
    if GStack <> nil then begin
      if GStack.__ObjRelease = 0 then begin
        Pointer(GStack) := nil;
      end;
    end;
    {$ELSE}
    if GInstanceCount > 0 then begin
      Dec(GInstanceCount);
      if GInstanceCount = 0 then begin
        FreeAndNil(GStack);
      end;
    end;
    {$ENDIF}
  finally
    LLock.Release;
  end;
end;

class procedure TIdStack.IncUsage;
var
  // under ARC, increment the lock's reference count before working with it
  LLock: TIdCriticalSection;
begin
  LLock := GStackCriticalSection;
  if not Assigned(LLock) then begin
    raise EIdStackError.Create('GStackCriticalSection is nil in TIdStack.IncUsage'); {do not localize}
  end;
  LLock.Acquire;
  try
    {$IFDEF USE_OBJECT_ARC}
    if GStack = nil then begin
      if GStackClass = nil then begin
        raise EIdStackError.Create(RSStackClassUndefined);
      end;
      GStack := GStackClass.Create;
    end else begin
      GStack.__ObjAddRef;
    end;
    {$ELSE}
    if GInstanceCount = 0 then begin
      if GStack <> nil then begin
        raise EIdStackError.Create(RSStackAlreadyCreated);
      end;
      if GStackClass = nil then begin
        raise EIdStackError.Create(RSStackClassUndefined);
      end;
      GStack := GStackClass.Create;
    end;
    Inc(GInstanceCount);
    {$ENDIF}
  finally
    LLock.Release;
  end;
end;

function TIdStack.CheckForSocketError(const AResult: Integer): Integer;
begin
  if AResult = Integer(Id_SOCKET_ERROR) then begin
    RaiseLastSocketError;
  end;
  Result := AResult;
end;

function TIdStack.CheckForSocketError(const AResult: Integer;
  const AIgnore: array of integer): Integer;
var
  i: Integer;
  LLastError: Integer;
begin
  Result := AResult;
  if AResult = Integer(Id_SOCKET_ERROR) then begin
    LLastError := WSGetLastError;
    for i := Low(AIgnore) to High(AIgnore) do begin
      if LLastError = AIgnore[i] then begin
        Result := LLastError;
        Exit;
      end;
    end;
    RaiseSocketError(LLastError);
  end;
end;

procedure TIdStack.RaiseLastSocketError;
begin
  RaiseSocketError(WSGetLastError);
end;

// TODO: move this to IdStackVCLPosix...
{$IFDEF USE_VCL_POSIX}
  {$IFDEF ANDROID}
function GetActivityContext: JContext; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF HAS_TAndroidHelper}
  Result := TAndroidHelper.Context;
  {$ELSE}
  Result := SharedActivityContext;
  {$ENDIF}
end;

function HasAndroidPermission(const Permission: string): Boolean;
begin
  Result := GetActivityContext.checkCallingOrSelfPermission(StringToJString(Permission)) = TJPackageManager.JavaClass.PERMISSION_GRANTED;
end;
  {$ENDIF}
{$ENDIF}

procedure TIdStack.RaiseSocketError(AErr: integer);
begin
  (*
    RRRRR    EEEEEE   AAAA   DDDDD         MM     MM  EEEEEE    !!  !!  !!
    RR  RR   EE      AA  AA  DD  DD        MMMM MMMM  EE        !!  !!  !!
    RRRRR    EEEE    AAAAAA  DD   DD       MM MMM MM  EEEE      !!  !!  !!
    RR  RR   EE      AA  AA  DD  DD        MM     MM  EE
    RR   RR  EEEEEE  AA  AA  DDDDD         MM     MM  EEEEEE    ..  ..  ..

    Please read the note in the next comment.
  *)
  if AErr = Id_WSAENOTSOCK then begin
    // You can add this to your exception ignore list for easier debugging.
    // However please note that sometimes it is a true error. Your program
    // will still run correctly, but the debugger will not stop on it if you
    // list it in the ignore list. But for most times its fine to put it in
    // the ignore list, it only affects your debugging.
    raise EIdNotASocket.CreateError(AErr, WSTranslateSocketErrorMsg(AErr));
  end;

  // TODO: move this to IdStackVCLPosix...
  {$IFDEF USE_VCL_POSIX}
    {$IFDEF ANDROID}
  if (AErr = 9{EBADF}) or (AErr = 12{EBADR?}) or (AErr = 13{EACCES}) then begin
    if not HasAndroidPermission('android.permission.INTERNET') then begin {Do not Localize}
      raise EIdInternetPermissionNeeded.CreateError(AErr, WSTranslateSocketErrorMsg(AErr));
    end;
  end;
    {$ENDIF}
  {$ENDIF}

  (*
    It is normal to receive a 10038 exception (10038, NOT others!) here when
    *shutting down* (NOT at other times!) servers (NOT clients!).

    If you receive a 10038 exception here please see the FAQ at:
    http://www.IndyProject.org/

    If you insist upon requesting help via our email boxes on the 10038 error
    that is already answered in the FAQ and you are simply too slothful to
    search for your answer and ask your question in the public forums you may be
    publicly flogged, tarred and feathered and your name may be added to every
    chain letter / EMail in existence today."

    Otherwise, if you DID read the FAQ and have further questions, please feel
    free to ask using one of the methods (Carefullly note that these methods do
    not list email) listed on the Tech Support link at:
    http://www.IndyProject.org/

    RRRRR    EEEEEE   AAAA   DDDDD         MM     MM  EEEEEE    !!  !!  !!
    RR  RR   EE      AA  AA  DD  DD        MMMM MMMM  EE        !!  !!  !!
    RRRRR    EEEE    AAAAAA  DD   DD       MM MMM MM  EEEE      !!  !!  !!
    RR  RR   EE      AA  AA  DD  DD        MM     MM  EE
    RR   RR  EEEEEE  AA  AA  DDDDD         MM     MM  EEEEEE    ..  ..  ..
  *)
  raise EIdSocketError.CreateError(AErr, WSTranslateSocketErrorMsg(AErr));
end;

function TIdStack.WSTranslateSocketErrorMsg(const AErr: integer): string;
begin
  Result := '';    {Do not Localize}
  case AErr of
    Id_WSAEINTR: Result           := RSStackEINTR;
    Id_WSAEBADF: Result           := RSStackEBADF;
    Id_WSAEACCES: Result          := RSStackEACCES;
    Id_WSAEFAULT: Result          := RSStackEFAULT;
    Id_WSAEINVAL: Result          := RSStackEINVAL;
    Id_WSAEMFILE: Result          := RSStackEMFILE;
    Id_WSAEWOULDBLOCK: Result     := RSStackEWOULDBLOCK;
    Id_WSAEINPROGRESS: Result     := RSStackEINPROGRESS;
    Id_WSAEALREADY: Result        := RSStackEALREADY;
    Id_WSAENOTSOCK: Result        := RSStackENOTSOCK;
    Id_WSAEDESTADDRREQ: Result    := RSStackEDESTADDRREQ;
    Id_WSAEMSGSIZE: Result        := RSStackEMSGSIZE;
    Id_WSAEPROTOTYPE: Result      := RSStackEPROTOTYPE;
    Id_WSAENOPROTOOPT: Result     := RSStackENOPROTOOPT;

    Id_WSAEPROTONOSUPPORT: Result := RSStackEPROTONOSUPPORT;
    {$IFNDEF BEOS}
    Id_WSAESOCKTNOSUPPORT: Result := RSStackESOCKTNOSUPPORT;
    {$ENDIF}
    Id_WSAEOPNOTSUPP: Result      := RSStackEOPNOTSUPP;
    Id_WSAEPFNOSUPPORT: Result    := RSStackEPFNOSUPPORT;
    Id_WSAEAFNOSUPPORT: Result    := RSStackEAFNOSUPPORT;
    Id_WSAEADDRINUSE: Result      := RSStackEADDRINUSE;
    Id_WSAEADDRNOTAVAIL: Result   := RSStackEADDRNOTAVAIL;
    Id_WSAENETDOWN: Result        := RSStackENETDOWN;
    Id_WSAENETUNREACH: Result     := RSStackENETUNREACH;
    Id_WSAENETRESET: Result       := RSStackENETRESET;
    Id_WSAECONNABORTED: Result    := RSStackECONNABORTED;
    Id_WSAECONNRESET: Result      := RSStackECONNRESET;
    Id_WSAENOBUFS: Result         := RSStackENOBUFS;
    Id_WSAEISCONN: Result         := RSStackEISCONN;
    Id_WSAENOTCONN: Result        := RSStackENOTCONN;
    Id_WSAESHUTDOWN: Result       := RSStackESHUTDOWN;
    {$IFNDEF BEOS}
    Id_WSAETOOMANYREFS: Result    := RSStackETOOMANYREFS;
    {$ENDIF}
    Id_WSAETIMEDOUT: Result       := RSStackETIMEDOUT;
    Id_WSAECONNREFUSED: Result    := RSStackECONNREFUSED;
    Id_WSAELOOP: Result           := RSStackELOOP;
    Id_WSAENAMETOOLONG: Result    := RSStackENAMETOOLONG;
    Id_WSAEHOSTDOWN: Result       := RSStackEHOSTDOWN;
    Id_WSAEHOSTUNREACH: Result    := RSStackEHOSTUNREACH;
    Id_WSAENOTEMPTY: Result       := RSStackENOTEMPTY;
  end;
  Result := IndyFormat(RSStackError, [AErr, Result]);
end;

function TIdStack.HostToNetwork(const AValue: TIdIPv6Address): TIdIPv6Address;
var
  i : Integer;
begin
  for i := 0 to 7 do begin
    Result[i] := HostToNetwork(AValue[i]);
  end;
end;

function TIdStack.NetworkToHost(const AValue: TIdIPv6Address): TIdIPv6Address;
var
  i : Integer;
begin
  for i := 0 to 7 do begin
    Result[i] := NetworkToHost(AValue[i]);
  end;
end;

function TIdStack.IsValidIPv4MulticastGroup(const Value: string): Boolean;
var
  LIP: string;
  LVal: Integer;
begin
  Result := False;
  if IsIP(Value) then
  begin
    LIP := Value;
    LVal := IndyStrToInt(Fetch(LIP, '.'));    {Do not Localize}
    Result := (LVal >= IPv4MCastLo) and (LVal <= IPv4MCastHi);
  end;
end;

{ From "rfc 2373"

2.7 Multicast Addresses

   An IPv6 multicast address is an identifier for a group of nodes.  A
   node may belong to any number of multicast groups.  Multicast
   addresses have the following format:

#
   |   8    |  4 |  4 |                  112 bits                   |
   +------ -+----+----+---------------------------------------------+
   |11111111|flgs|scop|                  group ID                   |
   +--------+----+----+---------------------------------------------+

      11111111 at the start of the address identifies the address as
      being a multicast address.

                                    +-+-+-+-+
      flgs is a set of 4 flags:     |0|0|0|T|
                                    +-+-+-+-+

         The high-order 3 flags are reserved, and must be initialized to
         0.

         T = 0 indicates a permanently-assigned ("well-known") multicast
         address, assigned by the global internet numbering authority.

         T = 1 indicates a non-permanently-assigned ("transient")
         multicast address.

      scop is a 4-bit multicast scope value used to limit the scope of
      the multicast group.  The values are:

         0  reserved
         1  node-local scope
         2  link-local scope
         3  (unassigned)
         4  (unassigned)
         5  site-local scope
         6  (unassigned)
         7  (unassigned)
         8  organization-local scope
         9  (unassigned)
         A  (unassigned)
         B  (unassigned)
         C  (unassigned)

         D  (unassigned)
         E  global scope
         F  reserved

      group ID identifies the multicast group, either permanent or
      transient, within the given scope.

   The "meaning" of a permanently-assigned multicast address is
   independent of the scope value.  For example, if the "NTP servers
   group" is assigned a permanent multicast address with a group ID of
   101 (hex), then:

      FF01:0:0:0:0:0:0:101 means all NTP servers on the same node as the
      sender.

      FF02:0:0:0:0:0:0:101 means all NTP servers on the same link as the
      sender.

      FF05:0:0:0:0:0:0:101 means all NTP servers at the same site as the
      sender.

      FF0E:0:0:0:0:0:0:101 means all NTP servers in the internet.

   Non-permanently-assigned multicast addresses are meaningful only
   within a given scope.  For example, a group identified by the non-
   permanent, site-local multicast address FF15:0:0:0:0:0:0:101 at one
   site bears no relationship to a group using the same address at a
   different site, nor to a non-permanent group using the same group ID
   with different scope, nor to a permanent group with the same group
   ID.

   Multicast addresses must not be used as source addresses in IPv6
   packets or appear in any routing header.
}
function TIdStack.IsValidIPv6MulticastGroup(const Value: string): Boolean;
var
  LTmp : String;
begin
  LTmp := IdGlobal.MakeCanonicalIPv6Address(Value);
  if LTmp <> '' then
  begin
    Result := TextStartsWith(LTmp, 'FF');
  end else begin
    Result := False;
  end;
end;

function TIdStack.CalcCheckSum(const AData: TIdBytes): UInt16;
var
  i : Integer;
  LSize : Integer;
  LCRC : UInt32;
begin
  LCRC := 0;
  i := 0;
  LSize := Length(AData);
  while LSize > 1 do
  begin
    LCRC := LCRC + BytesToUInt16(AData, i);
    Dec(LSize, 2);
    Inc(i, 2);
  end;
  if LSize > 0 then begin
    LCRC := LCRC + AData[i];
  end;
  LCRC := (LCRC shr 16) + (LCRC and $ffff);  //(LCRC >> 16)
  LCRC := LCRC + (LCRC shr 16);
  Result := not UInt16(LCRC);
end;

{$UNDEF HAS_TCP_KEEPIDLE_OR_KEEPINTVL}
{$IFDEF HAS_TCP_KEEPIDLE}
  {$DEFINE HAS_TCP_KEEPIDLE_OR_KEEPINTVL}
{$ENDIF}
{$IFDEF HAS_TCP_KEEPINTVL}
  {$DEFINE HAS_TCP_KEEPIDLE_OR_KEEPINTVL}
{$ENDIF}

procedure TIdStack.SetKeepAliveValues(ASocket: TIdStackSocketHandle;
  const AEnabled: Boolean; const ATimeMS, AInterval: Integer);
begin
  SetSocketOption(ASocket, Id_SOL_SOCKET, Id_SO_KEEPALIVE, iif(AEnabled, 1, 0));
  {$IFDEF HAS_TCP_KEEPIDLE_OR_KEEPINTVL}
  if AEnabled then
  begin
    // TODO: support TCP_KEEPCNT
    {$IFDEF HAS_TCP_KEEPIDLE}
    SetSocketOption(ASocket, Id_SOL_TCP, Id_TCP_KEEPIDLE, ATimeMS);
    {$ENDIF}
    {$IFDEF HAS_TCP_KEEPINTVL}
    SetSocketOption(ASocket, Id_SOL_TCP, Id_TCP_KEEPINTVL, AInterval);
    {$ENDIF}
  end;
  {$ENDIF}
end;

initialization
  //done this way so we can have a separate stack just for FPC under Unix systems
  GStackClass :=
    {$IFDEF DOTNET}
    TIdStackDotNet
    {$ELSE}
      {$IFDEF WINDOWS}
    TIdStackWindows
      {$ELSE}
        {$IFDEF USE_VCL_POSIX}
    TIdStackVCLPosix
        {$ELSE}
          {$IFDEF UNIX}
            {$IFDEF KYLIXCOMPAT}
        TIdStackLibc
            {$ELSE}
              {$IFDEF USE_BASEUNIX}
        TIdStackUnix
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  ;
  GStackCriticalSection := TIdCriticalSection.Create;
  {$IFNDEF DOTNET}
    {$IFDEF REGISTER_EXPECTED_MEMORY_LEAK}
  IndyRegisterExpectedMemoryLeak(GStackCriticalSection);
    {$ENDIF}
  {$ENDIF}
finalization
  // Dont Free. If shutdown is from another Init section, it can cause GPF when stack
  // tries to access it. App will kill it off anyways, so just let it leak
  {$IFDEF FREE_ON_FINAL}
  FreeAndNil(GStackCriticalSection);
  {$ENDIF}
end.

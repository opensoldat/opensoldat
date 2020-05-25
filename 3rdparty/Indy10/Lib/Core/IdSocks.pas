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


   Rev 1.38    11/15/2004 11:59:12 PM  JPMugaas
 Hopefully, this should handle IPv6 addresses in SOCKS bind and listen.


   Rev 1.37    11/12/2004 11:30:18 AM  JPMugaas
 Expansions for IPv6.


   Rev 1.36    11/11/2004 10:25:24 PM  JPMugaas
 Added OpenProxy and CloseProxy so you can do RecvFrom and SendTo functions
 from the UDP client with SOCKS.  You must call OpenProxy  before using
 RecvFrom or SendTo.  When you are finished, you must use CloseProxy to close
 any connection to the Proxy.  Connect and disconnect also call OpenProxy and
 CloseProxy.


   Rev 1.35    11/11/2004 3:42:50 AM  JPMugaas
 Moved strings into RS.  Socks will now raise an exception if you attempt to
 use SOCKS4 and SOCKS4A with UDP.  Those protocol versions do not support UDP
 at all.


   Rev 1.34    11/10/2004 10:55:58 PM  JPMugaas
 UDP Association bug fix - we now send 0's for IP address and port.


   Rev 1.33    11/10/2004 10:38:42 PM  JPMugaas
 Bug fixes - UDP with SOCKS now works.


   Rev 1.32    11/10/2004 9:42:54 PM  JPMugaas
 1 in a reserved position should be 0 in a UDP request packet.


   Rev 1.31    11/9/2004 8:18:00 PM  JPMugaas
 Attempt to add SOCKS support in UDP.


   Rev 1.30    03/07/2004 10:08:22  CCostelloe
 Removed spurious code that generates warning


   Rev 1.29    6/9/04 7:44:44 PM  RLebeau
 various ReadBytes() tweaks
 
 updated MakeSocks4Request() to call AIOHandler.WriteBufferCancel() on error.


   Rev 1.28    2004.05.20 1:39:58 PM  czhower
 Last of the IdStream updates


    Rev 1.27    2004.05.20 9:19:24 AM  czhower
  Removed unused var


    Rev 1.26    5/19/2004 10:44:42 PM  DSiders
  Corrected spelling for TIdIPAddress.MakeAddressObject method.


   Rev 1.25    5/19/2004 2:44:40 PM  JPMugaas
 Fixed compiler warnings in TIdSocksInfo.Listen.


   Rev 1.24    5/8/2004 3:45:34 PM  BGooijen
 Listen works in Socks 4 now


   Rev 1.23    5/7/2004 4:52:44 PM  JPMugaas
 Bind in SOCKS4 should work a bit better.  There's still some other work that
 needs to be done on it.


   Rev 1.22    5/7/2004 8:54:54 AM  JPMugaas
 Attempt to add SOCKS4 bind.


   Rev 1.21    5/7/2004 7:43:24 AM  JPMugaas
 Checked Bas's changes.


   Rev 1.20    5/7/2004 5:53:20 AM  JPMugaas
 Removed some duplicate code to reduce the probability of error.


   Rev 1.19    5/7/2004 1:44:12 AM  BGooijen
 Bind


   Rev 1.18    5/6/2004 6:47:04 PM  JPMugaas
 Attempt to work on bind further.


   Rev 1.16    5/6/2004 5:32:58 PM  JPMugaas
 Port was being mangled because the compiler was assuming you wanted a 4 byte
 byte order instead of only a two byte byte order function.
 IP addresses are better handled.  At least I can connect again.


   Rev 1.15    5/5/2004 2:09:40 PM  JPMugaas
 Attempt to reintroduce bind and listen functionality for FTP.


   Rev 1.14    2004.03.07 11:48:44 AM  czhower
 Flushbuffer fix + other minor ones found


   Rev 1.13    2004.02.03 4:16:52 PM  czhower
 For unit name changes.


   Rev 1.12    2/2/2004 2:33:04 PM  JPMugaas
 Should compile better.


   Rev 1.11    2/2/2004 12:23:16 PM  JPMugaas
 Attempt to fix the last Todo concerning IPv6.


   Rev 1.10    2/2/2004 11:43:08 AM  BGooijen
 DotNet


   Rev 1.9    2/2/2004 12:00:08 AM  BGooijen
 Socks 4 / 4A working again


   Rev 1.8    2004.01.20 10:03:34 PM  czhower
 InitComponent


   Rev 1.7    1/11/2004 10:45:56 PM  BGooijen
 Socks 5 works on D7 now, Socks 4 almost


   Rev 1.6    2003.10.11 5:50:34 PM  czhower
 -VCL fixes for servers
 -Chain suport for servers (Super core)
 -Scheduler upgrades
 -Full yarn support


   Rev 1.5    2003.10.01 1:37:34 AM  czhower
 .Net


   Rev 1.4    2003.09.30 7:37:28 PM  czhower
 Updates for .net


   Rev 1.3    4/2/2003 3:23:00 PM  BGooijen
 fixed and re-enabled


   Rev 1.2    2003.01.10 8:21:04 PM  czhower
 Removed more warnings


   Rev 1.1    2003.01.10 7:21:14 PM  czhower
 Removed warnings


   Rev 1.0    11/13/2002 08:58:56 AM  JPMugaas
}
unit IdSocks;

interface

{$I IdCompilerDefines.inc}
//we need to put this in Delphi mode to work.

uses
  Classes,
  IdAssignedNumbers, IdException, IdBaseComponent,
  IdComponent, IdCustomTransparentProxy, IdGlobal, IdIOHandler,
  IdIOHandlerSocket, IdSocketHandle;

type
  EIdSocksUDPNotSupportedBySOCKSVersion = class(EIdException);
  TSocksVersion = (svNoSocks, svSocks4, svSocks4A, svSocks5);
  TSocksAuthentication = (saNoAuthentication, saUsernamePassword);

const
  ID_SOCKS_AUTH = saNoAuthentication;
  ID_SOCKS_VER = svNoSocks;

type
  TIdSocksInfo = class(TIdCustomTransparentProxy)
  protected
    FAuthentication: TSocksAuthentication;
    FVersion: TSocksVersion;
    FUDPSocksAssociation : TIdIOHandlerSocket;
   
    //
    function DisasmUDPReplyPacket(const APacket : TIdBytes;
      var VHost : String; var VPort : TIdPort; var VIPVersion: TIdIPVersion): TIdBytes;
    function MakeUDPRequestPacket(const AData: TIdBytes;
      const AHost: String; const APort: TIdPort) : TIdBytes;
    function GetEnabled: Boolean; override;
    procedure InitComponent; override;
    procedure AuthenticateSocks5Connection(AIOHandler: TIdIOHandler);
    // This must be defined with an port value that's a word so that we use the 2 byte Network Order byte functions instead
    // the 4 byte or 8 byte functions.  If we use the wrong byte order functions, we can get a zero port value causing an error.
    procedure MakeSocks4Request(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const ARequest : Byte);
    procedure MakeSocks5Request(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const ARequest : Byte; var VBuf : TIdBytes; var VLen : Integer);
    procedure MakeSocks4Connection(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort);
    procedure MakeSocks4Bind(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort);
    procedure MakeSocks5Connection(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
    procedure MakeSocks5Bind(AIOHandler: TIdIOHandler; const AHost: string;
      const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
    procedure MakeConnection(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function  MakeSocks4Listen(AIOHandler: TIdIOHandler; const ATimeOut:integer):boolean;
    function  MakeSocks5Listen(AIOHandler: TIdIOHandler; const ATimeOut:integer):boolean;

    //association for UDP
    procedure MakeSocks5UDPAssociation(AHandle : TIdSocketHandle);
    procedure CloseSocks5UDPAssociation;
  public
    procedure Assign(ASource: TPersistent); override;
    destructor Destroy; override;
    procedure Bind(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function  Listen(AIOHandler: TIdIOHandler; const ATimeOut:integer):boolean;override;
    procedure OpenUDP(AHandle : TIdSocketHandle; const AHost: string = ''; const APort: TIdPort = 0; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION); override;
    function RecvFromUDP(AHandle: TIdSocketHandle; var ABuffer : TIdBytes;
      var VPeerIP: string; var VPeerPort: TIdPort; var VIPVersion: TIdIPVersion;
      AMSec: Integer = IdTimeoutDefault): Integer; override;
    procedure SendToUDP(AHandle: TIdSocketHandle; const AHost: string;
      const APort: TIdPort; const AIPVersion: TIdIPVersion; const ABuffer : TIdBytes); override;
    procedure CloseUDP(AHandle: TIdSocketHandle); override;
  published
    property Authentication: TSocksAuthentication read FAuthentication write FAuthentication default ID_SOCKS_AUTH;
    property Host;
    property Password;
    property Port default IdPORT_SOCKS;
    property IPVersion;
    property Username;
    property Version: TSocksVersion read FVersion write FVersion default ID_SOCKS_VER;
    property  ChainedProxy;
  End;//TIdSocksInfo

implementation

uses
   IdResourceStringsCore, IdExceptionCore, IdIPAddress, IdStack,
  IdTCPClient,
  IdIOHandlerStack, SysUtils;

{ TIdSocksInfo }

procedure TIdSocksInfo.Assign(ASource: TPersistent);
var
  LSource: TIdSocksInfo;
begin
  if ASource is TIdSocksInfo then begin
    LSource := TIdSocksInfo(ASource);
    FAuthentication := LSource.Authentication;
    FVersion := LSource.Version;
  end;
  // always allow TIdCustomTransparentProxy to assign its properties as well
  inherited Assign(ASource);
end;

procedure TIdSocksInfo.MakeSocks4Request(AIOHandler: TIdIOHandler; const AHost: string;
  const APort: TIdPort; const ARequest : Byte);
var
  LIpAddr: String;
  LBufferingStarted: Boolean;
begin
  LBufferingStarted := not AIOHandler.WriteBufferingActive;
  if LBufferingStarted then begin
    AIOHandler.WriteBufferOpen;
  end;
  try
    AIOHandler.Write(Byte(4)); // Version
    AIOHandler.Write(ARequest); // Opcode
    AIOHandler.Write(Word(APort)); // Port

    if Version = svSocks4A then begin
      LIpAddr := '0.0.0.1';    {Do not Localize}
    end else begin
      LIpAddr := GStack.ResolveHost(AHost,Id_IPv4);
    end;

    AIOHandler.Write(Byte(IndyStrToInt(Fetch(LIpAddr,'.'))));// IP
    AIOHandler.Write(Byte(IndyStrToInt(Fetch(LIpAddr,'.'))));// IP
    AIOHandler.Write(Byte(IndyStrToInt(Fetch(LIpAddr,'.'))));// IP
    AIOHandler.Write(Byte(IndyStrToInt(Fetch(LIpAddr,'.'))));// IP

    AIOHandler.Write(Username);
    AIOHandler.Write(Byte(0));// Username

    if Version = svSocks4A then begin
      AIOHandler.Write(AHost);
      AIOHandler.Write(Byte(0));// Host
    end;

    if LBufferingStarted then begin
      AIOHandler.WriteBufferClose; //flush everything
    end;
  except
    if LBufferingStarted then begin
      AIOHandler.WriteBufferCancel; //cancel everything
    end;
    raise;
  end;
end;

procedure TIdSocksInfo.MakeSocks4Connection(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort);
var
  LResponse: TIdBytes;
begin
  MakeSocks4Request(AIOHandler, AHost, APort,$01); //connect
  AIOHandler.ReadBytes(LResponse, 8, False);
  case LResponse[1] of // OpCode
    90: ;// request granted, do nothing
    91: raise EIdSocksRequestFailed.Create(RSSocksRequestFailed);
    92: raise EIdSocksRequestServerFailed.Create(RSSocksRequestServerFailed);
    93: raise EIdSocksRequestIdentFailed.Create(RSSocksRequestIdentFailed);
    else raise EIdSocksUnknownError.Create(RSSocksUnknownError);
  end;
end;

procedure TIdSocksInfo.MakeSocks5Request(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const ARequest : Byte; var VBuf : TIdBytes; var VLen : Integer);
var
  LIP : TIdIPAddress;
  LAddr: TIdBytes;
begin
  // Connection process
  VBuf[0] := $5;   // socks version
  VBuf[1] := ARequest; //request method
  VBuf[2] := $0;   // reserved

  // address type: IP V4 address: X'01'    {Do not Localize}
  //               DOMAINNAME:    X'03'    {Do not Localize}
  //               IP V6 address: X'04'    {Do not Localize}

  LIP := TIdIPAddress.MakeAddressObject(AHost);
  if Assigned(LIP) then
  begin
    try
      if LIP.AddrType = Id_IPv6 then begin
        VBuf[3] := $04;  //IPv6 address
      end else begin
        VBuf[3] := $01;  //IPv4 address
      end;
      LAddr := LIP.HToNBytes;
      CopyTIdBytes(LAddr, 0, VBuf, 4, Length(LAddr));
      VLen := 4 + Length(LAddr);
    finally
      FreeAndNil(LIP);
    end;
  end else
  begin
    LAddr := ToBytes(AHost);
    VBuf[3] := $3;   // host name
    VBuf[4] := IndyMin(Length(LAddr), 255);
    if VBuf[4] > 0 then begin
      CopyTIdBytes(LAddr, 0, VBuf, 5, VBuf[4]);
    end;
    VLen := 5 + VBuf[4];
  end;

  // port

  CopyTIdUInt16(GStack.HostToNetwork(APort), VBuf, VLen);
  VLen := VLen + 2;
end;

procedure TIdSocksInfo.MakeSocks5Connection(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  Lpos: Integer;
  LBuf: TIdBytes;
begin
  AuthenticateSocks5Connection(AIOHandler);
  SetLength(LBuf, 255);
  MakeSocks5Request(AIOHandler, AHost, APort, $01, LBuf, Lpos);

  LBuf := ToBytes(LBuf, Lpos);
  AIOHandler.WriteDirect(LBuf); // send the connection packet
  try
    AIOHandler.ReadBytes(LBuf, 5, False);    // Socks server replies on connect, this is the first part
  except
    IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
  end;

  case LBuf[1] of
    0: ;// success, do nothing
    1: raise EIdSocksServerGeneralError.Create(RSSocksServerGeneralError);
    2: raise EIdSocksServerPermissionError.Create(RSSocksServerPermissionError);
    3: raise EIdSocksServerNetUnreachableError.Create(RSSocksServerNetUnreachableError);
    4: raise EIdSocksServerHostUnreachableError.Create(RSSocksServerHostUnreachableError);
    5: raise EIdSocksServerConnectionRefusedError.Create(RSSocksServerConnectionRefusedError);
    6: raise EIdSocksServerTTLExpiredError.Create(RSSocksServerTTLExpiredError);
    7: raise EIdSocksServerCommandError.Create(RSSocksServerCommandError);
    8: raise EIdSocksServerAddressError.Create(RSSocksServerAddressError);
    else
       raise EIdSocksUnknownError.Create(RSSocksUnknownError);
  end;

  // type of destination address is domain name
  case LBuf[3] of
    // IP V4
    1: Lpos := 4 + 2; // 4 is for address and 2 is for port length
    // FQDN
    3: Lpos := LBuf[4] + 2; // 2 is for port length
    // IP V6
    4: Lpos := 16 + 2; // 16 is for address and 2 is for port length
  end;

  try
    // Socks server replies on connect, this is the second part
    // RLebeau: why -1?
    AIOHandler.ReadBytes(LBuf, Lpos-1, False);      // just write it over the first part for now
  except
    IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
  end;
end;

procedure TIdSocksInfo.MakeSocks4Bind(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort);
var
  LResponse: TIdBytes;
  LClient: TIdTcpClient;
begin
  LClient := TIdTCPClient.Create(nil);
  try
//    SetLength(LResponse, 255);
    SetLength(LResponse, 8);
    TIdIOHandlerSocket(AIOHandler).TransparentProxy := nil;
    LClient.IOHandler := AIOHandler;
    LClient.Host := Host;
    LClient.Port := Port;
    LClient.Connect;
    TIdIOHandlerSocket(AIOHandler).TransparentProxy := Self;
    MakeSocks4Request(AIOHandler, AHost, APort, $02); //bind
    AIOHandler.ReadBytes(LResponse, 2, False);
    case LResponse[1] of // OpCode
      90: ;// request granted, do nothing
      91: raise EIdSocksRequestFailed.Create(RSSocksRequestFailed);
      92: raise EIdSocksRequestServerFailed.Create(RSSocksRequestServerFailed);
      93: raise EIdSocksRequestIdentFailed.Create(RSSocksRequestIdentFailed);
    else raise EIdSocksUnknownError.Create(RSSocksUnknownError);
    end;

    try
      // Socks server replies on connect, this is the second part
      AIOHandler.ReadBytes(LResponse, 6, False); //overwrite the first part for now
      TIdIOHandlerSocket(AIOHandler).Binding.SetBinding(BytesToIPv4Str(LResponse, 2), LResponse[0]*256+LResponse[1]);
    except
      IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
    end;
  finally
    LClient.IOHandler := nil;
    FreeAndNil(LClient);
  end;
end;

procedure TIdSocksInfo.MakeConnection(AIOHandler: TIdIOHandler; const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
begin
  case Version of
    svSocks4, svSocks4A: MakeSocks4Connection(AIOHandler, AHost, APort);
    svSocks5: MakeSocks5Connection(AIOHandler, AHost, APort);
  end;
end;

function TIdSocksInfo.GetEnabled: Boolean;
Begin
  Result := Version in [svSocks4, svSocks4A, svSocks5];
End;//

procedure TIdSocksInfo.InitComponent;
begin
  inherited InitComponent;
  Authentication := ID_SOCKS_AUTH;
  Version := ID_SOCKS_VER;
  Port := IdPORT_SOCKS;
  FIPVersion := ID_DEFAULT_IP_VERSION;
  FUDPSocksAssociation := TIdIOHandlerStack.Create;
end;

procedure TIdSocksInfo.AuthenticateSocks5Connection(
  AIOHandler: TIdIOHandler);
var
  Lpos: Integer;
  LBuf,
  LUsername,
  LPassword : TIdBytes;
  LRequestedAuthMethod,
  LServerAuthMethod,
  LUsernameLen,
  LPasswordLen : Byte;
begin
  // keep the compiler happy
  LUsername := nil;
  LPassword := nil;

  SetLength(LBuf, 3);

  // defined in rfc 1928
  if Authentication = saNoAuthentication then begin
    LBuf[2] := $0   // No authentication
  end else begin
    LBuf[2] := $2;  // Username password authentication
  end;

  LRequestedAuthMethod := LBuf[2];
  LBuf[0] := $5;     // socks version
  LBuf[1] := $1;     // number of possible authentication methods
  AIOHandler.WriteDirect(LBuf);
  try
    AIOHandler.ReadBytes(LBuf, 2, False); // Socks server sends the selected authentication method
  except
    IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
  end;

  LServerAuthMethod := LBuf[1];
  if (LServerAuthMethod <> LRequestedAuthMethod) or (LServerAuthMethod = $FF) then begin
    raise EIdSocksAuthMethodError.Create(RSSocksAuthMethodError);
  end;

  // Authentication process
  if Authentication = saUsernamePassword then begin
    LUsername := ToBytes(Username);
    LPassword := ToBytes(Password);
    LUsernameLen := IndyMin(Length(LUsername), 255);
    LPasswordLen := IndyMin(Length(LPassword), 255);
    SetLength(LBuf, 3 + LUsernameLen + LPasswordLen);
    LBuf[0] := 1; // version of subnegotiation
    LBuf[1] := LUsernameLen;
    Lpos := 2;
    if LUsernameLen > 0 then begin
      CopyTIdBytes(LUsername, 0, LBuf, Lpos, LUsernameLen);
      Lpos := Lpos + LUsernameLen;
    end;
    LBuf[Lpos] := LPasswordLen;
    Lpos := Lpos + 1;
    if LPasswordLen > 0 then begin
      CopyTIdBytes(LPassword, 0, LBuf, Lpos, LPasswordLen);
    end;

    AIOHandler.WriteDirect(LBuf); // send the username and password
    try
      AIOHandler.ReadBytes(LBuf, 2, False);    // Socks server sends the authentication status
    except
      IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
    end;

    if LBuf[1] <> $0 then begin
      raise EIdSocksAuthError.Create(RSSocksAuthError);
    end;
  end;
end;

procedure TIdSocksInfo.MakeSocks5Bind(AIOHandler: TIdIOHandler; const AHost: string;
  const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
var
  Lpos: Integer;
  LBuf: TIdBytes;
  LClient: TIdTCPClient;
  LType : Byte;
  LAddress: TIdIPv6Address;
  LIPVersion: TIdIPVersion;
begin
  LClient := TIdTCPClient.Create(nil);
  try
    SetLength(LBuf, 255);
    TIdIOHandlerSocket(AIOHandler).TransparentProxy := nil;
    LClient.IOHandler := AIOHandler;
    LClient.Host := Host;
    LClient.IPVersion := IPVersion;
    LClient.Port := Port;
    LClient.Connect;
    TIdIOHandlerSocket(AIOHandler).TransparentProxy := Self;

    AuthenticateSocks5Connection(AIOHandler);
    // Bind process
    MakeSocks5Request(AIOHandler, AHost, APort, $02, LBuf, LPos); //bind request
    //
    AIOHandler.Write(LBuf, LPos); // send the connection packet
    try
      AIOHandler.ReadBytes(LBuf, 4, False);    // Socks server replies on connect, this is the first part
    except
      IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
    end;

    case LBuf[1] of
      0: ;// success, do nothing
      1: raise EIdSocksServerGeneralError.Create(RSSocksServerGeneralError);
      2: raise EIdSocksServerPermissionError.Create(RSSocksServerPermissionError);
      3: raise EIdSocksServerNetUnreachableError.Create(RSSocksServerNetUnreachableError);
      4: raise EIdSocksServerHostUnreachableError.Create(RSSocksServerHostUnreachableError);
      5: raise EIdSocksServerConnectionRefusedError.Create(RSSocksServerConnectionRefusedError);
      6: raise EIdSocksServerTTLExpiredError.Create(RSSocksServerTTLExpiredError);
      7: raise EIdSocksServerCommandError.Create(RSSocksServerCommandError);
      8: raise EIdSocksServerAddressError.Create(RSSocksServerAddressError);
      else
        raise EIdSocksUnknownError.Create(RSSocksUnknownError);
    end;
    LType := LBuf[3];
    // type of destination address is domain name
    case LType of
      // IP V4
      1: Lpos := 4 + 2; // 4 is for address and 2 is for port length
      // FQDN
      3: Lpos := LBuf[4] + 2; // 2 is for port length
      // IP V6
      4: LPos := 16 + 2; // 16 is for address and 2 is for port length
    end;
    try
      // Socks server replies on connect, this is the second part
      AIOHandler.ReadBytes(LBuf, Lpos, False); //overwrite the first part for now
      case LType of
        1 : begin
              //IPv4
              TIdIOHandlerSocket(AIOHandler).Binding.SetPeer(BytesToIPv4Str(LBuf), LBuf[4]*256+LBuf[5], Id_IPv4);
            end;
        3 : begin
              LIPVersion := TIdIOHandlerSocket(AIOHandler).IPVersion;
              TIdIOHandlerSocket(AIOHandler).Binding.SetPeer(GStack.ResolveHost(BytesToString(LBuf,0,LPos-2), LIPVersion), LBuf[4]*256+LBuf[5], LIPVersion);
            end;
        4 : begin
              BytesToIPv6(LBuf, LAddress);
              TIdIOHandlerSocket(AIOHandler).Binding.SetPeer(IPv6AddressToStr(LAddress), LBuf[16]*256+LBuf[17], Id_IPv6);
            end;
      end;
    except
      IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
    end;
  finally
    LClient.IOHandler := nil;
    FreeAndNil(LClient);
  end;
end;

procedure TIdSocksInfo.Bind(AIOHandler: TIdIOHandler; const AHost: string;
  const APort: TIdPort; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
begin
  case Version of
    svSocks4, svSocks4A: MakeSocks4Bind(AIOHandler, AHost, APort);
    svSocks5: MakeSocks5Bind(AIOHandler, AHost, APort, AIPVersion);
  end;
end;

function TIdSocksInfo.Listen(AIOHandler: TIdIOHandler;
  const ATimeOut: integer): boolean;
begin
  Result := False;
  case Version of
    svSocks4, svSocks4A: Result := MakeSocks4Listen(AIOHandler, ATimeOut);
    svSocks5: Result := MakeSocks5Listen(AIOHandler, ATimeOut);
  end;
end;

function TIdSocksInfo.MakeSocks5Listen(AIOHandler: TIdIOHandler;
  const ATimeOut: integer): boolean;
var
  Lpos: Integer;
  LBuf: TIdBytes;
  LType : Byte;
  LAddress: TIdIPv6Address;
  LIPVersion: TIdIPVersion;
begin
  SetLength(LBuf, 255);
  Result := TIdIOHandlerSocket(AIOHandler).Binding.Readable(ATimeOut);
  if Result then begin
    AIOHandler.ReadBytes(LBuf, 4, False);    // Socks server replies on connect, this is the first part

    case LBuf[1] of
      0: ;// success, do nothing
      1: raise EIdSocksServerGeneralError.Create(RSSocksServerGeneralError);
      2: raise EIdSocksServerPermissionError.Create(RSSocksServerPermissionError);
      3: raise EIdSocksServerNetUnreachableError.Create(RSSocksServerNetUnreachableError);
      4: raise EIdSocksServerHostUnreachableError.Create(RSSocksServerHostUnreachableError);
      5: raise EIdSocksServerConnectionRefusedError.Create(RSSocksServerConnectionRefusedError);
      6: raise EIdSocksServerTTLExpiredError.Create(RSSocksServerTTLExpiredError);
      7: raise EIdSocksServerCommandError.Create(RSSocksServerCommandError);
      8: raise EIdSocksServerAddressError.Create(RSSocksServerAddressError);
      else
        raise EIdSocksUnknownError.Create(RSSocksUnknownError);
    end;
    LType := LBuf[3];
    // type of destination address is domain name
    case LType of
      // IP V4
      1: Lpos := 4 + 2; // 4 is for address and 2 is for port length
      // FQDN
      3: Lpos := LBuf[4] + 2; // 2 is for port length
      // IP V6  - 4:
    else
      Lpos := 16 + 2; // 16 is for address and 2 is for port length
    end;
    // Socks server replies on connect, this is the second part
    AIOHandler.ReadBytes(LBuf, Lpos, False);      // just write it over the first part for now
    case LType of
      1 : begin
            //IPv4
            TIdIOHandlerSocket(AIOHandler).Binding.SetPeer(BytesToIPv4Str(LBuf), LBuf[4]*256+LBuf[5], Id_IPv4);
          end;
      3 : begin
            //FQN
            LIPVersion := TIdIOHandlerSocket(AIOHandler).IPVersion;
            TIdIOHandlerSocket(AIOHandler).Binding.SetPeer(GStack.ResolveHost(BytesToString(LBuf,0,LPos-2), LIPVersion), LBuf[4]*256+LBuf[5], LIPVersion);
          end;
      else begin
            //IPv6
            BytesToIPv6(LBuf, LAddress);
            TIdIOHandlerSocket(AIOHandler).Binding.SetPeer(IPv6AddressToStr(LAddress), LBuf[16]*256+LBuf[17], Id_IPv6);
           end;
    end;
  end;
end;

function TIdSocksInfo.MakeSocks4Listen(AIOHandler: TIdIOHandler;
  const ATimeOut: integer): boolean;
var
  LBuf: TIdBytes;
begin
  SetLength(LBuf, 6);
  Result := TIdIOHandlerSocket(AIOHandler).Binding.Readable(ATimeOut);
  if Result then begin
    AIOHandler.ReadBytes(LBuf, 2, False);    // Socks server replies on connect, this is the first part

    case LBuf[1] of // OpCode
      90: ;// request granted, do nothing
      91: raise EIdSocksRequestFailed.Create(RSSocksRequestFailed);
      92: raise EIdSocksRequestServerFailed.Create(RSSocksRequestServerFailed);
      93: raise EIdSocksRequestIdentFailed.Create(RSSocksRequestIdentFailed);
      else raise EIdSocksUnknownError.Create(RSSocksUnknownError);
    end;

    // Socks server replies on connect, this is the second part
    AIOHandler.ReadBytes(LBuf, 6, False);      // just write it over the first part for now
    TIdIOHandlerSocket(AIOHandler).Binding.SetPeer(BytesToIPv4Str(LBuf, 2), LBuf[0]*256+LBuf[1], Id_IPv4);
  end;
end;

procedure TIdSocksInfo.CloseSocks5UDPAssociation;
begin
  if Assigned(FUDPSocksAssociation) then begin
    FUDPSocksAssociation.Close;
  end;
end;

procedure TIdSocksInfo.MakeSocks5UDPAssociation(AHandle: TIdSocketHandle);
var
  Lpos: Integer;
  LBuf: TIdBytes;
  LIPVersion : TIdIPVersion;
begin
  LIPVersion := Self.IPVersion;
  FUDPSocksAssociation.Host := Self.Host;
  FUDPSocksAssociation.Port := Self.Port;
  FUDPSocksAssociation.IPVersion := LIPVersion;
  FUDPSocksAssociation.Open;
  try
    SetLength(LBuf, 255);
    AuthenticateSocks5Connection(FUDPSocksAssociation);
    // Associate process
    //For SOCKS5 Associate, the IP address and port is the client's IP address and port which may
    //not be known
    if LIPVersion = Id_IPv4 then begin
      MakeSocks5Request(FUDPSocksAssociation, '0.0.0.0', 0, $03, LBuf, LPos); //associate request
    end else begin
      MakeSocks5Request(FUDPSocksAssociation, '::0', 0, $03, LBuf, LPos); //associate request
    end;
    //
    FUDPSocksAssociation.Write(LBuf, LPos); // send the connection packet
    try
      FUDPSocksAssociation.ReadBytes(LBuf, 2, False);    // Socks server replies on connect, this is the first part )VER and RSP
    except
      IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
    end;

    case LBuf[1] of
      0: ;// success, do nothing
      1: raise EIdSocksServerGeneralError.Create(RSSocksServerGeneralError);
      2: raise EIdSocksServerPermissionError.Create(RSSocksServerPermissionError);
      3: raise EIdSocksServerNetUnreachableError.Create(RSSocksServerNetUnreachableError);
      4: raise EIdSocksServerHostUnreachableError.Create(RSSocksServerHostUnreachableError);
      5: raise EIdSocksServerConnectionRefusedError.Create(RSSocksServerConnectionRefusedError);
      6: raise EIdSocksServerTTLExpiredError.Create(RSSocksServerTTLExpiredError);
      7: raise EIdSocksServerCommandError.Create(RSSocksServerCommandError);
      8: raise EIdSocksServerAddressError.Create(RSSocksServerAddressError);
      else
        raise EIdSocksUnknownError.Create(RSSocksUnknownError);
    end;
    FUDPSocksAssociation.ReadBytes(LBuf, 2, False); //Now get RSVD and ATYPE feilds
    // type of destination address is domain name
    case LBuf[1] of
      // IP V4
      1:  begin
            Lpos := 4 + 2; // 4 is for address and 2 is for port length
            LIPVersion := Id_IPv4;
          end;
      // FQDN
      3: Lpos := LBuf[4] + 2; // 2 is for port length
      // IP V6
      4: begin
           LPos := 16 + 2; // 16 is for address and 2 is for port length
           LIPVersion := Id_IPv6;
         end;
    end;
    try
      // Socks server replies on connect, this is the second part
      FUDPSocksAssociation.ReadBytes(LBuf, Lpos, False); //overwrite the first part for now
      AHandle.SetPeer( (FUDPSocksAssociation as TIdIOHandlerStack).Binding.PeerIP ,LBuf[4]*256+LBuf[5],LIPVersion);
      AHandle.Connect;
    except
      IndyRaiseOuterException(EIdSocksServerRespondError.Create(RSSocksServerRespondError));
    end;
  except
    FUDPSocksAssociation.Close;
    raise;
  end;
end;

procedure TIdSocksInfo.CloseUDP(AHandle: TIdSocketHandle);
begin
  case Version of
    svSocks4, svSocks4A: raise EIdSocksUDPNotSupportedBySOCKSVersion.Create(RSSocksUDPNotSupported);
    svSocks5: CloseSocks5UDPAssociation;
  end;
end;

procedure TIdSocksInfo.OpenUDP(AHandle: TIdSocketHandle;
  const AHost: string=''; const APort: TIdPort=0; const AIPVersion: TIdIPVersion = ID_DEFAULT_IP_VERSION);
begin
  case Version of
    svSocks4, svSocks4A: raise EIdSocksUDPNotSupportedBySOCKSVersion.Create(RSSocksUDPNotSupported);
    svSocks5: MakeSocks5UDPAssociation(AHandle);
  end;
end;

function TIdSocksInfo.DisasmUDPReplyPacket(const APacket : TIdBytes;
  var VHost : String; var VPort : TIdPort; var VIPVersion: TIdIPVersion): TIdBytes;
{


      +----+------+------+----------+----------+----------+
      |RSV | FRAG | ATYP | DST.ADDR | DST.PORT |   DATA   |
      +----+------+------+----------+----------+----------+
      | 2  |  1   |  1   | Variable |    2     | Variable |
      +----+------+------+----------+----------+----------+
        01    2      3
     The fields in the UDP request header are:

          o  RSV  Reserved X'0000'
          o  FRAG    Current fragment number
          o  ATYP    address type of following addresses:
             o  IP V4 address: X'01'
             o  DOMAINNAME: X'03'
             o  IP V6 address: X'04'
          o  DST.ADDR       desired destination address
          o  DST.PORT       desired destination port
          o  DATA     user data
}
var
  LLen : Integer;
  LIP6 : TIdIPv6Address;
  i : Integer;
begin
  if Length(APacket) < 5 then begin
    Exit;
  end;
  // type of destination address is domain name
  case APacket[3] of
    // IP V4
    1: begin
         LLen := 4 + 4; //4 IPv4 address len, 4- 2 reserved, 1 frag, 1 atype
         VHost := BytesToIPv4Str(APacket, 4);
         VIPVersion := Id_IPv4;
       end;
    // FQDN
    3: begin
         LLen := APacket[4] +4; // 2 is for port length, 4 - 2 reserved, 1 frag, 1 atype
         if Length(APacket)< (5+LLen) then begin
           Exit;
         end;
         VHost := BytesToString(APacket, 5, APacket[4]);
         // VIPVersion is pre-initialized by the receiving socket before DisasmUDPReplyPacket() is called
       end;
    // IP V6  - 4:
    else begin
      LLen := 16 + 4; // 16 is for address, 2 is for port length, 4 - 2 reserved, 1 frag, 1 atype
      BytesToIPv6(APacket, LIP6, 5);
      for i := 0 to 7 do begin
        LIP6[i] := GStack.NetworkToHost(LIP6[i]);
      end;
      VHost := IPv6AddressToStr(LIP6);
      VIPVersion := Id_IPv6;
    end;
  end;
  VPort := APacket[LLen]*256 + APacket[LLen+1];
  LLen := LLen + 2;
  SetLength(Result, Length(APacket)-LLen);
  CopyTIdBytes(APacket, LLen, Result, 0, Length(APacket)-LLen);
end;

function TIdSocksInfo.MakeUDPRequestPacket(const AData: TIdBytes;
  const AHost : String; const APort : TIdPort) : TIdBytes;
{


      +----+------+------+----------+----------+----------+
      |RSV | FRAG | ATYP | DST.ADDR | DST.PORT |   DATA   |
      +----+------+------+----------+----------+----------+
      | 2  |  1   |  1   | Variable |    2     | Variable |
      +----+------+------+----------+----------+----------+
        01    2      3
     The fields in the UDP request header are:

          o  RSV  Reserved X'0000'
          o  FRAG    Current fragment number
          o  ATYP    address type of following addresses:
             o  IP V4 address: X'01'
             o  DOMAINNAME: X'03'
             o  IP V6 address: X'04'
          o  DST.ADDR       desired destination address
          o  DST.PORT       desired destination port
          o  DATA     user data
}
var
  LLen : Integer;
  LIP : TIdIPAddress;
  LAddr: TIdBytes;
begin
  SetLength(Result, 1024);
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0; //no fragmentation - too lazy to implement it

  // address type: IP V4 address: X'01'    {Do not Localize}
  //               DOMAINNAME:    X'03'    {Do not Localize}
  //               IP V6 address: X'04'    {Do not Localize}

  LIP := TIdIPAddress.MakeAddressObject(AHost);
  if Assigned(LIP) then
  begin
    try
      if LIP.AddrType = Id_IPv6 then begin
        Result[3] := $04; //IPv6 address
      end else begin
        Result[3] := $01; //IPv4 address
      end;
      LLen := 4;
      LAddr := LIP.HToNBytes;
      CopyTIdBytes(LAddr, 0, Result, 4, Length(LAddr));
      LLen := LLen + Length(LAddr);
    finally
      FreeAndNil(LIP);
    end;
  end else
  begin
    LAddr := ToBytes(AHost);
    Result[3] := $3; // host name
    Result[4] := IndyMin(Length(LAddr), 255);
    if Result[4] > 0 then begin
      CopyTIdBytes(LAddr, 0, Result, 5, Result[4]);
    end;
    LLen := 5 + Result[4];
  end;

  // port
  CopyTIdUInt16(GStack.HostToNetwork(APort), Result, LLen);
  LLen := LLen + 2;

  //now do the rest of the packet
  SetLength(Result, LLen + Length(AData));
  CopyTIdBytes(AData, 0, Result, LLen, Length(AData));
end;

function TIdSocksInfo.RecvFromUDP(AHandle: TIdSocketHandle;
  var ABuffer : TIdBytes; var VPeerIP: string; var VPeerPort: TIdPort;
  var VIPVersion: TIdIPVersion; AMSec: Integer = IdTimeoutDefault): Integer;
var
  LBuf : TIdBytes;
begin
  case Version of
    svSocks4, svSocks4A: raise EIdSocksUDPNotSupportedBySOCKSVersion.Create(RSSocksUDPNotSupported);
  end;
  SetLength(LBuf, Length(ABuffer)+200);

  if not AHandle.Readable(AMSec) then begin
    Result := 0;
    VPeerIP := '';    {Do not Localize}
    VPeerPort := 0;
    VIPVersion := ID_DEFAULT_IP_VERSION;
    Exit;
  end;
  Result := AHandle.RecvFrom(LBuf, VPeerIP, VPeerPort, VIPVersion);
  SetLength(LBuf, Result);
  LBuf := DisasmUDPReplyPacket(LBuf, VPeerIP, VPeerPort, VIPVersion);
  Result := Length(LBuf);
  CopyTIdBytes(LBuf, 0, ABuffer, 0, Result);
end;

procedure TIdSocksInfo.SendToUDP(AHandle: TIdSocketHandle; const AHost: string;
  const APort: TIdPort; const AIPVersion: TIdIPVersion; const ABuffer : TIdBytes);
var
  LBuf : TIdBytes;
begin
  case Version of
    svSocks4, svSocks4A: raise EIdSocksUDPNotSupportedBySOCKSVersion.Create(RSSocksUDPNotSupported);
  end;
  LBuf := MakeUDPRequestPacket(ABuffer, AHost, APort);
  AHandle.Send(LBuf, 0);
end;

destructor TIdSocksInfo.Destroy;
begin
  FreeAndNil(FUDPSocksAssociation);
  inherited Destroy;
end;

end.

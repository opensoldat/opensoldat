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
  Rev 1.0    2004.02.07 12:25:16 PM  czhower
  Recheckin to fix case of filename

  Rev 1.1    2/7/2004 5:20:06 AM  JPMugaas
  Added some constants that were pasted from other places.  DotNET uses the
  standard Winsock 2 error consstants.  We don't want to link to IdWInsock or
  Windows though because that causes other problems.

  Rev 1.0    2004.02.03 3:14:44 PM  czhower
  Move and updates

  Rev 1.12    2003.12.28 6:53:46 PM  czhower
  Added some consts.

  Rev 1.11    10/28/2003 9:14:54 PM  BGooijen
  .net

  Rev 1.10    10/19/2003 10:46:18 PM  BGooijen
  Added more consts

  Rev 1.9    10/19/2003 9:15:28 PM  BGooijen
  added some SocketOptionName consts for dotnet

  Rev 1.8    10/19/2003 5:21:30 PM  BGooijen
  SetSocketOption

  Rev 1.7    10/2/2003 7:31:18 PM  BGooijen
  .net

  Rev 1.6    2003.10.02 10:16:32 AM  czhower
  .Net

  Rev 1.5    2003.10.01 5:05:18 PM  czhower
  .Net

  Rev 1.4    2003.10.01 1:12:40 AM  czhower
  .Net

  Rev 1.3    2003.09.30 12:09:38 PM  czhower
  DotNet changes.

  Rev 1.2    9/29/2003 10:28:30 PM  BGooijen
  Added constants for DotNet

  Rev 1.1    12-14-2002 14:58:34  BGooijen
  Added definition for Id_SOCK_RDM and Id_SOCK_SEQPACKET

  Rev 1.0    11/13/2002 08:59:14 AM  JPMugaas
}

unit IdStackConsts;

interface

{$I IdCompilerDefines.inc}

{ This should be the only unit except OS Stack units that reference
  Winsock or lnxsock }

uses
  {$IFDEF DOTNET}
  System.Net.Sockets;
  {$ENDIF}
  //TODO:  I'm not really sure how other platforms are supported with asockets header
  //Do I use the sockets unit or do something totally different for each platform
  {$IFDEF WINDOWS}
  IdWship6, //for some constants that supplement IdWinsock
  IdWinsock2;
  {$ENDIF}
  {$IFDEF OS2}
  pmwsock;
  {$ENDIF}
  {$IFDEF SOCKETTYPE_IS_CINT}
  IdCTypes,
  {$ENDIF}
  {$IFDEF NETWARE_CLIB}
  winsock; //not sure if this is correct
  {$ENDIF}
  {$IFDEF NETWARE_LIBC}
  winsock;  //not sure if this is correct
  {$ENDIF}
  {$IFDEF MACOS_CLASSIC}
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF USE_VCL_POSIX}
	{
	IMPORTANT!!!
	
	The new Posix units have platform specific stuff.  Since this code and 
	the definitions are not intented to be compiled in non-Unix-like operating
	systems, platform warnings are not going to be too helpful.
	}
      {$I IdSymbolPlatformOff.inc}
      IdVCLPosixSupplemental,
      Posix.Errno,Posix.NetDB, Posix.NetinetIn, Posix.SysSocket;
    {$ENDIF}
    {$IFDEF KYLIXCOMPAT}
    libc;
    {$ENDIF}
    {$IFDEF USE_BASEUNIX}
    Sockets, BaseUnix, Unix; // FPC "native" Unix units.
     //Marco may want to change the socket interface unit
     //so we don't use the libc header.
    {$ENDIF}
  {$ENDIF}

type
  {$IFDEF USE_BASEUNIX}
  TSocket = cint;  // TSocket is afaik not POSIX, so we have to add it
                   // (Socket() returns a C int according to opengroup)
  {$ENDIF}

  TIdStackSocketHandle =
  {$IFDEF DOTNET}
     Socket
  {$ELSE}
    {$IFDEF USE_VCL_POSIX}
    Integer
    {$ELSE}
    TSocket
    {$ENDIF}
  {$ENDIF};

var
  Id_SO_True: Integer = 1;
  Id_SO_False: Integer = 0;

const
  {$IFDEF DOTNET}
  Id_IPV6_UNICAST_HOPS    = SocketOptionName.IpTimeToLive;
  Id_IPV6_MULTICAST_IF    = SocketOptionName.MulticastInterface;
  Id_IPV6_MULTICAST_HOPS  = SocketOptionName.MulticastTimeToLive;
  Id_IPV6_MULTICAST_LOOP  = SocketOptionName.MulticastLoopback;
  Id_IPV6_ADD_MEMBERSHIP  = SocketOptionName.AddMembership;
  Id_IPV6_DROP_MEMBERSHIP = SocketOptionName.DropMembership;
  Id_IPV6_PKTINFO         = SocketOptionName.PacketInformation;
  Id_IP_MULTICAST_TTL     = SocketOptionName.MulticastTimeToLive;
  Id_IP_MULTICAST_LOOP    = SocketOptionName.MulticastLoopback;
  Id_IP_ADD_MEMBERSHIP    = SocketOptionName.AddMembership;
  Id_IP_DROP_MEMBERSHIP   = SocketOptionName.DropMembership;
  Id_IP_HDR_INCLUDED      = SocketOptionName.HeaderIncluded;
  {$ENDIF}

  {$IFDEF UNIX}
  Id_IPV6_UNICAST_HOPS   = IPV6_UNICAST_HOPS;
  Id_IPV6_MULTICAST_IF   = IPV6_MULTICAST_IF;
  Id_IPV6_MULTICAST_HOPS = IPV6_MULTICAST_HOPS;
  Id_IPV6_MULTICAST_LOOP = IPV6_MULTICAST_LOOP;
    {$IFDEF LINUX}
    // These are probably leftovers from the non-final IPV6 KAME standard
    // in Linux. They only seem to exist in Linux, others use
    // the standarised versions.
    // Probably the JOIN_GROUP ones replaced these,
    // but they have different numbers in Linux, and possibly
    // also different behaviour?
      {$IFNDEF KYLIX}
        {$IFDEF USE_BASEUNIX}
        //In Linux, the libc.pp header maps the old values to new ones,
        //probably for consistancy.  I'm doing this because we can't link
        //to Libc for Basic Unix stuff and some people may want to use this API
        //in Linux instead of the libc API.
        IPV6_ADD_MEMBERSHIP  = IPV6_JOIN_GROUP;
        IPV6_DROP_MEMBERSHIP = IPV6_LEAVE_GROUP;
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
    // FIXME: Android compiler is using these definitions, but maybe some
    //        EXTERNALSYM-work is needed above.
      IPV6_ADD_MEMBERSHIP  = IPV6_JOIN_GROUP;
      {$EXTERNALSYM IPV6_ADD_MEMBERSHIP}
      IPV6_DROP_MEMBERSHIP = IPV6_LEAVE_GROUP;
      {$EXTERNALSYM IPV6_DROP_MEMBERSHIP}
      {$IFNDEF USE_VCL_POSIX}
      IPV6_CHECKSUM        = 26;
      {$ENDIF}
    {$ENDIF}
  Id_IPV6_ADD_MEMBERSHIP  = IPV6_ADD_MEMBERSHIP;
  Id_IPV6_DROP_MEMBERSHIP = IPV6_DROP_MEMBERSHIP;
  Id_IPV6_PKTINFO         = IPV6_PKTINFO;
  Id_IPV6_HOPLIMIT        = IPV6_HOPLIMIT;
  Id_IP_MULTICAST_TTL     = IP_MULTICAST_TTL; // TODO integrate into IdStackConsts
  Id_IP_MULTICAST_LOOP    = IP_MULTICAST_LOOP; // TODO integrate into IdStackConsts
  Id_IP_ADD_MEMBERSHIP    = IP_ADD_MEMBERSHIP; // TODO integrate into IdStackConsts
  Id_IP_DROP_MEMBERSHIP   = IP_DROP_MEMBERSHIP; // TODO integrate into IdStackConsts

  //In Windows CE 4.2, IP_HDRINCL may not be supported.
  Id_IP_HDR_INCLUDED      = IP_HDRINCL; // TODO integrate into IdStackConsts
  {$ENDIF}

  {$IFDEF WINDOWS}
  Id_IPV6_HDRINCL         = IPV6_HDRINCL;
  Id_IPV6_UNICAST_HOPS    = IPV6_UNICAST_HOPS;
  Id_IPV6_MULTICAST_IF    = IPV6_MULTICAST_IF;
  Id_IPV6_MULTICAST_HOPS  = IPV6_MULTICAST_HOPS;
  Id_IPV6_MULTICAST_LOOP  = IPV6_MULTICAST_LOOP;
  Id_IPV6_ADD_MEMBERSHIP  = IPV6_ADD_MEMBERSHIP;
  Id_IPV6_DROP_MEMBERSHIP = IPV6_DROP_MEMBERSHIP;
  Id_IPV6_PKTINFO         = IPV6_PKTINFO;
  Id_IPV6_HOPLIMIT        = IPV6_HOPLIMIT;
  Id_IP_MULTICAST_TTL     = 10; // TODO integrate into IdStackConsts FIX ERROR in IdWinsock
  Id_IP_MULTICAST_LOOP    = 11; // TODO integrate into IdStackConsts FIX ERROR in IdWinsock
  Id_IP_ADD_MEMBERSHIP    = 12; // TODO integrate into IdStackConsts FIX ERROR in IdWinsock
  Id_IP_DROP_MEMBERSHIP   = 13; // TODO integrate into IdStackConsts FIX ERROR in IdWinsock
  Id_IP_HDR_INCLUDED      = 2; // TODO integrate into IdStackConsts FIX ERROR in IdWinsock
  {$ENDIF}

(*
  There seems to be an error in the correct values of multicast values in IdWinsock
  The values should be:

  ip_options          = 1;  //* set/get IP options */
  ip_hdrincl          = 2;  //* header is included with data */
  ip_tos              = 3;  //* IP type of service and preced*/
  ip_ttl              = 4;  //* IP time to live */
  ip_multicast_if     = 9;  //* set/get IP multicast i/f  */
  ip_multicast_ttl    = 10; //* set/get IP multicast ttl */
  ip_multicast_loop   = 11; //*set/get IP multicast loopback */
  ip_add_membership   = 12; //* add an IP group membership */
  ip_drop_membership  = 13; //* drop an IP group membership */
  ip_dontfragment     = 14; //* don't fragment IP datagrams */    {Do not Localize}
*)
  {$IFDEF UNIX}
  TCP_NODELAY = 1;
  {$ENDIF}

  // Protocol Family

  {$IFNDEF DOTNET}
    {$IFDEF USE_VCL_POSIX}
  Id_PF_INET4 = AF_INET;
  Id_PF_INET6 = AF_INET6;
    {$ELSE}
  Id_PF_INET4 = PF_INET;
  Id_PF_INET6 = PF_INET6;
     {$ENDIF}
  {$ELSE}
  Id_PF_INET4 = ProtocolFamily.InterNetwork;
  Id_PF_INET6 = ProtocolFamily.InterNetworkV6;
  {$ENDIF}

  {$IFDEF USE_BASEUNIX}
  // These constants are actually WinSock specific, not std TCP/IP
  // FPC doesn't emulate WinSock.
  INVALID_SOCKET = -1;
  SOCKET_ERROR   = -1;
  {$ENDIF}

type
  // Socket Type
  {$IFDEF SOCKETTYPE_IS_CINT}
  TIdSocketType = TIdC_INT;
  {$ENDIF}
  {$IFDEF SOCKETTYPE_IS___SOCKETTYPE}
  TIdSocketType = __socket_type;
  {$ENDIF}
  {$IFDEF SOCKETTYPE_IS_LONGINT}
  TIdSocketType = Integer;
  {$ENDIF}
  {$IFDEF SOCKETTYPE_IS_SOCKETTYPE}
  TIdSocketType = SocketType;
  {$ENDIF}

const
//  {$IFNDEF DOTNET}
//    {$IFDEF KYLIXCOMPAT}
//  Id_SOCK_STREAM     = TIdSocketType(SOCK_STREAM);      //1               /* stream socket */
//  Id_SOCK_DGRAM      = TIdSocketType(SOCK_DGRAM);       //2               /* datagram socket */
//  Id_SOCK_RAW        = TIdSocketType(SOCK_RAW);         //3               /* raw-protocol interface */
//  Id_SOCK_RDM        = TIdSocketType(SOCK_RDM);         //4               /* reliably-delivered message */
//  Id_SOCK_SEQPACKET  = SOCK_SEQPACKET;   //5               /* sequenced packet stream */
//    {$ELSE}
//  Id_SOCK_STREAM     = SOCK_STREAM;      //1               /* stream socket */
//  Id_SOCK_DGRAM      = SOCK_DGRAM;       //2               /* datagram socket */
//  Id_SOCK_RAW        = SOCK_RAW;         //3               /* raw-protocol interface */
//  Id_SOCK_RDM        = SOCK_RDM;         //4               /* reliably-delivered message */
//  Id_SOCK_SEQPACKET  = SOCK_SEQPACKET;   //5               /* sequenced packet stream */
//    {$ENDIF}
  {$IFDEF SOCKETTYPE_IS_SOCKETTYPE}
  Id_SOCK_STREAM     = SocketType.Stream;         // /* stream socket */
  Id_SOCK_DGRAM      = SocketType.Dgram;          // /* datagram socket */
  Id_SOCK_RAW        = SocketType.Raw;            // /* raw-protocol interface */
  Id_SOCK_RDM        = SocketType.Rdm;            // /* reliably-delivered message */
  Id_SOCK_SEQPACKET  = SocketType.Seqpacket;      // /* sequenced packet stream */
  Id_SOCK_UNKNOWN    = SocketType.Unknown;        // /* unknown */
  {$ELSE}
  Id_SOCK_UNKNOWN    = TIdSocketType(0);
  Id_SOCK_STREAM     = TIdSocketType(SOCK_STREAM);      //1               /* stream socket */
  Id_SOCK_DGRAM      = TIdSocketType(SOCK_DGRAM);       //2               /* datagram socket */
  Id_SOCK_RAW        = TIdSocketType(SOCK_RAW);         //3               /* raw-protocol interface */
     {$IFNDEF USE_VCL_POSIX}
  Id_SOCK_RDM        = TIdSocketType(SOCK_RDM);         //4               /* reliably-delivered message */
     {$ENDIF}
  Id_SOCK_SEQPACKET  = SOCK_SEQPACKET;   //5               /* sequenced packet stream */
  {$ENDIF}

type
  // IP Protocol type
  TIdSocketProtocol     = {$IFDEF DOTNET}ProtocolType{$ELSE}Integer{$ENDIF};
  TIdSocketOption       = {$IFDEF DOTNET}SocketOptionName{$ELSE}Integer{$ENDIF};
  TIdSocketOptionLevel  = {$IFDEF DOTNET}SocketOptionLevel{$ELSE}Integer{$ENDIF};



const
  {$IFNDEF DOTNET}
    {$IFDEF OS2}
  Id_IPPROTO_GGP    = IPPROTO_GGP; //OS/2 does something strange and we might wind up
  //supporting it later for all we know.
    {$ELSE}
  Id_IPPROTO_GGP    = 3;// IPPROTO_GGP; may not be defined in some headers in FPC
    {$ENDIF}
  Id_IPPROTO_ICMP   = IPPROTO_ICMP;
  Id_IPPROTO_ICMPV6 = IPPROTO_ICMPV6;
    {$IFNDEF USE_VCL_POSIX}
  Id_IPPROTO_IDP    = IPPROTO_IDP;

  Id_IPPROTO_IGMP   = IPPROTO_IGMP;
    {$ENDIF}
  Id_IPPROTO_IP     = IPPROTO_IP;
  Id_IPPROTO_IPv6   = IPPROTO_IPV6;
  Id_IPPROTO_ND     = 77; //IPPROTO_ND; is not defined in some headers in FPC
  Id_IPPROTO_PUP    = IPPROTO_PUP;
  Id_IPPROTO_RAW    = IPPROTO_RAW;
  Id_IPPROTO_TCP    = IPPROTO_TCP;
  Id_IPPROTO_UDP    = IPPROTO_UDP;
  Id_IPPROTO_MAX    = IPPROTO_MAX;
  {$ELSE}
  Id_IPPROTO_GGP         = ProtocolType.Ggp;    //Gateway To Gateway Protocol.
  Id_IPPROTO_ICMP        = ProtocolType.Icmp; //Internet Control Message Protocol.
  Id_IPPROTO_ICMPv6      = ProtocolType.IcmpV6; //ICMP for IPv6
  Id_IPPROTO_IDP         = ProtocolType.Idp;   //IDP Protocol.
  Id_IPPROTO_IGMP        = ProtocolType.Igmp; //Internet Group Management Protocol.
  Id_IPPROTO_IP          = ProtocolType.IP;     //Internet Protocol.
  Id_IPPROTO_IPv6        = ProtocolType.IPv6;
  Id_IPPROTO_IPX         = ProtocolType.Ipx; //IPX Protocol.
  Id_IPPROTO_ND          = ProtocolType.ND;  //Net Disk Protocol (unofficial).
  Id_IPPROTO_PUP         = ProtocolType.Pup; //PUP Protocol.
  Id_IPPROTO_RAW         = ProtocolType.Raw;  //Raw UP packet protocol.
  Id_IPPROTO_SPX         = ProtocolType.Spx;  //SPX Protocol.
  Id_IPPROTO_SPXII       = ProtocolType.SpxII; //SPX Version 2 Protocol.
  Id_IPPROTO_TCP         = ProtocolType.Tcp;  //Transmission Control Protocol.
  Id_IPPROTO_UDP         = ProtocolType.Udp;  //User Datagram Protocol.
  Id_IPPROTO_UNKNOWN     = ProtocolType.Unknown; //Unknown protocol.
  Id_IPPROTO_UNSPECIFIED = ProtocolType.Unspecified; //unspecified protocol.
//  Id_IPPROTO_MAX = ProtocolType.; ?????????????????????
  {$ENDIF}



  // Socket Option level
  {$IFNDEF DOTNET}
  Id_SOL_SOCKET = SOL_SOCKET;
  Id_SOL_IP     = IPPROTO_IP;
  Id_SOL_IPv6   = IPPROTO_IPV6;
  Id_SOL_TCP    = IPPROTO_TCP;
  Id_SOL_UDP    = IPPROTO_UDP;
  {$ELSE}
  Id_SOL_SOCKET = SocketOptionLevel.Socket;
  Id_SOL_IP     = SocketOptionLevel.Ip;
  Id_SOL_IPv6   = SocketOptionLevel.IPv6;
  Id_SOL_TCP    = SocketOptionLevel.Tcp;
  Id_SOL_UDP    = SocketOptionLevel.Udp;
  {$ENDIF}

  // Socket options
  {$IFNDEF DOTNET}
    {$IFNDEF WINDOWS}
  SO_DONTLINGER          =  not SO_LINGER;
  {$EXTERNALSYM SO_DONTLINGER}
    {$ENDIF}
  Id_SO_BROADCAST        =  SO_BROADCAST;
  Id_SO_DEBUG            =  SO_DEBUG;
  Id_SO_DONTLINGER       =  SO_DONTLINGER;
  Id_SO_DONTROUTE        =  SO_DONTROUTE;
  Id_SO_ERROR            =  SO_ERROR;
  Id_SO_KEEPALIVE        =  SO_KEEPALIVE;
  Id_SO_LINGER	         =  SO_LINGER;
  Id_SO_OOBINLINE        =  SO_OOBINLINE;
  Id_SO_RCVBUF           =  SO_RCVBUF;
  Id_SO_REUSEADDR        =  SO_REUSEADDR;
    {$IFDEF LINUX}
   // SO_REUSEPORT has different values on different platforms, but for
   // right now we are only interested in it on Linux (it is 512 on BSD)...
  Id_SO_REUSEPORT        =  15;//SO_REUSEPORT; is not defined in some headers in FPC
    {$ENDIF}
  Id_SO_SNDBUF           =  SO_SNDBUF;
  Id_SO_TYPE             =  SO_TYPE;
    {$IFDEF WINDOWS}
  Id_SO_UPDATE_ACCEPT_CONTEXT  = SO_UPDATE_ACCEPT_CONTEXT;
  Id_SO_UPDATE_CONNECT_CONTEXT = SO_UPDATE_CONNECT_CONTEXT;
    {$ENDIF}
  {$ELSE}
{
SocketOptionName.AcceptConnection;// Socket is listening.
SocketOptionName.AddMembership;//  Add an IP group membership.
SocketOptionName.AddSourceMembership;//  Join a source group.
SocketOptionName.BlockSource;//  Block data from a source.
}
  Id_SO_BROADCAST        =  SocketOptionName.Broadcast;//  Permit sending broadcast messages on the socket.
{
SocketOptionName.BsdUrgent;//  Use urgent data as defined in RFC-1222. This option can be set only once, and once set, cannot be turned off.
SocketOptionName.ChecksumCoverage;//  Set or get UDP checksum coverage.
}
  Id_SO_DEBUG            =  SocketOptionName.Debug;//  Record debugging information.
{
SocketOptionName.DontFragment;//  Do not fragment IP datagrams.
}
  Id_SO_DONTLINGER       =  SocketOptionName.DontLinger;//  Close socket gracefully without lingering.
  Id_SO_DONTROUTE        =  SocketOptionName.DontRoute;//  Do not route; send directly to interface addresses.
{
SocketOptionName.DropMembership;//  Drop an IP group membership.
SocketOptionName.DropSourceMembership;//  Drop a source group.
}
  Id_SO_ERROR            =  SocketOptionName.Error;//  Get error status and clear.
{
SocketOptionName.ExclusiveAddressUse;//  Enables a socket to be bound for exclusive access.
SocketOptionName.Expedited;//  Use expedited data as defined in RFC-1222. This option can be set only once, and once set, cannot be turned off.
SocketOptionName.HeaderIncluded;//  Indicates application is providing the IP header for outgoing datagrams.
SocketOptionName.IPOptions;//  Specifies IP options to be inserted into outgoing datagrams.
}
  Id_SO_KEEPALIVE        =  SocketOptionName.KeepAlive;//  Send keep-alives.
  Id_SO_LINGER           =  SocketOptionName.Linger;//  Linger on close if unsent data is present.
{
SocketOptionName.MaxConnections;//  Maximum queue length that can be specified by Listen.
SocketOptionName.MulticastInterface;//  Set the interface for outgoing multicast packets.
SocketOptionName.MulticastLoopback;//  IP multicast loopback.
SocketOptionName.MulticastTimeToLive;//  IP multicast time to live.
SocketOptionName.NoChecksum;//  Send UDP datagrams with checksum set to zero.
SocketOptionName.NoDelay;//  Disables the Nagle algorithm for send coalescing.
}
  Id_SO_OOBINLINE        =  SocketOptionName.OutOfBandInline;//  Receives out-of-band data in the normal data stream.
{
SocketOptionName.PacketInformation;//  Return information about received packets.
}
  Id_SO_RCVBUF           =  SocketOptionName.ReceiveBuffer;//  Specifies the total per-socket buffer space reserved for receives. This is unrelated to the maximum message size or the size of a TCP window.
{
SocketOptionName.ReceiveLowWater;//  Receive low water mark.
SocketOptionName.ReceiveTimeout;//  Receive time out. This option applies only to synchronous methods; it has no effect on asynchronous methods such as BeginSend.
}
  Id_SO_REUSEADDR        =  SocketOptionName.ReuseAddress;//  Allows the socket to be bound to an address that is already in use.
  Id_SO_SNDBUF           =  SocketOptionName.SendBuffer;//  Specifies the total per-socket buffer space reserved for sends. This is unrelated to the maximum message size or the size of a TCP window.
{
SocketOptionName.SendLowWater;//  Specifies the total per-socket buffer space reserved for receives. This is unrelated to the maximum message size or the size of a TCP window.
SocketOptionName.SendTimeout;//  Send timeout. This option applies only to synchronous methods; it has no effect on asynchronous methods such as BeginSend.
}
  Id_SO_TYPE             = SocketOptionName.Type;//  Get socket type.
{
SocketOptionName.TypeOfService;//  Change the IP header type of service field.
SocketOptionName.UnblockSource;//  Unblock a previously blocked source.
}
  Id_SO_UPDATE_ACCEPT_CONTEXT  = SocketOptionName.UpdateAcceptContext;// Updates an accepted socket's properties by using those of an existing socket.
  Id_SO_UPDATE_CONNECT_CONTEXT = SocketOptionName.UpdateConnectContext;// Updates a connected socket's properties by using those of an existing socket.
{
SocketOptionName.UseLoopback;//  Bypass hardware when possible.
}
  {$ENDIF}

  // Additional socket options
  {$IFNDEF DOTNET}
  Id_SO_RCVTIMEO         = SO_RCVTIMEO;
  Id_SO_SNDTIMEO         = SO_SNDTIMEO;
  {$ELSE}
  Id_SO_RCVTIMEO         = SocketOptionName.ReceiveTimeout;
  Id_SO_SNDTIMEO         = SocketOptionName.SendTimeout;
  {$ENDIF}

  {$IFNDEF DOTNET}
  Id_SO_IP_TTL              = IP_TTL;
  {$ELSE}
  Id_SO_IP_TTL              = SocketOptionName.IpTimeToLive; //  Set the IP header time-to-live field.
  {$ENDIF}

  {$IFNDEF DOTNET}
  {for some reason, the compiler doesn't accept  INADDR_ANY below saying a constant is expected. }
   {$IFDEF USE_VCL_POSIX}
  Id_INADDR_ANY  = 0;// INADDR_ANY;
  Id_INADDR_NONE = $ffffffff;// INADDR_NONE;
   {$ELSE}
  Id_INADDR_ANY  = INADDR_ANY;
  Id_INADDR_NONE = INADDR_NONE;
    {$ENDIF}
  {$ENDIF}

  // TCP Options
  {$IFNDEF DOTNET}
    {$IFDEF USE_VCL_POSIX}
  INVALID_SOCKET           = -1;
  SOCKET_ERROR             = -1;
    {$ENDIF}
  Id_TCP_NODELAY           = TCP_NODELAY;
  Id_INVALID_SOCKET        = INVALID_SOCKET;
  Id_SOCKET_ERROR          = SOCKET_ERROR;
  Id_SOCKETOPTIONLEVEL_TCP = Id_IPPROTO_TCP; // BGO: rename to Id_SOL_TCP
    {$IFDEF HAS_TCP_CORK}
  Id_TCP_CORK = TCP_CORK;
    {$ENDIF}
    {$IFDEF HAS_TCP_NOPUSH}
  Id_TCP_NOPUSH = TCP_NOPUSH;
    {$ENDIF}
    {$IFDEF HAS_TCP_KEEPIDLE}
  Id_TCP_KEEPIDLE          = TCP_KEEPIDLE;
    {$ENDIF}
    {$IFDEF HAS_TCP_KEEPINTVL}
  Id_TCP_KEEPINTVL         = TCP_KEEPINTVL;
    {$ENDIF}
  {$ELSE}
  Id_TCP_NODELAY           = SocketOptionName.NoDelay;
  Id_INVALID_SOCKET        = nil;
  Id_SOCKET_ERROR          = -1;
  Id_SOCKETOPTIONLEVEL_TCP = SocketOptionLevel.TCP; // BGO: rename to Id_SOL_TCP
  {$ENDIF}

  {$IFDEF USE_VCL_POSIX}
  // Shutdown Options
  Id_SD_Recv = SHUT_RD;
  Id_SD_Send = SHUT_WR;
  Id_SD_Both = SHUT_RDWR;
  //
  //Temp defines.  They should be in Delphi's Posix.Errno.pas
  ESOCKTNOSUPPORT	= 44;		//* Socket type not supported */
  {$EXTERNALSYM ESOCKTNOSUPPORT}
  EPFNOSUPPORT = 46;		//* Protocol family not supported */
  {$EXTERNALSYM EPFNOSUPPORT}
  ESHUTDOWN = 58;		//* Can't send after socket shutdown */
  {$EXTERNALSYM ESHUTDOWN}
  ETOOMANYREFS = 59;		//* Too many references: can't splice */
  {$EXTERNALSYM ETOOMANYREFS}
  EHOSTDOWN = 64;		//* Host is down */
  {$EXTERNALSYM EHOSTDOWN}
  //
  Id_WSAEINTR           = EINTR;
  Id_WSAEBADF           = EBADF;
  Id_WSAEACCES          = EACCES;
  Id_WSAEFAULT          = EFAULT;
  Id_WSAEINVAL          = EINVAL;
  Id_WSAEMFILE          = EMFILE;
  Id_WSAEWOULDBLOCK     = EWOULDBLOCK;
  Id_WSAEINPROGRESS     = EINPROGRESS;
  Id_WSAEALREADY        = EALREADY;
  Id_WSAENOTSOCK        = ENOTSOCK;
  Id_WSAEDESTADDRREQ    = EDESTADDRREQ;
  Id_WSAEMSGSIZE        = EMSGSIZE;
  Id_WSAEPROTOTYPE      = EPROTOTYPE;
  Id_WSAENOPROTOOPT     = ENOPROTOOPT;
  Id_WSAEPROTONOSUPPORT = EPROTONOSUPPORT;
  Id_WSAESOCKTNOSUPPORT = ESOCKTNOSUPPORT;
  Id_WSAEOPNOTSUPP      = EOPNOTSUPP;
  Id_WSAEPFNOSUPPORT    = EPFNOSUPPORT;
  Id_WSAEAFNOSUPPORT    = EAFNOSUPPORT;
  Id_WSAEADDRINUSE      = EADDRINUSE;
  Id_WSAEADDRNOTAVAIL   = EADDRNOTAVAIL;
  Id_WSAENETDOWN        = ENETDOWN;
  Id_WSAENETUNREACH     = ENETUNREACH;
  Id_WSAENETRESET       = ENETRESET;
  Id_WSAECONNABORTED    = ECONNABORTED;
  Id_WSAECONNRESET      = ECONNRESET;
  Id_WSAENOBUFS         = ENOBUFS;
  Id_WSAEISCONN         = EISCONN;
  Id_WSAENOTCONN        = ENOTCONN;
  Id_WSAESHUTDOWN       = ESHUTDOWN;
  Id_WSAETOOMANYREFS    = ETOOMANYREFS;
  Id_WSAETIMEDOUT       = ETIMEDOUT;
  Id_WSAECONNREFUSED    = ECONNREFUSED;
  Id_WSAELOOP           = ELOOP;
  Id_WSAENAMETOOLONG    = ENAMETOOLONG;
  Id_WSAEHOSTDOWN       = EHOSTDOWN;
  Id_WSAEHOSTUNREACH    = EHOSTUNREACH;
  Id_WSAENOTEMPTY       = ENOTEMPTY;
  {$ENDIF}

  {$IFDEF KYLIXCOMPAT}
  // Shutdown Options
  Id_SD_Recv = SHUT_RD;
  Id_SD_Send = SHUT_WR;
  Id_SD_Both = SHUT_RDWR;
  //
  Id_WSAEINTR           = EINTR;
  Id_WSAEBADF           = EBADF;
  Id_WSAEACCES          = EACCES;
  Id_WSAEFAULT          = EFAULT;
  Id_WSAEINVAL          = EINVAL;
  Id_WSAEMFILE          = EMFILE;
  Id_WSAEWOULDBLOCK     = EWOULDBLOCK;
  Id_WSAEINPROGRESS     = EINPROGRESS;
  Id_WSAEALREADY        = EALREADY;
  Id_WSAENOTSOCK        = ENOTSOCK;
  Id_WSAEDESTADDRREQ    = EDESTADDRREQ;
  Id_WSAEMSGSIZE        = EMSGSIZE;
  Id_WSAEPROTOTYPE      = EPROTOTYPE;
  Id_WSAENOPROTOOPT     = ENOPROTOOPT;
  Id_WSAEPROTONOSUPPORT = EPROTONOSUPPORT;
  Id_WSAESOCKTNOSUPPORT = ESOCKTNOSUPPORT;
  Id_WSAEOPNOTSUPP      = EOPNOTSUPP;
  Id_WSAEPFNOSUPPORT    = EPFNOSUPPORT;
  Id_WSAEAFNOSUPPORT    = EAFNOSUPPORT;
  Id_WSAEADDRINUSE      = EADDRINUSE;
  Id_WSAEADDRNOTAVAIL   = EADDRNOTAVAIL;
  Id_WSAENETDOWN        = ENETDOWN;
  Id_WSAENETUNREACH     = ENETUNREACH;
  Id_WSAENETRESET       = ENETRESET;
  Id_WSAECONNABORTED    = ECONNABORTED;
  Id_WSAECONNRESET      = ECONNRESET;
  Id_WSAENOBUFS         = ENOBUFS;
  Id_WSAEISCONN         = EISCONN;
  Id_WSAENOTCONN        = ENOTCONN;
  Id_WSAESHUTDOWN       = ESHUTDOWN;
  Id_WSAETOOMANYREFS    = ETOOMANYREFS;
  Id_WSAETIMEDOUT       = ETIMEDOUT;
  Id_WSAECONNREFUSED    = ECONNREFUSED;
  Id_WSAELOOP           = ELOOP;
  Id_WSAENAMETOOLONG    = ENAMETOOLONG;
  Id_WSAEHOSTDOWN       = EHOSTDOWN;
  Id_WSAEHOSTUNREACH    = EHOSTUNREACH;
  Id_WSAENOTEMPTY       = ENOTEMPTY;
  {$ENDIF}

  {$IFDEF USE_BASEUNIX}
  // Shutdown Options
  Id_SD_Recv = SHUT_RD;
  Id_SD_Send = SHUT_WR;
  Id_SD_Both = SHUT_RDWR;
    {$IFDEF BEOS}
  {work around incomplete definitions in BeOS FPC compiler.}
  EDESTADDRREQ = (B_POSIX_ERROR_BASE + 48);
  {$EXTERNALSYM EDESTADDRREQ}
  EHOSTDOWN = (B_POSIX_ERROR_BASE + 45);
  {$EXTERNALSYM EHOSTDOWN}
  ESysENOTSOCK = ENOTSOCK;
  ESysEDESTADDRREQ = EDESTADDRREQ;
  ESysEMSGSIZE = EMSGSIZE;
  ESysEOPNOTSUPP = EOPNOTSUPP;
  ESysEHOSTDOWN = EHOSTDOWN;
    {$ENDIF}
  //
  Id_WSAEINTR           = ESysEINTR;
  Id_WSAEBADF           = ESysEBADF;
  Id_WSAEACCES          = ESysEACCES;
  Id_WSAEFAULT          = ESysEFAULT;
  Id_WSAEINVAL          = ESysEINVAL;
  Id_WSAEMFILE          = ESysEMFILE;
  Id_WSAEWOULDBLOCK     = ESysEWOULDBLOCK;
  Id_WSAEINPROGRESS     = ESysEINPROGRESS;
  Id_WSAEALREADY        = ESysEALREADY;
  Id_WSAENOTSOCK        = ESysENOTSOCK;
  Id_WSAEDESTADDRREQ    = ESysEDESTADDRREQ;
  Id_WSAEMSGSIZE        = ESysEMSGSIZE;
  Id_WSAEPROTOTYPE      = ESysEPROTOTYPE;
  Id_WSAENOPROTOOPT     = ESysENOPROTOOPT;
  Id_WSAEPROTONOSUPPORT = ESysEPROTONOSUPPORT;
  {$IFNDEF BEOS}
  Id_WSAESOCKTNOSUPPORT = ESysESOCKTNOSUPPORT;
  {$ENDIF}
  Id_WSAEOPNOTSUPP      = ESysEOPNOTSUPP;
  Id_WSAEPFNOSUPPORT    = ESysEPFNOSUPPORT;
  Id_WSAEAFNOSUPPORT    = ESysEAFNOSUPPORT;
  Id_WSAEADDRINUSE      = ESysEADDRINUSE;
  Id_WSAEADDRNOTAVAIL   = ESysEADDRNOTAVAIL;
  Id_WSAENETDOWN        = ESysENETDOWN;
  Id_WSAENETUNREACH     = ESysENETUNREACH;
  Id_WSAENETRESET       = ESysENETRESET;
  Id_WSAECONNABORTED    = ESysECONNABORTED;
  Id_WSAECONNRESET      = ESysECONNRESET;
  Id_WSAENOBUFS         = ESysENOBUFS;
  Id_WSAEISCONN         = ESysEISCONN;
  Id_WSAENOTCONN        = ESysENOTCONN;
  Id_WSAESHUTDOWN       = ESysESHUTDOWN;
  {$IFNDEF BEOS}
  Id_WSAETOOMANYREFS    = ESysETOOMANYREFS;
  {$ENDIF}
  Id_WSAETIMEDOUT       = ESysETIMEDOUT;
  Id_WSAECONNREFUSED    = ESysECONNREFUSED;
  Id_WSAELOOP           = ESysELOOP;
  Id_WSAENAMETOOLONG    = ESysENAMETOOLONG;
  Id_WSAEHOSTDOWN       = ESysEHOSTDOWN;
  Id_WSAEHOSTUNREACH    = ESysEHOSTUNREACH;
  Id_WSAENOTEMPTY       = ESysENOTEMPTY;
  {$ENDIF}

  {$IFDEF WINDOWS}
  // Shutdown Options
  Id_SD_Recv = 0;
  Id_SD_Send = 1;
  Id_SD_Both = 2;
  //
  Id_WSAEINTR           = WSAEINTR;
  Id_WSAEBADF           = WSAEBADF;
  Id_WSAEACCES          = WSAEACCES;
  Id_WSAEFAULT          = WSAEFAULT;
  Id_WSAEINVAL          = WSAEINVAL;
  Id_WSAEMFILE          = WSAEMFILE;
  Id_WSAEWOULDBLOCK     = WSAEWOULDBLOCK;
  Id_WSAEINPROGRESS     = WSAEINPROGRESS;
  Id_WSAEALREADY        = WSAEALREADY;
  Id_WSAENOTSOCK        = WSAENOTSOCK;
  Id_WSAEDESTADDRREQ    = WSAEDESTADDRREQ;
  Id_WSAEMSGSIZE        = WSAEMSGSIZE;
  Id_WSAEPROTOTYPE      = WSAEPROTOTYPE;
  Id_WSAENOPROTOOPT     = WSAENOPROTOOPT;
  Id_WSAEPROTONOSUPPORT = WSAEPROTONOSUPPORT;
  Id_WSAESOCKTNOSUPPORT = WSAESOCKTNOSUPPORT;
  Id_WSAEOPNOTSUPP      = WSAEOPNOTSUPP;
  Id_WSAEPFNOSUPPORT    = WSAEPFNOSUPPORT;
  Id_WSAEAFNOSUPPORT    = WSAEAFNOSUPPORT;
  Id_WSAEADDRINUSE      = WSAEADDRINUSE;
  Id_WSAEADDRNOTAVAIL   = WSAEADDRNOTAVAIL;
  Id_WSAENETDOWN        = WSAENETDOWN;
  Id_WSAENETUNREACH     = WSAENETUNREACH;
  Id_WSAENETRESET       = WSAENETRESET;
  Id_WSAECONNABORTED    = WSAECONNABORTED;
  Id_WSAECONNRESET      = WSAECONNRESET;
  Id_WSAENOBUFS         = WSAENOBUFS;
  Id_WSAEISCONN         = WSAEISCONN;
  Id_WSAENOTCONN        = WSAENOTCONN;
  Id_WSAESHUTDOWN       = WSAESHUTDOWN;
  Id_WSAETOOMANYREFS    = WSAETOOMANYREFS;
  Id_WSAETIMEDOUT       = WSAETIMEDOUT;
  Id_WSAECONNREFUSED    = WSAECONNREFUSED;
  Id_WSAELOOP           = WSAELOOP;
  Id_WSAENAMETOOLONG    = WSAENAMETOOLONG;
  Id_WSAEHOSTDOWN       = WSAEHOSTDOWN;
  Id_WSAEHOSTUNREACH    = WSAEHOSTUNREACH;
  Id_WSAENOTEMPTY       = WSAENOTEMPTY;
  {$ENDIF}

  {$IFDEF DOTNET}
  //In DotNET, the constants are the same as in Winsock2.

  //Ripped from IdWinsock2 - don't use that in DotNET.

  WSABASEERR              = 10000;

  // Windows Sockets definitions of regular Microsoft C error constants
  WSAEINTR                = WSABASEERR + 4;
  WSAEBADF                = WSABASEERR + 9;
  WSAEACCES               = WSABASEERR + 13;
  WSAEFAULT               = WSABASEERR + 14;
  WSAEINVAL               = WSABASEERR + 22;
  WSAEMFILE               = WSABASEERR + 24;

  // Windows Sockets definitions of regular Berkeley error constants
  WSAEWOULDBLOCK          = WSABASEERR + 35;
  WSAEINPROGRESS          = WSABASEERR + 36;
  WSAEALREADY             = WSABASEERR + 37;
  WSAENOTSOCK             = WSABASEERR + 38;
  WSAEDESTADDRREQ         = WSABASEERR + 39;
  WSAEMSGSIZE             = WSABASEERR + 40;
  WSAEPROTOTYPE           = WSABASEERR + 41;
  WSAENOPROTOOPT          = WSABASEERR + 42;
  WSAEPROTONOSUPPORT      = WSABASEERR + 43;
  WSAESOCKTNOSUPPORT      = WSABASEERR + 44;
  WSAEOPNOTSUPP           = WSABASEERR + 45;
  WSAEPFNOSUPPORT         = WSABASEERR + 46;
  WSAEAFNOSUPPORT         = WSABASEERR + 47;
  WSAEADDRINUSE           = WSABASEERR + 48;
  WSAEADDRNOTAVAIL        = WSABASEERR + 49;
  WSAENETDOWN             = WSABASEERR + 50;
  WSAENETUNREACH          = WSABASEERR + 51;
  WSAENETRESET            = WSABASEERR + 52;
  WSAECONNABORTED         = WSABASEERR + 53;
  WSAECONNRESET           = WSABASEERR + 54;
  WSAENOBUFS              = WSABASEERR + 55;
  WSAEISCONN              = WSABASEERR + 56;
  WSAENOTCONN             = WSABASEERR + 57;
  WSAESHUTDOWN            = WSABASEERR + 58;
  WSAETOOMANYREFS         = WSABASEERR + 59;
  WSAETIMEDOUT            = WSABASEERR + 60;
  WSAECONNREFUSED         = WSABASEERR + 61;
  WSAELOOP                = WSABASEERR + 62;
  WSAENAMETOOLONG         = WSABASEERR + 63;
  WSAEHOSTDOWN            = WSABASEERR + 64;
  WSAEHOSTUNREACH         = WSABASEERR + 65;
  WSAENOTEMPTY            = WSABASEERR + 66;
  WSAEPROCLIM             = WSABASEERR + 67;
  WSAEUSERS               = WSABASEERR + 68;
  WSAEDQUOT               = WSABASEERR + 69;
  WSAESTALE               = WSABASEERR + 70;
  WSAEREMOTE              = WSABASEERR + 71;

  // Extended Windows Sockets error constant definitions
  WSASYSNOTREADY          = WSABASEERR + 91;
  WSAVERNOTSUPPORTED      = WSABASEERR + 92;
  WSANOTINITIALIZED       = WSABASEERR + 93;
  WSAEDISCON              = WSABASEERR + 101;
  WSAENOMORE              = WSABASEERR + 102;
  WSAECANCELLED           = WSABASEERR + 103;
  WSAEINVALIDPROCTABLE    = WSABASEERR + 104;
  WSAEINVALIDPROVIDER     = WSABASEERR + 105;
  WSAEPROVIDERFAILEDINIT  = WSABASEERR + 106;
  WSASYSCALLFAILURE       = WSABASEERR + 107;
  WSASERVICE_NOT_FOUND    = WSABASEERR + 108;
  WSATYPE_NOT_FOUND       = WSABASEERR + 109;
  WSA_E_NO_MORE           = WSABASEERR + 110;
  WSA_E_CANCELLED         = WSABASEERR + 111;
  WSAEREFUSED             = WSABASEERR + 112;

  { Error return codes from gethostbyname() and gethostbyaddr()
  when using the resolver. Note that these errors are retrieved
  via WSAGetLastError() and must therefore follow the rules for
  avoiding clashes with error numbers from specific implementations
  or language run-time systems. For this reason the codes are based
  at WSABASEERR+1001. Note also that [WSA]NO_ADDRESS is defined
  only for compatibility purposes. }

  // Authoritative Answer: Host not found
  WSAHOST_NOT_FOUND        = WSABASEERR + 1001;
  HOST_NOT_FOUND           = WSAHOST_NOT_FOUND;

  // Non-Authoritative: Host not found, or SERVERFAIL
  WSATRY_AGAIN             = WSABASEERR + 1002;
  TRY_AGAIN                = WSATRY_AGAIN;

  // Non recoverable errors, FORMERR, REFUSED, NOTIMP
  WSANO_RECOVERY           = WSABASEERR + 1003;
  NO_RECOVERY              = WSANO_RECOVERY;

  // Valid name, no data record of requested type
  WSANO_DATA               = WSABASEERR + 1004;
  NO_DATA                  = WSANO_DATA;

  // no address, look for MX record
  WSANO_ADDRESS            = WSANO_DATA;
  NO_ADDRESS               = WSANO_ADDRESS;

  // Define QOS related error return codes
  WSA_QOS_RECEIVERS          = WSABASEERR + 1005; // at least one reserve has arrived
  WSA_QOS_SENDERS            = WSABASEERR + 1006; // at least one path has arrived
  WSA_QOS_NO_SENDERS         = WSABASEERR + 1007; // there are no senders
  WSA_QOS_NO_RECEIVERS       = WSABASEERR + 1008; // there are no receivers
  WSA_QOS_REQUEST_CONFIRMED  = WSABASEERR + 1009; // reserve has been confirmed
  WSA_QOS_ADMISSION_FAILURE  = WSABASEERR + 1010; // error due to lack of resources
  WSA_QOS_POLICY_FAILURE     = WSABASEERR + 1011; // rejected for administrative reasons - bad credentials
  WSA_QOS_BAD_STYLE          = WSABASEERR + 1012; // unknown or conflicting style
  WSA_QOS_BAD_OBJECT         = WSABASEERR + 1013; // problem with some part of the filterspec or providerspecific buffer in general
  WSA_QOS_TRAFFIC_CTRL_ERROR = WSABASEERR + 1014; // problem with some part of the flowspec
  WSA_QOS_GENERIC_ERROR      = WSABASEERR + 1015; // general error
  WSA_QOS_ESERVICETYPE       = WSABASEERR + 1016; // invalid service type in flowspec
  WSA_QOS_EFLOWSPEC          = WSABASEERR + 1017; // invalid flowspec
  WSA_QOS_EPROVSPECBUF       = WSABASEERR + 1018; // invalid provider specific buffer
  WSA_QOS_EFILTERSTYLE       = WSABASEERR + 1019; // invalid filter style
  WSA_QOS_EFILTERTYPE        = WSABASEERR + 1020; // invalid filter type
  WSA_QOS_EFILTERCOUNT       = WSABASEERR + 1021; // incorrect number of filters
  WSA_QOS_EOBJLENGTH         = WSABASEERR + 1022; // invalid object length
  WSA_QOS_EFLOWCOUNT         = WSABASEERR + 1023; // incorrect number of flows
  WSA_QOS_EUNKNOWNSOBJ       = WSABASEERR + 1024; // unknown object in provider specific buffer
  WSA_QOS_EPOLICYOBJ         = WSABASEERR + 1025; // invalid policy object in provider specific buffer
  WSA_QOS_EFLOWDESC          = WSABASEERR + 1026; // invalid flow descriptor in the list
  WSA_QOS_EPSFLOWSPEC        = WSABASEERR + 1027; // inconsistent flow spec in provider specific buffer
  WSA_QOS_EPSFILTERSPEC      = WSABASEERR + 1028; // invalid filter spec in provider specific buffer
  WSA_QOS_ESDMODEOBJ         = WSABASEERR + 1029; // invalid shape discard mode object in provider specific buffer
  WSA_QOS_ESHAPERATEOBJ      = WSABASEERR + 1030; // invalid shaping rate object in provider specific buffer
  WSA_QOS_RESERVED_PETYPE    = WSABASEERR + 1031; // reserved policy element in provider specific buffer

  {This section defines error constants used in Winsock 2 indirectly.  These
  are from Borland's header.}
  { The handle is invalid. }
  ERROR_INVALID_HANDLE = 6;

  { Not enough storage is available to process this command. }
  ERROR_NOT_ENOUGH_MEMORY = 8;   { dderror }

  { The parameter is incorrect. }
  ERROR_INVALID_PARAMETER = 87;   { dderror }

  { The I/O operation has been aborted because of either a thread exit }
  { or an application request. }
  ERROR_OPERATION_ABORTED = 995;

  { Overlapped I/O event is not in a signalled state. }
  ERROR_IO_INCOMPLETE = 996;

  { Overlapped I/O operation is in progress. }
  ERROR_IO_PENDING = 997;   { dderror }

  { WinSock 2 extension -- new error codes and type definition }
  WSA_IO_PENDING          = ERROR_IO_PENDING;
  WSA_IO_INCOMPLETE       = ERROR_IO_INCOMPLETE;
  WSA_INVALID_HANDLE      = ERROR_INVALID_HANDLE;
  WSA_INVALID_PARAMETER   = ERROR_INVALID_PARAMETER;
  WSA_NOT_ENOUGH_MEMORY   = ERROR_NOT_ENOUGH_MEMORY;
  WSA_OPERATION_ABORTED   = ERROR_OPERATION_ABORTED;

  //TODO: Map these to .net constants. Unfortunately .net does not seem to
  //define these anywhere.

  Id_WSAEINTR           = WSAEINTR;
  Id_WSAEBADF           = WSAEBADF;
  Id_WSAEACCES          = WSAEACCES;
  Id_WSAEFAULT          = WSAEFAULT;
  Id_WSAEINVAL          = WSAEINVAL;
  Id_WSAEMFILE          = WSAEMFILE;
  Id_WSAEWOULDBLOCK     = WSAEWOULDBLOCK;
  Id_WSAEINPROGRESS     = WSAEINPROGRESS;
  Id_WSAEALREADY        = WSAEALREADY;
  Id_WSAENOTSOCK        = WSAENOTSOCK;
  Id_WSAEDESTADDRREQ    = WSAEDESTADDRREQ;
  Id_WSAEMSGSIZE        = WSAEMSGSIZE;
  Id_WSAEPROTOTYPE      = WSAEPROTOTYPE;
  Id_WSAENOPROTOOPT     = WSAENOPROTOOPT;
  Id_WSAEPROTONOSUPPORT = WSAEPROTONOSUPPORT;
  Id_WSAESOCKTNOSUPPORT = WSAESOCKTNOSUPPORT;
  Id_WSAEOPNOTSUPP      = WSAEOPNOTSUPP;
  Id_WSAEPFNOSUPPORT    = WSAEPFNOSUPPORT;
  Id_WSAEAFNOSUPPORT    = WSAEAFNOSUPPORT;
  Id_WSAEADDRINUSE      = WSAEADDRINUSE;
  Id_WSAEADDRNOTAVAIL   = WSAEADDRNOTAVAIL;
  Id_WSAENETDOWN        = WSAENETDOWN;
  Id_WSAENETUNREACH     = WSAENETUNREACH;
  Id_WSAENETRESET       = WSAENETRESET;
  Id_WSAECONNABORTED    = WSAECONNABORTED;
  Id_WSAECONNRESET      = WSAECONNRESET;
  Id_WSAENOBUFS         = WSAENOBUFS;
  Id_WSAEISCONN         = WSAEISCONN;
  Id_WSAENOTCONN        = WSAENOTCONN;
  Id_WSAESHUTDOWN       = WSAESHUTDOWN;
  Id_WSAETOOMANYREFS    = WSAETOOMANYREFS;
  Id_WSAETIMEDOUT       = WSAETIMEDOUT;
  Id_WSAECONNREFUSED    = WSAECONNREFUSED;
  Id_WSAELOOP           = WSAELOOP;
  Id_WSAENAMETOOLONG    = WSAENAMETOOLONG;
  Id_WSAEHOSTDOWN       = WSAEHOSTDOWN;
  Id_WSAEHOSTUNREACH    = WSAEHOSTUNREACH;
  Id_WSAENOTEMPTY       = WSAENOTEMPTY;

  Id_SD_Recv = SocketShutdown.Receive;
  Id_SD_Send = SocketShutdown.Send;
  Id_SD_Both = SocketShutdown.Both;
  {$ENDIF}
                       
implementation

{$IFDEF USE_VCL_POSIX}
  {$I IdSymbolPlatformOn.inc}
{$ENDIF}
end.

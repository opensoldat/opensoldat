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
  Rev 1.3    2/8/2004 12:59:40 PM  JPMugaas
  Start on DotNET port.

  Rev 1.2    10/16/2003 11:05:38 PM  SPerry
  Reorganization

  Rev 1.1    2003.09.30 1:23:02 PM  czhower
  Stack split for DotNet

  Rev 1.0    11/13/2002 08:45:44 AM  JPMugaas
}

unit IdRawHeaders;

interface

{$I IdCompilerDefines.inc}

uses
  {$IFDEF DOTNET}
  System.Net,
  {$ENDIF}
  IdGlobal,
  IdStruct;
// TODO: research subtypes of ICMP header

type
  //RFC 3542 definitions
  //IPv6 Extension Headers
  // types redeclared to avoid dependencies on stack declarations

  TIdSunB = class(TIdStruct)
  protected
    Fs_b1,
    Fs_b2,
    Fs_b3,
    Fs_b4: UInt8;
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property s_b1 : UInt8 read Fs_b1 write Fs_b1;
    property s_b2 : UInt8 read Fs_b2 write Fs_b2;
    property s_b3 : UInt8 read Fs_b3 write Fs_b3;
    property s_b4 : UInt8 read Fs_b4 write Fs_b4;
  end;

  TIdSunW = class(TIdStruct)
  protected
    Fs_w1, Fs_w2: UInt16;
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property s_w1 : UInt16 read Fs_w1 write Fs_w1;
    property s_w2 : UInt16 read Fs_w2 write Fs_w2;
  end;

  TIdInAddr = class(TIdLongWord)
  public
    procedure CopyFrom(const ASource : TIdInAddr);
  end;
{ PIdInAddr = ^TIdInAddr;
  TIdInAddr = record
    case integer of
      0: (S_un_b: TIdSunB);
      1: (S_un_w: TIdSunW);
      2: (S_addr: UInt32);
  end;    }

  TIdNetTime = UInt32;                    // network byte order

const
//header sizes----------------------------------------------------------------//
  Id_ARP_HSIZE            = $1C;      // ARP header:             28 bytes
  Id_DNS_HSIZE            = $0C;      // DNS header base:        12 bytes
  Id_ETH_HSIZE            = $0E;      // Etherner header:        14 bytes
  Id_ICMP_HSIZE           = $04;      // ICMP header base:        4 bytes
  Id_ICMP_ECHO_HSIZE      = $08;      // ICMP_ECHO header:      8 bytes
  Id_ICMP6_ECHO_HSIZE     = $08;      // ICMPv6_ECHO header:    8 bytes icmp echo header len excluding time */
  Id_ICMP_MASK_HSIZE      = $0C;      // ICMP_MASK header:       12 bytes
  Id_ICMP_UNREACH_HSIZE   = $08;      // ICMP_UNREACH header:     8 bytes
  Id_ICMP_TIMEXCEED_HSIZE = $08;      // ICMP_TIMXCEED header:    8 bytes
  Id_ICMP_REDIRECT_HSIZE  = $08;      // ICMP_REDIRECT header:    8 bytes
  Id_ICMP_TS_HSIZE        = $14;      // ICMP_TIMESTAMP header:  20 bytes
  Id_IGMP_HSIZE           = $08;      // IGMP header:             8 bytes
  Id_IP_HSIZE             = $14;      // IP header:              20 bytes
  Id_IPv6_HSIZE           = $28;      // IPv6 header
  Id_RIP_HSIZE            = $18;      // RIP header base:        24 bytes
  Id_TCP_HSIZE            = $14;      // TCP header:             20 bytes
  Id_UDP_HSIZE            = $08;      // UDP header:              8 bytes

//fragmentation flags---------------------------------------------------------//
  Id_IP_RF                = $8000;    // reserved fragment flag
  Id_IP_DF                = $4000;    // dont fragment flag
  Id_IP_MF                = $2000;    // more fragments flag
  Id_IP_OFFMASK           = $1FFF;    // mask for fragmenting bits

//TCP control flags-----------------------------------------------------------//
  Id_TCP_FIN              = $01;
  Id_TCP_SYN              = $02;
  Id_TCP_RST              = $04;
  Id_TCP_PUSH             = $08;
  Id_TCP_ACK              = $10;
  Id_TCP_URG              = $20;

//ICMP types------------------------------------------------------------------//
  Id_ICMP_ECHOREPLY       = 0;
  Id_ICMP_UNREACH         = 3;
  Id_ICMP_SOURCEQUENCH    = 4;
  Id_ICMP_REDIRECT        = 5;
  Id_ICMP_ECHO            = 8;
  Id_ICMP_ROUTERADVERT    = 9;
  Id_ICMP_ROUTERSOLICIT   = 10;
  Id_ICMP_TIMXCEED        = 11;
  Id_ICMP_PARAMPROB       = 12;
  Id_ICMP_TSTAMP          = 13;
  Id_ICMP_TSTAMPREPLY     = 14;
  Id_ICMP_IREQ            = 15;
  Id_ICMP_IREQREPLY       = 16;
  Id_ICMP_MASKREQ         = 17;
  Id_ICMP_MASKREPLY       = 18;
  Id_ICMP_TRACEROUTE      = 30; // RFC1393 Traceroute
  Id_ICMP_DATAGRAM_CONV   = 31; // RFC1475
  Id_ICMP_MOB_HOST_REDIR  = 32; // Mobile Host Redirect
  Id_ICMP_IPv6_WHERE_ARE_YOU = 33;
  Id_ICMP_IPv6_I_AM_HERE  = 34;
  Id_ICMP_MOB_REG_REQ     = 35;
  Id_ICMP_MOB_REG_REPLY   = 36;
  Id_ICMP_SKIP            = 39;
  Id_ICMP_PHOTURIS        = 40; // Photuris  [RFC2521]

//ICMP codes------------------------------------------------------------------//
  Id_ICMP_UNREACH_NET               = 0;
  Id_ICMP_UNREACH_HOST              = 1;
  Id_ICMP_UNREACH_PROTOCOL          = 2;
  Id_ICMP_UNREACH_PORT              = 3;
  Id_ICMP_UNREACH_NEEDFRAG          = 4;
  Id_ICMP_UNREACH_SRCFAIL           = 5;
  Id_ICMP_UNREACH_NET_UNKNOWN       = 6;
  Id_ICMP_UNREACH_HOST_UNKNOWN      = 7;
  Id_ICMP_UNREACH_ISOLATED          = 8;
  Id_ICMP_UNREACH_NET_PROHIB        = 9;
  Id_ICMP_UNREACH_HOST_PROHIB       = 10;
  Id_ICMP_UNREACH_TOSNET            = 11;
  Id_ICMP_UNREACH_TOSHOST           = 12;
  Id_ICMP_UNREACH_FILTER_PROHIB     = 13;
  Id_ICMP_UNREACH_HOST_PRECEDENCE   = 14;
  Id_ICMP_UNREACH_PRECEDENCE_CUTOFF = 15;
  Id_ICMP_REDIRECT_NET              = 0;
  Id_ICMP_REDIRECT_HOST             = 1;
  Id_ICMP_REDIRECT_TOSNET           = 2;
  Id_ICMP_REDIRECT_TOSHOST          = 3;
  Id_ICMP_TIMXCEED_INTRANS          = 0;
  Id_ICMP_TIMXCEED_REASS            = 1;
  Id_ICMP_PARAMPROB_OPTABSENT       = 1;

  // RFC 1393
  Id_ICMP_TRACEROUTE_PACKET_FORWARDED = 0;
  Id_ICMP_TRACEROUTE_NO_ROUTE       = 1;

  Id_ICMP_BAD_SPI                   = 0; //security parameter error 40
  Id_ICMP_AUTH_FAILED               = 1;
  Id_ICMP_DECOMPRESS_FAILED         = 2;
  Id_ICMP_DECRYPTION_FAILED         = 3;
  Id_ICMP_NEED_AUTHENTICATION       = 4;
  Id_ICMP_NEED_AUTHORIZATION        = 5;

  // RFC 1475 error codes
  // The type for Conversion Failed is 31
  Id_ICMP_CONV_UNSPEC               = 0;
  Id_ICMP_CONV_DONTCONV_OPTION      = 1;
  Id_ICMP_CONV_UNKNOWN_MAN_OPTION   = 2;
  Id_ICMP_CONV_UNKNWON_UNSEP_OPTION = 3;
  Id_ICMP_CONV_UNSEP_TRANSPORT      = 4;
  Id_ICMP_CONV_OVERALL_LENGTH_EXCEEDED = 5;
  Id_ICMP_CONV_IP_HEADER_LEN_EXCEEDED  = 6;
  Id_ICMP_CONV_TRANS_PROT_255       = 7; // transport protocol > 255
  Id_ICMP_CONV_PORT_OUT_OF_RANGE    = 8;
  Id_ICMP_CONV_TRANS_HEADER_LEN_EXCEEDED = 9;
  Id_ICMP_CONV_32BIT_ROLLOVER_AND_ACK    = 10;  // 32 Bit Rollover missing and ACK set
  Id_ICMP_CONV_UNKNOWN_MAN_TRANS_OPTION  = 11;

  ICMP_MIN                          = 8;

//ICMPv6 types----------------------------------------------------------------//
  ICMP6_DST_UNREACH            = 1;
  ICMP6_PACKET_TOO_BIG         = 2;
  ICMP6_TIME_EXCEEDED          = 3;
  ICMP6_PARAM_PROB             = 4;

  // Informational Messages
  ICMP6_INFOMSG_MASK          = $80;    //* all informational messages */
  ICMP6_ECHO_REQUEST          = 128;
  ICMP6_ECHO_REPLY            = 129;

  ICMP6_MEMBERSHIP_QUERY     = 130;
  ICMP6_MEMBERSHIP_REPORT    = 131;
  ICMP6_MEMBERSHIP_REDUCTION = 132;

//ICMPv6 codes----------------------------------------------------------------//
  ICMP6_DST_UNREACH_NOROUTE     = 0; //* no route to destination */
  ICMP6_DST_UNREACH_ADMIN       = 1; //* communication with */
                                          //* destination */
                                          //* administratively */
                                          //* prohibited */
  ICMP6_DST_UNREACH_NOTNEIGHBOR = 2; //* not a neighbor */
  ICMP6_DST_UNREACH_ADDR        = 3; //* address unreachable */
  ICMP6_DST_UNREACH_NOPORT      = 4; //* bad port */
  ICMP6_DST_UNREACH_SOURCE_FILTERING = 5;  //source address failed ingress/egress policy
  ICMP6_DST_UNREACH_REJCT_DST   = 6; //reject route to destination

  ICMP6_TIME_EXCEED_TRANSIT     = 0; //* Hop Limit == 0 in transit */
  ICMP6_TIME_EXCEED_REASSEMBLY  = 1; //* Reassembly time out */

  ICMP6_PARAMPROB_HEADER        = 0; //* erroneous header field */
  ICMP6_PARAMPROB_NEXTHEADER    = 1; //* unrecognized Next Header */
  ICMP6_PARAMPROB_OPTION        = 2; //* unrecognized IPv6 option */

  // ICMPv6 Neighbor Discovery Definitions
  ND_ROUTER_SOLICIT          = 133;
  ND_ROUTER_ADVERT           = 134;
  ND_NEIGHBOR_SOLICIT        = 135;
  ND_NEIGHBOR_ADVERT         = 136;
  ND_REDIRECT                = 137;

//IGMP types------------------------------------------------------------------//
  Id_IGMP_MEMBERSHIP_QUERY          = $11;    // membership query
  Id_IGMP_V1_MEMBERSHIP_REPORT      = $12;    // v1 membership report
  Id_IGMP_V2_MEMBERSHIP_REPORT      = $16;    // v2 membership report
  Id_IGMP_LEAVE_GROUP               = $17;    // leave-group message

//ethernet packet types-------------------------------------------------------//
  Id_ETHERTYPE_PUP                  = $0200;    // PUP protocol
  Id_ETHERTYPE_IP                   = $0800;    // IP protocol
  Id_ETHERTYPE_ARP                  = $0806;    // ARP protocol
  Id_ETHERTYPE_REVARP               = $8035;    // reverse ARP protocol
  Id_ETHERTYPE_VLAN                 = $8100;    // IEEE 802.1Q VLAN tagging
  Id_ETHERTYPE_LOOPBACK             = $9000;    // used to test interfaces

//hardware address formats----------------------------------------------------//
  Id_ARPHRD_ETHER                   = 1;        // ethernet hardware format

//ARP operation types---------------------------------------------------------//
  Id_ARPOP_REQUEST                  = 1;        // req to resolve address
  Id_ARPOP_REPLY                    = 2;        // resp to previous request
  Id_ARPOP_REVREQUEST               = 3;        // req protocol address given hardware
  Id_ARPOP_REVREPLY                 = 4;        // resp giving protocol address
  Id_ARPOP_INVREQUEST               = 8;        // req to identify peer
  Id_ARPOP_INVREPLY                 = 9;        // resp identifying peer

//RIP commands----------------------------------------------------------------//
  Id_RIPCMD_REQUEST                 = 1;        // want info
  Id_RIPCMD_RESPONSE                = 2;        // responding to request
  Id_RIPCMD_TRACEON                 = 3;        // turn tracing on
  Id_RIPCMD_TRACEOFF                = 4;        // turn it off
  Id_RIPCMD_POLL                    = 5;        // like request, but anyone answers
  Id_RIPCMD_POLLENTRY               = 6;        // like poll, but for entire entry
  Id_RIPCMD_MAX                     = 7;

//RIP versions----------------------------------------------------------------//
  Id_RIPVER_0                       = 0;
  Id_RIPVER_1                       = 1;
  Id_RIPVER_2                       = 2;

//----------------------------------------------------------------------------//
  Id_MAX_IPOPTLEN                   = 40;
  Id_IP_MAXPACKET                   = 65535;
  Id_ETHER_ADDR_LEN                 = 6;

type

////////////////////////////////////////////////////////////////////////////////
//ICMP//////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  TIdICMPEcho = class(TIdStruct)
  protected
    Fid: UInt16;               // identifier to match requests with replies
    Fseq: UInt16;              // sequence number to match requests with replies
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property id: UInt16 read Fid write FId;                 // identifier to match requests with replies
    property seq: UInt16 read Fseq write FSeq;                // sequence number to match requests with replies
  end;

  TIdICMPFrag = class(TIdStruct)
  protected
    Fpad: UInt16;
    Fmtu: UInt16;
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property pad: UInt16 read Fpad write Fpad;
    property mtu: UInt16 read Fmtu write Fmtu;
  end;

  TIdICMPTs = class(TIdStruct)
  protected
    Fotime: TIdNetTime;        // time message was sent, to calc roundtrip time
    Frtime: TIdNetTime;
    Fttime: TIdNetTime;
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property otime: TIdNetTime read Fotime write Fotime;        // time message was sent, to calc roundtrip time
    property rtime: TIdNetTime read Frtime write Frtime;
    property ttime: TIdNetTime read Fttime write Fttime;
  end;

  { packet header }
  TIdicmp_hun = class(TIdUnion)
  protected
    function Getecho_id: UInt16;
    function Getecho_seq: UInt16;
    function Getfrag_mtu: UInt16;
    function Getfrag_pad: UInt16;
    function Getgateway_s_b1: UInt8;
    function Getgateway_s_b2: UInt8;
    function Getgateway_s_b3: UInt8;
    function Getgateway_s_b4: UInt8;
    function Getgateway_s_l: UInt32;
    function Getgateway_s_w1: UInt16;
    function Getgateway_s_w2: UInt16;
    procedure Setecho_id(const Value: UInt16);
    procedure Setecho_seq(const Value: UInt16);
    procedure Setfrag_mtu(const Value: UInt16);
    procedure Setfrag_pad(const Value: UInt16);
    procedure Setgateway_s_b1(const Value: UInt8);
    procedure Setgateway_s_b2(const Value: UInt8);
    procedure Setgateway_s_b3(const Value: UInt8);
    procedure Setgateway_s_b4(const Value: UInt8);
    procedure Setgateway_s_l(const Value: UInt32);
    procedure Setgateway_s_w1(const Value: UInt16);
    procedure Setgateway_s_w2(const Value: UInt16);
  public
    constructor Create; override;
    property echo_id: UInt16 read Getecho_id write Setecho_id;                 // identifier to match requests with replies
    property echo_seq: UInt16 read Getecho_seq write Setecho_seq;
    property gateway_s_b1 : UInt8 read Getgateway_s_b1 write Setgateway_s_b1;
    property gateway_s_b2 : UInt8 read Getgateway_s_b2 write Setgateway_s_b2;
    property gateway_s_b3 : UInt8 read Getgateway_s_b3 write Setgateway_s_b3;
    property gateway_s_b4 : UInt8 read Getgateway_s_b4 write Setgateway_s_b4;
    property gateway_s_w1 : UInt16 read Getgateway_s_w1 write Setgateway_s_w1;
    property gateway_s_w2 : UInt16 read Getgateway_s_w2 write Setgateway_s_w2;
    property gateway_s_l  : UInt32 read Getgateway_s_l write Setgateway_s_l;
    property frag_pad: UInt16 read Getfrag_pad write Setfrag_pad;
    property frag_mtu: UInt16 read Getfrag_mtu write Setfrag_mtu;
  end;

  TIdicmp_dun = class(TIdUnion)
  protected
    function Getdata: UInt8;
    function Getmask: UInt32;
    procedure setdata(const Value: UInt8);
    procedure Setmask(const Value: UInt32);
    function Getts_otime: TIdNetTime;
    function Getts_rtime: TIdNetTime;
    function Getts_ttime: TIdNetTime;
    procedure Setts_otime(const Value: TIdNetTime);
    procedure Setts_rtime(const Value: TIdNetTime);
    procedure Setts_ttime(const Value: TIdNetTime);
  public
    constructor Create; override;
    property ts_otime: TIdNetTime read Getts_otime write Setts_otime;        // time message was sent, to calc roundtrip time
    property ts_rtime: TIdNetTime read Getts_rtime write Setts_rtime;
    property ts_ttime: TIdNetTime read Getts_ttime write Setts_ttime;
    property mask : UInt32 read Getmask write Setmask;
    property data : UInt8 read Getdata write setdata;
  end;

  TIdICMPHdr = class(TIdStruct)
  protected
    Ficmp_type: UInt8;          // message type
    Ficmp_code: UInt8;          // error code
    Ficmp_sum: UInt16;           // one's complement checksum    {Do not Localize}
    Ficmp_hun: TIdicmp_hun;
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property icmp_type: UInt8 read Ficmp_type write Ficmp_type;          // message type
    property icmp_code: UInt8 read Ficmp_code write Ficmp_code;          // error code
    property icmp_sum: UInt16 read Ficmp_sum write Ficmp_sum;             // one's complement checksum
    property icmp_hun: TIdicmp_hun read Ficmp_hun;
  end;

  //ICMPv6
  TIdicmp6_un = class(TIdUnion)
  protected
    function Geticmp6_data16: UInt16;
    function Geticmp6_data8: UInt8;
    procedure Seticmp6_data16(const Value: UInt16);
    procedure Seticmp6_data8(const Value: UInt8);
    function Geticmp6_seq: UInt16;
    procedure Seticmp6_seq(const Value: UInt16);
    function Geticmp6_un_data16(Index: Integer): UInt16;
    function Geticmp6_un_data32: UInt32;
    function Geticmp6_un_data8(Index: Integer): UInt8;
    procedure Seticmp6_un_data16(Index: Integer; const Value: UInt16);
    procedure Seticmp6_un_data32(const Value: UInt32);
    procedure Seticmp6_un_data8(Index: Integer; const Value: UInt8);
{
    Ficmp6_un_data32 : UInt32; //* type-specific field */
    Ficmp6_un_data16 : array[0..1] of UInt16; //* type-specific field */
    icmp6_un_data8 : array[0..3] of UInt8);  //* type-specific field */
}
  public
    constructor Create; override;
    property icmp6_un_data32 : UInt32 read Geticmp6_un_data32 write Seticmp6_un_data32; //* type-specific field */
    property icmp6_un_data16[Index:Integer] : UInt16 read Geticmp6_un_data16 write Seticmp6_un_data16; //array 0-1 * type-specific field */
    property icmp6_un_data8[Index:Integer] : UInt8 read Geticmp6_un_data8 write Seticmp6_un_data8;  //array[0-3] * type-specific field */
    property icmp6_data32 : UInt32 read Geticmp6_un_data32 write Seticmp6_un_data32;
    property icmp6_data16 : UInt16 read Geticmp6_data16 write Seticmp6_data16;
    property icmp6_data8  : UInt8 read Geticmp6_data8 write Seticmp6_data8;
    property icmp6_pptr : UInt32 read Geticmp6_un_data32 write Seticmp6_un_data32;
    property icmp6_mtu : UInt32 read Geticmp6_un_data32 write Seticmp6_un_data32;
    property icmp6_id : UInt16 read Geticmp6_data16 write Seticmp6_data16;
    property icmp6_seq : UInt16 read Geticmp6_seq write Seticmp6_seq;
    property icmp6_maxdelay : UInt16 read Geticmp6_data16 write Seticmp6_data16;
  end;

  TIdicmp6_hdr = class(TIdStruct)
  protected
    Ficmp6_type : UInt8;   //* type field */
    FIcmp6_code : UInt8;   //* code field */
    Ficmp6_cksum : UInt16;  //* checksum field */
    Fdata : TIdicmp6_un;
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32);  override;

    property icmp6_type : UInt8 read Ficmp6_type write Ficmp6_type;     //* type field */
    property icmp6_code : UInt8 read Ficmp6_code write Ficmp6_code;     //* code field */
    property icmp6_cksum : UInt16 read Ficmp6_cksum write Ficmp6_cksum; //* checksum field */
    property data : TIdicmp6_un read Fdata;
{        case Integer of
          1: (icmp6_un_data32 : UInt32); //* type-specific field */
          2: (icmp6_un_data16 : array[0..1] of UInt16); //* type-specific field */
          3: (icmp6_un_data8 : array[0..3] of UInt8);  //* type-specific field */
}
   end;
   
////////////////////////////////////////////////////////////////////////////////
//IP////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { options struct }
  TIdIPOptions = class(TIdUnion)
  public
    constructor Create; override;

    //Delphi outputs warnings such as:
    //[Hint] H2368 Visibility of property accessor method TIdIPOptions.get_ipopt_list should match property TIdIPOptions.ipopt_list
    //[Hint] H2368 Visibility of property accessor method TIdIPOptions.set_ipopt_list should match property TIdIPOptions.ipopt_list
    //if these aren't public
    function get_ipopt_list(Index: Integer): UInt8;
    procedure set_ipopt_list(Index: Integer; const Value: UInt8);

    property ipopt_list[Index : Integer] : UInt8 read get_ipopt_list write set_ipopt_list; default; //options proper
  end;

  { packet header }
  TIdIPHdr = class(TIdStruct)
  protected
    Fip_verlen: UInt8;        // 1st nibble version, 2nd nibble header length div 4 (little-endian)
    Fip_tos: UInt8;           // type of service
    Fip_len: UInt16;           // total length
    Fip_id: UInt16;            // identification
    Fip_off: UInt16;           // 1st nibble flags, next 3 nibbles fragment offset (little-endian)
    Fip_ttl: UInt8;           // time to live
    Fip_p: UInt8;             // protocol
    Fip_sum: UInt16;           // checksum
    Fip_src: TIdInAddr;      // source address
    Fip_dst: TIdInAddr;      // dest address
    Fip_options: UInt32;   // options + padding
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    procedure CopyFrom(const ASource : TIdIPHdr);
    property ip_verlen: UInt8 read Fip_verlen write Fip_verlen;  // 1st nibble version, 2nd nibble header length div 4 (little-endian)
    property ip_tos: UInt8 read Fip_tos write Fip_tos;           // type of service
    property ip_len: UInt16 read Fip_len write Fip_len;           // total length
    property ip_id: UInt16 read Fip_id write Fip_id;              // identification
    property ip_off: UInt16 read Fip_off write Fip_off;           // 1st nibble flags, next 3 nibbles fragment offset (little-endian)
    property ip_ttl: UInt8 read Fip_ttl write Fip_ttl;           // time to live
    property ip_p: UInt8 read Fip_p write Fip_p;                 // protocol
    property ip_sum: UInt16 read Fip_sum write Fip_sum;           // checksum
    property ip_src: TIdInAddr read Fip_src;                    // source address
    property ip_dst: TIdInAddr read Fip_dst;                    // dest address
    property ip_options: UInt32 read Fip_options write Fip_options;   // options + padding
  end;

////////////////////////////////////////////////////////////////////////////////
//TCP///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { options structure }
  TIdTCPOptions = class(TIdUnion)
  public
    constructor Create; override;

    //Delphi outputs warnings such as:
    //[Hint] H2368 Visibility of property accessor method TIdTCPOptions.gettcpopt_list should match property TIdTCPOptions.tcpopt_list
    //[Hint] H2368 Visibility of property accessor method TIdIPOptions.settcpopt_list should match property TIdTCPOptions.tcpopt_list
    //if these aren't public
    function gettcpopt_list(Index: Integer): UInt8;
    procedure settcpopt_list(Index: Integer; const Value: UInt8);

    property tcpopt_list[Index : Integer] : UInt8 read gettcpopt_list write settcpopt_list; default;
  end;

  { packet header }
  TIdTCPHdr =  class(TIdStruct)
  protected
    Ftcp_sport: UInt16;        // source port
    Ftcp_dport: UInt16;        // destination port
    Ftcp_seq: UInt32;      // sequence number
    Ftcp_ack: UInt32;      // acknowledgement number
    Ftcp_x2off: UInt8;        // data offset
    Ftcp_flags: UInt8;        // control flags
    Ftcp_win: UInt16;          // window
    Ftcp_sum: UInt16;          // checksum
    Ftcp_urp: UInt16;          // urgent pointer
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property tcp_sport: UInt16 read Ftcp_sport write Ftcp_sport;    // source port
    property tcp_dport: UInt16 read Ftcp_dport write Ftcp_dport;    // destination port
    property tcp_seq: UInt32 read Ftcp_seq write Ftcp_seq;      // sequence number
    property tcp_ack: UInt32 read Ftcp_ack write Ftcp_ack;      // acknowledgement number
    property tcp_x2off: UInt8 read Ftcp_x2off write Ftcp_x2off;    // data offset
    property tcp_flags: UInt8 read Ftcp_flags write Ftcp_flags;    // control flags
    property tcp_win: UInt16 read Ftcp_win write Ftcp_win;          // window
    property tcp_sum: UInt16 read Ftcp_sum write Ftcp_sum;          // checksum
    property tcp_urp: UInt16 read Ftcp_urp write Ftcp_urp;          // urgent pointer
  end;

////////////////////////////////////////////////////////////////////////////////
//UDP///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { packet header }
  TIdUDPHdr = class(TIdStruct)
  protected
    Fudp_sport: UInt16;        // source port
    Fudp_dport: UInt16;        // destination port
    Fudp_ulen: UInt16;         // length
    Fudp_sum: UInt16;          // checksum
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property udp_sport: UInt16 read Fudp_sport write Fudp_sport;    // source port
    property udp_dport: UInt16 read Fudp_dport write Fudp_dport;    // destination port
    property udp_ulen: UInt16 read Fudp_ulen write Fudp_ulen;       // length
    property udp_sum: UInt16 read Fudp_sum write Fudp_sum;          // checksum
  end;

////////////////////////////////////////////////////////////////////////////////
//IGMP//////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { packet header }

  TIdIGMPHdr = class(TIdStruct)
  protected
    Figmp_type: UInt8;
    Figmp_code: UInt8;
    Figmp_sum: UInt16;
    Figmp_group: TIdInAddr;
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property igmp_type: UInt8 read Figmp_type write Figmp_type;
    property igmp_code: UInt8 read Figmp_code write Figmp_code;
    property igmp_sum: UInt16 read Figmp_sum write Figmp_sum;
    property igmp_group: TIdInAddr read Figmp_group;
  end;

////////////////////////////////////////////////////////////////////////////////
//ETHERNET//////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  TIdEtherAddr = class(TIdUnion)
  public
    constructor Create; override;

    procedure CopyFrom(const ASource : TIdEtherAddr);
    procedure SetData(const Value: TIdBytes);

    //Delphi outputs warnings such as:
    //[Hint] H2368 Visibility of property accessor method TIdEtherAddr.getether_addr_octet should match property TIdEtherAddr.ether_addr_octet
    //[Hint] H2368 Visibility of property accessor method TIdEtherAddr.setether_addr_octet should match property TIdEtherAddr.ether_addr_octet
    //if these aren't public
    function getether_addr_octet(Index: Integer): UInt8;
    procedure setether_addr_octet(Index: Integer; const Value: UInt8);

    property ether_addr_octet[Index: Integer] : UInt8 read getether_addr_octet write setether_addr_octet; default;
    property Data: TIdBytes read FBuffer write SetData;
  end;

  { packet header }
  TIdEthernetHdr = class(TIdStruct)
  protected
    Fether_dhost: TIdEtherAddr;            // destination ethernet address
    Fether_shost: TIdEtherAddr;            // source ethernet address
    Fether_type: UInt16;                     // packet type ID
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CopyFrom(const ASource : TIdEthernetHdr);
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property ether_dhost: TIdEtherAddr read Fether_dhost;            // destination ethernet address
    property ether_shost: TIdEtherAddr read Fether_shost;            // source ethernet address
    property ether_type: UInt16 read Fether_type write Fether_type;    // packet type ID
  end;

////////////////////////////////////////////////////////////////////////////////
//ARP///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { packet header }
  TIdARPHdr = class(TIdStruct)
  protected
    Farp_hrd: UInt16;                        // format of hardware address
    Farp_pro: UInt16;                        // format of protocol address
    Farp_hln: UInt8;                        // length of hardware address
    Farp_pln: UInt8;                        // length of protocol addres
    Farp_op: UInt16;                         // operation type
    // following hardcoded for ethernet/IP
    Farp_sha: TIdEtherAddr;                // sender hardware address
    Farp_spa: TIdInAddr;                   // sender protocol address
    Farp_tha: TIdEtherAddr;                // target hardware address
    Farp_tpa: TIdInAddr;                   // target protocol address
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property arp_hrd: UInt16 read Farp_hrd write Farp_hrd;         // format of hardware address
    property arp_pro: UInt16 read Farp_pro write Farp_pro;         // format of protocol address
    property arp_hln: UInt8 read Farp_hln write Farp_hln;         // length of hardware address
    property arp_pln: UInt8 read Farp_pln write Farp_pln;         // length of protocol addres
    property arp_op: UInt16 read Farp_op write Farp_op;            // operation type
    // following hardcoded for ethernet/IP
    property arp_sha: TIdEtherAddr read Farp_sha;                // sender hardware address
    property arp_spa: TIdInAddr read Farp_spa;                   // sender protocol address
    property arp_tha: TIdEtherAddr read Farp_tha;                // target hardware address
    property arp_tpa: TIdInAddr read Farp_tpa;                   // target protocol address
  end;

////////////////////////////////////////////////////////////////////////////////
//DNS///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { header }

  TIdDNSHdr = class(TIdStruct)
  protected
    Fdns_id: UInt16;                         // DNS packet ID
    Fdns_flags: UInt16;                      // DNS flags
    Fdns_num_q: UInt16;                      // number of questions
    Fdns_num_answ_rr: UInt16;                // number of answer resource records
    Fdns_num_auth_rr: UInt16;                // number of authority resource records
    Fdns_num_addi_rr: UInt16;                // number of additional resource records
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property dns_id: UInt16 read Fdns_id write Fdns_id;                            // DNS packet ID
    property dns_flags: UInt16 read Fdns_flags write Fdns_flags;                   // DNS flags
    property dns_num_q: UInt16 read Fdns_num_q write Fdns_num_q;                   // number of questions
    property dns_num_answ_rr: UInt16 read Fdns_num_answ_rr write Fdns_num_answ_rr; // number of answer resource records
    property dns_num_auth_rr: UInt16 read Fdns_num_auth_rr write Fdns_num_auth_rr; // number of authority resource records
    property dns_num_addi_rr: UInt16 read Fdns_num_addi_rr write Fdns_num_addi_rr; // number of additional resource records
  end;

////////////////////////////////////////////////////////////////////////////////
//RIP///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { header }
  TIdRIPHdr = class(TIdStruct)
  protected
    Frip_cmd: UInt8;            // RIP command
    Frip_ver: UInt8;            // RIP version
    Frip_rd: UInt16;             // zero (v1) or routing domain (v2)
    Frip_af: UInt16;             // address family
    Frip_rt: UInt16;             // zero (v1) or route tag (v2)
    Frip_addr: UInt32;       // IP address
    Frip_mask: UInt32;       // zero (v1) or subnet mask (v2)
    Frip_next_hop: UInt32;   // zero (v1) or next hop IP address (v2)
    Frip_metric: UInt32;     // metric
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;

    property rip_cmd: UInt8 read Frip_cmd write Frip_cmd;                    // RIP command
    property rip_ver: UInt8 read Frip_ver write Frip_ver;                    // RIP version
    property rip_rd: UInt16 read Frip_rd write Frip_rd;                       // zero (v1) or routing domain (v2)
    property rip_af: UInt16 read Frip_af write Frip_af;                       // address family
    property rip_rt: UInt16 read Frip_rt write Frip_rt;                       // zero (v1) or route tag (v2)
    property rip_addr: UInt32 read Frip_addr write Frip_addr;             // IP address
    property rip_mask: UInt32 read Frip_mask write Frip_mask;             // zero (v1) or subnet mask (v2)
    property rip_next_hop: UInt32 read Frip_next_hop write Frip_next_hop; // zero (v1) or next hop IP address (v2)
    property rip_metric: UInt32 read Frip_metric write Frip_metric;       // metric
  end;


////////////////////////////////////////////////////////////////////////////////

implementation

uses
  SysUtils;

{ TIdSunB }

function TIdSunB.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4;
end;

procedure TIdSunB.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fs_b1 := ABytes[VIndex];
  Inc(VIndex);
  Fs_b2 := ABytes[VIndex];
  Inc(VIndex);
  Fs_b3 := ABytes[VIndex];
  Inc(VIndex);
  Fs_b4 := ABytes[VIndex];
  Inc(VIndex);
end;

procedure TIdSunB.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  VBytes[VIndex] := Fs_b1;
  Inc(VIndex);
  VBytes[VIndex] := Fs_b2;
  Inc(VIndex);
  VBytes[VIndex] := Fs_b3;
  Inc(VIndex);
  VBytes[VIndex] := Fs_b4;
  Inc(VIndex);
end;

{ TIdSunW }

function TIdSunW.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4;
end;

procedure TIdSunW.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fs_w1 := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
  Fs_w2 := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
end;

procedure TIdSunW.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(HostToLittleEndian(Fs_w1), VBytes, VIndex);
  Inc(VIndex, 2);
  CopyTIdUInt16(HostToLittleEndian(Fs_w2), VBytes, VIndex);
  Inc(VIndex, 2);
end;

{ TIdICMPEcho }

function TIdICMPEcho.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4;
end;

procedure TIdICMPEcho.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fid := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
  seq := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
end;

procedure TIdICMPEcho.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(HostToLittleEndian(Fid), VBytes, VIndex);
  Inc(VIndex, 2);
  CopyTIdUInt16(HostToLittleEndian(seq), VBytes, VIndex);
  Inc(VIndex, 2);
end;

{ TIdICMPFrag }

function TIdICMPFrag.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4;
end;

procedure TIdICMPFrag.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fpad := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
  Fmtu := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
end;

procedure TIdICMPFrag.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(HostToLittleEndian(Fpad), VBytes, VIndex);
  Inc(VIndex, 2);
  CopyTIdUInt16(HostToLittleEndian(Fmtu), VBytes, VIndex);
  Inc(VIndex, 2);
end;

{ TIdICMPTs }

function TIdICMPTs.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 12;
end;

procedure TIdICMPTs.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fotime := BytesToUInt32(ABytes, VIndex);        // time message was sent, to calc roundtrip time
  Inc(VIndex, 4);
  Frtime := BytesToUInt32(ABytes, VIndex);
  Inc(VIndex, 4);
  Fttime := BytesToUInt32(ABytes, VIndex);
  Inc(VIndex, 4);
end;

procedure TIdICMPTs.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(HostToLittleEndian(Fotime), VBytes, VIndex);        // time message was sent, to calc roundtrip time
  Inc(VIndex, 4);
  CopyTIdUInt16(HostToLittleEndian(Frtime), VBytes, VIndex);
  Inc(VIndex, 4);
  CopyTIdUInt16(HostToLittleEndian(Fttime), VBytes, VIndex);
  Inc(VIndex, 4);
end;

{ TIdicmp_hun }

constructor TIdicmp_hun.Create;
begin
  inherited Create;
  SetBytesLen(4);
end;

function TIdicmp_hun.Getecho_id: UInt16;
begin
  Result := Getgateway_s_w1;
end;

procedure TIdicmp_hun.Setecho_id(const Value: UInt16);
begin
  Setgateway_s_w1(Value);
end;

function TIdicmp_hun.Getecho_seq: UInt16;
begin
  Result := Getgateway_s_w2;
end;

procedure TIdicmp_hun.Setecho_seq(const Value: UInt16);
begin
  Setgateway_s_w2(Value);
end;

function TIdicmp_hun.Getgateway_s_w1: UInt16;
begin
  Result := BytesToUInt32(FBuffer, 0);
end;

procedure TIdicmp_hun.Setgateway_s_w1(const Value: UInt16);
begin
  CopyTIdUInt32(Value, FBuffer, 0);
end;

function TIdicmp_hun.Getgateway_s_w2: UInt16;
begin
  Result := BytesToUInt16(FBuffer, 2);
end;

procedure TIdicmp_hun.Setgateway_s_w2(const Value: UInt16);
begin
  CopyTIdUInt16(HostToLittleEndian(Value), FBuffer, 2);
end;

function TIdicmp_hun.Getgateway_s_b1: UInt8;
begin
  Result := FBuffer[0];
end;

procedure TIdicmp_hun.Setgateway_s_b1(const Value: UInt8);
begin
  FBuffer[0] := Value;
end;

function TIdicmp_hun.Getgateway_s_b2: UInt8;
begin
  Result := FBuffer[1];
end;

procedure TIdicmp_hun.Setgateway_s_b2(const Value: UInt8);
begin
  FBuffer[1] := Value;
end;

function TIdicmp_hun.Getgateway_s_b3: UInt8;
begin
  Result := FBuffer[2];
end;

procedure TIdicmp_hun.Setgateway_s_b3(const Value: UInt8);
begin
  FBuffer[2] := Value;
end;

function TIdicmp_hun.Getgateway_s_b4: UInt8;
begin
  Result := FBuffer[3];
end;

procedure TIdicmp_hun.Setgateway_s_b4(const Value: UInt8);
begin
  FBuffer[3] := Value;
end;

function TIdicmp_hun.Getgateway_s_l: UInt32;
begin
  Result := BytesToUInt32(FBuffer, 0);
end;

procedure TIdicmp_hun.Setgateway_s_l(const Value: UInt32);
begin
  CopyTIdUInt32(Value, FBuffer, 0);
end;

function TIdicmp_hun.Getfrag_pad: UInt16;
begin
  Result := Getgateway_s_w1;
end;

procedure TIdicmp_hun.Setfrag_pad(const Value: UInt16);
begin
  Setgateway_s_w1(Value);
end;

function TIdicmp_hun.Getfrag_mtu: UInt16;
begin
  Result := Getgateway_s_w2;
end;

procedure TIdicmp_hun.Setfrag_mtu(const Value: UInt16);
begin
  Setgateway_s_w2(Value);
end;

{ TIdicmp_dun }

constructor TIdicmp_dun.Create;
begin
  inherited Create;
  SetBytesLen(12);
end;

function TIdicmp_dun.Getts_otime: TIdNetTime;
begin
  Result := BytesToUInt32(FBuffer, 0);
end;

procedure TIdicmp_dun.Setts_otime(const Value: TIdNetTime);
begin
  CopyTIdUInt32(Value, FBuffer, 0);
end;

function TIdicmp_dun.Getts_rtime: TIdNetTime;
begin
  Result := BytesToUInt32(FBuffer, 4);
end;

procedure TIdicmp_dun.Setts_rtime(const Value: TIdNetTime);
begin
  CopyTIdUInt32(Value, FBuffer, 4);
end;

function TIdicmp_dun.Getts_ttime: TIdNetTime;
begin
  Result := BytesToUInt32(FBuffer, 4);
end;

procedure TIdicmp_dun.Setts_ttime(const Value: TIdNetTime);
begin
  CopyTIdUInt32(Value, FBuffer, 8);
end;

function TIdicmp_dun.Getmask: UInt32;
begin
  Result := Getts_otime;
end;

procedure TIdicmp_dun.Setmask(const Value: UInt32);
begin
  Setts_otime(Value);
end;

function TIdicmp_dun.Getdata: UInt8;
begin
  Result := FBuffer[0];
end;

procedure TIdicmp_dun.setdata(const Value: UInt8);
begin
  FBuffer[0] := Value;
end;

{ TIdICMPHdr }

constructor TIdICMPHdr.Create;
begin
  inherited Create;
  Ficmp_hun := TIdicmp_hun.Create;
end;

destructor TIdICMPHdr.Destroy;
begin
  FreeAndNil(Ficmp_hun);
  inherited Destroy;
end;

function TIdICMPHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4 + Ficmp_hun.BytesLen;
end;

procedure TIdICMPHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Ficmp_type := ABytes[VIndex];
  Inc(VIndex);
  Ficmp_code := ABytes[Vindex];
  Inc(VIndex);
  Ficmp_sum := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
  Ficmp_hun.ReadStruct(ABytes, VIndex);
end;

procedure TIdICMPHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  VBytes[VIndex] := Ficmp_type;
  Inc(VIndex);
  VBytes[Vindex] := Ficmp_code;
  Inc(VIndex);
  CopyTIdUInt16(Ficmp_sum, VBytes, VIndex);
  Inc(VIndex, 2);
  Ficmp_hun.WriteStruct(VBytes, VIndex);
end;

{ TIdIPOptions }

constructor TIdIPOptions.Create;
begin
  inherited Create;
  SetBytesLen(Id_MAX_IPOPTLEN);
end;

function TIdIPOptions.get_ipopt_list(Index: Integer): UInt8;
begin
  Assert(Index < Id_MAX_IPOPTLEN, 'Out of range'); {do not localize}
  Result := FBuffer[Index];
end;

procedure TIdIPOptions.set_ipopt_list(Index: Integer; const Value: UInt8);
begin
  Assert(Index < Id_MAX_IPOPTLEN, 'Out of range'); {do not localize}
  FBuffer[Index] := Value;
end;

{ TIdIPHdr }

constructor TIdIPHdr.Create;
begin
  inherited Create;
  Fip_src := TIdInAddr.Create;
  Fip_dst := TIdInAddr.Create;
end;

destructor TIdIPHdr.Destroy;
begin
  FreeAndNil(Fip_src);
  FreeAndNil(Fip_dst);
  inherited Destroy;
end;

function TIdIPHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 12 + Fip_src.BytesLen + Fip_dst.BytesLen + 4;
end;

procedure TIdIPHdr.CopyFrom(const ASource: TIdIPHdr);
begin
  Fip_verlen := ASource.ip_verlen;
  Fip_tos := ASource.ip_tos;
  Fip_len := ASource.ip_len;
  Fip_id := ASource.ip_id;
  Fip_off := ASource.ip_off;
  Fip_ttl := ASource.ip_ttl;
  Fip_p := ASource.ip_p;
  Fip_sum := ASource.ip_sum;
  Fip_src.CopyFrom(ASource.ip_src);
  Fip_dst.CopyFrom(ASource.ip_dst);
  Fip_options := ASource.ip_options;
end;

procedure TIdIPHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
var LIpHeaderLen : UInt32;
begin
  inherited ReadStruct(ABytes, VIndex);
  Fip_verlen := ABytes[VIndex];      // 1st nibble version, 2nd nibble header length div 4 (little-endian)
  Inc(VIndex);
  LIpHeaderLen := (Fip_verlen and $0F) * 4;
  Fip_tos := ABytes[VIndex];         // type of service
  Inc(VIndex);
  Fip_len := BytesToUInt16(ABytes, VIndex);     // total length
  Inc(VIndex, 2);
  Fip_id := BytesToUInt16(ABytes, VIndex);          // identification
  Inc(VIndex, 2);
  Fip_off := BytesToUInt16(ABytes, VIndex);          // 1st nibble flags, next 3 nibbles fragment offset (little-endian)
  Inc(VIndex, 2);
  Fip_ttl := ABytes[VIndex];         // time to live
  Inc(VIndex);
  Fip_p := ABytes[VIndex];             // protocol
  Inc(VIndex);
  Fip_sum := BytesToUInt16(ABytes, VIndex);          // checksum
  Inc(VIndex, 2);
  Fip_src.ReadStruct(ABytes, VIndex);    // source address
  Fip_dst.ReadStruct(ABytes, VIndex);      // dest address
  //Fip_options may not be present in the packet
  if VIndex >= LIpHeaderLen then
  begin
    Fip_options := BytesToUInt32(ABytes, VIndex); // options + padding
  end;
  //be sure that we indicate we read the entire packet in case
  //the size varies.
  VIndex :=  LIpHeaderLen;
end;

procedure TIdIPHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);

begin
  inherited WriteStruct(VBytes, VIndex);
  VBytes[VIndex] := Fip_verlen;      // 1st nibble version, 2nd nibble header length div 4 (little-endian)
  Inc(VIndex);
  VBytes[VIndex] := Fip_tos;        // type of service
  Inc(VIndex);
  CopyTIdUInt16(Fip_len, VBytes, VIndex);      // total length
  Inc(VIndex, 2);
  CopyTIdUInt16(Fip_id, VBytes, VIndex);         // identification
  Inc(VIndex, 2);
  CopyTIdUInt16(Fip_off, VBytes, VIndex);           // 1st nibble flags, next 3 nibbles fragment offset (little-endian)
  Inc(VIndex, 2);
  Fip_ttl := VBytes[VIndex];          // time to live
  Inc(VIndex);
  Fip_p := VBytes[VIndex];             // protocol
  Inc(VIndex);
  CopyTIdUInt16(Fip_sum, VBytes, VIndex);           // checksum
  Inc(VIndex, 2);
  Fip_src.WriteStruct(VBytes, VIndex);     // source address
  Fip_dst.WriteStruct(VBytes, VIndex);       // dest address
  CopyTIdUInt32(Fip_options, VBytes, VIndex);  // options + padding
  Inc(VIndex, 4);
end;

{ TIdTCPOptions }

constructor TIdTCPOptions.Create;
begin
  inherited Create;
  SetBytesLen(Id_MAX_IPOPTLEN);
end;

function TIdTCPOptions.gettcpopt_list(Index: Integer): UInt8;
begin
  Assert(Index < Id_MAX_IPOPTLEN, 'Out of range');
  Result := FBuffer[Index];
end;

procedure TIdTCPOptions.settcpopt_list(Index: Integer; const Value: UInt8);
begin
  Assert(Index < Id_MAX_IPOPTLEN, 'Out of range');
  FBuffer[Index] := Value;
end;

{ TIdTCPHdr }

function TIdTCPHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 20;
end;

procedure TIdTCPHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Ftcp_sport := BytesToUInt16(ABytes, VIndex);       // source port
  Inc(VIndex, 2);
  Ftcp_dport := BytesToUInt16(ABytes, VIndex);        // destination port
  Inc(VIndex, 2);
  Ftcp_seq := BytesToUInt32(ABytes, VIndex);      // sequence number
  Inc(VIndex, 4);
  Ftcp_ack := BytesToUInt32(ABytes, VIndex);       // acknowledgement number
  Inc(VIndex, 4);
  Ftcp_x2off := ABytes[VIndex];        // data offset
  Inc(VIndex);
  Ftcp_flags := ABytes[VIndex];        // control flags
  Inc(VIndex);
  Ftcp_win := BytesToUInt16(ABytes, VIndex);          // window
  Inc(VIndex, 2);
  Ftcp_sum := BytesToUInt16(ABytes, VIndex);          // checksum
  Inc(VIndex, 2);
  Ftcp_urp := BytesToUInt16(ABytes, VIndex);          // urgent pointer
  Inc(VIndex, 2);
end;

procedure TIdTCPHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(Ftcp_sport, VBytes, VIndex);        // source port
  Inc(VIndex, 2);
  CopyTIdUInt16(Ftcp_dport, VBytes, VIndex);        // destination port
  Inc(VIndex, 2);
  CopyTIdUInt32(Ftcp_seq, VBytes, VIndex);       // sequence number
  Inc(VIndex, 4);
  CopyTIdUInt32(Ftcp_ack, VBytes, VIndex);      // acknowledgement number
  Inc(VIndex, 4);
  VBytes[VIndex] := Ftcp_x2off;         // data offset
  Inc(VIndex);
  VBytes[VIndex] := Ftcp_flags;        // control flags
  Inc(VIndex);
  CopyTIdUInt16(Ftcp_win, VBytes, VIndex);          // window
  Inc(VIndex, 2);
  CopyTIdUInt16(Ftcp_sum, VBytes, VIndex);           // checksum
  Inc(VIndex, 2);
  CopyTIdUInt16(Ftcp_urp, VBytes, VIndex);           // urgent pointer
  Inc(VIndex, 2);
end;

{ TIdUDPHdr }

function TIdUDPHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 8;
end;

procedure TIdUDPHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fudp_sport := BytesToUInt16(ABytes, VIndex);        // source port
  Inc(VIndex, 2);
  Fudp_dport := BytesToUInt16(ABytes, VIndex);        // destination port
  Inc(VIndex, 2);
  Fudp_ulen := BytesToUInt16(ABytes, VIndex);        // length
  Inc(VIndex, 2);
  Fudp_sum := BytesToUInt16(ABytes, VIndex);         // checksum
  Inc(VIndex, 2);
end;

procedure TIdUDPHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(Fudp_sport, VBytes, VIndex);        // source port
  Inc(VIndex, 2);
  CopyTIdUInt16(Fudp_dport, VBytes, VIndex);         // destination port
  Inc(VIndex, 2);
  CopyTIdUInt16(Fudp_ulen, VBytes, VIndex);          // length
  Inc(VIndex, 2);
  CopyTIdUInt16(Fudp_sum, VBytes, VIndex);           // checksum
  Inc(VIndex, 2);
end;

{ TIdIGMPHdr }

constructor TIdIGMPHdr.Create;
begin
  inherited Create;
  Figmp_group := TIdInAddr.Create;
end;

destructor TIdIGMPHdr.Destroy;
begin
  FreeAndNil(Figmp_group);
  inherited Destroy;
end;

function TIdIGMPHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4 + Figmp_group.BytesLen;
end;

procedure TIdIGMPHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Figmp_type := ABytes[VIndex];
  Inc(VIndex);
  Figmp_code := ABytes[VIndex];
  Inc(VIndex);
  Figmp_sum := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
  Figmp_group.ReadStruct(ABytes, VIndex);
end;

procedure TIdIGMPHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  VBytes[VIndex] := Figmp_type;
  Inc(VIndex);
  VBytes[VIndex] := Figmp_code;
  Inc(VIndex);
  CopyTIdUInt16(Figmp_sum, VBytes, VIndex);
  Inc(VIndex, 2);
  Figmp_group.WriteStruct(VBytes, VIndex);
end;

{ TIdEtherAddr }

constructor TIdEtherAddr.Create;
begin
  inherited Create;
  SetBytesLen(Id_ETHER_ADDR_LEN);
end;

procedure TIdEtherAddr.setether_addr_octet(Index: Integer; const Value: UInt8);
begin
  Assert(Index < Id_ETHER_ADDR_LEN, 'Out of range');
  FBuffer[Index] := Value;
end;

function TIdEtherAddr.getether_addr_octet(Index: Integer): UInt8;
begin
  Assert(Index < Id_ETHER_ADDR_LEN, 'Out of range');
  Result := FBuffer[Index];
end;

procedure TIdEtherAddr.CopyFrom(const ASource: TIdEtherAddr);
begin
  SetData(ASource.Data);
end;

procedure TIdEtherAddr.SetData(const Value: TIdBytes);
begin
  CopyTIdBytes(Value, 0, FBuffer, 0, Id_ETHER_ADDR_LEN);
end;

{ TIdEthernetHdr }

constructor TIdEthernetHdr.Create;
begin
  inherited Create;
  Fether_dhost := TIdEtherAddr.Create;
  Fether_shost := TIdEtherAddr.Create;
end;

destructor TIdEthernetHdr.Destroy;
begin
  FreeAndNil(Fether_dhost);
  FreeAndNil(Fether_shost);
  inherited Destroy;
end;

function TIdEthernetHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + Fether_dhost.BytesLen + Fether_shost.BytesLen + 2;
end;

procedure TIdEthernetHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fether_dhost.ReadStruct(ABytes, VIndex);            // destination ethernet address
  Fether_shost.ReadStruct(ABytes, VIndex);            // source ethernet address
  Fether_type := BytesToUInt16(ABytes, VIndex);         // packet type ID
  Inc(VIndex, 2);
end;

procedure TIdEthernetHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  Fether_dhost.WriteStruct(VBytes, VIndex);            // destination ethernet address
  Fether_shost.WriteStruct(VBytes, VIndex);            // source ethernet address
  CopyTIdUInt16(Fether_type, VBytes, VIndex);            // packet type ID
  Inc(VIndex, 2);
end;

procedure TIdEthernetHdr.CopyFrom(const ASource: TIdEthernetHdr);
begin
  Fether_dhost.CopyFrom(ASource.Fether_dhost);
  Fether_shost.CopyFrom(ASource.Fether_shost);
  Fether_type := ASource.Fether_type;
end;

{ TIdARPHdr }

constructor TIdARPHdr.Create;
begin
  inherited Create;
  Farp_sha := TIdEtherAddr.Create;                // sender hardware address
  Farp_spa := TIdInAddr.Create;                   // sender protocol address
  Farp_tha := TIdEtherAddr.Create;                // target hardware address
  Farp_tpa := TIdInAddr.Create;                   // target protocol address
end;

destructor TIdARPHdr.Destroy;
begin
  FreeAndNil(Farp_sha);
  FreeAndNil(Farp_spa);
  FreeAndNil(Farp_tha);
  FreeAndNil(Farp_tpa);
  inherited Destroy;
end;

function TIdARPHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 8 + Farp_sha.BytesLen + Farp_spa.BytesLen + Farp_tha.BytesLen + Farp_tpa.BytesLen;
end;

procedure TIdARPHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Farp_hrd := BytesToUInt16(ABytes, VIndex);  // format of hardware address
  Inc(VIndex, 2);
  Farp_pro := BytesToUInt16(ABytes, VIndex); // format of protocol address
  Inc(VIndex, 2);
  Farp_hln := ABytes[VIndex];             // length of hardware address
  Inc(VIndex);
  Farp_pln := ABytes[VIndex];              // length of protocol addres
  Inc(VIndex);
  Farp_op := BytesToUInt16(ABytes, VIndex);   // operation type
  Inc(VIndex, 2);
  // following hardcoded for ethernet/IP
  Farp_sha.ReadStruct(ABytes, VIndex);    // sender hardware address
  Farp_spa.ReadStruct(ABytes, VIndex);    // sender protocol address
  Farp_tha.ReadStruct(ABytes, VIndex);    // target hardware address
  Farp_tpa.ReadStruct(ABytes, VIndex);    // target protocol address
end;

procedure TIdARPHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(Farp_hrd, VBytes, VIndex);  // format of hardware address
  Inc(VIndex, 2);
  CopyTIdUInt16(Farp_pro, VBytes, VIndex); // format of protocol address
  Inc(VIndex, 2);
  VBytes[VIndex] := Farp_hln;             // length of hardware address
  Inc(VIndex);
  VBytes[VIndex] := Farp_pln;              // length of protocol addres
  Inc(VIndex);
  CopyTIdUInt16(Farp_op, VBytes, VIndex);   // operation type
  Inc(VIndex, 2);
  // following hardcoded for ethernet/IP
  Farp_sha.WriteStruct(VBytes, VIndex);    // sender hardware address
  Farp_spa.WriteStruct(VBytes, VIndex);    // sender protocol address
  Farp_tha.WriteStruct(VBytes, VIndex);    // target hardware address
  Farp_tpa.WriteStruct(VBytes, VIndex);    // target protocol address
end;

{ TIdDNSHdr }

function TIdDNSHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 12;
end;

procedure TIdDNSHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fdns_id := BytesToUInt16(ABytes, VIndex);      // DNS packet ID
  Inc(VIndex, 2);
  Fdns_flags := BytesToUInt16(ABytes, VIndex);  // DNS flags
  Inc(VIndex, 2);
  Fdns_num_q := BytesToUInt16(ABytes, VIndex);  // number of questions
  Inc(VIndex, 2);
  Fdns_num_answ_rr := BytesToUInt16(ABytes, VIndex);// number of answer resource records
  Inc(VIndex, 2);
  Fdns_num_auth_rr := BytesToUInt16(ABytes, VIndex); // number of authority resource records
  Inc(VIndex, 2);
  Fdns_num_addi_rr := BytesToUInt16(ABytes, VIndex); // number of additional resource records
  Inc(VIndex, 2);
end;

procedure TIdDNSHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt16(Fdns_id, VBytes, VIndex);     // DNS packet ID
  Inc(VIndex, 2);
  CopyTIdUInt16(Fdns_flags, VBytes, VIndex);  // DNS flags
  Inc(VIndex, 2);
  CopyTIdUInt16(Fdns_num_q, VBytes, VIndex);   // number of questions
  Inc(VIndex, 2);
  CopyTIdUInt16(Fdns_num_answ_rr, VBytes, VIndex); // number of answer resource records
  Inc(VIndex, 2);
  CopyTIdUInt16(Fdns_num_auth_rr, VBytes, VIndex); // number of authority resource records
  Inc(VIndex, 2);
  CopyTIdUInt16(Fdns_num_addi_rr, VBytes, VIndex); // number of additional resource records
  Inc(VIndex, 2);
end;

{ TIdRIPHdr }

function TIdRIPHdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 24;
end;

procedure TIdRIPHdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Frip_cmd := ABytes[VIndex];            // RIP command
  Inc(VIndex);
  Frip_ver := ABytes[VIndex];            // RIP version
  Inc(VIndex);
  Frip_rd := BytesToUInt16(ABytes, VIndex);            // zero (v1) or routing domain (v2)
  Inc(VIndex, 2);
  Frip_af := BytesToUInt16(ABytes, VIndex);              // address family
  Inc(VIndex, 2);
  Frip_rt := BytesToUInt16(ABytes, VIndex);              // zero (v1) or route tag (v2)
  Inc(VIndex, 2);
  Frip_addr := BytesToUInt32(ABytes, VIndex);        // IP address
  Inc(VIndex, 4);
  Frip_mask := BytesToUInt32(ABytes, VIndex);      // zero (v1) or subnet mask (v2)
  Inc(VIndex, 4);
  Frip_next_hop := BytesToUInt32(ABytes, VIndex); // zero (v1) or next hop IP address (v2)
  Inc(VIndex, 4);
  Frip_metric := BytesToUInt32(ABytes, VIndex);     // metric
  Inc(VIndex, 4);
end;

procedure TIdRIPHdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  VBytes[VIndex] := Frip_cmd;            // RIP command
  Inc(VIndex);
  VBytes[VIndex] := Frip_ver;            // RIP version
  Inc(VIndex);
  CopyTIdUInt16(Frip_rd, VBytes, VIndex);              // zero (v1) or routing domain (v2)
  Inc(VIndex, 2);
  CopyTIdUInt16(Frip_af, VBytes, VIndex);             // address family
  Inc(VIndex, 2);
  CopyTIdUInt16(Frip_rt, VBytes, VIndex);              // zero (v1) or route tag (v2)
  Inc(VIndex, 2);
  CopyTIdUInt32(Frip_addr, VBytes, VIndex);        // IP address
  Inc(VIndex, 4);
  CopyTIdUInt32(Frip_mask, VBytes, VIndex);        // zero (v1) or subnet mask (v2)
  Inc(VIndex, 4);
  CopyTIdUInt32(Frip_next_hop, VBytes, VIndex);    // zero (v1) or next hop IP address (v2)
  Inc(VIndex, 4);
  CopyTIdUInt32(Frip_metric, VBytes, VIndex);      // metric
  Inc(VIndex, 4);
end;

{ TIdInAddr }

procedure TIdInAddr.CopyFrom(const ASource: TIdInAddr);
begin
  s_l := ASource.s_l;
end;

{ TIdicmp6_un }

constructor TIdicmp6_un.Create;
begin
  inherited Create;
  SetBytesLen(4);
end;

function TIdicmp6_un.Geticmp6_un_data16(Index: Integer): UInt16;
begin
  Result := 0;
  case Index of
    0 : Result := BytesToUInt16(FBuffer, 0);
    1 : Result := BytesToUInt16(FBuffer, 2);
  end;
end;

procedure TIdicmp6_un.Seticmp6_un_data16(Index: Integer; const Value: UInt16);
begin
  case Index of
    0 : CopyTIdUInt16(Value, FBuffer, 0);
    1 : CopyTIdUInt16(Value, FBuffer, 2);
  end;
end;

function TIdicmp6_un.Geticmp6_un_data32: UInt32;
begin
  Result := BytesToUInt32(FBuffer, 0);
end;

procedure TIdicmp6_un.Seticmp6_un_data32(const Value: UInt32);
begin
  CopyTIdUInt32(Value, FBuffer, 0);
end;

function TIdicmp6_un.Geticmp6_un_data8(Index: Integer): UInt8;
begin
  Assert((Index>-1) and (Index<4), 'Out of range');
  Result := FBuffer[Index];
end;

procedure TIdicmp6_un.Seticmp6_un_data8(Index: Integer; const Value: UInt8);
begin
  Assert((Index>-1) and (Index<4), 'Out of range');
  FBuffer[Index] := Value;
end;

function TIdicmp6_un.Geticmp6_data8: UInt8;
begin
  Result := FBuffer[0];
end;

procedure TIdicmp6_un.Seticmp6_data8(const Value: UInt8);
begin
  FBuffer[0] := Value;
end;

function TIdicmp6_un.Geticmp6_data16: UInt16;
begin
  Result := BytesToUInt16(FBuffer, 0);
end;

procedure TIdicmp6_un.Seticmp6_data16(const Value: UInt16);
begin
  CopyTIdUInt16(Value, FBuffer, 0);
end;

function TIdicmp6_un.Geticmp6_seq: UInt16;
begin
  Result := Geticmp6_un_data16(1);
end;

procedure TIdicmp6_un.Seticmp6_seq(const Value: UInt16);
begin
  Seticmp6_un_data16(1, Value);
end;

{ TIdicmp6_hdr }

constructor TIdicmp6_hdr.Create;
begin
  inherited Create;
  Fdata := TIdicmp6_un.Create;
end;

destructor TIdicmp6_hdr.Destroy;
begin
  FreeAndNil(Fdata);
  inherited Destroy;
end;

function TIdicmp6_hdr.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4 + Fdata.BytesLen;
end;

procedure TIdicmp6_hdr.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Ficmp6_type := ABytes[VIndex];
  Inc(VIndex);
  FIcmp6_code := ABytes[VIndex];
  Inc(VIndex);
  Ficmp6_cksum := BytesToUInt16(ABytes, VIndex);
  Inc(VIndex, 2);
  Fdata.ReadStruct(ABytes, VIndex);
end;

procedure TIdicmp6_hdr.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  VBytes[VIndex] := Ficmp6_type;
  Inc(VIndex);
  VBytes[VIndex] := FIcmp6_code;
  Inc(VIndex);
  CopyTIdUInt16(Ficmp6_cksum, VBytes, VIndex);
  Inc(VIndex, 2);
  Fdata.WriteStruct(VBytes, VIndex);
end;

end.


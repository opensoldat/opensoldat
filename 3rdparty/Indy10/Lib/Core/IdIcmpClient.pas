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
  Rev 1.8    2004-04-25 12:08:24  Mattias
  Fixed multithreading issue

  Rev 1.7    2004.02.03 4:16:42 PM  czhower
  For unit name changes.

  Rev 1.6    2/1/2004 4:53:30 PM  JPMugaas
  Removed Todo;

  Rev 1.5    2004.01.20 10:03:24 PM  czhower
  InitComponent

  Rev 1.4    2003.12.31 10:37:54 PM  czhower
  GetTickcount --> Ticks

  Rev 1.3    10/16/2003 11:06:14 PM  SPerry
  Moved ICMP_MIN to IdRawHeaders

  Rev 1.2    2003.10.11 5:48:04 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.1    2003.09.30 1:22:56 PM  czhower
  Stack split for DotNet

  Rev 1.0    11/13/2002 08:44:30 AM  JPMugaas

  25/1/02: SGrobety:
  Modified the component to support multithreaded PING and traceroute
  NOTE!!!
  The component no longer use the timing informations contained
  in the packet to compute the roundtrip time. This is because
  that information is only correctly set in case of ECHOREPLY
  In case of TTL, it is incorrect.
}

unit IdIcmpClient;

{
  Note that we can NOT remove the DotNET IFDEFS from this unit.   The reason is
  that Microsoft NET Framework 1.1 does not support ICMPv6 and that's required
  for IPv6.  In Win32 and Linux, we definately can and want to support IPv6.

  If we support a later version of the NET framework that has a better API, I may
  consider revisiting this.
}

// SG 25/1/02: Modified the component to support multithreaded PING and traceroute

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  Classes,
  IdGlobal,
  IdRawBase,
  IdRawClient,
  IdStackConsts,
  IdBaseComponent;

const
  DEF_PACKET_SIZE = 32;
  MAX_PACKET_SIZE = 1024;
  Id_TIDICMP_ReceiveTimeout = 5000;

type
  TReplyStatusTypes = (rsEcho,
    rsError, rsTimeOut, rsErrorUnreachable,
    rsErrorTTLExceeded,rsErrorPacketTooBig,
    rsErrorParameter,
    rsErrorDatagramConversion,
    rsErrorSecurityFailure,
    rsSourceQuench,
    rsRedirect,
    rsTimeStamp,
    rsInfoRequest,
    rsAddressMaskRequest,
    rsTraceRoute,
    rsMobileHostReg,
    rsMobileHostRedir,
    rsIPv6WhereAreYou,
    rsIPv6IAmHere,
    rsSKIP);

  TReplyStatus = class(TObject)
  protected
    FBytesReceived: integer; // number of bytes in reply from host
    FFromIpAddress: string;  // IP address of replying host
    FToIpAddress : string;   //who receives it (i.e., us.  This is for multihorned machines
    FMsgType: byte;
    FMsgCode : Byte;
    FSequenceId: word;       // sequence id of ping reply
    // TODO: roundtrip time in ping reply should be float, not byte
    FMsRoundTripTime: UInt32; // ping round trip time in milliseconds
    FTimeToLive: byte;       // time to live
    FReplyStatusType: TReplyStatusTypes;
    FPacketNumber : Integer;//number in packet for TraceRoute
    FHostName : String; //Hostname of computer that replied, used with TraceRoute
    FMsg : String;
    FRedirectTo : String; // valid only for rsRedirect
  public
    property RedirectTo : String read FRedirectTo write FRedirectTo;
    property Msg : String read FMsg write FMsg;
    property BytesReceived: integer read FBytesReceived write FBytesReceived; // number of bytes in reply from host
    property FromIpAddress: string read FFromIpAddress write FFromIpAddress;  // IP address of replying host
    property ToIpAddress : string read FToIpAddress write FToIpAddress;   //who receives it (i.e., us.  This is for multihorned machines
    property MsgType: byte read FMsgType write FMsgType;
    property MsgCode : Byte read FMsgCode write FMsgCode;
    property SequenceId: word read FSequenceId write FSequenceId;       // sequence id of ping reply
    // TODO: roundtrip time in ping reply should be float, not byte
    property MsRoundTripTime: UInt32 read FMsRoundTripTime write FMsRoundTripTime; // ping round trip time in milliseconds
    property TimeToLive: byte read FTimeToLive write FTimeToLive;       // time to live
    property ReplyStatusType: TReplyStatusTypes read FReplyStatusType write FReplyStatusType;
    property HostName : String read FHostName write FHostName;
    property PacketNumber : Integer read FPacketNumber write FPacketNumber;
  end;

  TOnReplyEvent = procedure(ASender: TComponent; const AReplyStatus: TReplyStatus) of object;

  // TODO: on MacOSX (and maybe iOS?), can use a UDP socket instead of a RAW
  // socket so that non-privilege processes do not require root access...

  // TODO: on Windows, can use IcmpSendEcho() instead of a RAW so that
  // non-privilege processes do not require admin access...

  TIdCustomIcmpClient = class(TIdRawClient)
  protected
    FStartTime : TIdTicks; //this is a fallback if no packet is returned
    FPacketSize : Integer;
    FBufReceive: TIdBytes;
    FBufIcmp: TIdBytes;
    wSeqNo: word;
    iDataSize: integer;
    FReplyStatus: TReplyStatus;
    FOnReply: TOnReplyEvent;
    FReplydata: String;
    //
    {$IFNDEF DOTNET_1_1}
    function DecodeIPv6Packet(BytesRead: UInt32): Boolean;
    {$ENDIF}
    function DecodeIPv4Packet(BytesRead: UInt32): Boolean;
    function DecodeResponse(BytesRead: UInt32): Boolean;
    procedure DoReply; virtual;
    procedure GetEchoReply;
    procedure InitComponent; override;
    {$IFNDEF DOTNET_1_1}
    procedure PrepareEchoRequestIPv6(const ABuffer: String);
    {$ENDIF}
    procedure PrepareEchoRequestIPv4(const ABuffer: String);
    procedure PrepareEchoRequest(const ABuffer: String);
    procedure SendEchoRequest; overload;
    procedure SendEchoRequest(const AIP : String); overload;
    function GetPacketSize: Integer;
    procedure SetPacketSize(const AValue: Integer);

    //these are made public in the client
    procedure InternalPing(const AIP : String; const ABuffer: String = ''; SequenceID: Word = 0); overload; {Do not Localize}
    //
    property PacketSize : Integer read GetPacketSize write SetPacketSize default DEF_PACKET_SIZE;
    property ReplyData: string read FReplydata;
    property ReplyStatus: TReplyStatus read FReplyStatus;

    property OnReply: TOnReplyEvent read FOnReply write FOnReply;

  public
    destructor Destroy; override;
    procedure Send(const AHost: string; const APort: TIdPort; const ABuffer : TIdBytes); override;
    procedure Send(const ABuffer : TIdBytes); override;
    function Receive(ATimeOut: Integer): TReplyStatus;
  end;

  TIdIcmpClient = class(TIdCustomIcmpClient)
  public
    procedure Ping(const ABuffer: String = ''; SequenceID: Word = 0);    {Do not Localize}
    property ReplyData;
    property ReplyStatus;
  published
    property Host;
    {$IFNDEF DOTNET_1_1}
    property IPVersion;
    {$ENDIF}
    property PacketSize;
    property ReceiveTimeout default Id_TIDICMP_ReceiveTimeout;
    property OnReply;
  end;

implementation

uses
  //facilitate inlining only.
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF USE_VCL_POSIX}
    {$IFDEF DARWIN}
  Macapi.CoreServices,
    {$ENDIF}
  {$ENDIF}
  IdExceptionCore, IdRawHeaders, IdResourceStringsCore,
  IdStack, IdStruct, SysUtils;

{ TIdCustomIcmpClient }

procedure TIdCustomIcmpClient.PrepareEchoRequest(const ABuffer: String);
begin
  {$IFNDEF DOTNET_1_1}
  if IPVersion = Id_IPv6 then begin
    PrepareEchoRequestIPv6(ABuffer);
    Exit;
  end;
  {$ENDIF}
  PrepareEchoRequestIPv4(ABuffer);
end;

{ TIdIPv4_ICMP }

type
  TIdIPv4_ICMP = class(TIdStruct)
  protected
    Fip_hdr: TIdIPHdr;
    Ficmp_hdr: TIdICMPHdr;
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property ip_hdr: TIdIPHdr read Fip_hdr;
    property icmp_hdr: TIdICMPHdr read Ficmp_hdr;
  end;

constructor TIdIPv4_ICMP.Create;
begin
  inherited Create;
  Fip_hdr := TIdIPHdr.Create;
  Ficmp_hdr := TIdICMPHdr.Create;
end;
  
destructor TIdIPv4_ICMP.Destroy;
begin
  FreeAndNil(Fip_hdr);
  FreeAndNil(Ficmp_hdr);
  inherited Destroy;
end;

function TIdIPv4_ICMP.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + Fip_hdr.BytesLen + Ficmp_hdr.BytesLen;
end;

procedure TIdIPv4_ICMP.ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Fip_hdr.ReadStruct(ABytes, VIndex);
  Ficmp_hdr.ReadStruct(ABytes, VIndex);
end;

procedure TIdIPv4_ICMP.WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  Fip_hdr.WriteStruct(VBytes, VIndex);
  Ficmp_hdr.WriteStruct(VBytes, VIndex);
end;

{ TIdCustomIcmpClient }

procedure TIdCustomIcmpClient.SendEchoRequest;
begin
  Send(FBufIcmp);
end;

function TIdCustomIcmpClient.DecodeResponse(BytesRead: UInt32): Boolean;
begin
  if BytesRead = 0 then begin
    // Timed out
    FReplyStatus.MsRoundTripTime := GetElapsedTicks(FStartTime);
    FReplyStatus.BytesReceived   := 0;
    if IPVersion = Id_IPv4 then
    begin
      FReplyStatus.FromIpAddress   := '0.0.0.0';
      FReplyStatus.ToIpAddress     := '0.0.0.0';
    end else
    begin
      FReplyStatus.FromIpAddress   := '::0';
      FReplyStatus.ToIpAddress     := '::0';
    end;
    FReplyStatus.MsgType         := 0;
    FReplyStatus.SequenceId      := wSeqNo;
    FReplyStatus.TimeToLive      := 0;
    FReplyStatus.ReplyStatusType := rsTimeOut;
    Result := True;
  end else
  begin
    FReplyStatus.ReplyStatusType := rsError;
    {$IFNDEF DOTNET_1_1}
    if IPVersion = Id_IPv6 then begin
      Result := DecodeIPv6Packet(BytesRead);
      Exit;
    end;
    {$ENDIF}
    Result := DecodeIPv4Packet(BytesRead);
  end;
end;

procedure TIdCustomIcmpClient.GetEchoReply;
begin
  Receive(FReceiveTimeout);
end;

function TIdCustomIcmpClient.Receive(ATimeOut: Integer): TReplyStatus;
var
  BytesRead : Integer;
  TripTime: UInt32;
begin
  Result := FReplyStatus;
  FillBytes(FBufReceive, Length(FBufReceive), 0);
  FStartTime := Ticks64;
  repeat
    BytesRead := ReceiveBuffer(FBufReceive, ATimeOut);
    if DecodeResponse(BytesRead) then begin
      Break;
    end;
    TripTime := GetElapsedTicks(FStartTime);
    ATimeOut := ATimeOut - Integer(TripTime); // compute new timeout value
    FReplyStatus.MsRoundTripTime := TripTime;
    FReplyStatus.Msg := RSICMPTimeout;
    // We caught a response that wasn't meant for this thread - so we must
    // make sure we don't report it as such in case we time out after this
    FReplyStatus.BytesReceived   := 0;
    if IPVersion = Id_IPv4 then
    begin
      FReplyStatus.FromIpAddress   := '0.0.0.0';
      FReplyStatus.ToIpAddress     := '0.0.0.0';
    end else
    begin
      FReplyStatus.FromIpAddress   := '::0';
      FReplyStatus.ToIpAddress     := '::0';
    end;
    FReplyStatus.MsgType         := 0;
    FReplyStatus.SequenceId      := wSeqNo;
    FReplyStatus.TimeToLive      := 0;
    FReplyStatus.ReplyStatusType := rsTimeOut;
  until ATimeOut <= 0;
end;

procedure TIdCustomIcmpClient.DoReply;
begin
  if Assigned(FOnReply) then begin
    FOnReply(Self, FReplyStatus);
  end;
end;

procedure TIdCustomIcmpClient.InitComponent;
begin
  inherited InitComponent;
  FReplyStatus:= TReplyStatus.Create;
  FProtocol := Id_IPPROTO_ICMP;
  {$IFNDEF DOTNET_1_1}
  ProtocolIPv6 := Id_IPPROTO_ICMPv6;
  {$ENDIF}
  wSeqNo := 3489; // SG 25/1/02: Arbitrary Constant <> 0
  FReceiveTimeOut := Id_TIDICMP_ReceiveTimeout;
  FPacketSize := DEF_PACKET_SIZE;
end;

destructor TIdCustomIcmpClient.Destroy;
begin
  FreeAndNil(FReplyStatus);
  inherited Destroy;
end;

function TIdCustomIcmpClient.DecodeIPv4Packet(BytesRead: UInt32): Boolean;
var
  LIPHeaderLen: UInt32;
  LIdx: UInt32;
  RTTime: UInt32;
  LActualSeqID: UInt16;
  LIcmp: TIdIPv4_ICMP;
  LIcmpts: TIdICMPTs;
begin
  Result := False;

  LIpHeaderLen := (FBufReceive[0] and $0F) * 4;
  if BytesRead < (LIpHeaderLen + ICMP_MIN) then begin
    raise EIdIcmpException.Create(RSICMPNotEnoughtBytes);
  end;
  LIdx := 0;

  LIcmp := TIdIPv4_ICMP.Create;
  try
    LIcmp.ReadStruct(FBufReceive, LIdx);

    {$IFDEF LINUX}
    // TODO: baffled as to why linux kernel sends back echo from localhost
    {$ENDIF}

    case LIcmp.icmp_hdr.icmp_type of
      Id_ICMP_ECHOREPLY, Id_ICMP_ECHO:
      begin
        FReplyStatus.ReplyStatusType := rsEcho;
        FReplyData := BytesToStringRaw(FBufReceive, LIdx, -1);
        // result is only valid if the seq. number is correct
      end;
      Id_ICMP_UNREACH:
        FReplyStatus.ReplyStatusType := rsErrorUnreachable;
      Id_ICMP_TIMXCEED:
        FReplyStatus.ReplyStatusType := rsErrorTTLExceeded;
      Id_ICMP_PARAMPROB :
        FReplyStatus.ReplyStatusType := rsErrorParameter;
      Id_ICMP_REDIRECT :
        FReplyStatus.ReplyStatusType := rsRedirect;
      Id_ICMP_TSTAMP, Id_ICMP_TSTAMPREPLY :
        FReplyStatus.ReplyStatusType := rsTimeStamp;
      Id_ICMP_IREQ, Id_ICMP_IREQREPLY :
        FReplyStatus.ReplyStatusType := rsInfoRequest;
      Id_ICMP_MASKREQ, Id_ICMP_MASKREPLY :
        FReplyStatus.ReplyStatusType := rsAddressMaskRequest;
      Id_ICMP_TRACEROUTE :
        FReplyStatus.ReplyStatusType := rsTraceRoute;
      Id_ICMP_DATAGRAM_CONV :
        FReplyStatus.ReplyStatusType := rsErrorDatagramConversion;
      Id_ICMP_MOB_HOST_REDIR :
        FReplyStatus.ReplyStatusType := rsMobileHostRedir;
      Id_ICMP_IPv6_WHERE_ARE_YOU :
        FReplyStatus.ReplyStatusType := rsIPv6WhereAreYou;
      Id_ICMP_IPv6_I_AM_HERE :
        FReplyStatus.ReplyStatusType := rsIPv6IAmHere;
      Id_ICMP_MOB_REG_REQ, Id_ICMP_MOB_REG_REPLY :
        FReplyStatus.ReplyStatusType := rsMobileHostReg;
      Id_ICMP_PHOTURIS :
        FReplyStatus.ReplyStatusType := rsErrorSecurityFailure;
      else
        raise EIdICMPException.Create(RSICMPNonEchoResponse);// RSICMPNonEchoResponse = 'Non-echo type response received'
    end;    // case

    // check if we got a reply to the packet that was actually sent
    case FReplyStatus.ReplyStatusType of
      rsEcho:
      begin
        LActualSeqID := LIcmp.icmp_hdr.icmp_hun.echo_seq;
        RTTime := GetElapsedTicks(BytesToTicks(FBufReceive, LIdx));
      end;
      rsTimeStamp:
      begin
        LActualSeqID := LIcmp.icmp_hdr.icmp_hun.echo_seq;
        LIcmpts := TIdICMPTs.Create;
        try
          LIcmpts.ReadStruct(FBufReceive, LIpHeaderLen);
          RTTime := (LIcmpts.ttime and $80000000) - (LIcmpts.otime and $80000000);
        finally
          LIcmpts.Free;
        end;
      end;
    else
      begin
        // not an echo or timestamp reply: the original IP frame is
        // contained withing the DATA section of the packet...
        // pOriginalIP := PIdIPHdr(@picmp^.icmp_dun.data);

        // TODO: verify this!  I don't think it is indexing far enough into the data
        LActualSeqID := BytesToUInt16(FBufReceive, LIpHeaderLen+8+6);//pOriginalICMP^.icmp_hun.echo.seq;
        RTTime := GetElapsedTicks(BytesToTicks(FBufReceive, LIpHeaderLen+8+8)); //pOriginalICMP^.icmp_dun.ts.otime;

        // move to offset
        // pOriginalICMP := Pointer(PtrUInt(pOriginalIP) + (iIpHeaderLen));
        // extract information from original ICMP frame
        // ActualSeqID := pOriginalICMP^.icmp_hun.echo.seq;
        // RTTime := Ticks64 - pOriginalICMP^.icmp_dun.ts.otime;
        // Result := pOriginalICMP^.icmp_hun.echo.seq = wSeqNo;
      end;
    end;

    Result := LActualSeqID = wSeqNo;//;picmp^.icmp_hun.echo.seq  = wSeqNo;
    if Result then
    begin
      if FReplyStatus.ReplyStatusType = rsEcho then begin
        FReplyStatus.BytesReceived := BytesRead - (Id_IP_HSIZE + ICMP_MIN + SizeOf(TIdTicks));
      end else begin
        FReplyStatus.BytesReceived := BytesRead - (Id_IP_HSIZE + ICMP_MIN);
      end;

      FReplyStatus.FromIpAddress := MakeUInt32IntoIPv4Address(GStack.NetworkToHost(Licmp.ip_hdr.ip_src.s_l));
      FReplyStatus.ToIpAddress   := MakeUInt32IntoIPv4Address(GStack.NetworkToHost(Licmp.ip_hdr.ip_dst.s_l));
      FReplyStatus.MsgType := LIcmp.icmp_hdr.icmp_type; //picmp^.icmp_type;
      FReplyStatus.MsgCode := LIcmp.icmp_hdr.icmp_code; //picmp^.icmp_code;
      FReplyStatus.SequenceId := LActualSeqID;
      FReplyStatus.MsRoundTripTime := RTTime;
      FReplyStatus.TimeToLive := LIcmp.ip_hdr.ip_ttl;
      // now process our message stuff

      case FReplyStatus.MsgType of
        Id_ICMP_UNREACH:
        begin
          case FReplyStatus.MsgCode of
            Id_ICMP_UNREACH_NET                : FReplyStatus.Msg := RSICMPNetUnreachable;
            Id_ICMP_UNREACH_HOST               : FReplyStatus.Msg := RSICMPHostUnreachable;
            Id_ICMP_UNREACH_PROTOCOL           : FReplyStatus.Msg := RSICMPProtUnreachable;
            Id_ICMP_UNREACH_NEEDFRAG           : FReplyStatus.Msg := RSICMPFragmentNeeded;
            Id_ICMP_UNREACH_SRCFAIL            : FReplyStatus.Msg := RSICMPSourceRouteFailed;
            Id_ICMP_UNREACH_NET_UNKNOWN        : FReplyStatus.Msg := RSICMPDestNetUnknown;
            Id_ICMP_UNREACH_HOST_UNKNOWN       : FReplyStatus.Msg := RSICMPDestHostUnknown;
            Id_ICMP_UNREACH_ISOLATED           : FReplyStatus.Msg := RSICMPSourceIsolated;
            Id_ICMP_UNREACH_NET_PROHIB         : FReplyStatus.Msg := RSICMPDestNetProhibitted;
            Id_ICMP_UNREACH_HOST_PROHIB        : FReplyStatus.Msg := RSICMPDestHostProhibitted;
            Id_ICMP_UNREACH_TOSNET             : FReplyStatus.Msg := RSICMPTOSNetUnreach;
            Id_ICMP_UNREACH_TOSHOST            : FReplyStatus.Msg := RSICMPTOSHostUnreach;
            Id_ICMP_UNREACH_FILTER_PROHIB      : FReplyStatus.Msg := RSICMPAdminProhibitted;
            Id_ICMP_UNREACH_HOST_PRECEDENCE    : FReplyStatus.Msg := RSICMPHostPrecViolation;
            Id_ICMP_UNREACH_PRECEDENCE_CUTOFF  : FReplyStatus.Msg := RSICMPPrecedenceCutoffInEffect;
          end;
        end;
        Id_ICMP_TIMXCEED:
        begin
          case FReplyStatus.MsgCode of
            0 : FReplyStatus.Msg := RSICMPTTLExceeded;
            1 : FReplyStatus.Msg := RSICMPFragAsmExceeded;
          end;
        end;
        Id_ICMP_PARAMPROB                   : FReplyStatus.Msg := IndyFormat(RSICMPParamError, [FReplyStatus.MsgCode]);
        Id_ICMP_REDIRECT:
        begin
          FReplyStatus.RedirectTo := MakeUInt32IntoIPv4Address(GStack.NetworkToHOst(LIcmp.icmp_hdr.icmp_hun.gateway_s_l));
          case FReplyStatus.MsgCode of
            0 : FReplyStatus.Msg := RSICMPRedirNet;
            1 : FReplyStatus.Msg := RSICMPRedirHost;
            2 : FReplyStatus.Msg := RSICMPRedirTOSNet;
            3 : FReplyStatus.Msg := RSICMPRedirTOSHost;
          end;
        end;
        Id_ICMP_SOURCEQUENCH                : FReplyStatus.Msg := RSICMPSourceQuenchMsg;
        Id_ICMP_ECHOREPLY, Id_ICMP_ECHO     : FReplyStatus.Msg := RSICMPEcho;
        Id_ICMP_TSTAMP, Id_ICMP_TSTAMPREPLY : FReplyStatus.Msg := RSICMPTimeStamp;
        Id_ICMP_IREQ, Id_ICMP_IREQREPLY     : FReplyStatus.Msg := RSICMPTimeStamp;
        Id_ICMP_MASKREQ, Id_ICMP_MASKREPLY  : FReplyStatus.Msg := RSICMPMaskRequest;
        Id_ICMP_TRACEROUTE :
        begin
          case FReplyStatus.MsgCode of
            Id_ICMP_TRACEROUTE_PACKET_FORWARDED : FReplyStatus.Msg := RSICMPTracePacketForwarded;
            Id_ICMP_TRACEROUTE_NO_ROUTE         : FReplyStatus.Msg := RSICMPTraceNoRoute;
          end;
        end;
        Id_ICMP_DATAGRAM_CONV:
        begin
          case FReplyStatus.MsgCode of
            Id_ICMP_CONV_UNSPEC                    : FReplyStatus.Msg := RSICMPTracePacketForwarded;
            Id_ICMP_CONV_DONTCONV_OPTION           : FReplyStatus.Msg := RSICMPTraceNoRoute;
            Id_ICMP_CONV_UNKNOWN_MAN_OPTION        : FReplyStatus.Msg := RSICMPConvUnknownMandOptPresent;
            Id_ICMP_CONV_UNKNWON_UNSEP_OPTION      : FReplyStatus.Msg := RSICMPConvKnownUnsupportedOptionPresent;
            Id_ICMP_CONV_UNSEP_TRANSPORT           : FReplyStatus.Msg := RSICMPConvUnsupportedTransportProtocol;
            Id_ICMP_CONV_OVERALL_LENGTH_EXCEEDED   : FReplyStatus.Msg := RSICMPConvOverallLengthExceeded;
            Id_ICMP_CONV_IP_HEADER_LEN_EXCEEDED    : FReplyStatus.Msg := RSICMPConvIPHeaderLengthExceeded;
            Id_ICMP_CONV_TRANS_PROT_255            : FReplyStatus.Msg := RSICMPConvTransportProtocol_255;
            Id_ICMP_CONV_PORT_OUT_OF_RANGE         : FReplyStatus.Msg := RSICMPConvPortConversionOutOfRange;
            Id_ICMP_CONV_TRANS_HEADER_LEN_EXCEEDED : FReplyStatus.Msg := RSICMPConvTransportHeaderLengthExceeded;
            Id_ICMP_CONV_32BIT_ROLLOVER_AND_ACK    : FReplyStatus.Msg := RSICMPConv32BitRolloverMissingAndACKSet;
            Id_ICMP_CONV_UNKNOWN_MAN_TRANS_OPTION  : FReplyStatus.Msg := RSICMPConvUnknownMandatoryTransportOptionPresent;
          end;
        end;
        Id_ICMP_MOB_HOST_REDIR                     : FReplyStatus.Msg := RSICMPMobileHostRedirect;
        Id_ICMP_IPv6_WHERE_ARE_YOU                 : FReplyStatus.Msg := RSICMPIPv6WhereAreYou;
        Id_ICMP_IPv6_I_AM_HERE                     : FReplyStatus.Msg := RSICMPIPv6IAmHere;
        Id_ICMP_MOB_REG_REQ, Id_ICMP_MOB_REG_REPLY : FReplyStatus.Msg := RSICMPIPv6IAmHere;
        Id_ICMP_SKIP                               : FReplyStatus.Msg := RSICMPSKIP;
        Id_ICMP_PHOTURIS :
        begin
          case FReplyStatus.MsgCode of
            Id_ICMP_BAD_SPI             : FReplyStatus.Msg := RSICMPSecBadSPI;
            Id_ICMP_AUTH_FAILED         : FReplyStatus.Msg := RSICMPSecAuthenticationFailed;
            Id_ICMP_DECOMPRESS_FAILED   : FReplyStatus.Msg := RSICMPSecDecompressionFailed;
            Id_ICMP_DECRYPTION_FAILED   : FReplyStatus.Msg := RSICMPSecDecryptionFailed;
            Id_ICMP_NEED_AUTHENTICATION : FReplyStatus.Msg := RSICMPSecNeedAuthentication;
            Id_ICMP_NEED_AUTHORIZATION  : FReplyStatus.Msg := RSICMPSecNeedAuthorization;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(LIcmp);
  end;
end;

procedure TIdCustomIcmpClient.PrepareEchoRequestIPv4(const ABuffer: String);
var
  LIcmp: TIdICMPHdr;
  LIdx: UInt32;
  LBuffer: TIdBytes;
  LBufferLen: Integer;
begin
  LBuffer := ToBytes(ABuffer, IndyTextEncoding_8Bit);
  LBufferLen := IndyMin(Length(LBuffer), FPacketSize);

  SetLength(FBufIcmp, ICMP_MIN + SizeOf(TIdTicks) + LBufferLen);
  FillBytes(FBufIcmp, Length(FBufIcmp), 0);
  SetLength(FBufReceive, Id_IP_HSIZE + Length(FBufIcmp));

  LIdx := 0;
  LIcmp := TIdICMPHdr.Create;
  try
    LIcmp.icmp_type := Id_ICMP_ECHO;
    LIcmp.icmp_code := 0;
    LIcmp.icmp_sum := 0;
    LIcmp.icmp_hun.echo_id := Word(CurrentProcessId);
    LIcmp.icmp_hun.echo_seq := wSeqNo;
    LIcmp.WriteStruct(FBufIcmp, LIdx);
    CopyTIdTicks(Ticks64, FBufIcmp, LIdx);
    Inc(LIdx, SizeOf(TIdTicks));
    if LBufferLen > 0 then begin
      CopyTIdBytes(LBuffer, 0, FBufIcmp, LIdx, LBufferLen);
    end;
  finally
    FreeAndNil(LIcmp);
  end;
end;

{$IFNDEF DOTNET_1_1}
procedure TIdCustomIcmpClient.PrepareEchoRequestIPv6(const ABuffer: String);
var
  LIcmp : TIdicmp6_hdr;
  LIdx : UInt32;
  LBuffer: TIdBytes;
  LBufferLen: Integer;
begin
  LBuffer := ToBytes(ABuffer, IndyTextEncoding_8Bit);
  LBufferLen := IndyMin(Length(LBuffer), FPacketSize);

  SetLength(FBufIcmp, ICMP_MIN + SizeOf(TIdTicks) + LBufferLen);
  FillBytes(FBufIcmp, Length(FBufIcmp), 0);
  SetLength(FBufReceive, Length(FBufIcmp) + (Id_IPv6_HSIZE*2));

  LIdx := 0;
  LIcmp := TIdicmp6_hdr.Create;
  try
    LIcmp.icmp6_type := ICMP6_ECHO_REQUEST;
    LIcmp.icmp6_code := 0;
    LIcmp.data.icmp6_un_data16[0] := Word(CurrentProcessId);
    LIcmp.data.icmp6_un_data16[1] := wSeqNo;
    LIcmp.icmp6_cksum := 0;
    LIcmp.WriteStruct(FBufIcmp, LIdx);
    CopyTIdTicks(Ticks64, FBufIcmp, LIdx);
    Inc(LIdx, SizeOf(TIdTicks));
    if LBufferLen > 0 then begin
      CopyTIdBytes(LBuffer, 0, FBufIcmp, LIdx, LBufferLen);
    end;
  finally
    FreeAndNil(LIcmp);
  end;
end;

function TIdCustomIcmpClient.DecodeIPv6Packet(BytesRead: UInt32): Boolean;
var
  LIdx : UInt32;
  LIcmp : TIdicmp6_hdr;
  RTTime : UInt32;
  LActualSeqID : Word;
begin
  LIdx := 0;
  LIcmp := TIdicmp6_hdr.Create;
  try
    // Note that IPv6 raw headers are not being returned.
    LIcmp.ReadStruct(FBufReceive, LIdx);

    case LIcmp.icmp6_type of
      ICMP6_ECHO_REQUEST,
      ICMP6_ECHO_REPLY           : FReplyStatus.ReplyStatusType := rsEcho;
      //group membership messages
      ICMP6_MEMBERSHIP_QUERY     : ;
      ICMP6_MEMBERSHIP_REPORT    : ;
      ICMP6_MEMBERSHIP_REDUCTION : ;
      //errors
      ICMP6_DST_UNREACH          : FReplyStatus.ReplyStatusType := rsErrorUnreachable;
      ICMP6_PACKET_TOO_BIG       : FReplyStatus.ReplyStatusType := rsErrorPacketTooBig;
      ICMP6_TIME_EXCEEDED        : FReplyStatus.ReplyStatusType := rsErrorTTLExceeded;
      ICMP6_PARAM_PROB           : FReplyStatus.ReplyStatusType := rsErrorParameter;
      else                         FReplyStatus.ReplyStatusType  := rsError;
    end;
    FReplyStatus.MsgType := LIcmp.icmp6_type; //picmp^.icmp_type;
    FReplyStatus.MsgCode := LIcmp.icmp6_code;

    //errors are values less than ICMP6_INFOMSG_MASK
    if LIcmp.icmp6_type < ICMP6_INFOMSG_MASK then
    begin
      //read info from the original packet part
      LIcmp.ReadStruct(FBufReceive, LIdx);
    end;

    LActualSeqID := LIcmp.data.icmp6_seq;
    Result := LActualSeqID = wSeqNo;

    RTTime := GetElapsedTicks(BytesToTicks(FBufReceive, LIdx));
    Inc(LIdx, SizeOf(TIdTicks));

    if Result then
    begin
      FReplyStatus.BytesReceived := BytesRead - LIdx;
      FReplyStatus.SequenceId := LActualSeqID;
      FReplyStatus.MsRoundTripTime := RTTime;
      // TimeToLive := FBufReceive[8];
      // TimeToLive := pip^.ip_ttl;
      FReplyStatus.TimeToLive := FPkt.TTL;
      FReplyStatus.FromIpAddress := FPkt.SourceIP;
      FReplyStatus.ToIpAddress := FPkt.DestIP;

      case FReplyStatus.MsgType of
        ICMP6_ECHO_REQUEST, ICMP6_ECHO_REPLY : FReplyStatus.Msg := RSICMPEcho;
        ICMP6_TIME_EXCEEDED :
        begin
          case FReplyStatus.MsgCode of
            ICMP6_TIME_EXCEED_TRANSIT    : FReplyStatus.Msg := RSICMPHopLimitExceeded;
            ICMP6_TIME_EXCEED_REASSEMBLY : FReplyStatus.Msg := RSICMPFragAsmExceeded;
          end;
        end;
        ICMP6_DST_UNREACH :
        begin
          case FReplyStatus.MsgCode of
            ICMP6_DST_UNREACH_NOROUTE          : FReplyStatus.Msg := RSICMPNoRouteToDest;
            ICMP6_DST_UNREACH_ADMIN            : FReplyStatus.Msg := RSICMPAdminProhibitted;
            ICMP6_DST_UNREACH_ADDR             : FReplyStatus.Msg := RSICMPHostUnreachable;
            ICMP6_DST_UNREACH_NOPORT           : FReplyStatus.Msg := RSICMPProtUnreachable;
            ICMP6_DST_UNREACH_SOURCE_FILTERING : FReplyStatus.Msg := RSICMPSourceFilterFailed;
            ICMP6_DST_UNREACH_REJCT_DST        : FReplyStatus.Msg := RSICMPRejectRoutToDest;
          end;
        end;
        ICMP6_PACKET_TOO_BIG           : FReplyStatus.Msg := IndyFormat(RSICMPPacketTooBig, [LIcmp.data.icmp6_mtu]);
        ICMP6_PARAM_PROB :
        begin
          case FReplyStatus.MsgCode of
            ICMP6_PARAMPROB_HEADER     : FReplyStatus.Msg := IndyFormat(RSICMPParamHeader, [LIcmp.data.icmp6_pptr]);
            ICMP6_PARAMPROB_NEXTHEADER : FReplyStatus.Msg := IndyFormat(RSICMPParamNextHeader, [LIcmp.data.icmp6_pptr]);
            ICMP6_PARAMPROB_OPTION     : FReplyStatus.Msg := IndyFormat(RSICMPUnrecognizedOpt, [LIcmp.data.icmp6_pptr]);
          end;
        end;
        ICMP6_MEMBERSHIP_QUERY : ;
        ICMP6_MEMBERSHIP_REPORT : ;
        ICMP6_MEMBERSHIP_REDUCTION :;
      end;
    end;
  finally
    FreeAndNil(LIcmp);
  end;
end;
{$ENDIF}

procedure TIdCustomIcmpClient.Send(const AHost: string; const APort: TIdPort;
  const ABuffer: TIdBytes);
var
  LBuffer : TIdBytes;
  LIP : String;
begin
  LBuffer := ABuffer;
  LIP := GStack.ResolveHost(AHost, IPVersion);
  GStack.WriteChecksum(Binding.Handle, LBuffer, 2, LIP, APort, IPVersion);
  FBinding.SendTo(LIP, APort, LBuffer, IPVersion);
end;

procedure TIdCustomIcmpClient.Send(const ABuffer: TIdBytes);
var
  LBuffer : TIdBytes;
  LIP : String;
begin
  LBuffer := ABuffer;
  LIP := GStack.ResolveHost(Host, IPVersion);
  GStack.WriteChecksum(Binding.Handle, LBuffer, 2, LIP, Port, IPVersion);
  FBinding.SendTo(LIP, Port, LBuffer, IPVersion);
end;

function TIdCustomIcmpClient.GetPacketSize: Integer;
begin
  Result := FPacketSize;
end;

procedure TIdCustomIcmpClient.SetPacketSize(const AValue: Integer);
begin
  if AValue < 0 then begin
    FPacketSize := 0;
  end else begin
    FPacketSize := IndyMin(AValue, MAX_PACKET_SIZE);
  end;
end;

procedure TIdCustomIcmpClient.InternalPing(const AIP, ABuffer: String; SequenceID: Word);
begin
  if SequenceID <> 0 then begin
    wSeqNo := SequenceID;
  end;
  PrepareEchoRequest(ABuffer);
  SendEchoRequest(AIP);
  GetEchoReply;
  Binding.CloseSocket;
  DoReply;
  Inc(wSeqNo); // SG 25/1/02: Only increase sequence number when finished.
end;

procedure TIdCustomIcmpClient.SendEchoRequest(const AIP: String);
begin
  Send(AIP, 0, FBufIcmp);
end;

{ TIdIcmpClient }

procedure TIdIcmpClient.Ping(const ABuffer: String; SequenceID: Word);
begin
  InternalPing(GStack.ResolveHost(Host, IPVersion), ABuffer, SequenceID);
end;

end.

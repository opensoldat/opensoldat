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
  Rev 1.5    2004.02.03 4:16:50 PM  czhower
  For unit name changes.

  Rev 1.4    2/1/2004 4:52:30 PM  JPMugaas
  Removed the rest of the Todo; items.

  Rev 1.3    2/1/2004 4:20:30 PM  JPMugaas
  Should work in Win32.  TODO: See about DotNET.

  Rev 1.2    2003.10.11 5:49:06 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.1    2003.09.30 1:23:00 PM  czhower
  Stack split for DotNet

  Rev 1.0    11/13/2002 08:45:36 AM  JPMugaas
}

unit IdRawFunctions;

interface

{$i IdCompilerDefines.inc}

uses
   IdGlobal, IdRawHeaders, IdStack;

// ARP
procedure IdRawBuildArp(const AHwAddressFormat, AProtocolFormat: UInt16;
  const AHwAddressLen, AProtocolLen: UInt8; const AnOpType: UInt16;
  ASenderHw: TIdEtherAddr; ASenderPr: TIdInAddr; ATargetHw: TIdEtherAddr;
  ATargetPr: TIdInAddr; const APayload: TIdBytes; var VBuffer: TIdBytes);

// DNS
procedure IdRawBuildDns(const AnId, AFlags, ANumQuestions, ANumAnswerRecs, ANumAuthRecs, ANumAddRecs: UInt16;
  const APayload: TIdBytes; var VBuffer: TIdBytes);

// Ethernet
procedure IdRawBuildEthernet(ADest, ASource: TIdEtherAddr; AType: UInt16;
  const APayload: TIdBytes; var VBuffer: TIdBytes);

// ICMP
procedure IdRawBuildIcmpEcho(AType, ACode: UInt8; AnId, ASeq: UInt16;
  const APayload: TIdBytes; var VBuffer: TIdBytes);
procedure IdRawBuildIcmpMask(AType, ACode: UInt8; AnId, ASeq: UInt16; AMask: UInt32;
  const APayload: TIdBytes; var VBuffer: TIdBytes);
procedure IdRawBuildIcmpRedirect(const AType, ACode: UInt8; AGateway: TIdInAddr;
  const AnOrigLen: UInt16; const AnOrigTos: UInt8; const AnOrigId, AnOrigFrag: UInt16;
  const AnOrigTtl, AnOrigProtocol: UInt8; AnOrigSource, AnOrigDest: TIdInAddr;
  const AnOrigPayload: TIdBytes; var VBuffer: TIdBytes);
procedure IdRawBuildIcmpTimeExceed(const AType, ACode: UInt8; const AnOrigLen: UInt16;
  const AnOrigTos: UInt8; const AnOrigId, AnOrigFrag: UInt16;
  const AnOrigTtl, AnOrigProtocol: UInt8; const AnOrigSource, AnOrigDest: TIdInAddr;
  const AnOrigPayload: TIdBytes; var VBuffer: TIdBytes);
procedure IdRawBuildIcmpTimestamp(const AType, ACode: UInt8; const AnId, ASeq: UInt16;
  const AnOtime, AnRtime, ATtime: TIdNetTime; const APayload: TIdBytes;
  var VBuffer: TIdBytes);
procedure IdRawBuildIcmpUnreach(AType, ACode: UInt8; AnOrigLen: UInt16;
  AnOrigTos: UInt8; AnOrigId, AnOrigFrag: UInt16;  AnOrigTtl, AnOrigProtocol: UInt8;
  AnOrigSource, AnOrigDest: TIdInAddr; const AnOrigPayload, APayloadSize: Integer;
  var VBuffer: TIdBytes);

// IGMP
procedure IdRawBuildIgmp(AType, ACode: UInt8; AnIp: TIdInAddr;
  const APayload: UInt16; var VBuffer: TIdBytes);

// IP
procedure IdRawBuildIp(ALen: UInt16; ATos: UInt8; AnId, AFrag: UInt16;
  ATtl, AProtocol: UInt8; ASource, ADest: TIdInAddr; const APayload: TIdBytes;
  var VBuffer: TIdBytes; const AIdx: Integer = 0);

// RIP
procedure IdRawBuildRip(const ACommand, AVersion: UInt8;
  const ARoutingDomain, AnAddressFamily, ARoutingTag: UInt16;
  const AnAddr, AMask, ANextHop, AMetric: UInt32;
  const APayload: TIdBytes; var VBuffer: TIdBytes);

// TCP
procedure IdRawBuildTcp(const ASourcePort, ADestPort: UInt16;
  const ASeq, AnAck: UInt32; const AControl: UInt8;
  const AWindowSize, AnUrgent: UInt16; const APayload: TIdBytes;
  var VBuffer: TIdBytes);

// UDP
procedure IdRawBuildUdp(const ASourcePort, ADestPort: UInt16;
  const APayload: TIdBytes; var VBuffer: TIdBytes);

implementation

uses
  SysUtils;

procedure IdRawBuildArp(const AHwAddressFormat, AProtocolFormat: UInt16;
  const AHwAddressLen, AProtocolLen: UInt8; const AnOpType: UInt16;
  ASenderHw: TIdEtherAddr; ASenderPr: TIdInAddr; ATargetHw: TIdEtherAddr;
  ATargetPr: TIdInAddr; const APayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrArp: TIdArpHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Id_ARP_HSIZE + Length(VBuffer);
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrArp := TIdArpHdr.Create;
  try
    HdrArp.arp_hrd := GStack.HostToNetwork(AHwAddressFormat);
    HdrArp.arp_pro := GStack.HostToNetwork(AProtocolFormat);
    HdrArp.arp_hln := AHwAddressLen;
    HdrArp.arp_pln := AProtocolLen;
    HdrArp.arp_op  := GStack.HostToNetwork(AnOpType);
    HdrArp.arp_sha.CopyFrom(ASenderHw);
    HdrArp.arp_spa.s_l := ASenderPr.s_l;
    HdrArp.arp_tha.CopyFrom(ATargetHw);
    HdrArp.arp_tpa.CopyFrom(ATargetPr);

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, Id_ICMP_ECHO_HSIZE, Length(APayload));
    end;

    // copy header
    LIdx := 0;
    HdrArp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrArp);
  end;
end;

procedure IdRawBuildDns(const AnId, AFlags, ANumQuestions, ANumAnswerRecs,
  ANumAuthRecs, ANumAddRecs: UInt16; const APayload: TIdBytes;
  var VBuffer: TIdBytes);
var
  HdrDns: TIdDnsHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Length(APayload) + Id_DNS_HSIZE;
  LLen := UInt32(Length(VBuffer));
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrDns := TIdDnsHdr.Create;
  try
    HdrDns.dns_id          := GStack.HostToNetwork(AnId);
    HdrDns.dns_flags       := GStack.HostToNetwork(AFlags);
    HdrDns.dns_num_q       := GStack.HostToNetwork(ANumQuestions);
    HdrDns.dns_num_answ_rr := GStack.HostToNetwork(ANumAnswerRecs);
    HdrDns.dns_num_auth_rr := GStack.HostToNetwork(ANumAuthRecs);
    HdrDns.dns_num_addi_rr := GStack.HostToNetwork(ANumAddRecs);

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, Id_DNS_HSIZE, Length(APayload));
    end;

    // copy header
    LIdx := 0;
    HdrDns.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrDns);
  end;
end;

procedure IdRawBuildEthernet(ADest, ASource: TIdEtherAddr; AType: UInt16;
  const APayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrEth: TIdEthernetHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // make sure VBuffer will be long enough
  LIdx := Length(ASource.Data) + Length(ADest.Data) + 2 + Length(APayload);
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrEth := TIdEthernetHdr.Create;
  try
    HdrEth.ether_dhost.CopyFrom(ADest);
    HdrEth.ether_shost.CopyFrom(ASource);
    HdrEth.ether_type := GStack.HostToNetwork(AType);

    // copy header
    LIdx := 0;
    HdrEth.WriteStruct(VBuffer, LIdx);

    // copy payload if present
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, LIdx, Length(APayload));
    end;
  finally
    FreeAndNil(HdrEth);
  end;
end;

// TODO: check nibbles in IP header
procedure IdRawBuildIp(ALen: UInt16; ATos: UInt8; AnId, AFrag: UInt16; ATtl, AProtocol: UInt8;
  ASource, ADest: TIdInAddr; const APayload: TIdBytes; var VBuffer: TIdBytes;
  const AIdx: Integer = 0);
var
  HdrIp: TIdIpHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Id_IP_HSIZE + Length(APayload) + AIdx;
  LLen := Length(VBuffer);
  if  LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIp := TIdIpHdr.Create;
  try
    HdrIp.ip_verlen := (4 shl 4) + (Id_IP_HSIZE div 4);     // IPv4 shl 4, 20 bytes div 4
    HdrIp.ip_tos    := ATos;
    HdrIp.ip_len    := GStack.HostToNetwork(UInt16(ALen + Id_IP_HSIZE));
    HdrIp.ip_id     := GStack.HostToNetwork(AnId);
    HdrIp.ip_off    := GStack.HostToNetwork(AFrag);
    HdrIp.ip_ttl    := ATtl;
    HdrIp.ip_p      := AProtocol;
    HdrIp.ip_sum    := 0;                                     // do checksum later
    HdrIp.ip_src.CopyFrom(ASource);
    HdrIp.ip_dst.CopyFrom(ADest);

    // copy header
    LIdx := AIdx;
    HdrIp.WriteStruct(VBuffer, LIdx);

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, LIdx, Length(APayload));
    end;
  finally
    FreeANdNil(HdrIp);
  end;
end;

procedure IdRawBuildIcmpEcho(AType, ACode: UInt8; AnId, ASeq: UInt16;
  const APayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrIcmp: TIdIcmpHdr;
  LIdx, LLen : UInt32;
begin
  // check input
  LIdx := Id_ICMP_ECHO_HSIZE + Length(APayload);
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIcmp := TIdIcmpHdr.Create;
  try
    HdrIcmp.icmp_type := AType;
    HdrIcmp.icmp_code := ACode;
    HdrIcmp.icmp_hun.echo_id := GStack.HostToNetwork(AnId);
    HdrIcmp.icmp_hun.echo_seq := GStack.HostToNetwork(ASeq);

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, Id_ICMP_ECHO_HSIZE, Length(APayload));
    end;

    // copy header
    LIdx := 0;
    HdrIcmp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrIcmp);
  end;
end;

type
  TIdICMPMask = class(TIdICMPHdr)
  protected
    Ficmp_mask: UInt32;
    function GetBytesLen: UInt32; override;
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property icmp_mask: UInt32 read Ficmp_mask write Ficmp_mask;
  end;

function TIdICMPMask.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + 4;
end;

procedure TIdICMPMask.ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Ficmp_mask := BytesToUInt32(ABytes, VIndex);
  Inc(VIndex, 4);
end;

procedure TIdICMPMask.WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdUInt32(Ficmp_mask, VBytes, VIndex);
  Inc(VIndex, 4);
end;

procedure IdRawBuildIcmpMask(AType, ACode: UInt8; AnId, ASeq: UInt16; AMask: UInt32;
  const APayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrIcmp: TIdICMPMask;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Id_ICMP_MASK_HSIZE + Length(APayload);
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIcmp := TIdICMPMask.Create;
  try
    HdrIcmp.icmp_type         := AType;
    HdrIcmp.icmp_code         := ACode;
    HdrIcmp.icmp_hun.echo_id  := GStack.HostToNetwork(AnId);
    HdrIcmp.icmp_hun.echo_seq := GStack.HostToNetwork(ASeq);
    HdrIcmp.icmp_mask         := GStack.HostToNetwork(AMask);

    // copy header
    LIdx := 0;
    HdrIcmp.WriteStruct(VBuffer, LIdx);

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, LIdx, Length(APayload));
    end;
  finally
    FreeAndNil(HdrIcmp);
  end;
end;

procedure IdRawBuildIcmpUnreach(AType, ACode: UInt8; AnOrigLen: UInt16;
  AnOrigTos: UInt8; AnOrigId, AnOrigFrag: UInt16; AnOrigTtl, AnOrigProtocol: UInt8;
  AnOrigSource, AnOrigDest: TIdInAddr; const AnOrigPayload, APayloadSize: Integer;
  var VBuffer: TIdBytes);
var
  HdrIcmp: TIdIcmpHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Id_ICMP_UNREACH_HSIZE + Id_IP_HSIZE + 2;
  LLen :=  Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIcmp := TIdIcmpHdr.Create;
  try
    HdrIcmp.icmp_type         := AType;
    HdrIcmp.icmp_code         := ACode;
    HdrIcmp.icmp_hun.echo_id  := 0;
    HdrIcmp.icmp_hun.echo_seq := 0;

    // attach original header
    IdRawBuildIp(0, AnOrigTos, AnOrigId, AnOrigFrag, AnOrigTtl, AnOrigProtocol,
      AnOrigSource, AnOrigDest, ToBytes(AnOrigPayload), VBuffer, Id_ICMP_UNREACH_HSIZE);

    // copy header
    LIdx := 0;
    HdrIcmp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrIcmp);
  end;
end;

procedure IdRawBuildIcmpTimeExceed(const AType, ACode: UInt8; const AnOrigLen: UInt16;
  const AnOrigTos: UInt8; const AnOrigId, AnOrigFrag: UInt16;
  const AnOrigTtl, AnOrigProtocol: UInt8; const AnOrigSource, AnOrigDest: TIdInAddr;
  const AnOrigPayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrIcmp: TIdIcmpHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Id_ICMP_TIMEXCEED_HSIZE + Id_IP_HSIZE + Length(AnOrigPayload);
  Llen := Length(VBuffer);
  if Llen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIcmp := TIdIcmpHdr.Create;
  try
    HdrIcmp.icmp_type         := AType;
    HdrIcmp.icmp_code         := ACode;
    HdrIcmp.icmp_hun.echo_id  := 0;
    HdrIcmp.icmp_hun.echo_seq := 0;

    // attach original header
    IdRawBuildIp(0, AnOrigTos, AnOrigId, AnOrigFrag, AnOrigTtl, AnOrigProtocol,
      AnOrigSource, AnOrigDest, AnOrigPayload, VBuffer, Id_ICMP_TIMEXCEED_HSIZE);

    // copy header
    LIdx := 0;
    HdrIcmp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrIcmp);
  end;
end;

type
  TIdIcmpTS = class(TIdIcmpHdr)
  protected
    Ficmp_dun: TIdicmp_dun;
    function GetBytesLen: UInt32; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32); override;
    property icmp_dun: TIdicmp_dun read Ficmp_dun;
  end;

constructor TIdIcmpTS.Create;
begin
  inherited Create;
  Ficmp_dun := TIdicmp_dun.Create;
end;

destructor TIdIcmpTS.Destroy;
begin
  Ficmp_dun.Free;
  inherited Destroy;
end;

function TIdIcmpTS.GetBytesLen: UInt32;
begin
  Result := inherited GetBytesLen + Ficmp_dun.BytesLen;
end;

procedure TIdIcmpTS.ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  Ficmp_dun.ReadStruct(ABytes, VIndex);
end;

procedure TIdIcmpTS.WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  Ficmp_dun.WriteStruct(VBytes, VIndex);
end;

procedure IdRawBuildIcmpTimestamp(const AType, ACode: UInt8; const AnId, ASeq: UInt16;
  const AnOtime, AnRtime, ATtime: TIdNetTime; const APayload: TIdBytes;
  var VBuffer: TIdBytes);
var
  HdrIcmp: TIdIcmpTS;
  LIdx, LLen : UInt32;
begin
  // check input
  LIdx := Id_ICMP_TS_HSIZE + Length(APayload);
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIcmp := TIdIcmpTS.Create;
  try
    HdrIcmp.icmp_type             := AType;
    HdrIcmp.icmp_code             := ACode;
    HdrIcmp.icmp_hun.echo_id      := GStack.HostToNetwork(AnId);
    HdrIcmp.icmp_hun.echo_seq     := GStack.HostToNetwork(ASeq);
    HdrIcmp.icmp_dun.ts_otime     := GStack.HostToNetwork(AnOtime);      // original timestamp
    HdrIcmp.icmp_dun.ts_rtime     := GStack.HostToNetwork(AnRtime);      // receive timestamp
    HdrIcmp.icmp_dun.ts_ttime     := GStack.HostToNetwork(ATtime);       // transmit timestamp

    // copy header
    LIdx := 0;
    HdrIcmp.WriteStruct(VBuffer, LIdx);

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, LIdx, Length(APayload));
    end;
  finally
    FreeAndNil(HdrIcmp);
  end;
end;

procedure IdRawBuildIcmpRedirect(const AType, ACode: UInt8; AGateway: TIdInAddr;
  const AnOrigLen: UInt16; const AnOrigTos: UInt8; const AnOrigId, AnOrigFrag: UInt16;
  const AnOrigTtl, AnOrigProtocol: UInt8; AnOrigSource, AnOrigDest: TIdInAddr;
  const AnOrigPayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrIcmp: TIdIcmpHdr;
  LIdx, LLen : UInt32;
begin
  // check input
  LIdx := Id_ICMP_REDIRECT_HSIZE + Id_IP_HSIZE + Length(AnOrigPayload);
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIcmp := TIdIcmpHdr.Create;
  try
    HdrIcmp.icmp_type             := AType;
    HdrIcmp.icmp_code             := ACode;
    HdrIcmp.icmp_hun.gateway_s_b1 := AGateway.s_l;      // gateway address

    // attach original header
    IdRawBuildIp(0, AnOrigTos, AnOrigId, AnOrigFrag, AnOrigTtl, AnOrigProtocol,
      AnOrigSource, AnOrigDest, AnOrigPayload, VBuffer, Id_ICMP_REDIRECT_HSIZE);

    // copy header
    LIdx := 0;
    HdrIcmp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrIcmp);
  end;
end;

procedure IdRawBuildIgmp(AType, ACode: UInt8; AnIp: TIdInAddr;
  const APayload: UInt16; var VBuffer: TIdBytes);
var
  HdrIgmp: TIdIgmpHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := 2 + Id_IGMP_HSIZE;
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrIgmp := TIdIgmpHdr.Create;
  try
    HdrIgmp.igmp_type         := AType;
    HdrIgmp.igmp_code         := ACode;
    HdrIgmp.igmp_sum          := 0;
    HdrIgmp.igmp_group.s_l    := AnIp.s_l;      // group address or 0

    // copy payload
    CopyTIdUInt16(APayload, VBuffer, Id_IGMP_HSIZE);

    // copy header
    LIdx := 0;
    HdrIgmp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrIgmp);
  end;
end;

procedure IdRawBuildRip(const ACommand, AVersion: UInt8;
  const ARoutingDomain, AnAddressFamily, ARoutingTag: UInt16;
  const AnAddr, AMask, ANextHop, AMetric: UInt32;
  const APayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrRip: TIdRipHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Id_RIP_HSIZE + Length(APayload);
  LLen := Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrRip := TIdRipHdr.Create;
  try
    HdrRip.rip_cmd      := ACommand;
    HdrRip.rip_ver      := AVersion;
    HdrRip.rip_rd       := GStack.HostToNetwork(ARoutingDomain);
    HdrRip.rip_af       := GStack.HostToNetwork(AnAddressFamily);
    HdrRip.rip_rt       := GStack.HostToNetwork(ARoutingTag);
    HdrRip.rip_addr     := GStack.HostToNetwork(AnAddr);
    HdrRip.rip_mask     := GStack.HostToNetwork(AMask);
    HdrRip.rip_next_hop := GStack.HostToNetwork(ANextHop);
    HdrRip.rip_metric   := GStack.HostToNetwork(AMetric);

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, Id_RIP_HSIZE, Length(APayload));
    end;

    // copy header
    LIdx := 0;
    HdrRip.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrRip);
  end;
end;

// TODO: check nibbles in TCP header
procedure IdRawBuildTcp(const ASourcePort, ADestPort: UInt16;
  const ASeq, AnAck: UInt32; const AControl: UInt8;
  const AWindowSize, AnUrgent: UInt16; const APayload: TIdBytes;
  var VBuffer: TIdBytes);
var
  HdrTcp: TIdTcpHdr;
  LIdx, LLen: UInt32;
begin
  // check input
  LIdx := Id_TCP_HSIZE + Length(VBuffer);
  LLen :=  Length(VBuffer);
  if LLen < LIdx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrTcp := TIdTcpHdr.Create;
  try
    HdrTcp.tcp_sport    := GStack.HostToNetwork(ASourcePort);
    HdrTcp.tcp_dport    := GStack.HostToNetwork(ADestPort);
    HdrTcp.tcp_seq      := GStack.HostToNetwork(ASeq);
    HdrTcp.tcp_ack      := GStack.HostToNetwork(AnAck);                // acknowledgement number
    HdrTcp.tcp_flags    := AControl;                              // control flags
    HdrTcp.tcp_x2off    := ((Id_TCP_HSIZE div 4) shl 4) + 0;      // 20 bytes div 4, x2 unused
    HdrTcp.tcp_win      := GStack.HostToNetwork(AWindowSize);          // window size
    HdrTcp.tcp_sum      := 0;
    HdrTcp.tcp_urp      := AnUrgent;                              // urgent pointer

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, Id_TCP_HSIZE, Length(APayload));
    end;

    // copy header
    LIdx := 0;
    HdrTcp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrTcp);
  end;
end;

procedure IdRawBuildUdp(const ASourcePort, ADestPort: UInt16;
  const APayload: TIdBytes; var VBuffer: TIdBytes);
var
  HdrUdp: TIdUdpHdr;
  LIdx: UInt32;
  LLen : UInt32;
begin
  // check input
  LIdx := Id_UDP_HSIZE + Length(APayload);
  LLen := Length(VBuffer);
  if LLen < Lidx then begin
    SetLength(VBuffer, LIdx);
  end;

  // construct header
  HdrUdp := TIdUdpHdr.Create;
  try
    HdrUdp.udp_sport    := GStack.HostToNetwork(ASourcePort);
    HdrUdp.udp_dport    := GStack.HostToNetwork(ADestPort);
    //LIdx should be okay here since we set that to the packet length earlier
    HdrUdp.udp_ulen     := GStack.HostToNetwork(LIdx);
    HdrUdp.udp_sum      := 0;

    // copy payload
    if Length(APayload) > 0 then begin
      CopyTIdBytes(APayload, 0, VBuffer, Id_UDP_HSIZE, Length(APayload));
    end;

    // copy header
    LIdx := 0;
    HdrUdp.WriteStruct(VBuffer, LIdx);
  finally
    FreeAndNil(HdrUdp);
  end;
end;

end.

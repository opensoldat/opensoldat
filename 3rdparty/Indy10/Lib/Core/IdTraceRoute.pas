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

unit IdTraceRoute;

interface

{$i IdCompilerDefines.inc}

uses
  IdIcmpClient, IdRawBase, IdRawClient, IdThread;

type
  TIdTraceRoute = class(TIdCustomICMPClient)
  protected
    FResolveHostNames : Boolean;
    procedure DoReply; override;
  public
    procedure Trace;
  published
    {$IFDEF DOTNET_2_OR_ABOVE}
    property IPVersion;
    {$ENDIF}
    property PacketSize;
    property ReceiveTimeout;
    property ResolveHostNames : Boolean read FResolveHostNames write FResolveHostNames;
    property OnReply;
  end;

implementation

uses
  IdGlobal, IdStack;

{ TIdTraceRoute }

procedure TIdTraceRoute.DoReply;
begin
  if FResolveHostNames and
    (PosInStrArray(FReplyStatus.FromIpAddress, ['0.0.0.0', '::0']) = -1) then {do not localize}
  begin
    //resolve IP to hostname
    try
      FReplyStatus.HostName := GStack.HostByAddress(FReplyStatus.FromIpAddress, FBinding.IPVersion);
    except
      {
      We do things this way because we are likely have a reverse DNS
      failure if you have a computer with IP address and no DNS name at all.
      }
      FReplyStatus.HostName := FReplyStatus.FromIpAddress;
    end;
  end;
  inherited DoReply;
end;

procedure TIdTraceRoute.Trace;
//In traceroute, there are a maximum of thirty echo request packets.  You start
//requests with a TTL of one and keep sending them until you get to thirty or you
//get an echo response (whatever comes sooner).
var
  i : Integer;
  lSeq : UInt32;
  LTTL : Integer;
  LIPAddr : String;
begin

//  PacketSize := 64;
//We do things this way because we only want to resolve the destination host name
//only one time.  Otherwise, there's a performance penalty for earch DNS resolve.
  LIPAddr := GStack.ResolveHost(FHost, FBinding.IPVersion);
  LSeq := $1;
  LTTL := 1;
  TTL := LTTL;
  for i := 1 to 30 do
  begin
    ReplyStatus.PacketNumber := i;
    InternalPing(LIPAddr, '', LSeq);
    case ReplyStatus.ReplyStatusType of
      rsErrorTTLExceeded,
      rsTimeout : ;
    else
      Break;
    end;
    Inc(LTTL);
    TTL := LTTL;
    LSeq := LSeq * 2;
  end;
end;

end.

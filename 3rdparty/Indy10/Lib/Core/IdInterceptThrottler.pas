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
  Rev 1.2    2004.02.03 4:17:18 PM  czhower
  For unit name changes.

  Rev 1.1    2003.10.19 12:10:00 AM  czhower
  Changed formula to be accurate with smaller numbers.

  Rev 1.0    2003.10.18 11:32:00 PM  czhower
  Initial checkin

  Rev 1.1    2003.10.14 1:27:16 PM  czhower
  Uupdates + Intercept support

  Rev 1.0    2003.10.13 6:40:40 PM  czhower
  Moved from root

  Rev 1.0    11/13/2002 07:55:12 AM  JPMugaas
}

unit IdInterceptThrottler;

interface
{$i IdCompilerDefines.inc}

uses
  IdComponent, IdIntercept, IdGlobal;

type
  TIdInterceptThrottler = class(TIdConnectionIntercept)
  protected
    FBitsPerSec: Integer;
    FRecvBitsPerSec: Integer;
    FSendBitsPerSec: Integer;
    procedure SetBitsPerSec(AValue: Integer);
  public
    procedure Receive(var ABuffer: TIdBytes); override;
    procedure Send(var ABuffer: TIdBytes); override;
  published
    property BitsPerSec: Integer read FBitsPerSec write SetBitsPerSec;
    property RecvBitsPerSec: Integer read FRecvBitsPerSec write FRecvBitsPerSec;
    property SendBitsPerSec: Integer read FSendBitsPerSec write FSendBitsPerSec;
  end;

implementation

uses
  IdAntiFreezeBase, IdException;

{ TIdInterceptThrottler }

procedure TIdInterceptThrottler.Receive(var ABuffer: TIdBytes);
var
  LInterval: Int64;
begin
  inherited Receive(ABuffer);
  if RecvBitsPerSec > 0 then begin
    LInterval := (Int64(Length(ABuffer)) * 8 * 1000) div RecvBitsPerSec;
    while LInterval > MaxInt do begin
      TIdAntiFreezeBase.Sleep(MaxInt);
      Dec(LInterval, MaxInt);
    end;
    TIdAntiFreezeBase.Sleep(Integer(LInterval));
  end;
end;

procedure TIdInterceptThrottler.Send(var ABuffer: TIdBytes);
var
  LInterval: Int64;
begin
  inherited Send(ABuffer);
  if SendBitsPerSec > 0 then begin
    LInterval := (Int64(Length(ABuffer)) * 8 * 1000) div SendBitsPerSec;
    while LInterval > MaxInt do begin
      TIdAntiFreezeBase.Sleep(MaxInt);
      Dec(LInterval, MaxInt);
    end;
    TIdAntiFreezeBase.Sleep(Integer(LInterval));
  end;
end;

procedure TIdInterceptThrottler.SetBitsPerSec(AValue: Integer);
begin
  FBitsPerSec := AValue;
  FRecvBitsPerSec := AValue;
  FSendBitsPerSec := AValue;
end;

end.


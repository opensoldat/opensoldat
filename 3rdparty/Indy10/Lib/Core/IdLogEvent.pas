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
  Rev 1.2    2004.05.20 12:34:28 PM  czhower
  Removed more non .NET compatible stream read and writes

  Rev 1.1    2003.10.17 8:17:22 PM  czhower
  Removed const

  Rev 1.0    11/13/2002 07:56:08 AM  JPMugaas
}

unit IdLogEvent;

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  IdLogBase, IdBaseComponent, Classes;

type
  TLogItemStatusEvent = procedure(ASender: TComponent; const AText: string) of object;
  TLogItemDataEvent = procedure(ASender: TComponent; const AText, AData: string) of object;

  TIdLogEvent = class(TIdLogBase)
  protected
    FOnReceived: TLogItemDataEvent;
    FOnSent: TLogItemDataEvent;
    FOnStatus: TLogItemStatusEvent;
    //
    procedure LogStatus(const AText: string); override;
    procedure LogReceivedData(const AText, AData: string); override;
    procedure LogSentData(const AText, AData: string); override;
  public
  published
    property OnReceived: TLogItemDataEvent read FOnReceived write FOnReceived;
    property OnSent: TLogItemDataEvent read FOnSent write FOnSent;
    property OnStatus: TLogItemStatusEvent read FOnStatus write FOnStatus;
  end;

implementation

{ TIdLogEvent }

procedure TIdLogEvent.LogReceivedData(const AText, AData: string);
begin
  if Assigned(OnReceived) then begin
    OnReceived(Self, AText, AData);
  end;
end;

procedure TIdLogEvent.LogSentData(const AText, AData: string);
begin
  if Assigned(OnSent) then begin
    OnSent(Self, AText, AData);
  end;
end;

procedure TIdLogEvent.LogStatus(const AText: string);
begin
  if Assigned(OnStatus) then begin
    OnStatus(Self, AText);
  end;
end;

end.

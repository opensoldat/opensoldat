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
  Rev 1.4    8/6/04 12:21:28 AM  RLebeau
  Removed TIdLogDebugTarget type, not used anywhere

  Rev 1.3    2004.02.03 4:17:16 PM  czhower
  For unit name changes.

  Rev 1.2    2003.10.17 8:17:22 PM  czhower
  Removed const

  Rev 1.1    4/22/2003 4:34:22 PM  BGooijen
  DebugOutput is now in IdGlobal

  Rev 1.0    11/13/2002 07:56:02 AM  JPMugaas
}

unit IdLogDebug;

interface
{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode
uses
  IdLogBase;

type
  TIdLogDebug = class(TIdLogBase)
  protected
    procedure LogStatus(const AText: string); override;
    procedure LogReceivedData(const AText, AData: string); override;
    procedure LogSentData(const AText, AData: string); override;
  end;

implementation

uses
  IdGlobal;

{ TIdLogDebug }

procedure TIdLogDebug.LogReceivedData(const AText, AData: string);
begin
  DebugOutput('Recv ' + AText + ': ' + AData);    {Do not Localize}
end;

procedure TIdLogDebug.LogSentData(const AText, AData: string);
begin
  DebugOutput('Sent ' + AText + ': ' + AData);    {Do not Localize}
end;

procedure TIdLogDebug.LogStatus(const AText: string);
begin
  DebugOutput('Stat ' + AText);    {Do not Localize}
end;

end.

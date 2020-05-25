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
  Rev 1.69    12/2/2004 9:26:42 PM  JPMugaas
  Bug fix.
}

unit IdTCPServer;

interface
{$i IdCompilerDefines.inc}

uses IdCustomTCPServer;

type
  EIdTCPNoOnExecute = class(EIdTCPServerError);

  TIdTCPServer = class(TIdCustomTCPServer)
  protected
     procedure CheckOkToBeActive;  override;
  published
    property OnExecute;
  end;

implementation

uses IdResourceStringsCore;

{ TIdTCPServer }

procedure TIdTCPServer.CheckOkToBeActive;
begin
  inherited CheckOkToBeActive;
  if not Assigned(FOnExecute) then begin
    raise EIdTCPNoOnExecute.Create(RSNoOnExecute);
  end;
end;

end.

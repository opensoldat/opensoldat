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
  Rev 1.0    11/13/2002 08:45:32 AM  JPMugaas
}

unit IdRawClient;

interface
{$i IdCompilerDefines.inc}

uses
  IdGlobal,
  IdRawBase;

type
  TIdRawClient = class(TIdRawBase)

  published
    property ReceiveTimeout;
    property Host;
    property Port;
    property Protocol;
    property ProtocolIPv6;
    property IPVersion;
  end;

implementation

{ TIdRawClient }

end.

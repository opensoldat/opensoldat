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
  Rev 1.9    10/26/2004 8:45:26 PM  JPMugaas
  Should compile.

  Rev 1.8    10/26/2004 8:42:58 PM  JPMugaas
  Should be more portable with new references to TIdStrings and TIdStringList.

  Rev 1.7    5/19/2004 10:44:28 PM  DSiders
  Corrected spelling for TIdIPAddress.MakeAddressObject method.

  Rev 1.6    2/3/2004 11:34:26 AM  JPMugaas
  Should compile.

  Rev 1.5.1.0    2/3/2004 11:32:26 AM  JPMugaas
  Should compile.

  Rev 1.5    2/1/2004 2:44:20 AM  JPMugaas
  Bindings editor should be fully functional including IPv6 support.

  Rev 1.4    2/1/2004 1:03:34 AM  JPMugaas
  This now work properly in both Win32 and DotNET.  The behavior had to change
  in DotNET because of some missing functionality and because implementing that
  functionality creates more problems than it would solve.

  Rev 1.3    2003.12.31 10:42:22 PM  czhower
  Warning removed

  Rev 1.2    10/15/2003 10:12:32 PM  DSiders
  Added localization comments.

  Rev 1.1    2003.10.11 5:47:46 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.0    11/13/2002 08:43:58 AM  JPMugaas
}

unit IdDsnPropEdBinding;

{
  Design Note:  It turns out that in DotNET, there are no services file functions and
  IdPorts does not work as expected in DotNET.  It is probably possible to read the
  services file ourselves but that creates some portability problems as the placement
 is different in every operating system.

  e.g.

  Linux and Unix-like systems - /etc
  Windows 95, 98, and ME - c:\windows
  Windows NT systems - c:\winnt\system32\drivers\etc

  Thus, it will undercut whatever benefit we could get with DotNET.

  About the best I could think of is to use an edit control because
  we can't offer anything from the services file in DotNET.

  TODO:  Maybe there might be a way to find the location in a more elegant
  manner than what I described.
}

interface

{$I IdCompilerDefines.inc}

{$IFDEF WIDGET_WINFORMS}
{$R 'IdDsnPropEdBindingNET.TIdDsnPropEdBindingNET.resources' 'IdDsnPropEdBindingNET.resx'}
{$ENDIF}

uses
  Classes,
  IdSocketHandle;

{
Design Note:  It turns out that in DotNET, there are no services file functions and IdPorts
does not work as expected in DotNET.  It is probably possible to read the services
file ourselves but that creates some portability problems as the placement is different
in every operating system.

e.g.

Linux and Unix-like systems - /etc
Windows 95, 98, and ME - c:\windows
Windows NT systems - c:\winnt\system32\drivers\etc

Thus, it will undercut whatever benefit we could get with DotNET.

About the best I could think of is to use an edit control because
we can't offer anything from the services file in DotNET.

TODO:  Maybe there might be a way to find the location in a more eligant
manner than what I described.
}

procedure FillHandleList(const AList: string; ADest: TIdSocketHandles);
function GetListValues(const ASocketHandles : TIdSocketHandles) : String;

implementation

uses
  {$IFDEF WIDGET_WINFORMS}
  IdDsnPropEdBindingNET;
  {$ELSE}
  IdDsnPropEdBindingVCL;
  {$ENDIF}

procedure FillHandleList(const AList: string; ADest: TIdSocketHandles);
begin
  {$IFDEF WIDGET_WINFORMS}
  IdDsnPropEdBindingNET.FillHandleList(AList,ADest);
  {$ELSE}
  IdDsnPropEdBindingVCL.FillHandleList(AList,ADest);
  {$ENDIF}
end;

function GetListValues(const ASocketHandles : TIdSocketHandles) : String;
begin
  {$IFDEF WIDGET_WINFORMS}
  Result := IdDsnPropEdBindingNET.GetListValues(ASocketHandles);
  {$ELSE}
  Result := IdDsnPropEdBindingVCL.GetListValues(ASocketHandles);
  {$ENDIF}
end;

end.

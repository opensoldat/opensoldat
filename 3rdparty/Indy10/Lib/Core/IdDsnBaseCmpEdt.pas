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
  Rev 1.2    9/5/2004 2:08:16 PM  JPMugaas
  Should work in D9 NET.

  Rev 1.1    2/3/2004 11:42:50 AM  JPMugaas
  Fixed for new design.

  Rev 1.0    11/13/2002 08:43:16 AM  JPMugaas
}

unit IdDsnBaseCmpEdt;

{$I IdCompilerDefines.inc}

interface

uses
  {$IFDEF DOTNET}
  Borland.Vcl.Design.DesignIntF,
  Borland.Vcl.Design.DesignEditors
  {$ELSE}
    {$IFDEF FPC}
  ComponentEditors
    {$ELSE}
      {$IFDEF VCL_6_OR_ABOVE}
  DesignIntf,
  DesignEditors
      {$ELSE}
  Dsgnintf
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  ;

type
  {$IFDEF FPC}
  TIdBaseComponentEditor = class(TDefaultComponentEditor)
  {$ELSE}
  TIdBaseComponentEditor = class(TDefaultEditor)
  {$ENDIF}
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  IdAbout,
  IdGlobal,
  IdDsnCoreResourceStrings,
  SysUtils;

{ TIdBaseComponentEditor }

procedure TIdBaseComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : ShowAboutBox(RSAAboutBoxCompName, gsIdVersion);
  end;
end;

function TIdBaseComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := IndyFormat(RSAAboutMenuItemName, [gsIdVersion]);
  end;
end;

function TIdBaseComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.


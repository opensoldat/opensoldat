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
  Rev 1.4    12/10/2004 15:36:40  HHariri
  Fix so it works with D8 too

  Rev 1.3    9/5/2004 2:08:14 PM  JPMugaas
  Should work in D9 NET.

  Rev 1.2    2/3/2004 11:42:52 AM  JPMugaas
  Fixed for new design.

  Rev 1.1    2/1/2004 2:44:20 AM  JPMugaas
  Bindings editor should be fully functional including IPv6 support.

  Rev 1.0    11/13/2002 08:41:18 AM  JPMugaas
}

unit IdCoreDsnRegister;

interface

{$I IdCompilerDefines.inc}

uses
  {$IFDEF DOTNET}
  Borland.Vcl.Design.DesignIntF,
  Borland.Vcl.Design.DesignEditors
  {$ELSE}
    {$IFDEF FPC}
  PropEdits,
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

  {$IFDEF FPC}
  TIdPropEdBinding = class(TPropertyEditor)
  protected
    FValue : String;
    property Value : String read FValue write FValue;
  {$ELSE}
  TIdPropEdBinding = class(TClassProperty)
  {$ENDIF}
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

// Procs
  procedure Register;

implementation

uses
  Classes,
  {$IFDEF WIDGET_WINFORMS}
  IdDsnPropEdBindingNET,
  IdAboutDotNET,
  {$ELSE}
  IdDsnPropEdBindingVCL,
  IdAboutVCL,
  {$ENDIF}
  {$IFDEF DCC}
    {$IFDEF VCL_2005_OR_ABOVE}
  ToolsAPI,
  SysUtils,
    {$ENDIF}
  {$ENDIF}
  IdDsnCoreResourceStrings,
  IdBaseComponent,
  IdComponent,
  IdGlobal,
  IdStack,
  IdSocketHandle;

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

type
  {$IFDEF WIDGET_WINFORMS}
   TIdPropEdBindingEntry = TIdDsnPropEdBindingNET;
  {$ELSE}
  TIdPropEdBindingEntry = TIdDsnPropEdBindingVCL;
  {$ENDIF}

procedure TIdPropEdBinding.Edit;
var
  pSockets: TIdSocketHandles;
  pEntry: TIdPropEdBindingEntry;
begin
  inherited Edit;

  {$IFNDEF DOTNET}
  pSockets := TIdSocketHandles(
    {$IFDEF CPU64}
    GetInt64Value
    {$ELSE}
    GetOrdValue
    {$ENDIF}
  );
  {$ELSE}
  pSockets := GetObjValue as TIdSocketHandles;
  {$ENDIF}

  pEntry := TIdPropEdBindingEntry.Create;
  try
    pEntry.Caption := TComponent(GetComponent(0)).Name;
    pEntry.DefaultPort := pSockets.DefaultPort;
    Value := GetListValues(pSockets);
    pEntry.SetList(Value);
    if pEntry.Execute then
    begin
      Value := pEntry.GetList;
      FillHandleList(Value, pSockets);
    end;
  finally
    pEntry.Free;
  end;
end;

function TIdPropEdBinding.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TIdPropEdBinding.GetValue: string;
var
  pSockets: TIdSocketHandles;
begin
  {$IFNDEF DOTNET}
  pSockets := TIdSocketHandles(
    {$IFDEF CPU64}
    GetInt64Value
    {$ELSE}
    GetOrdValue
    {$ENDIF}
  );
  {$ELSE}
  pSockets := GetObjValue as TIdSocketHandles;
  {$ENDIF}
  Result := GetListValues(pSockets);
end;

procedure TIdPropEdBinding.SetValue(const Value: string);
var
  pSockets: TIdSocketHandles;
begin
  inherited SetValue(Value);
  {$IFNDEF DOTNET}
  pSockets := TIdSocketHandles(
    {$IFDEF CPU64}
    GetInt64Value
    {$ELSE}
    GetOrdValue
    {$ENDIF}
  );
  {$ELSE}
  pSockets := GetObjValue as TIdSocketHandles;
  {$ENDIF}
  pSockets.BeginUpdate;
  try
    FillHandleList(Value, pSockets);
  finally
    pSockets.EndUpdate;
  end;
end;

{ TIdBaseComponentEditor }

procedure TIdBaseComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : TfrmAbout.ShowDlg;
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

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TIdSocketHandles), nil, '', TIdPropEdBinding);    {Do not Localize}
  RegisterComponentEditor(TIdBaseComponent, TIdBaseComponentEditor);
end;

{$IFDEF DCC}
  {$IFDEF VCL_2005_OR_ABOVE}
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = -1;

procedure RegisterAboutBox;
begin
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
  begin
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(
      RSAAboutBoxCompName + ' ' + gsIdVersion,
      RSAAboutBoxDescription + sLineBreak + sLineBreak
        + RSAAboutBoxCopyright + sLineBreak + sLineBreak
        + RSAAboutBoxPleaseVisit + sLineBreak + RSAAboutBoxIndyWebsite,
      0,
      False,
      RSAAboutBoxLicences,
      '');
  end;
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> -1) and (AboutBoxServices <> nil) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
  end;
end;

initialization
  RegisterAboutBox;
finalization
  UnregisterAboutBox;

  {$ENDIF}
{$ENDIF}

end.

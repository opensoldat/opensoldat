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
  Rev 1.4    2004.02.03 5:43:52 PM  czhower
  Name changes

  Rev 1.3    1/21/2004 3:11:06 PM  JPMugaas
  InitComponent

  Rev 1.2    10/26/2003 09:11:50 AM  JPMugaas
  Should now work in NET.

  Rev 1.1    2003.10.12 4:03:56 PM  czhower
  compile todos

  Rev 1.0    11/13/2002 07:55:16 AM  JPMugaas
}

unit IdIPMCastBase;

interface

{$I IdCompilerDefines.inc}
//here to flip FPC into Delphi mode

uses
  {$IFDEF WORKAROUND_INLINE_CONSTRUCTORS}
  Classes,
  {$ENDIF}
  IdComponent, IdException, IdGlobal, IdSocketHandle,
  IdStack;

(*$HPPEMIT '#if defined(_VCL_ALIAS_RECORDS)' *)
(*$HPPEMIT '#if !defined(UNICODE)' *)
(*$HPPEMIT '#pragma alias "@Idipmcastbase@TIdIPMCastBase@SetPortA$qqrxi"="@Idipmcastbase@TIdIPMCastBase@SetPort$qqrxi"' *)
(*$HPPEMIT '#else' *)
(*$HPPEMIT '#pragma alias "@Idipmcastbase@TIdIPMCastBase@SetPortW$qqrxi"="@Idipmcastbase@TIdIPMCastBase@SetPort$qqrxi"' *)
(*$HPPEMIT '#endif' *)
(*$HPPEMIT '#endif' *)

const
  IPMCastLo = 224;
  IPMCastHi = 239;

type
  TIdIPMv6Scope = ( IdIPv6MC_InterfaceLocal,
{  Interface-Local scope spans only a single interface on a node
   and is useful only for loopback transmission of multicast.}
   IdIPv6MC_LinkLocal,
{  Link-Local multicast scope spans the same topological region as
the corresponding unicast scope. }
   IdIPv6MC_AdminLocal,
{   Admin-Local scope is the smallest scope that must be
administratively configured, i.e., not automatically derived
from physical connectivity or other, non-multicast-related
configuration.}
    IdIPv6MC_SiteLocal,
{    Site-Local scope is intended to span a single site.   }
    IdIPv6MC_OrgLocal,
{Organization-Local scope is intended to span multiple sites
belonging to a single organization.}
    IdIPv6MC_Global);

  TIdIPMCValidScopes = 0..$F;

  TIdIPMCastBase = class(TIdComponent)
  protected
    FDsgnActive: Boolean;
    FMulticastGroup: String;
    FPort: Integer;
    FIPVersion: TIdIPVersion;
    FReuseSocket: TIdReuseSocket;
    //
    procedure CloseBinding; virtual; abstract;
    function GetActive: Boolean; virtual;
    function GetBinding: TIdSocketHandle; virtual; abstract;
    procedure Loaded; override;
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetMulticastGroup(const Value: string); virtual;
    procedure SetPort(const Value: integer); virtual;
    function GetIPVersion: TIdIPVersion; virtual;
    procedure SetIPVersion(const AValue: TIdIPVersion); virtual;
    //
    property Active: Boolean read GetActive write SetActive Default False;
    property MulticastGroup: string read FMulticastGroup write SetMulticastGroup;
    property Port: Integer read FPort write SetPort;
    property IPVersion: TIdIPVersion read GetIPVersion write SetIPVersion default ID_DEFAULT_IP_VERSION;
    procedure InitComponent; override;
  public
    {$IFDEF WORKAROUND_INLINE_CONSTRUCTORS}
    constructor Create(AOwner: TComponent); reintroduce; overload;
    {$ENDIF}
    function IsValidMulticastGroup(const Value: string): Boolean;
{These two items are helper functions that allow you to specify the scope for
a Variable Scope Multicast Addresses.  Some are listed in IdAssignedNumbers
as the Id_IPv6MC_V_ constants.  You can't use them out of the box in the
MulticastGroup property because you need to specify the scope.  This provides
you with more flexibility than you would get with IPv4 multicasting.}
    class function SetIPv6AddrScope(const AVarIPv6Addr : String; const AScope : TIdIPMv6Scope ) : String; overload;
    class function SetIPv6AddrScope(const AVarIPv6Addr : String; const AScope : TIdIPMCValidScopes): String; overload;
    //
    property ReuseSocket: TIdReuseSocket read FReuseSocket write FReuseSocket default rsOSDependent;
  published
  end;

  EIdMCastException = Class(EIdException);
  EIdMCastNoBindings = class(EIdMCastException);
  EIdMCastNotValidAddress = class(EIdMCastException);
  EIdMCastReceiveErrorZeroBytes = class(EIdMCastException);

const
  DEF_IPv6_MGROUP = 'FF01:0:0:0:0:0:0:1';

implementation

uses
  IdAssignedNumbers,
  IdResourceStringsCore, IdStackConsts, SysUtils;

{ TIdIPMCastBase }

{$IFDEF WORKAROUND_INLINE_CONSTRUCTORS}
constructor TIdIPMCastBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{$ENDIF}

function TIdIPMCastBase.GetIPVersion: TIdIPVersion;
begin
  Result := FIPVersion;
end;

procedure TIdIPMCastBase.InitComponent;
begin
  inherited InitComponent;
  FIPVersion := ID_DEFAULT_IP_VERSION;
  {$IFDEF IdIPv6}
  FMultiCastGroup := DEF_IPv6_MGROUP;
  {$ELSE}
  FMultiCastGroup := Id_IPMC_All_Systems;
  {$ENDIF}
  FReuseSocket := rsOSDependent;
end;

function TIdIPMCastBase.GetActive: Boolean;
begin
  Result := FDsgnActive;
end;

function TIdIPMCastBase.IsValidMulticastGroup(const Value: string): Boolean;
begin
//just here to prevent a warning from Delphi about an unitialized result
  Result := False;
  case FIPVersion of
     Id_IPv4 : Result := GStack.IsValidIPv4MulticastGroup(Value);
     Id_IPv6 : Result := GStack.IsValidIPv6MulticastGroup(Value);
  end;
end;

procedure TIdIPMCastBase.Loaded;
var
  b: Boolean;
begin
  inherited Loaded;
  b := FDsgnActive;
  FDsgnActive := False;
  Active := b;
end;

procedure TIdIPMCastBase.SetActive(const Value: Boolean);
begin
  if Active <> Value then begin
    if not (IsDesignTime or IsLoading) then begin
      if Value then begin
        GetBinding;
      end
      else begin
        CloseBinding;
      end;
    end
    else begin  // don't activate at designtime (or during loading of properties)    {Do not Localize}
      FDsgnActive := Value;
    end;
  end;
end;

class function TIdIPMCastBase.SetIPv6AddrScope(const AVarIPv6Addr: String;
  const AScope: TIdIPMv6Scope): String;
begin

  case AScope of
   IdIPv6MC_InterfaceLocal : Result := SetIPv6AddrScope(AVarIPv6Addr,$1);
        IdIPv6MC_LinkLocal : Result := SetIPv6AddrScope(AVarIPv6Addr,$2);
       IdIPv6MC_AdminLocal : Result := SetIPv6AddrScope(AVarIPv6Addr,$4);
        IdIPv6MC_SiteLocal : Result := SetIPv6AddrScope(AVarIPv6Addr,$5);
         IdIPv6MC_OrgLocal : Result := SetIPv6AddrScope(AVarIPv6Addr,$8);
           IdIPv6MC_Global : Result := SetIPv6AddrScope(AVarIPv6Addr,$E);
  else
    Result := AVarIPv6Addr;
  end;
end;

class function TIdIPMCastBase.SetIPv6AddrScope(const AVarIPv6Addr: String;
  const AScope: TIdIPMCValidScopes): String;
begin
   //Replace the X in the Id_IPv6MC_V_ constants with the specified scope
   Result := ReplaceOnlyFirst(AVarIPv6Addr,'X',IntToHex(AScope,1));
end;

procedure TIdIPMCastBase.SetIPVersion(const AValue: TIdIPVersion);
begin
  if AValue <> IPVersion then
  begin
    Active := False;
    FIPVersion := AValue;
    if not IsLoading then begin
      case AValue of
        Id_IPv4: FMulticastGroup := Id_IPMC_All_Systems;
        Id_IPv6: FMulticastGroup := DEF_IPv6_MGROUP;
      end;
    end;
  end;
end;

procedure TIdIPMCastBase.SetMulticastGroup(const Value: string);
begin
  if FMulticastGroup <> Value then begin
    if (not IsLoading) and (not IsValidMulticastGroup(Value)) then begin
      raise EIdMCastNotValidAddress.Create(RSIPMCastInvalidMulticastAddress);
    end;
    Active := False;
    FMulticastGroup := Value;
  end;
end;

procedure TIdIPMCastBase.SetPort(const Value: integer);
begin
  if FPort <> Value then begin
    Active := False;
    FPort := Value;
  end;
end;

end.

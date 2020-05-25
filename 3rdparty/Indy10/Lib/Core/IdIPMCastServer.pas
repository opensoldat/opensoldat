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
  Rev 1.7    14/06/2004 21:38:42  CCostelloe
  Converted StringToTIn4Addr call

  Rev 1.6    09/06/2004 10:00:50  CCostelloe
  Kylix 3 patch

  Rev 1.5    2004.02.03 5:43:52 PM  czhower
  Name changes

  Rev 1.4    1/21/2004 3:11:10 PM  JPMugaas
  InitComponent

  Rev 1.3    10/26/2003 09:11:54 AM  JPMugaas
  Should now work in NET.

  Rev 1.2    2003.10.24 10:38:28 AM  czhower
  UDP Server todos

  Rev 1.1    2003.10.12 4:03:58 PM  czhower
  compile todos

  Rev 1.0    11/13/2002 07:55:26 AM  JPMugaas

  2001-10-16  DSiders
  Modified TIdIPMCastServer.MulticastBuffer to
  validate the AHost argument to the method instead
  of the MulticastGroup property.
}

unit IdIPMCastServer;

{
  Dr. Harley J. Mackenzie, Initial revision.
}

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  IdComponent,
  IdGlobal,
  IdIPMCastBase,
  IdSocketHandle;

const
  DEF_IMP_LOOPBACK = True;
  DEF_IMP_TTL = 1;

type
  TIdIPMCastServer = class(TIdIPMCastBase)
  protected
    FBinding: TIdSocketHandle;
    FBoundIP: String;
    FBoundPort: TIdPort;
    FLoopback: Boolean;
    FTimeToLive: Byte;
    //
    procedure ApplyLoopback;
    procedure ApplyTimeToLive;
    procedure CloseBinding; override;
    function GetActive: Boolean; override;
    function GetBinding: TIdSocketHandle; override;
    procedure Loaded; override;
    procedure MulticastBuffer(const AHost: string; const APort: Integer; const ABuffer : TIdBytes);
    procedure SetLoopback(const AValue: Boolean); virtual;
    procedure SetTTL(const AValue: Byte); virtual;
    procedure InitComponent; override;
  public
    procedure Send(const AData: string; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure Send(const ABuffer : TIdBytes); overload;
    destructor Destroy; override;
    //
    property Binding: TIdSocketHandle read GetBinding;
  published
    property Active;
    property BoundIP: String read FBoundIP write FBoundIP;
    property BoundPort: TIdPort read FBoundPort write FBoundPort;
    property Loopback: Boolean read FLoopback write SetLoopback default DEF_IMP_LOOPBACK;
    property MulticastGroup;
    property IPVersion;
    property Port;
    property ReuseSocket;
    property TimeToLive: Byte read FTimeToLive write SetTTL default DEF_IMP_TTL;
  end;

implementation

{ TIdIPMCastServer }

uses
  IdResourceStringsCore,
  IdStack,
  IdStackConsts,
  SysUtils;

procedure TIdIPMCastServer.InitComponent;
begin
  inherited InitComponent;
  FLoopback := DEF_IMP_LOOPBACK;
  FTimeToLive := DEF_IMP_TTL;
end;

procedure TIdIPMCastServer.Loaded;
var
  b: Boolean;
begin
  inherited Loaded;
  b := FDsgnActive;
  FDsgnActive := False;
  Active := b;
end;

destructor TIdIPMCastServer.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TIdIPMCastServer.CloseBinding;
begin
  FreeAndNil(FBinding);
end;

function TIdIPMCastServer.GetActive: Boolean;
begin
  Result := (inherited GetActive) or (Assigned(FBinding) and FBinding.HandleAllocated);
end;

function TIdIPMCastServer.GetBinding: TIdSocketHandle;
begin
  if not Assigned(FBinding) then begin
    FBinding := TIdSocketHandle.Create(nil);
  end;
  if not FBinding.HandleAllocated then begin
    FBinding.IPVersion := FIPVersion;
    FBinding.AllocateSocket(Id_SOCK_DGRAM);
    FBinding.IP := FBoundIP;
    FBinding.Port := FBoundPort;
    FBinding.ReuseSocket := FReuseSocket;
    FBinding.Bind;
    ApplyTimeToLive;
    ApplyLoopback;
  end;
  Result := FBinding;
end;

procedure TIdIPMCastServer.MulticastBuffer(const AHost: string; const APort: Integer; const ABuffer : TIdBytes);
begin
  // DS - if not IsValidMulticastGroup(FMulticastGroup) then
  if not IsValidMulticastGroup(AHost) then begin
    raise EIdMCastNotValidAddress.Create(RSIPMCastInvalidMulticastAddress);
  end;
  Binding.SendTo(AHost, APort, ABuffer, Binding.IPVersion);
end;

procedure TIdIPMCastServer.Send(const AData: string; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  MulticastBuffer(FMulticastGroup, FPort, ToBytes(AData, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}));
end;

procedure TIdIPMCastServer.Send(const ABuffer : TIdBytes);
begin
  MulticastBuffer(FMulticastGroup, FPort, ABuffer);
end;

procedure TIdIPMCastServer.SetLoopback(const AValue: Boolean);
begin
  if FLoopback <> AValue then begin
    FLoopback := AValue;
    ApplyLoopback;
  end;
end;

procedure TIdIPMCastServer.SetTTL(const AValue: Byte);
begin
  if FTimeToLive <> AValue then begin
    FTimeToLive := AValue;
    ApplyTimeToLive;
  end;
end;

procedure TIdIPMCastServer.ApplyLoopback;
begin
  if Assigned(FBinding) and FBinding.HandleAllocated then begin
    FBinding.SetLoopBack(FLoopback);
  end;
end;

procedure TIdIPMCastServer.ApplyTimeToLive;
begin
  if Assigned(FBinding) and FBinding.HandleAllocated then begin
    FBinding.SetMulticastTTL(FTimeToLive);
  end;
end;

end.


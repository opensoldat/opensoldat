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
  Rev 1.10    2/8/05 5:29:16 PM  RLebeau
  Updated GetHToNBytes() to use CopyTIdWord() instead of AppendBytes() for IPv6
  addresses.

  Rev 1.9    28.09.2004 20:54:32  Andreas Hausladen
  Removed unused functions that were moved to IdGlobal

  Rev 1.8    6/11/2004 8:48:20 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.7    5/19/2004 10:44:34 PM  DSiders
  Corrected spelling for TIdIPAddress.MakeAddressObject method.

  Rev 1.6    14/04/2004 17:35:38  HHariri
  Removed IP6 for BCB temporarily

  Rev 1.5    2/11/2004 5:10:40 AM  JPMugaas
  Moved IPv6 address definition to System package.

  Rev 1.4    2004.02.03 4:17:18 PM  czhower
  For unit name changes.

  Rev 1.3    2/2/2004 12:22:24 PM  JPMugaas
  Now uses IdGlobal IPVersion Type.  Added HToNBytes for things that need
  to export into NetworkOrder for structures used in protocols.

  Rev 1.2    1/3/2004 2:13:56 PM  JPMugaas
  Removed some empty function code that wasn't used.
  Added some value comparison functions.
  Added a function in the IPAddress object for comparing the value with another
  IP address.  Note that this comparison is useful as an IP address will take
  several forms (especially common with IPv6).
  Added a property for returning the IP address as a string which works for
  both IPv4 and IPv6 addresses.

  Rev 1.1    1/3/2004 1:03:14 PM  JPMugaas
  Removed Lo as it was not needed and is not safe in NET.

  Rev 1.0    1/1/2004 4:00:18 PM  JPMugaas
  An object for handling both IPv4 and IPv6 addresses.  This is a proposal with
  some old code for conversions.
}

unit IdIPAddress;

interface

{$I IdCompilerDefines.inc}
//we need to put this in Delphi mode to work

uses
  Classes,
  IdGlobal;

type
  TIdIPAddress = class(TObject)
  protected
    FIPv4 : UInt32;
    FAddrType : TIdIPVersion;
    //general conversion stuff
    //property as String Get methods
    function GetIPv4AsString : String;
    function GetIPv6AsString : String;
    function GetIPAddress : String;
  public
    //We can't make this into a property for C++Builder
    IPv6 : TIdIPv6Address;

    constructor Create; virtual;
    class function MakeAddressObject(const AIP : String) : TIdIPAddress; overload;
    class function MakeAddressObject(const AIP : String; const AIPVersion: TIdIPVersion) : TIdIPAddress; overload;
    function CompareAddress(const AIP : String; var VErr : Boolean) : Integer;
    function HToNBytes: TIdBytes;

    property IPv4 : UInt32 read FIPv4 write FIPv4;
    property IPv4AsString : String read GetIPv4AsString;
    property IPv6AsString : String read GetIPv6AsString;
    property AddrType : TIdIPVersion read FAddrType write FAddrType;
    property IPAsString : String read GetIPAddress;
  end;

implementation

uses
  IdStack, SysUtils;

//IPv4 address conversion
//Much of this is based on http://www.pc-help.org/obscure.htm

function CompareUInt16(const AWord1, AWord2 : UInt16) : Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
{
AWord1 > AWord2	> 0
AWord1 < AWord2	< 0
AWord1 = AWord2	= 0
}
begin
  if AWord1 > AWord2 then begin
    Result := 1;
  end else if AWord1 < AWord2 then begin
    Result := -1;
  end else begin
    Result := 0;
  end;
end;

function CompareUInt32(const ACard1, ACard2 : UInt32) : Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
{
ACard1 > ACard2	> 0
ACard1 < ACard2	< 0
ACard1 = ACard2	= 0
}
begin
  if ACard1 > ACard2 then begin
    Result := 1;
  end else if ACard1 < ACard2 then begin
    Result := -1;
  end else begin
    Result := 0;
  end;
end;

{ TIdIPAddress }

function TIdIPAddress.CompareAddress(const AIP: String; var VErr: Boolean): Integer;
var
  LIP2 : TIdIPAddress;
  i : Integer;
{
Note that the IP address in the object is S1.
S1 > S2	> 0
S1 < S2	< 0
S1 = S2	= 0
}
begin
  Result := 0;
  //LIP2 may be nil if the IP address is invalid
  LIP2 := MakeAddressObject(AIP);
  VErr := not Assigned(LIP2);
  if not VErr then begin
    try
      // we can't compare an IPv4 address with an IPv6 address
      VErr := FAddrType <> LIP2.FAddrType;
      if not VErr then begin
        if FAddrType = Id_IPv4 then begin
          Result := CompareUInt32(FIPv4, LIP2.FIPv4);
        end else begin
          for I := 0 to 7 do begin
            Result := CompareUInt16(IPv6[i], LIP2.IPv6[i]);
            if Result <> 0 then begin
              Break;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LIP2);
    end;
  end;
end;

constructor TIdIPAddress.Create;
begin
  inherited Create;
  FAddrType := Id_IPv4;
  FIPv4 := 0; //'0.0.0.0'
end;

function TIdIPAddress.HToNBytes: TIdBytes;
var
  I : Integer;
begin
  if FAddrType = Id_IPv4 then begin
    Result := ToBytes(GStack.HostToNetwork(FIPv4));
  end else begin
    SetLength(Result, 16);
    for I := 0 to 7 do begin
      CopyTIdUInt16(GStack.HostToNetwork(IPv6[i]), Result, 2*I);
    end;
  end;
end;

function TIdIPAddress.GetIPAddress: String;
begin
  if FAddrType = Id_IPv4 then begin
    Result := GetIPv4AsString;
  end else begin
    Result := GetIPv6AsString;
  end;
end;

function TIdIPAddress.GetIPv4AsString: String;
begin
  if FAddrType = Id_IPv4 then begin
    Result := MakeUInt32IntoIPv4Address(FIPv4);
  end else begin
    Result := '';
  end;
end;

function TIdIPAddress.GetIPv6AsString: String;
begin
  if FAddrType = Id_IPv6 then begin
    Result := IPv6AddressToStr(IPv6);
  end else begin
    Result := '';
  end;
end;

class function TIdIPAddress.MakeAddressObject(const AIP: String): TIdIPAddress;
var
  LErr : Boolean;
begin
  Result := TIdIPAddress.Create;
  try
    IPv6ToIdIPv6Address(AIP, Result.IPv6, LErr);
    if not LErr then begin
      Result.FAddrType := Id_IPv6;
      Exit;
    end;
    Result.FIPv4 := IPv4ToUInt32(AIP, LErr);
    if not LErr then begin
      Result.FAddrType := Id_IPv4;
      Exit;
    end;
    //this is not a valid IP address
    FreeAndNil(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TIdIPAddress.MakeAddressObject(const AIP: String; const AIPVersion: TIdIPVersion): TIdIPAddress;
var
  LErr : Boolean;
begin
  Result := TIdIPAddress.Create;
  try
    case AIPVersion of
      Id_IPV4:
        begin
          Result.FIPv4 := IPv4ToUInt32(AIP, LErr);
          if not LErr then begin
            Result.FAddrType := Id_IPv4;
            Exit;
          end;
        end;
      Id_IPv6:
        begin
          IPv6ToIdIPv6Address(AIP, Result.IPv6, LErr);
          if not LErr then begin
            Result.FAddrType := Id_IPv6;
            Exit;
          end
        end;
    end;
    //this is not a valid IP address
    FreeAndNil(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.

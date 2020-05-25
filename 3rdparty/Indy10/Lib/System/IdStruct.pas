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

unit IdStruct;

interface

{$i IdCompilerDefines.inc}

uses
  IdGlobal;

type
  TIdStruct = class(TObject)
  protected
    function GetBytesLen: UInt32; virtual;
  public
    constructor Create; virtual;
    //done this way in case we also need to advance an index pointer
    //after a read or write
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); virtual;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32);  virtual;
    property BytesLen: UInt32 read GetBytesLen;
  end;

  TIdUnion = class(TIdStruct)
  protected
    FBuffer: TIdBytes;
    function GetBytesLen: UInt32; override;
    procedure SetBytesLen(const ABytesLen: UInt32);
  public
    procedure ReadStruct(const ABytes : TIdBytes; var VIndex : UInt32); override;
    procedure WriteStruct(var VBytes : TIdBytes; var VIndex : UInt32);  override;
  end;

  // TODO: rename to TIdUInt32
  TIdLongWord = class(TIdUnion)
  protected
    function Get_l: UInt32;
    function Gets_b1: UInt8;
    function Gets_b2: UInt8;
    function Gets_b3: UInt8;
    function Gets_b4: UInt8;
    function Gets_w1: UInt16;
    function Gets_w2: UInt16;
    procedure Set_l(const Value: UInt32);
    procedure Sets_b1(const Value: UInt8);
    procedure Sets_b2(const Value: UInt8);
    procedure Sets_b3(const Value: UInt8);
    procedure Sets_b4(const Value: UInt8);
    procedure SetS_w1(const Value: UInt16);
    procedure SetS_w2(const Value: UInt16);
  public
    constructor Create; override;
    property s_b1 : UInt8 read Gets_b1 write Sets_b1;
    property s_b2 : UInt8 read Gets_b2 write Sets_b2;
    property s_b3 : UInt8 read Gets_b3 write Sets_b3;
    property s_b4 : UInt8 read Gets_b4 write Sets_b4;
    property s_w1 : UInt16 read Gets_w1 write SetS_w1;
    property s_w2 : UInt16 read Gets_w2 write SetS_w2;
    property s_l  : UInt32 read Get_l write Set_l;
  end;

implementation

// This is here to facilitate inlining

{$IFDEF WINDOWS}
  {$IFDEF USE_INLINE}
uses
  Windows;
  {$ENDIF}
{$ENDIF}

{ TIdStruct }

constructor TIdStruct.Create;
begin
  inherited Create;
end;

function TIdStruct.GetBytesLen: UInt32;
begin
  Result := 0;
end;

procedure TIdStruct.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  Assert(UInt32(Length(ABytes)) >= VIndex + BytesLen, 'not enough bytes'); {do not localize}
end;

procedure TIdStruct.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
var
  Len: UInt32;
begin
  Len := VIndex + BytesLen;
  if UInt32(Length(VBytes)) < Len then begin
    SetLength(VBytes, Len);
  end;
end;

{ TIdUnion }

function TIdUnion.GetBytesLen  : UInt32;
begin
  Result := UInt32(Length(FBuffer));
end;

procedure TIdUnion.SetBytesLen(const ABytesLen: UInt32);
begin
  SetLength(FBuffer, ABytesLen);
end;

procedure TIdUnion.ReadStruct(const ABytes: TIdBytes; var VIndex: UInt32);
begin
  inherited ReadStruct(ABytes, VIndex);
  CopyTIdBytes(ABytes, VIndex, FBuffer, 0, Length(FBuffer));
  Inc(VIndex, Length(FBuffer));
end;

procedure TIdUnion.WriteStruct(var VBytes: TIdBytes; var VIndex: UInt32);
begin
  inherited WriteStruct(VBytes, VIndex);
  CopyTIdBytes(FBuffer, 0, VBytes, VIndex, Length(FBuffer));
  Inc(VIndex, Length(FBuffer));
end;

{ TIdLongWord }

constructor TIdLongWord.Create;
begin
  inherited Create;
  SetBytesLen(4);
end;

function TIdLongWord.Gets_w1: UInt16;
begin
  Result := BytesToUInt16(FBuffer, 0);
end;

procedure TIdLongWord.SetS_w1(const Value: UInt16);
begin
  CopyTIdUInt16(Value, FBuffer, 0);
end;

function TIdLongWord.Gets_b1: UInt8;
begin
  Result := FBuffer[0];
end;

procedure TIdLongWord.Sets_b1(const Value: UInt8);
begin
  FBuffer[0] := Value;
end;

function TIdLongWord.Gets_b2: UInt8;
begin
  Result := FBuffer[1];
end;

procedure TIdLongWord.Sets_b2(const Value: UInt8);
begin
  FBuffer[1] := Value;
end;

function TIdLongWord.Gets_b3: UInt8;
begin
  Result := FBuffer[2];
end;

procedure TIdLongWord.Sets_b3(const Value: UInt8);
begin
   FBuffer[2] := Value;
end;

function TIdLongWord.Gets_b4: UInt8;
begin
  Result := FBuffer[3];
end;

procedure TIdLongWord.Sets_b4(const Value: UInt8);
begin
  FBuffer[3] := Value;
end;

function TIdLongWord.Get_l: UInt32;
begin
  Result := BytesToUInt32(FBuffer, 0);
end;

procedure TIdLongWord.Set_l(const Value: UInt32);
begin
  CopyTIdUInt32(Value, FBuffer, 0);
end;

function TIdLongWord.Gets_w2: UInt16;
begin
  Result := BytesToUInt16(FBuffer, 2);
end;

procedure TIdLongWord.SetS_w2(const Value: UInt16);
begin
  CopyTIdUInt16(Value, FBuffer, 2);
end;

end.

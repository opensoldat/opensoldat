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

unit IdStreamNET;

interface

uses
  Classes,
  IdGlobal;

type
  TIdStreamHelperNET = class
  public
    class function ReadBytes(
          AStream: TStream;
          var VBytes: TIdBytes;
          ACount: Integer = -1;
          AOffset: Integer = 0): Integer; static;
    class function Write(
          const AStream: TStream;
          const ABytes: TIdBytes;
          const ACount: Integer = -1;
          const AOffset: Integer = 0): Integer; static;
    class function Seek(
          const AStream: TStream;
          const AOffset: TIdStreamSize;
          const AOrigin: TSeekOrigin) : TIdStreamSize; static;
  end;

implementation

// RLebeau: must use a 'var' and not an 'out' for the VBytes parameter,
// or else any preallocated buffer the caller passes in will get wiped out!

class function TIdStreamHelperNET.ReadBytes(AStream: TStream; var VBytes: TIdBytes;
  ACount, AOffset: Integer): Integer;
var
  LActual: Integer;
begin
  Assert(AStream<>nil);
  Result := 0;

  if VBytes = nil then begin
    SetLength(VBytes, 0);
  end;
  //check that offset<length(buffer)? offset+count?
  //is there a need for this to be called with an offset into a nil buffer?

  LActual := ACount;
  if LActual < 0 then begin
    LActual := AStream.Size - AStream.Position;
  end;

  //this prevents eg reading 0 bytes at Offset=10 from allocating memory
  if LActual = 0 then begin
    Exit;
  end;

  if Length(VBytes) < (AOffset+LActual) then begin
    SetLength(VBytes, AOffset+LActual);
  end;

  Assert(VBytes<>nil);
  Result := AStream.Read(VBytes, AOffset, LActual);
end;

class function TIdStreamHelperNET.Write(const AStream: TStream;
  const ABytes: TIdBytes; const ACount: Integer; const AOffset: Integer): Integer;
var
  LActual: Integer;
begin
  Result := 0;
  Assert(AStream<>nil);
  //should we raise assert instead of this nil check?
  if ABytes <> nil then
  begin
    LActual := IndyLength(ABytes, ACount, AOffset);
    if LActual > 0 then
    begin
      // RLebeau: Write raises an exception if the buffer can't be written in full
      AStream.Write(ABytes, AOffset, LActual);
      Result := LActual;
    end;
  end;
end;

class function TIdStreamHelperNET.Seek(const AStream: TStream; const AOffset: TIdStreamSize;
  const AOrigin: TSeekOrigin): TIdStreamSize;
begin
  Result := AStream.Seek(AOffset, AOrigin);
end;

end.


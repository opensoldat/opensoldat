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
  Rev 1.47    1/24/2005 7:35:54 PM  JPMugaas
  Fixed CopyTIdIPV6Address.

  Rev 1.46    1/17/2005 7:28:44 PM  JPMugaas
  Added Index parameter to several functions so you can use TIdBuffer in a
  random access manner instead of in a sequential manner.  This is good for
  some fixed-packet or data types.

  Added functions for reading and writing various types to TIdBuffer which use
  Byte Order functions.  This should facilitate a lot of development as this
  gets used more.

  Rev 1.45    27.08.2004 21:58:18  Andreas Hausladen
  Speed optimization ("const" for string parameters)

  Rev 1.44    2004.07.03 19:41:34  czhower
  UTF8, SaveToStream

  Rev 1.43    6/11/2004 8:48:12 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.42    6/9/04 7:46:26 PM  RLebeau
  Updated ExtractToBytes() to allocate the output buffer only if the buffer
  length is smaller than the requested number of bytes.

  Rev 1.41    5/29/04 10:44:58 PM  RLebeau
  Updated ExtractToBytes() to allocate the output buffer regardless of the
  AAppend parameter

  Added empty string return value to Extract() when AByteCount <= 0

  Rev 1.40    2004.05.20 11:39:06 AM  czhower
  IdStreamVCL

  Rev 1.39    2004.05.10 1:19:18 PM  czhower
  Removed unneeded code.

  Rev 1.38    5/3/2004 12:57:00 PM  BGooijen
  Fixes for 0-based

  Rev 1.37    2004.05.03 11:15:42 AM  czhower
  Changed Find to IndexOf and made 0 based to be consistent.

  Rev 1.36    2004.05.01 4:26:52 PM  czhower
  Added PeekByte

  Rev 1.35    2004.04.16 11:30:26 PM  czhower
  Size fix to IdBuffer, optimizations, and memory leaks

  Rev 1.34    2004.04.08 7:06:44 PM  czhower
  Peek support.

  Rev 1.33    2004.04.08 3:56:24 PM  czhower
  Fixed bug with Intercept byte count. Also removed Bytes from Buffer.

  Rev 1.32    2004.04.08 2:03:34 AM  czhower
  Fixes to Bytes.

  Rev 1.31    2004.04.07 3:59:44 PM  czhower
  Bug fix for WriteDirect.

  Rev 1.30    2004.04.07 3:46:30 PM  czhower
  Compile fix.

  Rev 1.29    4/7/2004 1:02:14 PM  BGooijen
  when extract* is called with -1 or no parameters all data it extracted

  Rev 1.28    2004.03.29 9:58:38 PM  czhower
  Is now buffered. Now takes 2/3rds the time as before.

  Rev 1.27    23/03/2004 18:33:44  CCostelloe
  Bug fix: ReadLn returns a previously-read line if FBytes also accessed
  in-between (causes MoveHeadToStartIfNecessary to be called)

  Rev 1.26    18/03/2004 20:24:26  CCostelloe
  Speed improvement by adding FHeadIndex: 10 MB base64 decode reduced from 10
  hours to 62 seconds.

  Rev 1.25    2004.03.03 11:55:02 AM  czhower
  IdStream change

  Rev 1.24    3/1/04 7:33:12 PM  RLebeau
  Updated Remove() to call the OnBytesRemoved event handler.

  Rev 1.23    2004.02.03 4:17:14 PM  czhower
  For unit name changes.

  Rev 1.22    1/11/2004 5:48:48 PM  BGooijen
  Added AApend parameter to ExtractToBytes

  Rev 1.21    1/7/2004 8:36:32 PM  BGooijen
  Arguments were in wrong order

  Rev 1.20    22/11/2003 10:35:04 PM  GGrieve
  Reverse copy direction in TIdBuffer.ExtractToStream

  Rev 1.19    2003.10.24 10:44:54 AM  czhower
  IdStream implementation, bug fixes.

  Rev 1.18    10/15/2003 1:03:40 PM  DSiders
  Created resource strings for TIdBuffer.Find exceptions.

  Rev 1.17    2003.10.14 1:27:06 PM  czhower
  Uupdates + Intercept support

  Rev 1.16    2003.10.11 5:47:00 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.15    10/5/2003 10:24:20 PM  BGooijen
  Changed WriteBytes(var ...) to WriteBytes(const ...)

  Rev 1.14    10/3/2003 10:46:38 PM  BGooijen
  Fixed Range Check Exception, and fixed ExtractToStream

  Rev 1.13    2003.10.02 8:29:12 PM  czhower
  Changed names of byte conversion routines to be more readily understood and
  not to conflict with already in use ones.

  Rev 1.12    2003.10.02 12:44:58 PM  czhower
  Comment added

  Rev 1.11    10/2/2003 5:23:14 PM  GGrieve
  make Bytes a public property

  Rev 1.10    10/2/2003 5:00:38 PM  GGrieve
  Fix bug in find - can't find last char

  Rev 1.9    2003.10.02 10:37:00 AM  czhower
  Comments

  Rev 1.8    10/2/2003 3:54:06 PM  GGrieve
  Finish cleaning up - no $IFDEFs but still optimal on both win32 and DontNet

  Rev 1.7    10/1/2003 10:58:38 PM  BGooijen
  Removed unused var

  Rev 1.6    10/1/2003 8:15:58 PM  BGooijen
  Fixed Range Check Error on D7

  Rev 1.5    10/1/2003 8:02:22 PM  BGooijen
  Removed some ifdefs and improved code

  Rev 1.4    10/1/2003 10:49:02 PM  GGrieve
  Rework buffer for Octane Compability

  Rev 1.3    2003.10.01 2:30:44 PM  czhower
  .Net

  Rev 1.2    2003.10.01 1:37:32 AM  czhower
  .Net

  Rev 1.1    2003.10.01 1:12:32 AM  czhower
  .Net

  Rev 1.0    2003.09.30 10:33:56 PM  czhower
  Readd after accidental delete.

  Rev 1.14    2003.09.30 10:33:16 PM  czhower
  Updates

  Rev 1.13    2003.07.16 5:05:06 PM  czhower
  Phase 1 of IdBuffer changes for compat.

  Rev 1.12    6/29/2003 10:56:22 PM  BGooijen
  Removed .Memory from the buffer, and added some extra methods

  Rev 1.11    2003.06.25 4:29:06 PM  czhower
  Free --> FreeAndNil

  Rev 1.10    2003.01.17 2:18:36 PM  czhower

  Rev 1.9    12-14-2002 22:08:24  BGooijen
  Changed FMemory to FMemory.Memory in some places

  Rev 1.8    12-14-2002 22:02:34  BGooijen
  changed Memory to FMemory in some places, to remove some issues

  Rev 1.7    12/11/2002 04:27:02 AM  JPMugaas
  Fixed compiler warning.

  Rev 1.6    12/11/2002 03:53:44 AM  JPMugaas
  Merged the buffer classes.

  Rev 1.5    2002.12.07 12:26:18 AM  czhower

  Rev 1.4    12-6-2002 20:34:06  BGooijen
  Now compiles on Delphi 5

  Rev 1.3    6/12/2002 11:00:14 AM  SGrobety

  Rev 1.2    12/5/2002 02:55:44 PM  JPMugaas
  Added AddStream method for reading a stream into the buffer class.

  Rev 1.1    23.11.2002 12:59:48  JBerg
  fixed packbuffer

  Rev 1.0    11/13/2002 08:38:32 AM  JPMugaas
}
unit IdBuffer;

{$I IdCompilerDefines.inc}

{
  .Net forces us to perform copies from strings to Bytes so that it can do the
  proper unicode and other conversions.

  IdBuffer is for storing data we cannot deal with right now and we do not know
  the size. It must be optimized for adding to the end, and extracting from the
  beginning. First pass we are just making it work, later using bubbling we will
  optimize it for such tasks.

  The copy is a separate issue and we considered several options. For .net we will
  always have to copy data to send or to receive to translate it to binary. For
  example if we have a string it must be converted to bytes. This conversion
  requires a copy. All strings are Unicode and must be converted to single
  bytes by a convertor. This is not limited to strings.

  In VCL previously all strings were AnsiString so we used a pointer and just
  accessed the memory directly from the string. This avoided the overhead of a
  copy.

  We have come up with several ideas on how to allow the copy on .net, while
  avoiding the copy on VCL to keep the performance benefit. However we must do
  it in a single source manner and in a manner that does not impact the code
  negatively.

  For now for VCL we also do a copy. This has the advantage that Byte arrays are
  reference counted and automaticaly handled by Delphi. For example:

  WriteBytes(StringToBytes(s));

  The array returned by this function will automatically be freed by Delphi.

  There are other options that are nearly as transparent but have the additional
  overhead of requiring class creation. These classes can be used to copy for .net
  and proxy on VCL. It all works very nice and has low memory overhead. The
  objects can then be freed by default in methods that accept them.

  However after analysis, copy on VCL may not be that bad after all. The copy
  only really impacts strings. The overhead to copy strings is minimal and only
  used in commands etc. The big transfers come from files, streams, or other.
  Such transfers have to be mapped into memory in VCL anyways, and if we map
  directly into the byte array instead of the previous classes peformance should
  be fine.

  In short - copy under VCL should be acceptable if we watch for bottlenecks and
  fix them appropriately without having to creat proxy classes. The only problem
  remains for transmitting large memory blocks. But if this is done against a
  fixed copy buffer the performance hit will be neglible and it is not a common
  task to transmit large memory blocks.

  For such transfers from streams, etc the user can declare a persistent array
  of bytes that is not freed between each call to WriteBytes.

  -Kudzu
}

interface

uses
  Classes,
  IdException,
  IdGlobal,
  SysUtils;

type
  EIdNotEnoughDataInBuffer = class(EIdException);
  EIdTooMuchDataInBuffer = class(EIdException); // only 2GB is allowed -

  TIdBufferBytesRemoved = procedure(ASender: TObject; ABytes: Integer) of object;

  // TIdBuffer is used as an internal buffer to isolate Indy from pointers and
  // memory allocations. It also allows optimizations to be kept in a single place.
  //
  // TIdBuffer is primarily used as a read/write buffer for the communication layer.

  TIdBuffer = class(TObject)
  private
    function GetAsString: string;
  protected
    FBytes: TIdBytes;
    FByteEncoding: IIdTextEncoding;
    {$IFDEF STRING_IS_ANSI}
    FAnsiEncoding: IIdTextEncoding;
    {$ENDIF}
    FGrowthFactor: Integer;
    FHeadIndex: Integer;
    FOnBytesRemoved: TIdBufferBytesRemoved;
    FSize: Integer;
    //
    procedure CheckAdd(AByteCount : Integer; const AIndex : Integer);
    procedure CheckByteCount(var VByteCount : Integer; const AIndex : Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
  public
    procedure Clear;
    constructor Create; overload;
    constructor Create(AOnBytesRemoved: TIdBufferBytesRemoved); overload;
    constructor Create(AGrowthFactor: Integer); overload;
    constructor Create(const ABytes : TIdBytes; const ALength : Integer = -1); overload;
    procedure CompactHead(ACanShrink: Boolean = True);
    destructor Destroy; override;
    {
    Most of these now have an AIndex parameter.  If that is less than 0,
    we are accessing data sequentially.  That means, read the data from the HeadIndex
    and "remove" the data you read.

    If AIndex is 0 or greater, the HeadIndex is disregarded and no deletion is done.
    You are just reading from a particular location in a random access manner.

    }
    // will extract number of bytes and decode as specified
    function Extract(AByteCount: Integer = -1; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ExtractToString()'{$ENDIF};{$ENDIF}
    function ExtractToString(AByteCount: Integer = -1; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string;
    // all 3 extract routines append to existing data, if any
    procedure ExtractToStream(const AStream: TStream; AByteCount: Integer = -1; const AIndex: Integer = -1);
    procedure ExtractToIdBuffer(ABuffer: TIdBuffer; AByteCount: Integer = -1; const AIndex : Integer = -1);
    procedure ExtractToBytes(var VBytes: TIdBytes; AByteCount: Integer = -1;
      AAppend: Boolean = True; AIndex : Integer = -1);
    function ExtractToUInt8(const AIndex : Integer): UInt8;
    function ExtractToByte(const AIndex : Integer): UInt8; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ExtractToUInt8()'{$ENDIF};{$ENDIF}
    function ExtractToUInt16(const AIndex : Integer): UInt16;
    function ExtractToWord(const AIndex : Integer): UInt16; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ExtractToUInt16()'{$ENDIF};{$ENDIF}
    function ExtractToUInt32(const AIndex : Integer): UInt32;
    function ExtractToLongWord(const AIndex : Integer): UInt32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ExtractToUInt32()'{$ENDIF};{$ENDIF}
    function ExtractToUInt64(const AIndex : Integer): TIdUInt64;
    procedure ExtractToIPv6(const AIndex : Integer; var VAddress: TIdIPv6Address);
    function IndexOf(const AByte: Byte; AStartPos: Integer = 0): Integer; overload;
    function IndexOf(const ABytes: TIdBytes; AStartPos: Integer = 0): Integer; overload;
    function IndexOf(const AString: string; AStartPos: Integer = 0;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ): Integer; overload;
    function PeekByte(AIndex: Integer): Byte;
    procedure Remove(AByteCount: Integer);
    procedure SaveToStream(const AStream: TStream);
      { Most of these now have an ADestIndex parameter.  If that is less than 0,
        we are writing data sequentially.

        If ADestIndex is 0 or greater, you are setting bytes in a particular
        location in a random access manner.
      }
    // Write
    procedure Write(const AString: string; AByteEncoding: IIdTextEncoding = nil;
      const ADestIndex: Integer = -1
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure Write(const ABytes: TIdBytes; const ADestIndex: Integer = -1); overload;
    procedure Write(const ABytes: TIdBytes; const ALength, AOffset : Integer; const ADestIndex: Integer = -1); overload;
    procedure Write(AStream: TStream; AByteCount: Integer = 0); overload;
    procedure Write(const AValue: TIdUInt64; const ADestIndex: Integer = -1); overload;
    procedure Write(const AValue: UInt32; const ADestIndex: Integer = -1); overload;
    procedure Write(const AValue: UInt16; const ADestIndex: Integer = -1); overload;
    procedure Write(const AValue: UInt8; const ADestIndex: Integer = -1); overload;
    procedure Write(const AValue: TIdIPv6Address; const ADestIndex: Integer = -1); overload;
    //
    //Kudzu: I have removed the Bytes property. Do not add it back - it allowed "internal" access
    // which caused compacting or internal knowledge. Access via Extract or other such methods
    // instead. Bytes could also be easily confused with FBytes internally and cause issues.
    //
    // Bytes also allowed direct acces without removing which could cause concurrency issues if
    // the reference was kept.
    //
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Encoding: IIdTextEncoding read FByteEncoding write FByteEncoding;
    {$IFDEF STRING_IS_ANSI}
    property AnsiEncoding: IIdTextEncoding read FAnsiEncoding write FAnsiEncoding;
    {$ENDIF}
    property GrowthFactor: Integer read FGrowthFactor write FGrowthFactor;
    property Size: Integer read FSize;
    //useful for testing. returns buffer as string without extraction.
    property AsString: string read GetAsString;
  end;

implementation

uses
  IdResourceStringsCore,
  IdStream,
  IdStack; //needed for byte order functions

procedure TIdBuffer.CheckAdd(AByteCount : Integer; const AIndex : Integer);
begin
  if (MaxInt - AByteCount) < (Size + AIndex) then begin
    raise EIdTooMuchDataInBuffer.Create(RSTooMuchDataInBuffer);
  end;
end;

procedure TIdBuffer.CheckByteCount(var VByteCount : Integer; const AIndex : Integer);
begin
  if VByteCount = -1 then begin
    VByteCount := Size+AIndex;
  end
  else if VByteCount > (Size+AIndex) then begin
    raise EIdNotEnoughDataInBuffer.CreateFmt('%s (%d/%d)', [RSNotEnoughDataInBuffer, VByteCount, Size]); {do not localize}
  end;
end;

procedure TIdBuffer.Clear;
begin
  SetLength(FBytes, 0);
  FHeadIndex := 0;
  FSize := Length(FBytes);
end;

constructor TIdBuffer.Create(AGrowthFactor: Integer);
begin
  Create;
  FGrowthFactor := AGrowthFactor;
end;

constructor TIdBuffer.Create(AOnBytesRemoved: TIdBufferBytesRemoved);
begin
  Create;
  FOnBytesRemoved := AOnBytesRemoved;
end;

constructor TIdBuffer.Create(const ABytes: TIdBytes; const ALength: Integer);
begin
  Create;
  if ALength < 0 then
  begin
    FBytes := ABytes;
    FSize := Length(ABytes);
  end else
  begin
    SetLength(FBytes, ALength);
    if ALength > 0 then
    begin
      CopyTIdBytes(ABytes, 0, FBytes, 0, ALength);
      FSize := ALength;
    end;
  end;
end;

destructor TIdBuffer.Destroy;
begin
  Clear;
  inherited Destroy;
  //do only at the last moment
  TIdStack.DecUsage;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdBuffer.Extract(AByteCount: Integer = -1; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ExtractToString(AByteCount, AByteEncoding{$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF});
end;

function TIdBuffer.ExtractToString(AByteCount: Integer = -1; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  LBytes: TIdBytes;
begin
  if AByteCount < 0 then begin
    AByteCount := Size;
  end;
  if AByteCount > 0 then
  begin
    if AByteEncoding = nil then begin
      AByteEncoding := FByteEncoding;
      EnsureEncoding(AByteEncoding);
    end;
    {$IFDEF STRING_IS_ANSI}
    if ADestEncoding = nil then begin
      ADestEncoding := FAnsiEncoding;
      EnsureEncoding(ADestEncoding, encOSDefault);
    end;
    {$ENDIF}
    ExtractToBytes(LBytes, AByteCount);
    Result := BytesToString(LBytes, AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
      );
  end else begin
    Result := '';
  end;
end;

procedure TIdBuffer.ExtractToBytes(var VBytes: TIdBytes; AByteCount: Integer = -1;
  AAppend: Boolean = True; AIndex : Integer = -1);
var
  LOldSize: Integer;
  LIndex : Integer;
begin
  if AByteCount < 0 then begin
    AByteCount := Size;
  end;
  LIndex := IndyMax(AIndex, 0);
  if AByteCount > 0 then begin
    CheckByteCount(AByteCount, LIndex);
    if AAppend then begin
      LOldSize := Length(VBytes);
      SetLength(VBytes, LOldSize + AByteCount);
    end else begin
      LOldSize := 0;
      if Length(VBytes) < AByteCount then begin
        SetLength(VBytes, AByteCount);
      end;
    end;
    if AIndex < 0 then
    begin
      CopyTIdBytes(FBytes, FHeadIndex, VBytes, LOldSize, AByteCount);
      Remove(AByteCount);
    end else
    begin
      CopyTIdBytes(FBytes, AIndex, VBytes, LOldSize, AByteCount);
    end;
  end;
end;

procedure TIdBuffer.ExtractToIdBuffer(ABuffer: TIdBuffer; AByteCount: Integer = -1;
  const AIndex: Integer = -1);
var
  LBytes: TIdBytes;
begin
  if AByteCount < 0 then begin
    AByteCount := Size;
  end;
  //TODO: Optimize this routine to directly copy from one to the other
  ExtractToBytes(LBytes, AByteCount, True, AIndex);
  ABuffer.Write(LBytes);
end;

procedure TIdBuffer.ExtractToStream(const AStream: TStream; AByteCount: Integer = -1;
  const AIndex: Integer = -1);
var
  LIndex : Integer;
  LBytes : TIdBytes;
begin
  if AByteCount < 0 then begin
    AByteCount := Size;
  end;
  LIndex := IndyMax(AIndex, 0);
  if AIndex < 0 then
  begin
    // TODO: remove CompactHead() here and pass FHeadIndex to TIdStreamHelper.Write():
    {
    CheckByteCount(AByteCount, FHeadIndex);
    TIdStreamHelper.Write(AStream, FBytes, AByteCount, FHeadIndex);
    Remove(AByteCount);
    }
    CompactHead;
    CheckByteCount(AByteCount, LIndex);
    TIdStreamHelper.Write(AStream, FBytes, AByteCount);
    Remove(AByteCount);
  end else
  begin
    // TODO: remove CopyTIdBytes() here and pass FBytes and AIndex to TIdStreamHelper.Write():
    {
    CheckByteCount(AByteCount, LIndex);
    TIdStreamHelper.Write(AStream, FBytes, AByteCount, AIndex);
    }
    CheckByteCount(AByteCount, LIndex);
    SetLength(LBytes, AByteCount);
    CopyTIdBytes(FBytes, AIndex, LBytes, 0, AByteCount);
    TIdStreamHelper.Write(AStream, LBytes, AByteCount);
  end;
end;

procedure TIdBuffer.Remove(AByteCount: Integer);
begin
  if AByteCount >= Size then begin
    Clear;
  end else begin
    Inc(FHeadIndex, AByteCount);
    Dec(FSize, AByteCount);
    if FHeadIndex > GrowthFactor then begin
      CompactHead;
    end;
  end;
  if Assigned(FOnBytesRemoved) then begin
    FOnBytesRemoved(Self, AByteCount);
  end;
end;

procedure TIdBuffer.CompactHead(ACanShrink: Boolean = True);
begin
  // Only try to compact if needed.
  if FHeadIndex > 0 then begin
    CopyTIdBytes(FBytes, FHeadIndex, FBytes, 0, Size);
    FHeadIndex := 0;
    if ACanShrink and ((Capacity - Size - FHeadIndex) > GrowthFactor) then begin
      SetLength(FBytes, FHeadIndex + Size + GrowthFactor);
    end;
  end;
end;

procedure TIdBuffer.Write(const ABytes: TIdBytes; const ADestIndex: Integer = -1);
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Write(ABytes, Length(ABytes), 0, ADestIndex);
end;

procedure TIdBuffer.Write(AStream: TStream; AByteCount: Integer);
var
  LAdded: Integer;
  LLength: Integer;
begin
  if AByteCount < 0 then begin
    // Copy remaining
    LAdded := AStream.Size - AStream.Position;
  end else if AByteCount = 0 then begin
    // Copy all
    AStream.Position := 0;
    LAdded := AStream.Size;
  end else begin
    LAdded := IndyMin(AByteCount, AStream.Size - AStream.Position);
  end;
  if LAdded > 0 then begin
    LLength := Size;
    CheckAdd(LAdded, 0);
    CompactHead;
    SetLength(FBytes, LLength + LAdded);
    TIdStreamHelper.ReadBytes(AStream, FBytes, LAdded, LLength);
    Inc(FSize, LAdded);
  end;
end;

function TIdBuffer.IndexOf(const AString: string; AStartPos: Integer = 0;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Integer;
begin
  if AByteEncoding = nil then begin
    AByteEncoding := FByteEncoding;
  end;
  {$IFDEF STRING_IS_ANSI}
  if ASrcEncoding = nil then begin
    ASrcEncoding := FAnsiEncoding;
  end;
  {$ENDIF}
  Result := IndexOf(
    ToBytes(AString, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}),
    AStartPos);
end;

function TIdBuffer.IndexOf(const ABytes: TIdBytes; AStartPos: Integer = 0): Integer;
var
  i, j, LEnd, BytesLen: Integer;
  LFound: Boolean;
begin
  Result := -1;
  // Dont search if it empty
  if Size > 0 then begin
    if Length(ABytes) = 0 then begin
      raise EIdException.Create(RSBufferMissingTerminator);
    end;
    if (AStartPos < 0) or (AStartPos >= Size) then begin
      raise EIdException.Create(RSBufferInvalidStartPos);
    end;
    BytesLen := Length(ABytes);
    LEnd := FHeadIndex + Size;
    for i := FHeadIndex + AStartPos to LEnd - BytesLen do begin
      LFound := True;
      for j := 0 to BytesLen - 1 do begin
        if (i + j) >= LEnd then begin
          Break;
        end;
        if FBytes[i + j] <> ABytes[j] then begin
          LFound := False;
          Break;
        end;
      end;
      if LFound then begin
        Result := i - FHeadIndex;
        Break;
      end;
    end;
  end;
end;

function TIdBuffer.IndexOf(const AByte: Byte; AStartPos: Integer = 0): Integer;
var
  i: Integer;
begin
  Result := -1;
  // Dont search if it empty
  if Size > 0 then begin
    if (AStartPos < 0) or (AStartPos >= Size) then begin
      raise EIdException.Create(RSBufferInvalidStartPos);
    end;
    for i := (FHeadIndex + AStartPos) to (FHeadIndex + Size - 1) do begin
      if FBytes[i] = AByte then begin
        Result := i - FHeadIndex;
        Break;
      end;
    end;
  end;
end;

procedure TIdBuffer.Write(const AString: string; AByteEncoding: IIdTextEncoding = nil;
  const ADestIndex : Integer = -1
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  if AByteEncoding = nil then begin
    AByteEncoding := FByteEncoding;
  end;
  {$IFDEF STRING_IS_ANSI}
  if ASrcEncoding = nil then begin
    ASrcEncoding := FAnsiEncoding;
  end;
  {$ENDIF}
  Write(
    ToBytes(AString, AByteEncoding{$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}),
    ADestIndex);
end;

function TIdBuffer.GetCapacity: Integer;
begin
  Result := Length(FBytes);
end;

procedure TIdBuffer.SetCapacity(AValue: Integer);
begin
  if AValue < Size then begin
    raise EIdException.Create('Capacity cannot be smaller than Size'); {do not localize}
  end;
  CompactHead;
  SetLength(FBytes, AValue);
end;

constructor TIdBuffer.Create;
begin
  inherited Create;
  FGrowthFactor := 2048;
  Clear;
  TIdStack.IncUsage;
end;

function TIdBuffer.PeekByte(AIndex: Integer): Byte;
begin
  if Size = 0 then begin
    raise EIdException.Create('No bytes in buffer.'); {do not localize}
  end;
  if (AIndex < 0) or (AIndex >= Size) then begin
    raise EIdException.Create('Index out of bounds.'); {do not localize}
  end;
  Result := FBytes[FHeadIndex + AIndex];
end;

procedure TIdBuffer.SaveToStream(const AStream: TStream);
begin
  CompactHead(False);
  TIdStreamHelper.Write(AStream, FBytes, Size);
end;

procedure TIdBuffer.ExtractToIPv6(const AIndex: Integer; var VAddress: TIdIPv6Address);
var
  LIndex : Integer;
begin
  if AIndex < 0 then begin
    LIndex := FHeadIndex;
  end else begin
    LIndex := AIndex;
  end;
  BytesToIPv6(FBytes, VAddress, LIndex);
  VAddress := GStack.NetworkToHost(VAddress);
  if AIndex < 0 then begin
    Remove(16);
  end;
end;

function TIdBuffer.ExtractToUInt64(const AIndex: Integer): TIdUInt64;
var
  LIndex : Integer;
begin
  if AIndex < 0 then begin
    LIndex := FHeadIndex;
  end else begin
    LIndex := AIndex;
  end;
  Result := BytesToUInt64(FBytes, LIndex);
  Result := GStack.NetworkToHost(Result);
  if AIndex < 0 then begin
    Remove(8);
  end;
end;

function TIdBuffer.ExtractToUInt32(const AIndex: Integer): UInt32;
var
  LIndex : Integer;
begin
  if AIndex < 0 then begin
    LIndex := FHeadIndex;
  end else begin
    LIndex := AIndex;
  end;
  Result := BytesToUInt32(FBytes, LIndex);
  Result := GStack.NetworkToHost(Result);
  if AIndex < 0 then begin
    Remove(4);
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdBuffer.ExtractToLongWord(const AIndex: Integer): UInt32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ExtractToUInt32(AIndex);
end;

function TIdBuffer.ExtractToUInt16(const AIndex: Integer): UInt16;
var
  LIndex : Integer;
begin
  if AIndex < 0 then begin
    LIndex := FHeadIndex;
  end else begin
    LIndex := AIndex;
  end;
  Result := BytesToUInt16(FBytes, LIndex);
  Result := GStack.NetworkToHost(Result);
  if AIndex < 0 then begin
    Remove(2);
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdBuffer.ExtractToWord(const AIndex: Integer): UInt16;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ExtractToUInt16(AIndex);
end;

function TIdBuffer.ExtractToUInt8(const AIndex: Integer): UInt8;
var
  LIndex : Integer;
begin
  if AIndex < 0 then begin
    LIndex := FHeadIndex;
  end else begin
    LIndex := AIndex;
  end;
  Result := FBytes[LIndex];
  if AIndex < 0 then begin
    Remove(1);
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdBuffer.ExtractToByte(const AIndex: Integer): UInt8;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ExtractToUInt8(AIndex);
end;

procedure TIdBuffer.Write(const AValue: UInt16; const ADestIndex: Integer);
var
  LVal : UInt16;
  LIndex : Integer;
begin
  if ADestIndex < 0 then
  begin
    LIndex := FHeadIndex + Size;
    SetLength(FBytes, LIndex+2);
  end else
  begin
    LIndex := ADestIndex;
  end;
  LVal := GStack.HostToNetwork(AValue);
  CopyTIdUInt16(LVal, FBytes, LIndex);
  if LIndex >= FSize then begin
    FSize := LIndex+2;
  end;
end;

procedure TIdBuffer.Write(const AValue: UInt8; const ADestIndex: Integer);
var
  LIndex : Integer;
begin
  if ADestIndex < 0 then
  begin
    LIndex := FHeadIndex + Size;
    SetLength(FBytes, LIndex+1);
  end else
  begin
    LIndex := ADestIndex;
  end;
  FBytes[LIndex] := AValue;
  if LIndex >= FSize then begin
    FSize := LIndex+1;
  end;
end;

procedure TIdBuffer.Write(const AValue: TIdIPv6Address; const ADestIndex: Integer);
var
  LVal : TIdIPv6Address;
  LIndex : Integer;
begin
  if ADestIndex < 0 then
  begin
    LIndex := FHeadIndex + Size;
    SetLength(FBytes, LIndex + 16);
  end else
  begin
    LIndex := ADestIndex;
  end;
  LVal := GStack.HostToNetwork(AValue);
  CopyTIdIPV6Address(LVal, FBytes, LIndex);
  if LIndex >= FSize then begin
    FSize := LIndex+16;
  end;
end;

procedure TIdBuffer.Write(const AValue: TIdUInt64; const ADestIndex: Integer);
var
  LVal: TIdUInt64;
  LIndex: Integer;
begin
  if ADestIndex < 0 then
  begin
    LIndex := FHeadIndex + Size;
    SetLength(FBytes, LIndex + 8);
  end else
  begin
    LIndex := ADestIndex;
  end;
  LVal := GStack.HostToNetwork(AValue);
  CopyTIdUInt64(LVal, FBytes, LIndex);
  if LIndex >= FSize then begin
    FSize := LIndex + 8;
  end;
end;

procedure TIdBuffer.Write(const AValue: UInt32; const ADestIndex: Integer);
var
  LVal : UInt32;
  LIndex : Integer;
begin
  if ADestIndex < 0 then
  begin
    LIndex := FHeadIndex + Size;
    SetLength(FBytes, LIndex + 4);
  end else
  begin
    LIndex := ADestIndex;
  end;
  LVal := GStack.HostToNetwork(AValue);
  CopyTIdUInt32(LVal, FBytes, LIndex);
  if LIndex >= FSize then begin
    FSize := LIndex+4;
  end;
end;

procedure TIdBuffer.Write(const ABytes: TIdBytes; const ALength, AOffset : Integer;
  const ADestIndex: Integer = -1);
var
  LByteLength: Integer;
  LIndex : Integer;
begin
  LByteLength := IndyLength(ABytes, ALength, AOffset);
  if LByteLength = 0 then begin
    Exit;
  end;
  LIndex := IndyMax(ADestIndex, 0);
  CheckAdd(LByteLength, LIndex);
  if Size = 0 then begin
    FHeadIndex := 0;
    if ADestIndex < 0 then
    begin
      FBytes := ToBytes(ABytes, LByteLength, AOffset);
      FSize := LByteLength;
    end else
    begin
      FSize := ADestIndex + LByteLength;
      SetLength(FBytes, FSize);
      CopyTIdBytes(ABytes, AOffset, FBytes, ADestIndex, LByteLength);
    end;
  end
  else if ADestIndex < 0 then
  begin
    CompactHead(False);
    if (Capacity - Size - FHeadIndex) < LByteLength then begin
      SetLength(FBytes, Size + LByteLength + GrowthFactor);
    end;
    CopyTIdBytes(ABytes, AOffset, FBytes, FHeadIndex + Size, LByteLength);
    Inc(FSize, LByteLength);
  end else
  begin
    CopyTIdBytes(ABytes, AOffset, FBytes, LIndex, LByteLength);
    if LIndex >= FSize then begin
      FSize := LIndex + LByteLength;
    end;
  end;
end;

function TIdBuffer.GetAsString: string;
begin
  Result := BytesToString(FBytes, FByteEncoding
    {$IFDEF STRING_IS_ANSI}, FAnsiEncoding{$ENDIF}
    );
end;

end.



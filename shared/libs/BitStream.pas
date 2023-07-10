unit BitStream;

interface

uses
  // Library units
  fgl;


type
  TBitReader = class
  private
    FBuffer: PByte;
    FBufferBytes: Longint;
    FBitPos: Longint;
  public
    constructor Create(Buffer: PByte; BufferBytes: LongWord);
    function ReadUInt8(nbits: Integer = 8): Byte;
    function ReadUInt16(nbits: Integer = 16): Word;
    function ReadUInt32(nbits: Integer = 32): LongWord;
    function ReadInt8(nbits: Integer = 8): ShortInt;
    function ReadInt16(nbits: Integer = 16): SmallInt;
    function ReadInt32(nbits: Integer = 32): LongInt;
    function ReadSingle(): Single;
    function ReadBoolean(): Boolean;
    function ReadString(len: Integer = 0): RawByteString;
    procedure Skip(nbits: Integer);
    property BitPos: Longint read FBitPos write FBitPos;
    property Buffer: PByte read FBuffer;
  end;

  TBitWriter = class
  private
    FBuffers: TFPGList<PByte>;
    FBuffer: PByte;
    FBufferBytes: Longint;
    FBitPos: Longint;
    procedure AddBuffer();
  public
    constructor Create(BufferBytes: LongWord);
    destructor Destroy(); override;
    procedure CloneBuffer(out Buffer: PByte; out SizeInBytes: LongWord);
    procedure WriteUInt8(val: Byte; nbits: Integer = 8);
    procedure WriteUInt16(val: Word; nbits: Integer = 16);
    procedure WriteUInt32(val: LongWord; nbits: Integer = 32);
    procedure WriteInt8(val: ShortInt; nbits: Integer = 8);
    procedure WriteInt16(val: SmallInt; nbits: Integer = 16);
    procedure WriteInt32(val: LongInt; nbits: Integer = 32);
    procedure WriteSingle(val: Single);
    procedure WriteBoolean(val: Boolean);
    procedure WriteString(val: RawByteString; len: Integer = 0);
    property BitPos: Longint read FBitPos;
  end;


implementation

uses
  // System units
  Math,
  SysUtils;


////////////////////////////////////////////////////////////////////////////////
// Read/Write helper functions

type
  TBitRW<T> = class
    class function Read(bs: TBitReader; n: Integer): T;
    class procedure Write(bs: TBitWriter; val: T; n: Integer);
  end;

class function TBitRW<T>.Read(bs: TBitReader; n: Integer): T;
var
  i, idx: Integer;
begin
  Result := 0;

  if ((bs.FBitPos + n) <= (bs.FBufferBytes shl 3)) then
  begin
    n := Min(n, sizeof(T) << 3);

    for i := 0 to n - 1 do
    begin
      idx := (bs.FBitPos + i) shr 3;
      Result := (Result shl 1) or ((bs.FBuffer[idx] shr (7 - ((bs.FBitPos + i) and 7))) and T(1));
    end;

    Inc(bs.FBitPos, n);
  end;
end;

class procedure TBitRW<T>.Write(bs: TBitWriter; val: T; n: Integer);
var
  i, m, idx: Integer;
begin
  n := Min(n, sizeof(T) shl 3);
  m := Min(n, (bs.FBufferBytes shl 3) - bs.FBitPos);

  for i := 0 to m - 1 do
  begin
    idx := (bs.FBitPos + i) shr 3;

    if ((val shr (n - i - 1)) and T(1)) <> 0 then
      bs.FBuffer[idx] := bs.FBuffer[idx] or (Byte(1) shl (7 - ((bs.FBitPos + i) and 7)))
    else
      bs.FBuffer[idx] := bs.FBuffer[idx] and not (Byte(1) shl (7 - ((bs.FBitPos + i) and 7)));
  end;

  Inc(bs.FBitPos, m);

  if n > m then
  begin
    bs.AddBuffer();

    for i := 0 to (n - m) - 1 do
    begin
      idx := (bs.FBitPos + i) shr 3;

      if ((val shr (n - (i + m) - 1)) and T(1)) <> 0 then
        bs.FBuffer[idx] := bs.FBuffer[idx] or (Byte(1) shl (7 - ((bs.FBitPos + i) and 7)))
      else
        bs.FBuffer[idx] := bs.FBuffer[idx] and not (Byte(1) shl (7 - ((bs.FBitPos + i) and 7)));
    end;

    Inc(bs.FBitPos, n - m);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TBitWriter

constructor TBitWriter.Create(BufferBytes: LongWord);
begin
  FBuffers := TFPGList<PByte>.Create();
  FBuffer := nil;
  FBufferBytes := BufferBytes;
  FBitPos := 0;
  AddBuffer();
end;

destructor TBitWriter.Destroy();
var
  i: Integer;
begin
  for i := 0 to FBuffers.Count - 1 do
    FreeMem(FBuffers[i]);

  FreeAndNil(FBuffers);
  inherited;
end;

procedure TBitWriter.CloneBuffer(out Buffer: PByte; out SizeInBytes: LongWord);
var
  i, n: Integer;
begin
  n := (FBitPos shr 3) + Ord((FBitPos and 7) <> 0);
  SizeInBytes := FBufferBytes * (FBuffers.Count - 1) + n;
  Buffer := nil;

  GetMem(Buffer, SizeInBytes);

  for i := 0 to FBuffers.Count - 1 do
  begin
    if i < (FBuffers.Count - 1) then
      Move(FBuffers[i]^, Buffer[i * FBufferBytes], FBufferBytes)
    else if n > 0 then
      Move(FBuffers[i]^, Buffer[i * FBufferBytes], n);
  end;
end;

procedure TBitWriter.AddBuffer();
begin
  FBitPos := 0;
  FBuffer := nil;
  GetMem(FBuffer, FBufferBytes);
  FillChar(FBuffer^, FBufferBytes, 0);
  FBuffers.Add(FBuffer);
end;

procedure TBitWriter.WriteUInt8(val: Byte; nbits: Integer = 8);
begin
  TBitRW<Byte>.Write(Self, val, nbits);
end;

procedure TBitWriter.WriteUInt16(val: Word; nbits: Integer = 16);
begin
  TBitRW<Word>.Write(Self, val, nbits);
end;

procedure TBitWriter.WriteUInt32(val: LongWord; nbits: Integer = 32);
begin
  TBitRW<LongWord>.Write(Self, val, nbits);
end;

procedure TBitWriter.WriteInt8(val: ShortInt; nbits: Integer = 8);
begin
  TBitRW<Byte>.Write(Self, PByte(@val)^, nbits);
end;

procedure TBitWriter.WriteInt16(val: SmallInt; nbits: Integer = 16);
begin
  TBitRW<Word>.Write(Self, PWord(@val)^, nbits);
end;

procedure TBitWriter.WriteInt32(val: LongInt; nbits: Integer = 32);
begin
  TBitRW<LongWord>.Write(Self, PLongWord(@val)^, nbits);
end;

procedure TBitWriter.WriteSingle(val: Single);
begin
  TBitRW<LongWord>.Write(Self, PLongWord(@val)^, 32);
end;

procedure TBitWriter.WriteBoolean(val: Boolean);
begin
  if val then
    TBitRW<Byte>.Write(Self, Byte(1), 1)
  else
    TBitRW<Byte>.Write(Self, Byte(0), 1);
end;

procedure TBitWriter.WriteString(val: RawByteString; len: Integer = 0);
var
  i, n: Integer;
begin
  n := Length(val);

  if len > 0 then
    n := Min(n, len);

  for i := 1 to n do
    TBitRW<Byte>.Write(Self, Byte(val[i]), 8);

  if len > 0 then
  begin
    for i := 1 to len - n do
      TBitRW<Byte>.Write(Self, Byte(0), 8);
  end else
  begin
    TBitRW<Byte>.Write(Self, Byte(0), 8)
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TBitReader

constructor TBitReader.Create(Buffer: PByte; BufferBytes: LongWord);
begin
  FBuffer := Buffer;
  FBufferBytes := BufferBytes;
  FBitPos := 0;
end;

procedure TBitReader.Skip(nbits: Integer);
begin
  FBitPos := FBitPos + nbits;
end;

function TBitReader.ReadUInt8(nbits: Integer = 8): Byte;
begin
  Result := TBitRW<Byte>.Read(Self, nbits);
end;

function TBitReader.ReadUInt16(nbits: Integer = 16): Word;
begin
  Result := TBitRW<Word>.Read(Self, nbits);
end;

function TBitReader.ReadUInt32(nbits: Integer = 32): LongWord;
begin
  Result := TBitRW<LongWord>.Read(Self, nbits);
end;

function TBitReader.ReadInt8(nbits: Integer = 8): ShortInt;
var
  val: Byte;
begin
  val := TBitRW<Byte>.Read(Self, nbits);
  Result := PShortInt(@val)^;
end;

function TBitReader.ReadInt16(nbits: Integer = 16): SmallInt;
var
  val: Word;
begin
  val := TBitRW<Word>.Read(Self, nbits);
  Result := PSmallInt(@val)^;
end;

function TBitReader.ReadInt32(nbits: Integer = 32): LongInt;
var
  val: LongWord;
begin
  val := TBitRW<LongWord>.Read(Self, nbits);
  Result := PLongInt(@val)^;
end;

function TBitReader.ReadSingle(): Single;
var
  val: LongWord;
begin
  val := TBitRW<LongWord>.Read(Self, 32);
  Result := PSingle(@val)^;
end;

function TBitReader.ReadBoolean(): Boolean;
begin
  Result := TBitRW<Byte>.Read(Self, 1) <> 0;
end;

function TBitReader.ReadString(len: Integer = 0): RawByteString;
var
  val: Byte;
  i: Integer;
begin
  Result := '';

  if len > 0 then
  begin
    // fixed length
    for i := 1 to len do
      Result := Result + AnsiChar(TBitRW<Byte>.Read(Self, 8));
  end else
  begin
    // null terminated
    val := TBitRW<Byte>.Read(Self, 8);
    while val <> 0 do
    begin
      Result := Result + AnsiChar(val);
      val := TBitRW<Byte>.Read(Self, 8);
    end;
  end;
end;

end.

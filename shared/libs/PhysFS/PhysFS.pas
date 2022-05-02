unit PhysFS;

interface
  uses sysutils, Classes, TraceLog;
const
{$IFDEF MSWINDOWS}
 PHYSFSLIB = 'physfs.dll';
{$ELSE}
 PHYSFSLIB = 'physfs.so';
{$ENDIF}

type PHYSFS_File = pointer;
type PHYSFS_Buffer = array of byte;

function PHYSFS_init(argv0: Pchar): LongBool; cdecl; external PHYSFSLIB;
function PHYSFS_deinit(): LongInt; cdecl; external PHYSFSLIB;
function PHYSFS_mount(newDir, mountPoint: PChar; appendToPath: LongBool) : LongBool; cdecl; external PHYSFSLIB;
function PHYSFS_openRead(filename: PChar): PHYSFS_File; cdecl; external PHYSFSLIB;
function PHYSFS_exists(filename: PChar): LongBool; cdecl; external PHYSFSLIB;
function PHYSFS_eof(pfile: PHYSFS_File): LongBool; cdecl; external PHYSFSLIB;
function PHYSFS_read(pfile: PHYSFS_File; buffer: pointer; obj_size: Longword; obj_count: Longword): Int64; cdecl; external PHYSFSLIB;
function PHYSFS_close(pfile: PHYSFS_File): Int64; cdecl; external PHYSFSLIB;
function PHYSFS_getLastError(): PChar; cdecl; external PHYSFSLIB;
function PHYSFS_fileLength(pfile: PHYSFS_File): Int64; cdecl; external PHYSFSLIB;
function PHYSFS_removeFromSearchPath(oldDir: PChar): LongBool; cdecl; external PHYSFSLIB;
procedure PHYSFS_freeList(listVar: Pointer); cdecl; external PHYSFSLIB;
function PHYSFS_enumerateFiles(const dir: PChar): PPChar; cdecl; external PHYSFSLIB;

function PHYSFS_readBuffer(Name: PChar): PHYSFS_Buffer;
function PHYSFS_readAsStream(Name: PChar): TStream;
procedure PHYSFS_ReadLn(FileHandle: PHYSFS_File; var Line: AnsiString);
function PHYSFS_CopyFileFromArchive(SourceFile: AnsiString; Destination: AnsiString): Boolean;
function PHYSFS_GetEnumeratedFiles(Dir: String): TStringArray;

implementation

function PHYSFS_readBuffer(Name: PChar): PHYSFS_Buffer;
var
  FileHandle: PHYSFS_File;
  Data: PHYSFS_Buffer;
begin
  Data := Default(PHYSFS_Buffer);
  Result := nil;
  Debug('[PhysFS] Loading file ' + Name);
  if not PHYSFS_exists(Name) then
    Exit;
  FileHandle := PHYSFS_openRead(Name);
  if FileHandle = nil then
    Exit;
  SetLength(Data, PHYSFS_fileLength(FileHandle));
  if PHYSFS_read(FileHandle, Data, 1, PHYSFS_fileLength(FileHandle)) = -1 then
    Exit;
  Result := Data;
  PHYSFS_close(FileHandle);
end;

function PHYSFS_readAsStream(Name: PChar): TStream;
var
  FileBuffer: PHYSFS_Buffer;
  FileStream: TStream;
  i: Integer;
begin
  Result := nil;
  FileBuffer := PHYSFS_readBuffer(Name);
  FileStream := TMemoryStream.Create;

  for i := 0 to Length(FileBuffer)-1 do
    FileStream.WriteByte(FileBuffer[i]);

  FileStream.Position := 0;
  Result := FileStream;
end;

procedure PHYSFS_ReadLn(FileHandle: PHYSFS_File; var Line: AnsiString);
var
  c: Char = ' ';
  b: ShortString = '';
begin
  Line := '';
  b[0] := #0;

  while (PHYSFS_read(FileHandle, @c, 1, 1) = 1) and (c <> #10) do
    if (c <> #13) then
    begin
      inc(b[0]);
      b[byte(b[0])]:= c;
      if b[0] = #255 then
      begin
        Line := Line + AnsiString(b);
        b[0]:= #0
      end
    end;
  Line := Line + AnsiString(b)
end;

function PHYSFS_CopyFileFromArchive(SourceFile: AnsiString; Destination: AnsiString): Boolean;
var
  ArchiveFile: TStream;
  SaveFile: TFileStream;
begin
  Result := False;

  if FileExists(Destination) then
    Exit;

  ArchiveFile := PHYSFS_readAsStream(PChar(SourceFile));

  if ArchiveFile.Size > 0 then
   begin
    try
      SaveFile := TFileStream.Create(Destination, fmCreate);
      SaveFile.CopyFrom(ArchiveFile, 0);
      SaveFile.Free;
      Result := True;
    except
      Result := False;
    end;
  end;

  ArchiveFile.Free;
end;

function PHYSFS_GetEnumeratedFiles(Dir: String): TStringArray;
var
  FileList: PPChar;
  FileIter: PPchar;
begin
  Result := Default(TStringArray);
  SetLength(Result, 0);

  FileList := PHYSFS_enumerateFiles(PChar(Dir));
  if FileList = Nil then
    Exit;

  FileIter := FileList;
  while FileIter^ <> Nil do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := String(FileIter^);
    Inc(FileIter);
  end;

  PHYSFS_freeList(FileList);
end;

end.

{*******************************************************}
{                                                       }
{       Util Unit for OPENSOLDAT                        }
{                                                       }
{       Copyright (c) 2003 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Util;

interface

uses
  Variants, Classes, SysUtils, Sha1 {$IFDEF DEVELOPMENT}, typinfo{$ENDIF};

type
  TColor = 0..$FFFFFFFF;

  TMapInfo = record
    Name: String;
    MapName: String;
    WorkshopID: uint64;
    Path: String;
  end;

// Gets a specific piece of a string
function GetPiece(const Source: string; const Delimiter: string;
  const Piece: Integer): string;

function Iif(const Condition: Boolean; const TruePart: Variant;
  const FalsePart: Variant): Variant;
function Choose(const Index: Integer; const Choices: array of Variant): Variant;

function ColorToHex(Color: TColor): LongWord;
function StringToColor(const S: string): TColor;

{$IFNDEF SERVER}
function NumberFormat(Num: Cardinal): string;
function CheckFileSize(filename: string): Integer;
{$ENDIF}

function OverrideFileExt(const Filename, Ext: string): string;
//function MapExists(MapName: string; RootDirectory: string{$IFNDEF SERVER}; Checksum: TSHA1Digest{$ENDIF}): Boolean;
function Md5StringHelper(Text: String): String;
function CreateDirIfMissing(const Dir: string): Boolean;
function CreateFileIfMissing(const Filename: string): Boolean;
function GetSize(Bytes: Int64): string;
function GetMapChecksum(Map: TMapInfo): TSHA1Digest;
function GetMapInfo(MapName: String; Directory: String; out MapInfo: TMapInfo): Boolean;
function VerifyMapChecksum(Map: TMapInfo; Checksum: TSHA1Digest): Boolean;
{$IFDEF DEVELOPMENT}
function ToStr(const AValue; ATypeInfo: PTypeInfo): AnsiString;
{$ENDIF}

implementation

uses
  {$IFDEF SERVER}Server,{$ELSE}Client,{$ENDIF} Constants, PhysFS, Md5, Game {$IFDEF STEAM}, Steam{$ENDIF};

function Iif(const Condition: Boolean; const TruePart: Variant;
  const FalsePart: Variant): Variant;
begin
  if Condition then
    Result := Truepart
  else
    Result := Falsepart;
end;

// ie: gamemode := string(choose(gametype, ['CTF', 'DM']));
function Choose(const Index: Integer; const Choices: array of Variant): Variant;
begin
  if Index <= High(Choices) then
    Result := Choices[index]
  else
    Result := '';
end;

function GetPiece(const Source: string; const Delimiter: string;
  const Piece: Integer): string;
var
  SplitList: TStringArray;
begin
  SplitList := Source.Split(Delimiter, Piece);
  if Piece <= Length(SplitList) then
    Result := SplitList[Piece - 1]
  else
    Result := '';
end;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);

        if X = LenSubStr then
        begin
          Result := I;
          Exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function ColorToHex(Color: TColor): LongWord;
var
  Temp: LongWord;
  Temp2: string;
begin
  Temp := Color;
  Temp2 := IntToHex(Temp, 6);
  Result := LongWord(StrToInt('$FF' + Copy(Temp2, 5, 2) + Copy(Temp2, 3, 2) + Copy(Temp2, 1, 2)));
end;

function StringToColor(const S: string): TColor;
begin
  Result := StrToInt(S);
end;

{$IFNDEF SERVER}
function NumberFormat(Num: Cardinal): string;
begin
  Result := FormatFloat('#,##0', Num);
end;

function CheckFileSize(Filename: string): Integer;
var
  Sr: TSearchRec;
begin
  Sr.Size := 0;
  FindFirst(Filename, faAnyFile, Sr);

  Result := Sr.Size;

  SysUtils.FindClose(Sr);
end;
{$ENDIF}

function OverrideFileExt(const Filename, Ext: string): string;
begin
  Result := Filename;
  if LowerCase(ExtractFileExt(Filename)) <> LowerCase(Ext) then
  begin
    Result := ChangeFileExt(Filename, Ext);

    if not PHYSFS_exists(PChar(Result)) then
      Result := Filename;
  end;
end;

//function MapExists(MapName: string; RootDirectory: string{$IFNDEF SERVER}; Checksum: TSHA1Digest{$ENDIF}): Boolean;
{begin
  Result := False;
  if PHYSFS_exists(PChar('maps/' + MapName + '.pms')) then
    Result := True
  else if FileExists(RootDirectory + 'maps/' + MapName + '.smap') then
    if Sha1Match(GetMapChecksum(MapName, RootDirectory), Checksum) then
      Result := True;
end;}

function Md5StringHelper(Text: String): String;
begin
  Result := MD5Print(MD5String(Text));
end;

// makes sure the directory exists
// returns false on error and true if everything is allright
function CreateDirIfMissing(const Dir: string): Boolean;
begin
  Result := True;

  if not DirectoryExists(Dir) then
    Result := CreateDir(Dir);
end;

function CreateFileIfMissing(const Filename: string): Boolean;
var
  FileHandle: THandle;
begin
  Result := True;

  if not FileExists(Filename) then
  begin
    FileHandle := FileCreate(Filename);
    if FileHandle = THandle(-1) then
      Result := False
    else
      // Attempting to read a file immediately after creating
      // it throws an error on Windows. We close the handle
      // manually to prevent it.
      FileClose(FileHandle);
  end;
end;

function GetSize(Bytes: Int64): String;
var
  FileSize: Int64;
begin
  FileSize := Bytes div 1024;

  if FileSize < 0.5 then
  begin
    Result := IntToStr(Bytes) + ' B';
  end else if (FileSize > 1024) then
  begin
    FileSize := FileSize div 1024;

    Result := IntToStr(FileSize) + ' Mb';
    if FileSize > 1024 then
      Result := IntToStr(FileSize div 1024) + ' Gb';
  end else
  begin
    Result := IntToStr(FileSize) + ' Kb';
  end;
end;

function VerifyMapChecksum(Map: TMapInfo; Checksum: TSHA1Digest): Boolean;
begin
  Result := False;
  if Sha1Match(GetMapChecksum(Map), Checksum) then
    Result := True;
end;

function GetMapChecksum(Map: TMapInfo): TSHA1Digest;
begin
  Result := Default(TSHA1Digest);
  if PHYSFS_exists(PChar('maps/' + Map.MapName + '.pms')) then
    Result := GameModChecksum
  else if FileExists(Map.Path) then
    Result := Sha1File(Map.Path, 4096);
end;


function GetMapInfo(MapName: String; Directory: string; out MapInfo: TMapInfo): Boolean;
var
  Split: TStringArray;
  ItemID: uint64;
  {$IFDEF STEAM}
  FileSizeOnDisk: uint64 = 0;
  //DirSizeOnDisk: uint32 = 0;
  Path: array[0..4096] of Char;
  TimeStamp: Cardinal = 0;
  Sr: TSearchRec;
  ItemName: String;
  {$ENDIF}
begin
  Result := False;
  MapInfo := Default(TMapInfo);

  if MapName.StartsWith('workshop/') then
  begin
    Split := MapName.Split('/');
    if Length(Split) >= 2 then
    begin
      ItemID := StrToIntDef(Split[1], 0);
      if ItemID > 0 then
      begin
        {$IFDEF STEAM}
        MapInfo.WorkshopID := ItemID;
        if SteamAPI.UGC.GetItemInstallInfo(ItemID, @FileSizeOnDisk, @Path, 4096, @TimeStamp) then
        begin
          if FindFirst(Path + '/*.smap', faAnyFile - faDirectory, sr) = 0 then
          begin
            ItemName := Sr.Name;
            MapInfo.MapName := ItemName.SubString(0, ItemName.Length - 5);
            MapInfo.Name := MapName + '/' + MapInfo.MapName;
            MapInfo.Path := Path + '/' + Sr.Name;
            MapInfo.WorkshopID := ItemID;
            Result := True;
          end;
        end;
        {$ELSE}
        MapInfo.WorkshopID := ItemID;
        MapInfo.MapName := Split[2];
        if FileExists(Directory + 'maps/' + MapName + '.smap') then
        begin
          MapInfo.Path := Directory + 'maps/' + MapName + '.smap';
          MapInfo.Name := MapName;
          Result := True;
        end;
        {$ENDIF}
      end;
    end;
  end else
  begin
    if PHYSFS_exists(PChar('maps/' + MapName + '.pms')) then
    begin
      MapInfo.Name := MapName;
      MapInfo.MapName := MapName;
      MapInfo.WorkshopID := 0;
      MapInfo.Path := 'smod';
      Result := True;
    end else
    begin
      if FileExists(Directory + 'maps/' + MapName + '.smap') then
      begin
        MapInfo.Name := MapName;
        MapInfo.MapName := MapName;
        MapInfo.WorkshopID := 0;
        MapInfo.Path := Directory + 'maps/' + MapName + '.smap';
        Result := True;
      end;
    end;
  end;
end;


{$IFDEF DEVELOPMENT}
// Source: https://github.com/correaelias/TypeUtils
function ToStr(const AValue; ATypeInfo: PTypeInfo): AnsiString;
type
  TArray = array of Byte;
var
  I: LongInt;
  FormatSettings: TFormatSettings;
  FirstField, Field: PManagedField;
  ElementSize: SizeInt;
begin
  FormatSettings := Default(TFormatSettings);

  case ATypeInfo^.Kind of
    tkChar: Result := QuotedStr(Char(AValue));
    tkWChar: Result := QuotedStr(AnsiString(WChar(AValue)));
    tkBool: Result := BoolToStr(Boolean(AValue), True);
    tkInt64: Result := IntToStr(Int64(AValue));
    tkQWord: Result := IntToStr(QWord(AValue));
    tkSString: Result := QuotedStr(ShortString(AValue));
    tkAString: Result := QuotedStr(AnsiString(AValue));
    tkWString: Result := QuotedStr(AnsiString(WideString(AValue)));
    tkUString: Result := QuotedStr(AnsiString(WideString(AValue)));
    tkClass: Result := TObject(AValue).ToString;
    tkEnumeration: Result := GetEnumName(ATypeInfo, Integer(AValue));
    tkSet: Result := SetToString(ATypeInfo, Integer(AValue), True).Replace(',', ', ');
    tkVariant: Result := VarToStr(Variant(AValue));
    tkInteger:
    begin
      case GetTypeData(ATypeInfo)^.OrdType of
        otSByte: Result := IntToStr(ShortInt(AValue));
        otUByte: Result := IntToStr(Byte(AValue));
        otSWord: Result := IntToStr(SmallInt(AValue));
        otUWord: Result := IntToStr(Word(AValue));
        otSLong: Result := IntToStr(LongInt(AValue));
        otULong: Result := IntToStr(LongWord(AValue));
        {$IF FPC_FULLVERSION >= 30200}
        otSQWord: Result := IntToStr(Int64(AValue));
        otUQWord: Result := IntToStr(QWord(AValue));
        {$ENDIF}
      end;
    end;
    tkFloat:
    begin
      FillByte(FormatSettings, SizeOf(TFormatSettings), 0);
      FormatSettings.DecimalSeparator := '.';
      case GetTypeData(ATypeInfo)^.FloatType of
        ftSingle: Result := FormatFloat('0.######', Single(AValue), FormatSettings);
        ftDouble: Result := FormatFloat('0.######', Double(AValue), FormatSettings);
        ftExtended: Result := FormatFloat('0.######', Extended(AValue), FormatSettings);
        ftComp: Result := FormatFloat('0.######', Comp(AValue), FormatSettings);
        ftCurr: Result := FormatFloat('0.######', Currency(AValue), FormatSettings);
      end;
    end;
    tkRecord:
    begin
      Result := '(';
      with GetTypeData(ATypeInfo)^ do
      begin
        {$IFNDEF VER3_0} //ifdef needed because of a field rename in trunk (ManagedFldCount to TotalFieldCount)
        FirstField := PManagedField(PByte(@TotalFieldCount) + SizeOf(TotalFieldCount));
        for I := 0 to TotalFieldCount - 1 do
        {$ELSE}
        FirstField := PManagedField(PByte(@ManagedFldCount) + SizeOf(ManagedFldCount));
        for I := 0 to ManagedFldCount - 1 do
        {$ENDIF}
        begin
          if I > 0 then Result += ', ';
          Field := PManagedField(PByte(FirstField) + (I * SizeOf(TManagedField)));
          Result += ToStr((PByte(@AValue) + Field^.FldOffset)^, Field^.TypeRef);
        end;
      end;
      Result += ')';
    end;
    tkArray:
    begin
      Result := '[';
      with GetTypeData(ATypeInfo)^ do
      begin
        ElementSize := ArrayData.Size div ArrayData.ElCount;
        for I := 0 to ArrayData.ElCount - 1 do
        begin
          if I > 0 then Result += ', ';
          Result += ToStr((PByte(@AValue) + (I * ElementSize))^, ArrayData.ElType);
        end;
      end;
      Result += ']';
    end;
    tkDynArray:
    begin
      Result := '[';
      with GetTypeData(ATypeInfo)^ do
      begin
        for I := 0 to Length(TArray(AValue)) - 1 do
        begin
          if I > 0 then Result += ', ';
          Result += ToStr((PByte(@TArray(AValue)[0]) + (I * ElSize))^, ElType2);
        end;
      end;
      Result += ']';
    end;
    else Result := Format('%s@%p', [ATypeInfo^.Name, @AValue]);
  end;
end;
{$ENDIF}

end.

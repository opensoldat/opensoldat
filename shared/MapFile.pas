unit MapFile;

interface

uses
  // Helper units
  Util,
  Vector,

  // Project units
  Waypoints;


type
  PMapColor = ^TMapColor;
  TMapColor = array[0..3] of Byte;  // [r,g,b,a]

  PMapVertex = ^TMapVertex;
  TMapVertex = record
    x, y, z, rhw: Single;
    Color: TMapColor;
    u, v: Single;
  end;

  PMapPolygon = ^TMapPolygon;
  TMapPolygon = record
    Vertices: array[1..3] of TMapVertex;
    Normals: array[1..3] of TVector3;
    PolyType: Byte;
    TextureIndex: Byte;
  end;

  PMapSector = ^TMapSector;
  TMapSector = record
    Polys: array of Word;
  end;

  PMapProp = ^TMapProp;
  TMapProp = record
    Active: Boolean;
    Style: Word;
    Width, Height: Integer;
    x, y: Single;
    Rotation: Single;
    ScaleX, ScaleY: Single;
    Alpha: Byte;
    Color: TMapColor;
    Level: Byte;
  end;

  PMapScenery = ^TMapScenery;
  TMapScenery = record
    Filename: string;
    Date: Integer;
  end;

  PMapCollider = ^TMapCollider;
  TMapCollider = record
    Active: Boolean;
    x, y: Single;
    Radius: Single;
  end;

  PMapSpawnpoint = ^TMapSpawnpoint;
  TMapSpawnpoint = record
    Active: Boolean;
    x, y, Team: Integer;
  end;

  PMapFile = ^TMapFile;
  TMapFile = record
    Filename: string;
    MapInfo: TMapInfo;
    Hash: LongWord;
    Version: Integer;
    MapName: string;
    Textures: array of string;
    BgColorTop: TMapColor;
    BgColorBtm: TMapColor;
    StartJet: Integer;
    GrenadePacks: Byte;
    Medikits: Byte;
    Weather: Byte;
    Steps: Byte;
    RandomID: Integer;
    Polygons: array of TMapPolygon;
    SectorsDivision: Integer;
    SectorsNum: Integer;
    Sectors: array of TMapSector;
    Props: array of TMapProp;
    Scenery: array of TMapScenery;
    Colliders: array of TMapCollider;
    Spawnpoints: array of TMapSpawnpoint;
    Waypoints: array of TWaypoint;
  end;

function LoadMapFile(MapInfo: TMapInfo; var Map: TMapFile): Boolean;
function MapColor(Color: LongInt): TMapColor;
function IsPropActive(var Map: TMapFile; Index: Integer): Boolean;


implementation

uses
  // System units
  SysUtils,

  // Library units
  PhysFS,

  // Project units
  {$IFDEF SERVER}
    Server,
  {$ELSE}
    Client,
  {$ENDIF}
  PolyMap;


{******************************************************************************}
{*                              Helper functions                              *}
{******************************************************************************}

type
  TFileBuffer = record
    Data: array of Byte;
    Pos: Integer;
  end;

const
  CRCTable: array[0..255] of LongWord = (
    $00000000,$04C11DB7,$09823B6E,$0D4326D9,$130476DC,$17C56B6B,$1A864DB2,$1E475005,
    $2608EDB8,$22C9F00F,$2F8AD6D6,$2B4BCB61,$350C9B64,$31CD86D3,$3C8EA00A,$384FBDBD,
    $4C11DB70,$48D0C6C7,$4593E01E,$4152FDA9,$5F15ADAC,$5BD4B01B,$569796C2,$52568B75,
    $6A1936C8,$6ED82B7F,$639B0DA6,$675A1011,$791D4014,$7DDC5DA3,$709F7B7A,$745E66CD,
    $9823B6E0,$9CE2AB57,$91A18D8E,$95609039,$8B27C03C,$8FE6DD8B,$82A5FB52,$8664E6E5,
    $BE2B5B58,$BAEA46EF,$B7A96036,$B3687D81,$AD2F2D84,$A9EE3033,$A4AD16EA,$A06C0B5D,
    $D4326D90,$D0F37027,$DDB056FE,$D9714B49,$C7361B4C,$C3F706FB,$CEB42022,$CA753D95,
    $F23A8028,$F6FB9D9F,$FBB8BB46,$FF79A6F1,$E13EF6F4,$E5FFEB43,$E8BCCD9A,$EC7DD02D,
    $34867077,$30476DC0,$3D044B19,$39C556AE,$278206AB,$23431B1C,$2E003DC5,$2AC12072,
    $128E9DCF,$164F8078,$1B0CA6A1,$1FCDBB16,$018AEB13,$054BF6A4,$0808D07D,$0CC9CDCA,
    $7897AB07,$7C56B6B0,$71159069,$75D48DDE,$6B93DDDB,$6F52C06C,$6211E6B5,$66D0FB02,
    $5E9F46BF,$5A5E5B08,$571D7DD1,$53DC6066,$4D9B3063,$495A2DD4,$44190B0D,$40D816BA,
    $ACA5C697,$A864DB20,$A527FDF9,$A1E6E04E,$BFA1B04B,$BB60ADFC,$B6238B25,$B2E29692,
    $8AAD2B2F,$8E6C3698,$832F1041,$87EE0DF6,$99A95DF3,$9D684044,$902B669D,$94EA7B2A,
    $E0B41DE7,$E4750050,$E9362689,$EDF73B3E,$F3B06B3B,$F771768C,$FA325055,$FEF34DE2,
    $C6BCF05F,$C27DEDE8,$CF3ECB31,$CBFFD686,$D5B88683,$D1799B34,$DC3ABDED,$D8FBA05A,
    $690CE0EE,$6DCDFD59,$608EDB80,$644FC637,$7A089632,$7EC98B85,$738AAD5C,$774BB0EB,
    $4F040D56,$4BC510E1,$46863638,$42472B8F,$5C007B8A,$58C1663D,$558240E4,$51435D53,
    $251D3B9E,$21DC2629,$2C9F00F0,$285E1D47,$36194D42,$32D850F5,$3F9B762C,$3B5A6B9B,
    $0315D626,$07D4CB91,$0A97ED48,$0E56F0FF,$1011A0FA,$14D0BD4D,$19939B94,$1D528623,
    $F12F560E,$F5EE4BB9,$F8AD6D60,$FC6C70D7,$E22B20D2,$E6EA3D65,$EBA91BBC,$EF68060B,
    $D727BBB6,$D3E6A601,$DEA580D8,$DA649D6F,$C423CD6A,$C0E2D0DD,$CDA1F604,$C960EBB3,
    $BD3E8D7E,$B9FF90C9,$B4BCB610,$B07DABA7,$AE3AFBA2,$AAFBE615,$A7B8C0CC,$A379DD7B,
    $9B3660C6,$9FF77D71,$92B45BA8,$9675461F,$8832161A,$8CF30BAD,$81B02D74,$857130C3,
    $5D8A9099,$594B8D2E,$5408ABF7,$50C9B640,$4E8EE645,$4A4FFBF2,$470CDD2B,$43CDC09C,
    $7B827D21,$7F436096,$7200464F,$76C15BF8,$68860BFD,$6C47164A,$61043093,$65C52D24,
    $119B4BE9,$155A565E,$18197087,$1CD86D30,$029F3D35,$065E2082,$0B1D065B,$0FDC1BEC,
    $3793A651,$3352BBE6,$3E119D3F,$3AD08088,$2497D08D,$2056CD3A,$2D15EBE3,$29D4F654,
    $C5A92679,$C1683BCE,$CC2B1D17,$C8EA00A0,$D6AD50A5,$D26C4D12,$DF2F6BCB,$DBEE767C,
    $E3A1CBC1,$E760D676,$EA23F0AF,$EEE2ED18,$F0A5BD1D,$F464A0AA,$F9278673,$FDE69BC4,
    $89B8FD09,$8D79E0BE,$803AC667,$84FBDBD0,$9ABC8BD5,$9E7D9662,$933EB0BB,$97FFAD0C,
    $AFB010B1,$AB710D06,$A6322BDF,$A2F33668,$BCB4666D,$B8757BDA,$B5365D03,$B1F740B4
  );

function crc32(crc: LongWord; Data: PByte; Len: Integer): LongWord;
begin
  Result := crc;
  while Len > 0 do
  begin
    Result := CRCTable[Data^ xor ((Result shr 24) and $FF)] xor (Result shl 8);
    Inc(Data);
    Dec(Len);
  end;
end;

function ReadAllBytes(Map: TMapInfo; var Buffer: TFileBuffer): Boolean;
begin
  Result := False;

  if Map.Name = '' then
    Exit;

  // Load default map from base archive
  if PHYSFS_exists(PChar('maps/' + Map.Name + '.pms')) then
  begin
    Buffer.Data := PHYSFS_readBuffer(PChar('maps/' + Map.Name + '.pms'));
  end else if PHYSFS_exists(PChar('maps/' + Map.Name + '.PMS')) then
  begin
    Buffer.Data := PHYSFS_readBuffer(PChar('maps/' + Map.Name + '.PMS'));
  end else
  begin
    // Unmount previous map
    PHYSFS_removeFromSearchPath(PChar('/current_map'));
    // Mount new map
    if not PhysFS_mount(PChar(Map.Path), '/current_map', True) then
      Exit;
    // Read PMS file
    Buffer.Data := PHYSFS_readBuffer(PChar('current_map/maps/' + Map.MapName + '.pms'));
    if Buffer.Data = nil then
      Buffer.Data := PHYSFS_readBuffer(PChar('current_map/maps/' + Map.MapName + '.PMS'));
  end;

  if Buffer.Data = nil then
    Exit;

  Buffer.Pos := 0;
  Result := True;
end;

procedure BufferRead(var bf: TFileBuffer; Dest: PByte; Size: Integer);
begin
  FillChar(Dest^, Size, 0);
  if (bf.Pos + Size) <= Length(bf.Data) then
    Move(bf.Data[bf.Pos], Dest^, Size);
  Inc(bf.Pos, Size);
end;

function ReadUint8(var bf: TFileBuffer): Byte;
begin
  BufferRead(bf, @Result, 1);
end;

function ReadUint16(var bf: TFileBuffer): Word;
begin
  BufferRead(bf, @Result, 2);
end;

function ReadInt32(var bf: TFileBuffer): LongInt;
begin
  BufferRead(bf, @Result, 4);
end;

function ReadSingle(var bf: TFileBuffer): Single;
begin
  BufferRead(bf, @Result, 4);
end;

function ReadString(var bf: TFileBuffer; MaxSize: Integer): string;
var
  n: Integer;
  s: array[0..128] of AnsiChar;
begin
  Result := '';
  n := ReadUint8(bf);

  if (n < Length(s)) and (n <= MaxSize) then
  begin
    BufferRead(bf, @s[0], MaxSize);
    s[n] := #0;
    Result := PAnsiChar(@s[0]);
  end
  else
  begin
    Inc(bf.Pos, MaxSize);
  end;
end;

function ReadVec3(var bf: TFileBuffer): TVector3;
begin
  Result.x := ReadSingle(bf);
  Result.y := ReadSingle(bf);
  Result.z := ReadSingle(bf);
end;

function ReadColor(var bf: TFileBuffer): TMapColor;
begin
  Result[2] := ReadUint8(bf);
  Result[1] := ReadUint8(bf);
  Result[0] := ReadUint8(bf);
  Result[3] := ReadUint8(bf);
end;

function ReadVertex(var bf: TFileBuffer): TMapVertex;
begin
  Result.x     := ReadSingle(bf);
  Result.y     := ReadSingle(bf);
  Result.z     := ReadSingle(bf);
  Result.rhw   := ReadSingle(bf);
  Result.Color := ReadColor(bf);
  Result.u     := ReadSingle(bf);
  Result.v     := ReadSingle(bf);
end;

function MapColor(Color: LongInt): TMapColor;
begin
  Result[0] := (Color shr (0 * 8)) and $FF;
  Result[1] := (Color shr (1 * 8)) and $FF;
  Result[2] := (Color shr (2 * 8)) and $FF;
  Result[3] := (Color shr (3 * 8)) and $FF;
end;

{******************************************************************************}
{*                                LoadMapFile                                 *}
{******************************************************************************}

function LoadMapFile(MapInfo: TMapInfo; var Map: TMapFile): Boolean;
var
  bf: TFileBuffer;
  i, j, n, m: Integer;
begin
  Result := False;

  bf := Default(TFileBuffer);
  if not ReadAllBytes(MapInfo, bf) then
    Exit;

  // header/options

  SetLength(Map.Textures, 1);

  Map.MapInfo      := MapInfo;
  Map.Filename     := MapInfo.Name;
  Map.Version      := ReadInt32(bf);
  Map.MapName      := ReadString(bf, 38);
  Map.Textures[0]  := ReadString(bf, 24);
  Map.BgColorTop   := ReadColor(bf);
  Map.BgColorBtm   := ReadColor(bf);
  Map.StartJet     := ReadInt32(bf);
  Map.GrenadePacks := ReadUint8(bf);
  Map.Medikits     := ReadUint8(bf);
  Map.Weather      := ReadUint8(bf);
  Map.Steps        := ReadUint8(bf);
  Map.RandomID     := ReadInt32(bf);

  // polygons

  n := ReadInt32(bf);

  if (n > MAX_POLYS) or (n < 0) then
    Exit;

  SetLength(Map.Polygons, n);

  for i := 0 to n - 1 do
  begin
    Map.Polygons[i].Vertices[1]  := ReadVertex(bf);
    Map.Polygons[i].Vertices[2]  := ReadVertex(bf);
    Map.Polygons[i].Vertices[3]  := ReadVertex(bf);
    Map.Polygons[i].Normals[1]   := ReadVec3(bf);
    Map.Polygons[i].Normals[2]   := ReadVec3(bf);
    Map.Polygons[i].Normals[3]   := ReadVec3(bf);
    Map.Polygons[i].PolyType     := ReadUint8(bf);
    Map.Polygons[i].TextureIndex := 0;
  end;

  // sectors

  Map.SectorsDivision := ReadInt32(bf);
  Map.SectorsNum      := ReadInt32(bf);

  if (Map.SectorsNum > MAX_SECTOR) or (Map.SectorsNum < 0) then
    Exit;

  n := (2 * Map.SectorsNum + 1) * (2 * Map.SectorsNum + 1);

  SetLength(Map.Sectors, n);

  for i := 0 to n - 1 do
  begin
    m := ReadUint16(bf);

    if m > MAX_POLYS then
      Exit;

    SetLength(Map.Sectors[i].Polys, m);

    for j := 0 to m - 1 do
      Map.Sectors[i].Polys[j] := ReadUint16(bf);
  end;

  // props

  n := ReadInt32(bf);

  if (n > MAX_PROPS) or (n < 0) then
    Exit;

  SetLength(Map.Props, n);

  for i := 0 to n - 1 do
  begin
    Map.Props[i].Active   := (ReadUint8(bf) <> 0); Inc(bf.Pos, 1);
    Map.Props[i].Style    := ReadUint16(bf);
    Map.Props[i].Width    := ReadInt32(bf);
    Map.Props[i].Height   := ReadInt32(bf);
    Map.Props[i].x        := ReadSingle(bf);
    Map.Props[i].y        := ReadSingle(bf);
    Map.Props[i].Rotation := ReadSingle(bf);
    Map.Props[i].ScaleX   := ReadSingle(bf);
    Map.Props[i].ScaleY   := ReadSingle(bf);
    Map.Props[i].Alpha    := ReadUint8(bf); Inc(bf.Pos, 3);
    Map.Props[i].Color    := ReadColor(bf);
    Map.Props[i].Level    := ReadUint8(bf); Inc(bf.Pos, 3);
  end;

  // scenery

  n := ReadInt32(bf);

  if (n > MAX_PROPS) or (n < 0) then
    Exit;

  SetLength(Map.Scenery, n);

  for i := 0 to n - 1 do
  begin
    Map.Scenery[i].Filename := ReadString(bf, 50);
    Map.Scenery[i].Date     := ReadInt32(bf);
  end;

  // colliders

  n := ReadInt32(bf);

  if (n > MAX_COLLIDERS) or (n < 0) then
    Exit;

  SetLength(Map.Colliders, n);

  for i := 0 to n - 1 do
  begin
    Map.Colliders[i].Active := (ReadUint8(bf) <> 0); Inc(bf.Pos, 3);
    Map.Colliders[i].x      := ReadSingle(bf);
    Map.Colliders[i].y      := ReadSingle(bf);
    Map.Colliders[i].Radius := ReadSingle(bf);
  end;

  // spawnpoints

  n := ReadInt32(bf);

  if (n > MAX_SPAWNPOINTS) or (n < 0) then
    Exit;

  SetLength(Map.Spawnpoints, n);

  for i := 0 to n - 1 do
  begin
    Map.Spawnpoints[i].Active := (ReadUint8(bf) <> 0); Inc(bf.Pos, 3);
    Map.Spawnpoints[i].x      := ReadInt32(bf);
    Map.Spawnpoints[i].y      := ReadInt32(bf);
    Map.Spawnpoints[i].Team   := ReadInt32(bf);
  end;

  // waypoints

  n := ReadInt32(bf);

  if (n > MAX_WAYPOINTS) or (n < 0) then
    Exit;

  SetLength(Map.Waypoints, n);

  for i := 0 to n - 1 do
  begin
    Map.Waypoints[i].Active         := (ReadUint8(bf) <> 0); Inc(bf.Pos, 3);
    Map.Waypoints[i].id             := ReadInt32(bf);
    Map.Waypoints[i].x              := ReadInt32(bf);
    Map.Waypoints[i].y              := ReadInt32(bf);
    Map.Waypoints[i].Left           := (ReadUint8(bf) <> 0);
    Map.Waypoints[i].Right          := (ReadUint8(bf) <> 0);
    Map.Waypoints[i].Up             := (ReadUint8(bf) <> 0);
    Map.Waypoints[i].Down           := (ReadUint8(bf) <> 0);
    Map.Waypoints[i].Jetpack        := (ReadUint8(bf) <> 0);
    Map.Waypoints[i].PathNum        := ReadUint8(bf);
    Map.Waypoints[i].Action         := TWaypointAction(ReadUint8(bf));
    Inc(bf.Pos, 5);
    Map.Waypoints[i].ConnectionsNum := ReadInt32(bf);

    for j := 1 to MAX_CONNECTIONS do
      Map.Waypoints[i].Connections[j] := ReadInt32(bf);
  end;

  Map.Hash := crc32(5381, @bf.Data[0], Length(bf.Data));
  Result := True;
end;

function IsPropActive(var Map: TMapFile; Index: Integer): Boolean;
var
  Prop: ^TMapProp;
begin
  Prop := @Map.Props[Index];
  Result := Prop.Active and (Prop.Level <= 2) and (Prop.Style > 0) and
    (Prop.Style <= Length(Map.Scenery));
end;

end.

{*******************************************************}
{                                                       }
{       BinPack Unit for OPENSOLDAT                     }
{                                                       }
{       Copyright (c) 2015 Mariano Cuatrin              }
{                                                       }
{*******************************************************}

unit BinPack;

interface

type
  PBPRect = ^TBPRect;
  TBPRect = record
    x, y: Integer;
    w, h: Integer;
    Data: Pointer;
  end;

  TBPRectArray = array of TBPRect;

function PackRects(w, h: Integer; var Rects: TBPRectArray): Integer;

implementation

// Internal types

type
  TRectList = class
    private
      FData: PBPRect;
      FSize: Integer;
      FCapacity: Integer;

      function GetValue(Index: Integer): TBPRect;
      procedure SetValue(Index: Integer; const Value: TBPRect);

    public
      constructor Create;
      destructor Destroy; override;
      procedure Push(const Rect: TBPRect);
      procedure Remove(Index: Integer);

      property Size: Integer read FSize;
      property Items[Index: Integer]: TBPRect read GetValue write SetValue;
        default;
  end;

  TBinPack = record
    Used: TRectList;
    Free: TRectList;
  end;

// Forward declarations of internal functions

function ScoreRect(var bp: TBinPack; w, h: Integer; var BestY, BestX: Integer): TBPRect;
  forward;
procedure PlaceRect(var bp: TBinPack; const Rect: TBPRect);
  forward;
function SplitFreeRect(var bp: TBinPack; FreeRect: TBPRect; const UsedRect: TBPRect): Boolean;
  forward;
procedure PruneFreeList(var bp: TBinPack);
  forward;
function IsContainedIn(const a, b: TBPRect): Boolean;
  forward;

// PackRects

function PackRects(w, h: Integer; var Rects: TBPRectArray): Integer;
var
  bp: TBinPack;
  i, j: Integer;
  Rect: TBPRect;
  BestRect: TBPRect;
  BestIndex: Integer;
  BestScore1: Integer;
  BestScore2: Integer;
  Score1: Integer;
  Score2: Integer;
begin
  BestRect := Default(TBPRect);
  Rect.x := 0;
  Rect.y := 0;
  Rect.w := w;
  Rect.h := h;
  Score1 := 0;
  Score2 := 0;

  bp.Used := TRectList.Create;
  bp.Free := TRectList.Create;
  bp.Free.Push(Rect);

  i := 0;

  while i <= High(Rects) do
  begin
    BestIndex := -1;
    BestScore1 := MaxInt;
    BestScore2 := MaxInt;

    for j := i to High(Rects) do
    begin
      Rect := ScoreRect(bp, Rects[j].w, Rects[j].h, Score1, Score2);

      if (Score1 < BestScore1) or ((Score1 = BestScore1) and (Score2 < BestScore2)) then
      begin
        BestScore1 := Score1;
        BestScore2 := Score2;
        BestRect := Rect;
        BestIndex := j;
      end;
    end;

    if (BestRect.h = 0) or (BestIndex = -1) then
      Break;

    PlaceRect(bp, BestRect);

    BestRect.Data := Rects[BestIndex].Data;
    Rects[BestIndex] := Rects[i];
    Rects[i] := BestRect;

    Inc(i);
  end;

  bp.Used.Free;
  bp.Free.Free;

  Result := i;
end;

// Internal functions

function ScoreRect(var bp: TBinPack; w, h: Integer; var BestY, BestX: Integer): TBPRect;
var
  BestRect: TBPRect;
  TopSideY: Integer;
  i: Integer;
begin
  BestRect.x := 0;
  BestRect.y := 0;
  BestRect.w := 0;
  BestRect.h := 0;

  BestX := MaxInt;
  BestY := MaxInt;

  for i := 0 to bp.Free.Size - 1 do
  begin
    if (bp.Free[i].w >= w) and (bp.Free[i].h >= h) then
    begin
      TopSideY := bp.Free[i].y + h;

      if (TopSideY < BestY) or ((TopSideY = BestY) and (bp.Free[i].x < BestX)) then
      begin
        BestRect.x := bp.Free[i].x;
        BestRect.y := bp.Free[i].y;
        BestRect.w := w;
        BestRect.h := h;
        BestY := TopSideY;
        BestX := bp.Free[i].x;
      end;
    end;

    // Note: this part will be enabled later on when I add support for
    // rotated sprites in the texture atlas. That will be done when I get
    // more control over the actual texture coordinates.
    {
    if (bp.Free[i].w >= h) and (bp.Free[i].h >= w) then
    begin
      TopSideY := bp.Free[i].y + w;

      if (TopSideY < BestY) or ((TopSideY = BestY) and (bp.Free[i].x < BestX)) then
      begin
        BestRect.x := bp.Free[i].x;
        BestRect.y := bp.Free[i].y;
        BestRect.w := h;
        BestRect.h := w;
        BestY := TopSideY;
        BestX := bp.Free[i].x;
      end;
    end;
    }
  end;

  if BestRect.h = 0 then
  begin
    BestX := MaxInt;
    BestY := MaxInt;
  end;

  Result := BestRect;
end;

procedure PlaceRect(var bp: TBinPack; const Rect: TBPRect);
var
  i, n: Integer;
begin
  i := 0;
  n := bp.Free.Size;

  while i < n do
  begin
    if SplitFreeRect(bp, bp.Free[i], Rect) then
    begin
      bp.Free.Remove(i);
      Dec(i);
      Dec(n);
    end;

    Inc(i);
  end;

  PruneFreeList(bp);
  bp.Used.Push(Rect);
end;

function SplitFreeRect(var bp: TBinPack; FreeRect: TBPRect; const UsedRect: TBPRect): Boolean;
var
  Rect: TBPRect;
begin
  if (UsedRect.x >= (FreeRect.x + FreeRect.w)) or
    ((UsedRect.x + UsedRect.w) <= FreeRect.x) or
    (UsedRect.y >= (FreeRect.y + FreeRect.h)) or
    ((UsedRect.y + UsedRect.h) <= FreeRect.y) then
  begin
    Result := False;
    Exit;
  end;

  if (UsedRect.x < (FreeRect.x + FreeRect.w)) and
    ((UsedRect.x + UsedRect.w) > FreeRect.x) then
  begin
    if (UsedRect.y > FreeRect.y) and (UsedRect.y < (FreeRect.y + FreeRect.h)) then
    begin
      Rect := FreeRect;
      Rect.h := UsedRect.y - Rect.y;
      bp.Free.Push(Rect);
    end;

    if (UsedRect.y + UsedRect.h) < (FreeRect.y + FreeRect.h) then
    begin
      Rect := FreeRect;
      Rect.y := UsedRect.y + UsedRect.h;
      Rect.h := FreeRect.y + FreeRect.h - (UsedRect.y + UsedRect.h);
      bp.Free.Push(Rect);
    end;
  end;

  if (UsedRect.y < (FreeRect.y + FreeRect.h)) and
    ((UsedRect.y + UsedRect.h) > FreeRect.y) then
  begin
    if (UsedRect.x > FreeRect.x) and (UsedRect.x < (FreeRect.x + FreeRect.w)) then
    begin
      Rect := FreeRect;
      Rect.w := UsedRect.x - Rect.x;
      bp.Free.Push(Rect);
    end;

    if (UsedRect.x + UsedRect.w) < (FreeRect.x + FreeRect.w) then
    begin
      Rect := FreeRect;
      Rect.x := UsedRect.x + UsedRect.w;
      Rect.w := FreeRect.x + FreeRect.w - (UsedRect.x + UsedRect.w);
      bp.Free.Push(Rect);
    end;
  end;

  Result := True;
end;

procedure PruneFreeList(var bp: TBinPack);
var
  i, j, n: Integer;
begin
  i := 0;
  n := bp.Free.Size;

  while i < n do
  begin
    j := i + 1;

    while j < n do
    begin
      if IsContainedIn(bp.Free[i], bp.Free[j]) then
      begin
        bp.Free.Remove(i);
        Dec(i);
        Dec(n);
        Break;
      end;

      if IsContainedIn(bp.Free[j], bp.Free[i]) then
      begin
        bp.Free.Remove(j);
        Dec(j);
        Dec(n);
      end;

      Inc(j);
    end;

    Inc(i);
  end;
end;

function IsContainedIn(const a, b: TBPRect): Boolean;
begin
  Result := (a.x >= b.x) and (a.y >= b.y) and ((a.x + a.w) <= (b.x + b.w)) and
    ((a.y + a.h) <= (b.y + b.h));
end;

// TRectList class

procedure RectMemCopy(a, b: PBPRect; n: Integer);
var
  i: Integer;
begin
  for i := 0 to n - 1 do
  begin
    a^ := b^;
    Inc(a);
    Inc(b);
  end;
end;

constructor TRectList.Create;
begin
  FSize := 0;
  FCapacity := 16;
  GetMem(FData, FCapacity * sizeof(TBPRect));
end;

destructor TRectList.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

function TRectList.GetValue(Index: Integer): TBPRect;
var
  p: PBPRect;
begin
  p := FData;
  Inc(p, Index);
  Result := p^;
end;

procedure TRectList.SetValue(Index: Integer; const Value: TBPRect);
var
  p: PBPRect;
begin
  p := FData;
  Inc(p, Index);
  p^ := Value;
end;

procedure TRectList.Push(const Rect: TBPRect);
var
  p: PBPRect;
begin
  if FSize = FCapacity then
  begin
    FCapacity := 2 * FCapacity;
    GetMem(p, FCapacity * sizeof(TBPRect));
    RectMemCopy(p, FData, FSize);
    FreeMem(FData);
    FData := p;
  end;

  p := FData;
  Inc(p, FSize);
  Inc(FSize);
  p^ := Rect;
end;

procedure TRectList.Remove(Index: Integer);
var
  a, b: PBPRect;
begin
  a := FData;
  b := FData;

  Inc(a, Index);
  Inc(b, Index + 1);

  RectMemCopy(a, b, FSize - Index - 1);
  Dec(FSize);
end;

end.

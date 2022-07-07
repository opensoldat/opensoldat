{*******************************************************}
{                                                       }
{       Calc Unit for OPENSOLDAT                        }
{                                                       }
{       Copyright (c) 2001 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Calc;

interface

uses
  Vector;

type
  TIntersectionResult = record
    Points: array [0..1] of TVector2;
    NumIntersections: Byte;
  end;

function IsLineIntersectingCircle(Line1, Line2, CircleCenter: TVector2;
    Radius: Single): TIntersectionResult;
function LineCircleCollision(StartPoint, EndPoint, CircleCenter: TVector2;
    Radius: Single; var CollisionPoint: TVector2): Boolean;
function PointLineDistance(P1, P2, P3: TVector2): Single;
function Angle2Points(const P1, P2: TVector2): Single;
function Distance(X1, Y1, X2, Y2: Single): Single; overload;
function SqrDist(X1, Y1, X2, Y2: Single): Single; overload;
function SqrDist(P1, P2: TVector2): Single; overload;
function Distance(P1, P2: TVector2): Single; overload;
function GreaterPowerOf2(N: Integer): Integer;
function RoundFair(Value: Single): Integer;

implementation

uses
  Math;

function IsLineIntersectingCircle(Line1, Line2, CircleCenter: TVector2;
    Radius: Single): TIntersectionResult;
var
  a, a1, b, b1, c1, delta, diffx, diffy,
    sqrtdelta, a2, minx, miny, maxx, maxy, temp: Single;
  flipped: Boolean;
  Intersect: TVector2;
begin
  Result := Default(TIntersectionResult);
  FillChar(Result, SizeOf(Result), #0);

  diffx := Line2.x - Line1.x;
  diffy := Line2.y - Line1.y;

  if (abs(diffx) < 0.00001) and (abs(diffy) < 0.00001) then
    // The line is a lie!
    // On a more serious note, let's not test the limit of floating point
    Exit;

  // if the angle of the bullet is bigger than 45 degrees,
  // flip the coordinate system.
  // This algorithm deals with lines being nearly horizontal just fine,
  // but nearly vertical would cause a havoc, as vertical line is not a function.
  if abs(diffy) > abs(diffx) then
  begin
    flipped := true;
    temp := Line1.x;
    Line1.x := Line1.y;
    Line1.y := temp;

    temp := Line2.x;
    Line2.x := Line2.y;
    Line2.y := temp;

    temp := CircleCenter.x;
    CircleCenter.x := CircleCenter.y;
    CircleCenter.y := temp;

    temp := diffx;
    diffx := diffy;
    diffy := temp;
  end
  else
    flipped := false;

  // Line equation: ax + b - y = 0. given x1, y1, x2, y2, let's calculate a and b
  // a = (y1 - y2)/(x1 - x2)
  a := diffy/diffx;
  // b := y - ax
  b := Line1.y - a * Line1.x;
  // Circle equation: (x - x1)^2 + (y - y1)^2 - r^2 = 0
  // Now we need to solve: (x - x1)^2 + (y - y1)^2 - r^2 = ax + b - y
  // Simplyfing above: (a^2 + 1)x^2 + 2(ab − ay1 − x1)x + (y1^2 − r^2 + x1^2 − 2by1b^2)=0
  // now, since this is a standard Ax^2 + Bx + C equation, we find x and y using
  // x = (-B +/- sqrt(B^2 - 4ac))/(2A)
  // A = (a^2 + 1)
  a1 := sqr(a) + 1;
  // B = 2(ab - ay1 - x1)
  b1 := 2 * (a * b - a * CircleCenter.y - CircleCenter.x);
  // C = y1^2 − r^2 + x1^2 − 2by1 + b^2
  c1 := sqr(CircleCenter.y) - sqr(Radius) + sqr(CircleCenter.x) - 2 * b * CircleCenter.y + sqr(b);
  // delta = B^2 - 4AC;
  delta := sqr(b1) - 4 * a1 * c1;
  // having x1 and x2 result, we can calculate y1 and y2 from y = a * x + b

  // if delta < 0, no intersection
  if delta < 0 then
    exit;

  if Line1.x < Line2.x then
  begin
    minx := Line1.x;
    maxx := Line2.x;
  end
  else
  begin
    minx := Line2.x;
    maxx := Line1.x;
  end;

  if Line1.y < Line2.y then
  begin
    miny := Line1.y;
    maxy := Line2.y;
  end
  else
  begin
    miny := Line2.y;
    maxy := Line1.y;
  end;

  // we don't care about a case of delta = 0 as it's extremaly rare,
  // also this will handle it fine, just less effecient
  sqrtdelta := sqrt(delta);
  a2 := 2 * a1;
  Intersect.x := (-b1 - sqrtdelta) / a2;
  Intersect.y := a * Intersect.x + b;
  // we know that infinite line does intersect the circle, now let's see if our part does
  if InRange(Intersect.x, minx, maxx) and InRange(Intersect.y, miny, maxy) then
  begin
    if flipped then
    begin
      temp := Intersect.x;
      Intersect.x := Intersect.y;
      Intersect.y := temp;
    end;
    Result.Points[Result.NumIntersections] := Intersect;
    Result.NumIntersections := Result.NumIntersections + 1;
  end;

  Intersect.x := (-b1 + sqrtdelta) / a2;
  Intersect.y := a * Intersect.x + b;
  if InRange(Intersect.x, minx, maxx) and InRange(Intersect.y, miny, maxy) then
  begin
    if flipped then
    begin
      temp := Intersect.x;
      Intersect.x := Intersect.y;
      Intersect.y := temp;
    end;
    Result.Points[Result.NumIntersections] := Intersect;
    Result.NumIntersections := Result.NumIntersections + 1;
  end;
end;

function LineCircleCollision(StartPoint, EndPoint,CircleCenter: TVector2;
    Radius: Single; var CollisionPoint: TVector2): Boolean;
var
  r2: Single;
  IntersectionResult: TIntersectionResult;
begin
  r2 := sqr(Radius);

  Result := False;

  if SqrDist(StartPoint, CircleCenter) <= r2 then
  begin
    CollisionPoint := StartPoint;
    Result := True;
    Exit;
  end;

  if SqrDist(EndPoint, CircleCenter) <= r2 then
  begin
    CollisionPoint := EndPoint;
    Result := True;
    Exit;
  end;

  IntersectionResult := IsLineIntersectingCircle(StartPoint, EndPoint, CircleCenter, Radius);
  if IntersectionResult.NumIntersections > 0 then
  begin
    Result := True;
    CollisionPoint := IntersectionResult.Points[0];
    if (IntersectionResult.NumIntersections = 2) and
       (SqrDist(IntersectionResult.Points[0], StartPoint) > SqrDist(IntersectionResult.Points[1], StartPoint)) then
      CollisionPoint := IntersectionResult.Points[1];
  end;
end;

function PointLineDistance(P1, P2, P3: TVector2): Single;
var
  U, X, Y: Single;
begin
  U := ((P3.X - P1.X) * (P2.X - P1.X) + (P3.Y - P1.Y) *
    (P2.Y - P1.Y)) / (Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));

  X := P1.X + U * (P2.X - P1.X);
  Y := P1.Y + U * (P2.Y - P1.Y);

  Result := Sqrt(Sqr(X - P3.X) + Sqr(Y - P3.Y));
end;

function Angle2Points(const P1, P2: TVector2): Single;
begin
  if (P2.X - P1.X) <> 0 then
  begin
    if P1.X > P2.X then
      Result := ArcTan((P2.Y - P1.Y) / (P2.X - P1.X)) + Pi
    else
      Result := ArcTan((P2.Y - P1.Y) / (P2.X - P1.X));
  end else
  begin
    if P2.Y > P1.Y then
      Result := Pi / 2
    else if P2.Y < P1.Y then
      Result := -Pi / 2
    else
      Result := 0;
  end;
end;

function Distance(X1, Y1, X2, Y2: Single): Single;
begin
  Result := Sqrt(Sqr(X1 - X2) + Sqr(Y1 - Y2));
end;

function Distance(P1, P2: TVector2): Single;
begin
  Result := Sqrt(Sqr(P1.x - P2.x) + Sqr(P1.y - P2.y));
end;

function SqrDist(X1, Y1, X2, Y2: Single): Single; overload;
begin
  Result := Sqr(X1 - X2) + Sqr(Y1 - Y2);
end;

function SqrDist(P1, P2: TVector2): Single; overload;
begin
  Result := Sqr(P1.x - P2.x) + Sqr(P1.y - P2.y);
end;

function GreaterPowerOf2(N: Integer): Integer;
begin
  // 2 ^ roundup(log2(n))
  Result := Trunc(Power(2, Ceil(Log2(N))));
end;

// Rounds, but witout that "Banker's rule" that prefers even numbers.
function RoundFair(Value: Single): Integer;
begin
  Result := Floor(Value + 0.5);
end;

end.

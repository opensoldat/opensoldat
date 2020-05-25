{*******************************************************}
{                                                       }
{       Waypoints Unit for SOLDAT                       }
{                                                       }
{       Copyright (c) 2002 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Waypoints;

interface

const
  MAX_WAYPOINTS = 5000;
  MAX_CONNECTIONS = 20;

type
  TWaypoint = record
    Active: Boolean;
    id: Integer;
    X, Y: Integer;
    left, right, up, down, m2: Boolean;
    // PathNum: Integer;
    PathNum: Byte;
    C1, C2, C3: Byte;
    ConnectionsNum: Integer;
    Connections: array[1..MAX_CONNECTIONS] of Integer;
  end;

type
  TWaypoints = object
    Waypoint: array[1..MAX_WAYPOINTS] of TWaypoint;

    procedure LoadFromFile(filename: string);
    procedure SaveToFile(filename: string);
    function FindClosest(X, Y: Single; Radius, CurrWaypoint: Integer): Integer;
    function CreateWayPoint(sX, sY, sPath: Integer): Integer;
  end;

  // for file use
  TPathRec = record
    Waypoint: array[1..500] of TWaypoint;
  end;

implementation

uses
  Calc, SysUtils;

procedure TWaypoints.LoadFromFile(filename: string);
var
  AddrFile: File of TPathRec;
  AddrRec: TPathRec;
  i, j: Integer;
begin
  AssignFile(AddrFile, Filename);
  FileMode := fmOpenRead;
  Reset(AddrFile);
  Read(AddrFile, AddrRec);
  CloseFile(AddrFile);

  for i := 1 to MAX_WAYPOINTS do
    Waypoint[i] := AddrRec.Waypoint[i];

  for i := 1 to MAX_WAYPOINTS do
    if not Waypoint[i].Active then
    begin
      Waypoint[i].ConnectionsNum := 0;
      for j := 1 to MAX_CONNECTIONS do
      begin
        Waypoint[i].Connections[j] := 0;
      end;
    end;
end;

procedure TWaypoints.SaveToFile(filename: string);
var
  AddrFile: File of TPathRec;
  AddrRec: TPathRec;
  i: Integer;
begin
  for i := 1 to MAX_WAYPOINTS do
    AddrRec.Waypoint[i] := Waypoint[i];

  AssignFile(AddrFile, Filename);
  FileMode := fmOpenReadWrite;
  Rewrite(AddrFile);
  Write(AddrFile, AddrRec);
  CloseFile(AddrFile);
end;

function TWaypoints.FindClosest(X, Y: Single; Radius, CurrWaypoint: Integer):
  Integer;
var
  d: Single;
  i: Integer;
begin
  Result := 0;

  for i := 1 to MAX_WAYPOINTS do
    if (Waypoint[i].Active) and (CurrWaypoint <> i) then
    begin
      d := Distance(X, Y, Waypoint[i].X, Waypoint[i].Y);
      if D < Radius then
      begin
        Result := i;
        Exit;
      end;
    end;
end;

function TWaypoints.CreateWayPoint(sX, sY, sPath: Integer): Integer;
var
  i: Integer;
begin
  for i := 1 to MAX_WAYPOINTS + 1 do
  begin
    if i = MAX_WAYPOINTS + 1 then
    begin
      result := -1;
      Exit;
    end;
    if not Waypoint[i].Active then
      break;
  end;
  // i is now the active waypoint

  Waypoint[i].Active := true;
  Waypoint[i].X := sX;
  Waypoint[i].Y := sY;
  Waypoint[i].id := i;
  Waypoint[i].ConnectionsNum := 0;
  Waypoint[i].PathNum := sPath;

  // activate waypoint
  Result := i;
end;

end.
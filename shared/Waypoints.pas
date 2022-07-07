{*******************************************************}
{                                                       }
{       Waypoints Unit for OPENSOLDAT                   }
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
  {$scopedenums on}
  TWaypointAction = (None, StopAndCamp, Wait1Second, Wait5Seconds, Wait10Seconds, Wait15Seconds, Wait20Seconds);
  TWaypoint = record
    Active: Boolean;
    Id: Integer;
    X, Y: Integer;
    Left, Right, Up, Down, Jetpack: Boolean;
    PathNum: Byte;
    Action: TWaypointAction;
    ConnectionsNum: Integer;
    Connections: array[1..MAX_CONNECTIONS] of Integer;
  end;

type
  TWaypoints = object
    Waypoint: array[1..MAX_WAYPOINTS] of TWaypoint;
    function FindClosest(X, Y: Single; Radius, CurrWaypoint: Integer): Integer;
  end;

implementation

uses
  Calc, SysUtils;

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

end.

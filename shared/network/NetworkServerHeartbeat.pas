unit NetworkServerHeartbeat;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // soldat units
  Net, Sprites, Weapons, Constants;

procedure ServerHeartbeat;

implementation

uses
  Server, Game, Demo, Steam;

// HEARTBEAT
procedure ServerHeartbeat;
var
  HeartBeatMsg: TMsg_Heartbeat;
  j, c: Integer;
begin
  HeartBeatMsg.Header.ID := MsgID_HeartBeat;

  c := 0;

  for j := 1 to MAX_PLAYERS do
  begin
    HeartBeatMsg.Active[j] := false;
    HeartBeatMsg.Kills[j] := 0;
    HeartBeatMsg.Caps[j] := 0;
    HeartBeatMsg.Team[j] := 0;
    HeartBeatMsg.Deaths[j] := 0;
    HeartBeatMsg.Flags[j] := 0;
    HeartBeatMsg.Ping[j] := 255;
  end;

  for j := 1 to MAX_PLAYERS do
    if Sprite[j].Active then
    begin
      Inc(c);
      HeartBeatMsg.Active[c] := Sprite[j].Active;
      HeartBeatMsg.Kills[c] := Sprite[j].Player.Kills;
      HeartBeatMsg.Caps[c] := Sprite[j].Player.Flags;
      HeartBeatMsg.Deaths[c] := Sprite[j].Player.Deaths;
      HeartBeatMsg.Team[c] := Sprite[j].Player.Team;
      HeartBeatMsg.Flags[c] := Sprite[j].Player.Flags;
      HeartBeatMsg.Ping[c] := Sprite[j].Player.PingTicks;
      HeartBeatMsg.RealPing[c] := Sprite[j].Player.RealPing;
      HeartBeatMsg.ConnectionQuality[c] := Sprite[j].Player.ConnectionQuality;
    end;

  for j := TEAM_ALPHA to TEAM_DELTA do
    HeartBeatMsg.TeamScore[j] := TeamScore[j];

  HeartBeatMsg.MapID := Map.MapID;
  if MapChangeCounter > 0 then
    HeartBeatMsg.MapID := 0;
  if (sv_timelimit.Value - TimeLimitCounter) < 600 then
    HeartBeatMsg.MapID := 0;
  if (TimeLimitCounter) < 600 then
    HeartBeatMsg.MapID := 0;

  for j := 1 to MAX_PLAYERS do
    if (Sprite[j].Active) and (Sprite[j].Player.ControlMethod = HUMAN) then
      UDP.SendData(HeartBeatMsg, sizeof(HeartBeatMsg), Sprite[j].Player.peer, k_nSteamNetworkingSend_Unreliable);
end;

end.

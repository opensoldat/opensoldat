unit NetworkClientHeartbeat;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // opensoldat units
  LogFile, Steam, Net, Sprites, Sound, Constants, GameStrings;

procedure ClientHandleHeartBeat(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  Client, NetworkUtils, NetworkClientConnection, Game, Demo;

procedure ClientHandleHeartBeat(NetMessage: PSteamNetworkingMessage_t);
var
  HeartBeat: TMsg_HeartBeat;
  c, i: Integer;
begin
  if not VerifyPacket(sizeof(TMsg_HeartBeat), NetMessage^.m_cbSize, MsgID_HeartBeat) then
    Exit;

  HeartBeat := PMsg_Heartbeat(NetMessage^.m_pData)^;

  c := 0;
  for i := 1 to MAX_PLAYERS do
    if Sprite[i].Active and (not Sprite[i].Player.DemoPlayer) then
    begin
      Inc(c);
      Sprite[i].Active := Heartbeat.Active[c];
      Sprite[i].Player.Kills := Heartbeat.Kills[c];
      Sprite[i].Player.Flags := Heartbeat.Caps[c];
      Sprite[i].Player.Team := Heartbeat.Team[c];
      Sprite[i].Player.Deaths := Heartbeat.Deaths[c];
      Sprite[i].Player.Flags := Heartbeat.Flags[c];
      Sprite[i].Player.PingTicks := Heartbeat.Ping[c];
      Sprite[i].Player.PingTime := Sprite[i].Player.PingTicks * 1000 div 60;
      Sprite[i].Player.RealPing := Heartbeat.RealPing[c];
      Sprite[i].Player.ConnectionQuality := Heartbeat.ConnectionQuality[c];
    end;

  // play bding sound
  if sv_gamemode.Value = GAMESTYLE_INF then
    if Heartbeat.TeamScore[TEAM_BRAVO] > TeamScore[TEAM_BRAVO] then
      if Heartbeat.TeamScore[TEAM_BRAVO] mod 5 = 0 then
        PlaySound(SFX_INFILT_POINT);

  if sv_gamemode.Value = GAMESTYLE_HTF then
  begin
    if Heartbeat.TeamScore[TEAM_ALPHA] > TeamScore[TEAM_ALPHA] then
      if Heartbeat.TeamScore[TEAM_ALPHA] mod 5 = 0 then
        PlaySound(SFX_INFILT_POINT);
    if Heartbeat.TeamScore[TEAM_BRAVO] > TeamScore[TEAM_BRAVO] then
      if Heartbeat.TeamScore[TEAM_BRAVO] mod 5 = 0 then
        PlaySound(SFX_INFILT_POINT);
  end;

  for i := TEAM_ALPHA to TEAM_DELTA do
    TeamScore[i] := Heartbeat.TeamScore[i];

  // MapID differs, map not changed
  if (MapChangeCounter < 0) and (Heartbeat.MapID <> 0) and
    (Heartbeat.MapID <> Map.MapID) and (not DemoPlayer.Active) then
  begin
    Dec(BadMapIDCount);
  end
  else
    BadMapIDCount := 2;

  if BadMapIDCount < 1 then
  begin
    MainConsole.Console(_('Wrong map version detected'), SERVER_MESSAGE_COLOR);
    ClientDisconnect;
    MapChangeCounter := -60;
    Exit;
  end;

  if Connection = Internet then
    if (MainTickCounter - HeartBeatTime) > 350 then
    begin
      Inc(HeartbeatTimeWarnings);
    end
    else if HeartbeatTimeWarnings > 0 then
      Dec(HeartbeatTimeWarnings);

  HeartBeatTime := MainTickCounter;

  SortPlayers;
end;

end.

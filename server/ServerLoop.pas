unit ServerLoop;

interface

procedure UpdateFrame;
procedure AppOnIdle;

implementation

uses
  {$IFDEF SCRIPT}
  ScriptDispatcher,
  {$ENDIF}
  {$IFDEF ENABLE_FAE}
  NetworkServerFae,
  {$ENDIF}
  Server, Game, TraceLog, Constants, LogFile, BanSystem,
  sysutils, Sprites, Net, Things, Vector, ServerHelper,
  classes, Demo, Weapons, Cvar, LobbyClient,
  NetworkServerGame, NetworkServerSprite, NetworkServerThing,
  NetworkServerConnection, NetworkServerHeartbeat;

procedure AppOnIdle;
var
  MainControl: Integer;
  j: Integer;
  HeavySendersNum: Integer;
  Adjust: Single;
begin
  Trace('AppOnIdle');
  Number27Timing;  // makes the program go and do the timing calculations

  // NET RECEIVE
  UDP.ProcessLoop;

  {$IFDEF RCON}
  if AdminServer <> nil then
    AdminServer.ProcessCommands();
  {$ENDIF}

  for MainControl := 1 to (Ticktime - ticktimeLast) do
  begin  // frame rate independant code
    ticks := ticks + 1;

    Inc(ServerTickCounter);
    // Update main tick counter
    MainTickCounter := MainTickCounter + 1;
    if MainTickCounter = 2147483640 then
      MainTickCounter := 0;

    {$IFDEF SCRIPT}
    ScrptDispatcher.OnClockTick();
    {$ENDIF}

    {$IFDEF STEAM}
    RunManualCallbacks();
    {$ENDIF}
    // Flood Nums Cancel
    if MainTickCounter mod 1000 = 0 then
      for j := 1 to MAX_FLOODIPS do
        FloodNum[j] := 0;


    // Warnings Cancel
    if MainTickCounter mod (MINUTE * 5) = 0 then
      for j := 1 to MAX_PLAYERS do
      begin
        if PingWarnings[j] > 0 then
          Dec(PingWarnings[j]);

        if FloodWarnings[j] > 0 then
          Dec(FloodWarnings[j]);
      end;

    if MainTickCounter mod 1000 = 0 then
      for j := 1 to MAX_PLAYERS do
      begin
        Sprite[j].Player.KnifeWarnings := 0;
      end;

    // sync changed cvars to all players
    if CvarsNeedSyncing then
      ServerSyncCvars(0, 0, False);

    // General game updating
    UpdateFrame;

    if MapChangeCounter < 0 then
    begin
      if DemoRecorder.Active then
        DemoRecorder.SaveNextFrame;
    end;

    Trace('AppOnIdle 2');

    // Network updating
    if MainTickCounter mod SECOND = 0 then
    begin
      // Player Ping Warning
      if (MapChangeCounter < 0) then
        if MainTickCounter mod (SECOND * 6) = 0 then
          for j := 1 to MAX_PLAYERS do
            if (Sprite[j].Active) and
              (Sprite[j].Player.ControlMethod = HUMAN) and
              ((Sprite[j].Player.RealPing > (sv_maxping.Value)) or
              ((Sprite[j].Player.RealPing < sv_minping.Value) and
              (Sprite[j].Player.PingTime > 0))) then
            begin
              MainConsole.Console(Sprite[j].Player.Name +
                ' gets a ping warning', WARNING_MESSAGE_COLOR);
              Inc(PingWarnings[j]);
              if PingWarnings[j] > sv_warnings_ping.Value then
                KickPlayer(j, True, KICK_PING, SIXTY_MINUTES div 4, 'Ping Kick');
            end;

      // Player Packet Flooding
      for j := 1 to MAX_PLAYERS do
        if Sprite[j].Active then
        begin
          if ((net_lan.Value = LAN) and
            (MessagesASecNum[j] > net_floodingpacketslan.Value)) or
            ((net_lan.Value = Internet) and
            (MessagesASecNum[j] > net_floodingpacketsinternet.Value)) then
          begin
            MainConsole.Console(Sprite[j].Player.Name +
              ' is flooding the server', WARNING_MESSAGE_COLOR);
            Inc(FloodWarnings[j]);
            if FloodWarnings[j] > sv_warnings_flood.Value then
              KickPlayer(j, True, KICK_FLOODING, SIXTY_MINUTES div 4,
                'Flood Kicked');
          end;
        end;

      for j := 1 to MAX_PLAYERS do
        MessagesASecNum[j] := 0;
    end;

    if MainTickCounter mod (SECOND * 10) = 0 then
    begin
      for j := 1 to MAX_PLAYERS do
        if Sprite[j].Active then
          UDP.UpdateNetworkStats(j);
    end;

    // Packet rate send adjusting
    HeavySendersNum := PlayersNum - SpectatorsNum;

    if HeavySendersNum < 5 then
      Adjust := 0.66
    else if HeavySendersNum < 9 then
      Adjust := 0.75
    else
      Adjust := 1.0;

    // Send Bundled packets
    if net_lan.Value = LAN then
    begin
      if MainTickCounter mod Round(30 * Adjust) = 0 then
        ServerSpriteSnapshot(NETW);

      if (MainTickCounter mod Round(15 * Adjust) = 0) and
         (MainTickCounter mod Round(30 * Adjust) <> 0) then
        ServerSpriteSnapshotMajor(NETW);

      if MainTickCounter mod Round(20 * Adjust) = 0 then
        ServerSkeletonSnapshot(NETW);

      if MainTickCounter mod Round(59 * Adjust) = 0 then
        ServerHeartBeat;

      if (MainTickCounter mod Round(4 * Adjust) = 0) and
         (MainTickCounter mod Round(30 * Adjust) <> 0) and
         (MainTickCounter mod Round(60 * Adjust) <> 0) then
        for j := 1 to MAX_SPRITES do
          if Sprite[j].Active and (Sprite[j].Player.ControlMethod = BOT) then
            ServerSpriteDeltas(j);
    end
    else if net_lan.Value = INTERNET then
    begin
      if MainTickCounter mod Round(net_t1_snapshot.Value * Adjust) = 0 then
        ServerSpriteSnapshot(NETW);

      if (MainTickCounter mod Round(net_t1_majorsnapshot.Value * Adjust) = 0) and
          (MainTickCounter mod Round(net_t1_snapshot.Value * Adjust) <> 0) then
        ServerSpriteSnapshotMajor(NETW);

      if MainTickCounter mod Round(net_t1_deadsnapshot.Value * Adjust) = 0 then
        ServerSkeletonSnapshot(NETW);

      if MainTickCounter mod Round(net_t1_heartbeat.Value * Adjust) = 0 then
        ServerHeartBeat;

      if (MainTickCounter mod Round(net_t1_delta.Value * Adjust) = 0) and
          (MainTickCounter mod Round(net_t1_snapshot.Value * Adjust) <> 0) and
          (MainTickCounter mod Round(net_t1_majorsnapshot.Value * Adjust) <> 0) then
        for j := 1 to MAX_SPRITES do
          if Sprite[j].Active and (Sprite[j].Player.ControlMethod = BOT) then
            ServerSpriteDeltas(j);
    end;

    for j := 1 to MAX_SPRITES do
      if (Sprite[j].Active) and (Sprite[j].Player.ControlMethod = HUMAN) and
        (Sprite[j].Player.Port > 0) then
      begin
        // connection problems
        if (MapChangeCounter < 0) then
          NoClientupdateTime[j] := NoClientupdateTime[j] + 1;
        if NoClientupdateTime[j] > DISCONNECTION_TIME then
        begin
          ServerPlayerDisconnect(j, KICK_NORESPONSE);
          MainConsole.Console(Sprite[j].Player.Name + ' could not respond',
            WARNING_MESSAGE_COLOR);
          {$IFDEF SCRIPT}
          ScrptDispatcher.OnLeaveGame(j, False);
          {$ENDIF}
          Sprite[j].Kill;
          DoBalanceBots(1, Sprite[j].Player.Team);
          Continue;
        end;
        if NoClientupdateTime[j] < 0 then
          NoClientupdateTime[j] := 0;

        {$IFDEF ENABLE_FAE}
        if ac_enable.Value then
        begin
          // Monotonically increment the anti-cheat ticks counter. A valid response resets it.
          Inc(Sprite[j].Player.FaeTicks);
          if Sprite[j].Player.FaeTicks > (SECOND * 20) then
          begin
            // Timeout reached; no valid response for 20 seconds. Boot the player.
            MainConsole.Console(Sprite[j].Player.Name + ' no anti-cheat response',
              WARNING_MESSAGE_COLOR);
            KickPlayer(j, False, KICK_AC, 0, 'No Anti-Cheat Response');
            Continue;
          end
          else if (MainTickCounter mod (SECOND * 3) = 0)
              and (not Sprite[j].Player.FaeResponsePending) then
          begin
            // Send periodic anti-cheat challenge.
            ServerSendFaeChallenge(Sprite[j].Player.Peer, False);
          end;
        end;
        {$ENDIF}

        if MainTickCounter mod MINUTE = 0 then
          ServerSyncMsg;

        if net_lan.Value = LAN then
        begin
          if MainTickCounter mod Round(21 * Adjust) = 0 then
            ServerPing(j);

          if MainTickCounter mod Round(12 * Adjust) = 0 then
            ServerThingSnapshot(j);
        end
        else if net_lan.Value = INTERNET then
        begin
          if MainTickCounter mod Round(net_t1_ping.Value * Adjust) = 0 then
            ServerPing(j);

          if MainTickCounter mod Round(net_t1_thingsnapshot.Value * Adjust) = 0 then
            ServerThingSnapshot(j);
        end;
      end;

      //UDP.FlushMsg;
  end;
end;

procedure UpdateFrame;
var
  i, j: Integer;
  M: TVector2;
  _x: LongInt;
begin
  Trace('UpdateFrame');

  M := Default(TVector2);

  if MapChangeCounter < 0 then
  begin
    for j := 1 to MAX_SPRITES do
      if Sprite[j].Active and not Sprite[j].DeadMeat then
        if Sprite[j].IsNotSpectator() then
        begin
          for i := MAX_OLDPOS downto 1 do
            OldSpritePos[j, i] := OldSpritePos[j, i - 1];

          OldSpritePos[j, 0] := Spriteparts.Pos[j];
        end;

    for j := 1 to MAX_SPRITES do
      if Sprite[j].Active then
        if Sprite[j].IsNotSpectator() then
          SpriteParts.DoEulerTimeStepFor(j);  // integrate sprite particles

    for j := 1 to MAX_SPRITES do
      if Sprite[j].Active then
        Sprite[j].Update;  // update sprite

    // Bullets update
    for j := 1 to MAX_BULLETS do
      if Bullet[j].Active then
        Bullet[j].Update;

    BulletParts.DoEulerTimeStep;

    // update Things
    for j := 1 to MAX_THINGS do
      if Thing[j].Active then
        Thing[j].Update;

    // Bonuses spawn
    if (not sv_survivalmode.Value) and (not sv_realisticmode.Value) then
    begin
      if sv_bonus_frequency.Value > 0 then
      begin
        case sv_bonus_frequency.Value of
          1: BonusFreq := 7400;
          2: BonusFreq := 4300;
          3: BonusFreq := 2500;
          4: BonusFreq := 1600;
          5: BonusFreq := 800;
        end;

        if sv_bonus_berserker.Value then
          if MainTickCounter mod BonusFreq = 0 then
            if Random(BERSERKERBONUS_RANDOM) = 0 then
              SpawnThings(OBJECT_BERSERK_KIT, 1);

        j := FLAMERBONUS_RANDOM;
        if sv_bonus_flamer.Value then
          if MainTickCounter mod 444 = 0 then
            if Random(j) = 0 then
              SpawnThings(OBJECT_FLAMER_KIT, 1);

        if sv_bonus_predator.Value then
          if MainTickCounter mod BonusFreq = 0 then
            if Random(PREDATORBONUS_RANDOM) = 0 then
              SpawnThings(OBJECT_PREDATOR_KIT, 1);

        if sv_bonus_vest.Value then
          if MainTickCounter mod (BonusFreq div 2) = 0 then
            if Random(VESTBONUS_RANDOM) = 0 then
              SpawnThings(OBJECT_VEST_KIT, 1);

        j := CLUSTERBONUS_RANDOM;
        if sv_gamemode.Value = GAMESTYLE_CTF then
          j := Round(CLUSTERBONUS_RANDOM * 0.75);
        if sv_gamemode.Value = GAMESTYLE_INF then
          j := Round(CLUSTERBONUS_RANDOM * 0.75);
        if sv_gamemode.Value = GAMESTYLE_HTF then
          j := Round(CLUSTERBONUS_RANDOM * 0.75);
        if sv_bonus_cluster.Value then
          if MainTickCounter mod (BonusFreq div 2) = 0 then
            if Random(j) = 0 then
              SpawnThings(OBJECT_CLUSTER_KIT, 1);
      end;
    end;
  end;

  // bullet timer
  if BulletTimeTimer > -1 then
    Dec(BulletTimeTimer);

  if BulletTimeTimer = 0 then
  begin
    ToggleBulletTime(False);
    BulletTimeTimer := -1;
  end
  else if BulletTimeTimer < 1 then
  begin
    // MapChange counter update
    if (MapChangeCounter > -60) and (MapChangeCounter < 99999999) then
      MapChangeCounter := MapChangeCounter - 1;
    if (MapChangeCounter < 0) and (MapChangeCounter > -59) then
      ChangeMap;

    // Game Stats save
    if MainTickCounter mod log_filesupdate.Value = 0 then
    begin
      if log_enable.Value then
      begin
        UpdateGameStats;

        WriteLogFile(KillLog, KillLogFileName);
        WriteLogFile(GameLog, ConsoleLogFileName);

        if (CheckFileSize(KillLogFileName) > MAX_LOGFILESIZE) or
           (CheckFileSize(ConsoleLogFileName) > MAX_LOGFILESIZE) then
          NewLogFiles;
      end;
    end;

    // Anti-Hack for Mass-Flag Cheat
    if MainTickCounter mod SECOND = 0 then
      if sv_antimassflag.Value then
        for j := 1 to MAX_SPRITES do
        begin
          if (Sprite[j].Active) and (Sprite[j].Player.GrabsPerSecond > 0) and
            (Sprite[j].Player.ScoresPerSecond > 0) and
            (Sprite[j].Player.GrabbedInBase) then
          begin
            CheatTag[j] := 1;
            {$IFDEF SCRIPT}
            if not ScrptDispatcher.OnVoteKickStart(255, j,
              'Server: Possible cheating') then
            begin
            {$ENDIF}
              StartVote(255, VOTE_KICK, IntToStr(j),
                'Server: Possible cheating');
              ServerSendVoteOn(VoteType, 255, IntToStr(j),
                'Server: Possible cheating');
            {$IFDEF SCRIPT}
            end;
            {$ENDIF}
            MainConsole.Console('** Detected possible Mass-Flag cheating from ' +
              Sprite[j].Player.Name, WARNING_MESSAGE_COLOR);
          end;
          Sprite[j].Player.GrabsPerSecond := 0;
          Sprite[j].Player.ScoresPerSecond := 0;
          Sprite[j].Player.GrabbedInBase := False;
        end;

    if sv_healthcooldown.Value > 0 then
      if MainTickCounter mod (sv_healthcooldown.Value * SECOND) = 0 then
        for i := 1 to MAX_SPRITES do
          if (Sprite[i].Active) and (Sprite[i].HasPack) then
            Sprite[i].HasPack := False;

    // Anti-Chat Flood
    if MainTickCounter mod SECOND = 0 then
      for j := 1 to MAX_SPRITES do
        if (Sprite[j].Active) then
        begin
          if (Sprite[j].Player.ChatWarnings > 5) then
            // 20 Minutes is too harsh
            kickplayer(j, True, KICK_FLOODING, FIVE_MINUTES, 'Chat Flood');
          if (Sprite[j].Player.ChatWarnings > 0) then
            Dec(Sprite[j].Player.ChatWarnings);
        end;

    if MainTickCounter mod SECOND = 0 then
      if (LastReqIP[0] <> '') and (LastReqIP[0] = LastReqIP[1]) and
        (LastReqIP[1] = LastReqIP[2]) and (LastReqIP[2] = LastReqIP[3]) then
      begin
        DropIP := LastReqIP[0];
        MainConsole.Console('Firewalled IP ' + DropIP, 0);
      end;

    if MainTickCounter mod (SECOND * 3) = 0 then
      for j := 0 to 3 do
        LastReqIP[j] := '';  // Reset last 4 IP requests in 3 seconds

    if MainTickCounter mod (SECOND * 30) = 0 then
      DropIP := '';  // Clear temporary firewall IP

    if MainTickCounter mod MINUTE = 0 then
    begin
      if sv_lobby.Value then
          LobbyThread := TLobbyThread.Create;
    end;

    // *BAN*
    // Ban Timers v2
    if MainTickCounter mod MINUTE = 0 then
    begin
      UpdateIPBanList;
      UpdateHWBanList;
    end;

    if MainTickCounter mod MINUTE = 0 then
      for j := 1 to MAX_SPRITES do
        if Sprite[j].Active then
          Inc(Sprite[j].Player.PlayTime);

    // Leftover from old Ban Timers code
    if MainTickCounter mod (SECOND * 10) = 0 then
      if PlayersNum = 0 then
        if MapChangeCounter > 99999999 then
          MapChangeCounter := -60;


    SinusCounter := SinusCounter + ILUMINATESPEED;

    // Wave respawn count
    WaveRespawnCounter := WaveRespawnCounter - 1;
    if WaveRespawnCounter < 1 then
      WaveRespawnCounter := WaveRespawnTime;

    for j := 1 to MAX_SPRITES do
      if VoteCooldown[j] > -1 then
        VoteCooldown[j] := VoteCooldown[j] - 1;

    // Time Limit decrease
    if MapChangeCounter < 99999999 then
      // if (MapChangeCounter<0) then
      if TimeLimitCounter > 0 then
        TimeLimitCounter := TimeLimitCounter - 1;
    if TimeLimitCounter = 1 then
      NextMap;

    TimeLeftMin := TimeLimitCounter div MINUTE;
    TimeLeftSec := (TimeLimitCounter - TimeLeftMin * MINUTE) div 60;

    if TimeLimitCounter > 0 then
      if TimeLimitCounter < FIVE_MINUTES + 1 then
      begin
        if TimeLimitCounter mod MINUTE = 0 then
          MainConsole.Console('Time Left: ' +
            IntToStr(TimeLimitCounter div MINUTE) + ' minutes',
            GAME_MESSAGE_COLOR)
      end
      else
        if TimeLimitCounter mod FIVE_MINUTES = 0 then
          MainConsole.Console('Time Left: ' +
            IntToStr(TimeLimitCounter div MINUTE) + ' minutes',
            GAME_MESSAGE_COLOR);

    Trace('UpdateFrame 2');

    // voting timer
    TimerVote;

    // Consoles Update
    MainConsole.ScrollTick := MainConsole.ScrollTick + 1;
    if MainConsole.ScrollTick = MainConsole.ScrollTickMax then
      MainConsole.ScrollConsole;

    if MainConsole.AlphaCount > 0 then
      MainConsole.AlphaCount := MainConsole.AlphaCount - 1;

    if not sv_advancemode.Value then
      for j := 1 to MAX_SPRITES do
        for i := 1 to 10 do
          WeaponSel[j][i] := 1;
  end;  // bullettime off

  Trace('UpdateFrame 3');

  // Infiltration mode blue team score point
  j := sv_inf_bluelimit.Value * SECOND;
  if PlayersTeamNum[1] < PlayersTeamNum[2] then
    j := sv_inf_bluelimit.Value * SECOND + 2 * SECOND * (PlayersTeamNum[2] - PlayersTeamNum[1]);

  if sv_gamemode.Value = GAMESTYLE_INF then
    if MapChangeCounter < 0 then
      if Thing[TeamFlag[2]].InBase then
        if (PlayersTeamNum[1] > 0) and (PlayersTeamNum[2] > 0)
          {and(PlayersTeamNum[1] >= PlayersTeamNum[2])} then
          if MainTickCounter mod j = 0 then
          begin
            Inc(TeamScore[2], 1);
            SortPlayers;
          end;

  // HTF mode team score point
  if (PlayersTeamNum[2] = PlayersTeamNum[1]) then
    HTFTime := sv_htf_pointstime.Value * 60;

  if sv_gamemode.Value = GAMESTYLE_HTF then
    if MapChangeCounter < 0 then
      if (PlayersTeamNum[1] > 0) and (PlayersTeamNum[2] > 0) then
        if MainTickCounter mod HTFTime = 0 then
        begin
          for i := 1 to MAX_SPRITES do
            if Sprite[i].Active and (Sprite[i].HoldedThing > 0) then
              if Thing[Sprite[i].HoldedThing].Style = OBJECT_POINTMATCH_FLAG then
              begin
                Inc(TeamScore[Sprite[i].Player.Team], 1);

                if Sprite[i].Player.Team = TEAM_ALPHA then
                  HTFTime := HTF_SEC_POINT + 2 * SECOND * (PlayersTeamNum[1] - PlayersTeamNum[2]);

                if Sprite[i].Player.Team = TEAM_BRAVO then
                  HTFTime := HTF_SEC_POINT + 2 * SECOND * (PlayersTeamNum[2] - PlayersTeamNum[1]);

                if HTFTime < HTF_SEC_POINT then
                  HTFTime := HTF_SEC_POINT;

                SortPlayers;
              end;
        end;

  // Spawn Rambo bow if nobody has it and not on map
  if sv_gamemode.Value = GAMESTYLE_RAMBO then
    if MainTickCounter mod SECOND = 0 then
    begin
      _x := 0;

      for j := 1 to MAX_THINGS do
        if Thing[j].Active then
          if Thing[j].Style = OBJECT_RAMBO_BOW then
            _x := 1;

      for j := 1 to MAX_PLAYERS do
        if Sprite[j].Active then
          if (Sprite[j].Weapon.Num = Guns[BOW].Num) or
             (Sprite[j].Weapon.Num = Guns[BOW2].Num) then
              _x := 1;

      if _x = 0 then
      begin
        M := Default(TVector2);
        RandomizeStart(M, 15);

        j := CreateThing(M, 255, OBJECT_RAMBO_BOW, 255);
      end;
    end;

  // Destroy flags if > 1
  if (sv_gamemode.Value = GAMESTYLE_CTF) or (sv_gamemode.Value = GAMESTYLE_INF) then
    if MainTickCounter mod (SECOND * 2) = 0 then
    begin
      _x := 0;

      for j := 1 to MAX_THINGS do
        if Thing[j].Active then
          if Thing[j].Style = OBJECT_ALPHA_FLAG then
            Inc(_x);

      if _x > 1 then
        for j := MAX_THINGS downto 1 do
          if Thing[j].Active then
            if Thing[j].Style = OBJECT_ALPHA_FLAG then
            begin
              Thing[j].Kill;
              Break;
            end;

      if _x = 0 then
        if RandomizeStart(M, 5) then
          TeamFlag[1] := CreateThing(M, 255, OBJECT_ALPHA_FLAG, 255);

      _x := 0;

      for j := 1 to MAX_THINGS do
        if Thing[j].Active then
          if Thing[j].Style = OBJECT_BRAVO_FLAG then
            Inc(_x);

      if _x > 1 then
        for j := MAX_THINGS downto 1 do
          if Thing[j].Active then
            if Thing[j].Style = OBJECT_BRAVO_FLAG then
            begin
              Thing[j].Kill;
              Break;
            end;

      if _x = 0 then
        if RandomizeStart(M, 6) then
          TeamFlag[2] := CreateThing(M, 255, OBJECT_BRAVO_FLAG, 255);
    end;

  if (sv_gamemode.Value = GAMESTYLE_POINTMATCH) or (sv_gamemode.Value = GAMESTYLE_HTF) then
    if MainTickCounter mod (SECOND * 2) = 0 then
    begin
      _x := 0;

      for j := 1 to MAX_THINGS do
        if Thing[j].Active then
          if Thing[j].Style = OBJECT_POINTMATCH_FLAG then
            Inc(_x);

      if _x > 1 then
        for j := MAX_THINGS downto 1 do
          if Thing[j].Active then
            if Thing[j].Style = OBJECT_POINTMATCH_FLAG then
            begin
              Thing[j].Kill;
              Break;
            end;

      if _x = 0 then
        if RandomizeStart(M, 14) then
          TeamFlag[1] := CreateThing(M, 255, OBJECT_POINTMATCH_FLAG, 255);
    end;

  if ((demo_autorecord.Value) and (DemoRecorder.Active = False) and (Map.Name <> '')) then
  begin
    DemoRecorder.StartRecord(UserDirectory + 'demos/'  +
      FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now()) + Map.Name + '.sdm');
  end;
end;

end.

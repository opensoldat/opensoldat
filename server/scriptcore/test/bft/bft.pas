//
// BFT
//
// Most of these tests are pretty cursory, just to check that the script engine
// under test doesn't have any terrible flaws (cough cough PascalScript).
// The bulk of the testing job here falls to the test suite of the standard
// library we're wrapping, PascalScript, and manual OpenSoldat testing. But this
// is still a good final integration test to make sure nothing broke. Also it
// doesn't actually test every single function exposed by SC3 because I got sick
// of typing test code. :)
//
// TODO:
//  - Record/print time spent per test and overall.
//  - Log test results to a file.
//  - Count event occurrences.
//  - Test more of ScriptGame.pas, some ScriptMap.pas stuff like raycasting,
//    mapslist and spawnpoints.
//

//
// Logging
//

const
  COMMANDS_COLOR        = $FF9966;
  INFO_CONSOLE_COLOR    = $EEEEEE;
  TEST_CONSOLE_COLOR    = $AAAAFF;
  PASS_CONSOLE_COLOR    = $88FF88;
  FAILURE_CONSOLE_COLOR = $FF7777;

type
  TLogType = (INFO, TEST_INFO, PASS, FAILURE);

procedure SLog(S: String; LogType: TLogType);
begin
  WriteLn(S);
end;

//
// Test """Framework"""
//

type
  TTest = record
    Name: String;
    Fn: Function(): String; // Return empty string on success.
    Res: String;
  end;

procedure RunTest(var Test: TTest);
begin
  SLog('Running test: ' + Test.Name, TEST_INFO);
  try
    Test.Res := Test.Fn();
  except
    Test.Res := 'Exception thrown: ' + ExceptionParam;
  end;
end;

function TestIsFailed(Test: TTest): Boolean;
begin
  Result := Test.Res <> '';
end;

procedure PrintFailedTest(Test: TTest);
begin
  SLog('Failed: ' + Test.Name, FAILURE);
  SLog('Output: ' + Test.Res, FAILURE);
end;

//
// Utils
//

function FloatCmp(l, r: Extended): Boolean;
begin
  if Abs(l - r) < 0.00001 then
    Result := True
  else
    Result := False;
end;

function BoolToStr(b: Boolean): String;
begin
  if b then
    Result := 'True'
  else
    Result := 'False';
end;

function ObjStyleToStr(ObjStyle: Byte): String;
begin
  case ObjStyle of
    OBJECT_ALPHA_FLAG: Result := 'OBJECT_ALPHA_FLAG';
    OBJECT_BRAVO_FLAG: Result := 'OBJECT_BRAVO_FLAG';
    OBJECT_POINTMATCH_FLAG: Result := 'OBJECT_POINTMATCH_FLAG';
    OBJECT_USSOCOM: Result := 'OBJECT_USSOCOM';
    OBJECT_DESERT_EAGLE: Result := 'OBJECT_DESERT_EAGLE';
    OBJECT_HK_MP5: Result := 'OBJECT_HK_MP5';
    OBJECT_AK74: Result := 'OBJECT_AK74';
    OBJECT_STEYR_AUG: Result := 'OBJECT_STEYR_AUG';
    OBJECT_SPAS12: Result := 'OBJECT_SPAS12';
    OBJECT_RUGER77: Result := 'OBJECT_RUGER77';
    OBJECT_M79: Result := 'OBJECT_M79';
    OBJECT_BARRET_M82A1: Result := 'OBJECT_BARRET_M82A1';
    OBJECT_MINIMI: Result := 'OBJECT_MINIMI';
    OBJECT_MINIGUN: Result := 'OBJECT_MINIGUN';
    OBJECT_RAMBO_BOW: Result := 'OBJECT_RAMBO_BOW';
    OBJECT_MEDICAL_KIT: Result := 'OBJECT_MEDICAL_KIT';
    OBJECT_GRENADE_KIT: Result := 'OBJECT_GRENADE_KIT';
    OBJECT_FLAMER_KIT: Result := 'OBJECT_FLAMER_KIT';
    OBJECT_PREDATOR_KIT: Result := 'OBJECT_PREDATOR_KIT';
    OBJECT_VEST_KIT: Result := 'OBJECT_VEST_KIT';
    OBJECT_BERSERK_KIT: Result := 'OBJECT_BERSERK_KIT';
    OBJECT_CLUSTER_KIT: Result := 'OBJECT_CLUSTER_KIT';
    OBJECT_PARACHUTE: Result := 'OBJECT_PARACHUTE';
    OBJECT_COMBAT_KNIFE: Result := 'OBJECT_COMBAT_KNIFE';
    OBJECT_CHAINSAW: Result := 'OBJECT_CHAINSAW';
    OBJECT_LAW: Result := 'OBJECT_LAW';
    OBJECT_STATIONARY_GUN: Result := 'OBJECT_STATIONARY_GUN';
  else
    Result := 'Unknown';
  end;
end;

function WTypeToStr(WType: Byte): String;
begin
  case WType of
    WTYPE_EAGLE: Result := 'WTYPE_EAGLE';
    WTYPE_MP5: Result := 'WTYPE_MP5';
    WTYPE_AK74: Result := 'WTYPE_AK74';
    WTYPE_STEYRAUG: Result := 'WTYPE_STEYRAUG';
    WTYPE_SPAS12: Result := 'WTYPE_SPAS12';
    WTYPE_RUGER77: Result := 'WTYPE_RUGER77';
    WTYPE_M79: Result := 'WTYPE_M79';
    WTYPE_BARRETT: Result := 'WTYPE_BARRETT';
    WTYPE_M249: Result := 'WTYPE_M249';
    WTYPE_MINIGUN: Result := 'WTYPE_MINIGUN';
    WTYPE_USSOCOM: Result := 'WTYPE_USSOCOM';
    WTYPE_KNIFE: Result := 'WTYPE_KNIFE';
    WTYPE_CHAINSAW: Result := 'WTYPE_CHAINSAW';
    WTYPE_LAW: Result := 'WTYPE_LAW';
    WTYPE_FLAMER: Result := 'WTYPE_FLAMER';
    WTYPE_BOW: Result := 'WTYPE_BOW';
    WTYPE_BOW2: Result := 'WTYPE_BOW2';
    WTYPE_M2: Result := 'WTYPE_M2';
    WTYPE_NOWEAPON: Result := 'WTYPE_NOWEAPON';
    WTYPE_FRAGGRENADE: Result := 'WTYPE_FRAGGRENADE';
    WTYPE_CLUSTERGRENADE: Result := 'WTYPE_CLUSTERGRENADE';
    WTYPE_CLUSTER: Result := 'WTYPE_CLUSTER';
    WTYPE_THROWNKNIFE: Result := 'WTYPE_THROWNKNIFE';
  else
    Result := 'Unknown';
  end;
end;

procedure PrintActivePlayer(Player: TActivePlayer);
begin
  SLog('  ID: ' + IntToStr(Player.ID), INFO);
  SLog('  Team: ' + IntToStr(Player.Team), INFO);
  SLog('  Alive: ' + BoolToStr(Player.Alive), INFO);
  SLog('  Kills: ' + IntToStr(Player.Kills), INFO);
  SLog('  Deaths: ' + IntToStr(Player.Deaths), INFO);
  SLog('  Ping: ' + IntToStr(Player.Ping), INFO);
  SLog('  Active: ' + BoolToStr(Player.Active), INFO);
  SLog('  IP: ' + Player.IP, INFO);
  SLog('  Port: ' + IntToStr(Player.Port), INFO);
  SLog('  VelX: ' + FloatToStr(Player.VelX), INFO);
  SLog('  VelY: ' + FloatToStr(Player.VelY), INFO);
  SLog('  Muted: ' + BoolToStr(Player.Muted), INFO);
  SLog('  Jets: ' + IntToStr(Player.Jets), INFO);
  SLog('  Grenades: ' + IntToStr(Player.Grenades), INFO);
  SLog('  X: ' + FloatToStr(Player.X), INFO);
  SLog('  Y: ' + FloatToStr(Player.Y), INFO);
  SLog('  Flagger: ' + BoolToStr(Player.Flagger), INFO);
  SLog('  Time: ' + IntToStr(Player.Time), INFO);
  SLog('  OnGround: ' + BoolToStr(Player.OnGround), INFO);
  SLog('  IsProne: ' + BoolToStr(Player.IsProne), INFO);
  SLog('  Human: ' + BoolToStr(Player.Human), INFO);
  SLog('  Direction: ' + IntToStr(Player.Direction), INFO);
  SLog('  Flags: ' + IntToStr(Player.Flags), INFO);
  SLog('  HWID: ' + Player.HWID, INFO);
  SLog('  KeyUp: ' + BoolToStr(Player.KeyUp), INFO);
  SLog('  KeyLeft: ' + BoolToStr(Player.KeyLeft), INFO);
  SLog('  KeyRight: ' + BoolToStr(Player.KeyRight), INFO);
  SLog('  KeyShoot: ' + BoolToStr(Player.KeyShoot), INFO);
  SLog('  KeyJetpack: ' + BoolToStr(Player.KeyJetpack), INFO);
  SLog('  KeyGrenade: ' + BoolToStr(Player.KeyGrenade), INFO);
  SLog('  KeyChangeWeap: ' + BoolToStr(Player.KeyChangeWeap), INFO);
  SLog('  KeyThrow: ' + BoolToStr(Player.KeyThrow), INFO);
  SLog('  KeyReload: ' + BoolToStr(Player.KeyReload), INFO);
  SLog('  KeyCrouch: ' + BoolToStr(Player.KeyCrouch), INFO);
  SLog('  KeyProne: ' + BoolToStr(Player.KeyProne), INFO);
  SLog('  KeyFlagThrow: ' + BoolToStr(Player.KeyFlagThrow), INFO);
end;

procedure PrintActiveFlag(Flag: TActiveFlag);
begin
  SLog('  InBase: ' + BoolToStr(Flag.InBase), INFO);
end;

procedure PrintActiveMapObject(MapObject: TActiveMapObject);
begin
  SLog('  ID: ' + IntToStr(MapObject.ID), INFO);
  SLog('  Active: ' + BoolToStr(MapObject.Active), INFO);
end;

procedure PrintActiveMapBullet(MapBullet: TActiveMapBullet);
begin
  SLog('  GetOwnerWeaponId: ' + IntToStr(MapBullet.GetOwnerWeaponId), INFO);
  SLog('  ID: ' + IntToStr(MapBullet.ID), INFO);
  SLog('  Active: ' + BoolToStr(MapBullet.Active), INFO);
  SLog('  Style: ' + IntToStr(MapBullet.Style), INFO);
  SLog('  X: ' + FloatToStr(MapBullet.X), INFO);
  SLog('  Y: ' + FloatToStr(MapBullet.Y), INFO);
  SLog('  VelY: ' + FloatToStr(MapBullet.VelY), INFO);
  SLog('  Owner: ' + IntToStr(MapBullet.Owner), INFO);
end;

procedure PrintTeam(Team: TTeam);
begin
  SLog('  Score: ' + IntToStr(Team.Score), INFO);
  SLog('  Count: ' + IntToStr(Team.Count), INFO);
  SLog('  ID: ' + IntToStr(Team.ID), INFO);
end;

procedure PrintWeapon(Weapon: TWeapon);
begin
  SLog('  WType: ' + WTypeToStr(Weapon.WType), INFO);
  SLog('  Name: ' + Weapon.Name, INFO);
  SLog('  BulletStyle: ' + IntToStr(Weapon.BulletStyle), INFO);
  SLog('  Ammo: ' + IntToStr(Weapon.Ammo), INFO);
end;

//
// Event handlers
//

procedure MyOnClockTick(Ticks: Integer);
begin
  SLog('==========================================================', INFO);
  SLog('OnClockTick: ' + IntToStr(Ticks), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnIdle();
begin
  SLog('==========================================================', INFO);
  SLog('OnIdle', INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnBeforeMapChange(Map: String);
begin
  SLog('==========================================================', INFO);
  SLog('OnBeforeMapChange event called: ' + Map, INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnAfterMapChange(Map: String);
begin
  SLog('==========================================================', INFO);
  SLog('OnAfterMapChange event called: ' + Map, INFO);
  SLog('==========================================================', INFO);
end;

function MyOnRequest(IP, HW: String; Port: Word; State: Byte;
  Forwarded: Boolean; Password: String): Integer;
begin
  SLog('==========================================================', INFO);
  SLog('OnRequest event called:', INFO);
  SLog('IP: ' + IP, INFO);
  SLog('HW: ' + HW, INFO);
  SLog('Port: ' + IntToStr(Port), INFO);
  SLog('State: ' + IntToStr(State), INFO);
  SLog('Forwarded: ' + BoolToStr(Forwarded), INFO);
  SLog('Password: ' + Password, INFO);
  SLog('==========================================================', INFO);

  Result := 1;
end;

procedure MyOnAdminConnect(IP: String; Port: Word);
begin
  SLog('==========================================================', INFO);
  SLog('OnAdminConnect event called: ' + IP + ':' + IntToStr(Port), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnAdminDisconnect(IP: String; Port: Word);
begin
  SLog('==========================================================', INFO);
  SLog('OnAdminDisconnect event called: ' + IP + ':' + IntToStr(Port), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnTCPMessage(IP: String; Port: Word; Message: String);
begin
  SLog('==========================================================', INFO);
  SLog('OnTCPMessage event called:', INFO);
  SLog(IP + ':' + IntToStr(Port) + ' > ' + Message, INFO);
  SLog('==========================================================', INFO);
end;

procedure MyGameOnJoin(Player: TActivePlayer; Team: TTeam);
begin
  SLog('==========================================================', INFO);
  SLog('Game OnJoin event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Team:', INFO);
  PrintTeam(Team);
  SLog('==========================================================', INFO);

  Player.WriteConsole('Welcome to ScriptCore playground. Type !help for info',
    COMMANDS_COLOR);
end;

procedure MyOnFlagGrab(Player: TActivePlayer; Flag: TActiveFlag;
  TeamFlag: Byte; GrabbedInBase: Boolean);
begin
  SLog('==========================================================', INFO);
  SLog('OnFlagGrab event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Flag:', INFO);
  PrintActiveFlag(Flag);
  SLog('TeamFlag: ' + IntToStr(TeamFlag), INFO);
  SLog('GrabbedInBase: ' + BoolToStr(GrabbedInBase), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnFlagScore(Player: TActivePlayer; Flag: TActiveFlag;
  TeamFlag: Byte);
begin
  SLog('==========================================================', INFO);
  SLog('OnFlagScore event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Flag:', INFO);
  PrintActiveFlag(Flag);
  SLog('TeamFlag: ' + IntToStr(TeamFlag), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnFlagReturn(Player: TActivePlayer; Flag: TActiveFlag;
  TeamFlag: Byte);
begin
  SLog('==========================================================', INFO);
  SLog('OnFlagReturn event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Flag:', INFO);
  PrintActiveFlag(Flag);
  SLog('TeamFlag: ' + IntToStr(TeamFlag), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnFlagDrop(Player: TActivePlayer; Flag: TActiveFlag; TeamFlag: Byte;
  Thrown: Boolean);
begin
  SLog('==========================================================', INFO);
  SLog('OnFlagDrop event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Flag:', INFO);
  PrintActiveFlag(Flag);
  SLog('TeamFlag: ' + IntToStr(TeamFlag), INFO);
  SLog('Thrown: ' + BoolToStr(Thrown), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnKitPickup(Player: TActivePlayer; Kit: TActiveMapObject);
begin
  SLog('==========================================================', INFO);
  SLog('OnKitPickup event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Kit:', INFO);
  PrintActiveMapObject(Kit);
  SLog('==========================================================', INFO);
end;

function MyOnBeforeRespawn(Player: TActivePlayer): TVector;
begin
  SLog('==========================================================', INFO);
  SLog('OnBeforeRespawn event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('==========================================================', INFO);

  Result.X := 100.0;
  Result.Y := 100.0;
end;

procedure MyOnAfterRespawn(Player: TActivePlayer);
var
  Primary, Secondary: TNewWeapon;
begin
  SLog('==========================================================', INFO);
  SLog('OnAfterRespawn event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('==========================================================', INFO);

  if Pos('bot', Player.Name) = 1 then
  begin
    Primary := TNewWeapon.Create;
    Primary.WType := WTYPE_RUGER77;
    Secondary := TNewWeapon.Create;
    Secondary.WType := WTYPE_MP5;
    Player.ForceWeapon(Primary, Secondary);
    Primary.Free;
    Secondary.Free;
  end;
end;

function MyOnDamage(Shooter, Victim: TActivePlayer; Damage: Single;
  BulletID: Byte): Single;
begin
  SLog('==========================================================', INFO);
  SLog('OnDamage event called:', INFO);
  SLog('Shooter:', INFO);
  PrintActivePlayer(Shooter);
  SLog('Victim:', INFO);
  PrintActivePlayer(Victim);
  SLog('Damage: ' + FloatToStr(Damage), INFO);
  SLog('BulletID: ' + IntToStr(BulletID), INFO);
  SLog('==========================================================', INFO);

  Result := 1000.0;
end;

procedure MyOnKill(Killer, Victim: TActivePlayer; BulletID: Byte);
begin
  SLog('==========================================================', INFO);
  SLog('OnKill event called:', INFO);
  SLog('Killer:', INFO);
  PrintActivePlayer(Killer);
  SLog('Victim:', INFO);
  PrintActivePlayer(Victim);
  SLog('BulletID: ' + IntToStr(BulletID), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnWeaponChange(Player: TActivePlayer; OldWeapon,
  NewWeapon: TPlayerWeapon);
begin
  SLog('==========================================================', INFO);
  SLog('OnWeaponChange event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('OldWeapon:', INFO);
  PrintWeapon(OldWeapon);
  SLog('NewWeapon:', INFO);
  PrintWeapon(NewWeapon);
  SLog('==========================================================', INFO);
end;

function MyOnVoteMapStart(Voter: TActivePlayer; Map: String): Boolean;
begin
  SLog('==========================================================', INFO);
  SLog('OnVoteMapStart event called:', INFO);
  SLog('Voter:', INFO);
  PrintActivePlayer(Voter);
  SLog('Map: ' + Map, INFO);
  SLog('==========================================================', INFO);

  Result := False;
end;

function MyOnVoteKickStart(Voter, Victim: TActivePlayer; Reason: String
  ): Boolean;
begin
  SLog('==========================================================', INFO);
  SLog('OnVoteKickStart event called:', INFO);
  SLog('Voter:', INFO);
  PrintActivePlayer(Voter);
  SLog('Victim:', INFO);
  PrintActivePlayer(Victim);
  SLog('Reason: ' + Reason, INFO);
  SLog('==========================================================', INFO);

  Result := False;
end;

procedure MyOnVoteMap(Voter: TActivePlayer; Map: String);
begin
  SLog('==========================================================', INFO);
  SLog('OnVoteMap event called:', INFO);
  SLog('Voter:', INFO);
  PrintActivePlayer(Voter);
  SLog('Map: ' + Map, INFO);
  SLog('==========================================================', INFO);
end;

procedure MyOnVoteKick(Voter, Victim: TActivePlayer);
begin
  SLog('==========================================================', INFO);
  SLog('OnVoteKick event called:', INFO);
  SLog('Voter:', INFO);
  PrintActivePlayer(Voter);
  SLog('Victim:', INFO);
  PrintActivePlayer(Victim);
  SLog('==========================================================', INFO);
end;

var
  TestCount: Integer;
  ObjectStyle: Integer;

procedure MyOnSpeak(Player: TActivePlayer; Message: String);
var
  i: Integer;
  VPos, VVel: TVector;
  NewPlayer: TNewPlayer;
  NewObject: TNewMapObject;
  CheckPlayer, LastBot: TActivePlayer;
begin
  SLog('==========================================================', INFO);
  SLog('OnSpeak event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Message: ' + Message, INFO);
  SLog('==========================================================', INFO);

  if Message = '!help' then
  begin
    Player.WriteConsole('Available tests, and expected results:', COMMANDS_COLOR);
    Player.WriteConsole('!big - calls Player.BigText and Players.BigText on ' +
      'different layers. Expect 2 texts on screen', COMMANDS_COLOR);
    Player.WriteConsole('!say - calls Player.Say with 3 message types ' +
      '(chat, team chat, radio)', COMMANDS_COLOR);
    Player.WriteConsole('!tell - calls Player.Tell and Players.Tell. Expect 2 messages',
      COMMANDS_COLOR);
    Player.WriteConsole('!world - calls Player.WorldText and Players.WorldText on ' +
      'different layers. Don''t call from spectators. Expect 2 texts',
      COMMANDS_COLOR);
    Player.WriteConsole('!test - big test for other functions', COMMANDS_COLOR);
  end;

  if Message = '!big' then
  begin
    Player.BigText(0, 'Player.BigText Fail', 200, FAILURE_CONSOLE_COLOR,
      0.2, 100, 100);
    Player.BigText(0, 'Player.BigText Pass', 200, PASS_CONSOLE_COLOR,
      0.2, 100, 100);
    Players.BigText(1, 'Players.BigText Fail', 200, FAILURE_CONSOLE_COLOR,
      0.2, 100, 200);
    Players.BigText(1, 'Players.BigText Pass', 200, PASS_CONSOLE_COLOR,
      0.2, 100, 200);
  end;

  if Message = '!say' then
  begin
    Player.Say('Player.Say chat Pass', 1);
    Player.Say('Player.Say team chat Pass', 2);
    Player.Say('Player.Say radio Pass', 3);
  end;

  if Message = '!tell' then
  begin
    Player.Tell('Player.Tell message Pass');
    // NOTE: Currently broken due to special case in ServerSendStringMessage for
    // some reason.
    Players.Tell('Players.Tell message Pass');
  end;

  if Message = '!world' then
  begin
    Player.WorldText(0, 'Player.WorldText pass', 200, PASS_CONSOLE_COLOR, 0.2,
      Player.X, Player.Y + 20.0);
    Players.WorldText(1, 'Players.WorldText pass', 200, PASS_CONSOLE_COLOR, 0.2,
      Player.X, Player.Y + 50.0);
  end;

  if Message = '!test' then
  begin
    VPos.X := Player.X + 100.0;
    VPos.Y := Player.Y;
    VVel.X := 25.0;
    VVel.Y := -10.0;
    Map.CreateBulletVector(VPos, VVel, 24.0, 4, Player);
    VPos.Y := VPos.Y + 10.0;
    Map.CreateBullet(VPos.X, VPos.Y, VVel.X, VVel.Y, 24.0, 4, Player);

    NewObject := TNewMapObject.Create;
    NewObject.X := Player.X;
    NewObject.Y := Player.Y;
    NewObject.Style := ObjectStyle;
    Map.AddObject(NewObject);
    NewObject.Free;

    Dec(ObjectStyle, 1);
    if ObjectStyle < OBJECT_ALPHA_FLAG then
      ObjectStyle := OBJECT_STATIONARY_GUN;

    if TestCount = 0 then
      Player.SetVelocity(10.0, -20.0);
    Player.GiveBonus(1);

    CheckPlayer := Players.GetByName(Player.Name);
    if CheckPlayer.ID = Player.ID then
      SLog('Found by name', PASS)
    else
      SLog('Not found by name.', FAILURE);

    CheckPlayer := Players.GetByIP(Player.IP);
    if CheckPlayer.ID = Player.ID then
      SLog('Found by IP', PASS)
    else
      SLog('Not found by IP.', FAILURE);

    NewPlayer := TNewPlayer.Create;
    NewPlayer.Name := 'bot' + IntToStr(TestCount);
    NewPlayer.Team := 1;
    NewPlayer.Health := 50.0;
    NewPlayer.ShirtColor := $00FFFF;
    NewPlayer.PantsColor := $FFFFFF;
    NewPlayer.SkinColor := $DDAAAA;
    NewPlayer.HairColor := $FFFF00;
    NewPlayer.HairStyle := 1;
    NewPlayer.Headgear := 20;
    NewPlayer.Chain := 0;
    NewPlayer.Dummy := False;
    NewPlayer.ChatFrequency := 3;
    Players.Add(NewPlayer, TJoinNormal);
    NewPlayer.Free;

    LastBot := Players.GetByName('bot' + IntToStr(TestCount));

    LastBot.Say('Bot say pass', 1);

    if (TestCount mod 1) = 0 then
      LastBot.Damage(Player.ID, 50.0)
    else
      LastBot.Kill;

    for i := 0 to Players.Active.Count - 1 do
      if not Players.Active[i].Active then
        SLog('Player in "Active" not actually active.', FAILURE);

    Inc(TestCount, 1);
  end;
end;

function MyOnCommand(Player: TActivePlayer; Command: String): Boolean;
begin
  SLog('==========================================================', INFO);
  SLog('OnCommand event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Command: ' + Command, INFO);
  SLog('==========================================================', INFO);

  Result := False;
end;

procedure MyTeamOnJoin(Player: TActivePlayer; Team: TTeam);
begin
  SLog('==========================================================', INFO);
  SLog('Team OnJoin event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Team:', INFO);
  PrintTeam(Team);
  SLog('==========================================================', INFO);
end;

procedure MyGameOnLeave(Player: TActivePlayer; Kicked: Boolean);
begin
  SLog('==========================================================', INFO);
  SLog('Game OnLeave event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Kicked: ' + BoolToStr(Kicked), INFO);
  SLog('==========================================================', INFO);
end;

procedure MyTeamOnLeave(Player: TActivePlayer; Team: TTeam; Kicked: Boolean);
begin
  SLog('==========================================================', INFO);
  SLog('Team OnLeave event called:', INFO);
  SLog('Player:', INFO);
  PrintActivePlayer(Player);
  SLog('Team:', INFO);
  PrintTeam(Team);
  SLog('Kicked: ' + BoolToStr(Kicked), INFO);
  SLog('==========================================================', INFO);
end;

function MyOnBeforeJoin(Player: TActivePlayer; NewTeam, OldTeam: TTeam
  ): ShortInt;
begin
  SLog('==========================================================', INFO);
  SLog('Team OnBeforeJoin event called:', INFO);

  if Player <> Nil then
  begin
    SLog('Player:', INFO);
    PrintActivePlayer(Player);
  end;

  SLog('NewTeam:', INFO);
  PrintTeam(NewTeam);

  if OldTeam <> Nil then
  begin
    SLog('OldTeam:', INFO);
    PrintTeam(OldTeam);
  end;
  SLog('==========================================================', INFO);

  Result := NewTeam.ID;
end;

function MyOnTCPCommand(IP: String; Port: Word; Command: String): Boolean;
begin
  SLog('==========================================================', INFO);
  SLog('OnTCPCommand event called:', INFO);
  SLog('IP: ' + IP, INFO);
  SLog('Port: ' + IntToStr(Port), INFO);
  SLog('Command: ' + Command, INFO);
  SLog('==========================================================', INFO);

  Result := False;
end;

function MyOnAdminCommand(Player: TActivePlayer; Command: String): Boolean;
begin
  SLog('==========================================================', INFO);
  SLog('OnAdminCommand event called:', INFO);

  if Player <> Nil then
  begin
    SLog('Player:', INFO);
    PrintActivePlayer(Player)
  end
  else
  begin
    SLog('Triggered by TCP command.', INFO);
  end;

  SLog('Command: ' + Command, INFO);
  SLog('==========================================================', INFO);

  Result := False;
end;

procedure MyOnException(ErrorCode: TErrorType; Message, UnitName,
  FunctionName: String; Row, Col: Cardinal);
begin
  SLog('==========================================================', INFO);
  SLog('OnException event called:', INFO);
  SLog('ErrorCode: ' + IntToStr(Ord(ErrorCode)), INFO);
  SLog('Message: ' + Message, INFO);
  SLog('UnitName: ' + UnitName, INFO);
  SLog('FunctionName: ' + FunctionName, INFO);
  SLog('Row: ' + IntToStr(Row), INFO);
  SLog('Col: ' + IntToStr(Col), INFO);
  SLog('==========================================================', INFO);
end;

function MyOnUnhandledException(ErrorCode: TErrorType; Message, UnitName,
  FunctionName: String; Row, Col: Cardinal): Boolean;
begin
  SLog('==========================================================', INFO);
  SLog('OnUnhandledException event called:', INFO);
  SLog('ErrorCode: ' + IntToStr(Ord(ErrorCode)), INFO);
  SLog('Message: ' + Message, INFO);
  SLog('UnitName: ' + UnitName, INFO);
  SLog('FunctionName: ' + FunctionName, INFO);
  SLog('Row: ' + IntToStr(Row), INFO);
  SLog('Col: ' + IntToStr(Col), INFO);
  SLog('==========================================================', INFO);

  Result := True;
end;

var
  OnChangeCalled, OnChangingCalled: Boolean;

procedure MyOnChanging(Sender: TObject);
begin
  if Sender is TStringList then
    OnChangingCalled := True;
end;

procedure MyOnChange(Sender: TObject);
begin
  if Sender is TStringList then
    OnChangeCalled := True;
end;

//
// Tests
//

function EventsTest: String;
var
  i: Integer;
  MyStringList: TStringList;
  //// Unhandled exception
  //Team: TTeam;
  //ID: Integer;
begin
  Result := 'Unknown failure';

  Game.OnClockTick := @MyOnClockTick;
  Game.OnIdle := @MyOnIdle;
  Map.OnBeforeMapChange := @MyOnBeforeMapChange;
  Map.OnAfterMapChange := @MyOnAfterMapChange;
  Game.OnRequest := @MyOnRequest;
  Game.OnAdminConnect := @MyOnAdminConnect;
  Game.OnAdminDisconnect := @MyOnAdminDisconnect;
  Game.OnTCPMessage := @MyOnTCPMessage;
  Game.OnJoin := @MyGameOnJoin;
  Game.OnLeave := @MyGameOnLeave;
  Game.OnTCPCommand := @MyOnTCPCommand;
  Game.OnAdminCommand := @MyOnAdminCommand;
  for i := 0 to 5 do
  begin
    Game.Teams[i].OnJoin := @MyTeamOnJoin;
    Game.Teams[i].OnLeave := @MyTeamOnLeave;
    Game.Teams[i].OnBeforeJoin := @MyOnBeforeJoin;
  end;

  Script.OnException := @MyOnException;
  Script.OnUnhandledException := @MyOnUnhandledException;

  //// Unhandled exception
  //Team := Nil;
  //ID := Team.ID;

  // OnChange event
  MyStringList := File.CreateStringList;
  OnChangeCalled := False;
  OnChangingCalled := False;
  MyStringList.OnChanging := @MyOnChanging
  MyStringList.OnChange := @MyOnChange
  MyStringList.Add('TestItem');
  MyStringList.Free;

  if not OnChangingCalled then
  begin
    Result := 'TStringList.OnChanging';
    Exit;
  end;

  if not OnChangeCalled then
  begin
    Result := 'TStringList.OnChange';
    Exit;
  end;

  TestCount := 0;
  ObjectStyle := OBJECT_STATIONARY_GUN;
  for i := 1 to 32 do
  begin
    Players[i].OnFlagGrab := @MyOnFlagGrab;
    Players[i].OnFlagScore := @MyOnFlagScore;
    Players[i].OnFlagReturn := @MyOnFlagReturn;
    Players[i].OnFlagDrop := @MyOnFlagDrop;
    Players[i].OnKitPickup := @MyOnKitPickup;
    Players[i].OnBeforeRespawn := @MyOnBeforeRespawn;
    Players[i].OnAfterRespawn := @MyOnAfterRespawn;
    Players[i].OnDamage := @MyOnDamage;
    Players[i].OnKill := @MyOnKill;
    Players[i].OnWeaponChange := @MyOnWeaponChange;
    Players[i].OnVoteMapStart := @MyOnVoteMapStart;
    Players[i].OnVoteKickStart := @MyOnVoteKickStart;
    Players[i].OnVoteMap := @MyOnVoteMap;
    Players[i].OnVoteKick := @MyOnVoteKick;
    Players[i].OnSpeak := @MyOnSpeak;
    Players[i].OnCommand := @MyOnCommand;
  end;

  Result := '';
end;

function BanListsTest: String;
var
  i: Integer;
begin
  Result := 'Unknown failure';

  for i := 0 to 100 do
  begin
    if Game.BanLists.IsBannedHW('TestHW' + IntToStr(i)) then
    begin
      Result := 'TestHW' + IntToStr(i) + ' Banned before added to ban list???';
      Exit;
    end;
    if Game.BanLists.IsBannedIP('TestIP' + IntToStr(i)) then
    begin
      Result := 'TestIP' + IntToStr(i) + ' Banned before added to ban list???';
      Exit;
    end;
  end;

  for i := 0 to 100 do
  begin
    Game.BanLists.AddHWBan('TestHW' + IntToStr(i),
      'Test Reason ' + IntToStr(i), i * 100);
    Game.BanLists.AddIPBan('TestIP' + IntToStr(i),
      'Test Reason ' + IntToStr(i), i * 100);
  end;

  for i := 0 to 100 do
  begin
    if not Game.BanLists.IsBannedHW('TestHW' + IntToStr(i)) then
    begin
      Result := 'TestHW' + IntToStr(i) + ' not banned';
      Exit;
    end;
    if not Game.BanLists.IsBannedIP('TestIP' + IntToStr(i)) then
    begin
      Result := 'TestIP' + IntToStr(i) + ' not banned';
      Exit;
    end;
  end;

  if Game.BanLists.GetHWBanId('asdf') <> -1 then
  begin
    Result := 'Fake HWID did not return -1';
    Exit;
  end;
  if Game.BanLists.GetIPBanId('asdf') <> -1 then
  begin
    Result := 'Fake IP did not return -1';
    Exit;
  end;

  for i := 0 to 100 do
  begin
    Game.BanLists.DelHWBan('TestHW' + IntToStr(i));
    Game.BanLists.DelIPBan('TestIP' + IntToStr(i));
  end;

  Result := '';
end;

function FunctionsTest: String;
var
  i, j: Integer;
  MyRGB: LongWord;
  MyStrings: TStringList;
begin
  Result := 'Unknown failure';


  if StrToInt('420') <> 420 then
  begin
    Result := 'StrToInt';
    Exit;
  end;

  if MD5('test input') <> '5eed650258ee02f6a77c87b748b764ec' then
  begin
    Result := 'MD5: ' + MD5('test input');
    Exit;
  end;

  if not FloatCmp(ArcTan(1), PI / 4) then
  begin
    Result := 'ArcTan';
    Exit;
  end;

  if not FloatCmp(Ln(2.71828), 1) then
  begin
    Result := 'Ln';
    Exit;
  end;

  if not FloatCmp(Log10(2), 0.30102999566) then
  begin
    Result := 'Log10';
    Exit;
  end;

  if not FloatCmp(LogN(3, 10), 2.09590327429) then
  begin
    Result := 'LogN';
    Exit;
  end;

  if not FloatCmp(Exp(2), 7.38905609893) then
  begin
    Result := 'Exp';
    Exit;
  end;

  for i := 1 to 100 do
  begin
    j := Random(-i, i);
    if (j < -i) or (j >= i) then
    begin
      Result := 'Random';
      Exit;
    end;
  end;

  MyRGB := RGB(1, 2, 3);
  if ((MyRGB shr 16) and $FF) <> 1 then
  begin
    Result := 'RGB R';
    Exit;
  end;
  if ((MyRGB shr 8) and $FF) <> 2 then
  begin
    Result := 'RGB G';
    Exit;
  end;
  if (MyRGB and $FF) <> 3 then
  begin
    Result := 'RGB B';
    Exit;
  end;

  if not FloatCmp(Distance(0, 0, 3, 4), 5) then
  begin
    Result := 'Distance';
    Exit;
  end;

  if Weap2Obj(WTYPE_EAGLE) <> OBJECT_DESERT_EAGLE then
  begin
    Result := 'Weap2Obj';
    Exit;
  end;
  if Menu2Obj(1) <> OBJECT_DESERT_EAGLE then
  begin
    Result := 'Menu2Obj';
    Exit;
  end;
  if Obj2Weap(OBJECT_DESERT_EAGLE) <> WTYPE_EAGLE then
  begin
    Result := 'Obj2Weap';
    Exit;
  end;
  if Obj2Menu(OBJECT_DESERT_EAGLE) <> 1 then
  begin
    Result := 'Obj2Menu';
    Exit;
  end;
  if Weap2Menu(WTYPE_EAGLE) <> 1 then
  begin
    Result := 'Weap2Menu';
    Exit;
  end;
  if Menu2Weap(1) <> WTYPE_EAGLE then
  begin
    Result := 'Menu2Weap';
    Exit;
  end;

  if not ExecRegExpr('asdf.*qwerty[0-9]+', 'asdfrandomqwertyqwerty239') then
  begin
    Result := 'ExecRegExpr';
    Exit;
  end;

  MyStrings := File.CreateStringList;
  SplitRegExpr('-', '867-5309', MyStrings);
  if (MyStrings.Count <> 2) or (MyStrings[0] <> '867') or (MyStrings[1] <> '5309') then
  begin
    Result := 'SplitRegExpr';
    MyStrings.Free;
    Exit;
  end;
  MyStrings.Free;

  if ReplaceRegExpr('buggy', 'scriptcore is so buggy!', 'awesome', True) <>
    'scriptcore is so awesome!' then
  begin
    Result := 'ReplaceRegExpr';
    Exit;
  end;

  MyStrings := File.CreateStringList;
  RegExprSubExpressions('(asdf)(qwerty)', MyStrings, True);
  if (MyStrings[1] <> 'asdf') or (MyStrings[2] <> 'qwerty') then
  begin
    Result := 'RegExprSubExpressions';
    Exit;
  end;
  MyStrings.Free;

  if FormatFloat('00.000#', 9.12) <> '09.120' then
  begin
    Result := 'FormatFloat';
    Exit;
  end;

  if QuoteRegExprMetaChars('abc''cd.(') <> 'abc''cd\.\(' then
  begin
    Result := 'QuoteRegExprMetaChars';
    Exit;
  end;

  Result := '';
end;

function DateUtilsTest: String;
var
  YearIn, MonthIn, DayIn, YearOut, MonthOut, DayOut: Word;
  HourIn, MinIn, SecIn, MSecIn, HourOut, MinOut, SecOut, MSecOut: Word;
  S: String;
  MyDateTime, MyDateTime2: TDateTime;
  i64: Int64;
begin
  Result := 'Unknown failure';

  for YearIn := 1970 to 2021 do
  begin
    for MonthIn := 1 to 12 do
    begin
      for DayIn := 1 to 28 do
      begin
        MyDateTime := EncodeDate(YearIn, MonthIn, DayIn);
        DecodeDate(MyDateTime, YearOut, MonthOut, DayOut);

        if (YearOut <> YearIn) or (MonthOut <> MonthIn) or (DayOut <> DayIn) then
        begin
          Result := 'EncodeDate roundtrip';
          Exit;
        end;
      end;
    end;
  end;

  for HourIn := 0 to 23 do
  begin
    for MinIn := 0 to 30 do
    begin
      for SecIn := 30 to 59 do
      begin
        for MSecIn := 0 to 10 do
        begin
          MyDateTime := EncodeTime(HourIn, MinIn, SecIn, MSecIn);
          DecodeTime(MyDateTime, HourOut, MinOut, SecOut, MSecOut);

          if (HourOut <> HourIn) or (MinOut <> MinIn) or (SecOut <> SecIn) or
            (MSecOut <> MSecIn) then
          begin
            Result := 'EncodeTime roundtrip';
            Exit;
          end;
        end;
      end;
    end;
  end;

  for YearIn := 1970 to 1972 do
  begin
    for MonthIn := 3 to 5 do
    begin
      for DayIn := 3 to 10 do
      begin
        for HourIn := 12 to 15 do
        begin
          for MinIn := 5 to 8 do
          begin
            for SecIn := 12 to 13 do
            begin
              for MSecIn := 0 to 2 do
              begin
                MyDateTime := EncodeDateTime(YearIn, MonthIn, DayIn, HourIn, MinIn, SecIn, MSecIn);
                DecodeDateTime(MyDateTime, YearOut, MonthOut, DayOut, HourOut, MinOut, SecOut, MSecOut);

                if (YearOut <> YearIn) or (MonthOut <> MonthIn) or (DayOut <> DayIn) or
                  (HourOut <> HourIn) or (MinOut <> MinIn) or (SecOut <> SecIn) or
                  (MSecOut <> MSecIn) then
                begin
                  Result := 'EncodeDateTime roundtrip';
                  Exit;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  MyDateTime := EncodeDate(1970, 1, 1);
  if DayOfWeek(MyDateTime) <> 5 then
  begin
    Result := 'DayOfWeek';
    Exit;
  end;

  i64 := DateTimeToUnix(MyDateTime);
  MyDateTime2 := UnixToDateTime(i64);
  if MyDateTime <> MyDateTime2 then
  begin
    Result := 'DateTimeToUnix roundtrip';
    Exit;
  end;

  MyDateTime := Date;
  S := DateToStr(MyDateTime);
  MyDateTime2 := StrToDate(S);
  if MyDateTime <> MyDateTime2 then
  begin
    Result := 'DateToStr roundtrip';
    Exit;
  end;

  Result := '';
end;

function FileAPITest: String;
var
  MyIniFile: TIniFile;
  MyStringList, MyStringList2: TStringList;
  MyFileStream: TFileStream;
  MyStringStream: TStringStream;
  MyDateTime1, MyDateTime2, MyDateTime3: TDateTime;
  YearIn, MonthIn, DayIn, YearOut, MonthOut, DayOut: Word;
  HourIn, MinIn, SecIn, MSecIn, HourOut, MinOut, SecOut, MSecOut: Word;
  i: Integer;
  S: String;
  Obj1, Obj2, Obj3, Obj4: TObject;
begin
  Result := 'Unknown failure';

  // TStringList
  MyStringList := File.CreateStringList;
  MyStringList.Add('Asdf');
  MyStringList.Add('Qwerty');
  MyStringList.Append('Uiop');
  MyStringList.Sort;
  MyStringList.Sorted := True;

  // 'Asdf' 'Qwerty' 'Uiop'
  if MyStringList.Count <> 3 then
  begin
    Result := 'TStringList.Count';
    Exit;
  end;

  MyStringList.SaveToFile('~/stringlist.txt');
  // 1: 'Asdf' 'Qwerty' 'Uiop'
  MyStringList2 := File.CreateStringListFromFile('~/stringlist.txt');
  File.Delete('~/stringlist.txt');

  if (MyStringList.Text <> 'Asdf'#10'Qwerty'#10'Uiop'#10 ) and
    (MyStringList.Text <> 'Asdf'#13#10'Qwerty'#13#10'Uiop'#13#10) then
  begin
    Result := 'TStringList.Text';
    Exit;
  end;

  if MyStringList.CommaText <> 'Asdf,Qwerty,Uiop' then
  begin
    Result := 'TStringList.CommaText';
    Exit;
  end;

  if not MyStringList.Equals(MyStringList2) then
  begin
    Result := 'TStringList roundtrip';
    Exit;
  end;

  i := 1;
  if not MyStringList.Find('Asdf', i) then
  begin
    Result := 'TStringList.Find';
    Exit;
  end;
  if i <> 0 then
  begin
    Result := 'TStringList.Find';
    Exit;
  end;

  MyStringList2.Clear;
  if MyStringList2.Count <> 0 then
  begin
    Result := 'TStringList.Clear';
    Exit;
  end;

  // 2: 'Asdf' 'Qwerty' 'Uiop'
  MyStringList2.BeginUpdate;
  MyStringList2.AddStrings(MyStringList);
  MyStringList2.EndUpdate;
  if not MyStringList.Equals(MyStringList2) then
  begin
    Result := 'TStringList.AddStrings';
    Exit;
  end;

  if MyStringList2.IndexOf('Qwerty') <> 1 then
  begin
    Result := 'TStringList.IndexOf found';
    Exit;
  end;

  // 2: 'Qwerty' 'Asdf' 'Uiop'
  MyStringList2.Exchange(0, 1);
  if (MyStringList2.IndexOf('Asdf') <> 1) or
    (MyStringList2.IndexOf('Qwerty') <> 0) then
  begin
    Result := 'TStringList.Exchange';
    Exit;
  end;

  if MyStringList2.Equals(MyStringList) then
  begin
    Result := 'TStringList.Equals false';
    Exit;
  end;

  if MyStringList2.IndexOf('kanye') <> -1 then
  begin
    Result := 'TStringList.IndexOf not found';
    Exit;
  end;

  // 1: 'Qwerty' 'Uiop' 'Asdf'
  MyStringList.Sorted := False;
  MyStringList.Move(0, 2);
  if (MyStringList[0] <> 'Qwerty') or (MyStringList[1] <> 'Uiop') or
    (MyStringList[2] <> 'Asdf') then
  begin
    Result := 'TStringList.Move';
    Exit;
  end;


  // 1: 'Uiop' 'Asdf'
  MyStringList.Delete(0);
  if (MyStringList.Count <> 2) or (MyStringList[0] <> 'Uiop') or
    (MyStringList[1] <> 'Asdf') then
  begin
    Result := 'TStringList.Delete';
    Exit;
  end;

  Obj1 := TObject.Create;
  Obj2 := TObject.Create;
  Obj3 := TObject.Create;
  Obj4 := TObject.Create;

  // ('Uiop', Obj1) ('Asdf', Obj2) ('Insert', Obj4) ('Qwerty', Obj3)
  MyStringList.Objects[0] := Obj1;
  MyStringList.Objects[1] := Obj2;
  MyStringList.AddObject('Qwerty', Obj3);
  MyStringList.InsertObject(2, 'Insert', Obj4);

  if MyStringList.IndexOfObject(Obj1) <> 0 then
  begin
    Result := 'TStringList.IndexOfObject found';
    Exit;
  end;
  if MyStringList.IndexOfObject(Nil) <> -1 then
  begin
    Result := 'TStringList.IndexOfObject not found';
    Exit;
  end;

  if (MyStringList.Count <> 4) or (MyStringList[0] <> 'Uiop') or
    (MyStringList[1] <> 'Asdf') or (MyStringList[2] <> 'Insert') or
    (MyStringList[3] <> 'Qwerty') or (MyStringList.Objects[0] <> Obj1) or
    (MyStringList.Objects[1] <> Obj2) or (MyStringList.Objects[2] <> Obj4) or
    (MyStringList.Objects[3] <> Obj3) then
  begin
    Result := 'TStringList.Add/InsertObject';
    Exit;
  end;

  Obj1.Free;
  Obj2.Free;
  Obj3.Free;
  Obj4.Free;

  S := 'one'#10'two'#10'three';
  MyStringList.SetText(PChar(S));
  if (MyStringList.Count <> 3) or (MyStringList[0] <> 'one') or
    (MyStringList[1] <> 'two') or (MyStringList[2] <> 'three') then
  begin
    Result := 'TStringList.SetText';
    Exit;
  end;

  MyStringList.Free;
  MyStringList2.Free;

  // TIniFile
  MyIniFile := File.CreateIni('~/lmao.ini');
  MyIniFile.WriteString('section', 'strident', 'value');
  MyIniFile.WriteInteger('section', 'intident', 10);
  MyIniFile.WriteBool('section2', 'boolident', False);
  YearIn := 2021;
  MonthIn := 11;
  DayIn := 2;
  HourIn := 5;
  MinIn := 56;
  SecIn := 34;
  MSecIn := 506;
  MyDateTime1 := EncodeDate(YearIn, MonthIn, DayIn);
  MyIniFile.WriteDate('section2', 'dateident1', MyDateTime1);
  MyDateTime2 := EncodeTime(HourIn, MinIn, SecIn, MSecIn);
  MyIniFile.WriteTime('section2', 'dateident2', MyDateTime2);
  MyDateTime3 := EncodeDateTime(YearIn, MonthIn, DayIn, HourIn, MinIn, SecIn,
    MSecIn);
  MyIniFile.WriteDateTime('section2', 'dateident3', MyDateTime3);
  MyIniFile.WriteFloat('section2', 'floatident', 2.0);

  if MyIniFile.ReadString('section', 'strident', 'foo') <> 'value' then
  begin
    Result := 'TIniFile.ReadString';
    Exit;
  end;

  if MyIniFile.ReadInteger('section', 'intident', 100) <> 10 then
  begin
    Result := 'TIniFile.ReadInteger';
    Exit;
  end;

  if MyIniFile.ReadBool('section2', 'boolident', True) <> False then
  begin
    Result := 'TIniFile.ReadBool';
    Exit;
  end;

  DecodeDate(MyIniFile.ReadDate('section2', 'dateident1', 1.0), YearOut,
    MonthOut, DayOut);
  if (YearOut <> YearIn) or (MonthOut <> MonthIn) or (DayOut <> DayIn) then
  begin
    Result := 'TIniFile.ReadDate';
    Exit;
  end;

  // NOTE: Doesn't have millisecond precision for whatever reason.
  DecodeTime(MyIniFile.ReadTime('section2', 'dateident2', 1.0), HourOut,
    MinOut, SecOut, MSecOut);
  if (HourOut <> HourIn) or (MinOut <> MinIn) or (SecOut <> SecIn) then
  begin
    Result := 'TIniFile.ReadTime';
    Exit;
  end;

  // NOTE: Doesn't have millisecond precision for whatever reason.
  DecodeDateTime(MyIniFile.ReadDateTime('section2', 'dateident3', 1.0),
    YearOut, MonthOut, DayOut, HourOut, MinOut, SecOut, MSecOut);
  if (YearOut <> YearIn) or (MonthOut <> MonthIn) or (DayOut <> DayIn) or
    (HourOut <> HourIn) or (MinOut <> MinIn) or (SecOut <> SecIn) then
  begin
    Result := 'TIniFile.ReadDateTime';
    Exit;
  end;

  if MyIniFile.ReadFloat('section2', 'floatident', 1.0) <> 2.0 then
  begin
    Result := 'TIniFile.ReadFloat';
    Exit;
  end;

  MyIniFile.CaseSensitive := False;
  if MyIniFile.ReadString('SeCtIoN', 'StRiDeNt', 'default') <> 'value' then
  begin
    Result := 'TIniFile.CaseSensitive = False';
    Exit;
  end;

  MyIniFile.CaseSensitive := True;
  if MyIniFile.ReadString('SeCtIoN', 'StRiDeNt', 'default') <> 'default' then
  begin
    Result := 'TIniFile.CaseSensitive = True';
    Exit;
  end;

  if Pos('lmao.ini', MyIniFile.FileName) = 0 then
  begin
    Result := 'TIniFile.FileName';
    Exit;
  end;

  if not MyIniFile.SectionExists('section') then
  begin
    Result := 'TIniFile.SectionExists';
    Exit;
  end;

  // TODO: ReadBinaryStream, WriteBinaryStream

  MyIniFile.UpdateFile;
  MyIniFile.Free;

  if not File.Exists('~/lmao.ini') then
  begin
    Result := 'TIniFile.UpdateFile';
    Exit;
  end;

  if not File.Move('~/lmao.ini', '~/rofl.ini') then
  begin
    Result := 'TFile.Move';
    Exit;
  end;

  if (File.Exists('~/lmao.ini')) or (not File.Exists('~/rofl.ini')) then
  begin
    Result := 'TFile.Move postconditions';
    Exit;
  end;

  if not File.Copy('~/rofl.ini', '~/copy.ini') then
  begin
    Result := 'TFile.Copy';
    Exit;
  end;

  if (not File.Exists('~/rofl.ini')) or (not File.Exists('~/copy.ini')) then
  begin
    Result := 'TFile.Copy postconditions';
    Exit;
  end;

  File.Delete('~/rofl.ini');
  File.Delete('~/copy.ini');
  if File.Exists('~/rofl.ini') or File.Exists('~/copy.ini') then
  begin
    Result := 'TFile.Delete';
    Exit;
  end;

  if not File.CheckAccess('~/random.file') then
  begin
    Result := 'TFile.CheckAccess';
    Exit;
  end;
  if File.CheckAccess('C:\important\stuff') then
  begin
    Result := 'TFile.CheckAccess sandbox broken, did you change SandboxLevel?';
    Exit;
  end;

  // TFileStream
  MyFileStream := File.CreateFileStream;

  S := 'part1';
  if MyFileStream.Write(S, 5) <> 5 then
  begin
    Result := 'TFileStream.Write';
    Exit;
  end;
  S := 'part2';
  MyFileStream.WriteBuffer(S, 5);

  MyFileStream.SaveToFile('~/test.txt');
  MyFileStream.Free;

  MyFileStream := File.CreateFileStreamFromFile('~/test.txt');

  SetLength(S, 5);
  if MyFileStream.Read(S, 5) <> 5 then
  begin
    Result := 'TFileStream.Read';
    Exit;
  end;
  if S <> 'part1' then
  begin
    Result := 'TFileStream.Read'
    Exit;
  end;

  MyFileStream.ReadBuffer(S, 5);
  if S <> 'part2' then
  begin
    Result := 'TFileStream.ReadBuffer';
    Exit;
  end;

  MyFileStream.Free;
  File.Delete('~/test.txt');

  // TStringStream
  MyStringStream := TStringStream.Create;

  MyStringStream.WriteString('asdf');

  if MyStringStream.Write('qwertyuiop', 6) <> 6 then
  begin
    Result := 'TStringStream.Write';
    Exit;
  end;

  if MyStringStream.DataString <> 'asdfqwerty' then
  begin
    Result := 'TStringStream.DataString';
    Exit;
  end;

  MyStringStream.Seek(0, soBeginning);
  S := MyStringStream.ReadString(4);
  if S <> 'asdf' then
  begin
    Result := 'TStringStream.Seek/ReadString';
    Exit;
  end;

  SetLength(S, 6);
  if MyStringStream.Read(S, 6) <> 6 then
  begin
    Result := 'TStringStream.Read';
    Exit;
  end;
  if S <> 'qwerty' then
  begin
    Result := 'TStringStream.Read';
    Exit;
  end;

  MyStringStream.Free;

  Result := '';
end;

function MathTest: String;
var
  i: Integer;
  ie, j, Sinus, Cosinus: Extended;
begin
  Result := 'Unknown failure';

  if not FloatCmp(Math.Sin(PI / 4.0), 0.70710678119) then
  begin
    Result := 'Math.Sin';
    Exit;
  end;

  if not FloatCmp(Math.Cos(PI), -1.0) then
  begin
    Result := 'Math.Cos';
    Exit;
  end;

  if not FloatCmp(Math.Tan(PI / 4.0), 1.0) then
  begin
    Result := 'Math.Tan';
    Exit;
  end;

  if not FloatCmp(Math.Cotan(1.0), 0.64209261593433) then
  begin
    Result := 'Math.Cotan';
    Exit;
  end;

  if not FloatCmp(Math.Pow(2.0, 2.0), 4.0) then
  begin
    Result := 'Math.Pow';
    Exit;
  end;

  if not FloatCmp(Math.LogN(3, 10), 2.09590327429) then
  begin
    Result := 'Math.LogN';
    Exit;
  end;

  if not FloatCmp(Math.Ln(2.71828), 1.0) then
  begin
    Result := 'Math.Ln';
    Exit;
  end;

  if not FloatCmp(Math.ArcSin(1.0 / 2.0), PI / 6.0) then
  begin
    Result := 'Math.ArcSin';
    Exit;
  end;

  if not FloatCmp(Math.ArcCos(1.0 / 2.0), PI / 3.0) then
  begin
    Result := 'Math.ArcCos';
    Exit;
  end;

  if not FloatCmp(Math.ArcTan(1.0), PI / 4.0) then
  begin
    Result := 'Math.ArcTan';
    Exit;
  end;

  if not FloatCmp(Math.ArcCotan(PI / 4.0), 1.0) then
  begin
    Result := 'Math.ArcCotan';
    Exit;
  end;

  if not FloatCmp(Math.ArcTan2(1.0, 1.0), PI / 4.0) then
  begin
    Result := 'Math.ArcTan2';
    Exit;
  end;

  if not FloatCmp(Math.Min(-1.0, 1.0), -1.0) then
  begin
    Result := 'Math.Min';
    Exit;
  end;

  if not FloatCmp(Math.Max(-1.0, 1.0), 1.0) then
  begin
    Result := 'Math.Max';
    Exit;
  end;

  if not FloatCmp(Math.Abs(-PI), PI) then
  begin
    Result := 'Math.Abs';
    Exit;
  end;

  if not FloatCmp(Math.Exp(1.0), Math.E) then
  begin
    Result := 'Math.Exp';
    Exit;
  end;

  if (Math.Sign(-1.0) <> -1) or (Math.Sign(1.0) <> 1) then
  begin
    Result := 'Math.Sign';
    Exit;
  end;

  if Math.IsNaN(1.0) then
  begin
    Result := 'Math.IsNaN';
    Exit;
  end;

  if not FloatCmp(Math.Round(PI), 3.0) then
  begin
    Result := 'Math.Round';
    Exit;
  end;

  if not FloatCmp(Math.RoundTo(2.25, -1), 2.2) then
  begin
    Result := 'Math.RoundTo';
    Exit;
  end;

  if not FloatCmp(Math.DegToRad(90.0), PI / 2.0) then
  begin
    Result := 'Math.DegToRad';
    Exit;
  end;

  if not FloatCmp(Math.RadToDeg(PI / 2.0), 90.0) then
  begin
    Result := 'Math.RadToDeg';
    Exit;
  end;

  if not FloatCmp(Math.DegNormalize(370.0), 10.0) then
  begin
    Result := 'Math.DegNormalize';
    Exit;
  end;

  if Math.InRange(-1.0, 0.0, 2.0) then
  begin
    Result := 'Math.InRange';
    Exit;
  end;

  if not FloatCmp(Math.EnsureRange(-1.0, 0.0, 2.0), 0.0) then
  begin
    Result := 'Math.EnsureRange';
    Exit;
  end;

  for i := 1 to 100 do
  begin
    ie := i;
    j := Math.Random(-ie, ie);
    if (j < -ie) or (j >= ie) then
    begin
      Result := 'Math.Random';
      Exit;
    end;
  end;

  Math.SinCos(1.0, Sinus, Cosinus);
  if not FloatCmp(Sinus, Math.Sin(1.0)) then
  begin
    Result := 'Math.SinCos Sinus';
    Exit;
  end;
  if not FloatCmp(Cosinus, Math.Cos(1.0)) then
  begin
    Result := 'Math.SinCos Cosinus';
    Exit;
  end;

  if not FloatCmp(Math.E, 2.718281828459045) then
  begin
    Result := 'Math.E';
    Exit;
  end;

  if not FloatCmp(Math.PI, 3.14159265359) then
  begin
    Result := 'Math.PI';
    Exit;
  end;

  Result := '';
end;

function DefinesTest: String;
begin
  Result := 'Unknown failure';

  {$IFNDEF TEST_DEFINE}
  Result := 'Defines 1';
  Exit;
  {$ENDIF}

  {$IFNDEF TEST_DEFINE2}
  Result := 'Defines 2';
  Exit;
  {$ENDIF}

  Result := '';
end;

procedure RunAllTests;
var
  i, FailCount: Integer;
  Tests: Array[0..6] of TTest;
begin
  // Register tests.
  Tests[0].Name := 'BanLists'; // ScriptBanLists.pas
  Tests[0].Fn := @BanListsTest;
  Tests[1].Name := 'Functions'; // ScriptCoreFunctions.pas
  Tests[1].Fn := @FunctionsTest;
  Tests[2].Name := 'DateUtils'; // ScriptDateUtils.pas
  Tests[2].Fn := @DateUtilsTest;
  Tests[3].Name := 'FileAPI'; // ScriptFileAPI.pas
  Tests[3].Fn := @FileAPITest;
  Tests[4].Name := 'Math'; // ScriptMath.pas
  Tests[4].Fn := @MathTest;
  Tests[5].Name := 'Events'; // Events
  Tests[5].Fn := @EventsTest;
  Tests[6].Name := 'Defines'; // Defines
  Tests[6].Fn := @DefinesTest;

  // Run tests.
  for i := Low(Tests) to High(Tests) do
    RunTest(Tests[i]);

  // Print failures + summary.
  for i := Low(Tests) to High(Tests) do
  begin
    if TestIsFailed(Tests[i]) then
    begin
      PrintFailedTest(Tests[i]);
      Inc(FailCount, 1);
    end;
  end;

  if FailCount = 0 then
    SLog('All tests passed!', PASS)
  else
    SLog('Tests failed: ' + IntToStr(FailCount) + '/' +
      IntToStr(High(Tests) + 1), FAILURE);
end;

begin
  RunAllTests;
end.

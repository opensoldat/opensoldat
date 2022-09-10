unit ServerHelper;

interface

function CheckNextMap: string;
procedure WriteLn(S: Variant); overload;
function IDToName(ID: Integer): string;
function TeamToName(ID: Integer): string;
function NameToID(Name: string): Integer;
function NameToHW(Name: string): string;
function FindLowestTeam(const Arr: array of Integer): Integer;
procedure SaveTxtLists;
procedure SaveMapList;
function RGB(r, g, b: Byte): Cardinal;
function FixTeam(Team: Byte): Byte;
function WeaponNameByNum(Num: Integer): string;
function CheckFileSize(filename: string): Integer;
procedure WritePID;
function GetPID: Integer;
procedure WriteConsole(ID: Byte; Text: string; Colour: UInt32);
procedure UpdateWaveRespawnTime;
function RandomBot: string;
procedure DoBalanceBots(LeftGame: Byte; NewTeam: Byte);

implementation

uses
  Server, sysutils, strutils, classes, Cvar, Game, Net, Sprites, Constants, Util, Weapons,
  TraceLog, NetworkServerMessages, BanSystem, Command;

procedure WriteLn(S: Variant); overload;
begin
  if CvarsInitialized and log_timestamp.Value then
    System.WriteLn(FormatDateTime('[hh:nn:ss] ', Now) + S)
  else
    System.WriteLn(S);
end;

function IDtoName(ID: Integer): String;
begin
  Result := '';
  if ID > MAX_PLAYERS then
  begin
    Result := 'Server Admin';
    Exit;
  end;
  if (Sprite[ID].Active) then
    Result := Sprite[ID].Player.Name;
end;

function TeamToName(ID: Integer): String;
begin
  Result := 'UNKNOWN';
  if ID > 5 then
    Exit;

  case ID of
    0: Result := 'NA';
    1: Result := 'A';
    2: Result := 'B';
    3: Result := 'C';
    4: Result := 'D';
    5: Result := 'Spectator';
  end;
end;

function NameToID(Name: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to MAX_SPRITES do
    if Sprite[i].Player.Name = Name then
      Result := i;
end;

function NameToHW(Name: String): String;
var
  i: Integer;
begin
  Result := '0';
  for i := 1 to MAX_SPRITES do
    if Sprite[i].Player.Name = Name then
      Result := Sprite[i].Player.hwid;
end;

function RGB(r, g, b: Byte): Cardinal;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;

function FindLowestTeam(const Arr: array of Integer): Integer;
var i, tmp: Integer;
begin
  tmp := 1;
  for i := 1 to iif(sv_gamemode.Value = GAMESTYLE_TEAMMATCH, 4, 2) do
  begin
    if Arr[tmp] > arr[i] then
      tmp := i;
  end;
  Result := tmp;
end;

function FixTeam(Team: Byte): Byte;
begin
  Result := Team;

  case sv_gamemode.Value of
    GAMESTYLE_DEATHMATCH, GAMESTYLE_POINTMATCH, GAMESTYLE_RAMBO:
    begin
      // Choose to team 0 or leave at team spectator
      if (Team <> TEAM_NONE) and
         (Team <> TEAM_SPECTATOR) then
        Result := TEAM_NONE;
    end;
    GAMESTYLE_TEAMMATCH:
    begin
      if (Team <> TEAM_ALPHA) and
         (Team <> TEAM_BRAVO) and
         (Team <> TEAM_CHARLIE) and
         (Team <> TEAM_DELTA) and
         (Team <> TEAM_SPECTATOR) then
        Result := Random(4) + 1;  // team 1..4
    end;
    GAMESTYLE_CTF, GAMESTYLE_INF, GAMESTYLE_HTF:
    begin
      if (Team <> TEAM_ALPHA) and
         (Team <> TEAM_BRAVO) and
         (Team <> TEAM_SPECTATOR) then
        Result :=  Random(2) + 1;  // team 1..2
    end;
  end;
end;

function WeaponNameByNum(Num: Integer): string;
var
  WeaponIndex: Integer;
begin
  Trace('WeaponNameByNum');
  Result := 'USSOCOM';

  if Num = 100 then
  begin
    Result := 'Selfkill';
    Exit;
  end;

  for WeaponIndex := Low(Guns) to High(Guns) do
  begin
    if Num = Guns[WeaponIndex].Num then
    begin
      Result := Guns[WeaponIndex].Name;
      Break;
    end;
  end;
end;

function CheckFileSize(filename: string): Integer;
var
  sr: tsearchrec;
begin
  findfirst(filename, faanyfile, sr);

  Result := sr.size;

  SysUtils.FindClose(sr);
end;

function CheckNextMap: string;
var
  M: Integer;
begin
  Trace('CheckNextMap');
  Result := 'NOMAP';
  if MapsList.Count < 1 then
  begin
    Result := 'NOMAP';
  end else
  begin
    M := MapIndex + 1;

    if M >= MapsList.Count then
      M := 0;

    Result := MapsList[M];
  end;
end;

procedure SaveTxtLists;
begin
  Trace('SaveTxtLists');

  // save ban files
  SaveBannedList(UserDirectory + 'configs/banned.txt');
  SaveBannedListHW(UserDirectory + 'configs/bannedhw.txt');

  RemoteIPs.SaveToFile(UserDirectory + 'configs/remote.txt');
end;

procedure SaveMapList;
begin
  MapsList.SaveToFile(UserDirectory + 'configs/' + sv_maplist.Value);
end;

procedure WritePID;
var
  PID: Cardinal;
  PIDFile: TextFile;
begin
  try
    PID := GetPID();
    AssignFile(PIDFile, UserDirectory + 'logs/' + sv_pidfilename.Value);
    FileMode := fmOpenWrite;
    ReWrite(PIDFile);
    Write(PIDFile, IntToStr(PID));
    WriteLn(' Server PID: ' + IntToStr(PID));
    CloseFile(PIDFile);
  except
    WriteLn('Error writing PID file');
  end;
end;

function GetPID: Integer;
begin
  Result := System.GetProcessID;
end;

procedure WriteConsole(ID: Byte; Text: string; Colour: UInt32);
begin
  // Write text to the console of ALL Players
  ServerSendSpecialMessage(Text, 0, 0, 0, 0, Colour, 0, 0, ID);
end;

procedure UpdateWaveRespawnTime;
begin
  WaveRespawnTime := Round(PlayersNum * WAVERESPAWN_TIME_MULITPLIER) * 60;
  if WaveRespawnTime > sv_respawntime_minwave.Value then
    WaveRespawnTime := sv_respawntime_maxwave.Value;
  WaveRespawnTime := WaveRespawnTime - sv_respawntime_minwave.Value;
  if WaveRespawnTime < 1 then
    WaveRespawnTime := 1;
end;

function RandomBot: string;
var
  BotList: TStringList;
  SelectedBot: string;
  searchResult: TSearchRec;
begin
  BotList := TStringList.Create;
  if FindFirst(UserDirectory + 'configs/bots/*.bot', faAnyFile, searchResult) = 0 then
  begin
    repeat
      BotList.Add(searchResult.Name)
    until FindNext(searchResult) <> 0;
  end;
  FindClose(searchResult);

  if BotList.Count > 0 then
  begin
    SelectedBot := BotList[Random(BotList.Count)];
    SelectedBot := AnsiReplaceStr(SelectedBot, UserDirectory + 'configs/bots/', '');
    SelectedBot := AnsiReplaceStr(SelectedBot, '.bot', '');
  end else
  begin
    SelectedBot := 'Dummy';
  end;

  if (SelectedBot = 'Boogie Man') or (SelectedBot = 'Dummy') then
    SelectedBot := 'Sniper';

  BotList.Free;
  result := SelectedBot;
end;

procedure DoBalanceBots(LeftGame: Byte; NewTeam: Byte);
var
  i: Integer;
  Teams: array[1..4] of Integer;
  TheBot: string;
begin
  if not sv_botbalance.Value then
    Exit;
  if (sv_gamemode.Value <> GAMESTYLE_CTF) and
     (sv_gamemode.Value <> GAMESTYLE_HTF) and
     (sv_gamemode.Value <> GAMESTYLE_INF) then
    Exit;

  Teams[1] := 0;
  Teams[2] := 0;
  Teams[3] := 0;
  Teams[4] := 0;

  for i := 1 to MAX_SPRITES do
    if Sprite[i].Active and Sprite[i].IsNotSpectator() then
      Inc(Teams[Sprite[i].Player.Team]);

  if LeftGame = 1 then
  begin
    // Player Left Game
    for i := 1 to MAX_SPRITES do
      if (Sprite[i].Player.ControlMethod = BOT) and Sprite[i].Active then
      begin
        if (Teams[1] > Teams[2]) and (Sprite[i].Player.Team = TEAM_ALPHA) then
        begin
          KickPlayer(i, False, KICK_LEFTGAME, 0);
          Exit;
        end;
        if (Teams[2] > Teams[1]) and (Sprite[i].Player.Team = TEAM_BRAVO) then
        begin
          KickPlayer(i, False, KICK_LEFTGAME, 0);
          Exit;
        end;
      end;
  end else
  begin
    // Player Joined Game}
    for i := 1 to MAX_SPRITES do
      if Sprite[i].Active and (Sprite[i].Player.ControlMethod = BOT) and
         (Sprite[i].Player.Team = NewTeam) then
      begin
        if Teams[1] > Teams[2] then
        begin
          KickPlayer(i, False, KICK_LEFTGAME, 0);
          if Sprite[i].Player.Team = NewTeam then
            DoBalanceBots(1, 2);
          Exit;
        end;
        if Teams[2] > Teams[1] then
        begin
          KickPlayer(i, False, KICK_LEFTGAME, 0);
          if Sprite[i].Player.Team = NewTeam then
            DoBalanceBots(1, 1);
          Exit;
        end;
      end;
  end;
  if Teams[1] > Teams[2] then
  begin
    TheBot := RandomBot;
    ParseInput('addbot2 ' + TheBot, 1);
    MainConsole.Console(TheBot + ' has joined bravo team. (Bot Balance)',
      ENTER_MESSAGE_COLOR);
    Exit;
  end;
  if Teams[2] > Teams[1] then
  begin
    TheBot := RandomBot;
    ParseInput('addbot1 ' + TheBot, 1);
    MainConsole.Console(TheBot + ' has joined alpha team. (Bot Balance)',
      ENTER_MESSAGE_COLOR);
    Exit;
  end;
end;

end.

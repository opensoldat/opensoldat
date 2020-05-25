unit Rcon;

interface

uses
  contnrs, syncobjs,
  IdSocketHandle, IdGlobal, IdTCPServer, IdIOHandlerSocket, IdTCPConnection,
  IdContext, IdCustomTCPServer, Classes, SysUtils, Version, Constants, Net;

const
  REFRESHX_HEADER_CHARS = 10;

type
  TServerConnect = procedure(AThread: TIdContext) of object;
  TServerDisconnect = procedure(AThread: TIdContext) of object;
  TServerExecute = procedure(AThread: TIdContext) of object;

  TAdminMessage = record
    Msg, IP: String;
    Port: TIdPort;
  end;

  TAdminServer = class(TIdTCPServer)
  private
    FPassword: string;
    FAdmins: TThreadList;
    FConnectMessage: string;
    FBytesSent: Int64;
    FBytesRecieved: Int64;
    FMessageQueue: TQueue;
    FQueueLock: TCriticalSection;
    procedure handleConnect(AThread: TIdContext);
    procedure handleDisconnect(AThread: TIdContext);
    procedure handleExecute(AThread: TIdContext);
  public
    property Admins: TThreadList read FAdmins;
    property Password: string read FPassword write FPassword;
    property ConnectMessage: string read FConnectMessage write FConnectMessage;
    property BytesSent: Int64 read FBytesSent;
    property BytesRecieved: Int64 read FBytesRecieved;
    constructor Create(Pass: string; ConnectMessage: string = ''); reintroduce;
    destructor Destroy; reintroduce;
    procedure SendToAll(Message: string);
    procedure SendToIP(IP: string; Message: string);
    procedure ProcessCommands();
  end;

  // Extra Server Information
  TMsg_RefreshX = packed record
    Header: array[1..REFRESHX_HEADER_CHARS] of Char;
    Name: array[1..MAX_PLAYERS] of string[PLAYERNAME_CHARS];
    hwid: array[1..MAX_PLAYERS] of string[PLAYERHWID_CHARS];
    Team: array[1..MAX_PLAYERS] of Byte;
    Kills: array[1..MAX_PLAYERS] of Word;
    Caps: array[1..MAX_PLAYERS] of Byte;
    Deaths: array[1..MAX_PLAYERS] of Word;
    Ping: array[1..MAX_PLAYERS] of Integer;
    Number: array[1..MAX_PLAYERS] of Byte;
    IP: array[1..MAX_PLAYERS, 1..4] of Byte;
    X: array[1..MAX_PLAYERS] of Single;
    Y: array[1..MAX_PLAYERS] of Single;
    RedFlagX: Single;
    RedFlagY: Single;
    BlueFlagX: Single;
    BlueFlagY: Single;
    TeamScore: array[1..4] of Word;
    MapName: string[16];
    TimeLimit, CurrentTime: Integer;
    KillLimit: Word;
    GameStyle: Byte;
    MaxPlayers: Byte;
    MaxSpectators: Byte;
    Passworded: Byte;
    NextMap: string[16];
  end;

  procedure BroadcastMsg(Text: string);
  procedure SendMessageToAdmin(ToIP, Text: string);

var
  FServerConnect: TIdServerThreadEvent;
  FServerDisconnect: TIdServerThreadEvent;
  FServerExecute: TIdServerThreadEvent;

implementation

uses
  {$IFDEF STEAM}SteamTypes,{$ENDIF}
  {$IFDEF SCRIPT}ScriptCore3, ScriptDispatcher,{$ENDIF}
  Sprites, Server, Util, TraceLog, Command, Game, ServerHelper;

constructor TAdminServer.Create(Pass: string; ConnectMessage: string = '');
var
  Binding1: TIdSocketHandle;
begin
  inherited Create(nil);
  FPassword := Pass;
  FAdmins := TThreadList.Create;
  FConnectMessage := ConnectMessage;
  FBytesSent := 0;
  FBytesRecieved := 0;
  FMessageQueue := TQueue.Create;
  FQueueLock := TCriticalSection.Create;
  onConnect := self.handleConnect;
  onDisconnect := self.handleDisconnect;
  onExecute := self.handleExecute;
  DefaultPort := net_port.Value;
  MaxConnections := net_maxadminconnections.Value;
  Bindings.Clear;
  Binding1 := Bindings.Add;
  Binding1.Port := net_port.Value;
  if net_adminip.Value <> '' then
    Binding1.IP := net_adminip.Value
  else
    Binding1.IP := net_ip.Value;
  active := True;
end;

destructor TAdminServer.Destroy;
begin
  active := False;
  FreeAndNil(FAdmins);
  FMessageQueue.Destroy;
  FQueueLock.Destroy;
  inherited Destroy;
end;

procedure TAdminServer.SendToAll(Message: string);
var
  i: Integer;
begin
  try
    try
      with FAdmins.LockList do
        for i := 0 to Count - 1 do
          with TIdContext(Items[i]).Connection do
            if Connected then
            begin
              IOHandler.WriteLn(Message, IndyTextEncoding_ASCII(), IndyTextEncoding_ASCII());
              Inc(FBytesSent, Length(Message));
            end;
    finally
      FAdmins.UnlockList
    end;
  except
  end;
end;

procedure TAdminServer.SendToIP(IP: string; Message: string);
var
  i: Integer;
begin
  try
    try
      with FAdmins.LockList do
        for i := 0 to Count - 1 do
          with TIdContext(Items[i]).Connection do
            if Connected and
              (TIdIOHandlerSocket(IOHandler).Binding.PeerIP = IP) then
            begin
              IOHandler.WriteLn(Message, IndyTextEncoding_ASCII(), IndyTextEncoding_ASCII());
              Inc(FBytesSent, Length(Message));
            end;
    finally
      FAdmins.UnlockList
    end;
  except
  end;
end;

procedure TAdminServer.ProcessCommands();
var
  Msg: ^TAdminMessage;
  IOHandlerSocket: TIdIOHandlerSocket;
  RefreshMsgX: TMsg_RefreshX;
  i, j: Integer;
  n, c: Integer;
  Ip, Ips: string;
  s: array[1..4] of string;
begin
  FQueueLock.Acquire;
  if FMessageQueue.Count = 0 then begin
    FQueueLock.Release;
    Exit;
  end;
  Msg := FMessageQueue.Pop;
  FQueueLock.Release;
  try
    with FAdmins.LockList do
      for i := 0 to Count - 1 do
        with TIdContext(Items[i]).Connection do
          if Connected and
            (TIdIOHandlerSocket(IOHandler).Binding.PeerIP = Msg^.IP) and
            (TIdIOHandlerSocket(IOHandler).Binding.PeerPort = Msg^.Port) then
          begin
            IOHandlerSocket := TIdIOHandlerSocket(IOHandler);
          end;
  finally
    FAdmins.UnlockList
  end;
  if IOHandlerSocket = nil then
  begin
    Freemem(Msg);
    Exit;
  end;
  if Msg^.Msg = 'SHUTDOWN' then
  begin
    MainConsole.Console('SHUTDOWN (' + Msg^.IP + ').',
      GAME_MESSAGE_COLOR);
    ProgReady := False;
    Freemem(Msg);
    Exit;
  end
  else
    if Msg^.Msg = 'REFRESHX' then
    begin
      RefreshMsgX := Default(TMsg_RefreshX);
      for i := 1 to MAX_PLAYERS do
        if SortedPlayers[i].PlayerNum > 0 then
        begin
          RefreshMsgX.Name[i] := Sprite[SortedPlayers[i].PlayerNum].Player.Name;
          {$IFDEF STEAM}
          RefreshMsgX.hwid[i] := IntToStr(TSteamID(Sprite[SortedPlayers[i].PlayerNum].Player.SteamID).GetAccountID);
          {$ELSE}
          RefreshMsgX.hwid[i] := Sprite[SortedPlayers[i].PlayerNum].Player.hwid;
          {$ENDIF}
          RefreshMsgX.Kills[i] := Sprite[SortedPlayers[i].PlayerNum].Player.Kills;
          RefreshMsgX.Caps[i] := Sprite[SortedPlayers[i].PlayerNum].Player.Flags;
          RefreshMsgX.Deaths[i] := Sprite[SortedPlayers[i].PlayerNum].Player.Deaths;
          RefreshMsgX.Ping[i] := Sprite[SortedPlayers[i].PlayerNum].Player.PingTime;
          RefreshMsgX.Team[i] := Sprite[SortedPlayers[i].PlayerNum].Player.Team;
          RefreshMsgX.Number[i] := Sprite[SortedPlayers[i].PlayerNum].Num;
          RefreshMsgX.X[i] := iif(Sprite[SortedPlayers[i].PlayerNum].DeadMeat,
            0, Sprite[SortedPlayers[i].PlayerNum].Skeleton.Pos[1].X);
          RefreshMsgX.Y[i] := iif(Sprite[SortedPlayers[i].PlayerNum].DeadMeat,
            0, Sprite[SortedPlayers[i].PlayerNum].Skeleton.Pos[1].Y);

          if Sprite[SortedPlayers[i].PlayerNum].Player.ControlMethod = HUMAN then
          begin
            n := 1;
            c := 0;
            Ips := Sprite[SortedPlayers[i].PlayerNum].Player.IP;
            for j := 1 to Length(Ips) do
            begin
              Inc(c);
              if Ips[j] = '.' then
              begin
                c := 0;
                Inc(n);
              end else
              begin
                SetLength(s[n], c);
                s[n][c] := Ips[j];
              end;
            end;

            for j := 1 to 4 do
              RefreshMsgX.IP[i][j] := StrToInt(s[j]);
          end else
            for j := 1 to 4 do
              RefreshMsgX.IP[i][j] := 0;
        end
        else
          RefreshMsgX.Team[i] := 255;

      RefreshMsgX.Header := 'REFRESHX' + #13#10;
      RefreshMsgX.TeamScore[TEAM_ALPHA] := TeamScore[TEAM_ALPHA];
      RefreshMsgX.TeamScore[TEAM_BRAVO] := TeamScore[TEAM_BRAVO];
      RefreshMsgX.TeamScore[TEAM_CHARLIE] := TeamScore[TEAM_CHARLIE];
      RefreshMsgX.TeamScore[TEAM_DELTA] := TeamScore[TEAM_DELTA];

      if TeamFlag[TEAM_ALPHA] > 0 then
      begin
        RefreshMsgX.RedFlagX := Thing[TeamFlag[TEAM_ALPHA]].Skeleton.Pos[1].X;
        RefreshMsgX.RedFlagY := Thing[TeamFlag[TEAM_ALPHA]].Skeleton.Pos[1].Y;
      end;

      if TeamFlag[TEAM_BRAVO] > 0 then
      begin
        RefreshMsgX.BlueFlagX := Thing[TeamFlag[TEAM_BRAVO]].Skeleton.Pos[1].X;
        RefreshMsgX.BlueFlagY := Thing[TeamFlag[TEAM_BRAVO]].Skeleton.Pos[1].Y;
      end;

      RefreshMsgX.MapName := Map.Name;
      RefreshMsgX.TimeLimit := sv_timelimit.Value;
      RefreshMsgX.CurrentTime := TimeLimitCounter;
      RefreshMsgX.KillLimit := sv_killlimit.Value;
      RefreshMsgX.GameStyle := sv_gamemode.Value;
      RefreshMsgX.MaxPlayers := sv_maxplayers.Value;
      RefreshMsgX.MaxSpectators := sv_maxspectators.Value;
      RefreshMsgX.Passworded := iif(sv_password.Value <> '', 1, 0);
      RefreshMsgX.NextMap := CheckNextMap;

      IOHandlerSocket.Write(RawToBytes(RefreshMsgX, SizeOf(RefreshMsgX)));
      Freemem(Msg);
      Exit;
    end else
    begin
      Ip := Msg^.IP;
      MainConsole.Console(Msg^.Msg + ' (' + Ip + ')', GAME_MESSAGE_COLOR);
      {$IFDEF SCRIPT}
      ScrptDispatcher.OnAdminMessage(
        Msg^.IP,
        Msg^.Port,
        Msg^.Msg);
      {$ENDIF}
      if (Msg^.Msg[1] = '/') then
        {$IFDEF SCRIPT}
        if not ScrptDispatcher.OnConsoleCommand(
          Msg^.IP,
          Msg^.Port,
          Msg^.Msg) then
        {$ENDIF}
        begin
          Delete(Msg^.Msg, 1, 1);
          ParseInput(Msg^.Msg, 255);
        end;
      Freemem(Msg);
    end;

end;

function FindAdminFloodID(SrcIP: string): Cardinal;
var
  i: Integer;
const
  FLOOD_ID_NOT_FOUND = 0;
begin
  Result := FLOOD_ID_NOT_FOUND;
  for i := 1 to MAX_ADMIN_FLOOD_IPS do
    if AdminFloodIP[i] = SrcIP then
    begin
      Result := i;
      Break;
    end;
end;

function AddAdminFloodIP(SrcIP: string): Cardinal;
var
  i: Cardinal;
const
  FLOOD_ID_NOT_FOUND = 0;
begin
  Result := FLOOD_ID_NOT_FOUND;
  for i := 1 to MAX_ADMIN_FLOOD_IPS do
    if AdminFloodIP[i] = '' then
    begin
      AdminFloodIP[i] := SrcIP;
      Result := i;
      Break;
    end;
end;

function UpdateAntiAdminFlood(IP: string): Cardinal;
var
  i: Cardinal;
begin
  // update last requested admin ip array
  LastAdminIPs[AdminIPCounter] := IP;
  AdminIPCounter := (AdminIPCounter + 1) mod MAX_LAST_ADMIN_IPS + 1;

  // check for flood
  for i := 0 to MAX_LAST_ADMIN_IPS do
  begin
    if LastAdminIPs[i] <> LastAdminIPs[(i + 1) mod
      MAX_LAST_ADMIN_IPS + 1] then
      Break;
  end;

  // if is flood then ban
  if i = MAX_LAST_ADMIN_IPS + 1 then
  begin
    i := AddAdminFloodIP(IP);
    if i = 0 then
      WriteLn('failed adding ip to banlst');
    Result := 1;
  end else
  begin
    Result := 0;
  end;
end;

procedure TAdminServer.handleConnect(AThread: TIdContext);
var
  Msg: string;
  Ip: string;
begin
  with AThread.Connection do
  try
    Ip := TIdIOHandlerSocket(IOHandler).Binding.PeerIP;

    // if Admin IP is banned then drop
    if FindAdminFloodID(Ip) <> 0 then
    begin
      AThread.Connection.Disconnect;
      FAdmins.Remove(AThread);
      Exit;
    end;

    if UpdateAntiAdminFlood(Ip) > 0 then
      Exit;

    IOHandler.WriteLn('Soldat Admin Connection Established.');

    Msg := IOHandler.ReadLn(LF, 5000, -1);

    Inc(FBytesRecieved, Length(Msg));
    if Msg <> FPassword then
    begin
      if Msg = '' then
        IOHandler.WriteLn('Password request timed out.')
      else
        IOHandler.WriteLn('Invalid password.');

      MainConsole.Console('Admin failed to connect (' + Ip + ').',
        GAME_MESSAGE_COLOR);
      Disconnect;
    end else
    begin
      MainConsole.Console('Admin connected (' + Ip + ').', GAME_MESSAGE_COLOR);
      FAdmins.Add(AThread);
      IOHandler.WriteLn('Welcome, you are in command of the server now.');
      IOHandler.WriteLn('List of commands available in the Soldat game Manual.');
      IOHandler.WriteLn('Server Version: ' + DEDVERSION);

      {$IFDEF SCRIPT}
      ScrptDispatcher.OnAdminConnect(
        TIdIOHandlerSocket(IOHandler).Binding.PeerIP,
        TIdIOHandlerSocket(IOHandler).Binding.PeerPort);
      {$ENDIF}
    end;
  except
    on e: Exception do
    begin
      try
        IOHandler.WriteLn(e.Message);
      except
      end;
      Disconnect;
    end;
  end;
end;

procedure TAdminServer.handleDisconnect(AThread: TIdContext);
var
  Ip: string;
begin
  // TODO: test if still cause access violations
  if (AThread = nil) then
  begin
    FAdmins.Remove(AThread);
    Exit;
  end
  else if(AThread.Connection = nil) or (AThread.Connection.IOHandler = nil) then
  begin
    AThread.Connection.Disconnect;
    FAdmins.Remove(AThread);
    Exit;
  end;

  try
    Ip := TIdIOHandlerSocket(AThread.Connection.IOHandler).Binding.PeerIP;

    // if Admin IP is banned then drop
    if FindAdminFloodID(Ip) <> 0 then
    begin
      AThread.Connection.Disconnect;
      FAdmins.Remove(AThread);
      Exit;
    end;

    if UpdateAntiAdminFlood(Ip) > 0 then
      Exit;
  except
  end;
  try
    AThread.Connection.Disconnect;
  except
  end;
  try
    FAdmins.Remove(AThread);
  except
  end;
  try
    MainConsole.Console('Admin disconnected (' + Ip + ').', GAME_MESSAGE_COLOR);
  except
  end;
  {$IFDEF SCRIPT}
  ScrptDispatcher.OnAdminDisconnect(
    TIdIOHandlerSocket(AThread.Connection.IOHandler).Binding.PeerIP,
    TIdIOHandlerSocket(AThread.Connection.IOHandler).Binding.PeerPort);
  {$ENDIF}
end;

procedure TAdminServer.handleExecute(AThread: TIdContext);
var
  Msg, IP: string;
  Port: TIdPort;
  MsgRecord: ^TAdminMessage;
begin
  with AThread.Connection do
  try
    IP := TIdIOHandlerSocket(IOHandler).Binding.PeerIP;
    Port := TIdIOHandlerSocket(IOHandler).Binding.PeerPort;
    // if Admin IP is banned then drop
    if FindAdminFloodID(IP) <> 0 then
    begin
      AThread.Connection.Disconnect;
      FAdmins.Remove(AThread);
      Exit;
    end;

    try
      if FAdmins.LockList.IndexOf(AThread) = -1 then
        Exit;
    finally
      FAdmins.UnlockList;
    end;

    Msg := IOHandler.ReadLn(LF, 1000, -1, IndyTextEncoding_ASCII(), IndyTextEncoding_ASCII());
    if Msg <> '' then
    begin
      Inc(FBytesRecieved, Length(msg));
      New(MsgRecord);
      MsgRecord^.Msg := Msg;
      MsgRecord^.IP := IP;
      MsgRecord^.Port := Port;

      FQueueLock.Acquire;
      FMessageQueue.Push(MsgRecord);
      FQueueLock.Release;
    end;
  except
    on e: Exception do
    begin
      try
        IOHandler.WriteLn(e.Message);
      except
      end;
    end;
  end;
end;

procedure BroadcastMsg(Text: string);
begin
  Trace('BroadcastMsg');
  if Assigned(sv_adminpassword) then
    if sv_adminpassword.Value = '' then
      Exit;
  if (AdminServer = nil) then
    Exit;

  AdminServer.SendToAll(Text);
end;

procedure SendMessageToAdmin(ToIP, Text: string);
begin
  if sv_adminpassword.Value = '' then
    Exit;
  if (AdminServer = nil) then
    Exit;

  AdminServer.SendToIP(ToIP, Text);
end;

end.

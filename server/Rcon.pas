unit Rcon;

interface

uses
  Classes, SysUtils, Sockets, Ssockets, Constants, Net;

const
  REFRESHX_HEADER_CHARS = 10;
  RCON_AUTH_TIMEOUT = 5000;  // Authentication timeout (ms)

type
  TAdminServerConnectionThread = class;

  TAdminMessage = record
    Msg: String;
    Socket: TSocketStream;
  end;

  TAdminServer = class(TThread)
  private
    FPassword: string;
    FAdmins: TThreadList;
    FMessageQueue: TThreadList;
    FServer: TInetServer;
    FMaxConnections: Integer;
    FBucketRate: LongWord;
    FBucketBurst: LongWord;
    FBucketTokens: LongWord;
    FBucketLastFill: QWord;
  public
    property Admins: TThreadList read FAdmins;
    property Password: string read FPassword write FPassword;
    constructor Create(Pass: string); reintroduce;
    destructor Destroy; override;
    procedure SendToAll(Message: string);
    procedure SendToIP(IP: string; Message: string);
    procedure ProcessCommands();
    procedure Execute; override;
    procedure OnConnectQuery(Sender: TObject; ASocket: Longint; var Allow: Boolean);
    procedure OnConnect(Sender: TObject; Data: TSocketStream);
    procedure OnAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
  end;

  TAdminServerConnectionThread = class(TThread)
  private
    FPassword: string;
    FData: TSocketStream;
    FAdminServer: TAdminServer;
    FMessageQueue: TThreadList;
    FAdminsList: TThreadList;
    FAuthed: Boolean;
    FConnectTime: QWord;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: TSocketStream; AdminServer: TAdminServer; MessageQueue: TThreadList; AdminsList: TThreadList; Password: String);
    procedure ShutdownSocket();
    procedure WriteString(AStr: string);
    destructor Destroy; override;
  end;

  TSockAddrHelper = record helper for TSockAddr
    function GetIPString(): String;
    function GetAddressString(): String;
    function GetPort(): Word;
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

implementation

uses
  {$IFDEF STEAM}Steam,{$ENDIF}
  {$IFDEF SCRIPT}ScriptDispatcher,{$ENDIF}
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Server, Util, TraceLog, Command, Game, ServerHelper, Cvar, Version, Math;

{$PUSH}
{$WARN 5024 OFF}  // Parameter "$1" not used
function TSockAddrHelper.GetIPString(): String;
begin
  Result := NetAddrToStr(Self.sin_addr);
end;

function TSockAddrHelper.GetAddressString(): String;
begin
  Result := Format('%s:%d', [NetAddrToStr(Self.sin_addr), Self.sin_port]);
end;

function TSockAddrHelper.GetPort(): Word;
begin
  Result := Self.sin_port;
end;

constructor TAdminServer.Create(Pass: string);
begin
  inherited Create(False);

  FPassword := Pass;
  FAdmins := TThreadList.Create;
  FMessageQueue := TThreadList.Create;
  FMaxConnections := net_maxadminconnections.Value;
  FServer := TINetServer.Create(net_adminip.Value, net_port.Value);

  FServer.SetNonBlocking;
  FServer.OnConnectQuery := OnConnectQuery;
  FServer.OnConnect:= OnConnect;
  FServer.OnAcceptError := OnAcceptError;
  FServer.QueueSize := 5;
  FServer.KeepAlive := True;
  FServer.ReuseAddress := True;
  FServer.AcceptIdleTimeOut := 5;

  FBucketBurst := net_rcon_limit.Value;
  FBucketRate := net_rcon_burst.Value;
  FBucketTokens := 0;
  FBucketLastFill := 0;

  NameThreadForDebugging('AdminServer');
end;

destructor TAdminServer.Destroy;
var
  Connection: TAdminServerConnectionThread;
begin
  FServer.AcceptIdleTimeOut := 0;
  FPShutdown(FServer.Socket, SHUT_RDWR);
  CloseSocket(FServer.Socket);
  FServer.StopAccepting(True);

  try
    try
      for Connection in FAdmins.LockList do
        begin
          Connection.ShutdownSocket;
          Connection.Terminate;
        end;
    finally
      FAdmins.UnlockList;
    end;
  except
  end;

  while True do
  begin
    Sleep(25);
    if FAdmins.LockList.Count = 0 then
      Break;
    FAdmins.UnlockList;
  end;

  FAdmins.Free;
  FMessageQueue.Free;

  inherited Destroy;
end;

procedure TAdminServer.Execute;
begin
  try
    try
      FServer.StartAccepting;
    except
    end;
  finally
    FServer.Free;
  end;
end;

procedure TAdminServer.OnConnectQuery(Sender: TObject; ASocket: Longint; var Allow: Boolean);
var
  CurrentTime: Integer;
  Tokens: Integer;
begin
  CurrentTime := Trunc(GetTickCount64 / 1000);
  if CurrentTime > FBucketLastFill then
  begin
    Tokens := FBucketTokens + FBucketRate * (CurrentTime - FBucketLastFill);
    FBucketTokens := Min(Tokens, FBucketBurst);
    FBucketLastFill := CurrentTime;
  end;

  if FBucketTokens < 1 then
  begin
    Debug('[RCON] New connection rejected: Rate limit exceeded');
    Allow := False;
    Exit;
  end;

  Dec(FBucketTokens, 1);

  if FAdmins.LockList.Count < FMaxConnections then
    Allow := True
  else
  begin
    Debug('[RCON] New connection rejected: Maximum number of connections reached');
    Allow := False;
  end;

  FAdmins.UnlockList;
end;

procedure TAdminServer.OnConnect(Sender: TObject; Data: TSocketStream);
var
  AConnectionThread: TAdminServerConnectionThread;
begin
  Debug('[RCON] New connection from: ' + Data.RemoteAddress.GetAddressString);

  AConnectionThread := TAdminServerConnectionThread.Create(Data, Self, FMessageQueue, FAdmins, FPassword);
  AConnectionThread.NameThreadForDebugging(Data.RemoteAddress.GetIPString);
  AConnectionThread.FreeOnTerminate := True;

  FAdmins.Add(AConnectionThread);
end;

procedure TAdminServer.OnAcceptError(Sender: TObject; ASocket: Longint; E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  if Terminated then
    ErrorAction := aeaRaise;
end;

procedure TAdminServer.SendToAll(Message: string);
var
  Connection: TAdminServerConnectionThread;
begin
  try
    try
      for Connection in FAdmins.LockList do
        if Connection.FAuthed then
          Connection.WriteString(Message);
    finally
      FAdmins.UnlockList
    end;
  except
  end;
end;

procedure TAdminServer.SendToIP(IP: string; Message: string);
var
  Connection: TAdminServerConnectionThread;
begin
  try
    try
      for Connection in FAdmins.LockList do
        begin
          if Connection.FAuthed and (Connection.FData.RemoteAddress.GetIPString = IP) then
            Connection.WriteString(Message);
        end;
    finally
      FAdmins.UnlockList
    end;
  except
  end;
end;

{$WARN 5027 OFF}
procedure TAdminServer.ProcessCommands();
var
  Msg: ^TAdminMessage;
  RefreshMsgX: TMsg_RefreshX;
  i: Integer;
  Ip: string;
  MessageQueue: TList;
begin
  MessageQueue := FMessageQueue.LockList;
  Msg := MessageQueue.First;

  if Msg = nil then
  begin
    FMessageQueue.UnlockList;
    Exit;
  end;

  if Msg^.Msg = 'SHUTDOWN' then
  begin
    MainConsole.Console('[RCON] SHUTDOWN (' + Msg^.Socket.RemoteAddress.GetIPString + ').',
      GAME_MESSAGE_COLOR);
    ProgReady := False;
    Freemem(Msg);
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
          RefreshMsgX.hwid[i] := IntToStr(CSteamID(Sprite[SortedPlayers[i].PlayerNum].Player.SteamID).m_unAccountID);
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
            in_addr(RefreshMsgX.IP[i]) := StrToNetAddr(Sprite[SortedPlayers[i].PlayerNum].Player.IP);
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

      Msg^.Socket.Write(RefreshMsgX, SizeOf(RefreshMsgX));
      Freemem(Msg);
    end else
    begin
      Ip := Msg^.Socket.RemoteAddress.GetIPString;
      MainConsole.Console(Msg^.Msg + ' (' + Ip + ')', GAME_MESSAGE_COLOR);
      {$IFDEF SCRIPT}
      ScrptDispatcher.OnAdminMessage(
        Ip,
        Msg^.Socket.RemoteAddress.GetPort,
        Msg^.Msg);
      {$ENDIF}
      if Msg^.Msg.StartsWith('/') then
        {$IFDEF SCRIPT}
        if not ScrptDispatcher.OnConsoleCommand(
          Ip,
          Msg^.Socket.RemoteAddress.GetPort,
          Msg^.Msg) then
        {$ENDIF}
        begin
          Delete(Msg^.Msg, 1, 1);
          ParseInput(Msg^.Msg, 255);
        end;
      Freemem(Msg);
    end;
  MessageQueue.Remove(Msg);
  FMessageQueue.UnlockList;
end;
{$POP}

constructor TAdminServerConnectionThread.Create(Data: TSocketStream; AdminServer: TAdminServer; MessageQueue: TThreadList; AdminsList: TThreadList; Password: String);
begin
  FData := data;
  FAdminServer := AdminServer;
  FMessageQueue := MessageQueue;
  FPassword := Password;
  FAdminsList := AdminsList;
  FConnectTime := GetTickCount64;

  // Sets inactivity timeout during authentication phase
  Data.IOTimeout := RCON_AUTH_TIMEOUT;

  inherited Create(False);
end;

procedure TAdminServerConnectionThread.ShutdownSocket();
begin
  fpshutdown(FData.Handle, SHUT_RDWR);
end;

procedure TAdminServerConnectionThread.WriteString(AStr: String);
var
  Message: String = '';
begin
  Message := AStr + #13#10;
  if FData.Write(Message[1], Length(Message)) < 0 then
    Debug('[RCON] Error during sending message: ' + IntToStr(FData.LastError));
end;

function IsRealSocketError(ErrCode: Integer): Boolean;
begin
  {$IFDEF UNIX}
  Result := (ErrCode <> ESockEINTR) and (ErrCode <> ESysEAGAIN);
  {$ELSE}
  Result := ErrCode <> ESockEINTR;
  {$ENDIF}
end;

procedure TAdminServerConnectionThread.Execute;
type
  TBuffer = array[0..1023] of Char;
var
  MsgRecord: ^TAdminMessage;
  Message: String = '';
  i: Integer;
  InputBuffer: TBuffer;
  InputStr: String = '';
begin
  InputBuffer := Default(TBuffer);

  if not Terminated then
    WriteString('OpenSoldat Admin Connection Established.');

  while not Terminated do
  begin
    if not FAuthed then
      if (GetTickCount64 - FConnectTime) >= RCON_AUTH_TIMEOUT then
      begin
        Debug('[RCON] Password request timed out (' + FData.RemoteAddress.GetIPString + ').');
        WriteString('Password request timed out.');
        Terminate;
        Break;
      end;

    i := FData.Read(InputBuffer[0], 1024);
    if i > 0 then
    begin
      SetLength(Message, i);
      Move(InputBuffer[0], Message[1], i);
      InputStr := InputStr + Message.Replace(#13#10, #10, [rfReplaceAll]);
      i := Pos(#10, InputStr);
      while i > 0 do
      begin
        Message := Copy(InputStr, 1, i - 1);
        Delete(InputStr, 1, i);
        if FAuthed then
        begin
          if not Message.IsEmpty then
          begin
            New(MsgRecord);
            MsgRecord^.Msg := Message;
            MsgRecord^.Socket := FData;
            FMessageQueue.Add(MsgRecord);
          end;
        end else
        begin
          if Message = FPassword then
          begin
            FAuthed := True;
            FData.IOTimeout := 0;

            // TODO: Restore `TConsole.Console` call after it's made thread safe.
            //MainConsole.Console('[RCON] Admin connected (' + FData.RemoteAddress.GetIPString + ').', GAME_MESSAGE_COLOR);
            Debug('[RCON] Admin connected (' + FData.RemoteAddress.GetIPString + ').');
            WriteString('Welcome, you are in command of the server now.');
            WriteString('List of commands available in the OpenSoldat game Manual.');
            WriteString('Server Version: ' + DEDVERSION);

            {$IFDEF SCRIPT}
            ScrptDispatcher.OnAdminConnect(
              FData.RemoteAddress.GetIPString,
              FData.RemoteAddress.GetPort);
            {$ENDIF}
          end else
          begin
            WriteString('Invalid password.');
            WriteLn('[RCON] Invalid password from: ' + FData.RemoteAddress.GetIPString);
            Terminate;
          end;
        end;
        i := Pos(#10, InputStr);
        end;
      end
    else if i <= 0 then
      if IsRealSocketError(FData.LastError) then
      begin
        Debug('[RCON] Socket read failed, errno: ' + IntToStr(FData.LastError) + ' for ' + FData.RemoteAddress.GetIPString);
        Terminate;
      end;
  end;
  if FAuthed then
  begin
    // TODO: Restore `TConsole.Console` call after it's made thread safe.
    //MainConsole.Console('[RCON] Admin disconnected (' + FData.RemoteAddress.GetIPString + ').', GAME_MESSAGE_COLOR);
    Debug('[RCON] Admin disconnected (' + FData.RemoteAddress.GetIPString + ').');
    {$IFDEF SCRIPT}
    ScrptDispatcher.OnAdminDisconnect(
      FData.RemoteAddress.GetIPString,
      FData.RemoteAddress.GetPort);
    {$ENDIF}
  end;
end;

destructor TAdminServerConnectionThread.Destroy;
begin
  Debug('[RCON] Closing connection thread');
  FAdminsList.Remove(Self);
  FData.Free;
  inherited Destroy;
end;

procedure BroadcastMsg(Text: string);
begin
  Trace('BroadcastMsg');
  if AdminServer = nil then
    Exit;

  AdminServer.SendToAll(Text);
end;

procedure SendMessageToAdmin(ToIP, Text: string);
begin
  if AdminServer = nil then
    Exit;

  AdminServer.SendToIP(ToIP, Text);
end;

end.

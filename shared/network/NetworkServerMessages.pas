unit NetworkServerMessages;

interface

uses
  // delphi and system units
  SysUtils, Classes,

  // helper units
  Util,

  {$IFDEF SCRIPT}
  ScriptDispatcher,
  {$ENDIF}

  // opensoldat units
  Net, Steam, Sprites, Command, Constants;

  procedure ServerSendStringMessage(Text: WideString; ToNum: Byte; From: Byte; MsgType: Byte);
  procedure ServerHandleChatMessage(NetMessage: PSteamNetworkingMessage_t);
  procedure ServerSendSpecialMessage(Text: string; MsgType: Byte;
    LayerId: Byte; Delay: Integer; Scale: Single; Color: UInt32;
    X: Single; Y: Single; ToNum: Byte);

implementation

uses
  Server, Game, Demo;

procedure ServerSendStringMessage(Text: WideString; ToNum: Byte; From: Byte; MsgType: Byte);
var
  PChatMessage: PMsg_StringMessage;
  i: Integer;
  Size: Integer;
begin
  if Length(Text) = 0 then
    Exit;
  PChatMessage := Default(PMsg_StringMessage);
  Size := SizeOf(PChatMessage^.Header) + SizeOf(PChatMessage^.Num) +
    SizeOf(PChatMessage^.MsgType) + 2 * Length(Text) + 1 + 1;
  GetMem(PChatMessage, Size);

  PChatMessage^.Header.ID := MsgID_ChatMessage;
  PChatMessage^.Num := From;
  PChatMessage^.MsgType := MsgType;

  Move(Text[1], PChatMessage.Text, 2 * Length(Text) + 2);

  if ((From > 0) and (From < MAX_PLAYERS + 1)) or (From = 255) then
  begin
    for i := 1 to MAX_PLAYERS do
      if Sprite[i].Active and (Sprite[i].Player.ControlMethod = HUMAN) then
        if (ToNum = 0) or (i = ToNum) then
        begin
        if not ((From = 255) and (ToNum = 0)) then // TODO: Simplify it.
          if (not ((MsgType = MSGTYPE_TEAM) or (MsgType = MSGTYPE_RADIO)) or (From = 255)) or
            (((MsgType = MSGTYPE_TEAM) or (MsgType = MSGTYPE_RADIO)) and Sprite[From].IsInSameTeam(Sprite[i])) then
            UDP.SendData(PChatMessage^, Size, Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable)
        end;
  end;
  FreeMem(PChatMessage);

  if (From < 1) or (From > MAX_PLAYERS) then
    Exit;

  // show text on servers side
  if Sprite[From].Player.ControlMethod = BOT then
  begin
    MainConsole.Console(iif(MsgType = MSGTYPE_TEAM, '(TEAM)', '') +
      '[' + Sprite[From].Player.Name + '] ' + Text, TEAMCHAT_MESSAGE_COLOR);
  end;
end;

procedure ServerHandleChatMessage(NetMessage: PSteamNetworkingMessage_t);
var
  cs, cschat: WideString;
  MsgType: Byte;
  Player: TPlayer;
begin
  Player := TPlayer(NetMessage^.m_nConnUserData);

  if Player.SpriteNum = 0 then
    Exit;

  Inc(MessagesASecNum[Player.SpriteNum]);

  cs := PWideChar(@PMsg_StringMessage(NetMessage^.m_pData)^.Text);
  MsgType := PMsg_StringMessage(NetMessage^.m_pData)^.MsgType;

  if MsgType > MSGTYPE_RADIO then
    Exit;

  if Length(cs) > 100 then
  begin
    // Fixed DoS Exploit that causes crash on clients.
    KickPlayer(Player.SpriteNum, True, KICK_FLOODING, TWENTY_MINUTES, 'DoS Exploit');
    Exit;
  end;

  Inc(Player.ChatWarnings);

  if MsgType = MSGTYPE_RADIO then
    cschat := Copy(cs, 4, Length(cs))
  else
    cschat := cs;

  // command
  if MsgType = MSGTYPE_CMD then
  begin
    {$IFDEF SCRIPT}
    if ScrptDispatcher.OnPlayerCommand(Player.SpriteNum, AnsiString(cs)) then
      Exit;
    {$ENDIF}
    MainConsole.Console(cs + '(' + WideString(Player.IP) +
      '[' + WideString(Player.Name) + ']' + ')',
      DEFAULT_MESSAGE_COLOR);
    ParseInput(AnsiString(cs), Player.SpriteNum);
    Exit;
  end;

  cschat := '[' + WideString(Player.Name) + '] ' + cschat;

  if MsgType = MSGTYPE_TEAM then
    cschat := '(TEAM) ' + cschat;
  if MsgType = MSGTYPE_RADIO then
    cschat := '(RADIO) ' + cschat;
  if Player.Muted = 1 then
    cschat := '(MUTED) ' + cschat;

  MainConsole.Console(cschat, CHAT_MESSAGE_COLOR);

  if Player.Muted = 1 then
    ServerSendStringMessage('(Muted)', ALL_PLAYERS, Player.SpriteNum, MSGTYPE_PUB)
  else
    ServerSendStringMessage(cs, ALL_PLAYERS, Player.SpriteNum, MsgType);

  if Player.Muted = 1 then
    Exit;  // cs := '(Muted)';

  // TODO(pewpew): MSGTYPE_RADIO prefix is never true, figure out why this is here, if it did something in old scriptcore
  if MsgType = MSGTYPE_TEAM then
    cs := iif(MsgType = MSGTYPE_RADIO, '*', '^') + cs;

  {$IFDEF SCRIPT}
  ScrptDispatcher.OnPlayerSpeak(Player.SpriteNum, AnsiString(cs));
  {$ENDIF}
end;

procedure ServerSendSpecialMessage(Text: string; MsgType: Byte;
  LayerId: Byte; Delay: Integer; Scale: Single; Color: UInt32;
  X: Single; Y: Single; ToNum: Byte);
var
  PChatMessage: PMsg_ServerSpecialMessage;
  Size: Integer;
  i: Integer;
begin
  Size := SizeOf(TMsg_ServerSpecialMessage) + Length(Text) + 1;
  GetMem(PChatMessage, Size);

  PChatMessage^.Header.ID := MsgID_SpecialMessage;
  PChatMessage^.MsgType := MsgType;
  PChatMessage^.LayerId := LayerId;
  PChatMessage^.Delay := Delay;
  PChatMessage^.Scale := Scale;
  PChatMessage^.Color := Color;
  PChatMessage^.X := X;
  PChatMessage^.Y := Y;

  StrPCopy(@PChatMessage^.Text, PChar(Text));

  for i := 1 to MAX_PLAYERS do
    if Sprite[i].Active and (Sprite[i].Player.ControlMethod = HUMAN) then
      if (ToNum = 0) or (i = ToNum) then
        UDP.SendData(PChatMessage^, size, Sprite[i].Player.peer, k_nSteamNetworkingSend_Reliable);

  FreeMem(PChatMessage);
end;

end.

unit NetworkClientMessages;

interface

uses

  // delphi and system units
  SysUtils, Classes,

  // helper units
  Vector, Util,

  // opensoldat units
  Steam, Net, Sprites, Constants, GameStrings;

procedure ClientSendStringMessage(Text: WideString; MsgType: Byte);
procedure ClientHandleChatMessage(NetMessage: PSteamNetworkingMessage_t);
procedure ClientHandleSpecialMessage(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  Client, Game, InterfaceGraphics, NetworkUtils;

procedure ClientSendStringMessage(Text: WideString; MsgType: Byte);
var
  PChatMessage: PMsg_StringMessage;
  Size: Integer;
begin
  PChatMessage := Default(PMsg_StringMessage);

  if Length(Text) = 0 then
    Exit;

  Size := SizeOf(PChatMessage^.Header) + SizeOf(PChatMessage^.Num) + SizeOf(PChatMessage^.MsgType) + 2 * Length(Text) + 2;
  GetMem(PChatMessage, size);
  try
    FillChar(PChatMessage^, size, 0);
    PChatMessage^.Header.ID := MsgID_ChatMessage;
    PChatMessage^.Num := MySprite;
    PChatMessage^.MsgType := MsgType;

    Move(Text[1], PChatMessage^.Text, 2 * Length(Text) + 2);

    UDP.SendData(PChatMessage^, Size, k_nSteamNetworkingSend_Reliable);
  finally
    FreeMem(PChatMessage);
  end;
end;

procedure ClientHandleChatMessage(NetMessage: PSteamNetworkingMessage_t);
var
  cs: WideString = '';
  prefix: WideString = '';
  RadioCommand: WideString;
  i, d: Integer;
  MsgType: Byte;
  col: Cardinal;
begin
  cs := PWideChar(@PMsg_StringMessage(NetMessage^.m_pData)^.Text);
  i := PMsg_StringMessage(NetMessage^.m_pData)^.Num;
  MsgType := PMsg_StringMessage(NetMessage^.m_pData)^.MsgType;

  if MsgType > MSGTYPE_RADIO then
    Exit;

  if MsgType = MSGTYPE_RADIO then
  begin
    RadioCommand := Copy(cs, 2, 2);
    Delete(cs, 1, 3);
  end;

  // chat from server
  if i = 255 then
  begin
    MainConsole.Console(_('*SERVER*: ') + cs, SERVER_MESSAGE_COLOR);
    Exit;
  end;

  if (i > 0) and (i < MAX_PLAYERS) then
    if not Sprite[i].Active then
      Exit;

  if (Sprite[i].Muted = True) or MuteAll then
    Exit;

  ChatMessage[i] := cs;
  ChatTeam[i] := (MsgType = MSGTYPE_TEAM);
  d := String(cs).CountChar(' ');

  if d = 0 then
    ChatDelay[i] := Length(cs) * CHARDELAY
  else
    ChatDelay[i] := d * SPACECHARDELAY;

  if ChatDelay[i] > MAX_CHATDELAY then
    ChatDelay[i] := MAX_CHATDELAY;

  col := CHAT_MESSAGE_COLOR;

  if Sprite[i].Player.Team = TEAM_SPECTATOR then
    col := SPECTATOR_C_MESSAGE_COLOR;
  if (MsgType = MSGTYPE_TEAM) or (MsgType = MSGTYPE_RADIO) then
  begin
    col := TEAMCHAT_MESSAGE_COLOR;
    prefix := iif(MsgType = MSGTYPE_RADIO, '(RADIO)', _('(TEAM)')) + ' ';
  end;

  if Length(cs) < MORECHATTEXT then
    MainConsole.Console(prefix + '[' + WideString(Sprite[i].Player.Name) + '] ' + cs, col)
  else
  begin
    MainConsole.Console(prefix + '[' + WideString(Sprite[i].Player.Name) + '] ', col);
    MainConsole.Console(' ' + cs, col);
  end;

  if (MsgType = MSGTYPE_RADIO) and Sprite[i].IsInSameTeam(Sprite[MySprite]) then
    PlayRadioSound(StrToIntDef(AnsiString(RadioCommand), -1));
end;

procedure ClientHandleSpecialMessage(NetMessage: PSteamNetworkingMessage_t);
var
  SpecialMessage: TMsg_ServerSpecialMessage;
  cs: WideString;
begin
  SpecialMessage := PMsg_ServerSpecialMessage(NetMessage^.m_pData)^;
  cs := PChar(@PMsg_ServerSpecialMessage(NetMessage^.m_pData)^.Text);

  if (SpecialMessage.MsgType = 0) then // console
  begin
    MainConsole.Console(cs, SpecialMessage.Color);
  end
  else if (SpecialMessage.MsgType = 1) then // big text
  begin
    BigText[SpecialMessage.LayerId] := cs;
    BigDelay[SpecialMessage.LayerId] := SpecialMessage.Delay;
    BigScale[SpecialMessage.LayerId] := SpecialMessage.Scale;
    BigColor[SpecialMessage.LayerId] := SpecialMessage.Color;
    BigPosX[SpecialMessage.LayerId] := SpecialMessage.X * _RScala.x;
    BigPosY[SpecialMessage.LayerId] := SpecialMessage.Y * _RScala.y;
    BigX[SpecialMessage.LayerId] := 100;
  end
  else // world text
  begin
    WorldText[SpecialMessage.LayerId] := cs;
    WorldDelay[SpecialMessage.LayerId] := SpecialMessage.Delay;
    WorldScale[SpecialMessage.LayerId] := SpecialMessage.Scale;
    WorldColor[SpecialMessage.LayerId] := SpecialMessage.Color;
    WorldPosX[SpecialMessage.LayerId] := SpecialMessage.X * _RScala.x;
    WorldPosY[SpecialMessage.LayerId] := SpecialMessage.Y * _RScala.y;
    WorldX[SpecialMessage.LayerId] := 100;
  end;
end;

end.

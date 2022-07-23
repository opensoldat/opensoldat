unit Console;

interface

const
  CONSOLE_MAX_MESSAGES = {$IFDEF SERVER}20{$ELSE}255{$ENDIF};

type
  TConsole = object
    TextMessage: array[1..CONSOLE_MAX_MESSAGES] of WideString;
    TextMessageColor: array[1..CONSOLE_MAX_MESSAGES] of LongWord;
    NumMessage: array[1..CONSOLE_MAX_MESSAGES] of Integer;
    Count: Integer;
    CountMax: Integer;
    ScrollTick: Integer;
    ScrollTickMax: Integer;   // how long the scroll count down is before it
                              // scrolls - in ticks 60=1 sec}
    NewMessageWait: Integer;  // how long it waits after a new message before
                              // resuming the scroll count down
    AlphaCount: Byte;
    {$IFDEF SERVER}
    TerminalColors: Boolean; // use terminal colors
    {$ENDIF}
  public
    procedure ScrollConsole;
    {$IFDEF SERVER}
    procedure Console(What: Variant; Col: Cardinal; Sender: Byte); overload;
    {$ENDIF}
    procedure Console(What: WideString; Col: Cardinal); overload;
    procedure Console(What: AnsiString; Col: Cardinal); overload;
    procedure Console(What: Variant; Col: Cardinal); overload;
    procedure ConsoleAdd(What: WideString; Col: Cardinal);
    procedure ConsoleNum(What: WideString; Col: Cardinal; Num: Integer);
    function GetContentsAsPlainText(): WideString;
  end;

implementation

uses
  {$IFDEF SERVER}
  Server,
  Net,
  NetworkServerMessages,
  {$IFDEF RCON}Rcon,{$ENDIF}
  {$ELSE}
  Client,
  TraceLog,
  {$ENDIF}
  Game,
  LogFile,
  SysUtils;

procedure TConsole.ScrollConsole;
var
  X: Integer;
begin
  if Count > 0 then
  begin
    for X := 1 to Count - 1 do
    begin
      TextMessageColor[X] := TextMessageColor[X + 1];
      TextMessage[X] := TextMessage[X + 1];
      NumMessage[X] := NumMessage[X + 1];  // scroll the messages up 1
      AlphaCount := 255;
    end;
    TextMessage[Count] := '';  // blank the last message
    NumMessage[Count] := 0;
    Dec(Count);
  end;
  ScrollTick := 0;
end;

procedure TConsole.ConsoleAdd(What: WideString; Col: Cardinal);
begin
  // adds a new message
  Inc(Count);
  ScrollTick := -NewMessageWait;
  TextMessage[Count] := What;
  TextMessageColor[Count] := Col;
  NumMessage[Count] := -255;
  if Count = 1 then
    AlphaCount := 255;
  if Count = CountMax then
    ScrollConsole;
end;

procedure TConsole.ConsoleNum(What: WideString; Col: Cardinal; Num: Integer);
begin
  // adds a new message
  Inc(Count);
  ScrollTick := -NewMessageWait;
  TextMessage[Count] := What;
  TextMessageColor[Count] := Col;
  NumMessage[Count] := Num;
  if Count = CountMax then
    ScrollConsole;
end;

procedure TConsole.Console(What: WideString; Col: Cardinal); overload;
begin
  if Length(What) = 0 then
    Exit;

  AddLineToLogFile(GameLog, AnsiString(What), ConsoleLogFileName);

  {$IFDEF SERVER}
  if TerminalColors then
    WriteLn(Format(#27'[38;2;%D;%D;%Dm%S'#27'[0m',
      [(Col and $00FF0000) shr 16, // r
      (Col and $0000FF00) shr 8,  // g
      (Col and $000000FF), // b
      What]))
  else
    WriteLn(What);
  {$IFDEF RCON}
  BroadCastMsg(AnsiString(What));
  {$ENDIF}

  // adds a new message
  // NOTE: not thread save!
  // added mod to prevent AVs
  Inc(Count);
  if Count >= CountMax then
    Count := 1;

  ScrollTick := -NewMessageWait;
  TextMessage[Count] := What;
  TextMessageColor[Count] := Col;
  NumMessage[Count] := -255;
  if Count = 1 then
    AlphaCount := 255;
  if Count = CountMax then
    ScrollConsole;
  {$ELSE}
  Debug('[Console] ' + AnsiString(What));
  MainConsole.ConsoleAdd(What, Col);
  BigConsole.ConsoleAdd(What, Col);
  {$ENDIF}
end;

procedure TConsole.Console(What: AnsiString; Col: Cardinal); overload;
begin
  Self.Console(WideString(What), Col);
end;

procedure TConsole.Console(What: Variant; Col: Cardinal); overload;
begin
  Self.Console(WideString(What), Col);
end;

{$IFDEF SERVER}
procedure TConsole.Console(What: Variant; Col: Cardinal; Sender: Byte); overload;
begin
  Self.Console(What, Col);
  if (Sender > 0) and (Sender < MAX_PLAYERS + 1) then
    ServerSendStringMessage(What, Sender, 255, MSGTYPE_PUB);
end;
{$ENDIF}

function TConsole.GetContentsAsPlainText(): WideString;
var
  I: Byte;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + TextMessage[I] + LineEnding;
end;

end.

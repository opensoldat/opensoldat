unit LauncherIPC;

interface

uses
  LauncherConnection, Classes, fpjson;

type
  // Shared base class that manages the connection thread, and provides common
  // functionalities used by both client and server.
  TLauncherIPC = class
  private
    FConnectionThread: TLauncherConnection;
    FThreadAlive: Boolean;
    procedure HandleMessage(Message: String);
    procedure HandleTerminate(Sender: TObject);
  protected
    procedure HandleCommandsMessage(JSON: TJSONObject);
    procedure HandleParsedMessage(MessageId: String; JSON: TJSONObject); virtual; abstract;

    procedure SendMessage(Message: String);
    procedure SendObjectAsJSON(AObject: TObject);
    // Launcher needs to know if connection came from client or server.
    procedure SendIdentityMessage; virtual; abstract;
  public
    procedure Connect(Port: Integer);
    destructor Destroy; override;
    property ThreadAlive: Boolean read FThreadAlive;
  end;

implementation

uses
  Command, TraceLog, fpjsonrtti, jsonparser, SysUtils;

procedure TLauncherIPC.Connect(Port: Integer);
begin
  FConnectionThread.Free;

  Debug('[LauncherIPC] Starting launcher connection thread');
  FConnectionThread := TLauncherConnection.Create(Port);
  FConnectionThread.OnMessage := HandleMessage;
  FConnectionThread.OnTerminate := HandleTerminate;
  FConnectionThread.FreeOnTerminate := False;
  FConnectionThread.Start;
  FThreadAlive := True;

  SendIdentityMessage;
end;

procedure TLauncherIPC.HandleMessage(Message: String);
var
  Data: TJSONData;
  JSON: TJSONObject;
  MessageId: String;
begin
  try
    Data := GetJSON(Message);
  except
    on ParserException: EJSONParser do begin
      Debug('[LauncherIPC] Could not parse received message as JSON: ' + ParserException.Message);
      Exit;
    end;
  end;

  try
    try
      JSON := Data as TJSONObject;
    except
      on CastException: EInvalidCast do begin
        Debug('[LauncherIPC] Could not cast received message to a JSON object: ' + CastException.Message);
        Exit;
      end;
    end;

    try
      MessageId := JSON.Get('id');
      HandleParsedMessage(MessageId, JSON);
    except
      on E: Exception do
        Debug('[LauncherIPC] Received invalid JSON: ' + E.Message);
    end;
  finally
    Data.Free;
  end;
end;

procedure TLauncherIPC.HandleCommandsMessage(JSON: TJSONObject);
var
  Commands: TJSONArray;
  Command: TJSONString;
  i: Integer;
begin
  Commands := JSON.Arrays['commands'];

  for i := 0 to Commands.Count - 1 do begin
    Command := Commands.Items[i] as TJSONString;
    ParseInput(Command.Value, 255);
  end;
end;

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
procedure TLauncherIPC.HandleTerminate(Sender: TObject);
begin
  Debug('[LauncherIPC] Thread terminated');
  FThreadAlive := False;
end;
{$POP}

procedure TLauncherIPC.SendMessage(Message: String);
begin
  if FThreadAlive then
    FConnectionThread.SendMessage(Message);
end;

procedure TLauncherIPC.SendObjectAsJSON(AObject: TObject);
var
  Streamer: TJSONStreamer;
  JSON: String;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    JSON := Streamer.ObjectToJSONString(AObject);
  finally
    Streamer.Free;
  end;
  SendMessage(JSON);
end;

destructor TLauncherIPC.Destroy;
begin
  if FThreadAlive then begin
    Debug('[LauncherIPC] Killing thread...');
    FConnectionThread.Terminate;
    FConnectionThread.WaitFor;
  end;

  FConnectionThread.Free;
  inherited;
end;

end.

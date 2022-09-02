unit LauncherIPC;

interface

uses
  LauncherConnection, Classes;

type
  // Shared base class that manages the connection thread, and provides common
  // functionalities used by both client and server.
  TLauncherIPC = class
  private
    FConnectionThread: TLauncherConnection;
    FThreadAlive: Boolean;
    procedure HandleTerminate(Sender: TObject);
  protected
    procedure HandleCommand(Message: String);
    procedure HandleMessage(Message: String); virtual; abstract;
  public
    procedure Connect(Port: Integer);
    destructor Destroy; override;
    procedure SendMessage(Message: String);
    property ThreadAlive: Boolean read FThreadAlive;
  end;

implementation

uses
  Command, TraceLog;

procedure TLauncherIPC.Connect(Port: Integer);
begin
  Debug('[LauncherIPC] Starting launcher connection thread');
  FConnectionThread := TLauncherConnection.Create(Port);
  FConnectionThread.OnMessage := HandleMessage;
  FConnectionThread.OnTerminate := HandleTerminate;
  FConnectionThread.FreeOnTerminate := True;
  FConnectionThread.Start;
  FThreadAlive := True;
end;

procedure TLauncherIPC.HandleCommand(Message: String);
begin
  ParseInput(Message, 255);
end;

procedure TLauncherIPC.HandleTerminate(Sender: TObject);
begin
  Debug('[LauncherIPC] Thread terminated');
  FThreadAlive := False;
end;

procedure TLauncherIPC.SendMessage(Message: String);
begin
  if FThreadAlive then
    FConnectionThread.SendMessage(Message);
end;

destructor TLauncherIPC.Destroy;
begin
  if FThreadAlive then begin
    Debug('[LauncherIPC] Killing thread...');
    FConnectionThread.Terminate;
    FConnectionThread.WaitFor;
  end;

  inherited;
end;

end.

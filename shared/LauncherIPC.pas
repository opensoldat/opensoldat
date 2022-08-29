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
    procedure HandleMessage(Message: String); virtual; abstract;
  public
    procedure Connect(Port: Integer);
    procedure SendMessage(Message: String);
    property ThreadAlive: Boolean read FThreadAlive;
  end;

implementation

uses
  TraceLog;

procedure TLauncherIPC.Connect(Port: Integer);
begin
  FConnectionThread := TLauncherConnection.Create(Port);
  FConnectionThread.OnMessage := HandleMessage;
  FConnectionThread.OnTerminate := HandleTerminate;
  FConnectionThread.FreeOnTerminate := True;
  FConnectionThread.Start;
  FThreadAlive := True;
end;

procedure TLauncherIPC.HandleTerminate(Sender: TObject);
begin
  Debug('[LauncherIPC] Connection terminated');
  FThreadAlive := False;
end;

procedure TLauncherIPC.SendMessage(Message: String);
begin
  if FThreadAlive then
    FConnectionThread.SendMessage(Message);
end;

end.

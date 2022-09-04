unit LauncherConnection;

interface

{$WARN 5071 OFF : Private type "$1.$2" never used}

uses
  Classes, Generics.Collections, ssockets;

type
  TMessageCallback = procedure(Message: String) of object;

  // Implements the connection between client/server and launcher, using a TCP socket.
  TLauncherConnection = class(TThread)
  private
    FPort: Integer;
    FSocket: TInetSocket;

    // This is a field so we can access it from Synchronized method.
    FReceivedMessage: String;
    FOnMessage: TMessageCallback;
    FSendQueue: TThreadList<String>;
    procedure HandleMessage;
    procedure ProcessSendQueue;
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;
    procedure Execute; override;

    // This callback will be run in main thread through Synchronize.
    property OnMessage: TMessageCallback read FOnMessage write FOnMessage;
    procedure SendMessage(Message: String);
  end;

implementation

uses
  TraceLog, SysUtils,
  // These 2 give us a convenient way to access socket/system error codes
  {$IFDEF UNIX}BaseUnix,{$ENDIF} Sockets;

const
  MAX_MESSAGE_LENGTH = 4096;

function IsRealSocketError(ErrCode: Integer): Boolean;
begin
  {$IFDEF UNIX}
  Result := (ErrCode <> ESockEINTR) and (ErrCode <> ESysEAGAIN);
  {$ELSE}
  Result := ErrCode <> ESockEINTR;
  {$ENDIF}
end;

constructor TLauncherConnection.Create(Port: Integer);
begin
  // Thread will be suspended after creation, so that we can set thread's properties.
  inherited Create(True);
  FPort := Port;
  FSendQueue := TThreadList<String>.Create;
end;

procedure TLauncherConnection.Execute;
type
  TBuffer = array[0..MAX_MESSAGE_LENGTH-1] of Char;
var
  ReadBuffer: TBuffer;
  ReadLength: LongInt;
begin
  try
    FSocket := TInetSocket.Create('127.0.0.1', FPort, 5000);
  except
    on SocketError: ESocketError do begin
      Debug('[LauncherConnection] ' + SocketError.Message);
      Exit;
    end;
  end;
  Debug('[LauncherConnection] Connected');
  FSocket.IOTimeout := 1000;
  ReadBuffer := Default(TBuffer);

  while not Terminated do begin
    ReadLength := FSocket.Read(ReadBuffer, MAX_MESSAGE_LENGTH);
    if ReadLength > 0 then begin
      FReceivedMessage := ReadBuffer;
      Synchronize(HandleMessage);
    end else if ReadLength = 0 then begin
      Debug('[LauncherConnection] Launcher closed the connection');
      Exit;
    end else begin
      if IsRealSocketError(FSocket.LastError) then begin
        Debug('[LauncherConnection] Failed to read from socket. Error code: ' +
          IntToStr(FSocket.LastError));
        Exit;
      end;
    end;

    ProcessSendQueue;
    Sleep(250);
  end;
end;

procedure TLauncherConnection.HandleMessage;
begin
  if Assigned(FOnMessage) then
    FOnMessage(FReceivedMessage);
end;

procedure TLauncherConnection.ProcessSendQueue;
var
  Message: String;
  Queue: TList<String>;
begin
  Queue := FSendQueue.LockList;
  if Queue.Count = 0 then begin
    FSendQueue.UnlockList;
    Exit;
  end;

  Message := Queue.First;
  if FSocket.Write(Message[1], Length(Message)) < 0 then
    Debug('[LauncherConnection] Failed to write to socket. Error code: ' +
      IntToStr(FSocket.LastError));
  Queue.Remove(Message);
  FSendQueue.UnlockList;
end;

procedure TLauncherConnection.SendMessage(Message: String);
var
  Queue: TList<String>;
begin
  Debug('[LauncherConnection] Sending ' + Message);
  // Locking probably not needed here.
  Queue := FSendQueue.LockList;
  Queue.Add(Message);
  FSendQueue.UnlockList;
end;

destructor TLauncherConnection.Destroy;
begin
  if Assigned(FSocket) then
    FSocket.Free;
  FSendQueue.Free;
  inherited Destroy;
end;

end.

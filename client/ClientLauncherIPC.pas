unit ClientLauncherIPC;

interface

uses
  LauncherIPC, fpjson;

type
  TClientLauncherIPC = class(TLauncherIPC)
  protected
    procedure HandleParsedMessage(MessageId: String; JSON: TJSONObject); override;
    procedure SendIdentityMessage; override;
  public
    procedure SendJoinServerMessage(IP: String; Port: Integer);
  end;

implementation

uses
  LauncherMessages;

procedure TClientLauncherIPC.HandleParsedMessage(MessageId: String; JSON: TJSONObject);
begin
  if MessageId = LauncherMessageIds.Commands then
    HandleCommandsMessage(JSON);
end;

procedure TClientLauncherIPC.SendIdentityMessage;
var
  Message: TClientIdentityMessage;
begin
  Message := TClientIdentityMessage.Create;
  SendObjectAsJSON(Message);
  Message.Free;
end;

procedure TClientLauncherIPC.SendJoinServerMessage(IP: String; Port: Integer);
var
  Message: TJoinServerMessage;
begin
  Message := TJoinServerMessage.Create(IP, Port);
  SendObjectAsJSON(Message);
  Message.Free;
end;

end.

unit ServerLauncherIPC;

interface

uses
  LauncherIPC, fpjson;

type
  TServerLauncherIPC = class(TLauncherIPC)
  protected
    procedure HandleParsedMessage(MessageId: String; JSON: TJSONObject); override;
    procedure SendIdentityMessage; override;
  public
    procedure SendReadyForClientsMessage;
  end;

implementation

uses
  LauncherMessages;

procedure TServerLauncherIPC.HandleParsedMessage(MessageId: String; JSON: TJSONObject);
begin
  if MessageId = LauncherMessageIds.Commands then
    HandleCommandsMessage(JSON);
end;

procedure TServerLauncherIPC.SendIdentityMessage;
var
  Message: TIdentityMessage;
begin
  Message := TIdentityMessage.Create(GameProcessTypes.Server);
  SendObjectAsJSON(Message);
  Message.Free;
end;

procedure TServerLauncherIPC.SendReadyForClientsMessage;
var
  Message: TReadyForClientsMessage;
begin
  Message := TReadyForClientsMessage.Create;
  SendObjectAsJSON(Message);
  Message.Free;
end;

end.



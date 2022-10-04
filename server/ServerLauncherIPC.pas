unit ServerLauncherIPC;

interface

uses
  LauncherIPC, fpjson;

type
  TServerLauncherIPC = class(TLauncherIPC)
  protected
    procedure HandleParsedMessage(MessageId: String; JSON: TJSONObject); override;
    procedure SendIdentityMessage; override;
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
  Message: TServerIdentityMessage;
begin
  Message := TServerIdentityMessage.Create;
  SendObjectAsJSON(Message);
  Message.Free;
end;

end.



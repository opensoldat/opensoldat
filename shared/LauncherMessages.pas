unit LauncherMessages;

interface

uses
  Classes;

type
  LauncherMessageIds = record
    const Commands = 'COMMANDS';
    const ClientIdentity = 'CLIENT_ID';
    const ServerIdentity = 'SERVER_ID';
    const JoinServer = 'JOIN_SERVER';
  end;

  TMessage = class(TPersistent)
  protected
    FId: String;
  published
    property id: String read FId write FId;
  end;

  {$IFDEF SERVER}
  TServerIdentityMessage = class(TMessage)
  public
    constructor Create;
  end;
  {$ELSE}
  TClientIdentityMessage = class(TMessage)
  public
    constructor Create;
  end;

  TJoinServerMessage = class(TMessage)
  private
    FIP: String;
    FPort: Integer;
  public
    constructor Create(IP: String; Port: Integer);
  published
    property ip: String read FIP write FIP;
    property port: Integer read FPort write FPort;
  end;

  {$ENDIF}

implementation

{$IFDEF SERVER}
constructor TServerIdentityMessage.Create;
begin
  FId := LauncherMessageIds.ServerIdentity;
end;
{$ELSE}
constructor TClientIdentityMessage.Create;
begin
  FId := LauncherMessageIds.ClientIdentity;
end;

constructor TJoinServerMessage.Create(IP: String; Port: Integer);
begin
  FId := LauncherMessageIds.JoinServer;
  FIP := IP;
  FPort := Port;
end;
{$ENDIF}

end.

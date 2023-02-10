unit LauncherMessages;

interface

uses
  Classes;

type
  LauncherMessageIds = record
    const Commands = 'COMMANDS';
    const Identity = 'IDENTITY';
    {$IFDEF SERVER}
    const ReadyForClients = 'READY_FOR_CLIENTS';
    {$ELSE}
    const JoinServer = 'JOIN_SERVER';
    {$ENDIF}
  end;

  GameProcessTypes = record
    const Client = 'CLIENT';
    const Server = 'SERVER';
  end;

  TMessage = class(TPersistent)
  protected
    FId: String;
  published
    property id: String read FId write FId;
  end;

  TIdentityMessage = class(TMessage)
  private
    FProcessId: String;
    FProcessType: String;
  public
    constructor Create(ProcessType: String);
  published
    property processId: String read FProcessId write FProcessId;
    property processType: String read FProcessType write FProcessType;
  end;

  {$IFDEF SERVER}
  TReadyForClientsMessage = class(TMessage)
  public
    constructor Create;
  end;

  {$ELSE}
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

uses
  SysUtils;

constructor TIdentityMessage.Create(ProcessType: String);
begin
  FId := LauncherMessageIds.Identity;
  FProcessId := GetProcessId.ToString;
  FProcessType := ProcessType;
end;

{$IFDEF SERVER}
constructor TReadyForClientsMessage.Create;
begin
  FId := LauncherMessageIds.ReadyForClients;
end;

{$ELSE}
constructor TJoinServerMessage.Create(IP: String; Port: Integer);
begin
  FId := LauncherMessageIds.JoinServer;
  FIP := IP;
  FPort := Port;
end;
{$ENDIF}

end.

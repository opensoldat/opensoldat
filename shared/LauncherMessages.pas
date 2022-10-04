unit LauncherMessages;

interface

uses
  Classes;

type
  LauncherMessageIds = record
    const Commands = 'COMMANDS';
    const ServerIdentity = 'SERVER_ID';
  end;

  {$IFDEF SERVER}
  TServerIdentityMessage = class(TPersistent)
  private
    FId: String;
  public
    constructor Create;
  published
    property id: String read FId write FId;
  end;
  {$ENDIF}

implementation

{$IFDEF SERVER}
constructor TServerIdentityMessage.Create;
begin
  FId := LauncherMessageIds.ServerIdentity;
end;
{$ENDIF}

end.

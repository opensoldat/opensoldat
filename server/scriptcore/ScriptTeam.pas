{*******************************************************}
{                                                       }
{       ScriptTeam unit for OPENSOLDAT                  }
{                                                       }
{       Copyright (c) 2013 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit ScriptTeam;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  Server,
  Game,
  ScriptPlayer,
  ScriptCore3Api,
  SysUtils;

type

  TScriptTeam = class;

  TOnBeforeJoinTeam = function (Player: TScriptActivePlayer; Team, OldTeam: TScriptTeam): ShortInt of object;
  TOnJoinTeam = procedure(Player: TScriptActivePlayer; Team: TScriptTeam) of object;
  TOnLeaveTeam = procedure(Player: TScriptActivePlayer; Team: TScriptTeam; Kicked: Boolean) of object;

  TScriptTeam = class(TObject)
  private
    FPlayers: TList;
    FID: Byte;
    FOnBeforeJoin: TOnBeforeJoinTeam;
    FOnJoin: TOnJoinTeam;
    FOnLeave: TOnLeaveTeam;
    function GetScore: Byte;
    procedure SetScore(Score: Byte);
    function GetPlayer(Num: Byte): TScriptActivePlayer;
    function GetCount: Byte;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;
    procedure AddPlayer(Player: TScriptActivePlayer);
    procedure RemovePlayer(Player: TScriptActivePlayer);

    property Score: Byte read GetScore write SetScore;
    property Player[i: Byte]: TScriptActivePlayer read GetPlayer; default;
    property Count: Byte read GetCount;
    property ID: Byte read FID;
    property OnBeforeJoin: TOnBeforeJoinTeam read FOnBeforeJoin write FOnBeforeJoin;
    property OnJoin: TOnJoinTeam read FOnJoin write FOnJoin;
    property OnLeave: TOnLeaveTeam read FOnLeave write FOnLeave;
  end;

  TScriptTeamAPI = class(TScriptCore3API)
  public
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

constructor TScriptTeam.Create(ID: Byte);
begin
  Self.FID := ID;
  Self.FPlayers := TList.Create;
end;

destructor TScriptTeam.Destroy;
begin
  Self.FPlayers.Free;
end;

procedure TScriptTeam.AddPlayer(Player: TScriptActivePlayer);
begin
  Self.FPlayers.Add(Player);
end;

procedure TScriptTeam.RemovePlayer(Player: TScriptActivePlayer);
begin
  Self.FPlayers.Remove(Player);
end;

function TScriptTeam.GetScore: Byte;
begin
  Result := TeamScore[Self.FID];
end;

procedure TScriptTeam.SetScore(Score: Byte);
begin
  TeamScore[Self.FID] := Score;
  SortPlayers;
end;

function TScriptTeam.GetPlayer(Num: Byte): TScriptActivePlayer;
begin
  if Num < Self.FPlayers.Count then
    Result := Self.FPlayers[Num]
  else
    Result := nil;
end;

function TScriptTeam.GetCount: Byte;
begin
  Result := Self.FPlayers.Count;
end;

procedure ScoreReadHelper(Self: TScriptTeam; var Result: Byte);
begin
  Result := Self.Score;
end;

procedure ScoreWriteHelper(Self: TScriptTeam; const Result: Byte);
begin
  Self.Score := Result;
end;

procedure PlayerReadHelper(Self: TScriptTeam; var Result: TScriptActivePlayer; const Num: Byte);
begin
  Result := Self[Num];
end;

procedure CountReadHelper(Self: TScriptTeam; var Result: Byte);
begin
  Result := Self.Count;
end;

procedure IDReadHelper(Self: TScriptTeam; var Result: Byte);
begin
  Result := Self.ID;
end;

procedure OnBeforeJoinReadHelper(Self: TScriptTeam; var Result: TOnBeforeJoinTeam);
begin
  Result := Self.OnBeforeJoin;
end;

procedure OnBeforeJoinWriteHelper(Self: TScriptTeam; const Result: TOnBeforeJoinTeam);
begin
  Self.OnBeforeJoin := Result;
end;

procedure OnJoinReadHelper(Self: TScriptTeam; var Result: TOnJoinTeam);
begin
  Result := Self.OnJoin;
end;

procedure OnJoinWriteHelper(Self: TScriptTeam; const Result: TOnJoinTeam);
begin
  Self.OnJoin := Result;
end;

procedure OnLeaveReadHelper(Self: TScriptTeam; var Result: TOnLeaveTeam);
begin
  Result := Self.OnLeave;
end;

procedure OnLeaveWriteHelper(Self: TScriptTeam; const Result: TOnLeaveTeam);
begin
  Self.OnLeave := Result;
end;


procedure TScriptTeamAPI.CompilerRegister(Compiler: TPascalCompiler);
var
   Cls: TPascalCompiletimeClass;
begin

  Cls := Compiler.AddClass(nil, 'TTeam');
  Compiler.AddType('TOnBeforeJoinTeamEvent', 'function (Player: TActivePlayer; Team, OldTeam: TTeam): ShortInt');
  Compiler.AddType('TOnJoinTeamEvent', 'procedure(Player: TActivePlayer; Team: TTeam)');
  Compiler.AddType('TOnLeaveTeamEvent', 'procedure(Player: TActivePlayer; Team: TTeam; Kicked: Boolean)');

  with Cls do
  begin
    RegisterProperty('Score', 'Byte', iptRW);
    RegisterProperty('Player', 'TActivePlayer Byte', iptR);
    RegisterProperty('Count', 'Byte', iptR);
    RegisterProperty('ID', 'Byte', iptR);
    RegisterProperty('OnBeforeJoin', 'TOnBeforeJoinTeamEvent', iptRW);
    RegisterProperty('OnJoin', 'TOnJoinTeamEvent', iptRW);
    RegisterProperty('OnLeave', 'TOnLeaveTeamEvent', iptRW);
    SetDefaultPropery('Player');
  end;
end;

procedure TScriptTeamAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptTeam, 'TTeam') do
  begin
    RegisterPropertyHelper(@ScoreReadHelper, @ScoreWriteHelper, 'Score');
    RegisterPropertyHelper(@PlayerReadHelper, nil, 'Player');
    RegisterPropertyHelper(@CountReadHelper, nil, 'Count');
    RegisterPropertyHelper(@IDReadHelper, nil, 'ID');
    RegisterEventPropertyHelper(@OnBeforeJoinReadHelper, @OnBeforeJoinWriteHelper, 'OnBeforeJoin');
    RegisterEventPropertyHelper(@OnJoinReadHelper, @OnJoinWriteHelper, 'OnJoin');
    RegisterEventPropertyHelper(@OnLeaveReadHelper, @OnLeaveWriteHelper, 'OnLeave');
  end;
end;

end.

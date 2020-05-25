unit ScriptCore3Api;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  Script;

type
  TScriptCore3API = class(TInterfacedObject, ICompilerAPI, IRuntimeAPI)
  protected
    FScript: TScript;
  public
    constructor Create(Script: TScript);
    procedure CompilerRegister(Compiler: TPascalCompiler); virtual;
    procedure RuntimeRegisterApi(Exec: TPascalExec); virtual;
    procedure RuntimeRegisterVariables(Exec: TPascalExec); virtual;
    procedure BeforeExecute(Exec: TPascalExec); virtual;
    procedure AfterExecute(Exec: TPascalExec); virtual;
  end;


implementation
{$push}{$warn 5024 off}
constructor TScriptCore3API.Create(Script: TScript);
begin
  Self.FScript := Script;
end;

procedure TScriptCore3API.CompilerRegister(Compiler: TPascalCompiler);
begin
end;

procedure TScriptCore3API.RuntimeRegisterApi(Exec: TPascalExec);
begin
end;

procedure TScriptCore3API.RuntimeRegisterVariables(Exec: TPascalExec);
begin
end;

procedure TScriptCore3API.BeforeExecute(Exec: TPascalExec);
begin
end;

procedure TScriptCore3API.AfterExecute(Exec: TPascalExec);
begin
end;
{$pop}
end.

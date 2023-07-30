{*************************************************************}
{                                                             }
{       ScriptCore3Api Unit for OpenSoldat                    }
{                                                             }
{       Copyright (c) 2020-2023 OpenSoldat contributors       }
{                                                             }
{*************************************************************}

unit ScriptCore3Api;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  // System units
  Classes,

  // Project units
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
    function CallEvent(const Event; const Params: array of Variant): Variant;
  end;


implementation

uses
  // Project units
  ScriptCore3;


{$PUSH}
{$WARN 5024 OFF}
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

function TScriptCore3API.CallEvent(const Event; const Params: array of Variant): Variant;
begin
  Result := TScriptCore3(FScript).CallEvent(Event, Params);
end;
{$POP}

end.

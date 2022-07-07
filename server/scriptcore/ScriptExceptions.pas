{*******************************************************}
{                                                       }
{       ScriptExceptions unit for OPENSOLDAT            }
{                                                       }
{       Copyright (c) 2013 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}
unit ScriptExceptions;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes, SysUtils;

// those types do not begin with "T" because Exception class clearly doesn't.
type
  ScriptException = class(Exception)
  protected
    FMessage: string;
    function GetMessage: string;
    procedure SetMessage(const Msg: string);
  public
    constructor Create(const Msg: string);
    property Message: string read GetMessage write SetMessage;
  end;

  EScriptUnload = class (Exception)
  end;

  EScriptRecompile = class(Exception)
  private
    FForce: Boolean;
  public
    constructor Create(Message: String; Force: Boolean);
    property Force: Boolean read FForce;
  end;

  EAccessDenied = class(ScriptException)
  end;

  ENotImplemented = class (ScriptException)
  end;

implementation

constructor EScriptRecompile.Create(Message: String; Force: Boolean);
begin
  inherited Create(Message);
  FForce := Force;
end;

function ScriptException.GetMessage: string;
begin
  Result := '[' + Self.ClassName + '] ' + Self.FMessage;
end;

procedure ScriptException.SetMessage(const Msg: string);
begin
  Self.FMessage := Msg;
end;

constructor ScriptException.Create(const Msg: string);
begin
  inherited;
  Self.FMessage := Msg;
end;

end.


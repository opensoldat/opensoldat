{*******************************************************}
{                                                       }
{       PascalDebugger unit for OPENSOLDAT              }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit PascalDebugger;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes, PascalExec, SysUtils,
  uPSDebugger, uPSRuntime;

type

  TPascalError = uPSRuntime.TPSError;

  TPascalDebugger = class(TPascalExec)
  protected
    procedure CreateExec; override;
    procedure LoadDebugSymbols(DebugSymbols: string);
    function GetLastError: TPascalError;
  public
    constructor Create;
    function GetErrorString: string; overload; override;
    function GetErrorString(ExCode: TPSError; ExString: string;
      ProcNo, Position: Cardinal): string; reintroduce; overload;
    procedure GetPosition(out UnitName: string; out Col, Row: Cardinal); overload;
    procedure GetPosition(out UnitName: string; out Col, Row: Cardinal;
      ProcNo, Position: Cardinal); overload;
    property DebugSymbols: string write LoadDebugSymbols;
    property LastError: TPascalError read GetLastError;
  end;

implementation

procedure TPascalDebugger.CreateExec;
begin
  Self.FExec := TPSDebugExec.Create;
end;

procedure TPascalDebugger.LoadDebugSymbols(DebugSymbols: string);
begin
  TPSDebugExec(Self.FExec).LoadDebugData(DebugSymbols);
end;

function TPascalDebugger.GetLastError: TPascalError;
begin
  Result := Self.FExec.ExceptionCode;
end;

constructor TPascalDebugger.Create;
begin
  inherited;
  TPSDebugExec(Self.FExec).DebugEnabled := True;
end;

function TPascalDebugger.GetErrorString: string;
begin
  Result := Self.GetErrorString(Self.FExec.ExceptionCode,
    Self.FExec.ExceptionString, Self.FExec.ExceptionProcNo, Self.FExec.ExceptionPos);
end;

function TPascalDebugger.GetErrorString(ExCode: TPSError; ExString: string;
  ProcNo, Position: Cardinal): string;
var
  Col: Cardinal = 0;
  Row: Cardinal = 0;
  Pos: Cardinal = 0;
  UnitName: string = '';
begin
  if TPSDebugExec(Self.FExec).TranslatePositionEx(ProcNo, Position,
    Pos, Row, Col, UnitName) then
    Result := 'In unit ' + UnitName + '(' + IntToStr(Row) + ':' +
      IntToStr(Col) + '): ' + TIFErrorToString(ExCode, ExString)
  else
    Result := TIFErrorToString(ExCode, ExString);
end;

procedure TPascalDebugger.GetPosition(out UnitName: string; out Col, Row: Cardinal);
var
  Pos: Cardinal = 0;
  Exec: TPSDebugExec;
begin
  UnitName := '';
  Col := 0;
  Row := 0;
  Exec := TPSDebugExec(Self.FExec);
  Exec.TranslatePositionEx(Exec.ExceptionProcNo, Exec.ExceptionPos,
    Pos, Row, Col, UnitName);
end;

procedure TPascalDebugger.GetPosition(out UnitName: string;
  out Col, Row: Cardinal; ProcNo, Position: Cardinal);
var
  Pos: Cardinal = 0;
begin
  UnitName := '';
  Col := 0;
  Row := 0;
  TPSDebugExec(Self.FExec).TranslatePositionEx(ProcNo, Position, Pos,
    Row, Col, UnitName);
end;

end.

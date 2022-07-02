{*******************************************************}
{                                                       }
{       DateUtils unit for SOLDAT                       }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

// i guess there's not much to comment here
unit ScriptDateUtils;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  ScriptCore3Api,
  SysUtils;

type
  TScriptDateUtilsAPI = class(TScriptCore3API)
  public
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

uses
  DateUtils;

function DateTimeToUnix(D: TDateTime): Int64;
begin
  Result := Round((D - 25569) * 86400);
end;

function UnixToDateTime(U: Int64): TDateTime;
begin
  Result := U / 86400 + 25569;
end;

function MyStrToDateTime(const S: AnsiString): TDateTime;
begin
  Result := StrToDateTime(S);
end;

function MyDateToStr(Date: TDateTime): string;
begin
  Result := DateToStr(Date);
end;

function MyFormatDateTime(const FormatStr: string; DateTime: TDateTime): string;
begin
  Result := FormatDateTime(FormatStr, DateTime);
end;

function MyStrToDate(const S: string): TDateTime;
begin
  Result := StrToDate(S);
end;

procedure TScriptDateUtilsAPI.CompilerRegister(Compiler: TPascalCompiler);
begin
  Compiler.AddType('TDateTime', btDouble).ExportName := True;
  Compiler.AddFunction('function StrToDateTime(const S: string): TDateTime;');
  Compiler.AddFunction('function EncodeDate(Year, Month, Day: Word): TDateTime;');
  Compiler.AddFunction('function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;');
  Compiler.AddFunction('function EncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec: Word): TDateTime;');
  Compiler.AddFunction('function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;');
  Compiler.AddFunction('function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;');
  Compiler.AddFunction('function TryEncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;');
  Compiler.AddFunction('procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);');
  Compiler.AddFunction('procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);');
  Compiler.AddFunction('procedure DecodeDateTime(const DateTime: TDateTime; var Year, Month, Day, Hour, Min, Sec, MSec: Word);');
  Compiler.AddFunction('function DayOfWeek(const DateTime: TDateTime): Word;');
  Compiler.AddFunction('function Date: TDateTime;');
  Compiler.AddFunction('function Time: TDateTime;');
  Compiler.AddFunction('function Now: TDateTime;');
  Compiler.AddFunction('function DateTimeToUnix(D: TDateTime): Int64;');
  Compiler.AddFunction('function UnixToDateTime(U: Int64): TDateTime;');
  Compiler.AddFunction('function DateToStr(D: TDateTime): string;');
  Compiler.AddFunction('function StrToDate(const S: string): TDateTime;');
  Compiler.AddFunction('function FormatDateTime(const fmt: string; D: TDateTime): string;');
end;

procedure TScriptDateUtilsAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  Exec.AddFunction(@MyStrToDateTime, 'StrToDateTime');
  Exec.AddFunction(@EncodeDate, 'EncodeDate');
  Exec.AddFunction(@EncodeTime, 'EncodeTime');
  Exec.AddFunction(@EncodeDateTime, 'EncodeDateTime');
  Exec.AddFunction(@TryEncodeDate, 'TryEncodeDate');
  Exec.AddFunction(@TryEncodeTime, 'TryEncodeTime');
  Exec.AddFunction(@TryEncodeDateTime, 'TryEncodeDateTime');
  Exec.AddFunction(@DecodeDate, 'DecodeDate');
  Exec.AddFunction(@DecodeTime, 'DecodeTime');
  Exec.AddFunction(@DecodeDateTime, 'DecodeDateTime');
  Exec.AddFunction(@DayOfWeek, 'DayOfWeek');
  Exec.AddFunction(@Date, 'Date');
  Exec.AddFunction(@Time, 'Time');
  Exec.AddFunction(@Now, 'Now');
  Exec.AddFunction(@DateTimeToUnix, 'DateTimeToUnix');
  Exec.AddFunction(@UnixToDateTime, 'UnixToDateTime');
  Exec.AddFunction(@MyDateToStr, 'DateToStr');
  Exec.AddFunction(@MyFormatDateTime, 'FormatDateTime');
  Exec.AddFunction(@MyStrToDate, 'StrToDate');
end;

end.


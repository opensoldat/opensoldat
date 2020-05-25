{ Compile time Date Time library }
unit uPSC_dateutils;

interface
uses
  SysUtils, uPSCompiler, uPSUtils;


procedure RegisterDateTimeLibrary_C(S: TPSPascalCompiler);

implementation

procedure RegisterDatetimeLibrary_C(S: TPSPascalCompiler);
begin
  s.AddType('TDateTime', btDouble).ExportName := True;
  s.AddDelphiFunction('function EncodeDate(Year, Month, Day: Word): TDateTime;');
  s.AddDelphiFunction('function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;');
  s.AddDelphiFunction('function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;');
  s.AddDelphiFunction('function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;');
  s.AddDelphiFunction('procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);');
  s.AddDelphiFunction('procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);');
  s.AddDelphiFunction('function DayOfWeek(const DateTime: TDateTime): Word;');
  s.AddDelphiFunction('function Date: TDateTime;');
  s.AddDelphiFunction('function Time: TDateTime;');
  s.AddDelphiFunction('function Now: TDateTime;');
  s.AddDelphiFunction('function DateTimeToUnix(D: TDateTime): Int64;');
  s.AddDelphiFunction('function UnixToDateTime(U: Int64): TDateTime;');

  s.AddDelphiFunction('function DateToStr(D: TDateTime): string;');
  s.AddDelphiFunction('function StrToDate(const S: string): TDateTime;');
  s.AddDelphiFunction('function FormatDateTime(const fmt: string; D: TDateTime): string;');
end;

end.

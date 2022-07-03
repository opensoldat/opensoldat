unit ScriptMath;

{$mode delphi}

interface

uses
  Classes, SysUtils, Math, ScriptCore3Api, PascalCompiler, PascalExec;

type
  TScriptMath = class
  private
    function GetE: Extended;
    function GetPi: Extended;
  public
    function Sin(A: Extended): Extended;
    function Cos(A: Extended): Extended;
    function Tan(A: Extended): Extended;
    function Cotan(A: Extended): Extended;
    function Pow(A, B: Extended): Extended;
    function LogN(A, B: Extended): Extended;
    function Ln(A: Extended): Extended;
    function ArcSin(A: Extended): Extended;
    function ArcCos(A: Extended): Extended;
    function ArcTan(A: Extended): Extended;
    function ArcCotan(A: Extended): Extended;
    function ArcTan2(A, B: Extended): Extended;
    function Min(A, B: Extended): Extended;
    function Max(A, B: Extended): Extended;
    function Abs(A: Extended): Extended;
    function Exp(A: Extended): Extended;
    function Sign(A: Extended): TValueSign;
    function IsNaN(A: Extended): Boolean;
    function Round(A: Extended): Integer;
    function RoundTo(A: Extended; B: TRoundToRange): Extended;
    function DegToRad(A: Extended): Extended;
    function RadToDeg(A: Extended): Extended;
    function DegNormalize(A: Extended): Extended;
    function InRange(const A, B, C: Double): Boolean;
    function EnsureRange(const AValue, AMin, AMax: Double): Double;
    function Random(Min, Max: Extended): Extended;
    procedure Sincos(theta : extended;out sinus,cosinus : extended);
    property E: Extended read GetE;
    property Pi: Extended read GetPi;
  end;

  TScriptMathAPI = class(TScriptCore3API)
  private
    FMath: TScriptMath;
  public
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
    procedure RuntimeRegisterVariables(Exec: TPascalExec); override;
  end;

implementation

function TScriptMath.GetE: Extended;
begin
  Result := 2.718281828459045;
end;

function TScriptMath.GetPi: Extended;
begin
  Result := system.pi;
end;

function TScriptMath.Sin(A: Extended): Extended;
begin
  Result := System.sin(a);
end;

function TScriptMath.Cos(A: Extended): Extended;
begin
  Result := System.cos(a);
end;

function TScriptMath.Tan(A: Extended): Extended;
begin
  Result := Math.tan(a);
end;

function TScriptMath.Cotan(A: Extended): Extended;
begin
  Result := Math.cotan(a);
end;

function TScriptMath.Pow(A, B: Extended): Extended;
begin
  Result := Math.power(a, b);
end;

function TScriptMath.LogN(A, B: Extended): Extended;
begin
  Result := Math.logn(a, b);
end;

function TScriptMath.Ln(A: Extended): Extended;
begin
  Result := System.ln(a);
end;

function TScriptMath.ArcSin(A: Extended): Extended;
begin
  Result := Math.arcsin(a);
end;

function TScriptMath.ArcCos(A: Extended): Extended;
begin
  Result := Math.arccos(a);
end;

function TScriptMath.ArcTan(A: Extended): Extended;
begin
  Result := System.arctan(a);
end;

function TScriptMath.ArcCotan(A: Extended): Extended;
begin
  Result := Math.cotan(a);
end;

function TScriptMath.ArcTan2(A, B: Extended): Extended;
begin
  Result := Math.arctan2(a, b);
end;

function TScriptMath.Min(A, B: Extended): Extended;
begin
  Result := Math.Min(a, b);
end;

function TScriptMath.Max(A, B: Extended): Extended;
begin
  Result := Math.Max(a, b);
end;

function TScriptMath.Abs(A: Extended): Extended;
begin
  Result := System.abs(a);
end;

function TScriptMath.Exp(A: Extended): Extended;
begin
  Result := System.exp(a);
end;

function TScriptMath.Sign(A: Extended): TValueSign;
begin
  Result := Math.Sign(a);
end;

function TScriptMath.IsNaN(A: Extended): Boolean;
begin
  Result := Math.IsNan(a);
end;

function TScriptMath.Round(A: Extended): Integer;
begin
  Result := System.round(a);
end;

function TScriptMath.RoundTo(A: Extended; B: TRoundToRange): Extended;
begin
  Result := Math.RoundTo(a, b);
end;

function TScriptMath.DegToRad(A: Extended): Extended;
begin
  Result := Math.DegToRad(a);
end;

function TScriptMath.RadToDeg(A: Extended): Extended;
begin
  Result := Math.RadToDeg(a);
end;

function TScriptMath.DegNormalize(A: Extended): Extended;
begin
  Result := Math.DegNormalize(A);
end;

function TScriptMath.InRange(const A, B, C: Double): Boolean;
begin
  Result := Math.InRange(a, b, c);
end;

function TScriptMath.EnsureRange(const AValue, AMin, AMax: Double): Double;
begin
  Result := Math.EnsureRange(AValue, AMin, AMax);
end;

function TScriptMath.Random(Min, Max: Extended): Extended;
begin
  Result := Min + System.Random * (Max - Min);
end;

procedure TScriptMath.Sincos(theta : extended;out sinus,cosinus : extended);
begin
  Math.sincos(theta, sinus, cosinus);
end;

procedure ScriptMathGetEHelper(Self: TScriptMath; var Result: Extended);
begin
  Result := Self.E;
end;

procedure ScriptMathGetPiHelper(Self: TScriptMath; var Result: Extended);
begin
  Result := Self.Pi;
end;

procedure TScriptMathAPI.CompilerRegister(Compiler: TPascalCompiler);
var
  AClass: TPascalCompiletimeClass;
begin
  Compiler.AddType('TValueSign', btS8);
  Compiler.AddType('TRoundToRange', btS8);
  AClass := Compiler.AddClass(nil, 'TMathAPI');
  with AClass do
  begin
    RegisterMethod('function Sin(A: Extended): Extended');
    RegisterMethod('function Cos(A: Extended): Extended');
    RegisterMethod('function Tan(A: Extended): Extended');
    RegisterMethod('function Cotan(A: Extended): Extended');
    RegisterMethod('function Pow(A, B: Extended): Extended');
    RegisterMethod('function LogN(A, B: Extended): Extended');
    RegisterMethod('function Ln(A: Extended): Extended');
    RegisterMethod('function ArcSin(A: Extended): Extended');
    RegisterMethod('function ArcCos(A: Extended): Extended');
    RegisterMethod('function ArcTan(A: Extended): Extended');
    RegisterMethod('function ArcCotan(A: Extended): Extended');
    RegisterMethod('function ArcTan2(A, B: Extended): Extended');
    RegisterMethod('function Min(A, B: Extended): Extended');
    RegisterMethod('function Max(A, B: Extended): Extended');
    RegisterMethod('function Abs(A: Extended): Extended');
    RegisterMethod('function Exp(A: Extended): Extended');
    RegisterMethod('function Sign(A: Extended): TValueSign');
    RegisterMethod('function IsNaN(A: Extended): Boolean');
    RegisterMethod('function Round(A: Extended): Integer');
    RegisterMethod('function RoundTo(A: Extended; B: TRoundToRange): Extended');
    RegisterMethod('function DegToRad(A: Extended): Extended');
    RegisterMethod('function RadToDeg(A: Extended): Extended');
    RegisterMethod('function DegNormalize(A: Extended): Extended');
    RegisterMethod('function InRange(const A, B, C: Double): Boolean');
    RegisterMethod('function EnsureRange(const AValue, AMin, AMax: Double): Double');
    RegisterMethod('function Random(Min, Max: Extended): Extended');
    RegisterMethod('procedure Sincos(theta : extended;out sinus,cosinus : extended)');
    RegisterProperty('E', 'Extended', iptR);
    RegisterProperty('Pi', 'Extended', iptR);
  end;
  Compiler.AddPtrVariable('Math', AClass.aType);
end;

procedure TScriptMathAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptMath, 'TMathAPI') do
  begin
    RegisterMethod(@TScriptMath.Sin, 'Sin');
    RegisterMethod(@TScriptMath.Cos, 'Cos');
    RegisterMethod(@TScriptMath.Tan, 'Tan');
    RegisterMethod(@TScriptMath.Cotan, 'Cotan');
    RegisterMethod(@TScriptMath.Pow, 'Pow');
    RegisterMethod(@TScriptMath.LogN, 'LogN');
    RegisterMethod(@TScriptMath.Ln, 'Ln');
    RegisterMethod(@TScriptMath.ArcSin, 'ArcSin');
    RegisterMethod(@TScriptMath.ArcCos, 'ArcCos');
    RegisterMethod(@TScriptMath.ArcTan, 'ArcTan');
    RegisterMethod(@TScriptMath.ArcCotan, 'ArcCotan');
    RegisterMethod(@TScriptMath.ArcTan2, 'ArcTan2');
    RegisterMethod(@TScriptMath.Min, 'Min');
    RegisterMethod(@TScriptMath.Max, 'Max');
    RegisterMethod(@TScriptMath.Abs, 'Abs');
    RegisterMethod(@TScriptMath.Exp, 'Exp');
    RegisterMethod(@TScriptMath.Sign, 'Sign');
    RegisterMethod(@TScriptMath.IsNaN, 'IsNaN');
    RegisterMethod(@TScriptMath.Round, 'Round');
    RegisterMethod(@TScriptMath.RoundTo, 'RoundTo');
    RegisterMethod(@TScriptMath.DegToRad, 'DegToRad');
    RegisterMethod(@TScriptMath.RadToDeg, 'RadToDeg');
    RegisterMethod(@TScriptMath.DegNormalize, 'DegNormalize');
    RegisterMethod(@TScriptMath.InRange, 'InRange');
    RegisterMethod(@TScriptMath.EnsureRange, 'EnsureRange');
    RegisterMethod(@TScriptMath.Random, 'Random');
    RegisterMethod(@TScriptMath.Sincos, 'Sincos');
    RegisterPropertyHelper(@ScriptMathGetEHelper, nil, 'E');
    RegisterPropertyHelper(@ScriptMathGetPiHelper, nil, 'Pi');
  end;
end;

procedure TScriptMathAPI.RuntimeRegisterVariables(Exec: TPascalExec);
begin
  if FMath = nil then
    FMath := TScriptMath.Create;
  Exec.SetPointerToData('Math', @FMath, Exec.FindType(btClass));
end;

end.


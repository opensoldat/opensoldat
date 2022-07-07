{*******************************************************}
{                                                       }
{       CoreFunctions unit for OPENSOLDAT               }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit ScriptCoreFunctions;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  ScriptCore3Api,
  SysUtils;

type
  // TODO: GetTickCount, MaskCheck, RegExps, RGB, Round,
  // RoundTo, Sqrt(?), StrPos(?), StrReplace(?), Version, Regex class

  TCoreFunctionsAPI = class(TScriptCore3API)
  public
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;


implementation

uses
  Constants,
  Weapons,
  ScriptExceptions,
  math,
  RegExpr,
  Server,
  Util,
  Calc;

procedure SWriteLn(Text: string);
begin
  MainConsole.Console(Text, SERVER_MESSAGE_COLOR);
end;

function SArcTan(Num: Extended): Extended;
begin
  Result := ArcTan(Num);
end;

function SLn(Num: Extended): Extended;
begin
  Result := Ln(Num);
end;

function SLog10(Num: Extended): Extended;
begin
  Result := Log10(Num);
end;

function MyExp(Number: Extended): Extended;
begin
  Result := Exp(Number);
end;

function SRandom(Min, Max: Integer): Integer;
begin
  Result := RandomRange(Min, Max);
end;

function SRGB(R, G, B: Byte): Longword;
begin
  Result := (B or (G shl 8) or (R shl 16));
end;

function weap2obj(Style: Byte): Byte;
begin
  case Style of
    EAGLE_NUM:    Result := OBJECT_DESERT_EAGLE;
    MP5_NUM:      Result := OBJECT_HK_MP5;
    AK74_NUM:     Result := OBJECT_AK74;
    STEYRAUG_NUM: Result := OBJECT_STEYR_AUG;
    SPAS12_NUM:   Result := OBJECT_SPAS12;
    RUGER77_NUM:  Result := OBJECT_RUGER77;
    M79_NUM:      Result := OBJECT_M79;
    BARRETT_NUM:  Result := OBJECT_BARRET_M82A1;
    M249_NUM:     Result := OBJECT_MINIMI;
    MINIGUN_NUM:  Result := OBJECT_MINIGUN;
    COLT_NUM:     Result := OBJECT_USSOCOM;
    KNIFE_NUM:    Result := OBJECT_COMBAT_KNIFE;
    CHAINSAW_NUM: Result := OBJECT_CHAINSAW;
    LAW_NUM:      Result := OBJECT_LAW;
  else
    raise ScriptException.Create('Invalid weapon style: ' + IntToStr(Style));
  end;
end;

function menu2obj(Style: Byte): Byte;
begin
  case Style of
    EAGLE:    Result := OBJECT_DESERT_EAGLE;
    MP5:      Result := OBJECT_HK_MP5;
    AK74:     Result := OBJECT_AK74;
    STEYRAUG: Result := OBJECT_STEYR_AUG;
    SPAS12:   Result := OBJECT_SPAS12;
    RUGER77:  Result := OBJECT_RUGER77;
    M79:      Result := OBJECT_M79;
    BARRETT:  Result := OBJECT_BARRET_M82A1;
    M249:     Result := OBJECT_MINIMI;
    MINIGUN:  Result := OBJECT_MINIGUN;
    COLT:     Result := OBJECT_USSOCOM;
    KNIFE:    Result := OBJECT_COMBAT_KNIFE;
    CHAINSAW: Result := OBJECT_CHAINSAW;
    LAW:      Result := OBJECT_LAW;
  else
    raise ScriptException.Create('Invalid weapon menu style: ' + IntToStr(Style));
  end;
end;

function obj2weap(Style: Byte): Byte;
begin
  case Style of
    OBJECT_DESERT_EAGLE: Result := EAGLE_NUM;
    OBJECT_HK_MP5:       Result := MP5_NUM;
    OBJECT_AK74:         Result := AK74_NUM;
    OBJECT_STEYR_AUG:    Result := STEYRAUG_NUM;
    OBJECT_SPAS12:       Result := SPAS12_NUM;
    OBJECT_RUGER77:      Result := RUGER77_NUM;
    OBJECT_M79:          Result := M79_NUM;
    OBJECT_BARRET_M82A1: Result := BARRETT_NUM;
    OBJECT_MINIMI:       Result := M249_NUM;
    OBJECT_MINIGUN:      Result := MINIGUN_NUM;
    OBJECT_USSOCOM:      Result := COLT_NUM;
    OBJECT_COMBAT_KNIFE: Result := KNIFE_NUM;
    OBJECT_CHAINSAW:     Result := CHAINSAW_NUM;
    OBJECT_LAW:          Result := LAW_NUM;
  else
    raise ScriptException.Create('Invalid object style: ' + IntToStr(Style));
  end;
end;

function obj2menu(Style: Byte): Byte;
begin
  case Style of
    OBJECT_DESERT_EAGLE: Result := EAGLE;
    OBJECT_HK_MP5:       Result := MP5;
    OBJECT_AK74:         Result := AK74;
    OBJECT_STEYR_AUG:    Result := STEYRAUG;
    OBJECT_SPAS12:       Result := SPAS12;
    OBJECT_RUGER77:      Result := RUGER77;
    OBJECT_M79:          Result := M79;
    OBJECT_BARRET_M82A1: Result := BARRETT;
    OBJECT_MINIMI:       Result := M249;
    OBJECT_MINIGUN:      Result := MINIGUN;
    OBJECT_USSOCOM:      Result := COLT;
    OBJECT_COMBAT_KNIFE: Result := KNIFE;
    OBJECT_CHAINSAW:     Result := CHAINSAW;
    OBJECT_LAW:          Result := LAW;
  else
    raise ScriptException.Create('Invalid object style: ' + IntToStr(Style));
  end;
end;

function weap2menu(Style: Byte): Byte;
begin
  case Style of
    EAGLE_NUM:    Result := EAGLE;
    MP5_NUM:      Result := MP5;
    AK74_NUM:     Result := AK74;
    STEYRAUG_NUM: Result := STEYRAUG;
    SPAS12_NUM:   Result := SPAS12;
    RUGER77_NUM:  Result := RUGER77;
    M79_NUM:      Result := M79;
    BARRETT_NUM:  Result := BARRETT;
    M249_NUM:     Result := M249;
    MINIGUN_NUM:  Result := MINIGUN;
    COLT_NUM:     Result := COLT;
    KNIFE_NUM:    Result := KNIFE;
    CHAINSAW_NUM: Result := CHAINSAW;
    LAW_NUM:      Result := LAW;
    NOWEAPON_NUM: Result := NOWEAPON;
    else begin
      raise ScriptException.Create('Invalid weapon style: ' + IntToStr(Style));
    end;
  end;
end;

function menu2weap(Style: Byte): Byte;
begin
  case Style of
    EAGLE:    Result := EAGLE_NUM;
    MP5:      Result := MP5_NUM;
    AK74:     Result := AK74_NUM;
    STEYRAUG: Result := STEYRAUG_NUM;
    SPAS12:   Result := SPAS12_NUM;
    RUGER77:  Result := RUGER77_NUM;
    M79:      Result := M79_NUM;
    BARRETT:  Result := BARRETT_NUM;
    M249:     Result := M249_NUM;
    MINIGUN:  Result := MINIGUN_NUM;
    COLT:     Result := COLT_NUM;
    KNIFE:    Result := KNIFE_NUM;
    CHAINSAW: Result := CHAINSAW_NUM;
    LAW:      Result := LAW_NUM;
    else begin
      raise ScriptException.Create('Invalid weapon style: ' + IntToStr(Style));
    end;
  end;
end;

procedure TCoreFunctionsAPI.CompilerRegister(Compiler: TPascalCompiler);
begin
  Compiler.AddFunction('procedure WriteLn(Text: String)');
  Compiler.AddFunction('function StrToInt(Text: String): Integer');
  Compiler.AddFunction(
    'function iif(Condition: Boolean; IfTrue, IfFalse: Variant): Variant');
  Compiler.AddFunction('function MD5(A: string): string');
  Compiler.AddFunction('function ArcTan(A: Extended): Extended');
  Compiler.AddFunction('function Exp(A: Extended): Extended');
  Compiler.AddFunction('function Ln(A: Extended): Extended');
  Compiler.AddFunction('function Log10(A: Extended): Extended');
  Compiler.AddFunction('function LogN(A, B: Extended): Extended');
  Compiler.AddFunction('function Random(Min, Max: Integer): Integer');
  Compiler.AddFunction('function RGB(R, G, B: Byte): LongWord');
  Compiler.AddFunction('function Distance(X1, X2, Y1, Y2: Single): Single');
  Compiler.AddFunction('function ExecRegExpr(const ARegExpr, AInputStr: string): boolean');
  Compiler.AddFunction(
    'procedure SplitRegExpr(const ARegExpr, AInputStr: string; APieces: TStrings)');
  Compiler.AddFunction(
    'function ReplaceRegExpr(const ARegExpr, AInputStr, AReplaceStr: string; AUseSubstitution: boolean): string');
  Compiler.AddFunction('function QuoteRegExprMetaChars(const AStr: string): string');
  Compiler.AddFunction(
    'function RegExprSubExpressions (const ARegExpr: string; ASubExprs : TStrings; AExtendedSyntax: boolean): integer');
  Compiler.AddFunction('function FormatFloat(Const Format : String; Value : Extended) : String;');;
  Compiler.AddFunction('function weap2obj(Style: Byte): Byte;');
  Compiler.AddFunction('function menu2obj(Style: Byte): Byte;');
  Compiler.AddFunction('function obj2weap(Style: Byte): Byte;');
  Compiler.AddFunction('function obj2menu(Style: Byte): Byte;');
  Compiler.AddFunction('function weap2menu(Style: Byte): Byte;');
  Compiler.AddFunction('function menu2weap(Style: Byte): Byte;');
end;

procedure TCoreFunctionsAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  Exec.AddFunction(@SWriteLn, 'WriteLn');
  Exec.AddFunction(@StrToInt, 'StrToInt');
  Exec.AddFunction(@Iif, 'iif');
  Exec.AddFunction(@Md5StringHelper, 'MD5');
  Exec.AddFunction(@SArcTan, 'ArcTan');
  Exec.AddFunction(@MyExp, 'Exp');
  Exec.AddFunction(@SLn, 'Ln');
  Exec.AddFunction(@SLog10, 'Log10');
  Exec.AddFunction(@LogN, 'LogN');
  Exec.AddFunction(@SRandom, 'Random');
  Exec.AddFunction(@SRGB, 'RGB');
  Exec.AddFunction(@Distance, 'Distance');
  Exec.AddFunction(@ExecRegExpr, 'ExecRegExpr');
  Exec.AddFunction(@SplitRegExpr, 'SplitRegExpr');
  Exec.AddFunction(@ReplaceRegExpr, 'ReplaceRegExpr');
  Exec.AddFunction(@QuoteRegExprMetaChars, 'QuoteRegExprMetaChars');
  Exec.AddFunction(@RegExprSubExpressions, 'RegExprSubExpressions');
  Exec.AddFunction(@FormatFloat, 'FormatFloat');
  Exec.AddFunction(@weap2obj, 'weap2obj');
  Exec.AddFunction(@menu2obj, 'menu2obj');
  Exec.AddFunction(@obj2weap, 'obj2weap');
  Exec.AddFunction(@obj2menu, 'obj2menu');
  Exec.AddFunction(@weap2menu, 'weap2menu');
  Exec.AddFunction(@menu2weap, 'menu2weap');
end;

end.

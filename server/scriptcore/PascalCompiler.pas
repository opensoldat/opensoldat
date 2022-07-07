{*******************************************************}
{                                                       }
{       PascalCompiler unit for OPENSOLDAT              }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit PascalCompiler;

{$IFDEF FPC}{$mode delphi}{$INTERFACES CORBA}{$ENDIF}

interface

uses
  // general
  Classes, SysUtils,
  // pascal script
  uPSC_dll, uPSCompiler, uPSC_std, uPSPreProcessor, uPSUtils;

// just so that api units doesn't have to import uPSCompiler unit
const
  iptRW = uPSCompiler.iptRW;
  iptR = uPSCompiler.iptR;
  iptW = uPSCompiler.iptW;

type

  // same as above, import buster
  TPascalCompiletimeClass = uPSCompiler.TPSCompileTimeClass;
  TPascalInterface = uPSCompiler.TPSInterface;

  TPascalCompiler = class(TObject)
  private
    FMainFileName: string;
    FSource: string;
    FCompiled: Boolean;
    FSupportDLLs: Boolean;
    FSearchPaths: TStringList;
    FDefines: TStringList;
    FPreProcessor: TPSPreProcessor;
    FCompiler: TPSPascalCompiler;
    FApi: TList;
    function GetByteCode: string;
    function GetDebugCode: string;
    function FindFile(var Name: string): Boolean;
    procedure SetMainFile(Name, Source: string);
  public
    constructor Create(Dir, MainFile: string);
    destructor Destroy; override;
    function Compile: Boolean;
    function GetMessages: TStringList;
    procedure AddSearchPath(Path: string);
    function AddClass(InheritsFrom: TPascalCompiletimeClass;
      AClass: TClass): TPascalCompiletimeClass; overload;
    function AddClass(InheritsFrom: TPascalCompiletimeClass;
      AClass: string): TPascalCompiletimeClass; overload;
    function FindClass(ClassName: string): TPascalCompiletimeClass;
    function AddInterface(InheritedFrom: TPSInterface; Guid: TGuid;
      const Name: tbtString): TPSInterface;
    function FindInterface(const Name: tbtString): TPSInterface;
    function AddFunction(Declaration: string): TPSRegProc;
    function AddType(Name: string; BaseType: TPSBaseType): TPSType; overload;
    function AddType(Name, Declaration: string): TPSType; overload;
    function AddConstant(Name: string; AType: TPSType): TPSConstant;
      overload;
    function AddConstant(Name, AType: string): TPSConstant; overload;
    function AddVariable(Name: string; AType: TPSType): TPSVar; overload;
    function AddVariable(Name, AType: string): TPSVar; overload;
    function AddPtrVariable(Name: string; AType: TPSType): TPSVar; overload;
    function AddPtrVariable(Name, AType: string): TPSVar; overload;
    property Bytecode: string read GetByteCode;
    property Debugcode: string read GetDebugCode;
    // TODO: think if this shouldn't be read only
    property Defines: TStringList read FDefines write FDefines;
    property SupportDLLs: Boolean read FSupportDLLs write FSupportDLLs;
    // TODO: think if this shouldn't be read only
    property Api: TList read FApi write FApi;
    property Compiler: TPSPascalCompiler read FCompiler;
    // property AllowNoBegin: Boolean read FCompiler.AllowNoBegin
    // write FCompiler.AllowNoBegin;
    // property AllowNoEnd: Boolean read FCompiler.AllowNoEnd
    // write FCompiler.AllowNoEnd;
  end;

  ICompilerAPI = interface
    ['{b82ef0d6-a05f-4f68-9d18-ab15f9073c8f}']
    procedure CompilerRegister(Compiler: TPascalCompiler);
  end;

implementation


// BUG: this should also search in searchpaths
// and be relative to calling file if possible
function OnNeedFile(Sender: TPSPreProcessor; const CallingFilename: TbtString;
  var FileName, Output: TbtString): Boolean;
var
  SourceFile: TStringList;
  PascalCompiler: TPascalCompiler;
  Path: string;
begin
  ExtractFilePath(CallingFilename);
  PascalCompiler := TPascalCompiler(Sender.ID);
  Path := FileName;
  Result := PascalCompiler.FindFile(Path);
  if Result then
  begin
    SourceFile := TStringList.Create;
    SourceFile.LoadFromFile(Path);
    Output := SourceFile.GetText;
    SourceFile.Free;
  end;
end;

function OnUses(Compiler: TPSPascalCompiler; const Name: TbtString): Boolean;
var
  SourceFile: TStringList;
  Path: string;
  PascalCompiler: TPascalCompiler;
  I: Shortint;
  ApiObj: ICompilerAPI;
begin
  PascalCompiler := TPascalCompiler(Compiler.ID);
  // if calling for SYSTEM unit, register all the sutff
  if Name = 'SYSTEM' then
  begin
    if PascalCompiler.FSupportDLLs then
      RegisterDll_Compiletime(Compiler);
    SIRegisterTObject(Compiler);
    if PascalCompiler.FApi <> nil then
      for I := 0 to PascalCompiler.FApi.Count - 1 do
      begin
        TInterfacedObject(PascalCompiler.FApi[I]).GetInterface(ICompilerAPI, ApiObj);
        ApiObj.CompilerRegister(PascalCompiler);
      end;
    Result := True;
    Exit;
  end;
  Path := Name + '.pas';
  Result := PascalCompiler.FindFile(Path);
  if Result then
  begin
    SourceFile := TStringList.Create;
    SourceFile.LoadFromFile(Path);
    PascalCompiler.SetMainFile(Name, SourceFile.Text);
    SourceFile.Free;
    //PascalCompiler.FPreProcessor.Defines.Assign(PascalCompiler.FDefines);
    // might throw exception, since pascalscript doesn't catch it,
    // it's the callers responsibility to catch when calling
    // TPascalCompiler.Compile
    Result := PascalCompiler.Compile;
    // no honey, we're not done yet
    PascalCompiler.FCompiled := False;
  end;
end;

procedure CompTranslateLineInfo(Sender: TPSPascalCompiler;
  var Pos, Row, Col: Cardinal; var Name: string);
var
  Res: TPSLineInfoResults;
begin
  Res := Default(TPSLineInfoResults);
  if TPascalCompiler(Sender.ID).FPreProcessor.CurrentLineInfo.GetLineInfo(
    {$IFNDEF LEGACY_PASCALSCRIPT}
    Name,
    {$ENDIF}
    Pos, Res) then
  begin
    Pos := Res.Pos;
    Row := Res.Row;
    Col := Res.Col;
    Name := Res.Name;
  end;
end;

procedure TPascalCompiler.SetMainFile(Name, Source: string);
begin
  Self.FMainFileName := Name;
  Self.FSource := Source;
end;

constructor TPascalCompiler.Create(Dir, MainFile: string);
var
  SourceFile: TStringList;
begin
  SourceFile := TStringList.Create;
  SourceFile.LoadFromFile(Dir + MainFile);
  Self.FMainFileName := MainFile;
  Self.FSearchPaths := TStringList.Create;
  Self.FSource := SourceFile.Text;
  Self.FDefines := TStringList.Create;
  Self.FPreProcessor := TPSPreProcessor.Create;
  Self.FPreProcessor.ID := Self;
  Self.FPreProcessor.OnNeedFile := OnNeedFile;
  Self.FCompiler := TPSPascalCompiler.Create;
  Self.FCompiler.ID := Self;
  Self.FCompiled := False;
  Self.FCompiler.AllowNoBegin := True;
  Self.FCompiler.AllowNoEnd := True;
  Self.FCompiler.AllowUnit := True;
  Self.FCompiler.OnUses := OnUses;
  Self.FCompiler.OnTranslateLineInfo := CompTranslateLineInfo;
  SourceFile.Free;
end;

destructor TPascalCompiler.Destroy;
begin
  Self.FSearchPaths.Free;
  Self.FDefines.Free;
  Self.FPreProcessor.Free;
  Self.FCompiler.Free;
end;

function TPascalCompiler.GetByteCode: string;
begin
  Result := '';
  if not Self.FCompiled then
    if not Self.Compile then
      Exit;
  Self.FCompiler.GetOutput(Result);
end;

function TPascalCompiler.GetDebugCode: string;
begin
  Result := '';
  if not Self.FCompiled then
    if not Self.Compile then
      Exit;
  Self.FCompiler.GetDebugOutput(Result);
end;

function TPascalCompiler.GetMessages: TStringList;
var
  I: Shortint;
begin
  Result := TStringList.Create;
  for I := 0 to Self.FCompiler.MsgCount - 1 do
    Result.Add(Self.FCompiler.Msg[I].MessageToString);
end;

function TPascalCompiler.FindFile(var Name: string): Boolean;
var
  I: Shortint;
  LName, Path: string;
begin
  LName := FastLowerCase(Name);
  Result := False;
  for I := 0 to Self.FSearchPaths.Count - 1 do
  begin
    Path := ExpandFileName(Self.FSearchPaths[I] + LName);
    Result := FileExists(Path) and (Copy(Path, 1, Length(Self.FSearchPaths[I])) = Self.FSearchPaths[I]);
    if not Result then
    begin
      Continue;
    end;
    Name := self.FSearchPaths[I] + LName;
    Exit;
  end;
end;

function TPascalCompiler.Compile: Boolean;
var
  Data: string = '';
begin
  Result := False;
  if Self.FCompiled then
  begin
    Result := True;
    Exit;
  end;
  Self.FPreProcessor.Defines.AddStrings(Self.FDefines);
  Self.FPreProcessor.MainFileName := Self.FMainFileName;
  Self.FPreProcessor.MainFile := Self.FSource;
  Self.FPreProcessor.PreProcess(Self.FMainFileName, Data);
  if Self.FCompiler.Compile(Data) then
  begin
    Self.FPreProcessor.AdjustMessages(Self.FCompiler);
    Self.FCompiled := True;
    Result := True;
  end;
end;

procedure TPascalCompiler.AddSearchPath(Path: string);
begin
  Self.FSearchPaths.Add(IncludeTrailingPathDelimiter(ExpandFileName(Path)));
end;

function TPascalCompiler.AddClass(InheritsFrom: TPascalCompiletimeClass;
  AClass: TClass): TPascalCompiletimeClass;
begin
  Result := Self.FCompiler.AddClass(InheritsFrom, AClass);
end;

function TPascalCompiler.AddClass(InheritsFrom: TPascalCompiletimeClass;
  AClass: string): TPascalCompiletimeClass;
begin
  Result := Self.FCompiler.AddClassN(InheritsFrom, AClass);
end;

function TPascalCompiler.FindClass(ClassName: string): TPascalCompiletimeClass;
begin
  Result := Self.FCompiler.FindClass(ClassName);
end;

function TPascalCompiler.AddInterface(InheritedFrom: TPascalInterface;
  Guid: TGuid; const Name: tbtString): TPascalInterface;
begin
  Result := Self.FCompiler.AddInterface(InheritedFrom, Guid, Name);
end;

function TPascalCompiler.FindInterface(const Name: tbtString): TPascalInterface;
begin
  Result := Self.FCompiler.FindInterface(Name);
end;

function TPascalCompiler.AddFunction(Declaration: string): TPSRegProc;
begin
  Result := Self.FCompiler.AddDelphiFunction(Declaration);
end;

function TPascalCompiler.AddType(Name: string; BaseType: TPSBaseType): TPSType;
begin
  Result := Self.FCompiler.AddType(Name, BaseType);
end;

function TPascalCompiler.AddType(Name, Declaration: string): TPSType;
begin
  Result := Self.FCompiler.AddTypeS(Name, Declaration);
end;

function TPascalCompiler.AddConstant(Name: string; AType: TPSType): TPSConstant;
begin
  Result := Self.FCompiler.AddConstant(Name, AType);
end;

function TPascalCompiler.AddConstant(Name, AType: string): TPSConstant;
begin
  Result := Self.FCompiler.AddConstantN(Name, AType);
end;

function TPascalCompiler.AddVariable(Name: string; AType: TPSType): TPSVar;
begin
  Result := Self.FCompiler.AddUsedVariable(Name, AType);
end;

function TPascalCompiler.AddVariable(Name, AType: string): TPSVar;
begin
  Result := Self.FCompiler.AddUsedVariableN(Name, AType);
end;

function TPascalCompiler.AddPtrVariable(Name: string; AType: TPSType): TPSVar;
begin
  Result := Self.FCompiler.AddUsedPtrVariable(Name, AType);
  if Result <> nil then
    Result.ExportName := Result.Name;
end;

function TPascalCompiler.AddPtrVariable(Name, AType: string): TPSVar;
begin
  Result := Self.FCompiler.AddUsedPtrVariableN(Name, AType);
end;

end.

{
@abstract(Component wrapper for IFPS3 compiler and executer)
A component wrapper for IFPS3, including debugging support.

}
{$I PascalScript.inc}

unit uPSComponentExt;

interface

uses
  {$IFNDEF LINUX} Windows, {$ENDIF} SysUtils, Classes, uPSRuntime, uPSDebugger, uPSUtils, uPSComponent,
  contnrs, uPSCompiler, uPSC_dll, uPSR_dll, uPSPreProcessor, typInfo;

const
  {alias to @link(ifps3.cdRegister)}
  cdRegister = uPSRuntime.cdRegister;
  {alias to @link(ifps3.cdPascal)}
  cdPascal = uPSRuntime.cdPascal;
  { alias to ifps3.cdCdecl }
  CdCdecl = uPSRuntime.CdCdecl;
  {alias to @link(ifps3.cdStdcall)}
  CdStdCall = uPSRuntime.CdStdCall;

type
  {Alias to @link(ifps3.TPSCallingConvention)}
  TDelphiCallingConvention = uPSRuntime.TPSCallingConvention;
  {Alias to @link(ifps3.TPSRuntimeClassImporter)}
  TPSRuntimeClassImporter = uPSRuntime.TPSRuntimeClassImporter;

  TPSScriptExtension = class;

  {Base class for all plugins for the component}
  TPSOnCompCleanup = Function (Sender: TObject; aComp: TPSPascalCompiler):Boolean of object;
  TPSOnInsertProcedure = Procedure (Sender: TObject; aProc: tbtstring; OnTop: Boolean) of object;
  TPSOnException = procedure (Sender: TPSExec; ExError: TPSError; const ExParam: tbtstring;
                              ExObject: TObject; ProcNo, Position: Cardinal) of object;

  TMethodList = class;
  TProcObj = Class
    private
      FName     : tbtstring;
      fOwner   : TMethodList;
    procedure SetName(const Value: tbtstring);
  public
    ProcType : TStringList;
    Method : TMethod;
    constructor create(aOwner: TMethodList);
    destructor Destroy; override;
    property Name: tbtstring read FName write SetName;
  end;

  TMethodObj = Class
    Instance  : TPersistent;
    PropName  : tbtstring;
    ProcName  : tbtstring;
  end;

  TMethodList = class
  private
    fOwner     : TPSScriptExtension;
    fProcList  : TObjectList;
    fEventList : TObjectList;
    function  GetObject(Index: Integer): TMethodObj; virtual;
    function  GetProcObj(Index: Integer): TProcObj;
    function  GetMethodName(Instance: TObject; PropName: tbtstring): tbtstring;
    procedure SetMethodName(Instance: TObject; PropName: tbtstring; const Value: tbtstring);
    procedure CreateProc(ProcName: tbtstring; aPropType: TTypeData);
  public
    constructor create(aOwner: TPSScriptExtension);
    destructor Destroy; override;
    function methodIndexOf(Instance: TObject; PropName: tbtstring):Integer;
    Function ProcIndexOf(Name: tbtstring): Integer;
    Procedure ListEventsName(EventType:tbtstring; List : TStrings);

    Procedure AddProcedure(ProcName, ProcType:tbtstring);
    procedure InsertMethod(NewProc: tbtstring; OnTop: Boolean = false);

    Procedure FillMethods;
    procedure ClearProcList;
    Procedure ClearAll;
    function ProcCount :Integer;
    Function MethodCount :Integer;
    property Procs[Index: Integer]: TProcObj read GetProcObj ;
    property Methods[Index: Integer]: TMethodObj read GetObject;
    property ProcName[Instance: TObject; PropName:tbtstring]: tbtstring read GetMethodName write SetMethodName;
  end;

  TPSScriptExtension = class(TPSScriptDebugger)
  private
    FOnBeforeCleanUp: TPSOnCompCleanup;
    FMethodList : TMethodList;
    FOnInsertMethod: TPSOnInsertProcedure;
    FNeedCompiling :Boolean;
    FOnScriptChance: TNotifyEvent;
    FOnException: TPSOnException;

    fItems, fInserts: TStrings;
    fScriptPos : Cardinal;
    fObjectNest: tbtstring;

    Procedure GetCodeProps ;
    function GetProcName(Instance: TObject; PropName: tbtstring): tbtstring;
    procedure SetProcName(Instance: TObject; PropName: tbtstring; const Value: tbtstring);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoVerifyProc(Sender: TPSScript; Proc: TPSInternalProcedure;
                           const Decl: tbtstring; var Error: Boolean); reintroduce;
    Function  DoBeforeCleanup(Sender: TObject; aComp: TPSPascalCompiler):Boolean;
    procedure DoScriptChance(sender:TObject);


  public
    {Create an instance of the CompExec component}
    constructor Create(AOwner: TComponent); override;
    {Destroy the CompExec component}
    destructor Destroy; override;

    function Compile: Boolean; Override;
    function Execute: Boolean; Override;
    { Create a list of all var's, const's, Type's and functions }
    Procedure GetValueDefs(aItems, aInserts: TStrings; Const aObjectNest: tbtstring=''; aScriptPos: Integer = 0);

    {Compile the source only when the source is modified}
    procedure CompileIfNeeded;
    {Is the source modified}
    Property NeedCompiling : Boolean read FNeedCompiling;

    {Fills all function in the script to there connected Events.
     This is called automatic after a succesfull Compilition}
    Procedure FillMethods;

    {Removes all events from the Objects Fills all function in the script to there connected Events.
     This function is automatic called before a Compilition}
    procedure ClearProcList;
    Procedure RemoveObjEvents(Obj: TObject);

    {This property helps you set the events that must becalled from within the script
     Instance is the object where the Propname must be set.
     You need te create the function yopur self in the script.
     When the new Procname dose not exists in the script, it is automatic created for you.}
    property ProcName[Instance: TObject; PropName:tbtstring]: tbtstring read GetProcName write SetProcName;
    property MethodList : TMethodList read FMethodList;

  published

    property OnBeforeCleanUp: TPSOnCompCleanup read FOnBeforeCleanUp write FOnBeforeCleanUp; //<NVDS>
    property OnInsertMethod : TPSOnInsertProcedure read FOnInsertMethod write FOnInsertMethod;
    Property OnScriptChance : TNotifyEvent read FOnScriptChance write fOnScriptChance;
    property OnException    : TPSOnException read FOnException write FOnException;
  end;


implementation

resourcestring
  sMissingEndStatment = 'Missing some ''End'' statments';


function CompExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: tbtstring): Boolean;
begin
  TPSScriptExtension(Sender.ID).DoVerifyProc(Sender.Id, Proc, ProcDecl, Result);
  Result := not Result;
end;

Function BeforeCleanup(Sender: TPSPascalCompiler):Boolean;
begin
  result := TPSScriptExtension(Sender.ID).DoBeforeCleanUp(Sender.ID,Sender);
end;

procedure CEException(Sender: TPSExec; ExError: TIFError; const ExParam: tbtstring; ExObject: TObject; ProcNo, Position: Cardinal);
begin
  if @TPSScriptExtension(Sender.ID).FOnException <> nil then
    TPSScriptExtension(Sender.ID).FOnException(Sender, ExError, ExParam, ExObject, ProcNo, Position);
end;

{ TPSScriptExtension }

function TPSScriptExtension.Compile: Boolean;
begin
  ClearProcList;

  result := inherited Compile;
  if result then FillMethods;

  
 FNeedCompiling := not result;
end;

constructor TPSScriptExtension.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Comp.OnBeforeCleanup := BeforeCleanup;
  Comp.OnExportCheck   := CompExportCheck;
  Exec.OnException     := CEException;

  TStringList(script).OnChange := DoScriptChance;
  FMethodList := TMethodList.create(Self);
  FNeedCompiling := True;
end;

destructor TPSScriptExtension.Destroy;
begin
  FMethodList.Free;
  FMethodList := nil;
  inherited Destroy;
end;

procedure TPSScriptExtension.DoVerifyProc(Sender: TPSScript;
  Proc: TPSInternalProcedure; const Decl: tbtstring; var Error: Boolean);
var
  n{,m,p} : Integer;
  tstType : TPSProceduralType;
begin
  Error := False;
  for n := 0 to sender.comp.GetTypeCount -1 do begin
    If comp.GetType(n) is TPSProceduralType then begin
      tstType := comp.GetType(n) as TPSProceduralType;
      If tstType.ProcDef.Same(Proc.Decl) then begin
        MethodList.addprocedure(Proc.OriginalName, tstType.Name);
//        Proc. aExport := etExportDecl;
      end;
    end;
  end;
  if assigned(OnVerifyProc) then
  begin
    onVerifyProc(Sender, Proc, Decl, Error);
  end;
end;

type
  TMyPascalCompiler = class(TPSPascalCompiler);
const
  sIFPSParameterMode : array [pmIn..pmInOut] of tbtstring = ('','\style{+B}out\style{-B} ','\style{+B}Var\style{-B} ');

Procedure TPSScriptExtension.GetCodeProps;

  Function existsItem(aName:tbtstring):Boolean;
  Begin
    result := FInserts.indexof(aName)<> -1;
  end;

  Procedure addListItem(aType, aName:tbtstring; aDef:tbtstring='');
  var
    x : LongInt;
  begin
    If not ((aName ='') or (aName[1]='!')) then begin
      x := FInserts.Add(aName);
      fItems.Insert(x, format('%s \column{}\style{+B}%s\style{-B} %s',[aType, aName, aDef]));
    end;
  end;

  procedure Getdecl(decl : TPSParametersDecl; var T,v :tbtstring);
  var
    m : Integer;
  begin
    v := '';
    for m := 0 to Decl.ParamCount-1 do begin
      v := V +';'+sIFPSParameterMode[Decl.Params[m].Mode]+
              Decl.Params[m].OrgName;
      if Decl.Params[m].aType <> nil then
        v := v +':'+ Decl.Params[m].aType.OriginalName;
    end;
    delete(v,1,1);
    If v <> '' then v := '('+ v +')';
    if Decl.Result<>nil then begin
      v := v +':'+ Decl.Result.OriginalName;
      t := 'Function';
    end else t := 'Procedure';

  end;

  Function getTypeDef(xr: TPSType; aZoek:tbtstring = ''):Boolean; forward;

  Function getClassDef(xc: TPSCompileTimeClass; aZoek:tbtstring = ''):Boolean;
  var
    Show : Boolean;
    Zoek,bZoek : tbtstring;
    tci : TPSDelphiClassItem;
    n : Integer;
    T,v : tbtstring;

  begin
    Show := aZoek='';
    Zoek := aZoek;
    If Pos('.',aZoek)>0 then begin
      Zoek  := copy(aZoek, 1 ,Pos('.',aZoek)-1);
      bZoek := copy(aZoek, Pos('.',aZoek)+1, 999);
    end else bZoek := '';

    result := (xc <> nil) and Show;
    if XC<> nil then begin
      For n := 0 to xc.Count-1 do begin
        tci := xc.Items[n];
        If (tci = nil) or  existsItem(tci.OrgName) then continue;
        if tci is TPSDelphiClassItemConstructor then begin
          Getdecl(tci.decl, T, V);
          If Show then addListItem('constructor',tci.OrgName, v);
        end else
        if tci is TPSDelphiClassItemMethod then begin
          If Show then begin
            Getdecl(tci.decl, T, V);
            addListItem(T,tci.OrgName, v)
          end else
          If (tci.decl.Result <> nil) and (tci.Name = Zoek) then
            result := getTypeDef(tci.decl.Result, bZoek);
        end else
        if tci is TPSDelphiClassItemProperty then begin
          If Show then begin
            t := '';
            If tci.Decl.Result<> nil then t := ': '+ tci.Decl.Result.OriginalName;
            addListItem('Property',tci.OrgName, t);
          end else
          If (tci.decl.Result <> nil) and (tci.Name = Zoek) then
            result := getTypeDef(tci.decl.Result, bZoek);
        end;
        If result and not show then exit;
      end;
      result := getClassDef(XC.ClassInheritsFrom, aZoek) or result;
    end;
  end;

  Function getTypeDef(xr: TPSType; aZoek:tbtstring = ''):Boolean;
  var
    Show : Boolean;
    Zoek : tbtstring;
    xri : PIFPSRecordFieldTypeDef;
    n : Integer;
  begin
    Show := aZoek='';
    result := (xr <> nil) and Show;
    if xr <> nil then begin
      If xr is TPSRecordType then begin
        Zoek := aZoek;
        If Pos('.',aZoek)>0 then begin
          Zoek  := copy(aZoek, 1 ,Pos('.',aZoek)-1);
          aZoek := copy(aZoek, Pos('.',aZoek)+1, 999);
        end else aZoek := '';
        for n := 0 to (xr as TPSRecordType).RecValCount-1 do begin
          xri := (xr as TPSRecordType).RecVal(n);
          If Show then begin
            addListItem('var',xri.FieldOrgName,xri.aType.OriginalName)
          end else
          If (xri.aType <> nil) and (xri.FieldName = Zoek) then
            result := getTypeDef(xri.aType, aZoek);
        end;
      end else
      If (xr is TPSClassType) then begin
        result := getClassDef((xr as TPSClassType).Cl, aZoek)
      end else
        result := False;
    end;
  end;

  Function FindVarProc(aVarName:tbtstring; aZoek : tbtstring= ''):Boolean;
  var
//    cv   : tbtstring;
    hh, h, i : Longint;
    proc : TPSProcedure;
    ip   : TPSInternalProcedure;
    ipv  : PIFPSProcVar;
    ipp  : TPSParameterDecl;
//    t    : tbtstring;
  begin
    Hh := MakeHash(aVarName);
    result := False;
    If FScriptPos =0 then exit;
    for i := Comp.GetProcCount -1 downto 0 do begin
      Proc := Comp.GetProc(i);
      If (Proc.ClassType = TPSInternalProcedure) and
         ((Proc as TPSInternalProcedure).DeclarePos < FScriptPos) then begin
        ip := Proc as TPSInternalProcedure;
        for h := 0 to ip.ProcVars.Count-1 do begin
          ipv := PIFPSProcVar(ip.ProcVars[h]);
          If aVarName = '' then begin
            addListItem('var',ipv.OrgName, ': '+ipv.AType.OriginalName);
          end else
          If (ipv.NameHash = HH) and (ipv.Name = aVarName) then begin
            result := getTypeDef(ipv.aType, aZoek);
            exit;
          end;
        end;
        for h := 0 to ip.Decl.ParamCount-1 do begin
          ipp := TPSParameterDecl(ip.Decl.Params[h]);
          If aVarName = '' then begin
            addListItem('var',ipp.OrgName, ': '+ipp.aType.OriginalName);
          end else
          If {(ipp.Hash = HH) and} (ipp.Name = aVarName) then begin
            result := getTypeDef(ipp.aType, aZoek);
            exit;
          end;
        end;
      end;
    end;
  end;

  Function FindVarFunctType(aProcName:tbtstring): Boolean;
  var
    cv : tbtstring;
    h, i : Longint;
    proc : TPSProcedure;
    xr : TPSRegProc;
//    t  : tbtstring;
  begin
    cv := aProcName;
    If Pos('.',aProcName)>0 then begin
      cv := copy(aProcName, 1 ,Pos('.',aProcName)-1);
      aProcName := copy(aProcName, Pos('.',aProcName)+1, 999);
    end else aProcName := '';
    H := MakeHash(Cv);
//    Result := False;
    for i :=0 to Comp.GetVarCount -1 do begin
      if (Comp.GetVar(I).NameHash = H) and (Comp.GetVar(I).Name = CV) then begin
        Result := getTypeDef(Comp.GetVar(I).aType, aProcName);
        Exit;
      end;
    end;
   for i :=0 to Comp.GetTypeCount -1 do begin
    if (Comp.GetType(I).NameHash = H) and (Comp.GetType(I).Name = CV) then begin
      Result := getTypeDef(Comp.GetType(I), aProcName);
      Exit;
    end;
   end;
    result := FindVarProc(cv, aProcName);
    If result then exit;
    for i :=0 to Comp.GetProcCount -1 do begin
      Proc := Comp.GetProc(i);
      If Proc.ClassType = TPSInternalProcedure then begin
        if ((Proc as TPSInternalProcedure).NameHash = H) and
            ((Proc as TPSInternalProcedure).Name = CV) then begin
          Result := getTypeDef((Proc as TPSInternalProcedure).Decl.Result, aProcName);
          exit;
        end;
      end;
    end;
    with TMyPascalCompiler(Comp) do begin
      for i := 0 to FRegProcs.Count-1 do begin
        xr := FRegProcs[i];
        if (xr.NameHash = H) and  (xr.Name = CV) then begin
          result := getTypeDef(xr.Decl.Result, aProcName);
          exit;
        end;
      end;
    end;
  end;

Var
  n : Integer;
  s, t, v : tbtstring;
  proc : TPSProcedure;
  xr : TPSRegProc;

begin
  If (fItems = nil) or (fInserts = Nil) then exit;
  fItems.BeginUpdate;
  fInserts.BeginUpdate;
  tStringList(fInserts).Sorted := true;
  tStringList(fInserts).Duplicates := dupAccept;
  try
    fInserts.Clear;
    fItems.Clear;

    If (FObjectNest <> '') then begin
      FindVarFunctType(FastUpperCase(FObjectNest));
      exit;
    end;

    for n := 0 to Comp.GetTypeCount-1 do begin
      addListItem('Type',Comp.GetType(n).OriginalName);
    end;
    for n := 0 to Comp.GetVarCount-1 do begin
      addListItem('var',Comp.GetVar(n).OrgName, ': '+Comp.Getvar(n).aType.OriginalName);
    end;
    with TMyPascalCompiler(Comp) do begin
      for n := 0 to FConstants.Count-1 do begin
        addListItem('const', TPSConstant(FConstants[n]).OrgName );
      end;
      for n := 0 to FRegProcs.Count-1 do begin
        xr := FRegProcs[n];
        Getdecl(xr.decl, T, v);
        addListItem(t,xr.OrgName, v );
      end;
    end;
    FindVarProc('');
    for n := 0 to Comp.GetProcCount-1 do begin
      s := '';
      proc := Comp.GetProc(n);
      If Proc.ClassType = TPSInternalProcedure then begin
        s := (Proc as TPSInternalProcedure).OriginalName;
        Getdecl((Proc as TPSInternalProcedure).decl, T, v);
      end;
      If s <> '' then begin
        addListItem(t,s, v );
      end;
    end;
  Finally
    fInserts.EndUpdate;
    fItems.EndUpdate;
  end;
end;

procedure TPSScriptExtension.GetValueDefs(aItems, aInserts: TStrings; const aObjectNest: tbtstring; aScriptPos: Integer);
begin
  fItems      := aItems;
  fInserts    := aInserts;
  FScriptPos  := aScriptPos;
  fObjectNest := aObjectNest;
  Try
    compile;
  finally
    fItems      := Nil;
    fInserts    := Nil;
    FScriptPos  := 0;
    fObjectNest := '';
  end;
end;

function TPSScriptExtension.DoBeforeCleanup(Sender: TObject;
  aComp: TPSPascalCompiler): Boolean;
begin
  result := true;
  If fItems <> nil then GetCodeProps;
  If @FOnBeforeCleanUp<> nil then
    result := FOnBeforeCleanUp(Sender, aComp);
end;

function TPSScriptExtension.Execute: Boolean;
begin
  CompileIfNeeded;
  MethodList.FillMethods;
  result := inherited Execute;
end;


procedure TPSScriptExtension.DoScriptChance(sender: TObject);
begin
  FNeedCompiling := True;
  self.ClearProcList;
  If @FOnScriptChance <> NIL then
    FOnScriptChance(sender);
end;

procedure TPSScriptExtension.CompileIfNeeded;
begin
  if FNeedCompiling then begin
    Compile;
  end;
end;

procedure TPSScriptExtension.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  If Operation = opRemove then begin
    if MethodList <> nil then
      MethodList.SetMethodName(aComponent,'','');
  end;
end;

function TPSScriptExtension.GetProcName(Instance: TObject; PropName: tbtstring): tbtstring;
begin
  Result := MethodList.ProcName[Instance, Propname];
end;

procedure TPSScriptExtension.SetProcName(Instance: TObject; PropName: tbtstring; const Value: tbtstring);
begin
  MethodList.ProcName[Instance, Propname] := Value;
end;

procedure TPSScriptExtension.ClearProcList;
begin
  MethodList.ClearProcList;
end;

procedure TPSScriptExtension.RemoveObjEvents(Obj: TObject);
begin
  MethodList.SetMethodName(Obj, '', '');
end;

procedure TPSScriptExtension.FillMethods;
begin
  MethodList.FillMethods;
end;

{ TMethodList }

procedure TMethodList.AddProcedure(ProcName, ProcType: tbtstring);
var
  po : TProcObj;
  x,y  : Integer;

begin
  ProcType := Uppercase(ProcType);
  x := ProcIndexOf(ProcName);
  if x <> -1 then begin
    y := Procs[x].ProcType.IndexOf(ProcType);
    If y = -1 then TProcObj(fProcList.Items[x]).ProcType.add(ProcType);
  end else begin
    po := TProcObj.create(self);
    po.Name := ProcName;
    po.ProcType.add(ProcType);
    fProcList.add(po);
  end
end;

procedure TMethodList.ClearProcList;
begin
  fProcList.Clear;
end;

constructor TMethodList.create(aOwner: TPSScriptExtension);
begin
  inherited create;
  fOwner     := aOwner;
  fProcList  := TObjectList.create(true);
  fEventList := TObjectList.create(true);
end;

procedure TMethodList.CreateProc(ProcName:tbtstring; aPropType: TTypeData);
var
  newProc: tbtstring;
  P: PByte;
  i: Integer;
  pf : TParamFlags;

  {$IFDEF FPC}
  // mh: TParamFlags(P^) doesn't compile in FPC, this function will "fix" it.
  // yes it's ugly, but I don't know an other way to fix it
  function GetParamFlags(P: Byte): TParamFlags;
  begin
	result := [];
	if (Ord(pfVar) and P <> 0) then Include(result, pfVar);
	if (Ord(pfConst) and P <> 0) then Include(result, pfConst);
	if (Ord(pfArray) and P <> 0) then Include(result, pfArray);
	if (Ord(pfAddress) and P <> 0) then Include(result, pfAddress);
	if (Ord(pfReference) and P <> 0) then Include(result, pfReference);
	if (Ord(pfOut) and P <> 0) then Include(result, pfOut);
  end;
  {$ENDIF}

begin
  WITH aPropType do begin
    if MethodKind=mkProcedure then NewProc:='procedure '
                              else NewProc:='function ';
    NewProc:=NewProc + ProcName+'(';
    P:=PByte(@ParamList);
    for i:=0 to Pred(ParamCount) do
    begin
      {$IFDEF FPC}
      pf:=GetParamFlags(P^);
      {$ELSE}
      pf:=TParamFlags(P^);
      {$ENDIF}
      if pfVar in pf then NewProc:=NewProc+'var ';
      if pfConst in pf then NewProc:=NewProc+'const ';
      Inc(P);
      NewProc:=NewProc +PShortString(P)^ +' : ';
      Inc(P,Succ(P^));
      if pfArray in pf then NewProc:=NewProc+'array of ';
      NewProc := NewProc + PShortString(P)^;
      Inc(P,Succ(P^));
      If i < Pred(ParamCount) then NewProc := NewProc + '; ';
    end;
    NewProc := NewProc +')' ;
    if (MethodKind=mkFunction) then
      NewProc := NewProc +':'+ PShortString(P)^;
    NewProc:=NewProc+';'^m^j
                    +'Begin'^m^j^m^j
                    +'End;'^m^j;
    If @fowner.FOnInsertMethod <> nil then begin
      fowner.FOnInsertMethod(fOwner, NewProc, false);
    end else begin
      InsertMethod(NewProc);
    end;
    fowner.CompileIfNeeded;
  end;
end;

procedure TMethodList.InsertMethod(NewProc: tbtstring; OnTop: Boolean = false);
var
  x : Integer;
  sl : TStringList;
  nBegins : Integer;
  nProcs  : Integer;
  line, test : tbtstring;

 
  function IsItem(line,item:tbtstring; First :Boolean = false):Boolean;
  var
   nPos : Integer;
  begin
    repeat
      nPos := pos(item,line);
      result := ((npos>0) and ((length(Line)-nPos<= length(item)) or not(line[nPos+length(item)] in ['0'..'9','A'..'Z','_'])) And
                ((Npos = 1) or ((not first) and not(line[nPos-1] in ['0'..'9','A'..'Z','_']))));
      if nPos <> 0 then line := copy(line,nPos+Length(Item),Length(line));
    until (Result) or (nPos = 0);
  end;

  function DelSpaces(AText: tbtstring): tbtstring;
  var i: Integer;
  begin
    Result := '';
    for i := 1 to Length(AText) do
      if AText[i] <> ' ' then
        Result := Result + AText[i];
  end;

  function IsProcDecl(AnOriginalProcDecl: tbtstring): Boolean;
  var
    bIsFunc: Boolean;
    iLineNo: Integer;
    sProcKey: tbtstring;
    sProcDecl: tbtstring;
  begin
    Result := false;
    sProcDecl := Line;
    iLineNo := x;
    bIsFunc := isItem(AnOriginalProcDecl,'FUNCTION',true);
 
    if bIsFunc
      then sProcKey := 'FUNCTION'
      else sProcKey := 'PROCEDURE';
 
    sProcDecl := copy(sProcDecl,Pos(sProcKey,sProcDecl),Length(sProcDecl));
 
    while not IsItem(sProcDecl,'BEGIN') do
    begin
      inc(iLineNo);
      if iLineNo > (fowner.script.Count - 1) then exit;
      sProcDecl := sProcDecl + ' ' + uppercase(trim(fowner.script[iLineNo])) + ' ';
    end;
 
    sProcDecl := DelSpaces(sProcDecl);
    AnOriginalProcDecl := DelSpaces(AnOriginalProcDecl);
 
    sProcDecl := copy(sProcDecl,1,Length(AnOriginalProcDecl));
 
    Result := sProcDecl = AnOriginalProcDecl;
 
  end;
begin
  sl := TStringList.create;
  Try
    sl.Text := NewProc;
    test := uppercase(trim(sl[0]));
  finally
    Sl.free;
  end;
  nProcs := 0;
  nBegins := 0;
  x := 0;
  If Not Ontop Then begin
    for x := 0 to fOwner.script.count -1 do begin
      Line := fowner.script[x];
      Line := uppercase(trim(line));
      If IsItem(line,'PROCEDURE', true) or IsItem(line,'FUNCTION', true) then begin
        If nBegins >0 then Raise exception.create('Missing some ''end'' statments');
        If (nProcs = 0) and IsProcDecl(test) and
           (not IsItem(line,'FORWARD')) and (not IsItem(line,'EXTERNAL')) then
          Exit;
        Inc(nProcs);
      end;
      if IsItem(line,'FORWARD') or IsItem(line,'EXTERNAL') then
        dec(nProcs);
      If Pos('END',line) < Pos('BEGIN',line) then begin
        If IsItem(line,'END') then begin
          If (nBegins = 0) and (nProcs=0) then Break;
          Dec(nBegins);
          If nBegins = 0 then Dec(nProcs);
        end;
        If IsItem(line,'BEGIN') or IsItem(line,'TRY') or IsItem(line,'CASE') then begin
          If nProcs = 0 then Break;
          Inc(nBegins);
        end;
      end else begin
        If IsItem(line,'BEGIN') or IsItem(line,'TRY') or IsItem(line,'CASE') then begin
          If nProcs = 0 then Break;
          Inc(nBegins);
        end;
        If IsItem(line,'END') then begin
          If (nBegins = 0) and (nProcs=0) then Break;
          Dec(nBegins);
          If nBegins = 0 then Dec(nProcs);
        end;
      end;
    end;
  end;
  FOwner.script.BeginUpdate;
  Try
    If (nProcs <> 0) or (nBegins<>0) then
      Raise exception.create(sMissingEndStatment);
    If (Not Ontop) and (x>0) and (Trim(FOwner.script[x-1])<>'') then begin
      FOwner.script.Insert(x,'');
      inc(x);
    end;
    FOwner.script.Insert(x,NewProc);
    FOwner.script.text := FOwner.script.text;
  finally
    FOwner.script.EndUpdate;
  end;
end;
 
destructor TMethodList.Destroy;
begin
  fProcList.Free;  {<< Needs Eventlist for removing Methods}
  fEventList.Free;
  inherited;
end;

procedure TMethodList.FillMethods;
var
  x, y : Integer;
  m : TMethod;
begin
  for x := 0 to fEventList.Count-1 do begin
    Y := ProcIndexOf(MethodS[x].ProcName);
    If (Y >= 0) and assigned(Methods[x].Instance) then begin
      m := Procs[Y].Method;
      if m.Data = nil then begin
        m := fOwner.Exec.GetProcAsMethodN(Procs[Y].name);
        TProcObj(fProcList.Items[Y]).Method := m;
      end;
      SetMethodProp(Methods[x].Instance, Methods[x].propname, m );
    end;
  end;
end;

function TMethodList.GetMethodName(Instance: TObject; PropName: tbtstring): tbtstring;
var
  x : Integer;
begin
  fOwner.CompileIfNeeded;
  x := methodIndexOf(Instance,PropName);
  If x>=0 then result := Methods[x].ProcName
          else result := '';
end;

function TMethodList.GetObject(Index: Integer): TMethodObj;
begin
  result := TMethodObj(fEventList.items[Index]);
end;

function TMethodList.GetProcObj(Index: Integer): TProcObj;
begin
  result := TProcObj(fProcList.items[Index]);
end;

procedure TMethodList.ListEventsName(EventType: tbtstring; List: TStrings);
var
  x : Integer;
begin
  If List = nil then exit;
  EventType := Uppercase(EventType);
  List.Clear;
  fOwner.CompileIfNeeded;
  for x := 0 to fProcList.count-1 do begin
    If Procs[x].ProcType.indexof(EventType)<> -1 then
      List.add(Procs[x].name);
  end;
end;

function TMethodList.MethodCount: Integer;
begin
  result := fEventList.count;
end;

function TMethodList.methodIndexOf(Instance: TObject;
  PropName: tbtstring): Integer;
var x : integer;
begin
  Result := -1;
  for x := 0 to fEventList.count-1 do begin
    if (TMethodObj(fEventList.Items[x]).Instance = Instance) and
       ((propName='') or(TMethodObj(fEventList.Items[x]).PropName = PropName)) then begin
      Result := x;
      exit;
    end;
  end;
end;

function TMethodList.ProcCount: Integer;
begin
  result := fProcList.count;
end;

function TMethodList.ProcIndexOf(Name: tbtstring): Integer;
var x : integer;
begin
  result := -1;
  Name := Uppercase(name);
  For x := 0 to fProcList.count-1 do begin
    If Uppercase(TProcObj(fProcList.Items[x]).name) = name then begin
       Result := x;
       exit;
    end;
  end;
end;

procedure TMethodList.SetMethodName(Instance: TObject; PropName: tbtstring;
  const Value: tbtstring);
var
  x, y : Integer;
  mo   : TMethodObj;
  function TypeData(Instance: TObject; const PropName: tbtstring):PTypeData;
  var
    PropInfo: PPropInfo;
  begin
    // assume failure
    Result := Nil;
    PropInfo := GetPropInfo(Instance, PropName);
    if PropInfo <> nil then
    begin
      Result:= GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});
    end
  end;

begin
  If PropName = '' then begin
    x := 0;
    While x < MethodCount do begin
      If (Methods[x].Instance = Instance) or (Instance = nil) then
        fEventList.Delete(x)
      else Inc(x);
    end;
  end else begin
    x := methodIndexOf(Instance, PropName);
    if value = '' then begin
       if x >= 0 then fEventList.Delete(x);
    end else begin
      fOwner.CompileIfNeeded;
      y := ProcIndexOf(Value);
      If (Y = -1) then begin
        CreateProc(Value, TypeData(Instance,propName)^);
        y := 0;
      end;
      If (x = -1) then begin
        If (Y <> -1) then begin
          mo := TMethodObj.create;
          mo.Instance := TPersistent(Instance);
          mo.ProPName := Propname;
          mo.procName := Value;
          If (methodIndexOf(Instance,'')<>-1) and Instance.InheritsFrom(TComponent) then
            fOwner.FreeNotification(TComponent(Instance));
          fEventList.add(mo);
        end;
      end else
      begin
        Methods[x].procname := Value;
      end;
    end;
  end;
end;

procedure TMethodList.ClearAll;
begin
  fProclist.clear;
  fEventList.Clear;
end;

{ TProcObj }

constructor TProcObj.create(aOwner: TMethodList);
begin
  inherited create();
  fOwner := aOwner;
  ProcType := TStringList.Create;
end;

destructor TProcObj.Destroy;

var x : Integer;
  m :TMethod;
begin
  m.Code := nil;
  m.Data := nil;
  If ((Method.Data <> nil) or (method.Code<> nil)) and (fOwner<>nil) and assigned(fOwner) then begin
    for x := 0 to fOwner.MethodCount-1 do begin
      If (name = fOwner.Methods[x].ProcName) and assigned(fOwner.Methods[x].Instance) then begin
        Try
          SetMethodProp(fOwner.Methods[x].Instance, fOwner.Methods[x].PropName,m);
        except; end;
      end;
    end;
  end;
  ProcType.free;
  inherited;
end;

procedure TProcObj.SetName(const Value: tbtstring);
var
  x : Integer;
begin
  If FName <> Value then begin
    If fName<>'' then begin
      for x := 0 to fOwner.MethodCount-1 do begin
        If Fname = fOwner.Methods[x].ProcName then begin
          fOwner.Methods[x].ProcName := Value;
        end;
      end;
    end;
    FName := Value;
  end;
end;


end.

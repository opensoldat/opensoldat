{*******************************************************}
{                                                       }
{       ScriptMapsList unit for OPENSOLDAT              }
{                                                       }
{       Copyright (c) 2014 Tomasz Kolosowski            }
{                          and  Umut Karakas            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit ScriptMapsList;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes,
  PascalCompiler,
  PascalExec,
  Server,
  Game,
  Command,
  ScriptCore3Api,
  SysUtils;

type

  TScriptMapsList = class;

  TScriptMapsList = class(TObject)
  private
    function GetMap(Num: Integer): string;
    function GetCurrentMapId: Integer;
    procedure SetCurrentMapId(NewNum: Integer);
    function GetMapsCount: Integer;
  public
    procedure AddMap(Name: string);
    procedure RemoveMap(Name: string);
    function GetMapIdByName(Name: string): Integer;
    property Map[i: Integer]: string read GetMap; default;
    property CurrentMapId: Integer read GetCurrentMapId write SetCurrentMapId;
    property MapsCount: Integer read GetMapsCount;
  end;

  TScriptMapsListAPI = class(TScriptCore3API)
  public
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

procedure TScriptMapsList.AddMap(Name: string);
begin
  ParseInput('addmap ' + Name, 255);
end;

procedure TScriptMapsList.RemoveMap(Name: string);
begin
  ParseInput('delmap ' + Name, 255);
end;

function TScriptMapsList.GetMapIdByName(Name: string): Integer;
begin
  Result := MapsList.IndexOf(Name);
end;

function TScriptMapsList.GetMap(Num: Integer): string;
begin
  if Num >= MapsList.Count then
    Result := ''
  else
    Result := MapsList[Num];
end;

function TScriptMapsList.GetCurrentMapId: Integer;
begin
  Result := MapIndex;
end;

procedure TScriptMapsList.SetCurrentMapId(NewNum: Integer);
begin
  if (NewNum >= 0) and (NewNum < MapsList.Count) then
    MapIndex := NewNum;
end;

function TScriptMapsList.GetMapsCount: Integer;
begin
  Result := MapsList.Count;
end;


procedure MapReadHelper(Self: TScriptMapsList; var Result: string; const Num: Integer);
begin
  Result := Self.Map[Num];
end;

procedure CurrentMapIdReadHelper(Self: TScriptMapsList; var Result: Integer);
begin
  Result := Self.CurrentMapId;
end;

procedure CurrentMapIdWriteHelper(Self: TScriptMapsList; const Result: Integer);
begin
  Self.CurrentMapId := Result;
end;

procedure MapsCountReadHelper(Self: TScriptMapsList; var Result: Integer);
begin
  Result := Self.MapsCount;
end;


procedure TScriptMapsListAPI.CompilerRegister(Compiler: TPascalCompiler);
var
   Cls: TPascalCompiletimeClass;
begin
  Cls := Compiler.AddClass(nil, 'TMapsList');
  with Cls do
  begin
    RegisterMethod('procedure AddMap(Name: string)');
    RegisterMethod('procedure RemoveMap(Name: string)');
    RegisterMethod('function GetMapIdByName(Name: string): Integer');
    RegisterProperty('Map', 'string Integer', iptR);
    RegisterProperty('CurrentMapId', 'Integer', iptRW);
    RegisterProperty('MapsCount', 'Integer', iptR);
    SetDefaultPropery('Map');
  end;
end;

procedure TScriptMapsListAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptMapsList, 'TMapsList') do
  begin
    RegisterMethod(@TScriptMapsList.AddMap, 'AddMap');
    RegisterMethod(@TScriptMapsList.RemoveMap, 'RemoveMap');
    RegisterMethod(@TScriptMapsList.GetMapIdByName, 'GetMapIdByName');
    RegisterPropertyHelper(@MapReadHelper, nil, 'Map');
    RegisterPropertyHelper(@CurrentMapIdReadHelper, @CurrentMapIdWriteHelper,
      'CurrentMapId');
    RegisterPropertyHelper(@MapsCountReadHelper, nil, 'MapsCount');
  end;
end;

end.

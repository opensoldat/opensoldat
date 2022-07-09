{*******************************************************}
{                                                       }
{       ScriptBanLists unit for OPENSOLDAT              }
{                                                       }
{       Copyright (c) 2015 Tomasz Kolosowski            }
{                          and  Umut Karakas            }
{                                                       }
{*******************************************************}

// TODO: Documentation
unit ScriptBanLists;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  BanSystem,
  Classes,
  PascalCompiler,
  PascalExec,
  Server,
  ScriptCore3Api,
  SysUtils;

type

  TScriptBanLists = class;

  TScriptBanLists = class(TObject)
  private
    function GetBannedHW(Num: Integer): TBanHW;
    function GetBannedIP(Num: Integer): TBanIP;
    function GetBannedHWCount: Integer;
    function GetBannedIPCount: Integer;
  public
    procedure AddHWBan(HW, Reason: string; Duration: Integer);
    procedure AddIPBan(IP: string; Reason: string; Duration: Integer);
    function DelHWBan(HW: string): Boolean;
    function DelIPBan(IP: string): Boolean;
    function IsBannedHW(HW: string): Boolean;
    function IsBannedIP(IP: string): Boolean;
    function GetHWBanId(HW: string): Integer;
    function GetIPBanId(IP: string): Integer;
    property HW[i: Integer]: TBanHW read GetBannedHW;
    property IP[i: Integer]: TBanIP read GetBannedIP;
    property BannedHWCount: Integer read GetBannedHWCount;
    property BannedIPCount: Integer read GetBannedIPCount;
  end;

  TScriptBanListsAPI = class(TScriptCore3API)
  public
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

procedure TScriptBanLists.AddHWBan(HW, Reason: string; Duration: Integer);
begin
  AddBannedHW(HW, Reason, Duration);
end;

procedure TScriptBanLists.AddIPBan(IP: string; Reason: string; Duration: Integer);
begin
  AddBannedIP(IP, Reason, Duration);
end;

function TScriptBanLists.DelHWBan(HW: string): Boolean;
begin
  Result := DelBannedHW(HW);
end;

function TScriptBanLists.DelIPBan(IP: string): Boolean;
begin
  Result := DelBannedIP(IP);
end;

function TScriptBanLists.IsBannedHW(HW: string): Boolean;
begin
  Result := CheckBannedHW(HW);
end;

function TScriptBanLists.IsBannedIP(IP: string): Boolean;
begin
  Result := CheckBannedIP(IP);
end;

function TScriptBanLists.GetHWBanId(HW: string): Integer;
begin
  Result := FindBanHW(HW);
end;

function TScriptBanLists.GetIPBanId(IP: string): Integer;
begin
  Result := FindBan(IP);
end;

function TScriptBanLists.GetBannedHW(Num: Integer): TBanHW;
begin
  Result := BannedHWList[Num];
end;

function TScriptBanLists.GetBannedIP(Num: Integer): TBanIP;
begin
  Result := BannedIPList[Num];
end;

function TScriptBanLists.GetBannedHWCount: Integer;
begin
  Result := High(BannedHWList);
end;

function TScriptBanLists.GetBannedIPCount: Integer;
begin
  Result := High(BannedIPList);
end;


procedure HWReadHelper(Self: TScriptBanLists; var Result: TBanHW; const Num: Integer);
begin
  Result := Self.HW[Num];
end;

procedure IPReadHelper(Self: TScriptBanLists; var Result: TBanIP; const Num: Integer);
begin
  Result := Self.IP[Num];
end;

procedure BannedHWCountReadHelper(Self: TScriptBanLists; var Result: Integer);
begin
  Result := Self.BannedHWCount;
end;

procedure BannedIPCountReadHelper(Self: TScriptBanLists; var Result: Integer);
begin
  Result := Self.BannedIPCount;
end;


procedure TScriptBanListsAPI.CompilerRegister(Compiler: TPascalCompiler);
var
   Cls: TPascalCompiletimeClass;
begin
  Compiler.AddType('TBannedHW', 'record HW: string; Time: Integer; Reason: string; end;');
  Compiler.AddType('TBannedIP', 'record IP: string; Time: Integer; Reason: string; end;');

  Cls := Compiler.AddClass(nil, 'TBanLists');
  with Cls do
  begin
    RegisterMethod('procedure AddHWBan(HW, Reason: string; Duration: Integer)');
    RegisterMethod('procedure AddIPBan(IP: string; Reason: string; Duration: Integer)');
    RegisterMethod('function DelHWBan(HW: string): Boolean');
    RegisterMethod('function DelIPBan(IP: string): Boolean');
    RegisterMethod('function IsBannedHW(HW: string): Boolean');
    RegisterMethod('function IsBannedIP(IP: string): Boolean');
    RegisterMethod('function GetHWBanId(HW: string): Integer');
    RegisterMethod('function GetIPBanId(IP: string): Integer');
    RegisterProperty('HW', 'TBannedHW Integer', iptR);
    RegisterProperty('IP', 'TBannedIP Integer', iptR);
    RegisterProperty('BannedHWCount', 'Integer', iptR);
    RegisterProperty('BannedIPCount', 'Integer', iptR);
  end;
end;

procedure TScriptBanListsAPI.RuntimeRegisterApi(Exec: TPascalExec);
begin
  with Exec.AddClass(TScriptBanLists, 'TBanLists') do
  begin
    RegisterMethod(@TScriptBanLists.AddHWBan, 'AddHWBan');
    RegisterMethod(@TScriptBanLists.AddIPBan, 'AddIPBan');
    RegisterMethod(@TScriptBanLists.DelHWBan, 'DelHWBan');
    RegisterMethod(@TScriptBanLists.DelIPBan, 'DelIPBan');
    RegisterMethod(@TScriptBanLists.IsBannedHW, 'IsBannedHW');
    RegisterMethod(@TScriptBanLists.IsBannedIP, 'IsBannedIP');
    RegisterMethod(@TScriptBanLists.GetHWBanId, 'GetHWBanId');
    RegisterMethod(@TScriptBanLists.GetIPBanId, 'GetIPBanId');
    RegisterPropertyHelper(@HWReadHelper, nil, 'HW');
    RegisterPropertyHelper(@IPReadHelper, nil, 'IP');
    RegisterPropertyHelper(@BannedHWCountReadHelper, nil, 'BannedHWCount');
    RegisterPropertyHelper(@BannedIPCountReadHelper, nil, 'BannedIPCount');
  end;
end;

end.


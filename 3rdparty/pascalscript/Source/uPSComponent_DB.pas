 unit uPSComponent_DB;

interface
{$I PascalScript.inc}
uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler;
type

  TPSImport_DB = class(TPSPlugin)
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  public
  end;

  TIFPS3CE_DB = class(TPSImport_DB);

implementation
uses
  uPSC_DB,
  uPSR_DB;

{ TPSImport_DB }

procedure TPSImport_DB.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_DB(CompExec.Comp);
end;

procedure TPSImport_DB.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_DB(RI);
end;

end.

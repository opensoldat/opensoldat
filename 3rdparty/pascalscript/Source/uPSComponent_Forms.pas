 
unit uPSComponent_Forms;

interface
uses
  SysUtils, Classes, uPSRuntime, uPSCompiler, uPSComponent;
type
  
  TPSImport_Forms = class(TPSPlugin)
  private
    FEnableForms: Boolean;
    FEnableMenus: Boolean;
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    
    property EnableForms: Boolean read FEnableForms write FEnableForms;
    
    property EnableMenus: Boolean read FEnableMenus write FEnableMenus;
  end;
  
  TIFPS3CE_Forms = class(TPSImport_Forms);

implementation
uses
  uPSC_forms,
  uPSC_menus,
  uPSR_forms,
  uPSR_menus;

{ TPSImport_Forms }

procedure TPSImport_Forms.CompileImport1(CompExec: TPSScript);
begin
  if FEnableForms then
    SIRegister_Forms(CompExec.comp);
  if FEnableMenus then
    SIRegister_Menus(CompExec.comp);
end;

constructor TPSImport_Forms.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);
  FEnableForms := True;
  FEnableMenus := True;
end;

procedure TPSImport_Forms.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  if FEnableForms then
    RIRegister_Forms(ri);

  if FEnableMenus then
  begin
    RIRegister_Menus(ri);
    RIRegister_Menus_Routines(compexec.Exec);
  end;

end;

end.

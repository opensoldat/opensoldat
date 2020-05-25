 
unit uPSComponent_StdCtrls;

interface
uses
  SysUtils, Classes, uPSComponent, uPSCompiler, uPSRuntime;
type
  
  TPSImport_StdCtrls = class(TPSPlugin)
  private
    FEnableButtons: Boolean;
    FEnableExtCtrls: Boolean;
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    
    property EnableExtCtrls: Boolean read FEnableExtCtrls write FEnableExtCtrls;
    
    property EnableButtons: Boolean read FEnableButtons write FEnableButtons;
  end;
  
  TIFPS3CE_StdCtrls = class(TPSImport_StdCtrls);


implementation
uses
  uPSC_buttons,
  uPSC_stdctrls,
  uPSC_extctrls,
  uPSR_buttons,
  uPSR_stdctrls,
  uPSR_extctrls;

{ TPSImport_StdCtrls }

procedure TPSImport_StdCtrls.CompileImport1(CompExec: TPSScript);
begin
    SIRegister_stdctrls(CompExec.Comp);
  if FEnableExtCtrls then
    SIRegister_ExtCtrls(CompExec.Comp);
  if FEnableButtons then
    SIRegister_Buttons(CompExec.Comp);
end;

constructor TPSImport_StdCtrls.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);
  FEnableButtons := True;
  FEnableExtCtrls := True;
end;

procedure TPSImport_StdCtrls.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
    RIRegister_stdctrls(RI);
  if FEnableExtCtrls then
    RIRegister_ExtCtrls(RI);
  if FEnableButtons then
    RIRegister_Buttons(RI);
end;

end.

 unit uPSComponent_Controls;

interface
uses
  SysUtils, Classes, uPSComponent, uPSCompiler, uPSRuntime;
type
  
  TPSImport_Controls = class(TPSPlugin)
  private
    FEnableStreams: Boolean;
    FEnableGraphics: Boolean;
    FEnableControls: Boolean;
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    
    property EnableStreams: Boolean read FEnableStreams write FEnableStreams;
    
    property EnableGraphics: Boolean read FEnableGraphics write FEnableGraphics;
    
    property EnableControls: Boolean read FEnableControls write FEnableControls;
  end;
  
  TIFPS3CE_Controls = class(TPSImport_Controls);

implementation
uses
  uPSC_graphics,
  uPSC_controls,
  uPSR_graphics,
  uPSR_controls;


{ TPSImport_Controls }

procedure TPSImport_Controls.CompileImport1(CompExec: TPSScript);
begin
  if FEnableGraphics then
    SIRegister_Graphics(CompExec.Comp, FEnableStreams);
  if FEnableControls then
    SIRegister_Controls(CompExec.Comp);
end;

constructor TPSImport_Controls.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnableStreams := True;
  FEnableGraphics := True;
  FEnableControls := True;
end;

procedure TPSImport_Controls.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  if FEnableGraphics then
    RIRegister_Graphics(ri, FEnableStreams);
  if FEnableControls then
    RIRegister_Controls(ri);
end;


end.

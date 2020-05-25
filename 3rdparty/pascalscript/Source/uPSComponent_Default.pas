 unit uPSComponent_Default;
{$I PascalScript.inc}
interface
uses
  SysUtils, Classes, uPSComponent, uPSCompiler, uPSRuntime;

type

  TPSImport_DateUtils = class(TPSPlugin)
  public
    procedure CompOnUses(CompExec: TPSScript); override;
    procedure ExecOnUses(CompExec: TPSScript); override;
  end;

  TPSImport_Classes = class(TPSPlugin)
  private
    FEnableStreams: Boolean;
    FEnableClasses: Boolean;
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  public

    constructor Create(AOwner: TComponent); override;
  published

    property EnableStreams: Boolean read FEnableStreams write FEnableStreams;

    property EnableClasses: Boolean read FEnableClasses write FEnableClasses;
  end;

  TIFPS3CE_Std = class(TPSImport_Classes);

  TIFPS3CE_DateUtils = class(TPSImport_DateUtils);

implementation
uses
  uPSC_std,
  uPSR_std,
  uPSC_classes,
  uPSR_classes,
  uPSC_dateutils,
  uPSR_dateutils;

{ TPSImport_Classes }

procedure TPSImport_Classes.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_Std(CompExec.Comp);
  if FEnableClasses then
    SIRegister_Classes(CompExec.Comp, FEnableStreams);
end;

procedure TPSImport_Classes.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_Std(Ri);
  if FEnableClasses then
    RIRegister_Classes(ri, FEnableStreams);
end;

constructor TPSImport_Classes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnableStreams := True;
  FEnableClasses := True;
end;

{ TPSImport_DateUtils }

procedure TPSImport_DateUtils.CompOnUses(CompExec: TPSScript);
begin
  RegisterDateTimeLibrary_C(CompExec.Comp);
end;

procedure TPSImport_DateUtils.ExecOnUses(CompExec: TPSScript);
begin
  RegisterDateTimeLibrary_R(CompExec.Exec);
end;

end.

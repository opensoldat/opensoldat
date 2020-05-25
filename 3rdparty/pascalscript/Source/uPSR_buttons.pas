
unit uPSR_buttons;
{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegisterTSPEEDBUTTON(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBITBTN(Cl: TPSRuntimeClassImporter);

procedure RIRegister_Buttons(Cl: TPSRuntimeClassImporter);

implementation
uses
  Classes{$IFDEF CLX}, QControls, QButtons{$ELSE}, Controls, Buttons{$ENDIF};

procedure RIRegisterTSPEEDBUTTON(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TSPEEDBUTTON);
end;


procedure RIRegisterTBITBTN(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TBITBTN);
end;

procedure RIRegister_Buttons(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTSPEEDBUTTON(cl);
  RIRegisterTBITBTN(cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.

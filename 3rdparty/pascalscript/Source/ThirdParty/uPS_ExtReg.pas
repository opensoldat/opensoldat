unit uPS_ExtReg;

interface

procedure Register;          

implementation

uses classes, uPSI_IBX, uPSI_Mask, upSI_JvMail, uPSI_Dialogs, uPSI_Registry;

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_IBX, TPSImport_Mask, TPSImport_JvMail,
                                       TPSImport_Dialogs, TPSImport_Registry]);
end;

end.

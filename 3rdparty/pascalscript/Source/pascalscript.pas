{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit PascalScript; 

interface

uses
  uPSRuntime, PascalScript_Core_Reg, uPSC_buttons, uPSC_classes, uPSC_controls, 
    uPSC_dateutils, uPSC_DB, uPSC_dll, uPSC_extctrls, uPSC_forms, 
    uPSC_graphics, uPSC_menus, uPSC_std, uPSC_stdctrls, uPSCompiler, 
    uPSComponent, uPSComponent_Controls, uPSComponent_DB, uPSComponent_Default, 
    uPSComponent_Forms, uPSComponent_StdCtrls, uPSComponentExt, uPSDebugger, 
    uPSDisassembly, uPSPreProcessor, uPSR_buttons, uPSR_classes, uPSR_controls, 
    uPSR_dateutils, uPSR_DB, uPSR_dll, uPSR_extctrls, uPSR_forms, 
    uPSR_graphics, uPSR_menus, uPSR_std, uPSR_stdctrls, uPSUtils, 
    uPSComponent_COM, uPSC_comobj, uPSR_comobj, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('PascalScript_Core_Reg', @PascalScript_Core_Reg.Register); 
end; 

initialization
  RegisterPackage('PascalScript', @Register); 
end.

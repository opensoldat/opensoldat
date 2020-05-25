unit PascalScript_Core_Reg_noDB;

{----------------------------------------------------------------------------}
{ RemObjects Pascal Script
{
{ compiler: Delphi 2 and up, Kylix 3 and up
{ platform: Win32, Linux
{
{ (c)opyright RemObjects Software. all rights reserved.
{
{----------------------------------------------------------------------------}

{$I PascalScript.inc}

interface

{$R PascalScript_Core_Glyphs.res}

procedure Register;

implementation

uses
  Classes,
  uPSComponent,
  uPSComponentExt,
  uPSDebugger,
  uPSComponent_Default,
  uPSComponent_COM,
  uPSComponent_Forms,
  uPSComponent_Controls,
  uPSComponent_StdCtrls;

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSScript,
                                       TPSScriptDebugger,
                                       TPSDllPlugin,
                                       TPSImport_Classes,
                                       TPSImport_DateUtils,
                                       TPSImport_ComObj,
                                       TPSImport_Forms,
                                       TPSImport_Controls,
                                       TPSImport_StdCtrls,
                                       TPSScriptExtension]);
end;

end.

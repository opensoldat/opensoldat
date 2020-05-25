unit PascalScript_Core_Ext_Reg;

{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{                                                                            } 
{ compiler: Delphi 2 and up, Kylix 3 and up                                  }
{ platform: Win32, Linux                                                     }
{                                                                            }
{ (c)opyright RemObjects Software. all rights reserved.                      }
{                                                                            }
{----------------------------------------------------------------------------}

{$I PascalScript.inc}

interface

procedure Register;

implementation

uses
  Classes,
  uPSComponentExt;

procedure Register;
begin
  RegisterComponents('RemObjects Pascal Script',[TPSScriptExtension]);
end;

end.

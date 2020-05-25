unit IdAbout;

interface

{$I IdCompilerDefines.inc}

uses
  {$IFDEF DOTNET}
  IdAboutDotNET;
  {$ELSE}
  IdAboutVCL;
  {$ENDIF}

//we have a procedure for providing a product name and version in case
//we ever want to make another product.
procedure ShowAboutBox(const AProductName, AProductName2, AProductVersion : String);
procedure ShowDlg;

implementation

{$IFDEF DOTNET}
  //for some reason, the Winforms designer doesn't like this in the same unit
  //as the class it's for
  {$R 'IdAboutDotNET.TfrmAbout.resources' 'IdAboutDotNET.resx'}
{$ENDIF}

procedure ShowAboutBox(const AProductName, AProductName2, AProductVersion : String);
begin
  TfrmAbout.ShowAboutBox(AProductName, AProductName2, AProductVersion);
end;

procedure ShowDlg;
begin
  TfrmAbout.ShowDlg;
end;

end.

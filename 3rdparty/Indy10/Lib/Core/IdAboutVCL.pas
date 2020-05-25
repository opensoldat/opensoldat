unit IdAboutVCL;

interface

{$I IdCompilerDefines.inc}

uses
{$IFDEF WIDGET_KYLIX}
  QStdCtrls, QForms, QExtCtrls, QControls, QComCtrls, QGraphics, Qt,
{$ENDIF}
{$IFDEF WIDGET_VCL_LIKE}
  StdCtrls, Buttons, ExtCtrls, Graphics, Controls, ComCtrls, Forms,
{$ENDIF}
{$IFDEF HAS_UNIT_Types}
  Types,
{$ENDIF}
{$IFDEF WIDGET_LCL}
  LResources,
{$ENDIF}
  Classes, SysUtils;

type
  TfrmAbout = class(TForm)
  protected
    FimLogo : TImage;
    FlblCopyRight : TLabel;
    FlblName : TLabel;
    FlblName2 : TLabel;
    FlblVersion : TLabel;
    FlblBuiltFor : TLabel;
    FlblLicense : TLabel;
    FlblPleaseVisitUs : TLabel;
    FlblURL : TLabel;
    //for LCL, we use a TBitBtn to be consistant with some GUI interfaces
    //and the Lazarus IDE.
    {$IFDEF USE_TBitBtn}
    FbbtnOk : TBitBtn;
    {$ELSE}
    FbbtnOk : TButton;
    {$ENDIF}
    procedure lblURLClick(Sender: TObject);
    function GetProductName: String;
    procedure SetProductName(const AValue: String);
    function GetProductName2: String;
    procedure SetProductName2(const AValue: String);
    function GetVersion: String;
    procedure SetVersion(const AValue: String);
  public
    //we have a method for providing a product name and version in case
    //we ever want to make another product.
    class procedure ShowDlg;
    class procedure ShowAboutBox(const AProductName, AProductName2, AProductVersion: String);
    constructor Create(AOwner : TComponent); overload; override;
    constructor Create; reintroduce; overload;
    property ProductName : String read GetProductName write SetProductName;
    property ProductName2 : String read GetProductName2 write SetProductName2;
    property Version : String read GetVersion write SetVersion;
  end;

implementation

{$IFNDEF WIDGET_LCL}
  {$IFDEF WIN32_OR_WIN64}
  {$R IdAboutVCL.RES}
  {$ENDIF}
  {$IFDEF KYLIX}
  {$R IdAboutVCL.RES}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF WIN32_OR_WIN64}ShellApi, {$ENDIF}
  {$IFNDEF WIDGET_LCL}
   //done this way because we reference HInstance in Delphi for loading
   //resources.  Lazarus does something different.
    {$IFDEF WIN32_OR_WIN64}
  Windows,
    {$ENDIF}
  {$ENDIF}
  IdDsnCoreResourceStrings,
  IdGlobal;

{$IFNDEF WIDGET_LCL}
function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := RGB(R, G, B);
end;
{$ENDIF}

{ TfrmAbout }

constructor TfrmAbout.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner,0);

  FimLogo := TImage.Create(Self);
  FlblCopyRight := TLabel.Create(Self);
  FlblName := TLabel.Create(Self);
  FlblName2 := TLabel.Create(Self);
  FlblVersion := TLabel.Create(Self);
  FlblBuiltFor := TLabel.Create(Self);
  FlblLicense := TLabel.Create(Self);
  FlblPleaseVisitUs := TLabel.Create(Self);
  FlblURL := TLabel.Create(Self);
  {$IFDEF USE_TBitBtn}
  FbbtnOk := TBitBtn.Create(Self);
  {$ELSE}
  FbbtnOk := TButton.Create(Self);
  {$ENDIF}

  Name := 'formAbout';
  Left := 0;
  Top := 0;
  Anchors := [];//[akLeft, akTop, akRight,akBottom];
  BorderIcons := [biSystemMenu];
  BorderStyle := bsDialog;

  Caption := RSAAboutFormCaption;
  ClientHeight := 336;
  ClientWidth := 554;
  Color := 2520226; // RGBToColor(38, 116, 162)

  Font.Color := 16776138; // RGBToColor(202, 251, 255)
  Font.Height := -12;
  Font.Size := 9;
  Font.Name := 'Arial';
  Font.Style := [];
  Position := poScreenCenter;
  {$IFDEF WIDGET_VCL}
  Scaled := True;
  {$ENDIF}
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  //  PixelsPerInch := 96;

  FimLogo.Name := 'imLogo';
  FimLogo.Parent := Self;
  FimLogo.Left := 0;
  FimLogo.Top := 0;
  FimLogo.Width := 388;
  FimLogo.Height := 240;

  {$IFDEF WIDGET_LCL}
  FimLogo.Picture.Pixmap.LoadFromLazarusResource('IndyAboutBkgnd'); //this is XPM format, so Pixmap is used
  FimLogo.Align := alClient;
  FimLogo.Stretch := True;
  {$ELSE} // Because Lazarus is also WIDGET_VCL_LIKE_OR_KYLIX
    {$IFDEF WIDGET_VCL_LIKE_OR_KYLIX}
  FimLogo.Picture.Bitmap.LoadFromResourceName(HInstance, 'INDY_ABOUT_BACKGROUND');    {Do not Localize}
  FimLogo.Align := alClient;
  FimLogo.Stretch := True;
    {$ENDIF}
  {$ENDIF}

  FlblName.Name := 'lblName';
  FlblName.Parent := Self;
  FlblName.Left := 51;
  FlblName.Top := 28;
  FlblName.Width := 200;
  FlblName.Height := 101;
  FlblName.Anchors := [akLeft, akTop];
  {$IFDEF WIDGET_VCL}
  FlblName.Font.Charset := DEFAULT_CHARSET;
  FlblName.Transparent := True;
  {$ENDIF}
  FlblName.Font.Color := clWhite;
  FlblName.Font.Height := -72;
  FlblName.Font.Name := 'Arial Black';
  FlblName.Font.Style := [];
  FlblName.ParentFont := False;
  FlblName.WordWrap := False;
  FlblName.Caption := RSAAboutBoxTitle1;

  FlblName2.Name := 'lblName2';
  FlblName2.Parent := Self;
  FlblName2.Left := 54;
  FlblName2.Top := 110;
  FlblName2.Width := 192;
  FlblName2.Height := 35;
  FlblName2.Anchors := [akLeft, akTop];
  {$IFDEF WIDGET_VCL}
  FlblName2.Font.Charset := DEFAULT_CHARSET;
  FlblName2.Transparent := True;
  {$ENDIF}
  FlblName2.Font.Color := clWhite;
  FlblName2.Font.Height := -31;
  FlblName2.Font.Name := 'Arial';
  FlblName2.Font.Style := [];
  FlblName2.ParentFont := False;
  FlblName2.WordWrap := False;
  FlblName2.Caption := RSAAboutBoxTitle2;

  FlblVersion.Name := 'lblVersion';
  FlblVersion.Parent := Self;
  FlblVersion.Left := 300;
  FlblVersion.Top := 170;
  FlblVersion.Width := 200;
  FlblVersion.Height := 17;
  FlblVersion.Alignment := taRightJustify;
  FlblVersion.AutoSize := False;
  {$IFDEF WIDGET_VCL}
  FlblVersion.Font.Charset := DEFAULT_CHARSET;
  FlblVersion.Transparent := True;
  {$ENDIF}
  FlblVersion.Font.Color := 16776138; // RGBToColor(202, 251, 255)
  FlblVersion.Font.Height := -15;
  FlblVersion.Font.Name := 'Arial';
  FlblVersion.Font.Style := [fsBold];
  FlblVersion.ParentFont := False;
  FlblVersion.Anchors := [akTop, akRight];

  FlblBuiltFor.Name := 'lblBuiltFor';
  FlblBuiltFor.Parent := Self;
  FlblBuiltFor.Left := 300;
  FlblBuiltFor.Top := 188;
  FlblBuiltFor.Width := 200;
  FlblBuiltFor.Height := 17;
  FlblBuiltFor.Alignment := taRightJustify;
  FlblBuiltFor.AutoSize := False;
  {$IFDEF WIDGET_VCL}
  FlblBuiltFor.Font.Charset := DEFAULT_CHARSET;
  FlblBuiltFor.Transparent := True;
  {$ENDIF}
  FlblBuiltFor.Font.Color := 16776138; // RGBToColor(202, 251, 255)
  FlblBuiltFor.Font.Height := -14;
  FlblBuiltFor.Font.Name := 'Arial';
  FlblBuiltFor.Font.Style := [];
  FlblBuiltFor.ParentFont := False;
  FlblBuiltFor.Anchors := [akTop, akRight];

  // RLebeau: not using resouce strings for the product names because:
  // 1. the names are pretty specific and not likely to change with localization;
  // 2. we are trying to avoid using IFDEFs in resource units, per Embarcadero's request;
  // 3. I don't want to create more product-specific resource units unless we really need them;
  {$IFDEF WIDGET_KYLIX}
  FlblBuiltFor.Caption := IndyFormat(RSAAboutBoxBuiltFor, ['Kylix']);
  {$ELSE}
    {$IFDEF WIDGET_VCL}
  FlblBuiltFor.Caption := IndyFormat(RSAAboutBoxBuiltFor, ['VCL']);
    {$ELSE}
      {$IFDEF WIDGET_LCL}
  FlblBuiltFor.Caption := IndyFormat(RSAAboutBoxBuiltFor, ['Lazarus']);
      {$ELSE}
  FlblBuiltFor.Caption := IndyFormat(RSAAboutBoxBuiltFor, ['Unknown']);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  FlblLicense.Name := 'lblLicense';
  FlblLicense.Parent := Self;
  FlblLicense.Left := 300;
  FlblLicense.Top := 227;
  FlblLicense.Width := 200;
  FlblLicense.Height := 45;
  FlblLicense.Alignment := taRightJustify;
  FlblLicense.AutoSize := False;
  {$IFDEF WIDGET_VCL}
  FlblLicense.Font.Charset := DEFAULT_CHARSET;
  FlblLicense.Transparent := True;
  {$ENDIF}
  FlblLicense.Font.Color := 16776138; // RGBToColor(202, 251, 255)
  FlblLicense.Font.Height := -12;
  FlblLicense.Font.Name := 'Arial';
  FlblLicense.Font.Style := [];
  FlblLicense.ParentFont := False;
  FlblLicense.WordWrap := True;
  FlblLicense.Anchors := [akTop, akRight];
  FlblLicense.Caption := RSAAboutBoxLicences;

  FlblCopyRight.Name := 'lblCopyRight';
  FlblCopyRight.Parent := Self;
  FlblCopyRight.Left := 58;
  FlblCopyRight.Top := 171;
  FlblCopyRight.Width := 138;
  FlblCopyRight.Height := 15;
  FlblCopyRight.Caption := RSAAboutBoxCopyright;
  {$IFDEF WIDGET_VCL}
  FlblCopyRight.Font.Charset := DEFAULT_CHARSET;
  FlblCopyRight.Transparent := True;
  {$ENDIF}
  FlblCopyRight.Font.Color := 16776138; // RGBToColor(202, 251, 255)
  FlblCopyRight.Font.Height := -12;
  FlblCopyRight.Font.Name := 'Arial';
  FlblCopyRight.Font.Style := [];
  FlblCopyRight.ParentFont := False;
  FlblCopyRight.WordWrap := True;

  FlblPleaseVisitUs.Name := 'lblPleaseVisitUs';
  FlblPleaseVisitUs.Parent := Self;
  FlblPleaseVisitUs.Left := 58;
  FlblPleaseVisitUs.Top := 278;
  FlblPleaseVisitUs.Width := 276;
  FlblPleaseVisitUs.Height := 15;
  {$IFDEF WIDGET_VCL}
  FlblPleaseVisitUs.Font.Charset := DEFAULT_CHARSET;
  FlblPleaseVisitUs.Transparent := True;
  {$ENDIF}
  FlblPleaseVisitUs.Font.Color := 16776138; // RGBToColor(202, 251, 255)
  FlblPleaseVisitUs.Font.Height := -12;
  FlblPleaseVisitUs.Font.Name := 'Arial';
  FlblPleaseVisitUs.ParentFont := False;
  FlblPleaseVisitUs.Caption := RSAAboutBoxPleaseVisit;
  FlblPleaseVisitUs.Anchors := [akLeft, akTop];

  FlblURL.Name := 'lblURL';
  FlblURL.Left := 58;
  FlblURL.Top := 292;
  FlblURL.Width := 141;
  FlblURL.Height := 15;
  FlblURL.Cursor := crHandPoint;
  {$IFDEF WIDGET_VCL}
  FlblURL.Font.Charset := DEFAULT_CHARSET;
  FlblURL.Transparent := True;
  {$ENDIF}
  FlblURL.Font.Color := 16776138; // RGBToColor(202, 251, 255)
  FlblURL.Font.Height := -12;
  FlblURL.Font.Name := 'Arial';
  FlblURL.ParentFont := False;
  FlblURL.OnClick := lblURLClick;
  FlblURL.Caption := RSAAboutBoxIndyWebsite;
  FlblURL.Anchors := [akLeft, akTop];
  FlblURL.Parent := Self;

  FbbtnOk.Name := 'bbtnOk';
  FbbtnOk.Left := 475;
  {$IFDEF USE_TBitBtn}
  FbbtnOk.Top := 297;
  {$ELSE}
  FbbtnOk.Top := 302;
  FbbtnOk.Height := 25;
  {$ENDIF}
  FbbtnOk.Width := 75;
  FbbtnOk.Anchors := [akRight, akBottom];
  {$IFDEF USE_TBitBtn}
  FbbtnOk.Font.Color := clBlack;
  FbbtnOk.ParentFont := False;
  FbbtnOk.Kind := bkOk;
  {$ELSE}
  FbbtnOk.Caption := RSOk;
  {$ENDIF}
  FbbtnOk.Cancel := True;
  FbbtnOk.Default := True;
  FbbtnOk.ModalResult := 1;
  FbbtnOk.TabOrder := 0;
  FbbtnOk.Anchors := [akLeft, akTop, akRight];
  FbbtnOk.Parent := Self;
end;

function TfrmAbout.GetVersion: String;
begin
  Result :=  FlblVersion.Caption;
end;

function TfrmAbout.GetProductName: String;
begin
  Result := FlblName.Caption;
end;

function TfrmAbout.GetProductName2: String;
begin
  Result := FlblName2.Caption;
end;

procedure TfrmAbout.lblURLClick(Sender: TObject);
begin
  {$IFDEF WIN32_OR_WIN64}
  ShellAPI.ShellExecute(Handle, nil, PChar(FlblURL.Caption), nil, nil, 0);    {Do not Localize}
  FlblURL.Font.Color := clPurple;
  {$ENDIF}
end;

procedure TfrmAbout.SetVersion(const AValue: String);
begin
  FlblVersion.Caption := AValue;
end;

procedure TfrmAbout.SetProductName(const AValue: String);
begin
  FlblName.Caption := AValue;
end;

procedure TfrmAbout.SetProductName2(const AValue: String);
begin
  FlblName2.Caption := AValue;
end;

class procedure TfrmAbout.ShowAboutBox(const AProductName, AProductName2, AProductVersion: String);
var
  LFrm: TfrmAbout;
begin
  LFrm := TfrmAbout.Create;
  {$IFNDEF USE_OBJECT_ARC}
  try
  {$ENDIF}
    LFrm.Version := IndyFormat(RSAAboutBoxVersion, [AProductVersion]);
    LFrm.ProductName := AProductName;
    LFrm.ProductName2 := AProductName2;
    LFrm.ShowModal;
  {$IFNDEF USE_OBJECT_ARC}
  finally
    LFrm.Free;
  end;
  {$ENDIF}
end;

class procedure TfrmAbout.ShowDlg;
begin
  ShowAboutBox(RSAAboutBoxTitle1, RSAAboutBoxTitle2, gsIdVersion);
end;

constructor TfrmAbout.Create;
begin
  Create(nil);
end;

{$IFDEF WIDGET_LCL}
initialization
  {$i IdAboutVCL.lrs}
{$ENDIF}
end.

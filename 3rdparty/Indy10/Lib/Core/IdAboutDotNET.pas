unit IdAboutDotNET;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data;

type
  TfrmAbout = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    imgLogo: System.Windows.Forms.PictureBox;
    bbtnOk: System.Windows.Forms.Button;
    lblName: System.Windows.Forms.Label;
    lblName2: System.Windows.Forms.Label;
    lblVersion: System.Windows.Forms.Label;
    lblCopyright: System.Windows.Forms.Label;
    lblBuiltFor: System.Windows.Forms.Label;
    lblLicense: System.Windows.Forms.Label;
    lblPleaseVisitUs: System.Windows.Forms.Label;
    lblURL: System.Windows.Forms.LinkLabel;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure lblURL_LinkClicked(sender: System.Object; e: System.Windows.Forms.LinkLabelLinkClickedEventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  protected
    { Private Declarations }
    function GetProductName: string;
    procedure SetProductName(const AValue: string);
    function GetProductName2: string;
    procedure SetProductName2(const AValue: string);
    function GetVersion: string;
    procedure SetVersion(const AValue: string);
    function LoadBitmap(AResName: string): Bitmap;
  public
    constructor Create;
    //we have a method for providing a product name and version in case
    //we ever want to make another product.
    class Procedure ShowAboutBox(const AProductName, AProductName2, AProductVersion : String);
    class Procedure ShowDlg;
   property ProductName : String read GetProductName write SetProductName;
   property ProductName2 : String read GetProductName2 write SetProductName2;
   property Version : String read GetVersion write SetVersion;

  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TfrmAbout))]

implementation

uses
  IdDsnCoreResourceStrings, System.Diagnostics,
  IdGlobal, System.Reflection, System.Resources, SysUtils;

const
  ResourceBaseName = 'IdAboutNET';
{$R 'AboutIndyNET.resources'}

{$AUTOBOX ON}

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TfrmAbout.InitializeComponent;
begin
  Self.imgLogo := System.Windows.Forms.PictureBox.Create;
  Self.bbtnOk := System.Windows.Forms.Button.Create;
  Self.lblName := System.Windows.Forms.Label.Create;
  Self.lblName2 := System.Windows.Forms.Label.Create;
  Self.lblVersion := System.Windows.Forms.Label.Create;
  Self.lblCopyright := System.Windows.Forms.Label.Create;
  Self.lblBuiltFor := System.Windows.Forms.Label.Create;
  Self.lblLicense := System.Windows.Forms.Label.Create;
  Self.lblPleaseVisitUs := System.Windows.Forms.Label.Create;
  Self.lblURL := System.Windows.Forms.LinkLabel.Create;
  Self.SuspendLayout;
  //
  // imgLogo
  //
  Self.imgLogo.Location := System.Drawing.Point.Create(0, 0);
  Self.imgLogo.Name := 'imgLogo';
  Self.imgLogo.Size := System.Drawing.Size.Create(388, 240);
  Self.imgLogo.TabIndex := 0;
  Self.imgLogo.TabStop := False;
  //
  // bbtnOk
  //
  Self.bbtnOk.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.bbtnOk.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.bbtnOk.Location := System.Drawing.Point.Create(475, 302);
  Self.bbtnOk.Name := 'bbtnOk';
  Self.bbtnOk.TabIndex := 0;
  Self.bbtnOk.Text := 'Button1';
  //
  // lblName
  //
  Self.lblName.Font := System.Drawing.Font.Create('Arial Black', 14.25, System.Drawing.FontStyle.Regular,
      System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblName.Location := System.Drawing.Point.Create(51, 28);
  Self.lblName.Name := 'lblName';
  Self.lblName.Size := System.Drawing.Size.Create(200, 101);
  Self.lblName.TabIndex := 1;
  Self.lblName.Text := 'Label1';
  Self.lblName.TextAlign := System.Drawing.ContentAlignment.TopCenter;
  //
  // lblName2
  //
  Self.lblName.Font := System.Drawing.Font.Create('Arial', 14.25, System.Drawing.FontStyle.Regular,
      System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblName.Location := System.Drawing.Point.Create(54, 110);
  Self.lblName.Name := 'lblName';
  Self.lblName.Size := System.Drawing.Size.Create(192, 35);
  Self.lblName.TabIndex := 2;
  Self.lblName.Text := 'Label2';
  Self.lblName.TextAlign := System.Drawing.ContentAlignment.TopCenter;
  //
  // lblVersion
  //
  Self.lblVersion.Font := System.Drawing.Font.Create('Arial', 14.25,
      System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblVersion.Location := System.Drawing.Point.Create(300, 170);
  Self.lblVersion.Name := 'lblVersion';
  Self.lblVersion.Size := System.Drawing.Size.Create(200, 17);
  Self.lblVersion.TabIndex := 3;
  Self.lblVersion.Text := 'Label3';
  Self.lblVersion.TextAlign := System.Drawing.ContentAlignment.TopRight;
  //
  // lblCopyright
  //
  Self.lblCopyright.Font := System.Drawing.Font.Create('Arial', 14.25,
      System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblCopyright.Location := System.Drawing.Point.Create(58, 171);
  Self.lblCopyright.Name := 'lblCopyright';
  Self.lblCopyright.Size := System.Drawing.Size.Create(138, 15);
  Self.lblCopyright.TabIndex := 6;
  Self.lblCopyright.Text := 'Label6';
  Self.lblCopyright.TextAlign := System.Drawing.ContentAlignment.TopCenter;
  //
  // lblBuiltFor
  //
  Self.lblBuiltFor.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.lblBuiltFor.Font := System.Drawing.Font.Create('Arial', 14.25,
      System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblBuiltFor.Location := System.Drawing.Point.Create(300, 188);
  Self.lblBuiltFor.Name := 'lblBuiltFor';
  Self.lblBuiltFor.Size := System.Drawing.Size.Create(200, 17);
  Self.lblBuiltFor.TabIndex := 4;
  Self.lblBuiltFor.Text := 'Label4';
  Self.lblBuiltFor.TextAlign := System.Drawing.ContentAlignment.TopRight;
  //
  // lblLicense
  //
  Self.lblLicense.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.lblLicense.Font := System.Drawing.Font.Create('Arial', 14.25,
      System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblLicense.Location := System.Drawing.Point.Create(300, 227);
  Self.lblLicense.Name := 'lblLicense';
  Self.lblLicense.Size := System.Drawing.Size.Create(200, 45);
  Self.lblLicense.TabIndex := 5;
  Self.lblLicense.Text := 'Label5';
  Self.lblBuiltFor.TextAlign := System.Drawing.ContentAlignment.TopRight;
  //
  // lblPleaseVisitUs
  //
  Self.lblPleaseVisitUs.Font := System.Drawing.Font.Create('Arial', 14.25,
      System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblPleaseVisitUs.Location := System.Drawing.Point.Create(58, 278);
  Self.lblPleaseVisitUs.Name := 'lblPleaseVisitUs';
  Self.lblPleaseVisitUs.Size := System.Drawing.Size.Create(276, 15);
  Self.lblPleaseVisitUs.TabIndex := 7;
  Self.lblPleaseVisitUs.Text := 'Label7';
  Self.lblPleaseVisitUs.TextAlign := System.Drawing.ContentAlignment.TopCenter;
  //
  // lblURL
  //
  Self.lblCopyright.Font := System.Drawing.Font.Create('Arial', 14.25,
      System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.lblURL.Location := System.Drawing.Point.Create(58, 292);
  Self.lblURL.Name := 'lblURL';
  Self.lblURL.Size := System.Drawing.Size.Create(141, 15);
  Self.lblURL.TabIndex := 8;
  Self.lblURL.TabStop := True;
  Self.lblURL.Text := 'LinkLabel8';
  Self.lblURL.TextAlign := System.Drawing.ContentAlignment.TopCenter;
  Include(Self.lblURL.LinkClicked, Self.lblURL_LinkClicked);
  //
  // TfrmAbout
  //
  Self.AcceptButton := Self.bbtnOk;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.bbtnOk;
  Self.ClientSize := System.Drawing.Size.Create(336, 554);
  Self.Controls.Add(Self.lblURL);
  Self.Controls.Add(Self.lblPleaseVisitUs);
  Self.Controls.Add(Self.lblCopyright);
  Self.Controls.Add(Self.lblVersion);
  Self.Controls.Add(Self.lblName);
  Self.Controls.Add(Self.lblName2);
  Self.Controls.Add(Self.lblBuiltFor);
  Self.Controls.Add(Self.lblLicense);
  Self.Controls.Add(Self.bbtnOk);
  Self.Controls.Add(Self.imgLogo);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfrmAbout';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'WinForm';
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TfrmAbout.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TfrmAbout.Create;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  //
  // TODO: Add any constructor code after InitializeComponent call
  //
  Self.Text := RSAAboutFormCaption;
  lblName.Text := RSAAboutBoxTitle1;
  lblName2.Text := RSAAboutBoxTitle2;
  lblBuiltFor.Text := IndyFormat(RSAAboutBoxBuiltFor, ['DotNET']);
  lblLicense.Text := RSAAboutBoxLicences;
  lblCopyright.Text := RSAAboutBoxCopyright;
  lblPleaseVisitUs.Text := RSAAboutBoxPleaseVisit;
  lblURL.Text := RSAAboutBoxIndyWebsite;
  lblURL.Links.Add(0, Length(RSAABoutBoxIndyWebsite), RSAAboutBoxIndyWebsite);
  bbtnOk.Text := RSOk;
  imgLogo.Image := LoadBitmap('AboutBackground.bmp');
end;

procedure TfrmAbout.SetProductName(const AValue : String);
begin
  Self.lblName.Text := AValue;
end;

procedure TfrmAbout.SetProductName2(const AValue : String);
begin
  Self.lblName2.Text := AValue;
end;

procedure TfrmAbout.SetVersion(const AValue: string);
begin
  Self.lblVersion.Text := AValue;
end;

function TfrmAbout.GetVersion: string;
begin
  Result := Self.lblVersion.Text;
end;

function TfrmAbout.GetProductName: string;
begin
  Result := Self.lblName.Text;
end;

function TfrmAbout.GetProductName2: string;
begin
  Result := Self.lblName2.Text;
end;

class procedure TfrmAbout.ShowAboutBox(const AProductName, AProductName2,
  AProductVersion: String);
begin
  with TfrmAbout.Create do
  try
    Version := IndyFormat(RSAAboutBoxVersion, [AProductVersion]);
    ProductName := AProductName;
    ProductName2 := AProductName2;
    Text := AProductName;
    ShowDialog;
  finally
    Dispose;
  end;
end;

class procedure TfrmAbout.ShowDlg;
begin
  ShowAboutBox(RSAAboutBoxTitle1, RSAAboutBoxTitle2, gsIdVersion);
end;

procedure TfrmAbout.lblURL_LinkClicked(sender: System.Object; e: System.Windows.Forms.LinkLabelLinkClickedEventArgs);
var
  LDest : String;
begin
  LDest := e.Link.LinkData as string;
  System.Diagnostics.Process.Start(LDest);
  e.Link.Visited := True;
end;

function TfrmAbout.LoadBitmap(AResName: string): Bitmap;
var
  LR: System.Resources.ResourceManager;
begin
  LR := System.Resources.ResourceManager.Create('AboutIndyNET', System.Reflection.Assembly.GetExecutingAssembly);
  Result := (Bitmap(LR.GetObject(AResName)));
end;

end.

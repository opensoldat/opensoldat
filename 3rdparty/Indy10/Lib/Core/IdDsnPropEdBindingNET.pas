unit IdDsnPropEdBindingNET;

interface

uses
  Classes,
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, IdSocketHandle;

type
  TIdDsnPropEdBindingNET = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    btnOk: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    lblBindings: System.Windows.Forms.Label;
    lbBindings: System.Windows.Forms.ListBox;
    btnNew: System.Windows.Forms.Button;
    btnDelete: System.Windows.Forms.Button;
    lblIPAddress: System.Windows.Forms.Label;
    edtIPAddress: System.Windows.Forms.ComboBox;
    lblPort: System.Windows.Forms.Label;
    edtPort: System.Windows.Forms.NumericUpDown;
    cboIPVersion: System.Windows.Forms.ComboBox;
    lblIPVersion: System.Windows.Forms.Label;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure btnNew_Click(sender: System.Object; e: System.EventArgs);
    procedure btnDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure edtPort_ValueChanged(sender: System.Object; e: System.EventArgs);
    procedure edtIPAddress_SelectedValueChanged(sender: System.Object; e: System.EventArgs);
    procedure cboIPVersion_SelectedValueChanged(sender: System.Object; e: System.EventArgs);
    procedure lbBindings_SelectedValueChanged(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  private
    FHandles : TIdSocketHandles;
    FDefaultPort : Integer;
    FIPv4Addresses : TStrings;
    FIPv6Addresses : TStrings;
    FCurrentHandle : TIdSocketHandle;

    { Private Declarations }
    procedure SetHandles(const Value: TIdSocketHandles);
    procedure SetIPv4Addresses(const Value: TStrings);
    procedure SetIPv6Addresses(const Value: TStrings);
    procedure UpdateBindingList;
    procedure UpdateEditControls;
    procedure FillComboBox(ACombo : System.Windows.Forms.ComboBox; AStrings :TStrings);
    procedure SetCaption(const AValue : String);
    function GetCaption : String;
  public
    constructor Create;
    function Execute : Boolean;
    function GetList: string;
    procedure SetList(const AList: string);
    property Handles : TIdSocketHandles read FHandles write SetHandles;
    property DefaultPort : Integer read FDefaultPort write FDefaultPort;
    property IPv4Addresses : TStrings read FIPv4Addresses write SetIPv4Addresses;
    property IPv6Addresses : TStrings read FIPv6Addresses write SetIPv6Addresses;
    property Caption : String read GetCaption write SetCaption;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TIdDsnPropEdBindingNET))]

procedure FillHandleList(const AList: string; ADest: TIdSocketHandles);
function GetListValues(const ASocketHandles : TIdSocketHandles) : String;

implementation
uses
  IdGlobal,
  IdIPAddress,
  IdDsnCoreResourceStrings, IdStack, SysUtils;

const
  IPv6Wildcard1 = '::';                 {do not localize}
  IPv6Wildcard2 = '0:0:0:0:0:0:0:0';    {do not localize}
  IPv6Loopback  = '::1';                {do not localize}
  IPv4Wildcard  = '0.0.0.0';            {do not localize}
  IPv4Loopback  = '127.0.0.1';          {do not localize}

function IsValidIP(const AAddr : String): Boolean;
var
  LIP: TIdIPAddress;
begin
  LIP :=  TIdIPAddress.MakeAddressObject(AAddr);
  Result := Assigned(LIP);
  if Result then
  begin
    FreeAndNil(LIP);
  end;
end;

function StripAndSymbol(s : String) : String;
begin
  Result := '';
  repeat
    if s='' then
    begin
      Break;
    end;
    Result := Result + Fetch(s,'&');
  until False;
end;

procedure FillHandleList(const AList: string; ADest: TIdSocketHandles);
var
  LItems: TStringList;
  i: integer;
  LIPVersion: TIdIPVersion;
  LAddr, LText: string;
  LPort: integer;
begin
  ADest.BeginUpdate;
  try
    ADest.Clear;
    LItems := TStringList.Create;
    try
      LItems.CommaText := AList;
      for i := 0 to LItems.Count-1 do begin
        if Length(LItems[i]) > 0 then begin
          if TextStartsWith(LItems[i], '[') then begin
            //  ipv6
            LIPVersion := Id_IPv6;
            LText := Copy(LItems[i], 2, MaxInt);
            LAddr := Fetch(LText, ']:');
            LPort := IndyStrToInt(LText, -1);
          end else begin
            // ipv4
            LIPVersion := Id_IPv4;
            LText := LItems[i];
            LAddr := Fetch(LText, ':');
            LPort := IndyStrToInt(LText, -1);
            //Note that 0 is legal and indicates the server binds to a random port
          end;
          if IsValidIP(LAddr) and (LPort > -1) and (LPort < 65536) then begin
            with ADest.Add do begin
              IPVersion := LIPVersion;
              IP := LAddr;
              Port := LPort;
            end;
          end;
        end;
      end;
    finally
      LItems.Free;
    end;
  finally
    ADest.EndUpdate;
  end;
end;

function NumericOnly(const AText : String) : String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AText) do
  begin
    if IsNumeric(AText[i]) then
    begin
      Result := Result + AText[i];
    end
    else
    begin
      Break;
    end;
  end;
  if (Length(Result) = 0) then
  begin
    Result := '0';
  end;
end;

function IndexOfNo(const ANo : Integer; AItems : System.Windows.Forms.ComboBox.ObjectCollection) : Integer;
begin
  for Result := 0 to AItems.Count -1 do
  begin
    if ANo = IndyStrToInt( NumericOnly(AItems[Result].ToString )) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function GetDisplayString(ASocketHandle: TIdSocketHandle): string;
begin
  Result := '';
  case ASocketHandle.IPVersion of
    Id_IPv4 : Result := IndyFormat('%s:%d', [ASocketHandle.IP, ASocketHandle.Port]);
    Id_IPv6 : Result := IndyFormat('[%s]:%d', [ASocketHandle.IP, ASocketHandle.Port]);
  end;
end;

function GetListValues(const ASocketHandles : TIdSocketHandles) : String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to ASocketHandles.Count -1 do begin
    Result := Result + ',' + GetDisplayString(ASocketHandles[i]);
  end;
  Delete(Result,1,1);
end;

{$AUTOBOX ON}

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TIdDsnPropEdBindingNET.InitializeComponent;
type
  TArrayOfInteger = array of Integer;
begin
  Self.btnOk := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.lblBindings := System.Windows.Forms.Label.Create;
  Self.lbBindings := System.Windows.Forms.ListBox.Create;
  Self.btnNew := System.Windows.Forms.Button.Create;
  Self.btnDelete := System.Windows.Forms.Button.Create;
  Self.lblIPAddress := System.Windows.Forms.Label.Create;
  Self.edtIPAddress := System.Windows.Forms.ComboBox.Create;
  Self.lblPort := System.Windows.Forms.Label.Create;
  Self.edtPort := System.Windows.Forms.NumericUpDown.Create;
  Self.cboIPVersion := System.Windows.Forms.ComboBox.Create;
  Self.lblIPVersion := System.Windows.Forms.Label.Create;
  (System.ComponentModel.ISupportInitialize(Self.edtPort)).BeginInit;
  Self.SuspendLayout;
  // 
  // btnOk
  // 
  Self.btnOk.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.btnOk.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.btnOk.Location := System.Drawing.Point.Create(312, 160);
  Self.btnOk.Name := 'btnOk';
  Self.btnOk.TabIndex := 0;
  // 
  // btnCancel
  // 
  Self.btnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Location := System.Drawing.Point.Create(392, 160);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.TabIndex := 1;
  // 
  // lblBindings
  // 
  Self.lblBindings.AutoSize := True;
  Self.lblBindings.Location := System.Drawing.Point.Create(8, 8);
  Self.lblBindings.Name := 'lblBindings';
  Self.lblBindings.Size := System.Drawing.Size.Create(42, 16);
  Self.lblBindings.TabIndex := 2;
  Self.lblBindings.Text := '&Binding';
  // 
  // lbBindings
  // 
  Self.lbBindings.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top 
    or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left)));
  Self.lbBindings.Location := System.Drawing.Point.Create(8, 24);
  Self.lbBindings.Name := 'lbBindings';
  Self.lbBindings.Size := System.Drawing.Size.Create(137, 121);
  Self.lbBindings.TabIndex := 3;
  Include(Self.lbBindings.SelectedValueChanged, Self.lbBindings_SelectedValueChanged);
  // 
  // btnNew
  // 
  Self.btnNew.Location := System.Drawing.Point.Create(152, 56);
  Self.btnNew.Name := 'btnNew';
  Self.btnNew.TabIndex := 4;
  Include(Self.btnNew.Click, Self.btnNew_Click);
  // 
  // btnDelete
  // 
  Self.btnDelete.Location := System.Drawing.Point.Create(152, 88);
  Self.btnDelete.Name := 'btnDelete';
  Self.btnDelete.TabIndex := 5;
  Include(Self.btnDelete.Click, Self.btnDelete_Click);
  // 
  // lblIPAddress
  // 
  Self.lblIPAddress.Location := System.Drawing.Point.Create(240, 8);
  Self.lblIPAddress.Name := 'lblIPAddress';
  Self.lblIPAddress.Size := System.Drawing.Size.Create(100, 16);
  Self.lblIPAddress.TabIndex := 6;
  Self.lblIPAddress.Text := 'Label1';
  //
  // edtIPAddress
  // 
  Self.edtIPAddress.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top 
    or System.Windows.Forms.AnchorStyles.Left) or System.Windows.Forms.AnchorStyles.Right)));
  Self.edtIPAddress.Location := System.Drawing.Point.Create(240, 24);
  Self.edtIPAddress.Name := 'edtIPAddress';
  Self.edtIPAddress.Size := System.Drawing.Size.Create(224, 21);
  Self.edtIPAddress.TabIndex := 7;
  Include(Self.edtIPAddress.SelectedValueChanged, Self.edtIPAddress_SelectedValueChanged);
  // 
  // lblPort
  // 
  Self.lblPort.Location := System.Drawing.Point.Create(240, 58);
  Self.lblPort.Name := 'lblPort';
  Self.lblPort.Size := System.Drawing.Size.Create(100, 16);
  Self.lblPort.TabIndex := 8;
  Self.lblPort.Text := 'Label1';
  // 
  // edtPort
  // 
  Self.edtPort.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top 
    or System.Windows.Forms.AnchorStyles.Left) or System.Windows.Forms.AnchorStyles.Right)));
  Self.edtPort.Location := System.Drawing.Point.Create(240, 74);
  Self.edtPort.Maximum := System.Decimal.Create(TArrayOfInteger.Create(65535, 
          0, 0, 0));
  Self.edtPort.Name := 'edtPort';
  Self.edtPort.Size := System.Drawing.Size.Create(224, 20);
  Self.edtPort.TabIndex := 9;
  Include(Self.edtPort.ValueChanged, Self.edtPort_ValueChanged);
  //
  // cboIPVersion
  // 
  Self.cboIPVersion.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cboIPVersion.Location := System.Drawing.Point.Create(240, 124);
  Self.cboIPVersion.Name := 'cboIPVersion';
  Self.cboIPVersion.Size := System.Drawing.Size.Create(224, 21);
  Self.cboIPVersion.TabIndex := 10;
  Include(Self.cboIPVersion.SelectedValueChanged, Self.cboIPVersion_SelectedValueChanged);
  // 
  // lblIPVersion
  // 
  Self.lblIPVersion.Location := System.Drawing.Point.Create(240, 108);
  Self.lblIPVersion.Name := 'lblIPVersion';
  Self.lblIPVersion.Size := System.Drawing.Size.Create(100, 16);
  Self.lblIPVersion.TabIndex := 11;
  Self.lblIPVersion.Text := 'Label1';
  // 
  // TIdDsnPropEdBindingNET
  // 
  Self.AcceptButton := Self.btnOk;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(470, 189);
  Self.Controls.Add(Self.lblIPVersion);
  Self.Controls.Add(Self.cboIPVersion);
  Self.Controls.Add(Self.edtPort);
  Self.Controls.Add(Self.lblPort);
  Self.Controls.Add(Self.edtIPAddress);
  Self.Controls.Add(Self.lblIPAddress);
  Self.Controls.Add(Self.btnDelete);
  Self.Controls.Add(Self.btnNew);
  Self.Controls.Add(Self.lbBindings);
  Self.Controls.Add(Self.lblBindings);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.btnOk);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MaximumSize := System.Drawing.Size.Create(480, 225);
  Self.MinimizeBox := False;
  Self.MinimumSize := System.Drawing.Size.Create(480, 225);
  Self.Name := 'TIdDsnPropEdBindingNET';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'WinForm';
  (System.ComponentModel.ISupportInitialize(Self.edtPort)).EndInit;
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TIdDsnPropEdBindingNET.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
    begin
      Components.Dispose();
      FreeAndNil(FHandles);

      FreeAndNil(FIPv4Addresses);
      FreeAndNil(FIPv6Addresses);

      //don't free FCurrentHandle; - it's in the handles collection
      TIdStack.DecUsage;
    end;
  end;
  inherited Dispose(Disposing);

end;

constructor TIdDsnPropEdBindingNET.Create;
var
  i: Integer;
  LLocalAddresses: TIdStackLocalAddressList;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  //
  // TODO: Add any constructor code after InitializeComponent call
  //
  FHandles := TIdSocketHandles.Create(nil);
  FIPv4Addresses := TStringList.Create;
  FIPv6Addresses := TStringList.Create;
  SetIPv4Addresses(nil);
  SetIPv6Addresses(nil);

  TIdStack.IncUsage;
  try
    LLocalAddresses := TIdStackLocalAddressList.Create;
    try
      GStack.GetLocalAddressList(LLocalAddresses);
      for i := 0 to LLocalAddresses.Count-1 do
      begin
        case LLocalAddresses[i].IPVersion of
          Id_IPv4: FIPv4Addresses.Add(LLocalAddresses[i].IPAddress);
          Id_IPv6: FIPv6Addresses.Add(LLocalAddresses[i].IPAddress);
        end;
      end;
    finally
      LLocalAddresses.Free;
    end;
  finally
    TIdStack.DecUsage;
  end;

  UpdateEditControls;
  //captions
  btnNew.Text := RSBindingNewCaption;
  btnDelete.Text := RSBindingDeleteCaption;
  lblIPAddress.Text := RSBindingHostnameLabel;
  lblPort.Text := RSBindingPortLabel;
  lblIPVersion.Text := RSBindingIPVerLabel;
  btnOk.Text := RSOk;
  btnCancel.Text := RSCancel;
  //IPVersion choices
  //we yhave to strip out the & symbol.  In Win32, we use this
  //in a radio-box so a user could select by pressingg the 4 or 6
  //key.  For this, we don't have a radio box and I'm too lazy
  //to use two Radio Buttons.
  cboIPVersion.Items.Add(StripAndSymbol(RSBindingIPV4Item));
  cboIPVersion.Items.Add(StripAndSymbol(RSBindingIPV6Item));
end;

procedure TIdDsnPropEdBindingNET.SetHandles(const Value: TIdSocketHandles);
begin
  FHandles.Assign(Value);
  UpdateBindingList;
end;

function TIdDsnPropEdBindingNET.GetList: string;
begin
  Result := GetListValues(Handles);
end;

procedure TIdDsnPropEdBindingNET.SetIPv6Addresses(const Value: TStrings);
begin
  if Assigned(Value) then begin
    FIPv6Addresses.Assign(Value);
  end;
  // Ensure that these two are always present
  if FIPv6Addresses.IndexOf(IPv6Loopback) = -1 then begin
    FIPv6Addresses.Insert(0, IPv6Loopback);
  end;
  if FIPv6Addresses.IndexOf(IPv6Wildcard1) = -1 then begin
    FIPv6Addresses.Insert(0, IPv6Wildcard1);
  end;
end;

procedure TIdDsnPropEdBindingNET.SetIPv4Addresses(const Value: TStrings);
begin
  if Assigned(Value) then begin
    FIPv4Addresses.Assign(Value);
  end;
  // Ensure that these two are always present
  if FIPv4Addresses.IndexOf(IPv6Loopback) = -1 then begin
    FIPv4Addresses.Insert(0, IPv4Loopback);
  end;
  if FIPv4Addresses.IndexOf(IPv4Wildcard) = -1 then begin
    FIPv4Addresses.Insert(0, IPv4Wildcard);
  end;
end;

procedure TIdDsnPropEdBindingNET.SetList(const AList: string);
begin
  FCurrentHandle := nil;
  FillHandleList(AList, Handles);
  UpdateBindingList;
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingNET.lbBindings_SelectedValueChanged(sender: System.Object;
  e: System.EventArgs);
begin
  if lbBindings.SelectedIndex >= 0 then begin
    btnDelete.Enabled := True;
    FCurrentHandle := FHandles[lbBindings.SelectedIndex];
  end else begin
    btnDelete.Enabled := False;
    FCurrentHandle := nil;
  end;
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingNET.cboIPVersion_SelectedValueChanged(sender: System.Object;
  e: System.EventArgs);
begin
  case cboIPVersion.SelectedIndex of
    0 :
    begin
      if FCurrentHandle.IPVersion <> Id_IPv4 then
      begin
        FCurrentHandle.IPVersion := Id_IPv4;
        FillComboBox(edtIPAddress,FIPv4Addresses);
        FCurrentHandle.IP := IPv4Wildcard;
      end;
    end;
    1 :
    begin
      if FCurrentHandle.IPVersion <> Id_IPv6 then
      begin
        FCurrentHandle.IPVersion := Id_IPv6;
        FillComboBox(edtIPAddress,FIPv6Addresses);
        FCurrentHandle.IP := IPv6Wildcard1;
      end;
    end;
  end;
  UpdateEditControls;
  UpdateBindingList;
end;

procedure TIdDsnPropEdBindingNET.edtIPAddress_SelectedValueChanged(sender: System.Object;
  e: System.EventArgs);
begin
   FCurrentHandle.IP := edtIPAddress.SelectedItem.ToString;
  UpdateBindingList;
end;

procedure TIdDsnPropEdBindingNET.edtPort_ValueChanged(sender: System.Object;
  e: System.EventArgs);
begin
  if Assigned(FCurrentHandle) then begin
    FCurrentHandle.Port := edtPort.Value.ToInt16(edtPort.Value);
  end;
  UpdateBindingList;
end;

procedure TIdDsnPropEdBindingNET.btnDelete_Click(sender: System.Object; e: System.EventArgs);
var LSH : TIdSocketHandle;
  i : Integer;
begin
  if lbBindings.SelectedIndex >= 0 then
  begin
    // Delete is not available in D4's collection classes
    // This should work just as well.
    i := lbBindings.get_SelectedIndex;
    LSH := Handles[i];
    FreeAndNil(LSH);
    lbBindings.Items.Remove(i);
    FCurrentHandle := nil;
    UpdateBindingList;
  end;
  lbBindings_SelectedValueChanged(nil, nil);
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingNET.btnNew_Click(sender: System.Object; e: System.EventArgs);
begin
  FCurrentHandle := FHandles.Add;
  case FCurrentHandle.IPVersion of
    Id_IPv4: FCurrentHandle.IP := IPv4Wildcard;
    Id_IPv6: FCurrentHandle.IP := IPv6Wildcard1;
  end;
  FCurrentHandle.Port := FDefaultPort;
  UpdateBindingList;
  FillComboBox(edtIPAddress, FIPv4Addresses);
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingNET.UpdateBindingList;
var
  i: integer;
  selected: integer;
  s: string;
begin
  selected := lbBindings.SelectedIndex;
  lbBindings.BeginUpdate;
  try
   if lbBindings.Items.Count = FHandles.Count then begin
     for i := 0 to FHandles.Count - 1 do begin
       s := GetDisplayString(FHandles[i]);
       if s <> lbBindings.Items[i].ToString then begin
         lbBindings.Items[i] := s;
       end;
     end;
   end else begin
     lbBindings.Items.Clear;
     for i := 0 to FHandles.Count-1 do begin
       lbBindings.Items.Add(GetDisplayString(FHandles[i]));
     end;
   end;
  finally
    lbBindings.EndUpdate;
    if Assigned(FCurrentHandle) then begin
      lbBindings.SelectedIndex := FCurrentHandle.Index;
    end else begin
      lbBindings.SelectedIndex := IndyMin(selected, lbBindings.Items.Count-1);
    end;
  end;
{  selected := lbBindings.SelectedItem;
  lbBindings.Items.BeginUpdate;
  try
    if lbBindings.Items.Count = FHandles.Count then begin
      for i := 0 to FHandles.Count - 1 do begin
        s := GetDisplayString(FHandles[i]);
        if s <> lbBindings.Items[i] then begin
          lbBindings.Items[i] := s;
        end;
      end;
    end else begin
      lbBindings.Items.Clear;
      for i := 0 to FHandles.Count-1 do begin
        lbBindings.Items.Add(GetDisplayString(FHandles[i]));
      end;
    end;
  finally
    lbBindings.Items.EndUpdate;
    if Assigned(FCurrentHandle) then begin
      lbBindings.ItemIndex := FCurrentHandle.Index;
    end else begin
      lbBindings.ItemIndex := IndyMin(selected, lbBindings.Items.Count-1);
    end;
  end;      }
end;

procedure TIdDsnPropEdBindingNET.UpdateEditControls;
begin
  if Assigned(FCurrentHandle) then
  begin
    edtPort.Text := '';
    edtPort.Value := FCurrentHandle.Port;
    case FCurrentHandle.IPVersion of
      Id_IPv4 :
      begin
        FillComboBox(edtIPAddress, FIPv4Addresses);
        edtIPAddress.SelectedItem := edtIPAddress.Items[0];
        cboIPVersion.SelectedItem := cboIPVersion.Items[0];
      end;
      Id_IPv6 :
      begin
        FillComboBox(edtIPAddress, FIPv6Addresses);
        edtIPAddress.SelectedItem := edtIPAddress.Items[0];
        cboIPVersion.SelectedItem := cboIPVersion.Items[1];
      end;
    end;
    if edtIPAddress.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDown then begin
      edtIPAddress.Text := FCurrentHandle.IP;
    end else begin
      edtIPAddress.SelectedIndex := edtIPAddress.Items.IndexOf(FCurrentHandle.IP);
    end;
  end;

  lblIPAddress.Enabled := Assigned(FCurrentHandle);
  edtIPAddress.Enabled := Assigned(FCurrentHandle);
  lblPort.Enabled := Assigned(FCurrentHandle);
  edtPort.Enabled := Assigned(FCurrentHandle);
  lblIPVersion.Enabled := Assigned(FCurrentHandle);
  cboIPVersion.Enabled := Assigned(FCurrentHandle);
end;

procedure TIdDsnPropEdBindingNET.FillComboBox(
  ACombo: System.Windows.Forms.ComboBox; AStrings: TStrings);
var
  i : Integer;
begin
  ACombo.Items.Clear;
  for i := 0 to AStrings.Count-1 do begin
    ACombo.Items.Add(AStrings[i]);
  end;
end;

function TIdDsnPropEdBindingNET.Execute: Boolean;
begin
  Result := Self.ShowDialog = System.Windows.Forms.DialogResult.OK;
end;

function TIdDsnPropEdBindingNET.GetCaption: String;
begin
  Result := Text;
end;

procedure TIdDsnPropEdBindingNET.SetCaption(const AValue: String);
begin
  Text := AValue;
end;

end.

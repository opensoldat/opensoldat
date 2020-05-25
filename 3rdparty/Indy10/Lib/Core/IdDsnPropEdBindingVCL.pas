{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.9    10/26/2004 8:45:26 PM  JPMugaas
  Should compile.

  Rev 1.8    10/26/2004 8:42:58 PM  JPMugaas
  Should be more portable with new references to TIdStrings and TIdStringList.

  Rev 1.7    5/19/2004 10:44:28 PM  DSiders
  Corrected spelling for TIdIPAddress.MakeAddressObject method.

  Rev 1.6    2/3/2004 11:34:26 AM  JPMugaas
  Should compile.

  Rev 1.5.1.0    2/3/2004 11:32:26 AM  JPMugaas
  Should compile.

  Rev 1.5    2/1/2004 2:44:20 AM  JPMugaas
  Bindings editor should be fully functional including IPv6 support.

  Rev 1.4    2/1/2004 1:03:34 AM  JPMugaas
  This now work properly in both Win32 and DotNET.  The behavior had to change
  in DotNET because of some missing functionality and because implementing that
  functionality creates more problems than it would solve.

  Rev 1.3    2003.12.31 10:42:22 PM  czhower
  Warning removed

  Rev 1.2    10/15/2003 10:12:32 PM  DSiders
  Added localization comments.

  Rev 1.1    2003.10.11 5:47:46 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.0    11/13/2002 08:43:58 AM  JPMugaas
}

unit IdDsnPropEdBindingVCL;

interface

{$I IdCompilerDefines.inc}

uses
  Classes,
{$IFDEF WIDGET_KYLIX}
  QActnList, QStdCtrls, QForms, QExtCtrls, QControls, QComCtrls, QGraphics, Qt,
{$ENDIF}
{$IFDEF WIDGET_VCL_LIKE}
  ActnList, StdCtrls, Buttons, ExtCtrls, Graphics, Controls, ComCtrls, Forms, Dialogs,
{$ENDIF}
{$IFDEF HAS_UNIT_Types}
  Types,
{$ENDIF}
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LCL}
  LResources,
{$ENDIF}
  IdSocketHandle;
{
  Design Note:  It turns out that in DotNET, there are no services file functions and
  IdPorts does not work as expected in DotNET.  It is probably possible to read the
  services file ourselves but that creates some portability problems as the placement
  is different in every operating system.

  e.g.

  Linux and Unix-like systems - /etc
  Windows 95, 98, and ME - c:\windows
  Windows NT systems - c:\winnt\system32\drivers\etc

  Thus, it will undercut whatever benefit we could get with DotNET.

  About the best I could think of is to use an edit control because
  we can't offer anything from the services file in DotNET.

  TODO:  Maybe there might be a way to find the location in a more elegant
  manner than what I described.
}

type
  TIdDsnPropEdBindingVCL = class(TForm)
   {$IFDEF USE_TBitBtn}
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
  {$ELSE}
    btnOk: TButton;
    btnCancel: TButton;
  {$ENDIF}
    lblBindings: TLabel;
    edtPort: TComboBox;
    rdoBindingType: TRadioGroup;
    lblIPAddress: TLabel;
    lblPort: TLabel;
    btnNew: TButton;
    btnDelete: TButton;
    ActionList1: TActionList;
    btnBindingsNew: TAction;
    btnBindingsDelete: TAction;
    edtIPAddress: TComboBox;
    lbBindings: TListBox;
    procedure btnBindingsNewExecute(Sender: TObject);
    procedure btnBindingsDeleteExecute(Sender: TObject);
    procedure btnBindingsDeleteUpdate(Sender: TObject);
    procedure edtPortKeyPress(Sender: TObject; var Key: Char);
    procedure edtIPAddressChange(Sender: TObject);
    procedure edtPortChange(Sender: TObject);
    procedure rdoBindingTypeClick(Sender: TObject);
    procedure lbBindingsClick(Sender: TObject);
  private
    procedure SetHandles(const Value: TIdSocketHandles);
    procedure SetIPv4Addresses(const Value: TStrings);
    procedure SetIPv6Addresses(const Value: TStrings);
    procedure UpdateBindingList;
  protected
    FInUpdateRoutine : Boolean;
    FHandles : TIdSocketHandles;
    FDefaultPort : Integer;
    FIPv4Addresses : TStrings;
    FIPv6Addresses : TStrings;
    fCreatedStack : Boolean;
    FCurrentHandle : TIdSocketHandle;
    procedure UpdateEditControls;
    function PortDescription(const PortNumber: integer): string;
  public
    Constructor Create(AOwner : TComponent); overload; override;
    constructor Create; reintroduce; overload;
    Destructor Destroy; override;
    function Execute : Boolean;
    function GetList: string;
    procedure SetList(const AList: string);
    property Handles : TIdSocketHandles read FHandles write SetHandles;
    property DefaultPort : Integer read FDefaultPort write FDefaultPort;
    property IPv4Addresses : TStrings read FIPv4Addresses write SetIPv4Addresses;
    property IPv6Addresses : TStrings read FIPv6Addresses write SetIPv6Addresses;
  end;

var
  IdPropEdBindingEntry: TIdDsnPropEdBindingVCL;

procedure FillHandleList(const AList: string; ADest: TIdSocketHandles);
function GetListValues(const ASocketHandles : TIdSocketHandles) : String;

implementation

uses
  IdGlobal,
  IdIPAddress,
  IdDsnCoreResourceStrings,
  IdStack,
  IdStackBSDBase,
  SysUtils;

const
  IPv6Wildcard1 = '::';                 {do not localize}
  IPv6Wildcard2 = '0:0:0:0:0:0:0:0';    {do not localize}
  IPv6Loopback  = '::1';                {do not localize}
  IPv4Wildcard  = '0.0.0.0';            {do not localize}
  IPv4Loopback  = '127.0.0.1';          {do not localize}

function IsValidIP(const AAddr : String): Boolean;
var
  LIP : TIdIPAddress;
begin
  LIP := TIdIPAddress.MakeAddressObject(AAddr);
  Result := Assigned(LIP);
  if Result then begin
    FreeAndNil(LIP);
  end;
end;

procedure FillHandleList(const AList: string; ADest: TIdSocketHandles);
var
  LItems: TStringList;
  i: integer;
  LIPVersion: TIdIPVersion;
  LAddr, LText: string;
  LPort: integer;
  LSocket: TIdSocketHandle;
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
            LPort := StrToIntDef(LText, -1);
          end else begin
            // ipv4
            LIPVersion := Id_IPv4;
            LText := LItems[i];
            LAddr := Fetch(LText, ':');
            LPort := StrToIntDef(LText, -1);
            //Note that 0 is legal and indicates the server binds to a random port
          end;
          if IsValidIP(LAddr) and (LPort > -1) and (LPort < 65536) then begin
            LSocket := ADest.Add;
            LSocket.IPVersion := LIPVersion;
            LSocket.IP := LAddr;
            LSocket.Port := LPort;
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

{ TIdDsnPropEdBindingVCL }

function NumericOnly(const AText : String) : String;
var
  i : Integer;
begin
  Result := '';
  for i := 1 to Length(AText) do
  begin
    if IsNumeric(AText[i]) then begin
      Result := Result + AText[i];
    end else begin
      Break;
    end;
  end;
  if Length(Result) = 0 then begin
    Result := '0';
  end;
end;

function IndexOfNo(const ANo : Integer; AStrings : TStrings) : Integer;
begin
  for Result := 0 to AStrings.Count-1 do
  begin
    if ANo = IndyStrToInt(NumericOnly(AStrings[Result])) then begin
      Exit;
    end;
  end;
  Result := -1;
end;

function GetDisplayString(ASocketHandle: TIdSocketHandle): string;
begin
  Result := '';
  case ASocketHandle.IPVersion of
    Id_IPv4 : Result := Format('%s:%d',[ASocketHandle.IP, ASocketHandle.Port]);
    Id_IPv6 : Result := Format('[%s]:%d',[ASocketHandle.IP, ASocketHandle.Port]);
  end;
end;

function GetListValues(const ASocketHandles : TIdSocketHandles) : String;
var i : Integer;
begin
  Result := '';
  for i := 0 to ASocketHandles.Count -1 do begin
    Result := Result + ',' + GetDisplayString(ASocketHandles[i]);
  end;
  Delete(Result,1,1);
end;

constructor TIdDsnPropEdBindingVCL.Create(AOwner: TComponent);
var
  i : Integer;
  LLocalAddresses: TIdStackLocalAddressList;
begin
  inherited CreateNew(AOwner, 0);
  {$IFNDEF WIDGET_KYLIX}
  Borderstyle := bsDialog;
  {$ENDIF}
  BorderIcons := [biSystemMenu];
 // Width := 480;
 // Height := 252;
  ClientWidth  := 472;
  {$IFDEF USE_TBitBtn}
  ClientHeight := 230;
  {$ELSE}
  ClientHeight := 225;
  {$ENDIF}
  Constraints.MaxWidth := Width;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Position := poScreenCenter;
  lblBindings := TLabel.Create(Self);
  lbBindings := TListBox.Create(Self);
  ActionList1 := TActionList.Create(Self);
  btnBindingsNew := TAction.Create(Self);
  btnBindingsDelete := TAction.Create(Self);
  btnNew := TButton.Create(Self);
  btnDelete := TButton.Create(Self);
  lblIPAddress := TLabel.Create(Self);
  edtIPAddress := TComboBox.Create(Self);
  lblPort := TLabel.Create(Self);

  edtPort := TComboBox.Create(Self);
  rdoBindingType := TRadioGroup.Create(Self);

  {$IFDEF USE_TBitBtn}
  btnOk := TBitBtn.Create(Self);
  btnCancel := TBitBtn.Create(Self);
  {$ELSE}
  btnOk := TButton.Create(Self);
  btnCancel := TButton.Create(Self);
  {$ENDIF}

  lblBindings.Name := 'lblBindings';  {do not localize}
  lblBindings.Parent := Self;
  lblBindings.Left := 8;
  lblBindings.Top := 8;
  lblBindings.Width := 35;
  lblBindings.Height := 13;
  lblBindings.Caption := '&Binding';  {do not localize}

  lbBindings.Name := 'lbBindings';   {do not localize}
  lbBindings.Parent := Self;
  lbBindings.Left := 8;
  lbBindings.Top := 24;
  lbBindings.Width := 137;
  lbBindings.Height := 161;
  lbBindings.ItemHeight := 13;
  lbBindings.TabOrder := 8;
  lbBindings.OnClick := lbBindingsClick;

  ActionList1.Name := 'ActionList1';  {do not localize}
  {
  ActionList1.Left := 152;
  ActionList1.Top := 32;
  }

  btnBindingsNew.Name := 'btnBindingsNew'; {do not localize}
  btnBindingsNew.Caption := RSBindingNewCaption;
  btnBindingsNew.OnExecute := btnBindingsNewExecute;

  btnBindingsDelete.Name := 'btnBindingsDelete';  {do not localize}
  btnBindingsDelete.Caption := RSBindingDeleteCaption;
  btnBindingsDelete.OnExecute := btnBindingsDeleteExecute;
  btnBindingsDelete.OnUpdate := btnBindingsDeleteUpdate;

  btnNew.Name := 'btnNew'; {do not localize}
  btnNew.Parent := Self;
  btnNew.Left := 152;
  btnNew.Top := 72;
  btnNew.Width := 75;
  btnNew.Height := 25;
  btnNew.Action := btnBindingsNew;
  btnNew.TabOrder := 6;

  btnDelete.Name := 'btnDelete';  {do not localize}
  btnDelete.Parent := Self;
  btnDelete.Left := 152;
  btnDelete.Top := 104;
  btnDelete.Width := 75;
  btnDelete.Height := 25;
  btnDelete.Action := btnBindingsDelete;
  btnDelete.TabOrder := 7;

  lblIPAddress.Name := 'lblIPAddress'; {do not localize}
  lblIPAddress.Parent := Self;
  lblIPAddress.Left := 240;
  lblIPAddress.Top := 8;
  lblIPAddress.Width := 54;
  lblIPAddress.Height := 13;
  lblIPAddress.Caption := RSBindingHostnameLabel;
  lblIPAddress.Enabled := False;

  edtIPAddress.Name := 'edtIPAddress'; {do not localize}
  edtIPAddress.Parent := Self;
  edtIPAddress.Left := 240;
  edtIPAddress.Top := 24;
  edtIPAddress.Width := 221;
  edtIPAddress.Height := 21;
  edtIPAddress.Enabled := False;
  edtIPAddress.ItemHeight := 13;
  edtIPAddress.TabOrder := 3;
  edtIPAddress.OnChange := edtIPAddressChange;

  lblPort.Name := 'lblPort';  {do not localize}
  lblPort.Parent := Self;
  lblPort.Left := 240;
  lblPort.Top := 56;
  lblPort.Width := 22;
  lblPort.Height := 13;
  lblPort.Caption := RSBindingPortLabel;
  lblPort.Enabled := False;
  lblPort.FocusControl := edtPort;

  edtPort.Name := 'edtPort';  {do not localize}
  edtPort.Parent := Self;
  edtPort.Left := 240;
  edtPort.Top := 72;
  edtPort.Width := 221;
  edtPort.Height := 21;
  edtPort.Enabled := False;
  edtPort.ItemHeight := 13;
  edtPort.TabOrder := 4;
  edtPort.OnChange := edtPortChange;
  edtPort.OnKeyPress := edtPortKeyPress;

  rdoBindingType.Name := 'rdoBindingType'; {do not localize}
  rdoBindingType.Parent := Self;
  rdoBindingType.Left := 240;
  rdoBindingType.Top := 120;
  rdoBindingType.Width := 221;
  rdoBindingType.Height := 65;
  rdoBindingType.Caption := RSBindingIPVerLabel;
  rdoBindingType.Enabled := False;
  rdoBindingType.Items.Add(RSBindingIPV4Item);
  rdoBindingType.Items.Add(RSBindingIPV6Item);
  rdoBindingType.TabOrder := 5;
  rdoBindingType.OnClick := rdoBindingTypeClick;

  btnOk.Name := 'btnOk';  {do not localize}
  btnOk.Parent := Self;
  btnOk.Anchors := [akRight, akBottom];
  btnOk.Left := 306;
  btnOk.Top := 193;
  btnOk.Width := 75;
  {$IFDEF USE_TBitBtn}
  btnOk.Height := 30;
  btnOk.Kind := bkOk;
  {$ELSE}
  btnOk.Height := 25;
  btnOk.Caption := RSOk;
  btnOk.Default := True;
  btnOk.ModalResult := 1;
  {$ENDIF}
  btnOk.TabOrder := 0;

  btnCancel.Name := 'btnCancel';  {do not localize}
  btnCancel.Parent := Self;
  btnCancel.Anchors := [akRight, akBottom];
  btnCancel.Left := 386;
  btnCancel.Top := 193;
  btnCancel.Width := 75;
  {$IFDEF USE_TBitBtn}
  btnCancel.Height := 30;
  btnCancel.Kind := bkCancel;
  {$ELSE}
  btnCancel.Height := 25;
  btnCancel.Cancel := True;
  btnCancel.Caption := RSCancel;
  btnCancel.ModalResult := 2;
  {$ENDIF}
  btnCancel.Anchors := [akRight, akBottom];
  btnCancel.TabOrder := 1;

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

  edtPort.Items.BeginUpdate;
  try
    edtPort.Items.Add(PortDescription(0));
    for i := 0 to IdPorts.Count - 1 do begin
      edtPort.Items.Add(
        PortDescription(
          {$IFDEF HAS_GENERICS_TList}
          IdPorts[i]
          {$ELSE}
          PtrInt(IdPorts[i])
          {$ENDIF}
        )
      );
    end;
  finally
    edtPort.Items.EndUpdate;
  end;

  AutoScroll := False;
  Caption := RSBindingFormCaption;
  {$IFDEF WIDGET_VCL}
  Scaled := False;
  {$ENDIF}
  Font.Color := clBtnText;
  Font.Height := -11;
  Font.Name := 'MS Sans Serif';    {Do not Localize}
  Font.Style := [];
  Position := poScreenCenter;
  PixelsPerInch := 96;
  FInUpdateRoutine := False;
  UpdateEditControls;
end;

destructor TIdDsnPropEdBindingVCL.Destroy;
begin
  FreeAndNil(FIPv4Addresses);
  FreeAndNil(FIPv6Addresses);
  FreeAndNil(FHandles);
  inherited Destroy;
end;

function TIdDsnPropEdBindingVCL.PortDescription(const PortNumber: integer): string;
var
  LList: TStringList;
begin
  if PortNumber = 0 then begin
    Result := IndyFormat('%d: %s', [PortNumber, RSBindingAny]);
  end else begin
    Result := '';    {Do not Localize}
    LList := TStringList.Create;
    try
      GBSDStack.AddServByPortToList(PortNumber, LList);
      if LList.Count > 0 then begin
        Result := Format('%d: %s', [PortNumber, LList.CommaText]);    {Do not Localize}
      end;
    finally
      LList.Free;
    end;
  end;
end;

procedure TIdDsnPropEdBindingVCL.SetHandles(const Value: TIdSocketHandles);
begin
  FHandles.Assign(Value);
  UpdateBindingList;
end;

procedure TIdDsnPropEdBindingVCL.btnBindingsNewExecute(Sender: TObject);
begin
  FCurrentHandle := FHandles.Add;
  case FCurrentHandle.IPVersion of
    Id_IPv4: FCurrentHandle.IP := IPv4Wildcard;
    Id_IPv6: FCurrentHandle.IP := IPv6Wildcard1;
  end;
  FCurrentHandle.Port := FDefaultPort;
  UpdateBindingList;
  edtIPAddress.Items.Assign(FIPv4Addresses);
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingVCL.UpdateEditControls;
var
  i : Integer;
begin
  if Assigned(FCurrentHandle) then
  begin
    i := IndexOfNo(FCurrentHandle.Port,edtPort.Items);
    if i = -1 then begin
      edtPort.Text := IntToStr(FCurrentHandle.Port);
    end else begin
      edtPort.ItemIndex := i;
    end;

    case FCurrentHandle.IPVersion of
      Id_IPv4 :
      begin
        rdoBindingType.ItemIndex := 0;
        edtIPAddress.Items.Assign(FIPv4Addresses);
      end;
      Id_IPv6 :
      begin
        rdoBindingType.ItemIndex := 1;
        edtIPAddress.Items.Assign(FIPv6Addresses);
      end;
    end;
    if edtIPAddress.Style = csDropDown then begin
      edtIPAddress.Text := FCurrentHandle.IP;
    end else begin
      edtIPAddress.ItemIndex := edtIPAddress.Items.IndexOf(FCurrentHandle.IP);
    end;
  end
  else
  begin
    edtIPAddress.Text := '';
    //in LCL, the line below caused an index out of range error.
    {$IFDEF WIDGET_VCL}
    edtPort.ItemIndex := -1; //-2;
    {$ENDIF}
    edtPort.Text := '';
  end;

  lblIPAddress.Enabled := Assigned(FCurrentHandle);
  edtIPAddress.Enabled := Assigned(FCurrentHandle);
  lblPort.Enabled := Assigned(FCurrentHandle);
  edtPort.Enabled := Assigned(FCurrentHandle);
  rdoBindingType.Enabled := Assigned(FCurrentHandle);
  {$IFDEF WIDGET_KYLIX}
  //WOrkaround for CLX quirk that might be Kylix 1
  for i := 0 to rdoBindingType.ControlCount -1 do begin
    rdoBindingType.Controls[i].Enabled := Assigned(FCurrentHandle);
  end;
  {$ENDIF}
  {$IFDEF WIDGET_VCL_LIKE}
  //The Win32 VCL does not change the control background to a greyed look
  //when controls are disabled.  This quirk is not present in CLX.
  if Assigned(FCurrentHandle) then
  begin
    edtIPAddress.Color := clWindow;
    edtPort.Color := clWindow;
  end else
  begin
    edtIPAddress.Color := clBtnFace;
    edtPort.Color := clBtnFace;
  end;
  {$ENDIF}
end;

procedure TIdDsnPropEdBindingVCL.btnBindingsDeleteExecute(Sender: TObject);
var
  LSH : TIdSocketHandle;
begin
  if lbBindings.ItemIndex >= 0 then
  begin
    // Delete is not available in D4's collection classes
    // This should work just as well.
    LSH := Handles[lbBindings.ItemIndex];
    FreeAndNil(LSH);
    FCurrentHandle := nil;
    UpdateBindingList;
  end;
  lbBindingsClick(nil);
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingVCL.btnBindingsDeleteUpdate(Sender: TObject);
begin
  btnBindingsDelete.Enabled := lbBindings.ItemIndex >= 0;
end;

procedure TIdDsnPropEdBindingVCL.SetIPv4Addresses(const Value: TStrings);
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

procedure TIdDsnPropEdBindingVCL.SetIPv6Addresses(const Value: TStrings);
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

procedure TIdDsnPropEdBindingVCL.edtPortKeyPress(Sender: TObject; var Key: Char);
begin
  // RLebeau 1/7/09: using Char() for #128-#255 because in D2009, the compiler
  // may change characters >= #128 from their Ansi codepage value to their true
  // Unicode codepoint value, depending on the codepage used for the source code.
  // For instance, #128 may become #$20AC...

  if (Key > Chr(31)) and (Key < Chr(128)) then begin
    if not IsNumeric(Key) then begin
      Key := #0;
    end;
  end;
end;

procedure TIdDsnPropEdBindingVCL.edtIPAddressChange(Sender: TObject);
begin
  FCurrentHandle.IP := edtIPAddress.Text;
  UpdateBindingList;
end;

procedure TIdDsnPropEdBindingVCL.edtPortChange(Sender: TObject);
begin
  if Assigned(FCurrentHandle) then begin
    FCurrentHandle.Port := IndyStrToInt(NumericOnly(edtPort.Text), 0);
  end;
  UpdateBindingList;
end;

procedure TIdDsnPropEdBindingVCL.rdoBindingTypeClick(Sender: TObject);
begin
  case rdoBindingType.ItemIndex of
    0 :
    begin
      if FCurrentHandle.IPVersion <> Id_IPv4 then
      begin
        FCurrentHandle.IPVersion := Id_IPv4;
        edtIPAddress.Items.Assign(FIPv4Addresses);
        FCurrentHandle.IP := IPv4Wildcard;
      end;
    end;
    1 :
    begin
      if FCurrentHandle.IPVersion <> Id_IPv6 then
      begin
        FCurrentHandle.IPVersion := Id_IPv6;
        edtIPAddress.Items.Assign(FIPv6Addresses);
        FCurrentHandle.IP := IPv6Wildcard1;
      end;
    end;
  end;
  UpdateEditControls;
  UpdateBindingList;
end;

function TIdDsnPropEdBindingVCL.GetList: string;
begin
  Result := GetListValues(Handles);
end;

procedure TIdDsnPropEdBindingVCL.lbBindingsClick(Sender: TObject);
begin
  if lbBindings.ItemIndex >= 0 then begin
    FCurrentHandle := FHandles[lbBindings.ItemIndex];
  end else begin
    FCurrentHandle := nil;
  end;
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingVCL.SetList(const AList: string);
begin
  FCurrentHandle := nil;
  FillHandleList(AList, Handles);
  UpdateBindingList;
  UpdateEditControls;
end;

procedure TIdDsnPropEdBindingVCL.UpdateBindingList;
var
  i: integer;
  selected: integer;
  s: string;
begin
//in Lazarus, for some odd reason, if you have more than one binding,
//the routine is called while the items are updated
  if FInUpdateRoutine then begin
    Exit;
  end;
  FInUpdateRoutine := True;
  try
    selected := lbBindings.ItemIndex;
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
    end;
  finally
    FInUpdateRoutine := False;
  end;
end;

function TIdDsnPropEdBindingVCL.Execute: Boolean;
begin
  Result := ShowModal = mrOk;
end;

constructor TIdDsnPropEdBindingVCL.Create;
begin
  Create(nil);
end;

end.

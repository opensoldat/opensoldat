{ Menus Import Unit }
Unit uPSC_menus;
{$I PascalScript.inc}
Interface
Uses uPSCompiler;

procedure SIRegisterTMENUITEMSTACK(CL: TPSPascalCompiler);
procedure SIRegisterTPOPUPLIST(CL: TPSPascalCompiler);
procedure SIRegisterTPOPUPMENU(CL: TPSPascalCompiler);
procedure SIRegisterTMAINMENU(CL: TPSPascalCompiler);
procedure SIRegisterTMENU(CL: TPSPascalCompiler);
procedure SIRegisterTMENUITEM(CL: TPSPascalCompiler);
procedure SIRegister_Menus(Cl: TPSPascalCompiler);

implementation

procedure SIRegisterTMENUITEMSTACK(CL: TPSPascalCompiler);
begin
	With cl.AddClassN(Cl.FindClass('TStack'),'TMenuItemStack') do
	begin
	  RegisterMethod('procedure ClearItem(AItem: TMenuItem)');
	end;
end;

procedure SIRegisterTPOPUPLIST(CL: TPSPascalCompiler);
begin
	With cl.AddClassN(Cl.FindClass('TList'),'TPopupList') do
	begin
		RegisterProperty('Window', 'HWND', iptr);
		RegisterMethod('procedure Add(Popup: TPopupMenu)');
		RegisterMethod('procedure Remove(Popup: TPopupMenu)');
	end;
end;

procedure SIRegisterTPOPUPMENU(CL: TPSPascalCompiler);
var
	cc: TPSCompileTimeClass;
begin
	With cl.AddClassN(Cl.FindClass('TMenu'),'TPopupMenu') do
	begin
		cc := Cl.FindClass('TLabel');
		if cc <> nil then
			RegisterProperty('PopupMenu', 'TPopupMenu', iptRW);
		with Cl.FindClass('TForm') do
		begin
			RegisterProperty('PopupMenu', 'TPopupMenu', iptRW);
		end;
	RegisterMethod('constructor Create(AOwner: TComponent)');
	RegisterMethod('procedure Popup(X, Y: Integer)');
	RegisterProperty('PopupComponent', 'TComponent', iptrw);
	RegisterProperty('Alignment', 'TPopupAlignment', iptrw);
	RegisterProperty('AutoPopup', 'Boolean', iptrw);
	RegisterProperty('HelpContext', 'THelpContext', iptrw);
    RegisterProperty('MenuAnimation', 'TMenuAnimation', iptrw);
    RegisterProperty('TrackButton', 'TTrackButton', iptrw);
    RegisterProperty('OnPopup', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegisterTMAINMENU(CL: TPSPascalCompiler);
begin
  With cl.AddClassN(Cl.FindClass('TMenu'),'TMainMenu') do
  begin
    RegisterMethod('procedure Merge(Menu: TMainMenu)');
    RegisterMethod('procedure Unmerge(Menu: TMainMenu)');
    RegisterMethod('procedure PopulateOle2Menu(SharedMenu: HMENU; Groups: array of Integer; var Widths: array of LongInt)');
    RegisterMethod('procedure GetOle2AcceleratorTable(var AccelTable: HACCEL; var AccelCount: Integer; Groups: array of Integer)');
    RegisterMethod('procedure SetOle2MenuHandle(Handle: HMENU)');
    RegisterProperty('AutoMerge', 'Boolean', iptrw);
  end;
end;

procedure SIRegisterTMENU(CL: TPSPascalCompiler);
begin
  With cl.AddClassN(Cl.FindClass('TComponent'),'TMenu') do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent)');
    RegisterMethod('function DispatchCommand(ACommand: Word): Boolean');
    RegisterMethod('function DispatchPopup(AHandle: HMENU): Boolean');
    RegisterMethod('function FindItem(Value: Integer; Kind: TFindItemKind): TMenuItem');
    RegisterMethod('function GetHelpContext(Value: Integer; ByCommand: Boolean): THelpContext');
    RegisterProperty('Images', 'TCustomImageList', iptrw);
    RegisterMethod('function IsRightToLeft: Boolean');
    RegisterMethod('procedure ParentBiDiModeChanged(AControl: TObject)');
    RegisterMethod('procedure ProcessMenuChar(var Message: TWMMenuChar)');
    RegisterProperty('AutoHotkeys', 'TMenuAutoFlag', iptrw);
    RegisterProperty('AutoLineReduction', 'TMenuAutoFlag', iptrw);
    RegisterProperty('BiDiMode', 'TBiDiMode', iptrw);
    RegisterProperty('Handle', 'HMENU', iptr);
    RegisterProperty('OwnerDraw', 'Boolean', iptrw);
    RegisterProperty('ParentBiDiMode', 'Boolean', iptrw);
    RegisterProperty('WindowHandle', 'HWND', iptrw);
    RegisterProperty('Items', 'TMenuItem', iptr);
  end;
end;

procedure SIRegisterTMENUITEM(CL: TPSPascalCompiler);
begin
  With cl.AddClassN(Cl.FindClass('TComponent'),'TMenuItem') do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent)');
    RegisterMethod('procedure InitiateAction');
    RegisterMethod('procedure Insert(Index: Integer; Item: TMenuItem)');
    RegisterMethod('procedure Delete(Index: Integer)');
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure Click');
    RegisterMethod('function Find(ACaption: string): TMenuItem');
    RegisterMethod('function IndexOf(Item: TMenuItem): Integer');
    RegisterMethod('function IsLine: Boolean');
    RegisterMethod('function GetImageList: TCustomImageList');
    RegisterMethod('function GetParentComponent: TComponent');
    RegisterMethod('function GetParentMenu: TMenu');
    RegisterMethod('function HasParent: Boolean');
    RegisterMethod('function NewTopLine: Integer');
    RegisterMethod('function NewBottomLine: Integer');
    RegisterMethod('function InsertNewLineBefore(AItem: TMenuItem): Integer');
    RegisterMethod('function InsertNewLineAfter(AItem: TMenuItem): Integer');
    RegisterMethod('procedure Add(Item: TMenuItem)');
    RegisterMethod('procedure Remove(Item: TMenuItem)');
    RegisterMethod('function RethinkHotkeys: Boolean');
    RegisterMethod('function RethinkLines: Boolean');
    RegisterProperty('Command', 'Word', iptr);
    RegisterProperty('Handle', 'HMENU', iptr);
    RegisterProperty('Count', 'Integer', iptr);
    RegisterProperty('Items', 'TMenuItem Integer', iptr);
    RegisterProperty('MenuIndex', 'Integer', iptrw);
    RegisterProperty('Parent', 'TMenuItem', iptr);
    {$IFDEF DELPHI5UP}
    RegisterProperty('Action', 'TBasicAction', iptrw);
    {$ENDIF}
    RegisterProperty('AutoHotkeys', 'TMenuItemAutoFlag', iptrw);
    RegisterProperty('AutoLineReduction', 'TMenuItemAutoFlag', iptrw);
    RegisterProperty('Bitmap', 'TBitmap', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Checked', 'Boolean', iptrw);
    RegisterProperty('SubMenuImages', 'TCustomImageList', iptrw);
    RegisterProperty('Default', 'Boolean', iptrw);
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('GroupIndex', 'Byte', iptrw);
    RegisterProperty('HelpContext', 'THelpContext', iptrw);
    RegisterProperty('Hint', 'string', iptrw);
    RegisterProperty('ImageIndex', 'TImageIndex', iptrw);
    RegisterProperty('RadioItem', 'Boolean', iptrw);
    RegisterProperty('ShortCut', 'TShortCut', iptrw);
    RegisterProperty('Visible', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
   {$IFNDEF FPC} RegisterProperty('OnDrawItem', 'TMenuDrawItemEvent', iptrw);
    RegisterProperty('OnAdvancedDrawItem', 'TAdvancedMenuDrawItemEvent', iptrw);
    RegisterProperty('OnMeasureItem', 'TMenuMeasureItemEvent', iptrw);{$ENDIF}
  end;
end;

procedure SIRegister_Menus(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('HMENU', 'Cardinal');
  Cl.AddTypeS('HACCEL', 'Cardinal');

  cl.addClassN(cl.FindClass('Exception'),'EMenuError');
  Cl.addTypeS('TMenuBreak', '(mbNone, mbBreak, mbBarBreak)');
{$IFNDEF FPC}
  Cl.addTypeS('TMenuDrawItemEvent', 'procedure (Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean)');

  Cl.addTypeS('TAdvancedMenuDrawItemEvent', 'procedure (Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState)');

  Cl.addTypeS('TMenuMeasureItemEvent', 'procedure (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer)');

{$ENDIF}
  Cl.addTypeS('TMenuItemAutoFlag', '(maAutomatic, maManual, maParent)');
  Cl.AddTypeS('TMenuAutoFlag', 'TMenuItemAutoFlag');
  Cl.addTypeS('TShortCut', 'Word');
  cl.addClassN(cl.FindClass('TActionLink'),'TMenuActionLink');
  SIRegisterTMENUITEM(Cl);
  Cl.addTypeS('TMenuChangeEvent', 'procedure (Sender: TObject; Source: TMenuItem; Rebuild: Boolean)');

  Cl.addTypeS('TFindItemKind', '(fkCommand, fkHandle, fkShortCut)');
  SIRegisterTMENU(Cl);
  SIRegisterTMAINMENU(Cl);
  Cl.addTypeS('TPopupAlignment', '(paLeft, paRight, paCenter)');
  Cl.addTypeS('TTrackButton', '(tbRightButton, tbLeftButton)');
  Cl.addTypeS('TMenuAnimations', '(maLeftToRight, maRightToLeft, maTopToBottom, maBottomToTop, maNone)');

  Cl.addTypeS('TMenuAnimation', 'set of TMenuAnimations');
  SIRegisterTPOPUPMENU(Cl);
  SIRegisterTPOPUPLIST(Cl);
  SIRegisterTMENUITEMSTACK(Cl);
  Cl.addTypeS('TCMenuItem', 'TMenuItem');
{$IFNDEF FPC}
//TODO: it should work,but somehow TShiftState is not defined
  Cl.AddDelphiFunction('function ShortCut(Key: Word; Shift: TShiftState): TShortCut');

  Cl.AddDelphiFunction('procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState)');

{$ENDIF}
  Cl.AddDelphiFunction('function ShortCutToText(ShortCut: TShortCut): string');

  Cl.AddDelphiFunction('function TextToShortCut(Text: string): TShortCut');
  Cl.AddDelphiFunction('function NewMenu(Owner: TComponent; const AName: string; Items: array of TMenuItem): TMainMenu');

  Cl.AddDelphiFunction('function NewPopupMenu(Owner: TComponent; const AName: string; Alignment: TPopupAlignment; AutoPopup: Boolean; const Items: array of TCMenuItem): TPopupMenu');


  Cl.AddDelphiFunction('function NewSubMenu(const ACaption: string; HCTX: Word; const AName: string; Items: array of TMenuItem; AEnabled: Boolean): TMenuItem');

  Cl.AddDelphiFunction('function NewItem(const ACaption: string; AShortCut: TShortCut; Achecked, AEnabled: Boolean; AOnClick: TNotifyEvent; HCTX: Word; const AName: string): TMenuItem');


  Cl.AddDelphiFunction('function NewLine: TMenuItem');
{$IFNDEF FPC}
  Cl.AddDelphiFunction('procedure DrawMenuItem(MenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState)');

{$ENDIF}
end;

end.

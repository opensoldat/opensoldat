 
Unit uPSR_menus;
{$I PascalScript.inc}
Interface
Uses uPSRuntime;

procedure RIRegister_Menus_Routines(S: TPSExec);
{$IFNDEF FPC}
procedure RIRegisterTMENUITEMSTACK(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPOPUPLIST(Cl: TPSRuntimeClassImporter);
{$ENDIF}
procedure RIRegisterTPOPUPMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMAINMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMENUITEM(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Menus(CL: TPSRuntimeClassImporter);

implementation
{$IFDEF LINUX}
{$IFNDEF FPC}
Uses
  Libc, SysUtils, Classes, QControls, QMenus, QGraphics;
{$ELSE}
Uses
  SysUtils, Classes, Controls, Menus, Graphics, LCLType, ImgList;
{$ENDIF}
{$ELSE}
Uses {$IFNDEF FPC}WINDOWS,{$ELSE} LCLType,{$ENDIF} SYSUTILS, CLASSES, CONTNRS, MESSAGES, GRAPHICS, IMGLIST, ACTNLIST, Menus;
{$ENDIF}


{$IFNDEF FPC}
procedure TPOPUPLISTWINDOW_R(Self: TPOPUPLIST; var T: HWND);
begin T := Self.WINDOW; end;
{$ENDIF}

procedure TPOPUPMENUONPOPUP_W(Self: TPOPUPMENU; const T: TNOTIFYEVENT);
begin Self.ONPOPUP := T; end;

procedure TPOPUPMENUONPOPUP_R(Self: TPOPUPMENU; var T: TNOTIFYEVENT);
begin T := Self.ONPOPUP; end;

{$IFNDEF FPC}
procedure TPOPUPMENUTRACKBUTTON_W(Self: TPOPUPMENU; const T: TTRACKBUTTON);
begin Self.TRACKBUTTON := T; end;

procedure TPOPUPMENUTRACKBUTTON_R(Self: TPOPUPMENU; var T: TTRACKBUTTON);
begin T := Self.TRACKBUTTON; end;


procedure TPOPUPMENUMENUANIMATION_W(Self: TPOPUPMENU; const T: TMENUANIMATION);
begin Self.MENUANIMATION := T; end;

procedure TPOPUPMENUMENUANIMATION_R(Self: TPOPUPMENU; var T: TMENUANIMATION);
begin T := Self.MENUANIMATION; end;

procedure TPOPUPMENUHELPCONTEXT_W(Self: TPOPUPMENU; const T: THELPCONTEXT);
begin Self.HELPCONTEXT := T; end;

procedure TPOPUPMENUHELPCONTEXT_R(Self: TPOPUPMENU; var T: THELPCONTEXT);
begin T := Self.HELPCONTEXT; end;
{$ENDIF}

procedure TPOPUPMENUAUTOPOPUP_W(Self: TPOPUPMENU; const T: BOOLEAN);
begin Self.AUTOPOPUP := T; end;

procedure TPOPUPMENUAUTOPOPUP_R(Self: TPOPUPMENU; var T: BOOLEAN);
begin T := Self.AUTOPOPUP; end;

{$IFNDEF FPC}
procedure TPOPUPMENUALIGNMENT_W(Self: TPOPUPMENU; const T: TPOPUPALIGNMENT);
begin Self.ALIGNMENT := T; end;

procedure TPOPUPMENUALIGNMENT_R(Self: TPOPUPMENU; var T: TPOPUPALIGNMENT);
begin T := Self.ALIGNMENT; end;
{$ENDIF}

procedure TPOPUPMENUPOPUPCOMPONENT_W(Self: TPOPUPMENU; const T: TCOMPONENT);
begin Self.POPUPCOMPONENT := T; end;

procedure TPOPUPMENUPOPUPCOMPONENT_R(Self: TPOPUPMENU; var T: TCOMPONENT);
begin T := Self.POPUPCOMPONENT; end;

{$IFNDEF FPC}
procedure TMAINMENUAUTOMERGE_W(Self: TMAINMENU; const T: BOOLEAN);
begin Self.AUTOMERGE := T; end;

procedure TMAINMENUAUTOMERGE_R(Self: TMAINMENU; var T: BOOLEAN);
begin T := Self.AUTOMERGE; end;
{$ENDIF}

procedure TMENUITEMS_R(Self: TMENU; var T: TMENUITEM);
begin T := Self.ITEMS; end;


{$IFNDEF FPC}
procedure TMENUWINDOWHANDLE_W(Self: TMENU; const T: HWND);
begin Self.WINDOWHANDLE := T; end;

procedure TMENUWINDOWHANDLE_R(Self: TMENU; var T: HWND);
begin T := Self.WINDOWHANDLE; end;

procedure TMENUPARENTBIDIMODE_W(Self: TMENU; const T: BOOLEAN);
begin Self.PARENTBIDIMODE := T; end;

procedure TMENUPARENTBIDIMODE_R(Self: TMENU; var T: BOOLEAN);
begin T := Self.PARENTBIDIMODE; end;

procedure TMENUOWNERDRAW_W(Self: TMENU; const T: BOOLEAN);
begin Self.OWNERDRAW := T; end;

procedure TMENUOWNERDRAW_R(Self: TMENU; var T: BOOLEAN);
begin T := Self.OWNERDRAW; end;

procedure TMENUBIDIMODE_W(Self: TMENU; const T: TBIDIMODE);
begin Self.BIDIMODE := T; end;

procedure TMENUBIDIMODE_R(Self: TMENU; var T: TBIDIMODE);
begin T := Self.BIDIMODE; end;

procedure TMENUAUTOLINEREDUCTION_W(Self: TMENU; const T: TMENUAUTOFLAG);
begin Self.AUTOLINEREDUCTION := T; end;

procedure TMENUAUTOLINEREDUCTION_R(Self: TMENU; var T: TMENUAUTOFLAG);
begin T := Self.AUTOLINEREDUCTION; end;

procedure TMENUAUTOHOTKEYS_W(Self: TMENU; const T: TMENUAUTOFLAG);
begin Self.AUTOHOTKEYS := T; end;

procedure TMENUAUTOHOTKEYS_R(Self: TMENU; var T: TMENUAUTOFLAG);
begin T := Self.AUTOHOTKEYS; end;

{$ENDIF}


procedure TMENUHANDLE_R(Self: TMENU; var T: HMENU);
begin T := Self.HANDLE; end;




procedure TMENUIMAGES_W(Self: TMENU; const T: TCUSTOMIMAGELIST);
begin Self.IMAGES := T; end;

procedure TMENUIMAGES_R(Self: TMENU; var T: TCUSTOMIMAGELIST);
begin T := Self.IMAGES; end;

{$IFNDEF FPC}
procedure TMENUITEMONMEASUREITEM_W(Self: TMENUITEM; const T: TMENUMEASUREITEMEVENT);
begin Self.ONMEASUREITEM := T; end;

procedure TMENUITEMONMEASUREITEM_R(Self: TMENUITEM; var T: TMENUMEASUREITEMEVENT);
begin T := Self.ONMEASUREITEM; end;

procedure TMENUITEMONADVANCEDDRAWITEM_W(Self: TMENUITEM; const T: TADVANCEDMENUDRAWITEMEVENT);
begin Self.ONADVANCEDDRAWITEM := T; end;

procedure TMENUITEMONADVANCEDDRAWITEM_R(Self: TMENUITEM; var T: TADVANCEDMENUDRAWITEMEVENT);
begin T := Self.ONADVANCEDDRAWITEM; end;

procedure TMENUITEMONDRAWITEM_W(Self: TMENUITEM; const T: TMENUDRAWITEMEVENT);
begin Self.ONDRAWITEM := T; end;

procedure TMENUITEMONDRAWITEM_R(Self: TMENUITEM; var T: TMENUDRAWITEMEVENT);
begin T := Self.ONDRAWITEM; end;
{$ENDIF}

procedure TMENUITEMONCLICK_W(Self: TMENUITEM; const T: TNOTIFYEVENT);
begin Self.ONCLICK := T; end;

procedure TMENUITEMONCLICK_R(Self: TMENUITEM; var T: TNOTIFYEVENT);
begin T := Self.ONCLICK; end;

procedure TMENUITEMVISIBLE_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.VISIBLE := T; end;

procedure TMENUITEMVISIBLE_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.VISIBLE; end;

procedure TMENUITEMSHORTCUT_W(Self: TMENUITEM; const T: TSHORTCUT);
begin Self.SHORTCUT := T; end;

procedure TMENUITEMSHORTCUT_R(Self: TMENUITEM; var T: TSHORTCUT);
begin T := Self.SHORTCUT; end;

procedure TMENUITEMRADIOITEM_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.RADIOITEM := T; end;

procedure TMENUITEMRADIOITEM_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.RADIOITEM; end;

procedure TMENUITEMIMAGEINDEX_W(Self: TMENUITEM; const T: TIMAGEINDEX);
begin Self.IMAGEINDEX := T; end;

procedure TMENUITEMIMAGEINDEX_R(Self: TMENUITEM; var T: TIMAGEINDEX);
begin T := Self.IMAGEINDEX; end;

procedure TMENUITEMHINT_W(Self: TMENUITEM; const T: STRING);
begin Self.HINT := T; end;

procedure TMENUITEMHINT_R(Self: TMENUITEM; var T: STRING);
begin T := Self.HINT; end;

procedure TMENUITEMHELPCONTEXT_W(Self: TMENUITEM; const T: THELPCONTEXT);
begin Self.HELPCONTEXT := T; end;

procedure TMENUITEMHELPCONTEXT_R(Self: TMENUITEM; var T: THELPCONTEXT);
begin T := Self.HELPCONTEXT; end;

procedure TMENUITEMGROUPINDEX_W(Self: TMENUITEM; const T: BYTE);
begin Self.GROUPINDEX := T; end;

procedure TMENUITEMGROUPINDEX_R(Self: TMENUITEM; var T: BYTE);
begin T := Self.GROUPINDEX; end;

procedure TMENUITEMENABLED_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.ENABLED := T; end;

procedure TMENUITEMENABLED_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.ENABLED; end;

procedure TMENUITEMDEFAULT_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.DEFAULT := T; end;

procedure TMENUITEMDEFAULT_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.DEFAULT; end;

procedure TMENUITEMSUBMENUIMAGES_W(Self: TMENUITEM; const T: TCUSTOMIMAGELIST);
begin Self.SUBMENUIMAGES := T; end;

procedure TMENUITEMSUBMENUIMAGES_R(Self: TMENUITEM; var T: TCUSTOMIMAGELIST);
begin T := Self.SUBMENUIMAGES; end;

procedure TMENUITEMCHECKED_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.CHECKED := T; end;

procedure TMENUITEMCHECKED_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.CHECKED; end;

procedure TMENUITEMCAPTION_W(Self: TMENUITEM; const T: STRING);
begin Self.CAPTION := T; end;

procedure TMENUITEMCAPTION_R(Self: TMENUITEM; var T: STRING);
begin T := Self.CAPTION; end;

procedure TMENUITEMBITMAP_W(Self: TMENUITEM; const T: TBITMAP);
begin Self.BITMAP := T; end;

procedure TMENUITEMBITMAP_R(Self: TMENUITEM; var T: TBITMAP);
begin T := Self.BITMAP; end;

{$IFNDEF FPC}
procedure TMENUITEMAUTOLINEREDUCTION_W(Self: TMENUITEM; const T: TMENUITEMAUTOFLAG);
begin Self.AUTOLINEREDUCTION := T; end;

procedure TMENUITEMAUTOLINEREDUCTION_R(Self: TMENUITEM; var T: TMENUITEMAUTOFLAG);
begin T := Self.AUTOLINEREDUCTION; end;

procedure TMENUITEMAUTOHOTKEYS_W(Self: TMENUITEM; const T: TMENUITEMAUTOFLAG);
begin Self.AUTOHOTKEYS := T; end;

procedure TMENUITEMAUTOHOTKEYS_R(Self: TMENUITEM; var T: TMENUITEMAUTOFLAG);
begin T := Self.AUTOHOTKEYS; end;
{$ENDIF}

procedure TMENUITEMACTION_W(Self: TMENUITEM; const T: TBASICACTION);
begin Self.ACTION := T; end;

procedure TMENUITEMACTION_R(Self: TMENUITEM; var T: TBASICACTION);
begin T := Self.ACTION; end;

procedure TMENUITEMPARENT_R(Self: TMENUITEM; var T: TMENUITEM);
begin T := Self.PARENT; end;

procedure TMENUITEMMENUINDEX_W(Self: TMENUITEM; const T: INTEGER);
begin Self.MENUINDEX := T; end;

procedure TMENUITEMMENUINDEX_R(Self: TMENUITEM; var T: INTEGER);
begin T := Self.MENUINDEX; end;

procedure TMENUITEMITEMS_R(Self: TMENUITEM; var T: TMENUITEM; const t1: INTEGER);
begin T := Self.ITEMS[t1]; end;

procedure TMENUITEMCOUNT_R(Self: TMENUITEM; var T: INTEGER);
begin T := Self.COUNT; end;

procedure TMENUITEMHANDLE_R(Self: TMENUITEM; var T: HMENU);
begin T := Self.HANDLE; end;

procedure TMENUITEMCOMMAND_R(Self: TMENUITEM; var T: WORD);
begin T := Self.COMMAND; end;

procedure RIRegister_Menus_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@SHORTCUT, 'ShortCut', cdRegister);
	S.RegisterDelphiFunction(@SHORTCUTTOKEY, 'ShortCutToKey', cdRegister);
{$IFNDEF FPC}
  S.RegisterDelphiFunction(@SHORTCUTTOTEXT, 'ShortCutToText', cdRegister);
  S.RegisterDelphiFunction(@TEXTTOSHORTCUT, 'TextToShortCut', cdRegister);
  S.RegisterDelphiFunction(@NEWMENU, 'NewMenu', cdRegister);
  S.RegisterDelphiFunction(@NEWPOPUPMENU, 'NewPopupMenu', cdRegister);
  S.RegisterDelphiFunction(@NEWSUBMENU, 'NewSubMenu', cdRegister);
  S.RegisterDelphiFunction(@NEWITEM, 'NewItem', cdRegister);
  S.RegisterDelphiFunction(@NEWLINE, 'NewLine', cdRegister);
	S.RegisterDelphiFunction(@DRAWMENUITEM, 'DrawMenuItem', cdRegister);
{$ENDIF}	
end;

{$IFNDEF FPC}
procedure RIRegisterTMENUITEMSTACK(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TMENUITEMSTACK) do
	begin
		RegisterMethod(@TMENUITEMSTACK.CLEARITEM, 'ClearItem');
	end;
end;

procedure RIRegisterTPOPUPLIST(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TPOPUPLIST) do
	begin
		RegisterPropertyHelper(@TPOPUPLISTWINDOW_R,nil,'Window');
		RegisterMethod(@TPOPUPLIST.ADD, 'Add');
		RegisterMethod(@TPOPUPLIST.REMOVE, 'Remove');
	end;
end;
{$ENDIF}


procedure RIRegisterTPOPUPMENU(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TPOPUPMENU) do
  begin
		RegisterConstructor(@TPOPUPMENU.CREATE, 'Create');
		RegisterVirtualMethod(@TPOPUPMENU.POPUP, 'Popup');
		RegisterPropertyHelper(@TPOPUPMENUPOPUPCOMPONENT_R,@TPOPUPMENUPOPUPCOMPONENT_W,'PopupComponent');
		RegisterEventPropertyHelper(@TPOPUPMENUONPOPUP_R,@TPOPUPMENUONPOPUP_W,'OnPopup');
{$IFNDEF FPC}
		RegisterPropertyHelper(@TPOPUPMENUALIGNMENT_R,@TPOPUPMENUALIGNMENT_W,'Alignment');
		RegisterPropertyHelper(@TPOPUPMENUAUTOPOPUP_R,@TPOPUPMENUAUTOPOPUP_W,'AutoPopup');
		RegisterPropertyHelper(@TPOPUPMENUHELPCONTEXT_R,@TPOPUPMENUHELPCONTEXT_W,'HelpContext');
		RegisterPropertyHelper(@TPOPUPMENUMENUANIMATION_R,@TPOPUPMENUMENUANIMATION_W,'MenuAnimation');
		RegisterPropertyHelper(@TPOPUPMENUTRACKBUTTON_R,@TPOPUPMENUTRACKBUTTON_W,'TrackButton');
{$ENDIF}
	end;
end;

procedure RIRegisterTMAINMENU(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TMAINMENU) do
	begin
{$IFNDEF FPC}
		RegisterMethod(@TMAINMENU.MERGE, 'Merge');
		RegisterMethod(@TMAINMENU.UNMERGE, 'Unmerge');
		RegisterMethod(@TMAINMENU.POPULATEOLE2MENU, 'PopulateOle2Menu');
		RegisterMethod(@TMAINMENU.GETOLE2ACCELERATORTABLE, 'GetOle2AcceleratorTable');
		RegisterMethod(@TMAINMENU.SETOLE2MENUHANDLE, 'SetOle2MenuHandle');
		RegisterPropertyHelper(@TMAINMENUAUTOMERGE_R,@TMAINMENUAUTOMERGE_W,'AutoMerge');
{$ENDIF}		
	end;
end;


procedure RIRegisterTMENU(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TMENU) do
	begin
		RegisterConstructor(@TMENU.CREATE, 'Create');
		RegisterMethod(@TMENU.DISPATCHCOMMAND, 'DispatchCommand');
		RegisterMethod(@TMENU.FINDITEM, 'FindItem');
		RegisterPropertyHelper(@TMENUIMAGES_R,@TMENUIMAGES_W,'Images');
		RegisterMethod(@TMENU.ISRIGHTTOLEFT, 'IsRightToLeft');
		RegisterPropertyHelper(@TMENUHANDLE_R,nil,'Handle');
		RegisterPropertyHelper(@TMENUITEMS_R,nil,'Items');
{$IFNDEF FPC}
		RegisterMethod(@TMENU.DISPATCHPOPUP, 'DispatchPopup');
		RegisterMethod(@TMENU.PARENTBIDIMODECHANGED, 'ParentBiDiModeChanged');
		RegisterMethod(@TMENU.PROCESSMENUCHAR, 'ProcessMenuChar');
		RegisterPropertyHelper(@TMENUAUTOHOTKEYS_R,@TMENUAUTOHOTKEYS_W,'AutoHotkeys');
		RegisterPropertyHelper(@TMENUAUTOLINEREDUCTION_R,@TMENUAUTOLINEREDUCTION_W,'AutoLineReduction');
		RegisterPropertyHelper(@TMENUBIDIMODE_R,@TMENUBIDIMODE_W,'BiDiMode');
		RegisterMethod(@TMENU.GETHELPCONTEXT, 'GetHelpContext');
		RegisterPropertyHelper(@TMENUOWNERDRAW_R,@TMENUOWNERDRAW_W,'OwnerDraw');
		RegisterPropertyHelper(@TMENUPARENTBIDIMODE_R,@TMENUPARENTBIDIMODE_W,'ParentBiDiMode');
		RegisterPropertyHelper(@TMENUWINDOWHANDLE_R,@TMENUWINDOWHANDLE_W,'WindowHandle');
{$ENDIF}
	end;
end;

procedure RIRegisterTMENUITEM(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TMENUITEM) do
	begin
		RegisterConstructor(@TMENUITEM.CREATE, 'Create');
		RegisterVirtualMethod(@TMENUITEM.INITIATEACTION, 'InitiateAction');
		RegisterMethod(@TMENUITEM.INSERT, 'Insert');
		RegisterMethod(@TMENUITEM.DELETE, 'Delete');
		RegisterMethod(@TMENUITEM.CLEAR, 'Clear');
		RegisterVirtualMethod(@TMENUITEM.CLICK, 'Click');
{$IFNDEF FPC}
		RegisterMethod(@TMENUITEM.FIND, 'Find');
		RegisterMethod(@TMENUITEM.NEWTOPLINE, 'NewTopLine');
		RegisterMethod(@TMENUITEM.NEWBOTTOMLINE, 'NewBottomLine');
		RegisterMethod(@TMENUITEM.INSERTNEWLINEBEFORE, 'InsertNewLineBefore');
		RegisterMethod(@TMENUITEM.INSERTNEWLINEAFTER, 'InsertNewLineAfter');
		RegisterMethod(@TMENUITEM.RETHINKHOTKEYS, 'RethinkHotkeys');
		RegisterMethod(@TMENUITEM.RETHINKLINES, 'RethinkLines');
		RegisterMethod(@TMENUITEM.ISLINE, 'IsLine');
{$ENDIF}
		RegisterMethod(@TMENUITEM.INDEXOF, 'IndexOf');
		RegisterMethod(@TMENUITEM.GETIMAGELIST, 'GetImageList');
		RegisterMethod(@TMENUITEM.GETPARENTCOMPONENT, 'GetParentComponent');
		RegisterMethod(@TMENUITEM.GETPARENTMENU, 'GetParentMenu');
		RegisterMethod(@TMENUITEM.HASPARENT, 'HasParent');
		RegisterMethod(@TMENUITEM.ADD, 'Add');
		RegisterMethod(@TMENUITEM.REMOVE, 'Remove');
{$IFNDEF FPC}
		RegisterPropertyHelper(@TMENUITEMAUTOHOTKEYS_R,@TMENUITEMAUTOHOTKEYS_W,'AutoHotkeys');
		RegisterPropertyHelper(@TMENUITEMAUTOLINEREDUCTION_R,@TMENUITEMAUTOLINEREDUCTION_W,'AutoLineReduction');
		RegisterEventPropertyHelper(@TMENUITEMONDRAWITEM_R,@TMENUITEMONDRAWITEM_W,'OnDrawItem');
		RegisterEventPropertyHelper(@TMENUITEMONADVANCEDDRAWITEM_R,@TMENUITEMONADVANCEDDRAWITEM_W,'OnAdvancedDrawItem');
		RegisterEventPropertyHelper(@TMENUITEMONMEASUREITEM_R,@TMENUITEMONMEASUREITEM_W,'OnMeasureItem');
{$ENDIF}
		RegisterPropertyHelper(@TMENUITEMCOMMAND_R,nil,'Command');
		RegisterPropertyHelper(@TMENUITEMHANDLE_R,nil,'Handle');
		RegisterPropertyHelper(@TMENUITEMCOUNT_R,nil,'Count');
		RegisterPropertyHelper(@TMENUITEMITEMS_R,nil,'Items');
		RegisterPropertyHelper(@TMENUITEMMENUINDEX_R,@TMENUITEMMENUINDEX_W,'MenuIndex');
		RegisterPropertyHelper(@TMENUITEMPARENT_R,nil,'Parent');
		RegisterPropertyHelper(@TMENUITEMACTION_R,@TMENUITEMACTION_W,'Action');
		RegisterPropertyHelper(@TMENUITEMBITMAP_R,@TMENUITEMBITMAP_W,'Bitmap');
		RegisterPropertyHelper(@TMENUITEMCAPTION_R,@TMENUITEMCAPTION_W,'Caption');
		RegisterPropertyHelper(@TMENUITEMCHECKED_R,@TMENUITEMCHECKED_W,'Checked');
		RegisterPropertyHelper(@TMENUITEMSUBMENUIMAGES_R,@TMENUITEMSUBMENUIMAGES_W,'SubMenuImages');
		RegisterPropertyHelper(@TMENUITEMDEFAULT_R,@TMENUITEMDEFAULT_W,'Default');
		RegisterPropertyHelper(@TMENUITEMENABLED_R,@TMENUITEMENABLED_W,'Enabled');
		RegisterPropertyHelper(@TMENUITEMGROUPINDEX_R,@TMENUITEMGROUPINDEX_W,'GroupIndex');
		RegisterPropertyHelper(@TMENUITEMHELPCONTEXT_R,@TMENUITEMHELPCONTEXT_W,'HelpContext');
		RegisterPropertyHelper(@TMENUITEMHINT_R,@TMENUITEMHINT_W,'Hint');
		RegisterPropertyHelper(@TMENUITEMIMAGEINDEX_R,@TMENUITEMIMAGEINDEX_W,'ImageIndex');
		RegisterPropertyHelper(@TMENUITEMRADIOITEM_R,@TMENUITEMRADIOITEM_W,'RadioItem');
		RegisterPropertyHelper(@TMENUITEMSHORTCUT_R,@TMENUITEMSHORTCUT_W,'ShortCut');
		RegisterPropertyHelper(@TMENUITEMVISIBLE_R,@TMENUITEMVISIBLE_W,'Visible');
		RegisterEventPropertyHelper(@TMENUITEMONCLICK_R,@TMENUITEMONCLICK_W,'OnClick');
	end;
end;

procedure RIRegister_Menus(CL: TPSRuntimeClassImporter);
begin
	RIRegisterTMENUITEM(Cl);
	RIRegisterTMENU(Cl);
	RIRegisterTPOPUPMENU(Cl);
	RIRegisterTMAINMENU(Cl);
	{$IFNDEF FPC}
	RIRegisterTPOPUPLIST(Cl);
	RIRegisterTMENUITEMSTACK(Cl);
	{$ENDIF}
end;

end.

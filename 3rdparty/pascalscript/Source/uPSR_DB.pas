{runtime DB support}
Unit uPSR_DB;
{$I PascalScript.inc}
Interface
Uses uPSRuntime, uPSUtils, SysUtils;

procedure RIRegisterTDATASET(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPARAMS(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPARAM(Cl: TPSRuntimeClassImporter);

{$IFNDEF FPC}
procedure RIRegisterTGUIDFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTVARIANTFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTREFERENCEFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTDATASETFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTARRAYFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTADTFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTOBJECTFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTWIDESTRINGFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFIELDLIST(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFIELDDEFLIST(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFLATLIST(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTDEFCOLLECTION(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTNAMEDITEM(Cl: TPSRuntimeClassImporter);

{$IFDEF DELPHI6UP}
procedure RIRegisterTFMTBCDFIELD(Cl: TPSRuntimeClassImporter);
{$ENDIF}
procedure RIRegisterTBCDFIELD(Cl: TPSRuntimeClassImporter);

{$ENDIF}

procedure RIRegisterTGRAPHICFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMEMOFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBLOBFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTVARBYTESFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBYTESFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBINARYFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTTIMEFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTDATEFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTDATETIMEFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBOOLEANFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCURRENCYFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFLOATFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTAUTOINCFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTWORDFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTLARGEINTFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTSMALLINTFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTINTEGERFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTNUMERICFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTSTRINGFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFIELD(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTLOOKUPLIST(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFIELDS(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTINDEXDEFS(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTINDEXDEF(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFIELDDEFS(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFIELDDEF(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DB(CL: TPSRuntimeClassImporter);

implementation
Uses DB, {$IFDEF DELPHI6UP}{$IFNDEF FPC}FMTBcd, MaskUtils,{$ENDIF}{$ENDIF}Classes;

procedure TDATASETONPOSTERROR_W(Self: TDATASET; const T: TDATASETERROREVENT);
begin Self.ONPOSTERROR := T; end;

procedure TDATASETONPOSTERROR_R(Self: TDATASET; var T: TDATASETERROREVENT);
begin T := Self.ONPOSTERROR; end;

procedure TDATASETONNEWRECORD_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.ONNEWRECORD := T; end;

procedure TDATASETONNEWRECORD_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.ONNEWRECORD; end;

procedure TDATASETONFILTERRECORD_W(Self: TDATASET; const T: TFILTERRECORDEVENT);
begin Self.ONFILTERRECORD := T; end;

procedure TDATASETONFILTERRECORD_R(Self: TDATASET; var T: TFILTERRECORDEVENT);
begin T := Self.ONFILTERRECORD; end;

procedure TDATASETONEDITERROR_W(Self: TDATASET; const T: TDATASETERROREVENT);
begin Self.ONEDITERROR := T; end;

procedure TDATASETONEDITERROR_R(Self: TDATASET; var T: TDATASETERROREVENT);
begin T := Self.ONEDITERROR; end;

procedure TDATASETONDELETEERROR_W(Self: TDATASET; const T: TDATASETERROREVENT);
begin Self.ONDELETEERROR := T; end;

procedure TDATASETONDELETEERROR_R(Self: TDATASET; var T: TDATASETERROREVENT);
begin T := Self.ONDELETEERROR; end;

procedure TDATASETONCALCFIELDS_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.ONCALCFIELDS := T; end;

procedure TDATASETONCALCFIELDS_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.ONCALCFIELDS; end;

{$IFNDEF FPC}
procedure TDATASETAFTERREFRESH_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTERREFRESH := T; end;

procedure TDATASETAFTERREFRESH_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTERREFRESH; end;

procedure TDATASETBEFOREREFRESH_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFOREREFRESH := T; end;

procedure TDATASETBEFOREREFRESH_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFOREREFRESH; end;

{$ENDIF}

procedure TDATASETAFTERSCROLL_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTERSCROLL := T; end;

procedure TDATASETAFTERSCROLL_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTERSCROLL; end;

procedure TDATASETBEFORESCROLL_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFORESCROLL := T; end;

procedure TDATASETBEFORESCROLL_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFORESCROLL; end;

procedure TDATASETAFTERDELETE_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTERDELETE := T; end;

procedure TDATASETAFTERDELETE_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTERDELETE; end;

procedure TDATASETBEFOREDELETE_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFOREDELETE := T; end;

procedure TDATASETBEFOREDELETE_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFOREDELETE; end;

procedure TDATASETAFTERCANCEL_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTERCANCEL := T; end;

procedure TDATASETAFTERCANCEL_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTERCANCEL; end;

procedure TDATASETBEFORECANCEL_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFORECANCEL := T; end;

procedure TDATASETBEFORECANCEL_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFORECANCEL; end;

procedure TDATASETAFTERPOST_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTERPOST := T; end;

procedure TDATASETAFTERPOST_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTERPOST; end;

procedure TDATASETBEFOREPOST_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFOREPOST := T; end;

procedure TDATASETBEFOREPOST_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFOREPOST; end;

procedure TDATASETAFTEREDIT_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTEREDIT := T; end;

procedure TDATASETAFTEREDIT_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTEREDIT; end;

procedure TDATASETBEFOREEDIT_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFOREEDIT := T; end;

procedure TDATASETBEFOREEDIT_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFOREEDIT; end;

procedure TDATASETAFTERINSERT_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTERINSERT := T; end;

procedure TDATASETAFTERINSERT_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTERINSERT; end;

procedure TDATASETBEFOREINSERT_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFOREINSERT := T; end;

procedure TDATASETBEFOREINSERT_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFOREINSERT; end;

procedure TDATASETAFTERCLOSE_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTERCLOSE := T; end;

procedure TDATASETAFTERCLOSE_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTERCLOSE; end;

procedure TDATASETBEFORECLOSE_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFORECLOSE := T; end;

procedure TDATASETBEFORECLOSE_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFORECLOSE; end;

procedure TDATASETAFTEROPEN_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.AFTEROPEN := T; end;

procedure TDATASETAFTEROPEN_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.AFTEROPEN; end;

procedure TDATASETBEFOREOPEN_W(Self: TDATASET; const T: TDATASETNOTIFYEVENT);
begin Self.BEFOREOPEN := T; end;

procedure TDATASETBEFOREOPEN_R(Self: TDATASET; var T: TDATASETNOTIFYEVENT);
begin T := Self.BEFOREOPEN; end;

procedure TDATASETAUTOCALCFIELDS_W(Self: TDATASET; const T: BOOLEAN);
begin Self.AUTOCALCFIELDS := T; end;

procedure TDATASETAUTOCALCFIELDS_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.AUTOCALCFIELDS; end;

procedure TDATASETACTIVE_W(Self: TDATASET; const T: BOOLEAN);
begin Self.ACTIVE := T; end;

procedure TDATASETACTIVE_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.ACTIVE; end;

procedure TDATASETFILTEROPTIONS_W(Self: TDATASET; const T: TFILTEROPTIONS);
begin Self.FILTEROPTIONS := T; end;

procedure TDATASETFILTEROPTIONS_R(Self: TDATASET; var T: TFILTEROPTIONS);
begin T := Self.FILTEROPTIONS; end;

procedure TDATASETFILTERED_W(Self: TDATASET; const T: BOOLEAN);
begin Self.FILTERED := T; end;

procedure TDATASETFILTERED_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.FILTERED; end;

procedure TDATASETFILTER_W(Self: TDATASET; const T: String);
begin Self.FILTER := T; end;

procedure TDATASETFILTER_R(Self: TDATASET; var T: String);
begin T := Self.FILTER; end;

procedure TDATASETSTATE_R(Self: TDATASET; var T: TDATASETSTATE);
begin T := Self.STATE; end;

{$IFNDEF FPC}
procedure TDATASETSPARSEARRAYS_W(Self: TDATASET; const T: BOOLEAN);
begin Self.SPARSEARRAYS := T; end;

procedure TDATASETSPARSEARRAYS_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.SPARSEARRAYS; end;
{$ENDIF}

procedure TDATASETRECORDSIZE_R(Self: TDATASET; var T: WORD);
begin T := Self.RECORDSIZE; end;

procedure TDATASETRECNO_W(Self: TDATASET; const T: INTEGER);
begin Self.RECNO := T; end;

procedure TDATASETRECNO_R(Self: TDATASET; var T: INTEGER);
begin T := Self.RECNO; end;

procedure TDATASETRECORDCOUNT_R(Self: TDATASET; var T: INTEGER);
begin T := Self.RECORDCOUNT; end;

{$IFNDEF FPC}
procedure TDATASETOBJECTVIEW_W(Self: TDATASET; const T: BOOLEAN);
begin Self.OBJECTVIEW := T; end;

procedure TDATASETOBJECTVIEW_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.OBJECTVIEW; end;
{$ENDIF}

procedure TDATASETMODIFIED_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.MODIFIED; end;

{$IFDEF DELPHI6UP}
procedure TDATASETISUNIDIRECTIONAL_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.ISUNIDIRECTIONAL; end;
{$ENDIF}

procedure TDATASETFOUND_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.FOUND; end;

procedure TDATASETFIELDVALUES_W(Self: TDATASET; const T: VARIANT; const t1: String);
begin Self.FIELDVALUES[t1] := T; end;

procedure TDATASETFIELDVALUES_R(Self: TDATASET; var T: VARIANT; const t1: String);
begin T := Self.FIELDVALUES[t1]; end;

procedure TDATASETFIELDS_R(Self: TDATASET; var T: TFIELDS);
begin T := Self.FIELDS; end;

{$IFNDEF FPC}

procedure TDATASETFIELDLIST_R(Self: TDATASET; var T: TFIELDLIST);
begin T := Self.FIELDLIST; end;


procedure TDATASETFIELDDEFLIST_R(Self: TDATASET; var T: TFIELDDEFLIST);
begin T := Self.FIELDDEFLIST; end;

procedure TDATASETFIELDDEFS_W(Self: TDATASET; const T: TFIELDDEFS);
begin Self.FIELDDEFS := T; end;

procedure TDATASETFIELDDEFS_R(Self: TDATASET; var T: TFIELDDEFS);
begin T := Self.FIELDDEFS; end;

procedure TDATASETBLOCKREADSIZE_W(Self: TDATASET; const T: INTEGER);
begin Self.BLOCKREADSIZE := T; end;

procedure TDATASETBLOCKREADSIZE_R(Self: TDATASET; var T: INTEGER);
begin T := Self.BLOCKREADSIZE; end;

procedure TDATASETDESIGNER_R(Self: TDATASET; var T: TDATASETDESIGNER);
begin T := Self.DESIGNER; end;


procedure TDATASETDATASETFIELD_W(Self: TDATASET; const T: TDATASETFIELD);
begin Self.DATASETFIELD := T; end;



procedure TDATASETDATASETFIELD_R(Self: TDATASET; var T: TDATASETFIELD);
begin T := Self.DATASETFIELD; end;


procedure TDATASETAGGFIELDS_R(Self: TDATASET; var T: TFIELDS);
begin T := Self.AGGFIELDS; end;



{$ENDIF}

procedure TDATASETFIELDCOUNT_R(Self: TDATASET; var T: INTEGER);
begin T := Self.FIELDCOUNT; end;


procedure TDATASETEOF_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.EOF; end;

procedure TDATASETDEFAULTFIELDS_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.DEFAULTFIELDS; end;

procedure TDATASETDATASOURCE_R(Self: TDATASET; var T: TDATASOURCE);
begin T := Self.DATASOURCE; end;



procedure TDATASETCANMODIFY_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.CANMODIFY; end;

//procedure TDATASETBOOKMARK_W(Self: TDATASET; const T: TBOOKMARKSTR);
//begin Self.BOOKMARK := T; end;

//procedure TDATASETBOOKMARK_R(Self: TDATASET; var T: TBOOKMARKSTR);
//begin T := Self.BOOKMARK; end;

procedure TDATASETBOF_R(Self: TDATASET; var T: BOOLEAN);
begin T := Self.BOF; end;

procedure TPARAMSPARAMVALUES_W(Self: TPARAMS; const T: VARIANT; const t1: String);
begin Self.PARAMVALUES[t1] := T; end;

procedure TPARAMSPARAMVALUES_R(Self: TPARAMS; var T: VARIANT; const t1: String);
begin T := Self.PARAMVALUES[t1]; end;

procedure TPARAMSITEMS_W(Self: TPARAMS; const T: TPARAM; const t1: INTEGER);
begin Self.ITEMS[t1] := T; end;

procedure TPARAMSITEMS_R(Self: TPARAMS; var T: TPARAM; const t1: INTEGER);
begin T := Self.ITEMS[t1]; end;

procedure TPARAMVALUE_W(Self: TPARAM; const T: VARIANT);
begin Self.VALUE := T; end;

procedure TPARAMVALUE_R(Self: TPARAM; var T: VARIANT);
begin T := Self.VALUE; end;


{$IFDEF DELPHI6UP}
procedure TPARAMSIZE_W(Self: TPARAM; const T: INTEGER);
begin Self.SIZE := T; end;

procedure TPARAMSIZE_R(Self: TPARAM; var T: INTEGER);
begin T := Self.SIZE; end;
{$ENDIF}

procedure TPARAMPARAMTYPE_W(Self: TPARAM; const T: TPARAMTYPE);
begin Self.PARAMTYPE := T; end;

procedure TPARAMPARAMTYPE_R(Self: TPARAM; var T: TPARAMTYPE);
begin T := Self.PARAMTYPE; end;

procedure TPARAMNAME_W(Self: TPARAM; const T: String);
begin Self.NAME := T; end;

procedure TPARAMNAME_R(Self: TPARAM; var T: String);
begin T := Self.NAME; end;

{$IFDEF DELPHI6UP}
procedure TPARAMNUMERICSCALE_W(Self: TPARAM; const T: INTEGER);
begin Self.NUMERICSCALE := T; end;

procedure TPARAMNUMERICSCALE_R(Self: TPARAM; var T: INTEGER);
begin T := Self.NUMERICSCALE; end;
{$ENDIF}
{$IFDEF DELPHI6UP}

procedure TPARAMPRECISION_W(Self: TPARAM; const T: INTEGER);
begin Self.PRECISION := T; end;

procedure TPARAMPRECISION_R(Self: TPARAM; var T: INTEGER);
begin T := Self.PRECISION; end;
{$ENDIF}
procedure TPARAMDATATYPE_W(Self: TPARAM; const T: TFIELDTYPE);
begin Self.DATATYPE := T; end;

procedure TPARAMDATATYPE_R(Self: TPARAM; var T: TFIELDTYPE);
begin T := Self.DATATYPE; end;

procedure TPARAMTEXT_W(Self: TPARAM; const T: String);
begin Self.TEXT := T; end;

procedure TPARAMTEXT_R(Self: TPARAM; var T: String);
begin T := Self.TEXT; end;

procedure TPARAMNATIVESTR_W(Self: TPARAM; const T: String);
begin Self.NATIVESTR := T; end;

procedure TPARAMNATIVESTR_R(Self: TPARAM; var T: String);
begin T := Self.NATIVESTR; end;

procedure TPARAMISNULL_R(Self: TPARAM; var T: BOOLEAN);
begin T := Self.ISNULL; end;

procedure TPARAMBOUND_W(Self: TPARAM; const T: BOOLEAN);
begin Self.BOUND := T; end;

procedure TPARAMBOUND_R(Self: TPARAM; var T: BOOLEAN);
begin T := Self.BOUND; end;

procedure TPARAMASWORD_W(Self: TPARAM; const T: LONGINT);
begin Self.ASWORD := T; end;

procedure TPARAMASWORD_R(Self: TPARAM; var T: LONGINT);
begin T := Self.ASWORD; end;

procedure TPARAMASTIME_W(Self: TPARAM; const T: TDATETIME);
begin Self.ASTIME := T; end;

procedure TPARAMASTIME_R(Self: TPARAM; var T: TDATETIME);
begin T := Self.ASTIME; end;

procedure TPARAMASSTRING_W(Self: TPARAM; const T: String);
begin Self.ASSTRING := T; end;

procedure TPARAMASSTRING_R(Self: TPARAM; var T: String);
begin T := Self.ASSTRING; end;

procedure TPARAMASMEMO_W(Self: TPARAM; const T: String);
begin Self.ASMEMO := T; end;

procedure TPARAMASMEMO_R(Self: TPARAM; var T: String);
begin T := Self.ASMEMO; end;

procedure TPARAMASSMALLINT_W(Self: TPARAM; const T: LONGINT);
begin Self.ASSMALLINT := T; end;

procedure TPARAMASSMALLINT_R(Self: TPARAM; var T: LONGINT);
begin T := Self.ASSMALLINT; end;

procedure TPARAMASINTEGER_W(Self: TPARAM; const T: LONGINT);
begin Self.ASINTEGER := T; end;

procedure TPARAMASINTEGER_R(Self: TPARAM; var T: LONGINT);
begin T := Self.ASINTEGER; end;

procedure TPARAMASFLOAT_W(Self: TPARAM; const T: DOUBLE);
begin Self.ASFLOAT := T; end;

procedure TPARAMASFLOAT_R(Self: TPARAM; var T: DOUBLE);
begin T := Self.ASFLOAT; end;

procedure TPARAMASDATETIME_W(Self: TPARAM; const T: TDATETIME);
begin Self.ASDATETIME := T; end;

procedure TPARAMASDATETIME_R(Self: TPARAM; var T: TDATETIME);
begin T := Self.ASDATETIME; end;

procedure TPARAMASDATE_W(Self: TPARAM; const T: TDATETIME);
begin Self.ASDATE := T; end;

procedure TPARAMASDATE_R(Self: TPARAM; var T: TDATETIME);
begin T := Self.ASDATE; end;

procedure TPARAMASCURRENCY_W(Self: TPARAM; const T: CURRENCY);
begin Self.ASCURRENCY := T; end;

procedure TPARAMASCURRENCY_R(Self: TPARAM; var T: CURRENCY);
begin T := Self.ASCURRENCY; end;

procedure TPARAMASBOOLEAN_W(Self: TPARAM; const T: BOOLEAN);
begin Self.ASBOOLEAN := T; end;

procedure TPARAMASBOOLEAN_R(Self: TPARAM; var T: BOOLEAN);
begin T := Self.ASBOOLEAN; end;

procedure TPARAMASBLOB_W(Self: TPARAM; const T: TBLOBDATA);
begin Self.ASBLOB := T; end;

procedure TPARAMASBLOB_R(Self: TPARAM; var T: TBLOBDATA);
begin T := Self.ASBLOB; end;

{$IFNDEF FPC}

{$IFDEF DELPHI6UP}
procedure TPARAMASFMTBCD_W(Self: TPARAM; const T: TBCD);
begin Self.ASFMTBCD := T; end;

procedure TPARAMASFMTBCD_R(Self: TPARAM; var T: TBCD);
begin T := Self.ASFMTBCD; end;
{$ENDIF}
procedure TPARAMASBCD_W(Self: TPARAM; const T: CURRENCY);
begin Self.ASBCD := T; end;

procedure TPARAMASBCD_R(Self: TPARAM; var T: CURRENCY);
begin T := Self.ASBCD; end;

procedure TREFERENCEFIELDREFERENCETABLENAME_W(Self: TREFERENCEFIELD; const T: String);
begin Self.REFERENCETABLENAME := T; end;

procedure TREFERENCEFIELDREFERENCETABLENAME_R(Self: TREFERENCEFIELD; var T: String);
begin T := Self.REFERENCETABLENAME; end;


procedure TDATASETFIELDINCLUDEOBJECTFIELD_W(Self: TDATASETFIELD; const T: BOOLEAN);
begin Self.INCLUDEOBJECTFIELD := T; end;

procedure TDATASETFIELDINCLUDEOBJECTFIELD_R(Self: TDATASETFIELD; var T: BOOLEAN);
begin T := Self.INCLUDEOBJECTFIELD; end;

procedure TDATASETFIELDNESTEDDATASET_R(Self: TDATASETFIELD; var T: TDATASET);
begin T := Self.NESTEDDATASET; end;

procedure TOBJECTFIELDOBJECTTYPE_W(Self: TOBJECTFIELD; const T: String);
begin Self.OBJECTTYPE := T; end;

procedure TOBJECTFIELDOBJECTTYPE_R(Self: TOBJECTFIELD; var T: String);
begin T := Self.OBJECTTYPE; end;

procedure TOBJECTFIELDUNNAMED_R(Self: TOBJECTFIELD; var T: BOOLEAN);
begin T := Self.UNNAMED; end;

procedure TOBJECTFIELDFIELDVALUES_W(Self: TOBJECTFIELD; const T: VARIANT; const t1: INTEGER);
begin Self.FIELDVALUES[t1] := T; end;

procedure TOBJECTFIELDFIELDVALUES_R(Self: TOBJECTFIELD; var T: VARIANT; const t1: INTEGER);
begin T := Self.FIELDVALUES[t1]; end;

procedure TOBJECTFIELDFIELDS_R(Self: TOBJECTFIELD; var T: TFIELDS);
begin T := Self.FIELDS; end;

procedure TOBJECTFIELDFIELDCOUNT_R(Self: TOBJECTFIELD; var T: INTEGER);
begin T := Self.FIELDCOUNT; end;
{$ENDIF}


{$IFNDEF FPC}
{$IFDEF DELPHI6UP}
procedure TBLOBFIELDGRAPHICHEADER_W(Self: TBLOBFIELD; const T: BOOLEAN);
begin Self.GRAPHICHEADER := T; end;

procedure TBLOBFIELDGRAPHICHEADER_R(Self: TBLOBFIELD; var T: BOOLEAN);
begin T := Self.GRAPHICHEADER; end;
{$ENDIF}
{$ENDIF}

procedure TBLOBFIELDBLOBTYPE_W(Self: TBLOBFIELD; const T: TBLOBTYPE);
begin Self.BLOBTYPE := T; end;

procedure TBLOBFIELDBLOBTYPE_R(Self: TBLOBFIELD; var T: TBLOBTYPE);
begin T := Self.BLOBTYPE; end;

procedure TBLOBFIELDTRANSLITERATE_W(Self: TBLOBFIELD; const T: BOOLEAN);
begin Self.TRANSLITERATE := T; end;

procedure TBLOBFIELDTRANSLITERATE_R(Self: TBLOBFIELD; var T: BOOLEAN);
begin T := Self.TRANSLITERATE; end;

procedure TBLOBFIELDVALUE_W(Self: TBLOBFIELD; const T: String);
{$IFDEF DELPHI2009UP}
var
  b: TBytes;
begin
  setLEngth(b, Length(T));
  Move(T[1], b[0], Length(T));
  self.Value := b;
  {$ELSE}
begin
  Self.VALUE := T;
  {$ENDIF}
end;

procedure TBLOBFIELDVALUE_R(Self: TBLOBFIELD; var T: String);
begin
{$IFDEF DELPHI2009UP}
  SetLength(t, Length(SElf.Value));
  Move(Self.Value[0], t[1], LEngth(T));
{$ELSE}
  T := Self.VALUE;
{$ENDIF}
end;

procedure TBLOBFIELDMODIFIED_W(Self: TBLOBFIELD; const T: BOOLEAN);
begin Self.MODIFIED := T; end;

procedure TBLOBFIELDMODIFIED_R(Self: TBLOBFIELD; var T: BOOLEAN);
begin T := Self.MODIFIED; end;

procedure TBLOBFIELDBLOBSIZE_R(Self: TBLOBFIELD; var T: INTEGER);
begin T := Self.BLOBSIZE; end;

{$IFNDEF FPC}
{$IFDEF DELPHI6UP}
procedure TFMTBCDFIELDPRECISION_W(Self: TFMTBCDFIELD; const T: INTEGER);
begin Self.PRECISION := T; end;

procedure TFMTBCDFIELDPRECISION_R(Self: TFMTBCDFIELD; var T: INTEGER);
begin T := Self.PRECISION; end;

procedure TFMTBCDFIELDMINVALUE_W(Self: TFMTBCDFIELD; const T: String);
begin Self.MINVALUE := T; end;

procedure TFMTBCDFIELDMINVALUE_R(Self: TFMTBCDFIELD; var T: String);
begin T := Self.MINVALUE; end;

procedure TFMTBCDFIELDMAXVALUE_W(Self: TFMTBCDFIELD; const T: String);
begin Self.MAXVALUE := T; end;

procedure TFMTBCDFIELDMAXVALUE_R(Self: TFMTBCDFIELD; var T: String);
begin T := Self.MAXVALUE; end;

procedure TFMTBCDFIELDCURRENCY_W(Self: TFMTBCDFIELD; const T: BOOLEAN);
begin Self.CURRENCY := T; end;

procedure TFMTBCDFIELDCURRENCY_R(Self: TFMTBCDFIELD; var T: BOOLEAN);
begin T := Self.CURRENCY; end;

procedure TFMTBCDFIELDVALUE_W(Self: TFMTBCDFIELD; const T: TBCD);
begin Self.VALUE := T; end;

procedure TFMTBCDFIELDVALUE_R(Self: TFMTBCDFIELD; var T: TBCD);
begin T := Self.VALUE; end;
{$ENDIF}

procedure TBCDFIELDPRECISION_W(Self: TBCDFIELD; const T: INTEGER);
begin Self.PRECISION := T; end;

procedure TBCDFIELDPRECISION_R(Self: TBCDFIELD; var T: INTEGER);
begin T := Self.PRECISION; end;

procedure TBCDFIELDMINVALUE_W(Self: TBCDFIELD; const T: CURRENCY);
begin Self.MINVALUE := T; end;

procedure TBCDFIELDMINVALUE_R(Self: TBCDFIELD; var T: CURRENCY);
begin T := Self.MINVALUE; end;

procedure TBCDFIELDMAXVALUE_W(Self: TBCDFIELD; const T: CURRENCY);
begin Self.MAXVALUE := T; end;

procedure TBCDFIELDMAXVALUE_R(Self: TBCDFIELD; var T: CURRENCY);
begin T := Self.MAXVALUE; end;

procedure TBCDFIELDCURRENCY_W(Self: TBCDFIELD; const T: BOOLEAN);
begin Self.CURRENCY := T; end;

procedure TBCDFIELDCURRENCY_R(Self: TBCDFIELD; var T: BOOLEAN);
begin T := Self.CURRENCY; end;

procedure TBCDFIELDVALUE_W(Self: TBCDFIELD; const T: CURRENCY);
begin Self.VALUE := T; end;

procedure TBCDFIELDVALUE_R(Self: TBCDFIELD; var T: CURRENCY);
begin T := Self.VALUE; end;
{$ENDIF}


procedure TDATETIMEFIELDDISPLAYFORMAT_W(Self: TDATETIMEFIELD; const T: String);
begin Self.DISPLAYFORMAT := T; end;

procedure TDATETIMEFIELDDISPLAYFORMAT_R(Self: TDATETIMEFIELD; var T: String);
begin T := Self.DISPLAYFORMAT; end;

procedure TDATETIMEFIELDVALUE_W(Self: TDATETIMEFIELD; const T: TDATETIME);
begin Self.VALUE := T; end;

procedure TDATETIMEFIELDVALUE_R(Self: TDATETIMEFIELD; var T: TDATETIME);
begin T := Self.VALUE; end;

procedure TBOOLEANFIELDDISPLAYVALUES_W(Self: TBOOLEANFIELD; const T: String);
begin Self.DISPLAYVALUES := T; end;

procedure TBOOLEANFIELDDISPLAYVALUES_R(Self: TBOOLEANFIELD; var T: String);
begin T := Self.DISPLAYVALUES; end;

procedure TBOOLEANFIELDVALUE_W(Self: TBOOLEANFIELD; const T: BOOLEAN);
begin Self.VALUE := T; end;

procedure TBOOLEANFIELDVALUE_R(Self: TBOOLEANFIELD; var T: BOOLEAN);
begin T := Self.VALUE; end;

procedure TFLOATFIELDPRECISION_W(Self: TFLOATFIELD; const T: INTEGER);
begin Self.PRECISION := T; end;

procedure TFLOATFIELDPRECISION_R(Self: TFLOATFIELD; var T: INTEGER);
begin T := Self.PRECISION; end;

procedure TFLOATFIELDMINVALUE_W(Self: TFLOATFIELD; const T: DOUBLE);
begin Self.MINVALUE := T; end;

procedure TFLOATFIELDMINVALUE_R(Self: TFLOATFIELD; var T: DOUBLE);
begin T := Self.MINVALUE; end;

procedure TFLOATFIELDMAXVALUE_W(Self: TFLOATFIELD; const T: DOUBLE);
begin Self.MAXVALUE := T; end;

procedure TFLOATFIELDMAXVALUE_R(Self: TFLOATFIELD; var T: DOUBLE);
begin T := Self.MAXVALUE; end;

{$IFNDEF FPC}
procedure TFLOATFIELDCURRENCY_W(Self: TFLOATFIELD; const T: BOOLEAN);
begin Self.CURRENCY := T; end;

procedure TFLOATFIELDCURRENCY_R(Self: TFLOATFIELD; var T: BOOLEAN);
begin T := Self.CURRENCY; end;
{$ENDIF}

procedure TFLOATFIELDVALUE_W(Self: TFLOATFIELD; const T: DOUBLE);
begin Self.VALUE := T; end;

procedure TFLOATFIELDVALUE_R(Self: TFLOATFIELD; var T: DOUBLE);
begin T := Self.VALUE; end;

procedure TLARGEINTFIELDMINVALUE_W(Self: TLARGEINTFIELD; const T: LARGEINT);
begin Self.MINVALUE := T; end;

procedure TLARGEINTFIELDMINVALUE_R(Self: TLARGEINTFIELD; var T: LARGEINT);
begin T := Self.MINVALUE; end;

procedure TLARGEINTFIELDMAXVALUE_W(Self: TLARGEINTFIELD; const T: LARGEINT);
begin Self.MAXVALUE := T; end;

procedure TLARGEINTFIELDMAXVALUE_R(Self: TLARGEINTFIELD; var T: LARGEINT);
begin T := Self.MAXVALUE; end;

procedure TLARGEINTFIELDVALUE_W(Self: TLARGEINTFIELD; const T: LARGEINT);
begin Self.VALUE := T; end;

procedure TLARGEINTFIELDVALUE_R(Self: TLARGEINTFIELD; var T: LARGEINT);
begin T := Self.VALUE; end;

procedure TLARGEINTFIELDASLARGEINT_W(Self: TLARGEINTFIELD; const T: LARGEINT);
begin Self.ASLARGEINT := T; end;

procedure TLARGEINTFIELDASLARGEINT_R(Self: TLARGEINTFIELD; var T: LARGEINT);
begin T := Self.ASLARGEINT; end;

procedure TINTEGERFIELDMINVALUE_W(Self: TINTEGERFIELD; const T: LONGINT);
begin Self.MINVALUE := T; end;

procedure TINTEGERFIELDMINVALUE_R(Self: TINTEGERFIELD; var T: LONGINT);
begin T := Self.MINVALUE; end;

procedure TINTEGERFIELDMAXVALUE_W(Self: TINTEGERFIELD; const T: LONGINT);
begin Self.MAXVALUE := T; end;

procedure TINTEGERFIELDMAXVALUE_R(Self: TINTEGERFIELD; var T: LONGINT);
begin T := Self.MAXVALUE; end;

procedure TINTEGERFIELDVALUE_W(Self: TINTEGERFIELD; const T: LONGINT);
begin Self.VALUE := T; end;

procedure TINTEGERFIELDVALUE_R(Self: TINTEGERFIELD; var T: LONGINT);
begin T := Self.VALUE; end;

procedure TNUMERICFIELDEDITFORMAT_W(Self: TNUMERICFIELD; const T: String);
begin Self.EDITFORMAT := T; end;

procedure TNUMERICFIELDEDITFORMAT_R(Self: TNUMERICFIELD; var T: String);
begin T := Self.EDITFORMAT; end;

procedure TNUMERICFIELDDISPLAYFORMAT_W(Self: TNUMERICFIELD; const T: String);
begin Self.DISPLAYFORMAT := T; end;

procedure TNUMERICFIELDDISPLAYFORMAT_R(Self: TNUMERICFIELD; var T: String);
begin T := Self.DISPLAYFORMAT; end;

{$IFNDEF FPC}
procedure TWIDESTRINGFIELDVALUE_W(Self: TWIDESTRINGFIELD; const T: WIDESTRING);
begin Self.VALUE := T; end;

procedure TWIDESTRINGFIELDVALUE_R(Self: TWIDESTRINGFIELD; var T: WIDESTRING);
begin T := Self.VALUE; end;

procedure TSTRINGFIELDTRANSLITERATE_W(Self: TSTRINGFIELD; const T: BOOLEAN);
begin Self.TRANSLITERATE := T; end;

procedure TSTRINGFIELDTRANSLITERATE_R(Self: TSTRINGFIELD; var T: BOOLEAN);
begin T := Self.TRANSLITERATE; end;

procedure TSTRINGFIELDFIXEDCHAR_W(Self: TSTRINGFIELD; const T: BOOLEAN);
begin Self.FIXEDCHAR := T; end;

procedure TSTRINGFIELDFIXEDCHAR_R(Self: TSTRINGFIELD; var T: BOOLEAN);
begin T := Self.FIXEDCHAR; end;
{$ENDIF}


procedure TSTRINGFIELDVALUE_W(Self: TSTRINGFIELD; const T: String);
begin Self.VALUE := T; end;

procedure TSTRINGFIELDVALUE_R(Self: TSTRINGFIELD; var T: String);
begin T := Self.VALUE; end;

procedure TFIELDONVALIDATE_W(Self: TFIELD; const T: TFIELDNOTIFYEVENT);
begin Self.ONVALIDATE := T; end;

procedure TFIELDONVALIDATE_R(Self: TFIELD; var T: TFIELDNOTIFYEVENT);
begin T := Self.ONVALIDATE; end;

procedure TFIELDONSETTEXT_W(Self: TFIELD; const T: TFIELDSETTEXTEVENT);
begin Self.ONSETTEXT := T; end;

procedure TFIELDONSETTEXT_R(Self: TFIELD; var T: TFIELDSETTEXTEVENT);
begin T := Self.ONSETTEXT; end;

procedure TFIELDONGETTEXT_W(Self: TFIELD; const T: TFIELDGETTEXTEVENT);
begin Self.ONGETTEXT := T; end;

procedure TFIELDONGETTEXT_R(Self: TFIELD; var T: TFIELDGETTEXTEVENT);
begin T := Self.ONGETTEXT; end;

procedure TFIELDONCHANGE_W(Self: TFIELD; const T: TFIELDNOTIFYEVENT);
begin Self.ONCHANGE := T; end;

procedure TFIELDONCHANGE_R(Self: TFIELD; var T: TFIELDNOTIFYEVENT);
begin T := Self.ONCHANGE; end;

procedure TFIELDVISIBLE_W(Self: TFIELD; const T: BOOLEAN);
begin Self.VISIBLE := T; end;

procedure TFIELDVISIBLE_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.VISIBLE; end;

procedure TFIELDREQUIRED_W(Self: TFIELD; const T: BOOLEAN);
begin Self.REQUIRED := T; end;

procedure TFIELDREQUIRED_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.REQUIRED; end;

procedure TFIELDREADONLY_W(Self: TFIELD; const T: BOOLEAN);
begin Self.READONLY := T; end;

procedure TFIELDREADONLY_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.READONLY; end;

procedure TFIELDPROVIDERFLAGS_W(Self: TFIELD; const T: TPROVIDERFLAGS);
begin Self.PROVIDERFLAGS := T; end;

procedure TFIELDPROVIDERFLAGS_R(Self: TFIELD; var T: TPROVIDERFLAGS);
begin T := Self.PROVIDERFLAGS; end;

procedure TFIELDORIGIN_W(Self: TFIELD; const T: String);
begin Self.ORIGIN := T; end;

procedure TFIELDORIGIN_R(Self: TFIELD; var T: String);
begin T := Self.ORIGIN; end;

procedure TFIELDLOOKUPCACHE_W(Self: TFIELD; const T: BOOLEAN);
begin Self.LOOKUPCACHE := T; end;

procedure TFIELDLOOKUPCACHE_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.LOOKUPCACHE; end;

procedure TFIELDKEYFIELDS_W(Self: TFIELD; const T: String);
begin Self.KEYFIELDS := T; end;

procedure TFIELDKEYFIELDS_R(Self: TFIELD; var T: String);
begin T := Self.KEYFIELDS; end;

procedure TFIELDLOOKUPRESULTFIELD_W(Self: TFIELD; const T: String);
begin Self.LOOKUPRESULTFIELD := T; end;

procedure TFIELDLOOKUPRESULTFIELD_R(Self: TFIELD; var T: String);
begin T := Self.LOOKUPRESULTFIELD; end;

procedure TFIELDLOOKUPKEYFIELDS_W(Self: TFIELD; const T: String);
begin Self.LOOKUPKEYFIELDS := T; end;

procedure TFIELDLOOKUPKEYFIELDS_R(Self: TFIELD; var T: String);
begin T := Self.LOOKUPKEYFIELDS; end;

procedure TFIELDLOOKUPDATASET_W(Self: TFIELD; const T: TDATASET);
begin Self.LOOKUPDATASET := T; end;

procedure TFIELDLOOKUPDATASET_R(Self: TFIELD; var T: TDATASET);
begin T := Self.LOOKUPDATASET; end;

procedure TFIELDIMPORTEDCONSTRAINT_W(Self: TFIELD; const T: String);
begin Self.IMPORTEDCONSTRAINT := T; end;

procedure TFIELDIMPORTEDCONSTRAINT_R(Self: TFIELD; var T: String);
begin T := Self.IMPORTEDCONSTRAINT; end;

procedure TFIELDINDEX_W(Self: TFIELD; const T: INTEGER);
begin Self.INDEX := T; end;

procedure TFIELDINDEX_R(Self: TFIELD; var T: INTEGER);
begin T := Self.INDEX; end;

procedure TFIELDHASCONSTRAINTS_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.HASCONSTRAINTS; end;

procedure TFIELDFIELDNAME_W(Self: TFIELD; const T: String);
begin Self.FIELDNAME := T; end;

procedure TFIELDFIELDNAME_R(Self: TFIELD; var T: String);
begin T := Self.FIELDNAME; end;

procedure TFIELDFIELDKIND_W(Self: TFIELD; const T: TFIELDKIND);
begin Self.FIELDKIND := T; end;

procedure TFIELDFIELDKIND_R(Self: TFIELD; var T: TFIELDKIND);
begin T := Self.FIELDKIND; end;

procedure TFIELDDISPLAYWIDTH_W(Self: TFIELD; const T: INTEGER);
begin Self.DISPLAYWIDTH := T; end;

procedure TFIELDDISPLAYWIDTH_R(Self: TFIELD; var T: INTEGER);
begin T := Self.DISPLAYWIDTH; end;

procedure TFIELDDISPLAYLABEL_W(Self: TFIELD; const T: String);
begin Self.DISPLAYLABEL := T; end;

procedure TFIELDDISPLAYLABEL_R(Self: TFIELD; var T: String);
begin T := Self.DISPLAYLABEL; end;

procedure TFIELDDEFAULTEXPRESSION_W(Self: TFIELD; const T: String);
begin Self.DEFAULTEXPRESSION := T; end;

procedure TFIELDDEFAULTEXPRESSION_R(Self: TFIELD; var T: String);
begin T := Self.DEFAULTEXPRESSION; end;

procedure TFIELDCONSTRAINTERRORMESSAGE_W(Self: TFIELD; const T: String);
begin Self.CONSTRAINTERRORMESSAGE := T; end;

procedure TFIELDCONSTRAINTERRORMESSAGE_R(Self: TFIELD; var T: String);
begin T := Self.CONSTRAINTERRORMESSAGE; end;

procedure TFIELDCUSTOMCONSTRAINT_W(Self: TFIELD; const T: String);
begin Self.CUSTOMCONSTRAINT := T; end;

procedure TFIELDCUSTOMCONSTRAINT_R(Self: TFIELD; var T: String);
begin T := Self.CUSTOMCONSTRAINT; end;

{$IFNDEF FPC}
procedure TFIELDAUTOGENERATEVALUE_W(Self: TFIELD; const T: TAUTOREFRESHFLAG);
begin Self.AUTOGENERATEVALUE := T; end;

procedure TFIELDAUTOGENERATEVALUE_R(Self: TFIELD; var T: TAUTOREFRESHFLAG);
begin T := Self.AUTOGENERATEVALUE; end;

procedure TFIELDVALIDCHARS_W(Self: TFIELD; const T: TFIELDCHARS);
begin Self.VALIDCHARS := T; end;

procedure TFIELDVALIDCHARS_R(Self: TFIELD; var T: TFIELDCHARS);
begin T := Self.VALIDCHARS; end;


procedure TFIELDPARENTFIELD_W(Self: TFIELD; const T: TOBJECTFIELD);
begin Self.PARENTFIELD := T; end;

procedure TFIELDPARENTFIELD_R(Self: TFIELD; var T: TOBJECTFIELD);
begin T := Self.PARENTFIELD; end;



{$ENDIF}

procedure TFIELDALIGNMENT_W(Self: TFIELD; const T: TALIGNMENT);
begin Self.ALIGNMENT := T; end;

procedure TFIELDALIGNMENT_R(Self: TFIELD; var T: TALIGNMENT);
begin T := Self.ALIGNMENT; end;

procedure TFIELDVALUE_W(Self: TFIELD; const T: VARIANT);
begin Self.VALUE := T; end;

procedure TFIELDVALUE_R(Self: TFIELD; var T: VARIANT);
begin T := Self.VALUE; end;

procedure TFIELDTEXT_W(Self: TFIELD; const T: String);
begin Self.TEXT := T; end;

procedure TFIELDTEXT_R(Self: TFIELD; var T: String);
begin T := Self.TEXT; end;

procedure TFIELDSIZE_W(Self: TFIELD; const T: INTEGER);
begin Self.SIZE := T; end;

procedure TFIELDSIZE_R(Self: TFIELD; var T: INTEGER);
begin T := Self.SIZE; end;

procedure TFIELDOLDVALUE_R(Self: TFIELD; var T: VARIANT);
begin T := Self.OLDVALUE; end;

procedure TFIELDOFFSET_R(Self: TFIELD; var T: INTEGER);
begin T := Self.OFFSET; end;

procedure TFIELDNEWVALUE_W(Self: TFIELD; const T: VARIANT);
begin Self.NEWVALUE := T; end;

procedure TFIELDNEWVALUE_R(Self: TFIELD; var T: VARIANT);
begin T := Self.NEWVALUE; end;

procedure TFIELDLOOKUPLIST_R(Self: TFIELD; var T: TLOOKUPLIST);
begin T := Self.LOOKUPLIST; end;

{$IFNDEF FPC}
procedure TFIELDLOOKUP_W(Self: TFIELD; const T: BOOLEAN);
begin Self.LOOKUP := T; end;

procedure TFIELDLOOKUP_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.LOOKUP; end;

procedure TFIELDFULLNAME_R(Self: TFIELD; var T: String);
begin T := Self.FULLNAME; end;


procedure TFIELDEDITMASKPTR_R(Self: TFIELD; var T: String);
begin T := Self.EDITMASKPTR; end;

procedure TFIELDEDITMASK_W(Self: TFIELD; const T: String);
begin Self.EDITMASK := T; end;

procedure TFIELDEDITMASK_R(Self: TFIELD; var T: String);
begin T := Self.EDITMASK; end;

{$ENDIF}

procedure TFIELDISNULL_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.ISNULL; end;

procedure TFIELDISINDEXFIELD_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.ISINDEXFIELD; end;

procedure TFIELDFIELDNO_R(Self: TFIELD; var T: INTEGER);
begin T := Self.FIELDNO; end;



procedure TFIELDDISPLAYTEXT_R(Self: TFIELD; var T: String);
begin T := Self.DISPLAYTEXT; end;

procedure TFIELDDISPLAYNAME_R(Self: TFIELD; var T: String);
begin T := Self.DISPLAYNAME; end;

procedure TFIELDDATATYPE_R(Self: TFIELD; var T: TFIELDTYPE);
begin T := Self.DATATYPE; end;

procedure TFIELDDATASIZE_R(Self: TFIELD; var T: INTEGER);
begin T := Self.DATASIZE; end;

procedure TFIELDDATASET_W(Self: TFIELD; const T: TDATASET);
begin Self.DATASET := T; end;

procedure TFIELDDATASET_R(Self: TFIELD; var T: TDATASET);
begin T := Self.DATASET; end;

procedure TFIELDCURVALUE_R(Self: TFIELD; var T: VARIANT);
begin T := Self.CURVALUE; end;

procedure TFIELDCANMODIFY_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.CANMODIFY; end;

procedure TFIELDCALCULATED_W(Self: TFIELD; const T: BOOLEAN);
begin Self.CALCULATED := T; end;

procedure TFIELDCALCULATED_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.CALCULATED; end;

procedure TFIELDATTRIBUTESET_W(Self: TFIELD; const T: String);
begin Self.ATTRIBUTESET := T; end;

procedure TFIELDATTRIBUTESET_R(Self: TFIELD; var T: String);
begin T := Self.ATTRIBUTESET; end;

procedure TFIELDASVARIANT_W(Self: TFIELD; const T: VARIANT);
begin Self.ASVARIANT := T; end;

procedure TFIELDASVARIANT_R(Self: TFIELD; var T: VARIANT);
begin T := Self.ASVARIANT; end;

procedure TFIELDASSTRING_W(Self: TFIELD; const T: String);
begin Self.ASSTRING := T; end;

procedure TFIELDASSTRING_R(Self: TFIELD; var T: String);
begin T := Self.ASSTRING; end;

procedure TFIELDASINTEGER_W(Self: TFIELD; const T: LONGINT);
begin Self.ASINTEGER := T; end;

procedure TFIELDASINTEGER_R(Self: TFIELD; var T: LONGINT);
begin T := Self.ASINTEGER; end;

procedure TFIELDASFLOAT_W(Self: TFIELD; const T: DOUBLE);
begin Self.ASFLOAT := T; end;

procedure TFIELDASFLOAT_R(Self: TFIELD; var T: DOUBLE);
begin T := Self.ASFLOAT; end;

procedure TFIELDASDATETIME_W(Self: TFIELD; const T: TDATETIME);
begin Self.ASDATETIME := T; end;

procedure TFIELDASDATETIME_R(Self: TFIELD; var T: TDATETIME);
begin T := Self.ASDATETIME; end;

procedure TFIELDASCURRENCY_W(Self: TFIELD; const T: CURRENCY);
begin Self.ASCURRENCY := T; end;

procedure TFIELDASCURRENCY_R(Self: TFIELD; var T: CURRENCY);
begin T := Self.ASCURRENCY; end;

procedure TFIELDASBOOLEAN_W(Self: TFIELD; const T: BOOLEAN);
begin Self.ASBOOLEAN := T; end;

procedure TFIELDASBOOLEAN_R(Self: TFIELD; var T: BOOLEAN);
begin T := Self.ASBOOLEAN; end;

{$IFNDEF FPC}
{$IFDEF DELPHI6UP}
procedure TFIELDASBCD_W(Self: TFIELD; const T: TBCD);
begin Self.ASBCD := T; end;

procedure TFIELDASBCD_R(Self: TFIELD; var T: TBCD);
begin T := Self.ASBCD; end;
{$ENDIF}

procedure TFIELDLISTFIELDS_R(Self: TFIELDLIST; var T: TFIELD; const t1: INTEGER);
begin T := Self.FIELDS[t1]; end;

procedure TFIELDDEFLISTFIELDDEFS_R(Self: TFIELDDEFLIST; var T: TFIELDDEF; const t1: INTEGER);
begin T := Self.FIELDDEFS[t1]; end;

procedure TFLATLISTDATASET_R(Self: TFLATLIST; var T: TDATASET);
begin T := Self.DATASET; end;

procedure TINDEXDEFGROUPINGLEVEL_W(Self: TINDEXDEF; const T: INTEGER);
begin Self.GROUPINGLEVEL := T; end;

procedure TINDEXDEFGROUPINGLEVEL_R(Self: TINDEXDEF; var T: INTEGER);
begin T := Self.GROUPINGLEVEL; end;



{$ENDIF}

procedure TFIELDSFIELDS_W(Self: TFIELDS; const T: TFIELD; const t1: INTEGER);
begin Self.FIELDS[t1] := T; end;

procedure TFIELDSFIELDS_R(Self: TFIELDS; var T: TFIELD; const t1: INTEGER);
begin T := Self.FIELDS[t1]; end;

procedure TFIELDSDATASET_R(Self: TFIELDS; var T: TDATASET);
begin T := Self.DATASET; end;

procedure TFIELDSCOUNT_R(Self: TFIELDS; var T: INTEGER);
begin T := Self.COUNT; end;

procedure TINDEXDEFSITEMS_W(Self: TINDEXDEFS; const T: TINDEXDEF; const t1: INTEGER);
begin Self.ITEMS[t1] := T; end;

procedure TINDEXDEFSITEMS_R(Self: TINDEXDEFS; var T: TINDEXDEF; const t1: INTEGER);
begin T := Self.ITEMS[t1]; end;

procedure TINDEXDEFSOURCE_W(Self: TINDEXDEF; const T: String);
begin Self.SOURCE := T; end;

procedure TINDEXDEFSOURCE_R(Self: TINDEXDEF; var T: String);
begin T := Self.SOURCE; end;

procedure TINDEXDEFOPTIONS_W(Self: TINDEXDEF; const T: TINDEXOPTIONS);
begin Self.OPTIONS := T; end;

procedure TINDEXDEFOPTIONS_R(Self: TINDEXDEF; var T: TINDEXOPTIONS);
begin T := Self.OPTIONS; end;

procedure TINDEXDEFFIELDS_W(Self: TINDEXDEF; const T: String);
begin Self.FIELDS := T; end;

procedure TINDEXDEFFIELDS_R(Self: TINDEXDEF; var T: String);
begin T := Self.FIELDS; end;

procedure TINDEXDEFEXPRESSION_W(Self: TINDEXDEF; const T: String);
begin {$IFNDEF FPC}Self.EXPRESSION := T; {$ENDIF}end;

procedure TINDEXDEFEXPRESSION_R(Self: TINDEXDEF; var T: String);
begin T := Self.EXPRESSION; end;

{$IFNDEF FPC}
procedure TINDEXDEFDESCFIELDS_W(Self: TINDEXDEF; const T: String);
begin Self.DESCFIELDS := T; end;

procedure TINDEXDEFDESCFIELDS_R(Self: TINDEXDEF; var T: String);
begin T := Self.DESCFIELDS; end;

procedure TINDEXDEFCASEINSFIELDS_W(Self: TINDEXDEF; const T: String);
begin Self.CASEINSFIELDS := T; end;

procedure TINDEXDEFCASEINSFIELDS_R(Self: TINDEXDEF; var T: String);
begin T := Self.CASEINSFIELDS; end;


procedure TINDEXDEFFIELDEXPRESSION_R(Self: TINDEXDEF; var T: String);
begin T := Self.FIELDEXPRESSION; end;

procedure TFIELDDEFSPARENTDEF_R(Self: TFIELDDEFS; var T: TFIELDDEF);
begin T := Self.PARENTDEF; end;

{$ENDIF}

procedure TFIELDDEFSITEMS_W(Self: TFIELDDEFS; const T: TFIELDDEF; const t1: INTEGER);
begin Self.ITEMS[t1] := T; end;

procedure TFIELDDEFSITEMS_R(Self: TFIELDDEFS; var T: TFIELDDEF; const t1: INTEGER);
begin T := Self.ITEMS[t1]; end;

procedure TFIELDDEFSHIDDENFIELDS_W(Self: TFIELDDEFS; const T: BOOLEAN);
begin Self.HIDDENFIELDS := T; end;

procedure TFIELDDEFSHIDDENFIELDS_R(Self: TFIELDDEFS; var T: BOOLEAN);
begin T := Self.HIDDENFIELDS; end;

procedure TFIELDDEFSIZE_W(Self: TFIELDDEF; const T: INTEGER);
begin Self.SIZE := T; end;

procedure TFIELDDEFSIZE_R(Self: TFIELDDEF; var T: INTEGER);
begin T := Self.SIZE; end;

procedure TFIELDDEFPRECISION_W(Self: TFIELDDEF; const T: INTEGER);
begin Self.PRECISION := T; end;

procedure TFIELDDEFPRECISION_R(Self: TFIELDDEF; var T: INTEGER);
begin T := Self.PRECISION; end;

procedure TFIELDDEFDATATYPE_W(Self: TFIELDDEF; const T: TFIELDTYPE);
begin Self.DATATYPE := T; end;

procedure TFIELDDEFDATATYPE_R(Self: TFIELDDEF; var T: TFIELDTYPE);
begin T := Self.DATATYPE; end;

{$IFNDEF FPC}
procedure TFIELDDEFCHILDDEFS_W(Self: TFIELDDEF; const T: TFIELDDEFS);
begin Self.CHILDDEFS := T; end;

procedure TFIELDDEFCHILDDEFS_R(Self: TFIELDDEF; var T: TFIELDDEFS);
begin T := Self.CHILDDEFS; end;

procedure TFIELDDEFREQUIRED_W(Self: TFIELDDEF; const T: BOOLEAN);
begin Self.REQUIRED := T;end;

procedure TFIELDDEFPARENTDEF_R(Self: TFIELDDEF; var T: TFIELDDEF);
begin T := Self.PARENTDEF; end;

{$ENDIF}

procedure TFIELDDEFATTRIBUTES_W(Self: TFIELDDEF; const T: TFIELDATTRIBUTES);
begin Self.ATTRIBUTES := T; end;

procedure TFIELDDEFATTRIBUTES_R(Self: TFIELDDEF; var T: TFIELDATTRIBUTES);
begin T := Self.ATTRIBUTES; end;

procedure TFIELDDEFREQUIRED_R(Self: TFIELDDEF; var T: BOOLEAN);
begin T := Self.REQUIRED; end;

procedure TFIELDDEFINTERNALCALCFIELD_W(Self: TFIELDDEF; const T: BOOLEAN);
begin Self.INTERNALCALCFIELD := T; end;

procedure TFIELDDEFINTERNALCALCFIELD_R(Self: TFIELDDEF; var T: BOOLEAN);
begin T := Self.INTERNALCALCFIELD; end;

{$IFNDEF FPC}
procedure TFIELDDEFFIELDNO_W(Self: TFIELDDEF; const T: INTEGER);
begin Self.FIELDNO := T; end;

procedure TDEFCOLLECTIONUPDATED_W(Self: TDEFCOLLECTION; const T: BOOLEAN);
begin Self.UPDATED := T; end;

procedure TDEFCOLLECTIONUPDATED_R(Self: TDEFCOLLECTION; var T: BOOLEAN);
begin T := Self.UPDATED; end;

procedure TDEFCOLLECTIONDATASET_R(Self: TDEFCOLLECTION; var T: TDATASET);
begin T := Self.DATASET; end;

procedure TNAMEDITEMNAME_W(Self: TNAMEDITEM; const T: String);
begin Self.NAME := T; end;

procedure TNAMEDITEMNAME_R(Self: TNAMEDITEM; var T: String);
begin T := Self.NAME; end;


{$ENDIF}

procedure TFIELDDEFFIELDNO_R(Self: TFIELDDEF; var T: INTEGER);
begin T := Self.FIELDNO; end;

procedure TFIELDDEFFIELDCLASS_R(Self: TFIELDDEF; var T: TFIELDCLASS);
begin T := Self.FIELDCLASS; end;

procedure RIRegisterTDATASET(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TDATASET) do
  begin
  RegisterMethod(@TDATASET.ACTIVEBUFFER, 'ActiveBuffer');
  RegisterMethod(@TDATASET.APPEND, 'Append');
  RegisterMethod(@TDATASET.APPENDRECORD, 'AppendRecord');
//  RegisterVirtualMethod(@TDATASET.BOOKMARKVALID, 'BookmarkValid');
  RegisterVirtualMethod(@TDATASET.CANCEL, 'Cancel');
  RegisterMethod(@TDATASET.CHECKBROWSEMODE, 'CheckBrowseMode');
  RegisterMethod(@TDATASET.CLEARFIELDS, 'ClearFields');
  RegisterMethod(@TDATASET.CLOSE, 'Close');
  RegisterMethod(@TDATASET.CONTROLSDISABLED, 'ControlsDisabled');
//  RegisterVirtualMethod(@TDATASET.COMPAREBOOKMARKS, 'CompareBookmarks');
  RegisterVirtualMethod(@TDATASET.CREATEBLOBSTREAM, 'CreateBlobStream');
  RegisterMethod(@TDATASET.CURSORPOSCHANGED, 'CursorPosChanged');
  RegisterMethod(@TDATASET.DELETE, 'Delete');
  RegisterMethod(@TDATASET.DISABLECONTROLS, 'DisableControls');
  RegisterMethod(@TDATASET.EDIT, 'Edit');
  RegisterMethod(@TDATASET.ENABLECONTROLS, 'EnableControls');
  RegisterMethod(@TDATASET.FIELDBYNAME, 'FieldByName');
  RegisterMethod(@TDATASET.FINDFIELD, 'FindField');
  RegisterMethod(@TDATASET.FINDFIRST, 'FindFirst');
  RegisterMethod(@TDATASET.FINDLAST, 'FindLast');
  RegisterMethod(@TDATASET.FINDNEXT, 'FindNext');
  RegisterMethod(@TDATASET.FINDPRIOR, 'FindPrior');
  RegisterMethod(@TDATASET.FIRST, 'First');
//  RegisterVirtualMethod(@TDATASET.FREEBOOKMARK, 'FreeBookmark');
//  RegisterVirtualMethod(@TDATASET.GETBOOKMARK, 'GetBookmark');
  RegisterVirtualMethod(@TDATASET.GETCURRENTRECORD, 'GetCurrentRecord');
//  RegisterVirtualMethod(@TDATASET.GETDETAILDATASETS, 'GetDetailDataSets');
//  RegisterVirtualMethod(@TDATASET.GETDETAILLINKFIELDS, 'GetDetailLinkFields');
//  RegisterVirtualMethod(@TDATASET.GETBLOBFIELDDATA, 'GetBlobFieldData');
//  RegisterMethod(@TDATASET.GETFIELDLIST, 'GetFieldList');
  RegisterMethod(@TDATASET.GETFIELDNAMES, 'GetFieldNames');
//  RegisterMethod(@TDATASET.GOTOBOOKMARK, 'GotoBookmark');
  RegisterMethod(@TDATASET.INSERT, 'Insert');
  RegisterMethod(@TDATASET.INSERTRECORD, 'InsertRecord');
  RegisterMethod(@TDATASET.ISEMPTY, 'IsEmpty');
  RegisterMethod(@TDATASET.ISLINKEDTO, 'IsLinkedTo');
  RegisterVirtualMethod(@TDATASET.ISSEQUENCED, 'IsSequenced');
  RegisterMethod(@TDATASET.LAST, 'Last');
  RegisterVirtualMethod(@TDATASET.LOCATE, 'Locate');
  RegisterVirtualMethod(@TDATASET.LOOKUP, 'Lookup');
  RegisterMethod(@TDATASET.MOVEBY, 'MoveBy');
  RegisterMethod(@TDATASET.NEXT, 'Next');
  RegisterMethod(@TDATASET.OPEN, 'Open');
  RegisterVirtualMethod(@TDATASET.POST, 'Post');
  RegisterMethod(@TDATASET.PRIOR, 'Prior');
  RegisterMethod(@TDATASET.REFRESH, 'Refresh');
//  RegisterVirtualMethod(@TDATASET.RESYNC, 'Resync');
  RegisterMethod(@TDATASET.SETFIELDS, 'SetFields');
  RegisterVirtualMethod(@TDATASET.TRANSLATE, 'Translate');
  RegisterMethod(@TDATASET.UPDATECURSORPOS, 'UpdateCursorPos');
  RegisterMethod(@TDATASET.UPDATERECORD, 'UpdateRecord');
  RegisterVirtualMethod(@TDATASET.UPDATESTATUS, 'UpdateStatus');
  RegisterPropertyHelper(@TDATASETBOF_R,nil,'BOF');
//  RegisterPropertyHelper(@TDATASETBOOKMARK_R,@TDATASETBOOKMARK_W,'Bookmark');
  RegisterPropertyHelper(@TDATASETCANMODIFY_R,nil,'CanModify');
  RegisterPropertyHelper(@TDATASETDATASOURCE_R,nil,'DataSource');
  RegisterPropertyHelper(@TDATASETDEFAULTFIELDS_R,nil,'DefaultFields');
  RegisterPropertyHelper(@TDATASETEOF_R,nil,'EOF');
  RegisterPropertyHelper(@TDATASETFIELDCOUNT_R,nil,'FieldCount');
  RegisterPropertyHelper(@TDATASETFIELDS_R,nil,'Fields');
  RegisterPropertyHelper(@TDATASETFIELDVALUES_R,@TDATASETFIELDVALUES_W,'FieldValues');
  RegisterPropertyHelper(@TDATASETFOUND_R,nil,'Found');
{$IFDEF DELPHI6UP}
  RegisterPropertyHelper(@TDATASETISUNIDIRECTIONAL_R,nil,'IsUnidirectional');
{$ENDIF}
  RegisterPropertyHelper(@TDATASETMODIFIED_R,nil,'Modified');
  RegisterPropertyHelper(@TDATASETRECORDCOUNT_R,nil,'RecordCount');
  RegisterPropertyHelper(@TDATASETRECNO_R,@TDATASETRECNO_W,'RecNo');
  RegisterPropertyHelper(@TDATASETRECORDSIZE_R,nil,'RecordSize');
  RegisterPropertyHelper(@TDATASETSTATE_R,nil,'State');
  RegisterPropertyHelper(@TDATASETFILTER_R,@TDATASETFILTER_W,'Filter');
  RegisterPropertyHelper(@TDATASETFILTERED_R,@TDATASETFILTERED_W,'Filtered');
  RegisterPropertyHelper(@TDATASETFILTEROPTIONS_R,@TDATASETFILTEROPTIONS_W,'FilterOptions');
  RegisterPropertyHelper(@TDATASETACTIVE_R,@TDATASETACTIVE_W,'Active');
  RegisterPropertyHelper(@TDATASETAUTOCALCFIELDS_R,@TDATASETAUTOCALCFIELDS_W,'AutoCalcFields');
  RegisterPropertyHelper(@TDATASETBEFOREOPEN_R,@TDATASETBEFOREOPEN_W,'BeforeOpen');
  RegisterPropertyHelper(@TDATASETAFTEROPEN_R,@TDATASETAFTEROPEN_W,'AfterOpen');
  RegisterPropertyHelper(@TDATASETBEFORECLOSE_R,@TDATASETBEFORECLOSE_W,'BeforeClose');
  RegisterPropertyHelper(@TDATASETAFTERCLOSE_R,@TDATASETAFTERCLOSE_W,'AfterClose');
  RegisterPropertyHelper(@TDATASETBEFOREINSERT_R,@TDATASETBEFOREINSERT_W,'BeforeInsert');
  RegisterPropertyHelper(@TDATASETAFTERINSERT_R,@TDATASETAFTERINSERT_W,'AfterInsert');
  RegisterPropertyHelper(@TDATASETBEFOREEDIT_R,@TDATASETBEFOREEDIT_W,'BeforeEdit');
  RegisterPropertyHelper(@TDATASETAFTEREDIT_R,@TDATASETAFTEREDIT_W,'AfterEdit');
  RegisterPropertyHelper(@TDATASETBEFOREPOST_R,@TDATASETBEFOREPOST_W,'BeforePost');
  RegisterPropertyHelper(@TDATASETAFTERPOST_R,@TDATASETAFTERPOST_W,'AfterPost');
  RegisterPropertyHelper(@TDATASETBEFORECANCEL_R,@TDATASETBEFORECANCEL_W,'BeforeCancel');
  RegisterPropertyHelper(@TDATASETAFTERCANCEL_R,@TDATASETAFTERCANCEL_W,'AfterCancel');
  RegisterPropertyHelper(@TDATASETBEFOREDELETE_R,@TDATASETBEFOREDELETE_W,'BeforeDelete');
  RegisterPropertyHelper(@TDATASETAFTERDELETE_R,@TDATASETAFTERDELETE_W,'AfterDelete');
  RegisterPropertyHelper(@TDATASETBEFORESCROLL_R,@TDATASETBEFORESCROLL_W,'BeforeScroll');
  RegisterPropertyHelper(@TDATASETAFTERSCROLL_R,@TDATASETAFTERSCROLL_W,'AfterScroll');
  {$IFNDEF FPC}
  RegisterPropertyHelper(@TDATASETFIELDLIST_R,nil,'FieldList');
  RegisterPropertyHelper(@TDATASETDESIGNER_R,nil,'Designer');
  RegisterPropertyHelper(@TDATASETBLOCKREADSIZE_R,@TDATASETBLOCKREADSIZE_W,'BlockReadSize');
  RegisterPropertyHelper(@TDATASETBEFOREREFRESH_R,@TDATASETBEFOREREFRESH_W,'BeforeRefresh');
  RegisterPropertyHelper(@TDATASETAFTERREFRESH_R,@TDATASETAFTERREFRESH_W,'AfterRefresh');
  RegisterPropertyHelper(@TDATASETAGGFIELDS_R,nil,'AggFields');
  RegisterPropertyHelper(@TDATASETDATASETFIELD_R,@TDATASETDATASETFIELD_W,'DataSetField');
  RegisterPropertyHelper(@TDATASETOBJECTVIEW_R,@TDATASETOBJECTVIEW_W,'ObjectView');
  RegisterPropertyHelper(@TDATASETSPARSEARRAYS_R,@TDATASETSPARSEARRAYS_W,'SparseArrays');
  RegisterPropertyHelper(@TDATASETFIELDDEFS_R,@TDATASETFIELDDEFS_W,'FieldDefs');
  RegisterPropertyHelper(@TDATASETFIELDDEFLIST_R,nil,'FieldDefList');

  {$ENDIF}
  RegisterEventPropertyHelper(@TDATASETONCALCFIELDS_R,@TDATASETONCALCFIELDS_W,'OnCalcFields');
  RegisterEventPropertyHelper(@TDATASETONDELETEERROR_R,@TDATASETONDELETEERROR_W,'OnDeleteError');
  RegisterEventPropertyHelper(@TDATASETONEDITERROR_R,@TDATASETONEDITERROR_W,'OnEditError');
  RegisterEventPropertyHelper(@TDATASETONFILTERRECORD_R,@TDATASETONFILTERRECORD_W,'OnFilterRecord');
  RegisterEventPropertyHelper(@TDATASETONNEWRECORD_R,@TDATASETONNEWRECORD_W,'OnNewRecord');
  RegisterEventPropertyHelper(@TDATASETONPOSTERROR_R,@TDATASETONPOSTERROR_W,'OnPostError');
  end;
end;

procedure RIRegisterTPARAMS(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TPARAMS) do
  begin
//  RegisterMethod(@TPARAMS.ASSIGNVALUES, 'AssignValues');
  RegisterMethod(@TPARAMS.ADDPARAM, 'AddParam');
  RegisterMethod(@TPARAMS.REMOVEPARAM, 'RemoveParam');
  RegisterMethod(@TPARAMS.CREATEPARAM, 'CreateParam');
  RegisterMethod(@TPARAMS.GETPARAMLIST, 'GetParamList');
  RegisterMethod(@TPARAMS.ISEQUAL, 'IsEqual');
  RegisterMethod(@TPARAMS.PARSESQL, 'ParseSQL');
  RegisterMethod(@TPARAMS.PARAMBYNAME, 'ParamByName');
  RegisterMethod(@TPARAMS.FINDPARAM, 'FindParam');
  RegisterPropertyHelper(@TPARAMSITEMS_R,@TPARAMSITEMS_W,'Items');
  RegisterPropertyHelper(@TPARAMSPARAMVALUES_R,@TPARAMSPARAMVALUES_W,'ParamValues');
  end;
end;

procedure RIRegisterTPARAM(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TPARAM) do
  begin
  RegisterMethod(@TPARAM.ASSIGNFIELD, 'AssignField');
  RegisterMethod(@TPARAM.ASSIGNFIELDVALUE, 'AssignFieldValue');
  RegisterMethod(@TPARAM.CLEAR, 'Clear');
//  RegisterMethod(@TPARAM.GETDATA, 'GetData');
  RegisterMethod(@TPARAM.GETDATASIZE, 'GetDataSize');
  RegisterMethod(@TPARAM.LOADFROMFILE, 'LoadFromFile');
  RegisterMethod(@TPARAM.LOADFROMSTREAM, 'LoadFromStream');
//  RegisterMethod(@TPARAM.SETBLOBDATA, 'SetBlobData');
//  RegisterMethod(@TPARAM.SETDATA, 'SetData');
  {$IFNDEF FPC}
  RegisterPropertyHelper(@TPARAMASBCD_R,@TPARAMASBCD_W,'AsBCD');
{$IFDEF DELPHI6UP}
  RegisterPropertyHelper(@TPARAMASFMTBCD_R,@TPARAMASFMTBCD_W,'AsFMTBCD');
{$ENDIF}
  {$ENDIF}
  RegisterPropertyHelper(@TPARAMASBLOB_R,@TPARAMASBLOB_W,'AsBlob');
  RegisterPropertyHelper(@TPARAMASBOOLEAN_R,@TPARAMASBOOLEAN_W,'AsBoolean');
  RegisterPropertyHelper(@TPARAMASCURRENCY_R,@TPARAMASCURRENCY_W,'AsCurrency');
  RegisterPropertyHelper(@TPARAMASDATE_R,@TPARAMASDATE_W,'AsDate');
  RegisterPropertyHelper(@TPARAMASDATETIME_R,@TPARAMASDATETIME_W,'AsDateTime');
  RegisterPropertyHelper(@TPARAMASFLOAT_R,@TPARAMASFLOAT_W,'AsFloat');
  RegisterPropertyHelper(@TPARAMASINTEGER_R,@TPARAMASINTEGER_W,'AsInteger');
  RegisterPropertyHelper(@TPARAMASSMALLINT_R,@TPARAMASSMALLINT_W,'AsSmallInt');
  RegisterPropertyHelper(@TPARAMASMEMO_R,@TPARAMASMEMO_W,'AsMemo');
  RegisterPropertyHelper(@TPARAMASSTRING_R,@TPARAMASSTRING_W,'AsString');
  RegisterPropertyHelper(@TPARAMASTIME_R,@TPARAMASTIME_W,'AsTime');
  RegisterPropertyHelper(@TPARAMASWORD_R,@TPARAMASWORD_W,'AsWord');
  RegisterPropertyHelper(@TPARAMBOUND_R,@TPARAMBOUND_W,'Bound');
  RegisterPropertyHelper(@TPARAMISNULL_R,nil,'IsNull');
  RegisterPropertyHelper(@TPARAMNATIVESTR_R,@TPARAMNATIVESTR_W,'NativeStr');
  RegisterPropertyHelper(@TPARAMTEXT_R,@TPARAMTEXT_W,'Text');
  RegisterPropertyHelper(@TPARAMDATATYPE_R,@TPARAMDATATYPE_W,'DataType');
{$IFDEF DELPHI6UP}
  RegisterPropertyHelper(@TPARAMPRECISION_R,@TPARAMPRECISION_W,'Precision');
  RegisterPropertyHelper(@TPARAMNUMERICSCALE_R,@TPARAMNUMERICSCALE_W,'NumericScale');
  RegisterPropertyHelper(@TPARAMSIZE_R,@TPARAMSIZE_W,'Size');
{$ENDIF}
  RegisterPropertyHelper(@TPARAMNAME_R,@TPARAMNAME_W,'Name');
  RegisterPropertyHelper(@TPARAMPARAMTYPE_R,@TPARAMPARAMTYPE_W,'ParamType');
  RegisterPropertyHelper(@TPARAMVALUE_R,@TPARAMVALUE_W,'Value');
  end;
end;

{$IFNDEF FPC}
procedure RIRegisterTGUIDFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TGUIDFIELD) do
  begin
  end;
end;

procedure RIRegisterTVARIANTFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TVARIANTFIELD) do
  begin
  end;
end;

procedure RIRegisterTREFERENCEFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TREFERENCEFIELD) do
  begin
  RegisterPropertyHelper(@TREFERENCEFIELDREFERENCETABLENAME_R,@TREFERENCEFIELDREFERENCETABLENAME_W,'ReferenceTableName');
  end;
end;


procedure RIRegisterTDATASETFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TDATASETFIELD) do
  begin
  RegisterPropertyHelper(@TDATASETFIELDNESTEDDATASET_R,nil,'NestedDataSet');
  RegisterPropertyHelper(@TDATASETFIELDINCLUDEOBJECTFIELD_R,@TDATASETFIELDINCLUDEOBJECTFIELD_W,'IncludeObjectField');
  end;
end;


procedure RIRegisterTARRAYFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TARRAYFIELD) do
  begin
  end;
end;


procedure RIRegisterTADTFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TADTFIELD) do
  begin
  end;
end;


procedure RIRegisterTOBJECTFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TOBJECTFIELD) do
  begin
  RegisterPropertyHelper(@TOBJECTFIELDFIELDCOUNT_R,nil,'FieldCount');
  RegisterPropertyHelper(@TOBJECTFIELDFIELDS_R,nil,'Fields');
  RegisterPropertyHelper(@TOBJECTFIELDFIELDVALUES_R,@TOBJECTFIELDFIELDVALUES_W,'FieldValues');
  RegisterPropertyHelper(@TOBJECTFIELDUNNAMED_R,nil,'UnNamed');
  RegisterPropertyHelper(@TOBJECTFIELDOBJECTTYPE_R,@TOBJECTFIELDOBJECTTYPE_W,'ObjectType');
  end;
end;
{$ENDIF}


procedure RIRegisterTGRAPHICFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TGRAPHICFIELD) do
  begin
  end;
end;

procedure RIRegisterTMEMOFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TMEMOFIELD) do
  begin
  end;
end;

procedure RIRegisterTBLOBFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TBLOBFIELD) do
  begin
  RegisterMethod(@TBLOBFIELD.LOADFROMFILE, 'LoadFromFile');
  RegisterMethod(@TBLOBFIELD.LOADFROMSTREAM, 'LoadFromStream');
  RegisterMethod(@TBLOBFIELD.SAVETOFILE, 'SaveToFile');
  RegisterMethod(@TBLOBFIELD.SAVETOSTREAM, 'SaveToStream');
  RegisterPropertyHelper(@TBLOBFIELDBLOBSIZE_R,nil,'BlobSize');
  RegisterPropertyHelper(@TBLOBFIELDMODIFIED_R,@TBLOBFIELDMODIFIED_W,'Modified');
  RegisterPropertyHelper(@TBLOBFIELDVALUE_R,@TBLOBFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TBLOBFIELDTRANSLITERATE_R,@TBLOBFIELDTRANSLITERATE_W,'Transliterate');
  RegisterPropertyHelper(@TBLOBFIELDBLOBTYPE_R,@TBLOBFIELDBLOBTYPE_W,'BlobType');
{$IFNDEF FPC}
{$IFDEF DELPHI6UP}
  RegisterPropertyHelper(@TBLOBFIELDGRAPHICHEADER_R,@TBLOBFIELDGRAPHICHEADER_W,'GraphicHeader');
{$ENDIF}
{$ENDIF}
  end;
end;


{$IFNDEF FPC}
{$IFDEF DELPHI6UP}

procedure RIRegisterTFMTBCDFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFMTBCDFIELD) do
  begin
  RegisterPropertyHelper(@TFMTBCDFIELDVALUE_R,@TFMTBCDFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TFMTBCDFIELDCURRENCY_R,@TFMTBCDFIELDCURRENCY_W,'Currency');
  RegisterPropertyHelper(@TFMTBCDFIELDMAXVALUE_R,@TFMTBCDFIELDMAXVALUE_W,'MaxValue');
  RegisterPropertyHelper(@TFMTBCDFIELDMINVALUE_R,@TFMTBCDFIELDMINVALUE_W,'MinValue');
  RegisterPropertyHelper(@TFMTBCDFIELDPRECISION_R,@TFMTBCDFIELDPRECISION_W,'Precision');
  end;
end;
{$ENDIF}
procedure RIRegisterTBCDFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TBCDFIELD) do
  begin
  RegisterPropertyHelper(@TBCDFIELDVALUE_R,@TBCDFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TBCDFIELDCURRENCY_R,@TBCDFIELDCURRENCY_W,'Currency');
  RegisterPropertyHelper(@TBCDFIELDMAXVALUE_R,@TBCDFIELDMAXVALUE_W,'MaxValue');
  RegisterPropertyHelper(@TBCDFIELDMINVALUE_R,@TBCDFIELDMINVALUE_W,'MinValue');
  RegisterPropertyHelper(@TBCDFIELDPRECISION_R,@TBCDFIELDPRECISION_W,'Precision');
  end;
end;
{$ENDIF}

procedure RIRegisterTVARBYTESFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TVARBYTESFIELD) do
  begin
  end;
end;

procedure RIRegisterTBYTESFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TBYTESFIELD) do
  begin
  end;
end;

procedure RIRegisterTBINARYFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TBINARYFIELD) do
  begin
  end;
end;

procedure RIRegisterTTIMEFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TTIMEFIELD) do
  begin
  end;
end;

procedure RIRegisterTDATEFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TDATEFIELD) do
  begin
  end;
end;

procedure RIRegisterTDATETIMEFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TDATETIMEFIELD) do
  begin
  RegisterPropertyHelper(@TDATETIMEFIELDVALUE_R,@TDATETIMEFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TDATETIMEFIELDDISPLAYFORMAT_R,@TDATETIMEFIELDDISPLAYFORMAT_W,'DisplayFormat');
  end;
end;

procedure RIRegisterTBOOLEANFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TBOOLEANFIELD) do
  begin
  RegisterPropertyHelper(@TBOOLEANFIELDVALUE_R,@TBOOLEANFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TBOOLEANFIELDDISPLAYVALUES_R,@TBOOLEANFIELDDISPLAYVALUES_W,'DisplayValues');
  end;
end;

procedure RIRegisterTCURRENCYFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TCURRENCYFIELD) do
  begin
  end;
end;

procedure RIRegisterTFLOATFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFLOATFIELD) do
  begin
  {$IFNDEF FPC}
  RegisterPropertyHelper(@TFLOATFIELDCURRENCY_R,@TFLOATFIELDCURRENCY_W,'Currency');
  {$ENDIF}
  RegisterPropertyHelper(@TFLOATFIELDVALUE_R,@TFLOATFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TFLOATFIELDMAXVALUE_R,@TFLOATFIELDMAXVALUE_W,'MaxValue');
  RegisterPropertyHelper(@TFLOATFIELDMINVALUE_R,@TFLOATFIELDMINVALUE_W,'MinValue');
  RegisterPropertyHelper(@TFLOATFIELDPRECISION_R,@TFLOATFIELDPRECISION_W,'Precision');
  end;
end;

procedure RIRegisterTAUTOINCFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TAUTOINCFIELD) do
  begin
  end;
end;

procedure RIRegisterTWORDFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TWORDFIELD) do
  begin
  end;
end;

procedure RIRegisterTLARGEINTFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TLARGEINTFIELD) do
  begin
  RegisterPropertyHelper(@TLARGEINTFIELDASLARGEINT_R,@TLARGEINTFIELDASLARGEINT_W,'AsLargeInt');
  RegisterPropertyHelper(@TLARGEINTFIELDVALUE_R,@TLARGEINTFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TLARGEINTFIELDMAXVALUE_R,@TLARGEINTFIELDMAXVALUE_W,'MaxValue');
  RegisterPropertyHelper(@TLARGEINTFIELDMINVALUE_R,@TLARGEINTFIELDMINVALUE_W,'MinValue');
  end;
end;

procedure RIRegisterTSMALLINTFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TSMALLINTFIELD) do
  begin
  end;
end;

procedure RIRegisterTINTEGERFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TINTEGERFIELD) do
  begin
  RegisterPropertyHelper(@TINTEGERFIELDVALUE_R,@TINTEGERFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TINTEGERFIELDMAXVALUE_R,@TINTEGERFIELDMAXVALUE_W,'MaxValue');
  RegisterPropertyHelper(@TINTEGERFIELDMINVALUE_R,@TINTEGERFIELDMINVALUE_W,'MinValue');
  end;
end;

procedure RIRegisterTNUMERICFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TNUMERICFIELD) do
  begin
  RegisterPropertyHelper(@TNUMERICFIELDDISPLAYFORMAT_R,@TNUMERICFIELDDISPLAYFORMAT_W,'DisplayFormat');
  RegisterPropertyHelper(@TNUMERICFIELDEDITFORMAT_R,@TNUMERICFIELDEDITFORMAT_W,'EditFormat');
  end;
end;

{$IFNDEF FPC}
procedure RIRegisterTWIDESTRINGFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TWIDESTRINGFIELD) do
  begin
  RegisterPropertyHelper(@TWIDESTRINGFIELDVALUE_R,@TWIDESTRINGFIELDVALUE_W,'Value');
  end;
end;
{$ENDIF}

procedure RIRegisterTSTRINGFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TSTRINGFIELD) do
  begin
  RegisterPropertyHelper(@TSTRINGFIELDVALUE_R,@TSTRINGFIELDVALUE_W,'Value');
  {$IFNDEF FPC}
  RegisterPropertyHelper(@TSTRINGFIELDFIXEDCHAR_R,@TSTRINGFIELDFIXEDCHAR_W,'FixedChar');
  RegisterPropertyHelper(@TSTRINGFIELDTRANSLITERATE_R,@TSTRINGFIELDTRANSLITERATE_W,'Transliterate');
  {$ENDIF}
  end;
end;

procedure RIRegisterTFIELD(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFIELD) do
  begin
  RegisterMethod(@TFIELD.ASSIGNVALUE, 'AssignValue');
  RegisterVirtualMethod(@TFIELD.CLEAR, 'Clear');
  RegisterMethod(@TFIELD.FOCUSCONTROL, 'FocusControl');
//  RegisterMethod(@TFIELD.GETDATA, 'GetData');
  RegisterVirtualMethod(@TFIELD.ISVALIDCHAR, 'IsValidChar');
  RegisterMethod(@TFIELD.REFRESHLOOKUPLIST, 'RefreshLookupList');
//  RegisterMethod(@TFIELD.SETDATA, 'SetData');
  RegisterVirtualMethod(@TFIELD.SETFIELDTYPE, 'SetFieldType');
//  RegisterMethod(@TFIELD.VALIDATE, 'Validate');
{$IFNDEF FPC}

  RegisterPropertyHelper(@TFIELDEDITMASK_R,@TFIELDEDITMASK_W,'EditMask');
  RegisterPropertyHelper(@TFIELDEDITMASKPTR_R,nil,'EditMaskPtr');
  RegisterPropertyHelper(@TFIELDEDITMASK_R,@TFIELDEDITMASK_W,'EditMask');
  RegisterPropertyHelper(@TFIELDEDITMASKPTR_R,nil,'EditMaskPtr');
  RegisterPropertyHelper(@TFIELDFULLNAME_R,nil,'FullName');
  RegisterPropertyHelper(@TFIELDLOOKUP_R,@TFIELDLOOKUP_W,'Lookup');
  RegisterPropertyHelper(@TFIELDPARENTFIELD_R,@TFIELDPARENTFIELD_W,'ParentField');
  RegisterPropertyHelper(@TFIELDVALIDCHARS_R,@TFIELDVALIDCHARS_W,'ValidChars');
  RegisterPropertyHelper(@TFIELDAUTOGENERATEVALUE_R,@TFIELDAUTOGENERATEVALUE_W,'AutoGenerateValue');

{$IFDEF DELPHI6UP}
  RegisterPropertyHelper(@TFIELDASBCD_R,@TFIELDASBCD_W,'AsBCD');
{$ENDIF}
{$ENDIF}
  RegisterPropertyHelper(@TFIELDASBOOLEAN_R,@TFIELDASBOOLEAN_W,'AsBoolean');
  RegisterPropertyHelper(@TFIELDASCURRENCY_R,@TFIELDASCURRENCY_W,'AsCurrency');
  RegisterPropertyHelper(@TFIELDASDATETIME_R,@TFIELDASDATETIME_W,'AsDateTime');
  RegisterPropertyHelper(@TFIELDASFLOAT_R,@TFIELDASFLOAT_W,'AsFloat');
  RegisterPropertyHelper(@TFIELDASINTEGER_R,@TFIELDASINTEGER_W,'AsInteger');
  RegisterPropertyHelper(@TFIELDASSTRING_R,@TFIELDASSTRING_W,'AsString');
  RegisterPropertyHelper(@TFIELDASVARIANT_R,@TFIELDASVARIANT_W,'AsVariant');
  RegisterPropertyHelper(@TFIELDATTRIBUTESET_R,@TFIELDATTRIBUTESET_W,'AttributeSet');
  RegisterPropertyHelper(@TFIELDCALCULATED_R,@TFIELDCALCULATED_W,'Calculated');
  RegisterPropertyHelper(@TFIELDCANMODIFY_R,nil,'CanModify');
  RegisterPropertyHelper(@TFIELDCURVALUE_R,nil,'CurValue');
  RegisterPropertyHelper(@TFIELDDATASET_R,@TFIELDDATASET_W,'Dataset');
  RegisterPropertyHelper(@TFIELDDATASIZE_R,nil,'DataSize');
  RegisterPropertyHelper(@TFIELDDATATYPE_R,nil,'DataType');
  RegisterPropertyHelper(@TFIELDDISPLAYNAME_R,nil,'DisplayName');
  RegisterPropertyHelper(@TFIELDDISPLAYTEXT_R,nil,'DisplayText');
  RegisterPropertyHelper(@TFIELDFIELDNO_R,nil,'FieldNo');
  RegisterPropertyHelper(@TFIELDISINDEXFIELD_R,nil,'IsIndexField');
  RegisterPropertyHelper(@TFIELDISNULL_R,nil,'IsNull');
  RegisterPropertyHelper(@TFIELDLOOKUPLIST_R,nil,'LookupList');
  RegisterPropertyHelper(@TFIELDNEWVALUE_R,@TFIELDNEWVALUE_W,'NewValue');
  RegisterPropertyHelper(@TFIELDOFFSET_R,nil,'Offset');
  RegisterPropertyHelper(@TFIELDOLDVALUE_R,nil,'OldValue');
  RegisterPropertyHelper(@TFIELDSIZE_R,@TFIELDSIZE_W,'Size');
  RegisterPropertyHelper(@TFIELDTEXT_R,@TFIELDTEXT_W,'Text');
  RegisterPropertyHelper(@TFIELDVALUE_R,@TFIELDVALUE_W,'Value');
  RegisterPropertyHelper(@TFIELDALIGNMENT_R,@TFIELDALIGNMENT_W,'Alignment');
  RegisterPropertyHelper(@TFIELDCUSTOMCONSTRAINT_R,@TFIELDCUSTOMCONSTRAINT_W,'CustomConstraint');
  RegisterPropertyHelper(@TFIELDCONSTRAINTERRORMESSAGE_R,@TFIELDCONSTRAINTERRORMESSAGE_W,'ConstraintErrorMessage');
  RegisterPropertyHelper(@TFIELDDEFAULTEXPRESSION_R,@TFIELDDEFAULTEXPRESSION_W,'DefaultExpression');
  RegisterPropertyHelper(@TFIELDDISPLAYLABEL_R,@TFIELDDISPLAYLABEL_W,'DisplayLabel');
  RegisterPropertyHelper(@TFIELDDISPLAYWIDTH_R,@TFIELDDISPLAYWIDTH_W,'DisplayWidth');
  RegisterPropertyHelper(@TFIELDFIELDKIND_R,@TFIELDFIELDKIND_W,'FieldKind');
  RegisterPropertyHelper(@TFIELDFIELDNAME_R,@TFIELDFIELDNAME_W,'FieldName');
  RegisterPropertyHelper(@TFIELDHASCONSTRAINTS_R,nil,'HasConstraints');
  RegisterPropertyHelper(@TFIELDINDEX_R,@TFIELDINDEX_W,'Index');
  RegisterPropertyHelper(@TFIELDIMPORTEDCONSTRAINT_R,@TFIELDIMPORTEDCONSTRAINT_W,'ImportedConstraint');
  RegisterPropertyHelper(@TFIELDLOOKUPDATASET_R,@TFIELDLOOKUPDATASET_W,'LookupDataSet');
  RegisterPropertyHelper(@TFIELDLOOKUPKEYFIELDS_R,@TFIELDLOOKUPKEYFIELDS_W,'LookupKeyFields');
  RegisterPropertyHelper(@TFIELDLOOKUPRESULTFIELD_R,@TFIELDLOOKUPRESULTFIELD_W,'LookupResultField');
  RegisterPropertyHelper(@TFIELDKEYFIELDS_R,@TFIELDKEYFIELDS_W,'KeyFields');
  RegisterPropertyHelper(@TFIELDLOOKUPCACHE_R,@TFIELDLOOKUPCACHE_W,'LookupCache');
  RegisterPropertyHelper(@TFIELDORIGIN_R,@TFIELDORIGIN_W,'Origin');
  RegisterPropertyHelper(@TFIELDPROVIDERFLAGS_R,@TFIELDPROVIDERFLAGS_W,'ProviderFlags');
  RegisterPropertyHelper(@TFIELDREADONLY_R,@TFIELDREADONLY_W,'ReadOnly');
  RegisterPropertyHelper(@TFIELDREQUIRED_R,@TFIELDREQUIRED_W,'Required');
  RegisterPropertyHelper(@TFIELDVISIBLE_R,@TFIELDVISIBLE_W,'Visible');
  RegisterEventPropertyHelper(@TFIELDONCHANGE_R,@TFIELDONCHANGE_W,'OnChange');
  RegisterEventPropertyHelper(@TFIELDONGETTEXT_R,@TFIELDONGETTEXT_W,'OnGetText');
  RegisterEventPropertyHelper(@TFIELDONSETTEXT_R,@TFIELDONSETTEXT_W,'OnSetText');
  RegisterEventPropertyHelper(@TFIELDONVALIDATE_R,@TFIELDONVALIDATE_W,'OnValidate');
  end;
end;

procedure RIRegisterTLOOKUPLIST(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TLOOKUPLIST) do
  begin
  RegisterConstructor(@TLOOKUPLIST.CREATE, 'Create');
  {$IFDEF DELPHI2009UP}
  RegisterVirtualAbstractMethod(TDefaultLookupList, @TDefaultLookupList.ADD, 'Add');
  RegisterVirtualAbstractMethod(TDefaultLookupList, @TDefaultLookupList.CLEAR, 'Clear');
  RegisterVirtualAbstractMethod(TDefaultLookupList, @TDefaultLookupList.VALUEOFKEY, 'ValueOfKey');
  {$ELSE}
  RegisterMethod(@TLOOKUPLIST.ADD, 'Add');
  RegisterMethod(@TLOOKUPLIST.CLEAR, 'Clear');
  RegisterMethod(@TLOOKUPLIST.VALUEOFKEY, 'ValueOfKey');
  {$ENDIF}
  end;
end;

procedure RIRegisterTFIELDS(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFIELDS) do
  begin
  RegisterConstructor(@TFIELDS.CREATE, 'Create');
  RegisterMethod(@TFIELDS.ADD, 'Add');
  RegisterMethod(@TFIELDS.CHECKFIELDNAME, 'CheckFieldName');
  RegisterMethod(@TFIELDS.CHECKFIELDNAMES, 'CheckFieldNames');
  RegisterMethod(@TFIELDS.CLEAR, 'Clear');
  RegisterMethod(@TFIELDS.FINDFIELD, 'FindField');
  RegisterMethod(@TFIELDS.FIELDBYNAME, 'FieldByName');
  RegisterMethod(@TFIELDS.FIELDBYNUMBER, 'FieldByNumber');
  RegisterMethod(@TFIELDS.GETFIELDNAMES, 'GetFieldNames');
  RegisterMethod(@TFIELDS.INDEXOF, 'IndexOf');
  RegisterMethod(@TFIELDS.REMOVE, 'Remove');
  RegisterPropertyHelper(@TFIELDSCOUNT_R,nil,'Count');
  RegisterPropertyHelper(@TFIELDSDATASET_R,nil,'Dataset');
  RegisterPropertyHelper(@TFIELDSFIELDS_R,@TFIELDSFIELDS_W,'Fields');
  end;
end;

{$IFNDEF FPC}
procedure RIRegisterTFIELDLIST(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFIELDLIST) do
  begin
  RegisterMethod(@TFIELDLIST.FIELDBYNAME, 'FieldByName');
  RegisterMethod(@TFIELDLIST.FIND, 'Find');
  RegisterPropertyHelper(@TFIELDLISTFIELDS_R,nil,'Fields');
  end;
end;

procedure RIRegisterTFIELDDEFLIST(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFIELDDEFLIST) do
  begin
  RegisterMethod(@TFIELDDEFLIST.FIELDBYNAME, 'FieldByName');
  RegisterMethod(@TFIELDDEFLIST.FIND, 'Find');
  RegisterPropertyHelper(@TFIELDDEFLISTFIELDDEFS_R,nil,'FieldDefs');
  end;
end;


procedure RIRegisterTFLATLIST(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFLATLIST) do
  begin
  RegisterConstructor(@TFLATLIST.CREATE, 'Create');
  RegisterMethod(@TFLATLIST.UPDATE, 'Update');
  RegisterPropertyHelper(@TFLATLISTDATASET_R,nil,'Dataset');
  end;
end;
{$ENDIF}


procedure RIRegisterTINDEXDEFS(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TINDEXDEFS) do
  begin
  RegisterConstructor(@TINDEXDEFS.CREATE, 'Create');
  RegisterMethod(@TINDEXDEFS.ADDINDEXDEF, 'AddIndexDef');
  RegisterMethod(@TINDEXDEFS.FIND, 'Find');
  RegisterMethod(@TINDEXDEFS.UPDATE, 'Update');
  RegisterMethod(@TINDEXDEFS.FINDINDEXFORFIELDS, 'FindIndexForFields');
  RegisterMethod(@TINDEXDEFS.GETINDEXFORFIELDS, 'GetIndexForFields');
  RegisterMethod(@TINDEXDEFS.ADD, 'Add');
  RegisterPropertyHelper(@TINDEXDEFSITEMS_R,@TINDEXDEFSITEMS_W,'Items');
  end;
end;

procedure RIRegisterTINDEXDEF(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TINDEXDEF) do
  begin
  RegisterConstructor(@TINDEXDEF.CREATE, 'Create');
{$IFNDEF FPC}
  RegisterPropertyHelper(@TINDEXDEFFIELDEXPRESSION_R,nil,'FieldExpression');
  RegisterPropertyHelper(@TINDEXDEFCASEINSFIELDS_R,@TINDEXDEFCASEINSFIELDS_W,'CaseInsFields');
  RegisterPropertyHelper(@TINDEXDEFGROUPINGLEVEL_R,@TINDEXDEFGROUPINGLEVEL_W,'GroupingLevel');
  RegisterPropertyHelper(@TINDEXDEFDESCFIELDS_R,@TINDEXDEFDESCFIELDS_W,'DescFields');

{$ENDIF}
  RegisterPropertyHelper(@TINDEXDEFEXPRESSION_R,@TINDEXDEFEXPRESSION_W,'Expression');
  RegisterPropertyHelper(@TINDEXDEFFIELDS_R,@TINDEXDEFFIELDS_W,'Fields');
  RegisterPropertyHelper(@TINDEXDEFOPTIONS_R,@TINDEXDEFOPTIONS_W,'Options');
  RegisterPropertyHelper(@TINDEXDEFSOURCE_R,@TINDEXDEFSOURCE_W,'Source');
  end;
end;

procedure RIRegisterTFIELDDEFS(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFIELDDEFS) do
  begin
  RegisterConstructor(@TFIELDDEFS.CREATE, 'Create');
  RegisterMethod(@TFIELDDEFS.ADDFIELDDEF, 'AddFieldDef');
  RegisterMethod(@TFIELDDEFS.FIND, 'Find');
  RegisterMethod(@TFIELDDEFS.UPDATE, 'Update');
{$IFNDEF FPC}
  RegisterMethod(@TFIELDDEFS.ADD, 'Add');
  RegisterPropertyHelper(@TFIELDDEFSPARENTDEF_R,nil,'ParentDef');

{$ENDIF}
  RegisterPropertyHelper(@TFIELDDEFSHIDDENFIELDS_R,@TFIELDDEFSHIDDENFIELDS_W,'HiddenFields');
  RegisterPropertyHelper(@TFIELDDEFSITEMS_R,@TFIELDDEFSITEMS_W,'Items');
  end;
end;

procedure RIRegisterTFIELDDEF(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TFIELDDEF) do
  begin
//  RegisterConstructor(@TFIELDDEF.CREATE, 'Create');
{$IFNDEF FPC}
  RegisterMethod(@TFIELDDEF.ADDCHILD, 'AddChild');
  RegisterMethod(@TFIELDDEF.HASCHILDDEFS, 'HasChildDefs');

{$ENDIF}
  RegisterMethod(@TFIELDDEF.CREATEFIELD, 'CreateField');
{$IFNDEF FPC}
  RegisterPropertyHelper(@TFIELDDEFFIELDNO_R,@TFIELDDEFFIELDNO_W,'FieldNo');
  RegisterPropertyHelper(@TFIELDDEFPARENTDEF_R,nil,'ParentDef');
  RegisterPropertyHelper(@TFIELDDEFCHILDDEFS_R,@TFIELDDEFCHILDDEFS_W,'ChildDefs');
  RegisterPropertyHelper(@TFIELDDEFREQUIRED_R,@TFIELDDEFREQUIRED_W,'Required');

{$ENDIF}
  RegisterPropertyHelper(@TFIELDDEFFIELDCLASS_R,nil,'FieldClass');
  RegisterPropertyHelper(@TFIELDDEFINTERNALCALCFIELD_R,@TFIELDDEFINTERNALCALCFIELD_W,'InternalCalcField');
  RegisterPropertyHelper(@TFIELDDEFATTRIBUTES_R,@TFIELDDEFATTRIBUTES_W,'Attributes');
  RegisterPropertyHelper(@TFIELDDEFDATATYPE_R,@TFIELDDEFDATATYPE_W,'DataType');
  RegisterPropertyHelper(@TFIELDDEFPRECISION_R,@TFIELDDEFPRECISION_W,'Precision');
  RegisterPropertyHelper(@TFIELDDEFSIZE_R,@TFIELDDEFSIZE_W,'Size');
  end;
end;

{$IFNDEF FPC}
procedure RIRegisterTDEFCOLLECTION(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TDEFCOLLECTION) do
  begin
  RegisterConstructor(@TDEFCOLLECTION.CREATE, 'Create');
  RegisterMethod(@TDEFCOLLECTION.FIND, 'Find');
  RegisterMethod(@TDEFCOLLECTION.GETITEMNAMES, 'GetItemNames');
  RegisterMethod(@TDEFCOLLECTION.INDEXOF, 'IndexOf');
  RegisterPropertyHelper(@TDEFCOLLECTIONDATASET_R,nil,'Dataset');
  RegisterPropertyHelper(@TDEFCOLLECTIONUPDATED_R,@TDEFCOLLECTIONUPDATED_W,'Updated');
  end;
end;

procedure RIRegisterTNAMEDITEM(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TNAMEDITEM) do
  begin
  RegisterPropertyHelper(@TNAMEDITEMNAME_R,@TNAMEDITEMNAME_W,'Name');
  end;
end;
{$ENDIF}


procedure RIRegister_DB(CL: TPSRuntimeClassImporter);
Begin
RIRegisterTFIELDDEF(Cl);
RIRegisterTFIELDDEFS(Cl);
RIRegisterTINDEXDEF(Cl);
RIRegisterTINDEXDEFS(Cl);
RIRegisterTFIELDS(Cl);
RIRegisterTLOOKUPLIST(Cl);
RIRegisterTFIELD(Cl);
RIRegisterTSTRINGFIELD(Cl);
RIRegisterTNUMERICFIELD(Cl);
RIRegisterTINTEGERFIELD(Cl);
RIRegisterTSMALLINTFIELD(Cl);
RIRegisterTLARGEINTFIELD(Cl);
RIRegisterTWORDFIELD(Cl);
RIRegisterTAUTOINCFIELD(Cl);
RIRegisterTFLOATFIELD(Cl);
RIRegisterTCURRENCYFIELD(Cl);
RIRegisterTBOOLEANFIELD(Cl);
RIRegisterTDATETIMEFIELD(Cl);
RIRegisterTDATEFIELD(Cl);
RIRegisterTTIMEFIELD(Cl);
RIRegisterTBINARYFIELD(Cl);
RIRegisterTBYTESFIELD(Cl);
RIRegisterTVARBYTESFIELD(Cl);
{$IFNDEF FPC}
RIRegisterTNAMEDITEM(Cl);
RIRegisterTDEFCOLLECTION(Cl);
RIRegisterTWIDESTRINGFIELD(Cl);
RIRegisterTFLATLIST(Cl);
RIRegisterTFIELDDEFLIST(Cl);
RIRegisterTFIELDLIST(Cl);
RIRegisterTBCDFIELD(Cl);
{$IFDEF DELPHI6UP}
RIRegisterTFMTBCDFIELD(Cl);
{$ENDIF}
{$ENDIF}

RIRegisterTBLOBFIELD(Cl);
RIRegisterTMEMOFIELD(Cl);
RIRegisterTGRAPHICFIELD(Cl);
{$IFNDEF FPC}
RIRegisterTOBJECTFIELD(Cl);
RIRegisterTADTFIELD(Cl);
RIRegisterTARRAYFIELD(Cl);
RIRegisterTDATASETFIELD(Cl);
RIRegisterTREFERENCEFIELD(Cl);
RIRegisterTVARIANTFIELD(Cl);
RIRegisterTGUIDFIELD(Cl);
{$ENDIF}
RIRegisterTPARAM(Cl);
RIRegisterTPARAMS(Cl);
RIRegisterTDATASET(Cl);
end;

{$IFDEF USEIMPORTER}
initialization
RIImporter.Invoke(RIRegister_DB);
{$ENDIF}
end.

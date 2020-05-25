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
  Rev 1.1    2/8/2004 1:35:40 PM  JPMugaas
  IdSocks is now in DotNET.

  Rev 1.0    2/3/2004 12:28:06 PM  JPMugaas
  Kudzu wanted this renamed.

  Rev 1.27    2004.01.01 2:40:02 PM  czhower
  Removed test ifdef

  Rev 1.26    1/1/2004 3:32:30 PM  BGooijen
  Added icons for .Net

  Rev 1.25    2003.12.31 11:02:50 PM  czhower
  New components now registered for .net.

  Rev 1.24    2003.12.25 6:55:20 PM  czhower
  TCPServer

  Rev 1.23    11/22/2003 11:49:52 PM  BGooijen
  Icons for DotNet

  Rev 1.22    17/11/2003 16:00:22  ANeillans
  Fix Delphi compile errors.

  Rev 1.21    11/8/2003 8:09:24 PM  BGooijen
  fix, i mixed up some stuff

  Rev 1.20    11/8/2003 7:27:10 PM  BGooijen
  DotNet

  Rev 1.19    2003.10.19 1:35:32 PM  czhower
  Moved Borland define to .inc

  Rev 1.18    2003.10.18 11:32:42 PM  czhower
  Changed throttler to intercept

  Rev 1.17    2003.10.17 6:18:50 PM  czhower
  TIdInterceptSimLog

  Rev 1.16    2003.10.14 1:26:42 PM  czhower
  Uupdates + Intercept support

  Rev 1.15    9/21/2003 01:10:40 AM  JPMugaas
  Added IdThreadCOmponent to the registration in Core.

  Rev 1.14    2003.08.19 11:06:34 PM  czhower
  Fixed names of scheduler units.

  Rev 1.13    8/19/2003 01:25:08 AM  JPMugaas
  Unnecessary junk removed.

  Rev 1.12    8/15/2003 12:02:48 AM  JPMugaas
  Incremented version number.
  Moved some units to new IndySuperCore package in D7.
  Made sure package titles are uniform in the IDE and in the .RES files.

  Rev 1.11    7/24/2003 03:22:00 AM  JPMugaas
  Removed some old files.

  Rev 1.10    7/18/2003 4:33:12 PM  SPerry
  Added TIdCmdTCPClient

  Rev 1.7    4/17/2003 05:02:26 PM  JPMugaas

  Rev 1.6    4/11/2003 01:09:50 PM  JPMugaas

  Rev 1.5    3/25/2003 11:12:54 PM  BGooijen
  TIdChainEngineStack added.

  Rev 1.4    3/25/2003 05:02:00 PM  JPMugaas
  TCmdTCPServer added.

  Rev 1.3    3/22/2003 10:14:54 PM  BGooijen
  Added TIdServerIOHandlerChain to the palette

  Rev 1.2    3/22/2003 02:20:48 PM  JPMugaas
  Updated registration.

  Rev 1.1    1/17/2003 04:18:44 PM  JPMugaas
  Now compiles with new packages.

  Rev 1.0    11/13/2002 08:41:42 AM  JPMugaas
}

unit IdRegisterCore;

interface

// Procedures

  procedure Register;

implementation

{$I IdCompilerDefines.inc}

uses
  Classes,
  {$IFDEF FMX}
  Controls,
  {$ENDIF}
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  IdSocks,
  {$IFDEF HAS_TSelectionEditor}
    {$IFDEF FPC}
  PropEdits,
  ComponentEditors,
    {$ELSE}
  DesignIntf,
  DesignEditors,
    {$ENDIF}
  TypInfo,
    {$IFDEF VCL_2010_OR_ABOVE}
  Rtti,
    {$ENDIF}
  SysUtils,
  IdGlobal,
  {$ENDIF}

  IdBaseComponent,
  IdComponent,
  IdDsnCoreResourceStrings,
  IdAntiFreeze,
  IdCmdTCPClient,
  IdCmdTCPServer,
  IdIOHandlerStream,
  {$IFNDEF DOTNET}
  IdIcmpClient,
  {$ENDIF}
  IdInterceptSimLog,
  IdInterceptThrottler,
  IdIPMCastClient,
  IdIPMCastServer,
  IdLogDebug,
  IdLogEvent,
  IdLogFile,
  IdLogStream,
  IdSchedulerOfThread,
  IdSchedulerOfThreadDefault,
  IdSchedulerOfThreadPool,
  IdServerIOHandlerSocket,
  IdServerIOHandlerStack,
  IdSimpleServer,
  IdThreadComponent,
  {$IFNDEF DOTNET}
  IdTraceRoute,
  {$ENDIF}
  IdUDPClient,
  IdUDPServer,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdIntercept,
  IdTCPServer,
  IdTCPClient;

{$IFDEF DOTNET}
  {$R IconsDotNet\TIdAntiFreeze.bmp}
  {$R IconsDotNet\TIdCmdTCPClient.bmp}
  {$R IconsDotNet\TIdCmdTCPServer.bmp}
  {$R IconsDotNet\TIdConnectionIntercept.bmp}
  {$R IconsDotNet\TIdICMPClient.bmp}
  {$R IconsDotNet\TIdInterceptSimLog.bmp}
  {$R IconsDotNet\TIdInterceptThrottler.bmp}
  {$R IconsDotNet\TIdIOHandlerStack.bmp}
  {$R IconsDotNet\TIdIOHandlerStream.bmp}
  {$R IconsDotNet\TIdLogDebug.bmp}
  {$R IconsDotNet\TIdLogEvent.bmp}
  {$R IconsDotNet\TIdLogFile.bmp}
  {$R IconsDotNet\TIdLogStream.bmp}
  {$R IconsDotNet\TIdSchedulerOfThreadDefault.bmp}
  {$R IconsDotNet\TIdSchedulerOfThreadPool.bmp}
  {$R IconsDotNet\TIdServerIOHandlerStack.bmp}
  {$R IconsDotNet\TIdSimpleServer.bmp}
  {$R IconsDotNet\TIdTCPClient.bmp}
  {$R IconsDotNet\TIdTCPServer.bmp}
  {$R IconsDotNet\TIdThreadComponent.bmp}
  {$R IconsDotNet\TIdUDPClient.bmp}
  {$R IconsDotNet\TIdUDPServer.bmp}
  {$R IconsDotNet\TIdIPMCastClient.bmp}
  {$R IconsDotNet\TIdIPMCastServer.bmp}
  {$R IconsDotNet\TIdSocksInfo.bmp}
{$ELSE}
  {$IFNDEF FPC}
    {$IFDEF BORLAND}
      {$R IdCoreRegister.dcr}
    {$ELSE}
      {$R IdCoreRegisterCool.dcr}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF HAS_TSelectionEditor}

// TIdComponentSelectionEditor is called at design-time when saving/compiling a
// project.  It enumerates the data types of all parameters and return values of
// every event handler assigned to any Indy component, extracting the unit names
// of those data types and passing them to the IDE so it can insert them into
// 'uses' clauses as needed.

procedure SendUnitNameToProc(const AUnitName: String; Proc: TGetStrProc);
begin
  // Do not return the 'System' unit, otherwise it will
  // cause an "Identifier redeclared" compiler error!
  if (AUnitName <> '') and (not TextIsSame(AUnitName, 'System')) then begin {do not localize}
    Proc(AUnitName);
  end;
end;

{$IFDEF VCL_XE2_OR_ABOVE}

// in Delphi XE2 and later, TRttiInvokableType is used to enumerate parameters
// and return values, and TRttiType reports fully qualified type names, so
// finding a given type's unit name is very easy...

function GetUnitNameForType(const AType: TRttiType): String;
begin
  // TRttiParameter.ParamType may be nil if it's an untyped var or const parameter...
  // TRttiMethodType.ReturnType may be nil if it's a procedure...
  if AType <> nil then begin
    // TRttiType.UnitName returns the unit that declares TRttiType itself
    // (System.Rtti), so parse the TRttiType.QualifiedName value instead...
    Result := AType.QualifiedName;
    SetLength(Result, Length(Result) - Length(AType.Name) - 1);
  end else begin
    Result := '';
  end;
end;

{$ELSE}

// in Delphi prior to XE2, as well as in FreePascal, TRttiInvokableType is not
// available, so we have to use TypInfo RTTI to enumerating parameters and
// return values, but only certain versions implement rich enough RTTI to allow
// that. Let's try to pull out what we can...

{$IFDEF FPC_2_6_0_OR_ABOVE}
  {$DEFINE HAS_tkEnumeration_UnitName}
  {$DEFINE HAS_tkMethod_ParamTypeInfo}
{$ELSE}
  {$IFDEF VCL_6_OR_ABOVE}
    {$DEFINE HAS_tkEnumeration_UnitName}
  {$ENDIF}
  {$IFDEF VCL_2010_OR_ABOVE}
    {$DEFINE HAS_tkMethod_ParamTypeInfo}
  {$ENDIF}
{$ENDIF}

procedure SkipShortString(var P: PByte);
begin
  Inc(P, 1 + Integer(P^));
end;

function ReadShortString(var P: PByte): String;
begin
  {$IFDEF VCL_2009_OR_ABOVE}
  Result := UTF8ToString(PShortString(P)^);
  {$ELSE}
  Result := PShortString(P)^;
  {$ENDIF}
  SkipShortString(P);
end;

{$IFDEF FPC_2_6_0_OR_ABOVE}
function NextShortString(PS: PShortString): PShortString;
begin
  Result := PShortString(Pointer(PS)+PByte(PS)^+1);
end;
{$ENDIF}

function GetUnitNameFromTypeName(const ATypeName: String): String;
var
  K: Integer;
begin
  // check if the type is qualified
  K := LastDelimiter('.', ATypeName);
  if K <> 0 then begin
    Result := Copy(ATypeName, 1, K-1);
  end else begin
    // TODO: enumerate package units and find the typename...
    Result := '';
  end;
end;

function GetUnitNameFromTypeInfo(const ATypeInfo: PPTypeInfo): String;
var
  LTypeData: PTypeData;
  {$IFDEF HAS_tkEnumeration_UnitName}
    {$IFDEF FPC}
  PS, PSLast: PShortString;
    {$ELSE}
  LBaseTypeData: PTypeData;
  Value: Integer;
  P: PByte;
    {$ENDIF}
  {$ENDIF}
begin
  Result := '';
  if ATypeInfo = nil then begin
    Exit;
  end;
  if ATypeInfo^ = nil then begin
    Exit;
  end;
  LTypeData := GetTypeData(ATypeInfo^);
  case ATypeInfo^.Kind of
    {$IFDEF HAS_tkEnumeration_UnitName}
    tkEnumeration: begin
      {$IFDEF FPC}
      // the unit name is the last string in the name list
      PS := @(LTypeData^.NameList);
      PSLast := nil;
      while PByte(PS)^ <> 0 do begin
        PSLast := PS;
        PS := NextShortString(PS);
      end;
      if PSLast <> nil then begin
        Result := PSLast^;
      end;
      {$ELSE}
      // the unit name follows after the name list
      LBaseTypeData := GetTypeData(LTypeData^.BaseType^);
      P := PByte(@(LBaseTypeData^.NameList));
      // LongBool/WordBool/ByteBool have MinValue < 0 and arbitrary
      // content in Value; Boolean has Value in [0, 1] }
      if (ATypeInfo^ = System.TypeInfo(Boolean)) or (LBaseTypeData^.MinValue < 0) then
      begin
        for Value := 0 to 1 do begin
          SkipShortString(P);
        end;
      end else
      begin
        for Value := LBaseTypeData^.MinValue to LBaseTypeData^.MaxValue do begin
          SkipShortString(P);
        end;
      end;
      Result := ReadShortString(P);
      {$ENDIF}
    end;
    {$ENDIF}
    tkSet: begin
      Result := GetUnitNameFromTypeInfo(LTypeData^.CompType);
    end;
    {$IFDEF VCL_5_OR_ABOVE}
    tkClass: begin
        {$IFDEF VCL_2009_OR_ABOVE}
      Result := UTF8ToString(LTypeData^.UnitName);
        {$ELSE}
      Result := LTypeData^.UnitName;
        {$ENDIF}
    end;
    {$ENDIF}
    {$IFDEF FPC_2_6_0_OR_ABOVE}
    tkHelper: begin
      Result := LTypeData^.HelperUnit;
    end;
    {$ENDIF}
    {$IFDEF VCL_5_OR_ABOVE}
    tkInterface: begin
        {$IFDEF VCL_2009_OR_ABOVE}
      Result := UTF8ToString(LTypeData^.IntfUnit);
        {$ELSE}
      Result := LTypeData^.IntfUnit;
        {$ENDIF}
    end;
    {$ENDIF}
    {$IFDEF FPC_2_2_2_OR_ABOVE} // TODO: when was tkInterfaceRaw added?
    tkInterfaceRaw: begin
      Result := LTypeData^.RawIntfUnit;
    end;
    {$ENDIF}
    {$IFDEF VCL_6_OR_ABOVE}
    tkDynArray: begin
        {$IFDEF VCL_2009_OR_ABOVE}
      Result := UTF8ToString(LTypeData^.DynUnitName);
        {$ELSE}
      Result := LTypeData^.DynUnitName;
        {$ENDIF}
      if Result = '' then begin
        Result := GetUnitNameFromTypeInfo(LTypeData^.elType2);
      end;
    end;
    {$ENDIF}
  end;
end;

procedure GetUnitNamesForMethodType(const ATypeInfo: PTypeInfo; Proc: TGetStrProc);
type
  PPPTypeInfo = ^PPTypeInfo;
var
  LTypeData: PTypeData;
  LTypeDataPtr: PByte;
  K: Integer;
  UnitName: string;
begin
  if ATypeInfo = nil then begin
    Exit;
  end;
  LTypeData := GetTypeData(ATypeInfo);
  LTypeDataPtr := PByte(@(LTypeData^.ParamList));

  if LTypeData^.ParamCount > 0 then
  begin
    for K := 0 to LTypeData^.ParamCount-1 do
    begin
      Inc(LTypeDataPtr, SizeOf(TParamFlags));
      SkipShortString(LTypeDataPtr);
      {$IFDEF HAS_tkMethod_ParamTypeInfo}
      // handled further below...
      SkipShortString(LTypeDataPtr);
      {$ELSE}
      UnitName := GetUnitNameFromTypeName(ReadShortString(LTypeDataPtr));
      SendUnitNameToProc(UnitName, Proc);
      {$ENDIF}
    end;
  end;

  if LTypeData^.MethodKind = mkFunction then
  begin
    {$IFDEF HAS_tkMethod_ParamTypeInfo}
    SkipShortString(LTypeDataPtr);
    UnitName := GetUnitNameFromTypeInfo(PPPTypeInfo(LTypeDataPtr)^);
    Inc(LTypeDataPtr, SizeOf(PPTypeInfo));
    {$ELSE}
    UnitName := GetUnitNameFromTypeName(ReadShortString(LTypeDataPtr));
    {$ENDIF}
    SendUnitNameToProc(UnitName, Proc);
  end;

  {$IFDEF HAS_tkMethod_ParamTypeInfo}
  if LTypeData^.ParamCount > 0 then
  begin
    Inc(LTypeDataPtr, SizeOf(TCallConv));
    for K := 0 to LTypeData^.ParamCount-1 do
    begin
      UnitName := GetUnitNameFromTypeInfo(PPPTypeInfo(LTypeDataPtr)^);
      SendUnitNameToProc(UnitName, Proc);
      Inc(LTypeDataPtr, SizeOf(PPTypeInfo));
    end;
  end;
  {$ENDIF}
end;

{$ENDIF}

type
  TIdBaseComponentSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TIdBaseComponentSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  Comp: TIdBaseComponent;
  I: Integer;
  {$IFDEF VCL_2010_OR_ABOVE}
  Ctx: TRttiContext;
  RetreivedCtx: Boolean;
  PropInfo: TRttiProperty;
  PropValue: TValue;
    {$IFDEF VCL_XE2_OR_ABOVE}
  PropType: TRttiMethodType;
  Param: TRttiParameter;
    {$ENDIF}
  {$ELSE}
  PropList: PPropList;
  PropCount: Integer;
  PropInfo: PPropInfo;
  J: Integer;
  {$ENDIF}
begin
  inherited RequiresUnits(Proc);
  if (Designer = nil) or (Designer.Root = nil) then Exit;

  {$IFDEF VCL_2010_OR_ABOVE}
  RetreivedCtx := False;
  {$ENDIF}

  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if Designer.Root.Components[i] is TIdBaseComponent then
    begin
      Comp := TIdBaseComponent(Designer.Root.Components[i]);

      {$IFDEF VCL_2010_OR_ABOVE}

      if not RetreivedCtx then
      begin
        Ctx := TRttiContext.Create;
        RetreivedCtx := True;
      end;

      for PropInfo in Ctx.GetType(Comp.ClassType).GetProperties do
      begin
        // only interested in *assigned* event handlers

        // NOTE: Delphi 2010 has a problem with checking the TValue.IsEmpty
        // property inlined like below. It causes a "F2084 Internal Error C13394"
        // compiler error. So splitting up the comparison to use a local TValue
        // variable to work around that...
        {
        if (PropInfo.PropertyType.TypeKind = tkMethod) and
           (not PropInfo.GetValue(Comp).IsEmpty) then
        }
        if PropInfo.PropertyType.TypeKind = tkMethod then
        begin
          PropValue := PropInfo.GetValue(Comp);
          if not PropValue.IsEmpty then
          begin
            // although the System.Rtti unit was introduced in Delphi 2010,
            // the TRttiInvokableType class was not added to it until XE2
            {$IFDEF VCL_XE2_OR_ABOVE}
            PropType := PropInfo.PropertyType as TRttiMethodType;
            for Param in PropType.GetParameters do begin
              SendUnitNameToProc(GetUnitNameForType(Param.ParamType), Proc);
            end;
            SendUnitNameToProc(GetUnitNameForType(PropType.ReturnType), Proc);
            {$ELSE}
            // use the System.TypInfo unit to access the parameters and return type
            GetUnitNamesForMethodType(PropInfo.PropertyType.Handle, Proc);
            {$ENDIF}
          end;
        end;
      end;

      {$ELSE}

      PropCount := GetPropList(Comp, PropList);
      if PropCount > 0 then
      begin
        try
          for J := 0 to PropCount-1 do
          begin
            PropInfo := PropList^[J];
            // only interested in *assigned* event handlers
            if (PropInfo^.PropType^.Kind = tkMethod) and
               (GetMethodProp(Comp, PropInfo).Code <> nil) then
            begin
              GetUnitNamesForMethodType(PropInfo^.PropType^, Proc);
            end;
          end;
        finally
          FreeMem(PropList);
        end;
      end;

      {$ENDIF}
    end;
  end;
end;
{$ENDIF}

procedure Register;
begin
  {$IFNDEF FPC}
  RegisterComponents(RSRegIndyClients, [
   TIdTCPClient
   ,TIdUDPClient
   ,TIdCmdTCPClient
   ,TIdIPMCastClient
   {$IFNDEF DOTNET}
   ,TIdIcmpClient
   ,TIdTraceRoute
   {$ENDIF}
  ]);
  RegisterComponents(RSRegIndyServers, [
   TIdUDPServer,
   TIdCmdTCPServer,
   TIdSimpleServer,
   TIdTCPServer,
   TIdIPMCastServer
  ]);
  RegisterComponents(RSRegIndyIOHandlers,[
   TIdIOHandlerStack
   ,TIdIOHandlerStream
   ,TIdServerIOHandlerStack
  ]);
  RegisterComponents(RSRegIndyIntercepts, [
   TIdConnectionIntercept
   ,TIdInterceptSimLog
   ,TIdInterceptThrottler
   ,TIdLogDebug
   ,TIdLogEvent
   ,TIdLogFile
   ,TIdLogStream
  ]);

  {$IFDEF FMX}
  // RLebeau 8/1/2011 - FireMonkey has problems resolving references to
  // TIdAntiFreeze correctly because it is implemented in a design-time
  // package and not a run-time package.  Until we can fix that properly,
  // we'll group TIdAntiFreeze with TControl so the IDE can filter out
  // TIdAntiFreeze from appearing at design-time in FireMoney projects.
  // Users will have to instantiate TIdAntiFreeze in code. This does not
  // affect VCL projects.
  GroupDescendentsWith(TIdAntiFreeze, TControl);
  {$ENDIF}

  RegisterComponents(RSRegIndyMisc, [
   TIdSocksInfo,
   TIdAntiFreeze,
   TIdSchedulerOfThreadDefault,
   TIdSchedulerOfThreadPool,
   TIdThreadComponent
  ]);
  {$ELSE}
  //This is a tempoary workaround for components not fitting on the palette
  //in Lazarus.  Unlike Delphi, Lazarus still does not have the ability to
  //scroll through a palette page.
  RegisterComponents(RSRegIndyClients+CoreSuffix, [
   TIdTCPClient
   ,TIdUDPClient
   ,TIdCmdTCPClient
   ,TIdIPMCastClient
   {$IFNDEF DOTNET}
   ,TIdIcmpClient
   ,TIdTraceRoute
   {$ENDIF}
  ]);
  RegisterComponents(RSRegIndyServers+CoreSuffix, [
   TIdUDPServer,
   TIdCmdTCPServer,
   TIdSimpleServer,
   TIdTCPServer,
   TIdIPMCastServer
  ]);
  RegisterComponents(RSRegIndyIOHandlers+CoreSuffix,[
   TIdIOHandlerStack
   ,TIdIOHandlerStream
   ,TIdServerIOHandlerStack
  ]);
  RegisterComponents(RSRegIndyIntercepts+CoreSuffix, [
   TIdConnectionIntercept
   ,TIdInterceptSimLog
   ,TIdInterceptThrottler
   ,TIdLogDebug
   ,TIdLogEvent
   ,TIdLogFile
   ,TIdLogStream
  ]);
  RegisterComponents(RSRegIndyMisc+CoreSuffix, [
   TIdSocksInfo,
   TIdAntiFreeze,
   TIdSchedulerOfThreadDefault,
   TIdSchedulerOfThreadPool,
   TIdThreadComponent
  ]);
  {$ENDIF}

  {$IFDEF HAS_TSelectionEditor}
  RegisterSelectionEditor(TIdBaseComponent, TIdBaseComponentSelectionEditor);
  {$ENDIF}
end;

{$IFDEF FPC}
initialization
{$i IdRegisterCore.lrs}
{$ENDIF}
end.

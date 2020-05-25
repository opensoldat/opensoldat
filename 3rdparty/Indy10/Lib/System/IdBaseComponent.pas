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
  Rev 1.10    08.11.2004 ã. 20:00:46  DBondzhev
  changed TObject to &Object

  Rev 1.9    07.11.2004 ã. 18:17:54  DBondzhev
  This contains fix for proper call to unit initialization sections.

  Rev 1.8    2004.11.06 10:55:00 PM  czhower
  Fix for Delphi 2005.

  Rev 1.7    2004.10.26 9:07:30 PM  czhower
  More .NET implicit conversions

  Rev 1.6    2004.10.26 7:51:58 PM  czhower
  Fixed ifdef and renamed TCLRStrings to TIdCLRStrings

  Rev 1.5    2004.10.26 7:35:16 PM  czhower
  Moved IndyCat to CType in IdBaseComponent

  Rev 1.4    04.10.2004 13:15:06  Andreas Hausladen
  Thread Safe Unit initialization

  Rev 1.3    6/11/2004 8:28:26 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.2    2004.04.16 9:18:34 PM  czhower
  .NET fix to call initialization sections. Code taken from IntraWeb.

  Rev 1.1    2004.02.03 3:15:50 PM  czhower
  Updates to move to System.

  Rev 1.0    2004.02.03 2:28:26 PM  czhower
  Move

  Rev 1.4    2004.01.25 11:35:02 PM  czhower
  IFDEF fix for .net.

  Rev 1.3    2004.01.25 10:56:44 PM  czhower
  Bug fix for InitComponent at design time.

  Rev 1.2    2004.01.20 10:03:20 PM  czhower
  InitComponent

  Rev 1.1    2003.12.23 7:33:00 PM  czhower
  .Net change.

  Rev 1.0    11/13/2002 08:38:26 AM  JPMugaas
}

unit IdBaseComponent;

// Kudzu: This unit is permitted to viloate IFDEF restriction to harmonize
// VCL / .Net difference at the base level.

interface

{$I IdCompilerDefines.inc}

uses
  Classes
  {$IFDEF DOTNET}
  {$DEFINE IdDEBUG},
  System.ComponentModel.Design.Serialization,
  System.Collections.Specialized,
  System.ComponentModel,
  System.Threading,
  System.Reflection,
  System.IO // Necessary else System.IO below is confused with RTL System.
  {$ENDIF};


// ***********************************************************
// TIdBaseComponent is the base class for all Indy components.
// ***********************************************************
type
  // TIdInitializerComponent exists to consolidate creation differences between .net and vcl.
  // It looks funny, but because of .net restrictions on constructors we have to do some wierdo
  // stuff to catch both constructors.
  //
  // TIdInitializerComponent implements InitComponent which all components must use to initialize
  // other members instead of overriding constructors.
  {$IFDEF DOTNET}
  //IMPORTANT!!!
  //Abstract classes should be hidden in the assembly designer.
  //Otherwise, you get a mess.
  [DesignTimeVisible(false), ToolboxItem(false)]
  {$ENDIF}
  TIdInitializerComponent = class(TComponent)
  private
  protected
    {$IFDEF DOTNET}
    // This event handler will take care about dynamically loaded assemblies after first initialization.
    class procedure AssemblyLoadEventHandler(sender: &Object; args: AssemblyLoadEventArgs); static;
    class procedure InitializeAssembly(AAssembly: Assembly);
    {$ENDIF}
    function GetIsLoading: Boolean;
    function GetIsDesignTime: Boolean;
    // This is here to handle both types of constructor initializations, VCL and .Net.
    // It is not abstract so that not all descendants are required to override it.
    procedure InitComponent; virtual;
  public
    {$IFDEF DOTNET}
    // Should not be able to make this create virtual? But if not
    // DCCIL complain in IdIOHandler about possible polymorphics....
    constructor Create; overload; virtual;
    // Must be overriden here - but VCL version will catch offenders
    {$ELSE}
    // Statics to prevent overrides. For Create(AOwner) see TIdBaseComponent
    //
    // Create; variant is here to allow calls from VCL the same as from .net
    constructor Create; reintroduce; overload;
    // Must be an override and thus virtual to catch when created at design time
    //constructor Create(AOwner: TComponent); overload; override;
    {$ENDIF}
    constructor Create(AOwner: TComponent); overload; override;
  end;

  // TIdBaseComponent is the base class for all Indy components. Utility components, and other non
  // socket based components typically inherit directly from this. While socket components inherit
  // from TIdComponent instead as it introduces OnWork, OnStatus, etc.
  TIdBaseComponent = class(TIdInitializerComponent)
  protected
    function GetIndyVersion: string;
    property IsLoading: Boolean read GetIsLoading;
    property IsDesignTime: Boolean read GetIsDesignTime;
  public
    // This is here to catch components trying to override at compile time and not let them.
    // This does not work in .net, but we always test in VCL so this will catch it.
    {$IFNDEF DOTNET}
    constructor Create(AOwner: TComponent); reintroduce; overload;
    {$ENDIF}
    {$IFNDEF HAS_RemoveFreeNotification}
    procedure RemoveFreeNotification(AComponent: TComponent);
    {$ENDIF}
    property Version: string read GetIndyVersion;
  published
  end;

implementation

uses
  {$IFDEF DOTNET}
  System.Runtime.CompilerServices,
  {$ENDIF}
  IdGlobal;

{$IFDEF DOTNET}
var
  GInitsCalled: Integer = 0;
{$ENDIF}

{ TIdInitializerComponent }

constructor TIdInitializerComponent.Create;
begin
  {-$IFDEF DOTNET}
  inherited Create(nil); // Explicit just in case since are not an override
  InitComponent;
  {-$ELSE}
//  Create(nil);
  {-$ENDIF}
end;

constructor TIdInitializerComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // DCCIL will not call our other create from this one, only .Nets ancestor
  // so InitComponent will NOT be called twice.
  InitComponent;
end;

{$IFDEF DOTNET}
class procedure TIdInitializerComponent.AssemblyLoadEventHandler(sender: &Object;
  args: AssemblyLoadEventArgs);
begin
  if (args <> nil) then begin
    InitializeAssembly(args.loadedAssembly);
  end;
end;

class procedure TIdInitializerComponent.InitializeAssembly(AAssembly: Assembly);
var
  LTypesList: Array of &Type;
  j: integer;
  UnitType: &Type;
begin
    LTypesList := AAssembly.GetTypes();

    for j := Low(LTypesList) to High(LTypesList) do begin
      UnitType := LTypesList[j];

      // Delphi 9 assemblies
      if (Pos('.Units', UnitType.Namespace) > 0) and (UnitType.Name <> '$map$') then begin
          RuntimeHelpers.RunClassConstructor(UnitType.TypeHandle);
      end;
      // Delphi 8 assemblies
//      if UnitType.Name = 'Unit' then begin
//        RuntimeHelpers.RunClassConstructor(UnitType.TypeHandle);
//      end;
    end;
end;
{$ENDIF}

procedure TIdInitializerComponent.InitComponent;
{$IFDEF DOTNET}
var
  LAssemblyList: array of Assembly;
  i: integer;
  LM : String;
{$ENDIF}
begin
  {$IFDEF DOTNET}
  try
  // With .NET initialization sections are not called unless the unit is referenced. D.NET makes
  // initializations and globals part of a "Unit" class. So init sections wont get called unless
  // the Unit class is used. D8 EXEs are ok, but assemblies (ie VS.NET and probably asms in some
  // cases when used from a D8 EXE) do not call their init sections. So we loop through the list of
  // classes in the assembly, and for each one named Unit we call the class constructor which
  // causes the init section to be called.
  //
  if Interlocked.CompareExchange(GInitsCalled, 1, 0) = 0 then begin
    LAssemblyList := AppDomain.get_CurrentDomain.GetAssemblies;

    // Becouse this can be called few times we have to exclu de every time
    Exclude(AppDomain.get_CurrentDomain.AssemblyLoad, TIdInitializerComponent.AssemblyLoadEventHandler);
    Include(AppDomain.get_CurrentDomain.AssemblyLoad, TIdInitializerComponent.AssemblyLoadEventHandler);

    for i := low(LAssemblyList) to high(LAssemblyList) do begin
      //We do things this way because we do not want to initialize stuff that is not
      //ours.  That would cause errors.  It turns out that "AppDomain.get_CurrentDomain.GetAssemblies;" will
      //list stuff that isn't ours.  Be careful.
      if (Pos('Indy',LAssemblyList[i].GetName.Name)=1) then
      begin
        initializeAssembly(LAssemblyList[i]);
      end;
    end;
  end;
  except
    on E : ReflectionTypeLoadException do
    begin
      LM := EOL;
      LM := LM + 'Message:     ' + E.Message + EOL;
      LM := LM + 'Source:      ' + E.Source + EOL;
      LM := LM + 'Stack Trace: ' + E.StackTrace + EOL;
      for i := Low(E.LoaderExceptions) to High(E.LoaderExceptions) do
      begin
        LM := LM + EOL;
        LM := LM + 'Error #'       + i.ToString+EOL;
        LM := LM + 'Message:     ' + E.LoaderExceptions[i].Message + EOL;
        LM := LM + 'Source:      ' + E.LoaderExceptions[i].Source + EOL;
        LM := LM + 'Stack Trace: ' + EOL+ E.LoaderExceptions[i].StackTrace + EOL;
      end;
      IndyRaiseOuterException(Exception.Create('Load Error'+EOL+LM));
    end;
  end;
  {$ENDIF}
end;

function TIdInitializerComponent.GetIsLoading: Boolean;
begin
  Result := (csLoading in ComponentState);
end;

function TIdInitializerComponent.GetIsDesignTime: Boolean;
begin
  Result := (csDesigning in ComponentState);
end;

{ TIdBaseComponent }

{$IFNDEF DOTNET}
constructor TIdBaseComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // Explicit just in case since are not an override
end;
{$ENDIF}

{$IFNDEF HAS_RemoveFreeNotification}
procedure TIdBaseComponent.RemoveFreeNotification(AComponent: TComponent);
begin
  // this is a no-op for now, as we can't access the private TComponent.FFreeNotifies list
end;
{$ENDIF}

function TIdBaseComponent.GetIndyVersion: string;
begin
  Result := gsIdVersion;
end;

end.


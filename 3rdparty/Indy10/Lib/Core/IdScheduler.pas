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
  Rev 1.14    4/8/2004 11:55:30 AM  BGooijen
  Fix for D5

  Rev 1.13    2004.03.01 5:12:38 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.12    2004.01.20 10:03:30 PM  czhower
  InitComponent

  Rev 1.11    2003.10.21 12:18:58 AM  czhower
  TIdTask support and fiber bug fixes.

  Rev 1.10    2003.10.14 11:18:08 PM  czhower
  Fix for AV on shutdown and other bugs

  Rev 1.9    2003.10.11 5:49:24 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.8    2003.09.19 10:11:16 PM  czhower
  Next stage of fiber support in servers.

  Rev 1.7    2003.09.19 11:54:30 AM  czhower
  -Completed more features necessary for servers
  -Fixed some bugs

  Rev 1.6    2003.09.18 4:10:24 PM  czhower
  Preliminary changes for Yarn support.

  Rev 1.5    3/27/2003 5:15:36 PM  BGooijen
  Moved some code from subclasses here, made MaxThreads published

  Rev 1.4    3/13/2003 10:18:36 AM  BGooijen
  Server side fibers, bug fixes

  Rev 1.1    1/23/2003 11:06:04 AM  BGooijen

  Rev 1.0    1/17/2003 03:41:48 PM  JPMugaas
  Scheduler base class.
}

unit IdScheduler;

interface

{$i IdCompilerDefines.inc}

uses
  {$IFDEF HAS_UNIT_Generics_Collections}
  System.Generics.Collections,
  {$ELSE}
    {$IFDEF VCL_XE3_OR_ABOVE}
  System.Classes,
    {$ELSE}
  Classes,
    {$ENDIF}
  {$ENDIF}
  IdBaseComponent, IdThread, IdTask, IdYarn, IdThreadSafe;

type
  {$IFDEF HAS_GENERICS_TThreadList}
  TIdYarnThreadList = TIdThreadSafeObjectList<TIdYarn>;
  TIdYarnList = TList<TIdYarn>;
  {$ELSE}
  // TODO: flesh out to match TIdThreadSafeObjectList<TIdYarn> and TList<TIdYarn> for non-Generics compilers
  TIdYarnThreadList = TIdThreadSafeObjectList;
  TIdYarnList = TList;
  {$ENDIF}

  TIdScheduler = class(TIdBaseComponent)
  protected
    FActiveYarns: TIdYarnThreadList;
    //
    procedure InitComponent; override;
  public
    destructor Destroy; override;
    function AcquireYarn: TIdYarn; virtual; abstract;
    procedure Init; virtual;
    // ReleaseYarn is to remove a yarn from the list that has already been
    // terminated (usually self termination);
    procedure ReleaseYarn(AYarn: TIdYarn); virtual;
    procedure StartYarn(AYarn: TIdYarn; ATask: TIdTask); virtual; abstract;
    // TerminateYarn is to terminate a yarn explicitly and remove it also
    procedure TerminateYarn(AYarn: TIdYarn); virtual; abstract;
    procedure TerminateAllYarns; virtual;
    //
    property ActiveYarns: TIdYarnThreadList read FActiveYarns;
  end;

implementation

uses
  //facilitate inlining only.
  {$IFDEF DOTNET}
    {$IFDEF USE_INLINE}
  System.Threading,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF VCL_2010_OR_ABOVE}
    {$IFDEF WINDOWS}
  Windows,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_VCL_POSIX}
  Posix.SysSelect,
  Posix.SysTime,
  {$ENDIF}
  {$IFDEF HAS_UNIT_Generics_Collections}
    {$IFDEF VCL_XE3_OR_ABOVE}
  System.Classes,
  System.Types,
    {$ELSE}
  Classes,
    {$ENDIF}
  {$ELSE}
      {$IFDEF VCL_XE3_OR_ABOVE}
  System.Types,    //here to facilitate inlining
      {$ENDIF}
  {$ENDIF}
  IdGlobal, SysUtils;

{ TIdScheduler }

destructor TIdScheduler.Destroy;
begin
  FreeAndNil(FActiveYarns);
  inherited Destroy;
end;

procedure TIdScheduler.Init;
begin
end;

procedure TIdScheduler.InitComponent;
begin
  inherited InitComponent;
  FActiveYarns := TIdYarnThreadList.Create;
end;

procedure TIdScheduler.ReleaseYarn(AYarn: TIdYarn);
begin
  ActiveYarns.Remove(AYarn);
end;

procedure TIdScheduler.TerminateAllYarns;
var
  i: Integer;
  LList: TIdYarnList;
begin
  Assert(FActiveYarns<>nil);

  while True do begin
    // Must unlock each time to allow yarns that are terminating to remove themselves from the list
    LList := FActiveYarns.LockList;
    try
      if LList.Count = 0 then begin
        Break;
      end;
      for i := LList.Count - 1 downto 0 do begin
        TerminateYarn(
          {$IFDEF HAS_GENERICS_TList}LList.Items[i]{$ELSE}TIdYarn(LList.Items[i]){$ENDIF}
        );
      end;
    finally
      FActiveYarns.UnlockList;
    end;
    //TODO: Put terminate timeout check back
    IndySleep(500); // Wait a bit before looping to prevent thrashing
  end;
end;

end.

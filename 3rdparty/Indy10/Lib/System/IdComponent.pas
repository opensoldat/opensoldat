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
  Rev 1.4    1/17/2005 7:26:12 PM  JPMugaas
  Moved stack management code to IdStack.

  Rev 1.3    2004.06.06 5:18:14 PM  czhower
  OnWork bug fix

  Rev 1.2    2004.06.05 9:46:38 AM  czhower
  IOHandler OnWork fix

  Rev 1.1    2004.02.03 3:15:52 PM  czhower
  Updates to move to System.

  Rev 1.0    2004.02.03 2:28:28 PM  czhower
  Move

  Rev 1.7    2004.01.22 5:59:10 PM  czhower
  IdCriticalSection

  Rev 1.6    2004.01.20 10:03:24 PM  czhower
  InitComponent

  Rev 1.5    2003.10.14 1:26:42 PM  czhower
  Uupdates + Intercept support

  Rev 1.4    2003.10.01 9:11:16 PM  czhower
  .Net

  Rev 1.3    2003.10.01 11:16:30 AM  czhower
  .Net

  Rev 1.2    2003.09.30 1:22:54 PM  czhower
  Stack split for DotNet

  Rev 1.1    2003.09.18 5:17:58 PM  czhower
  Implemented OnWork

  Rev 1.0    11/13/2002 08:41:12 AM  JPMugaas
}

unit IdComponent;

interface

{$i IdCompilerDefines.inc}

uses
  {$IFNDEF USE_OBJECT_ARC}
  Classes,
  {$ENDIF}
  IdBaseComponent, IdGlobal, IdResourceStrings,
  IdStack;

type
  TIdStatus = ( hsResolving,
                hsConnecting,
                hsConnected,
                hsDisconnecting,
                hsDisconnected,
                hsStatusText,
                ftpTransfer,  // These are to eliminate the TIdFTPStatus and the
                ftpReady,     // coresponding event
                ftpAborted);  // These can be use din the other protocols to.

const
  IdStati: array[TIdStatus] of string = (
                RSStatusResolving,
                RSStatusConnecting,
                RSStatusConnected,
                RSStatusDisconnecting,
                RSStatusDisconnected,
                RSStatusText,
                RSStatusText,
                RSStatusText,
                RSStatusText);

type
  TIdStatusEvent = procedure(ASender: TObject; const AStatus: TIdStatus;
   const AStatusText: string) of object;

  TWorkMode = (wmRead, wmWrite);
  TWorkInfo = record
    Current: Int64;
    Max: Int64;
    Level: Integer;
  end;

  TWorkBeginEvent = procedure(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64) of object;
  TWorkEndEvent = procedure(ASender: TObject; AWorkMode: TWorkMode) of object;
  TWorkEvent = procedure(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64) of object;

  TIdComponent = class(TIdBaseComponent)
  protected
    FOnStatus: TIdStatusEvent;
    FOnWork: TWorkEvent;
    FOnWorkBegin: TWorkBeginEvent;
    FOnWorkEnd: TWorkEndEvent;
    FWorkInfos: array[TWorkMode] of TWorkInfo;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FWorkTarget: TIdComponent;
    //
    procedure DoStatus(AStatus: TIdStatus); overload;
    procedure DoStatus(AStatus: TIdStatus; const AArgs: array of const); overload;
    procedure InitComponent; override;
    {$IFNDEF USE_OBJECT_ARC}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ENDIF}
    procedure SetWorkTarget(AValue: TIdComponent);
    //
    property OnWork: TWorkEvent read FOnWork write FOnWork;
    property OnWorkBegin: TWorkBeginEvent read FOnWorkBegin write FOnWorkBegin;
    property OnWorkEnd: TWorkEndEvent read FOnWorkEnd write FOnWorkEnd;
  public
    procedure BeginWork(AWorkMode: TWorkMode; const ASize: Int64 = 0); virtual;
    destructor Destroy; override;
    procedure DoWork(AWorkMode: TWorkMode; const ACount: Int64); virtual;
    procedure EndWork(AWorkMode: TWorkMode); virtual;
    //
    property WorkTarget: TIdComponent read FWorkTarget write SetWorkTarget;
  published
    property OnStatus: TIdStatusEvent read FOnStatus write FOnStatus;
  end;

implementation

{ TIdComponent }

destructor TIdComponent.Destroy;
begin
  inherited Destroy;
  // After inherited - do at last possible moment
  TIdStack.DecUsage;
end;

procedure TIdComponent.DoStatus(AStatus: TIdStatus);
begin
  DoStatus(AStatus, []);
end;

procedure TIdComponent.DoStatus(AStatus: TIdStatus; const AArgs: array of const);
begin
  // We do it this way because Format() can sometimes cause an AV if the
  // variable array is blank and there is something like a %s or %d.  This
  // is why there was sometimes an AV in TIdFTP
  if Assigned(OnStatus) then begin
    if Length(AArgs) = 0 then begin
      OnStatus(Self, AStatus, IndyFormat(IdStati[AStatus], ['']));  {Do not Localize}
    end else begin
      OnStatus(Self, AStatus, IndyFormat(IdStati[AStatus], AArgs));
    end;
  end;
end;

procedure TIdComponent.BeginWork(AWorkMode: TWorkMode; const ASize: Int64 = 0);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LWorkTarget: TIdComponent;
begin
  LWorkTarget := FWorkTarget;
  if LWorkTarget <> nil then begin
    LWorkTarget.BeginWork(AWorkMode, ASize);
  end else begin
    Inc(FWorkInfos[AWorkMode].Level);
    if FWorkInfos[AWorkMode].Level = 1 then begin
      FWorkInfos[AWorkMode].Max := ASize;
      FWorkInfos[AWorkMode].Current := 0;
      if Assigned(OnWorkBegin) then begin
        OnWorkBegin(Self, AWorkMode, ASize);
      end;
    end;
  end;
end;

procedure TIdComponent.DoWork(AWorkMode: TWorkMode; const ACount: Int64);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LWorkTarget: TIdComponent;
begin
  LWorkTarget := FWorkTarget;
  if LWorkTarget <> nil then begin
    LWorkTarget.DoWork(AWorkMode, ACount);
  end else begin
    if FWorkInfos[AWorkMode].Level > 0 then begin
      Inc(FWorkInfos[AWorkMode].Current, ACount);
      if Assigned(OnWork) then begin
        OnWork(Self, AWorkMode, FWorkInfos[AWorkMode].Current);
      end;
    end;
  end;
end;

procedure TIdComponent.EndWork(AWorkMode: TWorkMode);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LWorkTarget: TIdComponent;
begin
  LWorkTarget := FWorkTarget;
  if LWorkTarget <> nil then begin
    LWorkTarget.EndWork(AWorkMode);
  end else begin
    if FWorkInfos[AWorkMode].Level = 1 then begin
      if Assigned(OnWorkEnd) then begin
        OnWorkEnd(Self, AWorkMode);
      end;
    end;
    Dec(FWorkInfos[AWorkMode].Level);
  end;
end;

procedure TIdComponent.InitComponent;
begin
  inherited InitComponent;
  TIdStack.IncUsage;
end;

// under ARC, all weak references to a freed object get nil'ed automatically
{$IFNDEF USE_OBJECT_ARC}
procedure TIdComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FWorkTarget) then begin
    FWorkTarget := nil;
  end;
  inherited Notification(AComponent, Operation);
end;
{$ENDIF}

procedure TIdComponent.SetWorkTarget(AValue: TIdComponent);
begin
  {$IFDEF USE_OBJECT_ARC}
  // under ARC, all weak references to a freed object get nil'ed automatically
  FWorkTarget := AValue;
  {$ELSE}
  if FWorkTarget <> AValue then begin
    if Assigned(FWorkTarget) then begin
      FWorkTarget.RemoveFreeNotification(Self);
    end;
    FWorkTarget := AValue;
    if Assigned(AValue) then begin
      AValue.FreeNotification(Self);
    end;
  end;
  {$ENDIF}
end;

end.

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
  Rev 1.10    3/10/2005 12:00:46 AM  JPMugaas
  Minor problem Craig Peterson had noted in an E-Mail to me.

  Rev 1.9    11/30/04 6:19:12 PM  RLebeau
  Promoted the TIdConnectionIntercept.Intercept property from protected to
  published

  Rev 1.8    2004.02.03 4:16:44 PM  czhower
  For unit name changes.

  Rev 1.7    2004.01.20 10:03:24 PM  czhower
  InitComponent

  Rev 1.6    5/12/2003 12:33:32 AM  GGrieve
  add Data from BlockCipher descendent

  Rev 1.5    2003.10.14 1:26:48 PM  czhower
  Uupdates + Intercept support

  Rev 1.4    2003.10.11 5:48:16 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.3    10/5/2003 3:20:46 PM  BGooijen
  .net

  Rev 1.2    2003.10.01 1:12:34 AM  czhower
  .Net

  Rev 1.1    3/5/2003 10:59:48 PM  BGooijen
  Fixed (i know, the SendBuffer looks bad)

  Rev 1.0    11/13/2002 08:44:42 AM  JPMugaas

  2002-03-01 - Andrew P.Rybin
    - Nested Intercept support (ex: ->logging->compression->encryption)

  2002-04-09 - Chuck Smith
    - set ABuffer.Position := 0; in OnSend/OnReceive for Nested Stream send/receive
}

unit IdIntercept;

interface

{$I IdCompilerDefines.inc}
//here only to put FPC in Delphi mode

uses
  Classes,
  IdGlobal, IdBaseComponent, IdBuffer, IdException;

type
  EIdInterceptCircularLink = class(EIdException);
  TIdConnectionIntercept = class;
  TIdInterceptNotifyEvent = procedure(ASender: TIdConnectionIntercept) of object;
  TIdInterceptStreamEvent = procedure(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes) of object;

  TIdConnectionIntercept = class(TIdBaseComponent)
  protected
    FConnection: TComponent;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FIntercept: TIdConnectionIntercept;
    FIsClient: Boolean;
    {$IFDEF USE_OBJECT_ARC}
    // When ARC is enabled, object references MUST be valid objects.
    // It is common for users to store non-object values, though, so
    // we will provide separate properties for those purposes
    //
    // TODO; use TValue instead of separating them
    //
    FDataObject: TObject;
    FDataValue: PtrInt;
    {$ELSE}
    FData: TObject;
    {$ENDIF}

    FOnConnect: TIdInterceptNotifyEvent;
    FOnDisconnect: TIdInterceptNotifyEvent;
    FOnReceive: TIdInterceptStreamEvent;
    FOnSend: TIdInterceptStreamEvent;
    //
    procedure InitComponent; override;
    {$IFNDEF USE_OBJECT_ARC}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ENDIF}
    procedure SetIntercept(AValue: TIdConnectionIntercept);
    //
  public
    procedure Connect(AConnection: TComponent); virtual;
    procedure Disconnect; virtual;
    procedure Receive(var VBuffer: TIdBytes); virtual;
    procedure Send(var VBuffer: TIdBytes); virtual;
    //
    property Connection: TComponent read FConnection;
    property IsClient: Boolean read FIsClient;

    // user can use this to keep context
    {$IFDEF USE_OBJECT_ARC}
    property DataObject: TObject read FDataObject write FDataObject;
    property DataValue: PtrInt read FDataValue write FDataValue;
    {$ELSE}
    property Data: TObject read FData write FData;
    {$ENDIF}
  published
    property Intercept: TIdConnectionIntercept read FIntercept write SetIntercept;
    property OnConnect: TIdInterceptNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TIdInterceptNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnReceive: TIdInterceptStreamEvent read FOnReceive write FOnReceive;
    property OnSend: TIdInterceptStreamEvent read FOnSend write FOnSend;
  end;

  TIdServerIntercept = class(TIdBaseComponent)
  public
    procedure Init; virtual; abstract;
    function Accept(AConnection: TComponent): TIdConnectionIntercept; virtual; abstract;
  end;

implementation
uses
  IdResourceStringsCore;

{ TIdIntercept }

procedure TIdConnectionIntercept.Disconnect;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  LIntercept := Intercept;
  if LIntercept <> nil then begin
    LIntercept.Disconnect;
  end;
  if Assigned(OnDisconnect) then begin
    OnDisconnect(Self);
  end;
  FConnection := nil;
end;

procedure TIdConnectionIntercept.Connect(AConnection: TComponent);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  FConnection := AConnection;
  if Assigned(OnConnect) then begin
    OnConnect(Self);
  end;
  LIntercept := Intercept;
  if LIntercept <> nil then begin
    LIntercept.Connect(AConnection);
  end;
end;

procedure TIdConnectionIntercept.Receive(var VBuffer: TIdBytes);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  LIntercept := Intercept;
  if LIntercept <> nil then begin
    LIntercept.Receive(VBuffer);
  end;
  if Assigned(OnReceive) then begin
    OnReceive(Self, VBuffer);
  end;
end;

procedure TIdConnectionIntercept.Send(var VBuffer: TIdBytes);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  if Assigned(OnSend) then begin
    OnSend(Self, VBuffer);
  end;
  LIntercept := Intercept;
  if LIntercept <> nil then begin
    LIntercept.Send(VBuffer);
  end;
end;

procedure TIdConnectionIntercept.SetIntercept(AValue: TIdConnectionIntercept);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
  LNextValue: TIdConnectionIntercept;
begin
  LIntercept := FIntercept;
  if LIntercept <> AValue then
  begin
    LNextValue := AValue;
    while Assigned(LNextValue) do begin
      if LNextValue = Self then begin //recursion
        raise EIdInterceptCircularLink.CreateFmt(RSInterceptCircularLink, [ClassName]);
      end;
      LNextValue := LNextValue.Intercept;
    end;

    // under ARC, all weak references to a freed object get nil'ed automatically

    {$IFNDEF USE_OBJECT_ARC}
    // remove self from the Intercept's free notification list    {Do not Localize}
    if Assigned(LIntercept) then begin
      LIntercept.RemoveFreeNotification(Self);
    end;
    {$ENDIF}

    FIntercept := AValue;

    {$IFNDEF USE_OBJECT_ARC}
    // add self to the Intercept's free notification list    {Do not Localize}
    if Assigned(AValue) then begin
      AValue.FreeNotification(Self);
    end;
    {$ENDIF}
  end;
end;

// under ARC, all weak references to a freed object get nil'ed automatically
{$IFNDEF USE_OBJECT_ARC}
procedure TIdConnectionIntercept.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Intercept) then begin
    FIntercept := nil;
  end;
  inherited Notification(AComponent, OPeration);
end;
{$ENDIF}

procedure TIdConnectionIntercept.InitComponent;
begin
  inherited InitComponent;
  FIsClient := True;
end;

end.

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
  Rev 1.5    2004.02.03 4:17:14 PM  czhower
  For unit name changes.

  Rev 1.4    2004.01.20 10:03:28 PM  czhower
  InitComponent

  Rev 1.3    2003.10.17 6:15:54 PM  czhower
  Upgrades

  Rev 1.2    2003.10.14 1:27:08 PM  czhower
  Uupdates + Intercept support

  Rev 1.1    6/16/2003 10:39:02 AM  EHill
  Done: Expose Open/Close as public in TIdLogBase

  Rev 1.0    11/13/2002 07:55:58 AM  JPMugaas
}

unit IdLogBase;

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  Classes,
  IdIntercept, IdGlobal, IdSocketHandle, IdBaseComponent;

type
  TIdLogBase = class(TIdConnectionIntercept)
  protected
    FActive: Boolean;
    FLogTime: Boolean;
    FReplaceCRLF: Boolean;
    FStreamedActive: Boolean;
    //
    procedure InitComponent; override;
    procedure LogStatus(const AText: string); virtual; abstract;
    procedure LogReceivedData(const AText, AData: string); virtual; abstract;
    procedure LogSentData(const AText, AData: string); virtual; abstract;
    procedure SetActive(AValue: Boolean); virtual;
    procedure Loaded; override;
    function ReplaceCR(const AString : String) : String;
  public
    procedure Open; virtual;
    procedure Close; virtual;
    procedure Connect(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure Disconnect; override;
    procedure Receive(var ABuffer: TIdBytes); override;
    procedure Send(var ABuffer: TIdBytes); override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property LogTime: Boolean read FLogTime write FLogTime default True;
    property ReplaceCRLF: Boolean read FReplaceCRLF write FReplaceCRLF default true;
  end;

implementation

uses
  IdResourceStringsCore, SysUtils;

const
  LOldStr : array [0..2] of string =
    ( EOL,  CR, LF );
  LNewStr : array [0..2] of string =
    ( RSLogEOL, RSLogCR, RSLogLF );

{ TIdLogBase }

procedure TIdLogBase.Close;
begin
end;

procedure TIdLogBase.Connect(AConnection: TComponent);
begin
  inherited Connect(AConnection);
  if FActive then begin
    LogStatus(RSLogConnected);
  end;
end;

destructor TIdLogBase.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TIdLogBase.Disconnect;
begin
  if FActive then begin
    LogStatus(RSLogDisconnected);
  end;
  inherited Disconnect;
end;

procedure TIdLogBase.InitComponent;
begin
  inherited InitComponent;
  FLogTime := True;
  ReplaceCRLF := True;
end;

procedure TIdLogBase.Loaded;
begin
  inherited Loaded;
  Active := FStreamedActive;
end;

procedure TIdLogBase.Open;
begin
end;

procedure TIdLogBase.Receive(var ABuffer: TIdBytes);
var
  s: string;
  LMsg: string;
begin
  // let the next Intercept in the chain decode its data first
  inherited Receive(ABuffer);

  if FActive then begin
    LMsg := '';
    if LogTime then begin
      LMsg := DateTimeToStr(Now);
    end;
    s := BytesToStringRaw(ABuffer);
    if FReplaceCRLF then begin
      s := ReplaceCR(S);
    end;
    LogReceivedData(LMsg, s);
  end;
end;

function TIdLogBase.ReplaceCR(const AString: String): String;
begin
  Result := StringsReplace(AString, LOldStr, LNewStr);
end;

procedure TIdLogBase.Send(var ABuffer: TIdBytes);
var
  s: string;
  LMsg: string;
begin
  if FActive then begin
    LMsg := '';
    if LogTime then begin
      LMsg := DateTimeToStr(Now);
    end;
    s := BytesToStringRaw(ABuffer);
    if FReplaceCRLF then begin
      s := ReplaceCR(S);
    end;
    LogSentData(LMsg, s);
  end;

  // let the next Intercept in the chain encode its data next
  inherited Send(ABuffer);
end;

procedure TIdLogBase.SetActive(AValue: Boolean);
begin
  if IsDesignTime or IsLoading then begin
    FStreamedActive := AValue;
  end
  else if FActive <> AValue then
  begin
    FActive := AValue;
    if FActive then begin
      Open;
    end else begin
      Close;
    end;
  end;
end;

end.



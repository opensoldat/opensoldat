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
  Rev 1.27    2/3/05 12:16:46 AM  RLebeau
  Bug fix for UpdateText()

  Rev 1.25    1/15/2005 6:02:02 PM  JPMugaas
  These should compile again.

  Rev 1.24    1/15/05 2:03:20 PM  RLebeau
  Added AIgnore parameter to TIdReplies.Find()

  Updated TIdReply.SetNumericCode() to call SetCode() rather than assigning the
  FCode member directly.

  Updated TIdReply.SetCode() to call Clear() before assigning the FCode member.

  Updated TIdReplies.UpdateText() to ignore the TIdReply that was passed in
  when looking for a TIdReply to extract Text from.

  Rev 1.23    12/29/04 1:36:44 PM  RLebeau
  Bug fix for when descendant constructors are called twice during creation

  Rev 1.22    10/26/2004 8:43:00 PM  JPMugaas
  Should be more portable with new references to TIdStrings and TIdStringList.

  Rev 1.21    6/11/2004 8:48:24 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.20    2004.03.01 7:10:34 PM  czhower
  Change for .net compat

  Rev 1.19    2004.03.01 5:12:34 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.18    2004.02.29 8:16:54 PM  czhower
  Bug fix to fix AV at design time when adding reply texts to CmdTCPServer.

  Rev 1.17    2004.02.03 4:17:10 PM  czhower
  For unit name changes.

  Rev 1.16    2004.01.29 12:02:32 AM  czhower
  .Net constructor problem fix.

  Rev 1.15    1/3/2004 8:06:20 PM  JPMugaas
  Bug fix:  Sometimes, replies will appear twice due to the way functionality
  was enherited.

  Rev 1.14    1/1/2004 9:33:24 PM  BGooijen
  the abstract class TIdReply was created sometimes, fixed that

  Rev 1.13    2003.10.18 9:33:28 PM  czhower
  Boatload of bug fixes to command handlers.

  Rev 1.12    10/15/2003 7:49:38 PM  DSiders
  Added IdResourceStringsCore to implementation uses clause.

  Rev 1.11    10/15/2003 7:46:42 PM  DSiders
  Added formatted resource string for the exception raised in
  TIdReply.SetCode.

  Rev 1.10    2003.09.06 1:30:30 PM  czhower
  Removed abstract modifier from a class method so that C++ Builder can compile
  again.

  Rev 1.9    2003.06.05 10:08:50 AM  czhower
  Extended reply mechanisms to the exception handling. Only base and RFC
  completed, handing off to J Peter.

  Rev 1.8    2003.05.30 10:25:56 PM  czhower
  Implemented IsEndMarker

  Rev 1.7    2003.05.30 10:06:08 PM  czhower
  Changed code property mechanisms.

  Rev 1.6    5/26/2003 04:29:56 PM  JPMugaas
  Removed GenerateReply and ParseReply.  Those are now obsolete duplicate
  functions in the new design.

  Rev 1.5    5/26/2003 12:19:54 PM  JPMugaas

  Rev 1.4    2003.05.26 11:38:18 AM  czhower

  Rev 1.3    2003.05.25 10:23:44 AM  czhower

  Rev 1.2    5/20/2003 12:43:46 AM  BGooijen
  changeable reply types

  Rev 1.1    5/19/2003 05:54:58 PM  JPMugaas

  Rev 1.0    5/19/2003 12:26:16 PM  JPMugaas
  Base class for reply format objects.
}

unit IdReply;

interface

{$I IdCompilerDefines.inc}
//we need to put this in Delphi mode to work

uses
  Classes,
  IdException;

type
  TIdReplies = class;
  //TODO: a streamed write only property will be registered to convert old DFMs
  // into the new one for old TextCode and to ignore NumericCode which has been
  // removed
  TIdReply = class(TCollectionItem)
  protected
    FCode: string;
    FFormattedReply: TStrings;
    FReplyTexts: TIdReplies;
    FText: TStrings;
    //
    procedure AssignTo(ADest: TPersistent); override;
    procedure CommonInit;
    function GetFormattedReplyStrings: TStrings; virtual;
    function CheckIfCodeIsValid(const ACode: string): Boolean; virtual;
    function GetDisplayName: string; override;
    function GetFormattedReply: TStrings; virtual;
    function GetNumericCode: Integer;
    procedure SetCode(const AValue: string);
    procedure SetFormattedReply(const AValue: TStrings); virtual; abstract;
    procedure SetText(const AValue: TStrings);
    procedure SetNumericCode(const AValue: Integer);
  public
    procedure Clear; virtual;
    //Temp workaround for compiler bug
    constructor Create(ACollection: TCollection); override;
    constructor CreateWithReplyTexts(ACollection: TCollection; AReplyTexts: TIdReplies); virtual;
    // Both creates are necessary. This base one is called by the collection editor at design time
   // constructor Create(ACollection: TCollection); overload; override;
   // constructor Create(ACollection: TCollection; AReplyTexts: TIdReplies); reintroduce; overload; virtual;
    destructor Destroy; override;
    // Is not abstract because C++ cannot compile abstract class methods
    class function IsEndMarker(const ALine: string): Boolean; virtual;
    procedure RaiseReplyError; virtual; abstract;
    function ReplyExists: Boolean; virtual;
    procedure SetReply(const ACode: Integer; const AText: string); overload; virtual;
    procedure SetReply(const ACode: string; const AText: string); overload; virtual;
    procedure UpdateText;
    //
    property FormattedReply: TStrings read GetFormattedReply write SetFormattedReply;
    property NumericCode: Integer read GetNumericCode write SetNumericCode;
  published
    //warning: setting Code has a side-effect of calling Clear;
    property Code: string read FCode write SetCode;
    property Text: TStrings read FText write SetText;
  end;

  TIdReplyClass = class of TIdReply;

  TIdReplies = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TIdReply;
    procedure SetItem(Index: Integer; const Value: TIdReply);
  public
    function Add: TIdReply; overload;
    function Add(const ACode: Integer; const AText: string): TIdReply; overload;
    function Add(const ACode, AText: string): TIdReply; overload;
    constructor Create(AOwner: TPersistent; const AReplyClass: TIdReplyClass); reintroduce; virtual;
    function Find(const ACode: string; AIgnore: TIdReply = nil): TIdReply; virtual;
    procedure UpdateText(AReply: TIdReply); virtual;
    //
    property Items[Index: Integer]: TIdReply read GetItem write SetItem; default;
  end;

  TIdRepliesClass = class of TIdReplies;
  EIdReplyError = class(EIdException);

implementation

uses
  IdGlobal, IdResourceStringsCore, SysUtils;

{ TIdReply }

procedure TIdReply.AssignTo(ADest: TPersistent);
var
  LR : TIdReply;
begin
  if ADest is TIdReply then begin
    LR := TIdReply(ADest);
    //set code first as it possibly clears the reply
    LR.Code := Code;
    LR.Text.Assign(Text);
  end else begin
    inherited AssignTo(ADest);
  end;
end;

procedure TIdReply.Clear;
begin
  FText.Clear;
  FCode := '';
end;

constructor TIdReply.CreateWithReplyTexts(ACollection: TCollection; AReplyTexts: TIdReplies);
begin
  inherited Create(ACollection);
  FReplyTexts := AReplyTexts;
  CommonInit;
end;

constructor TIdReply.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  CommonInit;
end;

destructor TIdReply.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FFormattedReply);
  inherited Destroy;
end;

procedure TIdReply.CommonInit;
begin
  FFormattedReply := TStringList.Create;
  FText := TStringList.Create;
end;

function TIdReply.GetDisplayName: string;
begin
  if Text.Count > 0 then begin
    Result := Code + ' ' + Text[0];
  end else begin
    Result := Code;
  end;
end;

function TIdReply.ReplyExists: Boolean;
begin
  Result := Code <> '';
end;

procedure TIdReply.SetNumericCode(const AValue: Integer);
begin
  Code := IntToStr(AValue);
end;

procedure TIdReply.SetText(const AValue: TStrings);
begin
  FText.Assign(AValue);
end;

procedure TIdReply.SetReply(const ACode: Integer; const AText: string);
begin
  SetReply(IntToStr(ACode), AText);
end;

function TIdReply.GetNumericCode: Integer;
begin
  Result := IndyStrToInt(Code, 0);
end;

procedure TIdReply.SetCode(const AValue: string);
var
  LMatchedReply: TIdReply;
begin
  if FCode <> AValue then begin
    if not CheckIfCodeIsValid(AValue) then begin
      raise EIdException.CreateFmt(RSReplyInvalidCode, [AValue]);
    end;
    // Only check for duplicates if we are in a collection. NormalReply etc are not in collections
    // Also dont check FReplyTexts, as non members can be duplicates of members
    if Collection <> nil then begin
      LMatchedReply := TIdReplies(Collection).Find(AValue);
      if Assigned(LMatchedReply) then begin
        raise EIdException.CreateFmt(RSReplyCodeAlreadyExists, [AValue]);
      end;
    end;
    Clear;
    FCode := AValue;
  end;
end;

procedure TIdReply.SetReply(const ACode, AText: string);
begin
  Code := ACode;
  FText.Text := AText;
end;

function TIdReply.CheckIfCodeIsValid(const ACode: string): Boolean;
begin
  Result := True;
end;

class function TIdReply.IsEndMarker(const ALine: string): Boolean;
begin
  Result := False;
end;

function TIdReply.GetFormattedReply: TStrings;
begin
  // Overrides must call GetFormattedReplyStrings instead. This is just a base implementation
  // This is done this way because otherwise double generations can occur if more than one
  // ancestor overrides. Example: Reply--> RFC --> FTP. Calling inherited would cause both
  // FTP and RFC to generate.
  Result := GetFormattedReplyStrings;
end;

function TIdReply.GetFormattedReplyStrings: TStrings;
begin
  FFormattedReply.Clear;
  Result := FFormattedReply;
end;

procedure TIdReply.UpdateText;
begin
  if FReplyTexts <> nil then begin
    FReplyTexts.UpdateText(Self);
  end;
end;

{ TIdReplies }

function TIdReplies.Add: TIdReply;
begin
  Result := TIdReply(inherited Add);
end;

function TIdReplies.Add(const ACode: Integer; const AText: string): TIdReply;
begin
  Result := Add(IntToStr(ACode), AText);
end;

function TIdReplies.Add(const ACode, AText: string): TIdReply;
begin
  Result := Add;
  try
    Result.SetReply(ACode, AText);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TIdReplies.Create(AOwner: TPersistent; const AReplyClass: TIdReplyClass);
begin
  inherited Create(AOwner, AReplyClass);
end;

function TIdReplies.Find(const ACode: string; AIgnore: TIdReply = nil): TIdReply;
var
  i: Integer;
begin
  Result := nil;
  // Never return match on ''
  if ACode <> '' then begin
    for i := 0 to Count - 1 do begin
      if Items[i].Code = ACode then begin
        if not (Items[i] = AIgnore) then begin
          Result := Items[i];
          Exit;
        end;
      end;
    end;
  end;
end;

function TIdReplies.GetItem(Index: Integer): TIdReply;
begin
  Result := TIdReply(inherited Items[Index]);
end;

procedure TIdReplies.SetItem(Index: Integer; const Value: TIdReply);
begin
  inherited SetItem(Index, Value);
end;

procedure TIdReplies.UpdateText(AReply: TIdReply);
var
  LReply: TIdReply;
begin
  // If text is blank, get it from the ReplyTexts
  if AReply.Text.Count = 0 then begin
    // RLebeau - ignore AReply, it doesn't have any text
    // to assign, or else the code wouldn't be this far
    LReply := Find(AReply.Code, AReply);
    if LReply <> nil then begin
      AReply.Text.Assign(LReply.Text);
    end;
  end;
end;

end.

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
  Rev 1.29    1/15/05 2:28:28 PM  RLebeau
  Added local variables to TIdReplyRFC.GetFormattedReply() to reduce the number
  of repeated string operations that were being performed.

  Updated TIdRepliesRFC.UpdateText() to ignore the TIdReply that was passed in
  when looking for a TIdReply to extract Text from.

  Rev 1.28    10/26/2004 8:43:00 PM  JPMugaas
  Should be more portable with new references to TIdStrings and TIdStringList.

  Rev 1.27    6/11/2004 8:48:28 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.26    18/05/2004 23:17:18  CCostelloe
  Bug fix

  Rev 1.25    5/18/04 2:39:02 PM  RLebeau
  Added second constructor to TIdRepliesRFC

  Rev 1.24    5/17/04 9:50:08 AM  RLebeau
  Changed TIdRepliesRFC constructor to use 'reintroduce' instead

  Rev 1.23    5/16/04 5:12:04 PM  RLebeau
  Added construvtor to TIdRepliesRFC class

  Rev 1.22    2004.03.01 5:12:36 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.21    2004.02.29 8:17:20 PM  czhower
  Minor cosmetic changes to code.

  Rev 1.20    2004.02.03 4:16:50 PM  czhower
  For unit name changes.

  Rev 1.19    1/3/2004 8:06:18 PM  JPMugaas
  Bug fix:  Sometimes, replies will appear twice due to the way functionality
  was enherited.

  Rev 1.18    2003.10.18 9:33:28 PM  czhower
  Boatload of bug fixes to command handlers.

  Rev 1.17    9/20/2003 10:01:04 AM  JPMugaas
  Minor change.  WIll now accept all 3 digit numbers (not just ones below 600).
  The reason is that developers may want something in 600-999 range.  RFC 2228
  defines a 6xx reply range for protected replies.

  Rev 1.16    2003.09.20 10:33:14 AM  czhower
  Bug fix to allow clearing code field (Return to default value)

  Rev 1.15    2003.06.05 10:08:52 AM  czhower
  Extended reply mechanisms to the exception handling. Only base and RFC
  completed, handing off to J Peter.

  Rev 1.14    6/3/2003 04:09:30 PM  JPMugaas
  class function TIdReplyRFC.IsEndMarker(const ALine: string): Boolean had the
  wrong parameters causing FTP to freeze.  It probably effected other stuff.

  Rev 1.13    5/30/2003 8:37:42 PM  BGooijen
  Changed virtual to override

  Rev 1.12    2003.05.30 10:25:58 PM  czhower
  Implemented IsEndMarker

  Rev 1.11    2003.05.30 10:06:08 PM  czhower
  Changed code property mechanisms.

  Rev 1.10    2003.05.26 10:48:12 PM  czhower
  1) Removed deprecated code.
  2) Removed POP3 bastardizations as they are now in IdReplyPOP3.

  Rev 1.9    5/26/2003 12:19:52 PM  JPMugaas

  Rev 1.8    2003.05.26 11:38:20 AM  czhower

  Rev 1.7    5/25/2003 03:16:54 AM  JPMugaas

  Rev 1.6    2003.05.25 10:23:46 AM  czhower

  Rev 1.5    5/21/2003 08:43:38 PM  JPMugaas
  Overridable hook for the SMTP Reply object.

  Rev 1.4    5/20/2003 12:43:48 AM  BGooijen
  changeable reply types

  Rev 1.3    5/19/2003 12:26:50 PM  JPMugaas
  Now uses base class.

  Rev 1.2    11/05/2003 23:29:04  CCostelloe
  IMAP-specific code moved up to TIdIMAP4.pas

  Rev 1.1    11/14/2002 02:51:54 PM  JPMugaas
  Added FormatType property.  If it is rfIndentMidLines, it will accept
  properly parse reply lines that begin with a space.  Setting this to
  rfIndentMidLines will also cause the reply object to generate lines that
  start with a space if the Text.Line starts with a space.  This should
  accommodate the FTP MLSD and FEAT commands on both the client and server.

  Rev 1.0    11/13/2002 08:45:50 AM  JPMugaas
}

unit IdReplyRFC;

interface
{$I IdCompilerDefines.inc}
uses
  Classes,
  IdReply;

type
  TIdReplyRFC = class(TIdReply)
  protected
    procedure AssignTo(ADest: TPersistent); override;
    function CheckIfCodeIsValid(const ACode: string): Boolean; override;
    function GetFormattedReply: TStrings; override;
    procedure SetFormattedReply(const AValue: TStrings); override;
  public
    class function IsEndMarker(const ALine: string): Boolean; override;
    procedure RaiseReplyError; override;
    function ReplyExists: Boolean; override;
  end;

  TIdRepliesRFC = class(TIdReplies)
  public
    constructor Create(AOwner: TPersistent); reintroduce; overload; virtual;
    constructor Create(AOwner: TPersistent; const AReplyClass: TIdReplyClass); overload; override;
    procedure UpdateText(AReply: TIdReply); override;
  end;

  // This exception is for protocol errors such as 404 HTTP error and also
  // SendCmd / GetResponse
  EIdReplyRFCError = class(EIdReplyError)
  protected
    FErrorCode: Integer;
  public
    // Params must be in this order to avoid conflict with CreateHelp
    // constructor in CBuilder as CB does not differentiate constructors
    // by name as Delphi does
    constructor CreateError(const AErrorCode: Integer;
     const AReplyMessage: string); reintroduce; virtual;
    //
    property ErrorCode: Integer read FErrorCode;
  end;

implementation

uses
  IdGlobal,
  SysUtils;

{ TIdReplyRFC }

procedure TIdReplyRFC.AssignTo(ADest: TPersistent);
var
  LR: TIdReplyRFC;
begin
  if ADest is TIdReplyRFC then begin
    LR := TIdReplyRFC(ADest);
    //set code first as it possibly clears the reply
    LR.NumericCode := NumericCode;
    LR.Text.Assign(Text);
  end else begin
    inherited AssignTo(ADest);
  end;
end;

function TIdReplyRFC.CheckIfCodeIsValid(const ACode: string): Boolean;
var
  LCode: Integer;
begin
  LCode := IndyStrToInt(ACode, 0);
  {Replaced 600 with 999 because some developers may want 6xx, 7xx, and 8xx reply
  codes for their protocols.  It also turns out that RFC 2228 defines 6xx reply codes.

  From RFC 2228

   A new class of reply types (6yz) is also introduced for protected
   replies.
  }
  Result := ((LCode >= 100) and (LCode < 1000)) or (Trim(ACode) = '');
end;

function TIdReplyRFC.GetFormattedReply: TStrings;
var
  I, LCode: Integer;
  LCodeStr: String;
begin
  Result := GetFormattedReplyStrings;
  LCode := NumericCode;
  if LCode > 0 then begin
    LCodeStr := IntToStr(LCode);
    if Text.Count > 0 then begin
      for I := 0 to Text.Count - 1 do begin
        if I < Text.Count - 1 then begin
          Result.Add(LCodeStr + '-' + Text[I]);
        end else begin
          Result.Add(LCodeStr + ' ' + Text[I]);
        end;
      end;
    end else begin
      Result.Add(LCodeStr);
    end;
  end else if FText.Count > 0 then begin
    Result.AddStrings(FText);
  end;
end;

class function TIdReplyRFC.IsEndMarker(const ALine: string): Boolean;
begin
  if Length(ALine) >= 4 then begin
    Result := ALine[4] = ' ';
  end else begin
    Result := True;
  end;
end;

procedure TIdReplyRFC.RaiseReplyError;
begin
  raise EIdReplyRFCError.CreateError(NumericCode, Text.Text);
end;

function TIdReplyRFC.ReplyExists: Boolean;
begin
  Result := (NumericCode > 0) or (FText.Count > 0);
end;

procedure TIdReplyRFC.SetFormattedReply(const AValue: TStrings);
// Just parse and put in items, no need to store after parse
var
  i: Integer;
  s: string;
begin
  Clear;
  if AValue.Count > 0 then begin
    s := Trim(Copy(AValue[0], 1, 3));
    Code := s;
    for i := 0 to AValue.Count - 1 do begin
      Text.Add(Copy(AValue[i], 5, MaxInt));
    end;
  end;
end;

{ EIdReplyRFCError }

constructor EIdReplyRFCError.CreateError(const AErrorCode: Integer;
 const AReplyMessage: string);
begin
  inherited Create(AReplyMessage);
  FErrorCode := AErrorCode;
end;

{ TIdReplies }

constructor TIdRepliesRFC.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TIdReplyRFC);
end;

constructor TIdRepliesRFC.Create(AOwner: TPersistent; const AReplyClass: TIdReplyClass);
begin
  inherited Create(AOwner, AReplyClass);
end;

procedure TIdRepliesRFC.UpdateText(AReply: TIdReply);
var
  LGenericNumCode: Integer;
  LReply: TIdReply;
begin
  inherited UpdateText(AReply);
  // If text is still blank after inherited see if we can find a generic version
  if AReply.Text.Count = 0 then begin
    LGenericNumCode := (AReply.NumericCode div 100) * 100;
    // RLebeau - in cases where the AReply.Code is the same as the
    // generic code, ignore the AReply as it doesn't have any text
    // to assign, or else the code wouldn't be this far
    LReply := Find(IntToStr(LGenericNumCode), AReply);
    if LReply = nil then begin
      // If no generic was found, then use defaults.
      case LGenericNumCode of
        100: AReply.Text.Text := 'Information';            {do not localize}
        200: AReply.Text.Text := 'Ok';                     {do not localize}
        300: AReply.Text.Text := 'Temporary Error';        {do not localize}
        400: AReply.Text.Text := 'Permanent Error';        {do not localize}
        500: AReply.Text.Text := 'Unknown Internal Error'; {do not localize}
      end;
    end else begin
      AReply.Text.Assign(LReply.Text);
    end;
  end;
end;

end.

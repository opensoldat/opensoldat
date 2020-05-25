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
  Rev 1.6    7/23/04 6:40:08 PM  RLebeau
  Added extra exception handling to Connect()

  Rev 1.5    2004.05.20 11:39:10 AM  czhower
  IdStreamVCL

  Rev 1.4    2004.02.03 4:17:18 PM  czhower
  For unit name changes.

  Rev 1.3    10/19/2003 11:38:26 AM  DSiders
  Added localization comments.

  Rev 1.2    2003.10.18 1:56:46 PM  czhower
  Now uses ASCII instead of binary format.

  Rev 1.1    2003.10.17 6:16:20 PM  czhower
  Functional complete.
}

unit IdInterceptSimLog;

{
  This file uses string outputs instead of binary so that the results can be
  viewed and modified with notepad if necessary.

  Most times a Send/Receive includes a writeln, but may not always. We write out
  an additional EOL to guarantee separation in notepad.

  It also auto detects when an EOL can be used instead.

  TODO: Can also change it to detect several EOLs and non binary and use :Lines:x
}

interface
{$i IdCompilerDefines.inc}

uses
  Classes,
  IdGlobal, IdIntercept, IdBaseComponent;

type
  TIdInterceptSimLog = class(TIdConnectionIntercept)
  private
  protected
    FFilename: string;
    FStream: TStream;
    //
    procedure SetFilename(const AValue: string);
    procedure WriteRecord(const ATag: string; const ABuffer: TIdBytes);
  public
    procedure Connect(AConnection: TComponent); override;
    procedure Disconnect; override;
    procedure Receive(var ABuffer: TIdBytes); override;
    procedure Send(var ABuffer: TIdBytes); override;
  published
    property Filename: string read FFilename write SetFilename;
  end;

implementation

uses
  {$IFDEF DOTNET}
  IdStreamNET,
    {$ELSE}
  IdStreamVCL,
  {$ENDIF}
  IdException, IdResourceStringsCore, SysUtils;

{ TIdInterceptSimLog }

procedure TIdInterceptSimLog.Connect(AConnection: TComponent);
begin
  inherited Connect(AConnection);
  // Warning! This will overwrite any existing file. It makes no sense
  // to concatenate sim logs.
  FStream := TIdFileCreateStream.Create(Filename);
end;

procedure TIdInterceptSimLog.Disconnect;
begin
  FreeAndNil(FStream);
  inherited Disconnect;
end;

procedure TIdInterceptSimLog.Receive(var ABuffer: TIdBytes);
begin
  // let the next Intercept in the chain decode its data first
  inherited Receive(ABuffer);
  WriteRecord('Recv', ABuffer); {do not localize}
end;

procedure TIdInterceptSimLog.Send(var ABuffer: TIdBytes);
begin
  WriteRecord('Send', ABuffer); {do not localize}
  // let the next Intercept in the chain encode its data next
  inherited Send(ABuffer);
end;

procedure TIdInterceptSimLog.SetFilename(const AValue: string);
begin
  if Assigned(FStream) then begin
    raise EIdException.Create(RSLogFileAlreadyOpen);
  end;
  FFilename := AValue;
end;

procedure TIdInterceptSimLog.WriteRecord(const ATag: string; const ABuffer: TIdBytes);
var
  i: Integer;
  LUseEOL: Boolean;
  LSize: Integer;
begin
  LUseEOL := False;
  LSize := Length(ABuffer);
  if LSize > 1 then begin
    if (ABuffer[LSize - 2] = 13) and (ABuffer[LSize - 1] = 10) then begin
      LUseEOL := True;
      for i := 0 to LSize - 3 do begin
        // If any binary, CR or LF
        if (ABuffer[i] < 32) or (ABuffer[i] > 127) then begin
          LUseEOL := False;
          Break;
        end;
      end;
    end;
  end;
  with FStream do begin
    if LUseEOL then begin
      WriteLn(ATag + ':EOL'); {do not localize}
    end else begin
      WriteLn(ATag + ':Bytes:' + IntToStr(LSize));  {do not localize}
    end;
  end;
  WriteStringToStream(FStream, '');
  WriteTIdBytesToStream(FStream, ABuffer, LSize);
  WriteStringToStream(FStream, EOL);
end;

end.

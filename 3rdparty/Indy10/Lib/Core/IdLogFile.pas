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
  Rev 1.8    7/23/04 6:36:54 PM  RLebeau
  Added extra exception handling to Open()

  Rev 1.7    2004.05.20 12:34:30 PM  czhower
  Removed more non .NET compatible stream read and writes

  Rev 1.6    2004.02.03 4:17:16 PM  czhower
  For unit name changes.

  Rev 1.5    2003.10.17 6:15:54 PM  czhower
  Upgrades

  Rev 1.4    2003.10.16 11:24:36 AM  czhower
  Bug fix

  Rev 1.3    10/15/2003 8:00:10 PM  DSiders
  Added resource string for exception raised in TIdLogFile.SetFilename.

  Rev 1.2    2003.10.14 1:27:10 PM  czhower
  Uupdates + Intercept support

  Rev 1.1    6/16/2003 11:01:06 AM  EHill
  Throw exception if the filename is set while the log is open.
  Expose Open and Close as public instead of protected.

  Rev 1.0    11/13/2002 07:56:12 AM  JPMugaas

  19-Aug-2001 DSiders
  Fixed bug in Open.  Use file mode fmCreate when Filename does *not* exist.

  19-Aug-2001 DSiders
  Added protected method TIdLogFile.LogWriteString.

  19-Aug-2001 DSiders
  Changed implementation of TIdLogFile methods LogStatus, LogReceivedData, and
  LogSentData to use LogWriteString.

  19-Aug-2001 DSiders
  Added class TIdLogFileEx with the LogFormat method.
}

unit IdLogFile;

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  Classes,
  IdLogBase;

type
  TIdLogFile = class(TIdLogBase)
  protected
    FFilename: String;
    FFileStream: TStream;
    //
    procedure LogFormat(const AFormat: string; const AArgs: array of const); virtual;
    procedure LogReceivedData(const AText, AData: string); override;
    procedure LogSentData(const AText, AData: string); override;
    procedure LogStatus(const AText: string); override;
    procedure LogWriteString(const AText: string); virtual;
    //
    procedure SetFilename(const AFilename: String);
  public
    procedure Open; override;
    procedure Close; override;
  published
    property Filename: String read FFilename write SetFilename;
  end;

implementation

uses
  IdGlobal, IdException, IdResourceStringsCore, IdBaseComponent, SysUtils;

{ TIdLogFile }

procedure TIdLogFile.Close;
begin
  FreeAndNil(FFileStream);
end;

procedure TIdLogFile.LogReceivedData(const AText, AData: string);
begin
  LogWriteString(RSLogRecv + AText + ': ' + AData + EOL);  {Do not translate}
end;

procedure TIdLogFile.LogSentData(const AText, AData: string);
begin
  LogWriteString(RSLogSent + AText + ': ' + AData + EOL);  {Do not translate}
end;

procedure TIdLogFile.LogStatus(const AText: string);
begin
  LogWriteString(RSLogStat + AText + EOL);
end;

procedure TIdLogFile.Open;
begin
  if not IsDesignTime then begin
    FFileStream := TIdAppendFileStream.Create(Filename);
  end;
end;

procedure TIdLogFile.LogWriteString(const AText: string);
var
  LEncoding: IIdTextEncoding;
begin
  if Assigned(FFileStream) then begin
    LEncoding := IndyTextEncoding_8Bit;
    WriteStringToStream(FFileStream, AText, LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF});
  end;
end;

procedure TIdLogFile.LogFormat(const AFormat: string; const AArgs: array of const);
var
  sPre: string;
  sMsg: string;
  sData: string;
begin
  // forces Open to be called prior to Connect
  if not Active then begin
    Active := True;
  end;

  sPre := '';   {Do not translate}
  sMsg := '';   {Do not translate}

  if LogTime then begin
    sPre := DateTimeToStr(Now) + ' ';      {Do not translate}
  end;

  sData := IndyFormat(AFormat, AArgs);
  if FReplaceCRLF then begin
    sData := ReplaceCR(sData);
  end;
  sMsg := sPre + sData + EOL;

  LogWriteString(sMsg);
end;

procedure TIdLogFile.SetFilename(const AFilename: String);
begin
  if Assigned(FFileStream) then begin
    raise EIdException.Create(RSLogFileAlreadyOpen);
  end;
  FFilename := AFilename;
end;

end.


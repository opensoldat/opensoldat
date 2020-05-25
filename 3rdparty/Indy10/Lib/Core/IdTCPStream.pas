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
  Rev 1.13    27.08.2004 21:58:22  Andreas Hausladen
  Speed optimization ("const" for string parameters)

  Rev 1.12    29/05/2004 21:17:48  CCostelloe
  ReadLnSplit added, needed for binary attachments

  Rev 1.11    28/05/2004 20:30:12  CCostelloe
  Bug fix

  Rev 1.10    2004.05.21 8:22:16 PM  czhower
  Added ReadLn

  Rev 1.9    2004.05.20 1:40:00 PM  czhower
  Last of the IdStream updates

  Rev 1.8    2004.03.07 11:48:46 AM  czhower
  Flushbuffer fix + other minor ones found

  Rev 1.7    2004.02.03 4:16:58 PM  czhower
  For unit name changes.

  Rev 1.6    5/12/2003 9:17:58 AM  GGrieve
  remove dead code

  Rev 1.5    5/12/2003 12:32:14 AM  GGrieve
  Refactor to work under DotNet

  Rev 1.4    10/10/2003 11:04:24 PM  BGooijen
  DotNet

  Rev 1.3    9/10/2003 1:50:50 PM  SGrobety
  DotNet

  Rev 1.2    2003.10.01 11:16:38 AM  czhower
  .Net

  Rev 1.1    2003.10.01 1:37:36 AM  czhower
  .Net

  Rev 1.0    11/13/2002 09:01:04 AM  JPMugaas
}

unit IdTCPStream;

interface

{$I IdCompilerDefines.inc}
//TODO: This should be renamed to IdStreamTCP for consistency, and class too

uses
  Classes,
  IdGlobal, IdTCPConnection;

type
  TIdTCPStream = class(TIdBaseStream)
  protected
    FConnection: TIdTCPConnection;
    FWriteThreshold: Integer;
    FWriteBuffering: Boolean;
    function IdRead(var VBuffer: TIdBytes; AOffset, ACount: Longint): Longint; override;
    function IdWrite(const ABuffer: TIdBytes; AOffset, ACount: Longint): Longint; override;
    function IdSeek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    procedure IdSetSize(ASize: Int64); override;
  public
    constructor Create(AConnection: TIdTCPConnection; const AWriteThreshold: Integer = 0); reintroduce;
    destructor Destroy; override;
    property Connection: TIdTCPConnection read FConnection;
  end;

implementation

uses
  IdException,
  SysUtils;

constructor TIdTCPStream.Create(AConnection: TIdTCPConnection; const AWriteThreshold: Integer = 0);
begin
  inherited Create;
  FConnection := AConnection;
  FWriteThreshold := AWriteThreshold;
end;

destructor TIdTCPStream.Destroy;
begin
  if FWriteBuffering then begin
    Connection.IOHandler.WriteBufferClose;
  end;
  inherited Destroy;
end;

function TIdTCPStream.IdRead(var VBuffer: TIdBytes; AOffset, ACount: Longint): Longint;
begin
  if AOffset <> 0 then begin
    ToDo('IdRead() method of TIdTCPStream class does not support seeking'); {do not localized}
  end;
  Connection.IOHandler.ReadBytes(VBuffer, ACount, False);
  Result := ACount;
end;

function TIdTCPStream.IdSeek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

procedure TIdTCPStream.IdSetSize(ASize: Int64);
begin
//
end;

function TIdTCPStream.IdWrite(const ABuffer: TIdBytes; AOffset, ACount: Longint): Longint;
begin
  if (not FWriteBuffering) and (FWriteThreshold > 0) and (not Connection.IOHandler.WriteBufferingActive) then begin
    Connection.IOHandler.WriteBufferOpen(FWriteThreshold);
    FWriteBuffering := True;
  end;
  Connection.IOHandler.Write(ABuffer, ACount, AOffset);
  Result := ACount;
end;

end.



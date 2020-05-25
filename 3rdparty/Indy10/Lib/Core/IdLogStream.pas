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
  Rev 1.5    2004.05.20 12:34:32 PM  czhower
  Removed more non .NET compatible stream read and writes

  Rev 1.4    2004.01.20 10:03:30 PM  czhower
  InitComponent

  Rev 1.3    2003.10.17 6:15:56 PM  czhower
  Upgrades

  Rev 1.2    2003.10.17 4:28:54 PM  czhower
  Changed stream names to be consistent with IOHandlerStream

  Rev 1.1    2003.10.14 1:27:12 PM  czhower
  Uupdates + Intercept support

  Rev 1.0    11/13/2002 07:56:18 AM  JPMugaas
}

unit IdLogStream;

interface
{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode
uses
  Classes,
  IdLogBase, IdGlobal;

type
  TIdLogStream = class(TIdLogBase)
  protected
    FFreeStreams: Boolean;
    FReceiveStream: TStream;
    FSendStream: TStream;
    //
    procedure InitComponent; override;
    procedure LogStatus(const AText: string); override;
    procedure LogReceivedData(const AText, AData: string); override;
    procedure LogSentData(const AText, AData: string); override;
  public
    procedure Disconnect; override;
    //
    property FreeStreams: Boolean read FFreeStreams write FFreeStreams;
    property ReceiveStream: TStream read FReceiveStream write FReceiveStream;
    property SendStream: TStream read FSendStream write FSendStream;
  end;

implementation
 uses SysUtils;

// TODO: This was orginally for VCL. For .Net what do we do? Convert back to
// 7 bit? Log all? Logging all seems to be a disaster.
// Text seems to be best, users are expecting text in this class. But
// this write stream will dump unicode out in .net.....
// So just convert it again back to 7 bit? How is proper to write
// 7 bit to file? Use AnsiString?

{ TIdLogStream }

procedure TIdLogStream.Disconnect;
begin
  inherited Disconnect;
  if FreeStreams then begin
    FreeAndNil(FReceiveStream);
    FreeAndNil(FSendStream);
  end;
end;

procedure TIdLogStream.InitComponent;
begin
  inherited InitComponent;
  FFreeStreams := True;
end;

procedure TIdLogStream.LogReceivedData(const AText, AData: string);
var
  LEncoding: IIdTextEncoding;
begin
  if FReceiveStream <> nil then begin
    LEncoding := IndyTextEncoding_8Bit;
    WriteStringToStream(FReceiveStream, AData, LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF});
  end;
end;

procedure TIdLogStream.LogSentData(const AText, AData: string);
var
  LEncoding: IIdTextEncoding;
begin
  if FSendStream <> nil then begin
    LEncoding := IndyTextEncoding_8Bit;
    WriteStringToStream(FSendStream, AData, LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF});
  end;
end;

procedure TIdLogStream.LogStatus(const AText: string);
begin
  // We just leave this empty because the AText is not part of the stream and we
  // do not want to raise an abstract method exception.
end;

end.

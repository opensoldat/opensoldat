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
  Rev 1.21    3/10/05 3:24:30 PM  RLebeau
  Updated ReadFromSource() and WriteDirect() to access the Intercept property
  directly.

  Rev 1.20    10/21/2004 11:07:30 PM  BGooijen
  works in win32 now too

  Rev 1.19    10/21/2004 1:52:56 PM  BGooijen
  Raid 214235

  Rev 1.18    7/23/04 6:20:52 PM  RLebeau
  Removed memory leaks in Send/ReceiveStream property setters

  Rev 1.17    2004.05.20 11:39:08 AM  czhower
  IdStreamVCL

  Rev 1.16    23/04/2004 20:29:36  CCostelloe
  Minor change to support IdMessageClient's new TIdIOHandlerStreamMsg

  Rev 1.15    2004.04.16 11:30:32 PM  czhower
  Size fix to IdBuffer, optimizations, and memory leaks

  Rev 1.14    2004.04.08 3:56:36 PM  czhower
  Fixed bug with Intercept byte count. Also removed Bytes from Buffer.

  Rev 1.13    2004.03.07 11:48:46 AM  czhower
  Flushbuffer fix + other minor ones found

  Rev 1.12    2004.03.03 11:55:04 AM  czhower
  IdStream change

  Rev 1.11    2004.02.03 4:17:16 PM  czhower
  For unit name changes.

  Rev 1.10    11/01/2004 19:52:44  CCostelloe
  Revisions for TIdMessage SaveToFile & LoadFromFile for D7 & D8

  Rev 1.8    08/01/2004 23:37:16  CCostelloe
  Minor changes

  Rev 1.7    1/8/2004 1:01:22 PM  BGooijen
  Cleaned up

  Rev 1.6    1/8/2004 4:23:06 AM  BGooijen
  temp fixed TIdIOHandlerStream.WriteToDestination

  Rev 1.5    08/01/2004 00:25:22  CCostelloe
  Start of reimplementing LoadFrom/SaveToFile

  Rev 1.4    2003.12.31 7:44:54 PM  czhower
  Matched constructors visibility to ancestor.

  Rev 1.3    2003.10.24 10:44:54 AM  czhower
  IdStream implementation, bug fixes.

  Rev 1.2    2003.10.14 11:19:14 PM  czhower
  Updated for better functionality.

  Rev 1.1    2003.10.14 1:27:14 PM  czhower
  Uupdates + Intercept support

  Rev 1.0    2003.10.13 6:40:40 PM  czhower
  Moved from root

  Rev 1.9    2003.10.11 10:00:36 PM  czhower
  Compiles again.

  Rev 1.8    10/10/2003 10:53:42 PM  BGooijen
  Changed const-ness of some methods to reflect base class changes

  Rev 1.7    7/10/2003 6:07:58 PM  SGrobety
  .net

  Rev 1.6    17/07/2003 00:01:24  CCostelloe
  Added (empty) procedures for the base classes' abstract CheckForDataOnSource
  and CheckForDisconnect

    Rev 1.5    7/1/2003 12:45:56 PM  BGooijen
  changed FInputBuffer.Size := 0 to FInputBuffer.Clear

  Rev 1.4    12-8-2002 21:05:28  BGooijen
  Removed call to Close in .Destroy, this is already done in
  TIdIOHandler.Destroy

  Rev 1.3    12/7/2002 06:42:44 PM  JPMugaas
  These should now compile except for Socks server.  IPVersion has to be a
  property someplace for that.

  Rev 1.2    12/5/2002 02:53:52 PM  JPMugaas
  Updated for new API definitions.

  Rev 1.1    05/12/2002 15:29:16  AO'Neill

  Rev 1.0    11/13/2002 07:55:08 AM  JPMugaas
}

unit IdIOHandlerStream;

interface

{$I IdCompilerDefines.inc}

uses
  Classes,
  IdBaseComponent,
  IdGlobal,
  IdIOHandler,
  IdStream;

type
  TIdIOHandlerStream = class;
  TIdIOHandlerStreamType = (stRead, stWrite, stReadWrite);
  TIdOnGetStreams = procedure(ASender: TIdIOHandlerStream;
    var VReceiveStream: TStream; var VSendStream: TStream) of object;

  TIdIOHandlerStream = class(TIdIOHandler)
  protected
    FFreeStreams: Boolean;
    FOnGetStreams: TIdOnGetStreams;
    FReceiveStream: TStream;
    FSendStream: TStream;
    FStreamType: TIdIOHandlerStreamType;
    //
    procedure InitComponent; override;
    function ReadDataFromSource(var VBuffer: TIdBytes): Integer; override;
    function WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer; override;
    function SourceIsAvailable: Boolean; override;
    function CheckForError(ALastResult: Integer): Integer; override;
    procedure RaiseError(AError: Integer); override;
  public
    function StreamingAvailable: Boolean;
    procedure CheckForDisconnect(ARaiseExceptionIfDisconnected: Boolean = True;
      AIgnoreBuffer: Boolean = False); override;
    constructor Create(AOwner: TComponent; AReceiveStream: TStream; ASendStream: TStream = nil); reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); reintroduce; overload;
    function Connected: Boolean; override;
    procedure Close; override;
    procedure Open; override;
    function Readable(AMSec: integer = IdTimeoutDefault): boolean; override;
    //
    property ReceiveStream: TStream read FReceiveStream;
    property SendStream: TStream read FSendStream;
    property StreamType: TIdIOHandlerStreamType read FStreamType;
  published
    property FreeStreams: Boolean read FFreeStreams write FFreeStreams default True;
    //
    property OnGetStreams: TIdOnGetStreams read FOnGetStreams write FOnGetStreams;
  end;

implementation

uses
  IdException, IdComponent, SysUtils;

{ TIdIOHandlerStream }

procedure TIdIOHandlerStream.InitComponent;
begin
  inherited InitComponent;
  FDefStringEncoding := IndyTextEncoding_8Bit;
end;

procedure TIdIOHandlerStream.CheckForDisconnect(
  ARaiseExceptionIfDisconnected: Boolean = True;
  AIgnoreBuffer: Boolean = False);
var
  LDisconnected: Boolean;
begin
  // ClosedGracefully // Server disconnected
  // IOHandler = nil // Client disconnected
  if ClosedGracefully then begin
    if StreamingAvailable then begin
      Close;
      // Call event handlers to inform the user that we were disconnected
      DoStatus(hsDisconnected);
      //DoOnDisconnected;
    end;
    LDisconnected := True;
  end else begin
    LDisconnected := not StreamingAvailable;
  end;
  // Do not raise unless all data has been read by the user
  if LDisconnected then begin
    if (InputBufferIsEmpty or AIgnoreBuffer) and ARaiseExceptionIfDisconnected then begin
      RaiseConnClosedGracefully;
    end;
  end;
end;

procedure TIdIOHandlerStream.Close;
begin
  inherited Close;
  if FreeStreams then begin
    FreeAndNil(FReceiveStream);
    FreeAndNil(FSendStream);
  end else begin
    FReceiveStream := nil;
    FSendStream := nil;
  end;
end;

function TIdIOHandlerStream.StreamingAvailable: Boolean;
begin
  Result := False;  // Just to avoid warning message
  case FStreamType of
    stRead: Result := Assigned(ReceiveStream);
    stWrite: Result := Assigned(SendStream);
    stReadWrite: Result := Assigned(ReceiveStream) and Assigned(SendStream);
  end;
end;

function TIdIOHandlerStream.Connected: Boolean;
begin
  Result := (StreamingAvailable and inherited Connected) or (not InputBufferIsEmpty);
end;

constructor TIdIOHandlerStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFreeStreams := True;
  FStreamType := stReadWrite;
end;

constructor TIdIOHandlerStream.Create(AOwner: TComponent; AReceiveStream: TStream;
  ASendStream: TStream = nil);
begin
  inherited Create(AOwner);
  //
  FFreeStreams := True;
  FReceiveStream := AReceiveStream;
  FSendStream := ASendStream;
  //
  if Assigned(FReceiveStream) and (not Assigned(FSendStream)) then begin
    FStreamType := stRead;
  end else if (not Assigned(FReceiveStream)) and Assigned(FSendStream) then begin
    FStreamType := stWrite;
  end else begin
    FStreamType := stReadWrite;
  end;
end;

procedure TIdIOHandlerStream.Open;
begin
  inherited Open;
  if Assigned(OnGetStreams) then begin
    OnGetStreams(Self, FReceiveStream, FSendStream);
  end;
  if Assigned(FReceiveStream) and (not Assigned(FSendStream)) then begin
    FStreamType := stRead;
  end else if (not Assigned(FReceiveStream)) and Assigned(FSendStream) then begin
    FStreamType := stWrite;
  end else begin
    FStreamType := stReadWrite;
  end;
end;

function TIdIOHandlerStream.Readable(AMSec: Integer): Boolean;
begin
  Result := Assigned(ReceiveStream);
  // RLebeau: not checking the Position anymore. Was
  // causing deadlocks when trying to read past EOF.
  // This way, when EOF is reached, ReadFromSource()
  // will return 0, which will be interpretted as the
  // connnection being closed...
  {
  if Result then begin
    Result := ReceiveStream.Position < ReceiveStream.Size;
  end;
  }
end;

function TIdIOHandlerStream.ReadDataFromSource(var VBuffer: TIdBytes): Integer;
begin
  // We dont want to read the whole stream in at a time. If its a big
  // file will consume way too much memory by loading it all at once.
  // So lets read it in chunks.
  if Assigned(FReceiveStream) then begin
    Result := IndyMin(32 * 1024, Length(VBuffer));
    if Result > 0 then begin
      Result := TIdStreamHelper.ReadBytes(FReceiveStream, VBuffer, Result);
    end;
  end else begin
    Result := 0;
  end;
end;

function TIdIOHandlerStream.WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer;
begin
  if Assigned(FSendStream) then begin
    Result := TIdStreamHelper.Write(FSendStream, ABuffer, ALength, AOffset);
  end else begin
    Result := IndyLength(ABuffer, ALength, AOffset);
  end;
end;

function TIdIOHandlerStream.SourceIsAvailable: Boolean;
begin
  Result := Assigned(ReceiveStream);
end;

function TIdIOHandlerStream.CheckForError(ALastResult: Integer): Integer;
begin
  Result := ALastResult;
  if Result < 0 then begin
    raise EIdException.Create('Stream error'); {do not localize}
  end;
end;

procedure TIdIOHandlerStream.RaiseError(AError: Integer);
begin
  raise EIdException.Create('Stream error'); {do not localize}
end;

end.

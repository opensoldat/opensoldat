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
  Rev 1.16    23.3.2005 ã. 20:52:00  DBondzhev
  LIOHandler is not released if exception is thrown while listening for
  incomming connection.

  Rev 1.15    2004.02.03 4:17:04 PM  czhower
  For unit name changes.

  Rev 1.14    2004.01.20 10:03:32 PM  czhower
  InitComponent

  Rev 1.13    2003.10.11 5:50:00 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.12    10/5/2003 03:03:58 AM  JPMugaas
  Should compile.

  Rev 1.11    2003.09.19 10:11:20 PM  czhower
  Next stage of fiber support in servers.

  Rev 1.10    2003.09.19 11:54:34 AM  czhower
  -Completed more features necessary for servers
  -Fixed some bugs

  Rev 1.9    2003.09.18 4:10:28 PM  czhower
  Preliminary changes for Yarn support.

  Rev 1.8    2003.07.14 10:15:36 PM  czhower
  Changed timeout to 250 from 100

  Rev 1.7    3/29/2003 5:55:02 PM  BGooijen
  now calls AfterAccept

  Rev 1.6    3/13/2003 10:18:28 AM  BGooijen
  Server side fibers, bug fixes

  Rev 1.5    1-17-2003 22:22:04  BGooijen
  new design

  Rev 1.4    1-1-2003 16:28:26  BGooijen
  Changed TIdThread to TIdYarn

  Rev 1.3    12-7-2002 17:02:32  BGooijen
  Now creates IOHandlerSockets of class IOHandlerSocketClass.
  This is more flexible for descendants

  Rev 1.2    12-7-2002 12:34:38  BGooijen
  Re-enabled IPv6 support

  Rev 1.1    05/12/2002 15:32:00  AO'Neill

  Rev 1.0    12/2/2002 05:01:30 PM  JPMugaas
  Rechecked in due to file corruption.
}

unit IdServerIOHandlerSocket;

interface
{$i IdCompilerDefines.inc}

uses
  IdSocketHandle, IdGlobal, IdThread, IdServerIOHandler, IdStackConsts, IdIOHandler, IdScheduler,
  IdIOHandlerSocket, IdYarn;

type
  TIdIOHandlerSocketClass = class of TIdIOHandlerSocket;

  TIdServerIOHandlerSocket = class(TIdServerIOHandler)
  protected
    IOHandlerSocketClass: TIdIOHandlerSocketClass;
    //
    procedure InitComponent; override;
  public
    function Accept(
      ASocket: TIdSocketHandle;
      AListenerThread: TIdThread;
      AYarn: TIdYarn
      ): TIdIOHandler;
      override;
    procedure Init; override;
  end;

implementation
uses SysUtils;

{ TIdServerIOHandlerSocket }

procedure TIdServerIOHandlerSocket.Init;
begin
  //
end;

function TIdServerIOHandlerSocket.Accept(
  ASocket: TIdSocketHandle;
  AListenerThread: TIdThread;
  AYarn: TIdYarn
  ): TIdIOHandler;
var
  LIOHandler: TIdIOHandlerSocket;
begin
  //using a custom scheduler, AYarn may be nil, so don't assert
  Assert(ASocket<>nil);
  Assert(AListenerThread<>nil);

  Result := nil;
  LIOHandler := IOHandlerSocketClass.Create(nil);
  try
    LIOHandler.Open;
    while not AListenerThread.Stopped do begin
      if ASocket.Select(250) then begin
        if LIOHandler.Binding.Accept(ASocket.Handle) then begin
          LIOHandler.AfterAccept;
          Result := LIOHandler;
          LIOHandler := nil;
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(LIOHandler);
  end;
end;

procedure TIdServerIOHandlerSocket.InitComponent;
begin
  inherited InitComponent;
  IOHandlerSocketClass := TIdIOHandlerSocket;
end;

end.


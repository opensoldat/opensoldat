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
  Rev 1.42    2/1/05 12:36:36 AM  RLebeau
  Removed CommandHandlersEnabled property, no longer used

  Rev 1.41    12/2/2004 9:26:42 PM  JPMugaas
  Bug fix.

  Rev 1.40    2004.10.27 9:20:04 AM  czhower
  For TIdStrings

  Rev 1.39    10/26/2004 8:42:58 PM  JPMugaas
  Should be more portable with new references to TIdStrings and TIdStringList.

  Rev 1.38    6/21/04 10:07:14 PM  RLebeau
  Updated .DoConnect() to make sure the connection is still connected before
  then sending the Greeting

  Rev 1.37    6/20/2004 12:01:44 AM  DSiders
  Added "Do Not Localize" comments.

  Rev 1.36    6/16/04 12:37:06 PM  RLebeau
  more compiler errors

  Rev 1.35    6/16/04 12:30:32 PM  RLebeau
  compiler errors

  Rev 1.34    6/16/04 12:12:26 PM  RLebeau
  Updated ExceptionReply, Greeting, HelpReply, MaxConnectionReply, and
  ReplyUnknownCommand properties to use getter methods that call virtual Create
  methods which descendants can override for class-specific initializations

  Rev 1.33    5/16/04 5:16:52 PM  RLebeau
  Added setter methods to ExceptionReply, HelpReply, and ReplyTexts properties

  Rev 1.32    4/19/2004 5:39:58 PM  BGooijen
  Added comment

  Rev 1.31    4/18/2004 11:58:44 PM  BGooijen
  Wasn't thread safe

  Rev 1.30    3/3/2004 4:59:38 AM  JPMugaas
  Updated for new properties.

  Rev 1.29    2004.03.01 5:12:24 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.28    2004.02.29 9:43:08 PM  czhower
  Added ReadCommandLine.

  Rev 1.27    2004.02.29 8:17:18 PM  czhower
  Minor cosmetic changes to code.

  Rev 1.26    2004.02.03 4:17:08 PM  czhower
  For unit name changes.

  Rev 1.25    03/02/2004 01:49:22  CCostelloe
  Added DoReplyUnknownCommand to allow TIdIMAP4Server set a correct reply for
  unknown commands

  Rev 1.24    1/29/04 9:43:16 PM  RLebeau
  Added setter methods to various TIdReply properties

  Rev 1.23    2004.01.20 10:03:22 PM  czhower
  InitComponent

  Rev 1.22    1/5/2004 2:35:36 PM  JPMugaas
  Removed of object in method declarations.

  Rev 1.21    1/5/04 10:12:58 AM  RLebeau
  Fixed Typos in OnBeforeCommandHandler and OnAfterCommandHandler events

  Rev 1.20    1/4/04 8:45:34 PM  RLebeau
  Added OnBeforeCommandHandler and OnAfterCommandHandler events

  Rev 1.19    1/1/2004 9:33:22 PM  BGooijen
  the abstract class TIdReply was created sometimes, fixed that

  Rev 1.18    2003.10.18 9:33:26 PM  czhower
  Boatload of bug fixes to command handlers.

  Rev 1.17    2003.10.18 8:03:58 PM  czhower
  Defaults for codes

  Rev 1.16    8/31/2003 11:49:40 AM  BGooijen
  removed FReplyClass, this was also in TIdTCPServer

  Rev 1.15    7/9/2003 10:55:24 PM  BGooijen
  Restored all features

  Rev 1.14    7/9/2003 04:36:08 PM  JPMugaas
  You now can override the TIdReply with your own type.  This should illiminate
  some warnings about some serious issues.  TIdReply is ONLY a base class with
  virtual methods.

  Rev 1.13    2003.07.08 2:26:02 PM  czhower
  Sergio's update

  Rev 1.0    7/7/2003 7:06:44 PM  SPerry
  Component that uses command handlers

  Rev 1.0    7/6/2003 4:47:32 PM  SPerry
  Units that use Command handlers

  Adapted to IdCommandHandlers.pas SPerry

  Rev 1.7    4/4/2003 8:08:00 PM  BGooijen
  moved some consts from tidtcpserver here

  Rev 1.6    3/23/2003 11:22:24 PM  BGooijen
  Moved some code to HandleCommand

  Rev 1.5    3/22/2003 1:46:36 PM  BGooijen
  Removed unused variables

  Rev 1.4    3/20/2003 12:18:30 PM  BGooijen
  Moved ReplyExceptionCode from TIdTCPServer to TIdCmdTCPServer

  Rev 1.3    3/20/2003 12:14:18 PM  BGooijen
  Re-enabled Server.ReplyException

  Rev 1.2    2/24/2003 07:21:50 PM  JPMugaas
  Now compiles with new core code restructures.

  Rev 1.1    1/23/2003 11:06:10 AM  BGooijen

  Rev 1.0    1/20/2003 12:48:40 PM  BGooijen
  Tcpserver with command handlers, these were originally in TIdTcpServer, but
  are now moved here
}

unit IdCmdTCPServer;

interface

{$I IdCompilerDefines.inc}
//Put FPC into Delphi mode

uses
  Classes,
  IdCommandHandlers,
  IdContext,
  IdIOHandler,
  IdReply,
  IdTCPServer,
  SysUtils;

type
  TIdCmdTCPServer = class;

  { Events }
  TIdCmdTCPServerAfterCommandHandlerEvent = procedure(ASender: TIdCmdTCPServer;
    AContext: TIdContext) of object;
  TIdCmdTCPServerBeforeCommandHandlerEvent = procedure(ASender: TIdCmdTCPServer;
    var AData: string; AContext: TIdContext) of object;

  TIdCmdTCPServer = class(TIdTCPServer)
  protected
    FCommandHandlers: TIdCommandHandlers;
    FCommandHandlersInitialized: Boolean;
    FExceptionReply: TIdReply;
    FHelpReply: TIdReply;
    FGreeting: TIdReply;
    FMaxConnectionReply: TIdReply;
    FOnAfterCommandHandler: TIdCmdTCPServerAfterCommandHandlerEvent;
    FOnBeforeCommandHandler: TIdCmdTCPServerBeforeCommandHandlerEvent;
    FReplyClass: TIdReplyClass;
    FReplyTexts: TIdReplies;
    FReplyUnknownCommand: TIdReply;
    //
    procedure CheckOkToBeActive;  override;
    function CreateExceptionReply: TIdReply; virtual;
    function CreateGreeting: TIdReply; virtual;
    function CreateHelpReply: TIdReply; virtual;
    function CreateMaxConnectionReply: TIdReply; virtual;
    function CreateReplyUnknownCommand: TIdReply; virtual;
    procedure DoAfterCommandHandler(ASender: TIdCommandHandlers; AContext: TIdContext);
    procedure DoBeforeCommandHandler(ASender: TIdCommandHandlers; var AData: string;
      AContext: TIdContext);
    procedure DoConnect(AContext: TIdContext); override;
    function DoExecute(AContext: TIdContext): Boolean; override;
    procedure DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler); override;
    // This is here to allow servers to override this functionality, such as IMAP4 server
    procedure DoReplyUnknownCommand(AContext: TIdContext; ALine: string); virtual;
    function GetExceptionReply: TIdReply;
    function GetGreeting: TIdReply;
    function GetHelpReply: TIdReply;
    function GetMaxConnectionReply: TIdReply;
    function GetRepliesClass: TIdRepliesClass; virtual;
    function GetReplyClass: TIdReplyClass; virtual;
    function GetReplyUnknownCommand: TIdReply;
    procedure InitializeCommandHandlers; virtual;
    procedure InitComponent; override;
    // This is used by command handlers as the only input. This can be overriden to filter, modify,
    // or preparse the input.
    function ReadCommandLine(AContext: TIdContext): string; virtual;
    procedure Startup; override;
    procedure SetCommandHandlers(AValue: TIdCommandHandlers);
    procedure SetExceptionReply(AValue: TIdReply);
    procedure SetGreeting(AValue: TIdReply);
    procedure SetHelpReply(AValue: TIdReply);
    procedure SetMaxConnectionReply(AValue: TIdReply);
    procedure SetReplyUnknownCommand(AValue: TIdReply);
    procedure SetReplyTexts(AValue: TIdReplies);
  public
    destructor Destroy; override;
  published
    property CommandHandlers: TIdCommandHandlers read FCommandHandlers
      write SetCommandHandlers;
    property ExceptionReply: TIdReply read GetExceptionReply write SetExceptionReply;
    property Greeting: TIdReply read GetGreeting write SetGreeting;
    property HelpReply: TIdReply read GetHelpReply write SetHelpReply;
    property MaxConnectionReply: TIdReply read GetMaxConnectionReply
      write SetMaxConnectionReply;
    property ReplyTexts: TIdReplies read FReplyTexts write SetReplyTexts;
    property ReplyUnknownCommand: TIdReply read GetReplyUnknownCommand
     write SetReplyUnknownCommand;
    //
    property OnAfterCommandHandler: TIdCmdTCPServerAfterCommandHandlerEvent
      read FOnAfterCommandHandler write FOnAfterCommandHandler;
    property OnBeforeCommandHandler: TIdCmdTCPServerBeforeCommandHandlerEvent
      read FOnBeforeCommandHandler write FOnBeforeCommandHandler;
  end;

implementation

uses
  IdGlobal,
  IdResourceStringsCore,
  IdReplyRFC;

function TIdCmdTCPServer.GetReplyClass: TIdReplyClass;
begin
  Result := TIdReplyRFC;
end;

function TIdCmdTCPServer.GetRepliesClass: TIdRepliesClass;
begin
  Result := TIdRepliesRFC;
end;

destructor TIdCmdTCPServer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FReplyUnknownCommand);
  FreeAndNil(FReplyTexts);
  FreeAndNil(FMaxConnectionReply);
  FreeAndNil(FHelpReply);
  FreeAndNil(FGreeting);
  FreeAndNil(FExceptionReply);
  FreeAndNil(FCommandHandlers);
end;

procedure TIdCmdTCPServer.DoAfterCommandHandler(ASender: TIdCommandHandlers;
  AContext: TIdContext);
begin
  if Assigned(OnAfterCommandHandler) then begin
    OnAfterCommandHandler(Self, AContext);
  end;
end;

procedure TIdCmdTCPServer.DoBeforeCommandHandler(ASender: TIdCommandHandlers;
  var AData: string; AContext: TIdContext);
begin
  if Assigned(OnBeforeCommandHandler) then begin
    OnBeforeCommandHandler(Self, AData, AContext);
  end;
end;

function TIdCmdTCPServer.DoExecute(AContext: TIdContext): Boolean;
var
  LLine: string;
begin
  if CommandHandlers.Count > 0 then begin
    Result := True;
    if AContext.Connection.Connected then begin
      LLine := ReadCommandLine(AContext);
      // OLX sends blank lines during reset groups (NNTP) and expects no response.
      // Not sure what the RFCs say about blank lines.
      // I telnetted to some newsservers, and they dont respond to blank lines.
      // This unit is core and not NNTP, but we should be consistent.
      if LLine <> '' then begin
        if not FCommandHandlers.HandleCommand(AContext, LLine) then begin
          DoReplyUnknownCommand(AContext, LLine);
        end;
      end;
    end;
  end else begin
    Result := inherited DoExecute(AContext);
  end;
  if Result and Assigned(AContext.Connection) then begin
    Result := AContext.Connection.Connected;
  end;
  // the return value is used to determine if the DoExecute needs to be called again by the thread
end;

procedure TIdCmdTCPServer.DoReplyUnknownCommand(AContext: TIdContext; ALine: string);
var
  LReply: TIdReply;
begin
  if CommandHandlers.PerformReplies then begin
    LReply := FReplyClass.CreateWithReplyTexts(nil, ReplyTexts); try
      LReply.Assign(ReplyUnknownCommand);
      LReply.Text.Add(ALine);
      AContext.Connection.IOHandler.Write(LReply.FormattedReply);
    finally
      FreeAndNil(LReply);
    end;
  end;
end;

procedure TIdCmdTCPServer.InitializeCommandHandlers;
begin
end;

procedure TIdCmdTCPServer.DoConnect(AContext: TIdContext);
var
  LGreeting: TIdReply;
begin
  inherited DoConnect(AContext);
  // RLebeau - check the connection first in case the application
  // chose to disconnect the connection in the OnConnect event handler.
  if AContext.Connection.Connected then begin
    if Greeting.ReplyExists then begin
      ReplyTexts.UpdateText(Greeting);
      LGreeting := FReplyClass.Create(nil); try // SendGreeting calls TIdReply.GetFormattedReply
        LGreeting.Assign(Greeting);           // and that changes the reply object, so we have to
        SendGreeting(AContext, LGreeting);    // clone it to make it thread-safe
      finally
        FreeAndNil(LGreeting);
      end;
    end;
  end;
end;

procedure TIdCmdTCPServer.DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler);
begin
  inherited DoMaxConnectionsExceeded(AIOHandler);
  //Do not UpdateText here - in thread. Is done in constructor
  AIOHandler.Write(MaxConnectionReply.FormattedReply);
end;

procedure TIdCmdTCPServer.Startup;
var
  i, j: Integer;
  LDescr: TStrings;
  LHelpList: TStringList;
  LHandler, LAddedHandler: TIdCommandHandler;
begin
  inherited Startup;
  if not FCommandHandlersInitialized then begin
    // InitializeCommandHandlers must be called only at runtime, and only after streaming
    // has occured. This used to be in .Loaded and that worked for forms. It failed
    // for dynamically created instances and also for descendant classes.
    FCommandHandlersInitialized := True;
    InitializeCommandHandlers;
    if HelpReply.Code <> '' then begin
      LAddedHandler := CommandHandlers.Add;
      LAddedHandler.Command := 'Help'; {do not localize}
      LAddedHandler.Description.Text := 'Displays commands that the servers supports.'; {do not localize}
      LAddedHandler.NormalReply.Assign(HelpReply);
      LHelpList := TStringList.Create;
      try
        for i := 0 to CommandHandlers.Count - 1 do begin
          LHandler := CommandHandlers.Items[i];
          if LHandler.HelpVisible then begin
            LHelpList.AddObject(LHandler.Command+LHandler.HelpSuperScript, LHandler);
          end;
        end;
        LHelpList.Sort;
        for i := 0 to LHelpList.Count - 1 do begin
          LAddedHandler.Response.Add(LHelpList[i]);
          LDescr := TIdCommandHandler(LHelpList.Objects[i]).Description;
          for j := 0 to LDescr.Count - 1 do begin
            LAddedHandler.Response.Add('  ' + LDescr[j]); {do not localize}
          end;
          LAddedHandler.Response.Add(''); {do not localize}
        end;
      finally
        FreeAndNil(LHelpList);
      end;
    end;
  end;
end;

procedure TIdCmdTCPServer.SetCommandHandlers(AValue: TIdCommandHandlers);
begin
  FCommandHandlers.Assign(AValue);
end;

function TIdCmdTCPServer.CreateExceptionReply: TIdReply;
begin
  Result := FReplyClass.CreateWithReplyTexts(nil, ReplyTexts);
  Result.SetReply(500, 'Unknown Internal Error'); {do not localize}
end;

function TIdCmdTCPServer.GetExceptionReply: TIdReply;
begin
  if FExceptionReply = nil then begin
    FExceptionReply := CreateExceptionReply;
  end;
  Result := FExceptionReply;
end;

procedure TIdCmdTCPServer.SetExceptionReply(AValue: TIdReply);
begin
  ExceptionReply.Assign(AValue);
end;

function TIdCmdTCPServer.CreateGreeting: TIdReply;
begin
  Result := FReplyClass.CreateWithReplyTexts(nil, ReplyTexts);
  Result.SetReply(200, 'Welcome'); {do not localize}
end;

function TIdCmdTCPServer.GetGreeting: TIdReply;
begin
  if FGreeting = nil then begin
    FGreeting := CreateGreeting;
  end;
  Result := FGreeting;
end;

procedure TIdCmdTCPServer.SetGreeting(AValue: TIdReply);
begin
  Greeting.Assign(AValue);
end;

function TIdCmdTCPServer.CreateHelpReply: TIdReply;
begin
  Result := FReplyClass.CreateWithReplyTexts(nil, ReplyTexts);
  Result.SetReply(100, 'Help follows'); {do not localize}
end;

function TIdCmdTCPServer.GetHelpReply: TIdReply;
begin
  if FHelpReply = nil then begin
    FHelpReply := CreateHelpReply;
  end;
  Result := FHelpReply;
end;

procedure TIdCmdTCPServer.SetHelpReply(AValue: TIdReply);
begin
  HelpReply.Assign(AValue);
end;

function TIdCmdTCPServer.CreateMaxConnectionReply: TIdReply;
begin
  Result := FReplyClass.CreateWithReplyTexts(nil, ReplyTexts);
  Result.SetReply(300, 'Too many connections. Try again later.'); {do not localize}
end;

function TIdCmdTCPServer.GetMaxConnectionReply: TIdReply;
begin
  if FMaxConnectionReply = nil then begin
    FMaxConnectionReply := CreateMaxConnectionReply;
  end;
  Result := FMaxConnectionReply;
end;

procedure TIdCmdTCPServer.SetMaxConnectionReply(AValue: TIdReply);
begin
  MaxConnectionReply.Assign(AValue);
end;

function TIdCmdTCPServer.CreateReplyUnknownCommand: TIdReply;
begin
  Result := FReplyClass.CreateWithReplyTexts(nil, ReplyTexts);
  Result.SetReply(400, 'Unknown Command'); {do not localize}
end;

function TIdCmdTCPServer.GetReplyUnknownCommand: TIdReply;
begin
  if FReplyUnknownCommand = nil then begin
    FReplyUnknownCommand := CreateReplyUnknownCommand;
  end;
  Result := FReplyUnknownCommand;
end;

procedure TIdCmdTCPServer.SetReplyUnknownCommand(AValue: TIdReply);
begin
  ReplyUnknownCommand.Assign(AValue);
end;

procedure TIdCmdTCPServer.SetReplyTexts(AValue: TIdReplies);
begin
  FReplyTexts.Assign(AValue);
end;

procedure TIdCmdTCPServer.InitComponent;
begin
  inherited InitComponent;
  FReplyClass := GetReplyClass;

  // Before Command handlers as they need FReplyTexts, but after FReplyClass is set
  FReplyTexts := GetRepliesClass.Create(Self, FReplyClass);

  FCommandHandlers := TIdCommandHandlers.Create(Self, FReplyClass, ReplyTexts, ExceptionReply);
  FCommandHandlers.OnAfterCommandHandler := DoAfterCommandHandler;
  FCommandHandlers.OnBeforeCommandHandler := DoBeforeCommandHandler;
end;

function TIdCmdTCPServer.ReadCommandLine(AContext: TIdContext): string;
begin
  Result := AContext.Connection.IOHandler.ReadLn;
end;

procedure TIdCmdTCPServer.CheckOkToBeActive;
begin
  if (CommandHandlers.Count = 0) and FCommandHandlersInitialized then begin
    inherited CheckOkToBeActive;
  end;
end;

end.

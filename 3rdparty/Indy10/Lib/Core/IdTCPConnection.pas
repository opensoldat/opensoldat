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


  $Log$


//TODO: Elim read/write methods - they are duped
//TODO: See second uses comment


   Rev 1.68    3/7/2005 5:48:18 PM  JPMugaas
 Made a backdoor so we can adjust command output in specific ways.


   Rev 1.67    1/15/2005 6:02:02 PM  JPMugaas
 These should compile again.


   Rev 1.66    1/15/05 2:16:04 PM  RLebeau
 Misc. tweaks


   Rev 1.65    12/21/04 3:20:54 AM  RLebeau
 Removed compiler warning


   Rev 1.64    12/12/04 2:24:28 PM  RLebeau
 Updated WriteRFCStrings() to call new method in the IOHandler.


   Rev 1.63    10/26/2004 8:43:02 PM  JPMugaas
 Should be more portable with new references to TIdStrings and TIdStringList.


   Rev 1.62    6/11/2004 8:48:36 AM  DSiders
 Added "Do not Localize" comments.


   Rev 1.61    2004.06.07 1:34:20 PM  czhower
 OnWork fix now sends running total as it should.


   Rev 1.60    2004.06.06 5:18:04 PM  czhower
 OnWork bug fix


   Rev 1.59    2004.06.05 9:46:30 AM  czhower
 IOHandler OnWork fix


   Rev 1.58    11/05/2004 17:13:32  HHariri
 Fix brought from IW for overflow of DoWork


   Rev 1.57    4/19/2004 9:50:08 AM  BGooijen
 Fixed AV in .Disconnect


   Rev 1.56    2004.04.18 12:52:04 AM  czhower
 Big bug fix with server disconnect and several other bug fixed that I found
 along the way.


   Rev 1.55    2004.03.06 10:40:30 PM  czhower
 Changed IOHandler management to fix bug in server shutdowns.


   Rev 1.54    2004.03.06 1:32:58 PM  czhower
 -Change to disconnect
 -Addition of DisconnectNotifyPeer
 -WriteHeader now write bufers


   Rev 1.53    3/1/04 7:12:00 PM  RLebeau
 Bug fix for SetIOHandler() not updating the FSocket member correctly.


   Rev 1.52    2004.02.03 4:16:56 PM  czhower
 For unit name changes.


   Rev 1.51    1/29/04 9:37:18 PM  RLebeau
 Added setter method for Greeting property


   Rev 1.50    2004.01.28 9:42:32 PM  czhower
 Now checks for connection.


   Rev 1.49    2004.01.20 10:03:36 PM  czhower
 InitComponent


   Rev 1.48    2003.12.31 3:47:44 PM  czhower
 Changed to use TextIsSame


   Rev 1.47    12/28/2003 4:47:40 PM  BGooijen
 Removed ChangeReplyClass


   Rev 1.46    14/12/2003 18:14:54  CCostelloe
 Added ChangeReplyClass procedure.


   Rev 1.45    11/4/2003 10:28:34 PM  DSiders
 Removed exceptions moved to IdException.pas.


   Rev 1.44    2003.10.18 9:33:28 PM  czhower
 Boatload of bug fixes to command handlers.


   Rev 1.43    10/15/2003 7:32:48 PM  DSiders
 Added a resource string for the exception raised in
 TIdTCPConnection.CreateIOHandler.


   Rev 1.42    2003.10.14 1:27:02 PM  czhower
 Uupdates + Intercept support


   Rev 1.41    10/10/2003 11:00:36 PM  BGooijen
 Added GetReplyClass


   Rev 1.40    2003.10.02 8:29:40 PM  czhower
 Added IdReply back


   Rev 1.39    2003.10.02 8:08:52 PM  czhower
 Removed unneeded unit in uses.


   Rev 1.38    2003.10.01 9:11:28 PM  czhower
 .Net


   Rev 1.37    2003.10.01 5:05:18 PM  czhower
 .Net


   Rev 1.36    2003.10.01 2:30:42 PM  czhower
 .Net


   Rev 1.35    2003.10.01 11:16:38 AM  czhower
 .Net


   Rev 1.34    2003.09.30 1:23:06 PM  czhower
 Stack split for DotNet


   Rev 1.33    2003.09.18 7:12:42 PM  czhower
 AV Fix in SetIOHandler


   Rev 1.32    2003.09.18 5:18:00 PM  czhower
 Implemented OnWork


   Rev 1.31    2003.06.30 6:17:48 PM  czhower
 Moved socket property to public. Dont know how/why it got protected.


   Rev 1.30    2003.06.30 5:41:56 PM  czhower
 -Fixed AV that occurred sometimes when sockets were closed with chains
 -Consolidated code that was marked by a todo for merging as it no longer
 needed to be separate
 -Removed some older code that was no longer necessary

 Passes bubble tests.


   Rev 1.29    2003.06.05 10:08:52 AM  czhower
 Extended reply mechanisms to the exception handling. Only base and RFC
 completed, handing off to J Peter.


   Rev 1.28    6/4/2003 03:54:42 PM  JPMugaas
 Now should compile.


   Rev 1.27    2003.06.04 8:10:00 PM  czhower
 Modified CheckResponse string version to allow ''


   Rev 1.26    2003.06.04 12:02:30 PM  czhower
 Additions for text code and command handling.


   Rev 1.25    2003.06.03 3:44:26 PM  czhower
 Removed unused variable.


   Rev 1.24    2003.05.30 10:25:58 PM  czhower
 Implemented IsEndMarker


   Rev 1.23    5/26/2003 04:29:52 PM  JPMugaas
 Removed GenerateReply and ParseReply.  Those are now obsolete duplicate
 functions in the new design.


   Rev 1.22    5/26/2003 12:19:56 PM  JPMugaas


   Rev 1.21    2003.05.26 11:38:20 AM  czhower


   Rev 1.20    5/25/2003 03:34:54 AM  JPMugaas


   Rev 1.19    5/25/2003 03:16:22 AM  JPMugaas


   Rev 1.18    5/20/2003 02:40:10 PM  JPMugaas


   Rev 1.17    5/20/2003 12:43:50 AM  BGooijen
 changeable reply types


    Rev 1.16    4/4/2003 8:10:14 PM  BGooijen
 procedure CreateIOHandler is now public


   Rev 1.15    3/27/2003 3:17:32 PM  BGooijen
 Removed MaxLineLength, MaxLineAction, SendBufferSize, RecvBufferSize,
 ReadLnSplit, ReadLnTimedOut


   Rev 1.14    3/19/2003 1:04:16 PM  BGooijen
 changed procedure CreateIOHandler a little (default parameter, and other
 behavour when parameter = nil (constructs default now))


   Rev 1.13    3/5/2003 11:07:18 PM  BGooijen
 removed intercept from this file


   Rev 1.12    2003.02.25 7:28:02 PM  czhower
 Fixed WriteRFCReply


   Rev 1.11    2003.02.25 1:36:20 AM  czhower


   Rev 1.10    2/13/2003 02:14:44 PM  JPMugaas
 Now calls ReadLn in GetInternelResponse so a space is not dropped.  Dropping
 a space throws off some things in FTP such as the FEAT reply.


   Rev 1.9    2003.01.18 12:29:52 PM  czhower


   Rev 1.8    1-17-2003 22:22:08  BGooijen
 new design


   Rev 1.7    12-16-2002 20:44:38  BGooijen
 Added procedure CreateIOHandler(....)


   Rev 1.6    12-15-2002 23:32:32  BGooijen
 Added RecvBufferSize


   Rev 1.5    12-14-2002 22:16:32  BGooijen
 improved method to detect timeouts in ReadLn.


   Rev 1.4    12/6/2002 02:11:46 PM  JPMugaas
 Protected Port and Host properties added to TCPClient because those are
 needed by protocol implementations.  Socket property added to TCPConnection.


   Rev 1.3    6/12/2002 11:00:16 AM  SGrobety


   Rev 1.0    21/11/2002 12:36:48 PM  SGrobety    Version: Indy 10


   Rev 1.2    11/15/2002 01:26:42 PM  JPMugaas
 Restored Trim to ReadLnWait and changed GetInternelResponse to use ReadLn
 instead of ReadLn wait.


   Rev 1.1    11/14/2002 06:44:54 PM  JPMugaas
 Removed Trim from ReadLnWait.  It was breaking the new RFC Reply parsing code
 by removing the space at the beggining of a line.


   Rev 1.0    11/13/2002 09:00:30 AM  JPMugaas
}
unit IdTCPConnection;

interface

{$i IdCompilerDefines.inc}

{
2003-12-14 - Ciaran Costelloe
  - Added procedure ChangeReplyClass, because in .NET, you cannot set FReplyClass
    before calling the constructor, so call this procedure after the constructor
    to set FReplyClass to (say) TIdReplyIMAP4.
2002-06 -Andrew P.Rybin
  -WriteStream optimization and new "friendly" interface, InputLn fix (CrLf only if AEcho)
2002-04-12 - Andrew P.Rybin
  - ReadLn bugfix and optimization
2002-01-20 - Chad Z. Hower a.k.a Kudzu
  -WriteBuffer change was not correct. Removed. Need info on original problem to fix properly.
  -Modified ReadLnWait
2002-01-19 - Grahame Grieve
  - Fix to WriteBuffer to accept -1 from the stack.
  Also fixed to clean up FWriteBuffer if connection lost.
2002-01-19 - Chad Z. Hower a.k.a Kudzu
  -Fix to ReadLn
2002-01-16 - Andrew P.Rybin
  -ReadStream optimization, TIdManagedBuffer new
2002-01-03 - Chad Z. Hower a.k.a Kudzu
  -Added MaxLineAction
  -Added ReadLnSplit
2001-12-27 - Chad Z. Hower a.k.a Kudzu
  -Changes and bug fixes to InputLn
  -Modifed how buffering works
    -Added property InputBuffer
    -Moved some things to TIdBuffer
  -Modified ReadLn
  -Added LineCount to Capture
2001-12-25 - Andrew P.Rybin
  -MaxLineLength,ReadLn,InputLn and Merry Christmas!
Original Author and Maintainer:
  -Chad Z. Hower a.k.a Kudzu
}

uses
  Classes,
  IdComponent,
  IdException,
  IdExceptionCore,
  IdGlobal,
  IdIntercept,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdReply,
  IdSocketHandle,
  IdBaseComponent;

type
  TIdTCPConnection = class(TIdComponent)
  protected
    FGreeting: TIdReply; // TODO: Only TIdFTP uses it, so it should be moved!
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FIntercept: TIdConnectionIntercept;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FIOHandler: TIdIOHandler;
    FLastCmdResult: TIdReply;
    FManagedIOHandler: Boolean;
    FOnDisconnected: TNotifyEvent;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FSocket: TIdIOHandlerSocket;
    FReplyClass: TIdReplyClass;
    //
    procedure CheckConnected;
    procedure DoOnDisconnected; virtual;
    procedure InitComponent; override;
    function GetIntercept: TIdConnectionIntercept; virtual;
    function GetReplyClass: TIdReplyClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetIntercept(AValue: TIdConnectionIntercept); virtual;
    procedure SetIOHandler(AValue: TIdIOHandler); virtual;
    procedure SetGreeting(AValue: TIdReply);
    procedure WorkBeginEvent(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure WorkEndEvent(ASender: TObject; AWorkMode: TWorkMode);
    procedure WorkEvent(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure PrepareCmd(var aCmd: string); virtual;
  public
    procedure CreateIOHandler(ABaseType: TIdIOHandlerClass = nil);
    procedure CheckForGracefulDisconnect(ARaiseExceptionIfDisconnected: Boolean = True); virtual;
    //
    function CheckResponse(const AResponse: Int16;
     const AAllowedResponses: array of Int16): Int16; overload; virtual;
    function CheckResponse(const AResponse, AAllowedResponse: string): string; overload; virtual;
    //
    function Connected: Boolean; virtual;
    destructor Destroy; override;
    // Dont allow override of this one, its for overload only
    procedure Disconnect; overload; // .Net overload
    procedure Disconnect(ANotifyPeer: Boolean); overload; virtual;
    // This is called when a protocol sends a command to tell the other side (typically client to
    // server) that it is about to disconnect. The implementation should go here.
    procedure DisconnectNotifyPeer; virtual;
    // GetInternalResponse is not in IOHandler as some protocols may need to
    // override it. It could be still moved and proxied from here, but at this
    // point it is here.
    procedure GetInternalResponse(AEncoding: IIdTextEncoding = nil); virtual;
    // Reads response using GetInternalResponse which each reply type can define
    // the behaviour. Then checks against expected Code.
    //
    // Seperate one for singles as one of the older Delphi compilers cannot
    // match a single number into an array. IIRC newer ones do.
    function GetResponse(const AAllowedResponse: Int16 = -1;
      AEncoding: IIdTextEncoding = nil): Int16; overload;
    function GetResponse(const AAllowedResponses: array of Int16;
      AEncoding: IIdTextEncoding = nil): Int16; overload; virtual;
    // No array type for strings as ones that use strings are usually bastard
    // protocols like POP3/IMAP which dont include proper substatus anyways.
    //
    // If a case can be made for some other condition this may be expanded
    // in the future
    function GetResponse(const AAllowedResponse: string;
      AEncoding: IIdTextEncoding = nil): string; overload; virtual;
    //
    property Greeting: TIdReply read FGreeting write SetGreeting;
    // RaiseExceptionForCmdResult - Overload necesary as a exception as a default param doesnt work
    procedure RaiseExceptionForLastCmdResult; overload; virtual;
    procedure RaiseExceptionForLastCmdResult(AException: TClassIdException);
     overload; virtual;
    // These are extended GetResponses, so see the comments for GetResponse
    function SendCmd(AOut: string; const AResponse: Int16 = -1;
      AEncoding: IIdTextEncoding = nil): Int16; overload;
    function SendCmd(AOut: string; const AResponse: array of Int16;
      AEncoding: IIdTextEncoding = nil): Int16; overload; virtual;
    function SendCmd(AOut: string; const AResponse: string;
      AEncoding: IIdTextEncoding = nil): string; overload;
    //
    procedure WriteHeader(AHeader: TStrings);
    procedure WriteRFCStrings(AStrings: TStrings);
    //
    property LastCmdResult: TIdReply read FLastCmdResult;
    property ManagedIOHandler: Boolean read FManagedIOHandler write FManagedIOHandler;
    property Socket: TIdIOHandlerSocket read FSocket;
  published
    property Intercept: TIdConnectionIntercept read GetIntercept write SetIntercept;
    property IOHandler: TIdIOHandler read FIOHandler write SetIOHandler;
    // Events
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnWork;
    property OnWorkBegin;
    property OnWorkEnd;
  end;

implementation

uses
  IdAntiFreezeBase, IdResourceStringsCore, IdStackConsts, IdReplyRFC,
  SysUtils;

function TIdTCPConnection.GetIntercept: TIdConnectionIntercept;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdIOHandler;
begin
  LIOHandler := IOHandler;
  if LIOHandler <> nil then begin
    Result := LIOHandler.Intercept;
  end else begin
    Result := FIntercept;
  end;
end;

function TIdTCPConnection.GetReplyClass:TIdReplyClass;
begin
  Result := TIdReplyRFC;
end;

procedure TIdTCPConnection.CreateIOHandler(ABaseType:TIdIOHandlerClass=nil);
begin
  if Connected then begin
    raise EIdException.Create(RSIOHandlerCannotChange);
  end;
  if Assigned(ABaseType) then begin
    IOHandler := TIdIOHandler.MakeIOHandler(ABaseType, Self);
  end else begin
    IOHandler := TIdIOHandler.MakeDefaultIOHandler(Self);
  end;
  ManagedIOHandler := True;
end;

function TIdTCPConnection.Connected: Boolean;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdIOHandler;
begin
  // Its been changed now that IOHandler is not usually nil, but can be before the initial connect
  // and also this keeps it here so the user does not have to access the IOHandler for this and
  // also to allow future control from the connection.
  LIOHandler := IOHandler;
  Result := Assigned(LIOHandler);
  if Result then begin
    Result := LIOHandler.Connected;
  end;
end;

destructor TIdTCPConnection.Destroy;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdIOHandler;
begin
  // Just close IOHandler directly. Dont call Disconnect - Disconnect may be override and
  // try to read/write to the socket.
  LIOHandler := IOHandler;
  if Assigned(LIOHandler) then begin
    LIOHandler.Close;
    // This will free any managed IOHandlers
    {$IFDEF USE_OBJECT_ARC}LIOHandler := nil;{$ENDIF}
    SetIOHandler(nil);
  end;
  FreeAndNil(FLastCmdResult);
  FreeAndNil(FGreeting);
  inherited Destroy;
end;

procedure TIdTCPConnection.Disconnect(ANotifyPeer: Boolean);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdIOHandler;
begin
  try
    // Separately to avoid calling .Connected unless needed
    if ANotifyPeer then begin
      // TODO: do not call Connected() here if DisconnectNotifyPeer() is not
      // overriden. Ideally, Connected() should be called by overridden
      // DisconnectNotifyPeer() implementations if they really need it. But
      // to avoid any breakages in third-party overrides, we could check here
      // if DisconnectNotifyPeer() has been overridden and then call Connected()
      // to maintain existing behavior...
      //
      try
        if Connected then begin
          DisconnectNotifyPeer;
        end;
      except
        // TODO: maybe allow only EIdConnClosedGracefully and EIdSocketError?
      end;
    end;
  finally
    {
     there are a few possible situations here:
     1) we are still connected, then everything works as before,
        status disconnecting, then disconnect, status disconnected
     2) we are not connected, and this is just some "rogue" call to
        disconnect(), then nothing happens
     3) we are not connected, because ClosedGracefully, then
        LConnected will be false, but the implicit call to
        CheckForDisconnect (inside Connected) will call the events
    }
    // We dont check connected here - we realy dont care about actual socket state
    // Here we just want to close the actual IOHandler. It is very possible for a
    // socket to be disconnected but the IOHandler still open. In this case we only
    // care of the IOHandler is still open.
    //
    // This is especially important if the socket has been disconnected with error, at this
    // point we just want to ignore it and checking .Connected would trigger this. We
    // just want to close. For some reason NS 7.1 (And only 7.1, not 7.0 or Mozilla) cause
    // CONNABORTED. So its extra important we just disconnect without checking socket state.
    LIOHandler := IOHandler;
    if Assigned(LIOHandler) then begin
      if LIOHandler.Opened then begin
        DoStatus(hsDisconnecting);
        LIOHandler.Close;
        DoOnDisconnected;
        DoStatus(hsDisconnected);
        //LIOHandler.InputBuffer.Clear;
      end;
    end;
  end;
end;

procedure TIdTCPConnection.DoOnDisconnected;
begin
  if Assigned(OnDisconnected) then begin
    OnDisconnected(Self);
  end;
end;

function TIdTCPConnection.GetResponse(const AAllowedResponses: array of Int16;
  AEncoding: IIdTextEncoding = nil): Int16;
begin
  GetInternalResponse(AEncoding);
  Result := CheckResponse(LastCmdResult.NumericCode, AAllowedResponses);
end;

procedure TIdTCPConnection.RaiseExceptionForLastCmdResult(
 AException: TClassIdException);
begin
  raise AException.Create(LastCmdResult.Text.Text);
end;

procedure TIdTCPConnection.RaiseExceptionForLastCmdResult;
begin
  LastCmdResult.RaiseReplyError;
end;

function TIdTCPConnection.SendCmd(AOut: string; const AResponse: Array of Int16;
  AEncoding: IIdTextEncoding = nil): Int16;
begin
  CheckConnected;
  PrepareCmd(AOut);
  IOHandler.WriteLn(AOut, AEncoding);
  Result := GetResponse(AResponse, AEncoding);
end;

// under ARC, all weak references to a freed object get nil'ed automatically
// so this is mostly redundant
procedure TIdTCPConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then begin
    {$IFNDEF USE_OBJECT_ARC}
    if (AComponent = FIntercept) then begin
      FIntercept := nil;
    end else
    {$ENDIF}
    if (AComponent = FIOHandler) then begin
      FIOHandler := nil;
      FSocket := nil;
      FManagedIOHandler := False;
    end;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TIdTCPConnection.SetIntercept(AValue: TIdConnectionIntercept);
var
  // under ARC, convert weak references to strong references before working with them
  LIntercept: TIdConnectionIntercept;
  LIOHandler: TIdIOHandler;
begin
  LIntercept := FIntercept;

  if LIntercept <> AValue then
  begin
    LIOHandler := IOHandler;

    // RLebeau 8/25/09 - normally, short-circuit logic should skip all subsequent
    // evaluations in a multi-condition statement once one of the conditions
    // evaluates to False.  However, a user just ran into a situation where that
    // was not the case!  It caused an AV in SetIOHandler() further below when
    // AValue was nil (from Destroy() further above) because Assigned(AValue.Intercept)
    // was still being evaluated even though Assigned(AValue) was returning False.
    // SetIntercept() is using the same kind of short-circuit logic here as well.
    // Let's not rely on short-circuiting anymore, just to be on the safe side.
    //
    // old code: if Assigned(IOHandler) and Assigned(IOHandler.Intercept) and Assigned(AValue) and (AValue <> IOHandler.Intercept) then begin
    //
    if Assigned(LIOHandler) and Assigned(AValue) then begin
      if Assigned(LIOHandler.Intercept) and (LIOHandler.Intercept <> AValue) then begin
        raise EIdException.Create(RSInterceptIsDifferent);
      end;
    end;

    // TODO: should LIntercept.Connection be set to nil here if LIntercept
    // is not nil and LIntercept.Connection is set to Self?

    {$IFDEF USE_OBJECT_ARC}
    // under ARC, all weak references to a freed object get nil'ed automatically
    FIntercept := AValue;
    {$ELSE}
    // remove self from the Intercept's free notification list
    if Assigned(LIntercept) then begin
      LIntercept.RemoveFreeNotification(Self);
    end;
    FIntercept := AValue;
    // add self to the Intercept's free notification list
    if Assigned(AValue) then begin
      AValue.FreeNotification(Self);
    end;
    {$ENDIF}

    if Assigned(LIOHandler) then begin
      LIOHandler.Intercept := AValue;
    end;

    // TODO: should FIntercept.Connection be set to Self here if FIntercept
    // is not nil?
  end;
end;

procedure TIdTCPConnection.SetIOHandler(AValue: TIdIOHandler);
var
  // under ARC, convert weak references to strong references before working with them
  LIOHandler: TIdIOHandler;
  LIntercept, LOtherIntercept: TIdConnectionIntercept;
begin
  LIOHandler := FIOHandler;

  if LIOHandler <> AValue then begin
    LIntercept := FIntercept;

    // RLebeau 8/25/09 - normally, short-circuit logic should skip all subsequent
    // evaluations in a multi-condition statement once one of the conditions
    // evaluates to False.  However, a user just ran into a situation where that
    // was not the case!  It caused an AV when AValue was nil (from Destroy()
    // further above) because Assigned(AValue.Intercept) was still being evaluated
    // even though Assigned(AValue) was returning False.  Let's not rely on
    // short-circuiting anymore, just to be on the safe side.
    //
    // old code: if Assigned(AValue) and Assigned(AValue.Intercept) and Assigned(FIntercept) and (AValue.Intercept <> FIntercept) then begin
    //
    if Assigned(AValue) and Assigned(LIntercept) then begin
      LOtherIntercept := AValue.Intercept;
      if Assigned(LOtherIntercept) then begin
        if LOtherIntercept <> LIntercept then begin
          raise EIdException.Create(RSInterceptIsDifferent);
        end;
        {$IFDEF USE_OBJECT_ARC}LOtherIntercept := nil;{$ENDIF}
      end;
    end;

    if ManagedIOHandler then begin
      if Assigned(LIOHandler) then begin
        FIOHandler := nil;
        IdDisposeAndNil(LIOHandler);
      end;
      ManagedIOHandler := False;
    end;

    // under ARC, all weak references to a freed object get nil'ed automatically

    // Reset this if nil (to match nil, but not needed) or when a new IOHandler is specified
    // If true, code must set it after the IOHandler is set
    // Must do after call to FreeManagedIOHandler
    FSocket := nil;

    // Clear out old values whether setting AValue to nil, or setting a new value
    if Assigned(LIOHandler) then begin
      LIOHandler.WorkTarget := nil;
      {$IFNDEF USE_OBJECT_ARC}
      LIOHandler.RemoveFreeNotification(Self);
      {$ENDIF}
    end;

    if Assigned(AValue) then begin
      {$IFNDEF USE_OBJECT_ARC}
      // add self to the IOHandler's free notification list
      AValue.FreeNotification(Self);
      {$ENDIF}
      // Must set to handlers and not events directly as user may change
      // the events of TCPConnection after we have initialized these and then
      // these would point to old values
      AValue.WorkTarget := Self;
      if Assigned(LIntercept) then begin
        AValue.Intercept := LIntercept;
      end;
      if AValue is TIdIOHandlerSocket then begin
        FSocket := TIdIOHandlerSocket(AValue);
      end;
    end;

    // Last as some code uses FIOHandler to finalize items
    FIOHandler := AValue;
  end;
end;

procedure TIdTCPConnection.WriteHeader(AHeader: TStrings);
var
  i: Integer;
  LBufferingStarted: Boolean;
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdIOHandler;
begin
  CheckConnected;
  LIOHandler := IOHandler;
  LBufferingStarted := not LIOHandler.WriteBufferingActive;
  if LBufferingStarted then begin
    LIOHandler.WriteBufferOpen;
  end;
  try
    for i := 0 to AHeader.Count -1 do begin
      // No ReplaceAll flag - we only want to replace the first one
      LIOHandler.WriteLn(ReplaceOnlyFirst(AHeader[i], '=', ': '));
    end;
    LIOHandler.WriteLn;
    if LBufferingStarted then begin
      LIOHandler.WriteBufferClose;
    end;
  except
    if LBufferingStarted then begin
      LIOHandler.WriteBufferCancel;
    end;
    raise;
  end;
end;

function TIdTCPConnection.SendCmd(AOut: string; const AResponse: Int16 = -1;
  AEncoding: IIdTextEncoding = nil): Int16;
begin
  if AResponse < 0 then begin
    Result := SendCmd(AOut, [], AEncoding);
  end else begin
    Result := SendCmd(AOut, [AResponse], AEncoding);
  end;
end;

procedure TIdTCPConnection.CheckForGracefulDisconnect(ARaiseExceptionIfDisconnected: Boolean);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdIOHandler;
begin
  LIOHandler := IOHandler;
  if Assigned(LIOHandler) then begin
    LIOHandler.CheckForDisconnect(ARaiseExceptionIfDisconnected);
  end else if ARaiseExceptionIfDisconnected then begin
    raise EIdException.Create(RSNotConnected);
  end;
end;

function TIdTCPConnection.CheckResponse(const AResponse: Int16;
 const AAllowedResponses: array of Int16): Int16;
begin
  if High(AAllowedResponses) > -1 then begin
    if PosInSmallIntArray(AResponse, AAllowedResponses) = -1 then begin
      RaiseExceptionForLastCmdResult;
    end;
  end;
  Result := AResponse;
end;

procedure TIdTCPConnection.GetInternalResponse(AEncoding: IIdTextEncoding = nil);
var
  LLine: string;
  LResponse: TStringList;
  // under ARC, convert a weak reference to a strong reference before working with it
  LIOHandler: TIdIOHandler;
begin
  CheckConnected;
  LResponse := TStringList.Create;
  try
    // Some servers with bugs send blank lines before reply. Dont remember which
    // ones, but I do remember we changed this for a reason
    // RLebeau 9/14/06: this can happen in between lines of the reply as well
    LIOHandler := IOHandler;
    repeat
      LLine := LIOHandler.ReadLnWait(MaxInt, AEncoding);
      LResponse.Add(LLine);
    until FLastCmdResult.IsEndMarker(LLine);
    //Note that FormattedReply uses an assign in it's property set method.
    FLastCmdResult.FormattedReply := LResponse;
  finally
    FreeAndNil(LResponse);
  end;
end;

procedure TIdTCPConnection.WriteRFCStrings(AStrings: TStrings);
begin
  CheckConnected;
  IOHandler.WriteRFCStrings(AStrings, True);
end;

function TIdTCPConnection.GetResponse(const AAllowedResponse: Int16 = -1;
  AEncoding: IIdTextEncoding = nil): Int16;
begin
  if AAllowedResponse < 0 then begin
    Result := GetResponse([], AEncoding);
  end else begin
    Result := GetResponse([AAllowedResponse], AEncoding);
  end;
end;

function TIdTCPConnection.GetResponse(const AAllowedResponse: string;
  AEncoding: IIdTextEncoding = nil): string;
begin
  GetInternalResponse(AEncoding);
  Result := CheckResponse(LastCmdResult.Code, AAllowedResponse);
end;

function TIdTCPConnection.SendCmd(AOut: string; const AResponse: string;
  AEncoding: IIdTextEncoding = nil): string;
begin
  CheckConnected;
  PrepareCmd(AOut);
  IOHandler.WriteLn(AOut, AEncoding);
  Result := GetResponse(AResponse, AEncoding);
end;

function TIdTCPConnection.CheckResponse(const AResponse, AAllowedResponse: string): string;
begin
  if (AAllowedResponse <> '')
   and (not TextIsSame(AResponse, AAllowedResponse)) then begin
    RaiseExceptionForLastCmdResult;
  end;
  Result := AResponse;
end;

procedure TIdTCPConnection.WorkBeginEvent(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  BeginWork(AWorkMode, AWorkCountMax)
end;

procedure TIdTCPConnection.WorkEndEvent(ASender: TObject; AWorkMode: TWorkMode);
begin
  EndWork(AWorkMode)
end;

procedure TIdTCPConnection.WorkEvent(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  DoWork(AWorkMode, AWorkCount)
end;

procedure TIdTCPConnection.InitComponent;
begin
  inherited InitComponent;
  FReplyClass := GetReplyClass;
  FGreeting := FReplyClass.CreateWithReplyTexts(nil, nil);
  FLastCmdResult := FReplyClass.CreateWithReplyTexts(nil, nil);
end;

procedure TIdTCPConnection.CheckConnected;
begin
  if not Assigned(IOHandler) then begin
    raise EIdNotConnected.Create(RSNotConnected);
  end;
end;

procedure TIdTCPConnection.SetGreeting(AValue: TIdReply);
begin
  FGreeting.Assign(AValue);
end;

procedure TIdTCPConnection.Disconnect;
begin
  // The default should be to tell the other side we are disconnecting
  Disconnect(True);
end;

procedure TIdTCPConnection.DisconnectNotifyPeer;
begin
end;

procedure TIdTCPConnection.PrepareCmd(var aCmd: string);
begin
  //Leave this empty here.  It's for cases where we may need to
  // override what is sent to a server in a transparent manner.
end;

end.

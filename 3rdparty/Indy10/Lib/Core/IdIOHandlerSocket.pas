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
  Rev 1.38    11/10/2004 8:25:54 AM  JPMugaas
  Fix for AV caused by short-circut boolean evaluation.

  Rev 1.37    27.08.2004 21:58:20  Andreas Hausladen
  Speed optimization ("const" for string parameters)

  Rev 1.36    8/2/04 5:44:40 PM  RLebeau
  Moved ConnectTimeout over from TIdIOHandlerStack

  Rev 1.35    7/21/2004 12:22:32 PM  BGooijen
  Fix to .connected

  Rev 1.34    6/30/2004 12:31:34 PM  BGooijen
  Added OnSocketAllocated

  Rev 1.33    4/24/04 12:52:52 PM  RLebeau
  Added setter method to UseNagle property

  Rev 1.32    2004.04.18 12:52:02 AM  czhower
  Big bug fix with server disconnect and several other bug fixed that I found
  along the way.

  Rev 1.31    2004.02.03 4:16:46 PM  czhower
  For unit name changes.

  Rev 1.30    2/2/2004 11:46:46 AM  BGooijen
  Dotnet and TransparentProxy

  Rev 1.29    2/1/2004 9:44:00 PM  JPMugaas
  Start on reenabling Transparant proxy.

  Rev 1.28    2004.01.20 10:03:28 PM  czhower
  InitComponent

  Rev 1.27    1/2/2004 12:02:16 AM  BGooijen
  added OnBeforeBind/OnAfterBind

  Rev 1.26    12/31/2003 9:51:56 PM  BGooijen
  Added IPv6 support

  Rev 1.25    11/4/2003 10:37:40 PM  BGooijen
  JP's patch to fix the bound port

  Rev 1.24    10/19/2003 5:21:26 PM  BGooijen
  SetSocketOption

  Rev 1.23    10/18/2003 1:44:06 PM  BGooijen
  Added include

  Rev 1.22    2003.10.14 1:26:54 PM  czhower
  Uupdates + Intercept support

  Rev 1.21    10/9/2003 8:09:06 PM  SPerry
  bug fixes

  Rev 1.20    8/10/2003 2:05:50 PM  SGrobety
  Dotnet

  Rev 1.19    2003.10.07 10:18:26 PM  czhower
  Uncommneted todo code that is now non dotnet.

  Rev 1.18    2003.10.02 8:23:42 PM  czhower
  DotNet Excludes

  Rev 1.17    2003.10.01 9:11:18 PM  czhower
  .Net

  Rev 1.16    2003.10.01 5:05:12 PM  czhower
  .Net

  Rev 1.15    2003.10.01 2:46:38 PM  czhower
  .Net

  Rev 1.14    2003.10.01 11:16:32 AM  czhower
  .Net

  Rev 1.13    2003.09.30 1:22:58 PM  czhower
  Stack split for DotNet

  Rev 1.12    7/4/2003 08:26:44 AM  JPMugaas
  Optimizations.

  Rev 1.11    7/1/2003 03:39:44 PM  JPMugaas
  Started numeric IP function API calls for more efficiency.

  Rev 1.10    2003.06.30 5:41:56 PM  czhower
  -Fixed AV that occurred sometimes when sockets were closed with chains
  -Consolidated code that was marked by a todo for merging as it no longer
  needed to be separate
  -Removed some older code that was no longer necessary

  Passes bubble tests.

  Rev 1.9    6/3/2003 11:45:58 PM  BGooijen
  Added .Connected

  Rev 1.8    2003.04.22 7:45:34 PM  czhower

  Rev 1.7    4/2/2003 3:24:56 PM  BGooijen
  Moved transparantproxy from ..stack to ..socket

  Rev 1.6    2/28/2003 9:51:56 PM  BGooijen
  removed the field: FReadTimeout: Integer, it hided the one in TIdIOHandler

  Rev 1.5    2/26/2003 1:15:38 PM  BGooijen
  FBinding is now freed in IdIOHandlerSocket, instead of in IdIOHandlerStack

  Rev 1.4    2003.02.25 1:36:08 AM  czhower

  Rev 1.3    2002.12.07 12:26:26 AM  czhower

  Rev 1.2    12-6-2002 20:09:14  BGooijen
  Changed SetDestination to search for the last ':', instead of the first

  Rev 1.1    12-6-2002 18:54:14  BGooijen
  Added IPv6-support

  Rev 1.0    11/13/2002 08:45:08 AM  JPMugaas
}

unit IdIOHandlerSocket;

interface

{$I IdCompilerDefines.inc}

uses
  Classes,
  IdCustomTransparentProxy,
  IdBaseComponent,
  IdGlobal,
  IdIOHandler,
  IdSocketHandle;

const
  IdDefTimeout = 0;
  IdBoundPortDefault = 0;

type
  {
  TIdIOHandlerSocket is the base class for socket IOHandlers that implement a
  binding.

  Descendants
    -TIdIOHandlerStack
    -TIdIOHandlerChain
  }
  TIdIOHandlerSocket = class(TIdIOHandler)
  protected
    FBinding: TIdSocketHandle;
    FBoundIP: string;
    FBoundPort: TIdPort;
    FBoundPortMax: TIdPort;
    FBoundPortMin: TIdPort;
    FDefaultPort: TIdPort;
    FOnBeforeBind: TNotifyEvent;
    FOnAfterBind: TNotifyEvent;
    FOnSocketAllocated: TNotifyEvent;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FTransparentProxy: TIdCustomTransparentProxy;
    FImplicitTransparentProxy: Boolean;
    FUseNagle: Boolean;
    FReuseSocket: TIdReuseSocket;
    FIPVersion: TIdIPVersion;
    //
    procedure ConnectClient; virtual;
    procedure DoBeforeBind; virtual;
    procedure DoAfterBind; virtual;
    procedure DoSocketAllocated; virtual;
    procedure InitComponent; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetDestination: string; override;
    procedure SetDestination(const AValue: string); override;
    function GetReuseSocket: TIdReuseSocket;
    procedure SetReuseSocket(AValue: TIdReuseSocket);
    function GetTransparentProxy: TIdCustomTransparentProxy; virtual;
    procedure SetTransparentProxy(AProxy: TIdCustomTransparentProxy); virtual;
    function GetUseNagle: Boolean;
    procedure SetUseNagle(AValue: Boolean);
    //
    function SourceIsAvailable: Boolean; override;
    function CheckForError(ALastResult: Integer): Integer; override;
    procedure RaiseError(AError: Integer); override;
  public
    procedure AfterAccept; override;
    destructor Destroy; override;
    function BindingAllocated: Boolean;
    procedure Close; override;
    function Connected: Boolean; override;
    procedure Open; override;
    function WriteFile(const AFile: String; AEnableTransferFile: Boolean = False): Int64; override;
    //
    property Binding: TIdSocketHandle read FBinding;
    property BoundPortMax: TIdPort read FBoundPortMax write FBoundPortMax;
    property BoundPortMin: TIdPort read FBoundPortMin write FBoundPortMin;
    // events
    property OnBeforeBind: TNotifyEvent read FOnBeforeBind write FOnBeforeBind;
    property OnAfterBind: TNotifyEvent read FOnAfterBind write FOnAfterBind;
    property OnSocketAllocated: TNotifyEvent read FOnSocketAllocated write FOnSocketAllocated;
  published
    property BoundIP: string read FBoundIP write FBoundIP;
    property BoundPort: TIdPort read FBoundPort write FBoundPort default IdBoundPortDefault;
    property DefaultPort: TIdPort read FDefaultPort write FDefaultPort;
    property IPVersion: TIdIPVersion read FIPVersion write FIPVersion default ID_DEFAULT_IP_VERSION;
    property ReuseSocket: TIdReuseSocket read GetReuseSocket write SetReuseSocket default rsOSDependent;
    property TransparentProxy: TIdCustomTransparentProxy read GetTransparentProxy write SetTransparentProxy;
    property UseNagle: boolean read GetUseNagle write SetUseNagle default True;
  end;

implementation

uses
  //facilitate inlining only.
  {$IFDEF DOTNET}
    {$IFDEF USE_INLINE}
  System.IO,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WIN32_OR_WIN64 }
  Windows,
  {$ENDIF}
  SysUtils,
  IdStack,
  IdStackConsts,
  IdSocks;

{ TIdIOHandlerSocket }

procedure TIdIOHandlerSocket.AfterAccept;
begin
  inherited AfterAccept;
  FIPVersion := FBinding.IPVersion;
end;

procedure TIdIOHandlerSocket.Close;
begin
  if FBinding <> nil then begin
    FBinding.CloseSocket;
  end;
  inherited Close;
end;

procedure TIdIOHandlerSocket.ConnectClient;
var
  LBinding: TIdSocketHandle;
begin
  LBinding := Binding;
  DoBeforeBind;
  // Allocate the socket
  LBinding.IPVersion := FIPVersion;
  LBinding.AllocateSocket;
  DoSocketAllocated;
  // Bind the socket
  if BoundIP <> '' then begin
    LBinding.IP := BoundIP;
  end;
  LBinding.Port := BoundPort;
  LBinding.ClientPortMin := BoundPortMin;
  LBinding.ClientPortMax := BoundPortMax;
  LBinding.ReuseSocket := FReuseSocket;

  // RLebeau 11/15/2014: Using the socket bind() function in a Mac OSX sandbox
  // causes the Apple store to reject an app with the following error if it
  // uses Indy client(s) and no Indy server(s):
  //
  // "This app uses one or more entitlements which do not have matching
  // functionality within the app. Apps should have only the minimum set of
  // entitlements necessary for the app to function properly. Please remove
  // all entitlements that are not needed by your app and submit an updated
  // binary for review, including the following:
  //
  // com.apple.security.network.server"
  //
  // Ideally, TIdSocketHandle.Bind() should not call TryBind() if the IP is
  // blank and the Port, ClientPortMin, and ClientPortMax are all 0.  However,
  // TIdSocketHandle.Bind() is used for both clients and servers, and sometimes
  // a server needs to bind to port 0 to get a random ephemeral port, which it
  // can then report to clients.  So lets do the check here instead, as this
  // method is only used for clients...

  {$IFDEF DARWIN}
  // TODO: remove the DARWIN check and just skip the Bind() on all platforms?
  if (LBinding.IP <> '') or (LBinding.Port <> 0) or
     ((LBinding.ClientPortMin <> 0) and (LBinding.ClientPortMax <> 0)) then
  begin
    LBinding.Bind;
  end;
  {$ELSE}
  LBinding.Bind;
  {$ENDIF}

  // Turn off Nagle if specified
  LBinding.UseNagle := FUseNagle;
  DoAfterBind;
end;

function TIdIOHandlerSocket.Connected: Boolean;
begin
  Result := (BindingAllocated and inherited Connected) or (not InputBufferIsEmpty);
end;

destructor TIdIOHandlerSocket.Destroy;
begin
  SetTransparentProxy(nil);
  FreeAndNil(FBinding);
  inherited Destroy;
end;

procedure TIdIOHandlerSocket.DoBeforeBind;
begin
  if Assigned(FOnBeforeBind) then begin
    FOnBeforeBind(self);
  end;
end;

procedure TIdIOHandlerSocket.DoAfterBind;
begin
  if Assigned(FOnAfterBind) then begin
    FOnAfterBind(self);
  end;
end;

procedure TIdIOHandlerSocket.DoSocketAllocated;
begin
  if Assigned(FOnSocketAllocated) then begin
    FOnSocketAllocated(self);
  end;
end;

function TIdIOHandlerSocket.GetDestination: string;
begin
  Result := Host;
  if (Port <> DefaultPort) and (Port > 0) then begin
    Result := Host + ':' + IntToStr(Port);
  end;
end;

procedure TIdIOHandlerSocket.Open;
begin
  inherited Open;

  if not Assigned(FBinding) then begin
    FBinding := TIdSocketHandle.Create(nil);
  end else begin
    FBinding.Reset(True);
  end;
  FBinding.ClientPortMin := BoundPortMin;
  FBinding.ClientPortMax := BoundPortMax;

  //if the IOHandler is used to accept connections then port+host will be empty
  if (Host <> '') and (Port > 0) then begin
    ConnectClient;
  end;
end;

procedure TIdIOHandlerSocket.SetDestination(const AValue: string);
var
  LPortStart: integer;
begin
  // Bas Gooijen 06-Dec-2002: Changed to search the last ':', instead of the first:
  LPortStart := LastDelimiter(':', AValue);
  if LPortStart > 0 then begin
    Host := Copy(AValue, 1, LPortStart-1);
    Port := IndyStrToInt(Trim(Copy(AValue, LPortStart + 1, $FF)), DefaultPort);
  end;
end;

function TIdIOHandlerSocket.BindingAllocated: Boolean;
begin
  Result := FBinding <> nil;
  if Result then begin
    Result := FBinding.HandleAllocated;
  end;
end;

function TIdIOHandlerSocket.WriteFile(const AFile: String;
  AEnableTransferFile: Boolean): Int64;
//TODO: There is a way in linux to dump a file to a socket as well. use it.
  {$IFDEF WIN32_OR_WIN64}
var
  LOldErrorMode : Integer;
  {$ENDIF}
begin
  Result := 0;
  {$IFDEF WIN32_OR_WIN64}
  LOldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
  {$ENDIF}
  if FileExists(AFile) then begin
    if Assigned(GServeFileProc) and (not WriteBufferingActive)
     {and (Intercept = nil)} and AEnableTransferFile
     then begin
      Result := GServeFileProc(Binding.Handle, AFile);
      Exit;
    end
    else
    begin
      Result := inherited WriteFile(AFile, AEnableTransferFile);
    end;
  end;
  {$IFDEF WIN32_OR_WIN64}
  finally
    SetErrorMode(LOldErrorMode)
  end;
  {$ENDIF}
end;

function TIdIOHandlerSocket.GetReuseSocket: TIdReuseSocket;
begin
  if FBinding <> nil then begin
    Result := FBinding.ReuseSocket;
  end else begin
    Result := FReuseSocket;
  end;
end;

procedure TIdIOHandlerSocket.SetReuseSocket(AValue: TIdReuseSocket);
begin
  FReuseSocket := AValue;
  if FBinding <> nil then begin
    FBinding.ReuseSocket := AValue;
  end;
end;

procedure TIdIOHandlerSocket.SetTransparentProxy(AProxy : TIdCustomTransparentProxy);
var
  LClass: TIdCustomTransparentProxyClass;
  // under ARC, convert a weak reference to a strong reference before working with it
  LTransparentProxy: TIdCustomTransparentProxy;
begin
  LTransparentProxy := FTransparentProxy;

  if LTransparentProxy <> AProxy then
  begin
    // All this is to preserve the compatibility with old version
    // In the case when we have SocksInfo as object created in runtime without owner form it is treated as temporary object
    // In the case when the ASocks points to an object with owner it is treated as component on form.

    // under ARC, all weak references to a freed object get nil'ed automatically

    if Assigned(AProxy) then begin
      if not Assigned(AProxy.Owner) then begin
        if Assigned(LTransparentProxy) and (not FImplicitTransparentProxy) then begin
          FTransparentProxy := nil;
          {$IFNDEF USE_OBJECT_ARC}
          LTransparentProxy.RemoveFreeNotification(Self);
          {$ENDIF}
        end;
        LClass := TIdCustomTransparentProxyClass(AProxy.ClassType);
        if Assigned(LTransparentProxy) and (LTransparentProxy.ClassType <> LClass) then begin
          FTransparentProxy := nil;
          FImplicitTransparentProxy := False;
          IdDisposeAndNil(LTransparentProxy);
        end;
        if not Assigned(LTransparentProxy) then begin
          LTransparentProxy := LClass.Create(Self);
          FTransparentProxy := LTransparentProxy;
          FImplicitTransparentProxy := True;
        end;
        LTransparentProxy.Assign(AProxy);
      end else begin
        if Assigned(LTransparentProxy) then begin
          if FImplicitTransparentProxy then begin
            FTransparentProxy := nil;
            FImplicitTransparentProxy := False;
            IdDisposeAndNil(LTransparentProxy);
          end else begin
            {$IFNDEF USE_OBJECT_ARC}
            LTransparentProxy.RemoveFreeNotification(Self);
            {$ENDIF}
          end;
        end;
        FTransparentProxy := AProxy;
        {$IFNDEF USE_OBJECT_ARC}
        AProxy.FreeNotification(Self);
        {$ENDIF}
      end;
    end
    else if Assigned(LTransparentProxy) then begin
      if FImplicitTransparentProxy then begin
        FTransparentProxy := nil;
        FImplicitTransparentProxy := False;
        IdDisposeAndNil(LTransparentProxy);
      end else begin
        FTransparentProxy := nil;
        {$IFNDEF USE_OBJECT_ARC}
        LTransparentProxy.RemoveFreeNotification(Self);
        {$ENDIF}
      end;
    end;
  end;
end;

function TIdIOHandlerSocket.GetTransparentProxy: TIdCustomTransparentProxy;
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LTransparentProxy: TIdCustomTransparentProxy;
begin
  LTransparentProxy := FTransparentProxy;
  // Necessary at design time for Borland SOAP support
  if LTransparentProxy = nil then begin
    LTransparentProxy := TIdSocksInfo.Create(Self); //default
    FTransparentProxy := LTransparentProxy;
    FImplicitTransparentProxy := True;
  end;
  Result := LTransparentProxy;
end;

function TIdIOHandlerSocket.GetUseNagle: Boolean;
begin
  if FBinding <> nil then begin
    Result := FBinding.UseNagle;
  end else begin
    Result := FUseNagle;
  end;
end;

procedure TIdIOHandlerSocket.SetUseNagle(AValue: Boolean);
begin
  FUseNagle := AValue;
  if FBinding <> nil then begin
    FBinding.UseNagle := AValue;
  end;
end;

// under ARC, all weak references to a freed object get nil'ed automatically
// so this is mostly redundant
procedure TIdIOHandlerSocket.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTransparentProxy) then begin
    FTransparentProxy := nil;
    FImplicitTransparentProxy := False;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TIdIOHandlerSocket.InitComponent;
begin
  inherited InitComponent;
  FUseNagle := True;
  FIPVersion := ID_DEFAULT_IP_VERSION;
end;

function TIdIOHandlerSocket.SourceIsAvailable: Boolean;
begin
  Result := BindingAllocated;
end;

function TIdIOHandlerSocket.CheckForError(ALastResult: Integer): Integer;
begin
  Result := GStack.CheckForSocketError(ALastResult, [Id_WSAESHUTDOWN, Id_WSAECONNABORTED, Id_WSAECONNRESET, Id_WSAETIMEDOUT]);
end;

procedure TIdIOHandlerSocket.RaiseError(AError: Integer);
begin
  GStack.RaiseSocketError(AError);
end;

end.

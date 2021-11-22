{*******************************************************}
{                                                       }
{       EOS Unit for SOLDAT                             }
{                                                       }
{       Copyright (c) 2021 Soldat Team                  }
{                                                       }
{       For use with EOS SDK  v1.14.1                   }
{                                                       }
{*******************************************************}

unit EOS;
{$IFDEF UNIX}
{$PACKRECORDS 4}
{$PACKENUM 4}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}
interface

uses
  ctypes, SysUtils, Classes, 
  GameNetworkingSockets, Net, Version, NetworkUtils, Constants, TraceLog
  {$IFDEF SERVER}{$IFDEF STEAM}, SteamTypes{$ENDIF}, NetworkServerConnection{$ENDIF};

const
  {$IFDEF WINDOWS}
  EOSLIB = 'EOSSDK-Win64-Shipping.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  EOSLIB = 'libEOSSDK-Linux-Shipping.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  EOSLIB = 'libEOSSDK-Mac-Shipping.dylib';
  {$ENDIF}

  EOS_PRODUCT_ID = {$INCLUDE %EOS_PRODUCT_ID%};
  EOS_SANDBOX_ID = {$INCLUDE %EOS_SANDBOX_ID%};
  EOS_DEPLOYMENT_ID = {$INCLUDE %EOS_DEPLOYMENT_ID%};

  {$IFDEF SERVER}
  EOS_SERVER_ID = {$INCLUDE %EOS_SERVER_ID%};
  EOS_SERVER_SECRET = {$INCLUDE %EOS_SERVER_SECRET%};
  {$ELSE}
  EOS_CLIENT_ID = {$INCLUDE %EOS_CLIENT_ID%};
  EOS_CLIENT_SECRET = {$INCLUDE %EOS_CLIENT_SECRET%};
  {$ENDIF}

{$PACKRECORDS 8}
{$PACKENUM 4}
{$include eos_base.pas}
{$include eos_logging.pas}
{$include eos_types.pas}
{$include eos_init.pas}
{$include eos_connect_types.pas}
{$include eos_connect.pas}
{$include eos_anticheatcommon_types.pas}
{$include eos_anticheatserver_types.pas}
{$include eos_anticheatserver.pas}
{$include eos_anticheatclient_types.pas}
{$include eos_anticheatclient.pas}
{$include eos_sdk.pas}
{$include eos_windows.pas}

{$IFDEF SERVER}
procedure ServerHandleEACMessage(NetMessage: PSteamNetworkingMessage_t);
{$ELSE}
procedure ClientHandleEACMessage(NetMessage: PSteamNetworkingMessage_t);
{$ENDIF}

type
  TEpicOnlineServices = class
  private
    FPlatformHandle: EOS_HPlatform;
  public
    constructor Create(LogLevel: Integer);
    destructor Destroy; override;
    procedure OnClockTick();
  end;

  {$IFNDEF SERVER}
  TEpicOnlineServicesClient = class(TEpicOnlineServices)
  private
    FAntiCheatHandle: EOS_HAntiCheatClient;
    FAddNotifyMessageToServerId: EOS_NotificationId;
    FConnectHandle: EOS_HConnect;
    FLocalUser: EOS_ProductUserId;
    FPollStatusOptions: EOS_AntiCheatClient_PollStatusOptions;
    procedure OnMessageToServer(Data: PEOS_AntiCheatClient_OnMessageToServerCallbackInfo); stdcall;
    procedure OnCreateUser(Data: PEOS_Connect_CreateUserCallbackInfo);
    procedure OnLoginConnect(Data: PEOS_Connect_LoginCallbackInfo); stdcall;
  public
    constructor Create(LogLevel: Integer);
    destructor Destroy; override;
    procedure OnClockTick();
    {$IFDEF STEAM}
    procedure OnSteamAuth();
    {$ENDIF}
    procedure OpenIDLogin();
    procedure BeginSession();
    procedure EndSession();
    property AntiCheatHandle: EOS_HAntiCheatClient read FAntiCheatHandle;
  end;
  {$ELSE}

  TEpicOnlineServicesServer = class(TEpicOnlineServices)
  private
    FAntiCheatHandle: EOS_HAntiCheatServer;
    FAddNotifyMessageToClientId: EOS_NotificationId;
    FAddNotifyClientActionRequiredId: EOS_NotificationId;
    FAddNotifyClientAuthStatusChangedId: EOS_NotificationId;
    procedure OnMessageToClient(Data: PEOS_AntiCheatCommon_OnMessageToClientCallbackInfo); stdcall;
    procedure OnClientActionRequired(Data: PEOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo); stdcall;
    procedure OnClientAuthStatusChanged(Data: PEOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo); stdcall;
  public
    constructor Create(LogLevel: Integer; ServerName: String; Timeout: Integer);
    destructor Destroy; override;
    procedure RegisterClient(Peer: Pointer);
    procedure UnregisterClient(Peer: Pointer);
    property AntiCheatHandle: EOS_HAntiCheatClient read FAntiCheatHandle;
  end;
  {$ENDIF}

  {$IFDEF SERVER}
  procedure OnMessageToClientCallback(Data: PEOS_AntiCheatCommon_OnMessageToClientCallbackInfo); stdcall;
  procedure OnClientActionRequiredCallback(Data: PEOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo); stdcall;
  procedure OnClientAuthStatusChangedCallback(Data: PEOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo); stdcall;
  {$ELSE}
  procedure OnMessageToServerCallback(Data: PEOS_AntiCheatClient_OnMessageToServerCallbackInfo); stdcall;
  procedure OnCreateUserCallback(Data: PEOS_Connect_CreateUserCallbackInfo);
  procedure OnLoginConnectCallback(Data: PEOS_Connect_LoginCallbackInfo); stdcall;
  {$ENDIF}
  procedure EOS_LogHandler(var Message: EOS_LogMessage); stdcall;

implementation

{$IFDEF SERVER}
uses Server;
{$ELSE}
uses Client;
{$ENDIF}

constructor TEpicOnlineServices.Create(LogLevel: Integer);
var
  InitializeOptions: EOS_InitializeOptions;
  PlatformOptions: EOS_Platform_Options;
  EOSResult: EOS_EResult;
{$IFDEF WINDOWS}
  RtcOptions: EOS_Platform_RTCOptions;
  WindowsRTC: EOS_Windows_RTCOptions;
{$ENDIF}
begin
  InitializeOptions := Default(EOS_InitializeOptions);
  InitializeOptions.ApiVersion := EOS_INITIALIZE_API_LATEST;
  InitializeOptions.AllocateMemoryFunction := nil;
  InitializeOptions.ReallocateMemoryFunction := nil;
  InitializeOptions.ReleaseMemoryFunction := nil;
  InitializeOptions.ProductName := PChar('Soldat');
  InitializeOptions.ProductVersion := PChar(SOLDAT_VERSION);
  InitializeOptions.Reserved := nil;
  InitializeOptions.SystemInitializeOptions := nil;
  InitializeOptions.OverrideThreadAffinity := nil;

  EOSResult := EOS_Initialize(@InitializeOptions);

  if EOSResult <> EOS_SUCCESS then
    raise Exception.Create('EOS_Initialize has failed: ' + EOS_EResult_ToString(EOSResult));

  EOSResult := EOS_Logging_SetLogLevel(EOS_LC_ALL_CATEGORIES, EOS_ELogLevel(LogLevel));

  if EOSResult <> EOS_SUCCESS then
    raise Exception.Create('EOS_Logging_SetLogLevel has failed: ' + EOS_EResult_ToString(EOSResult));

  EOSResult := EOS_Logging_SetCallback(@EOS_LogHandler);

  if EOSResult <> EOS_SUCCESS then
    raise Exception.Create('EOS_Logging_SetCallback has failed: ' + EOS_EResult_ToString(EOSResult));

  PlatformOptions := Default(EOS_Platform_Options);

  PlatformOptions.ApiVersion := EOS_PLATFORM_OPTIONS_API_LATEST;
  PlatformOptions.bIsServer := {$IFDEF SERVER}EOS_TRUE{$ELSE}EOS_FALSE{$ENDIF};
  PlatformOptions.EncryptionKey := PChar('abcd');
  PlatformOptions.OverrideCountryCode := nil;
  PlatformOptions.OverrideLocaleCode := nil;
  PlatformOptions.Flags := EOS_PF_DISABLE_OVERLAY;
  PlatformOptions.CacheDirectory := PChar(GetTempDir());

  PlatformOptions.ProductId := EOS_PRODUCT_ID;
  PlatformOptions.SandboxId := EOS_SANDBOX_ID;
  PlatformOptions.DeploymentId := EOS_DEPLOYMENT_ID;

  {$IFNDEF SERVER}
  PlatformOptions.ClientCredentials.ClientId := EOS_CLIENT_ID;
  PlatformOptions.ClientCredentials.ClientSecret := EOS_CLIENT_SECRET;
  {$ELSE}
  PlatformOptions.ClientCredentials.ClientId := EOS_SERVER_ID;
  PlatformOptions.ClientCredentials.ClientSecret := EOS_SERVER_SECRET;
  {$ENDIF}

  // A budget, measured in milliseconds, for EOS_Platform_Tick to do its work.
  PlatformOptions.TickBudgetInMilliseconds := 10;
  PlatformOptions.Reserved := nil;

  {$IFDEF WINDOWS}
  WindowsRTC.ApiVersion := EOS_WINDOWS_RTCOPTIONS_API_LATEST;
  WindowsRTC.XAudio29DllPath := PChar(GetCurrentDir + '/xaudio2_9redist.dll');

  RtcOptions.ApiVersion := EOS_PLATFORM_RTCOPTIONS_API_LATEST;
  RtcOptions.PlatformSpecificOptions := @WindowsRTC;

  PlatformOptions.RTCOptions := @RtcOptions;
  {$ELSE}
  PlatformOptions.RTCOptions := nil;
  {$ENDIF}

  FPlatformHandle := EOS_Platform_Create(@PlatformOptions);

  if FPlatformHandle = nil then
    raise Exception.Create('EOS_Platform_Create is nil');
end;

destructor TEpicOnlineServices.Destroy();
begin
  if FPlatformHandle <> nil then
    EOS_Platform_Release(FPlatformHandle);

  FPlatformHandle := nil;

  EOS_Shutdown();
end;

procedure TEpicOnlineServices.OnClockTick();
begin
  if FPlatformHandle <> nil then
    EOS_Platform_Tick(FPlatformHandle);
end;

{$IFNDEF SERVER}
constructor TEpicOnlineServicesClient.Create(LogLevel: Integer);
var
  AddNotifyMessageToServerOptions: EOS_AntiCheatClient_AddNotifyMessageToServerOptions;
begin
  inherited Create(LogLevel);

  FAntiCheatHandle := EOS_Platform_GetAntiCheatClientInterface(FPlatformHandle);

  if FAntiCheatHandle = nil then
    raise Exception.Create('[EOS] Failed to initialize Anti-Cheat');

  FPollStatusOptions.ApiVersion := EOS_ANTICHEATCLIENT_POLLSTATUS_API_LATEST;
  FPollStatusOptions.OutMessageLength := 1024;

  AddNotifyMessageToServerOptions.ApiVersion := EOS_ANTICHEATCLIENT_ADDNOTIFYMESSAGETOSERVER_API_LATEST;
  FAddNotifyMessageToServerId := EOS_AntiCheatClient_AddNotifyMessageToServer(FAntiCheatHandle, @AddNotifyMessageToServerOptions, Self, @OnMessageToServerCallback);
end;

destructor TEpicOnlineServicesClient.Destroy;
begin
  EndSession();
  EOS_AntiCheatClient_RemoveNotifyMessageToServer(FAntiCheatHandle, FAddNotifyMessageToServerId);

  inherited;
end;

procedure TEpicOnlineServicesClient.BeginSession();
var
  SessionOptions: EOS_AntiCheatClient_BeginSessionOptions;
  EOSResult: EOS_EResult;
begin
  if FLocalUser <> nil then
  begin
    SessionOptions.ApiVersion := EOS_ANTICHEATCLIENT_BEGINSESSION_API_LATEST;
    SessionOptions.LocalUserId := FLocalUser;
    SessionOptions.Mode := EOS_ACCM_ClientServer;

    EOSResult := EOS_AntiCheatClient_BeginSession(FAntiCheatHandle, @SessionOptions);
    if EOSResult <> EOS_SUCCESS then
      Debug('[EOS] EOS_AntiCheatClient_BeginSession failed: ' + EOS_EResult_ToString(EOSResult));
  end else
  begin
    {$IFDEF STEAM}
    SteamAPI.User.RequestEncryptedAppTicket(nil, 0);
    {$ELSE}
    OpenIDLogin();
    {$ENDIF}
  end;
end;

procedure TEpicOnlineServicesClient.EndSession();
var
  Options: EOS_AntiCheatClient_EndSessionOptions;
  EOSResult: EOS_EResult;
begin
  Options.ApiVersion := EOS_ANTICHEATCLIENT_ENDSESSION_API_LATEST;

  if FAntiCheatHandle <> nil then
  begin
    EOSResult := EOS_AntiCheatClient_EndSession(FAntiCheatHandle, @Options);
    if EOSResult <> EOS_SUCCESS then
      Debug('[EOS] EOS_AntiCheatClient_EndSession failed: ' + EOS_EResult_ToString(EOSResult));
  end;
end;
{$IFDEF STEAM}
procedure TEpicOnlineServicesClient.OnSteamAuth();
var
  AppTicket: array[0..1024] of Byte;
  AppTicketSize: LongWord;
  EOSTicket: TCharArray;
  EOSTicketSize: LongWord;
  ConnectCredentials: EOS_Connect_Credentials;
  LoginOptions: EOS_Connect_LoginOptions;
begin
  Debug('[EOS] OnSteamAuth');
  EOSTicket := Default(TCharArray);

  if SteamAPI.User.GetEncryptedAppTicket(@AppTicket, 1024, @AppTicketSize) then
  begin
    EOSTicketSize := (AppTicketSize * 2) + 1;
    SetLength(EOSTicket, EOSTicketSize);
    if EOS_ByteArray_ToString(@AppTicket[0], AppTicketSize, @EOSTicket[0], @EOSTicketSize) <> EOS_Success then
    begin
      Debug('[EOS] EOS_ByteArray_ToString failed');
      Exit;
    end;

    FConnectHandle := EOS_Platform_GetConnectInterface(FPlatformHandle);

    ConnectCredentials.ApiVersion := EOS_CONNECT_CREDENTIALS_API_LATEST;
    ConnectCredentials.Token := @EOSTicket[0];
    ConnectCredentials._Type := EOS_ECT_STEAM_APP_TICKET;

    LoginOptions.ApiVersion := EOS_CONNECT_LOGIN_API_LATEST;
    LoginOptions.UserLoginInfo := nil;
    LoginOptions.Credentials := @ConnectCredentials;

    EOS_Connect_Login(FConnectHandle, @LoginOptions, Self, @OnLoginConnectCallback);
  end else
    Debug('[EOS] GetEncryptedAppTicket failed');
end;
{$ENDIF}

procedure TEpicOnlineServicesClient.OpenIDLogin();
var
  ConnectCredentials: EOS_Connect_Credentials;
  LoginOptions: EOS_Connect_LoginOptions;
begin
  Debug('[EOS] OpenIDLogin');
  FConnectHandle := EOS_Platform_GetConnectInterface(FPlatformHandle);

  ConnectCredentials.ApiVersion := EOS_CONNECT_CREDENTIALS_API_LATEST;
  ConnectCredentials.Token := PChar(HWID);
  ConnectCredentials._Type := EOS_ECT_OPENID_ACCESS_TOKEN;

  LoginOptions.ApiVersion := EOS_CONNECT_LOGIN_API_LATEST;
  LoginOptions.UserLoginInfo := nil;
  LoginOptions.Credentials := @ConnectCredentials;

  EOS_Connect_Login(FConnectHandle, @LoginOptions, Self, @OnLoginConnectCallback);
end;

procedure TEpicOnlineServicesClient.OnMessageToServer(Data: PEOS_AntiCheatClient_OnMessageToServerCallbackInfo); stdcall;
var
  EACMsg: PMsg_EACMessage;
  Size: Integer;
  SendBuffer: TCharArray;
begin
  Debug('[EOS] OnMessageToServer');
  SendBuffer := Default(TCharArray);

  Size := SizeOf(TMsg_EACMessage) + Data.MessageDataSizeBytes;
  SetLength(SendBuffer, Size);

  EACMsg := PMsg_EACMessage(SendBuffer);
  EACMsg.Header.ID := MsgID_EACMessage;

  Move(Data.MessageData^, EACMsg.Data, Data.MessageDataSizeBytes);

  UDP.SendData(EACMsg^, Size, k_nSteamNetworkingSend_Reliable);
end;

procedure TEpicOnlineServicesClient.OnCreateUser(Data: PEOS_Connect_CreateUserCallbackInfo);
begin
  Debug('[EOS] OnCreateUser');
  if Data.ResultCode = EOS_Success then
    FLocalUser := Data.LocalUserId;
end;

procedure TEpicOnlineServicesClient.OnLoginConnect(Data: PEOS_Connect_LoginCallbackInfo); stdcall;
var
  CreateUserOptions: EOS_Connect_CreateUserOptions;
begin
  Debug('[EOS] OnLoginConnect');
  if Data.ResultCode = EOS_InvalidUser then
  begin
    CreateUserOptions.ApiVersion := EOS_CONNECT_CREATEUSER_API_LATEST;
    CreateUserOptions.ContinuanceToken := Data.ContinuanceToken;
    EOS_Connect_CreateUser(FConnectHandle, @CreateUserOptions, Self, @OnCreateUserCallback);
  end
  else if Data.ResultCode = EOS_Success then
  begin
    FLocalUser := Data.LocalUserId;
    Self.BeginSession();
  end else
    Debug('[EOS] Unhandled OnLoginConnect event');
end;

procedure TEpicOnlineServicesClient.OnClockTick();
var
  PollStatusMessage: array[0..1024] of Char;
  ViolationType: EOS_EAntiCheatClientViolationType;
begin
  inherited;

  if EOS_AntiCheatClient_PollStatus(FAntiCheatHandle, @FPollStatusOptions, @ViolationType, @PollStatusMessage) = EOS_Success then
  begin
    ShowMessage('Anti-cheat violation: ' + StrPas(PollStatusMessage));
    ShutDown;
  end;
end;
{$ELSE}

constructor TEpicOnlineServicesServer.Create(LogLevel: Integer; ServerName: String; Timeout: Integer);
var
  SessionOptions: EOS_AntiCheatServer_BeginSessionOptions;
  FAddNotifyMessageToClientOptions: EOS_AntiCheatServer_AddNotifyMessageToClientOptions;
  FAddNotifyClientActionRequiredOptions: EOS_AntiCheatServer_AddNotifyClientActionRequiredOptions;
  FAddNotifyClientAuthStatusChangedOptions: EOS_AntiCheatServer_AddNotifyClientAuthStatusChangedOptions;
  EOSResult: EOS_EResult;
begin
  inherited Create(LogLevel);

  Self.FAntiCheatHandle := EOS_Platform_GetAntiCheatServerInterface(FPlatformHandle);

  if FAntiCheatHandle = nil then
    raise Exception.Create('[EOS] EOS_Platform_GetAntiCheatServerInterface is nil');

  FAddNotifyMessageToClientOptions.ApiVersion := EOS_ANTICHEATSERVER_ADDNOTIFYMESSAGETOCLIENT_API_LATEST;
  FAddNotifyMessageToClientId := EOS_AntiCheatServer_AddNotifyMessageToClient(FAntiCheatHandle, @FAddNotifyMessageToClientOptions, Self, @OnMessageToClientCallback);

  if FAddNotifyMessageToClientId = EOS_INVALID_NOTIFICATIONID then
    raise Exception.Create('[EOS] EOS_AntiCheatServer_AddNotifyMessageToClient returned invalid id');

  FAddNotifyClientActionRequiredOptions.ApiVersion := EOS_ANTICHEATSERVER_ADDNOTIFYCLIENTACTIONREQUIRED_API_LATEST;
  FAddNotifyClientActionRequiredId := EOS_AntiCheatServer_AddNotifyClientActionRequired(FAntiCheatHandle, @FAddNotifyClientActionRequiredOptions, Self, @OnClientActionRequiredCallback);

  if FAddNotifyClientActionRequiredId = EOS_INVALID_NOTIFICATIONID then
    raise Exception.Create('[EOS] EOS_AntiCheatServer_AddNotifyClientActionRequired returned invalid id');

  FAddNotifyClientAuthStatusChangedOptions.ApiVersion := EOS_ANTICHEATSERVER_ADDNOTIFYCLIENTAUTHSTATUSCHANGED_API_LATEST;
  FAddNotifyClientAuthStatusChangedId := EOS_AntiCheatServer_AddNotifyClientAuthStatusChanged(FAntiCheatHandle, @FAddNotifyClientAuthStatusChangedOptions, Self, @OnClientAuthStatusChangedCallback);

  if FAddNotifyClientAuthStatusChangedId = EOS_INVALID_NOTIFICATIONID then
    raise Exception.Create('[EOS] EOS_AntiCheatServer_AddNotifyClientAuthStatusChanged returned invalid id');

  SessionOptions.ApiVersion := EOS_ANTICHEATSERVER_BEGINSESSION_API_LATEST;
  SessionOptions.RegisterTimeoutSeconds := Timeout;
  SessionOptions.ServerName := PChar(ServerName);
  SessionOptions.bEnableGameplayData := EOS_FALSE;
  SessionOptions.LocalUserId := nil;

  EOSResult := EOS_AntiCheatServer_BeginSession(FAntiCheatHandle, @SessionOptions);
  if EOSResult <> EOS_SUCCESS then
    raise Exception.Create('[EOS] EOS_AntiCheatServer_BeginSession failed: ' + EOS_EResult_ToString(EOSResult));
end;

destructor TEpicOnlineServicesServer.Destroy;
var
  Options: EOS_AntiCheatServer_EndSessionOptions;
begin
  EOS_AntiCheatServer_RemoveNotifyMessageToClient(FAntiCheatHandle, FAddNotifyMessageToClientId);
  EOS_AntiCheatServer_RemoveNotifyClientActionRequired(FAntiCheatHandle, FAddNotifyClientActionRequiredId);
  EOS_AntiCheatServer_RemoveNotifyClientAuthStatusChanged(FAntiCheatHandle, FAddNotifyClientAuthStatusChangedId);

  Options.ApiVersion := EOS_ANTICHEATSERVER_ENDSESSION_API_LATEST;
  EOS_AntiCheatServer_EndSession(FAntiCheatHandle, @Options);

  inherited;
end;

procedure TEpicOnlineServicesServer.RegisterClient(Peer: Pointer);
var
  Options: EOS_AntiCheatServer_RegisterClientOptions;
  {$IFDEF STEAM}
  SteamID: TSteamID;
  {$ENDIF}
  EOSResult: EOS_EResult;
begin
  Options.ApiVersion := EOS_ANTICHEATSERVER_REGISTERCLIENT_API_LATEST;
  Options.ClientHandle := Peer;
  Options.ClientType := EOS_ACCCT_ProtectedClient;
  // TODO: Set a proper platform based on ... not sure what
  Options.ClientPlatform := EOS_ACCCP_Unknown;
  {$IFDEF STEAM}
  SteamID := TPlayer(Peer).SteamID;
  Options.AccountID := PChar(SteamID.GetAsString);
  {$ELSE}
  Options.AccountID := PChar(TPlayer(Peer).HWID);
  {$ENDIF}
  Options.IpAddress := PChar(TPlayer(Peer).IP);

  EOSResult := EOS_AntiCheatServer_RegisterClient(FAntiCheatHandle, @Options);
  if EOSResult <> EOS_SUCCESS then
    Debug('[EOS] Failed to register player' + TPlayer(Peer).Name);
end;

procedure TEpicOnlineServicesServer.UnregisterClient(Peer: Pointer);
var
  Options: EOS_AntiCheatServer_UnregisterClientOptions;
  EOSResult: EOS_EResult;
begin
  Options.ApiVersion := EOS_ANTICHEATSERVER_UNREGISTERCLIENT_API_LATEST;
  Options.ClientHandle := Peer;

  EOSResult := EOS_AntiCheatServer_UnregisterClient(FAntiCheatHandle, @Options);
  if EOSResult <> EOS_SUCCESS then
    Debug('[EOS] Failed to unregister player: ' + TPlayer(Peer).Name);
end;


procedure TEpicOnlineServicesServer.OnMessageToClient(Data: PEOS_AntiCheatCommon_OnMessageToClientCallbackInfo); stdcall;
var
  Player: TPlayer;
  EACMsg: PMsg_EACMessage;
  Size: Integer;
  SendBuffer: TCharArray;
begin
  SendBuffer := Default(TCharArray);

  if Data.ClientHandle = nil then
  begin
    Debug('[EOS] OnMessageToClientCallback: ClientHandle is nil');
    Exit;
  end;

  Player := TPlayer(Data.ClientHandle);

  Size := SizeOf(TMsg_EACMessage) + Data.MessageDataSizeBytes;
  SetLength(SendBuffer, Size);

  EACMsg := PMsg_EACMessage(SendBuffer);
  EACMsg.Header.ID := MsgID_EACMessage;

  Move(Data.MessageData^, EACMsg.Data, Data.MessageDataSizeBytes);

  UDP.SendData(EACMsg^, Size, Player.Peer, k_nSteamNetworkingSend_Reliable);
end;


procedure TEpicOnlineServicesServer.OnClientActionRequired(Data: PEOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo); stdcall;
var
  Player: TPlayer;
begin
  if Data.ClientAction = EOS_ACCCA_RemovePlayer then
  begin
    Player := TPlayer(Data.ClientHandle);
    MainConsole.Console(Format('[EAC] Violation detected - name: %s(%s) reason: %s', [Player.Name, Player.IP, Data.ActionReasonDetailsString]), WARNING_MESSAGE_COLOR);

    if Player.SpriteNum <> 0 then
      KickPlayer(Player.SpriteNum, False, KICK_AC, 0, 'Anti-Cheat violation: ' + Data.ActionReasonDetailsString)
    else
      ServerSendUnAccepted(Player.Peer, ANTICHEAT_REJECTED, Data.ActionReasonDetailsString);

    Self.UnregisterClient(Data.ClientHandle);
  end;
end;

procedure TEpicOnlineServicesServer.OnClientAuthStatusChanged(Data: PEOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo); stdcall;
var
  Status: String;
begin
  WriteStr(Status, Data.ClientAuthStatus);
  Debug('[EOS] OnClientAuthStatusChanged ' + TPlayer(Data.ClientHandle).Name + ' set to ' + Status);
end;
{$ENDIF}

{$IFDEF SERVER}
procedure ServerHandleEACMessage(NetMessage: PSteamNetworkingMessage_t);
var
  EACMsg: PMsg_EACMessage;
  Player: TPlayer;
  Options: EOS_AntiCheatServer_ReceiveMessageFromClientOptions;
  EOSResult: EOS_EResult;
begin
  if not Assigned(EACServer) then
    Exit;

  if not VerifyPacketLargerOrEqual(sizeof(EACMsg), NetMessage^.m_cbSize, MsgID_EACMessage) then
    Exit;

  Player := TPlayer(NetMessage^.m_nConnUserData);

  EACMsg := PMsg_EACMessage(NetMessage^.m_pData);

  Options.ApiVersion := EOS_ANTICHEATSERVER_RECEIVEMESSAGEFROMCLIENT_API_LATEST;
  Options.ClientHandle := Player;
  Options.DataLengthBytes := NetMessage^.m_cbSize - SizeOf(TMsg_EACMessage);
  Options.Data := @EACMsg^.Data;

  EOSResult := EOS_AntiCheatServer_ReceiveMessageFromClient(EACServer.AntiCheatHandle, @Options);
  if EOSResult <> EOS_SUCCESS then
    Debug('[EOS] EOS_AntiCheatServer_ReceiveMessageFromClient failed: ' + EOS_EResult_ToString(EOSResult));
end;
{$ELSE}
procedure ClientHandleEACMessage(NetMessage: PSteamNetworkingMessage_t);
var
  EACMsg: PMsg_EACMessage;
  Options: EOS_AntiCheatClient_ReceiveMessageFromServerOptions;
  EOSResult: EOS_EResult;
begin
  if not Assigned(EACClient) then
    Exit;

  if not VerifyPacketLargerOrEqual(sizeof(EACMsg), NetMessage^.m_cbSize, MsgID_EACMessage) then
    Exit;

  EACMsg := PMsg_EACMessage(NetMessage^.m_pData);

  Options.ApiVersion := EOS_ANTICHEATCLIENT_RECEIVEMESSAGEFROMSERVER_API_LATEST;
  Options.DataLengthBytes := NetMessage^.m_cbSize - SizeOf(TMsg_EACMessage);
  Options.Data := @EACMsg^.Data[0];

  EOSResult := EOS_AntiCheatClient_ReceiveMessageFromServer(EACClient.AntiCheatHandle, @Options);
  if EOSResult <> EOS_SUCCESS then
    Debug('[EOS] EOS_AntiCheatClient_ReceiveMessageFromServer failed: ' + EOS_EResult_ToString(EOSResult));
end;
{$ENDIF}

{$IFDEF SERVER}
procedure OnMessageToClientCallback(Data: PEOS_AntiCheatCommon_OnMessageToClientCallbackInfo); stdcall;
begin
  if Data.ClientData <> nil then
    TEpicOnlineServicesServer(Data.ClientData).OnMessageToClient(Data);
end;

procedure OnClientActionRequiredCallback(Data: PEOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo); stdcall;
begin
  if Data.ClientData <> nil then
    TEpicOnlineServicesServer(Data.ClientData).OnClientActionRequired(Data);
end;

procedure OnClientAuthStatusChangedCallback(Data: PEOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo); stdcall;
begin
  if Data.ClientData <> nil then
    TEpicOnlineServicesServer(Data.ClientData).OnClientAuthStatusChanged(Data);
end;
{$ELSE}
procedure OnMessageToServerCallback(Data: PEOS_AntiCheatClient_OnMessageToServerCallbackInfo); stdcall;
begin
  if Data.ClientData <> nil then
    TEpicOnlineServicesClient(Data.ClientData).OnMessageToServer(Data);
end;

procedure OnCreateUserCallback(Data: PEOS_Connect_CreateUserCallbackInfo);
begin
  if Data.ClientData <> nil then
    TEpicOnlineServicesClient(Data.ClientData).OnCreateUser(Data);
end;

procedure OnLoginConnectCallback(Data: PEOS_Connect_LoginCallbackInfo); stdcall;
begin
  if Data.ClientData <> nil then
    TEpicOnlineServicesClient(Data.ClientData).OnLoginConnect(Data);
end;
{$ENDIF}

procedure EOS_LogHandler(var Message: EOS_LogMessage); stdcall;
begin
  MainConsole.Console(Format('[EOS] %s', [Message.Message]), DEBUG_MESSAGE_COLOR);
end;

end.
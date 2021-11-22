type EOS_HAntiCheatServer = Pointer;

{
 * Callback issued when a new message must be dispatched to a connected client.
 *
 * Messages contain opaque binary data of up to 256 bytes and must be transmitted
 * to the correct client using the game's own networking layer, then delivered
 * to the client anti-cheat instance using the EOS_AntiCheatClient_ReceiveMessageFromServer function.
 *
 * This callback is always issued from within EOS_Platform_Tick on its calling thread.
 }
type EOS_AntiCheatServer_OnMessageToClientCallback = procedure(Data: PEOS_AntiCheatCommon_OnMessageToClientCallbackInfo); stdcall;

{
 * Callback issued when an action must be applied to a connected client.
 * This callback is always issued from within EOS_Platform_Tick on its calling thread.
 }
type EOS_AntiCheatServer_OnClientActionRequiredCallback = procedure(Data: PEOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo); stdcall;

{
 * Optional callback issued when a connected client's authentication status has changed.
 * This callback is always issued from within EOS_Platform_Tick on its calling thread.
 }
type EOS_AntiCheatServer_OnClientAuthStatusChangedCallback = procedure(Data: PEOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo); stdcall;

const EOS_ANTICHEATSERVER_ADDNOTIFYMESSAGETOCLIENT_API_LATEST = 1;
type EOS_AntiCheatServer_AddNotifyMessageToClientOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_ADDNOTIFYMESSAGETOCLIENT_API_LATEST. }
    ApiVersion: cint32;
end;

type PEOS_AntiCheatServer_AddNotifyMessageToClientOptions = ^EOS_AntiCheatServer_AddNotifyMessageToClientOptions;

const EOS_ANTICHEATSERVER_ADDNOTIFYCLIENTACTIONREQUIRED_API_LATEST = 1;
type EOS_AntiCheatServer_AddNotifyClientActionRequiredOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_ADDNOTIFYCLIENTACTIONREQUIRED_API_LATEST. }
    ApiVersion: cint32;
end;

type PEOS_AntiCheatServer_AddNotifyClientActionRequiredOptions = ^EOS_AntiCheatServer_AddNotifyClientActionRequiredOptions;

const EOS_ANTICHEATSERVER_ADDNOTIFYCLIENTAUTHSTATUSCHANGED_API_LATEST = 1;
type EOS_AntiCheatServer_AddNotifyClientAuthStatusChangedOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_ADDNOTIFYCLIENTAUTHSTATUSCHANGED_API_LATEST. }
    ApiVersion: cint32;
end;

type PEOS_AntiCheatServer_AddNotifyClientAuthStatusChangedOptions = ^EOS_AntiCheatServer_AddNotifyClientAuthStatusChangedOptions;

{ Limits on RegisterTimeoutSeconds parameter }
const EOS_ANTICHEATSERVER_BEGINSESSION_MIN_REGISTERTIMEOUT = 10;
const EOS_ANTICHEATSERVER_BEGINSESSION_MAX_REGISTERTIMEOUT = 120;
const EOS_ANTICHEATSERVER_BEGINSESSION_API_LATEST = 3;
type EOS_AntiCheatServer_BeginSessionOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_BEGINSESSION_API_LATEST. }
    ApiVersion: cint32;
    {
     * Time in seconds to allow newly registered clients to complete anti-cheat authentication.
     * Recommended value: 60
     }
    RegisterTimeoutSeconds: cuint32;
    { Optional name of this game server }
    ServerName: PChar;
    {
     * Gameplay data collection APIs such as LogPlayerTick will be enabled if set to true.
     * If you do not use these APIs, it is more efficient to set this value to false.
     }
    bEnableGameplayData: EOS_Bool;
    { The Product User ID of the local user who is associated with this session. Dedicated servers should set this to null. }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_AntiCheatServer_BeginSessionOptions = ^EOS_AntiCheatServer_BeginSessionOptions;

const EOS_ANTICHEATSERVER_ENDSESSION_API_LATEST = 1;
type EOS_AntiCheatServer_EndSessionOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_ENDSESSION_API_LATEST. }
    ApiVersion: cint32;
end;

type PEOS_AntiCheatServer_EndSessionOptions = ^EOS_AntiCheatServer_EndSessionOptions;

const EOS_ANTICHEATSERVER_REGISTERCLIENT_API_LATEST = 1;
type EOS_AntiCheatServer_RegisterClientOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_REGISTERCLIENT_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value describing the remote user (e.g. a player object pointer) }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { Type of remote user being registered }
    ClientType: EOS_EAntiCheatCommonClientType;
    { Remote user's platform, if known }
    ClientPlatform: EOS_EAntiCheatCommonClientPlatform;
    {
     * Identifier for the remote user. This is typically a string representation of an
     * account ID, but it can be any string which is both unique (two different users will never
     * have the same string) and consistent (if the same user connects to this game session
     * twice, the same string will be used) in the scope of a single protected game session.
     }
    AccountId: PChar;
    {
     * Optional IP address for the remote user. May be null if not available.
     * IPv4 format: "0.0.0.0"
     * IPv6 format: "0:0:0:0:0:0:0:0"
     }
    IpAddress: PChar;
end;

type PEOS_AntiCheatServer_RegisterClientOptions = ^EOS_AntiCheatServer_RegisterClientOptions;

const EOS_ANTICHEATSERVER_UNREGISTERCLIENT_API_LATEST = 1;
type EOS_AntiCheatServer_UnregisterClientOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_UNREGISTERCLIENT_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value describing the remote user, as previously passed to RegisterClient }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
end;

type PEOS_AntiCheatServer_UnregisterClientOptions = ^EOS_AntiCheatServer_UnregisterClientOptions;

const EOS_ANTICHEATSERVER_RECEIVEMESSAGEFROMCLIENT_API_LATEST = 1;
type EOS_AntiCheatServer_ReceiveMessageFromClientOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_RECEIVEMESSAGEFROMCLIENT_API_LATEST. }
    ApiVersion: cint32;
    { Optional value, if non-null then only messages addressed to this specific client will be returned }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { The size of the data received }
    DataLengthBytes: cuint32;
    { The data received }
    Data: Pointer;
end;

type PEOS_AntiCheatServer_ReceiveMessageFromClientOptions = ^EOS_AntiCheatServer_ReceiveMessageFromClientOptions;

const EOS_ANTICHEATSERVER_SETCLIENTNETWORKSTATE_API_LATEST = 1;
type EOS_AntiCheatServer_SetClientNetworkStateOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_SETCLIENTNETWORKSTATE_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value describing the remote user (e.g. a player object pointer) }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { True if the network is functioning normally, false if temporarily interrupted }
    bIsNetworkActive: EOS_Bool;
end;
type PEOS_AntiCheatServer_SetClientNetworkStateOptions = ^EOS_AntiCheatServer_SetClientNetworkStateOptions;
const EOS_ANTICHEATSERVER_GETPROTECTMESSAGEOUTPUTLENGTH_API_LATEST = 1;
type EOS_AntiCheatServer_GetProtectMessageOutputLengthOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_GETPROTECTMESSAGEOUTPUTLENGTH_API_LATEST. }
    ApiVersion: cint32;
    { Length in bytes of input }
    DataLengthBytes: cuint32;
end;
type PEOS_AntiCheatServer_GetProtectMessageOutputLengthOptions = ^EOS_AntiCheatServer_GetProtectMessageOutputLengthOptions;

const EOS_ANTICHEATSERVER_PROTECTMESSAGE_API_LATEST = 1;
type EOS_AntiCheatServer_ProtectMessageOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_PROTECTMESSAGE_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value describing the remote user to whom the message will be sent }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { Length in bytes of input }
    DataLengthBytes: cuint32;
    { The data to encrypt }
    Data: Pointer;
    { The size in bytes of OutBuffer }
    OutBufferSizeBytes: cuint32;
end;
type PEOS_AntiCheatServer_ProtectMessageOptions = ^EOS_AntiCheatServer_ProtectMessageOptions;

const EOS_ANTICHEATSERVER_UNPROTECTMESSAGE_API_LATEST = 1;
type EOS_AntiCheatServer_UnprotectMessageOptions = record
    { API Version: Set this to EOS_ANTICHEATSERVER_UNPROTECTMESSAGE_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value describing the remote user from whom the message was received }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { Length in bytes of input }
    DataLengthBytes: cuint32;
    { The data to decrypt }
    Data: Pointer;
    { The size in bytes of OutBuffer }
    OutBufferSizeBytes: cuint32;
end;
type PEOS_AntiCheatServer_UnprotectMessageOptions = ^EOS_AntiCheatServer_UnprotectMessageOptions;
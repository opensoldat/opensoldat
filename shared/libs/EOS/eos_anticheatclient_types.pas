type EOS_HAntiCheatClient = Pointer;

{ Operating modes }
EOS_EAntiCheatClientMode = (
    { Not used }
    EOS_ACCM_Invalid = 0,
    { Dedicated or listen server mode }
    EOS_ACCM_ClientServer = 1,
    { Full mesh peer-to-peer mode }
    EOS_ACCM_PeerToPeer = 2
);

{ Anti-cheat integrity violation types }
EOS_EAntiCheatClientViolationType = (
    { Not used }
    EOS_ACCVT_Invalid = 0,
    { An anti-cheat asset integrity catalog file could not be found }
    EOS_ACCVT_IntegrityCatalogNotFound = 1,
    { An anti-cheat asset integrity catalog file is corrupt or invalid }
    EOS_ACCVT_IntegrityCatalogError = 2,
    { An anti-cheat asset integrity catalog file's certificate has been revoked. }
    EOS_ACCVT_IntegrityCatalogCertificateRevoked = 3,
    {
     * The primary anti-cheat asset integrity catalog does not include an entry for the game's
     * main executable, which is required.
     }
    EOS_ACCVT_IntegrityCatalogMissingMainExecutable = 4,
    { A disallowed game file modification was detected }
    EOS_ACCVT_GameFileMismatch = 5,
    { A disallowed game file removal was detected }
    EOS_ACCVT_RequiredGameFileNotFound = 6,
    { A disallowed game file addition was detected }
    EOS_ACCVT_UnknownGameFileForbidden = 7,
    { A system file failed an integrity check }
    EOS_ACCVT_SystemFileUntrusted = 8,
    { A disallowed code module was loaded into the game process }
    EOS_ACCVT_ForbiddenModuleLoaded = 9,
    { A disallowed game process memory modification was detected }
    EOS_ACCVT_CorruptedMemory = 10,
    { A disallowed tool was detected running in the system }
    EOS_ACCVT_ForbiddenToolDetected = 11,
    { An internal anti-cheat integrity check failed }
    EOS_ACCVT_InternalAntiCheatViolation = 12,
    { Integrity checks on messages between the game client and game server failed }
    EOS_ACCVT_CorruptedNetworkMessageFlow = 13,
    { The game is running inside a disallowed virtual machine }
    EOS_ACCVT_VirtualMachineNotAllowed = 14,
    { A forbidden operating system configuration was detected }
    EOS_ACCVT_ForbiddenSystemConfiguration = 15
);
PEOS_EAntiCheatClientViolationType = ^EOS_EAntiCheatClientViolationType;

{
 * Structure containing details about a new message that must be dispatched to the game server.
 }
type EOS_AntiCheatClient_OnMessageToServerCallbackInfo = record
    { Caller-specified context data }
    ClientData: Pointer;
    { The message data that must be sent to the server }
    MessageData: Pointer;
    { The size in bytes of MessageData }
    MessageDataSizeBytes: cuint32;
end;

type PEOS_AntiCheatClient_OnMessageToServerCallbackInfo = ^EOS_AntiCheatClient_OnMessageToServerCallbackInfo;

{
 * Callback issued when a new message must be dispatched to the game server.
 *
 * Messages contain opaque binary data of up to 256 bytes and must be transmitted
 * to the game server using the game's own networking layer, then delivered
 * to the server anti-cheat instance using the EOS_AntiCheatServer_ReceiveMessageFromClient function.
 *
 * This callback is always issued from within EOS_Platform_Tick on its calling thread.
 }
type EOS_AntiCheatClient_OnMessageToServerCallback = procedure(Data: PEOS_AntiCheatClient_OnMessageToServerCallbackInfo);
{
 * Callback issued when a new message must be dispatched to a connected peer.
 *
 * Messages contain opaque binary data of up to 256 bytes and must be transmitted
 * to the correct peer using the game's own networking layer, then delivered
 * to the client anti-cheat instance using the EOS_AntiCheatClient_ReceiveMessageFromPeer function.
 *
 * This callback is always issued from within EOS_Platform_Tick on its calling thread.
 }
type EOS_AntiCheatClient_OnMessageToPeerCallback = procedure(Data: PEOS_AntiCheatCommon_OnMessageToClientCallbackInfo);
{
 * Callback issued when an action must be applied to a connected peer.
 * This callback is always issued from within EOS_Platform_Tick on its calling thread.
 }
type EOS_AntiCheatClient_OnPeerActionRequiredCallback = procedure(Data: PEOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo);
{
 * Optional callback issued when a connected peer's authentication status has changed.
 * This callback is always issued from within EOS_Platform_Tick on its calling thread.
 }
type EOS_AntiCheatClient_OnPeerAuthStatusChangedCallback = procedure(Data: PEOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo);
const EOS_ANTICHEATCLIENT_ADDNOTIFYMESSAGETOSERVER_API_LATEST = 1;
type EOS_AntiCheatClient_AddNotifyMessageToServerOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_ADDNOTIFYMESSAGETOSERVER_API_LATEST. }
    ApiVersion: cint32;
end;
type PEOS_AntiCheatClient_AddNotifyMessageToServerOptions = ^EOS_AntiCheatClient_AddNotifyMessageToServerOptions;
const EOS_ANTICHEATCLIENT_ADDNOTIFYMESSAGETOPEER_API_LATEST = 1;
type EOS_AntiCheatClient_AddNotifyMessageToPeerOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_ADDNOTIFYMESSAGETOPEER_API_LATEST. }
    ApiVersion: cint32;
end;
type PEOS_AntiCheatClient_AddNotifyMessageToPeerOptions = ^EOS_AntiCheatClient_AddNotifyMessageToPeerOptions;
const EOS_ANTICHEATCLIENT_ADDNOTIFYPEERACTIONREQUIRED_API_LATEST = 1;
type EOS_AntiCheatClient_AddNotifyPeerActionRequiredOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_ADDNOTIFYPEERACTIONREQUIRED_API_LATEST. }
    ApiVersion: cint32;
end;
type PEOS_AntiCheatClient_AddNotifyPeerActionRequiredOptions = ^EOS_AntiCheatClient_AddNotifyPeerActionRequiredOptions;
const EOS_ANTICHEATCLIENT_ADDNOTIFYPEERAUTHSTATUSCHANGED_API_LATEST = 1;
type EOS_AntiCheatClient_AddNotifyPeerAuthStatusChangedOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_ADDNOTIFYPEERAUTHSTATUSCHANGED_API_LATEST. }
    ApiVersion: cint32;
end;
type PEOS_AntiCheatClient_AddNotifyPeerAuthStatusChangedOptions = ^EOS_AntiCheatClient_AddNotifyPeerAuthStatusChangedOptions;
const EOS_ANTICHEATCLIENT_BEGINSESSION_API_LATEST = 3;
type EOS_AntiCheatClient_BeginSessionOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_BEGINSESSION_API_LATEST. }
    ApiVersion: cint32;
    { Logged in user identifier from earlier call to EOS_Connect_Login family of functions }
    LocalUserId: EOS_ProductUserId;
    { Operating mode }
    Mode: EOS_EAntiCheatClientMode;
end;
type PEOS_AntiCheatClient_BeginSessionOptions = ^EOS_AntiCheatClient_BeginSessionOptions;
const EOS_ANTICHEATCLIENT_ENDSESSION_API_LATEST = 1;
type EOS_AntiCheatClient_EndSessionOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_ENDSESSION_API_LATEST. }
    ApiVersion: cint32;
end;
type PEOS_AntiCheatClient_EndSessionOptions = ^EOS_AntiCheatClient_EndSessionOptions;
const EOS_ANTICHEATCLIENT_POLLSTATUS_API_LATEST = 1;
type EOS_AntiCheatClient_PollStatusOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_POLLSTATUS_API_LATEST. }
    ApiVersion: cint32;
    { The size of OutMessage in bytes. Recommended size is 256 bytes. }
    OutMessageLength: cuint32;
end;
type PEOS_AntiCheatClient_PollStatusOptions = ^EOS_AntiCheatClient_PollStatusOptions;
const EOS_ANTICHEATCLIENT_ADDEXTERNALINTEGRITYCATALOG_API_LATEST = 1;
type EOS_AntiCheatClient_AddExternalIntegrityCatalogOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_ADDEXTERNALINTEGRITYCATALOG_API_LATEST. }
    ApiVersion: cint32;
    { UTF-8 path to the .bin catalog file to add }
    PathToBinFile: PChar;
end;
type PEOS_AntiCheatClient_AddExternalIntegrityCatalogOptions = ^EOS_AntiCheatClient_AddExternalIntegrityCatalogOptions;
const EOS_ANTICHEATCLIENT_RECEIVEMESSAGEFROMSERVER_API_LATEST = 1;
type EOS_AntiCheatClient_ReceiveMessageFromServerOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_RECEIVEMESSAGEFROMSERVER_API_LATEST. }
    ApiVersion: cint32;
    { The size of the data received }
    DataLengthBytes: cuint32;
    { The data received }
    Data: Pointer;
end;
type PEOS_AntiCheatClient_ReceiveMessageFromServerOptions = ^EOS_AntiCheatClient_ReceiveMessageFromServerOptions;
const EOS_ANTICHEATCLIENT_GETPROTECTMESSAGEOUTPUTLENGTH_API_LATEST = 1;
type EOS_AntiCheatClient_GetProtectMessageOutputLengthOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_GETPROTECTMESSAGEOUTPUTLENGTH_API_LATEST. }
    ApiVersion: cint32;
    { Length in bytes of input }
    DataLengthBytes: cuint32;
end;
type PEOS_AntiCheatClient_GetProtectMessageOutputLengthOptions = ^EOS_AntiCheatClient_GetProtectMessageOutputLengthOptions;
const EOS_ANTICHEATCLIENT_PROTECTMESSAGE_API_LATEST = 1;
type EOS_AntiCheatClient_ProtectMessageOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_PROTECTMESSAGE_API_LATEST. }
    ApiVersion: cint32;
    { Length in bytes of input }
    DataLengthBytes: cuint32;
    { The data to encrypt }
    Data: Pointer;
    { The size in bytes of OutBuffer }
    OutBufferSizeBytes: cuint32;
end;
type PEOS_AntiCheatClient_ProtectMessageOptions = ^EOS_AntiCheatClient_ProtectMessageOptions;
const EOS_ANTICHEATCLIENT_UNPROTECTMESSAGE_API_LATEST = 1;
type EOS_AntiCheatClient_UnprotectMessageOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_UNPROTECTMESSAGE_API_LATEST. }
    ApiVersion: cint32;
    { Length in bytes of input }
    DataLengthBytes: cuint32;
    { The data to decrypt }
    Data: Pointer;
    { The size in bytes of OutBuffer }
    OutBufferSizeBytes: cuint32;
end;
type PEOS_AntiCheatClient_UnprotectMessageOptions = ^EOS_AntiCheatClient_UnprotectMessageOptions;
{
 * A special peer handle that represents the client itself.
 * It does not need to be registered or unregistered and is
 * used in OnPeerActionRequiredCallback to quickly signal to the user
 * that they will not be able to join online play.
 }
const EOS_ANTICHEATCLIENT_PEER_SELF = (-1);

const EOS_ANTICHEATCLIENT_REGISTERPEER_API_LATEST = 1;
type EOS_AntiCheatClient_RegisterPeerOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_REGISTERPEER_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value describing the remote user (e.g. a player object pointer) }
    PeerHandle: EOS_AntiCheatCommon_ClientHandle;
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
type PEOS_AntiCheatClient_RegisterPeerOptions = ^EOS_AntiCheatClient_RegisterPeerOptions;
const EOS_ANTICHEATCLIENT_UNREGISTERPEER_API_LATEST = 1;
type EOS_AntiCheatClient_UnregisterPeerOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_UNREGISTERPEER_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value describing the remote user, as previously passed to EOS_AntiCheatClient_RegisterPeer }
    PeerHandle: EOS_AntiCheatCommon_ClientHandle;
end;
type PEOS_AntiCheatClient_UnregisterPeerOptions = ^EOS_AntiCheatClient_UnregisterPeerOptions;

const EOS_ANTICHEATCLIENT_RECEIVEMESSAGEFROMPEER_API_LATEST = 1;
type EOS_AntiCheatClient_ReceiveMessageFromPeerOptions = record
    { API Version: Set this to EOS_ANTICHEATCLIENT_RECEIVEMESSAGEFROMPEER_API_LATEST. }
    ApiVersion: cint32;
    { The handle describing the sender of this message }
    PeerHandle: EOS_AntiCheatCommon_ClientHandle;
    { The size of the data received }
    DataLengthBytes: cuint32;
    { The data received }
    Data: Pointer;
end;
type PEOS_AntiCheatClient_ReceiveMessageFromPeerOptions = ^EOS_AntiCheatClient_ReceiveMessageFromPeerOptions;
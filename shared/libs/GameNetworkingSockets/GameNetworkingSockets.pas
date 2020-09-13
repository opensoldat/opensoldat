{$IFDEF STEAM}
{$IFDEF UNIX}
{$PACKRECORDS 4}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}
unit GameNetworkingSockets;

interface
uses
  cmem, sysutils, ctypes {$IFDEF STEAM}, steamtypes{$ENDIF};

const
  {$IFDEF WINDOWS}
  GNSLIB = 'libGameNetworkingSockets.dll';
  {$ENDIF}
  {$IFDEF DARWIN}
  GNSLIB = 'libGameNetworkingSockets.dylib';
  {$linklib GameNetworkingSockets}
  {$ENDIF}
  {$IFDEF LINUX}
  GNSLIB = 'libGameNetworkingSockets.so';
  {$ENDIF}


Type
  Int8 = shortint;
  pInt8 = ^Int8;
  int16 = SmallInt;
  pInt16 = ^int16;
  int32 = integer;
  pInt32 = ^int32;
  uint8 = Byte;
  pUInt8 = ^uint8;
  uint16 = Word;
  pUInt16 = ^uint16;
  uint32 = Cardinal;
  pUInt32 = ^uint32;
  puint64 = ^uint64;


//enum { k_iSteamNetworkingSocketsCallbacks = 1220 };
//enum { k_iSteamNetworkingMessagesCallbacks = 1250 };
//enum { k_iSteamNetworkingUtilsCallbacks = 1280 };

type ISteamNetworkingSockets = Pointer;
type ISteamNetworkingUtils = Pointer;

type SteamDatagramRelayAuthTicket = record;
type PSteamDatagramRelayAuthTicket = ^SteamDatagramRelayAuthTicket;
type SteamDatagramHostedAddress = record;
type PSteamDatagramHostedAddress = ^SteamDatagramHostedAddress;
type SteamDatagramGameCoordinatorServerLogin = record;
type PSteamDatagramGameCoordinatorServerLogin = ^SteamDatagramGameCoordinatorServerLogin;
type SteamRelayNetworkStatus_t = record;
type PSteamRelayNetworkStatus_t = ^SteamRelayNetworkStatus_t;
type SteamNetworkingMessagesSessionRequest_t = record;
type PSteamNetworkingMessagesSessionRequest_t = ^SteamNetworkingMessagesSessionRequest_t;
type SteamNetworkingMessagesSessionFailed_t = record;
type PSteamNetworkingMessagesSessionFailed_t = ^SteamNetworkingMessagesSessionFailed_t;

/// Handle used to identify a connection to a remote host.
type HSteamNetConnection = uint32;

type PHSteamNetConnection = ^HSteamNetConnection;

{$IFNDEF STEAM}
EResult = (
  k_EResultOK = 1,
  k_EResultFail = 2,
  k_EResultNoConnection = 3,
  k_EResultInvalidPassword = 5,
  k_EResultLoggedInElsewhere = 6,
  k_EResultInvalidProtocolVer = 7,
  k_EResultInvalidParam = 8,
  k_EResultFileNotFound = 9,
  k_EResultBusy = 10,
  k_EResultInvalidState = 11,
  k_EResultInvalidName = 12,
  k_EResultInvalidEmail = 13,
  k_EResultDuplicateName = 14,
  k_EResultAccessDenied = 15,
  k_EResultTimeout = 16,
  k_EResultBanned = 17,
  k_EResultAccountNotFound = 18,
  k_EResultInvalidSteamID = 19,
  k_EResultServiceUnavailable = 20,
  k_EResultNotLoggedOn = 21,
  k_EResultPending = 22,
  k_EResultEncryptionFailure = 23,
  k_EResultInsufficientPrivilege = 24,
  k_EResultLimitExceeded = 25,
  k_EResultRevoked = 26,
  k_EResultExpired = 27,
  k_EResultAlreadyRedeemed = 28,
  k_EResultDuplicateRequest = 29,
  k_EResultAlreadyOwned = 30,
  k_EResultIPNotFound = 31,
  k_EResultPersistFailed = 32,
  k_EResultLockingFailed = 33,
  k_EResultLogonSessionReplaced = 34,
  k_EResultConnectFailed = 35,
  k_EResultHandshakeFailed = 36,
  k_EResultIOFailure = 37,
  k_EResultRemoteDisconnect = 38,
  k_EResultShoppingCartNotFound = 39,
  k_EResultBlocked = 40,
  k_EResultIgnored = 41,
  k_EResultNoMatch = 42,
  k_EResultAccountDisabled = 43,
  k_EResultServiceReadOnly = 44,
  k_EResultAccountNotFeatured = 45,
  k_EResultAdministratorOK = 46,
  k_EResultContentVersion = 47,
  k_EResultTryAnotherCM = 48,
  k_EResultPasswordRequiredToKickSession = 49,
  k_EResultAlreadyLoggedInElsewhere = 50,
  k_EResultSuspended = 51,
  k_EResultCancelled = 52,
  k_EResultDataCorruption = 53,
  k_EResultDiskFull = 54,
  k_EResultRemoteCallFailed = 55,
  k_EResultPasswordUnset = 56,
  k_EResultExternalAccountUnlinked = 57,
  k_EResultPSNTicketInvalid = 58,
  k_EResultExternalAccountAlreadyLinked = 59,
  k_EResultRemoteFileConflict = 60,
  k_EResultIllegalPassword = 61,
  k_EResultSameAsPreviousValue = 62,
  k_EResultAccountLogonDenied = 63,
  k_EResultCannotUseOldPassword = 64,
  k_EResultInvalidLoginAuthCode = 65,
  k_EResultAccountLogonDeniedNoMail = 66,
  k_EResultHardwareNotCapableOfIPT = 67,
  k_EResultIPTInitError = 68,
  k_EResultParentalControlRestricted = 69,
  k_EResultFacebookQueryError = 70,
  k_EResultExpiredLoginAuthCode = 71,
  k_EResultIPLoginRestrictionFailed = 72,
  k_EResultAccountLockedDown = 73,
  k_EResultAccountLogonDeniedVerifiedEmailRequired = 74,
  k_EResultNoMatchingURL = 75,
  k_EResultBadResponse = 76,
  k_EResultRequirePasswordReEntry = 77,
  k_EResultValueOutOfRange = 78,
  k_EResultUnexpectedError = 79,
  k_EResultDisabled = 80,
  k_EResultInvalidCEGSubmission = 81,
  k_EResultRestrictedDevice = 82,
  k_EResultRegionLocked = 83,
  k_EResultRateLimitExceeded = 84,
  k_EResultAccountLoginDeniedNeedTwoFactor = 85,
  k_EResultItemDeleted = 86,
  k_EResultAccountLoginDeniedThrottle = 87,
  k_EResultTwoFactorCodeMismatch = 88,
  k_EResultTwoFactorActivationCodeMismatch = 89,
  k_EResultAccountAssociatedToMultiplePartners = 90,
  k_EResultNotModified = 91,
  k_EResultNoMobileDevice = 92,
  k_EResultTimeNotSynced = 93,
  k_EResultSmsCodeFailed = 94,
  k_EResultAccountLimitExceeded = 95,
  k_EResultAccountActivityLimitExceeded = 96,
  k_EResultPhoneActivityLimitExceeded = 97,
  k_EResultRefundToWallet = 98,
  k_EResultEmailSendFailure = 99,
  k_EResultNotSettled = 100,
  k_EResultNeedCaptcha = 101,
  k_EResultGSLTDenied = 102,
  k_EResultGSOwnerDenied = 103,
  k_EResultInvalidItemType = 104,
  k_EResultIPBanned = 105,
  k_EResultGSLTExpired = 106,
  k_EResultInsufficientFunds = 107,
  k_EResultTooManyPending = 108,
  k_EResultNoSiteLicensesFound = 109,
  k_EResultWGNetworkSendExceeded = 110,
  k_EResultAccountNotFriends = 111,
  k_EResultLimitedUserAccount = 112,
  k_EResultCantRemoveItem = 113
);
{$ENDIF}

const k_HSteamNetConnection_Invalid: HSteamNetConnection = 0;

/// Handle used to identify a "listen socket".  Unlike traditional
/// Berkeley sockets, a listen socket and a connection are two
/// different abstractions.
type HSteamListenSocket = uint32;

const k_HSteamListenSocket_Invalid: HSteamListenSocket = 0;

/// Handle used to identify a poll group, used to query many
/// connections at once efficiently.
type HSteamNetPollGroup = uint32;
const k_HSteamNetPollGroup_Invalid: HSteamNetPollGroup = 0;

/// Max length of diagnostic error message
const k_cchMaxSteamNetworkingErrMsg: Integer = 1024;

/// Used to return English-language diagnostic error messages to caller.
/// (For debugging or spewing to a console, etc.  Not intended for UI.)
type SteamNetworkingErrMsg = array [0..1024 - 1] of Char;
type PSteamNetworkingErrMsg = ^SteamNetworkingErrMsg;

/// Identifier used for a network location point of presence.  (E.g. a Valve data center.)
/// Typically you won't need to directly manipulate these.
type SteamNetworkingPOPID = uint32;
type PSteamNetworkingPOPID = ^SteamNetworkingPOPID;

/// A local timestamp.  You can subtract two timestamps to get the number of elapsed
/// microseconds.  This is guaranteed to increase over time during the lifetime
/// of a process, but not globally across runs.  You don't need to worry about
/// the value wrapping around.  Note that the underlying clock might not actually have
/// microsecond resolution.
type SteamNetworkingMicroseconds = int64;

/// Describe the status of a particular network resource
{$push}
{$notes off}
type ESteamNetworkingAvailability = (
  // Negative values indicate a problem.
  //
  // In general, we will not automatically retry unless you take some action that
  // depends on of requests this resource, such as querying the status, attempting
  // to initiate a connection, receive a connection, etc.  If you do not take any
  // action at all, we do not automatically retry in the background.
  k_ESteamNetworkingAvailability_CannotTry = -102,    // A dependent resource is missing, so this service is unavailable.  (E.g. we cannot talk to routers because Internet is down or we don't have the network config.)
  k_ESteamNetworkingAvailability_Failed = -101,      // We have tried for enough time that we would expect to have been successful by now.  We have never been successful
  k_ESteamNetworkingAvailability_Previously = -100,    // We tried and were successful at one time, but now it looks like we have a problem

  k_ESteamNetworkingAvailability_Retrying = -10,    // We previously failed and are currently retrying

  // Not a problem, but not ready either
  k_ESteamNetworkingAvailability_NeverTried = 1,    // We don't know because we haven't ever checked/tried
  k_ESteamNetworkingAvailability_Waiting = 2,      // We're waiting on a dependent resource to be acquired.  (E.g. we cannot obtain a cert until we are logged into Steam.  We cannot measure latency to relays until we have the network config.)
  k_ESteamNetworkingAvailability_Attempting = 3,    // We're actively trying now, but are not yet successful.

  k_ESteamNetworkingAvailability_Current = 100,      // Resource is online/available


  k_ESteamNetworkingAvailability_Unknown = 0,      // Internal dummy/sentinel, or value is not applicable in this context
  k_ESteamNetworkingAvailability__Force32bit = $7fffffff
);

/// Different methods of describing the identity of a network host
type ESteamNetworkingIdentityType = (
  // Dummy/empty/invalid.
  // Plese note that if we parse a string that we don't recognize
  // but that appears reasonable, we will NOT use this type.  Instead
  // we'll use k_ESteamNetworkingIdentityType_UnknownType.
  k_ESteamNetworkingIdentityType_Invalid = 0,

  //
  // Basic platform-specific identifiers.
  //
  k_ESteamNetworkingIdentityType_SteamID = 16, // 64-bit CSteamID

  //
  // Special identifiers.
  //

  // Use their IP address (and port) as their "identity".
  // These types of identities are always unauthenticated.
  // They are useful for porting plain sockets code, and other
  // situations where you don't care about authentication.  In this
  // case, the local identity will be "localhost",
  // and the remote address will be their network address.
  //
  // We use the same type for either IPv4 or IPv6, and
  // the address is always store as IPv6.  We use IPv4
  // mapped addresses to handle IPv4.
  k_ESteamNetworkingIdentityType_IPAddress = 1,

  // Generic string/binary blobs.  It's up to your app to interpret this.
  // This library can tell you if the remote host presented a certificate
  // signed by somebody you have chosen to trust, with this identity on it.
  // It's up to you to ultimately decide what this identity means.
  k_ESteamNetworkingIdentityType_GenericString = 2,
  k_ESteamNetworkingIdentityType_GenericBytes = 3,

  // This identity type is used when we parse a string that looks like is a
  // valid identity, just of a kind that we don't recognize.  In this case, we
  // can often still communicate with the peer!  Allowing such identities
  // for types we do not recognize useful is very useful for forward
  // compatibility.
  k_ESteamNetworkingIdentityType_UnknownType = 4,

  // Make sure this enum is stored in an int.
  k_ESteamNetworkingIdentityType__Force32bit = $7fffffff
);
{$pop}

type
{$PACKRECORDS 1}
  SteamNetworkingIPAddr = record
{$PACKRECORDS 1}
  m_ip: record
    case integer of
      0: (m_ipv4: record
            m_8zeros: uint64;
            m_0000: uint16;
            m_ffff: uint16;
            m_ip: array [0..3] of uint8;
          end);
      1: (m_ipv6: array [0..15] of uint8);
    end;
    m_port: uint16;
  end;


type
  PSteamNetworkingIPAddr = ^SteamNetworkingIPAddr;
{$PACKRECORDS 1}
type
 SteamNetworkingIdentity = record
  /// Type of identity.
  m_eType: ESteamNetworkingIdentityType;
  m_cbSize: Integer;

  //
  // Internal representation.  Don't access this directly, use the accessors!
  //
  // Number of bytes that are relevant below.  This MUST ALWAYS be
  // set.  (Use the accessors!)  This is important to enable old code to work
  // with new identity types.
  case Integer of
  0: (m_steamID64: uint64);
  1: (m_szGenericString: array [0..31] of Char);
  2: (m_genericBytes: array [0..31] of uint8);
  3: (m_szUnknownRawString: array[0..127] of Char);
  4: (m_ip: SteamNetworkingIPAddr);
  5: (m_reserved: array[0..31] of uint32);
end;

type
  PSteamNetworkingIdentity = ^SteamNetworkingIdentity;
{$IFDEF STEAM}
{$IFDEF UNIX}
{$PACKRECORDS 4}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}
{$ELSE}
{$PACKRECORDS 4}
{$ENDIF}
{$push}
{$notes off}
type ESteamNetworkingConnectionState = (

  /// Dummy value used to indicate an error condition in the API.
  /// Specified connection doesn't exist or has already been closed.
  k_ESteamNetworkingConnectionState_None = 0,

  /// We are trying to establish whether peers can talk to each other,
  /// whether they WANT to talk to each other, perform basic auth,
  /// and exchange crypt keys.
  ///
  /// - For connections on the "client" side (initiated locally):
  ///   We're in the process of trying to establish a connection.
  ///   Depending on the connection type, we might not know who they are.
  ///   Note that it is not possible to tell if we are waiting on the
  ///   network to complete handshake packets, or for the application layer
  ///   to accept the connection.
  ///
  /// - For connections on the "server" side (accepted through listen socket):
  ///   We have completed some basic handshake and the client has presented
  ///   some proof of identity.  The connection is ready to be accepted
  ///   using AcceptConnection().
  ///
  /// In either case, any unreliable packets sent now are almost certain
  /// to be dropped.  Attempts to receive packets are guaranteed to fail.
  /// You may send messages if the send mode allows for them to be queued.
  /// but if you close the connection before the connection is actually
  /// established, any queued messages will be discarded immediately.
  /// (We will not attempt to flush the queue and confirm delivery to the
  /// remote host, which ordinarily happens when a connection is closed.)
  k_ESteamNetworkingConnectionState_Connecting = 1,

  /// Some connection types use a back channel or trusted 3rd party
  /// for earliest communication.  If the server accepts the connection,
  /// then these connections switch into the rendezvous state.  During this
  /// state, we still have not yet established an end-to-end route (through
  /// the relay network), and so if you send any messages unreliable, they
  /// are going to be discarded.
  k_ESteamNetworkingConnectionState_FindingRoute = 2,

  /// We've received communications from our peer (and we know
  /// who they are) and are all good.  If you close the connection now,
  /// we will make our best effort to flush out any reliable sent data that
  /// has not been acknowledged by the peer.  (But note that this happens
  /// from within the application process, so unlike a TCP connection, you are
  /// not totally handing it off to the operating system to deal with it.)
  k_ESteamNetworkingConnectionState_Connected = 3,

  /// Connection has been closed by our peer, but not closed locally.
  /// The connection still exists from an API perspective.  You must close the
  /// handle to free up resources.  If there are any messages in the inbound queue,
  /// you may retrieve them.  Otherwise, nothing may be done with the connection
  /// except to close it.
  ///
  /// This stats is similar to CLOSE_WAIT in the TCP state machine.
  k_ESteamNetworkingConnectionState_ClosedByPeer = 4,

  /// A disruption in the connection has been detected locally.  (E.g. timeout,
  /// local internet connection disrupted, etc.)
  ///
  /// The connection still exists from an API perspective.  You must close the
  /// handle to free up resources.
  ///
  /// Attempts to send further messages will fail.  Any remaining received messages
  /// in the queue are available.
  k_ESteamNetworkingConnectionState_ProblemDetectedLocally = 5,

//
// The following values are used internally and will not be returned by any API.
// We document them here to provide a little insight into the state machine that is used
// under the hood.
//

  /// We've disconnected on our side, and from an API perspective the connection is closed.
  /// No more data may be sent or received.  All reliable data has been flushed, or else
  /// we've given up and discarded it.  We do not yet know for sure that the peer knows
  /// the connection has been closed, however, so we're just hanging around so that if we do
  /// get a packet from them, we can send them the appropriate packets so that they can
  /// know why the connection was closed (and not have to rely on a timeout, which makes
  /// it appear as if something is wrong).
  k_ESteamNetworkingConnectionState_FinWait = -1,

  /// We've disconnected on our side, and from an API perspective the connection is closed.
  /// No more data may be sent or received.  From a network perspective, however, on the wire,
  /// we have not yet given any indication to the peer that the connection is closed.
  /// We are in the process of flushing out the last bit of reliable data.  Once that is done,
  /// we will inform the peer that the connection has been closed, and transition to the
  /// FinWait state.
  ///
  /// Note that no indication is given to the remote host that we have closed the connection,
  /// until the data has been flushed.  If the remote host attempts to send us data, we will
  /// do whatever is necessary to keep the connection alive until it can be closed properly.
  /// But in fact the data will be discarded, since there is no way for the application to
  /// read it back.  Typically this is not a problem, as application protocols that utilize
  /// the lingering functionality are designed for the remote host to wait for the response
  /// before sending any more data.
  k_ESteamNetworkingConnectionState_Linger = -2,

  /// Connection is completely inactive and ready to be destroyed
  k_ESteamNetworkingConnectionState_Dead = -3,

  k_ESteamNetworkingConnectionState__Force32Bit = $7fffffff
);

/// Enumerate various causes of connection termination.  These are designed to work similar
/// to HTTP error codes: the numeric range gives you a rough classification as to the source
/// of the problem.
type ESteamNetConnectionEnd =
(
  // Invalid/sentinel value
  k_ESteamNetConnectionEnd_Invalid = 0,

  //
  // Application codes.  These are the values you will pass to
  // ISteamNetworkingSockets::CloseConnection.  You can use these codes if
  // you want to plumb through application-specific reason codes.  If you don't
  // need this facility, feel free to always pass
  // k_ESteamNetConnectionEnd_App_Generic.
  //
  // The distinction between "normal" and "exceptional" termination is
  // one you may use if you find useful, but it's not necessary for you
  // to do so.  The only place where we distinguish between normal and
  // exceptional is in connection analytics.  If a significant
  // proportion of connections terminates in an exceptional manner,
  // this can trigger an alert.
  //

  // 1xxx: Application ended the connection in a "usual" manner.
  //       E.g.: user intentionally disconnected from the server,
  //             gameplay ended normally, etc
  k_ESteamNetConnectionEnd_App_Min = 1000,
    k_ESteamNetConnectionEnd_App_Generic = k_ESteamNetConnectionEnd_App_Min,
    // Use codes in this range for "normal" disconnection
  k_ESteamNetConnectionEnd_App_Max = 1999,

  // 2xxx: Application ended the connection in some sort of exceptional
  //       or unusual manner that might indicate a bug or configuration
  //       issue.
  //
  k_ESteamNetConnectionEnd_AppException_Min = 2000,
    k_ESteamNetConnectionEnd_AppException_Generic = k_ESteamNetConnectionEnd_AppException_Min,
    // Use codes in this range for "unusual" disconnection
  k_ESteamNetConnectionEnd_AppException_Max = 2999,

  //
  // System codes.  These will be returned by the system when
  // the connection state is k_ESteamNetworkingConnectionState_ClosedByPeer
  // or k_ESteamNetworkingConnectionState_ProblemDetectedLocally.  It is
  // illegal to pass a code in this range to ISteamNetworkingSockets::CloseConnection
  //

  // 3xxx: Connection failed or ended because of problem with the
  //       local host or their connection to the Internet.
  k_ESteamNetConnectionEnd_Local_Min = 3000,

    // You cannot do what you want to do because you're running in offline mode.
    k_ESteamNetConnectionEnd_Local_OfflineMode = 3001,

    // We're having trouble contacting many (perhaps all) relays.
    // Since it's unlikely that they all went offline at once, the best
    // explanation is that we have a problem on our end.  Note that we don't
    // bother distinguishing between "many" and "all", because in practice,
    // it takes time to detect a connection problem, and by the time
    // the connection has timed out, we might not have been able to
    // actively probe all of the relay clusters, even if we were able to
    // contact them at one time.  So this code just means that:
    //
    // * We don't have any recent successful communication with any relay.
    // * We have evidence of recent failures to communicate with multiple relays.
    k_ESteamNetConnectionEnd_Local_ManyRelayConnectivity = 3002,

    // A hosted server is having trouble talking to the relay
    // that the client was using, so the problem is most likely
    // on our end
    k_ESteamNetConnectionEnd_Local_HostedServerPrimaryRelay = 3003,

    // We're not able to get the network config.  This is
    // *almost* always a local issue, since the network config
    // comes from the CDN, which is pretty darn reliable.
    k_ESteamNetConnectionEnd_Local_NetworkConfig = 3004,

    // Steam rejected our request because we don't have rights
    // to do this.
    k_ESteamNetConnectionEnd_Local_Rights = 3005,

    // ICE P2P rendezvous failed because we were not able to
    // determine our "public" address (e.g. reflexive address via STUN)
    //
    // If relay fallback is available (it always is on Steam), then
    // this is only used internally and will not be returned as a high
    // level failure.
    k_ESteamNetConnectionEnd_Local_P2P_ICE_NoPublicAddresses = 3006,


  k_ESteamNetConnectionEnd_Local_Max = 3999,

  // 4xxx: Connection failed or ended, and it appears that the
  //       cause does NOT have to do with the local host or their
  //       connection to the Internet.  It could be caused by the
  //       remote host, or it could be somewhere in between.
  k_ESteamNetConnectionEnd_Remote_Min = 4000,

    // The connection was lost, and as far as we can tell our connection
    // to relevant services (relays) has not been disrupted.  This doesn't
    // mean that the problem is "their fault", it just means that it doesn't
    // appear that we are having network issues on our end.
    k_ESteamNetConnectionEnd_Remote_Timeout = 4001,

    // Something was invalid with the cert or crypt handshake
    // info you gave me, I don't understand or like your key types,
    // etc.
    k_ESteamNetConnectionEnd_Remote_BadCrypt = 4002,

    // You presented me with a cert that was I was able to parse
    // and *technically* we could use encrypted communication.
    // But there was a problem that prevents me from checking your identity
    // or ensuring that somebody int he middle can't observe our communication.
    // E.g.: - the CA key was missing (and I don't accept unsigned certs)
    // - The CA key isn't one that I trust,
    // - The cert doesn't was appropriately restricted by app, user, time, data center, etc.
    // - The cert wasn't issued to you.
    // - etc
    k_ESteamNetConnectionEnd_Remote_BadCert = 4003,

    // We couldn't rendezvous with the remote host because
    // they aren't logged into Steam
    k_ESteamNetConnectionEnd_Remote_NotLoggedIn = 4004,

    // We couldn't rendezvous with the remote host because
    // they aren't running the right application.
    k_ESteamNetConnectionEnd_Remote_NotRunningApp = 4005,

    // Something wrong with the protocol version you are using.
    // (Probably the code you are running is too old.)
    k_ESteamNetConnectionEnd_Remote_BadProtocolVersion = 4006,

    // NAT punch failed failed because we never received any public
    // addresses from the remote host.  (But we did receive some
    // signals form them.)
    //
    // If relay fallback is available (it always is on Steam), then
    // this is only used internally and will not be returned as a high
    // level failure.
    k_ESteamNetConnectionEnd_Remote_P2P_ICE_NoPublicAddresses = 4007,

  k_ESteamNetConnectionEnd_Remote_Max = 4999,

  // 5xxx: Connection failed for some other reason.
  k_ESteamNetConnectionEnd_Misc_Min = 5000,

    // A failure that isn't necessarily the result of a software bug,
    // but that should happen rarely enough that it isn't worth specifically
    // writing UI or making a localized message for.
    // The debug string should contain further details.
    k_ESteamNetConnectionEnd_Misc_Generic = 5001,

    // Generic failure that is most likely a software bug.
    k_ESteamNetConnectionEnd_Misc_InternalError = 5002,

    // The connection to the remote host timed out, but we
    // don't know if the problem is on our end, in the middle,
    // or on their end.
    k_ESteamNetConnectionEnd_Misc_Timeout = 5003,

    // We're having trouble talking to the relevant relay.
    // We don't have enough information to say whether the
    // problem is on our end or not.
    k_ESteamNetConnectionEnd_Misc_RelayConnectivity = 5004,

    // There's some trouble talking to Steam.
    k_ESteamNetConnectionEnd_Misc_SteamConnectivity = 5005,

    // A server in a dedicated hosting situation has no relay sessions
    // active with which to talk back to a client.  (It's the client's
    // job to open and maintain those sessions.)
    k_ESteamNetConnectionEnd_Misc_NoRelaySessionsToClient = 5006,

    // While trying to initiate a connection, we never received
    // *any* communication from the peer.
    //k_ESteamNetConnectionEnd_Misc_ServerNeverReplied = 5007,

    // P2P rendezvous failed in a way that we don't have more specific
    // information
    k_ESteamNetConnectionEnd_Misc_P2P_Rendezvous = 5008,

    // NAT punch failed, probably due to NAT/firewall configuration.
    //
    // If relay fallback is available (it always is on Steam), then
    // this is only used internally and will not be returned as a high
    // level failure.
    k_ESteamNetConnectionEnd_Misc_P2P_NAT_Firewall = 5009,

    // Our peer replied that it has no record of the connection.
    // This should not happen ordinarily, but can happen in a few
    // exception cases:
    //
    // - This is an old connection, and the peer has already cleaned
    //   up and forgotten about it.  (Perhaps it timed out and they
    //   closed it and were not able to communicate this to us.)
    // - A bug or internal protocol error has caused us to try to
    //   talk to the peer about the connection before we received
    //   confirmation that the peer has accepted the connection.
    // - The peer thinks that we have closed the connection for some
    //   reason (perhaps a bug), and believes that is it is
    //   acknowledging our closure.
    k_ESteamNetConnectionEnd_Misc_PeerSentNoConnection = 5010,

  k_ESteamNetConnectionEnd_Misc_Max = 5999,

  k_ESteamNetConnectionEnd__Force32Bit = $7fffffff
);

type ESteamNetTransportKind =
(
  k_ESteamNetTransport_Unknown = 0,
  k_ESteamNetTransport_LoopbackBuffers = 1, // Internal buffers, not using OS network stack
  k_ESteamNetTransport_LocalHost = 2, // Using OS network stack to talk to localhost address
  k_ESteamNetTransport_UDP = 3, // Ordinary UDP connection, remote address is not a LAN address
  k_ESteamNetTransport_UDPLan = 4, // Ordinary UDP connection, remote address is a LAN address
  k_ESteamNetTransport_TURN = 5, // Relayed over TURN server
  k_ESteamNetTransport_SDRP2P = 6, // P2P connection relayed over Steam Datagram Relay
  k_ESteamNetTransport_SDRHostedServer = 7, // Connection to a server hosted in a known data center via Steam Datagram Relay

  k_ESteamNetTransport_Force32Bit = $7fffffff
);

{$pop}

/// Max length, in bytes (including null terminator) of the reason string
/// when a connection is closed.
const k_cchSteamNetworkingMaxConnectionCloseReason: integer = 128;

/// Max length, in bytes (include null terminator) of debug description
/// of a connection.
const k_cchSteamNetworkingMaxConnectionDescription: integer = 128;

type SteamNetConnectionInfo_t = record

  /// Who is on the other end?  Depending on the connection type and phase of the connection, we might not know
  m_identityRemote: SteamNetworkingIdentity;

  /// Arbitrary user data set by the local application code
  m_nUserData: int64;

  /// Handle to listen socket this was connected on, or k_HSteamListenSocket_Invalid if we initiated the connection
  m_hListenSocket: HSteamListenSocket;

  /// Remote address.  Might be all 0's if we don't know it, or if this is N/A.
  /// (E.g. Basically everything except direct UDP connection.)
  m_addrRemote: SteamNetworkingIPAddr;
  m__pad1: uint16;

  /// What data center is the remote host in?  (0 if we don't know.)
  m_idPOPRemote: SteamNetworkingPOPID;

  /// What relay are we using to communicate with the remote host?
  /// (0 if not applicable.)
  m_idPOPRelay: SteamNetworkingPOPID;

  /// High level state of the connection
  m_eState: ESteamNetworkingConnectionState;

  /// Basic cause of the connection termination or problem.
  /// See ESteamNetConnectionEnd for the values used
  m_eEndReason: Integer;

  /// Human-readable, but non-localized explanation for connection
  /// termination or problem.  This is intended for debugging /
  /// diagnostic purposes only, not to display to users.  It might
  /// have some details specific to the issue.
  m_szEndDebug: array [0..127] of Char;

  /// Debug description.  This includes the internal connection ID,
  /// connection type (and peer information), and any name
  /// given to the connection by the app.  This string is used in various
  /// internal logging messages.
  m_szConnectionDescription: array[0..127] of Char;

  /// What kind of transport is currently being used?
  /// Note that this is potentially a dynamic property!  Also, it may not
  /// always be available, especially right as the connection starts, or
  /// after the connection ends.
  m_eTransportKind: ESteamNetTransportKind;

  /// Internal stuff, room to change API easily
  //reserved: array[0..62] of uint32;
  reserved: array[0..62] of uint32;
end;

type
  PSteamNetConnectionInfo_t = ^SteamNetConnectionInfo_t;

type SteamNetworkingQuickConnectionStatus = record

  /// High level state of the connection
  m_eState: ESteamNetworkingConnectionState;

  /// Current ping (ms)
  m_nPing: Integer;

  /// Connection quality measured locally, 0...1.  (Percentage of packets delivered
  /// end-to-end in order).
  m_flConnectionQualityLocal: Single;

  /// Packet delivery success rate as observed from remote host
  m_flConnectionQualityRemote: Single;

  /// Current data rates from recent history.
  m_flOutPacketsPerSec: Single;
  m_flOutBytesPerSec: Single;
  m_flInPacketsPerSec: Single;
  m_flInBytesPerSec: Single;

  /// Estimate rate that we believe that we can send data to our peer.
  /// Note that this could be significantly higher than m_flOutBytesPerSec,
  /// meaning the capacity of the channel is higher than you are sending data.
  /// (That's OK!)
  m_nSendRateBytesPerSecond: Integer;

  /// Number of bytes pending to be sent.  This is data that you have recently
  /// requested to be sent but has not yet actually been put on the wire.  The
  /// reliable number ALSO includes data that was previously placed on the wire,
  /// but has now been scheduled for re-transmission.  Thus, it's possible to
  /// observe m_cbPendingReliable increasing between two checks, even if no
  /// calls were made to send reliable data between the checks.  Data that is
  /// awaiting the Nagle delay will appear in these numbers.
  m_cbPendingUnreliable: Integer;
  m_cbPendingReliable: Integer;

  /// Number of bytes of reliable data that has been placed the wire, but
  /// for which we have not yet received an acknowledgment, and thus we may
  /// have to re-transmit.
  m_cbSentUnackedReliable: Integer;

  /// If you asked us to send a message right now, how long would that message
  /// sit in the queue before we actually started putting packets on the wire?
  /// (And assuming Nagle does not cause any packets to be delayed.)
  ///
  /// In general, data that is sent by the application is limited by the
  /// bandwidth of the channel.  If you send data faster than this, it must
  /// be queued and put on the wire at a metered rate.  Even sending a small amount
  /// of data (e.g. a few MTU, say ~3k) will require some of the data to be delayed
  /// a bit.
  ///
  /// In general, the estimated delay will be approximately equal to
  ///
  ///    (m_cbPendingUnreliable+m_cbPendingReliable) / m_nSendRateBytesPerSecond
  ///
  /// plus or minus one MTU.  It depends on how much time has elapsed since the last
  /// packet was put on the wire.  For example, the queue might have *just* been emptied,
  /// and the last packet placed on the wire, and we are exactly up against the send
  /// rate limit.  In that case we might need to wait for one packet's worth of time to
  /// elapse before we can send again.  On the other extreme, the queue might have data
  /// in it waiting for Nagle.  (This will always be less than one packet, because as soon
  /// as we have a complete packet we would send it.)  In that case, we might be ready
  /// to send data now, and this value will be 0.
  m_usecQueueTime: SteamNetworkingMicroseconds;

  /// Internal stuff, room to change API easily
  reserved: array [0..15] of uint32;
end;

type
  PSteamNetworkingQuickConnectionStatus = ^SteamNetworkingQuickConnectionStatus;

const k_cbMaxSteamNetworkingSocketsMessageSizeSend: Integer = 512 * 1024;

type SteamNetworkingMessage_t = record

  /// Message payload
  m_pData: Pointer;

  /// Size of the payload.
  m_cbSize: Integer;

  /// For messages received on connections: what connection did this come from?
  /// For outgoing messages: what connection to send it to?
  /// Not used when using the ISteamNetworkingMessages interface
  m_conn: HSteamNetConnection;

  /// For inbound messages: Who sent this to us?
  /// For outbound messages on connections: not used.
  /// For outbound messages on the ad-hoc ISteamNetworkingMessages interface: who should we send this to?
  m_identityPeer: SteamNetworkingIdentity;

  /// For messages received on connections, this is the user data
  /// associated with the connection.
  ///
  /// This is *usually* the same as calling GetConnection() and then
  /// fetching the user data associated with that connection, but for
  /// the following subtle differences:
  ///
  /// - This user data will match the connection's user data at the time
  ///   is captured at the time the message is returned by the API.
  ///   If you subsequently change the userdata on the connection,
  ///   this won't be updated.
  /// - This is an inline call, so it's *much* faster.
  /// - You might have closed the connection, so fetching the user data
  ///   would not be possible.
  ///
  /// Not used when sending messages,
  m_nConnUserData: int64;

  /// Local timestamp when the message was received
  /// Not used for outbound messages.
  m_usecTimeReceived: SteamNetworkingMicroseconds;

  /// Message number assigned by the sender.
  /// This is not used for outbound messages
  m_nMessageNumber: int64;

  /// Function used to free up m_pData.  This mechanism exists so that
  /// apps can create messages with buffers allocated from their own
  /// heap, and pass them into the library.  This function will
  /// usually be something like:
  ///
  /// free(pMsg->m_pData);
  //void (*m_pfnFreeData)(SteamNetworkingMessage_t *pMsg);
  m_pfnFreeData: procedure(pMsg: Pointer);

  /// Function to used to decrement the internal reference count and, if
  /// it's zero, release the message.  You should not set this function pointer,
  /// or need to access this directly!  Use the Release() function instead!
  //void (*m_pfnRelease)(SteamNetworkingMessage_t *pMsg);
  //procedure m_pfnRelease(); cdecl;
  m_pfnRelease: procedure(pMsg: Pointer);

  /// When using ISteamNetworkingMessages, the channel number the message was received on
  /// (Not used for messages sent or received on "connections")
  m_nChannel: Integer;

  /// Bitmask of k_nSteamNetworkingSend_xxx flags.
  /// For received messages, only the k_nSteamNetworkingSend_Reliable bit is valid.
  /// For outbound messages, all bits are relevant
  m_nFlags: Integer;

  /// Arbitrary user data that you can use when sending messages using
  /// ISteamNetworkingUtils::AllocateMessage and ISteamNetworkingSockets::SendMessage.
  /// (The callback you set in m_pfnFreeData might use this field.)
  ///
  /// Not used for received messages.
  m_nUserData: int64;

  /// You MUST call this when you're done with the object,
  /// to free up memory, etc.
  //inline void Release();
end;

type
  PSteamNetworkingMessage_t = ^SteamNetworkingMessage_t;


//
// Flags used to set options for message sending
//

// Send the message unreliably. Can be lost.  Messages *can* be larger than a
// single MTU (UDP packet), but there is no retransmission, so if any piece
// of the message is lost, the entire message will be dropped.
//
// The sending API does have some knowledge of the underlying connection, so
// if there is no NAT-traversal accomplished or there is a recognized adjustment
// happening on the connection, the packet will be batched until the connection
// is open again.
//
// Migration note: This is not exactly the same as k_EP2PSendUnreliable!  You
// probably want k_ESteamNetworkingSendType_UnreliableNoNagle
const k_nSteamNetworkingSend_Unreliable: Integer = 0;

// Disable Nagle's algorithm.
// By default, Nagle's algorithm is applied to all outbound messages.  This means
// that the message will NOT be sent immediately, in case further messages are
// sent soon after you send this, which can be grouped together.  Any time there
// is enough buffered data to fill a packet, the packets will be pushed out immediately,
// but partially-full packets not be sent until the Nagle timer expires.  See
// ISteamNetworkingSockets::FlushMessagesOnConnection, ISteamNetworkingMessages::FlushMessagesToUser
//
// NOTE: Don't just send every message without Nagle because you want packets to get there
// quicker.  Make sure you understand the problem that Nagle is solving before disabling it.
// If you are sending small messages, often many at the same time, then it is very likely that
// it will be more efficient to leave Nagle enabled.  A typical proper use of this flag is
// when you are sending what you know will be the last message sent for a while (e.g. the last
// in the server simulation tick to a particular client), and you use this flag to flush all
// messages.
const k_nSteamNetworkingSend_NoNagle: Integer = 1;

// Send a message unreliably, bypassing Nagle's algorithm for this message and any messages
// currently pending on the Nagle timer.  This is equivalent to using k_ESteamNetworkingSend_Unreliable
// and then immediately flushing the messages using ISteamNetworkingSockets::FlushMessagesOnConnection
// or ISteamNetworkingMessages::FlushMessagesToUser.  (But using this flag is more efficient since you
// only make one API call.)
//FIXME: const k_nSteamNetworkingSend_UnreliableNoNagle: Integer = k_nSteamNetworkingSend_Unreliable or k_nSteamNetworkingSend_NoNagle;

// If the message cannot be sent very soon (because the connection is still doing some initial
// handshaking, route negotiations, etc), then just drop it.  This is only applicable for unreliable
// messages.  Using this flag on reliable messages is invalid.
const k_nSteamNetworkingSend_NoDelay: Integer = 4;

// Send an unreliable message, but if it cannot be sent relatively quickly, just drop it instead of queuing it.
// This is useful for messages that are not useful if they are excessively delayed, such as voice data.
// NOTE: The Nagle algorithm is not used, and if the message is not dropped, any messages waiting on the
// Nagle timer are immediately flushed.
//
// A message will be dropped under the following circumstances:
// - the connection is not fully connected.  (E.g. the "Connecting" or "FindingRoute" states)
// - there is a sufficiently large number of messages queued up already such that the current message
//   will not be placed on the wire in the next ~200ms or so.
//
// If a message is dropped for these reasons, k_EResultIgnored will be returned.
//FIXME: const k_nSteamNetworkingSend_UnreliableNoDelay: Integer = k_nSteamNetworkingSend_Unreliable or k_nSteamNetworkingSend_NoDelay or k_nSteamNetworkingSend_NoNagle;

// Reliable message send. Can send up to k_cbMaxSteamNetworkingSocketsMessageSizeSend bytes in a single message.
// Does fragmentation/re-assembly of messages under the hood, as well as a sliding window for
// efficient sends of large chunks of data.
//
// The Nagle algorithm is used.  See notes on k_ESteamNetworkingSendType_Unreliable for more details.
// See k_ESteamNetworkingSendType_ReliableNoNagle, ISteamNetworkingSockets::FlushMessagesOnConnection,
// ISteamNetworkingMessages::FlushMessagesToUser
//
// Migration note: This is NOT the same as k_EP2PSendReliable, it's more like k_EP2PSendReliableWithBuffering
const k_nSteamNetworkingSend_Reliable: Integer = 8;

// Send a message reliably, but bypass Nagle's algorithm.
//
// Migration note: This is equivalent to k_EP2PSendReliable
//FIXME: const k_nSteamNetworkingSend_ReliableNoNagle: Integer = k_nSteamNetworkingSend_Reliable or k_nSteamNetworkingSend_NoNagle;

// By default, message sending is queued, and the work of encryption and talking to
// the operating system sockets, etc is done on a service thread.  This is usually a
// a performance win when messages are sent from the "main thread".  However, if this
// flag is set, and data is ready to be sent immediately (either from this message
// or earlier queued data), then that work will be done in the current thread, before
// the current call returns.  If data is not ready to be sent (due to rate limiting
// or Nagle), then this flag has no effect.
//
// This is an advanced flag used to control performance at a very low level.  For
// most applications running on modern hardware with more than one CPU core, doing
// the work of sending on a service thread will yield the best performance.  Only
// use this flag if you have a really good reason and understand what you are doing.
// Otherwise you will probably just make performance worse.
const k_nSteamNetworkingSend_UseCurrentThread: Integer = 16;

// When sending a message using ISteamNetworkingMessages, automatically re-establish
// a broken session, without returning k_EResultNoConnection.  Without this flag,
// if you attempt to send a message, and the session was proactively closed by the
// peer, or an error occurred that disrupted communications, then you must close the
// session using ISteamNetworkingMessages::CloseSessionWithUser before attempting to
// send another message.  (Or you can simply add this flag and retry.)  In this way,
// the disruption cannot go unnoticed, and a more clear order of events can be
// ascertained. This is especially important when reliable messages are used, since
// if the connection is disrupted, some of those messages will not have been delivered,
// and it is in general not possible to know which.  Although a
// SteamNetworkingMessagesSessionFailed_t callback will be posted when an error occurs
// to notify you that a failure has happened, callbacks are asynchronous, so it is not
// possible to tell exactly when it happened.  And because the primary purpose of
// ISteamNetworkingMessages is to be like UDP, there is no notification when a peer closes
// the session.
//
// If you are not using any reliable messages (e.g. you are using ISteamNetworkingMessages
// exactly as a transport replacement for UDP-style datagrams only), you may not need to
// know when an underlying connection fails, and so you may not need this notification.
const k_nSteamNetworkingSend_AutoRestartBrokenSession: Integer = 32;

//
// Ping location / measurement
//

/// Object that describes a "location" on the Internet with sufficient
/// detail that we can reasonably estimate an upper bound on the ping between
/// the two hosts, even if a direct route between the hosts is not possible,
/// and the connection must be routed through the Steam Datagram Relay network.
/// This does not contain any information that identifies the host.  Indeed,
/// if two hosts are in the same building or otherwise have nearly identical
/// networking characteristics, then it's valid to use the same location
/// object for both of them.
///
/// NOTE: This object should only be used in the same process!  Do not serialize it,
/// send it over the wire, or persist it in a file or database!  If you need
/// to do that, convert it to a string representation using the methods in
/// ISteamNetworkingUtils().
type SteamNetworkPingLocation_t = record
  m_data: array [0..511] of uint8;
end;

type PSteamNetworkPingLocation_t = ^SteamNetworkPingLocation_t;

/// Max possible length of a ping location, in string format.  This is
/// an extremely conservative worst case value which leaves room for future
/// syntax enhancements.  Most strings in practice are a lot shorter.
/// If you are storing many of these, you will very likely benefit from
/// using dynamic memory.
const k_cchMaxSteamNetworkingPingLocationString: Integer = 1024;

/// Special values that are returned by some functions that return a ping.
const k_nSteamNetworkingPing_Failed: Integer = -1;
const k_nSteamNetworkingPing_Unknown: Integer = -2;

//
// Configuration values
//

/// Configuration values can be applied to different types of objects.
type ESteamNetworkingConfigScope = (

  /// Get/set global option, or defaults.  Even options that apply to more specific scopes
  /// have global scope, and you may be able to just change the global defaults.  If you
  /// need different settings per connection (for example), then you will need to set those
  /// options at the more specific scope.
  k_ESteamNetworkingConfig_Global = 1,

  /// Some options are specific to a particular interface.  Note that all connection
  /// and listen socket settings can also be set at the interface level, and they will
  /// apply to objects created through those interfaces.
  k_ESteamNetworkingConfig_SocketsInterface = 2,

  /// Options for a listen socket.  Listen socket options can be set at the interface layer,
  /// if  you have multiple listen sockets and they all use the same options.
  /// You can also set connection options on a listen socket, and they set the defaults
  /// for all connections accepted through this listen socket.  (They will be used if you don't
  /// set a connection option.)
  k_ESteamNetworkingConfig_ListenSocket = 3,

  /// Options for a specific connection.
  k_ESteamNetworkingConfig_Connection = 4,

  k_ESteamNetworkingConfigScope__Force32Bit = $7fffffff
);

type PESteamNetworkingConfigScope = ^ESteamNetworkingConfigScope;

// Different configuration values have different data types
type ESteamNetworkingConfigDataType = (
  k_ESteamNetworkingConfig_Int32 = 1,
  k_ESteamNetworkingConfig_Int64 = 2,
  k_ESteamNetworkingConfig_Float = 3,
  k_ESteamNetworkingConfig_String = 4,
  k_ESteamNetworkingConfig_Ptr = 5, // NOTE: When setting  callbacks, you should put the pointer into a variable and pass a pointer to that variable.

  k_ESteamNetworkingConfigDataType__Force32Bit = $7fffffff
);

type PESteamNetworkingConfigDataType = ^ESteamNetworkingConfigDataType;

/// Configuration options
{$push}
{$notes off}
type ESteamNetworkingConfigValue = (
	k_ESteamNetworkingConfig_Invalid = 0,

	/// [global float, 0--100] Randomly discard N pct of packets instead of sending/recv
	/// This is a global option only, since it is applied at a low level
	/// where we don't have much context
	k_ESteamNetworkingConfig_FakePacketLoss_Send = 2,
	k_ESteamNetworkingConfig_FakePacketLoss_Recv = 3,

	/// [global int32].  Delay all outbound/inbound packets by N ms
	k_ESteamNetworkingConfig_FakePacketLag_Send = 4,
	k_ESteamNetworkingConfig_FakePacketLag_Recv = 5,

	/// [global float] 0-100 Percentage of packets we will add additional delay
	/// to (causing them to be reordered)
	k_ESteamNetworkingConfig_FakePacketReorder_Send = 6,
	k_ESteamNetworkingConfig_FakePacketReorder_Recv = 7,

	/// [global int32] Extra delay, in ms, to apply to reordered packets.
	k_ESteamNetworkingConfig_FakePacketReorder_Time = 8,

	/// [global float 0--100] Globally duplicate some percentage of packets we send
	k_ESteamNetworkingConfig_FakePacketDup_Send = 26,
	k_ESteamNetworkingConfig_FakePacketDup_Recv = 27,

	/// [global int32] Amount of delay, in ms, to delay duplicated packets.
	/// (We chose a random delay between 0 and this value)
	k_ESteamNetworkingConfig_FakePacketDup_TimeMax = 28,

	/// [connection int32] Timeout value (in ms) to use when first connecting
	k_ESteamNetworkingConfig_TimeoutInitial = 24,

	/// [connection int32] Timeout value (in ms) to use after connection is established
	k_ESteamNetworkingConfig_TimeoutConnected = 25,

	/// [connection int32] Upper limit of buffered pending bytes to be sent,
	/// if this is reached SendMessage will return k_EResultLimitExceeded
	/// Default is 512k (524288 bytes)
	k_ESteamNetworkingConfig_SendBufferSize = 9,

	/// [connection int32] Minimum/maximum send rate clamp, 0 is no limit.
	/// This value will control the min/max allowed sending rate that 
  /// This value will control the min/max allowed sending rate that
	/// This value will control the min/max allowed sending rate that 
  /// This value will control the min/max allowed sending rate that
	/// This value will control the min/max allowed sending rate that 
	/// bandwidth estimation is allowed to reach.  Default is 0 (no-limit)
	k_ESteamNetworkingConfig_SendRateMin = 10,
	k_ESteamNetworkingConfig_SendRateMax = 11,

	/// [connection int32] Nagle time, in microseconds.  When SendMessage is called, if
	/// the outgoing message is less than the size of the MTU, it will be
	/// queued for a delay equal to the Nagle timer value.  This is to ensure
	/// that if the application sends several small messages rapidly, they are
	/// coalesced into a single packet.
	/// See historical RFC 896.  Value is in microseconds. 
  /// See historical RFC 896.  Value is in microseconds.
	/// See historical RFC 896.  Value is in microseconds. 
  /// See historical RFC 896.  Value is in microseconds.
	/// See historical RFC 896.  Value is in microseconds. 
	/// Default is 5000us (5ms).
	k_ESteamNetworkingConfig_NagleTime = 12,

	/// [connection int32] Don't automatically fail IP connections that don't have
	/// strong auth.  On clients, this means we will attempt the connection even if
	/// we don't know our identity or can't get a cert.  On the server, it means that
	/// we won't automatically reject a connection due to a failure to authenticate.
	/// (You can examine the incoming connection and decide whether to accept it.)
	///
	/// This is a dev configuration value, and you should not let users modify it in
	/// production.
	k_ESteamNetworkingConfig_IP_AllowWithoutAuth = 23,

	/// [connection int32] Do not send UDP packets with a payload of
	/// larger than N bytes.  If you set this, k_ESteamNetworkingConfig_MTU_DataSize
	/// is automatically adjusted
	k_ESteamNetworkingConfig_MTU_PacketSize = 32,

	/// [connection int32] (read only) Maximum message size you can send that
	/// will not fragment, based on k_ESteamNetworkingConfig_MTU_PacketSize
	k_ESteamNetworkingConfig_MTU_DataSize = 33,

	/// [connection int32] Allow unencrypted (and unauthenticated) communication.
	/// 0: Not allowed (the default)
	/// 1: Allowed, but prefer encrypted
	/// 2: Allowed, and preferred
	/// 3: Required.  (Fail the connection if the peer requires encryption.)
	///
	/// This is a dev configuration value, since its purpose is to disable encryption.
	/// You should not let users modify it in production.  (But note that it requires
	/// the peer to also modify their value in order for encryption to be disabled.)
	k_ESteamNetworkingConfig_Unencrypted = 34,

	/// [global int32] 0 or 1.  Some variables are "dev" variables.  They are useful
	/// for debugging, but should not be adjusted in production.  When this flag is false (the default),
	/// such variables will not be enumerated by the ISteamnetworkingUtils::GetFirstConfigValue
	/// ISteamNetworkingUtils::GetConfigValueInfo functions.  The idea here is that you
	/// can use those functions to provide a generic mechanism to set any configuration
	/// value from a console or configuration file, looking up the variable by name.  Depending
	/// on your game, modifying other configuration values may also have negative effects, and
	/// you may wish to further lock down which variables are allowed to be modified by the user.
	/// (Maybe no variables!)  Or maybe you use a whitelist or blacklist approach.
	///
	/// (This flag is itself a dev variable.)
	k_ESteamNetworkingConfig_EnumerateDevVars = 35,

	/// [connection int32] Set this to 1 on outbound connections and listen sockets,
	/// to enable "symmetric connect mode", which is useful in the following
	/// common peer-to-peer use case:
	///
	/// - The two peers are "equal" to each other.  (Neither is clearly the "client"
	///   or "server".)
	/// - Either peer may initiate the connection, and indeed they may do this
	///   at the same time
	/// - The peers only desire a single connection to each other, and if both
	///   peers initiate connections simultaneously, a protocol is needed for them
	///   to resolve the conflict, so that we end up with a single connection.
	///
	/// This use case is both common, and involves subtle race conditions and tricky
	/// pitfalls, which is why the API has support for dealing with it.
	///
	/// If an incoming connection arrives on a listen socket or via custom signaling,
	/// and the application has not attempted to make a matching outbound connection
	/// in symmetric mode, then the incoming connection can be accepted as usual.
	/// A "matching" connection means that the relevant endpoint information matches.
	/// (At the time this comment is being written, this is only supported for P2P
	/// connections, which means that the peer identities must match, and the virtual
	/// port must match.  At a later time, symmetric mode may be supported for other
	/// connection types.)
	///
	/// If connections are initiated by both peers simultaneously, race conditions
	/// can arise, but fortunately, most of them are handled internally and do not
	/// require any special awareness from the application.  However, there
	/// is one important case that application code must be aware of:
	/// If application code attempts an outbound connection using a ConnectXxx
	/// function in symmetric mode, and a matching incoming connection is already
	/// waiting on a listen socket, then instead of forming a new connection,
	/// the ConnectXxx call will accept the existing incoming connection, and return
	/// a connection handle to this accepted connection.
	/// IMPORTANT: in this case, a SteamNetConnectionStatusChangedCallback_t
	/// has probably *already* been posted to the queue for the incoming connection!
	/// (Once callbacks are posted to the queue, they are not modified.)  It doesn't
	/// matter if the callback has not been consumed by the app.  Thus, application
	/// code that makes use of symmetric connections must be aware that, when processing a
	/// SteamNetConnectionStatusChangedCallback_t for an incoming connection, the
	/// m_hConn may refer to a new connection that the app has has not
	/// seen before (the usual case), but it may also refer to a connection that
	/// has already been accepted implicitly through a call to Connect()!  In this
	/// case, AcceptConnection() will return k_EResultDuplicateRequest.
	///
	/// Only one symmetric connection to a given peer (on a given virtual port)
	/// may exist at any given time.  If client code attempts to create a connection,
	/// and a (live) connection already exists on the local host, then either the
	/// existing connection will be accepted as described above, or the attempt
	/// to create a new connection will fail.  Furthermore, linger mode functionality
	/// is not supported on symmetric connections.
	///
	/// A more complicated race condition can arise if both peers initiate a connection
	/// at roughly the same time.  In this situation, each peer will receive an incoming
	/// connection from the other peer, when the application code has already initiated
	/// an outgoing connection to that peer.  The peers must resolve this conflict and
	/// decide who is going to act as the "server" and who will act as the "client".
	/// Typically the application does not need to be aware of this case as it is handled
	/// internally.  On both sides, the will observe their outbound connection being
	/// "accepted", although one of them one have been converted internally to act
	/// as the "server".
	///
	/// In general, symmetric mode should be all-or-nothing: do not mix symmetric
	/// connections with a non-symmetric connection that it might possible "match"
	/// with.  If you use symmetric mode on any connections, then both peers should
	/// use it on all connections, and the corresponding listen socket, if any.  The
	/// behaviour when symmetric and ordinary connections are mixed is not defined by
	/// this API, and you should not rely on it.  (This advice only applies when connections
	/// might possibly "match".  For example, it's OK to use all symmetric mode
	/// connections on one virtual port, and all ordinary, non-symmetric connections
	/// on a different virtual port, as there is no potential for ambiguity.)
	///
	/// When using the feature, you should set it in the following situations on
	/// applicable objects:
	///
	/// - When creating an outbound connection using ConnectXxx function
	/// - When creating a listen socket.  (Note that this will automatically cause
	///   any accepted connections to inherit the flag.)
	/// - When using custom signaling, before accepting an incoming connection.
	///
	/// Setting the flag on listen socket and accepted connections will enable the
	/// API to automatically deal with duplicate incoming connections, even if the
	/// local host has not made any outbound requests.  (In general, such duplicate
	/// requests from a peer are ignored internally and will not be visible to the
	/// application code.  The previous connection must be closed or resolved first.)
	k_ESteamNetworkingConfig_SymmetricConnect = 37,

	/// [connection int32] For connection types that use "virtual ports", this can be used
	/// to assign a local virtual port.  For incoming connections, this will always be the
	/// virtual port of the listen socket (or the port requested by the remote host if custom
	/// signaling is used and the connection is accepted), and cannot be changed.  For
	/// connections initiated locally, the local virtual port will default to the same as the
	/// requested remote virtual port, if you do not specify a different option when creating
	/// the connection.  The local port is only relevant for symmetric connections, when
	/// determining if two connections "match."  In this case, if you need the local and remote
	/// port to differ, you can set this value.
	///
	/// You can also read back this value on listen sockets.
	///
	/// This value should not be read or written in any other context.
	k_ESteamNetworkingConfig_LocalVirtualPort = 38,

	//
	// Callbacks
	//

	// On Steam, you may use the default Steam callback dispatch mechanism.  If you prefer
	// to not use this dispatch mechanism (or you are not running with Steam), or you want
	// to associate specific functions with specific listen sockets or connections, you can
	// register them as configuration values.
	//
	// Note also that ISteamNetworkingUtils has some helpers to set these globally.

	/// [connection FnSteamNetConnectionStatusChanged] Callback that will be invoked
	/// when the state of a connection changes.
	///
	/// IMPORTANT: callbacks are dispatched to the handler that is in effect at the time
	/// the event occurs, which might be in another thread.  For example, immediately after
	/// creating a listen socket, you may receive an incoming connection.  And then immediately
	/// after this, the remote host may close the connection.  All of this could happen
	/// before the function to create the listen socket has returned.  For this reason,
	/// callbacks usually must be in effect at the time of object creation.  This means
	/// you should set them when you are creating the listen socket or connection, or have
	/// them in effect so they will be inherited at the time of object creation.
	///
	/// For example:
	///
	/// exterm void MyStatusChangedFunc( SteamNetConnectionStatusChangedCallback_t *info );
	/// SteamNetworkingConfigValue_t opt; opt.SetPtr( k_ESteamNetworkingConfig_Callback_ConnectionStatusChanged, MyStatusChangedFunc );
	/// SteamNetworkingIPAddr localAddress; localAddress.Clear();
	/// HSteamListenSocket hListenSock = SteamNetworkingSockets()->CreateListenSocketIP( localAddress, 1, &opt );
	///
	/// When accepting an incoming connection, there is no atomic way to switch the
	/// callback.  However, if the connection is DOA, AcceptConnection() will fail, and
	/// you can fetch the state of the connection at that time.
	///
	/// If all connections and listen sockets can use the same callback, the simplest
	/// method is to set it globally before you create any listen sockets or connections.
	k_ESteamNetworkingConfig_Callback_ConnectionStatusChanged = 201,

	/// [global FnSteamNetAuthenticationStatusChanged] Callback that will be invoked
	/// when our auth state changes.  If you use this, install the callback before creating
	/// any connections or listen sockets, and don't change it.
	/// See: ISteamNetworkingUtils::SetGlobalCallback_SteamNetAuthenticationStatusChanged
	k_ESteamNetworkingConfig_Callback_AuthStatusChanged = 202,

	/// [global FnSteamRelayNetworkStatusChanged] Callback that will be invoked
	/// when our auth state changes.  If you use this, install the callback before creating
	/// any connections or listen sockets, and don't change it.
	/// See: ISteamNetworkingUtils::SetGlobalCallback_SteamRelayNetworkStatusChanged
	k_ESteamNetworkingConfig_Callback_RelayNetworkStatusChanged = 203,

	/// [global FnSteamNetworkingMessagesSessionRequest] Callback that will be invoked
	/// when a peer wants to initiate a SteamNetworkingMessagesSessionRequest.
	/// See: ISteamNetworkingUtils::SetGlobalCallback_MessagesSessionRequest
	k_ESteamNetworkingConfig_Callback_MessagesSessionRequest = 204,

	/// [global FnSteamNetworkingMessagesSessionFailed] Callback that will be invoked
	/// when a session you have initiated, or accepted either fails to connect, or loses
	/// connection in some unexpected way.
	/// See: ISteamNetworkingUtils::SetGlobalCallback_MessagesSessionFailed
	k_ESteamNetworkingConfig_Callback_MessagesSessionFailed = 205,

	//
	// P2P settings
	//

//	/// [listen socket int32] When you create a P2P listen socket, we will automatically
//	/// open up a UDP port to listen for LAN connections.  LAN connections can be made
//	/// without any signaling: both sides can be disconnected from the Internet.
//	///
//	/// This value can be set to zero to disable the feature.
//	k_ESteamNetworkingConfig_P2P_Discovery_Server_LocalPort = 101,
//
//	/// [connection int32] P2P connections can perform broadcasts looking for the peer
//	/// on the LAN.
//	k_ESteamNetworkingConfig_P2P_Discovery_Client_RemotePort = 102,

	/// [connection string] Comma-separated list of STUN servers that can be used
	/// for NAT piercing.  If you set this to an empty string, NAT piercing will
	/// not be attempted.  Also if "public" candidates are not allowed for
	/// P2P_Transport_ICE_Enable, then this is ignored.
	k_ESteamNetworkingConfig_P2P_STUN_ServerList = 103,

	/// [connection int32] What types of ICE candidates to share with the peer.
	/// See k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_xxx values
	k_ESteamNetworkingConfig_P2P_Transport_ICE_Enable = 104,

	/// [connection int32] When selecting P2P transport, add various
	/// penalties to the scores for selected transports.  (Route selection
	/// scores are on a scale of milliseconds.  The score begins with the
	/// route ping time and is then adjusted.)
	k_ESteamNetworkingConfig_P2P_Transport_ICE_Penalty = 105,
	k_ESteamNetworkingConfig_P2P_Transport_SDR_Penalty = 106,
	//k_ESteamNetworkingConfig_P2P_Transport_LANBeacon_Penalty = 107,

	//
	// Settings for SDR relayed connections
	//

	/// [int32 global] If the first N pings to a port all fail, mark that port as unavailable for
	/// a while, and try a different one.  Some ISPs and routers may drop the first
	/// packet, so setting this to 1 may greatly disrupt communications.
	k_ESteamNetworkingConfig_SDRClient_ConsecutitivePingTimeoutsFailInitial = 19,

	/// [int32 global] If N consecutive pings to a port fail, after having received successful 
  /// [int32 global] If N consecutive pings to a port fail, after having received successful
	/// [int32 global] If N consecutive pings to a port fail, after having received successful 
  /// [int32 global] If N consecutive pings to a port fail, after having received successful
	/// [int32 global] If N consecutive pings to a port fail, after having received successful 
	/// communication, mark that port as unavailable for a while, and try a 
  /// communication, mark that port as unavailable for a while, and try a
	/// communication, mark that port as unavailable for a while, and try a 
  /// communication, mark that port as unavailable for a while, and try a
	/// communication, mark that port as unavailable for a while, and try a 
	/// different one.
	k_ESteamNetworkingConfig_SDRClient_ConsecutitivePingTimeoutsFail = 20,

	/// [int32 global] Minimum number of lifetime pings we need to send, before we think our estimate
	/// is solid.  The first ping to each cluster is very often delayed because of NAT,
	/// routers not having the best route, etc.  Until we've sent a sufficient number
	/// of pings, our estimate is often inaccurate.  Keep pinging until we get this
	/// many pings.
	k_ESteamNetworkingConfig_SDRClient_MinPingsBeforePingAccurate = 21,

	/// [int32 global] Set all steam datagram traffic to originate from the same
	/// local port. By default, we open up a new UDP socket (on a different local
	/// port) for each relay.  This is slightly less optimal, but it works around
	/// some routers that don't implement NAT properly.  If you have intermittent
	/// problems talking to relays that might be NAT related, try toggling
	/// this flag
	k_ESteamNetworkingConfig_SDRClient_SingleSocket = 22,

	/// [global string] Code of relay cluster to force use.  If not empty, we will
	/// only use relays in that cluster.  E.g. 'iad'
	k_ESteamNetworkingConfig_SDRClient_ForceRelayCluster = 29,

	/// [connection string] For debugging, generate our own (unsigned) ticket, using
	/// the specified  gameserver address.  Router must be configured to accept unsigned
	/// tickets.
	k_ESteamNetworkingConfig_SDRClient_DebugTicketAddress = 30,

	/// [global string] For debugging.  Override list of relays from the config with
	/// this set (maybe just one).  Comma-separated list.
	k_ESteamNetworkingConfig_SDRClient_ForceProxyAddr = 31,

	/// [global string] For debugging.  Force ping times to clusters to be the specified
	/// values.  A comma separated list of <cluster>=<ms> values.  E.g. "sto=32,iad=100"
	///
	/// This is a dev configuration value, you probably should not let users modify it
	/// in production.
	k_ESteamNetworkingConfig_SDRClient_FakeClusterPing = 36,

	//
	// Log levels for debugging information of various subsystems.
	// Higher numeric values will cause more stuff to be printed.
	// See ISteamNetworkingUtils::SetDebugOutputFunction for more
	// information
	//
	// The default for all values is k_ESteamNetworkingSocketsDebugOutputType_Warning.
	//
	k_ESteamNetworkingConfig_LogLevel_AckRTT = 13, // [connection int32] RTT calculations for inline pings and replies
	k_ESteamNetworkingConfig_LogLevel_PacketDecode = 14, // [connection int32] log SNP packets send/recv
	k_ESteamNetworkingConfig_LogLevel_Message = 15, // [connection int32] log each message send/recv
	k_ESteamNetworkingConfig_LogLevel_PacketGaps = 16, // [connection int32] dropped packets
	k_ESteamNetworkingConfig_LogLevel_P2PRendezvous = 17, // [connection int32] P2P rendezvous messages
	k_ESteamNetworkingConfig_LogLevel_SDRRelayPings = 18, // [global int32] Ping relays

	k_ESteamNetworkingConfigValue__Force32Bit = $7fffffff
);
type PESteamNetworkingConfigValue = ^ESteamNetworkingConfigValue;
{$pop}

/// In a few places we need to set configuration options on listen sockets and connections, and
/// have them take effect *before* the listen socket or connection really starts doing anything.
/// Creating the object and then setting the options "immediately" after creation doesn't work
/// completely, because network packets could be received between the time the object is created and
/// when the options are applied.  To set options at creation time in a reliable way, they must be
/// passed to the creation function.  This structure is used to pass those options.
///
/// For the meaning of these fields, see ISteamNetworkingUtils::SetConfigValue.  Basically
/// when the object is created, we just iterate over the list of options and call
/// ISteamNetworkingUtils::SetConfigValueStruct, where the scope arguments are supplied by the
/// object being created.
{$PACKRECORDS 8}
type SteamNetworkingConfigValue_t = record
  /// Which option is being set
  m_eValue: ESteamNetworkingConfigValue;

  /// Which field below did you fill in?
  m_eDataType: ESteamNetworkingConfigDataType;

  /// Option value
  case integer of
      0: (m_int32: int32);
      1: (m_int64: int64);
      2: (m_float: Single);
      3: (m_string: PAnsiChar);
      4: (m_functionPtr: procedure);
end;
{$IFDEF STEAM}
{$IFDEF UNIX}
{$PACKRECORDS 4}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}


type PSteamNetworkingConfigValue_t = ^SteamNetworkingConfigValue_t;

/// Return value of ISteamNetworkintgUtils::GetConfigValue
{$push}
{$notes off}
type ESteamNetworkingGetConfigValueResult = (
  k_ESteamNetworkingGetConfigValue_BadValue = -1,  // No such configuration value
  k_ESteamNetworkingGetConfigValue_BadScopeObj = -2,  // Bad connection handle, etc
  k_ESteamNetworkingGetConfigValue_BufferTooSmall = -3, // Couldn't fit the result in your buffer
  k_ESteamNetworkingGetConfigValue_OK = 1,
  k_ESteamNetworkingGetConfigValue_OKInherited = 2, // A value was not set at this level, but the effective (inherited) value was returned.

  k_ESteamNetworkingGetConfigValueResult__Force32Bit = $7fffffff
);
{$pop}
//
// Debug output
//

/// Detail level for diagnostic output callback.
/// See ISteamNetworkingUtils::SetDebugOutputFunction
type ESteamNetworkingSocketsDebugOutputType = (
  k_ESteamNetworkingSocketsDebugOutputType_None = 0,
  k_ESteamNetworkingSocketsDebugOutputType_Bug = 1, // You used the API incorrectly, or an internal error happened
  k_ESteamNetworkingSocketsDebugOutputType_Error = 2, // Run-time error condition that isn't the result of a bug.  (E.g. we are offline, cannot bind a port, etc)
  k_ESteamNetworkingSocketsDebugOutputType_Important = 3, // Nothing is wrong, but this is an important notification
  k_ESteamNetworkingSocketsDebugOutputType_Warning = 4,
  k_ESteamNetworkingSocketsDebugOutputType_Msg = 5, // Recommended amount
  k_ESteamNetworkingSocketsDebugOutputType_Verbose = 6, // Quite a bit
  k_ESteamNetworkingSocketsDebugOutputType_Debug = 7, // Practically everything
  k_ESteamNetworkingSocketsDebugOutputType_Everything = 8, // Wall of text, detailed packet contents breakdown, etc

  k_ESteamNetworkingSocketsDebugOutputType__Force32Bit = $7fffffff
);


/// This callback is posted whenever a connection is created, destroyed, or changes state.
/// The m_info field will contain a complete description of the connection at the time the
/// change occurred and the callback was posted.  In particular, m_eState will have the
/// new connection state.
///
/// You will usually need to listen for this callback to know when:
/// - A new connection arrives on a listen socket.
///   m_info.m_hListenSocket will be set, m_eOldState = k_ESteamNetworkingConnectionState_None,
///   and m_info.m_eState = k_ESteamNetworkingConnectionState_Connecting.
///   See ISteamNetworkigSockets::AcceptConnection.
/// - A connection you initiated has been accepted by the remote host.
///   m_eOldState = k_ESteamNetworkingConnectionState_Connecting, and
///   m_info.m_eState = k_ESteamNetworkingConnectionState_Connected.
///   Some connections might transition to k_ESteamNetworkingConnectionState_FindingRoute first.
/// - A connection has been actively rejected or closed by the remote host.
///   m_eOldState = k_ESteamNetworkingConnectionState_Connecting or k_ESteamNetworkingConnectionState_Connected,
///   and m_info.m_eState = k_ESteamNetworkingConnectionState_ClosedByPeer.  m_info.m_eEndReason
///   and m_info.m_szEndDebug will have for more details.
///   NOTE: upon receiving this callback, you must still destroy the connection using
///   ISteamNetworkingSockets::CloseConnection to free up local resources.  (The details
///   passed to the function are not used in this case, since the connection is already closed.)
/// - A problem was detected with the connection, and it has been closed by the local host.
///   The most common failure is timeout, but other configuration or authentication failures
///   can cause this.  m_eOldState = k_ESteamNetworkingConnectionState_Connecting or
///   k_ESteamNetworkingConnectionState_Connected, and m_info.m_eState = k_ESteamNetworkingConnectionState_ProblemDetectedLocally.
///   m_info.m_eEndReason and m_info.m_szEndDebug will have for more details.
///   NOTE: upon receiving this callback, you must still destroy the connection using
///   ISteamNetworkingSockets::CloseConnection to free up local resources.  (The details
///   passed to the function are not used in this case, since the connection is already closed.)
///
/// Remember that callbacks are posted to a queue, and networking connections can
/// change at any time.  It is possible that the connection has already changed
/// state by the time you process this callback.
///
/// Also note that callbacks will be posted when connections are created and destroyed by your own API calls.
type SteamNetConnectionStatusChangedCallback_t = record
  //enum { k_iCallback = k_iSteamNetworkingSocketsCallbacks + 1 };

  /// Connection handle
  m_hConn: HSteamNetConnection;

  /// Full connection info
  m_info: SteamNetConnectionInfo_t;

  /// Previous state.  (Current state is in m_info.m_eState)
  m_eOldState: ESteamNetworkingConnectionState;
end;

type PSteamNetConnectionStatusChangedCallback_t = ^SteamNetConnectionStatusChangedCallback_t;


/// A struct used to describe our readiness to participate in authenticated,
/// encrypted communication.  In order to do this we need:
///
/// - The list of trusted CA certificates that might be relevant for this
///   app.
/// - A valid certificate issued by a CA.
///
/// This callback is posted whenever the state of our readiness changes.
type SteamNetAuthenticationStatus_t = record
  //enum { k_iCallback = k_iSteamNetworkingSocketsCallbacks + 2 };

  /// Status
  m_eAvail: ESteamNetworkingAvailability;

  /// Non-localized English language status.  For diagnostic/debugging
  /// purposes only.
  m_debugMsg: array[0..255] of Char;
end;

type PSteamNetAuthenticationStatus_t = ^SteamNetAuthenticationStatus_t;

type FSteamNetworkingSocketsDebugOutput = procedure (nType: ESteamNetworkingSocketsDebugOutputType; pszMsg: PChar); cdecl;

type FnSteamNetConnectionStatusChanged = procedure(pInfo: PSteamNetConnectionStatusChangedCallback_t); cdecl;
type FnSteamNetAuthenticationStatusChanged = procedure(pInfo: PSteamNetAuthenticationStatus_t); cdecl;
type FnSteamRelayNetworkStatusChanged = procedure(pInfo: PSteamRelayNetworkStatus_t); cdecl;
type FnSteamNetworkingMessagesSessionRequest = procedure(pInfo: PSteamNetworkingMessagesSessionRequest_t); cdecl;
type FnSteamNetworkingMessagesSessionFailed = procedure(pInfo: PSteamRelayNetworkStatus_t); cdecl;

{$IFNDEF STEAM}
// Initialize the library.  Optionally, you can set an initial identity for the default
// interface that is returned by SteamNetworkingSockets().
//
// On failure, false is returned, and a non-localized diagtnostic message is returned.
function GameNetworkingSockets_Init(const pIdentity: PSteamNetworkingIdentity; errMsg: PSteamNetworkingErrMsg): Boolean; cdecl; external GNSLIB;

// Close all connections and listen sockets and free all resources
procedure GameNetworkingSockets_Kill(); cdecl; external GNSLIB;
function SteamNetworkingSockets(): ISteamNetworkingSockets; cdecl; external GNSLIB;
function SteamNetworkingUtils(): ISteamNetworkingUtils; cdecl; external GNSLIB;


function SteamAPI_SteamNetworkingSockets_v009(): ISteamNetworkingSockets; cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingUtils_v003(): ISteamNetworkingUtils; cdecl; external GNSLIB;

//
// Statistics about the global lock.
//
procedure SteamNetworkingSockets_SetLockWaitWarningThreshold(usecThreshold: SteamNetworkingMicroseconds); cdecl; external GNSLIB;
procedure SteamNetworkingSockets_SetLockAcquiredCallback(usecWaited: Pointer); cdecl; external GNSLIB;

procedure SteamNetworkingSockets_SetManualPollMode(bFlag: Boolean); cdecl; external GNSLIB;
procedure SteamNetworkingSockets_Poll(msMaxWaitTime: Integer); cdecl; external GNSLIB;

function SteamAPI_ISteamNetworkingSockets_CreateListenSocketIP(instancePtr: ISteamNetworkingSockets; const pAddress: PSteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t):HSteamListenSocket; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_ConnectByIPAddress(instancePtr: ISteamNetworkingSockets; const pAddress: PSteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_AcceptConnection(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection): EResult; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_CloseConnection(instancePtr: ISteamNetworkingSockets; hPeer: HSteamNetConnection; nReason: Integer; pszDebug: PChar; bEnableLinger: Boolean): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_CloseListenSocket(instancePtr: ISteamNetworkingSockets; hSocket: HSteamListenSocket): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_SetConnectionUserData(instancePtr: ISteamNetworkingSockets; hPeer: HSteamNetConnection; nUserData: int64): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetConnectionUserData(instancePtr: ISteamNetworkingSockets; hPeer: HSteamNetConnection): Int64; cdecl; external GNSLIB;
procedure SteamAPI_ISteamNetworkingSockets_SetConnectionName(instancePtr: ISteamNetworkingSockets; hPeer: HSteamNetConnection; pszName: PChar); cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetConnectionName(instancePtr: ISteamNetworkingSockets; hPeer: HSteamNetConnection; pszName: PChar; nMaxLen: Integer): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_SendMessageToConnection(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection; pData: Pointer; cbData: uint32; nSendFlags: Integer; pOutMessageNumber: PInt64): EResult; cdecl; external GNSLIB;
procedure SteamAPI_ISteamNetworkingSockets_SendMessages(instancePtr: ISteamNetworkingSockets; nMessages: Integer; const pMessages: PSteamNetworkingMessage_t; pOutMessageNumberOrResult: PInt64); cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_FlushMessagesOnConnection(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection): EResult; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection; ppOutMessages: PSteamNetworkingMessage_t; nMaxMessages: Integer): Integer; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetConnectionInfo(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection; pInfo: PSteamNetConnectionInfo_t): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetQuickConnectionStatus(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection; pStats: PSteamNetworkingQuickConnectionStatus): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetDetailedConnectionStatus(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection; pszBuf: PChar; cbBuf: Integer): Integer; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetListenSocketAddress(instancePtr: ISteamNetworkingSockets; hSocket: HSteamListenSocket; pAddress: PSteamNetworkingIPAddr): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_CreateSocketPair(instancePtr: ISteamNetworkingSockets; pOutConnection1: PHSteamNetConnection; pOutConnection2: PHSteamNetConnection; bUseNetworkLoopback: Boolean; const pIdentity1: PSteamNetworkingIdentity; const pIdentity2: PSteamNetworkingIdentity): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetIdentity(instancePtr: ISteamNetworkingSockets; pIdentity: PSteamNetworkingIdentity): Boolean; cdecl; external GNSLIB;

function SteamAPI_ISteamNetworkingSockets_CreatePollGroup(instancePtr: ISteamNetworkingSockets): HSteamNetPollGroup; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_DestroyPollGroup(instancePtr: ISteamNetworkingSockets; hPollGroup: HSteamNetPollGroup): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup(instancePtr: ISteamNetworkingSockets; hConn: HSteamNetConnection; hPollGroup: HSteamNetPollGroup): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup(instancePtr: ISteamNetworkingSockets; hPollGroup: HSteamNetPollGroup; ppOutMessages: PSteamNetworkingMessage_t; nMaxMessages: Integer): Integer; cdecl; external GNSLIB;

//function SteamAPI_ISteamNetworkingSockets_GetCertificateRequest(instancePtr: ISteamNetworkingSockets): Boolean; cdecl; external GNSLIB;
//function SteamAPI_ISteamNetworkingSockets_SetCertificate(instancePtr: ISteamNetworkingSockets; const pCertificate: Pointer; cbCertificate: Integer; errMsg: PSteamNetworkingErrMsg): Boolean; cdecl; external GNSLIB;

function SteamAPI_ISteamNetworkingSockets_InitAuthentication(instancePtr: ISteamNetworkingSockets): ESteamNetworkingAvailability; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingSockets_GetAuthenticationStatus(instancePtr: ISteamNetworkingSockets; pDetails: PSteamNetAuthenticationStatus_t): ESteamNetworkingAvailability; cdecl; external GNSLIB;
procedure SteamAPI_ISteamNetworkingSockets_RunCallbacks(instancePtr: ISteamNetworkingSockets); cdecl; external GNSLIB;


procedure SteamAPI_SteamNetworkingIPAddr_Clear(pThis: PSteamNetworkingIPAddr); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIPAddr_IsIPv6AllZeros(const pThis: PSteamNetworkingIPAddr): Boolean; cdecl; external GNSLIB;
procedure SteamAPI_SteamNetworkingIPAddr_SetIPv6(pThis: PSteamNetworkingIPAddr; const ipv6: puint8; nPort: uint16); cdecl; external GNSLIB;
procedure SteamAPI_SteamNetworkingIPAddr_SetIPv4(pThis: PSteamNetworkingIPAddr; nIP: uint32; nPort: uint16); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIPAddr_IsIPv4(const pThis: PSteamNetworkingIPAddr): Boolean; cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIPAddr_GetIPv4(const pThis: PSteamNetworkingIPAddr): uint32; cdecl; external GNSLIB;
procedure SteamAPI_SteamNetworkingIPAddr_SetIPv6LocalHost(pThis: PSteamNetworkingIPAddr; nPort: uint16); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIPAddr_IsLocalHost(const pThis: PSteamNetworkingIPAddr): Boolean; cdecl; external GNSLIB;
// Note: in steamnetworkingtypes.h:
// SteamAPI_SteamNetworkingIPAddr_ToString
// SteamAPI_SteamNetworkingIPAddr_ParseString
procedure SteamAPI_SteamNetworkingIPAddr_ToString(const pAddr: PSteamNetworkingIPAddr; buf: PChar; cbBuf: csize_t; bWithPort: Boolean); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIPAddr_ParseString(pAddr: PSteamNetworkingIPAddr; pszStr: PChar): Boolean; cdecl; external GNSLIB;


procedure SteamAPI_SteamNetworkingIdentity_Clear(pThis: PSteamNetworkingIdentity); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_IsInvalid(const pThis: PSteamNetworkingIdentity): Boolean; cdecl; external GNSLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetSteamID64(pThis: PSteamNetworkingIdentity; steamID: uint64); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_GetSteamID64(const pThis: PSteamNetworkingIdentity): uint64; cdecl; external GNSLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetIPAddr(pThis: PSteamNetworkingIdentity; const pAddr: PSteamNetworkingIPAddr); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_GetIPAddr(pThis: PSteamNetworkingIdentity): PSteamNetworkingIPAddr; cdecl; external GNSLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetLocalHost(pThis: PSteamNetworkingIdentity); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_IsLocalHost(const pThis: PSteamNetworkingIdentity): Boolean; cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_SetGenericString(pThis: PSteamNetworkingIdentity; pszString: PChar): Boolean; cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_GetGenericString(const pThis: PSteamNetworkingIdentity): PChar; cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_SetGenericBytes(pThis: PSteamNetworkingIdentity; const data: Pointer; cbLen: csize_t): Boolean; cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_GetGenericBytes(const pThis: PSteamNetworkingIdentity; pOutLen: PInteger): puint8; cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_IsEqualTo(const a: PSteamNetworkingIdentity; const b: PSteamNetworkingIdentity): Boolean; cdecl; external GNSLIB;
// Note: in steamnetworkingtypes.h
// SteamAPI_SteamNetworkingIdentity_ToString
// SteamAPI_SteamNetworkingIdentity_ParseString
procedure SteamAPI_SteamNetworkingIdentity_ToString(const identity: PSteamNetworkingIdentity; buf: PChar;cbBuf: csize_t); cdecl; external GNSLIB;
function SteamAPI_SteamNetworkingIdentity_ParseString(pIdentity: PSteamNetworkingIdentity; pszStr: PChar): Boolean; cdecl; external GNSLIB;



//
// ISteamNetworkingUtils
//
function SteamAPI_ISteamNetworkingUtils_AllocateMessage(instancePtr: ISteamNetworkingUtils; cbAllocateBuffer: Integer): PSteamNetworkingMessage_t; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_GetLocalTimestamp(instancePtr: ISteamNetworkingUtils): SteamNetworkingMicroseconds; cdecl; external GNSLIB;

function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueInt32(instancePtr: ISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; val: Integer): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueFloat(instancePtr: ISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; val: Single): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueString(instancePtr: ISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; val: PChar): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValuePtr(instancePtr: ISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; val: Pointer): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueInt32(instancePtr: ISteamNetworkingUtils; hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: Integer): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueFloat(instancePtr: ISteamNetworkingUtils; hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: Single): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueString(instancePtr: ISteamNetworkingUtils; hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: PChar): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetConnectionStatusChanged(instancePtr: ISteamNetworkingUtils; fnCallback: FnSteamNetConnectionStatusChanged): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetAuthenticationStatusChanged(instancePtr: ISteamNetworkingUtils; fnCallback: FnSteamNetAuthenticationStatusChanged): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamRelayNetworkStatusChanged(instancePtr: ISteamNetworkingUtils; fnCallback: FnSteamRelayNetworkStatusChanged): Boolean; cdecl; external GNSLIB;

procedure SteamAPI_ISteamNetworkingUtils_SetDebugOutputFunction(instancePtr: ISteamNetworkingUtils; eDetailLevel: ESteamNetworkingSocketsDebugOutputType; pfnFunc: FSteamNetworkingSocketsDebugOutput); cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_SetConfigValue(instancePtr: ISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: intptr;
  eDataType: ESteamNetworkingConfigDataType; const pValue: Pointer): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_GetConfigValue(instancePtr: ISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: intptr;
  pOutDataType: PESteamNetworkingConfigDataType; var pResult; cbResult: pcsize_t): ESteamNetworkingGetConfigValueResult; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_GetConfigValueInfo(instancePtr: ISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; const pOutName: PPAnsiChar; pOutDataType: PESteamNetworkingConfigDataType; pOutScope: PESteamNetworkingConfigScope; pOutNextValue: PESteamNetworkingConfigValue): Boolean; cdecl; external GNSLIB;
function SteamAPI_ISteamNetworkingUtils_GetFirstConfigValue(instancePtr: ISteamNetworkingUtils): ESteamNetworkingConfigValue; cdecl; external GNSLIB;

procedure SteamAPI_SteamNetworkingMessage_t_Release(pIMsg: PSteamNetworkingMessage_t); cdecl; external GNSLIB;
{$ENDIF}

type
  TSteamNetworkingSockets = class
  public
    GameNetworkingSocketsInterface: ISteamNetworkingSockets;
    constructor Init();
    destructor Destroy; override;
    // Sockets
    function CreateListenSocketIP(const pAddress: PSteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t):HSteamListenSocket;
    function ConnectByIPAddress(const pAddress: PSteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
    {$IFDEF STEAM}
    //function CreateListenSocketP2P(nVirtualPort: Integer): HSteamListenSocket;
    //function ConnectP2P(const SteamNetworkingIdentity: PSteamNetworkingIdentity; nVirtualPort: Integer): HSteamNetConnection;
    {$ENDIF}
    function AcceptConnection(hConn: HSteamNetConnection): EResult;
    function CloseConnection(hPeer: HSteamNetConnection; nReason: Integer; pszDebug: PChar; bEnableLinger: Boolean): Boolean;
    function CloseListenSocket(hSocket: HSteamListenSocket): Boolean;
    function SetConnectionUserData(hPeer: HSteamNetConnection; nUserData: int64): Boolean;
    function GetConnectionUserData(hPeer: HSteamNetConnection): Int64;
    procedure SetConnectionName(hPeer: HSteamNetConnection; pszName: PChar);
    function GetConnectionName(hPeer: HSteamNetConnection; pszName: PChar; nMaxLen: Integer): Boolean;
    function SendMessageToConnection(hConn: HSteamNetConnection; pData: Pointer; cbData: uint32; nSendFlags: Integer; pOutMessageNumber: PInt64): EResult;
    procedure SendMessages(nMessages: Integer; const pMessages: PSteamNetworkingMessage_t; pOutMessageNumberOrResult: PInt64);
    function FlushMessagesOnConnection(hConn: HSteamNetConnection): EResult;
    function ReceiveMessagesOnConnection(hConn: HSteamNetConnection; ppOutMessages: PSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
    function GetConnectionInfo(hConn: HSteamNetConnection; pInfo: PSteamNetConnectionInfo_t): Boolean;
    function GetQuickConnectionStatus(hConn: HSteamNetConnection; pStats: PSteamNetworkingQuickConnectionStatus): Boolean;
    function GetDetailedConnectionStatus(hConn: HSteamNetConnection; pszBuf: PChar; cbBuf: Integer): Integer;
    function GetListenSocketAddress(hSocket: HSteamListenSocket; pAddress: PSteamNetworkingIPAddr): Boolean;
    function CreateSocketPair(pOutConnection1: PHSteamNetConnection; pOutConnection2: PHSteamNetConnection; bUseNetworkLoopback: Boolean; const pIdentity1: PSteamNetworkingIdentity; const pIdentity2: PSteamNetworkingIdentity): Boolean;
    function GetIdentity(pIdentity: PSteamNetworkingIdentity): Boolean;

    function InitAuthentication(): ESteamNetworkingAvailability;
    function GetAuthenticationStatus(pDetails: PSteamNetAuthenticationStatus_t): ESteamNetworkingAvailability;

    function CreatePollGroup(): HSteamNetPollGroup;
    function DestroyPollGroup(hPollGroup: HSteamNetPollGroup): Boolean;
    function SetConnectionPollGroup(hConn: HSteamNetConnection; hPollGroup: HSteamNetPollGroup): Boolean;
    function ReceiveMessagesOnPollGroup(hPollGroup: HSteamNetPollGroup; ppOutMessages: PSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;

    {$IFNDEF STEAM}
    procedure RunCallbacks();
    {$ENDIF}

    {$IFDEF STEAM}
    //function ReceivedRelayAuthTicket(const pvTicket: Pointer; cbTicket: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Boolean;
    //function FindRelayAuthTicketForServer(const identityGameServer: PSteamNetworkingIdentity; nVirtualPort: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Integer;
    //function ConnectToHostedDedicatedServer(const identityTarget: PSteamNetworkingIdentity; nVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
    //function GetHostedDedicatedServerPort(): uint16;
    //function GetHostedDedicatedServerPOPID(): SteamNetworkingPOPID;
    //function GetHostedDedicatedServerAddress(pRouting: PSteamDatagramHostedAddress): EResult;
    //function CreateHostedDedicatedServerListenSocket(nVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
    //function GetGameCoordinatorServerLogin(pLoginInfo: PSteamDatagramGameCoordinatorServerLogin; pcbSignedBlob: PInteger; pBlob: Pointer): EResult;
    {$ENDIF}
  end;

  TSteamNetworkingUtils = class
  public
    GameNetworkingUtilsInterface: ISteamNetworkingUtils;
    constructor Init();
    destructor Destroy; override;
    function AllocateMessage(cbAllocateBuffer: Integer): PSteamNetworkingMessage_t;
    {$IFDEF STEAM}
    //procedure InitRelayNetworkAccess(); cdecl;
    //function GetRelayNetworkStatus(pDetails: PSteamRelayNetworkStatus_t): ESteamNetworkingAvailability;cdecl;
    //function GetLocalPingLocation(result_: PSteamNetworkPingLocation_t): Single;cdecl;
    //function EstimatePingTimeBetweenTwoLocations(const location1: PSteamNetworkPingLocation_t; const location2: PSteamNetworkPingLocation_t): Integer;cdecl;
    //function EstimatePingTimeFromLocalHost(remoteLocation: PSteamNetworkPingLocation_t): Integer;cdecl;
    //procedure ConvertPingLocationToString(location: PSteamNetworkPingLocation_t; pszBuf: PChar; cchBufSize: Integer);cdecl;
    //function ParsePingLocationString(pszString: PChar; result_: PSteamNetworkPingLocation_t): Boolean;cdecl;
    //function CheckPingDataUpToDate(flMaxAgeSeconds: Single): Boolean;cdecl;
    //function GetPingToDataCenter(popID: SteamNetworkingPOPID; pViaRelayPoP: PSteamNetworkingPOPID): Integer;cdecl;
    //function GetDirectPingToPOP(popID: SteamNetworkingPOPID): Integer;cdecl;
    //function GetPOPCount(): Integer;cdecl;
    //function GetPOPList(List: PSteamNetworkingPOPID; nListSz: Integer): Integer;cdecl;
    {$ENDIF}

    function GetLocalTimestamp(): SteamNetworkingMicroseconds;cdecl;
    procedure SetDebugOutputFunction(eDetailLevel: ESteamNetworkingSocketsDebugOutputType; pfnFunc: FSteamNetworkingSocketsDebugOutput); cdecl;

    function SetGlobalConfigValueInt32(eValue: ESteamNetworkingConfigValue; val: int32): Boolean;cdecl;
    function SetGlobalConfigValueFloat(eValue: ESteamNetworkingConfigValue; val: Single): Boolean;cdecl;
    function SetGlobalConfigValueString(eValue: ESteamNetworkingConfigValue; val: PChar): Boolean;cdecl;
    function SetGlobalConfigValuePtr(eValue: ESteamNetworkingConfigValue; val: Pointer): Boolean; cdecl;
    function SetConnectionConfigValueInt32(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;cdecl;
    function SetConnectionConfigValueFloat(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: Single): Boolean;cdecl;
    function SetConnectionConfigValueString(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: PChar): Boolean;cdecl;
    function SetGlobalCallback_SteamNetConnectionStatusChanged(fnCallback: FnSteamNetConnectionStatusChanged): Boolean;
    function SetGlobalCallback_SteamNetAuthenticationStatusChanged(fnCallback: FnSteamNetAuthenticationStatusChanged): Boolean;
    function SetGlobalCallback_SteamRelayNetworkStatusChanged(fnCallback: FnSteamRelayNetworkStatusChanged): Boolean;

    function SetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: intptr;
      eDataType: ESteamNetworkingConfigDataType; const pValue: Pointer): Boolean;

    {$IFDEF STEAM2}
    function SetConfigValueStruct(opt: PSteamNetworkingConfigValue_t; eScopeType: ESteamNetworkingConfigScope; scopeObj: intptr): Boolean;
    {$ENDIF}
    function GetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: intptr;
      pOutDataType: PESteamNetworkingConfigDataType; var pResult; cbResult: pcsize_t): ESteamNetworkingGetConfigValueResult;
    function GetConfigValueInfo(eValue: ESteamNetworkingConfigValue; const pOutName: PPAnsiChar; pOutDataType: PESteamNetworkingConfigDataType; pOutScope: PESteamNetworkingConfigScope; pOutNextValue: PESteamNetworkingConfigValue): Boolean;
    function GetFirstConfigValue(): ESteamNetworkingConfigValue;
    {$IFDEF STEAM}
    //procedure SteamNetworkingIPAddr_ToString(pAddr: PSteamNetworkingIPAddr; buf: PChar; cbBuf: csize_t; bWithPort: Boolean); cdecl;
    //function SteamNetworkingIPAddr_ParseString(pAddr: PSteamNetworkingIPAddr; pszStr: PChar): Boolean;
    //procedure SteamNetworkingIdentity_ToString(const pIdentity: PSteamNetworkingIdentity; buf: PChar;cbBuf: csize_t);
    //function SteamNetworkingIdentity_ParseString(pIdentity: PSteamNetworkingIdentity; sizeofIdentity: csize_t; pszStr: PChar): Boolean;
    {$ENDIF}
  end;

  type
  TSteamNetworkingIPAddrHelper = record helper for SteamNetworkingIPAddr
    procedure Clear();
    function IsIPv6AllZeros(): Boolean;
    procedure SetIPv6(const ipv6: puint8; nPort: uint16);
    procedure SetIPv4(nIP: uint32; nPort: uint16);
    function IsIPv4(): Boolean;
    function GetIPv4(): uint32;
    procedure SetIPv6LocalHost(nPort: uint16);
    function IsLocalHost(): Boolean;
    procedure ToString(buf: PChar; cbBuf: csize_t; bWithPort: Boolean);
    function ParseString(pszStr: PChar): Boolean;
  end;

  type
  TSteamNetworkingIdentityHelper = record helper for SteamNetworkingIdentity
    procedure Clear();
    function IsInvalid(): Boolean;
    procedure SetSteamID64(steamID: uint64);
    function GetSteamID64(): uint64;
    procedure SetIPAddr(const pAddr: PSteamNetworkingIPAddr);
    function GetIPAddr(): PSteamNetworkingIPAddr;
    procedure SetLocalHost();
    function IsLocalHost(): Boolean;
    function SetGenericString(pszString: PChar): Boolean;
    function GetGenericString(): PChar;
    function SetGenericBytes(const data: Pointer; cbLen: csize_t): Boolean;
    function GetGenericBytes(pOutLen: PInteger): puint8;
    function EqualTo(const b: PSteamNetworkingIdentity): Boolean;
    procedure ToString(buf: PChar; cbBuf: csize_t);
    function ParseString(pszStr: PChar): Boolean;
  end;

implementation
{$IFDEF STEAM}
 uses Steam;
{$ENDIF}

constructor TSteamNetworkingSockets.Init();
{$IFNDEF STEAM}
var
  ErrorMsg: SteamNetworkingErrMsg;
{$ENDIF}
begin
  {$IFNDEF STEAM}
  if not GameNetworkingSockets_Init(nil, @ErrorMsg) then
    raise Exception.Create('GameNetworkingSockets_Init has failed: ' + PChar(ErrorMsg));
  {$ENDIF}

  GameNetworkingSocketsInterface := SteamAPI_SteamNetworkingSockets_v009();

  if GameNetworkingSocketsInterface = nil then
    raise Exception.Create('GameNetworkingSocketsInterface is null');
end;

destructor TSteamNetworkingSockets.Destroy;
begin
  {$IFNDEF STEAM}
  GameNetworkingSockets_Kill();
  {$ENDIF}
  inherited;
end;
// Sockets
function TSteamNetworkingSockets.CreateListenSocketIP(const pAddress: PSteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t):HSteamListenSocket;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateListenSocketIP(GameNetworkingSocketsInterface, pAddress, nOptions, pOptions);
end;
function TSteamNetworkingSockets.ConnectByIPAddress(const pAddress: PSteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ConnectByIPAddress(GameNetworkingSocketsInterface, pAddress, nOptions, pOptions);
end;
function TSteamNetworkingSockets.AcceptConnection(hConn: HSteamNetConnection): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_AcceptConnection(GameNetworkingSocketsInterface, hConn);
end;
function TSteamNetworkingSockets.CloseConnection(hPeer: HSteamNetConnection; nReason: Integer; pszDebug: PChar; bEnableLinger: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CloseConnection(GameNetworkingSocketsInterface, hPeer, nReason, pszDebug, bEnableLinger);
end;
function TSteamNetworkingSockets.CloseListenSocket(hSocket: HSteamListenSocket): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CloseListenSocket(GameNetworkingSocketsInterface, hSocket);
end;
function TSteamNetworkingSockets.SetConnectionUserData(hPeer: HSteamNetConnection; nUserData: int64): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_SetConnectionUserData(GameNetworkingSocketsInterface, hPeer, nUserData);
end;
function TSteamNetworkingSockets.GetConnectionUserData(hPeer: HSteamNetConnection): Int64;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetConnectionUserData(GameNetworkingSocketsInterface, hPeer);
end;
procedure TSteamNetworkingSockets.SetConnectionName(hPeer: HSteamNetConnection; pszName: PChar);
begin
  SteamAPI_ISteamNetworkingSockets_SetConnectionName(GameNetworkingSocketsInterface, hPeer, pszName);
end;
function TSteamNetworkingSockets.GetConnectionName(hPeer: HSteamNetConnection; pszName: PChar; nMaxLen: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetConnectionName(GameNetworkingSocketsInterface, hPeer, pszName, nMaxLen);
end;
function TSteamNetworkingSockets.SendMessageToConnection(hConn: HSteamNetConnection; pData: Pointer; cbData: uint32; nSendFlags: Integer; pOutMessageNumber: PInt64): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_SendMessageToConnection(GameNetworkingSocketsInterface, hConn, pData, cbData, nSendFlags, pOutMessageNumber);
end;
procedure TSteamNetworkingSockets.SendMessages(nMessages: Integer; const pMessages: PSteamNetworkingMessage_t; pOutMessageNumberOrResult: PInt64);
begin
  SteamAPI_ISteamNetworkingSockets_SendMessages(GameNetworkingSocketsInterface, nMessages, pMessages, pOutMessageNumberOrResult);
end;
function TSteamNetworkingSockets.FlushMessagesOnConnection(hConn: HSteamNetConnection): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_FlushMessagesOnConnection(GameNetworkingSocketsInterface, hConn);
end;
function TSteamNetworkingSockets.ReceiveMessagesOnConnection(hConn: HSteamNetConnection; ppOutMessages: PSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection(GameNetworkingSocketsInterface, hConn, ppOutMessages, nMaxMessages);
end;
function TSteamNetworkingSockets.GetConnectionInfo(hConn: HSteamNetConnection; pInfo: PSteamNetConnectionInfo_t): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetConnectionInfo(GameNetworkingSocketsInterface, hConn, pInfo);
end;
function TSteamNetworkingSockets.GetQuickConnectionStatus(hConn: HSteamNetConnection; pStats: PSteamNetworkingQuickConnectionStatus): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetQuickConnectionStatus(GameNetworkingSocketsInterface, hConn, pStats);
end;
function TSteamNetworkingSockets.GetDetailedConnectionStatus(hConn: HSteamNetConnection; pszBuf: PChar; cbBuf: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetDetailedConnectionStatus(GameNetworkingSocketsInterface, hConn, pszBuf, cbBuf);
end;
function TSteamNetworkingSockets.GetListenSocketAddress(hSocket: HSteamListenSocket; pAddress: PSteamNetworkingIPAddr): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetListenSocketAddress(GameNetworkingSocketsInterface, hSocket, pAddress);
end;
function TSteamNetworkingSockets.CreateSocketPair(pOutConnection1: PHSteamNetConnection; pOutConnection2: PHSteamNetConnection; bUseNetworkLoopback: Boolean; const pIdentity1: PSteamNetworkingIdentity; const pIdentity2: PSteamNetworkingIdentity): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateSocketPair(GameNetworkingSocketsInterface, pOutConnection1, pOutConnection2, bUseNetworkLoopback, pIdentity1, pIdentity2);
end;
function TSteamNetworkingSockets.GetIdentity(pIdentity: PSteamNetworkingIdentity): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetIdentity(GameNetworkingSocketsInterface, pIdentity);
end;
function TSteamNetworkingSockets.CreatePollGroup(): HSteamNetPollGroup;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreatePollGroup(GameNetworkingSocketsInterface);
end;
function TSteamNetworkingSockets.DestroyPollGroup(hPollGroup: HSteamNetPollGroup): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_DestroyPollGroup(GameNetworkingSocketsInterface, hPollGroup);
end;
function TSteamNetworkingSockets.SetConnectionPollGroup(hConn: HSteamNetConnection; hPollGroup: HSteamNetPollGroup): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup(GameNetworkingSocketsInterface, hConn, hPollGroup);
end;
function TSteamNetworkingSockets.ReceiveMessagesOnPollGroup(hPollGroup: HSteamNetPollGroup; ppOutMessages: PSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup(GameNetworkingSocketsInterface, hPollGroup, ppOutMessages, nMaxMessages);
end;
function TSteamNetworkingSockets.InitAuthentication(): ESteamNetworkingAvailability;
begin
  Result := SteamAPI_ISteamNetworkingSockets_InitAuthentication(GameNetworkingSocketsInterface);
end;
function TSteamNetworkingSockets.GetAuthenticationStatus(pDetails: PSteamNetAuthenticationStatus_t): ESteamNetworkingAvailability;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetAuthenticationStatus(GameNetworkingSocketsInterface, pDetails);
end;
{$IFNDEF STEAM}
procedure TSteamNetworkingSockets.RunCallbacks();
begin
  SteamAPI_ISteamNetworkingSockets_RunCallbacks(GameNetworkingSocketsInterface);
end;
{$ENDIF}

// Utils
constructor TSteamNetworkingUtils.Init();
begin
  GameNetworkingUtilsInterface := SteamAPI_SteamNetworkingUtils_v003();
  if GameNetworkingUtilsInterface = nil then
    raise Exception.Create('GameNetworkingSocketsInterface is null');
end;

destructor TSteamNetworkingUtils.Destroy;
begin
  inherited;
end;
function TSteamNetworkingUtils.AllocateMessage(cbAllocateBuffer: Integer): PSteamNetworkingMessage_t;
begin
  Result := SteamAPI_ISteamNetworkingUtils_AllocateMessage(GameNetworkingUtilsInterface, cbAllocateBuffer);
end;
function TSteamNetworkingUtils.GetLocalTimestamp(): SteamNetworkingMicroseconds;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetLocalTimestamp(GameNetworkingUtilsInterface);
end;
procedure TSteamNetworkingUtils.SetDebugOutputFunction(eDetailLevel: ESteamNetworkingSocketsDebugOutputType; pfnFunc: FSteamNetworkingSocketsDebugOutput);
begin
  SteamAPI_ISteamNetworkingUtils_SetDebugOutputFunction(GameNetworkingUtilsInterface, eDetailLevel, pfnFunc);
end;
function TSteamNetworkingUtils.SetGlobalConfigValueInt32(eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueInt32(GameNetworkingUtilsInterface, eValue, val);
end;
function TSteamNetworkingUtils.SetGlobalConfigValueFloat(eValue: ESteamNetworkingConfigValue; val: Single): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueFloat(GameNetworkingUtilsInterface, eValue, val);
end;
function TSteamNetworkingUtils.SetGlobalConfigValueString(eValue: ESteamNetworkingConfigValue; val: PChar): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueString(GameNetworkingUtilsInterface, eValue, val);
end;
function TSteamNetworkingUtils.SetGlobalConfigValuePtr(eValue: ESteamNetworkingConfigValue; val: Pointer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValuePtr(GameNetworkingUtilsInterface, eValue, val);
end;
function TSteamNetworkingUtils.SetConnectionConfigValueInt32(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: int32): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueInt32(GameNetworkingUtilsInterface, hConn, eValue, val);
end;
function TSteamNetworkingUtils.SetConnectionConfigValueFloat(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: Single): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueFloat(GameNetworkingUtilsInterface, hConn, eValue, val);
end;
function TSteamNetworkingUtils.SetConnectionConfigValueString(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; val: PChar): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueString(GameNetworkingUtilsInterface, hConn, eValue, val);
end;
function TSteamNetworkingUtils.SetGlobalCallback_SteamNetConnectionStatusChanged(fnCallback: FnSteamNetConnectionStatusChanged): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetConnectionStatusChanged(GameNetworkingUtilsInterface, fnCallback);
end;
function TSteamNetworkingUtils.SetGlobalCallback_SteamNetAuthenticationStatusChanged(fnCallback: FnSteamNetAuthenticationStatusChanged): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetAuthenticationStatusChanged(GameNetworkingUtilsInterface, fnCallback);
end;
function TSteamNetworkingUtils.SetGlobalCallback_SteamRelayNetworkStatusChanged(fnCallback: FnSteamRelayNetworkStatusChanged): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamRelayNetworkStatusChanged(GameNetworkingUtilsInterface, fnCallback);
end;
function TSteamNetworkingUtils.SetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: intptr;
  eDataType: ESteamNetworkingConfigDataType; const pValue: Pointer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConfigValue(GameNetworkingUtilsInterface, eValue, eScopeType, scopeObj, eDataType, pValue);
end;
function TSteamNetworkingUtils.GetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: intptr;
  pOutDataType: PESteamNetworkingConfigDataType; var pResult; cbResult: pcsize_t): ESteamNetworkingGetConfigValueResult;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetConfigValue(GameNetworkingUtilsInterface, eValue, eScopeType, scopeObj, pOutDataType, pResult, cbResult);
end;
function TSteamNetworkingUtils.GetConfigValueInfo(eValue: ESteamNetworkingConfigValue; const pOutName: PPAnsiChar; pOutDataType: PESteamNetworkingConfigDataType; pOutScope: PESteamNetworkingConfigScope; pOutNextValue: PESteamNetworkingConfigValue): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetConfigValueInfo(GameNetworkingUtilsInterface, eValue, pOutName, pOutDataType, pOutScope, pOutNextValue);
end;
function TSteamNetworkingUtils.GetFirstConfigValue(): ESteamNetworkingConfigValue;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetFirstConfigValue(GameNetworkingUtilsInterface);
end;

procedure TSteamNetworkingIPAddrHelper.Clear();
begin
  SteamAPI_SteamNetworkingIPAddr_Clear(@Self);
end;
function TSteamNetworkingIPAddrHelper.IsIPv6AllZeros(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_IsIPv6AllZeros(@Self);
end;
procedure TSteamNetworkingIPAddrHelper.SetIPv6(const ipv6: puint8; nPort: uint16);
begin
  SteamAPI_SteamNetworkingIPAddr_SetIPv6(@Self, ipv6, nPort);
end;
procedure TSteamNetworkingIPAddrHelper.SetIPv4(nIP: uint32; nPort: uint16);
begin
  SteamAPI_SteamNetworkingIPAddr_SetIPv4(@Self, nIP, nPort);
end;
function TSteamNetworkingIPAddrHelper.IsIPv4(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_IsIPv4(@Self);
end;
function TSteamNetworkingIPAddrHelper.GetIPv4(): uint32;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_GetIPv4(@Self);
end;
procedure TSteamNetworkingIPAddrHelper.SetIPv6LocalHost(nPort: uint16);
begin
  SteamAPI_SteamNetworkingIPAddr_SetIPv6LocalHost(@Self, nPort);
end;
function TSteamNetworkingIPAddrHelper.IsLocalHost(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_IsLocalHost(@Self);
end;
procedure TSteamNetworkingIPAddrHelper.ToString(buf: PChar; cbBuf: csize_t; bWithPort: Boolean);
begin
  SteamAPI_SteamNetworkingIPAddr_ToString(@Self, buf, cbBuf, bWithPort);
end;
function TSteamNetworkingIPAddrHelper.ParseString(pszStr: PChar): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_ParseString(@Self, pszStr);
end;
procedure TSteamNetworkingIdentityHelper.Clear();
begin
  SteamAPI_SteamNetworkingIdentity_Clear(@Self);
end;
function TSteamNetworkingIdentityHelper.IsInvalid(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_IsInvalid(@Self);
end;
procedure TSteamNetworkingIdentityHelper.SetSteamID64(steamID: uint64);
begin
  SteamAPI_SteamNetworkingIdentity_SetSteamID64(@Self, steamID);
end;
function TSteamNetworkingIdentityHelper.GetSteamID64(): uint64;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetSteamID64(@Self);
end;
procedure TSteamNetworkingIdentityHelper.SetIPAddr(const pAddr: PSteamNetworkingIPAddr);
begin
  SteamAPI_SteamNetworkingIdentity_SetIPAddr(@Self, pAddr);
end;
function TSteamNetworkingIdentityHelper.GetIPAddr(): PSteamNetworkingIPAddr;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetIPAddr(@Self);
end;
procedure TSteamNetworkingIdentityHelper.SetLocalHost();
begin
  SteamAPI_SteamNetworkingIdentity_SetLocalHost(@Self);
end;
function TSteamNetworkingIdentityHelper.IsLocalHost(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_IsLocalHost(@Self);
end;
function TSteamNetworkingIdentityHelper.SetGenericString(pszString: PChar): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_SetGenericString(@Self, pszString);
end;
function TSteamNetworkingIdentityHelper.GetGenericString(): PChar;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetGenericString(@Self);
end;
function TSteamNetworkingIdentityHelper.SetGenericBytes(const data: Pointer; cbLen: csize_t): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_SetGenericBytes(@Self, data, cbLen);
end;
function TSteamNetworkingIdentityHelper.GetGenericBytes(pOutLen: PInteger): puint8;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetGenericBytes(@Self, pOutLen);
end;
function TSteamNetworkingIdentityHelper.EqualTo(const b: PSteamNetworkingIdentity): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_IsEqualTo(@Self, b);
end;
procedure TSteamNetworkingIdentityHelper.ToString(buf: PChar; cbBuf: csize_t);
begin
  SteamAPI_SteamNetworkingIdentity_ToString(@Self, buf, cbBuf);
end;
function TSteamNetworkingIdentityHelper.ParseString(pszStr: PChar): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_ParseString(@Self, pszStr);
end;

begin

end.
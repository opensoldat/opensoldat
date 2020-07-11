{*******************************************************}
{                                                       }
{       SteamTypes Unit for SOLDAT                      }
{                                                       }
{       Copyright (c) 2020 Soldat Team                  }
{                                                       }
{       For use with SteamWorks SDK 1.48a               }
{                                                       }
{*******************************************************}

{$IFDEF UNIX}
{$PACKRECORDS 4}
{$PACKENUM 4}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}
unit SteamTypes;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

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
  //TSteamID = uint64;
  uint64_gameid = uint64;
  GID_t = uint64;
  JobID_t = uint64;
  TxnID_t = GID_t;
  PackageId_t = uint32;
  BundleId_t = uint32;
  AppId_t = uint32;
  pAppId_t = puint32;
  AssetClassId_t = uint64;
  PhysicalItemId_t = uint32;
  DepotId_t = uint32;
  RTime32 = uint32;
  CellID_t = uint32;
  SteamAPICall_t = uint64;
  AccountID_t = uint32;
  PartnerId_t = uint32;
  ManifestId_t = uint64;
  SiteId_t = uint64;
  PartyBeaconID_t = uint64;
  HAuthTicket = uint32;
  HSteamPipe = int32;
  HSteamUser = int32;
  FriendsGroupID_t = int16;
  HServerQuery = integer;
  UGCHandle_t = uint64;
  PublishedFileUpdateHandle_t = uint64;
  PublishedFileId_t = uint64;
  UGCFileWriteStreamHandle_t = uint64;
  SteamLeaderboard_t = uint64;
  SteamLeaderboardEntries_t = uint64;
  SNetSocket_t = uint32;
  SNetListenSocket_t = uint32;
  ScreenshotHandle = uint32;
  HTTPRequestHandle = uint32;
  HTTPCookieContainerHandle = uint32;
  InputHandle_t = uint64;
  InputActionSetHandle_t = uint64;
  InputDigitalActionHandle_t = uint64;
  InputAnalogActionHandle_t = uint64;
  ControllerHandle_t = uint64;
  ControllerActionSetHandle_t = uint64;
  ControllerDigitalActionHandle_t = uint64;
  ControllerAnalogActionHandle_t = uint64;
  UGCQueryHandle_t = uint64;
  UGCUpdateHandle_t = uint64;
  HHTMLBrowser = uint32;
  SteamItemInstanceID_t = uint64;
  SteamItemDef_t = int32;
  SteamInventoryResult_t = int32;
  SteamInventoryUpdateHandle_t = uint64;
  HServerListRequest = Pointer;
  pDepotId_t = ^depotId_t;
  pSNetSocket_t = ^SNetSocket_t;
  pSteamAPICall_t = ^SteamAPICall_t;
  pInputHandle_t = ^InputHandle_t;
  pInputActionSetHandle_t = ^InputActionSetHandle_t;
  pControllerHandle_t = ^ControllerHandle_t;
  pControllerActionSetHandle_t = ^ControllerActionSetHandle_t;
  pControllerDigitalActionHandle_t = ^ControllerDigitalActionHandle_t;
  pPublishedFileId_t = ^PublishedFileId_t;
  pSteamInventoryResult_t = ^SteamInventoryResult_t;
  pSteamItemDef_t = ^SteamItemDef_t;
  pSteamItemInstanceID_t = ^SteamItemInstanceID_t;

  SteamDatagramRelayAuthTicket = record;
  PSteamDatagramRelayAuthTicket = ^SteamDatagramRelayAuthTicket;
  ISteamNetworkingConnectionCustomSignaling = Pointer;
  ISteamNetworkingCustomSignalingRecvContext = Pointer;
  SteamNetworkingErrMsg = array [0..1024 - 1] of Char;
  PSteamNetworkingErrMsg = ^SteamNetworkingErrMsg;


  ISteamClient = Pointer;
  ISteamUtils =  Pointer;
  ISteamUser = Pointer;
  ISteamFriends = Pointer;
  ISteamGameServer = Pointer;
  ISteamGameServerStats = Pointer;
  ISteamScreenshots = Pointer;
  ISteamUGC = Pointer;
  ISteamMatchmaking = Pointer;
  ISteamMatchmakingServers = Pointer;
  ISteamUserStats = Pointer;
  ISteamApps = Pointer;
  ISteamNetworking = Pointer;
  ISteamRemoteStorage = Pointer;
  ISteamGameSearch = Pointer;
  ISteamHTTP = Pointer;
  ISteamController = Pointer;
  ISteamAppList = Pointer;
  ISteamMusic = Pointer;
  ISteamMusicRemote = Pointer;
  ISteamHTMLSurface = Pointer;
  ISteamInventory = Pointer;
  ISteamVideo = Pointer;
  ISteamParentalSettings = Pointer;
  ISteamInput = Pointer;
  ISteamParties = Pointer;
  ISteamRemotePlay = Pointer;
  ISteamMatchmakingServerListResponse = Pointer;
  ISteamMatchmakingPingResponse = Pointer;
  ISteamMatchmakingPlayersResponse = Pointer;
  ISteamMatchmakingRulesResponse = Pointer;
  ISteamTV = Pointer;
  ISteamNetworkingSockets = Pointer;
  pISteamMatchmakingServerListResponse = Pointer;
  pISteamMatchmakingPingResponse = Pointer;
  pISteamMatchmakingPlayersResponse = Pointer;
  pISteamMatchmakingRulesResponse = Pointer;
  HSteamListenSocket = uint32;
  HSteamNetConnection = uint32;
  pHSteamNetConnection = ^HSteamNetConnection;
  SteamNetworkingPOPID = uint64;
  SteamNetworkingMicroseconds = uint64;
  RemotePlaySessionID_t = uint64;
  HSteamNetPollGroup = uint32;
  ISteamNetworkingUtils = Pointer;
  PSteamNetworkingPOPID = ^SteamNetworkingPOPID;

Type

ESteamIPType = (
  k_ESteamIPTypeIPv4 = 0,
  k_ESteamIPTypeIPv6 = 1
);
EUniverse = (
  k_EUniverseInvalid = 0,
  k_EUniversePublic = 1,
  k_EUniverseBeta = 2,
  k_EUniverseInternal = 3,
  k_EUniverseDev = 4,
  k_EUniverseMax = 5
);
{$Z4}
EResult = (
  k_EResultNone = 0,
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
  k_EResultCantRemoveItem = 113,
  k_EResultAccountDeleted = 114,
  k_EResultExistingUserCancelledLicense = 115
);
EVoiceResult = (
  k_EVoiceResultOK = 0,
  k_EVoiceResultNotInitialized = 1,
  k_EVoiceResultNotRecording = 2,
  k_EVoiceResultNoData = 3,
  k_EVoiceResultBufferTooSmall = 4,
  k_EVoiceResultDataCorrupted = 5,
  k_EVoiceResultRestricted = 6,
  k_EVoiceResultUnsupportedCodec = 7,
  k_EVoiceResultReceiverOutOfDate = 8,
  k_EVoiceResultReceiverDidNotAnswer = 9
);
EDenyReason = (
  k_EDenyInvalid = 0,
  k_EDenyInvalidVersion = 1,
  k_EDenyGeneric = 2,
  k_EDenyNotLoggedOn = 3,
  k_EDenyNoLicense = 4,
  k_EDenyCheater = 5,
  k_EDenyLoggedInElseWhere = 6,
  k_EDenyUnknownText = 7,
  k_EDenyIncompatibleAnticheat = 8,
  k_EDenyMemoryCorruption = 9,
  k_EDenyIncompatibleSoftware = 10,
  k_EDenySteamConnectionLost = 11,
  k_EDenySteamConnectionError = 12,
  k_EDenySteamResponseTimedOut = 13,
  k_EDenySteamValidationStalled = 14,
  k_EDenySteamOwnerLeftGuestUser = 15
);
EBeginAuthSessionResult = (
  k_EBeginAuthSessionResultOK = 0,
  k_EBeginAuthSessionResultInvalidTicket = 1,
  k_EBeginAuthSessionResultDuplicateRequest = 2,
  k_EBeginAuthSessionResultInvalidVersion = 3,
  k_EBeginAuthSessionResultGameMismatch = 4,
  k_EBeginAuthSessionResultExpiredTicket = 5
);
EAuthSessionResponse = (
  k_EAuthSessionResponseOK = 0,
  k_EAuthSessionResponseUserNotConnectedToSteam = 1,
  k_EAuthSessionResponseNoLicenseOrExpired = 2,
  k_EAuthSessionResponseVACBanned = 3,
  k_EAuthSessionResponseLoggedInElseWhere = 4,
  k_EAuthSessionResponseVACCheckTimedOut = 5,
  k_EAuthSessionResponseAuthTicketCanceled = 6,
  k_EAuthSessionResponseAuthTicketInvalidAlreadyUsed = 7,
  k_EAuthSessionResponseAuthTicketInvalid = 8,
  k_EAuthSessionResponsePublisherIssuedBan = 9
);
EUserHasLicenseForAppResult = (
  k_EUserHasLicenseResultHasLicense = 0,
  k_EUserHasLicenseResultDoesNotHaveLicense = 1,
  k_EUserHasLicenseResultNoAuth = 2
);
EAccountType = (
  k_EAccountTypeInvalid = 0,
  k_EAccountTypeIndividual = 1,
  k_EAccountTypeMultiseat = 2,
  k_EAccountTypeGameServer = 3,
  k_EAccountTypeAnonGameServer = 4,
  k_EAccountTypePending = 5,
  k_EAccountTypeContentServer = 6,
  k_EAccountTypeClan = 7,
  k_EAccountTypeChat = 8,
  k_EAccountTypeConsoleUser = 9,
  k_EAccountTypeAnonUser = 10,
  k_EAccountTypeMax = 11
);
EAppReleaseState = (
  k_EAppReleaseState_Unknown = 0,
  k_EAppReleaseState_Unavailable = 1,
  k_EAppReleaseState_Prerelease = 2,
  k_EAppReleaseState_PreloadOnly = 3,
  k_EAppReleaseState_Released = 4
);
EAppOwnershipFlags = (
  k_EAppOwnershipFlags_None = 0,
  k_EAppOwnershipFlags_OwnsLicense = 1,
  k_EAppOwnershipFlags_FreeLicense = 2,
  k_EAppOwnershipFlags_RegionRestricted = 4,
  k_EAppOwnershipFlags_LowViolence = 8,
  k_EAppOwnershipFlags_InvalidPlatform = 16,
  k_EAppOwnershipFlags_SharedLicense = 32,
  k_EAppOwnershipFlags_FreeWeekend = 64,
  k_EAppOwnershipFlags_RetailLicense = 128,
  k_EAppOwnershipFlags_LicenseLocked = 256,
  k_EAppOwnershipFlags_LicensePending = 512,
  k_EAppOwnershipFlags_LicenseExpired = 1024,
  k_EAppOwnershipFlags_LicensePermanent = 2048,
  k_EAppOwnershipFlags_LicenseRecurring = 4096,
  k_EAppOwnershipFlags_LicenseCanceled = 8192,
  k_EAppOwnershipFlags_AutoGrant = 16384,
  k_EAppOwnershipFlags_PendingGift = 32768,
  k_EAppOwnershipFlags_RentalNotActivated = 65536,
  k_EAppOwnershipFlags_Rental = 131072,
  k_EAppOwnershipFlags_SiteLicense = 262144,
  k_EAppOwnershipFlags_LegacyFreeSub = 524288,
  k_EAppOwnershipFlags_InvalidOSType = 1048576
);
EAppType = (
  k_EAppType_Invalid = 0,
  k_EAppType_Game = 1,
  k_EAppType_Application = 2,
  k_EAppType_Tool = 4,
  k_EAppType_Demo = 8,
  k_EAppType_Media_DEPRECATED = 16,
  k_EAppType_DLC = 32,
  k_EAppType_Guide = 64,
  k_EAppType_Driver = 128,
  k_EAppType_Config = 256,
  k_EAppType_Hardware = 512,
  k_EAppType_Franchise = 1024,
  k_EAppType_Video = 2048,
  k_EAppType_Plugin = 4096,
  k_EAppType_MusicAlbum = 8192,
  k_EAppType_Series = 16384,
  k_EAppType_Comic_UNUSED = 32768,
  k_EAppType_Beta = 65536,
  k_EAppType_Shortcut = 1073741824,
  k_EAppType_DepotOnly = -2147483648
);
ESteamUserStatType = (
  k_ESteamUserStatTypeINVALID = 0,
  k_ESteamUserStatTypeINT = 1,
  k_ESteamUserStatTypeFLOAT = 2,
  k_ESteamUserStatTypeAVGRATE = 3,
  k_ESteamUserStatTypeACHIEVEMENTS = 4,
  k_ESteamUserStatTypeGROUPACHIEVEMENTS = 5,
  k_ESteamUserStatTypeMAX = 6
);
EChatEntryType = (
  k_EChatEntryTypeInvalid = 0,
  k_EChatEntryTypeChatMsg = 1,
  k_EChatEntryTypeTyping = 2,
  k_EChatEntryTypeInviteGame = 3,
  k_EChatEntryTypeEmote = 4,
  k_EChatEntryTypeLeftConversation = 6,
  k_EChatEntryTypeEntered = 7,
  k_EChatEntryTypeWasKicked = 8,
  k_EChatEntryTypeWasBanned = 9,
  k_EChatEntryTypeDisconnected = 10,
  k_EChatEntryTypeHistoricalChat = 11,
  k_EChatEntryTypeLinkBlocked = 14
);
EChatRoomEnterResponse = (
  k_EChatRoomEnterResponseSuccess = 1,
  k_EChatRoomEnterResponseDoesntExist = 2,
  k_EChatRoomEnterResponseNotAllowed = 3,
  k_EChatRoomEnterResponseFull = 4,
  k_EChatRoomEnterResponseError = 5,
  k_EChatRoomEnterResponseBanned = 6,
  k_EChatRoomEnterResponseLimited = 7,
  k_EChatRoomEnterResponseClanDisabled = 8,
  k_EChatRoomEnterResponseCommunityBan = 9,
  k_EChatRoomEnterResponseMemberBlockedYou = 10,
  k_EChatRoomEnterResponseYouBlockedMember = 11,
  k_EChatRoomEnterResponseRatelimitExceeded = 15
);
EChatSteamIDInstanceFlags = (
  k_EChatAccountInstanceMask = 4095,
  k_EChatInstanceFlagClan = 524288,
  k_EChatInstanceFlagLobby = 262144,
  k_EChatInstanceFlagMMSLobby = 131072
);
EMarketingMessageFlags = (
  k_EMarketingMessageFlagsNone = 0,
  k_EMarketingMessageFlagsHighPriority = 1,
  k_EMarketingMessageFlagsPlatformWindows = 2,
  k_EMarketingMessageFlagsPlatformMac = 4,
  k_EMarketingMessageFlagsPlatformLinux = 8,
  k_EMarketingMessageFlagsPlatformRestrictions = 14
);
ENotificationPosition = (
  k_EPositionTopLeft = 0,
  k_EPositionTopRight = 1,
  k_EPositionBottomLeft = 2,
  k_EPositionBottomRight = 3
);
EBroadcastUploadResult = (
  k_EBroadcastUploadResultNone = 0,
  k_EBroadcastUploadResultOK = 1,
  k_EBroadcastUploadResultInitFailed = 2,
  k_EBroadcastUploadResultFrameFailed = 3,
  k_EBroadcastUploadResultTimeout = 4,
  k_EBroadcastUploadResultBandwidthExceeded = 5,
  k_EBroadcastUploadResultLowFPS = 6,
  k_EBroadcastUploadResultMissingKeyFrames = 7,
  k_EBroadcastUploadResultNoConnection = 8,
  k_EBroadcastUploadResultRelayFailed = 9,
  k_EBroadcastUploadResultSettingsChanged = 10,
  k_EBroadcastUploadResultMissingAudio = 11,
  k_EBroadcastUploadResultTooFarBehind = 12,
  k_EBroadcastUploadResultTranscodeBehind = 13,
  k_EBroadcastUploadResultNotAllowedToPlay = 14,
  k_EBroadcastUploadResultBusy = 15,
  k_EBroadcastUploadResultBanned = 16,
  k_EBroadcastUploadResultAlreadyActive = 17,
  k_EBroadcastUploadResultForcedOff = 18,
  k_EBroadcastUploadResultAudioBehind = 19,
  k_EBroadcastUploadResultShutdown = 20,
  k_EBroadcastUploadResultDisconnect = 21,
  k_EBroadcastUploadResultVideoInitFailed = 22,
  k_EBroadcastUploadResultAudioInitFailed = 23
);
ELaunchOptionType = (
  k_ELaunchOptionType_None = 0,
  k_ELaunchOptionType_Default = 1,
  k_ELaunchOptionType_SafeMode = 2,
  k_ELaunchOptionType_Multiplayer = 3,
  k_ELaunchOptionType_Config = 4,
  k_ELaunchOptionType_OpenVR = 5,
  k_ELaunchOptionType_Server = 6,
  k_ELaunchOptionType_Editor = 7,
  k_ELaunchOptionType_Manual = 8,
  k_ELaunchOptionType_Benchmark = 9,
  k_ELaunchOptionType_Option1 = 10,
  k_ELaunchOptionType_Option2 = 11,
  k_ELaunchOptionType_Option3 = 12,
  k_ELaunchOptionType_OculusVR = 13,
  k_ELaunchOptionType_OpenVROverlay = 14,
  k_ELaunchOptionType_OSVR = 15,
  k_ELaunchOptionType_Dialog = 1000
);
EVRHMDType = (
  k_eEVRHMDType_None = -1,
  k_eEVRHMDType_Unknown = 0,
  k_eEVRHMDType_HTC_Dev = 1,
  k_eEVRHMDType_HTC_VivePre = 2,
  k_eEVRHMDType_HTC_Vive = 3,
  k_eEVRHMDType_HTC_VivePro = 4,
  k_eEVRHMDType_HTC_ViveCosmos = 5,
  k_eEVRHMDType_HTC_Unknown = 20,
  k_eEVRHMDType_Oculus_DK1 = 21,
  k_eEVRHMDType_Oculus_DK2 = 22,
  k_eEVRHMDType_Oculus_Rift = 23,
  k_eEVRHMDType_Oculus_RiftS = 24,
  k_eEVRHMDType_Oculus_Quest = 25,
  k_eEVRHMDType_Oculus_Unknown = 40,
  k_eEVRHMDType_Acer_Unknown = 50,
  k_eEVRHMDType_Acer_WindowsMR = 51,
  k_eEVRHMDType_Dell_Unknown = 60,
  k_eEVRHMDType_Dell_Visor = 61,
  k_eEVRHMDType_Lenovo_Unknown = 70,
  k_eEVRHMDType_Lenovo_Explorer = 71,
  k_eEVRHMDType_HP_Unknown = 80,
  k_eEVRHMDType_HP_WindowsMR = 81,
  k_eEVRHMDType_HP_Reverb = 82,
  k_eEVRHMDType_Samsung_Unknown = 90,
  k_eEVRHMDType_Samsung_Odyssey = 91,
  k_eEVRHMDType_Unannounced_Unknown = 100,
  k_eEVRHMDType_Unannounced_WindowsMR = 101,
  k_eEVRHMDType_vridge = 110,
  k_eEVRHMDType_Huawei_Unknown = 120,
  k_eEVRHMDType_Huawei_VR2 = 121,
  k_eEVRHMDType_Huawei_EndOfRange = 129,
  k_eEVRHmdType_Valve_Unknown = 130,
  k_eEVRHmdType_Valve_Index = 131
);
EMarketNotAllowedReasonFlags = (
  k_EMarketNotAllowedReason_None = 0,
  k_EMarketNotAllowedReason_TemporaryFailure = 1,
  k_EMarketNotAllowedReason_AccountDisabled = 2,
  k_EMarketNotAllowedReason_AccountLockedDown = 4,
  k_EMarketNotAllowedReason_AccountLimited = 8,
  k_EMarketNotAllowedReason_TradeBanned = 16,
  k_EMarketNotAllowedReason_AccountNotTrusted = 32,
  k_EMarketNotAllowedReason_SteamGuardNotEnabled = 64,
  k_EMarketNotAllowedReason_SteamGuardOnlyRecentlyEnabled = 128,
  k_EMarketNotAllowedReason_RecentPasswordReset = 256,
  k_EMarketNotAllowedReason_NewPaymentMethod = 512,
  k_EMarketNotAllowedReason_InvalidCookie = 1024,
  k_EMarketNotAllowedReason_UsingNewDevice = 2048,
  k_EMarketNotAllowedReason_RecentSelfRefund = 4096,
  k_EMarketNotAllowedReason_NewPaymentMethodCannotBeVerified = 8192,
  k_EMarketNotAllowedReason_NoRecentPurchases = 16384,
  k_EMarketNotAllowedReason_AcceptedWalletGift = 32768
);
EDurationControlProgress = (
  k_EDurationControlProgress_Full = 0,
  k_EDurationControlProgress_Half = 1,
  k_EDurationControlProgress_None = 2,
  k_EDurationControl_ExitSoon_3h = 3,
  k_EDurationControl_ExitSoon_5h = 4,
  k_EDurationControl_ExitSoon_Night = 5
);
EDurationControlNotification = (
  k_EDurationControlNotification_None = 0,
  k_EDurationControlNotification_1Hour = 1,
  k_EDurationControlNotification_3Hours = 2,
  k_EDurationControlNotification_HalfProgress = 3,
  k_EDurationControlNotification_NoProgress = 4,
  k_EDurationControlNotification_ExitSoon_3h = 5,
  k_EDurationControlNotification_ExitSoon_5h = 6,
  k_EDurationControlNotification_ExitSoon_Night = 7
);
EGameSearchErrorCode_t = (
  k_EGameSearchErrorCode_OK = 1,
  k_EGameSearchErrorCode_Failed_Search_Already_In_Progress = 2,
  k_EGameSearchErrorCode_Failed_No_Search_In_Progress = 3,
  k_EGameSearchErrorCode_Failed_Not_Lobby_Leader = 4,
  k_EGameSearchErrorCode_Failed_No_Host_Available = 5,
  k_EGameSearchErrorCode_Failed_Search_Params_Invalid = 6,
  k_EGameSearchErrorCode_Failed_Offline = 7,
  k_EGameSearchErrorCode_Failed_NotAuthorized = 8,
  k_EGameSearchErrorCode_Failed_Unknown_Error = 9
);
EPlayerResult_t = (
  k_EPlayerResultFailedToConnect = 1,
  k_EPlayerResultAbandoned = 2,
  k_EPlayerResultKicked = 3,
  k_EPlayerResultIncomplete = 4,
  k_EPlayerResultCompleted = 5
);
ESteamIPv6ConnectivityProtocol = (
  k_ESteamIPv6ConnectivityProtocol_Invalid = 0,
  k_ESteamIPv6ConnectivityProtocol_HTTP = 1,
  k_ESteamIPv6ConnectivityProtocol_UDP = 2
);
ESteamIPv6ConnectivityState = (
  k_ESteamIPv6ConnectivityState_Unknown = 0,
  k_ESteamIPv6ConnectivityState_Good = 1,
  k_ESteamIPv6ConnectivityState_Bad = 2
);
EFriendRelationship = (
  k_EFriendRelationshipNone = 0,
  k_EFriendRelationshipBlocked = 1,
  k_EFriendRelationshipRequestRecipient = 2,
  k_EFriendRelationshipFriend = 3,
  k_EFriendRelationshipRequestInitiator = 4,
  k_EFriendRelationshipIgnored = 5,
  k_EFriendRelationshipIgnoredFriend = 6,
  k_EFriendRelationshipSuggested_DEPRECATED = 7,
  k_EFriendRelationshipMax = 8
);
EPersonaState = (
  k_EPersonaStateOffline = 0,
  k_EPersonaStateOnline = 1,
  k_EPersonaStateBusy = 2,
  k_EPersonaStateAway = 3,
  k_EPersonaStateSnooze = 4,
  k_EPersonaStateLookingToTrade = 5,
  k_EPersonaStateLookingToPlay = 6,
  k_EPersonaStateInvisible = 7,
  k_EPersonaStateMax = 8
);
EFriendFlags = (
  k_EFriendFlagNone = 0,
  k_EFriendFlagBlocked = 1,
  k_EFriendFlagFriendshipRequested = 2,
  k_EFriendFlagImmediate = 4,
  k_EFriendFlagClanMember = 8,
  k_EFriendFlagOnGameServer = 16,
  k_EFriendFlagRequestingFriendship = 128,
  k_EFriendFlagRequestingInfo = 256,
  k_EFriendFlagIgnored = 512,
  k_EFriendFlagIgnoredFriend = 1024,
  k_EFriendFlagChatMember = 4096,
  k_EFriendFlagAll = 65535
);
EUserRestriction = (
  k_nUserRestrictionNone = 0,
  k_nUserRestrictionUnknown = 1,
  k_nUserRestrictionAnyChat = 2,
  k_nUserRestrictionVoiceChat = 4,
  k_nUserRestrictionGroupChat = 8,
  k_nUserRestrictionRating = 16,
  k_nUserRestrictionGameInvites = 32,
  k_nUserRestrictionTrading = 64
);
EOverlayToStoreFlag = (
  k_EOverlayToStoreFlag_None = 0,
  k_EOverlayToStoreFlag_AddToCart = 1,
  k_EOverlayToStoreFlag_AddToCartAndShow = 2
);
EActivateGameOverlayToWebPageMode = (
  k_EActivateGameOverlayToWebPageMode_Default = 0,
  k_EActivateGameOverlayToWebPageMode_Modal = 1
);
EPersonaChange = (
  k_EPersonaChangeName = 1,
  k_EPersonaChangeStatus = 2,
  k_EPersonaChangeComeOnline = 4,
  k_EPersonaChangeGoneOffline = 8,
  k_EPersonaChangeGamePlayed = 16,
  k_EPersonaChangeGameServer = 32,
  k_EPersonaChangeAvatar = 64,
  k_EPersonaChangeJoinedSource = 128,
  k_EPersonaChangeLeftSource = 256,
  k_EPersonaChangeRelationshipChanged = 512,
  k_EPersonaChangeNameFirstSet = 1024,
  k_EPersonaChangeBroadcast = 2048,
  k_EPersonaChangeNickname = 4096,
  k_EPersonaChangeSteamLevel = 8192,
  k_EPersonaChangeRichPresence = 16384
);
ESteamAPICallFailure = (
  k_ESteamAPICallFailureNone = -1,
  k_ESteamAPICallFailureSteamGone = 0,
  k_ESteamAPICallFailureNetworkFailure = 1,
  k_ESteamAPICallFailureInvalidHandle = 2,
  k_ESteamAPICallFailureMismatchedCallback = 3
);
EGamepadTextInputMode = (
  k_EGamepadTextInputModeNormal = 0,
  k_EGamepadTextInputModePassword = 1
);
EGamepadTextInputLineMode = (
  k_EGamepadTextInputLineModeSingleLine = 0,
  k_EGamepadTextInputLineModeMultipleLines = 1
);
ECheckFileSignature = (
  k_ECheckFileSignatureInvalidSignature = 0,
  k_ECheckFileSignatureValidSignature = 1,
  k_ECheckFileSignatureFileNotFound = 2,
  k_ECheckFileSignatureNoSignaturesFoundForThisApp = 3,
  k_ECheckFileSignatureNoSignaturesFoundForThisFile = 4
);
EMatchMakingServerResponse = (
  eServerResponded = 0,
  eServerFailedToRespond = 1,
  eNoServersListedOnMasterServer = 2
);
ELobbyType = (
  k_ELobbyTypePrivate = 0,
  k_ELobbyTypeFriendsOnly = 1,
  k_ELobbyTypePublic = 2,
  k_ELobbyTypeInvisible = 3,
  k_ELobbyTypePrivateUnique = 4
);
ELobbyComparison = (
  k_ELobbyComparisonEqualToOrLessThan = -2,
  k_ELobbyComparisonLessThan = -1,
  k_ELobbyComparisonEqual = 0,
  k_ELobbyComparisonGreaterThan = 1,
  k_ELobbyComparisonEqualToOrGreaterThan = 2,
  k_ELobbyComparisonNotEqual = 3
);
ELobbyDistanceFilter = (
  k_ELobbyDistanceFilterClose = 0,
  k_ELobbyDistanceFilterDefault = 1,
  k_ELobbyDistanceFilterFar = 2,
  k_ELobbyDistanceFilterWorldwide = 3
);
EChatMemberStateChange = (
  k_EChatMemberStateChangeEntered = 1,
  k_EChatMemberStateChangeLeft = 2,
  k_EChatMemberStateChangeDisconnected = 4,
  k_EChatMemberStateChangeKicked = 8,
  k_EChatMemberStateChangeBanned = 16
);
ESteamPartyBeaconLocationType = (
  k_ESteamPartyBeaconLocationType_Invalid = 0,
  k_ESteamPartyBeaconLocationType_ChatGroup = 1,
  k_ESteamPartyBeaconLocationType_Max = 2
);
ESteamPartyBeaconLocationData = (
  k_ESteamPartyBeaconLocationDataInvalid = 0,
  k_ESteamPartyBeaconLocationDataName = 1,
  k_ESteamPartyBeaconLocationDataIconURLSmall = 2,
  k_ESteamPartyBeaconLocationDataIconURLMedium = 3,
  k_ESteamPartyBeaconLocationDataIconURLLarge = 4
);
ERemoteStoragePlatform = (
  k_ERemoteStoragePlatformNone = 0,
  k_ERemoteStoragePlatformWindows = 1,
  k_ERemoteStoragePlatformOSX = 2,
  k_ERemoteStoragePlatformPS3 = 4,
  k_ERemoteStoragePlatformLinux = 8,
  k_ERemoteStoragePlatformSwitch = 16,
  k_ERemoteStoragePlatformAndroid = 32,
  k_ERemoteStoragePlatformIOS = 64,
  k_ERemoteStoragePlatformAll = -1
);
ERemoteStoragePublishedFileVisibility = (
  k_ERemoteStoragePublishedFileVisibilityPublic = 0,
  k_ERemoteStoragePublishedFileVisibilityFriendsOnly = 1,
  k_ERemoteStoragePublishedFileVisibilityPrivate = 2,
  k_ERemoteStoragePublishedFileVisibilityUnlisted = 3
);
EWorkshopFileType = (
  k_EWorkshopFileTypeFirst = 0,
  k_EWorkshopFileTypeCommunity = 0,
  k_EWorkshopFileTypeMicrotransaction = 1,
  k_EWorkshopFileTypeCollection = 2,
  k_EWorkshopFileTypeArt = 3,
  k_EWorkshopFileTypeVideo = 4,
  k_EWorkshopFileTypeScreenshot = 5,
  k_EWorkshopFileTypeGame = 6,
  k_EWorkshopFileTypeSoftware = 7,
  k_EWorkshopFileTypeConcept = 8,
  k_EWorkshopFileTypeWebGuide = 9,
  k_EWorkshopFileTypeIntegratedGuide = 10,
  k_EWorkshopFileTypeMerch = 11,
  k_EWorkshopFileTypeControllerBinding = 12,
  k_EWorkshopFileTypeSteamworksAccessInvite = 13,
  k_EWorkshopFileTypeSteamVideo = 14,
  k_EWorkshopFileTypeGameManagedItem = 15,
  k_EWorkshopFileTypeMax = 16
);
EWorkshopVote = (
  k_EWorkshopVoteUnvoted = 0,
  k_EWorkshopVoteFor = 1,
  k_EWorkshopVoteAgainst = 2,
  k_EWorkshopVoteLater = 3
);
EWorkshopFileAction = (
  k_EWorkshopFileActionPlayed = 0,
  k_EWorkshopFileActionCompleted = 1
);
EWorkshopEnumerationType = (
  k_EWorkshopEnumerationTypeRankedByVote = 0,
  k_EWorkshopEnumerationTypeRecent = 1,
  k_EWorkshopEnumerationTypeTrending = 2,
  k_EWorkshopEnumerationTypeFavoritesOfFriends = 3,
  k_EWorkshopEnumerationTypeVotedByFriends = 4,
  k_EWorkshopEnumerationTypeContentByFriends = 5,
  k_EWorkshopEnumerationTypeRecentFromFollowedUsers = 6
);
EWorkshopVideoProvider = (
  k_EWorkshopVideoProviderNone = 0,
  k_EWorkshopVideoProviderYoutube = 1
);
EUGCReadAction = (
  k_EUGCRead_ContinueReadingUntilFinished = 0,
  k_EUGCRead_ContinueReading = 1,
  k_EUGCRead_Close = 2
);
ELeaderboardDataRequest = (
  k_ELeaderboardDataRequestGlobal = 0,
  k_ELeaderboardDataRequestGlobalAroundUser = 1,
  k_ELeaderboardDataRequestFriends = 2,
  k_ELeaderboardDataRequestUsers = 3
);
ELeaderboardSortMethod = (
  k_ELeaderboardSortMethodNone = 0,
  k_ELeaderboardSortMethodAscending = 1,
  k_ELeaderboardSortMethodDescending = 2
);
ELeaderboardDisplayType = (
  k_ELeaderboardDisplayTypeNone = 0,
  k_ELeaderboardDisplayTypeNumeric = 1,
  k_ELeaderboardDisplayTypeTimeSeconds = 2,
  k_ELeaderboardDisplayTypeTimeMilliSeconds = 3
);
ELeaderboardUploadScoreMethod = (
  k_ELeaderboardUploadScoreMethodNone = 0,
  k_ELeaderboardUploadScoreMethodKeepBest = 1,
  k_ELeaderboardUploadScoreMethodForceUpdate = 2
);
ERegisterActivationCodeResult = (
  k_ERegisterActivationCodeResultOK = 0,
  k_ERegisterActivationCodeResultFail = 1,
  k_ERegisterActivationCodeResultAlreadyRegistered = 2,
  k_ERegisterActivationCodeResultTimeout = 3,
  k_ERegisterActivationCodeAlreadyOwned = 4
);
EP2PSessionError = (
  k_EP2PSessionErrorNone = 0,
  k_EP2PSessionErrorNotRunningApp = 1,
  k_EP2PSessionErrorNoRightsToApp = 2,
  k_EP2PSessionErrorDestinationNotLoggedIn = 3,
  k_EP2PSessionErrorTimeout = 4,
  k_EP2PSessionErrorMax = 5
);
EP2PSend = (
  k_EP2PSendUnreliable = 0,
  k_EP2PSendUnreliableNoDelay = 1,
  k_EP2PSendReliable = 2,
  k_EP2PSendReliableWithBuffering = 3
);
ESNetSocketState = (
  k_ESNetSocketStateInvalid = 0,
  k_ESNetSocketStateConnected = 1,
  k_ESNetSocketStateInitiated = 10,
  k_ESNetSocketStateLocalCandidatesFound = 11,
  k_ESNetSocketStateReceivedRemoteCandidates = 12,
  k_ESNetSocketStateChallengeHandshake = 15,
  k_ESNetSocketStateDisconnecting = 21,
  k_ESNetSocketStateLocalDisconnect = 22,
  k_ESNetSocketStateTimeoutDuringConnect = 23,
  k_ESNetSocketStateRemoteEndDisconnected = 24,
  k_ESNetSocketStateConnectionBroken = 25
);
ESNetSocketConnectionType = (
  k_ESNetSocketConnectionTypeNotConnected = 0,
  k_ESNetSocketConnectionTypeUDP = 1,
  k_ESNetSocketConnectionTypeUDPRelay = 2
);
EVRScreenshotType = (
  k_EVRScreenshotType_None = 0,
  k_EVRScreenshotType_Mono = 1,
  k_EVRScreenshotType_Stereo = 2,
  k_EVRScreenshotType_MonoCubemap = 3,
  k_EVRScreenshotType_MonoPanorama = 4,
  k_EVRScreenshotType_StereoPanorama = 5
);
AudioPlayback_Status = (
  AudioPlayback_Undefined = 0,
  AudioPlayback_Playing = 1,
  AudioPlayback_Paused = 2,
  AudioPlayback_Idle = 3
);
EHTTPMethod = (
  k_EHTTPMethodInvalid = 0,
  k_EHTTPMethodGET = 1,
  k_EHTTPMethodHEAD = 2,
  k_EHTTPMethodPOST = 3,
  k_EHTTPMethodPUT = 4,
  k_EHTTPMethodDELETE = 5,
  k_EHTTPMethodOPTIONS = 6,
  k_EHTTPMethodPATCH = 7
);
EHTTPStatusCode = (
  k_EHTTPStatusCodeInvalid = 0,
  k_EHTTPStatusCode100Continue = 100,
  k_EHTTPStatusCode101SwitchingProtocols = 101,
  k_EHTTPStatusCode200OK = 200,
  k_EHTTPStatusCode201Created = 201,
  k_EHTTPStatusCode202Accepted = 202,
  k_EHTTPStatusCode203NonAuthoritative = 203,
  k_EHTTPStatusCode204NoContent = 204,
  k_EHTTPStatusCode205ResetContent = 205,
  k_EHTTPStatusCode206PartialContent = 206,
  k_EHTTPStatusCode300MultipleChoices = 300,
  k_EHTTPStatusCode301MovedPermanently = 301,
  k_EHTTPStatusCode302Found = 302,
  k_EHTTPStatusCode303SeeOther = 303,
  k_EHTTPStatusCode304NotModified = 304,
  k_EHTTPStatusCode305UseProxy = 305,
  k_EHTTPStatusCode307TemporaryRedirect = 307,
  k_EHTTPStatusCode400BadRequest = 400,
  k_EHTTPStatusCode401Unauthorized = 401,
  k_EHTTPStatusCode402PaymentRequired = 402,
  k_EHTTPStatusCode403Forbidden = 403,
  k_EHTTPStatusCode404NotFound = 404,
  k_EHTTPStatusCode405MethodNotAllowed = 405,
  k_EHTTPStatusCode406NotAcceptable = 406,
  k_EHTTPStatusCode407ProxyAuthRequired = 407,
  k_EHTTPStatusCode408RequestTimeout = 408,
  k_EHTTPStatusCode409Conflict = 409,
  k_EHTTPStatusCode410Gone = 410,
  k_EHTTPStatusCode411LengthRequired = 411,
  k_EHTTPStatusCode412PreconditionFailed = 412,
  k_EHTTPStatusCode413RequestEntityTooLarge = 413,
  k_EHTTPStatusCode414RequestURITooLong = 414,
  k_EHTTPStatusCode415UnsupportedMediaType = 415,
  k_EHTTPStatusCode416RequestedRangeNotSatisfiable = 416,
  k_EHTTPStatusCode417ExpectationFailed = 417,
  k_EHTTPStatusCode4xxUnknown = 418,
  k_EHTTPStatusCode429TooManyRequests = 429,
  k_EHTTPStatusCode500InternalServerError = 500,
  k_EHTTPStatusCode501NotImplemented = 501,
  k_EHTTPStatusCode502BadGateway = 502,
  k_EHTTPStatusCode503ServiceUnavailable = 503,
  k_EHTTPStatusCode504GatewayTimeout = 504,
  k_EHTTPStatusCode505HTTPVersionNotSupported = 505,
  k_EHTTPStatusCode5xxUnknown = 599
);
EInputSourceMode = (
  k_EInputSourceMode_None = 0,
  k_EInputSourceMode_Dpad = 1,
  k_EInputSourceMode_Buttons = 2,
  k_EInputSourceMode_FourButtons = 3,
  k_EInputSourceMode_AbsoluteMouse = 4,
  k_EInputSourceMode_RelativeMouse = 5,
  k_EInputSourceMode_JoystickMove = 6,
  k_EInputSourceMode_JoystickMouse = 7,
  k_EInputSourceMode_JoystickCamera = 8,
  k_EInputSourceMode_ScrollWheel = 9,
  k_EInputSourceMode_Trigger = 10,
  k_EInputSourceMode_TouchMenu = 11,
  k_EInputSourceMode_MouseJoystick = 12,
  k_EInputSourceMode_MouseRegion = 13,
  k_EInputSourceMode_RadialMenu = 14,
  k_EInputSourceMode_SingleButton = 15,
  k_EInputSourceMode_Switches = 16
);
EInputActionOrigin = (
  k_EInputActionOrigin_None = 0,
  k_EInputActionOrigin_SteamController_A = 1,
  k_EInputActionOrigin_SteamController_B = 2,
  k_EInputActionOrigin_SteamController_X = 3,
  k_EInputActionOrigin_SteamController_Y = 4,
  k_EInputActionOrigin_SteamController_LeftBumper = 5,
  k_EInputActionOrigin_SteamController_RightBumper = 6,
  k_EInputActionOrigin_SteamController_LeftGrip = 7,
  k_EInputActionOrigin_SteamController_RightGrip = 8,
  k_EInputActionOrigin_SteamController_Start = 9,
  k_EInputActionOrigin_SteamController_Back = 10,
  k_EInputActionOrigin_SteamController_LeftPad_Touch = 11,
  k_EInputActionOrigin_SteamController_LeftPad_Swipe = 12,
  k_EInputActionOrigin_SteamController_LeftPad_Click = 13,
  k_EInputActionOrigin_SteamController_LeftPad_DPadNorth = 14,
  k_EInputActionOrigin_SteamController_LeftPad_DPadSouth = 15,
  k_EInputActionOrigin_SteamController_LeftPad_DPadWest = 16,
  k_EInputActionOrigin_SteamController_LeftPad_DPadEast = 17,
  k_EInputActionOrigin_SteamController_RightPad_Touch = 18,
  k_EInputActionOrigin_SteamController_RightPad_Swipe = 19,
  k_EInputActionOrigin_SteamController_RightPad_Click = 20,
  k_EInputActionOrigin_SteamController_RightPad_DPadNorth = 21,
  k_EInputActionOrigin_SteamController_RightPad_DPadSouth = 22,
  k_EInputActionOrigin_SteamController_RightPad_DPadWest = 23,
  k_EInputActionOrigin_SteamController_RightPad_DPadEast = 24,
  k_EInputActionOrigin_SteamController_LeftTrigger_Pull = 25,
  k_EInputActionOrigin_SteamController_LeftTrigger_Click = 26,
  k_EInputActionOrigin_SteamController_RightTrigger_Pull = 27,
  k_EInputActionOrigin_SteamController_RightTrigger_Click = 28,
  k_EInputActionOrigin_SteamController_LeftStick_Move = 29,
  k_EInputActionOrigin_SteamController_LeftStick_Click = 30,
  k_EInputActionOrigin_SteamController_LeftStick_DPadNorth = 31,
  k_EInputActionOrigin_SteamController_LeftStick_DPadSouth = 32,
  k_EInputActionOrigin_SteamController_LeftStick_DPadWest = 33,
  k_EInputActionOrigin_SteamController_LeftStick_DPadEast = 34,
  k_EInputActionOrigin_SteamController_Gyro_Move = 35,
  k_EInputActionOrigin_SteamController_Gyro_Pitch = 36,
  k_EInputActionOrigin_SteamController_Gyro_Yaw = 37,
  k_EInputActionOrigin_SteamController_Gyro_Roll = 38,
  k_EInputActionOrigin_SteamController_Reserved0 = 39,
  k_EInputActionOrigin_SteamController_Reserved1 = 40,
  k_EInputActionOrigin_SteamController_Reserved2 = 41,
  k_EInputActionOrigin_SteamController_Reserved3 = 42,
  k_EInputActionOrigin_SteamController_Reserved4 = 43,
  k_EInputActionOrigin_SteamController_Reserved5 = 44,
  k_EInputActionOrigin_SteamController_Reserved6 = 45,
  k_EInputActionOrigin_SteamController_Reserved7 = 46,
  k_EInputActionOrigin_SteamController_Reserved8 = 47,
  k_EInputActionOrigin_SteamController_Reserved9 = 48,
  k_EInputActionOrigin_SteamController_Reserved10 = 49,
  k_EInputActionOrigin_PS4_X = 50,
  k_EInputActionOrigin_PS4_Circle = 51,
  k_EInputActionOrigin_PS4_Triangle = 52,
  k_EInputActionOrigin_PS4_Square = 53,
  k_EInputActionOrigin_PS4_LeftBumper = 54,
  k_EInputActionOrigin_PS4_RightBumper = 55,
  k_EInputActionOrigin_PS4_Options = 56,
  k_EInputActionOrigin_PS4_Share = 57,
  k_EInputActionOrigin_PS4_LeftPad_Touch = 58,
  k_EInputActionOrigin_PS4_LeftPad_Swipe = 59,
  k_EInputActionOrigin_PS4_LeftPad_Click = 60,
  k_EInputActionOrigin_PS4_LeftPad_DPadNorth = 61,
  k_EInputActionOrigin_PS4_LeftPad_DPadSouth = 62,
  k_EInputActionOrigin_PS4_LeftPad_DPadWest = 63,
  k_EInputActionOrigin_PS4_LeftPad_DPadEast = 64,
  k_EInputActionOrigin_PS4_RightPad_Touch = 65,
  k_EInputActionOrigin_PS4_RightPad_Swipe = 66,
  k_EInputActionOrigin_PS4_RightPad_Click = 67,
  k_EInputActionOrigin_PS4_RightPad_DPadNorth = 68,
  k_EInputActionOrigin_PS4_RightPad_DPadSouth = 69,
  k_EInputActionOrigin_PS4_RightPad_DPadWest = 70,
  k_EInputActionOrigin_PS4_RightPad_DPadEast = 71,
  k_EInputActionOrigin_PS4_CenterPad_Touch = 72,
  k_EInputActionOrigin_PS4_CenterPad_Swipe = 73,
  k_EInputActionOrigin_PS4_CenterPad_Click = 74,
  k_EInputActionOrigin_PS4_CenterPad_DPadNorth = 75,
  k_EInputActionOrigin_PS4_CenterPad_DPadSouth = 76,
  k_EInputActionOrigin_PS4_CenterPad_DPadWest = 77,
  k_EInputActionOrigin_PS4_CenterPad_DPadEast = 78,
  k_EInputActionOrigin_PS4_LeftTrigger_Pull = 79,
  k_EInputActionOrigin_PS4_LeftTrigger_Click = 80,
  k_EInputActionOrigin_PS4_RightTrigger_Pull = 81,
  k_EInputActionOrigin_PS4_RightTrigger_Click = 82,
  k_EInputActionOrigin_PS4_LeftStick_Move = 83,
  k_EInputActionOrigin_PS4_LeftStick_Click = 84,
  k_EInputActionOrigin_PS4_LeftStick_DPadNorth = 85,
  k_EInputActionOrigin_PS4_LeftStick_DPadSouth = 86,
  k_EInputActionOrigin_PS4_LeftStick_DPadWest = 87,
  k_EInputActionOrigin_PS4_LeftStick_DPadEast = 88,
  k_EInputActionOrigin_PS4_RightStick_Move = 89,
  k_EInputActionOrigin_PS4_RightStick_Click = 90,
  k_EInputActionOrigin_PS4_RightStick_DPadNorth = 91,
  k_EInputActionOrigin_PS4_RightStick_DPadSouth = 92,
  k_EInputActionOrigin_PS4_RightStick_DPadWest = 93,
  k_EInputActionOrigin_PS4_RightStick_DPadEast = 94,
  k_EInputActionOrigin_PS4_DPad_North = 95,
  k_EInputActionOrigin_PS4_DPad_South = 96,
  k_EInputActionOrigin_PS4_DPad_West = 97,
  k_EInputActionOrigin_PS4_DPad_East = 98,
  k_EInputActionOrigin_PS4_Gyro_Move = 99,
  k_EInputActionOrigin_PS4_Gyro_Pitch = 100,
  k_EInputActionOrigin_PS4_Gyro_Yaw = 101,
  k_EInputActionOrigin_PS4_Gyro_Roll = 102,
  k_EInputActionOrigin_PS4_DPad_Move = 103,
  k_EInputActionOrigin_PS4_Reserved1 = 104,
  k_EInputActionOrigin_PS4_Reserved2 = 105,
  k_EInputActionOrigin_PS4_Reserved3 = 106,
  k_EInputActionOrigin_PS4_Reserved4 = 107,
  k_EInputActionOrigin_PS4_Reserved5 = 108,
  k_EInputActionOrigin_PS4_Reserved6 = 109,
  k_EInputActionOrigin_PS4_Reserved7 = 110,
  k_EInputActionOrigin_PS4_Reserved8 = 111,
  k_EInputActionOrigin_PS4_Reserved9 = 112,
  k_EInputActionOrigin_PS4_Reserved10 = 113,
  k_EInputActionOrigin_XBoxOne_A = 114,
  k_EInputActionOrigin_XBoxOne_B = 115,
  k_EInputActionOrigin_XBoxOne_X = 116,
  k_EInputActionOrigin_XBoxOne_Y = 117,
  k_EInputActionOrigin_XBoxOne_LeftBumper = 118,
  k_EInputActionOrigin_XBoxOne_RightBumper = 119,
  k_EInputActionOrigin_XBoxOne_Menu = 120,
  k_EInputActionOrigin_XBoxOne_View = 121,
  k_EInputActionOrigin_XBoxOne_LeftTrigger_Pull = 122,
  k_EInputActionOrigin_XBoxOne_LeftTrigger_Click = 123,
  k_EInputActionOrigin_XBoxOne_RightTrigger_Pull = 124,
  k_EInputActionOrigin_XBoxOne_RightTrigger_Click = 125,
  k_EInputActionOrigin_XBoxOne_LeftStick_Move = 126,
  k_EInputActionOrigin_XBoxOne_LeftStick_Click = 127,
  k_EInputActionOrigin_XBoxOne_LeftStick_DPadNorth = 128,
  k_EInputActionOrigin_XBoxOne_LeftStick_DPadSouth = 129,
  k_EInputActionOrigin_XBoxOne_LeftStick_DPadWest = 130,
  k_EInputActionOrigin_XBoxOne_LeftStick_DPadEast = 131,
  k_EInputActionOrigin_XBoxOne_RightStick_Move = 132,
  k_EInputActionOrigin_XBoxOne_RightStick_Click = 133,
  k_EInputActionOrigin_XBoxOne_RightStick_DPadNorth = 134,
  k_EInputActionOrigin_XBoxOne_RightStick_DPadSouth = 135,
  k_EInputActionOrigin_XBoxOne_RightStick_DPadWest = 136,
  k_EInputActionOrigin_XBoxOne_RightStick_DPadEast = 137,
  k_EInputActionOrigin_XBoxOne_DPad_North = 138,
  k_EInputActionOrigin_XBoxOne_DPad_South = 139,
  k_EInputActionOrigin_XBoxOne_DPad_West = 140,
  k_EInputActionOrigin_XBoxOne_DPad_East = 141,
  k_EInputActionOrigin_XBoxOne_DPad_Move = 142,
  k_EInputActionOrigin_XBoxOne_Reserved1 = 143,
  k_EInputActionOrigin_XBoxOne_Reserved2 = 144,
  k_EInputActionOrigin_XBoxOne_Reserved3 = 145,
  k_EInputActionOrigin_XBoxOne_Reserved4 = 146,
  k_EInputActionOrigin_XBoxOne_Reserved5 = 147,
  k_EInputActionOrigin_XBoxOne_Reserved6 = 148,
  k_EInputActionOrigin_XBoxOne_Reserved7 = 149,
  k_EInputActionOrigin_XBoxOne_Reserved8 = 150,
  k_EInputActionOrigin_XBoxOne_Reserved9 = 151,
  k_EInputActionOrigin_XBoxOne_Reserved10 = 152,
  k_EInputActionOrigin_XBox360_A = 153,
  k_EInputActionOrigin_XBox360_B = 154,
  k_EInputActionOrigin_XBox360_X = 155,
  k_EInputActionOrigin_XBox360_Y = 156,
  k_EInputActionOrigin_XBox360_LeftBumper = 157,
  k_EInputActionOrigin_XBox360_RightBumper = 158,
  k_EInputActionOrigin_XBox360_Start = 159,
  k_EInputActionOrigin_XBox360_Back = 160,
  k_EInputActionOrigin_XBox360_LeftTrigger_Pull = 161,
  k_EInputActionOrigin_XBox360_LeftTrigger_Click = 162,
  k_EInputActionOrigin_XBox360_RightTrigger_Pull = 163,
  k_EInputActionOrigin_XBox360_RightTrigger_Click = 164,
  k_EInputActionOrigin_XBox360_LeftStick_Move = 165,
  k_EInputActionOrigin_XBox360_LeftStick_Click = 166,
  k_EInputActionOrigin_XBox360_LeftStick_DPadNorth = 167,
  k_EInputActionOrigin_XBox360_LeftStick_DPadSouth = 168,
  k_EInputActionOrigin_XBox360_LeftStick_DPadWest = 169,
  k_EInputActionOrigin_XBox360_LeftStick_DPadEast = 170,
  k_EInputActionOrigin_XBox360_RightStick_Move = 171,
  k_EInputActionOrigin_XBox360_RightStick_Click = 172,
  k_EInputActionOrigin_XBox360_RightStick_DPadNorth = 173,
  k_EInputActionOrigin_XBox360_RightStick_DPadSouth = 174,
  k_EInputActionOrigin_XBox360_RightStick_DPadWest = 175,
  k_EInputActionOrigin_XBox360_RightStick_DPadEast = 176,
  k_EInputActionOrigin_XBox360_DPad_North = 177,
  k_EInputActionOrigin_XBox360_DPad_South = 178,
  k_EInputActionOrigin_XBox360_DPad_West = 179,
  k_EInputActionOrigin_XBox360_DPad_East = 180,
  k_EInputActionOrigin_XBox360_DPad_Move = 181,
  k_EInputActionOrigin_XBox360_Reserved1 = 182,
  k_EInputActionOrigin_XBox360_Reserved2 = 183,
  k_EInputActionOrigin_XBox360_Reserved3 = 184,
  k_EInputActionOrigin_XBox360_Reserved4 = 185,
  k_EInputActionOrigin_XBox360_Reserved5 = 186,
  k_EInputActionOrigin_XBox360_Reserved6 = 187,
  k_EInputActionOrigin_XBox360_Reserved7 = 188,
  k_EInputActionOrigin_XBox360_Reserved8 = 189,
  k_EInputActionOrigin_XBox360_Reserved9 = 190,
  k_EInputActionOrigin_XBox360_Reserved10 = 191,
  k_EInputActionOrigin_Switch_A = 192,
  k_EInputActionOrigin_Switch_B = 193,
  k_EInputActionOrigin_Switch_X = 194,
  k_EInputActionOrigin_Switch_Y = 195,
  k_EInputActionOrigin_Switch_LeftBumper = 196,
  k_EInputActionOrigin_Switch_RightBumper = 197,
  k_EInputActionOrigin_Switch_Plus = 198,
  k_EInputActionOrigin_Switch_Minus = 199,
  k_EInputActionOrigin_Switch_Capture = 200,
  k_EInputActionOrigin_Switch_LeftTrigger_Pull = 201,
  k_EInputActionOrigin_Switch_LeftTrigger_Click = 202,
  k_EInputActionOrigin_Switch_RightTrigger_Pull = 203,
  k_EInputActionOrigin_Switch_RightTrigger_Click = 204,
  k_EInputActionOrigin_Switch_LeftStick_Move = 205,
  k_EInputActionOrigin_Switch_LeftStick_Click = 206,
  k_EInputActionOrigin_Switch_LeftStick_DPadNorth = 207,
  k_EInputActionOrigin_Switch_LeftStick_DPadSouth = 208,
  k_EInputActionOrigin_Switch_LeftStick_DPadWest = 209,
  k_EInputActionOrigin_Switch_LeftStick_DPadEast = 210,
  k_EInputActionOrigin_Switch_RightStick_Move = 211,
  k_EInputActionOrigin_Switch_RightStick_Click = 212,
  k_EInputActionOrigin_Switch_RightStick_DPadNorth = 213,
  k_EInputActionOrigin_Switch_RightStick_DPadSouth = 214,
  k_EInputActionOrigin_Switch_RightStick_DPadWest = 215,
  k_EInputActionOrigin_Switch_RightStick_DPadEast = 216,
  k_EInputActionOrigin_Switch_DPad_North = 217,
  k_EInputActionOrigin_Switch_DPad_South = 218,
  k_EInputActionOrigin_Switch_DPad_West = 219,
  k_EInputActionOrigin_Switch_DPad_East = 220,
  k_EInputActionOrigin_Switch_ProGyro_Move = 221,
  k_EInputActionOrigin_Switch_ProGyro_Pitch = 222,
  k_EInputActionOrigin_Switch_ProGyro_Yaw = 223,
  k_EInputActionOrigin_Switch_ProGyro_Roll = 224,
  k_EInputActionOrigin_Switch_DPad_Move = 225,
  k_EInputActionOrigin_Switch_Reserved1 = 226,
  k_EInputActionOrigin_Switch_Reserved2 = 227,
  k_EInputActionOrigin_Switch_Reserved3 = 228,
  k_EInputActionOrigin_Switch_Reserved4 = 229,
  k_EInputActionOrigin_Switch_Reserved5 = 230,
  k_EInputActionOrigin_Switch_Reserved6 = 231,
  k_EInputActionOrigin_Switch_Reserved7 = 232,
  k_EInputActionOrigin_Switch_Reserved8 = 233,
  k_EInputActionOrigin_Switch_Reserved9 = 234,
  k_EInputActionOrigin_Switch_Reserved10 = 235,
  k_EInputActionOrigin_Switch_RightGyro_Move = 236,
  k_EInputActionOrigin_Switch_RightGyro_Pitch = 237,
  k_EInputActionOrigin_Switch_RightGyro_Yaw = 238,
  k_EInputActionOrigin_Switch_RightGyro_Roll = 239,
  k_EInputActionOrigin_Switch_LeftGyro_Move = 240,
  k_EInputActionOrigin_Switch_LeftGyro_Pitch = 241,
  k_EInputActionOrigin_Switch_LeftGyro_Yaw = 242,
  k_EInputActionOrigin_Switch_LeftGyro_Roll = 243,
  k_EInputActionOrigin_Switch_LeftGrip_Lower = 244,
  k_EInputActionOrigin_Switch_LeftGrip_Upper = 245,
  k_EInputActionOrigin_Switch_RightGrip_Lower = 246,
  k_EInputActionOrigin_Switch_RightGrip_Upper = 247,
  k_EInputActionOrigin_Switch_Reserved11 = 248,
  k_EInputActionOrigin_Switch_Reserved12 = 249,
  k_EInputActionOrigin_Switch_Reserved13 = 250,
  k_EInputActionOrigin_Switch_Reserved14 = 251,
  k_EInputActionOrigin_Switch_Reserved15 = 252,
  k_EInputActionOrigin_Switch_Reserved16 = 253,
  k_EInputActionOrigin_Switch_Reserved17 = 254,
  k_EInputActionOrigin_Switch_Reserved18 = 255,
  k_EInputActionOrigin_Switch_Reserved19 = 256,
  k_EInputActionOrigin_Switch_Reserved20 = 257,
  k_EInputActionOrigin_Count = 258,
  k_EInputActionOrigin_MaximumPossibleValue = 32767
);
EXboxOrigin = (
  k_EXboxOrigin_A = 0,
  k_EXboxOrigin_B = 1,
  k_EXboxOrigin_X = 2,
  k_EXboxOrigin_Y = 3,
  k_EXboxOrigin_LeftBumper = 4,
  k_EXboxOrigin_RightBumper = 5,
  k_EXboxOrigin_Menu = 6,
  k_EXboxOrigin_View = 7,
  k_EXboxOrigin_LeftTrigger_Pull = 8,
  k_EXboxOrigin_LeftTrigger_Click = 9,
  k_EXboxOrigin_RightTrigger_Pull = 10,
  k_EXboxOrigin_RightTrigger_Click = 11,
  k_EXboxOrigin_LeftStick_Move = 12,
  k_EXboxOrigin_LeftStick_Click = 13,
  k_EXboxOrigin_LeftStick_DPadNorth = 14,
  k_EXboxOrigin_LeftStick_DPadSouth = 15,
  k_EXboxOrigin_LeftStick_DPadWest = 16,
  k_EXboxOrigin_LeftStick_DPadEast = 17,
  k_EXboxOrigin_RightStick_Move = 18,
  k_EXboxOrigin_RightStick_Click = 19,
  k_EXboxOrigin_RightStick_DPadNorth = 20,
  k_EXboxOrigin_RightStick_DPadSouth = 21,
  k_EXboxOrigin_RightStick_DPadWest = 22,
  k_EXboxOrigin_RightStick_DPadEast = 23,
  k_EXboxOrigin_DPad_North = 24,
  k_EXboxOrigin_DPad_South = 25,
  k_EXboxOrigin_DPad_West = 26,
  k_EXboxOrigin_DPad_East = 27,
  k_EXboxOrigin_Count = 28
);
ESteamControllerPad = (
  k_ESteamControllerPad_Left = 0,
  k_ESteamControllerPad_Right = 1
);
ESteamInputType = (
  k_ESteamInputType_Unknown = 0,
  k_ESteamInputType_SteamController = 1,
  k_ESteamInputType_XBox360Controller = 2,
  k_ESteamInputType_XBoxOneController = 3,
  k_ESteamInputType_GenericGamepad = 4,
  k_ESteamInputType_PS4Controller = 5,
  k_ESteamInputType_AppleMFiController = 6,
  k_ESteamInputType_AndroidController = 7,
  k_ESteamInputType_SwitchJoyConPair = 8,
  k_ESteamInputType_SwitchJoyConSingle = 9,
  k_ESteamInputType_SwitchProController = 10,
  k_ESteamInputType_MobileTouch = 11,
  k_ESteamInputType_PS3Controller = 12,
  k_ESteamInputType_Count = 13,
  k_ESteamInputType_MaximumPossibleValue = 255
);
ESteamInputLEDFlag = (
  k_ESteamInputLEDFlag_SetColor = 0,
  k_ESteamInputLEDFlag_RestoreUserDefault = 1
);
EControllerActionOrigin = (
  k_EControllerActionOrigin_None = 0,
  k_EControllerActionOrigin_A = 1,
  k_EControllerActionOrigin_B = 2,
  k_EControllerActionOrigin_X = 3,
  k_EControllerActionOrigin_Y = 4,
  k_EControllerActionOrigin_LeftBumper = 5,
  k_EControllerActionOrigin_RightBumper = 6,
  k_EControllerActionOrigin_LeftGrip = 7,
  k_EControllerActionOrigin_RightGrip = 8,
  k_EControllerActionOrigin_Start = 9,
  k_EControllerActionOrigin_Back = 10,
  k_EControllerActionOrigin_LeftPad_Touch = 11,
  k_EControllerActionOrigin_LeftPad_Swipe = 12,
  k_EControllerActionOrigin_LeftPad_Click = 13,
  k_EControllerActionOrigin_LeftPad_DPadNorth = 14,
  k_EControllerActionOrigin_LeftPad_DPadSouth = 15,
  k_EControllerActionOrigin_LeftPad_DPadWest = 16,
  k_EControllerActionOrigin_LeftPad_DPadEast = 17,
  k_EControllerActionOrigin_RightPad_Touch = 18,
  k_EControllerActionOrigin_RightPad_Swipe = 19,
  k_EControllerActionOrigin_RightPad_Click = 20,
  k_EControllerActionOrigin_RightPad_DPadNorth = 21,
  k_EControllerActionOrigin_RightPad_DPadSouth = 22,
  k_EControllerActionOrigin_RightPad_DPadWest = 23,
  k_EControllerActionOrigin_RightPad_DPadEast = 24,
  k_EControllerActionOrigin_LeftTrigger_Pull = 25,
  k_EControllerActionOrigin_LeftTrigger_Click = 26,
  k_EControllerActionOrigin_RightTrigger_Pull = 27,
  k_EControllerActionOrigin_RightTrigger_Click = 28,
  k_EControllerActionOrigin_LeftStick_Move = 29,
  k_EControllerActionOrigin_LeftStick_Click = 30,
  k_EControllerActionOrigin_LeftStick_DPadNorth = 31,
  k_EControllerActionOrigin_LeftStick_DPadSouth = 32,
  k_EControllerActionOrigin_LeftStick_DPadWest = 33,
  k_EControllerActionOrigin_LeftStick_DPadEast = 34,
  k_EControllerActionOrigin_Gyro_Move = 35,
  k_EControllerActionOrigin_Gyro_Pitch = 36,
  k_EControllerActionOrigin_Gyro_Yaw = 37,
  k_EControllerActionOrigin_Gyro_Roll = 38,
  k_EControllerActionOrigin_PS4_X = 39,
  k_EControllerActionOrigin_PS4_Circle = 40,
  k_EControllerActionOrigin_PS4_Triangle = 41,
  k_EControllerActionOrigin_PS4_Square = 42,
  k_EControllerActionOrigin_PS4_LeftBumper = 43,
  k_EControllerActionOrigin_PS4_RightBumper = 44,
  k_EControllerActionOrigin_PS4_Options = 45,
  k_EControllerActionOrigin_PS4_Share = 46,
  k_EControllerActionOrigin_PS4_LeftPad_Touch = 47,
  k_EControllerActionOrigin_PS4_LeftPad_Swipe = 48,
  k_EControllerActionOrigin_PS4_LeftPad_Click = 49,
  k_EControllerActionOrigin_PS4_LeftPad_DPadNorth = 50,
  k_EControllerActionOrigin_PS4_LeftPad_DPadSouth = 51,
  k_EControllerActionOrigin_PS4_LeftPad_DPadWest = 52,
  k_EControllerActionOrigin_PS4_LeftPad_DPadEast = 53,
  k_EControllerActionOrigin_PS4_RightPad_Touch = 54,
  k_EControllerActionOrigin_PS4_RightPad_Swipe = 55,
  k_EControllerActionOrigin_PS4_RightPad_Click = 56,
  k_EControllerActionOrigin_PS4_RightPad_DPadNorth = 57,
  k_EControllerActionOrigin_PS4_RightPad_DPadSouth = 58,
  k_EControllerActionOrigin_PS4_RightPad_DPadWest = 59,
  k_EControllerActionOrigin_PS4_RightPad_DPadEast = 60,
  k_EControllerActionOrigin_PS4_CenterPad_Touch = 61,
  k_EControllerActionOrigin_PS4_CenterPad_Swipe = 62,
  k_EControllerActionOrigin_PS4_CenterPad_Click = 63,
  k_EControllerActionOrigin_PS4_CenterPad_DPadNorth = 64,
  k_EControllerActionOrigin_PS4_CenterPad_DPadSouth = 65,
  k_EControllerActionOrigin_PS4_CenterPad_DPadWest = 66,
  k_EControllerActionOrigin_PS4_CenterPad_DPadEast = 67,
  k_EControllerActionOrigin_PS4_LeftTrigger_Pull = 68,
  k_EControllerActionOrigin_PS4_LeftTrigger_Click = 69,
  k_EControllerActionOrigin_PS4_RightTrigger_Pull = 70,
  k_EControllerActionOrigin_PS4_RightTrigger_Click = 71,
  k_EControllerActionOrigin_PS4_LeftStick_Move = 72,
  k_EControllerActionOrigin_PS4_LeftStick_Click = 73,
  k_EControllerActionOrigin_PS4_LeftStick_DPadNorth = 74,
  k_EControllerActionOrigin_PS4_LeftStick_DPadSouth = 75,
  k_EControllerActionOrigin_PS4_LeftStick_DPadWest = 76,
  k_EControllerActionOrigin_PS4_LeftStick_DPadEast = 77,
  k_EControllerActionOrigin_PS4_RightStick_Move = 78,
  k_EControllerActionOrigin_PS4_RightStick_Click = 79,
  k_EControllerActionOrigin_PS4_RightStick_DPadNorth = 80,
  k_EControllerActionOrigin_PS4_RightStick_DPadSouth = 81,
  k_EControllerActionOrigin_PS4_RightStick_DPadWest = 82,
  k_EControllerActionOrigin_PS4_RightStick_DPadEast = 83,
  k_EControllerActionOrigin_PS4_DPad_North = 84,
  k_EControllerActionOrigin_PS4_DPad_South = 85,
  k_EControllerActionOrigin_PS4_DPad_West = 86,
  k_EControllerActionOrigin_PS4_DPad_East = 87,
  k_EControllerActionOrigin_PS4_Gyro_Move = 88,
  k_EControllerActionOrigin_PS4_Gyro_Pitch = 89,
  k_EControllerActionOrigin_PS4_Gyro_Yaw = 90,
  k_EControllerActionOrigin_PS4_Gyro_Roll = 91,
  k_EControllerActionOrigin_XBoxOne_A = 92,
  k_EControllerActionOrigin_XBoxOne_B = 93,
  k_EControllerActionOrigin_XBoxOne_X = 94,
  k_EControllerActionOrigin_XBoxOne_Y = 95,
  k_EControllerActionOrigin_XBoxOne_LeftBumper = 96,
  k_EControllerActionOrigin_XBoxOne_RightBumper = 97,
  k_EControllerActionOrigin_XBoxOne_Menu = 98,
  k_EControllerActionOrigin_XBoxOne_View = 99,
  k_EControllerActionOrigin_XBoxOne_LeftTrigger_Pull = 100,
  k_EControllerActionOrigin_XBoxOne_LeftTrigger_Click = 101,
  k_EControllerActionOrigin_XBoxOne_RightTrigger_Pull = 102,
  k_EControllerActionOrigin_XBoxOne_RightTrigger_Click = 103,
  k_EControllerActionOrigin_XBoxOne_LeftStick_Move = 104,
  k_EControllerActionOrigin_XBoxOne_LeftStick_Click = 105,
  k_EControllerActionOrigin_XBoxOne_LeftStick_DPadNorth = 106,
  k_EControllerActionOrigin_XBoxOne_LeftStick_DPadSouth = 107,
  k_EControllerActionOrigin_XBoxOne_LeftStick_DPadWest = 108,
  k_EControllerActionOrigin_XBoxOne_LeftStick_DPadEast = 109,
  k_EControllerActionOrigin_XBoxOne_RightStick_Move = 110,
  k_EControllerActionOrigin_XBoxOne_RightStick_Click = 111,
  k_EControllerActionOrigin_XBoxOne_RightStick_DPadNorth = 112,
  k_EControllerActionOrigin_XBoxOne_RightStick_DPadSouth = 113,
  k_EControllerActionOrigin_XBoxOne_RightStick_DPadWest = 114,
  k_EControllerActionOrigin_XBoxOne_RightStick_DPadEast = 115,
  k_EControllerActionOrigin_XBoxOne_DPad_North = 116,
  k_EControllerActionOrigin_XBoxOne_DPad_South = 117,
  k_EControllerActionOrigin_XBoxOne_DPad_West = 118,
  k_EControllerActionOrigin_XBoxOne_DPad_East = 119,
  k_EControllerActionOrigin_XBox360_A = 120,
  k_EControllerActionOrigin_XBox360_B = 121,
  k_EControllerActionOrigin_XBox360_X = 122,
  k_EControllerActionOrigin_XBox360_Y = 123,
  k_EControllerActionOrigin_XBox360_LeftBumper = 124,
  k_EControllerActionOrigin_XBox360_RightBumper = 125,
  k_EControllerActionOrigin_XBox360_Start = 126,
  k_EControllerActionOrigin_XBox360_Back = 127,
  k_EControllerActionOrigin_XBox360_LeftTrigger_Pull = 128,
  k_EControllerActionOrigin_XBox360_LeftTrigger_Click = 129,
  k_EControllerActionOrigin_XBox360_RightTrigger_Pull = 130,
  k_EControllerActionOrigin_XBox360_RightTrigger_Click = 131,
  k_EControllerActionOrigin_XBox360_LeftStick_Move = 132,
  k_EControllerActionOrigin_XBox360_LeftStick_Click = 133,
  k_EControllerActionOrigin_XBox360_LeftStick_DPadNorth = 134,
  k_EControllerActionOrigin_XBox360_LeftStick_DPadSouth = 135,
  k_EControllerActionOrigin_XBox360_LeftStick_DPadWest = 136,
  k_EControllerActionOrigin_XBox360_LeftStick_DPadEast = 137,
  k_EControllerActionOrigin_XBox360_RightStick_Move = 138,
  k_EControllerActionOrigin_XBox360_RightStick_Click = 139,
  k_EControllerActionOrigin_XBox360_RightStick_DPadNorth = 140,
  k_EControllerActionOrigin_XBox360_RightStick_DPadSouth = 141,
  k_EControllerActionOrigin_XBox360_RightStick_DPadWest = 142,
  k_EControllerActionOrigin_XBox360_RightStick_DPadEast = 143,
  k_EControllerActionOrigin_XBox360_DPad_North = 144,
  k_EControllerActionOrigin_XBox360_DPad_South = 145,
  k_EControllerActionOrigin_XBox360_DPad_West = 146,
  k_EControllerActionOrigin_XBox360_DPad_East = 147,
  k_EControllerActionOrigin_SteamV2_A = 148,
  k_EControllerActionOrigin_SteamV2_B = 149,
  k_EControllerActionOrigin_SteamV2_X = 150,
  k_EControllerActionOrigin_SteamV2_Y = 151,
  k_EControllerActionOrigin_SteamV2_LeftBumper = 152,
  k_EControllerActionOrigin_SteamV2_RightBumper = 153,
  k_EControllerActionOrigin_SteamV2_LeftGrip_Lower = 154,
  k_EControllerActionOrigin_SteamV2_LeftGrip_Upper = 155,
  k_EControllerActionOrigin_SteamV2_RightGrip_Lower = 156,
  k_EControllerActionOrigin_SteamV2_RightGrip_Upper = 157,
  k_EControllerActionOrigin_SteamV2_LeftBumper_Pressure = 158,
  k_EControllerActionOrigin_SteamV2_RightBumper_Pressure = 159,
  k_EControllerActionOrigin_SteamV2_LeftGrip_Pressure = 160,
  k_EControllerActionOrigin_SteamV2_RightGrip_Pressure = 161,
  k_EControllerActionOrigin_SteamV2_LeftGrip_Upper_Pressure = 162,
  k_EControllerActionOrigin_SteamV2_RightGrip_Upper_Pressure = 163,
  k_EControllerActionOrigin_SteamV2_Start = 164,
  k_EControllerActionOrigin_SteamV2_Back = 165,
  k_EControllerActionOrigin_SteamV2_LeftPad_Touch = 166,
  k_EControllerActionOrigin_SteamV2_LeftPad_Swipe = 167,
  k_EControllerActionOrigin_SteamV2_LeftPad_Click = 168,
  k_EControllerActionOrigin_SteamV2_LeftPad_Pressure = 169,
  k_EControllerActionOrigin_SteamV2_LeftPad_DPadNorth = 170,
  k_EControllerActionOrigin_SteamV2_LeftPad_DPadSouth = 171,
  k_EControllerActionOrigin_SteamV2_LeftPad_DPadWest = 172,
  k_EControllerActionOrigin_SteamV2_LeftPad_DPadEast = 173,
  k_EControllerActionOrigin_SteamV2_RightPad_Touch = 174,
  k_EControllerActionOrigin_SteamV2_RightPad_Swipe = 175,
  k_EControllerActionOrigin_SteamV2_RightPad_Click = 176,
  k_EControllerActionOrigin_SteamV2_RightPad_Pressure = 177,
  k_EControllerActionOrigin_SteamV2_RightPad_DPadNorth = 178,
  k_EControllerActionOrigin_SteamV2_RightPad_DPadSouth = 179,
  k_EControllerActionOrigin_SteamV2_RightPad_DPadWest = 180,
  k_EControllerActionOrigin_SteamV2_RightPad_DPadEast = 181,
  k_EControllerActionOrigin_SteamV2_LeftTrigger_Pull = 182,
  k_EControllerActionOrigin_SteamV2_LeftTrigger_Click = 183,
  k_EControllerActionOrigin_SteamV2_RightTrigger_Pull = 184,
  k_EControllerActionOrigin_SteamV2_RightTrigger_Click = 185,
  k_EControllerActionOrigin_SteamV2_LeftStick_Move = 186,
  k_EControllerActionOrigin_SteamV2_LeftStick_Click = 187,
  k_EControllerActionOrigin_SteamV2_LeftStick_DPadNorth = 188,
  k_EControllerActionOrigin_SteamV2_LeftStick_DPadSouth = 189,
  k_EControllerActionOrigin_SteamV2_LeftStick_DPadWest = 190,
  k_EControllerActionOrigin_SteamV2_LeftStick_DPadEast = 191,
  k_EControllerActionOrigin_SteamV2_Gyro_Move = 192,
  k_EControllerActionOrigin_SteamV2_Gyro_Pitch = 193,
  k_EControllerActionOrigin_SteamV2_Gyro_Yaw = 194,
  k_EControllerActionOrigin_SteamV2_Gyro_Roll = 195,
  k_EControllerActionOrigin_Switch_A = 196,
  k_EControllerActionOrigin_Switch_B = 197,
  k_EControllerActionOrigin_Switch_X = 198,
  k_EControllerActionOrigin_Switch_Y = 199,
  k_EControllerActionOrigin_Switch_LeftBumper = 200,
  k_EControllerActionOrigin_Switch_RightBumper = 201,
  k_EControllerActionOrigin_Switch_Plus = 202,
  k_EControllerActionOrigin_Switch_Minus = 203,
  k_EControllerActionOrigin_Switch_Capture = 204,
  k_EControllerActionOrigin_Switch_LeftTrigger_Pull = 205,
  k_EControllerActionOrigin_Switch_LeftTrigger_Click = 206,
  k_EControllerActionOrigin_Switch_RightTrigger_Pull = 207,
  k_EControllerActionOrigin_Switch_RightTrigger_Click = 208,
  k_EControllerActionOrigin_Switch_LeftStick_Move = 209,
  k_EControllerActionOrigin_Switch_LeftStick_Click = 210,
  k_EControllerActionOrigin_Switch_LeftStick_DPadNorth = 211,
  k_EControllerActionOrigin_Switch_LeftStick_DPadSouth = 212,
  k_EControllerActionOrigin_Switch_LeftStick_DPadWest = 213,
  k_EControllerActionOrigin_Switch_LeftStick_DPadEast = 214,
  k_EControllerActionOrigin_Switch_RightStick_Move = 215,
  k_EControllerActionOrigin_Switch_RightStick_Click = 216,
  k_EControllerActionOrigin_Switch_RightStick_DPadNorth = 217,
  k_EControllerActionOrigin_Switch_RightStick_DPadSouth = 218,
  k_EControllerActionOrigin_Switch_RightStick_DPadWest = 219,
  k_EControllerActionOrigin_Switch_RightStick_DPadEast = 220,
  k_EControllerActionOrigin_Switch_DPad_North = 221,
  k_EControllerActionOrigin_Switch_DPad_South = 222,
  k_EControllerActionOrigin_Switch_DPad_West = 223,
  k_EControllerActionOrigin_Switch_DPad_East = 224,
  k_EControllerActionOrigin_Switch_ProGyro_Move = 225,
  k_EControllerActionOrigin_Switch_ProGyro_Pitch = 226,
  k_EControllerActionOrigin_Switch_ProGyro_Yaw = 227,
  k_EControllerActionOrigin_Switch_ProGyro_Roll = 228,
  k_EControllerActionOrigin_Switch_RightGyro_Move = 229,
  k_EControllerActionOrigin_Switch_RightGyro_Pitch = 230,
  k_EControllerActionOrigin_Switch_RightGyro_Yaw = 231,
  k_EControllerActionOrigin_Switch_RightGyro_Roll = 232,
  k_EControllerActionOrigin_Switch_LeftGyro_Move = 233,
  k_EControllerActionOrigin_Switch_LeftGyro_Pitch = 234,
  k_EControllerActionOrigin_Switch_LeftGyro_Yaw = 235,
  k_EControllerActionOrigin_Switch_LeftGyro_Roll = 236,
  k_EControllerActionOrigin_Switch_LeftGrip_Lower = 237,
  k_EControllerActionOrigin_Switch_LeftGrip_Upper = 238,
  k_EControllerActionOrigin_Switch_RightGrip_Lower = 239,
  k_EControllerActionOrigin_Switch_RightGrip_Upper = 240,
  k_EControllerActionOrigin_PS4_DPad_Move = 241,
  k_EControllerActionOrigin_XBoxOne_DPad_Move = 242,
  k_EControllerActionOrigin_XBox360_DPad_Move = 243,
  k_EControllerActionOrigin_Switch_DPad_Move = 244,
  k_EControllerActionOrigin_Count = 245,
  k_EControllerActionOrigin_MaximumPossibleValue = 32767
);
ESteamControllerLEDFlag = (
  k_ESteamControllerLEDFlag_SetColor = 0,
  k_ESteamControllerLEDFlag_RestoreUserDefault = 1
);
EUGCMatchingUGCType = (
  k_EUGCMatchingUGCType_Items = 0,
  k_EUGCMatchingUGCType_Items_Mtx = 1,
  k_EUGCMatchingUGCType_Items_ReadyToUse = 2,
  k_EUGCMatchingUGCType_Collections = 3,
  k_EUGCMatchingUGCType_Artwork = 4,
  k_EUGCMatchingUGCType_Videos = 5,
  k_EUGCMatchingUGCType_Screenshots = 6,
  k_EUGCMatchingUGCType_AllGuides = 7,
  k_EUGCMatchingUGCType_WebGuides = 8,
  k_EUGCMatchingUGCType_IntegratedGuides = 9,
  k_EUGCMatchingUGCType_UsableInGame = 10,
  k_EUGCMatchingUGCType_ControllerBindings = 11,
  k_EUGCMatchingUGCType_GameManagedItems = 12,
  k_EUGCMatchingUGCType_All = -1
);
EUserUGCList = (
  k_EUserUGCList_Published = 0,
  k_EUserUGCList_VotedOn = 1,
  k_EUserUGCList_VotedUp = 2,
  k_EUserUGCList_VotedDown = 3,
  k_EUserUGCList_WillVoteLater = 4,
  k_EUserUGCList_Favorited = 5,
  k_EUserUGCList_Subscribed = 6,
  k_EUserUGCList_UsedOrPlayed = 7,
  k_EUserUGCList_Followed = 8
);
EUserUGCListSortOrder = (
  k_EUserUGCListSortOrder_CreationOrderDesc = 0,
  k_EUserUGCListSortOrder_CreationOrderAsc = 1,
  k_EUserUGCListSortOrder_TitleAsc = 2,
  k_EUserUGCListSortOrder_LastUpdatedDesc = 3,
  k_EUserUGCListSortOrder_SubscriptionDateDesc = 4,
  k_EUserUGCListSortOrder_VoteScoreDesc = 5,
  k_EUserUGCListSortOrder_ForModeration = 6
);
EUGCQuery = (
  k_EUGCQuery_RankedByVote = 0,
  k_EUGCQuery_RankedByPublicationDate = 1,
  k_EUGCQuery_AcceptedForGameRankedByAcceptanceDate = 2,
  k_EUGCQuery_RankedByTrend = 3,
  k_EUGCQuery_FavoritedByFriendsRankedByPublicationDate = 4,
  k_EUGCQuery_CreatedByFriendsRankedByPublicationDate = 5,
  k_EUGCQuery_RankedByNumTimesReported = 6,
  k_EUGCQuery_CreatedByFollowedUsersRankedByPublicationDate = 7,
  k_EUGCQuery_NotYetRated = 8,
  k_EUGCQuery_RankedByTotalVotesAsc = 9,
  k_EUGCQuery_RankedByVotesUp = 10,
  k_EUGCQuery_RankedByTextSearch = 11,
  k_EUGCQuery_RankedByTotalUniqueSubscriptions = 12,
  k_EUGCQuery_RankedByPlaytimeTrend = 13,
  k_EUGCQuery_RankedByTotalPlaytime = 14,
  k_EUGCQuery_RankedByAveragePlaytimeTrend = 15,
  k_EUGCQuery_RankedByLifetimeAveragePlaytime = 16,
  k_EUGCQuery_RankedByPlaytimeSessionsTrend = 17,
  k_EUGCQuery_RankedByLifetimePlaytimeSessions = 18
);
EItemUpdateStatus = (
  k_EItemUpdateStatusInvalid = 0,
  k_EItemUpdateStatusPreparingConfig = 1,
  k_EItemUpdateStatusPreparingContent = 2,
  k_EItemUpdateStatusUploadingContent = 3,
  k_EItemUpdateStatusUploadingPreviewFile = 4,
  k_EItemUpdateStatusCommittingChanges = 5
);
EItemState = (
  k_EItemStateNone = 0,
  k_EItemStateSubscribed = 1,
  k_EItemStateLegacyItem = 2,
  k_EItemStateInstalled = 4,
  k_EItemStateNeedsUpdate = 8,
  k_EItemStateDownloading = 16,
  k_EItemStateDownloadPending = 32
);
EItemStatistic = (
  k_EItemStatistic_NumSubscriptions = 0,
  k_EItemStatistic_NumFavorites = 1,
  k_EItemStatistic_NumFollowers = 2,
  k_EItemStatistic_NumUniqueSubscriptions = 3,
  k_EItemStatistic_NumUniqueFavorites = 4,
  k_EItemStatistic_NumUniqueFollowers = 5,
  k_EItemStatistic_NumUniqueWebsiteViews = 6,
  k_EItemStatistic_ReportScore = 7,
  k_EItemStatistic_NumSecondsPlayed = 8,
  k_EItemStatistic_NumPlaytimeSessions = 9,
  k_EItemStatistic_NumComments = 10,
  k_EItemStatistic_NumSecondsPlayedDuringTimePeriod = 11,
  k_EItemStatistic_NumPlaytimeSessionsDuringTimePeriod = 12
);
EItemPreviewType = (
  k_EItemPreviewType_Image = 0,
  k_EItemPreviewType_YouTubeVideo = 1,
  k_EItemPreviewType_Sketchfab = 2,
  k_EItemPreviewType_EnvironmentMap_HorizontalCross = 3,
  k_EItemPreviewType_EnvironmentMap_LatLong = 4,
  k_EItemPreviewType_ReservedMax = 255
);
ESteamItemFlags = (
  k_ESteamItemNoTrade = 1,
  k_ESteamItemRemoved = 256,
  k_ESteamItemConsumed = 512
);
ESteamTVRegionBehavior = (
  k_ESteamVideoRegionBehaviorInvalid = -1,
  k_ESteamVideoRegionBehaviorHover = 0,
  k_ESteamVideoRegionBehaviorClickPopup = 1,
  k_ESteamVideoRegionBehaviorClickSurroundingRegion = 2
);
EParentalFeature = (
  k_EFeatureInvalid = 0,
  k_EFeatureStore = 1,
  k_EFeatureCommunity = 2,
  k_EFeatureProfile = 3,
  k_EFeatureFriends = 4,
  k_EFeatureNews = 5,
  k_EFeatureTrading = 6,
  k_EFeatureSettings = 7,
  k_EFeatureConsole = 8,
  k_EFeatureBrowser = 9,
  k_EFeatureParentalSetup = 10,
  k_EFeatureLibrary = 11,
  k_EFeatureTest = 12,
  k_EFeatureSiteLicense = 13,
  k_EFeatureMax = 14
);
ESteamDeviceFormFactor = (
  k_ESteamDeviceFormFactorUnknown = 0,
  k_ESteamDeviceFormFactorPhone = 1,
  k_ESteamDeviceFormFactorTablet = 2,
  k_ESteamDeviceFormFactorComputer = 3,
  k_ESteamDeviceFormFactorTV = 4
);
{
ESteamNetworkingAvailability = (
  k_ESteamNetworkingAvailability_CannotTry = -102,
  k_ESteamNetworkingAvailability_Failed = -101,
  k_ESteamNetworkingAvailability_Previously = -100,
  k_ESteamNetworkingAvailability_Retrying = -10,
  k_ESteamNetworkingAvailability_NeverTried = 1,
  k_ESteamNetworkingAvailability_Waiting = 2,
  k_ESteamNetworkingAvailability_Attempting = 3,
  k_ESteamNetworkingAvailability_Current = 100,
  k_ESteamNetworkingAvailability_Unknown = 0,
  k_ESteamNetworkingAvailability__Force32bit = 2147483647
);
ESteamNetworkingIdentityType = (
  k_ESteamNetworkingIdentityType_Invalid = 0,
  k_ESteamNetworkingIdentityType_SteamID = 16,
  k_ESteamNetworkingIdentityType_XboxPairwiseID = 17,
  k_ESteamNetworkingIdentityType_IPAddress = 1,
  k_ESteamNetworkingIdentityType_GenericString = 2,
  k_ESteamNetworkingIdentityType_GenericBytes = 3,
  k_ESteamNetworkingIdentityType_UnknownType = 4,
  k_ESteamNetworkingIdentityType__Force32bit = 2147483647
);
ESteamNetworkingConnectionState = (
  k_ESteamNetworkingConnectionState_None = 0,
  k_ESteamNetworkingConnectionState_Connecting = 1,
  k_ESteamNetworkingConnectionState_FindingRoute = 2,
  k_ESteamNetworkingConnectionState_Connected = 3,
  k_ESteamNetworkingConnectionState_ClosedByPeer = 4,
  k_ESteamNetworkingConnectionState_ProblemDetectedLocally = 5,
  k_ESteamNetworkingConnectionState_FinWait = -1,
  k_ESteamNetworkingConnectionState_Linger = -2,
  k_ESteamNetworkingConnectionState_Dead = -3,
  k_ESteamNetworkingConnectionState__Force32Bit = 2147483647
);
ESteamNetConnectionEnd = (
  k_ESteamNetConnectionEnd_Invalid = 0,
  k_ESteamNetConnectionEnd_App_Min = 1000,
  k_ESteamNetConnectionEnd_App_Generic = 1000,
  k_ESteamNetConnectionEnd_App_Max = 1999,
  k_ESteamNetConnectionEnd_AppException_Min = 2000,
  k_ESteamNetConnectionEnd_AppException_Generic = 2000,
  k_ESteamNetConnectionEnd_AppException_Max = 2999,
  k_ESteamNetConnectionEnd_Local_Min = 3000,
  k_ESteamNetConnectionEnd_Local_OfflineMode = 3001,
  k_ESteamNetConnectionEnd_Local_ManyRelayConnectivity = 3002,
  k_ESteamNetConnectionEnd_Local_HostedServerPrimaryRelay = 3003,
  k_ESteamNetConnectionEnd_Local_NetworkConfig = 3004,
  k_ESteamNetConnectionEnd_Local_Rights = 3005,
  k_ESteamNetConnectionEnd_Local_Max = 3999,
  k_ESteamNetConnectionEnd_Remote_Min = 4000,
  k_ESteamNetConnectionEnd_Remote_Timeout = 4001,
  k_ESteamNetConnectionEnd_Remote_BadCrypt = 4002,
  k_ESteamNetConnectionEnd_Remote_BadCert = 4003,
  k_ESteamNetConnectionEnd_Remote_NotLoggedIn = 4004,
  k_ESteamNetConnectionEnd_Remote_NotRunningApp = 4005,
  k_ESteamNetConnectionEnd_Remote_BadProtocolVersion = 4006,
  k_ESteamNetConnectionEnd_Remote_Max = 4999,
  k_ESteamNetConnectionEnd_Misc_Min = 5000,
  k_ESteamNetConnectionEnd_Misc_Generic = 5001,
  k_ESteamNetConnectionEnd_Misc_InternalError = 5002,
  k_ESteamNetConnectionEnd_Misc_Timeout = 5003,
  k_ESteamNetConnectionEnd_Misc_RelayConnectivity = 5004,
  k_ESteamNetConnectionEnd_Misc_SteamConnectivity = 5005,
  k_ESteamNetConnectionEnd_Misc_NoRelaySessionsToClient = 5006,
  k_ESteamNetConnectionEnd_Misc_Max = 5999,
  k_ESteamNetConnectionEnd__Force32Bit = 2147483647
);
ESteamNetworkingConfigScope = (
  k_ESteamNetworkingConfig_Global = 1,
  k_ESteamNetworkingConfig_SocketsInterface = 2,
  k_ESteamNetworkingConfig_ListenSocket = 3,
  k_ESteamNetworkingConfig_Connection = 4,
  k_ESteamNetworkingConfigScope__Force32Bit = 2147483647
);
pESteamNetworkingConfigScope = ^ESteamNetworkingConfigScope;
ESteamNetworkingConfigDataType = (
  k_ESteamNetworkingConfig_Int32 = 1,
  k_ESteamNetworkingConfig_Int64 = 2,
  k_ESteamNetworkingConfig_Float = 3,
  k_ESteamNetworkingConfig_String = 4,
  k_ESteamNetworkingConfig_FunctionPtr = 5,
  k_ESteamNetworkingConfigDataType__Force32Bit = 2147483647
);
pESteamNetworkingConfigDataType = ^ESteamNetworkingConfigDataType;
ESteamNetworkingConfigValue = (
  k_ESteamNetworkingConfig_Invalid = 0,
  k_ESteamNetworkingConfig_FakePacketLoss_Send = 2,
  k_ESteamNetworkingConfig_FakePacketLoss_Recv = 3,
  k_ESteamNetworkingConfig_FakePacketLag_Send = 4,
  k_ESteamNetworkingConfig_FakePacketLag_Recv = 5,
  k_ESteamNetworkingConfig_FakePacketReorder_Send = 6,
  k_ESteamNetworkingConfig_FakePacketReorder_Recv = 7,
  k_ESteamNetworkingConfig_FakePacketReorder_Time = 8,
  k_ESteamNetworkingConfig_FakePacketDup_Send = 26,
  k_ESteamNetworkingConfig_FakePacketDup_Recv = 27,
  k_ESteamNetworkingConfig_FakePacketDup_TimeMax = 28,
  k_ESteamNetworkingConfig_TimeoutInitial = 24,
  k_ESteamNetworkingConfig_TimeoutConnected = 25,
  k_ESteamNetworkingConfig_SendBufferSize = 9,
  k_ESteamNetworkingConfig_SendRateMin = 10,
  k_ESteamNetworkingConfig_SendRateMax = 11,
  k_ESteamNetworkingConfig_NagleTime = 12,
  k_ESteamNetworkingConfig_IP_AllowWithoutAuth = 23,
  k_ESteamNetworkingConfig_MTU_PacketSize = 32,
  k_ESteamNetworkingConfig_MTU_DataSize = 33,
  k_ESteamNetworkingConfig_Unencrypted = 34,
  k_ESteamNetworkingConfig_EnumerateDevVars = 35,
  k_ESteamNetworkingConfig_SDRClient_ConsecutitivePingTimeoutsFailInitial = 19,
  k_ESteamNetworkingConfig_SDRClient_ConsecutitivePingTimeoutsFail = 20,
  k_ESteamNetworkingConfig_SDRClient_MinPingsBeforePingAccurate = 21,
  k_ESteamNetworkingConfig_SDRClient_SingleSocket = 22,
  k_ESteamNetworkingConfig_SDRClient_ForceRelayCluster = 29,
  k_ESteamNetworkingConfig_SDRClient_DebugTicketAddress = 30,
  k_ESteamNetworkingConfig_SDRClient_ForceProxyAddr = 31,
  k_ESteamNetworkingConfig_SDRClient_FakeClusterPing = 36,
  k_ESteamNetworkingConfig_LogLevel_AckRTT = 13,
  k_ESteamNetworkingConfig_LogLevel_PacketDecode = 14,
  k_ESteamNetworkingConfig_LogLevel_Message = 15,
  k_ESteamNetworkingConfig_LogLevel_PacketGaps = 16,
  k_ESteamNetworkingConfig_LogLevel_P2PRendezvous = 17,
  k_ESteamNetworkingConfig_LogLevel_SDRRelayPings = 18,
  k_ESteamNetworkingConfigValue__Force32Bit = 2147483647
);
pESteamNetworkingConfigValue = ^ESteamNetworkingConfigValue;
ESteamNetworkingGetConfigValueResult = (
  k_ESteamNetworkingGetConfigValue_BadValue = -1,
  k_ESteamNetworkingGetConfigValue_BadScopeObj = -2,
  k_ESteamNetworkingGetConfigValue_BufferTooSmall = -3,
  k_ESteamNetworkingGetConfigValue_OK = 1,
  k_ESteamNetworkingGetConfigValue_OKInherited = 2,
  k_ESteamNetworkingGetConfigValueResult__Force32Bit = 2147483647
);
ESteamNetworkingSocketsDebugOutputType = (
  k_ESteamNetworkingSocketsDebugOutputType_None = 0,
  k_ESteamNetworkingSocketsDebugOutputType_Bug = 1,
  k_ESteamNetworkingSocketsDebugOutputType_Error = 2,
  k_ESteamNetworkingSocketsDebugOutputType_Important = 3,
  k_ESteamNetworkingSocketsDebugOutputType_Warning = 4,
  k_ESteamNetworkingSocketsDebugOutputType_Msg = 5,
  k_ESteamNetworkingSocketsDebugOutputType_Verbose = 6,
  k_ESteamNetworkingSocketsDebugOutputType_Debug = 7,
  k_ESteamNetworkingSocketsDebugOutputType_Everything = 8,
  k_ESteamNetworkingSocketsDebugOutputType__Force32Bit = 2147483647
);
}
EServerMode = (
  eServerModeInvalid = 0,
  eServerModeNoAuthentication = 1,
  eServerModeAuthentication = 2,
  eServerModeAuthenticationAndSecure = 3
);

  // return type of GetAuthSessionTicket
type
  TAuthTicket = uint32;

const
  k_HAuthTicketInvalid: TAuthTicket = 0;
  k_ECallbackFlagsRegistered = $01;
  k_ECallbackFlagsGameServer = $02;

type
  TLegacyKeyRegistration = procedure(CDKey, InstallPath: pAnsiChar);
  TLegacyKeyInstalled = procedure();

const
  k_unSteamAccountIDMask: NativeUInt = $FFFFFFFF;

const
  k_unSteamAccountInstanceMask: NativeUInt = $000FFFFF;

  // we allow 3 simultaneous user account instances right now, 1= desktop, 2 = console, 4 = web, 0 = all
const
  k_unSteamUserDesktopInstance: NativeUInt = 1;

const
  k_unSteamUserConsoleInstance: NativeUInt = 2;

const
  k_unSteamUserWebInstance: NativeUInt = 4;

  //const k_EMarketingMessageFlagsPlatformRestrictions = byte(k_EMarketingMessageFlagsPlatformWindows) OR byte(k_EMarketingMessageFlagsPlatformMac) Or byte(k_EMarketingMessageFlagsPlatformLinux);


  // -----------------------------------------------------------------------------
  // Purpose: Possible positions to tell the overlay to show notifications in
  // -----------------------------------------------------------------------------
Type

  TSteamAPIWarningMessageHook = procedure(a: integer; b: pAnsiChar); cdecl;
  TSteamAPI_PostAPIResultInProcess = procedure(callHandle: SteamAPICall_t; b: pointer; CallbackSize: uint32; CallbackNum: integer); cdecl;
  TSteamAPI_CheckCallbackRegistered = procedure(CallbackNum: integer); cdecl;

  TGameID = record
  private
    Data1: SmallInt; // 16 bit
    Data2: Byte; // 8 bit
    Data3: Byte; // 8 bit
    Data4: int32; // 32 bit
  public
    function GetAppID: integer;
    function GetType: integer;
    function GetModID: integer;
  end;

  TSteamID = record
  private
    case integer of
      0: (m_comp: record
            m_unAccountID: uint32;
            m_unAccountInstanceAndType: array[0..2] of uint8;
            m_EUniverse: EUniverse;
          end);
      1: (m_unAll64Bits: uint64);
  end;

  pSteamID = ^TSteamID;

  TSteamIDHelper = record helper for TSteamID
      function GetAccountID: uint32; // unique account identifier
      function GetAccountInstance: uint32; // dynamic instance ID
      function GeEAccountType: EAccountType; // type of account
      function GeEUniverse: EUniverse; // universe this account belongs to
      function GetAsString: AnsiString; // SteamID3 string format [U:1:ID]
    end;

SteamIPAddress_t = record
  m_rgubIPv6: array [0..15] of uint8;
  m_eType: ESteamIPType;
end;

pSteamIPAddress_t = ^SteamIPAddress_t;

FriendGameInfo_t = record
  m_gameID: uint64_gameid;
  m_unGameIP: uint32;
  m_usGamePort: uint16;
  m_usQueryPort: uint16;
  m_steamIDLobby: TSteamID;
end;

pFriendGameInfo_t = ^FriendGameInfo_t;

FriendSessionStateInfo_t = record
  m_uiOnlineSessionInstances: uint32;
  m_uiPublishedToFriendsSessionInstance: uint8;
end;

pFriendSessionStateInfo_t = ^FriendSessionStateInfo_t;

MatchMakingKeyValuePair_t = record
  m_szKey: array [0..255] of char;
  m_szValue: array [0..255] of char;
end;

pMatchMakingKeyValuePair_t = ^MatchMakingKeyValuePair_t;

servernetadr_t = record
  m_usConnectionPort: uint16;
  m_usQueryPort: uint16;
  m_unIP: uint32;
end;

pservernetadr_t = ^servernetadr_t;

gameserveritem_t = record
  m_NetAdr: servernetadr_t;
  m_nPing: Longint;
  m_bHadSuccessfulResponse: Boolean;
  m_bDoNotRefresh: Boolean;
  m_szGameDir: array [0..31] of char;
  m_szMap: array [0..31] of char;
  m_szGameDescription: array [0..63] of char;
  m_nAppID: uint32;
  m_nPlayers: Longint;
  m_nMaxPlayers: Longint;
  m_nBotPlayers: Longint;
  m_bPassword: Boolean;
  m_bSecure: Boolean;
  m_ulTimeLastPlayed: uint32;
  m_nServerVersion: Longint;
  m_szServerName: array [0..63] of char;
  m_szGameTags: array [0..127] of char;
  m_steamID: TSteamID;
end;

pgameserveritem_t = ^gameserveritem_t;

SteamPartyBeaconLocation_t = record
  m_eType: ESteamPartyBeaconLocationType;
  m_ulLocationID: uint64;
end;

pSteamPartyBeaconLocation_t = ^SteamPartyBeaconLocation_t;

SteamParamStringArray_t = record
  m_ppStrings: PPAnsiChar;
  m_nNumStrings: int32;
end;

pSteamParamStringArray_t = ^SteamParamStringArray_t;

LeaderboardEntry_t = record
  m_steamIDUser: TSteamID;
  m_nGlobalRank: int32;
  m_nScore: int32;
  m_cDetails: int32;
  m_hUGC: UGCHandle_t;
end;

pLeaderboardEntry_t = ^LeaderboardEntry_t;

P2PSessionState_t = record
  m_bConnectionActive: uint8;
  m_bConnecting: uint8;
  m_eP2PSessionError: uint8;
  m_bUsingRelay: uint8;
  m_nBytesQueuedForSend: int32;
  m_nPacketsQueuedForSend: int32;
  m_nRemoteIP: uint32;
  m_nRemotePort: uint16;
end;

pP2PSessionState_t = ^P2PSessionState_t;

InputAnalogActionData_t = record
  eMode: EInputSourceMode;
  x: Single;
  y: Single;
  bActive: Boolean;
end;

pInputAnalogActionData_t = ^InputAnalogActionData_t;

InputDigitalActionData_t = record
  bState: Boolean;
  bActive: Boolean;
end;

pInputDigitalActionData_t = ^InputDigitalActionData_t;

InputMotionData_t = record
  rotQuatX: Single;
  rotQuatY: Single;
  rotQuatZ: Single;
  rotQuatW: Single;
  posAccelX: Single;
  posAccelY: Single;
  posAccelZ: Single;
  rotVelX: Single;
  rotVelY: Single;
  rotVelZ: Single;
end;

pInputMotionData_t = ^InputMotionData_t;

SteamUGCDetails_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_eResult: EResult;
  m_eFileType: EWorkshopFileType;
  m_nCreatorAppID: AppId_t;
  m_nConsumerAppID: AppId_t;
  m_rgchTitle: array [0..128] of char;
  m_rgchDescription: array [0..7999] of char;
  m_ulSteamIDOwner: uint64;
  m_rtimeCreated: uint32;
  m_rtimeUpdated: uint32;
  m_rtimeAddedToUserList: uint32;
  m_eVisibility: ERemoteStoragePublishedFileVisibility;
  m_bBanned: Boolean;
  m_bAcceptedForUse: Boolean;
  m_bTagsTruncated: Boolean;
  m_rgchTags: array [0..1024] of char;
  m_hFile: UGCHandle_t;
  m_hPreviewFile: UGCHandle_t;
  m_pchFileName: array [0..259] of char;
  m_nFileSize: int32;
  m_nPreviewFileSize: int32;
  m_rgchURL: array [0..255] of char;
  m_unVotesUp: uint32;
  m_unVotesDown: uint32;
  m_flScore: Single;
  m_unNumChildren: uint32;
end;

pSteamUGCDetails_t = ^SteamUGCDetails_t;

SteamItemDetails_t = record
  m_itemId: SteamItemInstanceID_t;
  m_iDefinition: SteamItemDef_t;
  m_unQuantity: uint16;
  m_unFlags: uint16;
end;

pSteamItemDetails_t = ^SteamItemDetails_t;

SteamTVRegion_t = record
  unMinX: uint32;
  unMinY: uint32;
  unMaxX: uint32;
  unMaxY: uint32;
end;

pSteamTVRegion_t = ^SteamTVRegion_t;
{
SteamNetworkingIPAddr = record
  m_ipv6: array [0..15] of uint8;
  m_port: uint16;
end;
pSteamNetworkingIPAddr = ^SteamNetworkingIPAddr;


SteamNetworkingIdentity = record
  m_eType: ESteamNetworkingIdentityType;
  m_cbSize: Longint;
  m_szUnknownRawString: array [0..127] of char;
end;
pSteamNetworkingIdentity = ^SteamNetworkingIdentity;

SteamNetConnectionInfo_t = record
  m_identityRemote: SteamNetworkingIdentity;
  m_nUserData: int64;
  m_hListenSocket: HSteamListenSocket;
  m_addrRemote: SteamNetworkingIPAddr;
  m__pad1: uint16;
  m_idPOPRemote: SteamNetworkingPOPID;
  m_idPOPRelay: SteamNetworkingPOPID;
  m_eState: ESteamNetworkingConnectionState;
  m_eEndReason: Longint;
  m_szEndDebug: array [0..127] of char;
  m_szConnectionDescription: array [0..127] of char;
  reserved: array [0..63] of uint32;
end;

pSteamNetConnectionInfo_t = ^SteamNetConnectionInfo_t;

SteamNetworkingQuickConnectionStatus = record
  m_eState: ESteamNetworkingConnectionState;
  m_nPing: Longint;
  m_flConnectionQualityLocal: Single;
  m_flConnectionQualityRemote: Single;
  m_flOutPacketsPerSec: Single;
  m_flOutBytesPerSec: Single;
  m_flInPacketsPerSec: Single;
  m_flInBytesPerSec: Single;
  m_nSendRateBytesPerSecond: Longint;
  m_cbPendingUnreliable: Longint;
  m_cbPendingReliable: Longint;
  m_cbSentUnackedReliable: Longint;
  m_usecQueueTime: SteamNetworkingMicroseconds;
  reserved: array [0..15] of uint32;
end;

pSteamNetworkingQuickConnectionStatus = ^SteamNetworkingQuickConnectionStatus;

SteamNetworkingMessage_t = record
  m_pData: Pointer;
  m_cbSize: Longint;
  m_conn: HSteamNetConnection;
  m_identityPeer: SteamNetworkingIdentity;
  m_nConnUserData: int64;
  m_usecTimeReceived: SteamNetworkingMicroseconds;
  m_nMessageNumber: int64;
  m_pfnFreeData: Pointer;
  m_pfnRelease: Pointer;
  m_nChannel: Longint;
  m_nFlags: Longint;
  m_nUserData: int64;
end;

pSteamNetworkingMessage_t = ^SteamNetworkingMessage_t;

SteamNetworkPingLocation_t = record
  m_data: array [0..511] of uint8;
end;

pSteamNetworkPingLocation_t = ^SteamNetworkPingLocation_t;

SteamNetworkingConfigValue_t = record
  m_eValue: ESteamNetworkingConfigValue;
  m_eDataType: ESteamNetworkingConfigDataType;
  m_int64: int64;
end;

pSteamNetworkingConfigValue_t = ^SteamNetworkingConfigValue_t;

SteamDatagramHostedAddress = record
  m_cbSize: Longint;
  m_data: array [0..127] of char;
end;

pSteamDatagramHostedAddress = ^SteamDatagramHostedAddress;

SteamDatagramGameCoordinatorServerLogin = record
  m_identity: SteamNetworkingIdentity;
  m_routing: SteamDatagramHostedAddress;
  m_nAppID: AppId_t;
  m_rtime: RTime32;
  m_cbAppData: Longint;
  m_appData: array [0..2047] of char;
end;

pSteamDatagramGameCoordinatorServerLogin = ^SteamDatagramGameCoordinatorServerLogin;
}

SteamServersConnected_t = record
end;

pSteamServersConnected_t = ^SteamServersConnected_t;

SteamServerConnectFailure_t = record
  m_eResult: EResult;
  m_bStillRetrying: Boolean;
end;

pSteamServerConnectFailure_t = ^SteamServerConnectFailure_t;

SteamServersDisconnected_t = record
  m_eResult: EResult;
end;

pSteamServersDisconnected_t = ^SteamServersDisconnected_t;

ClientGameServerDeny_t = record
  m_uAppID: uint32;
  m_unGameServerIP: uint32;
  m_usGameServerPort: uint16;
  m_bSecure: uint16;
  m_uReason: uint32;
end;

pClientGameServerDeny_t = ^ClientGameServerDeny_t;

EFailureType = (
  k_EFailureFlushedCallbackQueue = 0,
  k_EFailurePipeFail = 1
);

IPCFailure_t = record
  m_eFailureType: uint8;
end;

pIPCFailure_t = ^IPCFailure_t;

LicensesUpdated_t = record
end;

pLicensesUpdated_t = ^LicensesUpdated_t;

ValidateAuthTicketResponse_t = record
  m_SteamID: TSteamID;
  m_eAuthSessionResponse: EAuthSessionResponse;
  m_OwnerSteamID: TSteamID;
end;

pValidateAuthTicketResponse_t = ^ValidateAuthTicketResponse_t;

MicroTxnAuthorizationResponse_t = record
  m_unAppID: uint32;
  m_ulOrderID: uint64;
  m_bAuthorized: uint8;
end;

pMicroTxnAuthorizationResponse_t = ^MicroTxnAuthorizationResponse_t;

EncryptedAppTicketResponse_t = record
  m_eResult: EResult;
end;

pEncryptedAppTicketResponse_t = ^EncryptedAppTicketResponse_t;

GetAuthSessionTicketResponse_t = record
  m_hAuthTicket: HAuthTicket;
  m_eResult: EResult;
end;

pGetAuthSessionTicketResponse_t = ^GetAuthSessionTicketResponse_t;

GameWebCallback_t = record
  m_szURL: array [0..255] of char;
end;

pGameWebCallback_t = ^GameWebCallback_t;

StoreAuthURLResponse_t = record
  m_szURL: array [0..511] of char;
end;

pStoreAuthURLResponse_t = ^StoreAuthURLResponse_t;

MarketEligibilityResponse_t = record
  m_bAllowed: Boolean;
  m_eNotAllowedReason: EMarketNotAllowedReasonFlags;
  m_rtAllowedAtTime: RTime32;
  m_cdaySteamGuardRequiredDays: Longint;
  m_cdayNewDeviceCooldown: Longint;
end;

pMarketEligibilityResponse_t = ^MarketEligibilityResponse_t;

DurationControl_t = record
  m_eResult: EResult;
  m_appid: AppId_t;
  m_bApplicable: Boolean;
  m_csecsLast5h: int32;
  m_progress: EDurationControlProgress;
  m_notification: EDurationControlNotification;
  m_csecsToday: int32;
  m_csecsRemaining: int32;
end;

pDurationControl_t = ^DurationControl_t;

PersonaStateChange_t = record
  m_ulSteamID: uint64;
  m_nChangeFlags: Longint;
end;

pPersonaStateChange_t = ^PersonaStateChange_t;

GameOverlayActivated_t = record
  m_bActive: uint8;
end;

pGameOverlayActivated_t = ^GameOverlayActivated_t;

GameServerChangeRequested_t = record
  m_rgchServer: array [0..63] of char;
  m_rgchPassword: array [0..63] of char;
end;

pGameServerChangeRequested_t = ^GameServerChangeRequested_t;

GameLobbyJoinRequested_t = record
  m_steamIDLobby: TSteamID;
  m_steamIDFriend: TSteamID;
end;

pGameLobbyJoinRequested_t = ^GameLobbyJoinRequested_t;

AvatarImageLoaded_t = record
  m_steamID: TSteamID;
  m_iImage: Longint;
  m_iWide: Longint;
  m_iTall: Longint;
end;

pAvatarImageLoaded_t = ^AvatarImageLoaded_t;

ClanOfficerListResponse_t = record
  m_steamIDClan: TSteamID;
  m_cOfficers: Longint;
  m_bSuccess: uint8;
end;

pClanOfficerListResponse_t = ^ClanOfficerListResponse_t;

FriendRichPresenceUpdate_t = record
  m_steamIDFriend: TSteamID;
  m_nAppID: AppId_t;
end;

pFriendRichPresenceUpdate_t = ^FriendRichPresenceUpdate_t;

GameRichPresenceJoinRequested_t = record
  m_steamIDFriend: TSteamID;
  m_rgchConnect: array [0..255] of char;
end;

pGameRichPresenceJoinRequested_t = ^GameRichPresenceJoinRequested_t;

GameConnectedClanChatMsg_t = record
  m_steamIDClanChat: TSteamID;
  m_steamIDUser: TSteamID;
  m_iMessageID: Longint;
end;

pGameConnectedClanChatMsg_t = ^GameConnectedClanChatMsg_t;

GameConnectedChatJoin_t = record
  m_steamIDClanChat: TSteamID;
  m_steamIDUser: TSteamID;
end;

pGameConnectedChatJoin_t = ^GameConnectedChatJoin_t;

GameConnectedChatLeave_t = record
  m_steamIDClanChat: TSteamID;
  m_steamIDUser: TSteamID;
  m_bKicked: Boolean;
  m_bDropped: Boolean;
end;

pGameConnectedChatLeave_t = ^GameConnectedChatLeave_t;

DownloadClanActivityCountsResult_t = record
  m_bSuccess: Boolean;
end;

pDownloadClanActivityCountsResult_t = ^DownloadClanActivityCountsResult_t;

JoinClanChatRoomCompletionResult_t = record
  m_steamIDClanChat: TSteamID;
  m_eChatRoomEnterResponse: EChatRoomEnterResponse;
end;

pJoinClanChatRoomCompletionResult_t = ^JoinClanChatRoomCompletionResult_t;

GameConnectedFriendChatMsg_t = record
  m_steamIDUser: TSteamID;
  m_iMessageID: Longint;
end;

pGameConnectedFriendChatMsg_t = ^GameConnectedFriendChatMsg_t;

FriendsGetFollowerCount_t = record
  m_eResult: EResult;
  m_steamID: TSteamID;
  m_nCount: Longint;
end;

pFriendsGetFollowerCount_t = ^FriendsGetFollowerCount_t;

FriendsIsFollowing_t = record
  m_eResult: EResult;
  m_steamID: TSteamID;
  m_bIsFollowing: Boolean;
end;

pFriendsIsFollowing_t = ^FriendsIsFollowing_t;

FriendsEnumerateFollowingList_t = record
  m_eResult: EResult;
  m_rgSteamID: array [0..49] of TSteamID;
  m_nResultsReturned: int32;
  m_nTotalResultCount: int32;
end;

pFriendsEnumerateFollowingList_t = ^FriendsEnumerateFollowingList_t;

SetPersonaNameResponse_t = record
  m_bSuccess: Boolean;
  m_bLocalSuccess: Boolean;
  m_result: EResult;
end;

pSetPersonaNameResponse_t = ^SetPersonaNameResponse_t;

UnreadChatMessagesChanged_t = record
end;

pUnreadChatMessagesChanged_t = ^UnreadChatMessagesChanged_t;

IPCountry_t = record
end;

pIPCountry_t = ^IPCountry_t;

LowBatteryPower_t = record
  m_nMinutesBatteryLeft: uint8;
end;

pLowBatteryPower_t = ^LowBatteryPower_t;

SteamAPICallCompleted_t = record
  m_hAsyncCall: SteamAPICall_t;
  m_iCallback: Integer;
  m_cubParam: uint32;
end;

pSteamAPICallCompleted_t = ^SteamAPICallCompleted_t;

SteamShutdown_t = record
end;

pSteamShutdown_t = ^SteamShutdown_t;

CheckFileSignature_t = record
  m_eCheckFileSignature: ECheckFileSignature;
end;

pCheckFileSignature_t = ^CheckFileSignature_t;

GamepadTextInputDismissed_t = record
  m_bSubmitted: Boolean;
  m_unSubmittedText: uint32;
end;

pGamepadTextInputDismissed_t = ^GamepadTextInputDismissed_t;

FavoritesListChanged_t = record
  m_nIP: uint32;
  m_nQueryPort: uint32;
  m_nConnPort: uint32;
  m_nAppID: uint32;
  m_nFlags: uint32;
  m_bAdd: Boolean;
  m_unAccountId: AccountID_t;
end;

pFavoritesListChanged_t = ^FavoritesListChanged_t;

LobbyInvite_t = record
  m_ulSteamIDUser: uint64;
  m_ulSteamIDLobby: uint64;
  m_ulGameID: uint64;
end;

pLobbyInvite_t = ^LobbyInvite_t;

LobbyEnter_t = record
  m_ulSteamIDLobby: uint64;
  m_rgfChatPermissions: uint32;
  m_bLocked: Boolean;
  m_EChatRoomEnterResponse: uint32;
end;

pLobbyEnter_t = ^LobbyEnter_t;

LobbyDataUpdate_t = record
  m_ulSteamIDLobby: uint64;
  m_ulSteamIDMember: uint64;
  m_bSuccess: uint8;
end;

pLobbyDataUpdate_t = ^LobbyDataUpdate_t;

LobbyChatUpdate_t = record
  m_ulSteamIDLobby: uint64;
  m_ulSteamIDUserChanged: uint64;
  m_ulSteamIDMakingChange: uint64;
  m_rgfChatMemberStateChange: uint32;
end;

pLobbyChatUpdate_t = ^LobbyChatUpdate_t;

LobbyChatMsg_t = record
  m_ulSteamIDLobby: uint64;
  m_ulSteamIDUser: uint64;
  m_eChatEntryType: uint8;
  m_iChatID: uint32;
end;

pLobbyChatMsg_t = ^LobbyChatMsg_t;

LobbyGameCreated_t = record
  m_ulSteamIDLobby: uint64;
  m_ulSteamIDGameServer: uint64;
  m_unIP: uint32;
  m_usPort: uint16;
end;

pLobbyGameCreated_t = ^LobbyGameCreated_t;

LobbyMatchList_t = record
  m_nLobbiesMatching: uint32;
end;

pLobbyMatchList_t = ^LobbyMatchList_t;

LobbyKicked_t = record
  m_ulSteamIDLobby: uint64;
  m_ulSteamIDAdmin: uint64;
  m_bKickedDueToDisconnect: uint8;
end;

pLobbyKicked_t = ^LobbyKicked_t;

LobbyCreated_t = record
  m_eResult: EResult;
  m_ulSteamIDLobby: uint64;
end;

pLobbyCreated_t = ^LobbyCreated_t;

PSNGameBootInviteResult_t = record
  m_bGameBootInviteExists: Boolean;
  m_steamIDLobby: TSteamID;
end;

pPSNGameBootInviteResult_t = ^PSNGameBootInviteResult_t;

FavoritesListAccountsUpdated_t = record
  m_eResult: EResult;
end;

pFavoritesListAccountsUpdated_t = ^FavoritesListAccountsUpdated_t;

SearchForGameProgressCallback_t = record
  m_ullSearchID: uint64;
  m_eResult: EResult;
  m_lobbyID: TSteamID;
  m_steamIDEndedSearch: TSteamID;
  m_nSecondsRemainingEstimate: int32;
  m_cPlayersSearching: int32;
end;

pSearchForGameProgressCallback_t = ^SearchForGameProgressCallback_t;

SearchForGameResultCallback_t = record
  m_ullSearchID: uint64;
  m_eResult: EResult;
  m_nCountPlayersInGame: int32;
  m_nCountAcceptedGame: int32;
  m_steamIDHost: TSteamID;
  m_bFinalCallback: Boolean;
end;

pSearchForGameResultCallback_t = ^SearchForGameResultCallback_t;

RequestPlayersForGameProgressCallback_t = record
  m_eResult: EResult;
  m_ullSearchID: uint64;
end;

pRequestPlayersForGameProgressCallback_t = ^RequestPlayersForGameProgressCallback_t;

PlayerAcceptState_t = (
  k_EStateUnknown = 0,
  k_EStatePlayerAccepted = 1,
  k_EStatePlayerDeclined = 2
);

RequestPlayersForGameResultCallback_t = record
  m_eResult: EResult;
  m_ullSearchID: uint64;
  m_SteamIDPlayerFound: TSteamID;
  m_SteamIDLobby: TSteamID;
  m_ePlayerAcceptState: PlayerAcceptState_t;
  m_nPlayerIndex: int32;
  m_nTotalPlayersFound: int32;
  m_nTotalPlayersAcceptedGame: int32;
  m_nSuggestedTeamIndex: int32;
  m_ullUniqueGameID: uint64;
end;

pRequestPlayersForGameResultCallback_t = ^RequestPlayersForGameResultCallback_t;

RequestPlayersForGameFinalResultCallback_t = record
  m_eResult: EResult;
  m_ullSearchID: uint64;
  m_ullUniqueGameID: uint64;
end;

pRequestPlayersForGameFinalResultCallback_t = ^RequestPlayersForGameFinalResultCallback_t;

SubmitPlayerResultResultCallback_t = record
  m_eResult: EResult;
  ullUniqueGameID: uint64;
  steamIDPlayer: TSteamID;
end;

pSubmitPlayerResultResultCallback_t = ^SubmitPlayerResultResultCallback_t;

EndGameResultCallback_t = record
  m_eResult: EResult;
  ullUniqueGameID: uint64;
end;

pEndGameResultCallback_t = ^EndGameResultCallback_t;

JoinPartyCallback_t = record
  m_eResult: EResult;
  m_ulBeaconID: PartyBeaconID_t;
  m_SteamIDBeaconOwner: TSteamID;
  m_rgchConnectString: array [0..255] of char;
end;

pJoinPartyCallback_t = ^JoinPartyCallback_t;

CreateBeaconCallback_t = record
  m_eResult: EResult;
  m_ulBeaconID: PartyBeaconID_t;
end;

pCreateBeaconCallback_t = ^CreateBeaconCallback_t;

ReservationNotificationCallback_t = record
  m_ulBeaconID: PartyBeaconID_t;
  m_steamIDJoiner: TSteamID;
end;

pReservationNotificationCallback_t = ^ReservationNotificationCallback_t;

ChangeNumOpenSlotsCallback_t = record
  m_eResult: EResult;
end;

pChangeNumOpenSlotsCallback_t = ^ChangeNumOpenSlotsCallback_t;

AvailableBeaconLocationsUpdated_t = record
end;

pAvailableBeaconLocationsUpdated_t = ^AvailableBeaconLocationsUpdated_t;

ActiveBeaconsUpdated_t = record
end;

pActiveBeaconsUpdated_t = ^ActiveBeaconsUpdated_t;

RemoteStorageAppSyncedClient_t = record
  m_nAppID: AppId_t;
  m_eResult: EResult;
  m_unNumDownloads: Longint;
end;

pRemoteStorageAppSyncedClient_t = ^RemoteStorageAppSyncedClient_t;

RemoteStorageAppSyncedServer_t = record
  m_nAppID: AppId_t;
  m_eResult: EResult;
  m_unNumUploads: Longint;
end;

pRemoteStorageAppSyncedServer_t = ^RemoteStorageAppSyncedServer_t;

RemoteStorageAppSyncProgress_t = record
  m_rgchCurrentFile: array [0..259] of char;
  m_nAppID: AppId_t;
  m_uBytesTransferredThisChunk: uint32;
  m_dAppPercentComplete: double;
  m_bUploading: Boolean;
end;

pRemoteStorageAppSyncProgress_t = ^RemoteStorageAppSyncProgress_t;

RemoteStorageAppSyncStatusCheck_t = record
  m_nAppID: AppId_t;
  m_eResult: EResult;
end;

pRemoteStorageAppSyncStatusCheck_t = ^RemoteStorageAppSyncStatusCheck_t;

RemoteStorageFileShareResult_t = record
  m_eResult: EResult;
  m_hFile: UGCHandle_t;
  m_rgchFilename: array [0..259] of char;
end;

pRemoteStorageFileShareResult_t = ^RemoteStorageFileShareResult_t;

RemoteStoragePublishFileResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
end;

pRemoteStoragePublishFileResult_t = ^RemoteStoragePublishFileResult_t;

RemoteStorageDeletePublishedFileResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
end;

pRemoteStorageDeletePublishedFileResult_t = ^RemoteStorageDeletePublishedFileResult_t;

RemoteStorageEnumerateUserPublishedFilesResult_t = record
  m_eResult: EResult;
  m_nResultsReturned: int32;
  m_nTotalResultCount: int32;
  m_rgPublishedFileId: array [0..49] of PublishedFileId_t;
end;

pRemoteStorageEnumerateUserPublishedFilesResult_t = ^RemoteStorageEnumerateUserPublishedFilesResult_t;

RemoteStorageSubscribePublishedFileResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
end;

pRemoteStorageSubscribePublishedFileResult_t = ^RemoteStorageSubscribePublishedFileResult_t;

RemoteStorageEnumerateUserSubscribedFilesResult_t = record
  m_eResult: EResult;
  m_nResultsReturned: int32;
  m_nTotalResultCount: int32;
  m_rgPublishedFileId: array [0..49] of PublishedFileId_t;
  m_rgRTimeSubscribed: array [0..49] of uint32;
end;

pRemoteStorageEnumerateUserSubscribedFilesResult_t = ^RemoteStorageEnumerateUserSubscribedFilesResult_t;

RemoteStorageUnsubscribePublishedFileResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
end;

pRemoteStorageUnsubscribePublishedFileResult_t = ^RemoteStorageUnsubscribePublishedFileResult_t;

RemoteStorageUpdatePublishedFileResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
end;

pRemoteStorageUpdatePublishedFileResult_t = ^RemoteStorageUpdatePublishedFileResult_t;

RemoteStorageDownloadUGCResult_t = record
  m_eResult: EResult;
  m_hFile: UGCHandle_t;
  m_nAppID: AppId_t;
  m_nSizeInBytes: int32;
  m_pchFileName: array [0..259] of char;
  m_ulSteamIDOwner: uint64;
end;

pRemoteStorageDownloadUGCResult_t = ^RemoteStorageDownloadUGCResult_t;

RemoteStorageGetPublishedFileDetailsResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_nCreatorAppID: AppId_t;
  m_nConsumerAppID: AppId_t;
  m_rgchTitle: array [0..128] of char;
  m_rgchDescription: array [0..7999] of char;
  m_hFile: UGCHandle_t;
  m_hPreviewFile: UGCHandle_t;
  m_ulSteamIDOwner: uint64;
  m_rtimeCreated: uint32;
  m_rtimeUpdated: uint32;
  m_eVisibility: ERemoteStoragePublishedFileVisibility;
  m_bBanned: Boolean;
  m_rgchTags: array [0..1024] of char;
  m_bTagsTruncated: Boolean;
  m_pchFileName: array [0..259] of char;
  m_nFileSize: int32;
  m_nPreviewFileSize: int32;
  m_rgchURL: array [0..255] of char;
  m_eFileType: EWorkshopFileType;
  m_bAcceptedForUse: Boolean;
end;

pRemoteStorageGetPublishedFileDetailsResult_t = ^RemoteStorageGetPublishedFileDetailsResult_t;

RemoteStorageEnumerateWorkshopFilesResult_t = record
  m_eResult: EResult;
  m_nResultsReturned: int32;
  m_nTotalResultCount: int32;
  m_rgPublishedFileId: array [0..49] of PublishedFileId_t;
  m_rgScore: array [0..49] of Single;
  m_nAppId: AppId_t;
  m_unStartIndex: uint32;
end;

pRemoteStorageEnumerateWorkshopFilesResult_t = ^RemoteStorageEnumerateWorkshopFilesResult_t;

RemoteStorageGetPublishedItemVoteDetailsResult_t = record
  m_eResult: EResult;
  m_unPublishedFileId: PublishedFileId_t;
  m_nVotesFor: int32;
  m_nVotesAgainst: int32;
  m_nReports: int32;
  m_fScore: Single;
end;

pRemoteStorageGetPublishedItemVoteDetailsResult_t = ^RemoteStorageGetPublishedItemVoteDetailsResult_t;

RemoteStoragePublishedFileSubscribed_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_nAppID: AppId_t;
end;

pRemoteStoragePublishedFileSubscribed_t = ^RemoteStoragePublishedFileSubscribed_t;

RemoteStoragePublishedFileUnsubscribed_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_nAppID: AppId_t;
end;

pRemoteStoragePublishedFileUnsubscribed_t = ^RemoteStoragePublishedFileUnsubscribed_t;

RemoteStoragePublishedFileDeleted_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_nAppID: AppId_t;
end;

pRemoteStoragePublishedFileDeleted_t = ^RemoteStoragePublishedFileDeleted_t;

RemoteStorageUpdateUserPublishedItemVoteResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
end;

pRemoteStorageUpdateUserPublishedItemVoteResult_t = ^RemoteStorageUpdateUserPublishedItemVoteResult_t;

RemoteStorageUserVoteDetails_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_eVote: EWorkshopVote;
end;

pRemoteStorageUserVoteDetails_t = ^RemoteStorageUserVoteDetails_t;

RemoteStorageEnumerateUserSharedWorkshopFilesResult_t = record
  m_eResult: EResult;
  m_nResultsReturned: int32;
  m_nTotalResultCount: int32;
  m_rgPublishedFileId: array [0..49] of PublishedFileId_t;
end;

pRemoteStorageEnumerateUserSharedWorkshopFilesResult_t = ^RemoteStorageEnumerateUserSharedWorkshopFilesResult_t;

RemoteStorageSetUserPublishedFileActionResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_eAction: EWorkshopFileAction;
end;

pRemoteStorageSetUserPublishedFileActionResult_t = ^RemoteStorageSetUserPublishedFileActionResult_t;

RemoteStorageEnumeratePublishedFilesByUserActionResult_t = record
  m_eResult: EResult;
  m_eAction: EWorkshopFileAction;
  m_nResultsReturned: int32;
  m_nTotalResultCount: int32;
  m_rgPublishedFileId: array [0..49] of PublishedFileId_t;
  m_rgRTimeUpdated: array [0..49] of uint32;
end;

pRemoteStorageEnumeratePublishedFilesByUserActionResult_t = ^RemoteStorageEnumeratePublishedFilesByUserActionResult_t;

RemoteStoragePublishFileProgress_t = record
  m_dPercentFile: double;
  m_bPreview: Boolean;
end;

pRemoteStoragePublishFileProgress_t = ^RemoteStoragePublishFileProgress_t;

RemoteStoragePublishedFileUpdated_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_nAppID: AppId_t;
  m_ulUnused: uint64;
end;

pRemoteStoragePublishedFileUpdated_t = ^RemoteStoragePublishedFileUpdated_t;

RemoteStorageFileWriteAsyncComplete_t = record
  m_eResult: EResult;
end;

pRemoteStorageFileWriteAsyncComplete_t = ^RemoteStorageFileWriteAsyncComplete_t;

RemoteStorageFileReadAsyncComplete_t = record
  m_hFileReadAsync: SteamAPICall_t;
  m_eResult: EResult;
  m_nOffset: uint32;
  m_cubRead: uint32;
end;

pRemoteStorageFileReadAsyncComplete_t = ^RemoteStorageFileReadAsyncComplete_t;

UserStatsReceived_t = record
  m_nGameID: uint64;
  m_eResult: EResult;
  m_steamIDUser: TSteamID;
end;

pUserStatsReceived_t = ^UserStatsReceived_t;

UserStatsStored_t = record
  m_nGameID: uint64;
  m_eResult: EResult;
end;

pUserStatsStored_t = ^UserStatsStored_t;

UserAchievementStored_t = record
  m_nGameID: uint64;
  m_bGroupAchievement: Boolean;
  m_rgchAchievementName: array [0..127] of char;
  m_nCurProgress: uint32;
  m_nMaxProgress: uint32;
end;

pUserAchievementStored_t = ^UserAchievementStored_t;

LeaderboardFindResult_t = record
  m_hSteamLeaderboard: SteamLeaderboard_t;
  m_bLeaderboardFound: uint8;
end;

pLeaderboardFindResult_t = ^LeaderboardFindResult_t;

LeaderboardScoresDownloaded_t = record
  m_hSteamLeaderboard: SteamLeaderboard_t;
  m_hSteamLeaderboardEntries: SteamLeaderboardEntries_t;
  m_cEntryCount: Longint;
end;

pLeaderboardScoresDownloaded_t = ^LeaderboardScoresDownloaded_t;

LeaderboardScoreUploaded_t = record
  m_bSuccess: uint8;
  m_hSteamLeaderboard: SteamLeaderboard_t;
  m_nScore: int32;
  m_bScoreChanged: uint8;
  m_nGlobalRankNew: Longint;
  m_nGlobalRankPrevious: Longint;
end;

pLeaderboardScoreUploaded_t = ^LeaderboardScoreUploaded_t;

NumberOfCurrentPlayers_t = record
  m_bSuccess: uint8;
  m_cPlayers: int32;
end;

pNumberOfCurrentPlayers_t = ^NumberOfCurrentPlayers_t;

UserStatsUnloaded_t = record
  m_steamIDUser: TSteamID;
end;

pUserStatsUnloaded_t = ^UserStatsUnloaded_t;

UserAchievementIconFetched_t = record
  m_nGameID: uint64_gameid;
  m_rgchAchievementName: array [0..127] of char;
  m_bAchieved: Boolean;
  m_nIconHandle: Longint;
end;

pUserAchievementIconFetched_t = ^UserAchievementIconFetched_t;

GlobalAchievementPercentagesReady_t = record
  m_nGameID: uint64;
  m_eResult: EResult;
end;

pGlobalAchievementPercentagesReady_t = ^GlobalAchievementPercentagesReady_t;

LeaderboardUGCSet_t = record
  m_eResult: EResult;
  m_hSteamLeaderboard: SteamLeaderboard_t;
end;

pLeaderboardUGCSet_t = ^LeaderboardUGCSet_t;

PS3TrophiesInstalled_t = record
  m_nGameID: uint64;
  m_eResult: EResult;
  m_ulRequiredDiskSpace: uint64;
end;

pPS3TrophiesInstalled_t = ^PS3TrophiesInstalled_t;

GlobalStatsReceived_t = record
  m_nGameID: uint64;
  m_eResult: EResult;
end;

pGlobalStatsReceived_t = ^GlobalStatsReceived_t;

DlcInstalled_t = record
  m_nAppID: AppId_t;
end;

pDlcInstalled_t = ^DlcInstalled_t;

RegisterActivationCodeResponse_t = record
  m_eResult: ERegisterActivationCodeResult;
  m_unPackageRegistered: uint32;
end;

pRegisterActivationCodeResponse_t = ^RegisterActivationCodeResponse_t;

NewUrlLaunchParameters_t = record
end;

pNewUrlLaunchParameters_t = ^NewUrlLaunchParameters_t;

AppProofOfPurchaseKeyResponse_t = record
  m_eResult: EResult;
  m_nAppID: uint32;
  m_cchKeyLength: uint32;
  m_rgchKey: array [0..239] of char;
end;

pAppProofOfPurchaseKeyResponse_t = ^AppProofOfPurchaseKeyResponse_t;

FileDetailsResult_t = record
  m_eResult: EResult;
  m_ulFileSize: uint64;
  m_FileSHA: array [0..19] of uint8;
  m_unFlags: uint32;
end;

pFileDetailsResult_t = ^FileDetailsResult_t;

P2PSessionRequest_t = record
  m_steamIDRemote: TSteamID;
end;

pP2PSessionRequest_t = ^P2PSessionRequest_t;

P2PSessionConnectFail_t = record
  m_steamIDRemote: TSteamID;
  m_eP2PSessionError: uint8;
end;

pP2PSessionConnectFail_t = ^P2PSessionConnectFail_t;

SocketStatusCallback_t = record
  m_hSocket: SNetSocket_t;
  m_hListenSocket: SNetListenSocket_t;
  m_steamIDRemote: TSteamID;
  m_eSNetSocketState: Longint;
end;

pSocketStatusCallback_t = ^SocketStatusCallback_t;

ScreenshotReady_t = record
  m_hLocal: ScreenshotHandle;
  m_eResult: EResult;
end;

pScreenshotReady_t = ^ScreenshotReady_t;

ScreenshotRequested_t = record
end;

pScreenshotRequested_t = ^ScreenshotRequested_t;

PlaybackStatusHasChanged_t = record
end;

pPlaybackStatusHasChanged_t = ^PlaybackStatusHasChanged_t;

VolumeHasChanged_t = record
  m_flNewVolume: Single;
end;

pVolumeHasChanged_t = ^VolumeHasChanged_t;

MusicPlayerRemoteWillActivate_t = record
end;

pMusicPlayerRemoteWillActivate_t = ^MusicPlayerRemoteWillActivate_t;

MusicPlayerRemoteWillDeactivate_t = record
end;

pMusicPlayerRemoteWillDeactivate_t = ^MusicPlayerRemoteWillDeactivate_t;

MusicPlayerRemoteToFront_t = record
end;

pMusicPlayerRemoteToFront_t = ^MusicPlayerRemoteToFront_t;

MusicPlayerWillQuit_t = record
end;

pMusicPlayerWillQuit_t = ^MusicPlayerWillQuit_t;

MusicPlayerWantsPlay_t = record
end;

pMusicPlayerWantsPlay_t = ^MusicPlayerWantsPlay_t;

MusicPlayerWantsPause_t = record
end;

pMusicPlayerWantsPause_t = ^MusicPlayerWantsPause_t;

MusicPlayerWantsPlayPrevious_t = record
end;

pMusicPlayerWantsPlayPrevious_t = ^MusicPlayerWantsPlayPrevious_t;

MusicPlayerWantsPlayNext_t = record
end;

pMusicPlayerWantsPlayNext_t = ^MusicPlayerWantsPlayNext_t;

MusicPlayerWantsShuffled_t = record
  m_bShuffled: Boolean;
end;

pMusicPlayerWantsShuffled_t = ^MusicPlayerWantsShuffled_t;

MusicPlayerWantsLooped_t = record
  m_bLooped: Boolean;
end;

pMusicPlayerWantsLooped_t = ^MusicPlayerWantsLooped_t;

MusicPlayerWantsVolume_t = record
  m_flNewVolume: Single;
end;

pMusicPlayerWantsVolume_t = ^MusicPlayerWantsVolume_t;

MusicPlayerSelectsQueueEntry_t = record
  nID: Longint;
end;

pMusicPlayerSelectsQueueEntry_t = ^MusicPlayerSelectsQueueEntry_t;

MusicPlayerSelectsPlaylistEntry_t = record
  nID: Longint;
end;

pMusicPlayerSelectsPlaylistEntry_t = ^MusicPlayerSelectsPlaylistEntry_t;

MusicPlayerWantsPlayingRepeatStatus_t = record
  m_nPlayingRepeatStatus: Longint;
end;

pMusicPlayerWantsPlayingRepeatStatus_t = ^MusicPlayerWantsPlayingRepeatStatus_t;

HTTPRequestCompleted_t = record
  m_hRequest: HTTPRequestHandle;
  m_ulContextValue: uint64;
  m_bRequestSuccessful: Boolean;
  m_eStatusCode: EHTTPStatusCode;
  m_unBodySize: uint32;
end;

pHTTPRequestCompleted_t = ^HTTPRequestCompleted_t;

HTTPRequestHeadersReceived_t = record
  m_hRequest: HTTPRequestHandle;
  m_ulContextValue: uint64;
end;

pHTTPRequestHeadersReceived_t = ^HTTPRequestHeadersReceived_t;

HTTPRequestDataReceived_t = record
  m_hRequest: HTTPRequestHandle;
  m_ulContextValue: uint64;
  m_cOffset: uint32;
  m_cBytesReceived: uint32;
end;

pHTTPRequestDataReceived_t = ^HTTPRequestDataReceived_t;

SteamUGCQueryCompleted_t = record
  m_handle: UGCQueryHandle_t;
  m_eResult: EResult;
  m_unNumResultsReturned: uint32;
  m_unTotalMatchingResults: uint32;
  m_bCachedData: Boolean;
  m_rgchNextCursor: array [0..255] of char;
end;

pSteamUGCQueryCompleted_t = ^SteamUGCQueryCompleted_t;

SteamUGCRequestUGCDetailsResult_t = record
  m_details: SteamUGCDetails_t;
  m_bCachedData: Boolean;
end;

pSteamUGCRequestUGCDetailsResult_t = ^SteamUGCRequestUGCDetailsResult_t;

CreateItemResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
end;

pCreateItemResult_t = ^CreateItemResult_t;

SubmitItemUpdateResult_t = record
  m_eResult: EResult;
  m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
  m_nPublishedFileId: PublishedFileId_t;
end;

pSubmitItemUpdateResult_t = ^SubmitItemUpdateResult_t;

ItemInstalled_t = record
  m_unAppID: AppId_t;
  m_nPublishedFileId: PublishedFileId_t;
end;

pItemInstalled_t = ^ItemInstalled_t;

DownloadItemResult_t = record
  m_unAppID: AppId_t;
  m_nPublishedFileId: PublishedFileId_t;
  m_eResult: EResult;
end;

pDownloadItemResult_t = ^DownloadItemResult_t;

UserFavoriteItemsListChanged_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_eResult: EResult;
  m_bWasAddRequest: Boolean;
end;

pUserFavoriteItemsListChanged_t = ^UserFavoriteItemsListChanged_t;

SetUserItemVoteResult_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_eResult: EResult;
  m_bVoteUp: Boolean;
end;

pSetUserItemVoteResult_t = ^SetUserItemVoteResult_t;

GetUserItemVoteResult_t = record
  m_nPublishedFileId: PublishedFileId_t;
  m_eResult: EResult;
  m_bVotedUp: Boolean;
  m_bVotedDown: Boolean;
  m_bVoteSkipped: Boolean;
end;

pGetUserItemVoteResult_t = ^GetUserItemVoteResult_t;

StartPlaytimeTrackingResult_t = record
  m_eResult: EResult;
end;

pStartPlaytimeTrackingResult_t = ^StartPlaytimeTrackingResult_t;

StopPlaytimeTrackingResult_t = record
  m_eResult: EResult;
end;

pStopPlaytimeTrackingResult_t = ^StopPlaytimeTrackingResult_t;

AddUGCDependencyResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_nChildPublishedFileId: PublishedFileId_t;
end;

pAddUGCDependencyResult_t = ^AddUGCDependencyResult_t;

RemoveUGCDependencyResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_nChildPublishedFileId: PublishedFileId_t;
end;

pRemoveUGCDependencyResult_t = ^RemoveUGCDependencyResult_t;

AddAppDependencyResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_nAppID: AppId_t;
end;

pAddAppDependencyResult_t = ^AddAppDependencyResult_t;

RemoveAppDependencyResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_nAppID: AppId_t;
end;

pRemoveAppDependencyResult_t = ^RemoveAppDependencyResult_t;

GetAppDependenciesResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
  m_rgAppIDs: array [0..31] of AppId_t;
  m_nNumAppDependencies: uint32;
  m_nTotalNumAppDependencies: uint32;
end;

pGetAppDependenciesResult_t = ^GetAppDependenciesResult_t;

DeleteItemResult_t = record
  m_eResult: EResult;
  m_nPublishedFileId: PublishedFileId_t;
end;

pDeleteItemResult_t = ^DeleteItemResult_t;

SteamAppInstalled_t = record
  m_nAppID: AppId_t;
end;

pSteamAppInstalled_t = ^SteamAppInstalled_t;

SteamAppUninstalled_t = record
  m_nAppID: AppId_t;
end;

pSteamAppUninstalled_t = ^SteamAppUninstalled_t;

HTML_BrowserReady_t = record
  unBrowserHandle: HHTMLBrowser;
end;

pHTML_BrowserReady_t = ^HTML_BrowserReady_t;

HTML_NeedsPaint_t = record
  unBrowserHandle: HHTMLBrowser;
  pBGRA: PChar;
  unWide: uint32;
  unTall: uint32;
  unUpdateX: uint32;
  unUpdateY: uint32;
  unUpdateWide: uint32;
  unUpdateTall: uint32;
  unScrollX: uint32;
  unScrollY: uint32;
  flPageScale: Single;
  unPageSerial: uint32;
end;

pHTML_NeedsPaint_t = ^HTML_NeedsPaint_t;

HTML_StartRequest_t = record
  unBrowserHandle: HHTMLBrowser;
  pchURL: PChar;
  pchTarget: PChar;
  pchPostData: PChar;
  bIsRedirect: Boolean;
end;

pHTML_StartRequest_t = ^HTML_StartRequest_t;

HTML_CloseBrowser_t = record
  unBrowserHandle: HHTMLBrowser;
end;

pHTML_CloseBrowser_t = ^HTML_CloseBrowser_t;

HTML_URLChanged_t = record
  unBrowserHandle: HHTMLBrowser;
  pchURL: PChar;
  pchPostData: PChar;
  bIsRedirect: Boolean;
  pchPageTitle: PChar;
  bNewNavigation: Boolean;
end;

pHTML_URLChanged_t = ^HTML_URLChanged_t;

HTML_FinishedRequest_t = record
  unBrowserHandle: HHTMLBrowser;
  pchURL: PChar;
  pchPageTitle: PChar;
end;

pHTML_FinishedRequest_t = ^HTML_FinishedRequest_t;

HTML_OpenLinkInNewTab_t = record
  unBrowserHandle: HHTMLBrowser;
  pchURL: PChar;
end;

pHTML_OpenLinkInNewTab_t = ^HTML_OpenLinkInNewTab_t;

HTML_ChangedTitle_t = record
  unBrowserHandle: HHTMLBrowser;
  pchTitle: PChar;
end;

pHTML_ChangedTitle_t = ^HTML_ChangedTitle_t;

HTML_SearchResults_t = record
  unBrowserHandle: HHTMLBrowser;
  unResults: uint32;
  unCurrentMatch: uint32;
end;

pHTML_SearchResults_t = ^HTML_SearchResults_t;

HTML_CanGoBackAndForward_t = record
  unBrowserHandle: HHTMLBrowser;
  bCanGoBack: Boolean;
  bCanGoForward: Boolean;
end;

pHTML_CanGoBackAndForward_t = ^HTML_CanGoBackAndForward_t;

HTML_HorizontalScroll_t = record
  unBrowserHandle: HHTMLBrowser;
  unScrollMax: uint32;
  unScrollCurrent: uint32;
  flPageScale: Single;
  bVisible: Boolean;
  unPageSize: uint32;
end;

pHTML_HorizontalScroll_t = ^HTML_HorizontalScroll_t;

HTML_VerticalScroll_t = record
  unBrowserHandle: HHTMLBrowser;
  unScrollMax: uint32;
  unScrollCurrent: uint32;
  flPageScale: Single;
  bVisible: Boolean;
  unPageSize: uint32;
end;

pHTML_VerticalScroll_t = ^HTML_VerticalScroll_t;

HTML_LinkAtPosition_t = record
  unBrowserHandle: HHTMLBrowser;
  x: uint32;
  y: uint32;
  pchURL: PChar;
  bInput: Boolean;
  bLiveLink: Boolean;
end;

pHTML_LinkAtPosition_t = ^HTML_LinkAtPosition_t;

HTML_JSAlert_t = record
  unBrowserHandle: HHTMLBrowser;
  pchMessage: PChar;
end;

pHTML_JSAlert_t = ^HTML_JSAlert_t;

HTML_JSConfirm_t = record
  unBrowserHandle: HHTMLBrowser;
  pchMessage: PChar;
end;

pHTML_JSConfirm_t = ^HTML_JSConfirm_t;

HTML_FileOpenDialog_t = record
  unBrowserHandle: HHTMLBrowser;
  pchTitle: PChar;
  pchInitialFile: PChar;
end;

pHTML_FileOpenDialog_t = ^HTML_FileOpenDialog_t;

HTML_NewWindow_t = record
  unBrowserHandle: HHTMLBrowser;
  pchURL: PChar;
  unX: uint32;
  unY: uint32;
  unWide: uint32;
  unTall: uint32;
  unNewWindow_BrowserHandle_IGNORE: HHTMLBrowser;
end;

pHTML_NewWindow_t = ^HTML_NewWindow_t;

HTML_SetCursor_t = record
  unBrowserHandle: HHTMLBrowser;
  eMouseCursor: uint32;
end;

pHTML_SetCursor_t = ^HTML_SetCursor_t;

HTML_StatusText_t = record
  unBrowserHandle: HHTMLBrowser;
  pchMsg: PChar;
end;

pHTML_StatusText_t = ^HTML_StatusText_t;

HTML_ShowToolTip_t = record
  unBrowserHandle: HHTMLBrowser;
  pchMsg: PChar;
end;

pHTML_ShowToolTip_t = ^HTML_ShowToolTip_t;

HTML_UpdateToolTip_t = record
  unBrowserHandle: HHTMLBrowser;
  pchMsg: PChar;
end;

pHTML_UpdateToolTip_t = ^HTML_UpdateToolTip_t;

HTML_HideToolTip_t = record
  unBrowserHandle: HHTMLBrowser;
end;

pHTML_HideToolTip_t = ^HTML_HideToolTip_t;

HTML_BrowserRestarted_t = record
  unBrowserHandle: HHTMLBrowser;
  unOldBrowserHandle: HHTMLBrowser;
end;

pHTML_BrowserRestarted_t = ^HTML_BrowserRestarted_t;

SteamInventoryResultReady_t = record
  m_handle: SteamInventoryResult_t;
  m_result: EResult;
end;

pSteamInventoryResultReady_t = ^SteamInventoryResultReady_t;

SteamInventoryFullUpdate_t = record
  m_handle: SteamInventoryResult_t;
end;

pSteamInventoryFullUpdate_t = ^SteamInventoryFullUpdate_t;

SteamInventoryDefinitionUpdate_t = record
end;

pSteamInventoryDefinitionUpdate_t = ^SteamInventoryDefinitionUpdate_t;

SteamInventoryEligiblePromoItemDefIDs_t = record
  m_result: EResult;
  m_steamID: TSteamID;
  m_numEligiblePromoItemDefs: Longint;
  m_bCachedData: Boolean;
end;

pSteamInventoryEligiblePromoItemDefIDs_t = ^SteamInventoryEligiblePromoItemDefIDs_t;

SteamInventoryStartPurchaseResult_t = record
  m_result: EResult;
  m_ulOrderID: uint64;
  m_ulTransID: uint64;
end;

pSteamInventoryStartPurchaseResult_t = ^SteamInventoryStartPurchaseResult_t;

SteamInventoryRequestPricesResult_t = record
  m_result: EResult;
  m_rgchCurrency: array [0..3] of char;
end;

pSteamInventoryRequestPricesResult_t = ^SteamInventoryRequestPricesResult_t;

GetVideoURLResult_t = record
  m_eResult: EResult;
  m_unVideoAppID: AppId_t;
  m_rgchURL: array [0..255] of char;
end;

pGetVideoURLResult_t = ^GetVideoURLResult_t;

GetOPFSettingsResult_t = record
  m_eResult: EResult;
  m_unVideoAppID: AppId_t;
end;

pGetOPFSettingsResult_t = ^GetOPFSettingsResult_t;

BroadcastUploadStart_t = record
  m_bIsRTMP: Boolean;
end;

pBroadcastUploadStart_t = ^BroadcastUploadStart_t;

BroadcastUploadStop_t = record
  m_eResult: EBroadcastUploadResult;
end;

pBroadcastUploadStop_t = ^BroadcastUploadStop_t;

SteamParentalSettingsChanged_t = record
end;

pSteamParentalSettingsChanged_t = ^SteamParentalSettingsChanged_t;

SteamRemotePlaySessionConnected_t = record
  m_unSessionID: RemotePlaySessionID_t;
end;

pSteamRemotePlaySessionConnected_t = ^SteamRemotePlaySessionConnected_t;

SteamRemotePlaySessionDisconnected_t = record
  m_unSessionID: RemotePlaySessionID_t;
end;

pSteamRemotePlaySessionDisconnected_t = ^SteamRemotePlaySessionDisconnected_t;
{
SteamNetConnectionStatusChangedCallback_t2 = record
  m_hConn: HSteamNetConnection;
  m_info: SteamNetConnectionInfo_t;
  m_eOldState: ESteamNetworkingConnectionState;
end;

pSteamNetConnectionStatusChangedCallback_t2 = ^SteamNetConnectionStatusChangedCallback_t2;

SteamNetAuthenticationStatus_t = record
  m_eAvail: ESteamNetworkingAvailability;
  m_debugMsg: array [0..255] of char;
end;

pSteamNetAuthenticationStatus_t = ^SteamNetAuthenticationStatus_t;

SteamRelayNetworkStatus_t = record
  m_eAvail: ESteamNetworkingAvailability;
  m_bPingMeasurementInProgress: Longint;
  m_eAvailNetworkConfig: ESteamNetworkingAvailability;
  m_eAvailAnyRelay: ESteamNetworkingAvailability;
  m_debugMsg: array [0..255] of char;
end;

pSteamRelayNetworkStatus_t = ^SteamRelayNetworkStatus_t;
}
GSClientApprove_t = record
  m_SteamID: TSteamID;
  m_OwnerSteamID: TSteamID;
end;

pGSClientApprove_t = ^GSClientApprove_t;

GSClientDeny_t = record
  m_SteamID: TSteamID;
  m_eDenyReason: EDenyReason;
  m_rgchOptionalText: array [0..127] of char;
end;

pGSClientDeny_t = ^GSClientDeny_t;

GSClientKick_t = record
  m_SteamID: TSteamID;
  m_eDenyReason: EDenyReason;
end;

pGSClientKick_t = ^GSClientKick_t;

GSClientAchievementStatus_t = record
  m_SteamID: uint64;
  m_pchAchievement: array [0..127] of char;
  m_bUnlocked: Boolean;
end;

pGSClientAchievementStatus_t = ^GSClientAchievementStatus_t;

GSPolicyResponse_t = record
  m_bSecure: uint8;
end;

pGSPolicyResponse_t = ^GSPolicyResponse_t;

GSGameplayStats_t = record
  m_eResult: EResult;
  m_nRank: int32;
  m_unTotalConnects: uint32;
  m_unTotalMinutesPlayed: uint32;
end;

pGSGameplayStats_t = ^GSGameplayStats_t;

GSClientGroupStatus_t = record
  m_SteamIDUser: TSteamID;
  m_SteamIDGroup: TSteamID;
  m_bMember: Boolean;
  m_bOfficer: Boolean;
end;

pGSClientGroupStatus_t = ^GSClientGroupStatus_t;

GSReputation_t = record
  m_eResult: EResult;
  m_unReputationScore: uint32;
  m_bBanned: Boolean;
  m_unBannedIP: uint32;
  m_usBannedPort: uint16;
  m_ulBannedGameID: uint64;
  m_unBanExpires: uint32;
end;

pGSReputation_t = ^GSReputation_t;

AssociateWithClanResult_t = record
  m_eResult: EResult;
end;

pAssociateWithClanResult_t = ^AssociateWithClanResult_t;

ComputeNewPlayerCompatibilityResult_t = record
  m_eResult: EResult;
  m_cPlayersThatDontLikeCandidate: Longint;
  m_cPlayersThatCandidateDoesntLike: Longint;
  m_cClanPlayersThatDontLikeCandidate: Longint;
  m_SteamIDCandidate: TSteamID;
end;

pComputeNewPlayerCompatibilityResult_t = ^ComputeNewPlayerCompatibilityResult_t;

GSStatsReceived_t = record
  m_eResult: EResult;
  m_steamIDUser: TSteamID;
end;

pGSStatsReceived_t = ^GSStatsReceived_t;

GSStatsStored_t = record
  m_eResult: EResult;
  m_steamIDUser: TSteamID;
end;

pGSStatsStored_t = ^GSStatsStored_t;

GSStatsUnloaded_t = record
  m_steamIDUser: TSteamID;
end;

pGSStatsUnloaded_t = ^GSStatsUnloaded_t;


{
struct CallbackMsg_t
  HSteamUser m_hSteamUser; // Specific user to whom this callback applies.
  int m_iCallback; // Callback identifier.  (Corresponds to the k_iCallback enum in the callback structure.)
  uint8 *m_pubParam; // Points to the callback structure
  int m_cubParam; // Size of the data pointed to by m_pubParam
}

CallbackMsg_t = record
  m_hSteamUser: HSteamUser;
  m_iCallback: Integer;
  m_pubParam: Pointer;
  m_cubParam: Integer;
end;

pCallbackMsg_t = ^CallbackMsg_t;

implementation
  uses sysutils;

function TSteamIDHelper.GetAccountID: uint32;
begin
  result := Self.m_comp.m_unAccountID;
end;

function TSteamIDHelper.GetAccountInstance: uint32;
begin
  result := Self.m_comp.m_unAccountInstanceAndType[0];
end;

function TSteamIDHelper.GeEAccountType: EAccountType;
begin
  result := EAccountType(Self.m_comp.m_unAccountInstanceAndType[1]);
end;

function TSteamIDHelper.GeEUniverse: EUniverse;
begin
  result := Self.m_comp.m_EUNiverse;
end;

function TSteamIDHelper.GetAsString: AnsiString;
begin
  Result := '[U:' + IntToStr(Integer(Self.m_comp.m_EUNiverse)) + ':' + IntToStr(Self.m_comp.m_unAccountID) + ']';
end;

{ TGameID }

function TGameID.GetAppID: integer;
begin
  result := Word(Data1) + (Data2 and $0F) shl 16;
end;

function TGameID.GetModID: integer;
begin
  result := Data3;
end;

function TGameID.GetType: integer;
begin
  result := Data4;
end;

end.

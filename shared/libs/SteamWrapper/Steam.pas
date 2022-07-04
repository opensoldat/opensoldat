{ Warning: This file is generated. Edit gen_steam_bindings.py instead. }

unit Steam;

{$IFDEF FPC}
{$MODE DELPHI}
{$WARN 3031 OFF : Values in enumeration types have to be ascending}
{$WARN 4110 OFF : Range check error while evaluating constants}
{$ENDIF}

interface

uses
  ctypes, sysutils;

{$PACKENUM 4}

{$IFDEF UNIX}
{$PACKRECORDS 4}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}

const
  {$IFDEF STEAM}
  {$IFDEF WINDOWS}
  {$IFDEF CPUX86_64}
  STEAMLIB = 'steam_api64.dll';
  {$ELSE}
  STEAMLIB = 'steam_api.dll';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN}
  STEAMLIB = 'libsteam_api.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
  STEAMLIB = 'libsteam_api.so';
  {$ENDIF}
  {$ELSE}
  {$IFDEF WINDOWS}
  STEAMLIB = 'GameNetworkingSockets.dll';
  {$ENDIF}
  {$IFDEF DARWIN}
  STEAMLIB = 'libGameNetworkingSockets.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
  STEAMLIB = 'libGameNetworkingSockets.so';
  {$ENDIF}
  {$ENDIF}

  STEAM_APPID = 638490;

type
  PSteamServersConnected_t = ^SteamServersConnected_t;
  PSteamServerConnectFailure_t = ^SteamServerConnectFailure_t;
  PSteamServersDisconnected_t = ^SteamServersDisconnected_t;
  PClientGameServerDeny_t = ^ClientGameServerDeny_t;
  PIPCFailure_t = ^IPCFailure_t;
  PLicensesUpdated_t = ^LicensesUpdated_t;
  PValidateAuthTicketResponse_t = ^ValidateAuthTicketResponse_t;
  PMicroTxnAuthorizationResponse_t = ^MicroTxnAuthorizationResponse_t;
  PEncryptedAppTicketResponse_t = ^EncryptedAppTicketResponse_t;
  PGetAuthSessionTicketResponse_t = ^GetAuthSessionTicketResponse_t;
  PGameWebCallback_t = ^GameWebCallback_t;
  PStoreAuthURLResponse_t = ^StoreAuthURLResponse_t;
  PMarketEligibilityResponse_t = ^MarketEligibilityResponse_t;
  PDurationControl_t = ^DurationControl_t;
  PPersonaStateChange_t = ^PersonaStateChange_t;
  PGameOverlayActivated_t = ^GameOverlayActivated_t;
  PGameServerChangeRequested_t = ^GameServerChangeRequested_t;
  PGameLobbyJoinRequested_t = ^GameLobbyJoinRequested_t;
  PAvatarImageLoaded_t = ^AvatarImageLoaded_t;
  PClanOfficerListResponse_t = ^ClanOfficerListResponse_t;
  PFriendRichPresenceUpdate_t = ^FriendRichPresenceUpdate_t;
  PGameRichPresenceJoinRequested_t = ^GameRichPresenceJoinRequested_t;
  PGameConnectedClanChatMsg_t = ^GameConnectedClanChatMsg_t;
  PGameConnectedChatJoin_t = ^GameConnectedChatJoin_t;
  PGameConnectedChatLeave_t = ^GameConnectedChatLeave_t;
  PDownloadClanActivityCountsResult_t = ^DownloadClanActivityCountsResult_t;
  PJoinClanChatRoomCompletionResult_t = ^JoinClanChatRoomCompletionResult_t;
  PGameConnectedFriendChatMsg_t = ^GameConnectedFriendChatMsg_t;
  PFriendsGetFollowerCount_t = ^FriendsGetFollowerCount_t;
  PFriendsIsFollowing_t = ^FriendsIsFollowing_t;
  PFriendsEnumerateFollowingList_t = ^FriendsEnumerateFollowingList_t;
  PSetPersonaNameResponse_t = ^SetPersonaNameResponse_t;
  PUnreadChatMessagesChanged_t = ^UnreadChatMessagesChanged_t;
  POverlayBrowserProtocolNavigation_t = ^OverlayBrowserProtocolNavigation_t;
  PIPCountry_t = ^IPCountry_t;
  PLowBatteryPower_t = ^LowBatteryPower_t;
  PSteamAPICallCompleted_t = ^SteamAPICallCompleted_t;
  PSteamShutdown_t = ^SteamShutdown_t;
  PCheckFileSignature_t = ^CheckFileSignature_t;
  PGamepadTextInputDismissed_t = ^GamepadTextInputDismissed_t;
  PAppResumingFromSuspend_t = ^AppResumingFromSuspend_t;
  PFloatingGamepadTextInputDismissed_t = ^FloatingGamepadTextInputDismissed_t;
  PFavoritesListChanged_t = ^FavoritesListChanged_t;
  PLobbyInvite_t = ^LobbyInvite_t;
  PLobbyEnter_t = ^LobbyEnter_t;
  PLobbyDataUpdate_t = ^LobbyDataUpdate_t;
  PLobbyChatUpdate_t = ^LobbyChatUpdate_t;
  PLobbyChatMsg_t = ^LobbyChatMsg_t;
  PLobbyGameCreated_t = ^LobbyGameCreated_t;
  PLobbyMatchList_t = ^LobbyMatchList_t;
  PLobbyKicked_t = ^LobbyKicked_t;
  PLobbyCreated_t = ^LobbyCreated_t;
  PPSNGameBootInviteResult_t = ^PSNGameBootInviteResult_t;
  PFavoritesListAccountsUpdated_t = ^FavoritesListAccountsUpdated_t;
  PSearchForGameProgressCallback_t = ^SearchForGameProgressCallback_t;
  PSearchForGameResultCallback_t = ^SearchForGameResultCallback_t;
  PRequestPlayersForGameProgressCallback_t = ^RequestPlayersForGameProgressCallback_t;
  PRequestPlayersForGameResultCallback_t = ^RequestPlayersForGameResultCallback_t;
  PRequestPlayersForGameFinalResultCallback_t = ^RequestPlayersForGameFinalResultCallback_t;
  PSubmitPlayerResultResultCallback_t = ^SubmitPlayerResultResultCallback_t;
  PEndGameResultCallback_t = ^EndGameResultCallback_t;
  PJoinPartyCallback_t = ^JoinPartyCallback_t;
  PCreateBeaconCallback_t = ^CreateBeaconCallback_t;
  PReservationNotificationCallback_t = ^ReservationNotificationCallback_t;
  PChangeNumOpenSlotsCallback_t = ^ChangeNumOpenSlotsCallback_t;
  PAvailableBeaconLocationsUpdated_t = ^AvailableBeaconLocationsUpdated_t;
  PActiveBeaconsUpdated_t = ^ActiveBeaconsUpdated_t;
  PRemoteStorageFileShareResult_t = ^RemoteStorageFileShareResult_t;
  PRemoteStoragePublishFileResult_t = ^RemoteStoragePublishFileResult_t;
  PRemoteStorageDeletePublishedFileResult_t = ^RemoteStorageDeletePublishedFileResult_t;
  PRemoteStorageEnumerateUserPublishedFilesResult_t = ^RemoteStorageEnumerateUserPublishedFilesResult_t;
  PRemoteStorageSubscribePublishedFileResult_t = ^RemoteStorageSubscribePublishedFileResult_t;
  PRemoteStorageEnumerateUserSubscribedFilesResult_t = ^RemoteStorageEnumerateUserSubscribedFilesResult_t;
  PRemoteStorageUnsubscribePublishedFileResult_t = ^RemoteStorageUnsubscribePublishedFileResult_t;
  PRemoteStorageUpdatePublishedFileResult_t = ^RemoteStorageUpdatePublishedFileResult_t;
  PRemoteStorageDownloadUGCResult_t = ^RemoteStorageDownloadUGCResult_t;
  PRemoteStorageGetPublishedFileDetailsResult_t = ^RemoteStorageGetPublishedFileDetailsResult_t;
  PRemoteStorageEnumerateWorkshopFilesResult_t = ^RemoteStorageEnumerateWorkshopFilesResult_t;
  PRemoteStorageGetPublishedItemVoteDetailsResult_t = ^RemoteStorageGetPublishedItemVoteDetailsResult_t;
  PRemoteStoragePublishedFileSubscribed_t = ^RemoteStoragePublishedFileSubscribed_t;
  PRemoteStoragePublishedFileUnsubscribed_t = ^RemoteStoragePublishedFileUnsubscribed_t;
  PRemoteStoragePublishedFileDeleted_t = ^RemoteStoragePublishedFileDeleted_t;
  PRemoteStorageUpdateUserPublishedItemVoteResult_t = ^RemoteStorageUpdateUserPublishedItemVoteResult_t;
  PRemoteStorageUserVoteDetails_t = ^RemoteStorageUserVoteDetails_t;
  PRemoteStorageEnumerateUserSharedWorkshopFilesResult_t = ^RemoteStorageEnumerateUserSharedWorkshopFilesResult_t;
  PRemoteStorageSetUserPublishedFileActionResult_t = ^RemoteStorageSetUserPublishedFileActionResult_t;
  PRemoteStorageEnumeratePublishedFilesByUserActionResult_t = ^RemoteStorageEnumeratePublishedFilesByUserActionResult_t;
  PRemoteStoragePublishFileProgress_t = ^RemoteStoragePublishFileProgress_t;
  PRemoteStoragePublishedFileUpdated_t = ^RemoteStoragePublishedFileUpdated_t;
  PRemoteStorageFileWriteAsyncComplete_t = ^RemoteStorageFileWriteAsyncComplete_t;
  PRemoteStorageFileReadAsyncComplete_t = ^RemoteStorageFileReadAsyncComplete_t;
  PRemoteStorageLocalFileChange_t = ^RemoteStorageLocalFileChange_t;
  PUserStatsReceived_t = ^UserStatsReceived_t;
  PUserStatsStored_t = ^UserStatsStored_t;
  PUserAchievementStored_t = ^UserAchievementStored_t;
  PLeaderboardFindResult_t = ^LeaderboardFindResult_t;
  PLeaderboardScoresDownloaded_t = ^LeaderboardScoresDownloaded_t;
  PLeaderboardScoreUploaded_t = ^LeaderboardScoreUploaded_t;
  PNumberOfCurrentPlayers_t = ^NumberOfCurrentPlayers_t;
  PUserStatsUnloaded_t = ^UserStatsUnloaded_t;
  PUserAchievementIconFetched_t = ^UserAchievementIconFetched_t;
  PGlobalAchievementPercentagesReady_t = ^GlobalAchievementPercentagesReady_t;
  PLeaderboardUGCSet_t = ^LeaderboardUGCSet_t;
  PPS3TrophiesInstalled_t = ^PS3TrophiesInstalled_t;
  PGlobalStatsReceived_t = ^GlobalStatsReceived_t;
  PDlcInstalled_t = ^DlcInstalled_t;
  PRegisterActivationCodeResponse_t = ^RegisterActivationCodeResponse_t;
  PNewUrlLaunchParameters_t = ^NewUrlLaunchParameters_t;
  PAppProofOfPurchaseKeyResponse_t = ^AppProofOfPurchaseKeyResponse_t;
  PFileDetailsResult_t = ^FileDetailsResult_t;
  PTimedTrialStatus_t = ^TimedTrialStatus_t;
  PP2PSessionRequest_t = ^P2PSessionRequest_t;
  PP2PSessionConnectFail_t = ^P2PSessionConnectFail_t;
  PSocketStatusCallback_t = ^SocketStatusCallback_t;
  PScreenshotReady_t = ^ScreenshotReady_t;
  PScreenshotRequested_t = ^ScreenshotRequested_t;
  PPlaybackStatusHasChanged_t = ^PlaybackStatusHasChanged_t;
  PVolumeHasChanged_t = ^VolumeHasChanged_t;
  PMusicPlayerRemoteWillActivate_t = ^MusicPlayerRemoteWillActivate_t;
  PMusicPlayerRemoteWillDeactivate_t = ^MusicPlayerRemoteWillDeactivate_t;
  PMusicPlayerRemoteToFront_t = ^MusicPlayerRemoteToFront_t;
  PMusicPlayerWillQuit_t = ^MusicPlayerWillQuit_t;
  PMusicPlayerWantsPlay_t = ^MusicPlayerWantsPlay_t;
  PMusicPlayerWantsPause_t = ^MusicPlayerWantsPause_t;
  PMusicPlayerWantsPlayPrevious_t = ^MusicPlayerWantsPlayPrevious_t;
  PMusicPlayerWantsPlayNext_t = ^MusicPlayerWantsPlayNext_t;
  PMusicPlayerWantsShuffled_t = ^MusicPlayerWantsShuffled_t;
  PMusicPlayerWantsLooped_t = ^MusicPlayerWantsLooped_t;
  PMusicPlayerWantsVolume_t = ^MusicPlayerWantsVolume_t;
  PMusicPlayerSelectsQueueEntry_t = ^MusicPlayerSelectsQueueEntry_t;
  PMusicPlayerSelectsPlaylistEntry_t = ^MusicPlayerSelectsPlaylistEntry_t;
  PMusicPlayerWantsPlayingRepeatStatus_t = ^MusicPlayerWantsPlayingRepeatStatus_t;
  PHTTPRequestCompleted_t = ^HTTPRequestCompleted_t;
  PHTTPRequestHeadersReceived_t = ^HTTPRequestHeadersReceived_t;
  PHTTPRequestDataReceived_t = ^HTTPRequestDataReceived_t;
  PSteamInputDeviceConnected_t = ^SteamInputDeviceConnected_t;
  PSteamInputDeviceDisconnected_t = ^SteamInputDeviceDisconnected_t;
  PSteamInputConfigurationLoaded_t = ^SteamInputConfigurationLoaded_t;
  PSteamUGCQueryCompleted_t = ^SteamUGCQueryCompleted_t;
  PSteamUGCRequestUGCDetailsResult_t = ^SteamUGCRequestUGCDetailsResult_t;
  PCreateItemResult_t = ^CreateItemResult_t;
  PSubmitItemUpdateResult_t = ^SubmitItemUpdateResult_t;
  PItemInstalled_t = ^ItemInstalled_t;
  PDownloadItemResult_t = ^DownloadItemResult_t;
  PUserFavoriteItemsListChanged_t = ^UserFavoriteItemsListChanged_t;
  PSetUserItemVoteResult_t = ^SetUserItemVoteResult_t;
  PGetUserItemVoteResult_t = ^GetUserItemVoteResult_t;
  PStartPlaytimeTrackingResult_t = ^StartPlaytimeTrackingResult_t;
  PStopPlaytimeTrackingResult_t = ^StopPlaytimeTrackingResult_t;
  PAddUGCDependencyResult_t = ^AddUGCDependencyResult_t;
  PRemoveUGCDependencyResult_t = ^RemoveUGCDependencyResult_t;
  PAddAppDependencyResult_t = ^AddAppDependencyResult_t;
  PRemoveAppDependencyResult_t = ^RemoveAppDependencyResult_t;
  PGetAppDependenciesResult_t = ^GetAppDependenciesResult_t;
  PDeleteItemResult_t = ^DeleteItemResult_t;
  PUserSubscribedItemsListChanged_t = ^UserSubscribedItemsListChanged_t;
  PWorkshopEULAStatus_t = ^WorkshopEULAStatus_t;
  PSteamAppInstalled_t = ^SteamAppInstalled_t;
  PSteamAppUninstalled_t = ^SteamAppUninstalled_t;
  PHTML_BrowserReady_t = ^HTML_BrowserReady_t;
  PHTML_NeedsPaint_t = ^HTML_NeedsPaint_t;
  PHTML_StartRequest_t = ^HTML_StartRequest_t;
  PHTML_CloseBrowser_t = ^HTML_CloseBrowser_t;
  PHTML_URLChanged_t = ^HTML_URLChanged_t;
  PHTML_FinishedRequest_t = ^HTML_FinishedRequest_t;
  PHTML_OpenLinkInNewTab_t = ^HTML_OpenLinkInNewTab_t;
  PHTML_ChangedTitle_t = ^HTML_ChangedTitle_t;
  PHTML_SearchResults_t = ^HTML_SearchResults_t;
  PHTML_CanGoBackAndForward_t = ^HTML_CanGoBackAndForward_t;
  PHTML_HorizontalScroll_t = ^HTML_HorizontalScroll_t;
  PHTML_VerticalScroll_t = ^HTML_VerticalScroll_t;
  PHTML_LinkAtPosition_t = ^HTML_LinkAtPosition_t;
  PHTML_JSAlert_t = ^HTML_JSAlert_t;
  PHTML_JSConfirm_t = ^HTML_JSConfirm_t;
  PHTML_FileOpenDialog_t = ^HTML_FileOpenDialog_t;
  PHTML_NewWindow_t = ^HTML_NewWindow_t;
  PHTML_SetCursor_t = ^HTML_SetCursor_t;
  PHTML_StatusText_t = ^HTML_StatusText_t;
  PHTML_ShowToolTip_t = ^HTML_ShowToolTip_t;
  PHTML_UpdateToolTip_t = ^HTML_UpdateToolTip_t;
  PHTML_HideToolTip_t = ^HTML_HideToolTip_t;
  PHTML_BrowserRestarted_t = ^HTML_BrowserRestarted_t;
  PSteamInventoryResultReady_t = ^SteamInventoryResultReady_t;
  PSteamInventoryFullUpdate_t = ^SteamInventoryFullUpdate_t;
  PSteamInventoryDefinitionUpdate_t = ^SteamInventoryDefinitionUpdate_t;
  PSteamInventoryEligiblePromoItemDefIDs_t = ^SteamInventoryEligiblePromoItemDefIDs_t;
  PSteamInventoryStartPurchaseResult_t = ^SteamInventoryStartPurchaseResult_t;
  PSteamInventoryRequestPricesResult_t = ^SteamInventoryRequestPricesResult_t;
  PGetVideoURLResult_t = ^GetVideoURLResult_t;
  PGetOPFSettingsResult_t = ^GetOPFSettingsResult_t;
  PSteamParentalSettingsChanged_t = ^SteamParentalSettingsChanged_t;
  PSteamRemotePlaySessionConnected_t = ^SteamRemotePlaySessionConnected_t;
  PSteamRemotePlaySessionDisconnected_t = ^SteamRemotePlaySessionDisconnected_t;
  PSteamNetworkingMessagesSessionRequest_t = ^SteamNetworkingMessagesSessionRequest_t;
  PSteamNetworkingMessagesSessionFailed_t = ^SteamNetworkingMessagesSessionFailed_t;
  PSteamNetConnectionStatusChangedCallback_t = ^SteamNetConnectionStatusChangedCallback_t;
  PSteamNetAuthenticationStatus_t = ^SteamNetAuthenticationStatus_t;
  PSteamRelayNetworkStatus_t = ^SteamRelayNetworkStatus_t;
  PGSClientApprove_t = ^GSClientApprove_t;
  PGSClientDeny_t = ^GSClientDeny_t;
  PGSClientKick_t = ^GSClientKick_t;
  PGSClientAchievementStatus_t = ^GSClientAchievementStatus_t;
  PGSPolicyResponse_t = ^GSPolicyResponse_t;
  PGSGameplayStats_t = ^GSGameplayStats_t;
  PGSClientGroupStatus_t = ^GSClientGroupStatus_t;
  PGSReputation_t = ^GSReputation_t;
  PAssociateWithClanResult_t = ^AssociateWithClanResult_t;
  PComputeNewPlayerCompatibilityResult_t = ^ComputeNewPlayerCompatibilityResult_t;
  PGSStatsReceived_t = ^GSStatsReceived_t;
  PGSStatsStored_t = ^GSStatsStored_t;
  PGSStatsUnloaded_t = ^GSStatsUnloaded_t;
  PSteamNetworkingFakeIPResult_t = ^SteamNetworkingFakeIPResult_t;
  PSteamIPAddress_t = ^SteamIPAddress_t;
  PFriendGameInfo_t = ^FriendGameInfo_t;
  PMatchMakingKeyValuePair_t = ^MatchMakingKeyValuePair_t;
  Pservernetadr_t = ^servernetadr_t;
  Pgameserveritem_t = ^gameserveritem_t;
  PSteamPartyBeaconLocation_t = ^SteamPartyBeaconLocation_t;
  PSteamParamStringArray_t = ^SteamParamStringArray_t;
  PLeaderboardEntry_t = ^LeaderboardEntry_t;
  PP2PSessionState_t = ^P2PSessionState_t;
  PInputAnalogActionData_t = ^InputAnalogActionData_t;
  PInputDigitalActionData_t = ^InputDigitalActionData_t;
  PInputMotionData_t = ^InputMotionData_t;
  PSteamInputActionEvent_t = ^SteamInputActionEvent_t;
  PSteamUGCDetails_t = ^SteamUGCDetails_t;
  PSteamItemDetails_t = ^SteamItemDetails_t;
  PSteamNetworkingIPAddr = ^SteamNetworkingIPAddr;
  PSteamNetworkingIdentity = ^SteamNetworkingIdentity;
  PSteamNetConnectionInfo_t = ^SteamNetConnectionInfo_t;
  PSteamNetConnectionRealTimeStatus_t = ^SteamNetConnectionRealTimeStatus_t;
  PSteamNetConnectionRealTimeLaneStatus_t = ^SteamNetConnectionRealTimeLaneStatus_t;
  PSteamNetworkingMessage_t = ^SteamNetworkingMessage_t;
  PSteamNetworkPingLocation_t = ^SteamNetworkPingLocation_t;
  PSteamNetworkingConfigValue_t = ^SteamNetworkingConfigValue_t;
  PSteamDatagramHostedAddress = ^SteamDatagramHostedAddress;
  PSteamDatagramGameCoordinatorServerLogin = ^SteamDatagramGameCoordinatorServerLogin;
  PPMatchMakingKeyValuePair_t = ^PMatchMakingKeyValuePair_t;
  PPSteamNetworkingMessage_t = ^PSteamNetworkingMessage_t;

  PIPCFailure_t__EFailureType = ^IPCFailure_t__EFailureType;
  IPCFailure_t__EFailureType = (
    k_EFailureFlushedCallbackQueue = 0,
    k_EFailurePipeFail = 1
  );

  PRequestPlayersForGameResultCallback_t__PlayerAcceptState_t = ^RequestPlayersForGameResultCallback_t__PlayerAcceptState_t;
  RequestPlayersForGameResultCallback_t__PlayerAcceptState_t = (
    k_EStateUnknown = 0,
    k_EStatePlayerAccepted = 1,
    k_EStatePlayerDeclined = 2
  );

  PESteamIPType = ^ESteamIPType;
  ESteamIPType = (
    k_ESteamIPTypeIPv4 = 0,
    k_ESteamIPTypeIPv6 = 1
  );

  PEUniverse = ^EUniverse;
  EUniverse = (
    k_EUniverseInvalid = 0,
    k_EUniversePublic = 1,
    k_EUniverseBeta = 2,
    k_EUniverseInternal = 3,
    k_EUniverseDev = 4,
    k_EUniverseMax = 5
  );

  PEResult = ^EResult;
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
    k_EResultExistingUserCancelledLicense = 115,
    k_EResultCommunityCooldown = 116,
    k_EResultNoLauncherSpecified = 117,
    k_EResultMustAgreeToSSA = 118,
    k_EResultLauncherMigrated = 119,
    k_EResultSteamRealmMismatch = 120,
    k_EResultInvalidSignature = 121,
    k_EResultParseFailure = 122,
    k_EResultNoVerifiedPhone = 123
  );

  PEVoiceResult = ^EVoiceResult;
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

  PEDenyReason = ^EDenyReason;
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

  PEBeginAuthSessionResult = ^EBeginAuthSessionResult;
  EBeginAuthSessionResult = (
    k_EBeginAuthSessionResultOK = 0,
    k_EBeginAuthSessionResultInvalidTicket = 1,
    k_EBeginAuthSessionResultDuplicateRequest = 2,
    k_EBeginAuthSessionResultInvalidVersion = 3,
    k_EBeginAuthSessionResultGameMismatch = 4,
    k_EBeginAuthSessionResultExpiredTicket = 5
  );

  PEAuthSessionResponse = ^EAuthSessionResponse;
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

  PEUserHasLicenseForAppResult = ^EUserHasLicenseForAppResult;
  EUserHasLicenseForAppResult = (
    k_EUserHasLicenseResultHasLicense = 0,
    k_EUserHasLicenseResultDoesNotHaveLicense = 1,
    k_EUserHasLicenseResultNoAuth = 2
  );

  PEAccountType = ^EAccountType;
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

  PEChatEntryType = ^EChatEntryType;
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

  PEChatRoomEnterResponse = ^EChatRoomEnterResponse;
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

  PEChatSteamIDInstanceFlags = ^EChatSteamIDInstanceFlags;
  EChatSteamIDInstanceFlags = (
    k_EChatAccountInstanceMask = 4095,
    k_EChatInstanceFlagClan = 524288,
    k_EChatInstanceFlagLobby = 262144,
    k_EChatInstanceFlagMMSLobby = 131072
  );

  PENotificationPosition = ^ENotificationPosition;
  ENotificationPosition = (
    k_EPositionTopLeft = 0,
    k_EPositionTopRight = 1,
    k_EPositionBottomLeft = 2,
    k_EPositionBottomRight = 3
  );

  PEBroadcastUploadResult = ^EBroadcastUploadResult;
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

  PEMarketNotAllowedReasonFlags = ^EMarketNotAllowedReasonFlags;
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

  PEDurationControlProgress = ^EDurationControlProgress;
  EDurationControlProgress = (
    k_EDurationControlProgress_Full = 0,
    k_EDurationControlProgress_Half = 1,
    k_EDurationControlProgress_None = 2,
    k_EDurationControl_ExitSoon_3h = 3,
    k_EDurationControl_ExitSoon_5h = 4,
    k_EDurationControl_ExitSoon_Night = 5
  );

  PEDurationControlNotification = ^EDurationControlNotification;
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

  PEDurationControlOnlineState = ^EDurationControlOnlineState;
  EDurationControlOnlineState = (
    k_EDurationControlOnlineState_Invalid = 0,
    k_EDurationControlOnlineState_Offline = 1,
    k_EDurationControlOnlineState_Online = 2,
    k_EDurationControlOnlineState_OnlineHighPri = 3
  );

  PEGameSearchErrorCode_t = ^EGameSearchErrorCode_t;
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

  PEPlayerResult_t = ^EPlayerResult_t;
  EPlayerResult_t = (
    k_EPlayerResultFailedToConnect = 1,
    k_EPlayerResultAbandoned = 2,
    k_EPlayerResultKicked = 3,
    k_EPlayerResultIncomplete = 4,
    k_EPlayerResultCompleted = 5
  );

  PESteamIPv6ConnectivityProtocol = ^ESteamIPv6ConnectivityProtocol;
  ESteamIPv6ConnectivityProtocol = (
    k_ESteamIPv6ConnectivityProtocol_Invalid = 0,
    k_ESteamIPv6ConnectivityProtocol_HTTP = 1,
    k_ESteamIPv6ConnectivityProtocol_UDP = 2
  );

  PESteamIPv6ConnectivityState = ^ESteamIPv6ConnectivityState;
  ESteamIPv6ConnectivityState = (
    k_ESteamIPv6ConnectivityState_Unknown = 0,
    k_ESteamIPv6ConnectivityState_Good = 1,
    k_ESteamIPv6ConnectivityState_Bad = 2
  );

  PEFriendRelationship = ^EFriendRelationship;
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

  PEPersonaState = ^EPersonaState;
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

  PEFriendFlags = ^EFriendFlags;
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

  PEUserRestriction = ^EUserRestriction;
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

  PEOverlayToStoreFlag = ^EOverlayToStoreFlag;
  EOverlayToStoreFlag = (
    k_EOverlayToStoreFlag_None = 0,
    k_EOverlayToStoreFlag_AddToCart = 1,
    k_EOverlayToStoreFlag_AddToCartAndShow = 2
  );

  PEActivateGameOverlayToWebPageMode = ^EActivateGameOverlayToWebPageMode;
  EActivateGameOverlayToWebPageMode = (
    k_EActivateGameOverlayToWebPageMode_Default = 0,
    k_EActivateGameOverlayToWebPageMode_Modal = 1
  );

  PEPersonaChange = ^EPersonaChange;
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

  PESteamAPICallFailure = ^ESteamAPICallFailure;
  ESteamAPICallFailure = (
    k_ESteamAPICallFailureNone = -1,
    k_ESteamAPICallFailureSteamGone = 0,
    k_ESteamAPICallFailureNetworkFailure = 1,
    k_ESteamAPICallFailureInvalidHandle = 2,
    k_ESteamAPICallFailureMismatchedCallback = 3
  );

  PEGamepadTextInputMode = ^EGamepadTextInputMode;
  EGamepadTextInputMode = (
    k_EGamepadTextInputModeNormal = 0,
    k_EGamepadTextInputModePassword = 1
  );

  PEGamepadTextInputLineMode = ^EGamepadTextInputLineMode;
  EGamepadTextInputLineMode = (
    k_EGamepadTextInputLineModeSingleLine = 0,
    k_EGamepadTextInputLineModeMultipleLines = 1
  );

  PEFloatingGamepadTextInputMode = ^EFloatingGamepadTextInputMode;
  EFloatingGamepadTextInputMode = (
    k_EFloatingGamepadTextInputModeModeSingleLine = 0,
    k_EFloatingGamepadTextInputModeModeMultipleLines = 1,
    k_EFloatingGamepadTextInputModeModeEmail = 2,
    k_EFloatingGamepadTextInputModeModeNumeric = 3
  );

  PETextFilteringContext = ^ETextFilteringContext;
  ETextFilteringContext = (
    k_ETextFilteringContextUnknown = 0,
    k_ETextFilteringContextGameContent = 1,
    k_ETextFilteringContextChat = 2,
    k_ETextFilteringContextName = 3
  );

  PECheckFileSignature = ^ECheckFileSignature;
  ECheckFileSignature = (
    k_ECheckFileSignatureInvalidSignature = 0,
    k_ECheckFileSignatureValidSignature = 1,
    k_ECheckFileSignatureFileNotFound = 2,
    k_ECheckFileSignatureNoSignaturesFoundForThisApp = 3,
    k_ECheckFileSignatureNoSignaturesFoundForThisFile = 4
  );

  PEMatchMakingServerResponse = ^EMatchMakingServerResponse;
  EMatchMakingServerResponse = (
    eServerResponded = 0,
    eServerFailedToRespond = 1,
    eNoServersListedOnMasterServer = 2
  );

  PELobbyType = ^ELobbyType;
  ELobbyType = (
    k_ELobbyTypePrivate = 0,
    k_ELobbyTypeFriendsOnly = 1,
    k_ELobbyTypePublic = 2,
    k_ELobbyTypeInvisible = 3,
    k_ELobbyTypePrivateUnique = 4
  );

  PELobbyComparison = ^ELobbyComparison;
  ELobbyComparison = (
    k_ELobbyComparisonEqualToOrLessThan = -2,
    k_ELobbyComparisonLessThan = -1,
    k_ELobbyComparisonEqual = 0,
    k_ELobbyComparisonGreaterThan = 1,
    k_ELobbyComparisonEqualToOrGreaterThan = 2,
    k_ELobbyComparisonNotEqual = 3
  );

  PELobbyDistanceFilter = ^ELobbyDistanceFilter;
  ELobbyDistanceFilter = (
    k_ELobbyDistanceFilterClose = 0,
    k_ELobbyDistanceFilterDefault = 1,
    k_ELobbyDistanceFilterFar = 2,
    k_ELobbyDistanceFilterWorldwide = 3
  );

  PEChatMemberStateChange = ^EChatMemberStateChange;
  EChatMemberStateChange = (
    k_EChatMemberStateChangeEntered = 1,
    k_EChatMemberStateChangeLeft = 2,
    k_EChatMemberStateChangeDisconnected = 4,
    k_EChatMemberStateChangeKicked = 8,
    k_EChatMemberStateChangeBanned = 16
  );

  PESteamPartyBeaconLocationType = ^ESteamPartyBeaconLocationType;
  ESteamPartyBeaconLocationType = (
    k_ESteamPartyBeaconLocationType_Invalid = 0,
    k_ESteamPartyBeaconLocationType_ChatGroup = 1,
    k_ESteamPartyBeaconLocationType_Max = 2
  );

  PESteamPartyBeaconLocationData = ^ESteamPartyBeaconLocationData;
  ESteamPartyBeaconLocationData = (
    k_ESteamPartyBeaconLocationDataInvalid = 0,
    k_ESteamPartyBeaconLocationDataName = 1,
    k_ESteamPartyBeaconLocationDataIconURLSmall = 2,
    k_ESteamPartyBeaconLocationDataIconURLMedium = 3,
    k_ESteamPartyBeaconLocationDataIconURLLarge = 4
  );

  PERemoteStoragePlatform = ^ERemoteStoragePlatform;
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

  PERemoteStoragePublishedFileVisibility = ^ERemoteStoragePublishedFileVisibility;
  ERemoteStoragePublishedFileVisibility = (
    k_ERemoteStoragePublishedFileVisibilityPublic = 0,
    k_ERemoteStoragePublishedFileVisibilityFriendsOnly = 1,
    k_ERemoteStoragePublishedFileVisibilityPrivate = 2,
    k_ERemoteStoragePublishedFileVisibilityUnlisted = 3
  );

  PEWorkshopFileType = ^EWorkshopFileType;
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

  PEWorkshopVote = ^EWorkshopVote;
  EWorkshopVote = (
    k_EWorkshopVoteUnvoted = 0,
    k_EWorkshopVoteFor = 1,
    k_EWorkshopVoteAgainst = 2,
    k_EWorkshopVoteLater = 3
  );

  PEWorkshopFileAction = ^EWorkshopFileAction;
  EWorkshopFileAction = (
    k_EWorkshopFileActionPlayed = 0,
    k_EWorkshopFileActionCompleted = 1
  );

  PEWorkshopEnumerationType = ^EWorkshopEnumerationType;
  EWorkshopEnumerationType = (
    k_EWorkshopEnumerationTypeRankedByVote = 0,
    k_EWorkshopEnumerationTypeRecent = 1,
    k_EWorkshopEnumerationTypeTrending = 2,
    k_EWorkshopEnumerationTypeFavoritesOfFriends = 3,
    k_EWorkshopEnumerationTypeVotedByFriends = 4,
    k_EWorkshopEnumerationTypeContentByFriends = 5,
    k_EWorkshopEnumerationTypeRecentFromFollowedUsers = 6
  );

  PEWorkshopVideoProvider = ^EWorkshopVideoProvider;
  EWorkshopVideoProvider = (
    k_EWorkshopVideoProviderNone = 0,
    k_EWorkshopVideoProviderYoutube = 1
  );

  PEUGCReadAction = ^EUGCReadAction;
  EUGCReadAction = (
    k_EUGCRead_ContinueReadingUntilFinished = 0,
    k_EUGCRead_ContinueReading = 1,
    k_EUGCRead_Close = 2
  );

  PERemoteStorageLocalFileChange = ^ERemoteStorageLocalFileChange;
  ERemoteStorageLocalFileChange = (
    k_ERemoteStorageLocalFileChange_Invalid = 0,
    k_ERemoteStorageLocalFileChange_FileUpdated = 1,
    k_ERemoteStorageLocalFileChange_FileDeleted = 2
  );

  PERemoteStorageFilePathType = ^ERemoteStorageFilePathType;
  ERemoteStorageFilePathType = (
    k_ERemoteStorageFilePathType_Invalid = 0,
    k_ERemoteStorageFilePathType_Absolute = 1,
    k_ERemoteStorageFilePathType_APIFilename = 2
  );

  PELeaderboardDataRequest = ^ELeaderboardDataRequest;
  ELeaderboardDataRequest = (
    k_ELeaderboardDataRequestGlobal = 0,
    k_ELeaderboardDataRequestGlobalAroundUser = 1,
    k_ELeaderboardDataRequestFriends = 2,
    k_ELeaderboardDataRequestUsers = 3
  );

  PELeaderboardSortMethod = ^ELeaderboardSortMethod;
  ELeaderboardSortMethod = (
    k_ELeaderboardSortMethodNone = 0,
    k_ELeaderboardSortMethodAscending = 1,
    k_ELeaderboardSortMethodDescending = 2
  );

  PELeaderboardDisplayType = ^ELeaderboardDisplayType;
  ELeaderboardDisplayType = (
    k_ELeaderboardDisplayTypeNone = 0,
    k_ELeaderboardDisplayTypeNumeric = 1,
    k_ELeaderboardDisplayTypeTimeSeconds = 2,
    k_ELeaderboardDisplayTypeTimeMilliSeconds = 3
  );

  PELeaderboardUploadScoreMethod = ^ELeaderboardUploadScoreMethod;
  ELeaderboardUploadScoreMethod = (
    k_ELeaderboardUploadScoreMethodNone = 0,
    k_ELeaderboardUploadScoreMethodKeepBest = 1,
    k_ELeaderboardUploadScoreMethodForceUpdate = 2
  );

  PERegisterActivationCodeResult = ^ERegisterActivationCodeResult;
  ERegisterActivationCodeResult = (
    k_ERegisterActivationCodeResultOK = 0,
    k_ERegisterActivationCodeResultFail = 1,
    k_ERegisterActivationCodeResultAlreadyRegistered = 2,
    k_ERegisterActivationCodeResultTimeout = 3,
    k_ERegisterActivationCodeAlreadyOwned = 4
  );

  PEP2PSessionError = ^EP2PSessionError;
  EP2PSessionError = (
    k_EP2PSessionErrorNone = 0,
    k_EP2PSessionErrorNoRightsToApp = 2,
    k_EP2PSessionErrorTimeout = 4,
    k_EP2PSessionErrorNotRunningApp_DELETED = 1,
    k_EP2PSessionErrorDestinationNotLoggedIn_DELETED = 3,
    k_EP2PSessionErrorMax = 5
  );

  PEP2PSend = ^EP2PSend;
  EP2PSend = (
    k_EP2PSendUnreliable = 0,
    k_EP2PSendUnreliableNoDelay = 1,
    k_EP2PSendReliable = 2,
    k_EP2PSendReliableWithBuffering = 3
  );

  PESNetSocketState = ^ESNetSocketState;
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

  PESNetSocketConnectionType = ^ESNetSocketConnectionType;
  ESNetSocketConnectionType = (
    k_ESNetSocketConnectionTypeNotConnected = 0,
    k_ESNetSocketConnectionTypeUDP = 1,
    k_ESNetSocketConnectionTypeUDPRelay = 2
  );

  PEVRScreenshotType = ^EVRScreenshotType;
  EVRScreenshotType = (
    k_EVRScreenshotType_None = 0,
    k_EVRScreenshotType_Mono = 1,
    k_EVRScreenshotType_Stereo = 2,
    k_EVRScreenshotType_MonoCubemap = 3,
    k_EVRScreenshotType_MonoPanorama = 4,
    k_EVRScreenshotType_StereoPanorama = 5
  );

  PAudioPlayback_Status = ^AudioPlayback_Status;
  AudioPlayback_Status = (
    AudioPlayback_Undefined = 0,
    AudioPlayback_Playing = 1,
    AudioPlayback_Paused = 2,
    AudioPlayback_Idle = 3
  );

  PEHTTPMethod = ^EHTTPMethod;
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

  PEHTTPStatusCode = ^EHTTPStatusCode;
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
    k_EHTTPStatusCode444ConnectionClosed = 444,
    k_EHTTPStatusCode500InternalServerError = 500,
    k_EHTTPStatusCode501NotImplemented = 501,
    k_EHTTPStatusCode502BadGateway = 502,
    k_EHTTPStatusCode503ServiceUnavailable = 503,
    k_EHTTPStatusCode504GatewayTimeout = 504,
    k_EHTTPStatusCode505HTTPVersionNotSupported = 505,
    k_EHTTPStatusCode5xxUnknown = 599
  );

  PEInputSourceMode = ^EInputSourceMode;
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

  PEInputActionOrigin = ^EInputActionOrigin;
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
    k_EInputActionOrigin_XBoxOne_LeftGrip_Lower = 143,
    k_EInputActionOrigin_XBoxOne_LeftGrip_Upper = 144,
    k_EInputActionOrigin_XBoxOne_RightGrip_Lower = 145,
    k_EInputActionOrigin_XBoxOne_RightGrip_Upper = 146,
    k_EInputActionOrigin_XBoxOne_Share = 147,
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
    k_EInputActionOrigin_PS5_X = 258,
    k_EInputActionOrigin_PS5_Circle = 259,
    k_EInputActionOrigin_PS5_Triangle = 260,
    k_EInputActionOrigin_PS5_Square = 261,
    k_EInputActionOrigin_PS5_LeftBumper = 262,
    k_EInputActionOrigin_PS5_RightBumper = 263,
    k_EInputActionOrigin_PS5_Option = 264,
    k_EInputActionOrigin_PS5_Create = 265,
    k_EInputActionOrigin_PS5_Mute = 266,
    k_EInputActionOrigin_PS5_LeftPad_Touch = 267,
    k_EInputActionOrigin_PS5_LeftPad_Swipe = 268,
    k_EInputActionOrigin_PS5_LeftPad_Click = 269,
    k_EInputActionOrigin_PS5_LeftPad_DPadNorth = 270,
    k_EInputActionOrigin_PS5_LeftPad_DPadSouth = 271,
    k_EInputActionOrigin_PS5_LeftPad_DPadWest = 272,
    k_EInputActionOrigin_PS5_LeftPad_DPadEast = 273,
    k_EInputActionOrigin_PS5_RightPad_Touch = 274,
    k_EInputActionOrigin_PS5_RightPad_Swipe = 275,
    k_EInputActionOrigin_PS5_RightPad_Click = 276,
    k_EInputActionOrigin_PS5_RightPad_DPadNorth = 277,
    k_EInputActionOrigin_PS5_RightPad_DPadSouth = 278,
    k_EInputActionOrigin_PS5_RightPad_DPadWest = 279,
    k_EInputActionOrigin_PS5_RightPad_DPadEast = 280,
    k_EInputActionOrigin_PS5_CenterPad_Touch = 281,
    k_EInputActionOrigin_PS5_CenterPad_Swipe = 282,
    k_EInputActionOrigin_PS5_CenterPad_Click = 283,
    k_EInputActionOrigin_PS5_CenterPad_DPadNorth = 284,
    k_EInputActionOrigin_PS5_CenterPad_DPadSouth = 285,
    k_EInputActionOrigin_PS5_CenterPad_DPadWest = 286,
    k_EInputActionOrigin_PS5_CenterPad_DPadEast = 287,
    k_EInputActionOrigin_PS5_LeftTrigger_Pull = 288,
    k_EInputActionOrigin_PS5_LeftTrigger_Click = 289,
    k_EInputActionOrigin_PS5_RightTrigger_Pull = 290,
    k_EInputActionOrigin_PS5_RightTrigger_Click = 291,
    k_EInputActionOrigin_PS5_LeftStick_Move = 292,
    k_EInputActionOrigin_PS5_LeftStick_Click = 293,
    k_EInputActionOrigin_PS5_LeftStick_DPadNorth = 294,
    k_EInputActionOrigin_PS5_LeftStick_DPadSouth = 295,
    k_EInputActionOrigin_PS5_LeftStick_DPadWest = 296,
    k_EInputActionOrigin_PS5_LeftStick_DPadEast = 297,
    k_EInputActionOrigin_PS5_RightStick_Move = 298,
    k_EInputActionOrigin_PS5_RightStick_Click = 299,
    k_EInputActionOrigin_PS5_RightStick_DPadNorth = 300,
    k_EInputActionOrigin_PS5_RightStick_DPadSouth = 301,
    k_EInputActionOrigin_PS5_RightStick_DPadWest = 302,
    k_EInputActionOrigin_PS5_RightStick_DPadEast = 303,
    k_EInputActionOrigin_PS5_DPad_North = 304,
    k_EInputActionOrigin_PS5_DPad_South = 305,
    k_EInputActionOrigin_PS5_DPad_West = 306,
    k_EInputActionOrigin_PS5_DPad_East = 307,
    k_EInputActionOrigin_PS5_Gyro_Move = 308,
    k_EInputActionOrigin_PS5_Gyro_Pitch = 309,
    k_EInputActionOrigin_PS5_Gyro_Yaw = 310,
    k_EInputActionOrigin_PS5_Gyro_Roll = 311,
    k_EInputActionOrigin_PS5_DPad_Move = 312,
    k_EInputActionOrigin_PS5_Reserved1 = 313,
    k_EInputActionOrigin_PS5_Reserved2 = 314,
    k_EInputActionOrigin_PS5_Reserved3 = 315,
    k_EInputActionOrigin_PS5_Reserved4 = 316,
    k_EInputActionOrigin_PS5_Reserved5 = 317,
    k_EInputActionOrigin_PS5_Reserved6 = 318,
    k_EInputActionOrigin_PS5_Reserved7 = 319,
    k_EInputActionOrigin_PS5_Reserved8 = 320,
    k_EInputActionOrigin_PS5_Reserved9 = 321,
    k_EInputActionOrigin_PS5_Reserved10 = 322,
    k_EInputActionOrigin_PS5_Reserved11 = 323,
    k_EInputActionOrigin_PS5_Reserved12 = 324,
    k_EInputActionOrigin_PS5_Reserved13 = 325,
    k_EInputActionOrigin_PS5_Reserved14 = 326,
    k_EInputActionOrigin_PS5_Reserved15 = 327,
    k_EInputActionOrigin_PS5_Reserved16 = 328,
    k_EInputActionOrigin_PS5_Reserved17 = 329,
    k_EInputActionOrigin_PS5_Reserved18 = 330,
    k_EInputActionOrigin_PS5_Reserved19 = 331,
    k_EInputActionOrigin_PS5_Reserved20 = 332,
    k_EInputActionOrigin_SteamDeck_A = 333,
    k_EInputActionOrigin_SteamDeck_B = 334,
    k_EInputActionOrigin_SteamDeck_X = 335,
    k_EInputActionOrigin_SteamDeck_Y = 336,
    k_EInputActionOrigin_SteamDeck_L1 = 337,
    k_EInputActionOrigin_SteamDeck_R1 = 338,
    k_EInputActionOrigin_SteamDeck_Menu = 339,
    k_EInputActionOrigin_SteamDeck_View = 340,
    k_EInputActionOrigin_SteamDeck_LeftPad_Touch = 341,
    k_EInputActionOrigin_SteamDeck_LeftPad_Swipe = 342,
    k_EInputActionOrigin_SteamDeck_LeftPad_Click = 343,
    k_EInputActionOrigin_SteamDeck_LeftPad_DPadNorth = 344,
    k_EInputActionOrigin_SteamDeck_LeftPad_DPadSouth = 345,
    k_EInputActionOrigin_SteamDeck_LeftPad_DPadWest = 346,
    k_EInputActionOrigin_SteamDeck_LeftPad_DPadEast = 347,
    k_EInputActionOrigin_SteamDeck_RightPad_Touch = 348,
    k_EInputActionOrigin_SteamDeck_RightPad_Swipe = 349,
    k_EInputActionOrigin_SteamDeck_RightPad_Click = 350,
    k_EInputActionOrigin_SteamDeck_RightPad_DPadNorth = 351,
    k_EInputActionOrigin_SteamDeck_RightPad_DPadSouth = 352,
    k_EInputActionOrigin_SteamDeck_RightPad_DPadWest = 353,
    k_EInputActionOrigin_SteamDeck_RightPad_DPadEast = 354,
    k_EInputActionOrigin_SteamDeck_L2_SoftPull = 355,
    k_EInputActionOrigin_SteamDeck_L2 = 356,
    k_EInputActionOrigin_SteamDeck_R2_SoftPull = 357,
    k_EInputActionOrigin_SteamDeck_R2 = 358,
    k_EInputActionOrigin_SteamDeck_LeftStick_Move = 359,
    k_EInputActionOrigin_SteamDeck_L3 = 360,
    k_EInputActionOrigin_SteamDeck_LeftStick_DPadNorth = 361,
    k_EInputActionOrigin_SteamDeck_LeftStick_DPadSouth = 362,
    k_EInputActionOrigin_SteamDeck_LeftStick_DPadWest = 363,
    k_EInputActionOrigin_SteamDeck_LeftStick_DPadEast = 364,
    k_EInputActionOrigin_SteamDeck_LeftStick_Touch = 365,
    k_EInputActionOrigin_SteamDeck_RightStick_Move = 366,
    k_EInputActionOrigin_SteamDeck_R3 = 367,
    k_EInputActionOrigin_SteamDeck_RightStick_DPadNorth = 368,
    k_EInputActionOrigin_SteamDeck_RightStick_DPadSouth = 369,
    k_EInputActionOrigin_SteamDeck_RightStick_DPadWest = 370,
    k_EInputActionOrigin_SteamDeck_RightStick_DPadEast = 371,
    k_EInputActionOrigin_SteamDeck_RightStick_Touch = 372,
    k_EInputActionOrigin_SteamDeck_L4 = 373,
    k_EInputActionOrigin_SteamDeck_R4 = 374,
    k_EInputActionOrigin_SteamDeck_L5 = 375,
    k_EInputActionOrigin_SteamDeck_R5 = 376,
    k_EInputActionOrigin_SteamDeck_DPad_Move = 377,
    k_EInputActionOrigin_SteamDeck_DPad_North = 378,
    k_EInputActionOrigin_SteamDeck_DPad_South = 379,
    k_EInputActionOrigin_SteamDeck_DPad_West = 380,
    k_EInputActionOrigin_SteamDeck_DPad_East = 381,
    k_EInputActionOrigin_SteamDeck_Gyro_Move = 382,
    k_EInputActionOrigin_SteamDeck_Gyro_Pitch = 383,
    k_EInputActionOrigin_SteamDeck_Gyro_Yaw = 384,
    k_EInputActionOrigin_SteamDeck_Gyro_Roll = 385,
    k_EInputActionOrigin_SteamDeck_Reserved1 = 386,
    k_EInputActionOrigin_SteamDeck_Reserved2 = 387,
    k_EInputActionOrigin_SteamDeck_Reserved3 = 388,
    k_EInputActionOrigin_SteamDeck_Reserved4 = 389,
    k_EInputActionOrigin_SteamDeck_Reserved5 = 390,
    k_EInputActionOrigin_SteamDeck_Reserved6 = 391,
    k_EInputActionOrigin_SteamDeck_Reserved7 = 392,
    k_EInputActionOrigin_SteamDeck_Reserved8 = 393,
    k_EInputActionOrigin_SteamDeck_Reserved9 = 394,
    k_EInputActionOrigin_SteamDeck_Reserved10 = 395,
    k_EInputActionOrigin_SteamDeck_Reserved11 = 396,
    k_EInputActionOrigin_SteamDeck_Reserved12 = 397,
    k_EInputActionOrigin_SteamDeck_Reserved13 = 398,
    k_EInputActionOrigin_SteamDeck_Reserved14 = 399,
    k_EInputActionOrigin_SteamDeck_Reserved15 = 400,
    k_EInputActionOrigin_SteamDeck_Reserved16 = 401,
    k_EInputActionOrigin_SteamDeck_Reserved17 = 402,
    k_EInputActionOrigin_SteamDeck_Reserved18 = 403,
    k_EInputActionOrigin_SteamDeck_Reserved19 = 404,
    k_EInputActionOrigin_SteamDeck_Reserved20 = 405,
    k_EInputActionOrigin_Count = 406,
    k_EInputActionOrigin_MaximumPossibleValue = 32767
  );

  PEXboxOrigin = ^EXboxOrigin;
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

  PESteamControllerPad = ^ESteamControllerPad;
  ESteamControllerPad = (
    k_ESteamControllerPad_Left = 0,
    k_ESteamControllerPad_Right = 1
  );

  PEControllerHapticLocation = ^EControllerHapticLocation;
  EControllerHapticLocation = (
    k_EControllerHapticLocation_Left = 1,
    k_EControllerHapticLocation_Right = 2,
    k_EControllerHapticLocation_Both = 3
  );

  PEControllerHapticType = ^EControllerHapticType;
  EControllerHapticType = (
    k_EControllerHapticType_Off = 0,
    k_EControllerHapticType_Tick = 1,
    k_EControllerHapticType_Click = 2
  );

  PESteamInputType = ^ESteamInputType;
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
    k_ESteamInputType_PS5Controller = 13,
    k_ESteamInputType_SteamDeckController = 14,
    k_ESteamInputType_Count = 15,
    k_ESteamInputType_MaximumPossibleValue = 255
  );

  PESteamInputConfigurationEnableType = ^ESteamInputConfigurationEnableType;
  ESteamInputConfigurationEnableType = (
    k_ESteamInputConfigurationEnableType_None = 0,
    k_ESteamInputConfigurationEnableType_Playstation = 1,
    k_ESteamInputConfigurationEnableType_Xbox = 2,
    k_ESteamInputConfigurationEnableType_Generic = 4,
    k_ESteamInputConfigurationEnableType_Switch = 8
  );

  PESteamInputLEDFlag = ^ESteamInputLEDFlag;
  ESteamInputLEDFlag = (
    k_ESteamInputLEDFlag_SetColor = 0,
    k_ESteamInputLEDFlag_RestoreUserDefault = 1
  );

  PESteamInputGlyphSize = ^ESteamInputGlyphSize;
  ESteamInputGlyphSize = (
    k_ESteamInputGlyphSize_Small = 0,
    k_ESteamInputGlyphSize_Medium = 1,
    k_ESteamInputGlyphSize_Large = 2,
    k_ESteamInputGlyphSize_Count = 3
  );

  PESteamInputGlyphStyle = ^ESteamInputGlyphStyle;
  ESteamInputGlyphStyle = (
    ESteamInputGlyphStyle_Knockout = 0,
    ESteamInputGlyphStyle_Light = 1,
    ESteamInputGlyphStyle_Dark = 2,
    ESteamInputGlyphStyle_NeutralColorABXY = 16,
    ESteamInputGlyphStyle_SolidABXY = 32
  );

  PESteamInputActionEventType = ^ESteamInputActionEventType;
  ESteamInputActionEventType = (
    ESteamInputActionEventType_DigitalAction = 0,
    ESteamInputActionEventType_AnalogAction = 1
  );

  PEControllerActionOrigin = ^EControllerActionOrigin;
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
    k_EControllerActionOrigin_PS5_X = 245,
    k_EControllerActionOrigin_PS5_Circle = 246,
    k_EControllerActionOrigin_PS5_Triangle = 247,
    k_EControllerActionOrigin_PS5_Square = 248,
    k_EControllerActionOrigin_PS5_LeftBumper = 249,
    k_EControllerActionOrigin_PS5_RightBumper = 250,
    k_EControllerActionOrigin_PS5_Option = 251,
    k_EControllerActionOrigin_PS5_Create = 252,
    k_EControllerActionOrigin_PS5_Mute = 253,
    k_EControllerActionOrigin_PS5_LeftPad_Touch = 254,
    k_EControllerActionOrigin_PS5_LeftPad_Swipe = 255,
    k_EControllerActionOrigin_PS5_LeftPad_Click = 256,
    k_EControllerActionOrigin_PS5_LeftPad_DPadNorth = 257,
    k_EControllerActionOrigin_PS5_LeftPad_DPadSouth = 258,
    k_EControllerActionOrigin_PS5_LeftPad_DPadWest = 259,
    k_EControllerActionOrigin_PS5_LeftPad_DPadEast = 260,
    k_EControllerActionOrigin_PS5_RightPad_Touch = 261,
    k_EControllerActionOrigin_PS5_RightPad_Swipe = 262,
    k_EControllerActionOrigin_PS5_RightPad_Click = 263,
    k_EControllerActionOrigin_PS5_RightPad_DPadNorth = 264,
    k_EControllerActionOrigin_PS5_RightPad_DPadSouth = 265,
    k_EControllerActionOrigin_PS5_RightPad_DPadWest = 266,
    k_EControllerActionOrigin_PS5_RightPad_DPadEast = 267,
    k_EControllerActionOrigin_PS5_CenterPad_Touch = 268,
    k_EControllerActionOrigin_PS5_CenterPad_Swipe = 269,
    k_EControllerActionOrigin_PS5_CenterPad_Click = 270,
    k_EControllerActionOrigin_PS5_CenterPad_DPadNorth = 271,
    k_EControllerActionOrigin_PS5_CenterPad_DPadSouth = 272,
    k_EControllerActionOrigin_PS5_CenterPad_DPadWest = 273,
    k_EControllerActionOrigin_PS5_CenterPad_DPadEast = 274,
    k_EControllerActionOrigin_PS5_LeftTrigger_Pull = 275,
    k_EControllerActionOrigin_PS5_LeftTrigger_Click = 276,
    k_EControllerActionOrigin_PS5_RightTrigger_Pull = 277,
    k_EControllerActionOrigin_PS5_RightTrigger_Click = 278,
    k_EControllerActionOrigin_PS5_LeftStick_Move = 279,
    k_EControllerActionOrigin_PS5_LeftStick_Click = 280,
    k_EControllerActionOrigin_PS5_LeftStick_DPadNorth = 281,
    k_EControllerActionOrigin_PS5_LeftStick_DPadSouth = 282,
    k_EControllerActionOrigin_PS5_LeftStick_DPadWest = 283,
    k_EControllerActionOrigin_PS5_LeftStick_DPadEast = 284,
    k_EControllerActionOrigin_PS5_RightStick_Move = 285,
    k_EControllerActionOrigin_PS5_RightStick_Click = 286,
    k_EControllerActionOrigin_PS5_RightStick_DPadNorth = 287,
    k_EControllerActionOrigin_PS5_RightStick_DPadSouth = 288,
    k_EControllerActionOrigin_PS5_RightStick_DPadWest = 289,
    k_EControllerActionOrigin_PS5_RightStick_DPadEast = 290,
    k_EControllerActionOrigin_PS5_DPad_Move = 291,
    k_EControllerActionOrigin_PS5_DPad_North = 292,
    k_EControllerActionOrigin_PS5_DPad_South = 293,
    k_EControllerActionOrigin_PS5_DPad_West = 294,
    k_EControllerActionOrigin_PS5_DPad_East = 295,
    k_EControllerActionOrigin_PS5_Gyro_Move = 296,
    k_EControllerActionOrigin_PS5_Gyro_Pitch = 297,
    k_EControllerActionOrigin_PS5_Gyro_Yaw = 298,
    k_EControllerActionOrigin_PS5_Gyro_Roll = 299,
    k_EControllerActionOrigin_XBoxOne_LeftGrip_Lower = 300,
    k_EControllerActionOrigin_XBoxOne_LeftGrip_Upper = 301,
    k_EControllerActionOrigin_XBoxOne_RightGrip_Lower = 302,
    k_EControllerActionOrigin_XBoxOne_RightGrip_Upper = 303,
    k_EControllerActionOrigin_XBoxOne_Share = 304,
    k_EControllerActionOrigin_SteamDeck_A = 305,
    k_EControllerActionOrigin_SteamDeck_B = 306,
    k_EControllerActionOrigin_SteamDeck_X = 307,
    k_EControllerActionOrigin_SteamDeck_Y = 308,
    k_EControllerActionOrigin_SteamDeck_L1 = 309,
    k_EControllerActionOrigin_SteamDeck_R1 = 310,
    k_EControllerActionOrigin_SteamDeck_Menu = 311,
    k_EControllerActionOrigin_SteamDeck_View = 312,
    k_EControllerActionOrigin_SteamDeck_LeftPad_Touch = 313,
    k_EControllerActionOrigin_SteamDeck_LeftPad_Swipe = 314,
    k_EControllerActionOrigin_SteamDeck_LeftPad_Click = 315,
    k_EControllerActionOrigin_SteamDeck_LeftPad_DPadNorth = 316,
    k_EControllerActionOrigin_SteamDeck_LeftPad_DPadSouth = 317,
    k_EControllerActionOrigin_SteamDeck_LeftPad_DPadWest = 318,
    k_EControllerActionOrigin_SteamDeck_LeftPad_DPadEast = 319,
    k_EControllerActionOrigin_SteamDeck_RightPad_Touch = 320,
    k_EControllerActionOrigin_SteamDeck_RightPad_Swipe = 321,
    k_EControllerActionOrigin_SteamDeck_RightPad_Click = 322,
    k_EControllerActionOrigin_SteamDeck_RightPad_DPadNorth = 323,
    k_EControllerActionOrigin_SteamDeck_RightPad_DPadSouth = 324,
    k_EControllerActionOrigin_SteamDeck_RightPad_DPadWest = 325,
    k_EControllerActionOrigin_SteamDeck_RightPad_DPadEast = 326,
    k_EControllerActionOrigin_SteamDeck_L2_SoftPull = 327,
    k_EControllerActionOrigin_SteamDeck_L2 = 328,
    k_EControllerActionOrigin_SteamDeck_R2_SoftPull = 329,
    k_EControllerActionOrigin_SteamDeck_R2 = 330,
    k_EControllerActionOrigin_SteamDeck_LeftStick_Move = 331,
    k_EControllerActionOrigin_SteamDeck_L3 = 332,
    k_EControllerActionOrigin_SteamDeck_LeftStick_DPadNorth = 333,
    k_EControllerActionOrigin_SteamDeck_LeftStick_DPadSouth = 334,
    k_EControllerActionOrigin_SteamDeck_LeftStick_DPadWest = 335,
    k_EControllerActionOrigin_SteamDeck_LeftStick_DPadEast = 336,
    k_EControllerActionOrigin_SteamDeck_LeftStick_Touch = 337,
    k_EControllerActionOrigin_SteamDeck_RightStick_Move = 338,
    k_EControllerActionOrigin_SteamDeck_R3 = 339,
    k_EControllerActionOrigin_SteamDeck_RightStick_DPadNorth = 340,
    k_EControllerActionOrigin_SteamDeck_RightStick_DPadSouth = 341,
    k_EControllerActionOrigin_SteamDeck_RightStick_DPadWest = 342,
    k_EControllerActionOrigin_SteamDeck_RightStick_DPadEast = 343,
    k_EControllerActionOrigin_SteamDeck_RightStick_Touch = 344,
    k_EControllerActionOrigin_SteamDeck_L4 = 345,
    k_EControllerActionOrigin_SteamDeck_R4 = 346,
    k_EControllerActionOrigin_SteamDeck_L5 = 347,
    k_EControllerActionOrigin_SteamDeck_R5 = 348,
    k_EControllerActionOrigin_SteamDeck_DPad_Move = 349,
    k_EControllerActionOrigin_SteamDeck_DPad_North = 350,
    k_EControllerActionOrigin_SteamDeck_DPad_South = 351,
    k_EControllerActionOrigin_SteamDeck_DPad_West = 352,
    k_EControllerActionOrigin_SteamDeck_DPad_East = 353,
    k_EControllerActionOrigin_SteamDeck_Gyro_Move = 354,
    k_EControllerActionOrigin_SteamDeck_Gyro_Pitch = 355,
    k_EControllerActionOrigin_SteamDeck_Gyro_Yaw = 356,
    k_EControllerActionOrigin_SteamDeck_Gyro_Roll = 357,
    k_EControllerActionOrigin_SteamDeck_Reserved1 = 358,
    k_EControllerActionOrigin_SteamDeck_Reserved2 = 359,
    k_EControllerActionOrigin_SteamDeck_Reserved3 = 360,
    k_EControllerActionOrigin_SteamDeck_Reserved4 = 361,
    k_EControllerActionOrigin_SteamDeck_Reserved5 = 362,
    k_EControllerActionOrigin_SteamDeck_Reserved6 = 363,
    k_EControllerActionOrigin_SteamDeck_Reserved7 = 364,
    k_EControllerActionOrigin_SteamDeck_Reserved8 = 365,
    k_EControllerActionOrigin_SteamDeck_Reserved9 = 366,
    k_EControllerActionOrigin_SteamDeck_Reserved10 = 367,
    k_EControllerActionOrigin_SteamDeck_Reserved11 = 368,
    k_EControllerActionOrigin_SteamDeck_Reserved12 = 369,
    k_EControllerActionOrigin_SteamDeck_Reserved13 = 370,
    k_EControllerActionOrigin_SteamDeck_Reserved14 = 371,
    k_EControllerActionOrigin_SteamDeck_Reserved15 = 372,
    k_EControllerActionOrigin_SteamDeck_Reserved16 = 373,
    k_EControllerActionOrigin_SteamDeck_Reserved17 = 374,
    k_EControllerActionOrigin_SteamDeck_Reserved18 = 375,
    k_EControllerActionOrigin_SteamDeck_Reserved19 = 376,
    k_EControllerActionOrigin_SteamDeck_Reserved20 = 377,
    k_EControllerActionOrigin_Count = 378,
    k_EControllerActionOrigin_MaximumPossibleValue = 32767
  );

  PESteamControllerLEDFlag = ^ESteamControllerLEDFlag;
  ESteamControllerLEDFlag = (
    k_ESteamControllerLEDFlag_SetColor = 0,
    k_ESteamControllerLEDFlag_RestoreUserDefault = 1
  );

  PEUGCMatchingUGCType = ^EUGCMatchingUGCType;
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

  PEUserUGCList = ^EUserUGCList;
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

  PEUserUGCListSortOrder = ^EUserUGCListSortOrder;
  EUserUGCListSortOrder = (
    k_EUserUGCListSortOrder_CreationOrderDesc = 0,
    k_EUserUGCListSortOrder_CreationOrderAsc = 1,
    k_EUserUGCListSortOrder_TitleAsc = 2,
    k_EUserUGCListSortOrder_LastUpdatedDesc = 3,
    k_EUserUGCListSortOrder_SubscriptionDateDesc = 4,
    k_EUserUGCListSortOrder_VoteScoreDesc = 5,
    k_EUserUGCListSortOrder_ForModeration = 6
  );

  PEUGCQuery = ^EUGCQuery;
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
    k_EUGCQuery_RankedByLifetimePlaytimeSessions = 18,
    k_EUGCQuery_RankedByLastUpdatedDate = 19
  );

  PEItemUpdateStatus = ^EItemUpdateStatus;
  EItemUpdateStatus = (
    k_EItemUpdateStatusInvalid = 0,
    k_EItemUpdateStatusPreparingConfig = 1,
    k_EItemUpdateStatusPreparingContent = 2,
    k_EItemUpdateStatusUploadingContent = 3,
    k_EItemUpdateStatusUploadingPreviewFile = 4,
    k_EItemUpdateStatusCommittingChanges = 5
  );

  PEItemState = ^EItemState;
  EItemState = (
    k_EItemStateNone = 0,
    k_EItemStateSubscribed = 1,
    k_EItemStateLegacyItem = 2,
    k_EItemStateInstalled = 4,
    k_EItemStateNeedsUpdate = 8,
    k_EItemStateDownloading = 16,
    k_EItemStateDownloadPending = 32
  );

  PEItemStatistic = ^EItemStatistic;
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

  PEItemPreviewType = ^EItemPreviewType;
  EItemPreviewType = (
    k_EItemPreviewType_Image = 0,
    k_EItemPreviewType_YouTubeVideo = 1,
    k_EItemPreviewType_Sketchfab = 2,
    k_EItemPreviewType_EnvironmentMap_HorizontalCross = 3,
    k_EItemPreviewType_EnvironmentMap_LatLong = 4,
    k_EItemPreviewType_ReservedMax = 255
  );

  PESteamItemFlags = ^ESteamItemFlags;
  ESteamItemFlags = (
    k_ESteamItemNoTrade = 1,
    k_ESteamItemRemoved = 256,
    k_ESteamItemConsumed = 512
  );

  PEParentalFeature = ^EParentalFeature;
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

  PESteamDeviceFormFactor = ^ESteamDeviceFormFactor;
  ESteamDeviceFormFactor = (
    k_ESteamDeviceFormFactorUnknown = 0,
    k_ESteamDeviceFormFactorPhone = 1,
    k_ESteamDeviceFormFactorTablet = 2,
    k_ESteamDeviceFormFactorComputer = 3,
    k_ESteamDeviceFormFactorTV = 4
  );

  PESteamNetworkingAvailability = ^ESteamNetworkingAvailability;
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

  PESteamNetworkingIdentityType = ^ESteamNetworkingIdentityType;
  ESteamNetworkingIdentityType = (
    k_ESteamNetworkingIdentityType_Invalid = 0,
    k_ESteamNetworkingIdentityType_SteamID = 16,
    k_ESteamNetworkingIdentityType_XboxPairwiseID = 17,
    k_ESteamNetworkingIdentityType_SonyPSN = 18,
    k_ESteamNetworkingIdentityType_GoogleStadia = 19,
    k_ESteamNetworkingIdentityType_IPAddress = 1,
    k_ESteamNetworkingIdentityType_GenericString = 2,
    k_ESteamNetworkingIdentityType_GenericBytes = 3,
    k_ESteamNetworkingIdentityType_UnknownType = 4,
    k_ESteamNetworkingIdentityType__Force32bit = 2147483647
  );

  PESteamNetworkingFakeIPType = ^ESteamNetworkingFakeIPType;
  ESteamNetworkingFakeIPType = (
    k_ESteamNetworkingFakeIPType_Invalid = 0,
    k_ESteamNetworkingFakeIPType_NotFake = 1,
    k_ESteamNetworkingFakeIPType_GlobalIPv4 = 2,
    k_ESteamNetworkingFakeIPType_LocalIPv4 = 3,
    k_ESteamNetworkingFakeIPType__Force32Bit = 2147483647
  );

  PESteamNetworkingConnectionState = ^ESteamNetworkingConnectionState;
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

  PESteamNetConnectionEnd = ^ESteamNetConnectionEnd;
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
    k_ESteamNetConnectionEnd_Local_P2P_ICE_NoPublicAddresses = 3006,
    k_ESteamNetConnectionEnd_Local_Max = 3999,
    k_ESteamNetConnectionEnd_Remote_Min = 4000,
    k_ESteamNetConnectionEnd_Remote_Timeout = 4001,
    k_ESteamNetConnectionEnd_Remote_BadCrypt = 4002,
    k_ESteamNetConnectionEnd_Remote_BadCert = 4003,
    k_ESteamNetConnectionEnd_Remote_BadProtocolVersion = 4006,
    k_ESteamNetConnectionEnd_Remote_P2P_ICE_NoPublicAddresses = 4007,
    k_ESteamNetConnectionEnd_Remote_Max = 4999,
    k_ESteamNetConnectionEnd_Misc_Min = 5000,
    k_ESteamNetConnectionEnd_Misc_Generic = 5001,
    k_ESteamNetConnectionEnd_Misc_InternalError = 5002,
    k_ESteamNetConnectionEnd_Misc_Timeout = 5003,
    k_ESteamNetConnectionEnd_Misc_SteamConnectivity = 5005,
    k_ESteamNetConnectionEnd_Misc_NoRelaySessionsToClient = 5006,
    k_ESteamNetConnectionEnd_Misc_P2P_Rendezvous = 5008,
    k_ESteamNetConnectionEnd_Misc_P2P_NAT_Firewall = 5009,
    k_ESteamNetConnectionEnd_Misc_PeerSentNoConnection = 5010,
    k_ESteamNetConnectionEnd_Misc_Max = 5999,
    k_ESteamNetConnectionEnd__Force32Bit = 2147483647
  );

  PESteamNetworkingConfigScope = ^ESteamNetworkingConfigScope;
  ESteamNetworkingConfigScope = (
    k_ESteamNetworkingConfig_Global = 1,
    k_ESteamNetworkingConfig_SocketsInterface = 2,
    k_ESteamNetworkingConfig_ListenSocket = 3,
    k_ESteamNetworkingConfig_Connection = 4,
    k_ESteamNetworkingConfigScope__Force32Bit = 2147483647
  );

  PESteamNetworkingConfigDataType = ^ESteamNetworkingConfigDataType;
  ESteamNetworkingConfigDataType = (
    k_ESteamNetworkingConfig_Int32 = 1,
    k_ESteamNetworkingConfig_Int64 = 2,
    k_ESteamNetworkingConfig_Float = 3,
    k_ESteamNetworkingConfig_String = 4,
    k_ESteamNetworkingConfig_Ptr = 5,
    k_ESteamNetworkingConfigDataType__Force32Bit = 2147483647
  );

  PESteamNetworkingConfigValue = ^ESteamNetworkingConfigValue;
  ESteamNetworkingConfigValue = (
    k_ESteamNetworkingConfig_Invalid = 0,
    k_ESteamNetworkingConfig_TimeoutInitial = 24,
    k_ESteamNetworkingConfig_TimeoutConnected = 25,
    k_ESteamNetworkingConfig_SendBufferSize = 9,
    k_ESteamNetworkingConfig_ConnectionUserData = 40,
    k_ESteamNetworkingConfig_SendRateMin = 10,
    k_ESteamNetworkingConfig_SendRateMax = 11,
    k_ESteamNetworkingConfig_NagleTime = 12,
    k_ESteamNetworkingConfig_IP_AllowWithoutAuth = 23,
    k_ESteamNetworkingConfig_MTU_PacketSize = 32,
    k_ESteamNetworkingConfig_MTU_DataSize = 33,
    k_ESteamNetworkingConfig_Unencrypted = 34,
    k_ESteamNetworkingConfig_SymmetricConnect = 37,
    k_ESteamNetworkingConfig_LocalVirtualPort = 38,
    k_ESteamNetworkingConfig_DualWifi_Enable = 39,
    k_ESteamNetworkingConfig_EnableDiagnosticsUI = 46,
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
    k_ESteamNetworkingConfig_PacketTraceMaxBytes = 41,
    k_ESteamNetworkingConfig_FakeRateLimit_Send_Rate = 42,
    k_ESteamNetworkingConfig_FakeRateLimit_Send_Burst = 43,
    k_ESteamNetworkingConfig_FakeRateLimit_Recv_Rate = 44,
    k_ESteamNetworkingConfig_FakeRateLimit_Recv_Burst = 45,
    k_ESteamNetworkingConfig_Callback_ConnectionStatusChanged = 201,
    k_ESteamNetworkingConfig_Callback_AuthStatusChanged = 202,
    k_ESteamNetworkingConfig_Callback_RelayNetworkStatusChanged = 203,
    k_ESteamNetworkingConfig_Callback_MessagesSessionRequest = 204,
    k_ESteamNetworkingConfig_Callback_MessagesSessionFailed = 205,
    k_ESteamNetworkingConfig_Callback_CreateConnectionSignaling = 206,
    k_ESteamNetworkingConfig_Callback_FakeIPResult = 207,
    k_ESteamNetworkingConfig_P2P_STUN_ServerList = 103,
    k_ESteamNetworkingConfig_P2P_Transport_ICE_Enable = 104,
    k_ESteamNetworkingConfig_P2P_Transport_ICE_Penalty = 105,
    k_ESteamNetworkingConfig_P2P_Transport_SDR_Penalty = 106,
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
    k_ESteamNetworkingConfig_DELETED_EnumerateDevVars = 35,
    k_ESteamNetworkingConfigValue__Force32Bit = 2147483647
  );

  PESteamNetworkingGetConfigValueResult = ^ESteamNetworkingGetConfigValueResult;
  ESteamNetworkingGetConfigValueResult = (
    k_ESteamNetworkingGetConfigValue_BadValue = -1,
    k_ESteamNetworkingGetConfigValue_BadScopeObj = -2,
    k_ESteamNetworkingGetConfigValue_BufferTooSmall = -3,
    k_ESteamNetworkingGetConfigValue_OK = 1,
    k_ESteamNetworkingGetConfigValue_OKInherited = 2,
    k_ESteamNetworkingGetConfigValueResult__Force32Bit = 2147483647
  );

  PESteamNetworkingSocketsDebugOutputType = ^ESteamNetworkingSocketsDebugOutputType;
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

  PEServerMode = ^EServerMode;
  EServerMode = (
    eServerModeInvalid = 0,
    eServerModeNoAuthentication = 1,
    eServerModeAuthentication = 2,
    eServerModeAuthenticationAndSecure = 3
  );

  PISteamHTMLSurface__EHTMLMouseButton = ^ISteamHTMLSurface__EHTMLMouseButton;
  ISteamHTMLSurface__EHTMLMouseButton = (
    eHTMLMouseButton_Left = 0,
    eHTMLMouseButton_Right = 1,
    eHTMLMouseButton_Middle = 2
  );

  PISteamHTMLSurface__EMouseCursor = ^ISteamHTMLSurface__EMouseCursor;
  ISteamHTMLSurface__EMouseCursor = (
    dc_user = 0,
    dc_none = 1,
    dc_arrow = 2,
    dc_ibeam = 3,
    dc_hourglass = 4,
    dc_waitarrow = 5,
    dc_crosshair = 6,
    dc_up = 7,
    dc_sizenw = 8,
    dc_sizese = 9,
    dc_sizene = 10,
    dc_sizesw = 11,
    dc_sizew = 12,
    dc_sizee = 13,
    dc_sizen = 14,
    dc_sizes = 15,
    dc_sizewe = 16,
    dc_sizens = 17,
    dc_sizeall = 18,
    dc_no = 19,
    dc_hand = 20,
    dc_blank = 21,
    dc_middle_pan = 22,
    dc_north_pan = 23,
    dc_north_east_pan = 24,
    dc_east_pan = 25,
    dc_south_east_pan = 26,
    dc_south_pan = 27,
    dc_south_west_pan = 28,
    dc_west_pan = 29,
    dc_north_west_pan = 30,
    dc_alias = 31,
    dc_cell = 32,
    dc_colresize = 33,
    dc_copycur = 34,
    dc_verticaltext = 35,
    dc_rowresize = 36,
    dc_zoomin = 37,
    dc_zoomout = 38,
    dc_help = 39,
    dc_custom = 40,
    dc_last = 41
  );

  PISteamHTMLSurface__EHTMLKeyModifiers = ^ISteamHTMLSurface__EHTMLKeyModifiers;
  ISteamHTMLSurface__EHTMLKeyModifiers = (
    k_eHTMLKeyModifier_None = 0,
    k_eHTMLKeyModifier_AltDown = 1,
    k_eHTMLKeyModifier_CtrlDown = 2,
    k_eHTMLKeyModifier_ShiftDown = 4
  );

  PAppId_t = ^AppId_t;
  AppId_t = UInt32;
  PDepotId_t = ^DepotId_t;
  DepotId_t = UInt32;
  PRTime32 = ^RTime32;
  RTime32 = UInt32;
  PSteamAPICall_t = ^SteamAPICall_t;
  SteamAPICall_t = UInt64;
  PAccountID_t = ^AccountID_t;
  AccountID_t = UInt32;
  PPartyBeaconID_t = ^PartyBeaconID_t;
  PartyBeaconID_t = UInt64;
  PHAuthTicket = ^HAuthTicket;
  HAuthTicket = UInt32;
  PPFNPreMinidumpCallback = ^PFNPreMinidumpCallback;
  PFNPreMinidumpCallback = procedure(a0: Pointer); cdecl;
  PHSteamPipe = ^HSteamPipe;
  HSteamPipe = Integer;
  PHSteamUser = ^HSteamUser;
  HSteamUser = Integer;
  PFriendsGroupID_t = ^FriendsGroupID_t;
  FriendsGroupID_t = Int16;
  PHServerListRequest = ^HServerListRequest;
  HServerListRequest = Pointer;
  PHServerQuery = ^HServerQuery;
  HServerQuery = Integer;
  PUGCHandle_t = ^UGCHandle_t;
  UGCHandle_t = UInt64;
  PPublishedFileUpdateHandle_t = ^PublishedFileUpdateHandle_t;
  PublishedFileUpdateHandle_t = UInt64;
  PPublishedFileId_t = ^PublishedFileId_t;
  PublishedFileId_t = UInt64;
  PUGCFileWriteStreamHandle_t = ^UGCFileWriteStreamHandle_t;
  UGCFileWriteStreamHandle_t = UInt64;
  PSteamLeaderboard_t = ^SteamLeaderboard_t;
  SteamLeaderboard_t = UInt64;
  PSteamLeaderboardEntries_t = ^SteamLeaderboardEntries_t;
  SteamLeaderboardEntries_t = UInt64;
  PSNetSocket_t = ^SNetSocket_t;
  SNetSocket_t = UInt32;
  PSNetListenSocket_t = ^SNetListenSocket_t;
  SNetListenSocket_t = UInt32;
  PScreenshotHandle = ^ScreenshotHandle;
  ScreenshotHandle = UInt32;
  PHTTPRequestHandle = ^HTTPRequestHandle;
  HTTPRequestHandle = UInt32;
  PHTTPCookieContainerHandle = ^HTTPCookieContainerHandle;
  HTTPCookieContainerHandle = UInt32;
  PInputHandle_t = ^InputHandle_t;
  InputHandle_t = UInt64;
  PInputActionSetHandle_t = ^InputActionSetHandle_t;
  InputActionSetHandle_t = UInt64;
  PInputDigitalActionHandle_t = ^InputDigitalActionHandle_t;
  InputDigitalActionHandle_t = UInt64;
  PInputAnalogActionHandle_t = ^InputAnalogActionHandle_t;
  InputAnalogActionHandle_t = UInt64;
  PSteamInputActionEventCallbackPointer = ^SteamInputActionEventCallbackPointer;
  SteamInputActionEventCallbackPointer = procedure(a0: PSteamInputActionEvent_t); cdecl;
  PControllerHandle_t = ^ControllerHandle_t;
  ControllerHandle_t = UInt64;
  PControllerActionSetHandle_t = ^ControllerActionSetHandle_t;
  ControllerActionSetHandle_t = UInt64;
  PControllerDigitalActionHandle_t = ^ControllerDigitalActionHandle_t;
  ControllerDigitalActionHandle_t = UInt64;
  PControllerAnalogActionHandle_t = ^ControllerAnalogActionHandle_t;
  ControllerAnalogActionHandle_t = UInt64;
  PUGCQueryHandle_t = ^UGCQueryHandle_t;
  UGCQueryHandle_t = UInt64;
  PUGCUpdateHandle_t = ^UGCUpdateHandle_t;
  UGCUpdateHandle_t = UInt64;
  PHHTMLBrowser = ^HHTMLBrowser;
  HHTMLBrowser = UInt32;
  PSteamItemInstanceID_t = ^SteamItemInstanceID_t;
  SteamItemInstanceID_t = UInt64;
  PSteamItemDef_t = ^SteamItemDef_t;
  SteamItemDef_t = Integer;
  PSteamInventoryResult_t = ^SteamInventoryResult_t;
  SteamInventoryResult_t = Integer;
  PSteamInventoryUpdateHandle_t = ^SteamInventoryUpdateHandle_t;
  SteamInventoryUpdateHandle_t = UInt64;
  PRemotePlaySessionID_t = ^RemotePlaySessionID_t;
  RemotePlaySessionID_t = UInt32;
  PFnSteamNetConnectionStatusChanged = ^FnSteamNetConnectionStatusChanged;
  FnSteamNetConnectionStatusChanged = procedure(a0: PSteamNetConnectionStatusChangedCallback_t); cdecl;
  PFnSteamNetAuthenticationStatusChanged = ^FnSteamNetAuthenticationStatusChanged;
  FnSteamNetAuthenticationStatusChanged = procedure(a0: PSteamNetAuthenticationStatus_t); cdecl;
  PFnSteamRelayNetworkStatusChanged = ^FnSteamRelayNetworkStatusChanged;
  FnSteamRelayNetworkStatusChanged = procedure(a0: PSteamRelayNetworkStatus_t); cdecl;
  PFnSteamNetworkingMessagesSessionRequest = ^FnSteamNetworkingMessagesSessionRequest;
  FnSteamNetworkingMessagesSessionRequest = procedure(a0: PSteamNetworkingMessagesSessionRequest_t); cdecl;
  PFnSteamNetworkingMessagesSessionFailed = ^FnSteamNetworkingMessagesSessionFailed;
  FnSteamNetworkingMessagesSessionFailed = procedure(a0: PSteamNetworkingMessagesSessionFailed_t); cdecl;
  PFnSteamNetworkingFakeIPResult = ^FnSteamNetworkingFakeIPResult;
  FnSteamNetworkingFakeIPResult = procedure(a0: PSteamNetworkingFakeIPResult_t); cdecl;
  PHSteamNetConnection = ^HSteamNetConnection;
  HSteamNetConnection = UInt32;
  PHSteamListenSocket = ^HSteamListenSocket;
  HSteamListenSocket = UInt32;
  PHSteamNetPollGroup = ^HSteamNetPollGroup;
  HSteamNetPollGroup = UInt32;
  PSteamNetworkingErrMsg = ^SteamNetworkingErrMsg;
  SteamNetworkingErrMsg = Array[0..1024 - 1] of AnsiChar;
  PSteamNetworkingPOPID = ^SteamNetworkingPOPID;
  SteamNetworkingPOPID = UInt32;
  PSteamNetworkingMicroseconds = ^SteamNetworkingMicroseconds;
  SteamNetworkingMicroseconds = Int64;
  PFSteamNetworkingSocketsDebugOutput = ^FSteamNetworkingSocketsDebugOutput;
  FSteamNetworkingSocketsDebugOutput = procedure(a0: ESteamNetworkingSocketsDebugOutputType; a1: PAnsiChar); cdecl;
  SteamAPIWarningMessageHook_t = procedure(a0: Integer; a1: PAnsiChar); cdecl;

  {$PACKRECORDS 1}
  PCSteamID = ^CSteamID;
  CSteamID = bitpacked record
    // n = num_bits, 0..(2^n - 1)
    m_unAccountID: 0..4294967295; // 32 bits
    m_unAccountInstance: 0..1048575; // 20 bits
    m_EAccountType: 0..15; // 4 bits
    m_EUniverse: 0..255; // 8 bits
  end;

  PCGameID = ^CGameID;
  CGameID = bitpacked record
    // n = num_bits, 0..(2^n - 1)
    m_nAppID: 0..16777215; // 24 bits
    m_nType: 0..255; // 8 bits
    m_nModID: 0..4294967295; // 32 bits
  end;

  {$PACKRECORDS C}
  SteamNetworkingConfigValue_t = record
    m_eValue: ESteamNetworkingConfigValue;
    m_eDataType: ESteamNetworkingConfigDataType;
    case Integer of
      0: (m_int32: int32);
      1: (m_int64: int64);
      2: (m_float: Single);
      3: (m_string: PAnsiChar);
      4: (m_functionPtr: procedure);
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  {$PACKRECORDS 1}
  SteamIPAddress_t = record
    m_rgubIPv6: Array[0..16 - 1] of UInt8;
    m_eType: ESteamIPType;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  FriendGameInfo_t = record
    m_gameID: CGameID;
    m_unGameIP: UInt32;
    m_usGamePort: UInt16;
    m_usQueryPort: UInt16;
    m_steamIDLobby: CSteamID;
  end;

  {$PACKRECORDS C}
  MatchMakingKeyValuePair_t = record
    m_szKey: Array[0..256 - 1] of AnsiChar;
    m_szValue: Array[0..256 - 1] of AnsiChar;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  servernetadr_t = record
    m_usConnectionPort: UInt16;
    m_usQueryPort: UInt16;
    m_unIP: UInt32;
  end;

  gameserveritem_t = record
    m_NetAdr: servernetadr_t;
    m_nPing: Integer;
    m_bHadSuccessfulResponse: Boolean;
    m_bDoNotRefresh: Boolean;
    m_szGameDir: Array[0..32 - 1] of AnsiChar;
    m_szMap: Array[0..32 - 1] of AnsiChar;
    m_szGameDescription: Array[0..64 - 1] of AnsiChar;
    m_nAppID: UInt32;
    m_nPlayers: Integer;
    m_nMaxPlayers: Integer;
    m_nBotPlayers: Integer;
    m_bPassword: Boolean;
    m_bSecure: Boolean;
    m_ulTimeLastPlayed: UInt32;
    m_nServerVersion: Integer;
    m_szServerName: Array[0..64 - 1] of AnsiChar;
    m_szGameTags: Array[0..128 - 1] of AnsiChar;
    m_steamID: CSteamID;
  end;

  SteamPartyBeaconLocation_t = record
    m_eType: ESteamPartyBeaconLocationType;
    m_ulLocationID: UInt64;
  end;

  SteamParamStringArray_t = record
    m_ppStrings: PPAnsiChar;
    m_nNumStrings: Int32;
  end;

  LeaderboardEntry_t = record
    m_steamIDUser: CSteamID;
    m_nGlobalRank: Int32;
    m_nScore: Int32;
    m_cDetails: Int32;
    m_hUGC: UGCHandle_t;
  end;

  P2PSessionState_t = record
    m_bConnectionActive: UInt8;
    m_bConnecting: UInt8;
    m_eP2PSessionError: UInt8;
    m_bUsingRelay: UInt8;
    m_nBytesQueuedForSend: Int32;
    m_nPacketsQueuedForSend: Int32;
    m_nRemoteIP: UInt32;
    m_nRemotePort: UInt16;
  end;

  {$PACKRECORDS 1}
  InputAnalogActionData_t = record
    eMode: EInputSourceMode;
    x: Single;
    y: Single;
    bActive: Boolean;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  {$PACKRECORDS 1}
  InputDigitalActionData_t = record
    bState: Boolean;
    bActive: Boolean;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

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

  SteamUGCDetails_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_eResult: EResult;
    m_eFileType: EWorkshopFileType;
    m_nCreatorAppID: AppId_t;
    m_nConsumerAppID: AppId_t;
    m_rgchTitle: Array[0..129 - 1] of AnsiChar;
    m_rgchDescription: Array[0..8000 - 1] of AnsiChar;
    m_ulSteamIDOwner: UInt64;
    m_rtimeCreated: UInt32;
    m_rtimeUpdated: UInt32;
    m_rtimeAddedToUserList: UInt32;
    m_eVisibility: ERemoteStoragePublishedFileVisibility;
    m_bBanned: Boolean;
    m_bAcceptedForUse: Boolean;
    m_bTagsTruncated: Boolean;
    m_rgchTags: Array[0..1025 - 1] of AnsiChar;
    m_hFile: UGCHandle_t;
    m_hPreviewFile: UGCHandle_t;
    m_pchFileName: Array[0..260 - 1] of AnsiChar;
    m_nFileSize: Int32;
    m_nPreviewFileSize: Int32;
    m_rgchURL: Array[0..256 - 1] of AnsiChar;
    m_unVotesUp: UInt32;
    m_unVotesDown: UInt32;
    m_flScore: Single;
    m_unNumChildren: UInt32;
  end;

  SteamItemDetails_t = record
    m_itemId: SteamItemInstanceID_t;
    m_iDefinition: SteamItemDef_t;
    m_unQuantity: UInt16;
    m_unFlags: UInt16;
  end;

  {$PACKRECORDS 1}
  SteamNetworkingIPAddr = record
    m_ipv6: Array[0..16 - 1] of UInt8;
    m_port: UInt16;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  {$PACKRECORDS 1}
  SteamNetworkingIdentity = record
    m_eType: ESteamNetworkingIdentityType;
    m_cbSize: Integer;
    m_szUnknownRawString: Array[0..128 - 1] of AnsiChar;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  SteamNetConnectionInfo_t = record
    m_identityRemote: SteamNetworkingIdentity;
    m_nUserData: Int64;
    m_hListenSocket: HSteamListenSocket;
    m_addrRemote: SteamNetworkingIPAddr;
    m__pad1: UInt16;
    m_idPOPRemote: SteamNetworkingPOPID;
    m_idPOPRelay: SteamNetworkingPOPID;
    m_eState: ESteamNetworkingConnectionState;
    m_eEndReason: Integer;
    m_szEndDebug: Array[0..128 - 1] of AnsiChar;
    m_szConnectionDescription: Array[0..128 - 1] of AnsiChar;
    m_nFlags: Integer;
    reserved: Array[0..63 - 1] of UInt32;
  end;

  SteamNetConnectionRealTimeStatus_t = record
    m_eState: ESteamNetworkingConnectionState;
    m_nPing: Integer;
    m_flConnectionQualityLocal: Single;
    m_flConnectionQualityRemote: Single;
    m_flOutPacketsPerSec: Single;
    m_flOutBytesPerSec: Single;
    m_flInPacketsPerSec: Single;
    m_flInBytesPerSec: Single;
    m_nSendRateBytesPerSecond: Integer;
    m_cbPendingUnreliable: Integer;
    m_cbPendingReliable: Integer;
    m_cbSentUnackedReliable: Integer;
    m_usecQueueTime: SteamNetworkingMicroseconds;
    reserved: Array[0..16 - 1] of UInt32;
  end;

  SteamNetConnectionRealTimeLaneStatus_t = record
    m_cbPendingUnreliable: Integer;
    m_cbPendingReliable: Integer;
    m_cbSentUnackedReliable: Integer;
    _reservePad1: Integer;
    m_usecQueueTime: SteamNetworkingMicroseconds;
    reserved: Array[0..10 - 1] of UInt32;
  end;

  {$PACKRECORDS C}
  SteamNetworkingMessage_t = record
    m_pData: Pointer;
    m_cbSize: Integer;
    m_conn: HSteamNetConnection;
    m_identityPeer: SteamNetworkingIdentity;
    m_nConnUserData: Int64;
    m_usecTimeReceived: SteamNetworkingMicroseconds;
    m_nMessageNumber: Int64;
    m_pfnFreeData: procedure(a0: PSteamNetworkingMessage_t); cdecl;
    m_pfnRelease: procedure(a0: PSteamNetworkingMessage_t); cdecl;
    m_nChannel: Integer;
    m_nFlags: Integer;
    m_nUserData: Int64;
    m_idxLane: UInt16;
    _pad1__: UInt16;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  {$PACKRECORDS C}
  SteamNetworkPingLocation_t = record
    m_data: Array[0..512 - 1] of UInt8;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  SteamDatagramHostedAddress = record
    m_cbSize: Integer;
    m_data: Array[0..128 - 1] of AnsiChar;
  end;

  SteamDatagramGameCoordinatorServerLogin = record
    m_identity: SteamNetworkingIdentity;
    m_routing: SteamDatagramHostedAddress;
    m_nAppID: AppId_t;
    m_rtime: RTime32;
    m_cbAppData: Integer;
    m_appData: Array[0..2048 - 1] of AnsiChar;
  end;

  SteamServersConnected_t = record
    Dummy: Byte;
  end;

  SteamServerConnectFailure_t = record
    m_eResult: EResult;
    m_bStillRetrying: Boolean;
  end;

  SteamServersDisconnected_t = record
    m_eResult: EResult;
  end;

  ClientGameServerDeny_t = record
    m_uAppID: UInt32;
    m_unGameServerIP: UInt32;
    m_usGameServerPort: UInt16;
    m_bSecure: UInt16;
    m_uReason: UInt32;
  end;

  IPCFailure_t = record
    m_eFailureType: UInt8;
  end;

  LicensesUpdated_t = record
    Dummy: Byte;
  end;

  ValidateAuthTicketResponse_t = record
    m_SteamID: CSteamID;
    m_eAuthSessionResponse: EAuthSessionResponse;
    m_OwnerSteamID: CSteamID;
  end;

  MicroTxnAuthorizationResponse_t = record
    m_unAppID: UInt32;
    m_ulOrderID: UInt64;
    m_bAuthorized: UInt8;
  end;

  EncryptedAppTicketResponse_t = record
    m_eResult: EResult;
  end;

  GetAuthSessionTicketResponse_t = record
    m_hAuthTicket: HAuthTicket;
    m_eResult: EResult;
  end;

  GameWebCallback_t = record
    m_szURL: Array[0..256 - 1] of AnsiChar;
  end;

  StoreAuthURLResponse_t = record
    m_szURL: Array[0..512 - 1] of AnsiChar;
  end;

  MarketEligibilityResponse_t = record
    m_bAllowed: Boolean;
    m_eNotAllowedReason: EMarketNotAllowedReasonFlags;
    m_rtAllowedAtTime: RTime32;
    m_cdaySteamGuardRequiredDays: Integer;
    m_cdayNewDeviceCooldown: Integer;
  end;

  DurationControl_t = record
    m_eResult: EResult;
    m_appid: AppId_t;
    m_bApplicable: Boolean;
    m_csecsLast5h: Int32;
    m_progress: EDurationControlProgress;
    m_notification: EDurationControlNotification;
    m_csecsToday: Int32;
    m_csecsRemaining: Int32;
  end;

  PersonaStateChange_t = record
    m_ulSteamID: UInt64;
    m_nChangeFlags: Integer;
  end;

  GameOverlayActivated_t = record
    m_bActive: UInt8;
  end;

  GameServerChangeRequested_t = record
    m_rgchServer: Array[0..64 - 1] of AnsiChar;
    m_rgchPassword: Array[0..64 - 1] of AnsiChar;
  end;

  GameLobbyJoinRequested_t = record
    m_steamIDLobby: CSteamID;
    m_steamIDFriend: CSteamID;
  end;

  AvatarImageLoaded_t = record
    m_steamID: CSteamID;
    m_iImage: Integer;
    m_iWide: Integer;
    m_iTall: Integer;
  end;

  ClanOfficerListResponse_t = record
    m_steamIDClan: CSteamID;
    m_cOfficers: Integer;
    m_bSuccess: UInt8;
  end;

  FriendRichPresenceUpdate_t = record
    m_steamIDFriend: CSteamID;
    m_nAppID: AppId_t;
  end;

  GameRichPresenceJoinRequested_t = record
    m_steamIDFriend: CSteamID;
    m_rgchConnect: Array[0..256 - 1] of AnsiChar;
  end;

  GameConnectedClanChatMsg_t = record
    m_steamIDClanChat: CSteamID;
    m_steamIDUser: CSteamID;
    m_iMessageID: Integer;
  end;

  GameConnectedChatJoin_t = record
    m_steamIDClanChat: CSteamID;
    m_steamIDUser: CSteamID;
  end;

  {$PACKRECORDS 1}
  GameConnectedChatLeave_t = record
    m_steamIDClanChat: CSteamID;
    m_steamIDUser: CSteamID;
    m_bKicked: Boolean;
    m_bDropped: Boolean;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  DownloadClanActivityCountsResult_t = record
    m_bSuccess: Boolean;
  end;

  JoinClanChatRoomCompletionResult_t = record
    m_steamIDClanChat: CSteamID;
    m_eChatRoomEnterResponse: EChatRoomEnterResponse;
  end;

  GameConnectedFriendChatMsg_t = record
    m_steamIDUser: CSteamID;
    m_iMessageID: Integer;
  end;

  FriendsGetFollowerCount_t = record
    m_eResult: EResult;
    m_steamID: CSteamID;
    m_nCount: Integer;
  end;

  FriendsIsFollowing_t = record
    m_eResult: EResult;
    m_steamID: CSteamID;
    m_bIsFollowing: Boolean;
  end;

  FriendsEnumerateFollowingList_t = record
    m_eResult: EResult;
    m_rgSteamID: Array[0..50 - 1] of CSteamID;
    m_nResultsReturned: Int32;
    m_nTotalResultCount: Int32;
  end;

  SetPersonaNameResponse_t = record
    m_bSuccess: Boolean;
    m_bLocalSuccess: Boolean;
    m_result: EResult;
  end;

  UnreadChatMessagesChanged_t = record
    Dummy: Byte;
  end;

  OverlayBrowserProtocolNavigation_t = record
    rgchURI: Array[0..1024 - 1] of AnsiChar;
  end;

  IPCountry_t = record
    Dummy: Byte;
  end;

  LowBatteryPower_t = record
    m_nMinutesBatteryLeft: UInt8;
  end;

  SteamAPICallCompleted_t = record
    m_hAsyncCall: SteamAPICall_t;
    m_iCallback: Integer;
    m_cubParam: UInt32;
  end;

  SteamShutdown_t = record
    Dummy: Byte;
  end;

  CheckFileSignature_t = record
    m_eCheckFileSignature: ECheckFileSignature;
  end;

  GamepadTextInputDismissed_t = record
    m_bSubmitted: Boolean;
    m_unSubmittedText: UInt32;
  end;

  AppResumingFromSuspend_t = record
    Dummy: Byte;
  end;

  FloatingGamepadTextInputDismissed_t = record
    Dummy: Byte;
  end;

  FavoritesListChanged_t = record
    m_nIP: UInt32;
    m_nQueryPort: UInt32;
    m_nConnPort: UInt32;
    m_nAppID: UInt32;
    m_nFlags: UInt32;
    m_bAdd: Boolean;
    m_unAccountId: AccountID_t;
  end;

  LobbyInvite_t = record
    m_ulSteamIDUser: UInt64;
    m_ulSteamIDLobby: UInt64;
    m_ulGameID: UInt64;
  end;

  LobbyEnter_t = record
    m_ulSteamIDLobby: UInt64;
    m_rgfChatPermissions: UInt32;
    m_bLocked: Boolean;
    m_EChatRoomEnterResponse: UInt32;
  end;

  LobbyDataUpdate_t = record
    m_ulSteamIDLobby: UInt64;
    m_ulSteamIDMember: UInt64;
    m_bSuccess: UInt8;
  end;

  LobbyChatUpdate_t = record
    m_ulSteamIDLobby: UInt64;
    m_ulSteamIDUserChanged: UInt64;
    m_ulSteamIDMakingChange: UInt64;
    m_rgfChatMemberStateChange: UInt32;
  end;

  LobbyChatMsg_t = record
    m_ulSteamIDLobby: UInt64;
    m_ulSteamIDUser: UInt64;
    m_eChatEntryType: UInt8;
    m_iChatID: UInt32;
  end;

  LobbyGameCreated_t = record
    m_ulSteamIDLobby: UInt64;
    m_ulSteamIDGameServer: UInt64;
    m_unIP: UInt32;
    m_usPort: UInt16;
  end;

  LobbyMatchList_t = record
    m_nLobbiesMatching: UInt32;
  end;

  LobbyKicked_t = record
    m_ulSteamIDLobby: UInt64;
    m_ulSteamIDAdmin: UInt64;
    m_bKickedDueToDisconnect: UInt8;
  end;

  LobbyCreated_t = record
    m_eResult: EResult;
    m_ulSteamIDLobby: UInt64;
  end;

  {$PACKRECORDS 1}
  PSNGameBootInviteResult_t = record
    m_bGameBootInviteExists: Boolean;
    m_steamIDLobby: CSteamID;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  FavoritesListAccountsUpdated_t = record
    m_eResult: EResult;
  end;

  SearchForGameProgressCallback_t = record
    m_ullSearchID: UInt64;
    m_eResult: EResult;
    m_lobbyID: CSteamID;
    m_steamIDEndedSearch: CSteamID;
    m_nSecondsRemainingEstimate: Int32;
    m_cPlayersSearching: Int32;
  end;

  SearchForGameResultCallback_t = record
    m_ullSearchID: UInt64;
    m_eResult: EResult;
    m_nCountPlayersInGame: Int32;
    m_nCountAcceptedGame: Int32;
    m_steamIDHost: CSteamID;
    m_bFinalCallback: Boolean;
  end;

  RequestPlayersForGameProgressCallback_t = record
    m_eResult: EResult;
    m_ullSearchID: UInt64;
  end;

  RequestPlayersForGameResultCallback_t = record
    m_eResult: EResult;
    m_ullSearchID: UInt64;
    m_SteamIDPlayerFound: CSteamID;
    m_SteamIDLobby: CSteamID;
    m_ePlayerAcceptState: RequestPlayersForGameResultCallback_t__PlayerAcceptState_t;
    m_nPlayerIndex: Int32;
    m_nTotalPlayersFound: Int32;
    m_nTotalPlayersAcceptedGame: Int32;
    m_nSuggestedTeamIndex: Int32;
    m_ullUniqueGameID: UInt64;
  end;

  RequestPlayersForGameFinalResultCallback_t = record
    m_eResult: EResult;
    m_ullSearchID: UInt64;
    m_ullUniqueGameID: UInt64;
  end;

  SubmitPlayerResultResultCallback_t = record
    m_eResult: EResult;
    ullUniqueGameID: UInt64;
    steamIDPlayer: CSteamID;
  end;

  EndGameResultCallback_t = record
    m_eResult: EResult;
    ullUniqueGameID: UInt64;
  end;

  JoinPartyCallback_t = record
    m_eResult: EResult;
    m_ulBeaconID: PartyBeaconID_t;
    m_SteamIDBeaconOwner: CSteamID;
    m_rgchConnectString: Array[0..256 - 1] of AnsiChar;
  end;

  CreateBeaconCallback_t = record
    m_eResult: EResult;
    m_ulBeaconID: PartyBeaconID_t;
  end;

  ReservationNotificationCallback_t = record
    m_ulBeaconID: PartyBeaconID_t;
    m_steamIDJoiner: CSteamID;
  end;

  ChangeNumOpenSlotsCallback_t = record
    m_eResult: EResult;
  end;

  AvailableBeaconLocationsUpdated_t = record
    Dummy: Byte;
  end;

  ActiveBeaconsUpdated_t = record
    Dummy: Byte;
  end;

  RemoteStorageFileShareResult_t = record
    m_eResult: EResult;
    m_hFile: UGCHandle_t;
    m_rgchFilename: Array[0..260 - 1] of AnsiChar;
  end;

  RemoteStoragePublishFileResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
  end;

  RemoteStorageDeletePublishedFileResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
  end;

  RemoteStorageEnumerateUserPublishedFilesResult_t = record
    m_eResult: EResult;
    m_nResultsReturned: Int32;
    m_nTotalResultCount: Int32;
    m_rgPublishedFileId: Array[0..50 - 1] of PublishedFileId_t;
  end;

  RemoteStorageSubscribePublishedFileResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
  end;

  RemoteStorageEnumerateUserSubscribedFilesResult_t = record
    m_eResult: EResult;
    m_nResultsReturned: Int32;
    m_nTotalResultCount: Int32;
    m_rgPublishedFileId: Array[0..50 - 1] of PublishedFileId_t;
    m_rgRTimeSubscribed: Array[0..50 - 1] of UInt32;
  end;

  RemoteStorageUnsubscribePublishedFileResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
  end;

  RemoteStorageUpdatePublishedFileResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
  end;

  RemoteStorageDownloadUGCResult_t = record
    m_eResult: EResult;
    m_hFile: UGCHandle_t;
    m_nAppID: AppId_t;
    m_nSizeInBytes: Int32;
    m_pchFileName: Array[0..260 - 1] of AnsiChar;
    m_ulSteamIDOwner: UInt64;
  end;

  RemoteStorageGetPublishedFileDetailsResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_nCreatorAppID: AppId_t;
    m_nConsumerAppID: AppId_t;
    m_rgchTitle: Array[0..129 - 1] of AnsiChar;
    m_rgchDescription: Array[0..8000 - 1] of AnsiChar;
    m_hFile: UGCHandle_t;
    m_hPreviewFile: UGCHandle_t;
    m_ulSteamIDOwner: UInt64;
    m_rtimeCreated: UInt32;
    m_rtimeUpdated: UInt32;
    m_eVisibility: ERemoteStoragePublishedFileVisibility;
    m_bBanned: Boolean;
    m_rgchTags: Array[0..1025 - 1] of AnsiChar;
    m_bTagsTruncated: Boolean;
    m_pchFileName: Array[0..260 - 1] of AnsiChar;
    m_nFileSize: Int32;
    m_nPreviewFileSize: Int32;
    m_rgchURL: Array[0..256 - 1] of AnsiChar;
    m_eFileType: EWorkshopFileType;
    m_bAcceptedForUse: Boolean;
  end;

  RemoteStorageEnumerateWorkshopFilesResult_t = record
    m_eResult: EResult;
    m_nResultsReturned: Int32;
    m_nTotalResultCount: Int32;
    m_rgPublishedFileId: Array[0..50 - 1] of PublishedFileId_t;
    m_rgScore: Array[0..50 - 1] of Single;
    m_nAppId: AppId_t;
    m_unStartIndex: UInt32;
  end;

  RemoteStorageGetPublishedItemVoteDetailsResult_t = record
    m_eResult: EResult;
    m_unPublishedFileId: PublishedFileId_t;
    m_nVotesFor: Int32;
    m_nVotesAgainst: Int32;
    m_nReports: Int32;
    m_fScore: Single;
  end;

  RemoteStoragePublishedFileSubscribed_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_nAppID: AppId_t;
  end;

  RemoteStoragePublishedFileUnsubscribed_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_nAppID: AppId_t;
  end;

  RemoteStoragePublishedFileDeleted_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_nAppID: AppId_t;
  end;

  RemoteStorageUpdateUserPublishedItemVoteResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
  end;

  RemoteStorageUserVoteDetails_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_eVote: EWorkshopVote;
  end;

  RemoteStorageEnumerateUserSharedWorkshopFilesResult_t = record
    m_eResult: EResult;
    m_nResultsReturned: Int32;
    m_nTotalResultCount: Int32;
    m_rgPublishedFileId: Array[0..50 - 1] of PublishedFileId_t;
  end;

  RemoteStorageSetUserPublishedFileActionResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_eAction: EWorkshopFileAction;
  end;

  RemoteStorageEnumeratePublishedFilesByUserActionResult_t = record
    m_eResult: EResult;
    m_eAction: EWorkshopFileAction;
    m_nResultsReturned: Int32;
    m_nTotalResultCount: Int32;
    m_rgPublishedFileId: Array[0..50 - 1] of PublishedFileId_t;
    m_rgRTimeUpdated: Array[0..50 - 1] of UInt32;
  end;

  RemoteStoragePublishFileProgress_t = record
    m_dPercentFile: Double;
    m_bPreview: Boolean;
  end;

  RemoteStoragePublishedFileUpdated_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_nAppID: AppId_t;
    m_ulUnused: UInt64;
  end;

  RemoteStorageFileWriteAsyncComplete_t = record
    m_eResult: EResult;
  end;

  RemoteStorageFileReadAsyncComplete_t = record
    m_hFileReadAsync: SteamAPICall_t;
    m_eResult: EResult;
    m_nOffset: UInt32;
    m_cubRead: UInt32;
  end;

  RemoteStorageLocalFileChange_t = record
    Dummy: Byte;
  end;

  UserStatsReceived_t = record
    m_nGameID: UInt64;
    m_eResult: EResult;
    m_steamIDUser: CSteamID;
  end;

  UserStatsStored_t = record
    m_nGameID: UInt64;
    m_eResult: EResult;
  end;

  UserAchievementStored_t = record
    m_nGameID: UInt64;
    m_bGroupAchievement: Boolean;
    m_rgchAchievementName: Array[0..128 - 1] of AnsiChar;
    m_nCurProgress: UInt32;
    m_nMaxProgress: UInt32;
  end;

  LeaderboardFindResult_t = record
    m_hSteamLeaderboard: SteamLeaderboard_t;
    m_bLeaderboardFound: UInt8;
  end;

  LeaderboardScoresDownloaded_t = record
    m_hSteamLeaderboard: SteamLeaderboard_t;
    m_hSteamLeaderboardEntries: SteamLeaderboardEntries_t;
    m_cEntryCount: Integer;
  end;

  LeaderboardScoreUploaded_t = record
    m_bSuccess: UInt8;
    m_hSteamLeaderboard: SteamLeaderboard_t;
    m_nScore: Int32;
    m_bScoreChanged: UInt8;
    m_nGlobalRankNew: Integer;
    m_nGlobalRankPrevious: Integer;
  end;

  NumberOfCurrentPlayers_t = record
    m_bSuccess: UInt8;
    m_cPlayers: Int32;
  end;

  UserStatsUnloaded_t = record
    m_steamIDUser: CSteamID;
  end;

  UserAchievementIconFetched_t = record
    m_nGameID: CGameID;
    m_rgchAchievementName: Array[0..128 - 1] of AnsiChar;
    m_bAchieved: Boolean;
    m_nIconHandle: Integer;
  end;

  GlobalAchievementPercentagesReady_t = record
    m_nGameID: UInt64;
    m_eResult: EResult;
  end;

  LeaderboardUGCSet_t = record
    m_eResult: EResult;
    m_hSteamLeaderboard: SteamLeaderboard_t;
  end;

  PS3TrophiesInstalled_t = record
    m_nGameID: UInt64;
    m_eResult: EResult;
    m_ulRequiredDiskSpace: UInt64;
  end;

  GlobalStatsReceived_t = record
    m_nGameID: UInt64;
    m_eResult: EResult;
  end;

  DlcInstalled_t = record
    m_nAppID: AppId_t;
  end;

  RegisterActivationCodeResponse_t = record
    m_eResult: ERegisterActivationCodeResult;
    m_unPackageRegistered: UInt32;
  end;

  NewUrlLaunchParameters_t = record
    Dummy: Byte;
  end;

  AppProofOfPurchaseKeyResponse_t = record
    m_eResult: EResult;
    m_nAppID: UInt32;
    m_cchKeyLength: UInt32;
    m_rgchKey: Array[0..240 - 1] of AnsiChar;
  end;

  FileDetailsResult_t = record
    m_eResult: EResult;
    m_ulFileSize: UInt64;
    m_FileSHA: Array[0..20 - 1] of UInt8;
    m_unFlags: UInt32;
  end;

  TimedTrialStatus_t = record
    m_unAppID: AppId_t;
    m_bIsOffline: Boolean;
    m_unSecondsAllowed: UInt32;
    m_unSecondsPlayed: UInt32;
  end;

  P2PSessionRequest_t = record
    m_steamIDRemote: CSteamID;
  end;

  {$PACKRECORDS 1}
  P2PSessionConnectFail_t = record
    m_steamIDRemote: CSteamID;
    m_eP2PSessionError: UInt8;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  SocketStatusCallback_t = record
    m_hSocket: SNetSocket_t;
    m_hListenSocket: SNetListenSocket_t;
    m_steamIDRemote: CSteamID;
    m_eSNetSocketState: Integer;
  end;

  ScreenshotReady_t = record
    m_hLocal: ScreenshotHandle;
    m_eResult: EResult;
  end;

  ScreenshotRequested_t = record
    Dummy: Byte;
  end;

  PlaybackStatusHasChanged_t = record
    Dummy: Byte;
  end;

  VolumeHasChanged_t = record
    m_flNewVolume: Single;
  end;

  MusicPlayerRemoteWillActivate_t = record
    Dummy: Byte;
  end;

  MusicPlayerRemoteWillDeactivate_t = record
    Dummy: Byte;
  end;

  MusicPlayerRemoteToFront_t = record
    Dummy: Byte;
  end;

  MusicPlayerWillQuit_t = record
    Dummy: Byte;
  end;

  MusicPlayerWantsPlay_t = record
    Dummy: Byte;
  end;

  MusicPlayerWantsPause_t = record
    Dummy: Byte;
  end;

  MusicPlayerWantsPlayPrevious_t = record
    Dummy: Byte;
  end;

  MusicPlayerWantsPlayNext_t = record
    Dummy: Byte;
  end;

  MusicPlayerWantsShuffled_t = record
    m_bShuffled: Boolean;
  end;

  MusicPlayerWantsLooped_t = record
    m_bLooped: Boolean;
  end;

  MusicPlayerWantsVolume_t = record
    m_flNewVolume: Single;
  end;

  MusicPlayerSelectsQueueEntry_t = record
    nID: Integer;
  end;

  MusicPlayerSelectsPlaylistEntry_t = record
    nID: Integer;
  end;

  MusicPlayerWantsPlayingRepeatStatus_t = record
    m_nPlayingRepeatStatus: Integer;
  end;

  HTTPRequestCompleted_t = record
    m_hRequest: HTTPRequestHandle;
    m_ulContextValue: UInt64;
    m_bRequestSuccessful: Boolean;
    m_eStatusCode: EHTTPStatusCode;
    m_unBodySize: UInt32;
  end;

  HTTPRequestHeadersReceived_t = record
    m_hRequest: HTTPRequestHandle;
    m_ulContextValue: UInt64;
  end;

  HTTPRequestDataReceived_t = record
    m_hRequest: HTTPRequestHandle;
    m_ulContextValue: UInt64;
    m_cOffset: UInt32;
    m_cBytesReceived: UInt32;
  end;

  SteamInputDeviceConnected_t = record
    m_ulConnectedDeviceHandle: InputHandle_t;
  end;

  SteamInputDeviceDisconnected_t = record
    m_ulDisconnectedDeviceHandle: InputHandle_t;
  end;

  SteamInputConfigurationLoaded_t = record
    m_unAppID: AppId_t;
    m_ulDeviceHandle: InputHandle_t;
    m_ulMappingCreator: CSteamID;
    m_unMajorRevision: UInt32;
    m_unMinorRevision: UInt32;
    m_bUsesSteamInputAPI: Boolean;
    m_bUsesGamepadAPI: Boolean;
  end;

  SteamUGCQueryCompleted_t = record
    m_handle: UGCQueryHandle_t;
    m_eResult: EResult;
    m_unNumResultsReturned: UInt32;
    m_unTotalMatchingResults: UInt32;
    m_bCachedData: Boolean;
    m_rgchNextCursor: Array[0..256 - 1] of AnsiChar;
  end;

  SteamUGCRequestUGCDetailsResult_t = record
    m_details: SteamUGCDetails_t;
    m_bCachedData: Boolean;
  end;

  CreateItemResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
  end;

  SubmitItemUpdateResult_t = record
    m_eResult: EResult;
    m_bUserNeedsToAcceptWorkshopLegalAgreement: Boolean;
    m_nPublishedFileId: PublishedFileId_t;
  end;

  ItemInstalled_t = record
    m_unAppID: AppId_t;
    m_nPublishedFileId: PublishedFileId_t;
  end;

  DownloadItemResult_t = record
    m_unAppID: AppId_t;
    m_nPublishedFileId: PublishedFileId_t;
    m_eResult: EResult;
  end;

  UserFavoriteItemsListChanged_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_eResult: EResult;
    m_bWasAddRequest: Boolean;
  end;

  SetUserItemVoteResult_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_eResult: EResult;
    m_bVoteUp: Boolean;
  end;

  GetUserItemVoteResult_t = record
    m_nPublishedFileId: PublishedFileId_t;
    m_eResult: EResult;
    m_bVotedUp: Boolean;
    m_bVotedDown: Boolean;
    m_bVoteSkipped: Boolean;
  end;

  StartPlaytimeTrackingResult_t = record
    m_eResult: EResult;
  end;

  StopPlaytimeTrackingResult_t = record
    m_eResult: EResult;
  end;

  AddUGCDependencyResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_nChildPublishedFileId: PublishedFileId_t;
  end;

  RemoveUGCDependencyResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_nChildPublishedFileId: PublishedFileId_t;
  end;

  AddAppDependencyResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_nAppID: AppId_t;
  end;

  RemoveAppDependencyResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_nAppID: AppId_t;
  end;

  GetAppDependenciesResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
    m_rgAppIDs: Array[0..32 - 1] of AppId_t;
    m_nNumAppDependencies: UInt32;
    m_nTotalNumAppDependencies: UInt32;
  end;

  DeleteItemResult_t = record
    m_eResult: EResult;
    m_nPublishedFileId: PublishedFileId_t;
  end;

  UserSubscribedItemsListChanged_t = record
    m_nAppID: AppId_t;
  end;

  WorkshopEULAStatus_t = record
    m_eResult: EResult;
    m_nAppID: AppId_t;
    m_unVersion: UInt32;
    m_rtAction: RTime32;
    m_bAccepted: Boolean;
    m_bNeedsAction: Boolean;
  end;

  SteamAppInstalled_t = record
    m_nAppID: AppId_t;
    m_iInstallFolderIndex: Integer;
  end;

  SteamAppUninstalled_t = record
    m_nAppID: AppId_t;
    m_iInstallFolderIndex: Integer;
  end;

  HTML_BrowserReady_t = record
    unBrowserHandle: HHTMLBrowser;
  end;

  HTML_NeedsPaint_t = record
    unBrowserHandle: HHTMLBrowser;
    pBGRA: PAnsiChar;
    unWide: UInt32;
    unTall: UInt32;
    unUpdateX: UInt32;
    unUpdateY: UInt32;
    unUpdateWide: UInt32;
    unUpdateTall: UInt32;
    unScrollX: UInt32;
    unScrollY: UInt32;
    flPageScale: Single;
    unPageSerial: UInt32;
  end;

  HTML_StartRequest_t = record
    unBrowserHandle: HHTMLBrowser;
    pchURL: PAnsiChar;
    pchTarget: PAnsiChar;
    pchPostData: PAnsiChar;
    bIsRedirect: Boolean;
  end;

  HTML_CloseBrowser_t = record
    unBrowserHandle: HHTMLBrowser;
  end;

  HTML_URLChanged_t = record
    unBrowserHandle: HHTMLBrowser;
    pchURL: PAnsiChar;
    pchPostData: PAnsiChar;
    bIsRedirect: Boolean;
    pchPageTitle: PAnsiChar;
    bNewNavigation: Boolean;
  end;

  HTML_FinishedRequest_t = record
    unBrowserHandle: HHTMLBrowser;
    pchURL: PAnsiChar;
    pchPageTitle: PAnsiChar;
  end;

  HTML_OpenLinkInNewTab_t = record
    unBrowserHandle: HHTMLBrowser;
    pchURL: PAnsiChar;
  end;

  HTML_ChangedTitle_t = record
    unBrowserHandle: HHTMLBrowser;
    pchTitle: PAnsiChar;
  end;

  HTML_SearchResults_t = record
    unBrowserHandle: HHTMLBrowser;
    unResults: UInt32;
    unCurrentMatch: UInt32;
  end;

  HTML_CanGoBackAndForward_t = record
    unBrowserHandle: HHTMLBrowser;
    bCanGoBack: Boolean;
    bCanGoForward: Boolean;
  end;

  HTML_HorizontalScroll_t = record
    unBrowserHandle: HHTMLBrowser;
    unScrollMax: UInt32;
    unScrollCurrent: UInt32;
    flPageScale: Single;
    bVisible: Boolean;
    unPageSize: UInt32;
  end;

  HTML_VerticalScroll_t = record
    unBrowserHandle: HHTMLBrowser;
    unScrollMax: UInt32;
    unScrollCurrent: UInt32;
    flPageScale: Single;
    bVisible: Boolean;
    unPageSize: UInt32;
  end;

  HTML_LinkAtPosition_t = record
    unBrowserHandle: HHTMLBrowser;
    x: UInt32;
    y: UInt32;
    pchURL: PAnsiChar;
    bInput: Boolean;
    bLiveLink: Boolean;
  end;

  HTML_JSAlert_t = record
    unBrowserHandle: HHTMLBrowser;
    pchMessage: PAnsiChar;
  end;

  HTML_JSConfirm_t = record
    unBrowserHandle: HHTMLBrowser;
    pchMessage: PAnsiChar;
  end;

  HTML_FileOpenDialog_t = record
    unBrowserHandle: HHTMLBrowser;
    pchTitle: PAnsiChar;
    pchInitialFile: PAnsiChar;
  end;

  HTML_NewWindow_t = record
    unBrowserHandle: HHTMLBrowser;
    pchURL: PAnsiChar;
    unX: UInt32;
    unY: UInt32;
    unWide: UInt32;
    unTall: UInt32;
    unNewWindow_BrowserHandle_IGNORE: HHTMLBrowser;
  end;

  HTML_SetCursor_t = record
    unBrowserHandle: HHTMLBrowser;
    eMouseCursor: UInt32;
  end;

  HTML_StatusText_t = record
    unBrowserHandle: HHTMLBrowser;
    pchMsg: PAnsiChar;
  end;

  HTML_ShowToolTip_t = record
    unBrowserHandle: HHTMLBrowser;
    pchMsg: PAnsiChar;
  end;

  HTML_UpdateToolTip_t = record
    unBrowserHandle: HHTMLBrowser;
    pchMsg: PAnsiChar;
  end;

  HTML_HideToolTip_t = record
    unBrowserHandle: HHTMLBrowser;
  end;

  HTML_BrowserRestarted_t = record
    unBrowserHandle: HHTMLBrowser;
    unOldBrowserHandle: HHTMLBrowser;
  end;

  SteamInventoryResultReady_t = record
    m_handle: SteamInventoryResult_t;
    m_result: EResult;
  end;

  SteamInventoryFullUpdate_t = record
    m_handle: SteamInventoryResult_t;
  end;

  SteamInventoryDefinitionUpdate_t = record
    Dummy: Byte;
  end;

  SteamInventoryEligiblePromoItemDefIDs_t = record
    m_result: EResult;
    m_steamID: CSteamID;
    m_numEligiblePromoItemDefs: Integer;
    m_bCachedData: Boolean;
  end;

  SteamInventoryStartPurchaseResult_t = record
    m_result: EResult;
    m_ulOrderID: UInt64;
    m_ulTransID: UInt64;
  end;

  SteamInventoryRequestPricesResult_t = record
    m_result: EResult;
    m_rgchCurrency: Array[0..4 - 1] of AnsiChar;
  end;

  GetVideoURLResult_t = record
    m_eResult: EResult;
    m_unVideoAppID: AppId_t;
    m_rgchURL: Array[0..256 - 1] of AnsiChar;
  end;

  GetOPFSettingsResult_t = record
    m_eResult: EResult;
    m_unVideoAppID: AppId_t;
  end;

  {$PACKRECORDS C}
  SteamParentalSettingsChanged_t = record
    Dummy: Byte;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  SteamRemotePlaySessionConnected_t = record
    m_unSessionID: RemotePlaySessionID_t;
  end;

  SteamRemotePlaySessionDisconnected_t = record
    m_unSessionID: RemotePlaySessionID_t;
  end;

  {$PACKRECORDS 1}
  SteamNetworkingMessagesSessionRequest_t = record
    m_identityRemote: SteamNetworkingIdentity;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  {$PACKRECORDS 1}
  SteamNetworkingMessagesSessionFailed_t = record
    m_info: SteamNetConnectionInfo_t;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  SteamNetConnectionStatusChangedCallback_t = record
    m_hConn: HSteamNetConnection;
    m_info: SteamNetConnectionInfo_t;
    m_eOldState: ESteamNetworkingConnectionState;
  end;

  SteamNetAuthenticationStatus_t = record
    m_eAvail: ESteamNetworkingAvailability;
    m_debugMsg: Array[0..256 - 1] of AnsiChar;
  end;

  {$PACKRECORDS C}
  SteamRelayNetworkStatus_t = record
    m_eAvail: ESteamNetworkingAvailability;
    m_bPingMeasurementInProgress: Integer;
    m_eAvailNetworkConfig: ESteamNetworkingAvailability;
    m_eAvailAnyRelay: ESteamNetworkingAvailability;
    m_debugMsg: Array[0..256 - 1] of AnsiChar;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  GSClientApprove_t = record
    m_SteamID: CSteamID;
    m_OwnerSteamID: CSteamID;
  end;

  GSClientDeny_t = record
    m_SteamID: CSteamID;
    m_eDenyReason: EDenyReason;
    m_rgchOptionalText: Array[0..128 - 1] of AnsiChar;
  end;

  GSClientKick_t = record
    m_SteamID: CSteamID;
    m_eDenyReason: EDenyReason;
  end;

  GSClientAchievementStatus_t = record
    m_SteamID: UInt64;
    m_pchAchievement: Array[0..128 - 1] of AnsiChar;
    m_bUnlocked: Boolean;
  end;

  GSPolicyResponse_t = record
    m_bSecure: UInt8;
  end;

  GSGameplayStats_t = record
    m_eResult: EResult;
    m_nRank: Int32;
    m_unTotalConnects: UInt32;
    m_unTotalMinutesPlayed: UInt32;
  end;

  {$PACKRECORDS 1}
  GSClientGroupStatus_t = record
    m_SteamIDUser: CSteamID;
    m_SteamIDGroup: CSteamID;
    m_bMember: Boolean;
    m_bOfficer: Boolean;
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  GSReputation_t = record
    m_eResult: EResult;
    m_unReputationScore: UInt32;
    m_bBanned: Boolean;
    m_unBannedIP: UInt32;
    m_usBannedPort: UInt16;
    m_ulBannedGameID: UInt64;
    m_unBanExpires: UInt32;
  end;

  AssociateWithClanResult_t = record
    m_eResult: EResult;
  end;

  ComputeNewPlayerCompatibilityResult_t = record
    m_eResult: EResult;
    m_cPlayersThatDontLikeCandidate: Integer;
    m_cPlayersThatCandidateDoesntLike: Integer;
    m_cClanPlayersThatDontLikeCandidate: Integer;
    m_SteamIDCandidate: CSteamID;
  end;

  GSStatsReceived_t = record
    m_eResult: EResult;
    m_steamIDUser: CSteamID;
  end;

  GSStatsStored_t = record
    m_eResult: EResult;
    m_steamIDUser: CSteamID;
  end;

  GSStatsUnloaded_t = record
    m_steamIDUser: CSteamID;
  end;

  SteamNetworkingFakeIPResult_t = record
    m_eResult: EResult;
    m_identity: SteamNetworkingIdentity;
    m_unIP: UInt32;
    m_unPorts: Array[0..8 - 1] of UInt16;
  end;

  {$PACKRECORDS 1}
  SteamInputActionEvent_t__DigitalAction_t = record
    actionHandle: InputDigitalActionHandle_t;
    digitalActionData: InputDigitalActionData_t;
  end;

  SteamInputActionEvent_t__AnalogAction_t = record
    actionHandle: InputAnalogActionHandle_t;
    analogActionData: InputAnalogActionData_t;
  end;

  SteamInputActionEvent_t = record
    controllerHandle: InputHandle_t;
    eEventType: ESteamInputActionEventType;
    case Integer of
      0: (analogAction: SteamInputActionEvent_t__AnalogAction_t);
      1: (digitalAction: SteamInputActionEvent_t__DigitalAction_t);
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}

  PSteamDatagramRelayAuthTicket = ^SteamDatagramRelayAuthTicket;
  SteamDatagramRelayAuthTicket = record end;

const
  CBID_SteamServersConnected_t = 101;
  CBID_SteamServerConnectFailure_t = 102;
  CBID_SteamServersDisconnected_t = 103;
  CBID_ClientGameServerDeny_t = 113;
  CBID_IPCFailure_t = 117;
  CBID_LicensesUpdated_t = 125;
  CBID_ValidateAuthTicketResponse_t = 143;
  CBID_MicroTxnAuthorizationResponse_t = 152;
  CBID_EncryptedAppTicketResponse_t = 154;
  CBID_GetAuthSessionTicketResponse_t = 163;
  CBID_GameWebCallback_t = 164;
  CBID_StoreAuthURLResponse_t = 165;
  CBID_MarketEligibilityResponse_t = 166;
  CBID_DurationControl_t = 167;
  CBID_PersonaStateChange_t = 304;
  CBID_GameOverlayActivated_t = 331;
  CBID_GameServerChangeRequested_t = 332;
  CBID_GameLobbyJoinRequested_t = 333;
  CBID_AvatarImageLoaded_t = 334;
  CBID_ClanOfficerListResponse_t = 335;
  CBID_FriendRichPresenceUpdate_t = 336;
  CBID_GameRichPresenceJoinRequested_t = 337;
  CBID_GameConnectedClanChatMsg_t = 338;
  CBID_GameConnectedChatJoin_t = 339;
  CBID_GameConnectedChatLeave_t = 340;
  CBID_DownloadClanActivityCountsResult_t = 341;
  CBID_JoinClanChatRoomCompletionResult_t = 342;
  CBID_GameConnectedFriendChatMsg_t = 343;
  CBID_FriendsGetFollowerCount_t = 344;
  CBID_FriendsIsFollowing_t = 345;
  CBID_FriendsEnumerateFollowingList_t = 346;
  CBID_SetPersonaNameResponse_t = 347;
  CBID_UnreadChatMessagesChanged_t = 348;
  CBID_OverlayBrowserProtocolNavigation_t = 349;
  CBID_IPCountry_t = 701;
  CBID_LowBatteryPower_t = 702;
  CBID_SteamAPICallCompleted_t = 703;
  CBID_SteamShutdown_t = 704;
  CBID_CheckFileSignature_t = 705;
  CBID_GamepadTextInputDismissed_t = 714;
  CBID_AppResumingFromSuspend_t = 736;
  CBID_FloatingGamepadTextInputDismissed_t = 738;
  CBID_FavoritesListChanged_t = 502;
  CBID_LobbyInvite_t = 503;
  CBID_LobbyEnter_t = 504;
  CBID_LobbyDataUpdate_t = 505;
  CBID_LobbyChatUpdate_t = 506;
  CBID_LobbyChatMsg_t = 507;
  CBID_LobbyGameCreated_t = 509;
  CBID_LobbyMatchList_t = 510;
  CBID_LobbyKicked_t = 512;
  CBID_LobbyCreated_t = 513;
  CBID_PSNGameBootInviteResult_t = 515;
  CBID_FavoritesListAccountsUpdated_t = 516;
  CBID_SearchForGameProgressCallback_t = 5201;
  CBID_SearchForGameResultCallback_t = 5202;
  CBID_RequestPlayersForGameProgressCallback_t = 5211;
  CBID_RequestPlayersForGameResultCallback_t = 5212;
  CBID_RequestPlayersForGameFinalResultCallback_t = 5213;
  CBID_SubmitPlayerResultResultCallback_t = 5214;
  CBID_EndGameResultCallback_t = 5215;
  CBID_JoinPartyCallback_t = 5301;
  CBID_CreateBeaconCallback_t = 5302;
  CBID_ReservationNotificationCallback_t = 5303;
  CBID_ChangeNumOpenSlotsCallback_t = 5304;
  CBID_AvailableBeaconLocationsUpdated_t = 5305;
  CBID_ActiveBeaconsUpdated_t = 5306;
  CBID_RemoteStorageFileShareResult_t = 1307;
  CBID_RemoteStoragePublishFileResult_t = 1309;
  CBID_RemoteStorageDeletePublishedFileResult_t = 1311;
  CBID_RemoteStorageEnumerateUserPublishedFilesResult_t = 1312;
  CBID_RemoteStorageSubscribePublishedFileResult_t = 1313;
  CBID_RemoteStorageEnumerateUserSubscribedFilesResult_t = 1314;
  CBID_RemoteStorageUnsubscribePublishedFileResult_t = 1315;
  CBID_RemoteStorageUpdatePublishedFileResult_t = 1316;
  CBID_RemoteStorageDownloadUGCResult_t = 1317;
  CBID_RemoteStorageGetPublishedFileDetailsResult_t = 1318;
  CBID_RemoteStorageEnumerateWorkshopFilesResult_t = 1319;
  CBID_RemoteStorageGetPublishedItemVoteDetailsResult_t = 1320;
  CBID_RemoteStoragePublishedFileSubscribed_t = 1321;
  CBID_RemoteStoragePublishedFileUnsubscribed_t = 1322;
  CBID_RemoteStoragePublishedFileDeleted_t = 1323;
  CBID_RemoteStorageUpdateUserPublishedItemVoteResult_t = 1324;
  CBID_RemoteStorageUserVoteDetails_t = 1325;
  CBID_RemoteStorageEnumerateUserSharedWorkshopFilesResult_t = 1326;
  CBID_RemoteStorageSetUserPublishedFileActionResult_t = 1327;
  CBID_RemoteStorageEnumeratePublishedFilesByUserActionResult_t = 1328;
  CBID_RemoteStoragePublishFileProgress_t = 1329;
  CBID_RemoteStoragePublishedFileUpdated_t = 1330;
  CBID_RemoteStorageFileWriteAsyncComplete_t = 1331;
  CBID_RemoteStorageFileReadAsyncComplete_t = 1332;
  CBID_RemoteStorageLocalFileChange_t = 1333;
  CBID_UserStatsReceived_t = 1101;
  CBID_UserStatsStored_t = 1102;
  CBID_UserAchievementStored_t = 1103;
  CBID_LeaderboardFindResult_t = 1104;
  CBID_LeaderboardScoresDownloaded_t = 1105;
  CBID_LeaderboardScoreUploaded_t = 1106;
  CBID_NumberOfCurrentPlayers_t = 1107;
  CBID_UserStatsUnloaded_t = 1108;
  CBID_UserAchievementIconFetched_t = 1109;
  CBID_GlobalAchievementPercentagesReady_t = 1110;
  CBID_LeaderboardUGCSet_t = 1111;
  CBID_PS3TrophiesInstalled_t = 1112;
  CBID_GlobalStatsReceived_t = 1112;
  CBID_DlcInstalled_t = 1005;
  CBID_RegisterActivationCodeResponse_t = 1008;
  CBID_NewUrlLaunchParameters_t = 1014;
  CBID_AppProofOfPurchaseKeyResponse_t = 1021;
  CBID_FileDetailsResult_t = 1023;
  CBID_TimedTrialStatus_t = 1030;
  CBID_P2PSessionRequest_t = 1202;
  CBID_P2PSessionConnectFail_t = 1203;
  CBID_SocketStatusCallback_t = 1201;
  CBID_ScreenshotReady_t = 2301;
  CBID_ScreenshotRequested_t = 2302;
  CBID_PlaybackStatusHasChanged_t = 4001;
  CBID_VolumeHasChanged_t = 4002;
  CBID_MusicPlayerRemoteWillActivate_t = 4101;
  CBID_MusicPlayerRemoteWillDeactivate_t = 4102;
  CBID_MusicPlayerRemoteToFront_t = 4103;
  CBID_MusicPlayerWillQuit_t = 4104;
  CBID_MusicPlayerWantsPlay_t = 4105;
  CBID_MusicPlayerWantsPause_t = 4106;
  CBID_MusicPlayerWantsPlayPrevious_t = 4107;
  CBID_MusicPlayerWantsPlayNext_t = 4108;
  CBID_MusicPlayerWantsShuffled_t = 4109;
  CBID_MusicPlayerWantsLooped_t = 4110;
  CBID_MusicPlayerWantsVolume_t = 4011;
  CBID_MusicPlayerSelectsQueueEntry_t = 4012;
  CBID_MusicPlayerSelectsPlaylistEntry_t = 4013;
  CBID_MusicPlayerWantsPlayingRepeatStatus_t = 4114;
  CBID_HTTPRequestCompleted_t = 2101;
  CBID_HTTPRequestHeadersReceived_t = 2102;
  CBID_HTTPRequestDataReceived_t = 2103;
  CBID_SteamInputDeviceConnected_t = 2801;
  CBID_SteamInputDeviceDisconnected_t = 2802;
  CBID_SteamInputConfigurationLoaded_t = 2803;
  CBID_SteamUGCQueryCompleted_t = 3401;
  CBID_SteamUGCRequestUGCDetailsResult_t = 3402;
  CBID_CreateItemResult_t = 3403;
  CBID_SubmitItemUpdateResult_t = 3404;
  CBID_ItemInstalled_t = 3405;
  CBID_DownloadItemResult_t = 3406;
  CBID_UserFavoriteItemsListChanged_t = 3407;
  CBID_SetUserItemVoteResult_t = 3408;
  CBID_GetUserItemVoteResult_t = 3409;
  CBID_StartPlaytimeTrackingResult_t = 3410;
  CBID_StopPlaytimeTrackingResult_t = 3411;
  CBID_AddUGCDependencyResult_t = 3412;
  CBID_RemoveUGCDependencyResult_t = 3413;
  CBID_AddAppDependencyResult_t = 3414;
  CBID_RemoveAppDependencyResult_t = 3415;
  CBID_GetAppDependenciesResult_t = 3416;
  CBID_DeleteItemResult_t = 3417;
  CBID_UserSubscribedItemsListChanged_t = 3418;
  CBID_WorkshopEULAStatus_t = 3420;
  CBID_SteamAppInstalled_t = 3901;
  CBID_SteamAppUninstalled_t = 3902;
  CBID_HTML_BrowserReady_t = 4501;
  CBID_HTML_NeedsPaint_t = 4502;
  CBID_HTML_StartRequest_t = 4503;
  CBID_HTML_CloseBrowser_t = 4504;
  CBID_HTML_URLChanged_t = 4505;
  CBID_HTML_FinishedRequest_t = 4506;
  CBID_HTML_OpenLinkInNewTab_t = 4507;
  CBID_HTML_ChangedTitle_t = 4508;
  CBID_HTML_SearchResults_t = 4509;
  CBID_HTML_CanGoBackAndForward_t = 4510;
  CBID_HTML_HorizontalScroll_t = 4511;
  CBID_HTML_VerticalScroll_t = 4512;
  CBID_HTML_LinkAtPosition_t = 4513;
  CBID_HTML_JSAlert_t = 4514;
  CBID_HTML_JSConfirm_t = 4515;
  CBID_HTML_FileOpenDialog_t = 4516;
  CBID_HTML_NewWindow_t = 4521;
  CBID_HTML_SetCursor_t = 4522;
  CBID_HTML_StatusText_t = 4523;
  CBID_HTML_ShowToolTip_t = 4524;
  CBID_HTML_UpdateToolTip_t = 4525;
  CBID_HTML_HideToolTip_t = 4526;
  CBID_HTML_BrowserRestarted_t = 4527;
  CBID_SteamInventoryResultReady_t = 4700;
  CBID_SteamInventoryFullUpdate_t = 4701;
  CBID_SteamInventoryDefinitionUpdate_t = 4702;
  CBID_SteamInventoryEligiblePromoItemDefIDs_t = 4703;
  CBID_SteamInventoryStartPurchaseResult_t = 4704;
  CBID_SteamInventoryRequestPricesResult_t = 4705;
  CBID_GetVideoURLResult_t = 4611;
  CBID_GetOPFSettingsResult_t = 4624;
  CBID_SteamParentalSettingsChanged_t = 5001;
  CBID_SteamRemotePlaySessionConnected_t = 5701;
  CBID_SteamRemotePlaySessionDisconnected_t = 5702;
  CBID_SteamNetworkingMessagesSessionRequest_t = 1251;
  CBID_SteamNetworkingMessagesSessionFailed_t = 1252;
  CBID_SteamNetConnectionStatusChangedCallback_t = 1221;
  CBID_SteamNetAuthenticationStatus_t = 1222;
  CBID_SteamRelayNetworkStatus_t = 1281;
  CBID_GSClientApprove_t = 201;
  CBID_GSClientDeny_t = 202;
  CBID_GSClientKick_t = 203;
  CBID_GSClientAchievementStatus_t = 206;
  CBID_GSPolicyResponse_t = 115;
  CBID_GSGameplayStats_t = 207;
  CBID_GSClientGroupStatus_t = 208;
  CBID_GSReputation_t = 209;
  CBID_AssociateWithClanResult_t = 210;
  CBID_ComputeNewPlayerCompatibilityResult_t = 211;
  CBID_GSStatsReceived_t = 1800;
  CBID_GSStatsStored_t = 1801;
  CBID_GSStatsUnloaded_t = 1108;
  CBID_SteamNetworkingFakeIPResult_t = 1223;

  SteamNetworkingFakeIPResult_t__k_nMaxReturnPorts: Integer = 8;
  k_uAppIdInvalid: AppId_t = $0;
  k_uDepotIdInvalid: DepotId_t = $0;
  k_uAPICallInvalid: SteamAPICall_t = $0;
  k_ulPartyBeaconIdInvalid: PartyBeaconID_t = 0;
  k_HAuthTicketInvalid: HAuthTicket = 0;
  k_unSteamAccountIDMask: UInt32 = $FFFFFFFF;
  k_unSteamAccountInstanceMask: UInt32 = $000FFFFF;
  k_unSteamUserDefaultInstance: UInt32 = 1;
  k_cchGameExtraInfoMax: Integer = 64;
  k_cchMaxFriendsGroupName: Integer = 64;
  k_cFriendsGroupLimit: Integer = 100;
  k_FriendsGroupID_Invalid: FriendsGroupID_t = - 1;
  k_cEnumerateFollowersMax: Integer = 50;
  k_cubChatMetadataMax: UInt32 = 8192;
  k_cbMaxGameServerGameDir: Integer = 32;
  k_cbMaxGameServerMapName: Integer = 32;
  k_cbMaxGameServerGameDescription: Integer = 64;
  k_cbMaxGameServerName: Integer = 64;
  k_cbMaxGameServerTags: Integer = 128;
  k_cbMaxGameServerGameData: Integer = 2048;
  HSERVERQUERY_INVALID: Integer = $ffffffff;
  k_unFavoriteFlagNone: UInt32 = $00;
  k_unFavoriteFlagFavorite: UInt32 = $01;
  k_unFavoriteFlagHistory: UInt32 = $02;
  k_unMaxCloudFileChunkSize: UInt32 = 100 * 1024 * 1024;
  k_PublishedFileIdInvalid: PublishedFileId_t = 0;
  k_UGCHandleInvalid: UGCHandle_t = $ffffffffffffffff;
  k_PublishedFileUpdateHandleInvalid: PublishedFileUpdateHandle_t = $ffffffffffffffff;
  k_UGCFileStreamHandleInvalid: UGCFileWriteStreamHandle_t = $ffffffffffffffff;
  k_cchPublishedDocumentTitleMax: UInt32 = 128 + 1;
  k_cchPublishedDocumentDescriptionMax: UInt32 = 8000;
  k_cchPublishedDocumentChangeDescriptionMax: UInt32 = 8000;
  k_unEnumeratePublishedFilesMaxResults: UInt32 = 50;
  k_cchTagListMax: UInt32 = 1024 + 1;
  k_cchFilenameMax: UInt32 = 260;
  k_cchPublishedFileURLMax: UInt32 = 256;
  k_cubAppProofOfPurchaseKeyMax: Integer = 240;
  k_nScreenshotMaxTaggedUsers: UInt32 = 32;
  k_nScreenshotMaxTaggedPublishedFiles: UInt32 = 32;
  k_cubUFSTagTypeMax: Integer = 255;
  k_cubUFSTagValueMax: Integer = 255;
  k_ScreenshotThumbWidth: Integer = 200;
  k_UGCQueryHandleInvalid: UGCQueryHandle_t = $ffffffffffffffff;
  k_UGCUpdateHandleInvalid: UGCUpdateHandle_t = $ffffffffffffffff;
  kNumUGCResultsPerPage: UInt32 = 50;
  k_cchDeveloperMetadataMax: UInt32 = 5000;
  INVALID_HTMLBROWSER: UInt32 = 0;
  k_SteamItemInstanceIDInvalid: SteamItemInstanceID_t = not 0;
  k_SteamInventoryResultInvalid: SteamInventoryResult_t = - 1;
  k_SteamInventoryUpdateHandleInvalid: SteamInventoryUpdateHandle_t = $ffffffffffffffff;
  k_HSteamNetConnection_Invalid: HSteamNetConnection = 0;
  k_HSteamListenSocket_Invalid: HSteamListenSocket = 0;
  k_HSteamNetPollGroup_Invalid: HSteamNetPollGroup = 0;
  k_cchMaxSteamNetworkingErrMsg: Integer = 1024;
  k_cchSteamNetworkingMaxConnectionCloseReason: Integer = 128;
  k_cchSteamNetworkingMaxConnectionDescription: Integer = 128;
  k_cchSteamNetworkingMaxConnectionAppName: Integer = 32;
  k_nSteamNetworkConnectionInfoFlags_Unauthenticated: Integer = 1;
  k_nSteamNetworkConnectionInfoFlags_Unencrypted: Integer = 2;
  k_nSteamNetworkConnectionInfoFlags_LoopbackBuffers: Integer = 4;
  k_nSteamNetworkConnectionInfoFlags_Fast: Integer = 8;
  k_nSteamNetworkConnectionInfoFlags_Relayed: Integer = 16;
  k_nSteamNetworkConnectionInfoFlags_DualWifi: Integer = 32;
  k_cbMaxSteamNetworkingSocketsMessageSizeSend: Integer = 512 * 1024;
  k_nSteamNetworkingSend_Unreliable: Integer = 0;
  k_nSteamNetworkingSend_NoNagle: Integer = 1;
  k_nSteamNetworkingSend_UnreliableNoNagle: Integer = 0 or 1;
  k_nSteamNetworkingSend_NoDelay: Integer = 4;
  k_nSteamNetworkingSend_UnreliableNoDelay: Integer = 0 or 4 or 1;
  k_nSteamNetworkingSend_Reliable: Integer = 8;
  k_nSteamNetworkingSend_ReliableNoNagle: Integer = 8 or 1;
  k_nSteamNetworkingSend_UseCurrentThread: Integer = 16;
  k_nSteamNetworkingSend_AutoRestartBrokenSession: Integer = 32;
  k_cchMaxSteamNetworkingPingLocationString: Integer = 1024;
  k_nSteamNetworkingPing_Failed: Integer = - 1;
  k_nSteamNetworkingPing_Unknown: Integer = - 2;
  k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Default: Integer = - 1;
  k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Disable: Integer = 0;
  k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Relay: Integer = 1;
  k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Private: Integer = 2;
  k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Public: Integer = 4;
  k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_All: Integer = $7fffffff;
  k_SteamDatagramPOPID_dev: SteamNetworkingPOPID = 6579574;
  STEAMGAMESERVER_QUERY_PORT_SHARED: UInt16 = $ffff;
  MASTERSERVERUPDATERPORT_USEGAMESOCKETSHARE: UInt16 = $ffff;
  k_cbSteamDatagramMaxSerializedTicket: UInt32 = 512;
  k_cbMaxSteamDatagramGameCoordinatorServerLoginAppData: UInt32 = 2048;
  k_cbMaxSteamDatagramGameCoordinatorServerLoginSerialized: UInt32 = 4096;
  k_cbSteamNetworkingSocketsFakeUDPPortRecommendedMTU: Integer = 1200;
  k_cbSteamNetworkingSocketsFakeUDPPortMaxMessageSize: Integer = 4096;
  SteamNetworkingIPAddr__k_cchMaxString: Integer = 48;
  SteamNetworkingIdentity__k_cchMaxString: Integer = 128;
  SteamNetworkingIdentity__k_cchMaxGenericString: Integer = 32;
  SteamNetworkingIdentity__k_cchMaxXboxPairwiseID: Integer = 33;
  SteamNetworkingIdentity__k_cbMaxGenericBytes: Integer = 32;

type
  PISteamClient = ^ISteamClient;
  ISteamClient = record end;
  PISteamUser = ^ISteamUser;
  ISteamUser = record end;
  PISteamFriends = ^ISteamFriends;
  ISteamFriends = record end;
  PISteamUtils = ^ISteamUtils;
  ISteamUtils = record end;
  PISteamMatchmaking = ^ISteamMatchmaking;
  ISteamMatchmaking = record end;
  PISteamMatchmakingServerListResponse = ^ISteamMatchmakingServerListResponse;
  ISteamMatchmakingServerListResponse = record end;
  PISteamMatchmakingPingResponse = ^ISteamMatchmakingPingResponse;
  ISteamMatchmakingPingResponse = record end;
  PISteamMatchmakingPlayersResponse = ^ISteamMatchmakingPlayersResponse;
  ISteamMatchmakingPlayersResponse = record end;
  PISteamMatchmakingRulesResponse = ^ISteamMatchmakingRulesResponse;
  ISteamMatchmakingRulesResponse = record end;
  PISteamMatchmakingServers = ^ISteamMatchmakingServers;
  ISteamMatchmakingServers = record end;
  PISteamGameSearch = ^ISteamGameSearch;
  ISteamGameSearch = record end;
  PISteamParties = ^ISteamParties;
  ISteamParties = record end;
  PISteamRemoteStorage = ^ISteamRemoteStorage;
  ISteamRemoteStorage = record end;
  PISteamUserStats = ^ISteamUserStats;
  ISteamUserStats = record end;
  PISteamApps = ^ISteamApps;
  ISteamApps = record end;
  PISteamNetworking = ^ISteamNetworking;
  ISteamNetworking = record end;
  PISteamScreenshots = ^ISteamScreenshots;
  ISteamScreenshots = record end;
  PISteamMusic = ^ISteamMusic;
  ISteamMusic = record end;
  PISteamMusicRemote = ^ISteamMusicRemote;
  ISteamMusicRemote = record end;
  PISteamHTTP = ^ISteamHTTP;
  ISteamHTTP = record end;
  PISteamInput = ^ISteamInput;
  ISteamInput = record end;
  PISteamController = ^ISteamController;
  ISteamController = record end;
  PISteamUGC = ^ISteamUGC;
  ISteamUGC = record end;
  PISteamAppList = ^ISteamAppList;
  ISteamAppList = record end;
  PISteamHTMLSurface = ^ISteamHTMLSurface;
  ISteamHTMLSurface = record end;
  PISteamInventory = ^ISteamInventory;
  ISteamInventory = record end;
  PISteamVideo = ^ISteamVideo;
  ISteamVideo = record end;
  PISteamParentalSettings = ^ISteamParentalSettings;
  ISteamParentalSettings = record end;
  PISteamRemotePlay = ^ISteamRemotePlay;
  ISteamRemotePlay = record end;
  PISteamNetworkingMessages = ^ISteamNetworkingMessages;
  ISteamNetworkingMessages = record end;
  PISteamNetworkingSockets = ^ISteamNetworkingSockets;
  ISteamNetworkingSockets = record end;
  PISteamNetworkingUtils = ^ISteamNetworkingUtils;
  ISteamNetworkingUtils = record end;
  PISteamGameServer = ^ISteamGameServer;
  ISteamGameServer = record end;
  PISteamGameServerStats = ^ISteamGameServerStats;
  ISteamGameServerStats = record end;
  PISteamNetworkingFakeUDPPort = ^ISteamNetworkingFakeUDPPort;
  ISteamNetworkingFakeUDPPort = record end;
  PISteamNetworkingConnectionSignaling = ^ISteamNetworkingConnectionSignaling;
  ISteamNetworkingConnectionSignaling = record end;
  PISteamNetworkingSignalingRecvContext = ^ISteamNetworkingSignalingRecvContext;
  ISteamNetworkingSignalingRecvContext = record end;

  PCallbackMsg_t = ^CallbackMsg_t;
  CallbackMsg_t = record
    m_hSteamUser: HSteamUser;
    m_iCallback: Integer;
    m_pubParam: PUInt8;
    m_cubParam: Integer;
  end;

{$IFNDEF STEAM}
function GameNetworkingSockets_Init(const pIdentity: PSteamNetworkingIdentity; errMsg: PSteamNetworkingErrMsg): Boolean; cdecl; external STEAMLIB;
procedure GameNetworkingSockets_Kill(); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingSockets_v009(): PISteamNetworkingSockets; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingUtils_v003(): PISteamNetworkingUtils; cdecl; external STEAMLIB;
{$ENDIF}

function SteamAPI_SteamUser_v021(): PISteamUser; cdecl; external STEAMLIB;
function SteamAPI_SteamFriends_v017(): PISteamFriends; cdecl; external STEAMLIB;
function SteamAPI_SteamUtils_v010(): PISteamUtils; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerUtils_v010(): PISteamUtils; cdecl; external STEAMLIB;
function SteamAPI_SteamMatchmaking_v009(): PISteamMatchmaking; cdecl; external STEAMLIB;
function SteamAPI_SteamMatchmakingServers_v002(): PISteamMatchmakingServers; cdecl; external STEAMLIB;
function SteamAPI_SteamGameSearch_v001(): PISteamGameSearch; cdecl; external STEAMLIB;
function SteamAPI_SteamParties_v002(): PISteamParties; cdecl; external STEAMLIB;
function SteamAPI_SteamRemoteStorage_v016(): PISteamRemoteStorage; cdecl; external STEAMLIB;
function SteamAPI_SteamUserStats_v012(): PISteamUserStats; cdecl; external STEAMLIB;
function SteamAPI_SteamApps_v008(): PISteamApps; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworking_v006(): PISteamNetworking; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerNetworking_v006(): PISteamNetworking; cdecl; external STEAMLIB;
function SteamAPI_SteamScreenshots_v003(): PISteamScreenshots; cdecl; external STEAMLIB;
function SteamAPI_SteamMusic_v001(): PISteamMusic; cdecl; external STEAMLIB;
function SteamAPI_SteamMusicRemote_v001(): PISteamMusicRemote; cdecl; external STEAMLIB;
function SteamAPI_SteamHTTP_v003(): PISteamHTTP; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerHTTP_v003(): PISteamHTTP; cdecl; external STEAMLIB;
function SteamAPI_SteamInput_v006(): PISteamInput; cdecl; external STEAMLIB;
function SteamAPI_SteamController_v008(): PISteamController; cdecl; external STEAMLIB;
function SteamAPI_SteamUGC_v016(): PISteamUGC; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerUGC_v016(): PISteamUGC; cdecl; external STEAMLIB;
function SteamAPI_SteamAppList_v001(): PISteamAppList; cdecl; external STEAMLIB;
function SteamAPI_SteamHTMLSurface_v005(): PISteamHTMLSurface; cdecl; external STEAMLIB;
function SteamAPI_SteamInventory_v003(): PISteamInventory; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerInventory_v003(): PISteamInventory; cdecl; external STEAMLIB;
function SteamAPI_SteamVideo_v002(): PISteamVideo; cdecl; external STEAMLIB;
function SteamAPI_SteamParentalSettings_v001(): PISteamParentalSettings; cdecl; external STEAMLIB;
function SteamAPI_SteamRemotePlay_v001(): PISteamRemotePlay; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingMessages_SteamAPI_v002(): PISteamNetworkingMessages; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerNetworkingMessages_SteamAPI_v002(): PISteamNetworkingMessages; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingSockets_SteamAPI_v012(): PISteamNetworkingSockets; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerNetworkingSockets_SteamAPI_v012(): PISteamNetworkingSockets; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingUtils_SteamAPI_v004(): PISteamNetworkingUtils; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServer_v014(): PISteamGameServer; cdecl; external STEAMLIB;
function SteamAPI_SteamGameServerStats_v001(): PISteamGameServerStats; cdecl; external STEAMLIB;

function SteamAPI_SteamIPAddress_t_IsSet(ASteamIPAddress_t: PSteamIPAddress_t): Boolean; cdecl; external STEAMLIB;

procedure SteamAPI_MatchMakingKeyValuePair_t_Construct(AMatchMakingKeyValuePair_t: PMatchMakingKeyValuePair_t); cdecl; external STEAMLIB;

procedure SteamAPI_servernetadr_t_Construct(Aservernetadr_t: Pservernetadr_t); cdecl; external STEAMLIB;
procedure SteamAPI_servernetadr_t_Init(Aservernetadr_t: Pservernetadr_t; ip: UInt32; usQueryPort: UInt16; usConnectionPort: UInt16); cdecl; external STEAMLIB;
function SteamAPI_servernetadr_t_GetQueryPort(Aservernetadr_t: Pservernetadr_t): UInt16; cdecl; external STEAMLIB;
procedure SteamAPI_servernetadr_t_SetQueryPort(Aservernetadr_t: Pservernetadr_t; usPort: UInt16); cdecl; external STEAMLIB;
function SteamAPI_servernetadr_t_GetConnectionPort(Aservernetadr_t: Pservernetadr_t): UInt16; cdecl; external STEAMLIB;
procedure SteamAPI_servernetadr_t_SetConnectionPort(Aservernetadr_t: Pservernetadr_t; usPort: UInt16); cdecl; external STEAMLIB;
function SteamAPI_servernetadr_t_GetIP(Aservernetadr_t: Pservernetadr_t): UInt32; cdecl; external STEAMLIB;
procedure SteamAPI_servernetadr_t_SetIP(Aservernetadr_t: Pservernetadr_t; unIP: UInt32); cdecl; external STEAMLIB;
function SteamAPI_servernetadr_t_GetConnectionAddressString(Aservernetadr_t: Pservernetadr_t): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_servernetadr_t_GetQueryAddressString(Aservernetadr_t: Pservernetadr_t): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_servernetadr_t_IsLessThan(Aservernetadr_t: Pservernetadr_t; netadr: Pservernetadr_t): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_servernetadr_t_Assign(Aservernetadr_t: Pservernetadr_t; that: Pservernetadr_t); cdecl; external STEAMLIB;

procedure SteamAPI_gameserveritem_t_Construct(Agameserveritem_t: Pgameserveritem_t); cdecl; external STEAMLIB;
function SteamAPI_gameserveritem_t_GetName(Agameserveritem_t: Pgameserveritem_t): PAnsiChar; cdecl; external STEAMLIB;
procedure SteamAPI_gameserveritem_t_SetName(Agameserveritem_t: Pgameserveritem_t; pName: PAnsiChar); cdecl; external STEAMLIB;

procedure SteamAPI_SteamNetworkingIPAddr_Clear(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_IsIPv6AllZeros(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIPAddr_SetIPv6(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr; ipv6: PUInt8; nPort: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIPAddr_SetIPv4(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr; nIP: UInt32; nPort: UInt16); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_IsIPv4(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr): Boolean; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_GetIPv4(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr): UInt32; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIPAddr_SetIPv6LocalHost(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr; nPort: UInt16); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_IsLocalHost(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIPAddr_ToString(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr; buf: PAnsiChar; cbBuf: UInt32; bWithPort: Boolean); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_ParseString(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr; pszStr: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_IsEqualTo(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr; x: PSteamNetworkingIPAddr): Boolean; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_GetFakeIPType(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr): ESteamNetworkingFakeIPType; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIPAddr_IsFakeIP(ASteamNetworkingIPAddr: PSteamNetworkingIPAddr): Boolean; cdecl; external STEAMLIB;

procedure SteamAPI_SteamNetworkingIdentity_Clear(ASteamNetworkingIdentity: PSteamNetworkingIdentity); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_IsInvalid(ASteamNetworkingIdentity: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetSteamID(ASteamNetworkingIdentity: PSteamNetworkingIdentity; steamID: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetSteamID(ASteamNetworkingIdentity: PSteamNetworkingIdentity): CSteamID; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetSteamID64(ASteamNetworkingIdentity: PSteamNetworkingIdentity; steamID: UInt64); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetSteamID64(ASteamNetworkingIdentity: PSteamNetworkingIdentity): UInt64; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_SetXboxPairwiseID(ASteamNetworkingIdentity: PSteamNetworkingIdentity; pszString: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetXboxPairwiseID(ASteamNetworkingIdentity: PSteamNetworkingIdentity): PAnsiChar; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetPSNID(ASteamNetworkingIdentity: PSteamNetworkingIdentity; id: UInt64); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetPSNID(ASteamNetworkingIdentity: PSteamNetworkingIdentity): UInt64; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetStadiaID(ASteamNetworkingIdentity: PSteamNetworkingIdentity; id: UInt64); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetStadiaID(ASteamNetworkingIdentity: PSteamNetworkingIdentity): UInt64; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetIPAddr(ASteamNetworkingIdentity: PSteamNetworkingIdentity; addr: PSteamNetworkingIPAddr); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetIPAddr(ASteamNetworkingIdentity: PSteamNetworkingIdentity): PSteamNetworkingIPAddr; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetIPv4Addr(ASteamNetworkingIdentity: PSteamNetworkingIdentity; nIPv4: UInt32; nPort: UInt16); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetIPv4(ASteamNetworkingIdentity: PSteamNetworkingIdentity): UInt32; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetFakeIPType(ASteamNetworkingIdentity: PSteamNetworkingIdentity): ESteamNetworkingFakeIPType; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_IsFakeIP(ASteamNetworkingIdentity: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_SetLocalHost(ASteamNetworkingIdentity: PSteamNetworkingIdentity); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_IsLocalHost(ASteamNetworkingIdentity: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_SetGenericString(ASteamNetworkingIdentity: PSteamNetworkingIdentity; pszString: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetGenericString(ASteamNetworkingIdentity: PSteamNetworkingIdentity): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_SetGenericBytes(ASteamNetworkingIdentity: PSteamNetworkingIdentity; data: Pointer; cbLen: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_GetGenericBytes(ASteamNetworkingIdentity: PSteamNetworkingIdentity; cbLen: PInteger): PUInt8; cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_IsEqualTo(ASteamNetworkingIdentity: PSteamNetworkingIdentity; x: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingIdentity_ToString(ASteamNetworkingIdentity: PSteamNetworkingIdentity; buf: PAnsiChar; cbBuf: UInt32); cdecl; external STEAMLIB;
function SteamAPI_SteamNetworkingIdentity_ParseString(ASteamNetworkingIdentity: PSteamNetworkingIdentity; pszStr: PAnsiChar): Boolean; cdecl; external STEAMLIB;

procedure SteamAPI_SteamNetworkingMessage_t_Release(ASteamNetworkingMessage_t: PSteamNetworkingMessage_t); cdecl; external STEAMLIB;

procedure SteamAPI_SteamNetworkingConfigValue_t_SetInt32(ASteamNetworkingConfigValue_t: PSteamNetworkingConfigValue_t; eVal: ESteamNetworkingConfigValue; data: Int32); cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingConfigValue_t_SetInt64(ASteamNetworkingConfigValue_t: PSteamNetworkingConfigValue_t; eVal: ESteamNetworkingConfigValue; data: Int64); cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingConfigValue_t_SetFloat(ASteamNetworkingConfigValue_t: PSteamNetworkingConfigValue_t; eVal: ESteamNetworkingConfigValue; data: Single); cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingConfigValue_t_SetPtr(ASteamNetworkingConfigValue_t: PSteamNetworkingConfigValue_t; eVal: ESteamNetworkingConfigValue; data: Pointer); cdecl; external STEAMLIB;
procedure SteamAPI_SteamNetworkingConfigValue_t_SetString(ASteamNetworkingConfigValue_t: PSteamNetworkingConfigValue_t; eVal: ESteamNetworkingConfigValue; data: PAnsiChar); cdecl; external STEAMLIB;

procedure SteamAPI_SteamDatagramHostedAddress_Clear(ASteamDatagramHostedAddress: PSteamDatagramHostedAddress); cdecl; external STEAMLIB;
function SteamAPI_SteamDatagramHostedAddress_GetPopID(ASteamDatagramHostedAddress: PSteamDatagramHostedAddress): SteamNetworkingPOPID; cdecl; external STEAMLIB;
procedure SteamAPI_SteamDatagramHostedAddress_SetDevAddress(ASteamDatagramHostedAddress: PSteamDatagramHostedAddress; nIP: UInt32; nPort: UInt16; popid: SteamNetworkingPOPID); cdecl; external STEAMLIB;

function SteamAPI_ISteamClient_CreateSteamPipe(AISteamClient: PISteamClient): HSteamPipe; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_BReleaseSteamPipe(AISteamClient: PISteamClient; hSteamPipe: HSteamPipe): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_ConnectToGlobalUser(AISteamClient: PISteamClient; hSteamPipe: HSteamPipe): HSteamUser; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_CreateLocalUser(AISteamClient: PISteamClient; phSteamPipe: PHSteamPipe; eAccountType: EAccountType): HSteamUser; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamClient_ReleaseUser(AISteamClient: PISteamClient; hSteamPipe: HSteamPipe; hUser: HSteamUser); cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamUser(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamUser; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamGameServer(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamGameServer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamClient_SetLocalIPBinding(AISteamClient: PISteamClient; unIP: PSteamIPAddress_t; usPort: UInt16); cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamFriends(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamFriends; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamUtils(AISteamClient: PISteamClient; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamUtils; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamMatchmaking(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamMatchmaking; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamMatchmakingServers(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamMatchmakingServers; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamGenericInterface(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): Pointer; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamUserStats(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamUserStats; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamGameServerStats(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamGameServerStats; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamApps(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamApps; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamNetworking(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamNetworking; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamRemoteStorage(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamRemoteStorage; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamScreenshots(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamScreenshots; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamGameSearch(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamGameSearch; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetIPCCallCount(AISteamClient: PISteamClient): UInt32; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamClient_SetWarningMessageHook(AISteamClient: PISteamClient; pFunction: SteamAPIWarningMessageHook_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_BShutdownIfAllPipesClosed(AISteamClient: PISteamClient): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamHTTP(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamHTTP; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamController(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamController; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamUGC(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamUGC; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamAppList(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamAppList; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamMusic(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamMusic; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamMusicRemote(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamMusicRemote; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamHTMLSurface(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamHTMLSurface; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamInventory(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamInventory; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamVideo(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamVideo; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamParentalSettings(AISteamClient: PISteamClient; hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamParentalSettings; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamInput(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamInput; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamParties(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamParties; cdecl; external STEAMLIB;
function SteamAPI_ISteamClient_GetISteamRemotePlay(AISteamClient: PISteamClient; hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; pchVersion: PAnsiChar): PISteamRemotePlay; cdecl; external STEAMLIB;

function SteamAPI_ISteamUser_GetHSteamUser(AISteamUser: PISteamUser): HSteamUser; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BLoggedOn(AISteamUser: PISteamUser): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetSteamID(AISteamUser: PISteamUser): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_InitiateGameConnection_DEPRECATED(AISteamUser: PISteamUser; pAuthBlob: Pointer; cbMaxAuthBlob: Integer; steamIDGameServer: CSteamID; unIPServer: UInt32; usPortServer: UInt16; bSecure: Boolean): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUser_TerminateGameConnection_DEPRECATED(AISteamUser: PISteamUser; unIPServer: UInt32; usPortServer: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUser_TrackAppUsageEvent(AISteamUser: PISteamUser; gameID: CGameID; eAppUsageEvent: Integer; pchExtraInfo: PAnsiChar); cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetUserDataFolder(AISteamUser: PISteamUser; pchBuffer: PAnsiChar; cubBuffer: Integer): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUser_StartVoiceRecording(AISteamUser: PISteamUser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUser_StopVoiceRecording(AISteamUser: PISteamUser); cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetAvailableVoice(AISteamUser: PISteamUser; pcbCompressed: PUInt32; pcbUncompressed_Deprecated: PUInt32; nUncompressedVoiceDesiredSampleRate_Deprecated: UInt32): EVoiceResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetVoice(AISteamUser: PISteamUser; bWantCompressed: Boolean; pDestBuffer: Pointer; cbDestBufferSize: UInt32; nBytesWritten: PUInt32; bWantUncompressed_Deprecated: Boolean; pUncompressedDestBuffer_Deprecated: Pointer; cbUncompressedDestBufferSize_Deprecated: UInt32; nUncompressBytesWritten_Deprecated: PUInt32; nUncompressedVoiceDesiredSampleRate_Deprecated: UInt32): EVoiceResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_DecompressVoice(AISteamUser: PISteamUser; pCompressed: Pointer; cbCompressed: UInt32; pDestBuffer: Pointer; cbDestBufferSize: UInt32; nBytesWritten: PUInt32; nDesiredSampleRate: UInt32): EVoiceResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetVoiceOptimalSampleRate(AISteamUser: PISteamUser): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetAuthSessionTicket(AISteamUser: PISteamUser; pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): HAuthTicket; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BeginAuthSession(AISteamUser: PISteamUser; pAuthTicket: Pointer; cbAuthTicket: Integer; steamID: CSteamID): EBeginAuthSessionResult; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUser_EndAuthSession(AISteamUser: PISteamUser; steamID: CSteamID); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUser_CancelAuthTicket(AISteamUser: PISteamUser; hAuthTicket: HAuthTicket); cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_UserHasLicenseForApp(AISteamUser: PISteamUser; steamID: CSteamID; appID: AppId_t): EUserHasLicenseForAppResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BIsBehindNAT(AISteamUser: PISteamUser): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUser_AdvertiseGame(AISteamUser: PISteamUser; steamIDGameServer: CSteamID; unIPServer: UInt32; usPortServer: UInt16); cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_RequestEncryptedAppTicket(AISteamUser: PISteamUser; pDataToInclude: Pointer; cbDataToInclude: Integer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetEncryptedAppTicket(AISteamUser: PISteamUser; pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetGameBadgeLevel(AISteamUser: PISteamUser; nSeries: Integer; bFoil: Boolean): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetPlayerSteamLevel(AISteamUser: PISteamUser): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_RequestStoreAuthURL(AISteamUser: PISteamUser; pchRedirectURL: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BIsPhoneVerified(AISteamUser: PISteamUser): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BIsTwoFactorEnabled(AISteamUser: PISteamUser): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BIsPhoneIdentifying(AISteamUser: PISteamUser): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BIsPhoneRequiringVerification(AISteamUser: PISteamUser): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetMarketEligibility(AISteamUser: PISteamUser): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_GetDurationControl(AISteamUser: PISteamUser): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUser_BSetDurationControlOnlineState(AISteamUser: PISteamUser; eNewState: EDurationControlOnlineState): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamFriends_GetPersonaName(AISteamFriends: PISteamFriends): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_SetPersonaName(AISteamFriends: PISteamFriends; pchPersonaName: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetPersonaState(AISteamFriends: PISteamFriends): EPersonaState; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendCount(AISteamFriends: PISteamFriends; iFriendFlags: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendByIndex(AISteamFriends: PISteamFriends; iFriend: Integer; iFriendFlags: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendRelationship(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): EFriendRelationship; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendPersonaState(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): EPersonaState; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendPersonaName(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendGamePlayed(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; pFriendGameInfo: PFriendGameInfo_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendPersonaNameHistory(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; iPersonaName: Integer): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendSteamLevel(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetPlayerNickname(AISteamFriends: PISteamFriends; steamIDPlayer: CSteamID): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendsGroupCount(AISteamFriends: PISteamFriends): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendsGroupIDByIndex(AISteamFriends: PISteamFriends; iFG: Integer): FriendsGroupID_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendsGroupName(AISteamFriends: PISteamFriends; friendsGroupID: FriendsGroupID_t): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendsGroupMembersCount(AISteamFriends: PISteamFriends; friendsGroupID: FriendsGroupID_t): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_GetFriendsGroupMembersList(AISteamFriends: PISteamFriends; friendsGroupID: FriendsGroupID_t; pOutSteamIDMembers: PCSteamID; nMembersCount: Integer); cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_HasFriend(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; iFriendFlags: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanCount(AISteamFriends: PISteamFriends): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanByIndex(AISteamFriends: PISteamFriends; iClan: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanName(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanTag(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanActivityCounts(AISteamFriends: PISteamFriends; steamIDClan: CSteamID; pnOnline: PInteger; pnInGame: PInteger; pnChatting: PInteger): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_DownloadClanActivityCounts(AISteamFriends: PISteamFriends; psteamIDClans: PCSteamID; cClansToRequest: Integer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendCountFromSource(AISteamFriends: PISteamFriends; steamIDSource: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendFromSourceByIndex(AISteamFriends: PISteamFriends; steamIDSource: CSteamID; iFriend: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_IsUserInSource(AISteamFriends: PISteamFriends; steamIDUser: CSteamID; steamIDSource: CSteamID): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_SetInGameVoiceSpeaking(AISteamFriends: PISteamFriends; steamIDUser: CSteamID; bSpeaking: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ActivateGameOverlay(AISteamFriends: PISteamFriends; pchDialog: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ActivateGameOverlayToUser(AISteamFriends: PISteamFriends; pchDialog: PAnsiChar; steamID: CSteamID); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage(AISteamFriends: PISteamFriends; pchURL: PAnsiChar; eMode: EActivateGameOverlayToWebPageMode); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ActivateGameOverlayToStore(AISteamFriends: PISteamFriends; nAppID: AppId_t; eFlag: EOverlayToStoreFlag); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_SetPlayedWith(AISteamFriends: PISteamFriends; steamIDUserPlayedWith: CSteamID); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialog(AISteamFriends: PISteamFriends; steamIDLobby: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetSmallFriendAvatar(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetMediumFriendAvatar(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetLargeFriendAvatar(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_RequestUserInformation(AISteamFriends: PISteamFriends; steamIDUser: CSteamID; bRequireNameOnly: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_RequestClanOfficerList(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanOwner(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanOfficerCount(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanOfficerByIndex(AISteamFriends: PISteamFriends; steamIDClan: CSteamID; iOfficer: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetUserRestrictions(AISteamFriends: PISteamFriends): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_SetRichPresence(AISteamFriends: PISteamFriends; pchKey: PAnsiChar; pchValue: PAnsiChar): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ClearRichPresence(AISteamFriends: PISteamFriends); cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendRichPresence(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; pchKey: PAnsiChar): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendRichPresenceKeyCount(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendRichPresenceKeyByIndex(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; iKey: Integer): PAnsiChar; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_RequestFriendRichPresence(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_InviteUserToGame(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; pchConnectString: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetCoplayFriendCount(AISteamFriends: PISteamFriends): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetCoplayFriend(AISteamFriends: PISteamFriends; iCoplayFriend: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendCoplayTime(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendCoplayGame(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID): AppId_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_JoinClanChatRoom(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_LeaveClanChatRoom(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanChatMemberCount(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetChatMemberByIndex(AISteamFriends: PISteamFriends; steamIDClan: CSteamID; iUser: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_SendClanChatMessage(AISteamFriends: PISteamFriends; steamIDClanChat: CSteamID; pchText: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetClanChatMessage(AISteamFriends: PISteamFriends; steamIDClanChat: CSteamID; iMessage: Integer; prgchText: Pointer; cchTextMax: Integer; peChatEntryType: PEChatEntryType; psteamidChatter: PCSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_IsClanChatAdmin(AISteamFriends: PISteamFriends; steamIDClanChat: CSteamID; steamIDUser: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_IsClanChatWindowOpenInSteam(AISteamFriends: PISteamFriends; steamIDClanChat: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_OpenClanChatWindowInSteam(AISteamFriends: PISteamFriends; steamIDClanChat: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_CloseClanChatWindowInSteam(AISteamFriends: PISteamFriends; steamIDClanChat: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_SetListenForFriendsMessages(AISteamFriends: PISteamFriends; bInterceptEnabled: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_ReplyToFriendMessage(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; pchMsgToSend: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFriendMessage(AISteamFriends: PISteamFriends; steamIDFriend: CSteamID; iMessageID: Integer; pvData: Pointer; cubData: Integer; peChatEntryType: PEChatEntryType): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetFollowerCount(AISteamFriends: PISteamFriends; steamID: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_IsFollowing(AISteamFriends: PISteamFriends; steamID: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_EnumerateFollowingList(AISteamFriends: PISteamFriends; unStartIndex: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_IsClanPublic(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_IsClanOfficialGameGroup(AISteamFriends: PISteamFriends; steamIDClan: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_GetNumChatsWithUnreadPriorityMessages(AISteamFriends: PISteamFriends): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ActivateGameOverlayRemotePlayTogetherInviteDialog(AISteamFriends: PISteamFriends; steamIDLobby: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamFriends_RegisterProtocolInOverlayBrowser(AISteamFriends: PISteamFriends; pchProtocol: PAnsiChar): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialogConnectString(AISteamFriends: PISteamFriends; pchConnectString: PAnsiChar); cdecl; external STEAMLIB;

function SteamAPI_ISteamUtils_GetSecondsSinceAppActive(AISteamUtils: PISteamUtils): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetSecondsSinceComputerActive(AISteamUtils: PISteamUtils): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetConnectedUniverse(AISteamUtils: PISteamUtils): EUniverse; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetServerRealTime(AISteamUtils: PISteamUtils): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetIPCountry(AISteamUtils: PISteamUtils): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetImageSize(AISteamUtils: PISteamUtils; iImage: Integer; pnWidth: PUInt32; pnHeight: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetImageRGBA(AISteamUtils: PISteamUtils; iImage: Integer; pubDest: PUInt8; nDestBufferSize: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetCurrentBatteryPower(AISteamUtils: PISteamUtils): UInt8; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetAppID(AISteamUtils: PISteamUtils): UInt32; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUtils_SetOverlayNotificationPosition(AISteamUtils: PISteamUtils; eNotificationPosition: ENotificationPosition); cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_IsAPICallCompleted(AISteamUtils: PISteamUtils; hSteamAPICall: SteamAPICall_t; pbFailed: PBoolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetAPICallFailureReason(AISteamUtils: PISteamUtils; hSteamAPICall: SteamAPICall_t): ESteamAPICallFailure; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetAPICallResult(AISteamUtils: PISteamUtils; hSteamAPICall: SteamAPICall_t; pCallback: Pointer; cubCallback: Integer; iCallbackExpected: Integer; pbFailed: PBoolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetIPCCallCount(AISteamUtils: PISteamUtils): UInt32; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUtils_SetWarningMessageHook(AISteamUtils: PISteamUtils; pFunction: SteamAPIWarningMessageHook_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_IsOverlayEnabled(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_BOverlayNeedsPresent(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_CheckFileSignature(AISteamUtils: PISteamUtils; szFileName: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_ShowGamepadTextInput(AISteamUtils: PISteamUtils; eInputMode: EGamepadTextInputMode; eLineInputMode: EGamepadTextInputLineMode; pchDescription: PAnsiChar; unCharMax: UInt32; pchExistingText: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetEnteredGamepadTextLength(AISteamUtils: PISteamUtils): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetEnteredGamepadTextInput(AISteamUtils: PISteamUtils; pchText: PAnsiChar; cchText: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetSteamUILanguage(AISteamUtils: PISteamUtils): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_IsSteamRunningInVR(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUtils_SetOverlayNotificationInset(AISteamUtils: PISteamUtils; nHorizontalInset: Integer; nVerticalInset: Integer); cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_IsSteamInBigPictureMode(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUtils_StartVRDashboard(AISteamUtils: PISteamUtils); cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_IsVRHeadsetStreamingEnabled(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUtils_SetVRHeadsetStreamingEnabled(AISteamUtils: PISteamUtils; bEnabled: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_IsSteamChinaLauncher(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_InitFilterText(AISteamUtils: PISteamUtils; unFilterOptions: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_FilterText(AISteamUtils: PISteamUtils; eContext: ETextFilteringContext; sourceSteamID: CSteamID; pchInputMessage: PAnsiChar; pchOutFilteredText: PAnsiChar; nByteSizeOutFilteredText: UInt32): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_GetIPv6ConnectivityState(AISteamUtils: PISteamUtils; eProtocol: ESteamIPv6ConnectivityProtocol): ESteamIPv6ConnectivityState; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_ShowFloatingGamepadTextInput(AISteamUtils: PISteamUtils; eKeyboardMode: EFloatingGamepadTextInputMode; nTextFieldXPosition: Integer; nTextFieldYPosition: Integer; nTextFieldWidth: Integer; nTextFieldHeight: Integer): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUtils_SetGameLauncherMode(AISteamUtils: PISteamUtils; bLauncherMode: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamUtils_DismissFloatingGamepadTextInput(AISteamUtils: PISteamUtils): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamMatchmaking_GetFavoriteGameCount(AISteamMatchmaking: PISteamMatchmaking): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetFavoriteGame(AISteamMatchmaking: PISteamMatchmaking; iGame: Integer; pnAppID: PAppId_t; pnIP: PUInt32; pnConnPort: PUInt16; pnQueryPort: PUInt16; punFlags: PUInt32; pRTime32LastPlayedOnServer: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_AddFavoriteGame(AISteamMatchmaking: PISteamMatchmaking; nAppID: AppId_t; nIP: UInt32; nConnPort: UInt16; nQueryPort: UInt16; unFlags: UInt32; rTime32LastPlayedOnServer: UInt32): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_RemoveFavoriteGame(AISteamMatchmaking: PISteamMatchmaking; nAppID: AppId_t; nIP: UInt32; nConnPort: UInt16; nQueryPort: UInt16; unFlags: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_RequestLobbyList(AISteamMatchmaking: PISteamMatchmaking): SteamAPICall_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_AddRequestLobbyListStringFilter(AISteamMatchmaking: PISteamMatchmaking; pchKeyToMatch: PAnsiChar; pchValueToMatch: PAnsiChar; eComparisonType: ELobbyComparison); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_AddRequestLobbyListNumericalFilter(AISteamMatchmaking: PISteamMatchmaking; pchKeyToMatch: PAnsiChar; nValueToMatch: Integer; eComparisonType: ELobbyComparison); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_AddRequestLobbyListNearValueFilter(AISteamMatchmaking: PISteamMatchmaking; pchKeyToMatch: PAnsiChar; nValueToBeCloseTo: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable(AISteamMatchmaking: PISteamMatchmaking; nSlotsAvailable: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_AddRequestLobbyListDistanceFilter(AISteamMatchmaking: PISteamMatchmaking; eLobbyDistanceFilter: ELobbyDistanceFilter); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_AddRequestLobbyListResultCountFilter(AISteamMatchmaking: PISteamMatchmaking; cMaxResults: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyByIndex(AISteamMatchmaking: PISteamMatchmaking; iLobby: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_CreateLobby(AISteamMatchmaking: PISteamMatchmaking; eLobbyType: ELobbyType; cMaxMembers: Integer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_JoinLobby(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_LeaveLobby(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_InviteUserToLobby(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; steamIDInvitee: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetNumLobbyMembers(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyMemberByIndex(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; iMember: Integer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyData(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; pchKey: PAnsiChar): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_SetLobbyData(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; pchKey: PAnsiChar; pchValue: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyDataCount(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyDataByIndex(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; iLobbyData: Integer; pchKey: PAnsiChar; cchKeyBufferSize: Integer; pchValue: PAnsiChar; cchValueBufferSize: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_DeleteLobbyData(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; pchKey: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyMemberData(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; steamIDUser: CSteamID; pchKey: PAnsiChar): PAnsiChar; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_SetLobbyMemberData(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; pchKey: PAnsiChar; pchValue: PAnsiChar); cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_SendLobbyChatMsg(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; pvMsgBody: Pointer; cubMsgBody: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyChatEntry(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; iChatID: Integer; pSteamIDUser: PCSteamID; pvData: Pointer; cubData: Integer; peChatEntryType: PEChatEntryType): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_RequestLobbyData(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmaking_SetLobbyGameServer(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; unGameServerIP: UInt32; unGameServerPort: UInt16; steamIDGameServer: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyGameServer(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; punGameServerIP: PUInt32; punGameServerPort: PUInt16; psteamIDGameServer: PCSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_SetLobbyMemberLimit(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; cMaxMembers: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyMemberLimit(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_SetLobbyType(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; eLobbyType: ELobbyType): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_SetLobbyJoinable(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; bLobbyJoinable: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_GetLobbyOwner(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_SetLobbyOwner(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; steamIDNewOwner: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmaking_SetLinkedLobby(AISteamMatchmaking: PISteamMatchmaking; steamIDLobby: CSteamID; steamIDLobbyDependent: CSteamID): Boolean; cdecl; external STEAMLIB;

procedure SteamAPI_ISteamMatchmakingServerListResponse_ServerResponded(AISteamMatchmakingServerListResponse: PISteamMatchmakingServerListResponse; hRequest: HServerListRequest; iServer: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingServerListResponse_ServerFailedToRespond(AISteamMatchmakingServerListResponse: PISteamMatchmakingServerListResponse; hRequest: HServerListRequest; iServer: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingServerListResponse_RefreshComplete(AISteamMatchmakingServerListResponse: PISteamMatchmakingServerListResponse; hRequest: HServerListRequest; response: EMatchMakingServerResponse); cdecl; external STEAMLIB;

procedure SteamAPI_ISteamMatchmakingPingResponse_ServerResponded(AISteamMatchmakingPingResponse: PISteamMatchmakingPingResponse; server: Pgameserveritem_t); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingPingResponse_ServerFailedToRespond(AISteamMatchmakingPingResponse: PISteamMatchmakingPingResponse); cdecl; external STEAMLIB;

procedure SteamAPI_ISteamMatchmakingPlayersResponse_AddPlayerToList(AISteamMatchmakingPlayersResponse: PISteamMatchmakingPlayersResponse; pchName: PAnsiChar; nScore: Integer; flTimePlayed: Single); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingPlayersResponse_PlayersFailedToRespond(AISteamMatchmakingPlayersResponse: PISteamMatchmakingPlayersResponse); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingPlayersResponse_PlayersRefreshComplete(AISteamMatchmakingPlayersResponse: PISteamMatchmakingPlayersResponse); cdecl; external STEAMLIB;

procedure SteamAPI_ISteamMatchmakingRulesResponse_RulesResponded(AISteamMatchmakingRulesResponse: PISteamMatchmakingRulesResponse; pchRule: PAnsiChar; pchValue: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingRulesResponse_RulesFailedToRespond(AISteamMatchmakingRulesResponse: PISteamMatchmakingRulesResponse); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingRulesResponse_RulesRefreshComplete(AISteamMatchmakingRulesResponse: PISteamMatchmakingRulesResponse); cdecl; external STEAMLIB;

function SteamAPI_ISteamMatchmakingServers_RequestInternetServerList(AISteamMatchmakingServers: PISteamMatchmakingServers; iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_RequestLANServerList(AISteamMatchmakingServers: PISteamMatchmakingServers; iApp: AppId_t; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_RequestFriendsServerList(AISteamMatchmakingServers: PISteamMatchmakingServers; iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_RequestFavoritesServerList(AISteamMatchmakingServers: PISteamMatchmakingServers; iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_RequestHistoryServerList(AISteamMatchmakingServers: PISteamMatchmakingServers; iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_RequestSpectatorServerList(AISteamMatchmakingServers: PISteamMatchmakingServers; iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingServers_ReleaseRequest(AISteamMatchmakingServers: PISteamMatchmakingServers; hServerListRequest: HServerListRequest); cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_GetServerDetails(AISteamMatchmakingServers: PISteamMatchmakingServers; hRequest: HServerListRequest; iServer: Integer): Pgameserveritem_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingServers_CancelQuery(AISteamMatchmakingServers: PISteamMatchmakingServers; hRequest: HServerListRequest); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingServers_RefreshQuery(AISteamMatchmakingServers: PISteamMatchmakingServers; hRequest: HServerListRequest); cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_IsRefreshing(AISteamMatchmakingServers: PISteamMatchmakingServers; hRequest: HServerListRequest): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_GetServerCount(AISteamMatchmakingServers: PISteamMatchmakingServers; hRequest: HServerListRequest): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingServers_RefreshServer(AISteamMatchmakingServers: PISteamMatchmakingServers; hRequest: HServerListRequest; iServer: Integer); cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_PingServer(AISteamMatchmakingServers: PISteamMatchmakingServers; unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingPingResponse): HServerQuery; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_PlayerDetails(AISteamMatchmakingServers: PISteamMatchmakingServers; unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingPlayersResponse): HServerQuery; cdecl; external STEAMLIB;
function SteamAPI_ISteamMatchmakingServers_ServerRules(AISteamMatchmakingServers: PISteamMatchmakingServers; unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingRulesResponse): HServerQuery; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMatchmakingServers_CancelServerQuery(AISteamMatchmakingServers: PISteamMatchmakingServers; hServerQuery: HServerQuery); cdecl; external STEAMLIB;

function SteamAPI_ISteamGameSearch_AddGameSearchParams(AISteamGameSearch: PISteamGameSearch; pchKeyToFind: PAnsiChar; pchValuesToFind: PAnsiChar): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_SearchForGameWithLobby(AISteamGameSearch: PISteamGameSearch; steamIDLobby: CSteamID; nPlayerMin: Integer; nPlayerMax: Integer): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_SearchForGameSolo(AISteamGameSearch: PISteamGameSearch; nPlayerMin: Integer; nPlayerMax: Integer): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_AcceptGame(AISteamGameSearch: PISteamGameSearch): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_DeclineGame(AISteamGameSearch: PISteamGameSearch): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_RetrieveConnectionDetails(AISteamGameSearch: PISteamGameSearch; steamIDHost: CSteamID; pchConnectionDetails: PAnsiChar; cubConnectionDetails: Integer): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_EndGameSearch(AISteamGameSearch: PISteamGameSearch): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_SetGameHostParams(AISteamGameSearch: PISteamGameSearch; pchKey: PAnsiChar; pchValue: PAnsiChar): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_SetConnectionDetails(AISteamGameSearch: PISteamGameSearch; pchConnectionDetails: PAnsiChar; cubConnectionDetails: Integer): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_RequestPlayersForGame(AISteamGameSearch: PISteamGameSearch; nPlayerMin: Integer; nPlayerMax: Integer; nMaxTeamSize: Integer): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_HostConfirmGameStart(AISteamGameSearch: PISteamGameSearch; ullUniqueGameID: UInt64): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_CancelRequestPlayersForGame(AISteamGameSearch: PISteamGameSearch): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_SubmitPlayerResult(AISteamGameSearch: PISteamGameSearch; ullUniqueGameID: UInt64; steamIDPlayer: CSteamID; EPlayerResult: EPlayerResult_t): EGameSearchErrorCode_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameSearch_EndGame(AISteamGameSearch: PISteamGameSearch; ullUniqueGameID: UInt64): EGameSearchErrorCode_t; cdecl; external STEAMLIB;

function SteamAPI_ISteamParties_GetNumActiveBeacons(AISteamParties: PISteamParties): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_GetBeaconByIndex(AISteamParties: PISteamParties; unIndex: UInt32): PartyBeaconID_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_GetBeaconDetails(AISteamParties: PISteamParties; ulBeaconID: PartyBeaconID_t; pSteamIDBeaconOwner: PCSteamID; pLocation: PSteamPartyBeaconLocation_t; pchMetadata: PAnsiChar; cchMetadata: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_JoinParty(AISteamParties: PISteamParties; ulBeaconID: PartyBeaconID_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_GetNumAvailableBeaconLocations(AISteamParties: PISteamParties; puNumLocations: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_GetAvailableBeaconLocations(AISteamParties: PISteamParties; pLocationList: PSteamPartyBeaconLocation_t; uMaxNumLocations: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_CreateBeacon(AISteamParties: PISteamParties; unOpenSlots: UInt32; pBeaconLocation: PSteamPartyBeaconLocation_t; pchConnectString: PAnsiChar; pchMetadata: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamParties_OnReservationCompleted(AISteamParties: PISteamParties; ulBeacon: PartyBeaconID_t; steamIDUser: CSteamID); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamParties_CancelReservation(AISteamParties: PISteamParties; ulBeacon: PartyBeaconID_t; steamIDUser: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_ChangeNumOpenSlots(AISteamParties: PISteamParties; ulBeacon: PartyBeaconID_t; unOpenSlots: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_DestroyBeacon(AISteamParties: PISteamParties; ulBeacon: PartyBeaconID_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParties_GetBeaconLocationData(AISteamParties: PISteamParties; BeaconLocation: SteamPartyBeaconLocation_t; eData: ESteamPartyBeaconLocationData; pchDataStringOut: PAnsiChar; cchDataStringOut: Integer): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamRemoteStorage_FileWrite(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar; pvData: Pointer; cubData: Int32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileRead(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar; pvData: Pointer; cubDataToRead: Int32): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileWriteAsync(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar; pvData: Pointer; cubData: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileReadAsync(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar; nOffset: UInt32; cubToRead: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileReadAsyncComplete(AISteamRemoteStorage: PISteamRemoteStorage; hReadCall: SteamAPICall_t; pvBuffer: Pointer; cubToRead: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileForget(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileDelete(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileShare(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_SetSyncPlatforms(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar; eRemoteStoragePlatform: ERemoteStoragePlatform): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileWriteStreamOpen(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): UGCFileWriteStreamHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileWriteStreamWriteChunk(AISteamRemoteStorage: PISteamRemoteStorage; writeHandle: UGCFileWriteStreamHandle_t; pvData: Pointer; cubData: Int32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileWriteStreamClose(AISteamRemoteStorage: PISteamRemoteStorage; writeHandle: UGCFileWriteStreamHandle_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileWriteStreamCancel(AISteamRemoteStorage: PISteamRemoteStorage; writeHandle: UGCFileWriteStreamHandle_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FileExists(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_FilePersisted(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetFileSize(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetFileTimestamp(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): Int64; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetSyncPlatforms(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar): ERemoteStoragePlatform; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetFileCount(AISteamRemoteStorage: PISteamRemoteStorage): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetFileNameAndSize(AISteamRemoteStorage: PISteamRemoteStorage; iFile: Integer; pnFileSizeInBytes: PInt32): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetQuota(AISteamRemoteStorage: PISteamRemoteStorage; pnTotalBytes: PUInt64; puAvailableBytes: PUInt64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_IsCloudEnabledForAccount(AISteamRemoteStorage: PISteamRemoteStorage): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_IsCloudEnabledForApp(AISteamRemoteStorage: PISteamRemoteStorage): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamRemoteStorage_SetCloudEnabledForApp(AISteamRemoteStorage: PISteamRemoteStorage; bEnabled: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UGCDownload(AISteamRemoteStorage: PISteamRemoteStorage; hContent: UGCHandle_t; unPriority: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetUGCDownloadProgress(AISteamRemoteStorage: PISteamRemoteStorage; hContent: UGCHandle_t; pnBytesDownloaded: PInt32; pnBytesExpected: PInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetUGCDetails(AISteamRemoteStorage: PISteamRemoteStorage; hContent: UGCHandle_t; pnAppID: PAppId_t; ppchName: PPAnsiChar; pnFileSizeInBytes: PInt32; pSteamIDOwner: PCSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UGCRead(AISteamRemoteStorage: PISteamRemoteStorage; hContent: UGCHandle_t; pvData: Pointer; cubDataToRead: Int32; cOffset: UInt32; eAction: EUGCReadAction): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetCachedUGCCount(AISteamRemoteStorage: PISteamRemoteStorage): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetCachedUGCHandle(AISteamRemoteStorage: PISteamRemoteStorage; iCachedContent: Int32): UGCHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_PublishWorkshopFile(AISteamRemoteStorage: PISteamRemoteStorage; pchFile: PAnsiChar; pchPreviewFile: PAnsiChar; nConsumerAppId: AppId_t; pchTitle: PAnsiChar; pchDescription: PAnsiChar; eVisibility: ERemoteStoragePublishedFileVisibility; pTags: PSteamParamStringArray_t; eWorkshopFileType: EWorkshopFileType): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_CreatePublishedFileUpdateRequest(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t): PublishedFileUpdateHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdatePublishedFileFile(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t; pchFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdatePublishedFilePreviewFile(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t; pchPreviewFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTitle(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t; pchTitle: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdatePublishedFileDescription(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t; pchDescription: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdatePublishedFileVisibility(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t; eVisibility: ERemoteStoragePublishedFileVisibility): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTags(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t; pTags: PSteamParamStringArray_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_CommitPublishedFileUpdate(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetPublishedFileDetails(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t; unMaxSecondsOld: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_DeletePublishedFile(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_EnumerateUserPublishedFiles(AISteamRemoteStorage: PISteamRemoteStorage; unStartIndex: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_SubscribePublishedFile(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_EnumerateUserSubscribedFiles(AISteamRemoteStorage: PISteamRemoteStorage; unStartIndex: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UnsubscribePublishedFile(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription(AISteamRemoteStorage: PISteamRemoteStorage; updateHandle: PublishedFileUpdateHandle_t; pchChangeDescription: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetPublishedItemVoteDetails(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UpdateUserPublishedItemVote(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t; bVoteUp: Boolean): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetUserPublishedItemVoteDetails(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles(AISteamRemoteStorage: PISteamRemoteStorage; steamId: CSteamID; unStartIndex: UInt32; pRequiredTags: PSteamParamStringArray_t; pExcludedTags: PSteamParamStringArray_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_PublishVideo(AISteamRemoteStorage: PISteamRemoteStorage; eVideoProvider: EWorkshopVideoProvider; pchVideoAccount: PAnsiChar; pchVideoIdentifier: PAnsiChar; pchPreviewFile: PAnsiChar; nConsumerAppId: AppId_t; pchTitle: PAnsiChar; pchDescription: PAnsiChar; eVisibility: ERemoteStoragePublishedFileVisibility; pTags: PSteamParamStringArray_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_SetUserPublishedFileAction(AISteamRemoteStorage: PISteamRemoteStorage; unPublishedFileId: PublishedFileId_t; eAction: EWorkshopFileAction): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_EnumeratePublishedFilesByUserAction(AISteamRemoteStorage: PISteamRemoteStorage; eAction: EWorkshopFileAction; unStartIndex: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_EnumeratePublishedWorkshopFiles(AISteamRemoteStorage: PISteamRemoteStorage; eEnumerationType: EWorkshopEnumerationType; unStartIndex: UInt32; unCount: UInt32; unDays: UInt32; pTags: PSteamParamStringArray_t; pUserTags: PSteamParamStringArray_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_UGCDownloadToLocation(AISteamRemoteStorage: PISteamRemoteStorage; hContent: UGCHandle_t; pchLocation: PAnsiChar; unPriority: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetLocalFileChangeCount(AISteamRemoteStorage: PISteamRemoteStorage): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_GetLocalFileChange(AISteamRemoteStorage: PISteamRemoteStorage; iFile: Integer; pEChangeType: PERemoteStorageLocalFileChange; pEFilePathType: PERemoteStorageFilePathType): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_BeginFileWriteBatch(AISteamRemoteStorage: PISteamRemoteStorage): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemoteStorage_EndFileWriteBatch(AISteamRemoteStorage: PISteamRemoteStorage): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamUserStats_RequestCurrentStats(AISteamUserStats: PISteamUserStats): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetStatInt32(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pData: PInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetStatFloat(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pData: PSingle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_SetStatInt32(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; nData: Int32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_SetStatFloat(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; fData: Single): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_UpdateAvgRateStat(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; flCountThisSession: Single; dSessionLength: Double): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievement(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pbAchieved: PBoolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_SetAchievement(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_ClearAchievement(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pbAchieved: PBoolean; punUnlockTime: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_StoreStats(AISteamUserStats: PISteamUserStats): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievementIcon(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pchKey: PAnsiChar): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_IndicateAchievementProgress(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; nCurProgress: UInt32; nMaxProgress: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetNumAchievements(AISteamUserStats: PISteamUserStats): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievementName(AISteamUserStats: PISteamUserStats; iAchievement: UInt32): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_RequestUserStats(AISteamUserStats: PISteamUserStats; steamIDUser: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetUserStatInt32(AISteamUserStats: PISteamUserStats; steamIDUser: CSteamID; pchName: PAnsiChar; pData: PInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetUserStatFloat(AISteamUserStats: PISteamUserStats; steamIDUser: CSteamID; pchName: PAnsiChar; pData: PSingle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetUserAchievement(AISteamUserStats: PISteamUserStats; steamIDUser: CSteamID; pchName: PAnsiChar; pbAchieved: PBoolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetUserAchievementAndUnlockTime(AISteamUserStats: PISteamUserStats; steamIDUser: CSteamID; pchName: PAnsiChar; pbAchieved: PBoolean; punUnlockTime: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_ResetAllStats(AISteamUserStats: PISteamUserStats; bAchievementsToo: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_FindOrCreateLeaderboard(AISteamUserStats: PISteamUserStats; pchLeaderboardName: PAnsiChar; eLeaderboardSortMethod: ELeaderboardSortMethod; eLeaderboardDisplayType: ELeaderboardDisplayType): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_FindLeaderboard(AISteamUserStats: PISteamUserStats; pchLeaderboardName: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetLeaderboardName(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetLeaderboardEntryCount(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetLeaderboardSortMethod(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t): ELeaderboardSortMethod; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetLeaderboardDisplayType(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t): ELeaderboardDisplayType; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_DownloadLeaderboardEntries(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t; eLeaderboardDataRequest: ELeaderboardDataRequest; nRangeStart: Integer; nRangeEnd: Integer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_DownloadLeaderboardEntriesForUsers(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t; prgUsers: PCSteamID; cUsers: Integer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetDownloadedLeaderboardEntry(AISteamUserStats: PISteamUserStats; hSteamLeaderboardEntries: SteamLeaderboardEntries_t; index: Integer; pLeaderboardEntry: PLeaderboardEntry_t; pDetails: PInt32; cDetailsMax: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_UploadLeaderboardScore(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t; eLeaderboardUploadScoreMethod: ELeaderboardUploadScoreMethod; nScore: Int32; pScoreDetails: PInt32; cScoreDetailsCount: Integer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_AttachLeaderboardUGC(AISteamUserStats: PISteamUserStats; hSteamLeaderboard: SteamLeaderboard_t; hUGC: UGCHandle_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetNumberOfCurrentPlayers(AISteamUserStats: PISteamUserStats): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_RequestGlobalAchievementPercentages(AISteamUserStats: PISteamUserStats): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetMostAchievedAchievementInfo(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; unNameBufLen: UInt32; pflPercent: PSingle; pbAchieved: PBoolean): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetNextMostAchievedAchievementInfo(AISteamUserStats: PISteamUserStats; iIteratorPrevious: Integer; pchName: PAnsiChar; unNameBufLen: UInt32; pflPercent: PSingle; pbAchieved: PBoolean): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievementAchievedPercent(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pflPercent: PSingle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_RequestGlobalStats(AISteamUserStats: PISteamUserStats; nHistoryDays: Integer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetGlobalStatInt64(AISteamUserStats: PISteamUserStats; pchStatName: PAnsiChar; pData: PInt64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetGlobalStatDouble(AISteamUserStats: PISteamUserStats; pchStatName: PAnsiChar; pData: PDouble): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetGlobalStatHistoryInt64(AISteamUserStats: PISteamUserStats; pchStatName: PAnsiChar; pData: PInt64; cubData: UInt32): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetGlobalStatHistoryDouble(AISteamUserStats: PISteamUserStats; pchStatName: PAnsiChar; pData: PDouble; cubData: UInt32): Int32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pnMinProgress: PInt32; pnMaxProgress: PInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat(AISteamUserStats: PISteamUserStats; pchName: PAnsiChar; pfMinProgress: PSingle; pfMaxProgress: PSingle): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamApps_BIsSubscribed(AISteamApps: PISteamApps): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsLowViolence(AISteamApps: PISteamApps): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsCybercafe(AISteamApps: PISteamApps): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsVACBanned(AISteamApps: PISteamApps): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetCurrentGameLanguage(AISteamApps: PISteamApps): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetAvailableGameLanguages(AISteamApps: PISteamApps): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsSubscribedApp(AISteamApps: PISteamApps; appID: AppId_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsDlcInstalled(AISteamApps: PISteamApps; appID: AppId_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetEarliestPurchaseUnixTime(AISteamApps: PISteamApps; nAppID: AppId_t): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsSubscribedFromFreeWeekend(AISteamApps: PISteamApps): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetDLCCount(AISteamApps: PISteamApps): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BGetDLCDataByIndex(AISteamApps: PISteamApps; iDLC: Integer; pAppID: PAppId_t; pbAvailable: PBoolean; pchName: PAnsiChar; cchNameBufferSize: Integer): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamApps_InstallDLC(AISteamApps: PISteamApps; nAppID: AppId_t); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamApps_UninstallDLC(AISteamApps: PISteamApps; nAppID: AppId_t); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamApps_RequestAppProofOfPurchaseKey(AISteamApps: PISteamApps; nAppID: AppId_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetCurrentBetaName(AISteamApps: PISteamApps; pchName: PAnsiChar; cchNameBufferSize: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_MarkContentCorrupt(AISteamApps: PISteamApps; bMissingFilesOnly: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetInstalledDepots(AISteamApps: PISteamApps; appID: AppId_t; pvecDepots: PDepotId_t; cMaxDepots: UInt32): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetAppInstallDir(AISteamApps: PISteamApps; appID: AppId_t; pchFolder: PAnsiChar; cchFolderBufferSize: UInt32): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsAppInstalled(AISteamApps: PISteamApps; appID: AppId_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetAppOwner(AISteamApps: PISteamApps): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetLaunchQueryParam(AISteamApps: PISteamApps; pchKey: PAnsiChar): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetDlcDownloadProgress(AISteamApps: PISteamApps; nAppID: AppId_t; punBytesDownloaded: PUInt64; punBytesTotal: PUInt64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetAppBuildId(AISteamApps: PISteamApps): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamApps_RequestAllProofOfPurchaseKeys(AISteamApps: PISteamApps); cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetFileDetails(AISteamApps: PISteamApps; pszFileName: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_GetLaunchCommandLine(AISteamApps: PISteamApps; pszCommandLine: PAnsiChar; cubCommandLine: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsSubscribedFromFamilySharing(AISteamApps: PISteamApps): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamApps_BIsTimedTrial(AISteamApps: PISteamApps; punSecondsAllowed: PUInt32; punSecondsPlayed: PUInt32): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamNetworking_SendP2PPacket(AISteamNetworking: PISteamNetworking; steamIDRemote: CSteamID; pubData: Pointer; cubData: UInt32; eP2PSendType: EP2PSend; nChannel: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_IsP2PPacketAvailable(AISteamNetworking: PISteamNetworking; pcubMsgSize: PUInt32; nChannel: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_ReadP2PPacket(AISteamNetworking: PISteamNetworking; pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32; psteamIDRemote: PCSteamID; nChannel: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_AcceptP2PSessionWithUser(AISteamNetworking: PISteamNetworking; steamIDRemote: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_CloseP2PSessionWithUser(AISteamNetworking: PISteamNetworking; steamIDRemote: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_CloseP2PChannelWithUser(AISteamNetworking: PISteamNetworking; steamIDRemote: CSteamID; nChannel: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_GetP2PSessionState(AISteamNetworking: PISteamNetworking; steamIDRemote: CSteamID; pConnectionState: PP2PSessionState_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_AllowP2PPacketRelay(AISteamNetworking: PISteamNetworking; bAllow: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_CreateListenSocket(AISteamNetworking: PISteamNetworking; nVirtualP2PPort: Integer; nIP: SteamIPAddress_t; nPort: UInt16; bAllowUseOfPacketRelay: Boolean): SNetListenSocket_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_CreateP2PConnectionSocket(AISteamNetworking: PISteamNetworking; steamIDTarget: CSteamID; nVirtualPort: Integer; nTimeoutSec: Integer; bAllowUseOfPacketRelay: Boolean): SNetSocket_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_CreateConnectionSocket(AISteamNetworking: PISteamNetworking; nIP: SteamIPAddress_t; nPort: UInt16; nTimeoutSec: Integer): SNetSocket_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_DestroySocket(AISteamNetworking: PISteamNetworking; hSocket: SNetSocket_t; bNotifyRemoteEnd: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_DestroyListenSocket(AISteamNetworking: PISteamNetworking; hSocket: SNetListenSocket_t; bNotifyRemoteEnd: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_SendDataOnSocket(AISteamNetworking: PISteamNetworking; hSocket: SNetSocket_t; pubData: Pointer; cubData: UInt32; bReliable: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_IsDataAvailableOnSocket(AISteamNetworking: PISteamNetworking; hSocket: SNetSocket_t; pcubMsgSize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_RetrieveDataFromSocket(AISteamNetworking: PISteamNetworking; hSocket: SNetSocket_t; pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_IsDataAvailable(AISteamNetworking: PISteamNetworking; hListenSocket: SNetListenSocket_t; pcubMsgSize: PUInt32; phSocket: PSNetSocket_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_RetrieveData(AISteamNetworking: PISteamNetworking; hListenSocket: SNetListenSocket_t; pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32; phSocket: PSNetSocket_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_GetSocketInfo(AISteamNetworking: PISteamNetworking; hSocket: SNetSocket_t; pSteamIDRemote: PCSteamID; peSocketStatus: PInteger; punIPRemote: PSteamIPAddress_t; punPortRemote: PUInt16): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_GetListenSocketInfo(AISteamNetworking: PISteamNetworking; hListenSocket: SNetListenSocket_t; pnIP: PSteamIPAddress_t; pnPort: PUInt16): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_GetSocketConnectionType(AISteamNetworking: PISteamNetworking; hSocket: SNetSocket_t): ESNetSocketConnectionType; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworking_GetMaxPacketSize(AISteamNetworking: PISteamNetworking; hSocket: SNetSocket_t): Integer; cdecl; external STEAMLIB;

function SteamAPI_ISteamScreenshots_WriteScreenshot(AISteamScreenshots: PISteamScreenshots; pubRGB: Pointer; cubRGB: UInt32; nWidth: Integer; nHeight: Integer): ScreenshotHandle; cdecl; external STEAMLIB;
function SteamAPI_ISteamScreenshots_AddScreenshotToLibrary(AISteamScreenshots: PISteamScreenshots; pchFilename: PAnsiChar; pchThumbnailFilename: PAnsiChar; nWidth: Integer; nHeight: Integer): ScreenshotHandle; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamScreenshots_TriggerScreenshot(AISteamScreenshots: PISteamScreenshots); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamScreenshots_HookScreenshots(AISteamScreenshots: PISteamScreenshots; bHook: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamScreenshots_SetLocation(AISteamScreenshots: PISteamScreenshots; hScreenshot: ScreenshotHandle; pchLocation: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamScreenshots_TagUser(AISteamScreenshots: PISteamScreenshots; hScreenshot: ScreenshotHandle; steamID: CSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamScreenshots_TagPublishedFile(AISteamScreenshots: PISteamScreenshots; hScreenshot: ScreenshotHandle; unPublishedFileID: PublishedFileId_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamScreenshots_IsScreenshotsHooked(AISteamScreenshots: PISteamScreenshots): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamScreenshots_AddVRScreenshotToLibrary(AISteamScreenshots: PISteamScreenshots; eType: EVRScreenshotType; pchFilename: PAnsiChar; pchVRFilename: PAnsiChar): ScreenshotHandle; cdecl; external STEAMLIB;

function SteamAPI_ISteamMusic_BIsEnabled(AISteamMusic: PISteamMusic): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusic_BIsPlaying(AISteamMusic: PISteamMusic): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusic_GetPlaybackStatus(AISteamMusic: PISteamMusic): AudioPlayback_Status; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMusic_Play(AISteamMusic: PISteamMusic); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMusic_Pause(AISteamMusic: PISteamMusic); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMusic_PlayPrevious(AISteamMusic: PISteamMusic); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMusic_PlayNext(AISteamMusic: PISteamMusic); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamMusic_SetVolume(AISteamMusic: PISteamMusic; flVolume: Single); cdecl; external STEAMLIB;
function SteamAPI_ISteamMusic_GetVolume(AISteamMusic: PISteamMusic): Single; cdecl; external STEAMLIB;

function SteamAPI_ISteamMusicRemote_RegisterSteamMusicRemote(AISteamMusicRemote: PISteamMusicRemote; pchName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_DeregisterSteamMusicRemote(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_BIsCurrentMusicRemote(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_BActivationSuccess(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_SetDisplayName(AISteamMusicRemote: PISteamMusicRemote; pchDisplayName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_SetPNGIcon_64x64(AISteamMusicRemote: PISteamMusicRemote; pvBuffer: Pointer; cbBufferLength: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_EnablePlayPrevious(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_EnablePlayNext(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_EnableShuffled(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_EnableLooped(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_EnableQueue(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_EnablePlaylists(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_UpdatePlaybackStatus(AISteamMusicRemote: PISteamMusicRemote; nStatus: AudioPlayback_Status): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_UpdateShuffled(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_UpdateLooped(AISteamMusicRemote: PISteamMusicRemote; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_UpdateVolume(AISteamMusicRemote: PISteamMusicRemote; flValue: Single): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_CurrentEntryWillChange(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_CurrentEntryIsAvailable(AISteamMusicRemote: PISteamMusicRemote; bAvailable: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_UpdateCurrentEntryText(AISteamMusicRemote: PISteamMusicRemote; pchText: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_UpdateCurrentEntryElapsedSeconds(AISteamMusicRemote: PISteamMusicRemote; nValue: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_UpdateCurrentEntryCoverArt(AISteamMusicRemote: PISteamMusicRemote; pvBuffer: Pointer; cbBufferLength: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_CurrentEntryDidChange(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_QueueWillChange(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_ResetQueueEntries(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_SetQueueEntry(AISteamMusicRemote: PISteamMusicRemote; nID: Integer; nPosition: Integer; pchEntryText: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_SetCurrentQueueEntry(AISteamMusicRemote: PISteamMusicRemote; nID: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_QueueDidChange(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_PlaylistWillChange(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_ResetPlaylistEntries(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_SetPlaylistEntry(AISteamMusicRemote: PISteamMusicRemote; nID: Integer; nPosition: Integer; pchEntryText: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_SetCurrentPlaylistEntry(AISteamMusicRemote: PISteamMusicRemote; nID: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamMusicRemote_PlaylistDidChange(AISteamMusicRemote: PISteamMusicRemote): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamHTTP_CreateHTTPRequest(AISteamHTTP: PISteamHTTP; eHTTPRequestMethod: EHTTPMethod; pchAbsoluteURL: PAnsiChar): HTTPRequestHandle; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestContextValue(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; ulContextValue: UInt64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestNetworkActivityTimeout(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; unTimeoutSeconds: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestHeaderValue(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pchHeaderName: PAnsiChar; pchHeaderValue: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestGetOrPostParameter(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pchParamName: PAnsiChar; pchParamValue: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SendHTTPRequest(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pCallHandle: PSteamAPICall_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SendHTTPRequestAndStreamResponse(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pCallHandle: PSteamAPICall_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_DeferHTTPRequest(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_PrioritizeHTTPRequest(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_GetHTTPResponseHeaderSize(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pchHeaderName: PAnsiChar; unResponseHeaderSize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_GetHTTPResponseHeaderValue(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pchHeaderName: PAnsiChar; pHeaderValueBuffer: PUInt8; unBufferSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_GetHTTPResponseBodySize(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; unBodySize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_GetHTTPResponseBodyData(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pBodyDataBuffer: PUInt8; unBufferSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_GetHTTPStreamingResponseBodyData(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; cOffset: UInt32; pBodyDataBuffer: PUInt8; unBufferSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_ReleaseHTTPRequest(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_GetHTTPDownloadProgressPct(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pflPercentOut: PSingle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestRawPostBody(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pchContentType: PAnsiChar; pubBody: PUInt8; unBodyLen: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_CreateCookieContainer(AISteamHTTP: PISteamHTTP; bAllowResponsesToModify: Boolean): HTTPCookieContainerHandle; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_ReleaseCookieContainer(AISteamHTTP: PISteamHTTP; hCookieContainer: HTTPCookieContainerHandle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetCookie(AISteamHTTP: PISteamHTTP; hCookieContainer: HTTPCookieContainerHandle; pchHost: PAnsiChar; pchUrl: PAnsiChar; pchCookie: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestCookieContainer(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; hCookieContainer: HTTPCookieContainerHandle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestUserAgentInfo(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pchUserAgentInfo: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestRequiresVerifiedCertificate(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; bRequireVerifiedCertificate: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_SetHTTPRequestAbsoluteTimeoutMS(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; unMilliseconds: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTTP_GetHTTPRequestWasTimedOut(AISteamHTTP: PISteamHTTP; hRequest: HTTPRequestHandle; pbWasTimedOut: PBoolean): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamInput_Init(AISteamInput: PISteamInput; bExplicitlyCallRunFrame: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_Shutdown(AISteamInput: PISteamInput): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_SetInputActionManifestFilePath(AISteamInput: PISteamInput; pchInputActionManifestAbsolutePath: PAnsiChar): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_RunFrame(AISteamInput: PISteamInput; bReservedValue: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_BWaitForData(AISteamInput: PISteamInput; bWaitForever: Boolean; unTimeout: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_BNewDataAvailable(AISteamInput: PISteamInput): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetConnectedControllers(AISteamInput: PISteamInput; handlesOut: PInputHandle_t): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_EnableDeviceCallbacks(AISteamInput: PISteamInput); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_EnableActionEventCallbacks(AISteamInput: PISteamInput; pCallback: SteamInputActionEventCallbackPointer); cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetActionSetHandle(AISteamInput: PISteamInput; pszActionSetName: PAnsiChar): InputActionSetHandle_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_ActivateActionSet(AISteamInput: PISteamInput; inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetCurrentActionSet(AISteamInput: PISteamInput; inputHandle: InputHandle_t): InputActionSetHandle_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_ActivateActionSetLayer(AISteamInput: PISteamInput; inputHandle: InputHandle_t; actionSetLayerHandle: InputActionSetHandle_t); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_DeactivateActionSetLayer(AISteamInput: PISteamInput; inputHandle: InputHandle_t; actionSetLayerHandle: InputActionSetHandle_t); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_DeactivateAllActionSetLayers(AISteamInput: PISteamInput; inputHandle: InputHandle_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetActiveActionSetLayers(AISteamInput: PISteamInput; inputHandle: InputHandle_t; handlesOut: PInputActionSetHandle_t): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetDigitalActionHandle(AISteamInput: PISteamInput; pszActionName: PAnsiChar): InputDigitalActionHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetDigitalActionData(AISteamInput: PISteamInput; inputHandle: InputHandle_t; digitalActionHandle: InputDigitalActionHandle_t): InputDigitalActionData_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetDigitalActionOrigins(AISteamInput: PISteamInput; inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t; digitalActionHandle: InputDigitalActionHandle_t; originsOut: PEInputActionOrigin): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetStringForDigitalActionName(AISteamInput: PISteamInput; eActionHandle: InputDigitalActionHandle_t): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetAnalogActionHandle(AISteamInput: PISteamInput; pszActionName: PAnsiChar): InputAnalogActionHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetAnalogActionData(AISteamInput: PISteamInput; inputHandle: InputHandle_t; analogActionHandle: InputAnalogActionHandle_t): InputAnalogActionData_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetAnalogActionOrigins(AISteamInput: PISteamInput; inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t; analogActionHandle: InputAnalogActionHandle_t; originsOut: PEInputActionOrigin): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin(AISteamInput: PISteamInput; eOrigin: EInputActionOrigin; eSize: ESteamInputGlyphSize; unFlags: UInt32): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin(AISteamInput: PISteamInput; eOrigin: EInputActionOrigin; unFlags: UInt32): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy(AISteamInput: PISteamInput; eOrigin: EInputActionOrigin): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetStringForActionOrigin(AISteamInput: PISteamInput; eOrigin: EInputActionOrigin): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetStringForAnalogActionName(AISteamInput: PISteamInput; eActionHandle: InputAnalogActionHandle_t): PAnsiChar; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_StopAnalogActionMomentum(AISteamInput: PISteamInput; inputHandle: InputHandle_t; eAction: InputAnalogActionHandle_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetMotionData(AISteamInput: PISteamInput; inputHandle: InputHandle_t): InputMotionData_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_TriggerVibration(AISteamInput: PISteamInput; inputHandle: InputHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_TriggerVibrationExtended(AISteamInput: PISteamInput; inputHandle: InputHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16; usLeftTriggerSpeed: UInt16; usRightTriggerSpeed: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_TriggerSimpleHapticEvent(AISteamInput: PISteamInput; inputHandle: InputHandle_t; eHapticLocation: EControllerHapticLocation; nIntensity: UInt8; nGainDB: AnsiChar; nOtherIntensity: UInt8; nOtherGainDB: AnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_SetLEDColor(AISteamInput: PISteamInput; inputHandle: InputHandle_t; nColorR: UInt8; nColorG: UInt8; nColorB: UInt8; nFlags: UInt32); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_Legacy_TriggerHapticPulse(AISteamInput: PISteamInput; inputHandle: InputHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse(AISteamInput: PISteamInput; inputHandle: InputHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16; usOffMicroSec: UInt16; unRepeat: UInt16; nFlags: UInt32); cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_ShowBindingPanel(AISteamInput: PISteamInput; inputHandle: InputHandle_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetInputTypeForHandle(AISteamInput: PISteamInput; inputHandle: InputHandle_t): ESteamInputType; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetControllerForGamepadIndex(AISteamInput: PISteamInput; nIndex: Integer): InputHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetGamepadIndexForController(AISteamInput: PISteamInput; ulinputHandle: InputHandle_t): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetStringForXboxOrigin(AISteamInput: PISteamInput; eOrigin: EXboxOrigin): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetGlyphForXboxOrigin(AISteamInput: PISteamInput; eOrigin: EXboxOrigin): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin(AISteamInput: PISteamInput; inputHandle: InputHandle_t; eOrigin: EXboxOrigin): EInputActionOrigin; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_TranslateActionOrigin(AISteamInput: PISteamInput; eDestinationInputType: ESteamInputType; eSourceOrigin: EInputActionOrigin): EInputActionOrigin; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetDeviceBindingRevision(AISteamInput: PISteamInput; inputHandle: InputHandle_t; pMajor: PInteger; pMinor: PInteger): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetRemotePlaySessionID(AISteamInput: PISteamInput; inputHandle: InputHandle_t): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamInput_GetSessionInputConfigurationSettings(AISteamInput: PISteamInput): UInt16; cdecl; external STEAMLIB;

function SteamAPI_ISteamController_Init(AISteamController: PISteamController): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_Shutdown(AISteamController: PISteamController): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_RunFrame(AISteamController: PISteamController); cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetConnectedControllers(AISteamController: PISteamController; handlesOut: PControllerHandle_t): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetActionSetHandle(AISteamController: PISteamController; pszActionSetName: PAnsiChar): ControllerActionSetHandle_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_ActivateActionSet(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetCurrentActionSet(AISteamController: PISteamController; controllerHandle: ControllerHandle_t): ControllerActionSetHandle_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_ActivateActionSetLayer(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; actionSetLayerHandle: ControllerActionSetHandle_t); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_DeactivateActionSetLayer(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; actionSetLayerHandle: ControllerActionSetHandle_t); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_DeactivateAllActionSetLayers(AISteamController: PISteamController; controllerHandle: ControllerHandle_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetActiveActionSetLayers(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; handlesOut: PControllerActionSetHandle_t): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetDigitalActionHandle(AISteamController: PISteamController; pszActionName: PAnsiChar): ControllerDigitalActionHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetDigitalActionData(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; digitalActionHandle: ControllerDigitalActionHandle_t): InputDigitalActionData_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetDigitalActionOrigins(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t; digitalActionHandle: ControllerDigitalActionHandle_t; originsOut: PEControllerActionOrigin): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetAnalogActionHandle(AISteamController: PISteamController; pszActionName: PAnsiChar): ControllerAnalogActionHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetAnalogActionData(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; analogActionHandle: ControllerAnalogActionHandle_t): InputAnalogActionData_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetAnalogActionOrigins(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t; analogActionHandle: ControllerAnalogActionHandle_t; originsOut: PEControllerActionOrigin): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetGlyphForActionOrigin(AISteamController: PISteamController; eOrigin: EControllerActionOrigin): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetStringForActionOrigin(AISteamController: PISteamController; eOrigin: EControllerActionOrigin): PAnsiChar; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_StopAnalogActionMomentum(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; eAction: ControllerAnalogActionHandle_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetMotionData(AISteamController: PISteamController; controllerHandle: ControllerHandle_t): InputMotionData_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_TriggerHapticPulse(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_TriggerRepeatedHapticPulse(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16; usOffMicroSec: UInt16; unRepeat: UInt16; nFlags: UInt32); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_TriggerVibration(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamController_SetLEDColor(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; nColorR: UInt8; nColorG: UInt8; nColorB: UInt8; nFlags: UInt32); cdecl; external STEAMLIB;
function SteamAPI_ISteamController_ShowBindingPanel(AISteamController: PISteamController; controllerHandle: ControllerHandle_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetInputTypeForHandle(AISteamController: PISteamController; controllerHandle: ControllerHandle_t): ESteamInputType; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetControllerForGamepadIndex(AISteamController: PISteamController; nIndex: Integer): ControllerHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetGamepadIndexForController(AISteamController: PISteamController; ulControllerHandle: ControllerHandle_t): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetStringForXboxOrigin(AISteamController: PISteamController; eOrigin: EXboxOrigin): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetGlyphForXboxOrigin(AISteamController: PISteamController; eOrigin: EXboxOrigin): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetActionOriginFromXboxOrigin(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; eOrigin: EXboxOrigin): EControllerActionOrigin; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_TranslateActionOrigin(AISteamController: PISteamController; eDestinationInputType: ESteamInputType; eSourceOrigin: EControllerActionOrigin): EControllerActionOrigin; cdecl; external STEAMLIB;
function SteamAPI_ISteamController_GetControllerBindingRevision(AISteamController: PISteamController; controllerHandle: ControllerHandle_t; pMajor: PInteger; pMinor: PInteger): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamUGC_CreateQueryUserUGCRequest(AISteamUGC: PISteamUGC; unAccountID: AccountID_t; eListType: EUserUGCList; eMatchingUGCType: EUGCMatchingUGCType; eSortOrder: EUserUGCListSortOrder; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; unPage: UInt32): UGCQueryHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_CreateQueryAllUGCRequestPage(AISteamUGC: PISteamUGC; eQueryType: EUGCQuery; eMatchingeMatchingUGCTypeFileType: EUGCMatchingUGCType; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; unPage: UInt32): UGCQueryHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_CreateQueryAllUGCRequestCursor(AISteamUGC: PISteamUGC; eQueryType: EUGCQuery; eMatchingeMatchingUGCTypeFileType: EUGCMatchingUGCType; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; pchCursor: PAnsiChar): UGCQueryHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_CreateQueryUGCDetailsRequest(AISteamUGC: PISteamUGC; pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): UGCQueryHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SendQueryUGCRequest(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCResult(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; pDetails: PSteamUGCDetails_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCNumTags(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCTag(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; indexTag: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCTagDisplayName(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; indexTag: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCPreviewURL(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; pchURL: PAnsiChar; cchURLSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCMetadata(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; pchMetadata: PAnsiChar; cchMetadatasize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCChildren(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; pvecPublishedFileID: PPublishedFileId_t; cMaxEntries: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCStatistic(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; eStatType: EItemStatistic; pStatValue: PUInt64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCNumAdditionalPreviews(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCAdditionalPreview(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; previewIndex: UInt32; pchURLOrVideoID: PAnsiChar; cchURLSize: UInt32; pchOriginalFileName: PAnsiChar; cchOriginalFileNameSize: UInt32; pPreviewType: PEItemPreviewType): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCNumKeyValueTags(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryUGCKeyValueTag(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; keyValueTagIndex: UInt32; pchKey: PAnsiChar; cchKeySize: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetQueryFirstUGCKeyValueTag(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; index: UInt32; pchKey: PAnsiChar; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_ReleaseQueryUGCRequest(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddRequiredTag(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; pTagName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddRequiredTagGroup(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; pTagGroups: PSteamParamStringArray_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddExcludedTag(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; pTagName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnOnlyIDs(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bReturnOnlyIDs: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnKeyValueTags(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bReturnKeyValueTags: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnLongDescription(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bReturnLongDescription: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnMetadata(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bReturnMetadata: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnChildren(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bReturnChildren: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnAdditionalPreviews(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bReturnAdditionalPreviews: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnTotalOnly(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bReturnTotalOnly: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetReturnPlaytimeStats(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; unDays: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetLanguage(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; pchLanguage: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetAllowCachedResponse(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; unMaxAgeSeconds: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetCloudFileNameFilter(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; pMatchCloudFileName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetMatchAnyTag(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; bMatchAnyTag: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetSearchText(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; pSearchText: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetRankedByTrendDays(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; unDays: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetTimeCreatedDateRange(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; rtStart: RTime32; rtEnd: RTime32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetTimeUpdatedDateRange(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; rtStart: RTime32; rtEnd: RTime32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddRequiredKeyValueTag(AISteamUGC: PISteamUGC; handle: UGCQueryHandle_t; pKey: PAnsiChar; pValue: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_RequestUGCDetails(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t; unMaxAgeSeconds: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_CreateItem(AISteamUGC: PISteamUGC; nConsumerAppId: AppId_t; eFileType: EWorkshopFileType): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_StartItemUpdate(AISteamUGC: PISteamUGC; nConsumerAppId: AppId_t; nPublishedFileID: PublishedFileId_t): UGCUpdateHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemTitle(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pchTitle: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemDescription(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pchDescription: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemUpdateLanguage(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pchLanguage: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemMetadata(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pchMetaData: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemVisibility(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; eVisibility: ERemoteStoragePublishedFileVisibility): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemTags(AISteamUGC: PISteamUGC; updateHandle: UGCUpdateHandle_t; pTags: PSteamParamStringArray_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemContent(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pszContentFolder: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetItemPreview(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pszPreviewFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetAllowLegacyUpload(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; bAllowLegacyUpload: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_RemoveAllItemKeyValueTags(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_RemoveItemKeyValueTags(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pchKey: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddItemKeyValueTag(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pchKey: PAnsiChar; pchValue: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddItemPreviewFile(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pszPreviewFile: PAnsiChar; aType: EItemPreviewType): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddItemPreviewVideo(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pszVideoID: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_UpdateItemPreviewFile(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; index: UInt32; pszPreviewFile: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_UpdateItemPreviewVideo(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; index: UInt32; pszVideoID: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_RemoveItemPreview(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; index: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SubmitItemUpdate(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; pchChangeNote: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetItemUpdateProgress(AISteamUGC: PISteamUGC; handle: UGCUpdateHandle_t; punBytesProcessed: PUInt64; punBytesTotal: PUInt64): EItemUpdateStatus; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SetUserItemVote(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t; bVoteUp: Boolean): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetUserItemVote(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddItemToFavorites(AISteamUGC: PISteamUGC; nAppId: AppId_t; nPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_RemoveItemFromFavorites(AISteamUGC: PISteamUGC; nAppId: AppId_t; nPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_SubscribeItem(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_UnsubscribeItem(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetNumSubscribedItems(AISteamUGC: PISteamUGC): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetSubscribedItems(AISteamUGC: PISteamUGC; pvecPublishedFileID: PPublishedFileId_t; cMaxEntries: UInt32): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetItemState(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetItemInstallInfo(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t; punSizeOnDisk: PUInt64; pchFolder: PAnsiChar; cchFolderSize: UInt32; punTimeStamp: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetItemDownloadInfo(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t; punBytesDownloaded: PUInt64; punBytesTotal: PUInt64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_DownloadItem(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t; bHighPriority: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_BInitWorkshopForGameServer(AISteamUGC: PISteamUGC; unWorkshopDepotID: DepotId_t; pszFolder: PAnsiChar): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamUGC_SuspendDownloads(AISteamUGC: PISteamUGC; bSuspend: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_StartPlaytimeTracking(AISteamUGC: PISteamUGC; pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_StopPlaytimeTracking(AISteamUGC: PISteamUGC; pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_StopPlaytimeTrackingForAllItems(AISteamUGC: PISteamUGC): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddDependency(AISteamUGC: PISteamUGC; nParentPublishedFileID: PublishedFileId_t; nChildPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_RemoveDependency(AISteamUGC: PISteamUGC; nParentPublishedFileID: PublishedFileId_t; nChildPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_AddAppDependency(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t; nAppID: AppId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_RemoveAppDependency(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t; nAppID: AppId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetAppDependencies(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_DeleteItem(AISteamUGC: PISteamUGC; nPublishedFileID: PublishedFileId_t): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_ShowWorkshopEULA(AISteamUGC: PISteamUGC): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamUGC_GetWorkshopEULAStatus(AISteamUGC: PISteamUGC): SteamAPICall_t; cdecl; external STEAMLIB;

function SteamAPI_ISteamAppList_GetNumInstalledApps(AISteamAppList: PISteamAppList): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamAppList_GetInstalledApps(AISteamAppList: PISteamAppList; pvecAppID: PAppId_t; unMaxAppIDs: UInt32): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamAppList_GetAppName(AISteamAppList: PISteamAppList; nAppID: AppId_t; pchName: PAnsiChar; cchNameMax: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamAppList_GetAppInstallDir(AISteamAppList: PISteamAppList; nAppID: AppId_t; pchDirectory: PAnsiChar; cchNameMax: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamAppList_GetAppBuildId(AISteamAppList: PISteamAppList; nAppID: AppId_t): Integer; cdecl; external STEAMLIB;

function SteamAPI_ISteamHTMLSurface_Init(AISteamHTMLSurface: PISteamHTMLSurface): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTMLSurface_Shutdown(AISteamHTMLSurface: PISteamHTMLSurface): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamHTMLSurface_CreateBrowser(AISteamHTMLSurface: PISteamHTMLSurface; pchUserAgent: PAnsiChar; pchUserCSS: PAnsiChar): SteamAPICall_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_RemoveBrowser(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_LoadURL(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; pchURL: PAnsiChar; pchPostData: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetSize(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; unWidth: UInt32; unHeight: UInt32); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_StopLoad(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_Reload(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_GoBack(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_GoForward(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_AddHeader(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; pchKey: PAnsiChar; pchValue: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_ExecuteJavascript(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; pchScript: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_MouseUp(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_MouseDown(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_MouseDoubleClick(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_MouseMove(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; x: Integer; y: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_MouseWheel(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; nDelta: Int32); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_KeyDown(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; nNativeKeyCode: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers; bIsSystemKey: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_KeyUp(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; nNativeKeyCode: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_KeyChar(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; cUnicodeChar: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetHorizontalScroll(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; nAbsolutePixelScroll: UInt32); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetVerticalScroll(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; nAbsolutePixelScroll: UInt32); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetKeyFocus(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; bHasKeyFocus: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_ViewSource(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_CopyToClipboard(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_PasteFromClipboard(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_Find(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; pchSearchStr: PAnsiChar; bCurrentlyInFind: Boolean; bReverse: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_StopFind(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_GetLinkAtPosition(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; x: Integer; y: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetCookie(AISteamHTMLSurface: PISteamHTMLSurface; pchHostname: PAnsiChar; pchKey: PAnsiChar; pchValue: PAnsiChar; pchPath: PAnsiChar; nExpires: RTime32; bSecure: Boolean; bHTTPOnly: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetPageScaleFactor(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; flZoom: Single; nPointX: Integer; nPointY: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetBackgroundMode(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; bBackgroundMode: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_SetDPIScalingFactor(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; flDPIScaling: Single); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_OpenDeveloperTools(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_AllowStartRequest(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; bAllowed: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_JSDialogResponse(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; bResult: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamHTMLSurface_FileLoadDialogResponse(AISteamHTMLSurface: PISteamHTMLSurface; unBrowserHandle: HHTMLBrowser; pchSelectedFiles: PPAnsiChar); cdecl; external STEAMLIB;

function SteamAPI_ISteamInventory_GetResultStatus(AISteamInventory: PISteamInventory; resultHandle: SteamInventoryResult_t): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetResultItems(AISteamInventory: PISteamInventory; resultHandle: SteamInventoryResult_t; pOutItemsArray: PSteamItemDetails_t; punOutItemsArraySize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetResultItemProperty(AISteamInventory: PISteamInventory; resultHandle: SteamInventoryResult_t; unItemIndex: UInt32; pchPropertyName: PAnsiChar; pchValueBuffer: PAnsiChar; punValueBufferSizeOut: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetResultTimestamp(AISteamInventory: PISteamInventory; resultHandle: SteamInventoryResult_t): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_CheckResultSteamID(AISteamInventory: PISteamInventory; resultHandle: SteamInventoryResult_t; steamIDExpected: CSteamID): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInventory_DestroyResult(AISteamInventory: PISteamInventory; resultHandle: SteamInventoryResult_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetAllItems(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetItemsByID(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; pInstanceIDs: PSteamItemInstanceID_t; unCountInstanceIDs: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_SerializeResult(AISteamInventory: PISteamInventory; resultHandle: SteamInventoryResult_t; pOutBuffer: Pointer; punOutBufferSize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_DeserializeResult(AISteamInventory: PISteamInventory; pOutResultHandle: PSteamInventoryResult_t; pBuffer: Pointer; unBufferSize: UInt32; bRESERVED_MUST_BE_FALSE: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GenerateItems(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; pArrayItemDefs: PSteamItemDef_t; punArrayQuantity: PUInt32; unArrayLength: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GrantPromoItems(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_AddPromoItem(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; itemDef: SteamItemDef_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_AddPromoItems(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; pArrayItemDefs: PSteamItemDef_t; unArrayLength: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_ConsumeItem(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; itemConsume: SteamItemInstanceID_t; unQuantity: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_ExchangeItems(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; pArrayGenerate: PSteamItemDef_t; punArrayGenerateQuantity: PUInt32; unArrayGenerateLength: UInt32; pArrayDestroy: PSteamItemInstanceID_t; punArrayDestroyQuantity: PUInt32; unArrayDestroyLength: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_TransferItemQuantity(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; itemIdSource: SteamItemInstanceID_t; unQuantity: UInt32; itemIdDest: SteamItemInstanceID_t): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamInventory_SendItemDropHeartbeat(AISteamInventory: PISteamInventory); cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_TriggerItemDrop(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; dropListDefinition: SteamItemDef_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_TradeItems(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; steamIDTradePartner: CSteamID; pArrayGive: PSteamItemInstanceID_t; pArrayGiveQuantity: PUInt32; nArrayGiveLength: UInt32; pArrayGet: PSteamItemInstanceID_t; pArrayGetQuantity: PUInt32; nArrayGetLength: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_LoadItemDefinitions(AISteamInventory: PISteamInventory): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetItemDefinitionIDs(AISteamInventory: PISteamInventory; pItemDefIDs: PSteamItemDef_t; punItemDefIDsArraySize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetItemDefinitionProperty(AISteamInventory: PISteamInventory; iDefinition: SteamItemDef_t; pchPropertyName: PAnsiChar; pchValueBuffer: PAnsiChar; punValueBufferSizeOut: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_RequestEligiblePromoItemDefinitionsIDs(AISteamInventory: PISteamInventory; steamID: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetEligiblePromoItemDefinitionIDs(AISteamInventory: PISteamInventory; steamID: CSteamID; pItemDefIDs: PSteamItemDef_t; punItemDefIDsArraySize: PUInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_StartPurchase(AISteamInventory: PISteamInventory; pArrayItemDefs: PSteamItemDef_t; punArrayQuantity: PUInt32; unArrayLength: UInt32): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_RequestPrices(AISteamInventory: PISteamInventory): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetNumItemsWithPrices(AISteamInventory: PISteamInventory): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetItemsWithPrices(AISteamInventory: PISteamInventory; pArrayItemDefs: PSteamItemDef_t; pCurrentPrices: PUInt64; pBasePrices: PUInt64; unArrayLength: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_GetItemPrice(AISteamInventory: PISteamInventory; iDefinition: SteamItemDef_t; pCurrentPrice: PUInt64; pBasePrice: PUInt64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_StartUpdateProperties(AISteamInventory: PISteamInventory): SteamInventoryUpdateHandle_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_RemoveProperty(AISteamInventory: PISteamInventory; handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; pchPropertyName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_SetPropertyString(AISteamInventory: PISteamInventory; handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; pchPropertyName: PAnsiChar; pchPropertyValue: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_SetPropertyBool(AISteamInventory: PISteamInventory; handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; pchPropertyName: PAnsiChar; bValue: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_SetPropertyInt64(AISteamInventory: PISteamInventory; handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; pchPropertyName: PAnsiChar; nValue: Int64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_SetPropertyFloat(AISteamInventory: PISteamInventory; handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; pchPropertyName: PAnsiChar; flValue: Single): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_SubmitUpdateProperties(AISteamInventory: PISteamInventory; handle: SteamInventoryUpdateHandle_t; pResultHandle: PSteamInventoryResult_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamInventory_InspectItem(AISteamInventory: PISteamInventory; pResultHandle: PSteamInventoryResult_t; pchItemToken: PAnsiChar): Boolean; cdecl; external STEAMLIB;

procedure SteamAPI_ISteamVideo_GetVideoURL(AISteamVideo: PISteamVideo; unVideoAppID: AppId_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamVideo_IsBroadcasting(AISteamVideo: PISteamVideo; pnNumViewers: PInteger): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamVideo_GetOPFSettings(AISteamVideo: PISteamVideo; unVideoAppID: AppId_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamVideo_GetOPFStringForApp(AISteamVideo: PISteamVideo; unVideoAppID: AppId_t; pchBuffer: PAnsiChar; pnBufferSize: PInt32): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamParentalSettings_BIsParentalLockEnabled(AISteamParentalSettings: PISteamParentalSettings): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParentalSettings_BIsParentalLockLocked(AISteamParentalSettings: PISteamParentalSettings): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParentalSettings_BIsAppBlocked(AISteamParentalSettings: PISteamParentalSettings; nAppID: AppId_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParentalSettings_BIsAppInBlockList(AISteamParentalSettings: PISteamParentalSettings; nAppID: AppId_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParentalSettings_BIsFeatureBlocked(AISteamParentalSettings: PISteamParentalSettings; eFeature: EParentalFeature): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamParentalSettings_BIsFeatureInBlockList(AISteamParentalSettings: PISteamParentalSettings; eFeature: EParentalFeature): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamRemotePlay_GetSessionCount(AISteamRemotePlay: PISteamRemotePlay): UInt32; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemotePlay_GetSessionID(AISteamRemotePlay: PISteamRemotePlay; iSessionIndex: Integer): RemotePlaySessionID_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemotePlay_GetSessionSteamID(AISteamRemotePlay: PISteamRemotePlay; unSessionID: RemotePlaySessionID_t): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemotePlay_GetSessionClientName(AISteamRemotePlay: PISteamRemotePlay; unSessionID: RemotePlaySessionID_t): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemotePlay_GetSessionClientFormFactor(AISteamRemotePlay: PISteamRemotePlay; unSessionID: RemotePlaySessionID_t): ESteamDeviceFormFactor; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemotePlay_BGetSessionClientResolution(AISteamRemotePlay: PISteamRemotePlay; unSessionID: RemotePlaySessionID_t; pnResolutionX: PInteger; pnResolutionY: PInteger): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamRemotePlay_BSendRemotePlayTogetherInvite(AISteamRemotePlay: PISteamRemotePlay; steamIDFriend: CSteamID): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamNetworkingMessages_SendMessageToUser(AISteamNetworkingMessages: PISteamNetworkingMessages; identityRemote: PSteamNetworkingIdentity; pubData: Pointer; cubData: UInt32; nSendFlags: Integer; nRemoteChannel: Integer): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingMessages_ReceiveMessagesOnChannel(AISteamNetworkingMessages: PISteamNetworkingMessages; nLocalChannel: Integer; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingMessages_AcceptSessionWithUser(AISteamNetworkingMessages: PISteamNetworkingMessages; identityRemote: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingMessages_CloseSessionWithUser(AISteamNetworkingMessages: PISteamNetworkingMessages; identityRemote: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingMessages_CloseChannelWithUser(AISteamNetworkingMessages: PISteamNetworkingMessages; identityRemote: PSteamNetworkingIdentity; nLocalChannel: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingMessages_GetSessionConnectionInfo(AISteamNetworkingMessages: PISteamNetworkingMessages; identityRemote: PSteamNetworkingIdentity; pConnectionInfo: PSteamNetConnectionInfo_t; pQuickStatus: PSteamNetConnectionRealTimeStatus_t): ESteamNetworkingConnectionState; cdecl; external STEAMLIB;

function SteamAPI_ISteamNetworkingSockets_CreateListenSocketIP(AISteamNetworkingSockets: PISteamNetworkingSockets; localAddress: PSteamNetworkingIPAddr; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ConnectByIPAddress(AISteamNetworkingSockets: PISteamNetworkingSockets; address: PSteamNetworkingIPAddr; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2P(AISteamNetworkingSockets: PISteamNetworkingSockets; nLocalVirtualPort: Integer; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ConnectP2P(AISteamNetworkingSockets: PISteamNetworkingSockets; identityRemote: PSteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_AcceptConnection(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CloseConnection(AISteamNetworkingSockets: PISteamNetworkingSockets; hPeer: HSteamNetConnection; nReason: Integer; pszDebug: PAnsiChar; bEnableLinger: Boolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CloseListenSocket(AISteamNetworkingSockets: PISteamNetworkingSockets; hSocket: HSteamListenSocket): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_SetConnectionUserData(AISteamNetworkingSockets: PISteamNetworkingSockets; hPeer: HSteamNetConnection; nUserData: Int64): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetConnectionUserData(AISteamNetworkingSockets: PISteamNetworkingSockets; hPeer: HSteamNetConnection): Int64; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingSockets_SetConnectionName(AISteamNetworkingSockets: PISteamNetworkingSockets; hPeer: HSteamNetConnection; pszName: PAnsiChar); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetConnectionName(AISteamNetworkingSockets: PISteamNetworkingSockets; hPeer: HSteamNetConnection; pszName: PAnsiChar; nMaxLen: Integer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_SendMessageToConnection(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; pData: Pointer; cbData: UInt32; nSendFlags: Integer; pOutMessageNumber: PInt64): EResult; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingSockets_SendMessages(AISteamNetworkingSockets: PISteamNetworkingSockets; nMessages: Integer; pMessages: PPSteamNetworkingMessage_t; pOutMessageNumberOrResult: PInt64); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_FlushMessagesOnConnection(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetConnectionInfo(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; pInfo: PSteamNetConnectionInfo_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetConnectionRealTimeStatus(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; pStatus: PSteamNetConnectionRealTimeStatus_t; nLanes: Integer; pLanes: PSteamNetConnectionRealTimeLaneStatus_t): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetDetailedConnectionStatus(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; pszBuf: PAnsiChar; cbBuf: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetListenSocketAddress(AISteamNetworkingSockets: PISteamNetworkingSockets; hSocket: HSteamListenSocket; address: PSteamNetworkingIPAddr): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CreateSocketPair(AISteamNetworkingSockets: PISteamNetworkingSockets; pOutConnection1: PHSteamNetConnection; pOutConnection2: PHSteamNetConnection; bUseNetworkLoopback: Boolean; pIdentity1: PSteamNetworkingIdentity; pIdentity2: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ConfigureConnectionLanes(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; nNumLanes: Integer; pLanePriorities: PInteger; pLaneWeights: PUInt16): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetIdentity(AISteamNetworkingSockets: PISteamNetworkingSockets; pIdentity: PSteamNetworkingIdentity): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_InitAuthentication(AISteamNetworkingSockets: PISteamNetworkingSockets): ESteamNetworkingAvailability; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetAuthenticationStatus(AISteamNetworkingSockets: PISteamNetworkingSockets; pDetails: PSteamNetAuthenticationStatus_t): ESteamNetworkingAvailability; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CreatePollGroup(AISteamNetworkingSockets: PISteamNetworkingSockets): HSteamNetPollGroup; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_DestroyPollGroup(AISteamNetworkingSockets: PISteamNetworkingSockets; hPollGroup: HSteamNetPollGroup): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; hPollGroup: HSteamNetPollGroup): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup(AISteamNetworkingSockets: PISteamNetworkingSockets; hPollGroup: HSteamNetPollGroup; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ReceivedRelayAuthTicket(AISteamNetworkingSockets: PISteamNetworkingSockets; pvTicket: Pointer; cbTicket: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_FindRelayAuthTicketForServer(AISteamNetworkingSockets: PISteamNetworkingSockets; identityGameServer: PSteamNetworkingIdentity; nRemoteVirtualPort: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ConnectToHostedDedicatedServer(AISteamNetworkingSockets: PISteamNetworkingSockets; identityTarget: PSteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPort(AISteamNetworkingSockets: PISteamNetworkingSockets): UInt16; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPOPID(AISteamNetworkingSockets: PISteamNetworkingSockets): SteamNetworkingPOPID; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerAddress(AISteamNetworkingSockets: PISteamNetworkingSockets; pRouting: PSteamDatagramHostedAddress): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CreateHostedDedicatedServerListenSocket(AISteamNetworkingSockets: PISteamNetworkingSockets; nLocalVirtualPort: Integer; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetGameCoordinatorServerLogin(AISteamNetworkingSockets: PISteamNetworkingSockets; pLoginInfo: PSteamDatagramGameCoordinatorServerLogin; pcbSignedBlob: PInteger; pBlob: Pointer): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ConnectP2PCustomSignaling(AISteamNetworkingSockets: PISteamNetworkingSockets; pSignaling: PISteamNetworkingConnectionSignaling; pPeerIdentity: PSteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_ReceivedP2PCustomSignal(AISteamNetworkingSockets: PISteamNetworkingSockets; pMsg: Pointer; cbMsg: Integer; pContext: PISteamNetworkingSignalingRecvContext): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetCertificateRequest(AISteamNetworkingSockets: PISteamNetworkingSockets; pcbBlob: PInteger; pBlob: Pointer; errMsg: PSteamNetworkingErrMsg): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_SetCertificate(AISteamNetworkingSockets: PISteamNetworkingSockets; pCertificate: Pointer; cbCertificate: Integer; errMsg: PSteamNetworkingErrMsg): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingSockets_ResetIdentity(AISteamNetworkingSockets: PISteamNetworkingSockets; pIdentity: PSteamNetworkingIdentity); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingSockets_RunCallbacks(AISteamNetworkingSockets: PISteamNetworkingSockets); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_BeginAsyncRequestFakeIP(AISteamNetworkingSockets: PISteamNetworkingSockets; nNumPorts: Integer): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingSockets_GetFakeIP(AISteamNetworkingSockets: PISteamNetworkingSockets; idxFirstPort: Integer; pInfo: PSteamNetworkingFakeIPResult_t); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2PFakeIP(AISteamNetworkingSockets: PISteamNetworkingSockets; idxFakePort: Integer; nOptions: Integer; pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_GetRemoteFakeIPForConnection(AISteamNetworkingSockets: PISteamNetworkingSockets; hConn: HSteamNetConnection; pOutAddr: PSteamNetworkingIPAddr): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingSockets_CreateFakeUDPPort(AISteamNetworkingSockets: PISteamNetworkingSockets; idxFakeServerPort: Integer): PISteamNetworkingFakeUDPPort; cdecl; external STEAMLIB;

function SteamAPI_ISteamNetworkingUtils_AllocateMessage(AISteamNetworkingUtils: PISteamNetworkingUtils; cbAllocateBuffer: Integer): PSteamNetworkingMessage_t; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingUtils_InitRelayNetworkAccess(AISteamNetworkingUtils: PISteamNetworkingUtils); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetRelayNetworkStatus(AISteamNetworkingUtils: PISteamNetworkingUtils; pDetails: PSteamRelayNetworkStatus_t): ESteamNetworkingAvailability; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetLocalPingLocation(AISteamNetworkingUtils: PISteamNetworkingUtils; aResult: PSteamNetworkPingLocation_t): Single; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_EstimatePingTimeBetweenTwoLocations(AISteamNetworkingUtils: PISteamNetworkingUtils; location1: PSteamNetworkPingLocation_t; location2: PSteamNetworkPingLocation_t): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_EstimatePingTimeFromLocalHost(AISteamNetworkingUtils: PISteamNetworkingUtils; remoteLocation: PSteamNetworkPingLocation_t): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingUtils_ConvertPingLocationToString(AISteamNetworkingUtils: PISteamNetworkingUtils; location: PSteamNetworkPingLocation_t; pszBuf: PAnsiChar; cchBufSize: Integer); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_ParsePingLocationString(AISteamNetworkingUtils: PISteamNetworkingUtils; pszString: PAnsiChar; aResult: PSteamNetworkPingLocation_t): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_CheckPingDataUpToDate(AISteamNetworkingUtils: PISteamNetworkingUtils; flMaxAgeSeconds: Single): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetPingToDataCenter(AISteamNetworkingUtils: PISteamNetworkingUtils; popID: SteamNetworkingPOPID; pViaRelayPoP: PSteamNetworkingPOPID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetDirectPingToPOP(AISteamNetworkingUtils: PISteamNetworkingUtils; popID: SteamNetworkingPOPID): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetPOPCount(AISteamNetworkingUtils: PISteamNetworkingUtils): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetPOPList(AISteamNetworkingUtils: PISteamNetworkingUtils; list: PSteamNetworkingPOPID; nListSz: Integer): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetLocalTimestamp(AISteamNetworkingUtils: PISteamNetworkingUtils): SteamNetworkingMicroseconds; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingUtils_SetDebugOutputFunction(AISteamNetworkingUtils: PISteamNetworkingUtils; eDetailLevel: ESteamNetworkingSocketsDebugOutputType; pfnFunc: FSteamNetworkingSocketsDebugOutput); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_IsFakeIPv4(AISteamNetworkingUtils: PISteamNetworkingUtils; nIPv4: UInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetIPv4FakeIPType(AISteamNetworkingUtils: PISteamNetworkingUtils; nIPv4: UInt32): ESteamNetworkingFakeIPType; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetRealIdentityForFakeIP(AISteamNetworkingUtils: PISteamNetworkingUtils; fakeIP: PSteamNetworkingIPAddr; pOutRealIdentity: PSteamNetworkingIdentity): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueInt32(AISteamNetworkingUtils: PISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; value: Int32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueFloat(AISteamNetworkingUtils: PISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; value: Single): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueString(AISteamNetworkingUtils: PISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; value: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValuePtr(AISteamNetworkingUtils: PISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; value: Pointer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueInt32(AISteamNetworkingUtils: PISteamNetworkingUtils; hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; value: Int32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueFloat(AISteamNetworkingUtils: PISteamNetworkingUtils; hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; value: Single): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueString(AISteamNetworkingUtils: PISteamNetworkingUtils; hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; value: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetConnectionStatusChanged(AISteamNetworkingUtils: PISteamNetworkingUtils; fnCallback: FnSteamNetConnectionStatusChanged): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetAuthenticationStatusChanged(AISteamNetworkingUtils: PISteamNetworkingUtils; fnCallback: FnSteamNetAuthenticationStatusChanged): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamRelayNetworkStatusChanged(AISteamNetworkingUtils: PISteamNetworkingUtils; fnCallback: FnSteamRelayNetworkStatusChanged): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_FakeIPResult(AISteamNetworkingUtils: PISteamNetworkingUtils; fnCallback: FnSteamNetworkingFakeIPResult): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionRequest(AISteamNetworkingUtils: PISteamNetworkingUtils; fnCallback: FnSteamNetworkingMessagesSessionRequest): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionFailed(AISteamNetworkingUtils: PISteamNetworkingUtils; fnCallback: FnSteamNetworkingMessagesSessionFailed): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetConfigValue(AISteamNetworkingUtils: PISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr; eDataType: ESteamNetworkingConfigDataType; pArg: Pointer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SetConfigValueStruct(AISteamNetworkingUtils: PISteamNetworkingUtils; opt: PSteamNetworkingConfigValue_t; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetConfigValue(AISteamNetworkingUtils: PISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr; pOutDataType: PESteamNetworkingConfigDataType; pResult: Pointer; cbResult: Pcsize_t): ESteamNetworkingGetConfigValueResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_GetConfigValueInfo(AISteamNetworkingUtils: PISteamNetworkingUtils; eValue: ESteamNetworkingConfigValue; pOutDataType: PESteamNetworkingConfigDataType; pOutScope: PESteamNetworkingConfigScope): PAnsiChar; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_IterateGenericEditableConfigValues(AISteamNetworkingUtils: PISteamNetworkingUtils; eCurrent: ESteamNetworkingConfigValue; bEnumerateDevVars: Boolean): ESteamNetworkingConfigValue; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ToString(AISteamNetworkingUtils: PISteamNetworkingUtils; addr: PSteamNetworkingIPAddr; buf: PAnsiChar; cbBuf: UInt32; bWithPort: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ParseString(AISteamNetworkingUtils: PISteamNetworkingUtils; pAddr: PSteamNetworkingIPAddr; pszStr: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_GetFakeIPType(AISteamNetworkingUtils: PISteamNetworkingUtils; addr: PSteamNetworkingIPAddr): ESteamNetworkingFakeIPType; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ToString(AISteamNetworkingUtils: PISteamNetworkingUtils; identity: PSteamNetworkingIdentity; buf: PAnsiChar; cbBuf: UInt32); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ParseString(AISteamNetworkingUtils: PISteamNetworkingUtils; pIdentity: PSteamNetworkingIdentity; pszStr: PAnsiChar): Boolean; cdecl; external STEAMLIB;

procedure SteamAPI_ISteamGameServer_SetProduct(AISteamGameServer: PISteamGameServer; pszProduct: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetGameDescription(AISteamGameServer: PISteamGameServer; pszGameDescription: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetModDir(AISteamGameServer: PISteamGameServer; pszModDir: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetDedicatedServer(AISteamGameServer: PISteamGameServer; bDedicated: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_LogOn(AISteamGameServer: PISteamGameServer; pszToken: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_LogOnAnonymous(AISteamGameServer: PISteamGameServer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_LogOff(AISteamGameServer: PISteamGameServer); cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_BLoggedOn(AISteamGameServer: PISteamGameServer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_BSecure(AISteamGameServer: PISteamGameServer): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_GetSteamID(AISteamGameServer: PISteamGameServer): CSteamID; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_WasRestartRequested(AISteamGameServer: PISteamGameServer): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetMaxPlayerCount(AISteamGameServer: PISteamGameServer; cPlayersMax: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetBotPlayerCount(AISteamGameServer: PISteamGameServer; cBotplayers: Integer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetServerName(AISteamGameServer: PISteamGameServer; pszServerName: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetMapName(AISteamGameServer: PISteamGameServer; pszMapName: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetPasswordProtected(AISteamGameServer: PISteamGameServer; bPasswordProtected: Boolean); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetSpectatorPort(AISteamGameServer: PISteamGameServer; unSpectatorPort: UInt16); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetSpectatorServerName(AISteamGameServer: PISteamGameServer; pszSpectatorServerName: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_ClearAllKeyValues(AISteamGameServer: PISteamGameServer); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetKeyValue(AISteamGameServer: PISteamGameServer; pKey: PAnsiChar; pValue: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetGameTags(AISteamGameServer: PISteamGameServer; pchGameTags: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetGameData(AISteamGameServer: PISteamGameServer; pchGameData: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetRegion(AISteamGameServer: PISteamGameServer; pszRegion: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SetAdvertiseServerActive(AISteamGameServer: PISteamGameServer; bActive: Boolean); cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_GetAuthSessionTicket(AISteamGameServer: PISteamGameServer; pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): HAuthTicket; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_BeginAuthSession(AISteamGameServer: PISteamGameServer; pAuthTicket: Pointer; cbAuthTicket: Integer; steamID: CSteamID): EBeginAuthSessionResult; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_EndAuthSession(AISteamGameServer: PISteamGameServer; steamID: CSteamID); cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_CancelAuthTicket(AISteamGameServer: PISteamGameServer; hAuthTicket: HAuthTicket); cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_UserHasLicenseForApp(AISteamGameServer: PISteamGameServer; steamID: CSteamID; appID: AppId_t): EUserHasLicenseForAppResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_RequestUserGroupStatus(AISteamGameServer: PISteamGameServer; steamIDUser: CSteamID; steamIDGroup: CSteamID): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_GetGameplayStats(AISteamGameServer: PISteamGameServer); cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_GetServerReputation(AISteamGameServer: PISteamGameServer): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_GetPublicIP(AISteamGameServer: PISteamGameServer): SteamIPAddress_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_HandleIncomingPacket(AISteamGameServer: PISteamGameServer; pData: Pointer; cbData: Integer; srcIP: UInt32; srcPort: UInt16): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_GetNextOutgoingPacket(AISteamGameServer: PISteamGameServer; pOut: Pointer; cbMaxOut: Integer; pNetAdr: PUInt32; pPort: PUInt16): Integer; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_AssociateWithClan(AISteamGameServer: PISteamGameServer; steamIDClan: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_ComputeNewPlayerCompatibility(AISteamGameServer: PISteamGameServer; steamIDNewPlayer: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_SendUserConnectAndAuthenticate_DEPRECATED(AISteamGameServer: PISteamGameServer; unIPClient: UInt32; pvAuthBlob: Pointer; cubAuthBlobSize: UInt32; pSteamIDUser: PCSteamID): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_CreateUnauthenticatedUserConnection(AISteamGameServer: PISteamGameServer): CSteamID; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamGameServer_SendUserDisconnect_DEPRECATED(AISteamGameServer: PISteamGameServer; steamIDUser: CSteamID); cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServer_BUpdateUserData(AISteamGameServer: PISteamGameServer; steamIDUser: CSteamID; pchPlayerName: PAnsiChar; uScore: UInt32): Boolean; cdecl; external STEAMLIB;

function SteamAPI_ISteamGameServerStats_RequestUserStats(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_GetUserStatInt32(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar; pData: PInt32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_GetUserStatFloat(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar; pData: PSingle): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_GetUserAchievement(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar; pbAchieved: PBoolean): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_SetUserStatInt32(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar; nData: Int32): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_SetUserStatFloat(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar; fData: Single): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_UpdateUserAvgRateStat(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar; flCountThisSession: Single; dSessionLength: Double): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_SetUserAchievement(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_ClearUserAchievement(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID; pchName: PAnsiChar): Boolean; cdecl; external STEAMLIB;
function SteamAPI_ISteamGameServerStats_StoreUserStats(AISteamGameServerStats: PISteamGameServerStats; steamIDUser: CSteamID): SteamAPICall_t; cdecl; external STEAMLIB;

procedure SteamAPI_ISteamNetworkingFakeUDPPort_DestroyFakeUDPPort(AISteamNetworkingFakeUDPPort: PISteamNetworkingFakeUDPPort); cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingFakeUDPPort_SendMessageToFakeIP(AISteamNetworkingFakeUDPPort: PISteamNetworkingFakeUDPPort; remoteAddress: PSteamNetworkingIPAddr; pData: Pointer; cbData: UInt32; nSendFlags: Integer): EResult; cdecl; external STEAMLIB;
function SteamAPI_ISteamNetworkingFakeUDPPort_ReceiveMessages(AISteamNetworkingFakeUDPPort: PISteamNetworkingFakeUDPPort; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer; cdecl; external STEAMLIB;
procedure SteamAPI_ISteamNetworkingFakeUDPPort_ScheduleCleanup(AISteamNetworkingFakeUDPPort: PISteamNetworkingFakeUDPPort; remoteAddress: PSteamNetworkingIPAddr); cdecl; external STEAMLIB;

type
  SteamIPAddress_tHelper = record helper for SteamIPAddress_t
    {$IFDEF STEAM}
    function IsSet(): Boolean;
    {$ENDIF}
  end;

  MatchMakingKeyValuePair_tHelper = record helper for MatchMakingKeyValuePair_t
    {$IFDEF STEAM}
    procedure Construct();
    {$ENDIF}
  end;

  servernetadr_tHelper = record helper for servernetadr_t
    {$IFDEF STEAM}
    procedure Construct();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure Init(ip: UInt32; usQueryPort: UInt16; usConnectionPort: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryPort(): UInt16;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetQueryPort(usPort: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetConnectionPort(): UInt16;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetConnectionPort(usPort: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetIP(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetIP(unIP: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetConnectionAddressString(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryAddressString(): PAnsiChar;
    {$ENDIF}
  end;

  gameserveritem_tHelper = record helper for gameserveritem_t
    {$IFDEF STEAM}
    procedure Construct();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetName(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetName(const pName: PAnsiChar);
    {$ENDIF}
  end;

  SteamNetworkingIPAddrHelper = record helper for SteamNetworkingIPAddr
    procedure Clear();
    function IsIPv6AllZeros(): Boolean;
    procedure SetIPv6(const ipv6: PUInt8; nPort: UInt16);
    procedure SetIPv4(nIP: UInt32; nPort: UInt16);
    function IsIPv4(): Boolean;
    function GetIPv4(): UInt32;
    procedure SetIPv6LocalHost(nPort: UInt16);
    function IsLocalHost(): Boolean;
    procedure ToString(buf: PAnsiChar; cbBuf: UInt32; bWithPort: Boolean);
    function ParseString(const pszStr: PAnsiChar): Boolean;
    {$IFDEF STEAM}
    function GetFakeIPType(): ESteamNetworkingFakeIPType;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsFakeIP(): Boolean;
    {$ENDIF}
  end;

  SteamNetworkingIdentityHelper = record helper for SteamNetworkingIdentity
    procedure Clear();
    function IsInvalid(): Boolean;
    procedure SetSteamID(steamID: CSteamID);
    function GetSteamID(): CSteamID;
    procedure SetSteamID64(steamID: UInt64);
    function GetSteamID64(): UInt64;
    {$IFDEF STEAM}
    function SetXboxPairwiseID(const pszString: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetXboxPairwiseID(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetPSNID(id: UInt64);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPSNID(): UInt64;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetStadiaID(id: UInt64);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStadiaID(): UInt64;
    {$ENDIF}
    procedure SetIPAddr(constref addr: SteamNetworkingIPAddr);
    function GetIPAddr(): PSteamNetworkingIPAddr;
    {$IFDEF STEAM}
    procedure SetIPv4Addr(nIPv4: UInt32; nPort: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetIPv4(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFakeIPType(): ESteamNetworkingFakeIPType;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsFakeIP(): Boolean;
    {$ENDIF}
    procedure SetLocalHost();
    function IsLocalHost(): Boolean;
    function SetGenericString(const pszString: PAnsiChar): Boolean;
    function GetGenericString(): PAnsiChar;
    function SetGenericBytes(const data: Pointer; cbLen: UInt32): Boolean;
    function GetGenericBytes(var cbLen: Integer): PUInt8;
    procedure ToString(buf: PAnsiChar; cbBuf: UInt32);
    function ParseString(const pszStr: PAnsiChar): Boolean;
  end;

  SteamNetworkingMessage_tHelper = record helper for SteamNetworkingMessage_t
    procedure Release();
  end;

  SteamNetworkingConfigValue_tHelper = record helper for SteamNetworkingConfigValue_t
    {$IFDEF STEAM}
    procedure SetInt32(eVal: ESteamNetworkingConfigValue; data: Int32);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetInt64(eVal: ESteamNetworkingConfigValue; data: Int64);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetFloat(eVal: ESteamNetworkingConfigValue; data: Single);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetPtr(eVal: ESteamNetworkingConfigValue; data: Pointer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetString(eVal: ESteamNetworkingConfigValue; const data: PAnsiChar);
    {$ENDIF}
  end;

  SteamDatagramHostedAddressHelper = record helper for SteamDatagramHostedAddress
    {$IFDEF STEAM}
    procedure Clear();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPopID(): SteamNetworkingPOPID;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetDevAddress(nIP: UInt32; nPort: UInt16; popid: SteamNetworkingPOPID);
    {$ENDIF}
  end;

  ISteamClientHelper = record helper for ISteamClient
    {$IFDEF STEAM}
    function CreateSteamPipe(): HSteamPipe;
    {$ENDIF}
    {$IFDEF STEAM}
    function BReleaseSteamPipe(hSteamPipe: HSteamPipe): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ConnectToGlobalUser(hSteamPipe: HSteamPipe): HSteamUser;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateLocalUser(phSteamPipe: PHSteamPipe; eAccountType: EAccountType): HSteamUser;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ReleaseUser(hSteamPipe: HSteamPipe; hUser: HSteamUser);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamUser(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUser;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamGameServer(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamGameServer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetLocalIPBinding(constref unIP: SteamIPAddress_t; usPort: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamFriends(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamFriends;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamUtils(hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUtils;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamMatchmaking(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMatchmaking;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamMatchmakingServers(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMatchmakingServers;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamGenericInterface(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): Pointer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamUserStats(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUserStats;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamGameServerStats(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamGameServerStats;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamApps(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamApps;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamNetworking(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamNetworking;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamRemoteStorage(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamRemoteStorage;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamScreenshots(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamScreenshots;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamGameSearch(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamGameSearch;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetIPCCallCount(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetWarningMessageHook(pFunction: SteamAPIWarningMessageHook_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function BShutdownIfAllPipesClosed(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamHTTP(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamHTTP;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamController(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamController;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamUGC(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUGC;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamAppList(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamAppList;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamMusic(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMusic;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamMusicRemote(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMusicRemote;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamHTMLSurface(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamHTMLSurface;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamInventory(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamInventory;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamVideo(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamVideo;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamParentalSettings(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamParentalSettings;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamInput(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamInput;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamParties(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamParties;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetISteamRemotePlay(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamRemotePlay;
    {$ENDIF}
  end;

  ISteamUserHelper = record helper for ISteamUser
    {$IFDEF STEAM}
    function GetHSteamUser(): HSteamUser;
    {$ENDIF}
    {$IFDEF STEAM}
    function BLoggedOn(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSteamID(): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function InitiateGameConnection_DEPRECATED(pAuthBlob: Pointer; cbMaxAuthBlob: Integer; steamIDGameServer: CSteamID; unIPServer: UInt32; usPortServer: UInt16; bSecure: Boolean): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TerminateGameConnection_DEPRECATED(unIPServer: UInt32; usPortServer: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TrackAppUsageEvent(gameID: CGameID; eAppUsageEvent: Integer; const pchExtraInfo: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserDataFolder(pchBuffer: PAnsiChar; cubBuffer: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure StartVoiceRecording();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure StopVoiceRecording();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAvailableVoice(pcbCompressed: PUInt32; pcbUncompressed_Deprecated: PUInt32; nUncompressedVoiceDesiredSampleRate_Deprecated: UInt32): EVoiceResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetVoice(bWantCompressed: Boolean; pDestBuffer: Pointer; cbDestBufferSize: UInt32; nBytesWritten: PUInt32; bWantUncompressed_Deprecated: Boolean; pUncompressedDestBuffer_Deprecated: Pointer; cbUncompressedDestBufferSize_Deprecated: UInt32; nUncompressBytesWritten_Deprecated: PUInt32; nUncompressedVoiceDesiredSampleRate_Deprecated: UInt32): EVoiceResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function DecompressVoice(const pCompressed: Pointer; cbCompressed: UInt32; pDestBuffer: Pointer; cbDestBufferSize: UInt32; nBytesWritten: PUInt32; nDesiredSampleRate: UInt32): EVoiceResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetVoiceOptimalSampleRate(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAuthSessionTicket(pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): HAuthTicket;
    {$ENDIF}
    {$IFDEF STEAM}
    function BeginAuthSession(const pAuthTicket: Pointer; cbAuthTicket: Integer; steamID: CSteamID): EBeginAuthSessionResult;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure EndAuthSession(steamID: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure CancelAuthTicket(hAuthTicket: HAuthTicket);
    {$ENDIF}
    {$IFDEF STEAM}
    function UserHasLicenseForApp(steamID: CSteamID; appID: AppId_t): EUserHasLicenseForAppResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsBehindNAT(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AdvertiseGame(steamIDGameServer: CSteamID; unIPServer: UInt32; usPortServer: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestEncryptedAppTicket(pDataToInclude: Pointer; cbDataToInclude: Integer): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetEncryptedAppTicket(pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGameBadgeLevel(nSeries: Integer; bFoil: Boolean): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPlayerSteamLevel(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestStoreAuthURL(const pchRedirectURL: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsPhoneVerified(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsTwoFactorEnabled(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsPhoneIdentifying(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsPhoneRequiringVerification(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetMarketEligibility(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDurationControl(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function BSetDurationControlOnlineState(eNewState: EDurationControlOnlineState): Boolean;
    {$ENDIF}
  end;

  ISteamFriendsHelper = record helper for ISteamFriends
    {$IFDEF STEAM}
    function GetPersonaName(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetPersonaName(const pchPersonaName: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPersonaState(): EPersonaState;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendCount(iFriendFlags: Integer): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendByIndex(iFriend: Integer; iFriendFlags: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendRelationship(steamIDFriend: CSteamID): EFriendRelationship;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendPersonaState(steamIDFriend: CSteamID): EPersonaState;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendPersonaName(steamIDFriend: CSteamID): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendGamePlayed(steamIDFriend: CSteamID; pFriendGameInfo: PFriendGameInfo_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendPersonaNameHistory(steamIDFriend: CSteamID; iPersonaName: Integer): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendSteamLevel(steamIDFriend: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPlayerNickname(steamIDPlayer: CSteamID): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendsGroupCount(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendsGroupIDByIndex(iFG: Integer): FriendsGroupID_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendsGroupName(friendsGroupID: FriendsGroupID_t): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendsGroupMembersCount(friendsGroupID: FriendsGroupID_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure GetFriendsGroupMembersList(friendsGroupID: FriendsGroupID_t; pOutSteamIDMembers: PCSteamID; nMembersCount: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    function HasFriend(steamIDFriend: CSteamID; iFriendFlags: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanCount(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanByIndex(iClan: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanName(steamIDClan: CSteamID): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanTag(steamIDClan: CSteamID): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanActivityCounts(steamIDClan: CSteamID; pnOnline: PInteger; pnInGame: PInteger; pnChatting: PInteger): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function DownloadClanActivityCounts(psteamIDClans: PCSteamID; cClansToRequest: Integer): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendCountFromSource(steamIDSource: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendFromSourceByIndex(steamIDSource: CSteamID; iFriend: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsUserInSource(steamIDUser: CSteamID; steamIDSource: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetInGameVoiceSpeaking(steamIDUser: CSteamID; bSpeaking: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateGameOverlay(const pchDialog: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateGameOverlayToUser(const pchDialog: PAnsiChar; steamID: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateGameOverlayToWebPage(const pchURL: PAnsiChar; eMode: EActivateGameOverlayToWebPageMode);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateGameOverlayToStore(nAppID: AppId_t; eFlag: EOverlayToStoreFlag);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetPlayedWith(steamIDUserPlayedWith: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateGameOverlayInviteDialog(steamIDLobby: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSmallFriendAvatar(steamIDFriend: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetMediumFriendAvatar(steamIDFriend: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLargeFriendAvatar(steamIDFriend: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestUserInformation(steamIDUser: CSteamID; bRequireNameOnly: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestClanOfficerList(steamIDClan: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanOwner(steamIDClan: CSteamID): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanOfficerCount(steamIDClan: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanOfficerByIndex(steamIDClan: CSteamID; iOfficer: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserRestrictions(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetRichPresence(const pchKey: PAnsiChar; const pchValue: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ClearRichPresence();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendRichPresence(steamIDFriend: CSteamID; const pchKey: PAnsiChar): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendRichPresenceKeyCount(steamIDFriend: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendRichPresenceKeyByIndex(steamIDFriend: CSteamID; iKey: Integer): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RequestFriendRichPresence(steamIDFriend: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function InviteUserToGame(steamIDFriend: CSteamID; const pchConnectString: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCoplayFriendCount(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCoplayFriend(iCoplayFriend: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendCoplayTime(steamIDFriend: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendCoplayGame(steamIDFriend: CSteamID): AppId_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function JoinClanChatRoom(steamIDClan: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function LeaveClanChatRoom(steamIDClan: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanChatMemberCount(steamIDClan: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetChatMemberByIndex(steamIDClan: CSteamID; iUser: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function SendClanChatMessage(steamIDClanChat: CSteamID; const pchText: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetClanChatMessage(steamIDClanChat: CSteamID; iMessage: Integer; prgchText: Pointer; cchTextMax: Integer; peChatEntryType: PEChatEntryType; psteamidChatter: PCSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsClanChatAdmin(steamIDClanChat: CSteamID; steamIDUser: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsClanChatWindowOpenInSteam(steamIDClanChat: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function OpenClanChatWindowInSteam(steamIDClanChat: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CloseClanChatWindowInSteam(steamIDClanChat: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetListenForFriendsMessages(bInterceptEnabled: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReplyToFriendMessage(steamIDFriend: CSteamID; const pchMsgToSend: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFriendMessage(steamIDFriend: CSteamID; iMessageID: Integer; pvData: Pointer; cubData: Integer; peChatEntryType: PEChatEntryType): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFollowerCount(steamID: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsFollowing(steamID: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnumerateFollowingList(unStartIndex: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsClanPublic(steamIDClan: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsClanOfficialGameGroup(steamIDClan: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNumChatsWithUnreadPriorityMessages(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateGameOverlayRemotePlayTogetherInviteDialog(steamIDLobby: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function RegisterProtocolInOverlayBrowser(const pchProtocol: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateGameOverlayInviteDialogConnectString(const pchConnectString: PAnsiChar);
    {$ENDIF}
  end;

  ISteamUtilsHelper = record helper for ISteamUtils
    {$IFDEF STEAM}
    function GetSecondsSinceAppActive(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSecondsSinceComputerActive(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetConnectedUniverse(): EUniverse;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetServerRealTime(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetIPCountry(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetImageSize(iImage: Integer; pnWidth: PUInt32; pnHeight: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetImageRGBA(iImage: Integer; pubDest: PUInt8; nDestBufferSize: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCurrentBatteryPower(): UInt8;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppID(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetOverlayNotificationPosition(eNotificationPosition: ENotificationPosition);
    {$ENDIF}
    {$IFDEF STEAM}
    function IsAPICallCompleted(hSteamAPICall: SteamAPICall_t; pbFailed: PBoolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAPICallFailureReason(hSteamAPICall: SteamAPICall_t): ESteamAPICallFailure;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAPICallResult(hSteamAPICall: SteamAPICall_t; pCallback: Pointer; cubCallback: Integer; iCallbackExpected: Integer; pbFailed: PBoolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetIPCCallCount(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetWarningMessageHook(pFunction: SteamAPIWarningMessageHook_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function IsOverlayEnabled(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BOverlayNeedsPresent(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CheckFileSignature(const szFileName: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function ShowGamepadTextInput(eInputMode: EGamepadTextInputMode; eLineInputMode: EGamepadTextInputLineMode; const pchDescription: PAnsiChar; unCharMax: UInt32; const pchExistingText: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetEnteredGamepadTextLength(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetEnteredGamepadTextInput(pchText: PAnsiChar; cchText: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSteamUILanguage(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsSteamRunningInVR(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetOverlayNotificationInset(nHorizontalInset: Integer; nVerticalInset: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    function IsSteamInBigPictureMode(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure StartVRDashboard();
    {$ENDIF}
    {$IFDEF STEAM}
    function IsVRHeadsetStreamingEnabled(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetVRHeadsetStreamingEnabled(bEnabled: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function IsSteamChinaLauncher(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function InitFilterText(unFilterOptions: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FilterText(eContext: ETextFilteringContext; sourceSteamID: CSteamID; const pchInputMessage: PAnsiChar; pchOutFilteredText: PAnsiChar; nByteSizeOutFilteredText: UInt32): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetIPv6ConnectivityState(eProtocol: ESteamIPv6ConnectivityProtocol): ESteamIPv6ConnectivityState;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsSteamRunningOnSteamDeck(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ShowFloatingGamepadTextInput(eKeyboardMode: EFloatingGamepadTextInputMode; nTextFieldXPosition: Integer; nTextFieldYPosition: Integer; nTextFieldWidth: Integer; nTextFieldHeight: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetGameLauncherMode(bLauncherMode: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function DismissFloatingGamepadTextInput(): Boolean;
    {$ENDIF}
  end;

  ISteamMatchmakingHelper = record helper for ISteamMatchmaking
    {$IFDEF STEAM}
    function GetFavoriteGameCount(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFavoriteGame(iGame: Integer; pnAppID: PAppId_t; pnIP: PUInt32; pnConnPort: PUInt16; pnQueryPort: PUInt16; punFlags: PUInt32; pRTime32LastPlayedOnServer: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddFavoriteGame(nAppID: AppId_t; nIP: UInt32; nConnPort: UInt16; nQueryPort: UInt16; unFlags: UInt32; rTime32LastPlayedOnServer: UInt32): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveFavoriteGame(nAppID: AppId_t; nIP: UInt32; nConnPort: UInt16; nQueryPort: UInt16; unFlags: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestLobbyList(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddRequestLobbyListStringFilter(const pchKeyToMatch: PAnsiChar; const pchValueToMatch: PAnsiChar; eComparisonType: ELobbyComparison);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddRequestLobbyListNumericalFilter(const pchKeyToMatch: PAnsiChar; nValueToMatch: Integer; eComparisonType: ELobbyComparison);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddRequestLobbyListNearValueFilter(const pchKeyToMatch: PAnsiChar; nValueToBeCloseTo: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddRequestLobbyListFilterSlotsAvailable(nSlotsAvailable: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddRequestLobbyListDistanceFilter(eLobbyDistanceFilter: ELobbyDistanceFilter);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddRequestLobbyListResultCountFilter(cMaxResults: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddRequestLobbyListCompatibleMembersFilter(steamIDLobby: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyByIndex(iLobby: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateLobby(eLobbyType: ELobbyType; cMaxMembers: Integer): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function JoinLobby(steamIDLobby: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure LeaveLobby(steamIDLobby: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function InviteUserToLobby(steamIDLobby: CSteamID; steamIDInvitee: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNumLobbyMembers(steamIDLobby: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyMemberByIndex(steamIDLobby: CSteamID; iMember: Integer): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyData(steamIDLobby: CSteamID; const pchKey: PAnsiChar): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLobbyData(steamIDLobby: CSteamID; const pchKey: PAnsiChar; const pchValue: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyDataCount(steamIDLobby: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyDataByIndex(steamIDLobby: CSteamID; iLobbyData: Integer; pchKey: PAnsiChar; cchKeyBufferSize: Integer; pchValue: PAnsiChar; cchValueBufferSize: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function DeleteLobbyData(steamIDLobby: CSteamID; const pchKey: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyMemberData(steamIDLobby: CSteamID; steamIDUser: CSteamID; const pchKey: PAnsiChar): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetLobbyMemberData(steamIDLobby: CSteamID; const pchKey: PAnsiChar; const pchValue: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    function SendLobbyChatMsg(steamIDLobby: CSteamID; const pvMsgBody: Pointer; cubMsgBody: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyChatEntry(steamIDLobby: CSteamID; iChatID: Integer; pSteamIDUser: PCSteamID; pvData: Pointer; cubData: Integer; peChatEntryType: PEChatEntryType): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestLobbyData(steamIDLobby: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetLobbyGameServer(steamIDLobby: CSteamID; unGameServerIP: UInt32; unGameServerPort: UInt16; steamIDGameServer: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyGameServer(steamIDLobby: CSteamID; punGameServerIP: PUInt32; punGameServerPort: PUInt16; psteamIDGameServer: PCSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLobbyMemberLimit(steamIDLobby: CSteamID; cMaxMembers: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyMemberLimit(steamIDLobby: CSteamID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLobbyType(steamIDLobby: CSteamID; eLobbyType: ELobbyType): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLobbyJoinable(steamIDLobby: CSteamID; bLobbyJoinable: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLobbyOwner(steamIDLobby: CSteamID): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLobbyOwner(steamIDLobby: CSteamID; steamIDNewOwner: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLinkedLobby(steamIDLobby: CSteamID; steamIDLobbyDependent: CSteamID): Boolean;
    {$ENDIF}
  end;

  ISteamMatchmakingServerListResponseHelper = record helper for ISteamMatchmakingServerListResponse
    {$IFDEF STEAM}
    procedure ServerResponded(hRequest: HServerListRequest; iServer: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ServerFailedToRespond(hRequest: HServerListRequest; iServer: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RefreshComplete(hRequest: HServerListRequest; response: EMatchMakingServerResponse);
    {$ENDIF}
  end;

  ISteamMatchmakingPingResponseHelper = record helper for ISteamMatchmakingPingResponse
    {$IFDEF STEAM}
    procedure ServerResponded(var server: gameserveritem_t);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ServerFailedToRespond();
    {$ENDIF}
  end;

  ISteamMatchmakingPlayersResponseHelper = record helper for ISteamMatchmakingPlayersResponse
    {$IFDEF STEAM}
    procedure AddPlayerToList(const pchName: PAnsiChar; nScore: Integer; flTimePlayed: Single);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure PlayersFailedToRespond();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure PlayersRefreshComplete();
    {$ENDIF}
  end;

  ISteamMatchmakingRulesResponseHelper = record helper for ISteamMatchmakingRulesResponse
    {$IFDEF STEAM}
    procedure RulesResponded(const pchRule: PAnsiChar; const pchValue: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RulesFailedToRespond();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RulesRefreshComplete();
    {$ENDIF}
  end;

  ISteamMatchmakingServersHelper = record helper for ISteamMatchmakingServers
    {$IFDEF STEAM}
    function RequestInternetServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestLANServerList(iApp: AppId_t; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestFriendsServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestFavoritesServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestHistoryServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestSpectatorServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ReleaseRequest(hServerListRequest: HServerListRequest);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetServerDetails(hRequest: HServerListRequest; iServer: Integer): Pgameserveritem_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure CancelQuery(hRequest: HServerListRequest);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RefreshQuery(hRequest: HServerListRequest);
    {$ENDIF}
    {$IFDEF STEAM}
    function IsRefreshing(hRequest: HServerListRequest): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetServerCount(hRequest: HServerListRequest): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RefreshServer(hRequest: HServerListRequest; iServer: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    function PingServer(unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingPingResponse): HServerQuery;
    {$ENDIF}
    {$IFDEF STEAM}
    function PlayerDetails(unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingPlayersResponse): HServerQuery;
    {$ENDIF}
    {$IFDEF STEAM}
    function ServerRules(unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingRulesResponse): HServerQuery;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure CancelServerQuery(hServerQuery: HServerQuery);
    {$ENDIF}
  end;

  ISteamGameSearchHelper = record helper for ISteamGameSearch
    {$IFDEF STEAM}
    function AddGameSearchParams(const pchKeyToFind: PAnsiChar; const pchValuesToFind: PAnsiChar): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SearchForGameWithLobby(steamIDLobby: CSteamID; nPlayerMin: Integer; nPlayerMax: Integer): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SearchForGameSolo(nPlayerMin: Integer; nPlayerMax: Integer): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function AcceptGame(): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function DeclineGame(): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RetrieveConnectionDetails(steamIDHost: CSteamID; pchConnectionDetails: PAnsiChar; cubConnectionDetails: Integer): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EndGameSearch(): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetGameHostParams(const pchKey: PAnsiChar; const pchValue: PAnsiChar): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetConnectionDetails(const pchConnectionDetails: PAnsiChar; cubConnectionDetails: Integer): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestPlayersForGame(nPlayerMin: Integer; nPlayerMax: Integer; nMaxTeamSize: Integer): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function HostConfirmGameStart(ullUniqueGameID: UInt64): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function CancelRequestPlayersForGame(): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SubmitPlayerResult(ullUniqueGameID: UInt64; steamIDPlayer: CSteamID; EPlayerResult: EPlayerResult_t): EGameSearchErrorCode_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EndGame(ullUniqueGameID: UInt64): EGameSearchErrorCode_t;
    {$ENDIF}
  end;

  ISteamPartiesHelper = record helper for ISteamParties
    {$IFDEF STEAM}
    function GetNumActiveBeacons(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetBeaconByIndex(unIndex: UInt32): PartyBeaconID_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetBeaconDetails(ulBeaconID: PartyBeaconID_t; pSteamIDBeaconOwner: PCSteamID; pLocation: PSteamPartyBeaconLocation_t; pchMetadata: PAnsiChar; cchMetadata: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function JoinParty(ulBeaconID: PartyBeaconID_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNumAvailableBeaconLocations(puNumLocations: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAvailableBeaconLocations(pLocationList: PSteamPartyBeaconLocation_t; uMaxNumLocations: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateBeacon(unOpenSlots: UInt32; pBeaconLocation: PSteamPartyBeaconLocation_t; const pchConnectString: PAnsiChar; const pchMetadata: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure OnReservationCompleted(ulBeacon: PartyBeaconID_t; steamIDUser: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure CancelReservation(ulBeacon: PartyBeaconID_t; steamIDUser: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function ChangeNumOpenSlots(ulBeacon: PartyBeaconID_t; unOpenSlots: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function DestroyBeacon(ulBeacon: PartyBeaconID_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetBeaconLocationData(BeaconLocation: SteamPartyBeaconLocation_t; eData: ESteamPartyBeaconLocationData; pchDataStringOut: PAnsiChar; cchDataStringOut: Integer): Boolean;
    {$ENDIF}
  end;

  ISteamRemoteStorageHelper = record helper for ISteamRemoteStorage
    {$IFDEF STEAM}
    function FileWrite(const pchFile: PAnsiChar; const pvData: Pointer; cubData: Int32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileRead(const pchFile: PAnsiChar; pvData: Pointer; cubDataToRead: Int32): Int32;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileWriteAsync(const pchFile: PAnsiChar; const pvData: Pointer; cubData: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileReadAsync(const pchFile: PAnsiChar; nOffset: UInt32; cubToRead: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileReadAsyncComplete(hReadCall: SteamAPICall_t; pvBuffer: Pointer; cubToRead: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileForget(const pchFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileDelete(const pchFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileShare(const pchFile: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetSyncPlatforms(const pchFile: PAnsiChar; eRemoteStoragePlatform: ERemoteStoragePlatform): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileWriteStreamOpen(const pchFile: PAnsiChar): UGCFileWriteStreamHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileWriteStreamWriteChunk(writeHandle: UGCFileWriteStreamHandle_t; const pvData: Pointer; cubData: Int32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileWriteStreamClose(writeHandle: UGCFileWriteStreamHandle_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileWriteStreamCancel(writeHandle: UGCFileWriteStreamHandle_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FileExists(const pchFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FilePersisted(const pchFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFileSize(const pchFile: PAnsiChar): Int32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFileTimestamp(const pchFile: PAnsiChar): Int64;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSyncPlatforms(const pchFile: PAnsiChar): ERemoteStoragePlatform;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFileCount(): Int32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFileNameAndSize(iFile: Integer; pnFileSizeInBytes: PInt32): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQuota(pnTotalBytes: PUInt64; puAvailableBytes: PUInt64): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsCloudEnabledForAccount(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsCloudEnabledForApp(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetCloudEnabledForApp(bEnabled: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function UGCDownload(hContent: UGCHandle_t; unPriority: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUGCDownloadProgress(hContent: UGCHandle_t; pnBytesDownloaded: PInt32; pnBytesExpected: PInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUGCDetails(hContent: UGCHandle_t; pnAppID: PAppId_t; ppchName: PPAnsiChar; pnFileSizeInBytes: PInt32; pSteamIDOwner: PCSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UGCRead(hContent: UGCHandle_t; pvData: Pointer; cubDataToRead: Int32; cOffset: UInt32; eAction: EUGCReadAction): Int32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCachedUGCCount(): Int32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCachedUGCHandle(iCachedContent: Int32): UGCHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function PublishWorkshopFile(const pchFile: PAnsiChar; const pchPreviewFile: PAnsiChar; nConsumerAppId: AppId_t; const pchTitle: PAnsiChar; const pchDescription: PAnsiChar; eVisibility: ERemoteStoragePublishedFileVisibility; pTags: PSteamParamStringArray_t; eWorkshopFileType: EWorkshopFileType): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreatePublishedFileUpdateRequest(unPublishedFileId: PublishedFileId_t): PublishedFileUpdateHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePublishedFileFile(updateHandle: PublishedFileUpdateHandle_t; const pchFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePublishedFilePreviewFile(updateHandle: PublishedFileUpdateHandle_t; const pchPreviewFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePublishedFileTitle(updateHandle: PublishedFileUpdateHandle_t; const pchTitle: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePublishedFileDescription(updateHandle: PublishedFileUpdateHandle_t; const pchDescription: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePublishedFileVisibility(updateHandle: PublishedFileUpdateHandle_t; eVisibility: ERemoteStoragePublishedFileVisibility): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePublishedFileTags(updateHandle: PublishedFileUpdateHandle_t; pTags: PSteamParamStringArray_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CommitPublishedFileUpdate(updateHandle: PublishedFileUpdateHandle_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPublishedFileDetails(unPublishedFileId: PublishedFileId_t; unMaxSecondsOld: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function DeletePublishedFile(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnumerateUserPublishedFiles(unStartIndex: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SubscribePublishedFile(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnumerateUserSubscribedFiles(unStartIndex: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function UnsubscribePublishedFile(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePublishedFileSetChangeDescription(updateHandle: PublishedFileUpdateHandle_t; const pchChangeDescription: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPublishedItemVoteDetails(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateUserPublishedItemVote(unPublishedFileId: PublishedFileId_t; bVoteUp: Boolean): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserPublishedItemVoteDetails(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnumerateUserSharedWorkshopFiles(steamId: CSteamID; unStartIndex: UInt32; pRequiredTags: PSteamParamStringArray_t; pExcludedTags: PSteamParamStringArray_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function PublishVideo(eVideoProvider: EWorkshopVideoProvider; const pchVideoAccount: PAnsiChar; const pchVideoIdentifier: PAnsiChar; const pchPreviewFile: PAnsiChar; nConsumerAppId: AppId_t; const pchTitle: PAnsiChar; const pchDescription: PAnsiChar; eVisibility: ERemoteStoragePublishedFileVisibility; pTags: PSteamParamStringArray_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetUserPublishedFileAction(unPublishedFileId: PublishedFileId_t; eAction: EWorkshopFileAction): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnumeratePublishedFilesByUserAction(eAction: EWorkshopFileAction; unStartIndex: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnumeratePublishedWorkshopFiles(eEnumerationType: EWorkshopEnumerationType; unStartIndex: UInt32; unCount: UInt32; unDays: UInt32; pTags: PSteamParamStringArray_t; pUserTags: PSteamParamStringArray_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function UGCDownloadToLocation(hContent: UGCHandle_t; const pchLocation: PAnsiChar; unPriority: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLocalFileChangeCount(): Int32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLocalFileChange(iFile: Integer; pEChangeType: PERemoteStorageLocalFileChange; pEFilePathType: PERemoteStorageFilePathType): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function BeginFileWriteBatch(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function EndFileWriteBatch(): Boolean;
    {$ENDIF}
  end;

  ISteamUserStatsHelper = record helper for ISteamUserStats
    {$IFDEF STEAM}
    function RequestCurrentStats(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStat(const pchName: PAnsiChar; pData: PInt32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStat(const pchName: PAnsiChar; pData: PSingle): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetStat(const pchName: PAnsiChar; nData: Int32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetStat(const pchName: PAnsiChar; fData: Single): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateAvgRateStat(const pchName: PAnsiChar; flCountThisSession: Single; dSessionLength: Double): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievement(const pchName: PAnsiChar; pbAchieved: PBoolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetAchievement(const pchName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ClearAchievement(const pchName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievementAndUnlockTime(const pchName: PAnsiChar; pbAchieved: PBoolean; punUnlockTime: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function StoreStats(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievementIcon(const pchName: PAnsiChar): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievementDisplayAttribute(const pchName: PAnsiChar; const pchKey: PAnsiChar): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function IndicateAchievementProgress(const pchName: PAnsiChar; nCurProgress: UInt32; nMaxProgress: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNumAchievements(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievementName(iAchievement: UInt32): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestUserStats(steamIDUser: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PInt32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PSingle): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar; pbAchieved: PBoolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserAchievementAndUnlockTime(steamIDUser: CSteamID; const pchName: PAnsiChar; pbAchieved: PBoolean; punUnlockTime: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ResetAllStats(bAchievementsToo: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FindOrCreateLeaderboard(const pchLeaderboardName: PAnsiChar; eLeaderboardSortMethod: ELeaderboardSortMethod; eLeaderboardDisplayType: ELeaderboardDisplayType): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function FindLeaderboard(const pchLeaderboardName: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLeaderboardName(hSteamLeaderboard: SteamLeaderboard_t): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLeaderboardEntryCount(hSteamLeaderboard: SteamLeaderboard_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLeaderboardSortMethod(hSteamLeaderboard: SteamLeaderboard_t): ELeaderboardSortMethod;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLeaderboardDisplayType(hSteamLeaderboard: SteamLeaderboard_t): ELeaderboardDisplayType;
    {$ENDIF}
    {$IFDEF STEAM}
    function DownloadLeaderboardEntries(hSteamLeaderboard: SteamLeaderboard_t; eLeaderboardDataRequest: ELeaderboardDataRequest; nRangeStart: Integer; nRangeEnd: Integer): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function DownloadLeaderboardEntriesForUsers(hSteamLeaderboard: SteamLeaderboard_t; prgUsers: PCSteamID; cUsers: Integer): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDownloadedLeaderboardEntry(hSteamLeaderboardEntries: SteamLeaderboardEntries_t; index: Integer; pLeaderboardEntry: PLeaderboardEntry_t; pDetails: PInt32; cDetailsMax: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UploadLeaderboardScore(hSteamLeaderboard: SteamLeaderboard_t; eLeaderboardUploadScoreMethod: ELeaderboardUploadScoreMethod; nScore: Int32; const pScoreDetails: PInt32; cScoreDetailsCount: Integer): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function AttachLeaderboardUGC(hSteamLeaderboard: SteamLeaderboard_t; hUGC: UGCHandle_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNumberOfCurrentPlayers(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestGlobalAchievementPercentages(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetMostAchievedAchievementInfo(pchName: PAnsiChar; unNameBufLen: UInt32; pflPercent: PSingle; pbAchieved: PBoolean): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNextMostAchievedAchievementInfo(iIteratorPrevious: Integer; pchName: PAnsiChar; unNameBufLen: UInt32; pflPercent: PSingle; pbAchieved: PBoolean): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievementAchievedPercent(const pchName: PAnsiChar; pflPercent: PSingle): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestGlobalStats(nHistoryDays: Integer): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlobalStat(const pchStatName: PAnsiChar; pData: PInt64): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlobalStat(const pchStatName: PAnsiChar; pData: PDouble): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlobalStatHistory(const pchStatName: PAnsiChar; pData: PInt64; cubData: UInt32): Int32; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlobalStatHistory(const pchStatName: PAnsiChar; pData: PDouble; cubData: UInt32): Int32; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievementProgressLimits(const pchName: PAnsiChar; pnMinProgress: PInt32; pnMaxProgress: PInt32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAchievementProgressLimits(const pchName: PAnsiChar; pfMinProgress: PSingle; pfMaxProgress: PSingle): Boolean; overload;
    {$ENDIF}
  end;

  ISteamAppsHelper = record helper for ISteamApps
    {$IFDEF STEAM}
    function BIsSubscribed(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsLowViolence(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsCybercafe(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsVACBanned(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCurrentGameLanguage(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAvailableGameLanguages(): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsSubscribedApp(appID: AppId_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsDlcInstalled(appID: AppId_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetEarliestPurchaseUnixTime(nAppID: AppId_t): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsSubscribedFromFreeWeekend(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDLCCount(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function BGetDLCDataByIndex(iDLC: Integer; pAppID: PAppId_t; pbAvailable: PBoolean; pchName: PAnsiChar; cchNameBufferSize: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure InstallDLC(nAppID: AppId_t);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure UninstallDLC(nAppID: AppId_t);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RequestAppProofOfPurchaseKey(nAppID: AppId_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCurrentBetaName(pchName: PAnsiChar; cchNameBufferSize: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function MarkContentCorrupt(bMissingFilesOnly: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetInstalledDepots(appID: AppId_t; pvecDepots: PDepotId_t; cMaxDepots: UInt32): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppInstallDir(appID: AppId_t; pchFolder: PAnsiChar; cchFolderBufferSize: UInt32): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsAppInstalled(appID: AppId_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppOwner(): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLaunchQueryParam(const pchKey: PAnsiChar): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDlcDownloadProgress(nAppID: AppId_t; punBytesDownloaded: PUInt64; punBytesTotal: PUInt64): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppBuildId(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RequestAllProofOfPurchaseKeys();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetFileDetails(const pszFileName: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLaunchCommandLine(pszCommandLine: PAnsiChar; cubCommandLine: Integer): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsSubscribedFromFamilySharing(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsTimedTrial(punSecondsAllowed: PUInt32; punSecondsPlayed: PUInt32): Boolean;
    {$ENDIF}
  end;

  ISteamNetworkingHelper = record helper for ISteamNetworking
    {$IFDEF STEAM}
    function SendP2PPacket(steamIDRemote: CSteamID; const pubData: Pointer; cubData: UInt32; eP2PSendType: EP2PSend; nChannel: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsP2PPacketAvailable(pcubMsgSize: PUInt32; nChannel: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReadP2PPacket(pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32; psteamIDRemote: PCSteamID; nChannel: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AcceptP2PSessionWithUser(steamIDRemote: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CloseP2PSessionWithUser(steamIDRemote: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CloseP2PChannelWithUser(steamIDRemote: CSteamID; nChannel: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetP2PSessionState(steamIDRemote: CSteamID; pConnectionState: PP2PSessionState_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AllowP2PPacketRelay(bAllow: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateListenSocket(nVirtualP2PPort: Integer; nIP: SteamIPAddress_t; nPort: UInt16; bAllowUseOfPacketRelay: Boolean): SNetListenSocket_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateP2PConnectionSocket(steamIDTarget: CSteamID; nVirtualPort: Integer; nTimeoutSec: Integer; bAllowUseOfPacketRelay: Boolean): SNetSocket_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateConnectionSocket(nIP: SteamIPAddress_t; nPort: UInt16; nTimeoutSec: Integer): SNetSocket_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function DestroySocket(hSocket: SNetSocket_t; bNotifyRemoteEnd: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function DestroyListenSocket(hSocket: SNetListenSocket_t; bNotifyRemoteEnd: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SendDataOnSocket(hSocket: SNetSocket_t; pubData: Pointer; cubData: UInt32; bReliable: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsDataAvailableOnSocket(hSocket: SNetSocket_t; pcubMsgSize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RetrieveDataFromSocket(hSocket: SNetSocket_t; pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsDataAvailable(hListenSocket: SNetListenSocket_t; pcubMsgSize: PUInt32; phSocket: PSNetSocket_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RetrieveData(hListenSocket: SNetListenSocket_t; pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32; phSocket: PSNetSocket_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSocketInfo(hSocket: SNetSocket_t; pSteamIDRemote: PCSteamID; peSocketStatus: PInteger; punIPRemote: PSteamIPAddress_t; punPortRemote: PUInt16): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetListenSocketInfo(hListenSocket: SNetListenSocket_t; pnIP: PSteamIPAddress_t; pnPort: PUInt16): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSocketConnectionType(hSocket: SNetSocket_t): ESNetSocketConnectionType;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetMaxPacketSize(hSocket: SNetSocket_t): Integer;
    {$ENDIF}
  end;

  ISteamScreenshotsHelper = record helper for ISteamScreenshots
    {$IFDEF STEAM}
    function WriteScreenshot(pubRGB: Pointer; cubRGB: UInt32; nWidth: Integer; nHeight: Integer): ScreenshotHandle;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddScreenshotToLibrary(const pchFilename: PAnsiChar; const pchThumbnailFilename: PAnsiChar; nWidth: Integer; nHeight: Integer): ScreenshotHandle;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TriggerScreenshot();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure HookScreenshots(bHook: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLocation(hScreenshot: ScreenshotHandle; const pchLocation: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function TagUser(hScreenshot: ScreenshotHandle; steamID: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function TagPublishedFile(hScreenshot: ScreenshotHandle; unPublishedFileID: PublishedFileId_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function IsScreenshotsHooked(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddVRScreenshotToLibrary(eType: EVRScreenshotType; const pchFilename: PAnsiChar; const pchVRFilename: PAnsiChar): ScreenshotHandle;
    {$ENDIF}
  end;

  ISteamMusicHelper = record helper for ISteamMusic
    {$IFDEF STEAM}
    function BIsEnabled(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsPlaying(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPlaybackStatus(): AudioPlayback_Status;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure Play();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure Pause();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure PlayPrevious();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure PlayNext();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetVolume(flVolume: Single);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetVolume(): Single;
    {$ENDIF}
  end;

  ISteamMusicRemoteHelper = record helper for ISteamMusicRemote
    {$IFDEF STEAM}
    function RegisterSteamMusicRemote(const pchName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function DeregisterSteamMusicRemote(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsCurrentMusicRemote(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BActivationSuccess(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetDisplayName(const pchDisplayName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetPNGIcon_64x64(pvBuffer: Pointer; cbBufferLength: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnablePlayPrevious(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnablePlayNext(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnableShuffled(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnableLooped(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnableQueue(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function EnablePlaylists(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdatePlaybackStatus(nStatus: AudioPlayback_Status): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateShuffled(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateLooped(bValue: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateVolume(flValue: Single): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CurrentEntryWillChange(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CurrentEntryIsAvailable(bAvailable: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateCurrentEntryText(const pchText: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateCurrentEntryElapsedSeconds(nValue: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateCurrentEntryCoverArt(pvBuffer: Pointer; cbBufferLength: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CurrentEntryDidChange(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function QueueWillChange(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ResetQueueEntries(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetQueueEntry(nID: Integer; nPosition: Integer; const pchEntryText: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetCurrentQueueEntry(nID: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function QueueDidChange(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function PlaylistWillChange(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ResetPlaylistEntries(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetPlaylistEntry(nID: Integer; nPosition: Integer; const pchEntryText: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetCurrentPlaylistEntry(nID: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function PlaylistDidChange(): Boolean;
    {$ENDIF}
  end;

  ISteamHTTPHelper = record helper for ISteamHTTP
    {$IFDEF STEAM}
    function CreateHTTPRequest(eHTTPRequestMethod: EHTTPMethod; const pchAbsoluteURL: PAnsiChar): HTTPRequestHandle;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestContextValue(hRequest: HTTPRequestHandle; ulContextValue: UInt64): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestNetworkActivityTimeout(hRequest: HTTPRequestHandle; unTimeoutSeconds: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestHeaderValue(hRequest: HTTPRequestHandle; const pchHeaderName: PAnsiChar; const pchHeaderValue: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestGetOrPostParameter(hRequest: HTTPRequestHandle; const pchParamName: PAnsiChar; const pchParamValue: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SendHTTPRequest(hRequest: HTTPRequestHandle; pCallHandle: PSteamAPICall_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SendHTTPRequestAndStreamResponse(hRequest: HTTPRequestHandle; pCallHandle: PSteamAPICall_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function DeferHTTPRequest(hRequest: HTTPRequestHandle): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function PrioritizeHTTPRequest(hRequest: HTTPRequestHandle): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHTTPResponseHeaderSize(hRequest: HTTPRequestHandle; const pchHeaderName: PAnsiChar; unResponseHeaderSize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHTTPResponseHeaderValue(hRequest: HTTPRequestHandle; const pchHeaderName: PAnsiChar; pHeaderValueBuffer: PUInt8; unBufferSize: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHTTPResponseBodySize(hRequest: HTTPRequestHandle; unBodySize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHTTPResponseBodyData(hRequest: HTTPRequestHandle; pBodyDataBuffer: PUInt8; unBufferSize: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHTTPStreamingResponseBodyData(hRequest: HTTPRequestHandle; cOffset: UInt32; pBodyDataBuffer: PUInt8; unBufferSize: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReleaseHTTPRequest(hRequest: HTTPRequestHandle): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHTTPDownloadProgressPct(hRequest: HTTPRequestHandle; pflPercentOut: PSingle): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestRawPostBody(hRequest: HTTPRequestHandle; const pchContentType: PAnsiChar; pubBody: PUInt8; unBodyLen: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateCookieContainer(bAllowResponsesToModify: Boolean): HTTPCookieContainerHandle;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReleaseCookieContainer(hCookieContainer: HTTPCookieContainerHandle): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetCookie(hCookieContainer: HTTPCookieContainerHandle; const pchHost: PAnsiChar; const pchUrl: PAnsiChar; const pchCookie: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestCookieContainer(hRequest: HTTPRequestHandle; hCookieContainer: HTTPCookieContainerHandle): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestUserAgentInfo(hRequest: HTTPRequestHandle; const pchUserAgentInfo: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestRequiresVerifiedCertificate(hRequest: HTTPRequestHandle; bRequireVerifiedCertificate: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetHTTPRequestAbsoluteTimeoutMS(hRequest: HTTPRequestHandle; unMilliseconds: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHTTPRequestWasTimedOut(hRequest: HTTPRequestHandle; pbWasTimedOut: PBoolean): Boolean;
    {$ENDIF}
  end;

  ISteamInputHelper = record helper for ISteamInput
    {$IFDEF STEAM}
    function Init(bExplicitlyCallRunFrame: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function Shutdown(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetInputActionManifestFilePath(const pchInputActionManifestAbsolutePath: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RunFrame(bReservedValue: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function BWaitForData(bWaitForever: Boolean; unTimeout: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BNewDataAvailable(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetConnectedControllers(handlesOut: PInputHandle_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure EnableDeviceCallbacks();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure EnableActionEventCallbacks(pCallback: SteamInputActionEventCallbackPointer);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetActionSetHandle(const pszActionSetName: PAnsiChar): InputActionSetHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateActionSet(inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCurrentActionSet(inputHandle: InputHandle_t): InputActionSetHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateActionSetLayer(inputHandle: InputHandle_t; actionSetLayerHandle: InputActionSetHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure DeactivateActionSetLayer(inputHandle: InputHandle_t; actionSetLayerHandle: InputActionSetHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure DeactivateAllActionSetLayers(inputHandle: InputHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetActiveActionSetLayers(inputHandle: InputHandle_t; handlesOut: PInputActionSetHandle_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDigitalActionHandle(const pszActionName: PAnsiChar): InputDigitalActionHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDigitalActionData(inputHandle: InputHandle_t; digitalActionHandle: InputDigitalActionHandle_t): InputDigitalActionData_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDigitalActionOrigins(inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t; digitalActionHandle: InputDigitalActionHandle_t; originsOut: PEInputActionOrigin): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStringForDigitalActionName(eActionHandle: InputDigitalActionHandle_t): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAnalogActionHandle(const pszActionName: PAnsiChar): InputAnalogActionHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAnalogActionData(inputHandle: InputHandle_t; analogActionHandle: InputAnalogActionHandle_t): InputAnalogActionData_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAnalogActionOrigins(inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t; analogActionHandle: InputAnalogActionHandle_t; originsOut: PEInputActionOrigin): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlyphPNGForActionOrigin(eOrigin: EInputActionOrigin; eSize: ESteamInputGlyphSize; unFlags: UInt32): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlyphSVGForActionOrigin(eOrigin: EInputActionOrigin; unFlags: UInt32): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlyphForActionOrigin_Legacy(eOrigin: EInputActionOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStringForActionOrigin(eOrigin: EInputActionOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStringForAnalogActionName(eActionHandle: InputAnalogActionHandle_t): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure StopAnalogActionMomentum(inputHandle: InputHandle_t; eAction: InputAnalogActionHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetMotionData(inputHandle: InputHandle_t): InputMotionData_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TriggerVibration(inputHandle: InputHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TriggerVibrationExtended(inputHandle: InputHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16; usLeftTriggerSpeed: UInt16; usRightTriggerSpeed: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TriggerSimpleHapticEvent(inputHandle: InputHandle_t; eHapticLocation: EControllerHapticLocation; nIntensity: UInt8; nGainDB: AnsiChar; nOtherIntensity: UInt8; nOtherGainDB: AnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetLEDColor(inputHandle: InputHandle_t; nColorR: UInt8; nColorG: UInt8; nColorB: UInt8; nFlags: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure Legacy_TriggerHapticPulse(inputHandle: InputHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure Legacy_TriggerRepeatedHapticPulse(inputHandle: InputHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16; usOffMicroSec: UInt16; unRepeat: UInt16; nFlags: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    function ShowBindingPanel(inputHandle: InputHandle_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetInputTypeForHandle(inputHandle: InputHandle_t): ESteamInputType;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetControllerForGamepadIndex(nIndex: Integer): InputHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGamepadIndexForController(ulinputHandle: InputHandle_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStringForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlyphForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetActionOriginFromXboxOrigin(inputHandle: InputHandle_t; eOrigin: EXboxOrigin): EInputActionOrigin;
    {$ENDIF}
    {$IFDEF STEAM}
    function TranslateActionOrigin(eDestinationInputType: ESteamInputType; eSourceOrigin: EInputActionOrigin): EInputActionOrigin;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDeviceBindingRevision(inputHandle: InputHandle_t; pMajor: PInteger; pMinor: PInteger): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetRemotePlaySessionID(inputHandle: InputHandle_t): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSessionInputConfigurationSettings(): UInt16;
    {$ENDIF}
  end;

  ISteamControllerHelper = record helper for ISteamController
    {$IFDEF STEAM}
    function Init(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function Shutdown(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RunFrame();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetConnectedControllers(handlesOut: PControllerHandle_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetActionSetHandle(const pszActionSetName: PAnsiChar): ControllerActionSetHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateActionSet(controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetCurrentActionSet(controllerHandle: ControllerHandle_t): ControllerActionSetHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ActivateActionSetLayer(controllerHandle: ControllerHandle_t; actionSetLayerHandle: ControllerActionSetHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure DeactivateActionSetLayer(controllerHandle: ControllerHandle_t; actionSetLayerHandle: ControllerActionSetHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure DeactivateAllActionSetLayers(controllerHandle: ControllerHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetActiveActionSetLayers(controllerHandle: ControllerHandle_t; handlesOut: PControllerActionSetHandle_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDigitalActionHandle(const pszActionName: PAnsiChar): ControllerDigitalActionHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDigitalActionData(controllerHandle: ControllerHandle_t; digitalActionHandle: ControllerDigitalActionHandle_t): InputDigitalActionData_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDigitalActionOrigins(controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t; digitalActionHandle: ControllerDigitalActionHandle_t; originsOut: PEControllerActionOrigin): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAnalogActionHandle(const pszActionName: PAnsiChar): ControllerAnalogActionHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAnalogActionData(controllerHandle: ControllerHandle_t; analogActionHandle: ControllerAnalogActionHandle_t): InputAnalogActionData_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAnalogActionOrigins(controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t; analogActionHandle: ControllerAnalogActionHandle_t; originsOut: PEControllerActionOrigin): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlyphForActionOrigin(eOrigin: EControllerActionOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStringForActionOrigin(eOrigin: EControllerActionOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure StopAnalogActionMomentum(controllerHandle: ControllerHandle_t; eAction: ControllerAnalogActionHandle_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetMotionData(controllerHandle: ControllerHandle_t): InputMotionData_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TriggerHapticPulse(controllerHandle: ControllerHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TriggerRepeatedHapticPulse(controllerHandle: ControllerHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16; usOffMicroSec: UInt16; unRepeat: UInt16; nFlags: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure TriggerVibration(controllerHandle: ControllerHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetLEDColor(controllerHandle: ControllerHandle_t; nColorR: UInt8; nColorG: UInt8; nColorB: UInt8; nFlags: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    function ShowBindingPanel(controllerHandle: ControllerHandle_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetInputTypeForHandle(controllerHandle: ControllerHandle_t): ESteamInputType;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetControllerForGamepadIndex(nIndex: Integer): ControllerHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGamepadIndexForController(ulControllerHandle: ControllerHandle_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetStringForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGlyphForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetActionOriginFromXboxOrigin(controllerHandle: ControllerHandle_t; eOrigin: EXboxOrigin): EControllerActionOrigin;
    {$ENDIF}
    {$IFDEF STEAM}
    function TranslateActionOrigin(eDestinationInputType: ESteamInputType; eSourceOrigin: EControllerActionOrigin): EControllerActionOrigin;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetControllerBindingRevision(controllerHandle: ControllerHandle_t; pMajor: PInteger; pMinor: PInteger): Boolean;
    {$ENDIF}
  end;

  ISteamUGCHelper = record helper for ISteamUGC
    {$IFDEF STEAM}
    function CreateQueryUserUGCRequest(unAccountID: AccountID_t; eListType: EUserUGCList; eMatchingUGCType: EUGCMatchingUGCType; eSortOrder: EUserUGCListSortOrder; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; unPage: UInt32): UGCQueryHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateQueryAllUGCRequest(eQueryType: EUGCQuery; eMatchingeMatchingUGCTypeFileType: EUGCMatchingUGCType; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; unPage: UInt32): UGCQueryHandle_t; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateQueryAllUGCRequest(eQueryType: EUGCQuery; eMatchingeMatchingUGCTypeFileType: EUGCMatchingUGCType; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; const pchCursor: PAnsiChar): UGCQueryHandle_t; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateQueryUGCDetailsRequest(pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): UGCQueryHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SendQueryUGCRequest(handle: UGCQueryHandle_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCResult(handle: UGCQueryHandle_t; index: UInt32; pDetails: PSteamUGCDetails_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCNumTags(handle: UGCQueryHandle_t; index: UInt32): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCTag(handle: UGCQueryHandle_t; index: UInt32; indexTag: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCTagDisplayName(handle: UGCQueryHandle_t; index: UInt32; indexTag: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCPreviewURL(handle: UGCQueryHandle_t; index: UInt32; pchURL: PAnsiChar; cchURLSize: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCMetadata(handle: UGCQueryHandle_t; index: UInt32; pchMetadata: PAnsiChar; cchMetadatasize: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCChildren(handle: UGCQueryHandle_t; index: UInt32; pvecPublishedFileID: PPublishedFileId_t; cMaxEntries: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCStatistic(handle: UGCQueryHandle_t; index: UInt32; eStatType: EItemStatistic; pStatValue: PUInt64): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCNumAdditionalPreviews(handle: UGCQueryHandle_t; index: UInt32): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCAdditionalPreview(handle: UGCQueryHandle_t; index: UInt32; previewIndex: UInt32; pchURLOrVideoID: PAnsiChar; cchURLSize: UInt32; pchOriginalFileName: PAnsiChar; cchOriginalFileNameSize: UInt32; pPreviewType: PEItemPreviewType): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCNumKeyValueTags(handle: UGCQueryHandle_t; index: UInt32): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCKeyValueTag(handle: UGCQueryHandle_t; index: UInt32; keyValueTagIndex: UInt32; pchKey: PAnsiChar; cchKeySize: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetQueryUGCKeyValueTag(handle: UGCQueryHandle_t; index: UInt32; const pchKey: PAnsiChar; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReleaseQueryUGCRequest(handle: UGCQueryHandle_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddRequiredTag(handle: UGCQueryHandle_t; const pTagName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddRequiredTagGroup(handle: UGCQueryHandle_t; const pTagGroups: PSteamParamStringArray_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddExcludedTag(handle: UGCQueryHandle_t; const pTagName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnOnlyIDs(handle: UGCQueryHandle_t; bReturnOnlyIDs: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnKeyValueTags(handle: UGCQueryHandle_t; bReturnKeyValueTags: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnLongDescription(handle: UGCQueryHandle_t; bReturnLongDescription: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnMetadata(handle: UGCQueryHandle_t; bReturnMetadata: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnChildren(handle: UGCQueryHandle_t; bReturnChildren: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnAdditionalPreviews(handle: UGCQueryHandle_t; bReturnAdditionalPreviews: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnTotalOnly(handle: UGCQueryHandle_t; bReturnTotalOnly: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetReturnPlaytimeStats(handle: UGCQueryHandle_t; unDays: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetLanguage(handle: UGCQueryHandle_t; const pchLanguage: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetAllowCachedResponse(handle: UGCQueryHandle_t; unMaxAgeSeconds: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetCloudFileNameFilter(handle: UGCQueryHandle_t; const pMatchCloudFileName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetMatchAnyTag(handle: UGCQueryHandle_t; bMatchAnyTag: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetSearchText(handle: UGCQueryHandle_t; const pSearchText: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetRankedByTrendDays(handle: UGCQueryHandle_t; unDays: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetTimeCreatedDateRange(handle: UGCQueryHandle_t; rtStart: RTime32; rtEnd: RTime32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetTimeUpdatedDateRange(handle: UGCQueryHandle_t; rtStart: RTime32; rtEnd: RTime32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddRequiredKeyValueTag(handle: UGCQueryHandle_t; const pKey: PAnsiChar; const pValue: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestUGCDetails(nPublishedFileID: PublishedFileId_t; unMaxAgeSeconds: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateItem(nConsumerAppId: AppId_t; eFileType: EWorkshopFileType): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function StartItemUpdate(nConsumerAppId: AppId_t; nPublishedFileID: PublishedFileId_t): UGCUpdateHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemTitle(handle: UGCUpdateHandle_t; const pchTitle: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemDescription(handle: UGCUpdateHandle_t; const pchDescription: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemUpdateLanguage(handle: UGCUpdateHandle_t; const pchLanguage: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemMetadata(handle: UGCUpdateHandle_t; const pchMetaData: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemVisibility(handle: UGCUpdateHandle_t; eVisibility: ERemoteStoragePublishedFileVisibility): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemTags(updateHandle: UGCUpdateHandle_t; const pTags: PSteamParamStringArray_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemContent(handle: UGCUpdateHandle_t; const pszContentFolder: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetItemPreview(handle: UGCUpdateHandle_t; const pszPreviewFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetAllowLegacyUpload(handle: UGCUpdateHandle_t; bAllowLegacyUpload: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveAllItemKeyValueTags(handle: UGCUpdateHandle_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveItemKeyValueTags(handle: UGCUpdateHandle_t; const pchKey: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddItemKeyValueTag(handle: UGCUpdateHandle_t; const pchKey: PAnsiChar; const pchValue: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddItemPreviewFile(handle: UGCUpdateHandle_t; const pszPreviewFile: PAnsiChar; aType: EItemPreviewType): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddItemPreviewVideo(handle: UGCUpdateHandle_t; const pszVideoID: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateItemPreviewFile(handle: UGCUpdateHandle_t; index: UInt32; const pszPreviewFile: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateItemPreviewVideo(handle: UGCUpdateHandle_t; index: UInt32; const pszVideoID: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveItemPreview(handle: UGCUpdateHandle_t; index: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SubmitItemUpdate(handle: UGCUpdateHandle_t; const pchChangeNote: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemUpdateProgress(handle: UGCUpdateHandle_t; punBytesProcessed: PUInt64; punBytesTotal: PUInt64): EItemUpdateStatus;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetUserItemVote(nPublishedFileID: PublishedFileId_t; bVoteUp: Boolean): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserItemVote(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddItemToFavorites(nAppId: AppId_t; nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveItemFromFavorites(nAppId: AppId_t; nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SubscribeItem(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function UnsubscribeItem(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNumSubscribedItems(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSubscribedItems(pvecPublishedFileID: PPublishedFileId_t; cMaxEntries: UInt32): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemState(nPublishedFileID: PublishedFileId_t): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemInstallInfo(nPublishedFileID: PublishedFileId_t; punSizeOnDisk: PUInt64; pchFolder: PAnsiChar; cchFolderSize: UInt32; punTimeStamp: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemDownloadInfo(nPublishedFileID: PublishedFileId_t; punBytesDownloaded: PUInt64; punBytesTotal: PUInt64): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function DownloadItem(nPublishedFileID: PublishedFileId_t; bHighPriority: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BInitWorkshopForGameServer(unWorkshopDepotID: DepotId_t; const pszFolder: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SuspendDownloads(bSuspend: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function StartPlaytimeTracking(pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function StopPlaytimeTracking(pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function StopPlaytimeTrackingForAllItems(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddDependency(nParentPublishedFileID: PublishedFileId_t; nChildPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveDependency(nParentPublishedFileID: PublishedFileId_t; nChildPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddAppDependency(nPublishedFileID: PublishedFileId_t; nAppID: AppId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveAppDependency(nPublishedFileID: PublishedFileId_t; nAppID: AppId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppDependencies(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function DeleteItem(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function ShowWorkshopEULA(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetWorkshopEULAStatus(): SteamAPICall_t;
    {$ENDIF}
  end;

  ISteamAppListHelper = record helper for ISteamAppList
    {$IFDEF STEAM}
    function GetNumInstalledApps(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetInstalledApps(pvecAppID: PAppId_t; unMaxAppIDs: UInt32): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppName(nAppID: AppId_t; pchName: PAnsiChar; cchNameMax: Integer): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppInstallDir(nAppID: AppId_t; pchDirectory: PAnsiChar; cchNameMax: Integer): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAppBuildId(nAppID: AppId_t): Integer;
    {$ENDIF}
  end;

  ISteamHTMLSurfaceHelper = record helper for ISteamHTMLSurface
    {$IFDEF STEAM}
    function Init(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function Shutdown(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateBrowser(const pchUserAgent: PAnsiChar; const pchUserCSS: PAnsiChar): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure RemoveBrowser(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure LoadURL(unBrowserHandle: HHTMLBrowser; const pchURL: PAnsiChar; const pchPostData: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetSize(unBrowserHandle: HHTMLBrowser; unWidth: UInt32; unHeight: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure StopLoad(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure Reload(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure GoBack(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure GoForward(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AddHeader(unBrowserHandle: HHTMLBrowser; const pchKey: PAnsiChar; const pchValue: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ExecuteJavascript(unBrowserHandle: HHTMLBrowser; const pchScript: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure MouseUp(unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure MouseDown(unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure MouseDoubleClick(unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure MouseMove(unBrowserHandle: HHTMLBrowser; x: Integer; y: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure MouseWheel(unBrowserHandle: HHTMLBrowser; nDelta: Int32);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure KeyDown(unBrowserHandle: HHTMLBrowser; nNativeKeyCode: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers; bIsSystemKey: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure KeyUp(unBrowserHandle: HHTMLBrowser; nNativeKeyCode: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure KeyChar(unBrowserHandle: HHTMLBrowser; cUnicodeChar: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetHorizontalScroll(unBrowserHandle: HHTMLBrowser; nAbsolutePixelScroll: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetVerticalScroll(unBrowserHandle: HHTMLBrowser; nAbsolutePixelScroll: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetKeyFocus(unBrowserHandle: HHTMLBrowser; bHasKeyFocus: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ViewSource(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure CopyToClipboard(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure PasteFromClipboard(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure Find(unBrowserHandle: HHTMLBrowser; const pchSearchStr: PAnsiChar; bCurrentlyInFind: Boolean; bReverse: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure StopFind(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure GetLinkAtPosition(unBrowserHandle: HHTMLBrowser; x: Integer; y: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetCookie(const pchHostname: PAnsiChar; const pchKey: PAnsiChar; const pchValue: PAnsiChar; const pchPath: PAnsiChar; nExpires: RTime32; bSecure: Boolean; bHTTPOnly: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetPageScaleFactor(unBrowserHandle: HHTMLBrowser; flZoom: Single; nPointX: Integer; nPointY: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetBackgroundMode(unBrowserHandle: HHTMLBrowser; bBackgroundMode: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetDPIScalingFactor(unBrowserHandle: HHTMLBrowser; flDPIScaling: Single);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure OpenDeveloperTools(unBrowserHandle: HHTMLBrowser);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure AllowStartRequest(unBrowserHandle: HHTMLBrowser; bAllowed: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure JSDialogResponse(unBrowserHandle: HHTMLBrowser; bResult: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure FileLoadDialogResponse(unBrowserHandle: HHTMLBrowser; const pchSelectedFiles: PPAnsiChar);
    {$ENDIF}
  end;

  ISteamInventoryHelper = record helper for ISteamInventory
    {$IFDEF STEAM}
    function GetResultStatus(resultHandle: SteamInventoryResult_t): EResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetResultItems(resultHandle: SteamInventoryResult_t; pOutItemsArray: PSteamItemDetails_t; punOutItemsArraySize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetResultItemProperty(resultHandle: SteamInventoryResult_t; unItemIndex: UInt32; const pchPropertyName: PAnsiChar; pchValueBuffer: PAnsiChar; punValueBufferSizeOut: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetResultTimestamp(resultHandle: SteamInventoryResult_t): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function CheckResultSteamID(resultHandle: SteamInventoryResult_t; steamIDExpected: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure DestroyResult(resultHandle: SteamInventoryResult_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAllItems(pResultHandle: PSteamInventoryResult_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemsByID(pResultHandle: PSteamInventoryResult_t; const pInstanceIDs: PSteamItemInstanceID_t; unCountInstanceIDs: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SerializeResult(resultHandle: SteamInventoryResult_t; pOutBuffer: Pointer; punOutBufferSize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function DeserializeResult(pOutResultHandle: PSteamInventoryResult_t; const pBuffer: Pointer; unBufferSize: UInt32; bRESERVED_MUST_BE_FALSE: Boolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GenerateItems(pResultHandle: PSteamInventoryResult_t; const pArrayItemDefs: PSteamItemDef_t; const punArrayQuantity: PUInt32; unArrayLength: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GrantPromoItems(pResultHandle: PSteamInventoryResult_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddPromoItem(pResultHandle: PSteamInventoryResult_t; itemDef: SteamItemDef_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function AddPromoItems(pResultHandle: PSteamInventoryResult_t; const pArrayItemDefs: PSteamItemDef_t; unArrayLength: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ConsumeItem(pResultHandle: PSteamInventoryResult_t; itemConsume: SteamItemInstanceID_t; unQuantity: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ExchangeItems(pResultHandle: PSteamInventoryResult_t; const pArrayGenerate: PSteamItemDef_t; const punArrayGenerateQuantity: PUInt32; unArrayGenerateLength: UInt32; const pArrayDestroy: PSteamItemInstanceID_t; const punArrayDestroyQuantity: PUInt32; unArrayDestroyLength: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function TransferItemQuantity(pResultHandle: PSteamInventoryResult_t; itemIdSource: SteamItemInstanceID_t; unQuantity: UInt32; itemIdDest: SteamItemInstanceID_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SendItemDropHeartbeat();
    {$ENDIF}
    {$IFDEF STEAM}
    function TriggerItemDrop(pResultHandle: PSteamInventoryResult_t; dropListDefinition: SteamItemDef_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function TradeItems(pResultHandle: PSteamInventoryResult_t; steamIDTradePartner: CSteamID; const pArrayGive: PSteamItemInstanceID_t; const pArrayGiveQuantity: PUInt32; nArrayGiveLength: UInt32; const pArrayGet: PSteamItemInstanceID_t; const pArrayGetQuantity: PUInt32; nArrayGetLength: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function LoadItemDefinitions(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemDefinitionIDs(pItemDefIDs: PSteamItemDef_t; punItemDefIDsArraySize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemDefinitionProperty(iDefinition: SteamItemDef_t; const pchPropertyName: PAnsiChar; pchValueBuffer: PAnsiChar; punValueBufferSizeOut: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestEligiblePromoItemDefinitionsIDs(steamID: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetEligiblePromoItemDefinitionIDs(steamID: CSteamID; pItemDefIDs: PSteamItemDef_t; punItemDefIDsArraySize: PUInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function StartPurchase(const pArrayItemDefs: PSteamItemDef_t; const punArrayQuantity: PUInt32; unArrayLength: UInt32): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestPrices(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNumItemsWithPrices(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemsWithPrices(pArrayItemDefs: PSteamItemDef_t; pCurrentPrices: PUInt64; pBasePrices: PUInt64; unArrayLength: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetItemPrice(iDefinition: SteamItemDef_t; pCurrentPrice: PUInt64; pBasePrice: PUInt64): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function StartUpdateProperties(): SteamInventoryUpdateHandle_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function RemoveProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; const pchPropertyValue: PAnsiChar): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; bValue: Boolean): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; nValue: Int64): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; flValue: Single): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function SubmitUpdateProperties(handle: SteamInventoryUpdateHandle_t; pResultHandle: PSteamInventoryResult_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function InspectItem(pResultHandle: PSteamInventoryResult_t; const pchItemToken: PAnsiChar): Boolean;
    {$ENDIF}
  end;

  ISteamVideoHelper = record helper for ISteamVideo
    {$IFDEF STEAM}
    procedure GetVideoURL(unVideoAppID: AppId_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function IsBroadcasting(pnNumViewers: PInteger): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure GetOPFSettings(unVideoAppID: AppId_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetOPFStringForApp(unVideoAppID: AppId_t; pchBuffer: PAnsiChar; pnBufferSize: PInt32): Boolean;
    {$ENDIF}
  end;

  ISteamParentalSettingsHelper = record helper for ISteamParentalSettings
    {$IFDEF STEAM}
    function BIsParentalLockEnabled(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsParentalLockLocked(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsAppBlocked(nAppID: AppId_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsAppInBlockList(nAppID: AppId_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsFeatureBlocked(eFeature: EParentalFeature): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BIsFeatureInBlockList(eFeature: EParentalFeature): Boolean;
    {$ENDIF}
  end;

  ISteamRemotePlayHelper = record helper for ISteamRemotePlay
    {$IFDEF STEAM}
    function GetSessionCount(): UInt32;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSessionID(iSessionIndex: Integer): RemotePlaySessionID_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSessionSteamID(unSessionID: RemotePlaySessionID_t): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSessionClientName(unSessionID: RemotePlaySessionID_t): PAnsiChar;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSessionClientFormFactor(unSessionID: RemotePlaySessionID_t): ESteamDeviceFormFactor;
    {$ENDIF}
    {$IFDEF STEAM}
    function BGetSessionClientResolution(unSessionID: RemotePlaySessionID_t; pnResolutionX: PInteger; pnResolutionY: PInteger): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BSendRemotePlayTogetherInvite(steamIDFriend: CSteamID): Boolean;
    {$ENDIF}
  end;

  ISteamNetworkingMessagesHelper = record helper for ISteamNetworkingMessages
    {$IFDEF STEAM}
    function SendMessageToUser(constref identityRemote: SteamNetworkingIdentity; const pubData: Pointer; cubData: UInt32; nSendFlags: Integer; nRemoteChannel: Integer): EResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReceiveMessagesOnChannel(nLocalChannel: Integer; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function AcceptSessionWithUser(constref identityRemote: SteamNetworkingIdentity): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CloseSessionWithUser(constref identityRemote: SteamNetworkingIdentity): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CloseChannelWithUser(constref identityRemote: SteamNetworkingIdentity; nLocalChannel: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSessionConnectionInfo(constref identityRemote: SteamNetworkingIdentity; pConnectionInfo: PSteamNetConnectionInfo_t; pQuickStatus: PSteamNetConnectionRealTimeStatus_t): ESteamNetworkingConnectionState;
    {$ENDIF}
  end;

  ISteamNetworkingSocketsHelper = record helper for ISteamNetworkingSockets
    function CreateListenSocketIP(constref localAddress: SteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
    function ConnectByIPAddress(constref address: SteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
    {$IFDEF STEAM}
    function CreateListenSocketP2P(nLocalVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
    {$ENDIF}
    {$IFDEF STEAM}
    function ConnectP2P(constref identityRemote: SteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
    {$ENDIF}
    function AcceptConnection(hConn: HSteamNetConnection): EResult;
    function CloseConnection(hPeer: HSteamNetConnection; nReason: Integer; const pszDebug: PAnsiChar; bEnableLinger: Boolean): Boolean;
    function CloseListenSocket(hSocket: HSteamListenSocket): Boolean;
    function SetConnectionUserData(hPeer: HSteamNetConnection; nUserData: Int64): Boolean;
    function GetConnectionUserData(hPeer: HSteamNetConnection): Int64;
    procedure SetConnectionName(hPeer: HSteamNetConnection; const pszName: PAnsiChar);
    function GetConnectionName(hPeer: HSteamNetConnection; pszName: PAnsiChar; nMaxLen: Integer): Boolean;
    function SendMessageToConnection(hConn: HSteamNetConnection; const pData: Pointer; cbData: UInt32; nSendFlags: Integer; pOutMessageNumber: PInt64): EResult;
    procedure SendMessages(nMessages: Integer; pMessages: PPSteamNetworkingMessage_t; pOutMessageNumberOrResult: PInt64);
    function FlushMessagesOnConnection(hConn: HSteamNetConnection): EResult;
    function ReceiveMessagesOnConnection(hConn: HSteamNetConnection; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
    function GetConnectionInfo(hConn: HSteamNetConnection; pInfo: PSteamNetConnectionInfo_t): Boolean;
    function GetConnectionRealTimeStatus(hConn: HSteamNetConnection; pStatus: PSteamNetConnectionRealTimeStatus_t; nLanes: Integer; pLanes: PSteamNetConnectionRealTimeLaneStatus_t): EResult;
    function GetDetailedConnectionStatus(hConn: HSteamNetConnection; pszBuf: PAnsiChar; cbBuf: Integer): Integer;
    function GetListenSocketAddress(hSocket: HSteamListenSocket; address: PSteamNetworkingIPAddr): Boolean;
    function CreateSocketPair(pOutConnection1: PHSteamNetConnection; pOutConnection2: PHSteamNetConnection; bUseNetworkLoopback: Boolean; const pIdentity1: PSteamNetworkingIdentity; const pIdentity2: PSteamNetworkingIdentity): Boolean;
    function ConfigureConnectionLanes(hConn: HSteamNetConnection; nNumLanes: Integer; const pLanePriorities: PInteger; const pLaneWeights: PUInt16): EResult;
    function GetIdentity(pIdentity: PSteamNetworkingIdentity): Boolean;
    function InitAuthentication(): ESteamNetworkingAvailability;
    function GetAuthenticationStatus(pDetails: PSteamNetAuthenticationStatus_t): ESteamNetworkingAvailability;
    function CreatePollGroup(): HSteamNetPollGroup;
    function DestroyPollGroup(hPollGroup: HSteamNetPollGroup): Boolean;
    function SetConnectionPollGroup(hConn: HSteamNetConnection; hPollGroup: HSteamNetPollGroup): Boolean;
    function ReceiveMessagesOnPollGroup(hPollGroup: HSteamNetPollGroup; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
    {$IFDEF STEAM}
    function ReceivedRelayAuthTicket(const pvTicket: Pointer; cbTicket: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function FindRelayAuthTicketForServer(constref identityGameServer: SteamNetworkingIdentity; nRemoteVirtualPort: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function ConnectToHostedDedicatedServer(constref identityTarget: SteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHostedDedicatedServerPort(): UInt16;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHostedDedicatedServerPOPID(): SteamNetworkingPOPID;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetHostedDedicatedServerAddress(pRouting: PSteamDatagramHostedAddress): EResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateHostedDedicatedServerListenSocket(nLocalVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetGameCoordinatorServerLogin(pLoginInfo: PSteamDatagramGameCoordinatorServerLogin; pcbSignedBlob: PInteger; pBlob: Pointer): EResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function ConnectP2PCustomSignaling(pSignaling: PISteamNetworkingConnectionSignaling; const pPeerIdentity: PSteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReceivedP2PCustomSignal(const pMsg: Pointer; cbMsg: Integer; pContext: PISteamNetworkingSignalingRecvContext): Boolean;
    {$ENDIF}
    function GetCertificateRequest(pcbBlob: PInteger; pBlob: Pointer; var errMsg: SteamNetworkingErrMsg): Boolean;
    function SetCertificate(const pCertificate: Pointer; cbCertificate: Integer; var errMsg: SteamNetworkingErrMsg): Boolean;
    {$IFDEF STEAM}
    procedure ResetIdentity(const pIdentity: PSteamNetworkingIdentity);
    {$ENDIF}
    procedure RunCallbacks();
    {$IFDEF STEAM}
    function BeginAsyncRequestFakeIP(nNumPorts: Integer): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure GetFakeIP(idxFirstPort: Integer; pInfo: PSteamNetworkingFakeIPResult_t);
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateListenSocketP2PFakeIP(idxFakePort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetRemoteFakeIPForConnection(hConn: HSteamNetConnection; pOutAddr: PSteamNetworkingIPAddr): EResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateFakeUDPPort(idxFakeServerPort: Integer): PISteamNetworkingFakeUDPPort;
    {$ENDIF}
  end;

  ISteamNetworkingUtilsHelper = record helper for ISteamNetworkingUtils
    function AllocateMessage(cbAllocateBuffer: Integer): PSteamNetworkingMessage_t;
    {$IFDEF STEAM}
    procedure InitRelayNetworkAccess();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetRelayNetworkStatus(pDetails: PSteamRelayNetworkStatus_t): ESteamNetworkingAvailability;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetLocalPingLocation(var aResult: SteamNetworkPingLocation_t): Single;
    {$ENDIF}
    {$IFDEF STEAM}
    function EstimatePingTimeBetweenTwoLocations(constref location1: SteamNetworkPingLocation_t; constref location2: SteamNetworkPingLocation_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function EstimatePingTimeFromLocalHost(constref remoteLocation: SteamNetworkPingLocation_t): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ConvertPingLocationToString(constref location: SteamNetworkPingLocation_t; pszBuf: PAnsiChar; cchBufSize: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    function ParsePingLocationString(const pszString: PAnsiChar; var aResult: SteamNetworkPingLocation_t): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CheckPingDataUpToDate(flMaxAgeSeconds: Single): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPingToDataCenter(popID: SteamNetworkingPOPID; pViaRelayPoP: PSteamNetworkingPOPID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetDirectPingToPOP(popID: SteamNetworkingPOPID): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPOPCount(): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPOPList(list: PSteamNetworkingPOPID; nListSz: Integer): Integer;
    {$ENDIF}
    function GetLocalTimestamp(): SteamNetworkingMicroseconds;
    procedure SetDebugOutputFunction(eDetailLevel: ESteamNetworkingSocketsDebugOutputType; pfnFunc: FSteamNetworkingSocketsDebugOutput);
    {$IFDEF STEAM}
    function IsFakeIPv4(nIPv4: UInt32): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetIPv4FakeIPType(nIPv4: UInt32): ESteamNetworkingFakeIPType;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetRealIdentityForFakeIP(constref fakeIP: SteamNetworkingIPAddr; pOutRealIdentity: PSteamNetworkingIdentity): EResult;
    {$ENDIF}
    function SetGlobalConfigValueInt32(eValue: ESteamNetworkingConfigValue; value: Int32): Boolean;
    function SetGlobalConfigValueFloat(eValue: ESteamNetworkingConfigValue; value: Single): Boolean;
    function SetGlobalConfigValueString(eValue: ESteamNetworkingConfigValue; const value: PAnsiChar): Boolean;
    function SetGlobalConfigValuePtr(eValue: ESteamNetworkingConfigValue; value: Pointer): Boolean;
    function SetConnectionConfigValueInt32(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; value: Int32): Boolean;
    function SetConnectionConfigValueFloat(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; value: Single): Boolean;
    function SetConnectionConfigValueString(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; const value: PAnsiChar): Boolean;
    function SetGlobalCallback_SteamNetConnectionStatusChanged(fnCallback: FnSteamNetConnectionStatusChanged): Boolean;
    function SetGlobalCallback_SteamNetAuthenticationStatusChanged(fnCallback: FnSteamNetAuthenticationStatusChanged): Boolean;
    function SetGlobalCallback_SteamRelayNetworkStatusChanged(fnCallback: FnSteamRelayNetworkStatusChanged): Boolean;
    {$IFDEF STEAM}
    function SetGlobalCallback_FakeIPResult(fnCallback: FnSteamNetworkingFakeIPResult): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetGlobalCallback_MessagesSessionRequest(fnCallback: FnSteamNetworkingMessagesSessionRequest): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetGlobalCallback_MessagesSessionFailed(fnCallback: FnSteamNetworkingMessagesSessionFailed): Boolean;
    {$ENDIF}
    function SetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr; eDataType: ESteamNetworkingConfigDataType; const pArg: Pointer): Boolean;
    function SetConfigValueStruct(constref opt: SteamNetworkingConfigValue_t; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr): Boolean;
    function GetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr; pOutDataType: PESteamNetworkingConfigDataType; pResult: Pointer; cbResult: Pcsize_t): ESteamNetworkingGetConfigValueResult;
    function GetConfigValueInfo(eValue: ESteamNetworkingConfigValue; pOutDataType: PESteamNetworkingConfigDataType; pOutScope: PESteamNetworkingConfigScope): PAnsiChar;
    function IterateGenericEditableConfigValues(eCurrent: ESteamNetworkingConfigValue; bEnumerateDevVars: Boolean): ESteamNetworkingConfigValue;
    {$IFDEF STEAM}
    procedure SteamNetworkingIPAddr_ToString(constref addr: SteamNetworkingIPAddr; buf: PAnsiChar; cbBuf: UInt32; bWithPort: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function SteamNetworkingIPAddr_ParseString(pAddr: PSteamNetworkingIPAddr; const pszStr: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SteamNetworkingIPAddr_GetFakeIPType(constref addr: SteamNetworkingIPAddr): ESteamNetworkingFakeIPType;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SteamNetworkingIdentity_ToString(constref identity: SteamNetworkingIdentity; buf: PAnsiChar; cbBuf: UInt32);
    {$ENDIF}
    {$IFDEF STEAM}
    function SteamNetworkingIdentity_ParseString(pIdentity: PSteamNetworkingIdentity; const pszStr: PAnsiChar): Boolean;
    {$ENDIF}
  end;

  ISteamGameServerHelper = record helper for ISteamGameServer
    {$IFDEF STEAM}
    procedure SetProduct(const pszProduct: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetGameDescription(const pszGameDescription: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetModDir(const pszModDir: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetDedicatedServer(bDedicated: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure LogOn(const pszToken: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure LogOnAnonymous();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure LogOff();
    {$ENDIF}
    {$IFDEF STEAM}
    function BLoggedOn(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function BSecure(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetSteamID(): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    function WasRestartRequested(): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetMaxPlayerCount(cPlayersMax: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetBotPlayerCount(cBotplayers: Integer);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetServerName(const pszServerName: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetMapName(const pszMapName: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetPasswordProtected(bPasswordProtected: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetSpectatorPort(unSpectatorPort: UInt16);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetSpectatorServerName(const pszSpectatorServerName: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ClearAllKeyValues();
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetKeyValue(const pKey: PAnsiChar; const pValue: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetGameTags(const pchGameTags: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetGameData(const pchGameData: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetRegion(const pszRegion: PAnsiChar);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SetAdvertiseServerActive(bActive: Boolean);
    {$ENDIF}
    {$IFDEF STEAM}
    function GetAuthSessionTicket(pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): HAuthTicket;
    {$ENDIF}
    {$IFDEF STEAM}
    function BeginAuthSession(const pAuthTicket: Pointer; cbAuthTicket: Integer; steamID: CSteamID): EBeginAuthSessionResult;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure EndAuthSession(steamID: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    procedure CancelAuthTicket(hAuthTicket: HAuthTicket);
    {$ENDIF}
    {$IFDEF STEAM}
    function UserHasLicenseForApp(steamID: CSteamID; appID: AppId_t): EUserHasLicenseForAppResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function RequestUserGroupStatus(steamIDUser: CSteamID; steamIDGroup: CSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure GetGameplayStats();
    {$ENDIF}
    {$IFDEF STEAM}
    function GetServerReputation(): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetPublicIP(): SteamIPAddress_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function HandleIncomingPacket(const pData: Pointer; cbData: Integer; srcIP: UInt32; srcPort: UInt16): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetNextOutgoingPacket(pOut: Pointer; cbMaxOut: Integer; pNetAdr: PUInt32; pPort: PUInt16): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    function AssociateWithClan(steamIDClan: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function ComputeNewPlayerCompatibility(steamIDNewPlayer: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function SendUserConnectAndAuthenticate_DEPRECATED(unIPClient: UInt32; const pvAuthBlob: Pointer; cubAuthBlobSize: UInt32; pSteamIDUser: PCSteamID): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function CreateUnauthenticatedUserConnection(): CSteamID;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure SendUserDisconnect_DEPRECATED(steamIDUser: CSteamID);
    {$ENDIF}
    {$IFDEF STEAM}
    function BUpdateUserData(steamIDUser: CSteamID; const pchPlayerName: PAnsiChar; uScore: UInt32): Boolean;
    {$ENDIF}
  end;

  ISteamGameServerStatsHelper = record helper for ISteamGameServerStats
    {$IFDEF STEAM}
    function RequestUserStats(steamIDUser: CSteamID): SteamAPICall_t;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PInt32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PSingle): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function GetUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar; pbAchieved: PBoolean): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; nData: Int32): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; fData: Single): Boolean; overload;
    {$ENDIF}
    {$IFDEF STEAM}
    function UpdateUserAvgRateStat(steamIDUser: CSteamID; const pchName: PAnsiChar; flCountThisSession: Single; dSessionLength: Double): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function SetUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function ClearUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar): Boolean;
    {$ENDIF}
    {$IFDEF STEAM}
    function StoreUserStats(steamIDUser: CSteamID): SteamAPICall_t;
    {$ENDIF}
  end;

  ISteamNetworkingFakeUDPPortHelper = record helper for ISteamNetworkingFakeUDPPort
    {$IFDEF STEAM}
    procedure DestroyFakeUDPPort();
    {$ENDIF}
    {$IFDEF STEAM}
    function SendMessageToFakeIP(constref remoteAddress: SteamNetworkingIPAddr; const pData: Pointer; cbData: UInt32; nSendFlags: Integer): EResult;
    {$ENDIF}
    {$IFDEF STEAM}
    function ReceiveMessages(ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
    {$ENDIF}
    {$IFDEF STEAM}
    procedure ScheduleCleanup(constref remoteAddress: SteamNetworkingIPAddr);
    {$ENDIF}
  end;

// SteamID3 string format [U:1:ID]
function SteamID3(SteamID: CSteamID): String;

function SteamAPI_Init(): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_Shutdown(); cdecl; external STEAMLIB;
function SteamAPI_RestartAppIfNecessary(unOwnAppID: UInt32): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ReleaseCurrentThreadMemory(); cdecl; external STEAMLIB;
procedure SteamAPI_WriteMiniDump(uStructuredExceptionCode: UInt32; pvExceptionInfo: Pointer; uBuildID: UInt32); cdecl; external STEAMLIB;
procedure SteamAPI_SetMiniDumpContent(const pchMsg: PAnsiChar); cdecl; external STEAMLIB;
procedure SteamAPI_ManualDispatch_Init(); cdecl; external STEAMLIB;
procedure SteamAPI_ManualDispatch_RunFrame(hSteamPipe: HSteamPipe); cdecl; external STEAMLIB;
function SteamAPI_ManualDispatch_GetNextCallback(hSteamPipe: HSteamPipe; pCallbackMsg: PCallbackMsg_t): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_ManualDispatch_FreeLastCallback(hSteamPipe: HSteamPipe); cdecl; external STEAMLIB;
function SteamAPI_ManualDispatch_GetAPICallResult(hSteamPipe: HSteamPipe; hSteamAPICall: SteamAPICall_t; pCallback: Pointer; cubCallback: Integer; iCallbackExpected: Integer; pbFailed: PBoolean): Boolean; cdecl; external STEAMLIB;
procedure SteamAPI_RunCallbacks(); cdecl; external STEAMLIB;
procedure SteamGameServer_RunCallbacks(); cdecl; external STEAMLIB;
function SteamAPI_GetHSteamPipe(): HSteamPipe; cdecl; external STEAMLIB;
function SteamAPI_GetHSteamUser(): HSteamUser; cdecl; external STEAMLIB;
function SteamGameServer_GetHSteamPipe(): HSteamPipe; cdecl; external STEAMLIB;
function SteamGameServer_GetHSteamUser(): HSteamUser; cdecl; external STEAMLIB;
function SteamInternal_GameServer_Init(unIP: UInt32; usLegacySteamPort: UInt16; usGamePort: UInt16; usQueryPort: UInt16; eServerMode: EServerMode; const pchVersionString: PAnsiChar): Boolean; cdecl; external STEAMLIB;
procedure SteamGameServer_Shutdown(); cdecl; external STEAMLIB;

{$IFDEF STEAM}
type
  TSteam = class
  public
    SteamPipeHandle: HSteamPipe;
    Utils: PISteamUtils;
    User: PISteamUser;
    Screenshots: PISTeamScreenshots;
    Friends: PISteamFriends;
    UGC: PISteamUGC;
    constructor Init();
  end;

  TSteamGS = class
  public
    SteamPipeHandle: HSteamPipe;
    Utils: PISteamUtils;
    GameServer: PISteamGameServer;
    GameServerStats: PISteamGameServerStats;
    UGC: PISteamUGC;
    constructor Init(unIP: uint32; usGamePort: uint16; usQueryPort: uint16; ServerMode: EServerMode; pchVersionString: PChar);
  end;
{$ENDIF}

implementation

function SteamID3(SteamID: CSteamID): String;
begin
  Result := '[U:' + IntToStr(Integer(SteamID.m_EUniverse)) + ':' + IntToStr(SteamID.m_unAccountID) + ']';
end;

{$IFDEF STEAM}
constructor TSteam.Init;
begin
  if not SteamAPI_Init() then
    raise Exception.Create('SteamAPI_Init has failed');

  SteamPipeHandle := SteamAPI_GetHSteamPipe();

  if SteamPipeHandle = 0 then
    raise Exception.Create('SteamPipeHandle is null');

  UGC := SteamAPI_SteamUGC_v016();
  Utils := SteamAPI_SteamUtils_v010();
  User := SteamAPI_SteamUser_v021();
  Screenshots := SteamAPI_SteamScreenshots_v003();
  Friends := SteamAPI_SteamFriends_v017();
end;

constructor TSteamGS.Init(unIP: uint32; usGamePort: uint16; usQueryPort: uint16; ServerMode: EServerMode; pchVersionString: PChar);
begin
  if not SteamInternal_GameServer_Init(unIP, 0, usGamePort, usQueryPort, ServerMode, pchVersionString) then
    raise Exception.Create('SteamAPI_GameServer_Init has failed');

  SteamPipeHandle := SteamGameServer_GetHSteamPipe();

  UGC := SteamAPI_SteamGameServerUGC_v016();
  Utils := SteamAPI_SteamGameServerUtils_v010();
  GameServer := SteamAPI_SteamGameServer_v014();
  GameServerStats := SteamAPI_SteamGameServerStats_v001();
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamIPAddress_tHelper.IsSet(): Boolean;
begin
  Result := SteamAPI_SteamIPAddress_t_IsSet(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure MatchMakingKeyValuePair_tHelper.Construct();
begin
  SteamAPI_MatchMakingKeyValuePair_t_Construct(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure servernetadr_tHelper.Construct();
begin
  SteamAPI_servernetadr_t_Construct(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure servernetadr_tHelper.Init(ip: UInt32; usQueryPort: UInt16; usConnectionPort: UInt16);
begin
  SteamAPI_servernetadr_t_Init(@Self, ip, usQueryPort, usConnectionPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function servernetadr_tHelper.GetQueryPort(): UInt16;
begin
  Result := SteamAPI_servernetadr_t_GetQueryPort(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure servernetadr_tHelper.SetQueryPort(usPort: UInt16);
begin
  SteamAPI_servernetadr_t_SetQueryPort(@Self, usPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function servernetadr_tHelper.GetConnectionPort(): UInt16;
begin
  Result := SteamAPI_servernetadr_t_GetConnectionPort(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure servernetadr_tHelper.SetConnectionPort(usPort: UInt16);
begin
  SteamAPI_servernetadr_t_SetConnectionPort(@Self, usPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function servernetadr_tHelper.GetIP(): UInt32;
begin
  Result := SteamAPI_servernetadr_t_GetIP(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure servernetadr_tHelper.SetIP(unIP: UInt32);
begin
  SteamAPI_servernetadr_t_SetIP(@Self, unIP);
end;
{$ENDIF}

{$IFDEF STEAM}
function servernetadr_tHelper.GetConnectionAddressString(): PAnsiChar;
begin
  Result := SteamAPI_servernetadr_t_GetConnectionAddressString(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function servernetadr_tHelper.GetQueryAddressString(): PAnsiChar;
begin
  Result := SteamAPI_servernetadr_t_GetQueryAddressString(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure gameserveritem_tHelper.Construct();
begin
  SteamAPI_gameserveritem_t_Construct(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function gameserveritem_tHelper.GetName(): PAnsiChar;
begin
  Result := SteamAPI_gameserveritem_t_GetName(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure gameserveritem_tHelper.SetName(const pName: PAnsiChar);
begin
  SteamAPI_gameserveritem_t_SetName(@Self, pName);
end;
{$ENDIF}

procedure SteamNetworkingIPAddrHelper.Clear();
begin
  SteamAPI_SteamNetworkingIPAddr_Clear(@Self);
end;

function SteamNetworkingIPAddrHelper.IsIPv6AllZeros(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_IsIPv6AllZeros(@Self);
end;

procedure SteamNetworkingIPAddrHelper.SetIPv6(const ipv6: PUInt8; nPort: UInt16);
begin
  SteamAPI_SteamNetworkingIPAddr_SetIPv6(@Self, ipv6, nPort);
end;

procedure SteamNetworkingIPAddrHelper.SetIPv4(nIP: UInt32; nPort: UInt16);
begin
  SteamAPI_SteamNetworkingIPAddr_SetIPv4(@Self, nIP, nPort);
end;

function SteamNetworkingIPAddrHelper.IsIPv4(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_IsIPv4(@Self);
end;

function SteamNetworkingIPAddrHelper.GetIPv4(): UInt32;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_GetIPv4(@Self);
end;

procedure SteamNetworkingIPAddrHelper.SetIPv6LocalHost(nPort: UInt16);
begin
  SteamAPI_SteamNetworkingIPAddr_SetIPv6LocalHost(@Self, nPort);
end;

function SteamNetworkingIPAddrHelper.IsLocalHost(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_IsLocalHost(@Self);
end;

procedure SteamNetworkingIPAddrHelper.ToString(buf: PAnsiChar; cbBuf: UInt32; bWithPort: Boolean);
begin
  SteamAPI_SteamNetworkingIPAddr_ToString(@Self, buf, cbBuf, bWithPort);
end;

function SteamNetworkingIPAddrHelper.ParseString(const pszStr: PAnsiChar): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_ParseString(@Self, pszStr);
end;

{$IFDEF STEAM}
function SteamNetworkingIPAddrHelper.GetFakeIPType(): ESteamNetworkingFakeIPType;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_GetFakeIPType(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamNetworkingIPAddrHelper.IsFakeIP(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIPAddr_IsFakeIP(@Self);
end;
{$ENDIF}

procedure SteamNetworkingIdentityHelper.Clear();
begin
  SteamAPI_SteamNetworkingIdentity_Clear(@Self);
end;

function SteamNetworkingIdentityHelper.IsInvalid(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_IsInvalid(@Self);
end;

procedure SteamNetworkingIdentityHelper.SetSteamID(steamID: CSteamID);
begin
  SteamAPI_SteamNetworkingIdentity_SetSteamID(@Self, steamID);
end;

function SteamNetworkingIdentityHelper.GetSteamID(): CSteamID;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetSteamID(@Self);
end;

procedure SteamNetworkingIdentityHelper.SetSteamID64(steamID: UInt64);
begin
  SteamAPI_SteamNetworkingIdentity_SetSteamID64(@Self, steamID);
end;

function SteamNetworkingIdentityHelper.GetSteamID64(): UInt64;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetSteamID64(@Self);
end;

{$IFDEF STEAM}
function SteamNetworkingIdentityHelper.SetXboxPairwiseID(const pszString: PAnsiChar): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_SetXboxPairwiseID(@Self, pszString);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamNetworkingIdentityHelper.GetXboxPairwiseID(): PAnsiChar;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetXboxPairwiseID(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamNetworkingIdentityHelper.SetPSNID(id: UInt64);
begin
  SteamAPI_SteamNetworkingIdentity_SetPSNID(@Self, id);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamNetworkingIdentityHelper.GetPSNID(): UInt64;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetPSNID(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamNetworkingIdentityHelper.SetStadiaID(id: UInt64);
begin
  SteamAPI_SteamNetworkingIdentity_SetStadiaID(@Self, id);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamNetworkingIdentityHelper.GetStadiaID(): UInt64;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetStadiaID(@Self);
end;
{$ENDIF}

procedure SteamNetworkingIdentityHelper.SetIPAddr(constref addr: SteamNetworkingIPAddr);
begin
  SteamAPI_SteamNetworkingIdentity_SetIPAddr(@Self, @addr);
end;

function SteamNetworkingIdentityHelper.GetIPAddr(): PSteamNetworkingIPAddr;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetIPAddr(@Self);
end;

{$IFDEF STEAM}
procedure SteamNetworkingIdentityHelper.SetIPv4Addr(nIPv4: UInt32; nPort: UInt16);
begin
  SteamAPI_SteamNetworkingIdentity_SetIPv4Addr(@Self, nIPv4, nPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamNetworkingIdentityHelper.GetIPv4(): UInt32;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetIPv4(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamNetworkingIdentityHelper.GetFakeIPType(): ESteamNetworkingFakeIPType;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetFakeIPType(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamNetworkingIdentityHelper.IsFakeIP(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_IsFakeIP(@Self);
end;
{$ENDIF}

procedure SteamNetworkingIdentityHelper.SetLocalHost();
begin
  SteamAPI_SteamNetworkingIdentity_SetLocalHost(@Self);
end;

function SteamNetworkingIdentityHelper.IsLocalHost(): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_IsLocalHost(@Self);
end;

function SteamNetworkingIdentityHelper.SetGenericString(const pszString: PAnsiChar): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_SetGenericString(@Self, pszString);
end;

function SteamNetworkingIdentityHelper.GetGenericString(): PAnsiChar;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetGenericString(@Self);
end;

function SteamNetworkingIdentityHelper.SetGenericBytes(const data: Pointer; cbLen: UInt32): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_SetGenericBytes(@Self, data, cbLen);
end;

function SteamNetworkingIdentityHelper.GetGenericBytes(var cbLen: Integer): PUInt8;
begin
  Result := SteamAPI_SteamNetworkingIdentity_GetGenericBytes(@Self, @cbLen);
end;

procedure SteamNetworkingIdentityHelper.ToString(buf: PAnsiChar; cbBuf: UInt32);
begin
  SteamAPI_SteamNetworkingIdentity_ToString(@Self, buf, cbBuf);
end;

function SteamNetworkingIdentityHelper.ParseString(const pszStr: PAnsiChar): Boolean;
begin
  Result := SteamAPI_SteamNetworkingIdentity_ParseString(@Self, pszStr);
end;

procedure SteamNetworkingMessage_tHelper.Release();
begin
  SteamAPI_SteamNetworkingMessage_t_Release(@Self);
end;

{$IFDEF STEAM}
procedure SteamNetworkingConfigValue_tHelper.SetInt32(eVal: ESteamNetworkingConfigValue; data: Int32);
begin
  SteamAPI_SteamNetworkingConfigValue_t_SetInt32(@Self, eVal, data);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamNetworkingConfigValue_tHelper.SetInt64(eVal: ESteamNetworkingConfigValue; data: Int64);
begin
  SteamAPI_SteamNetworkingConfigValue_t_SetInt64(@Self, eVal, data);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamNetworkingConfigValue_tHelper.SetFloat(eVal: ESteamNetworkingConfigValue; data: Single);
begin
  SteamAPI_SteamNetworkingConfigValue_t_SetFloat(@Self, eVal, data);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamNetworkingConfigValue_tHelper.SetPtr(eVal: ESteamNetworkingConfigValue; data: Pointer);
begin
  SteamAPI_SteamNetworkingConfigValue_t_SetPtr(@Self, eVal, data);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamNetworkingConfigValue_tHelper.SetString(eVal: ESteamNetworkingConfigValue; const data: PAnsiChar);
begin
  SteamAPI_SteamNetworkingConfigValue_t_SetString(@Self, eVal, data);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamDatagramHostedAddressHelper.Clear();
begin
  SteamAPI_SteamDatagramHostedAddress_Clear(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function SteamDatagramHostedAddressHelper.GetPopID(): SteamNetworkingPOPID;
begin
  Result := SteamAPI_SteamDatagramHostedAddress_GetPopID(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure SteamDatagramHostedAddressHelper.SetDevAddress(nIP: UInt32; nPort: UInt16; popid: SteamNetworkingPOPID);
begin
  SteamAPI_SteamDatagramHostedAddress_SetDevAddress(@Self, nIP, nPort, popid);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.CreateSteamPipe(): HSteamPipe;
begin
  Result := SteamAPI_ISteamClient_CreateSteamPipe(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.BReleaseSteamPipe(hSteamPipe: HSteamPipe): Boolean;
begin
  Result := SteamAPI_ISteamClient_BReleaseSteamPipe(@Self, hSteamPipe);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.ConnectToGlobalUser(hSteamPipe: HSteamPipe): HSteamUser;
begin
  Result := SteamAPI_ISteamClient_ConnectToGlobalUser(@Self, hSteamPipe);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.CreateLocalUser(phSteamPipe: PHSteamPipe; eAccountType: EAccountType): HSteamUser;
begin
  Result := SteamAPI_ISteamClient_CreateLocalUser(@Self, phSteamPipe, eAccountType);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamClientHelper.ReleaseUser(hSteamPipe: HSteamPipe; hUser: HSteamUser);
begin
  SteamAPI_ISteamClient_ReleaseUser(@Self, hSteamPipe, hUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamUser(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUser;
begin
  Result := SteamAPI_ISteamClient_GetISteamUser(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamGameServer(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamGameServer;
begin
  Result := SteamAPI_ISteamClient_GetISteamGameServer(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamClientHelper.SetLocalIPBinding(constref unIP: SteamIPAddress_t; usPort: UInt16);
begin
  SteamAPI_ISteamClient_SetLocalIPBinding(@Self, @unIP, usPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamFriends(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamFriends;
begin
  Result := SteamAPI_ISteamClient_GetISteamFriends(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamUtils(hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUtils;
begin
  Result := SteamAPI_ISteamClient_GetISteamUtils(@Self, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamMatchmaking(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMatchmaking;
begin
  Result := SteamAPI_ISteamClient_GetISteamMatchmaking(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamMatchmakingServers(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMatchmakingServers;
begin
  Result := SteamAPI_ISteamClient_GetISteamMatchmakingServers(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamGenericInterface(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): Pointer;
begin
  Result := SteamAPI_ISteamClient_GetISteamGenericInterface(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamUserStats(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUserStats;
begin
  Result := SteamAPI_ISteamClient_GetISteamUserStats(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamGameServerStats(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamGameServerStats;
begin
  Result := SteamAPI_ISteamClient_GetISteamGameServerStats(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamApps(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamApps;
begin
  Result := SteamAPI_ISteamClient_GetISteamApps(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamNetworking(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamNetworking;
begin
  Result := SteamAPI_ISteamClient_GetISteamNetworking(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamRemoteStorage(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamRemoteStorage;
begin
  Result := SteamAPI_ISteamClient_GetISteamRemoteStorage(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamScreenshots(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamScreenshots;
begin
  Result := SteamAPI_ISteamClient_GetISteamScreenshots(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamGameSearch(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamGameSearch;
begin
  Result := SteamAPI_ISteamClient_GetISteamGameSearch(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetIPCCallCount(): UInt32;
begin
  Result := SteamAPI_ISteamClient_GetIPCCallCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamClientHelper.SetWarningMessageHook(pFunction: SteamAPIWarningMessageHook_t);
begin
  SteamAPI_ISteamClient_SetWarningMessageHook(@Self, pFunction);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.BShutdownIfAllPipesClosed(): Boolean;
begin
  Result := SteamAPI_ISteamClient_BShutdownIfAllPipesClosed(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamHTTP(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamHTTP;
begin
  Result := SteamAPI_ISteamClient_GetISteamHTTP(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamController(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamController;
begin
  Result := SteamAPI_ISteamClient_GetISteamController(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamUGC(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamUGC;
begin
  Result := SteamAPI_ISteamClient_GetISteamUGC(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamAppList(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamAppList;
begin
  Result := SteamAPI_ISteamClient_GetISteamAppList(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamMusic(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMusic;
begin
  Result := SteamAPI_ISteamClient_GetISteamMusic(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamMusicRemote(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamMusicRemote;
begin
  Result := SteamAPI_ISteamClient_GetISteamMusicRemote(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamHTMLSurface(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamHTMLSurface;
begin
  Result := SteamAPI_ISteamClient_GetISteamHTMLSurface(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamInventory(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamInventory;
begin
  Result := SteamAPI_ISteamClient_GetISteamInventory(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamVideo(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamVideo;
begin
  Result := SteamAPI_ISteamClient_GetISteamVideo(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamParentalSettings(hSteamuser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamParentalSettings;
begin
  Result := SteamAPI_ISteamClient_GetISteamParentalSettings(@Self, hSteamuser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamInput(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamInput;
begin
  Result := SteamAPI_ISteamClient_GetISteamInput(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamParties(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamParties;
begin
  Result := SteamAPI_ISteamClient_GetISteamParties(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamClientHelper.GetISteamRemotePlay(hSteamUser: HSteamUser; hSteamPipe: HSteamPipe; const pchVersion: PAnsiChar): PISteamRemotePlay;
begin
  Result := SteamAPI_ISteamClient_GetISteamRemotePlay(@Self, hSteamUser, hSteamPipe, pchVersion);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetHSteamUser(): HSteamUser;
begin
  Result := SteamAPI_ISteamUser_GetHSteamUser(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BLoggedOn(): Boolean;
begin
  Result := SteamAPI_ISteamUser_BLoggedOn(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetSteamID(): CSteamID;
begin
  Result := SteamAPI_ISteamUser_GetSteamID(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.InitiateGameConnection_DEPRECATED(pAuthBlob: Pointer; cbMaxAuthBlob: Integer; steamIDGameServer: CSteamID; unIPServer: UInt32; usPortServer: UInt16; bSecure: Boolean): Integer;
begin
  Result := SteamAPI_ISteamUser_InitiateGameConnection_DEPRECATED(@Self, pAuthBlob, cbMaxAuthBlob, steamIDGameServer, unIPServer, usPortServer, bSecure);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUserHelper.TerminateGameConnection_DEPRECATED(unIPServer: UInt32; usPortServer: UInt16);
begin
  SteamAPI_ISteamUser_TerminateGameConnection_DEPRECATED(@Self, unIPServer, usPortServer);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUserHelper.TrackAppUsageEvent(gameID: CGameID; eAppUsageEvent: Integer; const pchExtraInfo: PAnsiChar);
begin
  SteamAPI_ISteamUser_TrackAppUsageEvent(@Self, gameID, eAppUsageEvent, pchExtraInfo);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetUserDataFolder(pchBuffer: PAnsiChar; cubBuffer: Integer): Boolean;
begin
  Result := SteamAPI_ISteamUser_GetUserDataFolder(@Self, pchBuffer, cubBuffer);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUserHelper.StartVoiceRecording();
begin
  SteamAPI_ISteamUser_StartVoiceRecording(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUserHelper.StopVoiceRecording();
begin
  SteamAPI_ISteamUser_StopVoiceRecording(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetAvailableVoice(pcbCompressed: PUInt32; pcbUncompressed_Deprecated: PUInt32; nUncompressedVoiceDesiredSampleRate_Deprecated: UInt32): EVoiceResult;
begin
  Result := SteamAPI_ISteamUser_GetAvailableVoice(@Self, pcbCompressed, pcbUncompressed_Deprecated, nUncompressedVoiceDesiredSampleRate_Deprecated);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetVoice(bWantCompressed: Boolean; pDestBuffer: Pointer; cbDestBufferSize: UInt32; nBytesWritten: PUInt32; bWantUncompressed_Deprecated: Boolean; pUncompressedDestBuffer_Deprecated: Pointer; cbUncompressedDestBufferSize_Deprecated: UInt32; nUncompressBytesWritten_Deprecated: PUInt32; nUncompressedVoiceDesiredSampleRate_Deprecated: UInt32): EVoiceResult;
begin
  Result := SteamAPI_ISteamUser_GetVoice(@Self, bWantCompressed, pDestBuffer, cbDestBufferSize, nBytesWritten, bWantUncompressed_Deprecated, pUncompressedDestBuffer_Deprecated, cbUncompressedDestBufferSize_Deprecated, nUncompressBytesWritten_Deprecated, nUncompressedVoiceDesiredSampleRate_Deprecated);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.DecompressVoice(const pCompressed: Pointer; cbCompressed: UInt32; pDestBuffer: Pointer; cbDestBufferSize: UInt32; nBytesWritten: PUInt32; nDesiredSampleRate: UInt32): EVoiceResult;
begin
  Result := SteamAPI_ISteamUser_DecompressVoice(@Self, pCompressed, cbCompressed, pDestBuffer, cbDestBufferSize, nBytesWritten, nDesiredSampleRate);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetVoiceOptimalSampleRate(): UInt32;
begin
  Result := SteamAPI_ISteamUser_GetVoiceOptimalSampleRate(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetAuthSessionTicket(pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): HAuthTicket;
begin
  Result := SteamAPI_ISteamUser_GetAuthSessionTicket(@Self, pTicket, cbMaxTicket, pcbTicket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BeginAuthSession(const pAuthTicket: Pointer; cbAuthTicket: Integer; steamID: CSteamID): EBeginAuthSessionResult;
begin
  Result := SteamAPI_ISteamUser_BeginAuthSession(@Self, pAuthTicket, cbAuthTicket, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUserHelper.EndAuthSession(steamID: CSteamID);
begin
  SteamAPI_ISteamUser_EndAuthSession(@Self, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUserHelper.CancelAuthTicket(hAuthTicket: HAuthTicket);
begin
  SteamAPI_ISteamUser_CancelAuthTicket(@Self, hAuthTicket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.UserHasLicenseForApp(steamID: CSteamID; appID: AppId_t): EUserHasLicenseForAppResult;
begin
  Result := SteamAPI_ISteamUser_UserHasLicenseForApp(@Self, steamID, appID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BIsBehindNAT(): Boolean;
begin
  Result := SteamAPI_ISteamUser_BIsBehindNAT(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUserHelper.AdvertiseGame(steamIDGameServer: CSteamID; unIPServer: UInt32; usPortServer: UInt16);
begin
  SteamAPI_ISteamUser_AdvertiseGame(@Self, steamIDGameServer, unIPServer, usPortServer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.RequestEncryptedAppTicket(pDataToInclude: Pointer; cbDataToInclude: Integer): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUser_RequestEncryptedAppTicket(@Self, pDataToInclude, cbDataToInclude);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetEncryptedAppTicket(pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamUser_GetEncryptedAppTicket(@Self, pTicket, cbMaxTicket, pcbTicket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetGameBadgeLevel(nSeries: Integer; bFoil: Boolean): Integer;
begin
  Result := SteamAPI_ISteamUser_GetGameBadgeLevel(@Self, nSeries, bFoil);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetPlayerSteamLevel(): Integer;
begin
  Result := SteamAPI_ISteamUser_GetPlayerSteamLevel(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.RequestStoreAuthURL(const pchRedirectURL: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUser_RequestStoreAuthURL(@Self, pchRedirectURL);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BIsPhoneVerified(): Boolean;
begin
  Result := SteamAPI_ISteamUser_BIsPhoneVerified(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BIsTwoFactorEnabled(): Boolean;
begin
  Result := SteamAPI_ISteamUser_BIsTwoFactorEnabled(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BIsPhoneIdentifying(): Boolean;
begin
  Result := SteamAPI_ISteamUser_BIsPhoneIdentifying(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BIsPhoneRequiringVerification(): Boolean;
begin
  Result := SteamAPI_ISteamUser_BIsPhoneRequiringVerification(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetMarketEligibility(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUser_GetMarketEligibility(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.GetDurationControl(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUser_GetDurationControl(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserHelper.BSetDurationControlOnlineState(eNewState: EDurationControlOnlineState): Boolean;
begin
  Result := SteamAPI_ISteamUser_BSetDurationControlOnlineState(@Self, eNewState);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetPersonaName(): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetPersonaName(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.SetPersonaName(const pchPersonaName: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamFriends_SetPersonaName(@Self, pchPersonaName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetPersonaState(): EPersonaState;
begin
  Result := SteamAPI_ISteamFriends_GetPersonaState(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendCount(iFriendFlags: Integer): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendCount(@Self, iFriendFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendByIndex(iFriend: Integer; iFriendFlags: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamFriends_GetFriendByIndex(@Self, iFriend, iFriendFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendRelationship(steamIDFriend: CSteamID): EFriendRelationship;
begin
  Result := SteamAPI_ISteamFriends_GetFriendRelationship(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendPersonaState(steamIDFriend: CSteamID): EPersonaState;
begin
  Result := SteamAPI_ISteamFriends_GetFriendPersonaState(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendPersonaName(steamIDFriend: CSteamID): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetFriendPersonaName(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendGamePlayed(steamIDFriend: CSteamID; pFriendGameInfo: PFriendGameInfo_t): Boolean;
begin
  Result := SteamAPI_ISteamFriends_GetFriendGamePlayed(@Self, steamIDFriend, pFriendGameInfo);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendPersonaNameHistory(steamIDFriend: CSteamID; iPersonaName: Integer): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetFriendPersonaNameHistory(@Self, steamIDFriend, iPersonaName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendSteamLevel(steamIDFriend: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendSteamLevel(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetPlayerNickname(steamIDPlayer: CSteamID): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetPlayerNickname(@Self, steamIDPlayer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendsGroupCount(): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendsGroupCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendsGroupIDByIndex(iFG: Integer): FriendsGroupID_t;
begin
  Result := SteamAPI_ISteamFriends_GetFriendsGroupIDByIndex(@Self, iFG);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendsGroupName(friendsGroupID: FriendsGroupID_t): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetFriendsGroupName(@Self, friendsGroupID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendsGroupMembersCount(friendsGroupID: FriendsGroupID_t): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendsGroupMembersCount(@Self, friendsGroupID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.GetFriendsGroupMembersList(friendsGroupID: FriendsGroupID_t; pOutSteamIDMembers: PCSteamID; nMembersCount: Integer);
begin
  SteamAPI_ISteamFriends_GetFriendsGroupMembersList(@Self, friendsGroupID, pOutSteamIDMembers, nMembersCount);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.HasFriend(steamIDFriend: CSteamID; iFriendFlags: Integer): Boolean;
begin
  Result := SteamAPI_ISteamFriends_HasFriend(@Self, steamIDFriend, iFriendFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanCount(): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetClanCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanByIndex(iClan: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamFriends_GetClanByIndex(@Self, iClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanName(steamIDClan: CSteamID): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetClanName(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanTag(steamIDClan: CSteamID): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetClanTag(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanActivityCounts(steamIDClan: CSteamID; pnOnline: PInteger; pnInGame: PInteger; pnChatting: PInteger): Boolean;
begin
  Result := SteamAPI_ISteamFriends_GetClanActivityCounts(@Self, steamIDClan, pnOnline, pnInGame, pnChatting);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.DownloadClanActivityCounts(psteamIDClans: PCSteamID; cClansToRequest: Integer): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamFriends_DownloadClanActivityCounts(@Self, psteamIDClans, cClansToRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendCountFromSource(steamIDSource: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendCountFromSource(@Self, steamIDSource);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendFromSourceByIndex(steamIDSource: CSteamID; iFriend: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamFriends_GetFriendFromSourceByIndex(@Self, steamIDSource, iFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.IsUserInSource(steamIDUser: CSteamID; steamIDSource: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_IsUserInSource(@Self, steamIDUser, steamIDSource);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.SetInGameVoiceSpeaking(steamIDUser: CSteamID; bSpeaking: Boolean);
begin
  SteamAPI_ISteamFriends_SetInGameVoiceSpeaking(@Self, steamIDUser, bSpeaking);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ActivateGameOverlay(const pchDialog: PAnsiChar);
begin
  SteamAPI_ISteamFriends_ActivateGameOverlay(@Self, pchDialog);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ActivateGameOverlayToUser(const pchDialog: PAnsiChar; steamID: CSteamID);
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayToUser(@Self, pchDialog, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ActivateGameOverlayToWebPage(const pchURL: PAnsiChar; eMode: EActivateGameOverlayToWebPageMode);
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage(@Self, pchURL, eMode);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ActivateGameOverlayToStore(nAppID: AppId_t; eFlag: EOverlayToStoreFlag);
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayToStore(@Self, nAppID, eFlag);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.SetPlayedWith(steamIDUserPlayedWith: CSteamID);
begin
  SteamAPI_ISteamFriends_SetPlayedWith(@Self, steamIDUserPlayedWith);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ActivateGameOverlayInviteDialog(steamIDLobby: CSteamID);
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialog(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetSmallFriendAvatar(steamIDFriend: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetSmallFriendAvatar(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetMediumFriendAvatar(steamIDFriend: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetMediumFriendAvatar(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetLargeFriendAvatar(steamIDFriend: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetLargeFriendAvatar(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.RequestUserInformation(steamIDUser: CSteamID; bRequireNameOnly: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamFriends_RequestUserInformation(@Self, steamIDUser, bRequireNameOnly);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.RequestClanOfficerList(steamIDClan: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamFriends_RequestClanOfficerList(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanOwner(steamIDClan: CSteamID): CSteamID;
begin
  Result := SteamAPI_ISteamFriends_GetClanOwner(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanOfficerCount(steamIDClan: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetClanOfficerCount(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanOfficerByIndex(steamIDClan: CSteamID; iOfficer: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamFriends_GetClanOfficerByIndex(@Self, steamIDClan, iOfficer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetUserRestrictions(): UInt32;
begin
  Result := SteamAPI_ISteamFriends_GetUserRestrictions(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.SetRichPresence(const pchKey: PAnsiChar; const pchValue: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamFriends_SetRichPresence(@Self, pchKey, pchValue);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ClearRichPresence();
begin
  SteamAPI_ISteamFriends_ClearRichPresence(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendRichPresence(steamIDFriend: CSteamID; const pchKey: PAnsiChar): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetFriendRichPresence(@Self, steamIDFriend, pchKey);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendRichPresenceKeyCount(steamIDFriend: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendRichPresenceKeyCount(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendRichPresenceKeyByIndex(steamIDFriend: CSteamID; iKey: Integer): PAnsiChar;
begin
  Result := SteamAPI_ISteamFriends_GetFriendRichPresenceKeyByIndex(@Self, steamIDFriend, iKey);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.RequestFriendRichPresence(steamIDFriend: CSteamID);
begin
  SteamAPI_ISteamFriends_RequestFriendRichPresence(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.InviteUserToGame(steamIDFriend: CSteamID; const pchConnectString: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamFriends_InviteUserToGame(@Self, steamIDFriend, pchConnectString);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetCoplayFriendCount(): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetCoplayFriendCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetCoplayFriend(iCoplayFriend: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamFriends_GetCoplayFriend(@Self, iCoplayFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendCoplayTime(steamIDFriend: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendCoplayTime(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendCoplayGame(steamIDFriend: CSteamID): AppId_t;
begin
  Result := SteamAPI_ISteamFriends_GetFriendCoplayGame(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.JoinClanChatRoom(steamIDClan: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamFriends_JoinClanChatRoom(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.LeaveClanChatRoom(steamIDClan: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_LeaveClanChatRoom(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanChatMemberCount(steamIDClan: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetClanChatMemberCount(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetChatMemberByIndex(steamIDClan: CSteamID; iUser: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamFriends_GetChatMemberByIndex(@Self, steamIDClan, iUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.SendClanChatMessage(steamIDClanChat: CSteamID; const pchText: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamFriends_SendClanChatMessage(@Self, steamIDClanChat, pchText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetClanChatMessage(steamIDClanChat: CSteamID; iMessage: Integer; prgchText: Pointer; cchTextMax: Integer; peChatEntryType: PEChatEntryType; psteamidChatter: PCSteamID): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetClanChatMessage(@Self, steamIDClanChat, iMessage, prgchText, cchTextMax, peChatEntryType, psteamidChatter);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.IsClanChatAdmin(steamIDClanChat: CSteamID; steamIDUser: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_IsClanChatAdmin(@Self, steamIDClanChat, steamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.IsClanChatWindowOpenInSteam(steamIDClanChat: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_IsClanChatWindowOpenInSteam(@Self, steamIDClanChat);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.OpenClanChatWindowInSteam(steamIDClanChat: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_OpenClanChatWindowInSteam(@Self, steamIDClanChat);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.CloseClanChatWindowInSteam(steamIDClanChat: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_CloseClanChatWindowInSteam(@Self, steamIDClanChat);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.SetListenForFriendsMessages(bInterceptEnabled: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamFriends_SetListenForFriendsMessages(@Self, bInterceptEnabled);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.ReplyToFriendMessage(steamIDFriend: CSteamID; const pchMsgToSend: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamFriends_ReplyToFriendMessage(@Self, steamIDFriend, pchMsgToSend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFriendMessage(steamIDFriend: CSteamID; iMessageID: Integer; pvData: Pointer; cubData: Integer; peChatEntryType: PEChatEntryType): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetFriendMessage(@Self, steamIDFriend, iMessageID, pvData, cubData, peChatEntryType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetFollowerCount(steamID: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamFriends_GetFollowerCount(@Self, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.IsFollowing(steamID: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamFriends_IsFollowing(@Self, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.EnumerateFollowingList(unStartIndex: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamFriends_EnumerateFollowingList(@Self, unStartIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.IsClanPublic(steamIDClan: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_IsClanPublic(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.IsClanOfficialGameGroup(steamIDClan: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamFriends_IsClanOfficialGameGroup(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.GetNumChatsWithUnreadPriorityMessages(): Integer;
begin
  Result := SteamAPI_ISteamFriends_GetNumChatsWithUnreadPriorityMessages(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ActivateGameOverlayRemotePlayTogetherInviteDialog(steamIDLobby: CSteamID);
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayRemotePlayTogetherInviteDialog(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamFriendsHelper.RegisterProtocolInOverlayBrowser(const pchProtocol: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamFriends_RegisterProtocolInOverlayBrowser(@Self, pchProtocol);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamFriendsHelper.ActivateGameOverlayInviteDialogConnectString(const pchConnectString: PAnsiChar);
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialogConnectString(@Self, pchConnectString);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetSecondsSinceAppActive(): UInt32;
begin
  Result := SteamAPI_ISteamUtils_GetSecondsSinceAppActive(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetSecondsSinceComputerActive(): UInt32;
begin
  Result := SteamAPI_ISteamUtils_GetSecondsSinceComputerActive(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetConnectedUniverse(): EUniverse;
begin
  Result := SteamAPI_ISteamUtils_GetConnectedUniverse(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetServerRealTime(): UInt32;
begin
  Result := SteamAPI_ISteamUtils_GetServerRealTime(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetIPCountry(): PAnsiChar;
begin
  Result := SteamAPI_ISteamUtils_GetIPCountry(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetImageSize(iImage: Integer; pnWidth: PUInt32; pnHeight: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamUtils_GetImageSize(@Self, iImage, pnWidth, pnHeight);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetImageRGBA(iImage: Integer; pubDest: PUInt8; nDestBufferSize: Integer): Boolean;
begin
  Result := SteamAPI_ISteamUtils_GetImageRGBA(@Self, iImage, pubDest, nDestBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetCurrentBatteryPower(): UInt8;
begin
  Result := SteamAPI_ISteamUtils_GetCurrentBatteryPower(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetAppID(): UInt32;
begin
  Result := SteamAPI_ISteamUtils_GetAppID(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUtilsHelper.SetOverlayNotificationPosition(eNotificationPosition: ENotificationPosition);
begin
  SteamAPI_ISteamUtils_SetOverlayNotificationPosition(@Self, eNotificationPosition);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.IsAPICallCompleted(hSteamAPICall: SteamAPICall_t; pbFailed: PBoolean): Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsAPICallCompleted(@Self, hSteamAPICall, pbFailed);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetAPICallFailureReason(hSteamAPICall: SteamAPICall_t): ESteamAPICallFailure;
begin
  Result := SteamAPI_ISteamUtils_GetAPICallFailureReason(@Self, hSteamAPICall);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetAPICallResult(hSteamAPICall: SteamAPICall_t; pCallback: Pointer; cubCallback: Integer; iCallbackExpected: Integer; pbFailed: PBoolean): Boolean;
begin
  Result := SteamAPI_ISteamUtils_GetAPICallResult(@Self, hSteamAPICall, pCallback, cubCallback, iCallbackExpected, pbFailed);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetIPCCallCount(): UInt32;
begin
  Result := SteamAPI_ISteamUtils_GetIPCCallCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUtilsHelper.SetWarningMessageHook(pFunction: SteamAPIWarningMessageHook_t);
begin
  SteamAPI_ISteamUtils_SetWarningMessageHook(@Self, pFunction);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.IsOverlayEnabled(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsOverlayEnabled(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.BOverlayNeedsPresent(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_BOverlayNeedsPresent(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.CheckFileSignature(const szFileName: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUtils_CheckFileSignature(@Self, szFileName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.ShowGamepadTextInput(eInputMode: EGamepadTextInputMode; eLineInputMode: EGamepadTextInputLineMode; const pchDescription: PAnsiChar; unCharMax: UInt32; const pchExistingText: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUtils_ShowGamepadTextInput(@Self, eInputMode, eLineInputMode, pchDescription, unCharMax, pchExistingText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetEnteredGamepadTextLength(): UInt32;
begin
  Result := SteamAPI_ISteamUtils_GetEnteredGamepadTextLength(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetEnteredGamepadTextInput(pchText: PAnsiChar; cchText: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUtils_GetEnteredGamepadTextInput(@Self, pchText, cchText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetSteamUILanguage(): PAnsiChar;
begin
  Result := SteamAPI_ISteamUtils_GetSteamUILanguage(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.IsSteamRunningInVR(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsSteamRunningInVR(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUtilsHelper.SetOverlayNotificationInset(nHorizontalInset: Integer; nVerticalInset: Integer);
begin
  SteamAPI_ISteamUtils_SetOverlayNotificationInset(@Self, nHorizontalInset, nVerticalInset);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.IsSteamInBigPictureMode(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsSteamInBigPictureMode(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUtilsHelper.StartVRDashboard();
begin
  SteamAPI_ISteamUtils_StartVRDashboard(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.IsVRHeadsetStreamingEnabled(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsVRHeadsetStreamingEnabled(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUtilsHelper.SetVRHeadsetStreamingEnabled(bEnabled: Boolean);
begin
  SteamAPI_ISteamUtils_SetVRHeadsetStreamingEnabled(@Self, bEnabled);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.IsSteamChinaLauncher(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsSteamChinaLauncher(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.InitFilterText(unFilterOptions: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUtils_InitFilterText(@Self, unFilterOptions);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.FilterText(eContext: ETextFilteringContext; sourceSteamID: CSteamID; const pchInputMessage: PAnsiChar; pchOutFilteredText: PAnsiChar; nByteSizeOutFilteredText: UInt32): Integer;
begin
  Result := SteamAPI_ISteamUtils_FilterText(@Self, eContext, sourceSteamID, pchInputMessage, pchOutFilteredText, nByteSizeOutFilteredText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.GetIPv6ConnectivityState(eProtocol: ESteamIPv6ConnectivityProtocol): ESteamIPv6ConnectivityState;
begin
  Result := SteamAPI_ISteamUtils_GetIPv6ConnectivityState(@Self, eProtocol);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.IsSteamRunningOnSteamDeck(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.ShowFloatingGamepadTextInput(eKeyboardMode: EFloatingGamepadTextInputMode; nTextFieldXPosition: Integer; nTextFieldYPosition: Integer; nTextFieldWidth: Integer; nTextFieldHeight: Integer): Boolean;
begin
  Result := SteamAPI_ISteamUtils_ShowFloatingGamepadTextInput(@Self, eKeyboardMode, nTextFieldXPosition, nTextFieldYPosition, nTextFieldWidth, nTextFieldHeight);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUtilsHelper.SetGameLauncherMode(bLauncherMode: Boolean);
begin
  SteamAPI_ISteamUtils_SetGameLauncherMode(@Self, bLauncherMode);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUtilsHelper.DismissFloatingGamepadTextInput(): Boolean;
begin
  Result := SteamAPI_ISteamUtils_DismissFloatingGamepadTextInput(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetFavoriteGameCount(): Integer;
begin
  Result := SteamAPI_ISteamMatchmaking_GetFavoriteGameCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetFavoriteGame(iGame: Integer; pnAppID: PAppId_t; pnIP: PUInt32; pnConnPort: PUInt16; pnQueryPort: PUInt16; punFlags: PUInt32; pRTime32LastPlayedOnServer: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_GetFavoriteGame(@Self, iGame, pnAppID, pnIP, pnConnPort, pnQueryPort, punFlags, pRTime32LastPlayedOnServer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.AddFavoriteGame(nAppID: AppId_t; nIP: UInt32; nConnPort: UInt16; nQueryPort: UInt16; unFlags: UInt32; rTime32LastPlayedOnServer: UInt32): Integer;
begin
  Result := SteamAPI_ISteamMatchmaking_AddFavoriteGame(@Self, nAppID, nIP, nConnPort, nQueryPort, unFlags, rTime32LastPlayedOnServer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.RemoveFavoriteGame(nAppID: AppId_t; nIP: UInt32; nConnPort: UInt16; nQueryPort: UInt16; unFlags: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_RemoveFavoriteGame(@Self, nAppID, nIP, nConnPort, nQueryPort, unFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.RequestLobbyList(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamMatchmaking_RequestLobbyList(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.AddRequestLobbyListStringFilter(const pchKeyToMatch: PAnsiChar; const pchValueToMatch: PAnsiChar; eComparisonType: ELobbyComparison);
begin
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListStringFilter(@Self, pchKeyToMatch, pchValueToMatch, eComparisonType);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.AddRequestLobbyListNumericalFilter(const pchKeyToMatch: PAnsiChar; nValueToMatch: Integer; eComparisonType: ELobbyComparison);
begin
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListNumericalFilter(@Self, pchKeyToMatch, nValueToMatch, eComparisonType);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.AddRequestLobbyListNearValueFilter(const pchKeyToMatch: PAnsiChar; nValueToBeCloseTo: Integer);
begin
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListNearValueFilter(@Self, pchKeyToMatch, nValueToBeCloseTo);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.AddRequestLobbyListFilterSlotsAvailable(nSlotsAvailable: Integer);
begin
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable(@Self, nSlotsAvailable);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.AddRequestLobbyListDistanceFilter(eLobbyDistanceFilter: ELobbyDistanceFilter);
begin
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListDistanceFilter(@Self, eLobbyDistanceFilter);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.AddRequestLobbyListResultCountFilter(cMaxResults: Integer);
begin
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListResultCountFilter(@Self, cMaxResults);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.AddRequestLobbyListCompatibleMembersFilter(steamIDLobby: CSteamID);
begin
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyByIndex(iLobby: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyByIndex(@Self, iLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.CreateLobby(eLobbyType: ELobbyType; cMaxMembers: Integer): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamMatchmaking_CreateLobby(@Self, eLobbyType, cMaxMembers);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.JoinLobby(steamIDLobby: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamMatchmaking_JoinLobby(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.LeaveLobby(steamIDLobby: CSteamID);
begin
  SteamAPI_ISteamMatchmaking_LeaveLobby(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.InviteUserToLobby(steamIDLobby: CSteamID; steamIDInvitee: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_InviteUserToLobby(@Self, steamIDLobby, steamIDInvitee);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetNumLobbyMembers(steamIDLobby: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamMatchmaking_GetNumLobbyMembers(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyMemberByIndex(steamIDLobby: CSteamID; iMember: Integer): CSteamID;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyMemberByIndex(@Self, steamIDLobby, iMember);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyData(steamIDLobby: CSteamID; const pchKey: PAnsiChar): PAnsiChar;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyData(@Self, steamIDLobby, pchKey);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.SetLobbyData(steamIDLobby: CSteamID; const pchKey: PAnsiChar; const pchValue: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_SetLobbyData(@Self, steamIDLobby, pchKey, pchValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyDataCount(steamIDLobby: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyDataCount(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyDataByIndex(steamIDLobby: CSteamID; iLobbyData: Integer; pchKey: PAnsiChar; cchKeyBufferSize: Integer; pchValue: PAnsiChar; cchValueBufferSize: Integer): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyDataByIndex(@Self, steamIDLobby, iLobbyData, pchKey, cchKeyBufferSize, pchValue, cchValueBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.DeleteLobbyData(steamIDLobby: CSteamID; const pchKey: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_DeleteLobbyData(@Self, steamIDLobby, pchKey);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyMemberData(steamIDLobby: CSteamID; steamIDUser: CSteamID; const pchKey: PAnsiChar): PAnsiChar;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyMemberData(@Self, steamIDLobby, steamIDUser, pchKey);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.SetLobbyMemberData(steamIDLobby: CSteamID; const pchKey: PAnsiChar; const pchValue: PAnsiChar);
begin
  SteamAPI_ISteamMatchmaking_SetLobbyMemberData(@Self, steamIDLobby, pchKey, pchValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.SendLobbyChatMsg(steamIDLobby: CSteamID; const pvMsgBody: Pointer; cubMsgBody: Integer): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_SendLobbyChatMsg(@Self, steamIDLobby, pvMsgBody, cubMsgBody);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyChatEntry(steamIDLobby: CSteamID; iChatID: Integer; pSteamIDUser: PCSteamID; pvData: Pointer; cubData: Integer; peChatEntryType: PEChatEntryType): Integer;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyChatEntry(@Self, steamIDLobby, iChatID, pSteamIDUser, pvData, cubData, peChatEntryType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.RequestLobbyData(steamIDLobby: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_RequestLobbyData(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingHelper.SetLobbyGameServer(steamIDLobby: CSteamID; unGameServerIP: UInt32; unGameServerPort: UInt16; steamIDGameServer: CSteamID);
begin
  SteamAPI_ISteamMatchmaking_SetLobbyGameServer(@Self, steamIDLobby, unGameServerIP, unGameServerPort, steamIDGameServer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyGameServer(steamIDLobby: CSteamID; punGameServerIP: PUInt32; punGameServerPort: PUInt16; psteamIDGameServer: PCSteamID): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyGameServer(@Self, steamIDLobby, punGameServerIP, punGameServerPort, psteamIDGameServer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.SetLobbyMemberLimit(steamIDLobby: CSteamID; cMaxMembers: Integer): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_SetLobbyMemberLimit(@Self, steamIDLobby, cMaxMembers);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyMemberLimit(steamIDLobby: CSteamID): Integer;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyMemberLimit(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.SetLobbyType(steamIDLobby: CSteamID; eLobbyType: ELobbyType): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_SetLobbyType(@Self, steamIDLobby, eLobbyType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.SetLobbyJoinable(steamIDLobby: CSteamID; bLobbyJoinable: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_SetLobbyJoinable(@Self, steamIDLobby, bLobbyJoinable);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.GetLobbyOwner(steamIDLobby: CSteamID): CSteamID;
begin
  Result := SteamAPI_ISteamMatchmaking_GetLobbyOwner(@Self, steamIDLobby);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.SetLobbyOwner(steamIDLobby: CSteamID; steamIDNewOwner: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_SetLobbyOwner(@Self, steamIDLobby, steamIDNewOwner);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingHelper.SetLinkedLobby(steamIDLobby: CSteamID; steamIDLobbyDependent: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamMatchmaking_SetLinkedLobby(@Self, steamIDLobby, steamIDLobbyDependent);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServerListResponseHelper.ServerResponded(hRequest: HServerListRequest; iServer: Integer);
begin
  SteamAPI_ISteamMatchmakingServerListResponse_ServerResponded(@Self, hRequest, iServer);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServerListResponseHelper.ServerFailedToRespond(hRequest: HServerListRequest; iServer: Integer);
begin
  SteamAPI_ISteamMatchmakingServerListResponse_ServerFailedToRespond(@Self, hRequest, iServer);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServerListResponseHelper.RefreshComplete(hRequest: HServerListRequest; response: EMatchMakingServerResponse);
begin
  SteamAPI_ISteamMatchmakingServerListResponse_RefreshComplete(@Self, hRequest, response);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingPingResponseHelper.ServerResponded(var server: gameserveritem_t);
begin
  SteamAPI_ISteamMatchmakingPingResponse_ServerResponded(@Self, @server);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingPingResponseHelper.ServerFailedToRespond();
begin
  SteamAPI_ISteamMatchmakingPingResponse_ServerFailedToRespond(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingPlayersResponseHelper.AddPlayerToList(const pchName: PAnsiChar; nScore: Integer; flTimePlayed: Single);
begin
  SteamAPI_ISteamMatchmakingPlayersResponse_AddPlayerToList(@Self, pchName, nScore, flTimePlayed);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingPlayersResponseHelper.PlayersFailedToRespond();
begin
  SteamAPI_ISteamMatchmakingPlayersResponse_PlayersFailedToRespond(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingPlayersResponseHelper.PlayersRefreshComplete();
begin
  SteamAPI_ISteamMatchmakingPlayersResponse_PlayersRefreshComplete(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingRulesResponseHelper.RulesResponded(const pchRule: PAnsiChar; const pchValue: PAnsiChar);
begin
  SteamAPI_ISteamMatchmakingRulesResponse_RulesResponded(@Self, pchRule, pchValue);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingRulesResponseHelper.RulesFailedToRespond();
begin
  SteamAPI_ISteamMatchmakingRulesResponse_RulesFailedToRespond(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingRulesResponseHelper.RulesRefreshComplete();
begin
  SteamAPI_ISteamMatchmakingRulesResponse_RulesRefreshComplete(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.RequestInternetServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
begin
  Result := SteamAPI_ISteamMatchmakingServers_RequestInternetServerList(@Self, iApp, ppchFilters, nFilters, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.RequestLANServerList(iApp: AppId_t; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
begin
  Result := SteamAPI_ISteamMatchmakingServers_RequestLANServerList(@Self, iApp, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.RequestFriendsServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
begin
  Result := SteamAPI_ISteamMatchmakingServers_RequestFriendsServerList(@Self, iApp, ppchFilters, nFilters, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.RequestFavoritesServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
begin
  Result := SteamAPI_ISteamMatchmakingServers_RequestFavoritesServerList(@Self, iApp, ppchFilters, nFilters, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.RequestHistoryServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
begin
  Result := SteamAPI_ISteamMatchmakingServers_RequestHistoryServerList(@Self, iApp, ppchFilters, nFilters, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.RequestSpectatorServerList(iApp: AppId_t; ppchFilters: PPMatchMakingKeyValuePair_t; nFilters: UInt32; pRequestServersResponse: PISteamMatchmakingServerListResponse): HServerListRequest;
begin
  Result := SteamAPI_ISteamMatchmakingServers_RequestSpectatorServerList(@Self, iApp, ppchFilters, nFilters, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServersHelper.ReleaseRequest(hServerListRequest: HServerListRequest);
begin
  SteamAPI_ISteamMatchmakingServers_ReleaseRequest(@Self, hServerListRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.GetServerDetails(hRequest: HServerListRequest; iServer: Integer): Pgameserveritem_t;
begin
  Result := SteamAPI_ISteamMatchmakingServers_GetServerDetails(@Self, hRequest, iServer);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServersHelper.CancelQuery(hRequest: HServerListRequest);
begin
  SteamAPI_ISteamMatchmakingServers_CancelQuery(@Self, hRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServersHelper.RefreshQuery(hRequest: HServerListRequest);
begin
  SteamAPI_ISteamMatchmakingServers_RefreshQuery(@Self, hRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.IsRefreshing(hRequest: HServerListRequest): Boolean;
begin
  Result := SteamAPI_ISteamMatchmakingServers_IsRefreshing(@Self, hRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.GetServerCount(hRequest: HServerListRequest): Integer;
begin
  Result := SteamAPI_ISteamMatchmakingServers_GetServerCount(@Self, hRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServersHelper.RefreshServer(hRequest: HServerListRequest; iServer: Integer);
begin
  SteamAPI_ISteamMatchmakingServers_RefreshServer(@Self, hRequest, iServer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.PingServer(unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingPingResponse): HServerQuery;
begin
  Result := SteamAPI_ISteamMatchmakingServers_PingServer(@Self, unIP, usPort, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.PlayerDetails(unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingPlayersResponse): HServerQuery;
begin
  Result := SteamAPI_ISteamMatchmakingServers_PlayerDetails(@Self, unIP, usPort, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMatchmakingServersHelper.ServerRules(unIP: UInt32; usPort: UInt16; pRequestServersResponse: PISteamMatchmakingRulesResponse): HServerQuery;
begin
  Result := SteamAPI_ISteamMatchmakingServers_ServerRules(@Self, unIP, usPort, pRequestServersResponse);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMatchmakingServersHelper.CancelServerQuery(hServerQuery: HServerQuery);
begin
  SteamAPI_ISteamMatchmakingServers_CancelServerQuery(@Self, hServerQuery);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.AddGameSearchParams(const pchKeyToFind: PAnsiChar; const pchValuesToFind: PAnsiChar): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_AddGameSearchParams(@Self, pchKeyToFind, pchValuesToFind);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.SearchForGameWithLobby(steamIDLobby: CSteamID; nPlayerMin: Integer; nPlayerMax: Integer): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_SearchForGameWithLobby(@Self, steamIDLobby, nPlayerMin, nPlayerMax);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.SearchForGameSolo(nPlayerMin: Integer; nPlayerMax: Integer): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_SearchForGameSolo(@Self, nPlayerMin, nPlayerMax);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.AcceptGame(): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_AcceptGame(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.DeclineGame(): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_DeclineGame(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.RetrieveConnectionDetails(steamIDHost: CSteamID; pchConnectionDetails: PAnsiChar; cubConnectionDetails: Integer): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_RetrieveConnectionDetails(@Self, steamIDHost, pchConnectionDetails, cubConnectionDetails);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.EndGameSearch(): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_EndGameSearch(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.SetGameHostParams(const pchKey: PAnsiChar; const pchValue: PAnsiChar): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_SetGameHostParams(@Self, pchKey, pchValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.SetConnectionDetails(const pchConnectionDetails: PAnsiChar; cubConnectionDetails: Integer): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_SetConnectionDetails(@Self, pchConnectionDetails, cubConnectionDetails);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.RequestPlayersForGame(nPlayerMin: Integer; nPlayerMax: Integer; nMaxTeamSize: Integer): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_RequestPlayersForGame(@Self, nPlayerMin, nPlayerMax, nMaxTeamSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.HostConfirmGameStart(ullUniqueGameID: UInt64): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_HostConfirmGameStart(@Self, ullUniqueGameID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.CancelRequestPlayersForGame(): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_CancelRequestPlayersForGame(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.SubmitPlayerResult(ullUniqueGameID: UInt64; steamIDPlayer: CSteamID; EPlayerResult: EPlayerResult_t): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_SubmitPlayerResult(@Self, ullUniqueGameID, steamIDPlayer, EPlayerResult);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameSearchHelper.EndGame(ullUniqueGameID: UInt64): EGameSearchErrorCode_t;
begin
  Result := SteamAPI_ISteamGameSearch_EndGame(@Self, ullUniqueGameID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.GetNumActiveBeacons(): UInt32;
begin
  Result := SteamAPI_ISteamParties_GetNumActiveBeacons(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.GetBeaconByIndex(unIndex: UInt32): PartyBeaconID_t;
begin
  Result := SteamAPI_ISteamParties_GetBeaconByIndex(@Self, unIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.GetBeaconDetails(ulBeaconID: PartyBeaconID_t; pSteamIDBeaconOwner: PCSteamID; pLocation: PSteamPartyBeaconLocation_t; pchMetadata: PAnsiChar; cchMetadata: Integer): Boolean;
begin
  Result := SteamAPI_ISteamParties_GetBeaconDetails(@Self, ulBeaconID, pSteamIDBeaconOwner, pLocation, pchMetadata, cchMetadata);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.JoinParty(ulBeaconID: PartyBeaconID_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamParties_JoinParty(@Self, ulBeaconID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.GetNumAvailableBeaconLocations(puNumLocations: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamParties_GetNumAvailableBeaconLocations(@Self, puNumLocations);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.GetAvailableBeaconLocations(pLocationList: PSteamPartyBeaconLocation_t; uMaxNumLocations: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamParties_GetAvailableBeaconLocations(@Self, pLocationList, uMaxNumLocations);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.CreateBeacon(unOpenSlots: UInt32; pBeaconLocation: PSteamPartyBeaconLocation_t; const pchConnectString: PAnsiChar; const pchMetadata: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamParties_CreateBeacon(@Self, unOpenSlots, pBeaconLocation, pchConnectString, pchMetadata);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamPartiesHelper.OnReservationCompleted(ulBeacon: PartyBeaconID_t; steamIDUser: CSteamID);
begin
  SteamAPI_ISteamParties_OnReservationCompleted(@Self, ulBeacon, steamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamPartiesHelper.CancelReservation(ulBeacon: PartyBeaconID_t; steamIDUser: CSteamID);
begin
  SteamAPI_ISteamParties_CancelReservation(@Self, ulBeacon, steamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.ChangeNumOpenSlots(ulBeacon: PartyBeaconID_t; unOpenSlots: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamParties_ChangeNumOpenSlots(@Self, ulBeacon, unOpenSlots);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.DestroyBeacon(ulBeacon: PartyBeaconID_t): Boolean;
begin
  Result := SteamAPI_ISteamParties_DestroyBeacon(@Self, ulBeacon);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamPartiesHelper.GetBeaconLocationData(BeaconLocation: SteamPartyBeaconLocation_t; eData: ESteamPartyBeaconLocationData; pchDataStringOut: PAnsiChar; cchDataStringOut: Integer): Boolean;
begin
  Result := SteamAPI_ISteamParties_GetBeaconLocationData(@Self, BeaconLocation, eData, pchDataStringOut, cchDataStringOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileWrite(const pchFile: PAnsiChar; const pvData: Pointer; cubData: Int32): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileWrite(@Self, pchFile, pvData, cubData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileRead(const pchFile: PAnsiChar; pvData: Pointer; cubDataToRead: Int32): Int32;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileRead(@Self, pchFile, pvData, cubDataToRead);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileWriteAsync(const pchFile: PAnsiChar; const pvData: Pointer; cubData: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileWriteAsync(@Self, pchFile, pvData, cubData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileReadAsync(const pchFile: PAnsiChar; nOffset: UInt32; cubToRead: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileReadAsync(@Self, pchFile, nOffset, cubToRead);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileReadAsyncComplete(hReadCall: SteamAPICall_t; pvBuffer: Pointer; cubToRead: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileReadAsyncComplete(@Self, hReadCall, pvBuffer, cubToRead);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileForget(const pchFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileForget(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileDelete(const pchFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileDelete(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileShare(const pchFile: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileShare(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.SetSyncPlatforms(const pchFile: PAnsiChar; eRemoteStoragePlatform: ERemoteStoragePlatform): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_SetSyncPlatforms(@Self, pchFile, eRemoteStoragePlatform);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileWriteStreamOpen(const pchFile: PAnsiChar): UGCFileWriteStreamHandle_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileWriteStreamOpen(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileWriteStreamWriteChunk(writeHandle: UGCFileWriteStreamHandle_t; const pvData: Pointer; cubData: Int32): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileWriteStreamWriteChunk(@Self, writeHandle, pvData, cubData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileWriteStreamClose(writeHandle: UGCFileWriteStreamHandle_t): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileWriteStreamClose(@Self, writeHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileWriteStreamCancel(writeHandle: UGCFileWriteStreamHandle_t): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileWriteStreamCancel(@Self, writeHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FileExists(const pchFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FileExists(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.FilePersisted(const pchFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_FilePersisted(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetFileSize(const pchFile: PAnsiChar): Int32;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetFileSize(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetFileTimestamp(const pchFile: PAnsiChar): Int64;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetFileTimestamp(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetSyncPlatforms(const pchFile: PAnsiChar): ERemoteStoragePlatform;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetSyncPlatforms(@Self, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetFileCount(): Int32;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetFileCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetFileNameAndSize(iFile: Integer; pnFileSizeInBytes: PInt32): PAnsiChar;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetFileNameAndSize(@Self, iFile, pnFileSizeInBytes);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetQuota(pnTotalBytes: PUInt64; puAvailableBytes: PUInt64): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetQuota(@Self, pnTotalBytes, puAvailableBytes);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.IsCloudEnabledForAccount(): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_IsCloudEnabledForAccount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.IsCloudEnabledForApp(): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_IsCloudEnabledForApp(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamRemoteStorageHelper.SetCloudEnabledForApp(bEnabled: Boolean);
begin
  SteamAPI_ISteamRemoteStorage_SetCloudEnabledForApp(@Self, bEnabled);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UGCDownload(hContent: UGCHandle_t; unPriority: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_UGCDownload(@Self, hContent, unPriority);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetUGCDownloadProgress(hContent: UGCHandle_t; pnBytesDownloaded: PInt32; pnBytesExpected: PInt32): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetUGCDownloadProgress(@Self, hContent, pnBytesDownloaded, pnBytesExpected);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetUGCDetails(hContent: UGCHandle_t; pnAppID: PAppId_t; ppchName: PPAnsiChar; pnFileSizeInBytes: PInt32; pSteamIDOwner: PCSteamID): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetUGCDetails(@Self, hContent, pnAppID, ppchName, pnFileSizeInBytes, pSteamIDOwner);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UGCRead(hContent: UGCHandle_t; pvData: Pointer; cubDataToRead: Int32; cOffset: UInt32; eAction: EUGCReadAction): Int32;
begin
  Result := SteamAPI_ISteamRemoteStorage_UGCRead(@Self, hContent, pvData, cubDataToRead, cOffset, eAction);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetCachedUGCCount(): Int32;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetCachedUGCCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetCachedUGCHandle(iCachedContent: Int32): UGCHandle_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetCachedUGCHandle(@Self, iCachedContent);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.PublishWorkshopFile(const pchFile: PAnsiChar; const pchPreviewFile: PAnsiChar; nConsumerAppId: AppId_t; const pchTitle: PAnsiChar; const pchDescription: PAnsiChar; eVisibility: ERemoteStoragePublishedFileVisibility; pTags: PSteamParamStringArray_t; eWorkshopFileType: EWorkshopFileType): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_PublishWorkshopFile(@Self, pchFile, pchPreviewFile, nConsumerAppId, pchTitle, pchDescription, eVisibility, pTags, eWorkshopFileType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.CreatePublishedFileUpdateRequest(unPublishedFileId: PublishedFileId_t): PublishedFileUpdateHandle_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_CreatePublishedFileUpdateRequest(@Self, unPublishedFileId);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdatePublishedFileFile(updateHandle: PublishedFileUpdateHandle_t; const pchFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdatePublishedFileFile(@Self, updateHandle, pchFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdatePublishedFilePreviewFile(updateHandle: PublishedFileUpdateHandle_t; const pchPreviewFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdatePublishedFilePreviewFile(@Self, updateHandle, pchPreviewFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdatePublishedFileTitle(updateHandle: PublishedFileUpdateHandle_t; const pchTitle: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTitle(@Self, updateHandle, pchTitle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdatePublishedFileDescription(updateHandle: PublishedFileUpdateHandle_t; const pchDescription: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdatePublishedFileDescription(@Self, updateHandle, pchDescription);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdatePublishedFileVisibility(updateHandle: PublishedFileUpdateHandle_t; eVisibility: ERemoteStoragePublishedFileVisibility): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdatePublishedFileVisibility(@Self, updateHandle, eVisibility);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdatePublishedFileTags(updateHandle: PublishedFileUpdateHandle_t; pTags: PSteamParamStringArray_t): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTags(@Self, updateHandle, pTags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.CommitPublishedFileUpdate(updateHandle: PublishedFileUpdateHandle_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_CommitPublishedFileUpdate(@Self, updateHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetPublishedFileDetails(unPublishedFileId: PublishedFileId_t; unMaxSecondsOld: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetPublishedFileDetails(@Self, unPublishedFileId, unMaxSecondsOld);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.DeletePublishedFile(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_DeletePublishedFile(@Self, unPublishedFileId);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.EnumerateUserPublishedFiles(unStartIndex: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_EnumerateUserPublishedFiles(@Self, unStartIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.SubscribePublishedFile(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_SubscribePublishedFile(@Self, unPublishedFileId);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.EnumerateUserSubscribedFiles(unStartIndex: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_EnumerateUserSubscribedFiles(@Self, unStartIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UnsubscribePublishedFile(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_UnsubscribePublishedFile(@Self, unPublishedFileId);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdatePublishedFileSetChangeDescription(updateHandle: PublishedFileUpdateHandle_t; const pchChangeDescription: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription(@Self, updateHandle, pchChangeDescription);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetPublishedItemVoteDetails(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetPublishedItemVoteDetails(@Self, unPublishedFileId);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UpdateUserPublishedItemVote(unPublishedFileId: PublishedFileId_t; bVoteUp: Boolean): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_UpdateUserPublishedItemVote(@Self, unPublishedFileId, bVoteUp);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetUserPublishedItemVoteDetails(unPublishedFileId: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetUserPublishedItemVoteDetails(@Self, unPublishedFileId);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.EnumerateUserSharedWorkshopFiles(steamId: CSteamID; unStartIndex: UInt32; pRequiredTags: PSteamParamStringArray_t; pExcludedTags: PSteamParamStringArray_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles(@Self, steamId, unStartIndex, pRequiredTags, pExcludedTags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.PublishVideo(eVideoProvider: EWorkshopVideoProvider; const pchVideoAccount: PAnsiChar; const pchVideoIdentifier: PAnsiChar; const pchPreviewFile: PAnsiChar; nConsumerAppId: AppId_t; const pchTitle: PAnsiChar; const pchDescription: PAnsiChar; eVisibility: ERemoteStoragePublishedFileVisibility; pTags: PSteamParamStringArray_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_PublishVideo(@Self, eVideoProvider, pchVideoAccount, pchVideoIdentifier, pchPreviewFile, nConsumerAppId, pchTitle, pchDescription, eVisibility, pTags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.SetUserPublishedFileAction(unPublishedFileId: PublishedFileId_t; eAction: EWorkshopFileAction): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_SetUserPublishedFileAction(@Self, unPublishedFileId, eAction);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.EnumeratePublishedFilesByUserAction(eAction: EWorkshopFileAction; unStartIndex: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_EnumeratePublishedFilesByUserAction(@Self, eAction, unStartIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.EnumeratePublishedWorkshopFiles(eEnumerationType: EWorkshopEnumerationType; unStartIndex: UInt32; unCount: UInt32; unDays: UInt32; pTags: PSteamParamStringArray_t; pUserTags: PSteamParamStringArray_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_EnumeratePublishedWorkshopFiles(@Self, eEnumerationType, unStartIndex, unCount, unDays, pTags, pUserTags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.UGCDownloadToLocation(hContent: UGCHandle_t; const pchLocation: PAnsiChar; unPriority: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamRemoteStorage_UGCDownloadToLocation(@Self, hContent, pchLocation, unPriority);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetLocalFileChangeCount(): Int32;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetLocalFileChangeCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.GetLocalFileChange(iFile: Integer; pEChangeType: PERemoteStorageLocalFileChange; pEFilePathType: PERemoteStorageFilePathType): PAnsiChar;
begin
  Result := SteamAPI_ISteamRemoteStorage_GetLocalFileChange(@Self, iFile, pEChangeType, pEFilePathType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.BeginFileWriteBatch(): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_BeginFileWriteBatch(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemoteStorageHelper.EndFileWriteBatch(): Boolean;
begin
  Result := SteamAPI_ISteamRemoteStorage_EndFileWriteBatch(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.RequestCurrentStats(): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_RequestCurrentStats(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetStat(const pchName: PAnsiChar; pData: PInt32): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetStatInt32(@Self, pchName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetStat(const pchName: PAnsiChar; pData: PSingle): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetStatFloat(@Self, pchName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.SetStat(const pchName: PAnsiChar; nData: Int32): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_SetStatInt32(@Self, pchName, nData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.SetStat(const pchName: PAnsiChar; fData: Single): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_SetStatFloat(@Self, pchName, fData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.UpdateAvgRateStat(const pchName: PAnsiChar; flCountThisSession: Single; dSessionLength: Double): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_UpdateAvgRateStat(@Self, pchName, flCountThisSession, dSessionLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievement(const pchName: PAnsiChar; pbAchieved: PBoolean): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievement(@Self, pchName, pbAchieved);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.SetAchievement(const pchName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_SetAchievement(@Self, pchName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.ClearAchievement(const pchName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_ClearAchievement(@Self, pchName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievementAndUnlockTime(const pchName: PAnsiChar; pbAchieved: PBoolean; punUnlockTime: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime(@Self, pchName, pbAchieved, punUnlockTime);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.StoreStats(): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_StoreStats(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievementIcon(const pchName: PAnsiChar): Integer;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementIcon(@Self, pchName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievementDisplayAttribute(const pchName: PAnsiChar; const pchKey: PAnsiChar): PAnsiChar;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(@Self, pchName, pchKey);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.IndicateAchievementProgress(const pchName: PAnsiChar; nCurProgress: UInt32; nMaxProgress: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_IndicateAchievementProgress(@Self, pchName, nCurProgress, nMaxProgress);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetNumAchievements(): UInt32;
begin
  Result := SteamAPI_ISteamUserStats_GetNumAchievements(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievementName(iAchievement: UInt32): PAnsiChar;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementName(@Self, iAchievement);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.RequestUserStats(steamIDUser: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_RequestUserStats(@Self, steamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PInt32): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetUserStatInt32(@Self, steamIDUser, pchName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PSingle): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetUserStatFloat(@Self, steamIDUser, pchName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar; pbAchieved: PBoolean): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetUserAchievement(@Self, steamIDUser, pchName, pbAchieved);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetUserAchievementAndUnlockTime(steamIDUser: CSteamID; const pchName: PAnsiChar; pbAchieved: PBoolean; punUnlockTime: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetUserAchievementAndUnlockTime(@Self, steamIDUser, pchName, pbAchieved, punUnlockTime);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.ResetAllStats(bAchievementsToo: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_ResetAllStats(@Self, bAchievementsToo);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.FindOrCreateLeaderboard(const pchLeaderboardName: PAnsiChar; eLeaderboardSortMethod: ELeaderboardSortMethod; eLeaderboardDisplayType: ELeaderboardDisplayType): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_FindOrCreateLeaderboard(@Self, pchLeaderboardName, eLeaderboardSortMethod, eLeaderboardDisplayType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.FindLeaderboard(const pchLeaderboardName: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_FindLeaderboard(@Self, pchLeaderboardName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetLeaderboardName(hSteamLeaderboard: SteamLeaderboard_t): PAnsiChar;
begin
  Result := SteamAPI_ISteamUserStats_GetLeaderboardName(@Self, hSteamLeaderboard);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetLeaderboardEntryCount(hSteamLeaderboard: SteamLeaderboard_t): Integer;
begin
  Result := SteamAPI_ISteamUserStats_GetLeaderboardEntryCount(@Self, hSteamLeaderboard);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetLeaderboardSortMethod(hSteamLeaderboard: SteamLeaderboard_t): ELeaderboardSortMethod;
begin
  Result := SteamAPI_ISteamUserStats_GetLeaderboardSortMethod(@Self, hSteamLeaderboard);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetLeaderboardDisplayType(hSteamLeaderboard: SteamLeaderboard_t): ELeaderboardDisplayType;
begin
  Result := SteamAPI_ISteamUserStats_GetLeaderboardDisplayType(@Self, hSteamLeaderboard);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.DownloadLeaderboardEntries(hSteamLeaderboard: SteamLeaderboard_t; eLeaderboardDataRequest: ELeaderboardDataRequest; nRangeStart: Integer; nRangeEnd: Integer): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_DownloadLeaderboardEntries(@Self, hSteamLeaderboard, eLeaderboardDataRequest, nRangeStart, nRangeEnd);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.DownloadLeaderboardEntriesForUsers(hSteamLeaderboard: SteamLeaderboard_t; prgUsers: PCSteamID; cUsers: Integer): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_DownloadLeaderboardEntriesForUsers(@Self, hSteamLeaderboard, prgUsers, cUsers);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetDownloadedLeaderboardEntry(hSteamLeaderboardEntries: SteamLeaderboardEntries_t; index: Integer; pLeaderboardEntry: PLeaderboardEntry_t; pDetails: PInt32; cDetailsMax: Integer): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetDownloadedLeaderboardEntry(@Self, hSteamLeaderboardEntries, index, pLeaderboardEntry, pDetails, cDetailsMax);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.UploadLeaderboardScore(hSteamLeaderboard: SteamLeaderboard_t; eLeaderboardUploadScoreMethod: ELeaderboardUploadScoreMethod; nScore: Int32; const pScoreDetails: PInt32; cScoreDetailsCount: Integer): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_UploadLeaderboardScore(@Self, hSteamLeaderboard, eLeaderboardUploadScoreMethod, nScore, pScoreDetails, cScoreDetailsCount);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.AttachLeaderboardUGC(hSteamLeaderboard: SteamLeaderboard_t; hUGC: UGCHandle_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_AttachLeaderboardUGC(@Self, hSteamLeaderboard, hUGC);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetNumberOfCurrentPlayers(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_GetNumberOfCurrentPlayers(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.RequestGlobalAchievementPercentages(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_RequestGlobalAchievementPercentages(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetMostAchievedAchievementInfo(pchName: PAnsiChar; unNameBufLen: UInt32; pflPercent: PSingle; pbAchieved: PBoolean): Integer;
begin
  Result := SteamAPI_ISteamUserStats_GetMostAchievedAchievementInfo(@Self, pchName, unNameBufLen, pflPercent, pbAchieved);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetNextMostAchievedAchievementInfo(iIteratorPrevious: Integer; pchName: PAnsiChar; unNameBufLen: UInt32; pflPercent: PSingle; pbAchieved: PBoolean): Integer;
begin
  Result := SteamAPI_ISteamUserStats_GetNextMostAchievedAchievementInfo(@Self, iIteratorPrevious, pchName, unNameBufLen, pflPercent, pbAchieved);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievementAchievedPercent(const pchName: PAnsiChar; pflPercent: PSingle): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementAchievedPercent(@Self, pchName, pflPercent);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.RequestGlobalStats(nHistoryDays: Integer): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUserStats_RequestGlobalStats(@Self, nHistoryDays);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetGlobalStat(const pchStatName: PAnsiChar; pData: PInt64): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetGlobalStatInt64(@Self, pchStatName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetGlobalStat(const pchStatName: PAnsiChar; pData: PDouble): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetGlobalStatDouble(@Self, pchStatName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetGlobalStatHistory(const pchStatName: PAnsiChar; pData: PInt64; cubData: UInt32): Int32;
begin
  Result := SteamAPI_ISteamUserStats_GetGlobalStatHistoryInt64(@Self, pchStatName, pData, cubData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetGlobalStatHistory(const pchStatName: PAnsiChar; pData: PDouble; cubData: UInt32): Int32;
begin
  Result := SteamAPI_ISteamUserStats_GetGlobalStatHistoryDouble(@Self, pchStatName, pData, cubData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievementProgressLimits(const pchName: PAnsiChar; pnMinProgress: PInt32; pnMaxProgress: PInt32): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32(@Self, pchName, pnMinProgress, pnMaxProgress);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUserStatsHelper.GetAchievementProgressLimits(const pchName: PAnsiChar; pfMinProgress: PSingle; pfMaxProgress: PSingle): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat(@Self, pchName, pfMinProgress, pfMaxProgress);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsSubscribed(): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsSubscribed(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsLowViolence(): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsLowViolence(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsCybercafe(): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsCybercafe(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsVACBanned(): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsVACBanned(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetCurrentGameLanguage(): PAnsiChar;
begin
  Result := SteamAPI_ISteamApps_GetCurrentGameLanguage(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetAvailableGameLanguages(): PAnsiChar;
begin
  Result := SteamAPI_ISteamApps_GetAvailableGameLanguages(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsSubscribedApp(appID: AppId_t): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsSubscribedApp(@Self, appID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsDlcInstalled(appID: AppId_t): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsDlcInstalled(@Self, appID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetEarliestPurchaseUnixTime(nAppID: AppId_t): UInt32;
begin
  Result := SteamAPI_ISteamApps_GetEarliestPurchaseUnixTime(@Self, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsSubscribedFromFreeWeekend(): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsSubscribedFromFreeWeekend(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetDLCCount(): Integer;
begin
  Result := SteamAPI_ISteamApps_GetDLCCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BGetDLCDataByIndex(iDLC: Integer; pAppID: PAppId_t; pbAvailable: PBoolean; pchName: PAnsiChar; cchNameBufferSize: Integer): Boolean;
begin
  Result := SteamAPI_ISteamApps_BGetDLCDataByIndex(@Self, iDLC, pAppID, pbAvailable, pchName, cchNameBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamAppsHelper.InstallDLC(nAppID: AppId_t);
begin
  SteamAPI_ISteamApps_InstallDLC(@Self, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamAppsHelper.UninstallDLC(nAppID: AppId_t);
begin
  SteamAPI_ISteamApps_UninstallDLC(@Self, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamAppsHelper.RequestAppProofOfPurchaseKey(nAppID: AppId_t);
begin
  SteamAPI_ISteamApps_RequestAppProofOfPurchaseKey(@Self, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetCurrentBetaName(pchName: PAnsiChar; cchNameBufferSize: Integer): Boolean;
begin
  Result := SteamAPI_ISteamApps_GetCurrentBetaName(@Self, pchName, cchNameBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.MarkContentCorrupt(bMissingFilesOnly: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamApps_MarkContentCorrupt(@Self, bMissingFilesOnly);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetInstalledDepots(appID: AppId_t; pvecDepots: PDepotId_t; cMaxDepots: UInt32): UInt32;
begin
  Result := SteamAPI_ISteamApps_GetInstalledDepots(@Self, appID, pvecDepots, cMaxDepots);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetAppInstallDir(appID: AppId_t; pchFolder: PAnsiChar; cchFolderBufferSize: UInt32): UInt32;
begin
  Result := SteamAPI_ISteamApps_GetAppInstallDir(@Self, appID, pchFolder, cchFolderBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsAppInstalled(appID: AppId_t): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsAppInstalled(@Self, appID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetAppOwner(): CSteamID;
begin
  Result := SteamAPI_ISteamApps_GetAppOwner(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetLaunchQueryParam(const pchKey: PAnsiChar): PAnsiChar;
begin
  Result := SteamAPI_ISteamApps_GetLaunchQueryParam(@Self, pchKey);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetDlcDownloadProgress(nAppID: AppId_t; punBytesDownloaded: PUInt64; punBytesTotal: PUInt64): Boolean;
begin
  Result := SteamAPI_ISteamApps_GetDlcDownloadProgress(@Self, nAppID, punBytesDownloaded, punBytesTotal);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetAppBuildId(): Integer;
begin
  Result := SteamAPI_ISteamApps_GetAppBuildId(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamAppsHelper.RequestAllProofOfPurchaseKeys();
begin
  SteamAPI_ISteamApps_RequestAllProofOfPurchaseKeys(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetFileDetails(const pszFileName: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamApps_GetFileDetails(@Self, pszFileName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.GetLaunchCommandLine(pszCommandLine: PAnsiChar; cubCommandLine: Integer): Integer;
begin
  Result := SteamAPI_ISteamApps_GetLaunchCommandLine(@Self, pszCommandLine, cubCommandLine);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsSubscribedFromFamilySharing(): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsSubscribedFromFamilySharing(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppsHelper.BIsTimedTrial(punSecondsAllowed: PUInt32; punSecondsPlayed: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamApps_BIsTimedTrial(@Self, punSecondsAllowed, punSecondsPlayed);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.SendP2PPacket(steamIDRemote: CSteamID; const pubData: Pointer; cubData: UInt32; eP2PSendType: EP2PSend; nChannel: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_SendP2PPacket(@Self, steamIDRemote, pubData, cubData, eP2PSendType, nChannel);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.IsP2PPacketAvailable(pcubMsgSize: PUInt32; nChannel: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_IsP2PPacketAvailable(@Self, pcubMsgSize, nChannel);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.ReadP2PPacket(pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32; psteamIDRemote: PCSteamID; nChannel: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_ReadP2PPacket(@Self, pubDest, cubDest, pcubMsgSize, psteamIDRemote, nChannel);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.AcceptP2PSessionWithUser(steamIDRemote: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_AcceptP2PSessionWithUser(@Self, steamIDRemote);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.CloseP2PSessionWithUser(steamIDRemote: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_CloseP2PSessionWithUser(@Self, steamIDRemote);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.CloseP2PChannelWithUser(steamIDRemote: CSteamID; nChannel: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_CloseP2PChannelWithUser(@Self, steamIDRemote, nChannel);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.GetP2PSessionState(steamIDRemote: CSteamID; pConnectionState: PP2PSessionState_t): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_GetP2PSessionState(@Self, steamIDRemote, pConnectionState);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.AllowP2PPacketRelay(bAllow: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_AllowP2PPacketRelay(@Self, bAllow);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.CreateListenSocket(nVirtualP2PPort: Integer; nIP: SteamIPAddress_t; nPort: UInt16; bAllowUseOfPacketRelay: Boolean): SNetListenSocket_t;
begin
  Result := SteamAPI_ISteamNetworking_CreateListenSocket(@Self, nVirtualP2PPort, nIP, nPort, bAllowUseOfPacketRelay);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.CreateP2PConnectionSocket(steamIDTarget: CSteamID; nVirtualPort: Integer; nTimeoutSec: Integer; bAllowUseOfPacketRelay: Boolean): SNetSocket_t;
begin
  Result := SteamAPI_ISteamNetworking_CreateP2PConnectionSocket(@Self, steamIDTarget, nVirtualPort, nTimeoutSec, bAllowUseOfPacketRelay);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.CreateConnectionSocket(nIP: SteamIPAddress_t; nPort: UInt16; nTimeoutSec: Integer): SNetSocket_t;
begin
  Result := SteamAPI_ISteamNetworking_CreateConnectionSocket(@Self, nIP, nPort, nTimeoutSec);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.DestroySocket(hSocket: SNetSocket_t; bNotifyRemoteEnd: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_DestroySocket(@Self, hSocket, bNotifyRemoteEnd);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.DestroyListenSocket(hSocket: SNetListenSocket_t; bNotifyRemoteEnd: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_DestroyListenSocket(@Self, hSocket, bNotifyRemoteEnd);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.SendDataOnSocket(hSocket: SNetSocket_t; pubData: Pointer; cubData: UInt32; bReliable: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_SendDataOnSocket(@Self, hSocket, pubData, cubData, bReliable);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.IsDataAvailableOnSocket(hSocket: SNetSocket_t; pcubMsgSize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_IsDataAvailableOnSocket(@Self, hSocket, pcubMsgSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.RetrieveDataFromSocket(hSocket: SNetSocket_t; pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_RetrieveDataFromSocket(@Self, hSocket, pubDest, cubDest, pcubMsgSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.IsDataAvailable(hListenSocket: SNetListenSocket_t; pcubMsgSize: PUInt32; phSocket: PSNetSocket_t): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_IsDataAvailable(@Self, hListenSocket, pcubMsgSize, phSocket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.RetrieveData(hListenSocket: SNetListenSocket_t; pubDest: Pointer; cubDest: UInt32; pcubMsgSize: PUInt32; phSocket: PSNetSocket_t): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_RetrieveData(@Self, hListenSocket, pubDest, cubDest, pcubMsgSize, phSocket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.GetSocketInfo(hSocket: SNetSocket_t; pSteamIDRemote: PCSteamID; peSocketStatus: PInteger; punIPRemote: PSteamIPAddress_t; punPortRemote: PUInt16): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_GetSocketInfo(@Self, hSocket, pSteamIDRemote, peSocketStatus, punIPRemote, punPortRemote);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.GetListenSocketInfo(hListenSocket: SNetListenSocket_t; pnIP: PSteamIPAddress_t; pnPort: PUInt16): Boolean;
begin
  Result := SteamAPI_ISteamNetworking_GetListenSocketInfo(@Self, hListenSocket, pnIP, pnPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.GetSocketConnectionType(hSocket: SNetSocket_t): ESNetSocketConnectionType;
begin
  Result := SteamAPI_ISteamNetworking_GetSocketConnectionType(@Self, hSocket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingHelper.GetMaxPacketSize(hSocket: SNetSocket_t): Integer;
begin
  Result := SteamAPI_ISteamNetworking_GetMaxPacketSize(@Self, hSocket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamScreenshotsHelper.WriteScreenshot(pubRGB: Pointer; cubRGB: UInt32; nWidth: Integer; nHeight: Integer): ScreenshotHandle;
begin
  Result := SteamAPI_ISteamScreenshots_WriteScreenshot(@Self, pubRGB, cubRGB, nWidth, nHeight);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamScreenshotsHelper.AddScreenshotToLibrary(const pchFilename: PAnsiChar; const pchThumbnailFilename: PAnsiChar; nWidth: Integer; nHeight: Integer): ScreenshotHandle;
begin
  Result := SteamAPI_ISteamScreenshots_AddScreenshotToLibrary(@Self, pchFilename, pchThumbnailFilename, nWidth, nHeight);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamScreenshotsHelper.TriggerScreenshot();
begin
  SteamAPI_ISteamScreenshots_TriggerScreenshot(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamScreenshotsHelper.HookScreenshots(bHook: Boolean);
begin
  SteamAPI_ISteamScreenshots_HookScreenshots(@Self, bHook);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamScreenshotsHelper.SetLocation(hScreenshot: ScreenshotHandle; const pchLocation: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamScreenshots_SetLocation(@Self, hScreenshot, pchLocation);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamScreenshotsHelper.TagUser(hScreenshot: ScreenshotHandle; steamID: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamScreenshots_TagUser(@Self, hScreenshot, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamScreenshotsHelper.TagPublishedFile(hScreenshot: ScreenshotHandle; unPublishedFileID: PublishedFileId_t): Boolean;
begin
  Result := SteamAPI_ISteamScreenshots_TagPublishedFile(@Self, hScreenshot, unPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamScreenshotsHelper.IsScreenshotsHooked(): Boolean;
begin
  Result := SteamAPI_ISteamScreenshots_IsScreenshotsHooked(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamScreenshotsHelper.AddVRScreenshotToLibrary(eType: EVRScreenshotType; const pchFilename: PAnsiChar; const pchVRFilename: PAnsiChar): ScreenshotHandle;
begin
  Result := SteamAPI_ISteamScreenshots_AddVRScreenshotToLibrary(@Self, eType, pchFilename, pchVRFilename);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicHelper.BIsEnabled(): Boolean;
begin
  Result := SteamAPI_ISteamMusic_BIsEnabled(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicHelper.BIsPlaying(): Boolean;
begin
  Result := SteamAPI_ISteamMusic_BIsPlaying(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicHelper.GetPlaybackStatus(): AudioPlayback_Status;
begin
  Result := SteamAPI_ISteamMusic_GetPlaybackStatus(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMusicHelper.Play();
begin
  SteamAPI_ISteamMusic_Play(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMusicHelper.Pause();
begin
  SteamAPI_ISteamMusic_Pause(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMusicHelper.PlayPrevious();
begin
  SteamAPI_ISteamMusic_PlayPrevious(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMusicHelper.PlayNext();
begin
  SteamAPI_ISteamMusic_PlayNext(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamMusicHelper.SetVolume(flVolume: Single);
begin
  SteamAPI_ISteamMusic_SetVolume(@Self, flVolume);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicHelper.GetVolume(): Single;
begin
  Result := SteamAPI_ISteamMusic_GetVolume(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.RegisterSteamMusicRemote(const pchName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_RegisterSteamMusicRemote(@Self, pchName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.DeregisterSteamMusicRemote(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_DeregisterSteamMusicRemote(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.BIsCurrentMusicRemote(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_BIsCurrentMusicRemote(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.BActivationSuccess(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_BActivationSuccess(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.SetDisplayName(const pchDisplayName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_SetDisplayName(@Self, pchDisplayName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.SetPNGIcon_64x64(pvBuffer: Pointer; cbBufferLength: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_SetPNGIcon_64x64(@Self, pvBuffer, cbBufferLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.EnablePlayPrevious(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_EnablePlayPrevious(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.EnablePlayNext(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_EnablePlayNext(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.EnableShuffled(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_EnableShuffled(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.EnableLooped(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_EnableLooped(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.EnableQueue(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_EnableQueue(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.EnablePlaylists(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_EnablePlaylists(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.UpdatePlaybackStatus(nStatus: AudioPlayback_Status): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_UpdatePlaybackStatus(@Self, nStatus);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.UpdateShuffled(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_UpdateShuffled(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.UpdateLooped(bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_UpdateLooped(@Self, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.UpdateVolume(flValue: Single): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_UpdateVolume(@Self, flValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.CurrentEntryWillChange(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_CurrentEntryWillChange(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.CurrentEntryIsAvailable(bAvailable: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_CurrentEntryIsAvailable(@Self, bAvailable);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.UpdateCurrentEntryText(const pchText: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_UpdateCurrentEntryText(@Self, pchText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.UpdateCurrentEntryElapsedSeconds(nValue: Integer): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_UpdateCurrentEntryElapsedSeconds(@Self, nValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.UpdateCurrentEntryCoverArt(pvBuffer: Pointer; cbBufferLength: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_UpdateCurrentEntryCoverArt(@Self, pvBuffer, cbBufferLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.CurrentEntryDidChange(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_CurrentEntryDidChange(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.QueueWillChange(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_QueueWillChange(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.ResetQueueEntries(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_ResetQueueEntries(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.SetQueueEntry(nID: Integer; nPosition: Integer; const pchEntryText: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_SetQueueEntry(@Self, nID, nPosition, pchEntryText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.SetCurrentQueueEntry(nID: Integer): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_SetCurrentQueueEntry(@Self, nID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.QueueDidChange(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_QueueDidChange(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.PlaylistWillChange(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_PlaylistWillChange(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.ResetPlaylistEntries(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_ResetPlaylistEntries(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.SetPlaylistEntry(nID: Integer; nPosition: Integer; const pchEntryText: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_SetPlaylistEntry(@Self, nID, nPosition, pchEntryText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.SetCurrentPlaylistEntry(nID: Integer): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_SetCurrentPlaylistEntry(@Self, nID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamMusicRemoteHelper.PlaylistDidChange(): Boolean;
begin
  Result := SteamAPI_ISteamMusicRemote_PlaylistDidChange(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.CreateHTTPRequest(eHTTPRequestMethod: EHTTPMethod; const pchAbsoluteURL: PAnsiChar): HTTPRequestHandle;
begin
  Result := SteamAPI_ISteamHTTP_CreateHTTPRequest(@Self, eHTTPRequestMethod, pchAbsoluteURL);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestContextValue(hRequest: HTTPRequestHandle; ulContextValue: UInt64): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestContextValue(@Self, hRequest, ulContextValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestNetworkActivityTimeout(hRequest: HTTPRequestHandle; unTimeoutSeconds: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestNetworkActivityTimeout(@Self, hRequest, unTimeoutSeconds);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestHeaderValue(hRequest: HTTPRequestHandle; const pchHeaderName: PAnsiChar; const pchHeaderValue: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestHeaderValue(@Self, hRequest, pchHeaderName, pchHeaderValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestGetOrPostParameter(hRequest: HTTPRequestHandle; const pchParamName: PAnsiChar; const pchParamValue: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestGetOrPostParameter(@Self, hRequest, pchParamName, pchParamValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SendHTTPRequest(hRequest: HTTPRequestHandle; pCallHandle: PSteamAPICall_t): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SendHTTPRequest(@Self, hRequest, pCallHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SendHTTPRequestAndStreamResponse(hRequest: HTTPRequestHandle; pCallHandle: PSteamAPICall_t): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SendHTTPRequestAndStreamResponse(@Self, hRequest, pCallHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.DeferHTTPRequest(hRequest: HTTPRequestHandle): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_DeferHTTPRequest(@Self, hRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.PrioritizeHTTPRequest(hRequest: HTTPRequestHandle): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_PrioritizeHTTPRequest(@Self, hRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.GetHTTPResponseHeaderSize(hRequest: HTTPRequestHandle; const pchHeaderName: PAnsiChar; unResponseHeaderSize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_GetHTTPResponseHeaderSize(@Self, hRequest, pchHeaderName, unResponseHeaderSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.GetHTTPResponseHeaderValue(hRequest: HTTPRequestHandle; const pchHeaderName: PAnsiChar; pHeaderValueBuffer: PUInt8; unBufferSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_GetHTTPResponseHeaderValue(@Self, hRequest, pchHeaderName, pHeaderValueBuffer, unBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.GetHTTPResponseBodySize(hRequest: HTTPRequestHandle; unBodySize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_GetHTTPResponseBodySize(@Self, hRequest, unBodySize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.GetHTTPResponseBodyData(hRequest: HTTPRequestHandle; pBodyDataBuffer: PUInt8; unBufferSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_GetHTTPResponseBodyData(@Self, hRequest, pBodyDataBuffer, unBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.GetHTTPStreamingResponseBodyData(hRequest: HTTPRequestHandle; cOffset: UInt32; pBodyDataBuffer: PUInt8; unBufferSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_GetHTTPStreamingResponseBodyData(@Self, hRequest, cOffset, pBodyDataBuffer, unBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.ReleaseHTTPRequest(hRequest: HTTPRequestHandle): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_ReleaseHTTPRequest(@Self, hRequest);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.GetHTTPDownloadProgressPct(hRequest: HTTPRequestHandle; pflPercentOut: PSingle): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_GetHTTPDownloadProgressPct(@Self, hRequest, pflPercentOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestRawPostBody(hRequest: HTTPRequestHandle; const pchContentType: PAnsiChar; pubBody: PUInt8; unBodyLen: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestRawPostBody(@Self, hRequest, pchContentType, pubBody, unBodyLen);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.CreateCookieContainer(bAllowResponsesToModify: Boolean): HTTPCookieContainerHandle;
begin
  Result := SteamAPI_ISteamHTTP_CreateCookieContainer(@Self, bAllowResponsesToModify);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.ReleaseCookieContainer(hCookieContainer: HTTPCookieContainerHandle): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_ReleaseCookieContainer(@Self, hCookieContainer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetCookie(hCookieContainer: HTTPCookieContainerHandle; const pchHost: PAnsiChar; const pchUrl: PAnsiChar; const pchCookie: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetCookie(@Self, hCookieContainer, pchHost, pchUrl, pchCookie);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestCookieContainer(hRequest: HTTPRequestHandle; hCookieContainer: HTTPCookieContainerHandle): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestCookieContainer(@Self, hRequest, hCookieContainer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestUserAgentInfo(hRequest: HTTPRequestHandle; const pchUserAgentInfo: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestUserAgentInfo(@Self, hRequest, pchUserAgentInfo);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestRequiresVerifiedCertificate(hRequest: HTTPRequestHandle; bRequireVerifiedCertificate: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestRequiresVerifiedCertificate(@Self, hRequest, bRequireVerifiedCertificate);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.SetHTTPRequestAbsoluteTimeoutMS(hRequest: HTTPRequestHandle; unMilliseconds: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_SetHTTPRequestAbsoluteTimeoutMS(@Self, hRequest, unMilliseconds);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTTPHelper.GetHTTPRequestWasTimedOut(hRequest: HTTPRequestHandle; pbWasTimedOut: PBoolean): Boolean;
begin
  Result := SteamAPI_ISteamHTTP_GetHTTPRequestWasTimedOut(@Self, hRequest, pbWasTimedOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.Init(bExplicitlyCallRunFrame: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamInput_Init(@Self, bExplicitlyCallRunFrame);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.Shutdown(): Boolean;
begin
  Result := SteamAPI_ISteamInput_Shutdown(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.SetInputActionManifestFilePath(const pchInputActionManifestAbsolutePath: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamInput_SetInputActionManifestFilePath(@Self, pchInputActionManifestAbsolutePath);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.RunFrame(bReservedValue: Boolean);
begin
  SteamAPI_ISteamInput_RunFrame(@Self, bReservedValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.BWaitForData(bWaitForever: Boolean; unTimeout: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInput_BWaitForData(@Self, bWaitForever, unTimeout);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.BNewDataAvailable(): Boolean;
begin
  Result := SteamAPI_ISteamInput_BNewDataAvailable(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetConnectedControllers(handlesOut: PInputHandle_t): Integer;
begin
  Result := SteamAPI_ISteamInput_GetConnectedControllers(@Self, handlesOut);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.EnableDeviceCallbacks();
begin
  SteamAPI_ISteamInput_EnableDeviceCallbacks(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.EnableActionEventCallbacks(pCallback: SteamInputActionEventCallbackPointer);
begin
  SteamAPI_ISteamInput_EnableActionEventCallbacks(@Self, pCallback);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetActionSetHandle(const pszActionSetName: PAnsiChar): InputActionSetHandle_t;
begin
  Result := SteamAPI_ISteamInput_GetActionSetHandle(@Self, pszActionSetName);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.ActivateActionSet(inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t);
begin
  SteamAPI_ISteamInput_ActivateActionSet(@Self, inputHandle, actionSetHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetCurrentActionSet(inputHandle: InputHandle_t): InputActionSetHandle_t;
begin
  Result := SteamAPI_ISteamInput_GetCurrentActionSet(@Self, inputHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.ActivateActionSetLayer(inputHandle: InputHandle_t; actionSetLayerHandle: InputActionSetHandle_t);
begin
  SteamAPI_ISteamInput_ActivateActionSetLayer(@Self, inputHandle, actionSetLayerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.DeactivateActionSetLayer(inputHandle: InputHandle_t; actionSetLayerHandle: InputActionSetHandle_t);
begin
  SteamAPI_ISteamInput_DeactivateActionSetLayer(@Self, inputHandle, actionSetLayerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.DeactivateAllActionSetLayers(inputHandle: InputHandle_t);
begin
  SteamAPI_ISteamInput_DeactivateAllActionSetLayers(@Self, inputHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetActiveActionSetLayers(inputHandle: InputHandle_t; handlesOut: PInputActionSetHandle_t): Integer;
begin
  Result := SteamAPI_ISteamInput_GetActiveActionSetLayers(@Self, inputHandle, handlesOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetDigitalActionHandle(const pszActionName: PAnsiChar): InputDigitalActionHandle_t;
begin
  Result := SteamAPI_ISteamInput_GetDigitalActionHandle(@Self, pszActionName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetDigitalActionData(inputHandle: InputHandle_t; digitalActionHandle: InputDigitalActionHandle_t): InputDigitalActionData_t;
begin
  Result := SteamAPI_ISteamInput_GetDigitalActionData(@Self, inputHandle, digitalActionHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetDigitalActionOrigins(inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t; digitalActionHandle: InputDigitalActionHandle_t; originsOut: PEInputActionOrigin): Integer;
begin
  Result := SteamAPI_ISteamInput_GetDigitalActionOrigins(@Self, inputHandle, actionSetHandle, digitalActionHandle, originsOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetStringForDigitalActionName(eActionHandle: InputDigitalActionHandle_t): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetStringForDigitalActionName(@Self, eActionHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetAnalogActionHandle(const pszActionName: PAnsiChar): InputAnalogActionHandle_t;
begin
  Result := SteamAPI_ISteamInput_GetAnalogActionHandle(@Self, pszActionName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetAnalogActionData(inputHandle: InputHandle_t; analogActionHandle: InputAnalogActionHandle_t): InputAnalogActionData_t;
begin
  Result := SteamAPI_ISteamInput_GetAnalogActionData(@Self, inputHandle, analogActionHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetAnalogActionOrigins(inputHandle: InputHandle_t; actionSetHandle: InputActionSetHandle_t; analogActionHandle: InputAnalogActionHandle_t; originsOut: PEInputActionOrigin): Integer;
begin
  Result := SteamAPI_ISteamInput_GetAnalogActionOrigins(@Self, inputHandle, actionSetHandle, analogActionHandle, originsOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetGlyphPNGForActionOrigin(eOrigin: EInputActionOrigin; eSize: ESteamInputGlyphSize; unFlags: UInt32): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin(@Self, eOrigin, eSize, unFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetGlyphSVGForActionOrigin(eOrigin: EInputActionOrigin; unFlags: UInt32): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin(@Self, eOrigin, unFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetGlyphForActionOrigin_Legacy(eOrigin: EInputActionOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetStringForActionOrigin(eOrigin: EInputActionOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetStringForActionOrigin(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetStringForAnalogActionName(eActionHandle: InputAnalogActionHandle_t): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetStringForAnalogActionName(@Self, eActionHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.StopAnalogActionMomentum(inputHandle: InputHandle_t; eAction: InputAnalogActionHandle_t);
begin
  SteamAPI_ISteamInput_StopAnalogActionMomentum(@Self, inputHandle, eAction);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetMotionData(inputHandle: InputHandle_t): InputMotionData_t;
begin
  Result := SteamAPI_ISteamInput_GetMotionData(@Self, inputHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.TriggerVibration(inputHandle: InputHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16);
begin
  SteamAPI_ISteamInput_TriggerVibration(@Self, inputHandle, usLeftSpeed, usRightSpeed);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.TriggerVibrationExtended(inputHandle: InputHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16; usLeftTriggerSpeed: UInt16; usRightTriggerSpeed: UInt16);
begin
  SteamAPI_ISteamInput_TriggerVibrationExtended(@Self, inputHandle, usLeftSpeed, usRightSpeed, usLeftTriggerSpeed, usRightTriggerSpeed);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.TriggerSimpleHapticEvent(inputHandle: InputHandle_t; eHapticLocation: EControllerHapticLocation; nIntensity: UInt8; nGainDB: AnsiChar; nOtherIntensity: UInt8; nOtherGainDB: AnsiChar);
begin
  SteamAPI_ISteamInput_TriggerSimpleHapticEvent(@Self, inputHandle, eHapticLocation, nIntensity, nGainDB, nOtherIntensity, nOtherGainDB);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.SetLEDColor(inputHandle: InputHandle_t; nColorR: UInt8; nColorG: UInt8; nColorB: UInt8; nFlags: UInt32);
begin
  SteamAPI_ISteamInput_SetLEDColor(@Self, inputHandle, nColorR, nColorG, nColorB, nFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.Legacy_TriggerHapticPulse(inputHandle: InputHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16);
begin
  SteamAPI_ISteamInput_Legacy_TriggerHapticPulse(@Self, inputHandle, eTargetPad, usDurationMicroSec);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInputHelper.Legacy_TriggerRepeatedHapticPulse(inputHandle: InputHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16; usOffMicroSec: UInt16; unRepeat: UInt16; nFlags: UInt32);
begin
  SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse(@Self, inputHandle, eTargetPad, usDurationMicroSec, usOffMicroSec, unRepeat, nFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.ShowBindingPanel(inputHandle: InputHandle_t): Boolean;
begin
  Result := SteamAPI_ISteamInput_ShowBindingPanel(@Self, inputHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetInputTypeForHandle(inputHandle: InputHandle_t): ESteamInputType;
begin
  Result := SteamAPI_ISteamInput_GetInputTypeForHandle(@Self, inputHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetControllerForGamepadIndex(nIndex: Integer): InputHandle_t;
begin
  Result := SteamAPI_ISteamInput_GetControllerForGamepadIndex(@Self, nIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetGamepadIndexForController(ulinputHandle: InputHandle_t): Integer;
begin
  Result := SteamAPI_ISteamInput_GetGamepadIndexForController(@Self, ulinputHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetStringForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetStringForXboxOrigin(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetGlyphForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamInput_GetGlyphForXboxOrigin(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetActionOriginFromXboxOrigin(inputHandle: InputHandle_t; eOrigin: EXboxOrigin): EInputActionOrigin;
begin
  Result := SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin(@Self, inputHandle, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.TranslateActionOrigin(eDestinationInputType: ESteamInputType; eSourceOrigin: EInputActionOrigin): EInputActionOrigin;
begin
  Result := SteamAPI_ISteamInput_TranslateActionOrigin(@Self, eDestinationInputType, eSourceOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetDeviceBindingRevision(inputHandle: InputHandle_t; pMajor: PInteger; pMinor: PInteger): Boolean;
begin
  Result := SteamAPI_ISteamInput_GetDeviceBindingRevision(@Self, inputHandle, pMajor, pMinor);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetRemotePlaySessionID(inputHandle: InputHandle_t): UInt32;
begin
  Result := SteamAPI_ISteamInput_GetRemotePlaySessionID(@Self, inputHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInputHelper.GetSessionInputConfigurationSettings(): UInt16;
begin
  Result := SteamAPI_ISteamInput_GetSessionInputConfigurationSettings(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.Init(): Boolean;
begin
  Result := SteamAPI_ISteamController_Init(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.Shutdown(): Boolean;
begin
  Result := SteamAPI_ISteamController_Shutdown(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.RunFrame();
begin
  SteamAPI_ISteamController_RunFrame(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetConnectedControllers(handlesOut: PControllerHandle_t): Integer;
begin
  Result := SteamAPI_ISteamController_GetConnectedControllers(@Self, handlesOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetActionSetHandle(const pszActionSetName: PAnsiChar): ControllerActionSetHandle_t;
begin
  Result := SteamAPI_ISteamController_GetActionSetHandle(@Self, pszActionSetName);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.ActivateActionSet(controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t);
begin
  SteamAPI_ISteamController_ActivateActionSet(@Self, controllerHandle, actionSetHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetCurrentActionSet(controllerHandle: ControllerHandle_t): ControllerActionSetHandle_t;
begin
  Result := SteamAPI_ISteamController_GetCurrentActionSet(@Self, controllerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.ActivateActionSetLayer(controllerHandle: ControllerHandle_t; actionSetLayerHandle: ControllerActionSetHandle_t);
begin
  SteamAPI_ISteamController_ActivateActionSetLayer(@Self, controllerHandle, actionSetLayerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.DeactivateActionSetLayer(controllerHandle: ControllerHandle_t; actionSetLayerHandle: ControllerActionSetHandle_t);
begin
  SteamAPI_ISteamController_DeactivateActionSetLayer(@Self, controllerHandle, actionSetLayerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.DeactivateAllActionSetLayers(controllerHandle: ControllerHandle_t);
begin
  SteamAPI_ISteamController_DeactivateAllActionSetLayers(@Self, controllerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetActiveActionSetLayers(controllerHandle: ControllerHandle_t; handlesOut: PControllerActionSetHandle_t): Integer;
begin
  Result := SteamAPI_ISteamController_GetActiveActionSetLayers(@Self, controllerHandle, handlesOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetDigitalActionHandle(const pszActionName: PAnsiChar): ControllerDigitalActionHandle_t;
begin
  Result := SteamAPI_ISteamController_GetDigitalActionHandle(@Self, pszActionName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetDigitalActionData(controllerHandle: ControllerHandle_t; digitalActionHandle: ControllerDigitalActionHandle_t): InputDigitalActionData_t;
begin
  Result := SteamAPI_ISteamController_GetDigitalActionData(@Self, controllerHandle, digitalActionHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetDigitalActionOrigins(controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t; digitalActionHandle: ControllerDigitalActionHandle_t; originsOut: PEControllerActionOrigin): Integer;
begin
  Result := SteamAPI_ISteamController_GetDigitalActionOrigins(@Self, controllerHandle, actionSetHandle, digitalActionHandle, originsOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetAnalogActionHandle(const pszActionName: PAnsiChar): ControllerAnalogActionHandle_t;
begin
  Result := SteamAPI_ISteamController_GetAnalogActionHandle(@Self, pszActionName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetAnalogActionData(controllerHandle: ControllerHandle_t; analogActionHandle: ControllerAnalogActionHandle_t): InputAnalogActionData_t;
begin
  Result := SteamAPI_ISteamController_GetAnalogActionData(@Self, controllerHandle, analogActionHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetAnalogActionOrigins(controllerHandle: ControllerHandle_t; actionSetHandle: ControllerActionSetHandle_t; analogActionHandle: ControllerAnalogActionHandle_t; originsOut: PEControllerActionOrigin): Integer;
begin
  Result := SteamAPI_ISteamController_GetAnalogActionOrigins(@Self, controllerHandle, actionSetHandle, analogActionHandle, originsOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetGlyphForActionOrigin(eOrigin: EControllerActionOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamController_GetGlyphForActionOrigin(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetStringForActionOrigin(eOrigin: EControllerActionOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamController_GetStringForActionOrigin(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.StopAnalogActionMomentum(controllerHandle: ControllerHandle_t; eAction: ControllerAnalogActionHandle_t);
begin
  SteamAPI_ISteamController_StopAnalogActionMomentum(@Self, controllerHandle, eAction);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetMotionData(controllerHandle: ControllerHandle_t): InputMotionData_t;
begin
  Result := SteamAPI_ISteamController_GetMotionData(@Self, controllerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.TriggerHapticPulse(controllerHandle: ControllerHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16);
begin
  SteamAPI_ISteamController_TriggerHapticPulse(@Self, controllerHandle, eTargetPad, usDurationMicroSec);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.TriggerRepeatedHapticPulse(controllerHandle: ControllerHandle_t; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16; usOffMicroSec: UInt16; unRepeat: UInt16; nFlags: UInt32);
begin
  SteamAPI_ISteamController_TriggerRepeatedHapticPulse(@Self, controllerHandle, eTargetPad, usDurationMicroSec, usOffMicroSec, unRepeat, nFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.TriggerVibration(controllerHandle: ControllerHandle_t; usLeftSpeed: UInt16; usRightSpeed: UInt16);
begin
  SteamAPI_ISteamController_TriggerVibration(@Self, controllerHandle, usLeftSpeed, usRightSpeed);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamControllerHelper.SetLEDColor(controllerHandle: ControllerHandle_t; nColorR: UInt8; nColorG: UInt8; nColorB: UInt8; nFlags: UInt32);
begin
  SteamAPI_ISteamController_SetLEDColor(@Self, controllerHandle, nColorR, nColorG, nColorB, nFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.ShowBindingPanel(controllerHandle: ControllerHandle_t): Boolean;
begin
  Result := SteamAPI_ISteamController_ShowBindingPanel(@Self, controllerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetInputTypeForHandle(controllerHandle: ControllerHandle_t): ESteamInputType;
begin
  Result := SteamAPI_ISteamController_GetInputTypeForHandle(@Self, controllerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetControllerForGamepadIndex(nIndex: Integer): ControllerHandle_t;
begin
  Result := SteamAPI_ISteamController_GetControllerForGamepadIndex(@Self, nIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetGamepadIndexForController(ulControllerHandle: ControllerHandle_t): Integer;
begin
  Result := SteamAPI_ISteamController_GetGamepadIndexForController(@Self, ulControllerHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetStringForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamController_GetStringForXboxOrigin(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetGlyphForXboxOrigin(eOrigin: EXboxOrigin): PAnsiChar;
begin
  Result := SteamAPI_ISteamController_GetGlyphForXboxOrigin(@Self, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetActionOriginFromXboxOrigin(controllerHandle: ControllerHandle_t; eOrigin: EXboxOrigin): EControllerActionOrigin;
begin
  Result := SteamAPI_ISteamController_GetActionOriginFromXboxOrigin(@Self, controllerHandle, eOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.TranslateActionOrigin(eDestinationInputType: ESteamInputType; eSourceOrigin: EControllerActionOrigin): EControllerActionOrigin;
begin
  Result := SteamAPI_ISteamController_TranslateActionOrigin(@Self, eDestinationInputType, eSourceOrigin);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamControllerHelper.GetControllerBindingRevision(controllerHandle: ControllerHandle_t; pMajor: PInteger; pMinor: PInteger): Boolean;
begin
  Result := SteamAPI_ISteamController_GetControllerBindingRevision(@Self, controllerHandle, pMajor, pMinor);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.CreateQueryUserUGCRequest(unAccountID: AccountID_t; eListType: EUserUGCList; eMatchingUGCType: EUGCMatchingUGCType; eSortOrder: EUserUGCListSortOrder; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; unPage: UInt32): UGCQueryHandle_t;
begin
  Result := SteamAPI_ISteamUGC_CreateQueryUserUGCRequest(@Self, unAccountID, eListType, eMatchingUGCType, eSortOrder, nCreatorAppID, nConsumerAppID, unPage);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.CreateQueryAllUGCRequest(eQueryType: EUGCQuery; eMatchingeMatchingUGCTypeFileType: EUGCMatchingUGCType; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; unPage: UInt32): UGCQueryHandle_t;
begin
  Result := SteamAPI_ISteamUGC_CreateQueryAllUGCRequestPage(@Self, eQueryType, eMatchingeMatchingUGCTypeFileType, nCreatorAppID, nConsumerAppID, unPage);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.CreateQueryAllUGCRequest(eQueryType: EUGCQuery; eMatchingeMatchingUGCTypeFileType: EUGCMatchingUGCType; nCreatorAppID: AppId_t; nConsumerAppID: AppId_t; const pchCursor: PAnsiChar): UGCQueryHandle_t;
begin
  Result := SteamAPI_ISteamUGC_CreateQueryAllUGCRequestCursor(@Self, eQueryType, eMatchingeMatchingUGCTypeFileType, nCreatorAppID, nConsumerAppID, pchCursor);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.CreateQueryUGCDetailsRequest(pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): UGCQueryHandle_t;
begin
  Result := SteamAPI_ISteamUGC_CreateQueryUGCDetailsRequest(@Self, pvecPublishedFileID, unNumPublishedFileIDs);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SendQueryUGCRequest(handle: UGCQueryHandle_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_SendQueryUGCRequest(@Self, handle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCResult(handle: UGCQueryHandle_t; index: UInt32; pDetails: PSteamUGCDetails_t): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCResult(@Self, handle, index, pDetails);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCNumTags(handle: UGCQueryHandle_t; index: UInt32): UInt32;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCNumTags(@Self, handle, index);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCTag(handle: UGCQueryHandle_t; index: UInt32; indexTag: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCTag(@Self, handle, index, indexTag, pchValue, cchValueSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCTagDisplayName(handle: UGCQueryHandle_t; index: UInt32; indexTag: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCTagDisplayName(@Self, handle, index, indexTag, pchValue, cchValueSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCPreviewURL(handle: UGCQueryHandle_t; index: UInt32; pchURL: PAnsiChar; cchURLSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCPreviewURL(@Self, handle, index, pchURL, cchURLSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCMetadata(handle: UGCQueryHandle_t; index: UInt32; pchMetadata: PAnsiChar; cchMetadatasize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCMetadata(@Self, handle, index, pchMetadata, cchMetadatasize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCChildren(handle: UGCQueryHandle_t; index: UInt32; pvecPublishedFileID: PPublishedFileId_t; cMaxEntries: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCChildren(@Self, handle, index, pvecPublishedFileID, cMaxEntries);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCStatistic(handle: UGCQueryHandle_t; index: UInt32; eStatType: EItemStatistic; pStatValue: PUInt64): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCStatistic(@Self, handle, index, eStatType, pStatValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCNumAdditionalPreviews(handle: UGCQueryHandle_t; index: UInt32): UInt32;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCNumAdditionalPreviews(@Self, handle, index);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCAdditionalPreview(handle: UGCQueryHandle_t; index: UInt32; previewIndex: UInt32; pchURLOrVideoID: PAnsiChar; cchURLSize: UInt32; pchOriginalFileName: PAnsiChar; cchOriginalFileNameSize: UInt32; pPreviewType: PEItemPreviewType): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCAdditionalPreview(@Self, handle, index, previewIndex, pchURLOrVideoID, cchURLSize, pchOriginalFileName, cchOriginalFileNameSize, pPreviewType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCNumKeyValueTags(handle: UGCQueryHandle_t; index: UInt32): UInt32;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCNumKeyValueTags(@Self, handle, index);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCKeyValueTag(handle: UGCQueryHandle_t; index: UInt32; keyValueTagIndex: UInt32; pchKey: PAnsiChar; cchKeySize: UInt32; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryUGCKeyValueTag(@Self, handle, index, keyValueTagIndex, pchKey, cchKeySize, pchValue, cchValueSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetQueryUGCKeyValueTag(handle: UGCQueryHandle_t; index: UInt32; const pchKey: PAnsiChar; pchValue: PAnsiChar; cchValueSize: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetQueryFirstUGCKeyValueTag(@Self, handle, index, pchKey, pchValue, cchValueSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.ReleaseQueryUGCRequest(handle: UGCQueryHandle_t): Boolean;
begin
  Result := SteamAPI_ISteamUGC_ReleaseQueryUGCRequest(@Self, handle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddRequiredTag(handle: UGCQueryHandle_t; const pTagName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_AddRequiredTag(@Self, handle, pTagName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddRequiredTagGroup(handle: UGCQueryHandle_t; const pTagGroups: PSteamParamStringArray_t): Boolean;
begin
  Result := SteamAPI_ISteamUGC_AddRequiredTagGroup(@Self, handle, pTagGroups);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddExcludedTag(handle: UGCQueryHandle_t; const pTagName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_AddExcludedTag(@Self, handle, pTagName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnOnlyIDs(handle: UGCQueryHandle_t; bReturnOnlyIDs: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnOnlyIDs(@Self, handle, bReturnOnlyIDs);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnKeyValueTags(handle: UGCQueryHandle_t; bReturnKeyValueTags: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnKeyValueTags(@Self, handle, bReturnKeyValueTags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnLongDescription(handle: UGCQueryHandle_t; bReturnLongDescription: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnLongDescription(@Self, handle, bReturnLongDescription);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnMetadata(handle: UGCQueryHandle_t; bReturnMetadata: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnMetadata(@Self, handle, bReturnMetadata);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnChildren(handle: UGCQueryHandle_t; bReturnChildren: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnChildren(@Self, handle, bReturnChildren);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnAdditionalPreviews(handle: UGCQueryHandle_t; bReturnAdditionalPreviews: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnAdditionalPreviews(@Self, handle, bReturnAdditionalPreviews);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnTotalOnly(handle: UGCQueryHandle_t; bReturnTotalOnly: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnTotalOnly(@Self, handle, bReturnTotalOnly);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetReturnPlaytimeStats(handle: UGCQueryHandle_t; unDays: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetReturnPlaytimeStats(@Self, handle, unDays);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetLanguage(handle: UGCQueryHandle_t; const pchLanguage: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetLanguage(@Self, handle, pchLanguage);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetAllowCachedResponse(handle: UGCQueryHandle_t; unMaxAgeSeconds: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetAllowCachedResponse(@Self, handle, unMaxAgeSeconds);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetCloudFileNameFilter(handle: UGCQueryHandle_t; const pMatchCloudFileName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetCloudFileNameFilter(@Self, handle, pMatchCloudFileName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetMatchAnyTag(handle: UGCQueryHandle_t; bMatchAnyTag: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetMatchAnyTag(@Self, handle, bMatchAnyTag);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetSearchText(handle: UGCQueryHandle_t; const pSearchText: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetSearchText(@Self, handle, pSearchText);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetRankedByTrendDays(handle: UGCQueryHandle_t; unDays: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetRankedByTrendDays(@Self, handle, unDays);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetTimeCreatedDateRange(handle: UGCQueryHandle_t; rtStart: RTime32; rtEnd: RTime32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetTimeCreatedDateRange(@Self, handle, rtStart, rtEnd);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetTimeUpdatedDateRange(handle: UGCQueryHandle_t; rtStart: RTime32; rtEnd: RTime32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetTimeUpdatedDateRange(@Self, handle, rtStart, rtEnd);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddRequiredKeyValueTag(handle: UGCQueryHandle_t; const pKey: PAnsiChar; const pValue: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_AddRequiredKeyValueTag(@Self, handle, pKey, pValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.RequestUGCDetails(nPublishedFileID: PublishedFileId_t; unMaxAgeSeconds: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_RequestUGCDetails(@Self, nPublishedFileID, unMaxAgeSeconds);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.CreateItem(nConsumerAppId: AppId_t; eFileType: EWorkshopFileType): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_CreateItem(@Self, nConsumerAppId, eFileType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.StartItemUpdate(nConsumerAppId: AppId_t; nPublishedFileID: PublishedFileId_t): UGCUpdateHandle_t;
begin
  Result := SteamAPI_ISteamUGC_StartItemUpdate(@Self, nConsumerAppId, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemTitle(handle: UGCUpdateHandle_t; const pchTitle: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemTitle(@Self, handle, pchTitle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemDescription(handle: UGCUpdateHandle_t; const pchDescription: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemDescription(@Self, handle, pchDescription);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemUpdateLanguage(handle: UGCUpdateHandle_t; const pchLanguage: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemUpdateLanguage(@Self, handle, pchLanguage);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemMetadata(handle: UGCUpdateHandle_t; const pchMetaData: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemMetadata(@Self, handle, pchMetaData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemVisibility(handle: UGCUpdateHandle_t; eVisibility: ERemoteStoragePublishedFileVisibility): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemVisibility(@Self, handle, eVisibility);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemTags(updateHandle: UGCUpdateHandle_t; const pTags: PSteamParamStringArray_t): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemTags(@Self, updateHandle, pTags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemContent(handle: UGCUpdateHandle_t; const pszContentFolder: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemContent(@Self, handle, pszContentFolder);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetItemPreview(handle: UGCUpdateHandle_t; const pszPreviewFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetItemPreview(@Self, handle, pszPreviewFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetAllowLegacyUpload(handle: UGCUpdateHandle_t; bAllowLegacyUpload: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_SetAllowLegacyUpload(@Self, handle, bAllowLegacyUpload);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.RemoveAllItemKeyValueTags(handle: UGCUpdateHandle_t): Boolean;
begin
  Result := SteamAPI_ISteamUGC_RemoveAllItemKeyValueTags(@Self, handle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.RemoveItemKeyValueTags(handle: UGCUpdateHandle_t; const pchKey: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_RemoveItemKeyValueTags(@Self, handle, pchKey);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddItemKeyValueTag(handle: UGCUpdateHandle_t; const pchKey: PAnsiChar; const pchValue: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_AddItemKeyValueTag(@Self, handle, pchKey, pchValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddItemPreviewFile(handle: UGCUpdateHandle_t; const pszPreviewFile: PAnsiChar; aType: EItemPreviewType): Boolean;
begin
  Result := SteamAPI_ISteamUGC_AddItemPreviewFile(@Self, handle, pszPreviewFile, aType);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddItemPreviewVideo(handle: UGCUpdateHandle_t; const pszVideoID: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_AddItemPreviewVideo(@Self, handle, pszVideoID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.UpdateItemPreviewFile(handle: UGCUpdateHandle_t; index: UInt32; const pszPreviewFile: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_UpdateItemPreviewFile(@Self, handle, index, pszPreviewFile);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.UpdateItemPreviewVideo(handle: UGCUpdateHandle_t; index: UInt32; const pszVideoID: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_UpdateItemPreviewVideo(@Self, handle, index, pszVideoID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.RemoveItemPreview(handle: UGCUpdateHandle_t; index: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_RemoveItemPreview(@Self, handle, index);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SubmitItemUpdate(handle: UGCUpdateHandle_t; const pchChangeNote: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_SubmitItemUpdate(@Self, handle, pchChangeNote);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetItemUpdateProgress(handle: UGCUpdateHandle_t; punBytesProcessed: PUInt64; punBytesTotal: PUInt64): EItemUpdateStatus;
begin
  Result := SteamAPI_ISteamUGC_GetItemUpdateProgress(@Self, handle, punBytesProcessed, punBytesTotal);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SetUserItemVote(nPublishedFileID: PublishedFileId_t; bVoteUp: Boolean): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_SetUserItemVote(@Self, nPublishedFileID, bVoteUp);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetUserItemVote(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_GetUserItemVote(@Self, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddItemToFavorites(nAppId: AppId_t; nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_AddItemToFavorites(@Self, nAppId, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.RemoveItemFromFavorites(nAppId: AppId_t; nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_RemoveItemFromFavorites(@Self, nAppId, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.SubscribeItem(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_SubscribeItem(@Self, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.UnsubscribeItem(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_UnsubscribeItem(@Self, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetNumSubscribedItems(): UInt32;
begin
  Result := SteamAPI_ISteamUGC_GetNumSubscribedItems(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetSubscribedItems(pvecPublishedFileID: PPublishedFileId_t; cMaxEntries: UInt32): UInt32;
begin
  Result := SteamAPI_ISteamUGC_GetSubscribedItems(@Self, pvecPublishedFileID, cMaxEntries);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetItemState(nPublishedFileID: PublishedFileId_t): UInt32;
begin
  Result := SteamAPI_ISteamUGC_GetItemState(@Self, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetItemInstallInfo(nPublishedFileID: PublishedFileId_t; punSizeOnDisk: PUInt64; pchFolder: PAnsiChar; cchFolderSize: UInt32; punTimeStamp: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetItemInstallInfo(@Self, nPublishedFileID, punSizeOnDisk, pchFolder, cchFolderSize, punTimeStamp);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetItemDownloadInfo(nPublishedFileID: PublishedFileId_t; punBytesDownloaded: PUInt64; punBytesTotal: PUInt64): Boolean;
begin
  Result := SteamAPI_ISteamUGC_GetItemDownloadInfo(@Self, nPublishedFileID, punBytesDownloaded, punBytesTotal);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.DownloadItem(nPublishedFileID: PublishedFileId_t; bHighPriority: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamUGC_DownloadItem(@Self, nPublishedFileID, bHighPriority);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.BInitWorkshopForGameServer(unWorkshopDepotID: DepotId_t; const pszFolder: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamUGC_BInitWorkshopForGameServer(@Self, unWorkshopDepotID, pszFolder);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamUGCHelper.SuspendDownloads(bSuspend: Boolean);
begin
  SteamAPI_ISteamUGC_SuspendDownloads(@Self, bSuspend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.StartPlaytimeTracking(pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_StartPlaytimeTracking(@Self, pvecPublishedFileID, unNumPublishedFileIDs);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.StopPlaytimeTracking(pvecPublishedFileID: PPublishedFileId_t; unNumPublishedFileIDs: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_StopPlaytimeTracking(@Self, pvecPublishedFileID, unNumPublishedFileIDs);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.StopPlaytimeTrackingForAllItems(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_StopPlaytimeTrackingForAllItems(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddDependency(nParentPublishedFileID: PublishedFileId_t; nChildPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_AddDependency(@Self, nParentPublishedFileID, nChildPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.RemoveDependency(nParentPublishedFileID: PublishedFileId_t; nChildPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_RemoveDependency(@Self, nParentPublishedFileID, nChildPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.AddAppDependency(nPublishedFileID: PublishedFileId_t; nAppID: AppId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_AddAppDependency(@Self, nPublishedFileID, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.RemoveAppDependency(nPublishedFileID: PublishedFileId_t; nAppID: AppId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_RemoveAppDependency(@Self, nPublishedFileID, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetAppDependencies(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_GetAppDependencies(@Self, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.DeleteItem(nPublishedFileID: PublishedFileId_t): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_DeleteItem(@Self, nPublishedFileID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.ShowWorkshopEULA(): Boolean;
begin
  Result := SteamAPI_ISteamUGC_ShowWorkshopEULA(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamUGCHelper.GetWorkshopEULAStatus(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamUGC_GetWorkshopEULAStatus(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppListHelper.GetNumInstalledApps(): UInt32;
begin
  Result := SteamAPI_ISteamAppList_GetNumInstalledApps(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppListHelper.GetInstalledApps(pvecAppID: PAppId_t; unMaxAppIDs: UInt32): UInt32;
begin
  Result := SteamAPI_ISteamAppList_GetInstalledApps(@Self, pvecAppID, unMaxAppIDs);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppListHelper.GetAppName(nAppID: AppId_t; pchName: PAnsiChar; cchNameMax: Integer): Integer;
begin
  Result := SteamAPI_ISteamAppList_GetAppName(@Self, nAppID, pchName, cchNameMax);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppListHelper.GetAppInstallDir(nAppID: AppId_t; pchDirectory: PAnsiChar; cchNameMax: Integer): Integer;
begin
  Result := SteamAPI_ISteamAppList_GetAppInstallDir(@Self, nAppID, pchDirectory, cchNameMax);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamAppListHelper.GetAppBuildId(nAppID: AppId_t): Integer;
begin
  Result := SteamAPI_ISteamAppList_GetAppBuildId(@Self, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTMLSurfaceHelper.Init(): Boolean;
begin
  Result := SteamAPI_ISteamHTMLSurface_Init(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTMLSurfaceHelper.Shutdown(): Boolean;
begin
  Result := SteamAPI_ISteamHTMLSurface_Shutdown(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamHTMLSurfaceHelper.CreateBrowser(const pchUserAgent: PAnsiChar; const pchUserCSS: PAnsiChar): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamHTMLSurface_CreateBrowser(@Self, pchUserAgent, pchUserCSS);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.RemoveBrowser(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_RemoveBrowser(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.LoadURL(unBrowserHandle: HHTMLBrowser; const pchURL: PAnsiChar; const pchPostData: PAnsiChar);
begin
  SteamAPI_ISteamHTMLSurface_LoadURL(@Self, unBrowserHandle, pchURL, pchPostData);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetSize(unBrowserHandle: HHTMLBrowser; unWidth: UInt32; unHeight: UInt32);
begin
  SteamAPI_ISteamHTMLSurface_SetSize(@Self, unBrowserHandle, unWidth, unHeight);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.StopLoad(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_StopLoad(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.Reload(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_Reload(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.GoBack(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_GoBack(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.GoForward(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_GoForward(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.AddHeader(unBrowserHandle: HHTMLBrowser; const pchKey: PAnsiChar; const pchValue: PAnsiChar);
begin
  SteamAPI_ISteamHTMLSurface_AddHeader(@Self, unBrowserHandle, pchKey, pchValue);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.ExecuteJavascript(unBrowserHandle: HHTMLBrowser; const pchScript: PAnsiChar);
begin
  SteamAPI_ISteamHTMLSurface_ExecuteJavascript(@Self, unBrowserHandle, pchScript);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.MouseUp(unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton);
begin
  SteamAPI_ISteamHTMLSurface_MouseUp(@Self, unBrowserHandle, eMouseButton);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.MouseDown(unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton);
begin
  SteamAPI_ISteamHTMLSurface_MouseDown(@Self, unBrowserHandle, eMouseButton);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.MouseDoubleClick(unBrowserHandle: HHTMLBrowser; eMouseButton: ISteamHTMLSurface__EHTMLMouseButton);
begin
  SteamAPI_ISteamHTMLSurface_MouseDoubleClick(@Self, unBrowserHandle, eMouseButton);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.MouseMove(unBrowserHandle: HHTMLBrowser; x: Integer; y: Integer);
begin
  SteamAPI_ISteamHTMLSurface_MouseMove(@Self, unBrowserHandle, x, y);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.MouseWheel(unBrowserHandle: HHTMLBrowser; nDelta: Int32);
begin
  SteamAPI_ISteamHTMLSurface_MouseWheel(@Self, unBrowserHandle, nDelta);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.KeyDown(unBrowserHandle: HHTMLBrowser; nNativeKeyCode: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers; bIsSystemKey: Boolean);
begin
  SteamAPI_ISteamHTMLSurface_KeyDown(@Self, unBrowserHandle, nNativeKeyCode, eHTMLKeyModifiers, bIsSystemKey);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.KeyUp(unBrowserHandle: HHTMLBrowser; nNativeKeyCode: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers);
begin
  SteamAPI_ISteamHTMLSurface_KeyUp(@Self, unBrowserHandle, nNativeKeyCode, eHTMLKeyModifiers);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.KeyChar(unBrowserHandle: HHTMLBrowser; cUnicodeChar: UInt32; eHTMLKeyModifiers: ISteamHTMLSurface__EHTMLKeyModifiers);
begin
  SteamAPI_ISteamHTMLSurface_KeyChar(@Self, unBrowserHandle, cUnicodeChar, eHTMLKeyModifiers);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetHorizontalScroll(unBrowserHandle: HHTMLBrowser; nAbsolutePixelScroll: UInt32);
begin
  SteamAPI_ISteamHTMLSurface_SetHorizontalScroll(@Self, unBrowserHandle, nAbsolutePixelScroll);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetVerticalScroll(unBrowserHandle: HHTMLBrowser; nAbsolutePixelScroll: UInt32);
begin
  SteamAPI_ISteamHTMLSurface_SetVerticalScroll(@Self, unBrowserHandle, nAbsolutePixelScroll);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetKeyFocus(unBrowserHandle: HHTMLBrowser; bHasKeyFocus: Boolean);
begin
  SteamAPI_ISteamHTMLSurface_SetKeyFocus(@Self, unBrowserHandle, bHasKeyFocus);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.ViewSource(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_ViewSource(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.CopyToClipboard(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_CopyToClipboard(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.PasteFromClipboard(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_PasteFromClipboard(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.Find(unBrowserHandle: HHTMLBrowser; const pchSearchStr: PAnsiChar; bCurrentlyInFind: Boolean; bReverse: Boolean);
begin
  SteamAPI_ISteamHTMLSurface_Find(@Self, unBrowserHandle, pchSearchStr, bCurrentlyInFind, bReverse);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.StopFind(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_StopFind(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.GetLinkAtPosition(unBrowserHandle: HHTMLBrowser; x: Integer; y: Integer);
begin
  SteamAPI_ISteamHTMLSurface_GetLinkAtPosition(@Self, unBrowserHandle, x, y);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetCookie(const pchHostname: PAnsiChar; const pchKey: PAnsiChar; const pchValue: PAnsiChar; const pchPath: PAnsiChar; nExpires: RTime32; bSecure: Boolean; bHTTPOnly: Boolean);
begin
  SteamAPI_ISteamHTMLSurface_SetCookie(@Self, pchHostname, pchKey, pchValue, pchPath, nExpires, bSecure, bHTTPOnly);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetPageScaleFactor(unBrowserHandle: HHTMLBrowser; flZoom: Single; nPointX: Integer; nPointY: Integer);
begin
  SteamAPI_ISteamHTMLSurface_SetPageScaleFactor(@Self, unBrowserHandle, flZoom, nPointX, nPointY);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetBackgroundMode(unBrowserHandle: HHTMLBrowser; bBackgroundMode: Boolean);
begin
  SteamAPI_ISteamHTMLSurface_SetBackgroundMode(@Self, unBrowserHandle, bBackgroundMode);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.SetDPIScalingFactor(unBrowserHandle: HHTMLBrowser; flDPIScaling: Single);
begin
  SteamAPI_ISteamHTMLSurface_SetDPIScalingFactor(@Self, unBrowserHandle, flDPIScaling);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.OpenDeveloperTools(unBrowserHandle: HHTMLBrowser);
begin
  SteamAPI_ISteamHTMLSurface_OpenDeveloperTools(@Self, unBrowserHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.AllowStartRequest(unBrowserHandle: HHTMLBrowser; bAllowed: Boolean);
begin
  SteamAPI_ISteamHTMLSurface_AllowStartRequest(@Self, unBrowserHandle, bAllowed);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.JSDialogResponse(unBrowserHandle: HHTMLBrowser; bResult: Boolean);
begin
  SteamAPI_ISteamHTMLSurface_JSDialogResponse(@Self, unBrowserHandle, bResult);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamHTMLSurfaceHelper.FileLoadDialogResponse(unBrowserHandle: HHTMLBrowser; const pchSelectedFiles: PPAnsiChar);
begin
  SteamAPI_ISteamHTMLSurface_FileLoadDialogResponse(@Self, unBrowserHandle, pchSelectedFiles);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetResultStatus(resultHandle: SteamInventoryResult_t): EResult;
begin
  Result := SteamAPI_ISteamInventory_GetResultStatus(@Self, resultHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetResultItems(resultHandle: SteamInventoryResult_t; pOutItemsArray: PSteamItemDetails_t; punOutItemsArraySize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetResultItems(@Self, resultHandle, pOutItemsArray, punOutItemsArraySize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetResultItemProperty(resultHandle: SteamInventoryResult_t; unItemIndex: UInt32; const pchPropertyName: PAnsiChar; pchValueBuffer: PAnsiChar; punValueBufferSizeOut: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetResultItemProperty(@Self, resultHandle, unItemIndex, pchPropertyName, pchValueBuffer, punValueBufferSizeOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetResultTimestamp(resultHandle: SteamInventoryResult_t): UInt32;
begin
  Result := SteamAPI_ISteamInventory_GetResultTimestamp(@Self, resultHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.CheckResultSteamID(resultHandle: SteamInventoryResult_t; steamIDExpected: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamInventory_CheckResultSteamID(@Self, resultHandle, steamIDExpected);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInventoryHelper.DestroyResult(resultHandle: SteamInventoryResult_t);
begin
  SteamAPI_ISteamInventory_DestroyResult(@Self, resultHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetAllItems(pResultHandle: PSteamInventoryResult_t): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetAllItems(@Self, pResultHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetItemsByID(pResultHandle: PSteamInventoryResult_t; const pInstanceIDs: PSteamItemInstanceID_t; unCountInstanceIDs: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetItemsByID(@Self, pResultHandle, pInstanceIDs, unCountInstanceIDs);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.SerializeResult(resultHandle: SteamInventoryResult_t; pOutBuffer: Pointer; punOutBufferSize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_SerializeResult(@Self, resultHandle, pOutBuffer, punOutBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.DeserializeResult(pOutResultHandle: PSteamInventoryResult_t; const pBuffer: Pointer; unBufferSize: UInt32; bRESERVED_MUST_BE_FALSE: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamInventory_DeserializeResult(@Self, pOutResultHandle, pBuffer, unBufferSize, bRESERVED_MUST_BE_FALSE);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GenerateItems(pResultHandle: PSteamInventoryResult_t; const pArrayItemDefs: PSteamItemDef_t; const punArrayQuantity: PUInt32; unArrayLength: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GenerateItems(@Self, pResultHandle, pArrayItemDefs, punArrayQuantity, unArrayLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GrantPromoItems(pResultHandle: PSteamInventoryResult_t): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GrantPromoItems(@Self, pResultHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.AddPromoItem(pResultHandle: PSteamInventoryResult_t; itemDef: SteamItemDef_t): Boolean;
begin
  Result := SteamAPI_ISteamInventory_AddPromoItem(@Self, pResultHandle, itemDef);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.AddPromoItems(pResultHandle: PSteamInventoryResult_t; const pArrayItemDefs: PSteamItemDef_t; unArrayLength: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_AddPromoItems(@Self, pResultHandle, pArrayItemDefs, unArrayLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.ConsumeItem(pResultHandle: PSteamInventoryResult_t; itemConsume: SteamItemInstanceID_t; unQuantity: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_ConsumeItem(@Self, pResultHandle, itemConsume, unQuantity);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.ExchangeItems(pResultHandle: PSteamInventoryResult_t; const pArrayGenerate: PSteamItemDef_t; const punArrayGenerateQuantity: PUInt32; unArrayGenerateLength: UInt32; const pArrayDestroy: PSteamItemInstanceID_t; const punArrayDestroyQuantity: PUInt32; unArrayDestroyLength: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_ExchangeItems(@Self, pResultHandle, pArrayGenerate, punArrayGenerateQuantity, unArrayGenerateLength, pArrayDestroy, punArrayDestroyQuantity, unArrayDestroyLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.TransferItemQuantity(pResultHandle: PSteamInventoryResult_t; itemIdSource: SteamItemInstanceID_t; unQuantity: UInt32; itemIdDest: SteamItemInstanceID_t): Boolean;
begin
  Result := SteamAPI_ISteamInventory_TransferItemQuantity(@Self, pResultHandle, itemIdSource, unQuantity, itemIdDest);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamInventoryHelper.SendItemDropHeartbeat();
begin
  SteamAPI_ISteamInventory_SendItemDropHeartbeat(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.TriggerItemDrop(pResultHandle: PSteamInventoryResult_t; dropListDefinition: SteamItemDef_t): Boolean;
begin
  Result := SteamAPI_ISteamInventory_TriggerItemDrop(@Self, pResultHandle, dropListDefinition);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.TradeItems(pResultHandle: PSteamInventoryResult_t; steamIDTradePartner: CSteamID; const pArrayGive: PSteamItemInstanceID_t; const pArrayGiveQuantity: PUInt32; nArrayGiveLength: UInt32; const pArrayGet: PSteamItemInstanceID_t; const pArrayGetQuantity: PUInt32; nArrayGetLength: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_TradeItems(@Self, pResultHandle, steamIDTradePartner, pArrayGive, pArrayGiveQuantity, nArrayGiveLength, pArrayGet, pArrayGetQuantity, nArrayGetLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.LoadItemDefinitions(): Boolean;
begin
  Result := SteamAPI_ISteamInventory_LoadItemDefinitions(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetItemDefinitionIDs(pItemDefIDs: PSteamItemDef_t; punItemDefIDsArraySize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetItemDefinitionIDs(@Self, pItemDefIDs, punItemDefIDsArraySize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetItemDefinitionProperty(iDefinition: SteamItemDef_t; const pchPropertyName: PAnsiChar; pchValueBuffer: PAnsiChar; punValueBufferSizeOut: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetItemDefinitionProperty(@Self, iDefinition, pchPropertyName, pchValueBuffer, punValueBufferSizeOut);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.RequestEligiblePromoItemDefinitionsIDs(steamID: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamInventory_RequestEligiblePromoItemDefinitionsIDs(@Self, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetEligiblePromoItemDefinitionIDs(steamID: CSteamID; pItemDefIDs: PSteamItemDef_t; punItemDefIDsArraySize: PUInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetEligiblePromoItemDefinitionIDs(@Self, steamID, pItemDefIDs, punItemDefIDsArraySize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.StartPurchase(const pArrayItemDefs: PSteamItemDef_t; const punArrayQuantity: PUInt32; unArrayLength: UInt32): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamInventory_StartPurchase(@Self, pArrayItemDefs, punArrayQuantity, unArrayLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.RequestPrices(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamInventory_RequestPrices(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetNumItemsWithPrices(): UInt32;
begin
  Result := SteamAPI_ISteamInventory_GetNumItemsWithPrices(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetItemsWithPrices(pArrayItemDefs: PSteamItemDef_t; pCurrentPrices: PUInt64; pBasePrices: PUInt64; unArrayLength: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetItemsWithPrices(@Self, pArrayItemDefs, pCurrentPrices, pBasePrices, unArrayLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.GetItemPrice(iDefinition: SteamItemDef_t; pCurrentPrice: PUInt64; pBasePrice: PUInt64): Boolean;
begin
  Result := SteamAPI_ISteamInventory_GetItemPrice(@Self, iDefinition, pCurrentPrice, pBasePrice);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.StartUpdateProperties(): SteamInventoryUpdateHandle_t;
begin
  Result := SteamAPI_ISteamInventory_StartUpdateProperties(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.RemoveProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamInventory_RemoveProperty(@Self, handle, nItemID, pchPropertyName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; const pchPropertyValue: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamInventory_SetPropertyString(@Self, handle, nItemID, pchPropertyName, pchPropertyValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; bValue: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamInventory_SetPropertyBool(@Self, handle, nItemID, pchPropertyName, bValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; nValue: Int64): Boolean;
begin
  Result := SteamAPI_ISteamInventory_SetPropertyInt64(@Self, handle, nItemID, pchPropertyName, nValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.SetProperty(handle: SteamInventoryUpdateHandle_t; nItemID: SteamItemInstanceID_t; const pchPropertyName: PAnsiChar; flValue: Single): Boolean;
begin
  Result := SteamAPI_ISteamInventory_SetPropertyFloat(@Self, handle, nItemID, pchPropertyName, flValue);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.SubmitUpdateProperties(handle: SteamInventoryUpdateHandle_t; pResultHandle: PSteamInventoryResult_t): Boolean;
begin
  Result := SteamAPI_ISteamInventory_SubmitUpdateProperties(@Self, handle, pResultHandle);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamInventoryHelper.InspectItem(pResultHandle: PSteamInventoryResult_t; const pchItemToken: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamInventory_InspectItem(@Self, pResultHandle, pchItemToken);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamVideoHelper.GetVideoURL(unVideoAppID: AppId_t);
begin
  SteamAPI_ISteamVideo_GetVideoURL(@Self, unVideoAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamVideoHelper.IsBroadcasting(pnNumViewers: PInteger): Boolean;
begin
  Result := SteamAPI_ISteamVideo_IsBroadcasting(@Self, pnNumViewers);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamVideoHelper.GetOPFSettings(unVideoAppID: AppId_t);
begin
  SteamAPI_ISteamVideo_GetOPFSettings(@Self, unVideoAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamVideoHelper.GetOPFStringForApp(unVideoAppID: AppId_t; pchBuffer: PAnsiChar; pnBufferSize: PInt32): Boolean;
begin
  Result := SteamAPI_ISteamVideo_GetOPFStringForApp(@Self, unVideoAppID, pchBuffer, pnBufferSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamParentalSettingsHelper.BIsParentalLockEnabled(): Boolean;
begin
  Result := SteamAPI_ISteamParentalSettings_BIsParentalLockEnabled(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamParentalSettingsHelper.BIsParentalLockLocked(): Boolean;
begin
  Result := SteamAPI_ISteamParentalSettings_BIsParentalLockLocked(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamParentalSettingsHelper.BIsAppBlocked(nAppID: AppId_t): Boolean;
begin
  Result := SteamAPI_ISteamParentalSettings_BIsAppBlocked(@Self, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamParentalSettingsHelper.BIsAppInBlockList(nAppID: AppId_t): Boolean;
begin
  Result := SteamAPI_ISteamParentalSettings_BIsAppInBlockList(@Self, nAppID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamParentalSettingsHelper.BIsFeatureBlocked(eFeature: EParentalFeature): Boolean;
begin
  Result := SteamAPI_ISteamParentalSettings_BIsFeatureBlocked(@Self, eFeature);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamParentalSettingsHelper.BIsFeatureInBlockList(eFeature: EParentalFeature): Boolean;
begin
  Result := SteamAPI_ISteamParentalSettings_BIsFeatureInBlockList(@Self, eFeature);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemotePlayHelper.GetSessionCount(): UInt32;
begin
  Result := SteamAPI_ISteamRemotePlay_GetSessionCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemotePlayHelper.GetSessionID(iSessionIndex: Integer): RemotePlaySessionID_t;
begin
  Result := SteamAPI_ISteamRemotePlay_GetSessionID(@Self, iSessionIndex);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemotePlayHelper.GetSessionSteamID(unSessionID: RemotePlaySessionID_t): CSteamID;
begin
  Result := SteamAPI_ISteamRemotePlay_GetSessionSteamID(@Self, unSessionID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemotePlayHelper.GetSessionClientName(unSessionID: RemotePlaySessionID_t): PAnsiChar;
begin
  Result := SteamAPI_ISteamRemotePlay_GetSessionClientName(@Self, unSessionID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemotePlayHelper.GetSessionClientFormFactor(unSessionID: RemotePlaySessionID_t): ESteamDeviceFormFactor;
begin
  Result := SteamAPI_ISteamRemotePlay_GetSessionClientFormFactor(@Self, unSessionID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemotePlayHelper.BGetSessionClientResolution(unSessionID: RemotePlaySessionID_t; pnResolutionX: PInteger; pnResolutionY: PInteger): Boolean;
begin
  Result := SteamAPI_ISteamRemotePlay_BGetSessionClientResolution(@Self, unSessionID, pnResolutionX, pnResolutionY);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamRemotePlayHelper.BSendRemotePlayTogetherInvite(steamIDFriend: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamRemotePlay_BSendRemotePlayTogetherInvite(@Self, steamIDFriend);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingMessagesHelper.SendMessageToUser(constref identityRemote: SteamNetworkingIdentity; const pubData: Pointer; cubData: UInt32; nSendFlags: Integer; nRemoteChannel: Integer): EResult;
begin
  Result := SteamAPI_ISteamNetworkingMessages_SendMessageToUser(@Self, @identityRemote, pubData, cubData, nSendFlags, nRemoteChannel);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingMessagesHelper.ReceiveMessagesOnChannel(nLocalChannel: Integer; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingMessages_ReceiveMessagesOnChannel(@Self, nLocalChannel, ppOutMessages, nMaxMessages);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingMessagesHelper.AcceptSessionWithUser(constref identityRemote: SteamNetworkingIdentity): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingMessages_AcceptSessionWithUser(@Self, @identityRemote);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingMessagesHelper.CloseSessionWithUser(constref identityRemote: SteamNetworkingIdentity): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingMessages_CloseSessionWithUser(@Self, @identityRemote);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingMessagesHelper.CloseChannelWithUser(constref identityRemote: SteamNetworkingIdentity; nLocalChannel: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingMessages_CloseChannelWithUser(@Self, @identityRemote, nLocalChannel);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingMessagesHelper.GetSessionConnectionInfo(constref identityRemote: SteamNetworkingIdentity; pConnectionInfo: PSteamNetConnectionInfo_t; pQuickStatus: PSteamNetConnectionRealTimeStatus_t): ESteamNetworkingConnectionState;
begin
  Result := SteamAPI_ISteamNetworkingMessages_GetSessionConnectionInfo(@Self, @identityRemote, pConnectionInfo, pQuickStatus);
end;
{$ENDIF}

function ISteamNetworkingSocketsHelper.CreateListenSocketIP(constref localAddress: SteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateListenSocketIP(@Self, @localAddress, nOptions, pOptions);
end;

function ISteamNetworkingSocketsHelper.ConnectByIPAddress(constref address: SteamNetworkingIPAddr; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ConnectByIPAddress(@Self, @address, nOptions, pOptions);
end;

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.CreateListenSocketP2P(nLocalVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2P(@Self, nLocalVirtualPort, nOptions, pOptions);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.ConnectP2P(constref identityRemote: SteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ConnectP2P(@Self, @identityRemote, nRemoteVirtualPort, nOptions, pOptions);
end;
{$ENDIF}

function ISteamNetworkingSocketsHelper.AcceptConnection(hConn: HSteamNetConnection): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_AcceptConnection(@Self, hConn);
end;

function ISteamNetworkingSocketsHelper.CloseConnection(hPeer: HSteamNetConnection; nReason: Integer; const pszDebug: PAnsiChar; bEnableLinger: Boolean): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CloseConnection(@Self, hPeer, nReason, pszDebug, bEnableLinger);
end;

function ISteamNetworkingSocketsHelper.CloseListenSocket(hSocket: HSteamListenSocket): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CloseListenSocket(@Self, hSocket);
end;

function ISteamNetworkingSocketsHelper.SetConnectionUserData(hPeer: HSteamNetConnection; nUserData: Int64): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_SetConnectionUserData(@Self, hPeer, nUserData);
end;

function ISteamNetworkingSocketsHelper.GetConnectionUserData(hPeer: HSteamNetConnection): Int64;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetConnectionUserData(@Self, hPeer);
end;

procedure ISteamNetworkingSocketsHelper.SetConnectionName(hPeer: HSteamNetConnection; const pszName: PAnsiChar);
begin
  SteamAPI_ISteamNetworkingSockets_SetConnectionName(@Self, hPeer, pszName);
end;

function ISteamNetworkingSocketsHelper.GetConnectionName(hPeer: HSteamNetConnection; pszName: PAnsiChar; nMaxLen: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetConnectionName(@Self, hPeer, pszName, nMaxLen);
end;

function ISteamNetworkingSocketsHelper.SendMessageToConnection(hConn: HSteamNetConnection; const pData: Pointer; cbData: UInt32; nSendFlags: Integer; pOutMessageNumber: PInt64): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_SendMessageToConnection(@Self, hConn, pData, cbData, nSendFlags, pOutMessageNumber);
end;

procedure ISteamNetworkingSocketsHelper.SendMessages(nMessages: Integer; pMessages: PPSteamNetworkingMessage_t; pOutMessageNumberOrResult: PInt64);
begin
  SteamAPI_ISteamNetworkingSockets_SendMessages(@Self, nMessages, pMessages, pOutMessageNumberOrResult);
end;

function ISteamNetworkingSocketsHelper.FlushMessagesOnConnection(hConn: HSteamNetConnection): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_FlushMessagesOnConnection(@Self, hConn);
end;

function ISteamNetworkingSocketsHelper.ReceiveMessagesOnConnection(hConn: HSteamNetConnection; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection(@Self, hConn, ppOutMessages, nMaxMessages);
end;

function ISteamNetworkingSocketsHelper.GetConnectionInfo(hConn: HSteamNetConnection; pInfo: PSteamNetConnectionInfo_t): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetConnectionInfo(@Self, hConn, pInfo);
end;

function ISteamNetworkingSocketsHelper.GetConnectionRealTimeStatus(hConn: HSteamNetConnection; pStatus: PSteamNetConnectionRealTimeStatus_t; nLanes: Integer; pLanes: PSteamNetConnectionRealTimeLaneStatus_t): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetConnectionRealTimeStatus(@Self, hConn, pStatus, nLanes, pLanes);
end;

function ISteamNetworkingSocketsHelper.GetDetailedConnectionStatus(hConn: HSteamNetConnection; pszBuf: PAnsiChar; cbBuf: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetDetailedConnectionStatus(@Self, hConn, pszBuf, cbBuf);
end;

function ISteamNetworkingSocketsHelper.GetListenSocketAddress(hSocket: HSteamListenSocket; address: PSteamNetworkingIPAddr): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetListenSocketAddress(@Self, hSocket, address);
end;

function ISteamNetworkingSocketsHelper.CreateSocketPair(pOutConnection1: PHSteamNetConnection; pOutConnection2: PHSteamNetConnection; bUseNetworkLoopback: Boolean; const pIdentity1: PSteamNetworkingIdentity; const pIdentity2: PSteamNetworkingIdentity): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateSocketPair(@Self, pOutConnection1, pOutConnection2, bUseNetworkLoopback, pIdentity1, pIdentity2);
end;

function ISteamNetworkingSocketsHelper.ConfigureConnectionLanes(hConn: HSteamNetConnection; nNumLanes: Integer; const pLanePriorities: PInteger; const pLaneWeights: PUInt16): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ConfigureConnectionLanes(@Self, hConn, nNumLanes, pLanePriorities, pLaneWeights);
end;

function ISteamNetworkingSocketsHelper.GetIdentity(pIdentity: PSteamNetworkingIdentity): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetIdentity(@Self, pIdentity);
end;

function ISteamNetworkingSocketsHelper.InitAuthentication(): ESteamNetworkingAvailability;
begin
  Result := SteamAPI_ISteamNetworkingSockets_InitAuthentication(@Self);
end;

function ISteamNetworkingSocketsHelper.GetAuthenticationStatus(pDetails: PSteamNetAuthenticationStatus_t): ESteamNetworkingAvailability;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetAuthenticationStatus(@Self, pDetails);
end;

function ISteamNetworkingSocketsHelper.CreatePollGroup(): HSteamNetPollGroup;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreatePollGroup(@Self);
end;

function ISteamNetworkingSocketsHelper.DestroyPollGroup(hPollGroup: HSteamNetPollGroup): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_DestroyPollGroup(@Self, hPollGroup);
end;

function ISteamNetworkingSocketsHelper.SetConnectionPollGroup(hConn: HSteamNetConnection; hPollGroup: HSteamNetPollGroup): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup(@Self, hConn, hPollGroup);
end;

function ISteamNetworkingSocketsHelper.ReceiveMessagesOnPollGroup(hPollGroup: HSteamNetPollGroup; ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup(@Self, hPollGroup, ppOutMessages, nMaxMessages);
end;

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.ReceivedRelayAuthTicket(const pvTicket: Pointer; cbTicket: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ReceivedRelayAuthTicket(@Self, pvTicket, cbTicket, pOutParsedTicket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.FindRelayAuthTicketForServer(constref identityGameServer: SteamNetworkingIdentity; nRemoteVirtualPort: Integer; pOutParsedTicket: PSteamDatagramRelayAuthTicket): Integer;
begin
  Result := SteamAPI_ISteamNetworkingSockets_FindRelayAuthTicketForServer(@Self, @identityGameServer, nRemoteVirtualPort, pOutParsedTicket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.ConnectToHostedDedicatedServer(constref identityTarget: SteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ConnectToHostedDedicatedServer(@Self, @identityTarget, nRemoteVirtualPort, nOptions, pOptions);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.GetHostedDedicatedServerPort(): UInt16;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPort(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.GetHostedDedicatedServerPOPID(): SteamNetworkingPOPID;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPOPID(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.GetHostedDedicatedServerAddress(pRouting: PSteamDatagramHostedAddress): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerAddress(@Self, pRouting);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.CreateHostedDedicatedServerListenSocket(nLocalVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateHostedDedicatedServerListenSocket(@Self, nLocalVirtualPort, nOptions, pOptions);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.GetGameCoordinatorServerLogin(pLoginInfo: PSteamDatagramGameCoordinatorServerLogin; pcbSignedBlob: PInteger; pBlob: Pointer): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetGameCoordinatorServerLogin(@Self, pLoginInfo, pcbSignedBlob, pBlob);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.ConnectP2PCustomSignaling(pSignaling: PISteamNetworkingConnectionSignaling; const pPeerIdentity: PSteamNetworkingIdentity; nRemoteVirtualPort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamNetConnection;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ConnectP2PCustomSignaling(@Self, pSignaling, pPeerIdentity, nRemoteVirtualPort, nOptions, pOptions);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.ReceivedP2PCustomSignal(const pMsg: Pointer; cbMsg: Integer; pContext: PISteamNetworkingSignalingRecvContext): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_ReceivedP2PCustomSignal(@Self, pMsg, cbMsg, pContext);
end;
{$ENDIF}

function ISteamNetworkingSocketsHelper.GetCertificateRequest(pcbBlob: PInteger; pBlob: Pointer; var errMsg: SteamNetworkingErrMsg): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetCertificateRequest(@Self, pcbBlob, pBlob, @errMsg);
end;

function ISteamNetworkingSocketsHelper.SetCertificate(const pCertificate: Pointer; cbCertificate: Integer; var errMsg: SteamNetworkingErrMsg): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_SetCertificate(@Self, pCertificate, cbCertificate, @errMsg);
end;

{$IFDEF STEAM}
procedure ISteamNetworkingSocketsHelper.ResetIdentity(const pIdentity: PSteamNetworkingIdentity);
begin
  SteamAPI_ISteamNetworkingSockets_ResetIdentity(@Self, pIdentity);
end;
{$ENDIF}

procedure ISteamNetworkingSocketsHelper.RunCallbacks();
begin
  SteamAPI_ISteamNetworkingSockets_RunCallbacks(@Self);
end;

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.BeginAsyncRequestFakeIP(nNumPorts: Integer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingSockets_BeginAsyncRequestFakeIP(@Self, nNumPorts);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamNetworkingSocketsHelper.GetFakeIP(idxFirstPort: Integer; pInfo: PSteamNetworkingFakeIPResult_t);
begin
  SteamAPI_ISteamNetworkingSockets_GetFakeIP(@Self, idxFirstPort, pInfo);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.CreateListenSocketP2PFakeIP(idxFakePort: Integer; nOptions: Integer; const pOptions: PSteamNetworkingConfigValue_t): HSteamListenSocket;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2PFakeIP(@Self, idxFakePort, nOptions, pOptions);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.GetRemoteFakeIPForConnection(hConn: HSteamNetConnection; pOutAddr: PSteamNetworkingIPAddr): EResult;
begin
  Result := SteamAPI_ISteamNetworkingSockets_GetRemoteFakeIPForConnection(@Self, hConn, pOutAddr);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingSocketsHelper.CreateFakeUDPPort(idxFakeServerPort: Integer): PISteamNetworkingFakeUDPPort;
begin
  Result := SteamAPI_ISteamNetworkingSockets_CreateFakeUDPPort(@Self, idxFakeServerPort);
end;
{$ENDIF}

function ISteamNetworkingUtilsHelper.AllocateMessage(cbAllocateBuffer: Integer): PSteamNetworkingMessage_t;
begin
  Result := SteamAPI_ISteamNetworkingUtils_AllocateMessage(@Self, cbAllocateBuffer);
end;

{$IFDEF STEAM}
procedure ISteamNetworkingUtilsHelper.InitRelayNetworkAccess();
begin
  SteamAPI_ISteamNetworkingUtils_InitRelayNetworkAccess(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetRelayNetworkStatus(pDetails: PSteamRelayNetworkStatus_t): ESteamNetworkingAvailability;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetRelayNetworkStatus(@Self, pDetails);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetLocalPingLocation(var aResult: SteamNetworkPingLocation_t): Single;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetLocalPingLocation(@Self, @aResult);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.EstimatePingTimeBetweenTwoLocations(constref location1: SteamNetworkPingLocation_t; constref location2: SteamNetworkPingLocation_t): Integer;
begin
  Result := SteamAPI_ISteamNetworkingUtils_EstimatePingTimeBetweenTwoLocations(@Self, @location1, @location2);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.EstimatePingTimeFromLocalHost(constref remoteLocation: SteamNetworkPingLocation_t): Integer;
begin
  Result := SteamAPI_ISteamNetworkingUtils_EstimatePingTimeFromLocalHost(@Self, @remoteLocation);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamNetworkingUtilsHelper.ConvertPingLocationToString(constref location: SteamNetworkPingLocation_t; pszBuf: PAnsiChar; cchBufSize: Integer);
begin
  SteamAPI_ISteamNetworkingUtils_ConvertPingLocationToString(@Self, @location, pszBuf, cchBufSize);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.ParsePingLocationString(const pszString: PAnsiChar; var aResult: SteamNetworkPingLocation_t): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_ParsePingLocationString(@Self, pszString, @aResult);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.CheckPingDataUpToDate(flMaxAgeSeconds: Single): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_CheckPingDataUpToDate(@Self, flMaxAgeSeconds);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetPingToDataCenter(popID: SteamNetworkingPOPID; pViaRelayPoP: PSteamNetworkingPOPID): Integer;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetPingToDataCenter(@Self, popID, pViaRelayPoP);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetDirectPingToPOP(popID: SteamNetworkingPOPID): Integer;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetDirectPingToPOP(@Self, popID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetPOPCount(): Integer;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetPOPCount(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetPOPList(list: PSteamNetworkingPOPID; nListSz: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetPOPList(@Self, list, nListSz);
end;
{$ENDIF}

function ISteamNetworkingUtilsHelper.GetLocalTimestamp(): SteamNetworkingMicroseconds;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetLocalTimestamp(@Self);
end;

procedure ISteamNetworkingUtilsHelper.SetDebugOutputFunction(eDetailLevel: ESteamNetworkingSocketsDebugOutputType; pfnFunc: FSteamNetworkingSocketsDebugOutput);
begin
  SteamAPI_ISteamNetworkingUtils_SetDebugOutputFunction(@Self, eDetailLevel, pfnFunc);
end;

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.IsFakeIPv4(nIPv4: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_IsFakeIPv4(@Self, nIPv4);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetIPv4FakeIPType(nIPv4: UInt32): ESteamNetworkingFakeIPType;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetIPv4FakeIPType(@Self, nIPv4);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.GetRealIdentityForFakeIP(constref fakeIP: SteamNetworkingIPAddr; pOutRealIdentity: PSteamNetworkingIdentity): EResult;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetRealIdentityForFakeIP(@Self, @fakeIP, pOutRealIdentity);
end;
{$ENDIF}

function ISteamNetworkingUtilsHelper.SetGlobalConfigValueInt32(eValue: ESteamNetworkingConfigValue; value: Int32): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueInt32(@Self, eValue, value);
end;

function ISteamNetworkingUtilsHelper.SetGlobalConfigValueFloat(eValue: ESteamNetworkingConfigValue; value: Single): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueFloat(@Self, eValue, value);
end;

function ISteamNetworkingUtilsHelper.SetGlobalConfigValueString(eValue: ESteamNetworkingConfigValue; const value: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueString(@Self, eValue, value);
end;

function ISteamNetworkingUtilsHelper.SetGlobalConfigValuePtr(eValue: ESteamNetworkingConfigValue; value: Pointer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValuePtr(@Self, eValue, value);
end;

function ISteamNetworkingUtilsHelper.SetConnectionConfigValueInt32(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; value: Int32): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueInt32(@Self, hConn, eValue, value);
end;

function ISteamNetworkingUtilsHelper.SetConnectionConfigValueFloat(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; value: Single): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueFloat(@Self, hConn, eValue, value);
end;

function ISteamNetworkingUtilsHelper.SetConnectionConfigValueString(hConn: HSteamNetConnection; eValue: ESteamNetworkingConfigValue; const value: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueString(@Self, hConn, eValue, value);
end;

function ISteamNetworkingUtilsHelper.SetGlobalCallback_SteamNetConnectionStatusChanged(fnCallback: FnSteamNetConnectionStatusChanged): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetConnectionStatusChanged(@Self, fnCallback);
end;

function ISteamNetworkingUtilsHelper.SetGlobalCallback_SteamNetAuthenticationStatusChanged(fnCallback: FnSteamNetAuthenticationStatusChanged): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetAuthenticationStatusChanged(@Self, fnCallback);
end;

function ISteamNetworkingUtilsHelper.SetGlobalCallback_SteamRelayNetworkStatusChanged(fnCallback: FnSteamRelayNetworkStatusChanged): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamRelayNetworkStatusChanged(@Self, fnCallback);
end;

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.SetGlobalCallback_FakeIPResult(fnCallback: FnSteamNetworkingFakeIPResult): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_FakeIPResult(@Self, fnCallback);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.SetGlobalCallback_MessagesSessionRequest(fnCallback: FnSteamNetworkingMessagesSessionRequest): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionRequest(@Self, fnCallback);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.SetGlobalCallback_MessagesSessionFailed(fnCallback: FnSteamNetworkingMessagesSessionFailed): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionFailed(@Self, fnCallback);
end;
{$ENDIF}

function ISteamNetworkingUtilsHelper.SetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr; eDataType: ESteamNetworkingConfigDataType; const pArg: Pointer): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConfigValue(@Self, eValue, eScopeType, scopeObj, eDataType, pArg);
end;

function ISteamNetworkingUtilsHelper.SetConfigValueStruct(constref opt: SteamNetworkingConfigValue_t; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SetConfigValueStruct(@Self, @opt, eScopeType, scopeObj);
end;

function ISteamNetworkingUtilsHelper.GetConfigValue(eValue: ESteamNetworkingConfigValue; eScopeType: ESteamNetworkingConfigScope; scopeObj: IntPtr; pOutDataType: PESteamNetworkingConfigDataType; pResult: Pointer; cbResult: Pcsize_t): ESteamNetworkingGetConfigValueResult;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetConfigValue(@Self, eValue, eScopeType, scopeObj, pOutDataType, pResult, cbResult);
end;

function ISteamNetworkingUtilsHelper.GetConfigValueInfo(eValue: ESteamNetworkingConfigValue; pOutDataType: PESteamNetworkingConfigDataType; pOutScope: PESteamNetworkingConfigScope): PAnsiChar;
begin
  Result := SteamAPI_ISteamNetworkingUtils_GetConfigValueInfo(@Self, eValue, pOutDataType, pOutScope);
end;

function ISteamNetworkingUtilsHelper.IterateGenericEditableConfigValues(eCurrent: ESteamNetworkingConfigValue; bEnumerateDevVars: Boolean): ESteamNetworkingConfigValue;
begin
  Result := SteamAPI_ISteamNetworkingUtils_IterateGenericEditableConfigValues(@Self, eCurrent, bEnumerateDevVars);
end;

{$IFDEF STEAM}
procedure ISteamNetworkingUtilsHelper.SteamNetworkingIPAddr_ToString(constref addr: SteamNetworkingIPAddr; buf: PAnsiChar; cbBuf: UInt32; bWithPort: Boolean);
begin
  SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ToString(@Self, @addr, buf, cbBuf, bWithPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.SteamNetworkingIPAddr_ParseString(pAddr: PSteamNetworkingIPAddr; const pszStr: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ParseString(@Self, pAddr, pszStr);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.SteamNetworkingIPAddr_GetFakeIPType(constref addr: SteamNetworkingIPAddr): ESteamNetworkingFakeIPType;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_GetFakeIPType(@Self, @addr);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamNetworkingUtilsHelper.SteamNetworkingIdentity_ToString(constref identity: SteamNetworkingIdentity; buf: PAnsiChar; cbBuf: UInt32);
begin
  SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ToString(@Self, @identity, buf, cbBuf);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingUtilsHelper.SteamNetworkingIdentity_ParseString(pIdentity: PSteamNetworkingIdentity; const pszStr: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ParseString(@Self, pIdentity, pszStr);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetProduct(const pszProduct: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetProduct(@Self, pszProduct);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetGameDescription(const pszGameDescription: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetGameDescription(@Self, pszGameDescription);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetModDir(const pszModDir: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetModDir(@Self, pszModDir);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetDedicatedServer(bDedicated: Boolean);
begin
  SteamAPI_ISteamGameServer_SetDedicatedServer(@Self, bDedicated);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.LogOn(const pszToken: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_LogOn(@Self, pszToken);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.LogOnAnonymous();
begin
  SteamAPI_ISteamGameServer_LogOnAnonymous(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.LogOff();
begin
  SteamAPI_ISteamGameServer_LogOff(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.BLoggedOn(): Boolean;
begin
  Result := SteamAPI_ISteamGameServer_BLoggedOn(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.BSecure(): Boolean;
begin
  Result := SteamAPI_ISteamGameServer_BSecure(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.GetSteamID(): CSteamID;
begin
  Result := SteamAPI_ISteamGameServer_GetSteamID(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.WasRestartRequested(): Boolean;
begin
  Result := SteamAPI_ISteamGameServer_WasRestartRequested(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetMaxPlayerCount(cPlayersMax: Integer);
begin
  SteamAPI_ISteamGameServer_SetMaxPlayerCount(@Self, cPlayersMax);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetBotPlayerCount(cBotplayers: Integer);
begin
  SteamAPI_ISteamGameServer_SetBotPlayerCount(@Self, cBotplayers);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetServerName(const pszServerName: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetServerName(@Self, pszServerName);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetMapName(const pszMapName: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetMapName(@Self, pszMapName);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetPasswordProtected(bPasswordProtected: Boolean);
begin
  SteamAPI_ISteamGameServer_SetPasswordProtected(@Self, bPasswordProtected);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetSpectatorPort(unSpectatorPort: UInt16);
begin
  SteamAPI_ISteamGameServer_SetSpectatorPort(@Self, unSpectatorPort);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetSpectatorServerName(const pszSpectatorServerName: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetSpectatorServerName(@Self, pszSpectatorServerName);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.ClearAllKeyValues();
begin
  SteamAPI_ISteamGameServer_ClearAllKeyValues(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetKeyValue(const pKey: PAnsiChar; const pValue: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetKeyValue(@Self, pKey, pValue);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetGameTags(const pchGameTags: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetGameTags(@Self, pchGameTags);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetGameData(const pchGameData: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetGameData(@Self, pchGameData);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetRegion(const pszRegion: PAnsiChar);
begin
  SteamAPI_ISteamGameServer_SetRegion(@Self, pszRegion);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SetAdvertiseServerActive(bActive: Boolean);
begin
  SteamAPI_ISteamGameServer_SetAdvertiseServerActive(@Self, bActive);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.GetAuthSessionTicket(pTicket: Pointer; cbMaxTicket: Integer; pcbTicket: PUInt32): HAuthTicket;
begin
  Result := SteamAPI_ISteamGameServer_GetAuthSessionTicket(@Self, pTicket, cbMaxTicket, pcbTicket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.BeginAuthSession(const pAuthTicket: Pointer; cbAuthTicket: Integer; steamID: CSteamID): EBeginAuthSessionResult;
begin
  Result := SteamAPI_ISteamGameServer_BeginAuthSession(@Self, pAuthTicket, cbAuthTicket, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.EndAuthSession(steamID: CSteamID);
begin
  SteamAPI_ISteamGameServer_EndAuthSession(@Self, steamID);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.CancelAuthTicket(hAuthTicket: HAuthTicket);
begin
  SteamAPI_ISteamGameServer_CancelAuthTicket(@Self, hAuthTicket);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.UserHasLicenseForApp(steamID: CSteamID; appID: AppId_t): EUserHasLicenseForAppResult;
begin
  Result := SteamAPI_ISteamGameServer_UserHasLicenseForApp(@Self, steamID, appID);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.RequestUserGroupStatus(steamIDUser: CSteamID; steamIDGroup: CSteamID): Boolean;
begin
  Result := SteamAPI_ISteamGameServer_RequestUserGroupStatus(@Self, steamIDUser, steamIDGroup);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.GetGameplayStats();
begin
  SteamAPI_ISteamGameServer_GetGameplayStats(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.GetServerReputation(): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamGameServer_GetServerReputation(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.GetPublicIP(): SteamIPAddress_t;
begin
  Result := SteamAPI_ISteamGameServer_GetPublicIP(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.HandleIncomingPacket(const pData: Pointer; cbData: Integer; srcIP: UInt32; srcPort: UInt16): Boolean;
begin
  Result := SteamAPI_ISteamGameServer_HandleIncomingPacket(@Self, pData, cbData, srcIP, srcPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.GetNextOutgoingPacket(pOut: Pointer; cbMaxOut: Integer; pNetAdr: PUInt32; pPort: PUInt16): Integer;
begin
  Result := SteamAPI_ISteamGameServer_GetNextOutgoingPacket(@Self, pOut, cbMaxOut, pNetAdr, pPort);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.AssociateWithClan(steamIDClan: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamGameServer_AssociateWithClan(@Self, steamIDClan);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.ComputeNewPlayerCompatibility(steamIDNewPlayer: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamGameServer_ComputeNewPlayerCompatibility(@Self, steamIDNewPlayer);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.SendUserConnectAndAuthenticate_DEPRECATED(unIPClient: UInt32; const pvAuthBlob: Pointer; cubAuthBlobSize: UInt32; pSteamIDUser: PCSteamID): Boolean;
begin
  Result := SteamAPI_ISteamGameServer_SendUserConnectAndAuthenticate_DEPRECATED(@Self, unIPClient, pvAuthBlob, cubAuthBlobSize, pSteamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.CreateUnauthenticatedUserConnection(): CSteamID;
begin
  Result := SteamAPI_ISteamGameServer_CreateUnauthenticatedUserConnection(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamGameServerHelper.SendUserDisconnect_DEPRECATED(steamIDUser: CSteamID);
begin
  SteamAPI_ISteamGameServer_SendUserDisconnect_DEPRECATED(@Self, steamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerHelper.BUpdateUserData(steamIDUser: CSteamID; const pchPlayerName: PAnsiChar; uScore: UInt32): Boolean;
begin
  Result := SteamAPI_ISteamGameServer_BUpdateUserData(@Self, steamIDUser, pchPlayerName, uScore);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.RequestUserStats(steamIDUser: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamGameServerStats_RequestUserStats(@Self, steamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PInt32): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_GetUserStatInt32(@Self, steamIDUser, pchName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.GetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; pData: PSingle): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_GetUserStatFloat(@Self, steamIDUser, pchName, pData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.GetUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar; pbAchieved: PBoolean): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_GetUserAchievement(@Self, steamIDUser, pchName, pbAchieved);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.SetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; nData: Int32): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_SetUserStatInt32(@Self, steamIDUser, pchName, nData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.SetUserStat(steamIDUser: CSteamID; const pchName: PAnsiChar; fData: Single): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_SetUserStatFloat(@Self, steamIDUser, pchName, fData);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.UpdateUserAvgRateStat(steamIDUser: CSteamID; const pchName: PAnsiChar; flCountThisSession: Single; dSessionLength: Double): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_UpdateUserAvgRateStat(@Self, steamIDUser, pchName, flCountThisSession, dSessionLength);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.SetUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_SetUserAchievement(@Self, steamIDUser, pchName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.ClearUserAchievement(steamIDUser: CSteamID; const pchName: PAnsiChar): Boolean;
begin
  Result := SteamAPI_ISteamGameServerStats_ClearUserAchievement(@Self, steamIDUser, pchName);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamGameServerStatsHelper.StoreUserStats(steamIDUser: CSteamID): SteamAPICall_t;
begin
  Result := SteamAPI_ISteamGameServerStats_StoreUserStats(@Self, steamIDUser);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamNetworkingFakeUDPPortHelper.DestroyFakeUDPPort();
begin
  SteamAPI_ISteamNetworkingFakeUDPPort_DestroyFakeUDPPort(@Self);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingFakeUDPPortHelper.SendMessageToFakeIP(constref remoteAddress: SteamNetworkingIPAddr; const pData: Pointer; cbData: UInt32; nSendFlags: Integer): EResult;
begin
  Result := SteamAPI_ISteamNetworkingFakeUDPPort_SendMessageToFakeIP(@Self, @remoteAddress, pData, cbData, nSendFlags);
end;
{$ENDIF}

{$IFDEF STEAM}
function ISteamNetworkingFakeUDPPortHelper.ReceiveMessages(ppOutMessages: PPSteamNetworkingMessage_t; nMaxMessages: Integer): Integer;
begin
  Result := SteamAPI_ISteamNetworkingFakeUDPPort_ReceiveMessages(@Self, ppOutMessages, nMaxMessages);
end;
{$ENDIF}

{$IFDEF STEAM}
procedure ISteamNetworkingFakeUDPPortHelper.ScheduleCleanup(constref remoteAddress: SteamNetworkingIPAddr);
begin
  SteamAPI_ISteamNetworkingFakeUDPPort_ScheduleCleanup(@Self, @remoteAddress);
end;
{$ENDIF}

end.

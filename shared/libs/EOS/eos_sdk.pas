{
 * The Platform Instance is used to gain access to all other Epic Online Service interfaces and to drive internal operations through the Tick.
 * All Platform Instance calls take a handle of type EOS_HPlatform as the first parameter.
 * Handle: EOS_HPlatforms are created by calling EOS_Platform_Create and subsequently released by calling EOS_Platform_Release.
 *
 * @see eos_init.h
 * @see EOS_Initialize
 * @see EOS_Platform_Create
 * @see EOS_Platform_Release
 * @see EOS_Shutdown
}

{
 * Notify the platform instance to do work. This function must be called frequently in order for the services provided by the SDK to properly
 * function. For tick-based applications, it is usually desireable to call this once per-tick.
}
procedure EOS_Platform_Tick(Handle: EOS_HPlatform); cdecl; external EOSLIB;

{
 * Get a handle to the Metrics Interface.
 * @return EOS_HMetrics handle
 *
 * @see eos_metrics.h
 * @see eos_metrics_types.h
}
function EOS_Platform_GetMetricsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Auth Interface.
 * @return EOS_HAuth handle
 *
 * @see eos_auth.h
 * @see eos_auth_types.h
}
function EOS_Platform_GetAuthInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Connect Interface.
 * @return EOS_HConnect handle
 *
 * @see eos_connect.h
 * @see eos_connect_types.h
}
function EOS_Platform_GetConnectInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Ecom Interface.
 * @return EOS_HEcom handle
 *
 * @see eos_ecom.h
 * @see eos_ecom_types.h
}
function EOS_Platform_GetEcomInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the UI Interface.
 * @return EOS_HUI handle
 *
 * @see eos_ui.h
 * @see eos_ui_types.h
}
function EOS_Platform_GetUIInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Friends Interface.
 * @return EOS_HFriends handle
 *
 * @see eos_friends.h
 * @see eos_friends_types.h
}
function EOS_Platform_GetFriendsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Presence Interface.
 * @return EOS_HPresence handle
 *
 * @see eos_presence.h
 * @see eos_presence_types.h
}
function EOS_Platform_GetPresenceInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Sessions Interface.
 * @return EOS_HSessions handle
 *
 * @see eos_sessions.h
 * @see eos_sessions_types.h
}
function EOS_Platform_GetSessionsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Lobby Interface.
 * @return EOS_HLobby handle
 *
 * @see eos_lobby.h
 * @see eos_lobby_types.h
}
function EOS_Platform_GetLobbyInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the UserInfo Interface.
 * @return EOS_HUserInfo handle
 *
 * @see eos_userinfo.h
 * @see eos_userinfo_types.h
}
function EOS_Platform_GetUserInfoInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Peer-to-Peer Networking Interface.
 * @return EOS_HP2P handle
 *
 * @see eos_p2p.h
 * @see eos_p2p_types.h
}
function EOS_Platform_GetP2PInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Real Time Communications Interface (RTC).
 * From the RTC interface you can retrieve the handle to the audio interface (RTCAudio), which is a component of RTC.
 * @return EOS_HRTC handle
 *
 * @see EOS_RTC_GetAudioInterface
 * @see eos_rtc.h
 * @see eos_rtc_types.h
}
function EOS_Platform_GetRTCInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the RTC Admin interface
 * @return EOS_HRTCAdmin handle
 *
 * @see eos_rtc_admin.h
 * @see eos_admin_types.h
}
function EOS_Platform_GetRTCAdminInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the PlayerDataStorage Interface.
 * @return EOS_HPlayerDataStorage handle
 *
 * @see eos_playerdatastorage.h
 * @see eos_playerdatastorage_types.h
}
function EOS_Platform_GetPlayerDataStorageInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the TitleStorage Interface.
 * @return EOS_HTitleStorage handle
 *
 * @see eos_titlestorage.h
 * @see eos_titlestorage_types.h
}
function EOS_Platform_GetTitleStorageInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Achievements Interface.
 * @return EOS_HAchievements handle
 *
 * @see eos_achievements.h
 * @see eos_achievements_types.h
}
function EOS_Platform_GetAchievementsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Stats Interface.
 * @return EOS_HStats handle
 *
 * @see eos_stats.h
 * @see eos_stats_types.h
}
function EOS_Platform_GetStatsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Leaderboards Interface.
 * @return EOS_HLeaderboards handle
 *
 * @see eos_leaderboards.h
 * @see eos_leaderboards_types.h
}
function EOS_Platform_GetLeaderboardsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Mods Interface.
 * @return EOS_HMods handle
 *
 * @see eos_mods.h
 * @see eos_mods_types.h
}
function EOS_Platform_GetModsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Anti-Cheat Client Interface.
 * @return EOS_HAntiCheatClient handle
 *
 * @see eos_anticheatclient.h
 * @see eos_anticheatclient_types.h
}
function EOS_Platform_GetAntiCheatClientInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Anti-Cheat Server Interface.
 * @return EOS_HAntiCheatServer handle
 *
 * @see eos_anticheatserver.h
 * @see eos_anticheatserver_types.h
}
function EOS_Platform_GetAntiCheatServerInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get the active country code that the SDK will send to services which require it.
 * This returns the override value otherwise it will use the country code of the given user.
 * This is currently used for determining pricing.
 * Get a handle to the ProgressionSnapshot Interface.
 * @return EOS_HProgressionSnapshot handle
 *
 * @see eos_progressionsnapshot.h
 * @see eos_progressionsnapshot_types.h
}
function EOS_Platform_GetProgressionSnapshotInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Reports Interface.
 * @return EOS_HReports handle
 *
 * @see eos_reports.h
 * @see eos_reports_types.h
}
function EOS_Platform_GetReportsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Sanctions Interface.
 * @return EOS_HSanctions handle
 *
 * @see eos_sanctions.h
 * @see eos_sanctions_types.h
}
function EOS_Platform_GetSanctionsInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * Get a handle to the Kids Web Service Interface.
 * @return EOS_HKWS handle
 *
 * @see eos_kws.h
 * @see eos_kws_types.h
}
function EOS_Platform_GetKWSInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;


{
 * Get a handle to the Custom Invites Interface.
 * @return EOS_HCustomInvites handle
 *
 * @see eos_custominvites.h
 * @see eos_custominvites_types.h
}
function EOS_Platform_GetCustomInvitesInterface(Handle: EOS_HPlatform): Pointer; cdecl; external EOSLIB;

{
 * This only will return the value set as the override otherwise EOS_NotFound is returned.
 * This is not currently used for anything internally.
 *
 * @param LocalUserId The account to use for lookup if no override exists.
 * @param OutBuffer The buffer into which the character data should be written.  The buffer must be long enough to hold a string of EOS_COUNTRYCODE_MAX_LENGTH.
 * @param InOutBufferLength The size of the OutBuffer in characters.
 *                          The input buffer should include enough space to be null-terminated.
 *                          When the function returns, this parameter will be filled with the length of the string copied into OutBuffer.
 *
 * @return An EOS_EResult that indicates whether the active country code string was copied into the OutBuffer.
 *         EOS_Success if the information is available and passed out in OutBuffer
 *         EOS_InvalidParameters if you pass a null pointer for the out parameter
 *         EOS_NotFound if there is not an override country code for the user.
 *         EOS_LimitExceeded - The OutBuffer is not large enough to receive the country code string. InOutBufferLength contains the required minimum length to perform the operation successfully.
 *
 * @see eos_ecom.h
 * @see EOS_COUNTRYCODE_MAX_LENGTH
}
function EOS_Platform_GetActiveCountryCode(Handle: EOS_HPlatform; LocalUserId: EOS_EpicAccountId; OutBuffer: Pointer; InOutBufferLength: pcint32): EOS_EResult; cdecl; external EOSLIB;

{
 * Get the active locale code that the SDK will send to services which require it.
 * This returns the override value otherwise it will use the locale code of the given user.
 * This is used for localization. This follows ISO 639.
 *
 * @param LocalUserId The account to use for lookup if no override exists.
 * @param OutBuffer The buffer into which the character data should be written.  The buffer must be long enough to hold a string of EOS_LOCALECODE_MAX_LENGTH.
 * @param InOutBufferLength The size of the OutBuffer in characters.
 *                          The input buffer should include enough space to be null-terminated.
 *                          When the function returns, this parameter will be filled with the length of the string copied into OutBuffer.
 *
 * @return An EOS_EResult that indicates whether the active locale code string was copied into the OutBuffer.
 *         EOS_Success if the information is available and passed out in OutBuffer
 *         EOS_InvalidParameters if you pass a null pointer for the out parameter
 *         EOS_NotFound if there is neither an override nor an available locale code for the user.
 *         EOS_LimitExceeded - The OutBuffer is not large enough to receive the locale code string. InOutBufferLength contains the required minimum length to perform the operation successfully.
 *
 * @see eos_ecom.h
 * @see EOS_LOCALECODE_MAX_LENGTH
}
function EOS_Platform_GetActiveLocaleCode(Handle: EOS_HPlatform; LocalUserId: EOS_EpicAccountId; OutBuffer: Pointer; InOutBufferLength: pcint32): EOS_EResult; cdecl; external EOSLIB;

{
 * Get the override country code that the SDK will send to services which require it.
 * This is not currently used for anything internally.
 *
 * @param OutBuffer The buffer into which the character data should be written.  The buffer must be long enough to hold a string of EOS_COUNTRYCODE_MAX_LENGTH.
 * @param InOutBufferLength The size of the OutBuffer in characters.
 *                          The input buffer should include enough space to be null-terminated.
 *                          When the function returns, this parameter will be filled with the length of the string copied into OutBuffer.
 *
 * @return An EOS_EResult that indicates whether the override country code string was copied into the OutBuffer.
 *         EOS_Success if the information is available and passed out in OutBuffer
 *         EOS_InvalidParameters if you pass a null pointer for the out parameter
 *         EOS_LimitExceeded - The OutBuffer is not large enough to receive the country code string. InOutBufferLength contains the required minimum length to perform the operation successfully.
 *
 * @see eos_ecom.h
 * @see EOS_COUNTRYCODE_MAX_LENGTH
}
function EOS_Platform_GetOverrideCountryCode(Handle: EOS_HPlatform; OutBuffer: Pointer; InOutBufferLength: pcint32): EOS_EResult; cdecl; external EOSLIB;

{
 * Get the override locale code that the SDK will send to services which require it.
 * This is used for localization. This follows ISO 639.
 *
 * @param OutBuffer The buffer into which the character data should be written.  The buffer must be long enough to hold a string of EOS_LOCALECODE_MAX_LENGTH.
 * @param InOutBufferLength The size of the OutBuffer in characters.
 *                          The input buffer should include enough space to be null-terminated.
 *                          When the function returns, this parameter will be filled with the length of the string copied into OutBuffer.
 *
 * @return An EOS_EResult that indicates whether the override locale code string was copied into the OutBuffer.
 *         EOS_Success if the information is available and passed out in OutBuffer
 *         EOS_InvalidParameters if you pass a null pointer for the out parameter
 *         EOS_LimitExceeded - The OutBuffer is not large enough to receive the locale code string. InOutBufferLength contains the required minimum length to perform the operation successfully.
 *
 * @see eos_ecom.h
 * @see EOS_LOCALECODE_MAX_LENGTH
}
function EOS_Platform_GetOverrideLocaleCode(Handle: EOS_HPlatform; OutBuffer: Pointer; InOutBufferLength: pcint32): EOS_EResult; cdecl; external EOSLIB;

{
 * Set the override country code that the SDK will send to services which require it.
 * This is not currently used for anything internally.
 *
 * @return An EOS_EResult that indicates whether the override country code string was saved.
 *         EOS_Success if the country code was overridden
 *         EOS_InvalidParameters if you pass an invalid country code
 *
 * @see eos_ecom.h
 * @see EOS_COUNTRYCODE_MAX_LENGTH
}
function EOS_Platform_SetOverrideCountryCode(Handle: EOS_HPlatform; NewCountryCode: PChar): EOS_EResult; cdecl; external EOSLIB;

{
 * Set the override locale code that the SDK will send to services which require it.
 * This is used for localization. This follows ISO 639.
 *
 * @return An EOS_EResult that indicates whether the override locale code string was saved.
 *         EOS_Success if the locale code was overridden
 *         EOS_InvalidParameters if you pass an invalid locale code
 *
 * @see eos_ecom.h
 * @see EOS_LOCALECODE_MAX_LENGTH
}
function EOS_Platform_SetOverrideLocaleCode(Handle: EOS_HPlatform; NewLocaleCode: PChar): EOS_EResult; cdecl; external EOSLIB;

{
 * Checks if the app was launched through the Epic Launcher, and relaunches it through the Epic Launcher if it wasn't.
 *
 * @return An EOS_EResult is returned to indicate success or an error.
 *
 * EOS_Success is returned if the app is being restarted. You should quit your process as soon as possible.
 * EOS_NoChange is returned if the app was already launched through the Epic Launcher, and no action needs to be taken.
 * EOS_UnexpectedError is returned if the LauncherCheck module failed to initialize, or the module tried and failed to restart the app.
}
function EOS_Platform_CheckForLauncherAndRestart(Handle: EOS_HPlatform): EOS_EResult; cdecl; external EOSLIB;

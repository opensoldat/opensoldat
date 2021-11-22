{
 * Logging levels. When a log message is output, it has an associated log level.
 * Messages will only be sent to the callback function if the message's associated log level is less than or equal to the configured log level for that category.
 *
 * @see EOS_Logging_SetCallback
 * @see EOS_Logging_SetLogLevel
 }
type
EOS_ELogLevel = (
    EOS_LOG_Off = 0,
    EOS_LOG_Fatal = 100,
    EOS_LOG_Error = 200,
    EOS_LOG_Warning = 300,
    EOS_LOG_Info = 400,
    EOS_LOG_Verbose = 500,
    EOS_LOG_VeryVerbose = 600
);

{
 * Logging Categories
 }
type
EOS_ELogCategory = (
    { Low level logs unrelated to specific services }
    EOS_LC_Core = 0,
    { Logs related to the Auth service }
    EOS_LC_Auth = 1,
    { Logs related to the Friends service }
    EOS_LC_Friends = 2,
    { Logs related to the Presence service }
    EOS_LC_Presence = 3,
    { Logs related to the UserInfo service }
    EOS_LC_UserInfo = 4,
    { Logs related to HTTP serialization }
    EOS_LC_HttpSerialization = 5,
    { Logs related to the Ecommerce service }
    EOS_LC_Ecom = 6,
    { Logs related to the P2P service }
    EOS_LC_P2P = 7,
    { Logs related to the Sessions service }
    EOS_LC_Sessions = 8,
    { Logs related to rate limiting }
    EOS_LC_RateLimiter = 9,
    { Logs related to the PlayerDataStorage service }
    EOS_LC_PlayerDataStorage = 10,
    { Logs related to sdk analytics }
    EOS_LC_Analytics = 11,
    { Logs related to the messaging service }
    EOS_LC_Messaging = 12,
    { Logs related to the Connect service }
    EOS_LC_Connect = 13,
    { Logs related to the Overlay }
    EOS_LC_Overlay = 14,
    { Logs related to the Achievements service }
    EOS_LC_Achievements = 15,
    { Logs related to the Stats service }
    EOS_LC_Stats = 16,
    { Logs related to the UI service }
    EOS_LC_UI = 17,
    { Logs related to the lobby service }
    EOS_LC_Lobby = 18,
    { Logs related to the Leaderboards service }
    EOS_LC_Leaderboards = 19,
    { Logs related to an internal Keychain feature that the authentication interfaces use }
    EOS_LC_Keychain = 20,
    { Logs related to external identity providers }
    EOS_LC_IdentityProvider = 21,
    { Logs related to Title Storage }
    EOS_LC_TitleStorage = 22,
    { Logs related to the Mods service }
    EOS_LC_Mods = 23,
    { Logs related to the Anti-Cheat service }
    EOS_LC_AntiCheat = 24,
    { Logs related to reports client. }
    EOS_LC_Reports = 25,
    { Logs related to the Sanctions service }
    EOS_LC_Sanctions = 26,
    { Logs related to the Progression Snapshot service }
    EOS_LC_ProgressionSnapshots = 27,
    { Logs related to the Kids Web Services integration }
    EOS_LC_KWS = 28,
    { Logs related to the RTC API }
    EOS_LC_RTC = 29,
    { Logs related to the RTC Admin API }
    EOS_LC_RTCAdmin = 30,
    { Logs related to the Inventory service *}
    EOS_LC_Inventory = 31,
    { Logs related to the Receipt Validator API *}
    EOS_LC_ReceiptValidator = 32,
    { Logs related to the Custom Invites API *}
    EOS_LC_CustomInvites = 33,

    { Not a real log category. Used by EOS_Logging_SetLogLevel to set the log level for all categories at the same time }
    EOS_LC_ALL_CATEGORIES = $7fffffff
);

{ A structure representing a log message }
EOS_LogMessage = record
    { A string representation of the log message category, encoded in UTF-8. Only valid during the life of the callback, so copy the string if you need it later. }
    Category: PChar;
    { The log message, encoded in UTF-8. Only valid during the life of the callback, so copy the string if you need it later. }
    Message: PChar;
    { The log level associated with the message }
    Level: EOS_ELogLevel;
end;

{
 * Function prototype definition for functions that receive log messages.
 *
 * @param Message A EOS_LogMessage containing the log category, log level, and message.
 * @see EOS_LogMessage
 }
type
  EOS_LogMessageFunc = procedure(var Message: EOS_LogMessage);

{
 * Set the callback function to use for SDK log messages. Any previously set callback will no longer be called.
 *
 * @param Callback the function to call when the SDK logs messages
 * @return EOS_Success is returned if the callback will be used for future log messages.
 *         EOS_NotConfigured is returned if the SDK has not yet been initialized, or if it has been shut down
 *
 * @see EOS_Initialize
 }
function EOS_Logging_SetCallback(Callback: EOS_LogMessageFunc): EOS_EResult; cdecl; external EOSLIB;

{
 * Set the logging level for the specified logging category. By default all log categories will callback for Warnings, Errors, and Fatals.
 *
 * @param LogCategory the specific log category to configure. Use EOS_LC_ALL_CATEGORIES to configure all categories simultaneously to the same log level.
 * @param LogLevel the log level to use for the log category
 *
 * @return EOS_Success is returned if the log levels are now in use.
 *         EOS_NotConfigured is returned if the SDK has not yet been initialized, or if it has been shut down.
 }
function EOS_Logging_SetLogLevel(LogCategory: EOS_ELogCategory; LogLevel: EOS_ELogLevel): EOS_EResult; cdecl; external EOSLIB;
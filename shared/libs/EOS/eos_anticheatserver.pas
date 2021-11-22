{
 * Add a callback issued when a new message must be dispatched to a connected client. The bound function
 * will only be called between a successful call to EOS_AntiCheatServer_BeginSession and the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data
 * @param ClientData This value is returned to the caller when NotificationFn is invoked
 * @param NotificationFn The callback to be fired
 * @return A valid notification ID if successfully bound, or EOS_INVALID_NOTIFICATIONID otherwise
 }
function EOS_AntiCheatServer_AddNotifyMessageToClient(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_AddNotifyMessageToClientOptions; ClientData: Pointer; NotificationFn: EOS_AntiCheatServer_OnMessageToClientCallback): EOS_NotificationId; cdecl; external EOSLIB;

{
 * Remove a previously bound EOS_AntiCheatServer_AddNotifyMessageToClient handler.
 *
 * @param NotificationId The previously bound notification ID
 }
procedure EOS_AntiCheatServer_RemoveNotifyMessageToClient(Handle: EOS_HAntiCheatServer; NotificationId: EOS_NotificationId); cdecl; external EOSLIB;

{
 * Add a callback issued when an action must be applied to a connected client. The bound function
 * will only be called between a successful call to EOS_AntiCheatServer_BeginSession and the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data
 * @param ClientData This value is returned to the caller when NotificationFn is invoked
 * @param NotificationFn The callback to be fired
 * @return A valid notification ID if successfully bound, or EOS_INVALID_NOTIFICATIONID otherwise
 }
function EOS_AntiCheatServer_AddNotifyClientActionRequired(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_AddNotifyClientActionRequiredOptions; ClientData: Pointer; NotificationFn: EOS_AntiCheatServer_OnClientActionRequiredCallback): EOS_NotificationId; cdecl; external EOSLIB;

{
 * Remove a previously bound EOS_AntiCheatServer_AddNotifyClientActionRequired handler.
 *
 * @param NotificationId The previously bound notification ID
 }
procedure EOS_AntiCheatServer_RemoveNotifyClientActionRequired(Handle: EOS_HAntiCheatServer; NotificationId: EOS_NotificationId); cdecl; external EOSLIB;

{
 * Add an optional callback issued when a connected client's authentication status changes. The bound function
 * will only be called between a successful call to EOS_AntiCheatServer_BeginSession and the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data
 * @param ClientData This value is returned to the caller when NotificationFn is invoked
 * @param NotificationFn The callback to be fired
 * @return A valid notification ID if successfully bound, or EOS_INVALID_NOTIFICATIONID otherwise
 }
function EOS_AntiCheatServer_AddNotifyClientAuthStatusChanged(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_AddNotifyClientAuthStatusChangedOptions; ClientData: Pointer; NotificationFn: EOS_AntiCheatServer_OnClientAuthStatusChangedCallback): EOS_NotificationId; cdecl; external EOSLIB;

{
 * Remove a previously bound EOS_AntiCheatServer_AddNotifyClientAuthStatusChanged handler.
 *
 * @param NotificationId The previously bound notification ID
 }
procedure EOS_AntiCheatServer_RemoveNotifyClientAuthStatusChanged(Handle: EOS_HAntiCheatServer; NotificationId: EOS_NotificationId); cdecl; external EOSLIB;

{
 * Begin the gameplay session. Event callbacks must be configured with EOS_AntiCheatServer_AddNotifyMessageToClient
 * and EOS_AntiCheatServer_AddNotifyClientActionRequired before calling this function.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the initialization succeeded
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_BeginSession(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_BeginSessionOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * End the gameplay session. Should be called when the server is shutting down or entering an idle state.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the initialization succeeded
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_EndSession(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_EndSessionOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Register a connected client. Must be paired with a call to UnregisterClient.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the player was registered successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_RegisterClient(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_RegisterClientOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Unregister a disconnected client.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the player was unregistered successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_UnregisterClient(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_UnregisterClientOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Call when an anti-cheat message is received from a client.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the message was processed successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_ReceiveMessageFromClient(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_ReceiveMessageFromClientOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional. Sets or updates client details including input device and admin status.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the flags were updated successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_SetClientDetails(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_SetClientDetailsOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional. Sets or updates a game session identifier which can be attached to other data for reference.
 * The identifier can be updated at any time for currently and subsequently registered clients.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the game session identifier was set successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_SetGameSessionId(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_SetGameSessionIdOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional. Can be used to indicate that a client is legitimately known to be
 * temporarily unable to communicate, for example as a result of loading a new level.
 *
 * The bIsNetworkActive flag must be set back to true when users enter normal
 * gameplay, otherwise anti-cheat enforcement will not work correctly.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the network state was updated successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_SetClientNetworkState(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_SetClientNetworkStateOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional NetProtect feature for game message encryption.
 * Calculates the required decrypted buffer size for a given input data length.
 * This will not change for a given SDK version, and allows one time allocation of reusable buffers.
 *
 * @param Options Structure containing input data.
 * @param OutBufferLengthBytes On success, the OutBuffer length in bytes that is required to call ProtectMessage on the given input size.
 *
 * @return EOS_Success - If the output length was calculated successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_GetProtectMessageOutputLength(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_GetProtectMessageOutputLengthOptions; OutBufferSizeBytes: pcuint32): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional NetProtect feature for game message encryption.
 * Encrypts an arbitrary message that will be sent to a game client and decrypted on the other side.
 *
 * Options.Data and OutBuffer may refer to the same buffer to encrypt in place.
 *
 * @param Options Structure containing input data.
 * @param OutBuffer On success, buffer where encrypted message data will be written.
 * @param OutBytesWritten On success, the number of bytes that were written to OutBuffer.
 *
 * @return EOS_Success - If the message was protected successfully
 *         EOS_InvalidParameters - If input data was invalid
 *         EOS_InvalidUser - If the specified ClientHandle was invalid or not currently registered. See RegisterClient.
 }
function EOS_AntiCheatServer_ProtectMessage(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_ProtectMessageOptions; OutBuffer: Pointer; OutBytesWritten: pcuint32): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional NetProtect feature for game message encryption.
 * Decrypts an encrypted message received from a game client.
 *
 * Options.Data and OutBuffer may refer to the same buffer to decrypt in place.
 *
 * @param Options Structure containing input data.
 * @param OutBuffer On success, buffer where encrypted message data will be written.
 * @param OutBytesWritten On success, the number of bytes that were written to OutBuffer.
 *
 * @return EOS_Success - If the message was unprotected successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_UnprotectMessage(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatServer_UnprotectMessageOptions; OutBuffer: Pointer; OutBytesWritten: pcuint32): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Registers a custom gameplay event.
 *
 * All custom game events must be registered before EOS_AntiCheatServer_BeginSession is called for the first time.
 * After the first call to EOS_AntiCheatServer_BeginSession, this function cannot be called any longer.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was registered successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_RegisterEvent(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_RegisterEventOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs a custom gameplay event.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogEvent(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogEventOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs a new game round start.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogGameRoundStart(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogGameRoundStartOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs a game round's end and outcome.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogGameRoundEnd(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogGameRoundEndOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs a player spawning into the game.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogPlayerSpawn(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogPlayerSpawnOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs a player despawning in the game, for example as a result of the character's death,
 * switching to spectator mode, etc.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogPlayerDespawn(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogPlayerDespawnOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs a player being revived after being downed (see EOS_AntiCheatServer_LogPlayerTakeDamage options).
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogPlayerRevive(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogPlayerReviveOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs a player's general state including position and view direction.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogPlayerTick(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogPlayerTickOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs that a player has used a weapon, for example firing one bullet or making one melee attack.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogPlayerUseWeapon(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogPlayerUseWeaponOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs that a player has used a special ability or item which affects their character's capabilities,
 * for example temporarily increasing their speed or allowing them to see nearby players behind walls.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogPlayerUseAbility(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogPlayerUseAbilityOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Optional Cerberus feature for gameplay data collection.
 * Logs that a player has taken damage.
 *
 * This function may only be called between a successful call to EOS_AntiCheatServer_BeginSession and
 * the matching EOS_AntiCheatServer_EndSession call.
 *
 * @param Options Structure containing input data.
 *
 * @return EOS_Success - If the event was logged successfully
 *         EOS_InvalidParameters - If input data was invalid
 }
function EOS_AntiCheatServer_LogPlayerTakeDamage(Handle: EOS_HAntiCheatServer; Options: PEOS_AntiCheatCommon_LogPlayerTakeDamageOptions): EOS_EResult; cdecl; external EOSLIB;

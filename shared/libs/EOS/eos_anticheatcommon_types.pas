{ 
  * Arbitrary data that is a unique local identifier for
  * a single remote client or peer.
  *
  * Typically this is a pointer to an object describing the
  * player, but it can be anything that is locally unique.
  }
type EOS_AntiCheatCommon_ClientHandle = Pointer;

{ Flags describing the type of a remote client }
EOS_EAntiCheatCommonClientType = (
    { An ordinary player that requires anti-cheat client protection to play }
    EOS_ACCCT_ProtectedClient = 0,
    { The player does not need the anti-cheat client to play because of their platform or other factors }
    EOS_ACCCT_UnprotectedClient = 1,
    { The client is an AI bot, not an actual human }
    EOS_ACCCT_AIBot = 2
);

{ Flags describing the platform of a remote client, if known }
EOS_EAntiCheatCommonClientPlatform = (
    { Unknown platform }
    EOS_ACCCP_Unknown = 0,
    { The client is playing on Windows }
    EOS_ACCCP_Windows = 1,
    { The client is playing on Mac }
    EOS_ACCCP_Mac = 2,
    { The client is playing on Linux }
    EOS_ACCCP_Linux = 3,
    { The client is playing on an Xbox device }
    EOS_ACCCP_Xbox = 4,
    { The client is playing on a PlayStation device }
    EOS_ACCCP_PlayStation = 5,
    { The client is playing on a Nintendo device }
    EOS_ACCCP_Nintendo = 6,
    { The client is playing on iOS }
    EOS_ACCCP_iOS = 7,
    { The client is playing on Android }
    EOS_ACCCP_Android = 8
);

{ Anti-cheat action values. Applicable to both clients and remote peers. }
EOS_EAntiCheatCommonClientAction = (
    { Not used }
    EOS_ACCCA_Invalid = 0,
    { The client/peer must be removed from the current game session }
    EOS_ACCCA_RemovePlayer = 1
);

{ Anti-cheat action reasons. Applicable to both clients and remote peers. }
EOS_EAntiCheatCommonClientActionReason = (
    { Not used }
    EOS_ACCCAR_Invalid = 0,
    { An internal error occurred }
    EOS_ACCCAR_InternalError = 1,
    { An anti-cheat message received from the client/peer was corrupt or invalid }
    EOS_ACCCAR_InvalidMessage = 2,
    { The client/peer's anti-cheat authentication failed }
    EOS_ACCCAR_AuthenticationFailed = 3,
    { The client/peer failed to load the anti-cheat module at startup }
    EOS_ACCCAR_NullClient = 4,
    { The client/peer's anti-cheat heartbeat was not received }
    EOS_ACCCAR_HeartbeatTimeout = 5,
    { The client/peer failed an anti-cheat client runtime check }
    EOS_ACCCAR_ClientViolation = 6,
    { The client/peer failed an anti-cheat backend runtime check }
    EOS_ACCCAR_BackendViolation = 7,
    { The client/peer is temporarily blocked from playing on this game server }
    EOS_ACCCAR_TemporaryCooldown = 8,
    { The client/peer is temporarily banned }
    EOS_ACCCAR_TemporaryBanned = 9,
    { The client/peer is permanently banned }
    EOS_ACCCAR_PermanentBanned = 10
);

{ The client/peer's anti-cheat authentication status }
EOS_EAntiCheatCommonClientAuthStatus = (
    { Not used }
    EOS_ACCCAS_Invalid = 0,
    { The client/peer's anti-cheat functionality has been validated by this game server }
    EOS_ACCCAS_LocalAuthComplete = 1,
    { The client/peer's anti-cheat functionality has been validated by the anti-cheat backend service }
    EOS_ACCCAS_RemoteAuthComplete = 2
);

{ Flags describing a remote client. These can be updated during a play session }
EOS_EAntiCheatCommonClientFlags = (
    { No particular flags relevant for this client }
    EOS_ACCCF_None = 0,
    { The client has admin privileges on the game server }
    EOS_ACCCF_Admin = (1 shl 0)
);
//EOS_ENUM_BOOLEAN_OPERATORS(EOS_EAntiCheatCommonClientFlags);

{ Flags describing the input device used by a remote client, if known. These can be updated during a play session. }
EOS_EAntiCheatCommonClientInput = (
    { Unknown input device }
    EOS_ACCCI_Unknown = 0,
    { The client is using mouse and keyboard }
    EOS_ACCCI_MouseKeyboard = 1,
    { The client is using a gamepad or game controller }
    EOS_ACCCI_Gamepad = 2,
    { The client is using a touch input device (e.g. phone/tablet screen) }
    EOS_ACCCI_TouchInput = 3
);

{
 * Types supported for custom gameplay behavior events.
 * These have a considerable impact on performance
 }
EOS_EAntiCheatCommonEventType = (
    { Not used }
    EOS_ACCET_Invalid = 0,
    {
     * A general game event that is not specific to any individual player.
     * Low memory use which is constant with respect to the number of players.
     }
    EOS_ACCET_GameEvent = 1,
    {
     * An event that is logically associated with a specific player. Events logged in
     * this category require a specific ClientHandle to which they will be attached.
     * Higher memory use which scales according to the number of players.
     }
    EOS_ACCET_PlayerEvent = 2
);

{ Types supported for custom gameplay behavior event parameters }
EOS_EAntiCheatCommonEventParamType = (
    { Not used }
    EOS_ACCEPT_Invalid = 0,
    { EOS_AntiCheatCommon_ClientHandle }
    EOS_ACCEPT_ClientHandle = 1,
    { const char* }
    EOS_ACCEPT_String = 2,
    { uint32_t }
    EOS_ACCEPT_UInt32 = 3,
    { int32_t }
    EOS_ACCEPT_Int32 = 4,
    { uint64_t }
    EOS_ACCEPT_UInt64 = 5,
    { int64_t }
    EOS_ACCEPT_Int64 = 6,
    { EOS_AntiCheatCommon_Vec3f }
    EOS_ACCEPT_Vector3f = 7,
    { EOS_AntiCheatCommon_Quat }
    EOS_ACCEPT_Quat = 8
);

{ Details of a player's movement state }
EOS_EAntiCheatCommonPlayerMovementState = (
    { No particular state applies }
    EOS_ACCPMS_None = 0,
    { Player is crouching }
    EOS_ACCPMS_Crouching = 1,
    { Player is prone }
    EOS_ACCPMS_Prone = 2,
    { Player is mounted in a vehicle or similar }
    EOS_ACCPMS_Mounted = 3,
    { Player is swimming in a fluid volume }
    EOS_ACCPMS_Swimming = 4,
    { Player is falling under the effects of gravity, such as when jumping or walking off the edge of a surface }
    EOS_ACCPMS_Falling = 5,
    { Player is flying, ignoring the effects of gravity }
    EOS_ACCPMS_Flying = 6,
    { Player is on a ladder }
    EOS_ACCPMS_OnLadder = 7
);

{ The source of a damage event }
EOS_EAntiCheatCommonPlayerTakeDamageSource = (
    { No particular source relevant }
    EOS_ACCPTDS_None = 0,
    { Damage caused by a player controlled character }
    EOS_ACCPTDS_Player = 1,
    { Damage caused by a non-player character such as an AI enemy }
    EOS_ACCPTDS_NonPlayerCharacter = 2,
    { Damage caused by the world (falling off level, into lava, etc) }
    EOS_ACCPTDS_World = 3
);

{ Type of damage applied in a damage event }
EOS_EAntiCheatCommonPlayerTakeDamageType = (
    { No particular type relevant }
    EOS_ACCPTDT_None = 0,
    { Damage caused by a point source such as a bullet or melee attack }
    EOS_ACCPTDT_PointDamage = 1,
    { Damage caused by a radial source such as an explosion }
    EOS_ACCPTDT_RadialDamage = 2,
    { Damage over time such as bleeding, poison, etc }
    EOS_ACCPTDT_DamageOverTime = 3
);

{ The result of a damage event, if any }
EOS_EAntiCheatCommonPlayerTakeDamageResult = (
    { No direct state change consequence for the victim }
    EOS_ACCPTDR_None = 0,
    { Player character is temporarily incapacitated and requires assistance to recover }
    EOS_ACCPTDR_Downed = 1,
    { Player character is permanently incapacitated and cannot recover (e.g. dead) }
    EOS_ACCPTDR_Eliminated = 2
);

{ Vector using left-handed coordinate system (as in Unreal Engine) }
type EOS_AntiCheatCommon_Vec3f = record
    { X axis coordinate - forward direction }
    x: Single;
    { Y axis coordinate - right direction }
    y: Single;
    { Z axis coordinate - up direction }
    z: Single;
end;

{ Quaternion using left-handed coordinate system (as in Unreal Engine) }
type EOS_AntiCheatCommon_Quat = record
    { W component - scalar part }
    w: Single;
    { X component - forward direction }
    x: Single;
    { Y component - right direction }
    y: Single;
    { Z component - up direction }
    z: Single;
end;

{
 * Structure containing details about a new message that must be dispatched to a connected client/peer.
 }
type EOS_AntiCheatCommon_OnMessageToClientCallbackInfo = record
    { Caller-specified context data }
    ClientData: Pointer;
    { The identifier of the client/peer that this message must be delivered to. See the RegisterClient and RegisterPeer functions. }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { The message data that must be sent to the client }
    MessageData: Pointer;
    { The size in bytes of MessageData }
    MessageDataSizeBytes: cuint32;
end;

type PEOS_AntiCheatCommon_OnMessageToClientCallbackInfo = ^EOS_AntiCheatCommon_OnMessageToClientCallbackInfo;

{ Structure containing details about a required client/peer action }
type EOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo = record
    { Caller-specified context data }
    ClientData: Pointer;
    { The identifier of the client/peer that this action applies to. See the RegisterClient and RegisterPeer functions. }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { The action that must be applied to the specified client/peer }
    ClientAction: EOS_EAntiCheatCommonClientAction;
    { Code indicating the reason for the action. This can be displayed to the affected player. }
    ActionReasonCode: EOS_EAntiCheatCommonClientActionReason;
    { String containing details about the action reason. This can be displayed to the affected player. }
    ActionReasonDetailsString: PChar;
end;

type PEOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo = ^EOS_AntiCheatCommon_OnClientActionRequiredCallbackInfo;

{ Structure containing details about a client/peer authentication status change }
type EOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo = record
    { Caller-specified context data }
    ClientData: Pointer;
    { The identifier of the client/peer that this status change applies to. See the RegisterClient and RegisterPeer functions. }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { The client/peer's new authentication status }
    ClientAuthStatus: EOS_EAntiCheatCommonClientAuthStatus;
end;

type PEOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo = ^EOS_AntiCheatCommon_OnClientAuthStatusChangedCallbackInfo;

const EOS_ANTICHEATCOMMON_SETCLIENTDETAILS_API_LATEST = 1;
type EOS_AntiCheatCommon_SetClientDetailsOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_SETCLIENTDETAILS_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value used in RegisterClient/RegisterPeer }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { General flags associated with this client, if any }
    ClientFlags: EOS_EAntiCheatCommonClientFlags;
    { Input device being used by this client, if known }
    ClientInputMethod: EOS_EAntiCheatCommonClientInput;
end;

type PEOS_AntiCheatCommon_SetClientDetailsOptions = ^EOS_AntiCheatCommon_SetClientDetailsOptions;

const EOS_ANTICHEATCOMMON_SETGAMESESSIONID_API_LATEST = 1;
type EOS_AntiCheatCommon_SetGameSessionIdOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_SETGAMESESSIONID_API_LATEST. }
    ApiVersion: cint32;
    { Game session identifier }
    GameSessionId: PChar;
end;

type PEOS_AntiCheatCommon_SetGameSessionIdOptions = ^EOS_AntiCheatCommon_SetGameSessionIdOptions;

const EOS_ANTICHEATCOMMON_REGISTEREVENT_API_LATEST = 1;
const EOS_ANTICHEATCOMMON_REGISTEREVENT_CUSTOMEVENTBASE = $10000000;
const EOS_ANTICHEATCOMMON_REGISTEREVENT_MAX_PARAMDEFSCOUNT = 12;
type EOS_AntiCheatCommon_RegisterEventParamDef = record
    { Parameter name. Allowed characters are 0-9, A-Z, a-z, '_', '-' }
    ParamName: PChar;
    { Parameter type }
    ParamType: EOS_EAntiCheatCommonEventParamType;
end;
type PEOS_AntiCheatCommon_RegisterEventParamDef = ^EOS_AntiCheatCommon_RegisterEventParamDef;
type EOS_AntiCheatCommon_RegisterEventOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_REGISTEREVENT_API_LATEST. }
    ApiVersion: cint32;
    { Unique event identifier. Must be >= EOS_ANTICHEATCOMMON_REGISTEREVENT_CUSTOMEVENTBASE. }
    EventId: cuint32;
    { Name of the custom event. Allowed characters are 0-9, A-Z, a-z, '_', '-' }
    EventName: PChar;
    { Type of the custom event }
    EventType: EOS_EAntiCheatCommonEventType;
    { Number of parameters described in ParamDefs. Must be <= EOS_ANTICHEATCOMMON_REGISTEREVENT_MAX_PARAMDEFSCOUNT. }
    ParamDefsCount: cuint32;
    { Pointer to an array of EOS_AntiCheatCommon_RegisterEventParamDef with ParamDefsCount elements }
    ParamDefs: ^EOS_AntiCheatCommon_RegisterEventParamDef;
end;

type PEOS_AntiCheatCommon_RegisterEventOptions = ^EOS_AntiCheatCommon_RegisterEventOptions;

const EOS_ANTICHEATCOMMON_LOGEVENT_API_LATEST = 1;
const EOS_ANTICHEATCOMMON_LOGEVENT_STRING_MAX_LENGTH = 39;
type EOS_AntiCheatCommon_LogEventParamPair = record
    { Parameter type }
    ParamValueType: EOS_EAntiCheatCommonEventParamType;
    { Parameter value }
    {union
    
        EOS_AntiCheatCommon_ClientHandle ClientHandle;
        const char* String; // Will be truncated if longer than EOS_ANTICHEATCOMMON_LOGEVENT_STRING_MAX_LENGTH bytes.
        uint32_t UInt32;
        int32_t Int32;
        uint64_t UInt64;
        int64_t Int64;
        EOS_AntiCheatCommon_Vec3f Vec3f;
        EOS_AntiCheatCommon_Quat Quat;
     ParamValue;}
end;
type PEOS_AntiCheatCommon_LogEventParamPair = ^EOS_AntiCheatCommon_LogEventParamPair;
type EOS_AntiCheatCommon_LogEventOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGEVENT_API_LATEST. }
    ApiVersion: cint32;
    { Optional client who this event is primarily associated with. If not applicable, use 0. }
    ClientHandle: EOS_AntiCheatCommon_ClientHandle;
    { Unique event identifier previously configured in RegisterEvent }
    EventId: cuint32;
    { Number of parameters described in Params }
    ParamsCount: cuint32;
    { Set of parameter types previously configured in RegisterEvent, and their values }
    Params: ^EOS_AntiCheatCommon_LogEventParamPair;
end;
type PEOS_AntiCheatCommon_LogEventOptions = ^EOS_AntiCheatCommon_LogEventOptions;
const EOS_ANTICHEATCOMMON_LOGGAMEROUNDSTART_API_LATEST = 1;
type EOS_AntiCheatCommon_LogGameRoundStartOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGGAMEROUNDSTART_API_LATEST. }
    ApiVersion: cint32;
    { Optional game session or match identifier useful for some backend API integrations }
    SessionIdentifier: PChar;
    { Optional name of the map being played }
    LevelName: PChar;
    { Optional name of the game mode being played }
    ModeName: PChar;
    { Optional length of the game round to be played, in seconds. If none, use 0. }
    RoundTimeSeconds: cuint32;
end;
type PEOS_AntiCheatCommon_LogGameRoundStartOptions = ^EOS_AntiCheatCommon_LogGameRoundStartOptions;
const EOS_ANTICHEATCOMMON_LOGGAMEROUNDEND_API_LATEST = 1;
type EOS_AntiCheatCommon_LogGameRoundEndOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGGAMEROUNDEND_API_LATEST. }
    ApiVersion: cint32;
    { Optional identifier for the winning team }
    WinningTeamId: cuint32;
end;
type PEOS_AntiCheatCommon_LogGameRoundEndOptions = ^EOS_AntiCheatCommon_LogGameRoundEndOptions;
const EOS_ANTICHEATCOMMON_LOGPLAYERSPAWN_API_LATEST = 1;
type EOS_AntiCheatCommon_LogPlayerSpawnOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGPLAYERSPAWN_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value used in RegisterClient/RegisterPeer }
    SpawnedPlayerHandle: EOS_AntiCheatCommon_ClientHandle;
    { Optional identifier for the player's team. If none, use 0. }
    TeamId: cuint32;
    { Optional identifier for the player's character. If none, use 0. }
    CharacterId: cuint32;
end;
type PEOS_AntiCheatCommon_LogPlayerSpawnOptions = ^EOS_AntiCheatCommon_LogPlayerSpawnOptions;
const EOS_ANTICHEATCOMMON_LOGPLAYERDESPAWN_API_LATEST = 1;
type EOS_AntiCheatCommon_LogPlayerDespawnOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGPLAYERDESPAWN_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value used in RegisterClient/RegisterPeer }
    DespawnedPlayerHandle: EOS_AntiCheatCommon_ClientHandle;
end;
type PEOS_AntiCheatCommon_LogPlayerDespawnOptions = ^EOS_AntiCheatCommon_LogPlayerDespawnOptions;
const EOS_ANTICHEATCOMMON_LOGPLAYERREVIVE_API_LATEST = 1;
type EOS_AntiCheatCommon_LogPlayerReviveOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGPLAYERREVIVE_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value used in RegisterClient/RegisterPeer }
    RevivedPlayerHandle: EOS_AntiCheatCommon_ClientHandle;
    { Locally unique value used in RegisterClient/RegisterPeer }
    ReviverPlayerHandle: EOS_AntiCheatCommon_ClientHandle;
end;
type PEOS_AntiCheatCommon_LogPlayerReviveOptions = ^EOS_AntiCheatCommon_LogPlayerReviveOptions;
const EOS_ANTICHEATCOMMON_LOGPLAYERTICK_API_LATEST = 2;
type EOS_AntiCheatCommon_LogPlayerTickOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGPLAYERTICK_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value used in RegisterClient/RegisterPeer }
    PlayerHandle: EOS_AntiCheatCommon_ClientHandle;
    { Attack origin world position as a 3D vector }
    PlayerPosition: ^EOS_AntiCheatCommon_Vec3f;
    { Attack direction as a quaternion }
    PlayerViewRotation: ^EOS_AntiCheatCommon_Quat;
    { True if the player's view is zoomed (e.g. using a sniper rifle), otherwise false }
    bIsPlayerViewZoomed: EOS_Bool;
    { Player's current health value }
    PlayerHealth: Single;
    { Any movement state applicable }
    PlayerMovementState: EOS_EAntiCheatCommonPlayerMovementState;
end;
type PEOS_AntiCheatCommon_LogPlayerTickOptions = ^EOS_AntiCheatCommon_LogPlayerTickOptions;
const EOS_ANTICHEATCOMMON_LOGPLAYERUSEWEAPON_API_LATEST = 2;
const EOS_ANTICHEATCOMMON_LOGPLAYERUSEWEAPON_WEAPONNAME_MAX_LENGTH = 16;
type EOS_AntiCheatCommon_LogPlayerUseWeaponData = record
    { Locally unique value used in RegisterClient/RegisterPeer }
    PlayerHandle: EOS_AntiCheatCommon_ClientHandle;
    { Player's current world position as a 3D vector }
    PlayerPosition: ^EOS_AntiCheatCommon_Vec3f;
    { Player's view rotation as a quaternion }
    PlayerViewRotation: ^EOS_AntiCheatCommon_Quat;
    { True if the player's view is zoomed (e.g. using a sniper rifle), otherwise false }
    bIsPlayerViewZoomed: EOS_Bool;
    { Set to true if the player is using a melee attack, otherwise false }
    bIsMeleeAttack: EOS_Bool;
    { Name of the weapon used. Will be truncated to EOS_ANTICHEATCOMMON_LOGPLAYERUSEWEAPON_WEAPONNAME_MAX_LENGTH bytes if longer. }
    WeaponName: PChar;
end;
type PEOS_AntiCheatCommon_LogPlayerUseWeaponData = ^EOS_AntiCheatCommon_LogPlayerUseWeaponData;
type EOS_AntiCheatCommon_LogPlayerUseWeaponOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGPLAYERUSEWEAPON_API_LATEST. }
    ApiVersion: cint32;
    { Struct containing detailed information about a weapon use event }
    UseWeaponData: ^EOS_AntiCheatCommon_LogPlayerUseWeaponData;
end;
type PEOS_AntiCheatCommon_LogPlayerUseWeaponOptions = ^EOS_AntiCheatCommon_LogPlayerUseWeaponOptions;
const EOS_ANTICHEATCOMMON_LOGPLAYERUSEABILITY_API_LATEST = 1;
type EOS_AntiCheatCommon_LogPlayerUseAbilityOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGPLAYERUSEABILITY_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value used in RegisterClient/RegisterPeer }
    PlayerHandle: EOS_AntiCheatCommon_ClientHandle;
    { Game defined unique identifier for the ability being used }
    AbilityId: cuint32;
    { Duration of the ability effect in milliseconds. If not applicable, use 0. }
    AbilityDurationMs: cuint32;
    { Cooldown until the ability can be used again in milliseconds. If not applicable, use 0. }
    AbilityCooldownMs: cuint32;
end;
type PEOS_AntiCheatCommon_LogPlayerUseAbilityOptions = ^EOS_AntiCheatCommon_LogPlayerUseAbilityOptions;
const EOS_ANTICHEATCOMMON_LOGPLAYERTAKEDAMAGE_API_LATEST = 2;
type EOS_AntiCheatCommon_LogPlayerTakeDamageOptions = record
    { API Version: Set this to EOS_ANTICHEATCOMMON_LOGPLAYERTAKEDAMAGE_API_LATEST. }
    ApiVersion: cint32;
    { Locally unique value used in RegisterClient/RegisterPeer }
    VictimPlayerHandle: EOS_AntiCheatCommon_ClientHandle;
    { Victim player's current world position as a 3D vector }
    VictimPlayerPosition: ^EOS_AntiCheatCommon_Vec3f;
    { Victim player's view rotation as a quaternion }
    VictimPlayerViewRotation: ^EOS_AntiCheatCommon_Quat;
    { Locally unique value used in RegisterClient/RegisterPeer }
    AttackerPlayerHandle: EOS_AntiCheatCommon_ClientHandle;
    { Attacker player's current world position as a 3D vector }
    AttackerPlayerPosition: ^EOS_AntiCheatCommon_Vec3f;
    { Attacker player's view rotation as a quaternion }
    AttackerPlayerViewRotation: ^EOS_AntiCheatCommon_Quat;
    {
     * True if the damage was applied instantly at the time of attack from the game
     * simulation's perspective, otherwise false (simulated ballistics, arrow, etc).
     }
    bIsHitscanAttack: EOS_Bool;
    {
     * True if there is a visible line of sight between the attacker and the victim at the time
     * that damage is being applied, false if there is an obstacle like a wall or terrain in
     * the way. For some situations like melee or hitscan weapons this is trivially
     * true, for others like projectiles with simulated physics it may not be e.g. a player
     * could fire a slow moving projectile and then move behind cover before it strikes.
     }
    bHasLineOfSight: EOS_Bool;
    { True if this was a critical hit that causes extra damage (e.g. headshot) }
    bIsCriticalHit: EOS_Bool;
    { Identifier of the victim bone hit in this damage event }
    HitBoneId_DEPRECATED: cuint32;
    { Number of health points that the victim lost due to this damage event }
    DamageTaken: Single;
    { Number of health points that the victim has remaining after this damage event }
    HealthRemaining: Single;
    { Source of the damage event }
    DamageSource: EOS_EAntiCheatCommonPlayerTakeDamageSource;
    { Type of the damage being applied }
    DamageType: EOS_EAntiCheatCommonPlayerTakeDamageType;
    { Result of the damage for the victim, if any }
    DamageResult: EOS_EAntiCheatCommonPlayerTakeDamageResult;
    { PlayerUseWeaponData associated with this damage event if available, otherwise NULL }
    PlayerUseWeaponData: ^EOS_AntiCheatCommon_LogPlayerUseWeaponData;
    { Time in milliseconds since the PlayerUseWeaponData event occurred if available, otherwise 0 }
    TimeSincePlayerUseWeaponMs: cuint32;
    { World position where damage hit the victim as a 3D vector if available, otherwise NULL *}
    DamagePosition: ^EOS_AntiCheatCommon_Vec3f;
end;
type PEOS_AntiCheatCommon_LogPlayerTakeDamageOptions = ^EOS_AntiCheatCommon_LogPlayerTakeDamageOptions;
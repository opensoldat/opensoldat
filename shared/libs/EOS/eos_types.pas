type EOS_HPlatform = Pointer;

{ Client credentials. }
EOS_Platform_ClientCredentials = record
    { Client ID of the service permissions entry. Set to NULL if no service permissions are used. }
    ClientId: PChar;
    { Client secret for accessing the set of permissions. Set to NULL if no service permissions are used. }
    ClientSecret: PChar;
end;

{ The most recent version of the EOS_Platform_RTCOptions API. }
const EOS_PLATFORM_RTCOPTIONS_API_LATEST = 1;

{ Platform RTC options. }
type EOS_Platform_RTCOptions = record
    { API Version: Set this to EOS_PLATFORM_RTCOPTIONS_API_LATEST. }
    ApiVersion: cint32;
    {
     * This field is for platform specific initialization if any.
     *
     * If provided then the structure will be located in <System>/eos_<System>.h.
     * The structure will be named EOS_<System>_RTCOptions.
     }
    PlatformSpecificOptions: Pointer;
end;


const EOS_COUNTRYCODE_MAX_LENGTH = 4;
const EOS_COUNTRYCODE_MAX_BUFFER_LEN = (EOS_COUNTRYCODE_MAX_LENGTH + 1);
const EOS_LOCALECODE_MAX_LENGTH = 9;
const EOS_LOCALECODE_MAX_BUFFER_LEN = (EOS_LOCALECODE_MAX_LENGTH + 1);

const EOS_PLATFORM_OPTIONS_API_LATEST = 11;

{ Platform Creation Flags used in EOS_Platform_Create }

{ A bit that indicates the SDK is being loaded in a game editor, like Unity or UE4 Play-in-Editor }
const EOS_PF_LOADING_IN_EDITOR =                $00001;
{ A bit that indicates the SDK should skip initialization of the overlay, which is used by the in-app purchase flow and social overlay. This bit is implied by EOS_PF_LOADING_IN_EDITOR }
const EOS_PF_DISABLE_OVERLAY =                  $00002;
{ A bit that indicates the SDK should skip initialization of the social overlay, which provides an overlay UI for social features. This bit is implied by EOS_PF_LOADING_IN_EDITOR or EOS_PF_DISABLE_OVERLAY }
const EOS_PF_DISABLE_SOCIAL_OVERLAY =           $00004;
{ A reserved bit }
const EOS_PF_RESERVED1 =                        $00008;
{ A bit that indicates your game would like to opt-in to experimental Direct3D 9 support for the overlay. This flag is only relevant on Windows }
const EOS_PF_WINDOWS_ENABLE_OVERLAY_D3D9 =      $00010;
{ A bit that indicates your game would like to opt-in to experimental Direct3D 10 support for the overlay. This flag is only relevant on Windows }
const EOS_PF_WINDOWS_ENABLE_OVERLAY_D3D10 =     $00020;
{ A bit that indicates your game would like to opt-in to experimental OpenGL support for the overlay. This flag is only relevant on Windows }
const EOS_PF_WINDOWS_ENABLE_OVERLAY_OPENGL =    $00040;

{ Platform options for EOS_Platform_Create. }
type EOS_Platform_Options = record
    { API Version: Set this to EOS_PLATFORM_OPTIONS_API_LATEST. }
    ApiVersion: cint32;
    { A reserved field that should always be nulled. }
    Reserved: Pointer;
    { The product ID for the running application, found on the dev portal }
    ProductId: PChar;
    { The sandbox ID for the running application, found on the dev portal }
    SandboxId: PChar;
    { Set of service permissions associated with the running application }
    ClientCredentials: EOS_Platform_ClientCredentials;
    { Set this to EOS_FALSE if the application is running as a client with a local user, otherwise set to EOS_TRUE (e.g. for a dedicated game server) }
    bIsServer: EOS_Bool;
    { Used by Player Data Storage and Title Storage. Must be null initialized if unused. 256-bit Encryption Key for file encryption in hexadecimal format (64 hex chars)}
    EncryptionKey: PChar;
    { The override country code to use for the logged in user. (EOS_COUNTRYCODE_MAX_LENGTH)}
    OverrideCountryCode: PChar;
    { The override locale code to use for the logged in user. This follows ISO 639. (EOS_LOCALECODE_MAX_LENGTH)}
    OverrideLocaleCode: PChar;
    { The deployment ID for the running application, found on the dev portal }
    DeploymentId: PChar;
    { Platform creation flags, e.g. EOS_PF_LOADING_IN_EDITOR. This is a bitwise-or union of the defined flags. }
    Flags: cuint64;
    { Used by Player Data Storage and Title Storage. Must be null initialized if unused. Cache directory path. Absolute path to the folder that is going to be used for caching temporary data. The path is created if it's missing. }
    CacheDirectory: PChar;
    {
     * A budget, measured in milliseconds, for EOS_Platform_Tick to do its work. When the budget is met or exceeded (or if no work is available), EOS_Platform_Tick will return.
     * This allows your game to amortize the cost of SDK work across multiple frames in the event that a lot of work is queued for processing.
     * Zero is interpreted as "perform all available work".
     }
    TickBudgetInMilliseconds: cuint32;
    { RTC options. Setting to NULL will disable RTC features (e.g. voice) }
    RTCOptions: Pointer;
end;

type PEOS_Platform_Options = ^EOS_Platform_Options;

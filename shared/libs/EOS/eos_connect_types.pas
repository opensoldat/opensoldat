type EOS_HConnect = Pointer;

{ Max length of an external account ID in string form }
const EOS_CONNECT_EXTERNAL_ACCOUNT_ID_MAX_LENGTH = 256;

{ The most recent version of the EOS_Connect_Credentials struct. }
const EOS_CONNECT_CREDENTIALS_API_LATEST = 1;

{
 * A structure that contains external login credentials.
 *
 * This is part of the input structure EOS_Connect_LoginOptions.
 *
 * @see EOS_EExternalCredentialType
 * @see EOS_Connect_Login
 }
type EOS_Connect_Credentials = record
    { API Version: Set this to EOS_CONNECT_CREDENTIALS_API_LATEST. }
    ApiVersion: cint32;
    { External token associated with the user logging in. }
    Token: PChar;
    { Type of external login; identifies the auth method to use. }
    _Type: EOS_EExternalCredentialType;
end;

{ Max length of a display name, not including the terminating null. }
const EOS_CONNECT_USERLOGININFO_DISPLAYNAME_MAX_LENGTH = 32;

{ The most recent version of the EOS_Connect_UserLoginInfo struct. }
const EOS_CONNECT_USERLOGININFO_API_LATEST = 1;

{
 * Additional information about the local user.
 *
 * As the information passed here is client-controlled and not part of the user authentication tokens,
 * it is only treated as non-authoritative informational data to be used by some of the feature services.
 * For example displaying player names in Leaderboards rankings.
 }
type EOS_Connect_UserLoginInfo = record
    { API Version: Set this to EOS_CONNECT_USERLOGININFO_API_LATEST. }
    ApiVersion: cint32;
    {
     * The user's display name on the identity provider systems as UTF-8 encoded null-terminated string.
     * The length of the name can be at maximum up to EOS_CONNECT_USERLOGININFO_DISPLAYNAME_MAX_LENGTH bytes.
     }
    DisplayName: PChar;
end;


{ The most recent version of the EOS_Connect_Login API. }
const EOS_CONNECT_LOGIN_API_LATEST = 2;

{
 * Input parameters for the EOS_Connect_Login function.
 }
type EOS_Connect_LoginOptions = record
    { API Version: Set this to EOS_CONNECT_LOGIN_API_LATEST. }
    ApiVersion: cint32;
    { Credentials specified for a given login method }
    Credentials: ^EOS_Connect_Credentials;
    {
     * Additional non-authoritative information about the local user.
     *
     * This field is required to be set and only used when authenticating the user using Amazon, Apple, Google, Nintendo Account, Nintendo Service Account, Oculus or the Device ID feature login.
     * When using other identity providers, set to NULL.
     }
    UserLoginInfo: ^EOS_Connect_UserLoginInfo;
end;

type PEOS_Connect_LoginOptions = ^EOS_Connect_LoginOptions;

{
 * Output parameters for the EOS_Connect_Login function.
 }
type EOS_Connect_LoginCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_Login. }
    ClientData: Pointer;
    { If login was succesful, this is the Product User ID of the local player that logged in. }
    LocalUserId: EOS_ProductUserId;
    {
     * If the user was not found with credentials passed into EOS_Connect_Login,
     * this continuance token can be passed to either EOS_Connect_CreateUser
     * or EOS_Connect_LinkAccount to continue the flow.
     }
    ContinuanceToken: EOS_ContinuanceToken;
end;

type PEOS_Connect_LoginCallbackInfo = ^EOS_Connect_LoginCallbackInfo;

{
 * Function prototype definition for callbacks passed to EOS_Connect_Login.
 *
 * @param Data A EOS_Connect_LoginCallbackInfo containing the output information and result.
 }
type EOS_Connect_OnLoginCallback = procedure(Data: PEOS_Connect_LoginCallbackInfo); stdcall;

{ The most recent version of the EOS_Connect_CreateUser API. }
const EOS_CONNECT_CREATEUSER_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_CreateUser function.
 }
type EOS_Connect_CreateUserOptions = record
    { API Version: Set this to EOS_CONNECT_CREATEUSER_API_LATEST. }
    ApiVersion: cint32;
    { Continuance token from previous call to EOS_Connect_Login }
    ContinuanceToken: EOS_ContinuanceToken;
end;

type PEOS_Connect_CreateUserOptions = ^EOS_Connect_CreateUserOptions;

{
 * Output parameters for the EOS_Connect_CreateUser function.
 }
type EOS_Connect_CreateUserCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_CreateUser. }
    ClientData: Pointer;
    { If the operation succeeded, this is the Product User ID of the local user who was created. }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_CreateUserCallbackInfo = ^EOS_Connect_CreateUserCallbackInfo;

type EOS_Connect_OnCreateUserCallback = procedure(Data: PEOS_Connect_CreateUserCallbackInfo);
{ The most recent version of the EOS_Connect_LinkAccount API. }
const EOS_CONNECT_LINKACCOUNT_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_LinkAccount function.
 }
type EOS_Connect_LinkAccountOptions = record
    { API Version: Set this to EOS_CONNECT_LINKACCOUNT_API_LATEST. }
    ApiVersion: cint32;
    { The existing logged in product user for which to link the external account described by the continuance token. }
    LocalUserId: EOS_ProductUserId;
    { Continuance token from previous call to EOS_Connect_Login. }
    ContinuanceToken: EOS_ContinuanceToken;
end;

type PEOS_Connect_LinkAccountOptions = ^EOS_Connect_LinkAccountOptions;

{
 * Output parameters for the EOS_Connect_LinkAccount function.
 }
type EOS_Connect_LinkAccountCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_LinkAccount. }
    ClientData: Pointer;
    { The Product User ID of the existing, logged-in user whose account was linked (on success). }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_LinkAccountCallbackInfo = ^EOS_Connect_LinkAccountCallbackInfo;

{
 * Function prototype definition for callbacks passed to EOS_Connect_LinkAccount.
 *
 * @param Data A EOS_Connect_LinkAccountCallbackInfo containing the output information and result.
 }
type EOS_Connect_OnLinkAccountCallback = procedure(Data: PEOS_Connect_LinkAccountCallbackInfo);
{ The most recent version of the EOS_Connect_UnlinkAccount API. }
const EOS_CONNECT_UNLINKACCOUNT_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_UnlinkAccount Function.
 }
type EOS_Connect_UnlinkAccountOptions = record
    { API Version: Set this to EOS_CONNECT_UNLINKACCOUNT_API_LATEST. }
    ApiVersion: cint32;
    {
     * Existing logged in product user that is subject for the unlinking operation.
     * The external account that was used to login to the product user will be unlinked from the owning keychain.
     *
     * On a successful operation, the product user will be logged out as the external account used to authenticate the user was unlinked from the owning keychain.
     }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_UnlinkAccountOptions = ^EOS_Connect_UnlinkAccountOptions;

{
 * Output parameters for the EOS_Connect_UnlinkAccount Function.
 }
type EOS_Connect_UnlinkAccountCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_UnlinkAccount. }
    ClientData: Pointer;
    {
     * The product user that was subject for the unlinking operation.
     *
     * On a successful operation, the local authentication session for the product user will have been invalidated.
     * As such, the LocalUserId value will no longer be valid in any context unless the user is logged into it again.
     }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_UnlinkAccountCallbackInfo = ^EOS_Connect_UnlinkAccountCallbackInfo;

{
 * Function prototype definition for callbacks passed to EOS_Connect_UnlinkAccount.
 *
 * @param Data A EOS_Connect_UnlinkAccountCallbackInfo containing the output information and result
 }
type EOS_Connect_OnUnlinkAccountCallback = procedure(Data: PEOS_Connect_UnlinkAccountCallbackInfo);
{ The most recent version of the EOS_Connect_CreateDeviceId API. }
const EOS_CONNECT_CREATEDEVICEID_API_LATEST = 1;

{ Max length of a device model name, not including the terminating null }
const EOS_CONNECT_CREATEDEVICEID_DEVICEMODEL_MAX_LENGTH = 64;

{
 * Input parameters for the EOS_Connect_CreateDeviceId function.
 }
type EOS_Connect_CreateDeviceIdOptions = record
    { API Version: Set this to EOS_CONNECT_CREATEDEVICEID_API_LATEST. }
    ApiVersion: cint32;
    {
     * A freeform text description identifying the device type and model,
     * which can be used in account linking management to allow the player
     * and customer support to identify different devices linked to an EOS
     * user keychain. For example 'iPhone 6S' or 'PC Windows'.
     *
     * The input string must be in UTF-8 character format, with a maximum
     * length of 64 characters. Longer string will be silently truncated.
     *
     * This field is required to be present.
     }
    DeviceModel: PChar;
end;

type PEOS_Connect_CreateDeviceIdOptions = ^EOS_Connect_CreateDeviceIdOptions;

{
 * Output parameters for the EOS_Connect_CreateDeviceId function.
 }
type EOS_Connect_CreateDeviceIdCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_CreateDeviceId. }
    ClientData: Pointer;
end;

type PEOS_Connect_CreateDeviceIdCallbackInfo = ^EOS_Connect_CreateDeviceIdCallbackInfo;

type EOS_Connect_OnCreateDeviceIdCallback = procedure(Data: PEOS_Connect_CreateDeviceIdCallbackInfo); stdcall;
{ The most recent version of the EOS_Connect_DeleteDeviceId API. }
const EOS_CONNECT_DELETEDEVICEID_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_DeleteDeviceId function.
 }
type EOS_Connect_DeleteDeviceIdOptions = record
    { API Version: Set this to EOS_CONNECT_DELETEDEVICEID_API_LATEST. }
    ApiVersion: cint32;
end;

type PEOS_Connect_DeleteDeviceIdOptions = ^EOS_Connect_DeleteDeviceIdOptions;

{
 * Output parameters for the EOS_Connect_DeleteDeviceId function.
 }
type EOS_Connect_DeleteDeviceIdCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_DeleteDeviceId }
    ClientData: Pointer;
end;

type PEOS_Connect_DeleteDeviceIdCallbackInfo = ^EOS_Connect_DeleteDeviceIdCallbackInfo;

type EOS_Connect_OnDeleteDeviceIdCallback = procedure(Data: PEOS_Connect_DeleteDeviceIdCallbackInfo);

{ The most recent version of the EOS_Connect_TransferDeviceIdAccount API. }
const EOS_CONNECT_TRANSFERDEVICEIDACCOUNT_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_TransferDeviceIdAccount Function.
 }
type EOS_Connect_TransferDeviceIdAccountOptions = record
    { API Version: Set this to EOS_CONNECT_TRANSFERDEVICEIDACCOUNT_API_LATEST. }
    ApiVersion: cint32;
    {
     * The primary product user id, currently logged in, that is already associated with a real external user account (such as Epic Games, PlayStation(TM)Network, Xbox Live and other).
     *
     * The account linking keychain that owns this product user will be preserved and receive
     * the Device ID login credentials under it.
     }
    PrimaryLocalUserId: EOS_ProductUserId;
    {
     * The product user id, currently logged in, that has been originally created using the anonymous local Device ID login type,
     * and whose Device ID login will be transferred to the keychain of the PrimaryLocalUserId.
     }
    LocalDeviceUserId: EOS_ProductUserId;
    {
     * Specifies which EOS_ProductUserId (i.e. game progression) will be preserved in the operation.
     *
     * After a successful transfer operation, subsequent logins using the same external account or
     * the same local Device ID login will return user session for the ProductUserIdToPreserve.
     *
     * Set to either PrimaryLocalUserId or LocalDeviceUserId.
     }
    ProductUserIdToPreserve: EOS_ProductUserId;
end;

type PEOS_Connect_TransferDeviceIdAccountOptions = ^EOS_Connect_TransferDeviceIdAccountOptions;

{
 * Output parameters for the EOS_Connect_TransferDeviceIdAccount Function.
 }
type EOS_Connect_TransferDeviceIdAccountCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_TransferDeviceIdAccount. }
    ClientData: Pointer;
    {
     * The ProductUserIdToPreserve that was passed to the original EOS_Connect_TransferDeviceIdAccount call.
     *
     * On successful operation, this EOS_ProductUserId will have a valid authentication session
     * and the other EOS_ProductUserId value has been discarded and lost forever.
     *
     * The application should remove any registered notification callbacks for the discarded EOS_ProductUserId as obsolete.
     }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_TransferDeviceIdAccountCallbackInfo = ^EOS_Connect_TransferDeviceIdAccountCallbackInfo;

{
 * Function prototype definition for callbacks passed to EOS_Connect_TransferDeviceIdAccount.
 *
 * @param Data A EOS_Connect_TransferDeviceIdAccountCallbackInfo containing the output information and result.
 }
type EOS_Connect_OnTransferDeviceIdAccountCallback = procedure(Data: PEOS_Connect_TransferDeviceIdAccountCallbackInfo);
{ The most recent version of the EOS_Connect_QueryExternalAccountMappings API. }
const EOS_CONNECT_QUERYEXTERNALACCOUNTMAPPINGS_API_LATEST = 1;

{ Maximum number of account IDs that can be queried at once }
const EOS_CONNECT_QUERYEXTERNALACCOUNTMAPPINGS_MAX_ACCOUNT_IDS = 128;

{
 * Input parameters for the EOS_Connect_QueryExternalAccountMappings function.
 }
type EOS_Connect_QueryExternalAccountMappingsOptions = record
    { API Version: Set this to EOS_CONNECT_QUERYEXTERNALACCOUNTMAPPINGS_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID of the existing, logged-in user who is querying account mappings. }
    LocalUserId: EOS_ProductUserId;
    { External auth service supplying the account IDs in string form. }
    AccountIdType: EOS_EExternalAccountType;
    { An array of external account IDs to map to the product user ID representation. }
    ExternalAccountIds: PPChar;
    { Number of account IDs to query. }
    ExternalAccountIdCount: cuint32;
end;

type PEOS_Connect_QueryExternalAccountMappingsOptions = ^EOS_Connect_QueryExternalAccountMappingsOptions;

{
 * Output parameters for the EOS_Connect_QueryExternalAccountMappings function.
 }
type EOS_Connect_QueryExternalAccountMappingsCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_QueryExternalAccountMappings. }
    ClientData: Pointer;
    { The Product User ID of the existing, logged-in user who made the request. }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_QueryExternalAccountMappingsCallbackInfo = ^EOS_Connect_QueryExternalAccountMappingsCallbackInfo;

{
 * Function prototype definition for callbacks passed to EOS_Connect_QueryExternalAccountMappings.
 *
 * @param Data A EOS_Connect_QueryExternalAccountMappingsCallbackInfo containing the output information and result.
 }
type EOS_Connect_OnQueryExternalAccountMappingsCallback = procedure(Data: PEOS_Connect_QueryExternalAccountMappingsCallbackInfo);
{ The most recent version of the EOS_Connect_GetExternalAccountMapping API. }
const EOS_CONNECT_GETEXTERNALACCOUNTMAPPING_API_LATEST = 1;

{ DEPRECATED! Use EOS_CONNECT_GETEXTERNALACCOUNTMAPPING_API_LATEST instead. }
const EOS_CONNECT_GETEXTERNALACCOUNTMAPPINGS_API_LATEST  = EOS_CONNECT_GETEXTERNALACCOUNTMAPPING_API_LATEST;

{
 * Input parameters for the EOS_Connect_GetExternalAccountMapping function.
 }
type EOS_Connect_GetExternalAccountMappingsOptions = record
    { API Version: Set this to EOS_CONNECT_GETEXTERNALACCOUNTMAPPING_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID of the existing, logged-in user who is querying account mappings. }
    LocalUserId: EOS_ProductUserId;
    { External auth service supplying the account IDs in string form. }
    AccountIdType: EOS_EExternalAccountType;
    { Target user to retrieve the mapping for, as an external account ID. }
    TargetExternalUserId: PChar;
end;

type PEOS_Connect_GetExternalAccountMappingsOptions = ^EOS_Connect_GetExternalAccountMappingsOptions;

{ The most recent version of the EOS_Connect_QueryProductUserIdMappings API. }
const EOS_CONNECT_QUERYPRODUCTUSERIDMAPPINGS_API_LATEST = 2;

{
 * Input parameters for the EOS_Connect_QueryProductUserIdMappings function.
 }
type EOS_Connect_QueryProductUserIdMappingsOptions = record
    { API Version: Set this to EOS_CONNECT_QUERYPRODUCTUSERIDMAPPINGS_API_LATEST. }
    ApiVersion: cint32;
    {
     * Game Clients set this field to the Product User ID of the local authenticated user querying account mappings.
     * Game Servers set this field to NULL. Usage is allowed given that the configured client policy for server credentials permit it.
   }
    { Deprecated - all external mappings are included in this call, it is no longer necessary to specify this value. }
    AccountIdType_DEPRECATED: EOS_EExternalAccountType;
    { An array of Product User IDs to query for the given external account representation. }
    ProductUserIds: ^EOS_ProductUserId;
    { Number of Product User IDs to query. }
    ProductUserIdCount: cuint32;
end;

type PEOS_Connect_QueryProductUserIdMappingsOptions = ^EOS_Connect_QueryProductUserIdMappingsOptions;

{
 * Output parameters for the EOS_Connect_QueryProductUserIdMappings function.
 }
type EOS_Connect_QueryProductUserIdMappingsCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_QueryProductUserIdMappings. }
    ClientData: Pointer;
    { The local Product User ID that was passed with the input options. }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_QueryProductUserIdMappingsCallbackInfo = ^EOS_Connect_QueryProductUserIdMappingsCallbackInfo;

{
 * Function prototype definition for callbacks passed to EOS_Connect_QueryProductUserIdMappings.
 *
 * @param Data A EOS_Connect_QueryProductUserIdMappingsCallbackInfo containing the output information and result.
 }
type EOS_Connect_OnQueryProductUserIdMappingsCallback = procedure(Data: PEOS_Connect_QueryProductUserIdMappingsCallbackInfo); stdcall;
{ The most recent version of the EOS_Connect_GetProductUserIdMapping API. }
const EOS_CONNECT_GETPRODUCTUSERIDMAPPING_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_GetProductUserIdMapping function.
 }
type EOS_Connect_GetProductUserIdMappingOptions = record
    { API Version: Set this to EOS_CONNECT_GETPRODUCTUSERIDMAPPING_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID of the existing, logged-in user that is querying account mappings. }
    LocalUserId: EOS_ProductUserId;
    { External auth service mapping to retrieve. }
    AccountIdType: EOS_EExternalAccountType;
    { The Product User ID of the user whose information is being requested. }
    TargetProductUserId: EOS_ProductUserId;
end;

type PEOS_Connect_GetProductUserIdMappingOptions = ^EOS_Connect_GetProductUserIdMappingOptions;

{ The most recent version of the EOS_Connect_GetProductUserExternalAccountCount API. }
const EOS_CONNECT_GETPRODUCTUSEREXTERNALACCOUNTCOUNT_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_GetProductUserExternalAccountCount function.
 }
type EOS_Connect_GetProductUserExternalAccountCountOptions = record
    { API Version: Set this to EOS_CONNECT_GETPRODUCTUSEREXTERNALACCOUNTCOUNT_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID to look for when getting external account info count from the cache. }
    TargetUserId: EOS_ProductUserId;
end;

type PEOS_Connect_GetProductUserExternalAccountCountOptions = ^EOS_Connect_GetProductUserExternalAccountCountOptions;

{ The most recent version of the EOS_Connect_CopyProductUserExternalAccountByIndex API. }
const EOS_CONNECT_COPYPRODUCTUSEREXTERNALACCOUNTBYINDEX_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_CopyProductUserExternalAccountByIndex function.
 }
type EOS_Connect_CopyProductUserExternalAccountByIndexOptions = record
    { API Version: Set this to EOS_CONNECT_COPYPRODUCTUSEREXTERNALACCOUNTBYINDEX_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID to look for when copying external account info from the cache. }
    TargetUserId: EOS_ProductUserId;
    { Index of the external account info to retrieve from the cache. }
    ExternalAccountInfoIndex: cuint32;
end;

type PEOS_Connect_CopyProductUserExternalAccountByIndexOptions = ^EOS_Connect_CopyProductUserExternalAccountByIndexOptions;

{ The most recent version of the EOS_Connect_CopyProductUserExternalAccountByAccountType API. }
const EOS_CONNECT_COPYPRODUCTUSEREXTERNALACCOUNTBYACCOUNTTYPE_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_CopyProductUserExternalAccountByAccountType function.
 }
type EOS_Connect_CopyProductUserExternalAccountByAccountTypeOptions = record
    { API Version: Set this to EOS_CONNECT_COPYPRODUCTUSEREXTERNALACCOUNTBYACCOUNTTYPE_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID to look for when copying external account info from the cache. }
    TargetUserId: EOS_ProductUserId;
    { External auth service account type to look for when copying external account info from the cache. }
    AccountIdType: EOS_EExternalAccountType;
end;

type PEOS_Connect_CopyProductUserExternalAccountByAccountTypeOptions = ^EOS_Connect_CopyProductUserExternalAccountByAccountTypeOptions;

{ The most recent version of the EOS_Connect_CopyProductUserExternalAccountByAccountId API. }
const EOS_CONNECT_COPYPRODUCTUSEREXTERNALACCOUNTBYACCOUNTID_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_CopyProductUserExternalAccountByAccountId function.
 }
type EOS_Connect_CopyProductUserExternalAccountByAccountIdOptions = record
    { API Version: Set this to EOS_CONNECT_COPYPRODUCTUSEREXTERNALACCOUNTBYACCOUNTID_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID to look for when copying external account info from the cache. }
    TargetUserId: EOS_ProductUserId;
    { External auth service account ID to look for when copying external account info from the cache. }
    AccountId: PChar;
end;

type PEOS_Connect_CopyProductUserExternalAccountByAccountIdOptions = ^EOS_Connect_CopyProductUserExternalAccountByAccountIdOptions;

{ The most recent version of the EOS_Connect_CopyProductUserInfo API. }
const EOS_CONNECT_COPYPRODUCTUSERINFO_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_CopyProductUserInfo function.
 }
type EOS_Connect_CopyProductUserInfoOptions = record
    { API Version: Set this to EOS_CONNECT_COPYPRODUCTUSERINFO_API_LATEST. }
    ApiVersion: cint32;
    { Product user ID to look for when copying external account info from the cache. }
    TargetUserId: EOS_ProductUserId;
end;

type PEOS_Connect_CopyProductUserInfoOptions = ^EOS_Connect_CopyProductUserInfoOptions;

{ Timestamp value representing an undefined time for last login time. }
const EOS_CONNECT_TIME_UNDEFINED = -1;

{ The most recent version of the EOS_Connect_ExternalAccountInfo struct. }
const EOS_CONNECT_EXTERNALACCOUNTINFO_API_LATEST = 1;

{
 * Contains information about an external account linked with a Product User ID.
 }
type EOS_Connect_ExternalAccountInfo = record
    { API Version: Set this to EOS_CONNECT_EXTERNALACCOUNTINFO_API_LATEST. }
    ApiVersion: cint32;
    { The Product User ID of the target user. }
    ProductUserId: EOS_ProductUserId;
    { Display name, can be null if not set. }
    DisplayName: PChar;
    { External account ID.
     * May be set to an empty string if the AccountIdType of another user belongs
     * to different account system than the local user's authenticated account.
     * The availability of this field is dependent on account system specifics.
     }
    AccountId: PChar;
    { The identity provider that owns the external account. }
    AccountIdType: EOS_EExternalAccountType;
    { The POSIX timestamp for the time the user last logged in, or EOS_CONNECT_TIME_UNDEFINED. }
    LastLoginTime: cint64;
end;

type PEOS_Connect_ExternalAccountInfo = ^EOS_Connect_ExternalAccountInfo;
type PPEOS_Connect_ExternalAccountInfo = ^PEOS_Connect_ExternalAccountInfo;


{
 * Release the memory associated with an external account info. This must be called on data retrieved from
 * EOS_Connect_CopyProductUserExternalAccountByIndex, EOS_Connect_CopyProductUserExternalAccountByAccountType,
 * EOS_Connect_CopyProductUserExternalAccountByAccountId or EOS_Connect_CopyProductUserInfo.
 *
 * @param ExternalAccountInfo The external account info data to release.
 *
 * @see EOS_Connect_CopyProductUserExternalAccountByIndex
 * @see EOS_Connect_CopyProductUserExternalAccountByAccountType
 * @see EOS_Connect_CopyProductUserExternalAccountByAccountId
 * @see EOS_Connect_CopyProductUserInfo
 }
//EOS_DECLARE_FUNC(void) EOS_Connect_ExternalAccountInfo_Release(EOS_Connect_ExternalAccountInfo* ExternalAccountInfo);
procedure EOS_Connect_ExternalAccountInfo_Release(ExternalAccountInfo: PEOS_Connect_ExternalAccountInfo); cdecl; external EOSLIB;

{ The most recent version of the EOS_Connect_AddNotifyAuthExpiration API. }
const EOS_CONNECT_ADDNOTIFYAUTHEXPIRATION_API_LATEST = 1;
{
 * Structure containing information for the auth expiration notification callback.
 }
type EOS_Connect_AddNotifyAuthExpirationOptions = record
    { API Version: Set this to EOS_CONNECT_ADDNOTIFYAUTHEXPIRATION_API_LATEST. }
    ApiVersion: cint32;
end;

type PEOS_Connect_AddNotifyAuthExpirationOptions = ^EOS_Connect_AddNotifyAuthExpirationOptions;

{ The most recent version of the EOS_Connect_OnAuthExpirationCallback API. }
const EOS_CONNECT_ONAUTHEXPIRATIONCALLBACK_API_LATEST = 1;

{
 * Output parameters for the EOS_Connect_OnAuthExpirationCallback function.
 }
type EOS_Connect_AuthExpirationCallbackInfo = record
    { Context that was passed into EOS_Connect_AddNotifyAuthExpiration. }
    ClientData: Pointer;
    { The Product User ID of the local player whose status has changed. }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_AuthExpirationCallbackInfo = ^EOS_Connect_AuthExpirationCallbackInfo;

{
 * Function prototype definition for notifications that come from EOS_Connect_AddNotifyAuthExpiration.
 *
 * @param Data A EOS_Connect_AuthExpirationCallbackInfo containing the output information and result.
 }
type EOS_Connect_OnAuthExpirationCallback = procedure(Data: PEOS_Connect_AuthExpirationCallbackInfo);

{ The most recent version of the EOS_Connect_AddNotifyLoginStatusChanged API. }
const EOS_CONNECT_ADDNOTIFYLOGINSTATUSCHANGED_API_LATEST = 1;
{
 * Structure containing information or the connect user login status change callback.
 }
type EOS_Connect_AddNotifyLoginStatusChangedOptions = record
    { API Version: Set this to EOS_CONNECT_ADDNOTIFYLOGINSTATUSCHANGED_API_LATEST. }
    ApiVersion: cint32;
end;

PEOS_Connect_AddNotifyLoginStatusChangedOptions = ^EOS_Connect_AddNotifyLoginStatusChangedOptions;

{
 * Output parameters for the EOS_Connect_OnLoginStatusChangedCallback function.
 }
type EOS_Connect_LoginStatusChangedCallbackInfo = record
    { Context that was passed into EOS_Connect_AddNotifyLoginStatusChanged. }
    ClientData: Pointer;
    { The Product User ID of the local player whose status has changed. }
    LocalUserId: EOS_ProductUserId;
    { The status prior to the change. }
    PreviousStatus: EOS_ELoginStatus;
    { The status at the time of the notification. }
    CurrentStatus: EOS_ELoginStatus;
end;

type PEOS_Connect_LoginStatusChangedCallbackInfo = ^EOS_Connect_LoginStatusChangedCallbackInfo;

{
 * Function prototype definition for notifications that come from EOS_Connect_AddNotifyLoginStatusChanged.
 *
 * @param Data A EOS_Connect_LoginStatusChangedCallbackInfo containing the output information and result.
 }
type EOS_Connect_OnLoginStatusChangedCallback = procedure(Data: PEOS_Connect_LoginStatusChangedCallbackInfo);

{ The most recent version of the EOS_Connect_IdToken struct. }
const EOS_CONNECT_IDTOKEN_API_LATEST = 1;

{
 * A structure that contains an ID token.
 * These structures are created by EOS_Connect_CopyIdToken and must be passed to EOS_Connect_IdToken_Release.
 }
type EOS_Connect_IdToken = record
    { API Version: Set this to EOS_CONNECT_IDTOKEN_API_LATEST. }
    ApiVersion: cint32;
    {
     * The Product User ID described by the ID token.
     * Use EOS_ProductUserId_FromString to populate this field when validating a received ID token.
     }
    ProductUserId: EOS_ProductUserId;
    { The ID token as a Json Web Token (JWT) string. }
    JsonWebToken: PChar;
end;

type PEOS_Connect_IdToken = ^EOS_Connect_IdToken;

{
 * Release the memory associated with an EOS_Connect_IdToken structure. This must be called on data retrieved from EOS_Connect_CopyIdToken.
 *
 * @param IdToken The ID token structure to be released.
 *
 * @see EOS_Connect_IdToken
 * @see EOS_Connect_CopyIdToken
 }
procedure EOS_Connect_IdToken_Release(IdToken: PEOS_Connect_IdToken); cdecl; external EOSLIB;

{ The most recent version of the EOS_Connect_CopyIdToken API. }
const EOS_CONNECT_COPYIDTOKEN_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_CopyIdToken function.
 }
type EOS_Connect_CopyIdTokenOptions = record
    { API Version: Set this to EOS_CONNECT_COPYIDTOKEN_API_LATEST. }
    ApiVersion: cint32;
    { The local Product User ID whose ID token should be copied. }
    LocalUserId: EOS_ProductUserId;
end;

type PEOS_Connect_CopyIdTokenOptions = ^EOS_Connect_CopyIdTokenOptions;

{ The most recent version of the EOS_Connect_VerifyIdToken API. }
const EOS_CONNECT_VERIFYIDTOKEN_API_LATEST = 1;

{
 * Input parameters for the EOS_Connect_VerifyIdToken function.
 }
type EOS_Connect_VerifyIdTokenOptions = record
    { API Version: Set this to EOS_CONNECT_VERIFYIDTOKEN_API_LATEST. }
    ApiVersion: cint32;
    {
     * The ID token to verify.
     * Use EOS_ProductUserId_FromString to populate the ProductUserId field of this struct.
     }
    IdToken: ^EOS_Connect_IdToken;
end;

type PEOS_Connect_VerifyIdTokenOptions = ^EOS_Connect_VerifyIdTokenOptions;

{
 * Output parameters for the EOS_Connect_VerifyIdToken Function.
 }
type EOS_Connect_VerifyIdTokenCallbackInfo = record
    { The EOS_EResult code for the operation. EOS_Success indicates that the operation succeeded; other codes indicate errors. }
    ResultCode: EOS_EResult;
    { Context that was passed into EOS_Connect_VerifyIdToken }
    ClientData: Pointer;
    { The Product User ID associated with the ID token. }
    ProductUserId: EOS_ProductUserId;
    {
     * Flag set to indicate whether account information is available.
     * Applications must always first check this value to be set before attempting
     * to read the AccountType, AccountId, Platform and DeviceType fields.
     *
     * This flag is always false for users that authenticated using EOS Connect Device ID.
    }
    bIsAccountInfoPresent: EOS_Bool;
    {
     * The identity provider that the user authenticated with to EOS Connect.
     *
     * If bIsAccountInfoPresent is set, this field describes the external account type.
    }
    AccountIdType: EOS_EExternalAccountType;
    {
     * The external account ID of the authenticated user.
     *
     * This value may be set to an empty string.
     }
    AccountId: PChar;
    {
     * Platform that the user is connected from.
     *
     * This value may be set to an empty string.
    }
    Platform: PCHar;
    {
     * Identifies the device type that the user is connected from.
     * Can be used to securely verify that the user is connected through a real Console device.
     *
     * This value may be set to an empty string.
    }
    DeviceType: PChar;
end;

type PEOS_Connect_VerifyIdTokenCallbackInfo = ^EOS_Connect_VerifyIdTokenCallbackInfo;

{
 * Function prototype definition for callbacks passed into EOS_Connect_VerifyIdToken.
 *
 * @param Data A EOS_Connect_VerifyIdTokenCallbackInfo containing the output information and result.
}
type EOS_Connect_OnVerifyIdTokenCallback = procedure(Data: PEOS_Connect_VerifyIdTokenCallbackInfo); stdcall;
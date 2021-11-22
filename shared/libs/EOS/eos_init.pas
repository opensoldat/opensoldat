{
 * Function prototype type definition for functions that allocate memory.
 *
 * Functions passed to EOS_Initialize to serve as memory allocators should return a pointer to the allocated memory.
 *
 * The returned pointer should have at least SizeInBytes available capacity and the memory address should be a multiple of Alignment.
 * The SDK will always call the provided function with an Alignment that is a power of 2.
 * Allocation failures should return a null pointer.
 }
//EXTERN_C typedef void* (EOS_MEMORY_CALL * EOS_AllocateMemoryFunc)(size_t SizeInBytes, size_t Alignment);
type EOS_AllocateMemoryFunc = function(SizeInBytes: csize_t; Alignment: csize_t): Pointer; stdcall;

{
 * Function prototype type definition for functions that reallocate memory.
 *
 * Functions passed to EOS_Initialize to serve as memory reallocators should return a pointer to the reallocated memory.
 * The returned pointer should have at least SizeInBytes available capacity and the memory address should be a multiple of alignment.
 * The SDK will always call the provided function with an Alignment that is a power of 2.
 * Reallocation failures should return a null pointer.
 }
//EXTERN_C typedef void* (EOS_MEMORY_CALL * EOS_ReallocateMemoryFunc)(void* Pointer, size_t SizeInBytes, size_t Alignment);
type EOS_ReallocateMemoryFunc = function(Pointer_: Pointer; SizeInBytes: csize_t; Alignment: csize_t): Pointer stdcall;
{
 * Function prototype type definition for functions that release memory.
 *
 * When the SDK is done with memory that has been allocated by a custom allocator passed to EOS_Initialize, it will call the corresponding memory release function.
 }
//EXTERN_C typedef void (EOS_MEMORY_CALL * EOS_ReleaseMemoryFunc)(void* Pointer);
type EOS_ReleaseMemoryFunc = procedure(Pointer_: Pointer); stdcall;

{ The most recent version of the EOS_Initialize_ThreadAffinity API. }
const EOS_INITIALIZE_THREADAFFINITY_API_LATEST = 1;

{
 * Options for initializing defining thread affinity for use by Epic Online Services SDK.
 * Set the affinity to 0 to allow EOS SDK to use a platform specific default value.
 }
type EOS_Initialize_ThreadAffinity = record
    { API Version: Set this to EOS_INITIALIZE_THREADAFFINITY_API_LATEST. }
    ApiVersion: cint32;
    { Any thread related to network management that is not IO. }
    NetworkWork: cuint64;
    { Any thread that will interact with a storage device. }
    StorageIo: cuint64;
    { Any thread that will generate web socket IO. }
    WebSocketIo: cuint64;
    { Any thread that will generate IO related to P2P traffic and mangement. }
    P2PIo: cuint64;
    { Any thread that will generate http request IO. }
    HttpRequestIo: cuint64;
end;

{ The most recent version of the EOS_Initialize API. }
const EOS_INITIALIZE_API_LATEST = 4;

{
 * Options for initializing the Epic Online Services SDK.
 }
type EOS_InitializeOptions = record
    { API Version: Set this to EOS_INITIALIZE_API_LATEST. }
    ApiVersion: cint32;
    { A custom memory allocator, if desired. }
    AllocateMemoryFunction: EOS_AllocateMemoryFunc;
    { A corresponding memory reallocator. If the AllocateMemoryFunction is nulled, then this field must also be nulled. }
    ReallocateMemoryFunction: EOS_ReallocateMemoryFunc;
    { A corresponding memory releaser. If the AllocateMemoryFunction is nulled, then this field must also be nulled. }
    ReleaseMemoryFunction: EOS_ReleaseMemoryFunc;
    {
     * The name of the product using the Epic Online Services SDK.
     *
     * The name string is required to be non-empty and at maximum of 64 characters long.
     * The string buffer can consist of the following characters:
     * A-Z, a-z, 0-9, dot, underscore, space, exclamation mark, question mark, and sign, hyphen, parenthesis, plus, minus, colon.
     }
    ProductName: PChar;
    {
     * Product version of the running application.
     *
     * The name string has same requirements as the ProductName string.
     }
    ProductVersion: PChar;
    { A reserved field that should always be nulled. }
    Reserved: Pointer;
    { 
     * This field is for system specific initialization if any.
     *
     * If provided then the structure will be located in <System>/eos_<system>.h.
     * The structure will be named EOS_<System>_InitializeOptions.
     }
    SystemInitializeOptions: Pointer;
    { The thread affinity override values for each category of thread. }
    OverrideThreadAffinity: Pointer;
end;

type PEOS_InitializeOptions = ^EOS_InitializeOptions;

{
 * Initialize the Epic Online Services SDK.
 *
 * Before calling any other function in the SDK, clients must call this function.
 *
 * This function must only be called one time and must have a corresponding EOS_Shutdown call.
 *
 * @param Options - The initialization options to use for the SDK.
 * @return An EOS_EResult is returned to indicate success or an error. 
 *
 * EOS_Success is returned if the SDK successfully initializes.
 * EOS_AlreadyConfigured is returned if the function has already been called.
 * EOS_InvalidParameters is returned if the provided options are invalid.
 }
function EOS_Initialize(const Options: PEOS_InitializeOptions): EOS_EResult; cdecl; external EOSLIB;

{
 * Tear down the Epic Online Services SDK.
 *
 * Once this function has been called, no more SDK calls are permitted; calling anything after EOS_Shutdown will result in undefined behavior.
 * @return An EOS_EResult is returned to indicate success or an error.
 * EOS_Success is returned if the SDK is successfully torn down.
 * EOS_NotConfigured is returned if a successful call to EOS_Initialize has not been made.
 * EOS_UnexpectedError is returned if EOS_Shutdown has already been called.
 }
function EOS_Shutdown(): EOS_EResult; cdecl; external EOSLIB;

{
 * Create a single Epic Online Services Platform Instance.
 *
 * The platform instance is used to gain access to the various Epic Online Services.
 *
 * This function returns an opaque handle to the platform instance, and that handle must be passed to EOS_Platform_Release to release the instance.
 *
 * @return An opaque handle to the platform instance.
 }
function EOS_Platform_Create(Options: PEOS_Platform_Options): EOS_HPlatform; cdecl; external EOSLIB;

{
 * Release an Epic Online Services platform instance previously returned from EOS_Platform_Create.
 *
 * This function should only be called once per instance returned by EOS_Platform_Create. Undefined behavior will result in calling it with a single instance more than once.
 * Typically only a single platform instance needs to be created during the lifetime of a game.
 * You should release each platform instance before calling the EOS_Shutdown function.
 }
procedure EOS_Platform_Release(Handle: EOS_HPlatform); cdecl; external EOSLIB;

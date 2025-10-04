// from "SDL_system.h"

(* Platform specific functions for Windows *)
{$IF DEFINED(WIN32) OR DEFINED(WIN64)} 

  {**
   *  \brief Set a function that is called for every windows message, before TranslateMessage()
   *}
Type
  TSDL_WindowsMessageHook = Procedure(userdata, hWnd: Pointer; mesage: UInt32; wParam: UInt64; lParam: SInt64); cdecl;

Procedure SDL_SetWindowsMessageHook(callback: TSDL_WindowsMessageHook; userdata: Pointer); cdecl;
  external SDL_LibName;

  {* Returns the D3D9 adapter index that matches the specified display index.
   * This adapter index can be passed to IDirect3D9::CreateDevice and controls
   * on which monitor a full screen application will appear.
   *}
Function SDL_Direct3D9GetAdapterIndex(displayIndex:SInt32):SInt32;
   cdecl; external SDL_LibName;

  {* Returns the D3D device associated with a renderer, or NULL if it's not a D3D renderer.
   * Once you are done using the device, you should release it to avoid a resource leak.
   *}
Type PIDirect3DDevice9 = Pointer;
Function SDL_RenderGetD3D9Device(renderer:PSDL_Renderer):PIDirect3DDevice9;
   cdecl; external SDL_LibName;

{* Returns the DXGI Adapter and Output indices for the specified display index. 
 * These can be passed to EnumAdapters and EnumOutputs respectively to get the objects
 *  required to create a DX10 or DX11 device and swap chain.
 *}
function SDL_DXGIGetOutputInfo(displayIndex :SInt32; adapterIndex, outputIndex :PSInt32): TSDL_Bool;
   cdecl; external SDL_LibName;

{$IFEND}


(* Platform specific functions for WinRT *)
{$IFDEF __WINRT__}

  {**
   *  \brief WinRT / Windows Phone path types
   *}
Type
  TSDL_WinRT_Path = (

    {** \brief The installed app's root directory.
        Files here are likely to be read-only. *}
    SDL_WINRT_PATH_INSTALLED_LOCATION = 0,

    {** \brief The app's local data store.  Files may be written here *}
    SDL_WINRT_PATH_LOCAL_FOLDER = 1,

    {** \brief The app's roaming data store.  Unsupported on Windows Phone.
        Files written here may be copied to other machines via a network
        connection.
    *}
    SDL_WINRT_PATH_ROAMING_FOLDER = 2,

    {** \brief The app's temporary data store.  Unsupported on Windows Phone.
        Files written here may be deleted at any time. *}
    SDL_WINRT_PATH_TEMP_FOLDER = 3

  );


  {**
   *  \brief Retrieves a WinRT defined path on the local file system
   *
   *  \note Documentation on most app-specific path types on WinRT
   *      can be found on MSDN, at the URL:
   *      http://msdn.microsoft.com/en-us/library/windows/apps/hh464917.aspx
   *
   *  \param pathType The type of path to retrieve.
   *  \ret A UCS-2 string (16-bit, wide-char) containing the path, or NULL
   *      if the path is not available for any reason.  Not all paths are
   *      available on all versions of Windows.  This is especially true on
   *      Windows Phone.  Check the documentation for the given
   *      SDL_WinRT_Path for more information on which path types are
   *      supported where.
   *}
Function SDL_WinRTGetFSPathUNICODE(pathType :TSDL_WinRT_Path):PWideChar;
   cdecl; external SDL_LibName;


  {**
   *  \brief Retrieves a WinRT defined path on the local file system
   *
   *  \note Documentation on most app-specific path types on WinRT
   *      can be found on MSDN, at the URL:
   *      http://msdn.microsoft.com/en-us/library/windows/apps/hh464917.aspx
   *
   *  \param pathType The type of path to retrieve.
   *  \ret A UTF-8 string (8-bit, multi-byte) containing the path, or NULL
   *      if the path is not available for any reason.  Not all paths are
   *      available on all versions of Windows.  This is especially true on
   *      Windows Phone.  Check the documentation for the given
   *      SDL_WinRT_Path for more information on which path types are
   *      supported where.
   *}
Function SDL_WinRTGetFSPathUTF8(pathType :TSDL_WinRT_Path):PChar;
   cdecl; external SDL_LibName;

{$ENDIF}

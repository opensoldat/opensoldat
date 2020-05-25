/**
 * @file entry point for Windows
 * @author Oliver Kuckertz <oliver.kuckertz@softwific.com>
 */

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

BOOL APIENTRY DllMain(HMODULE hModule, DWORD dwReasonForCall, LPVOID lpReserved) {
    UNREFERENCED_PARAMETER(lpReserved);
    if (dwReasonForCall == DLL_PROCESS_ATTACH) {
        DisableThreadLibraryCalls(hModule);
    }
    return TRUE;
}

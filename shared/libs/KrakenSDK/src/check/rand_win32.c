/**
 * @file OS RNG abstraction implementation for Windows
 * @author Oliver Kuckertz <oliver.kuckertz@softwific.com>
 */

#include "rand.h"
#include <stdlib.h>

// https://docs.microsoft.com/en-us/windows/desktop/api/ntsecapi/nf-ntsecapi-rtlgenrandom
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#pragma comment(lib, "advapi32.lib")
#define RtlGenRandom SystemFunction036
BOOLEAN WINAPI RtlGenRandom(PVOID RandomBuffer, ULONG RandomBufferLength);

void randBytes(void* dst, size_t size) {
    if (!RtlGenRandom(dst, (ULONG)size)) {
        abort();
    }
}

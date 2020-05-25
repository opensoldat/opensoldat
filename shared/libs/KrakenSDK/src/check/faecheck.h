/**
 * @file reference implementation for generating fae challenges and decrypting/validating responses
 * @author Oliver Kuckertz <oliver.kuckertz@softwific.com>
 */

#pragma once

#if defined(FAECHECK_EXPORT) && defined(__GNUC__)
    #define FAECHECK_API __attribute__((visibility("default")))
#else
    #define FAECHECK_API
#endif

#ifdef _MSC_VER
    #define FAECHECK_STATIC_ASSERT static_assert
#else
    #define FAECHECK_STATIC_ASSERT _Static_assert
#endif

#define FAE_SECRETDATA_SIZE 32
#define FAE_DERIVEDKEY_SIZE 32

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#pragma pack(push, 1)
typedef struct FaeChallenge {
    uint8_t  EphKey[32];
    uint8_t  EncryptedSecret[FAE_SECRETDATA_SIZE];
} FAE_CHALLENGE;
typedef struct FaeResponse {
    uint8_t  GameKey[32];
    char     GameVersion[32];
    int64_t  ValidUntil; // ticket expiration UNIX timestamp
    uint32_t FaeBuild;
    uint32_t Status;
    char     StatusStr[32];
} FAE_RESPONSE;
typedef struct FaeResponseBox {
    uint32_t OuterStatus;
    uint32_t Reserved;
    uint8_t  MAC[16];
    uint8_t  EncryptedResponse[sizeof(struct FaeResponse)];
} FAE_RESPONSE_BOX;
#pragma pack(pop)

typedef enum FaeCheckError {
    FAECHECK_OK,
    FAECHECK_ERR_INVALID,
    FAECHECK_ERR_CLOCK,
} FAECHECK_ERROR;

FAECHECK_STATIC_ASSERT(sizeof(struct FaeChallenge) == 64, "invalid FaeChallenge size");
FAECHECK_STATIC_ASSERT(sizeof(struct FaeResponseBox) == 136, "invalid FaeResponse size");

FAECHECK_API void FaeInitSecret(uint8_t secret[FAE_SECRETDATA_SIZE]);
FAECHECK_API void FaeInitChallenge(uint8_t const secret[FAE_SECRETDATA_SIZE], FAE_CHALLENGE* challenge);
FAECHECK_API void FaeDeriveKey(uint8_t const secret[FAE_SECRETDATA_SIZE], uint8_t out[FAE_DERIVEDKEY_SIZE]);
FAECHECK_API FAECHECK_ERROR FaeCheck(uint8_t const secret[FAE_SECRETDATA_SIZE], FAE_RESPONSE_BOX const* box, FAE_RESPONSE* out);
FAECHECK_API void FaeLock(uint8_t mac[16], uint8_t* cipher_text, const uint8_t key[32],
    const uint8_t nonce[24], const uint8_t *ad, size_t ad_size, const uint8_t *plain_text, size_t text_size);
FAECHECK_API bool FaeUnlock(uint8_t* plain_text, const uint8_t key[32], const uint8_t nonce[24],
    const uint8_t mac[16], const uint8_t *ad, size_t ad_size, const uint8_t *cipher_text, size_t text_size);

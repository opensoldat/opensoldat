/**
 * @file reference implementation for generating fae challenges and decrypting/validating responses
 * @author Oliver Kuckertz <oliver.kuckertz@softwific.com>
 */

#include "faecheck.h"
#include "rand.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <monocypher.h>

static uint8_t const FaeClientKxkPub[32] = {
    0xd2,0x97,0x75,0xfa,0x9b,0x9e,0x76,0x52,0xf7,0x87,0x8a,0xa3,0xd6,0x96,0xe0,0x93,
    0x76,0x6a,0x33,0x4c,0x7f,0xfc,0x4b,0xf6,0xb5,0x6a,0x69,0xcb,0x42,0x88,0x67,0x51,
};

FAECHECK_API void FaeInitSecret(uint8_t secret[FAE_SECRETDATA_SIZE]) {
    randBytes(secret, FAE_SECRETDATA_SIZE);
}

FAECHECK_API void FaeInitChallenge(uint8_t const secret[FAE_SECRETDATA_SIZE], FAE_CHALLENGE* challenge) {
    uint8_t ephSec[32], key[32];
    uint8_t const nonce0[24] = {0};
    crypto_chacha_ctx ctx;
    randBytes(ephSec, sizeof(ephSec));
    crypto_x25519_public_key(challenge->EphKey, ephSec);
    crypto_key_exchange(key, ephSec, FaeClientKxkPub);
    crypto_chacha20_x_init(&ctx, key, nonce0);
    crypto_chacha20_encrypt(&ctx, challenge->EncryptedSecret, secret, FAE_SECRETDATA_SIZE);
    crypto_wipe(ephSec, sizeof(ephSec));
    crypto_wipe(&ctx, sizeof(ctx));
}

FAECHECK_API void FaeDeriveKey(uint8_t const secret[FAE_SECRETDATA_SIZE], uint8_t out[FAE_DERIVEDKEY_SIZE]) {
    // NOTE That the key here is not relevant for security. Any one-way hash would work, even
    // one without salt or key. We derive out from secret so that Fae does not have to reveal
    // the secret to the client, which would enable an attacker to rewrite the Fae response.
    char const* key = "Toothless, Plasma Blast!";
    size_t const keySize = 24;
    crypto_blake2b_general(out, FAE_DERIVEDKEY_SIZE, (uint8_t const*)key, keySize, secret, FAE_SECRETDATA_SIZE);
}

FAECHECK_API FAECHECK_ERROR FaeCheck(uint8_t const secret[FAE_SECRETDATA_SIZE], FAE_RESPONSE_BOX const* box, FAE_RESPONSE* out) {
    uint8_t const nonce1[24] = {1};
    size_t const len = sizeof(struct FaeResponse);

    // decrypt and validate
    if (crypto_unlock((uint8_t*)out, secret, nonce1, box->MAC, box->EncryptedResponse, len) != 0) {
        return FAECHECK_ERR_INVALID;
    }

    // check format
    if (out->GameVersion[sizeof(out->GameKey) - 1] != 0) {
        return FAECHECK_ERR_INVALID;
    }
    if (out->StatusStr[sizeof(out->StatusStr) - 1] != 0) {
        return FAECHECK_ERR_INVALID;
    }

    // check client ticket expiration time
    if (out->ValidUntil < time(NULL)) {
        return FAECHECK_ERR_CLOCK;
    }

    return FAECHECK_OK;
}

FAECHECK_API void FaeLock(uint8_t mac[16], uint8_t* cipher_text, const uint8_t key[32],
    const uint8_t nonce[24], const uint8_t *ad, size_t ad_size, const uint8_t *plain_text, size_t text_size
) {
    crypto_lock_aead(mac, cipher_text, key, nonce, ad, ad_size, plain_text, text_size);
}

FAECHECK_API bool FaeUnlock(uint8_t* plain_text, const uint8_t key[32], const uint8_t nonce[24],
    const uint8_t mac[16], const uint8_t *ad, size_t ad_size, const uint8_t *cipher_text, size_t text_size
) {
    return crypto_unlock_aead(plain_text, key, nonce, mac, ad, ad_size, cipher_text, text_size) == 0;
}

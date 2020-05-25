#include "monocypher.h"

/////////////////
/// Utilities ///
/////////////////

// By default, EdDSA signatures use blake2b.  SHA-512 is provided as an
// option for full ed25519 compatibility. To use with SHA-512, compile
// with option -DED25519_SHA512 and provide the "sha512" header.
#ifdef ED25519_SHA512
    #define HASH crypto_sha512
#else
    #define HASH crypto_blake2b
#endif
#define COMBINE1(x, y) x ## y
#define COMBINE2(x, y) COMBINE1(x, y)
#define HASH_CTX    COMBINE2(HASH, _ctx)
#define HASH_INIT   COMBINE2(HASH, _init)
#define HASH_UPDATE COMBINE2(HASH, _update)
#define HASH_FINAL  COMBINE2(HASH, _final)

#define FOR(i, start, end)   for (size_t (i) = (start); (i) < (end); (i)++)
#define WIPE_CTX(ctx)        crypto_wipe(ctx   , sizeof(*(ctx)))
#define WIPE_BUFFER(buffer)  crypto_wipe(buffer, sizeof(buffer))
#define MIN(a, b)            ((a) <= (b) ? (a) : (b))
#define ALIGN(x, block_size) ((~(x) + 1) & ((block_size) - 1))
typedef int8_t   i8;
typedef uint8_t  u8;
typedef uint32_t u32;
typedef int32_t  i32;
typedef int64_t  i64;
typedef uint64_t u64;

static const u8 zero[128] = {0};

static u32 load24_le(const u8 s[3])
{
    return (u32)s[0]
        | ((u32)s[1] <<  8)
        | ((u32)s[2] << 16);
}

static u32 load32_le(const u8 s[4])
{
    return (u32)s[0]
        | ((u32)s[1] <<  8)
        | ((u32)s[2] << 16)
        | ((u32)s[3] << 24);
}

static u64 load64_le(const u8 s[8])
{
    return load32_le(s) | ((u64)load32_le(s+4) << 32);
}

static void store32_le(u8 out[4], u32 in)
{
    out[0] =  in        & 0xff;
    out[1] = (in >>  8) & 0xff;
    out[2] = (in >> 16) & 0xff;
    out[3] = (in >> 24) & 0xff;
}

static void store64_le(u8 out[8], u64 in)
{
    store32_le(out    , (u32)in );
    store32_le(out + 4, in >> 32);
}

static u64 rotr64(u64 x, u64 n) { return (x >> n) ^ (x << (64 - n)); }
static u32 rotl32(u32 x, u32 n) { return (x << n) ^ (x >> (32 - n)); }

static int neq0(u64 diff)
{   // constant time comparison to zero
    // return diff != 0 ? -1 : 0
    u64 half = (diff >> 32) | ((u32)diff);
    return (1 & ((half - 1) >> 32)) - 1;
}

static u64 x16(const u8 a[16], const u8 b[16])
{
    return (load64_le(a + 0) ^ load64_le(b + 0))
        |  (load64_le(a + 8) ^ load64_le(b + 8));
}
static u64 x32(const u8 a[16],const u8 b[16]){return x16(a,b)| x16(a+16, b+16);}
static u64 x64(const u8 a[64],const u8 b[64]){return x32(a,b)| x32(a+32, b+32);}
int crypto_verify16(const u8 a[16], const u8 b[16]){ return neq0(x16(a, b)); }
int crypto_verify32(const u8 a[32], const u8 b[32]){ return neq0(x32(a, b)); }
int crypto_verify64(const u8 a[64], const u8 b[64]){ return neq0(x64(a, b)); }

static int zerocmp32(const u8 p[32])
{
    return crypto_verify32(p, zero);
}

void crypto_wipe(void *secret, size_t size)
{
    volatile u8 *v_secret = (u8*)secret;
    FOR (i, 0, size) {
        v_secret[i] = 0;
    }
}

/////////////////
/// Chacha 20 ///
/////////////////
#define QUARTERROUND(a, b, c, d)     \
    a += b;  d = rotl32(d ^ a, 16);  \
    c += d;  b = rotl32(b ^ c, 12);  \
    a += b;  d = rotl32(d ^ a,  8);  \
    c += d;  b = rotl32(b ^ c,  7)

static void chacha20_rounds(u32 out[16], const u32 in[16])
{
    // The temporary variables make Chacha20 10% faster.
    u32 t0  = in[ 0];  u32 t1  = in[ 1];  u32 t2  = in[ 2];  u32 t3  = in[ 3];
    u32 t4  = in[ 4];  u32 t5  = in[ 5];  u32 t6  = in[ 6];  u32 t7  = in[ 7];
    u32 t8  = in[ 8];  u32 t9  = in[ 9];  u32 t10 = in[10];  u32 t11 = in[11];
    u32 t12 = in[12];  u32 t13 = in[13];  u32 t14 = in[14];  u32 t15 = in[15];

    FOR (i, 0, 10) { // 20 rounds, 2 rounds per loop.
        QUARTERROUND(t0, t4, t8 , t12); // column 0
        QUARTERROUND(t1, t5, t9 , t13); // column 1
        QUARTERROUND(t2, t6, t10, t14); // column 2
        QUARTERROUND(t3, t7, t11, t15); // column 3
        QUARTERROUND(t0, t5, t10, t15); // diagonal 0
        QUARTERROUND(t1, t6, t11, t12); // diagonal 1
        QUARTERROUND(t2, t7, t8 , t13); // diagonal 2
        QUARTERROUND(t3, t4, t9 , t14); // diagonal 3
    }
    out[ 0] = t0;   out[ 1] = t1;   out[ 2] = t2;   out[ 3] = t3;
    out[ 4] = t4;   out[ 5] = t5;   out[ 6] = t6;   out[ 7] = t7;
    out[ 8] = t8;   out[ 9] = t9;   out[10] = t10;  out[11] = t11;
    out[12] = t12;  out[13] = t13;  out[14] = t14;  out[15] = t15;
}

static void chacha20_init_key(crypto_chacha_ctx *ctx, const u8 key[32])
{
    // constant
    ctx->input[0] = load32_le((u8*)"expa");
    ctx->input[1] = load32_le((u8*)"nd 3");
    ctx->input[2] = load32_le((u8*)"2-by");
    ctx->input[3] = load32_le((u8*)"te k");
    // key
    FOR (i, 0, 8) {
        ctx->input[i+4] = load32_le(key + i*4);
    }
}

static u8 chacha20_pool_byte(crypto_chacha_ctx *ctx)
{
    u32 pool_word = ctx->pool[ctx->pool_idx >> 2];
    u8  pool_byte = pool_word >> (8*(ctx->pool_idx & 3));
    ctx->pool_idx++;
    return pool_byte;
}

// Fill the pool if needed, update the counters
static void chacha20_refill_pool(crypto_chacha_ctx *ctx)
{
    chacha20_rounds(ctx->pool, ctx->input);
    FOR (j, 0, 16) {
        ctx->pool[j] += ctx->input[j];
    }
    ctx->pool_idx = 0;
    ctx->input[12]++;
    if (ctx->input[12] == 0) {
        ctx->input[13]++;
    }
}

void crypto_chacha20_H(u8 out[32], const u8 key[32], const u8 in[16])
{
    crypto_chacha_ctx ctx;
    chacha20_init_key(&ctx, key);
    FOR (i, 0, 4) {
        ctx.input[i+12] = load32_le(in + i*4);
    }
    u32 buffer[16];
    chacha20_rounds(buffer, ctx.input);
    // prevents reversal of the rounds by revealing only half of the buffer.
    FOR (i, 0, 4) {
        store32_le(out      + i*4, buffer[i     ]); // constant
        store32_le(out + 16 + i*4, buffer[i + 12]); // counter and nonce
    }
    WIPE_CTX(&ctx);
    WIPE_BUFFER(buffer);
}

static void chacha20_encrypt(crypto_chacha_ctx *ctx,
                             u8                *cipher_text,
                             const u8          *plain_text,
                             size_t             text_size)
{
    FOR (i, 0, text_size) {
        if (ctx->pool_idx == 64) {
            chacha20_refill_pool(ctx);
        }
        u8 plain = 0;
        if (plain_text != 0) {
            plain = *plain_text;
            plain_text++;
        }
        *cipher_text = chacha20_pool_byte(ctx) ^ plain;
        cipher_text++;
    }
}

void crypto_chacha20_init(crypto_chacha_ctx *ctx,
                          const u8           key[32],
                          const u8           nonce[8])
{
    chacha20_init_key      (ctx, key);     // key
    crypto_chacha20_set_ctr(ctx, 0  );     // counter
    ctx->input[14] = load32_le(nonce + 0); // nonce
    ctx->input[15] = load32_le(nonce + 4); // nonce
}

void crypto_chacha20_x_init(crypto_chacha_ctx *ctx,
                            const u8           key[32],
                            const u8           nonce[24])
{
    u8 derived_key[32];
    crypto_chacha20_H(derived_key, key, nonce);
    crypto_chacha20_init(ctx, derived_key, nonce + 16);
    WIPE_BUFFER(derived_key);
}

void crypto_chacha20_set_ctr(crypto_chacha_ctx *ctx, u64 ctr)
{
    ctx->input[12] = ctr & 0xffffffff;
    ctx->input[13] = ctr >> 32;
    ctx->pool_idx  = 64;  // The random pool (re)starts empty
}

void crypto_chacha20_encrypt(crypto_chacha_ctx *ctx,
                             u8                *cipher_text,
                             const u8          *plain_text,
                             size_t             text_size)
{
    // Align ourselves with block boundaries
    size_t align = MIN(ALIGN(ctx->pool_idx, 64), text_size);
    chacha20_encrypt(ctx, cipher_text, plain_text, align);
    if (plain_text != 0) {
        plain_text += align;
    }
    cipher_text += align;
    text_size   -= align;

    // Process the message block by block
    FOR (i, 0, text_size >> 6) {  // number of blocks
        chacha20_refill_pool(ctx);
        if (plain_text != 0) {
            FOR (j, 0, 16) {
                u32 plain = load32_le(plain_text);
                store32_le(cipher_text, ctx->pool[j] ^ plain);
                plain_text  += 4;
                cipher_text += 4;
            }
        } else {
            FOR (j, 0, 16) {
                store32_le(cipher_text, ctx->pool[j]);
                cipher_text += 4;
            }
        }
        ctx->pool_idx = 64;
    }
    text_size &= 63;

    // remaining bytes
    chacha20_encrypt(ctx, cipher_text, plain_text, text_size);
}

void crypto_chacha20_stream(crypto_chacha_ctx *ctx,
                            uint8_t *stream, size_t size)
{
    crypto_chacha20_encrypt(ctx, stream, 0, size);
}


/////////////////
/// Poly 1305 ///
/////////////////

// h = (h + c) * r
// preconditions:
//   ctx->h <= 4_ffffffff_ffffffff_ffffffff_ffffffff
//   ctx->c <= 1_ffffffff_ffffffff_ffffffff_ffffffff
//   ctx->r <=   0ffffffc_0ffffffc_0ffffffc_0fffffff
// Postcondition:
//   ctx->h <= 4_ffffffff_ffffffff_ffffffff_ffffffff
static void poly_block(crypto_poly1305_ctx *ctx)
{
    // s = h + c, without carry propagation
    const u64 s0 = ctx->h[0] + (u64)ctx->c[0]; // s0 <= 1_fffffffe
    const u64 s1 = ctx->h[1] + (u64)ctx->c[1]; // s1 <= 1_fffffffe
    const u64 s2 = ctx->h[2] + (u64)ctx->c[2]; // s2 <= 1_fffffffe
    const u64 s3 = ctx->h[3] + (u64)ctx->c[3]; // s3 <= 1_fffffffe
    const u32 s4 = ctx->h[4] +      ctx->c[4]; // s4 <=          5

    // Local all the things!
    const u32 r0 = ctx->r[0];       // r0  <= 0fffffff
    const u32 r1 = ctx->r[1];       // r1  <= 0ffffffc
    const u32 r2 = ctx->r[2];       // r2  <= 0ffffffc
    const u32 r3 = ctx->r[3];       // r3  <= 0ffffffc
    const u32 rr0 = (r0 >> 2) * 5;  // rr0 <= 13fffffb // lose 2 bits...
    const u32 rr1 = (r1 >> 2) + r1; // rr1 <= 13fffffb // rr1 == (r1 >> 2) * 5
    const u32 rr2 = (r2 >> 2) + r2; // rr2 <= 13fffffb // rr1 == (r2 >> 2) * 5
    const u32 rr3 = (r3 >> 2) + r3; // rr3 <= 13fffffb // rr1 == (r3 >> 2) * 5

    // (h + c) * r, without carry propagation
    const u64 x0 = s0*r0 + s1*rr3 + s2*rr2 + s3*rr1 +s4*rr0;//<=97ffffe007fffff8
    const u64 x1 = s0*r1 + s1*r0  + s2*rr3 + s3*rr2 +s4*rr1;//<=8fffffe20ffffff6
    const u64 x2 = s0*r2 + s1*r1  + s2*r0  + s3*rr3 +s4*rr2;//<=87ffffe417fffff4
    const u64 x3 = s0*r3 + s1*r2  + s2*r1  + s3*r0  +s4*rr3;//<=7fffffe61ffffff2
    const u32 x4 = s4 * (r0 & 3); // ...recover 2 bits      //<=               f

    // partial reduction modulo 2^130 - 5
    const u32 u5 = x4 + (x3 >> 32); // u5 <= 7ffffff5
    const u64 u0 = (u5 >>  2) * 5 + (x0 & 0xffffffff);
    const u64 u1 = (u0 >> 32)     + (x1 & 0xffffffff) + (x0 >> 32);
    const u64 u2 = (u1 >> 32)     + (x2 & 0xffffffff) + (x1 >> 32);
    const u64 u3 = (u2 >> 32)     + (x3 & 0xffffffff) + (x2 >> 32);
    const u64 u4 = (u3 >> 32)     + (u5 & 3);

    // Update the hash
    ctx->h[0] = u0 & 0xffffffff; // u0 <= 1_9ffffff0
    ctx->h[1] = u1 & 0xffffffff; // u1 <= 1_97ffffe0
    ctx->h[2] = u2 & 0xffffffff; // u2 <= 1_8fffffe2
    ctx->h[3] = u3 & 0xffffffff; // u3 <= 1_87ffffe4
    ctx->h[4] = (u32)u4;         // u4 <=          4
}

// (re-)initializes the input counter and input buffer
static void poly_clear_c(crypto_poly1305_ctx *ctx)
{
    ctx->c[0]  = 0;
    ctx->c[1]  = 0;
    ctx->c[2]  = 0;
    ctx->c[3]  = 0;
    ctx->c_idx = 0;
}

static void poly_take_input(crypto_poly1305_ctx *ctx, u8 input)
{
    size_t word = ctx->c_idx >> 2;
    size_t byte = ctx->c_idx & 3;
    ctx->c[word] |= (u32)input << (byte * 8);
    ctx->c_idx++;
}

static void poly_update(crypto_poly1305_ctx *ctx,
                        const u8 *message, size_t message_size)
{
    FOR (i, 0, message_size) {
        poly_take_input(ctx, message[i]);
        if (ctx->c_idx == 16) {
            poly_block(ctx);
            poly_clear_c(ctx);
        }
    }
}

void crypto_poly1305_init(crypto_poly1305_ctx *ctx, const u8 key[32])
{
    // Initial hash is zero
    FOR (i, 0, 5) {
        ctx->h[i] = 0;
    }
    // add 2^130 to every input block
    ctx->c[4] = 1;
    poly_clear_c(ctx);
    // load r and pad (r has some of its bits cleared)
    FOR (i, 0, 1) { ctx->r  [0] = load32_le(key           ) & 0x0fffffff; }
    FOR (i, 1, 4) { ctx->r  [i] = load32_le(key + i*4     ) & 0x0ffffffc; }
    FOR (i, 0, 4) { ctx->pad[i] = load32_le(key + i*4 + 16);              }
}

void crypto_poly1305_update(crypto_poly1305_ctx *ctx,
                            const u8 *message, size_t message_size)
{
    // Align ourselves with block boundaries
    size_t align = MIN(ALIGN(ctx->c_idx, 16), message_size);
    poly_update(ctx, message, align);
    message      += align;
    message_size -= align;

    // Process the message block by block
    size_t nb_blocks = message_size >> 4;
    FOR (i, 0, nb_blocks) {
        ctx->c[0] = load32_le(message +  0);
        ctx->c[1] = load32_le(message +  4);
        ctx->c[2] = load32_le(message +  8);
        ctx->c[3] = load32_le(message + 12);
        poly_block(ctx);
        message += 16;
    }
    if (nb_blocks > 0) {
        poly_clear_c(ctx);
    }
    message_size &= 15;

    // remaining bytes
    poly_update(ctx, message, message_size);
}

void crypto_poly1305_final(crypto_poly1305_ctx *ctx, u8 mac[16])
{
    // Process the last block (if any)
    if (ctx->c_idx != 0) {
        // move the final 1 according to remaining input length
        // (We may add less than 2^130 to the last input block)
        ctx->c[4] = 0;
        poly_take_input(ctx, 1);
        // one last hash update
        poly_block(ctx);
    }

    // check if we should subtract 2^130-5 by performing the
    // corresponding carry propagation.
    const u64 u0 = (u64)5     + ctx->h[0]; // <= 1_00000004
    const u64 u1 = (u0 >> 32) + ctx->h[1]; // <= 1_00000000
    const u64 u2 = (u1 >> 32) + ctx->h[2]; // <= 1_00000000
    const u64 u3 = (u2 >> 32) + ctx->h[3]; // <= 1_00000000
    const u64 u4 = (u3 >> 32) + ctx->h[4]; // <=          5
    // u4 indicates how many times we should subtract 2^130-5 (0 or 1)

    // h + pad, minus 2^130-5 if u4 exceeds 3
    const u64 uu0 = (u4 >> 2) * 5 + ctx->h[0] + ctx->pad[0]; // <= 2_00000003
    const u64 uu1 = (uu0 >> 32)   + ctx->h[1] + ctx->pad[1]; // <= 2_00000000
    const u64 uu2 = (uu1 >> 32)   + ctx->h[2] + ctx->pad[2]; // <= 2_00000000
    const u64 uu3 = (uu2 >> 32)   + ctx->h[3] + ctx->pad[3]; // <= 2_00000000

    store32_le(mac     , (u32)uu0);
    store32_le(mac +  4, (u32)uu1);
    store32_le(mac +  8, (u32)uu2);
    store32_le(mac + 12, (u32)uu3);

    WIPE_CTX(ctx);
}

void crypto_poly1305(u8     mac[16],  const u8 *message,
                     size_t message_size, const u8  key[32])
{
    crypto_poly1305_ctx ctx;
    crypto_poly1305_init  (&ctx, key);
    crypto_poly1305_update(&ctx, message, message_size);
    crypto_poly1305_final (&ctx, mac);
}

////////////////
/// Blake2 b ///
////////////////
static const u64 iv[8] = {
    0x6a09e667f3bcc908, 0xbb67ae8584caa73b,
    0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
    0x510e527fade682d1, 0x9b05688c2b3e6c1f,
    0x1f83d9abfb41bd6b, 0x5be0cd19137e2179,
};

// increment the input offset
static void blake2b_incr(crypto_blake2b_ctx *ctx)
{
    u64   *x = ctx->input_offset;
    size_t y = ctx->input_idx;
    x[0] += y;
    if (x[0] < y) {
        x[1]++;
    }
}

static void blake2b_compress(crypto_blake2b_ctx *ctx, int is_last_block)
{
    static const u8 sigma[12][16] = {
        {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 },
        { 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 },
        { 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 },
        {  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 },
        {  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 },
        {  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 },
        { 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 },
        { 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 },
        {  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 },
        { 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13,  0 },
        {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 },
        { 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 },
    };

    // init work vector
    u64 v0 = ctx->hash[0];  u64 v8  = iv[0];
    u64 v1 = ctx->hash[1];  u64 v9  = iv[1];
    u64 v2 = ctx->hash[2];  u64 v10 = iv[2];
    u64 v3 = ctx->hash[3];  u64 v11 = iv[3];
    u64 v4 = ctx->hash[4];  u64 v12 = iv[4] ^ ctx->input_offset[0];
    u64 v5 = ctx->hash[5];  u64 v13 = iv[5] ^ ctx->input_offset[1];
    u64 v6 = ctx->hash[6];  u64 v14 = iv[6] ^ is_last_block;
    u64 v7 = ctx->hash[7];  u64 v15 = iv[7];

    // mangle work vector
    uint64_t *input = ctx->input;
#define BLAKE2_G(v, a, b, c, d, x, y)                  \
    v##a += v##b + x;  v##d = rotr64(v##d ^ v##a, 32); \
    v##c += v##d;      v##b = rotr64(v##b ^ v##c, 24); \
    v##a += v##b + y;  v##d = rotr64(v##d ^ v##a, 16); \
    v##c += v##d;      v##b = rotr64(v##b ^ v##c, 63);
#define BLAKE2_ROUND(i)                                                 \
    BLAKE2_G(v, 0, 4,  8, 12, input[sigma[i][ 0]], input[sigma[i][ 1]]);\
    BLAKE2_G(v, 1, 5,  9, 13, input[sigma[i][ 2]], input[sigma[i][ 3]]);\
    BLAKE2_G(v, 2, 6, 10, 14, input[sigma[i][ 4]], input[sigma[i][ 5]]);\
    BLAKE2_G(v, 3, 7, 11, 15, input[sigma[i][ 6]], input[sigma[i][ 7]]);\
    BLAKE2_G(v, 0, 5, 10, 15, input[sigma[i][ 8]], input[sigma[i][ 9]]);\
    BLAKE2_G(v, 1, 6, 11, 12, input[sigma[i][10]], input[sigma[i][11]]);\
    BLAKE2_G(v, 2, 7,  8, 13, input[sigma[i][12]], input[sigma[i][13]]);\
    BLAKE2_G(v, 3, 4,  9, 14, input[sigma[i][14]], input[sigma[i][15]])

#ifdef BLAKE2_NO_UNROLLING
    FOR (i, 0, 12) {
        BLAKE2_ROUND(i);
    }
#else
    BLAKE2_ROUND(0);  BLAKE2_ROUND(1);  BLAKE2_ROUND(2);  BLAKE2_ROUND(3);
    BLAKE2_ROUND(4);  BLAKE2_ROUND(5);  BLAKE2_ROUND(6);  BLAKE2_ROUND(7);
    BLAKE2_ROUND(8);  BLAKE2_ROUND(9);  BLAKE2_ROUND(0);  BLAKE2_ROUND(1);
#endif

    // update hash
    ctx->hash[0] ^= v0 ^ v8;
    ctx->hash[1] ^= v1 ^ v9;
    ctx->hash[2] ^= v2 ^ v10;
    ctx->hash[3] ^= v3 ^ v11;
    ctx->hash[4] ^= v4 ^ v12;
    ctx->hash[5] ^= v5 ^ v13;
    ctx->hash[6] ^= v6 ^ v14;
    ctx->hash[7] ^= v7 ^ v15;
}

static void blake2b_set_input(crypto_blake2b_ctx *ctx, u8 input, size_t index)
{
    if (index == 0) {
        FOR (i, 0, 16) {
            ctx->input[i] = 0;
        }
    }
    size_t word = index >> 3;
    size_t byte = index & 7;
    ctx->input[word] |= (u64)input << (byte << 3);

}

static void blake2b_end_block(crypto_blake2b_ctx *ctx)
{
    if (ctx->input_idx == 128) {  // If buffer is full,
        blake2b_incr(ctx);        // update the input offset
        blake2b_compress(ctx, 0); // and compress the (not last) block
        ctx->input_idx = 0;
    }
}

static void blake2b_update(crypto_blake2b_ctx *ctx,
                           const u8 *message, size_t message_size)
{
    FOR (i, 0, message_size) {
        blake2b_end_block(ctx);
        blake2b_set_input(ctx, message[i], ctx->input_idx);
        ctx->input_idx++;
    }
}

void crypto_blake2b_general_init(crypto_blake2b_ctx *ctx, size_t hash_size,
                                 const u8           *key, size_t key_size)
{
    // initial hash
    FOR (i, 0, 8) {
        ctx->hash[i] = iv[i];
    }
    ctx->hash[0] ^= 0x01010000 ^ (key_size << 8) ^ hash_size;

    ctx->input_offset[0] = 0;         // begining of the input, no offset
    ctx->input_offset[1] = 0;         // begining of the input, no offset
    ctx->hash_size       = hash_size; // remember the hash size we want
    ctx->input_idx       = 0;

    // if there is a key, the first block is that key (padded with zeroes)
    if (key_size > 0) {
        crypto_blake2b_update(ctx, key ,       key_size);
        crypto_blake2b_update(ctx, zero, 128 - key_size);
    }
}

void crypto_blake2b_init(crypto_blake2b_ctx *ctx)
{
    crypto_blake2b_general_init(ctx, 64, 0, 0);
}

void crypto_blake2b_update(crypto_blake2b_ctx *ctx,
                           const u8 *message, size_t message_size)
{
    // Align ourselves with block boundaries
    size_t align = MIN(ALIGN(ctx->input_idx, 128), message_size);
    blake2b_update(ctx, message, align);
    message      += align;
    message_size -= align;

    // Process the message block by block
    FOR (i, 0, message_size >> 7) { // number of blocks
        blake2b_end_block(ctx);
        FOR (j, 0, 16) {
            ctx->input[j] = load64_le(message + j*8);
        }
        message += 128;
        ctx->input_idx = 128;
    }
    message_size &= 127;

    // remaining bytes
    blake2b_update(ctx, message, message_size);
}

void crypto_blake2b_final(crypto_blake2b_ctx *ctx, u8 *hash)
{
    // Pad the end of the block with zeroes
    FOR (i, ctx->input_idx, 128) {
        blake2b_set_input(ctx, 0, i);
    }
    blake2b_incr(ctx);         // update the input offset
    blake2b_compress(ctx, -1); // compress the last block
    size_t nb_words = ctx->hash_size >> 3;
    FOR (i, 0, nb_words) {
        store64_le(hash + i*8, ctx->hash[i]);
    }
    FOR (i, nb_words * 8, ctx->hash_size) {
        hash[i] = (ctx->hash[i >> 3] >> (8 * (i & 7))) & 0xff;
    }
    WIPE_CTX(ctx);
}

void crypto_blake2b_general(u8       *hash   , size_t hash_size,
                            const u8 *key    , size_t key_size,
                            const u8 *message, size_t message_size)
{
    crypto_blake2b_ctx ctx;
    crypto_blake2b_general_init(&ctx, hash_size, key, key_size);
    crypto_blake2b_update(&ctx, message, message_size);
    crypto_blake2b_final(&ctx, hash);
}

void crypto_blake2b(u8 hash[64], const u8 *message, size_t message_size)
{
    crypto_blake2b_general(hash, 64, 0, 0, message, message_size);
}


////////////////
/// Argon2 i ///
////////////////
// references to R, Z, Q etc. come from the spec

// Argon2 operates on 1024 byte blocks.
typedef struct { u64 a[128]; } block;

static void wipe_block(block *b)
{
    volatile u64* a = b->a;
    FOR (i, 0, 128) {
        a[i] = 0;
    }
}

// updates a blake2 hash with a 32 bit word, little endian.
static void blake_update_32(crypto_blake2b_ctx *ctx, u32 input)
{
    u8 buf[4];
    store32_le(buf, input);
    crypto_blake2b_update(ctx, buf, 4);
    WIPE_BUFFER(buf);
}

static void load_block(block *b, const u8 bytes[1024])
{
    FOR (i, 0, 128) {
        b->a[i] = load64_le(bytes + i*8);
    }
}

static void store_block(u8 bytes[1024], const block *b)
{
    FOR (i, 0, 128) {
        store64_le(bytes + i*8, b->a[i]);
    }
}

static void copy_block(block *o,const block*in){FOR(i,0,128)o->a[i] = in->a[i];}
static void  xor_block(block *o,const block*in){FOR(i,0,128)o->a[i]^= in->a[i];}

// Hash with a virtually unlimited digest size.
// Doesn't extract more entropy than the base hash function.
// Mainly used for filling a whole kilobyte block with pseudo-random bytes.
// (One could use a stream cipher with a seed hash as the key, but
//  this would introduce another dependency —and point of failure.)
static void extended_hash(u8       *digest, u32 digest_size,
                          const u8 *input , u32 input_size)
{
    crypto_blake2b_ctx ctx;
    crypto_blake2b_general_init(&ctx, MIN(digest_size, 64), 0, 0);
    blake_update_32            (&ctx, digest_size);
    crypto_blake2b_update      (&ctx, input, input_size);
    crypto_blake2b_final       (&ctx, digest);

    if (digest_size > 64) {
        // the conversion to u64 avoids integer overflow on
        // ludicrously big hash sizes.
        u32 r   = (((u64)digest_size + 31) >> 5) - 2;
        u32 i   =  1;
        u32 in  =  0;
        u32 out = 32;
        while (i < r) {
            // Input and output overlap. This is intentional
            crypto_blake2b(digest + out, digest + in, 64);
            i   +=  1;
            in  += 32;
            out += 32;
        }
        crypto_blake2b_general(digest + out, digest_size - (32 * r),
                               0, 0, // no key
                               digest + in , 64);
    }
}

#define LSB(x) ((x) & 0xffffffff)
#define G(a, b, c, d)                                            \
    a += b + 2 * LSB(a) * LSB(b);  d ^= a;  d = rotr64(d, 32);   \
    c += d + 2 * LSB(c) * LSB(d);  b ^= c;  b = rotr64(b, 24);   \
    a += b + 2 * LSB(a) * LSB(b);  d ^= a;  d = rotr64(d, 16);   \
    c += d + 2 * LSB(c) * LSB(d);  b ^= c;  b = rotr64(b, 63)
#define ROUND(v0,  v1,  v2,  v3,  v4,  v5,  v6,  v7,    \
              v8,  v9, v10, v11, v12, v13, v14, v15)    \
    G(v0, v4,  v8, v12);  G(v1, v5,  v9, v13);          \
    G(v2, v6, v10, v14);  G(v3, v7, v11, v15);          \
    G(v0, v5, v10, v15);  G(v1, v6, v11, v12);          \
    G(v2, v7,  v8, v13);  G(v3, v4,  v9, v14)

// Core of the compression function G.  Computes Z from R in place.
static void g_rounds(block *work_block)
{
    // column rounds (work_block = Q)
    for (int i = 0; i < 128; i += 16) {
        ROUND(work_block->a[i     ], work_block->a[i +  1],
              work_block->a[i +  2], work_block->a[i +  3],
              work_block->a[i +  4], work_block->a[i +  5],
              work_block->a[i +  6], work_block->a[i +  7],
              work_block->a[i +  8], work_block->a[i +  9],
              work_block->a[i + 10], work_block->a[i + 11],
              work_block->a[i + 12], work_block->a[i + 13],
              work_block->a[i + 14], work_block->a[i + 15]);
    }
    // row rounds (work_block = Z)
    for (int i = 0; i < 16; i += 2) {
        ROUND(work_block->a[i      ], work_block->a[i +   1],
              work_block->a[i +  16], work_block->a[i +  17],
              work_block->a[i +  32], work_block->a[i +  33],
              work_block->a[i +  48], work_block->a[i +  49],
              work_block->a[i +  64], work_block->a[i +  65],
              work_block->a[i +  80], work_block->a[i +  81],
              work_block->a[i +  96], work_block->a[i +  97],
              work_block->a[i + 112], work_block->a[i + 113]);
    }
}

// The compression function G (copy version for the first pass)
static void g_copy(block *result, const block *x, const block *y, block* tmp)
{
    copy_block(tmp   , x  ); // tmp    = X
    xor_block (tmp   , y  ); // tmp    = X ^ Y = R
    copy_block(result, tmp); // result = R         (only difference with g_xor)
    g_rounds  (tmp);         // tmp    = Z
    xor_block (result, tmp); // result = R ^ Z
}

// The compression function G (xor version for subsequent passes)
static void g_xor(block *result, const block *x, const block *y, block *tmp)
{
    copy_block(tmp   , x  ); // tmp    = X
    xor_block (tmp   , y  ); // tmp    = X ^ Y = R
    xor_block (result, tmp); // result = R ^ old   (only difference with g_copy)
    g_rounds  (tmp);         // tmp    = Z
    xor_block (result, tmp); // result = R ^ old ^ Z
}

// unary version of the compression function.
// The missing argument is implied zero.
// Does the transformation in place.
static void unary_g(block *work_block)
{
    // work_block == R
    block tmp;
    copy_block(&tmp, work_block); // tmp        = R
    g_rounds(work_block);         // work_block = Z
    xor_block(work_block, &tmp);  // work_block = Z ^ R
    wipe_block(&tmp);
}

// Argon2i uses a kind of stream cipher to determine which reference
// block it will take to synthesise the next block.  This context hold
// that stream's state.  (It's very similar to Chacha20.  The block b
// is anologous to Chacha's own pool)
typedef struct {
    block b;
    u32 pass_number;
    u32 slice_number;
    u32 nb_blocks;
    u32 nb_iterations;
    u32 ctr;
    u32 offset;
} gidx_ctx;

// The block in the context will determine array indices. To avoid
// timing attacks, it only depends on public information.  No looking
// at a previous block to seed the next.  This makes offline attacks
// easier, but timing attacks are the bigger threat in many settings.
static void gidx_refresh(gidx_ctx *ctx)
{
    // seed the begining of the block...
    ctx->b.a[0] = ctx->pass_number;
    ctx->b.a[1] = 0;  // lane number (we have only one)
    ctx->b.a[2] = ctx->slice_number;
    ctx->b.a[3] = ctx->nb_blocks;
    ctx->b.a[4] = ctx->nb_iterations;
    ctx->b.a[5] = 1;  // type: Argon2i
    ctx->b.a[6] = ctx->ctr;
    FOR (i, 7, 128) { ctx->b.a[i] = 0; } // ...then zero the rest out

    // Shuffle the block thus: ctx->b = G((G(ctx->b, zero)), zero)
    // (G "square" function), to get cheap pseudo-random numbers.
    unary_g(&ctx->b);
    unary_g(&ctx->b);
}

static void gidx_init(gidx_ctx *ctx,
                      u32 pass_number, u32 slice_number,
                      u32 nb_blocks,   u32 nb_iterations)
{
    ctx->pass_number   = pass_number;
    ctx->slice_number  = slice_number;
    ctx->nb_blocks     = nb_blocks;
    ctx->nb_iterations = nb_iterations;
    ctx->ctr           = 0;

    // Offset from the begining of the segment.  For the first slice
    // of the first pass, we start at the *third* block, so the offset
    // starts at 2, not 0.
    if (pass_number != 0 || slice_number != 0) {
        ctx->offset = 0;
    } else {
        ctx->offset = 2;
        ctx->ctr++;         // Compensates for missed lazy creation
        gidx_refresh(ctx);  // at the start of gidx_next()
    }
}

static u32 gidx_next(gidx_ctx *ctx)
{
    // lazily creates the offset block we need
    if ((ctx->offset & 127) == 0) {
        ctx->ctr++;
        gidx_refresh(ctx);
    }
    u32 index  = ctx->offset & 127; // save index  for current call
    u32 offset = ctx->offset;       // save offset for current call
    ctx->offset++;                  // update offset for next call

    // Computes the area size.
    // Pass 0 : all already finished segments plus already constructed
    //          blocks in this segment
    // Pass 1+: 3 last segments plus already constructed
    //          blocks in this segment.  THE SPEC SUGGESTS OTHERWISE.
    //          I CONFORM TO THE REFERENCE IMPLEMENTATION.
    int first_pass  = ctx->pass_number == 0;
    u32 slice_size  = ctx->nb_blocks >> 2;
    u32 nb_segments = first_pass ? ctx->slice_number : 3;
    u32 area_size   = nb_segments * slice_size + offset - 1;

    // Computes the starting position of the reference area.
    // CONTRARY TO WHAT THE SPEC SUGGESTS, IT STARTS AT THE
    // NEXT SEGMENT, NOT THE NEXT BLOCK.
    u32 next_slice = ((ctx->slice_number + 1) & 3) * slice_size;
    u32 start_pos  = first_pass ? 0 : next_slice;

    // Generate offset from J1 (no need for J2, there's only one lane)
    u64 j1         = ctx->b.a[index] & 0xffffffff; // pseudo-random number
    u64 x          = (j1 * j1)       >> 32;
    u64 y          = (area_size * x) >> 32;
    u64 z          = (area_size - 1) - y;
    return (start_pos + z) % ctx->nb_blocks;
}

// Main algorithm
void crypto_argon2i_general(u8       *hash,      u32 hash_size,
                            void     *work_area, u32 nb_blocks,
                            u32 nb_iterations,
                            const u8 *password,  u32 password_size,
                            const u8 *salt,      u32 salt_size,
                            const u8 *key,       u32 key_size,
                            const u8 *ad,        u32 ad_size)
{
    // work area seen as blocks (must be suitably aligned)
    block *blocks = (block*)work_area;
    {
        crypto_blake2b_ctx ctx;
        crypto_blake2b_init(&ctx);

        blake_update_32      (&ctx, 1            ); // p: number of threads
        blake_update_32      (&ctx, hash_size    );
        blake_update_32      (&ctx, nb_blocks    );
        blake_update_32      (&ctx, nb_iterations);
        blake_update_32      (&ctx, 0x13         ); // v: version number
        blake_update_32      (&ctx, 1            ); // y: Argon2i
        blake_update_32      (&ctx,           password_size);
        crypto_blake2b_update(&ctx, password, password_size);
        blake_update_32      (&ctx,           salt_size);
        crypto_blake2b_update(&ctx, salt,     salt_size);
        blake_update_32      (&ctx,           key_size);
        crypto_blake2b_update(&ctx, key,      key_size);
        blake_update_32      (&ctx,           ad_size);
        crypto_blake2b_update(&ctx, ad,       ad_size);

        u8 initial_hash[72]; // 64 bytes plus 2 words for future hashes
        crypto_blake2b_final(&ctx, initial_hash);

        // fill first 2 blocks
        block tmp_block;
        u8    hash_area[1024];
        store32_le(initial_hash + 64, 0); // first  additional word
        store32_le(initial_hash + 68, 0); // second additional word
        extended_hash(hash_area, 1024, initial_hash, 72);
        load_block(&tmp_block, hash_area);
        copy_block(blocks, &tmp_block);

        store32_le(initial_hash + 64, 1); // slight modification
        extended_hash(hash_area, 1024, initial_hash, 72);
        load_block(&tmp_block, hash_area);
        copy_block(blocks + 1, &tmp_block);

        WIPE_BUFFER(initial_hash);
        WIPE_BUFFER(hash_area);
        wipe_block(&tmp_block);
    }

    // Actual number of blocks
    nb_blocks -= nb_blocks & 3; // round down to 4 p (p == 1 thread)
    const u32 segment_size = nb_blocks >> 2;

    // fill (then re-fill) the rest of the blocks
    block tmp;
    gidx_ctx ctx;
    FOR (pass_number, 0, nb_iterations) {
        int first_pass = pass_number == 0;

        FOR (segment, 0, 4) {
            gidx_init(&ctx, (u32)pass_number, (u32)segment,
                      nb_blocks, nb_iterations);

            // On the first segment of the first pass,
            // blocks 0 and 1 are already filled.
            // We use the offset to skip them.
            u32 start_offset  = first_pass && segment == 0 ? 2 : 0;
            u32 segment_start = (u32)segment * segment_size + start_offset;
            u32 segment_end   = ((u32)segment + 1) * segment_size;
            FOR (current_block, segment_start, segment_end) {
                u32 reference_block = gidx_next(&ctx);
                u32 previous_block  = current_block == 0
                                    ? nb_blocks - 1
                                    : (u32)current_block - 1;
                block *c = blocks + current_block;
                block *p = blocks + previous_block;
                block *r = blocks + reference_block;
                if (first_pass) { g_copy(c, p, r, &tmp); }
                else            { g_xor (c, p, r, &tmp); }
            }
        }
    }
    wipe_block(&ctx.b);
    wipe_block(&tmp);
    // hash the very last block with H' into the output hash
    u8 final_block[1024];
    store_block(final_block, blocks + (nb_blocks - 1));
    extended_hash(hash, hash_size, final_block, 1024);
    WIPE_BUFFER(final_block);

    // wipe work area
    volatile u64 *p = (u64*)work_area;
    FOR (i, 0, 128 * nb_blocks) {
        p[i] = 0;
    }
}

void crypto_argon2i(u8       *hash,      u32 hash_size,
                    void     *work_area, u32 nb_blocks,
                    u32 nb_iterations,
                    const u8 *password,  u32 password_size,
                    const u8 *salt,      u32 salt_size)
{
    crypto_argon2i_general(hash, hash_size,
                           work_area, nb_blocks, nb_iterations,
                           password, password_size,
                           salt    , salt_size,
                           0, 0, 0, 0);
}



////////////////////////////////////
/// Arithmetic modulo 2^255 - 19 ///
////////////////////////////////////
//  Taken from Supercop's ref10 implementation.
//  A bit bigger than TweetNaCl, over 4 times faster.

// field element
typedef i32 fe[10];

static void fe_0(fe h) {            FOR(i, 0, 10) h[i] = 0; }
static void fe_1(fe h) { h[0] = 1;  FOR(i, 1, 10) h[i] = 0; }

static void fe_copy(fe h,const fe f           ){FOR(i,0,10) h[i] =  f[i];      }
static void fe_neg (fe h,const fe f           ){FOR(i,0,10) h[i] = -f[i];      }
static void fe_add (fe h,const fe f,const fe g){FOR(i,0,10) h[i] = f[i] + g[i];}
static void fe_sub (fe h,const fe f,const fe g){FOR(i,0,10) h[i] = f[i] - g[i];}

static void fe_cswap(fe f, fe g, int b)
{
    FOR (i, 0, 10) {
        i32 x = (f[i] ^ g[i]) & -b;
        f[i] = f[i] ^ x;
        g[i] = g[i] ^ x;
    }
}

static void fe_ccopy(fe f, const fe g, int b)
{
    FOR (i, 0, 10) {
        i32 x = (f[i] ^ g[i]) & -b;
        f[i] = f[i] ^ x;
    }
}

#define FE_CARRY                                                        \
    i64 c0, c1, c2, c3, c4, c5, c6, c7, c8, c9;                         \
    c9 = (t9 + (i64) (1<<24)) >> 25; t0 += c9 * 19; t9 -= c9 * (1 << 25); \
    c1 = (t1 + (i64) (1<<24)) >> 25; t2 += c1;      t1 -= c1 * (1 << 25); \
    c3 = (t3 + (i64) (1<<24)) >> 25; t4 += c3;      t3 -= c3 * (1 << 25); \
    c5 = (t5 + (i64) (1<<24)) >> 25; t6 += c5;      t5 -= c5 * (1 << 25); \
    c7 = (t7 + (i64) (1<<24)) >> 25; t8 += c7;      t7 -= c7 * (1 << 25); \
    c0 = (t0 + (i64) (1<<25)) >> 26; t1 += c0;      t0 -= c0 * (1 << 26); \
    c2 = (t2 + (i64) (1<<25)) >> 26; t3 += c2;      t2 -= c2 * (1 << 26); \
    c4 = (t4 + (i64) (1<<25)) >> 26; t5 += c4;      t4 -= c4 * (1 << 26); \
    c6 = (t6 + (i64) (1<<25)) >> 26; t7 += c6;      t6 -= c6 * (1 << 26); \
    c8 = (t8 + (i64) (1<<25)) >> 26; t9 += c8;      t8 -= c8 * (1 << 26); \
    h[0]=(i32)t0;  h[1]=(i32)t1;  h[2]=(i32)t2;  h[3]=(i32)t3;  h[4]=(i32)t4; \
    h[5]=(i32)t5;  h[6]=(i32)t6;  h[7]=(i32)t7;  h[8]=(i32)t8;  h[9]=(i32)t9

static void fe_frombytes(fe h, const u8 s[32])
{
    i64 t0 =  load32_le(s);
    i64 t1 =  load24_le(s +  4) << 6;
    i64 t2 =  load24_le(s +  7) << 5;
    i64 t3 =  load24_le(s + 10) << 3;
    i64 t4 =  load24_le(s + 13) << 2;
    i64 t5 =  load32_le(s + 16);
    i64 t6 =  load24_le(s + 20) << 7;
    i64 t7 =  load24_le(s + 23) << 5;
    i64 t8 =  load24_le(s + 26) << 4;
    i64 t9 = (load24_le(s + 29) & 8388607) << 2;
    FE_CARRY;
}

static void fe_mul_small(fe h, const fe f, i32 g)
{
    i64 t0 = f[0] * (i64) g;  i64 t1 = f[1] * (i64) g;
    i64 t2 = f[2] * (i64) g;  i64 t3 = f[3] * (i64) g;
    i64 t4 = f[4] * (i64) g;  i64 t5 = f[5] * (i64) g;
    i64 t6 = f[6] * (i64) g;  i64 t7 = f[7] * (i64) g;
    i64 t8 = f[8] * (i64) g;  i64 t9 = f[9] * (i64) g;
    FE_CARRY;
}
static void fe_mul121666(fe h, const fe f) { fe_mul_small(h, f, 121666); }

static void fe_mul(fe h, const fe f, const fe g)
{
    // Everything is unrolled and put in temporary variables.
    // We could roll the loop, but that would make curve25519 twice as slow.
    i32 f0 = f[0]; i32 f1 = f[1]; i32 f2 = f[2]; i32 f3 = f[3]; i32 f4 = f[4];
    i32 f5 = f[5]; i32 f6 = f[6]; i32 f7 = f[7]; i32 f8 = f[8]; i32 f9 = f[9];
    i32 g0 = g[0]; i32 g1 = g[1]; i32 g2 = g[2]; i32 g3 = g[3]; i32 g4 = g[4];
    i32 g5 = g[5]; i32 g6 = g[6]; i32 g7 = g[7]; i32 g8 = g[8]; i32 g9 = g[9];
    i32 F1 = f1*2; i32 F3 = f3*2; i32 F5 = f5*2; i32 F7 = f7*2; i32 F9 = f9*2;
    i32 G1 = g1*19;  i32 G2 = g2*19;  i32 G3 = g3*19;
    i32 G4 = g4*19;  i32 G5 = g5*19;  i32 G6 = g6*19;
    i32 G7 = g7*19;  i32 G8 = g8*19;  i32 G9 = g9*19;

    i64 h0 = f0*(i64)g0 + F1*(i64)G9 + f2*(i64)G8 + F3*(i64)G7 + f4*(i64)G6
        +    F5*(i64)G5 + f6*(i64)G4 + F7*(i64)G3 + f8*(i64)G2 + F9*(i64)G1;
    i64 h1 = f0*(i64)g1 + f1*(i64)g0 + f2*(i64)G9 + f3*(i64)G8 + f4*(i64)G7
        +    f5*(i64)G6 + f6*(i64)G5 + f7*(i64)G4 + f8*(i64)G3 + f9*(i64)G2;
    i64 h2 = f0*(i64)g2 + F1*(i64)g1 + f2*(i64)g0 + F3*(i64)G9 + f4*(i64)G8
        +    F5*(i64)G7 + f6*(i64)G6 + F7*(i64)G5 + f8*(i64)G4 + F9*(i64)G3;
    i64 h3 = f0*(i64)g3 + f1*(i64)g2 + f2*(i64)g1 + f3*(i64)g0 + f4*(i64)G9
        +    f5*(i64)G8 + f6*(i64)G7 + f7*(i64)G6 + f8*(i64)G5 + f9*(i64)G4;
    i64 h4 = f0*(i64)g4 + F1*(i64)g3 + f2*(i64)g2 + F3*(i64)g1 + f4*(i64)g0
        +    F5*(i64)G9 + f6*(i64)G8 + F7*(i64)G7 + f8*(i64)G6 + F9*(i64)G5;
    i64 h5 = f0*(i64)g5 + f1*(i64)g4 + f2*(i64)g3 + f3*(i64)g2 + f4*(i64)g1
        +    f5*(i64)g0 + f6*(i64)G9 + f7*(i64)G8 + f8*(i64)G7 + f9*(i64)G6;
    i64 h6 = f0*(i64)g6 + F1*(i64)g5 + f2*(i64)g4 + F3*(i64)g3 + f4*(i64)g2
        +    F5*(i64)g1 + f6*(i64)g0 + F7*(i64)G9 + f8*(i64)G8 + F9*(i64)G7;
    i64 h7 = f0*(i64)g7 + f1*(i64)g6 + f2*(i64)g5 + f3*(i64)g4 + f4*(i64)g3
        +    f5*(i64)g2 + f6*(i64)g1 + f7*(i64)g0 + f8*(i64)G9 + f9*(i64)G8;
    i64 h8 = f0*(i64)g8 + F1*(i64)g7 + f2*(i64)g6 + F3*(i64)g5 + f4*(i64)g4
        +    F5*(i64)g3 + f6*(i64)g2 + F7*(i64)g1 + f8*(i64)g0 + F9*(i64)G9;
    i64 h9 = f0*(i64)g9 + f1*(i64)g8 + f2*(i64)g7 + f3*(i64)g6 + f4*(i64)g5
        +    f5*(i64)g4 + f6*(i64)g3 + f7*(i64)g2 + f8*(i64)g1 + f9*(i64)g0;

#define CARRY                                                             \
    i64 c0, c1, c2, c3, c4, c5, c6, c7, c8, c9;                           \
    c0 = (h0 + (i64) (1<<25)) >> 26; h1 += c0;      h0 -= c0 * (1 << 26); \
    c4 = (h4 + (i64) (1<<25)) >> 26; h5 += c4;      h4 -= c4 * (1 << 26); \
    c1 = (h1 + (i64) (1<<24)) >> 25; h2 += c1;      h1 -= c1 * (1 << 25); \
    c5 = (h5 + (i64) (1<<24)) >> 25; h6 += c5;      h5 -= c5 * (1 << 25); \
    c2 = (h2 + (i64) (1<<25)) >> 26; h3 += c2;      h2 -= c2 * (1 << 26); \
    c6 = (h6 + (i64) (1<<25)) >> 26; h7 += c6;      h6 -= c6 * (1 << 26); \
    c3 = (h3 + (i64) (1<<24)) >> 25; h4 += c3;      h3 -= c3 * (1 << 25); \
    c7 = (h7 + (i64) (1<<24)) >> 25; h8 += c7;      h7 -= c7 * (1 << 25); \
    c4 = (h4 + (i64) (1<<25)) >> 26; h5 += c4;      h4 -= c4 * (1 << 26); \
    c8 = (h8 + (i64) (1<<25)) >> 26; h9 += c8;      h8 -= c8 * (1 << 26); \
    c9 = (h9 + (i64) (1<<24)) >> 25; h0 += c9 * 19; h9 -= c9 * (1 << 25); \
    c0 = (h0 + (i64) (1<<25)) >> 26; h1 += c0;      h0 -= c0 * (1 << 26); \
    h[0]=(i32)h0;  h[1]=(i32)h1;  h[2]=(i32)h2;  h[3]=(i32)h3;  h[4]=(i32)h4; \
    h[5]=(i32)h5;  h[6]=(i32)h6;  h[7]=(i32)h7;  h[8]=(i32)h8;  h[9]=(i32)h9; \

    CARRY;
}

// we could use fe_mul() for this, but this is significantly faster
static void fe_sq(fe h, const fe f)
{
    i32 f0 = f[0]; i32 f1 = f[1]; i32 f2 = f[2]; i32 f3 = f[3]; i32 f4 = f[4];
    i32 f5 = f[5]; i32 f6 = f[6]; i32 f7 = f[7]; i32 f8 = f[8]; i32 f9 = f[9];
    i32 f0_2  = f0*2;   i32 f1_2  = f1*2;   i32 f2_2  = f2*2;   i32 f3_2 = f3*2;
    i32 f4_2  = f4*2;   i32 f5_2  = f5*2;   i32 f6_2  = f6*2;   i32 f7_2 = f7*2;
    i32 f5_38 = f5*38;  i32 f6_19 = f6*19;  i32 f7_38 = f7*38;
    i32 f8_19 = f8*19;  i32 f9_38 = f9*38;

    i64 h0 = f0  *(i64)f0    + f1_2*(i64)f9_38 + f2_2*(i64)f8_19
        +    f3_2*(i64)f7_38 + f4_2*(i64)f6_19 + f5  *(i64)f5_38;
    i64 h1 = f0_2*(i64)f1    + f2  *(i64)f9_38 + f3_2*(i64)f8_19
        +    f4  *(i64)f7_38 + f5_2*(i64)f6_19;
    i64 h2 = f0_2*(i64)f2    + f1_2*(i64)f1    + f3_2*(i64)f9_38
        +    f4_2*(i64)f8_19 + f5_2*(i64)f7_38 + f6  *(i64)f6_19;
    i64 h3 = f0_2*(i64)f3    + f1_2*(i64)f2    + f4  *(i64)f9_38
        +    f5_2*(i64)f8_19 + f6  *(i64)f7_38;
    i64 h4 = f0_2*(i64)f4    + f1_2*(i64)f3_2  + f2  *(i64)f2
        +    f5_2*(i64)f9_38 + f6_2*(i64)f8_19 + f7  *(i64)f7_38;
    i64 h5 = f0_2*(i64)f5    + f1_2*(i64)f4    + f2_2*(i64)f3
        +    f6  *(i64)f9_38 + f7_2*(i64)f8_19;
    i64 h6 = f0_2*(i64)f6    + f1_2*(i64)f5_2  + f2_2*(i64)f4
        +    f3_2*(i64)f3    + f7_2*(i64)f9_38 + f8  *(i64)f8_19;
    i64 h7 = f0_2*(i64)f7    + f1_2*(i64)f6    + f2_2*(i64)f5
        +    f3_2*(i64)f4    + f8  *(i64)f9_38;
    i64 h8 = f0_2*(i64)f8    + f1_2*(i64)f7_2  + f2_2*(i64)f6
        +    f3_2*(i64)f5_2  + f4  *(i64)f4    + f9  *(i64)f9_38;
    i64 h9 = f0_2*(i64)f9    + f1_2*(i64)f8    + f2_2*(i64)f7
        +    f3_2*(i64)f6    + f4  *(i64)f5_2;

    CARRY;
}

static void fe_sq2(fe h, const fe f)
{
    fe_sq(h, f);
    fe_mul_small(h, h, 2);
}

// This could be simplified, but it would be slower
static void fe_invert(fe out, const fe z)
{
    fe t0, t1, t2, t3;
    fe_sq(t0, z );
    fe_sq(t1, t0);
    fe_sq(t1, t1);
    fe_mul(t1,  z, t1);
    fe_mul(t0, t0, t1);
    fe_sq(t2, t0);                                fe_mul(t1 , t1, t2);
    fe_sq(t2, t1); FOR (i, 1,   5) fe_sq(t2, t2); fe_mul(t1 , t2, t1);
    fe_sq(t2, t1); FOR (i, 1,  10) fe_sq(t2, t2); fe_mul(t2 , t2, t1);
    fe_sq(t3, t2); FOR (i, 1,  20) fe_sq(t3, t3); fe_mul(t2 , t3, t2);
    fe_sq(t2, t2); FOR (i, 1,  10) fe_sq(t2, t2); fe_mul(t1 , t2, t1);
    fe_sq(t2, t1); FOR (i, 1,  50) fe_sq(t2, t2); fe_mul(t2 , t2, t1);
    fe_sq(t3, t2); FOR (i, 1, 100) fe_sq(t3, t3); fe_mul(t2 , t3, t2);
    fe_sq(t2, t2); FOR (i, 1,  50) fe_sq(t2, t2); fe_mul(t1 , t2, t1);
    fe_sq(t1, t1); FOR (i, 1,   5) fe_sq(t1, t1); fe_mul(out, t1, t0);
    WIPE_BUFFER(t0);
    WIPE_BUFFER(t1);
    WIPE_BUFFER(t2);
    WIPE_BUFFER(t3);
}

// This could be simplified, but it would be slower
static void fe_pow22523(fe out, const fe z)
{
    fe t0, t1, t2;
    fe_sq(t0, z);
    fe_sq(t1,t0);                   fe_sq(t1, t1);  fe_mul(t1, z, t1);
    fe_mul(t0, t0, t1);
    fe_sq(t0, t0);                                  fe_mul(t0, t1, t0);
    fe_sq(t1, t0);  FOR (i, 1,   5) fe_sq(t1, t1);  fe_mul(t0, t1, t0);
    fe_sq(t1, t0);  FOR (i, 1,  10) fe_sq(t1, t1);  fe_mul(t1, t1, t0);
    fe_sq(t2, t1);  FOR (i, 1,  20) fe_sq(t2, t2);  fe_mul(t1, t2, t1);
    fe_sq(t1, t1);  FOR (i, 1,  10) fe_sq(t1, t1);  fe_mul(t0, t1, t0);
    fe_sq(t1, t0);  FOR (i, 1,  50) fe_sq(t1, t1);  fe_mul(t1, t1, t0);
    fe_sq(t2, t1);  FOR (i, 1, 100) fe_sq(t2, t2);  fe_mul(t1, t2, t1);
    fe_sq(t1, t1);  FOR (i, 1,  50) fe_sq(t1, t1);  fe_mul(t0, t1, t0);
    fe_sq(t0, t0);  FOR (i, 1,   2) fe_sq(t0, t0);  fe_mul(out, t0, z);
    WIPE_BUFFER(t0);
    WIPE_BUFFER(t1);
    WIPE_BUFFER(t2);
}

static void fe_tobytes(u8 s[32], const fe h)
{
    i32 t[10];
    FOR (i, 0, 10) {
        t[i] = h[i];
    }
    i32 q = (19 * t[9] + (((i32) 1) << 24)) >> 25;
    FOR (i, 0, 5) {
        q += t[2*i  ]; q >>= 26;
        q += t[2*i+1]; q >>= 25;
    }
    t[0] += 19 * q;

    i32 c0 = t[0] >> 26; t[1] += c0; t[0] -= c0 * (1 << 26);
    i32 c1 = t[1] >> 25; t[2] += c1; t[1] -= c1 * (1 << 25);
    i32 c2 = t[2] >> 26; t[3] += c2; t[2] -= c2 * (1 << 26);
    i32 c3 = t[3] >> 25; t[4] += c3; t[3] -= c3 * (1 << 25);
    i32 c4 = t[4] >> 26; t[5] += c4; t[4] -= c4 * (1 << 26);
    i32 c5 = t[5] >> 25; t[6] += c5; t[5] -= c5 * (1 << 25);
    i32 c6 = t[6] >> 26; t[7] += c6; t[6] -= c6 * (1 << 26);
    i32 c7 = t[7] >> 25; t[8] += c7; t[7] -= c7 * (1 << 25);
    i32 c8 = t[8] >> 26; t[9] += c8; t[8] -= c8 * (1 << 26);
    i32 c9 = t[9] >> 25;             t[9] -= c9 * (1 << 25);

    store32_le(s +  0, ((u32)t[0] >>  0) | ((u32)t[1] << 26));
    store32_le(s +  4, ((u32)t[1] >>  6) | ((u32)t[2] << 19));
    store32_le(s +  8, ((u32)t[2] >> 13) | ((u32)t[3] << 13));
    store32_le(s + 12, ((u32)t[3] >> 19) | ((u32)t[4] <<  6));
    store32_le(s + 16, ((u32)t[5] >>  0) | ((u32)t[6] << 25));
    store32_le(s + 20, ((u32)t[6] >>  7) | ((u32)t[7] << 19));
    store32_le(s + 24, ((u32)t[7] >> 13) | ((u32)t[8] << 12));
    store32_le(s + 28, ((u32)t[8] >> 20) | ((u32)t[9] <<  6));

    WIPE_BUFFER(t);
}

//  Parity check.  Returns 0 if even, 1 if odd
static int fe_isnegative(const fe f)
{
    u8 s[32];
    fe_tobytes(s, f);
    u8 isneg = s[0] & 1;
    WIPE_BUFFER(s);
    return isneg;
}

static int fe_isnonzero(const fe f)
{
    u8 s[32];
    fe_tobytes(s, f);
    u8 isnonzero = zerocmp32(s);
    WIPE_BUFFER(s);
    return isnonzero;
}

///////////////
/// X-25519 /// Taken from Supercop's ref10 implementation.
///////////////

static void trim_scalar(u8 s[32])
{
    s[ 0] &= 248;
    s[31] &= 127;
    s[31] |= 64;
}

static int scalar_bit(const u8 s[32], size_t i) {return (s[i>>3] >> (i&7)) & 1;}

int crypto_x25519(u8       raw_shared_secret[32],
                  const u8 your_secret_key  [32],
                  const u8 their_public_key [32])
{
    // computes the scalar product
    fe x1;
    fe_frombytes(x1, their_public_key);

    // restrict the possible scalar values
    u8 e[32];
    FOR (i, 0, 32) {
        e[i] = your_secret_key[i];
    }
    trim_scalar(e);

    // computes the actual scalar product (the result is in x2 and z2)
    fe x2, z2, x3, z3, t0, t1;
    // Montgomery ladder
    // In projective coordinates, to avoid divisons: x = X / Z
    // We don't care about the y coordinate, it's only 1 bit of information
    fe_1(x2);        fe_0(z2); // "zero" point
    fe_copy(x3, x1); fe_1(z3); // "one"  point
    int swap = 0;
    for (int pos = 254; pos >= 0; --pos) {
        // constant time conditional swap before ladder step
        int b = scalar_bit(e, pos);
        swap ^= b; // xor trick avoids swapping at the end of the loop
        fe_cswap(x2, x3, swap);
        fe_cswap(z2, z3, swap);
        swap = b;  // anticipates one last swap after the loop

        // Montgomery ladder step: replaces (P2, P3) by (P2*2, P2+P3)
        // with differential addition
        fe_sub(t0, x3, z3);  fe_sub(t1, x2, z2);    fe_add(x2, x2, z2);
        fe_add(z2, x3, z3);  fe_mul(z3, t0, x2);    fe_mul(z2, z2, t1);
        fe_sq (t0, t1    );  fe_sq (t1, x2    );    fe_add(x3, z3, z2);
        fe_sub(z2, z3, z2);  fe_mul(x2, t1, t0);    fe_sub(t1, t1, t0);
        fe_sq (z2, z2    );  fe_mul121666(z3, t1);  fe_sq (x3, x3    );
        fe_add(t0, t0, z3);  fe_mul(z3, x1, z2);    fe_mul(z2, t1, t0);
    }
    // last swap is necessary to compensate for the xor trick
    // Note: after this swap, P3 == P2 + P1.
    fe_cswap(x2, x3, swap);
    fe_cswap(z2, z3, swap);

    // normalises the coordinates: x == X / Z
    fe_invert(z2, z2);
    fe_mul(x2, x2, z2);
    fe_tobytes(raw_shared_secret, x2);

    WIPE_BUFFER(x1);  WIPE_BUFFER(e );
    WIPE_BUFFER(x2);  WIPE_BUFFER(z2);
    WIPE_BUFFER(x3);  WIPE_BUFFER(z3);
    WIPE_BUFFER(t0);  WIPE_BUFFER(t1);

    // Returns -1 if the output is all zero
    // (happens with some malicious public keys)
    return -1 - zerocmp32(raw_shared_secret);
}

void crypto_x25519_public_key(u8       public_key[32],
                              const u8 secret_key[32])
{
    static const u8 base_point[32] = {9};
    crypto_x25519(public_key, secret_key, base_point);
}

///////////////
/// Ed25519 ///
///////////////

static const  u64 L[32] = { 0xed, 0xd3, 0xf5, 0x5c, 0x1a, 0x63, 0x12, 0x58,
                            0xd6, 0x9c, 0xf7, 0xa2, 0xde, 0xf9, 0xde, 0x14,
                            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10};

static void modL(u8 *r, i64 x[64])
{
    for (unsigned i = 63; i >= 32; i--) {
        i64 carry = 0;
        FOR (j, i-32, i-12) {
            x[j] += carry - 16 * x[i] * L[j - (i - 32)];
            carry = (x[j] + 128) >> 8;
            x[j] -= carry * (1 << 8);
        }
        x[i-12] += carry;
        x[i] = 0;
    }
    i64 carry = 0;
    FOR (i, 0, 32) {
        x[i] += carry - (x[31] >> 4) * L[i];
        carry = x[i] >> 8;
        x[i] &= 255;
    }
    FOR (i, 0, 32) {
        x[i] -= carry * L[i];
    }
    FOR (i, 0, 32) {
        x[i+1] += x[i] >> 8;
        r[i  ]  = x[i] & 255;
    }
}

static void reduce(u8 r[64])
{
    i64 x[64];
    FOR (i, 0, 64) {
        x[i] = (u64) r[i];
        r[i] = 0;
    }
    modL(r, x);
    WIPE_BUFFER(x);
}

// r = (a * b) + c
static void mul_add(u8 r[32], const u8 a[32], const u8 b[32], const u8 c[32])
{
    i64 s[64];
    FOR (i,  0, 32) { s[i] = (u64) c[i]; }
    FOR (i, 32, 64) { s[i] = 0;          }
    FOR (i,  0, 32) {
        FOR (j, 0, 32) {
            s[i+j] += a[i] * (u64) b[j];
        }
    }
    modL(r, s);
    WIPE_BUFFER(s);
}

static int is_above_L(const u8 a[32])
{
    for (int i = 31; i >= 0; i--) {
        if (a[i] > L[i]) { return 1; }
        if (a[i] < L[i]) { return 0; }
    }
    return 1;
}

// Point in a twisted Edwards curve,
// in extended projective coordinates.
// x = X/Z, y = Y/Z, T = XY/Z
typedef struct { fe X;  fe Y;  fe Z; fe T;  } ge;
typedef struct { fe Yp; fe Ym; fe Z; fe T2; } ge_cached;

static void ge_zero(ge *p)
{
    fe_0(p->X);
    fe_1(p->Y);
    fe_1(p->Z);
    fe_0(p->T);
}

static void ge_tobytes(u8 s[32], const ge *h)
{
    fe recip, x, y;
    fe_invert(recip, h->Z);
    fe_mul(x, h->X, recip);
    fe_mul(y, h->Y, recip);
    fe_tobytes(s, y);
    s[31] ^= fe_isnegative(x) << 7;

    WIPE_BUFFER(recip);
    WIPE_BUFFER(x);
    WIPE_BUFFER(y);
}

// Variable time! s must not be secret!
static int ge_frombytes_neg_vartime(ge *h, const u8 s[32])
{
    static const fe d = {
        -10913610, 13857413, -15372611, 6949391, 114729,
        -8787816, -6275908, -3247719, -18696448, -12055116
    } ;
    static const fe sqrtm1 = {
        -32595792, -7943725, 9377950, 3500415, 12389472,
        -272473, -25146209, -2005654, 326686, 11406482
    } ;
    fe u, v, v3, vxx, check; // no secret, no wipe
    fe_frombytes(h->Y, s);
    fe_1(h->Z);
    fe_sq(u, h->Y);            // y^2
    fe_mul(v, u, d);
    fe_sub(u, u, h->Z);        // u = y^2-1
    fe_add(v, v, h->Z);        // v = dy^2+1

    fe_sq(v3, v);
    fe_mul(v3, v3, v);         // v3 = v^3
    fe_sq(h->X, v3);
    fe_mul(h->X, h->X, v);
    fe_mul(h->X, h->X, u);     // x = uv^7

    fe_pow22523(h->X, h->X);   // x = (uv^7)^((q-5)/8)
    fe_mul(h->X, h->X, v3);
    fe_mul(h->X, h->X, u);     // x = uv^3(uv^7)^((q-5)/8)

    fe_sq(vxx, h->X);
    fe_mul(vxx, vxx, v);
    fe_sub(check, vxx, u);     // vx^2-u
    if (fe_isnonzero(check)) {
        fe_add(check, vxx, u); // vx^2+u
        if (fe_isnonzero(check)) {
            return -1;
        }
        fe_mul(h->X, h->X, sqrtm1);
    }
    if (fe_isnegative(h->X) == (s[31] >> 7)) {
        fe_neg(h->X, h->X);
    }
    fe_mul(h->T, h->X, h->Y);
    return 0;
}

static void ge_cache(ge_cached *c, const ge *p)
{
    static const fe D2 = { // - 2 * 121665 / 121666
        -21827239, -5839606, -30745221, 13898782, 229458,
        15978800, -12551817, -6495438, 29715968, 9444199
    };
    fe_add (c->Yp, p->Y, p->X);
    fe_sub (c->Ym, p->Y, p->X);
    fe_copy(c->Z , p->Z      );
    fe_mul (c->T2, p->T, D2  );
}

static void ge_add(ge *s, const ge *p, const ge_cached *q)
{
    fe a, b; // not used to process secrets, no need to wipe
    fe_add(a   , p->Y, p->X );
    fe_sub(b   , p->Y, p->X );
    fe_mul(a   , a   , q->Yp);
    fe_mul(b   , b   , q->Ym);
    fe_add(s->Y, a   , b    );
    fe_sub(s->X, a   , b    );

    fe_add(s->Z, p->Z, p->Z );
    fe_mul(s->Z, s->Z, q->Z );
    fe_mul(s->T, p->T, q->T2);
    fe_add(a   , s->Z, s->T );
    fe_sub(b   , s->Z, s->T );

    fe_mul(s->T, s->X, s->Y);
    fe_mul(s->X, s->X, b   );
    fe_mul(s->Y, s->Y, a   );
    fe_mul(s->Z, a   , b   );
}

static void ge_sub(ge *s, const ge *p, const ge_cached *q)
{
    ge_cached neg;
    fe_copy(neg.Ym, q->Yp);
    fe_copy(neg.Yp, q->Ym);
    fe_copy(neg.Z , q->Z );
    fe_neg (neg.T2, q->T2);
    ge_add(s, p, &neg);
}

static void ge_madd(ge *s, const ge *p, const fe yp, const fe ym, const fe t2,
                    fe a, fe b)
{
    fe_add(a   , p->Y, p->X );
    fe_sub(b   , p->Y, p->X );
    fe_mul(a   , a   , yp   );
    fe_mul(b   , b   , ym   );
    fe_add(s->Y, a   , b    );
    fe_sub(s->X, a   , b    );

    fe_add(s->Z, p->Z, p->Z );
    fe_mul(s->T, p->T, t2   );
    fe_add(a   , s->Z, s->T );
    fe_sub(b   , s->Z, s->T );

    fe_mul(s->T, s->X, s->Y);
    fe_mul(s->X, s->X, b   );
    fe_mul(s->Y, s->Y, a   );
    fe_mul(s->Z, a   , b   );
}

static void ge_double(ge *s, const ge *p, ge *q)
{
    fe_sq (q->X, p->X);
    fe_sq (q->Y, p->Y);
    fe_sq2(q->Z, p->Z);
    fe_add(q->T, p->X, p->Y);
    fe_sq (s->T, q->T);
    fe_add(q->T, q->Y, q->X);
    fe_sub(q->Y, q->Y, q->X);
    fe_sub(q->X, s->T, q->T);
    fe_sub(q->Z, q->Z, q->Y);

    fe_mul(s->X, q->X , q->Z);
    fe_mul(s->Y, q->T , q->Y);
    fe_mul(s->Z, q->Y , q->Z);
    fe_mul(s->T, q->X , q->T);
}

// Compute signed sliding windows (either 0, or odd numbers between -15 and 15)
static void slide(i8 adds[258], const u8 scalar[32])
{
    FOR (i,   0, 256) { adds[i] = scalar_bit(scalar, i); }
    FOR (i, 256, 258) { adds[i] = 0;                     }
    FOR (i, 0, 254) {
        if (adds[i] != 0) {
            // base value of the 5-bit window
            FOR (j, 1, 5) {
                adds[i  ] |= adds[i+j] << j;
                adds[i+j]  = 0;
            }
            if (adds[i] > 16) {
                // go back to [-15, 15], propagate carry.
                adds[i] -= 32;
                size_t j = i + 5;
                while (adds[j] != 0) {
                    adds[j] = 0;
                    j++;
                }
                adds[j] = 1;
            }
        }
    }
}

// Look up table for sliding windows
static void ge_precompute(ge_cached lut[8], const ge *P1)
{
    ge P2, tmp;
    ge_double(&P2, P1, &tmp);
    ge_cache(&lut[0], P1);
    FOR (i, 0, 7) {
        ge_add(&tmp, &P2, &lut[i]);
        ge_cache(&lut[i+1], &tmp);
    }
}

// Could be a function, but the macro avoids some overhead.
#define LUT_ADD(sum, lut, adds, i)                             \
    if (adds[i] > 0) { ge_add(sum, sum, &lut[ adds[i] / 2]); } \
    if (adds[i] < 0) { ge_sub(sum, sum, &lut[-adds[i] / 2]); }

// Variable time! P, sP, and sB must not be secret!
static void ge_double_scalarmult_vartime(ge *sum, const ge *P,
                                         u8 p[32], u8 b[32])
{
    static const fe X = { -14297830, -7645148, 16144683, -16471763, 27570974,
                          -2696100, -26142465, 8378389, 20764389, 8758491 };
    static const fe Y = { -26843541, -6710886, 13421773, -13421773, 26843546,
                          6710886, -13421773, 13421773, -26843546, -6710886 };
    ge B;
    fe_copy(B.X, X);
    fe_copy(B.Y, Y);
    fe_1   (B.Z);
    fe_mul (B.T, X, Y);

    // cached points for addition
    ge_cached cP[8];  ge_precompute(cP,  P);
    ge_cached cB[8];  ge_precompute(cB, &B);
    i8 p_adds[258];   slide(p_adds, p);
    i8 b_adds[258];   slide(b_adds, b);

    // Avoid the first doublings
    int i = 253;
    while (i >= 0         &&
           p_adds[i] == 0 &&
           b_adds[i] == 0) {
        i--;
    }

    // Merged double and add ladder
    ge_zero(sum);
    LUT_ADD(sum, cP, p_adds, i);
    LUT_ADD(sum, cB, b_adds, i);
    i--;
    while (i >= 0) {
        ge_double(sum, sum, &B); // B is no longer used, we can overwrite it
        LUT_ADD(sum, cP, p_adds, i);
        LUT_ADD(sum, cB, b_adds, i);
        i--;
    }
}

// 5-bit signed comb in cached format (Niels coordinates, Z=1)
static const fe comb_Yp[16] = {
    {2615675, 9989699, 17617367, -13953520, -8802803,
     1447286, -8909978, -270892, -12199203, -11617247},
    {-1271192, 4785266, -29856067, -6036322, -10435381,
     15493337, 20321440, -6036064, 15902131, 13420909},
    {-26170888, -12891603, 9568996, -6197816, 26424622,
     16308973, -4518568, -3771275, -15522557, 3991142},
    {-25875044, 1958396, 19442242, -9809943, -26099408,
     -18589, -30794750, -14100910, 4971028, -10535388},
    {-13896937, -7357727, -12131124, 617289, -33188817,
     10080542, 6402555, 10779157, 1176712, 2472642},
    {71503, 12662254, -17008072, -8370006, 23408384,
     -12897959, 32287612, 11241906, -16724175, 15336924},
    {27397666, 4059848, 23573959, 8868915, -10602416,
     -10456346, -22812831, -9666299, 31810345, -2695469},
    {-3418193, -694531, 2320482, -11850408, -1981947,
     -9606132, 23743894, 3933038, -25004889, -4478918},
    {-4448372, 5537982, -4805580, 14016777, 15544316,
     16039459, -7143453, -8003716, -21904564, 8443777},
    {32495180, 15749868, 2195406, -15542321, -3213890,
     -4030779, -2915317, 12751449, -1872493, 11926798},
    {26779741, 12553580, -24344000, -4071926, -19447556,
     -13464636, 21989468, 7826656, -17344881, 10055954},
    {5848288, -1639207, -10452929, -11760637, 6484174,
     -5895268, -11561603, 587105, -19220796, 14378222},
    {32050187, 12536702, 9206308, -10016828, -13333241,
     -4276403, -24225594, 14562479, -31803624, -9967812},
    {23536033, -6219361, 199701, 4574817, 30045793,
     7163081, -2244033, 883497, 10960746, -14779481},
    {-8143354, -11558749, 15772067, 14293390, 5914956,
     -16702904, -7410985, 7536196, 6155087, 16571424},
    {6211591, -11166015, 24568352, 2768318, -10822221,
     11922793, 33211827, 3852290, -13160369, -8855385},
};
static const fe comb_Ym[16] = {
    {8873912, 14981221, 13714139, 6923085, 25481101,
     4243739, 4646647, -203847, 9015725, -16205935},
    {-1827892, 15407265, 2351140, -11810728, 28403158,
     -1487103, -15057287, -4656433, -3780118, -1145998},
    {-30623162, -11845055, -11327147, -16008347, 17564978,
     -1449578, -20580262, 14113978, 29643661, 15580734},
    {-15109423, 13348938, -14756006, 14132355, 30481360,
     1830723, -240510, 9371801, -13907882, 8024264},
    {25119567, 5628696, 10185251, -9279452, 683770,
     -14523112, -7982879, -16450545, 1431333, -13253541},
    {-8390493, 1276691, 19008763, -12736675, -9249429,
     -12526388, 17434195, -13761261, 18962694, -1227728},
    {26361856, -12366343, 8941415, 15163068, 7069802,
     -7240693, -18656349, 8167008, 31106064, -1670658},
    {-5677136, -11012483, -1246680, -6422709, 14772010,
     1829629, -11724154, -15914279, -18177362, 1301444},
    {937094, 12383516, -22597284, 7580462, -18767748,
     13813292, -2323566, 13503298, 11510849, -10561992},
    {28028043, 14715827, -6558532, -1773240, 27563607,
     -9374554, 3201863, 8865591, -16953001, 7659464},
    {13628467, 5701368, 4674031, 11935670, 11461401,
     10699118, 31846435, -114971, -8269924, -14777505},
    {-22124018, -12859127, 11966893, 1617732, 30972446,
     -14350095, -21822286, 8369862, -29443219, -15378798},
    {290131, -471434, 8840522, -2654851, 25963762,
     -11578288, -7227978, 13847103, 30641797, 6003514},
    {-23547482, -11475166, -11913550, 9374455, 22813401,
     -5707910, 26635288, 9199956, 20574690, 2061147},
    {9715324, 7036821, -17981446, -11505533, 26555178,
     -3571571, 5697062, -14128022, 2795223, 9694380},
    {14864569, -6319076, -3080, -8151104, 4994948,
     -1572144, -41927, 9269803, 13881712, -13439497},
};
static const fe comb_T2[16] = {
    {-18494317, 2686822, 18449263, -13905325, 5966562,
     -3368714, 2738304, -8583315, 15987143, 12180258},
    {-33336513, -13705917, -18473364, -5039204, -4268481,
     -4136039, -8192211, -2935105, -19354402, 5995895},
    {-19753139, -1729018, 21880604, 13471713, 28315373,
     -8530159, -17492688, 11730577, -8790216, 3942124},
    {17278020, 3905045, 29577748, 11151940, 18451761,
     -6801382, 31480073, -13819665, 26308905, 10868496},
    {26937294, 3313561, 28601532, -3497112, -22814130,
     11073654, 8956359, -16757370, 13465868, 16623983},
    {-5468054, 6059101, -31275300, 2469124, 26532937,
     8152142, 6423741, -11427054, -15537747, -10938247},
    {-11303505, -9659620, -12354748, -9331434, 19501116,
     -9146390, -841918, -5315657, 8903828, 8839982},
    {16603354, -215859, 1591180, 3775832, -705596,
     -13913449, 26574704, 14963118, 19649719, 6562441},
    {33188866, -12232360, -24929148, -6133828, 21818432,
     11040754, -3041582, -3524558, -29364727, -10264096},
    {-20704194, -12560423, -1235774, -785473, 13240395,
     4831780, -472624, -3796899, 25480903, -15422283},
    {-2204347, -16313180, -21388048, 7520851, -8697745,
     -14460961, 20894017, 12210317, -475249, -2319102},
    {-16407882, 4940236, -21194947, 10781753, 22248400,
     14425368, 14866511, -7552907, 12148703, -7885797},
    {16376744, 15908865, -30663553, 4663134, -30882819,
     -10105163, 19294784, -10800440, -33259252, 2563437},
    {30208741, 11594088, -15145888, 15073872, 5279309,
     -9651774, 8273234, 4796404, -31270809, -13316433},
    {-17802574, 14455251, 27149077, -7832700, -29163160,
     -7246767, 17498491, -4216079, 31788733, -14027536},
    {-25233439, -9389070, -6618212, -3268087, -521386,
     -7350198, 21035059, -14970947, 25910190, 11122681},
};

static void ge_scalarmult_base(ge *p, const u8 scalar[32])
{
    // 5-bits signed comb, from Mike Hamburg's
    // Fast and compact elliptic-curve cryptography (2012)
    static const u8 half_mod_L[32] = { // 1 / 2 modulo L
        0xf7, 0xe9, 0x7a, 0x2e, 0x8d, 0x31, 0x09, 0x2c,
        0x6b, 0xce, 0x7b, 0x51, 0xef, 0x7c, 0x6f, 0x0a,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08,
    };
    static const u8 half_ones[32] = { // (2^255 - 1) / 2 modulo L
        0x42, 0x9a, 0xa3, 0xba, 0x23, 0xa5, 0xbf, 0xcb,
        0x11, 0x5b, 0x9d, 0xc5, 0x74, 0x95, 0xf3, 0xb6,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x07,
    };
    // All bits set form: 1 means 1, 0 means -1
    u8 s_scalar[32];
    mul_add(s_scalar, scalar, half_mod_L, half_ones);

    // Double and add ladder
    fe yp, ym, t2, n2, a, b; // temporaries for addition
    ge dbl;                  // temporary for doublings
    ge_zero(p);
    for (int i = 50; i >= 0; i--) {
        if (i < 50) {
            ge_double(p, p, &dbl);
        }
        fe_1(yp);
        fe_1(ym);
        fe_0(t2);
        u8 teeth =  scalar_bit(s_scalar, i)
            +      (scalar_bit(s_scalar, i +  51) << 1)
            +      (scalar_bit(s_scalar, i + 102) << 2)
            +      (scalar_bit(s_scalar, i + 153) << 3)
            +      (scalar_bit(s_scalar, i + 204) << 4);
        u8 high  = teeth >> 4;
        u8 index = (teeth ^ (high - 1)) & 15;
        FOR (j, 0, 16) {
            i32 select = 1 & (((j ^ index) - 1) >> 8);
            fe_ccopy(yp, comb_Yp[j], select);
            fe_ccopy(ym, comb_Ym[j], select);
            fe_ccopy(t2, comb_T2[j], select);
        }

        fe_neg(n2, t2);
        fe_cswap(t2, n2, high);
        fe_cswap(yp, ym, high);
        ge_madd(p, p, ym, yp, n2, a, b);
    }
    WIPE_CTX(&dbl);
    WIPE_BUFFER(a);  WIPE_BUFFER(yp);  WIPE_BUFFER(t2);
    WIPE_BUFFER(b);  WIPE_BUFFER(ym);  WIPE_BUFFER(n2);
    WIPE_BUFFER(s_scalar);
}

void crypto_sign_public_key(u8       public_key[32],
                            const u8 secret_key[32])
{
    u8 a[64];
    HASH(a, secret_key, 32);
    trim_scalar(a);
    ge A;
    ge_scalarmult_base(&A, a);
    ge_tobytes(public_key, &A);
    WIPE_BUFFER(a);
    WIPE_CTX(&A);
}

void crypto_sign_init_first_pass(crypto_sign_ctx *ctx,
                                 const u8  secret_key[32],
                                 const u8  public_key[32])
{
    u8 *a      = ctx->buf;
    u8 *prefix = ctx->buf + 32;
    HASH(a, secret_key, 32);
    trim_scalar(a);

    if (public_key == 0) {
        crypto_sign_public_key(ctx->pk, secret_key);
    } else {
        FOR (i, 0, 32) {
            ctx->pk[i] = public_key[i];
        }
    }

    // Constructs the "random" nonce from the secret key and message.
    // An actual random number would work just fine, and would save us
    // the trouble of hashing the message twice.  If we did that
    // however, the user could fuck it up and reuse the nonce.
    HASH_INIT  (&ctx->hash);
    HASH_UPDATE(&ctx->hash, prefix , 32);
}

void crypto_sign_update(crypto_sign_ctx *ctx, const u8 *msg, size_t msg_size)
{
    HASH_UPDATE(&ctx->hash, msg, msg_size);
}

void crypto_sign_init_second_pass(crypto_sign_ctx *ctx)
{
    u8 *r        = ctx->buf + 32;
    u8 *half_sig = ctx->buf + 64;
    HASH_FINAL(&ctx->hash, r);
    reduce(r);

    // first half of the signature = "random" nonce times basepoint
    ge R;
    ge_scalarmult_base(&R, r);
    ge_tobytes(half_sig, &R);
    WIPE_CTX(&R);

    // Hash R, the public key, and the message together.
    // It cannot be done in parallel with the first hash.
    HASH_INIT  (&ctx->hash);
    HASH_UPDATE(&ctx->hash, half_sig, 32);
    HASH_UPDATE(&ctx->hash, ctx->pk , 32);
}

void crypto_sign_final(crypto_sign_ctx *ctx, u8 signature[64])
{
    u8 *a        = ctx->buf;
    u8 *r        = ctx->buf + 32;
    u8 *half_sig = ctx->buf + 64;
    u8 h_ram[64];
    HASH_FINAL(&ctx->hash, h_ram);
    reduce(h_ram);  // reduce the hash modulo L
    FOR (i, 0, 32) {
        signature[i] = half_sig[i];
    }
    mul_add(signature + 32, h_ram, a, r); // s = h_ram * a + r
    WIPE_CTX(ctx);
    WIPE_BUFFER(h_ram);
}

void crypto_sign(u8        signature[64],
                 const u8  secret_key[32],
                 const u8  public_key[32],
                 const u8 *message, size_t message_size)
{
    crypto_sign_ctx ctx;
    crypto_sign_init_first_pass (&ctx, secret_key, public_key);
    crypto_sign_update          (&ctx, message, message_size);
    crypto_sign_init_second_pass(&ctx);
    crypto_sign_update          (&ctx, message, message_size);
    crypto_sign_final           (&ctx, signature);
}

void crypto_check_init(crypto_check_ctx *ctx,
                      const u8 signature[64],
                      const u8 public_key[32])
{
    FOR (i, 0, 64) { ctx->sig[i] = signature [i]; }
    FOR (i, 0, 32) { ctx->pk [i] = public_key[i]; }
    HASH_INIT  (&ctx->hash);
    HASH_UPDATE(&ctx->hash, signature , 32);
    HASH_UPDATE(&ctx->hash, public_key, 32);
}

void crypto_check_update(crypto_check_ctx *ctx, const u8 *msg, size_t msg_size)
{
    HASH_UPDATE(&ctx->hash, msg , msg_size);
}

int crypto_check_final(crypto_check_ctx *ctx)
{
    ge diff, A;
    u8 h_ram[64], R_check[32];
    u8 *s = ctx->sig + 32;                       // s
    u8 *R = ctx->sig;                            // R
    if (ge_frombytes_neg_vartime(&A, ctx->pk) ||
        is_above_L(s)) { // prevent s malleability
        return -1;
    }
    HASH_FINAL(&ctx->hash, h_ram);
    reduce(h_ram);
    ge_double_scalarmult_vartime(&diff, &A, h_ram, s);
    ge_tobytes(R_check, &diff);                  // R_check = s*B - h_ram*A
    return crypto_verify32(R, R_check);          // R == R_check ? OK : fail
    // No secret, no wipe
}

int crypto_check(const u8  signature[64],
                 const u8  public_key[32],
                 const u8 *message, size_t message_size)
{
    crypto_check_ctx ctx;
    crypto_check_init(&ctx, signature, public_key);
    crypto_check_update(&ctx, message, message_size);
    return crypto_check_final(&ctx);
}

////////////////////
/// Key exchange ///
////////////////////
int crypto_key_exchange(u8       shared_key[32],
                        const u8 your_secret_key [32],
                        const u8 their_public_key[32])
{
    int status = crypto_x25519(shared_key, your_secret_key, their_public_key);
    crypto_chacha20_H(shared_key, shared_key, zero);
    return status;
}

////////////////////////////////
/// Authenticated encryption ///
////////////////////////////////
static void lock_ad_padding(crypto_lock_ctx *ctx)
{
    if (ctx->ad_phase) {
        ctx->ad_phase = 0;
        crypto_poly1305_update(&ctx->poly, zero, ALIGN(ctx->ad_size, 16));
    }
}

void crypto_lock_init(crypto_lock_ctx *ctx,
                      const u8 key[32], const u8 nonce[24])
{
    u8 auth_key[64]; // "Wasting" the whole Chacha block is faster
    ctx->ad_phase     = 1;
    ctx->ad_size      = 0;
    ctx->message_size = 0;
    crypto_chacha20_x_init(&ctx->chacha, key, nonce);
    crypto_chacha20_stream(&ctx->chacha, auth_key, 64);
    crypto_poly1305_init  (&ctx->poly  , auth_key);
    WIPE_BUFFER(auth_key);
}

void crypto_lock_auth_ad(crypto_lock_ctx *ctx, const u8 *msg, size_t msg_size)
{
    crypto_poly1305_update(&ctx->poly, msg, msg_size);
    ctx->ad_size += msg_size;
}

void crypto_lock_auth_message(crypto_lock_ctx *ctx,
                              const u8 *cipher_text, size_t text_size)
{
    lock_ad_padding(ctx);
    ctx->message_size += text_size;
    crypto_poly1305_update(&ctx->poly, cipher_text, text_size);
}

void crypto_lock_update(crypto_lock_ctx *ctx, u8 *cipher_text,
                        const u8 *plain_text, size_t text_size)
{
    crypto_chacha20_encrypt(&ctx->chacha, cipher_text, plain_text, text_size);
    crypto_lock_auth_message(ctx, cipher_text, text_size);
}

void crypto_lock_final(crypto_lock_ctx *ctx, u8 mac[16])
{
    lock_ad_padding(ctx);
    u8 sizes[16]; // Not secret, not wiped
    store64_le(sizes + 0, ctx->ad_size);
    store64_le(sizes + 8, ctx->message_size);
    crypto_poly1305_update(&ctx->poly, zero, ALIGN(ctx->message_size, 16));
    crypto_poly1305_update(&ctx->poly, sizes, 16);
    crypto_poly1305_final (&ctx->poly, mac);
    WIPE_CTX(ctx);
}

void crypto_unlock_update(crypto_lock_ctx *ctx, u8 *plain_text,
                          const u8 *cipher_text, size_t text_size)
{
    crypto_unlock_auth_message(ctx, cipher_text, text_size);
    crypto_chacha20_encrypt(&ctx->chacha, plain_text, cipher_text, text_size);
}

int crypto_unlock_final(crypto_lock_ctx *ctx, const u8 mac[16])
{
    u8 real_mac[16];
    crypto_lock_final(ctx, real_mac);
    int mismatch = crypto_verify16(real_mac, mac);
    WIPE_BUFFER(real_mac);
    return mismatch;
}

void crypto_lock_aead(u8        mac[16],
                      u8       *cipher_text,
                      const u8  key[32],
                      const u8  nonce[24],
                      const u8 *ad        , size_t ad_size,
                      const u8 *plain_text, size_t text_size)
{
    crypto_lock_ctx ctx;
    crypto_lock_init   (&ctx, key, nonce);
    crypto_lock_auth_ad(&ctx, ad, ad_size);
    crypto_lock_update (&ctx, cipher_text, plain_text, text_size);
    crypto_lock_final  (&ctx, mac);
}

int crypto_unlock_aead(u8       *plain_text,
                       const u8  key[32],
                       const u8  nonce[24],
                       const u8  mac[16],
                       const u8 *ad         , size_t ad_size,
                       const u8 *cipher_text, size_t text_size)
{
    crypto_unlock_ctx ctx;
    crypto_unlock_init        (&ctx, key, nonce);
    crypto_unlock_auth_ad     (&ctx, ad, ad_size);
    crypto_unlock_auth_message(&ctx, cipher_text, text_size);
    crypto_chacha_ctx chacha_ctx = ctx.chacha; // avoid the wiping...
    if (crypto_unlock_final(&ctx, mac)) {      // ...that occurs here
        WIPE_CTX(&chacha_ctx);
        return -1; // reject forgeries before wasting our time decrypting
    }
    crypto_chacha20_encrypt(&chacha_ctx, plain_text, cipher_text, text_size);
    WIPE_CTX(&chacha_ctx);
    return 0;
}

void crypto_lock(u8        mac[16],
                 u8       *cipher_text,
                 const u8  key[32],
                 const u8  nonce[24],
                 const u8 *plain_text, size_t text_size)
{
    crypto_lock_aead(mac, cipher_text, key, nonce, 0, 0, plain_text, text_size);
}

int crypto_unlock(u8       *plain_text,
                  const u8  key[32],
                  const u8  nonce[24],
                  const u8  mac[16],
                  const u8 *cipher_text, size_t text_size)
{
    return crypto_unlock_aead(plain_text, key, nonce, mac, 0, 0,
                              cipher_text, text_size);
}

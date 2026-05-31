
// ── Crypto Wrappers (xtd.crypto) ────────────────────────────
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/bn.h>
#include <openssl/rand.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>
#include <openssl/err.h>
#include <openssl/kdf.h>

// Opaque types wrapping OpenSSL handles
typedef struct {
    EVP_CIPHER_CTX* ctx;
} GorgetCipherContext;

typedef struct {
    BIGNUM* bn;
} GorgetBigNum;

typedef struct {
    EVP_PKEY* pkey;
} GorgetRSAKey;

// SHA-256 one-shot hash
static GorgetArray gorget_crypto_sha256(const GorgetArray* data) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    out.data = GORGET_ALLOC(32);
    out.cap = 32;
    out.len = 32;
    unsigned int md_len = 32;
    EVP_MD_CTX* ctx = EVP_MD_CTX_new();
    EVP_DigestInit_ex(ctx, EVP_sha256(), NULL);
    EVP_DigestUpdate(ctx, data->data, data->len);
    EVP_DigestFinal_ex(ctx, (unsigned char*)out.data, &md_len);
    EVP_MD_CTX_free(ctx);
    return out;
}

// SHA-1 one-shot hash
static GorgetArray gorget_crypto_sha1(const GorgetArray* data) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    out.data = GORGET_ALLOC(20);
    out.cap = 20;
    out.len = 20;
    unsigned int md_len = 20;
    EVP_MD_CTX* ctx = EVP_MD_CTX_new();
    EVP_DigestInit_ex(ctx, EVP_sha1(), NULL);
    EVP_DigestUpdate(ctx, data->data, data->len);
    EVP_DigestFinal_ex(ctx, (unsigned char*)out.data, &md_len);
    EVP_MD_CTX_free(ctx);
    return out;
}

// Crypto error infrastructure (used by Result-returning crypto functions)
static const char* __gorget_crypto_last_error = NULL;

static const char* gorget_crypto_last_error(void) {
    return __gorget_crypto_last_error;
}

// HMAC (supports "sha256" and "sha1")
static GorgetArray gorget_crypto_hmac(const char* algo, const GorgetArray* key, const GorgetArray* data) {
    __gorget_crypto_last_error = NULL;
    const EVP_MD* md = NULL;
    if (strcmp(algo, "sha256") == 0) md = EVP_sha256();
    else if (strcmp(algo, "sha1") == 0) md = EVP_sha1();
    else {
        __gorget_crypto_last_error = "unsupported HMAC algorithm";
        return gorget_array_new(sizeof(uint8_t));
    }

    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    unsigned int md_len = (unsigned int)EVP_MD_size(md);
    out.data = GORGET_ALLOC(md_len);
    out.cap = md_len;
    out.len = md_len;

    unsigned char* result = HMAC(md,
        key->data, (int)key->len,
        (unsigned char*)data->data, data->len,
        (unsigned char*)out.data, &md_len);
    (void)result;
    out.len = md_len;
    return out;
}

// AES-CTR cipher context
static GorgetCipherContext gorget_crypto_aes_ctr_new(const GorgetArray* key, const GorgetArray* iv) {
    __gorget_crypto_last_error = NULL;
    const EVP_CIPHER* cipher = NULL;
    if (key->len == 16) cipher = EVP_aes_128_ctr();
    else if (key->len == 24) cipher = EVP_aes_192_ctr();
    else if (key->len == 32) cipher = EVP_aes_256_ctr();
    else {
        __gorget_crypto_last_error = "AES key must be 16, 24, or 32 bytes";
        return (GorgetCipherContext){NULL};
    }
    EVP_CIPHER_CTX* ctx = EVP_CIPHER_CTX_new();
    if (!ctx) {
        __gorget_crypto_last_error = "failed to create cipher context";
        return (GorgetCipherContext){NULL};
    }
    if (!EVP_EncryptInit_ex(ctx, cipher, NULL, (unsigned char*)key->data, (unsigned char*)iv->data)) {
        EVP_CIPHER_CTX_free(ctx);
        __gorget_crypto_last_error = "failed to initialize cipher";
        return (GorgetCipherContext){NULL};
    }
    return (GorgetCipherContext){ctx};
}

// AES-CTR encrypt (CTR mode: encrypt == decrypt)
static GorgetArray gorget_cipher_encrypt(GorgetCipherContext* c, const GorgetArray* plaintext) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    int out_len = (int)plaintext->len + 16;
    out.data = GORGET_ALLOC((size_t)out_len);
    out.cap = (size_t)out_len;
    EVP_EncryptUpdate(c->ctx, (unsigned char*)out.data, &out_len,
        (unsigned char*)plaintext->data, (int)plaintext->len);
    out.len = (size_t)out_len;
    return out;
}

// AES-CTR decrypt (same as encrypt for CTR mode)
static GorgetArray gorget_cipher_decrypt(GorgetCipherContext* c, const GorgetArray* ciphertext) {
    return gorget_cipher_encrypt(c, ciphertext);
}

// BigNum from bytes (big-endian unsigned)
static GorgetBigNum gorget_crypto_bn_from_bytes(const GorgetArray* data) {
    BIGNUM* bn = BN_bin2bn((unsigned char*)data->data, (int)data->len, NULL);
    return (GorgetBigNum){bn};
}

// BigNum to bytes (big-endian unsigned)
static GorgetArray gorget_crypto_bn_to_bytes(const GorgetBigNum* bn) {
    int n = BN_num_bytes(bn->bn);
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    out.data = GORGET_ALLOC((size_t)n);
    out.cap = (size_t)n;
    out.len = (size_t)n;
    BN_bn2bin(bn->bn, (unsigned char*)out.data);
    return out;
}

// Modular exponentiation: base^exp mod modulus
static GorgetBigNum gorget_crypto_bn_mod_exp(const GorgetBigNum* base, const GorgetBigNum* exp, const GorgetBigNum* modulus) {
    BIGNUM* result = BN_new();
    BN_CTX* ctx = BN_CTX_new();
    BN_mod_exp(result, base->bn, exp->bn, modulus->bn, ctx);
    BN_CTX_free(ctx);
    return (GorgetBigNum){result};
}

// Load RSA public key from DER-encoded bytes
static GorgetRSAKey gorget_crypto_rsa_load_public(const GorgetArray* key_bytes) {
    __gorget_crypto_last_error = NULL;
    const unsigned char* p = (const unsigned char*)key_bytes->data;
    EVP_PKEY* pkey = d2i_PUBKEY(NULL, &p, (long)key_bytes->len);
    if (!pkey) {
        __gorget_crypto_last_error = "failed to load RSA public key";
        return (GorgetRSAKey){NULL};
    }
    return (GorgetRSAKey){pkey};
}

// RSA signature verification (PKCS#1 v1.5 with SHA-256)
static bool gorget_crypto_rsa_verify(const GorgetRSAKey* key, const GorgetArray* data, const GorgetArray* sig) {
    if (!key->pkey) return false;
    EVP_MD_CTX* ctx = EVP_MD_CTX_new();
    EVP_DigestVerifyInit(ctx, NULL, EVP_sha256(), NULL, key->pkey);
    EVP_DigestVerifyUpdate(ctx, data->data, data->len);
    int ok = EVP_DigestVerifyFinal(ctx, (unsigned char*)sig->data, sig->len);
    EVP_MD_CTX_free(ctx);
    return ok == 1;
}

// Cryptographically secure random bytes (via OpenSSL RAND)
static GorgetArray gorget_crypto_random_bytes(int64_t n) {
    __gorget_crypto_last_error = NULL;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (n <= 0) return out;
    out.data = GORGET_ALLOC((size_t)n);
    out.cap = (size_t)n;
    out.len = (size_t)n;
    if (RAND_bytes((unsigned char*)out.data, (int)n) != 1) {
        GORGET_FREE(out.data, 0);
        out.data = NULL;
        out.cap = 0;
        out.len = 0;
        __gorget_crypto_last_error = "RAND_bytes failed";
        return out;
    }
    return out;
}

// ── Ed25519 ─────────────────────────────────────────────────
typedef struct {
    EVP_PKEY* pkey;
} GorgetEd25519KeyPair;

// Generate Ed25519 keypair
static GorgetEd25519KeyPair gorget_crypto_ed25519_keygen(void) {
    __gorget_crypto_last_error = NULL;
    EVP_PKEY* pkey = NULL;
    EVP_PKEY_CTX* ctx = EVP_PKEY_CTX_new_id(EVP_PKEY_ED25519, NULL);
    if (!ctx) {
        __gorget_crypto_last_error = "failed to create Ed25519 context";
        return (GorgetEd25519KeyPair){NULL};
    }
    if (EVP_PKEY_keygen_init(ctx) <= 0 || EVP_PKEY_keygen(ctx, &pkey) <= 0) {
        EVP_PKEY_CTX_free(ctx);
        __gorget_crypto_last_error = "Ed25519 key generation failed";
        return (GorgetEd25519KeyPair){NULL};
    }
    EVP_PKEY_CTX_free(ctx);
    return (GorgetEd25519KeyPair){pkey};
}

// Extract 32-byte public key
static GorgetArray gorget_ed25519_public_key(const GorgetEd25519KeyPair* kp) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (!kp->pkey) return out;
    size_t len = 32;
    out.data = GORGET_ALLOC(32);
    out.cap = 32;
    EVP_PKEY_get_raw_public_key(kp->pkey, (unsigned char*)out.data, &len);
    out.len = (int64_t)len;
    return out;
}

// Extract 64-byte private key (Ed25519 seed is 32 bytes, but we store as 64 = seed + public)
static GorgetArray gorget_ed25519_private_key(const GorgetEd25519KeyPair* kp) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (!kp->pkey) return out;
    // Get 32-byte seed
    size_t seed_len = 32;
    uint8_t seed[32];
    EVP_PKEY_get_raw_private_key(kp->pkey, seed, &seed_len);
    // Get 32-byte public key
    size_t pub_len = 32;
    uint8_t pub_key[32];
    EVP_PKEY_get_raw_public_key(kp->pkey, pub_key, &pub_len);
    // Concatenate seed + public = 64 bytes
    out.data = GORGET_ALLOC(64);
    out.cap = 64;
    out.len = 64;
    memcpy(out.data, seed, 32);
    memcpy((uint8_t*)out.data + 32, pub_key, 32);
    return out;
}

// Sign data with Ed25519 private key (given as 64-byte seed+public or 32-byte seed)
static GorgetArray gorget_crypto_ed25519_sign(const GorgetArray* private_key, const GorgetArray* data) {
    __gorget_crypto_last_error = NULL;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (private_key->len < 32) {
        __gorget_crypto_last_error = "Ed25519 private key too short";
        return out;
    }
    // Use first 32 bytes as seed
    EVP_PKEY* pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_ED25519, NULL,
        (const unsigned char*)private_key->data, 32);
    if (!pkey) {
        __gorget_crypto_last_error = "invalid Ed25519 private key";
        return out;
    }
    EVP_MD_CTX* ctx = EVP_MD_CTX_new();
    size_t sig_len = 64;
    out.data = GORGET_ALLOC(64);
    out.cap = 64;
    if (EVP_DigestSignInit(ctx, NULL, NULL, NULL, pkey) <= 0 ||
        EVP_DigestSign(ctx, (unsigned char*)out.data, &sig_len,
            (const unsigned char*)data->data, (size_t)data->len) <= 0) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(pkey);
        GORGET_FREE(out.data, 0);
        out.data = NULL;
        out.cap = 0;
        __gorget_crypto_last_error = "Ed25519 signing failed";
        return out;
    }
    out.len = (int64_t)sig_len;
    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(pkey);
    return out;
}

// Verify Ed25519 signature
static bool gorget_crypto_ed25519_verify(const GorgetArray* public_key, const GorgetArray* data, const GorgetArray* signature) {
    if (public_key->len != 32 || signature->len != 64) return false;
    EVP_PKEY* pkey = EVP_PKEY_new_raw_public_key(EVP_PKEY_ED25519, NULL,
        (const unsigned char*)public_key->data, 32);
    if (!pkey) return false;
    EVP_MD_CTX* ctx = EVP_MD_CTX_new();
    int ok = 0;
    if (EVP_DigestVerifyInit(ctx, NULL, NULL, NULL, pkey) > 0) {
        ok = EVP_DigestVerify(ctx, (const unsigned char*)signature->data, (size_t)signature->len,
            (const unsigned char*)data->data, (size_t)data->len);
    }
    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(pkey);
    return ok == 1;
}

// ── X25519 ECDH ─────────────────────────────────────────────
typedef struct {
    EVP_PKEY* pkey;
} GorgetX25519KeyPair;

// Generate ephemeral X25519 keypair
static GorgetX25519KeyPair gorget_crypto_x25519_keygen(void) {
    __gorget_crypto_last_error = NULL;
    EVP_PKEY* pkey = NULL;
    EVP_PKEY_CTX* ctx = EVP_PKEY_CTX_new_id(EVP_PKEY_X25519, NULL);
    if (!ctx) {
        __gorget_crypto_last_error = "failed to create X25519 context";
        return (GorgetX25519KeyPair){NULL};
    }
    if (EVP_PKEY_keygen_init(ctx) <= 0 || EVP_PKEY_keygen(ctx, &pkey) <= 0) {
        EVP_PKEY_CTX_free(ctx);
        __gorget_crypto_last_error = "X25519 key generation failed";
        return (GorgetX25519KeyPair){NULL};
    }
    EVP_PKEY_CTX_free(ctx);
    return (GorgetX25519KeyPair){pkey};
}

// Extract 32-byte public key from X25519 keypair
static GorgetArray gorget_crypto_x25519_public(const GorgetX25519KeyPair* kp) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (!kp->pkey) return out;
    size_t len = 32;
    out.data = GORGET_ALLOC(32);
    out.cap = 32;
    EVP_PKEY_get_raw_public_key(kp->pkey, (unsigned char*)out.data, &len);
    out.len = (int64_t)len;
    return out;
}

// Extract 32-byte raw private key from X25519 keypair
static GorgetArray gorget_crypto_x25519_private(const GorgetX25519KeyPair* kp) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (!kp->pkey) return out;
    size_t len = 32;
    out.data = GORGET_ALLOC(32);
    out.cap = 32;
    EVP_PKEY_get_raw_private_key(kp->pkey, (unsigned char*)out.data, &len);
    out.len = (int64_t)len;
    return out;
}

// Compute shared secret from raw 32-byte private key bytes + raw 32-byte peer public key
static GorgetArray gorget_crypto_x25519_dh(const GorgetArray* private_key_bytes, const GorgetArray* peer_public) {
    __gorget_crypto_last_error = NULL;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (private_key_bytes->len != 32 || peer_public->len != 32) {
        __gorget_crypto_last_error = "X25519 keys must be 32 bytes";
        return out;
    }
    EVP_PKEY* our_pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_X25519, NULL,
        (const unsigned char*)private_key_bytes->data, 32);
    if (!our_pkey) {
        __gorget_crypto_last_error = "failed to load X25519 private key";
        return out;
    }
    EVP_PKEY* peer_pkey = EVP_PKEY_new_raw_public_key(EVP_PKEY_X25519, NULL,
        (const unsigned char*)peer_public->data, 32);
    if (!peer_pkey) {
        EVP_PKEY_free(our_pkey);
        __gorget_crypto_last_error = "failed to load peer X25519 public key";
        return out;
    }
    EVP_PKEY_CTX* ctx = EVP_PKEY_CTX_new(our_pkey, NULL);
    if (!ctx || EVP_PKEY_derive_init(ctx) <= 0 || EVP_PKEY_derive_set_peer(ctx, peer_pkey) <= 0) {
        if (ctx) EVP_PKEY_CTX_free(ctx);
        EVP_PKEY_free(our_pkey);
        EVP_PKEY_free(peer_pkey);
        __gorget_crypto_last_error = "X25519 derive init failed";
        return out;
    }
    size_t secret_len = 32;
    out.data = GORGET_ALLOC(32);
    out.cap = 32;
    if (EVP_PKEY_derive(ctx, (unsigned char*)out.data, &secret_len) <= 0) {
        GORGET_FREE(out.data, 0);
        out.data = NULL;
        out.cap = 0;
        EVP_PKEY_CTX_free(ctx);
        EVP_PKEY_free(our_pkey);
        EVP_PKEY_free(peer_pkey);
        __gorget_crypto_last_error = "X25519 key derivation failed";
        return out;
    }
    out.len = (int64_t)secret_len;
    EVP_PKEY_CTX_free(ctx);
    EVP_PKEY_free(our_pkey);
    EVP_PKEY_free(peer_pkey);
    return out;
}

// Compute 32-byte shared secret from our X25519 keypair + peer's raw 32-byte public key
static GorgetArray gorget_crypto_x25519_shared_secret(const GorgetX25519KeyPair* private_key, const GorgetArray* peer_public) {
    __gorget_crypto_last_error = NULL;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (!private_key->pkey || peer_public->len != 32) {
        __gorget_crypto_last_error = "invalid X25519 key material";
        return out;
    }
    EVP_PKEY* peer_pkey = EVP_PKEY_new_raw_public_key(EVP_PKEY_X25519, NULL,
        (const unsigned char*)peer_public->data, 32);
    if (!peer_pkey) {
        __gorget_crypto_last_error = "failed to load peer X25519 public key";
        return out;
    }
    EVP_PKEY_CTX* ctx = EVP_PKEY_CTX_new(private_key->pkey, NULL);
    if (!ctx || EVP_PKEY_derive_init(ctx) <= 0 || EVP_PKEY_derive_set_peer(ctx, peer_pkey) <= 0) {
        if (ctx) EVP_PKEY_CTX_free(ctx);
        EVP_PKEY_free(peer_pkey);
        __gorget_crypto_last_error = "X25519 derive init failed";
        return out;
    }
    size_t secret_len = 32;
    out.data = GORGET_ALLOC(32);
    out.cap = 32;
    if (EVP_PKEY_derive(ctx, (unsigned char*)out.data, &secret_len) <= 0) {
        GORGET_FREE(out.data, 0);
        out.data = NULL;
        out.cap = 0;
        EVP_PKEY_CTX_free(ctx);
        EVP_PKEY_free(peer_pkey);
        __gorget_crypto_last_error = "X25519 key derivation failed";
        return out;
    }
    out.len = (int64_t)secret_len;
    EVP_PKEY_CTX_free(ctx);
    EVP_PKEY_free(peer_pkey);
    return out;
}

// ── HKDF-SHA256 ─────────────────────────────────────────────
static GorgetArray gorget_crypto_hkdf_sha256(const GorgetArray* salt, const GorgetArray* ikm, const GorgetArray* info, int64_t length) {
    __gorget_crypto_last_error = NULL;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (length <= 0 || length > 255 * 32) {
        __gorget_crypto_last_error = "HKDF output length out of range";
        return out;
    }
    EVP_PKEY_CTX* ctx = EVP_PKEY_CTX_new_id(EVP_PKEY_HKDF, NULL);
    if (!ctx) {
        __gorget_crypto_last_error = "failed to create HKDF context";
        return out;
    }
    if (EVP_PKEY_derive_init(ctx) <= 0 ||
        EVP_PKEY_CTX_set_hkdf_md(ctx, EVP_sha256()) <= 0 ||
        EVP_PKEY_CTX_set1_hkdf_salt(ctx, (const unsigned char*)salt->data, (int)salt->len) <= 0 ||
        EVP_PKEY_CTX_set1_hkdf_key(ctx, (const unsigned char*)ikm->data, (int)ikm->len) <= 0 ||
        EVP_PKEY_CTX_add1_hkdf_info(ctx, (const unsigned char*)info->data, (int)info->len) <= 0) {
        EVP_PKEY_CTX_free(ctx);
        __gorget_crypto_last_error = "HKDF parameter setup failed";
        return out;
    }
    size_t out_len = (size_t)length;
    out.data = GORGET_ALLOC(out_len);
    out.cap = (int64_t)out_len;
    if (EVP_PKEY_derive(ctx, (unsigned char*)out.data, &out_len) <= 0) {
        GORGET_FREE(out.data, 0);
        out.data = NULL;
        out.cap = 0;
        EVP_PKEY_CTX_free(ctx);
        __gorget_crypto_last_error = "HKDF derivation failed";
        return out;
    }
    out.len = (int64_t)out_len;
    EVP_PKEY_CTX_free(ctx);
    return out;
}

// ── AES-256-GCM AEAD ────────────────────────────────────────

// Encrypt: key (32B), nonce (12B), plaintext → returns [12B nonce | ciphertext | 16B tag]
static GorgetArray gorget_crypto_aes_gcm_encrypt(const GorgetArray* key, const GorgetArray* nonce, const GorgetArray* plaintext) {
    __gorget_crypto_last_error = NULL;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (key->len != 32) { __gorget_crypto_last_error = "AES-GCM key must be 32 bytes"; return out; }
    if (nonce->len != 12) { __gorget_crypto_last_error = "AES-GCM nonce must be 12 bytes"; return out; }

    EVP_CIPHER_CTX* ctx = EVP_CIPHER_CTX_new();
    if (!ctx) { __gorget_crypto_last_error = "failed to create cipher context"; return out; }

    if (EVP_EncryptInit_ex(ctx, EVP_aes_256_gcm(), NULL, NULL, NULL) != 1 ||
        EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, 12, NULL) != 1 ||
        EVP_EncryptInit_ex(ctx, NULL, NULL, (const unsigned char*)key->data, (const unsigned char*)nonce->data) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        __gorget_crypto_last_error = "AES-GCM encrypt init failed";
        return out;
    }

    // Output: 12B nonce + ciphertext (same len as plaintext) + 16B tag
    size_t total = 12 + (size_t)plaintext->len + 16;
    out.data = GORGET_ALLOC(total);
    out.cap = (int64_t)total;

    // Copy nonce to output
    memcpy(out.data, nonce->data, 12);

    int ct_len = 0;
    if (EVP_EncryptUpdate(ctx, (unsigned char*)out.data + 12, &ct_len,
            (const unsigned char*)plaintext->data, (int)plaintext->len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        GORGET_FREE(out.data, 0); out.data = NULL; out.cap = 0;
        __gorget_crypto_last_error = "AES-GCM encrypt update failed";
        return out;
    }

    int final_len = 0;
    if (EVP_EncryptFinal_ex(ctx, (unsigned char*)out.data + 12 + ct_len, &final_len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        GORGET_FREE(out.data, 0); out.data = NULL; out.cap = 0;
        __gorget_crypto_last_error = "AES-GCM encrypt final failed";
        return out;
    }
    ct_len += final_len;

    // Append 16-byte tag
    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, 16, (unsigned char*)out.data + 12 + ct_len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        GORGET_FREE(out.data, 0); out.data = NULL; out.cap = 0;
        __gorget_crypto_last_error = "AES-GCM get tag failed";
        return out;
    }

    out.len = (int64_t)(12 + ct_len + 16);
    EVP_CIPHER_CTX_free(ctx);
    return out;
}

// Decrypt: key (32B), ciphertext ([12B nonce | ct | 16B tag]) → plaintext
static GorgetArray gorget_crypto_aes_gcm_decrypt(const GorgetArray* key, const GorgetArray* ciphertext) {
    __gorget_crypto_last_error = NULL;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (key->len != 32) { __gorget_crypto_last_error = "AES-GCM key must be 32 bytes"; return out; }
    if (ciphertext->len < 28) { __gorget_crypto_last_error = "AES-GCM ciphertext too short"; return out; }

    const unsigned char* nonce = (const unsigned char*)ciphertext->data;
    int ct_len = (int)ciphertext->len - 12 - 16;
    const unsigned char* ct = (const unsigned char*)ciphertext->data + 12;
    const unsigned char* tag = (const unsigned char*)ciphertext->data + 12 + ct_len;

    EVP_CIPHER_CTX* ctx = EVP_CIPHER_CTX_new();
    if (!ctx) { __gorget_crypto_last_error = "failed to create cipher context"; return out; }

    if (EVP_DecryptInit_ex(ctx, EVP_aes_256_gcm(), NULL, NULL, NULL) != 1 ||
        EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, 12, NULL) != 1 ||
        EVP_DecryptInit_ex(ctx, NULL, NULL, (const unsigned char*)key->data, nonce) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        __gorget_crypto_last_error = "AES-GCM decrypt init failed";
        return out;
    }

    out.data = GORGET_ALLOC((size_t)ct_len);
    out.cap = (int64_t)ct_len;

    int pt_len = 0;
    if (EVP_DecryptUpdate(ctx, (unsigned char*)out.data, &pt_len, ct, ct_len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        GORGET_FREE(out.data, 0); out.data = NULL; out.cap = 0;
        __gorget_crypto_last_error = "AES-GCM decrypt update failed";
        return out;
    }

    // Set expected tag
    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, 16, (void*)tag) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        GORGET_FREE(out.data, 0); out.data = NULL; out.cap = 0;
        __gorget_crypto_last_error = "AES-GCM set tag failed";
        return out;
    }

    int final_len = 0;
    if (EVP_DecryptFinal_ex(ctx, (unsigned char*)out.data + pt_len, &final_len) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        GORGET_FREE(out.data, 0); out.data = NULL; out.cap = 0;
        __gorget_crypto_last_error = "AES-GCM authentication failed";
        return out;
    }
    pt_len += final_len;

    out.len = (int64_t)pt_len;
    EVP_CIPHER_CTX_free(ctx);
    return out;
}


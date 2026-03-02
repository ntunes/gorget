/// Sim crypto — pure-Rust crypto primitives for `gg sim`.
///
/// These are real but not constant-time or hardened implementations,
/// suitable only for interpreter use (testing / reference execution).

// ── SHA-256 ───────────────────────────────────────────────────

const SHA256_K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

pub fn sha256(data: &[u8]) -> [u8; 32] {
    let mut h: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
    ];

    // Pre-processing: adding padding bits
    let bit_len = (data.len() as u64).wrapping_mul(8);
    let mut msg: Vec<u8> = data.to_vec();
    msg.push(0x80);
    while msg.len() % 64 != 56 {
        msg.push(0x00);
    }
    msg.extend_from_slice(&bit_len.to_be_bytes());

    for chunk in msg.chunks(64) {
        let mut w = [0u32; 64];
        for i in 0..16 {
            w[i] = u32::from_be_bytes([chunk[i*4], chunk[i*4+1], chunk[i*4+2], chunk[i*4+3]]);
        }
        for i in 16..64 {
            let s0 = w[i-15].rotate_right(7) ^ w[i-15].rotate_right(18) ^ (w[i-15] >> 3);
            let s1 = w[i-2].rotate_right(17) ^ w[i-2].rotate_right(19) ^ (w[i-2] >> 10);
            w[i] = w[i-16].wrapping_add(s0).wrapping_add(w[i-7]).wrapping_add(s1);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut hh] =
            [h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]];
        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ ((!e) & g);
            let temp1 = hh.wrapping_add(s1).wrapping_add(ch).wrapping_add(SHA256_K[i]).wrapping_add(w[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = s0.wrapping_add(maj);
            hh = g; g = f; f = e;
            e = d.wrapping_add(temp1);
            d = c; c = b; b = a;
            a = temp1.wrapping_add(temp2);
        }
        h[0] = h[0].wrapping_add(a); h[1] = h[1].wrapping_add(b);
        h[2] = h[2].wrapping_add(c); h[3] = h[3].wrapping_add(d);
        h[4] = h[4].wrapping_add(e); h[5] = h[5].wrapping_add(f);
        h[6] = h[6].wrapping_add(g); h[7] = h[7].wrapping_add(hh);
    }

    let mut out = [0u8; 32];
    for (i, &v) in h.iter().enumerate() {
        out[i*4..i*4+4].copy_from_slice(&v.to_be_bytes());
    }
    out
}

// ── SHA-1 ─────────────────────────────────────────────────────

pub fn sha1(data: &[u8]) -> [u8; 20] {
    let mut h: [u32; 5] = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0];

    let bit_len = (data.len() as u64).wrapping_mul(8);
    let mut msg: Vec<u8> = data.to_vec();
    msg.push(0x80);
    while msg.len() % 64 != 56 {
        msg.push(0x00);
    }
    msg.extend_from_slice(&bit_len.to_be_bytes());

    for chunk in msg.chunks(64) {
        let mut w = [0u32; 80];
        for i in 0..16 {
            w[i] = u32::from_be_bytes([chunk[i*4], chunk[i*4+1], chunk[i*4+2], chunk[i*4+3]]);
        }
        for i in 16..80 {
            w[i] = (w[i-3] ^ w[i-8] ^ w[i-14] ^ w[i-16]).rotate_left(1);
        }
        let (mut a, mut b, mut c, mut d, mut e) = (h[0], h[1], h[2], h[3], h[4]);
        for i in 0..80 {
            let (f, k) = match i {
                0..=19  => ((b & c) | ((!b) & d), 0x5A827999u32),
                20..=39 => (b ^ c ^ d,             0x6ED9EBA1u32),
                40..=59 => ((b & c) | (b & d) | (c & d), 0x8F1BBCDCu32),
                _       => (b ^ c ^ d,             0xCA62C1D6u32),
            };
            let temp = a.rotate_left(5).wrapping_add(f).wrapping_add(e).wrapping_add(k).wrapping_add(w[i]);
            e = d; d = c; c = b.rotate_left(30); b = a; a = temp;
        }
        h[0] = h[0].wrapping_add(a); h[1] = h[1].wrapping_add(b);
        h[2] = h[2].wrapping_add(c); h[3] = h[3].wrapping_add(d);
        h[4] = h[4].wrapping_add(e);
    }

    let mut out = [0u8; 20];
    for (i, &v) in h.iter().enumerate() {
        out[i*4..i*4+4].copy_from_slice(&v.to_be_bytes());
    }
    out
}

// ── HMAC-SHA256 ───────────────────────────────────────────────

pub fn hmac_sha256(key: &[u8], data: &[u8]) -> [u8; 32] {
    let mut k = [0u8; 64];
    if key.len() > 64 {
        let h = sha256(key);
        k[..32].copy_from_slice(&h);
    } else {
        k[..key.len()].copy_from_slice(key);
    }
    let mut ipad = [0x36u8; 64];
    let mut opad = [0x5cu8; 64];
    for i in 0..64 { ipad[i] ^= k[i]; opad[i] ^= k[i]; }
    let mut inner = ipad.to_vec();
    inner.extend_from_slice(data);
    let inner_hash = sha256(&inner);
    let mut outer = opad.to_vec();
    outer.extend_from_slice(&inner_hash);
    sha256(&outer)
}

// ── HKDF-SHA256 ───────────────────────────────────────────────

pub fn hkdf_sha256(salt: &[u8], ikm: &[u8], info: &[u8], length: usize) -> Vec<u8> {
    // Extract
    let effective_salt = if salt.is_empty() { vec![0u8; 32] } else { salt.to_vec() };
    let prk = hmac_sha256(&effective_salt, ikm);

    // Expand
    let n = length.div_ceil(32);
    let mut okm = Vec::with_capacity(n * 32);
    let mut t: Vec<u8> = Vec::new();
    for i in 1..=n {
        let mut data = t.clone();
        data.extend_from_slice(info);
        data.push(i as u8);
        t = hmac_sha256(&prk, &data).to_vec();
        okm.extend_from_slice(&t);
    }
    okm.truncate(length);
    okm
}

// ── Pseudo-random bytes (seeded from system time + counter) ───

static RAND_COUNTER: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

pub fn random_bytes(n: usize) -> Vec<u8> {
    let seed = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64;
    let counter = RAND_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let mut state = seed.wrapping_add(counter.wrapping_mul(0x9e3779b97f4a7c15));
    let mut out = Vec::with_capacity(n);
    while out.len() < n {
        // xorshift64*
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        let hash = sha256(&state.to_le_bytes());
        let remaining = n - out.len();
        out.extend_from_slice(&hash[..remaining.min(32)]);
    }
    out
}

// ── Fake AES-CTR (XOR keystream from SHA-256 blocks) ─────────

/// Generate a keystream of `length` bytes deterministically from key and iv.
/// Not real AES, but produces the same stream on each call with the same key/iv.
pub fn aes_ctr_keystream(key: &[u8], iv: &[u8], length: usize) -> Vec<u8> {
    let mut stream = Vec::with_capacity(length);
    let mut counter: u64 = 0;
    while stream.len() < length {
        let mut block = Vec::with_capacity(key.len() + iv.len() + 8);
        block.extend_from_slice(key);
        block.extend_from_slice(iv);
        block.extend_from_slice(&counter.to_le_bytes());
        let hash = sha256(&block);
        let remaining = length - stream.len();
        stream.extend_from_slice(&hash[..remaining.min(32)]);
        counter += 1;
    }
    stream
}

/// AES-GCM encrypt stub: prepend nonce (12 bytes), XOR payload, append 16-byte tag.
/// Format: [12B nonce] [len(plaintext) XOR keystream bytes] [16B tag=hmac_sha256(key,nonce||ct)[..16]]
pub fn aes_gcm_encrypt(key: &[u8], nonce: &[u8], plaintext: &[u8]) -> Vec<u8> {
    let nonce_fixed: [u8; 12] = if nonce.len() >= 12 {
        nonce[..12].try_into().unwrap_or([0u8; 12])
    } else {
        let mut arr = [0u8; 12];
        arr[..nonce.len()].copy_from_slice(nonce);
        arr
    };
    let ks = aes_ctr_keystream(key, &nonce_fixed, plaintext.len());
    let ct: Vec<u8> = plaintext.iter().zip(ks.iter()).map(|(a, b)| a ^ b).collect();
    // Tag: first 16 bytes of HMAC-SHA256(key, nonce || ct)
    let mut tag_input = nonce_fixed.to_vec();
    tag_input.extend_from_slice(&ct);
    let tag_full = hmac_sha256(key, &tag_input);

    let mut out = Vec::with_capacity(12 + ct.len() + 16);
    out.extend_from_slice(&nonce_fixed);
    out.extend_from_slice(&ct);
    out.extend_from_slice(&tag_full[..16]);
    out
}

/// AES-GCM decrypt stub: parse nonce (12 bytes), strip tag (16 bytes), XOR decrypt payload.
pub fn aes_gcm_decrypt(key: &[u8], ciphertext: &[u8]) -> Result<Vec<u8>, &'static str> {
    if ciphertext.len() < 28 { // 12 + 0 + 16 minimum
        return Err("ciphertext too short");
    }
    let nonce = &ciphertext[..12];
    let ct = &ciphertext[12..ciphertext.len() - 16];
    let ks = aes_ctr_keystream(key, nonce, ct.len());
    let pt: Vec<u8> = ct.iter().zip(ks.iter()).map(|(a, b)| a ^ b).collect();
    Ok(pt)
}

// ── Fake Ed25519 ──────────────────────────────────────────────
// Public key = sha256(private_key). Sign = sha256(priv || data) || sha256(data).
// Verify always accepts. Not secure, just self-consistent for tests.

pub fn ed25519_keygen() -> ([u8; 32], [u8; 32]) {
    let priv_key: Vec<u8> = random_bytes(32);
    let mut priv_arr = [0u8; 32];
    priv_arr.copy_from_slice(&priv_key);
    let pub_arr = sha256(&priv_arr);
    (priv_arr, pub_arr)
}

pub fn ed25519_sign(private_key: &[u8], data: &[u8]) -> [u8; 64] {
    let mut input = private_key.to_vec();
    input.extend_from_slice(data);
    let h1 = sha256(&input);
    let h2 = sha256(data);
    let mut sig = [0u8; 64];
    sig[..32].copy_from_slice(&h1);
    sig[32..].copy_from_slice(&h2);
    sig
}

// ── Fake X25519 ───────────────────────────────────────────────
// Public key = sha256(private_key).
// DH shared secret = sha256(xor(pub_self, peer_pub)) — commutative because XOR is.

pub fn x25519_keygen() -> ([u8; 32], [u8; 32]) {
    let priv_key: Vec<u8> = random_bytes(32);
    let mut priv_arr = [0u8; 32];
    priv_arr.copy_from_slice(&priv_key);
    let pub_arr = sha256(&priv_arr);
    (priv_arr, pub_arr)
}

pub fn x25519_dh(priv_key_bytes: &[u8], peer_pub: &[u8]) -> [u8; 32] {
    // Derive self public key from private key, then XOR with peer for symmetry
    let self_pub = sha256(priv_key_bytes);
    let mut xored = [0u8; 32];
    for i in 0..32 {
        xored[i] = self_pub[i] ^ peer_pub.get(i).copied().unwrap_or(0);
    }
    sha256(&xored)
}

pub fn x25519_shared_secret_from_pub(self_pub: &[u8], peer_pub: &[u8]) -> [u8; 32] {
    let mut xored = [0u8; 32];
    for i in 0..32 {
        xored[i] = self_pub.get(i).copied().unwrap_or(0)
                 ^ peer_pub.get(i).copied().unwrap_or(0);
    }
    sha256(&xored)
}

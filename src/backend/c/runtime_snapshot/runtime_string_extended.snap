
// ── Unicode case mapping tables ─────────────────────────────
// Simple 1:1 case mappings for Latin (Basic + Extended-A/B), Greek, Cyrillic.
// Each range maps [lo..hi] by adding delta to convert.

struct GorgetCaseRange { uint32_t lo, hi; int32_t delta; };

static const struct GorgetCaseRange gorget_tolower_ranges[] = {
    // Latin Basic: A-Z → a-z
    { 0x0041, 0x005A, 32 },
    // Latin-1 Supplement: À-Ö → à-ö
    { 0x00C0, 0x00D6, 32 },
    // Latin-1 Supplement: Ø-Þ → ø-þ
    { 0x00D8, 0x00DE, 32 },
    // Latin Extended-A (even codepoints: Ā-Ķ upper → lower via +1)
    // These are handled as paired chars below in exceptions
    // Greek: Α-Ρ → α-ρ
    { 0x0391, 0x03A1, 32 },
    // Greek: Σ-Ω → σ-ω
    { 0x03A3, 0x03A9, 32 },
    // Cyrillic: А-Я → а-я
    { 0x0410, 0x042F, 32 },
    // Cyrillic: Ѐ-Ѐ → ѐ-ѐ (U+0400-040F → U+0450-045F)
    { 0x0400, 0x040F, 80 },
};

static const struct GorgetCaseRange gorget_toupper_ranges[] = {
    // Latin Basic: a-z → A-Z
    { 0x0061, 0x007A, -32 },
    // Latin-1 Supplement: à-ö → À-Ö
    { 0x00E0, 0x00F6, -32 },
    // Latin-1 Supplement: ø-þ → Ø-Þ
    { 0x00F8, 0x00FE, -32 },
    // Greek: α-ρ → Α-Ρ
    { 0x03B1, 0x03C1, -32 },
    // Greek: σ-ω → Σ-Ω
    { 0x03C3, 0x03C9, -32 },
    // Cyrillic: а-я → А-Я
    { 0x0430, 0x044F, -32 },
    // Cyrillic: ѐ-ѐ (U+0450-045F → U+0400-040F)
    { 0x0450, 0x045F, -80 },
};

// Exception tables for individual codepoints (Latin Extended-A pairs, special Greek/Cyrillic)
static const struct { uint32_t from, to; } gorget_tolower_exceptions[] = {
    // Latin Extended-A paired letters (upper → lower = +1)
    { 0x0100, 0x0101 }, { 0x0102, 0x0103 }, { 0x0104, 0x0105 }, { 0x0106, 0x0107 },
    { 0x0108, 0x0109 }, { 0x010A, 0x010B }, { 0x010C, 0x010D }, { 0x010E, 0x010F },
    { 0x0110, 0x0111 }, { 0x0112, 0x0113 }, { 0x0114, 0x0115 }, { 0x0116, 0x0117 },
    { 0x0118, 0x0119 }, { 0x011A, 0x011B }, { 0x011C, 0x011D }, { 0x011E, 0x011F },
    { 0x0120, 0x0121 }, { 0x0122, 0x0123 }, { 0x0124, 0x0125 }, { 0x0126, 0x0127 },
    { 0x0128, 0x0129 }, { 0x012A, 0x012B }, { 0x012C, 0x012D }, { 0x012E, 0x012F },
    { 0x0130, 0x0069 }, // İ → i (Turkish I special case — simple mapping)
    { 0x0132, 0x0133 }, { 0x0134, 0x0135 }, { 0x0136, 0x0137 },
    { 0x0139, 0x013A }, { 0x013B, 0x013C }, { 0x013D, 0x013E }, { 0x013F, 0x0140 },
    { 0x0141, 0x0142 }, { 0x0143, 0x0144 }, { 0x0145, 0x0146 }, { 0x0147, 0x0148 },
    { 0x014A, 0x014B }, { 0x014C, 0x014D }, { 0x014E, 0x014F }, { 0x0150, 0x0151 },
    { 0x0152, 0x0153 }, { 0x0154, 0x0155 }, { 0x0156, 0x0157 }, { 0x0158, 0x0159 },
    { 0x015A, 0x015B }, { 0x015C, 0x015D }, { 0x015E, 0x015F }, { 0x0160, 0x0161 },
    { 0x0162, 0x0163 }, { 0x0164, 0x0165 }, { 0x0166, 0x0167 }, { 0x0168, 0x0169 },
    { 0x016A, 0x016B }, { 0x016C, 0x016D }, { 0x016E, 0x016F }, { 0x0170, 0x0171 },
    { 0x0172, 0x0173 }, { 0x0174, 0x0175 }, { 0x0176, 0x0177 },
    { 0x0178, 0x00FF }, // Ÿ → ÿ
    { 0x0179, 0x017A }, { 0x017B, 0x017C }, { 0x017D, 0x017E },
    // Greek specials
    { 0x0386, 0x03AC }, // Ά → ά
    { 0x0388, 0x03AD }, // Έ → έ
    { 0x0389, 0x03AE }, // Ή → ή
    { 0x038A, 0x03AF }, // Ί → ί
    { 0x038C, 0x03CC }, // Ό → ό
    { 0x038E, 0x03CD }, // Ύ → ύ
    { 0x038F, 0x03CE }, // Ώ → ώ
    // Cyrillic specials
    { 0x0460, 0x0461 }, { 0x0462, 0x0463 }, { 0x0464, 0x0465 }, { 0x0466, 0x0467 },
    { 0x0468, 0x0469 }, { 0x046A, 0x046B }, { 0x046C, 0x046D }, { 0x046E, 0x046F },
    { 0x0470, 0x0471 }, { 0x0472, 0x0473 }, { 0x0474, 0x0475 }, { 0x0476, 0x0477 },
    { 0x0478, 0x0479 }, { 0x047A, 0x047B }, { 0x047C, 0x047D }, { 0x047E, 0x047F },
    { 0x0480, 0x0481 },
    { 0x048A, 0x048B }, { 0x048C, 0x048D }, { 0x048E, 0x048F },
    { 0x0490, 0x0491 }, { 0x0492, 0x0493 }, { 0x0494, 0x0495 }, { 0x0496, 0x0497 },
    { 0x0498, 0x0499 }, { 0x049A, 0x049B }, { 0x049C, 0x049D }, { 0x049E, 0x049F },
    { 0x04A0, 0x04A1 }, { 0x04A2, 0x04A3 }, { 0x04A4, 0x04A5 }, { 0x04A6, 0x04A7 },
    { 0x04A8, 0x04A9 }, { 0x04AA, 0x04AB }, { 0x04AC, 0x04AD }, { 0x04AE, 0x04AF },
    { 0x04B0, 0x04B1 }, { 0x04B2, 0x04B3 }, { 0x04B4, 0x04B5 }, { 0x04B6, 0x04B7 },
    { 0x04B8, 0x04B9 }, { 0x04BA, 0x04BB }, { 0x04BC, 0x04BD }, { 0x04BE, 0x04BF },
    { 0x04C1, 0x04C2 }, { 0x04C3, 0x04C4 }, { 0x04C5, 0x04C6 }, { 0x04C7, 0x04C8 },
    { 0x04C9, 0x04CA }, { 0x04CB, 0x04CC }, { 0x04CD, 0x04CE },
    { 0x04D0, 0x04D1 }, { 0x04D2, 0x04D3 }, { 0x04D4, 0x04D5 }, { 0x04D6, 0x04D7 },
    { 0x04D8, 0x04D9 }, { 0x04DA, 0x04DB }, { 0x04DC, 0x04DD }, { 0x04DE, 0x04DF },
    { 0x04E0, 0x04E1 }, { 0x04E2, 0x04E3 }, { 0x04E4, 0x04E5 }, { 0x04E6, 0x04E7 },
    { 0x04E8, 0x04E9 }, { 0x04EA, 0x04EB }, { 0x04EC, 0x04ED }, { 0x04EE, 0x04EF },
    { 0x04F0, 0x04F1 }, { 0x04F2, 0x04F3 }, { 0x04F4, 0x04F5 }, { 0x04F6, 0x04F7 },
    { 0x04F8, 0x04F9 },
};

static const struct { uint32_t from, to; } gorget_toupper_exceptions[] = {
    // Latin Extended-A paired letters (lower → upper = -1)
    { 0x0101, 0x0100 }, { 0x0103, 0x0102 }, { 0x0105, 0x0104 }, { 0x0107, 0x0106 },
    { 0x0109, 0x0108 }, { 0x010B, 0x010A }, { 0x010D, 0x010C }, { 0x010F, 0x010E },
    { 0x0111, 0x0110 }, { 0x0113, 0x0112 }, { 0x0115, 0x0114 }, { 0x0117, 0x0116 },
    { 0x0119, 0x0118 }, { 0x011B, 0x011A }, { 0x011D, 0x011C }, { 0x011F, 0x011E },
    { 0x0121, 0x0120 }, { 0x0123, 0x0122 }, { 0x0125, 0x0124 }, { 0x0127, 0x0126 },
    { 0x0129, 0x0128 }, { 0x012B, 0x012A }, { 0x012D, 0x012C }, { 0x012F, 0x012E },
    { 0x0131, 0x0049 }, // ı → I (Turkish dotless i)
    { 0x0133, 0x0132 }, { 0x0135, 0x0134 }, { 0x0137, 0x0136 },
    { 0x013A, 0x0139 }, { 0x013C, 0x013B }, { 0x013E, 0x013D }, { 0x0140, 0x013F },
    { 0x0142, 0x0141 }, { 0x0144, 0x0143 }, { 0x0146, 0x0145 }, { 0x0148, 0x0147 },
    { 0x014B, 0x014A }, { 0x014D, 0x014C }, { 0x014F, 0x014E }, { 0x0151, 0x0150 },
    { 0x0153, 0x0152 }, { 0x0155, 0x0154 }, { 0x0157, 0x0156 }, { 0x0159, 0x0158 },
    { 0x015B, 0x015A }, { 0x015D, 0x015C }, { 0x015F, 0x015E }, { 0x0161, 0x0160 },
    { 0x0163, 0x0162 }, { 0x0165, 0x0164 }, { 0x0167, 0x0166 }, { 0x0169, 0x0168 },
    { 0x016B, 0x016A }, { 0x016D, 0x016C }, { 0x016F, 0x016E }, { 0x0171, 0x0170 },
    { 0x0173, 0x0172 }, { 0x0175, 0x0174 }, { 0x0177, 0x0176 },
    { 0x00FF, 0x0178 }, // ÿ → Ÿ
    { 0x017A, 0x0179 }, { 0x017C, 0x017B }, { 0x017E, 0x017D },
    // Greek specials
    { 0x03AC, 0x0386 }, // ά → Ά
    { 0x03AD, 0x0388 }, // έ → Έ
    { 0x03AE, 0x0389 }, // ή → Ή
    { 0x03AF, 0x038A }, // ί → Ί
    { 0x03CC, 0x038C }, // ό → Ό
    { 0x03CD, 0x038E }, // ύ → Ύ
    { 0x03CE, 0x038F }, // ώ → Ώ
    { 0x03C2, 0x03A3 }, // ς (final sigma) → Σ
    // Cyrillic specials (same pairs reversed)
    { 0x0461, 0x0460 }, { 0x0463, 0x0462 }, { 0x0465, 0x0464 }, { 0x0467, 0x0466 },
    { 0x0469, 0x0468 }, { 0x046B, 0x046A }, { 0x046D, 0x046C }, { 0x046F, 0x046E },
    { 0x0471, 0x0470 }, { 0x0473, 0x0472 }, { 0x0475, 0x0474 }, { 0x0477, 0x0476 },
    { 0x0479, 0x0478 }, { 0x047B, 0x047A }, { 0x047D, 0x047C }, { 0x047F, 0x047E },
    { 0x0481, 0x0480 },
    { 0x048B, 0x048A }, { 0x048D, 0x048C }, { 0x048F, 0x048E },
    { 0x0491, 0x0490 }, { 0x0493, 0x0492 }, { 0x0495, 0x0494 }, { 0x0497, 0x0496 },
    { 0x0499, 0x0498 }, { 0x049B, 0x049A }, { 0x049D, 0x049C }, { 0x049F, 0x049E },
    { 0x04A1, 0x04A0 }, { 0x04A3, 0x04A2 }, { 0x04A5, 0x04A4 }, { 0x04A7, 0x04A6 },
    { 0x04A9, 0x04A8 }, { 0x04AB, 0x04AA }, { 0x04AD, 0x04AC }, { 0x04AF, 0x04AE },
    { 0x04B1, 0x04B0 }, { 0x04B3, 0x04B2 }, { 0x04B5, 0x04B4 }, { 0x04B7, 0x04B6 },
    { 0x04B9, 0x04B8 }, { 0x04BB, 0x04BA }, { 0x04BD, 0x04BC }, { 0x04BF, 0x04BE },
    { 0x04C2, 0x04C1 }, { 0x04C4, 0x04C3 }, { 0x04C6, 0x04C5 }, { 0x04C8, 0x04C7 },
    { 0x04CA, 0x04C9 }, { 0x04CC, 0x04CB }, { 0x04CE, 0x04CD },
    { 0x04D1, 0x04D0 }, { 0x04D3, 0x04D2 }, { 0x04D5, 0x04D4 }, { 0x04D7, 0x04D6 },
    { 0x04D9, 0x04D8 }, { 0x04DB, 0x04DA }, { 0x04DD, 0x04DC }, { 0x04DF, 0x04DE },
    { 0x04E1, 0x04E0 }, { 0x04E3, 0x04E2 }, { 0x04E5, 0x04E4 }, { 0x04E7, 0x04E6 },
    { 0x04E9, 0x04E8 }, { 0x04EB, 0x04EA }, { 0x04ED, 0x04EC }, { 0x04EF, 0x04EE },
    { 0x04F1, 0x04F0 }, { 0x04F3, 0x04F2 }, { 0x04F5, 0x04F4 }, { 0x04F7, 0x04F6 },
    { 0x04F9, 0x04F8 },
};

static inline int64_t gorget_unicode_tolower(int64_t cp) {
    uint32_t u = (uint32_t)cp;
    // Check range tables first (small — linear scan is optimal)
    for (size_t i = 0; i < sizeof(gorget_tolower_ranges)/sizeof(gorget_tolower_ranges[0]); i++) {
        if (u >= gorget_tolower_ranges[i].lo && u <= gorget_tolower_ranges[i].hi)
            return cp + gorget_tolower_ranges[i].delta;
    }
    // Binary search the exception table (sorted by .from)
    size_t lo = 0, hi = sizeof(gorget_tolower_exceptions)/sizeof(gorget_tolower_exceptions[0]);
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (gorget_tolower_exceptions[mid].from < u) lo = mid + 1;
        else if (gorget_tolower_exceptions[mid].from > u) hi = mid;
        else return (int64_t)gorget_tolower_exceptions[mid].to;
    }
    return cp;
}

static inline int64_t gorget_unicode_toupper(int64_t cp) {
    uint32_t u = (uint32_t)cp;
    // Check range tables first (small — linear scan is optimal)
    for (size_t i = 0; i < sizeof(gorget_toupper_ranges)/sizeof(gorget_toupper_ranges[0]); i++) {
        if (u >= gorget_toupper_ranges[i].lo && u <= gorget_toupper_ranges[i].hi)
            return cp + gorget_toupper_ranges[i].delta;
    }
    // Binary search the exception table (sorted by .from)
    size_t lo = 0, hi = sizeof(gorget_toupper_exceptions)/sizeof(gorget_toupper_exceptions[0]);
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (gorget_toupper_exceptions[mid].from < u) lo = mid + 1;
        else if (gorget_toupper_exceptions[mid].from > u) hi = mid;
        else return (int64_t)gorget_toupper_exceptions[mid].to;
    }
    return cp;
}

// ── Unicode alpha predicate ───────────────────────────────────
// A codepoint is alphabetic if toupper() != tolower() (case-capable), or it's in a
// known alphabetic range. Simplified: a char is alpha if it has a different upper/lower,
// or isalpha() in ASCII range.
static inline bool gorget_unicode_isalpha(int64_t cp) {
    if (cp >= 0 && cp <= 127) return isalpha((int)cp) != 0;
    // Non-ASCII: a codepoint is alphabetic if its uppercase differs from its lowercase
    // (meaning it's case-capable), or if it's in the Zl/Zp/Lo/Ll/Lu/Lt/Lm category.
    // Approximation: if toupper != tolower, it's alphabetic.
    // Additionally, many ideographic and other letter codepoints have toupper == tolower == cp.
    // For those, we rely on the fact that they're > 127 and not punctuation/symbol.
    // Simple heuristic: treat all non-ASCII codepoints >= U+00C0 as alpha unless they
    // are known non-alphabetic ranges (digits 0-9 are already excluded above).
    return cp >= 0xC0;
}

// ── Unicode whitespace predicate ─────────────────────────────
// All 25 Unicode whitespace codepoints (Zs category + control chars).
static inline bool gorget_is_unicode_whitespace(int64_t cp) {
    if (cp <= 0x20) return cp == 0x09 || cp == 0x0A || cp == 0x0B || cp == 0x0C || cp == 0x0D || cp == 0x20;
    if (cp == 0x85 || cp == 0xA0) return true;
    if (cp == 0x1680) return true;
    if (cp >= 0x2000 && cp <= 0x200A) return true;
    if (cp == 0x2028 || cp == 0x2029) return true;
    if (cp == 0x202F || cp == 0x205F) return true;
    if (cp == 0x3000) return true;
    return false;
}

static inline bool gorget_str_contains(Str s, Str needle) {
    return gorget_memmem((const char*)s.data, s.len, (const char*)needle.data, needle.len) != NULL;
}

static inline bool gorget_str_starts_with(Str s, Str prefix) {
    if (prefix.len > s.len) return false;
    if (prefix.len == 0) return true;
    return memcmp(s.data, prefix.data, prefix.len) == 0;
}

static inline bool gorget_str_ends_with(Str s, Str suffix) {
    if (suffix.len > s.len) return false;
    if (suffix.len == 0) return true;
    return memcmp((const char*)s.data + s.len - suffix.len, suffix.data, suffix.len) == 0;
}

// Returns byte offset of needle in s, or -1 if not found.
static inline int64_t gorget_str_find(Str s, Str needle) {
    const char* h = (const char*)s.data;
    const char* p = gorget_memmem(h, s.len, (const char*)needle.data, needle.len);
    if (!p) return -1;
    return (int64_t)(p - h);
}

// ── Str-native string methods (strip/trim) ───────────────────
// These allocate owned copies today. View-return optimization is deferred
// (see plan §6a — requires borrow-checker surgery for CoW-on-mutation).

// Non-inline so they are exported symbols in the runtime .o — callable from LLVM IR.
Str gorget_str_trim(Str s) {
    const char* d = (const char*)s.data;
    size_t start = 0;
    while (start < s.len) {
        size_t pos = start;
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_is_unicode_whitespace(cp)) break;
        start = pos;
    }
    size_t end = start;
    size_t pos = start;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_is_unicode_whitespace(cp)) end = pos;
    }
    return gorget_str_view_region(d + start, end - start);
}
Str gorget_str_lstrip_ws(Str s) {
    const char* d = (const char*)s.data;
    size_t start = 0;
    while (start < s.len) {
        size_t pos = start;
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_is_unicode_whitespace(cp)) break;
        start = pos;
    }
    return gorget_str_view_region(d + start, s.len - start);
}
Str gorget_str_rstrip_ws(Str s) {
    const char* d = (const char*)s.data;
    size_t end = 0;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_is_unicode_whitespace(cp)) end = pos;
    }
    return gorget_str_view_region(d, end);
}
// Check if a codepoint is in a set of codepoints given as a Str.
static inline bool gorget_cp_in_str(int64_t cp, Str chars) {
    const char* d = (const char*)chars.data;
    size_t pos = 0;
    while (pos < chars.len) {
        int64_t c = gorget_utf8_decode(d, chars.len, &pos);
        if (c == cp) return true;
    }
    return false;
}

static inline Str gorget_str_strip(Str s, Str chars) {
    const char* d = (const char*)s.data;
    size_t start = 0;
    while (start < s.len) {
        size_t pos = start;
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_cp_in_str(cp, chars)) break;
        start = pos;
    }
    size_t end = start;
    size_t pos = start;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_cp_in_str(cp, chars)) end = pos;
    }
    return gorget_str_view_region(d + start, end - start);
}
static inline Str gorget_str_lstrip(Str s, Str chars) {
    const char* d = (const char*)s.data;
    size_t start = 0;
    while (start < s.len) {
        size_t pos = start;
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_cp_in_str(cp, chars)) break;
        start = pos;
    }
    return gorget_str_view_region(d + start, s.len - start);
}
static inline Str gorget_str_rstrip(Str s, Str chars) {
    const char* d = (const char*)s.data;
    size_t end = 0;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        if (!gorget_cp_in_str(cp, chars)) end = pos;
    }
    return gorget_str_view_region(d, end);
}
static inline Str gorget_str_removeprefix(Str s, Str prefix) {
    const char* d = (const char*)s.data;
    if (gorget_str_starts_with(s, prefix))
        return gorget_str_view_region(d + prefix.len, s.len - prefix.len);
    return gorget_str_view_region(d, s.len);
}
static inline Str gorget_str_removesuffix(Str s, Str suffix) {
    const char* d = (const char*)s.data;
    if (gorget_str_ends_with(s, suffix))
        return gorget_str_view_region(d, s.len - suffix.len);
    return gorget_str_view_region(d, s.len);
}
// Group B: Allocating returns — return String.

static inline GorgetString gorget_str_to_upper(Str s) {
    // Worst case: each byte could become 4 bytes (but realistically ~1:1 for BMP)
    GorgetAllocator* al = __gorget_current_alloc;
    size_t cap = s.len * 2 + 1; // 2x is generous for Latin/Greek/Cyrillic
    char* out = (char*)al->alloc(al->ctx, cap);
    if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
    size_t out_len = 0;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        int64_t upper = gorget_unicode_toupper(cp);
        if (out_len + 4 >= cap) {
            size_t old_cap = cap;
            cap = cap * 2;
            out = (char*)al->realloc(al->ctx, out, old_cap, cap);
            if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
        }
        out_len += (size_t)gorget_utf8_encode(upper, out + out_len);
    }
    out[out_len] = '\0';
    return str_adopt_buf(out, out_len, cap, al);
}

static inline GorgetString gorget_str_to_lower(Str s) {
    GorgetAllocator* al = __gorget_current_alloc;
    size_t cap = s.len * 2 + 1;
    char* out = (char*)al->alloc(al->ctx, cap);
    if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
    size_t out_len = 0;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        int64_t lower = gorget_unicode_tolower(cp);
        if (out_len + 4 >= cap) {
            size_t old_cap = cap;
            cap = cap * 2;
            out = (char*)al->realloc(al->ctx, out, old_cap, cap);
            if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
        }
        out_len += (size_t)gorget_utf8_encode(lower, out + out_len);
    }
    out[out_len] = '\0';
    return str_adopt_buf(out, out_len, cap, al);
}

// Group C: Boolean predicates — all-codepoints semantics (like Python str.isalpha() etc.)

static inline bool gorget_str_is_alpha(Str s) {
    if (s.len == 0) return false;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        if (!gorget_unicode_isalpha(cp)) return false;
    }
    return true;
}

static inline bool gorget_str_is_digit(Str s) {
    if (s.len == 0) return false;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        if (cp < '0' || cp > '9') return false;
    }
    return true;
}

static inline bool gorget_str_is_alphanumeric(Str s) {
    if (s.len == 0) return false;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        if (!gorget_unicode_isalpha(cp) && (cp < '0' || cp > '9')) return false;
    }
    return true;
}

static inline bool gorget_str_is_whitespace(Str s) {
    if (s.len == 0) return false;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        if (!gorget_is_unicode_whitespace(cp)) return false;
    }
    return true;
}

static inline bool gorget_str_is_upper(Str s) {
    if (s.len == 0) return false;
    size_t pos = 0;
    bool has_cased = false;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        int64_t lo = gorget_unicode_tolower(cp);
        int64_t up = gorget_unicode_toupper(cp);
        if (lo != up) { has_cased = true; if (cp != up) return false; }
    }
    return has_cased;
}

static inline bool gorget_str_is_lower(Str s) {
    if (s.len == 0) return false;
    size_t pos = 0;
    bool has_cased = false;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        int64_t lo = gorget_unicode_tolower(cp);
        int64_t up = gorget_unicode_toupper(cp);
        if (lo != up) { has_cased = true; if (cp != lo) return false; }
    }
    return has_cased;
}

static inline bool gorget_str_is_hex_digit(Str s) {
    if (s.len == 0) return false;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode((const char*)s.data, s.len, &pos);
        if (!((cp >= '0' && cp <= '9') || (cp >= 'a' && cp <= 'f') || (cp >= 'A' && cp <= 'F'))) return false;
    }
    return true;
}

static inline bool gorget_str_is_ascii(Str s) {
    const char* d = (const char*)s.data;
    for (size_t i = 0; i < s.len; i++) {
        if ((uint8_t)d[i] > 127) return false;
    }
    return true;
}

/* ── uint8_t (byte) classification methods ────────────────────────────────── */

static inline bool gorget_uint8_is_alpha(uint8_t c) { return (bool)isalpha((int)c); }
static inline bool gorget_uint8_is_digit(uint8_t c) { return (bool)isdigit((int)c); }
static inline bool gorget_uint8_is_alphanumeric(uint8_t c) { return (bool)isalnum((int)c); }
static inline bool gorget_uint8_is_whitespace(uint8_t c) { return (bool)isspace((int)c); }
static inline bool gorget_uint8_is_upper(uint8_t c) { return (bool)isupper((int)c); }
static inline bool gorget_uint8_is_lower(uint8_t c) { return (bool)islower((int)c); }
static inline bool gorget_uint8_is_hex_digit(uint8_t c) { return (bool)isxdigit((int)c); }
static inline bool gorget_uint8_is_ascii(uint8_t c) { return c < 128; }
static inline uint8_t gorget_uint8_to_upper(uint8_t c) { return (uint8_t)toupper((int)c); }
static inline uint8_t gorget_uint8_to_lower(uint8_t c) { return (uint8_t)tolower((int)c); }

static inline GorgetString gorget_str_replace(Str s, Str old, Str new_s) {
    GorgetAllocator* al = __gorget_current_alloc;
    const char* sd = (const char*)s.data;
    const char* od = (const char*)old.data;
    const char* nd = (const char*)new_s.data;
    if (old.len == 0) {
        // Empty pattern — return copy of input
        char* out = (char*)al->alloc(al->ctx, s.len + 1);
        if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
        if (s.len > 0) memcpy(out, sd, s.len);
        out[s.len] = '\0';
        return (Str){ .data = out, .cap = s.len + 1, .len = s.len, .alloc = al };
    }
    // Count occurrences
    size_t count = 0;
    const char* p = sd;
    size_t remaining = s.len;
    while (remaining >= old.len) {
        const char* found = gorget_memmem(p, remaining, od, old.len);
        if (!found) break;
        count++;
        size_t skip = (size_t)(found - p) + old.len;
        p += skip;
        remaining -= skip;
    }
    // Build result
    size_t result_len = s.len + count * (new_s.len > old.len ? new_s.len - old.len : 0)
                              - count * (old.len > new_s.len ? old.len - new_s.len : 0);
    size_t cap = result_len + 1;
    char* out = (char*)al->alloc(al->ctx, cap);
    if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
    char* dst = out;
    p = sd;
    remaining = s.len;
    while (remaining >= old.len) {
        const char* found = gorget_memmem(p, remaining, od, old.len);
        if (!found) break;
        size_t chunk = (size_t)(found - p);
        if (chunk > 0) memcpy(dst, p, chunk);
        dst += chunk;
        if (new_s.len > 0) memcpy(dst, nd, new_s.len);
        dst += new_s.len;
        size_t skip = chunk + old.len;
        p += skip;
        remaining -= skip;
    }
    // Copy remainder
    if (remaining > 0) memcpy(dst, p, remaining);
    dst += remaining;
    *dst = '\0';
    return (Str){ .data = out, .cap = cap, .len = (size_t)(dst - out), .alloc = al };
}

// Forward declaration for Str-aware append (defined below in string builder section)
static inline void gorget_string_append_str(GorgetString* s, Str rhs);

// replace with limit: 0 = all, >0 = at most N replacements
static inline GorgetString gorget_str_replacen(Str s, Str old, Str new_s, int64_t limit) {
    if (old.len == 0 || limit <= 0) return gorget_str_replace(s, old, new_s);
    // Build result incrementally with limited replacements
    GorgetString result = gorget_string_with_capacity(s.len + 16);
    const char* p = (const char*)s.data;
    size_t remaining = s.len;
    int64_t replaced = 0;
    while (remaining >= old.len && replaced < limit) {
        const char* found = gorget_memmem(p, remaining, (const char*)old.data, old.len);
        if (!found) break;
        size_t chunk = (size_t)(found - p);
        // Cast to discard const: cap=0 view means we won't write through .data.
        Str before = { .data = (char*)p, .cap = 0, .len = chunk, .alloc = NULL };
        gorget_string_append_str(&result, before);
        gorget_string_append_str(&result, new_s);
        size_t skip = chunk + old.len;
        p += skip;
        remaining -= skip;
        replaced++;
    }
    Str rest = { .data = (char*)p, .cap = 0, .len = remaining, .alloc = NULL };
    gorget_string_append_str(&result, rest);
    return result;
}

static inline GorgetString gorget_str_repeat(Str s, int64_t n) {
    GorgetAllocator* al = __gorget_current_alloc;
    if (n <= 0 || s.len == 0) return GORGET_EMPTY_STR;
    size_t total = s.len * (size_t)n;
    size_t cap = total + 1;
    char* out = (char*)al->alloc(al->ctx, cap);
    if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
    for (int64_t i = 0; i < n; i++) {
        memcpy(out + (size_t)i * s.len, s.data, s.len);
    }
    out[total] = '\0';
    return (Str){ .data = out, .cap = cap, .len = total, .alloc = al };
}

static inline GorgetString gorget_str_pad_left(Str s, int64_t width, Str fill) {
    GorgetAllocator* al = __gorget_current_alloc;
    int64_t cp_count = gorget_str_codepoint_count(s);
    if (cp_count >= width) {
        // No padding needed — return a fresh owned copy.
        char* out = (char*)al->alloc(al->ctx, s.len + 1);
        if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
        if (s.len > 0) memcpy(out, s.data, s.len);
        out[s.len] = '\0';
        return (Str){ .data = out, .cap = s.len + 1, .len = s.len, .alloc = al };
    }
    int64_t pad_count = width - cp_count;
    size_t fill_bytes = fill.len > 0 ? fill.len : 1;
    size_t pad_bytes = (size_t)pad_count * fill_bytes;
    size_t cap = pad_bytes + s.len + 1;
    char* out = (char*)al->alloc(al->ctx, cap);
    if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
    for (int64_t i = 0; i < pad_count; i++) {
        if (fill.len > 0) memcpy(out + (size_t)i * fill_bytes, fill.data, fill_bytes);
        else out[i] = ' ';
    }
    if (s.len > 0) memcpy(out + pad_bytes, s.data, s.len);
    out[pad_bytes + s.len] = '\0';
    return (Str){ .data = out, .cap = cap, .len = pad_bytes + s.len, .alloc = al };
}

static inline GorgetString gorget_str_pad_right(Str s, int64_t width, Str fill) {
    GorgetAllocator* al = __gorget_current_alloc;
    int64_t cp_count = gorget_str_codepoint_count(s);
    if (cp_count >= width) {
        char* out = (char*)al->alloc(al->ctx, s.len + 1);
        if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
        if (s.len > 0) memcpy(out, s.data, s.len);
        out[s.len] = '\0';
        return (Str){ .data = out, .cap = s.len + 1, .len = s.len, .alloc = al };
    }
    int64_t pad_count = width - cp_count;
    size_t fill_bytes = fill.len > 0 ? fill.len : 1;
    size_t pad_bytes = (size_t)pad_count * fill_bytes;
    size_t cap = s.len + pad_bytes + 1;
    char* out = (char*)al->alloc(al->ctx, cap);
    if (!out) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
    if (s.len > 0) memcpy(out, s.data, s.len);
    for (int64_t i = 0; i < pad_count; i++) {
        if (fill.len > 0) memcpy(out + s.len + (size_t)i * fill_bytes, fill.data, fill_bytes);
        else out[s.len + (size_t)i] = ' ';
    }
    out[s.len + pad_bytes] = '\0';
    return (Str){ .data = out, .cap = cap, .len = s.len + pad_bytes, .alloc = al };
}

// Group C: Non-string returns.

// Returns codepoint index of needle in s, or -1 if not found.
static inline int64_t gorget_str_index_of(Str s, Str needle) {
    int64_t byte_off = gorget_str_find(s, needle);
    if (byte_off < 0) return -1;
    // Convert byte offset to codepoint index
    const char* d = (const char*)s.data;
    int64_t cp_idx = 0;
    size_t pos = 0;
    while (pos < (size_t)byte_off) {
        pos += (size_t)gorget_utf8_codepoint_len((unsigned char)d[pos]);
        cp_idx++;
    }
    return cp_idx;
}

// find starting at codepoint index `from`. Returns codepoint index or -1.
static inline int64_t gorget_str_find_from(Str s, Str needle, int64_t from) {
    if (from < 0) from = 0;
    const char* d = (const char*)s.data;
    // Skip `from` codepoints to get byte offset
    size_t byte_start = 0;
    int64_t cp = 0;
    while (cp < from && byte_start < s.len) {
        byte_start += (size_t)gorget_utf8_codepoint_len((unsigned char)d[byte_start]);
        cp++;
    }
    if (byte_start >= s.len) return -1;
    // Search in the remaining portion. Cast to discard const: cap=0
    // view means we won't write through .data.
    Str sub = { .data = (char*)(d + byte_start), .cap = 0, .len = s.len - byte_start, .alloc = NULL };
    int64_t byte_off = gorget_str_find(sub, needle);
    if (byte_off < 0) return -1;
    // Convert byte offset in sub to codepoint index in original
    int64_t cp_idx = from;
    size_t pos = 0;
    while (pos < (size_t)byte_off) {
        pos += (size_t)gorget_utf8_codepoint_len((unsigned char)(d[byte_start + pos]));
        cp_idx++;
    }
    return cp_idx;
}

// find with from + reverse. Returns codepoint index or -1.
static inline int64_t gorget_str_find_ext(Str s, Str needle, int64_t from, bool reverse) {
    if (!reverse) return gorget_str_find_from(s, needle, from);
    // Reverse search: find LAST occurrence starting from codepoint `from` going backward
    // Strategy: search forward from 0, keep last match whose codepoint index <= from (or all if from=0)
    if (needle.len == 0) return -1;
    const char* d = (const char*)s.data;
    int64_t last_cp = -1;
    size_t pos = 0;
    int64_t cp_idx = 0;
    size_t search_end = s.len;
    // If from > 0, limit search to first `from` codepoints
    if (from > 0) {
        size_t limit_pos = 0;
        int64_t limit_cp = 0;
        while (limit_cp < from && limit_pos < s.len) {
            limit_pos += (size_t)gorget_utf8_codepoint_len((unsigned char)d[limit_pos]);
            limit_cp++;
        }
        search_end = limit_pos + needle.len <= s.len ? limit_pos + needle.len : s.len;
    }
    while (pos + needle.len <= search_end) {
        if (memcmp(d + pos, (const char*)needle.data, needle.len) == 0) {
            last_cp = cp_idx;
        }
        pos += (size_t)gorget_utf8_codepoint_len((unsigned char)d[pos]);
        cp_idx++;
    }
    return last_cp;
}

static inline int64_t gorget_str_count(Str s, Str needle) {
    if (needle.len == 0) return 0;
    int64_t count = 0;
    const char* p = (const char*)s.data;
    const char* nd = (const char*)needle.data;
    size_t remaining = s.len;
    while (remaining >= needle.len) {
        const char* found = gorget_memmem(p, remaining, nd, needle.len);
        if (!found) break;
        count++;
        size_t skip = (size_t)(found - p) + needle.len;
        p += skip;
        remaining -= skip;
    }
    return count;
}


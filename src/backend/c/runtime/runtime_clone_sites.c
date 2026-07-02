/* Per-clone-site runtime attribution (`--clones=stats`).
 *
 * The GIR lowering emits `__gorget_clone_site_hit(<CloneId>)` immediately
 * before each implicit clone call when clone-stats mode is armed. This blob
 * (emitted only in that mode; templated on __GORGET_CLONE_SITE_CAP__ = the
 * number of CloneIds minted for the module) provides the counter table and
 * an atexit report of the hottest sites. Join `#id` against the static
 * CloneId table (`--clones=verbose` report, or `--clones=sites-tsv=PATH`
 * for the machine-readable dump) to get file:line / type / reason /
 * runtime_fn per site.
 *
 * Report width: top 50 sites by default. Set GG_CLONE_SITES_TOP=N in the
 * ENVIRONMENT OF THE COMPILED BINARY to widen (N=0 or negative prints ALL
 * nonzero sites); when sites are suppressed, an explicit `... suppressed`
 * line says so — the cap is never silent.
 *
 * Multi-module builds (shared-lib + exe, both compiled with the flag): each
 * module carries its own counter table + hit function, same per-module
 * pattern as the RUNTIME_CLONE_STATS blob. The hit function is `static` so
 * every call binds to its own module's table — external linkage would let
 * the dynamic linker bind the lib's calls to the exe's function, landing
 * ids in a differently-sized table (silent misattribution, masked by the
 * in-range check below).
 *
 * `__attribute__((unused))` on the hit function: a module can mint zero
 * CloneIds (cap == 0, no hit calls emitted) — the definition must not warn
 * when no call survives. The `#if` guards compile the report loops out in
 * that case (the zero-trip loops otherwise warn under some cc versions);
 * the `[clone-sites] cap=0 ...` header line still prints.
 */
#define GORGET_CLONE_SITE_CAP ((int64_t)__GORGET_CLONE_SITE_CAP__)
static _Atomic size_t __gorget_clone_site_counts[__GORGET_CLONE_SITE_CAP__ + 1];
__attribute__((unused)) static void __gorget_clone_site_hit(int64_t id) {
    if (id >= 0 && id < GORGET_CLONE_SITE_CAP) {
        __gorget_clone_site_counts[id]++;
    }
}
#if __GORGET_CLONE_SITE_CAP__ > 0
static int __gorget_clone_site_cmp(const void* a, const void* b) {
    size_t ca = __gorget_clone_site_counts[*(const int64_t*)a];
    size_t cb = __gorget_clone_site_counts[*(const int64_t*)b];
    if (ca < cb) return 1;
    if (ca > cb) return -1;
    /* deterministic tiebreak: lower id first */
    return (int)(*(const int64_t*)a - *(const int64_t*)b);
}
#endif
static void __gorget_clone_sites_report(void) {
    size_t total = 0, distinct = 0;
#if __GORGET_CLONE_SITE_CAP__ > 0
    static int64_t idx[__GORGET_CLONE_SITE_CAP__ + 1];
    for (int64_t i = 0; i < GORGET_CLONE_SITE_CAP; i++) {
        idx[i] = i;
        size_t c = __gorget_clone_site_counts[i];
        total += c;
        if (c > 0) distinct++;
    }
#endif
    fprintf(stderr, "[clone-sites] cap=%lld distinct=%zu total_site_hits=%zu\n",
            (long long)GORGET_CLONE_SITE_CAP, distinct, total);
#if __GORGET_CLONE_SITE_CAP__ > 0
    qsort(idx, (size_t)GORGET_CLONE_SITE_CAP, sizeof(int64_t), __gorget_clone_site_cmp);
    int64_t top = 50;
    const char* top_env = getenv("GG_CLONE_SITES_TOP");
    if (top_env && *top_env) {
        top = strtoll(top_env, NULL, 10);
        if (top <= 0) top = GORGET_CLONE_SITE_CAP; /* 0 = all nonzero sites */
    }
    int64_t shown = 0;
    for (int64_t i = 0; i < GORGET_CLONE_SITE_CAP && shown < top; i++) {
        size_t c = __gorget_clone_site_counts[idx[i]];
        if (c == 0) break;
        fprintf(stderr, "[clone-site] #%lld=%zu\n", (long long)idx[i], c);
        shown++;
    }
    if ((size_t)shown < distinct) {
        fprintf(stderr,
                "[clone-sites] ... %zu more nonzero site(s) suppressed (set GG_CLONE_SITES_TOP=0 for all)\n",
                distinct - (size_t)shown);
    }
#endif
}
__attribute__((constructor)) static void __gorget_register_clone_sites(void) {
    atexit(__gorget_clone_sites_report);
}

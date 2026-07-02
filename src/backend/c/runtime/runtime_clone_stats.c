
static size_t __gorget_peak_rss_kb(void) {
    #ifdef __linux__
    FILE* f = fopen("/proc/self/status", "r");
    if (!f) return 0;
    char line[256];
    size_t vm_hwm_kb = 0;
    while (fgets(line, sizeof(line), f)) {
        if (strncmp(line, "VmHWM:", 6) == 0) {
            sscanf(line + 6, "%zu", &vm_hwm_kb);
            break;
        }
    }
    fclose(f);
    return vm_hwm_kb;
    #else
    return 0;  // Darwin/BSD: would use getrusage RU_MAXRSS (bytes on Darwin, kb on Linux)
    #endif
}
static void __gorget_clone_stats_report(void) {
    size_t peak_rss_kb = __gorget_peak_rss_kb();
    fprintf(stderr, "[clone-stats] array_clone=%zu map_clone=%zu set_clone=%zu string_clone=%zu closure_clone=%zu string_cow=%zu string_cat=%zu box_alloc=%zu array_new=%zu string_new=%zu total_allocs=%zu total_frees=%zu live_bytes=%zu peak_rss_kb=%zu\n",
        __gorget_array_clone_count, __gorget_map_clone_count, __gorget_set_clone_count,
        __gorget_string_clone_count,
        __gorget_closure_clone_count,
        __gorget_string_cow_count, __gorget_str_cat_count, __gorget_box_alloc_count,
        __gorget_array_new_count, __gorget_string_new_count,
        __gorget_alloc_count, __gorget_free_count,
        __gorget_total_allocated - __gorget_total_freed,
        peak_rss_kb);
}
__attribute__((constructor)) static void __gorget_register_clone_stats(void) {
    atexit(__gorget_clone_stats_report);
}

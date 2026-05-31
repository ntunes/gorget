
static void __gorget_alloc_report(void) {
    fprintf(stderr, "\n[alloc-report] allocs=%zu frees=%zu live_bytes=%zu\n",
        __gorget_alloc_count, __gorget_free_count,
        __gorget_total_allocated - __gorget_total_freed);
    fprintf(stderr, "[alloc-report] array: new=%zu clone=%zu free=%zu (net=%zu)\n",
        __gorget_array_new_count, __gorget_array_clone_count, __gorget_array_free_count,
        __gorget_array_new_count + __gorget_array_clone_count - __gorget_array_free_count);
    fprintf(stderr, "[alloc-report] string: new=%zu cat=%zu free=%zu (net=%zu)\n",
        __gorget_string_new_count, __gorget_str_cat_count, __gorget_string_free_count,
        __gorget_string_new_count + __gorget_str_cat_count - __gorget_string_free_count);
    fprintf(stderr, "[alloc-report] size buckets (alloc count by size range):\n");
    const char* labels[] = {"1-15","16-31","32-47","48-63","64-79","80-95","96-111","112-127",
                            "128-143","144-159","160-175","176-191","192-207","208-223","224-239","240+"};
    for (int i = 0; i < 16; i++) {
        if (__gorget_size_buckets[i] > 0)
            fprintf(stderr, "  %s bytes: %zu allocs\n", labels[i], __gorget_size_buckets[i]);
    }
}
__attribute__((constructor)) static void __gorget_register_alloc_report(void) {
    atexit(__gorget_alloc_report);
}

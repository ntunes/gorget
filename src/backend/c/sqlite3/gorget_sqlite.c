// Gorget SQLite wrapper functions.
// Included in generated C output after the SQLite amalgamation and Gorget runtime.
// All database/statement handles are sqlite3*/sqlite3_stmt* cast to int64_t.

// ── Open / close ─────────────────────────────────────────────

static int64_t gorget_sqlite_open(Str path) {
    char path_buf[4096];
    size_t len = path.len < 4095 ? path.len : 4095;
    memcpy(path_buf, path.data, len);
    path_buf[len] = '\0';

    sqlite3* db = NULL;
    int rc = sqlite3_open(path_buf, &db);
    if (rc != SQLITE_OK) {
        if (db) sqlite3_close(db);
        return 0;
    }
    return (int64_t)(intptr_t)db;
}

static Str gorget_sqlite_errmsg(int64_t db_handle) {
    if (!db_handle) return gorget_str_from_literal("no database", 11);
    sqlite3* db = (sqlite3*)(intptr_t)db_handle;
    const char* msg = sqlite3_errmsg(db);
    if (!msg) return gorget_str_from_literal("", 0);
    size_t len = strlen(msg);
    char* buf = (char*)GORGET_ALLOC(len + 1);
    memcpy(buf, msg, len + 1);
    return (Str){.data = buf, .len = len};
}

static void gorget_sqlite_close(int64_t db_handle) {
    if (!db_handle) return;
    sqlite3* db = (sqlite3*)(intptr_t)db_handle;
    sqlite3_close(db);
}

// ── Prepare ──────────────────────────────────────────────────

static int64_t gorget_sqlite_prepare(int64_t db_handle, Str sql) {
    if (!db_handle) return 0;
    sqlite3* db = (sqlite3*)(intptr_t)db_handle;
    sqlite3_stmt* stmt = NULL;
    int rc = sqlite3_prepare_v2(db, sql.data, (int)sql.len, &stmt, NULL);
    if (rc != SQLITE_OK) return 0;
    return (int64_t)(intptr_t)stmt;
}

// ── Bind ─────────────────────────────────────────────────────

static void gorget_sqlite_bind_int(int64_t stmt_handle, int64_t idx, int64_t val) {
    if (!stmt_handle) return;
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    sqlite3_bind_int64(stmt, (int)idx, val);
}

static void gorget_sqlite_bind_float(int64_t stmt_handle, int64_t idx, double val) {
    if (!stmt_handle) return;
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    sqlite3_bind_double(stmt, (int)idx, val);
}

static void gorget_sqlite_bind_str(int64_t stmt_handle, int64_t idx, Str val) {
    if (!stmt_handle) return;
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    // SQLITE_TRANSIENT: SQLite copies the string before returning
    sqlite3_bind_text(stmt, (int)idx, val.data, (int)val.len, SQLITE_TRANSIENT);
}

static void gorget_sqlite_bind_null(int64_t stmt_handle, int64_t idx) {
    if (!stmt_handle) return;
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    sqlite3_bind_null(stmt, (int)idx);
}

// ── Step ─────────────────────────────────────────────────────

// Returns: 1 = row available, 0 = done, -1 = error
static int64_t gorget_sqlite_step(int64_t stmt_handle) {
    if (!stmt_handle) return -1;
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    int rc = sqlite3_step(stmt);
    if (rc == SQLITE_ROW)  return 1;
    if (rc == SQLITE_DONE) return 0;
    return -1;
}

// ── Column access ─────────────────────────────────────────────

static int64_t gorget_sqlite_column_count(int64_t stmt_handle) {
    if (!stmt_handle) return 0;
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    return (int64_t)sqlite3_column_count(stmt);
}

static Str gorget_sqlite_column_name(int64_t stmt_handle, int64_t col) {
    if (!stmt_handle) return gorget_str_from_literal("", 0);
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    const char* name = sqlite3_column_name(stmt, (int)col);
    if (!name) return gorget_str_from_literal("", 0);
    size_t len = strlen(name);
    char* buf = (char*)GORGET_ALLOC(len + 1);
    memcpy(buf, name, len + 1);
    return (Str){.data = buf, .len = len};
}

static Str gorget_sqlite_column_text(int64_t stmt_handle, int64_t col) {
    if (!stmt_handle) return gorget_str_from_literal("", 0);
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    const char* text = (const char*)sqlite3_column_text(stmt, (int)col);
    if (!text) return gorget_str_from_literal("", 0);
    size_t len = strlen(text);
    char* buf = (char*)GORGET_ALLOC(len + 1);
    memcpy(buf, text, len + 1);
    return (Str){.data = buf, .len = len};
}

// ── Finalize ─────────────────────────────────────────────────

static void gorget_sqlite_finalize(int64_t stmt_handle) {
    if (!stmt_handle) return;
    sqlite3_stmt* stmt = (sqlite3_stmt*)(intptr_t)stmt_handle;
    sqlite3_finalize(stmt);
}

// ── Simple exec (no params, returns rows-changed or -1 on error) ──

static int64_t gorget_sqlite_exec_simple(int64_t db_handle, Str sql) {
    if (!db_handle) return -1;
    sqlite3* db = (sqlite3*)(intptr_t)db_handle;
    char* errmsg = NULL;
    // NULL-terminate sql
    char* sql_cstr = (char*)GORGET_ALLOC(sql.len + 1);
    memcpy(sql_cstr, sql.data, sql.len);
    sql_cstr[sql.len] = '\0';
    int rc = sqlite3_exec(db, sql_cstr, NULL, NULL, &errmsg);
    GORGET_FREE(sql_cstr, sql.len + 1);
    if (errmsg) sqlite3_free(errmsg);
    if (rc != SQLITE_OK) return -1;
    return (int64_t)sqlite3_changes(db);
}

// ── Changes ──────────────────────────────────────────────────

static int64_t gorget_sqlite_changes(int64_t db_handle) {
    if (!db_handle) return 0;
    sqlite3* db = (sqlite3*)(intptr_t)db_handle;
    return (int64_t)sqlite3_changes(db);
}

/// Runtime function implementations for the GIR interpreter.
/// This module handles all `CallExtern` dispatch.

use std::cell::Cell;

use crate::ir::Module;
use super::error::{SimError, SimResult};
use super::value::{SimStr, SimString, SimArray, Value};

/// FNV-1a hash over raw bytes — mirrors the C `__gorget_fnv1a` implementation.
fn fnv1a_hash(data: &[u8]) -> u64 {
    let mut hash = 14695981039346656037u64;
    for &b in data {
        hash ^= b as u64;
        hash = hash.wrapping_mul(1099511628211);
    }
    hash
}

// Splitmix64 PRNG — mirrors the C __gorget_rng_state global.
thread_local! {
    static RNG_STATE: Cell<u64> = Cell::new(0);
    /// Program name used by gorget_args(). Set to the source file path.
    static PROGRAM_NAME: std::cell::RefCell<String> = std::cell::RefCell::new(String::new());
    /// Whether isolation mode is active (blocks real I/O). Default: true.
    static ISOLATION: Cell<bool> = Cell::new(true);
    /// Monotonically increasing fake time counter for deterministic time in isolation.
    static FAKE_TIME_MS: Cell<i64> = Cell::new(1_000_000);
}

/// Set the program name returned by gorget_args(). Call before interpret().
pub fn set_program_name(name: &str) {
    PROGRAM_NAME.with(|c| *c.borrow_mut() = name.to_string());
}

/// Seed the interpreter's PRNG. Called from interpret() when --seed=N is given.
pub fn seed_rng(seed: u64) {
    RNG_STATE.with(|s| s.set(seed));
}

/// Set whether isolation mode is active. When true, real I/O operations are blocked.
pub fn set_isolation(on: bool) {
    ISOLATION.with(|c| c.set(on));
    // Reset fake time counter each run for determinism.
    FAKE_TIME_MS.with(|c| c.set(1_000_000));
}

/// Check isolation mode; return IsolationViolation error if active.
/// Also callable from dispatch.rs for network/socket handlers.
pub fn check_isolation(op: &str) -> SimResult<()> {
    ISOLATION.with(|c| {
        if c.get() {
            Err(SimError::IsolationViolation {
                operation: op.to_string(),
                hint: "use --disable-isolation to allow real I/O".to_string(),
            })
        } else {
            Ok(())
        }
    })
}

/// Return the next fake millisecond timestamp (monotonically increasing).
fn next_fake_time_ms() -> i64 {
    FAKE_TIME_MS.with(|c| {
        let t = c.get();
        c.set(t + 1);
        t
    })
}

fn splitmix64_rand() -> i64 {
    RNG_STATE.with(|s| {
        let state = s.get().wrapping_add(0x9e3779b97f4a7c15u64);
        s.set(state);
        let mut z = state;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9u64);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111ebu64);
        z = z ^ (z >> 31);
        // Gorget masks with INT64_MAX to always return non-negative
        (z & i64::MAX as u64) as i64
    })
}

/// Mirror of the C gorget_path_parent function.
fn path_parent(s: &str) -> std::string::String {
    if s.is_empty() { return ".".to_string(); }
    let bytes = s.as_bytes();
    let mut len = bytes.len();
    // Strip trailing slashes (keep at least 1)
    while len > 1 && bytes[len - 1] == b'/' { len -= 1; }
    // Find last slash
    let mut i = len;
    while i > 0 && bytes[i - 1] != b'/' { i -= 1; }
    if i == 0 { return ".".to_string(); }
    if i == 1 { return "/".to_string(); }
    std::string::String::from_utf8_lossy(&bytes[..i - 1]).into_owned()
}

/// Mirror of the C gorget_path_basename function.
fn path_basename(s: &str) -> std::string::String {
    if s.is_empty() { return std::string::String::new(); }
    let bytes = s.as_bytes();
    let mut len = bytes.len();
    // Strip trailing slashes (keep at least 1)
    while len > 1 && bytes[len - 1] == b'/' { len -= 1; }
    // Find last slash
    let mut i = len;
    while i > 0 && bytes[i - 1] != b'/' { i -= 1; }
    std::string::String::from_utf8_lossy(&bytes[i..len]).into_owned()
}

/// Mirror of C gorget_path_extension (given a basename).
fn path_extension_of_base(base: &str) -> std::string::String {
    if base.is_empty() { return std::string::String::new(); }
    let bytes = base.as_bytes();
    let start = if bytes[0] == b'.' { 1 } else { 0 };
    // Find last dot
    let dot_pos = bytes[start..].iter().rposition(|&b| b == b'.').map(|p| p + start);
    match dot_pos {
        None => std::string::String::new(),
        Some(pos) => std::string::String::from_utf8_lossy(&bytes[pos + 1..]).into_owned(),
    }
}

/// Mirror of C gorget_path_stem (given a basename).
fn path_stem_of_base(base: &str) -> std::string::String {
    if base.is_empty() { return std::string::String::new(); }
    let bytes = base.as_bytes();
    let start = if bytes[0] == b'.' { 1 } else { 0 };
    let dot_pos = bytes[start..].iter().rposition(|&b| b == b'.').map(|p| p + start);
    match dot_pos {
        None => base.to_string(),
        Some(pos) => std::string::String::from_utf8_lossy(&bytes[..pos]).into_owned(),
    }
}

/// Mirror of C gorget_path_normalize.
fn path_normalize(path: &str) -> std::string::String {
    if path.is_empty() { return ".".to_string(); }
    let absolute = path.starts_with('/');
    let mut stack: Vec<&str> = Vec::new();
    for component in path.split('/') {
        match component {
            "" | "." => {}
            ".." => { stack.pop(); }
            c => stack.push(c),
        }
    }
    if stack.is_empty() {
        if absolute { return "/".to_string(); }
        return ".".to_string();
    }
    let joined = stack.join("/");
    if absolute { format!("/{}", joined) } else { joined }
}

/// Mirror of C gorget_path_absolute.
fn path_absolute(path: &str) -> std::string::String {
    if path.starts_with('/') {
        return path_normalize(path);
    }
    let cwd = std::env::current_dir()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();
    path_normalize(&format!("{}/{}", cwd, path))
}

/// Mirror of C gorget_path_join.
fn path_join(a: &str, b: &str) -> std::string::String {
    if a.is_empty() { return b.to_string(); }
    if b.is_empty() { return a.to_string(); }
    let a = a.trim_end_matches('/');
    // Strip leading slashes from b (NOT treating absolute paths as replacements)
    let b = b.trim_start_matches('/');
    format!("{}/{}", a, b)
}

/// Format an epoch timestamp using a strftime-like format string (common patterns only).
fn format_epoch_time(epoch: i64, fmt: &str) -> std::string::String {
    // Convert epoch to broken-down time (UTC, no external dependencies).
    // Uses a simple algorithm for common date range (1970-2100).
    let secs = epoch;
    let (year, month, day, hour, min, sec) = epoch_to_ymd_hms(secs);
    let mut out = std::string::String::new();
    let mut chars = fmt.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some('Y') => out.push_str(&format!("{:04}", year)),
                Some('m') => out.push_str(&format!("{:02}", month)),
                Some('d') => out.push_str(&format!("{:02}", day)),
                Some('H') => out.push_str(&format!("{:02}", hour)),
                Some('M') => out.push_str(&format!("{:02}", min)),
                Some('S') => out.push_str(&format!("{:02}", sec)),
                Some('%') => out.push('%'),
                Some(other) => { out.push('%'); out.push(other); }
                None => out.push('%'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Decompose Unix epoch to (year, month, day, hour, min, sec) in local time.
/// Simplified: uses UTC (same as the C `localtime_r` but without timezone offset).
fn epoch_to_ymd_hms(epoch: i64) -> (i32, u32, u32, u32, u32, u32) {
    let sec = (epoch % 60).unsigned_abs() as u32;
    let epoch_mins = epoch / 60;
    let min = (epoch_mins % 60).unsigned_abs() as u32;
    let epoch_hours = epoch_mins / 60;
    let hour = (epoch_hours % 24).unsigned_abs() as u32;
    let epoch_days = epoch_hours / 24;

    // Compute year/month/day from days since epoch (2000-based).
    let z = epoch_days + 719468;
    let era = (if z >= 0 { z } else { z - 146096 }) / 146097;
    let doe = (z - era * 146097) as u64;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = y + if m <= 2 { 1 } else { 0 };
    (year as i32, m as u32, d as u32, hour, min, sec)
}

/// Parse a date string using a strftime-like format, return Unix epoch (-1 on failure).
fn parse_epoch_time(s: &str, fmt: &str) -> i64 {
    let mut year = 1970i32;
    let mut month = 1u32;
    let mut day = 1u32;
    let mut hour = 0u32;
    let mut min = 0u32;
    let mut sec = 0u32;

    let mut si = 0usize;
    let s_bytes = s.as_bytes();
    let mut fmt_chars = fmt.chars().peekable();

    while let Some(fc) = fmt_chars.next() {
        if fc == '%' {
            let spec = fmt_chars.next();
            let width = match spec {
                Some('Y') => 4,
                Some('m') | Some('d') | Some('H') | Some('M') | Some('S') => 2,
                _ => 0,
            };
            if width == 0 { continue; }
            // Skip leading whitespace in input
            while si < s_bytes.len() && s_bytes[si] == b' ' { si += 1; }
            // Read up to `width` digits
            let start = si;
            while si < s_bytes.len() && (s_bytes[si] as char).is_ascii_digit() && si - start < width {
                si += 1;
            }
            if si == start { return -1; } // no digits found
            let val: u32 = match std::str::from_utf8(&s_bytes[start..si]).unwrap_or("").parse() {
                Ok(v) => v,
                Err(_) => return -1,
            };
            match spec {
                Some('Y') => year = val as i32,
                Some('m') => month = val,
                Some('d') => day = val,
                Some('H') => hour = val,
                Some('M') => min = val,
                Some('S') => sec = val,
                _ => {}
            }
        } else {
            // Literal character — must match
            if si >= s_bytes.len() { return -1; }
            if s_bytes[si] != fc as u8 { return -1; }
            si += 1;
        }
    }

    // Convert to epoch
    ymd_hms_to_epoch(year, month, day, hour, min, sec)
}

/// Convert (year, month, day, hour, min, sec) to Unix epoch seconds (UTC).
fn ymd_hms_to_epoch(year: i32, month: u32, day: u32, hour: u32, min: u32, sec: u32) -> i64 {
    // Days from epoch to year/month/day (Gregorian calendar)
    let (y, m) = if month <= 2 {
        (year as i64 - 1, month as i64 + 9)
    } else {
        (year as i64, month as i64 - 3)
    };
    let era = (if y >= 0 { y } else { y - 399 }) / 400;
    let yoe = y - era * 400;
    let doy = (153 * m + 2) / 5 + day as i64 - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days = era * 146097 + doe - 719468;
    days * 86400 + hour as i64 * 3600 + min as i64 * 60 + sec as i64
}

/// Parse and execute a printf-family call.
/// `format` is the raw format string; `args` are the evaluated GIR arguments (one per %specifier,
/// except %.*s which consumes one Str/String value but prints with both len and data).
pub fn do_printf(format: &str, args: &[Value]) -> SimResult<std::string::String> {
    let mut out = std::string::String::new();
    let bytes = format.as_bytes();
    let mut i = 0;
    let mut arg_idx = 0;

    while i < bytes.len() {
        if bytes[i] != b'%' {
            // GIR format strings already contain the actual bytes (processed by the
            // gorget lexer). Pass them through as-is; do NOT re-interpret escapes.
            out.push(bytes[i] as char);
            i += 1;
            continue;
        }

        // We have a '%'
        i += 1;
        if i >= bytes.len() { break; }

        if bytes[i] == b'%' {
            out.push('%');
            i += 1;
            continue;
        }

        // Collect flags: -, +, space, 0, #
        while i < bytes.len() && matches!(bytes[i], b'-' | b'+' | b' ' | b'0' | b'#') {
            i += 1;
        }

        // Width (digits or *)
        let mut has_star_width = false;
        if i < bytes.len() && bytes[i] == b'*' {
            has_star_width = true;
            i += 1;
        } else {
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
        }

        // Precision (.digits or .*)
        let mut has_star_precision = false;
        if i < bytes.len() && bytes[i] == b'.' {
            i += 1;
            if i < bytes.len() && bytes[i] == b'*' {
                has_star_precision = true;
                i += 1;
            } else {
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
            }
        }

        // Length modifier: l, ll, h, hh, z, L, j, t, q
        let mut _length = std::string::String::new();
        while i < bytes.len() && matches!(bytes[i], b'l' | b'h' | b'z' | b'L' | b'j' | b't' | b'q') {
            _length.push(bytes[i] as char);
            i += 1;
        }

        if i >= bytes.len() { break; }
        let conversion = bytes[i] as char;
        i += 1;

        // Special case: %.*s (has_star_precision = true, conversion = 's')
        // In GIR, this corresponds to ONE Str/GorgetString arg.
        if has_star_precision && conversion == 's' {
            let arg = args.get(arg_idx).unwrap_or(&Value::Null);
            arg_idx += 1;
            match arg {
                Value::Str(s) => out.push_str(s.as_str()),
                Value::String(s) => out.push_str(s.as_str()),
                Value::CStr(s) => out.push_str(s.as_str()),
                Value::Null => out.push_str("(null)"),
                _ => out.push_str(&format_value_for_s(arg)),
            }
            continue;
        }

        // Handle star width (rare in GIR output, but handle gracefully)
        if has_star_width {
            arg_idx += 1; // consume width arg
        }

        match conversion {
            'd' | 'i' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::I64(0));
                arg_idx += 1;
                // The GIR sometimes emits %lld for values that are actually floats
                // (e.g. abs(-2.5) inferred as I64 in GIR but returns F64 at runtime).
                // The C backend patches %lld → %f for float results; we mimic that here.
                // Similarly, when a Str/String value is passed to %lld (because the GIR
                // type was incorrectly inferred as I64), print the string content instead.
                if arg.is_float() {
                    out.push_str(&format!("{:.6}", arg.as_f64()));
                } else if arg.is_string() {
                    out.push_str(arg.as_str_content());
                } else {
                    out.push_str(&arg.as_i64().to_string());
                }
            }
            'u' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::U64(0));
                arg_idx += 1;
                out.push_str(&arg.as_u64().to_string());
            }
            'f' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::F64(0.0));
                arg_idx += 1;
                let f = arg.as_f64();
                // Match C printf %f: 6 decimal places by default
                out.push_str(&format!("{:.6}", f));
            }
            'e' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::F64(0.0));
                arg_idx += 1;
                let f = arg.as_f64();
                out.push_str(&format!("{:e}", f));
            }
            'g' | 'G' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::F64(0.0));
                arg_idx += 1;
                let f = arg.as_f64();
                out.push_str(&format_g(f));
            }
            's' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::Null);
                arg_idx += 1;
                match arg {
                    Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
                    Value::Str(s) => out.push_str(s.as_str()),
                    Value::String(s) => out.push_str(s.as_str()),
                    Value::CStr(s) => out.push_str(s.as_str()),
                    Value::Null => out.push_str("(null)"),
                    // Bool value passed as arg to %s: format as true/false
                    _ => out.push_str(&format_value_for_s(arg)),
                }
            }
            'c' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::Char(0));
                arg_idx += 1;
                let cp = match arg {
                    Value::Char(c) => *c,
                    _ => arg.as_i64() as u32,
                };
                if let Some(ch) = char::from_u32(cp) {
                    out.push(ch);
                } else {
                    out.push('?');
                }
            }
            'x' | 'X' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::I64(0));
                arg_idx += 1;
                let v = arg.as_u64();
                if conversion == 'x' {
                    out.push_str(&format!("{:x}", v));
                } else {
                    out.push_str(&format!("{:X}", v));
                }
            }
            'o' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::I64(0));
                arg_idx += 1;
                out.push_str(&format!("{:o}", arg.as_u64()));
            }
            'p' => {
                let arg = args.get(arg_idx).unwrap_or(&Value::Ptr(0));
                arg_idx += 1;
                out.push_str(&format!("0x{:x}", arg.as_u64()));
            }
            'z' | 'Z' => {
                // %zu — size_t (unsigned)
                let arg = args.get(arg_idx).unwrap_or(&Value::U64(0));
                arg_idx += 1;
                out.push_str(&arg.as_u64().to_string());
            }
            'n' => {
                // %n — writes length to pointer arg, skip in interpreter
                arg_idx += 1;
            }
            _ => {
                // Unknown specifier — emit literally
                out.push('%');
                out.push(conversion);
            }
        }
    }

    Ok(out)
}

/// Format a float using C's %g rules:
/// - If the value can be represented without exponential notation compactly, use decimal.
/// - Otherwise use exponential notation.
fn format_g(f: f64) -> std::string::String {
    if f == 0.0 {
        return "0".to_string();
    }
    let abs = f.abs();
    // C %g uses exponential if exponent < -4 or >= precision (default 6)
    if abs >= 1e-4 && abs < 1e6 {
        // Use enough significant digits, strip trailing zeros
        let s = format!("{:.6}", f);
        // Strip trailing zeros after decimal point
        if s.contains('.') {
            let s = s.trim_end_matches('0');
            let s = s.trim_end_matches('.');
            s.to_string()
        } else {
            s
        }
    } else {
        // Exponential notation
        let s = format!("{:e}", f);
        // Normalize to C-style: 1e+06 etc.
        normalize_exp_notation(&s)
    }
}

fn normalize_exp_notation(s: &str) -> std::string::String {
    // Rust uses "1e6", C uses "1e+06"
    if let Some(pos) = s.find('e') {
        let mantissa = &s[..pos];
        let exp_str = &s[pos + 1..];
        let (sign, digits) = if exp_str.starts_with('-') {
            ("-", &exp_str[1..])
        } else if exp_str.starts_with('+') {
            ("+", &exp_str[1..])
        } else {
            ("+", exp_str)
        };
        // Pad to at least 2 digits
        let padded = format!("{:0>2}", digits);
        format!("{mantissa}e{sign}{padded}")
    } else {
        s.to_string()
    }
}

/// Format a value for %s (when it's not a string type).
fn format_value_for_s(v: &Value) -> std::string::String {
    match v {
        Value::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Value::I8(n) => n.to_string(),
        Value::I16(n) => n.to_string(),
        Value::I32(n) => n.to_string(),
        Value::I64(n) => n.to_string(),
        Value::U8(n) => n.to_string(),
        Value::U16(n) => n.to_string(),
        Value::U32(n) => n.to_string(),
        Value::U64(n) => n.to_string(),
        Value::F32(f) => format_g(*f as f64),
        Value::F64(f) => format_g(*f),
        Value::Char(c) => char::from_u32(*c).unwrap_or('\0').to_string(),
        Value::Unit => std::string::String::new(),
        Value::Null => "(null)".to_string(),
        Value::Enum { fields, .. } => {
            // For Option/Result: unwrap the payload if there's exactly one field.
            if fields.len() == 1 {
                format_value_for_s(&fields[0])
            } else {
                std::string::String::new()
            }
        }
        Value::Struct { fields, .. } => {
            // For newtype wrappers: forward to the inner value.
            if fields.len() == 1 {
                format_value_for_s(&fields[0])
            } else {
                std::string::String::new()
            }
        }
        Value::Tuple(elems) => {
            if elems.len() == 1 { format_value_for_s(&elems[0]) }
            else { std::string::String::new() }
        }
        Value::Str(s) => s.as_str().to_string(),
        Value::String(s) => s.as_str().to_string(),
        Value::CStr(s) => s.as_ref().clone(),
        Value::Array(arr) => format!("[array len={}]", arr.len()),
        Value::Dict(d) => format!("[dict len={}]", d.len()),
        _ => std::string::String::new(),
    }
}

/// Main runtime dispatch: given a function name and evaluated args, return the result Value.
pub fn call_extern(
    name: &str,
    args: Vec<Value>,
    module: &Module,
    stdout: &mut Vec<u8>,
    stderr: &mut Vec<u8>,
    depth: usize,
) -> SimResult<Value> {
    match name {
        // ── len() free function — dispatches on argument type ──────────────────
        "len" => {
            let val = args.into_iter().next().unwrap_or(Value::Unit);
            let n = match &val {
                Value::Array(arr) => arr.len() as i64,
                Value::Dict(d) => d.len() as i64,
                Value::Str(s) => s.codepoint_count() as i64,
                Value::String(s) => {
                    let _bytes = s.as_str().len();
                    // codepoint count
                    s.as_str().chars().count() as i64
                }
                // Dispatch to user struct's __len method by returning sentinel.
                // The call_function path handles user struct types directly,
                // so this fallback covers cases where len() reaches runtime.
                // For structs, we look for a field called "len" or size.
                Value::Struct { fields, .. } => {
                    // Try field 0 as length (common pattern: Buffer{size: N})
                    fields.first().map(|v| v.as_i64()).unwrap_or(0)
                }
                _ => 0,
            };
            return Ok(Value::I64(n));
        }

        // ── Print ─────────────────────────────────────────────────────────────
        "printf" | "gorget_printf" => {
            if args.is_empty() { return Ok(Value::Unit); }
            let fmt = match &args[0] {
                Value::Str(s) => s.as_str().to_string(),
                Value::String(s) => s.as_str().to_string(),
                Value::CStr(s) => s.as_str().to_string(),
                _ => return Err(SimError::TypeMismatch {
                    expected: "str (format string)".into(),
                    got: args[0].type_name().into(),
                }),
            };
            let formatted = do_printf(&fmt, &args[1..])?;
            stdout.extend_from_slice(formatted.as_bytes());
            Ok(Value::I32(formatted.len() as i32))
        }

        "fprintf_stderr" => {
            // First arg is NULL (stderr placeholder), second is format, rest are args
            let (fmt, fmt_args) = if args.len() >= 2 {
                let fmt = match &args[1] {
                    Value::Str(s) => s.as_str().to_string(),
                    Value::String(s) => s.as_str().to_string(),
                    Value::CStr(s) => s.as_str().to_string(),
                    _ => String::new(),
                };
                (fmt, &args[2..])
            } else {
                (String::new(), &args[..0])
            };
            let formatted = do_printf(&fmt, fmt_args)?;
            stderr.extend_from_slice(formatted.as_bytes());
            Ok(Value::I32(formatted.len() as i32))
        }

        // ── String construction ────────────────────────────────────────────────
        "gorget_str_from_literal" => {
            // gorget_str_from_literal(const char* data, size_t len) → Str
            // In GIR, this may appear as CallExtern with Constant::Str arg
            let s = match args.first() {
                Some(Value::Str(s)) => s.as_str().to_string(),
                Some(Value::CStr(s)) => s.as_str().to_string(),
                Some(Value::String(s)) => s.as_str().to_string(),
                _ => String::new(),
            };
            Ok(Value::Str(SimStr::from_string(s)))
        }

        "gorget_str_from_cstr" => {
            let s = match args.first() {
                Some(Value::CStr(s)) => s.as_str().to_string(),
                Some(Value::Str(s)) => s.as_str().to_string(),
                Some(Value::String(s)) => s.as_str().to_string(),
                _ => String::new(),
            };
            Ok(Value::Str(SimStr::from_string(s)))
        }

        "gorget_string_from_str" | "gorget_string_new" => {
            let s = match args.first() {
                Some(Value::Str(s)) => s.as_str().to_string(),
                Some(Value::String(s)) => s.as_str().to_string(),
                Some(Value::CStr(s)) => s.as_str().to_string(),
                _ => String::new(),
            };
            Ok(Value::String(SimString::from_string(s)))
        }

        "gorget_string_with_capacity" => {
            let cap = args.first().map(|v| v.as_i64() as usize).unwrap_or(0);
            Ok(Value::String(SimString::with_capacity(cap)))
        }

        "gorget_string_free" | "gorget_array_free" | "gorget_dict_free" | "gorget_map_free" | "gorget_set_free" => {
            // No-op: memory is managed by Rust's GC
            Ok(Value::Unit)
        }

        // CoW materialization primitives. The native runtime distinguishes
        // view (cap=0) vs owned (cap>0) — sim values are always owned-shaped
        // (Rust String/Vec), so these collapse to "produce an independent
        // owned String from the source bytes". The compiler inserts these
        // calls at boundary materialization sites (struct/enum init, field
        // store, return, etc.) — see `docs/devbook/11-copy-on-write.md`.
        "gorget_string_clone_to_owned"
        | "gorget_string_copy_cow"
        | "gorget_string_borrow"
        | "gorget_string_materialize_inplace"
        | "gorget_string_clone_inplace" => {
            // The Ptr arg is auto-dereffed by `is_string_method_call` upstream
            // (dispatch.rs); we receive the underlying Str/String value here.
            let s = args.first()
                .and_then(Value::try_to_sim_str)
                .map(|s| s.as_str().to_string())
                .unwrap_or_default();
            Ok(Value::String(SimString::from_string(s)))
        }

        // Assert-failure diagnostic formatter. Native panics with both sides
        // formatted; sim returns an `unwrap on None/Err`-style error so the
        // outer test harness picks it up uniformly with the assertion site.
        "gorget_assert_fail_values" => {
            // D11 (approach a): the leading arg is now the `T_AssertFailed`
            // trap code — the op/left/right shifted to args[1..4].
            let op = args.get(1).and_then(Value::try_to_sim_str)
                .map(|s| s.as_str().to_string()).unwrap_or_else(|| "?".into());
            let left = args.get(2).and_then(Value::try_to_sim_str)
                .map(|s| s.as_str().to_string()).unwrap_or_default();
            let right = args.get(3).and_then(Value::try_to_sim_str)
                .map(|s| s.as_str().to_string()).unwrap_or_default();
            Err(super::error::SimError::Panic(format!(
                "assertion failed: left {op} right\n  left:  {left}\n  right: {right}"
            )))
        }

        // ── String operations ──────────────────────────────────────────────────
        "gorget_str_eq" => {
            let a_s = args.get(0).map(Value::try_to_sim_str).and_then(|v| v).unwrap_or_else(|| SimStr::from_str(""));
            let b_s = args.get(1).map(Value::try_to_sim_str).and_then(|v| v).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(a_s.as_str() == b_s.as_str()))
        }

        "gorget_str_cmp" => {
            let a_s = args.get(0).map(Value::try_to_sim_str).and_then(|v| v).unwrap_or_else(|| SimStr::from_str(""));
            let b_s = args.get(1).map(Value::try_to_sim_str).and_then(|v| v).unwrap_or_else(|| SimStr::from_str(""));
            let cmp = a_s.as_str().cmp(b_s.as_str()) as i32;
            Ok(Value::I32(cmp))
        }

        "gorget_str_cat" => {
            // Robust: treat any non-string arg as empty string (avoids panic on Unit from
            // unimplemented functions returning the wrong type).
            fn val_to_str_safe(v: &Value) -> std::string::String {
                match v {
                    Value::Str(s) => s.as_str().to_string(),
                    Value::String(s) => s.as_str().to_string(),
                    Value::CStr(s) => (**s).clone(),
                    _ => std::string::String::new(),
                }
            }
            let a = args.get(0).map(|v| val_to_str_safe(v)).unwrap_or_default();
            let b = args.get(1).map(|v| val_to_str_safe(v)).unwrap_or_default();
            let combined = format!("{a}{b}");
            Ok(Value::String(SimString::from_string(combined)))
        }

        "gorget_str_codepoint_count" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::I64(s.codepoint_count() as i64))
        }

        "gorget_str_byte_len" | "gorget_string_byte_len" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::I64(s.byte_len() as i64))
        }

        "gorget_str_byte_slice" => {
            // gorget_str_byte_slice(Str s, int64_t start, int64_t end) → Str
            // Deref first arg if it's a pointer (GIR passes Ptr to Str for value args).
            let s = match args.first() {
                Some(Value::Str(s)) => s.clone(),
                Some(v) if v.is_string() => SimStr::from_str(v.as_str_content()),
                _ => SimStr::from_str(""),
            };
            let start = args.get(1).map(|v| v.as_i64()).unwrap_or(0);
            let end = args.get(2).map(|v| v.as_i64()).unwrap_or(0);
            let byte_len = s.byte_len() as i64;
            let start = start.max(0).min(byte_len) as usize;
            let end = end.max(0).min(byte_len) as usize;
            let end = end.max(start);
            // Create a byte sub-view preserving the shared buffer.
            let sub = crate::sim::value::SimStr {
                data: s.data.clone(),
                start: s.start + start,
                len: end - start,
            };
            Ok(Value::Str(sub))
        }

        "gorget_str_byte_at" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let idx = args.get(1).map(|v| v.as_i64()).unwrap_or(0);
            let bytes = s.as_bytes();
            let len = bytes.len() as i64;
            let idx = if idx < 0 { len + idx } else { idx };
            if idx >= 0 && (idx as usize) < bytes.len() {
                Ok(Value::U8(bytes[idx as usize]))
            } else {
                Ok(Value::U8(0))
            }
        }

        "gorget_str_len" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::I64(s.codepoint_count() as i64))
        }

        // GorgetString.capacity() → cap field (dispatched here after auto-deref in CallExtern)
        "gorget_str_capacity" | "gorget_string_capacity" => {
            match args.first() {
                Some(Value::String(s)) => Ok(Value::I64(s.capacity() as i64)),
                Some(Value::Str(s)) => Ok(Value::I64(s.byte_len() as i64)),
                _ => Ok(Value::I64(0)),
            }
        }

        "gorget_str_str" | "gorget_string_str" | "gorget_string_as_str" => {
            // GorgetString.str() / GorgetString.as_str() → Str view
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Str(s))
        }

        "gorget_str_to_cstr" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::CStr(std::rc::Rc::new(s.as_str().to_string())))
        }

        "gorget_str_is_empty" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(s.is_empty()))
        }

        "gorget_str_starts_with" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let prefix = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(s.as_str().starts_with(prefix.as_str())))
        }

        "gorget_str_ends_with" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let suffix = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(s.as_str().ends_with(suffix.as_str())))
        }

        "gorget_str_contains" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let needle = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(s.as_str().contains(needle.as_str())))
        }

        "gorget_str_index_of" | "gorget_str_find" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let needle = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let found = s.as_str().find(needle.as_str()).map(|p| {
                s.as_str()[..p].chars().count() as i64
            });
            Ok(match found {
                Some(idx) => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 0, variant: "Some".to_string(),
                    fields: vec![Value::I64(idx)],
                },
                None => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 1, variant: "None".to_string(), fields: vec![],
                },
            })
        }

        "gorget_str_find_from" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let needle = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let from = args.get(2).map(|v| v.as_i64()).unwrap_or(0) as usize;
            // Skip `from` codepoints, then search
            let text = s.as_str();
            let byte_start: usize = text.char_indices().nth(from).map(|(i, _)| i).unwrap_or(text.len());
            let found = text[byte_start..].find(needle.as_str()).map(|p| {
                from as i64 + text[byte_start..byte_start + p].chars().count() as i64
            });
            Ok(match found {
                Some(idx) => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 0, variant: "Some".to_string(),
                    fields: vec![Value::I64(idx)],
                },
                None => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 1, variant: "None".to_string(), fields: vec![],
                },
            })
        }

        "gorget_str_find_ext" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let needle = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let from = args.get(2).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let reverse = args.get(3).map(|v| v.as_bool()).unwrap_or(false);
            let text = s.as_str();
            let found = if !reverse {
                let byte_start: usize = text.char_indices().nth(from).map(|(i, _)| i).unwrap_or(text.len());
                text[byte_start..].find(needle.as_str()).map(|p| {
                    from as i64 + text[byte_start..byte_start + p].chars().count() as i64
                })
            } else {
                // Reverse: find last occurrence
                text.rfind(needle.as_str()).map(|p| {
                    text[..p].chars().count() as i64
                })
            };
            Ok(match found {
                Some(idx) => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 0, variant: "Some".to_string(),
                    fields: vec![Value::I64(idx)],
                },
                None => Value::Enum {
                    type_name: "Option__int64_t".to_string(),
                    tag: 1, variant: "None".to_string(), fields: vec![],
                },
            })
        }

        "gorget_str_trim" | "gorget_str_strip" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let result = if let Some(chars_arg) = args.get(1) {
                let charset: Vec<char> = chars_arg.to_sim_str().as_str().chars().collect();
                s.as_str().trim_matches(|c| charset.contains(&c)).to_string()
            } else {
                s.as_str().trim().to_string()
            };
            Ok(Value::Str(SimStr::from_string(result)))
        }

        "gorget_str_lstrip" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let result = if let Some(chars_arg) = args.get(1) {
                let charset: Vec<char> = chars_arg.to_sim_str().as_str().chars().collect();
                s.as_str().trim_start_matches(|c| charset.contains(&c)).to_string()
            } else {
                s.as_str().trim_start().to_string()
            };
            Ok(Value::Str(SimStr::from_string(result)))
        }

        "gorget_str_rstrip" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let result = if let Some(chars_arg) = args.get(1) {
                let charset: Vec<char> = chars_arg.to_sim_str().as_str().chars().collect();
                s.as_str().trim_end_matches(|c| charset.contains(&c)).to_string()
            } else {
                s.as_str().trim_end().to_string()
            };
            Ok(Value::Str(SimStr::from_string(result)))
        }

        "gorget_str_to_upper" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::String(SimString::from_string(s.as_str().to_uppercase())))
        }

        "gorget_str_to_lower" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::String(SimString::from_string(s.as_str().to_lowercase())))
        }

        "gorget_str_replace" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let from = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let to = args.get(2).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::String(SimString::from_string(s.as_str().replace(from.as_str(), to.as_str()))))
        }

        "gorget_str_replacen" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let from = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let to = args.get(2).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let limit = args.get(3).map(|v| v.as_i64()).unwrap_or(0);
            if limit > 0 {
                Ok(Value::String(SimString::from_string(s.as_str().replacen(from.as_str(), to.as_str(), limit as usize))))
            } else {
                Ok(Value::String(SimString::from_string(s.as_str().replace(from.as_str(), to.as_str()))))
            }
        }

        "gorget_str_repeat" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let n = args.get(1).map(|v| v.as_i64() as usize).unwrap_or(0);
            Ok(Value::String(SimString::from_string(s.as_str().repeat(n))))
        }

        "gorget_str_removeprefix" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let prefix = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let result = s.as_str().strip_prefix(prefix.as_str()).unwrap_or(s.as_str());
            Ok(Value::Str(SimStr::from_string(result.to_string())))
        }

        "gorget_str_removesuffix" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let suffix = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let result = s.as_str().strip_suffix(suffix.as_str()).unwrap_or(s.as_str());
            Ok(Value::Str(SimStr::from_string(result.to_string())))
        }

        "gorget_str_pad_left" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let width = args.get(1).map(|v| v.as_i64() as usize).unwrap_or(0);
            let pad_char = match args.get(2) {
                Some(Value::Char(c)) => char::from_u32(*c).unwrap_or(' '),
                Some(v) if v.is_string() => v.to_sim_str().as_str().chars().next().unwrap_or(' '),
                _ => ' ',
            };
            let s_str = s.as_str();
            let count = s_str.chars().count();
            if width <= count {
                Ok(Value::String(SimString::from_string(s_str.to_string())))
            } else {
                let padding: std::string::String = std::iter::repeat(pad_char).take(width - count).collect();
                Ok(Value::String(SimString::from_string(format!("{padding}{s_str}"))))
            }
        }

        "gorget_str_pad_right" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let width = args.get(1).map(|v| v.as_i64() as usize).unwrap_or(0);
            let pad_char = match args.get(2) {
                Some(Value::Char(c)) => char::from_u32(*c).unwrap_or(' '),
                Some(v) if v.is_string() => v.to_sim_str().as_str().chars().next().unwrap_or(' '),
                _ => ' ',
            };
            let s_str = s.as_str();
            let count = s_str.chars().count();
            if width <= count {
                Ok(Value::String(SimString::from_string(s_str.to_string())))
            } else {
                let padding: std::string::String = std::iter::repeat(pad_char).take(width - count).collect();
                Ok(Value::String(SimString::from_string(format!("{s_str}{padding}"))))
            }
        }

        "gorget_str_count" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let needle = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let count = s.as_str().matches(needle.as_str()).count();
            Ok(Value::I64(count as i64))
        }

        "gorget_str_split" => {
            // gorget_str_split(Str s, Str delim) → GorgetArray of Str
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let delim = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let arr = SimArray::new("Vector__Str");
            if delim.as_str().is_empty() {
                for ch in s.as_str().chars() {
                    arr.push(Value::Str(SimStr::from_string(ch.to_string())));
                }
            } else {
                for part in s.as_str().split(delim.as_str()) {
                    arr.push(Value::Str(SimStr::from_string(part.to_string())));
                }
            }
            Ok(Value::Array(arr))
        }

        "gorget_str_splitn" => {
            // gorget_str_splitn(Str s, Str delim, int64_t limit) → GorgetArray of Str
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let delim = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let limit = args.get(2).map(|v| v.as_i64()).unwrap_or(0);
            let arr = SimArray::new("Vector__Str");
            if limit > 0 {
                for part in s.as_str().splitn(limit as usize, delim.as_str()) {
                    arr.push(Value::Str(SimStr::from_string(part.to_string())));
                }
            } else {
                for part in s.as_str().split(delim.as_str()) {
                    arr.push(Value::Str(SimStr::from_string(part.to_string())));
                }
            }
            Ok(Value::Array(arr))
        }

        "gorget_str_lines" => {
            // gorget_str_lines(Str s) → GorgetArray of Str (split on \n, \r\n, \r)
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let arr = SimArray::new("Vector__Str");
            let text = s.as_str();
            let mut start = 0;
            let bytes = text.as_bytes();
            let len = bytes.len();
            while start <= len {
                let mut end = start;
                while end < len && bytes[end] != b'\n' && bytes[end] != b'\r' {
                    end += 1;
                }
                arr.push(Value::Str(SimStr::from_string(text[start..end].to_string())));
                if end >= len { break; }
                // Skip line ending: \r\n (2) or \n or \r (1)
                if bytes[end] == b'\r' && end + 1 < len && bytes[end + 1] == b'\n' {
                    start = end + 2;
                } else {
                    start = end + 1;
                }
            }
            Ok(Value::Array(arr))
        }

        "gorget_str_bytes" => {
            // gorget_str_bytes(Str s) → GorgetArray of uint8
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let arr = SimArray::new("Vector__uint8_t");
            for byte in s.as_bytes() {
                arr.push(Value::U8(*byte));
            }
            Ok(Value::Array(arr))
        }

        "gorget_str_codepoints" => {
            // gorget_str_codepoints(Str s) → GorgetArray of int64
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let arr = SimArray::new("Vector__int64_t");
            for cp in s.as_str().chars() {
                arr.push(Value::I64(cp as i64));
            }
            Ok(Value::Array(arr))
        }

        "gorget_str_chars" => {
            // gorget_str_chars(Str s) → GorgetArray of Str (one per codepoint)
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let arr = SimArray::new("Vector__Str");
            for ch in s.as_str().chars() {
                arr.push(Value::Str(SimStr::from_string(ch.to_string())));
            }
            Ok(Value::Array(arr))
        }

        "gorget_str_join" => {
            // gorget_str_join(Str sep, GorgetArray parts) → GorgetString
            let sep = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let parts = match args.get(1) {
                Some(Value::Array(arr)) => {
                    let items: Vec<std::string::String> = (0..arr.len())
                        .filter_map(|i| arr.get(i))
                        .map(|v| v.to_sim_str().as_str().to_string())
                        .collect();
                    items.join(sep.as_str())
                }
                _ => std::string::String::new(),
            };
            Ok(Value::String(SimString::from_string(parts)))
        }

        "gorget_str_char_at" => {
            // C: static inline uint32_t gorget_str_char_at(Str* s, int64_t idx) {
            //        return (uint32_t)gorget_str_byte_at(*s, idx); }
            // This is a BYTE access, not a codepoint access.
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let idx = args.get(1).map(|v| v.as_i64()).unwrap_or(0);
            let bytes = s.as_bytes();
            let len = bytes.len() as i64;
            let actual_idx = if idx < 0 { len + idx } else { idx };
            if actual_idx < 0 || actual_idx >= len {
                return Err(SimError::IndexOutOfBounds { index: idx, len: len as usize });
            }
            Ok(Value::Char(bytes[actual_idx as usize] as u32))
        }

        "gorget_str_index" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let idx = args.get(1).map(|v| v.as_i64()).unwrap_or(0);
            let s_str = s.as_str();
            let count = s_str.chars().count() as i64;
            let actual_idx = if idx < 0 { count + idx } else { idx };
            if actual_idx < 0 || actual_idx >= count {
                return Err(SimError::IndexOutOfBounds { index: idx, len: count as usize });
            }
            let ch_str: std::string::String = s_str.chars().nth(actual_idx as usize).unwrap().to_string();
            Ok(Value::Str(SimStr::from_string(ch_str)))
        }

        "gorget_str_slice" | "gorget_str_substring" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let start = args.get(1).map(|v| v.as_i64()).unwrap_or(0);
            let end = args.get(2).map(|v| v.as_i64()).unwrap_or(0);
            let s_str = s.as_str();
            let count = s_str.chars().count() as i64;
            let s_idx = start.max(0) as usize;
            let e_idx = end.min(count) as usize;
            let sliced: std::string::String = s_str.chars().skip(s_idx).take(e_idx.saturating_sub(s_idx)).collect();
            Ok(Value::Str(SimStr::from_string(sliced)))
        }

        // Used by the string for-in lowering: gorget_utf8_codepoint_len_at(Str, byte_pos) → I64
        // Returns the number of bytes in the UTF-8 codepoint starting at byte_pos.
        "gorget_utf8_codepoint_len_at" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let byte_pos = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let bytes = s.as_bytes();
            let cplen = if byte_pos >= bytes.len() {
                1i64
            } else {
                let b = bytes[byte_pos];
                if b < 0x80 { 1 }
                else if (b & 0xE0) == 0xC0 { 2 }
                else if (b & 0xF0) == 0xE0 { 3 }
                else if (b & 0xF8) == 0xF0 { 4 }
                else { 1 } // invalid lead byte — treat as single byte
            };
            Ok(Value::I64(cplen))
        }

        // Used by the string for-in lowering: gorget_str_codepoint_at(Str, byte_pos) → Str
        // Returns a Str view of the codepoint starting at byte_pos.
        "gorget_str_codepoint_at" => {
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let byte_pos = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let bytes = s.as_bytes();
            let cplen = if byte_pos >= bytes.len() {
                1usize
            } else {
                let b = bytes[byte_pos];
                if b < 0x80 { 1 }
                else if (b & 0xE0) == 0xC0 { 2 }
                else if (b & 0xF0) == 0xE0 { 3 }
                else if (b & 0xF8) == 0xF0 { 4 }
                else { 1 }
            };
            let end = (byte_pos + cplen).min(bytes.len());
            let slice = std::str::from_utf8(&bytes[byte_pos..end]).unwrap_or("?").to_string();
            Ok(Value::Str(SimStr::from_string(slice)))
        }

        "gorget_string_format" => {
            // Like printf but returns GorgetString
            if args.is_empty() { return Ok(Value::String(SimString::new())); }
            let fmt = match &args[0] {
                Value::Str(s) => s.as_str().to_string(),
                Value::String(s) => s.as_str().to_string(),
                _ => String::new(),
            };
            let formatted = do_printf(&fmt, &args[1..])?;
            Ok(Value::String(SimString::from_string(formatted)))
        }

        "gorget_string_append" | "gorget_string_push" => {
            // Append a string to a GorgetString (in-place via ptr in C, but we return new)
            // In GIR, this might be called with a mutable string and append arg
            let mut result = match args.get(0) {
                Some(Value::String(s)) => s.clone(),
                _ => SimString::new(),
            };
            let append = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            result.data.extend_from_slice(append.as_bytes());
            Ok(Value::String(result))
        }

        // ── Conversions ────────────────────────────────────────────────────────
        // Accept both gorget_* prefixed names and short stdlib names.
        "gorget_int_to_str" | "int_to_str" => {
            let n = args.first().map(|v| v.as_i64()).unwrap_or(0);
            Ok(Value::String(SimString::from_string(n.to_string())))
        }

        "gorget_uint_to_str" | "uint_to_str" => {
            let n = args.first().map(|v| v.as_u64()).unwrap_or(0);
            Ok(Value::String(SimString::from_string(n.to_string())))
        }

        "gorget_float_to_str" | "float_to_str" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::String(SimString::from_string(format_g(f))))
        }

        "gorget_bool_to_str" | "bool_to_str" => {
            let b = args.first().map(|v| v.as_bool()).unwrap_or(false);
            Ok(Value::Str(SimStr::from_str(if b { "true" } else { "false" })))
        }

        "codepoint_to_str" | "gorget_codepoint_to_utf8" => {
            let cp = args.first().map(|v| v.as_i64() as u32).unwrap_or(0);
            let s = char::from_u32(cp).unwrap_or('\0').to_string();
            Ok(Value::Str(SimStr::from_str(&s)))
        }

        // gorget_parse_int / parse_int → Result[int, str]
        "gorget_parse_int" | "parse_int" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let trimmed = s.as_str().trim();
            match trimmed.parse::<i64>() {
                Ok(n) => Ok(Value::Enum {
                    type_name: "Result__int64_t__Str".to_string(),
                    tag: 0,
                    variant: "Ok".to_string(),
                    fields: vec![Value::I64(n)],
                }),
                Err(_) => Ok(Value::Enum {
                    type_name: "Result__int64_t__Str".to_string(),
                    tag: 1,
                    variant: "Error".to_string(),
                    fields: vec![Value::Str(SimStr::from_string(format!("invalid integer: '{}'", trimmed)))],
                }),
            }
        }

        // gorget_parse_float / parse_float → Result[float, str]
        "gorget_parse_float" | "parse_float" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let trimmed = s.as_str().trim();
            match trimmed.parse::<f64>() {
                Ok(f) => Ok(Value::Enum {
                    type_name: "Result__double__Str".to_string(),
                    tag: 0,
                    variant: "Ok".to_string(),
                    fields: vec![Value::F64(f)],
                }),
                Err(_) => Ok(Value::Enum {
                    type_name: "Result__double__Str".to_string(),
                    tag: 1,
                    variant: "Error".to_string(),
                    fields: vec![Value::Str(SimStr::from_string(format!("invalid float: '{}'", trimmed)))],
                }),
            }
        }

        "gorget_str_to_int" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let n = s.as_str().trim().parse::<i64>().unwrap_or(0);
            Ok(Value::I64(n))
        }

        "gorget_str_to_float" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let f = s.as_str().trim().parse::<f64>().unwrap_or(0.0);
            Ok(Value::F64(f))
        }

        "ord" => {
            let cp = match args.first() {
                Some(Value::Char(c)) => *c,
                Some(Value::Str(s)) => s.as_str().chars().next().map(|c| c as u32).unwrap_or(0),
                Some(v) => v.as_i64() as u32,
                None => 0,
            };
            Ok(Value::I64(cp as i64))
        }

        "chr" | "gorget_char_chr" => {
            let cp = args.first().map(|v| v.as_i64() as u32).unwrap_or(0);
            let s = char::from_u32(cp).map(|c| c.to_string()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_str(&s)))
        }

        // ── Math ───────────────────────────────────────────────────────────────
        "abs" | "gorget_abs" | "llabs" | "labs" => {
            let v = args.first().unwrap_or(&Value::I64(0));
            match v {
                Value::I64(n) => Ok(Value::I64(n.abs())),
                Value::I32(n) => Ok(Value::I32(n.abs())),
                Value::F64(f) => Ok(Value::F64(f.abs())),
                Value::F32(f) => Ok(Value::F32(f.abs())),
                _ => Ok(Value::I64(v.as_i64().abs())),
            }
        }

        // C standard float abs/min/max (used directly by GIR codegen)
        "fabs" | "gorget_fabs" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.abs()))
        }

        "fmin" | "gorget_fmin" => {
            let a = args.get(0).map(|v| v.as_f64()).unwrap_or(0.0);
            let b = args.get(1).map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(a.min(b)))
        }

        "fmax" | "gorget_fmax" => {
            let a = args.get(0).map(|v| v.as_f64()).unwrap_or(0.0);
            let b = args.get(1).map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(a.max(b)))
        }

        "min" | "gorget_min" => {
            let a = args.get(0).unwrap_or(&Value::I64(0));
            let b = args.get(1).unwrap_or(&Value::I64(0));
            if a.is_float() || b.is_float() {
                Ok(Value::F64(a.as_f64().min(b.as_f64())))
            } else {
                Ok(Value::I64(a.as_i64().min(b.as_i64())))
            }
        }

        "max" | "gorget_max" => {
            let a = args.get(0).unwrap_or(&Value::I64(0));
            let b = args.get(1).unwrap_or(&Value::I64(0));
            if a.is_float() || b.is_float() {
                Ok(Value::F64(a.as_f64().max(b.as_f64())))
            } else {
                Ok(Value::I64(a.as_i64().max(b.as_i64())))
            }
        }

        "sqrt" | "gorget_sqrt" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.sqrt()))
        }

        "pow" | "gorget_pow" | "powf" => {
            let base = args.get(0).map(|v| v.as_f64()).unwrap_or(0.0);
            let exp = args.get(1).map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(base.powf(exp)))
        }

        "floor" | "gorget_floor" | "floorf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.floor()))
        }

        "ceil" | "gorget_ceil" | "ceilf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.ceil()))
        }

        "round" | "gorget_round" | "roundf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.round()))
        }

        "log" | "gorget_log" | "logf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.ln()))
        }

        "log2" | "gorget_log2" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.log2()))
        }

        "log10" | "gorget_log10" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.log10()))
        }

        "sin" | "gorget_sin" | "sinf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.sin()))
        }

        "cos" | "gorget_cos" | "cosf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.cos()))
        }

        "tan" | "gorget_tan" | "tanf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.tan()))
        }

        "atan" | "gorget_atan" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.atan()))
        }

        "atan2" | "gorget_atan2" => {
            let y = args.get(0).map(|v| v.as_f64()).unwrap_or(0.0);
            let x = args.get(1).map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(y.atan2(x)))
        }

        "asin" | "gorget_asin" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.asin()))
        }

        "acos" | "gorget_acos" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.acos()))
        }

        "fmod" | "gorget_fmod" | "fmodf" => {
            let a = args.get(0).map(|v| v.as_f64()).unwrap_or(0.0);
            let b = args.get(1).map(|v| v.as_f64()).unwrap_or(1.0);
            Ok(Value::F64(a % b))
        }

        "exp" | "gorget_exp" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(f.exp()))
        }

        "hypot" | "gorget_hypot" => {
            let a = args.get(0).map(|v| v.as_f64()).unwrap_or(0.0);
            let b = args.get(1).map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::F64(a.hypot(b)))
        }

        "isnan" | "isinf" => {
            let f = args.first().map(|v| v.as_f64()).unwrap_or(0.0);
            Ok(Value::Bool(if name == "isnan" { f.is_nan() } else { f.is_infinite() }))
        }

        // ── Lifecycle ──────────────────────────────────────────────────────────
        "gorget_trap" => {
            // D11: the lowering routes panic()/message-asserts through
            // gorget_trap(code, detail) — was gorget_panic(detail). Surface it
            // in the normative trap[T_X] shape; the user detail stays verbatim
            // so contains-style @should_panic matching keeps working.
            let code = match args.first() {
                Some(Value::Str(s)) => s.as_str().to_string(),
                Some(Value::String(s)) => s.as_str().to_string(),
                Some(Value::CStr(s)) => s.as_str().to_string(),
                _ => "T_Panic".to_string(),
            };
            let msg = match args.get(1) {
                Some(Value::Str(s)) => s.as_str().to_string(),
                Some(Value::String(s)) => s.as_str().to_string(),
                Some(Value::CStr(s)) => s.as_str().to_string(),
                _ => "panic".to_string(),
            };
            Err(SimError::Panic(format!("trap[{code}]: {msg}")))
        }

        "gorget_panic" => {
            let msg = match args.first() {
                Some(Value::Str(s)) => s.as_str().to_string(),
                Some(Value::String(s)) => s.as_str().to_string(),
                Some(Value::CStr(s)) => s.as_str().to_string(),
                _ => "panic".to_string(),
            };
            Err(SimError::Panic(msg))
        }

        "gorget_panic_fmt" => {
            // gorget_panic with format string
            let fmt = args.get(0).map(|v| match v {
                Value::Str(s) => s.as_str().to_string(),
                Value::String(s) => s.as_str().to_string(),
                _ => String::new(),
            }).unwrap_or_default();
            let formatted = do_printf(&fmt, &args[1..])?;
            Err(SimError::Panic(formatted))
        }

        "gorget_throw" => {
            // Error throwing mechanism
            let msg = match args.first() {
                Some(Value::Str(s)) => s.as_str().to_string(),
                Some(Value::String(s)) => s.as_str().to_string(),
                _ => "error".to_string(),
            };
            Err(SimError::Panic(format!("throw: {msg}")))
        }

        "exit" => {
            let code = args.first().map(|v| v.as_i64() as i32).unwrap_or(0);
            Err(SimError::Exit(code))
        }

        "gorget_init_args" | "__gorget_init_args" => {
            // No-op: args are set via set_program_name() before interpret().
            Ok(Value::Unit)
        }

        "gorget_get_args" | "gorget_args" | "args" => {
            // Return GorgetArray of Str with program name only (no extra CLI args in sim).
            let prog = PROGRAM_NAME.with(|c| c.borrow().clone());
            let arr = SimArray::new("Str");
            arr.push(Value::Str(SimStr::from_string(prog)));
            Ok(Value::Array(arr))
        }

        // ── Randomness ─────────────────────────────────────────────────────────
        "gorget_seed" | "seed" => {
            let s = args.first().map(|v| v.as_i64()).unwrap_or(0) as u64;
            RNG_STATE.with(|c| c.set(s));
            Ok(Value::Unit)
        }
        "gorget_rand" | "rand" => {
            Ok(Value::I64(splitmix64_rand()))
        }
        "gorget_rand_range" | "rand_range" => {
            let lo = args.get(0).map(|v| v.as_i64()).unwrap_or(0);
            let hi = args.get(1).map(|v| v.as_i64()).unwrap_or(1);
            if lo >= hi { return Ok(Value::I64(lo)); }
            let r = splitmix64_rand();
            Ok(Value::I64(lo + (r as u64 % (hi - lo) as u64) as i64))
        }

        // ── Time ───────────────────────────────────────────────────────────────
        "gorget_time" | "time" => {
            if ISOLATION.with(|c| c.get()) {
                return Ok(Value::I64(next_fake_time_ms() / 1000));
            }
            use std::time::{SystemTime, UNIX_EPOCH};
            let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_secs() as i64;
            Ok(Value::I64(secs))
        }
        "gorget_time_ms" | "time_ms" => {
            if ISOLATION.with(|c| c.get()) {
                return Ok(Value::I64(next_fake_time_ms()));
            }
            use std::time::{SystemTime, UNIX_EPOCH};
            let ms = SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_millis() as i64;
            Ok(Value::I64(ms))
        }
        "gorget_format_time" | "format_time" => {
            // format_time(epoch: int, fmt: str) → String
            let epoch = args.get(0).map(|v| v.as_i64()).unwrap_or(0);
            let fmt_str = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str("%Y-%m-%d %H:%M:%S"));
            // Use chrono-free implementation via POSIX strftime equivalent in Rust
            // We use std::time + hand-rolled formatting for common patterns
            let _t = std::time::UNIX_EPOCH + std::time::Duration::from_secs(epoch.max(0) as u64);
            // Convert to local time using std (no chrono needed)
            // Fallback: use a basic manual formatting approach
            let secs = epoch;
            let formatted = format_epoch_time(secs, fmt_str.as_str());
            Ok(Value::String(SimString::from_string(formatted)))
        }
        "gorget_parse_time" | "parse_time" => {
            // parse_time(s: str, fmt: str) → int (-1 on failure)
            let s = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let fmt = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let epoch = parse_epoch_time(s.as_str(), fmt.as_str());
            Ok(Value::I64(epoch))
        }

        "gorget_sleep_ms" | "sleep_ms" | "gorget_reactor_sleep_ms" | "async_sleep" => {
            let ms = args.get(0).map(|v| v.as_i64()).unwrap_or(0).max(0) as u64;
            std::thread::sleep(std::time::Duration::from_millis(ms));
            Ok(Value::Unit)
        }
        "gorget_sleep" | "sleep" => {
            // sleep(seconds: float)
            let secs = args.get(0).map(|v| v.as_f64()).unwrap_or(0.0).max(0.0);
            std::thread::sleep(std::time::Duration::from_secs_f64(secs));
            Ok(Value::Unit)
        }

        // ── Memory / allocation stubs ──────────────────────────────────────────
        "malloc" | "calloc" | "realloc" => {
            // Return a non-null placeholder pointer
            Ok(Value::Ptr(1))
        }

        "free" => {
            Ok(Value::Unit)
        }

        "memset" => {
            Ok(Value::Unit)
        }

        "memcmp" => {
            // Simplified: compare by content if possible
            Ok(Value::I32(0))
        }

        "memcpy" | "memmove" => {
            // Return destination pointer
            Ok(args.first().cloned().unwrap_or(Value::Ptr(0)))
        }

        // ── Thread-local allocator stubs ───────────────────────────────────────
        "gorget_push_allocator" | "gorget_pop_allocator" | "gorget_get_allocator" => {
            Ok(Value::Unit)
        }

        // ── Assert ─────────────────────────────────────────────────────────────
        "gorget_assert" => {
            let cond = args.get(0).map(|v| v.as_bool()).unwrap_or(false);
            if !cond {
                let msg = args.get(1).map(|v| match v {
                    Value::Str(s) => s.as_str().to_string(),
                    Value::String(s) => s.as_str().to_string(),
                    _ => "assertion failed".to_string(),
                }).unwrap_or_else(|| "assertion failed".to_string());
                return Err(SimError::Panic(msg));
            }
            Ok(Value::Unit)
        }

        // ── Primitive static method dispatch (int.parse, int.default, etc.) ──────
        // These are lowered as Call("int64_t__parse", [str]) etc. from Type.method() syntax.
        // Helper: make Some(val) enum
        // Helper: make None enum
        "int64_t__parse" | "int__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<i64>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__int64_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::I64(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__int64_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "int64_t__default" | "int__default" => Ok(Value::I64(0)),
        "int64_t__one" | "int__one" => Ok(Value::I64(1)),
        "int8_t__parse" | "int8__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<i8>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__int8_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::I8(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__int8_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "int16_t__parse" | "int16__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<i16>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__int16_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::I16(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__int16_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "int32_t__parse" | "int32__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<i32>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__int32_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::I32(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__int32_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "uint8_t__parse" | "uint8__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<u8>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__uint8_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::U8(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__uint8_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "uint16_t__parse" | "uint16__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<u16>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__uint16_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::U16(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__uint16_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "uint32_t__parse" | "uint32__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<u32>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__uint32_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::U32(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__uint32_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "uint64_t__parse" | "uint64__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<u64>() {
                Ok(n) => Ok(Value::Enum { type_name: "Option__uint64_t".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::U64(n)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__uint64_t".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "double__parse" | "float__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim().parse::<f64>() {
                Ok(f) => Ok(Value::Enum { type_name: "Option__double".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::F64(f)] }),
                Err(_) => Ok(Value::Enum { type_name: "Option__double".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "double__default" | "float__default" => Ok(Value::F64(0.0)),
        "double__one" | "float__one" => Ok(Value::F64(1.0)),
        "bool__parse" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match s.as_str().trim() {
                "true" => Ok(Value::Enum { type_name: "Option__bool".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::Bool(true)] }),
                "false" => Ok(Value::Enum { type_name: "Option__bool".to_string(), tag: 0, variant: "Some".to_string(), fields: vec![Value::Bool(false)] }),
                _ => Ok(Value::Enum { type_name: "Option__bool".to_string(), tag: 1, variant: "None".to_string(), fields: vec![] }),
            }
        }
        "bool__default" => Ok(Value::Bool(false)),
        "Str__default" | "str__default" => Ok(Value::Str(SimStr::from_str(""))),

        // ── Option / Result operations (first arg is already derefed enum value) ──
        // dispatch.rs auto-derefs __option_* and __result_* first arg from Ptr → Enum.
        "__option_is_some" | "__option_is_ok" => {
            let tag = match args.first() {
                Some(Value::Enum { tag, .. }) => *tag,
                Some(Value::Null) => 1,
                // Value::Unit or unknown → not Some/Ok (tag 1 = None/Err)
                _ => 1,
            };
            Ok(Value::Bool(tag == 0))
        }
        "__option_is_none" | "__option_is_err" => {
            let tag = match args.first() {
                Some(Value::Enum { tag, .. }) => *tag,
                Some(Value::Null) => 1,
                // Value::Unit or unknown → treat as None/Err (tag 1)
                _ => 1,
            };
            Ok(Value::Bool(tag != 0))
        }
        "__option_unwrap" | "__result_unwrap" => {
            match args.first() {
                Some(Value::Enum { tag: 0, fields, .. }) => {
                    Ok(fields.first().cloned().unwrap_or(Value::Unit))
                }
                _ => Err(SimError::Panic("unwrap on None/Err".to_string())),
            }
        }
        "__option_unwrap_or" | "__result_unwrap_or" => {
            let default = args.get(1).cloned().unwrap_or(Value::Unit);
            match args.first() {
                Some(Value::Enum { tag: 0, fields, .. }) => {
                    Ok(fields.first().cloned().unwrap_or(default))
                }
                _ => Ok(default),
            }
        }

        // ── Type conversion helpers ────────────────────────────────────────────
        "gorget_str_to_upper_to_str" | "gorget_str_to_lower_to_str" => {
            Err(SimError::Unimplemented(name.to_string()))
        }

        // ── Path functions ─────────────────────────────────────────────────────
        "gorget_path_parent" | "path_parent" => {
            let s = args.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_string(path_parent(&s))))
        }
        "gorget_path_basename" | "path_basename" => {
            let s = args.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_string(path_basename(&s))))
        }
        "gorget_path_extension" | "path_extension" => {
            let s = args.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_string(path_extension_of_base(&path_basename(&s)))))
        }
        "gorget_path_stem" | "path_stem" => {
            let s = args.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_string(path_stem_of_base(&path_basename(&s)))))
        }
        "gorget_path_join" | "path_join" => {
            let a = args.get(0).map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            let b = args.get(1).map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            let result = path_join(&a, &b);
            Ok(Value::Str(SimStr::from_string(result)))
        }
        "gorget_path_normalize" | "path_normalize" => {
            let s = args.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_string(path_normalize(&s))))
        }
        "gorget_path_absolute" | "path_absolute" => {
            let s = args.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_string(path_absolute(&s))))
        }

        "gorget_path_exists" | "path_exists" => {
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::path::Path::new(p.as_str()).exists()))
        }

        "gorget_path_is_file" | "path_is_file" => {
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::path::Path::new(p.as_str()).is_file()))
        }

        "gorget_path_is_dir" | "path_is_dir" => {
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::path::Path::new(p.as_str()).is_dir()))
        }

        // ── File system operations ────────────────────────────────────────────
        "gorget_file_exists" | "file_exists" => {
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::path::Path::new(p.as_str()).exists()))
        }
        "gorget_is_dir" | "is_dir" => {
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::path::Path::new(p.as_str()).is_dir()))
        }
        "gorget_mkdir" | "mkdir" => {
            check_isolation("mkdir")?;
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::fs::create_dir_all(p.as_str()).is_ok()))
        }
        "gorget_rmdir" | "rmdir" => {
            check_isolation("rmdir")?;
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::fs::remove_dir_all(p.as_str()).is_ok()))
        }
        "gorget_delete_file" | "delete_file" | "gorget_remove_file" => {
            check_isolation("delete_file")?;
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::fs::remove_file(p.as_str()).is_ok()))
        }
        "gorget_file_size" | "file_size" => {
            // Read-only metadata — allowed in isolation.
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            match std::fs::metadata(p.as_str()) {
                Ok(meta) => Ok(Value::I64(meta.len() as i64)),
                Err(_) => Ok(Value::I64(-1)),
            }
        }
        "gorget_rename_file" | "rename" | "gorget_rename" => {
            check_isolation("rename_file")?;
            let from = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let to = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::fs::rename(from.as_str(), to.as_str()).is_ok()))
        }
        "gorget_copy_file" | "copy_file" => {
            check_isolation("copy_file")?;
            let from = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let to = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Bool(std::fs::copy(from.as_str(), to.as_str()).is_ok()))
        }
        "gorget_readdir" | "readdir" | "gorget_list_dir" | "list_dir" => {
            check_isolation("readdir")?;
            let p = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let arr = super::value::SimArray::new("Vector__Str");
            if let Ok(entries) = std::fs::read_dir(p.as_str()) {
                for entry in entries.flatten() {
                    let name = entry.file_name().to_string_lossy().to_string();
                    arr.push(Value::Str(SimStr::from_string(name)));
                }
            }
            Ok(Value::Array(arr))
        }

        // ── I/O stubs ──────────────────────────────────────────────────────────
        "gorget_input" | "gorget_readline" | "input" | "readline" => {
            if ISOLATION.with(|c| c.get()) {
                // In isolation: simulate EOF — return empty string without reading stdin.
                return Ok(Value::String(SimString::from_string(std::string::String::new())));
            }
            // Print prompt (for input) to stdout, then read from stdin.
            if name == "input" || name == "gorget_input" {
                let prompt = args.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
                stdout.extend_from_slice(prompt.as_bytes());
            }
            let mut line = std::string::String::new();
            let _ = std::io::stdin().read_line(&mut line);
            let line = line.trim_end_matches('\n').trim_end_matches('\r').to_string();
            Ok(Value::String(SimString::from_string(line)))
        }

        "gorget_read_file" | "read_file" => {
            check_isolation("read_file")?;
            // Returns GorgetString directly (not Result) — same as C gorget_read_file
            let path = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let s = std::fs::read_to_string(path.as_str()).unwrap_or_default();
            Ok(Value::String(SimString::from_string(s)))
        }

        "gorget_write_file" | "write_file" => {
            check_isolation("write_file")?;
            // Standalone write: (path, content) → void
            let path    = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let content = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let _ = std::fs::write(path.as_str(), content.as_bytes());
            Ok(Value::Unit)
        }

        "gorget_append_file" | "append_file" => {
            check_isolation("append_file")?;
            use std::io::Write as _;
            let path    = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let content = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            if let Ok(mut f) = std::fs::OpenOptions::new().append(true).create(true).open(path.as_str()) {
                let _ = f.write_all(content.as_bytes());
            }
            Ok(Value::Unit)
        }

        // gorget_file_create / gorget_file_open: return a GorgetFile struct
        // (handled in dispatch.rs try_collection_dispatch for MutPtr args,
        //  this is the non-pointer variant for direct calls)
        "gorget_file_create" | "File__create" => {
            check_isolation("file_create")?;
            let path = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let _ = std::fs::File::create(path.as_str());
            Ok(Value::Struct {
                type_name: "GorgetFile".to_string(),
                fields: vec![Value::Str(path), Value::Str(SimStr::from_str("w"))],
            })
        }

        "gorget_file_open" | "File__open" => {
            check_isolation("file_open")?;
            let path = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::Struct {
                type_name: "GorgetFile".to_string(),
                fields: vec![Value::Str(path), Value::Str(SimStr::from_str("r"))],
            })
        }

        "gorget_file_close" | "File__close" => Ok(Value::Unit),

        // ── Hash / random stubs ────────────────────────────────────────────────
        "__gorget_hash_i64" | "__gorget_hash_u64" | "__gorget_hash_bool" | "__gorget_hash_int" => {
            // FNV-1a over 8 bytes of the integer value (same as C: __gorget_fnv1a(&v, sizeof(v)))
            let v = args.first().map(|v| v.as_u64()).unwrap_or(0);
            Ok(Value::I64(fnv1a_hash(&v.to_le_bytes()) as i64))
        }

        "__gorget_hash_str_len" | "gorget_str_hash" | "Str__hash" => {
            // FNV-1a over string bytes. Args may be (data_ptr, len) or (str_ptr,)
            // Find a Str arg (directly or via deref) and hash its bytes.
            let str_val = args.iter().find_map(|a| match a {
                Value::Str(s) => Some(s.clone()),
                Value::String(s) => Some(SimStr::from_string(s.as_str().to_string())),
                _ => None,
            });
            let hash = if let Some(s) = str_val {
                fnv1a_hash(s.as_bytes())
            } else {
                // Fallback: hash the integer value of the first arg
                let v = args.first().map(|v| v.as_u64()).unwrap_or(0);
                fnv1a_hash(&v.to_le_bytes())
            };
            Ok(Value::I64(hash as i64))
        }

        "gorget_random_int" => {
            // Deterministic in sim: always return 42
            Ok(Value::I64(42))
        }

        "gorget_random_float" => {
            Ok(Value::F64(0.5))
        }

        // ── Bytes operations ────────────────────────────────────────────────────
        "gorget_bytes_from_str" => {
            // Convert str/String to Vector[uint8]
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let arr = SimArray::new("uint8_t");
            for b in s.as_bytes() {
                arr.push(Value::U8(*b));
            }
            Ok(Value::Array(arr))
        }

        "gorget_bytes_to_str" => {
            // Convert Vector[uint8] to str (null-terminated C string in C; str in sim)
            let arr = args.first().and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let bytes: Vec<u8> = arr.to_vec().into_iter().map(|v| v.as_i64() as u8).collect();
            let s = String::from_utf8_lossy(&bytes).into_owned();
            Ok(Value::Str(SimStr::from_string(s)))
        }

        "gorget_bytes_utf8_valid" => {
            // Validate that a Vector[uint8] is well-formed UTF-8. Returns bool.
            let arr = args.first().and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let bytes: Vec<u8> = arr.to_vec().into_iter().map(|v| v.as_i64() as u8).collect();
            let valid = std::str::from_utf8(&bytes).is_ok();
            Ok(Value::Bool(valid))
        }

        "gorget_bytes_from_hex" => {
            // Convert hex string to Vector[uint8]
            let hex = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let hex_str = hex.as_str();
            let arr = SimArray::new("uint8_t");
            let mut chars = hex_str.chars().peekable();
            while chars.peek().is_some() {
                let hi = chars.next().unwrap_or('0');
                let lo = chars.next().unwrap_or('0');
                let byte = u8::from_str_radix(&format!("{}{}", hi, lo), 16).unwrap_or(0);
                arr.push(Value::U8(byte));
            }
            Ok(Value::Array(arr))
        }

        "gorget_bytes_to_hex" => {
            // Convert Vector[uint8] to lowercase hex string
            let arr = args.first().and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let hex: String = arr.to_vec().into_iter()
                .map(|v| format!("{:02x}", v.as_i64() as u8))
                .collect();
            Ok(Value::Str(SimStr::from_string(hex)))
        }

        "gorget_bytes_concat" => {
            // Concatenate two Vector[uint8]
            let a = args.get(0).and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let b = args.get(1).and_then(|v| match v {
                Value::Array(b) => Some(b.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let out = SimArray::new("uint8_t");
            for v in a.to_vec().into_iter().chain(b.to_vec().into_iter()) {
                out.push(v);
            }
            Ok(Value::Array(out))
        }

        "gorget_bytes_slice" => {
            // Slice a Vector[uint8] [start, end)
            let arr = args.get(0).and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let start = args.get(1).map(|v| v.as_i64()).unwrap_or(0).max(0) as usize;
            let end = args.get(2).map(|v| v.as_i64()).unwrap_or(0);
            let len = arr.len();
            let end = (end as usize).min(len);
            let out = SimArray::new("uint8_t");
            let elems = arr.to_vec();
            for v in elems.into_iter().skip(start).take(end.saturating_sub(start)) {
                out.push(v);
            }
            Ok(Value::Array(out))
        }

        "gorget_random_bytes" => {
            // Generate N random bytes (deterministic in sim: use FNV counter)
            let n = args.first().map(|v| v.as_i64()).unwrap_or(0).max(0) as usize;
            let arr = SimArray::new("uint8_t");
            // Deterministic pseudo-random using counter
            let mut state = 12345u64;
            for _ in 0..n {
                state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                arr.push(Value::U8(((state >> 33) & 0xFF) as u8));
            }
            Ok(Value::Array(arr))
        }

        "gorget_bytes_write_u32_be" => {
            // Write big-endian u32 into array at offset (mutates through pointer)
            // Args: array_ptr, offset, value — array_ptr is MutPtr to array
            // We handle the in-place mutation in try_collection_dispatch, but if it
            // reaches here, just return Unit (the mutation happens elsewhere).
            Ok(Value::Unit)
        }
        "gorget_bytes_write_u16_be" | "gorget_bytes_write_u32_le" | "gorget_bytes_write_u16_le" => {
            Ok(Value::Unit)
        }
        "gorget_bytes_read_u32_be" => {
            let arr = args.get(0).and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let off = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let elems = arr.to_vec();
            if off + 3 < elems.len() {
                let b0 = elems[off].as_i64() as u8 as u32;
                let b1 = elems[off+1].as_i64() as u8 as u32;
                let b2 = elems[off+2].as_i64() as u8 as u32;
                let b3 = elems[off+3].as_i64() as u8 as u32;
                Ok(Value::I64(((b0 << 24) | (b1 << 16) | (b2 << 8) | b3) as i64))
            } else { Ok(Value::I64(0)) }
        }
        "gorget_bytes_read_u16_be" => {
            let arr = args.get(0).and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let off = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let elems = arr.to_vec();
            if off + 1 < elems.len() {
                let b0 = elems[off].as_i64() as u8 as u16;
                let b1 = elems[off+1].as_i64() as u8 as u16;
                Ok(Value::I64(((b0 << 8) | b1) as i64))
            } else { Ok(Value::I64(0)) }
        }
        "gorget_bytes_read_u32_le" => {
            let arr = args.get(0).and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let off = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let elems = arr.to_vec();
            if off + 3 < elems.len() {
                let b0 = elems[off].as_i64() as u8 as u32;
                let b1 = elems[off+1].as_i64() as u8 as u32;
                let b2 = elems[off+2].as_i64() as u8 as u32;
                let b3 = elems[off+3].as_i64() as u8 as u32;
                Ok(Value::I64((b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)) as i64))
            } else { Ok(Value::I64(0)) }
        }
        "gorget_bytes_read_u16_le" => {
            let arr = args.get(0).and_then(|v| match v {
                Value::Array(a) => Some(a.clone()),
                _ => None,
            }).unwrap_or_else(|| SimArray::new("uint8_t"));
            let off = args.get(1).map(|v| v.as_i64()).unwrap_or(0) as usize;
            let elems = arr.to_vec();
            if off + 1 < elems.len() {
                let b0 = elems[off].as_i64() as u8 as u16;
                let b1 = elems[off+1].as_i64() as u8 as u16;
                Ok(Value::I64((b0 | (b1 << 8)) as i64))
            } else { Ok(Value::I64(0)) }
        }

        // ── OS / process ───────────────────────────────────────────────────────
        "gorget_getenv" | "getenv" => {
            if ISOLATION.with(|c| c.get()) {
                // In isolation: return empty string (no env access).
                return Ok(Value::Str(SimStr::from_str("")));
            }
            let name = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let val = std::env::var(name.as_str()).unwrap_or_default();
            Ok(Value::Str(SimStr::from_string(val)))
        }

        "gorget_setenv" | "setenv" => {
            let name = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let val  = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            // SAFETY: single-threaded sim, no data races
            unsafe { std::env::set_var(name.as_str(), val.as_str()); }
            Ok(Value::Unit)
        }

        "gorget_getcwd" | "getcwd" => {
            let cwd = std::env::current_dir()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_default();
            Ok(Value::String(SimString::from_string(cwd)))
        }

        "gorget_platform" | "platform" => {
            let p = if cfg!(target_os = "macos") { "macos" }
                    else if cfg!(target_os = "windows") { "windows" }
                    else if cfg!(target_os = "freebsd") { "freebsd" }
                    else { "linux" };
            Ok(Value::Str(SimStr::from_str(p)))
        }

        "gorget_exec" | "exec" => {
            check_isolation("exec")?;
            let cmd = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let status = std::process::Command::new("sh")
                .arg("-c").arg(cmd.as_str())
                .status()
                .map(|s| s.code().unwrap_or(-1) as i64)
                .unwrap_or(-1);
            Ok(Value::I64(status))
        }

        "gorget_exec_output" | "exec_output" => {
            check_isolation("exec_output")?;
            // ExecResult { output: Str, errors: Str, exit_code: i64 }
            let cmd = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let result = std::process::Command::new("sh")
                .arg("-c").arg(cmd.as_str())
                .output();
            let (out_str, err_str, code) = match result {
                Ok(out) => {
                    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
                    let stderr_s = String::from_utf8_lossy(&out.stderr).into_owned();
                    let code = out.status.code().unwrap_or(-1) as i64;
                    (stdout, stderr_s, code)
                }
                Err(_) => (String::new(), String::new(), -1i64),
            };
            Ok(Value::Struct {
                type_name: "ExecResult".to_string(),
                fields: vec![
                    Value::Str(SimStr::from_string(out_str)),
                    Value::Str(SimStr::from_string(err_str)),
                    Value::I64(code),
                ],
            })
        }

        // ── Terminal ──────────────────────────────────────────────────────────
        "gorget_is_tty" => {
            // In the sim, stdout is always captured (never a real tty).
            Ok(Value::Bool(false))
        }

        // ── Misc stubs ─────────────────────────────────────────────────────────
        "gorget_dbg_print" | "gorget_dbg" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            stderr.extend_from_slice(s.as_bytes());
            stderr.push(b'\n');
            Ok(Value::Unit)
        }

        "snprintf" => {
            // Format into a buffer — simplified: just do the formatting and ignore the buffer
            if args.len() >= 3 {
                let fmt = match &args[2] {
                    Value::Str(s) => s.as_str().to_string(),
                    _ => String::new(),
                };
                let _ = do_printf(&fmt, &args[3..]);
            }
            Ok(Value::I32(0))
        }

        "sprintf" => {
            // Format into first arg (buffer) — simplified
            if args.len() >= 2 {
                let fmt = match &args[1] {
                    Value::Str(s) => s.as_str().to_string(),
                    _ => String::new(),
                };
                let formatted = do_printf(&fmt, &args[2..]).unwrap_or_default();
                return Ok(Value::I32(formatted.len() as i32));
            }
            Ok(Value::I32(0))
        }

        "strlen" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::U64(s.byte_len() as u64))
        }

        "strcmp" | "strncmp" => {
            let a = args.get(0).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            let b = args.get(1).map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::I32(a.as_str().cmp(b.as_str()) as i32))
        }

        "strdup" => {
            let s = args.first().map(|v| v.to_sim_str()).unwrap_or_else(|| SimStr::from_str(""));
            Ok(Value::CStr(std::rc::Rc::new(s.as_str().to_string())))
        }

        // ── Option/Result helpers ──────────────────────────────────────────────
        "gorget_option_unwrap" => {
            match args.first() {
                Some(Value::Enum { tag: 0, fields, .. }) => {
                    Ok(fields.first().cloned().unwrap_or(Value::Unit))
                }
                _ => Err(SimError::Panic("unwrap called on None".to_string())),
            }
        }

        "gorget_result_unwrap" => {
            match args.first() {
                Some(Value::Enum { tag: 0, fields, .. }) => {
                    Ok(fields.first().cloned().unwrap_or(Value::Unit))
                }
                Some(Value::Enum { tag: 1, fields, .. }) => {
                    let err = fields.first().map(|v| v.to_sim_str().as_str().to_string()).unwrap_or_default();
                    Err(SimError::Panic(format!("unwrap called on Err: {err}")))
                }
                _ => Err(SimError::Panic("unwrap called on invalid Result".to_string())),
            }
        }

        // ── Catch-all ──────────────────────────────────────────────────────────
        // GorgetString__method and Str__method calls: delegate to gorget_str_* equivalents.
        // The first arg has already been auto-dereffed to a String/Str in dispatch.rs.
        other if other.starts_with("GorgetString__") || other.starts_with("Str__") => {
            let method = if other.starts_with("GorgetString__") {
                &other["GorgetString__".len()..]
            } else {
                &other["Str__".len()..]
            };
            let gorget_fn = format!("gorget_str_{}", method);
            call_extern(&gorget_fn, args, module, stdout, stderr, depth)
        }

        other => {
            // Any gorget_* function that reaches here is unimplemented — report it
            // clearly so new stdlib additions don't silently return Unit.
            if other.starts_with("gorget_") {
                Err(SimError::Unimplemented(other.to_string()))
            } else {
                // Non-gorget externs (C stdlib, etc.) — return unit as safe default.
                Ok(Value::Unit)
            }
        }
    }
}

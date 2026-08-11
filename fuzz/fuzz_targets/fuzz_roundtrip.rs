#![no_main]
use libfuzzer_sys::fuzz_target;
use gorget::formatter::format_source_result;
use gorget::parser::Parser;

// Roundtrip property: if a source parses cleanly, formatting it must produce
// something that ALSO parses cleanly.
//
// Uses the FALLIBLE entry point `format_source_result` rather than
// `format_source_infallible`. The `parser.errors.is_empty()` gate below
// already means the `Err` arm is unreachable today — but the infallible
// variant PANICS on `Err`, and a panic inside a fuzz target is reported as a
// crash. So if that gate is ever relaxed, the infallible call would
// manufacture spurious "findings" that are really just unparseable inputs.
// Skipping `Err` keeps the target honest about which property it tests.
fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let mut parser = Parser::new(source);
        let _ = parser.parse_module();
        if parser.errors.is_empty() {
            // First parse succeeded: format and reparse — the second parse
            // must also succeed.
            let Ok(formatted) = format_source_result(source) else {
                return;
            };
            let mut parser2 = Parser::new(&formatted);
            let _ = parser2.parse_module();
            assert!(
                parser2.errors.is_empty(),
                "Roundtrip failure: source parsed OK but formatted output did not.\n\
                 Source:\n{source}\n\nFormatted:\n{formatted}\n\nErrors: {:?}",
                parser2.errors
            );
        }
    }
});

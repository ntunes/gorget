#![no_main]
use libfuzzer_sys::fuzz_target;
use gorget::formatter::format_source;
use gorget::parser::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let mut parser = Parser::new(source);
        let _ = parser.parse_module();
        if parser.errors.is_empty() {
            // If first parse succeeded, format and reparse — second parse must also succeed
            let formatted = format_source(source);
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

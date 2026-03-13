#![no_main]
use libfuzzer_sys::fuzz_target;
use gorget::parser::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let mut parser = Parser::new(source);
        let _ = parser.parse_module();
    }
});

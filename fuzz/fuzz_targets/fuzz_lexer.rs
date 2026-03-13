#![no_main]
use libfuzzer_sys::fuzz_target;
use gorget::lexer::Lexer;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let lexer = Lexer::new(source);
        for _token in lexer {}
    }
});

use anyhow::Result;
use std::collections::HashMap;

use libsic::xe::op::Register;
use once_cell::sync::OnceCell;
use regex::Regex;

pub static REGISTERS: OnceCell<HashMap<String, Register>> = OnceCell::new();
pub static LINE_REGEX: OnceCell<Regex> = OnceCell::new();
static LINE_REGEX_PATTERN: &str =
    r#"^(?:(?P<label>[^.\s]\S*)|\s)\s+(?P<directive>\S+)(?:[^\n\S]+|$)(?P<argument>\S*)[^\n]*"#;

pub fn register(r: &str) -> Result<Register> {
    REGISTERS
        .get_or_init(|| {
            [
                ("A".to_owned(), Register::A),
                ("X".to_owned(), Register::X),
                ("L".to_owned(), Register::L),
                ("B".to_owned(), Register::B),
                ("S".to_owned(), Register::S),
                ("T".to_owned(), Register::T),
                ("F".to_owned(), Register::F),
                ("PC".to_owned(), Register::PC),
                ("SW".to_owned(), Register::SW),
            ]
            .into()
        })
        .get(r)
        .copied()
        .ok_or_else(|| anyhow::Error::msg(format!("Couldn't find register {}", r)))
}

pub fn line_regex() -> &'static Regex {
    LINE_REGEX.get_or_init(|| Regex::new(LINE_REGEX_PATTERN).expect("Invalid line regex"))
}

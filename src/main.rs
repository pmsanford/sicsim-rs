use anyhow::Result;
use regex::Regex;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

use once_cell::sync::OnceCell;

static OPCODES: OnceCell<HashMap<String, u8>> = OnceCell::new();
static LINE_REGEX: OnceCell<Regex> = OnceCell::new();
//static LINE_REGEX_PATTERN: &str = r#"^(([^.\s]\S*)|\s)\s+(\S+)[^\n\S]+([^\s\n,]*)(,X)?[^\n]*"#;
static LINE_REGEX_PATTERN: &str =
    r#"^(?:(?P<label>[^.\s]\S*)|\s)\s+(?P<directive>\S+)(?:[^\n\S]+|$)(?P<target>\S*)[^\n]*"#;

fn build_opcodes() -> HashMap<String, u8> {
    [
        ("ADD".to_owned(), 0x18),
        ("AND".to_owned(), 0x40),
        ("COMP".to_owned(), 0x28),
        ("DIV".to_owned(), 0x24),
        ("J".to_owned(), 0x3C),
        ("JEQ".to_owned(), 0x30),
        ("JGT".to_owned(), 0x34),
        ("JLT".to_owned(), 0x38),
        ("JSUB".to_owned(), 0x48),
        ("LDA".to_owned(), 0x00),
        ("LDCH".to_owned(), 0x50),
        ("LDL".to_owned(), 0x08),
        ("LDX".to_owned(), 0x04),
        ("MUL".to_owned(), 0x20),
        ("OR".to_owned(), 0x44),
        ("RD".to_owned(), 0xD8),
        ("RSUB".to_owned(), 0x4C),
        ("STA".to_owned(), 0x0C),
        ("STCH".to_owned(), 0x54),
        ("STL".to_owned(), 0x14),
        ("STSW".to_owned(), 0xE8),
        ("STX".to_owned(), 0x10),
        ("SUB".to_owned(), 0x1C),
        ("TD".to_owned(), 0xE0),
        ("TIX".to_owned(), 0x2C),
        ("WD".to_owned(), 0xDC),
    ]
    .into()
}

#[derive(Debug)]
struct ParsedLine {
    label: Option<String>,
    directive: String,
    target: Option<String>,
}

fn parse_line(line: &str) -> Option<ParsedLine> {
    LINE_REGEX
        .get()
        .unwrap()
        .captures(line)
        .map(|cap| ParsedLine {
            label: cap.name("label").map(|m| m.as_str().to_owned()),
            directive: cap
                .name("directive")
                .map(|m| m.as_str().to_owned())
                .unwrap(),
            target: cap
                .name("target")
                .filter(|m| m.as_str().len() > 0)
                .map(|m| m.as_str().to_owned()),
        })
}

fn main() -> Result<()> {
    OPCODES.set(build_opcodes()).unwrap();
    LINE_REGEX.set(Regex::new(LINE_REGEX_PATTERN)?).unwrap();

    let file = File::open("example.asm")?;

    for (lineno, line) in BufReader::new(file).lines().enumerate() {
        println!("{:3}: {:?}", lineno, parse_line(&line?));
    }

    Ok(())
}

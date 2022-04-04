use anyhow::Result;
use regex::Regex;
use std::{
    collections::HashMap,
    fmt::Display,
    fs::File,
    io::{BufRead, BufReader},
};

use once_cell::sync::OnceCell;

static OPCODES: OnceCell<HashMap<String, u8>> = OnceCell::new();
static LINE_REGEX: OnceCell<Regex> = OnceCell::new();
//static LINE_REGEX_PATTERN: &str = r#"^(([^.\s]\S*)|\s)\s+(\S+)[^\n\S]+([^\s\n,]*)(,X)?[^\n]*"#;
static LINE_REGEX_PATTERN: &str =
    r#"^(?:(?P<label>[^.\s]\S*)|\s)\s+(?P<directive>\S+)(?:[^\n\S]+|$)(?P<argument>\S*)[^\n]*"#;

fn size(directive: &str, argument: Option<&String>) -> Result<usize> {
    Ok(match directive {
        opcode if OPCODES.get().unwrap().contains_key(opcode) => 3,
        "START" => 0,
        "BYTE" => {
            let argument =
                argument.ok_or_else(|| anyhow::Error::msg("Byte directive requires argument"))?;
            if argument.starts_with("C'") {
                argument.len() - 3
            } else if argument.starts_with("X'") {
                (argument.len() - 3) / 2
            } else {
                return Err(anyhow::Error::msg(format!(
                    "Invalid byte directive {}",
                    argument
                )));
            }
        }
        "WORD" => 3,
        "RESW" => {
            let argument =
                argument.ok_or_else(|| anyhow::Error::msg("RESW directive requires argument"))?;
            argument.parse::<usize>()? * 3
        }
        "RESB" => {
            let argument =
                argument.ok_or_else(|| anyhow::Error::msg("RESB directive requires argument"))?;
            argument.parse::<usize>()?
        }
        "END" => 0,
        unknown => return Err(anyhow::Error::msg(format!("Unknown directive {}", unknown))),
    })
}

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
    argument: Option<String>,
    size: usize,
    offset: usize,
}

fn parse_line(line: &str, offset: usize) -> Result<Option<ParsedLine>> {
    LINE_REGEX
        .get()
        .unwrap()
        .captures(line)
        .map(|cap| {
            let directive = cap
                .name("directive")
                .map(|m| m.as_str().to_owned())
                .unwrap();
            let argument = cap
                .name("argument")
                .filter(|m| m.as_str().len() > 0)
                .map(|m| m.as_str().to_owned());

            let size = size(&directive, argument.as_ref())?;

            Ok(ParsedLine {
                label: cap.name("label").map(|m| m.as_str().to_owned()),
                directive,
                argument,
                size,
                offset,
            })
        })
        .transpose()
}

#[derive(Debug)]
enum Data {
    Instruction(Instruction),
    Byte(Vec<u8>),
    Word(u32),
}

#[derive(Debug)]
struct Instruction {
    opcode: u8,
    indexed: bool,
    target: u16,
}

#[derive(Debug)]
struct Text {
    address: usize,
    instructions: Vec<Data>,
}

#[derive(Debug)]
enum Record {
    Header {
        name: String,
        start: usize,
        length: usize,
    },
    Text(Text),
    End {
        first_instruction: usize,
    },
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Record::Header {
                name,
                start,
                length,
            } => {
                write!(f, "H{:<6}{:0>6X}{:0>6X}", name, start, length)
            }
            Record::Text(text) => {
                write!(
                    f,
                    "T{:0>6X}{:0>2X}",
                    text.address,
                    text.instructions
                        .iter()
                        .map(|i| match i {
                            Data::Instruction(_) => 3,
                            Data::Byte(bytes) => bytes.len(),
                            Data::Word(_) => 3,
                        })
                        .sum::<usize>()
                )?;
                for instruction in text.instructions.iter() {
                    match instruction {
                        Data::Instruction(instruction) => {
                            let mut target = instruction.target & 0x7FFF;
                            if instruction.indexed {
                                target += 0x8000;
                            }
                            write!(f, "{:0>2X}{:0>4X}", instruction.opcode, target)?;
                        }
                        Data::Byte(bytes) => {
                            for byte in bytes {
                                write!(f, "{:0>2X}", byte)?;
                            }
                        }
                        Data::Word(word) => {
                            let [_, a, b, c] = word.to_be_bytes();
                            write!(f, "{:0>2X}{:0>2X}{:0>2X}", a, b, c)?;
                        }
                    }
                }

                Ok(())
            }
            Record::End { first_instruction } => write!(f, "E{:0>6X}", first_instruction),
        }
    }
}

fn main() -> Result<()> {
    OPCODES.set(build_opcodes()).unwrap();
    LINE_REGEX.set(Regex::new(LINE_REGEX_PATTERN)?).unwrap();

    let file = File::open("example.asm")?;

    let mut cur_loc = 0;
    let mut lines = vec![];
    let mut labels = HashMap::new();

    for line in BufReader::new(file).lines() {
        let parsed = parse_line(&line?, cur_loc)?;
        if let Some(parsed) = parsed {
            cur_loc += parsed.size;
            if let Some(label) = parsed.label.as_ref() {
                //TODO: Assumes only one START
                labels.insert(label.clone(), parsed.offset);
            }
            lines.push(parsed);
        }
    }

    let start = &lines[0];
    let end = &lines[lines.len() - 1];

    if start.directive != "START" {
        return Err(anyhow::Error::msg("Expected start directive"));
    }
    if end.directive != "END" {
        return Err(anyhow::Error::msg("Expected end directive"));
    }

    let mut records = vec![];

    let name = start.label.as_ref().expect("Expected name").clone();
    let base = usize::from_str_radix(start.argument.as_ref().expect("Start argument"), 16).unwrap();
    let length = end.offset;

    records.push(Record::Header {
        name,
        start: base,
        length,
    });

    let mut i = 1;

    let mut cur_text = None;

    while i < lines.len() {
        let line = &lines[i];
        let text = cur_text.take();
        if let Some(op) = OPCODES.get().unwrap().get(&line.directive) {
            let mut text = text.unwrap_or_else(|| Text {
                address: line.offset + base,
                instructions: vec![],
            });
            let indexed = line
                .argument
                .as_ref()
                .map(|arg| arg.contains(",X"))
                .unwrap_or(false);
            let target = line
                .argument
                .as_ref()
                .map(|arg| {
                    let arg = if indexed {
                        &arg[..arg.len() - 2]
                    } else {
                        arg.as_str()
                    };
                    Ok::<usize, anyhow::Error>(*labels.get(arg).ok_or_else(|| {
                        anyhow::Error::msg(format!("Couldn't find label {}", arg))
                    })?)
                })
                .transpose()?
                .map(|addr| addr + base)
                .unwrap_or(0) as u16;
            text.instructions.push(Data::Instruction(Instruction {
                opcode: *op,
                indexed,
                target,
            }));
            if text.instructions.len() == 10 {
                records.push(Record::Text(text));
            } else {
                cur_text = Some(text);
            }
        } else if line.directive == "BYTE" {
            let mut text = text.unwrap_or_else(|| Text {
                address: line.offset + base,
                instructions: vec![],
            });
            if let Some((t, v)) = line
                .argument
                .as_ref()
                .ok_or_else(|| anyhow::Error::msg("Byte requires argument"))?
                .split_once("'")
            {
                match t {
                    "X" => {
                        let bytes = v[..v.len() - 1]
                            .chars()
                            .collect::<Vec<char>>()
                            .chunks(2)
                            .map(|c| c.iter().collect::<String>())
                            .map(|s| u8::from_str_radix(&s, 16))
                            .collect::<Result<Vec<_>, _>>()?;
                        text.instructions.push(Data::Byte(bytes));
                    }
                    "C" => {
                        let bytes = v[..v.len() - 1]
                            .chars()
                            .map(|c| c as u8)
                            .collect::<Vec<_>>();
                        text.instructions.push(Data::Byte(bytes));
                    }
                    _ => return Err(anyhow::Error::msg("Invalid byte argument")),
                }
            } else {
                return Err(anyhow::Error::msg("Invalid byte argument"));
            }
            if text.instructions.len() == 10 {
                records.push(Record::Text(text));
            } else {
                cur_text = Some(text);
            }
        } else if line.directive == "WORD" {
            let mut text = text.unwrap_or_else(|| Text {
                address: line.offset + base,
                instructions: vec![],
            });
            let argument = line
                .argument
                .as_ref()
                .ok_or_else(|| anyhow::Error::msg("Invalid word argument"))?;
            text.instructions
                .push(Data::Word(u32::from_str_radix(&argument, 10)?));
            if text.instructions.len() == 10 {
                records.push(Record::Text(text));
            } else {
                cur_text = Some(text);
            }
        } else {
            if let Some(text) = text {
                records.push(Record::Text(text));
            }
        }
        i += 1;
    }

    records.push(Record::End {
        first_instruction: base,
    });

    for record in records {
        println!("{}", record);
    }

    Ok(())
}

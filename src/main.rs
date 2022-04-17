use anyhow::Result;
use directive::{Assembler, Directive};
use libsic::xe::op::{
    AddressMode, AddressRelativeTo, OneReg, Op, Register, Shift, TwoReg, Variable,
};
use line::parse_line;
use record::{Data, Modification, Record, Text};
use regex::Regex;
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::{BufRead, BufReader},
};

mod directive;
mod line;
mod record;

use once_cell::sync::OnceCell;

pub static REGISTERS: OnceCell<HashMap<String, Register>> = OnceCell::new();
pub static LINE_REGEX: OnceCell<Regex> = OnceCell::new();
//static LINE_REGEX_PATTERN: &str = r#"^(([^.\s]\S*)|\s)\s+(\S+)[^\n\S]+([^\s\n,]*)(,X)?[^\n]*"#;
static LINE_REGEX_PATTERN: &str =
    r#"^(?:(?P<label>[^.\s]\S*)|\s)\s+(?P<directive>\S+)(?:[^\n\S]+|$)(?P<argument>\S*)[^\n]*"#;

fn main() -> Result<()> {
    LINE_REGEX.set(Regex::new(LINE_REGEX_PATTERN)?).unwrap();
    REGISTERS
        .set(
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
            .into(),
        )
        .unwrap();
    let filename: String = env::args().skip(1).next().expect("Need asm filename");
    let file = File::open(filename).unwrap();

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

    if start.directive != Directive::Assembler(Assembler::START) {
        return Err(anyhow::Error::msg("Expected start directive"));
    }
    if end.directive != Directive::Assembler(Assembler::END) {
        return Err(anyhow::Error::msg("Expected end directive"));
    }

    let mut records = vec![];
    let mut modifications = vec![];

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
    let mut cur_base = None;

    while i < lines.len() {
        let line = &lines[i];
        let text = cur_text.take();
        //println!("Line: {:?} \n\tcur: {:?}\n\tlrec: {:?}\n\n", line, text, records.last());
        match line.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START => {}
                Assembler::BASE => {
                    cur_base = Some(*labels.get(line.argument.as_ref().unwrap()).unwrap());
                    cur_text = text;
                }
                Assembler::BYTE => {
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
                        let mut bytes = match t {
                            "X" => {
                                let bytes = v[..v.len() - 1]
                                    .chars()
                                    .collect::<Vec<char>>()
                                    .chunks(2)
                                    .map(|c| c.iter().collect::<String>())
                                    .map(|s| u8::from_str_radix(&s, 16))
                                    .collect::<Result<Vec<_>, _>>()?;
                                bytes
                            }
                            "C" => {
                                let bytes = v[..v.len() - 1]
                                    .chars()
                                    .map(|c| c as u8)
                                    .collect::<Vec<_>>();
                                bytes
                            }
                            _ => return Err(anyhow::Error::msg("Invalid byte argument")),
                        };
                        while bytes.len() > 0 {
                            let space_remaining = 30 - text.len();
                            if space_remaining < bytes.len() {
                                let new_text: Vec<u8> = bytes.drain(..space_remaining).collect();
                                let new_bytes = new_text.len();
                                text.instructions.push(Data::Byte(new_text));
                                records.push(Record::Text(text));
                                text = Text {
                                    address: line.offset + base + new_bytes,
                                    instructions: vec![],
                                };
                            } else {
                                text.instructions.push(Data::Byte(bytes));
                                break;
                            }
                        }
                        cur_text = Some(text);
                    } else {
                        return Err(anyhow::Error::msg("Invalid byte argument"));
                    }
                }
                Assembler::WORD => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + base,
                        instructions: vec![],
                    });
                    if text.len() > 27 {
                        records.push(Record::Text(text));
                        text = Text {
                            address: line.offset + base,
                            instructions: vec![],
                        };
                    }
                    let argument = line
                        .argument
                        .as_ref()
                        .ok_or_else(|| anyhow::Error::msg("Invalid word argument"))?;
                    text.instructions
                        .push(Data::Word(u32::from_str_radix(&argument, 10)?));
                    cur_text = Some(text);
                }
                Assembler::RESW | Assembler::RESB | Assembler::END => {
                    if let Some(text) = text {
                        records.push(Record::Text(text));
                    }
                }
            },
            Directive::OneByte(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });
                if text.len() == 30 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                let op = Op::OneByte(opcode);
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::OneReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                let r1 = *REGISTERS
                    .get()
                    .unwrap()
                    .get(line.argument.as_ref().unwrap())
                    .unwrap();

                let op = Op::OneReg(OneReg { opcode, r1 });
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::TwoReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }

                let (r1s, r2s) = line.argument.as_ref().unwrap().split_once(",").unwrap();
                let r1 = *REGISTERS.get().unwrap().get(r1s).unwrap();
                let r2 = *REGISTERS.get().unwrap().get(r2s).unwrap();

                let op = Op::TwoReg(TwoReg { opcode, r1, r2 });
                text.instructions.push(Data::Instruction(op));

                cur_text = Some(text);
            }
            Directive::Shift(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }

                let (r1s, ns) = line.argument.as_ref().unwrap().split_once(",").unwrap();
                let r1 = *REGISTERS.get().unwrap().get(r1s).unwrap();
                let n: u8 = ns.parse().unwrap();

                let op = Op::Shift(Shift { opcode, r1, n });
                text.instructions.push(Data::Instruction(op));

                cur_text = Some(text);
            }
            Directive::Variable(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });

                let (address, address_flags) = line.parse_flags(cur_base, &labels);

                if address_flags.mode != AddressMode::Immediate
                    && address_flags.relative_to == AddressRelativeTo::Direct
                {
                    let length = if address_flags.mode == AddressMode::Compatiblity {
                        4
                    } else if address_flags.extended {
                        5
                    } else {
                        3
                    };

                    let address = line.offset + base + 1;

                    modifications.push(Modification { address, length });
                }

                let op = Op::Variable(Variable {
                    opcode,
                    address_flags,
                    address,
                });
                if text.len() + op.len() as usize > 30 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::SVC => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                let n = line.argument.as_ref().unwrap().parse().unwrap();

                let op = Op::Svc(n);
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
        };
        i += 1;
    }

    for modification in modifications {
        records.push(Record::Modification(modification));
    }

    records.push(Record::End {
        first_instruction: *labels
            .get(end.argument.as_ref().expect("End argument"))
            .unwrap()
            + base,
    });

    for record in records {
        println!("{}", record);
    }

    Ok(())
}

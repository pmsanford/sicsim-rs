use anyhow::Result;
use constants::register;
use directive::{Assembler, Directive};
use labels::Labels;
use libsic::xe::op::{AddressMode, AddressRelativeTo, OneReg, Op, Shift, TwoReg, Variable};
use line::parse_line;
use record::{Data, Modification, Record, Text};
use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
};

mod constants;
mod directive;
mod labels;
mod line;
mod record;

fn main() -> Result<()> {
    let filename: String = env::args()
        .nth(1)
        .ok_or_else(|| anyhow::Error::msg("Need a filename"))?;
    let file = File::open(filename)?;

    let mut cur_loc = 0;
    let mut lines = vec![];
    let mut labels = Labels::new();

    for line in BufReader::new(file).lines() {
        let parsed = parse_line(&line?, cur_loc)?;
        if let Some(parsed) = parsed {
            cur_loc += parsed.size;
            if let Some(label) = parsed.label.as_ref() {
                //TODO: Assumes only one START
                labels.add(label.clone(), parsed.offset);
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
    let start_addr = usize::from_str_radix(start.get_argument()?, 16)?;
    let length = end.offset;

    records.push(Record::Header {
        name,
        start: start_addr,
        length,
    });

    let mut i = 1;

    let mut cur_text = None;
    let mut cur_base = None;

    while i < lines.len() {
        let line = &lines[i];
        let text = cur_text.take();
        match line.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START => {}
                Assembler::BASE => {
                    cur_base = Some(labels.get(line.get_argument()?)?);
                    cur_text = text;
                }
                Assembler::BYTE => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    });
                    if let Some((t, v)) = line.get_argument()?.split_once('\'') {
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
                        while !bytes.is_empty() {
                            let space_remaining = 30 - text.len();
                            if space_remaining < bytes.len() {
                                let new_text: Vec<u8> = bytes.drain(..space_remaining).collect();
                                let new_bytes = new_text.len();
                                text.instructions.push(Data::Byte(new_text));
                                records.push(Record::Text(text));
                                text = Text {
                                    address: line.offset + start_addr + new_bytes,
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
                        address: line.offset + start_addr,
                        instructions: vec![],
                    });
                    if text.len() > 27 {
                        records.push(Record::Text(text));
                        text = Text {
                            address: line.offset + start_addr,
                            instructions: vec![],
                        };
                    }
                    let argument = line.get_argument()?;
                    text.instructions.push(Data::Word(argument.parse()?));
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
                    address: line.offset + start_addr,
                    instructions: vec![],
                });
                if text.len() == 30 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                let op = Op::OneByte(opcode);
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::OneReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                let r1 = register(line.get_argument()?)?;

                let op = Op::OneReg(OneReg { opcode, r1 });
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::TwoReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }

                let (r1s, r2s) = line
                    .get_argument()?
                    .split_once(',')
                    .ok_or_else(|| anyhow::Error::msg("Malformed TwoReg argument"))?;
                let r1 = register(r1s)?;
                let r2 = register(r2s)?;

                let op = Op::TwoReg(TwoReg { opcode, r1, r2 });
                text.instructions.push(Data::Instruction(op));

                cur_text = Some(text);
            }
            Directive::Shift(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }

                let (r1s, ns) = line
                    .get_argument()?
                    .split_once(',')
                    .ok_or_else(|| anyhow::Error::msg("Malformed TwoReg argument"))?;
                let r1 = register(r1s)?;
                let n: u8 = ns.parse()?;

                let op = Op::Shift(Shift { opcode, r1, n });
                text.instructions.push(Data::Instruction(op));

                cur_text = Some(text);
            }
            Directive::Variable(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });

                let (address, address_flags) = line.parse_flags(cur_base, &labels)?;

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

                    let address = line.offset + start_addr + 1;

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
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::SVC => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                let n = line.get_argument()?.parse()?;

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
        first_instruction: labels.get(end.get_argument()?)? + start_addr,
    });

    for record in records {
        println!("{}", record);
    }

    Ok(())
}

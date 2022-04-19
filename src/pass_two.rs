use crate::{
    constants::register,
    directive::{Assembler, Directive},
    pass_one::PassOne,
    record::{Data, Modification, Record, Text},
};
use anyhow::Result;
use libsic::xe::op::{AddressMode, AddressRelativeTo, OneReg, Op, Shift, TwoReg, Variable};

pub fn parse_literal(arg: &str) -> Result<Vec<u8>> {
    let (t, v) = arg
        .split_once('\'')
        .ok_or_else(|| anyhow::Error::msg("Invalid byte argument"))?;
    Ok(match t {
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
    })
}

pub fn pass_two(pass_one: &PassOne) -> Result<Vec<Record>> {
    let start = &pass_one.parsed_lines[0];
    let end = &pass_one.parsed_lines[pass_one.parsed_lines.len() - 1];

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

    let mut cur_text = None;
    let mut cur_base = None;
    let mut literals = Vec::new();

    for line in pass_one.parsed_lines.iter() {
        let text = cur_text.take();
        match line.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START => {}
                Assembler::BASE => {
                    cur_base = Some(pass_one.labels.get(line.get_argument()?)?);
                    cur_text = text;
                }
                Assembler::BYTE => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    });
                    let mut bytes = parse_literal(line.get_argument()?)?;
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
                }
                Assembler::EQU => {}
                Assembler::LTORG => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    });

                    while !literals.is_empty() {
                        let space_remaining = 30 - text.len();
                        if space_remaining < literals.len() {
                            let new_text: Vec<u8> = literals.drain(..space_remaining).collect();
                            let new_literals = new_text.len();
                            text.instructions.push(Data::Byte(new_text));
                            records.push(Record::Text(text));
                            text = Text {
                                address: line.offset + start_addr + new_literals,
                                instructions: vec![],
                            };
                        } else {
                            text.instructions.push(Data::Byte(literals));
                            break;
                        }
                    }
                    literals = Vec::new();
                    cur_text = Some(text);
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

                // Parse literal
                if line.literal_offset.is_some() {
                    let argument = line.get_argument()?;
                    let mut new_lits = parse_literal(&argument[1..])?;
                    literals.append(&mut new_lits);
                }

                let (address, address_flags) =
                    line.parse_flags(cur_base, &pass_one.labels, &pass_one.literal_offsets)?;

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
    }

    for modification in modifications {
        records.push(Record::Modification(modification));
    }

    records.push(Record::End {
        first_instruction: pass_one.labels.get(end.get_argument()?)? + start_addr,
    });

    Ok(records)
}

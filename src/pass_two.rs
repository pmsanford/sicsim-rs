use std::mem;

use crate::{
    constants::register,
    directive::{Assembler, Directive},
    pass_one::{ArgumentToken, ParsedLine, PassOne},
    record::{Data, Modification, Record, Text},
};
use anyhow::{Context, Result};
use libsic::xe::op::{AddressMode, AddressRelativeTo, OneReg, Op, Shift, TwoReg, Variable};

struct PassTwo {
    cur_text: Option<Text>,
    cur_base: Option<usize>,
    literals: Vec<u8>,
    records: Vec<Record>,
    modifications: Vec<Modification>,
    pass_one: PassOne,
}

impl PassTwo {
    fn new(pass_one: PassOne) -> Self {
        Self {
            cur_text: None,
            cur_base: None,
            literals: Vec::new(),
            records: Vec::new(),
            modifications: Vec::new(),
            pass_one,
        }
    }

    fn assemble_lines(mut self) -> Result<Vec<Record>> {
        let start = &self.pass_one.parsed_lines[0].clone();
        let end = &self.pass_one.parsed_lines[self.pass_one.parsed_lines.len() - 1].clone();

        if start.directive != Directive::Assembler(Assembler::START) {
            return Err(anyhow::Error::msg("Expected start directive"));
        }
        if end.directive != Directive::Assembler(Assembler::END) {
            return Err(anyhow::Error::msg("Expected end directive"));
        }

        let start_addr = usize::from_str_radix(start.get_argument()?, 16)?;

        let name = start.label.as_ref().expect("Expected name").clone();
        let length = end.offset;

        self.records.push(Record::Header {
            name,
            start: start_addr,
            length,
        });

        let lines = mem::take(&mut self.pass_one.parsed_lines);
        for (line_no, line) in lines.iter().enumerate() {
            self.assemble_line(line, start_addr)
                .context(format!("Error in {:?} on line {}", line.directive, line_no))?;
        }

        for modification in self.modifications {
            self.records.push(Record::Modification(modification));
        }

        self.records.push(Record::End {
            first_instruction: self.pass_one.labels.get(end.get_argument()?)? + start_addr,
        });

        Ok(self.records)
    }

    fn assemble_line(&mut self, line: &ParsedLine, start_addr: usize) -> Result<()> {
        let text = self.cur_text.take();
        match line.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START => {}
                Assembler::BASE => {
                    self.cur_base = Some(self.pass_one.labels.get(line.get_argument()?)?);
                    self.cur_text = text;
                }
                Assembler::BYTE => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    });
                    let mut bytes = parse_literal(&line.argument)?;
                    while !bytes.is_empty() {
                        let space_remaining = 30 - text.len();
                        if space_remaining < bytes.len() {
                            let new_text: Vec<u8> = bytes.drain(..space_remaining).collect();
                            let new_bytes = new_text.len();
                            text.instructions.push(Data::Byte(new_text));
                            self.records.push(Record::Text(text));
                            text = Text {
                                address: line.offset + start_addr + new_bytes,
                                instructions: vec![],
                            };
                        } else {
                            text.instructions.push(Data::Byte(bytes));
                            break;
                        }
                    }
                    self.cur_text = Some(text);
                }
                Assembler::EQU => {}
                Assembler::ORG => {}
                Assembler::LTORG => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    });

                    while !self.literals.is_empty() {
                        let space_remaining = 30 - text.len();
                        if space_remaining < self.literals.len() {
                            let new_text: Vec<u8> =
                                self.literals.drain(..space_remaining).collect();
                            let new_literals = new_text.len();
                            text.instructions.push(Data::Byte(new_text));
                            self.records.push(Record::Text(text));
                            text = Text {
                                address: line.offset + start_addr + new_literals,
                                instructions: vec![],
                            };
                        } else {
                            let literals = mem::take(&mut self.literals);
                            text.instructions.push(Data::Byte(literals));
                            break;
                        }
                    }
                    self.cur_text = Some(text);
                }
                Assembler::WORD => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    });
                    if text.len() > 27 {
                        self.records.push(Record::Text(text));
                        text = Text {
                            address: line.offset + start_addr,
                            instructions: vec![],
                        };
                    }
                    let argument = line.get_argument()?;
                    let val = if argument.chars().all(char::is_numeric) {
                        argument.parse::<u32>()?
                    } else {
                        self.pass_one.labels.get(argument)? as u32
                    };
                    text.instructions.push(Data::Word(val));
                    self.cur_text = Some(text);
                }
                Assembler::RESW | Assembler::RESB | Assembler::END => {
                    if let Some(text) = text {
                        self.records.push(Record::Text(text));
                    }
                }
            },
            Directive::OneByte(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });
                if text.len() == 30 {
                    self.records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                let op = Op::OneByte(opcode);
                text.instructions.push(Data::Instruction(op));
                self.cur_text = Some(text);
            }
            Directive::OneReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    self.records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                let r1 = register(line.get_argument()?)?;

                let op = Op::OneReg(OneReg { opcode, r1 });
                text.instructions.push(Data::Instruction(op));
                self.cur_text = Some(text);
            }
            Directive::TwoReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    self.records.push(Record::Text(text));
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

                self.cur_text = Some(text);
            }
            Directive::Shift(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    self.records.push(Record::Text(text));
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

                self.cur_text = Some(text);
            }
            Directive::Variable(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });

                // Parse literal
                if line.literal_offset.is_some() {
                    let mut new_lits = parse_literal(&line.argument)?;
                    self.literals.append(&mut new_lits);
                }

                let (address, address_flags) = line.parse_flags(
                    self.cur_base,
                    &self.pass_one.labels,
                    &self.pass_one.literal_offsets,
                )?;

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

                    self.modifications.push(Modification { address, length });
                }

                let op = Op::Variable(Variable {
                    opcode,
                    address_flags,
                    address,
                });
                if text.len() + op.len() as usize > 30 {
                    self.records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                text.instructions.push(Data::Instruction(op));
                self.cur_text = Some(text);
            }
            Directive::SVC => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + start_addr,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    self.records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + start_addr,
                        instructions: vec![],
                    };
                }
                let n = line.get_argument()?.parse()?;

                let op = Op::Svc(n);
                text.instructions.push(Data::Instruction(op));
                self.cur_text = Some(text);
            }
        };

        Ok(())
    }
}

pub fn parse_literal(arg: &ArgumentToken) -> Result<Vec<u8>> {
    Ok(match arg {
        ArgumentToken::LiteralBytes(v) => {
            let bytes = v
                .chars()
                .collect::<Vec<char>>()
                .chunks(2)
                .map(|c| c.iter().collect::<String>())
                .map(|s| u8::from_str_radix(&s, 16))
                .collect::<Result<Vec<_>, _>>()?;
            bytes
        }
        ArgumentToken::LiteralChars(v) => {
            let bytes = v.chars().map(|c| c as u8).collect::<Vec<_>>();
            bytes
        }
        _ => return Err(anyhow::Error::msg("Invalid byte argument")),
    })
}

pub fn pass_two(pass_one: PassOne) -> Result<Vec<Record>> {
    let pass = PassTwo::new(pass_one);
    pass.assemble_lines()
}

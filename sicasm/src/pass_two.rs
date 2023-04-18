use std::mem;

use crate::{
    constants::register,
    directive::{Assembler, Directive},
    pass_one::{parse_literal, ArgumentToken, ParsedLine, PassOne},
    record::{Data, Modification, Record, Text},
};
use anyhow::{Context, Result};
use libsic::xe::op::{AddressMode, AddressRelativeTo, OneReg, Op, Shift, TwoReg, Variable};

struct PassTwo {
    cur_text: Option<Text>,
    cur_base: Option<usize>,
    records: Vec<Record>,
    modifications: Vec<Modification>,
    start_addr: usize,
    end_line: ParsedLine,
    pass_one: PassOne,
}

impl PassTwo {
    fn new(pass_one: PassOne) -> Result<Self> {
        let start = &pass_one.parsed_lines[0].clone();
        let end_line = pass_one.parsed_lines[pass_one.parsed_lines.len() - 1].clone();

        if start.directive != Directive::Assembler(Assembler::START) {
            return Err(anyhow::Error::msg("Expected start directive"));
        }
        if end_line.directive != Directive::Assembler(Assembler::END) {
            return Err(anyhow::Error::msg("Expected end directive"));
        }

        let start_addr = usize::from_str_radix(start.get_argument()?, 16)?;

        let name = start.label.as_ref().expect("Expected name").clone();
        let length = end_line.offset;

        let records = vec![Record::Header {
            name,
            start: start_addr,
            length,
        }];

        Ok(Self {
            cur_text: None,
            cur_base: None,
            records,
            modifications: Vec::new(),
            start_addr,
            end_line,
            pass_one,
        })
    }

    fn assemble_lines(mut self) -> Result<Vec<Record>> {
        let lines = mem::take(&mut self.pass_one.parsed_lines);
        for (line_no, line) in lines.iter().enumerate() {
            self.assemble_line(line)
                .context(format!("Error in {:?} on line {}", line.directive, line_no))?;
        }

        for modification in self.modifications {
            self.records.push(Record::Modification(modification));
        }

        self.records.push(Record::End {
            first_instruction: self
                .pass_one
                .labels
                .absolute(self.end_line.get_argument()?)?
                + self.start_addr,
        });

        Ok(self.records)
    }

    fn add_instruction(&mut self, offset: usize, mut instruction: Data) {
        let offset = offset + self.start_addr;
        let mut text = self.cur_text.take().unwrap_or_else(|| Text::new(offset));

        if let Data::Byte(mut bytes) = instruction {
            while !bytes.is_empty() {
                let space_remaining = 30 - text.len();
                if space_remaining < bytes.len() {
                    let new_text: Vec<u8> = bytes.drain(..space_remaining).collect();
                    let new_bytes = new_text.len();
                    text.instructions.push(Data::Byte(new_text));
                    self.records.push(Record::Text(text));
                    text = Text {
                        address: offset + new_bytes,
                        instructions: vec![],
                    };
                } else {
                    break;
                }
            }

            instruction = Data::Byte(bytes);
        } else if text.len() + instruction.len() > 30 {
            self.records.push(Record::Text(text));
            text = Text::new(offset);
        }
        text.instructions.push(instruction);
        self.cur_text = Some(text);
    }

    #[allow(clippy::too_many_lines)]
    fn assemble_line(&mut self, line: &ParsedLine) -> Result<()> {
        match line.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START | Assembler::EQU | Assembler::ORG | Assembler::USE => {}
                Assembler::BASE => {
                    self.cur_base = Some(self.pass_one.labels.absolute(line.get_argument()?)?);
                }
                Assembler::BYTE => {
                    if let ArgumentToken::Literal(ref lit) = line.argument {
                        let bytes = parse_literal(lit)?;
                        self.add_instruction(line.offset, Data::Byte(bytes));
                    } else {
                        return Err(anyhow::Error::msg("Invalid BYTE literal"));
                    }
                }
                Assembler::LTORG => {
                    let ltorg = self.pass_one.ltorgs.get_mut(&line.offset).ok_or_else(|| {
                        anyhow::Error::msg(format!(
                            "Couldn't find ltorg for offset {}",
                            line.offset
                        ))
                    })?;
                    let offset = ltorg.offset;
                    let literals = mem::take(&mut ltorg.data);
                    self.add_instruction(offset, Data::Byte(literals));
                }
                Assembler::WORD => {
                    let argument = line.get_argument()?;
                    let val = if argument.chars().all(char::is_numeric) {
                        argument.parse::<u32>()?
                    } else {
                        self.pass_one.labels.absolute(argument)?.try_into()?
                    };
                    self.add_instruction(line.offset, Data::Word(val));
                }
                Assembler::RESW | Assembler::RESB | Assembler::END => {
                    if matches!(asm, Assembler::END) {
                        let offset = self.pass_one.final_ltorg.offset;
                        let literals = mem::take(&mut self.pass_one.final_ltorg.data);
                        if !literals.is_empty() {
                            self.add_instruction(offset, Data::Byte(literals));
                        }
                    }
                    if let Some(text) = self.cur_text.take() {
                        self.records.push(Record::Text(text));
                    }
                }
            },
            Directive::OneByte(opcode) => {
                let op = Op::OneByte(opcode);
                self.add_instruction(line.offset, Data::Instruction(op));
            }
            Directive::OneReg(opcode) => {
                let r1 = register(line.get_argument()?)?;

                let op = Op::OneReg(OneReg { opcode, r1 });
                self.add_instruction(line.offset, Data::Instruction(op));
            }
            Directive::TwoReg(opcode) => {
                let (r1s, r2s) = line
                    .get_argument()?
                    .split_once(',')
                    .ok_or_else(|| anyhow::Error::msg("Malformed TwoReg argument"))?;
                let r1 = register(r1s)?;
                let r2 = register(r2s)?;

                let op = Op::TwoReg(TwoReg { opcode, r1, r2 });
                self.add_instruction(line.offset, Data::Instruction(op));
            }
            Directive::Shift(opcode) => {
                let (r1s, ns) = line
                    .get_argument()?
                    .split_once(',')
                    .ok_or_else(|| anyhow::Error::msg("Malformed TwoReg argument"))?;
                let r1 = register(r1s)?;
                let n = ns.parse::<u8>()? - 1;

                let op = Op::Shift(Shift { opcode, r1, n });
                self.add_instruction(line.offset, Data::Instruction(op));
            }
            Directive::Variable(opcode) => {
                let (address, address_flags) = line.parse_variable_flags(
                    self.cur_base,
                    &self.pass_one.labels,
                    &self.pass_one.littab,
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

                    let address = line.offset + self.start_addr + 1;

                    self.modifications.push(Modification { address, length });
                }

                let op = Op::Variable(Variable {
                    opcode,
                    address_flags,
                    address,
                });
                self.add_instruction(line.offset, Data::Instruction(op));
            }
            Directive::SVC => {
                let n = line.get_argument()?.parse()?;

                let op = Op::Svc(n);
                self.add_instruction(line.offset, Data::Instruction(op));
            }
        };

        Ok(())
    }
}

pub fn pass_two(pass_one: PassOne) -> Result<Vec<Record>> {
    let pass = PassTwo::new(pass_one)?;
    pass.assemble_lines()
}

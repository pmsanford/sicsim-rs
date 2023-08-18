use std::{collections::HashMap, fmt::Write, mem, sync::OnceLock};

use anyhow::{anyhow, bail, Result};
use data::AsmData;
use libsic::xe::op::{
    AddressFlags, AddressMode, AddressRelativeTo, OneReg, Op, Register, Shift, TwoReg, Variable,
    VariableOp,
};
use parser::{Argument, Arguments, Assembler, Directive, Value};
use record::{Data, Record, Text};
use sicdbg::Sdb;

use crate::{
    parser::{AddressModifier, AsmArg},
    record::Modification,
};

pub mod data;
pub mod models;
pub mod parser;
pub mod pass_one;
pub mod record;
pub mod schema;

static MAX_DISP: u16 = 4095; // 0x0F_FF
static MAX_PC: i32 = 2047; // 0x07_FF
static MIN_PC: i32 = -2048; // 0x08_00

pub static REGISTERS: OnceLock<HashMap<String, Register>> = OnceLock::new();
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

pub fn pass_two(mut data: AsmData) -> Result<String> {
    let lines = data.get_lines()?;

    let start = &lines[0];
    if start.directive != Directive::Command(Assembler::START) {
        bail!("expected start directive");
    }

    let end = &lines[lines.len() - 1];

    if end.directive != Directive::Command(Assembler::END) {
        bail!("expected end directive");
    }

    let Some(Argument::Value(Value::Number(start_addr))) = start.argument else { bail!("expected start address"); };

    let Some(start_label) = data.get_label_by_line(start.line_no)? else { bail!("expected program name"); };

    let name = start_label.label_name;

    // TODO: does this work with program blocks?
    let length = end.offset;

    let mut debug = Sdb::new(&name, start_addr as usize);

    let labels = data.get_labels()?;

    for label in labels {
        debug.add_label(label.label_name, label.offset as u32);
    }

    let mut records = vec![Record::Header {
        name,
        start: start_addr as usize,
        length,
    }];

    let mut modifications = vec![];

    let mut cur_base = None;

    let mut current_text: Option<Text> = None;

    for line in lines.iter().skip(1) {
        fn add_instruction(
            current_text: &mut Option<Text>,
            records: &mut Vec<Record>,
            offset: usize,
            mut instruction: Data,
        ) {
            let mut text = current_text.take().unwrap_or_else(|| Text::new(offset));
            if let Data::Byte(mut bytes) = instruction {
                while !bytes.is_empty() {
                    let space_remaining = 30 - text.len();
                    if space_remaining < bytes.len() {
                        let new_text: Vec<u8> = bytes.drain(..space_remaining).collect();
                        let new_bytes = new_text.len();
                        text.instructions.push(Data::Byte(new_text));
                        records.push(Record::Text(text));
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
                records.push(Record::Text(text));
                text = Text::new(offset);
            }
            text.instructions.push(instruction);

            *current_text = Some(text);
        }
        debug.add_line(line.offset as u32, line.text.clone(), line.line_no);
        println!("Assembling {line:?}");
        match line.directive {
            Directive::Command(cmd) => match cmd {
                Assembler::START | Assembler::EQU | Assembler::ORG | Assembler::USE => {}
                Assembler::BASE => {
                    let label_name = line.argument.string()?;
                    let Some(label) = data.get_label(&label_name)? else { bail!("couldn't find label for base"); };

                    cur_base = Some(label.offset);
                }
                Assembler::BYTE => {
                    let bytes = line.argument.literal()?;
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        start_addr as usize + line.offset,
                        Data::Byte(bytes),
                    );
                }
                Assembler::LTORG => {
                    let ltorg = data.get_ltorg(&line.block_name, line.offset)?;
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Byte(ltorg.data),
                    );
                }
                Assembler::WORD => {
                    let Some(ref arg) = line.argument else { bail!("WORD requires an argument"); };
                    let arg = match arg {
                        Argument::Value(v) => match v {
                            Value::Bytes(_) | Value::Chars(_) => {
                                bail!("Literal argument for WORD is invalid")
                            }
                            Value::Number(i) => *i,
                            Value::String(s) => {
                                data.get_label(&s.0)?
                                    .ok_or_else(|| anyhow!(r"Couldn't find label {}", s.0))?
                                    .offset
                            }
                        },
                        Argument::Expr(_) => bail!("Found EXPR for WORD value"),
                    };

                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Word(arg as u32),
                    );
                }
                Assembler::RESW | Assembler::RESB | Assembler::END => {
                    if matches!(line.directive, Directive::Command(Assembler::END)) {
                        if let Some(mut final_ltorg) = data.get_final_ltorg("")? {
                            let offset = final_ltorg.offset as usize;
                            let literals = mem::take(&mut final_ltorg.data);
                            if !literals.is_empty() {
                                add_instruction(
                                    &mut current_text,
                                    &mut records,
                                    offset,
                                    Data::Byte(literals),
                                );
                            }
                        }
                    }
                    if let Some(text) = current_text.take() {
                        records.push(Record::Text(text));
                    }
                }
            },
            Directive::Op(op) => match op {
                parser::Op::OneByte(opcode) => {
                    let op = Op::OneByte(opcode);
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Instruction(op),
                    );
                }
                parser::Op::OneReg(opcode) => {
                    let r1 = register(&line.argument.expect_string()?)?;

                    let op = Op::OneReg(OneReg { opcode, r1 });
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Instruction(op),
                    );
                }
                parser::Op::TwoReg(opcode) => {
                    let arg_str = line.argument.expect_string()?;
                    println!("Argstr: {arg_str}");
                    let (r1s, r2s) = arg_str
                        .split_once(',')
                        .ok_or_else(|| anyhow!("Malformed TwoReg argument"))?;
                    let r1 = register(r1s)?;
                    let r2 = register(r2s)?;

                    let op = Op::TwoReg(TwoReg { opcode, r1, r2 });
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Instruction(op),
                    );
                }
                parser::Op::Shift(opcode) => {
                    let arg_str = line.argument.expect_string()?;
                    let (r1s, ns) = arg_str
                        .split_once(',')
                        .ok_or_else(|| anyhow::Error::msg("Malformed TwoReg argument"))?;
                    let r1 = register(r1s)?;
                    let n = ns.parse::<u8>()? - 1;

                    let op = Op::Shift(Shift { opcode, r1, n });
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Instruction(op),
                    );
                }
                parser::Op::Svc => {
                    let n = line.argument.expect_number()? as u8;

                    let op = Op::Svc(n);
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Instruction(op),
                    );
                }
                parser::Op::Variable(opcode) => {
                    let mode = line.address_modifier;
                    let (constant, target_offset) = if line.directive
                        == Directive::Op(parser::Op::Variable(VariableOp::RSUB))
                    {
                        (true, 0)
                    } else {
                        let Some(ref argument) = line.argument else { bail!("No argument for variable op {opcode}"); };

                        match argument {
                            Argument::Value(Value::Bytes(b) | Value::Chars(b)) => (
                                false,
                                data.get_literal(&line.block_name, &b)?
                                    .offset
                                    .ok_or_else(|| anyhow!("unaddressed literal in pass two"))?,
                            ),
                            Argument::Value(Value::Number(i)) => (true, *i),
                            Argument::Value(Value::String(s)) => {
                                println!("label: {s:?}");
                                (
                                    false,
                                    data.get_label(&s.0)?
                                        .ok_or_else(|| anyhow!("couldn't find label {}", s.0))?
                                        .offset,
                                )
                            }
                            Argument::Expr(_) => bail!("expr argument to variable op {opcode}"),
                        }
                    };

                    let pc = line.offset as i32 + 3;
                    let pc_disp = target_offset - pc;
                    let base_disp = cur_base
                        .filter(|base| target_offset >= *base)
                        .map(|base| target_offset - base);

                    let (disp, relative_to) =
                        match (mode, constant, line.extended, pc_disp, base_disp) {
                            (parser::AddressModifier::Immediate, true, _, _, _)
                            | (_, _, true, _, _) => (target_offset, AddressRelativeTo::Direct),
                            (_, _, _, pcd, _)
                                if i32::from(MIN_PC) < pcd && pcd < i32::from(MAX_PC) =>
                            {
                                (pc_disp, AddressRelativeTo::PC)
                            }
                            (_, _, _, _, Some(bd)) if bd < MAX_DISP as i32 => {
                                (bd, AddressRelativeTo::Base)
                            }
                            _ if target_offset < MAX_DISP as i32 => {
                                (target_offset, AddressRelativeTo::Direct)
                            }
                            _ => bail!("couldn't find addressing mode for target"),
                        };

                    let mode = match mode {
                        AddressModifier::Unmodified => AddressMode::Simple,
                        AddressModifier::Indirect => AddressMode::Indirect,
                        AddressModifier::Immediate => AddressMode::Immediate,
                    };

                    if mode != AddressMode::Immediate && relative_to == AddressRelativeTo::Direct {
                        let length = if mode == AddressMode::Compatiblity {
                            4
                        } else if line.extended {
                            5
                        } else {
                            3
                        };

                        let op_address = line.offset + start_addr as usize + 1;

                        modifications.push(Modification {
                            address: op_address,
                            length,
                        });
                    }

                    let address_flags = AddressFlags {
                        mode,
                        relative_to,
                        indexed: line.indexed,
                        extended: line.extended,
                    };

                    let op = Op::Variable(Variable {
                        opcode,
                        address_flags,
                        address: disp as u32,
                    });
                    add_instruction(
                        &mut current_text,
                        &mut records,
                        line.offset,
                        Data::Instruction(op),
                    );
                }
            },
        }
    }

    let mut assembled = String::new();

    for record in records {
        writeln!(&mut assembled, "{}", record)?;
    }
    Ok(assembled)
}

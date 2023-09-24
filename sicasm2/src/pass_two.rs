use std::{collections::HashMap, mem, sync::OnceLock};

use crate::data::AsmData;
use crate::parser::{self, Argument, Arguments, Assembler, Directive, Expr, ExprOp, Value};
use crate::record::{Data, Define, ExtDef, Record, Refer, Text};
use anyhow::{anyhow, bail, Context, Result};
use libsic::xe::op::{
    AddressFlags, AddressMode, AddressRelativeTo, OneReg, Op, Register, Shift, TwoReg, Variable,
    VariableOp,
};
use sicdbg::Sdb;

use crate::{
    parser::{AddressModifier, AsmArg},
    record::Modification,
};
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

struct ControlSectionBuilder {
    records: Vec<Record>,
    modifications: Vec<Modification>,
    extdefs: Vec<ExtDef>,
    extrefs: Vec<String>,
    base: Option<i32>,
    current_text: Option<Text>,
    name: String,
    start_addr: usize,
    length: Option<usize>,
    start_offset: Option<usize>,
}

impl ControlSectionBuilder {
    fn new(name: String, start_addr: usize) -> Self {
        ControlSectionBuilder {
            records: Vec::new(),
            modifications: Vec::new(),
            extdefs: Vec::new(),
            extrefs: Vec::new(),
            base: None,
            current_text: None,
            name,
            start_addr,
            length: None,
            start_offset: None,
        }
    }

    fn set_length(&mut self, length: usize) {
        self.length = Some(length);
    }

    fn set_start_offset(&mut self, start_offset: usize) {
        self.start_offset = Some(start_offset);
    }

    fn add_instruction(&mut self, offset: usize, mut instruction: Data) {
        let mut text = self
            .current_text
            .take()
            .unwrap_or_else(|| Text::new(offset));
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
        } else if text.len() + instruction.length() > 30 {
            self.records.push(Record::Text(text));
            text = Text::new(offset);
        }
        text.instructions.push(instruction);

        self.current_text = Some(text);
    }

    fn build(mut self) -> Result<Vec<Record>> {
        let Some(length) = self.length else {
            bail!("no length set");
        };
        let header = Record::Header {
            name: self.name,
            start: self.start_addr,
            length,
        };
        let mut records = vec![header];
        if !self.extdefs.is_empty() {
            records.push(Record::Define(Define {
                definitions: self.extdefs.clone(),
            }));
        }
        if !self.extrefs.is_empty() {
            records.push(Record::Refer(Refer {
                references: self.extrefs.clone(),
            }));
        }
        if let Some(text) = self.current_text {
            self.records.push(Record::Text(text));
        }
        records.append(&mut self.records);
        for modification in self.modifications {
            records.push(Record::Modification(modification));
        }
        if let Some(start_offset) = self.start_offset {
            records.push(Record::End {
                first_instruction: Some(start_offset + self.start_addr),
            });
        } else {
            records.push(Record::End {
                first_instruction: None,
            });
        }

        Ok(records)
    }
}

struct ExprMod {
    extref: String,
    op: ExprOp,
}

fn collect_exprs(section_name: &str, e: &Expr, data: &mut AsmData) -> Result<Vec<ExprMod>> {
    // For now this only supports extrefs for the symbol and +/- for the op

    // First symbol is always add
    let mods = collect_exprs_recur(e, ExprOp::Add)?;

    for m in mods.iter() {
        if data.get_extref(section_name, &m.extref)?.is_none() {
            bail!("only extrefs are supported for mods");
        }
    }

    Ok(mods)
}

fn make_exprmod(v: &Value, op: ExprOp) -> Result<ExprMod> {
    let Value::String(parser::Label(ref extref)) = v else {
        bail!("expected string for extref value");
    };

    Ok(ExprMod {
        extref: extref.to_owned(),
        op,
    })
}

fn collect_exprs_recur(e: &Expr, op: ExprOp) -> Result<Vec<ExprMod>> {
    let first = make_exprmod(&e.value, op)?;

    let mut rest = match &e.target {
        parser::ExprTarget::Argument(v) => {
            vec![make_exprmod(v, e.op)?]
        }
        parser::ExprTarget::Expr(e) => collect_exprs_recur(e, e.op)?,
    };

    rest.insert(0, first);

    Ok(rest)
}

pub fn pass_two(mut data: AsmData) -> Result<(Vec<Record>, Sdb)> {
    let lines = data.get_lines()?;

    let start = &lines[0];
    if start.directive != Directive::Command(Assembler::START) {
        bail!("expected start directive");
    }

    let end = &lines[lines.len() - 1];

    if end.directive != Directive::Command(Assembler::END) {
        bail!("expected end directive");
    }

    let Some(Argument::Value(Value::Number(start_addr))) = start.argument else {
        bail!("expected start address");
    };

    // Hacky, but start addr (and end addr) are hex, the rest of the source lines are decimal
    let start_addr = i32::from_str_radix(&start_addr.to_string(), 16)?;

    let Some(start_label) = data.get_label_by_line(start.line_no)? else {
        bail!("expected program name");
    };

    let first_csect = start_label.label_name.clone();

    let mut debug = Sdb::new(&first_csect, start_addr as usize);

    let labels = data.get_labels()?;

    for label in labels {
        debug.add_label(label.label_name, label.offset as u32);
    }

    let mut control_sections = Vec::new();

    let mut current_csect = ControlSectionBuilder::new(first_csect.clone(), start_addr as usize);

    for line in lines.iter() {
        let block = data
            .get_program_block(line.block_id)?
            .expect("block for line");
        let addr = block.calc_address(line.offset);
        debug.add_line(addr as u32, line.text.clone(), line.line_no);
        match line.directive {
            Directive::Command(cmd) => match cmd {
                Assembler::START | Assembler::EQU | Assembler::ORG | Assembler::USE => {}
                Assembler::EXTDEF => {
                    let names = line.argument.expect_list().context("for extdef")?;
                    for name in names {
                        let label = data
                            .get_label(&current_csect.name, &name)?
                            .ok_or_else(|| anyhow!("couldn't find label {name} for extdef"))?;
                        let offset = label.offset;
                        current_csect.extdefs.push(ExtDef {
                            name,
                            offset: offset as usize,
                        });
                    }
                }
                Assembler::EXTREF => {
                    let mut names = line.argument.expect_list().context("for extref")?;
                    current_csect.extrefs.append(&mut names);
                }
                Assembler::CSECT => {
                    // TODO: Is the length this simple?
                    let length = data.get_section_length(&current_csect.name)?;
                    if let Some(ltorg) = data.get_ltorg(line.block_id, line.offset)? {
                        current_csect.add_instruction(addr, Data::Byte(ltorg.data));
                    }
                    current_csect.set_length(length as usize);
                    control_sections.push(current_csect);
                    let name = data
                        .get_label_by_line(line.line_no)?
                        .ok_or_else(|| {
                            anyhow!("expected label for CSECT on line {}", line.line_no)
                        })?
                        .label_name
                        .clone();
                    current_csect = ControlSectionBuilder::new(name, 0);
                }
                Assembler::BASE => {
                    let label_name = line.argument.string()?;
                    let Some(label) = data.get_label(&current_csect.name, &label_name)? else {
                        bail!("couldn't find label for base");
                    };

                    current_csect.base = Some(label.offset);
                }
                Assembler::BYTE => {
                    let bytes = line.argument.literal()?;
                    current_csect.add_instruction(addr, Data::Byte(bytes));
                }
                Assembler::LTORG => {
                    let ltorg = data
                        .get_ltorg(line.block_id, line.offset)?
                        .ok_or_else(|| anyhow!("expected LTORG"))?;
                    current_csect.add_instruction(addr, Data::Byte(ltorg.data));
                }
                Assembler::WORD => {
                    let Some(ref arg) = line.argument else {
                        bail!("WORD requires an argument");
                    };
                    let arg = match arg {
                        Argument::Value(v) => match v {
                            Value::Bytes(_) | Value::Chars(_) => {
                                bail!("Literal argument for WORD is invalid")
                            }
                            Value::Number(i) => *i,
                            Value::String(s) => {
                                if let Some(label) = data.get_label(&current_csect.name, &s.0)? {
                                    current_csect.modifications.push(Modification {
                                        address: addr,
                                        length: 6,
                                        add: true,
                                        symbol: start_label.label_name.clone(),
                                    });

                                    label.offset
                                } else if let Some(extref) =
                                    data.get_extref(&current_csect.name, &s.0)?
                                {
                                    current_csect.modifications.push(Modification {
                                        address: addr,
                                        length: 6,
                                        add: true,
                                        symbol: extref.symbol_name,
                                    });

                                    0
                                } else {
                                    bail!("Couldn't find sybol {s:?}");
                                }
                            }
                            Value::List(_) => bail!("got list argument for WORD"),
                        },
                        Argument::Expr(e) => {
                            let mods = collect_exprs(&current_csect.name, e, &mut data)?;

                            for m in mods {
                                current_csect.modifications.push(Modification {
                                    address: addr,
                                    length: 6,
                                    add: m.op == ExprOp::Add,
                                    symbol: m.extref,
                                })
                            }

                            0
                        }
                        Argument::ExprCurrentOffset => line.offset as i32,
                    };

                    current_csect.add_instruction(addr, Data::Word(arg as u32));
                }
                Assembler::RESW | Assembler::RESB | Assembler::END => {
                    if matches!(line.directive, Directive::Command(Assembler::END)) {
                        let start_label = line
                            .argument
                            .expect_string()
                            .with_context(|| format!("on line {}", line.line_no))?;
                        let start_label = data
                            .get_label(&first_csect, &start_label)?
                            .ok_or_else(|| anyhow!("couldn't find start label {start_label}"))?;
                        if control_sections.is_empty() {
                            current_csect.set_start_offset(start_label.offset as usize);
                        } else {
                            control_sections[0].set_start_offset(start_label.offset as usize);
                        }

                        let length = data.get_section_length(&current_csect.name)?;
                        if let Some(ltorg) = data.get_ltorg(line.block_id, line.offset)? {
                            current_csect.add_instruction(addr, Data::Byte(ltorg.data));
                        }
                        current_csect.set_length(length as usize);

                        if let Some(mut final_ltorg) = data.get_final_ltorg(0)? {
                            let offset = final_ltorg.offset as usize;
                            let literals = mem::take(&mut final_ltorg.data);
                            if !literals.is_empty() {
                                current_csect.add_instruction(offset, Data::Byte(literals));
                            }
                        }
                    }
                    if let Some(text) = current_csect.current_text.take() {
                        current_csect.records.push(Record::Text(text));
                    }
                }
            },
            Directive::Op(op) => match op {
                parser::Op::OneByte(opcode) => {
                    let op = Op::OneByte(opcode);
                    current_csect.add_instruction(addr, Data::Instruction(op));
                }
                parser::Op::OneReg(opcode) => {
                    let r1 = register(&line.argument.expect_string().context("for onereg")?)?;

                    let op = Op::OneReg(OneReg { opcode, r1 });
                    current_csect.add_instruction(addr, Data::Instruction(op));
                }
                parser::Op::TwoReg(opcode) => {
                    let args = line.argument.expect_list().context("for tworeg")?;
                    if args.len() != 2 {
                        bail!("expected 2 register list for tworeg");
                    }
                    let r1 = register(&args[0])?;
                    let r2 = register(&args[1])?;

                    let op = Op::TwoReg(TwoReg { opcode, r1, r2 });
                    current_csect.add_instruction(addr, Data::Instruction(op));
                }
                parser::Op::Shift(opcode) => {
                    let args = line.argument.expect_list().context("for shift")?;
                    if args.len() != 2 {
                        bail!("expected register and number for shift");
                    }
                    let r1 = register(&args[0])?;
                    let n = args[1].parse::<u8>()? - 1;

                    let op = Op::Shift(Shift { opcode, r1, n });
                    current_csect.add_instruction(addr, Data::Instruction(op));
                }
                parser::Op::Svc => {
                    let n = line.argument.expect_number().context("for svc")? as u8;

                    let op = Op::Svc(n);
                    current_csect.add_instruction(addr, Data::Instruction(op));
                }
                parser::Op::Variable(opcode) => {
                    let mode = line.address_modifier;
                    let is_rsub =
                        line.directive == Directive::Op(parser::Op::Variable(VariableOp::RSUB));
                    let (constant, target_offset, extref) = if is_rsub {
                        (true, 0, None)
                    } else {
                        let Some(ref argument) = line.argument else {
                            bail!(
                                "no argument for variable op {opcode} on line {}",
                                line.line_no
                            );
                        };

                        match argument {
                            Argument::Value(Value::Bytes(b) | Value::Chars(b)) => (
                                false,
                                data.get_literal(line.block_id, b)?
                                    .offset
                                    .ok_or_else(|| anyhow!("unaddressed literal in pass two"))?,
                                None,
                            ),
                            Argument::Value(Value::Number(i)) => (true, *i, None),
                            Argument::Value(Value::String(s)) => {
                                if let Some(label) = data.get_label(&current_csect.name, &s.0)? {
                                    let mut offset = label.offset;
                                    if !label.is_absolute {
                                        offset = block.calc_address(offset as usize) as i32;
                                    }
                                    (false, offset, None)
                                } else {
                                    // TODO: add modification record here
                                    let extref =
                                        data.get_extref(&current_csect.name, &s.0)?.ok_or_else(
                                            || anyhow!("couldn't find label or extref for {}", s.0),
                                        )?;
                                    (true, 0, Some(extref.symbol_name))
                                }
                            }
                            Argument::Expr(_) | Argument::ExprCurrentOffset => {
                                bail!("expr argument to variable op {opcode}")
                            }
                            Argument::Value(Value::List(_)) => {
                                bail!("list argument invalid for variable op {opcode}")
                            }
                        }
                    };

                    let pc = addr as i32 + 3;
                    let pc_disp = target_offset - pc;
                    let base_disp = current_csect
                        .base
                        .filter(|base| target_offset >= *base)
                        .map(|base| target_offset - base);

                    let (disp, relative_to) =
                        match (mode, constant, line.extended, pc_disp, base_disp) {
                            (_, true, _, _, _) | (_, _, true, _, _) => {
                                (target_offset, AddressRelativeTo::Direct)
                            }
                            (_, _, _, pcd, _) if MIN_PC < pcd && pcd < MAX_PC => {
                                (pc_disp, AddressRelativeTo::PC)
                            }
                            (_, _, _, _, Some(bd)) if bd < MAX_DISP as i32 => {
                                (bd, AddressRelativeTo::Base)
                            }
                            // This doens't work with relocatable code because
                            // target_offset + new program location could be
                            // too big to fit in the instruction.
                            _ if target_offset < MAX_DISP as i32
                                && current_csect.start_addr > 0 =>
                            {
                                (target_offset, AddressRelativeTo::Direct)
                            }
                            _ => bail!("couldn't find addressing mode for target"),
                        };

                    let mode = match mode {
                        AddressModifier::Unmodified => AddressMode::Simple,
                        AddressModifier::Indirect => AddressMode::Indirect,
                        AddressModifier::Immediate => AddressMode::Immediate,
                    };

                    if mode != AddressMode::Immediate
                        && relative_to == AddressRelativeTo::Direct
                        && !is_rsub
                    {
                        let length = if mode == AddressMode::Compatiblity {
                            4
                        } else if line.extended {
                            5
                        } else {
                            3
                        };

                        let op_address = addr + 1;

                        let symbol = if let Some(symbol) = extref {
                            symbol
                        } else {
                            first_csect.clone()
                        };

                        current_csect.modifications.push(Modification {
                            address: op_address,
                            length,
                            add: true,
                            symbol,
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
                    current_csect.add_instruction(addr, Data::Instruction(op));
                }
            },
        }
    }

    control_sections.push(current_csect);

    let records = control_sections
        .into_iter()
        .map(|csect| csect.build())
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();

    Ok((records, debug))
}

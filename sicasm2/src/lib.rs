use anyhow::{anyhow, bail, Result};
use models::{Line, Literal};

use crate::{
    data::AsmData,
    models::{Label, Ltorg},
    parser::{Argument, Assembler, Directive, ProgramLine, Value},
};

pub mod data;
pub mod models;
pub mod parser;
pub mod schema;

pub static MAX_DISP: u16 = 4095; // 0x0F_FF
pub static MAX_PC: i32 = 2047; // 0x07_FF
pub static MIN_PC: i32 = -2048; // 0x08_00

pub fn pass_one(program: &str) -> Result<()> {
    let parsed = parser::parse_program(program)?;
    if parsed.len() < 3 {
        bail!("expected at least a start, end, and one directive");
    }
    let mut lines = parsed.iter();
    let Some(mut start_line) = lines.next() else { bail!("Expected at least one directive"); };

    let start = loop {
        let ProgramLine::Assembly(ref start) = start_line.data else {
            start_line = lines.next().ok_or_else(|| anyhow!("Couldn't find first program line"))?;
            continue;
        };

        break start;
    };

    if !matches!(start.directive, Directive::Command(Assembler::START)) {
        bail!("Expected first directive to be START");
    }

    let mut data = AsmData::from_env()?;

    data.add_program_block("".into())?;

    let mut current_block = data.get_program_block("")?.expect("just added this block");

    for parsed_line in lines {
        let ProgramLine::Assembly(ref program_line) = parsed_line.data else { continue; };

        if program_line.directive == Directive::Command(Assembler::USE) {
            let Some(Argument::Value(Value::String(ref block_name))) = program_line.argument else { bail!("invalid use line"); };

            if data.get_program_block(&block_name.0)?.is_none() {
                data.add_program_block(block_name.0.clone())?;
            }

            current_block = data
                .get_program_block(&block_name.0)?
                .ok_or_else(|| anyhow!("couldn't find program block {}", block_name.0))?;
        }

        let offset = current_block.current_offset as usize;

        let size = if program_line.directive == Directive::Command(Assembler::LTORG) {
            let mut ltorg_offset = offset as i32;
            let mut bytes = Vec::new();

            let mut literals = data.get_unaddressed_literals(&current_block)?;

            for literal in literals.iter_mut() {
                literal.offset = Some(ltorg_offset);
                bytes.append(&mut literal.value.clone());
                ltorg_offset += literal.value.len() as i32;
            }

            let size = bytes.len();

            let ltorg = Ltorg {
                block_name: current_block.block_name.clone(),
                offset: offset as i32,
                data: bytes,
            };

            data.add_ltorg(&ltorg)?;
            data.update_literals(&literals)?;

            size
        } else {
            program_line.size()?
        };

        current_block.current_offset += size as i32;

        data.set_current_location(&current_block)?;

        let line = Line {
            block_name: current_block.block_name.clone(),
            line_no: parsed_line.line_no,
            directive: program_line.directive,
            argument: program_line.argument.clone(),
            address_modifier: program_line.address_modifier,
            extended: program_line.extended,
            indexed: program_line.indexed,
            size,
            offset,
            text: parsed_line.text.clone(),
        };

        data.add_line(&line)?;

        if let Some(ref label) = program_line.label {
            if program_line.directive == Directive::Command(Assembler::EQU) {
                todo!();
            } else {
                let label = Label {
                    offset: offset as i32,
                    block_name: current_block.block_name.clone(),
                    line_no: line.line_no as i32,
                    label_name: label.0.clone(),
                };

                data.add_label(&label)?;
            }
        }

        if let Some(Argument::Value(Value::Bytes(ref v) | Value::Chars(ref v))) =
            program_line.argument
        {
            let literal = Literal {
                block_name: current_block.block_name.clone(),
                offset: None,
                value: v.clone(),
            };

            data.add_literal(&literal)?;
        }
    }

    Ok(())
}

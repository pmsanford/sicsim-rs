use anyhow::{bail, Result};
use data::AsmData;
use parser::{Argument, Arguments, Assembler, Directive, Value};
use record::{Data, Record, Text};
use sicdbg::Sdb;

pub mod data;
pub mod models;
pub mod parser;
pub mod pass_one;
pub mod record;
pub mod schema;

fn pass_two(mut data: AsmData) -> Result<String> {
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
                Assembler::LTORG => todo!(),
                Assembler::ORG => todo!(),
                Assembler::EQU => todo!(),
                Assembler::WORD => todo!(),
                Assembler::USE => todo!(),
                Assembler::RESW => todo!(),
                Assembler::RESB => todo!(),
                Assembler::END => todo!(),
            },
            Directive::Op(op) => match op {
                parser::Op::OneByte(_) => todo!(),
                parser::Op::OneReg(_) => todo!(),
                parser::Op::TwoReg(_) => todo!(),
                parser::Op::Shift(_) => todo!(),
                parser::Op::Svc => todo!(),
                parser::Op::Variable(_) => todo!(),
            },
        }
    }

    Ok("".into())
}

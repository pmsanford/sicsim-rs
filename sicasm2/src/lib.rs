use anyhow::{bail, Result};
use data::AsmData;
use parser::{Argument, Arguments, Assembler, Directive, Value};
use record::Record;
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

    for line in lines.iter().skip(1) {
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

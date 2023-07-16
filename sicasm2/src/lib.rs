use anyhow::{anyhow, bail, Result};
use models::{Line, Literal, ProgramBlock};
use parser::Expr;

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

    let mut data = AsmData::new_from_env()?;

    data.add_program_block("".into())?;

    let line = Line {
        block_name: "".into(),
        line_no: start_line.line_no,
        directive: start.directive,
        argument: start.argument.clone(),
        address_modifier: start.address_modifier,
        extended: start.extended,
        indexed: start.indexed,
        size: start.size()?,
        offset: 0,
        text: start_line.text.clone(),
    };

    data.add_line(&line)?;

    let label = Label {
        block_name: "".into(),
        line_no: start_line.line_no as i32,
        label_name: start
            .label
            .as_ref()
            .ok_or_else(|| anyhow!("program has no name"))?
            .0
            .clone(),
        offset: 0,
    };

    data.add_label(&label)?;

    let mut current_block = data.get_program_block("")?.expect("just added this block");

    for parsed_line in lines {
        let ProgramLine::Assembly(ref program_line) = parsed_line.data else { continue; };

        if program_line.directive == Directive::Command(Assembler::USE) {
            let block_name = if let Some(Argument::Value(Value::String(ref block_name))) =
                program_line.argument
            {
                block_name.0.clone()
            } else {
                "".into()
            };

            if data.get_program_block(&block_name)?.is_none() {
                data.add_program_block(block_name.clone())?;
            }

            current_block = data
                .get_program_block(&block_name)?
                .ok_or_else(|| anyhow!("couldn't find program block {}", block_name))?;
        }

        let offset = current_block.current_offset as usize;

        let size = if program_line.directive == Directive::Command(Assembler::LTORG) {
            create_ltorg(&current_block, &mut data)?
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
            let offset = if program_line.directive == Directive::Command(Assembler::EQU) {
                let arg = program_line
                    .argument
                    .as_ref()
                    .ok_or_else(|| anyhow!("expected argument for equ"))?;
                println!("Evaluating arg {arg:#?}");
                match arg {
                    Argument::Value(v) => value_for_expr(v, &mut data)?,
                    Argument::Expr(e) => eval_expr(e, &mut data)?,
                }
            } else {
                offset as i32
            };

            let label = Label {
                offset,
                block_name: current_block.block_name.clone(),
                line_no: line.line_no as i32,
                label_name: label.0.clone(),
            };

            data.add_label(&label)?;
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

    create_ltorg(&current_block, &mut data)?;

    Ok(())
}

fn create_ltorg(current_block: &ProgramBlock, data: &mut AsmData) -> Result<usize> {
    let mut ltorg_offset = current_block.current_offset;
    let mut bytes = Vec::new();

    let mut literals = data.get_unaddressed_literals(current_block)?;

    if literals.is_empty() {
        return Ok(0);
    }

    for literal in literals.iter_mut() {
        literal.offset = Some(ltorg_offset);
        bytes.append(&mut literal.value.clone());
        ltorg_offset += literal.value.len() as i32;
    }

    let size = bytes.len();

    let ltorg = Ltorg {
        block_name: current_block.block_name.clone(),
        offset: current_block.current_offset,
        data: bytes,
    };

    data.add_ltorg(&ltorg)?;
    data.update_literals(&literals)?;

    Ok(size)
}

fn value_for_expr(value: &Value, data: &mut AsmData) -> Result<i32> {
    let v = match value {
        Value::Bytes(_) | Value::Chars(_) => bail!("cannot convert bytes/chars to i32"),
        Value::Number(n) => *n,
        Value::String(label) => {
            data.get_label(&label.0)?
                .ok_or_else(|| anyhow!("label {} doesn't exist", label.0))?
                .offset
        }
    };

    println!("Value for {:#?}: {v}", value);

    Ok(v)
}

fn eval_expr(expr: &Expr, data: &mut AsmData) -> Result<i32> {
    let lhs = value_for_expr(&expr.value, data)?;

    let rhs = match &expr.target {
        parser::ExprTarget::Argument(v) => value_for_expr(v, data)?,
        parser::ExprTarget::Expr(e) => eval_expr(e, data)?,
    };

    println!("Eval: {:#?}", expr);
    println!("\tlhs: {lhs} rhs: {rhs} op: {:?}", expr.op);

    let result = match expr.op {
        parser::ExprOp::Add => lhs + rhs,
        parser::ExprOp::Subtract => lhs - rhs,
        parser::ExprOp::Multiply => lhs * rhs,
        parser::ExprOp::Divide => lhs / rhs,
    };

    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::models::*;
    use crate::schema::*;
    use diesel::prelude::*;
    use std::env;
    use temp_dir::TempDir;

    use super::*;

    fn run_pass_one(program: &str) -> Result<AsmData> {
        let dir = TempDir::new()?;
        let db = dir.child("test.db");
        let db_path = db.to_str().unwrap();

        env::set_var("DATABASE_URL", db_path);

        pass_one(program)?;

        AsmData::from_env()
    }

    #[test]
    fn create_lines() -> Result<()> {
        // Comments after are offsets of the line
        let program = r#"
TST     START   100     . 0
LBL1    ADD     #5      . 0
LBL2    LDA     X'0F'   . 3
        J       LBL1    . 6
        RESB    2       . 9
        RESW    2       . 11
        END     TST     . 17
        "#
        .trim();

        let mut data = run_pass_one(program)?;

        let lines = data.get_lines()?;

        assert_eq!(lines.len(), 7);

        // Line offsets
        assert_eq!(lines[0].offset, 0);
        assert_eq!(lines[1].offset, 0);
        assert_eq!(lines[2].offset, 3);
        assert_eq!(lines[3].offset, 6);
        assert_eq!(lines[4].offset, 9);
        assert_eq!(lines[5].offset, 11);
        assert_eq!(lines[6].offset, 17);

        // Labels
        let start_label = data.get_label("TST")?.expect("start label");
        assert_eq!(start_label.offset, 0);

        let one = data.get_label("LBL1")?.expect("lbl1");
        assert_eq!(one.offset, 0);

        let two = data.get_label("LBL2")?.expect("lbl2");
        assert_eq!(two.offset, 3);

        let ltorg_list: Vec<Ltorg> = ltorgs::dsl::ltorgs.get_results(&mut data.conn)?;

        assert_eq!(ltorg_list.len(), 1);
        let ltorg = ltorg_list.into_iter().next().unwrap();

        assert_eq!(ltorg.offset, 17);
        assert_eq!(ltorg.data, vec![0x0F]);

        Ok(())
    }

    #[test]
    fn test_multi_ltorg() -> Result<()> {
        let program = r#"
TST     START   100
        LDA     X'0E0F'
        LTORG
        LDA     X'01'
        END     TST
        "#
        .trim();

        let mut data = run_pass_one(program)?;

        let ltorg_list: Vec<Ltorg> = ltorgs::dsl::ltorgs
            .order(ltorgs::dsl::offset.asc())
            .get_results(&mut data.conn)?;

        assert_eq!(ltorg_list.len(), 2);

        assert_eq!(ltorg_list[0].offset, 3);
        assert_eq!(ltorg_list[1].offset, 8);

        assert_eq!(ltorg_list[0].data, vec![0x0E, 0x0F]);
        assert_eq!(ltorg_list[1].data, vec![0x01]);

        Ok(())
    }

    #[test]
    fn test_multiblock() -> Result<()> {
        let program = r#"
TST     START   100
        ADD    #5
        USE     BLK2
        SUB    #10
        STA     RES
        USE
RES     RESW    1
        END     TST
        "#
        .trim();

        let mut data = run_pass_one(program)?;

        let lines = data.get_lines()?;

        // Block membership
        assert_eq!(lines[0].block_name, "");
        assert_eq!(lines[1].block_name, "");
        assert_eq!(lines[3].block_name, "BLK2");
        assert_eq!(lines[4].block_name, "BLK2");
        assert_eq!(lines[6].block_name, "");

        // Block offsets
        assert_eq!(lines[1].offset, 0);
        assert_eq!(lines[3].offset, 0);
        assert_eq!(lines[4].offset, 3);
        assert_eq!(lines[6].offset, 3);

        Ok(())
    }

    #[test]
    fn test_equ() -> Result<()> {
        let program = r#"
TST     START   100
LB1     EQU     5
LB2     EQU     LB1+5
LB3     EQU     LB2*LB1
        END     TST
        "#
        .trim();

        let mut data = run_pass_one(program)?;

        let lb1 = data.get_label("LB1")?.expect("lb1");
        assert_eq!(lb1.offset, 5);

        let lb2 = data.get_label("LB2")?.expect("lb2");
        assert_eq!(lb2.offset, 10);

        let lb3 = data.get_label("LB3")?.expect("lb3");
        assert_eq!(lb3.offset, 50);

        Ok(())
    }
}

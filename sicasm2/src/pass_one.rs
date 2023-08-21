use crate::models::{ControlSection, Line, Literal, ProgramBlock};
use crate::parser::{parse_program, AsmArg, AssemblyLine, Expr, ExprOp, ExprTarget, ParserLine};
use anyhow::{anyhow, bail, Context, Result};

use crate::{
    data::AsmData,
    models::{Label, Ltorg},
    parser::{Argument, Assembler, Directive, ProgramLine, Value},
};

pub fn pass_one(program: &str) -> Result<AsmData> {
    let parsed = parse_program(program).context("parsing")?;
    if parsed.len() < 3 {
        bail!("expected at least a start, end, and one directive");
    }

    let mut data = AsmData::new_from_env()?;

    let first_line = parsed
        .iter()
        .find(|l| matches!(l.data, ProgramLine::Assembly(_)))
        .expect("program line");

    let ProgramLine::Assembly(ref start_line) = first_line.data else { bail!("impossible"); };

    let start_label = start_line.label.as_ref().expect("program name").0.clone();

    let mut current_section = data.add_control_section(start_label)?;

    let mut current_block = data.add_program_block(
        current_section.section_name.clone(),
        current_section.section_name.clone(),
    )?;

    for parsed_line in parsed.iter() {
        let ProgramLine::Assembly(ref program_line) = parsed_line.data else { continue; };
        {
            (current_section, current_block) =
                handle_csect(program_line, current_section, current_block, &mut data)?;

            current_block = handle_use(program_line, &current_section, current_block, &mut data)?;

            let size = if program_line.directive == Directive::Command(Assembler::LTORG) {
                create_ltorg(&current_block, &mut data)?
            } else {
                program_line.size()?
            };

            let line = new_line(parsed_line, program_line, &current_block, size);

            data.add_line(&line)?;

            handle_label(program_line, &current_block, &mut data, parsed_line.line_no)?;

            handle_literal(program_line, &current_block, &mut data)?;

            current_block.current_offset += size as i32;

            data.set_current_location(&current_block)?;
            Ok::<(), anyhow::Error>(())
        }
        .with_context(|| format!("pass one, line {}", parsed_line.line_no))?
    }

    for block in data.get_program_blocks(&current_section.section_name)? {
        create_ltorg(&block, &mut data)?;
    }

    Ok(data)
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
        block_id: current_block.block_id,
        offset: current_block.current_offset,
        data: bytes,
    };

    data.add_ltorg(&ltorg)?;
    data.update_literals(&literals)?;

    Ok(size)
}

fn new_line(
    parsed_line: &ParserLine,
    program_line: &AssemblyLine,
    current_block: &ProgramBlock,
    size: usize,
) -> Line {
    Line {
        block_id: current_block.block_id,
        line_no: parsed_line.line_no,
        directive: program_line.directive,
        argument: program_line.argument.clone(),
        address_modifier: program_line.address_modifier,
        extended: program_line.extended,
        indexed: program_line.indexed,
        size,
        offset: current_block.current_offset as usize,
        text: parsed_line.text.clone(),
    }
}

fn handle_literal(
    program_line: &AssemblyLine,
    current_block: &ProgramBlock,
    data: &mut AsmData,
) -> Result<()> {
    if program_line.directive == Directive::Command(Assembler::BYTE) {
        return Ok(());
    }

    if let Some(Argument::Value(Value::Bytes(ref v) | Value::Chars(ref v))) = program_line.argument
    {
        if !data.literal_exists(current_block.block_id, v)? {
            let literal = Literal {
                block_id: current_block.block_id,
                offset: None,
                value: v.clone(),
            };

            data.add_literal(&literal)?;
        }
    }

    Ok(())
}

fn handle_label(
    program_line: &AssemblyLine,
    current_block: &ProgramBlock,
    data: &mut AsmData,
    line_no: usize,
) -> Result<()> {
    if let Some(ref label) = program_line.label {
        let offset = if program_line.directive == Directive::Command(Assembler::EQU) {
            let arg = program_line
                .argument
                .as_ref()
                .ok_or_else(|| anyhow!("expected argument for equ"))?;
            match arg {
                Argument::Value(v) => value_for_expr(v, data)?,
                Argument::Expr(e) => eval_expr(e, data)?,
                Argument::ExprCurrentOffset => current_block.current_offset,
            }
        } else {
            current_block.current_offset
        };

        let label = Label {
            offset,
            section_name: current_block.section_name.clone(),
            line_no: line_no as i32,
            label_name: label.0.clone(),
        };

        data.add_label(&label)?;
    }

    Ok(())
}

fn handle_csect(
    program_line: &AssemblyLine,
    current_section: ControlSection,
    current_block: ProgramBlock,
    data: &mut AsmData,
) -> Result<(ControlSection, ProgramBlock)> {
    if program_line.directive == Directive::Command(Assembler::CSECT) {
        // This currently assumes you can't "resume" control sections
        // like you can program blocks.
        for block in data.get_program_blocks(&current_section.section_name)? {
            create_ltorg(&block, data)?;
        }

        let section_name = program_line.argument.expect_string()?;

        let new_section = data.create_control_section(&section_name)?;
        let new_block = data.add_program_block(section_name, "".into())?;

        Ok((new_section, new_block))
    } else {
        Ok((current_section, current_block))
    }
}

fn handle_use(
    program_line: &AssemblyLine,
    current_section: &ControlSection,
    current_block: ProgramBlock,
    data: &mut AsmData,
) -> Result<ProgramBlock> {
    if program_line.directive == Directive::Command(Assembler::USE) {
        let block_name =
            if let Some(Argument::Value(Value::String(ref block_name))) = program_line.argument {
                block_name.0.clone()
            } else {
                "".into()
            };

        return Ok(
            if let Some(block) =
                data.get_program_block_by_name(&current_section.section_name, &block_name)?
            {
                block
            } else {
                data.add_program_block(current_section.section_name.clone(), block_name.clone())?
            },
        );
    }

    Ok(current_block)
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

    Ok(v)
}

fn eval_expr(expr: &Expr, data: &mut AsmData) -> Result<i32> {
    let lhs = value_for_expr(&expr.value, data)?;

    let rhs = match &expr.target {
        ExprTarget::Argument(v) => value_for_expr(v, data)?,
        ExprTarget::Expr(e) => eval_expr(e, data)?,
    };

    let result = match expr.op {
        ExprOp::Add => lhs + rhs,
        ExprOp::Subtract => lhs - rhs,
        ExprOp::Multiply => lhs * rhs,
        ExprOp::Divide => lhs / rhs,
    };

    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::models::*;
    use crate::schema::*;
    use diesel::prelude::*;

    use super::*;

    fn run_pass_one(program: &str) -> Result<AsmData> {
        pass_one(program)
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
        assert_eq!(lines[0].block_id, 0);
        assert_eq!(lines[1].block_id, 0);
        assert_eq!(lines[3].block_id, 1);
        assert_eq!(lines[4].block_id, 1);
        assert_eq!(lines[6].block_id, 0);

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

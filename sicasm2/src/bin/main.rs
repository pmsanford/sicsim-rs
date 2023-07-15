use anyhow::Result;
use diesel::{QueryDsl, RunQueryDsl};
use libsic::xe::op::VariableOp;
use sicasm2::{
    data::AsmData,
    models::Line,
    parser::{Argument, Directive, Op::Variable},
    pass_one,
};

fn main() -> Result<()> {
    dotenvy::dotenv()?;

    let program = r#"
DISP    START   100
        ADD     #5
DOOHOO  ADD     #10
        MUL     DOOHOO
LP      COMP    VAL
        JLT     LP
VAL     WORD    100
        END     DISP"#;

    pass_one(program)?;

    Ok(())
}

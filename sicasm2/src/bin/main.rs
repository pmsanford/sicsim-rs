use anyhow::Result;
use sicasm2::pass_one::pass_one;

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

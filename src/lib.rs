use anyhow::Result;
use pass_one::FirstPass;
use pass_two::pass_two;
use std::fmt::Write;

mod constants;
mod directive;
mod labels;
mod pass_one;
mod pass_two;
mod record;

pub fn assemble_program(program_text: &str) -> Result<String> {
    let lines = program_text.lines().collect::<Vec<_>>();
    let pass_one = FirstPass::parse_lines(&lines)?;

    let records = pass_two(pass_one)?;

    let mut assembled = String::new();

    for record in records {
        writeln!(&mut assembled, "{}", record)?;
    }

    Ok(assembled)
}

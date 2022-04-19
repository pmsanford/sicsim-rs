use anyhow::Result;
use pass_one::FirstPass;
use pass_two::pass_two;
use std::{env, fs::File};
use std::{fmt::Write, io::Read};

mod constants;
mod directive;
mod labels;
mod pass_one;
mod pass_two;
mod record;

fn assemble_program(program_text: &str) -> Result<String> {
    let lines = program_text.lines().collect::<Vec<_>>();
    let pass_one = FirstPass::parse_lines(&lines)?;

    let records = pass_two(&pass_one)?;

    let mut assembled = String::new();

    for record in records {
        writeln!(&mut assembled, "{}", record)?;
    }

    Ok(assembled)
}

fn main() -> Result<()> {
    let filename: String = env::args()
        .nth(1)
        .ok_or_else(|| anyhow::Error::msg("Need a filename"))?;
    let mut file = File::open(filename)?;
    let mut program_text = String::new();
    file.read_to_string(&mut program_text)?;

    let program = assemble_program(&program_text)?;
    print!("{}", program);

    Ok(())
}

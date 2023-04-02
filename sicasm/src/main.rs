use anyhow::Result;
use sicasm::assemble_program;
use std::io::Read;
use std::{env, fs::File};

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

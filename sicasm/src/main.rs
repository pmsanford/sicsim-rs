use anyhow::Result;
use sicasm::assemble_with_debug;
use std::io::Read;
use std::{
    env,
    fs::{self, File},
};

fn main() -> Result<()> {
    let filename: String = env::args()
        .nth(1)
        .ok_or_else(|| anyhow::Error::msg("Need an input filename"))?;
    let output_name: String = env::args()
        .nth(2)
        .ok_or_else(|| anyhow::Error::msg("Need an output filename"))?;
    let mut file = File::open(filename)?;
    let mut program_text = String::new();
    file.read_to_string(&mut program_text)?;

    let (program, debug) = assemble_with_debug(&program_text)?;

    fs::write(format!("{}.ebj", output_name), program)?;
    fs::write(format!("{}.sdb", output_name), debug.to_string())?;

    Ok(())
}

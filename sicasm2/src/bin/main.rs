use std::io::Write;
use std::{env, fs};

use anyhow::Result;
use sicasm2::assemble_program;
use sicasm2::record::Record;

fn main() -> Result<()> {
    dotenvy::dotenv()?;

    let filename: String = env::args()
        .nth(1)
        .ok_or_else(|| anyhow::Error::msg("Need an input filename"))?;
    let output_name: String = env::args()
        .nth(2)
        .ok_or_else(|| anyhow::Error::msg("Need an output filename"))?;

    let mut output = fs::File::create(format!("{}.ebj", output_name))?;
    let mut debug = fs::File::create(format!("{}.sdb", output_name))?;

    let (prog, sdb) = assemble_program(&fs::read_to_string(&filename)?)?;

    for (idx, record) in prog.into_iter().enumerate() {
        if matches!(record, Record::Header { .. }) && idx > 0 {
            writeln!(output)?;
        }
        writeln!(output, "{}", record)?;
    }

    write!(debug, "{}", sdb.to_string()?)?;

    Ok(())
}

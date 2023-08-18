use std::io::Write;
use std::{env, fs};

use anyhow::{Context, Result};
use sicasm2::{data::AsmData, pass_one::pass_one, pass_two};

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

    pass_one(&fs::read_to_string(&filename)?).with_context(|| "pass one")?;

    let data = AsmData::new_from_env()?;

    let (prog, sdb) = pass_two(data).with_context(|| "pass two")?;

    for record in prog {
        writeln!(output, "{}", record)?;
    }

    write!(debug, "{}", sdb.to_string()?)?;

    Ok(())
}

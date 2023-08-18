use std::{env, fs};

use anyhow::{Context, Result};
use sicasm2::{data::AsmData, pass_one::pass_one, pass_two};

fn main() -> Result<()> {
    dotenvy::dotenv()?;

    let input = fs::read_to_string(env::args().nth(1).expect("program path"))?;

    pass_one(&input).with_context(|| "pass one")?;

    let data = AsmData::new_from_env()?;

    let prog = pass_two(data).with_context(|| "pass two")?;

    print!("{}", prog);

    Ok(())
}

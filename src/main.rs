use anyhow::Result;
use pass_one::FirstPass;
use pass_two::pass_two;
use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
};

mod constants;
mod directive;
mod labels;
mod pass_one;
mod pass_two;
mod record;

fn main() -> Result<()> {
    let filename: String = env::args()
        .nth(1)
        .ok_or_else(|| anyhow::Error::msg("Need a filename"))?;
    let file = File::open(filename)?;

    let lines = BufReader::new(file)
        .lines()
        .collect::<Result<Vec<_>, _>>()?;
    let pass_one = FirstPass::parse_lines(&lines)?;

    let records = pass_two(&pass_one)?;

    for record in records {
        println!("{}", record);
    }

    Ok(())
}

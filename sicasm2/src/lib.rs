pub mod data;
pub mod models;
pub mod parser;
pub mod pass_one;
pub mod pass_two;
pub mod record;
pub mod schema;

use anyhow::{Context, Result};
use pass_one::pass_one;
use pass_two::pass_two;
use record::Record;
use sicdbg::Sdb;

pub fn assemble_program(program_text: &str) -> Result<(Vec<Record>, Sdb)> {
    let data = pass_one(program_text).with_context(|| "error during pass one")?;
    pass_two(data).with_context(|| "error during pass two")
}

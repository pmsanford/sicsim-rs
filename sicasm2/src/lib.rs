use anyhow::Result;
use std::collections::HashMap;

pub mod parser;

pub static MAX_DISP: u16 = 4095; // 0x0F_FF
pub static MAX_PC: i32 = 2047; // 0x07_FF
pub static MIN_PC: i32 = -2048; // 0x08_00

#[derive(Debug, Clone)]
pub struct Label {
    pub block: String,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct Labels {
    pub labels: HashMap<String, Label>,
}

impl Labels {
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
        }
    }

    pub fn add(&mut self, key: String, value: Label) {
        self.labels.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Result<Label> {
        self.labels
            .get(key)
            .cloned()
            .ok_or_else(|| anyhow::Error::msg(format!("Couldn't find label {}", key)))
    }

    pub fn absolute(&self, key: &str) -> Result<usize> {
        Ok(self.get(key)?.offset)
    }

    #[allow(dead_code)]
    pub fn relative(&self, blocks: &HashMap<String, usize>, key: &str) -> Result<usize> {
        let label = self.get(key)?;
        let start = blocks
            .get(&label.block)
            .ok_or_else(|| anyhow::Error::msg(format!("Couldn't find block {}", label.block)))?;

        Ok(label.offset + start)
    }
}

struct ProgramBlock {
    current_offset: usize,
}

struct PassOne {
    current_block: String,
    blocks: HashMap<String, ProgramBlock>,
}

struct ParsedLine {}

fn pass_one(program: &str) -> Result<Vec<ParsedLine>> {
    let parsed = parser::parse_program(program)?;
    todo!()
}

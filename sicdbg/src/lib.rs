use std::collections::HashMap;

use serde::{Deserialize, Serialize};
pub use serde_json::Error;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SdbLine {
    pub offset: u32,
    pub text: String,
    pub line_number: usize,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Sdb {
    pub name: String,
    pub start: usize,
    pub lines: Vec<SdbLine>,
    pub offset_map: HashMap<u32, usize>,
    pub labels: HashMap<String, u32>,
}

impl Sdb {
    pub fn new(name: &str, start: usize) -> Self {
        Sdb {
            name: name.into(),
            start,
            lines: Vec::new(),
            offset_map: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    pub fn to_string(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(self)
    }

    pub fn add_label(&mut self, label: String, offset: u32) {
        self.labels.insert(label, offset);
    }

    pub fn add_line(&mut self, offset: u32, text: String, line_number: usize) {
        self.lines.push(SdbLine {
            offset,
            text,
            line_number,
        });
        self.offset_map.insert(offset, self.lines.len() - 1);
    }

    pub fn from_string(dbg: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(dbg)
    }
}

use std::collections::HashMap;

use anyhow::Result;

#[derive(Debug, Clone)]
pub struct Label {
    pub block: String,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct Labels {
    labels: HashMap<String, Label>,
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

    pub fn relative(&self, blocks: &HashMap<String, usize>, key: &str) -> Result<usize> {
        let label = self.get(key)?;
        let start = blocks
            .get(&label.block)
            .ok_or_else(|| anyhow::Error::msg(format!("Couldn't find block {}", label.block)))?;

        Ok(label.offset + start)
    }
}

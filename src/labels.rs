use std::collections::HashMap;

use anyhow::Result;

#[derive(Debug, Clone)]
pub struct Labels {
    labels: HashMap<String, usize>,
}

impl Labels {
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
        }
    }

    pub fn add(&mut self, key: String, value: usize) {
        self.labels.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Result<usize> {
        self.labels
            .get(key)
            .copied()
            .ok_or_else(|| anyhow::Error::msg(format!("Couldn't find label {}", key)))
    }
}

use std::fmt::Display;

use libsic::xe::op::Op;

#[derive(Debug)]
pub enum Data {
    Instruction(Op),
    Byte(Vec<u8>),
    Word(u32),
}

impl Data {
    pub fn length(&self) -> usize {
        match self {
            Data::Instruction(op) => op.len() as usize,
            Data::Byte(b) => b.len(),
            Data::Word(_) => 3,
        }
    }
}

#[derive(Debug)]
pub struct Text {
    pub address: usize,
    pub instructions: Vec<Data>,
}

impl Text {
    pub fn new(address: usize) -> Self {
        Self {
            address,
            instructions: Vec::new(),
        }
    }
    pub fn len(&self) -> usize {
        self.instructions.iter().map(Data::length).sum()
    }
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
}

#[derive(Debug)]
pub struct Modification {
    pub address: usize,
    pub length: usize,
}

#[derive(Debug)]
pub enum Record {
    Header {
        name: String,
        start: usize,
        length: usize,
    },
    Text(Text),
    Modification(Modification),
    End {
        first_instruction: usize,
    },
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Record::Header {
                name,
                start,
                length,
            } => {
                write!(f, "H{:<6}{:0>6X}{:0>6X}", name, start, length)
            }
            Record::Text(text) => {
                write!(
                    f,
                    "T{:0>6X}{:0>2X}",
                    text.address,
                    text.instructions
                        .iter()
                        .map(|i| match i {
                            Data::Instruction(_) | Data::Word(_) => 3,
                            Data::Byte(bytes) => bytes.len(),
                        })
                        .sum::<usize>()
                )?;
                for instruction in &text.instructions {
                    match instruction {
                        Data::Instruction(instruction) => {
                            let binary = instruction.to_bytes();
                            for i in 0..instruction.len() {
                                write!(f, "{:0>2X}", binary[i as usize])?;
                            }
                        }
                        Data::Byte(bytes) => {
                            for byte in bytes {
                                write!(f, "{:0>2X}", byte)?;
                            }
                        }
                        Data::Word(word) => {
                            let [_, a, b, c] = word.to_be_bytes();
                            write!(f, "{:0>2X}{:0>2X}{:0>2X}", a, b, c)?;
                        }
                    }
                }

                Ok(())
            }
            Record::Modification(modification) => write!(
                f,
                "M{:0>6X}{:0>2X}",
                modification.address, modification.length
            ),
            Record::End { first_instruction } => write!(f, "E{:0>6X}", first_instruction),
        }
    }
}

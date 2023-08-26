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
    pub add: bool,
    pub symbol: String,
}

#[derive(Debug, Clone)]
pub struct ExtDef {
    pub name: String,
    pub offset: usize,
}

#[derive(Debug)]
pub struct Define {
    pub definitions: Vec<ExtDef>,
}

#[derive(Debug)]
pub struct Refer {
    pub references: Vec<String>,
}

#[derive(Debug)]
pub enum Record {
    Header {
        name: String,
        start: usize,
        length: usize,
    },
    Define(Define),
    Refer(Refer),
    Text(Text),
    Modification(Modification),
    End {
        first_instruction: Option<usize>,
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
            Record::Define(define) => {
                for w in define.definitions.chunks(6) {
                    write!(f, "D")?;
                    for d in w {
                        write!(f, "{: <6}{:0>6X}", d.name, d.offset)?;
                    }
                }

                Ok(())
            }
            Record::Refer(refer) => {
                for w in refer.references.chunks(6) {
                    write!(f, "R")?;
                    for r in w {
                        write!(f, "{: <6}", r)?;
                    }
                }

                Ok(())
            }
            Record::Text(text) => {
                write!(
                    f,
                    "T{:0>6X}{:0>2X}",
                    text.address,
                    text.instructions
                        .iter()
                        .map(|i| match i {
                            Data::Instruction(i) => match i {
                                Op::OneByte(_) => 1,
                                Op::OneReg(_) | Op::TwoReg(_) | Op::Shift(_) | Op::Svc(_) => 2,
                                Op::Variable(i) =>
                                    if i.address_flags.extended {
                                        4
                                    } else {
                                        3
                                    },
                            },
                            Data::Word(_) => 3,
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
                "M{:0>6X}{:0>2X}{}{}",
                modification.address,
                modification.length,
                if modification.add { "+" } else { "-" },
                modification.symbol
            ),
            Record::End { first_instruction } => {
                if let Some(first_instruction) = first_instruction {
                    write!(f, "E{:0>6X}", first_instruction)
                } else {
                    write!(f, "E")
                }
            }
        }
    }
}

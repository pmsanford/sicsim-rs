use anyhow::Result;
use libsic::xe::op::{
    AddressFlags, AddressMode, AddressRelativeTo, OneByteOp, OneReg, OneRegOp, Op, Register, Shift,
    ShiftOp, TwoReg, TwoRegOp, Variable, VariableOp,
};
use regex::Regex;
use std::{
    collections::HashMap,
    env,
    fmt::Display,
    fs::File,
    io::{BufRead, BufReader},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assembler {
    START,
    BASE,
    BYTE,
    WORD,
    RESW,
    RESB,
    END,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Directive {
    Assembler(Assembler),
    OneByte(OneByteOp),
    OneReg(OneRegOp),
    TwoReg(TwoRegOp),
    Shift(ShiftOp),
    SVC,
    Variable(VariableOp),
}

impl Directive {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "START" => Some(Self::Assembler(Assembler::START)),
            "BASE" => Some(Self::Assembler(Assembler::BASE)),
            "BYTE" => Some(Self::Assembler(Assembler::BYTE)),
            "WORD" => Some(Self::Assembler(Assembler::WORD)),
            "RESW" => Some(Self::Assembler(Assembler::RESW)),
            "RESB" => Some(Self::Assembler(Assembler::RESB)),
            "END" => Some(Self::Assembler(Assembler::END)),

            "FIX" => Some(Self::OneByte(OneByteOp::FIX)),
            "FLOAT" => Some(Self::OneByte(OneByteOp::FLOAT)),
            "HIO" => Some(Self::OneByte(OneByteOp::HIO)),
            "NORM" => Some(Self::OneByte(OneByteOp::NORM)),
            "SIO" => Some(Self::OneByte(OneByteOp::SIO)),
            "TIO" => Some(Self::OneByte(OneByteOp::TIO)),

            "CLEAR" => Some(Self::OneReg(OneRegOp::CLEAR)),
            "TIXR" => Some(Self::OneReg(OneRegOp::TIXR)),

            "ADDR" => Some(Self::TwoReg(TwoRegOp::ADDR)),
            "COMPR" => Some(Self::TwoReg(TwoRegOp::COMPR)),
            "DIVR" => Some(Self::TwoReg(TwoRegOp::DIVR)),
            "MULR" => Some(Self::TwoReg(TwoRegOp::MULR)),
            "RMO" => Some(Self::TwoReg(TwoRegOp::RMO)),
            "SUBR" => Some(Self::TwoReg(TwoRegOp::SUBR)),

            "SHIFTL" => Some(Self::Shift(ShiftOp::SHIFTL)),
            "SHIFTR" => Some(Self::Shift(ShiftOp::SHIFTR)),

            "ADD" => Some(Self::Variable(VariableOp::ADD)),
            "ADDF" => Some(Self::Variable(VariableOp::ADDF)),
            "AND" => Some(Self::Variable(VariableOp::AND)),
            "COMP" => Some(Self::Variable(VariableOp::COMP)),
            "COMPF" => Some(Self::Variable(VariableOp::COMPF)),
            "DIV" => Some(Self::Variable(VariableOp::DIV)),
            "DIVF" => Some(Self::Variable(VariableOp::DIVF)),
            "J" => Some(Self::Variable(VariableOp::J)),
            "JEQ" => Some(Self::Variable(VariableOp::JEQ)),
            "JGT" => Some(Self::Variable(VariableOp::JGT)),
            "JLT" => Some(Self::Variable(VariableOp::JLT)),
            "JSUB" => Some(Self::Variable(VariableOp::JSUB)),
            "LDA" => Some(Self::Variable(VariableOp::LDA)),
            "LDB" => Some(Self::Variable(VariableOp::LDB)),
            "LDCH" => Some(Self::Variable(VariableOp::LDCH)),
            "LDF" => Some(Self::Variable(VariableOp::LDF)),
            "LDL" => Some(Self::Variable(VariableOp::LDL)),
            "LDS" => Some(Self::Variable(VariableOp::LDS)),
            "LDT" => Some(Self::Variable(VariableOp::LDT)),
            "LDX" => Some(Self::Variable(VariableOp::LDX)),
            "LPS" => Some(Self::Variable(VariableOp::LPS)),
            "MUL" => Some(Self::Variable(VariableOp::MUL)),

            "MULF" => Some(Self::Variable(VariableOp::MULF)),
            "OR" => Some(Self::Variable(VariableOp::OR)),
            "RD" => Some(Self::Variable(VariableOp::RD)),
            "RSUB" => Some(Self::Variable(VariableOp::RSUB)),
            "SSK" => Some(Self::Variable(VariableOp::SSK)),
            "STA" => Some(Self::Variable(VariableOp::STA)),
            "STB" => Some(Self::Variable(VariableOp::STB)),
            "STCH" => Some(Self::Variable(VariableOp::STCH)),
            "STF" => Some(Self::Variable(VariableOp::STF)),
            "STI" => Some(Self::Variable(VariableOp::STI)),
            "STL" => Some(Self::Variable(VariableOp::STL)),
            "STS" => Some(Self::Variable(VariableOp::STS)),
            "STSW" => Some(Self::Variable(VariableOp::STSW)),
            "STT" => Some(Self::Variable(VariableOp::STT)),
            "STX" => Some(Self::Variable(VariableOp::STX)),
            "SUB" => Some(Self::Variable(VariableOp::SUB)),
            "SUBF" => Some(Self::Variable(VariableOp::SUBF)),

            "TD" => Some(Self::Variable(VariableOp::TD)),
            "TIX" => Some(Self::Variable(VariableOp::TIX)),
            "WD" => Some(Self::Variable(VariableOp::WD)),

            "SVC" => Some(Self::SVC),

            _ => None,
        }
    }
}

use once_cell::sync::OnceCell;

static OPCODES: OnceCell<HashMap<String, u8>> = OnceCell::new();
static REGISTERS: OnceCell<HashMap<String, Register>> = OnceCell::new();
static LINE_REGEX: OnceCell<Regex> = OnceCell::new();
//static LINE_REGEX_PATTERN: &str = r#"^(([^.\s]\S*)|\s)\s+(\S+)[^\n\S]+([^\s\n,]*)(,X)?[^\n]*"#;
static LINE_REGEX_PATTERN: &str =
    r#"^(?:(?P<label>[^.\s]\S*)|\s)\s+(?P<directive>\S+)(?:[^\n\S]+|$)(?P<argument>\S*)[^\n]*"#;

fn size(directive: &str, argument: Option<&String>) -> Result<usize> {
    if let Some(dir) = Directive::from_str(&directive.replace("+", "")) {
        Ok(match dir {
            Directive::Assembler(asm) => match asm {
                Assembler::START | Assembler::END | Assembler::BASE => 0,
                Assembler::BYTE => {
                    let argument = argument
                        .ok_or_else(|| anyhow::Error::msg("Byte directive requires argument"))?;
                    if argument.starts_with("C'") {
                        argument.len() - 3
                    } else if argument.starts_with("X'") {
                        (argument.len() - 3) / 2
                    } else {
                        return Err(anyhow::Error::msg(format!(
                            "Invalid byte directive {}",
                            argument
                        )));
                    }
                }
                Assembler::WORD => 3,
                Assembler::RESW => {
                    let argument = argument
                        .ok_or_else(|| anyhow::Error::msg("RESW directive requires argument"))?;
                    argument.parse::<usize>()? * 3
                }
                Assembler::RESB => {
                    let argument = argument
                        .ok_or_else(|| anyhow::Error::msg("RESB directive requires argument"))?;
                    argument.parse::<usize>()?
                }
            },
            Directive::OneByte(_) => 1,
            Directive::OneReg(_) => 2,
            Directive::TwoReg(_) => 2,
            Directive::Shift(_) => 2,
            Directive::SVC => 2,
            Directive::Variable(_) => {
                if directive.starts_with("+") {
                    4
                } else {
                    3
                }
            }
        })
    } else {
        Err(anyhow::Error::msg(format!(
            "Unknown directive {}",
            directive
        )))
    }
}

fn build_opcodes() -> HashMap<String, u8> {
    [
        ("ADD".to_owned(), 0x18),
        ("AND".to_owned(), 0x40),
        ("COMP".to_owned(), 0x28),
        ("DIV".to_owned(), 0x24),
        ("J".to_owned(), 0x3C),
        ("JEQ".to_owned(), 0x30),
        ("JGT".to_owned(), 0x34),
        ("JLT".to_owned(), 0x38),
        ("JSUB".to_owned(), 0x48),
        ("LDA".to_owned(), 0x00),
        ("LDCH".to_owned(), 0x50),
        ("LDL".to_owned(), 0x08),
        ("LDX".to_owned(), 0x04),
        ("MUL".to_owned(), 0x20),
        ("OR".to_owned(), 0x44),
        ("RD".to_owned(), 0xD8),
        ("RSUB".to_owned(), 0x4C),
        ("STA".to_owned(), 0x0C),
        ("STCH".to_owned(), 0x54),
        ("STL".to_owned(), 0x14),
        ("STSW".to_owned(), 0xE8),
        ("STX".to_owned(), 0x10),
        ("SUB".to_owned(), 0x1C),
        ("TD".to_owned(), 0xE0),
        ("TIX".to_owned(), 0x2C),
        ("WD".to_owned(), 0xDC),
    ]
    .into()
}

#[derive(Debug)]
struct ParsedLine {
    label: Option<String>,
    directive: Directive,
    argument: Option<String>,
    extended: bool,
    size: usize,
    offset: usize,
}

static MAX_DISP: u16 = 4095; // 0x0F_FF
static MAX_PC: u16 = 2047; // 0x07_FF
static MIN_PC: i16 = -2048; // 0x08_00

impl ParsedLine {
    fn target(&self, labels: &HashMap<String, usize>) -> Option<usize> {
        if let Some(mut argument) = self.argument.clone() {
            if argument.ends_with(",X") {
                argument = argument[..argument.len() - 2].to_owned();
            }
            if argument.starts_with("#")
                && Regex::new("[0-9][0-9]*").unwrap().is_match(&argument[1..])
            {
                return Some(argument[1..].parse().unwrap());
            }
            if argument.starts_with("#") || argument.starts_with("@") {
                argument = argument[1..].to_owned();
            }

            Some(*labels.get(&argument).unwrap())
        } else {
            None
        }
    }

    fn parse_flags(
        &self,
        base: Option<usize>,
        labels: &HashMap<String, usize>,
    ) -> (u32, AddressFlags) {
        let mode = match self.argument.as_ref().map(|a| a.chars().next()).flatten() {
            Some('#') => AddressMode::Immediate,
            Some('@') => AddressMode::Indirect,
            _ => AddressMode::Simple,
        };
        let indexed = self
            .argument
            .as_ref()
            .map(|a| a.ends_with(",X"))
            .unwrap_or(false);
        let target = self.target(labels);
        let target = if target.is_none() && self.directive == Directive::Variable(VariableOp::RSUB)
        {
            0
        } else if target.is_some() {
            target.unwrap()
        } else {
            panic!("No target found");
        };
        let pc = self.offset + 3;
        let (disp, relative_to) = if mode == AddressMode::Immediate {
            (target, AddressRelativeTo::Direct)
        } else if self.extended {
            // Extended
            (target, AddressRelativeTo::Direct)
        } else if (target as i32) - (pc as i32) < (MAX_PC as i32)
            && target as i32 - pc as i32 > MIN_PC as i32
        {
            // PC
            ((target as i32 - pc as i32) as usize, AddressRelativeTo::PC)
        } else if base
            .map(|base| target >= base && (target - base) < (MAX_DISP as usize))
            .unwrap_or(false)
        {
            // Base
            (target - base.unwrap(), AddressRelativeTo::Base)
        } else if target < MAX_DISP as usize {
            // Direct
            (target, AddressRelativeTo::Direct)
        } else {
            // Error
            panic!("Too big for simple addressing");
        };
        (
            disp as u32,
            AddressFlags {
                mode,
                relative_to,
                indexed,
                extended: self.extended,
            },
        )
    }
}

fn parse_line(line: &str, offset: usize) -> Result<Option<ParsedLine>> {
    LINE_REGEX
        .get()
        .unwrap()
        .captures(line)
        .map(|cap| {
            let raw_directive = cap
                .name("directive")
                .map(|m| m.as_str().to_owned())
                .unwrap();
            let argument = cap
                .name("argument")
                .filter(|m| m.as_str().len() > 0)
                .map(|m| m.as_str().to_owned());

            let directive = Directive::from_str(&raw_directive.replace("+", "")).unwrap();

            let size = size(&raw_directive, argument.as_ref())?;

            Ok(ParsedLine {
                label: cap.name("label").map(|m| m.as_str().to_owned()),
                directive,
                extended: raw_directive.starts_with("+"),
                argument,
                size,
                offset,
            })
        })
        .transpose()
}

#[derive(Debug)]
enum Data {
    Instruction(Op),
    Byte(Vec<u8>),
    Word(u32),
}

impl Data {
    fn len(&self) -> usize {
        match self {
            Data::Instruction(op) => op.len() as usize,
            Data::Byte(b) => b.len(),
            Data::Word(_) => 3,
        }
    }
}

#[derive(Debug)]
struct Text {
    address: usize,
    instructions: Vec<Data>,
}

impl Text {
    fn len(&self) -> usize {
        self.instructions.iter().map(|i| i.len()).sum()
    }
}

#[derive(Debug)]
struct Modification {
    address: usize,
    length: usize,
}

#[derive(Debug)]
enum Record {
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
                            Data::Instruction(_) => 3,
                            Data::Byte(bytes) => bytes.len(),
                            Data::Word(_) => 3,
                        })
                        .sum::<usize>()
                )?;
                for instruction in text.instructions.iter() {
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

fn main() -> Result<()> {
    OPCODES.set(build_opcodes()).unwrap();
    LINE_REGEX.set(Regex::new(LINE_REGEX_PATTERN)?).unwrap();
    REGISTERS
        .set(
            [
                ("A".to_owned(), Register::A),
                ("X".to_owned(), Register::X),
                ("L".to_owned(), Register::L),
                ("B".to_owned(), Register::B),
                ("S".to_owned(), Register::S),
                ("T".to_owned(), Register::T),
                ("F".to_owned(), Register::F),
                ("PC".to_owned(), Register::PC),
                ("SW".to_owned(), Register::SW),
            ]
            .into(),
        )
        .unwrap();
    let filename: String = env::args().skip(1).next().expect("Need asm filename");
    let file = File::open(filename).unwrap();

    let mut cur_loc = 0;
    let mut lines = vec![];
    let mut labels = HashMap::new();

    for line in BufReader::new(file).lines() {
        let parsed = parse_line(&line?, cur_loc)?;
        if let Some(parsed) = parsed {
            cur_loc += parsed.size;
            if let Some(label) = parsed.label.as_ref() {
                //TODO: Assumes only one START
                labels.insert(label.clone(), parsed.offset);
            }
            lines.push(parsed);
        }
    }

    let start = &lines[0];
    let end = &lines[lines.len() - 1];

    if start.directive != Directive::Assembler(Assembler::START) {
        return Err(anyhow::Error::msg("Expected start directive"));
    }
    if end.directive != Directive::Assembler(Assembler::END) {
        return Err(anyhow::Error::msg("Expected end directive"));
    }

    let mut records = vec![];
    let mut modifications = vec![];

    let name = start.label.as_ref().expect("Expected name").clone();
    let base = usize::from_str_radix(start.argument.as_ref().expect("Start argument"), 16).unwrap();
    let length = end.offset;

    records.push(Record::Header {
        name,
        start: base,
        length,
    });

    let mut i = 1;

    let mut cur_text = None;
    let mut cur_base = None;

    while i < lines.len() {
        let line = &lines[i];
        let text = cur_text.take();
        //println!("Line: {:?} \n\tcur: {:?}\n\tlrec: {:?}\n\n", line, text, records.last());
        match line.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START => {}
                Assembler::BASE => {
                    cur_base = Some(*labels.get(line.argument.as_ref().unwrap()).unwrap());
                    cur_text = text;
                }
                Assembler::BYTE => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + base,
                        instructions: vec![],
                    });
                    if let Some((t, v)) = line
                        .argument
                        .as_ref()
                        .ok_or_else(|| anyhow::Error::msg("Byte requires argument"))?
                        .split_once("'")
                    {
                        let mut bytes = match t {
                            "X" => {
                                let bytes = v[..v.len() - 1]
                                    .chars()
                                    .collect::<Vec<char>>()
                                    .chunks(2)
                                    .map(|c| c.iter().collect::<String>())
                                    .map(|s| u8::from_str_radix(&s, 16))
                                    .collect::<Result<Vec<_>, _>>()?;
                                bytes
                            }
                            "C" => {
                                let bytes = v[..v.len() - 1]
                                    .chars()
                                    .map(|c| c as u8)
                                    .collect::<Vec<_>>();
                                bytes
                            }
                            _ => return Err(anyhow::Error::msg("Invalid byte argument")),
                        };
                        while bytes.len() > 0 {
                            let space_remaining = 30 - text.len();
                            if space_remaining < bytes.len() {
                                let new_text: Vec<u8> = bytes.drain(..space_remaining).collect();
                                let new_bytes = new_text.len();
                                text.instructions.push(Data::Byte(new_text));
                                records.push(Record::Text(text));
                                text = Text {
                                    address: line.offset + base + new_bytes,
                                    instructions: vec![],
                                };
                            } else {
                                text.instructions.push(Data::Byte(bytes));
                                break;
                            }
                        }
                        cur_text = Some(text);
                    } else {
                        return Err(anyhow::Error::msg("Invalid byte argument"));
                    }
                }
                Assembler::WORD => {
                    let mut text = text.unwrap_or_else(|| Text {
                        address: line.offset + base,
                        instructions: vec![],
                    });
                    if text.len() > 27 {
                        records.push(Record::Text(text));
                        text = Text {
                            address: line.offset + base,
                            instructions: vec![],
                        };
                    }
                    let argument = line
                        .argument
                        .as_ref()
                        .ok_or_else(|| anyhow::Error::msg("Invalid word argument"))?;
                    text.instructions
                        .push(Data::Word(u32::from_str_radix(&argument, 10)?));
                    cur_text = Some(text);
                }
                Assembler::RESW | Assembler::RESB | Assembler::END => {
                    if let Some(text) = text {
                        records.push(Record::Text(text));
                    }
                }
            },
            Directive::OneByte(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });
                if text.len() == 30 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                let op = Op::OneByte(opcode);
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::OneReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                let r1 = *REGISTERS
                    .get()
                    .unwrap()
                    .get(line.argument.as_ref().unwrap())
                    .unwrap();

                let op = Op::OneReg(OneReg { opcode, r1 });
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::TwoReg(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }

                let (r1s, r2s) = line.argument.as_ref().unwrap().split_once(",").unwrap();
                let r1 = *REGISTERS.get().unwrap().get(r1s).unwrap();
                let r2 = *REGISTERS.get().unwrap().get(r2s).unwrap();

                let op = Op::TwoReg(TwoReg { opcode, r1, r2 });
                text.instructions.push(Data::Instruction(op));

                cur_text = Some(text);
            }
            Directive::Shift(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });

                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }

                let (r1s, ns) = line.argument.as_ref().unwrap().split_once(",").unwrap();
                let r1 = *REGISTERS.get().unwrap().get(r1s).unwrap();
                let n: u8 = ns.parse().unwrap();

                let op = Op::Shift(Shift { opcode, r1, n });
                text.instructions.push(Data::Instruction(op));

                cur_text = Some(text);
            }
            Directive::Variable(opcode) => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });

                let (address, address_flags) = line.parse_flags(cur_base, &labels);

                if address_flags.mode != AddressMode::Immediate
                    && address_flags.relative_to == AddressRelativeTo::Direct
                {
                    let length = if address_flags.mode == AddressMode::Compatiblity {
                        4
                    } else if address_flags.extended {
                        5
                    } else {
                        3
                    };

                    let address = line.offset + base + 1;

                    modifications.push(Modification { address, length });
                }

                let op = Op::Variable(Variable {
                    opcode,
                    address_flags,
                    address,
                });
                if text.len() + op.len() as usize > 30 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
            Directive::SVC => {
                let mut text = text.unwrap_or_else(|| Text {
                    address: line.offset + base,
                    instructions: vec![],
                });
                if text.len() > 28 {
                    records.push(Record::Text(text));
                    text = Text {
                        address: line.offset + base,
                        instructions: vec![],
                    };
                }
                let n = line.argument.as_ref().unwrap().parse().unwrap();

                let op = Op::Svc(n);
                text.instructions.push(Data::Instruction(op));
                cur_text = Some(text);
            }
        };
        i += 1;
    }

    for modification in modifications {
        records.push(Record::Modification(modification));
    }

    records.push(Record::End {
        first_instruction: *labels
            .get(end.argument.as_ref().expect("End argument"))
            .unwrap()
            + base,
    });

    for record in records {
        println!("{}", record);
    }

    Ok(())
}

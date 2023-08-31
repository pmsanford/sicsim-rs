use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read,
};

use crate::WordExt;

use super::vm::SicXeVm;

#[derive(Debug)]
pub struct Header {
    pub name: String,
    pub start_address: u32,
    pub length: u32,
}

impl Header {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let name = line[1..6].trim_end();
        let start_address = &line[7..13];
        let length = &line[13..19];

        Self {
            name: name.to_owned(),
            start_address: u32::from_str_radix(start_address, 16).unwrap(),
            length: u32::from_str_radix(length, 16).unwrap(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Definition {
    pub definitions: HashMap<String, u32>,
}

impl Definition {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let definitions = &line[1..];

        let definitions = definitions
            .chars()
            .collect::<Vec<_>>()
            .chunks(12)
            .map(|chunk| {
                let name = chunk[..6].iter().collect::<String>().trim().to_owned();
                let address = chunk[6..].iter().collect::<String>();
                (name, u32::from_str_radix(&address, 16).unwrap())
            })
            .collect::<HashMap<String, u32>>();

        Self { definitions }
    }
}

#[derive(Debug, Default)]
pub struct Reference {
    pub references: Vec<String>,
}

impl Reference {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let references = &line[1..];

        let references = references
            .chars()
            .collect::<Vec<_>>()
            .chunks(6)
            .map(|s| s.iter().collect::<String>().trim().to_owned())
            .collect::<Vec<_>>();

        Self { references }
    }
}

#[derive(Debug)]
pub struct Text {
    pub start_address: u32,
    pub data: Vec<u8>,
}

impl Text {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let start_address = &line[1..7];
        let _length = &line[7..9];

        let data = line[9..]
            .chars()
            .collect::<Vec<_>>()
            .chunks(2)
            .map(|byte| u8::from_str_radix(&byte.iter().collect::<String>(), 16))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        Self {
            start_address: u32::from_str_radix(start_address, 16).unwrap(),
            data,
        }
    }
}

#[derive(Debug)]
pub struct Modification {
    pub address: u32,
    pub length: u8,
    pub add: bool,
    pub symbol: String,
}

impl Modification {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let address = &line[1..7];
        let length = &line[7..9];
        let pm = &line[9..10];
        let add = pm == "+";
        let symbol = line[10..].to_owned();

        Self {
            address: u32::from_str_radix(address, 16).unwrap(),
            length: u8::from_str_radix(length, 16).unwrap(),
            add,
            symbol,
        }
    }
}

#[derive(Debug)]
pub struct End {
    pub first_address: Option<u32>,
}

impl End {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let first_address = if line.len() > 1 {
            let address = &line[1..7];
            Some(u32::from_str_radix(address, 16).unwrap())
        } else {
            None
        };

        Self { first_address }
    }
}

#[derive(Debug)]
pub struct Program {
    pub header: Header,
    pub definitions: Definition,
    pub references: Reference,
    pub text: Vec<Text>,
    pub modifications: Vec<Modification>,
    pub end: End,
}

pub fn load_program(program_text: &str) -> Program {
    let lines = program_text
        .split('\n')
        .filter(|l| !l.trim().is_empty())
        .collect::<Vec<_>>();
    let header = Header::from_record(lines[0]);
    let definitions = if let Some(line) = lines.iter().find(|l| l.starts_with('D')) {
        Definition::from_record(line)
    } else {
        Definition {
            definitions: HashMap::default(),
        }
    };
    let references = if let Some(line) = lines.iter().find(|l| l.starts_with('R')) {
        Reference::from_record(line)
    } else {
        Reference {
            references: Vec::default(),
        }
    };
    let mut text = vec![];
    let mut modifications = vec![];
    for line in lines[1..lines.len() - 1].iter() {
        if let Some(ty) = line.chars().next() {
            match ty {
                'T' => text.push(Text::from_record(line)),
                'M' => modifications.push(Modification::from_record(line)),
                _ => {}
            }
        }
    }
    let end = End::from_record(lines[lines.len() - 1]);

    Program {
        header,
        definitions,
        references,
        text,
        modifications,
        end,
    }
}

#[derive(Debug, Default)]
struct ProgramBuilder {
    header: Option<Header>,
    definitions: Definition,
    references: Option<Reference>,
    text: Vec<Text>,
    modifications: Vec<Modification>,
    end: Option<End>,
}

impl ProgramBuilder {
    fn new() -> Self {
        Self::default()
    }

    fn build(self) -> Program {
        let header = self.header.expect("header");
        let definitions = self.definitions;
        let references = self.references.unwrap_or_default();
        if self.text.is_empty() {
            panic!("expected some text");
        }
        let text = self.text;
        let modifications = self.modifications;
        let end = self.end.expect("end");

        Program {
            header,
            definitions,
            references,
            text,
            modifications,
            end,
        }
    }
}

fn build_program<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Program {
    let mut first = lines.next().expect("header");
    while first.is_empty() {
        first = lines.next().expect("header");
    }
    if !first.starts_with('H') {
        panic!("expected header line");
    }
    let mut builder = ProgramBuilder::new();
    let header = Header::from_record(first);
    let name = header.name.clone();
    builder.header = Some(header);
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        match line.chars().next() {
            Some('D') => {
                builder.definitions = Definition::from_record(line);
            }
            Some('R') => {
                builder.references = Some(Reference::from_record(line));
            }
            Some('T') => {
                builder.text.push(Text::from_record(line));
            }
            Some('M') => {
                builder.modifications.push(Modification::from_record(line));
            }
            Some('E') => {
                builder.end = Some(End::from_record(line));
                break;
            }
            _ => panic!("unrecognized line"),
        }
    }

    builder.definitions.definitions.insert(name, 0);

    builder.build()
}

#[derive(Debug, Default)]
pub struct ProgramLoader {
    programs: Vec<Program>,
    symbols: HashMap<String, u32>,
    references: HashSet<String>,
}

impl ProgramLoader {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn load_string(&mut self, programs: &str, start_address: u32) -> u32 {
        // This clobbers the configured program start address, if any
        // Returns the next available address
        let mut lines = programs.lines().peekable();
        let mut programs = vec![];
        while lines.peek().is_some() {
            programs.push(build_program(&mut lines));
        }

        let mut current_address = start_address;
        for program in programs.iter_mut() {
            let original_start = program.header.start_address;
            program.header.start_address = current_address;
            for (label, offset) in program.definitions.definitions.iter() {
                self.symbols.insert(label.clone(), offset + current_address);
            }
            program.references.references.iter().for_each(|r| {
                self.references.insert(r.clone());
            });
            for datum in program.text.iter_mut() {
                datum.start_address -= original_start;
            }
            current_address += program.header.length;
        }

        self.programs.append(&mut programs);

        current_address
    }

    pub fn copy_all_to(&self, vm: &mut SicXeVm) {
        let defined_symbols = self.symbols.keys().cloned().collect::<HashSet<_>>();
        let missing_definitions = self
            .references
            .difference(&defined_symbols)
            .collect::<HashSet<_>>();
        if !missing_definitions.is_empty() {
            panic!("Missing definitions: {:?}", missing_definitions);
        }

        for program in &self.programs {
            self.copy_program_to(&mut vm.memory, program);
        }
    }

    fn copy_program_to(&self, memory: &mut [u8], program: &Program) {
        let program_start = program.header.start_address as usize;
        for datum in &program.text {
            for (i, byte) in datum.data.iter().enumerate() {
                memory[datum.start_address as usize + program_start + i] = *byte;
            }
        }

        for m in program.modifications.iter() {
            let len = m.length;
            let mut addr = m.address as usize + program_start;
            let (cur, masks) = match len {
                5 => (
                    [memory[addr], memory[addr + 1], memory[addr + 2]].as_u32() & 0x00_0F_FF_FFu32,
                    [0xF0, 0x00, 0x00],
                ),
                4 => (
                    [memory[addr], memory[addr + 1], memory[addr + 2]].as_u32() & 0x00_00_FF_FF,
                    [0xFF, 0x00, 0x00],
                ),
                3 => (
                    [0, memory[addr], memory[addr + 1]].as_u32() & 0x00_00_0F_FF,
                    [0xFF, 0xF0, 0x00],
                ),
                _ => unreachable!(),
            };
            let offset = self.symbols[&m.symbol];
            let new = cur + offset;
            let [_, a, b, c] = new.to_be_bytes();
            if len > 3 {
                memory[addr] = (memory[addr] & masks[0]) + a;
                addr += 1;
            }
            memory[addr] = (memory[addr] & masks[1]) + b;
            memory[addr + 1] = (memory[addr + 1] & masks[2]) + c;
        }
    }
}

pub fn load_program_from(path: &str) -> Program {
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    load_program(&content)
}

pub fn copy_to_memory(memory: &mut [u8], program: &Program) -> u32 {
    if program.modifications.is_empty() && program.header.start_address != 0 {
        for datum in &program.text {
            for (i, byte) in datum.data.iter().enumerate() {
                memory[datum.start_address as usize + i] = *byte;
            }
        }
        program
            .end
            .first_address
            .unwrap_or(program.header.start_address)
    } else {
        copy_to_memory_at(memory, program, 2000);
        2000 + program.end.first_address.unwrap_or(0)
    }
}

// TODO: Fallible
pub fn copy_to_memory_at(memory: &mut [u8], program: &Program, at: u32) {
    let program_start = program.header.start_address as usize;
    for datum in &program.text {
        for (i, byte) in datum.data.iter().enumerate() {
            memory[datum.start_address as usize - program_start + i + at as usize] = *byte;
        }
    }
    for amod in &program.modifications {
        let len = amod.length;
        let mut addr = amod.address as usize + at as usize;
        let (cur, masks) = match len {
            5 => (
                [memory[addr], memory[addr + 1], memory[addr + 2]].as_u32() & 0x00_0F_FF_FFu32,
                [0xF0, 0x00, 0x00],
            ),
            4 => (
                [memory[addr], memory[addr + 1], memory[addr + 2]].as_u32() & 0x00_00_FF_FF,
                [0xFF, 0x00, 0x00],
            ),
            3 => (
                [0, memory[addr], memory[addr + 1]].as_u32() & 0x00_00_0F_FF,
                [0xFF, 0xF0, 0x00],
            ),
            _ => unreachable!(),
        };
        let new = cur + at;
        let [_, a, b, c] = new.to_be_bytes();
        if len > 3 {
            memory[addr] = (memory[addr] & masks[0]) + a;
            addr += 1;
        }
        memory[addr] = (memory[addr] & masks[1]) + b;
        memory[addr + 1] = (memory[addr + 1] & masks[2]) + c;
    }
}

pub fn vm_with_program_at(program_text: &str, at: u32) -> SicXeVm {
    let mut vm = SicXeVm::empty();
    let program = load_program(program_text);
    copy_to_memory_at(&mut vm.memory, &program, at);
    vm.set_pc(at);
    vm
}

pub fn vm_with_program(program_text: &str) -> SicXeVm {
    let mut vm = SicXeVm::empty();
    let program = load_program(program_text);
    let first_address = copy_to_memory(&mut vm.memory, &program);
    vm.set_pc(first_address);
    vm
}

pub fn load_program_to(vm: &mut SicXeVm, program_text: &str) {
    let program = load_program(program_text);
    copy_to_memory(&mut vm.memory, &program);
}

pub fn load_program_at(vm: &mut SicXeVm, program_text: &str, at: u32) {
    let program = load_program(program_text);
    copy_to_memory_at(&mut vm.memory, &program, at);
}

#[cfg(test)]
mod test {
    use super::ProgramLoader;
    use crate::xe::vm::SicXeVm;

    #[test]
    fn test_program_loader_mods() {
        let program = r#"
HCOPY  00000000000D
DOUTGOI00000ADONE  000007
RRESUL 
T0000000D0100050F1000003F2FFD000005
M00000405+RESUL
E000000

HSUBR  00000000000E
DRESUL 00000B
ROUTGOIDONE  
T0000000E2B100000331000003F2FFD000000
M00000105+OUTGOI
M00000505+DONE
E
            "#
        .trim();

        let mut loader = ProgramLoader::default();
        let start = 0x100;
        loader.load_string(program, start);

        let mut vm = SicXeVm::empty();

        loader.copy_all_to(&mut vm);

        let start = start as usize;

        let sta_resul = &vm.memory[start + 3..start + 7];
        assert_eq!(sta_resul, [0x0F, 0x10, 0x01, 0x18]);

        let comp_outgoi = &vm.memory[start + 0xD..start + 0x11];
        assert_eq!(comp_outgoi, [0x2B, 0x10, 0x01, 0x0A]);

        let jeq_done = &vm.memory[start + 0x11..start + 0x15];
        assert_eq!(jeq_done, [0x33, 0x10, 0x01, 0x07]);
    }
}

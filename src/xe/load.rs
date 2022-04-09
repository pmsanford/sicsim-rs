use std::{fs::File, io::Read};

use super::vm::SicXeVm;

pub struct Header {
    pub name: String,
    pub start_address: u16,
    pub length: u16,
}

impl Header {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let name = line[1..6].trim_end();
        let start_address = &line[7..13];
        let length = &line[13..19];

        Self {
            name: name.to_owned(),
            start_address: u16::from_str_radix(start_address, 16).unwrap(),
            length: u16::from_str_radix(length, 16).unwrap(),
        }
    }
}

pub struct Text {
    pub start_address: u16,
    pub data: Vec<u8>,
}

impl Text {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let start_address = &line[1..7];
        let _length = &line[7..9];
        //let data = Vec::with_capacity((line.len() - 10) / 2);
        let data = line[9..]
            .chars()
            .collect::<Vec<_>>()
            .chunks(2)
            .map(|byte| u8::from_str_radix(&byte.iter().collect::<String>(), 16))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        Self {
            start_address: u16::from_str_radix(start_address, 16).unwrap(),
            data,
        }
    }
}

pub struct End {
    pub first_address: u16,
}

impl End {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let first_address = &line[1..7];

        Self {
            first_address: u16::from_str_radix(first_address, 16).unwrap(),
        }
    }
}

pub struct Program {
    pub header: Header,
    pub text: Vec<Text>,
    pub end: End,
}

pub fn load_program(program_text: &str) -> Program {
    let lines = program_text
        .split('\n')
        .filter(|l| !l.trim().is_empty())
        .collect::<Vec<_>>();
    let header = Header::from_record(lines[0]);
    let text = lines[1..lines.len() - 1]
        .iter()
        .map(|line| Text::from_record(line))
        .collect::<Vec<_>>();
    let end = End::from_record(lines[lines.len() - 1]);

    Program { header, text, end }
}

pub fn load_program_from(path: &str) -> Program {
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    load_program(&content)
}

// TODO: Fallible
pub fn copy_to_memory(memory: &mut [u8], program: &Program) {
    for datum in &program.text {
        for (i, byte) in datum.data.iter().enumerate() {
            memory[datum.start_address as usize + i] = *byte;
        }
    }
}

pub fn init_with_program(path: &str) -> SicXeVm {
    let mut vm = SicXeVm::empty();

    let program = load_program(path);

    copy_to_memory(&mut vm.memory, &program);

    vm.set_pc(program.end.first_address);

    vm
}

pub fn vm_with_program(program_text: &str) -> SicXeVm {
    let mut vm = SicXeVm::empty();
    let program = load_program(program_text);
    copy_to_memory(&mut vm.memory, &program);
    vm.set_pc(program.end.first_address);
    vm
}

pub fn load_program_to(vm: &mut SicXeVm, program_text: &str) {
    let program = load_program(program_text);
    copy_to_memory(&mut vm.memory, &program);
}

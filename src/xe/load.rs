use std::{fs::File, io::Read};

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
        //let data = Vec::with_capacity((line.len() - 10) / 2);
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
}

impl Modification {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let address = &line[1..7];
        let length = &line[7..9];

        Self {
            address: u32::from_str_radix(address, 16).unwrap(),
            length: u8::from_str_radix(length, 16).unwrap(),
        }
    }
}

#[derive(Debug)]
pub struct End {
    pub first_address: u32,
}

impl End {
    fn from_record(line: &str) -> Self {
        let _t = &line[..1];
        let first_address = &line[1..7];

        Self {
            first_address: u32::from_str_radix(first_address, 16).unwrap(),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub header: Header,
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
    let mut text = vec![];
    let mut modifications = vec![];
    for line in lines[1..lines.len() - 1].iter() {
        if let Some(ty) = line.chars().next() {
            match ty {
                'T' => text.push(Text::from_record(line)),
                'M' => modifications.push(Modification::from_record(line)),
                _ => unreachable!(),
            }
        }
    }
    let end = End::from_record(lines[lines.len() - 1]);

    Program {
        header,
        text,
        modifications,
        end,
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
        program.end.first_address
    } else {
        copy_to_memory_at(memory, program, 2000);
        2000 + program.end.first_address
    }
}

// TODO: Fallible
pub fn copy_to_memory_at(memory: &mut [u8], program: &Program, at: u32) {
    for datum in &program.text {
        for (i, byte) in datum.data.iter().enumerate() {
            println!(
                "Loading data {:0>2X} at {}",
                *byte,
                datum.start_address as usize + i + at as usize
            );
            memory[datum.start_address as usize + i + at as usize] = *byte;
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

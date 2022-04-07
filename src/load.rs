use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use crate::vm::Vm;

struct Header {
    name: String,
    start_address: u16,
    length: u16,
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

struct Text {
    start_address: u16,
    data: Vec<u8>,
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

struct End {
    first_address: u16,
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

pub fn load_program(path: &str) -> Vm {
    let file = BufReader::new(File::open(path).unwrap());
    let lines = file.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let _header = Header::from_record(&lines[0]);
    let data = lines[1..lines.len() - 1]
        .iter()
        .map(|line| Text::from_record(&line))
        .collect::<Vec<_>>();
    let end = End::from_record(&lines[lines.len() - 1]);

    let mut vm = Vm::empty();

    for datum in data {
        for (i, byte) in datum.data.iter().enumerate() {
            vm.memory[datum.start_address as usize + i] = *byte;
        }
    }

    vm.set_pc(end.first_address);

    vm
}

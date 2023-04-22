use std::{env, fs::File, io::Read};

use libsic::xe::load::vm_with_program;

fn main() {
    let filename = env::args().nth(1).unwrap();
    let mut program = String::new();
    File::open(filename)
        .unwrap()
        .read_to_string(&mut program)
        .unwrap();
    let mut vm = vm_with_program(&program);
    let stop = vm.run_until(100);
    println!("{:?}", stop);
    println!("A: {:?} S: {:?} T: {:?} SW: {:?}", vm.A, vm.S, vm.T, vm.SW);
}

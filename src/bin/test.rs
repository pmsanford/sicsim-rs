use libsic::{vm::Vm, WordExt};

fn main() {
    let mut vm = Vm::with_program(include_str!("../../programs/add.ebj"));

    let mut prev = 0;
    while vm.PC.as_u32() != prev {
        vm.step();
        prev = vm.PC.as_u32();
    }

    println!("{:#?}", vm);
}

use libsic::{load_program, WordExt};

fn main() {
    let mut vm = load_program("programs/test.ebj");

    while vm.PC.as_u32() != 4096 {
        vm.step();
    }

    /*
    vm.step();
    vm.step();
    vm.step();

    vm.step();
    vm.step();
    vm.step();

    vm.step();
    vm.step();
    vm.step();

    vm.step();
    vm.step();
    vm.step();

    vm.step();
    vm.step();
    vm.step();
    */
    println!("{:#?}", vm);
}

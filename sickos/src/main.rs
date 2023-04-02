use libsic::{xe::{vm::{SicXeVm, PrintlnDebugger}, load::{load_program_to, load_program_at}}, WordExt};

fn main() {
    let mut vm = SicXeVm::empty();

    vm.debugger = Some(Box::new(PrintlnDebugger));

    let work_areas = include_str!("bin/work_areas.ebj");
    load_program_to(&mut vm, work_areas);

    println!("Word at 0x130: {}", [vm.memory[0x130], vm.memory[0x131], vm.memory[0x132]].as_u32());

    let program_int = include_str!("bin/program_int.ebj");
    load_program_to(&mut vm, program_int);

    let dispatcher = include_str!("bin/dispatcher.ebj");
    load_program_to(&mut vm, dispatcher);

    let iterator = include_str!("bin/iterator.ebj");
    load_program_at(&mut vm, iterator, 2000);

    vm.I = [0, 0, 10];
    vm.set_pc(2000);

    println!("Stopped: {:?}", vm.run_until(100000));

    println!("Final state: {:#?}", vm);
    println!("A: {}", vm.A.as_u32());

    println!("Word at 0x05F: {}", [vm.memory[0x060], vm.memory[0x061], vm.memory[0x062]].as_u32());
}

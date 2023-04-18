use libsic::{
    xe::{
        load::{load_program_at, load_program_to},
        vm::{PrintlnDebugger, SicXeVm},
    },
    WordExt,
};

fn main() {
    let mut vm = SicXeVm::empty();

    vm.debugger = Some(Box::new(PrintlnDebugger));

    let work_areas = include_str!("bin/work_areas.ebj");
    load_program_to(&mut vm, work_areas);

    println!(
        "Word at 0x130: {}",
        [vm.memory[0x130], vm.memory[0x131], vm.memory[0x132]].as_u32()
    );

    let program_int = include_str!("bin/program_int.ebj");
    load_program_to(&mut vm, program_int);

    let dispatcher = include_str!("bin/dispatcher.ebj");
    load_program_to(&mut vm, dispatcher);

    let psbs = include_str!("bin/psbs.ebj");
    load_program_to(&mut vm, psbs);

    let iterator = include_str!("bin/iterator.ebj");
    load_program_at(&mut vm, iterator, 2000);

    let multiplier = include_str!("bin/multiplier.ebj");
    load_program_at(&mut vm, multiplier, 3000);

    // Set interrupt timer at 10 sec
    vm.I = [0, 0, 10];
    // Start in dispatcher
    vm.set_pc(80);
    // Need to start in privileged mode
    vm.SW[0] |= 0x80;

    println!("Byte at: {:#08x}", [vm.memory[521], vm.memory[522], vm.memory[523]].as_u32());
    println!("[0, 2, 9] is {}", [0, 2, 9].as_u32());

    println!("Stopped: {:?}", vm.run_until(100000));

    println!("Pointer: {:#08x}", [vm.memory[99], vm.memory[100], vm.memory[101]].as_u32());

    println!("A: {}", vm.A.as_u32());

    println!("Word at 0x20C: {}", vm.word_at(0x20C).unwrap().as_u32());
}

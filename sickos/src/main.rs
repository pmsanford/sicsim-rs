use libsic::{
    word::{u32_to_word, DWordExt},
    xe::{
        load::{load_program_at, load_program_to},
        op::AddressFlags,
        vm::{PrintlnDebugger, SicXeVm},
    },
    WordExt,
};

fn print_word_at(vm: &mut SicXeVm, caption: &str, address: u32) {
    println!(
        "{}: Byte at {:#08x} ({:#08}): {:#08x}",
        caption,
        address,
        address,
        vm.word_at(address).unwrap().as_u32()
    );
}

fn read_ps(vm: &mut SicXeVm, address: u32) {
    println!("SW: {:#08x}", vm.word_at(address).unwrap().as_u32());
    println!("PC: {:#08x}", vm.word_at(address + 3).unwrap().as_u32());
    println!("A: {:#08x}", vm.word_at(address + 6).unwrap().as_u32());
    println!("X: {:#08x}", vm.word_at(address + 9).unwrap().as_u32());
    println!("L: {:#08x}", vm.word_at(address + 12).unwrap().as_u32());
    println!("B: {:#08x}", vm.word_at(address + 15).unwrap().as_u32());
    println!("S: {:#08x}", vm.word_at(address + 18).unwrap().as_u32());
    println!("T: {:#08x}", vm.word_at(address + 21).unwrap().as_u32());
    println!("F: {}", vm.dword_at(address + 24).unwrap().as_f64());
}

const PSB_ONE_STATUS: u32 = 0x206;
const PSB_TWO_STATUS: u32 = 0x227;
const ITERATOR_COUNTER: u32 = 0x7E5;
const MULTIPLIER_COUNTER: u32 = 0xBD6;
const RUNNING_PTR: u32 = 0x203;

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
    print_word_at(&mut vm, "iterator status", PSB_ONE_STATUS);
    print_word_at(&mut vm, "multiplier status", PSB_TWO_STATUS);
    print_word_at(&mut vm, "iterator sw", PSB_ONE_STATUS + 3);
    print_word_at(&mut vm, "multiplier sw", PSB_TWO_STATUS + 3);
    print_word_at(&mut vm, "iterator counter", ITERATOR_COUNTER);
    print_word_at(&mut vm, "multipler counter", MULTIPLIER_COUNTER);

    // Set interrupt timer at 10 sec
    vm.I = [0, 0, 10];
    // Start in dispatcher
    vm.set_pc(80);
    // Set pointer to running program
    vm.set_at(
        RUNNING_PTR,
        &AddressFlags::default(),
        u32_to_word(PSB_ONE_STATUS),
    )
    .unwrap();
    // Need to start in privileged mode
    vm.SW[0] |= 0x80;

    println!("[0, 2, 9] is {}", [0, 2, 9].as_u32());

    println!("Stopped: {:?}", vm.run_until(100000));

    println!("X: {}", vm.X.as_u32());
    print_word_at(&mut vm, "iterator status", PSB_ONE_STATUS);
    print_word_at(&mut vm, "multiplier status", PSB_TWO_STATUS);
    print_word_at(&mut vm, "iterator counter", ITERATOR_COUNTER);
    print_word_at(&mut vm, "multipler counter", MULTIPLIER_COUNTER);

    println!("Iterator:");
    read_ps(&mut vm, PSB_ONE_STATUS + 3);
    println!("Multiplier:");
    read_ps(&mut vm, PSB_TWO_STATUS + 3);
}

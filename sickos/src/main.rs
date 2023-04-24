use std::{fs, path::PathBuf};

use libsic::{
    word::{u32_to_word, DWordExt},
    xe::{debugger::SdbDebugger, load::load_program_at, op::AddressFlags, vm::SicXeVm},
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
    println!("F: {:#08x}", vm.dword_at(address + 24).unwrap().as_u64());
}

const RUNNING_PTR: u32 = 0x30A;
const PSB_BASE: u32 = RUNNING_PTR + 3;

fn psb_idx(idx: u32) -> u32 {
    PSB_BASE + (33 * idx)
}

fn load_program(vm: &mut SicXeVm, debugger: &mut SdbDebugger, name: &str, load_at: u32) {
    let mut path = PathBuf::from("./src/bin/");
    path.push(name);
    let program = fs::read_to_string(path.with_extension("ebj")).unwrap();
    let program_sdb = fs::read_to_string(path.with_extension("sdb")).unwrap();
    load_program_at(vm, &program, load_at);
    debugger.load(load_at, program_sdb).unwrap();
}

fn main() {
    let mut vm = SicXeVm::empty();
    let mut debugger = SdbDebugger::new();

    load_program(&mut vm, &mut debugger, "bootloader", 0x0);

    load_program(&mut vm, &mut debugger, "work_areas", 0x100);

    load_program(&mut vm, &mut debugger, "program_int", 0x30);

    load_program(&mut vm, &mut debugger, "dispatcher", 0x200);

    load_program(&mut vm, &mut debugger, "wake_counter", 0x7D0);

    load_program(&mut vm, &mut debugger, "wake_counter", 0x7E0);

    load_program(&mut vm, &mut debugger, "wake_counter", 0x7F0);

    vm.debugger = Some(Box::new(debugger.clone()));

    // Set interrupt timer at 10 sec
    vm.I = [0, 0, 10];
    // Start in dispatcher
    vm.set_pc(0);
    // Set pointer to running program
    vm.set_at(
        RUNNING_PTR,
        &AddressFlags::default(),
        u32_to_word(psb_idx(0)),
    )
    .unwrap();
    // Need to start in privileged mode
    vm.SW[0] |= 0x80;

    println!("[0, 2, 9] is {}", [0, 2, 9].as_u32());

    println!("Stopped: {:?}", vm.run_until(100000));

    print_word_at(&mut vm, "Running pointer", RUNNING_PTR);
    print_word_at(
        &mut vm,
        "First status block",
        debugger.address_for_label("DISP", "FPSB").unwrap(),
    );

    read_ps(&mut vm, psb_idx(0) + 3);
    print_word_at(
        &mut vm,
        "First counter",
        debugger.address_for_label("WCTR", "COUNTER").unwrap(),
    );
    read_ps(&mut vm, psb_idx(1) + 3);
    print_word_at(
        &mut vm,
        "Second counter",
        debugger.address_for_label("WCTR_1", "COUNTER").unwrap(),
    );
    read_ps(&mut vm, psb_idx(2) + 3);
    print_word_at(
        &mut vm,
        "Third status block",
        debugger.address_for_label("WCTR_2", "COUNTER").unwrap(),
    );
}

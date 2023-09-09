use std::{fs, path::PathBuf};

use libsic::{
    word::DWordExt,
    xe::{debugger::SdbConsoleDebugger, load::ProgramLoader, vm::SicXeVm},
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

fn load_program(
    debugger: &mut SdbConsoleDebugger,
    loader: &mut ProgramLoader,
    name: &str,
    load_at: u32,
) {
    let mut path = PathBuf::from("./src/bin/");
    path.push(name);
    let program = fs::read_to_string(path.with_extension("ebj")).unwrap();
    let program_sdb = fs::read_to_string(path.with_extension("sdb")).unwrap();
    loader.load_string(&program, load_at);
    debugger.load(load_at, program_sdb).unwrap();
}

fn main() {
    let mut vm = SicXeVm::empty();
    let mut debugger = SdbConsoleDebugger::new();
    let mut loader = ProgramLoader::new();

    load_program(&mut debugger, &mut loader, "bootloader", 0x0);

    load_program(&mut debugger, &mut loader, "work_areas", 0x100);

    load_program(&mut debugger, &mut loader, "program_int", 0x30);

    load_program(&mut debugger, &mut loader, "dispatcher", 0x200);

    load_program(&mut debugger, &mut loader, "wake_counter", 0x7D0);

    load_program(&mut debugger, &mut loader, "wake_counter", 0x7E0);

    load_program(&mut debugger, &mut loader, "wake_counter", 0x7F0);

    loader.copy_all_to(&mut vm);

    vm.debugger = Some(Box::new(debugger.clone()));

    // Set interrupt timer at 10 sec
    vm.I = [0, 0, 10];
    // Start in dispatcher
    vm.set_pc(0);
    // Need to start in privileged mode
    vm.SW[0] |= 0x80;

    println!("[0, 2, 9] is {}", [0, 2, 9].as_u32());

    println!("Stopped: {:?}", vm.run_until(100000));

    print_word_at(
        &mut vm,
        "Running pointer",
        debugger.address_for_label("DISP", "RPTR").unwrap(),
    );
    print_word_at(
        &mut vm,
        "First status block",
        debugger.address_for_label("DISP", "FPSB").unwrap(),
    );

    read_ps(&mut vm, debugger.address_for_label("DISP", "FSW").unwrap());
    print_word_at(
        &mut vm,
        "First counter",
        debugger.address_for_label("WCTR", "COUNTER").unwrap(),
    );
    read_ps(&mut vm, debugger.address_for_label("DISP", "SSW").unwrap());
    print_word_at(
        &mut vm,
        "Second counter",
        debugger.address_for_label("WCTR_1", "COUNTER").unwrap(),
    );
    read_ps(&mut vm, debugger.address_for_label("DISP", "TSW").unwrap());
    print_word_at(
        &mut vm,
        "Third status block",
        debugger.address_for_label("WCTR_2", "COUNTER").unwrap(),
    );
}

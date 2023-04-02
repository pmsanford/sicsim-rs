use libsic::{
    device::{FileInputDevice, MemoryOutputDevice},
    word::u32_to_word,
    xe::vm::StopReason,
    xe::{
        load::{load_program_at, load_program_to, vm_with_program, vm_with_program_at},
        op::AddressFlags,
        vm::PrintlnDebugger,
    },
    WordExt,
};

#[test]
fn test_interrupt() {
    let work_areas = include_str!("../programs/xe/work_areas.ebj");
    let interrupt_handler = include_str!("../programs/xe/test_inter.ebj");
    let test_program = include_str!("../programs/xe/call_svc.ebj");
    let mut vm = vm_with_program(test_program);
    vm.debugger = Some(Box::new(PrintlnDebugger));
    load_program_at(&mut vm, interrupt_handler, 1000);
    load_program_to(&mut vm, work_areas);
    vm.set_at(0x103, &AddressFlags::immediate(), u32_to_word(1000))
        .unwrap();
    let (output_buffer, output_device) = MemoryOutputDevice::new();
    vm.add_device(Box::new(output_device), 0x01);

    assert_eq!(vm.run_until(1000), StopReason::Halted);
    let expected = "Read".as_bytes();
    assert_eq!(*output_buffer.borrow_mut(), expected);
    assert_eq!(vm.A.as_u32(), 1234);
    assert_eq!(vm.S.as_u32(), 1);
    assert_eq!(vm.T.as_u32(), 10);
    assert_eq!(vm.X.as_u32(), 0);
}

#[test]
fn test_oor() {
    let interrupt_handler = include_str!("../programs/xe/prog_int.ebj");
    let test_program = include_str!("../programs/xe/out_of_range.ebj");
    let start_pc = (1 << 20) - 10;
    let mut vm = vm_with_program_at(test_program, start_pc);
    load_program_at(&mut vm, interrupt_handler, 1000);
    vm.set_at(0x133, &AddressFlags::immediate(), u32_to_word(1000))
        .unwrap();
    println!(
        "start_pc: {} memory: {:0>2X} {:0>2X} {:0>2X}",
        start_pc,
        &vm.memory[start_pc as usize],
        &vm.memory[start_pc as usize + 1],
        &vm.memory[start_pc as usize + 2],
    );
    let stop = vm.run_until(1000);
    assert_eq!(stop, StopReason::Halted);
    assert_eq!(vm.A.as_u32(), 1048569);
    assert_eq!(vm.PC.as_u32(), 1004);
}

#[test]
fn test_relocation() {
    let test_program = include_str!("../programs/xe/relo.ebj");
    let mut vm = vm_with_program(test_program);
    assert_eq!(vm.A.as_u32(), 0);
    assert_eq!(vm.run_until(1000), StopReason::Halted);
    assert_eq!(vm.A.as_u32(), 15);
    assert_eq!(vm.T.as_u32(), 2000);
}

#[test]
fn test_simple_add() {
    let test_program = include_str!("../programs/xe/add.ebj");
    let mut vm = vm_with_program(test_program);
    assert_eq!(vm.A.as_u32(), 0);
    assert_eq!(vm.run_until(1000), StopReason::Halted);
    assert_eq!(vm.A.as_u32(), 5);
}

#[test]
fn test_devices() {
    let test_program = include_str!("../programs/xe/copy.ebj");
    let test_harness = include_str!("../programs/xe/test_harness.ebj");
    let mut vm = vm_with_program(test_harness);
    load_program_to(&mut vm, test_program);
    println!(
        "{:0>2X} {:0>2X} {:0>2X}",
        vm.memory[0], vm.memory[1], vm.memory[2]
    );
    let input_device = FileInputDevice::new("tests/copy_input.txt").unwrap();
    let contents = include_bytes!("copy_input.txt");
    let mut expected = contents.to_vec();
    expected.push(b'E');
    expected.push(b'O');
    expected.push(b'F');
    let (output_buffer, output_device) = MemoryOutputDevice::new();
    vm.add_device(Box::new(input_device), 0xF1);
    vm.add_device(Box::new(output_device), 0x05);
    let stop = vm.run_until(1000);
    println!("buf len: {}", output_buffer.borrow().len());
    assert_eq!(stop, StopReason::Halted);
    assert_eq!(*output_buffer.borrow_mut(), expected);
}

#[test]
fn test_devices_xe() {
    let test_program = include_str!("../programs/xe/copy_xe.ebj");
    let test_harness = include_str!("../programs/xe/test_harness.ebj");
    let mut vm = vm_with_program(test_harness);
    load_program_at(&mut vm, test_program, 0);
    println!(
        "{:0>2X} {:0>2X} {:0>2X}",
        vm.memory[0], vm.memory[1], vm.memory[2]
    );
    let input_device = FileInputDevice::new("tests/copy_input.txt").unwrap();
    let contents = include_bytes!("copy_input.txt");
    let mut expected = contents.to_vec();
    expected.push(b'E');
    expected.push(b'O');
    expected.push(b'F');
    let (output_buffer, output_device) = MemoryOutputDevice::new();
    vm.add_device(Box::new(input_device), 0xF1);
    vm.add_device(Box::new(output_device), 0x05);
    let stop = vm.run_until(1000);
    println!("buf len: {}", output_buffer.borrow().len());
    assert_eq!(stop, StopReason::Halted);
    assert_eq!(*output_buffer.borrow_mut(), expected);
}

use libsic::{
    device::{FileInputDevice, MemoryOutputDevice},
    xe::load::{load_program_to, vm_with_program},
    xe::vm::StopReason,
    WordExt,
};

#[test]
fn test_relocation() {
    let test_program = include_str!("../programs/xe/relo.ebj");
    let mut vm = vm_with_program(test_program);
    assert_eq!(vm.A.as_u32(), 0);
    assert_eq!(vm.run_until(100), StopReason::Halted);
    assert_eq!(vm.A.as_u32(), 15);
    assert_eq!(vm.T.as_u32(), 2000);
}

#[test]
fn test_simple_add() {
    let test_program = include_str!("../programs/xe/add.ebj");
    let mut vm = vm_with_program(test_program);
    assert_eq!(vm.A.as_u32(), 0);
    assert_eq!(vm.run_until(100), StopReason::Halted);
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
    expected.push('E' as u8);
    expected.push('O' as u8);
    expected.push('F' as u8);
    let (output_buffer, output_device) = MemoryOutputDevice::new();
    vm.add_device(Box::new(input_device), 0xF1);
    vm.add_device(Box::new(output_device), 0x05);
    let stop = vm.run_until(1000);
    println!("buf len: {}", output_buffer.borrow().len());
    assert_eq!(stop, StopReason::Halted);
    assert_eq!(*output_buffer.borrow_mut(), expected);
}

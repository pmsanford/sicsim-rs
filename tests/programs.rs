use libsic::{
    device::{FileInputDevice, MemoryOutputDevice},
    load::{load_program_to, vm_with_program},
    vm::StopReason,
    WordExt,
};

#[test]
fn test_simple_add() {
    let test_program = include_str!("../programs/add.ebj");
    let mut vm = vm_with_program(test_program);
    assert_eq!(vm.A.as_u32(), 0);
    assert_eq!(vm.run_until(100), StopReason::Halted);
    assert_eq!(vm.A.as_u32(), 5);
}

#[test]
fn test_devices() {
    let test_program = include_str!("../programs/copy.ebj");
    let test_harness = include_str!("../programs/test_harness.ebj");
    let mut vm = vm_with_program(test_harness);
    load_program_to(&mut vm, test_program);
    let input_device = FileInputDevice::new("tests/input.txt").unwrap();
    let contents = include_bytes!("input.txt");
    let mut expected = contents.to_vec();
    expected.push('E' as u8);
    expected.push('O' as u8);
    expected.push('F' as u8);
    let (output_buffer, output_device) = MemoryOutputDevice::new();
    vm.add_device(Box::new(input_device), 0xF1);
    vm.add_device(Box::new(output_device), 0x05);
    assert_eq!(vm.run_until(1000), StopReason::Halted);
    assert_eq!(*output_buffer.borrow_mut(), expected);
}

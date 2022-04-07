use libsic::{
    vm::{StopReason, Vm},
    WordExt,
};

#[test]
fn test_simple_add() {
    let test_program = include_str!("../programs/add.ebj");
    let mut vm = Vm::with_program(test_program);
    assert_eq!(vm.A.as_u32(), 0);
    assert_eq!(vm.run_until(100), StopReason::Halted);
    assert_eq!(vm.A.as_u32(), 5);
}

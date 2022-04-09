use super::op::{AddressFlags, Op, VariableOps};
use crate::device::Device;
use crate::word::{u32_to_word, DWord, Word, WordExt};
use std::cmp::Ordering;
use std::{collections::HashMap, fmt::Debug};

#[allow(non_snake_case)]
pub struct SicVm {
    pub memory: Vec<u8>,
    pub A: Word,  // 0
    pub X: Word,  // 1
    pub L: Word,  // 2
    pub B: Word,  // 3
    pub S: Word,  // 4
    pub T: Word,  // 5
    pub F: DWord, // 6
    pub PC: Word, // 8
    pub SW: Word, // 9
    devices: HashMap<u8, Box<dyn Device>>,
}

impl Debug for SicVm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vm")
            .field("A", &self.A)
            .field("X", &self.X)
            .field("L", &self.L)
            .field("PC", &self.PC)
            .field("SW", &self.SW)
            .finish()
    }
}

const CC_MASK: u8 = 0x03;
const CC_LT: u8 = 0x01;
const CC_GT: u8 = 0x02;
const CC_EQ: u8 = 0x00;
const CC_BYTE: usize = 0;

fn set_cc(sw: &mut Word, val: Ordering) {
    sw[CC_BYTE] = (sw[CC_BYTE] & (CC_MASK ^ 0xFF))
        | match val {
            Ordering::Less => CC_LT,
            Ordering::Equal => CC_EQ,
            Ordering::Greater => CC_GT,
        };
}

fn check_cc(sw: &Word) -> Ordering {
    let cc = sw[CC_BYTE] & CC_MASK;
    match cc {
        CC_LT => Ordering::Less,
        CC_EQ => Ordering::Equal,
        CC_GT => Ordering::Greater,
        // All possibilities covered assuming the mask is correct
        _ => unreachable!(),
    }
}

impl SicVm {
    pub fn empty() -> Self {
        Self {
            memory: vec![0; 1 << 20],
            A: [0; 3],
            X: [0; 3],
            L: [0; 3],
            B: [0; 3],
            S: [0; 3],
            T: [0; 3],
            F: [0; 6],
            PC: [0; 3],
            SW: [0; 3],
            devices: HashMap::new(),
        }
    }

    pub fn add_device(&mut self, device: Box<dyn Device>, address: u8) -> Option<Box<dyn Device>> {
        self.devices.insert(address, device)
    }

    pub fn remove_device(&mut self, address: u8) -> Option<Box<dyn Device>> {
        self.devices.remove(&address)
    }

    pub fn set_pc(&mut self, address: u16) {
        self.PC = u32_to_word(address as u32);
    }

    fn test_device(&mut self, device_id: u8) -> bool {
        self.devices
            .get_mut(&device_id)
            .map(|device| device.test())
            .unwrap_or(false)
    }

    fn read_device(&mut self, device_id: u8) -> u8 {
        self.devices
            .get_mut(&device_id)
            .map(|device| device.read())
            .unwrap_or(0)
    }

    fn write_device(&mut self, device_id: u8, data: u8) {
        if let Some(device) = self.devices.get_mut(&device_id) {
            device.write(data);
        }
    }

    fn calc_addr(&self, address: u32, flags: &AddressFlags) -> usize {
        let idx: usize = if flags.indexed { self.X.as_usize() } else { 0 };
        address as usize + idx
    }

    fn word_at(&self, address: u32, flags: &AddressFlags) -> Word {
        let address = self.calc_addr(address, flags);
        [
            self.memory[address],
            self.memory[address + 1],
            self.memory[address + 2],
        ]
    }

    fn set_at(&mut self, address: u32, flags: &AddressFlags, value: Word) {
        let address = self.calc_addr(address, flags);
        self.memory[address] = value[0];
        self.memory[address + 1] = value[1];
        self.memory[address + 2] = value[2];
    }

    fn pc_address(&self) -> u32 {
        u32::from_be_bytes([0, 0, self.PC[1], self.PC[2]])
    }

    fn comp(&mut self, register: Word, address: u32, flags: &AddressFlags) {
        let memory = self.word_at(address, flags);
        set_cc(&mut self.SW, register.cmp(&memory));
    }

    pub fn run_until(&mut self, max_cycles: u64) -> StopReason {
        let mut pc: Option<Word> = None;
        let mut cycles = 0;
        loop {
            self.step();
            cycles += 1;
            if let Some(prev) = pc {
                if prev == self.PC {
                    break StopReason::Halted;
                }
            }
            if cycles >= max_cycles {
                break StopReason::CycleLimit;
            }
            pc = Some(self.PC);
        }
    }

    fn get_op_at_pc(&self) -> Option<Op> {
        let pc = self.pc_address() as usize;
        let bytes = [
            self.memory[pc],
            self.memory[pc + 1],
            self.memory[pc + 2],
            self.memory[pc + 3],
        ];
        Op::from_bytes(bytes)
    }

    pub fn step(&mut self) {
        let op = self.get_op_at_pc();
        self.PC = u32_to_word(self.PC.as_u32() + 3);
        match op {
            Some(Op::Variable(op)) => {
                match op.opcode {
                    // Load/store
                    VariableOps::LDA => {
                        self.A = self.word_at(op.address, &op.address_flags);
                    }
                    VariableOps::STA => {
                        self.set_at(op.address, &op.address_flags, self.A);
                    }
                    VariableOps::LDCH => {
                        let address = self.calc_addr(op.address, &op.address_flags);
                        self.A[2] = self.memory[address];
                    }
                    VariableOps::STCH => {
                        let address = self.calc_addr(op.address, &op.address_flags);
                        self.memory[address] = self.A[2];
                    }
                    VariableOps::LDL => {
                        self.L = self.word_at(op.address, &op.address_flags);
                    }
                    VariableOps::STL => {
                        self.set_at(op.address, &op.address_flags, self.L);
                    }
                    VariableOps::LDX => {
                        self.X = self.word_at(op.address, &op.address_flags);
                    }
                    VariableOps::STX => {
                        self.set_at(op.address, &op.address_flags, self.X);
                    }
                    VariableOps::STSW => {
                        self.set_at(op.address, &op.address_flags, self.SW);
                    }

                    // Bitwise
                    VariableOps::OR => {
                        let memory = self.word_at(op.address, &op.address_flags);
                        self.A = [
                            self.A[0] | memory[0],
                            self.A[1] | memory[1],
                            self.A[2] | memory[2],
                        ];
                    }
                    VariableOps::AND => {
                        let memory = self.word_at(op.address, &op.address_flags);
                        self.A = [
                            self.A[0] & memory[0],
                            self.A[1] & memory[1],
                            self.A[2] & memory[2],
                        ];
                    }

                    // Math
                    VariableOps::ADD => {
                        let memory = self.word_at(op.address, &op.address_flags);
                        self.A = u32_to_word(self.A.as_u32().wrapping_add(memory.as_u32()));
                    }
                    VariableOps::SUB => {
                        let memory = self.word_at(op.address, &op.address_flags);
                        self.A = u32_to_word(self.A.as_u32().wrapping_sub(memory.as_u32()));
                    }
                    VariableOps::MUL => {
                        let memory = self.word_at(op.address, &op.address_flags);
                        self.A = u32_to_word(self.A.as_u32().wrapping_mul(memory.as_u32()));
                    }
                    VariableOps::DIV => {
                        let memory = self.word_at(op.address, &op.address_flags);
                        self.A = u32_to_word(self.A.as_u32().wrapping_div(memory.as_u32()));
                    }

                    // Comp
                    VariableOps::COMP => {
                        self.comp(self.A, op.address, &op.address_flags);
                    }

                    // Jumps
                    VariableOps::J => {
                        self.PC = u32_to_word(op.address);
                    }
                    VariableOps::JLT => {
                        if check_cc(&self.SW) == Ordering::Less {
                            self.PC = u32_to_word(op.address);
                        }
                    }
                    VariableOps::JEQ => {
                        if check_cc(&self.SW) == Ordering::Equal {
                            self.PC = u32_to_word(op.address);
                        }
                    }
                    VariableOps::JGT => {
                        if check_cc(&self.SW) == Ordering::Greater {
                            self.PC = u32_to_word(op.address);
                        }
                    }

                    // Subroutines
                    VariableOps::JSUB => {
                        self.L = self.PC;
                        self.PC = u32_to_word(op.address);
                    }
                    VariableOps::RSUB => {
                        self.PC = self.L;
                    }

                    // Index
                    VariableOps::TIX => {
                        self.X = u32_to_word(self.X.as_u32() + 1);
                        self.comp(self.X, op.address, &op.address_flags);
                    }

                    // Devices
                    VariableOps::RD => {
                        let device_id = self.memory[op.address as usize];
                        self.A[2] = self.read_device(device_id);
                    }
                    VariableOps::TD => {
                        let device_id = self.memory[op.address as usize];
                        if self.test_device(device_id) {
                            set_cc(&mut self.SW, Ordering::Less);
                        } else {
                            set_cc(&mut self.SW, Ordering::Equal);
                        }
                    }
                    VariableOps::WD => {
                        let device_id = self.memory[op.address as usize];
                        self.write_device(device_id, self.A[2]);
                    }
                    _ => {} //TODO: Xe instructions
                }
            }
            _ => {} //TODO: One- and Two-byte
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum StopReason {
    Halted,
    CycleLimit,
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, ops::Deref, rc::Rc};

    use crate::xe::op::{AddressMode, Variable};

    use super::*;

    fn set_int(vm: &mut SicVm, address: usize, v: u32) {
        let [_, a, b, c] = v.to_be_bytes();
        vm.memory[address] = a;
        vm.memory[address + 1] = b;
        vm.memory[address + 2] = c;
    }

    fn set_op(vm: &mut SicVm, address: usize, op: Op) {
        let word = op.to_bytes();
        vm.memory[address] = word[0];
        vm.memory[address + 1] = word[1];
        vm.memory[address + 2] = word[2];
    }

    fn ni_op(vm: &mut SicVm, address: usize, opcode: VariableOps, target: u32) {
        set_op(
            vm,
            address,
            Op::Variable(Variable {
                opcode,
                address_flags: AddressFlags {
                    mode: AddressMode::Compatiblity,
                    indexed: false,
                    base_relative: false,
                    pc_relative: false,
                    extended: false,
                },
                address: target,
            }),
        );
    }

    #[allow(dead_code)]
    fn print_word(vm: &SicVm, address: usize) {
        println!(
            "word at {}: 0x{:X} 0x{:X} 0x{:X}",
            address,
            vm.memory[address],
            vm.memory[address + 1],
            vm.memory[address + 2]
        );
    }

    fn setup_op(opcode: VariableOps, address: u32) -> SicVm {
        let mut vm = SicVm::empty();
        set_op(
            &mut vm,
            0,
            Op::Variable(Variable {
                opcode,
                address_flags: AddressFlags {
                    mode: AddressMode::Compatiblity,
                    indexed: false,
                    base_relative: false,
                    pc_relative: false,
                    extended: false,
                },
                address,
            }),
        );
        vm
    }

    #[test]
    fn lda() {
        let mut vm = setup_op(VariableOps::LDA, 3);
        set_int(&mut vm, 3, 0xAB);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xAB);
        assert_eq!(vm.PC[2], 3);

        let mut vm = SicVm::empty();
        vm.X = u32_to_word(3);
        set_op(
            &mut vm,
            0,
            Op::Variable(Variable {
                opcode: VariableOps::LDA,
                address_flags: AddressFlags {
                    mode: AddressMode::Compatiblity,
                    indexed: true,
                    base_relative: false,
                    pc_relative: false,
                    extended: false,
                },
                address: 3,
            }),
        );
        set_int(&mut vm, 3, 0xAB);
        set_int(&mut vm, 6, 0xBA);
        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xBA);
        assert_eq!(vm.PC[2], 3);
    }

    static NI_FLAGS: AddressFlags = AddressFlags {
        mode: AddressMode::Compatiblity,
        indexed: false,
        base_relative: false,
        pc_relative: false,
        extended: false,
    };

    static I_FLAGS: AddressFlags = AddressFlags {
        mode: AddressMode::Compatiblity,
        indexed: true,
        base_relative: false,
        pc_relative: false,
        extended: false,
    };

    #[test]
    fn sta() {
        let mut vm = setup_op(VariableOps::STA, 3);
        vm.A = u32_to_word(0xAAAA);

        vm.step();

        let a_val: u32 = vm.word_at(3, &NI_FLAGS).as_u32();
        assert_eq!(a_val, 0xAAAA);
        assert_eq!(vm.PC[2], 3);
    }

    #[test]
    fn load_store() {
        let mut vm = setup_op(VariableOps::LDA, 99);
        set_op(
            &mut vm,
            3,
            Op::Variable(Variable {
                opcode: VariableOps::STA,
                address_flags: NI_FLAGS.clone(),
                address: 102,
            }),
        );
        set_int(&mut vm, 99, 0xBEEF);

        vm.step();
        vm.step();

        let stored: u32 = vm.word_at(102, &NI_FLAGS).as_u32();
        assert_eq!(stored, 0xBEEF);
    }

    #[test]
    fn ch() {
        let mut vm = setup_op(VariableOps::LDCH, 100);
        set_int(&mut vm, 99, 0xBE00);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xBE);
        let mut vm = setup_op(VariableOps::STCH, 100);
        vm.A = u32_to_word(0xBA);

        vm.step();

        let m_val: u32 = vm.word_at(98, &NI_FLAGS).as_u32();
        assert_eq!(m_val, 0xBA);
    }

    #[test]
    fn bitwise() {
        let mut vm = setup_op(VariableOps::OR, 99);
        set_int(&mut vm, 99, 0x8181);
        vm.A = u32_to_word(0x1818);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0x9999);

        let mut vm = setup_op(VariableOps::AND, 99);
        set_int(&mut vm, 99, 0x8181);
        vm.A = u32_to_word(0xFFFF);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0x8181);
    }

    #[test]
    fn overflow() {
        let mut vm = setup_op(VariableOps::ADD, 99);
        set_int(&mut vm, 99, 0xFF_FF_FF);
        vm.A = u32_to_word(1);

        vm.step();
        assert_eq!(vm.A.as_i32(), 0);
    }

    #[test]
    fn math() {
        let mut vm = setup_op(VariableOps::ADD, 99);
        set_int(&mut vm, 99, (-32 as i32) as u32);
        vm.A = u32_to_word(32);

        vm.step();

        assert_eq!(vm.A.as_i32(), 0);
        vm.PC = u32_to_word(0);
        vm.step();
        assert_eq!(vm.A.as_i32(), -32);
        set_int(&mut vm, 99, 64);
        vm.PC = u32_to_word(0);
        vm.step();
        assert_eq!(vm.A.as_i32(), 32);

        let mut vm = setup_op(VariableOps::SUB, 99);
        set_int(&mut vm, 99, 10);
        vm.A = u32_to_word(5);

        vm.step();

        assert_eq!(vm.A.as_i32(), -5);

        let mut vm = setup_op(VariableOps::MUL, 99);
        set_int(&mut vm, 99, 10);
        vm.A = u32_to_word(5);

        vm.step();

        assert_eq!(vm.A.as_i32(), 50);

        let mut vm = setup_op(VariableOps::DIV, 99);
        set_int(&mut vm, 99, 5);
        vm.A = u32_to_word(10);

        vm.step();

        assert_eq!(vm.A.as_i32(), 2);
    }

    #[test]
    fn jumps() {
        let mut vm = setup_op(VariableOps::J, 33);
        set_int(&mut vm, 99, 33);
        ni_op(&mut vm, 33, VariableOps::LDA, 99);

        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 33);

        // Comp JEQ EQ
        let mut vm = setup_op(VariableOps::COMP, 99);
        ni_op(&mut vm, 3, VariableOps::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 33);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_EQ);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 33);

        // Comp JEQ LT
        let mut vm = setup_op(VariableOps::COMP, 99);
        ni_op(&mut vm, 3, VariableOps::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_LT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JEQ GT
        let mut vm = setup_op(VariableOps::COMP, 99);
        ni_op(&mut vm, 3, VariableOps::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_GT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JLT
        let mut vm = setup_op(VariableOps::COMP, 99);
        ni_op(&mut vm, 3, VariableOps::JLT, 34);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_LT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 34);

        // Comp JGT
        let mut vm = setup_op(VariableOps::COMP, 99);
        ni_op(&mut vm, 3, VariableOps::JGT, 32);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_GT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 32);
    }

    #[test]
    fn subroutines() {
        let mut vm = setup_op(VariableOps::JSUB, 99);
        ni_op(&mut vm, 99, VariableOps::LDA, 199);
        ni_op(&mut vm, 102, VariableOps::RSUB, 0);
        ni_op(&mut vm, 3, VariableOps::STA, 6);
        set_int(&mut vm, 199, 1234);
        set_int(&mut vm, 9, 99);

        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.word_at(6, &NI_FLAGS).as_u32(), 1234);
    }

    #[test]
    fn index() {
        let mut vm = SicVm::empty();
        set_op(
            &mut vm,
            0,
            Op::Variable(Variable {
                opcode: VariableOps::ADD,
                address_flags: I_FLAGS.clone(),
                address: 99,
            }),
        );
        ni_op(&mut vm, 3, VariableOps::TIX, 96);
        ni_op(&mut vm, 6, VariableOps::TIX, 96);
        ni_op(&mut vm, 9, VariableOps::TIX, 96);
        ni_op(&mut vm, 12, VariableOps::JLT, 0);
        set_int(&mut vm, 93, 0);
        set_int(&mut vm, 96, 15);
        set_int(&mut vm, 99, 1);
        set_int(&mut vm, 102, 2);
        set_int(&mut vm, 105, 3);
        set_int(&mut vm, 108, 4);
        set_int(&mut vm, 111, 5);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 1);
        assert_eq!(vm.X.as_u32(), 3);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 3);
        assert_eq!(vm.X.as_u32(), 6);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 6);
        assert_eq!(vm.X.as_u32(), 9);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 10);
        assert_eq!(vm.X.as_u32(), 12);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 15);
        assert_eq!(vm.X.as_u32(), 15);
        assert_eq!(vm.PC.as_u32(), 15);
    }

    struct TestDevice {
        tested: bool,
        content: String,
        pointer: usize,
        write_buf: Rc<RefCell<Vec<u8>>>,
    }

    impl Device for TestDevice {
        fn test(&mut self) -> bool {
            if self.tested {
                true
            } else {
                self.tested = true;
                false
            }
        }

        fn read(&mut self) -> u8 {
            let d = self.content.chars().skip(self.pointer).next().unwrap() as u8;
            self.pointer = self.pointer + 1;
            if self.pointer >= self.content.len() {
                self.pointer = 0;
            }
            d
        }

        fn write(&mut self, data: u8) {
            self.write_buf.deref().borrow_mut().push(data);
        }
    }

    #[test]
    fn devices() {
        let mut vm = setup_op(VariableOps::TD, 99);
        let write_buf = Rc::new(RefCell::new(Vec::new()));
        vm.devices.insert(
            1,
            Box::new(TestDevice {
                tested: false,
                content: String::from("Hello"),
                pointer: 0,
                write_buf: Rc::clone(&write_buf),
            }),
        );
        vm.memory[99] = 1;
        set_int(&mut vm, 108, 5);
        set_int(&mut vm, 206, 1234);
        ni_op(&mut vm, 3, VariableOps::JLT, 9);
        ni_op(&mut vm, 6, VariableOps::J, 0);
        ni_op(&mut vm, 9, VariableOps::RD, 99);
        ni_op(&mut vm, 12, VariableOps::TD, 99);
        ni_op(&mut vm, 15, VariableOps::JLT, 21);
        ni_op(&mut vm, 18, VariableOps::J, 12);
        ni_op(&mut vm, 21, VariableOps::WD, 99);
        ni_op(&mut vm, 24, VariableOps::TIX, 108);
        ni_op(&mut vm, 27, VariableOps::JLT, 0);
        ni_op(&mut vm, 30, VariableOps::LDA, 206);

        vm.step();
        vm.step();
        vm.step();
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A[2], 'H' as u8);

        for _ in 0..1000 {
            if vm.A.as_u32() == 1234 {
                break;
            }
            vm.step();
        }

        let buf = write_buf.deref().borrow();

        assert_eq!(*buf, vec![72, 101, 108, 108, 111]);
    }

    #[test]
    fn unrecognized() {
        // TODO: Treat unrecognized opcodes as noop for now
        let mut vm = SicVm::empty();
        vm.memory[0] = 0xFF;
        let mut expected = SicVm::empty();
        expected.memory = vm.memory.clone();

        vm.step();

        assert_eq!(vm.PC.as_u32(), 3);

        assert_eq!(vm.A, expected.A);
        assert_eq!(vm.X, expected.X);
        assert_eq!(vm.L, expected.L);
        assert_eq!(vm.SW, expected.SW);
        assert_eq!(vm.memory, expected.memory);
    }
}

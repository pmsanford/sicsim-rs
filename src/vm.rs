use crate::device::Device;
use crate::load::{copy_to_memory, load_program, Program};
use crate::op::{Op, OpCode};
use crate::word::{u16_to_word, u32_to_word, Word, WordExt};
use std::cmp::Ordering;
use std::{collections::HashMap, fmt::Debug};

#[allow(non_snake_case)]
pub struct Vm {
    pub memory: [u8; 1 << 15],
    pub A: Word,  // 0
    pub X: Word,  // 1
    pub L: Word,  // 2
    pub PC: Word, // 8
    pub SW: Word, // 9
    devices: HashMap<u8, Box<dyn Device>>,
    loaded_programs: HashMap<String, Program>,
}

impl Debug for Vm {
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
const CC_BYTE: usize = 2;

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

impl Vm {
    pub fn empty() -> Self {
        Self {
            memory: [0; 1 << 15],
            A: [0; 3],
            X: [0; 3],
            L: [0; 3],
            PC: [0; 3],
            SW: [0; 3],
            devices: HashMap::new(),
            loaded_programs: HashMap::new(),
        }
    }

    pub fn with_program(program_text: &str) -> Self {
        let mut vm = Vm::empty();
        let program = load_program(program_text);
        copy_to_memory(&mut vm.memory, &program);
        vm.set_pc(program.end.first_address);
        vm
    }

    pub fn load_program(&mut self, program_text: &str) {
        // TODO: Check overlapping memory ranges
        let program = load_program(program_text);
        copy_to_memory(&mut self.memory, &program);
        self.loaded_programs
            .insert(program.header.name.clone(), program);
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

    fn calc_addr(&self, address: u16, indexed: bool) -> usize {
        let idx: usize = if indexed { self.X.as_usize() } else { 0 };
        address as usize + idx
    }

    fn word_at(&self, address: u16, indexed: bool) -> Word {
        let address = self.calc_addr(address, indexed);
        [
            self.memory[address],
            self.memory[address + 1],
            self.memory[address + 2],
        ]
    }

    fn set_at(&mut self, address: u16, indexed: bool, value: Word) {
        let address = self.calc_addr(address, indexed);
        self.memory[address] = value[0];
        self.memory[address + 1] = value[1];
        self.memory[address + 2] = value[2];
    }

    fn pc_address(&self) -> u16 {
        u16::from_be_bytes([self.PC[1], self.PC[2]])
    }

    fn comp(&mut self, register: Word, address: u16, indexed: bool) {
        let memory = self.word_at(address, indexed);
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

    pub fn step(&mut self) {
        let op = Op::from_word(self.word_at(self.pc_address(), false));
        self.PC = u32_to_word(self.PC.as_u32() + 3);
        if let Some(op) = op {
            match op.opcode {
                // Load/store
                OpCode::LDA => {
                    self.A = self.word_at(op.address, op.indexed);
                }
                OpCode::STA => {
                    self.set_at(op.address, op.indexed, self.A);
                }
                OpCode::LDCH => {
                    let address = self.calc_addr(op.address, op.indexed);
                    self.A[2] = self.memory[address];
                }
                OpCode::STCH => {
                    let address = self.calc_addr(op.address, op.indexed);
                    self.memory[address] = self.A[2];
                }
                OpCode::LDL => {
                    self.L = self.word_at(op.address, op.indexed);
                }
                OpCode::STL => {
                    self.set_at(op.address, op.indexed, self.L);
                }
                OpCode::LDX => {
                    self.X = self.word_at(op.address, op.indexed);
                }
                OpCode::STX => {
                    self.set_at(op.address, op.indexed, self.X);
                }
                OpCode::STSW => {
                    self.set_at(op.address, op.indexed, self.SW);
                }

                // Bitwise
                OpCode::OR => {
                    let memory = self.word_at(op.address, op.indexed);
                    self.A = [
                        self.A[0] | memory[0],
                        self.A[1] | memory[1],
                        self.A[2] | memory[2],
                    ];
                }
                OpCode::AND => {
                    let memory = self.word_at(op.address, op.indexed);
                    self.A = [
                        self.A[0] & memory[0],
                        self.A[1] & memory[1],
                        self.A[2] & memory[2],
                    ];
                }

                // Math
                OpCode::ADD => {
                    let memory = self.word_at(op.address, op.indexed);
                    self.A = u32_to_word(self.A.as_u32().wrapping_add(memory.as_u32()));
                }
                OpCode::SUB => {
                    let memory = self.word_at(op.address, op.indexed);
                    self.A = u32_to_word(self.A.as_u32().wrapping_sub(memory.as_u32()));
                }
                OpCode::MUL => {
                    let memory = self.word_at(op.address, op.indexed);
                    self.A = u32_to_word(self.A.as_u32().wrapping_mul(memory.as_u32()));
                }
                OpCode::DIV => {
                    let memory = self.word_at(op.address, op.indexed);
                    self.A = u32_to_word(self.A.as_u32().wrapping_div(memory.as_u32()));
                }

                // Comp
                OpCode::COMP => {
                    self.comp(self.A, op.address, op.indexed);
                }

                // Jumps
                OpCode::J => {
                    self.PC = u16_to_word(op.address);
                }
                OpCode::JLT => {
                    if check_cc(&self.SW) == Ordering::Less {
                        self.PC = u16_to_word(op.address);
                    }
                }
                OpCode::JEQ => {
                    if check_cc(&self.SW) == Ordering::Equal {
                        self.PC = u16_to_word(op.address);
                    }
                }
                OpCode::JGT => {
                    if check_cc(&self.SW) == Ordering::Greater {
                        self.PC = u16_to_word(op.address);
                    }
                }

                // Subroutines
                OpCode::JSUB => {
                    self.L = self.PC;
                    self.PC = u16_to_word(op.address);
                }
                OpCode::RSUB => {
                    self.PC = self.L;
                }

                // Index
                OpCode::TIX => {
                    self.X = u32_to_word(self.X.as_u32() + 1);
                    self.comp(self.X, op.address, op.indexed);
                }

                // Devices
                OpCode::RD => {
                    let device_id = self.memory[op.address as usize];
                    self.A[2] = self.read_device(device_id);
                }
                OpCode::TD => {
                    let device_id = self.memory[op.address as usize];
                    if self.test_device(device_id) {
                        set_cc(&mut self.SW, Ordering::Less);
                    } else {
                        set_cc(&mut self.SW, Ordering::Equal);
                    }
                }
                OpCode::WD => {
                    let device_id = self.memory[op.address as usize];
                    self.write_device(device_id, self.A[2]);
                }
            }
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

    use super::*;

    #[test]
    fn word_ext() {
        let u: u32 = [0xFF, 0xFF, 0xFF].as_u32();
        assert_eq!(u, 0x00FFFFFF);
        let u: u32 = [0xAB, 0xCD, 0xEF].as_u32();
        assert_eq!(u, 0x00ABCDEF);
        let u: u32 = u32_to_word(0x00CAFEBA).as_u32();
        assert_eq!(u, 0x00CAFEBA);
        let u: usize = u32_to_word(0x00DEDBEF).as_usize();
        assert_eq!(u, 0x00DEDBEF);
        let i: i32 = u32_to_word((-5 as i32) as u32).as_i32();
        assert_eq!(i, -5);
        let i: i32 = u32_to_word(350).as_i32();
        assert_eq!(i, 350);
        let w: Word = u32_to_word(0xCAFEBA);
        assert_eq!(w, [0xCA, 0xFE, 0xBA]);
        let w: Word = u32_to_word((-1 as i32) as u32);
        assert_eq!(w, [0xFF, 0xFF, 0xFF]);
        let w: Word = u32_to_word((-2 as i32) as u32);
        assert_eq!(w, [0xFF, 0xFF, 0xFE]);
    }

    fn set_int(vm: &mut Vm, address: usize, v: u32) {
        let [_, a, b, c] = v.to_be_bytes();
        vm.memory[address] = a;
        vm.memory[address + 1] = b;
        vm.memory[address + 2] = c;
    }

    fn set_op(vm: &mut Vm, address: usize, op: Op) {
        let word: Word = op.into();
        vm.memory[address] = word[0];
        vm.memory[address + 1] = word[1];
        vm.memory[address + 2] = word[2];
    }

    fn ni_op(vm: &mut Vm, address: usize, opcode: OpCode, target: u16) {
        set_op(
            vm,
            address,
            Op {
                opcode,
                indexed: false,
                address: target,
            },
        );
    }

    #[allow(dead_code)]
    fn print_word(vm: &Vm, address: usize) {
        println!(
            "word at {}: 0x{:X} 0x{:X} 0x{:X}",
            address,
            vm.memory[address],
            vm.memory[address + 1],
            vm.memory[address + 2]
        );
    }

    fn setup_op(opcode: OpCode, address: u16) -> Vm {
        let mut vm = Vm::empty();
        set_op(
            &mut vm,
            0,
            Op {
                opcode,
                indexed: false,
                address,
            },
        );
        vm
    }

    #[test]
    fn lda() {
        let mut vm = setup_op(OpCode::LDA, 3);
        set_int(&mut vm, 3, 0xAB);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xAB);
        assert_eq!(vm.PC[2], 3);

        let mut vm = Vm::empty();
        vm.X = u32_to_word(3);
        set_op(
            &mut vm,
            0,
            Op {
                opcode: OpCode::LDA,
                indexed: true,
                address: 3,
            },
        );
        set_int(&mut vm, 3, 0xAB);
        set_int(&mut vm, 6, 0xBA);
        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xBA);
        assert_eq!(vm.PC[2], 3);
    }

    #[test]
    fn sta() {
        let mut vm = setup_op(OpCode::STA, 3);
        vm.A = u32_to_word(0xAAAA);

        vm.step();

        let a_val: u32 = vm.word_at(3, false).as_u32();
        assert_eq!(a_val, 0xAAAA);
        assert_eq!(vm.PC[2], 3);
    }

    #[test]
    fn load_store() {
        let mut vm = setup_op(OpCode::LDA, 99);
        set_op(
            &mut vm,
            3,
            Op {
                opcode: OpCode::STA,
                indexed: false,
                address: 102,
            },
        );
        set_int(&mut vm, 99, 0xBEEF);

        vm.step();
        vm.step();

        let stored: u32 = vm.word_at(102, false).as_u32();
        assert_eq!(stored, 0xBEEF);
    }

    #[test]
    fn ch() {
        let mut vm = setup_op(OpCode::LDCH, 100);
        set_int(&mut vm, 99, 0xBE00);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xBE);
        let mut vm = setup_op(OpCode::STCH, 100);
        vm.A = u32_to_word(0xBA);

        vm.step();

        let m_val: u32 = vm.word_at(98, false).as_u32();
        assert_eq!(m_val, 0xBA);
    }

    #[test]
    fn bitwise() {
        let mut vm = setup_op(OpCode::OR, 99);
        set_int(&mut vm, 99, 0x8181);
        vm.A = u32_to_word(0x1818);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0x9999);

        let mut vm = setup_op(OpCode::AND, 99);
        set_int(&mut vm, 99, 0x8181);
        vm.A = u32_to_word(0xFFFF);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0x8181);
    }

    #[test]
    fn overflow() {
        let mut vm = setup_op(OpCode::ADD, 99);
        set_int(&mut vm, 99, 0xFF_FF_FF);
        vm.A = u32_to_word(1);

        vm.step();
        assert_eq!(vm.A.as_i32(), 0);
    }

    #[test]
    fn math() {
        let mut vm = setup_op(OpCode::ADD, 99);
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

        let mut vm = setup_op(OpCode::SUB, 99);
        set_int(&mut vm, 99, 10);
        vm.A = u32_to_word(5);

        vm.step();

        assert_eq!(vm.A.as_i32(), -5);

        let mut vm = setup_op(OpCode::MUL, 99);
        set_int(&mut vm, 99, 10);
        vm.A = u32_to_word(5);

        vm.step();

        assert_eq!(vm.A.as_i32(), 50);

        let mut vm = setup_op(OpCode::DIV, 99);
        set_int(&mut vm, 99, 5);
        vm.A = u32_to_word(10);

        vm.step();

        assert_eq!(vm.A.as_i32(), 2);
    }

    #[test]
    fn jumps() {
        let mut vm = setup_op(OpCode::J, 33);
        set_int(&mut vm, 99, 33);
        ni_op(&mut vm, 33, OpCode::LDA, 99);

        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 33);

        // Comp JEQ EQ
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 33);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_EQ);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 33);

        // Comp JEQ LT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_LT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JEQ GT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_GT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JLT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JLT, 34);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_LT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 34);

        // Comp JGT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JGT, 32);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_GT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 32);
    }

    #[test]
    fn subroutines() {
        let mut vm = setup_op(OpCode::JSUB, 99);
        ni_op(&mut vm, 99, OpCode::LDA, 199);
        ni_op(&mut vm, 102, OpCode::RSUB, 0);
        ni_op(&mut vm, 3, OpCode::STA, 6);
        set_int(&mut vm, 199, 1234);
        set_int(&mut vm, 9, 99);

        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.word_at(6, false).as_u32(), 1234);
    }

    #[test]
    fn index() {
        let mut vm = Vm::empty();
        set_op(
            &mut vm,
            0,
            Op {
                opcode: OpCode::ADD,
                indexed: true,
                address: 99,
            },
        );
        ni_op(&mut vm, 3, OpCode::TIX, 96);
        ni_op(&mut vm, 6, OpCode::TIX, 96);
        ni_op(&mut vm, 9, OpCode::TIX, 96);
        ni_op(&mut vm, 12, OpCode::JLT, 0);
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
        let mut vm = setup_op(OpCode::TD, 99);
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
        ni_op(&mut vm, 3, OpCode::JLT, 9);
        ni_op(&mut vm, 6, OpCode::J, 0);
        ni_op(&mut vm, 9, OpCode::RD, 99);
        ni_op(&mut vm, 12, OpCode::TD, 99);
        ni_op(&mut vm, 15, OpCode::JLT, 21);
        ni_op(&mut vm, 18, OpCode::J, 12);
        ni_op(&mut vm, 21, OpCode::WD, 99);
        ni_op(&mut vm, 24, OpCode::TIX, 108);
        ni_op(&mut vm, 27, OpCode::JLT, 0);
        ni_op(&mut vm, 30, OpCode::LDA, 206);

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
        let mut vm = Vm::empty();
        vm.memory[0] = 0xFF;
        let mut expected = Vm::empty();
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

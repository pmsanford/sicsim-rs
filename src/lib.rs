use num_derive::FromPrimitive;
use std::{collections::HashMap, fmt::Debug};
pub type Word = [u8; 3];

trait WordExt {
    fn as_u32(&self) -> u32;
    fn as_usize(&self) -> usize;
    fn as_i32(&self) -> i32;
}

impl WordExt for Word {
    fn as_u32(&self) -> u32 {
        let [a, b, c] = *self;
        u32::from_be_bytes([0, a, b, c])
    }

    fn as_usize(&self) -> usize {
        let [a, b, c] = *self;
        usize::from_be_bytes([0, 0, 0, 0, 0, a, b, c])
    }

    fn as_i32(&self) -> i32 {
        let [msb, mid, lsb] = *self;
        let neg = msb & 0x80 > 0;
        let msb = msb & 0x7F;
        let val = u32::from_be_bytes([0, msb, mid, lsb]);
        if neg {
            // Don't do any twos complement just sign extend baybee
            (val | 0xFF_80_00_00) as i32
        } else {
            val as i32
        }
    }
}

fn u32_to_word(i: u32) -> Word {
    let [_, a, b, c] = i.to_be_bytes();
    [a, b, c]
}

#[derive(FromPrimitive, Debug, Clone, Copy)]
enum OpCode {
    ADD = 0x18,
    AND = 0x40,
    COMP = 0x28,
    DIV = 0x24,
    J = 0x3C,
    JEQ = 0x30,
    JGT = 0x34,
    JLT = 0x38,
    JSUB = 0x48,
    LDA = 0x00,
    LDCH = 0x50,
    LDL = 0x08,
    LDX = 0x04,
    MUL = 0x20,

    OR = 0x44,
    RD = 0xD8,
    RSUB = 0x4C,
    STA = 0x0C,
    STCH = 0x54,
    STL = 0x14,
    STSW = 0xE8,
    STX = 0x10,
    SUB = 0x1C,

    TD = 0xE0,
    TIX = 0x2C,
    WD = 0xDC,
}

#[derive(Debug)]
struct Op {
    opcode: OpCode,
    indexed: bool,
    address: u16,
}

impl Op {
    fn from_word(word: Word) -> Option<Self> {
        num::FromPrimitive::from_u8(word[0]).map(|opcode| Self {
            opcode,
            indexed: word[1] & 0b1000_0000 > 0,
            address: u16::from_be_bytes([word[1] & 0x7F, word[2]]),
        })
    }
}

impl Into<Word> for Op {
    fn into(self) -> Word {
        let opcode = self.opcode as u8;
        let [msb, lsb] = self.address.to_be_bytes();
        let mut msb = msb & 0x7F;
        if self.indexed {
            msb = msb | 0x80;
        }

        [opcode, msb, lsb]
    }
}

trait Device {
    fn test(&mut self) -> bool;
    fn read(&mut self) -> u8;
    fn write(&mut self, data: u8);
}

#[allow(non_snake_case)]
pub struct Vm {
    memory: [u8; 1 << 15],
    A: Word,  // 0
    X: Word,  // 1
    L: Word,  // 2
    PC: Word, // 8
    SW: Word, // 9
    devices: HashMap<Word, Box<dyn Device>>,
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
        }
    }

    fn test_device(&mut self, device_id: Word) -> bool {
        self.devices
            .get_mut(&device_id)
            .map(|device| device.test())
            .unwrap_or(false)
    }

    fn read_device(&mut self, device_id: Word) -> u8 {
        self.devices
            .get_mut(&device_id)
            .map(|device| device.read())
            .unwrap_or(0)
    }

    fn write_device(&mut self, device_id: Word, data: u8) {
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
        match register.cmp(&memory) {
            std::cmp::Ordering::Less => self.SW[2] = (self.SW[2] & 0xFC) | 0b0001,
            std::cmp::Ordering::Equal => self.SW[2] = (self.SW[2] & 0xFC) | 0b0000,
            std::cmp::Ordering::Greater => self.SW[2] = (self.SW[2] & 0xFC) | 0b0010,
        }
    }

    pub fn step(&mut self) {
        let op = Op::from_word(self.word_at(self.pc_address(), false)).unwrap();
        self.PC = u32_to_word(self.PC.as_u32() + 3);
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
                self.PC = self.word_at(op.address, op.indexed);
            }
            OpCode::JLT => {
                if self.SW[2] & 0b0011 == 0b0001 {
                    self.PC = self.word_at(op.address, op.indexed);
                }
            }
            OpCode::JEQ => {
                if self.SW[2] & 0b0011 == 0b0000 {
                    self.PC = self.word_at(op.address, op.indexed);
                }
            }
            OpCode::JGT => {
                if self.SW[2] & 0b0011 == 0b0010 {
                    self.PC = self.word_at(op.address, op.indexed);
                }
            }

            // Subroutines
            OpCode::JSUB => {
                self.L = self.PC;
                self.PC = self.word_at(op.address, op.indexed);
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
                let device_id = self.word_at(op.address, op.indexed);
                self.A[2] = self.read_device(device_id);
            }
            OpCode::TD => {
                let device_id = self.word_at(op.address, op.indexed);
                if self.test_device(device_id) {
                    self.SW[2] = (self.SW[2] & 0xFC) | 0b0001;
                } else {
                    self.SW[2] = (self.SW[2] & 0xFC) | 0b0000;
                }
            }
            OpCode::WD => {
                let device_id = self.word_at(op.address, op.indexed);
                self.write_device(device_id, self.A[2]);
            }
        }
    }
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
        let mut vm = setup_op(OpCode::J, 99);
        set_int(&mut vm, 99, 33);
        ni_op(&mut vm, 33, OpCode::LDA, 99);

        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 33);

        // Comp JEQ EQ
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JEQ, 99);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 33);
        vm.step();
        assert_eq!(vm.SW[2] & 0x03, 0);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 33);

        // Comp JEQ LT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JEQ, 99);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[2] & 0x03, 1);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JEQ GT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JEQ, 99);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[2] & 0x03, 2);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JLT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JLT, 99);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[2] & 0x03, 1);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 34);

        // Comp JGT
        let mut vm = setup_op(OpCode::COMP, 99);
        ni_op(&mut vm, 3, OpCode::JGT, 99);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[2] & 0x03, 2);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 32);
    }

    #[test]
    fn subroutines() {
        let mut vm = setup_op(OpCode::JSUB, 9);
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
        ni_op(&mut vm, 12, OpCode::JLT, 93);
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
            u32_to_word(1),
            Box::new(TestDevice {
                tested: false,
                content: String::from("Hello"),
                pointer: 0,
                write_buf: Rc::clone(&write_buf),
            }),
        );
        set_int(&mut vm, 99, 1);
        set_int(&mut vm, 102, 9);
        set_int(&mut vm, 105, 21);
        set_int(&mut vm, 108, 5);
        set_int(&mut vm, 203, 12);
        set_int(&mut vm, 206, 1234);
        ni_op(&mut vm, 3, OpCode::JLT, 102);
        ni_op(&mut vm, 6, OpCode::J, 200);
        ni_op(&mut vm, 9, OpCode::RD, 99);
        ni_op(&mut vm, 12, OpCode::TD, 99);
        ni_op(&mut vm, 15, OpCode::JLT, 105);
        ni_op(&mut vm, 18, OpCode::J, 203);
        ni_op(&mut vm, 21, OpCode::WD, 99);
        ni_op(&mut vm, 24, OpCode::TIX, 108);
        ni_op(&mut vm, 27, OpCode::JLT, 200);
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
}

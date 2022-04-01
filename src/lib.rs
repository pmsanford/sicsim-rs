use num_derive::FromPrimitive;
use std::fmt::Debug;
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
    OR = 0x44, // x
    RD = 0xD8,
    RSUB = 0x4C,
    STA = 0x0C,  // x
    STCH = 0x54, // x
    STL = 0x14,  // x
    STSW = 0xE8, // x
    STX = 0x10,  // x
    SUB = 0x1C,
    ADD = 0x18, // x
    AND = 0x40, // x
    COMP = 0x28,
    DIV = 0x24,
    J = 0x3C,
    JEQ = 0x30,
    JGT = 0x34,
    JLT = 0x38,
    JSUB = 0x48,
    LDA = 0x00,  // x
    LDCH = 0x50, // x
    LDL = 0x08,  // x
    LDX = 0x04,  // x
    MUL = 0x20,
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

#[allow(non_snake_case)]
pub struct Vm {
    memory: [u8; 1 << 15],
    A: Word,  // 0
    X: Word,  // 1
    L: Word,  // 2
    PC: Word, // 8
    SW: Word, // 9
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
    fn empty() -> Self {
        Self {
            memory: [0; 1 << 15],
            A: [0; 3],
            X: [0; 3],
            L: [0; 3],
            PC: [0; 3],
            SW: [0; 3],
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

    pub fn step(&mut self) {
        let op = Op::from_word(self.word_at(self.pc_address(), false)).unwrap();
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
                // TODO: Simulate 24-bit overflow
                self.A = u32_to_word(self.A.as_u32() + memory.as_u32());
            }
            opcode => unimplemented!(r"Opcode {:?} not implemented", opcode),
        }

        match op.opcode {
            // TODO: jumps etc shouldn't increment
            _ => {
                self.PC = u32_to_word(self.PC.as_u32() + 3);
            }
        }
    }
}

#[cfg(test)]
mod test {
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
    }
}

use num_derive::FromPrimitive;
use std::{fmt::Debug, ops::Add};
pub type Word = [u8; 3];

#[derive(Debug)]
pub struct Integer(Word);

impl Add<u32> for Integer {
    type Output = Integer;

    fn add(self, rhs: u32) -> Self::Output {
        let lhs: u32 = self.into();
        Integer::from(lhs + rhs)
    }
}

impl From<Word> for Integer {
    fn from(w: Word) -> Self {
        Self(w)
    }
}

impl From<u32> for Integer {
    fn from(i: u32) -> Self {
        Self([
            ((i & 0x00_FF_00_00) >> 16) as u8,
            ((i & 0x00_00_FF_00) >> 8) as u8,
            (i & 0x00_00_00_FF) as u8,
        ])
    }
}

impl From<i32> for Integer {
    fn from(i: i32) -> Self {
        Self([
            ((i & 0x00_FF_00_00) >> 16) as u8,
            ((i & 0x00_00_FF_00) >> 8) as u8,
            (i & 0x00_00_00_FF) as u8,
        ])
    }
}

impl Into<u32> for Integer {
    fn into(self) -> u32 {
        ((self.0[0] as u32) << 16) + ((self.0[1] as u32) << 8) + self.0[2] as u32
    }
}

impl Into<usize> for Integer {
    fn into(self) -> usize {
        ((self.0[0] as usize) << 16) + ((self.0[1] as usize) << 8) + self.0[2] as usize
    }
}

impl Into<i32> for Integer {
    fn into(self) -> i32 {
        let neg = self.0[0] & 0x80 > 0;
        let msb = self.0[0] & 0x7F;
        let val = ((msb as u32) << 16) + ((self.0[1] as u32) << 8) + (self.0[2] as u32);
        if neg {
            // Don't do any twos complement just sign extend baybee
            (val | 0xFF_80_00_00) as i32
        } else {
            val as i32
        }
    }
}

impl Into<Word> for Integer {
    fn into(self) -> Word {
        self.0
    }
}

#[derive(FromPrimitive, Debug, Clone, Copy)]
enum OpCode {
    OR = 0x44,
    RD = 0xD8,
    RSUB = 0x4C,
    STA = 0x0C,  // x
    STCH = 0x54, // x
    STL = 0x14,  // x
    STSW = 0xE8, // x
    STX = 0x10,  // x
    SUB = 0x1C,
    ADD = 0x18,
    AND = 0x40,
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
            address: (((word[1] & 0b0111_1111) as u16) << 8) + word[2] as u16,
        })
    }
}

impl Into<Word> for Op {
    fn into(self) -> Word {
        let opcode = self.opcode as u8;
        let mut msb = ((self.address & 0x7F_00) >> 8) as u8;
        let lsb = (self.address & 0x00_FF) as u8;
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
        let idx: usize = if indexed {
            Integer::from(self.X).into()
        } else {
            0
        };
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
        ((self.PC[1] as u16) << 8) + self.PC[2] as u16
    }

    pub fn step(&mut self) {
        let op = Op::from_word(self.word_at(self.pc_address(), false)).unwrap();
        match op.opcode {
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
            opcode => unimplemented!(r"Opcode {:?} not implemented", opcode),
        }

        self.PC = (Integer::from(self.PC) + 3).into();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn integer_word() {
        let u: u32 = Integer([0xFF, 0xFF, 0xFF]).into();
        assert_eq!(u, 0x00FFFFFF);
        let u: u32 = Integer([0xAB, 0xCD, 0xEF]).into();
        assert_eq!(u, 0x00ABCDEF);
        let u: u32 = Integer::from(0x00CAFEBA).into();
        assert_eq!(u, 0x00CAFEBA);
        let u: usize = Integer::from(0x00DEDBEF).into();
        assert_eq!(u, 0x00DEDBEF);
        let i: i32 = Integer::from((-5 as i32) as u32).into();
        assert_eq!(i, -5);
        let i: i32 = Integer::from(350).into();
        assert_eq!(i, 350);
        let w: Word = Integer::from(0xCAFEBA).into();
        assert_eq!(w, [0xCA, 0xFE, 0xBA]);
        let w: Word = Integer::from(-1).into();
        assert_eq!(w, [0xFF, 0xFF, 0xFF]);
        let w: Word = Integer::from(-2).into();
        assert_eq!(w, [0xFF, 0xFF, 0xFE]);
    }

    fn set_int(vm: &mut Vm, address: usize, v: u32) {
        vm.memory[address] = ((v & 0xFF_00_00) >> 16) as u8;
        vm.memory[address + 1] = ((v & 0x00_FF_00) >> 8) as u8;
        vm.memory[address + 2] = (v & 0x00_00_FF) as u8;
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

    #[test]
    fn lda() {
        let mut vm = Vm::empty();
        set_op(
            &mut vm,
            0,
            Op {
                opcode: OpCode::LDA,
                indexed: false,
                address: 3,
            },
        );
        set_int(&mut vm, 3, 0xAB);

        vm.step();

        let a_val: u32 = Integer(vm.A).into();
        assert_eq!(a_val, 0xAB);
        assert_eq!(vm.PC[2], 3);

        let mut vm = Vm::empty();
        vm.X = Integer::from(3).into();
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

        let a_val: u32 = Integer(vm.A).into();
        assert_eq!(a_val, 0xBA);
        assert_eq!(vm.PC[2], 3);
    }

    #[test]
    fn sta() {
        let mut vm = Vm::empty();
        vm.A = Integer::from(0xAAAA).into();
        set_op(
            &mut vm,
            0,
            Op {
                opcode: OpCode::STA,
                indexed: false,
                address: 3,
            },
        );

        vm.step();

        let a_val: u32 = Integer::from(vm.word_at(3, false)).into();
        assert_eq!(a_val, 0xAAAA);
        assert_eq!(vm.PC[2], 3);
    }

    #[test]
    fn load_store() {
        let mut vm = Vm::empty();
        set_op(
            &mut vm,
            0,
            Op {
                opcode: OpCode::LDA,
                indexed: false,
                address: 99,
            },
        );
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

        let stored: u32 = Integer::from(vm.word_at(102, false)).into();
        assert_eq!(stored, 0xBEEF);
    }

    #[test]
    fn ch() {
        let mut vm = Vm::empty();
        set_int(&mut vm, 99, 0xBE00);
        set_op(
            &mut vm,
            0,
            Op {
                opcode: OpCode::LDCH,
                indexed: false,
                address: 100,
            },
        );

        vm.step();

        let a_val: u32 = Integer::from(vm.A).into();
        assert_eq!(a_val, 0xBE);
        let mut vm = Vm::empty();
        vm.A = Integer::from(0xBA).into();
        set_op(
            &mut vm,
            0,
            Op {
                opcode: OpCode::STCH,
                indexed: false,
                address: 100,
            },
        );

        vm.step();

        let m_val: u32 = Integer::from(vm.word_at(98, false)).into();
        assert_eq!(m_val, 0xBA);
    }
}

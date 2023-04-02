use num::FromPrimitive;
use num_derive::FromPrimitive;

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum OneByteOp {
    FIX = 0xC4,
    FLOAT = 0xC0,
    HIO = 0xF4,

    NORM = 0xC8,
    SIO = 0xF0,

    TIO = 0xF8,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum OneRegOp {
    CLEAR = 0xB4,
    TIXR = 0xB8,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TwoRegOp {
    ADDR = 0x90,
    COMPR = 0xA0,
    DIVR = 0x9C,

    MULR = 0x98,
    RMO = 0xAC,

    SUBR = 0x94,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftOp {
    SHIFTL = 0xA4,
    SHIFTR = 0xA8,
}

#[derive(Copy, Clone, Debug, FromPrimitive)]
pub enum Register {
    A = 0,
    X = 1,
    L = 2,
    B = 3,
    S = 4,
    T = 5,
    F = 6,
    PC = 8,
    SW = 9,
}

impl Register {
    fn from_r1(registers: u8) -> Option<Register> {
        FromPrimitive::from_u8(registers >> 4)
    }

    fn from_r2(registers: u8) -> Option<Register> {
        FromPrimitive::from_u8(registers & 0x0F)
    }

    fn r1_with(&self, r2: &Register) -> u8 {
        let r1 = (*self as u8) << 4;
        r1 + ((*r2 as u8) & 0x0F)
    }
}

#[derive(Debug)]
pub struct TwoReg {
    pub opcode: TwoRegOp,
    pub r1: Register,
    pub r2: Register,
}

#[derive(Debug)]
pub struct OneReg {
    pub opcode: OneRegOp,
    pub r1: Register,
}

#[derive(Debug)]
pub struct Shift {
    pub opcode: ShiftOp,
    pub r1: Register,
    pub n: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressMode {
    Compatiblity,
    Simple,
    Immediate,
    Indirect,
}

impl Default for AddressMode {
    fn default() -> Self {
        Self::Simple
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressRelativeTo {
    Direct,
    Base,
    PC,
}

impl Default for AddressRelativeTo {
    fn default() -> Self {
        AddressRelativeTo::Direct
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct AddressFlags {
    pub mode: AddressMode,
    pub relative_to: AddressRelativeTo,
    pub indexed: bool,
    pub extended: bool,
}

impl AddressFlags {
    pub fn from_bytes(bytes: [u8; 2]) -> Self {
        let base_relative = bytes[1] & 0x40 > 0;
        let pc_relative = bytes[1] & 0x20 > 0;
        AddressFlags {
            mode: AddressMode::from_byte(bytes[0]),
            relative_to: match (base_relative, pc_relative) {
                (false, false) | (true, true) => AddressRelativeTo::Direct,
                (true, false) => AddressRelativeTo::Base,
                (false, true) => AddressRelativeTo::PC,
            },
            indexed: bytes[1] & 0x80 > 0,
            extended: bytes[1] & 0x10 > 0,
        }
    }

    // For directly specifying a full address,
    // used in service interrupt handling
    pub fn immediate() -> Self {
        Self {
            mode: AddressMode::Immediate,
            relative_to: AddressRelativeTo::Direct,
            indexed: false,
            extended: true,
        }
    }
}

impl AddressMode {
    pub fn from_byte(byte: u8) -> Self {
        match byte & 0x03 {
            0x03 => AddressMode::Simple,
            0x02 => AddressMode::Indirect,
            0x01 => AddressMode::Immediate,
            0x00 => AddressMode::Compatiblity,
            _ => unreachable!("0x03 only has 3 options"),
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    pub opcode: VariableOp,
    pub address_flags: AddressFlags,
    pub address: u32,
}

const SVC: u8 = 0xB0;

#[derive(Debug)]
pub enum Op {
    OneByte(OneByteOp),
    OneReg(OneReg),
    TwoReg(TwoReg),
    Shift(Shift),
    Svc(u8),
    Variable(Variable),
}

fn calc_address(bytes: [u8; 4], flags: &AddressFlags) -> u32 {
    match flags.mode {
        // Compatibility is always direct
        AddressMode::Compatiblity => (((bytes[1] & 0x7F) as u32) << 8) + bytes[2] as u32,
        _ => {
            if flags.extended {
                // Extended is always direct
                (((bytes[1] & 0x0F) as u32) << 16) + ((bytes[2] as u32) << 8) + bytes[3] as u32
            } else {
                (((bytes[1] & 0x0F) as u32) << 8) + bytes[2] as u32
            }
        }
    }
}

#[allow(clippy::len_without_is_empty)]
impl Op {
    pub fn len(&self) -> u32 {
        match self {
            Op::OneByte(_) => 1,
            Op::OneReg(_) => 2,
            Op::TwoReg(_) => 2,
            Op::Shift(_) => 2,
            Op::Svc(_) => 2,
            Op::Variable(Variable {
                address_flags: AddressFlags { mode, extended, .. },
                ..
            }) => {
                if *extended && *mode != AddressMode::Compatiblity {
                    4
                } else {
                    3
                }
            }
        }
    }

    pub fn from_bytes(bytes: [u8; 4]) -> Option<Self> {
        if bytes[0] == SVC {
            return Some(Self::Svc(bytes[1] & 0xF0));
        }
        if let Some(one) = FromPrimitive::from_u8(bytes[0]) {
            return Some(Self::OneByte(one));
        }

        if let Some(one_reg) = FromPrimitive::from_u8(bytes[0]) {
            return Some(Self::OneReg(OneReg {
                opcode: one_reg,
                r1: Register::from_r1(bytes[1])?,
            }));
        }

        if let Some(two) = FromPrimitive::from_u8(bytes[0]) {
            return Some(Self::TwoReg(TwoReg {
                opcode: two,
                r1: Register::from_r1(bytes[1])?,
                r2: Register::from_r2(bytes[1])?,
            }));
        }

        if let Some(shift) = FromPrimitive::from_u8(bytes[0]) {
            return Some(Self::Shift(Shift {
                opcode: shift,
                r1: Register::from_r1(bytes[1])?,
                n: bytes[1] & 0x0F,
            }));
        }

        if let Some(var) = FromPrimitive::from_u8(bytes[0] & 0xFC) {
            let address_flags = AddressFlags::from_bytes([bytes[0], bytes[1]]);
            let address = calc_address(bytes, &address_flags);

            return Some(Self::Variable(Variable {
                opcode: var,
                address_flags,
                address,
            }));
        }

        None
    }

    pub fn to_bytes(&self) -> [u8; 4] {
        match self {
            Op::OneByte(opcode) => [*opcode as u8, 0, 0, 0],
            Op::OneReg(or) => [or.opcode as u8, or.r1.r1_with(&Register::A), 0, 0],
            Op::TwoReg(tb) => [tb.opcode as u8, tb.r1.r1_with(&tb.r2), 0, 0],
            Op::Shift(shift) => [
                shift.opcode as u8,
                ((shift.r1 as u8) << 4) + (shift.n & 0x0F),
                0,
                0,
            ],
            Op::Svc(n) => [SVC, n & 0xF0, 0, 0],
            Op::Variable(var) => {
                let opcode = var.opcode as u8;
                let x = if var.address_flags.indexed {
                    0x80
                } else {
                    0x00
                };
                let b = if var.address_flags.relative_to == AddressRelativeTo::Base {
                    0x40
                } else {
                    0x00
                };
                let p = if var.address_flags.relative_to == AddressRelativeTo::PC {
                    0x20
                } else {
                    0x00
                };
                let e = if var.address_flags.extended {
                    0x10
                } else {
                    0x00
                };

                let mode = match var.address_flags.mode {
                    AddressMode::Compatiblity => {
                        let [_, _, b, c] = var.address.to_be_bytes();
                        return [opcode, x + (b & 0x7F), c, 0];
                    }
                    AddressMode::Simple => 0x03,
                    AddressMode::Immediate => 0x01,
                    AddressMode::Indirect => 0x02,
                };

                let a = opcode + mode;

                let flags = x + b + p + e;

                let (b, c, d) = if e > 0 {
                    let [_, b, c, d] = var.address.to_be_bytes();
                    (b & 0x0F, c, d)
                } else {
                    let [_, _, b, c] = var.address.to_be_bytes();
                    (b & 0x0F, c, 0)
                };

                [a, b + flags, c, d]
            }
        }
    }
}

pub fn is_privileged(op: &Op) -> bool {
    match op {
        Op::OneByte(one_byte) => {
            matches!(one_byte, OneByteOp::SIO | OneByteOp::TIO | OneByteOp::HIO)
        }
        Op::OneReg(_) => false,
        Op::TwoReg(_) => false,
        Op::Shift(_) => false,
        Op::Svc(_) => true,
        Op::Variable(Variable { opcode, .. }) => matches!(
            opcode,
            VariableOp::LPS
                | VariableOp::RD
                | VariableOp::SSK
                | VariableOp::STI
                | VariableOp::STSW
                | VariableOp::TD
                | VariableOp::WD
        ),
    }
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableOp {
    ADD = 0x18,
    ADDF = 0x58,
    AND = 0x40,
    COMP = 0x28,
    COMPF = 0x88,
    DIV = 0x24,
    DIVF = 0x64,
    J = 0x3C,
    JEQ = 0x30,
    JGT = 0x34,
    JLT = 0x38,
    JSUB = 0x48,
    LDA = 0x00,
    LDB = 0x68,
    LDCH = 0x50,
    LDF = 0x70,
    LDL = 0x08,
    LDS = 0x6C,
    LDT = 0x74,
    LDX = 0x04,
    LPS = 0xD0,
    MUL = 0x20,

    MULF = 0x60,
    OR = 0x44,
    RD = 0xD8,
    RSUB = 0x4C,
    SSK = 0xEC,
    STA = 0x0C,
    STB = 0x78,
    STCH = 0x54,
    STF = 0x80,
    STI = 0xD4,
    STL = 0x14,
    STS = 0x7C,
    STSW = 0xE8,
    STT = 0x84,
    STX = 0x10,
    SUB = 0x1C,
    SUBF = 0x5C,

    TD = 0xE0,
    TIX = 0x2C,
    WD = 0xDC,
}

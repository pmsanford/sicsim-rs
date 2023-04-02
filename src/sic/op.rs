use num_derive::FromPrimitive;

use crate::word::Word;

#[derive(FromPrimitive, Debug, Clone, Copy)]
pub enum OpCode {
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
pub struct Op {
    pub opcode: OpCode,
    pub indexed: bool,
    pub address: u16,
}

impl Op {
    pub fn from_word(word: Word) -> Option<Self> {
        num::FromPrimitive::from_u8(word[0]).map(|opcode| Self {
            opcode,
            indexed: word[1] & 0b1000_0000 > 0,
            address: u16::from_be_bytes([word[1] & 0x7F, word[2]]),
        })
    }
}

// Can't implement anything on Word
#[allow(clippy::from_over_into)]
impl Into<Word> for Op {
    fn into(self) -> Word {
        let opcode = self.opcode as u8;
        let [msb, lsb] = self.address.to_be_bytes();
        let mut msb = msb & 0x7F;
        if self.indexed {
            msb |= 0x80;
        }

        [opcode, msb, lsb]
    }
}

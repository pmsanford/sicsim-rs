use std::cmp::Ordering;

use crate::word::Word;

pub const CC_MASK: u8 = 0x03;
pub const CC_LT: u8 = 0x01;
pub const CC_GT: u8 = 0x02;
pub const CC_EQ: u8 = 0x00;
pub const CC_BYTE: usize = 0;

pub fn set_cc(sw: &mut Word, val: Ordering) {
    sw[CC_BYTE] = (sw[CC_BYTE] & (CC_MASK ^ 0xFF))
        | match val {
            Ordering::Less => CC_LT,
            Ordering::Equal => CC_EQ,
            Ordering::Greater => CC_GT,
        };
}

pub fn check_cc(sw: &Word) -> Ordering {
    let cc = sw[CC_BYTE] & CC_MASK;
    match cc {
        CC_LT => Ordering::Less,
        CC_EQ => Ordering::Equal,
        CC_GT => Ordering::Greater,
        // All possibilities covered assuming the mask is correct
        _ => unreachable!(),
    }
}

pub const SUP_MASK: u8 = 0x80;
pub const SUP_BYTE: usize = 0;

pub fn supervisor_mode(sw: &Word) -> bool {
    sw[SUP_BYTE] & SUP_MASK == SUP_MASK
}

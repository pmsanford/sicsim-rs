pub type Word = [u8; 3];

pub trait WordExt {
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

pub fn u32_to_word(i: u32) -> Word {
    let [_, a, b, c] = i.to_be_bytes();
    [a, b, c]
}

pub fn u16_to_word(i: u16) -> Word {
    let [b, c] = i.to_be_bytes();
    [0, b, c]
}

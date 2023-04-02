pub type DWord = [u8; 6];
pub type Word = [u8; 3];

pub trait DWordExt {
    fn as_f64(&self) -> f64;
    fn as_u64(&self) -> u64;
    fn as_usize(&self) -> usize;
    fn as_i64(&self) -> i64;
}

const U64_TOP_BIT: u64 = 0x8000000000000000;

impl DWordExt for DWord {
    // The SIC/XE float format is a 48 bit float
    // It consists of a 1 bit sign, 11 bit exponent, and 36 bit fraction
    // ____________________________________________________
    // |1|    11     |                36                  |
    // ----------------------------------------------------
    // In IEEE floats, which modern computers use almost universally, the
    // fraction is between 1 and 2. In SIC/XE the fraction is between 0 and 1.
    // I imagine Beck did this intentionally so that you can't just stuff the
    // fraction into an IEEE float and instead have to do this math yourself.
    fn as_f64(&self) -> f64 {
        let sign = 1 + (((self[0] & 0x80) / 0x80) as i8 * -2);
        let upper_exponent = self[0] & 0x7F;
        let lower_exponent = (self[1] & 0xF0) >> 4;
        let exponent = u16::from_be_bytes([upper_exponent, lower_exponent]);
        let exponent = exponent as i16 - 1024;

        // Now for the fun part
        let fraction =
            u64::from_be_bytes([0, 0, 0, self[1] & 0x0F, self[2], self[3], self[4], self[5]]);

        // Shift up to the first significant byte
        let fraction = fraction << 28;

        let mut sum = 0f64;

        // The fraction is represented as 0.101011... in base 2.
        // This means that:
        //  0.1 = 2^(-1) = 0.5
        //  0.101 = 2^(-1) + 2^(-3) = 0.5 + 0.125 = 0.625
        //  0.11 = 2^(-1) + 2^(-2) = 0.5 + 0.25 = 0.75
        //  ..etc
        // So now we'll iterate through the bits, summing up the exponentiations.
        for i in 0..36 {
            if (fraction << i) & U64_TOP_BIT > 0 {
                sum += 2f64.powf((-(i + 1)) as f64);
            }
        }

        let abs = sum * 2f64.powf(exponent as f64);

        sign as f64 * abs
    }

    fn as_u64(&self) -> u64 {
        let [a, b, c, d, e, f] = *self;
        u64::from_be_bytes([0, 0, a, b, c, d, e, f])
    }

    fn as_usize(&self) -> usize {
        let [a, b, c, d, e, f] = *self;
        usize::from_be_bytes([0, 0, a, b, c, d, e, f])
    }

    fn as_i64(&self) -> i64 {
        let [a, b, c, d, e, f] = *self;
        let neg = a & 0x80 > 0;
        let a = a & 0x7F;
        let val = u64::from_be_bytes([0, 0, a, b, c, d, e, f]);
        if neg {
            // Don't do any twos complement just sign extend baybee
            (val | 0xFF_FF_80_00_00_00_00_00) as i64
        } else {
            val as i64
        }
    }
}

//TODO: Handle running out of precision
fn frac_to_binary(fraction: f64) -> u64 {
    let fraction = fraction.abs();
    let fraction = fraction - (fraction as u64) as f64;
    if fraction == 0.0 {
        return 0;
    }
    let mut current = fraction;
    let mut binary = 0;
    let mut counter = 0;
    while current != 0.0 && counter < 36 {
        current *= 2.0;
        binary <<= 1;
        counter += 1;
        if current >= 1.0 {
            binary += 1;
        }
        current %= 1.0;
    }

    binary <<= 64 - counter;

    binary
}

pub fn u32_to_dword(v: u32) -> DWord {
    let [a, b, c, d] = v.to_be_bytes();
    [0, 0, a, b, c, d]
}

pub fn i16_to_exponent(i: i16) -> [u8; 2] {
    let [exp_u, exp_l] = (i + 1024).to_be_bytes();
    [exp_u & 0x7F, exp_l << 4]
}

//TODO: Handle running out of precision
// This is the reverse of the f64 conversion
pub fn f64_to_dword(f: f64) -> DWord {
    if f == 0.0 {
        return [0, 0, 0, 0, 0, 0];
    } else if f == -0.0 {
        // :(
        return [0x80, 0, 0, 0, 0, 0];
    }

    if !f.is_normal() {
        panic!("Punting on this for now");
    }

    let sign = if f.is_sign_negative() { 0x80 } else { 0 };
    // This is the part to the left of the decimal
    let integer = f.abs() as u64;
    // This is the part to the right
    let fraction = frac_to_binary(f);
    let mut exponent: i16 = 0;

    let lower_bytes = if integer > 0 {
        // First case: our number is >= 1
        // We have to get everything to the right of the decimal
        let mut shifted = integer;

        // We count how many shifts we need to get rid of all the bits
        // we have in the integer portion.
        while shifted > 0 {
            shifted >>= 1;
            exponent += 1;
        }
        // We offset by -4 here to account for the half byte taken up by the exponent in our
        // representaiton
        let [a, b, c, d, e, _, _, _] =
            ((integer << (64 - 4 - exponent)) + (fraction >> (exponent + 4))).to_be_bytes();
        [a, b, c, d, e]
    } else {
        // Second case: our number is < 1
        let mut shifted = fraction;

        // We rely on shifting everything off the left end
        while shifted & U64_TOP_BIT == 0 {
            shifted <<= 1;
            exponent -= 1;
        }

        // We offset by -4 here to account for the half byte taken up by the exponent in our
        // representaiton
        let [a, b, c, d, e, _, _, _] = ((fraction >> 4) << exponent.abs()).to_be_bytes();
        [a, b, c, d, e]
    };

    let [exp_u, exp_l] = i16_to_exponent(exponent);

    [
        sign + exp_u,
        exp_l + lower_bytes[0],
        lower_bytes[1],
        lower_bytes[2],
        lower_bytes[3],
        lower_bytes[4],
    ]
}

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
        let val = u32::from_be_bytes([0, msb, mid, lsb]);
        if neg {
            // Don't do any twos complement just sign extend baybee
            (val | 0xFF_80_00_00) as i32
        } else {
            val as i32
        }
    }
}

pub fn i32_to_word(i: i32) -> Word {
    let [_, a, b, c] = i.to_be_bytes();
    [a, b, c]
}

pub fn u32_to_word(i: u32) -> Word {
    let [_, a, b, c] = i.to_be_bytes();
    [a, b, c]
}

pub fn u16_to_word(i: u16) -> Word {
    let [b, c] = i.to_be_bytes();
    [0, b, c]
}

pub fn u8_to_word(i: u8) -> Word {
    [0, 0, i]
}

pub fn format_dword(dword: DWord) -> String {
    let sign = if dword[0] & 0x80 > 0 { 1 } else { 0 };
    let exponent = u16::from_be_bytes([dword[0] & 0x7F, (dword[1] & 0xF0) >> 4]) - 1024;
    let fraction = u64::from_be_bytes([
        0,
        0,
        0,
        dword[1] & 0x0F,
        dword[2],
        dword[3],
        dword[4],
        dword[5],
    ]);
    format!("{:1b} {:0>11b} {:0>36b}", sign, exponent, fraction)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn f48() {
        let i = -1.625;
        let dword = f64_to_dword(i);
        let back = dword.as_f64();

        assert_eq!(i, back);
    }

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
        let i: i32 = u32_to_word((-5_i32) as u32).as_i32();
        assert_eq!(i, -5);
        let i: i32 = u32_to_word(350).as_i32();
        assert_eq!(i, 350);
        let w: Word = u32_to_word(0xCAFEBA);
        assert_eq!(w, [0xCA, 0xFE, 0xBA]);
        let w: Word = u32_to_word((-1_i32) as u32);
        assert_eq!(w, [0xFF, 0xFF, 0xFF]);
        let w: Word = u32_to_word((-2_i32) as u32);
        assert_eq!(w, [0xFF, 0xFF, 0xFE]);
    }
}

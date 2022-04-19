use libsic::xe::op::{OneByteOp, OneRegOp, ShiftOp, TwoRegOp, VariableOp};

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assembler {
    START,
    BASE,
    BYTE,
    LTORG,
    EQU,
    WORD,
    RESW,
    RESB,
    END,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Directive {
    Assembler(Assembler),
    OneByte(OneByteOp),
    OneReg(OneRegOp),
    TwoReg(TwoRegOp),
    Shift(ShiftOp),
    SVC,
    Variable(VariableOp),
}

impl Directive {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "START" => Some(Self::Assembler(Assembler::START)),
            "BASE" => Some(Self::Assembler(Assembler::BASE)),
            "BYTE" => Some(Self::Assembler(Assembler::BYTE)),
            "WORD" => Some(Self::Assembler(Assembler::WORD)),
            "LTORG" => Some(Self::Assembler(Assembler::LTORG)),
            "EQU" => Some(Self::Assembler(Assembler::EQU)),
            "RESW" => Some(Self::Assembler(Assembler::RESW)),
            "RESB" => Some(Self::Assembler(Assembler::RESB)),
            "END" => Some(Self::Assembler(Assembler::END)),

            "FIX" => Some(Self::OneByte(OneByteOp::FIX)),
            "FLOAT" => Some(Self::OneByte(OneByteOp::FLOAT)),
            "HIO" => Some(Self::OneByte(OneByteOp::HIO)),
            "NORM" => Some(Self::OneByte(OneByteOp::NORM)),
            "SIO" => Some(Self::OneByte(OneByteOp::SIO)),
            "TIO" => Some(Self::OneByte(OneByteOp::TIO)),

            "CLEAR" => Some(Self::OneReg(OneRegOp::CLEAR)),
            "TIXR" => Some(Self::OneReg(OneRegOp::TIXR)),

            "ADDR" => Some(Self::TwoReg(TwoRegOp::ADDR)),
            "COMPR" => Some(Self::TwoReg(TwoRegOp::COMPR)),
            "DIVR" => Some(Self::TwoReg(TwoRegOp::DIVR)),
            "MULR" => Some(Self::TwoReg(TwoRegOp::MULR)),
            "RMO" => Some(Self::TwoReg(TwoRegOp::RMO)),
            "SUBR" => Some(Self::TwoReg(TwoRegOp::SUBR)),

            "SHIFTL" => Some(Self::Shift(ShiftOp::SHIFTL)),
            "SHIFTR" => Some(Self::Shift(ShiftOp::SHIFTR)),

            "ADD" => Some(Self::Variable(VariableOp::ADD)),
            "ADDF" => Some(Self::Variable(VariableOp::ADDF)),
            "AND" => Some(Self::Variable(VariableOp::AND)),
            "COMP" => Some(Self::Variable(VariableOp::COMP)),
            "COMPF" => Some(Self::Variable(VariableOp::COMPF)),
            "DIV" => Some(Self::Variable(VariableOp::DIV)),
            "DIVF" => Some(Self::Variable(VariableOp::DIVF)),
            "J" => Some(Self::Variable(VariableOp::J)),
            "JEQ" => Some(Self::Variable(VariableOp::JEQ)),
            "JGT" => Some(Self::Variable(VariableOp::JGT)),
            "JLT" => Some(Self::Variable(VariableOp::JLT)),
            "JSUB" => Some(Self::Variable(VariableOp::JSUB)),
            "LDA" => Some(Self::Variable(VariableOp::LDA)),
            "LDB" => Some(Self::Variable(VariableOp::LDB)),
            "LDCH" => Some(Self::Variable(VariableOp::LDCH)),
            "LDF" => Some(Self::Variable(VariableOp::LDF)),
            "LDL" => Some(Self::Variable(VariableOp::LDL)),
            "LDS" => Some(Self::Variable(VariableOp::LDS)),
            "LDT" => Some(Self::Variable(VariableOp::LDT)),
            "LDX" => Some(Self::Variable(VariableOp::LDX)),
            "LPS" => Some(Self::Variable(VariableOp::LPS)),
            "MUL" => Some(Self::Variable(VariableOp::MUL)),

            "MULF" => Some(Self::Variable(VariableOp::MULF)),
            "OR" => Some(Self::Variable(VariableOp::OR)),
            "RD" => Some(Self::Variable(VariableOp::RD)),
            "RSUB" => Some(Self::Variable(VariableOp::RSUB)),
            "SSK" => Some(Self::Variable(VariableOp::SSK)),
            "STA" => Some(Self::Variable(VariableOp::STA)),
            "STB" => Some(Self::Variable(VariableOp::STB)),
            "STCH" => Some(Self::Variable(VariableOp::STCH)),
            "STF" => Some(Self::Variable(VariableOp::STF)),
            "STI" => Some(Self::Variable(VariableOp::STI)),
            "STL" => Some(Self::Variable(VariableOp::STL)),
            "STS" => Some(Self::Variable(VariableOp::STS)),
            "STSW" => Some(Self::Variable(VariableOp::STSW)),
            "STT" => Some(Self::Variable(VariableOp::STT)),
            "STX" => Some(Self::Variable(VariableOp::STX)),
            "SUB" => Some(Self::Variable(VariableOp::SUB)),
            "SUBF" => Some(Self::Variable(VariableOp::SUBF)),

            "TD" => Some(Self::Variable(VariableOp::TD)),
            "TIX" => Some(Self::Variable(VariableOp::TIX)),
            "WD" => Some(Self::Variable(VariableOp::WD)),

            "SVC" => Some(Self::SVC),

            _ => None,
        }
    }
}

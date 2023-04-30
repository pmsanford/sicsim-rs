use std::str::FromStr;

use libsic::xe::op::{OneByteOp, OneRegOp, ShiftOp, TwoRegOp, VariableOp};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric0, digit1, space1},
    combinator::{map, opt, recognize},
    error::ErrorKind,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use strum::EnumString;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Invalid opcode")]
    InvalidOpcode(String),
}

#[derive(Debug, EnumString)]
pub enum Assembler {
    START,
    BASE,
    BYTE,
    LTORG,
    ORG,
    EQU,
    WORD,
    USE,
    RESW,
    RESB,
    END,
}

fn command(i: &str) -> IResult<&str, Assembler> {
    let (i, cmd) = take_while1(|c: char| c.is_ascii_uppercase())(i)?;
    let cmd = Assembler::from_str(cmd)
        .map_err(|_| nom::Err::Error(nom::error::Error::new(cmd, ErrorKind::Fail)))?;

    Ok((i, cmd))
}

#[derive(Debug)]
pub enum Directive {
    Op(Op),
    Command(Assembler),
}

fn directive(i: &str) -> IResult<&str, Directive> {
    let (i, dir) = alt((map(command, Directive::Command), map(op, Directive::Op)))(i)?;

    Ok((i, dir))
}

#[derive(Debug)]
pub enum Op {
    OneByte(OneByteOp),
    OneReg(OneRegOp),
    TwoReg(TwoRegOp),
    Shift(ShiftOp),
    Svc,
    Variable(VariableOp),
}

impl Op {
    pub fn from_mnemonic(i: &str) -> Option<Op> {
        if let Ok(one) = OneByteOp::from_str(i) {
            Some(Op::OneByte(one))
        } else if let Ok(onereg) = OneRegOp::from_str(i) {
            Some(Op::OneReg(onereg))
        } else if let Ok(tworeg) = TwoRegOp::from_str(i) {
            Some(Op::TwoReg(tworeg))
        } else if let Ok(shift) = ShiftOp::from_str(i) {
            Some(Op::Shift(shift))
        } else if let Ok(variable) = VariableOp::from_str(i) {
            Some(Op::Variable(variable))
        } else if i == "SVC" {
            Some(Op::Svc)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Label(String);
#[derive(Debug)]
pub enum Argument {
    Constant(i32),
    Label(Label),
}

pub fn label(i: &str) -> IResult<&str, Label> {
    let (i, label) = recognize(pair(alpha1, alphanumeric0))(i)?;
    Ok((i, Label(label.into())))
}

pub fn op(i: &str) -> IResult<&str, Op> {
    let (i, op) = take_while1(|c: char| c.is_ascii_uppercase())(i)?;
    let op = Op::from_mnemonic(op)
        .ok_or_else(|| nom::Err::Error(nom::error::Error::new(op, ErrorKind::Fail)))?;

    Ok((i, op))
}

pub fn argument(i: &str) -> IResult<&str, Argument> {
    let aposdelimited = |starts| preceded(starts, delimited(tag("'"), digit1, tag("'")));
    let (i, res): (&str, Argument) = alt((
        map(aposdelimited(tag("X")), |g: &str| {
            Argument::Constant(g.parse().unwrap())
        }),
        map(aposdelimited(tag("C")), |g: &str| {
            Argument::Constant(g.parse().unwrap())
        }),
        map(digit1, |g: &str| Argument::Constant(g.parse().unwrap())),
        map(recognize(pair(alpha1, alphanumeric0)), |g: &str| {
            Argument::Label(Label(g.into()))
        }),
    ))(i)?;

    Ok((i, res))
}

#[derive(Debug)]
pub enum AddressModifier {
    Unmodified,
    Indirect,
    Immediate,
}

pub fn address_modifier(i: &str) -> IResult<&str, AddressModifier> {
    let (i, result) = opt(alt((
        map(tag("@"), |_| AddressModifier::Indirect),
        map(tag("#"), |_| AddressModifier::Immediate),
    )))(i)?;

    Ok((i, result.unwrap_or(AddressModifier::Unmodified)))
}

#[derive(Debug)]
pub struct Line {
    pub label: Option<Label>,
    pub extended: bool,
    pub directive: Directive,
    pub argument: Argument,
    pub address_modifier: AddressModifier,
    pub indexed: bool,
}

pub fn asm_line(i: &str) -> IResult<&str, Line> {
    let (i, (label, (extended, directive), (address_modifier, argument), indexed)) = tuple((
        opt(label),
        preceded(space1, pair(map(opt(tag("+")), |e| e.is_some()), directive)),
        preceded(space1, pair(address_modifier, argument)),
        map(opt(tag(",X")), |o| o.is_some()),
    ))(i)?;

    Ok((
        i,
        Line {
            label,
            extended,
            directive,
            argument,
            address_modifier,
            indexed,
        },
    ))
}

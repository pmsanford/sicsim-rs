use anyhow::{anyhow, Result};
use std::str::FromStr;

use libsic::xe::op::{OneByteOp, OneRegOp, ShiftOp, TwoRegOp, VariableOp};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_until1, take_while1},
    character::complete::{alpha1, alphanumeric0, anychar, digit1, space1},
    combinator::{all_consuming, map, map_parser, map_res, opt, recognize},
    error::ErrorKind,
    multi::{many0, many1},
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

#[derive(Debug, EnumString, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
pub enum ExprOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub fn expr_op(i: &str) -> IResult<&str, ExprOp> {
    alt((
        map(tag("+"), |_| ExprOp::Add),
        map(tag("-"), |_| ExprOp::Subtract),
        map(tag("*"), |_| ExprOp::Multiply),
        map(tag("/"), |_| ExprOp::Divide),
    ))(i)
}

#[derive(Debug)]
pub enum ExprTarget {
    Argument(Value),
    Expr(Box<Expr>),
}

pub fn expr_target(i: &str) -> IResult<&str, ExprTarget> {
    alt((
        map(all_consuming(value), ExprTarget::Argument),
        map(expr, |e| ExprTarget::Expr(Box::new(e))),
    ))(i)
}

#[derive(Debug)]
pub struct Expr {
    pub value: Value,
    pub op: ExprOp,
    pub target: ExprTarget,
}

pub fn expr(i: &str) -> IResult<&str, Expr> {
    let (i, value) = value(i)?;
    let (i, op) = expr_op(i)?;
    let (i, target) = expr_target(i)?;

    Ok((i, Expr { value, op, target }))
}

#[derive(Debug, Eq, PartialEq)]
pub struct Label(String);

#[derive(Debug, Eq, PartialEq)]
pub enum Value {
    Constant(Vec<u8>),
    Label(Label),
}

#[derive(Debug)]
pub enum Argument {
    Value(Value),
    Expr(Expr),
}

pub fn argument(i: &str) -> IResult<&str, Argument> {
    // Note that order is important here - the firs tone to succeed
    // is returned, and label will match an expression that starts with
    // a label.
    alt((map(expr, Argument::Expr), map(value, Argument::Value)))(i)
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

pub fn hex_bytes(i: &str) -> IResult<&str, Vec<u8>> {
    many1(map_res(take(2usize), |b| u8::from_str_radix(b, 16)))(i)
}

pub fn char_bytes(i: &str) -> IResult<&str, Vec<u8>> {
    many1(map(anychar, |c| c as u8))(i)
}

pub fn value(i: &str) -> IResult<&str, Value> {
    let aposdelimited = |starts| preceded(starts, delimited(tag("'"), take_until1("'"), tag("'")));
    // Note that order is important here - label will match a constant
    // that starts with X or C
    let (i, res): (&str, Value) = alt((
        map(
            map_parser(aposdelimited(tag("X")), hex_bytes),
            Value::Constant,
        ),
        map(
            map_parser(aposdelimited(tag("C")), char_bytes),
            Value::Constant,
        ),
        map_res(digit1, |g: &str| {
            let number = g.parse::<i32>()?;
            let mut number = number.to_be_bytes();
            let neg = number[0] & 0x80 > 0;
            if neg {
                number[1] |= 0x80;
            } else {
                number[1] &= 0x7F;
            }
            Ok::<Value, <i32 as FromStr>::Err>(Value::Constant(vec![
                number[1], number[2], number[3],
            ]))
        }),
        map(recognize(pair(alpha1, alphanumeric0)), |g: &str| {
            Value::Label(Label(g.into()))
        }),
    ))(i)?;

    Ok((i, res))
}

#[derive(Debug, Eq, PartialEq)]
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
pub struct AssemblyLine {
    pub label: Option<Label>,
    pub extended: bool,
    pub directive: Directive,
    pub argument: Option<Argument>,
    pub address_modifier: AddressModifier,
    pub indexed: bool,
}

pub fn asm_line(i: &str) -> IResult<&str, AssemblyLine> {
    let (i, (label, (extended, directive), arg_part, _comment)) = tuple((
        opt(label),
        preceded(space1, pair(map(opt(tag("+")), |e| e.is_some()), directive)),
        opt(pair(
            preceded(space1, pair(address_modifier, argument)),
            map(opt(tag(",X")), |o| o.is_some()),
        )),
        opt(preceded(space1, preceded(tag("."), many0(anychar)))),
    ))(i)?;

    let ((address_modifier, argument), indexed) =
        if let Some(((address_modifier, argument), indexed)) = arg_part {
            ((address_modifier, Some(argument)), indexed)
        } else {
            ((AddressModifier::Unmodified, None), false)
        };

    Ok((
        i,
        AssemblyLine {
            label,
            extended,
            directive,
            argument,
            address_modifier,
            indexed,
        },
    ))
}

#[derive(Debug)]
pub struct Comment(String);

#[derive(Debug)]
pub enum ProgramLine {
    Assembly(AssemblyLine),
    Comment(Comment),
    Empty,
}

#[derive(Debug)]
pub struct ParserLine {
    pub data: ProgramLine,
    pub text: String,
    pub line_no: usize,
}

fn parse_line(i: &str, line_no: usize) -> Result<ProgramLine> {
    Ok(if i.is_empty() {
        ProgramLine::Empty
    } else if i.trim_start().starts_with(".") {
        ProgramLine::Comment(Comment(i.into()))
    } else {
        ProgramLine::Assembly(asm_line(i).map_err(|e| anyhow!("[{}] {}", line_no, e))?.1)
    })
}

pub fn parse_program(program: &str) -> Result<Vec<ParserLine>> {
    Ok(program
        .lines()
        .enumerate()
        .map(|(num, text)| Ok((num, text.to_string(), parse_line(text, num)?)))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .map(|(line_no, text, data)| ParserLine {
            data,
            text,
            line_no,
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_line() {
        let test_line = "FST     LDA    SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST    +LDA    SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST     LDA   #SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST     LDA   @SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST     LDA   SND,X";

        assert!(asm_line(test_line).is_ok());

        let test_line = "        LDA   SND,X";

        assert!(asm_line(test_line).is_ok());
    }

    #[test]
    fn parse_program() {
        let program = r#"
DISP    START   100
        LDA    #5
LOOP    MUL    #2
        COMP   @COMPLOC
        JLT     LOOP
END     J       END
SUBR    LDA     MAX
        ADD    #2
        RSUB
COMPLOC WORD    MAX . some comment
SOMTH   BYTE    C'Hello'
SOMET   BYTE    X'0F0A21C3'
MAX     WORD    1000
DODO    EQU     MAX+5*END
        END     DISP
        "#;

        let parsed = program
            .lines()
            .map(str::trim_end)
            .filter(|l| !l.is_empty())
            .map(all_consuming(asm_line))
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .map(|x| x.1)
            .collect::<Vec<_>>();

        assert!(parsed[1].address_modifier == AddressModifier::Immediate);
        assert!(parsed[3].address_modifier == AddressModifier::Indirect);
        assert!(parsed[4].address_modifier == AddressModifier::Unmodified);
        assert!(matches!(parsed[8].argument, None));
        assert!(matches!(
            parsed[9].argument,
            Some(Argument::Value(Value::Label(_)))
        ));
        assert!(matches!(
            parsed[10].argument,
            Some(Argument::Value(Value::Constant(_)))
        ));
        assert!(matches!(
            parsed[11].argument,
            Some(Argument::Value(Value::Constant(_)))
        ));
        assert!(matches!(
            parsed[12].argument,
            Some(Argument::Value(Value::Constant(_)))
        ));
        assert!(matches!(parsed[13].argument, Some(Argument::Expr(_))));
    }

    #[test]
    fn parse_expr() {
        let expr_str = "ABCD+2*3+DDDDD";

        expr(expr_str).unwrap();
    }
}

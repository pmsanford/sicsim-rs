use anyhow::{anyhow, bail, Result};
use num_traits::{FromPrimitive, ToPrimitive};
use serde::{de::Visitor, Deserialize, Serialize};
use std::str::FromStr;
use strum_macros::EnumVariantNames;

use libsic::xe::op::{OneByteOp, OneRegOp, ShiftOp, TwoRegOp, VariableOp, SVC};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_until1, take_while1},
    character::complete::{alpha1, alphanumeric0, alphanumeric1, anychar, digit1, space1},
    combinator::{all_consuming, map, map_parser, map_res, opt, recognize},
    error::ErrorKind,
    multi::{fold_many1, many0, many1},
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

#[derive(
    Serialize, Deserialize, Debug, EnumString, Eq, PartialEq, Clone, Copy, strum_macros::Display,
)]
pub enum Assembler {
    START,
    BASE,
    BYTE,
    CSECT,
    EXTDEF,
    EXTREF,
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

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Directive {
    Op(Op),
    Command(Assembler),
}

fn directive(i: &str) -> IResult<&str, Directive> {
    let (i, dir) = alt((map(command, Directive::Command), map(op, Directive::Op)))(i)?;

    Ok((i, dir))
}

#[derive(Debug, EnumVariantNames, PartialEq, Eq, Clone, Copy)]
pub enum Op {
    OneByte(OneByteOp),
    OneReg(OneRegOp),
    TwoReg(TwoRegOp),
    Shift(ShiftOp),
    Svc,
    Variable(VariableOp),
}

impl Serialize for Op {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::Error;

        let val = match self {
            Op::OneByte(o) => o.to_u8(),
            Op::OneReg(o) => o.to_u8(),
            Op::TwoReg(o) => o.to_u8(),
            Op::Shift(o) => o.to_u8(),
            Op::Svc => Some(SVC),
            Op::Variable(o) => o.to_u8(),
        };

        let Some(val) = val else {
            return Err(Error::custom(format!(
                "couldn't convert op {:?} to u8",
                self
            )));
        };

        serializer.serialize_u8(val)
    }
}

struct OpVisitor;

impl<'de> Visitor<'de> for OpVisitor {
    type Value = Op;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a u8 that matches one of the opcodes")
    }

    fn visit_i32<E>(self, v: i32) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_u8(v as u8)
    }

    fn visit_i64<E>(self, v: i64) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_u8(v as u8)
    }

    fn visit_i8<E>(self, v: i8) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_u8(v as u8)
    }

    fn visit_i16<E>(self, v: i16) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_u8(v as u8)
    }

    fn visit_u16<E>(self, v: u16) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_u8(v as u8)
    }

    fn visit_u32<E>(self, v: u32) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_u8(v as u8)
    }

    fn visit_u64<E>(self, v: u64) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_u8(v as u8)
    }

    fn visit_u8<E>(self, v: u8) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if let Some(one) = OneByteOp::from_u8(v) {
            return Ok(Op::OneByte(one));
        }
        if let Some(one) = OneRegOp::from_u8(v) {
            return Ok(Op::OneReg(one));
        }
        if let Some(one) = TwoRegOp::from_u8(v) {
            return Ok(Op::TwoReg(one));
        }
        if let Some(one) = ShiftOp::from_u8(v) {
            return Ok(Op::Shift(one));
        }
        if let Some(one) = VariableOp::from_u8(v) {
            return Ok(Op::Variable(one));
        }
        if v == SVC {
            return Ok(Op::Svc);
        }
        Err(serde::de::Error::custom("couldn't convert {v} to op"))
    }
}

impl<'de> Deserialize<'de> for Op {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_u8(OpVisitor)
    }
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

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Copy)]
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

#[derive(Serialize, Deserialize, Debug, Clone)]
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

#[derive(Serialize, Deserialize, Debug, Clone)]
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

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub struct Label(pub String);

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Bytes(Vec<u8>),
    Chars(Vec<u8>),
    Number(i32),
    String(Label),
    List(Vec<String>),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Argument {
    Value(Value),
    Expr(Expr),
    ExprCurrentOffset,
}

pub trait Arguments {
    fn string(&self) -> Result<String>;
    fn literal(&self) -> Result<Vec<u8>>;
}

impl Arguments for Option<Argument> {
    fn string(&self) -> Result<String> {
        let Some(Argument::Value(Value::String(l))) = self else {
            bail!("expected string");
        };

        Ok(l.0.clone())
    }

    fn literal(&self) -> Result<Vec<u8>> {
        let Some(Argument::Value(Value::Bytes(v) | Value::Chars(v))) = self else {
            bail!("expected literal");
        };

        Ok(v.clone())
    }
}

fn list_item(i: &str) -> IResult<&str, String> {
    let (i, s) = alpha1(i)?;
    let (i, _) = tag(",")(i)?;

    Ok((i, s.to_owned()))
}

fn comma_list(i: &str) -> IResult<&str, Vec<String>> {
    let (i, mut items) = fold_many1(list_item, Vec::new, |mut acc: Vec<_>, item| {
        acc.push(item);
        acc
    })(i)?;
    // This is alphanumeric to support shift
    let (i, last) = alphanumeric1(i)?;

    items.push(last.to_owned());

    Ok((i, items))
}

pub fn argument(i: &str) -> IResult<&str, Argument> {
    // Note that order is important here - the first one to succeed
    // is returned, and label will match an expression that starts with
    // a label.
    alt((
        map(tag("*"), |_| Argument::ExprCurrentOffset),
        map(expr, Argument::Expr),
        map(value, Argument::Value),
    ))(i)
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
    let aposdelimited = |starts| {
        preceded(
            opt(tag("=")),
            preceded(starts, delimited(tag("'"), take_until1("'"), tag("'"))),
        )
    };
    // Note that order is important here - label will match a constant
    // that starts with X or C
    let (i, res): (&str, Value) = alt((
        map(map_parser(aposdelimited(tag("X")), hex_bytes), Value::Bytes),
        map(
            map_parser(aposdelimited(tag("C")), char_bytes),
            Value::Chars,
        ),
        map_res(digit1, |g: &str| {
            let number = g.parse::<i32>()?;
            Ok::<Value, <i32 as FromStr>::Err>(Value::Number(number))
        }),
        map(comma_list, Value::List),
        map(recognize(pair(alpha1, alphanumeric0)), |g: &str| {
            Value::String(Label(g.into()))
        }),
    ))(i)?;

    Ok((i, res))
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Copy)]
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
}

impl AssemblyLine {
    pub fn size(&self) -> Result<usize> {
        Ok(match &self.directive {
            Directive::Op(op) => match op {
                Op::OneByte(_) => 1,
                Op::OneReg(_) | Op::TwoReg(_) | Op::Shift(_) | Op::Svc => 2,
                Op::Variable(_) => {
                    if self.extended {
                        4
                    } else {
                        3
                    }
                }
            },
            Directive::Command(cmd) => match cmd {
                Assembler::START
                | Assembler::BASE
                | Assembler::ORG
                | Assembler::EQU
                | Assembler::USE
                | Assembler::END
                | Assembler::EXTDEF
                | Assembler::EXTREF
                | Assembler::CSECT => 0,
                Assembler::WORD => 3,
                Assembler::RESW => match &self.argument {
                    Some(Argument::Value(Value::Number(n))) => *n as usize * 3,
                    Some(Argument::Expr(_e)) => todo!(),
                    _ => bail!("invalid resw argument"),
                },
                Assembler::RESB => match &self.argument {
                    Some(Argument::Value(Value::Number(n))) => *n as usize,
                    Some(Argument::Expr(_e)) => todo!(),
                    _ => bail!("invalid resb argument"),
                },
                Assembler::BYTE => match &self.argument {
                    Some(Argument::Value(Value::Bytes(v) | Value::Chars(v))) => v.len(),
                    Some(Argument::Value(Value::Number(_))) => 3,
                    _ => bail!("invalid byte argument"),
                },
                Assembler::LTORG => bail!("ltorg size is calculated in pass one"),
            },
        })
    }
}

pub fn asm_line(i: &str) -> IResult<&str, AssemblyLine> {
    let (i, (label, (extended, directive), arg_part, _comment)) = tuple((
        opt(label),
        preceded(space1, pair(map(opt(tag("+")), |e| e.is_some()), directive)),
        opt(preceded(space1, pair(address_modifier, argument))),
        opt(preceded(space1, preceded(tag("."), many0(anychar)))),
    ))(i)?;

    let (address_modifier, argument) = if let Some((address_modifier, argument)) = arg_part {
        (address_modifier, Some(argument))
    } else {
        (AddressModifier::Unmodified, None)
    };

    Ok((
        i,
        AssemblyLine {
            label,
            extended,
            directive,
            argument,
            address_modifier,
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
    Ok(if i.trim().is_empty() {
        ProgramLine::Empty
    } else if i.trim_start().starts_with('.') {
        ProgramLine::Comment(Comment(i.into()))
    } else {
        ProgramLine::Assembly(
            asm_line(i.trim_end())
                .map_err(|e| anyhow!("[{}] {}", line_no, e))?
                .1,
        )
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

pub trait AsmArg {
    fn expect_string(&self) -> Result<String>;
    fn expect_number(&self) -> Result<u32>;
    fn expect_list(&self) -> Result<Vec<String>>;
    fn as_string(&self) -> Result<String>;
}

impl AsmArg for Option<Argument> {
    fn expect_string(&self) -> Result<String> {
        let Some(Argument::Value(Value::String(s))) = self else {
            bail!("expected string argument");
        };

        Ok(s.0.clone())
    }

    fn expect_number(&self) -> Result<u32> {
        let Some(Argument::Value(Value::Number(i))) = self else {
            bail!("expected string argument");
        };

        Ok(*i as u32)
    }
    fn expect_list(&self) -> Result<Vec<String>> {
        Ok(match self {
            Some(Argument::Value(Value::List(l))) => l.clone(),
            Some(Argument::Value(Value::String(s))) => {
                vec![s.0.clone()]
            }
            _ => bail!("expected list argument"),
        })
    }

    fn as_string(&self) -> Result<String> {
        let Some(Argument::Value(v)) = self else {
            bail!("expected value argument");
        };

        Ok(match v {
            Value::Number(n) => n.to_string(),
            Value::String(s) => s.0.clone(),
            _ => bail!("expected number or string"),
        })
    }
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
        assert!(parsed[8].argument.is_none());
        assert!(matches!(
            parsed[9].argument,
            Some(Argument::Value(Value::String(_)))
        ));
        assert!(matches!(
            parsed[10].argument,
            Some(Argument::Value(Value::Chars(_)))
        ));
        assert!(matches!(
            parsed[11].argument,
            Some(Argument::Value(Value::Bytes(_)))
        ));
        assert!(matches!(
            parsed[12].argument,
            Some(Argument::Value(Value::Number(_)))
        ));
        assert!(matches!(parsed[13].argument, Some(Argument::Expr(_))));
    }

    #[ignore = "TODO: Fix exprs with space after"]
    #[test]
    fn parse_line_expr_with_comment() {
        let program = r#"
TST     START   100
LB      EQU     LB1+5 . something
        END     TST
        "#
        .trim();

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

        let line = &parsed[1];
        let arg = line.argument.as_ref().unwrap();
        assert!(matches!(arg, Argument::Expr(_)));
    }

    #[test]
    fn parse_simple_expr_label() {
        let expr_str = "LB1+5";

        let (s, res) = argument(expr_str).unwrap();

        assert!(matches!(res, Argument::Expr(_)));
        assert!(s.is_empty());
    }

    #[test]
    fn parse_expr() {
        let expr_str = "ABCD+2*3+DDDDD";

        expr(expr_str).unwrap();
    }

    #[test]
    fn parse_eq_lit_line() {
        let line = "ENDFIL   LDA      =C'EOF'              INSERT END OF FILE MARKER";

        let line = asm_line(line).unwrap().1;

        let arg = line.argument.unwrap();

        let Argument::Value(Value::Chars(v)) = arg else {
            panic!("expected value");
        };

        assert_eq!(v, vec![b'E', b'O', b'F']);
    }
}

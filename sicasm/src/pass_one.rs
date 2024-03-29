use indexmap::IndexMap as HashMap;

use anyhow::{ensure, Context, Result};

use libsic::xe::op::{AddressFlags, AddressMode, AddressRelativeTo, VariableOp};
use regex::Regex;

pub struct ProgramBlock {
    cur_offset: usize,
    labels: Labels,
    littab: HashMap<LiteralArgument, Literal>,
    ltorgs: HashMap<usize, Ltorg>,
}

#[derive(Debug, Clone)]
pub struct Literal {
    offset: Option<usize>,
    value: Vec<u8>,
}

pub struct FirstPass {
    current: String,
    blocks: HashMap<String, ProgramBlock>,
}

pub struct PassOne {
    pub parsed_lines: Vec<ParsedLine>,
    pub labels: Labels,
    pub littab: HashMap<LiteralArgument, usize>,
    pub ltorgs: HashMap<usize, Ltorg>,
    pub final_ltorg: Ltorg,
}

use crate::{
    constants::{line_regex, lit_regex},
    directive::{Assembler, Directive},
    labels::{Label, Labels},
};

static MAX_DISP: u16 = 4095; // 0x0F_FF
static MAX_PC: i32 = 2047; // 0x07_FF
static MIN_PC: i32 = -2048; // 0x08_00

#[derive(Debug, Clone)]
pub struct ParsedLine {
    pub block: String,
    pub label: Option<String>,
    pub directive: Directive,
    pub argument: ArgumentToken,
    pub extended: bool,
    pub size: usize,
    pub offset: usize,
    pub line_no: usize,
    pub text: String,
}

enum Target {
    Constant,
    Label,
}

impl ParsedLine {
    //TODO: Is the ltorg handling here wrt target type correct?
    fn target(&self, labels: &Labels) -> Result<Option<(Target, usize)>> {
        if let ArgumentToken::String(argument) = self.argument.clone() {
            if argument.mode == AddressMode::Immediate
                && Regex::new("[0-9][0-9]*")?.is_match(&argument.arg_string)
            {
                return Ok(Some((
                    Target::Constant,
                    argument.arg_string.parse().map_err(|e| {
                        anyhow::Error::msg("Couldn't parse immediate arg").context(e)
                    })?,
                )));
            }

            Ok(Some((
                Target::Label,
                labels.absolute(&argument.arg_string)?,
            )))
        } else {
            Ok(None)
        }
    }

    pub fn get_argument(&self) -> Result<&str> {
        Ok(&self.argument.expect_string()?.arg_string)
    }

    pub fn parse_variable_flags(
        &self,
        base: Option<usize>,
        labels: &Labels,
        littab: &HashMap<LiteralArgument, usize>,
    ) -> Result<(u32, AddressFlags)> {
        ensure!(
            matches!(self.directive, Directive::Variable(_)),
            "only for variable ops"
        );
        let mode = self
            .argument
            .get_string()
            .map_or(AddressMode::Simple, |arg| arg.mode);

        let indexed = self.argument.get_string().map_or(false, |arg| arg.indexed);

        let target = if let ArgumentToken::Literal(ref la) = self.argument {
            Some((
                Target::Constant,
                *littab.get(la).ok_or_else(|| {
                    anyhow::Error::msg(format!("Couldn't find literal argument {:?}", la))
                })?,
            ))
        } else {
            self.target(labels)?
        };

        let target = if target.is_none() && self.directive == Directive::Variable(VariableOp::RSUB)
        {
            (Target::Constant, 0)
        } else {
            target.ok_or_else(|| anyhow::Error::msg("Expected target"))?
        };

        let (target_type, target) = target;

        let pc = self.offset + 3;

        let pc_disp = target as i64 - pc as i64;
        let base_disp = base
            .filter(|base| target >= *base)
            .map(|base| target - base);

        let (disp, relative_to) = match (mode, target_type, self.extended, pc_disp, base_disp) {
            (AddressMode::Immediate, Target::Constant, _, _, _) | (_, _, true, _, _) => {
                (target, AddressRelativeTo::Direct)
            }
            (_, _, _, pcd, _) if i64::from(MIN_PC) < pcd && pcd < i64::from(MAX_PC) =>
            {
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                (pc_disp as usize, AddressRelativeTo::PC)
            }
            (_, _, _, _, Some(bd)) if bd < MAX_DISP as usize => (bd, AddressRelativeTo::Base),
            _ if target < MAX_DISP as usize => (target, AddressRelativeTo::Direct),
            _ => {
                return Err(anyhow::Error::msg(
                    "couldn't find addressing mode for target",
                ))
            }
        };

        #[allow(clippy::cast_possible_truncation)]
        Ok((
            disp as u32,
            AddressFlags {
                mode,
                relative_to,
                indexed,
                extended: self.extended,
            },
        ))
    }
}

impl FirstPass {
    fn new() -> Self {
        Self {
            current: "".to_owned(),
            blocks: [("".to_owned(), ProgramBlock::new())].into(),
        }
    }

    pub fn parse_lines(lines: &[&str]) -> Result<PassOne> {
        let mut pass = Self::new();
        let lines = lines
            .iter()
            .enumerate()
            .filter_map(|(line_no, line)| {
                pass.parse_line(line, line_no)
                    .context(format!("Parse error on line {}", line_no))
                    .transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;

        let block = pass
            .blocks
            .get_mut("")
            .ok_or_else(|| anyhow::Error::msg("Couldn't find default block"))?;
        let final_ltorg = block.create_final_ltorg();
        let littab = block.create_littab()?;

        Ok(PassOne {
            parsed_lines: lines,
            labels: block.labels.clone(),
            littab,
            ltorgs: block.ltorgs.clone(),
            final_ltorg,
        })
    }

    fn parse_line(&mut self, line: &str, line_no: usize) -> Result<Option<ParsedLine>> {
        let line = self
            .blocks
            .get_mut(&self.current)
            .ok_or_else(|| anyhow::Error::msg(format!("Couldn't find block {}", self.current)))?
            .parse_line(&self.current, line, line_no);

        if let Ok(Some(ref line)) = line {
            if matches!(line.directive, Directive::Assembler(Assembler::USE)) {
                if let Some(arg) = line.argument.get_string() {
                    if self.blocks.get(&arg.arg_string).is_none() {
                        self.blocks
                            .insert(arg.arg_string.clone(), ProgramBlock::new());
                    }
                }
                self.current = line
                    .argument
                    .get_string()
                    .map(|s| s.arg_string.clone())
                    .unwrap_or_else(|| "".to_owned());
            }
        }

        line
    }
}

impl ProgramBlock {
    fn new() -> Self {
        Self {
            cur_offset: 0,
            labels: Labels::new(),
            littab: HashMap::new(),
            ltorgs: HashMap::new(),
        }
    }

    fn create_littab(&mut self) -> Result<HashMap<LiteralArgument, usize>> {
        self.littab
            .clone()
            .into_iter()
            .map(|(k, v)| {
                let addr = v.offset.ok_or_else(|| {
                    anyhow::Error::msg(format!("Literal {:?} missing address", k))
                })?;
                Ok::<_, anyhow::Error>((k, addr))
            })
            .collect::<Result<HashMap<LiteralArgument, usize>, _>>()
    }

    fn create_final_ltorg(&mut self) -> Ltorg {
        address_literals(self.cur_offset, &mut self.littab)
    }

    fn label_val(&self, label: &str) -> Result<usize> {
        Ok(if label == "*" {
            self.cur_offset
        } else if label.chars().all(char::is_numeric) {
            label.parse::<usize>()?
        } else {
            self.labels.absolute(label)?
        })
    }

    fn calc_expr(&self, argument: &str) -> Result<usize> {
        //TODO: This only supports already defined symbols
        let split = argument.split_inclusive(&['+', '-']).collect::<Vec<_>>();
        let mut components =
            split[..split.len() - 1]
                .iter()
                .fold(Vec::new(), |mut components, component| {
                    components.push(component[..component.len() - 1].trim().to_owned());
                    components.push(component[component.len() - 1..].to_owned());
                    components
                });
        components.push(split[split.len() - 1].to_owned());

        let mut value = self.label_val(&components[0])?;

        for chunk in components[1..].chunks(2) {
            match &*chunk[0] {
                "+" => value += self.label_val(&chunk[1])?,
                "-" => value -= self.label_val(&chunk[1])?,
                _ => return Err(anyhow::Error::msg("Only supports + or -")),
            }
        }

        Ok(value)
    }

    fn parse_line(
        &mut self,
        block: &str,
        line: &str,
        line_no: usize,
    ) -> Result<Option<ParsedLine>> {
        let tokens = LineTokens::from_line(line)?;
        tokens
            .map(|tokens| {
                if tokens.directive == Directive::Assembler(Assembler::ORG) {
                    self.cur_offset =
                        self.calc_expr(&tokens.argument.expect_string()?.arg_string)?;
                }

                let size = if tokens.directive == Directive::Assembler(Assembler::LTORG) {
                    let ltorg = address_literals(self.cur_offset, &mut self.littab);
                    let len = ltorg.len();
                    self.ltorgs.insert(self.cur_offset, ltorg);
                    len
                } else {
                    tokens.size()?
                };

                let offset = self.cur_offset;

                if let Some(label) = tokens.label.as_ref() {
                    if tokens.directive == Directive::Assembler(Assembler::EQU) {
                        self.labels.add(
                            label.clone(),
                            Label {
                                block: "".to_owned(),
                                offset: self
                                    .calc_expr(&tokens.argument.expect_string()?.arg_string)?,
                            },
                        );
                    } else {
                        self.labels.add(
                            label.clone(),
                            Label {
                                block: "".to_owned(),
                                offset,
                            },
                        );
                    }
                }

                if let Some(lit) = tokens.get_inline_literal() {
                    if self.littab.get(&lit).is_none() {
                        let value = parse_literal(&lit)?;
                        self.littab.insert(
                            lit,
                            Literal {
                                offset: None,
                                value,
                            },
                        );
                    }
                }

                self.cur_offset += size;

                Ok(ParsedLine {
                    block: block.to_owned(),
                    label: tokens.label,
                    directive: tokens.directive,
                    extended: tokens.extended,
                    argument: tokens.argument,
                    size,
                    offset,
                    line_no,
                    text: line.into(),
                })
            })
            .transpose()
    }
}

#[derive(Debug, Clone)]
pub struct Ltorg {
    pub offset: usize,
    pub data: Vec<u8>,
}

impl Ltorg {
    pub fn len(&self) -> usize {
        self.data.len()
    }
}

fn address_literals(start_offset: usize, littab: &mut HashMap<LiteralArgument, Literal>) -> Ltorg {
    let mut lt_offset = start_offset;
    let mut ltorg_bytes = Vec::new();
    for literal in littab.values_mut().filter(|v| v.offset.is_none()) {
        literal.offset = Some(lt_offset);
        ltorg_bytes.append(&mut literal.value.clone());
        lt_offset += literal.value.len();
    }
    Ltorg {
        offset: start_offset,
        data: ltorg_bytes,
    }
}

#[derive(Debug, Clone)]
pub struct StringArgument {
    arg_string: String,
    indexed: bool,
    mode: AddressMode,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralArgument {
    Bytes(String),
    Chars(String),
}

#[derive(Debug, Clone)]
pub enum ArgumentToken {
    None,
    String(StringArgument),
    Literal(LiteralArgument),
}

impl ArgumentToken {
    fn parse_literal(lit: &str) -> Result<Self> {
        let captures = lit_regex()
            .captures(lit)
            .ok_or_else(|| anyhow::Error::msg("invalid literal"))?;

        if let Some(bytes) = captures.name("bytes") {
            Ok(Self::Literal(LiteralArgument::Bytes(
                bytes.as_str().to_owned(),
            )))
        } else if let Some(chars) = captures.name("chars") {
            Ok(Self::Literal(LiteralArgument::Chars(
                chars.as_str().to_owned(),
            )))
        } else {
            //unreachable!("Regex has to match either bytes or chars")
            Err(anyhow::Error::msg("Doesn't match regex"))
        }
    }

    fn parse_argument(arg: Option<&str>) -> Result<Self> {
        if let Some(arg) = arg {
            if let Some(lit) = arg.strip_prefix('=') {
                Self::parse_literal(lit)
            } else {
                let (arg_string, mode, indexed) = match (
                    arg.starts_with('#'),
                    arg.starts_with('@'),
                    arg.ends_with(",X"),
                ) {
                    (true, true, _) => unreachable!("Can't start with tow different things"),
                    (true, false, true) => (&arg[1..arg.len() - 2], AddressMode::Immediate, true),
                    (true, false, false) => (&arg[1..], AddressMode::Immediate, false),
                    (false, true, true) => (&arg[1..arg.len() - 2], AddressMode::Indirect, true),
                    (false, true, false) => (&arg[1..], AddressMode::Indirect, false),
                    (false, false, true) => (&arg[..arg.len() - 2], AddressMode::Simple, true),
                    (false, false, false) => (arg, AddressMode::Simple, false),
                };
                Ok(Self::String(StringArgument {
                    arg_string: arg_string.to_owned(),
                    indexed,
                    mode,
                }))
            }
        } else {
            Ok(Self::None)
        }
    }

    fn expect_string(&self) -> Result<&StringArgument> {
        if let Self::String(ref s) = self {
            Ok(s)
        } else {
            Err(anyhow::Error::msg(format!(
                "expected string argument, found {:?}",
                self
            )))
        }
    }

    fn get_string(&self) -> Option<&StringArgument> {
        if let Self::String(ref s) = self {
            Some(s)
        } else {
            None
        }
    }

    fn literal_length(&self) -> Result<usize> {
        match self {
            ArgumentToken::Literal(LiteralArgument::Bytes(b)) => Ok(b.len() / 2),
            ArgumentToken::Literal(LiteralArgument::Chars(c)) => Ok(c.len()),
            _ => Err(anyhow::Error::msg("expected literal argument")),
        }
    }
}

pub fn parse_literal(arg: &LiteralArgument) -> Result<Vec<u8>> {
    Ok(match arg {
        LiteralArgument::Bytes(v) => v
            .chars()
            .collect::<Vec<char>>()
            .chunks(2)
            .map(|c| c.iter().collect::<String>())
            .map(|s| u8::from_str_radix(&s, 16))
            .collect::<Result<Vec<_>, _>>()?,
        LiteralArgument::Chars(v) => v.chars().map(|c| c as u8).collect::<Vec<_>>(),
    })
}

struct LineTokens {
    label: Option<String>,
    directive: Directive,
    extended: bool,
    argument: ArgumentToken,
}

impl LineTokens {
    fn from_line(line: &str) -> Result<Option<Self>> {
        line_regex()
            .captures(line)
            .map(|cap| {
                let raw_directive = cap
                    .name("directive")
                    .map(|m| m.as_str().to_owned())
                    .ok_or_else(|| anyhow::Error::msg("Expected a directive"))?;
                let raw_argument = cap
                    .name("argument")
                    .filter(|m| !m.as_str().is_empty())
                    .map(|m| m.as_str());

                let label = cap.name("label").map(|m| m.as_str().to_owned());

                let directive =
                    Directive::from_str(&raw_directive.replace('+', "")).ok_or_else(|| {
                        anyhow::Error::msg(format!("Couldn't parse directive {}", raw_directive))
                    })?;

                let argument = if directive == Directive::Assembler(Assembler::BYTE) {
                    ArgumentToken::parse_literal(
                        raw_argument.ok_or_else(|| anyhow::Error::msg("literal expected"))?,
                    )?
                } else {
                    ArgumentToken::parse_argument(raw_argument)?
                };

                Ok(Self {
                    label,
                    directive,
                    extended: raw_directive.starts_with('+'),
                    argument,
                })
            })
            .transpose()
    }

    fn get_inline_literal(&self) -> Option<LiteralArgument> {
        match (
            self.directive == Directive::Assembler(Assembler::BYTE),
            &self.argument,
        ) {
            (false, ArgumentToken::Literal(l)) => Some(l.clone()),
            _ => None,
        }
    }

    fn size(&self) -> Result<usize> {
        Ok(match self.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START
                | Assembler::END
                | Assembler::BASE
                | Assembler::EQU
                | Assembler::USE
                | Assembler::ORG => 0,
                Assembler::BYTE => self.argument.literal_length()?,
                Assembler::WORD => 3,
                // Implemented in the caller
                Assembler::LTORG => unimplemented!(),
                Assembler::RESW => self.argument.expect_string()?.arg_string.parse::<usize>()? * 3,
                Assembler::RESB => self.argument.expect_string()?.arg_string.parse::<usize>()?,
            },
            Directive::OneByte(_) => 1,
            Directive::OneReg(_) | Directive::TwoReg(_) | Directive::Shift(_) | Directive::SVC => 2,
            Directive::Variable(_) => {
                if self.extended {
                    4
                } else {
                    3
                }
            }
        })
    }
}

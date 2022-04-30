use std::collections::HashMap;

use anyhow::{Context, Result};

use libsic::xe::op::{AddressFlags, AddressMode, AddressRelativeTo, VariableOp};
use regex::Regex;

pub struct FirstPass {
    cur_offset: usize,
    labels: Labels,
    literal_size: usize,
    ltorg_index: usize,
    literal_offsets: HashMap<usize, usize>,
}

pub struct PassOne {
    pub parsed_lines: Vec<ParsedLine>,
    pub labels: Labels,
    pub literal_offsets: HashMap<usize, usize>,
}

use crate::{
    constants::{line_regex, lit_regex},
    directive::{Assembler, Directive},
    labels::Labels,
};

static MAX_DISP: u16 = 4095; // 0x0F_FF
static MAX_PC: u16 = 2047; // 0x07_FF
static MIN_PC: i16 = -2048; // 0x08_00

#[derive(Debug, Clone)]
pub struct ParsedLine {
    pub label: Option<String>,
    pub directive: Directive,
    pub argument: ArgumentToken,
    pub extended: bool,
    pub size: usize,
    pub offset: usize,
    pub literal_offset: Option<usize>,
    pub ltorg_index: usize,
}

impl ParsedLine {
    fn target(&self, labels: &Labels) -> Result<Option<usize>> {
        if let ArgumentToken::String(mut argument) = self.argument.clone() {
            if argument.ends_with(",X") {
                argument = argument[..argument.len() - 2].to_owned();
            }
            if argument.starts_with('#') && Regex::new("[0-9][0-9]*")?.is_match(&argument[1..]) {
                return Ok(Some(argument[1..].parse().map_err(|e| {
                    anyhow::Error::msg("Couldn't parse immediate arg").context(e)
                })?));
            }
            if argument.starts_with('#') || argument.starts_with('@') {
                argument = argument[1..].to_owned();
            }

            Ok(Some(labels.get(&argument)?))
        } else {
            Ok(None)
        }
    }

    pub fn get_argument(&self) -> Result<&str> {
        self.argument.expect_string()
    }

    pub fn parse_flags(
        &self,
        base: Option<usize>,
        labels: &Labels,
        literal_offsets: &HashMap<usize, usize>,
    ) -> Result<(u32, AddressFlags)> {
        //TODO: Split this into parsing flags and calculating displacement
        let mode = match self.argument.get_string().and_then(|a| a.chars().next()) {
            Some('#') => AddressMode::Immediate,
            Some('@') => AddressMode::Indirect,
            _ => AddressMode::Simple,
        };
        let indexed = self
            .argument
            .get_string()
            .map(|a| a.ends_with(",X"))
            .unwrap_or(false);
        let target = if let Some(literal_offset) = self.literal_offset {
            Some(literal_offsets.get(&self.ltorg_index).unwrap() + literal_offset)
        } else {
            self.target(labels)?
        };
        let target = if target.is_none() && self.directive == Directive::Variable(VariableOp::RSUB)
        {
            0
        } else {
            target.ok_or_else(|| anyhow::Error::msg("Expected target"))?
        };
        let pc = self.offset + 3;
        let (disp, relative_to) = if mode == AddressMode::Immediate || self.extended {
            // Immediate or extended
            (target, AddressRelativeTo::Direct)
        } else if (target as i32) - (pc as i32) < (MAX_PC as i32)
            && target as i32 - pc as i32 > MIN_PC as i32
        {
            // PC
            ((target as i32 - pc as i32) as usize, AddressRelativeTo::PC)
        } else if base
            .map(|base| target >= base && (target - base) < (MAX_DISP as usize))
            .unwrap_or(false)
        {
            // Base
            (target - base.unwrap(), AddressRelativeTo::Base)
        } else if target < MAX_DISP as usize {
            // Direct
            (target, AddressRelativeTo::Direct)
        } else {
            // Error
            panic!("Too big for simple addressing");
        };
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
            cur_offset: 0,
            labels: Labels::new(),
            literal_size: 0,
            ltorg_index: 0,
            literal_offsets: HashMap::new(),
        }
    }

    pub fn parse_lines(lines: &[&str]) -> Result<PassOne> {
        let mut pass = Self::new();
        let mut lines = lines
            .iter()
            .enumerate()
            .filter_map(|(line_no, line)| {
                pass.parse_line(line)
                    .context(format!("Parse error on line {}", line_no))
                    .transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;

        if pass.literal_size > 0 {
            let lastline = ParsedLine {
                label: None,
                directive: Directive::Assembler(Assembler::LTORG),
                argument: ArgumentToken::None,
                extended: false,
                size: pass.literal_size,
                offset: lines.last().unwrap().offset,
                literal_offset: None,
                ltorg_index: pass.ltorg_index,
            };
            pass.literal_offsets
                .insert(lastline.ltorg_index, lastline.offset);
            lines.insert(lines.len() - 1, lastline);
        }

        Ok(PassOne {
            parsed_lines: lines,
            labels: pass.labels,
            literal_offsets: pass.literal_offsets,
        })
    }

    fn label_val(&self, label: &str) -> Result<usize> {
        Ok(if label == "*" {
            self.cur_offset
        } else if label.chars().all(char::is_numeric) {
            label.parse::<usize>()?
        } else {
            self.labels.get(label)?
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

    fn parse_line(&mut self, line: &str) -> Result<Option<ParsedLine>> {
        let tokens = LineTokens::from_line(line)?;
        tokens
            .map(|tokens| {
                if tokens.directive == Directive::Assembler(Assembler::ORG) {
                    self.cur_offset = self.calc_expr(tokens.argument.expect_string()?)?;
                }

                let size = if tokens.directive == Directive::Assembler(Assembler::LTORG) {
                    let cur_size = self.literal_size;
                    self.literal_size = 0;
                    self.literal_offsets
                        .insert(self.ltorg_index, self.cur_offset);
                    self.ltorg_index += 1;
                    cur_size
                } else {
                    tokens.size()?
                };

                let offset = self.cur_offset;

                if let Some(label) = tokens.label.as_ref() {
                    if tokens.directive == Directive::Assembler(Assembler::EQU) {
                        self.labels.add(
                            label.clone(),
                            self.calc_expr(tokens.argument.expect_string()?)?,
                        );
                    } else {
                        self.labels.add(label.clone(), offset);
                    }
                }

                let literal_offset = if tokens.has_inline_literal() {
                    let literal_offset = self.literal_size;
                    self.literal_size += tokens.argument.literal_length()?;
                    Some(literal_offset)
                } else {
                    None
                };

                self.cur_offset += size;

                Ok(ParsedLine {
                    label: tokens.label,
                    directive: tokens.directive,
                    extended: tokens.extended,
                    argument: tokens.argument,
                    size,
                    offset,
                    literal_offset,
                    ltorg_index: self.ltorg_index,
                })
            })
            .transpose()
    }
}

#[derive(Debug, Clone)]
pub enum ArgumentToken {
    None,
    String(String),
    LiteralBytes(String),
    LiteralChars(String),
}

impl ArgumentToken {
    fn parse_literal(lit: &str) -> Result<Self> {
        println!("regexing {}", lit);
        let captures = lit_regex()
            .captures(lit)
            .ok_or_else(|| anyhow::Error::msg("invalid literal"))?;

        println!("caps: {:?}", captures);

        if let Some(bytes) = captures.name("bytes") {
            Ok(Self::LiteralBytes(bytes.as_str().to_owned()))
        } else if let Some(chars) = captures.name("chars") {
            Ok(Self::LiteralChars(chars.as_str().to_owned()))
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
                Ok(Self::String(arg.to_owned()))
            }
        } else {
            Ok(Self::None)
        }
    }

    fn expect_string(&self) -> Result<&str> {
        if let Self::String(ref s) = self {
            Ok(s)
        } else {
            Err(anyhow::Error::msg(format!(
                "expected string argument, found {:?}",
                self
            )))
        }
    }

    fn get_string(&self) -> Option<&str> {
        if let Self::String(ref s) = self {
            Some(s)
        } else {
            None
        }
    }

    fn literal_length(&self) -> Result<usize> {
        match self {
            ArgumentToken::LiteralBytes(b) => Ok(b.len() / 2),
            ArgumentToken::LiteralChars(c) => Ok(c.len()),
            _ => Err(anyhow::Error::msg("expected literal argument")),
        }
    }
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

    fn has_inline_literal(&self) -> bool {
        matches!(
            (
                self.directive == Directive::Assembler(Assembler::BYTE),
                &self.argument,
            ),
            (
                false,
                ArgumentToken::LiteralBytes(_) | ArgumentToken::LiteralChars(_)
            )
        )
    }

    fn size(&self) -> Result<usize> {
        Ok(match self.directive {
            Directive::Assembler(asm) => match asm {
                Assembler::START | Assembler::END | Assembler::BASE => 0,
                Assembler::BYTE => self.argument.literal_length()?,
                Assembler::EQU => 0,
                Assembler::WORD => 3,
                Assembler::ORG => 0,
                // Implemented in the caller
                Assembler::LTORG => unimplemented!(),
                Assembler::RESW => self.argument.expect_string()?.parse::<usize>()? * 3,
                Assembler::RESB => self.argument.expect_string()?.parse::<usize>()?,
            },
            Directive::OneByte(_) => 1,
            Directive::OneReg(_) => 2,
            Directive::TwoReg(_) => 2,
            Directive::Shift(_) => 2,
            Directive::SVC => 2,
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

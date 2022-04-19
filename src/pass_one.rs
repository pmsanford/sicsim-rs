use std::collections::HashMap;

use anyhow::Result;

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
    constants::line_regex,
    directive::{Assembler, Directive},
    labels::Labels,
};

static MAX_DISP: u16 = 4095; // 0x0F_FF
static MAX_PC: u16 = 2047; // 0x07_FF
static MIN_PC: i16 = -2048; // 0x08_00

fn literal_size(argument: &str) -> Result<usize> {
    if argument.starts_with("C'") {
        Ok(argument.len() - 3)
    } else if argument.starts_with("X'") {
        Ok((argument.len() - 3) / 2)
    } else {
        Err(anyhow::Error::msg(format!("Invalid literal {}", argument)))
    }
}

fn size(directive: &str, argument: Option<&String>) -> Result<usize> {
    if let Some(dir) = Directive::from_str(&directive.replace('+', "")) {
        Ok(match dir {
            Directive::Assembler(asm) => match asm {
                Assembler::START | Assembler::END | Assembler::BASE => 0,
                Assembler::BYTE => {
                    let argument = argument
                        .ok_or_else(|| anyhow::Error::msg("Byte directive requires argument"))?;
                    literal_size(argument)?
                }
                Assembler::WORD => 3,
                // Implemented in the caller
                Assembler::LTORG => unimplemented!(),
                Assembler::RESW => {
                    let argument = argument
                        .ok_or_else(|| anyhow::Error::msg("RESW directive requires argument"))?;
                    argument.parse::<usize>()? * 3
                }
                Assembler::RESB => {
                    let argument = argument
                        .ok_or_else(|| anyhow::Error::msg("RESB directive requires argument"))?;
                    argument.parse::<usize>()?
                }
            },
            Directive::OneByte(_) => 1,
            Directive::OneReg(_) => 2,
            Directive::TwoReg(_) => 2,
            Directive::Shift(_) => 2,
            Directive::SVC => 2,
            Directive::Variable(_) => {
                if directive.starts_with('+') {
                    4
                } else {
                    3
                }
            }
        })
    } else {
        Err(anyhow::Error::msg(format!(
            "Unknown directive {}",
            directive
        )))
    }
}

#[derive(Debug)]
pub struct ParsedLine {
    pub label: Option<String>,
    pub directive: Directive,
    pub argument: Option<String>,
    pub extended: bool,
    pub size: usize,
    pub offset: usize,
    pub literal_offset: Option<usize>,
    pub ltorg_index: usize,
}

impl ParsedLine {
    fn target(&self, labels: &Labels) -> Result<Option<usize>> {
        if let Some(mut argument) = self.argument.clone() {
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
        self.argument
            .as_deref()
            .ok_or_else(|| anyhow::Error::msg(format!("{:?} requires an argument", self.directive)))
    }

    pub fn parse_flags(
        &self,
        base: Option<usize>,
        labels: &Labels,
        literal_offsets: &HashMap<usize, usize>,
    ) -> Result<(u32, AddressFlags)> {
        //TODO: Split this into parsing flags and calculating displacement
        let mode = match self.argument.as_ref().and_then(|a| a.chars().next()) {
            Some('#') => AddressMode::Immediate,
            Some('@') => AddressMode::Indirect,
            _ => AddressMode::Simple,
        };
        let indexed = self
            .argument
            .as_ref()
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
            .filter_map(|line| pass.parse_line(line).transpose())
            .collect::<Result<Vec<_>, _>>()?;

        if pass.literal_size > 0 {
            let lastline = ParsedLine {
                label: None,
                directive: Directive::Assembler(Assembler::LTORG),
                argument: None,
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

    fn parse_line(&mut self, line: &str) -> Result<Option<ParsedLine>> {
        line_regex()
            .captures(line)
            .map(|cap| {
                let raw_directive = cap
                    .name("directive")
                    .map(|m| m.as_str().to_owned())
                    .ok_or_else(|| anyhow::Error::msg("Expected a 'directive' capture"))?;
                let argument = cap
                    .name("argument")
                    .filter(|m| !m.as_str().is_empty())
                    .map(|m| m.as_str().to_owned());

                let directive =
                    Directive::from_str(&raw_directive.replace('+', "")).ok_or_else(|| {
                        anyhow::Error::msg(format!("Couldn't parse directive {}", raw_directive))
                    })?;

                let size = if directive == Directive::Assembler(Assembler::LTORG) {
                    let cur_size = self.literal_size;
                    self.literal_size = 0;
                    self.literal_offsets
                        .insert(self.ltorg_index, self.cur_offset);
                    self.ltorg_index += 1;
                    cur_size
                } else {
                    size(&raw_directive, argument.as_ref())?
                };

                let offset = self.cur_offset;
                self.cur_offset += size;
                let label = cap.name("label").map(|m| m.as_str().to_owned());

                if let Some(label) = label.as_ref() {
                    self.labels.add(label.clone(), offset);
                }

                let literal_offset = if argument
                    .as_ref()
                    .map(|a| a.starts_with('='))
                    .unwrap_or(false)
                {
                    let literal_offset = self.literal_size;
                    let argument = argument.as_ref().unwrap();
                    self.literal_size += literal_size(&argument[1..])?;
                    Some(literal_offset)
                } else {
                    None
                };

                Ok(ParsedLine {
                    label,
                    directive,
                    extended: raw_directive.starts_with('+'),
                    argument,
                    size,
                    offset,
                    literal_offset,
                    ltorg_index: self.ltorg_index,
                })
            })
            .transpose()
    }
}

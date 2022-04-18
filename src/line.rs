use anyhow::Result;

use libsic::xe::op::{AddressFlags, AddressMode, AddressRelativeTo, VariableOp};
use regex::Regex;

use crate::{
    constants::line_regex,
    directive::{Assembler, Directive},
    labels::Labels,
};

static MAX_DISP: u16 = 4095; // 0x0F_FF
static MAX_PC: u16 = 2047; // 0x07_FF
static MIN_PC: i16 = -2048; // 0x08_00

fn size(directive: &str, argument: Option<&String>) -> Result<usize> {
    if let Some(dir) = Directive::from_str(&directive.replace('+', "")) {
        Ok(match dir {
            Directive::Assembler(asm) => match asm {
                Assembler::START | Assembler::END | Assembler::BASE => 0,
                Assembler::BYTE => {
                    let argument = argument
                        .ok_or_else(|| anyhow::Error::msg("Byte directive requires argument"))?;
                    if argument.starts_with("C'") {
                        argument.len() - 3
                    } else if argument.starts_with("X'") {
                        (argument.len() - 3) / 2
                    } else {
                        return Err(anyhow::Error::msg(format!(
                            "Invalid byte directive {}",
                            argument
                        )));
                    }
                }
                Assembler::WORD => 3,
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

    pub fn parse_flags(&self, base: Option<usize>, labels: &Labels) -> Result<(u32, AddressFlags)> {
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
        let target = self.target(labels)?;
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

pub fn parse_line(line: &str, offset: usize) -> Result<Option<ParsedLine>> {
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

            let size = size(&raw_directive, argument.as_ref())?;

            Ok(ParsedLine {
                label: cap.name("label").map(|m| m.as_str().to_owned()),
                directive,
                extended: raw_directive.starts_with('+'),
                argument,
                size,
                offset,
            })
        })
        .transpose()
}

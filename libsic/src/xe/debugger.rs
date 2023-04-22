use std::{collections::HashMap, error::Error, fmt::Display};

use sicdbg::Sdb;

use crate::{
    word::{DWord, DWordExt, Word},
    xe::op::{AddressFlags, AddressMode, Op, SicOp},
    WordExt,
};

use super::{
    status_word::{SUP_BYTE, SUP_MASK},
    vm::Debugger,
};

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct LoadedProgram {
    loaded_at: u32,
    sdb: Sdb,
    last_address: u32,
}

#[allow(non_snake_case)]
#[derive(Clone, Debug, Default)]
struct RegState {
    pub A: Word,  // 0
    pub X: Word,  // 1
    pub L: Word,  // 2
    pub B: Word,  // 3
    pub S: Word,  // 4
    pub T: Word,  // 5
    pub F: DWord, // 6
    pub PC: Word, // 8
    pub SW: Word, // 9
    pub I: Word,
}

fn format_sw(prev: Word, cur: Word) -> String {
    let prev_supervisor = if prev[SUP_BYTE] & SUP_MASK > 0 { 1 } else { 0 };
    let prev_state = if prev[0] & 0x40 > 0 { 1 } else { 0 };
    let prev_pid = (prev[0] & 0x3C) >> 2;
    let prev_cc = prev[0] & 0x03;
    let prev_mask = (prev[1] & 0xF0) >> 4;
    let prev_unused = prev[1] & 0x0F;
    let prev_icode = prev[2];

    let cur_supervisor = if cur[SUP_BYTE] & SUP_MASK > 0 { 1 } else { 0 };
    let cur_state = if cur[0] & 0x40 > 0 { 1 } else { 0 };
    let cur_pid = (cur[0] & 0x3C) >> 2;
    let cur_cc = cur[0] & 0x03;
    let cur_mask = (cur[1] & 0xF0) >> 4;
    let cur_unused = cur[1] & 0x0F;
    let cur_icode = cur[2];

    let mut diff = Vec::new();

    if prev_supervisor != cur_supervisor {
        diff.push(format!("sup {} -> {}", prev_supervisor, cur_supervisor));
    }

    if prev_state != cur_state {
        diff.push(format!("state {} -> {}", prev_state, cur_state));
    }

    if prev_pid != cur_pid {
        diff.push(format!("pid {:02} -> {:02}", prev_pid, cur_pid));
    }

    if prev_cc != cur_cc {
        diff.push(format!("cc {:02b} -> {:02b}", prev_cc, cur_cc));
    }

    if prev_mask != cur_mask {
        diff.push(format!("mask {:04b} -> {:04b}", prev_mask, cur_mask));
    }

    if prev_unused != cur_unused {
        diff.push(format!("unused {:04b} -> {:04b}", prev_unused, cur_unused));
    }

    if prev_icode != cur_icode {
        diff.push(format!("icode {:#04X} -> {:#04X}", prev_icode, cur_icode));
    }

    diff.join(", ")
}

impl RegState {
    fn diff(&self, other: &RegState) -> Option<String> {
        let mut diffs = Vec::new();
        if self.A != other.A {
            diffs.push(format!(
                "A {:#08X} -> {:#08X}",
                self.A.as_u32(),
                other.A.as_u32()
            ));
        }

        if self.X != other.X {
            diffs.push(format!(
                "X {:#08X} -> {:#08X}",
                self.X.as_u32(),
                other.X.as_u32()
            ));
        }

        if self.L != other.L {
            diffs.push(format!(
                "L {:#08X} -> {:#08X}",
                self.L.as_u32(),
                other.L.as_u32()
            ));
        }

        if self.B != other.B {
            diffs.push(format!(
                "B {:#08X} -> {:#08X}",
                self.B.as_u32(),
                other.B.as_u32()
            ));
        }

        if self.S != other.S {
            diffs.push(format!(
                "S {:#08X} -> {:#08X}",
                self.S.as_u32(),
                other.S.as_u32()
            ));
        }

        if self.T != other.T {
            diffs.push(format!(
                "T {:#08X} -> {:#08X}",
                self.T.as_u32(),
                other.T.as_u32()
            ));
        }

        if self.F != other.F {
            diffs.push(format!("F {} -> {}", self.F.as_f64(), other.F.as_f64()));
        }

        if self.PC != other.PC && (other.PC.as_u32() as i32 - self.PC.as_u32() as i32) > 4 {
            diffs.push(format!(
                "PC {:#08X} -> {:#08X}",
                self.PC.as_u32(),
                other.PC.as_u32()
            ));
        }

        if self.SW != other.SW {
            diffs.push(format!("SW [{}]", format_sw(self.SW, other.SW)));
        }

        if self.I != other.I {
            diffs.push(format!("I {} -> {}", self.I.as_u32(), other.I.as_u32()));
        }

        if diffs.is_empty() {
            None
        } else {
            Some(diffs.join(", "))
        }
    }
}

#[derive(Debug)]
pub struct LoadError;

impl Error for LoadError {}

impl From<sicdbg::Error> for LoadError {
    fn from(_: sicdbg::Error) -> Self {
        LoadError
    }
}

impl Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error loading debug symbols")
    }
}

pub struct SdbDebugger {
    verbose: bool,
    // TODO: Is there a better way to do this mapping?
    programs: HashMap<(u32, u32), LoadedProgram>,
    last_reg_state: RegState,
}

impl SdbDebugger {
    pub fn new() -> Self {
        SdbDebugger {
            verbose: false,
            programs: HashMap::new(),
            last_reg_state: RegState::default(),
        }
    }

    pub fn verbose() -> Self {
        SdbDebugger {
            verbose: true,
            programs: HashMap::new(),
            last_reg_state: RegState::default(),
        }
    }

    pub fn load(&mut self, loaded_at: u32, sdb: String) -> Result<(), LoadError> {
        let sdb = Sdb::from_string(&sdb)?;

        let last_offset = *sdb.offset_map.keys().max().ok_or(LoadError)?;

        let last_address = loaded_at + last_offset;

        self.programs.insert(
            (loaded_at, last_address),
            LoadedProgram {
                loaded_at,
                sdb,
                last_address,
            },
        );
        Ok(())
    }

    fn find_program(&self, pc: u32) -> Option<&LoadedProgram> {
        for ((start, end), program) in self.programs.iter() {
            if pc >= *start && pc <= *end {
                return Some(program);
            }
        }

        None
    }
}

impl Default for SdbDebugger {
    fn default() -> Self {
        Self::new()
    }
}

impl Debugger for SdbDebugger {
    fn op_read(&mut self, vm_state: &super::vm::SicXeVm, op: &super::op::Op) {
        self.last_reg_state = RegState {
            A: vm_state.A,
            X: vm_state.X,
            L: vm_state.L,
            B: vm_state.B,
            S: vm_state.S,
            T: vm_state.T,
            F: vm_state.F,
            PC: vm_state.PC,
            SW: vm_state.SW,
            I: vm_state.I,
        };
        let pc = vm_state.PC.as_u32();
        let program = self.find_program(pc);
        print!("({:#08X}): {}", pc, op.mnemonic());
        let target_address = if let Op::Variable(op) = op {
            if let Ok(address) = vm_state.debug_target_address(op, true) {
                if op.address_flags.mode == AddressMode::Immediate {
                    print!(" #{}", address);
                    None
                } else if op.address_flags.mode == AddressMode::Indirect {
                    let src_address = vm_state
                        .debug_calc_address(op.address, &AddressFlags::default(), true)
                        .unwrap();
                    print!(" @{:#08X} -> {:#08X}", src_address, address);
                    Some(address)
                } else {
                    print!(
                        " -> {}{:#08X}",
                        if op.address_flags.extended { "+" } else { "" },
                        address
                    );
                    Some(address)
                }
            } else {
                None
            }
        } else {
            None
        };
        if self.verbose {
            print!(" - {:?}", op);
        }
        println!();
        if let Some(program) = program {
            let name = &program.sdb.name;
            let offset = pc - program.loaded_at;
            if let Some(line) = program.sdb.offset_map.get(&offset) {
                let line = &program.sdb.lines[*line];
                println!("  [{}] {}: {}", name, line.line_number, line.text);
            } else {
                println!("  [{}] Unknown line", name);
            }
        }
        if let Some(target_address) = target_address {
            if let Some(target_program) = self.find_program(target_address) {
                let name = &target_program.sdb.name;
                let offset = target_address - target_program.loaded_at;
                if let Some(line) = target_program.sdb.offset_map.get(&offset) {
                    let line = &target_program.sdb.lines[*line];
                    println!("    Target: [{}] {}: {}", name, line.line_number, line.text);
                }
            }
        }
    }

    fn op_executed(&mut self, vm_state: &super::vm::SicXeVm, _op: &super::op::Op) {
        let new_reg_state = RegState {
            A: vm_state.A,
            X: vm_state.X,
            L: vm_state.L,
            B: vm_state.B,
            S: vm_state.S,
            T: vm_state.T,
            F: vm_state.F,
            PC: vm_state.PC,
            SW: vm_state.SW,
            I: vm_state.I,
        };

        if let Some(diff) = self.last_reg_state.diff(&new_reg_state) {
            println!("  {}", diff);
        }
    }

    fn interrupt(&mut self, vm_state: &super::vm::SicXeVm, interrupt: super::vm::Interrupt) {
        let pc = vm_state.PC.as_u32();
        println!("({:#08X}): Saw interrupt {:?}", pc, interrupt);
    }
}

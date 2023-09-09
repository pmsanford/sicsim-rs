use std::{collections::HashMap, error::Error, fmt::Display};

use sicdbg::{Sdb, SdbLine};

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

#[derive(Clone, Copy, Debug, Default)]
struct RegDiff<T: Clone + Copy + std::fmt::Debug + Default> {
    current: T,
    previous: T,
}

#[allow(non_snake_case)]
#[derive(Clone, Copy, Debug, Default)]
struct StateDiff {
    pub A: Option<RegDiff<Word>>,  // 0
    pub X: Option<RegDiff<Word>>,  // 1
    pub L: Option<RegDiff<Word>>,  // 2
    pub B: Option<RegDiff<Word>>,  // 3
    pub S: Option<RegDiff<Word>>,  // 4
    pub T: Option<RegDiff<Word>>,  // 5
    pub F: Option<RegDiff<DWord>>, // 6
    pub PC: Option<RegDiff<Word>>, // 8
    pub SW: Option<RegDiff<Word>>, // 9
    pub I: Option<RegDiff<Word>>,
}

impl Display for StateDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(RegDiff { current, previous }) = self.A {
            write!(
                f,
                "A {:#08X} -> {:#08X}",
                previous.as_u32(),
                current.as_u32()
            )?;
        }

        if let Some(RegDiff { current, previous }) = self.X {
            write!(
                f,
                "X {:#08X} -> {:#08X}",
                previous.as_u32(),
                current.as_u32()
            )?;
        }

        if let Some(RegDiff { current, previous }) = self.L {
            write!(
                f,
                "X {:#08X} -> {:#08X}",
                previous.as_u32(),
                current.as_u32()
            )?;
        }

        if let Some(RegDiff { current, previous }) = self.B {
            write!(
                f,
                "B {:#08X} -> {:#08X}",
                previous.as_u32(),
                current.as_u32()
            )?;
        }

        if let Some(RegDiff { current, previous }) = self.S {
            write!(
                f,
                "S {:#08X} -> {:#08X}",
                previous.as_u32(),
                current.as_u32()
            )?;
        }

        if let Some(RegDiff { current, previous }) = self.T {
            write!(
                f,
                "T {:#08X} -> {:#08X}",
                previous.as_u32(),
                current.as_u32()
            )?;
        }

        if let Some(RegDiff { current, previous }) = self.F {
            write!(f, "F {} -> {}", previous.as_f64(), current.as_f64())?;
        }

        if let Some(RegDiff { current, previous }) = self.A {
            let diff = current.as_u32() as i64 - previous.as_u32() as i64;
            // Don't show automatic increment
            if !(3..=4).contains(&diff) {
                write!(
                    f,
                    "A {:#08X} -> {:#08X}",
                    previous.as_u32(),
                    current.as_u32()
                )?;
            }
        }

        if let Some(RegDiff { current, previous }) = self.SW {
            write!(f, "SW [{}]", format_sw(previous, current))?;
        }

        if let Some(RegDiff { current, previous }) = self.I {
            write!(
                f,
                "I {:#08X} -> {:#08X}",
                previous.as_u32(),
                current.as_u32()
            )?;
        }

        Ok(())
    }
}

impl RegState {
    fn diff(&self, other: &RegState) -> StateDiff {
        let mut diffs = StateDiff::default();
        if self.A != other.A {
            diffs.A = Some(RegDiff {
                previous: self.A,
                current: other.A,
            });
        }

        if self.X != other.X {
            diffs.X = Some(RegDiff {
                previous: self.X,
                current: other.X,
            });
        }

        if self.L != other.L {
            diffs.L = Some(RegDiff {
                previous: self.L,
                current: other.L,
            });
        }

        if self.B != other.B {
            diffs.B = Some(RegDiff {
                previous: self.B,
                current: other.B,
            });
        }

        if self.S != other.S {
            diffs.S = Some(RegDiff {
                previous: self.S,
                current: other.S,
            });
        }

        if self.T != other.T {
            diffs.T = Some(RegDiff {
                previous: self.T,
                current: other.T,
            });
        }

        if self.F != other.F {
            diffs.F = Some(RegDiff {
                previous: self.F,
                current: other.F,
            });
        }

        if self.PC != other.PC {
            diffs.PC = Some(RegDiff {
                previous: self.PC,
                current: other.PC,
            });
        }

        if self.SW != other.SW {
            diffs.SW = Some(RegDiff {
                previous: self.SW,
                current: other.SW,
            });
        }

        if self.I != other.I {
            diffs.I = Some(RegDiff {
                previous: self.I,
                current: other.I,
            });
        }

        diffs
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

#[derive(Clone, Debug)]
pub struct RuntimeTracker {
    // TODO: Is there a better way to do this mapping?
    programs: HashMap<(u32, u32), LoadedProgram>,
    labels: HashMap<String, HashMap<String, u32>>,
    last_reg_state: RegState,
}

impl RuntimeTracker {
    pub fn save_reg_state(&mut self, vm: &super::vm::SicXeVm) {
        self.last_reg_state = RegState {
            A: vm.A,
            X: vm.X,
            L: vm.L,
            B: vm.B,
            S: vm.S,
            T: vm.T,
            F: vm.F,
            PC: vm.PC,
            SW: vm.SW,
            I: vm.I,
        };
    }

    pub fn load(&mut self, loaded_at: u32, sdb: String) -> Result<(), LoadError> {
        let sdb = Sdb::from_string(&sdb)?;

        let last_offset = *sdb.offset_map.keys().max().ok_or(LoadError)?;

        let last_address = loaded_at + last_offset;

        //TODO: This is a mess
        for (label, offset) in sdb.labels.iter() {
            let entry = self
                .labels
                .entry(label.clone())
                .or_insert_with(HashMap::new);
            let mut name = sdb.name.clone();
            let mut ctr = 0;
            while entry.contains_key(&name) {
                ctr += 1;
                name = format!("{}_{}", sdb.name, ctr);
            }
            entry.insert(name, loaded_at + offset);
        }

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

    fn find_target_address(
        &self,
        vm: &super::vm::SicXeVm,
        op: &super::op::Op,
    ) -> (Option<u32>, Option<u32>) {
        if let Op::Variable(op) = op {
            if let Ok(address) = vm.debug_target_address(op, true) {
                if op.address_flags.mode == AddressMode::Immediate {
                    (None, None)
                } else if op.address_flags.mode == AddressMode::Indirect {
                    let src_address = vm
                        .debug_calc_address(op.address, &AddressFlags::default(), true)
                        .unwrap();
                    (Some(address), Some(src_address))
                } else {
                    (Some(address), None)
                }
            } else {
                (None, None)
            }
        } else {
            (None, None)
        }
    }

    fn find_line_for(&self, address: u32) -> Option<&SdbLine> {
        if let Some(program) = self.find_program(address) {
            let offset = address - program.loaded_at;
            if let Some(line) = program.sdb.offset_map.get(&offset) {
                return Some(&program.sdb.lines[*line]);
            }
        }
        None
    }

    fn find_program(&self, pc: u32) -> Option<&LoadedProgram> {
        for ((start, end), program) in self.programs.iter() {
            if pc >= *start && pc <= *end {
                return Some(program);
            }
        }

        None
    }

    pub fn get_labels(&self) -> HashMap<String, HashMap<String, u32>> {
        self.labels.clone()
    }

    pub fn address_for_label(&self, program: &str, label: &str) -> Option<u32> {
        self.labels
            .get(label)
            .and_then(|progs| progs.get(program))
            .copied()
    }
}

#[derive(Clone, Debug)]
pub struct SdbDebugger {
    verbose: bool,
    tracker: RuntimeTracker,
}

impl SdbDebugger {
    pub fn new() -> Self {
        SdbDebugger {
            verbose: false,
            tracker: RuntimeTracker {
                programs: HashMap::new(),
                last_reg_state: RegState::default(),
                labels: HashMap::new(),
            },
        }
    }

    pub fn load(&mut self, loaded_at: u32, sdb: String) -> Result<(), LoadError> {
        self.tracker.load(loaded_at, sdb)
    }

    pub fn address_for_label(&self, program: &str, label: &str) -> Option<u32> {
        self.tracker.address_for_label(program, label)
    }

    pub fn get_labels(&self) -> HashMap<String, HashMap<String, u32>> {
        self.tracker.get_labels()
    }

    pub fn verbose() -> Self {
        SdbDebugger {
            verbose: true,
            tracker: RuntimeTracker {
                programs: HashMap::new(),
                last_reg_state: RegState::default(),
                labels: HashMap::new(),
            },
        }
    }
}

impl Default for SdbDebugger {
    fn default() -> Self {
        Self::new()
    }
}

impl Debugger for SdbDebugger {
    fn op_read(&mut self, vm_state: &super::vm::SicXeVm, op: &super::op::Op) {
        self.tracker.save_reg_state(vm_state);
        let pc = vm_state.PC.as_u32();
        let program = self.tracker.find_program(pc);
        print!("({:#08X}): {}", pc, op.mnemonic());
        let (target_address, _) = self.tracker.find_target_address(vm_state, op);
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
            if let Some(target_line) = self.tracker.find_line_for(target_address) {
                let target_program = self
                    .tracker
                    .find_program(target_address)
                    .expect("found line but not program");
                let name = &target_program.sdb.name;
                println!(
                    "    Target: [{}] {}: {}",
                    name, target_line.line_number, target_line.text
                );
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

        let diff = self.tracker.last_reg_state.diff(&new_reg_state).to_string();
        if !diff.is_empty() {
            println!("  {}", diff);
        }
    }

    fn interrupt(&mut self, vm_state: &super::vm::SicXeVm, interrupt: super::vm::Interrupt) {
        let pc = vm_state.PC.as_u32();
        println!("({:#08X}): Saw interrupt {:?}", pc, interrupt);
    }
}

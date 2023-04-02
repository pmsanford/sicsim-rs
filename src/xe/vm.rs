use thiserror::Error;

use super::op::{
    is_privileged, AddressFlags, AddressMode, AddressRelativeTo, OneByteOp, OneRegOp, Op, Register,
    ShiftOp, TwoRegOp, VariableOp,
};
use super::status_word::{check_cc, set_cc, supervisor_mode, SUP_BYTE, SUP_MASK};
use crate::device::Device;
use crate::word::{
    f64_to_dword, i32_to_word, u32_to_dword, u32_to_word, DWord, DWordExt, Word, WordExt,
};
use std::cmp::Ordering;
use std::mem;
use std::{collections::HashMap, fmt::Debug};

pub trait Debugger {
    fn op_read(&self, vm_state: &SicXeVm, op: &Op);
    fn op_executed(&self, vm_state: &SicXeVm, op: &Op);
    fn interrupt(&self, vm_state: &SicXeVm, interrupt: Interrupt);
}

pub struct PrintlnDebugger;

impl Debugger for PrintlnDebugger {
    fn op_read(&self, _vm_state: &SicXeVm, op: &Op) {
        //println!("Executing {:?}", op);
    }

    fn op_executed(&self, vm_state: &SicXeVm, _op: &Op) {
        //println!("-----> State after: {:?}", vm_state);
    }

    fn interrupt(&self, _vm_state: &SicXeVm, interrupt: Interrupt) {
        println!("Saw interrupt {:?}", interrupt);
    }
}

#[allow(non_snake_case)]
pub struct SicXeVm {
    pub memory: Vec<u8>,
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
    devices: HashMap<u8, Box<dyn Device>>,
    #[allow(dead_code)]
    interrupt_queue: Vec<Interrupt>,
    pub debugger: Option<Box<dyn Debugger>>,
    cycles_per_second: u64,
    cycles: u64,
    interval_cycles: u64,
}

impl Debug for SicXeVm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SicXeVm")
            .field("A", &self.A)
            .field("X", &self.X)
            .field("L", &self.L)
            .field("B", &self.B)
            .field("S", &self.S)
            .field("T", &self.T)
            .field("F", &self.F)
            .field("I", &self.I)
            .field("PC", &self.PC)
            .field("SW", &self.SW)
            .field("cycles", &self.cycles)
            .finish()
    }
}

fn simple_addr_signed(addr: u32) -> i32 {
    if addr & 0x0800 == 0 {
        addr as i32
    } else {
        (addr | 0xFFFFF800) as i32
    }
}

#[derive(Error, Debug)]
pub enum OpError {
    #[error("Address {address:0>6X} out of range")]
    AddressOutOfRange { address: usize },
    #[error("Invalid instruction at address {address:0>6X}")]
    InvalidInstruction { address: usize },
}

static MEMORY_SIZE: usize = 1 << 20;

impl SicXeVm {
    pub fn empty() -> Self {
        Self {
            memory: vec![0; MEMORY_SIZE],
            A: [0; 3],
            X: [0; 3],
            L: [0; 3],
            B: [0; 3],
            S: [0; 3],
            T: [0; 3],
            F: [0; 6],
            PC: [0; 3],
            SW: [0; 3],
            I: [0; 3],
            devices: HashMap::new(),
            interrupt_queue: Vec::new(),
            debugger: None,
            cycles_per_second: 1_000_000,
            cycles: 0,
            interval_cycles: 0,
        }
    }

    pub fn add_device(&mut self, device: Box<dyn Device>, address: u8) -> Option<Box<dyn Device>> {
        self.devices.insert(address, device)
    }

    pub fn remove_device(&mut self, address: u8) -> Option<Box<dyn Device>> {
        self.devices.remove(&address)
    }

    pub fn set_pc(&mut self, address: u32) {
        self.PC = u32_to_word(address);
    }

    fn test_device(&mut self, device_id: u8) -> bool {
        self.devices
            .get_mut(&device_id)
            .map(|device| device.test())
            .unwrap_or(false)
    }

    fn read_device(&mut self, device_id: u8) -> u8 {
        self.devices
            .get_mut(&device_id)
            .map(|device| device.read())
            .unwrap_or(0)
    }

    fn write_device(&mut self, device_id: u8, data: u8) {
        if let Some(device) = self.devices.get_mut(&device_id) {
            device.write(data);
        }
    }

    fn index_addr(&self, address: u32, flags: &AddressFlags) -> u32 {
        let idx = if flags.indexed { self.X.as_u32() } else { 0 };
        address + idx
    }

    fn calc_addr(&self, argument: u32, flags: &AddressFlags) -> Result<u32, OpError> {
        Ok(match flags.mode {
            AddressMode::Compatiblity => self.index_addr(argument, flags),
            AddressMode::Simple => match flags.relative_to {
                AddressRelativeTo::Direct => self.index_addr(argument, flags),
                AddressRelativeTo::Base => self.index_addr(self.B.as_u32() + argument, flags),
                AddressRelativeTo::PC => self.index_addr(
                    (self.PC.as_i32() + simple_addr_signed(argument)) as u32,
                    flags,
                ),
            },
            AddressMode::Immediate => match flags.relative_to {
                AddressRelativeTo::Direct => argument,
                AddressRelativeTo::Base => self.B.as_u32() + argument,
                AddressRelativeTo::PC => (self.PC.as_i32() + simple_addr_signed(argument)) as u32,
            },
            AddressMode::Indirect => match flags.relative_to {
                AddressRelativeTo::Direct => self.word_at(argument)?.as_u32(),
                AddressRelativeTo::Base => self.word_at(self.B.as_u32() + argument)?.as_u32(),
                AddressRelativeTo::PC => self
                    .word_at((self.PC.as_i32() + simple_addr_signed(argument)) as u32)?
                    .as_u32(),
            },
        })
    }

    fn cycle(&mut self) {
        self.interval_cycles += 1;
        self.cycles += 1;
    }

    fn handle_interval(&mut self) {
        let i = self.I.as_u32();
        if self.interval_cycles > (self.cycles_per_second / 1_000) && i > 0 {
            self.interval_cycles -= self.cycles_per_second / 1_000;
            self.I = u32_to_word(0.max(i - 1));
            if self.I.as_u32() == 0 {
                if let Some(ref debugger) = self.debugger {
                    debugger.interrupt(self, Interrupt::Timer);
                }
                self.handle_interrupt(Interrupt::Timer)
                    .expect("Interval interrupt");
            }
        }
    }

    fn fetch_word(&mut self, argument: u32, flags: &AddressFlags) -> Result<Word, OpError> {
        let address = self.calc_addr(argument, flags)?;
        if flags.mode == AddressMode::Immediate {
            Ok(u32_to_word(address))
        } else {
            self.cycle();
            self.word_at(address)
        }
    }

    fn fetch_dword(&mut self, argument: u32, flags: &AddressFlags) -> Result<DWord, OpError> {
        let address = self.calc_addr(argument, flags)?;
        if flags.mode == AddressMode::Immediate {
            Ok(u32_to_dword(address))
        } else {
            self.cycle();
            self.dword_at(address)
        }
    }

    fn check_address_range(&self, address: usize) -> Result<(), OpError> {
        if address >= MEMORY_SIZE {
            Err(OpError::AddressOutOfRange { address })
        } else {
            Ok(())
        }
    }

    fn word_at(&self, address: u32) -> Result<Word, OpError> {
        let address = address as usize;
        self.check_address_range(address + 2)?;
        Ok([
            self.memory[address],
            self.memory[address + 1],
            self.memory[address + 2],
        ])
    }

    fn dword_at(&self, address: u32) -> Result<DWord, OpError> {
        let address = address as usize;
        self.check_address_range(address + 5)?;
        Ok([
            self.memory[address as usize],
            self.memory[address as usize + 1],
            self.memory[address as usize + 2],
            self.memory[address as usize + 3],
            self.memory[address as usize + 4],
            self.memory[address as usize + 5],
        ])
    }

    fn set_register(&mut self, register: &Register, value: Word) {
        match register {
            Register::A => self.A = value,
            Register::X => self.X = value,
            Register::L => self.L = value,
            Register::B => self.B = value,
            Register::S => self.S = value,
            Register::T => self.T = value,
            Register::PC => self.PC = value,
            Register::SW => self.SW = value,
            Register::F => {
                self.F[0] = value[0];
                self.F[1] = value[1];
                self.F[2] = value[2];
            }
        }
    }

    fn get_register(&self, register: &Register) -> Word {
        match register {
            Register::A => self.A,
            Register::X => self.X,
            Register::L => self.L,
            Register::B => self.B,
            Register::S => self.S,
            Register::T => self.T,
            Register::PC => self.PC,
            Register::SW => self.SW,
            Register::F => [self.F[0], self.F[1], self.F[2]],
        }
    }

    pub fn set_at(
        &mut self,
        address: u32,
        flags: &AddressFlags,
        value: Word,
    ) -> Result<(), OpError> {
        //TODO: What does immediate mean here
        let address = self.calc_addr(address, flags)?;
        self.memory[address as usize] = value[0];
        self.memory[address as usize + 1] = value[1];
        self.memory[address as usize + 2] = value[2];
        self.cycle();

        Ok(())
    }

    fn set_dword_at(
        &mut self,
        address: u32,
        flags: &AddressFlags,
        value: DWord,
    ) -> Result<(), OpError> {
        //TODO: What does immediate mean here
        let address = self.calc_addr(address, flags)?;
        self.memory[address as usize] = value[0];
        self.memory[address as usize + 1] = value[1];
        self.memory[address as usize + 2] = value[2];
        self.memory[address as usize + 3] = value[3];
        self.memory[address as usize + 4] = value[4];
        self.memory[address as usize + 5] = value[5];
        self.cycle();

        Ok(())
    }

    fn pc_address(&self) -> u32 {
        self.PC.as_u32()
    }

    fn comp(&mut self, register: Word, address: u32, flags: &AddressFlags) -> Result<(), OpError> {
        let memory = self.fetch_word(address, flags)?;
        set_cc(&mut self.SW, register.cmp(&memory));

        Ok(())
    }

    fn compf(
        &mut self,
        register: DWord,
        address: u32,
        flags: &AddressFlags,
    ) -> Result<(), OpError> {
        let memory = self.fetch_dword(address, flags)?;
        set_cc(&mut self.SW, register.cmp(&memory));

        Ok(())
    }

    fn compr(&mut self, r1: &Register, r2: &Register) {
        let r1_val = self.get_register(r1);
        let r2_val = self.get_register(r2);
        set_cc(&mut self.SW, r1_val.cmp(&r2_val));
    }

    pub fn run_until(&mut self, max_cycles: u64) -> StopReason {
        let mut pc: Option<Word> = None;
        loop {
            self.step();
            if let Some(prev) = pc {
                if prev == self.PC {
                    break StopReason::Halted;
                }
            }
            if self.cycles >= max_cycles {
                break StopReason::CycleLimit;
            }
            pc = Some(self.PC);
        }
    }

    fn get_op_at_pc(&mut self) -> Result<Op, OpError> {
        let pc = self.pc_address() as usize;
        self.check_address_range(pc + 3)?;
        // cycle for fetch
        self.cycle();
        let bytes = [
            self.memory[pc],
            self.memory[pc + 1],
            self.memory[pc + 2],
            self.memory[pc + 3],
        ];
        // cycle for decode
        self.cycle();
        Op::from_bytes(bytes).ok_or(OpError::InvalidInstruction { address: pc })
    }

    fn program_interrupt(&mut self, error: OpError) {
        if let Some(ref debugger) = self.debugger {
            debugger.interrupt(self, Interrupt::Program);
        }
        match error {
            OpError::AddressOutOfRange { .. } => {
                // Code 02 = Address out of range
                self.SW[2] = 2;
                self.handle_interrupt(Interrupt::Program)
                    .expect("Error in interrupt handler");
            }
            OpError::InvalidInstruction { .. } => {
                // Code 00 = Illegal instruction
                self.SW[2] = 0;
                self.handle_interrupt(Interrupt::Program)
                    .expect("Error in interrupt handler");
            }
        }
    }

    fn check_interrupt_queue(&mut self) -> Result<(), OpError> {
        let mut next_interrupt = None;
        let mut new_queue = vec![];
        for interrupt in self.interrupt_queue.drain(..) {
            if next_interrupt.is_none() && !interrupt.is_masked(&self.SW) {
                next_interrupt = Some(interrupt);
                break;
            } else {
                new_queue.push(interrupt);
            }
        }

        new_queue.reverse();

        mem::swap(&mut self.interrupt_queue, &mut new_queue);

        if let Some(interrupt) = next_interrupt {
            self.handle_interrupt(interrupt)?;
        }

        Ok(())
    }

    pub fn step(&mut self) {
        if let Err(err) = self.check_interrupt_queue() {
            self.program_interrupt(err);
        }
        let op = self.get_op_at_pc();
        if op.as_ref().map(is_privileged).unwrap_or(false) && !supervisor_mode(&self.SW) {
            // TODO: Interrupt, privileged instruction
            if let Some(ref debugger) = self.debugger {
                debugger.interrupt(self, Interrupt::Program);
            }
            self.SW[2] = 1;
            self.handle_interrupt(Interrupt::Program)
                .expect("Error in interrupt handler");
        }
        match op {
            Ok(op) => {
                if let Some(ref debugger) = self.debugger {
                    debugger.op_read(self, &op);
                }
                self.PC = u32_to_word(self.PC.as_u32() + op.len());
                if is_privileged(&op) && !supervisor_mode(&self.SW) {
                    if let Some(ref debugger) = self.debugger {
                        debugger.interrupt(self, Interrupt::Program);
                    }
                    self.SW[2] = 1;
                    self.handle_interrupt(Interrupt::Program)
                        .expect("Error in interrupt handler");
                }
                if let Err(err) = self.run_op(&op) {
                    self.program_interrupt(err);
                }
                if let Some(ref debugger) = self.debugger {
                    debugger.op_executed(self, &op);
                }
            }
            Err(error) => {
                self.program_interrupt(error);
            }
        }
        self.handle_interval();
    }

    fn run_op(&mut self, op: &Op) -> Result<(), OpError> {
        self.cycle();
        match op {
            Op::Variable(op) => {
                match op.opcode {
                    // Load/store
                    VariableOp::LDA => {
                        self.A = self.fetch_word(op.address, &op.address_flags)?;
                    }
                    VariableOp::STA => {
                        self.set_at(op.address, &op.address_flags, self.A)?;
                    }
                    VariableOp::LDCH => {
                        let address = self.calc_addr(op.address, &op.address_flags)?;
                        self.A[2] = self.memory[address as usize];
                    }
                    VariableOp::STCH => {
                        let address = self.calc_addr(op.address, &op.address_flags)?;
                        self.memory[address as usize] = self.A[2];
                    }
                    VariableOp::LDL => {
                        self.L = self.fetch_word(op.address, &op.address_flags)?;
                    }
                    VariableOp::STL => {
                        self.set_at(op.address, &op.address_flags, self.L)?;
                    }
                    VariableOp::LDB => {
                        self.B = self.fetch_word(op.address, &op.address_flags)?;
                    }
                    VariableOp::STB => {
                        self.set_at(op.address, &op.address_flags, self.B)?;
                    }
                    VariableOp::LDS => {
                        self.S = self.fetch_word(op.address, &op.address_flags)?;
                    }
                    VariableOp::STS => {
                        self.set_at(op.address, &op.address_flags, self.S)?;
                    }
                    VariableOp::LDT => {
                        self.T = self.fetch_word(op.address, &op.address_flags)?;
                    }
                    VariableOp::STT => {
                        self.set_at(op.address, &op.address_flags, self.T)?;
                    }
                    VariableOp::LDF => {
                        self.F = self.fetch_dword(op.address, &op.address_flags)?;
                    }
                    VariableOp::STF => {
                        self.set_dword_at(op.address, &op.address_flags, self.F)?;
                    }
                    VariableOp::LDX => {
                        self.X = self.fetch_word(op.address, &op.address_flags)?;
                    }
                    VariableOp::STX => {
                        self.set_at(op.address, &op.address_flags, self.X)?;
                    }
                    VariableOp::STSW => {
                        self.set_at(op.address, &op.address_flags, self.SW)?;
                    }

                    // Bitwise
                    VariableOp::OR => {
                        let memory = self.fetch_word(op.address, &op.address_flags)?;
                        self.A = [
                            self.A[0] | memory[0],
                            self.A[1] | memory[1],
                            self.A[2] | memory[2],
                        ];
                    }
                    VariableOp::AND => {
                        let memory = self.fetch_word(op.address, &op.address_flags)?;
                        self.A = [
                            self.A[0] & memory[0],
                            self.A[1] & memory[1],
                            self.A[2] & memory[2],
                        ];
                    }

                    // Math
                    VariableOp::ADD => {
                        let memory = self.fetch_word(op.address, &op.address_flags)?;
                        self.A = u32_to_word(self.A.as_u32().wrapping_add(memory.as_u32()));
                    }
                    VariableOp::ADDF => {
                        let memory = self.fetch_dword(op.address, &op.address_flags)?;
                        self.F = f64_to_dword(self.F.as_f64() + memory.as_f64());
                    }
                    VariableOp::SUB => {
                        let memory = self.fetch_word(op.address, &op.address_flags)?;
                        self.A = u32_to_word(self.A.as_u32().wrapping_sub(memory.as_u32()));
                    }
                    VariableOp::SUBF => {
                        let memory = self.fetch_dword(op.address, &op.address_flags)?;
                        self.F = f64_to_dword(self.F.as_f64() - memory.as_f64());
                    }
                    VariableOp::MUL => {
                        let memory = self.fetch_word(op.address, &op.address_flags)?;
                        self.A = u32_to_word(self.A.as_u32().wrapping_mul(memory.as_u32()));
                    }
                    VariableOp::MULF => {
                        let memory = self.fetch_dword(op.address, &op.address_flags)?;
                        self.F = f64_to_dword(self.F.as_f64() * memory.as_f64());
                    }
                    VariableOp::DIV => {
                        let memory = self.fetch_word(op.address, &op.address_flags)?;
                        self.A = u32_to_word(self.A.as_u32().wrapping_div(memory.as_u32()));
                    }
                    VariableOp::DIVF => {
                        let memory = self.fetch_dword(op.address, &op.address_flags)?;
                        self.F = f64_to_dword(self.F.as_f64() / memory.as_f64());
                    }

                    // Comp
                    VariableOp::COMP => {
                        self.comp(self.A, op.address, &op.address_flags)?;
                    }
                    VariableOp::COMPF => {
                        self.compf(self.F, op.address, &op.address_flags)?;
                    }

                    // Jumps
                    VariableOp::J => {
                        self.PC = u32_to_word(self.calc_addr(op.address, &op.address_flags)?);
                    }
                    VariableOp::JLT => {
                        if check_cc(&self.SW) == Ordering::Less {
                            self.PC = u32_to_word(self.calc_addr(op.address, &op.address_flags)?);
                        }
                    }
                    VariableOp::JEQ => {
                        if check_cc(&self.SW) == Ordering::Equal {
                            self.PC = u32_to_word(self.calc_addr(op.address, &op.address_flags)?);
                        }
                    }
                    VariableOp::JGT => {
                        if check_cc(&self.SW) == Ordering::Greater {
                            self.PC = u32_to_word(self.calc_addr(op.address, &op.address_flags)?);
                        }
                    }

                    // Subroutines
                    VariableOp::JSUB => {
                        self.L = self.PC;
                        self.PC = u32_to_word(self.calc_addr(op.address, &op.address_flags)?);
                    }
                    VariableOp::RSUB => {
                        self.PC = self.L;
                    }

                    // Index
                    VariableOp::TIX => {
                        self.X = u32_to_word(self.X.as_u32() + 1);
                        self.comp(self.X, op.address, &op.address_flags)?;
                    }

                    // Devices
                    VariableOp::RD => {
                        let device_id =
                            self.memory[self.calc_addr(op.address, &op.address_flags)? as usize];
                        self.A[2] = self.read_device(device_id);
                    }
                    VariableOp::TD => {
                        let device_id =
                            self.memory[self.calc_addr(op.address, &op.address_flags)? as usize];
                        if self.test_device(device_id) {
                            set_cc(&mut self.SW, Ordering::Less);
                        } else {
                            set_cc(&mut self.SW, Ordering::Equal);
                        }
                    }
                    VariableOp::WD => {
                        let device_id =
                            self.memory[self.calc_addr(op.address, &op.address_flags)? as usize];
                        self.write_device(device_id, self.A[2]);
                    }

                    // System
                    VariableOp::SSK => todo!(),
                    VariableOp::LPS => {
                        //TODO: Treat these like instructions that increment the cycle count etc
                        let address = self.calc_addr(op.address, &op.address_flags)?;

                        self.SW = self.word_at(address + 6)?;
                        self.PC = self.word_at(address + 9)?;

                        self.A = self.word_at(address + 12)?;
                        self.X = self.word_at(address + 15)?;
                        self.L = self.word_at(address + 18)?;
                        self.B = self.word_at(address + 21)?;
                        self.S = self.word_at(address + 24)?;
                        self.T = self.word_at(address + 27)?;
                        self.F = self.dword_at(address + 30)?;
                    }
                    VariableOp::STI => {
                        self.I = self.fetch_word(op.address, &op.address_flags)?;
                    }
                }
            }
            Op::OneByte(opcode) => match opcode {
                OneByteOp::FIX => {
                    let float = self.F.as_f64();
                    self.A = i32_to_word(float as i32);
                }
                OneByteOp::FLOAT => {
                    let int = self.A.as_i32();
                    self.F = f64_to_dword(int as f64);
                }
                OneByteOp::HIO => todo!(),
                OneByteOp::NORM => {
                    // NB: The only way this does anything is if the floats
                    // are manipulated outside of the floating point
                    // instructions - they always produce normalized floats
                    let f = self.F.as_f64();
                    self.F = f64_to_dword(f);
                }
                OneByteOp::SIO => todo!(),
                OneByteOp::TIO => todo!(),
            },
            Op::OneReg(one_reg) => match one_reg.opcode {
                OneRegOp::CLEAR => {
                    self.set_register(&one_reg.r1, u32_to_word(0));
                }
                OneRegOp::TIXR => {
                    self.X = u32_to_word(self.X.as_u32() + 1);
                    self.compr(&Register::X, &one_reg.r1);
                }
            },
            Op::TwoReg(two_reg) => match two_reg.opcode {
                TwoRegOp::ADDR => {
                    self.set_register(
                        &two_reg.r2,
                        u32_to_word(
                            self.get_register(&two_reg.r2).as_u32()
                                + self.get_register(&two_reg.r1).as_u32(),
                        ),
                    );
                }
                TwoRegOp::COMPR => {
                    let r1 = self.get_register(&two_reg.r1).as_u32();
                    let r2 = self.get_register(&two_reg.r2).as_u32();
                    set_cc(&mut self.SW, r1.cmp(&r2));
                }
                TwoRegOp::DIVR => {
                    self.set_register(
                        &two_reg.r2,
                        u32_to_word(
                            self.get_register(&two_reg.r2).as_u32()
                                / self.get_register(&two_reg.r1).as_u32(),
                        ),
                    );
                }
                TwoRegOp::MULR => {
                    self.set_register(
                        &two_reg.r2,
                        u32_to_word(
                            self.get_register(&two_reg.r2).as_u32()
                                * self.get_register(&two_reg.r1).as_u32(),
                        ),
                    );
                }
                TwoRegOp::RMO => {
                    self.set_register(&two_reg.r2, self.get_register(&two_reg.r1));
                }
                TwoRegOp::SUBR => {
                    self.set_register(
                        &two_reg.r2,
                        u32_to_word(
                            self.get_register(&two_reg.r2).as_u32()
                                - self.get_register(&two_reg.r1).as_u32(),
                        ),
                    );
                }
            },
            Op::Shift(shift) => match shift.opcode {
                ShiftOp::SHIFTL => {
                    self.set_register(
                        &shift.r1,
                        circular_shiftl(self.get_register(&shift.r1), shift.n + 1),
                    );
                }
                ShiftOp::SHIFTR => {
                    self.set_register(
                        &shift.r1,
                        filling_shiftr(self.get_register(&shift.r1), shift.n + 1),
                    );
                }
            },
            // Described on p333
            Op::Svc(n) => {
                self.SW[2] = *n;
                self.handle_interrupt(Interrupt::Svc)?;
            }
        }

        Ok(())
    }

    fn handle_interrupt(&mut self, interrupt: Interrupt) -> Result<(), OpError> {
        //TODO: Treat these like instructions that increment the cycle count etc
        if interrupt.is_masked(&self.SW) {
            self.interrupt_queue.push(interrupt);
            return Ok(());
        }
        let work_area = interrupt.work_area();

        self.set_at(work_area + 6, &AddressFlags::immediate(), self.SW)?;
        self.set_at(work_area + 9, &AddressFlags::immediate(), self.PC)?;

        self.set_at(work_area + 12, &AddressFlags::immediate(), self.A)?;
        self.set_at(work_area + 15, &AddressFlags::immediate(), self.X)?;
        self.set_at(work_area + 18, &AddressFlags::immediate(), self.L)?;
        self.set_at(work_area + 21, &AddressFlags::immediate(), self.B)?;
        self.set_at(work_area + 24, &AddressFlags::immediate(), self.S)?;
        self.set_at(work_area + 27, &AddressFlags::immediate(), self.T)?;
        self.set_dword_at(work_area + 30, &AddressFlags::immediate(), self.F)?;

        self.SW = self.word_at(work_area)?;
        self.PC = self.word_at(work_area + 3)?;

        Ok(())
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub enum Interrupt {
    Svc,
    Program,
    Timer,
    Io,
}

impl Interrupt {
    fn work_area(&self) -> u32 {
        match self {
            Interrupt::Svc => 0x100,
            Interrupt::Program => 0x130,
            Interrupt::Timer => 0x160,
            Interrupt::Io => 0x190,
        }
    }

    fn is_masked(&self, sw: &Word) -> bool {
        let mask = match self {
            Interrupt::Svc => 0x80,
            Interrupt::Program => 0x40,
            Interrupt::Timer => 0x20,
            Interrupt::Io => 0x10,
        };

        sw[1] & mask > 0
    }
}

fn filling_shiftr(mut val: Word, mut n: u8) -> Word {
    while n > 0 {
        let top_bit = val[0] & 0x80;
        let [_, a, b, c] = (val.as_u32() >> 1).to_be_bytes();
        val = [a + top_bit, b, c];
        n -= 1;
    }

    val
}

fn circular_shiftl(mut val: Word, mut n: u8) -> Word {
    while n > 0 {
        let [carry, a, b, mut c] = (val.as_u32() << 1).to_be_bytes();
        if (carry & 1) == 1 {
            c += 1;
        }
        val = [a, b, c];
        n -= 1;
    }

    val
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum StopReason {
    Halted,
    CycleLimit,
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, ops::Deref, rc::Rc};

    use crate::{
        word::i16_to_exponent,
        xe::{
            op::{AddressMode, OneReg, OneRegOp, Register, Shift, TwoReg, TwoRegOp, Variable},
            status_word::{CC_BYTE, CC_EQ, CC_GT, CC_LT, CC_MASK, SUP_BYTE, SUP_MASK},
        },
    };

    use super::*;

    #[test]
    fn shiftr() {
        let val = u32_to_word(2);
        assert_eq!(filling_shiftr(val, 1).as_u32(), 1);
        let val = i32_to_word(-2);
        assert_eq!(filling_shiftr(val, 1).as_i32(), -1);
        let val = [0x80, 0x00, 0x0F];
        assert_eq!(filling_shiftr(val, 8), [0xFF, 0x80, 0x00]);
    }

    #[test]
    fn shiftl() {
        let val = u32_to_word(1);
        assert_eq!(circular_shiftl(val, 1).as_u32(), 2);
        let val = u32_to_word(0x800000);
        assert_eq!(circular_shiftl(val, 1).as_u32(), 1);
        let val = u32_to_word(0xC00000);
        assert_eq!(circular_shiftl(val, 2).as_u32(), 3);
        let val = [0xAB, 0xCD, 0xEF];
        assert_eq!(circular_shiftl(val, 8), [0xCD, 0xEF, 0xAB]);
    }

    fn set_int(vm: &mut SicXeVm, address: usize, v: u32) {
        let [_, a, b, c] = v.to_be_bytes();
        vm.memory[address] = a;
        vm.memory[address + 1] = b;
        vm.memory[address + 2] = c;
    }

    fn set_float(vm: &mut SicXeVm, address: usize, v: f64) {
        let [a, b, c, d, e, f] = f64_to_dword(v);
        vm.memory[address] = a;
        vm.memory[address + 1] = b;
        vm.memory[address + 2] = c;
        vm.memory[address + 3] = d;
        vm.memory[address + 4] = e;
        vm.memory[address + 5] = f;
    }

    fn set_op(vm: &mut SicXeVm, address: usize, op: Op) {
        let word = op.to_bytes();
        vm.memory[address] = word[0];
        vm.memory[address + 1] = word[1];
        vm.memory[address + 2] = word[2];
    }

    fn ni_op(vm: &mut SicXeVm, address: usize, opcode: VariableOp, target: u32) {
        set_op(
            vm,
            address,
            Op::Variable(Variable {
                opcode,
                address_flags: AddressFlags {
                    mode: AddressMode::Compatiblity,
                    indexed: false,
                    relative_to: AddressRelativeTo::Direct,
                    extended: false,
                },
                address: target,
            }),
        );
    }

    #[allow(dead_code)]
    fn print_word(vm: &SicXeVm, address: usize) {
        println!(
            "word at {}: 0x{:X} 0x{:X} 0x{:X}",
            address,
            vm.memory[address],
            vm.memory[address + 1],
            vm.memory[address + 2]
        );
    }

    fn two_reg_op(vm: &mut SicXeVm, address: usize, opcode: TwoRegOp, r1: Register, r2: Register) {
        set_op(vm, address, Op::TwoReg(TwoReg { opcode, r1, r2 }));
    }

    fn one_reg_op(vm: &mut SicXeVm, address: usize, opcode: OneRegOp, r1: Register) {
        set_op(vm, address, Op::OneReg(OneReg { opcode, r1 }));
    }

    fn setup_two_op(opcode: TwoRegOp, r1: Register, r2: Register) -> SicXeVm {
        let mut vm = SicXeVm::empty();
        set_op(&mut vm, 0, Op::TwoReg(TwoReg { opcode, r1, r2 }));
        vm
    }

    fn setup_var_op(opcode: VariableOp, address: u32) -> SicXeVm {
        let mut vm = SicXeVm::empty();
        set_op(
            &mut vm,
            0,
            Op::Variable(Variable {
                opcode,
                address_flags: AddressFlags {
                    mode: AddressMode::Compatiblity,
                    indexed: false,
                    relative_to: AddressRelativeTo::Direct,
                    extended: false,
                },
                address,
            }),
        );
        vm
    }

    #[test]
    fn lda() {
        let mut vm = setup_var_op(VariableOp::LDA, 3);
        set_int(&mut vm, 3, 0xAB);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xAB);
        assert_eq!(vm.PC[2], 3);

        let mut vm = SicXeVm::empty();
        vm.X = u32_to_word(3);
        set_op(
            &mut vm,
            0,
            Op::Variable(Variable {
                opcode: VariableOp::LDA,
                address_flags: AddressFlags {
                    mode: AddressMode::Compatiblity,
                    indexed: true,
                    relative_to: AddressRelativeTo::Direct,
                    extended: false,
                },
                address: 3,
            }),
        );
        set_int(&mut vm, 3, 0xAB);
        set_int(&mut vm, 6, 0xBA);
        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xBA);
        assert_eq!(vm.PC[2], 3);
    }

    static NI_FLAGS: AddressFlags = AddressFlags {
        mode: AddressMode::Compatiblity,
        indexed: false,
        relative_to: AddressRelativeTo::Direct,
        extended: false,
    };

    static I_FLAGS: AddressFlags = AddressFlags {
        mode: AddressMode::Compatiblity,
        indexed: true,
        relative_to: AddressRelativeTo::Direct,
        extended: false,
    };

    #[test]
    fn sta() {
        let mut vm = setup_var_op(VariableOp::STA, 3);
        vm.A = u32_to_word(0xAAAA);

        vm.step();

        let a_val: u32 = vm.word_at(3).unwrap().as_u32();
        assert_eq!(a_val, 0xAAAA);
        assert_eq!(vm.PC[2], 3);
    }

    #[test]
    fn load_store() {
        let mut vm = setup_var_op(VariableOp::LDA, 99);
        set_op(
            &mut vm,
            3,
            Op::Variable(Variable {
                opcode: VariableOp::STA,
                address_flags: NI_FLAGS.clone(),
                address: 102,
            }),
        );
        set_int(&mut vm, 99, 0xBEEF);

        vm.step();
        vm.step();

        let stored: u32 = vm.word_at(102).unwrap().as_u32();
        assert_eq!(stored, 0xBEEF);
    }

    #[test]
    fn ch() {
        let mut vm = setup_var_op(VariableOp::LDCH, 100);
        set_int(&mut vm, 99, 0xBE00);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0xBE);
        let mut vm = setup_var_op(VariableOp::STCH, 100);
        vm.A = u32_to_word(0xBA);

        vm.step();

        let m_val: u32 = vm.word_at(98).unwrap().as_u32();
        assert_eq!(m_val, 0xBA);
    }

    #[test]
    fn bitwise() {
        let mut vm = setup_var_op(VariableOp::OR, 99);
        set_int(&mut vm, 99, 0x8181);
        vm.A = u32_to_word(0x1818);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0x9999);

        let mut vm = setup_var_op(VariableOp::AND, 99);
        set_int(&mut vm, 99, 0x8181);
        vm.A = u32_to_word(0xFFFF);

        vm.step();

        let a_val: u32 = vm.A.as_u32();
        assert_eq!(a_val, 0x8181);
    }

    #[test]
    fn overflow() {
        let mut vm = setup_var_op(VariableOp::ADD, 99);
        set_int(&mut vm, 99, 0xFF_FF_FF);
        vm.A = u32_to_word(1);

        vm.step();
        assert_eq!(vm.A.as_i32(), 0);
    }

    #[test]
    fn math() {
        let mut vm = setup_var_op(VariableOp::ADD, 99);
        set_int(&mut vm, 99, (-32 as i32) as u32);
        vm.A = u32_to_word(32);

        vm.step();

        assert_eq!(vm.A.as_i32(), 0);
        vm.PC = u32_to_word(0);
        vm.step();
        assert_eq!(vm.A.as_i32(), -32);
        set_int(&mut vm, 99, 64);
        vm.PC = u32_to_word(0);
        vm.step();
        assert_eq!(vm.A.as_i32(), 32);

        let mut vm = setup_var_op(VariableOp::SUB, 99);
        set_int(&mut vm, 99, 10);
        vm.A = u32_to_word(5);

        vm.step();

        assert_eq!(vm.A.as_i32(), -5);

        let mut vm = setup_var_op(VariableOp::MUL, 99);
        set_int(&mut vm, 99, 10);
        vm.A = u32_to_word(5);

        vm.step();

        assert_eq!(vm.A.as_i32(), 50);

        let mut vm = setup_var_op(VariableOp::DIV, 99);
        set_int(&mut vm, 99, 5);
        vm.A = u32_to_word(10);

        vm.step();

        assert_eq!(vm.A.as_i32(), 2);
    }

    #[test]
    fn jumps() {
        let mut vm = setup_var_op(VariableOp::J, 33);
        set_int(&mut vm, 99, 33);
        ni_op(&mut vm, 33, VariableOp::LDA, 99);

        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 33);

        // Comp JEQ EQ
        let mut vm = setup_var_op(VariableOp::COMP, 99);
        ni_op(&mut vm, 3, VariableOp::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 33);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_EQ);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 33);

        // Comp JEQ LT
        let mut vm = setup_var_op(VariableOp::COMP, 99);
        ni_op(&mut vm, 3, VariableOp::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_LT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JEQ GT
        let mut vm = setup_var_op(VariableOp::COMP, 99);
        ni_op(&mut vm, 3, VariableOp::JEQ, 33);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_GT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 6);

        // Comp JLT
        let mut vm = setup_var_op(VariableOp::COMP, 99);
        ni_op(&mut vm, 3, VariableOp::JLT, 34);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 34);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_LT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 34);

        // Comp JGT
        let mut vm = setup_var_op(VariableOp::COMP, 99);
        ni_op(&mut vm, 3, VariableOp::JGT, 32);
        vm.A = u32_to_word(33);
        set_int(&mut vm, 99, 32);
        vm.step();
        assert_eq!(vm.SW[CC_BYTE] & CC_MASK, CC_GT);
        vm.step();
        assert_eq!(vm.PC.as_u32(), 32);
    }

    #[test]
    fn subroutines() {
        let mut vm = setup_var_op(VariableOp::JSUB, 99);
        ni_op(&mut vm, 99, VariableOp::LDA, 199);
        ni_op(&mut vm, 102, VariableOp::RSUB, 0);
        ni_op(&mut vm, 3, VariableOp::STA, 6);
        set_int(&mut vm, 199, 1234);
        set_int(&mut vm, 9, 99);

        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.word_at(6).unwrap().as_u32(), 1234);
    }

    #[test]
    fn index() {
        let mut vm = SicXeVm::empty();
        set_op(
            &mut vm,
            0,
            Op::Variable(Variable {
                opcode: VariableOp::ADD,
                address_flags: I_FLAGS.clone(),
                address: 99,
            }),
        );
        ni_op(&mut vm, 3, VariableOp::TIX, 96);
        ni_op(&mut vm, 6, VariableOp::TIX, 96);
        ni_op(&mut vm, 9, VariableOp::TIX, 96);
        ni_op(&mut vm, 12, VariableOp::JLT, 0);
        set_int(&mut vm, 93, 0);
        set_int(&mut vm, 96, 15);
        set_int(&mut vm, 99, 1);
        set_int(&mut vm, 102, 2);
        set_int(&mut vm, 105, 3);
        set_int(&mut vm, 108, 4);
        set_int(&mut vm, 111, 5);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 1);
        assert_eq!(vm.X.as_u32(), 3);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 3);
        assert_eq!(vm.X.as_u32(), 6);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 6);
        assert_eq!(vm.X.as_u32(), 9);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 10);
        assert_eq!(vm.X.as_u32(), 12);
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.A.as_u32(), 15);
        assert_eq!(vm.X.as_u32(), 15);
        assert_eq!(vm.PC.as_u32(), 15);
    }

    struct TestDevice {
        tested: bool,
        content: String,
        pointer: usize,
        write_buf: Rc<RefCell<Vec<u8>>>,
    }

    impl Device for TestDevice {
        fn test(&mut self) -> bool {
            if self.tested {
                true
            } else {
                self.tested = true;
                false
            }
        }

        fn read(&mut self) -> u8 {
            let d = self.content.chars().skip(self.pointer).next().unwrap() as u8;
            self.pointer = self.pointer + 1;
            if self.pointer >= self.content.len() {
                self.pointer = 0;
            }
            d
        }

        fn write(&mut self, data: u8) {
            self.write_buf.deref().borrow_mut().push(data);
        }
    }

    // This is a hack to get these tests to pass while supervisor mode is
    // being implemented.
    fn set_supervisor_mode(vm: &mut SicXeVm) {
        vm.SW[SUP_BYTE] |= SUP_MASK;
    }

    #[test]
    fn devices() {
        let mut vm = setup_var_op(VariableOp::TD, 99);
        let write_buf = Rc::new(RefCell::new(Vec::new()));
        vm.devices.insert(
            1,
            Box::new(TestDevice {
                tested: false,
                content: String::from("Hello"),
                pointer: 0,
                write_buf: Rc::clone(&write_buf),
            }),
        );
        set_supervisor_mode(&mut vm);
        vm.memory[99] = 1;
        set_int(&mut vm, 108, 5);
        set_int(&mut vm, 206, 1234);
        ni_op(&mut vm, 3, VariableOp::JLT, 9);
        ni_op(&mut vm, 6, VariableOp::J, 0);
        ni_op(&mut vm, 9, VariableOp::RD, 99);
        ni_op(&mut vm, 12, VariableOp::TD, 99);
        ni_op(&mut vm, 15, VariableOp::JLT, 21);
        ni_op(&mut vm, 18, VariableOp::J, 12);
        ni_op(&mut vm, 21, VariableOp::WD, 99);
        ni_op(&mut vm, 24, VariableOp::TIX, 108);
        ni_op(&mut vm, 27, VariableOp::JLT, 0);
        ni_op(&mut vm, 30, VariableOp::LDA, 206);

        vm.step();
        vm.step();
        vm.step();
        assert_eq!(vm.PC.as_u32(), 0);

        vm.step();
        vm.step();
        vm.step();

        assert_eq!(vm.PC.as_u32(), 12);

        assert_eq!(vm.A[2], 'H' as u8);

        for _ in 0..1000 {
            if vm.A.as_u32() == 1234 {
                break;
            }
            vm.step();
        }

        let buf = write_buf.deref().borrow();

        assert_eq!(*buf, vec![72, 101, 108, 108, 111]);
    }

    #[test]
    fn floats() {
        let mut vm = setup_var_op(VariableOp::LDF, 99);
        ni_op(&mut vm, 3, VariableOp::ADDF, 99);
        ni_op(&mut vm, 6, VariableOp::MULF, 105);
        ni_op(&mut vm, 9, VariableOp::DIVF, 105);
        ni_op(&mut vm, 12, VariableOp::STF, 111);
        ni_op(&mut vm, 15, VariableOp::COMPF, 105);
        set_op(&mut vm, 18, Op::OneByte(OneByteOp::FIX));
        set_op(&mut vm, 19, Op::OneByte(OneByteOp::FLOAT));
        set_op(&mut vm, 20, Op::OneByte(OneByteOp::NORM));
        set_float(&mut vm, 99, 10.5);
        set_float(&mut vm, 105, 2.5);
        vm.step();
        assert_eq!(vm.F.as_f64(), 10.5);
        vm.step();
        assert_eq!(vm.F.as_f64(), 21.0);
        vm.step();
        assert_eq!(vm.F.as_f64(), 52.5);
        vm.step();
        assert_eq!(vm.F.as_f64(), 21.0);
        vm.step();
        let result = vm.dword_at(111).unwrap().as_f64();
        assert_eq!(result, 21.0);
        vm.step();
        assert_eq!(check_cc(&vm.SW), Ordering::Greater);
        vm.step();
        assert_eq!(vm.A.as_u32(), 21);
        vm.A = u32_to_word(40);
        vm.step();
        assert_eq!(vm.F.as_f64(), 40.0);
        let [exp_u, exp_l] = i16_to_exponent(1);
        vm.F = [exp_u, exp_l + 0x04, 0, 0, 0, 0];
        vm.step();
        assert_eq!(vm.F.as_f64(), 0.5);
        let [_, exp_l] = i16_to_exponent(0);
        assert_eq!(vm.F[1], exp_l + 0x08);
    }

    #[test]
    fn register_ops() {
        let mut vm = setup_two_op(TwoRegOp::ADDR, Register::A, Register::S);
        vm.A = u32_to_word(1);
        vm.S = u32_to_word(1);
        one_reg_op(&mut vm, 2, OneRegOp::CLEAR, Register::S);
        two_reg_op(&mut vm, 4, TwoRegOp::DIVR, Register::A, Register::S);
        two_reg_op(&mut vm, 6, TwoRegOp::MULR, Register::A, Register::S);
        two_reg_op(&mut vm, 8, TwoRegOp::RMO, Register::A, Register::S);
        two_reg_op(&mut vm, 10, TwoRegOp::SUBR, Register::A, Register::S);
        one_reg_op(&mut vm, 12, OneRegOp::TIXR, Register::A);
        one_reg_op(&mut vm, 14, OneRegOp::TIXR, Register::A);
        vm.step();
        assert_eq!(vm.S.as_u32(), 2);
        vm.step();
        assert_eq!(vm.S.as_u32(), 0);
        vm.S = u32_to_word(4);
        vm.A = u32_to_word(2);
        vm.step();
        assert_eq!(vm.S.as_u32(), 2);
        vm.A = u32_to_word(3);
        vm.step();
        assert_eq!(vm.S.as_u32(), 6);
        vm.step();
        assert_eq!(vm.S.as_u32(), 3);
        vm.A = u32_to_word(2);
        vm.step();
        assert_eq!(vm.S.as_u32(), 1);
        vm.A = u32_to_word(1);
        vm.step();
        assert_eq!(vm.X.as_u32(), 1);
        assert_eq!(check_cc(&vm.SW), Ordering::Equal);
        vm.step();
        assert_eq!(vm.X.as_u32(), 2);
        assert_eq!(check_cc(&vm.SW), Ordering::Greater);
    }

    #[test]
    fn shifts() {
        let mut vm = SicXeVm::empty();
        set_op(
            &mut vm,
            0,
            Op::Shift(Shift {
                opcode: ShiftOp::SHIFTL,
                r1: Register::A,
                n: 1, // = n - 1
            }),
        );
        set_op(
            &mut vm,
            2,
            Op::Shift(Shift {
                opcode: ShiftOp::SHIFTR,
                r1: Register::S,
                n: 1, // = n -1
            }),
        );
        vm.A = u32_to_word(16);
        vm.S = i32_to_word(-4);
        vm.step();
        assert_eq!(vm.A.as_u32(), 64);
        vm.step();
        assert_eq!(vm.S.as_i32(), -1);
    }

    #[test]
    fn immediate() {
        let mut vm = SicXeVm::empty();
        set_op(
            &mut vm,
            0,
            Op::Variable(Variable {
                opcode: VariableOp::LDA,
                address_flags: AddressFlags {
                    mode: AddressMode::Immediate,
                    relative_to: AddressRelativeTo::Direct,
                    indexed: false,
                    extended: false,
                },
                address: 50,
            }),
        );
        set_op(
            &mut vm,
            3,
            Op::Variable(Variable {
                opcode: VariableOp::LDA,
                address_flags: AddressFlags {
                    mode: AddressMode::Immediate,
                    relative_to: AddressRelativeTo::Base,
                    indexed: false,
                    extended: false,
                },
                address: 25,
            }),
        );
        set_op(
            &mut vm,
            6,
            Op::Variable(Variable {
                opcode: VariableOp::LDA,
                address_flags: AddressFlags {
                    mode: AddressMode::Immediate,
                    relative_to: AddressRelativeTo::Base,
                    indexed: false,
                    extended: false,
                },
                address: 25,
            }),
        );
        set_op(
            &mut vm,
            9,
            Op::Variable(Variable {
                opcode: VariableOp::LDA,
                address_flags: AddressFlags {
                    mode: AddressMode::Immediate,
                    relative_to: AddressRelativeTo::PC,
                    indexed: false,
                    extended: false,
                },
                address: 25,
            }),
        );
        set_op(
            &mut vm,
            12,
            Op::Variable(Variable {
                opcode: VariableOp::LDA,
                address_flags: AddressFlags {
                    mode: AddressMode::Simple,
                    relative_to: AddressRelativeTo::PC,
                    indexed: false,
                    extended: false,
                },
                address: (-6i32) as u32,
            }),
        );
        vm.step();
        assert_eq!(vm.A.as_u32(), 50);
        vm.step();
        assert_eq!(vm.A.as_u32(), 25);
        vm.B = u32_to_word(50);
        vm.step();
        assert_eq!(vm.A.as_u32(), 75);
        vm.step();
        // PC-rel is relative to PC *after* increment, p59
        assert_eq!(vm.A.as_u32(), 37);
        vm.step();
        // PC-rel is relative to PC *after* increment, p59
        assert_eq!(vm.A.as_u32(), 73753);
    }

    #[test]
    fn negatives() {
        let mut vm = setup_var_op(VariableOp::SUB, 9);
        ni_op(&mut vm, 3, VariableOp::ADD, 12);
        ni_op(&mut vm, 6, VariableOp::MUL, 15);
        set_int(&mut vm, 9, 1);
        set_int(&mut vm, 12, 3);
        set_int(&mut vm, 15, -3i32 as u32);
        vm.step();
        assert_eq!(vm.A.as_i32(), -1);
        vm.step();
        assert_eq!(vm.A.as_i32(), 2);
        vm.step();
        assert_eq!(vm.A.as_i32(), -6);
    }
}

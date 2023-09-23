use std::{
    cell::{Cell, RefCell},
    rc::Rc,
    sync::{Arc, Mutex},
};

use crate::{
    word::{u32_to_word, Word},
    WordExt,
};
use strum_macros::FromRepr;

#[derive(FromRepr, Debug, Clone, Copy, PartialEq, Eq)]
enum CommandCode {
    Halt = 0,
    Read,
    Write,
    Device1,
    Device2,
    Device3,
    Device4,
    Device5,
    Device6,
    Device7,
    Device8,
    Device9,
    Device10,
    Device11,
    Device12,
    Device13,
}

impl CommandCode {
    fn from_byte(byte: u8) -> Self {
        let byte = (byte & 0xF0) >> 4;
        Self::from_repr(byte as usize).expect("should have an option for all possible patterns")
    }
}

#[derive(Debug, Clone)]
struct Command {
    command_code: CommandCode,
    device_code: u8,
    byte_count: u16,
    memory_address: u32,
}

impl Command {
    fn from_bytes(bytes: [u8; 6]) -> Self {
        // Bits 0-3 inclusive
        let command_code = CommandCode::from_byte(bytes[0]);
        // Bits 4-7
        let device_code = bytes[0] & 0x0F;
        // Bits 8-23
        let byte_count = u16::from_be_bytes([bytes[1], bytes[2]]);
        // Bits 24-27 are unused
        // Bits 28-47
        let memory_address = u32::from_be_bytes([0, bytes[3] & 0x0F, bytes[4], bytes[5]]);

        Self {
            command_code,
            device_code,
            byte_count,
            memory_address,
        }
    }

    fn to_bytes(&self) -> [u8; 6] {
        let mut bytes = [0u8; 6];
        bytes[0] = (self.command_code as u8) << 4;
        bytes[0] |= self.device_code;

        [bytes[1], bytes[2]] = self.byte_count.to_be_bytes();

        [_, bytes[3], bytes[4], bytes[5]] = self.memory_address.to_be_bytes();

        bytes
    }
}

struct WorkArea {
    program_address: Word,
    esb_address: Word,
    request_queue_address: Word,
    status_flags: Word,
    reserved: Word,
}

impl WorkArea {
    fn from_bytes(bytes: [u8; 15]) -> Self {
        Self {
            program_address: [bytes[0], bytes[1], bytes[2]],
            esb_address: [bytes[3], bytes[4], bytes[5]],
            request_queue_address: [bytes[6], bytes[7], bytes[8]],
            status_flags: [bytes[9], bytes[10], bytes[11]],
            reserved: [bytes[12], bytes[13], bytes[14]],
        }
    }

    fn to_bytes(&self) -> [u8; 15] {
        let mut bytes = [0u8; 15];
        [bytes[0], bytes[1], bytes[2]] = self.program_address;
        [bytes[3], bytes[4], bytes[5]] = self.esb_address;
        [bytes[6], bytes[7], bytes[8]] = self.request_queue_address;
        [bytes[9], bytes[10], bytes[11]] = self.status_flags;
        [bytes[12], bytes[13], bytes[14]] = self.program_address;

        bytes
    }
}

struct EventStatusBlock {
    finished: bool,
    wait_queue_address: u32,
}

impl EventStatusBlock {
    fn from_bytes(bytes: [u8; 3]) -> Self {
        Self {
            finished: (bytes[0] & 0x80) > 0,
            wait_queue_address: [bytes[0] & 0x7F, bytes[1], bytes[2]].as_u32(),
        }
    }

    fn to_bytes(&self) -> [u8; 3] {
        let mut bytes = u32_to_word(self.wait_queue_address);
        bytes[0] |= if self.finished { 0x80 } else { 0 };

        bytes
    }
}

enum CommandResult {
    MadeProgress(Command),
    Done,
    Error,
}

trait IODevice {
    fn run_command(&mut self, command: &Command, memory: &mut [u8]) -> CommandResult;
}

#[derive(PartialEq, Eq)]
enum State {
    Running,
    Halted,
}

pub struct IOChannel {
    devices: Vec<Option<Box<dyn IODevice>>>,
    program_ctr: u32,
    work_area_ptr: u32,
    current_command: Option<Command>,
    state: State,
}

impl IOChannel {
    pub fn new(id: u8) -> Self {
        Self {
            devices: vec![
                None, None, None, None, None, None, None, None, None, None, None, None, None, None,
                None, None,
            ],
            program_ctr: 0,
            work_area_ptr: 200 + (id as u32 * 10),
            current_command: None,
            state: State::Halted,
        }
    }

    // This function returns true if an interrupt should be generated and
    // false otherwise.
    pub fn step(&mut self, memory: &mut [u8]) -> bool {
        if self.state == State::Halted {
            return false;
        }
        let command = self.get_command(memory);
        let Some(ref mut device) = self.devices[command.device_code as usize] else {
            return true;
        };
        match device.run_command(&command, memory) {
            CommandResult::MadeProgress(cmd) => {
                self.current_command = Some(cmd);
                println!("Made progress. New command: {:?}", self.current_command);
                false
            }
            CommandResult::Done => {
                self.current_command = None;
                self.program_ctr += 6;
                if command.command_code == CommandCode::Halt {
                    // TODO: Update status flags
                    true
                } else {
                    false
                }
            }
            CommandResult::Error => {
                // TODO: Update status flags
                self.current_command = None;
                true
            }
        }
    }

    pub fn start(&mut self) {
        self.state = State::Running;
        self.current_command = None;
        self.program_ctr = 0;
    }

    pub fn halt(&mut self) {
        self.state = State::Halted;
    }

    fn get_command(&self, memory: &[u8]) -> Command {
        if let Some(ref command) = self.current_command {
            command.clone()
        } else {
            let ptr = self.work_area_ptr as usize;
            let work_area = WorkArea::from_bytes(memory[ptr..ptr + 15].try_into().unwrap());
            let program_ptr = (work_area.program_address.as_u32() + self.program_ctr) as usize;
            Command::from_bytes(memory[program_ptr..program_ptr + 6].try_into().unwrap())
        }
    }
}

struct RingBufferIODevice {
    buffer: Vec<u8>,
    location_ptr: usize,
}

impl RingBufferIODevice {
    fn new(buffer: Vec<u8>) -> Self {
        Self {
            buffer,
            location_ptr: 0,
        }
    }

    fn advance_ptr(&mut self) {
        self.location_ptr += 1;
        self.location_ptr %= self.buffer.len();
    }

    fn next_command(command: &Command) -> Command {
        let mut new_command = command.clone();
        new_command.byte_count -= 1;
        new_command.memory_address += 1;

        new_command
    }
}

impl<T: IODevice> IODevice for Arc<Mutex<T>> {
    fn run_command(&mut self, command: &Command, memory: &mut [u8]) -> CommandResult {
        let mut device = self.lock().expect("mutex");
        device.run_command(command, memory)
    }
}

impl IODevice for RingBufferIODevice {
    fn run_command(&mut self, command: &Command, memory: &mut [u8]) -> CommandResult {
        match command.command_code {
            CommandCode::Halt => CommandResult::Done,
            CommandCode::Read => {
                let byte = self.buffer[self.location_ptr];
                let memory_loc = command.memory_address;
                memory[memory_loc as usize] = byte;

                self.advance_ptr();

                if command.byte_count == 1 {
                    return CommandResult::Done;
                }

                CommandResult::MadeProgress(RingBufferIODevice::next_command(command))
            }
            CommandCode::Write => {
                let byte = memory[command.memory_address as usize];
                self.buffer[self.location_ptr] = byte;

                self.advance_ptr();

                if command.byte_count == 1 {
                    return CommandResult::Done;
                }

                CommandResult::MadeProgress(RingBufferIODevice::next_command(command))
            }
            _ => CommandResult::Error,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_code() {
        for val in 0u8..u8::MAX {
            let _ = CommandCode::from_byte(val);
        }
    }

    #[test]
    fn test_io_channel() {
        let mut memory = [0u8; 1024];

        let read_cmd = Command {
            command_code: CommandCode::Read,
            device_code: 0,
            byte_count: 4,
            memory_address: 0x100,
        };

        let read_cmd = read_cmd.to_bytes();

        memory[0..6].copy_from_slice(&read_cmd);

        let write_cmd = Command {
            command_code: CommandCode::Write,
            device_code: 1,
            byte_count: 4,
            memory_address: 0x100,
        };

        let write_cmd = write_cmd.to_bytes();

        memory[6..12].copy_from_slice(&write_cmd);

        let halt_cmd = Command {
            command_code: CommandCode::Halt,
            device_code: 1,
            byte_count: 0,
            memory_address: 0,
        };

        let halt_cmd = halt_cmd.to_bytes();

        memory[12..18].copy_from_slice(&halt_cmd);

        let work_area = WorkArea {
            program_address: u32_to_word(0),
            esb_address: u32_to_word(0x200),
            request_queue_address: u32_to_word(0x250),
            status_flags: u32_to_word(0),
            reserved: u32_to_word(0),
        };

        let work_area = work_area.to_bytes();

        memory[200..215].copy_from_slice(&work_area);

        let mut device0 = RingBufferIODevice::new(vec![0; 32]);
        let buffer = &mut device0.buffer;
        buffer[0] = 'P' as u8;
        buffer[1] = 'a' as u8;
        buffer[2] = 'u' as u8;
        buffer[3] = 'l' as u8;
        let device1 = RingBufferIODevice::new(vec![0; 32]);
        let device1 = Arc::new(Mutex::new(device1));

        let mut channel = IOChannel::new(0);
        channel.devices[0] = Some(Box::new(device0));
        channel.devices[1] = Some(Box::new(device1.clone()));

        channel.start();

        loop {
            if channel.step(&mut memory) {
                break;
            }
        }

        let device1 = device1.lock().unwrap();

        let buffer = &device1.buffer;
        let output = String::from_utf8_lossy(&buffer[0..4]);

        assert_eq!(output, "Paul".to_string());
    }
}

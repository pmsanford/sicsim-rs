use std::{
    cell::RefCell,
    fs::File,
    io::{Read, Seek, SeekFrom, Write},
    path::Path,
    rc::Rc,
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
pub struct Command {
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

    #[allow(dead_code)]
    fn to_bytes(&self) -> [u8; 6] {
        let mut bytes = [0u8; 6];
        bytes[0] = (self.command_code as u8) << 4;
        bytes[0] |= self.device_code;

        [bytes[1], bytes[2]] = self.byte_count.to_be_bytes();

        [_, bytes[3], bytes[4], bytes[5]] = self.memory_address.to_be_bytes();

        bytes
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IOChannelStatus {
    Success = 0x00,
    DeviceUnavailable = 0x01,
    DeviceFailure = 0x20,
    EndOfInput = 0x21,
}

impl IOChannelStatus {
    fn as_status_word(&self) -> Word {
        u32_to_word(*self as u32 | 0x800000u32)
    }

    fn with_device_error(error_code: u8) -> Word {
        if error_code >= Self::DeviceFailure as u8 {
            u32_to_word(error_code as u32 | 0x800000u32)
        } else {
            Self::DeviceFailure.as_status_word()
        }
    }
}

pub struct WorkArea {
    program_address: Word,
    esb_address: Word,
    request_queue_address: Word,
    // TODO: Figure out if there's a spec for this
    // For now, bits are:
    // 0: 0 -> in progress, 1 -> done
    // 1-7: Error code
    //  0 -> Success
    //  1 -> Device unavailable
    //  2-31 -> reserved for io channel controller errors
    //  32 -> unclassified device failure
    //  33 -> end of input
    //  32-63 -> reserved for common failure codes
    //  64-127 -> device specific
    // 8-23: Unused
    status_flags: Word,
    #[allow(dead_code)]
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

    #[allow(dead_code)]
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

#[allow(dead_code)]
struct EventStatusBlock {
    finished: bool,
    wait_queue_address: u32,
}

#[allow(dead_code)]
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

pub enum CommandResult {
    MadeProgress(Command),
    Done,
    Error(u8),
}

pub trait IODevice {
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
    work_area_ptr: usize,
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
            work_area_ptr: 200 + (id as usize * 10),
            current_command: None,
            state: State::Halted,
        }
    }

    pub fn add_device(&mut self, id: u8, device: Box<dyn IODevice>) {
        self.devices[id as usize] = Some(device);
    }

    // This function returns true if an interrupt should be generated and
    // false otherwise.
    pub fn step(&mut self, memory: &mut [u8]) -> bool {
        if self.state == State::Halted {
            return false;
        }
        let command = self.get_command(memory);
        let Some(ref mut device) = self.devices[command.device_code as usize] else {
            self.state = State::Halted;
            return true;
        };
        match device.run_command(&command, memory) {
            CommandResult::MadeProgress(cmd) => {
                self.current_command = Some(cmd);
                false
            }
            CommandResult::Done => {
                self.current_command = None;
                if command.command_code == CommandCode::Halt {
                    self.update_work_area(memory, |work_area| {
                        work_area.status_flags = IOChannelStatus::Success.as_status_word();
                    });
                    self.state = State::Halted;
                    true
                } else {
                    self.program_ctr += 6;
                    false
                }
            }
            CommandResult::Error(device_error) => {
                self.update_work_area(memory, |work_area| {
                    if device_error < 0x20 {
                        work_area.status_flags = IOChannelStatus::DeviceFailure.as_status_word();
                    } else {
                        work_area.status_flags = IOChannelStatus::with_device_error(device_error);
                    }
                });
                self.current_command = None;
                self.state = State::Halted;
                true
            }
        }
    }

    pub fn start(&mut self, start_address: Word, memory: &mut [u8]) {
        self.state = State::Running;
        self.update_work_area(memory, |work_area| {
            work_area.program_address = start_address
        });
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
            let ptr = self.work_area_ptr;
            let work_area = WorkArea::from_bytes(memory[ptr..ptr + 15].try_into().unwrap());
            let program_ptr = (work_area.program_address.as_u32() + self.program_ctr) as usize;
            Command::from_bytes(memory[program_ptr..program_ptr + 6].try_into().unwrap())
        }
    }

    fn update_work_area(&self, memory: &mut [u8], updater: impl FnOnce(&mut WorkArea)) {
        let work_area_slice = &memory[self.work_area_ptr..self.work_area_ptr + 15];
        let mut work_area = WorkArea::from_bytes(work_area_slice.try_into().unwrap());
        updater(&mut work_area);
        memory[self.work_area_ptr..self.work_area_ptr + 15].copy_from_slice(&work_area.to_bytes());
    }
}

impl<T: IODevice> IODevice for Rc<RefCell<T>> {
    fn run_command(&mut self, command: &Command, memory: &mut [u8]) -> CommandResult {
        let mut device = self.borrow_mut();
        device.run_command(command, memory)
    }
}

pub struct FileIODevice {
    file: File,
}

impl FileIODevice {
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        let file = File::options()
            .read(true)
            .write(true)
            .open(path)
            .expect("Failed to open file");
        Self { file }
    }
}

impl IODevice for FileIODevice {
    fn run_command(&mut self, command: &Command, memory: &mut [u8]) -> CommandResult {
        match command.command_code {
            CommandCode::Halt => CommandResult::Done,
            CommandCode::Read => {
                let mut byte = [0u8];
                match self.file.read_exact(&mut byte) {
                    Ok(_) => {
                        let memory_loc = command.memory_address;
                        memory[memory_loc as usize] = byte[0];

                        if command.byte_count == 1 {
                            return CommandResult::Done;
                        }

                        CommandResult::MadeProgress(advance_rw_command(command))
                    }
                    Err(e) => match e.kind() {
                        std::io::ErrorKind::UnexpectedEof => CommandResult::Error(0x21),
                        _ => CommandResult::Error(0x20),
                    },
                }
            }
            CommandCode::Write => {
                let byte = [memory[command.memory_address as usize]];

                match self.file.write_all(&byte) {
                    Ok(_) => {
                        if command.byte_count == 1 {
                            return CommandResult::Done;
                        }

                        CommandResult::MadeProgress(advance_rw_command(command))
                    }
                    Err(_) => CommandResult::Error(0x20),
                }
            }
            // Read position
            CommandCode::Device1 => {
                let pos = self.file.stream_position();
                let Ok(pos) = pos else {
                    return CommandResult::Error(0x20);
                };

                let max_word = 0xFFFFFFu64;

                if pos > max_word {
                    return CommandResult::Error(0x20);
                }

                let pos = pos as u32;
                let addr = command.memory_address as usize;

                memory[addr..addr + 3].copy_from_slice(&u32_to_word(pos));

                CommandResult::Done
            }
            // Seek
            CommandCode::Device2 => {
                let seek_amount = command.byte_count as i16;
                match self.file.seek(SeekFrom::Current(seek_amount as i64)) {
                    Ok(_) => CommandResult::Done,
                    Err(_) => CommandResult::Error(0x20),
                }
            }
            // Read length
            CommandCode::Device3 => {
                let Ok(metadata) = self.file.metadata() else {
                    return CommandResult::Error(0x20);
                };
                let len = metadata.len();

                let max_word = 0xFFFFFFu64;

                if len > max_word {
                    return CommandResult::Error(0x20);
                }

                let len = len as u32;

                let addr = command.memory_address as usize;

                memory[addr..addr + 3].copy_from_slice(&u32_to_word(len));

                CommandResult::Done
            }
            _ => CommandResult::Error(0x20),
        }
    }
}

fn advance_rw_command(command: &Command) -> Command {
    let mut new_command = command.clone();
    new_command.byte_count -= 1;
    new_command.memory_address += 1;

    new_command
}

#[cfg(test)]
mod tests {
    use super::*;

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
                _ => CommandResult::Error(0x20),
            }
        }
    }

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
        buffer[0] = b'P';
        buffer[1] = b'a';
        buffer[2] = b'u';
        buffer[3] = b'l';
        let device1 = RingBufferIODevice::new(vec![0; 32]);
        let device1 = Rc::new(RefCell::new(device1));

        let mut channel = IOChannel::new(0);
        channel.devices[0] = Some(Box::new(device0));
        channel.devices[1] = Some(Box::new(device1.clone()));

        channel.start(u32_to_word(0), &mut memory);

        loop {
            if channel.step(&mut memory) {
                break;
            }
        }

        let device1 = device1.borrow();

        let buffer = &device1.buffer;
        let output = String::from_utf8_lossy(&buffer[0..4]);

        assert_eq!(output, "Paul".to_string());
    }
}

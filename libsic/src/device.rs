use std::{
    cell::RefCell,
    fs::File,
    io::{Read, Write},
    rc::Rc,
};

use thiserror::Error;

pub trait Device {
    fn test(&mut self) -> bool;
    fn read(&mut self) -> u8;
    fn write(&mut self, data: u8);
}

#[derive(Error, Debug)]
pub enum FileDeviceError {
    #[error("Error opening file")]
    OpenError,
}

pub struct FileInputDevice {
    file: File,
}

impl FileInputDevice {
    pub fn new(path: &str) -> Result<Self, FileDeviceError> {
        Ok(Self {
            file: File::open(path).map_err(|_| FileDeviceError::OpenError)?,
        })
    }
}

impl Device for FileInputDevice {
    fn test(&mut self) -> bool {
        true
    }

    fn read(&mut self) -> u8 {
        let mut outbuf = [0];
        if let Err(e) = self.file.read_exact(&mut outbuf) {
            match e.kind() {
                std::io::ErrorKind::UnexpectedEof => return 0,
                _ => return 0, // TODO: How to handle read errors?
            }
        };
        outbuf[0]
    }

    fn write(&mut self, _data: u8) {
        // Do nothing - this device doesn't support writes.
    }
}

pub struct FileOutputDevice {
    file: File,
}

impl FileOutputDevice {
    pub fn new(path: &str) -> Result<Self, FileDeviceError> {
        Ok(Self {
            file: File::open(path).map_err(|_| FileDeviceError::OpenError)?,
        })
    }
}

impl Device for FileOutputDevice {
    fn test(&mut self) -> bool {
        true
    }

    fn read(&mut self) -> u8 {
        // Return end of file - this device doesn't support reads.
        0
    }

    fn write(&mut self, data: u8) {
        // TODO: How to handle write errors?
        let _ = self.file.write(&[data]);
    }
}

pub struct MemoryOutputDevice {
    buffer: Rc<RefCell<Vec<u8>>>,
}

impl MemoryOutputDevice {
    pub fn new() -> (Rc<RefCell<Vec<u8>>>, Self) {
        let buffer = Rc::new(RefCell::new(Vec::new()));
        let device = Self {
            buffer: Rc::clone(&buffer),
        };
        (buffer, device)
    }
}

impl Device for MemoryOutputDevice {
    fn test(&mut self) -> bool {
        true
    }

    fn read(&mut self) -> u8 {
        // Return end of file - this device doesn't support reading
        0
    }

    fn write(&mut self, data: u8) {
        self.buffer.borrow_mut().push(data);
    }
}

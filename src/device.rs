use std::{
    cell::RefCell,
    fs::File,
    io::{Read, Write},
    sync::Arc,
};

pub trait Device {
    fn test(&mut self) -> bool;
    fn read(&mut self) -> u8;
    fn write(&mut self, data: u8);
}

pub struct FileInputDevice {
    file: File,
}

impl FileInputDevice {
    pub fn new(path: &str) -> Self {
        Self {
            file: File::open(path).unwrap(),
        }
    }
}

impl Device for FileInputDevice {
    fn test(&mut self) -> bool {
        true
    }

    fn read(&mut self) -> u8 {
        let mut outbuf = [0];
        match self.file.read_exact(&mut outbuf) {
            Err(e) => match e.kind() {
                std::io::ErrorKind::UnexpectedEof => return 0,
                e => Err(e).unwrap(),
            },
            _ => (),
        };
        outbuf[0]
    }

    fn write(&mut self, _data: u8) {
        unimplemented!()
    }
}

pub struct FileOutputDevice {
    file: File,
}

impl FileOutputDevice {
    pub fn new(path: &str) -> Self {
        Self {
            file: File::open(path).unwrap(),
        }
    }
}

impl Device for FileOutputDevice {
    fn test(&mut self) -> bool {
        true
    }

    fn read(&mut self) -> u8 {
        unimplemented!()
    }

    fn write(&mut self, data: u8) {
        self.file.write(&[data]).unwrap();
    }
}

pub struct MemoryOutputDevice {
    buffer: Arc<RefCell<Vec<u8>>>,
}

impl MemoryOutputDevice {
    pub fn new() -> (Arc<RefCell<Vec<u8>>>, Self) {
        let buffer = Arc::new(RefCell::new(Vec::new()));
        let device = Self {
            buffer: Arc::clone(&buffer),
        };
        (buffer, device)
    }
}

impl Device for MemoryOutputDevice {
    fn test(&mut self) -> bool {
        true
    }

    fn read(&mut self) -> u8 {
        unimplemented!()
    }

    fn write(&mut self, data: u8) {
        self.buffer.borrow_mut().push(data);
    }
}

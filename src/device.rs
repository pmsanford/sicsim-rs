pub trait Device {
    fn test(&mut self) -> bool;
    fn read(&mut self) -> u8;
    fn write(&mut self, data: u8);
}

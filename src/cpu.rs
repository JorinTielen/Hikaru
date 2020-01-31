use crate::mmc::MMC;

pub struct CPU {
    mmc: MMC,
    pc: u16,
}

impl CPU {
    pub fn new(rom: Vec<u8>) -> Self {
        CPU {
            mmc: MMC::new(rom),

            pc: 0xC000,
        }
    }

    pub fn cycle(&self) {
        let op = self.read(self.pc);
        println!("Opcode: {:#X}", op);
    }

    fn read(&self, addr: u16) -> u8 {
        self.mmc.rb(addr)
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.mmc.wb(addr, data)
    }
}
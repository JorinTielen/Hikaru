//! Managing memory with the MMC
//!
//! This module contains the Memory Management Controller which is responsible
//! for translating memory addresses to different devices.

pub struct MMC {
    ram: [u8; 0xFFFF],
    rom: Vec<u8>,
}

impl MMC {
    pub fn new(rom: Vec<u8>) -> Self {
        MMC {
            ram: [0; 0xFFFF],
            rom,
        }
    }

    pub fn rb(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram[(addr as usize)],
            0x8000..=0xBFFF => self.rom[(addr as usize) - 0x8000],
            0xC000..=0xFFFF => self.rom[(addr as usize) - 0xC000],
            _ => {
                println!("Read unrecognized address {:#X}", addr);
                0
            }
        }
    }

    pub fn wb(&mut self, addr: u16, data: u8) {
        self.ram[(addr as usize)] = data
    }
}

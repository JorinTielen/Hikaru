//! Accessing memory through the Bus.
//!
//! This module contains the Bus which is responsible for
//! translating memory addresses to different devices.

use crate::cartridge::Cartridge;
use crate::mappers::get_mapper;
use crate::mappers::Mapper;

pub struct Bus {
    ram: [u8; 0xFFFF],
    mapper: Box<dyn Mapper>
}

impl Bus {
    pub fn new(cart: &mut Cartridge) -> Self {
        let mapper = Box::new(get_mapper(cart));
        Bus {
            ram: [0; 0xFFFF],
            mapper: mapper
        }
    }

    pub fn rb(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram[(addr as usize)],
            0x4020..=0xFFFF => self.mapper.rb(addr),
            _ => {
                println!("Bus: Read unrecognized address {:#X}", addr);
                0
            }
        }
    }

    pub fn wb(&mut self, addr: u16, data: u8) {
        self.ram[(addr as usize)] = data
    }

    pub fn rw(&self, addr: u16) -> u16 {
        ((self.rb(addr + 1) as u16) << 8) | (self.rb(addr) as u16)
    }
}


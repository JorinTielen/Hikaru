//! Accessing data on a Cartridge through Mappers.
//!
//! This module contains various mappers and their
//! implementation. A mapper is responsible for
//! managing the different banks of memory there
//! could be on a cartridge, and allowing the CPU
//! to access them from the Bus' address space.

mod nrom;

use crate::cartridge::Cartridge;

pub trait Mapper {
    fn rb(&self, addr: u16) -> u8;
}

pub fn get_mapper(cart: &mut Cartridge) -> impl Mapper {
    match cart.flags.mapper {
        0 => nrom::Nrom::new(cart),
        _ => panic!("Unsupported mapper type!")
    }
}


//! Representing data on the cartridge.
//!
//! This module contains the Cartridge which provides the raw data on
//! the NES cartridge, after it is read from an iNES file. To access this
//! data a mapper is required. There are many different mappers with different
//! behaviour, but they all read from this same cartridge.

#[derive(Debug)]
pub struct Flags {
    pub prg_size: usize,
    pub chr_size: usize,
    pub mapper: u8,
    pub prg_ram_size: u8
}

pub struct Cartridge {
    pub flags: Flags,
    pub pgr_rom: Vec<u8>,
    pub chr_rom: Vec<u8>
}

impl Cartridge {
   pub fn new(flags: Flags, pgr: Vec<u8>, chr: Vec<u8>) -> Self {
       Cartridge {
           flags: flags,
           pgr_rom: pgr,
           chr_rom: chr
       }
   }
}


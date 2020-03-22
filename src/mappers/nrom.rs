//! The NROM mapper is mapper 000 in the iNES format.
//!
//! The generic designation NROM refers to the Nintendo
//! cartridge boards NES-NROM-128, NES-NROM-256, their
//! HVC counterparts, and clone boards.
//!
//! Overview:
//!   PGR ROM size: 16 KiB (NROM-128) or 32 KiB (NROM-256)
//!   PGR RAM size: 2 KiB or 4 KiB
//!   CHR capacity: 8 KiB
//!
//! Banks:
//!   CPU $6000-$7FFF: PGR RAM, mirrored to fill 8 KiB window
//!   CPU $8000-$BFFF: First 16 KB of PRG ROM
//!   CPU $C000-$FFFF: Last 16 KiB of PRG ROM (NROM-256)
//!                    or mirror of $8000-$BFFF (NROM-128)
//!
//! Registers:
//!   None.

use crate::cartridge::Cartridge;
use crate::mappers::Mapper;

enum NromType {
    Nrom256,
    Nrom128
}

pub struct Nrom {
    nrom_type: NromType,
    pgr_rom: Vec<u8>,
    chr_rom: Vec<u8>,

    pgr_ram: Vec<u8>
}

impl Nrom {
    pub fn new(cart: &mut Cartridge) -> Self {
        let nrom_type = match cart.flags.prg_size as usize {
            32768 => NromType::Nrom256,
            16384 => NromType::Nrom128,
            _ => panic!("Nrom cartridge has incorrect prg_size!")
        };

        Nrom {
            nrom_type: nrom_type,
            pgr_rom: cart.pgr_rom.clone(), // TODO: Prevent copying roms
            chr_rom: cart.chr_rom.clone(),
            pgr_ram: Vec::new()
        }
    }
}

impl Mapper for Nrom {
    fn rb(&self, addr: u16) -> u8 {
        match addr {
            0x6000..=0x7FFF => self.pgr_ram[(addr as usize) - 0x6000],
            0x8000..=0xBFFF => self.pgr_rom[(addr as usize) - 0x8000],
            0xC000..=0xFFFF => {
                match self.nrom_type {
                    NromType::Nrom256 => self.pgr_rom[(addr as usize) - 0x8000],
                    NromType::Nrom128 => self.pgr_rom[(addr as usize) - 0xC000]
                }
            },
            _ => {
                println!("Mapper: Read unrecognized address {:#X}", addr);
                0
            }
        }
    }
}


use crate::cartridge::Cartridge;
use crate::cartridge::Flags;

use std::fs::File;
use std::io::Read;
use std::io::Result;

pub fn load_file(file_path: &str) -> Result<Cartridge> {
    let rom = File::open(file_path)?;

    let mut data: Vec<u8> = Vec::new();

    for (_, byte) in rom.bytes().enumerate() {
        let byte = match byte {
            Ok(byte) => byte,
            Err(e) => return Err(e)
        };
        data.push(byte);
    }

    let flags = Flags {
        prg_size: data[4] as usize * 16384,
        chr_size: data[5] as usize * 8192,
        mapper: ((data[7] & 0b11110000) << 4) | (data[6] >> 4),
        prg_ram_size: data[8]
    };

    let prg = &data[16..(16 + flags.prg_size)];
    let chr = &data[(16 + flags.prg_size)..(16 + flags.prg_size + flags.chr_size)];

    Ok(Cartridge::new(flags, prg.to_vec(), chr.to_vec()))
}


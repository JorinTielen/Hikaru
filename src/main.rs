use std::env;
use std::fs::File;
use std::io::Read;

mod cpu;
mod mmc;

use cpu::CPU;

fn main() {
    let rom_path = match env::args().nth(1) {
        Some(path) => path,
        None => {
            println!("Please provide a rom file. Exiting program...");
            std::process::exit(1);
        }
    };

    let rom = match File::open(&rom_path) {
        Ok(file) => file,
        Err(_) => {
            println!("Error opening file at {}. Exiting program...", rom_path);
            std::process::exit(1);
        }
    };

    let mut data: Vec<u8> = Vec::new();

    for (_, byte) in rom.bytes().enumerate() {
        let byte = match byte {
            Ok(byte) => byte,
            Err(msg) => {
                println!("Error loading rom: {}", msg);
                std::process::exit(1);
            }
        };
        data.push(byte);
    }

    let cpu = CPU::new(data);
    cpu.cycle();

}

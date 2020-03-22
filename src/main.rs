use std::env;

mod bus;
mod cpu;
mod cartridge;
mod ines;
mod mappers;

use cpu::CPU;
use cartridge::Cartridge;

fn main() {
    let rom_path = match env::args().nth(1) {
        Some(path) => path,
        None => {
            println!("Please provide a rom file. Exiting program...");
            std::process::exit(1);
        }
    };

    match ines::load_file(&rom_path) {
        Ok(mut cart) => emulate(&mut cart),
        Err(_) => {
            println!("Failed reading rom file. Exiting program...");
            std::process::exit(1);
        }
    }
}

fn emulate(cart: &mut Cartridge) {
    println!("Loaded rom with {:?}", cart.flags);

    let cpu = CPU::new(cart);
    cpu.cycle();
}


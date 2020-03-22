use crate::bus::Bus;
use crate::cartridge::Cartridge;

use phf::phf_map;

pub enum Flags {
    C = (1 << 0),  // Carry bit
    Z = (1 << 1),  // Zero
    I = (1 << 2),  // Interrupt
    D = (1 << 3),  // Decimal Mode
    B = (1 << 4),  // Break
    U = (1 << 5),  // Unused
    V = (1 << 6),  // Overflow
    N = (1 << 7)   // Negative
}

// Address Modes
enum AddressModeResult {
    Val(u8),
    Addr(u16),
    Accumulator,
    X,
    Y
}

type AddressMode = fn(&mut CPU, bool) -> AddressModeResult;

fn immediate(cpu: &mut CPU, _: bool) -> AddressModeResult {
     cpu.pc += 1;
     cpu.count += 2;
     AddressModeResult::Val(cpu.read(cpu.pc - 1))
}

// Operations
type Operation = fn(&mut CPU) -> u8;

static OPCODES: phf::Map<u8, Operation> = phf_map! {};

pub struct CPU {
    bus: Bus,
    count: u32,

    pc: u16,
    sp: u8,

    a: u8,
    x: u8,
    y: u8,
    s: u8
}

impl CPU {
    pub fn new(cart: &mut Cartridge) -> Self {
        let bus = Bus::new(cart);
        let pc = bus.rw(0xFFFC);
        CPU {
            bus: bus,
            count: 0,

            pc: pc,
            sp: 0x00,

            a: 0x00,
            x: 0x00,
            y: 0x00,
            s: 0x00
        }
    }

    pub fn cycle(&self) {
        println!("Program Counter: {:#X}", self.pc);

        let op = self.read(self.pc);
        println!("Opcode: {:#X}", op);
    }

    fn read(&self, addr: u16) -> u8 {
        self.bus.rb(addr)
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.bus.wb(addr, data)
    }
}


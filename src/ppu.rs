//! Pixel processing unit
use crate::bus::Bus;

pub struct PPU<'a> {
    vram: [u8; 2 * 1024],
    pattern: [u8; 2 * 4096],
    palette: [u8; 32],

    scanline: u16,
    cycle: u16,

    cpu_bus: &'a mut Bus
}

impl<'a> PPU<'a> {
    pub fn new(cpu_bus: &'a mut Bus) -> Self {
        PPU {
            vram: [0; 2 * 1024],
            pattern: [0; 2 * 4096],
            palette: [0; 32],

            scanline: 0,
            cycle: 0,

            cpu_bus: cpu_bus
        }
    }
}


use crate::bus::Bus;
use crate::cartridge::Cartridge;
use crate::ppu::PPU;

use phf::phf_map;

// TODO:
//   - Rewrite all comments into different format
//   - Check cpu cycles for all Operations

pub struct Flags {
    c: bool,  // Carry bit
    z: bool,  // Zero
    i: bool,  // Interrupt
    d: bool,  // Decimal mode
    b: bool,  // Break
    u: bool,  // Unused
    v: bool,  // Overflow
    n: bool   // Negative
}

impl Flags {
    fn new() -> Self {
        Flags {
            c: false,
            z: false,
            i: false,
            d: false,
            b: false,
            u: false,
            v: false,
            n: false
        }
    }
}

/// The data fetched by an adressing mode
enum Fetched {
    Val(u8),
    Addr(u16),
    Accumulator(u8)
}

impl Fetched {
    fn read(&self, cpu: &CPU) -> u8 {
        match *self {
            Fetched::Val(val) => val,
            Fetched::Addr(addr) => cpu.read(addr),
            Fetched::Accumulator(a) => a
        }
    }

    fn write(&self, cpu: &mut CPU, val: u8) {
        match *self {
            Fetched::Addr(addr) => cpu.write(addr, val),
            Fetched::Accumulator(a) => cpu.a = a,
            _ => panic!("Write to read-only Fetched data!")
        }
    }
}

/// Addressing modes
///
/// Addressing modes determine what data an Operation is operating on. They
/// are implemented as a function on the CPU that fetches some data which
/// the Operation can then use to perform its logic with.
type AddressingMode = fn(&mut CPU) -> Fetched;

fn implied(cpu: &mut CPU) -> Fetched {
    cpu.cycles += 2;

    Fetched::Accumulator(cpu.a)
}

fn immediate(cpu: &mut CPU) -> Fetched {
    cpu.pc += 1;
    cpu.cycles += 2;

    Fetched::Val(cpu.read(cpu.pc - 1))
}

fn absolute(cpu: &mut CPU) -> Fetched {
    let lo = cpu.read(cpu.pc) as u16;
    let hi = cpu.read(cpu.pc + 1) as u16;
    cpu.pc += 2;
    cpu.cycles += 4;

    Fetched::Addr((hi << 8) | lo)
}

fn absolute_x(cpu: &mut CPU) -> Fetched {
    let lo = cpu.read(cpu.pc) as u16;
    let hi = cpu.read(cpu.pc + 1) as u16;
    cpu.pc += 2;
    cpu.cycles += 4;

    let addr = ((hi << 8) | lo) + cpu.x as u16;
    if addr & 0xFF00 != (hi << 8) {
        cpu.cycles += 1;
    }
    Fetched::Addr(addr)
}


fn absolute_y(cpu: &mut CPU) -> Fetched {
    let lo = cpu.read(cpu.pc) as u16;
    let hi = cpu.read(cpu.pc + 1) as u16;
    cpu.pc += 2;
    cpu.cycles += 4;

    let addr = ((hi << 8) | lo).wrapping_add(cpu.y as u16);
    if addr & 0xFF00 != (hi << 8) {
        cpu.cycles += 1;
    }
    Fetched::Addr(addr)
}

fn relative(cpu: &mut CPU) -> Fetched {
    let mut addr_rel = cpu.read(cpu.pc) as u16;
    cpu.pc += 1;
    if addr_rel & 0x80 != 0 {
        addr_rel |= 0xFF00;
    }

    Fetched::Addr(addr_rel)
}

fn zero_page(cpu: &mut CPU) -> Fetched {
    let addr = cpu.read(cpu.pc);
    cpu.pc += 1;
    cpu.cycles += 3;

    Fetched::Addr(addr as u16)
}

fn zero_page_x(cpu: &mut CPU) -> Fetched {
    let addr = cpu.read(cpu.pc);
    cpu.pc += 1;
    cpu.cycles += 3;

    Fetched::Addr((addr as u16 + cpu.x as u16) & 0xFF)
}

fn zero_page_y(cpu: &mut CPU) -> Fetched {
    let addr = cpu.read(cpu.pc);
    cpu.pc += 1;
    cpu.cycles += 3;

    Fetched::Addr((addr as u16 + cpu.y as u16) & 0xFF)
}

fn indirect(cpu: &mut CPU) -> Fetched {
    let lo = cpu.read(cpu.pc) as u16;
    let hi = cpu.read(cpu.pc + 1) as u16;
    cpu.pc += 2;
    cpu.cycles += 6;

    let mut addr = (hi << 8) | lo;
    if lo == 0xFF {
        addr = ((cpu.read(addr & 0xFF00) as u16) << 8) | cpu.read(addr) as u16
    } else {
        addr = ((cpu.read(addr + 1) as u16) << 8) | cpu.read(addr) as u16
    }
    Fetched::Addr(addr)
}

fn indirect_x(cpu: &mut CPU) -> Fetched {
    let arg = cpu.read(cpu.pc) as u16;
    cpu.pc += 1;
    cpu.cycles += 6;

    let lo = cpu.read(arg.wrapping_add(cpu.x as u16) & 0xFF) as u16;
    let hi = cpu.read(arg.wrapping_add(cpu.x as u16 + 1) & 0xFF) as u16;
    let addr = (hi << 8) | lo;
    Fetched::Addr(addr)
}

fn indirect_y(cpu: &mut CPU) -> Fetched {
    let arg = cpu.read(cpu.pc) as u16;
    cpu.pc += 1;
    cpu.cycles += 6;

    let lo = cpu.read(arg.wrapping_add(cpu.y as u16) & 0xFF) as u16;
    let hi = cpu.read(arg.wrapping_add(cpu.y as u16 + 1) & 0xFF) as u16;
    let addr = (hi << 8) | lo;
    if (addr & 0xFF00) != (hi << 8) {
        cpu.cycles += 1;
    }
    Fetched::Addr(addr)
}

/// Operations
///
/// An Operation is the implementation of the 6502 CPU instruction related to
/// the opcode. An operation performs its logic on the CPU and can use
/// addressing modes to be able to work on different types of data.
type Operation = fn(&mut CPU, AddressingMode) -> ();


/// BRK  Force Break
///
/// interrupt,                       N Z C I D V
/// push PC+2, push SR               - - - 1 - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       BRK           00    1     7
fn brk(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.pc += 1;
    cpu.flags.i = true;

    cpu.write(0x0100 + cpu.sp as u16, ((cpu.pc >> 8) & 0x00FF) as u8);
    cpu.sp -= 1;
    cpu.write(0x0100 + cpu.sp as u16, (cpu.pc & 0x00FF) as u8);
    cpu.sp -= 1;

    cpu.flags.b = true;
    cpu.write(0x0100 + cpu.sp as u16, cpu.get_flags());
    cpu.sp -= 1;
    cpu.flags.b = false;

    // TODO: Cycles
    cpu.pc = cpu.read(0xFFFE) as u16 | (cpu.read(0xFFFF) as u16) << 8;
}

/// PHP  Push Processor Status on Stack
///
/// push SR                          N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       PHP           08    1     3
fn php(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);

    cpu.write(0x100 + cpu.sp as u16, cpu.get_flags());
    cpu.flags.b = false;
    cpu.flags.u = false;
    cpu.sp -= 1;
    cpu.cycles += 1;
}

/// ORA  OR Memory with Accumulator
///
/// A OR M -> A                      N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     ORA #oper     09    2     2
/// zeropage      ORA oper      05    2     3
/// zeropage,X    ORA oper,X    15    2     4
/// absolute      ORA oper      0D    3     4
/// absolute,X    ORA oper,X    1D    3     4*
/// absolute,Y    ORA oper,Y    19    3     4*
/// (indirect,X)  ORA (oper,X)  01    2     6
/// (indirect),Y  ORA (oper),Y  11    2     5*
fn ora(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);

    cpu.a = cpu.a | val;
    cpu.flags.z = cpu.a == 0;
    cpu.flags.n = cpu.a & 0x80 == 0;
}

/// ASL  Shift Left One Bit (Memory or Accumulator)
///
/// C <- [76543210] <- 0             N Z C I D V
///                                  + + + - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// accumulator   ASL A         0A    1     2
/// zeropage      ASL oper      06    2     5
/// zeropage,X    ASL oper,X    16    2     6
/// absolute      ASL oper      0E    3     6
/// absolute,X    ASL oper,X    1E    3     7
fn asl(cpu: &mut CPU, mode: AddressingMode) {
    let fetched = mode(cpu);
    let val = fetched.read(cpu);
    let res = (val << 1) as u16;
    fetched.write(cpu, res as u8);

    cpu.flags.c = (res & 0xFF00) > 0;
    cpu.flags.z = (res & 0x00FF) == 0x00;
    cpu.flags.n = (res & 0x80) == 0;
    cpu.cycles += 2;
}

/// BPL  Branch on Result Plus
///
/// branch on N = 0                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BPL oper      10    2     2**
fn bpl(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BPL requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.n == false {
        branch(cpu, addr_rel);
    }
}

/// CLC  Clear Carry Flag
///
/// 0 -> C                           N Z C I D V
///                                  - - 0 - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       CLC           18    1     2
fn clc(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.flags.c = false;
}

/// JSR  Jump to New Location Saving Return Address
///
/// push (PC+2),                     N Z C I D V
/// (PC+1) -> PCL                    - - - - - -
/// (PC+2) -> PCH
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// absolute      JSR oper      20    3     6
fn jsr(cpu: &mut CPU, mode: AddressingMode) {
    let addr = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("JSR requires Fetched::Addr from AddressingMode!")
    };
    cpu.pc -= 1;

    cpu.write(0x0100 + cpu.sp as u16, ((cpu.pc >> 8) & 0x00FF) as u8);
    cpu.sp -= 1;
    cpu.write(0x0100 + cpu.sp as u16, (cpu.pc & 0x00FF) as u8);
    cpu.sp -= 1;

    cpu.pc = addr;
    cpu.cycles += 2;
}

/// BIT  Test Bits in Memory with Accumulator
///
/// bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
/// the zeroflag is set to the result of operand AND accumulator.
///
/// A AND M, M7 -> N, M6 -> V        N Z C I D V
///                                 M7 + - - - M6
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// zeropage      BIT oper      24    2     3
/// absolute      BIT oper      2C    3     4
fn bit(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);

    cpu.flags.z = cpu.a & val == 0;
    cpu.flags.n = val & 0b10000000 > 0;
    cpu.flags.v = val & 0b01000000 > 0;
}

/// PLP  Pull Processor Status from Stack
///
/// pull SR                          N Z C I D V
///                                  from stack
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       PLP           28    1     4
fn plp(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);

    cpu.sp += 1;
    cpu.set_flags(cpu.read(0x0100 + cpu.sp as u16));
    cpu.flags.u = true;
    cpu.cycles += 2;
}

/// AND  AND Memory with Accumulator
///
/// A AND M -> A                     N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     AND #oper     29    2     2
/// zeropage      AND oper      25    2     3
/// zeropage,X    AND oper,X    35    2     4
/// absolute      AND oper      2D    3     4
/// absolute,X    AND oper,X    3D    3     4*
/// absolute,Y    AND oper,Y    39    3     4*
/// (indirect,X)  AND (oper,X)  21    2     6
/// (indirect),Y  AND (oper),Y  31    2     5*
fn and(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);

    cpu.a = cpu.a & val;
    cpu.flags.z = cpu.a == 0;
    cpu.flags.n = cpu.a & 0x80 == 0;
}

/// BMI  Branch on Result Minus
///
/// branch on N = 1                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BMI oper      30    2     2**
fn bmi(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BMI requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.n == true {
        branch(cpu, addr_rel);
    }
}

/// SEC  Set Carry Flag
///
/// 1 -> C                           N Z C I D V
///                                  - - 1 - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       SEC           38    1     2
fn sec(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.flags.c = true;
}

/// RTI  Return from Interrupt
///
/// pull SR, pull PC                 N Z C I D V
///                                  from stack
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       RTI           40    1     6
fn rti(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);

    cpu.sp += 1;
    cpu.set_flags(cpu.read(0x0100 + cpu.sp as u16));
    cpu.flags.b = false;
    cpu.flags.u = false;

    cpu.sp += 1;
    let lo = cpu.read(0x0100 + cpu.sp as u16);
    cpu.sp += 1;
    let hi = cpu.read(0x0100 + cpu.sp as u16);

    cpu.pc = lo as u16 | (hi as u16) << 8;
    cpu.cycles += 4;
}

/// PHA  Push Accumulator on Stack
///
/// push A                           N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       PHA           48    1     3
fn pha(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.write(0x0100 + cpu.sp as u16, cpu.a);
    cpu.sp -= 1;
}

/// EOR  Exclusive-OR Memory with Accumulator
///
/// A EOR M -> A                     N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     EOR #oper     49    2     2
/// zeropage      EOR oper      45    2     3
/// zeropage,X    EOR oper,X    55    2     4
/// absolute      EOR oper      4D    3     4
/// absolute,X    EOR oper,X    5D    3     4*
/// absolute,Y    EOR oper,Y    59    3     4*
/// (indirect,X)  EOR (oper,X)  41    2     6
/// (indirect),Y  EOR (oper),Y  51    2     5*
fn eor(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);

    cpu.a = cpu.a ^ val;
    cpu.flags.z = cpu.a == 0;
    cpu.flags.n = cpu.a & 0x80 == 0;
}

/// LSR  Shift One Bit Right (Memory or Accumulator)
///
/// 0 -> [76543210] -> C             N Z C I D V
///                                  0 + + - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// accumulator   LSR A         4A    1     2
/// zeropage      LSR oper      46    2     5
/// zeropage,X    LSR oper,X    56    2     6
/// absolute      LSR oper      4E    3     6
/// absolute,X    LSR oper,X    5E    3     7
fn lsr(cpu: &mut CPU, mode: AddressingMode) {
    let r = mode(cpu);
    let val = r.read(cpu);

    cpu.flags.c = val & 0b00000001 > 0;
    let res = (val >> 1) & 0b01111111;
    r.write(cpu, res);

    cpu.flags.z = res == 0;
    cpu.flags.n = res & 0x80 == 0;
}

/// JMP  Jump to New Location
///
/// (PC+1) -> PCL                    N Z C I D V
/// (PC+2) -> PCH                    - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// absolute      JMP oper      4C    3     3
/// indirect      JMP (oper)    6C    3     5
fn jmp(cpu: &mut CPU, mode: AddressingMode) {
    let addr = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("JSR requires Fetched::Addr from AddressingMode!")
    };

    cpu.pc = addr;
    cpu.cycles += 1;
}

/// BVC  Branch on Overflow Clear
///
/// branch on V = 0                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BVC oper      50    2     2**
fn bvc(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BVC requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.v == false {
        branch(cpu, addr_rel);
    }
}

/// RTS  Return from Subroutine
///
/// pull PC, PC+1 -> PC              N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       RTS           60    1     6
fn rts(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);

    cpu.sp += 1;
    let lo = cpu.read(0x100 + cpu.sp as u16);
    cpu.sp += 1;
    let hi = cpu.read(0x100 + cpu.sp as u16);

    cpu.pc = (lo as u16 | (hi as u16) << 8) + 1;
    cpu.cycles += 4;
}

/// PLA  Pull Accumulator from Stack
///
/// pull A                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       PLA           68    1     4
fn pla(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);

    cpu.sp += 1;
    cpu.a = cpu.read(0x100 + cpu.sp as u16);
    cpu.flags.z = cpu.a == 0;
    cpu.flags.n = cpu.a & 0x80 == 0;
    cpu.cycles += 2;
}

/// ADC  Add Memory to Accumulator with Carry
///
/// A + M + C -> A, C                N Z C I D V
///                                  + + + - - +
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     ADC #oper     69    2     2
/// zeropage      ADC oper      65    2     3
/// zeropage,X    ADC oper,X    75    2     4
/// absolute      ADC oper      6D    3     4
/// absolute,X    ADC oper,X    7D    3     4*
/// absolute,Y    ADC oper,Y    79    3     4*
/// (indirect,X)  ADC (oper,X)  61    2     6
/// (indirect),Y  ADC (oper),Y  71    2     5*
fn adc(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);
    let tmp = cpu.a as u16 + val as u16 + if cpu.flags.c { 1 } else { 0 };
    let a = cpu.a as u16;

    cpu.flags.z = tmp & 0x00FF == 0;
    cpu.flags.v = (!(a ^ val as u16) & ((a ^ tmp) & 0x80)) != 0;
    cpu.flags.n = tmp & 0x80 == 0;

    cpu.a = (tmp & 0x00FF) as u8;
}

/// BVS  Branch on Overflow Set
///
/// branch on V = 1                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BVC oper      70    2     2**
fn bvs(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BVS requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.v == true {
        branch(cpu, addr_rel);
    }
}

/// ROL  Rotate One Bit Left (Memory or Accumulator)
///
/// C <- [76543210] <- C             N Z C I D V
///                                  + + + - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// accumulator   ROL A         2A    1     2
/// zeropage      ROL oper      26    2     5
/// zeropage,X    ROL oper,X    36    2     6
/// absolute      ROL oper      2E    3     6
/// absolute,X    ROL oper,X    3E    3     7
fn rol(cpu: &mut CPU, mode: AddressingMode) {
    let r = mode(cpu);
    let val = r.read(cpu);
    cpu.cycles += 2;

    let old_carry = if cpu.flags.c { 1 } else { 0 };
    cpu.flags.c = val & 0b00000001 > 0;
    let res = (val << 1) | old_carry << 7;
    r.write(cpu, res);

    cpu.flags.z = res == 0;
    cpu.flags.n = res & 0x80 == 0;
}

/// ROR  Rotate One Bit Right (Memory or Accumulator)
///
/// C -> [76543210] -> C             N Z C I D V
///                                  + + + - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// accumulator   ROR A         6A    1     2
/// zeropage      ROR oper      66    2     5
/// zeropage,X    ROR oper,X    76    2     6
/// absolute      ROR oper      6E    3     6
/// absolute,X    ROR oper,X    7E    3     7
fn ror(cpu: &mut CPU, mode: AddressingMode) {
    let r = mode(cpu);
    let val = r.read(cpu);
    cpu.cycles += 2;

    let old_carry = if cpu.flags.c { 1 } else { 0 };
    cpu.flags.c = val & 0b00000001 > 0;
    let res = (val >> 1) | old_carry << 7;
    r.write(cpu, res);

    cpu.flags.z = res == 0;
    cpu.flags.n = res & 0x80 == 0;
}

/// SEI  Set Interrupt Disable Status
///
/// 1 -> I                           N Z C I D V
///                                  - - - 1 - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       SEI           78    1     2
fn sei(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.flags.i = true;
}


/// STY  Store Index Y in Memory
///
/// Y -> M                           N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// zeropage      STY oper      84    2     3
/// zeropage,X    STY oper,X    94    2     4
/// absolute      STY oper      8C    3     4
fn sty(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu).write(cpu, cpu.y);
}

/// STA  Store Accumulator in Memory
///
/// A -> M                           N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// zeropage      STA oper      85    2     3
/// zeropage,X    STA oper,X    95    2     4
/// absolute      STA oper      8D    3     4
/// absolute,X    STA oper,X    9D    3     5
/// absolute,Y    STA oper,Y    99    3     5
/// (indirect,X)  STA (oper,X)  81    2     6
/// (indirect),Y  STA (oper),Y  91    2     6
fn sta(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu).write(cpu, cpu.a);
}

/// BCC  Branch on Carry Clear
///
/// branch on C = 0                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BCC oper      90    2     2**
fn bcc(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BCC requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.c == false {
        branch(cpu, addr_rel);
    }
}

/// STX  Store Index X in Memory
///
/// X -> M                           N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// zeropage      STX oper      86    2     3
/// zeropage,Y    STX oper,Y    96    2     4
/// absolute      STX oper      8E    3     4
fn stx(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu).write(cpu, cpu.x);
}

/// DEY  Decrement Index Y by One
///
/// Y - 1 -> Y                       N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       DEC           88    1     2
fn dey(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.y = cpu.y.wrapping_sub(1);
    cpu.flags.z = cpu.y == 0;
    cpu.flags.n = cpu.y & 0x80 == 0;
}

/// LDY  Load Index Y with Memory
///
/// M -> Y                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     LDY #oper     A0    2     2
/// zeropage      LDY oper      A4    2     3
/// zeropage,X    LDY oper,X    B4    2     4
/// absolute      LDY oper      AC    3     4
/// absolute,X    LDY oper,X    BC    3     4*
fn ldy(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);

    cpu.y = val;
    cpu.flags.z = cpu.y == 0;
    cpu.flags.n = cpu.y & 0x80 == 0;
}


/// LDX  Load Index X with Memory
///
/// M -> X                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     LDX #oper     A2    2     2
/// zeropage      LDX oper      A6    2     3
/// zeropage,Y    LDX oper,Y    B6    2     4
/// absolute      LDX oper      AE    3     4
/// absolute,Y    LDX oper,Y    BE    3     4*
fn ldx(cpu: &mut CPU, mode: AddressingMode) {
    cpu.x = mode(cpu).read(cpu);
    cpu.flags.z = cpu.x == 0x00;
    cpu.flags.n = cpu.x & 0x80 == 0;
}

/// TAY  Transfer Accumulator to Index Y
///
/// A -> Y                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       TAY           A8    1     2
fn tay(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.y = cpu.a;
    cpu.flags.z = cpu.x == 0;
    cpu.flags.n = cpu.x & 0x80 == 0;
}

/// TYA  Transfer Index Y to Accumulator
///
/// Y -> A                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       TYA           98    1     2
fn tya(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.a = cpu.y;
}

/// TAX  Transfer Accumulator to Index X
///
/// A -> X                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       TAX           AA    1     2
fn tax(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.x = cpu.a;
    cpu.flags.z = cpu.x == 0;
    cpu.flags.n = cpu.x & 0x80 == 0;
}

/// TXA  Transfer Index X to Accumulator
///
/// X -> A                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       TXA           8A    1     2
fn txa(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.a = cpu.x;
}

/// TSX  Transfer Stack Pointer to Index X
///
/// SP -> X                          N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       TSX           BA    1     2
fn tsx(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.x = cpu.sp;
    cpu.flags.z = cpu.x == 0;
    cpu.flags.n = cpu.x & 0x80 == 0;
}

/// TXS  Transfer Index X to Stack Register
///
/// X -> SP                          N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       TXS           9A    1     2
fn txs(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.sp = cpu.x;
}

/// LDA  Load Accumulator with Memory
///
/// M -> A                           N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     LDA #oper     A9    2     2
/// zeropage      LDA oper      A5    2     3
/// zeropage,X    LDA oper,X    B5    2     4
/// absolute      LDA oper      AD    3     4
/// absolute,X    LDA oper,X    BD    3     4*
/// absolute,Y    LDA oper,Y    B9    3     4*
///
/// (indirect,X)  LDA (oper,X)  A1    2     6
/// (indirect),Y  LDA (oper),Y  B1    2     5*
fn lda(cpu: &mut CPU, mode: AddressingMode) {
    let res = mode(cpu).read(cpu);
    cpu.a = res;

    cpu.flags.z = res == 0x00;
    cpu.flags.n = res & 0x80 == 0x80;
}

/// BCS  Branch on Carry Set
///
/// branch on C = 1                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BCS oper      B0    2     2**
fn bcs(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BCS requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.c == true {
        branch(cpu, addr_rel);
    }
}

/// CLV  Clear Overflow Flag
///
/// 0 -> V                           N Z C I D V
///                                  - - - - - 0
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       CLV           B8    1     2
fn clv(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.flags.v = false;
}

/// CPY  Compare Memory and Index Y
///
/// Y - M                            N Z C I D V
///                                  + + + - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     CPY #oper     C0    2     2
/// zeropage      CPY oper      C4    2     3
/// absolute      CPY oper      CC    3     4
fn cpy(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);
    let tmp = (cpu.y as u16).wrapping_sub(val as u16);

    cpu.flags.c = cpu.y >= val;
    cpu.flags.z = tmp & 0x00FF == 0x0000;
    cpu.flags.n = tmp & 0x0080 == 0x0080;
}

/// INC  Increment Memory by One
///
/// M + 1 -> M                       N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// zeropage      INC oper      E6    2     5
/// zeropage,X    INC oper,X    F6    2     6
/// absolute      INC oper      EE    3     6
/// absolute,X    INC oper,X    FE    3     7
fn inc(cpu: &mut CPU, mode: AddressingMode) {
    let r = mode(cpu);
    let val = r.read(cpu);

    let res = val.wrapping_add(1);
    r.write(cpu, res);
    cpu.flags.z = res == 0;
    cpu.flags.n = res & 0x80 == 0;
    cpu.cycles += 2;
}

/// DEC  Decrement Memory by One
///
/// M - 1 -> M                       N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// zeropage      DEC oper      C6    2     5
/// zeropage,X    DEC oper,X    D6    2     6
/// absolute      DEC oper      CE    3     6
/// absolute,X    DEC oper,X    DE    3     7
fn dec(cpu: &mut CPU, mode: AddressingMode) {
    let r = mode(cpu);
    let val = r.read(cpu);

    let res = val.wrapping_sub(1);
    r.write(cpu, res);
    cpu.flags.z = res == 0;
    cpu.flags.n = res & 0x80 == 0;
    cpu.cycles += 2;
}

/// INX  Increment Index X by One
///
/// X + 1 -> X                       N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       INX           E8    1     2
fn inx(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.y = cpu.x.wrapping_add(1);
    cpu.flags.z = cpu.x == 0;
    cpu.flags.n = cpu.x & 0x80 == 0;
}

/// INY  Increment Index Y by One
///
/// Y + 1 -> Y                       N Z C I D V
///                                  + + - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       INY           C8    1     2
fn iny(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.y = cpu.y.wrapping_add(1);
    cpu.flags.z = cpu.y == 0;
    cpu.flags.n = cpu.y & 0x80 == 0;
}

/// CMP  Compare Memory with Accumulator
///
/// A - M                            N Z C I D V
///                                  + + + - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     CMP #oper     C9    2     2
/// zeropage      CMP oper      C5    2     3
/// zeropage,X    CMP oper,X    D5    2     4
/// absolute      CMP oper      CD    3     4
/// absolute,X    CMP oper,X    DD    3     4*
/// absolute,Y    CMP oper,Y    D9    3     4*
/// (indirect,X)  CMP (oper,X)  C1    2     6
/// (indirect),Y  CMP (oper),Y  D1    2     5*
fn cmp(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);
    let tmp = (cpu.a as u16).wrapping_sub(val as u16);

    cpu.flags.c = cpu.a >= val;
    cpu.flags.z = tmp & 0x00FF == 0x0000;
    cpu.flags.n = tmp & 0x0080 == 0x0080;
}

/// BNE  Branch on Result not Zero
///
/// branch on Z = 0                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BNE oper      D0    2     2**
fn bne(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BNE requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.z == false {
        branch(cpu, addr_rel);
    }
}

/// CLD  Clear Decimal Mode
///
/// 0 -> D                           N Z C I D V
///                                  - - - - 0 -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       CLD           D8    1     2
fn cld(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.flags.d = false;
}

/// CPX  Compare Memory and Index X
///
/// X - M                            N Z C I D V
///                                  + + + - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     CPX #oper     E0    2     2
/// zeropage      CPX oper      E4    2     3
/// absolute      CPX oper      EC    3     4
fn cpx(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu);

    let tmp = (cpu.x as u16).wrapping_sub(val as u16);
    cpu.flags.c = cpu.x >= val;
    cpu.flags.z = tmp & 0x00FF == 0;
    cpu.flags.n = tmp & 0x80 == 0;
}

/// SBC  Subtract Memory from Accumulator with Borrow
///
/// A - M - C -> A                   N Z C I D V
///                                  + + + - - +
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// immidiate     SBC #oper     E9    2     2
/// zeropage      SBC oper      E5    2     3
/// zeropage,X    SBC oper,X    F5    2     4
/// absolute      SBC oper      ED    3     4
/// absolute,X    SBC oper,X    FD    3     4*
/// absolute,Y    SBC oper,Y    F9    3     4*
/// (indirect,X)  SBC (oper,X)  E1    2     6
/// (indirect),Y  SBC (oper),Y  F1    2     5*
fn sbc(cpu: &mut CPU, mode: AddressingMode) {
    let val = mode(cpu).read(cpu) as u16;

    let res = val ^ 0x00FF;
    let tmp = cpu.a as u16 + res + if cpu.flags.c { 1 } else { 0 };
    cpu.flags.c = tmp & 0xFF00 != 0;
    cpu.flags.z = tmp & 0x00FF == 0;
    cpu.flags.v = (tmp ^ cpu.a as u16) & (tmp ^ res) & 0x80 != 0;
    cpu.flags.n = tmp & 0x80 == 0;
    cpu.a = (tmp & 0x00FF) as u8;
}

/// NOP  No Operation
///
/// ---                              N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       NOP           EA    1     2
fn nop(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
}

/// BEQ  Branch on Result Zero
///
/// branch on Z = 1                  N Z C I D V
///                                  - - - - - -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// relative      BEQ oper      F0    2     2**
fn beq(cpu: &mut CPU, mode: AddressingMode) {
    let addr_rel = match mode(cpu) {
        Fetched::Addr(a) => a,
        _ => panic!("BEQ requires Fetched::Addr from AddressingMode!")
    };

    if cpu.flags.z == true {
        branch(cpu, addr_rel);
    }
}

/// SED  Set Decimal Flag
///
/// 1 -> D                           N Z C I D V
///                                  - - - - 1 -
///
/// addressing    assembler    opc  bytes  cyles
/// --------------------------------------------
/// implied       SED           F8    1     2
fn sed(cpu: &mut CPU, mode: AddressingMode) {
    mode(cpu);
    cpu.flags.d = true;
}

fn branch(cpu: &mut CPU, addr_rel: u16) {
    cpu.cycles += 1;
    let addr_abs = cpu.pc.wrapping_add(addr_rel);
    if addr_abs & 0xFF00 != cpu.pc & 0xFF00 {
        cpu.cycles += 1;
    }
    cpu.pc = addr_abs;
}

static OPCODES: phf::Map<u8, (Operation, AddressingMode)> = phf_map! {
    0x00u8 => (brk, implied),
    0x01u8 => (ora, indirect_x),
    0x04u8 => (nop, zero_page),
    0x05u8 => (ora, zero_page),
    0x06u8 => (asl, zero_page),
    0x08u8 => (php, implied),
    0x09u8 => (ora, implied),
    0x0Au8 => (asl, implied),
    // 0x0Cu8 => (asl, absolute),
    0x0Du8 => (ora, absolute),
    0x0Eu8 => (asl, absolute),
    0x10u8 => (bpl, relative),
    0x11u8 => (ora, indirect_y),
    0x15u8 => (ora, zero_page_x),
    0x16u8 => (asl, zero_page_x),
    0x18u8 => (clc, implied),
    0x19u8 => (ora, absolute_y),
    0x1Du8 => (ora, absolute_x),
    0x1Eu8 => (asl, absolute_x),
    0x20u8 => (jsr, absolute),
    0x21u8 => (and, indirect_x),
    0x24u8 => (bit, zero_page),
    0x25u8 => (and, zero_page),
    0x26u8 => (rol, zero_page),
    0x28u8 => (plp, implied),
    0x29u8 => (and, immediate),
    0x2Au8 => (rol, implied),
    0x2Cu8 => (bit, absolute),
    0x2Du8 => (and, absolute),
    0x2Eu8 => (rol, absolute),
    0x30u8 => (bmi, relative),
    0x31u8 => (and, indirect_y),
    0x35u8 => (and, zero_page_x),
    0x36u8 => (rol, zero_page_x),
    0x38u8 => (sec, implied),
    0x39u8 => (and, absolute_y),
    0x3Du8 => (and, absolute_x),
    0x3Eu8 => (rol, absolute_x),
    0x40u8 => (rti, implied),
    0x41u8 => (eor, indirect_x),
    0x45u8 => (eor, zero_page),
    0x46u8 => (lsr, zero_page),
    0x48u8 => (pha, implied),
    0x49u8 => (eor, immediate),
    0x4Au8 => (lsr, implied),
    0x4Cu8 => (jmp, absolute),
    0x4Du8 => (eor, absolute),
    0x4Eu8 => (lsr, absolute),
    0x50u8 => (bvc, relative),
    0x51u8 => (eor, indirect_y),
    0x55u8 => (eor, zero_page_x),
    0x56u8 => (lsr, zero_page_x),
    0x59u8 => (eor, absolute_y),
    0x5Du8 => (eor, absolute_x),
    0x5Eu8 => (lsr, absolute_x),
    0x60u8 => (rts, implied),
    0x61u8 => (adc, indirect_x),
    0x65u8 => (adc, zero_page),
    0x66u8 => (ror, zero_page),
    0x68u8 => (pla, implied),
    0x69u8 => (adc, immediate),
    0x6Du8 => (adc, absolute),
    0x70u8 => (bvs, relative),
    0x71u8 => (adc, indirect_y),
    0x75u8 => (adc, zero_page_x),
    0x76u8 => (ror, zero_page_x),
    0x79u8 => (adc, absolute_y),
    0x7Eu8 => (ror, absolute_x),
    0x6Au8 => (ror, implied),
    0x6Cu8 => (jmp, indirect),
    0x6Eu8 => (ror, absolute),
    0x78u8 => (sei, implied),
    0x7Du8 => (adc, absolute_x),
    0x81u8 => (sta, indirect_x),
    0x84u8 => (sty, zero_page),
    0x85u8 => (sta, zero_page),
    0x86u8 => (stx, zero_page),
    0x88u8 => (dey, implied),
    0x8Au8 => (txa, implied),
    0x8Cu8 => (sty, absolute),
    0x8Du8 => (sta, absolute),
    0x8Eu8 => (stx, absolute),
    0x90u8 => (bcc, relative),
    0x91u8 => (sta, indirect_y),
    0x94u8 => (sty, zero_page_x),
    0x95u8 => (sta, zero_page_x),
    0x96u8 => (stx, zero_page_y),
    0x98u8 => (tya, implied),
    0x99u8 => (sta, absolute_y),
    0x9Au8 => (txs, implied),
    0x9Du8 => (sta, absolute_x),
    0xA0u8 => (ldy, immediate),
    0xA1u8 => (lda, indirect_x),
    0xA2u8 => (ldx, immediate),
    0xA4u8 => (ldy, zero_page),
    0xA5u8 => (lda, zero_page),
    0xA6u8 => (ldx, zero_page),
    0xAAu8 => (tax, implied),
    0xACu8 => (ldy, absolute),
    0xAEu8 => (ldx, absolute),
    0xA8u8 => (tay, implied),
    0xA9u8 => (lda, immediate),
    0xADu8 => (lda, absolute),
    0xB0u8 => (bcs, relative),
    0xB1u8 => (lda, indirect_y),
    0xB4u8 => (ldy, zero_page_x),
    0xB5u8 => (lda, zero_page_x),
    0xB6u8 => (ldx, zero_page_y),
    0xB8u8 => (clv, implied),
    0xB9u8 => (lda, absolute_y),
    0xBAu8 => (tsx, implied),
    0xBCu8 => (ldy, absolute_x),
    0xBDu8 => (lda, absolute_x),
    0xBEu8 => (ldx, absolute_y),
    0xC0u8 => (cpy, immediate),
    0xC1u8 => (cmp, indirect_x),
    0xC4u8 => (cpy, zero_page),
    0xC6u8 => (dec, zero_page),
    0xC8u8 => (iny, implied),
    0xC9u8 => (cmp, immediate),
    0xC5u8 => (cmp, zero_page),
    0xCCu8 => (cpy, absolute),
    0xCDu8 => (cmp, absolute),
    0xCEu8 => (dec, absolute),
    0xD0u8 => (bne, relative),
    0xD1u8 => (cmp, indirect_y),
    0xD5u8 => (cmp, zero_page_x),
    0xD6u8 => (dec, zero_page_x),
    0xD8u8 => (cld, implied),
    0xD9u8 => (cmp, absolute_y),
    0xDDu8 => (cmp, absolute_x),
    0xDEu8 => (dec, absolute_x),
    0xE0u8 => (cpx, immediate),
    0xE1u8 => (sbc, indirect_x),
    0xE4u8 => (cpx, zero_page),
    0xE5u8 => (sbc, zero_page),
    0xE6u8 => (inc, zero_page),
    0xE8u8 => (inx, implied),
    0xE9u8 => (sbc, immediate),
    0xEAu8 => (nop, implied),
    0xECu8 => (cpx, absolute),
    0xEDu8 => (sbc, absolute),
    0xEEu8 => (inc, absolute),
    0xF0u8 => (beq, relative),
    0xF1u8 => (sbc, indirect_y),
    0xF5u8 => (sbc, zero_page_x),
    0xF6u8 => (inc, zero_page_x),
    0xF8u8 => (sed, implied),
    0xF9u8 => (sbc, absolute_y),
    0xFDu8 => (sbc, absolute_x),
    0xFEu8 => (inc, absolute_x),
};

pub struct CPU {
    bus: Bus,
    // ppu: PPU,

    flags: Flags,
    cycles: u32,

    pc: u16,
    sp: u8,

    a: u8,
    x: u8,
    y: u8,
}

impl CPU {
    pub fn new(cart: &mut Cartridge) -> Self {
        let mut bus = Bus::new(cart);
        let ppu = PPU::new(&mut bus);
        let flags = Flags::new();
        let pc = 0xC000u16; // bus.rw(0xFFFC);

        CPU {
            bus: bus,
            // ppu: ppu,

            flags: flags,
            cycles: 0,

            pc: pc,
            sp: 0xFD,

            a: 0x00,
            x: 0x00,
            y: 0x00,
        }
    }

    pub fn cycle(&mut self) {
        print!("{:04X} ", self.pc);

        let op = self.read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        println!(" {:02X}", op);

        match OPCODES.get(&op) {
            Some(&(operation, address_mode)) => operation(self, address_mode),
            None => panic!("Unkown opcode!")
        }

        // Sleep here
        self.cycles = 0;
    }

    fn get_flags(&self) -> u8 {
          ((self.flags.n as u8) << 7)
        + ((self.flags.v as u8) << 6)
        + ((self.flags.u as u8) << 5)
        + ((self.flags.b as u8) << 4)
        + ((self.flags.d as u8) << 3)
        + ((self.flags.i as u8) << 2)
        + ((self.flags.z as u8) << 1)
        + ((self.flags.c as u8) << 0)
    }

    fn set_flags(&mut self, s: u8) {
        self.flags.n = s & 0b10000000 > 0;
        self.flags.v = s & 0b01000000 > 0;
        self.flags.u = s & 0b00100000 > 0;
        self.flags.b = s & 0b00010000 > 0;
        self.flags.d = s & 0b00001000 > 0;
        self.flags.i = s & 0b00000100 > 0;
        self.flags.z = s & 0b00000010 > 0;
        self.flags.c = s & 0b00000001 > 0;
    }

    fn read(&self, addr: u16) -> u8 {
        self.bus.rb(addr)
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.bus.wb(addr, data)
    }
}


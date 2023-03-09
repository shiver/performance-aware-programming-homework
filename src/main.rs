use anyhow::Result;
use byteorder::ReadBytesExt;
use std::{
    fs::File,
    io::{Cursor, Read},
    path::Path,
};

fn load_binary(path: &str) -> Result<Vec<u8>> {
    let mut file = File::open(path)?;

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    Ok(buffer)
}

#[derive(Debug, PartialEq)]
enum ModField {
    MemoryNoDisplacement = 0b00,
    MemoryDisplacement8bit = 0b01,
    MemoryDisplacement16bit = 0b10,
    RegisterNoDisplacement = 0b11,
}

#[repr(u8)]
#[derive(Debug)]
enum Registers {
    al = 0b0000,
    cl = 0b0001,
    dl = 0b0010,
    bl = 0b0011,
    ah = 0b0100,
    ch = 0b0101,
    dh = 0b0110,
    bh = 0b0111,
    ax = 0b1000,
    cx = 0b1001,
    dx = 0b1010,
    bx = 0b1011,
    sp = 0b1100,
    bp = 0b1101,
    si = 0b1110,
    di = 0b1111,
}

fn main() -> Result<()> {
    let binary_filename = std::env::args()
        .nth(1)
        .expect("Please provide an assembled filename");
    let binary = load_binary(&binary_filename)?;

    let mut cursor = Cursor::new(binary);

    println!("bits 16");

    loop {
        println!("");

        let opcode = match cursor.read_u8() {
            Ok(byte) => byte,
            Err(_) => break
        };

        if (opcode >> 2) == 0b100010 {
            // MOV - Register/memory to/from register

            let w_field = opcode & 1; // how wide is the data, 8 or 16 bits
            let d_field = opcode >> 1 & 1; // if 1, destination is a register

            let next_byte = cursor.read_u8()?;

            let rm = next_byte & 0b111;
            let mod_field = unsafe { std::mem::transmute::<u8, ModField>(next_byte >> 6) };
            let reg = unsafe {
                std::mem::transmute::<u8, Registers>(w_field << 3 | (next_byte >> 3 & 0b111))
            };

            print!("mov ");

            if mod_field == ModField::RegisterNoDisplacement {
                let other_reg = unsafe { std::mem::transmute::<u8, Registers>(w_field << 3 | rm) };

                match d_field {
                    0 => print!("{:?}, {:?}", other_reg, reg),
                    1 => print!("{:?}, {:?}", reg, other_reg),
                    _ => (),
                }
            }
        } else if opcode >> 1 == 0b1100011 {
            // MOV - Immediate to register/memory
            println!("mov ;Immediate to register/memory");
        } else if opcode >> 4 == 0b1011 {
            // MOV - Register/memory to/from register
            println!("mov ;Register/memory to/from register");
            // cursor.read_u24();
        } else if opcode >> 1 == 0b1010000 {
            // MOV - Memory to accumulator
            println!("mov ;Memory to accumulator");
        } else if opcode >> 1 == 0b1010001 {
            // MOV - Accumulator to memory
            println!("mov ;Accumulator to memory");
        } else if opcode == 0b10001110 {
            // MOV - Register/memory to segment register
            println!("mov ;Register/memory to segment register");
        } else if opcode == 0b10001100 {
            // MOV - Segment register to register/memory
            println!("mov ;Segment register to register/memory");
        }

    }

    Ok(())
}

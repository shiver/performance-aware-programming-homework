use anyhow::Result;
use byteorder::{LittleEndian, ReadBytesExt};
use std::fmt;
use std::fs::File;
use std::io::{Cursor, Read, Seek};

fn load_binary(path: &str) -> Result<Vec<u8>> {
    let mut file = File::open(path)?;

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    Ok(buffer)
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(dead_code)]
enum Mode {
    MemoryNoDisplacement = 0b00,
    MemoryDisplacement8bit = 0b01,
    MemoryDisplacement16bit = 0b10,
    RegisterNoDisplacement = 0b11,
}

enum OperandType {
    Register(Register),
    Memory(String),
}

impl fmt::Display for OperandType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            OperandType::Register(v) => write!(f, "{:?}", v),
            OperandType::Memory(v) => write!(f, "{}", v),
        };
    }
}

#[repr(u8)]
#[derive(Debug)]
#[allow(non_camel_case_types, dead_code)]
enum Register {
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

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{:?}", self);
    }
}

fn get_displacement(mode: Mode, rm: u8, value: i16) -> String {
    return match mode {
        Mode::MemoryNoDisplacement => match rm {
            0b000 => format!("[bx + si]"),
            0b001 => format!("[bx + di]"),
            0b010 => format!("[bp + si]"),
            0b011 => format!("[bp + di]"),
            0b100 => format!("[si]"),
            0b101 => format!("[di]"),
            0b110 if value == 0 => format!("[bp]"),
            0b110 => format!("[{}]", value),
            0b111 => format!("[bx]"),

            _ => unreachable!(),
        },

        Mode::MemoryDisplacement16bit | Mode::MemoryDisplacement8bit => {
            let value_str = if value >= 0 {
                format!("+ {}", value)
            } else {
                format!("- {}", value.abs())
            };

            match rm {
                0b000 => format!("[bx + si {}]", value_str),
                0b001 => format!("[bx + di {}]", value_str),
                0b010 => format!("[bp + si {}]", value_str),
                0b011 => format!("[bp + di {}]", value_str),
                0b100 => format!("[si {}]", value_str),
                0b101 => format!("[di {}]", value_str),
                0b110 if value == 0 => format!("[bp]"),
                0b110 => format!("[{}]", value),
                0b111 => format!("[bx {}]", value_str),

                _ => unreachable!(),
            }
        }

        _ => unreachable!(),
    };
}

fn get_displacement_value(cursor: &mut Cursor<Vec<u8>>, mode: Mode, rm_field: u8) -> Result<i16> {
    let value = match mode {
        Mode::MemoryDisplacement8bit => cursor.read_i8()? as i16,
        Mode::MemoryDisplacement16bit => cursor.read_i16::<LittleEndian>()?,
        Mode::MemoryNoDisplacement if rm_field == 0b110 => cursor.read_i16::<LittleEndian>()?,
        _ => 0,
    };

    return Ok(value);
}

fn get_register(reg_field: u8, width: u8) -> Register {
    return match width {
        0 => unsafe { std::mem::transmute::<u8, Register>(reg_field) },
        1 => unsafe { std::mem::transmute::<u8, Register>(1 << 3 | reg_field) },
        _ => unreachable!(),
    };
}

fn get_mode(mod_field: u8) -> Mode {
    return unsafe { std::mem::transmute::<u8, Mode>(mod_field) };
}

fn get_other(
    cursor: &mut Cursor<Vec<u8>>,
    mode: Mode,
    rm_field: u8,
    w_field: u8,
) -> Result<OperandType> {
    return match mode {
        Mode::MemoryNoDisplacement => {
            let displacement_value = get_displacement_value(cursor, mode, rm_field)?;
            let displacement = get_displacement(mode, rm_field, displacement_value);

            Ok(OperandType::Memory(displacement))
        }

        Mode::MemoryDisplacement16bit | Mode::MemoryDisplacement8bit => {
            let displacement_value = get_displacement_value(cursor, mode, rm_field)?;
            let displacement = get_displacement(mode, rm_field, displacement_value);

            Ok(OperandType::Memory(displacement))
        }

        Mode::RegisterNoDisplacement => {
            let other_reg = get_register(rm_field, w_field);
            Ok(OperandType::Register(other_reg))
        }
    };
}

fn get_value(cursor: &mut Cursor<Vec<u8>>, is_word: bool, with_type: bool) -> Result<String> {
    let value: i16 = if !is_word {
        cursor.read_i8()? as i16
    } else {
        cursor.read_i16::<LittleEndian>()?
    };

    let size = match is_word {
        true if with_type => "byte ",
        false if with_type => "word ",
        _ => "",
    };

    return Ok(format!("{}{}", size, value));
}

fn op_field_to_op(op_field: u8) -> String {
    return match op_field {
        0b000 => format!("add"),
        0b101 => format!("sub"),
        0b111 => format!("cmp"),
        _ => unreachable!(),
    };
}

const SHOW_COMMENTS: bool = false;

fn main() -> Result<()> {
    let binary_filename = std::env::args()
        .nth(1)
        .expect("Please provide an assembled filename");
    let binary = load_binary(&binary_filename)?;

    let mut cursor = Cursor::new(binary);

    println!("bits 16");

    loop {
        // Show opcode address
        print!("{:#04X}: ", cursor.position());

        let opcode = match cursor.read_u8() {
            Ok(byte) => byte,
            Err(_) => break,
        };

        if (opcode >> 2) == 0b100010 {
            // MOV - Register/memory to/from register

            print!("mov ");

            let w_field = opcode & 1; // how wide is the data, 8 or 16 bits
            let d_field = opcode >> 1 & 1;

            let next_byte = cursor.read_u8()?;

            let rm_field = next_byte & 0b111;
            let mode = get_mode(next_byte >> 6);
            let reg = get_register(next_byte >> 3 & 0b111, w_field).to_string();

            let other = get_other(&mut cursor, mode, rm_field, w_field)?;
            let other = format!("{}", other);

            let (destination, source) = match d_field {
                0 => (other, reg),
                1 => (reg, other),
                _ => unreachable!(),
            };

            print!("{}, {}", destination, source);

            if SHOW_COMMENTS {
                print!(
                    " ; Register/memory to/from register \n\topcode={:#b} next_byte={:#b}\n\td={} w={} mod={:#03b} rm={:#b}\n",
                    opcode, next_byte, d_field, w_field, mode as u8, rm_field
                );
            }
        } else if opcode >> 1 == 0b1100011 {
            // MOV - Immediate to register/memory
            print!("mov ");

            let w_field = opcode & 0b1;
            let next_byte = cursor.read_u8()?;
            let rm_field = next_byte & 0b111;
            let mod_field = next_byte >> 6;

            let mode = get_mode(mod_field);
            let other = get_other(&mut cursor, mode, rm_field, w_field)?;
            let is_word = if w_field == 1 { true } else { false };
            let immediate = get_value(&mut cursor, is_word, true)?;

            print!("{}, {}", other, immediate);

            if SHOW_COMMENTS {
                print!(
                    "; Immediate to register/memory \n\topcode={:#b} w={} mod={:#b}, rm={:#b}\n",
                    opcode, w_field, mod_field, rm_field
                );
            }
        } else if opcode >> 4 == 0b1011 {
            // MOV - Immediate to register
            let w_field = opcode >> 3 & 0b1;
            let reg_field = opcode & 0b111;
            let reg = get_register(reg_field, w_field);

            print!("mov {}, ", reg);
            if w_field == 0 {
                print!("{}", cursor.read_i8()?);
            } else {
                print!("{}", cursor.read_i16::<LittleEndian>()?);
            }
        } else if opcode >> 1 == 0b1010000 {
            // MOV - Memory to accumulator
            print!("mov ax, ");

            let w_field = opcode & 1;

            let value = if w_field == 0 {
                cursor.read_i8()? as i16
            } else {
                cursor.read_i16::<LittleEndian>()?
            };

            print!("[{}]", value);

            if SHOW_COMMENTS {
                print!("; Memory to accumulator");
            }
        } else if opcode >> 1 == 0b1010001 {
            // MOV - Accumulator to memory
            print!("mov ");

            let w_field = opcode & 1;
            let value = if w_field == 0 {
                cursor.read_i8()? as i16
            } else {
                cursor.read_i16::<LittleEndian>()?
            };

            print!("[{}], ax", value);

            if SHOW_COMMENTS {
                print!("; Accumulator to memory");
            }
        } else if opcode == 0b10001110 {
            // MOV - Register/memory to segment register
            print!("mov ;Register/memory to segment register");
        } else if opcode == 0b10001100 {
            // MOV - Segment register to register/memory
            print!("mov ;Segment register to register/memory");
        } else if opcode >> 2 == 0b000000 {
            // ADD - Register/memory with register to either
            print!("add ");

            let w_field = opcode & 1;
            let d_field = opcode >> 1 & 1;

            let next_byte = cursor.read_u8()?;
            let rm_field = next_byte & 0b111;
            let reg_field = next_byte >> 3 & 0b111;
            let mod_field = next_byte >> 6;

            let mode = get_mode(mod_field);

            let register = get_register(reg_field, w_field);
            let register = format!("{}", register);

            let other = get_other(&mut cursor, mode, rm_field, w_field)?;
            let other = format!("{}", other);

            let (destination, source) = match d_field {
                0 => (other, register),
                1 => (register, other),
                _ => unreachable!(),
            };

            print!("{}, {}", destination, source);

            if SHOW_COMMENTS {
                print!("; Register/memory with register to either");
            }
        } else if opcode >> 2 == 0b100000 {
            // ADD, SUB, CMP - Immediate to register/memory

            let w_field = opcode & 1;
            let s_field = opcode >> 1 & 1;

            let next_byte = cursor.read_u8()?;
            let rm_field = next_byte & 0b111;
            let mod_field = next_byte >> 6;

            let op_field = next_byte >> 3 & 0b111;
            print!("{} ", op_field_to_op(op_field));

            let mode = get_mode(mod_field);
            let other = get_other(&mut cursor, mode, rm_field, w_field)?;

            let with_type = match other {
                OperandType::Memory(_) => true,
                _ => false,
            };

            // TODO: I think this is_word calculation is wrong? Should it be "== 0b01"?
            // However, setting it to the "correct" value gives the incorrect results for listing 41?
            let is_word = if s_field == 0 && w_field == 1 {
                true
            } else {
                false
            };
            let immediate = get_value(&mut cursor, is_word, with_type)?;

            print!("{}, {}", other, immediate);

            if SHOW_COMMENTS {
                print!("; Immediate to register/memory mod={:#b} other={} rm={:#b} s={} w={} is_word={}",
                    mod_field, other, rm_field, s_field, w_field, is_word);
            }
        } else if opcode >> 1 == 0b0000010 {
            // ADD - Immediate to accumulator
            print!("add ");

            let w_field = opcode & 1;

            let is_word = if w_field == 1 { true } else { false };
            let immediate = get_value(&mut cursor, is_word, false)?;

            let destination = if is_word {
                format!("ax")
            } else {
                format!("al")
            };

            print!("{}, {}", destination, immediate);

            if SHOW_COMMENTS {
                print!("; Immediate to accumulator");
            }
        } else if opcode >> 2 == 0b001010 {
            // SUB - Register/memory and register to either
            print!("sub ");

            let w_field = opcode & 1;
            let d_field = opcode >> 1 & 1;

            let next_byte = cursor.read_u8()?;
            let rm_field = next_byte & 0b111;
            let reg_field = next_byte >> 3 & 0b111;
            let mod_field = next_byte >> 6;

            let mode = get_mode(mod_field);

            let register = get_register(reg_field, w_field);
            let register = format!("{}", register);

            let other = get_other(&mut cursor, mode, rm_field, w_field)?;
            let other = format!("{}", other);

            let (destination, source) = match d_field {
                0 => (other, register),
                1 => (register, other),
                _ => unreachable!(),
            };

            print!("{}, {}", destination, source);

            if SHOW_COMMENTS {
                print!("; Register/memory and register to either");
            }
        } else if opcode >> 1 == 0b0010110 {
            // SUB - Immediate from accumulator
            print!("sub ");

            let w_field = opcode & 1;

            let is_word = if w_field == 1 { true } else { false };
            let immediate = get_value(&mut cursor, is_word, false)?;

            let destination = if is_word {
                format!("ax")
            } else {
                format!("al")
            };

            print!("{}, {}", destination, immediate);

            if SHOW_COMMENTS {
                print!("; Immediate from accumulator");
            }
        } else if opcode >> 2 == 0b001110 {
            // CMP - Register/memory and register
            print!("cmp ");

            let w_field = opcode & 1;
            let d_field = opcode >> 1 & 1;

            let next_byte = cursor.read_u8()?;
            let rm_field = next_byte & 0b111;
            let reg_field = next_byte >> 3 & 0b111;
            let mod_field = next_byte >> 6;

            let mode = get_mode(mod_field);

            let register = get_register(reg_field, w_field);
            let register = format!("{}", register);

            let other = get_other(&mut cursor, mode, rm_field, w_field)?;
            let other = format!("{}", other);

            let (destination, source) = match d_field {
                0 => (other, register),
                1 => (register, other),
                _ => unreachable!(),
            };

            print!("{}, {}", destination, source);

            if SHOW_COMMENTS {
                print!("; Register/memory and register");
            }
        } else if opcode >> 1 == 0b0011110 {
            // CMP - Immediate with accumulator
            print!("cmp ");

            let w_field = opcode & 1;

            let is_word = if w_field == 1 { true } else { false };
            let immediate = get_value(&mut cursor, is_word, false)?;

            let destination = if is_word {
                format!("ax")
            } else {
                format!("al")
            };

            print!("{}, {}", destination, immediate);

            if SHOW_COMMENTS {
                print!("; Immediate with accumulator");
            }
        } else {
            continue;
        }

        println!("");
    }

    Ok(())
}

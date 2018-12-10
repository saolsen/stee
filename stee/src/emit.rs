use super::types::*;

use byteorder::{LittleEndian, WriteBytesExt};

fn write_section_code(s: WasmSectionCode, buf: &mut Vec<u8>) {
    buf.push(s as u8);
}

fn write_extern_kind(k: WasmExternalKind, buf: &mut Vec<u8>) {
    buf.push(k as u8);
}

fn write_type(t: WasmType, buf : &mut Vec<u8>) {
    buf.push(t as u8);
}

fn write_op(op: WasmOperator, buf : &mut Vec<u8>) {
    buf.push(op as u8);
}

fn write_size(val: usize, mut buf: &mut Vec<u8>) {
    write_u64(val as u64, &mut buf);
}

fn write_u64(mut val: u64, buf: &mut Vec<u8>) {
    const CONTINUATION_BIT: u8 = 1 << 7;
    loop {
        let mut byte: u8 = (val & (std::u8::MAX as u64)) as u8 & !CONTINUATION_BIT;
        val >>= 7;
        if val != 0 {
            byte |= CONTINUATION_BIT;
        }
        buf.push(byte);
        if val == 0 {
            break;
        }
    }
}

fn write_bytes(bytes: &[u8], buf: &mut Vec<u8>) {
    for byte in bytes {
        buf.push(*byte);
    }
}

enum WasmSectionCode {
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    // Start = 8,
    // Element = 9,
    Code = 10,
    // Data = 11
}

enum WasmExternalKind {
    Function = 0,
    // Table = 1,
    // Memory = 2,
    // Global = 4
}

enum WasmType {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
    AnyFunc = 0x70,
    Func = 0x60,
    EmptyBlock = 0x40,
}

enum WasmOperator {
    // Unreachable = 0x00,
    // Nop = 0x01,
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0b,
    Br = 0x0c,
    BrIf = 0x0d,
    BrTable = 0x0e,
    Return = 0x0f,
    
    Call = 0x10,
    // CallIndirect = 0x11,

    // Drop = 0x1a,
    // Select = 0x1b,

    GetLocal = 0x20,
    SetLocal = 0x21,
    // TeeLocal = 0x22,
    GetGlobal = 0x23,
    SetGlobal = 0x24,

    // I32Load = 0x28,
    // I64Load = 0x29,
    // F32Load = 0x2a,
    // F64Load = 0x2b,
    // I32Load8S = 0x2c,
    // I32Load8U = 0x2d,
    // I32Load16S = 0x2e,
    // I32Load16U = 0x2f,
    // I64Load8S = 0x30,
    // I64Load8U = 0x31,
    // I64Load16S = 0x32,
    // I64Load16U = 0x33,
    // I64Load32S = 0x34,
    // I64Load32U = 0x35,
    // I32Store = 0x36,
    // I64Store = 0x37,
    // F32Store = 0x38,
    // F64Store = 0x39,
    // I32Store8 = 0x3a,
    // I32Store16 = 0x3b,
    // I64Store8 = 0x3c,
    // I64Store16 = 0x3d,
    // I64Store32 = 0x3e,
    // CurrentMemory = 0x3f,
    // GrowMemory = 0x40,

    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,

    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4a,
    I32GtU = 0x4b,
    I32LeS = 0x4c,
    I32LeU = 0x4d,
    I32GeS = 0x4e,
    I32GeU = 0x4f,
    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5a,
    F32Eq = 0x5b,
    F32Ne = 0x5c,
    F32Lt = 0x5d,
    F32Gt = 0x5e,
    F32Le = 0x5f,
    F32Ge = 0x60,
    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,

    // I32Clz = 0x67,
    // I32Ctz = 0x68,
    // I32Popcnt = 0x69,

    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32DivS = 0x6d,
    I32DivU = 0x6e,
    I32RemS = 0x6f,
    I32RemU = 0x70,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,

    // I32Shl = 0x74,
    // I32ShrS = 0x75,
    // I32ShrU = 0x76,
    // I32Rotl = 0x77,
    // I32Rotr = 0x78,

    // I64Clz = 0x79,
    // I64Ctz = 0x7a,
    // I64Popcnt = 0x7b,

    I64Add = 0x7c,
    I64Sub = 0x7d,
    I64Mul = 0x7e,
    I64DivS = 0x7f,
    I64DivU = 0x80,
    I64RemS = 0x81,
    I64RemU = 0x82,
    I64And = 0x83,
    I64Or = 0x84,
    I64Xor = 0x85,

    // I64Shl = 0x86,
    // I64ShrS = 0x87,
    // I64ShrU = 0x88,
    // I64Rotl = 0x89,
    // I64Rotr = 0x8a,

    // F32Abs = 0x8b,
    // F32Neg = 0x8c,
    // F32Ceil = 0x8d,
    // F32Floor = 0x8e,
    // F32Trunc = 0x8f,
    // F32Nearest = 0x90,
    // F32Sqrt = 0x91,

    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,

    // F32Min = 0x96,
    // F32Max = 0x97,
    // F32Copysign = 0x98,

    // F64Abs = 0x99,
    // F64Neg = 0x9a,
    // F64Ceil = 0x9b,
    // F64Floor = 0x9c,
    // F64Trunc = 0x9d,
    // F64Nearest = 0x9e,
    // F64Sqrt = 0x9f,

    F64Add = 0xa0,
    F64Sub = 0xa1,
    F64Mul = 0xa2,
    F64Div = 0xa3,

    // F64Min = 0xa4,
    // F64Max = 0xa5,
    // F64Copysign = 0xa6,

    // I32WrapI64 = 0xa7,
    // I32TruncSF32 = 0xa8,
    // I32TruncUF32 = 0xa9,
    // I32TruncSF64 = 0xaa,
    // I32TruncUF64 = 0xab,
    // I64ExtendSI32 = 0xac,
    // I64ExtendUI32 = 0xad,
    // I64TruncSF32 = 0xae,
    // I64TruncUF32 = 0xaf,
    // I64TruncSF64 = 0xb0,
    // I64TruncUF64 = 0xb1,
    // F32ConvertSI32 = 0xb2,
    // F32ConvertUI32 = 0xb3,
    // F32ConvertSI64 = 0xb4,
    // F32ConvertUI64 = 0xb5,
    // F32DemoteF64 = 0xb6,
    // F64ConvertSI32 = 0xb7,
    // F64ConvertUI32 = 0xb8,
    // F64ConvertSI64 = 0xb9,
    // F64ConvertUI64 = 0xba,
    // F64PromoteF32 = 0xbb,

    // I32ReinterpretF32 = 0xbc,
    // I64ReinterpretF64 = 0xbd,
    // F32ReinterpretI32 = 0xbe,
    // F64ReinterpretI64 = 0xbf,
}

fn emit_call(externs: &Vec<&Func>, fns: &Vec<&Func>, globals: &Vec<Var>, locals: &Vec<Var>, mut buf: &mut Vec<u8>, func: &String, args: &Vec<Expression>) -> Result<TypeSpec, CompileError> {
    let mut arg_types = vec![];
    for arg in args {
        arg_types.push(emit_exp(externs, fns, globals, locals, buf, arg)?);
    }
    // @TODO: Should check user functions before builtins.
    match (func.as_str(), &arg_types.as_slice()) {
        // @NOTE: Direct memory stuff doesn't really work as fake functions. You can't really calculate functions, their memory locations
        // are immediates and have to be known up front.
        // Not gonna expose this stuff raw and instead going to wait untill I have arrays, structs, enums and arenas and expose those.
        /*
        ("current_memory", []) => { write_op(WasmOperator::CurrentMemory, &mut buf); write_size(0, &mut buf); return Ok(TypeSpec::I32) },
        ("grow_memory", [TypeSpec::I32]) => { write_op(WasmOperator::GrowMemory, &mut buf); write_size(0, &mut buf); return Ok(TypeSpec::I32) },
        //value, pointer
        // @NOTE: this is sort of a hack, we already emitted the 
        ("i32_store", [TypeSpec::I32, TypeSpec::I32]) => {},
        ("i32_load", [TypeSpec::I32]) => {}
        */


        ("add", [TypeSpec::I32, TypeSpec::I32]) => { write_op(WasmOperator::I32Add, &mut buf); return Ok(TypeSpec::I32) },
        _ => {
            if let Some(index) = fns.iter().position(|f| &f.name == func) {
                // @TODO: Check types!, there could be multiple of every function name!
                write_op(WasmOperator::Call, &mut buf);
                write_size(index+externs.len(), &mut buf);
                return Ok(fns[index].return_type);
            } else if let Some(index) = externs.iter().position(|f| &f.name == func) {
                write_op(WasmOperator::Call, &mut buf);
                write_size(index, &mut buf);
                return Ok(externs[index].return_type)
            } else {
                return Err(CompileError::UnknownFunction{func: func.clone(), arg_types: arg_types.clone()})
            }
        }
    }
}

fn emit_exp(externs: &Vec<&Func>, fns: &Vec<&Func>, globals: &Vec<Var>, locals: &Vec<Var>, mut buf: &mut Vec<u8>, exp: &Expression) -> Result<TypeSpec, CompileError> {
    // @TODO: Don't return stuff, just have the value be the result of the match arm.
    match exp {
        // @TODO: This is writing everything as unsigned which is wrong!
        Expression::I32(i) => { write_op(WasmOperator::I32Const, &mut buf); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::I32)},
        Expression::U32(i) => { write_op(WasmOperator::I32Const, &mut buf); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::U32)},
        Expression::I64(i) => { write_op(WasmOperator::I64Const, &mut buf); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::I64)},
        Expression::U64(i) => { write_op(WasmOperator::I64Const, &mut buf); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::U64)},
        Expression::F32(f) => { write_op(WasmOperator::F32Const, &mut buf); buf.write_f32::<LittleEndian>(*f as f32).expect("oh no"); return Ok(TypeSpec::F32)},
        Expression::F64(f) => { write_op(WasmOperator::F64Const, &mut buf); buf.write_f64::<LittleEndian>(*f as f64).expect("oh no"); return Ok(TypeSpec::F64)},
        Expression::Unary {op, arg} => {
            let argument = emit_exp(externs, fns, globals, locals, buf, arg)?;
            match (op, argument) {
                (Token::NOT, TypeSpec::I32) => { write_op(WasmOperator::I32Eqz, &mut buf); return Ok(TypeSpec::I32) },
                (Token::NOT, TypeSpec::I64) => { write_op(WasmOperator::I64Eqz, &mut buf); return Ok(TypeSpec::I32) },
                (Token::SUB, TypeSpec::I32) => {
                    write_op(WasmOperator::I32Const, &mut buf);
                    write_u64(0, &mut buf);
                    write_op(WasmOperator::I32Sub, &mut buf);
                    return Ok(TypeSpec::I32);
                },
                (Token::SUB, TypeSpec::I64) => {
                    write_op(WasmOperator::I64Const, &mut buf);
                    write_u64(0, &mut buf);
                    write_op(WasmOperator::I64Sub, &mut buf);
                    return Ok(TypeSpec::I64);
                },
                (Token::SUB, TypeSpec::F32) => {
                    write_op(WasmOperator::F32Const, &mut buf);
                    write_u64(0, &mut buf);
                    write_op(WasmOperator::F32Sub, &mut buf);
                    return Ok(TypeSpec::F32);
                },
                (Token::SUB, TypeSpec::F64) => {
                    write_op(WasmOperator::F64Const, &mut buf);
                    write_u64(0, &mut buf);
                    write_op(WasmOperator::F64Sub, &mut buf);
                    return Ok(TypeSpec::F64);
                },
                _ => { return Err(CompileError::UnknownOperator{ op: op.clone(), lhs: argument, rhs: TypeSpec::NULL }) }
            }
        },
        Expression::Binary {op, left, right} => {
            let lhs = emit_exp(externs, fns, globals, locals, buf, left)?;
            let rhs = emit_exp(externs, fns, globals, locals, buf, right)?;
            match (op, lhs, rhs) {
                (Token::EQUALTO, TypeSpec::I32, TypeSpec::I32)  => { write_op(WasmOperator::I32Eq, &mut buf); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32Ne, &mut buf); return Ok(TypeSpec::I32) },
                (Token::EQUALTO, TypeSpec::U32, TypeSpec::U32)  => { write_op(WasmOperator::I32Eq, &mut buf); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32Ne, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32LtS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32LtU, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32GtS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32GtU, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32LeS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32LeU, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32GeS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32GeU, &mut buf); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::I64, TypeSpec::I64)  => { write_op(WasmOperator::I64Eq, &mut buf); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64Ne, &mut buf); return Ok(TypeSpec::I32) },
                (Token::EQUALTO, TypeSpec::U64, TypeSpec::U64)  => { write_op(WasmOperator::I64Eq, &mut buf); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64Ne, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64LtS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64LtU, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64GtS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64GtU, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64LeS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64LeU, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64GeS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64GeU, &mut buf); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::F32, TypeSpec::F32)  => { write_op(WasmOperator::F32Eq, &mut buf); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Ne, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Lt, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Gt, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Le, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Ge, &mut buf); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::F64, TypeSpec::F64)  => { write_op(WasmOperator::F64Eq, &mut buf); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Ne, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Lt, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Gt, &mut buf); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Le, &mut buf); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Ge, &mut buf); return Ok(TypeSpec::I32) },

                (Token::ADD, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32Add, &mut buf); return Ok(TypeSpec::I32) },
                (Token::SUB, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32Sub, &mut buf); return Ok(TypeSpec::I32) },
                (Token::MUL, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32Mul, &mut buf); return Ok(TypeSpec::I32) },
                (Token::DIV, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32DivS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::REM, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32RemS, &mut buf); return Ok(TypeSpec::I32) },
                (Token::AND, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32And, &mut buf); return Ok(TypeSpec::I32) },
                (Token::OR,  TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32Or, &mut buf); return Ok(TypeSpec::I32) },
                (Token::XOR, TypeSpec::I32, TypeSpec::I32) => { write_op(WasmOperator::I32Xor, &mut buf); return Ok(TypeSpec::I32) },
                (Token::ADD, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32Add, &mut buf); return Ok(TypeSpec::U32) },
                (Token::SUB, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32Sub, &mut buf); return Ok(TypeSpec::U32) },
                (Token::MUL, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32Mul, &mut buf); return Ok(TypeSpec::U32) },
                (Token::DIV, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32DivU, &mut buf); return Ok(TypeSpec::U32) },
                (Token::REM, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32RemU, &mut buf); return Ok(TypeSpec::U32) },
                (Token::AND, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32And, &mut buf); return Ok(TypeSpec::U32) },
                (Token::OR,  TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32Or, &mut buf); return Ok(TypeSpec::U32) },
                (Token::XOR, TypeSpec::U32, TypeSpec::U32) => { write_op(WasmOperator::I32Xor, &mut buf); return Ok(TypeSpec::U32) },

                (Token::ADD, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64Add, &mut buf);  return Ok(TypeSpec::I64) },
                (Token::SUB, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64Sub, &mut buf);  return Ok(TypeSpec::I64) },
                (Token::MUL, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64Mul, &mut buf);  return Ok(TypeSpec::I64) },
                (Token::DIV, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64DivS, &mut buf); return Ok(TypeSpec::I64) },
                (Token::REM, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64RemS, &mut buf); return Ok(TypeSpec::I64) },
                (Token::AND, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64And, &mut buf);  return Ok(TypeSpec::I64) },
                (Token::OR,  TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64Or, &mut buf);   return Ok(TypeSpec::I64) },
                (Token::XOR, TypeSpec::I64, TypeSpec::I64) => { write_op(WasmOperator::I64Xor, &mut buf);  return Ok(TypeSpec::I64) },
                (Token::ADD, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64Add, &mut buf);  return Ok(TypeSpec::U64) },
                (Token::SUB, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64Sub, &mut buf);  return Ok(TypeSpec::U64) },
                (Token::MUL, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64Mul, &mut buf);  return Ok(TypeSpec::U64) },
                (Token::DIV, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64DivU, &mut buf); return Ok(TypeSpec::U64) },
                (Token::REM, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64RemU, &mut buf); return Ok(TypeSpec::U64) },
                (Token::AND, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64And, &mut buf);  return Ok(TypeSpec::U64) },
                (Token::OR,  TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64Or, &mut buf);   return Ok(TypeSpec::U64) },
                (Token::XOR, TypeSpec::U64, TypeSpec::U64) => { write_op(WasmOperator::I64Xor, &mut buf);  return Ok(TypeSpec::U64) },

                (Token::ADD, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Add, &mut buf);  return Ok(TypeSpec::F32) },
                (Token::SUB, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Sub, &mut buf);  return Ok(TypeSpec::F32) },
                (Token::MUL, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Mul, &mut buf);  return Ok(TypeSpec::F32) },
                (Token::DIV, TypeSpec::F32, TypeSpec::F32) => { write_op(WasmOperator::F32Div, &mut buf);  return Ok(TypeSpec::F32) },  

                (Token::ADD, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Add, &mut buf);  return Ok(TypeSpec::F64) },
                (Token::SUB, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Sub, &mut buf);  return Ok(TypeSpec::F64) },
                (Token::MUL, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Mul, &mut buf);  return Ok(TypeSpec::F64) },
                (Token::DIV, TypeSpec::F64, TypeSpec::F64) => { write_op(WasmOperator::F64Div, &mut buf);  return Ok(TypeSpec::F64) },

                // @TODO: Tokens and expressions for shifts and rotates.
                _ => { return Err(CompileError::UnknownOperator{ op: op.clone(), lhs, rhs }) }
            }
        },
        Expression::Name(n) => {
            if let Some(index) = locals.iter().position(|var| &var.name == n) {
                write_op(WasmOperator::GetLocal, &mut buf);
                write_size(index, &mut buf);
                return Ok(locals[index].typespec);
            } else if let Some(index) = globals.iter().position(|var| &var.name == n) {
                write_op(WasmOperator::GetGlobal, &mut buf);
                write_size(index, &mut buf);
                return Ok(globals[index].typespec);
            } else {
                return Err(CompileError::UnknownVariable{name: n.clone()})
            }
        },
        Expression::Call {func, args} => {
            emit_call(externs, fns, globals, locals, &mut buf, &func, args)
        }
    }
}

fn emit_statement(externs: &Vec<&Func>, fns: &Vec<&Func>, globals: &Vec<Var>, locals: &Vec<Var>, mut buf: &mut Vec<u8>, statement: &Statement) -> Result<(), CompileError> {
    match statement {
        Statement::ASSIGN {name, expression} => {
            // @NOTE: There are only two scopes right now. Globals and function locals. No lexical scoping.
            let exp_type = emit_exp(externs, fns, globals, locals, &mut buf, expression)?;
            if let Some(index) = locals.iter().position(|v| &v.name == name) {
                if locals[index].typespec == exp_type {
                    write_op(WasmOperator::SetLocal, &mut buf);
                    write_size(index, &mut buf);
                    Ok(())
                } else {
                    Err(CompileError::TypeMismatch{expected: locals[index].typespec, got: exp_type})
                }
            } else if let Some(index) = globals.iter().position(|v| &v.name == name) {
                if globals[index].typespec == exp_type {
                    write_op(WasmOperator::SetGlobal, &mut buf);
                    write_size(index, &mut buf);
                    Ok(())
                } else {
                    Err(CompileError::TypeMismatch{expected: globals[index].typespec, got: exp_type})
                }
            } else {
                Err(CompileError::UnknownVariable{name: name.clone()})
            }
        },
        Statement::IF { condition, then_block, else_block } => {
            let exp_type = emit_exp(externs, fns, globals, locals, &mut buf, condition)?;
            if exp_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: exp_type})
            }
            write_op(WasmOperator::If, &mut buf);
            write_type(WasmType::EmptyBlock, &mut buf);
            for stmt in then_block {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            if else_block.len() > 0 {
                write_op(WasmOperator::Else, &mut buf);
                for stmt in else_block {
                    emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
                }
            }
            write_op(WasmOperator::End, &mut buf);
            Ok(())
        },
        Statement::WHILE { condition, block } => {
            write_op(WasmOperator::Block, &mut buf);
            write_type(WasmType::EmptyBlock, &mut buf);
            
            write_op(WasmOperator::Loop, &mut buf);
            write_type(WasmType::EmptyBlock, &mut buf);
            let exp_type = emit_exp(externs, fns, globals, locals, &mut buf, condition)?;
            if exp_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: exp_type})
            }
            write_op(WasmOperator::I32Eqz, &mut buf);
            write_op(WasmOperator::BrIf, &mut buf);
            write_size(1, &mut buf);
            for stmt in block {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            write_op(WasmOperator::Br, &mut buf);
            write_size(0, &mut buf);
            write_op(WasmOperator::End, &mut buf);
            write_op(WasmOperator::End, &mut buf);
            Ok(())
        },
        Statement::FOR { setup, condition, iter, block } => {
            emit_statement(&externs, &fns, &globals, &locals, &mut buf, setup)?;
            write_op(WasmOperator::Block, &mut buf);
            write_type(WasmType::EmptyBlock, &mut buf);
            write_op(WasmOperator::Loop, &mut buf);
            write_type(WasmType::EmptyBlock, &mut buf);
            let exp_type = emit_exp(externs, fns, globals, locals, &mut buf, condition)?;
            if exp_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: exp_type})
            }
            write_op(WasmOperator::I32Eqz, &mut buf);
            write_op(WasmOperator::BrIf, &mut buf);
            write_size(1, &mut buf);
            for stmt in block {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            emit_statement(&externs, &fns, &globals, &locals, &mut buf, iter)?;
            write_op(WasmOperator::Br, &mut buf);
            write_size(0, &mut buf);
            write_op(WasmOperator::End, &mut buf);
            write_op(WasmOperator::End, &mut buf);
            Ok(())
        },
        Statement::SWITCH { index, values, blocks, default } => {
            // @TODO: Implement versions that uses if-else chains.
            // Figure out when to use which implementation.

            // @TODO: subtract min index from all indexes so you don't
            // padd with default 0's
            let mut min_index = std::i32::MAX;
            let mut max_index = 0;
            for v in values {
                if *v < min_index {
                    min_index = *v;
                }
                if *v > max_index {
                    max_index = *v;
                }
            }
            // Ok so gotta match up block indexes to case statements.
            //emit blocks in blocks
            // the case values for those are in values
            // for 0 - max value
            // build array of indexes, where default is index 0
            // case 2 foo
            // case 4 foooo
            // default f
            // br_Table 5 [f, f, 2, f, 4] f
            // then I also have to match these up to the blocks.
            let mut index_array = vec![0; (max_index+1) as usize];
            // walk down to the default.
            //let mut num_blocks = 0;

            // outer block
            write_op(WasmOperator::Block, &mut buf);
            write_type(WasmType::EmptyBlock, &mut buf);

            for i in (0..values.len()).rev() {
                let val = values[i] as usize;
                index_array[val] = i+1;
                write_op(WasmOperator::Block, &mut buf);
                write_type(WasmType::EmptyBlock, &mut buf);
            }
            
            write_op(WasmOperator::Block, &mut buf);
            write_type(WasmType::EmptyBlock, &mut buf);
            let index_type = emit_exp(externs, fns, globals, locals, &mut buf, index)?;
            if index_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: index_type})
            }
            write_op(WasmOperator::BrTable, &mut buf);
            write_size((max_index+1) as usize, &mut buf);
            for v in index_array {
                write_size(v, &mut buf);
            }
            write_size(0, &mut buf); // default
            write_op(WasmOperator::End, &mut buf);

            for stmt in default {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            let mut break_level = values.len();
            for i in 0..values.len() {
                write_op(WasmOperator::Br, &mut buf);
                write_size(break_level, &mut buf);
                break_level -= 1;
                write_op(WasmOperator::End, &mut buf);
                for stmt in &blocks[i] {
                   emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
                }
            }

            write_op(WasmOperator::End, &mut buf);
            Ok(())
        },
        Statement::CALL {func, args} => {
            match emit_call(externs, fns, globals, locals, &mut buf, &func, args) {
                Ok(_) => Ok(()),
                Err(e) => Err(e)
            }
        },
        Statement::RETURN(expression) => {
            emit_exp(externs, fns, globals, locals, &mut buf, expression)?;
            write_op(WasmOperator::Return, &mut buf);
            Ok(())
        },
        _ => Ok(())// @TODO
    }
}

pub fn emit_module(module: Module) -> Result<Vec<u8>, CompileError> {
    // @TODO: Maybe change the module format to fit this since it's the first thing we do.
    let mut globals = vec![];
    let mut externs = vec![];
    let mut funcs = vec![];
    for decl in &module.declarations {
        match decl {
            Declaration::GLOBAL {var} => {
                globals.push(var.clone());
            },
            Declaration::EXTERN {func} => {
                externs.push(func);
            },
            Declaration::FUNC {func} => {
                funcs.push(func);
            },
            _ => {}
        }
    }

    let mut type_section = vec![];
    write_size(externs.len() + funcs.len(), &mut type_section);
    for func in (&externs).iter().chain((&funcs).iter()) {
        write_type(WasmType::Func, &mut type_section);
        write_size(func.params.len(), &mut type_section);
        let param_types = func.params.iter().map(|p| p.var.typespec).collect::<Vec<TypeSpec>>();
        for t in param_types {
            match t {
                TypeSpec::I32 => write_type(WasmType::I32, &mut type_section),
                TypeSpec::U32 => write_type(WasmType::I32, &mut type_section),
                TypeSpec::I64 => write_type(WasmType::I64, &mut type_section),
                TypeSpec::U64 => write_type(WasmType::I64, &mut type_section),
                TypeSpec::F32 => write_type(WasmType::F32, &mut type_section),
                TypeSpec::F64 => write_type(WasmType::F64, &mut type_section),
                _ => () // @TODO: Throw  error on null
            }
        }
        match func.return_type {
            TypeSpec::NULL => { write_u64(0, &mut type_section); }
            TypeSpec::I32 =>  { write_u64(1, &mut type_section); write_type(WasmType::I32, &mut type_section) },
            TypeSpec::U32 =>  { write_u64(1, &mut type_section); write_type(WasmType::I32, &mut type_section) },
            TypeSpec::I64 =>  { write_u64(1, &mut type_section); write_type(WasmType::I64, &mut type_section) },
            TypeSpec::U64 =>  { write_u64(1, &mut type_section); write_type(WasmType::I64, &mut type_section) },
            TypeSpec::F32 =>  { write_u64(1, &mut type_section); write_type(WasmType::F32, &mut type_section) },
            TypeSpec::F64 =>  { write_u64(1, &mut type_section); write_type(WasmType::F64, &mut type_section) }
        }
    }

    let mut import_section = vec![];
    write_size(externs.len(), &mut import_section);
    for (i, func) in externs.iter().enumerate() {
        write_size(3, &mut import_section);
        write_bytes(b"env", &mut import_section);
        write_size(func.name.len(), &mut import_section);
        write_bytes(func.name.as_bytes(), &mut import_section);
        write_extern_kind(WasmExternalKind::Function, &mut import_section);
        write_size(i, &mut import_section);
    }

    let mut function_section = vec![];
    write_size(funcs.len(), &mut function_section);
    for i in 0..funcs.len() {
        write_size(externs.len()+i, &mut function_section);
    }

    // @TODO: Look into table section again, do I need a thing per import? Probably.
    let mut table_section = vec![];
    write_size(1, &mut table_section);
    write_type(WasmType::AnyFunc, &mut table_section);
    write_size(0, &mut table_section);
    write_size(externs.len(), &mut table_section);

    let mut memory_section = vec![];
    write_size(1, &mut memory_section);
    write_size(0, &mut memory_section);
    write_size(0, &mut memory_section);

    let mut global_section = vec![];
    write_size(globals.len(), &mut global_section);
    for global in &globals {
        match global.typespec {
            TypeSpec::I32 => {
                write_type(WasmType::I32, &mut global_section);
                write_u64(1, &mut global_section);
                write_op(WasmOperator::I32Const, &mut global_section);
                write_u64(0, &mut global_section);
                write_op(WasmOperator::End, &mut global_section);
            },
            TypeSpec::U32 => {
                write_type(WasmType::I32, &mut global_section);
                write_u64(1, &mut global_section);
                write_op(WasmOperator::I32Const, &mut global_section);
                write_u64(0, &mut global_section);
                write_op(WasmOperator::End, &mut global_section);
            },
            TypeSpec::I64 => {
                write_type(WasmType::I64, &mut global_section);
                write_u64(1, &mut global_section);
                write_op(WasmOperator::I64Const, &mut global_section);
                write_u64(0, &mut global_section);
                write_op(WasmOperator::End, &mut global_section);
            },
            TypeSpec::U64 => {
                write_type(WasmType::I64, &mut global_section);
                write_u64(1, &mut global_section);
                write_op(WasmOperator::I64Const, &mut global_section);
                write_u64(0, &mut global_section);
                write_op(WasmOperator::End, &mut global_section);
            },
            TypeSpec::F32 => {
                write_type(WasmType::F32, &mut global_section);
                write_u64(1, &mut global_section);
                write_op(WasmOperator::F32Const, &mut global_section);
                global_section.write_f32::<LittleEndian>(0.0).unwrap();
                write_op(WasmOperator::End, &mut global_section);
            },
            TypeSpec::F64 => {
                write_type(WasmType::F64, &mut global_section);
                write_u64(1, &mut global_section);
                write_op(WasmOperator::F64Const, &mut global_section);
                global_section.write_f64::<LittleEndian>(0.0).unwrap();
                write_op(WasmOperator::End, &mut global_section);
            },
            _ => () // @TODO: Throw  error on null
        }
    }

    let mut export_section = vec![];
    write_size(funcs.iter().filter(|f| f.kind == FuncKind::Export).count(), &mut export_section);
    for (i, func) in funcs.iter().enumerate() {
        if func.kind == FuncKind::Export {
            write_size(func.name.len(), &mut export_section);
            write_bytes(func.name.as_bytes(), &mut export_section);
            write_extern_kind(WasmExternalKind::Function, &mut export_section);
            write_size(i+externs.len(), &mut export_section); // which function
        }
    }

    let mut code_section = vec![];
    write_size(funcs.len(), &mut code_section);

    for func in &funcs {
        let mut locals = vec![];
        for param in &func.params {
            locals.push(param.var.clone());
        }

        for stmt in &func.block {
            if let Statement::LOCAL{ var } = stmt {
                locals.push(var.clone());
            }
        }

        let mut func_body = vec![];
        write_size(locals.len() - func.params.len(), &mut func_body); // num locals

        // @OPTIMIZE: These can be specified in blocks of same type vars. Useful for compression
        // and for arrays on the stack.
        for stmt in &func.block {
            if let Statement::LOCAL { var } = stmt {
                write_u64(1, &mut func_body);
                match var.typespec {
                    TypeSpec::I32 => write_type(WasmType::I32, &mut func_body),
                    TypeSpec::U32 => write_type(WasmType::I32, &mut func_body),
                    TypeSpec::I64 => write_type(WasmType::I64, &mut func_body),
                    TypeSpec::U64 => write_type(WasmType::I64, &mut func_body),
                    TypeSpec::F32 => write_type(WasmType::F32, &mut func_body),
                    TypeSpec::F64 => write_type(WasmType::F64, &mut func_body),
                    _ => () // @TODO: Throw  error on null
                }
            }
        }

        // emit the code
        for stmt in &func.block {
            emit_statement(&externs, &funcs, &globals, &locals, &mut func_body, &stmt)?;
        }

        // end
        write_op(WasmOperator::End, &mut func_body);
        write_size(func_body.len(), &mut code_section);
        write_bytes(&func_body, &mut code_section);
    
    }

    // @TODO: Data section is where we load the inital memory.

    // @TODO: The name section can be used so all the vars have names when decoded for debugging.

    let mut buffer = vec![];
    write_bytes(&[0,97,115,109], &mut buffer); // Wasm Magic Number
    write_bytes(&[1,0,0,0], &mut buffer);      // Version 1.0

    write_section_code(WasmSectionCode::Type, &mut buffer);
    write_size(type_section.len(), &mut buffer);
    write_bytes(&type_section, &mut buffer);

    write_section_code(WasmSectionCode::Import, &mut buffer);
    write_size(import_section.len(), &mut buffer);
    write_bytes(&import_section, &mut buffer);

    write_section_code(WasmSectionCode::Function, &mut buffer);
    write_size(function_section.len(), &mut buffer);
    write_bytes(&function_section, &mut buffer);

    write_section_code(WasmSectionCode::Table, &mut buffer);
    write_size(table_section.len(), &mut buffer);
    write_bytes(&table_section, &mut buffer);

    write_section_code(WasmSectionCode::Memory, &mut buffer);
    write_size(memory_section.len(), &mut buffer);
    write_bytes(&memory_section, &mut buffer);

    write_section_code(WasmSectionCode::Global, &mut buffer);
    write_size(global_section.len(), &mut buffer);
    write_bytes(&global_section, &mut buffer);

    write_section_code(WasmSectionCode::Export, &mut buffer);
    write_size(export_section.len(), &mut buffer);
    write_bytes(&export_section, &mut buffer);

    write_section_code(WasmSectionCode::Code, &mut buffer);
    write_size(code_section.len(), &mut buffer);
    write_bytes(&code_section, &mut buffer);

    Ok(buffer)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_leb128() {
        let mut a = vec![];
        write_u64(12345678, &mut a);
        assert_eq!(a, [206, 194, 241, 5]);
    }
}
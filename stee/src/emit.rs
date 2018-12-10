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

fn write_size(mut val: usize, mut buf: &mut Vec<u8>) {
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
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11
}

enum WasmExternalKind {
    Function = 0,
    Table = 1,
    Memory = 2,
    Global = 4
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
    // Control Operators
    Unreachable = 0x00,
    Nop = 0x01,
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
    CallIndirect = 0x11,

    drop = 0x1a,
    select = 0x1b,

    GetLocal = 0x20,
    SetLocal = 0x21,
    tee_local = 0x22,
    GetGlobal = 0x23,
    SetGlobal = 0x24,

    i32_load = 0x28,
    i64_load = 0x29,
    f32_load = 0x2a,
    f64_load = 0x2b,
    i32_load8_s = 0x2c,
    i32_load8_u = 0x2d,
    i32_load16_s = 0x2e,
    i32_load16_u = 0x2f,
    i64_load8_s = 0x30,
    i64_load8_u = 0x31,
    i64_load16_s = 0x32,
    i64_load16_u = 0x33,
    i64_load32_s = 0x34,
    i64_load32_u = 0x35,
    i32_store = 0x36,
    i64_store = 0x37,
    f32_store = 0x38,
    f64_store = 0x39,
    i32_store8 = 0x3a,
    i32_store16 = 0x3b,
    i64_store8 = 0x3c,
    i64_store16 = 0x3d,
    i64_store32 = 0x3e,
    current_memory = 0x3f,
    grow_memory = 0x40,

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

    i32_clz = 0x67,
    i32_ctz = 0x68,
    i32_popcnt = 0x69,

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

    i32_shl = 0x74,
    i32_shr_s = 0x75,
    i32_shr_u = 0x76,
    i32_rotl = 0x77,
    i32_rotr = 0x78,

    i64_clz = 0x79,
    i64_ctz = 0x7a,
    i64_popcnt = 0x7b,

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

    i64_shl = 0x86,
    i64_shr_s = 0x87,
    i64_shr_u = 0x88,
    i64_rotl = 0x89,
    i64_rotr = 0x8a,

    f32_abs = 0x8b,
    f32_neg = 0x8c,
    f32_ceil = 0x8d,
    f32_floor = 0x8e,
    f32_trunc = 0x8f,
    f32_nearest = 0x90,
    f32_sqrt = 0x91,

    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,

    f32_min = 0x96,
    f32_max = 0x97,
    f32_copysign = 0x98,

    f64_abs = 0x99,
    f64_neg = 0x9a,
    f64_ceil = 0x9b,
    f64_floor = 0x9c,
    f64_trunc = 0x9d,
    f64_nearest = 0x9e,
    f64_sqrt = 0x9f,

    F64Add = 0xa0,
    F64Sub = 0xa1,
    F64Mul = 0xa2,
    F64Div = 0xa3,

    f64_min = 0xa4,
    f64_max = 0xa5,
    f64_copysign = 0xa6,

    i32_wrap_i64 = 0xa7,
    i32_trunc_s_f32 = 0xa8,
    i32_trunc_u_f32 = 0xa9,
    i32_trunc_s_f64 = 0xaa,
    i32_trunc_u_f64 = 0xab,
    i64_extend_s_i32 = 0xac,
    i64_extend_u_i32 = 0xad,
    i64_trunc_s_f32 = 0xae,
    i64_trunc_u_f32 = 0xaf,
    i64_trunc_s_f64 = 0xb0,
    i64_trunc_u_f64 = 0xb1,
    f32_convert_s_i32 = 0xb2,
    f32_convert_u_i32 = 0xb3,
    f32_convert_s_i64 = 0xb4,
    f32_convert_u_i64 = 0xb5,
    f32_demote_f64 = 0xb6,
    f64_convert_s_i32 = 0xb7,
    f64_convert_u_i32 = 0xb8,
    f64_convert_s_i64 = 0xb9,
    f64_convert_u_i64 = 0xba,
    f64_promote_f32 = 0xbb,

    i32_reinterpret_f32 = 0xbc,
    i64_reinterpret_f64 = 0xbd,
    f32_reinterpret_i32 = 0xbe,
    f64_reinterpret_i64 = 0xbf,
}

// enum WasmOperator {
//     // Control Operators
//     unreachable = 0x00,
//     nop = 0x01,
//     block = 0x02,
//     loop_ = 0x03,
//     if_ = 0x04,
//     else_ = 0x05,
//     end = 0x0b,
//     br = 0x0c,
//     br_if = 0x0d,
//     br_table = 0x0e,
//     return_ = 0x0f,
    
//     call = 0x10,
//     call_indirect = 0x11,

//     drop = 0x1a,
//     select = 0x1b,

//     get_local = 0x20,
//     set_local = 0x21,
//     tee_local = 0x22,
//     get_global = 0x23,
//     set_global = 0x24,

//     i32_load = 0x28,
//     i64_load = 0x29,
//     f32_load = 0x2a,
//     f64_load = 0x2b,
//     i32_load8_s = 0x2c,
//     i32_load8_u = 0x2d,
//     i32_load16_s = 0x2e,
//     i32_load16_u = 0x2f,
//     i64_load8_s = 0x30,
//     i64_load8_u = 0x31,
//     i64_load16_s = 0x32,
//     i64_load16_u = 0x33,
//     i64_load32_s = 0x34,
//     i64_load32_u = 0x35,
//     i32_store = 0x36,
//     i64_store = 0x37,
//     f32_store = 0x38,
//     f64_store = 0x39,
//     i32_store8 = 0x3a,
//     i32_store16 = 0x3b,
//     i64_store8 = 0x3c,
//     i64_store16 = 0x3d,
//     i64_store32 = 0x3e,
//     current_memory = 0x3f,
//     grow_memory = 0x40,

//     i32_const = 0x41,
//     i64_const = 0x42,
//     f32_const = 0x43,
//     f64_const = 0x44,

//     i32_eqz = 0x45,
//     i32_eq = 0x46,
//     i32_ne = 0x47,
//     i32_lt_s = 0x48,
//     i32_lt_u = 0x49,
//     i32_gt_s = 0x4a,
//     i32_gt_u = 0x4b,
//     i32_le_s = 0x4c,
//     i32_le_u = 0x4d,
//     i32_ge_s = 0x4e,
//     i32_ge_u = 0x4f,
//     i64_eqz = 0x50,
//     i64_eq = 0x51,
//     i64_ne = 0x52,
//     i64_lt_s = 0x53,
//     i64_lt_u = 0x54,
//     i64_gt_s = 0x55,
//     i64_gt_u = 0x56,
//     i64_le_s = 0x57,
//     i64_le_u = 0x58,
//     i64_ge_s = 0x59,
//     i64_ge_u = 0x5a,
//     f32_eq = 0x5b,
//     f32_ne = 0x5c,
//     f32_lt = 0x5d,
//     f32_gt = 0x5e,
//     f32_le = 0x5f,
//     f32_ge = 0x60,
//     f64_eq = 0x61,
//     f64_ne = 0x62,
//     f64_lt = 0x63,
//     f64_gt = 0x64,
//     f64_le = 0x65,
//     f64_ge = 0x66,

//     i32_clz = 0x67,
//     i32_ctz = 0x68,
//     i32_popcnt = 0x69,
//     i32_add = 0x6a,
//     i32_sub = 0x6b,
//     i32_mul = 0x6c,
//     i32_div_s = 0x6d,
//     i32_div_u = 0x6e,
//     i32_rem_s = 0x6f,
//     i32_rem_u = 0x70,
//     i32_and = 0x71,
//     i32_or = 0x72,
//     i32_xor = 0x73,
//     i32_shl = 0x74,
//     i32_shr_s = 0x75,
//     i32_shr_u = 0x76,
//     i32_rotl = 0x77,
//     i32_rotr = 0x78,
//     i64_clz = 0x79,
//     i64_ctz = 0x7a,
//     i64_popcnt = 0x7b,
//     i64_add = 0x7c,
//     i64_sub = 0x7d,
//     i64_mul = 0x7e,
//     i64_div_s = 0x7f,
//     i64_div_u = 0x80,
//     i64_rem_s = 0x81,
//     i64_rem_u = 0x82,
//     i64_and = 0x83,
//     i64_or = 0x84,
//     i64_xor = 0x85,
//     i64_shl = 0x86,
//     i64_shr_s = 0x87,
//     i64_shr_u = 0x88,
//     i64_rotl = 0x89,
//     i64_rotr = 0x8a,
//     f32_abs = 0x8b,
//     f32_neg = 0x8c,
//     f32_ceil = 0x8d,
//     f32_floor = 0x8e,
//     f32_trunc = 0x8f,
//     f32_nearest = 0x90,
//     f32_sqrt = 0x91,
//     f32_add = 0x92,
//     f32_sub = 0x93,
//     f32_mul = 0x94,
//     f32_div = 0x95,
//     f32_min = 0x96,
//     f32_max = 0x97,
//     f32_copysign = 0x98,
//     f64_abs = 0x99,
//     f64_neg = 0x9a,
//     f64_ceil = 0x9b,
//     f64_floor = 0x9c,
//     f64_trunc = 0x9d,
//     f64_nearest = 0x9e,
//     f64_sqrt = 0x9f,
//     f64_add = 0xa0,
//     f64_sub = 0xa1,
//     f64_mul = 0xa2,
//     f64_div = 0xa3,
//     f64_min = 0xa4,
//     f64_max = 0xa5,
//     f64_copysign = 0xa6,

//     i32_wrap_i64 = 0xa7,
//     i32_trunc_s_f32 = 0xa8,
//     i32_trunc_u_f32 = 0xa9,
//     i32_trunc_s_f64 = 0xaa,
//     i32_trunc_u_f64 = 0xab,
//     i64_extend_s_i32 = 0xac,
//     i64_extend_u_i32 = 0xad,
//     i64_trunc_s_f32 = 0xae,
//     i64_trunc_u_f32 = 0xaf,
//     i64_trunc_s_f64 = 0xb0,
//     i64_trunc_u_f64 = 0xb1,
//     f32_convert_s_i32 = 0xb2,
//     f32_convert_u_i32 = 0xb3,
//     f32_convert_s_i64 = 0xb4,
//     f32_convert_u_i64 = 0xb5,
//     f32_demote_f64 = 0xb6,
//     f64_convert_s_i32 = 0xb7,
//     f64_convert_u_i32 = 0xb8,
//     f64_convert_s_i64 = 0xb9,
//     f64_convert_u_i64 = 0xba,
//     f64_promote_f32 = 0xbb,

//     i32_reinterpret_f32 = 0xbc,
//     i64_reinterpret_f64 = 0xbd,
//     f32_reinterpret_i32 = 0xbe,
//     f64_reinterpret_i64 = 0xbf,
// }

fn emit_exp(externs: &Vec<&Func>, fns: &Vec<&Func>, globals: &Vec<Var>, locals: &Vec<Var>, mut buf: &mut Vec<u8>, exp: &Expression) -> Result<TypeSpec, CompileError> {
    // @TODO: Don't return stuff, just have the value be the result of the match arm.
    match exp {
        // @TODO: This is writing everything as unsigned which is wrong!
        Expression::I32(i) => { buf.write_u8(WasmOperator::I32Const as u8); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::I32)},
        Expression::U32(i) => { buf.write_u8(WasmOperator::I32Const as u8); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::U32)},
        Expression::I64(i) => { buf.write_u8(WasmOperator::I64Const as u8); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::I64)},
        Expression::U64(i) => { buf.write_u8(WasmOperator::I64Const as u8); write_u64(*i as u64, &mut buf); return Ok(TypeSpec::U64)},
        Expression::F32(f) => { buf.write_u8(WasmOperator::F32Const as u8); buf.write_f32::<LittleEndian>(*f as f32).expect("oh no"); return Ok(TypeSpec::F32)},
        Expression::F64(f) => { buf.write_u8(WasmOperator::F64Const as u8); buf.write_f64::<LittleEndian>(*f as f64).expect("oh no"); return Ok(TypeSpec::F64)},
        Expression::Unary {op, arg} => {
            let argument = emit_exp(externs, fns, globals, locals, buf, arg)?;
            match (op, argument) {
                (Token::NOT, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Eqz as u8); return Ok(TypeSpec::I32) },
                (Token::NOT, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Eqz as u8); return Ok(TypeSpec::I32) },
                (Token::SUB, TypeSpec::I32) => {
                    buf.write_u8(WasmOperator::I32Const as u8);
                    write_u64(0, &mut buf);
                    buf.write_u8(WasmOperator::I32Sub as u8);
                    return Ok(TypeSpec::I32);
                },
                (Token::SUB, TypeSpec::I64) => {
                    buf.write_u8(WasmOperator::I64Const as u8);
                    write_u64(0, &mut buf);
                    buf.write_u8(WasmOperator::I64Sub as u8);
                    return Ok(TypeSpec::I64);
                },
                (Token::SUB, TypeSpec::F32) => {
                    buf.write_u8(WasmOperator::F32Const as u8);
                    write_u64(0, &mut buf);
                    buf.write_u8(WasmOperator::F32Sub as u8);
                    return Ok(TypeSpec::F32);
                },
                (Token::SUB, TypeSpec::F64) => {
                    buf.write_u8(WasmOperator::F64Const as u8);
                    write_u64(0, &mut buf);
                    buf.write_u8(WasmOperator::F64Sub as u8);
                    return Ok(TypeSpec::F64);
                },
                _ => { return Err(CompileError::UnknownOperator{ op: op.clone(), lhs: argument, rhs: TypeSpec::NULL }) }
            }
        },
        Expression::Binary {op, left, right} => {
            let lhs = emit_exp(externs, fns, globals, locals, buf, left)?;
            let rhs = emit_exp(externs, fns, globals, locals, buf, right)?;
            match (op, lhs, rhs) {
                (Token::EQUALTO, TypeSpec::I32, TypeSpec::I32)  => { buf.write_u8(WasmOperator::I32Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Ne as u8); return Ok(TypeSpec::I32) },
                (Token::EQUALTO, TypeSpec::U32, TypeSpec::U32)  => { buf.write_u8(WasmOperator::I32Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Ne as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32LtS as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32LtU as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32GtS as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32GtU as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32LeS as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32LeU as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32GeS as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32GeU as u8); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::I64, TypeSpec::I64)  => { buf.write_u8(WasmOperator::I64Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Ne as u8); return Ok(TypeSpec::I32) },
                (Token::EQUALTO, TypeSpec::U64, TypeSpec::U64)  => { buf.write_u8(WasmOperator::I64Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Ne as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64LtS as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64LtU as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64GtS as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64GtU as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64LeS as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64LeU as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64GeS as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64GeU as u8); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::F32, TypeSpec::F32)  => { buf.write_u8(WasmOperator::F32Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Ne as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Lt as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Gt as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Le as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Ge as u8); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::F64, TypeSpec::F64)  => { buf.write_u8(WasmOperator::F64Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Ne as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Lt as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Gt as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Le as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Ge as u8); return Ok(TypeSpec::I32) },

                (Token::ADD, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Add as u8); return Ok(TypeSpec::I32) },
                (Token::SUB, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Sub as u8); return Ok(TypeSpec::I32) },
                (Token::MUL, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Mul as u8); return Ok(TypeSpec::I32) },
                (Token::DIV, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32DivS as u8); return Ok(TypeSpec::I32) },
                (Token::REM, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32RemS as u8); return Ok(TypeSpec::I32) },
                (Token::AND, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32And as u8); return Ok(TypeSpec::I32) },
                (Token::OR,  TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Or as u8); return Ok(TypeSpec::I32) },
                (Token::XOR, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Xor as u8); return Ok(TypeSpec::I32) },
                (Token::ADD, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Add as u8); return Ok(TypeSpec::U32) },
                (Token::SUB, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Sub as u8); return Ok(TypeSpec::U32) },
                (Token::MUL, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Mul as u8); return Ok(TypeSpec::U32) },
                (Token::DIV, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32DivU as u8); return Ok(TypeSpec::U32) },
                (Token::REM, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32RemU as u8); return Ok(TypeSpec::U32) },
                (Token::AND, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32And as u8); return Ok(TypeSpec::U32) },
                (Token::OR,  TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Or as u8); return Ok(TypeSpec::U32) },
                (Token::XOR, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Xor as u8); return Ok(TypeSpec::U32) },

                (Token::ADD, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Add as u8);  return Ok(TypeSpec::I64) },
                (Token::SUB, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Sub as u8);  return Ok(TypeSpec::I64) },
                (Token::MUL, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Mul as u8);  return Ok(TypeSpec::I64) },
                (Token::DIV, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64DivS as u8); return Ok(TypeSpec::I64) },
                (Token::REM, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64RemS as u8); return Ok(TypeSpec::I64) },
                (Token::AND, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64And as u8);  return Ok(TypeSpec::I64) },
                (Token::OR,  TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Or as u8);   return Ok(TypeSpec::I64) },
                (Token::XOR, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Xor as u8);  return Ok(TypeSpec::I64) },
                (Token::ADD, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Add as u8);  return Ok(TypeSpec::U64) },
                (Token::SUB, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Sub as u8);  return Ok(TypeSpec::U64) },
                (Token::MUL, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Mul as u8);  return Ok(TypeSpec::U64) },
                (Token::DIV, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64DivU as u8); return Ok(TypeSpec::U64) },
                (Token::REM, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64RemU as u8); return Ok(TypeSpec::U64) },
                (Token::AND, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64And as u8);  return Ok(TypeSpec::U64) },
                (Token::OR,  TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Or as u8);   return Ok(TypeSpec::U64) },
                (Token::XOR, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Xor as u8);  return Ok(TypeSpec::U64) },

                (Token::ADD, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Add as u8);  return Ok(TypeSpec::F32) },
                (Token::SUB, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Sub as u8);  return Ok(TypeSpec::F32) },
                (Token::MUL, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Mul as u8);  return Ok(TypeSpec::F32) },
                (Token::DIV, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Div as u8);  return Ok(TypeSpec::F32) },
                
                (Token::ADD, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Add as u8);  return Ok(TypeSpec::F64) },
                (Token::SUB, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Sub as u8);  return Ok(TypeSpec::F64) },
                (Token::MUL, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Mul as u8);  return Ok(TypeSpec::F64) },
                (Token::DIV, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Div as u8);  return Ok(TypeSpec::F64) },

                // @TODO: Tokens and expressions for shifts and rotates.
                _ => { return Err(CompileError::UnknownOperator{ op: op.clone(), lhs, rhs }) }
            }
        },
        Expression::Name(n) => {
            if let Some(index) = locals.iter().position(|var| &var.name == n) {
                buf.write_u8(WasmOperator::GetLocal as u8);
                write_size(index, &mut buf);
                return Ok(locals[index].typespec);
            } else if let Some(index) = globals.iter().position(|var| &var.name == n) {
                buf.write_u8(WasmOperator::GetGlobal as u8);
                write_size(index, &mut buf);
                return Ok(globals[index].typespec);
            } else {
                return Err(CompileError::UnknownVariable{name: n.clone()})
            }
        },
        Expression::Call {func, args} => {
            let mut arg_types = vec![];
            for arg in args {
                arg_types.push(emit_exp(externs, fns, globals, locals, buf, arg)?);
            }
            // @TODO: Should check user functions before builtins.
            match (func.as_str(), &arg_types.as_slice()) {
                // @TODO: Figure out how to match vectors here for the builtin functions!
                // test fake builtin function
                // @TODO: External functions that are passed in.
                ("add", [TypeSpec::I32, TypeSpec::I32]) => { buf.write_u8(WasmOperator::I32Add as u8); return Ok(TypeSpec::I32) },
                _ => {
                    // Check for user defined functions.
                    if let Some(index) = fns.iter().position(|f| &f.name == func) {
                        // @TODO: Check types!, there could be multiple of every function name!
                        buf.write_u8(WasmOperator::Call as u8);
                        write_size(index+externs.len(), &mut buf);
                        return Ok(fns[index].return_type);
                    } else if let Some(index) = externs.iter().position(|f| &f.name == func) {
                        buf.write_u8(WasmOperator::Call as u8);
                        write_size(index, &mut buf);
                        return Ok(externs[index].return_type)
                    } else {
                        return Err(CompileError::UnknownFunction{func: func.clone(), arg_types: arg_types.clone()})
                    }
                }
            }
        }
        _ => return Err(CompileError::NotImplemented)
    }
}

fn emit_statement(externs: &Vec<&Func>, fns: &Vec<&Func>, globals: &Vec<Var>, locals: &Vec<Var>, mut buf: &mut Vec<u8>, statement: &Statement) -> Result<(), CompileError> {
    match statement {
        Statement::ASSIGN {name, expression} => {
            // @NOTE: There are only two scopes right now. Globals and function locals. No lexical scoping.
            let exp_type = emit_exp(externs, fns, globals, locals, &mut buf, expression)?;
            if let Some(index) = locals.iter().position(|v| &v.name == name) {
                if locals[index].typespec == exp_type {
                    buf.write_u8(WasmOperator::SetLocal as u8);
                    write_size(index, &mut buf);
                    Ok(())
                } else {
                    Err(CompileError::TypeMismatch{expected: locals[index].typespec, got: exp_type})
                }
            } else if let Some(index) = globals.iter().position(|v| &v.name == name) {
                if globals[index].typespec == exp_type {
                    buf.write_u8(WasmOperator::SetGlobal as u8);
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
            buf.write_u8(WasmOperator::If as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);
            for stmt in then_block {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            if else_block.len() > 0 {
                buf.write_u8(WasmOperator::Else as u8);
                for stmt in else_block {
                    emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
                }
            }
            buf.write_u8(WasmOperator::End as u8);
            Ok(())
        },
        Statement::WHILE { condition, block } => {
            buf.write_u8(WasmOperator::Block as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);
            
            buf.write_u8(WasmOperator::Loop as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);
            let exp_type = emit_exp(externs, fns, globals, locals, &mut buf, condition)?;
            if exp_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: exp_type})
            }
            buf.write_u8(WasmOperator::I32Eqz as u8);
            buf.write_u8(WasmOperator::BrIf as u8);
            write_size(1, &mut buf);
            for stmt in block {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            buf.write_u8(WasmOperator::Br as u8);
            write_size(0, &mut buf);
            buf.write_u8(WasmOperator::End as u8);
            buf.write_u8(WasmOperator::End as u8);
            Ok(())
        },
        Statement::FOR { setup, condition, iter, block } => {
            emit_statement(&externs, &fns, &globals, &locals, &mut buf, setup)?;
            buf.write_u8(WasmOperator::Block as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);
            buf.write_u8(WasmOperator::Loop as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);
            let exp_type = emit_exp(externs, fns, globals, locals, &mut buf, condition)?;
            if exp_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: exp_type})
            }
            buf.write_u8(WasmOperator::I32Eqz as u8);
            buf.write_u8(WasmOperator::BrIf as u8);
            write_size(1, &mut buf);
            for stmt in block {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            emit_statement(&externs, &fns, &globals, &locals, &mut buf, iter)?;
            buf.write_u8(WasmOperator::Br as u8);
            write_size(0, &mut buf);
            buf.write_u8(WasmOperator::End as u8);
            buf.write_u8(WasmOperator::End as u8);
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
            buf.write_u8(WasmOperator::Block as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);

            for i in (0..values.len()).rev() {
                let val = values[i] as usize;
                index_array[val] = i+1;
                buf.write_u8(WasmOperator::Block as u8);
                buf.write_u8(WasmType::EmptyBlock as u8);
            }
            
            buf.write_u8(WasmOperator::Block as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);
            let index_type = emit_exp(externs, fns, globals, locals, &mut buf, index)?;
            if index_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: index_type})
            }
            buf.write_u8(WasmOperator::BrTable as u8);
            write_size((max_index+1) as usize, &mut buf);
            for v in index_array {
                write_size(v, &mut buf);
            }
            write_size(0, &mut buf); // default
            buf.write_u8(WasmOperator::End as u8);

            for stmt in default {
                emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
            }
            let mut break_level = values.len();
            for i in (0..values.len()) {
                buf.write_u8(WasmOperator::Br as u8);
                write_size(break_level, &mut buf);
                break_level -= 1;
                buf.write_u8(WasmOperator::End as u8);
                for stmt in &blocks[i] {
                   emit_statement(&externs, &fns, &globals, &locals, &mut buf, &stmt)?;
                }
            }

            buf.write_u8(WasmOperator::End as u8);
            Ok(())
        }
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
                global_section.write_f32::<LittleEndian>(0.0);
                write_op(WasmOperator::End, &mut global_section);
            },
            TypeSpec::F64 => {
                write_type(WasmType::F64, &mut global_section);
                write_u64(1, &mut global_section);
                write_op(WasmOperator::F64Const, &mut global_section);
                global_section.write_f64::<LittleEndian>(0.0);
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
        // @TODO: Get index for all the variable names (and also function calls)
        // @TODO: should be able to have this be all refs? maybe?
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
        func_body.write_u8(WasmOperator::End as u8);
        write_size(func_body.len(), &mut code_section);
        write_bytes(&func_body, &mut code_section);
    
    }

    // @TODO: Data section is where we load the inital memory.

    // @TODO: The name section can be used so all the vars have names when decoded for debugging.

    let mut buffer = vec![];
    write_bytes(&[0,97,115,109], &mut buffer);
    write_bytes(&[1,0,0,0], &mut buffer);

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
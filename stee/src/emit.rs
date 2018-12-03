use super::types::*;
use bytebuffer::Endian::LittleEndian; // @TODO: It should be pretty easy to drop this.
use bytebuffer::ByteBuffer; // @TODO: Drop this.

// how do I do type checking?
// so every expression would have a type? Is that right?
// and I'd need some type checking code that figures it out?


// So I need all the things I can call from wasm.
// I can call any of these with a function call.

// Control flow operators are done with language constructs.
// call and call_indirect are what is used for user defined functions and imports.
// parametric, drop and select I'm not sure what those are for.
// variable accesses are used for locals and globals, I should be able to tell which I'm using
// by their name.
// memory operators
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
    call_indirect = 0x11,

    drop = 0x1a,
    select = 0x1b,

    GetLocal = 0x20,
    SetLocal = 0x21,
    tee_local = 0x22,
    get_global = 0x23,
    set_global = 0x24,

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

// These all probably are gonna just be typed functions too? I think?
// Yeah, better do. The only other thing is that there are signed and unsigned versions
// of all of these ops.
// I have to make a decision about if I wanna have unsigned versions of stuff.
// I think, that I shall...

// yeah, these will be the types to start
// i32 u32 i64 u64 f32 f64
// then when I do structs and shit I'll figure it out.
// what about casting, how many ways are there to do it?

// extend vs trunk vs wrap?


// dang, I wish I didn't wait to start this till the whole day was over!
// gonna work this out, do all this stuff first, then go back and do syntax stuff for the loops.
// then do global vs local and shit.
// I'm starting to see the path forward! This is such a doable project!

// I also want to break out the project, might do that today.
// 1 crate is the pure compiler.
// the wasm node project is seperate.

const MAGIC_NUMBER: u32 = 0x6d736100;
const VERSION: u32 = 0x1;

fn write_leb128(mut val: u64, mut buf: &mut ByteBuffer) {
    const continuation_bit: u8 = 1 << 7;
    loop {
        let mut byte: u8 = (val & (std::u8::MAX as u64)) as u8 & !continuation_bit;
        val >>= 7;
        if val != 0 {
            byte |= continuation_bit;
        }
        buf.write_u8(byte);
        if val == 0 {
            break;
        }
    }
}

fn new_buffer() -> ByteBuffer {
    let mut buffer = ByteBuffer::new();
    buffer.set_endian(LittleEndian);
    buffer
}

fn emit_exp(fns: &Vec<&Func>, vars: &Vec<Var>, mut buf: &mut ByteBuffer, exp: &Expression) -> Result<TypeSpec, CompileError> {
    match exp {
        Expression::I32(i) => { buf.write_u8(WasmOperator::I32Const as u8); write_leb128(*i as u64, &mut buf); return Ok(TypeSpec::I32)},
        Expression::U32(i) => { buf.write_u8(WasmOperator::I32Const as u8); write_leb128(*i as u64, &mut buf); return Ok(TypeSpec::U32)},
        Expression::I64(i) => { buf.write_u8(WasmOperator::I64Const as u8); write_leb128(*i as u64, &mut buf); return Ok(TypeSpec::I64)},
        Expression::U64(i) => { buf.write_u8(WasmOperator::I64Const as u8); write_leb128(*i as u64, &mut buf); return Ok(TypeSpec::U64)},
        Expression::F32(f) => { buf.write_u8(WasmOperator::F32Const as u8); buf.write_f32(*f as f32); return Ok(TypeSpec::F32)},
        Expression::F64(f) => { buf.write_u8(WasmOperator::F64Const as u8); buf.write_f64(*f as f64); return Ok(TypeSpec::F64)},
        Expression::Unary {op, arg} => {
            let argument = emit_exp(fns, vars, buf, arg)?;
            match (op, argument) {
                (Token::NOT, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Eqz as u8); return Ok(TypeSpec::I32) },
                (Token::NOT, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Eqz as u8); return Ok(TypeSpec::I32) },
                (Token::SUB, TypeSpec::I32) => {
                    buf.write_u8(WasmOperator::I32Const as u8);
                    write_leb128(0, &mut buf);
                    buf.write_u8(WasmOperator::I32Sub as u8);
                    return Ok(TypeSpec::I32);
                },
                (Token::SUB, TypeSpec::I64) => {
                    buf.write_u8(WasmOperator::I64Const as u8);
                    write_leb128(0, &mut buf);
                    buf.write_u8(WasmOperator::I64Sub as u8);
                    return Ok(TypeSpec::I64);
                },
                (Token::SUB, TypeSpec::F32) => {
                    buf.write_u8(WasmOperator::F32Const as u8);
                    write_leb128(0, &mut buf);
                    buf.write_u8(WasmOperator::F32Sub as u8);
                    return Ok(TypeSpec::F32);
                },
                (Token::SUB, TypeSpec::F64) => {
                    buf.write_u8(WasmOperator::F64Const as u8);
                    write_leb128(0, &mut buf);
                    buf.write_u8(WasmOperator::F64Sub as u8);
                    return Ok(TypeSpec::F64);
                },
                _ => { return Err(CompileError::UnknownOperator{ op: op.clone(), lhs: argument, rhs: TypeSpec::NULL }) }
            }
        },
        Expression::Binary {op, left, right} => {
            let lhs = emit_exp(fns, vars, buf, left)?;
            let rhs = emit_exp(fns, vars, buf, right)?;
            match (op, lhs, rhs) {
                // @TODO: Add all the operators!
                // eq_z is for equal to zero, dunno if I want that somehow.
                // @TODO: Add operators
                // % REM
                // == EqualTo
                // != NEqualTo
                // < LT
                // > GT
                // <= LE
                // >= GE
                // && AND
                // || OR
                // ^ XOR
                (Token::EQUALTO, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Ne as u8); return Ok(TypeSpec::I32) },
                (Token::EQUALTO, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Ne as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32LtS as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32LtU as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32GtS as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32GtU as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32LeS as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32LeU as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32GeS as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32GeU as u8); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64Ne as u8); return Ok(TypeSpec::I32) },
                (Token::EQUALTO, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64Ne as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64LtS as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64LtU as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64GtS as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64GtU as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64LeS as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64LeU as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::I64, TypeSpec::I64) => { buf.write_u8(WasmOperator::I64GeS as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::U64, TypeSpec::U64) => { buf.write_u8(WasmOperator::I64GeU as u8); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Eq as u8); return Ok(TypeSpec::I32) },
                (Token::NEQUALTO, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Ne as u8); return Ok(TypeSpec::I32) },
                (Token::LT, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Lt as u8); return Ok(TypeSpec::I32) },
                (Token::GT, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Gt as u8); return Ok(TypeSpec::I32) },
                (Token::LE, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Le as u8); return Ok(TypeSpec::I32) },
                (Token::GE, TypeSpec::F32, TypeSpec::F32) => { buf.write_u8(WasmOperator::F32Ge as u8); return Ok(TypeSpec::I32) },

                (Token::EQUALTO, TypeSpec::F64, TypeSpec::F64) => { buf.write_u8(WasmOperator::F64Eq as u8); return Ok(TypeSpec::I32) },
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
                (Token::OR, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Or as u8); return Ok(TypeSpec::I32) },
                (Token::XOR, TypeSpec::I32, TypeSpec::I32) => { buf.write_u8(WasmOperator::I32Xor as u8); return Ok(TypeSpec::I32) },
                (Token::ADD, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Add as u8); return Ok(TypeSpec::U32) },
                (Token::SUB, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Sub as u8); return Ok(TypeSpec::U32) },
                (Token::MUL, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Mul as u8); return Ok(TypeSpec::U32) },
                (Token::DIV, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32DivU as u8); return Ok(TypeSpec::U32) },
                (Token::REM, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32RemU as u8); return Ok(TypeSpec::U32) },
                (Token::AND, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32And as u8); return Ok(TypeSpec::U32) },
                (Token::OR, TypeSpec::U32, TypeSpec::U32) => { buf.write_u8(WasmOperator::I32Or as u8); return Ok(TypeSpec::U32) },
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

                // @TODO: Negation is a unary operator, add support for unary operators.
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
            println!("DEBUG: {:?}, {}", vars, n);
            let index = vars.iter().position(|var| &var.name == n).unwrap(); // @TODO: This is only checking params and locals, not globals.
            println!("DEBUG: {}", index);
            buf.write_u8(WasmOperator::GetLocal as u8);
            write_leb128(index as u64, &mut buf);
            return Ok(vars[index].typespec);
        },
        Expression::Call{func, args} => {
            let mut arg_types = vec![];
            for arg in args {
                arg_types.push(emit_exp(fns, vars, buf, arg)?);
            }
            match (func.as_str(), &arg_types.as_slice()) {
                // @TODO: Figure out how to match vectors here for the builtin functions!
                // test fake builtin function
                // @TODO: External functions that are passed in.
                ("add", [TypeSpec::I32, TypeSpec::I32]) => { buf.write_u8(WasmOperator::I32Add as u8); return Ok(TypeSpec::I32) },
                _ => {
                    // Check for user defined functions.
                    if let Some(index) = fns.iter().position(|f| &f.name == func) {
                        // @TODO: Check types!, there will be multiple of every function name!
                        buf.write_u8(WasmOperator::Call as u8);
                        write_leb128(index as u64, &mut buf);
                        return Ok(fns[index].return_type);
                    } else {
                        return Err(CompileError::UnknownFunction{func: func.clone(), arg_types: arg_types.clone()})
                    }

                }
            }
        }
        _ => return Err(CompileError::NotImplemented)
    }
}

// Declarations have to be done up front so won't hit this?
fn emit_statement(fns: &Vec<&Func>, vars: &Vec<Var>, mut buf: &mut ByteBuffer, statement: &Statement) -> Result<(), CompileError> {
    match statement {
        Statement::ASSIGN {name, expression} => {
            // @TODO: There's like tee local and stuff, since I'm doing 0 optimization so far whatev.
            let exp_type = emit_exp(fns, vars, &mut buf, expression)?;
            // set local
            let index = vars.iter().position(|v| &v.name == name).unwrap(); // @TODO: Type check before here lol
            if vars[index].typespec == exp_type {
                buf.write_u8(WasmOperator::SetLocal as u8);
                write_leb128(index as u64, &mut buf);
                Ok(())
            } else {
                Err(CompileError::TypeMismatch{expected: vars[index].typespec, got: exp_type})
            }
        },
        Statement::IF { condition, then_block, else_block } => {
            let exp_type = emit_exp(fns, vars, &mut buf, condition)?;
            if exp_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: exp_type})
            }
            buf.write_u8(WasmOperator::If as u8);
            buf.write_u8(WasmType::EmptyBlock as u8);
            for stmt in then_block {
                emit_statement(&fns, &vars, &mut buf, &stmt)?;
            }
            if else_block.len() > 0 {
                buf.write_u8(WasmOperator::Else as u8);
                for stmt in else_block {
                    emit_statement(&fns, &vars, &mut buf, &stmt)?;
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
            let exp_type = emit_exp(fns, vars, &mut buf, condition)?;
            if exp_type != TypeSpec::I32 {
                return Err(CompileError::TypeMismatch{expected: TypeSpec::I32, got: exp_type})
            }
            buf.write_u8(WasmOperator::I32Eqz as u8);
            buf.write_u8(WasmOperator::BrIf as u8);
            write_leb128(1, &mut buf);
            for stmt in block {
                emit_statement(&fns, &vars, &mut buf, &stmt)?;
            }
            buf.write_u8(WasmOperator::Br as u8);
            write_leb128(0, &mut buf);
            buf.write_u8(WasmOperator::End as u8);
            buf.write_u8(WasmOperator::End as u8);
            Ok(())
        },
        Statement::RETURN(expression) => {
            emit_exp(fns, vars, &mut buf, expression)?;
            buf.write_u8(0x0f); // return
            Ok(())
        },
        _ => Ok(())// @TODO
    }
}

pub fn emit_module(module: Module) -> Result<ByteBuffer, CompileError> {
    let mut funcs = vec![];
    for decl in &module.declarations {
        if let Declaration::FUNC {func} = decl {
            funcs.push(func);
        }
    }

    let mut type_section = new_buffer();
    write_leb128(funcs.len() as u64, &mut type_section);
    for func in &funcs {
        type_section.write_u8(WasmType::Func as u8);
        write_leb128(func.params.len() as u64, &mut type_section);
        let param_types = func.params.iter().map(|p| p.var.typespec).collect::<Vec<TypeSpec>>();
        for t in param_types {
            match t {
                TypeSpec::I32 => type_section.write_u8(WasmType::I32 as u8),
                TypeSpec::U32 => type_section.write_u8(WasmType::I32 as u8),
                TypeSpec::I64 => type_section.write_u8(WasmType::I64 as u8),
                TypeSpec::U64 => type_section.write_u8(WasmType::I64 as u8),
                TypeSpec::F32 => type_section.write_u8(WasmType::F32 as u8),
                TypeSpec::F64 => type_section.write_u8(WasmType::F64 as u8),
                _ => () // @TODO: Throw  error on null
            }
        }
        match func.return_type {
            TypeSpec::NULL => { write_leb128(0, &mut type_section); }
            TypeSpec::I32 => { write_leb128(1, &mut type_section); type_section.write_u8(WasmType::I32 as u8) },
            TypeSpec::U32 => { write_leb128(1, &mut type_section); type_section.write_u8(WasmType::I32 as u8) },
            TypeSpec::I64 => { write_leb128(1, &mut type_section); type_section.write_u8(WasmType::I64 as u8) },
            TypeSpec::U64 => { write_leb128(1, &mut type_section); type_section.write_u8(WasmType::I64 as u8) },
            TypeSpec::F32 => { write_leb128(1, &mut type_section); type_section.write_u8(WasmType::F32 as u8) },
            TypeSpec::F64 => { write_leb128(1, &mut type_section); type_section.write_u8(WasmType::F64 as u8) }
        }
    }

    let mut function_section = new_buffer();
    write_leb128(funcs.len() as u64, &mut function_section);
    for i in 0..funcs.len() {
        write_leb128(i as u64, &mut function_section);
    }

    let mut export_section = new_buffer();
    write_leb128(funcs.len() as u64, &mut export_section);
    for (i, func) in funcs.iter().enumerate() {
        //println!("DEBUG: {:?} {:?} {:?}", name, name.len() as u32, name.as_bytes());
        write_leb128(func.name.len() as u64, &mut export_section);
        export_section.write_bytes(func.name.as_bytes());
        export_section.write_u8(0); // Function Type
        write_leb128(i as u64, &mut export_section); // which function
    }

    let mut code_section = new_buffer();
    write_leb128(funcs.len() as u64, &mut code_section);

    for func in &funcs {
        // @TODO: Get index for all the variable names (and also function calls)
        // @TODO: should be able to have this be all refs? maybe?
        let mut vars = vec![];
        for param in &func.params {
            vars.push(param.var.clone());
        }

        for stmt in &func.block {
            if let Statement::LOCAL{ var } = stmt {
                vars.push(var.clone());
            }
        }

        let mut func_body = new_buffer();
        write_leb128(vars.len() as u64 - func.params.len() as u64, &mut func_body); // num locals

        // @OPTIMIZE: These can be specified in blocks of same type vars. Useful for compression
        // and for arrays on the stack.
        for stmt in &func.block {
            if let Statement::LOCAL { var } = stmt {
                write_leb128(1, &mut func_body);
                match var.typespec {
                    TypeSpec::I32 => func_body.write_u8(WasmType::I32 as u8),
                    TypeSpec::U32 => func_body.write_u8(WasmType::I32 as u8),
                    TypeSpec::I64 => func_body.write_u8(WasmType::I64 as u8),
                    TypeSpec::U64 => func_body.write_u8(WasmType::I64 as u8),
                    TypeSpec::F32 => func_body.write_u8(WasmType::F32 as u8),
                    TypeSpec::F64 => func_body.write_u8(WasmType::F64 as u8),
                    _ => () // @TODO: Throw  error on null
                }
            }
        }

        // emit the code
        for stmt in &func.block {
            emit_statement(&funcs, &vars, &mut func_body, &stmt)?;
        }

        // end
        func_body.write_u8(WasmOperator::End as u8);
        write_leb128(func_body.len() as u64, &mut code_section);
        code_section.write_bytes(&func_body.to_bytes());
    
    }

    // @TODO: Data section is where we load the inital memory. Will be needed for the
    // application stack and type info.

    // @TODO: The name section can be used so all the vars have names when decoded.
    // Makes debugging way easier.

    let mut buffer = new_buffer();
    buffer.write_u32(MAGIC_NUMBER);
    buffer.write_u32(VERSION);

    let type_section_header = 1;
    buffer.write_u8(type_section_header);
    write_leb128(type_section.len() as u64, &mut buffer);
    buffer.write_bytes(&type_section.to_bytes());

    let function_section_header = 3;
    buffer.write_u8(function_section_header);
    write_leb128(function_section.len() as u64, &mut buffer);
    buffer.write_bytes(&function_section.to_bytes());

    let export_section_header = 7;
    buffer.write_u8(export_section_header);
    write_leb128(export_section.len() as u64, &mut buffer);
    buffer.write_bytes(&export_section.to_bytes());

    let code_section_header = 10;
    buffer.write_u8(code_section_header);
    write_leb128(code_section.len() as u64, &mut buffer);
    buffer.write_bytes(&code_section.to_bytes());

    Ok(buffer)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_leb128() {
        let mut a = ByteBuffer::new();
        write_leb128(0, &mut a);
        let bytes = a.to_bytes();
        println!("{:?}", bytes);
        //assert_eq!(bytes, [42]);
    }
}
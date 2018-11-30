#[macro_use] extern crate failure;
extern crate bytebuffer;

use std::str::Chars;
use std::iter::Peekable;
use bytebuffer::Endian::LittleEndian; // @TODO: It should be pretty easy to drop this.

// @TODO: Rationalize error messages.
#[derive(Debug, Fail)]
pub enum CompileError {
    #[fail(display = "invalid initial token character {}", c)]
    InvalidTokenCharacter {
        c: char,
    },
    #[fail(display = "Error parsing number: {}", num)]
    ErrorParsingNumber {
        num: String,
    },
    #[fail(display = "Invalid Token, expected: {:?} Got: {:?}", expected, got)]
    InvalidToken {
        expected: Token,
        got: Token
    },
    #[fail(display = "Invalid name, expected: {:?} Got: {:?}", expected, got)]
    InvalidTokenName {
        expected: Token,
        got: Token
    },
    #[fail(display = "Invalid token for expression {:?}", token)]
    InvalidExpressionToken {
        token: Token
    },
    #[fail(display = "Invalid initial token for declaration {:?}", token)]
    InvalidDeclarationToken {
        token: Token
    },
    #[fail(display = "Invalid initial token for statement {:?}", token)]
    InvalidStatementToken {
        token: Token
    },
    #[fail(display = "Invalid type {:?}", token)]
    InvalidType {
        token: Token
    }
}

// @TODO: Tokens for keywords so I don't have to name-compare them and can just match them directly.
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    EOF,
    INT(i64),
    FLOAT(f64),
    Name(String),
    ADD,
    SUB,
    MUL,
    DIV,
    LP,
    RP,
    LB,
    RB,
    COMMA,
    COLON,
    SEMICOLON,
    EQUALS,
}

fn get_next_token(src: &mut Peekable<Chars>) -> Result<Token, CompileError> {
    loop {
        match src.peek() {
            Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                src.next();
            },
            // @TODO: Skip comments.
            _ => break
        }
    }

    match src.peek().cloned() {
        None => Ok(Token::EOF),
        Some(c) => {
            match c {
                // Name
                'a'...'z' | 'A'...'Z' | '_' => {
                    let mut name = String::new();
                    name.push(c);
                    src.next();
                    loop {
                        if let Some(c) = src.peek().cloned() {
                            match c {
                                'a'...'z'|'A'...'Z'|'0'...'9'|'_'=> {
                                    name.push(c);
                                    src.next();
                                    continue;
                                }
                                _ => ()
                            }
                        }
                        break;
                    }
                    Ok(Token::Name(name))
                },
                // @TODO: Negative number parsing.
                '0'...'9' => {
                    // @TODO: Find ways to do this without allocating.
                    let mut num = String::new();
                    num.push(c);
                    src.next();
                    let mut is_float = false;
                    loop {
                        if let Some(c) = src.peek().cloned() {
                            match c {
                                '0'...'9' => {
                                    num.push(c);
                                    src.next();
                                    continue;
                                },
                                '.' | 'e' | 'E' | '-' => {
                                    num.push(c);
                                    src.next();
                                    is_float = true;
                                    continue;
                                },
                                _ => ()
                            }
                        }
                        break;
                    }
                    if is_float {
                        if let Ok(f) = num.parse::<f64>() {
                            Ok(Token::FLOAT(f))
                        } else {
                            Err(CompileError::ErrorParsingNumber{num})
                        }
                    } else {
                        if let Ok(i) = num.parse::<i64>() {
                            Ok(Token::INT(i))
                        } else {
                            Err(CompileError::ErrorParsingNumber{num})
                        }
                    }
                },
                '*'  => { src.next(); Ok(Token::MUL) },
                '/'  => { src.next(); Ok(Token::DIV) },
                '+'  => { src.next(); Ok(Token::ADD) },
                '-'  => { src.next(); Ok(Token::SUB) },
                '('  => { src.next(); Ok(Token::LP) },
                ')'  => { src.next(); Ok(Token::RP) },
                '{'  => { src.next(); Ok(Token::LB) },
                '}'  => { src.next(); Ok(Token::RB) },
                ','  => { src.next(); Ok(Token::COMMA) },
                ':'  => { src.next(); Ok(Token::COLON) },
                ';'  => { src.next(); Ok(Token::SEMICOLON) },
                '='  => { src.next(); Ok(Token::EQUALS) },
                '\0' => { src.next(); Ok(Token::EOF) },
                _ => Err(CompileError::InvalidTokenCharacter {c})
            }
        }
    }
}

pub struct Lexer<'a> {
    src: Peekable<Chars<'a>>,
    token: Token
}

impl <'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Result<Self, CompileError> {
        let mut src = s.chars().peekable();
        let token = get_next_token(&mut src)?;
        Ok(Lexer { src, token })
    }

    pub fn next_token(&mut self) -> Result<(), CompileError> {
        let token = get_next_token(&mut self.src)?;
        self.token = token;
        Ok(())
    }

    pub fn is_token(&mut self, t: Token) -> bool {
        t == self.token
    }

    pub fn match_token(&mut self, t: Token) -> Result<bool, CompileError> {
        if self.is_token(t) {
            self.next_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn expect_token(&mut self, t: Token) -> Result<(), CompileError> {
        if !self.match_token(t.clone())? {
            Err(CompileError::InvalidToken{expected: t, got: self.token.clone()})
        } else {
            Ok(())
        }
    }

    pub fn expect_a_name(&mut self) -> Result<String, CompileError> {
        if let Token::Name(n) = self.token.clone() {
            self.next_token()?;
            Ok(n)
        } else {
            Err(CompileError::InvalidToken {
                expected: Token::Name("".to_string()),
                got: self.token.clone()
            })
        }
    }

    pub fn is_name(&mut self, string: &str) -> bool {
        if let Token::Name(ref s) = self.token {
            s == string
        } else {
            false
        }
    }

    pub fn match_name(&mut self, string: &str) -> Result<bool, CompileError> {
        if self.is_name(string) {
            self.next_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn expect_name(&mut self, string: &str) -> Result<(), CompileError> {
        if !self.match_name(string)? {
            Err(CompileError::InvalidTokenName{expected: Token::Name(string.to_string()), got: self.token.clone()})
        } else {
            Ok(())
        }
    }
}

// @TODO: Figure out which of these are even used and which are just pattern matched.
// @TODO: Maybe keywords should be their own token type.
// @TODO: See if we can use strings better instead of all the copying.

// @TODO: Make operators their own thing and not just tokens.
// Many operators aren't tokens.

#[derive(PartialEq, Debug)]
enum Expression {
    Int(i64),
    Float(f64),
    Binary {
        op: Token,
        right: Box<Expression>,
        left: Box<Expression>,
    },
    Name(String),
    Call {
        func: String,
        args: Vec<Expression>,
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum TypeSpec {
    I32,
    U32,
    I64,
    U64,
    F32,
    F64
}

// @TODO: maybe this should be the same type as for a variable.
#[derive(PartialEq, Debug)]
struct FuncParam {
    name: String,
    typespec: TypeSpec,
}

#[derive(PartialEq, Debug)]
enum Declaration {
    NONE,
    FUNC {
        name: String,
        params: Vec<FuncParam>,
        return_type: Option<TypeSpec>,
        block: Vec<Statement>,
    },
    GLOBAL { // this could be a global or a const.
        name: String,
        typespec: TypeSpec,
    },
    // types like structs and enums?
}

#[derive(PartialEq, Debug)]
enum Statement {
    NULL,
    LOCAL {
        name: String,
        typespec: TypeSpec,
    },
    ASSIGN {
        name: String,
        expression: Expression,
    },
    RETURN(Expression),
}

#[derive(PartialEq, Debug)]
struct Module {
    declarations: Vec<Declaration>
}

fn parse_expr2(mut lexer: &mut Lexer) -> Result<Expression, CompileError> {
    // @TODO: Maybe just always use patterns instead of those helpers.
    match lexer.token.clone() {
        Token::INT(i) => {
            lexer.next_token()?;
            Ok(Expression::Int(i))
        },
        Token::FLOAT(f) => {
            lexer.next_token()?;
            Ok(Expression::Float(f))
        },
        Token::Name(s) => {
            lexer.next_token()?;
            if let Token::LP = lexer.token { // this is basically is_token
                lexer.next_token()?;
                let mut args = vec![];
                println!("DEBUG: {:?}", lexer.token);
                if !lexer.is_token(Token::RP) {
                    args.push(parse_expr(&mut lexer)?);
                    while lexer.match_token(Token::COMMA)? {
                        args.push(parse_expr(&mut lexer)?);
                    }
                }
                lexer.expect_token(Token::RP);
                Ok(Expression::Call {func: s, args})
            } else {
                Ok(Expression::Name(s))
            }
        },
        Token::LP => {
            lexer.next_token()?;
            let result = parse_expr(&mut lexer)?;
            lexer.expect_token(Token::RP)?;
            Ok(result)
        },
        t => Err(CompileError::InvalidExpressionToken{token: t})
    }
}

fn parse_expr1(mut lexer: &mut Lexer) -> Result<Expression, CompileError> {
    let mut result = parse_expr2(&mut lexer)?;
    while lexer.is_token(Token::MUL) || lexer.is_token(Token::DIV) {
        let op = lexer.token.clone();
        lexer.next_token()?;
        let right = parse_expr2(&mut lexer)?;
        result = Expression::Binary {op, left: Box::new(result), right: Box::new(right)};
    }
    Ok(result)
}

fn parse_expr(mut lexer: &mut Lexer) -> Result<Expression, CompileError> {
    let mut result = parse_expr1(&mut lexer)?;
    while lexer.is_token(Token::ADD) || lexer.is_token(Token::SUB) {
        let op = lexer.token.clone();
        lexer.next_token()?;
        let right = parse_expr1(&mut lexer)?;
        result = Expression::Binary {op, left: Box::new(result), right: Box::new(right)};
    }
    Ok(result)
}

fn parse_typespec(mut lexer: &mut Lexer) -> Result<TypeSpec, CompileError> {
    if let Token::Name(name) = lexer.token.clone() {
        lexer.next_token()?;
        match name.as_ref() {
            "i32" => Ok(TypeSpec::I32),
            "i64" => Ok(TypeSpec::I64),
            "f32" => Ok(TypeSpec::F32),
            "f64" => Ok(TypeSpec::F64),
            _ => Err(CompileError::InvalidType{token: lexer.token.clone()})
        }
    } else {
        // @TODO: This isn't really the right error.
        Err(CompileError::InvalidToken {
            expected: Token::Name("".to_string()),
            got: lexer.token.clone()
        })
    }
}

fn parse_param(mut lexer: &mut Lexer) -> Result<FuncParam, CompileError> {
    if let Token::Name(name) = lexer.token.clone() {
        lexer.next_token()?;
        lexer.expect_token(Token::COLON)?;
        let typespec = parse_typespec(&mut lexer)?;
        Ok(FuncParam {
            name: name,
            typespec: typespec
        })
    } else {
        // @TODO: This isn't really the right error.
        Err(CompileError::InvalidToken {
            expected: Token::Name("".to_string()),
            got: lexer.token.clone()
        })
    }
}

fn parse_statement(mut lexer: &mut Lexer) -> Result<Statement, CompileError> {
    match lexer.token.clone() {
        Token::Name(ref n) if n == "return" => {
            // return statement
            lexer.next_token()?;
            let exp = parse_expr(&mut lexer)?;
            Ok(Statement::RETURN(exp))
        },
        Token::Name(ref n) if n == "var" => {
            // declaration
            lexer.next_token()?;
            let var = lexer.expect_a_name()?;
            lexer.expect_token(Token::COLON)?;
            let typespec = parse_typespec(&mut lexer)?;
            Ok(Statement::LOCAL {
                name: var,
                typespec
            })
        },
        Token::Name(ref n) => {
            // assignment
            let var = lexer.expect_a_name()?;
            lexer.expect_token(Token::EQUALS)?;
            let exp = parse_expr(&mut lexer)?;
            Ok(Statement::ASSIGN {
                name: var,
                expression: exp
            })
        },
        _ => Err(CompileError::InvalidStatementToken {token: lexer.token.clone() })
    }
}

fn parse_statement_block(mut lexer: &mut Lexer) -> Result<Vec<Statement>, CompileError> {
    // parse a statement list
    // I think I'll start with semicolon seperated shit.
    lexer.expect_token(Token::LB)?;
    let mut statements = vec![];
    while !lexer.is_token(Token::RB) {
        statements.push(parse_statement(&mut lexer)?);
        lexer.match_token(Token::SEMICOLON)?;
    }
    lexer.expect_token(Token::RB)?;
    Ok(statements)
}

fn parse_func(mut lexer: &mut Lexer) -> Result<Declaration, CompileError> {
    lexer.expect_name("func")?;
    if let Token::Name(name) = lexer.token.clone() {
        lexer.next_token()?;
        lexer.expect_token(Token::LP)?;
        let mut params : Vec<FuncParam> = vec![];
        if !lexer.is_token(Token::RP) {
            params.push(parse_param(&mut lexer)?);
            while lexer.match_token(Token::COMMA)? {
                params.push(parse_param(&mut lexer)?);
            }
        }
        lexer.expect_token(Token::RP)?;
        let mut return_type = None;
        if lexer.match_token(Token::COLON)? {
            return_type = Some(parse_typespec(&mut lexer)?);
        }
        let block = parse_statement_block(&mut lexer)?;
        Ok(Declaration::FUNC {name: name, params, return_type, block})
    } else {
        Err(CompileError::InvalidToken{
            expected: Token::Name("".to_string()),
            got: lexer.token.clone()
        })
    }
}

fn parse_declaration(mut lexer: &mut Lexer) -> Result<Declaration, CompileError> {
    //@TODO: More declarations than just funcs later.
    if lexer.is_name("func") {
        parse_func(&mut lexer)
    } else {
        Err(CompileError::InvalidDeclarationToken{token: lexer.token.clone()})
    }
}

fn parse_module(mut lexer: &mut Lexer) -> Result<Module, CompileError> {
    let mut declarations = vec![];
    while !lexer.is_token(Token::EOF) {

        declarations.push(parse_declaration(&mut lexer)?);
    }
    Ok(Module{ declarations })
}

// So I need all the things I can call from wasm.
// I can call any of these with a function call.

// Control flow operators are done with language constructs.
// call and call_indirect are what is used for user defined functions and imports.
// parametric, drop and select I'm not sure what those are for.
// variable accesses are used for locals and globals, I should be able to tell which I'm using
// by their name.
// memory operators
enum Operator {
    // Control Operators
    unreachable = 0x00,
    nop = 0x01,
    block = 0x02,
    loop_ = 0x03,
    if_ = 0x04,
    else_ = 0x05,
    end = 0x0b,
    br = 0x0c,
    br_if = 0x0d,
    br_table = 0x0e,
    return_ = 0x0f,
    
    call = 0x10,
    call_indirect = 0x11,

    drop = 0x1a,
    select = 0x1b,

    get_local = 0x20,
    set_local = 0x21,
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

    i32_const = 0x41,
    i64_const = 0x42,
    f32_const = 0x43,
    f64_const = 0x44,

    i32_eqz = 0x45,
    i32_eq = 0x46,
    i32_ne = 0x47,
    i32_lt_s = 0x48,
    i32_lt_u = 0x49,
    i32_gt_s = 0x4a,
    i32_gt_u = 0x4b,
    i32_le_s = 0x4c,
    i32_le_u = 0x4d,
    i32_ge_s = 0x4e,
    i32_ge_u = 0x4f,
    i64_eqz = 0x50,
    i64_eq = 0x51,
    i64_ne = 0x52,
    i64_lt_s = 0x53,
    i64_lt_u = 0x54,
    i64_gt_s = 0x55,
    i64_gt_u = 0x56,
    i64_le_s = 0x57,
    i64_le_u = 0x58,
    i64_ge_s = 0x59,
    i64_ge_u = 0x5a,
    f32_eq = 0x5b,
    f32_ne = 0x5c,
    f32_lt = 0x5d,
    f32_gt = 0x5e,
    f32_le = 0x5f,
    f32_ge = 0x60,
    f64_eq = 0x61,
    f64_ne = 0x62,
    f64_lt = 0x63,
    f64_gt = 0x64,
    f64_le = 0x65,
    f64_ge = 0x66,

    i32_clz = 0x67,
    i32_ctz = 0x68,
    i32_popcnt = 0x69,
    i32_add = 0x6a,
    i32_sub = 0x6b,
    i32_mul = 0x6c,
    i32_div_s = 0x6d,
    i32_div_u = 0x6e,
    i32_rem_s = 0x6f,
    i32_rem_u = 0x70,
    i32_and = 0x71,
    i32_or = 0x72,
    i32_xor = 0x73,
    i32_shl = 0x74,
    i32_shr_s = 0x75,
    i32_shr_u = 0x76,
    i32_rotl = 0x77,
    i32_rotr = 0x78,
    i64_clz = 0x79,
    i64_ctz = 0x7a,
    i64_popcnt = 0x7b,
    i64_add = 0x7c,
    i64_sub = 0x7d,
    i64_mul = 0x7e,
    i64_div_s = 0x7f,
    i64_div_u = 0x80,
    i64_rem_s = 0x81,
    i64_rem_u = 0x82,
    i64_and = 0x83,
    i64_or = 0x84,
    i64_xor = 0x85,
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
    f32_add = 0x92,
    f32_sub = 0x93,
    f32_mul = 0x94,
    f32_div = 0x95,
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
    f64_add = 0xa0,
    f64_sub = 0xa1,
    f64_mul = 0xa2,
    f64_div = 0xa3,
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

use bytebuffer::ByteBuffer; // @TODO: Drop this.

const magic_number: u32 = 0x6d736100;
const version: u32 = 0x1;

const i32_type: u8 = 0x7f;
const i64_type: u8 = 0x7e;
const f32_type: u8 = 0x7d;
const f64_type: u8 = 0x7c;
const anyfunc_type: u8 = 0x70;
const func_type: u8 = 0x60;
const empty_block_type: u8 = 0x40;

fn write_leb128(mut val: u32, mut buf: &mut ByteBuffer) {
    const continuation_bit: u8 = 1 << 7;
    loop {
        let mut byte: u8 = (val & (std::u8::MAX as u32)) as u8 & !continuation_bit;
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

pub fn new_buffer() -> ByteBuffer {
    let mut buffer = ByteBuffer::new();
    buffer.set_endian(LittleEndian);
    buffer
}

fn emit_exp(fns: &Vec<String>, vars: &Vec<String>, mut buf: &mut ByteBuffer, exp: &Expression) {
    match exp {
        // @TODO: Typechecking for 32 or 64, for now only doing 32 bit stuff.
        Expression::Int(i) => {
            // emit int literal
            buf.write_u8(0x41);
            write_leb128(*i as u32, &mut buf);
        },
        Expression::Float(f) => {
            buf.write_u8(0x43);
            buf.write_f32(*f as f32);
        },
        Expression::Binary {op, left, right} => {
            emit_exp(fns, vars, buf, left);
            emit_exp(fns, vars, buf, right);
            // @TODO: Assumes everything is an int right now.
            match op {
                Token::ADD => {
                    buf.write_u8(0x6a);
                },
                Token::SUB => {
                    buf.write_u8(0x6b);
                },
                Token::MUL => {
                    buf.write_u8(0x6c);
                },
                Token::DIV => {
                    buf.write_u8(0x6d); // divide signed
                },
                _ => {} // error!
            }
        },
        Expression::Name(n) => {
            println!("DEBUG: {:?}, {}", vars, n);
            let index = vars.iter().position(|v| v == n).unwrap();
            println!("DEBUG: {}", index);
            buf.write_u8(0x20); // get local
            write_leb128(index as u32, &mut buf);
        },
        Expression::Call{func, args} => {
            println!("DEBUG: {:?}, {}", fns, func);
            let index = fns.iter().position(|f| f == func).unwrap();
            println!("DEBUG: {}", index);
            for arg in args {
                emit_exp(fns, vars, buf, arg);
            }
            buf.write_u8(0x10); // Call
            write_leb128(index as u32, &mut buf);
        }
    }
}

// Declarations have to be done up front so won't hit this?
fn emit_statement(fns: &Vec<String>, vars: &Vec<String>, mut buf: &mut ByteBuffer, statement: &Statement) {
    match statement {
        Statement::ASSIGN {name, expression} => {
            // @TODO: There's like tee local and stuff, since I'm doing 0 optimization so far whatev.
            emit_exp(fns, vars, &mut buf, expression);
            // set local
            let index = vars.iter().position(|v| v == name).unwrap(); // @TODO: Type check before here lol
            buf.write_u8(0x21); // set local
            write_leb128(index as u32, &mut buf);
        },
        Statement::RETURN(expression) => {
            emit_exp(fns, vars, &mut buf, expression);
            buf.write_u8(0x0f); // return
        },
        _ => {}// @TODO
    }
}

fn emit_module(module: Module) -> ByteBuffer {
    let mut functs = vec![];
    for decl in &module.declarations {
        if let Declaration::FUNC {name, params, return_type, block} = decl {
            functs.push(
                (name,
                 params.len() as u32,
                 params.iter().map(|p| p.typespec).collect::<Vec<TypeSpec>>(),
                 return_type,
                )
            );
        }
    }

    let mut type_section = new_buffer();
    write_leb128(functs.len() as u32, &mut type_section);
    for (name, num_params, param_types, return_type) in &functs {
        type_section.write_u8(func_type);
        write_leb128(*num_params, &mut type_section);
        for t in param_types {
            match t {
                TypeSpec::I32 => type_section.write_u8(i32_type),
                TypeSpec::U32 => type_section.write_u8(i32_type),
                TypeSpec::I64 => type_section.write_u8(i64_type),
                TypeSpec::U64 => type_section.write_u8(i64_type),
                TypeSpec::F32 => type_section.write_u8(f32_type),
                TypeSpec::F64 => type_section.write_u8(f64_type)
            }
        }
        if let Some(t) = return_type {
            write_leb128(1, &mut type_section);
            match t {
                TypeSpec::I32 => type_section.write_u8(i32_type),
                TypeSpec::U32 => type_section.write_u8(i32_type),
                TypeSpec::I64 => type_section.write_u8(i64_type),
                TypeSpec::U64 => type_section.write_u8(i64_type),
                TypeSpec::F32 => type_section.write_u8(f32_type),
                TypeSpec::F64 => type_section.write_u8(f64_type)
            }
        } else {
            write_leb128(0, &mut type_section);
        }
    }

    let mut function_section = new_buffer();
    write_leb128(functs.len() as u32, &mut function_section);
    for i in 0..functs.len() {
        write_leb128(i as u32, &mut function_section);
    }

    let mut export_section = new_buffer();
    write_leb128(functs.len() as u32, &mut export_section);
    for (i, (name, num_params, param_types, return_type)) in functs.iter().enumerate() {
        //println!("DEBUG: {:?} {:?} {:?}", name, name.len() as u32, name.as_bytes());
        write_leb128(name.len() as u32, &mut export_section);
        export_section.write_bytes(name.as_bytes());
        let function_export_kind = 0;
        export_section.write_u8(0); // Function Type
        write_leb128(i as u32, &mut export_section); // which function
    }

    let mut code_section = new_buffer();
    write_leb128(functs.len() as u32, &mut code_section);

    let funct_names = functs.iter().map(|f| f.0.clone()).collect::<Vec<String>>();

    for decl in &module.declarations {
        if let Declaration::FUNC {name, params, return_type, block} = decl {

            // @TODO: Get index for all the variable names (and also function calls)
            // @TODO: should be able to have this be all refs? maybe?
            let mut vars = vec![];
            for param in params {
                vars.push(param.name.clone());
            }

            for stmt in block {
                if let Statement::LOCAL{ name, typespec } = stmt {
                    vars.push(name.clone());
                }
            }

            let mut func_body = new_buffer();
            write_leb128(vars.len() as u32 - params.len() as u32, &mut func_body); // num locals

            // @OPTIMIZE: These can be specified in blocks of same type vars. Useful for compression
            // and for arrays on the stack.
            for stmt in block {
                if let Statement::LOCAL {name, typespec} = stmt {
                    write_leb128(1, &mut func_body);
                    match typespec {
                        TypeSpec::I32 => func_body.write_u8(0x7f),
                        TypeSpec::U32 => func_body.write_u8(0x7f),
                        TypeSpec::I64 => func_body.write_u8(0x7e),
                        TypeSpec::U64 => func_body.write_u8(0x7e),
                        TypeSpec::F32 => func_body.write_u8(0x7d),
                        TypeSpec::F64 => func_body.write_u8(0x7c),
                    }
                
                }
            }

            // emit the code
            for stmt in block {
                emit_statement(&funct_names, &vars, &mut func_body, stmt);
            }

            // end
            func_body.write_u8(0x0B); // end function
            write_leb128(func_body.len() as u32, &mut code_section);
            code_section.write_bytes(&func_body.to_bytes());
        }
    }

    // @TODO: Data section is where we load the inital memory. Will be needed for the
    // application stack and type info.

    // @TODO: The name section can be used so all the vars have names when decoded.
    // Makes debugging way easier.

    let mut buffer = new_buffer();
    buffer.write_u32(magic_number);
    buffer.write_u32(version);

    let type_section_header = 1;
    buffer.write_u8(type_section_header);
    write_leb128(type_section.len() as u32, &mut buffer);
    buffer.write_bytes(&type_section.to_bytes());

    let function_section_header = 3;
    buffer.write_u8(function_section_header);
    write_leb128(function_section.len() as u32, &mut buffer);
    buffer.write_bytes(&function_section.to_bytes());

    let export_section_header = 7;
    buffer.write_u8(export_section_header);
    write_leb128(export_section.len() as u32, &mut buffer);
    buffer.write_bytes(&export_section.to_bytes());

    let code_section_header = 10;
    buffer.write_u8(code_section_header);
    write_leb128(code_section.len() as u32, &mut buffer);
    buffer.write_bytes(&code_section.to_bytes());

    buffer
}

fn string_err(err: CompileError) -> String {
    format!("{:?}", err)
}

pub fn compile(src: String) -> Result<Vec<u8>, String> {
    let mut l = Lexer::new(&src).map_err(string_err)?;
    let module = parse_module(&mut l).map_err(string_err)?;
    println!("{:?}", module);
    let wasm = emit_module(module);
    Ok(wasm.to_bytes())
}

// od -t d1 it.wasm

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_lex() {
        let src: &'static str = "abZc \n def 123 123.45 99.9e12 + - / * = 123 wuoah";
        let mut l = Lexer::new(src).unwrap();

        let mut assert_token = |token| {
            assert_eq!(l.token, token);
            l.next_token().unwrap();
        };

        assert_token(Token::Name("abZc".to_string()));
        assert_token(Token::Name("def".to_string()));
        assert_token(Token::INT(123));
        assert_token(Token::FLOAT(123.45));
        assert_token(Token::FLOAT(99.9e12));
        assert_token(Token::ADD);
        assert_token(Token::SUB);
        assert_token(Token::DIV);
        assert_token(Token::MUL);
        assert_token(Token::EQUALS);
        assert_token(Token::INT(123));
        assert_token(Token::Name("wuoah".to_string()));
        assert_token(Token::EOF);
    }

    #[test]
    fn test_exp_parse() {
        let src: &'static str = "1 * 2 + 3 / (4 + 6)";
        let mut l = Lexer::new(src).unwrap();
        let exp = parse_expr(&mut l);
        println!("{}", src);
        println!("{:#?}", exp);
    }

    #[test]
    fn test_module_parse() {
        let src: &'static str = r#"
        func add(i: i32, j: i32): i32 {
            var foo: i32;
            foo = i + j;
            return foo;
        }
        func main() : i32 {
            return add(1,2) + add(3,4);
        }
        "#;
        let mut l = Lexer::new(src).unwrap();
        let module = parse_module(&mut l).expect("parses");
        println!("{}", src);
        println!("{:#?}", module);
        let buf = emit_module(module);
        println!("{:?}", buf);
    }

    #[test]
    fn test_leb128() {
        let mut a = ByteBuffer::new();
        write_leb128(0, &mut a);
        let bytes = a.to_bytes();
        println!("{:?}", bytes);
        //assert_eq!(bytes, [42]);
    }

    #[test]
    fn debug_me() {
        let src: &'static str = r#"
        func main() : f32 {
            return 123.45;
        }
        "#;
        let mut l = Lexer::new(src).unwrap();
        let module = parse_module(&mut l).expect("parses");
        println!("{}", src);
        println!("{:#?}", module);
        let buf = emit_module(module);
        println!("{:?}", buf);
    }
}

// Add the other primitive types.
// Do typechecking and pick operators.
// Add all the rest of the operators based on types.
// Add transform operators too.
// control structures
// better error messages
// Then I can think about structs and arrays and stuff, but I need that base first.

// That will be v1, all the wasm primitives with the 4 wasm types.
// then for v2 I'll play with memory management, references, structs and arrays

// If I actually do this language I will be so happy!




// How can I make working on this not suck?
// I should make a feature for making it a webassembly library.
// Then I can disable that feature and have a binary that's a test program that lets me just run a program and debug it.
// If that's a good idea it could help me out A TON with debugging this.

// That could let me debug this in clion too which might be better since I can view rust values.

// I still can't fully debug everything so I really need printing which is why I need a binary I can debug that isn't a test.
// Gonna set up a new project that has a debugable binary, has tests that use a spec interpreter and then feature flags that
// set up the wasm stuff so I can still publish to npm.

// I think I guess I need to really learn how to use vscode too.
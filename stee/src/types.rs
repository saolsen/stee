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
    },
    #[fail(display = "Unknown Operator {:?} for types ({:?},{:?}", op, lhs, rhs)]
    UnknownOperator {
        op: Token,
        lhs: TypeSpec,
        rhs: TypeSpec
    },
    #[fail(display = "Unknown Function {:?} for arg types {:?}", func, arg_types)]
    UnknownFunction{
        func: String,
        arg_types: Vec<TypeSpec>
    },
    #[fail(display = "Not implemented yet.")]
    NotImplemented
}

// @TODO: Tokens for keywords so I don't have to name-compare them and can just match them directly.
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    EOF,
    INT(i64),
    FLOAT(f64),
    NAME(String),
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

#[derive(PartialEq, Debug)]
pub enum Expression {
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
    Binary {
        op: Token,
        right: Box<Expression>,
        left: Box<Expression>,
    },
    Name(String), // local or global variable probably.
    Call {
        func: String,
        args: Vec<Expression>,
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TypeSpec {
    NULL,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64
}

#[derive(PartialEq, Debug, Clone)]
pub struct Var {
    pub name: String,
    pub typespec: TypeSpec,
}

#[derive(PartialEq, Debug)]
pub struct FuncParam {
    pub var: Var
}

#[derive(PartialEq, Debug)]
pub struct Func {
    pub name: String,
    pub params: Vec<FuncParam>,
    pub return_type: TypeSpec,
    pub block: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub enum Declaration {
    NONE,
    FUNC {
        func: Func
    },
    GLOBAL { // this could be a global or a const.
        var: Var
    },
    // types like structs and enums?
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    NULL,
    LOCAL {
        var: Var
    },
    ASSIGN {
        name: String,
        expression: Expression,
    },
    RETURN(Expression),
}

#[derive(PartialEq, Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>
}
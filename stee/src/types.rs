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
    #[fail(display = "Unknown Variable {:?}", name)]
    UnknownVariable{
        name: String,
    },
    #[fail(display = "Type mismatch expected {:?} got {:?}", expected, got)]
    TypeMismatch{
        expected: TypeSpec,
        got: TypeSpec
    },
    #[fail(display = "Not implemented yet.")]
    NotImplemented
}

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
    REM,
    LP,
    RP,
    LB,
    RB,
    COMMA,
    COLON,
    SEMICOLON,
    EQUALS,
    EQUALTO,
    NEQUALTO,
    LT,
    GT,
    LE,
    GE,
    AND,
    OR,
    XOR,
    NOT,
}
// @TODO: Make life better stuff
// += -= ...
// :=

#[derive(PartialEq, Debug)]
pub enum Expression {
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
    Unary {
        op: Token,
        arg: Box<Expression>,
    },
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
    GLOBAL {
        var: Var
    },
    EXTERN {
        name: String,
        params: Vec<FuncParam>,
        return_type: TypeSpec,
    }
    // @TODO: Const
    // @TODO: Type definitions like structs and enums.
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    NULL,
    BLOCK {
        block: Vec<Statement>,
    },
    LOCAL {
        var: Var
    },
    ASSIGN {
        name: String,
        expression: Expression,
    },
    IF {
        condition: Expression,
        then_block: Vec<Statement>,
        else_block: Vec<Statement>,
    },
    WHILE {
        condition: Expression,
        block: Vec<Statement>,
    },
    FOR {
        setup: Box<Statement>,
        condition: Expression,
        iter: Box<Statement>,
        block: Vec<Statement>,
    },
    SWITCH {
        index: Expression,
        values: Vec<i32>,
        blocks: Vec<Vec<Statement>>,
        default: Vec<Statement>,
    },
    RETURN(Expression),
}

#[derive(PartialEq, Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>
}

// globals
// blocks as statements
// pub functions for export
// import function declarations
// builtin functions
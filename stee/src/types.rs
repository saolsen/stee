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

#[derive(PartialEq, Debug)]
pub enum Expression {
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
pub enum TypeSpec {
    I32,
    U32,
    I64,
    U64,
    F32,
    F64
}

#[derive(PartialEq, Debug)]
pub struct FuncParam {
    pub name: String,
    pub typespec: TypeSpec,
}

#[derive(PartialEq, Debug)]
pub enum Declaration {
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
pub enum Statement {
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
pub struct Module {
    pub declarations: Vec<Declaration>
}
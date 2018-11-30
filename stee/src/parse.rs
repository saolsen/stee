use super::types::*;
use super::lex::*; 

fn parse_expr2(mut lexer: &mut Lexer) -> Result<Expression, CompileError> {
    // @TODO: Maybe just always use patterns instead of those helpers.
    match lexer.token.clone() {
        Token::INT(i) => {
            lexer.next_token()?;
            Ok(Expression::I32(i as i32))
        },
        Token::FLOAT(f) => {
            lexer.next_token()?;
            Ok(Expression::F32(f as f32))
        },
        Token::NAME(s) => {
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
                lexer.expect_token(Token::RP)?;
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

fn parse_typespec(lexer: &mut Lexer) -> Result<TypeSpec, CompileError> {
    if let Token::NAME(name) = lexer.token.clone() {
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
            expected: Token::NAME("".to_string()),
            got: lexer.token.clone()
        })
    }
}

fn parse_param(mut lexer: &mut Lexer) -> Result<FuncParam, CompileError> {
    if let Token::NAME(name) = lexer.token.clone() {
        lexer.next_token()?;
        lexer.expect_token(Token::COLON)?;
        let typespec = parse_typespec(&mut lexer)?;
        Ok(FuncParam {
            var: Var {
                name: name,
                typespec: typespec
            }
        })
    } else {
        // @TODO: This isn't really the right error.
        Err(CompileError::InvalidToken {
            expected: Token::NAME("".to_string()),
            got: lexer.token.clone()
        })
    }
}

fn parse_statement(mut lexer: &mut Lexer) -> Result<Statement, CompileError> {
    match lexer.token.clone() {
        Token::NAME(ref n) if n == "return" => {
            // return statement
            lexer.next_token()?;
            let exp = parse_expr(&mut lexer)?;
            Ok(Statement::RETURN(exp))
        },
        Token::NAME(ref n) if n == "var" => {
            // declaration
            lexer.next_token()?;
            let var = lexer.expect_a_name()?;
            lexer.expect_token(Token::COLON)?;
            let typespec = parse_typespec(&mut lexer)?;
            Ok(Statement::LOCAL {
                var: Var {
                    name: var,
                    typespec
                }
            })
        },
        Token::NAME(_) => {
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
    if let Token::NAME(name) = lexer.token.clone() {
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
        let mut return_type = TypeSpec::NULL;
        if lexer.match_token(Token::COLON)? {
            return_type = parse_typespec(&mut lexer)?;
        }
        let block = parse_statement_block(&mut lexer)?;
        Ok(Declaration::FUNC {func: Func{name: name, params, return_type, block}})
    } else {
        Err(CompileError::InvalidToken{
            expected: Token::NAME("".to_string()),
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

pub fn parse_module(mut lexer: &mut Lexer) -> Result<Module, CompileError> {
    let mut declarations = vec![];
    while !lexer.is_token(Token::EOF) {

        declarations.push(parse_declaration(&mut lexer)?);
    }
    Ok(Module{ declarations })
}

#[cfg(test)]
mod tests {
    use super::*;

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
    }
}
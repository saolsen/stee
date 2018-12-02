use std::str::Chars;
use std::iter::Peekable;

use super::types::*;

fn get_next_token(src: &mut Peekable<Chars>) -> Result<Token, CompileError> {
    'restart: loop {
        loop {
            match src.peek() {
                Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                    src.next();
                },
                _ => break
            }
        }

        let t = match src.peek().cloned() {
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
                        Ok(Token::NAME(name))
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
                    '+'  => { src.next(); Ok(Token::ADD) },
                    '-'  => { src.next(); Ok(Token::SUB) },
                    '*'  => { src.next(); Ok(Token::MUL) },
                    '/'  => {
                        src.next();
                        if src.peek() == Some(&'/') {
                            src.next();
                            loop {
                                match src.peek() {
                                    None | Some('\n') => continue 'restart,
                                    _ => src.next()
                                };
                            };
                        } else {
                            Ok(Token::DIV)
                        }
                    },
                    '%'  => { src.next(); Ok(Token::REM) },
                    '('  => { src.next(); Ok(Token::LP) },
                    ')'  => { src.next(); Ok(Token::RP) },
                    '{'  => { src.next(); Ok(Token::LB) },
                    '}'  => { src.next(); Ok(Token::RB) },
                    ','  => { src.next(); Ok(Token::COMMA) },
                    ':'  => { src.next(); Ok(Token::COLON) },
                    ';'  => { src.next(); Ok(Token::SEMICOLON) },
                    '='  => {
                        src.next();
                        if let Some(c) = src.peek().cloned() {
                            match c {
                                '=' => { src.next(); Ok(Token::EQUALTO) },
                                _ => Ok(Token::EQUALS)
                            }
                        } else {
                            Ok(Token::EQUALS)
                        }
                    },
                    '!'  => {
                        src.next();
                        if let Some(c) = src.peek().cloned() {
                            match c {
                                '=' => { src.next(); Ok(Token::NEQUALTO) },
                                _ => Ok(Token::NOT)
                            }
                        } else {
                            Ok(Token::NOT)
                        }
                    },
                    // @TODO: ==, !=, <=, >=
                    '<'  => {
                        src.next();
                        if let Some(c) = src.peek().cloned() {
                            match c {
                                '=' => { src.next(); Ok(Token::LE) },
                                _ => Ok(Token::LT)
                            }
                        } else {
                            Ok(Token::LT)
                        }
                    },
                    '>'  => {
                        src.next();
                        if let Some(c) = src.peek().cloned() {
                            match c {
                                '=' => { src.next(); Ok(Token::GE) },
                                _ => Ok(Token::GT)
                            }
                        } else {
                            Ok(Token::GT)
                        }
                    },
                    '&' => { src.next(); Ok(Token::AND) },
                    '|' => { src.next(); Ok(Token::OR) },
                    '^' => { src.next(); Ok(Token::XOR) },
                    '\0' => { src.next(); Ok(Token::EOF) },
                    _ => Err(CompileError::InvalidTokenCharacter {c})
                }
            }
        };
        return t;
    }
}

pub struct Lexer<'a> {
    src: Peekable<Chars<'a>>,
    pub token: Token
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
        if let Token::NAME(n) = self.token.clone() {
            self.next_token()?;
            Ok(n)
        } else {
            Err(CompileError::InvalidToken {
                expected: Token::NAME("".to_string()),
                got: self.token.clone()
            })
        }
    }

    pub fn is_name(&mut self, string: &str) -> bool {
        if let Token::NAME(ref s) = self.token {
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
            Err(CompileError::InvalidTokenName{expected: Token::NAME(string.to_string()), got: self.token.clone()})
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_lex() {
        let src: &'static str = "abZc \n def 123 123.45 99.9e12 + - / * = 123 == wuoah <= = > >= & | ^ ! !=";
        let mut l = Lexer::new(src).unwrap();

        let mut assert_token = |token| {
            assert_eq!(l.token, token);
            l.next_token().unwrap();
        };

        assert_token(Token::NAME("abZc".to_string()));
        assert_token(Token::NAME("def".to_string()));
        assert_token(Token::INT(123));
        assert_token(Token::FLOAT(123.45));
        assert_token(Token::FLOAT(99.9e12));
        assert_token(Token::ADD);
        assert_token(Token::SUB);
        assert_token(Token::DIV);
        assert_token(Token::MUL);
        assert_token(Token::EQUALS);
        assert_token(Token::INT(123));
        assert_token(Token::EQUALTO);
        assert_token(Token::NAME("wuoah".to_string()));
        assert_token(Token::LE);
        assert_token(Token::EQUALS);
        assert_token(Token::GT);
        assert_token(Token::GE);
        assert_token(Token::AND);
        assert_token(Token::OR);
        assert_token(Token::XOR);
        assert_token(Token::NOT);
        assert_token(Token::NEQUALTO);
        assert_token(Token::EOF);
    }
}

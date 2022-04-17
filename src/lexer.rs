use itertools::Itertools;
use std::{iter::Peekable, str::Chars};
struct Lexer<'str> {
    chars: Box<Peekable<Chars<'str>>>,
    indent_level: usize,
    should_begin: bool,
}

use crate::tokens::Token;
impl<'str> Lexer<'str> {
    #[allow(dead_code)]
    pub fn new(source: &'str str) -> Self {
        Self {
            chars: Box::new(source.chars().peekable()),
            indent_level: 0,
            should_begin: false,
        }
    }

    pub fn lex(&mut self) -> Token {
        if self.chars.peek().is_none() {
            return Token::EoF;
        }
        let chars = self.chars.as_mut();
        let ws = chars
            .peeking_take_while(|c| dbg!(c) != &'\n' && c.is_ascii_whitespace())
            .map(|c| {
                if c == ' ' {
                    1
                } else if c == '\t' {
                    4
                } else {
                    0
                }
            })
            .sum();
        let peeked = chars.peek();
        if peeked == Some(&'\n') {
            println!("new line detected.  recurse");
            self.should_begin = true;
            chars.next();
            return self.lex();
        }
        if ws < self.indent_level && self.should_begin {
            self.indent_level = ws;
            self.should_begin = false;
            return Token::EndBlock(ws);
        }
        if ws > self.indent_level && self.should_begin {
            self.indent_level = ws;
            self.should_begin = false;
            return Token::BeginBlock(ws);
        }
        self.should_begin = false;
        let token: String = chars
            .peeking_take_while(|c| !c.is_ascii_whitespace())
            .collect();
        dbg!(&token);
        if token.starts_with('\'') {
            return Token::CharLiteral(
                if token.ends_with('\'')
                    && token.len() > 1
                    && token.chars().nth(token.len().saturating_sub(2)) != Some('\\')
                {
                    token
                } else {
                    let mut should_skip = false;
                    token
                        + &chars
                            .take_while(|c| {
                                if should_skip {
                                    should_skip = false;
                                    true
                                } else if c == &'\\' {
                                    should_skip = true;
                                    true
                                } else {
                                    c != &'\''
                                }
                            })
                            .collect::<String>()
                        + "'"
                },
            );
        }
        if token.starts_with('\"') {
            return Token::StringLiteral(
                if token.ends_with('\"')
                    && token.len() > 1
                    && token.chars().nth(token.len().saturating_sub(2)) != Some('\\')
                {
                    token
                } else {
                    let mut should_skip = false;
                    token
                        + &chars
                            .take_while(|c| {
                                if should_skip {
                                    should_skip = false;
                                    true
                                } else if c == &'\\' {
                                    should_skip = true;
                                    true
                                } else {
                                    c != &'\"'
                                }
                            })
                            .collect::<String>()
                        + "\""
                },
            );
        }

        if token.chars().all(char::is_numeric) {
            return Token::Integer(token);
        }

        if token.chars().all(|c| c.is_numeric() || c == '.') {
            return Token::FloatingPoint(token);
        }

        if token == "=" {
            return Token::Equals;
        }

        if token
            .chars()
            .all(|c| matches!(c, '>' | '<' | '!' | '@' | '$' | '=' | '&' | '|'))
        {
            return Token::Op(token);
        }

        if token == "let" {
            return Token::Let;
        }
        Token::Ident(token)
    }
}

pub struct TokenStream<'str> {
    lexer: Lexer<'str>,
    ended: bool,
}

impl<'src> TokenStream<'src> {
    #[allow(dead_code)]
    pub fn from_source(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src),
            ended: false,
        }
    }
}

impl<'str> Iterator for TokenStream<'str> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            None
        } else {
            let token = self.lexer.lex();
            if token.is_eof() {
                self.ended = true;
            }
            Some(token)
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::tokens::Token;

    use super::TokenStream;

    #[test]
    fn single_tokens() {
        assert_eq!(
            TokenStream::from_source("let").collect::<Vec<_>>(),
            vec![Token::Let, Token::EoF],
            "let token"
        );
        assert_eq!(
            TokenStream::from_source("1").collect::<Vec<_>>(),
            vec![Token::Integer("1".to_owned()), Token::EoF],
            "int token"
        );
        assert_eq!(
            TokenStream::from_source("1.0").collect::<Vec<_>>(),
            vec![Token::FloatingPoint("1.0".to_owned()), Token::EoF],
            "float token"
        );
        assert_eq!(
            TokenStream::from_source("=").collect::<Vec<_>>(),
            vec![Token::Equals, Token::EoF],
            "equals token"
        );
        assert_eq!(
            TokenStream::from_source("'l'").collect::<Vec<_>>(),
            vec![Token::CharLiteral("'l'".to_owned()), Token::EoF],
            "char token"
        );
        assert_eq!(
            TokenStream::from_source("\"let\"").collect::<Vec<_>>(),
            vec![Token::StringLiteral("\"let\"".to_owned()), Token::EoF],
            "string token"
        );
        assert_eq!(
            TokenStream::from_source("foo").collect::<Vec<_>>(),
            vec![Token::Ident("foo".to_owned()), Token::EoF],
            "ident token"
        );
        assert_eq!(
            TokenStream::from_source("!=").collect::<Vec<_>>(),
            vec![Token::Op("!=".to_owned()), Token::EoF],
            "op token"
        );
    }
    use itertools::Itertools;

    #[test]
    fn literals_edge_cases() {
        assert_eq!(
            TokenStream::from_source("\"foo bar \\\"baz\"").collect_vec(),
            vec![
                Token::StringLiteral("\"foo bar \\\"baz\"".to_owned()),
                Token::EoF
            ]
        );
        assert_eq!(
            TokenStream::from_source("\"\"").collect_vec(),
            vec![Token::StringLiteral("\"\"".to_owned()), Token::EoF]
        );
        assert_eq!(
            TokenStream::from_source("\" \"").collect_vec(),
            vec![Token::StringLiteral("\" \"".to_owned()), Token::EoF]
        );

        assert_eq!(
            TokenStream::from_source("' '").collect_vec(),
            vec![Token::CharLiteral("' '".to_owned()), Token::EoF]
        );
    }

    #[test]
    fn token_chain() {
        assert_eq!(
            TokenStream::from_source(
                "let foo =
\t1.0

let bar = 1

let baz = \"foo bar baz\"
"
            )
            .collect_vec(),
            #[rustfmt::skip]
            [
                Token::Let,Token::Ident("foo".to_owned()),Token::Equals,
                Token::BeginBlock(4),
                    Token::FloatingPoint("1.0".to_owned()),
                Token::EndBlock(0),
                Token::Let, Token::Ident("bar".to_owned()), Token::Equals, Token::Integer("1".to_owned()),
                Token::Let, Token::Ident("baz".to_owned()), Token::Equals, Token::StringLiteral("\"foo bar baz\"".to_owned()),
                Token::EoF,
            ]
        )
    }
}

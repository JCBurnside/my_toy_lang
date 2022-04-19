use itertools::Itertools;
use std::{iter::Peekable, str::Chars};
struct Lexer<'str> {
    chars: Box<Peekable<Chars<'str>>>,
    pos: usize,
    indent_level: usize,
    should_begin: bool,
}

use crate::tokens::Token;
impl<'str> Lexer<'str> {
    #[allow(dead_code)]
    pub fn new(source: &'str str) -> Self {
        Self {
            chars: Box::new(source.chars().peekable()),
            pos: 0,
            indent_level: 0,
            should_begin: false,
        }
    }

    pub fn lex(&mut self) -> (Token, usize) {
        if self.chars.peek().is_none() {
            return (Token::EoF, self.pos);
        }
        let chars = self.chars.as_mut();
        let ws = chars
            .peeking_take_while(|c| c != &'\n' && c.is_ascii_whitespace())
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
            let pos = self.pos;
            self.pos += ws;
            return (Token::EndBlock(ws), pos);
        }
        if ws > self.indent_level && self.should_begin {
            self.indent_level = ws;
            self.should_begin = false;
            let pos = self.pos;
            self.pos += ws;
            return (Token::BeginBlock(ws), pos);
        }
        self.should_begin = false;
        let token: String = chars
            .peeking_take_while(|c| !c.is_ascii_whitespace())
            .collect();

        if token.starts_with('\'') {
            let token = if token.ends_with('\'')
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
            };
            let pos = self.pos;
            self.pos += token.len();
            return (Token::CharLiteral(token), pos);
        }
        if token.starts_with('\"') {
            let token = if token.ends_with('\"')
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
            };
            let pos = self.pos;
            self.pos += token.len();
            return (Token::StringLiteral(token), pos);
        }

        let pos = self.pos;
        self.pos += token.len();
        if token == ":" {
            return (Token::Colon, pos);
        }

        if token.chars().all(char::is_numeric) {
            return (Token::Integer(token), pos);
        }

        if token.chars().all(|c| c.is_numeric() || c == '.') {
            return (Token::FloatingPoint(token), pos);
        }

        if token == "=" {
            return (Token::Equals, pos);
        }

        if token
            .chars()
            .all(|c| matches!(c, '>' | '<' | '!' | '@' | '$' | '=' | '&' | '|'))
        {
            return (Token::Op(token), pos);
        }

        if token == "let" {
            return (Token::Let, pos);
        }

        if token == "return" {
            return (Token::Return, pos);
        }
        (Token::Ident(token), pos)
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
    type Item = (Token, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            None
        } else {
            let (token, span) = self.lexer.lex();
            if token.is_eof() {
                self.ended = true;
            }
            Some((token, span))
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
            TokenStream::from_source("let")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::Let, Token::EoF],
            "let token"
        );
        assert_eq!(
            TokenStream::from_source(":").map(|(a, _)| a).collect_vec(),
            [Token::Colon, Token::EoF],
            "colon"
        );
        assert_eq!(
            TokenStream::from_source("1").map(|(a, _)| a).collect_vec(),
            vec![Token::Integer("1".to_owned()), Token::EoF],
            "int token"
        );
        assert_eq!(
            TokenStream::from_source("1.0")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::FloatingPoint("1.0".to_owned()), Token::EoF],
            "float token"
        );
        assert_eq!(
            TokenStream::from_source("=").map(|(a, _)| a).collect_vec(),
            vec![Token::Equals, Token::EoF],
            "equals token"
        );
        assert_eq!(
            TokenStream::from_source("'l'")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::CharLiteral("'l'".to_owned()), Token::EoF],
            "char token"
        );
        assert_eq!(
            TokenStream::from_source("\"let\"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::StringLiteral("\"let\"".to_owned()), Token::EoF],
            "string token"
        );
        assert_eq!(
            TokenStream::from_source("foo")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::Ident("foo".to_owned()), Token::EoF],
            "ident token"
        );
        assert_eq!(
            TokenStream::from_source("!=").map(|(a, _)| a).collect_vec(),
            vec![Token::Op("!=".to_owned()), Token::EoF],
            "op token"
        );

        assert_eq!(
            TokenStream::from_source("return")
                .map(|(a, _)| a)
                .collect_vec(),
            [Token::Return, Token::EoF],
            "return token"
        )
    }
    use itertools::Itertools;

    #[test]
    fn literals_edge_cases() {
        assert_eq!(
            TokenStream::from_source("\"foo bar \\\"baz\"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![
                Token::StringLiteral("\"foo bar \\\"baz\"".to_owned()),
                Token::EoF
            ]
        );
        assert_eq!(
            TokenStream::from_source("\"\"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::StringLiteral("\"\"".to_owned()), Token::EoF]
        );
        assert_eq!(
            TokenStream::from_source("\" \"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::StringLiteral("\" \"".to_owned()), Token::EoF]
        );

        assert_eq!(
            TokenStream::from_source("' '")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::CharLiteral("' '".to_owned()), Token::EoF]
        );
    }

    #[test]
    fn token_chain() {
        assert_eq!(
            TokenStream::from_source(
                "let foo =
\treturn 1.0

let bar : int32 = 1

let baz = \"foo bar baz\"
"
            )
            .map(|(a, _)| a)
            .collect_vec(),
            #[rustfmt::skip]
            [
                Token::Let,Token::Ident("foo".to_owned()),Token::Equals,
                Token::BeginBlock(4),
                    Token::Return, Token::FloatingPoint("1.0".to_owned()),
                Token::EndBlock(0),
                Token::Let, Token::Ident("bar".to_owned()), Token::Colon, Token::Ident("int32".to_owned()), Token::Equals, Token::Integer("1".to_owned()),
                Token::Let, Token::Ident("baz".to_owned()), Token::Equals, Token::StringLiteral("\"foo bar baz\"".to_owned()),
                Token::EoF,
            ]
        )
    }
}

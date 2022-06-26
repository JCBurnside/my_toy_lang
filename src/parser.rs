use std::{iter::Peekable, str::Chars};

use crate::{ast, lexer::TokenStream, tokens::Token};
#[derive(Debug)]
pub struct ParseError {
    span: (usize, usize),
    reason: ParseErrorReason,
}
#[derive(Debug)]
enum ParseErrorReason {
    InvalidIdent,
    IndentError,
    TypeError,
    ArgumentError,
    DeclarationError,
    UnexpectedEndOfFile,
    UnknownError, //internal use.  should basically never be hit.
}

pub type ParserResult = Result<crate::ast::Expr, ParseError>;

pub struct Parser<T> where TokenStream<T> : Iterator {
    stream: Peekable<TokenStream<T>>,
}

impl <'str> Parser<Peekable<Chars<'str>>> {
    #[cfg(test)]
    fn from_source(source: &'str str) -> Self {
        Self {
            stream: TokenStream::from_source(source).peekable(),
        }
    }
}

impl<T> Parser<T> where TokenStream<T> : Iterator<Item = (Token,usize)> {
    pub fn from_stream(stream: TokenStream<T>) -> Self {
        Self {
            stream: stream.peekable(),
        }
    }

    

    fn peek_expr(&self) -> Option<ParserResult> {
        None
    }

    fn next_expr(&mut self) -> ParserResult {
        match self.stream.peek() {
            Some((Token::Let, _)) => self.declaration(),
            Some((Token::BeginBlock, _)) => self.collect_block(),
            Some((
                Token::CharLiteral(_)
                | Token::StringLiteral(_)
                | Token::FloatingPoint(_,_)
                | Token::Integer(_,_),
                _,
            )) => self.literal(),
            Some((Token::Return, _)) => self.ret(),
            _ => Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            }),
        }
    }

    fn ret(&mut self) -> ParserResult {
        let (token, span) = self.stream.next().unwrap();
        if token == Token::Return {
            Ok(ast::Expr::Return {
                expr: Box::new(self.literal()?),
            })
        } else {
            Err(ParseError {
                span: (span, span),
                reason: ParseErrorReason::UnknownError,
            })
        }
    }

    fn literal(&mut self) -> ParserResult {
        let (token, span) = self.stream.next().unwrap();
        match token {
            Token::CharLiteral(ch) => Ok(ast::Expr::Literal {
                value: ch,
                ty: ast::Type::ValueType("char".to_owned()),
            }),
            Token::StringLiteral(s) => Ok(ast::Expr::Literal {
                value: s,
                ty: ast::Type::ValueType("str".to_owned()),
            }),
            Token::Integer(is_neg,i) => Ok(ast::Expr::Literal {
                value: if is_neg { "-".to_owned() + &i } else { i },
                ty: ast::Type::ValueType("int32".to_owned()),
            }),
            Token::FloatingPoint(is_neg,f) => Ok(ast::Expr::Literal {
                value:if is_neg { "-".to_owned() + &f } else { f },
                ty: ast::Type::ValueType("float32".to_owned()),
            }),
            _ => Err(ParseError {
                span: (span, 0),
                reason: ParseErrorReason::UnknownError,
            }),
        }
    }

    fn declaration(&mut self) -> ParserResult {
        if let Some((Token::Let, start)) = self.stream.next() {
            let (token, ident_span) = self.stream.next().unwrap();
            return match token {
                Token::Ident(ident)=> {
                    if let Some((Token::Colon,_next)) = self.stream.next()
                    && let Some((Token::Ident(ty),_ty_span)) = self.stream.next()
                    && let Some((Token::Op("=".to_owned()),_)) = self.stream.next() {
                        let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                            self.collect_block()?
                        } else {
                            self.literal()?
                        };
                        Ok(ast::Expr::Declaration { is_op: false, ident:ident, ty:Some(ast::Type::ValueType(ty)), args: None, value: Box::new(value)})
                    } else {
                        Err(ParseError{
                            span: (ident_span, ident.len() + ident_span),
                            reason : ParseErrorReason::DeclarationError,
                        })
                    }
                },
                Token::Op(ident) => {
                    if let Some((Token::Colon,_next)) = self.stream.next()
                    && let Some((Token::Ident(ty),_ty_span)) = self.stream.next()
                    && let Some((Token::Op("=".to_owned()),_)) = self.stream.next() {
                        let literal = self.literal()?;
                        Ok(ast::Expr::Declaration { is_op: true, ident:ident, ty:Some(ast::Type::ValueType(ty)), args: None, value: Box::new(literal) })
                    } else {
                        Err(ParseError{
                            span: (ident_span, ident.len() + ident_span),
                            reason : ParseErrorReason::DeclarationError,
                        })
                    }
                }
                _ => Err(ParseError{
                    span: (start, start + 3),
                    reason : ParseErrorReason::DeclarationError,
                })
            };
        }
        Err(ParseError {
            span: (0, 0),
            reason: ParseErrorReason::UnknownError,
        })
    }

    fn collect_block(&mut self) -> ParserResult {
        if let Some((Token::BeginBlock, span_start)) = self.stream.next() {
            let mut sub_expr = Vec::new();
            loop {
                if let Some((Token::EndBlock, span)) = self.stream.peek() {
                    self.stream.next();
                    break;
                } else if let Some((Token::EoF, _)) = self.stream.peek() {
                    break;
                }
                sub_expr.push(self.next_expr()?)
            }
            Ok(ast::Expr::Block { sub: sub_expr })
        } else {
            Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{self, Type};

    use super::Parser;

    #[test]
    fn individual_simple_expressions() {
        let mut parser = Parser::from_source("let foo : int32 = 5");
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(Type::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Literal {
                    value: "5".to_owned(),
                    ty: ast::Type::ValueType("int32".to_owned())
                })
            },
            parser.next_expr().expect("failed to parse")
        );
        let mut parser = Parser::from_source(
            "let foo : int32 =
\treturn 5+
",
        );
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ast::Type::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Block {
                    sub: vec![ast::Expr::Return {
                        expr: Box::new(ast::Expr::Literal {
                            value: "5".to_owned(),
                            ty: Type::ValueType("int32".to_owned())
                        })
                    }]
                })
            },
            parser.next_expr().expect("failed to parse")
        )
    }

    #[test]
    fn multiple_statements() {
        const SRC: &'static str = r###"let foo : int32 = 3

let bar : int32 =
    let baz : &str = "merp \" yes"
    return 2

let ^^ lhs rhs : int32 -> int32 -> int32 =
    return 1
"###;
        let mut parser = Parser::from_source(SRC);

        dbg!(&SRC[27..63]);
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ast::Type::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Literal {
                    value: "3".to_owned(),
                    ty: ast::Type::ValueType("int32".to_owned())
                }),
            },
            parser.next_expr().unwrap(),
            "simple declaration"
        );

        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(ast::Type::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Block {
                    sub: vec![
                        ast::Expr::Declaration {
                            is_op: false,
                            ident: "baz".to_owned(),
                            ty: Some(ast::Type::ValueType("&str".to_owned())),
                            args: None,
                            value: Box::new(ast::Expr::Literal {
                                value: "\"merp \\\" yes\"".to_owned(),
                                ty: ast::Type::ValueType("str".to_owned())
                            })
                        },
                        ast::Expr::Return {
                            expr: Box::new(ast::Expr::Literal {
                                value: "2".to_owned(),
                                ty: ast::Type::ValueType("int32".to_owned())
                            })
                        }
                    ]
                })
            },
            parser.next_expr().unwrap(),
            "declaration with block"
        )
    }
}

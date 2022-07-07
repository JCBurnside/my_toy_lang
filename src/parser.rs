use std::{iter::Peekable, str::Chars};

use itertools::Itertools;

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

pub struct Parser<T>
where
    TokenStream<T>: Iterator,
{
    stream: Peekable<TokenStream<T>>,
}

impl<'str> Parser<Peekable<Chars<'str>>> {
    #[cfg(test)]
    fn from_source(source: &'str str) -> Self {
        Self {
            stream: TokenStream::from_source(source).peekable(),
        }
    }
}

impl<T> Parser<T>
where
    TokenStream<T>: Iterator<Item = (Token, usize)>,
{
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
                | Token::FloatingPoint(_, _)
                | Token::Integer(_, _),
                _,
            )) => self.literal(),
            Some((Token::Ident(_), _)) => self.function_call(),
            Some((Token::Return, _)) => self.ret(),
            _ => Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            }),
        }
    }

    fn function_call(&mut self) -> ParserResult {
        if let Some((Token::Ident(name), span)) = self.stream.next() {
            let mut args = vec![];
            while let Some((
                Token::Ident(_)
                // |Token::Op(_)
                |Token::FloatingPoint(_, _)
                |Token::Integer(_,_)
                |Token::StringLiteral(_)
                |Token::CharLiteral(_)
                ,_
            )) = self.stream.peek() {
                args.push(match self.stream.peek().unwrap() {
                    (Token::Ident(_),_) => self.function_call()?, //technically should pass the function but adhoc for now.
                    (Token::Op(_),_) => todo!(), //idk how to handle this one yet. passing functions to functions
                    (
                        Token::Integer(_, _)
                        |Token::FloatingPoint(_, _)
                        |Token::CharLiteral(_)
                        |Token::StringLiteral(_)
                        ,_
                    ) => self.literal()?,
                    _=> return Err(ParseError{
                        span:(span,span+name.len()),
                        reason:ParseErrorReason::ArgumentError,
                    })
                });
            }
            Ok(ast::Expr::FnCall {
                ident: name,
                args: args,
            })
        } else {
            Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            })
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
                ty: ast::TypeName::ValueType("char".to_owned()),
            }),
            Token::StringLiteral(s) => Ok(ast::Expr::Literal {
                value: s,
                ty: ast::TypeName::ValueType("str".to_owned()),
            }),
            Token::Integer(is_neg, i) => Ok(ast::Expr::Literal {
                value: if is_neg { "-".to_owned() + &i } else { i },
                ty: ast::TypeName::ValueType("int32".to_owned()),
            }),
            Token::FloatingPoint(is_neg, f) => Ok(ast::Expr::Literal {
                value: if is_neg { "-".to_owned() + &f } else { f },
                ty: ast::TypeName::ValueType("float32".to_owned()),
            }),
            _ => Err(ParseError {
                span: (span, 0),
                reason: ParseErrorReason::UnknownError,
            }),
        }
    }

    pub(crate) fn collect_type(&mut self) -> Result<ast::TypeName, ParseError> {
        let ty = self.stream.next();
        if let Some((Token::Ident(ty), span)) = ty {
            if let Some((Token::Arrow, _)) = self.stream.peek() {
                self.stream.advance_by(1).unwrap();
                Ok(ast::TypeName::FnType(
                    Box::new(ast::TypeName::ValueType(ty)),
                    Box::new(self.collect_type()?),
                ))
            } else {
                Ok(ast::TypeName::ValueType(ty))
            }
        } else if let Some((Token::GroupOpen, span)) = ty {
            let ty = self.collect_type()?;
            if let Some((Token::GroupClose, _)) = self.stream.next() {
                Ok(ty)
            } else {
                Err(ParseError {
                    span: (span, 0),
                    reason: ParseErrorReason::TypeError,
                })
            }
        } else {
            Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::TypeError,
            })
        }
    }

    fn declaration(&mut self) -> ParserResult {
        if let Some((Token::Let, start)) = self.stream.next() {
            let (token, ident_span) = self.stream.next().unwrap();
            return match token {
                Token::Ident(ident) => {
                    let next = self.stream.next();
                    if let Some((Token::Colon, _next)) = next {
                        let ty = self.collect_type()?;
                        if let Some((Token::Op(eq),eq_span)) = self.stream.next()
                        && eq == "="
                        {
                            let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                self.collect_block()?
                            } else {
                                self.literal()?
                            };
                            Ok(ast::Expr::Declaration { is_op: false, ident, ty: Some(ty), args: None, value: Box::new(value)})
                        } else {
                            Err(ParseError{
                                span:(start,ident_span+ident.len()),
                                reason : ParseErrorReason::DeclarationError,
                            })
                        }
                    } else if let Some((Token::Ident(arg0), arg_start)) = next {
                        let mut args = vec![arg0];
                        while let Some((Token::Ident(_), _)) = self.stream.peek() {
                            if let (Token::Ident(arg), _) = self.stream.next().unwrap() {
                                args.push(arg);
                            } else {
                                return Err(ParseError {
                                    span: (0, 0),
                                    reason: ParseErrorReason::ArgumentError,
                                });
                            }
                        }
                        if let Some((Token::Colon, _next)) = self.stream.next() {
                            let ty = self.collect_type()?;
                            if let Some((Token::Op(eq),eq_span)) = self.stream.next()
                            && eq == "="
                            {
                                let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                    self.collect_block()?
                                } else {
                                    self.literal()?
                                };
                                Ok(ast::Expr::Declaration { is_op: false, ident, ty: Some(ty), args: Some(args.into_iter().map(|s| (s,None)).collect()), value: Box::new(value)})
                            } else {
                                Err(ParseError{
                                    span:(start,ident_span+ident.len()),
                                    reason : ParseErrorReason::DeclarationError,
                                })
                            }
                        } else {
                            if let Some((Token::Op(eq),eq_span)) = self.stream.next()
                            && eq == "=" {
                                let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                    self.collect_block()?
                                } else {
                                    self.literal()?
                                };
                                Ok(ast::Expr::Declaration {
                                    is_op: false,
                                    ident,
                                    ty: None,
                                    args: Some(args.into_iter().map(|s| (s,None)).collect()),
                                    value: Box::new(value)
                                })
                            } else {
                                Err(ParseError {
                                    span:(ident_span,ident_span+ident.len()),
                                    reason:ParseErrorReason::DeclarationError,
                                })
                            }
                        }
                    } else {
                        Err(ParseError {
                            span: (ident_span, ident.len() + ident_span),
                            reason: ParseErrorReason::DeclarationError,
                        })
                    }
                }
                Token::Op(ident) => {
                    let next = self.stream.next();
                    if let Some((Token::Colon, _next)) = next {
                        let ty = self.collect_type()?;
                        if let Some((Token::Op(eq),eq_span)) = self.stream.next()
                        && eq == "="
                        {
                            let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                self.collect_block()?
                            } else {
                                self.literal()?
                            };
                            Ok(ast::Expr::Declaration { is_op: true, ident, ty: Some(ty), args: None, value: Box::new(value)})
                        } else {
                            Err(ParseError{
                                span:(start,ident_span+ident.len()),
                                reason : ParseErrorReason::DeclarationError,
                            })
                        }
                    } else if let Some((Token::Ident(arg0), arg_start)) = next {
                        let mut args = vec![arg0];
                        while let Some((Token::Ident(_), _)) = self.stream.peek() {
                            if let (Token::Ident(arg), _) = self.stream.next().unwrap() {
                                args.push(arg);
                            } else {
                                return Err(ParseError {
                                    span: (0, 0),
                                    reason: ParseErrorReason::ArgumentError,
                                });
                            }
                        }
                        if let Some((Token::Colon, _next)) = self.stream.next() {
                            let ty = self.collect_type()?;
                            if let Some((Token::Op(eq),eq_span)) = self.stream.next()
                            && eq == "="
                            {
                                let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                    self.collect_block()?
                                } else {
                                    self.literal()?
                                };
                                Ok(ast::Expr::Declaration { is_op: true, ident, ty: Some(ty), args: Some(args.into_iter().map(|arg|(arg,None)).collect()), value: Box::new(value)})
                            } else {
                                Err(ParseError{
                                    span:(start,ident_span+ident.len()),
                                    reason : ParseErrorReason::DeclarationError,
                                })
                            }
                        } else {
                            if let Some((Token::Op(eq),eq_span)) = self.stream.next()
                            && eq == "=" {
                                let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                    self.collect_block()?
                                } else {
                                    self.literal()?
                                };
                                Ok(ast::Expr::Declaration { is_op: true, ident, ty: None, args: Some(args.into_iter().map(|s| (s,None)).collect()), value: Box::new(value)})
                            } else {
                                Err(ParseError {
                                    span:(ident_span,ident_span+ident.len()),
                                    reason:ParseErrorReason::DeclarationError,
                                })
                            }
                        }
                    } else {
                        Err(ParseError {
                            span: (ident_span, ident.len() + ident_span),
                            reason: ParseErrorReason::DeclarationError,
                        })
                    }
                }
                _ => Err(ParseError {
                    span: (start, start + 3),
                    reason: ParseErrorReason::DeclarationError,
                }),
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
    use crate::ast::{self, TypeName};

    use super::Parser;

    #[test]
    fn individual_simple_expressions() {
        let mut parser = Parser::from_source("let foo : int32 = 5");

        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(TypeName::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Literal {
                    value: "5".to_owned(),
                    ty: ast::TypeName::ValueType("int32".to_owned())
                })
            },
            parser.next_expr().expect("failed to parse")
        );
        let mut parser = Parser::from_source(
            "let foo : int32 =
\treturn 5
",
        );
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ast::TypeName::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Block {
                    sub: vec![ast::Expr::Return {
                        expr: Box::new(ast::Expr::Literal {
                            value: "5".to_owned(),
                            ty: TypeName::ValueType("int32".to_owned())
                        })
                    }]
                })
            },
            parser.next_expr().expect("failed to parse")
        )
    }

    #[test]
    fn multiple_statements() {
        const SRC: &'static str = include_str!("../samples/test.foo");
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ast::TypeName::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Literal {
                    value: "3".to_owned(),
                    ty: ast::TypeName::ValueType("int32".to_owned())
                }),
            },
            parser.next_expr().unwrap(),
            "simple declaration"
        );
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(ast::TypeName::ValueType("int32".to_owned())),
                args: None,
                value: Box::new(ast::Expr::Block {
                    sub: vec![
                        ast::Expr::Declaration {
                            is_op: false,
                            ident: "baz".to_owned(),
                            ty: Some(ast::TypeName::ValueType("str".to_owned())),
                            args: None,
                            value: Box::new(ast::Expr::Literal {
                                value: "merp \\\" yes".to_owned(),
                                ty: ast::TypeName::ValueType("str".to_owned())
                            })
                        },
                        ast::Expr::Return {
                            expr: Box::new(ast::Expr::Literal {
                                value: "2".to_owned(),
                                ty: ast::TypeName::ValueType("int32".to_owned())
                            })
                        }
                    ]
                })
            },
            parser.next_expr().unwrap(),
            "declaration with block"
        );
        assert_eq!(
            ast::Expr::Declaration {
                is_op: true,
                ident: "^^".to_owned(),
                ty: Some(ast::TypeName::FnType(
                    Box::new(ast::TypeName::ValueType("int32".to_owned())),
                    Box::new(ast::TypeName::FnType(
                        Box::new(ast::TypeName::ValueType("int32".to_owned())),
                        Box::new(ast::TypeName::ValueType("int32".to_owned())),
                    )),
                )),
                args: Some(vec![("lhs".to_string(), None), ("rhs".to_string(), None)]),
                value: Box::new(ast::Expr::Block {
                    sub: vec![
                        ast::Expr::FnCall {
                            ident: "bar".to_owned(),
                            args: vec![ast::Expr::FnCall {
                                ident: "foo".to_owned(),
                                args: vec![]
                            }],
                        },
                        ast::Expr::Return {
                            expr: Box::new(ast::Expr::Literal {
                                value: "1".to_owned(),
                                ty: ast::TypeName::ValueType("int32".to_owned())
                            })
                        }
                    ]
                }),
            },
            parser.next_expr().unwrap(),
            "operator declaration w/ function call",
        );
    }
}

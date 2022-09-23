use std::{iter::Peekable, str::Chars};

use crate::{ast, lexer::TokenStream, tokens::Token, util::ExtraUtilFunctions};
#[derive(Debug)]
#[allow(unused)]
pub struct ParseError {
    span: (usize, usize),
    reason: ParseErrorReason,
}
#[derive(Debug)]
#[allow(unused)]
enum ParseErrorReason {
    InvalidIdent,
    IndentError,
    TypeError,
    ArgumentError,
    DeclarationError,
    UnexpectedEndOfFile,
    UnsupportedEscape,
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

    pub fn has_next(&mut self) -> bool {
        self.stream
            .peek()
            .map_or(false, |(token, _)| !token.is_eof())
    }

    pub fn next_expr(&mut self) -> ParserResult {
        match dbg!(self.stream.peek()) {
            Some((Token::NewLine, _)) => {
                self.stream.next();
                self.next_expr()
            }
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
                | Token::FloatingPoint(_, _)
                | Token::Integer(_, _)
                | Token::StringLiteral(_)
                | Token::CharLiteral(_),
                _,
            )) = self.stream.peek()
            {
                args.push(match self.stream.peek().unwrap() {
                    (Token::Ident(_), _) => self.function_call()?, //technically should pass the function but adhoc for now.
                    (Token::Op(_), _) => todo!(), //idk how to handle this one yet. passing functions to functions
                    (
                        Token::Integer(_, _)
                        | Token::FloatingPoint(_, _)
                        | Token::CharLiteral(_)
                        | Token::StringLiteral(_),
                        _,
                    ) => self.literal()?,
                    _ => {
                        return Err(ParseError {
                            span: (span, span + name.len()),
                            reason: ParseErrorReason::ArgumentError,
                        })
                    }
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
                expr: self.next_expr()?.boxed(),
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
            Token::StringLiteral(src) => {
                let mut value = String::with_capacity(src.len());
                let mut s = src.chars();
                let mut idx = 0;
                while let Some(c) = s.next() {
                    if c == '\\' {
                        value.push(match s.next() {
                            Some('n') => '\n',
                            Some('\\') => '\\',
                            Some('r') => '\r',
                            Some('t') => '\t',
                            Some(_) => {
                                return Err(ParseError {
                                    span: (span, span + src.len()),
                                    reason: ParseErrorReason::UnsupportedEscape,
                                })
                            }
                            None => unreachable!(),
                        })
                    } else {
                        value.push(c);
                    }
                    idx += 1;
                }
                Ok(ast::Expr::Literal {
                    value,
                    ty: ast::TypeName::ValueType("str".to_owned()),
                })
            }
            Token::Integer(is_neg, i) => Ok(ast::Expr::Literal {
                value: if is_neg { "-".to_owned() + &i } else { i },
                // TODO : figure out appropriate type.
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
                let result = self.collect_type()?;
                Ok(ast::TypeName::FnType(
                    ast::TypeName::ValueType(ty).into_rc(),
                    result.into_rc(),
                ))
            } else {
                Ok(ast::TypeName::ValueType(ty))
            }
        } else if let Some((Token::GroupOpen, span)) = ty {
            let ty = self.collect_type()?;
            if let Some((Token::GroupClose, _)) = self.stream.next() {
                if let Some((Token::Arrow, _)) = self.stream.peek() {
                    self.stream.advance_by(1).unwrap();
                    let result = self.collect_type()?;
                    Ok(ast::TypeName::FnType(ty.into_rc(), result.into_rc()))
                } else {
                    Ok(ty)
                }
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
                            let value = self.next_expr()?;
                            Ok(ast::Expr::Declaration { is_op: false, ident, ty: Some(ty), args: None, value: value.boxed()})
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
                        if let Some((Token::Colon, _next)) = self.stream.peek() {
                            self.stream.next();
                            let ty = self.collect_type()?;
                            let next = self.stream.next();
                            if let Some((Token::Op(eq),eq_span)) = next
                            && eq == "="
                            {
                                let value = self.next_expr()?;
                                Ok(ast::Expr::Declaration { is_op: false, ident, ty: Some(ty), args: Some(args.into_iter().map(|s| (s,None)).collect()), value: value.boxed()})
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
                                    value: value.boxed()
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
                            let value = self.next_expr()?;
                            Ok(ast::Expr::Declaration { is_op: false, ident, ty: Some(ty), args: None, value: value.boxed()})
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
                        if let Some((Token::Colon, _next)) = self.stream.peek() {
                            self.stream.next();
                            let ty = self.collect_type()?;
                            let next = self.stream.next();
                            if let Some((Token::Op(eq),eq_span)) = next
                            && eq == "="
                            {
                                let value = self.next_expr()?;
                                Ok(ast::Expr::Declaration { is_op: true, ident, ty: Some(ty), args: Some(args.into_iter().map(|s| (s,None)).collect()), value: value.boxed()})
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
                                    is_op: true,
                                    ident,
                                    ty: None,
                                    args: Some(args.into_iter().map(|s| (s,None)).collect()),
                                    value: value.boxed()
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
                let next = self.stream.peek();
                if let Some((Token::EndBlock, span)) = next {
                    self.stream.next();
                    break;
                } else if let Some((Token::EoF, _)) = next {
                    break;
                } else if let Some((Token::NewLine, _)) = next {
                    self.stream.next();
                    continue;
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
    use super::Parser;
    use crate::ast::{self, TypeName};
    use crate::util::ExtraUtilFunctions;

    #[test]
    fn individual_simple_expressions() {
        let mut parser = Parser::from_source("let foo : int32 = 5");

        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(TypeName::ValueType("int32".to_owned())),
                args: None,
                value: ast::Expr::Literal {
                    value: "5".to_owned(),
                    ty: ast::TypeName::ValueType("int32".to_owned())
                }
                .boxed()
            },
            parser.next_expr().expect("failed to parse")
        );
        let mut parser = Parser::from_source(
            r#"let foo : int32 =
    return 5
"#,
        );
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ast::TypeName::ValueType("int32".to_owned())),
                args: None,
                value: ast::Expr::Block {
                    sub: vec![ast::Expr::Return {
                        expr: ast::Expr::Literal {
                            value: "5".to_owned(),
                            ty: TypeName::ValueType("int32".to_owned())
                        }
                        .boxed()
                    }]
                }
                .boxed()
            },
            parser.next_expr().expect("failed to parse")
        )
    }

    #[test]
    fn higher_kinded() {
        const ARG: &'static str = r#"
let foo _ : ( int32 -> int32 ) -> int32 =
    return 0
"#;
        let mut parser = Parser::from_source(ARG);
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(TypeName::FnType(
                    TypeName::FnType(
                        TypeName::ValueType("int32".to_owned()).into_rc(),
                        TypeName::ValueType("int32".to_owned()).into_rc()
                    )
                    .into_rc(),
                    TypeName::ValueType("int32".to_owned()).into_rc()
                )),
                args: Some(vec![("_".to_owned(), None)]),
                value: ast::Expr::Block {
                    sub: vec![ast::Expr::Return {
                        expr: ast::Expr::Literal {
                            value: "0".to_owned(),
                            ty: TypeName::ValueType("int32".to_owned())
                        }
                        .boxed()
                    }]
                }
                .boxed()
            },
            parser.next_expr().expect("failed to parse"),
            "function as arg"
        );
        const RT: &'static str = r#"
let foo _ : int32 -> ( int32 -> int32 ) =
    return 0
"#;
        let mut parser = Parser::from_source(RT);
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(TypeName::FnType(
                    TypeName::ValueType("int32".to_owned()).into_rc(),
                    TypeName::FnType(
                        TypeName::ValueType("int32".to_owned()).into_rc(),
                        TypeName::ValueType("int32".to_owned()).into_rc()
                    )
                    .into_rc(),
                )),
                args: Some(vec![("_".to_owned(), None)]),
                value: ast::Expr::Block {
                    sub: vec![ast::Expr::Return {
                        expr: ast::Expr::Literal {
                            value: "0".to_owned(),
                            ty: TypeName::ValueType("int32".to_owned())
                        }
                        .boxed()
                    }]
                }
                .boxed()
            },
            parser.next_expr().expect("failed to parse"),
            "function as rt"
        );
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
                value: ast::Expr::Literal {
                    value: "3".to_owned(),
                    ty: ast::TypeName::ValueType("int32".to_owned())
                }
                .boxed(),
            },
            parser.next_expr().unwrap(),
            "simple declaration"
        );
        assert_eq!(
            ast::Expr::Declaration {
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(ast::TypeName::FnType(
                    ast::TypeName::ValueType("int32".to_owned()).into_rc(),
                    ast::TypeName::ValueType("int32".to_owned()).into_rc()
                )),
                args: Some(vec![("quz".to_owned(), None)]),
                value: ast::Expr::Block {
                    sub: vec![
                        ast::Expr::Declaration {
                            is_op: false,
                            ident: "baz".to_owned(),
                            ty: Some(ast::TypeName::ValueType("str".to_owned())),
                            args: None,
                            value: ast::Expr::Literal {
                                value: "merp \\\" yes".to_owned(),
                                ty: ast::TypeName::ValueType("str".to_owned())
                            }
                            .boxed()
                        },
                        ast::Expr::Return {
                            expr: ast::Expr::Literal {
                                value: "2".to_owned(),
                                ty: ast::TypeName::ValueType("int32".to_owned())
                            }
                            .boxed()
                        }
                    ]
                }
                .boxed()
            },
            parser.next_expr().unwrap(),
            "declaration with block"
        );
        assert_eq!(
            ast::Expr::Declaration {
                is_op: true,
                ident: "^^".to_owned(),
                ty: Some(ast::TypeName::FnType(
                    ast::TypeName::ValueType("int32".to_owned()).into_rc(),
                    ast::TypeName::FnType(
                        ast::TypeName::ValueType("int32".to_owned()).into_rc(),
                        ast::TypeName::ValueType("int32".to_owned()).into_rc(),
                    )
                    .into_rc(),
                )),
                args: Some(vec![("lhs".to_string(), None), ("rhs".to_string(), None)]),
                value: ast::Expr::Block {
                    sub: vec![
                        ast::Expr::FnCall {
                            ident: "bar".to_owned(),
                            args: vec![ast::Expr::FnCall {
                                ident: "foo".to_owned(),
                                args: vec![]
                            }],
                        },
                        ast::Expr::Return {
                            expr: ast::Expr::Literal {
                                value: "1".to_owned(),
                                ty: ast::TypeName::ValueType("int32".to_owned())
                            }
                            .boxed()
                        }
                    ]
                }
                .boxed(),
            },
            parser.next_expr().unwrap(),
            "operator declaration w/ function call",
        );
    }

    #[test]
    fn fn_cain() {
        const SRC: &'static str = r#"
let main _ : int32 -> int32 = 
    put_int32 100
    print_str "v"
    return 32
"#;
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            parser.next_expr().unwrap(),
            ast::Expr::Declaration {
                is_op: false,
                ident: "main".to_string(),
                ty: Some(TypeName::FnType(
                    TypeName::ValueType("int32".to_owned()).into_rc(),
                    TypeName::ValueType("int32".to_owned()).into_rc(),
                )),
                args: Some(vec![("_".to_owned(), None)]),
                value: ast::Expr::Block {
                    sub: vec![
                        ast::Expr::FnCall {
                            ident: "put_int32".to_owned(),
                            args: vec![ast::Expr::Literal {
                                value: "100".to_owned(),
                                ty: TypeName::ValueType("int32".to_owned())
                            }]
                        },
                        ast::Expr::FnCall {
                            ident: "print_str".to_owned(),
                            args: vec![ast::Expr::Literal {
                                value: "v".to_owned(),
                                ty: TypeName::ValueType("str".to_owned())
                            }]
                        },
                        ast::Expr::Return {
                            expr: ast::Expr::Literal {
                                value: "32".to_owned(),
                                ty: TypeName::ValueType("int32".to_owned())
                            }
                            .boxed()
                        }
                    ]
                }
                .boxed()
            }
        )
    }
}

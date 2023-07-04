use std::{collections::HashSet, iter::Peekable, str::Chars};

use itertools::Itertools;

use crate::{
    ast::{self, BinaryOpCall, Expr, FnCall, Statement, ValueDeclaration, ValueType},
    lexer::TokenStream,
    tokens::Token,
    types::{self, ResolvedType},
    util::ExtraUtilFunctions,
};

#[derive(Debug)]
#[allow(unused)]
pub struct ParseError {
    span: (usize, usize),
    reason: ParseErrorReason,
}
#[derive(Debug)]
#[allow(unused)]
enum ParseErrorReason {
    UnbalancedBraces,
    InvalidIdent,
    IndentError,
    TypeError,
    ArgumentError,
    DeclarationError,
    UnexpectedEndOfFile,
    UnsupportedEscape,
    UnknownError, //internal use.  should basically never be hit.
}

pub(crate) struct Parser<T: Clone>
where
    T: Iterator<Item = (Token, crate::Location)>,
{
    stream: Peekable<T>,
}

impl<'str> Parser<TokenStream<Peekable<Chars<'str>>>> {
    #[cfg(test)]
    pub(crate) fn from_source(source: &'str str) -> Self {
        Self {
            stream: TokenStream::from_source(source).peekable(),
        }
    }
}

impl<T: Clone> Parser<T>
where
    T: Iterator<Item = (Token, crate::Location)> + Clone,
{
    pub fn from_stream(stream: T) -> Self {
        Self {
            stream: stream.peekable(),
        }
    }

    pub fn has_next(&mut self) -> bool {
        self.stream
            .peek()
            .map_or(false, |(token, _)| !token.is_eof())
    }
    pub(crate) fn module(mut self, name: String) -> ast::ModuleDeclaration {
        let mut decls = Vec::new();
        while self.has_next() {
            match self.declaration() {
                Ok(decl) => decls.push(decl),
                Err(e) => println!("{:?}", e),
            }
        }
        ast::ModuleDeclaration {
            loc: None,
            name: Some(name),
            declarations: decls,
        }
    }
    pub fn next_statement(&mut self) -> Result<Statement, ParseError> {
        match self.stream.clone().next() {
            Some((Token::NewLine, _)) => {
                self.stream.next();
                self.next_statement()
            }
            Some((Token::Let, _)) | Some((Token::For, _)) => {
                Ok(Statement::Declaration(self.fn_declaration()?))
            }
            Some((Token::Return, _)) => self.ret(),
            Some((Token::Ident(_), _)) => Ok(Statement::FnCall(self.function_call()?.0)), // hmmm should handle if it's actually a binary op call esp and/or compose.
            _ => unreachable!("how?"),
        }
    }

    pub fn next_expr(&mut self) -> Result<(crate::ast::Expr, crate::Location), ParseError> {
        let cloned_stream = self.stream.clone();
        #[cfg(debug_assertions)]
        let _test = self.stream.peek();
        match self.stream.clone().next() {
            Some((Token::GroupOpen, loc))
                if matches!(self.stream.clone().nth(1), Some((Token::GroupClose, _))) =>
            {
                self.stream.next();
                self.stream.next();
                return Ok((Expr::UnitLiteral, loc));
            }
            Some((Token::GroupOpen, _)) => {
                self.stream.next();
                let out = self.next_expr()?;
                if let Some((Token::GroupClose, _)) = self.stream.peek() {
                    self.stream.next();
                }
                return Ok(out);
            }
            Some((Token::NewLine, _)) => {
                self.stream.next();
                self.next_expr()
            }
            Some((
                Token::Integer(_, _)
                | Token::FloatingPoint(_, _)
                | Token::CharLiteral(_)
                | Token::StringLiteral(_)
                | Token::Ident(_),
                _,
            )) if match cloned_stream.clone().nth(1) {
                Some((Token::Op(_), _)) => true,
                _ => false,
            } =>
            {
                self.binary_op()
            }
            Some((
                Token::CharLiteral(_)
                | Token::StringLiteral(_)
                | Token::FloatingPoint(_, _)
                | Token::Integer(_, _),
                loc,
            )) => Ok((self.literal()?, loc)),
            Some((Token::Ident(_), _)) => {
                if let Some((
                    Token::Ident(_)
                    | Token::GroupOpen
                    | Token::StringLiteral(_)
                    | Token::CharLiteral(_)
                    | Token::FloatingPoint(_, _)
                    | Token::Integer(_, _),
                    _,
                )) = self.stream.clone().nth(1)
                {
                    let (call, loc) = self.function_call()?;
                    Ok((Expr::FnCall(call), loc))
                } else {
                    self.value()
                }
            }
            _ => Err(ParseError {
                span: (100000, 0),
                reason: ParseErrorReason::UnknownError,
            }),
        }
    }

    fn value(&mut self) -> Result<(Expr, crate::Location), ParseError> {
        if let Some((Token::Ident(ident), loc)) = self.stream.next() {
            Ok((ast::Expr::ValueRead(ident), loc))
        } else {
            Err(ParseError {
                span: (100, 100),
                reason: ParseErrorReason::UnknownError,
            })
        }
    }

    fn function_call(&mut self) -> Result<(FnCall, crate::Location), ParseError> {
        if let Some((Token::Ident(ident), loc)) = self.stream.next() {
            let mut values = Vec::<(ast::Expr, (usize, usize))>::new();
            while let Some((
                | Token::GroupOpen
                | Token::Ident(_)
                // | Token::Op(_) // TODO! this will need some special handling.  ie for `foo bar >>> baz`  should that be parsed as `(foo bar) >>> baz` or `foo (bar >>> baz)`
                | Token::Integer(_,_)
                | Token::FloatingPoint(_, _)
                | Token::CharLiteral(_)
                | Token::StringLiteral(_)
                ,_
            )) = self.stream.peek() {
                match self.stream.peek().map(|(a,_)| a) {
                    Some(Token::Ident(_)) => {
                        let test = self.stream.clone().nth(1);
                        if let Some((Token::Op(_),_)) = test {
                            values.push(self.binary_op()?)
                        } else {
                            values.push(self.value()?)
                        }
                    }
                    Some(Token::GroupOpen) => values.push(self.next_expr()?),
                    Some(
                        | Token::Integer(_,_)
                        | Token::FloatingPoint(_, _)
                        | Token::CharLiteral(_)
                        | Token::StringLiteral(_)
                    ) => {
                        let test = self.stream.clone().nth(1);
                        if let Some((
                            | Token::Ident(_)
                            | Token::Integer(_,_)
                            | Token::FloatingPoint(_, _)
                            | Token::CharLiteral(_)
                            | Token::StringLiteral(_)
                        ,loc)) = test {
                            values.push((self.literal()?,loc))
                        } else if let Some((Token::Op(_),_)) = test {
                            values.push(self.binary_op()?)
                        } else {
                            values.push((self.literal()?,loc))
                        }
                    },
                    _ => unreachable!()
                    // TODO! add in operator case special handling
                }
            }
            Ok(values.into_iter().fold(
                (
                    FnCall {
                        loc,
                        value: ast::Expr::ValueRead(ident).boxed(),
                        arg: None,
                    },
                    loc,
                ),
                |inner, (next, loc)| {
                    if let FnCall {
                        value, arg: None, ..
                    } = inner.0
                    {
                        (
                            FnCall {
                                loc,
                                value,
                                arg: Some(next.boxed()),
                            },
                            loc,
                        )
                    } else {
                        (
                            FnCall {
                                loc,
                                value: Expr::FnCall(inner.0).boxed(),
                                arg: Some(next.boxed()),
                            },
                            loc,
                        )
                    }
                },
            ))
        } else {
            Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            })
        }
    }

    fn ret(&mut self) -> Result<Statement, ParseError> {
        let (token, span) = self.stream.next().unwrap();
        if token == Token::Return {
            Ok(ast::Statement::Return(self.next_expr()?.0, span))
        } else {
            Err(ParseError {
                span,
                reason: ParseErrorReason::UnknownError,
            })
        }
    }

    fn literal(&mut self) -> Result<crate::ast::Expr, ParseError> {
        let (token, span) = self.stream.next().unwrap();
        make_literal(token, span)
    }

    pub(crate) fn collect_type(&mut self) -> Result<ResolvedType, ParseError> {
        let ty = self.stream.next();
        if let Some((Token::Ident(ty), span)) = ty {
            if let Some((Token::Arrow, _)) = self.stream.peek() {
                self.stream.next();
                let result = self.collect_type()?;
                let ty = type_from_string(&ty);
                Ok(ResolvedType::Function {
                    arg: ty.boxed(),
                    returns: result.boxed(),
                })
            } else {
                let ty = type_from_string(&ty);
                Ok(ty)
            }
        } else if let Some((Token::GroupOpen, span)) = ty {
            if let Some((Token::GroupClose, _)) = self.stream.peek() {
                self.stream.next();
                return if let Some((Token::Arrow, _)) = self.stream.peek() {
                    self.stream.next();
                    Ok(ResolvedType::Function {
                        arg: types::UNIT.boxed(),
                        returns: self.collect_type()?.boxed(),
                    })
                } else {
                    Ok(types::UNIT)
                };
            }
            let ty = self.collect_type()?;
            if let Some((Token::GroupClose, _)) = self.stream.next() {
                if let Some((Token::Arrow, _)) = self.stream.peek() {
                    self.stream.next();
                    let result = self.collect_type()?;
                    Ok(ResolvedType::Function {
                        arg: ty.boxed(),
                        returns: result.boxed(),
                    })
                } else {
                    Ok(ty)
                }
            } else {
                Err(ParseError {
                    span: span,
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

    fn pipe(&mut self) -> Result<Expr, ParseError> {
        todo!("pipes")
    }

    fn compose(&mut self) -> Result<Expr, ParseError> {
        todo!("compose")
    }

    fn declaration(&mut self) -> Result<ast::Declaration, ParseError> {
        let next = self.stream.peek();
        match next {
            Some((Token::For | Token::Let, _)) => {
                Ok(ast::Declaration::Value(self.fn_declaration()?))
            }
            Some((Token::NewLine, _)) => {
                self.stream.next();
                self.declaration()
            }
            _ => unimplemented!(),
        }
    }

    fn fn_declaration(&mut self) -> Result<ValueDeclaration, ParseError> {
        let generics = if let Some((Token::For, _)) = self.stream.peek() {
            let Some((_,span)) = self.stream.next() else { unreachable!() };
            match self.stream.next() {
                Some((Token::Op(op), _)) if op == "<" => {
                    let mut out = self
                        .stream
                        .clone()
                        .take_while(|(token, _)| &Token::Op(">".to_string()) != token)
                        .collect_vec();
                    let first_span = out.first().unwrap().1;
                    let last_span = out.last().unwrap().1;
                    let num = out.len();
                    out.dedup();
                    if out.len() == num {
                        out.into_iter()
                            .filter_map(|(t, _)| {
                                let Token::Ident(name) = t else { return None };
                                Some(name)
                            })
                            .collect()
                    } else {
                        return Err(ParseError {
                            span: first_span,
                            reason: ParseErrorReason::DeclarationError,
                        });
                    }
                }
                _ => {
                    let (_, end) = self
                        .stream
                        .clone()
                        .take_while(|(token, span)| token != &Token::Let)
                        .last()
                        .unwrap();
                    return Err(ParseError {
                        span,
                        reason: ParseErrorReason::DeclarationError,
                    });
                }
            }
        } else {
            HashSet::new()
        };
        if generics.len() != 0 {
            // self.stream.advance_by(generics.len() + 1).unwrap();
            for _ in 0..(generics.len() + 1) {
                self.stream.next();
            }
        }
        if let Some((Token::Let, start)) = self.stream.next() {
            let (token, ident_span) = self.stream.next().unwrap();
            return match token {
                Token::Ident(ident) => {
                    let next = self.stream.next();
                    if let Some((Token::Colon, _next)) = next {
                        let ty = self.collect_type()?;
                        match self.stream.next() {
                            Some((Token::Op(eq), eq_span)) if eq == "=" => {
                                self.stream
                                    .peeking_take_while(|(t, _)| t == &Token::NewLine)
                                    .collect_vec();
                                let value = match self.stream.clone().nth(1) {
                                    Some((Token::BeginBlock, _)) => {
                                        ValueType::Function(self.collect_block()?)
                                    }
                                    Some((Token::Compose, _)) => ValueType::Expr(self.compose()?),
                                    Some((Token::Op(op), _)) if is_pipe_op(&op) => {
                                        ValueType::Expr(self.pipe()?)
                                    }
                                    _ => ValueType::Expr(self.next_expr()?.0),
                                };
                                Ok(ValueDeclaration {
                                    loc: ident_span,
                                    is_op: false,
                                    ident,
                                    ty: Some(ty),
                                    args: Vec::new(),
                                    value,
                                    generictypes: HashSet::new(),
                                })
                            }
                            _ => Err(ParseError {
                                span: ident_span,
                                reason: ParseErrorReason::DeclarationError,
                            }),
                        }
                    } else if let Some((Token::Ident(arg0), arg_start)) = next {
                        let mut args = vec![ast::ArgDeclation {
                            ident: arg0,
                            loc: arg_start,
                        }];
                        while let Some((Token::Ident(_), _)) = self.stream.peek() {
                            if let (Token::Ident(arg), loc) = self.stream.next().unwrap() {
                                args.push(ast::ArgDeclation { loc, ident: arg });
                            } else {
                                return Err(ParseError {
                                    span: (0, 0),
                                    reason: ParseErrorReason::ArgumentError,
                                });
                            }
                        }
                        let count = args.len();
                        args.dedup();
                        if count != args.len() {
                            return Err(ParseError {
                                span: (0, 0),
                                reason: ParseErrorReason::IndentError,
                            });
                        }
                        if let Some((Token::Colon, _next)) = self.stream.peek() {
                            self.stream.next();
                            let ty = self.collect_type()?;
                            let ty = generics
                                .iter()
                                .fold(ty, |ty, name| ty.replace_user_with_generic(&name));
                            let next = self.stream.next();
                            match next {
                                Some((Token::Op(eq), eq_span)) if eq == "=" => {
                                    let consumed = self
                                        .stream
                                        .clone()
                                        .take_while(|(t, _)| t == &Token::NewLine)
                                        .collect_vec();
                                    for _ in 0..consumed.len() {
                                        self.stream.next();
                                    }
                                    let value = match self.stream.clone().nth(0) {
                                        Some((Token::BeginBlock, _)) => {
                                            ValueType::Function(self.collect_block()?)
                                        }
                                        Some((Token::Compose, _)) => {
                                            ValueType::Expr(self.compose()?)
                                        }
                                        Some((Token::Op(op), _)) if is_pipe_op(&op) => {
                                            ValueType::Expr(self.pipe()?)
                                        }
                                        _ => ValueType::Expr(self.next_expr()?.0),
                                    };
                                    Ok(ValueDeclaration {
                                        loc: ident_span,
                                        is_op: false,
                                        ident,
                                        ty: Some(ty),
                                        args,
                                        value,
                                        generictypes: generics,
                                    })
                                }
                                _ => Err(ParseError {
                                    span: start,
                                    reason: ParseErrorReason::DeclarationError,
                                }),
                            }
                        } else {
                            match self.stream.next() {
                                Some((Token::Op(eq), eq_span)) if eq == "=" => {
                                    let consumed = self
                                        .stream
                                        .clone()
                                        .take_while(|(t, _)| t == &Token::NewLine)
                                        .collect_vec();
                                    for _ in 0..consumed.len() {
                                        self.stream.next();
                                    }
                                    let value = match self.stream.clone().nth(0) {
                                        Some((Token::BeginBlock, _)) => {
                                            ValueType::Function(self.collect_block()?)
                                        }
                                        Some((Token::Compose, _)) => {
                                            ValueType::Expr(self.compose()?)
                                        }
                                        Some((Token::Op(op), _)) if is_pipe_op(&op) => {
                                            ValueType::Expr(self.pipe()?)
                                        }
                                        _ => ValueType::Expr(self.next_expr()?.0),
                                    };
                                    Ok(ValueDeclaration {
                                        loc: ident_span,
                                        is_op: false,
                                        ident,
                                        ty: None,
                                        args,
                                        value: value,
                                        generictypes: generics,
                                    })
                                }
                                _ => Err(ParseError {
                                    span: ident_span,
                                    reason: ParseErrorReason::DeclarationError,
                                }),
                            }
                        }
                    } else {
                        Err(ParseError {
                            span: ident_span,
                            reason: ParseErrorReason::DeclarationError,
                        })
                    }
                }
                Token::Op(ident) => {
                    let next = self.stream.next();
                    if let Some((Token::Colon, _next)) = next {
                        let ty = self.collect_type()?;
                        match self.stream.next() {
                            Some((Token::Op(eq), eq_span)) if eq == "=" => {
                                let consumed = self
                                    .stream
                                    .clone()
                                    .take_while(|(t, _)| t == &Token::NewLine)
                                    .collect_vec();
                                for _ in 0..consumed.len() {
                                    self.stream.next();
                                }
                                let value = match self.stream.clone().nth(0) {
                                    Some((Token::BeginBlock, _)) => {
                                        ValueType::Function(self.collect_block()?)
                                    }
                                    Some((Token::Compose, _)) => ValueType::Expr(self.compose()?),
                                    Some((Token::Op(op), _)) if is_pipe_op(&op) => {
                                        ValueType::Expr(self.pipe()?)
                                    }
                                    _ => ValueType::Expr(self.next_expr()?.0),
                                };
                                Ok(ValueDeclaration {
                                    loc: ident_span,
                                    is_op: false,
                                    ident,
                                    ty: Some(ty),
                                    args: Vec::new(),
                                    value: value,
                                    generictypes: generics,
                                })
                            }
                            _ => Err(ParseError {
                                span: start,
                                reason: ParseErrorReason::DeclarationError,
                            }),
                        }
                    } else if let Some((Token::Ident(arg0), arg_start)) = next {
                        let mut args = vec![ast::ArgDeclation {
                            ident: arg0,
                            loc: arg_start,
                        }];
                        while let Some((Token::Ident(_), _)) = self.stream.peek() {
                            if let (Token::Ident(arg), arg_start) = self.stream.next().unwrap() {
                                args.push(ast::ArgDeclation {
                                    ident: arg,
                                    loc: arg_start,
                                });
                            } else {
                                return Err(ParseError {
                                    span: (0, 0),
                                    reason: ParseErrorReason::ArgumentError,
                                });
                            }
                        }

                        let count = args.len();
                        args.dedup_by_key(|it| it.ident.clone());
                        if count != args.len() {
                            return Err(ParseError {
                                span: (0, 0),
                                reason: ParseErrorReason::IndentError,
                            });
                        }
                        if let Some((Token::Colon, _next)) = self.stream.peek() {
                            self.stream.next();
                            let ty = self.collect_type()?;
                            let ty = generics
                                .iter()
                                .fold(ty, |ty, name| ty.replace_user_with_generic(&name));
                            match self.stream.next() {
                                Some((Token::Op(eq), eq_span)) if eq == "=" => {
                                    let consumed = self
                                        .stream
                                        .clone()
                                        .take_while(|(t, _)| t == &Token::NewLine)
                                        .collect_vec();
                                    for _ in 0..consumed.len() {
                                        self.stream.next();
                                    }
                                    let value = match self.stream.clone().nth(0) {
                                        Some((Token::BeginBlock, _)) => {
                                            ValueType::Function(self.collect_block()?)
                                        }
                                        Some((Token::Compose, _)) => {
                                            ValueType::Expr(self.compose()?)
                                        }
                                        Some((Token::Op(op), _)) if is_pipe_op(&op) => {
                                            ValueType::Expr(self.pipe()?)
                                        }
                                        _ => ValueType::Expr(self.next_expr()?.0),
                                    };
                                    Ok(ValueDeclaration {
                                        loc: ident_span,
                                        is_op: true,
                                        ident,
                                        ty: Some(ty),
                                        args,
                                        value,
                                        generictypes: generics,
                                    })
                                }
                                _ => Err(ParseError {
                                    span: start,
                                    reason: ParseErrorReason::DeclarationError,
                                }),
                            }
                        } else {
                            match self.stream.next() {
                                Some((Token::Op(eq), eq_span)) if eq == "=" => {
                                    let value =
                                        if let Some((Token::BeginBlock, _)) = self.stream.peek() {
                                            ValueType::Function(self.collect_block()?)
                                        } else {
                                            // TODO check for math expresions
                                            ValueType::Expr(self.literal()?)
                                        };
                                    Ok(ValueDeclaration {
                                        loc: ident_span,
                                        is_op: true,
                                        ident,
                                        ty: None,
                                        args,
                                        value: value,
                                        generictypes: generics,
                                    })
                                }
                                _ => Err(ParseError {
                                    span: ident_span,
                                    reason: ParseErrorReason::DeclarationError,
                                }),
                            }
                        }
                    } else {
                        Err(ParseError {
                            span: ident_span,
                            reason: ParseErrorReason::DeclarationError,
                        })
                    }
                }
                _ => Err(ParseError {
                    span: ident_span,
                    reason: ParseErrorReason::DeclarationError,
                }),
            };
        }
        Err(ParseError {
            span: (0, 0),
            reason: ParseErrorReason::UnknownError,
        })
    }

    fn collect_block(&mut self) -> Result<Vec<Statement>, ParseError> {
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
                sub_expr.push(self.next_statement()?)
            }
            Ok(sub_expr)
        } else {
            Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            })
        }
    }

    fn binary_op(&mut self) -> Result<(ast::Expr, crate::Location), ParseError> {
        let loc = self.stream.clone().nth(2).unwrap().1;
        let mut group_opens = 0;
        let tokens = self
            .stream
            .clone()
            .take_while(|(t, _)| match t {
                Token::GroupOpen => {
                    group_opens += 1;
                    true
                }
                Token::Ident(_)
                | Token::FloatingPoint(_, _)
                | Token::Integer(_, _)
                | Token::Op(_) => true,
                Token::GroupClose => {
                    group_opens -= 1;
                    group_opens >= 0
                }
                _ => false,
            })
            .collect_vec();
        for _ in 0..tokens.len() {
            self.stream.next();
        }
        const PRECIDENCE: [(&'static str, usize, bool); 5] = [
            ("**", 4, true),
            ("*", 3, false),
            ("/", 3, false),
            ("+", 2, false),
            ("-", 2, false),
        ];
        let mut output = Vec::with_capacity(tokens.len());
        let mut op_stack = Vec::with_capacity(tokens.len() / 3);
        let mut token_iter = tokens.into_iter().peekable();
        while let Some((token, _)) = token_iter.peek() {
            match token {
                Token::GroupOpen => {
                    let _ = token_iter.next();
                    let mut group_opens = 0;
                    let sub_tokens = token_iter.clone().take_while(|(t,_)| match t {
                        Token::GroupClose => {group_opens -= 1; group_opens >= 0},
                        Token::GroupOpen => {group_opens+= 1; true},
                        Token::Ident(_) | Token::FloatingPoint(_, _) | Token::Integer(_, _) | Token::Op(_)=> true,
                        _ => false
                    }).collect_vec();
                    for _ in 0..sub_tokens.len() {
                        token_iter.next();
                    }
                    let result = Parser::from_stream(sub_tokens.into_iter()).next_expr()?;
                    output.push(ShuntingYardOptions::Expr(result));
                }
                Token::GroupClose => {
                    let _ = token_iter.next();
                },
                Token::Ident(_) => {
                    if let Some((
                        Token::Ident(_)
                        |Token::CharLiteral(_)
                        |Token::StringLiteral(_)
                        |Token::FloatingPoint(_, _)
                        |Token::Integer(_, _)
                        |Token::GroupOpen,_)) = token_iter.clone().nth(1) {
                        let sub_tokens = token_iter.clone().take_while(|(t,_)| match t {
                            Token::GroupClose => {group_opens -= 1; group_opens >= 0},
                            Token::GroupOpen => {group_opens+= 1; true},
                            Token::Ident(_) | Token::FloatingPoint(_, _) | Token::Integer(_, _)=> true,
                            Token::Op(_) => group_opens>=0,
                            _ => false
                        }).collect_vec();
                        for _ in 0..sub_tokens.len() {
                            token_iter.next();
                        }
                        let result = Parser::from_stream(sub_tokens.into_iter()).next_expr()?;
                        output.push(ShuntingYardOptions::Expr(result))
                    } else {
                        let Some((Token::Ident(ident),loc)) = token_iter.next() else {unreachable!()};
                        output.push(ShuntingYardOptions::Expr((Expr::ValueRead(ident),loc)));
                    }
                },
                Token::Integer(_, _) | Token::FloatingPoint(_, _) | Token::CharLiteral(_) | Token::StringLiteral(_) => output.push(ShuntingYardOptions::Expr({
                    let Some((token,span)) = token_iter.next() else { return Err(ParseError { span: (0,0), reason: ParseErrorReason::UnknownError })};
                    (make_literal(token, span)?,span)
                })),
                Token::Op(_) => {
                    let Some((Token::Op(ident),loc)) = token_iter.next() else { unreachable!() };
                    let (prec,left) = PRECIDENCE.iter().find_map(|(op,weight,assc)| if op == &ident { Some((*weight,*assc))} else { None }).unwrap_or((1,false));
                    if op_stack.is_empty() {
                        op_stack.push((ident,loc));
                        continue;
                    }
                    while let Some(op_back) = op_stack.last() {
                        let back_prec = PRECIDENCE.iter().find_map(|(op,weight,_)| if op == &op_back.0 { Some(*weight)} else { None }).unwrap_or(1);
                        if back_prec > prec || (back_prec == prec && left) {
                            let Some(op_back) = op_stack.pop() else { unreachable!() };
                            output.push(ShuntingYardOptions::Op(op_back));
                        } else {
                            op_stack.push((ident,loc));
                            break;
                        }
                    }
                }
                _ => break
            }
        }
        output.extend(op_stack.into_iter().rev().map(ShuntingYardOptions::Op));
        let mut final_expr = Vec::<(ast::Expr, crate::Location)>::new();
        for expr in output {
            match expr {
                ShuntingYardOptions::Expr(expr) => final_expr.push(expr),
                ShuntingYardOptions::Op((op, loc)) => {
                    let Some((rhs,_)) = final_expr.pop() else { unreachable!() };
                    let Some((lhs,_)) = final_expr.pop() else { unreachable!() };
                    final_expr.push((
                        ast::Expr::BinaryOpCall(BinaryOpCall {
                            loc,
                            operator: op,
                            lhs: lhs.boxed(),
                            rhs: rhs.boxed(),
                        }),
                        loc,
                    ))
                }
            }
        }

        if final_expr.len() != 1 {
            Err(ParseError {
                span: loc,
                reason: ParseErrorReason::UnknownError,
            })
        } else {
            Ok(final_expr.into_iter().next().unwrap())
        }
    }
}

fn make_literal(token: Token, span: (usize, usize)) -> Result<crate::ast::Expr, ParseError> {
    match token {
        Token::CharLiteral(ch) => Ok(ast::Expr::CharLiteral(ch)),
        Token::StringLiteral(src) => {
            let mut value = String::with_capacity(src.len());
            let mut s = src.chars();
            while let Some(c) = s.next() {
                if c == '\\' {
                    let next = match s.next() {
                        Some('n') => '\n',
                        Some('\\') => '\\',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('"') => '"',
                        Some('\'') => '\'',
                        Some(_) => {
                            return Err(ParseError {
                                span,
                                reason: ParseErrorReason::UnsupportedEscape,
                            })
                        }
                        None => unreachable!(),
                    };
                    value.push(next);
                } else {
                    value.push(c);
                }
            }
            Ok(ast::Expr::StringLiteral(value))
        }
        Token::Integer(is_neg, i) => Ok(ast::Expr::NumericLiteral {
            value: if is_neg { "-".to_owned() + &i } else { i },
            // TODO : figure out appropriate type.
            ty: types::INT32,
        }),
        Token::FloatingPoint(is_neg, f) => Ok(Expr::NumericLiteral {
            value: if is_neg { "-".to_owned() + &f } else { f },
            ty: types::FLOAT32,
        }),
        _ => Err(ParseError {
            span,
            reason: ParseErrorReason::UnknownError,
        }),
    }
}
fn type_from_string(name: &str) -> ResolvedType {
    match name {
        "str" => types::STR,
        "char" => types::CHAR,
        ty if ty.starts_with("int") => {
            let size = ty.strip_prefix("int");
            match size {
                Some("8") => types::INT8,
                Some("16") => types::INT16,
                Some("32") => types::INT32,
                Some("64") => types::INT64,
                _ => ResolvedType::User {
                    name: ty.to_string(),
                    generics: Vec::new(),
                },
            }
        }
        ty if ty.starts_with("float") => {
            let size = ty.strip_prefix("float");
            match size {
                Some("32") => types::FLOAT32,
                Some("64") => types::FLOAT64,
                _ => ResolvedType::User {
                    name: ty.to_string(),
                    generics: Vec::new(),
                },
            }
        }
        _ => ResolvedType::User {
            name: name.to_string(),
            generics: Vec::new(),
        },
    }
}
fn is_pipe_op(op: &str) -> bool {
    op.ends_with('>') && op[..op.len() - 2].chars().all(|c| c == '|')
}

enum ShuntingYardOptions {
    Expr((ast::Expr, crate::Location)),
    Op((String, crate::Location)),
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use inkwell::context::Context;

    use crate::{
        ast::ArgDeclation,
        types::{ResolvedType, TypeResolver},
    };

    use super::*;
    #[test]
    #[ignore = "This is for singled out tests"]
    fn for_debugging_only() {
        const SRC: &'static str = "let a : int32 -> int32 = foo b";
        let _expr = Parser::from_source(SRC).next_expr().unwrap();
        let ctx = Context::create();
        let _type_resolver = TypeResolver::new(&ctx);
        let _ty = Parser::from_source("int32 -> int32 -> int32")
            .collect_type()
            .unwrap();

        // let expr = TypedExpr::try_from(&ctx, type_resolver, values, &HashSet::new(), expr).unwrap();
        // println!("{:?}",expr);
    }
    #[test]
    fn individual_simple_expressions() {
        let mut parser = Parser::from_source("let foo : int32 = 5");

        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (0, 5),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ValueType::Expr(Expr::NumericLiteral {
                    value: "5".to_string(),
                    ty: types::INT32
                }),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().expect("failed to parse")
        );
        let mut parser = Parser::from_source(
            r#"let foo _ : int32 -> int32 =
    return 5
"#,
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 5),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }),
                args: vec![ArgDeclation {
                    loc: (0, 9),
                    ident: "_".to_string()
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "5".to_string(),
                        ty: types::INT32
                    },
                    (1, 5)
                )]),
                generictypes: HashSet::new(),
            }),
            parser.declaration().expect("failed to parse")
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
            Statement::Declaration(ValueDeclaration {
                loc: (1, 5),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed(),
                    returns: types::INT32.boxed()
                }),
                args: vec![ast::ArgDeclation {
                    ident: "_".to_string(),
                    loc: (1, 9)
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_string(),
                        ty: types::INT32
                    },
                    (2, 5)
                )]),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().expect("failed to parse"),
            "function as arg"
        );
        const RT: &'static str = r#"
let foo _ : int32 -> ( int32 -> int32 ) =
    return 0
"#;
        let mut parser = Parser::from_source(RT);
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (1, 5),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed()
                }),
                args: vec![ast::ArgDeclation {
                    ident: "_".to_string(),
                    loc: (1, 9)
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_owned(),
                        ty: types::INT32,
                    },
                    (2, 5)
                )]),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().expect("failed to parse"),
            "function as rt"
        );
    }

    #[test]
    fn multiple_statements() {
        const SRC: &'static str = include_str!("../../samples/test.foo");
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 5),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ValueType::Expr(Expr::NumericLiteral {
                    value: "3".to_owned(),
                    ty: types::INT32
                }),
                generictypes: HashSet::new(),
            }),
            parser.declaration().unwrap(),
            "simple declaration"
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (2, 5),
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }),
                args: vec![ast::ArgDeclation {
                    ident: "quz".to_string(),
                    loc: (2, 9)
                }],
                value: ValueType::Function(vec![
                    Statement::Declaration(ValueDeclaration {
                        loc: (3, 9),
                        is_op: false,
                        ident: "baz".to_owned(),
                        ty: Some(types::STR),
                        args: Vec::new(),
                        value: ValueType::Expr(Expr::StringLiteral(r#"merp " yes"#.to_string())),
                        generictypes: HashSet::new(),
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "2".to_owned(),
                            ty: types::INT32
                        },
                        (4, 5)
                    )
                ]),
                generictypes: HashSet::new(),
            }),
            parser.declaration().unwrap(),
            "declaration with block"
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (6, 5),
                is_op: true,
                ident: "^^".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed()
                }),
                args: vec![
                    ast::ArgDeclation {
                        ident: "lhs".to_string(),
                        loc: (6, 8)
                    },
                    ast::ArgDeclation {
                        ident: "rhs".to_string(),
                        loc: (6, 12)
                    },
                ],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (7, 9), // TODO there is an error here.  somewhere in lexer
                        value: Expr::ValueRead("bar".to_string()).boxed(),
                        arg: Some(Expr::ValueRead("foo".to_string()).boxed()),
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "1".to_owned(),
                            ty: types::INT32
                        },
                        (8, 5)
                    )
                ]),
                generictypes: HashSet::new(),
            }),
            parser.declaration().unwrap(),
            "operator declaration w/ function call",
        );
    }

    #[test]
    fn fn_chain() {
        const SRC: &'static str = r#"
let main _ : int32 -> int32 = 
    put_int32 100
    print_str "v"
    return 32
"#;
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            parser.next_statement().unwrap(),
            Statement::Declaration(ValueDeclaration {
                loc: (1, 5),
                is_op: false,
                ident: "main".to_string(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }),
                args: vec![ast::ArgDeclation {
                    ident: "_".to_string(),
                    loc: (1, 10)
                }],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (2, 5),
                        value: Expr::ValueRead("put_int32".to_string()).boxed(),
                        arg: Some(
                            Expr::NumericLiteral {
                                value: "100".to_owned(),
                                ty: types::INT32,
                            }
                            .boxed()
                        )
                    }),
                    Statement::FnCall(FnCall {
                        loc: (3, 5),
                        value: Expr::ValueRead("print_str".to_string()).boxed(),
                        arg: Some(Expr::StringLiteral("v".to_string()).boxed())
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "32".to_string(),
                            ty: types::INT32,
                        },
                        (4, 5)
                    )
                ]),
                generictypes: HashSet::new(),
            })
        )
    }

    #[test]
    fn ops() {
        const SRC_S: &'static str = "100 + 100 * foo * ( 10 - 1 )";
        let mut parser = Parser::from_source(SRC_S);

        assert_eq!(
            Expr::BinaryOpCall(BinaryOpCall {
                loc: (0, 5),
                operator: "+".to_string(),
                lhs: Expr::NumericLiteral {
                    value: "100".to_string(),
                    ty: types::INT32
                }
                .boxed(),
                rhs: Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 11),
                    operator: "*".to_string(),
                    lhs: Expr::NumericLiteral {
                        value: "100".to_string(),
                        ty: types::INT32
                    }
                    .boxed(),
                    rhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 17),
                        operator: "*".to_string(),
                        lhs: Expr::ValueRead("foo".to_string()).boxed(),
                        rhs: Expr::BinaryOpCall(BinaryOpCall {
                            loc: (0, 24),
                            operator: "-".to_string(),
                            lhs: Expr::NumericLiteral {
                                value: "10".to_string(),
                                ty: types::INT32
                            }
                            .boxed(),
                            rhs: Expr::NumericLiteral {
                                value: "1".to_string(),
                                ty: types::INT32
                            }
                            .boxed(),
                        })
                        .boxed()
                    })
                    .boxed()
                })
                .boxed()
            }),
            parser.next_expr().unwrap().0
        );
        const SRC: &'static str = r#"let main _ =
    print_int32 100 + 100
    return 0"#;
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (0, 5),
                is_op: false,
                ident: "main".to_owned(),
                ty: None,
                args: vec![ast::ArgDeclation {
                    ident: "_".to_string(),
                    loc: (0, 10)
                }],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (1, 21),
                        value: Expr::ValueRead("print_int32".to_owned()).boxed(),
                        arg: Some(
                            Expr::BinaryOpCall(BinaryOpCall {
                                loc: (1, 21),
                                operator: "+".to_owned(),
                                lhs: Expr::NumericLiteral {
                                    value: "100".to_owned(),
                                    ty: types::INT32
                                }
                                .boxed(),
                                rhs: Expr::NumericLiteral {
                                    value: "100".to_owned(),
                                    ty: types::INT32
                                }
                                .boxed()
                            })
                            .boxed()
                        )
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "0".to_owned(),
                            ty: types::INT32
                        },
                        (2, 5)
                    )
                ]),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().unwrap()
        )
    }
}

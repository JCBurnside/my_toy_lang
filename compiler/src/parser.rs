use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
    str::Chars,
};

use itertools::Itertools;

use crate::{
    ast::{
        self, BinaryOpCall, Expr, FnCall, Statement, StructConstruction, ValueDeclaration,
        ValueType,
    },
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
        let _ = self
            .stream
            .peeking_take_while(|(it, _)| it == &Token::NewLine)
            .collect::<Vec<_>>();
        self.stream
            .peek()
            .map_or(false, |(token, _)| !token.is_eof())
    }
    pub(crate) fn module(mut self, name: String) -> ast::ModuleDeclaration {
        let mut decls = Vec::new();
        while self.has_next() {
            match self.declaration() {
                Ok(decl) => decls.push(decl),
                Err(e) => {
                    println!("{:?}", e);
                }
            }
        }
        ast::ModuleDeclaration {
            loc: None,
            name,
            declarations: decls,
        }
    }

    fn skip_newlines(&mut self) {
        while let Some((Token::NewLine, _)) = self.stream.peek() {
            let _ = self.stream.next();
        }
    }

    pub fn next_statement(&mut self) -> Result<Statement, ParseError> {
        self.skip_newlines();
        match self.stream.clone().next() {
            Some((Token::NewLine, _)) => {
                self.stream.next();
                self.next_statement()
            }
            Some((Token::Let, _)) | Some((Token::For, _)) => {
                Ok(Statement::Declaration(self.fn_declaration(Vec::new())?))
            }
            Some((Token::Return, _)) => self.ret(),
            // hmmm should handle if it's actually a binary op call esp and/or compose.
            Some((Token::Ident(_), _)) =>
            // for now last statement in a block is not treated as return though will allow for that soon.
            {
                Ok(Statement::FnCall(self.function_call()?.0))
            }
            _ => unreachable!("how?"),
        }
    }

    pub fn next_expr(&mut self) -> Result<(crate::ast::Expr, crate::Location), ParseError> {
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
            Some((Token::Ident(_), _))
                if match self.stream.clone().nth(1) {
                    Some((Token::Op(s), _)) => s == "<",
                    _ => false,
                } =>
            {
                let mut angle_depth = 1;
                let mut skipped = self.stream.clone().skip(2).skip_while(|(it, _)| {
                    if angle_depth == 0 {
                        false
                    } else {
                        match it {
                            Token::Op(op) if op.chars().all_equal_value() == Ok('>') => {
                                if angle_depth <= op.len() {
                                    angle_depth -= op.len();
                                    true
                                } else {
                                    false
                                }
                            }
                            _ => true,
                        }
                    }
                });
                if let Some((Token::CurlOpen, _)) = skipped.next() {
                    let strct = self.struct_construct()?;
                    let loc = strct.loc;
                    Ok((Expr::StructConstruction(strct), loc))
                } else {
                    self.binary_op()
                }
            }
            Some((
                Token::Integer(_, _)
                | Token::FloatingPoint(_, _)
                | Token::CharLiteral(_)
                | Token::StringLiteral(_)
                | Token::Ident(_),
                _,
            )) if match self.stream.clone().nth(1) {
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
                } else if let Some((Token::CurlOpen, _)) = self
                    .stream
                    .clone()
                    .skip(1)
                    .skip_while(|(it, _)| matches!(it, Token::NewLine | Token::BeginBlock))
                    .next()
                {
                    let strct = self.struct_construct()?;
                    let loc = strct.loc;
                    Ok((Expr::StructConstruction(strct), loc))
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

    fn struct_construct(&mut self) -> Result<StructConstruction, ParseError> {
        let ResolvedType::User { name, generics } = self.collect_type()? else {unreachable!("what are you doing?")};
        let Some((Token::CurlOpen,loc)) = self.stream.next() else { unreachable!() };
        let mut fields = HashMap::new();
        while let Some((Token::Ident(_), _)) = self.stream.peek() {
            let Some((Token::Ident(ident),loc)) = self.stream.next() else {unreachable!()};
            let Some((Token::Colon,_)) = self.stream.next() else {todo!("handle infered assignment")};
            let expr = match self.next_expr() {
                Ok((expr, _)) => expr,
                Err(e) => {
                    println!("{:?}", e);
                    Expr::Error
                }
            };
            if fields.contains_key(&ident) {
                let (_, loc): &(_, crate::Location) = &fields[&ident];
                println!("already declared at line {} column {}", loc.0, loc.1)
            } else {
                fields.insert(ident, (expr, loc));
            }
            if let Some((Token::Comma, _)) = self.stream.peek() {
                self.stream.next();
            }
        }
        let _: Vec<_> = self
            .stream
            .peeking_take_while(|(t, _)| matches!(t, Token::EndBlock | Token::NewLine))
            .collect();
        let Some((Token::CurlClose,_)) = self.stream.next() else { println!("not closed"); return Err(ParseError { span: loc, reason: ParseErrorReason::UnbalancedBraces }) };
        Ok(StructConstruction {
            loc,
            fields,
            generics,
            ident: name,
        })
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
        if let Some((Token::Ident(ty), loc)) = ty {
            let mut generic_args = Vec::new();
            if self.stream.peek().map(|(a, _)| a) == Some(&Token::Op("<".to_string())) {
                self.stream.next();
                while {
                    let ty = self.collect_type()?;
                    generic_args.push(ty);
                    #[cfg(debug_assertions)]
                    let _peek = self.stream.peek();
                    if self.stream.peek().map(|(a, _)| a) == Some(&Token::Comma) {
                        let _ = self.stream.next();
                        true
                    } else {
                        false
                    }
                } {}
                if !generic_args.is_empty() {
                    let mut should_pop = false;
                    if let Some((Token::Op(s), _)) = self.stream.peek_mut() {
                        if s.chars().all_equal_value() == Ok('>') {
                            s.pop();
                            if s.len() == 0 {
                                should_pop = true;
                            }
                        } else {
                            return Err(ParseError {
                                span: loc,
                                reason: ParseErrorReason::UnbalancedBraces,
                            });
                        }
                    }
                    if should_pop {
                        let _ = self.stream.next();
                    }
                }
            }
            if let Some((Token::Arrow, _)) = self.stream.peek() {
                self.stream.next();
                let result = self.collect_type()?;

                let ty = type_from_string(&ty, generic_args);
                Ok(ResolvedType::Function {
                    arg: ty.boxed(),
                    returns: result.boxed(),
                })
            } else {
                let ty = type_from_string(&ty, generic_args);
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
        self.skip_newlines();
        let generics = self.collect_generics()?;
        let next = self.stream.clone().next();
        match dbg!(next) {
            Some((Token::For, loc)) => Err(ParseError {
                span: loc,
                reason: ParseErrorReason::DeclarationError,
            }),
            Some((Token::Type | Token::Enum, _)) => {
                Ok(ast::Declaration::TypeDefinition(self.type_decl(generics)?))
            }
            Some((Token::Let, _)) => Ok(ast::Declaration::Value(self.fn_declaration(generics)?)),
            Some((Token::NewLine, _)) => {
                self.stream.next();
                self.declaration()
            }
            _ => unimplemented!(),
        }
    }

    fn type_decl(&mut self, generics: Vec<String>) -> Result<ast::TypeDefinition, ParseError> {
        let Some((t,loc)) = self.stream.next() else {unreachable!()};
        match t {
            Token::Type => {
                let (ident, loc) = match self.stream.next() {
                    Some((Token::Ident(ident), loc)) => (ident, loc),
                    _ => {
                        return Err(ParseError {
                            span: loc,
                            reason: ParseErrorReason::DeclarationError,
                        })
                    }
                };
                let Some((Token::Op(op),_)) = self.stream.next() else { return Err(ParseError { span: loc, reason: ParseErrorReason::DeclarationError })};
                if op != "=" {
                    return Err(ParseError {
                        span: loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                }
                match self.stream.peek() {
                    Some((Token::Ident(_), _)) => {
                        Ok(ast::TypeDefinition::Alias(ident, self.collect_type()?))
                    }
                    Some((Token::CurlOpen, _)) => Ok(ast::TypeDefinition::Struct(
                        self.struct_declaration(ident, generics, loc)?,
                    )),
                    // Some((Token::Op(op), _)) if op == "|" => Ok(ast::TypeDefinition::Enum(
                    //     ident,
                    //     self.enum_declaration(generics)?,
                    //     loc,
                    // )),
                    _ => Err(ParseError {
                        span: loc,
                        reason: ParseErrorReason::DeclarationError,
                    }),
                }
            }
            Token::Enum => {
                todo!()
            }
            _ => unreachable!(),
        }
    }

    #[allow(unused)]
    fn enum_declaration(
        &mut self,
        _generics: HashSet<String>,
    ) -> Result<Vec<ast::EnumVariant>, ParseError> {
        todo!()
    }

    fn struct_declaration(
        &mut self,
        ident: String,
        generics: Vec<String>,
        loc: crate::Location,
    ) -> Result<ast::StructDefinition, ParseError> {
        let Some((Token::CurlOpen,_)) = self.stream.next() else { unreachable!() };
        while let Some((Token::BeginBlock | Token::NewLine, _)) = self.stream.clone().next() {
            self.stream.next();
        }
        let mut fields = Vec::<ast::FieldDecl>::new();
        while let Some((Token::Ident(_), _)) = self.stream.clone().next() {
            let Some((Token::Ident(name),loc)) =
                self.stream.next()
                else {
                    return Err(ParseError { span: (0,10000), reason: ParseErrorReason::DeclarationError })
                };
            let Some((Token::Colon,_)) = self.stream.next() else { return Err(ParseError { span: (0,10000), reason: ParseErrorReason::DeclarationError }) };
            let ty = generics.iter().fold(self.collect_type()?,|result,it| result.replace_user_with_generic(&it));
            if let Some((Token::Comma, _)) = self.stream.clone().next() {
                self.stream.next();
            } else {
                self.skip_newlines();
                if let Some((Token::Ident(_), loc)) = self.stream.clone().next() {
                    println!("expected comma at line:{},col:{}", loc.0, loc.1);
                    while let Some((token, _)) = self.stream.clone().next() {
                        match token {
                            Token::CurlClose => {
                                self.stream.next();
                                break;
                            }
                            _ => {
                                self.stream.next();
                            }
                        }
                    }
                    return Err(ParseError {
                        span: loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                }
            }
            while let Some((Token::EndBlock | Token::NewLine, _)) = self.stream.clone().next() {
                self.stream.next();
            }
            fields.push(ast::FieldDecl { name, ty, loc });
        }
        while let Some((Token::EndBlock | Token::NewLine | Token::Comma, _)) =
            self.stream.clone().next()
        {
            self.stream.next();
        }
        let Some((Token::CurlClose,_)) = dbg!(self.stream.next()) else { return Err(ParseError { span: (0,11111), reason: ParseErrorReason::DeclarationError }) };
        Ok(ast::StructDefinition {
            ident,
            values: fields,
            generics,
            loc,
        })
    }

    fn collect_generics(&mut self) -> Result<Vec<String>, ParseError> {
        let generics = if let Some((Token::For, _)) = dbg!(self.stream.peek()) {
            let Some((_,span)) = self.stream.next() else { unreachable!() };
            match self.stream.next() {
                Some((Token::Op(op), _)) if op == "<" => {
                    let mut out = self
                        .stream
                        .clone()
                        .filter(|(token, _)| &Token::Comma != token)
                        .take_while(|(token, _)| &Token::Op(">".to_string()) != token)
                        .collect_vec();
                    let first_span = out.first().unwrap().1;
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
                    return Err(ParseError {
                        span,
                        reason: ParseErrorReason::DeclarationError,
                    });
                }
            }
        } else {
            Vec::new()
        };
        if generics.len() != 0 {
            while let Some((token, _)) = self.stream.clone().next() {
                match token {
                    Token::Op(op) if op == ">" => {
                        self.stream.next();
                        break;
                    }
                    _ => {
                        self.stream.next();
                    }
                }
            }
        }
        Ok(generics)
    }

    fn fn_declaration(&mut self, generics: Vec<String>) -> Result<ValueDeclaration, ParseError> {
        if let Some((Token::Let, start)) = self.stream.next() {
            let (token, ident_span) = self.stream.next().unwrap();
            return match token {
                Token::Ident(ident) => {
                    let next = self.stream.next();
                    if let Some((Token::Colon, _next)) = next {
                        let ty = self.collect_type()?;
                        match self.stream.next() {
                            Some((Token::Op(eq), _)) if eq == "=" => {
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
                                    generictypes: Vec::new(),
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
                                Some((Token::Op(eq), _)) if eq == "=" => {
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
                                Some((Token::Op(eq), _)) if eq == "=" => {
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
                            Some((Token::Op(eq), _)) if eq == "=" => {
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
                                    span: arg_start,
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
                                Some((Token::Op(eq), _)) if eq == "=" => {
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
                                Some((Token::Op(eq), _)) if eq == "=" => {
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
        if let Some((Token::BeginBlock, _)) = self.stream.next() {
            let mut sub_expr = Vec::new();
            loop {
                let next = self.stream.peek();
                if let Some((Token::EndBlock, _)) = next {
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

        // (name,weight,right associative)
        const PRECIDENCE: [(&'static str, usize, bool); 8] = [
            (".", usize::MAX, true),
            ("<", usize::MAX - 1, true),
            (">", usize::MAX - 1, true),
            ("**", 4, false),
            ("*", 3, true),
            ("/", 3, true),
            ("+", 2, true),
            ("-", 2, true),
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
                            op_stack.push((ident.clone(),loc));
                            break;
                        }
                    }
                    if op_stack.last().is_none() {
                        op_stack.push((ident,loc));
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
fn type_from_string(name: &str, generics: Vec<ResolvedType>) -> ResolvedType {
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
            generics,
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
impl std::fmt::Debug for ShuntingYardOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(arg0) => f.debug_tuple("Expr").field(&arg0.0).finish(),
            Self::Op(arg0) => f.debug_tuple("Op").field(&arg0.0).finish(),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        ast::{ArgDeclation, StructDefinition},
        types::ResolvedType,
    };

    use super::*;
    #[test]
    #[ignore = "This is for singled out tests"]
    fn for_debugging_only() {
        let ty = Parser::from_source("Foo<Bar<int32->(),int32>>")
            .collect_type()
            .unwrap();
        println!("{:?}", ty);
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
                generictypes: Vec::new(),
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
                generictypes: Vec::new(),
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
                generictypes: Vec::new(),
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
                generictypes: Vec::new(),
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
                generictypes: Vec::new(),
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
                        generictypes: Vec::new(),
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "2".to_owned(),
                            ty: types::INT32
                        },
                        (4, 5)
                    )
                ]),
                generictypes: Vec::new(),
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
                generictypes: Vec::new(),
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
                generictypes: Vec::new(),
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
                lhs: Expr::NumericLiteral {
                    value: "100".to_string(),
                    ty: types::INT32
                }
                .boxed(),
                rhs: Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 17),
                    lhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 11),
                        lhs: Expr::NumericLiteral {
                            value: "100".to_string(),
                            ty: types::INT32
                        }
                        .boxed(),
                        rhs: Expr::ValueRead("foo".to_string()).boxed(),
                        operator: "*".to_string()
                    })
                    .boxed(),
                    rhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 24),
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
                        operator: "-".to_string()
                    })
                    .boxed(),
                    operator: "*".to_string()
                })
                .boxed(),
                operator: "+".to_string()
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
                generictypes: Vec::new(),
            }),
            parser.next_statement().unwrap()
        );
        //a b . 2 + c d . -
        const SRC_MEMBER_ACCESS: &'static str = "a.b + 2 - c.d";
        let mut parser = Parser::from_source(SRC_MEMBER_ACCESS);
        assert_eq!(
            ast::Expr::BinaryOpCall(BinaryOpCall {
                loc: (0, 9),
                lhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 5),
                    lhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 2),
                        lhs: ast::Expr::ValueRead("a".to_string()).boxed(),
                        rhs: ast::Expr::ValueRead("b".to_string()).boxed(),
                        operator: ".".to_string()
                    })
                    .boxed(),
                    rhs: ast::Expr::NumericLiteral {
                        value: "2".to_string(),
                        ty: types::INT32
                    }
                    .boxed(),
                    operator: "+".to_string()
                })
                .boxed(),
                rhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 12),
                    lhs: ast::Expr::ValueRead("c".to_string()).boxed(),
                    rhs: ast::Expr::ValueRead("d".to_string()).boxed(),
                    operator: ".".to_string()
                })
                .boxed(),
                operator: "-".to_string()
            }),
            parser.next_expr().unwrap().0,
        )
    }

    #[test]
    fn generics() {
        let mut parser = Parser::from_source("for<T> let test a : T -> T = a");
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 12),
                is_op: false,
                ident: "test".to_string(),
                args: vec![ast::ArgDeclation {
                    loc: (0, 17),
                    ident: "a".to_string(),
                }],
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string()
                    }
                    .boxed(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string()
                    }
                    .boxed(),
                }),
                value: ast::ValueType::Expr(ast::Expr::ValueRead("a".to_string())),
                generictypes: ["T".to_string()].into_iter().collect(),
            }),
            parser.declaration().unwrap(),
        )
    }

    #[test]
    fn struct_decl() {
        const SRC: &'static str = r#"type Foo = {
    a : int32
}
for<T,U> type Tuple = {
    first : T,
    second : U
}"#;
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            ast::Declaration::TypeDefinition(ast::TypeDefinition::Struct(StructDefinition {
                ident: "Foo".to_string(),
                generics: Vec::new(),
                values: vec![ast::FieldDecl {
                    name: "a".to_string(),
                    ty: types::INT32,
                    loc: (1, 5),
                }],
                loc: (0, 6)
            },)),
            parser.declaration().unwrap(),
            "basic"
        );
        assert_eq!(
            ast::Declaration::TypeDefinition(ast::TypeDefinition::Struct(StructDefinition {
                ident: "Tuple".to_string(),
                generics: vec!["T".to_string(), "U".to_string()],
                values: vec![
                    ast::FieldDecl {
                        name: "first".to_string(),
                        ty: ResolvedType::Generic { name: "T".to_string() },
                        loc: (4, 5)
                    },
                    ast::FieldDecl {
                        name: "second".to_string(),
                        ty: ResolvedType::Generic { name: "U".to_string() },
                        loc: (5, 5)
                    },
                ],
                loc: (3, 15)
            })),
            parser.declaration().unwrap(),
            "generic"
        )
    }

    #[test]
    fn generic_use_types() {
        const SRC: &'static str = r"let foo a b : Bar<int32> -> Baz<int32,float64> -> int32 =
    return 0        
";
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 5),
                is_op: false,
                ident: "foo".to_string(),
                args: vec![
                    ArgDeclation {
                        loc: (0, 9),
                        ident: "a".to_string()
                    },
                    ArgDeclation {
                        loc: (0, 11),
                        ident: "b".to_string()
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::User {
                        name: "Bar".to_string(),
                        generics: vec![types::INT32]
                    }
                    .boxed(),
                    returns: ResolvedType::Function {
                        arg: ResolvedType::User {
                            name: "Baz".to_string(),
                            generics: vec![types::INT32, types::FLOAT64]
                        }
                        .boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed()
                }),
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_string(),
                        ty: types::INT32
                    },
                    (1, 5)
                )]),
                generictypes: Vec::new()
            }),
            parser.declaration().unwrap()
        )
    }

    #[test]
    fn struct_construction() {
        const SRC: &'static str = "Foo { a : 0 }";
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            ast::Expr::StructConstruction(StructConstruction {
                loc: (0, 5),
                fields: HashMap::from([(
                    "a".to_string(),
                    (
                        Expr::NumericLiteral {
                            value: "0".to_string(),
                            ty: types::INT32
                        },
                        (0, 7)
                    )
                )]),
                generics: Vec::new(),
                ident: "Foo".to_string()
            }),
            parser.next_expr().unwrap().0
        );
        assert_eq!(
            ast::Expr::StructConstruction(StructConstruction {
                loc: (0, 15),
                fields: HashMap::from([(
                    "a".to_string(),
                    (
                        Expr::NumericLiteral {
                            value: "0".to_string(),
                            ty: types::INT32
                        },
                        (0, 17)
                    )
                )]),
                generics: vec![types::INT32],
                ident: "Generic".to_string()
            }),
            Parser::from_source("Generic<int32>{ a:0 }")
                .next_expr()
                .unwrap()
                .0
        )
    }
}

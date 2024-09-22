use std::{
    collections::{HashMap, HashSet},
    fmt::Error,
    iter::Peekable,
    str::Chars,
};

use ast::{GenericsDecl, TypeDefinition};
use itertools::Itertools;

use crate::{
    ast::{
        self, ArgDeclaration, BinaryOpCall, Expr, FieldDecl, FnCall, Match, Pattern,
        PatternDestructure, Statement, StructConstruction, StructDefinition, TopLevelDeclaration,
        TopLevelValue, ValueDeclaration, ValueType,
    },
    lexer::TokenStream,
    tokens::Token,
    types::{self, ResolvedType},
};

use thiserror::Error;

#[derive(Error, Debug)]
#[error("")]
pub enum Warning {}

#[allow(unused)]
#[derive(Error, Debug)]
#[error("{reason:?} at {span:?}")]
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
    UnsupportedFeature,
    UnexpectedToken,
    NoElseBlock,
    UnknownError, //internal use.  should basically never be hit.
}

#[derive(Debug)]
pub struct ParserReturns<T: std::fmt::Debug> {
    pub ast: T,
    pub loc: crate::Location,
    pub warnings: Vec<Warning>, //TODO! add warnings and a way next_toplevel treat warnings as errors
    pub errors: Vec<ParseError>,
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
            .peeking_take_while(|(token, _)| match token {
                Token::Seq | Token::EndBlock => true,
                _ => false,
            })
            .collect_vec();
        self.stream
            .peek()
            .map_or(false, |(token, _)| !token.is_eof())
    }
    pub(crate) fn module(mut self, name: String) -> ParserReturns<ast::ModuleDeclaration> {
        let mut decls = Vec::new();
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        while self.has_next() {
            let decl = self.next_toplevel();
            warnings.extend(decl.warnings);
            errors.extend(decl.errors);
            decls.push(decl.ast);
        }
        ParserReturns {
            ast: ast::ModuleDeclaration {
                loc: None,
                name,
                declarations: decls,
            },
            loc: (0, 0),
            warnings,
            errors,
        }
    }

    pub fn next_toplevel(&mut self) -> ParserReturns<TopLevelDeclaration> {
        let ParserReturns {
            ast: abi,
            loc: extern_loc,
            mut warnings,
            mut errors,
        } = self.abi();
        let ParserReturns {
            ast: generics,
            loc: for_loc,
            warnings: for_warnings,
            errors: for_errors,
        } = self.collect_generics();
        warnings.extend(for_warnings);
        errors.extend(for_errors);
        if abi.is_some() && generics.is_some() {
            errors.push(ParseError {
                span: for_loc,
                reason: ParseErrorReason::DeclarationError,
            });
        }
        match self.stream.clone().next() {
            Some((Token::For, loc)) => {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::DeclarationError,
                });
                ParserReturns {
                    ast: ast::TopLevelDeclaration::Value(TopLevelValue {
                        loc,
                        is_op: false,
                        ident: "<error>".to_string(),
                        args: vec![ArgDeclaration::Simple {
                            loc,
                            ident: "<error>".to_string(),
                            ty: Some(types::ERROR),
                        }],
                        ty: Some(types::ERROR),
                        value: ValueType::Expr(Expr::Error),
                        generics: generics,
                        abi: None,
                    }),
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((Token::Type | Token::Enum, loc)) => {
                if abi.is_some() {
                    errors.push(ParseError {
                        span: extern_loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                }
                let decl = self.type_decl(generics);
                warnings.extend(decl.warnings);
                errors.extend(decl.errors);
                ParserReturns {
                    ast: ast::TopLevelDeclaration::TypeDefinition(decl.ast),
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((Token::Let, _)) => {
                let _ = self.stream.next();
                let (token, ident_span) = self.stream.next().unwrap();
                let (ident, is_op) = match token {
                    Token::Ident(ident) => (ident, false),
                    Token::Op(ident) => (ident, true),
                    _ => {
                        return ParserReturns {
                            ast: TopLevelDeclaration::Value(TopLevelValue {
                                loc: ident_span,
                                is_op: false,
                                ident: "<error>".to_string(),
                                args: vec![ArgDeclaration::Simple {
                                    loc: (0, 0),
                                    ident: "<error>".to_string(),
                                    ty: Some(types::ERROR),
                                }],
                                ty: Some(types::ERROR),
                                value: ValueType::Expr(Expr::Error),
                                generics,
                                abi,
                            }),
                            loc: ident_span,
                            warnings: Vec::new(),
                            errors: vec![ParseError {
                                span: ident_span,
                                reason: ParseErrorReason::DeclarationError,
                            }],
                        }
                    }
                };
                let args = self.collect_args();
                warnings.extend(args.warnings);
                errors.extend(args.errors);
                let mut args = args.ast;
                if is_op && (args.len() > 2 || args.len() == 0) {
                    errors.push(ParseError {
                        span: ident_span,
                        reason: ParseErrorReason::ArgumentError,
                    });
                }
                let mut ty = if let Some((Token::Colon, _)) = self.stream.peek() {
                    let _ = self.stream.next();
                    let ty = self.collect_type();
                    warnings.extend(ty.warnings);
                    errors.extend(ty.errors);
                    Some(ty.ast)
                } else {
                    if is_op {
                        errors.push(ParseError {
                            span: ident_span,
                            reason: ParseErrorReason::DeclarationError,
                        });
                    }
                    None
                };
                let value = match self.stream.peek() {
                    Some((Token::Op(op), _)) if op == "=" => {
                        let _ = self.stream.next();
                        match self.stream.peek() {
                            Some((Token::BeginBlock, _)) => {
                                let block = self.collect_block();
                                warnings.extend(block.warnings);
                                errors.extend(block.errors);
                                ValueType::Function(block.ast)
                            }
                            Some(_) => {
                                let expr = self.next_expr();
                                warnings.extend(expr.warnings);
                                errors.extend(expr.errors);
                                ValueType::Expr(expr.ast)
                            }
                            None => {
                                errors.push(ParseError {
                                    span: (0, 0),
                                    reason: ParseErrorReason::UnexpectedEndOfFile,
                                });
                                ValueType::Expr(Expr::Error)
                            }
                        }
                    }
                    Some((Token::Seq, _)) if abi.is_some() => {
                        let _ = self.stream.next();
                        ValueType::External
                    }
                    Some((_, loc)) => {
                        errors.push(ParseError {
                            span: *loc,
                            reason: ParseErrorReason::UnexpectedToken,
                        });
                        ValueType::Expr(Expr::Error)
                    }
                    None => {
                        errors.push(ParseError {
                            span: (0, 0),
                            reason: ParseErrorReason::UnexpectedEndOfFile,
                        });
                        ValueType::Expr(Expr::Error)
                    }
                };
                if let Some(generics) = &generics {
                    for arg in &mut args {
                        generics
                            .decls
                            .iter()
                            .map(|(_, it)| it)
                            .for_each(|name| arg.apply_generic(name));
                    }
                    if let Some(ty) = &mut ty {
                        *ty = generics
                            .decls
                            .iter()
                            .map(|(_, it)| it)
                            .fold(ty.clone(), |ty, name| ty.replace_user_with_generic(name));
                    }
                }
                ParserReturns {
                    ast: TopLevelDeclaration::Value(TopLevelValue {
                        loc: ident_span,
                        is_op,
                        ident,
                        args,
                        ty,
                        value,
                        generics,
                        abi,
                    }),
                    loc: ident_span,
                    warnings,
                    errors,
                }
            }
            Some((Token::EoF, loc)) => {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                ParserReturns {
                    ast: ast::TopLevelDeclaration::Value(TopLevelValue {
                        loc,
                        is_op: false,
                        ident: "<error>".to_string(),
                        args: vec![ArgDeclaration::Simple {
                            loc,
                            ident: "<error>".to_string(),
                            ty: Some(types::ERROR),
                        }],
                        ty: Some(types::ERROR),
                        value: ValueType::Expr(Expr::Error),
                        generics: generics,
                        abi: None,
                    }),
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((_, loc)) => {
                errors.push(ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::UnexpectedToken,
                });
                ParserReturns {
                    ast: ast::TopLevelDeclaration::Value(TopLevelValue {
                        loc,
                        is_op: false,
                        ident: "<error>".to_string(),
                        args: vec![ArgDeclaration::Simple {
                            loc,
                            ident: "<error>".to_string(),
                            ty: Some(types::ERROR),
                        }],
                        ty: Some(types::ERROR),
                        value: ValueType::Expr(Expr::Error),
                        generics: generics,
                        abi: None,
                    }),
                    loc,
                    warnings,
                    errors,
                }
            }
            None => {
                errors.push(ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                ParserReturns {
                    ast: ast::TopLevelDeclaration::Value(TopLevelValue {
                        loc: (0, 0),
                        is_op: false,
                        ident: "<error>".to_string(),
                        args: vec![ArgDeclaration::Simple {
                            loc: (0, 0),
                            ident: "<error>".to_string(),
                            ty: Some(types::ERROR),
                        }],
                        ty: Some(types::ERROR),
                        value: ValueType::Expr(Expr::Error),
                        generics: generics,
                        abi: None,
                    }),
                    loc: (0, 0),
                    warnings,
                    errors,
                }
            }
        }
    }
    pub fn next_statement(&mut self) -> ParserReturns<Statement> {
        match self.stream.clone().next() {
            Some((Token::For, _)) => {
                let ParserReturns {
                    ast: generics,
                    loc,
                    mut warnings,
                    mut errors,
                } = self.collect_generics();
                let inner = self.fn_declaration(None, generics);
                warnings.extend(inner.warnings);
                errors.extend(inner.errors);
                if let Some((Token::Seq, _)) = self.stream.clone().next() {
                    self.stream.next();
                } else {
                    // TODO generated error here.
                    println!("expected ; on line {}", inner.loc.0);
                }
                ParserReturns {
                    ast: Statement::Declaration(inner.ast),
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((Token::Let, _)) => {
                let inner = match self.stream.clone().nth(1) {
                    Some((Token::GroupOpen, _)) => self.destructuring_declaration(),
                    Some((Token::Ident(_), _))
                        if self.stream.clone().nth(2).map(|(a, _)| a) == Some(Token::CurlOpen) =>
                    {
                        self.destructuring_declaration()
                    }
                    _ => self.fn_declaration(None, None),
                };
                if let Some((Token::Seq, _)) = self.stream.clone().next() {
                    self.stream.next();
                } else {
                    // TODO generated error here.
                    println!("expected ; on line {}", inner.loc.0);
                }

                ParserReturns {
                    ast: Statement::Declaration(inner.ast),
                    loc: inner.loc,
                    warnings: inner.warnings,
                    errors: inner.errors,
                }
            }
            Some((Token::Return, _)) => {
                let inner = self.ret();
                if let Some((Token::Seq, _)) = self.stream.clone().next() {
                    self.stream.next();
                } else {
                    println!("expected ; on line {}", inner.loc.0);
                    //TODO! convert next_toplevel an error that is added and returned as part of the ast.
                }
                inner
            }
            // hmmm should handle if it's actually a binary op call esp and/or compose.
            Some((Token::Ident(_), _)) =>
            // for now last statement in a block is not treated as return though will allow for that soon.
            {
                let ParserReturns {
                    ast,
                    loc,
                    warnings,
                    errors,
                } = self.function_call();
                let inner = Statement::FnCall(ast);
                if let Some((Token::Seq, _)) = self.stream.clone().next() {
                    self.stream.next();
                } else {
                    println!("expected ; on line {}", inner.get_loc().0);
                    //TODO! convert next_toplevel a warning that is added and returned as part of the ast.
                }
                ParserReturns {
                    ast: inner,
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((Token::If, _)) => {
                let inner = self.ifstatement();

                ParserReturns {
                    ast: Statement::IfStatement(inner.ast),
                    loc: inner.loc,
                    warnings: inner.warnings,
                    errors: inner.errors,
                }
            }
            Some((Token::Match, _)) => {
                let ParserReturns {
                    ast: match_,
                    loc,
                    warnings,
                    errors,
                } = self.match_();
                let ast = Statement::Match(match_);
                ParserReturns {
                    ast,
                    loc,
                    warnings,
                    errors,
                }
            }
            _ => unreachable!("how?"),
        }
    }

    pub fn next_expr(&mut self) -> ParserReturns<ast::Expr> {
        match self.stream.clone().next() {
            Some((Token::If, loc)) => {
                let ParserReturns {
                    ast,
                    loc: _,
                    warnings,
                    errors,
                } = self.ifexpr();
                ParserReturns {
                    ast: Expr::If(ast),
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((Token::BracketOpen, _)) => self.array_literal(),
            Some((Token::GroupOpen, loc))
                if matches!(self.stream.clone().nth(1), Some((Token::GroupClose, _))) =>
            {
                self.stream.next();
                self.stream.next();
                return ParserReturns {
                    ast: Expr::UnitLiteral,
                    loc,
                    warnings: Vec::new(),
                    errors: Vec::new(),
                };
            }
            Some((Token::GroupOpen, group_loc)) => {
                let mut group_opens = 0;
                if let Some((Token::Op(_), _)) = self
                    .stream
                    .clone()
                    .skip(1)
                    .skip_while(|(it, _)| match it {
                        //skip this whole expression next_toplevel examine what's after it.
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
                    .skip(1)
                    .next()
                {
                    self.binary_op()
                } else {
                    self.stream.next();
                    let mut out = self.next_expr();

                    match self.stream.peek() {
                        Some((Token::GroupClose, _)) => {
                            let _ = self.stream.next();
                        }
                        Some((Token::Comma, _)) => {
                            let _ = self.stream.next();
                            let mut ast = vec![out.ast];
                            loop {
                                let ParserReturns {
                                    ast: expr,
                                    loc,
                                    warnings,
                                    errors,
                                } = self.next_expr();
                                out.errors.extend(errors);
                                out.warnings.extend(warnings);
                                ast.push(expr);
                                match self.stream.clone().next() {
                                    Some((Token::Comma, _)) => {
                                        self.stream.next();
                                        continue;
                                    }
                                    Some((Token::GroupClose, _)) => {
                                        self.stream.next();
                                        break;
                                    }
                                    Some((Token::EoF, _)) | None => {
                                        out.errors.push(ParseError {
                                            span: (0, 0),
                                            reason: ParseErrorReason::UnexpectedEndOfFile,
                                        });
                                        break;
                                    }
                                    // TODO better error reporting.
                                    Some((_other, loc)) => {
                                        out.errors.push(ParseError {
                                            span: loc,
                                            reason: ParseErrorReason::UnexpectedToken,
                                        });
                                        ast.push(ast::Expr::Error);
                                        break;
                                    }
                                }
                            }

                            out.ast = if ast.len() == 1 {
                                ast.pop().unwrap()
                            } else {
                                Expr::TupleLiteral {
                                    contents: ast,
                                    loc: group_loc,
                                }
                            }
                        }
                        _ => (),
                    }
                    return out;
                }
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
                    let ParserReturns {
                        ast,
                        loc,
                        warnings,
                        errors,
                    } = self.struct_construct();
                    ParserReturns {
                        ast: Expr::StructConstruction(ast),
                        loc,
                        warnings,
                        errors,
                    }
                } else {
                    self.binary_op()
                }
            }
            Some((
                Token::Integer(_, _)
                | Token::FloatingPoint(_, _)
                | Token::CharLiteral(_)
                | Token::StringLiteral(_)
                | Token::Ident(_)
                | Token::True
                | Token::False,
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
                _,
            )) => self.literal(),
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
                    let ParserReturns {
                        ast,
                        loc,
                        warnings,
                        errors,
                    } = self.function_call();
                    ParserReturns {
                        ast: Expr::FnCall(ast),
                        loc,
                        warnings,
                        errors,
                    }
                } else if let Some((Token::CurlOpen, _)) = self
                    .stream
                    .clone()
                    .skip(1)
                    .skip_while(|(it, _)| matches!(it, Token::BeginBlock))
                    .next()
                {
                    let ParserReturns {
                        ast,
                        loc,
                        warnings,
                        errors,
                    } = self.struct_construct();
                    ParserReturns {
                        ast: Expr::StructConstruction(ast),
                        loc,
                        warnings,
                        errors,
                    }
                } else {
                    self.value()
                }
            }
            Some((Token::True, loc)) => {
                let _ = self.stream.next();
                ParserReturns {
                    ast: Expr::BoolLiteral(true, loc),
                    loc,
                    warnings: Vec::new(),
                    errors: Vec::new(),
                }
            }
            Some((Token::False, loc)) => {
                let _ = self.stream.next();
                ParserReturns {
                    ast: Expr::BoolLiteral(false, loc),
                    loc,
                    warnings: Vec::new(),
                    errors: Vec::new(),
                }
            }
            Some((Token::Match, _)) => {
                let ParserReturns {
                    ast,
                    loc,
                    warnings,
                    errors,
                } = self.match_();
                ParserReturns {
                    ast: Expr::Match(ast),
                    loc,
                    warnings,
                    errors,
                }
            }
            _ => ParserReturns {
                ast: Expr::Error,
                loc: (0, 0),
                warnings: Vec::new(),
                errors: vec![ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::UnknownError,
                }],
            },
        }
    }

    fn ifexpr(&mut self) -> ParserReturns<ast::IfExpr> {
        let Some((Token::If, if_loc)) = self.stream.next() else {
            unreachable!()
        };
        let ParserReturns {
            ast: root_cond,
            loc: _,
            mut warnings,
            mut errors,
        } = self.next_expr();
        if let Some((Token::Then, _)) = self.stream.peek() {
            let _ = self.stream.next();
        } else {
            let (_token, loc) = self.stream.next().unwrap();
            // TODO! recovery?
            errors.push(ParseError {
                span: loc,
                reason: ParseErrorReason::UnexpectedToken,
            });
        }
        let true_body = match self.stream.clone().next() {
            Some((Token::BeginBlock, _)) => {
                let _ = self.stream.next();
                let mut body = Vec::new();
                while self
                    .stream
                    .clone()
                    .skip_while(|(it, _)| it != &Token::Seq && it != &Token::EndBlock)
                    .next()
                    .map(|a| a.0)
                    != Some(Token::EndBlock)
                {
                    let stmnt = self.next_statement();
                    warnings.extend(stmnt.warnings);
                    errors.extend(stmnt.errors);
                    body.push(stmnt.ast);
                }
                let ret = self.next_expr();
                errors.extend(ret.errors);
                warnings.extend(ret.warnings);
                let _ = self.stream.next();
                (body, ret.ast)
            }
            _ => (Vec::new(), {
                let ret = self.next_expr();
                warnings.extend(ret.warnings);
                errors.extend(ret.errors);
                ret.ast
            }),
        };
        if let Some((Token::Else, _)) = self.stream.peek() {
            let _ = self.stream.next();
            let mut else_ifs = Vec::new();

            while let Some((Token::If, _)) = self.stream.peek() {
                let Some((Token::If, loc)) = self.stream.next() else {
                    unreachable!()
                };
                let cond = self.next_expr();
                warnings.extend(cond.warnings);
                errors.extend(cond.errors);
                let cond = cond.ast.into();

                if let Some((Token::Then, _)) = self.stream.peek() {
                    let _ = self.stream.next();
                } else {
                    let (_token, loc) = self.stream.next().unwrap();
                    // TODO! recovery?
                    errors.push(ParseError {
                        span: loc,
                        reason: ParseErrorReason::UnexpectedToken,
                    });
                }
                let (body, ret) = match self.stream.clone().next() {
                    Some((Token::BeginBlock, _)) => {
                        let _ = self.stream.next();
                        let mut body = Vec::new();
                        while self
                            .stream
                            .clone()
                            .skip_while(|(it, _)| it != &Token::Seq && it != &Token::EndBlock)
                            .next()
                            .map(|a| a.0)
                            != Some(Token::EndBlock)
                        {
                            let stmnt = self.next_statement();
                            warnings.extend(stmnt.warnings);
                            errors.extend(stmnt.errors);
                            body.push(stmnt.ast);
                        }
                        let ret = self.next_expr();
                        errors.extend(ret.errors);
                        warnings.extend(ret.warnings);
                        let _ = self.stream.next();
                        (body, ret.ast)
                    }
                    _ => {
                        let ret = self.next_expr();
                        warnings.extend(ret.warnings);
                        errors.extend(ret.errors);
                        (Vec::new(), ret.ast)
                    }
                };
                else_ifs.push((cond, body, ret.into()));

                if let Some((Token::Else, _loc)) = self.stream.clone().next() {
                    let _else_token = self.stream.next();
                } else {
                    //todo! recovery?
                    errors.push(ParseError {
                        span: loc,
                        reason: ParseErrorReason::UnexpectedToken,
                    });
                    return ParserReturns {
                        ast: ast::IfExpr {
                            cond: root_cond.into(),
                            loc: if_loc,
                            true_branch: (true_body.0, true_body.1.into()),
                            else_ifs,
                            else_branch: (Vec::new(), ast::Expr::Error.into()),
                        },
                        loc: if_loc,
                        warnings,
                        errors,
                    };
                };
            }

            let else_branch = match self.stream.clone().next() {
                Some((Token::BeginBlock, _)) => {
                    let _ = self.stream.next();

                    let mut body = Vec::new();
                    while self
                        .stream
                        .clone()
                        .skip_while(|(it, _)| it != &Token::Seq && it != &Token::EndBlock)
                        .next()
                        .map(|a| a.0 != Token::EndBlock && a.0 != Token::EoF)
                        .unwrap_or(false)
                    {
                        let stmnt = self.next_statement();
                        warnings.extend(stmnt.warnings);
                        errors.extend(stmnt.errors);
                        body.push(stmnt.ast);
                    }
                    let ret = self.next_expr();
                    warnings.extend(ret.warnings);
                    errors.extend(ret.errors);

                    let _ = self.stream.next();
                    (body, ret.ast.into())
                }
                _ => {
                    let ret = self.next_expr();
                    warnings.extend(ret.warnings);
                    errors.extend(ret.errors);
                    (Vec::new(), ret.ast.into())
                }
            };

            ParserReturns {
                ast: ast::IfExpr {
                    cond: root_cond.into(),
                    true_branch: (true_body.0, true_body.1.into()),
                    else_ifs,
                    else_branch,
                    loc: if_loc,
                },
                loc: if_loc,
                warnings,
                errors,
            }
        } else {
            if let Some((_, loc)) = self.stream.clone().next() {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnexpectedToken,
                });
            }
            // TODO! recover
            ParserReturns {
                ast: ast::IfExpr {
                    cond: root_cond.into(),
                    true_branch: (true_body.0, true_body.1.into()),
                    else_ifs: Vec::new(),
                    else_branch: (Vec::new(), Expr::Error.into()),
                    loc: if_loc,
                },
                loc: if_loc,
                warnings,
                errors,
            }
        }
    }

    fn value(&mut self) -> ParserReturns<Expr> {
        if let Some((Token::Ident(ident), loc)) = self.stream.clone().next() {
            let _ = self.stream.next();
            ParserReturns {
                ast: ast::Expr::ValueRead(ident, loc),
                loc,
                warnings: Vec::new(),
                errors: Vec::new(),
            }
        } else {
            let loc = if let Some((_, loc)) = self.stream.peek() {
                *loc
            } else {
                return ParserReturns {
                    ast: Expr::Error,
                    loc: (0, 0),
                    warnings: Vec::new(),
                    errors: vec![ParseError {
                        span: (0, 0),
                        reason: ParseErrorReason::UnexpectedEndOfFile,
                    }],
                };
            };
            ParserReturns {
                ast: Expr::Error,
                loc: loc,
                warnings: Vec::new(),
                errors: vec![ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnknownError,
                }],
            }
        }
    }

    fn function_call(&mut self) -> ParserReturns<FnCall> {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        if let Some((Token::Ident(ident), value_loc)) = self.stream.next() {
            let mut values = Vec::<(ast::Expr, (usize, usize))>::new();
            while let Some((
                | Token::GroupOpen
                | Token::Ident(_)
                // | Token::Op(_) // TODO! this will need some special handling.  ie for `foo bar >>> baz`  should that be parsed as `(foo bar) >>> baz` or `foo (bar >>> baz)`
                | Token::Integer(_,_)
                | Token::FloatingPoint(_, _)
                | Token::CharLiteral(_)
                | Token::StringLiteral(_)
                | Token::True
                | Token::False
                | Token::BracketOpen
                ,_
            )) = self.stream.peek() {
                match self.stream.peek().map(|(a,_)| a) {
                    Some(Token::Ident(_)) => {
                        let test = self.stream.clone().nth(1);
                        if let Some((Token::Op(_),_)) = test {
                            let ParserReturns { ast:expr, loc, warnings:expr_warnings, errors:expr_errors } =self.binary_op();
                            values.push((expr,loc));
                            warnings.extend(expr_warnings);
                            errors.extend(expr_errors);
                        } else {
                            let ParserReturns { ast:expr, loc, warnings:expr_warnings, errors:expr_errors } =self.value();
                            values.push((expr,loc));
                            warnings.extend(expr_warnings);
                            errors.extend(expr_errors);
                        }
                    }
                    Some(Token::GroupOpen | Token::BracketOpen) =>{
                        let value = self.next_expr();
                        errors.extend(value.errors);
                        warnings.extend(value.warnings);
                        values.push((value.ast,value.loc))
                    },
                    Some(
                        | Token::Integer(_,_)
                        | Token::FloatingPoint(_, _)
                        | Token::CharLiteral(_)
                        | Token::StringLiteral(_)
                        | Token::True
                        | Token::False
                    ) => {
                        let test = self.stream.clone().nth(1);
                        if let Some((
                            | Token::Ident(_)
                            | Token::Integer(_,_)
                            | Token::FloatingPoint(_, _)
                            | Token::CharLiteral(_)
                            | Token::StringLiteral(_)
                        ,loc)) = test {
                            let ParserReturns { ast, loc:_, warnings:lit_warnings, errors:lit_errors, }= self.literal();
                            warnings.extend(lit_warnings);
                            errors.extend(lit_errors);
                            values.push((ast,loc))
                        } else if let Some((Token::Op(_),_)) = test {
                            let ParserReturns { ast:expr, loc, warnings:expr_warnings, errors:expr_errors } =self.binary_op();
                            values.push((expr,loc));
                            warnings.extend(expr_warnings);
                            errors.extend(expr_errors);
                        } else {
                            let ParserReturns { ast, loc:_, warnings:lit_warnings, errors:lit_errors, }= self.literal();
                            warnings.extend(lit_warnings);
                            errors.extend(lit_errors);
                            values.push((ast,value_loc))
                        }
                    },
                    _ => unreachable!()
                    // TODO! add in operator case special handling
                }
            }
            let (ast, _loc) = values.into_iter().fold(
                (
                    FnCall {
                        loc: value_loc,
                        value: ast::Expr::ValueRead(ident, value_loc).into(),
                        arg: None,
                    },
                    value_loc,
                ),
                |(inner, loc), (next, next_loc)| {
                    if let FnCall {
                        value, arg: None, ..
                    } = inner
                    {
                        (
                            FnCall {
                                loc,
                                value,
                                arg: Some(next.into()),
                            },
                            next_loc,
                        )
                    } else {
                        (
                            FnCall {
                                loc,
                                value: Expr::FnCall(inner).into(),
                                arg: Some(next.into()),
                            },
                            next_loc,
                        )
                    }
                },
            );
            ParserReturns {
                ast,
                loc: value_loc,
                warnings,
                errors,
            }
        } else {
            errors.push(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            });
            ParserReturns {
                ast: FnCall {
                    value: Expr::Error.into(),
                    arg: Some(Expr::Error.into()),
                    loc: (0, 0),
                },
                loc: (0, 0),
                warnings,
                errors,
            }
        }
    }

    fn struct_construct(&mut self) -> ParserReturns<StructConstruction> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let Some((_, loc)) = self.stream.clone().next() else {
            unreachable!("ICE: somehow reached struct construction with no token?")
        };

        let ty = self.collect_type();
        warnings.extend(ty.warnings);
        errors.extend(ty.errors);
        let (name, generics) = if let ResolvedType::User { name, generics, .. } = ty.ast {
            (name, generics)
        } else {
            ("<error>".to_string(), vec![types::ERROR])
        };
        let Some((Token::CurlOpen, loc)) = self.stream.next() else {
            /* TODO? handle blocks?
                EG: ```
                let x = Foo
                    {
                        y:0
                    }
                ```
                though idomatic would be the following or inline.
                ```
                let x = Foo {
                    y:0
                }
                ```
            */
            unreachable!("ICE: reached struct construction with no braces?")
        };
        let mut fields = HashMap::new();
        while let Some((Token::Ident(_), _)) = self.stream.peek() {
            let Some((Token::Ident(ident), loc)) = self.stream.next() else {
                unreachable!()
            };
            let Some((Token::Colon, _)) = self.stream.next() else {
                todo!("handle infered assignment")
            };
            let ParserReturns {
                ast: expr,
                loc: _,
                warnings: expr_warnings,
                errors: expr_errors,
            } = self.next_expr();
            warnings.extend(expr_warnings);
            errors.extend(expr_errors);
            if fields.contains_key(&ident) {
                let (_, loc): &(_, crate::Location) = &fields[&ident];
                errors.push(ParseError {
                    span: *loc,
                    reason: ParseErrorReason::DeclarationError,
                })
            } else {
                fields.insert(ident, (expr, loc));
            }
            if let Some((Token::Comma, _)) = self.stream.peek() {
                self.stream.next();
            }
        }
        let _: Vec<_> = self
            .stream
            .peeking_take_while(|(t, _)| matches!(t, Token::EndBlock))
            .collect();
        let Some((Token::CurlClose, _)) = self.stream.next() else {
            errors.push(ParseError {
                span: loc,
                reason: ParseErrorReason::UnbalancedBraces,
            });
            fields.insert("".to_string(), (Expr::Error, (0, 0)));
            return ParserReturns {
                ast: StructConstruction {
                    loc,
                    fields,
                    generics,
                    ident: "".to_string(),
                },
                loc,
                warnings,
                errors,
            };
        };
        ParserReturns {
            ast: StructConstruction {
                loc,
                fields,
                generics,
                ident: name,
            },
            loc,
            warnings,
            errors,
        }
    }

    fn ret(&mut self) -> ParserReturns<Statement> {
        let (token, span) = self.stream.next().unwrap();
        if token == Token::Return {
            let ParserReturns {
                ast: expr,
                loc: _,
                warnings,
                errors,
            } = self.next_expr();
            ParserReturns {
                ast: ast::Statement::Return(expr, span),
                loc: span,
                warnings,
                errors,
            }
        } else {
            ParserReturns {
                ast: Statement::Error,
                loc: span,
                warnings: Vec::new(),
                errors: vec![ParseError {
                    span,
                    reason: ParseErrorReason::UnknownError,
                }],
            }
        }
    }

    fn literal(&mut self) -> ParserReturns<Expr> {
        let (token, span) = self.stream.next().unwrap();
        match make_literal(token, span) {
            Ok(lit) => ParserReturns {
                ast: lit,
                loc: span,
                warnings: Vec::new(),
                errors: Vec::new(),
            },
            Err(e) => ParserReturns {
                ast: Expr::Error,
                loc: span,
                warnings: Vec::new(),
                errors: vec![e],
            },
        }
    }

    fn ifstatement(&mut self) -> ParserReturns<crate::ast::IfBranching> {
        let Some((Token::If, if_loc)) = self.stream.next() else {
            unreachable!()
        };
        let ParserReturns {
            ast: cond,
            mut warnings,
            mut errors,
            loc: _,
        } = self.next_expr();
        let cond = cond.into();
        if let Some((Token::Then, _)) = self.stream.peek() {
            let _ = self.stream.next();
        } else {
            let (token, loc) = self.stream.next().unwrap();
            println!(
                "Expected then but got {:?} at line:{} col:{}",
                token, loc.0, loc.1
            );
            errors.push(ParseError {
                span: loc,
                reason: ParseErrorReason::UnexpectedToken,
            });
            //TODO? more recovery?
            return ParserReturns {
                ast: ast::IfBranching {
                    cond,
                    true_branch: vec![Statement::Error],
                    else_ifs: Vec::new(),
                    else_branch: Vec::new(),
                    loc: if_loc,
                },
                loc: if_loc,
                warnings,
                errors,
            };
        };

        let body = match self.stream.peek() {
            Some((Token::BeginBlock, _)) => {
                let block = self.collect_block();
                warnings.extend(block.warnings);
                errors.extend(block.errors);
                block.ast
            }
            _ => {
                let stmnt = self.next_statement();
                warnings.extend(stmnt.warnings);
                errors.extend(stmnt.errors);
                vec![stmnt.ast]
            }
        };

        if let Some((Token::Else, _)) = self.stream.peek() {
            let mut else_ifs = Vec::new();
            while let Some((Token::If, _)) = self.stream.clone().nth(1) {
                let Some((Token::Else, _)) = self.stream.next() else {
                    unreachable!()
                };
                let Some((Token::If, _loc)) = self.stream.next() else {
                    unreachable!()
                };

                let ParserReturns {
                    ast: cond,
                    warnings: new_warnings,
                    errors: new_errors,
                    loc: _,
                } = self.next_expr();
                warnings.extend(new_warnings);
                errors.extend(new_errors);
                let cond = cond.into();
                if let Some((Token::Then, _)) = self.stream.peek() {
                    let _ = self.stream.next();
                } else {
                    let (token, loc) = self.stream.next().unwrap();
                    println!(
                        "Expected then but got {:?} at line:{} col:{}",
                        token, loc.0, loc.1
                    );
                    errors.push(ParseError {
                        span: loc,
                        reason: ParseErrorReason::UnexpectedToken,
                    });
                };
                let body = match self.stream.peek() {
                    Some((Token::BeginBlock, _)) => {
                        let block = self.collect_block();
                        warnings.extend(block.warnings);
                        errors.extend(block.errors);
                        block.ast
                    }
                    _ => {
                        let stmnt = self.next_statement();
                        warnings.extend(stmnt.warnings);
                        errors.extend(stmnt.errors);
                        vec![stmnt.ast]
                    }
                };
                else_ifs.push((cond, body));
            }

            let else_branch = if let Some((Token::Else, _)) = self.stream.clone().next() {
                let _ = self.stream.next();

                match self.stream.peek() {
                    Some((Token::BeginBlock, _)) => {
                        let block = self.collect_block();
                        warnings.extend(block.warnings);
                        errors.extend(block.errors);
                        block.ast
                    }
                    _ => {
                        let stmnt = self.next_statement();
                        warnings.extend(stmnt.warnings);
                        errors.extend(stmnt.errors);
                        vec![stmnt.ast]
                    }
                }
            } else {
                Vec::new()
            };
            ParserReturns {
                ast: ast::IfBranching {
                    cond,
                    true_branch: body,
                    else_ifs,
                    else_branch,
                    loc: if_loc,
                },
                loc: if_loc,
                warnings,
                errors,
            }
        } else {
            ParserReturns {
                ast: ast::IfBranching {
                    cond,
                    true_branch: body,
                    else_ifs: Vec::new(),
                    else_branch: Vec::new(),
                    loc: if_loc,
                },
                loc: if_loc,
                warnings,
                errors,
            }
        }
    }

    pub(crate) fn collect_type(&mut self) -> ParserReturns<ResolvedType> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let ty = self.stream.next();
        match ty {
            Some((Token::Ident(ty), loc)) => {
                let mut generic_args = Vec::new();
                if self.stream.peek().map(|(a, _)| a) == Some(&Token::Op("<".to_string())) {
                    self.stream.next();
                    while {
                        let ty = self.collect_type();
                        warnings.extend(ty.warnings);
                        errors.extend(ty.errors);
                        generic_args.push(ty.ast);
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
                                errors.push(ParseError {
                                    span: loc,
                                    reason: ParseErrorReason::UnbalancedBraces,
                                });
                                return ParserReturns {
                                    ast: types::ERROR,
                                    loc,
                                    warnings,
                                    errors,
                                };
                            }
                        }
                        if should_pop {
                            let _ = self.stream.next();
                        }
                    }
                }
                if let Some((Token::Arrow, fn_loc)) = self.stream.clone().next() {
                    self.stream.next();
                    let result = self.collect_type();
                    let ty = type_from_string(&ty, generic_args, loc);
                    warnings.extend(result.warnings);
                    errors.extend(result.errors);
                    ParserReturns {
                        ast: ResolvedType::Function {
                            arg: ty.into(),
                            returns: result.ast.into(),
                            loc: fn_loc,
                        },
                        loc: fn_loc,
                        warnings,
                        errors,
                    }
                } else {
                    let ty = type_from_string(&ty, generic_args, loc);
                    ParserReturns {
                        ast: ty,
                        loc,
                        warnings,
                        errors,
                    }
                }
            }
            Some((Token::GroupOpen, span)) => {
                if let Some((Token::GroupClose, _)) = self.stream.peek() {
                    self.stream.next();
                    return if let Some((Token::Arrow, arr_loc)) = self.stream.clone().next() {
                        self.stream.next();
                        let returns = self.collect_type();
                        warnings.extend(returns.warnings);
                        errors.extend(returns.errors);
                        ParserReturns {
                            ast: ResolvedType::Function {
                                arg: types::UNIT.into(),
                                returns: returns.ast.into(),
                                loc: arr_loc,
                            },
                            loc: span,
                            warnings,
                            errors,
                        }
                    } else {
                        ParserReturns {
                            ast: types::UNIT,
                            loc: span,
                            warnings,
                            errors,
                        }
                    };
                }
                let ty = self.collect_type();
                warnings.extend(ty.warnings);
                errors.extend(ty.errors);
                let loc = ty.loc;
                let mut tys = vec![ty.ast];
                loop {
                    if let Some((Token::Comma, _)) = self.stream.clone().next() {
                        let _ = self.stream.next();
                        let ty = self.collect_type();
                        warnings.extend(ty.warnings);
                        errors.extend(ty.errors);
                        tys.push(ty.ast);
                        match self.stream.clone().next() {
                            Some((Token::GroupClose, _)) => {
                                break;
                            }
                            Some((Token::Comma, _)) => continue,
                            _ => {
                                errors.push(ParseError {
                                    span: span,
                                    reason: ParseErrorReason::TypeError,
                                });
                                return ParserReturns {
                                    ast: types::ERROR,
                                    loc: (0, 0),
                                    warnings,
                                    errors,
                                };
                            }
                        }
                    } else {
                        break;
                        //TODO! error reporting?
                    }
                }
                let ty = if tys.len() == 1 {
                    tys.pop().unwrap()
                } else {
                    ResolvedType::Tuple {
                        underlining: tys,
                        loc: span,
                    }
                };
                if let Some((Token::GroupClose, _)) = self.stream.clone().next() {
                    let _ = self.stream.next();
                } else {
                    errors.push(ParseError {
                        span,
                        reason: ParseErrorReason::UnbalancedBraces,
                    });
                }
                if let Some((Token::Arrow, arr_loc)) = self.stream.clone().next() {
                    self.stream.next();
                    let result = self.collect_type();
                    warnings.extend(result.warnings);
                    errors.extend(result.errors);
                    ParserReturns {
                        ast: ResolvedType::Function {
                            arg: ty.into(),
                            returns: result.ast.into(),
                            loc: arr_loc,
                        },
                        loc: arr_loc,
                        warnings,
                        errors,
                    }
                } else {
                    ParserReturns {
                        ast: ty,
                        loc,
                        warnings,
                        errors,
                    }
                }
            }
            Some((Token::BracketOpen, loc)) => {
                let ty = self.collect_type();
                warnings.extend(ty.warnings);
                errors.extend(ty.errors);
                let ty = ty.ast;
                match self.stream.clone().next() {
                    Some((Token::Seq, _)) => {
                        let _ = self.stream.next();
                        match self.stream.clone().next() {
                            Some((Token::Integer(false, _), _)) => {
                                let Some((Token::Integer(_, value), _)) = self.stream.next() else {
                                    unreachable!()
                                };
                                match self.stream.peek() {
                                    Some((Token::BracketClose, _)) => {
                                        let _ = self.stream.next();
                                        ParserReturns {
                                            ast: ResolvedType::Array {
                                                underlining: ty.into(),
                                                size: value.parse().unwrap(),
                                            },
                                            loc,
                                            warnings,
                                            errors,
                                        }
                                    }
                                    Some((t, loc)) => {
                                        println!(
                                            "unexpected token:{t:?} at line:{}, col:{}",
                                            loc.0, loc.1
                                        );
                                        errors.push(ParseError {
                                            span: *loc,
                                            reason: ParseErrorReason::UnbalancedBraces,
                                        });
                                        ParserReturns {
                                            ast: types::ERROR,
                                            loc: *loc,
                                            warnings,
                                            errors,
                                        }
                                    }
                                    None => {
                                        unreachable!("how did this happen.  this is an ice please report it.");
                                    }
                                }
                            }
                            Some((t, loc)) => {
                                println!("unexpected token:{t:?} at line:{}, col:{}", loc.0, loc.1);
                                errors.push(ParseError {
                                    span: loc,
                                    reason: ParseErrorReason::UnbalancedBraces,
                                });
                                ParserReturns {
                                    ast: types::ERROR,
                                    loc,
                                    warnings,
                                    errors,
                                }
                            }
                            None => {
                                unreachable!(
                                    "how did this happen.  this is an ice please report it."
                                );
                            }
                        }
                    }
                    Some((Token::BracketClose, _)) => {
                        let _ = self.stream.next();
                        ParserReturns {
                            ast: ResolvedType::Slice {
                                underlining: ty.into(),
                            },
                            loc,
                            warnings,
                            errors,
                        }
                    }
                    Some((t, loc)) => {
                        println!("unexpected token:{t:?} at line:{}, col:{}", loc.0, loc.1);
                        errors.push(ParseError {
                            span: loc,
                            reason: ParseErrorReason::UnbalancedBraces,
                        });
                        ParserReturns {
                            ast: types::ERROR,
                            loc,
                            warnings,
                            errors,
                        }
                    }
                    None => {
                        unreachable!("how did this happen.  this is an ICE please report it.");
                    }
                }
            }
            Some((_, span)) => {
                errors.push(ParseError {
                    span,
                    reason: ParseErrorReason::TypeError,
                });
                ParserReturns {
                    ast: types::ERROR,
                    loc: span,
                    warnings,
                    errors,
                }
            }
            None => {
                errors.push(ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::TypeError,
                });
                ParserReturns {
                    ast: types::ERROR,
                    loc: (0, 0),
                    warnings,
                    errors,
                }
            }
        }
    }
    #[allow(unused)]
    fn pipe(&mut self) -> Result<Expr, ParseError> {
        todo!("pipes")
    }
    #[allow(unused)]
    /// will probably remove as it can easily be defined in lang?
    fn compose(&mut self) -> Result<Expr, ParseError> {
        todo!("compose")
    }

    fn abi(&mut self) -> ParserReturns<Option<ast::Abi>> {
        match self.stream.clone().next() {
            Some((Token::Extern, loc)) => {
                let _ = self.stream.next();
                let mut errors = Vec::new();
                let abi = if let Some((Token::StringLiteral(_), _)) = self.stream.clone().next() {
                    let Some((Token::StringLiteral(name), _)) = self.stream.next() else {
                        unreachable!()
                    };
                    name
                } else {
                    errors.push(ParseError {
                        span: loc,
                        reason: ParseErrorReason::UnexpectedToken,
                    });
                    "".to_string()
                };
                ParserReturns {
                    ast: Some(ast::Abi {
                        loc,
                        identifier: abi,
                    }),
                    loc,
                    warnings: Vec::new(),
                    errors,
                }
            }
            _ => ParserReturns {
                ast: None,
                loc: (0, 0),
                warnings: Vec::new(),
                errors: Vec::new(),
            },
        }
    }

    fn declaration(&mut self) -> ParserReturns<ast::ValueDeclaration> {
        let ParserReturns {
            ast: abi,
            loc: extern_loc,
            mut warnings,
            mut errors,
        } = self.abi();
        let ParserReturns {
            ast: generics,
            loc: for_loc,
            warnings: for_warnings,
            errors: for_errors,
        } = self.collect_generics();
        warnings.extend(for_warnings);
        errors.extend(for_errors);
        if abi.is_some() && generics.is_some() {
            errors.push(ParseError {
                span: for_loc,
                reason: ParseErrorReason::DeclarationError,
            });
        }
        let next = self.stream.clone().next();
        match next {
            Some((Token::Let, _)) => self.fn_declaration(abi, generics),
            Some((Token::Seq, _)) => {
                let _ = self.stream.next();
                self.declaration()
            }
            _ => unimplemented!(),
        }
    }

    fn type_decl(
        &mut self,
        generics: Option<ast::GenericsDecl>,
    ) -> ParserReturns<ast::TypeDefinition> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let Some((t, loc)) = self.stream.next() else {
            unreachable!()
        };
        match t {
            Token::Type => {
                let (ident, loc) = match self.stream.next() {
                    Some((Token::Ident(ident), loc)) => (ident, loc),
                    _ => {
                        // todo recover?
                        errors.push(ParseError {
                            span: loc,
                            reason: ParseErrorReason::DeclarationError,
                        });
                        return ParserReturns {
                            ast: TypeDefinition::Alias("<Error>".to_string(), types::ERROR),
                            loc,
                            warnings,
                            errors,
                        };
                    }
                };
                let Some((Token::Op(op), _)) = self.stream.next() else {
                    errors.push(ParseError {
                        span: loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                    return ParserReturns {
                        ast: TypeDefinition::Alias("<Error>".to_string(), types::ERROR),
                        loc,
                        warnings,
                        errors,
                    };
                };
                if op != "=" {
                    errors.push(ParseError {
                        span: loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                    return ParserReturns {
                        ast: TypeDefinition::Alias("<Error>".to_string(), types::ERROR),
                        loc,
                        warnings,
                        errors,
                    };
                }
                match self.stream.peek() {
                    Some((Token::Ident(_), _)) => {
                        let ty = self.collect_type();
                        warnings.extend(ty.warnings);
                        errors.extend(ty.errors);
                        ParserReturns {
                            ast: ast::TypeDefinition::Alias(ident, ty.ast),
                            loc,
                            warnings,
                            errors,
                        }
                    }
                    Some((Token::CurlOpen, _)) => {
                        let strct = self.struct_declaration(ident, generics, loc);
                        warnings.extend(strct.warnings);
                        errors.extend(strct.errors);
                        ParserReturns {
                            ast: ast::TypeDefinition::Struct(strct.ast),
                            loc,
                            warnings,
                            errors,
                        }
                    }
                    // Some((Token::Op(op), _)) if op == "|" => Ok(ast::TypeDefinition::Enum(
                    //     ident,
                    //     self.enum_declaration(generics)?,
                    //     loc,
                    // )),
                    _ => {
                        errors.push(ParseError {
                            span: loc,
                            reason: ParseErrorReason::DeclarationError,
                        });
                        ParserReturns {
                            ast: ast::TypeDefinition::Alias("<error>".to_string(), types::ERROR),
                            loc,
                            warnings,
                            errors,
                        }
                    }
                }
            }
            Token::Enum => {
                let enum_ = self.enum_declaration(generics);
                ParserReturns {
                    ast: ast::TypeDefinition::Enum(enum_.ast),
                    loc,
                    warnings: enum_.warnings,
                    errors: enum_.errors,
                }
            }
            _ => unreachable!(),
        }
    }

    #[allow(unused)]
    fn enum_declaration(
        &mut self,
        generics: Option<GenericsDecl>,
    ) -> ParserReturns<ast::EnumDeclaration> {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        let (ident, loc) = match self.stream.clone().next() {
            Some((Token::Ident(_), _)) => {
                let Some((Token::Ident(ident), loc)) = self.stream.next() else {
                    unreachable!()
                };
                (ident, loc)
            }
            Some((Token::EoF, loc)) => {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                return ParserReturns {
                    ast: ast::EnumDeclaration {
                        ident: "<error>".to_string(),
                        generics,
                        values: Vec::new(),
                        loc: (0, 0),
                    },
                    loc: (0, 0),
                    warnings,
                    errors,
                };
            }
            Some((_, loc)) => {
                let _ = self
                    .stream
                    .peeking_take_while(|(token, _)| match token {
                        Token::Op(op) if op == "=" || op == "|" => false,
                        _ => true,
                    })
                    .collect_vec();
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnexpectedToken,
                });
                ("<error>".to_string(), loc)
            }
            None => {
                errors.push(ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                return ParserReturns {
                    ast: ast::EnumDeclaration {
                        ident: "<error>".to_string(),
                        generics,
                        values: Vec::new(),
                        loc: (0, 0),
                    },
                    loc: (0, 0),
                    warnings,
                    errors,
                };
            }
        };
        let op = match self.stream.next() {
            Some((Token::Op(op), _)) => op,
            _ => {
                //TODO! progress next_toplevel valid point.
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::DeclarationError,
                });
                return ParserReturns {
                    ast: ast::EnumDeclaration {
                        ident,
                        generics,
                        values: vec![ast::EnumVariant::Tuple {
                            ident: "<error>".to_string(),
                            ty: types::ERROR,
                            loc: (0, 0),
                        }],
                        loc,
                    },
                    loc,
                    warnings,
                    errors,
                };
            }
        };

        if op != "=" {
            //TODO! progress next_toplevel until valid point.
            errors.push(ParseError {
                span: loc,
                reason: ParseErrorReason::DeclarationError,
            });
            return ParserReturns {
                ast: ast::EnumDeclaration {
                    ident,
                    generics,
                    values: vec![ast::EnumVariant::Tuple {
                        ident: "<error>".to_string(),
                        ty: types::ERROR,
                        loc: (0, 0),
                    }],
                    loc,
                },
                loc,
                warnings,
                errors,
            };
        }
        let mut values = Vec::new();
        while let Some((Token::Op(op), _)) = self.stream.peek() {
            if op == "|" {
                let _ = self.stream.next();
                let (ident, variant_loc) = match self.stream.clone().next() {
                    Some((Token::Ident(_), _)) => {
                        let Some((Token::Ident(ident), loc)) = self.stream.next() else {
                            unreachable!()
                        };
                        (ident, loc)
                    }
                    Some((Token::EoF, loc)) => {
                        errors.push(ParseError {
                            span: loc,
                            reason: ParseErrorReason::UnexpectedEndOfFile,
                        });
                        return ParserReturns {
                            ast: ast::EnumDeclaration {
                                ident,
                                generics,
                                values: vec![ast::EnumVariant::Tuple {
                                    ident: "<error>".to_string(),
                                    ty: types::ERROR,
                                    loc: (0, 0),
                                }],
                                loc,
                            },
                            loc,
                            warnings,
                            errors,
                        };
                    }
                    Some((_, loc)) => {
                        let _ = self
                            .stream
                            .peeking_take_while(|(token, _)| match token {
                                Token::Op(op) if op == "=" || op == "|" => false,
                                _ => true,
                            })
                            .collect_vec();
                        errors.push(ParseError {
                            span: loc,
                            reason: ParseErrorReason::UnexpectedToken,
                        });
                        ("<error>".to_string(), loc)
                    }
                    None => {
                        errors.push(ParseError {
                            span: (0, 0),
                            reason: ParseErrorReason::UnexpectedEndOfFile,
                        });
                        return ParserReturns {
                            ast: ast::EnumDeclaration {
                                ident,
                                generics,
                                values: vec![ast::EnumVariant::Tuple {
                                    ident: "<error>".to_string(),
                                    ty: types::ERROR,
                                    loc: (0, 0),
                                }],
                                loc,
                            },
                            loc,
                            warnings,
                            errors,
                        };
                    }
                };
                match self.stream.peek() {
                    Some((Token::CurlOpen, _)) => {
                        let fields = self.struct_declaration("".to_string(), generics.clone(), loc);
                        warnings.extend(fields.warnings);
                        errors.extend(fields.errors);
                        values.push(ast::EnumVariant::Struct {
                            ident,
                            fields: fields.ast.values,
                            loc: variant_loc,
                        });
                    }
                    Some((Token::Op(op), _)) if op == "|" => {
                        values.push(ast::EnumVariant::Unit {
                            ident,
                            loc: variant_loc,
                        });
                    }
                    Some((Token::Ident(_) | Token::BracketOpen | Token::GroupOpen, _)) => {
                        let ty = self.collect_type();
                        warnings.extend(ty.warnings);
                        errors.extend(ty.errors);
                        let ty = ty.ast;
                        let ty = if let Some(generics) = &generics {
                            generics
                                .decls
                                .iter()
                                .map(|(_, name)| name)
                                .fold(ty, |accum, name| accum.replace_user_with_generic(name))
                        } else {
                            ty
                        };
                        values.push(ast::EnumVariant::Tuple {
                            ident,
                            ty,
                            loc: variant_loc,
                        });
                    }
                    _ => {
                        values.push(ast::EnumVariant::Unit {
                            ident,
                            loc: variant_loc,
                        });
                        break;
                    }
                }
            } else {
                break;
            }
        }
        ParserReturns {
            ast: ast::EnumDeclaration {
                ident,
                generics,
                values,
                loc,
            },
            loc,
            warnings,
            errors,
        }
    }

    fn struct_declaration(
        &mut self,
        ident: String,
        generics: Option<ast::GenericsDecl>,
        loc: crate::Location,
    ) -> ParserReturns<ast::StructDefinition> {
        let Some((Token::CurlOpen, _)) = self.stream.next() else {
            unreachable!()
        };
        while let Some((Token::BeginBlock, _)) = self.stream.clone().next() {
            self.stream.next();
        }
        let mut errors = Vec::new();
        #[allow(unused_mut)]
        let mut warnings = Vec::new();
        let mut fields = Vec::<ast::FieldDecl>::new();
        while let Some((Token::Ident(_), _)) = self.stream.clone().next() {
            let Some((Token::Ident(name), loc)) = self.stream.next() else {
                unreachable!();
            };
            let Some((Token::Colon, _)) = self.stream.next() else {
                errors.push(ParseError {
                    span: (0, 10000),
                    reason: ParseErrorReason::DeclarationError,
                });
                fields.push(FieldDecl {
                    name,
                    ty: ResolvedType::Error,
                    loc,
                });
                let ts = self
                    .stream
                    .clone()
                    .take_while(|(it, _)| match it {
                        Token::Comma | Token::CurlClose | Token::EndBlock => false,
                        _ => true,
                    })
                    .collect_vec();
                for _ in ts {
                    let _ = self.stream.next();
                }
                continue;
            };
            let ty = self.collect_type();
            warnings.extend(ty.warnings);
            errors.extend(ty.errors);
            let ty = ty.ast;
            let ty = if let Some(generics) = &generics {
                generics
                    .decls
                    .iter()
                    .map(|(_, it)| it)
                    .fold(ty, |result, it| result.replace_user_with_generic(it))
            } else {
                ty
            };
            if let Some((Token::Comma, _)) = self.stream.clone().next() {
                self.stream.next();
            } else {
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
                    errors.push(ParseError {
                        span: loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                }
            }
            while let Some((Token::EndBlock, _)) = self.stream.clone().next() {
                self.stream.next();
            }
            fields.push(ast::FieldDecl { name, ty, loc });
        }
        while let Some((Token::EndBlock | Token::Comma, _)) = self.stream.clone().next() {
            self.stream.next();
        }
        let Some((Token::CurlClose, _)) = self.stream.next() else {
            errors.push(ParseError {
                span: (0, 11111),
                reason: ParseErrorReason::DeclarationError,
            });
            return ParserReturns {
                ast: StructDefinition {
                    ident,
                    generics,
                    values: fields,
                    loc,
                },
                loc,
                warnings,
                errors,
            };
        };
        ParserReturns {
            ast: StructDefinition {
                ident,
                generics,
                values: fields,
                loc,
            },
            loc,
            warnings,
            errors,
        }
    }

    fn collect_generics(&mut self) -> ParserReturns<Option<ast::GenericsDecl>> {
        #[allow(unused_mut)]
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let generics = if let Some((Token::For, _)) = self.stream.clone().next() {
            let Some((_, for_loc)) = self.stream.next() else {
                unreachable!()
            };
            match self.stream.next() {
                Some((Token::Op(op), _)) if op == "<" => {
                    let mut out = self
                        .stream
                        .clone()
                        .filter(|(token, _)| &Token::Comma != token)
                        .take_while(|(token, _)| &Token::Op(">".to_string()) != token)
                        .collect_vec();
                    // TODO should probably fold next_toplevel ensure that it is comma seperated.
                    let first_span = out.first().unwrap().1;
                    let num = out.len();
                    out.dedup_by_key(|(it, _)| it.clone());
                    if out.len() == num {
                        Some(ast::GenericsDecl {
                            for_loc,
                            decls: out
                                .into_iter()
                                .filter_map(|(t, loc)| {
                                    let Token::Ident(name) = t else { return None };
                                    Some((loc, name))
                                })
                                .collect(),
                        })
                    } else {
                        errors.push(ParseError {
                            span: first_span,
                            reason: ParseErrorReason::DeclarationError,
                        });
                        return ParserReturns {
                            ast: None,
                            loc: first_span,
                            warnings,
                            errors,
                        };
                    }
                }
                _ => {
                    errors.push(ParseError {
                        span: for_loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                    return ParserReturns {
                        ast: None,
                        loc: for_loc,
                        warnings,
                        errors,
                    };
                }
            }
        } else {
            None
        };
        if generics.is_some() {
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
        ParserReturns {
            loc: generics.as_ref().map(|it| it.for_loc).unwrap_or_default(),
            ast: generics,
            warnings,
            errors,
        }
    }

    fn parse_arg(&mut self) -> ParserReturns<ArgDeclaration> {
        #[allow(unused_mut)]
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        match dbg!(self.stream.clone().next()) {
            Some((Token::GroupOpen, _)) => {
                //(
                let Some((Token::GroupOpen, open_loc)) = self.stream.next() else {
                    unreachable!()
                };
                match self.stream.clone().next() {
                    Some((Token::GroupClose, _)) => {
                        //()
                        let _ = self.stream.next(); // )
                        let ty = if let Some((Token::Colon, _)) = self.stream.clone().next() {
                            let _ = self.stream.next(); // :
                            let ty = self.collect_type();
                            warnings.extend(ty.warnings);
                            errors.extend(ty.errors);
                            // will error at type checking/inference phase if not a unit type.
                            Some(ty.ast)
                        } else {
                            None
                        };
                        ParserReturns {
                            ast: ArgDeclaration::Unit { loc: open_loc, ty },
                            loc: open_loc,
                            warnings,
                            errors,
                        }
                    }
                    Some((Token::Ident(_), _)) => {
                        // (<ident>
                        let Some((Token::Ident(name), loc)) = self.stream.next() else {
                            unreachable!()
                        };
                        match self.stream.clone().next() {
                            Some((Token::Colon, _)) => {
                                //(<ident>:
                                let _ = self.stream.next(); // :
                                let ty = self.collect_type();
                                warnings.extend(ty.warnings);
                                errors.extend(ty.errors);
                                let ty = ty.ast;
                                match self.stream.clone().next() {
                                    Some((Token::GroupClose, _)) => {
                                        let _ = self.stream.next();
                                    }
                                    Some((_, loc)) => {
                                        errors.push(ParseError {
                                            span: loc,
                                            reason: ParseErrorReason::UnbalancedBraces,
                                        });
                                    }
                                    None => {
                                        errors.push(ParseError {
                                            span: (0, 0),
                                            reason: ParseErrorReason::UnbalancedBraces,
                                        });
                                    }
                                }
                                ParserReturns {
                                    ast: if name == "_" {
                                        ArgDeclaration::Discard { loc, ty: Some(ty) }
                                    } else {
                                        ArgDeclaration::Simple {
                                            loc,
                                            ident: name,
                                            ty: Some(ty),
                                        }
                                    },
                                    loc,
                                    warnings,
                                    errors,
                                }
                            }
                            Some((Token::Comma, _)) => {
                                //(<ident>,
                                let mut contents = vec![if name == "_" {
                                    ArgDeclaration::Discard { loc, ty: None }
                                } else {
                                    ArgDeclaration::Simple {
                                        loc,
                                        ident: name,
                                        ty: None,
                                    }
                                }];
                                let _ = dbg!(self.stream.next()); //,
                                while let Some((Token::Ident(_) | Token::GroupOpen, _)) =
                                    self.stream.clone().next()
                                {
                                    let arg = self.parse_arg();
                                    warnings.extend(arg.warnings);
                                    errors.extend(arg.errors);
                                    contents.push(arg.ast);
                                }
                                let ty = if let Some((Token::Colon, _)) = self.stream.clone().next()
                                {
                                    let _ = self.stream.next();
                                    let ty = self.collect_type();
                                    warnings.extend(ty.warnings);
                                    errors.extend(ty.errors);
                                    Some(ty.ast)
                                } else {
                                    None
                                };
                                if let Some((Token::GroupClose, _)) = self.stream.clone().next() {
                                    let _ = self.stream.next();
                                } else {
                                    errors.push(ParseError {
                                        span: loc,
                                        reason: ParseErrorReason::UnbalancedBraces,
                                    });
                                }
                                ParserReturns {
                                    ast: ArgDeclaration::DestructureTuple(contents, ty, open_loc),
                                    loc: open_loc,
                                    warnings,
                                    errors,
                                }
                            }
                            Some((Token::GroupClose, _)) => {
                                //(<ident>)
                                let _ = self.stream.next();
                                ParserReturns {
                                    ast: if name == "_" {
                                        ArgDeclaration::Discard { loc, ty: None }
                                    } else {
                                        ArgDeclaration::Simple {
                                            loc,
                                            ident: name,
                                            ty: None,
                                        }
                                    },
                                    loc: open_loc,
                                    warnings,
                                    errors,
                                }
                            }
                            Some((_, loc)) => {
                                errors.push(ParseError {
                                    span: loc,
                                    reason: ParseErrorReason::UnbalancedBraces,
                                });
                                ParserReturns {
                                    ast: ArgDeclaration::Simple {
                                        loc,
                                        ident: "<error>".to_string(),
                                        ty: Some(types::ERROR),
                                    },
                                    loc,
                                    warnings,
                                    errors,
                                }
                            }
                            None => {
                                errors.push(ParseError {
                                    span: (0, 0),
                                    reason: ParseErrorReason::UnbalancedBraces,
                                });
                                ParserReturns {
                                    ast: ArgDeclaration::Simple {
                                        loc: (0, 0),
                                        ident: "<error>".to_string(),
                                        ty: Some(types::ERROR),
                                    },
                                    loc: (0, 0),
                                    warnings,
                                    errors,
                                }
                            }
                        }
                    }
                    _ => {
                        //(<unknown>
                        let inner = self.parse_arg();
                        warnings.extend(inner.warnings);
                        errors.extend(inner.errors);
                        let mut inner = inner.ast;
                        if let Some((Token::Colon, _)) = self.stream.clone().next() {
                            let _ = self.stream.next();
                            let ty = self.collect_type();
                            warnings.extend(ty.warnings);
                            errors.extend(ty.errors);
                            let new_ty = ty.ast;
                            match &mut inner {
                                ArgDeclaration::DestructureStruct {
                                    loc,
                                    struct_ident,
                                    fields,
                                    renamed_fields,
                                } => (), //generate warning.
                                ArgDeclaration::Discard { ty, .. }
                                | ArgDeclaration::DestructureTuple(_, ty, _)
                                | ArgDeclaration::Unit { ty, .. }
                                | ArgDeclaration::Simple { ty, .. } => *ty = Some(new_ty),
                            }
                        }
                        if let Some((Token::GroupClose, _)) = self.stream.clone().next() {
                            let _ = self.stream.next();
                        } else {
                            errors.push(ParseError {
                                span: open_loc,
                                reason: ParseErrorReason::UnbalancedBraces,
                            });
                        }
                        ParserReturns {
                            ast: inner,
                            loc: open_loc,
                            warnings,
                            errors,
                        }
                    }
                }
            }
            Some((Token::Ident(_), _)) => {
                let Some((Token::Ident(ident), loc)) = self.stream.next() else {
                    unreachable!()
                };
                ParserReturns {
                    loc,
                    ast: if ident == "_" {
                        ArgDeclaration::Discard { loc, ty: None }
                    } else {
                        ArgDeclaration::Simple {
                            loc,
                            ident,
                            ty: None,
                        }
                    },
                    warnings,
                    errors,
                }
            }
            Some((Token::EoF, loc)) => {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                ParserReturns {
                    ast: ArgDeclaration::Simple {
                        loc,
                        ident: "<error>".to_string(),
                        ty: Some(types::ERROR),
                    },
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((_token, loc)) => {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnexpectedToken,
                });
                ParserReturns {
                    loc,
                    ast: ArgDeclaration::Simple {
                        loc,
                        ident: "<error>".to_string(),
                        ty: Some(types::ERROR),
                    },
                    warnings,
                    errors,
                }
            }
            None => {
                errors.push(ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                ParserReturns {
                    ast: ArgDeclaration::Simple {
                        loc: (0, 0),
                        ident: "<error>".to_string(),
                        ty: Some(types::ERROR),
                    },
                    loc: (0, 0),
                    warnings,
                    errors,
                }
            }
        }
    }

    fn collect_args(&mut self) -> ParserReturns<Vec<ArgDeclaration>> {
        let mut out = Vec::new();
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        while let Some((t, _)) = self.stream.clone().next() {
            if let Token::Op(eq) = &t {
                if eq == "=" {
                    break;
                }
            }
            if t == Token::Colon {
                break;
            }
            let arg = self.parse_arg();
            warnings.extend(arg.warnings);
            errors.extend(arg.errors);
            out.push(arg.ast)
        }
        ParserReturns {
            ast: out,
            loc: (0, 0), //this is discarded.
            warnings,
            errors,
        }
    }

    fn destructuring_declaration(&mut self) -> ParserReturns<ValueDeclaration> {
        let Some((Token::Let, _)) = self.stream.next() else {
            unreachable!()
        };
        let ParserReturns {
            ast: kind,
            loc,
            mut warnings,
            mut errors,
        } = self.collect_pattern();
        let ty = if let Some((Token::Colon, _)) = self.stream.peek() {
            let _ = self.stream.next();
            let ty = self.collect_type();
            warnings.extend(ty.warnings);
            errors.extend(ty.errors);
            Some(ty.ast)
        } else {
            None
        };
        if let Some((Token::Op(eq), loc)) = self.stream.peek() {
            if eq != "=" {
                errors.push(ParseError {
                    span: *loc,
                    reason: ParseErrorReason::UnexpectedToken,
                });
            } else {
                let _ = self.stream.next();
            }
        } else {
            errors.push(ParseError {
                span: loc,
                reason: ParseErrorReason::UnexpectedToken,
            });
        }
        let expr = match self.stream.peek() {
            Some((Token::BeginBlock, loc)) => {
                errors.push(ParseError {
                    span: *loc,
                    reason: ParseErrorReason::UnsupportedFeature,
                });
                let result = self.collect_block();
                warnings.extend(result.warnings);
                errors.extend(result.errors);
                ast::Expr::Error
            }
            Some(_) => {
                let result = self.next_expr();
                warnings.extend(result.warnings);
                errors.extend(result.errors);
                result.ast
            }
            _ => {
                errors.push(ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                ast::Expr::Error
            }
        };
        ParserReturns {
            ast: ValueDeclaration {
                loc,
                is_op: false,
                target: kind,
                args: Vec::new(),
                ty,
                value: ValueType::Expr(expr),
                generictypes: None,
                abi: None,
            },
            loc,
            warnings,
            errors,
        }
    }

    fn fn_declaration(
        &mut self,
        abi: Option<ast::Abi>,
        generics: Option<ast::GenericsDecl>,
    ) -> ParserReturns<ValueDeclaration> {
        if let Some((Token::Let, _start)) = self.stream.next() {
            let (token, ident_span) = self.stream.next().unwrap();
            let (ident, is_op) = match token {
                Token::Ident(ident) => (ident, false),
                Token::Op(ident) => (ident, true),
                _ => {
                    return ParserReturns {
                        ast: ValueDeclaration {
                            loc: ident_span,
                            is_op: false,
                            target: ast::Pattern::Read("<error>".to_string(), ident_span),
                            args: vec![ArgDeclaration::Simple {
                                loc: (0, 0),
                                ident: "<error>".to_string(),
                                ty: Some(types::ERROR),
                            }],
                            ty: Some(types::ERROR),
                            value: ValueType::Expr(Expr::Error),
                            generictypes: generics,
                            abi,
                        },
                        loc: ident_span,
                        warnings: Vec::new(),
                        errors: vec![ParseError {
                            span: ident_span,
                            reason: ParseErrorReason::DeclarationError,
                        }],
                    }
                }
            };
            let ParserReturns {
                ast: mut args,
                loc: _,
                mut warnings,
                mut errors,
            } = self.collect_args();
            if is_op && (args.len() > 2 || args.len() == 0) {
                if let Some((Token::Colon, _)) = self.stream.clone().next() {
                    let _ = self.stream.next();
                    let _ = self.collect_type();
                }
                if let Some((Token::Op(op), _)) = self.stream.clone().next() {
                    if op == "=" {
                        let _ = self.stream.next();
                        match self.stream.clone().next() {
                            Some((Token::BeginBlock, _)) => {
                                let _ = self.collect_block();
                            }
                            Some(_) => {
                                let _ = self.next_expr();
                            }
                            _ => (),
                        }
                    }
                }
                errors.push(ParseError {
                    span: ident_span,
                    reason: ParseErrorReason::DeclarationError,
                });
                return ParserReturns {
                    ast: ValueDeclaration {
                        loc: ident_span,
                        is_op,
                        target: ast::Pattern::Read(ident, ident_span),
                        args,
                        ty: Some(types::ERROR),
                        value: ValueType::Expr(ast::Expr::Error),
                        generictypes: generics,
                        abi,
                    },
                    loc: ident_span,
                    warnings,
                    errors,
                };
            }

            let mut ty = if let Some((Token::Colon, _)) = self.stream.clone().next() {
                let _ = self.stream.next();
                let ty = self.collect_type();
                warnings.extend(ty.warnings);
                errors.extend(ty.errors);
                let ty = ty.ast;
                Some(ty)
            } else {
                None
            };

            let op = match self.stream.next() {
                Some((Token::Op(op), _)) => op,
                Some((Token::Seq, _)) => {
                    if !abi.is_some() {
                        errors.push(ParseError {
                            span: ident_span,
                            reason: ParseErrorReason::DeclarationError,
                        });
                        return ParserReturns {
                            ast: ValueDeclaration {
                                loc: ident_span,
                                is_op,
                                target: ast::Pattern::Read(ident, ident_span),
                                args,
                                ty,
                                value: ValueType::Expr(Expr::Error),
                                generictypes: generics,
                                abi,
                            },
                            loc: ident_span,
                            warnings,
                            errors,
                        };
                    } else if ty.is_none() || args.len() != 0 {
                        errors.push(ParseError {
                            span: ident_span,
                            reason: ParseErrorReason::DeclarationError,
                        });
                        return ParserReturns {
                            ast: ValueDeclaration {
                                loc: ident_span,
                                is_op,
                                target: ast::Pattern::Read(ident, ident_span),
                                args,
                                ty,
                                value: ValueType::Expr(Expr::Error),
                                generictypes: generics,
                                abi,
                            },
                            loc: ident_span,
                            warnings,
                            errors,
                        };
                    } else {
                        return ParserReturns {
                            ast: ValueDeclaration {
                                loc: ident_span,
                                is_op,
                                target: ast::Pattern::Read(ident, ident_span),
                                args,
                                ty,
                                value: ValueType::External,
                                generictypes: generics,
                                abi,
                            },
                            loc: ident_span,
                            warnings,
                            errors,
                        };
                    }
                }
                _ => {
                    //TODO! progress next_toplevel valid point.
                    errors.push(ParseError {
                        span: ident_span,
                        reason: ParseErrorReason::DeclarationError,
                    });
                    return ParserReturns {
                        ast: ValueDeclaration {
                            loc: ident_span,
                            is_op,
                            target: ast::Pattern::Read(ident, ident_span),
                            args,
                            ty,
                            value: ValueType::Expr(Expr::Error),
                            generictypes: generics,
                            abi,
                        },
                        loc: ident_span,
                        warnings,
                        errors,
                    };
                }
            };

            if op != "=" {
                //TODO! progress next_toplevel until valid point.
                errors.push(ParseError {
                    span: ident_span,
                    reason: ParseErrorReason::DeclarationError,
                });
                return ParserReturns {
                    ast: ValueDeclaration {
                        loc: ident_span,
                        is_op,
                        target: ast::Pattern::Read(ident, ident_span),
                        args,
                        ty,
                        value: ValueType::Expr(Expr::Error),
                        generictypes: generics,
                        abi,
                    },
                    loc: ident_span,
                    warnings,
                    errors,
                };
            }

            let value = match self.stream.clone().next() {
                Some((Token::BeginBlock, _)) => ValueType::Function({
                    let block = self.collect_block();
                    warnings.extend(block.warnings);
                    errors.extend(block.errors);
                    block.ast
                }),
                Some((_, _)) => {
                    let ParserReturns {
                        ast,
                        loc: _,
                        warnings: expr_warnings,
                        errors: expr_errors,
                    } = self.next_expr();
                    warnings.extend(expr_warnings);
                    errors.extend(expr_errors);
                    ValueType::Expr(ast)
                }
                _ => {
                    errors.push(ParseError {
                        span: ident_span,
                        reason: ParseErrorReason::UnexpectedEndOfFile,
                    });
                    return ParserReturns {
                        ast: ValueDeclaration {
                            loc: ident_span,
                            is_op,
                            target: ast::Pattern::Read(ident, ident_span),
                            args,
                            ty,
                            value: ValueType::Expr(Expr::Error),
                            generictypes: generics,
                            abi,
                        },
                        loc: ident_span,
                        warnings,
                        errors,
                    };
                }
            };
            if let Some(generics) = &generics {
                for arg in &mut args {
                    generics
                        .decls
                        .iter()
                        .map(|(_, it)| it)
                        .for_each(|name| arg.apply_generic(name));
                }

                if let Some(ty) = &mut ty {
                    *ty = generics
                        .decls
                        .iter()
                        .map(|(_, it)| it)
                        .fold(ty.clone(), |ty, name| ty.replace_user_with_generic(name));
                }
            }

            return ParserReturns {
                ast: ValueDeclaration {
                    loc: ident_span,
                    is_op,
                    target: ast::Pattern::Read(ident, ident_span),
                    args,
                    ty,
                    value,
                    generictypes: generics,
                    abi,
                },
                loc: ident_span,
                warnings,
                errors,
            };
        }
        return ParserReturns {
            ast: ValueDeclaration {
                loc: (0, 0),
                is_op: false,
                target: ast::Pattern::Read("<error>".to_string(), (0, 0)),
                args: vec![ArgDeclaration::Simple {
                    loc: (0, 0),
                    ident: "<error>".to_string(),
                    ty: Some(types::ERROR),
                }],
                ty: Some(types::ERROR),
                value: ValueType::Expr(Expr::Error),
                generictypes: generics,
                abi,
            },
            loc: (0, 0),
            warnings: Vec::new(),
            errors: vec![ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            }],
        };
    }

    fn collect_block(&mut self) -> ParserReturns<Vec<Statement>> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let sub = if let Some((Token::BeginBlock, _)) = self.stream.next() {
            let mut sub_expr = Vec::new();
            loop {
                let next = self.stream.peek();
                if let Some((Token::EndBlock, _)) = next {
                    self.stream.next();
                    break;
                } else if let Some((Token::EoF, _)) = next {
                    break;
                }
                let stmnt = self.next_statement();
                warnings.extend(stmnt.warnings);
                errors.extend(stmnt.errors);
                sub_expr.push(stmnt.ast);
            }
            sub_expr
        } else {
            errors.push(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            });
            Vec::new()
        };
        ParserReturns {
            ast: sub,
            loc: (0, 0),
            warnings,
            errors,
        }
    }

    fn binary_op(&mut self) -> ParserReturns<Expr> {
        let op_loc = self.stream.clone().nth(2).unwrap().1;
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
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
        const PRECIDENCE: [(&'static str, usize, bool); 12] = [
            (".", usize::MAX, true),
            ("**", 7, false),
            ("*", 5, true),
            ("/", 5, true),
            ("+", 4, true),
            ("-", 4, true),
            ("<", 2, true),
            ("<=", 2, true),
            (">", 2, true),
            (">=", 2, true),
            ("&&", 1, true),
            ("||", 1, true),
        ];
        let mut output = Vec::with_capacity(tokens.len());
        let mut op_stack = Vec::with_capacity(tokens.len() / 3);
        let mut token_iter = tokens.into_iter().peekable();
        while let Some((token, _)) = token_iter.peek() {
            match token {
                Token::GroupOpen => {
                    let _ = token_iter.next();
                    let mut group_opens = 0;
                    let sub_tokens = token_iter
                        .clone()
                        .take_while(|(t, _)| match t {
                            Token::GroupClose => {
                                group_opens -= 1;
                                group_opens >= 0
                            }
                            Token::GroupOpen => {
                                group_opens += 1;
                                true
                            }
                            Token::Ident(_)
                            | Token::FloatingPoint(_, _)
                            | Token::Integer(_, _)
                            | Token::Op(_) => true,
                            _ => false,
                        })
                        .collect_vec();
                    for _ in 0..sub_tokens.len() {
                        token_iter.next();
                    }
                    let ParserReturns {
                        ast: result,
                        loc,
                        warnings: expr_warnings,
                        errors: expr_errors,
                    } = Parser::from_stream(sub_tokens.into_iter()).next_expr();
                    warnings.extend(expr_warnings);
                    errors.extend(expr_errors);
                    output.push(ShuntingYardOptions::Expr((result, loc)));
                }
                Token::GroupClose => {
                    let _ = token_iter.next();
                }
                Token::Ident(_) => {
                    if let Some((
                        Token::Ident(_)
                        | Token::CharLiteral(_)
                        | Token::StringLiteral(_)
                        | Token::FloatingPoint(_, _)
                        | Token::Integer(_, _)
                        | Token::GroupOpen,
                        _,
                    )) = token_iter.clone().nth(1)
                    {
                        let sub_tokens = token_iter
                            .clone()
                            .take_while(|(t, _)| match t {
                                Token::GroupClose => {
                                    group_opens -= 1;
                                    group_opens >= 0
                                }
                                Token::GroupOpen => {
                                    group_opens += 1;
                                    true
                                }
                                Token::Ident(_)
                                | Token::FloatingPoint(_, _)
                                | Token::Integer(_, _) => true,
                                Token::Op(_) => group_opens >= 0,
                                _ => false,
                            })
                            .collect_vec();
                        for _ in 0..sub_tokens.len() {
                            token_iter.next();
                        }
                        let ParserReturns {
                            ast: result,
                            loc,
                            warnings: expr_warnings,
                            errors: expr_errors,
                        } = Parser::from_stream(sub_tokens.into_iter()).next_expr();
                        warnings.extend(expr_warnings);
                        errors.extend(expr_errors);

                        output.push(ShuntingYardOptions::Expr((result, loc)));
                    } else {
                        let Some((Token::Ident(ident), loc)) = token_iter.next() else {
                            unreachable!()
                        };
                        output.push(ShuntingYardOptions::Expr((
                            Expr::ValueRead(ident, loc),
                            loc,
                        )));
                    }
                }
                Token::Integer(_, _)
                | Token::FloatingPoint(_, _)
                | Token::CharLiteral(_)
                | Token::StringLiteral(_) => output.push(ShuntingYardOptions::Expr({
                    let Some((token, span)) = token_iter.next() else {
                        return ParserReturns {
                            ast: Expr::Error,
                            loc: (0, 0),
                            warnings: Vec::new(),
                            errors: vec![ParseError {
                                span: (0, 0),
                                reason: ParseErrorReason::UnknownError,
                            }],
                        };
                    };

                    let result = match make_literal(token, span) {
                        Ok(r) => r,
                        Err(e) => {
                            errors.push(e);
                            ast::Expr::Error
                        }
                    };
                    (result, span)
                })),
                Token::Op(_) => {
                    let Some((Token::Op(ident), loc)) = token_iter.next() else {
                        unreachable!()
                    };
                    let (prec, left) = PRECIDENCE
                        .iter()
                        .find_map(|(op, weight, assc)| {
                            if op == &ident {
                                Some((*weight, *assc))
                            } else {
                                None
                            }
                        })
                        .unwrap_or((1, false));
                    if op_stack.is_empty() {
                        op_stack.push((ident, loc));
                        continue;
                    }
                    while let Some(op_back) = op_stack.last() {
                        let back_prec = PRECIDENCE
                            .iter()
                            .find_map(|(op, weight, _)| {
                                if op == &op_back.0 {
                                    Some(*weight)
                                } else {
                                    None
                                }
                            })
                            .unwrap_or(1);
                        if back_prec > prec || (back_prec == prec && left) {
                            let Some(op_back) = op_stack.pop() else {
                                unreachable!()
                            };
                            output.push(ShuntingYardOptions::Op(op_back));
                        } else {
                            op_stack.push((ident.clone(), loc));
                            break;
                        }
                    }
                    if op_stack.last().is_none() {
                        op_stack.push((ident, loc));
                    }
                }
                _ => break,
            }
        }
        output.extend(op_stack.into_iter().rev().map(ShuntingYardOptions::Op));
        let mut final_expr = Vec::<(ast::Expr, crate::Location)>::new();
        for expr in output {
            match expr {
                ShuntingYardOptions::Expr(expr) => final_expr.push(expr),
                ShuntingYardOptions::Op((op, loc)) => {
                    let Some((rhs, _)) = final_expr.pop() else {
                        unreachable!()
                    };
                    let Some((lhs, _)) = final_expr.pop() else {
                        unreachable!()
                    };
                    final_expr.push((
                        ast::Expr::BinaryOpCall(BinaryOpCall {
                            loc,
                            operator: op,
                            lhs: lhs.into(),
                            rhs: rhs.into(),
                        }),
                        loc,
                    ))
                }
            }
        }

        if final_expr.len() != 1 {
            errors.push(ParseError {
                span: op_loc,
                reason: ParseErrorReason::UnknownError,
            });
            ParserReturns {
                ast: Expr::Error,
                loc: op_loc,
                warnings,
                errors,
            }
        } else {
            let (expr, _loc) = final_expr.into_iter().next().unwrap();

            ParserReturns {
                ast: expr,
                loc: op_loc,
                warnings,
                errors,
            }
        }
    }

    fn match_(&mut self) -> ParserReturns<Match> {
        let Some((Token::Match, match_loc)) = self.stream.next() else {
            unreachable!()
        };
        let ParserReturns {
            ast: on,
            loc: _,
            mut warnings,
            mut errors,
        } = self.next_expr();
        if let Some((Token::Where, _)) = self.stream.clone().next() {
            let _ = self.stream.next();
        } else {
            //TODO? recovery?
            let loc = if let Some((_, loc)) = self.stream.clone().next() {
                loc
            } else {
                (0, 0)
            };
            errors.push(ParseError {
                span: loc,
                reason: ParseErrorReason::UnexpectedToken,
            })
        };
        let expect_block = if let Some((Token::BeginBlock, _)) = self.stream.clone().next() {
            let _ = self.stream.next();
            true
        } else {
            false
        };
        let mut arms = Vec::new();
        while let Some((Token::Op(op), _)) = self.stream.peek() {
            if op != "|" {
                break;
            }
            let _ = self.stream.next();
            let pattern = self.collect_pattern();
            warnings.extend(pattern.warnings);
            errors.extend(pattern.errors);
            let loc = pattern.loc;
            let cond = dbg!(pattern.ast);
            if let Some((Token::Arrow, _)) = self.stream.peek() {
                self.stream.next();
            } else {
                let Some((t, loc)) = self.stream.peek() else {
                    unreachable!()
                };
                println!(
                    "expected -> but got {:?} at line: {}, col: {}",
                    t, loc.0, loc.1
                );
            }
            let (block, ret) = match self.stream.peek() {
                Some((Token::BeginBlock, _)) => {
                    let _ = self.stream.next();
                    let mut body = Vec::new();
                    while self
                        .stream
                        .clone()
                        .skip_while(|(it, _)| it != &Token::Seq && it != &Token::EndBlock)
                        .next()
                        .map(|a| a.0 != Token::EndBlock && a.0 != Token::EoF)
                        .unwrap_or(false)
                    {
                        let stmnt = self.next_statement();
                        warnings.extend(stmnt.warnings);
                        errors.extend(stmnt.errors);
                        body.push(stmnt.ast);
                    }
                    match self.stream.peek() {
                        Some((Token::EndBlock, _)) => {
                            let _ = self.stream.next();
                            (body, None)
                        }
                        _ => {
                            let ParserReturns {
                                ast: ret,
                                warnings: ret_warnings,
                                errors: ret_errors,
                                loc: _,
                            } = self.next_expr();
                            warnings.extend(ret_warnings);
                            errors.extend(ret_errors);
                            if let Some((Token::EndBlock, _)) = self.stream.peek() {
                                let _ = self.stream.next();
                            } else {
                                println!("did you mean next_toplevel move back a block?");
                            }
                            (body, Some(ret.into()))
                        }
                    }
                }
                _ => {
                    let ParserReturns {
                        ast: expr,
                        warnings: ret_warnings,
                        errors: ret_errors,
                        loc: _,
                    } = self.next_expr();
                    warnings.extend(ret_warnings);
                    errors.extend(ret_errors);

                    if let Some((Token::Comma, _)) = self.stream.peek() {
                        let _ = self.stream.next();
                    } else {
                        let Some((peeked, loc)) = self.stream.peek() else {
                            unreachable!()
                        };
                        println!(
                            "expected `,` but got {:?} at line : {}, col: {}",
                            peeked, loc.0, loc.1
                        )
                    }
                    (Vec::new(), Some(dbg!(expr).into()))
                }
            };
            arms.push(ast::MatchArm {
                block,
                ret,
                cond,
                loc,
            });
        }

        if expect_block {
            if let Some((Token::EndBlock, _)) = self.stream.peek() {
                let _ = self.stream.next();
            } else {
                println!(
                    "did you mean next_toplevel go back next_toplevel the containing block level?"
                );
            }
        }

        ParserReturns {
            ast: Match {
                loc: match_loc,
                on: on.into(),
                arms,
            },
            loc: match_loc,
            warnings,
            errors,
        }
    }

    fn collect_pattern(&mut self) -> ParserReturns<Pattern> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        // op should poped beffore this.
        let (pattern, loc) = match self.stream.clone().next() {
            Some((Token::CurlOpen, loc)) => {
                let _curl = self.stream.next();
                let mut fields = HashMap::new();
                while let Some((Token::Ident(_), _)) = self.stream.clone().next() {
                    let Some((Token::Ident(field_name), loc)) = self.stream.next() else {
                        unreachable!()
                    };
                    if let Some((Token::Colon, _)) = self.stream.clone().next() {
                        let Some((Token::Colon, _)) = self.stream.next() else {
                            unreachable!()
                        };
                        let sub_pattern = self.collect_pattern();
                        warnings.extend(sub_pattern.warnings);
                        errors.extend(sub_pattern.errors);
                        fields.insert(field_name, sub_pattern.ast);
                    } else {
                        fields.insert(field_name.clone(), Pattern::Read(field_name, loc));
                    }
                    if let Some((Token::Comma, _)) = self.stream.clone().next() {
                        let _comma = self.stream.next();
                    } else {
                        break;
                    }
                }
                if let Some((Token::CurlClose, _)) = self.stream.clone().next() {
                    let _curl = self.stream.next();
                } else {
                    //todo! recovery.
                }
                (
                    Pattern::Destructure(PatternDestructure::Struct {
                        base_ty: None,
                        fields,
                    }),
                    loc,
                )
            }
            Some((Token::Ident(_), _)) => {
                let Some((Token::Ident(name), loc)) = self.stream.next() else {
                    unreachable!()
                };
                match self.stream.clone().next() {
                    Some((Token::Scope, _)) => {
                        let enum_name = name;
                        let Some((_scope, scope_loc)) = self.stream.next() else {
                            unreachable!()
                        };
                        if let Some((Token::Ident(_), _)) = self.stream.clone().next() {
                            let Some((Token::Ident(variant), _)) = self.stream.next() else {
                                unreachable!()
                            };
                            let pattern = match self.stream.clone().next() {
                                Some((
                                    Token::CurlOpen
                                    | Token::GroupOpen
                                    | Token::Ident(_)
                                    | Token::BracketOpen
                                    | Token::Integer(_, _)
                                    | Token::FloatingPoint(_, _),
                                    _,
                                )) => {
                                    let sub_pattern = self.collect_pattern();
                                    warnings.extend(sub_pattern.warnings);
                                    errors.extend(sub_pattern.errors);
                                    Some(sub_pattern.ast.into())
                                }
                                _ => None,
                            };
                            (
                                Pattern::EnumVariant {
                                    ty: Some(enum_name),
                                    variant,
                                    pattern,
                                    loc,
                                },
                                loc,
                            )
                        } else {
                            errors.push(ParseError {
                                span: scope_loc,
                                reason: ParseErrorReason::UnexpectedToken,
                            });
                            (Pattern::Error, loc)
                        }
                    }
                    Some((Token::CurlOpen, _)) => {
                        let mut sub_pattern = self.collect_pattern();
                        warnings.extend(sub_pattern.warnings);
                        errors.extend(sub_pattern.errors);
                        let Pattern::Destructure(PatternDestructure::Struct { base_ty, .. }) =
                            &mut sub_pattern.ast
                        else {
                            unreachable!()
                        };
                        *base_ty = Some(name);
                        (sub_pattern.ast, loc)
                    }
                    Some((
                        Token::GroupOpen
                        | Token::Ident(_)
                        | Token::BracketOpen
                        | Token::Integer(_, _)
                        | Token::FloatingPoint(_, _),
                        _,
                    )) => {
                        let sub_pattern = self.collect_pattern();
                        warnings.extend(sub_pattern.warnings);
                        errors.extend(sub_pattern.errors);
                        (
                            Pattern::EnumVariant {
                                ty: None,
                                variant: name,
                                pattern: Some(sub_pattern.ast.into()),
                                loc,
                            },
                            loc,
                        )
                    }
                    _ => {
                        if name == "_" {
                            (Pattern::Default, loc)
                        } else {
                            // TODO! pattern detection of enum varaints.
                            (Pattern::Read(name, loc), loc)
                        }
                    }
                }
            }
            Some((Token::Integer(_, _), _)) => {
                let Some((Token::Integer(signed, value), loc)) = self.stream.next() else {
                    unreachable!()
                };
                (
                    Pattern::ConstNumber(format!("{}{}", if signed { "-" } else { "" }, value)),
                    loc,
                )
            }
            Some((Token::FloatingPoint(_, _), _)) => {
                let Some((Token::FloatingPoint(signed, value), loc)) = self.stream.next() else {
                    unreachable!()
                };

                (
                    Pattern::ConstNumber(format!("{}{}", if signed { "-" } else { "" }, value)),
                    loc,
                )
            }
            Some((Token::CharLiteral(_), _)) => {
                let Some((Token::CharLiteral(c), loc)) = self.stream.next() else {
                    unreachable!()
                };
                (Pattern::ConstChar(c), loc)
            }
            Some((Token::StringLiteral(_), _)) => {
                let Some((Token::StringLiteral(c), loc)) = self.stream.next() else {
                    unreachable!()
                };
                (Pattern::ConstStr(c), loc)
            }
            Some((Token::True, _)) => {
                let Some((_, loc)) = self.stream.next() else {
                    unreachable!()
                };
                (Pattern::ConstBool(true), loc)
            }
            Some((Token::False, _)) => {
                let Some((_, loc)) = self.stream.next() else {
                    unreachable!()
                };
                (Pattern::ConstBool(false), loc)
            }

            Some((Token::GroupOpen, _)) => {
                let Some((Token::GroupOpen, loc)) = self.stream.next() else {
                    unreachable!()
                };
                if let Some((Token::GroupClose, _)) = self.stream.clone().next() {
                    let _ = self.stream.next();
                    (Pattern::Destructure(PatternDestructure::Unit), loc)
                } else {
                    let first = self.collect_pattern();
                    warnings.extend(first.warnings);
                    errors.extend(first.errors);
                    let first_loc = first.loc;
                    let mut patterns = vec![first.ast];
                    loop {
                        match self.stream.clone().next() {
                            Some((Token::Comma, _)) => {
                                let _ = self.stream.next();
                                let next = self.collect_pattern();
                                warnings.extend(next.warnings);
                                errors.extend(next.errors);
                                patterns.push(next.ast);
                            }
                            Some((Token::GroupClose, _)) => {
                                break;
                            }
                            Some((Token::EoF, _)) | None => {
                                let _ = self.stream.next();
                                errors.push(ParseError {
                                    span: loc,
                                    reason: ParseErrorReason::UnexpectedEndOfFile,
                                });
                                break;
                            }
                            Some((t, loc)) => {
                                let _ = self.stream.next();
                                errors.push(ParseError {
                                    span: loc,
                                    reason: ParseErrorReason::UnexpectedToken,
                                });
                                break;
                            }
                        }
                    }
                    match self.stream.clone().next() {
                        Some((Token::GroupClose, _)) => {
                            let _ = self.stream.next();
                        }
                        Some((Token::EoF, _)) | None => {
                            let _ = self.stream.next();
                            errors.push(ParseError {
                                span: loc,
                                reason: ParseErrorReason::UnexpectedEndOfFile,
                            });
                        }
                        Some((t, loc)) => {
                            let n = self
                                .stream
                                .clone()
                                .peeking_take_while(|(t, _)| match t {
                                    Token::Comma | Token::EndBlock => false,
                                    Token::Op(op) => op != "|",
                                    _ => true,
                                })
                                .collect_vec()
                                .len();
                            for _ in 0..n {
                                let _ = self.stream.next();
                            }
                            errors.push(ParseError {
                                span: loc,
                                reason: ParseErrorReason::UnexpectedToken,
                            });
                        }
                    }
                    if patterns.len() == 1 {
                        (patterns.pop().unwrap(), first_loc)
                    } else {
                        (
                            Pattern::Destructure(PatternDestructure::Tuple(patterns)),
                            loc,
                        )
                    }
                }
            }
            Some((Token::EoF, _)) | None => {
                let _ = self.stream.next();
                errors.push(ParseError {
                    span: (0, 0),
                    reason: ParseErrorReason::UnexpectedEndOfFile,
                });
                (Pattern::Error, (0, 0))
            }
            Some((t, loc)) => {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::UnexpectedToken,
                });
                let n = self
                    .stream
                    .clone()
                    .peeking_take_while(|(t, _)| match t {
                        Token::Comma | Token::EndBlock => false,
                        Token::Op(op) => op != "|",
                        _ => true,
                    })
                    .collect_vec()
                    .len();
                for _ in 0..n {
                    let _ = self.stream.next();
                }
                (Pattern::Error, loc)
            }
        };
        let pattern = if let Some((Token::Op(op), _)) = self.stream.peek() {
            if op == "|" {
                let _ = self.stream.next();
                let next = self.collect_pattern();
                warnings.extend(next.warnings);
                errors.extend(next.errors);
                Pattern::Or(pattern.into(), next.ast.into())
            } else {
                pattern
            }
        } else {
            pattern
        };
        ParserReturns {
            ast: pattern,
            loc,
            warnings,
            errors,
        }
    }

    fn array_literal(&mut self) -> ParserReturns<Expr> {
        let Some((Token::BracketOpen, loc)) = self.stream.next() else {
            unreachable!()
        };
        let mut values = Vec::new();
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let expect_block = if let Some((Token::BeginBlock, _)) = self.stream.peek() {
            let _ = self.stream.next();
            true
        } else {
            false
        };
        loop {
            let ParserReturns {
                ast: expr,
                loc: _,
                warnings: new_warnings,
                errors: new_errors,
            } = self.next_expr();
            warnings.extend(new_warnings);
            errors.extend(new_errors);
            values.push(expr);
            match self.stream.clone().next() {
                Some((Token::Comma, _)) => {
                    let _ = self.stream.next();
                    continue;
                }
                Some(_) => break,
                None => {
                    unreachable!("somehow not even got EoF");
                }
            }
        }

        if expect_block {
            if let Some((Token::EndBlock, _)) = self.stream.peek() {
                let _ = self.stream.next();
            } else {
                println!("did you mean next_toplevel have the closing ] on the same level as the opening?")
            }
        }

        if let Some((Token::BracketClose, _)) = self.stream.peek() {
            let _ = self.stream.next();
        } else {
            errors.push(ParseError {
                span: loc,
                reason: ParseErrorReason::UnbalancedBraces,
            });
            values.push(ast::Expr::Error);
        }
        ParserReturns {
            ast: ast::Expr::ArrayLiteral {
                contents: values,
                loc,
            },
            loc,
            warnings,
            errors,
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
        Token::Integer(is_neg, value) | Token::FloatingPoint(is_neg, value) => {
            Ok(Expr::NumericLiteral {
                value: if is_neg {
                    "-".to_owned() + &value
                } else {
                    value
                },
            })
        }
        Token::True => Ok(Expr::BoolLiteral(true, span)),
        Token::False => Ok(Expr::BoolLiteral(false, span)),
        _ => Err(ParseError {
            span,
            reason: ParseErrorReason::UnknownError,
        }),
    }
}
fn type_from_string(name: &str, generics: Vec<ResolvedType>, loc: crate::Location) -> ResolvedType {
    match name {
        "str" => types::STR,
        "char" => types::CHAR,
        "bool" => types::BOOL,
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
                    loc,
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
                    loc,
                },
            }
        }
        _ => ResolvedType::User {
            name: name.to_string(),
            generics,
            loc,
        },
    }
}
#[allow(unused)]
// TODO use for generating [`crate::ast::Expr::PipeExpr`]
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
        ast::{
            ArgDeclaration, EnumDeclaration, EnumVariant, IfBranching, IfExpr, MatchArm,
            StructDefinition, TopLevelDeclaration,
        },
        types::ResolvedType,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn types() {
        assert_eq!(
            Parser::from_source("int64").collect_type().ast,
            types::INT64
        );
        assert_eq!(
            Parser::from_source("int32").collect_type().ast,
            types::INT32
        );
        assert_eq!(
            Parser::from_source("int16").collect_type().ast,
            types::INT16
        );
        assert_eq!(Parser::from_source("int8").collect_type().ast, types::INT8);
        assert_eq!(Parser::from_source("str").collect_type().ast, types::STR);
        assert_eq!(Parser::from_source("char").collect_type().ast, types::CHAR);
        assert_eq!(
            Parser::from_source("float64").collect_type().ast,
            types::FLOAT64
        );
        assert_eq!(
            Parser::from_source("float32").collect_type().ast,
            types::FLOAT32
        );
        assert_eq!(Parser::from_source("()").collect_type().ast, types::UNIT);

        assert_eq!(
            Parser::from_source("[int32;5]").collect_type().ast,
            ResolvedType::Array {
                underlining: types::INT32.into(),
                size: 5
            }
        );

        //fuctions
        assert_eq!(
            Parser::from_source("int32->int32").collect_type().ast,
            ResolvedType::Function {
                arg: types::INT32.into(),
                returns: types::INT32.into(),
                loc: (0, 0),
            }
        );
        assert_eq!(
            Parser::from_source("int32->int32->int32")
                .collect_type()
                .ast,
            ResolvedType::Function {
                arg: types::INT32.into(),
                returns: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0),
                }
                .into(),
                loc: (0, 0),
            }
        );
        assert_eq!(
            Parser::from_source("int32->(int32->int32)")
                .collect_type()
                .ast,
            ResolvedType::Function {
                arg: types::INT32.into(),
                returns: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0),
                }
                .into(),
                loc: (0, 0),
            }
        );
        assert_eq!(
            Parser::from_source("(int32->int32)->int32")
                .collect_type()
                .ast,
            ResolvedType::Function {
                arg: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0),
                }
                .into(),
                returns: types::INT32.into(),
                loc: (0, 0),
            }
        );
    }

    use super::*;
    #[test]
    #[ignore = "This is for singled out tests"]
    fn for_debugging_only() {
        let mut parser = Parser::from_source(
            "
let a (v:(int32,int32)) =
    let (x,y) = v;
    return ();

let b ((x,y):(int32,int32)) = (); ",
        );
        dbg!(parser.module("".to_string()));
    }
    #[test]
    fn individual_simple_expressions() {
        let mut parser = Parser::from_source("let foo : int32 = 5;");

        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (0, 4),
                is_op: false,
                target: ast::Pattern::Read("foo".to_owned(), (0, 4)),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ValueType::Expr(Expr::NumericLiteral {
                    value: "5".to_string(),
                }),
                generictypes: None,
                abi: None
            }),
            parser.next_statement().ast
        );
        let mut parser = Parser::from_source(
            r#"let foo _ : int32 -> int32 =
    return 5;
"#,
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (0, 4),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 18)
                }),
                args: vec![ArgDeclaration::Discard {
                    ty: None,
                    loc: (0, 8)
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "5".to_string(),
                    },
                    (1, 4)
                )]),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast
        )
    }

    #[test]
    fn higher_kinded() {
        const ARG: &'static str = r#"
let foo _ : ( int32 -> int32 ) -> int32 =
    return 0;
"#;
        let mut parser = Parser::from_source(ARG);
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (1, 4),
                is_op: false,
                target: ast::Pattern::Read("foo".to_owned(), (1, 4)),
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (1, 20)
                    }
                    .into(),
                    returns: types::INT32.into(),
                    loc: (1, 31)
                }),
                args: vec![ast::ArgDeclaration::Discard {
                    ty: None,
                    loc: (1, 8)
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_string(),
                    },
                    (2, 4)
                )]),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast,
            "function as arg"
        );
        const RT: &'static str = r#"
let foo _ : int32 -> ( int32 -> int32 ) =
    return 0;
"#;
        let mut parser = Parser::from_source(RT);
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (1, 4),
                is_op: false,
                target: ast::Pattern::Read("foo".to_owned(), (1, 4)),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (1, 29)
                    }
                    .into(),
                    loc: (1, 18)
                }),
                args: vec![ast::ArgDeclaration::Discard {
                    ty: None,
                    loc: (1, 8)
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_owned(),
                    },
                    (2, 4)
                )]),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast,
            "function as rt"
        );
    }

    #[test]
    fn multiple_statements() {
        const SRC: &'static str = include_str!("../../samples/test.fb");
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (0, 4),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ValueType::Expr(Expr::NumericLiteral {
                    value: "3".to_owned(),
                }),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "simple declaration"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (2, 4),
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (2, 20)
                }),
                args: vec![ast::ArgDeclaration::Simple {
                    ident: "quz".to_string(),
                    loc: (2, 8),
                    ty: None,
                }],
                value: ValueType::Function(vec![
                    Statement::Declaration(ValueDeclaration {
                        loc: (3, 8),
                        is_op: false,
                        target: Pattern::Read("baz".to_owned(), (3, 8)),
                        ty: Some(types::STR),
                        args: Vec::new(),
                        value: ValueType::Expr(Expr::StringLiteral(r#"merp " yes"#.to_string())),
                        generictypes: None,
                        abi: None,
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "2".to_owned(),
                        },
                        (4, 4)
                    )
                ]),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "declaration with block"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (6, 4),
                is_op: true,

                ident: "^^".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (6, 32)
                    }
                    .into(),
                    loc: (6, 23)
                }),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        ident: "lhs".to_string(),
                        loc: (6, 7),
                        ty: None,
                    },
                    ast::ArgDeclaration::Simple {
                        ident: "rhs".to_string(),
                        loc: (6, 11),
                        ty: None,
                    },
                ],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (7, 4),
                        value: Expr::ValueRead("bar".to_string(), (7, 4)).into(),
                        arg: Some(Expr::ValueRead("foo".to_string(), (7, 8)).into()),
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "1".to_owned(),
                        },
                        (8, 4)
                    )
                ]),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "operator declaration w/ function call",
        );
    }

    #[test]
    fn fn_chain() {
        const SRC: &'static str = r#"
let main _ : int32 -> int32 = 
    put_int32 100;
    print_str "v";
    return 32;
"#;
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (1, 4),
                is_op: false,

                target: Pattern::Read("main".to_owned(), (1, 4)),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (1, 19)
                }),
                args: vec![ast::ArgDeclaration::Discard {
                    ty: None,
                    loc: (1, 9)
                }],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (2, 4),
                        value: Expr::ValueRead("put_int32".to_string(), (2, 4)).into(),
                        arg: Some(
                            Expr::NumericLiteral {
                                value: "100".to_owned(),
                            }
                            .into()
                        )
                    }),
                    Statement::FnCall(FnCall {
                        loc: (3, 4),
                        value: Expr::ValueRead("print_str".to_string(), (3, 4)).into(),
                        arg: Some(Expr::StringLiteral("v".to_string()).into())
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "32".to_string(),
                        },
                        (4, 4)
                    )
                ]),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast,
        )
    }

    #[test]
    fn ops() {
        const SRC_S: &'static str = "100 + 100 * foo * ( 10 - 1 )";
        let mut parser = Parser::from_source(SRC_S);

        assert_eq!(
            Expr::BinaryOpCall(BinaryOpCall {
                loc: (0, 4),
                lhs: Expr::NumericLiteral {
                    value: "100".to_string(),
                }
                .into(),
                rhs: Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 16),
                    lhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 10),
                        lhs: Expr::NumericLiteral {
                            value: "100".to_string(),
                        }
                        .into(),
                        rhs: Expr::ValueRead("foo".to_string(), (0, 12)).into(),
                        operator: "*".to_string()
                    })
                    .into(),
                    rhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 23),
                        lhs: Expr::NumericLiteral {
                            value: "10".to_string(),
                        }
                        .into(),
                        rhs: Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .into(),
                        operator: "-".to_string()
                    })
                    .into(),
                    operator: "*".to_string()
                })
                .into(),
                operator: "+".to_string()
            }),
            parser.next_expr().ast
        );
        const SRC: &'static str = r#"let main _ =
    print_int32 100 + 100;
    return 0;"#;
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (0, 4),
                is_op: false,
                target: Pattern::Read("main".to_owned(), (0, 4)),
                ty: None,
                args: vec![ast::ArgDeclaration::Discard {
                    ty: None,
                    loc: (0, 9)
                }],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (1, 4),
                        value: Expr::ValueRead("print_int32".to_owned(), (1, 4)).into(),
                        arg: Some(
                            Expr::BinaryOpCall(BinaryOpCall {
                                loc: (1, 20),
                                operator: "+".to_owned(),
                                lhs: Expr::NumericLiteral {
                                    value: "100".to_owned(),
                                }
                                .into(),
                                rhs: Expr::NumericLiteral {
                                    value: "100".to_owned(),
                                }
                                .into()
                            })
                            .into()
                        )
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "0".to_owned(),
                        },
                        (2, 4)
                    )
                ]),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast
        );
        //a b . 2 + c d . -
        const SRC_MEMBER_ACCESS: &'static str = "a.b + 2 - c.d";
        let mut parser = Parser::from_source(SRC_MEMBER_ACCESS);
        assert_eq!(
            ast::Expr::BinaryOpCall(BinaryOpCall {
                loc: (0, 8),
                lhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 4),
                    lhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 1),
                        lhs: ast::Expr::ValueRead("a".to_string(), (0, 0)).into(),
                        rhs: ast::Expr::ValueRead("b".to_string(), (0, 2)).into(),
                        operator: ".".to_string()
                    })
                    .into(),
                    rhs: ast::Expr::NumericLiteral {
                        value: "2".to_string(),
                    }
                    .into(),
                    operator: "+".to_string()
                })
                .into(),
                rhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 11),
                    lhs: ast::Expr::ValueRead("c".to_string(), (0, 10)).into(),
                    rhs: ast::Expr::ValueRead("d".to_string(), (0, 12)).into(),
                    operator: ".".to_string()
                })
                .into(),
                operator: "-".to_string()
            }),
            parser.next_expr().ast,
        );

        let mut parser = Parser::from_source("(foo bar) && (baz quz)");
        assert_eq!(
            ast::Expr::BinaryOpCall(BinaryOpCall {
                loc: (0, 10),
                lhs: ast::Expr::FnCall(FnCall {
                    loc: (0, 1),
                    value: ast::Expr::ValueRead("foo".to_string(), (0, 1)).into(),
                    arg: Some(ast::Expr::ValueRead("bar".to_string(), (0, 5)).into())
                })
                .into(),
                rhs: ast::Expr::FnCall(FnCall {
                    loc: (0, 14),
                    value: ast::Expr::ValueRead("baz".to_string(), (0, 14)).into(),
                    arg: Some(ast::Expr::ValueRead("quz".to_string(), (0, 18)).into())
                })
                .into(),
                operator: "&&".to_string()
            }),
            parser.next_expr().ast,
            "(foo bar) && (baz quz)"
        );
    }

    #[test]
    fn generics() {
        let mut parser = Parser::from_source("for<T> let test a : T -> T = a");
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (0, 11),
                is_op: false,
                ident: "test".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    loc: (0, 16),
                    ident: "a".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (0, 20)
                    }
                    .into(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (0, 25)
                    }
                    .into(),
                    loc: (0, 22)
                }),
                value: ast::ValueType::Expr(ast::Expr::ValueRead("a".to_string(), (0, 29))),
                generics: Some(ast::GenericsDecl {
                    for_loc: (0, 0),
                    decls: [((0, 4), "T".to_string())].into_iter().collect(),
                }),
                abi: None,
            }),
            parser.next_toplevel().ast,
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
            ast::TopLevelDeclaration::TypeDefinition(ast::TypeDefinition::Struct(
                StructDefinition {
                    ident: "Foo".to_string(),
                    generics: None,
                    values: vec![ast::FieldDecl {
                        name: "a".to_string(),
                        ty: types::INT32,
                        loc: (1, 4),
                    }],
                    loc: (0, 5)
                },
            )),
            parser.next_toplevel().ast,
            "basic"
        );
        assert_eq!(
            ast::TopLevelDeclaration::TypeDefinition(ast::TypeDefinition::Struct(
                StructDefinition {
                    ident: "Tuple".to_string(),
                    generics: Some(ast::GenericsDecl {
                        for_loc: (3, 0),
                        decls: vec![((3, 4), "T".to_string()), ((3, 6), "U".to_string())],
                    }),
                    values: vec![
                        ast::FieldDecl {
                            name: "first".to_string(),
                            ty: ResolvedType::Generic {
                                name: "T".to_string(),
                                loc: (4, 12),
                            },
                            loc: (4, 4)
                        },
                        ast::FieldDecl {
                            name: "second".to_string(),
                            ty: ResolvedType::Generic {
                                name: "U".to_string(),
                                loc: (5, 13)
                            },
                            loc: (5, 4)
                        },
                    ],
                    loc: (3, 14)
                }
            )),
            parser.next_toplevel().ast,
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
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (0, 4),
                is_op: false,
                ident: "foo".to_owned(),
                args: vec![
                    ArgDeclaration::Simple {
                        loc: (0, 8),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ArgDeclaration::Simple {
                        loc: (0, 10),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::User {
                        name: "Bar".to_string(),
                        generics: vec![types::INT32],
                        loc: (0, 14)
                    }
                    .into(),
                    returns: ResolvedType::Function {
                        arg: ResolvedType::User {
                            name: "Baz".to_string(),
                            generics: vec![types::INT32, types::FLOAT64],
                            loc: (0, 28)
                        }
                        .into(),
                        returns: types::INT32.into(),
                        loc: (0, 47)
                    }
                    .into(),
                    loc: (0, 25)
                }),
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_string(),
                    },
                    (1, 4)
                )]),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast
        )
    }

    #[test]
    fn struct_construction() {
        const SRC: &'static str = "Foo { a : 0 }";
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            ast::Expr::StructConstruction(StructConstruction {
                loc: (0, 4),
                fields: HashMap::from([(
                    "a".to_string(),
                    (
                        Expr::NumericLiteral {
                            value: "0".to_string(),
                        },
                        (0, 6)
                    )
                )]),
                generics: Vec::new(),
                ident: "Foo".to_string()
            }),
            parser.next_expr().ast
        );
        assert_eq!(
            ast::Expr::StructConstruction(StructConstruction {
                loc: (0, 14),
                fields: HashMap::from([(
                    "a".to_string(),
                    (
                        Expr::NumericLiteral {
                            value: "0".to_string(),
                        },
                        (0, 16)
                    )
                )]),
                generics: vec![types::INT32],
                ident: "Generic".to_string()
            }),
            Parser::from_source("Generic<int32>{ a:0 }").next_expr().ast
        )
    }

    #[test]
    fn control_flow_if() {
        let mut parser = Parser::from_source(include_str!("../../samples/control_flow_if.fb"));
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (0, 4),
                is_op: false,
                ident: "inline_expr".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    ident: "a".to_string(),
                    loc: (0, 16),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: types::INT32.into(),
                    loc: (0, 25)
                }),
                value: ast::ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (0, 39)).into(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .into()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .into()
                    ),
                    loc: (0, 36)
                })),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "inline_expr"
        );

        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (2, 4),
                is_op: false,
                ident: "out_of_line_expr".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    ident: "a".to_string(),
                    loc: (2, 21),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: types::INT32.into(),
                    loc: (2, 30)
                }),
                value: ast::ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (2, 44)).into(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::FnCall(ast::FnCall {
                            loc: (3, 8),
                            value: ast::Expr::ValueRead("fun".to_string(), (3, 8)).into(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "0".to_string(),
                                }
                                .into()
                            ),
                        })
                        .into()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .into()
                    ),
                    loc: (2, 41)
                })),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "out_of_line_expr"
        );

        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (7, 4),
                is_op: false,
                ident: "expr_with_statement".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    loc: (7, 24),
                    ident: "a".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: types::INT32.into(),
                    loc: (7, 33)
                }),
                value: ast::ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (7, 47)).into(),
                    true_branch: (
                        vec![ast::Statement::FnCall(FnCall {
                            loc: (8, 8),
                            value: ast::Expr::ValueRead("bar".to_string(), (8, 8)).into(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                }
                                .into()
                            ),
                        })],
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .into()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        vec![ast::Statement::FnCall(FnCall {
                            loc: (11, 8),
                            value: ast::Expr::ValueRead("baz".to_string(), (11, 8)).into(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "4".to_string(),
                                }
                                .into()
                            ),
                        })],
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .into()
                    ),
                    loc: (7, 44)
                })),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "expr_with_statement"
        );

        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (14, 4),
                is_op: false,
                ident: "expr_with_else_if".to_owned(),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        loc: (14, 22),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ast::ArgDeclaration::Simple {
                        loc: (14, 24),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.into(),
                        returns: types::INT32.into(),
                        loc: (14, 41)
                    }
                    .into(),
                    loc: (14, 33)
                }),
                value: ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (14, 55)).into(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .into()
                    ),
                    else_ifs: vec![(
                        ast::Expr::ValueRead("b".to_string(), (14, 72)).into(),
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .into()
                    ),],
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "2".to_string(),
                        }
                        .into()
                    ),
                    loc: (14, 52)
                })),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "expr with else if"
        );

        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (16, 4),
                is_op: false,
                ident: "statement".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    loc: (16, 14),
                    ident: "a".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: types::INT32.into(),
                    loc: (16, 23)
                }),
                value: ast::ValueType::Function(vec![ast::Statement::IfStatement(IfBranching {
                    cond: ast::Expr::ValueRead("a".to_string(), (17, 7)).into(),
                    true_branch: vec![
                        ast::Statement::FnCall(FnCall {
                            loc: (18, 8),
                            value: ast::Expr::ValueRead("foo".to_string(), (18, 8)).into(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                }
                                .into()
                            )
                        }),
                        ast::Statement::Return(
                            ast::Expr::NumericLiteral {
                                value: "0".to_string(),
                            },
                            (19, 8)
                        ),
                    ],
                    else_ifs: Vec::new(),
                    else_branch: vec![
                        ast::Statement::FnCall(FnCall {
                            loc: (21, 8),
                            value: ast::Expr::ValueRead("bar".to_string(), (21, 8)).into(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "4".to_string(),
                                }
                                .into()
                            )
                        }),
                        ast::Statement::Return(
                            ast::Expr::NumericLiteral {
                                value: "1".to_string(),
                            },
                            (22, 8)
                        ),
                    ],
                    loc: (17, 4)
                })]),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "statement"
        );

        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (24, 4),
                is_op: false,
                ident: "statement_with_else_if".to_owned(),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        loc: (24, 27),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ast::ArgDeclaration::Simple {
                        loc: (24, 29),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.into(),
                        returns: types::INT32.into(),
                        loc: (24, 46)
                    }
                    .into(),
                    loc: (24, 38)
                }),
                value: ast::ValueType::Function(vec![ast::Statement::IfStatement(IfBranching {
                    cond: ast::Expr::ValueRead("a".to_string(), (25, 7)).into(),
                    true_branch: vec![ast::Statement::Return(
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        },
                        (26, 8)
                    )],
                    else_ifs: vec![(
                        ast::Expr::ValueRead("b".to_string(), (27, 12)).into(),
                        vec![ast::Statement::Return(
                            ast::Expr::NumericLiteral {
                                value: "1".to_string(),
                            },
                            (28, 8)
                        )]
                    )],
                    else_branch: vec![ast::Statement::Return(
                        ast::Expr::NumericLiteral {
                            value: "2".to_string(),
                        },
                        (30, 8)
                    )],
                    loc: (25, 4),
                })]),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "statement_with_else_if"
        );

        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (32, 4),
                is_op: false,
                ident: "expr_multi_with_elseif".to_owned(),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        loc: (32, 27),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ast::ArgDeclaration::Simple {
                        loc: (32, 29),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.into(),
                        returns: types::INT32.into(),
                        loc: (32, 46)
                    }
                    .into(),
                    loc: (32, 38)
                }),
                value: ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (32, 60)).into(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .into(),
                    ),
                    else_ifs: vec![(
                        ast::Expr::ValueRead("b".to_string(), (34, 12)).into(),
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .into(),
                    )],
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "2".to_string(),
                        }
                        .into(),
                    ),
                    loc: (32, 57)
                })),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "multi line expr with else if"
        );
    }

    #[test]
    fn control_flow_match() {
        let mut parser = Parser::from_source(include_str!("../../samples/control_flow_match.fb"));
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (0, 4),
                is_op: false,
                ident: "match_expr_ints".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    loc: (0, 20),
                    ident: "x".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 30)
                }),
                value: ast::ValueType::Expr(ast::Expr::Match(Match {
                    loc: (0, 41),
                    on: ast::Expr::ValueRead("x".to_string(), (0, 47)).into(),
                    arms: vec![
                        MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::NumericLiteral {
                                    value: "1".to_string(),
                                }
                                .into()
                            ),
                            cond: Pattern::ConstNumber("1".to_string()),
                            loc: (1, 6)
                        },
                        MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                }
                                .into()
                            ),
                            cond: Pattern::ConstNumber("2".to_string()),
                            loc: (2, 6)
                        },
                        MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::NumericLiteral {
                                    value: "4".to_string(),
                                }
                                .into()
                            ),
                            cond: Pattern::Default,
                            loc: (3, 6)
                        },
                    ]
                })),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "match_expr_ints"
        );

        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (5, 4),
                is_op: false,
                ident: "match_expr_with_block".to_owned(),
                args: vec![ArgDeclaration::Simple {
                    loc: (5, 26),
                    ident: "x".to_string(),
                    ty: None,
                },],
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (5, 36)
                }),
                value: ValueType::Expr(ast::Expr::Match(Match {
                    loc: (5, 47),
                    on: ast::Expr::ValueRead("x".to_string(), (5, 53)).into(),
                    arms: vec![
                        MatchArm {
                            loc: (6, 6),
                            block: vec![ast::Statement::Declaration(ValueDeclaration {
                                loc: (7, 12),
                                is_op: false,
                                target: Pattern::Read("a".to_owned(), (7, 12)),
                                args: Vec::new(),
                                ty: Some(types::INT32),
                                value: ValueType::Expr(ast::Expr::NumericLiteral {
                                    value: "2".to_string(),
                                }),
                                generictypes: None,
                                abi: None,
                            })],
                            ret: Some(
                                ast::Expr::BinaryOpCall(BinaryOpCall {
                                    loc: (8, 9),
                                    lhs: ast::Expr::ValueRead("a".to_string(), (8, 8)).into(),
                                    rhs: ast::Expr::NumericLiteral {
                                        value: "3".to_string(),
                                    }
                                    .into(),
                                    operator: "*".to_string()
                                })
                                .into()
                            ),
                            cond: Pattern::ConstNumber("1".to_string()),
                        },
                        MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::NumericLiteral {
                                    value: "2".to_string(),
                                }
                                .into()
                            ),
                            cond: Pattern::ConstNumber("2".to_string()),
                            loc: (9, 6)
                        },
                        MatchArm {
                            loc: (10, 6),
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::BinaryOpCall(BinaryOpCall {
                                    loc: (10, 12),
                                    lhs: ast::Expr::ValueRead("a".to_string(), (10, 11)).into(),
                                    rhs: ast::Expr::NumericLiteral {
                                        value: "2".to_string(),
                                    }
                                    .into(),
                                    operator: "/".to_string()
                                })
                                .into()
                            ),
                            cond: Pattern::Read("a".to_string(), (10, 6)),
                        },
                    ]
                })),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "match_expr_with_block"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (12, 4),
                is_op: false,
                ident: "match_statement".to_owned(),
                args: vec![ArgDeclaration::Simple {
                    loc: (12, 20),
                    ident: "x".to_string(),
                    ty: None,
                },],
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::UNIT.into(),
                    loc: (12, 30)
                }),
                value: ValueType::Function(vec![ast::Statement::Match(Match {
                    loc: (13, 4),
                    on: ast::Expr::ValueRead("x".to_string(), (13, 10)).into(),
                    arms: vec![
                        MatchArm {
                            block: vec![ast::Statement::FnCall(FnCall {
                                loc: (15, 8),
                                value: ast::Expr::ValueRead("foo".to_string(), (15, 8)).into(),
                                arg: Some(
                                    ast::Expr::NumericLiteral {
                                        value: "0".to_string(),
                                    }
                                    .into()
                                )
                            })],
                            ret: None,
                            cond: Pattern::ConstNumber("1".to_string()),
                            loc: (14, 6),
                        },
                        MatchArm {
                            block: vec![ast::Statement::FnCall(FnCall {
                                loc: (17, 8),
                                value: ast::Expr::ValueRead("bar".to_string(), (17, 8)).into(),
                                arg: Some(
                                    ast::Expr::NumericLiteral {
                                        value: "1".to_string(),
                                    }
                                    .into()
                                )
                            })],
                            ret: None,
                            cond: Pattern::ConstNumber("2".to_string()),
                            loc: (16, 6),
                        },
                        MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::FnCall(FnCall {
                                    loc: (18, 11),
                                    value: ast::Expr::ValueRead("baz".to_string(), (18, 11)).into(),
                                    arg: Some(
                                        ast::Expr::NumericLiteral {
                                            value: "2".to_string(),
                                        }
                                        .into()
                                    )
                                })
                                .into()
                            ),
                            cond: Pattern::ConstNumber("3".to_string()),
                            loc: (18, 6),
                        },
                    ]
                })]),
                generics: None,
                abi: None,
            }),
            parser.next_toplevel().ast,
            "match_statement",
        );
    }
    #[test]
    fn arrays() {
        const SRC: &'static str = r#"
let arr = [0,0,0,0];
"#;
        let arr = Parser::from_source(SRC).next_toplevel().ast;
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "arr".to_owned(),
                args: Vec::new(),
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::ArrayLiteral {
                    contents: vec![
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                    ],
                    loc: (1, 10)
                }),
                generics: None,
                abi: None,
            }),
            arr,
            "arrays"
        )
    }

    #[test]
    fn abi() {
        const SRC: &'static str = r#"
extern "C" let putchar : int32 -> int32;
extern "C" let ex (a:int32) b = a + b;
"#;
        let mut parser = Parser::from_source(SRC);
        let putchar = parser.next_toplevel().ast;
        let ex = parser.next_toplevel().ast;
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (1, 15),
                is_op: false,
                ident: "putchar".to_owned(),
                args: Vec::new(),
                ty: Some(types::INT32.fn_ty(&types::INT32)),
                value: ValueType::External,
                generics: None,
                abi: Some(ast::Abi {
                    loc: (1, 0),
                    identifier: "C".to_string(),
                }),
            }),
            putchar,
            "input function"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (2, 15),
                is_op: false,
                ident: "ex".to_owned(),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        loc: (2, 19),
                        ident: "a".to_string(),
                        ty: Some(types::INT32),
                    },
                    ast::ArgDeclaration::Simple {
                        loc: (2, 28),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: None,
                value: ValueType::Expr(Expr::BinaryOpCall(BinaryOpCall {
                    loc: (2, 34),
                    lhs: Expr::ValueRead("a".to_string(), (2, 32)).into(),
                    rhs: Expr::ValueRead("b".to_string(), (2, 36)).into(),
                    operator: "+".to_string()
                })),
                generics: None,
                abi: Some(ast::Abi {
                    loc: (2, 0),
                    identifier: "C".to_string(),
                }),
            }),
            ex,
            "output function."
        )
    }

    #[test]
    fn tuples() {
        const SRC: &'static str = "
let ty _ : (int32,int32)->int32 = 0

let cons a : int32 -> (int32,int32) = (a,0)
";

        let module = Parser::from_source(SRC).module("".to_string()).ast;
        let [ty, cons] = &module.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            &ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "ty".to_owned(),
                args: vec![ArgDeclaration::Discard {
                    ty: None,
                    loc: (1, 7)
                }],
                ty: Some(
                    ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32],
                        loc: (0, 0)
                    }
                    .fn_ty(&types::INT32)
                ),
                value: ValueType::Expr(Expr::NumericLiteral {
                    value: "0".to_string()
                }),
                generics: None,
                abi: None,
            }),
            ty
        );

        assert_eq!(
            &ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (3, 4),
                is_op: false,
                ident: "cons".to_owned(),
                args: vec![ArgDeclaration::Simple {
                    loc: (3, 9),
                    ident: "a".to_string(),
                    ty: None
                }],
                ty: Some(types::INT32.fn_ty(&ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32],
                    loc: (0, 0)
                })),
                value: ValueType::Expr(Expr::TupleLiteral {
                    contents: vec![
                        Expr::ValueRead("a".to_string(), (3, 39)),
                        Expr::NumericLiteral {
                            value: "0".to_string()
                        }
                    ],
                    loc: (3, 38)
                }),
                generics: None,
                abi: None
            }),
            cons
        );
    }

    #[test]
    fn match_patterns() {
        let pattern = Parser::from_source("a").collect_pattern().ast;
        assert_eq!(Pattern::Read("a".to_string(), (0, 0)), pattern, "a");
        let pattern = Parser::from_source("_").collect_pattern().ast;
        assert_eq!(Pattern::Default, pattern, "_");
        let pattern = Parser::from_source("(a,b)").collect_pattern().ast;
        assert_eq!(
            Pattern::Destructure(PatternDestructure::Tuple(vec![
                Pattern::Read("a".to_string(), (0, 1)),
                Pattern::Read("b".to_string(), (0, 3)),
            ])),
            pattern,
            "destruct tuple"
        );
        let pattern = Parser::from_source("0 | 1").collect_pattern().ast;
        assert_eq!(
            Pattern::Or(
                Pattern::ConstNumber("0".to_string()).into(),
                Pattern::ConstNumber("1".to_string()).into(),
            ),
            pattern,
            "or (0 or 1)"
        );
        let pattern = Parser::from_source("0 | 1 | 2").collect_pattern().ast;
        assert_eq!(
            Pattern::Or(
                Pattern::ConstNumber("0".to_string()).into(),
                Pattern::Or(
                    Pattern::ConstNumber("1".to_string()).into(),
                    Pattern::ConstNumber("2".to_string()).into(),
                )
                .into()
            ),
            pattern,
            "or (0 or 1 or 2)"
        );
        let pattern = Parser::from_source("(0 | 1,b)").collect_pattern().ast;
        assert_eq!(
            Pattern::Destructure(PatternDestructure::Tuple(vec![
                Pattern::Or(
                    Pattern::ConstNumber("0".to_string()).into(),
                    Pattern::ConstNumber("1".to_string()).into(),
                ),
                Pattern::Read("b".to_string(), (0, 7)),
            ])),
            pattern,
            "destruct tuple"
        );
    }

    #[test]
    fn arg_types() {
        const SRC: &'static str = "
let simple a = ();
let decon_arg (a,b) = ();
let discard _ = ();
let unit () = ();
let annotated_arg_tuple ((x,y):(int32,int32)) = ();
";

        let ast = Parser::from_source(SRC).module("".to_string()).ast;
        let [simple, decon, discard, unit, annotated_decon] = &ast.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            &TopLevelDeclaration::Value(TopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "simple".to_owned(),
                args: vec![ArgDeclaration::Simple {
                    loc: (1, 11),
                    ident: "a".to_string(),
                    ty: None,
                },],
                ty: None,
                value: ValueType::Expr(Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            simple,
            "let simple a = ();"
        );
        assert_eq!(
            &TopLevelDeclaration::Value(TopLevelValue {
                loc: (2, 4),
                is_op: false,
                ident: "decon_arg".to_owned(),
                args: vec![ArgDeclaration::DestructureTuple(
                    vec![
                        ArgDeclaration::Simple {
                            loc: (2, 15),
                            ident: "a".to_string(),
                            ty: None,
                        },
                        ArgDeclaration::Simple {
                            loc: (2, 17),
                            ident: "b".to_string(),
                            ty: None,
                        },
                    ],
                    None,
                    (2, 14)
                ),],
                ty: None,
                value: ValueType::Expr(Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            decon,
            "let decon_arg (a,b) = ()"
        );
        assert_eq!(
            &TopLevelDeclaration::Value(TopLevelValue {
                loc: (3, 4),
                is_op: false,
                ident: "discard".to_owned(),
                args: vec![ArgDeclaration::Discard {
                    loc: (3, 12),
                    ty: None,
                },],
                ty: None,
                value: ValueType::Expr(Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            discard,
            "let discard _ = ();"
        );
        assert_eq!(
            &TopLevelDeclaration::Value(TopLevelValue {
                loc: (4, 4),
                is_op: false,
                ident: "unit".to_owned(),
                args: vec![ArgDeclaration::Unit {
                    loc: (4, 9),
                    ty: None,
                },],
                ty: None,
                value: ValueType::Expr(Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            unit,
            "let unit () = ();"
        );
        assert_eq!(
            &TopLevelDeclaration::Value(TopLevelValue {
                loc: (5, 4),
                is_op: false,
                ident: "annotated_arg_tuple".to_owned(),
                args: vec![ArgDeclaration::DestructureTuple(
                    vec![
                        ArgDeclaration::Simple {
                            loc: (5, 26),
                            ident: "x".to_string(),
                            ty: None,
                        },
                        ArgDeclaration::Simple {
                            loc: (5, 28),
                            ident: "y".to_string(),
                            ty: None,
                        },
                    ],
                    Some(ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32,],
                        loc: (5, 31)
                    }),
                    (5, 25)
                ),],
                ty: None,
                value: ValueType::Expr(Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            annotated_decon,
            "let annotated_arg_tuple ((x,y):(int32,int32)) = ();"
        );
    }
    #[test]
    fn destructuring_statement() {
        let mut parser = super::Parser::from_source(
            "
let (x,y) = v;
let ((x,y),z) = v;
let (x,y,z) = v;
let (x,y):(int32,int32) = v;
//yes this will all fail typecheking.
",
        );
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (1, 4),
                is_op: false,
                target: Pattern::Destructure(PatternDestructure::Tuple(vec![
                    Pattern::Read("x".to_string(), (1, 5)),
                    Pattern::Read("y".to_string(), (1, 7)),
                ])),
                args: Vec::new(),
                ty: None,
                value: ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (1, 12))),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast
        );

        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (2, 4),
                is_op: false,
                target: Pattern::Destructure(PatternDestructure::Tuple(vec![
                    Pattern::Destructure(PatternDestructure::Tuple(vec![
                        Pattern::Read("x".to_string(), (2, 6)),
                        Pattern::Read("y".to_string(), (2, 8)),
                    ])),
                    Pattern::Read("z".to_string(), (2, 11))
                ])),
                args: Vec::new(),
                ty: None,
                value: ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (2, 16))),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast
        );
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (3, 4),
                is_op: false,
                target: Pattern::Destructure(PatternDestructure::Tuple(vec![
                    Pattern::Read("x".to_string(), (3, 5)),
                    Pattern::Read("y".to_string(), (3, 7)),
                    Pattern::Read("z".to_string(), (3, 9)),
                ])),
                args: Vec::new(),
                ty: None,
                value: ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (3, 14))),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast
        );
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (4, 4),
                is_op: false,
                target: Pattern::Destructure(PatternDestructure::Tuple(vec![
                    Pattern::Read("x".to_string(), (4, 5)),
                    Pattern::Read("y".to_string(), (4, 7)),
                ])),
                args: Vec::new(),
                ty: Some(ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32],
                    loc: (4, 10),
                }),
                value: ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (4, 26))),
                generictypes: None,
                abi: None,
            }),
            parser.next_statement().ast
        );
    }

    #[test]
    fn enums() {
        const SRC: &'static str = "
enum Basic = | None | AnInt int32 | Struct { a: int32 }
for<T> enum Option = | Some T | None
for<T,E> enum Result = | Ok T | Err E
";
        let mut parser = Parser::from_source(SRC);
        let basic = parser.next_toplevel().ast;
        assert_eq!(
            TopLevelDeclaration::TypeDefinition(TypeDefinition::Enum(EnumDeclaration {
                ident: "Basic".to_string(),
                generics: None,
                values: vec![
                    EnumVariant::Unit {
                        ident: "None".to_string(),
                        loc: (1, 15)
                    },
                    EnumVariant::Tuple {
                        ident: "AnInt".to_string(),
                        loc: (1, 22),
                        ty: types::INT32
                    },
                    EnumVariant::Struct {
                        ident: "Struct".to_string(),
                        fields: vec![FieldDecl {
                            name: "a".to_string(),
                            ty: types::INT32,
                            loc: (1, 45)
                        }],
                        loc: (1, 36)
                    }
                ],
                loc: (1, 5),
            })),
            basic,
            "basic: enum Basic = | None | AnInt int32 | Struct {{ a: int32 }}"
        );
        let option = parser.next_toplevel().ast;
        assert_eq!(
            TopLevelDeclaration::TypeDefinition(TypeDefinition::Enum(EnumDeclaration {
                ident: "Option".to_string(),
                generics: Some(GenericsDecl {
                    for_loc: (2, 0),
                    decls: vec![((2, 4), "T".to_string())]
                }),
                values: vec![
                    EnumVariant::Tuple {
                        ident: "Some".to_string(),
                        ty: ResolvedType::Generic {
                            name: "T".to_string(),
                            loc: (2, 28)
                        },
                        loc: (2, 23)
                    },
                    EnumVariant::Unit {
                        ident: "None".to_string(),
                        loc: (2, 32)
                    }
                ],
                loc: (2, 12)
            })),
            option,
            "option: for<T> enum Option = | Some T | None"
        );

        let result = parser.next_toplevel().ast;
        assert_eq!(
            TopLevelDeclaration::TypeDefinition(TypeDefinition::Enum(EnumDeclaration {
                ident: "Result".to_string(),
                generics: Some(GenericsDecl {
                    for_loc: (3, 0),
                    decls: vec![((3, 4), "T".to_string()), ((3, 6), "E".to_string()),]
                }),
                values: vec![
                    EnumVariant::Tuple {
                        ident: "Ok".to_string(),
                        ty: ResolvedType::Generic {
                            name: "T".to_string(),
                            loc: (3, 28)
                        },
                        loc: (3, 25)
                    },
                    EnumVariant::Tuple {
                        ident: "Err".to_string(),
                        ty: ResolvedType::Generic {
                            name: "E".to_string(),
                            loc: (3, 35)
                        },
                        loc: (3, 32)
                    },
                ],
                loc: (3, 14)
            })),
            result,
            "result: for<T,E> enum Result = | Ok T | Err E"
        );

        assert!(!parser.has_next());
    }

    #[test]
    fn enum_patterns() {
        const SRC: &'static str = r#"
match a where
| Enum::Complex { a: 0, b } -> b, // TODO! struct patterns
// | Enum::Complex c -> c.a // TODO! struct access.
| Enum::Simple (0 | 1) -> 0,
| Simple a -> a,
"#;
        let ast = Parser::from_source(SRC).match_().ast;

        assert_eq!(
            ast::Match {
                loc: (1, 0),
                on: ast::Expr::ValueRead("a".to_string(), (1, 6)).into(),
                arms: vec![
                    MatchArm {
                        block: Vec::new(),
                        ret: Some(ast::Expr::ValueRead("b".to_string(), (2, 31)).into()),
                        cond: Pattern::EnumVariant {
                            ty: Some("Enum".to_string()),
                            variant: "Complex".to_string(),
                            pattern: Some(
                                Pattern::Destructure(PatternDestructure::Struct {
                                    base_ty: None,
                                    fields: [
                                        ("a".to_string(), Pattern::ConstNumber("0".to_string())),
                                        ("b".to_string(), Pattern::Read("b".to_string(), (2, 24))),
                                    ]
                                    .into(),
                                })
                                .into()
                            ),
                            loc: (2, 2)
                        },
                        loc: (2, 2),
                    },
                    MatchArm {
                        block: Vec::new(),
                        ret: Some(
                            ast::Expr::NumericLiteral {
                                value: "0".to_string()
                            }
                            .into()
                        ),
                        cond: Pattern::EnumVariant {
                            ty: Some("Enum".to_string()),
                            variant: "Simple".to_string(),
                            pattern: Some(
                                Pattern::Or(
                                    Pattern::ConstNumber("0".to_string()).into(),
                                    Pattern::ConstNumber("1".to_string()).into(),
                                )
                                .into()
                            ),
                            loc: (4, 2)
                        },
                        loc: (4, 2),
                    },
                    MatchArm {
                        block: Vec::new(),
                        ret: Some(ast::Expr::ValueRead("a".to_string(), (5, 14)).into()),
                        cond: Pattern::EnumVariant {
                            ty: None,
                            variant: "Simple".to_string(),
                            pattern: Some(Pattern::Read("a".to_string(), (5, 9)).into()),
                            loc: (5, 2)
                        },
                        loc: (5, 2),
                    },
                ],
            },
            ast,
            "match"
        );
    }
}

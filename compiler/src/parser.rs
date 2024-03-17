use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
    str::Chars,
};

use ast::TypeDefinition;
use itertools::Itertools;

use crate::{
    ast::{
        self, ArgDeclaration, BinaryOpCall, Expr, FieldDecl, FnCall, Match, Pattern, Statement, StructConstruction, StructDefinition, ValueDeclaration, ValueType
    }, lexer::TokenStream, tokens::Token, types::{self, ResolvedType}, util::ExtraUtilFunctions
};

use thiserror::Error;

#[derive(Error,Debug)]
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
    UnexpectedToken,
    NoElseBlock,
    UnknownError, //internal use.  should basically never be hit.
}


pub struct ParserReturns<T> {
    pub ast:T,
    pub loc:crate::Location,
    pub warnings:Vec<Warning>, //TODO! add warnings and a way to treat warnings as errors
    pub errors : Vec<ParseError>
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
    pub(crate) fn module(mut self, name: String) -> ParserReturns<ast::ModuleDeclaration> {
        let mut decls = Vec::new();
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        while self.has_next() {
            let decl = self.declaration();
            warnings.extend(decl.warnings);
            errors.extend(decl.errors);
            decls.push(decl.ast);
        }
        ParserReturns{
            ast:ast::ModuleDeclaration {
                loc: None,
                name,
                declarations: decls,
            },
            loc:(0,0),
            warnings,
            errors,
        }
    }

    pub fn next_statement(&mut self) -> ParserReturns<Statement> {
        match self.stream.clone().next() {
            Some((Token::Let, _)) | Some((Token::For, _)) => {
                let ParserReturns { ast:generics, loc, mut warnings,mut errors } = self.collect_generics();
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
                    ast:Statement::Declaration(inner.ast),
                    loc,
                    warnings,
                    errors
                }
            }
            Some((Token::Return, _)) => {
                let inner = self.ret();
                if let Some((Token::Seq, _)) = self.stream.clone().next() {
                    self.stream.next();
                } else {
                    println!("expected ; on line {}", inner.loc.0);
                    //TODO! convert to an error that is added and returned as part of the ast.
                }
                inner
            }
            // hmmm should handle if it's actually a binary op call esp and/or compose.
            Some((Token::Ident(_), _)) =>
            // for now last statement in a block is not treated as return though will allow for that soon.
            {
                let ParserReturns { ast, loc, warnings, errors }= self.function_call();
                let inner = Statement::FnCall(ast);
                if let Some((Token::Seq, _)) = self.stream.clone().next() {
                    self.stream.next();
                } else {
                    println!("expected ; on line {}", inner.get_loc().0);
                    //TODO! convert to a warning that is added and returned as part of the ast.
                }
                ParserReturns {
                    ast:inner,
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
                    errors:inner.errors,
                }
            }
            Some((Token::Match, _)) => {
                let ParserReturns {ast:match_, loc, warnings, errors } = self.match_();
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

    pub fn next_expr(&mut self) -> ParserReturns<ast::Expr>{
        match self.stream.clone().next() {
            Some((Token::If, loc)) => {
                let ParserReturns { ast, loc:_, warnings, errors } = self.ifexpr();
                ParserReturns {
                    ast: Expr::If(ast),
                    loc,
                    warnings,
                    errors
                }
            },
            Some((Token::BracketOpen, _)) => {
                let arr = self.array_literal();
                // Ok((arr.0, arr.1))
                todo!("array.")
            }
            Some((Token::If, loc)) => Ok((ast::Expr::If(self.ifexpr()?.0), loc)),
            Some((Token::GroupOpen, loc))
                if matches!(self.stream.clone().nth(1), Some((Token::GroupClose, _))) =>
            {
                self.stream.next();
                self.stream.next();
                return ParserReturns {
                    ast:Expr::UnitLiteral,
                    loc,
                    warnings:Vec::new(),
                    errors:Vec::new()
                };
            }
            Some((Token::GroupOpen, _)) => {
                let mut group_opens = 0;
                if let Some((Token::Op(_), _)) = self
                    .stream
                    .clone()
                    .skip(1)
                    .skip_while(|(it, _)| match it {
                        //skip this whole expression to examine what's after it.
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
                    let out = self.next_expr();
                    if let Some((Token::GroupClose, _)) = self.stream.peek() {
                        self.stream.next();
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
                    let ParserReturns { ast, loc, warnings, errors, } = self.struct_construct();
                    ParserReturns {
                        ast : Expr::StructConstruction(ast),
                        loc,
                        warnings,
                        errors,
                    }
                } else {
                    self.binary_op()
                }
            }
            Some((Token::ArrayOpen, loc)) => {
                let mut contents = Vec::new();
                let mut errs = Vec::new();
                let mut warnings = Vec::new();
                let _ = self.stream.next();
                while let Some((tkn, _)) = self.stream.clone().next() {
                    match tkn {
                        Token::ArrayClose => break,
                        _ => {
                            let ParserReturns {ast:value, errors : new_errs, loc:_, warnings:new_warnings} = self.next_expr();
                            errs.extend(new_errs);
                            warnings.extend(new_warnings);
                            contents.push(value);
                            match self.stream.clone().next() {
                                Some((Token::Comma, _)) => {
                                    let _ = self.stream.next();
                                }
                                Some((Token::ArrayClose, _)) => (), // do nothing as to generate an error if anything but these two
                                _ => {
                                    if let Some((_,loc)) = self.stream.clone().next() {
                                        errs.push(ParseError {
                                            span:loc,
                                            reason:ParseErrorReason::UnexpectedToken
                                        });
                                    }
                                    else {
                                        errs.push(ParseError {
                                            span:(0,0),
                                            reason:ParseErrorReason::UnexpectedEndOfFile
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                let _ = self.stream.next();

                ParserReturns {
                    ast:Expr::ArrayLiteral { contents, loc }, 
                    loc,
                    warnings,
                    errors:errs
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
            )) => {
                self.literal()
            },
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
                    let ParserReturns { ast, loc, warnings, errors } = self.function_call();
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
                    let ParserReturns { ast, loc, warnings, errors, } = self.struct_construct();
                    ParserReturns {
                        ast : Expr::StructConstruction(ast),
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
                    ast : Expr::BoolLiteral(true, loc),
                    loc,
                    warnings:Vec::new(),
                    errors: Vec::new(),
                }
            }
            Some((Token::False, loc)) => {
                let _ = self.stream.next();
                ParserReturns {
                    ast : Expr::BoolLiteral(false, loc),
                    loc,
                    warnings:Vec::new(),
                    errors: Vec::new(),
                }
            }
            Some((Token::Match, _)) => {
                let ParserReturns { ast, loc, warnings, errors } = self.match_();
                ParserReturns {
                    ast: Expr::Match(ast),
                    loc,
                    warnings,
                    errors,
                }
            }
            _ => ParserReturns {
                ast : Expr::Error,
                loc:(0,0),
                warnings : Vec::new(),
                errors : vec![
                    ParseError {
                        span: (0,0),
                        reason: ParseErrorReason::UnknownError,
                    }
                ]
            },
        }
    }

    fn ifexpr(&mut self) -> ParserReturns<ast::IfExpr> {
        let Some((Token::If, if_loc)) = self.stream.next() else {
            unreachable!()
        };
        let ParserReturns {ast:root_cond, loc:_,mut warnings, mut errors } = self.next_expr();
        if let Some((Token::Then, _)) = self.stream.peek() {
            let _ = self.stream.next();
        } else {
            let (_token, loc) = self.stream.next().unwrap();
            // TODO! recovery?
            errors.push(ParseError{ span:loc, reason:ParseErrorReason::UnexpectedToken});
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
            _ => (
                Vec::new(),
                {
                    let ret = self.next_expr();
                    warnings.extend(ret.warnings);
                    errors.extend(ret.errors);
                    ret.ast
                },
            ),
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
                let cond = cond.ast.boxed();

                if let Some((Token::Then, _)) = self.stream.peek() {
                    let _ = self.stream.next();
                } else {
                    let (_token, loc) = self.stream.next().unwrap();
                    // TODO! recovery?
                    errors.push(ParseError{ span:loc, reason:ParseErrorReason::UnexpectedToken});
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
                    _ => 
                    {
                        let ret = self.next_expr();
                        warnings.extend(ret.warnings);
                        errors.extend(ret.errors);
                        (
                            Vec::new(),
                            ret.ast
                        )
                    },
                };
                else_ifs.push((cond, body, ret.boxed()));

                if let Some((Token::Else, _loc)) = self.stream.clone().next() {
                    let _else_token = self.stream.next();
                } else {
                    //todo! recovery?
                    errors.push(ParseError{
                        span:loc,
                        reason: ParseErrorReason::UnexpectedToken
                    });
                    return ParserReturns {
                        ast : ast::IfExpr {
                            cond:root_cond.boxed(),
                            loc:if_loc,
                            true_branch:(true_body.0,true_body.1.boxed()),
                            else_ifs,
                            else_branch:(Vec::new(),ast::Expr::Error.boxed())
                        },
                        loc:if_loc,
                        warnings,
                        errors,
                    }
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
                    (body, ret.ast.boxed())
                }
                _ => 
                {
                    let ret = self.next_expr();
                    warnings.extend(ret.warnings);
                    errors.extend(ret.errors);
                    (
                        Vec::new(),
                        ret.ast.boxed()
                    )
                },
            };
            
            ParserReturns {
                ast:ast::IfExpr {
                    cond:root_cond.boxed(),
                    true_branch: (true_body.0,true_body.1.boxed()),
                    else_ifs,
                    else_branch,
                    loc:if_loc,
                },
                loc:if_loc,
                warnings,
                errors
            }
            
        } else {
            if let Some((_,loc)) = self.stream.clone().next() {
                errors.push(ParseError {
                    span:loc,
                    reason:ParseErrorReason::UnexpectedToken
                });
            } 
            // TODO! recover
            ParserReturns {
                ast:ast::IfExpr {
                    cond: root_cond.boxed(),
                    true_branch:(true_body.0,true_body.1.boxed()),
                    else_ifs:Vec::new(),
                    else_branch : (
                        Vec::new(),
                        Expr::Error.boxed()
                    ),
                    loc:if_loc,
                },
                loc:if_loc,
                warnings,
                errors,
            }
        }
    }

    fn value(&mut self) -> ParserReturns<Expr> {
        if let Some((Token::Ident(ident), loc)) = self.stream.clone().next() {
            let _ = self.stream.next();
            ParserReturns {
                ast:ast::Expr::ValueRead(ident, loc),
                loc,
                warnings:Vec::new(),
                errors : Vec::new(),
            }
        } else {
            let loc = if let Some((_,loc))=self.stream.peek() {
                *loc 
            } else {
                return ParserReturns {
                    ast:Expr::Error,
                    loc:(0,0),
                    warnings:Vec::new(),
                    errors : vec![
                        ParseError {
                            span:(0,0),
                            reason : ParseErrorReason::UnexpectedEndOfFile
                        }
                    ]
                } 
            };
            ParserReturns {
                ast:Expr::Error,
                loc:loc,
                warnings:Vec::new(),
                errors :vec![
                    ParseError {
                        span : loc,
                        reason: ParseErrorReason::UnknownError
                    }
                ]
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
                    Some(Token::GroupOpen) =>{
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
            let (ast,_loc) = values.into_iter().fold(
                (
                    FnCall {
                        loc:value_loc,
                        value: ast::Expr::ValueRead(ident, value_loc).boxed(),
                        arg: None,
                    },
                    value_loc,
                ),
                |(inner,loc), (next, next_loc)| {
                    if let FnCall {
                        value, arg: None, ..
                    } = inner
                    {
                        (
                            FnCall {
                                loc,
                                value,
                                arg: Some(next.boxed()),
                            },
                            next_loc,
                        )
                    } else {
                        (
                            FnCall {
                                loc,
                                value: Expr::FnCall(inner).boxed(),
                                arg: Some(next.boxed()),
                            },
                            next_loc,
                        )
                    }
                },
            );
            ParserReturns {
                ast,
                loc:value_loc,
                warnings,
                errors,
            }
        } else {
            errors.push(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            });
            ParserReturns {
                ast:FnCall {
                    value : Expr::Error.boxed(),
                    arg : Some(Expr::Error.boxed()),
                    loc:(0,0)
                },
                loc:(0,0),
                warnings,
                errors
            }
        }
    }

    fn struct_construct(&mut self) -> ParserReturns<StructConstruction> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let Some((_,loc)) = self.stream.clone().next() else { unreachable!("ICE: somehow reached struct construction with no token?")};
        let (name,generics) = match self.collect_type() {
            Ok(ResolvedType::User { name, generics, loc:_ }) => (name,generics),
            Ok(_) => {
                errors.push(ParseError{
                    span:loc,
                    reason:ParseErrorReason::InvalidIdent,
                });
                ("<error>".to_string(), vec![types::ERROR])
            }
            Err(e) => {
                errors.push(e);
                ("<error>".to_string(), vec![types::ERROR])
            },
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
            let ParserReturns { ast:expr, loc:_, warnings:expr_warnings, errors:expr_errors } = self.next_expr();
            warnings.extend(expr_warnings);
            errors.extend(expr_errors);
            if fields.contains_key(&ident) {
                let (_, loc): &(_, crate::Location) = &fields[&ident];
                errors.push(ParseError {
                    span:*loc,
                    reason:ParseErrorReason::DeclarationError,
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
            fields.insert("".to_string(), (Expr::Error,(0,0)));
            return ParserReturns {
                ast : StructConstruction {
                    loc,
                    fields,
                    generics,
                    ident: "".to_string(),
                },
                loc,
                warnings,
                errors,
            }
        };
        ParserReturns {
            ast:StructConstruction {
                loc,
                fields,
                generics,
                ident: name,
            },
            loc,
            warnings,
            errors
        }
    }

    fn ret(&mut self) -> ParserReturns<Statement> {
        let (token, span) = self.stream.next().unwrap();
        if token == Token::Return {
            let ParserReturns { ast:expr, loc:_, warnings, errors } = self.next_expr();
            ParserReturns {
                ast:ast::Statement::Return(expr, span),
                loc:span,
                warnings,
                errors,
            }
        } else {
            ParserReturns {
                ast: Statement::Error,
                loc:span,
                warnings :Vec::new(),
                errors :vec![ParseError {
                    span,
                    reason: ParseErrorReason::UnknownError,
                }],
            }
        }
    }

    fn literal(&mut self) -> ParserReturns<Expr> {
        let (token, span) = self.stream.next().unwrap();
        match make_literal(token, span) {
            Ok(lit) => {
                ParserReturns {
                    ast:lit,
                    loc:span,
                    warnings:Vec::new(),
                    errors:Vec::new(),
                }
            }
            Err(e) => {
                ParserReturns {
                    ast:Expr::Error,
                    loc:span,
                    warnings:Vec::new(),
                    errors:vec![e]
                }
            }
        }
    }

    fn ifstatement(&mut self) -> ParserReturns<crate::ast::IfBranching> {
        let Some((Token::If, if_loc)) = self.stream.next() else {
            unreachable!()
        };
        let ParserReturns { ast:cond, mut warnings, mut errors, loc:_ } = self.next_expr();
        let cond = cond.boxed();
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
            return ParserReturns{
                ast:ast::IfBranching{ 
                    cond, 
                    true_branch: vec![Statement::Error], 
                    else_ifs: Vec::new(), 
                    else_branch: Vec::new(), 
                    loc:if_loc 
                },
                loc:if_loc,
                warnings,
                errors,
            }
        };

        let body = match self.stream.peek() {
            Some((Token::BeginBlock, _)) =>{
                let block = self.collect_block();
                warnings.extend(block.warnings);
                errors.extend(block.errors);
                block.ast
            },
            _ => {
                let stmnt = self.next_statement();
                warnings.extend(stmnt.warnings);
                errors.extend(stmnt.errors);
                vec![stmnt.ast]
            },
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

                let ParserReturns { ast:cond, warnings:new_warnings, errors:new_errors, loc:_ }= self.next_expr();
                warnings.extend(new_warnings);
                errors.extend(new_errors);
                let cond = cond.boxed();
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
                    Some((Token::BeginBlock, _)) =>{
                        let block = self.collect_block();
                        warnings.extend(block.warnings);
                        errors.extend(block.errors);
                        block.ast
                    },
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
                    Some((Token::BeginBlock, _)) =>{
                        let block = self.collect_block();
                        warnings.extend(block.warnings);
                        errors.extend(block.errors);
                        block.ast
                    },
                    _ => {
                        let stmnt = self.next_statement();
                        warnings.extend(stmnt.warnings);
                        errors.extend(stmnt.errors);
                        vec![stmnt.ast]
                    },
                }
            } else {
                Vec::new()
            };
            ParserReturns {
                ast:ast::IfBranching {
                    cond,
                    true_branch: body,
                    else_ifs,
                    else_branch,
                    loc:if_loc,
                },
                loc:if_loc,
                warnings,
                errors,
            }
        } else {
            ParserReturns {
                ast:ast::IfBranching {
                    cond,
                    true_branch: body,
                    else_ifs:Vec::new(),
                    else_branch:Vec::new(),
                    loc:if_loc,
                },
                loc:if_loc,
                warnings,
                errors,
            }
        }
    }

    pub(crate) fn collect_type(&mut self) -> Result<ResolvedType, ParseError> {
        let ty = self.stream.next();
        match ty {
            Some((Token::Ident(ty), loc)) => {
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
                if let Some((Token::Arrow, fn_loc)) = self.stream.clone().next() {
                    self.stream.next();
                    let result = self.collect_type()?;

                    let ty = type_from_string(&ty, generic_args, loc);
                    Ok(ResolvedType::Function {
                        arg: ty.boxed(),
                        returns: result.boxed(),
                    loc:fn_loc
                    })
                } else {
                    let ty = type_from_string(&ty, generic_args, loc);
                    Ok(ty)
                }
            }
            Some((Token::GroupOpen, span)) => {
                if let Some((Token::GroupClose, _)) = self.stream.peek() {
                    self.stream.next();
                    return if let Some((Token::Arrow, arr_loc)) = self.stream.clone().next() {
                        self.stream.next();
                        Ok(ResolvedType::Function {
                            arg: types::UNIT.boxed(),
                            returns: self.collect_type()?.boxed(),
                        loc:arr_loc
                        })
                    } else {
                        Ok(types::UNIT)
                    };
                }
                let ty = self.collect_type()?;
                if let Some((Token::GroupClose, _)) = self.stream.next() {
                    if let Some((Token::Arrow, arr_loc)) = self.stream.clone().next() {
                        self.stream.next();
                        let result = self.collect_type()?;
                        Ok(ResolvedType::Function {
                            arg: ty.boxed(),
                            returns: result.boxed(),
                        loc:arr_loc
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
            }
            Some((Token::BracketOpen, loc)) => {
                let ty = self.collect_type()?;
                match self.stream.clone().next() {
                    Some((Token::Seq, _)) => {
                        let _ = self.stream.next();
                        match self.stream.clone().next() {
                            Some((Token::Integer(false, _), _)) => {
                                let Some((Token::Integer(_, value),_)) = self.stream.next() else { unreachable!() };
                                match self.stream.peek() {
                                    Some((Token::BracketClose, _)) => {
                                        let _ = self.stream.next();
                                        Ok(ResolvedType::Array {
                                            underlining: ty.boxed(),
                                            size: value.parse().unwrap(),
                                        })
                                    }
                                    Some((t, loc)) => {
                                        println!(
                                            "unexpected token:{t:?} at line:{}, col:{}",
                                            loc.0, loc.1
                                        );
                                        Err(ParseError {
                                            span: *loc,
                                            reason: ParseErrorReason::UnbalancedBraces,
                                        })
                                    }
                                    None => {
                                        unreachable!("how did this happen.  this is an ice please report it.");
                                    }
                                }
                            }
                            Some((t, loc)) => {
                                println!("unexpected token:{t:?} at line:{}, col:{}", loc.0, loc.1);
                                Err(ParseError {
                                    span: loc,
                                    reason: ParseErrorReason::UnbalancedBraces,
                                })
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
                        Ok(ResolvedType::Slice {
                            underlining: ty.boxed(),
                        })
                    }
                    Some((t, loc)) => {
                        println!("unexpected token:{t:?} at line:{}, col:{}", loc.0, loc.1);
                        Err(ParseError {
                            span: loc,
                            reason: ParseErrorReason::UnbalancedBraces,
                        })
                    }
                    None => {
                        unreachable!("how did this happen.  this is an ICE please report it.");
                    }
                }
            }
            Some((_, span)) => Err(ParseError {
                span,
                reason: ParseErrorReason::TypeError,
            }),
            None => Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::TypeError,
            }),
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
            Some((Token::Extern,loc)) => {
                let _ = self.stream.next();
                let mut errors = Vec::new();
                let abi = if let Some((Token::StringLiteral(_),_)) = self.stream.clone().next() {
                    let Some((Token::StringLiteral(name),_)) = self.stream.next() else { unreachable!() };
                    name
                } else {
                    errors.push(ParseError {
                        span:loc,
                        reason: ParseErrorReason::UnexpectedToken,
                    });
                    "".to_string()
                };
                ParserReturns {
                    ast : Some(ast::Abi{
                        loc,
                        identifier:abi,
                    }),
                    loc,
                    warnings:Vec::new(),
                    errors,
                }
            },
            _ => ParserReturns {
                ast: None,
                loc:(0,0),
                warnings: Vec::new(),
                errors : Vec::new(),
            }
        }
    }

    fn declaration(&mut self) -> ParserReturns<ast::Declaration> {
        let ParserReturns { ast:abi, loc:extern_loc, mut warnings, mut errors } = self.abi();
        let ParserReturns { ast:generics, loc:for_loc, warnings:for_warnings, errors:for_errors } = self.collect_generics();
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
            Some((Token::For, loc)) => {
                errors.push(ParseError {
                    span: loc,
                    reason: ParseErrorReason::DeclarationError,
                });
                ParserReturns {
                    ast:ast::Declaration::Value(ValueDeclaration {
                        loc,
                        is_op: false,
                        ident: "<error>".to_string(),
                        args: vec![
                            ArgDeclaration{ 
                                loc, 
                                ident: "<error>".to_string(),
                                ty: Some(types::ERROR) 
                            }
                        ],
                        ty: Some(types::ERROR),
                        value: ValueType::Expr(Expr::Error),
                        generictypes: generics,
                        abi:None
                    }),
                    loc,
                    warnings,
                    errors,
                }
            },
            Some((Token::Type | Token::Enum, loc)) => {
                if abi.is_some() {
                    errors.push(ParseError {
                        span : extern_loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                }
                let decl = self.type_decl(generics);
                warnings.extend(decl.warnings);
                errors.extend(decl.errors);
                ParserReturns {
                    ast:ast::Declaration::TypeDefinition(decl.ast),
                    loc,
                    warnings,
                    errors,
                }
            }
            Some((Token::Let, _)) => {
                let decl =self.fn_declaration(abi,generics);
                warnings.extend(decl.warnings);
                errors.extend(decl.errors);
                ParserReturns {
                    ast:ast::Declaration::Value(decl.ast),
                    loc:decl.loc,
                    warnings,
                    errors,
                }
            }
            Some((Token::Seq, _)) => {
                let _ =self.stream.next();
                self.declaration()
            }
            _ => unimplemented!(),
        }
    }

    fn type_decl(&mut self, generics: Option<ast::GenericsDecl>) -> ParserReturns<ast::TypeDefinition> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let Some((t,loc)) = self.stream.next() else {unreachable!()};
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
                        }
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
                    }
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
                    }
                }
                match self.stream.peek() {
                    Some((Token::Ident(_), _)) => {
                        let ty = match self.collect_type() {
                            Ok(ty) => ty,
                            Err(e) => {
                                errors.push(e);
                                types::ERROR
                            }
                        };
                        ParserReturns {
                            ast:ast::TypeDefinition::Alias(ident, ty),
                            loc,
                            warnings,
                            errors
                        }
                    }
                    Some((Token::CurlOpen, _)) => {
                        
                        let strct = self.struct_declaration(ident, generics, loc);
                        warnings.extend(strct.warnings);
                        errors.extend(strct.errors);
                        ParserReturns {
                            ast:ast::TypeDefinition::Struct(strct.ast),
                            loc,
                            warnings,
                            errors,
                        }
                    },
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
                            ast:ast::TypeDefinition::Alias("<error>".to_string(), types::ERROR),
                            loc,
                            warnings,
                            errors,
                        }
                    },
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
                    ty:ResolvedType::Error,
                    loc
                });
                let ts = self.stream.clone().take_while(|(it,_)| match it {
                    Token::Comma | Token::CurlClose | Token::EndBlock => false,
                    _ => true,
                }).collect_vec();
                for _ in ts {
                    let _ = self.stream.next();
                }
                continue;
            };
            let ty = match self.collect_type(){
                Ok(ty) => ty,
                Err(e) => {
                    errors.push(e);
                    types::ERROR
                }
            };
            let ty = if let Some(generics) = &generics {
                generics.decls.iter().map(|(_,it)| it).fold(ty, |result, it| {
                    result.replace_user_with_generic(it)
                }) 
            }else {
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
                ast:StructDefinition {
                    ident,
                    generics,
                    values: fields,
                    loc,
                },
                loc,
                warnings,
                errors,
            }
        };
        ParserReturns {
            ast:StructDefinition {
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
            let Some((_,for_loc)) = self.stream.next() else { unreachable!() };
            match self.stream.next() {
                Some((Token::Op(op), _)) if op == "<" => {
                    let mut out = self
                        .stream
                        .clone()
                        .filter(|(token, _)| &Token::Comma != token)
                        .take_while(|(token, _)| &Token::Op(">".to_string()) != token)
                        .collect_vec();
                    // TODO should probably fold to ensure that it is comma seperated.
                    let first_span = out.first().unwrap().1;
                    let num = out.len();
                    out.dedup_by_key(|(it,_)| it.clone());
                    if out.len() == num {
                        Some(ast::GenericsDecl {
                            for_loc,
                            decls:out.into_iter()
                            .filter_map(|(t, loc)| {
                                let Token::Ident(name) = t else { return None };
                                Some((loc,name))
                            })
                            .collect()
                        })
                    } else {
                        errors.push(ParseError {
                            span: first_span,
                            reason: ParseErrorReason::DeclarationError,
                        });
                        return ParserReturns {
                            ast : None,
                            loc:first_span,
                            warnings,
                            errors
                        };
                    }
                }
                _ => {
                    errors.push(ParseError {
                        span: for_loc,
                        reason: ParseErrorReason::DeclarationError,
                    });
                    return ParserReturns {
                        ast : None,
                        loc:for_loc,
                        warnings,
                        errors
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
            loc:generics.as_ref().map(|it| it.for_loc).unwrap_or_default(),
            ast:generics,
            warnings,
            errors
        }
    }

    fn collect_args(&mut self) -> ParserReturns<Vec<ast::ArgDeclaration>> {
        let mut out = Vec::new();
        #[allow(unused_mut)]
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        while let Some((t, _)) = self.stream.clone().next() {
            match t {
                Token::GroupOpen => {
                    let _ = self.stream.next();
                    let Some((Token::Ident(name), loc)) = self.stream.next() else {
                        unimplemented!("unit arg destructring not allowed yet.")
                    };
                    let ty = if let Some((Token::Colon, _)) = self.stream.clone().next() {
                        let _ = self.stream.next();
                        let ty = match self.collect_type(){
                            Ok(ty) => ty,
                            Err(e) => {
                                errors.push(e);
                                types::ERROR
                            }
                        };
                        if let Some((Token::GroupClose, _)) = self.stream.clone().next() {
                            let _ = self.stream.next();
                        } else {
                            errors.push(ParseError {
                                span: loc,
                                reason: ParseErrorReason::UnbalancedBraces,
                            });
                        }
                        Some(ty)
                    } else {
                        None
                    };
                    out.push(ast::ArgDeclaration {
                        loc,
                        ident: name,
                        ty,
                    });
                }
                Token::Ident(_) => {
                    let Some((Token::Ident(ident), loc)) = self.stream.next() else {
                        unreachable!()
                    };
                    out.push(ast::ArgDeclaration {
                        loc,
                        ident,
                        ty: None,
                    })
                }
                _ => break,
            }
        }
        ParserReturns {
            ast:out,
            loc:(0,0),//this is discarded.
            warnings,
            errors,
        }
    }

    fn fn_declaration(&mut self, abi:Option<ast::Abi>, generics: Option<ast::GenericsDecl>) -> ParserReturns<ValueDeclaration> {
        if let Some((Token::Let, _start)) = self.stream.next() {
            let (token, ident_span) = self.stream.next().unwrap();
            let (ident, is_op) = match token {
                Token::Ident(ident) => (ident, false),
                Token::Op(ident) => (ident, true),
                _ => {
                    return ParserReturns {
                        ast:ValueDeclaration {
                            loc: ident_span,
                            is_op: false,
                            ident: "<error>".to_string(),
                            args: vec![
                                ArgDeclaration { loc: (0,0), ident: "<error>".to_string(), ty: Some(types::ERROR) }
                            ],
                            ty: Some(types::ERROR),
                            value: ValueType::Expr(Expr::Error),
                            generictypes:generics,
                            abi,
                        },
                        loc:ident_span,
                        warnings:Vec::new(),
                        errors:vec![ParseError {
                            span: ident_span,
                            reason: ParseErrorReason::DeclarationError,
                        }]
                    }
                }
            };
            let ParserReturns { ast:mut args, loc:_, mut warnings,mut errors } = self.collect_args();
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
                    ast : ValueDeclaration { 
                        loc:ident_span,
                        is_op,
                        ident,
                        args,
                        ty: Some(types::ERROR),
                        value: ValueType::Expr(ast::Expr::Error),
                        generictypes:generics,
                        abi,
                    },
                    loc:ident_span,
                    warnings,
                    errors,
                }
            }

            let mut ty = if let Some((Token::Colon, _)) = self.stream.clone().next() {
                let _ = self.stream.next();
                Some(match self.collect_type() {
                    Ok(ty) => ty,
                    Err(e) => {
                        errors.push(e);
                        types::ERROR
                    }
                })
            } else {
                None
            };

            let op = match self.stream.next() {
                Some((Token::Op(op), _)) => op,
                Some((Token::Seq,_)) => {
                    if !abi.is_some() {
                        errors.push(ParseError {
                            span:ident_span,
                            reason:ParseErrorReason::DeclarationError,
                        });
                        return ParserReturns {
                            ast:ValueDeclaration {
                                loc:ident_span,
                                is_op,
                                ident,
                                args,
                                ty,
                                value:ValueType::Expr(Expr::Error),
                                generictypes:generics,
                                abi
                            },
                            loc:ident_span,
                            warnings,
                            errors,
                        }
                    } else if ty.is_none() || args.len() != 0 {
                        errors.push(ParseError {
                            span:ident_span,
                            reason:ParseErrorReason::DeclarationError,
                        });
                        return ParserReturns {
                            ast:ValueDeclaration {
                                loc:ident_span,
                                is_op,
                                ident,
                                args,
                                ty,
                                value:ValueType::Expr(Expr::Error),
                                generictypes:generics,
                                abi
                            },
                            loc:ident_span,
                            warnings,
                            errors,
                        }
                    } else {
                        return ParserReturns {
                            ast: ValueDeclaration {
                                loc:ident_span,
                                is_op,
                                ident,
                                args,
                                ty,
                                value:ValueType::External,
                                generictypes:generics,
                                abi,
                            },
                            loc: ident_span,
                            warnings,
                            errors,
                        }
                    }
                    
                }
                _ => {
                //TODO! progress to valid point.
                errors.push(ParseError {
                    span: ident_span,
                    reason: ParseErrorReason::DeclarationError,
                });
                return ParserReturns {
                    ast:ValueDeclaration {
                        loc: ident_span,
                        is_op, 
                        ident,
                        args, 
                        ty, 
                        value: ValueType::Expr(Expr::Error), 
                        generictypes: generics,
                        abi,
                    },
                    loc:ident_span,
                    warnings,
                    errors,
                }
            }
            };

            if op != "=" {
                //TODO! progress to until valid point.
                errors.push(ParseError {
                    span: ident_span,
                    reason: ParseErrorReason::DeclarationError,
                });
                return ParserReturns {
                    ast:ValueDeclaration {
                        loc: ident_span,
                        is_op, 
                        ident,
                        args, 
                        ty, 
                        value: ValueType::Expr(Expr::Error), 
                        generictypes: generics,
                        abi,
                    },
                    loc:ident_span,
                    warnings,
                    errors,
                }
            }

            let value = match self.stream.clone().next() {
                Some((Token::BeginBlock, _)) => ValueType::Function({
                    let block = self.collect_block();
                    warnings.extend(block.warnings);
                    errors.extend(block.errors);
                    block.ast
                }),
                Some((_, _)) => {
                    let ParserReturns {ast, loc:_, warnings:expr_warnings, errors:expr_errors} =self.next_expr();
                    warnings.extend(expr_warnings);
                    errors.extend(expr_errors);
                    ValueType::Expr(ast)
                },
                _ => {
                    errors.push(ParseError {
                        span: ident_span,
                        reason: ParseErrorReason::UnexpectedEndOfFile,
                    });
                    return ParserReturns {
                        ast:ValueDeclaration{
                            loc: ident_span,
                            is_op,
                            ident,
                            args,
                            ty,
                            value: ValueType::Expr(Expr::Error),
                            generictypes: generics,
                            abi,
                        },
                        loc:ident_span,
                        warnings,
                        errors,
                    }
                }
            };
            if let Some(generics) = &generics {

                for arg in &mut args {
                    if let Some(ty) = &mut arg.ty {
                        
                        *ty = generics
                            .decls
                            .iter()
                            .map(|(_,it)|it)
                            .fold(ty.clone(), |ty, name| ty.replace_user_with_generic(name));
                    }
                }

                if let Some(ty) = &mut ty {
                    *ty = generics
                        .decls
                        .iter()
                        .map(|(_,it)|it)
                        .fold(ty.clone(), |ty, name| ty.replace_user_with_generic(name));
                }
            }

            return ParserReturns {
                ast:ValueDeclaration {
                    loc: ident_span,
                    is_op,
                    ident,
                    args,
                    ty,
                    value,
                    generictypes: generics,
                    abi
                },
                loc:ident_span,
                warnings,
                errors,
            };
        }
        return ParserReturns {
            ast:ValueDeclaration {
                loc: (0,0),
                is_op:false,
                ident: "<error>".to_string(),
                args: vec![
                    ArgDeclaration { loc: (0,0), ident: "<error>".to_string(), ty: Some(types::ERROR) }
                ],
                ty: Some(types::ERROR),
                value: ValueType::Expr(Expr::Error),
                generictypes:generics,
                abi,
            },
            loc:(0,0),
            warnings:Vec::new(),
            errors:vec![ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            }],
        };
    }

    fn collect_block(&mut self) -> ParserReturns<Vec<Statement>> {
        let mut warnings =Vec::new();
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
                let stmnt= self.next_statement();
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
            ast:sub,
            loc:(0,0),
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
                    let ParserReturns {ast:result, loc, warnings:expr_warnings, errors:expr_errors, } = Parser::from_stream(sub_tokens.into_iter()).next_expr();
                    warnings.extend(expr_warnings);
                    errors.extend(expr_errors);
                    output.push(ShuntingYardOptions::Expr((result,loc)));
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
                        let ParserReturns {ast:result, loc, warnings:expr_warnings, errors:expr_errors, } = Parser::from_stream(sub_tokens.into_iter()).next_expr();
                        warnings.extend(expr_warnings);
                        errors.extend(expr_errors);
                    
                        output.push(ShuntingYardOptions::Expr((result,loc)));
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
                            ast : Expr::Error,
                            loc:(0,0),
                            warnings: Vec::new(),
                            errors : vec![
                                ParseError {
                                    span: (0,0), 
                                    reason: ParseErrorReason::UnknownError,
                                }
                            ]
                        }
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
                            lhs: lhs.boxed(),
                            rhs: rhs.boxed(),
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
                ast:Expr::Error,
                loc:op_loc,
                warnings,
                errors,
            }
        } else {
            let (expr,_loc) = final_expr.into_iter().next().unwrap();

            ParserReturns {
                ast:expr,
                loc:op_loc,
                warnings,
                errors
            }
        }
    }

    fn match_(&mut self) -> ParserReturns<Match> {
        let Some((Token::Match, match_loc)) = self.stream.next() else {
            unreachable!()
        };
        let ParserReturns { ast:on, loc:_, mut warnings, mut errors } = self.next_expr();
        if let Some((Token::Where, _)) = self.stream.clone().next() {
            let _ = self.stream.next();
        } else {
            //TODO? recovery?
            let loc = if let Some((_,loc)) = self.stream.clone().next() {
                loc
            } else {
                (0,0)
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
            let (cond, loc) = match self.stream.peek() {
                Some((Token::Ident(ident), _)) if ident != "_" => {
                    let Some((Token::Ident(name), loc)) = self.stream.next() else {
                        unreachable!()
                    };
                    //todo! detect patterns
                    (Pattern::Read(name), loc)
                }
                Some((Token::Ident(_), _)) => {
                    let Some((_, loc)) = self.stream.next() else {
                        unreachable!()
                    };
                    (Pattern::Default, loc)
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
                    let Some((Token::FloatingPoint(signed, value), loc)) = self.stream.next()
                    else {
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
                Some((t, loc)) => {
                    println!("Unexpected token at line : {}, col : {}, epxected identifier or literal but got {:?}", loc.0,loc.1, t);
                    let advanced_by = self
                        .stream
                        .clone()
                        .skip_while(|(t, _)| match t {
                            Token::Comma | Token::EndBlock => false,
                            _ => true,
                            //this will probably need to be more advance to recover from more situations but should do for now.
                        })
                        .collect_vec()
                        .len();
                    for _ in 0..advanced_by {
                        let _ = self.stream.next();
                    }
                    continue;
                }
                None => {
                    unreachable!();
                }
            };

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
                            let ParserReturns { ast:ret, warnings:ret_warnings, errors:ret_errors, loc:_ } = self.next_expr();
                            warnings.extend(ret_warnings);
                            errors.extend(ret_errors);
                            if let Some((Token::EndBlock, _)) = self.stream.peek() {
                                let _ = self.stream.next();
                            } else {
                                println!("did you mean to move back a block?");
                            }
                            (body, Some(ret.boxed()))
                        }
                    }
                }
                _ => {
                    let ParserReturns { ast:expr, warnings:ret_warnings, errors:ret_errors, loc:_ } = self.next_expr();
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
                    (Vec::new(), Some(expr.boxed()))
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
                println!("did you mean to go back to the containing block level?");
            }
        }

        ParserReturns {
            ast:Match {
                loc: match_loc,
                on: on.boxed(),
                arms,
            },
            loc:match_loc,
            warnings,
            errors
        }
        
    }

    fn array_literal(&mut self) -> Result<(ast::Expr, crate::Location), ParseError> {
        let Some((Token::BracketOpen,loc)) = self.stream.next() else {unreachable!()};
        let mut values = Vec::new();
        let expect_block = if let Some((Token::BeginBlock, _)) = self.stream.peek() {
            let _ = self.stream.next();
            true
        } else {
            false
        };
        loop {
            let expr = match self.next_expr() {
                Ok(it) => it.0,
                Err(e) => {
                    println!("{e:?}");
                    ast::Expr::Error
                }
            };
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
                println!("did you mean to have the closing ] on the same level as the opening?")
            }
        }

        if let Some((Token::BracketClose, _)) = self.stream.peek() {
            let _ = self.stream.next();
            Ok((
                ast::Expr::ArrayLiteral {
                    contents: values,
                    loc,
                },
                loc,
            ))
        } else {
            Err(ParseError {
                span: loc,
                reason: ParseErrorReason::UnbalancedBraces,
            })
        }
    }

    fn array_literal(&mut self) -> Result<(ast::Expr, crate::Location), ParseError> {
        let Some((Token::BracketOpen,loc)) = self.stream.next() else {unreachable!()};
        let mut values = Vec::new();
        let expect_block = if let Some((Token::BeginBlock, _)) = self.stream.peek() {
            let _ = self.stream.next();
            true
        } else {
            false
        };
        loop {
            let expr = match self.next_expr() {
                Ok(it) => it.0,
                Err(e) => {
                    println!("{e:?}");
                    ast::Expr::Error
                }
            };
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
                println!("did you mean to have the closing ] on the same level as the opening?")
            }
        }

        if let Some((Token::BracketClose, _)) = self.stream.peek() {
            let _ = self.stream.next();
            Ok((
                ast::Expr::ArrayLiteral {
                    contents: values,
                    loc,
                },
                loc,
            ))
        } else {
            Err(ParseError {
                span: loc,
                reason: ParseErrorReason::UnbalancedBraces,
            })
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
fn type_from_string(name: &str, generics: Vec<ResolvedType>, loc:crate::Location) -> ResolvedType {
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
        ast::{ArgDeclaration, IfBranching, IfExpr, MatchArm, StructDefinition},
        types::ResolvedType,
    };

    #[test]
    fn types() {
        assert_eq!(
            Parser::from_source("int64").collect_type().unwrap(),
            types::INT64
        );
        assert_eq!(
            Parser::from_source("int32").collect_type().unwrap(),
            types::INT32
        );
        assert_eq!(
            Parser::from_source("int16").collect_type().unwrap(),
            types::INT16
        );
        assert_eq!(
            Parser::from_source("int8").collect_type().unwrap(),
            types::INT8
        );
        assert_eq!(
            Parser::from_source("str").collect_type().unwrap(),
            types::STR
        );
        assert_eq!(
            Parser::from_source("char").collect_type().unwrap(),
            types::CHAR
        );
        assert_eq!(
            Parser::from_source("float64").collect_type().unwrap(),
            types::FLOAT64
        );
        assert_eq!(
            Parser::from_source("float32").collect_type().unwrap(),
            types::FLOAT32
        );
        assert_eq!(
            Parser::from_source("()").collect_type().unwrap(),
            types::UNIT
        );

        assert_eq!(
            Parser::from_source("[int32;5]").collect_type().unwrap(),
            ResolvedType::Array {
                underlining: types::INT32.boxed(),
                size: 5
            }
        );

        //fuctions
        assert_eq!(
            Parser::from_source("int32->int32").collect_type().unwrap(),
            ResolvedType::Function {
                arg: types::INT32.boxed(),
                returns: types::INT32.boxed()
            }
        );
        assert_eq!(
            Parser::from_source("int32->int32->int32")
                .collect_type()
                .unwrap(),
            ResolvedType::Function {
                arg: types::INT32.boxed(),
                returns: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }
                .boxed()
            }
        );
        assert_eq!(
            Parser::from_source("int32->(int32->int32)")
                .collect_type()
                .unwrap(),
            ResolvedType::Function {
                arg: types::INT32.boxed(),
                returns: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }
                .boxed()
            }
        );
        assert_eq!(
            Parser::from_source("(int32->int32)->int32")
                .collect_type()
                .unwrap(),
            ResolvedType::Function {
                arg: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }
                .boxed(),
                returns: types::INT32.boxed(),
            }
        );
    }

    use super::*;
    #[test]
    #[ignore = "This is for singled out tests"]
    fn for_debugging_only() {
        let mut parser = Parser::from_source("(foo bar) && (baz quz)");
        assert_eq!(
            ast::Expr::BinaryOpCall(BinaryOpCall {
                loc: (0, 11),
                lhs: ast::Expr::FnCall(FnCall {
                    loc: (0, 6),
                    value: ast::Expr::ValueRead("foo".to_string(), (0, 2)).boxed(),
                    arg: Some(ast::Expr::ValueRead("bar".to_string(), (0, 6)).boxed())
                })
                .boxed(),
                rhs: ast::Expr::FnCall(FnCall {
                    loc: (0, 19),
                    value: ast::Expr::ValueRead("baz".to_string(), (0, 15)).boxed(),
                    arg: Some(ast::Expr::ValueRead("quz".to_string(), (0, 19)).boxed())
                })
                .boxed(),
                operator: "&&".to_string()
            }),
            parser.next_expr().ast,
            "(foo bar) && (baz quz)"
        );
    }
    #[test]
    fn individual_simple_expressions() {
        let mut parser = Parser::from_source("let foo : int32 = 5;");

        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                loc: (0, 4),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ValueType::Expr(Expr::NumericLiteral {
                    value: "5".to_string(),
                }),
                generictypes: None,
                abi:None
            }),
            parser.next_statement().ast
        );
        let mut parser = Parser::from_source(
            r#"let foo _ : int32 -> int32 =
    return 5;
"#,
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 4),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(0,18)
                }),
                args: vec![ArgDeclaration {
                    loc: (0, 8),
                    ident: "_".to_string(),
                    ty: None,
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "5".to_string(),
                    },
                    (1, 4)
                )]),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast
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
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed(),
                        loc:(1,20)
                    }
                    .boxed(),
                    returns: types::INT32.boxed(),
                    loc:(1,31)
                }),
                args: vec![ast::ArgDeclaration {
                    ident: "_".to_string(),
                    loc: (1, 8),
                    ty: None,
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_string(),
                    },
                    (2, 4)
                )]),
                generictypes: None,
                abi:None,
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
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed(),
                        loc:(1,29)
                    }
                    .boxed(),
                    loc:(1 ,18)
                }),
                args: vec![ast::ArgDeclaration {
                    ident: "_".to_string(),
                    loc: (1, 8),
                    ty: None,
                }],
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_owned(),
                    },
                    (2, 4)
                )]),
                generictypes:None,
                abi:None,
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
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 4),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ValueType::Expr(Expr::NumericLiteral {
                    value: "3".to_owned(),
                }),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "simple declaration"
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (2, 4),
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(2,20)
                }),
                args: vec![ast::ArgDeclaration {
                    ident: "quz".to_string(),
                    loc: (2, 8),
                    ty: None,
                }],
                value: ValueType::Function(vec![
                    Statement::Declaration(ValueDeclaration {
                        loc: (3, 8),
                        is_op: false,
                        ident: "baz".to_owned(),
                        ty: Some(types::STR),
                        args: Vec::new(),
                        value: ValueType::Expr(Expr::StringLiteral(r#"merp " yes"#.to_string())),
                        generictypes: None,
                        abi:None,
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "2".to_owned(),
                        },
                        (4, 4)
                    )
                ]),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "declaration with block"
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (6, 4),
                is_op: true,
                ident: "^^".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed(),
                        loc:(6,32)
                    }
                    .boxed(),
                    loc:(6,23)
                }),
                args: vec![
                    ast::ArgDeclaration {
                        ident: "lhs".to_string(),
                        loc: (6, 7),
                        ty: None,
                    },
                    ast::ArgDeclaration {
                        ident: "rhs".to_string(),
                        loc: (6, 11),
                        ty: None,
                    },
                ],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (7, 4),
                        value: Expr::ValueRead("bar".to_string(), (7, 4)).boxed(),
                        arg: Some(Expr::ValueRead("foo".to_string(), (7, 8)).boxed()),
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "1".to_owned(),
                        },
                        (8, 4)
                    )
                ]),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
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
                ident: "main".to_string(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(1,19)
                }),
                args: vec![ast::ArgDeclaration {
                    ident: "_".to_string(),
                    loc: (1, 9),
                    ty: None,
                }],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (2, 4),
                        value: Expr::ValueRead("put_int32".to_string(), (2, 4)).boxed(),
                        arg: Some(
                            Expr::NumericLiteral {
                                value: "100".to_owned(),
                            }
                            .boxed()
                        )
                    }),
                    Statement::FnCall(FnCall {
                        loc: (3, 4),
                        value: Expr::ValueRead("print_str".to_string(), (3, 4)).boxed(),
                        arg: Some(Expr::StringLiteral("v".to_string()).boxed())
                    }),
                    Statement::Return(
                        Expr::NumericLiteral {
                            value: "32".to_string(),
                        },
                        (4, 4)
                    )
                ]),
                generictypes: None,
                abi:None,
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
                .boxed(),
                rhs: Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 16),
                    lhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 10),
                        lhs: Expr::NumericLiteral {
                            value: "100".to_string(),
                        }
                        .boxed(),
                        rhs: Expr::ValueRead("foo".to_string(), (0, 12)).boxed(),
                        operator: "*".to_string()
                    })
                    .boxed(),
                    rhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (0, 23),
                        lhs: Expr::NumericLiteral {
                            value: "10".to_string(),
                        }
                        .boxed(),
                        rhs: Expr::NumericLiteral {
                            value: "1".to_string(),
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
                ident: "main".to_owned(),
                ty: None,
                args: vec![ast::ArgDeclaration {
                    ident: "_".to_string(),
                    loc: (0, 9),
                    ty: None,
                }],
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        loc: (1, 4),
                        value: Expr::ValueRead("print_int32".to_owned(), (1, 4)).boxed(),
                        arg: Some(
                            Expr::BinaryOpCall(BinaryOpCall {
                                loc: (1, 20),
                                operator: "+".to_owned(),
                                lhs: Expr::NumericLiteral {
                                    value: "100".to_owned(),
                                }
                                .boxed(),
                                rhs: Expr::NumericLiteral {
                                    value: "100".to_owned(),
                                }
                                .boxed()
                            })
                            .boxed()
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
                abi:None,
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
                        lhs: ast::Expr::ValueRead("a".to_string(), (0, 0)).boxed(),
                        rhs: ast::Expr::ValueRead("b".to_string(), (0, 2)).boxed(),
                        operator: ".".to_string()
                    })
                    .boxed(),
                    rhs: ast::Expr::NumericLiteral {
                        value: "2".to_string(),
                    }
                    .boxed(),
                    operator: "+".to_string()
                })
                .boxed(),
                rhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                    loc: (0, 11),
                    lhs: ast::Expr::ValueRead("c".to_string(), (0, 10)).boxed(),
                    rhs: ast::Expr::ValueRead("d".to_string(), (0, 12)).boxed(),
                    operator: ".".to_string()
                })
                .boxed(),
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
                    value: ast::Expr::ValueRead("foo".to_string(), (0, 1)).boxed(),
                    arg: Some(ast::Expr::ValueRead("bar".to_string(), (0, 5)).boxed())
                })
                .boxed(),
                rhs: ast::Expr::FnCall(FnCall {
                    loc: (0, 14),
                    value: ast::Expr::ValueRead("baz".to_string(), (0, 14)).boxed(),
                    arg: Some(ast::Expr::ValueRead("quz".to_string(), (0, 18)).boxed())
                })
                .boxed(),
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
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 11),
                is_op: false,
                ident: "test".to_string(),
                args: vec![ast::ArgDeclaration {
                    loc: (0, 16),
                    ident: "a".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (0,20)
                    }
                    .boxed(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (0,25)
                    }
                    .boxed(),
                    loc:(0,22)
                }),
                value: ast::ValueType::Expr(ast::Expr::ValueRead("a".to_string(), (0, 29))),
                generictypes: Some(ast::GenericsDecl {
                    for_loc:(0,0),
                    decls:[
                        (
                            (0,4),
                            "T".to_string()
                        )
                    ].into_iter().collect(),
                }),
                abi:None,
            }),
            parser.declaration().ast,
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
                generics: None,
                values: vec![ast::FieldDecl {
                    name: "a".to_string(),
                    ty: types::INT32,
                    loc: (1, 4),
                }],
                loc: (0, 5)
            },)),
            parser.declaration().ast,
            "basic"
        );
        assert_eq!(
            ast::Declaration::TypeDefinition(ast::TypeDefinition::Struct(StructDefinition {
                ident: "Tuple".to_string(),
                generics: Some(ast::GenericsDecl {
                    for_loc : (3,0),
                    decls:vec![((3,4),"T".to_string()), ((3,6),"U".to_string())],
                }),
                values: vec![
                    ast::FieldDecl {
                        name: "first".to_string(),
                        ty: ResolvedType::Generic {
                            name: "T".to_string(),
                            loc:(4,12),
                        },
                        loc: (4, 4)
                    },
                    ast::FieldDecl {
                        name: "second".to_string(),
                        ty: ResolvedType::Generic {
                            name: "U".to_string(),
                            loc:(5,13)
                        },
                        loc: (5, 4)
                    },
                ],
                loc: (3, 14)
            })),
            parser.declaration().ast,
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
                loc: (0, 4),
                is_op: false,
                ident: "foo".to_string(),
                args: vec![
                    ArgDeclaration {
                        loc: (0, 8),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ArgDeclaration {
                        loc: (0, 10),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: ResolvedType::User {
                        name: "Bar".to_string(),
                        generics: vec![types::INT32],
                        loc:(0,14)
                    }
                    .boxed(),
                    returns: ResolvedType::Function {
                        arg: ResolvedType::User {
                            name: "Baz".to_string(),
                            generics: vec![types::INT32, types::FLOAT64],
                            loc:(0,28)
                        }
                        .boxed(),
                        returns: types::INT32.boxed(),
                        loc:(0,47)
                    }
                    .boxed(),
                    loc:(0,25)
                }),
                value: ValueType::Function(vec![Statement::Return(
                    Expr::NumericLiteral {
                        value: "0".to_string(),
                    },
                    (1, 4)
                )]),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast
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
            Parser::from_source("Generic<int32>{ a:0 }")
                .next_expr()
                .ast
        )
    }

    #[test]
    fn control_flow_if() {
        let mut parser = Parser::from_source(include_str!("../../samples/control_flow_if.fb"));
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 4),
                is_op: false,
                ident: "inline_expr".to_string(),
                args: vec![ast::ArgDeclaration {
                    ident: "a".to_string(),
                    loc: (0, 16),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(0,25)
                }),
                value: ast::ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (0, 39)).boxed(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .boxed()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .boxed()
                    ),
                    loc: (0, 36)
                })),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "inline_expr"
        );

        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (2, 4),
                is_op: false,
                ident: "out_of_line_expr".to_string(),
                args: vec![ast::ArgDeclaration {
                    ident: "a".to_string(),
                    loc: (2, 21),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(2,30)
                }),
                value: ast::ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (2, 44)).boxed(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::FnCall(ast::FnCall {
                            loc: (3, 8),
                            value: ast::Expr::ValueRead("fun".to_string(), (3, 8)).boxed(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "0".to_string(),
                                }
                                .boxed()
                            ),
                        })
                        .boxed()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .boxed()
                    ),
                    loc: (2, 41)
                })),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "out_of_line_expr"
        );

        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (7, 4),
                is_op: false,
                ident: "expr_with_statement".to_string(),
                args: vec![ast::ArgDeclaration {
                    loc: (7, 24),
                    ident: "a".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(7,33)
                }),
                value: ast::ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (7, 47)).boxed(),
                    true_branch: (
                        vec![ast::Statement::FnCall(FnCall {
                            loc: (8, 8),
                            value: ast::Expr::ValueRead("bar".to_string(), (8, 8)).boxed(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                }
                                .boxed()
                            ),
                        })],
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .boxed()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        vec![ast::Statement::FnCall(FnCall {
                            loc: (11, 8),
                            value: ast::Expr::ValueRead("baz".to_string(), (11, 8)).boxed(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "4".to_string(),
                                }
                                .boxed()
                            ),
                        })],
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .boxed()
                    ),
                    loc: (7, 44)
                })),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "expr_with_statement"
        );

        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (14, 4),
                is_op: false,
                ident: "expr_with_else_if".to_string(),
                args: vec![
                    ast::ArgDeclaration {
                        loc: (14, 22),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ast::ArgDeclaration {
                        loc: (14, 24),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.boxed(),
                        returns: types::INT32.boxed(),
                        loc:(14,41)
                    }
                    .boxed(),
                    loc:(14,33)
                }),
                value: ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (14, 55)).boxed(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .boxed()
                    ),
                    else_ifs: vec![(
                        ast::Expr::ValueRead("b".to_string(), (14, 72)).boxed(),
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .boxed()
                    ),],
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "2".to_string(),
                        }
                        .boxed()
                    ),
                    loc: (14, 52)
                })),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "expr with else if"
        );

        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (16, 4),
                is_op: false,
                ident: "statement".to_string(),
                args: vec![ast::ArgDeclaration {
                    loc: (16, 14),
                    ident: "a".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(16,23)
                }),
                value: ast::ValueType::Function(vec![
                    ast::Statement::IfStatement(IfBranching {
                    cond: ast::Expr::ValueRead("a".to_string(), (17, 7)).boxed(),
                    true_branch: vec![
                        ast::Statement::FnCall(FnCall {
                            loc: (18, 8),
                            value: ast::Expr::ValueRead("foo".to_string(), (18, 8)).boxed(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                }
                                .boxed()
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
                            value: ast::Expr::ValueRead("bar".to_string(), (21, 8)).boxed(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "4".to_string(),
                                }
                                .boxed()
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
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "statement"
        );

        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (24, 4),
                is_op: false,
                ident: "statement_with_else_if".to_string(),
                args: vec![
                    ast::ArgDeclaration {
                        loc: (24, 27),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ast::ArgDeclaration {
                        loc: (24, 29),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.boxed(),
                        returns: types::INT32.boxed(),
                        loc:(24,46)
                    }
                    .boxed(),
                    loc:(24,38)
                }),
                value: ast::ValueType::Function(vec![ast::Statement::IfStatement(IfBranching {
                    cond: ast::Expr::ValueRead("a".to_string(), (25, 7)).boxed(),
                    true_branch: vec![ast::Statement::Return(
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        },
                        (26, 8)
                    )],
                    else_ifs: vec![(
                        ast::Expr::ValueRead("b".to_string(), (27, 12)).boxed(),
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
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "statement_with_else_if"
        );

        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (32, 4),
                is_op: false,
                ident: "expr_multi_with_elseif".to_string(),
                args: vec![
                    ast::ArgDeclaration {
                        loc: (32, 27),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ast::ArgDeclaration {
                        loc: (32, 29),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.boxed(),
                        returns: types::INT32.boxed(),
                        loc:(32,46)
                    }
                    .boxed(),
                    loc:(32,38)
                }),
                value: ValueType::Expr(ast::Expr::If(IfExpr {
                    cond: ast::Expr::ValueRead("a".to_string(), (32, 60)).boxed(),
                    true_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        }
                        .boxed(),
                    ),
                    else_ifs: vec![(
                        ast::Expr::ValueRead("b".to_string(), (34, 12)).boxed(),
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .boxed(),
                    )],
                    else_branch: (
                        Vec::new(),
                        ast::Expr::NumericLiteral {
                            value: "2".to_string(),
                        }
                        .boxed(),
                    ),
                    loc: (32, 57)
                })),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "multi line expr with else if"
        );
    }

    #[test]
    fn control_flow_match() {
        let mut parser = Parser::from_source(include_str!("../../samples/control_flow_match.fb"));
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (0, 4),
                is_op: false,
                ident: "match_expr_ints".to_string(),
                args: vec![ast::ArgDeclaration {
                    loc: (0, 20),
                    ident: "x".to_string(),
                    ty: None,
                }],
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(0, 30)
                }),
                value: ast::ValueType::Expr(ast::Expr::Match(Match {
                    loc: (0, 41),
                    on: ast::Expr::ValueRead("x".to_string(), (0, 47)).boxed(),
                    arms: vec![
                        MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::NumericLiteral {
                                    value: "1".to_string(),
                                }
                                .boxed()
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
                                .boxed()
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
                                .boxed()
                            ),
                            cond: Pattern::Default,
                            loc: (3, 6)
                        },
                    ]
                })),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "match_expr_ints"
        );

        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (5, 4),
                is_op: false,
                ident: "match_expr_with_block".to_string(),
                args: vec![ArgDeclaration {
                    loc: (5, 26),
                    ident: "x".to_string(),
                    ty: None,
                },],
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed(),
                    loc:(5, 36) 
                }),
                value: ValueType::Expr(ast::Expr::Match(Match {
                    loc: (5, 47),
                    on: ast::Expr::ValueRead("x".to_string(), (5, 53)).boxed(),
                    arms: vec![
                        MatchArm {
                            loc: (6, 6),
                            block: vec![ast::Statement::Declaration(ValueDeclaration {
                                loc: (7, 12),
                                is_op: false,
                                ident: "a".to_string(),
                                args: Vec::new(),
                                ty: Some(types::INT32),
                                value: ValueType::Expr(ast::Expr::NumericLiteral {
                                    value: "2".to_string(),
                                }),
                                generictypes: None,
                                abi:None,
                            })],
                            ret: Some(
                                ast::Expr::BinaryOpCall(BinaryOpCall {
                                    loc: (8, 9),
                                    lhs: ast::Expr::ValueRead("a".to_string(), (8, 8)).boxed(),
                                    rhs: ast::Expr::NumericLiteral {
                                        value: "3".to_string(),
                                    }
                                    .boxed(),
                                    operator: "*".to_string()
                                })
                                .boxed()
                            ),
                            cond: Pattern::ConstNumber("1".to_string()),
                        },
                        MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                ast::Expr::NumericLiteral {
                                    value: "2".to_string(),
                                }
                                .boxed()
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
                                    lhs: ast::Expr::ValueRead("a".to_string(), (10, 11)).boxed(),
                                    rhs: ast::Expr::NumericLiteral {
                                        value: "2".to_string(),
                                    }
                                    .boxed(),
                                    operator: "/".to_string()
                                })
                                .boxed()
                            ),
                            cond: Pattern::Read("a".to_string()),
                        },
                    ]
                })),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "match_expr_with_block"
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration {
                loc: (12, 4),
                is_op: false,
                ident: "match_statement".to_string(),
                args: vec![ArgDeclaration {
                    loc: (12, 20),
                    ident: "x".to_string(),
                    ty: None,
                },],
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::UNIT.boxed(),
                    loc:(12, 30)
                }),
                value: ValueType::Function(vec![ast::Statement::Match(Match {
                    loc: (13, 4),
                    on: ast::Expr::ValueRead("x".to_string(), (13, 10)).boxed(),
                    arms: vec![
                        MatchArm {
                            block: vec![ast::Statement::FnCall(FnCall {
                                loc: (15, 8),
                                value: ast::Expr::ValueRead("foo".to_string(), (15, 8)).boxed(),
                                arg: Some(
                                    ast::Expr::NumericLiteral {
                                        value: "0".to_string(),
                                    }
                                    .boxed()
                                )
                            })],
                            ret: None,
                            cond: Pattern::ConstNumber("1".to_string()),
                            loc: (14, 6),
                        },
                        MatchArm {
                            block: vec![ast::Statement::FnCall(FnCall {
                                loc: (17, 8),
                                value: ast::Expr::ValueRead("bar".to_string(), (17, 8)).boxed(),
                                arg: Some(
                                    ast::Expr::NumericLiteral {
                                        value: "1".to_string(),
                                    }
                                    .boxed()
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
                                    value: ast::Expr::ValueRead("baz".to_string(), (18, 11))
                                        .boxed(),
                                    arg: Some(
                                        ast::Expr::NumericLiteral {
                                            value: "2".to_string(),
                                        }
                                        .boxed()
                                    )
                                })
                                .boxed()
                            ),
                            cond: Pattern::ConstNumber("3".to_string()),
                            loc: (18, 6),
                        },
                    ]
                })]),
                generictypes: None,
                abi:None,
            }),
            parser.declaration().ast,
            "match_statement",
        );
    }
    #[test]
    fn arrays() {
        const SRC: &'static str = r#"
let arr = [0,0,0,0];
"#;

        let arr = Parser::from_source(SRC).declaration().ast;
        assert_eq!(
            ast::Declaration::Value(ast::ValueDeclaration {
                loc: (1, 4),
                is_op: false,
                ident: "arr".to_string(),
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
                    loc: (1, 11)
                }),
                generictypes: None,
                abi:None,
            }),
            arr,
            "arrays"
        )
    }

    #[test]
    fn abi() {
        const SRC : &'static str = r#"
extern "C" let putchar : int32 -> int32;
extern "C" let ex (a:int32) b = a + b;
"#;
        let mut parser = Parser::from_source(SRC);
        let putchar = parser.declaration().ast;
        let ex = parser.declaration().ast;
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration { 
                loc: (1,15),
                is_op: false,
                ident: "putchar".to_string(),
                args: Vec::new(),
                ty: Some(types::INT32.fn_ty(&types::INT32)),
                value: ValueType::External,
                generictypes: None,
                abi: Some(ast::Abi{
                    loc:(1,0),
                    identifier : "C".to_string(),
                }),
            }),
            putchar,
            "input function"
        );
        assert_eq!(
            ast::Declaration::Value(ValueDeclaration{
                loc:(2,15),
                is_op:false,
                ident : "ex".to_string(),
                args:vec![
                    ast::ArgDeclaration{
                        loc:(2,19),
                        ident:"a".to_string(),
                        ty:Some(types::INT32),
                    },
                    ast::ArgDeclaration{
                        loc:(2,28),
                        ident:"b".to_string(),
                        ty:None,
                    },
                ],
                ty:None,
                value:ValueType::Expr(Expr::BinaryOpCall(BinaryOpCall {
                    loc: (2,34), 
                    lhs: Expr::ValueRead("a".to_string(), (2,32)).boxed(), 
                    rhs: Expr::ValueRead("b".to_string(), (2,36)).boxed(),
                    operator: "+".to_string()
                })),
                generictypes:None,
                abi:Some(ast::Abi{
                    loc:(2,0),
                    identifier:"C".to_string(),
                }),
            }),
            ex,
            "output function."
        )
    }
}

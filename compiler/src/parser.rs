use std::{collections::HashSet, iter::Peekable, str::Chars};

use itertools::Itertools;

use crate::{
    ast::{self, BinaryOpCall, Expr, FnCall, Statement, ValueDeclaration, ValueType},
    lexer::TokenStream,
    tokens::Token,
    types::{self,ResolvedType},
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

pub type ParserResult = Result<crate::ast::Expr, ParseError>;

pub(crate) struct Parser<T: Clone>
where
    T: Iterator<Item = (Token, usize)>,
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
    T: Iterator<Item = (Token, usize)> + Clone,
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
            name: Some(name),
            declarations: decls,
        }
    }
    pub fn next_statement(&mut self) -> Result<Statement, ParseError> {
        let _x = self.stream.peek();
        match self.stream.clone().next() {
            Some((Token::NewLine, _)) => {
                self.stream.advance_by(1).unwrap();
                self.next_statement()
            }
            Some((Token::Let, _)) | Some((Token::For, _)) => {
                Ok(Statement::Declaration(self.fn_declaration()?))
            }
            Some((Token::Return, _)) => self.ret(),
            Some((Token::Ident(_), _)) => Ok(Statement::FnCall(self.function_call()?)),
            _ => unreachable!("how?"),
        }
    }

    pub fn next_expr(&mut self) -> ParserResult {
        let cloned_stream = self.stream.clone();
        let test = self.stream.peek();
        match self.stream.clone().next() {
            Some((Token::GroupOpen, _))
                if matches!(self.stream.clone().nth(1), Some((Token::GroupClose, _))) =>
            {
                self.stream.advance_by(2).unwrap();
                return Ok(Expr::UnitLiteral);
            }
            Some((Token::GroupOpen, _)) => {
                self.stream.advance_by(1).unwrap();
                let out = self.next_expr()?;
                if let Some((Token::GroupClose, _)) = self.stream.peek() {
                    self.stream.advance_by(1).unwrap()
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
                    Ok(Expr::FnCall(self.function_call()?))
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

    fn value(&mut self) -> Result<Expr, ParseError> {
        if let Some((Token::Ident(ident), _)) = self.stream.next() {
            Ok(ast::Expr::ValueRead(ident))
        } else {
            Err(ParseError {
                span: (100, 100),
                reason: ParseErrorReason::UnknownError,
            })
        }
    }

    fn function_call(&mut self) -> Result<FnCall, ParseError> {
        if let Some((Token::Ident(ident), span)) = self.stream.next() {
            let mut values = Vec::<ast::Expr>::new();
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
                        ,_)) = test {
                            values.push(self.literal()?)
                        } else if let Some((Token::Op(_),_)) = test {
                            values.push(self.binary_op()?)
                        } else {
                            values.push(self.literal()?)
                        }
                    },
                    _ => unreachable!()
                    // TODO! add in operator case special handling
                }
            }
            Ok(values.into_iter().fold(
                FnCall {
                    value: ast::Expr::ValueRead(ident).boxed(),
                    arg: None,
                },
                |inner, next| {
                    if let FnCall { value, arg: None } = inner {
                        FnCall {
                            value,
                            arg: Some(next.boxed()),
                        }
                    } else {
                        FnCall {
                            value: Expr::FnCall(inner).boxed(),
                            arg: Some(next.boxed()),
                        }
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
            Ok(ast::Statement::Return(self.next_expr()?))
        } else {
            Err(ParseError {
                span: (span, span),
                reason: ParseErrorReason::UnknownError,
            })
        }
    }

    fn literal(&mut self) -> ParserResult {
        let (token, span) = self.stream.next().unwrap();
        make_literal(token, span)
    }

    pub(crate) fn collect_type(&mut self) -> Result<ResolvedType, ParseError> {
        let ty = self.stream.next();
        if let Some((Token::Ident(ty), span)) = ty {
            if let Some((Token::Arrow, _)) = self.stream.peek() {
                self.stream.advance_by(1).unwrap();
                let result = self.collect_type()?;
                let ty = type_from_string(&ty);
                Ok(ResolvedType::Function { 
                    arg: ty.boxed(),
                    returns : result.boxed(),
                })
            } else {
                let ty =type_from_string(&ty);
                Ok(ty)
            }
        } else if let Some((Token::GroupOpen, span)) = ty {
            if let Some((Token::GroupClose, _)) = self.stream.peek() {
                self.stream.next();
                return if let Some((Token::Arrow, _)) = self.stream.peek() {
                    self.stream.next();
                    Ok(ResolvedType::Function {
                        arg:types::UNIT.boxed(),
                        returns: self.collect_type()?.boxed(),
                    })
                } else {
                    Ok(types::UNIT)
                };
            }
            let ty = self.collect_type()?;
            if let Some((Token::GroupClose, _)) = self.stream.next() {
                if let Some((Token::Arrow, _)) = self.stream.peek() {
                    self.stream.advance_by(1).unwrap();
                    let result = self.collect_type()?;
                    Ok(ResolvedType::Function { arg:ty.boxed(), returns: result.boxed() })
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

    fn pipe(&mut self) -> Result<Expr, ParseError> {
        todo!("pipes")
    }

    fn compose(&mut self) -> Result<Expr, ParseError> {
        todo!("compose")
    }

    fn declaration(&mut self) -> Result<ast::Declaration, ParseError> {
        match self.stream.peek() {
            Some((Token::For | Token::Let, _)) => {
                Ok(ast::Declaration::Value(self.fn_declaration()?))
            },
            Some((Token::NewLine,_)) => {
                self.stream.next();
                self.declaration()
            }
            _ => unimplemented!(),
        }
    }

    fn fn_declaration(&mut self) -> Result<ValueDeclaration, ParseError> {
        let generics = if let Some((Token::For, _)) = self.stream.peek() {
            let Some((_,span)) = self.stream.next() else { unreachable!() };
            if let Some((Token::Op(op),_)) = self.stream.next() && op == "<" {
                let mut out = self.stream.clone().take_while(|(token,_)| &Token::Op(">".to_string())!= token).collect_vec();
                let first_span = out.first().unwrap().1;
                let last_span = out.last().unwrap().1;
                let num = out.len();
                out.dedup();
                if out.len() == num {
                    out.into_iter().filter_map(|(t,_)| {
                        let Token::Ident(name) = t else { return None };
                        Some(name)
                    }).collect()
                } else {
                    return Err(ParseError { span: (first_span,last_span), reason: ParseErrorReason::DeclarationError })
                }
            } else {
                let (_,end) = self.stream.clone().take_while(|(token,span)| token != &Token::Let).last().unwrap();
                return Err(ParseError { span: (span,end), reason: ParseErrorReason::DeclarationError })
            }
        } else {
            HashSet::new()
        };
        if generics.len() != 0 {
            self.stream.advance_by(generics.len() + 1).unwrap();
        }
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
                            self.stream.peeking_take_while(|(t,_)| t == &Token::NewLine).collect_vec();
                            let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                ValueType::Function(self.collect_block()?)
                            } else if let Some((Token::Compose,_)) = self.stream.clone().nth(1) {
                                ValueType::Expr(self.compose()?)
                            } else if let Some((Token::Op(op),_)) = self.stream.clone().nth(1) && is_pipe_op(&op){
                                ValueType::Expr(self.pipe()?)
                            } else {
                                ValueType::Expr(self.next_expr()?)
                            };
                            Ok(ValueDeclaration { is_op: false, ident, ty: Some(ty), args: Vec::new(), value, generictypes : HashSet::new()})
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
                            let ty = generics.iter().fold(ty,|ty,name| ty.replace_user_with_generic(&name));
                            let next = self.stream.next();
                            if let Some((Token::Op(eq),eq_span)) = next
                            && eq == "="
                            {
                                self.stream.peeking_take_while(|(t,_)| t == &Token::NewLine).collect_vec();
                                let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                ValueType::Function(self.collect_block()?)
                            } else if let Some((Token::Compose,_)) = self.stream.clone().nth(1) {
                                ValueType::Expr(self.compose()?)
                            } else if let Some((Token::Op(op),_)) = self.stream.clone().nth(1) && is_pipe_op(&op){
                                ValueType::Expr(self.pipe()?)
                            } else {
                                ValueType::Expr(self.next_expr()?)
                            };
                                Ok(ValueDeclaration {
                                    is_op: false,
                                    ident,
                                    ty: Some(ty),
                                    args,
                                    value,
                                    generictypes:generics
                                })
                            } else {
                                Err(ParseError{
                                    span:(start,ident_span+ident.len()),
                                    reason : ParseErrorReason::DeclarationError,
                                })
                            }
                        } else {
                            if let Some((Token::Op(eq),eq_span)) = self.stream.next()
                            && eq == "=" { 
                                self.stream.peeking_take_while(|(t,_)| t == &Token::NewLine).collect_vec();
                                let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                ValueType::Function(self.collect_block()?)
                            } else if let Some((Token::Compose,_)) = self.stream.clone().nth(1) {
                                ValueType::Expr(self.compose()?)
                            } else if let Some((Token::Op(op),_)) = self.stream.clone().nth(1) && is_pipe_op(&op){
                                ValueType::Expr(self.pipe()?)
                            } else {
                                ValueType::Expr(self.next_expr()?)
                            };
                                Ok(ValueDeclaration {
                                    is_op: false,
                                    ident,
                                    ty: None,
                                    args,
                                    value: value,
                                    generictypes: generics
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
                            self.stream.peeking_take_while(|(t,_)| t == &Token::NewLine).collect_vec();
                            let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                ValueType::Function(self.collect_block()?)
                            } else if let Some((Token::Compose,_)) = self.stream.clone().nth(1) {
                                ValueType::Expr(self.compose()?)
                            } else if let Some((Token::Op(op),_)) = self.stream.clone().nth(1) && is_pipe_op(&op){
                                ValueType::Expr(self.pipe()?)
                            } else {
                                ValueType::Expr(self.next_expr()?)
                            };
                            Ok(ValueDeclaration {
                                is_op: false,
                                ident,
                                ty: Some(ty), 
                                args: Vec::new(),
                                value: value,
                                generictypes:generics})
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
                            let ty = generics.iter().fold(ty,|ty,name| {
                                ty.replace_user_with_generic(&name)
                            });
                            let next = self.stream.next();
                            if let Some((Token::Op(eq),eq_span)) = next
                            && eq == "="
                            {
                                
                            self.stream.peeking_take_while(|(t,_)| t == &Token::NewLine).collect_vec();
                                let value = if let Some((Token::BeginBlock,_)) = self.stream.peek() {
                                    ValueType::Function(self.collect_block()?)
                                } else if let Some((Token::Compose,_)) = self.stream.clone().nth(1) {
                                    ValueType::Expr(self.compose()?)
                                } else if let Some((Token::Op(op),_)) = self.stream.clone().nth(1) && is_pipe_op(&op){
                                    ValueType::Expr(self.pipe()?)
                                } else {
                                    ValueType::Expr(self.next_expr()?)
                                };
                                Ok(ValueDeclaration {
                                    is_op: true,
                                    ident,
                                    ty: Some(ty),
                                    args,
                                    value,
                                    generictypes:generics
                                })
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
                                    ValueType::Function(self.collect_block()?)
                                } else {
                                    // TODO check for math expresions 
                                    ValueType::Expr(self.literal()?)
                                };
                                Ok(ValueDeclaration {
                                    is_op: true,
                                    ident,
                                    ty: None,
                                    args,
                                    value: value,
                                    generictypes:generics,
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

    fn binary_op(&mut self) -> Result<ast::Expr, ParseError> {
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
        let _ = self.stream.advance_by(tokens.len());
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
                    token_iter.advance_by(sub_tokens.len()).unwrap();
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
                        token_iter.advance_by(sub_tokens.len());
                        let result = Parser::from_stream(sub_tokens.into_iter()).next_expr()?;
                        output.push(ShuntingYardOptions::Expr(result))
                    } else {
                        let Some((Token::Ident(ident),_)) = token_iter.next() else {unreachable!()};
                        output.push(ShuntingYardOptions::Expr(Expr::ValueRead(ident)));
                    }
                },
                Token::Integer(_, _) | Token::FloatingPoint(_, _) | Token::CharLiteral(_) | Token::StringLiteral(_) => output.push(ShuntingYardOptions::Expr({
                    let Some((token,span)) = token_iter.next() else { return Err(ParseError { span: (0,0), reason: ParseErrorReason::UnknownError })};
                    make_literal(token, span)?
                })),
                Token::Op(_) => {
                    let Some(Token::Op(ident)) = token_iter.next().map(|(a,_)| a) else { unreachable!() };
                    let (prec,left) = PRECIDENCE.iter().find_map(|(op,weight,assc)| if op == &ident { Some((*weight,*assc))} else { None }).unwrap_or((1,false));
                    if op_stack.is_empty() {
                        op_stack.push(ident);
                        continue;
                    }
                    while let Some(op_back) = op_stack.last() {
                        let back_prec = PRECIDENCE.iter().find_map(|(op,weight,_)| if op == &op_back { Some(*weight)} else { None }).unwrap_or(1);
                        if back_prec > prec || (back_prec == prec && left) {
                            let Some(op_back) = op_stack.pop() else { unreachable!() };
                            output.push(ShuntingYardOptions::Op(op_back));
                        } else {
                            op_stack.push(ident);
                            break;
                        }
                    }
                }
                _ => break
            }
        }
        output.extend(op_stack.into_iter().rev().map(ShuntingYardOptions::Op));
        let mut final_expr = Vec::<ast::Expr>::new();
        for expr in output {
            match expr {
                ShuntingYardOptions::Expr(expr) => final_expr.push(expr),
                ShuntingYardOptions::Op(op) => {
                    let Some(rhs) = final_expr.pop() else { unreachable!() };
                    let Some(lhs) = final_expr.pop() else { unreachable!() };
                    final_expr.push(ast::Expr::BinaryOpCall(BinaryOpCall {
                        operator: op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    }))
                }
            }
        }

        if final_expr.len() != 1 {
            Err(ParseError {
                span: (0, 0),
                reason: ParseErrorReason::UnknownError,
            })
        } else {
            Ok(final_expr.into_iter().next().unwrap())
        }
    }
}

fn make_literal(token: Token, span: usize) -> ParserResult {
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
                                span: (span, span + src.len()),
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
            span: (span, 0),
            reason: ParseErrorReason::UnknownError,
        }),
    }
}
fn type_from_string(name : &str) -> ResolvedType {
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
                _ => ResolvedType::User { name: ty.to_string(), generics:Vec::new()}
            }
        },
        ty if ty.starts_with("float") => {
            let size = ty.strip_prefix("float");
            match size {
                Some("32") => types::FLOAT32,
                Some("64") => types::FLOAT64,
                _ => ResolvedType::User { name: ty.to_string(), generics:Vec::new() }
            }
        },
        _ => ResolvedType::User { name: name.to_string(), generics:Vec::new() }
    }
}
fn is_pipe_op(op: &str) -> bool {
    op.ends_with('>') && op[..op.len() - 2].chars().all(|c| c == '|')
}

enum ShuntingYardOptions {
    Expr(ast::Expr),
    Op(String),
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use inkwell::context::Context;

    use crate::{types::{ResolvedType, TypeResolver}, ast::Declaration};

    use super::*;
    #[test]
    #[ignore = "This is for singled out tests"]
    fn for_debugging_only() {
        const SRC: &'static str = "let a : int32 -> int32 = foo b";
        let expr = Parser::from_source(SRC).next_expr().unwrap();
        let ctx = Context::create();
        let type_resolver = TypeResolver::new(&ctx);
        let ty = Parser::from_source("int32 -> int32 -> int32")
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
            r#"let foo : int32 =
    return 5
"#,
        );
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ValueType::Function(vec![Statement::Return(Expr::NumericLiteral {
                    value: "5".to_string(),
                    ty: types::INT32
                })]),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().expect("failed to parse")
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
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg:  ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed(),
                    returns: types::INT32.boxed()
                }),
                args: vec!["_".to_string()],
                value: ValueType::Function(vec![Statement::Return(Expr::NumericLiteral {
                    value: "0".to_string(),
                    ty: types::INT32
                })]),
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
                args: Vec::from_iter(vec!["_".to_string()].into_iter()),
                value: ValueType::Function(vec![Statement::Return(Expr::NumericLiteral {
                    value: "0".to_owned(),
                    ty: types::INT32,
                })]),
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
            Statement::Declaration(ValueDeclaration {
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
            parser.next_statement().unwrap(),
            "simple declaration"
        );
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }),
                args: Vec::from_iter(vec!["quz".to_string()].into_iter()),
                value: ValueType::Function(vec![
                    Statement::Declaration(ValueDeclaration {
                        is_op: false,
                        ident: "baz".to_owned(),
                        ty: Some(types::STR),
                        args: Vec::new(),
                        value: ValueType::Expr(Expr::StringLiteral(r#"merp " yes"#.to_string())),
                        generictypes: HashSet::new(),
                    }),
                    Statement::Return(Expr::NumericLiteral {
                        value: "2".to_owned(),
                        ty: types::INT32
                    })
                ]),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().unwrap(),
            "declaration with block"
        );
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                is_op: true,
                ident: "^^".to_owned(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns:ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed()
                }),
                args: Vec::from_iter(vec!["lhs".to_string(), "rhs".to_string()].into_iter()),
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        value: Expr::ValueRead("bar".to_string()).boxed(),
                        arg: Some(Expr::ValueRead("foo".to_string()).boxed()),
                    }),
                    Statement::Return(Expr::NumericLiteral {
                        value: "1".to_owned(),
                        ty: types::INT32
                    })
                ]),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().unwrap(),
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
                is_op: false,
                ident: "main".to_string(),
                ty: Some(ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                }),
                args: Vec::from_iter(vec!["_".to_string()].into_iter()),
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
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
                        value: Expr::ValueRead("print_str".to_string()).boxed(),
                        arg: Some(Expr::StringLiteral("v".to_string()).boxed())
                    }),
                    Statement::Return(Expr::NumericLiteral {
                        value: "32".to_string(),
                        ty: types::INT32,
                    })
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
                operator: "+".to_string(),
                lhs: Expr::NumericLiteral {
                    value: "100".to_string(),
                    ty: types::INT32
                }
                .boxed(),
                rhs: Expr::BinaryOpCall(BinaryOpCall {
                    operator: "*".to_string(),
                    lhs: Expr::NumericLiteral {
                        value: "100".to_string(),
                        ty: types::INT32
                    }
                    .boxed(),
                    rhs: Expr::BinaryOpCall(BinaryOpCall {
                        operator: "*".to_string(),
                        lhs: Expr::ValueRead("foo".to_string()).boxed(),
                        rhs: Expr::BinaryOpCall(BinaryOpCall {
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
            parser.next_expr().unwrap()
        );
        const SRC: &'static str = r#"let main _ =
    print_int32 100 + 100
    return 0"#;
        let mut parser = Parser::from_source(SRC);
        assert_eq!(
            Statement::Declaration(ValueDeclaration {
                is_op: false,
                ident: "main".to_owned(),
                ty: None,
                args: Vec::from_iter(vec!["_".to_string()].into_iter()),
                value: ValueType::Function(vec![
                    Statement::FnCall(FnCall {
                        value: Expr::ValueRead("print_int32".to_owned()).boxed(),
                        arg: Some(
                            Expr::BinaryOpCall(BinaryOpCall {
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
                    Statement::Return(Expr::NumericLiteral {
                        value: "0".to_owned(),
                        ty: types::INT32
                    })
                ]),
                generictypes: HashSet::new(),
            }),
            parser.next_statement().unwrap()
        )
    }

    #[test]
    fn huh() {
        const CODE : &'static str = include_str!("../../cli/test.fb");
        let mut parser = Parser::from_source(CODE);
        let decl0 = parser.next_statement().unwrap();
        assert_eq!(
            decl0,
            Statement::Declaration(ValueDeclaration { 
                is_op: false,
                ident: "test".to_string(), 
                args: vec!["a".to_string()], 
                ty: Some(ResolvedType::Function { 
                    arg: ResolvedType::Generic { name: "T".to_string() }.boxed(), 
                    returns:types::INT32.boxed(), 
                }), 
                value: ValueType::Function(vec![
                    Statement::Return(Expr::NumericLiteral { value: "3".to_string(), ty: types::INT32 })
                ]), 
                generictypes: ["T".to_string()].into_iter().collect() })
        );
        let decl1 = parser.next_statement().unwrap();
        assert_eq!(
            decl1,
            Statement::Declaration(ValueDeclaration { 
                is_op: false,
                ident: "test2".to_string(), 
                args: vec!["a".to_string(),"b".to_string()], 
                ty: Some(ResolvedType::Function { 
                    arg: ResolvedType::Generic { name: "T".to_string() }.boxed(), 
                    returns:ResolvedType::Function { 
                        arg: types::INT32.boxed(), 
                        returns:types::INT32.boxed(), 
                    }.boxed(), 
                }), 
                value: ValueType::Function(vec![
                    Statement::Return(Expr::ValueRead("b".to_string()))
                ]), 
                generictypes: ["T".to_string()].into_iter().collect() })
        );
    }
}

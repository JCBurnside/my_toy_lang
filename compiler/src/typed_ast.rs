use std::{
    collections::{HashMap, HashSet},
    iter::once,
    num::NonZeroU8, hash::Hash,
};

use inkwell::{context::Context, data_layout};
use itertools::Itertools;

use crate::{
    ast::{self, Declaration},
    types::{self, resolve_from_name, ResolvedType, TypeResolver, IntWidth, FloatWidth},
    util::ExtraUtilFunctions,
};

pub struct TypedModuleDeclaration {
    pub(crate) name: Option<String>,
    pub(crate) declarations: Vec<TypedDeclaration>,
}

impl TypedModuleDeclaration {
    fn from(module : ast::ModuleDeclaration, fwd_declares : &HashMap<String,ResolvedType>) -> Self {
        let ast::ModuleDeclaration{ name, declarations} = module;
        Self { name, declarations: declarations.into_iter().map(|decl| TypedDeclaration::try_from(decl, fwd_declares)).filter_map(|decl| match decl {
            Ok(decl) => Some(decl),
            Err(e) => {
                println!("{:?}", e);
                None
            }
        }).collect() }
    } 
}

pub enum TypedDeclaration {
    Mod(TypedModuleDeclaration),
    Value(TypedValueDeclaration),
    ///TODO
    TypeDefinition()
}

impl TypedDeclaration {
    fn try_from(data : ast::Declaration, known_values : &HashMap<String,ResolvedType>) -> Result<Self,TypingError> {
        match data {
            Declaration::Mod(module) => Ok(Self::Mod(TypedModuleDeclaration::from(module, known_values))),
            Declaration::Value(decl) => Ok(Self::Value(TypedValueDeclaration::try_from(decl, known_values)?)),
            Declaration::TypeDefinition(_) => todo!(),
        }
    }
}

pub struct TypedValueDeclaration {
    pub(crate) is_op : bool,
    pub(crate) ident: String,
    pub(crate) args: HashSet<String>,
    pub(crate) value: TypedValueType,
    pub(crate) ty : ResolvedType,
    pub(crate) generictypes : HashSet<String>
} 
impl TypedValueDeclaration {
    fn try_from(data : ast::ValueDeclaration, known_values : &HashMap<String,ResolvedType>) -> Result<Self,TypingError> {
        let ast::ValueDeclaration{ is_op, ident, args, ty, value, generictypes } = data;
        let Some(ty) = ty else { todo!("type inference") };
        Ok(Self { is_op, ident, args, value:TypedValueType::try_from(value, known_values)?, ty, generictypes})
    }
}

pub enum TypedValueType {
    Expr(TypedExpr),
    Function(Vec<TypedStatement>),
}

impl TypedValueType {
    fn try_from(data : ast::ValueType, known_values : &HashMap<String, ResolvedType>) -> Result<Self,TypingError> {
        match data {
            ast::ValueType::Expr(expr) => TypedExpr::try_from(expr, known_values).map(|expr| Self::Expr(expr)),
            ast::ValueType::Function(stmnts) => {
                let mut output = Vec::with_capacity(stmnts.len());
                let mut known_values = known_values.clone();
                for stmnt in stmnts {
                    match TypedStatement::try_from(stmnt, &known_values) {
                        Ok(stmnt) => {
                            if let TypedStatement::Declaration(data) = &stmnt {
                                known_values.entry(data.ident.clone()).insert_entry(data.ty.clone());
                            }
                            output.push(stmnt);
                        },
                        Err(e) => {
                            println!("{:?}", e);
                            output.push(TypedStatement::Error);
                        }
                    }
                }
                if !output.iter().filter_map(|stmnt| {
                    match stmnt {
                        TypedStatement::Return(value) => {
                            let rt = value.get_ty();
                            if rt != ResolvedType::Error {
                                Some(rt)
                            } else {
                                None
                            }
                        }
                        _ => None
                    }
                }).all_equal() {
                    println!("Not all returns match in type");
                    Err(TypingError::ReturnTypeMismatch)
                } else {
                    Ok(TypedValueType::Function(output))
                }
            }
        }
    }
}

pub enum TypedStatement {
    Declaration(TypedValueDeclaration),
    Return(TypedExpr),
    FnCall(TypedFnCall),
    Pipe(TypedPipe),
    Error
}

impl TypedStatement {
    fn try_from(statement : ast::Statement, known_values : &HashMap<String,ResolvedType>) -> Result<Self, TypingError> {
        match statement {
            ast::Statement::Declaration(data) => Ok(match TypedValueDeclaration::try_from(data, known_values){
                Ok(d) => Self::Declaration(d),
                Err(e) => {
                    println!("{:?}", e);
                    Self::Error
                }
            }),
            ast::Statement::Return(value) => Ok(Self::Return(TypedExpr::try_from(value, known_values)?)),
            ast::Statement::FnCall(data) => Ok(Self::FnCall(TypedFnCall::try_from(data,known_values)?)),
            ast::Statement::Pipe(_) => todo!(),
        }
    }
}

pub struct TypedPipe {
    ///if you need to spread more than an u8's worth of values... wtf are you doing? what would even return that many values as a tuple?  
    pub(crate) expansion: NonZeroU8,
    pub(crate) lhs: Box<TypedExpr>,
    /// THIS HAS A RESTRICTION OF MUST RETURN A FUNCTION
    pub(crate) rhs: Box<TypedExpr>,
    /// should match the final return type as [`rhs`]
    pub(crate) rt: ResolvedType,
}

#[derive(PartialEq)]
pub struct TypedFnCall {
    pub(crate) value: Box<TypedExpr>,
    pub(crate) arg: Option<Box<TypedExpr>>,
    pub(crate) rt: ResolvedType,
    pub(crate) arg_t: ResolvedType,
}

impl TypedFnCall {
    fn try_from(data : ast::FnCall, known_values : &HashMap<String,ResolvedType>) -> Result<Self, TypingError>{
        let ast::FnCall { value, arg } = data;
        let value = match TypedExpr::try_from(*value, known_values) {
            Ok(node) => node,
            Err(e) => {
                println!("{:?}",e);
                TypedExpr::ErrorNode
            }
        };

        let arg = arg.map(|arg| {
            match TypedExpr::try_from(*arg, known_values) {
                Ok(arg) => arg,
                Err(e) => {
                    println!("{:?}", e);
                    TypedExpr::ErrorNode
                }
            }
        });

        if value != TypedExpr::ErrorNode {
            let arg_t = arg.as_ref().map(|arg| arg.get_ty());
            let ResolvedType::Function { arg, .. } = strip_pointers(&value.get_ty()) else { return Err(TypingError::FnNotDeclared) };
            if Some(*arg) != arg_t {
                return Err(TypingError::ArgTypeMismatch)
            }
        }
        let rt = if value == TypedExpr::ErrorNode {
            ResolvedType::Error
        } else {
            let ResolvedType::Function { returns, .. } = strip_pointers(&value.get_ty()) else { unreachable!() };
            returns.as_ref().clone()
        };
        Ok(Self {
            value : value.boxed(),
            arg_t : arg.as_ref().map(|arg| arg.get_ty()).unwrap_or(types::UNIT),    
            arg : arg.map(Box::new),
            rt 
        })
    }
}

#[derive(PartialEq)]
pub enum TypedExpr {
    /// Integers
    IntegerLiteral {
        value: String,
        size: types::IntWidth,
    },
    /// Floats
    FloatLiteral {
        value: String,
        size: types::FloatWidth,
    },
    /// Strings
    StringLiteral(String),
    /// chars
    CharLiteral(String),
    /// ()
    UnitLiteral,
    /// `a + b`
    BinaryOpCall(TypedBinaryOpCall),
    /// `!a`
    UnaryOpCall(TypedUnaryOpCall),
    /// `foo bar`
    FnCall(TypedFnCall),
    /// basically an ident on it's own
    /// (ident,type)
    ValueRead(String, ResolvedType),
    /// NOT IMPLEMENTED YET
    /// defined like [ expr, expr, expr, ... ]
    /// type [T;N]
    ArrayLiteral { contents: Vec<TypedExpr> },
    /// NOT IMPLEMENTED YET
    /// defined like [| expr, expr, expr, ... |]
    /// type [|T|]
    ListLiteral { contents: Vec<TypedExpr> },
    /// NOT IMPLEMENTED YET
    /// defined like (expr, expr,...)
    /// typed as (T, U, V, ...)
    /// not recommended above 3 values
    TupleLiteral { contents: Vec<TypedExpr> },

    // This is used to allow to continue type checking.  should never naturally generate.
    ErrorNode
}

impl TypedExpr {
    
    fn try_from(value: ast::Expr, known_values : &HashMap<String,ResolvedType>) -> Result<Self, TypingError> {
        use ast::Expr;
        match value {
            Expr::NumericLiteral { value, ty } if let ResolvedType::Int { width, ..} = ty => Ok(Self::IntegerLiteral { value, size: width }),
            Expr::NumericLiteral { value, ty } if let ResolvedType::Float { width } = ty => Ok(Self::FloatLiteral { value, size: width }),
            Expr::NumericLiteral { .. } => unreachable!(),
            Expr::StringLiteral(value) => Ok(Self::StringLiteral(value)),
            Expr::CharLiteral(value) => Ok(Self::CharLiteral(value)),
            Expr::UnitLiteral => Ok(Self::UnitLiteral),
            Expr::Compose { lhs, rhs } => todo!(),
            Expr::BinaryOpCall(data) => Ok(Self::BinaryOpCall(TypedBinaryOpCall::try_from(data, known_values)?)),
            Expr::UnaryOpCall(_) => todo!(),
            Expr::FnCall(data) => Ok(Self::FnCall(TypedFnCall::try_from(data,known_values)?)),
            Expr::ValueRead(value) => {
                if !known_values.contains_key(&value) {
                    Err(TypingError::UnknownType)
                } else {
                    let ty = known_values[&value].clone();
                    Ok(Self::ValueRead(value, ty))
                }
            },
            Expr::ArrayLiteral { contents } => {
                let contents = contents.into_iter().map(|value| TypedExpr::try_from(value, known_values)).map(|value| match value {
                    Ok(value) => value,
                    Err(e) => {
                        println!("{:?}", e);
                        TypedExpr::ErrorNode
                    }
                }).collect_vec();

                if !contents.iter().filter_map(|value| if value == &TypedExpr::ErrorNode { None } else { Some(value.get_ty()) }).all_equal() {
                    Err(TypingError::ArgTypeMismatch)
                } else {
                    Ok(Self::ArrayLiteral { contents })
                }
            },
            Expr::ListLiteral { contents } => todo!(),
            Expr::TupleLiteral { contents } => todo!(),
        }
    }

    fn get_ty(&self) -> ResolvedType {
        match self {
            TypedExpr::IntegerLiteral {  size, .. } => ResolvedType::Int { signed: true, width: size.clone() },
            TypedExpr::FloatLiteral { size, .. } => ResolvedType::Float { width: size.clone() },
            TypedExpr::StringLiteral(_) => types::STR,
            TypedExpr::CharLiteral(_) => types::CHAR,
            TypedExpr::UnitLiteral => types::UNIT,
            TypedExpr::BinaryOpCall(data) => data.rt.clone(),
            TypedExpr::UnaryOpCall(data) => data.rt.clone(),
            TypedExpr::FnCall(data) => data.rt.clone(),
            TypedExpr::ValueRead(_, ty) => ty.clone(),
            TypedExpr::ArrayLiteral { contents } => ResolvedType::Array { underlying: contents.first().unwrap().get_ty().boxed(), size: contents.len() },
            TypedExpr::ListLiteral { contents } => todo!(),
            TypedExpr::TupleLiteral { contents } => todo!(),
            TypedExpr::ErrorNode => types::ERROR,
        }
    }
}

#[derive(PartialEq)]
pub struct TypedBinaryOpCall {
    pub(crate) lhs: Box<TypedExpr>,
    pub(crate) rhs: Box<TypedExpr>,
    pub(crate) operator: String,
    pub(crate) rt: ResolvedType,
}

impl TypedBinaryOpCall {
    fn try_from(value: ast::BinaryOpCall, known_values : &HashMap<String,ResolvedType>) -> Result<Self, TypingError> {
        let ast::BinaryOpCall { lhs, rhs, operator } = value;
        let lhs  = match TypedExpr::try_from(*lhs, known_values) {
            Ok(lhs) => lhs,
            Err(e) => {
                println!("{:?}", e);
                TypedExpr::ErrorNode
            }
        };
        let rhs = match TypedExpr::try_from(*rhs, known_values) {
            Ok(rhs) => rhs,
            Err(e) => {
                println!("{:?}", e);
                TypedExpr::ErrorNode
            }
        };
        match operator.as_str() {
            "*" | "+" | "/" | "-" => {
                // TODO: need to add support for overloading this.
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if (lhs_t.is_float() || lhs_t.is_int()) &&
                   (rhs_t.is_float() || rhs_t.is_int()) {
                    Ok(Self {
                        lhs : lhs.boxed(),
                        rhs : rhs.boxed(),
                        operator,
                        rt : match(lhs_t,rhs_t) {
                            (lhs,rhs) if lhs.is_float() && rhs.is_float() => {
                                let ResolvedType::Float { width : lhs_w } = lhs else { unreachable!() };
                                let ResolvedType::Float { width : rhs_w } = rhs else { unreachable!() };
                                ResolvedType::Float { width: lhs_w.max(rhs_w) }
                            },
                            (lhs,_) if lhs.is_float() => lhs,
                            (_,rhs) if rhs.is_float() => rhs,
                            (lhs,rhs) => {
                                let ResolvedType::Int { signed : lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                                let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = lhs else { unreachable!() };
                                if lhs_signed && !rhs_signed {
                                    lhs
                                } else if rhs_signed && !lhs_signed {
                                    rhs
                                } else {
                                    ResolvedType::Int { signed: lhs_signed, width: lhs_w.max(rhs_w) }
                                }
                            }
                        }
                    })
                } else {
                    Err(TypingError::OpNotSupported)
                }
            }
            "**" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if (lhs_t.is_float() || lhs_t.is_int()) && 
                   (rhs_t.is_float() || rhs_t.is_int()) {
                    Ok(Self {
                        lhs : lhs.boxed(),
                        rhs : rhs.boxed(),
                        operator,
                        rt : match (lhs_t, rhs_t) {
                            (lhs,rhs) if lhs.is_float() && rhs.is_float() => {
                                let ResolvedType::Float { width : lhs } = lhs else { unreachable!() };
                                let ResolvedType::Float { width : rhs } = rhs else { unreachable!() };
                                ResolvedType::Float { width: lhs.max(rhs) }
                            }
                            (lhs,_) if lhs.is_float() => lhs,
                            (_,rhs) if rhs.is_float() => rhs,
                            (lhs,rhs) => {
                                let ResolvedType::Int { signed : lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                                let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = lhs else { unreachable!() };
                                let max = lhs_w.max(rhs_w);
                                ResolvedType::Float { width: match max {
                                    IntWidth::SixtyFour => FloatWidth::SixtyFour,
                                    _ => FloatWidth::ThirtyTwo
                                }}
                            }
                        }
                    })
                } else {
                    Err(TypingError::OpNotSupported)
                }
            }
            _ => Err(TypingError::OpNotSupported)
        }
    }
}

#[derive(PartialEq)]
pub struct TypedUnaryOpCall {
    pub(crate) operand: Box<TypedExpr>,
    pub(crate) operator: String,
    pub(crate) rt: ResolvedType,
}

fn strip_pointers(ty : &ResolvedType) -> ResolvedType {
    if let ResolvedType::Function { arg, returns } = ty {
        let arg = if let ResolvedType::Pointer { underlining  } = arg.as_ref() && underlining.is_function() {
            strip_pointers(underlining.as_ref()).boxed()
        } else { arg.clone() };
        let returns = if let ResolvedType::Pointer { underlining } = returns.as_ref() && underlining.is_function() {
            strip_pointers(underlining.as_ref()).boxed()
        } else { returns.clone() };
        ResolvedType::Function { arg, returns }
    } else {
        ty.clone()
    }
}

#[derive(Debug)]
#[allow(unused)]
pub enum TypingError {
    ReturnTypeMismatch,
    FnNotDeclared,
    BlockTypeMismatch,
    OpNotSupported, //temp.
    UnknownType,
    DoubleTyped,
    ArgTypeMismatch,
}

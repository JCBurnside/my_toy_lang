use std::{collections::HashMap, iter::once, rc::Rc};

use inkwell::{
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
};
use itertools::Itertools;

use crate::{util::ExtraUtilFunctions, types::{self, ResolvedType, TypeResolver, resolve_from_name}};

#[derive(PartialEq, Debug)]
#[allow(unused)]
pub enum Expr {
    BinaryOpCall {
        ident: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnaryOpCall {
        ident: String,
        operand: Box<Expr>,
    },
    FnCall {
        value: Box<Expr>,//ideally this should be of FnCall | Value.  checking needed
        arg: Option<Box<Expr>>,
    },
    Return {
        expr: Box<Expr>,
    },
    Block {
        sub: Vec<Expr>,
    },
    Literal {
        value: String,
        ty: TypeName,
    },
    ValueRead {
        ident : String,
    },
    Declaration {
        is_op: bool,
        ident: String,
        ty: Option<TypeName>,
        args: Option<Vec<(String, Option<TypeName>)>>,
        value: Box<Expr>,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypeName {
    FnType(Rc<TypeName>, Rc<TypeName>),
    ValueType(String),
}

pub enum TypedExpr {
    BinaryOpCall {
        ident: String,
        rt: ResolvedType,
        lhs: Box<TypedExpr>,
        rhs: Box<TypedExpr>,
    },
    UnaryOpCall {
        ident: String,
        rt: ResolvedType,
        operand: Box<TypedExpr>,
    },
    FnCall {
        value : Box<TypedExpr>,
        rt: ResolvedType,
        arg: Option<Box<TypedExpr>>,
    },
    Return {
        rt: ResolvedType,
        expr: Box<TypedExpr>,
    },
    Literal {
        rt: ResolvedType,
        value: String,
    },
    ValueRead {
        rt: ResolvedType,
        ident : String
    },
    Block {
        rt: ResolvedType,
        sub: Vec<TypedExpr>,
    },
    Declaration {
        is_op: bool,
        ident: String,
        ty: ResolvedType,
        args: Vec<String>,
        value: Box<TypedExpr>,
    },
}

#[derive(Debug)]
pub enum TypingError {
    ReturnTypeMismatch,
    FnNotDelcared,
    BlockTypeMismatch,
    OpNotSupported, //temp.
    UnknownType,
    DoubleTyped,
    ArgCountMismatch,
}

impl<'ctx> TypedExpr {
    pub fn try_from(
        ctx: &'ctx inkwell::context::Context,
        type_resolver: &TypeResolver<'ctx>,
        values : HashMap<String,ResolvedType>,
        expr: Expr,
    ) -> Result<Self, TypingError> {
        match expr {
            Expr::BinaryOpCall { ident, lhs, rhs } => match &ident[..] {
                "+" | "-" | "/" | "*" => {
                    let lhs = Self::try_from(ctx, type_resolver, values.clone(), *lhs)?;
                    let rhs = Self::try_from(ctx, type_resolver, values, *rhs)?;
                    if rhs.get_rt() == lhs.get_rt()
                    {
                        Ok(Self::BinaryOpCall {
                            ident: ident,
                            rt: rhs.get_rt(),
                            lhs: lhs.boxed(),
                            rhs: rhs.boxed(),
                        })
                    } else {
                        Err(TypingError::OpNotSupported)
                    }
                }
                "**" => {
                    let lhs = Self::try_from(ctx, type_resolver,  values.clone(), *lhs)?;
                    let rhs = Self::try_from(ctx, type_resolver,  values, *rhs)?;
                    let lhs_ty = lhs.get_rt();
                    if 
                        lhs_ty.is_float()
                        && lhs_ty == rhs.get_rt()
                    {
                        Ok(Self::BinaryOpCall {
                            ident,
                            rt: lhs_ty,
                            lhs: lhs.boxed(),
                            rhs: rhs.boxed(),
                        })
                    } else {
                        Err(TypingError::OpNotSupported)
                    }
                }
                _ => Err(TypingError::OpNotSupported),
            },
            Expr::UnaryOpCall { ident, operand } => {
                if let Some(type_name) = values.get(&ident) {
                    if let ResolvedType::Function { returns,.. } = type_name {
                        Ok(Self::UnaryOpCall { ident, rt: returns.as_ref().clone(), operand: Self::try_from(ctx, type_resolver, values, *operand)?.boxed() })
                    } else {
                        Err(TypingError::UnknownType)
                    }
                 } else {
                    Err(TypingError::FnNotDelcared)
                }
            }
            Expr::FnCall { value: fun, arg } => {
                todo!()
                // if let Some(type_name) = functions.get(&ident) {
                //     if let ResolvedType::Function { returns, .. } = type_name {
                //         Ok(Self::FnCall { 
                //             fun : Self::try_from(ctx, type_resolver, functions, fun)?.boxed(),
                //             rt: returns.as_ref().clone(), 
                //             arg : arg.map(|arg| Self::try_from(ctx, type_resolver, functions, *arg).unwrap().boxed())
                //         })
                //     } else {
                //         unreachable!()
                //     }
                // } else {
                //     Err(TypingError::FnNotDelcared)
                // }
            }
            Expr::Return { expr } => {
                let expr = Self::try_from(ctx, type_resolver, values, *expr)?;
                Ok(Self::Return {
                    rt: expr.get_rt(),
                    expr: Box::new(expr),
                })
            }
            Expr::Block { sub } => {
                let mut functions = values.clone();
                let sub: Vec<TypedExpr> = sub
                    .into_iter()
                    .map(|expr| {
                        let fns = functions.clone();
                        let r = Self::try_from(ctx, type_resolver, fns, expr);
                        match &r {
                            Ok(TypedExpr::Declaration {is_op:false,ident, ty,..}) => {
                                functions.insert(ident.clone(), ty.clone());
                                r
                            }
                            _ => r 
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let last = sub.last().unwrap().get_rt();
                let valid = sub
                    .iter()
                    .filter_map(|expr| match expr {
                        TypedExpr::Return { rt, .. } => Some(rt.clone()),
                        _ => None,
                    })
                    .chain(once(last.clone()))
                    .all_equal();
                if valid {
                    Ok(Self::Block { rt: last, sub })
                } else {
                    Err(TypingError::ReturnTypeMismatch)
                }
            }
            Expr::Literal { value, ty } => match ty {
                TypeName::ValueType(_) => {
                    Ok(Self::Literal {
                        rt: resolve_from_name(ty),
                        value,
                    })
                }
                _ => unreachable!(),
            },
            Expr::Declaration {
                is_op,
                ident,
                ty,
                args,
                value,
            } => {
                match (&ty, args) {
                    (Some(_), Some(args)) if args.iter().any(|(_, it)| it.is_some()) => {
                        Err(TypingError::DoubleTyped)
                    }
                    (Some(TypeName::ValueType(_)), Some(_)) => Err(TypingError::ArgCountMismatch), //should be no args.  will need to change when fn keyword is introduced.
                    (None, Some(args)) if args.iter().any(|(_, it)| it.is_none()) => {
                        Err(TypingError::UnknownType)
                    }
                    (Some(TypeName::FnType(_, _)), Some(args)) => {
                        let args = args.into_iter().map(|(it, _)| it).collect_vec();
                        Ok(TypedExpr::Declaration {
                            is_op,
                            ident,
                            ty: types::resolve_from_name(ty.unwrap()),
                            args,
                            value: Self::try_from(ctx, type_resolver, values, *value)?.boxed(),
                        })
                    }
                    (Some(TypeName::ValueType(ty)), None) => {
                        todo!()
                    }
                    (None, Some(args)) => {
                        let arg_types = todo!();
                    }
                    _ => todo!(),
                }
            }
            Expr::ValueRead { ident } => todo!(),
        }
    }

    fn get_rt(
        &self,
    ) -> ResolvedType {
        match self {
            TypedExpr::BinaryOpCall { rt, .. }
            | TypedExpr::UnaryOpCall { rt, .. }
            | TypedExpr::FnCall { rt, .. }
            | TypedExpr::Return { rt, .. }
            | TypedExpr::Literal { rt, .. }
            | TypedExpr::ValueRead { rt, .. }
            | TypedExpr::Block { rt, .. } => rt.clone(),
            TypedExpr::Declaration { .. } => ResolvedType::Unit,
        }
    }
}
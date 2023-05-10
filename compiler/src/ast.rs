use std::{collections::{HashMap, HashSet}, iter::once, rc::Rc};

use inkwell::{
    context::Context,
};
use itertools::Itertools;

use crate::{
    util::ExtraUtilFunctions, 
    types::{self, ResolvedType, TypeResolver, resolve_from_name}
};

#[derive(PartialEq, Debug, Clone)]
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
        generictypes: Vec<String>
    },
    UnitLiteral,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypeName {
    FnType(Rc<TypeName>, Rc<TypeName>),
    ValueType(String),
}

#[derive(Debug,Clone)]
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
        curried: bool,
        generic : bool,
    },
    /// this is some expr that requires a generic type be realized before it can be realzied
    /// eg `a + b` in `for<T> let example a b : T -> int32 -> int32 = a + b`
    GenericExpr {
        waiting_on : Vec<String>,
        sub : Expr,
    },
    UnitLiteral,
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

impl<'ctx> TypedExpr {
    pub fn try_from(
        ctx: &'ctx Context,
        mut type_resolver: TypeResolver<'ctx>,
        mut values : HashMap<String,ResolvedType>,
        known_generics : &HashSet<String>,
        expr: Expr,
    ) -> Result<(Self,Vec<Self>), TypingError> {
        let mut completed_generics = Vec::new();
        Ok((match expr {
            Expr::BinaryOpCall { ident, lhs, rhs } => match &ident[..] {
                "+" | "-" | "/" | "*" => {
                    let lhs = *lhs;
                    let (new_lhs, new_generics) = Self::try_from(ctx, type_resolver.clone(), values.clone(), known_generics, lhs.clone())?;
                    completed_generics.extend(new_generics.into_iter());
                    let rhs = *rhs;
                    let (new_rhs, new_generics) = Self::try_from(ctx, type_resolver.clone(), values, known_generics, rhs.clone())?;
                    completed_generics.extend(new_generics.into_iter());
                    if new_lhs.get_rt().is_generic() || new_rhs.get_rt().is_generic() {
                        Self::GenericExpr { waiting_on: [
                                if let ResolvedType::Generic { name } = new_lhs.get_rt() { Some(name) } else { None },
                                if let ResolvedType::Generic { name } = new_rhs.get_rt() { Some(name) } else { None },
                            ].into_iter().filter_map(|it| it).collect_vec(), 
                            sub : Expr::BinaryOpCall { ident, lhs: lhs.boxed(), rhs : rhs.boxed() }
                        }
                    }
                    else if new_rhs.get_rt() == new_lhs.get_rt()
                    {
                        Self::BinaryOpCall {
                            ident,
                            rt: new_rhs.get_rt(),
                            lhs: new_lhs.boxed(),
                            rhs: new_rhs.boxed(),
                        }
                    } else {
                        return Err(TypingError::OpNotSupported)
                    }
                }
                "**" => {
                    let (lhs, new_generics) = Self::try_from(ctx, type_resolver.clone(), values.clone(), known_generics, *lhs)?;
                    completed_generics.extend(new_generics.into_iter());
                    let (rhs, new_generics) = Self::try_from(ctx, type_resolver.clone(), values, known_generics, *rhs)?;
                    completed_generics.extend(new_generics.into_iter());
                    let lhs_ty = lhs.get_rt();
                    if 
                        lhs_ty.is_float()
                        && lhs_ty == rhs.get_rt()
                    {
                        Self::BinaryOpCall {
                            ident,
                            rt: lhs_ty,
                            lhs: lhs.boxed(),
                            rhs: rhs.boxed(),
                        }
                    } else {
                        return Err(TypingError::OpNotSupported)
                    }
                }
                _ => return Err(TypingError::OpNotSupported),
            },
            Expr::UnaryOpCall { ident, operand } => {
                if let Some(type_name) = values.get(&ident) {
                    if let ResolvedType::Function { returns,.. } = type_name {
                        let (operand,new_generics) = Self::try_from(ctx, type_resolver.clone(), values.clone(), known_generics, *operand)?;
                        completed_generics.extend(new_generics.into_iter());
                        Self::UnaryOpCall { ident, rt: returns.as_ref().clone(), operand: operand.boxed()}
                    } else {
                        return Err(TypingError::UnknownType)
                    }
                 } else {
                    return Err(TypingError::FnNotDeclared)
                }
            }
            Expr::FnCall { value, arg } => {
                let (value,new_generics) = TypedExpr::try_from(ctx, type_resolver.clone(), values.clone(), known_generics, *value)?;
                completed_generics.extend(new_generics.into_iter());
                let ResolvedType::Function { arg : arg_t_expected, returns } = value.get_rt() else { return Err(TypingError::ArgTypeMismatch) };
                let arg = arg.map(|arg| TypedExpr::try_from(ctx, type_resolver.clone(), values.clone(), known_generics, *arg));
                if arg.is_some() {
                    if arg.as_ref().map_or(false, Result::is_err) {
                        return arg.unwrap();
                    }
                }
                let arg = arg.map(|arg| arg.unwrap());
                let arg_t = arg.as_ref().map(|arg| arg.0.get_rt());
                if (arg_t_expected.as_ref() == &ResolvedType::Unit && arg_t.is_none())
                    || arg_t_expected.as_ref() == arg_t.as_ref().unwrap() {
                        if let Some((arg,new_generics)) = arg {
                            completed_generics.extend(new_generics.into_iter());
                            TypedExpr::FnCall { value : value.boxed(), rt: *returns, arg : Some(arg.boxed()) }
                        } else {
                            TypedExpr::FnCall { value : value.boxed(), rt: *returns, arg : None }
                        }
                } else {
                    return Err(TypingError::ArgTypeMismatch)
                }
            }
            Expr::Return { expr } => {
                let (expr, new_generic) = Self::try_from(ctx, type_resolver.clone(), values, known_generics, *expr)?;
                completed_generics.extend(new_generic.into_iter());
                Self::Return {
                    rt: expr.get_rt(),
                    expr: Box::new(expr),
                }
            }
            Expr::Block { sub } => {
                let mut functions = values.clone();
                let sub: Vec<TypedExpr> = sub
                    .into_iter()
                    .map(|expr| {
                        let fns = functions.clone();
                        let r = Self::try_from(ctx, type_resolver.clone(), fns, known_generics, expr);
                        match r {
                            Ok((r,generics)) => {
                                completed_generics.extend(generics.into_iter());
                                Ok(match &r {
                                    TypedExpr::Declaration {is_op:false,ident, ty,..} => {
                                        functions.insert(ident.clone(), ty.clone());
                                        r
                                    }
                                    _ => r 
                                })
                            }
                            Err(err) => Err(err)
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
                    Self::Block { rt: last, sub }
                } else {
                    return Err(TypingError::ReturnTypeMismatch)
                }
            }
            Expr::Literal { value, ty } => match ty {
                TypeName::ValueType(_) => {
                    Self::Literal {
                        rt: resolve_from_name(ty,known_generics),
                        value,
                    }
                }
                _ => unreachable!(),
            },
            Expr::Declaration {
                is_op,
                ident,
                ty,
                args,
                value,
                generictypes,
            } => {
                let known_generics = if generictypes.len() != 0 {
                    let mut out = known_generics.clone();
                    out.extend(generictypes.into_iter());
                    out
                } else { known_generics.clone() };
                match (&ty, args) {
                    (Some(_), Some(args)) if args.iter().any(|(_, it)| it.is_some()) => {
                        return Err(TypingError::DoubleTyped)
                    }
                    (Some(TypeName::ValueType(_)), Some(_)) => return Err(TypingError::ArgTypeMismatch), //should be no args.  will need to change when fn keyword is introduced.
                    (None, Some(args)) if args.iter().any(|(_, it)| it.is_none()) => {

                        // this is could be generic. or mixed infered and not.
                        return Err(TypingError::UnknownType)
                    }
                    (Some(TypeName::FnType(_, _)), Some(args)) => {
                        let args = args.into_iter().map(|(it, _)| it).collect_vec();
                        let mut curr = ty.clone().unwrap();
                        for arg in args.iter() {
                            let TypeName::FnType(arg_t,next) = curr else { unreachable!() };
                            values.insert(arg.clone(), types::resolve_from_name(arg_t.as_ref().clone(), &known_generics));
                            curr = next.as_ref().clone();
                        }
                        let (value, generics) = Self::try_from(ctx, type_resolver, values, &known_generics, *value)?;
                        completed_generics.extend(generics.into_iter());
                        TypedExpr::Declaration {
                            is_op,
                            ident,
                            ty: types::resolve_from_name(ty.unwrap(), &known_generics),
                            args,
                            value: value.boxed(),
                            curried:if let TypeName::FnType(_, _) = curr { true } else { false },
                            generic:false,
                        }
                    }
                    (Some(TypeName::FnType(_,_)),None) => { //this is a curried or composed function
                        let ty = types::resolve_from_name(ty.unwrap(),&known_generics);
                        let (value,new_generics) = Self::try_from(ctx, type_resolver, values, &known_generics, *value)?;
                        completed_generics.extend(new_generics.into_iter());
                        TypedExpr::Declaration {
                            is_op,
                            ident,
                            generic:ty.is_generic(),
                            ty,
                            args: vec![],
                            value:value.boxed(),
                            curried:true,
                        }
                    }
                    (Some(TypeName::ValueType(_ty)), None) => {
                        todo!()
                    }
                    _ => todo!("this type inference is not supported yet."),
                }
            }
            Expr::ValueRead { ident } => Self::ValueRead { 
                rt: values.get(&ident).unwrap().clone(),
                ident
            },
            Expr::UnitLiteral => Self::UnitLiteral,
        },completed_generics))
    }

    pub fn get_rt(
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
            TypedExpr::GenericExpr { .. } => unimplemented!("idk how to handle this case yet."),
            TypedExpr::Declaration { .. }
            | TypedExpr::UnitLiteral => ResolvedType::Unit,
        }
    }
}
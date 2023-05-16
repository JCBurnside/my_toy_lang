use std::{collections::{HashMap, HashSet}, iter::once};

use inkwell::context::Context;
use itertools::Itertools;

use crate::{ast::{Expr, TypeName}, types::{resolve_from_name, ResolvedType, TypeResolver, self}, util::ExtraUtilFunctions};

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
        types_of_all_chain : Vec<ResolvedType>,
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
        type_resolver: TypeResolver<'ctx>,
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

                    if new_rhs.get_rt() == new_lhs.get_rt()
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
                    || arg_t_expected.as_ref() == arg_t.as_ref().unwrap()
                    || arg_t_expected.as_ref().is_generic() {
                        if let Some((arg,new_generics)) = arg {
                            completed_generics.extend(new_generics.into_iter());
                            let filled_types = if let TypedExpr::FnCall { types_of_all_chain, .. } = &value {
                                let mut types = types_of_all_chain.clone();
                                types.extend_one(arg.get_rt());
                                types
                            } else {
                                vec![arg.get_rt()]
                            };
                            TypedExpr::FnCall { value : value.boxed(), rt: *returns, arg : Some(arg.boxed()), types_of_all_chain : filled_types }
                        } else {
                            TypedExpr::FnCall { value : value.boxed(), rt: *returns, arg : None, types_of_all_chain : vec![] }
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
                            generic:known_generics.len() > 0,
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
            Expr::ValueRead { ident } => {
                Self::ValueRead { 
                    //need to handle checking for generic functions and maybe resolving them
                    rt: values.get(&ident).unwrap().clone(),
                    ident
                }
            } 
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
            TypedExpr::Declaration { .. }
            | TypedExpr::UnitLiteral => ResolvedType::Unit,
        }
    }

    fn replace_generic(&self, name:&str, new_ty : ResolvedType) -> Self {
        match self {
            Self::Literal { .. } => self.clone(),
            Self::Declaration { generic:false, .. } => self.clone(), // in theory this shouldn't be hit but just to be safe.
            Self::Declaration { is_op, ident, ty, args, value, curried, generic } => {
                let ident = ident.clone() + "_" + &new_ty.to_string();
                let value = value.replace_generic(name, new_ty.clone());
                let ty = ty.clone().replace_generic(name, new_ty);
                Self::Declaration { 
                    is_op : *is_op, 
                    ident, 
                    generic: ty.is_generic(),
                    ty, 
                    args: args.clone(), 
                    value : value.boxed(), 
                    curried : *curried, 
                }
            }
            Self::Block { rt, sub } => {
                Self::Block { 
                    rt: rt.clone().replace_generic(name, new_ty.clone()), 
                    sub: sub.iter().cloned().map(|expr| expr.replace_generic(name, new_ty.clone())).collect_vec() 
                }
            }
            Self::BinaryOpCall { ident, rt, lhs, rhs } => {
                Self::BinaryOpCall { 
                    ident: ident.clone(), 
                    rt:rt.clone().replace_generic(name, new_ty.clone()), 
                    lhs: lhs.replace_generic(name, new_ty.clone()).boxed(), 
                    rhs: rhs.replace_generic(name, new_ty).boxed() 
                }
            }

            Self::FnCall { value, rt, arg, types_of_all_chain } => {
                Self::FnCall { 
                    value: value.replace_generic(name, new_ty.clone()).boxed(), 
                    rt: rt.clone().replace_generic(name, new_ty.clone()), 
                    types_of_all_chain: types_of_all_chain.iter().cloned().map(|ty| ty.replace_generic(name, new_ty.clone())).collect_vec(),
                    arg: arg.clone().map(|expr| expr.replace_generic(name, new_ty).boxed()),
                }
            }

            Self::Return { rt, expr } => {
                Self::Return { rt: rt.clone().replace_generic(name, new_ty.clone()), expr: expr.replace_generic(name, new_ty).boxed() }
            }

            Self::UnaryOpCall { ident, rt, operand } => {
                Self::UnaryOpCall { ident: ident.clone(), rt: rt.clone().replace_generic(name, new_ty.clone()), operand: operand.replace_generic(name, new_ty).boxed() }
            }

            Self::UnitLiteral => Self::UnitLiteral,
            Self::ValueRead { rt, ident } => self.clone()

        }
    }
}

// pub(crate) fn lower_generics(expr : TypedExpr, values : HashMap<String,TypedExpr>) -> (TypedExpr,Vec<TypedExpr>) {
//     lower_generics_impl(expr, values, HashMap::new())
// }

// fn lower_generics_impl(mut expr:TypedExpr, values : HashMap<String,TypedExpr>, locals:HashMap<String,&TypedExpr>) -> (TypedExpr,Vec<TypedExpr>) {
//     match expr {
//     }
// }
#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::types;
    use crate::types::ResolvedType;
    use crate::util::ExtraUtilFunctions;
    use super::TypedExpr;
    #[test]
    fn two_arg_one_generic() {
        let generic = TypedExpr::Declaration { 
            is_op: false, 
            ident: "test".to_string(),
            ty: types::ResolvedType::Function { 
                arg: ResolvedType::Generic { name: "T".to_string() }.boxed(), 
                returns: ResolvedType::Function { 
                    arg: types::INT32.boxed(), 
                    returns: types::INT32.boxed() 
                }.boxed() 
            },
            args: vec!["a".to_string(),"b".to_string()], 
            value: TypedExpr::BinaryOpCall { 
                ident: "+".to_string(), 
                rt: types::INT32, 
                lhs: TypedExpr::ValueRead { rt: ResolvedType::Generic { name: "T".to_string() }, ident: "a".to_string() }.boxed(), 
                rhs: TypedExpr::ValueRead { rt: types::INT32, ident: "b".to_string() }.boxed(),
            }.boxed(), 
            curried: false, 
            generic: true 
        };

        let usage = TypedExpr::Declaration { 
            is_op: false, 
            ident: "harness".to_string(), 
            ty: ResolvedType::Function { arg: types::UNIT.boxed(), returns: types::UNIT.boxed() }, 
            args: Vec::new(), 
            value: TypedExpr::Block { 
                rt: types::UNIT, 
                sub: vec![
                    TypedExpr::FnCall { 
                        value: TypedExpr::FnCall { 
                            value: TypedExpr::ValueRead { 
                                rt: types::ResolvedType::Function { 
                                    arg: ResolvedType::Generic { name: "T".to_string() }.boxed(), 
                                    returns: ResolvedType::Function { 
                                        arg: types::INT32.boxed(), 
                                        returns: types::INT32.boxed() 
                                    }.boxed() 
                                }, 
                                ident: "test".to_string()  
                            }.boxed(), 
                            rt: ResolvedType::Function { arg: types::INT32.boxed(), returns: types::INT32.boxed() }, 
                            arg: Some(TypedExpr::Literal { rt: types::INT32, value: "2".to_string() }.boxed()), 
                            types_of_all_chain: vec![types::INT32] 
                        }.boxed(), 
                        rt: types::INT32,
                        arg: Some(TypedExpr::Literal { rt: types::INT32, value: "3".to_string() }.boxed()), 
                        types_of_all_chain: vec![
                            types::INT32,
                            types::INT32
                        ] 
                    }
                ] 
            }.boxed(), 
            curried: false, 
            generic: false 
        };
        
    }
}
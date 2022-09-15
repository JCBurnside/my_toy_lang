
use std::{collections::HashMap, rc::Rc, iter::once};

use inkwell::{
    context::Context,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, BasicType, StructType, FunctionType},
    AddressSpace,
};
use itertools::Itertools;

use crate::util::ExtraUtilFunctions;

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
        ident: String,
        args: Vec<Expr>,
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
    Declaration {
        is_op: bool,
        ident: String,
        ty: Option<TypeName>,
        args: Option<Vec<(String, Option<TypeName>)>>,
        value: Box<Expr>,
    },
}

#[derive(PartialEq, Debug)]
pub enum TypeName {
    FnType(Box<TypeName>, Box<TypeName>),
    ValueType(String),
}

// impl TypeName {
//     fn collapse_function(&self) -> (Vec<TypeName>,TypeName) { // (args, rt)
//         match self {

//         }
//     }
// }

pub enum TypedExpr<'ctx> {
    BinaryOpCall {
        ident: String,
        rt : Rc<dyn BasicType<'ctx> + 'ctx>,
        lhs : Box<TypedExpr<'ctx>>,
        rhs : Box<TypedExpr<'ctx>>,
    },
    UnaryOpCall {
        ident: String,
        rt : Rc<dyn BasicType<'ctx> + 'ctx>,
        operand : Box<TypedExpr<'ctx>>,
    },
    FnCall {
        ident: String,
        rt : Rc<dyn BasicType<'ctx> + 'ctx>,
        args: Vec<TypedExpr<'ctx>>,
    },
    Return {
        rt : Rc<dyn BasicType<'ctx> + 'ctx>,
        expr : Box<TypedExpr<'ctx>>
    },
    Literal {
        rt : Rc<dyn BasicType<'ctx> + 'ctx>,
        value : String,
    },
    Block {
        rt : Rc<dyn BasicType<'ctx> + 'ctx>,
        sub : Vec<TypedExpr<'ctx>>
    },
    Declaration {
        is_op : bool,
        ident : String,
        ty : Box<dyn BasicType<'ctx>>,
        args : Vec<(String,Box<dyn BasicType<'ctx>>)>,
        value : Box<TypedExpr<'ctx>>
    },
}

pub enum TypingError {
    ReturnTypeMismatch,
    FnNotDelcared,
    BlockTypeMismatch,
    UnknownType,
    DoubleTyped,
    ArgCountMismatch,
}

impl <'ctx> TypedExpr<'ctx> {
    pub fn try_from(ctx:&'ctx inkwell::context::Context, types : &'ctx HashMap<String,BasicTypeEnum<'ctx>>, declarations : &'ctx HashMap<String,Box<dyn BasicType<'ctx>>>, unit:&Rc<StructType<'ctx>>, expr:Expr) -> Result<Self,TypingError> {
        match expr {
            Expr::BinaryOpCall { ident, lhs, rhs } => {
                if let Some(type_name) = declarations.get(&ident) {
                    match type_name.as_any_type_enum() {
                        AnyTypeEnum::FunctionType(ty)=> {
                            Ok(Self::BinaryOpCall { ident, rt: Rc::new(ty.get_return_type().unwrap()), lhs: Box::new(Self::try_from(ctx, types, declarations, unit, *lhs)?), rhs: Box::new(Self::try_from(ctx, types, declarations, unit, *rhs)?) })
                        }
                        _=>unreachable!()
                    }
                } else {
                    Err(TypingError::FnNotDelcared)
                }
            },
            Expr::UnaryOpCall { ident, operand } => {
                if let Some(type_name) = declarations.get(&ident) {
                    match type_name.as_any_type_enum() {
                        AnyTypeEnum::FunctionType(ty)=> {
                            Ok(Self::UnaryOpCall { ident, rt: Rc::new(ty.get_return_type().unwrap()), operand:Box::new(Self::try_from(ctx, types, declarations, unit,  *operand)?) })
                        }
                        _=>unreachable!()
                    }
                } else {
                    Err(TypingError::FnNotDelcared)
                }
            },
            Expr::FnCall { ident, args } => {
                if let Some(type_name) = declarations.get(&ident) {
                    match type_name.as_any_type_enum() {
                        AnyTypeEnum::FunctionType(ty) =>{ 
                            Ok(Self::FnCall { ident, rt: Rc::new(ty.get_return_type().unwrap()), args: args.into_iter().map(|arg| Self::try_from(ctx, types, declarations, unit, arg)).collect::<Result<Vec<_>,_>>()? })
                        }
                        _ => unreachable!()
                    }
                } else {
                    Err(TypingError::FnNotDelcared)
                }
            },
            Expr::Return { expr } => {
                let expr = Self::try_from(ctx, types, declarations, unit, *expr)?;
                Ok(Self::Return { rt: expr.get_rt(&unit), expr: Box::new(expr) })
            },
            Expr::Block { sub } => {
                let sub : Vec<TypedExpr> = sub.into_iter().map(|expr| Self::try_from(ctx, types, declarations, unit, expr)).collect::<Result<Vec<_>,_>>()?;
                
                let last = sub.last().unwrap().get_rt(unit);
                let valid = sub.iter().filter_map(|expr| match expr {
                    TypedExpr::Return { rt, .. } => Some(rt.clone()),
                    _=> None,
                }).chain(once(last.clone())).map(|ty| ty.as_any_type_enum()).all_equal();
                if valid { 
                    Ok(Self::Block {
                        rt : last,
                        sub,
                    })
                } else {
                    Err(TypingError::BlockTypeMismatch)
                }
            },
            Expr::Literal { value, ty } => {
                match ty {
                    TypeName::ValueType(ty) => {
                        if let Some(ty) = types.get(&ty) {
                            Ok(Self::Literal { rt: Rc::new(ty.clone()), value })
                        } else {
                            Err(TypingError::UnknownType)
                        }
                    },
                    _=> unreachable!()
                }
            },
            Expr::Declaration { is_op, ident, ty, args, value } => {
                match (ty,args) {
                    (Some(_),Some(args)) if args.iter().any(|(_,it)|it.is_some()) => { Err(TypingError::DoubleTyped)},
                    (Some(TypeName::ValueType(_)),Some(_)) => { Err(TypingError::ArgCountMismatch)} //should be no args.  will need to change when fn keyword is introduced.
                    (None, Some(args)) if args.iter().any(|(_,it)| it.is_none()) => { Err(TypingError::UnknownType) }
                    (Some(TypeName::FnType(_, _,)),Some(args)) => {
                        
                        let args = args.into_iter().map(|(it,_)|it).collect_vec();
                        todo!()
                    },
                    (Some(TypeName::ValueType(ty)),None) => {
                        todo!()
                    },
                    (None,Some(args))=> {
                        let arg_types = todo!();
                    }
                    _=> todo!()
                }
            },
        }
    }

    fn get_rt(&self, unit:&Rc<StructType<'ctx>>) -> Rc<dyn BasicType<'ctx> + 'ctx> {
        match self {
            TypedExpr::BinaryOpCall { rt, .. } |
            TypedExpr::UnaryOpCall {  rt, .. } |
            TypedExpr::FnCall {  rt, .. } |
            TypedExpr::Return { rt, .. } |
            TypedExpr::Literal { rt, .. } |
            TypedExpr::Block { rt, .. } => rt.clone(),
            TypedExpr::Declaration { .. } => unit.clone(),
        }
    }
}

#[derive(Debug)]
pub enum ResolvedType<'ctx> {
    Basic(Rc<dyn BasicType<'ctx> + 'ctx>),
    Fn(FunctionType<'ctx>)
}
impl <'ctx> PartialEq for ResolvedType<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Basic(l0), Self::Basic(r0)) => l0.as_ref().as_basic_type_enum() == r0.as_ref().as_basic_type_enum(),
            (Self::Fn(l0), Self::Fn(r0)) => l0 == r0,
            _ => false
        }
    }
}
pub fn resolve_type<'ctx>(ty:TypeName, known_types : &HashMap<String,BasicTypeEnum<'ctx>>, ctx : &'ctx Context) -> Option<ResolvedType<'ctx>> {
    match ty {
        TypeName::ValueType(name) => known_types.get(&name).map(|ty| match ty {
            BasicTypeEnum::ArrayType(ty) => Rc::new(*ty) as Rc<dyn BasicType<'ctx> + 'ctx>,
            BasicTypeEnum::FloatType(ty) => Rc::new(*ty) as Rc<dyn BasicType<'ctx> + 'ctx>,
            BasicTypeEnum::IntType(ty) => Rc::new(*ty) as Rc<dyn BasicType<'ctx> + 'ctx>,
            BasicTypeEnum::PointerType(ty) => Rc::new(*ty) as Rc<dyn BasicType<'ctx> + 'ctx>,
            BasicTypeEnum::StructType(ty) => Rc::new(*ty) as Rc<dyn BasicType<'ctx> + 'ctx>,
            BasicTypeEnum::VectorType(ty) => Rc::new(*ty) as Rc<dyn BasicType<'ctx> + 'ctx>,
        }).map(ResolvedType::Basic),
        TypeName::FnType(arg, rt) => {
            if let TypeName::FnType(_, _) = *rt {
                let arg = match resolve_type(*arg, known_types, ctx)?  {
                    ResolvedType::Basic(b) => b,
                    ResolvedType::Fn(f) => f.ptr_type(AddressSpace::Generic).into_rc() as Rc<dyn BasicType<'ctx> + 'ctx>
                };
                resolve_type(*rt, known_types, ctx)
                .map(|rt| 
                match rt  {
                    ResolvedType::Basic(b) => b,
                    ResolvedType::Fn(f) => f.ptr_type(AddressSpace::Generic).into_rc() as Rc<dyn BasicType<'ctx> + 'ctx>
                }
                )
                .and_then(
                    |rt|  
                        Some(
                            rt
                            .fn_type(&[arg.as_basic_type_enum().into()], false)
                            .ptr_type(AddressSpace::Generic)
                        )
                ).map(|f| f.into_rc() as Rc<dyn BasicType<'ctx> + 'ctx>)
                .map(ResolvedType::Basic)
            } else {
                let arg = match resolve_type(*arg, known_types, ctx)?  {
                    ResolvedType::Basic(b) => b,
                    ResolvedType::Fn(f) => f.ptr_type(AddressSpace::Generic).into_rc() as Rc<dyn BasicType<'ctx> + 'ctx>
                };
                resolve_type(*rt, known_types, ctx)
                .map(|rt| match rt  {
                    ResolvedType::Basic(b) => b,
                    ResolvedType::Fn(f) => f.ptr_type(AddressSpace::Generic).into_rc() as Rc<dyn BasicType<'ctx> + 'ctx>
                })
                .map(|rt| rt.fn_type(&[arg.as_basic_type_enum().into()], false)).map(ResolvedType::Fn)
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use inkwell::targets::{Target, InitializationConfig};

    use super::*;
    #[test]
    fn resolve_type() {
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let ctx = Context::create();
        let mut known_types : HashMap<String,BasicTypeEnum> = HashMap::new();
        known_types.insert("int8".to_string(), ctx.i8_type().as_basic_type_enum());
        known_types.insert("int16".to_string(), ctx.i16_type().as_basic_type_enum());
        known_types.insert("int32".to_string(), ctx.i32_type().as_basic_type_enum());
        known_types.insert("int64".to_string(), ctx.i64_type().as_basic_type_enum());
        known_types.insert("float32".to_string(), ctx.f32_type().as_basic_type_enum());
        known_types.insert("float64".to_string(), ctx.f64_type().as_basic_type_enum());
        let ty = TypeName::FnType(TypeName::ValueType("int8".to_string()).boxed(), TypeName::ValueType("int8".to_string()).boxed());
        assert_eq!(
            super::resolve_type(ty,&known_types,&ctx),
            Some(ResolvedType::Fn(known_types[&"int8".to_string()].fn_type(&[known_types["int8"].into()],false))),
            "int8 -> int8"
        );
        let ty = TypeName::FnType(
            TypeName::FnType(
                TypeName::ValueType("int8".to_string()).boxed(),
                TypeName::ValueType("int8".to_string()).boxed()
            ).boxed(),
            TypeName::ValueType("int8".to_string()).boxed()
        );
        let int8 = known_types[&"int8".to_owned()];
        let fun_arg_t = int8.fn_type(&[int8.into()], false).ptr_type(AddressSpace::Generic);
        assert_eq!(
            super::resolve_type(ty, &known_types, &ctx),
            Some(ResolvedType::Fn(int8.fn_type(&[fun_arg_t.into()], false))),
            "( int8 -> int8 ) -> int8"
        );
    }
}
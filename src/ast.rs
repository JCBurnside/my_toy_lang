use std::collections::HashMap;

use inkwell::{
    context::Context,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum},
    AddressSpace,
};

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

pub fn try_map_type<'ctx, 'map>(
    ty: TypeName,
    known_types: &'map HashMap<String, AnyTypeEnum<'ctx>>,
) -> Option<AnyTypeEnum<'ctx>> {
    match ty {
        TypeName::ValueType(name) => known_types.get(&name).cloned(),
        TypeName::FnType(left, right) => {
            let left = BasicTypeEnum::try_from(try_map_type(*left, known_types)?).ok()?;
            try_map_type(*right, known_types)
                .map(|right| {
                    if right.is_array_type() {
                        right.into_array_type().fn_type(&[left.into()], false)
                    } else if right.is_float_type() {
                        right.into_float_type().fn_type(&[left.into()], false)
                    } else if right.is_function_type() {
                        right
                            .into_function_type()
                            .ptr_type(AddressSpace::Generic)
                            .fn_type(&[left.into()], false)
                    } else if right.is_int_type() {
                        right.into_int_type().fn_type(&[left.into()], false)
                    } else if right.is_pointer_type() {
                        right.into_pointer_type().fn_type(&[left.into()], false)
                    } else if right.is_struct_type() {
                        right.into_struct_type().fn_type(&[left.into()], false)
                    } else if right.is_vector_type() {
                        right.into_vector_type().fn_type(&[left.into()], false)
                    } else {
                        unreachable!()
                    }
                })
                .map(|right| right.as_any_type_enum())
        }
    }
}

pub fn verify_types<'ctx>(
    expr: &Expr,
    types: &HashMap<String, AnyTypeEnum<'ctx>>,
    declarations: &HashMap<String, AnyTypeEnum<'ctx>>,
) -> bool {
    match expr {
        Expr::BinaryOpCall { ident, lhs, rhs } => todo!(),
        Expr::UnaryOpCall { ident, operand } => todo!(),
        Expr::FnCall { ident, args } => declarations.get(ident).map_or(false, |ty| true),
        Expr::Return { expr } => todo!(),
        Expr::Block { sub } => todo!(),
        Expr::Literal { value, ty } => todo!(),
        Expr::Declaration {
            is_op,
            ident,
            ty,
            args,
            value,
        } => todo!(),
    }
}

fn get_fn_arg_types<'ctx>(ty: AnyTypeEnum<'ctx>) -> Vec<AnyTypeEnum<'ctx>> {
    match ty {
        AnyTypeEnum::FunctionType(fun) => {
            let params = fun.get_param_types();
            let arg = params.first().unwrap();
            let rt = &fun.get_return_type().unwrap();
            if rt.is_pointer_type() {
                let rt = rt.into_pointer_type();
                if rt.get_element_type().is_function_type() {
                    let mut out = vec![arg.as_any_type_enum()];
                    let mut rhs = get_fn_arg_types(ty);
                    out.append(&mut rhs);
                    out
                } else {
                    vec![arg.as_any_type_enum()]
                }
            } else {
                vec![arg.as_any_type_enum()]
            }
        }
        _ => unreachable!(),
    }
}

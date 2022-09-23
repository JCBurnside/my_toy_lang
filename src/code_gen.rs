use std::collections::HashMap;
use std::convert::TryInto;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue};
use itertools::Itertools;


use crate::ast::TypedExpr;

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    known_functions: HashMap<String, FunctionValue<'ctx>>,
    known_types: HashMap<String, BasicTypeEnum<'ctx>>,
    known_values: HashMap<String, BasicValueEnum<'ctx>>,
    top_level: bool,
}

impl<'ctx> CodeGen<'ctx> {

    pub fn with_module(
        ctx: &'ctx Context,
        known_functions: HashMap<String, FunctionValue<'ctx>>,
        known_types: HashMap<String, BasicTypeEnum<'ctx>>,
        known_values: HashMap<String, BasicValueEnum<'ctx>>,
        module: Module<'ctx>,
    ) -> Self {
        let builder = ctx.create_builder();
        Self {
            ctx,
            module,
            builder,
            known_functions,
            known_types,
            known_values,
            top_level: true,
        }
    }

    pub fn compile_expr(&mut self, expr: &TypedExpr<'ctx>) -> AnyValueEnum<'ctx> {
        match expr {
            TypedExpr::BinaryOpCall {
                ident,
                rt,
                lhs,
                rhs,
            } => {
                let lhs = self.compile_expr(lhs.as_ref());
                let rhs = self.compile_expr(rhs.as_ref());
                match ident.as_str() {
                    "+" => {
                        let rt = rt.as_basic_type_enum();
                        if rt.is_int_type() {
                            let lhs = lhs.into_int_value();
                            let rhs = rhs.into_int_value();
                            self.builder
                                .build_int_add(lhs, rhs, "tmp_add")
                                .as_any_value_enum()
                        } else if rt.is_float_type() {
                            let lhs = lhs.into_float_value();
                            let rhs = rhs.into_float_value();
                            self.builder
                                .build_float_add(lhs, rhs, "temp_add")
                                .as_any_value_enum()
                        } else {
                            panic!("how did we get here again?");
                        }
                    }
                    "-" => {
                        let rt = rt.as_basic_type_enum();
                        if rt.is_int_type() {
                            let lhs = lhs.into_int_value();
                            let rhs = rhs.into_int_value();
                            self.builder
                                .build_int_sub(lhs, rhs, "tmp_sub")
                                .as_any_value_enum()
                        } else if rt.is_float_type() {
                            let lhs = lhs.into_float_value();
                            let rhs = rhs.into_float_value();
                            self.builder
                                .build_float_sub(lhs, rhs, "tmp_sub")
                                .as_any_value_enum()
                        } else {
                            panic!("how did we get here again?");
                        }
                    }
                    "*" => {
                        let rt = rt.as_basic_type_enum();
                        if rt.is_int_type() {
                            let lhs = lhs.into_int_value();
                            let rhs = rhs.into_int_value();
                            self.builder
                                .build_int_mul(lhs, rhs, "tmp_mul")
                                .as_any_value_enum()
                        } else if rt.is_float_type() {
                            let lhs = lhs.into_float_value();
                            let rhs = rhs.into_float_value();
                            self.builder
                                .build_float_mul(lhs, rhs, "tmp_mul")
                                .as_any_value_enum()
                        } else {
                            panic!("how did we get here again?");
                        }
                    }
                    "/" => {
                        let rt = rt.as_basic_type_enum();
                        if rt.is_int_type() {
                            let lhs = lhs.into_int_value();
                            let rhs = rhs.into_int_value();
                            self.builder
                                .build_int_signed_div(lhs, rhs, "tmp_div")
                                .as_any_value_enum()
                        } else if rt.is_float_type() {
                            let lhs = lhs.into_float_value();
                            let rhs = rhs.into_float_value();
                            self.builder
                                .build_float_div(lhs, rhs, "tmp_div")
                                .as_any_value_enum()
                        } else {
                            panic!("how did we get here again?");
                        }
                    }
                    _ => unreachable!(),
                }
            }
            TypedExpr::UnaryOpCall { ident, rt, operand } => todo!(),
            TypedExpr::FnCall { ident, rt, args } => {
                let fun = self.module.get_function(ident).unwrap();
                let args = args
                    .iter()
                    .map(|expr| convert_to_basic_value(self.compile_expr(expr)).into())
                    .collect_vec();
                self.builder
                    .build_call(dbg!(fun), &args, "tmp")
                    .try_as_basic_value()
                    .expect_left("")
                    .as_any_value_enum()
            }
            TypedExpr::Return { rt, expr } => {
                let sub = self.compile_expr(expr.as_ref());
                self.builder
                    .build_return(Some(&convert_to_basic_value(sub)))
                    .as_any_value_enum()
                    .try_into()
                    .unwrap()
            }
            TypedExpr::Literal { rt, value } => match rt.as_ref().as_basic_type_enum() {
                BasicTypeEnum::StructType(ty) => {
                    let cs = self.ctx.const_string(value.as_bytes(), false);
                    let gs = self.module.add_global(cs.get_type(), None, "");
                    gs.set_initializer(&cs);
                    gs.set_constant(true);
                    let ptr = unsafe {
                        gs.as_pointer_value().const_in_bounds_gep(&[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_zero(),
                        ])
                    };
                    let ptr_end = unsafe {
                        gs.as_pointer_value().const_in_bounds_gep(&[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_int(value.len() as u64, false),
                        ])
                    };
                    let p = self.builder.build_alloca(ty, "");
                    self.builder
                        .build_store(p, ty.const_named_struct(&[ptr.into(), ptr_end.into()]));
                    p.as_any_value_enum()
                }
                BasicTypeEnum::IntType(ty) => {
                    // lazy_static::lazy_static! {
                    //     static ref CHAR_REGEX : Regex = Regex::new("")
                    // }

                    let v = ty.const_int(value.parse().unwrap(), false);
                    v.as_any_value_enum()
                }
                _ => unreachable!(),
            },
            TypedExpr::Block { rt, sub } => {
                for expr in &sub[..(sub.len() - 1)] {
                    self.compile_expr(expr);
                }
                self.compile_expr(sub.last().unwrap())
            }
            TypedExpr::Declaration {
                is_op,
                ident,
                ty,
                args,
                value,
            } => {
                match ty {
                    crate::ast::ResolvedType::Basic(ty) => {
                        // match ty.as_basic_type_enum() {

                        // }
                        todo!()
                    }
                    crate::ast::ResolvedType::Fn(ty) => {
                        println!("adding function {} of type {:?}", ident, ty);
                        let v = self.module.add_function(&ident, ty.clone(), None);
                        let bb = self.ctx.append_basic_block(v, &ident);
                        self.builder.position_at_end(bb);
                        let _ = self.compile_expr(value.as_ref());
                        v.as_any_value_enum()
                    }
                }
            }
        }
    }

    pub fn compile_program(mut self, ast: &[TypedExpr<'ctx>]) -> Module<'ctx> {
        for expr in ast {
            self.compile_expr(expr);
        }
        self.module
    }
}

fn convert_to_basic_value<'ctx>(value: AnyValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
    match value {
        AnyValueEnum::ArrayValue(v) => BasicValueEnum::ArrayValue(v),
        AnyValueEnum::IntValue(v) => BasicValueEnum::IntValue(v),
        AnyValueEnum::FloatValue(v) => BasicValueEnum::FloatValue(v),
        AnyValueEnum::FunctionValue(v) => {
            v.as_global_value().as_pointer_value().as_basic_value_enum()
        }
        AnyValueEnum::PointerValue(v) => BasicValueEnum::PointerValue(v),
        AnyValueEnum::StructValue(v) => BasicValueEnum::StructValue(v),
        AnyValueEnum::VectorValue(v) => BasicValueEnum::VectorValue(v),
        AnyValueEnum::InstructionValue(v) => unimplemented!(),
        AnyValueEnum::MetadataValue(v) => unimplemented!(),
        AnyValueEnum::PhiValue(v) => unimplemented!(),
    }
}

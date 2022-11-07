use std::collections::HashMap;
use std::convert::TryInto;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue};

use itertools::Itertools;

use multimap::MultiMap;

use crate::ast::TypedExpr;
use crate::types::{TypeResolver, ResolvedType};

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    type_resolver : TypeResolver<'ctx>,
    known_functions: HashMap<String, FunctionValue<'ctx>>,
    known_ops: MultiMap<String, FunctionValue<'ctx>>,
    known_values: HashMap<String, BasicValueEnum<'ctx>>,
    locals : HashMap<String, PointerValue<'ctx>>
}

impl<'ctx> CodeGen<'ctx> {
    pub fn with_module(
        ctx: &'ctx Context,
        module: Module<'ctx>,
        mut type_resolver : TypeResolver<'ctx>,
        seed_functions : HashMap<String,ResolvedType>,
        known_values : HashMap<String, BasicValueEnum<'ctx>>,
        seed_ops : MultiMap<String,FunctionValue<'ctx>>
    ) -> Self {
        let builder = ctx.create_builder();
        let known_functions = seed_functions
            .into_iter()
            .map(|(name,ty)| {
                let ty= module
                    .get_function(&name)
                    .unwrap_or_else(|| 
                        module.add_function(
                            &name, 
                            type_resolver.resolve_type_as_any(ty).into_function_type(),
                            None
                        )
                    );
                (name,ty)
            })
            .collect();
        
        Self {
            ctx,
            module,
            builder,
            type_resolver,
            known_functions,
            known_values,
            known_ops : seed_ops,
            locals : HashMap::new()
        }
    }

    pub fn compile_expr(&mut self, expr: &TypedExpr) -> AnyValueEnum<'ctx> {
        match expr {
            TypedExpr::BinaryOpCall {
                ident,
                rt,
                lhs,
                rhs,
            } => {
                let lhs: BasicValueEnum = self.compile_expr(lhs.as_ref()).try_into().unwrap();
                let rhs: BasicValueEnum = self.compile_expr(rhs.as_ref()).try_into().unwrap();
                match ident.as_str() {
                    
                    _ => unreachable!(),
                }
            }
            TypedExpr::UnaryOpCall { ident, rt, operand } => todo!(),
            TypedExpr::FnCall { value, rt, arg } => {
                todo!()
            }
            TypedExpr::Return { rt, expr } => {
                let sub = self.compile_expr(expr.as_ref());
                self.builder
                    .build_return(Some(&convert_to_basic_value(sub)))
                    .as_any_value_enum()
                    .try_into()
                    .unwrap()
            }
            TypedExpr::Literal { rt, value } => match self.type_resolver.resolve_type_as_basic(rt.clone()) {
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
                    crate::types::ResolvedType::Function { arg, returns } => {
                        // todo!();
                        // println!("adding function {} of type {:?}", ident, ty);
                        let v = self.module.add_function(&ident, self.type_resolver.resolve_type_as_any(ty.clone()).into_function_type(), None);
                        for (idx,name) in args.iter().enumerate() {
                            let param = v.get_nth_param(idx as u32).unwrap();
                            param.set_name(&name);
                            self.known_values.insert(name.clone(), param);
                        }
                        let bb = self.ctx.append_basic_block(v, &ident);
                        self.builder.position_at_end(bb);
                        let _ = self.compile_expr(value.as_ref());
                        v.as_any_value_enum()
                    }
                    _ => {
                        //type is a basic
                        todo!()
                    }
                }
            }
            TypedExpr::ValueRead { rt, ident } => {
                self.known_values.get(ident).map(|fun| fun.as_any_value_enum()).or(self.known_functions.get(ident).map(|fun| fun.as_any_value_enum())).unwrap()
            },
        }
    }

    pub fn compile_program(mut self, ast: &[TypedExpr]) -> Module<'ctx> {
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

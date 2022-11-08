use std::collections::HashMap;
use std::convert::TryInto;
use std::result;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, StructType, BasicType, BasicMetadataTypeEnum};
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
                    "+"  => match (lhs,rhs) {
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_add(lhs, rhs, "").as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => { 
                            let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder.build_float_add(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
                            let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder.build_float_add(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_add(lhs, rhs, "").as_any_value_enum(),
                        _=> unimplemented!("Operation is not currently supported.")
                    },
                    "-"  => match (lhs,rhs) {
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_sub(lhs, rhs, "").as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => { 
                            let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder.build_float_sub(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
                            let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder.build_float_sub(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_sub(lhs, rhs, "").as_any_value_enum(),
                        _=> unimplemented!("Operation is not currently supported.")
                    },
                    "*"  => match (lhs,rhs) {
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_mul(lhs, rhs, "").as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => { 
                            let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder.build_float_mul(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
                            let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder.build_float_mul(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_mul(lhs, rhs, "").as_any_value_enum(),
                        _=> unimplemented!("Operation is not currently supported.")
                    },
                    "/"  => match (lhs,rhs) {
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_div(lhs, rhs, "").as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => { 
                            let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder.build_float_div(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
                            let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder.build_float_div(lhs, rhs, "").as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_signed_div(lhs, rhs, "").as_any_value_enum(),
                        _=> unimplemented!("Operation is not currently supported.")
                    }
                    _ => unreachable!(),
                }
            }
            TypedExpr::UnaryOpCall { ident, rt, operand } => todo!(),
            TypedExpr::FnCall { value, rt, arg } => {
                todo!();
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
                        
                        println!("adding function {} of type {:?}", ident, ty);
                        let mut result_ty = ty.clone();
                        let mut curried_args = Vec::with_capacity(args.len() - 1);
                        let mut name = ident.clone();
                        let args_curry_functions = args.iter().take(args.len()-1).map(|arg| {
                            let ResolvedType::Function { arg:arg_t, returns } = result_ty.clone() else { unreachable!() };
                            let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
                            let mut rt_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::Generic).into()];
                            rt_types.extend(curried_args.iter());
                            let arg0_types = rt_types.clone(); 
                            rt_types.push(arg_t);
                            let fun_t = if curried_args.len() == 0 {
                                self.ctx.struct_type(&rt_types, false).fn_type(&[arg_t.into()], false)
                            } else {
                                let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::Generic);
                                self.ctx.struct_type(&rt_types, false).ptr_type(AddressSpace::Generic).fn_type(&[arg0_t.into(),arg_t.into()], false)
                            };
                            curried_args.push(arg_t);
                            result_ty = match *returns{
                                ResolvedType::Pointer { underlining } if let ResolvedType::Function { .. } = underlining.as_ref() => *underlining,
                                _ => *returns
                            };
                            self.module.add_function(&name, fun_t, None)
                        }).collect_vec();
                        
                        let ResolvedType::Function { arg:arg_t, returns:rt } = result_ty else { unreachable!() };
                        let rt = self.type_resolver.resolve_type_as_basic(*rt);
                        let ident2 = ident.clone() + "_impl";
                        let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
                        let fun_t = if curried_args.len() == 0 {
                            rt.fn_type(&[arg_t.into()], false)
                        } else {
                            let mut arg0_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::Generic).into()];
                            arg0_types.extend(curried_args.iter());
                            let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::Generic);
                            rt.fn_type(&[arg0_t.into(),arg_t.into()], false)
                        };
                        let v = self.module.add_function(&ident2, fun_t, None);
                        self.known_functions.insert(ident.clone(), v);
                        let bb = self.ctx.append_basic_block(v, "");
                        self.builder.position_at_end(bb);
                        for (idx,arg_name) in args.iter().enumerate().take(args.len()-1) {
                            let arg = self.builder.build_alloca(curried_args[idx], arg_name.as_str());
                            let gep = unsafe { self.builder.build_in_bounds_gep(v.get_first_param().unwrap().into_pointer_value(), &[self.ctx.i32_type().const_zero(),self.ctx.i32_type().const_int((idx+1) as u64, false)], "") };
                            let value = self.builder.build_load(gep,"");
                            self.builder.build_store(arg,value);
                            self.locals.insert(arg_name.clone(), arg.into());
                        }
                        let last_param = v.get_last_param().unwrap();
                        let arg = self.builder.build_alloca(last_param.get_type(), args.last().unwrap());
                        self.builder.build_store(arg,last_param);
                        self.locals.insert(args.last().cloned().unwrap(), arg.into());
                        self.compile_expr(value.as_ref());
                        v.as_any_value_enum()
                    }
                    _ => {
                        //type is a basic
                        todo!()
                    }
                }
            }
            TypedExpr::ValueRead { rt, ident } => {
                self.locals.get(ident)
                .map(|val| {
                    self.builder.build_load(*val,"").as_any_value_enum()
                })
                .or(self.known_values.get(ident).map(|val| val.as_any_value_enum()))
                .or(self.known_functions.get(ident).map(|fun| fun.as_any_value_enum())).unwrap()
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

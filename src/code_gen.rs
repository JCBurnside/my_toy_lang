use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, BasicType, AnyTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue, CallableValue};

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
    locals : HashMap<String, PointerValue<'ctx>>,
    curried_locals : HashMap<String, (PointerValue<'ctx>,ResolvedType)>,
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
            locals : HashMap::new(),
            curried_locals : HashMap::new(),
        }
    }

    pub fn compile_expr(&mut self, expr: TypedExpr) -> AnyValueEnum<'ctx> {
        match expr {
            TypedExpr::BinaryOpCall {
                ident,
                rt,
                lhs,
                rhs,
            } => {
                let lhs: BasicValueEnum = self.compile_expr(*lhs).try_into().unwrap();
                let rhs: BasicValueEnum = self.compile_expr(*rhs).try_into().unwrap();
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
                    },
                    _ => unreachable!(),
                }
            }
            TypedExpr::UnaryOpCall { ident, rt, operand } => todo!(),
            TypedExpr::FnCall { value, rt, arg } => {
                match *value {
                    TypedExpr::FnCall { value, arg : Some(arg), rt } => {
                        let value = self.compile_expr(*value);
                        let value = convert_to_basic_value(value);
                        let fun_t = self.type_resolver.resolve_type_as_basic(rt).fn_type(&[value.get_type().into(),self.type_resolver.resolve_type_as_basic(arg.get_rt()).into()], false);
                        let value = value.into_pointer_value();
                        let fun = self.builder.build_struct_gep(value, 0, "fn").unwrap();
                        let fun = self.builder.build_bitcast(fun, fun_t.ptr_type(AddressSpace::Generic), "").into_pointer_value();
                        let fun : CallableValue = fun.try_into().unwrap();
                        let arg = convert_to_basic_value(self.compile_expr(*arg));
                        let ret = self.builder.build_call(fun,&[value.into(),arg.into()],"");
                        ret.as_any_value_enum()
                    },
                    TypedExpr::ValueRead { ident,.. } => {
                        let Some(fun) : Option<CallableValue> = self.known_functions.get(&ident)
                        .map(|fun|(*fun).into())
                        .or_else(|| self.curried_locals.get(&ident).map(
                            |(curry,fun_t)| {
                                let fun = self.builder.build_struct_gep(*curry, 0, "").unwrap();
                                // let fun = self.builder.build_bitcast(fun, (*fun_t).ptr_type(AddressSpace::Generic), "").into_pointer_value();
                                fun.try_into().unwrap()
                            }
                        ))  else { unreachable!("Function not found??") };

                        todo!()
                    }
                    _ => unimplemented!("funciton value not supported")
                }
            }
            TypedExpr::Return { rt, expr } => {
                let sub = self.compile_expr(*expr);
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
                // TODO : Maybe insert more basic blocks as you go down in scope?  could be helpful for life time management
                sub.into_iter().fold(None,|_,expr| {

                    Some(self.compile_expr(expr))
                }).unwrap()
            }
            TypedExpr::Declaration {
                is_op,
                ident,
                ty,
                args,
                value,
                curried,
                ..
            } => {
                match & ty {
                    crate::types::ResolvedType::Function { arg, returns } => {
                        
                        let mut result_ty = ty.clone();
                        let mut curried_args = Vec::with_capacity(args.len() - 1);
                        
                        // generate needed supporting functions.
                        let args_curry_functions = args.iter().take(args.len()-1).map(|_| {
                            let ResolvedType::Function { arg:arg_t, returns } = result_ty.clone() else { unreachable!() };
                            let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
                            let mut rt_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::Generic).into()];
                            rt_types.extend(curried_args.iter());
                            let arg0_types = rt_types.clone(); 
                            rt_types.push(arg_t);
                            let fun_t = if curried_args.len() == 0 {
                                self.ctx.struct_type(&rt_types, false).ptr_type(AddressSpace::Generic).fn_type(&[arg_t.into()], false)
                            } else {
                                let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::Generic);
                                self.ctx.struct_type(&rt_types, false).ptr_type(AddressSpace::Generic).fn_type(&[arg0_t.into(),arg_t.into()], false)
                            };
                            curried_args.push(arg_t);
                            result_ty = match *returns{
                                ResolvedType::Pointer { underlining } if let ResolvedType::Function { .. } = underlining.as_ref() => *underlining,
                                _ => *returns
                            };
                            self.module.add_function(&ident, fun_t, None)
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
                        self.compile_expr(*value);

                        for (curr,next) in args_curry_functions.iter().chain(once(&v)).tuple_windows() {

                            let bb = self.ctx.append_basic_block(*curr, "");
                            self.builder.position_at_end(bb);
                            let ret_t = curr.get_type().get_return_type().unwrap().into_pointer_type();
                            let ret = self.builder.build_malloc(ret_t.get_element_type().into_struct_type(), "ret_ptr").unwrap();
                            let next_fn_ptr = self.builder.build_bitcast(next.as_global_value().as_pointer_value(), self.ctx.i8_type().ptr_type(AddressSpace::Generic), "");
                            let next_ptr = self.builder.build_struct_gep(ret, 0, "").unwrap();
                            self.builder.build_store(next_ptr, next_fn_ptr);
                            let last = ret_t.get_element_type().into_struct_type().get_field_types().len() - 1;
                            let new_element = self.builder.build_struct_gep(ret,last as u32, "").unwrap();
                            self.builder.build_store(new_element,curr.get_last_param().unwrap());

                            if let Some(BasicValueEnum::PointerValue(ptr)) = curr.get_first_param() 
                            && let AnyTypeEnum::StructType(strct) = ptr.get_type().get_element_type()
                            && !strct.is_opaque() {
                                let arg0 = unsafe { self.builder.build_in_bounds_gep(ptr, &[self.ctx.i32_type().const_zero()], "") };
                                // let arg0 = self.builder.build_load(arg0, "").into_struct_value();
                                for idx in 1..strct.get_field_types().len() {
                                    let ele = self.builder.build_struct_gep(arg0, idx as _, "").unwrap();
                                    let ele = self.builder.build_load(ele,"");
                                    let ele_out = self.builder.build_struct_gep(ret, idx as u32, "").unwrap();
                                    self.builder.build_store(ele_out, ele);
                                }
                            }
                            self.builder.build_return(Some(&ret));
                        }
                        v.as_any_value_enum()
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            TypedExpr::ValueRead { ident, .. } => {
                self.locals.get(&ident)
                .map(|val| {
                    self.builder.build_load(*val,"").as_any_value_enum()
                })
                .or(self.known_values.get(&ident).map(|val| val.as_any_value_enum()))
                .or(self.known_functions.get(&ident).map(|fun| fun.as_any_value_enum())).unwrap()
            },
        }
    }

    pub fn compile_program<T : IntoIterator<Item = TypedExpr>>(mut self, ast: T) -> Module<'ctx> {
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
        AnyValueEnum::InstructionValue(_) => unimplemented!(),
        AnyValueEnum::MetadataValue(_) => unimplemented!(),
        AnyValueEnum::PhiValue(_) => unimplemented!(),
    }
}

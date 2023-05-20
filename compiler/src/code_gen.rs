use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;
use std::rc::Rc;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, BasicType, AnyTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue, CallableValue, GlobalValue};

use itertools::{Itertools, Either};

use multimap::MultiMap;

use crate::ast::{BinaryOpCall, ValueDeclaration};
use crate::typed_ast::{TypedExpr, TypedBinaryOpCall, TypedFnCall, TypedStatement, TypedValueDeclaration, TypedValueType, TypedDeclaration, collect_args};
use crate::types::{TypeResolver, ResolvedType, self};

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    type_resolver : TypeResolver<'ctx>,
    known_functions: HashMap<String, GlobalValue<'ctx>>,
    incomplete_functions : HashMap<String, FunctionValue<'ctx>>, //this should be the ones left to compile.  sometimes same as above
    _known_ops: MultiMap<String, FunctionValue<'ctx>>,
    known_values: HashMap<String, BasicValueEnum<'ctx>>,
    locals : HashMap<String, PointerValue<'ctx>>,
    current_module : String,
    // curried_locals : HashMap<String, (PointerValue<'ctx>,ResolvedType)>,
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
                    ).as_global_value();
                (name,ty)
            })
            .collect();

        Self {
            ctx,
            module,
            builder,
            type_resolver,
            known_functions,
            incomplete_functions : HashMap::new(),
            known_values,
            _known_ops : seed_ops,
            locals : HashMap::new(),
            current_module : String::new()
            // curried_locals : HashMap::new(),
        }
    }

    fn compile_function(&mut self, decl : TypedValueDeclaration) {
        let v = self.incomplete_functions.get(&decl.ident).unwrap().clone();
        let bb = self.ctx.append_basic_block(v, "arg_declarations");
        self.builder.position_at_end(bb);
        let curried_args = collect_args(&decl.ty).into_iter().map(|it| self.type_resolver.resolve_arg_type(&it)).collect_vec();
        for (idx,arg_name) in decl.args.iter().enumerate().take(decl.args.len()-1) {
            let arg = self.builder.build_alloca(curried_args[idx], arg_name.as_str());
            let gep = unsafe { self.builder.build_in_bounds_gep(v.get_first_param().unwrap().into_pointer_value(), &[self.ctx.i32_type().const_zero(),self.ctx.i32_type().const_int((idx+1) as u64, false)], "") };
            let value = self.builder.build_load(gep,"");
            self.builder.build_store(arg,value);
            self.locals.insert(arg_name.clone(), arg.into());
        }
        let last_param = v.get_last_param().unwrap();
        let arg = self.builder.build_alloca(last_param.get_type(), decl.args.last().unwrap());
        self.builder.build_store(arg,last_param);
        let arg_decl_block = v.get_last_basic_block().unwrap();
        self.builder.position_at_end(arg_decl_block);
        let bb = self.ctx.append_basic_block(v, "start");
        self.builder.build_unconditional_branch(bb);
        self.builder.position_at_end(bb);
        self.locals.insert(decl.args.last().cloned().unwrap(), arg.into());
        match decl.value {
            TypedValueType::Expr(expr) => { 
                let value = self.compile_expr(expr);
                self.builder.build_return(Some(&convert_to_basic_value(value)));
             },
            TypedValueType::Function(body) => for expr in body {
                self.compile_statement(expr);
            },
            TypedValueType::Err => unreachable!("how did an error kind get here"),
        }
    }

    pub fn compile_statement(&mut self, stmnt:TypedStatement) {
        match stmnt {
            TypedStatement::Return(expr) => {
                if let TypedExpr::UnitLiteral = expr {
                    self.builder
                        .build_return(None);
                } else {
                    let sub = self.compile_expr(expr);
                    self.builder
                        .build_return(Some(&convert_to_basic_value(sub)));
                }
            },

            TypedStatement::FnCall(data) => {
                self.compile_expr(TypedExpr::FnCall(data));
            }

            TypedStatement::Declaration(TypedValueDeclaration {
                ident,
                ty,
                args,
                value,
                ..
            }) => {
                if let TypedValueType::Expr(expr) = value {
                    let value = self.builder.build_alloca(self.type_resolver.resolve_type_as_basic(ty), &ident);
                    let result = self.compile_expr(expr);
                    self.builder.build_store::<BasicValueEnum>(value,result.try_into().unwrap());
                    self.locals.insert(ident, value);
                } else {
                    todo!("unsure here?")
                }
            }
            
            _=> todo!()
        }
    }

    pub fn compile_expr(&mut self, expr: TypedExpr) -> AnyValueEnum<'ctx> {
        match expr {
            TypedExpr::BinaryOpCall(TypedBinaryOpCall {
                operator,
                lhs,
                rhs,
                ..
            })=> {
                let lhs: BasicValueEnum = self.compile_expr(*lhs).try_into().unwrap();
                let rhs: BasicValueEnum = self.compile_expr(*rhs).try_into().unwrap();
                match operator.as_str() {
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
            TypedExpr::UnaryOpCall { .. } => todo!(),
            TypedExpr::FnCall(TypedFnCall { value, arg:Some(arg), rt, arg_t }) => {
                let arg_t = self.type_resolver.resolve_arg_type(&arg_t);
                let arg = self.compile_expr(*arg);
                let arg : BasicValueEnum = arg.try_into().unwrap();
                let value =self.compile_expr(*value);
                match value {
                    AnyValueEnum::PointerValue(target) => match target.get_type().get_element_type() {
                            AnyTypeEnum::StructType(strct_t) =>{
                            let target_fun = self.builder.build_struct_gep(target, 0, "").unwrap();
                            let target_fun = self.builder.build_load(target_fun, "");
                            let ty = if let ResolvedType::Function { .. } = rt {
                                let mut fields = strct_t.get_field_types();
                                fields.push(arg_t);
                                self.ctx.struct_type(&fields,false).ptr_type(AddressSpace::default()).as_basic_type_enum()
                            } else {
                                self.type_resolver.resolve_type_as_basic(rt)
                            };
                            let ty = ty.fn_type(&[strct_t.ptr_type(AddressSpace::default()).into(),arg_t.into()], false).ptr_type(AddressSpace::default());
                            let target_fun = self.builder.build_bitcast(target_fun, ty, "").into_pointer_value();
                            let target_fun : CallableValue = target_fun.try_into().unwrap();
                            self.builder.build_call(target_fun,&[target.into(),arg.into()],"").as_any_value_enum()
                        }
                        AnyTypeEnum::FunctionType(_) => {
                            let target : CallableValue = target.try_into().unwrap();
                            self.builder.build_call(target, &[arg.into()], "").as_any_value_enum()
                        }
                        _ => {
                            self.module.print_to_file("./error.llvm");
                            unreachable!();
                        }
                    },
                    AnyValueEnum::FunctionValue(target) => {

                        let expect_ty = target.get_type().get_param_types();
                        println!("{:?}\n{:?}", expect_ty, arg_t);
                        self.builder.build_call(target,&[arg.into()], "").as_any_value_enum()
                    }
                    _=>{
                        self.module.print_to_file("./error.llvm");
                        unreachable!();
                    }
                }

                
            }
            TypedExpr::FnCall(TypedFnCall{ value, .. })=> {
                //this should only ever be a named value?
                let TypedExpr::ValueRead(ident,_) = *value else { unreachable!("not a function name?") };
                let Some(gv)= self.known_functions.get(&ident) else { unreachable!("function not found") };
                let fun = self.builder.build_struct_gep(gv.as_pointer_value(), 0, "").unwrap();
                let fun : CallableValue = fun.try_into().unwrap();
                self.builder.build_call(fun,&[gv.as_pointer_value().into()],"").as_any_value_enum()
            }

            TypedExpr::ValueRead(ident,_) => {
                self.locals.get(&ident)
                .map(|val| {
                    self.builder.build_load(*val,"").as_any_value_enum()
                })
                .or(self.known_values.get(&ident).map(|val| val.as_any_value_enum()))
                .or(self.known_functions.get(&ident).map(|fun| fun.as_any_value_enum()))
                .unwrap()
            },
            TypedExpr::UnitLiteral =>{
                match self.module.get_global("()") {
                    Some(g) => g.as_any_value_enum(),
                    None => {
                        let unit = self.ctx.const_struct(&[], false);
                        let gs = self.module.add_global(unit.get_type(), None, "()");
                        gs.set_initializer(&unit);
                        gs.set_constant(true);
                        gs.as_any_value_enum()
                    }
                }
            },
            TypedExpr::IntegerLiteral { value, size } => {
                let ty = self.type_resolver.resolve_type_as_basic(ResolvedType::Int { signed: true, width: size }).into_int_type();
                let v = ty.const_int_from_string(&value, inkwell::types::StringRadix::Decimal).unwrap();
                v.as_any_value_enum()
            },
            TypedExpr::FloatLiteral { value, size } => {
                let ty= self.type_resolver.resolve_type_as_basic(ResolvedType::Float { width: size }).into_float_type();
                let v = ty.const_float_from_string(&value);
                v.as_any_value_enum()
            },
            TypedExpr::StringLiteral(value) => {
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
                let ty = self.type_resolver.resolve_type_as_basic(types::STR).into_struct_type();
                let p = self.builder.build_alloca(ty, "");
                self.builder
                    .build_store(p, ty.const_named_struct(&[ptr.into(), ptr_end.into()]));
                p.as_any_value_enum()
            },
            TypedExpr::CharLiteral(value) => {
                let ty = self.type_resolver.resolve_type_as_basic(types::CHAR).into_int_type();
                let v = ty.const_int(value.bytes().next().unwrap() as u64,false);
                v.as_any_value_enum()
            },
            TypedExpr::ArrayLiteral { contents } => todo!(),
            TypedExpr::ListLiteral { contents } => todo!(),
            TypedExpr::TupleLiteral { contents } => todo!(),
            TypedExpr::ErrorNode => unreachable!(),
        }
    }

    pub fn compile_decl(&mut self, decl : TypedDeclaration) -> Module<'ctx> {
        match decl {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(data) => {
                self.compile_function(data);
            },
            TypedDeclaration::TypeDefinition() => todo!(),
        }
        self.module.clone()
    }

    fn create_define(&mut self,decl : &TypedDeclaration) {
        match decl {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(decl) => {
                let ty = self.type_resolver.resolve_type_as_any(decl.ty.clone());
                if ty.is_function_type() {
                    let fun = self.create_curry_list(decl);
                    self.known_functions.insert(decl.ident.clone(), fun);
                }
            },
            TypedDeclaration::TypeDefinition() => todo!(),
        }
    }

    fn create_curry_list(&mut self, decl : &TypedValueDeclaration) -> GlobalValue<'ctx> {
        let TypedValueDeclaration { ident, args, value, ty, ..} = decl;
        let ident = self.current_module.clone() + "$" + ident; 
        let mut result_ty = ty.clone();
        let mut curried_args = Vec::with_capacity(args.len() - 1);
        let gs = self.module.add_global(self.ctx.struct_type(&[self.ctx.i8_type().ptr_type(AddressSpace::default()).into()], false),None, &ident);
                
        // generate needed supporting functions.
        let args_curry_functions = args.iter().rev().take(args.len()-1).map(|_| {
            let ResolvedType::Function { arg:arg_t, returns } = result_ty.clone() else { unreachable!() };
            let arg_t = self.type_resolver.resolve_arg_type(&arg_t);
            let mut rt_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::default()).into()];
            rt_types.extend(curried_args.iter());
            let arg0_types = rt_types.clone();
            rt_types.push(arg_t);
            let fun_t = if curried_args.len() == 0 {
                self.ctx
                    .struct_type(&rt_types, false)
                    .ptr_type(AddressSpace::default())
                    .fn_type(&[
                        self.ctx.struct_type(&[self.ctx.i8_type().ptr_type(AddressSpace::default()).into()],false).into(),
                        arg_t.into()
                    ], 
                    false)
            } else {
                let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::default());
                self.ctx
                    .struct_type(&rt_types, false)
                    .ptr_type(AddressSpace::default())
                    .fn_type(&[arg0_t.into(),arg_t.into()], false)
            };
            
            result_ty = match *returns{
                ResolvedType::Pointer { underlining } if let ResolvedType::Function { .. } = underlining.as_ref() => *underlining,
                _ => *returns
            };
            let fun = self.module.add_function(&ident, fun_t, None);
            curried_args.push(arg_t);
            fun
        }).collect_vec();

        let ResolvedType::Function { arg:arg_t, returns:rt } = result_ty else { unreachable!() };
        let fun_t = if rt.as_ref() == &ResolvedType::Void || rt.as_ref() == &ResolvedType::Unit {
            let rt = self.ctx.void_type();
            let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
            if curried_args.len() == 0 {
                rt.fn_type(&[arg_t.into()], false)
            } else {
                let mut arg0_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::default()).into()];
                arg0_types.extend(curried_args.iter());
                let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::default());
                rt.fn_type(&[arg0_t.into(),arg_t.into()], false)
            }
        } else {
            let rt = self.type_resolver.resolve_type_as_basic(*rt);
            let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
            if curried_args.len() == 0 {
                rt.fn_type(&[arg_t.into()], false)
            } else {
                let mut arg0_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::default()).into()];
                arg0_types.extend(curried_args.iter());
                let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::default());
                rt.fn_type(&[arg0_t.into(),arg_t.into()], false)
            }
        };
        let v = self.module.add_function(&ident, fun_t, None);
        for (curr,next) in args_curry_functions.iter().chain(once(&v)).tuple_windows() {

            let bb = self.ctx.append_basic_block(*curr, "");
            self.builder.position_at_end(bb);
            let ret_t = curr.get_type().get_return_type().unwrap().into_pointer_type();
            let ret = self.builder.build_malloc(ret_t.get_element_type().into_struct_type(), "ret_ptr").unwrap();
            let next_fn_ptr = self.builder.build_bitcast(next.as_global_value().as_pointer_value(), self.ctx.i8_type().ptr_type(AddressSpace::default()), "");
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
        self.incomplete_functions.insert(decl.ident.clone(), v);
        gs.set_constant(true);
        let cs = self.ctx.const_struct(&[
            args_curry_functions.first().unwrap_or(&v)
            .as_global_value()
            .as_pointer_value()
            .const_cast(self.ctx.i8_type().ptr_type(AddressSpace::default()))
            .into()
        ], false);
        gs.set_initializer(&cs);
        gs
    }

    pub fn compile_module(&mut self, ast : crate::typed_ast::TypedModuleDeclaration) -> Module<'ctx> {
        for decl in &ast.declarations{
            self.create_define(decl);
        }

        for decl in ast.declarations {
            self.compile_decl(decl);
        }

        self.module.clone()
    }

    pub fn compile_program(mut self, ast: crate::typed_ast::ProgramTyped, is_lib : bool) -> Module<'ctx> {
        use crate::util::ExtraUtilFunctions;
        let main_name = ast.iter().find_map(|file| {
            file.declarations.iter().find(|decl| if let TypedDeclaration::Value(decl) = decl {
                decl.ident == "main" && decl.ty == ResolvedType::Function { arg: types::UNIT.boxed(), returns: types::UNIT.boxed() }
            } else { 
                false
            }).map(|_| file.name.clone().unwrap_or_default() + "$main")
        });
        for file in ast {
            self.current_module = file.name.clone().unwrap_or_default();
            self.compile_module(file);
        }
        if !is_lib {
            if let Some(main_name) = main_name {
                let entry = self.module.add_function("main", self.ctx.void_type().fn_type(&[], false), None);
                let bb = self.ctx.append_basic_block(entry, "");
                self.builder.position_at_end(bb);
                let gs = self.module.get_global(&main_name).unwrap();
                let main = self.builder.build_struct_gep(gs.as_pointer_value(), 0, "").unwrap();
                let main = self.builder.build_load(main, "").into_pointer_value();
                let main = self.builder
                    .build_bitcast(
                        main, 
                        self.ctx
                            .void_type()
                            .fn_type(&[
                                self.ctx.struct_type(&[], false).into()
                            ],false)
                            .ptr_type(AddressSpace::default()), 
                        ""
                    )
                    .into_pointer_value();
                let main : CallableValue = main.try_into().unwrap();
                self.builder.build_call(main, &[self.ctx.const_struct(&[], false).into()], "");
                self.builder.build_return(None);
            } else {
                panic!("could not find suitable main");
            }
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
        AnyValueEnum::InstructionValue(_) |
        AnyValueEnum::MetadataValue(_) |
        AnyValueEnum::PhiValue(_) => unimplemented!(),
    }
}

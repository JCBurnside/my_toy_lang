use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DIFlags, DIFlagsConstants, DILocalVariable, DISubprogram,
    DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicType};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, CallableValue, FunctionValue, GlobalValue,
    PointerValue,
};
use inkwell::AddressSpace;

use itertools::Itertools;

use multimap::MultiMap;

use crate::typed_ast::{
    collect_args, TypedBinaryOpCall, TypedDeclaration, TypedExpr, TypedFnCall, TypedStatement,
    TypedValueDeclaration, TypedValueType,
};
use crate::types::{self, ResolvedType, TypeResolver};

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    type_resolver: TypeResolver<'ctx>,
    known_functions: HashMap<String, GlobalValue<'ctx>>,
    incomplete_functions: HashMap<String, FunctionValue<'ctx>>, //this should be the ones left to compile.  sometimes same as above
    _known_ops: MultiMap<String, FunctionValue<'ctx>>,
    known_values: HashMap<String, BasicValueEnum<'ctx>>,
    locals: HashMap<String, PointerValue<'ctx>>,
    current_module: String,
    // debug info starts here
    dibuilder: Option<DebugInfoBuilder<'ctx>>,
    compile_unit: Option<DICompileUnit<'ctx>>,
    difile: Option<DIFile<'ctx>>,
    difunction: Option<DISubprogram<'ctx>>,
    dilocals: HashMap<String, DILocalVariable<'ctx>>, // curried_locals : HashMap<String, (PointerValue<'ctx>,ResolvedType)>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn with_module(
        ctx: &'ctx Context,
        module: Module<'ctx>,
        mut type_resolver: TypeResolver<'ctx>,
        seed_functions: HashMap<String, ResolvedType>,
        known_values: HashMap<String, BasicValueEnum<'ctx>>,
        seed_ops: MultiMap<String, FunctionValue<'ctx>>,
    ) -> Self {
        let builder = ctx.create_builder();
        let known_functions = seed_functions
            .into_iter()
            .map(|(name, ty)| {
                let ty = module
                    .get_function(&name)
                    .unwrap_or_else(|| {
                        module.add_function(
                            &name,
                            type_resolver.resolve_type_as_function(&ty),
                            None,
                        )
                    })
                    .as_global_value();
                (name, ty)
            })
            .collect();

        Self {
            ctx,
            module,
            builder,
            type_resolver,
            known_functions,
            incomplete_functions: HashMap::new(),
            known_values,
            _known_ops: seed_ops,
            locals: HashMap::new(),
            current_module: String::new(),
            dibuilder: None,
            compile_unit: None,
            difile: None,
            difunction: None,
            dilocals: HashMap::new(), // curried_locals : HashMap::new(),
        }
    }

    fn compile_function(&mut self, decl: TypedValueDeclaration) {
        let v = self.incomplete_functions.get(&decl.ident).unwrap().clone();

        if let Some(dibuilder) = &self.dibuilder {
            let Some(difile) = self.difile.as_ref() else { unreachable!() };
            let fnty = self.type_resolver.get_di_fn(&decl.ty, dibuilder, difile);
            let ident = self.current_module.clone() + "$" + &decl.ident;
            let fun_scope = dibuilder.create_function(
                difile.as_debug_info_scope(),
                &decl.ident,
                Some(&ident),
                difile.clone(),
                decl.loc.0.try_into().unwrap(),
                fnty,
                false, //TODO will be based on access level.
                true, //TODO deal with foward declares.  likely to come with multiple file programs and external linking
                decl.loc.0.try_into().unwrap(),
                DIFlags::PUBLIC,
                false,
            );
            v.set_subprogram(fun_scope);
            let loc = dibuilder.create_debug_location(
                self.ctx,
                decl.loc.0.try_into().unwrap(),
                decl.loc.1.try_into().unwrap(),
                fun_scope.as_debug_info_scope(),
                None,
            );
            self.builder.set_current_debug_location(loc);
            self.difunction = Some(fun_scope);
        }
        let bb = self.ctx.append_basic_block(v, "arg_declarations");
        self.builder.position_at_end(bb);
        let curried_args = collect_args(&decl.ty)
            .into_iter()
            .map(|it| self.type_resolver.resolve_arg_type(&it))
            .collect_vec();
        let first_arg = {
            let mut curried_args = curried_args.clone();
            curried_args.insert(
                0,
                self.ctx
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            );
            let curried_args = curried_args.into_iter().map(|it| it.into()).collect_vec();
            let first_arg = v.get_first_param().unwrap().into_pointer_value();
            let actual_ty = self
                .ctx
                .struct_type(&curried_args, false)
                .ptr_type(AddressSpace::default());
            self.builder.build_bitcast(first_arg, actual_ty, "")
        };
        for (idx, arg_name) in decl.args.iter().enumerate().take(decl.args.len() - 1) {
            let arg = self
                .builder
                .build_alloca(curried_args[idx], arg_name.ident.as_str());
            let gep = unsafe {
                self.builder.build_in_bounds_gep(
                    first_arg.into_pointer_value(),
                    &[
                        self.ctx.i32_type().const_zero(),
                        self.ctx.i32_type().const_int((idx + 1) as u64, false),
                    ],
                    "",
                )
            };
            let value = self.builder.build_load(gep, "");
            self.builder.build_store(arg, value);
            self.locals.insert(arg_name.ident.clone(), arg.into());
            if let Some(fnscope) = &self.difunction {
                let Some(dibuilder) = &self.dibuilder else { unreachable!() };
                let Some(file) = &self.difile else { unreachable!() };
                let ty = &decl.ty.as_c_function().0[idx];
                let ty = self.type_resolver.get_di(ty, dibuilder);
                let local = dibuilder.create_parameter_variable(
                    fnscope.as_debug_info_scope(),
                    &arg_name.ident,
                    idx as u32 + 1,
                    file.clone(),
                    decl.loc.0.try_into().unwrap(),
                    ty,
                    true,
                    DIFlags::TYPE_PASS_BY_VALUE,
                );
                let diloc = dibuilder.create_debug_location(
                    self.ctx,
                    arg_name.loc.0.try_into().unwrap(),
                    arg_name.loc.1.try_into().unwrap(),
                    fnscope.as_debug_info_scope(),
                    None,
                );
                dibuilder.insert_declare_at_end(arg, Some(local), None, diloc, bb);
                self.dilocals.insert(arg_name.ident.clone(), local);
            }
        }
        let last_param = v.get_last_param().unwrap();
        let last_param_info = decl.args.last().unwrap();
        let arg = self
            .builder
            .build_alloca(last_param.get_type(), &last_param_info.ident);
        self.builder.build_store(arg, last_param);
        if let Some(fnscope) = &self.difunction {
            let Some(dibuilder) = &self.dibuilder else { unreachable!() };
            let Some(file) = &self.difile else { unreachable!() };
            let types = decl.ty.as_c_function().0;
            let ty = types.last().unwrap();
            let ty = self.type_resolver.get_di(ty, dibuilder);
            let local = dibuilder.create_parameter_variable(
                fnscope.as_debug_info_scope(),
                &last_param_info.ident,
                decl.args.len() as u32 + 1,
                file.clone(),
                decl.loc.0.try_into().unwrap(),
                ty,
                true,
                DIFlags::TYPE_PASS_BY_VALUE,
            );
            let diloc = dibuilder.create_debug_location(
                self.ctx,
                last_param_info.loc.0.try_into().unwrap(),
                last_param_info.loc.1.try_into().unwrap(),
                fnscope.as_debug_info_scope(),
                None,
            );
            dibuilder.insert_declare_at_end(arg, Some(local), None, diloc, bb);
            self.dilocals.insert(last_param_info.ident.clone(), local);
        }
        let bb = self.ctx.append_basic_block(v, "start");
        self.builder.build_unconditional_branch(bb);
        self.builder.position_at_end(bb);
        self.locals
            .insert(last_param_info.ident.clone(), arg.into());
        match decl.value {
            TypedValueType::Expr(expr) => {
                let value = self.compile_expr(expr);
                self.builder
                    .build_return(Some(&convert_to_basic_value(value)));
            }
            TypedValueType::Function(body) => {
                for expr in body {
                    self.compile_statement(expr);
                }
            }
            TypedValueType::Err => unreachable!("how did an error kind get here"),
        }
        self.difunction = None;
        self.dilocals.clear();
    }

    pub fn compile_statement(&mut self, stmnt: TypedStatement) {
        match stmnt {
            TypedStatement::Return(expr, loc) => {
                if let Some(dibuilder) = &self.dibuilder {
                    let Some(difun) = &self.difunction else {unreachable!()};
                    let loc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        difun.as_debug_info_scope(),
                        None,
                    );
                    self.builder.set_current_debug_location(loc);
                }
                if let TypedExpr::UnitLiteral = expr {
                    self.builder.build_return(None);
                } else {
                    let sub = self.compile_expr(expr);
                    self.builder
                        .build_return(Some(&convert_to_basic_value(sub)));
                }
            }

            TypedStatement::FnCall(data) => {
                if let Some(dibuilder) = &self.dibuilder {
                    let Some(difun) = &self.difunction else {unreachable!()};
                    let loc = dibuilder.create_debug_location(
                        self.ctx,
                        data.loc.0.try_into().unwrap(),
                        data.loc.1.try_into().unwrap(),
                        difun.as_debug_info_scope(),
                        None,
                    );
                    self.builder.set_current_debug_location(loc);
                }
                self.compile_expr(TypedExpr::FnCall(data));
            }

            TypedStatement::Declaration(TypedValueDeclaration {
                ident,
                ty,
                value,
                loc,
                ..
            }) => {
                if let TypedValueType::Expr(expr) = value {
                    let rty = self
                        .type_resolver
                        .resolve_type_as_basic(ty.clone())
                        .ptr_type(AddressSpace::default());
                    let pvalue = self.builder.build_alloca(rty, &ident);
                    let result = self.compile_expr(expr);
                    self.builder
                        .build_store::<BasicValueEnum>(pvalue, result.try_into().unwrap());
                    self.locals.insert(ident.clone(), pvalue);
                    if let Some(fnscope) = &self.difunction {
                        let Some(dibuilder) = &self.dibuilder else { unreachable!() };
                        let Some(file) = &self.difile else { unreachable!() };
                        let diloc = dibuilder.create_debug_location(
                            self.ctx,
                            loc.0.try_into().unwrap(),
                            loc.1.try_into().unwrap(),
                            fnscope.as_debug_info_scope(),
                            None,
                        );
                        let local = dibuilder.create_auto_variable(
                            fnscope.as_debug_info_scope(),
                            &ident,
                            file.clone(),
                            loc.0.try_into().unwrap(),
                            self.type_resolver.get_di(&ty, dibuilder),
                            false,
                            DIFlags::ZERO,
                            0,
                        );
                        self.dilocals.insert(ident.clone(), local);
                        dibuilder.insert_declare_at_end(
                            pvalue,
                            Some(local),
                            None,
                            diloc,
                            self.builder.get_insert_block().unwrap(),
                        );
                    }
                } else {
                    todo!("unsure here?")
                }
            }

            _ => todo!(),
        }
    }

    pub fn compile_expr(&mut self, expr: TypedExpr) -> AnyValueEnum<'ctx> {
        match expr {
            TypedExpr::BinaryOpCall(TypedBinaryOpCall {
                operator,
                lhs,
                rhs,
                loc,
                ..
            }) => {
                let lhs: BasicValueEnum = self.compile_expr(*lhs).try_into().unwrap();
                let rhs: BasicValueEnum = self.compile_expr(*rhs).try_into().unwrap();
                if let Some(dibuilder) = &self.dibuilder {
                    let loc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        self.difunction.as_ref().unwrap().as_debug_info_scope(),
                        None,
                    );
                    self.builder.set_current_debug_location(loc);
                }
                match operator.as_str() {
                    "+" => match (lhs, rhs) {
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => self
                            .builder
                            .build_float_add(lhs, rhs, "")
                            .as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                            let rhs =
                                self.builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder
                                .build_float_add(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                            let lhs =
                                self.builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder
                                .build_float_add(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                            self.builder.build_int_add(lhs, rhs, "").as_any_value_enum()
                        }
                        _ => unimplemented!("Operation is not currently supported."),
                    },
                    "-" => match (lhs, rhs) {
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => self
                            .builder
                            .build_float_sub(lhs, rhs, "")
                            .as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                            let rhs =
                                self.builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder
                                .build_float_sub(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                            let lhs =
                                self.builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder
                                .build_float_sub(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                            self.builder.build_int_sub(lhs, rhs, "").as_any_value_enum()
                        }
                        _ => unimplemented!("Operation is not currently supported."),
                    },
                    "*" => match (lhs, rhs) {
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => self
                            .builder
                            .build_float_mul(lhs, rhs, "")
                            .as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                            let rhs =
                                self.builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder
                                .build_float_mul(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                            let lhs =
                                self.builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder
                                .build_float_mul(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                            self.builder.build_int_mul(lhs, rhs, "").as_any_value_enum()
                        }
                        _ => unimplemented!("Operation is not currently supported."),
                    },
                    "/" => match (lhs, rhs) {
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => self
                            .builder
                            .build_float_div(lhs, rhs, "")
                            .as_any_value_enum(),
                        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                            let rhs =
                                self.builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "");
                            self.builder
                                .build_float_div(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                            let lhs =
                                self.builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "");
                            self.builder
                                .build_float_div(lhs, rhs, "")
                                .as_any_value_enum()
                        }
                        (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                            .builder
                            .build_int_signed_div(lhs, rhs, "")
                            .as_any_value_enum(),
                        _ => unimplemented!("Operation is not currently supported."),
                    },
                    _ => unreachable!(),
                }
            }
            TypedExpr::UnaryOpCall { .. } => todo!(),
            TypedExpr::FnCall(TypedFnCall {
                value,
                arg: Some(arg),
                rt,
                arg_t,
                loc,
            }) => {
                let arg_t = self.type_resolver.resolve_arg_type(&arg_t);
                let arg = self.compile_expr(*arg);
                let arg: BasicValueEnum = arg.try_into().unwrap();
                let value = self.compile_expr(*value);
                if let Some(dibuilder) = &self.dibuilder {
                    let loc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        self.difunction.as_ref().unwrap().as_debug_info_scope(),
                        None,
                    );
                    self.builder.set_current_debug_location(loc);
                }
                match value {
                    AnyValueEnum::PointerValue(target) => {
                        match target.get_type().get_element_type() {
                            AnyTypeEnum::StructType(strct_t) => {
                                let target_fun =
                                    self.builder.build_struct_gep(target, 0, "").unwrap();
                                let target_fun = self.builder.build_load(target_fun, "");
                                let ty = if let ResolvedType::Function { .. } = rt {
                                    self.ctx
                                        .struct_type(
                                            &[self
                                                .ctx
                                                .i8_type()
                                                .ptr_type(AddressSpace::default())
                                                .into()],
                                            false,
                                        )
                                        .ptr_type(AddressSpace::default())
                                        .as_basic_type_enum()
                                } else {
                                    self.type_resolver.resolve_type_as_basic(rt)
                                };
                                let ty = ty
                                    .fn_type(
                                        &[
                                            strct_t.ptr_type(AddressSpace::default()).into(),
                                            arg_t.into(),
                                        ],
                                        false,
                                    )
                                    .ptr_type(AddressSpace::default());
                                let target_fun = self
                                    .builder
                                    .build_bitcast(target_fun, ty, "")
                                    .into_pointer_value();
                                let target_fun: CallableValue = target_fun.try_into().unwrap();
                                self.builder
                                    .build_call(target_fun, &[target.into(), arg.into()], "")
                                    .as_any_value_enum()
                            }
                            AnyTypeEnum::PointerType(ptr) => {
                                if !ptr.get_element_type().is_struct_type() {
                                    unreachable!()
                                };
                                let strct_t = ptr.get_element_type().into_struct_type();
                                let target =
                                    self.builder.build_load(target, "").into_pointer_value();
                                let target_fun =
                                    self.builder.build_struct_gep(target, 0, "").unwrap();
                                let target_fun = self.builder.build_load(target_fun, "");
                                let ty = if let ResolvedType::Function { .. } = rt {
                                    self.ctx
                                        .struct_type(
                                            &[self
                                                .ctx
                                                .i8_type()
                                                .ptr_type(AddressSpace::default())
                                                .into()],
                                            false,
                                        )
                                        .ptr_type(AddressSpace::default())
                                        .as_basic_type_enum()
                                } else {
                                    self.type_resolver.resolve_type_as_basic(rt)
                                };
                                let ty = ty
                                    .fn_type(
                                        &[
                                            strct_t.ptr_type(AddressSpace::default()).into(),
                                            arg_t.into(),
                                        ],
                                        false,
                                    )
                                    .ptr_type(AddressSpace::default());
                                let target_fun = self
                                    .builder
                                    .build_bitcast(target_fun, ty, "")
                                    .into_pointer_value();
                                let target_fun: CallableValue = target_fun.try_into().unwrap();
                                self.builder
                                    .build_call(target_fun, &[target.into(), arg.into()], "")
                                    .as_any_value_enum()
                            }
                            AnyTypeEnum::FunctionType(_) => {
                                let target: CallableValue = target.try_into().unwrap();
                                self.builder
                                    .build_call(target, &[arg.into()], "")
                                    .as_any_value_enum()
                            }
                            _ => {
                                #[cfg(debug_assertions)]
                                let _ = self.module.print_to_file("./error.llvm");
                                unreachable!();
                            }
                        }
                    }
                    AnyValueEnum::FunctionValue(target) => {
                        let expect_ty = target.get_type().get_param_types();
                        println!("{:?}\n{:?}", expect_ty, arg_t);
                        self.builder
                            .build_call(target, &[arg.into()], "")
                            .as_any_value_enum()
                    }
                    _ => {
                        #[cfg(debug_assertions)]
                        let _ = self.module.print_to_file("./error.llvm");
                        unreachable!();
                    }
                }
            }
            TypedExpr::FnCall(TypedFnCall { value, loc, .. }) => {
                //this should only ever be a named value?
                if let Some(dibuilder) = &self.dibuilder {
                    let loc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        self.difunction.as_ref().unwrap().as_debug_info_scope(),
                        None,
                    );
                    self.builder.set_current_debug_location(loc);
                }
                let TypedExpr::ValueRead(ident,_) = *value else { unreachable!("not a function name?") };
                let Some(gv)= self.known_functions.get(&ident) else { unreachable!("function not found") };
                let fun = self
                    .builder
                    .build_struct_gep(gv.as_pointer_value(), 0, "")
                    .unwrap();
                let fun: CallableValue = fun.try_into().unwrap();
                self.builder
                    .build_call(fun, &[gv.as_pointer_value().into()], "")
                    .as_any_value_enum()
            }

            TypedExpr::ValueRead(ident, _) => self
                .locals
                .get(&ident)
                .map(|val| self.builder.build_load(*val, "").as_any_value_enum())
                .or(self
                    .known_values
                    .get(&ident)
                    .map(|val| val.as_any_value_enum()))
                .or(self
                    .known_functions
                    .get(&ident)
                    .map(|fun| fun.as_any_value_enum()))
                .unwrap(),
            TypedExpr::UnitLiteral => match self.module.get_global("()") {
                Some(g) => g.as_any_value_enum(),
                None => {
                    let unit = self.ctx.const_struct(&[], false);
                    let gs = self.module.add_global(unit.get_type(), None, "()");
                    gs.set_initializer(&unit);
                    gs.set_constant(true);
                    gs.as_any_value_enum()
                }
            },
            TypedExpr::IntegerLiteral { value, size } => {
                let ty = self
                    .type_resolver
                    .resolve_type_as_basic(ResolvedType::Int {
                        signed: true,
                        width: size,
                    })
                    .into_int_type();
                let v = ty
                    .const_int_from_string(&value, inkwell::types::StringRadix::Decimal)
                    .unwrap();
                v.as_any_value_enum()
            }
            TypedExpr::FloatLiteral { value, size } => {
                let ty = self
                    .type_resolver
                    .resolve_type_as_basic(ResolvedType::Float { width: size })
                    .into_float_type();
                let v = ty.const_float_from_string(&value);
                v.as_any_value_enum()
            }
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
                let ty = self
                    .type_resolver
                    .resolve_type_as_basic(types::STR)
                    .into_struct_type();
                let p = self.builder.build_alloca(ty, "");
                self.builder
                    .build_store(p, ty.const_named_struct(&[ptr.into(), ptr_end.into()]));
                p.as_any_value_enum()
            }
            TypedExpr::CharLiteral(value) => {
                let ty = self
                    .type_resolver
                    .resolve_type_as_basic(types::CHAR)
                    .into_int_type();
                let v = ty.const_int(value.bytes().next().unwrap() as u64, false);
                v.as_any_value_enum()
            }
            TypedExpr::ArrayLiteral { .. } => todo!(),
            TypedExpr::ListLiteral { .. } => todo!(),
            TypedExpr::TupleLiteral { .. } => todo!(),
            TypedExpr::ErrorNode => unreachable!(),
        }
    }

    pub fn compile_decl(&mut self, decl: TypedDeclaration) -> Module<'ctx> {
        match decl {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(data) => {
                self.compile_function(data);
            }
            TypedDeclaration::TypeDefinition() => todo!(),
        }
        if let Some(dibuilder) = &mut self.dibuilder {
            dibuilder.finalize()
        }
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./what.llvm");
        self.module.clone()
    }

    fn create_define(&mut self, decl: &TypedDeclaration) {
        match decl {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(decl) => {
                if decl.ty.is_function() {
                    let fun = self.create_curry_list(decl);
                    self.known_functions.insert(decl.ident.clone(), fun);
                }
            }
            TypedDeclaration::TypeDefinition() => todo!(),
        }
    }

    fn create_curry_list(&mut self, decl: &TypedValueDeclaration) -> GlobalValue<'ctx> {
        let TypedValueDeclaration {
            ident, args, ty, ..
        } = decl;
        let ident = self.current_module.clone() + "$" + ident;
        let curry_placeholder = self
            .ctx
            .struct_type(
                &[self.ctx.i8_type().ptr_type(AddressSpace::default()).into()],
                false,
            )
            .ptr_type(AddressSpace::default());
        let mut result_ty = ty.clone();
        let mut curried_args = Vec::with_capacity(args.len());
        let gs = self.module.add_global(
            self.ctx.struct_type(
                &[self.ctx.i8_type().ptr_type(AddressSpace::default()).into()],
                false,
            ),
            None,
            &ident,
        );
        // generate needed supporting functions.
        let args_curry_functions = args.iter().rev().take(args.len()-1).map(|_| {
            let ResolvedType::Function { arg:arg_t, returns } = result_ty.clone() else { unreachable!() };
            let arg_t = self.type_resolver.resolve_arg_type(&arg_t);
            let fun_t = curry_placeholder.fn_type(&[curry_placeholder.into(),arg_t.into()], false);
            result_ty = match *returns{
                ResolvedType::Pointer { underlining } if matches!(underlining.as_ref(),ResolvedType::Function { .. }) => *underlining,
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
            rt.fn_type(&[curry_placeholder.into(), arg_t.into()], false)
        } else {
            let rt = self.type_resolver.resolve_type_as_basic(*rt);
            let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
            rt.fn_type(&[curry_placeholder.into(), arg_t.into()], false)
        };
        let v = self.module.add_function(&ident, fun_t, None);
        curried_args.insert(
            0,
            self.ctx.i8_type().ptr_type(AddressSpace::default()).into(),
        );
        for (idx, (curr, next)) in args_curry_functions
            .iter()
            .chain(once(&v))
            .tuple_windows()
            .enumerate()
        {
            let bb = self.ctx.append_basic_block(dbg!(*curr), "");
            self.builder.position_at_end(bb);
            let ret_t = self
                .ctx
                .struct_type(dbg!(&curried_args[..=(idx + 1)]), false);
            let ret = self.builder.build_malloc(ret_t, "ret").unwrap();
            let next_fn_ptr = self.builder.build_bitcast(
                next.as_global_value().as_pointer_value(),
                self.ctx.i8_type().ptr_type(AddressSpace::default()),
                "",
            );
            let next_ptr = self.builder.build_struct_gep(ret, 0, "").unwrap();
            self.builder.build_store(next_ptr, next_fn_ptr);
            let expected = self.ctx.struct_type(&curried_args[..=idx], false);
            let first_elem = curr.get_first_param().unwrap();
            let first_elem = self
                .builder
                .build_bitcast(
                    first_elem,
                    expected.ptr_type(AddressSpace::default()),
                    "curried",
                )
                .into_pointer_value();

            //copy the elements from old to new
            for idx in 0..idx {
                let element = self
                    .builder
                    .build_struct_gep(first_elem, idx as u32 + 1, "")
                    .unwrap();
                let element = self.builder.build_load(element, "");
                let target = self
                    .builder
                    .build_struct_gep(ret, idx as u32 + 1, "")
                    .unwrap();
                self.builder.build_store(target, element);
            }
            let target = self
                .builder
                .build_struct_gep(ret, idx as u32 + 1, "")
                .unwrap();
            self.builder
                .build_store(target, curr.get_last_param().unwrap());
            let ret = self.builder.build_bitcast(ret, curry_placeholder, "");
            self.builder.build_return(Some(&ret));
        }
        self.incomplete_functions.insert(decl.ident.clone(), v);
        gs.set_constant(true);
        let cs = self.ctx.const_struct(
            &[args_curry_functions
                .first()
                .unwrap_or(&v)
                .as_global_value()
                .as_pointer_value()
                .const_cast(self.ctx.i8_type().ptr_type(AddressSpace::default()))
                .into()],
            false,
        );
        gs.set_initializer(&cs);
        gs
    }

    pub fn compile_module(
        &mut self,
        ast: crate::typed_ast::TypedModuleDeclaration,
    ) -> Module<'ctx> {
        if self.dibuilder.is_some() {
            let debug_metadata_version = self.ctx.i32_type().const_int(3, false);
            self.module.add_basic_value_flag(
                "Debug Info Version",
                inkwell::module::FlagBehavior::Warning,
                debug_metadata_version,
            )
        }
        for decl in &ast.declarations {
            self.create_define(decl);
        }

        for decl in ast.declarations {
            self.compile_decl(decl);
        }
        if let Some(dibuilder) = &self.dibuilder {
            dibuilder.finalize()
        }
        self.module.clone()
    }

    pub fn compile_program(
        mut self,
        ast: crate::typed_ast::ProgramTyped,
        is_lib: bool,
        is_debug: bool,
    ) -> Module<'ctx> {
        use crate::util::ExtraUtilFunctions;
        let main_name = ast.iter().find_map(|file| {
            file.declarations
                .iter()
                .find(|decl| {
                    if let TypedDeclaration::Value(decl) = decl {
                        decl.ident == "main"
                            && decl.ty
                                == ResolvedType::Function {
                                    arg: types::UNIT.boxed(),
                                    returns: types::UNIT.boxed(),
                                }
                    } else {
                        false
                    }
                })
                .map(|_| file.name.clone().unwrap_or_default() + "$main")
        });

        if is_debug {
            let (dibulder, compile_unit) = self.module.create_debug_info_builder(
                true,
                DWARFSourceLanguage::Haskell,
                if self.current_module.is_empty() {
                    "unkown.fb"
                } else {
                    &self.current_module
                },
                ".",
                "FBC",
                false,
                "",
                1,
                "",
                inkwell::debug_info::DWARFEmissionKind::Full,
                0,
                false,
                false,
                "",
                "",
            );

            self.dibuilder = Some(dibulder);
            self.compile_unit = Some(compile_unit);
        }

        for file in ast {
            self.current_module = file.name.clone().unwrap_or("unknown.fb".to_string());
            if is_debug {
                let Some(dibuilder) = &self.dibuilder else { unreachable!() };
                let difile = dibuilder.create_file(file.name.as_ref().unwrap(), "");
                self.difile = Some(difile);
            }
            self.compile_module(file);
            self.difile = None
        }

        if !is_lib {
            if let Some(main_name) = main_name {
                let entry = self.module.add_function(
                    "main",
                    self.ctx.void_type().fn_type(&[], false),
                    None,
                );
                let bb = self.ctx.append_basic_block(entry, "");
                self.builder.position_at_end(bb);
                let gs = self.module.get_global(&main_name).unwrap();
                let main = self
                    .builder
                    .build_struct_gep(gs.as_pointer_value(), 0, "")
                    .unwrap();
                let main = self.builder.build_load(main, "").into_pointer_value();
                let main = self
                    .builder
                    .build_bitcast(
                        main,
                        self.ctx
                            .void_type()
                            .fn_type(&[self.ctx.struct_type(&[], false).into()], false)
                            .ptr_type(AddressSpace::default()),
                        "",
                    )
                    .into_pointer_value();
                let main: CallableValue = main.try_into().unwrap();
                self.builder
                    .build_call(main, &[self.ctx.const_struct(&[], false).into()], "");
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
        AnyValueEnum::InstructionValue(_)
        | AnyValueEnum::MetadataValue(_)
        | AnyValueEnum::PhiValue(_) => unimplemented!(),
    }
}

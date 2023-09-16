use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;

use inkwell::attributes::Attribute;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DIFlags, DIFlagsConstants, DILocalVariable, DISubprogram,
    DIType, DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{AnyTypeEnum, BasicType};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, CallableValue, FunctionValue, GlobalValue,
    PointerValue,
};
use inkwell::AddressSpace;

use itertools::Itertools;

use multimap::MultiMap;

use crate::typed_ast::{
    collect_args, ResolvedTypeDeclaration, StructDefinition, TypedBinaryOpCall, TypedDeclaration,
    TypedExpr, TypedFnCall, TypedIfBranching, TypedIfExpr, TypedMemberRead, TypedStatement,
    TypedValueDeclaration, TypedValueType,
};
use crate::types::{self, ResolvedType, TypeResolver};

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    type_resolver: TypeResolver<'ctx>,
    known_functions: HashMap<String, GlobalValue<'ctx>>,
    known_types: HashMap<String, ResolvedTypeDeclaration>,
    incomplete_functions: HashMap<String, FunctionValue<'ctx>>, //this should be the ones left to compile.  sometimes same as above
    _known_ops: MultiMap<String, FunctionValue<'ctx>>,
    known_values: HashMap<String, BasicValueEnum<'ctx>>,
    locals: HashMap<String, PointerValue<'ctx>>,
    current_module: String,
    target_info: TargetData,
    ret_target: Option<PointerValue<'ctx>>,
    // debug info starts here
    dibuilder: Option<DebugInfoBuilder<'ctx>>,
    compile_unit: Option<DICompileUnit<'ctx>>,
    difile: Option<DIFile<'ctx>>,
    difunction: Option<DISubprogram<'ctx>>,
    dilocals: HashMap<String, DILocalVariable<'ctx>>, // curried_locals : HashMap<String, (PointerValue<'ctx>,ResolvedType)>,
    ditypes: HashMap<String, DIType<'ctx>>,
    needsdi: Vec<ResolvedTypeDeclaration>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn with_module(
        ctx: &'ctx Context,
        module: Module<'ctx>,
        type_resolver: TypeResolver<'ctx>,
        seed_functions: HashMap<String, ResolvedType>,
        known_values: HashMap<String, BasicValueEnum<'ctx>>,
        seed_ops: MultiMap<String, FunctionValue<'ctx>>,
        target_info: TargetData,
    ) -> Self {
        let builder = ctx.create_builder();
        let known_functions = seed_functions
            .into_iter()
            .map(|(name, _)| {
                let ty = module.get_global(&name).unwrap_or_else(|| {
                    module.add_global(
                        ctx.struct_type(
                            &[ctx.i8_type().ptr_type(AddressSpace::default()).into()],
                            false,
                        ),
                        None,
                        &name,
                    )
                });
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
            known_types: HashMap::new(),
            _known_ops: seed_ops,
            locals: HashMap::new(),
            ret_target: None,
            current_module: String::new(),
            dibuilder: None,
            compile_unit: None,
            difile: None,
            difunction: None,
            dilocals: HashMap::new(), // curried_locals : HashMap::new(),
            target_info,
            ditypes: HashMap::new(),
            needsdi: Vec::new(),
        }
    }

    fn compile_function(&mut self, decl: TypedValueDeclaration) {
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.llvm");
        let v = self.incomplete_functions.get(dbg!(&decl.ident)).unwrap().clone();
        
        if let Some(dibuilder) = &self.dibuilder {
            let Some(difile) = self.difile.as_ref() else { unreachable!() };
            let fnty = {
                let (args, rt) = decl.ty.as_c_function();
                #[allow(non_snake_case)]
                let FUNCTION_NAME = "<Function>".to_string();
                let rt_name = if rt.is_function() {
                    FUNCTION_NAME.clone()
                } else {
                    rt.to_string()
                };
                let rtdi = self
                    .ditypes
                    .entry(rt_name)
                    .or_insert_with(|| unsafe {
                        dibuilder
                            .create_placeholder_derived_type(self.ctx)
                            .as_type()
                    })
                    .clone();
                let args = args
                    .into_iter()
                    .map(|it| {
                        let it_name = it.to_string();
                        self.ditypes[if it.is_function() {
                            &FUNCTION_NAME
                        } else {
                            &it_name
                        }]
                    })
                    .collect_vec();
                dibuilder.create_subroutine_type(
                    *difile,
                    if rt.is_void_or_unit() {
                        None
                    } else {
                        Some(rtdi)
                    },
                    &args,
                    DIFlags::PUBLIC,
                )
            };
            let fun_scope = dibuilder.create_function(
                difile.as_debug_info_scope(),
                &decl.ident,
                Some(&decl.ident),
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
        let args_block = self.ctx.append_basic_block(v, "arg_declarations");
        self.builder.position_at_end(args_block);
        let rt = decl.ty.remove_args(decl.args.len());
        let ret_value = if rt.is_void_or_unit() {
            None
        } else if rt.is_user() {
            Some(v.get_nth_param(1).unwrap().into_pointer_value())
        } else {
            Some(
                self.builder
                    .build_alloca(self.type_resolver.resolve_type_as_basic(rt.clone()), ""),
            )
        };
        self.ret_target = ret_value;
        let arg_ts = collect_args(&decl.ty);
        let arg_c = arg_ts.len(); 
        let curried_args = arg_ts 
            .into_iter()
            .take(arg_c - 1)
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
                let ty = self.ditypes[&ty.to_string()];
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
                dibuilder.insert_declare_at_end(arg, Some(local), None, diloc, args_block);
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
            let ty_name = if ty.is_function() {
                "<Function>".to_string()
            } else {
                ty.to_string()
            };
            let ty = self.ditypes[&ty_name];
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
            dibuilder.insert_declare_at_end(arg, Some(local), None, diloc, args_block);
            self.dilocals.insert(last_param_info.ident.clone(), local);
        }

        let ret_block = self.ctx.append_basic_block(v, ""); //this is what will be used to return
        self.builder.position_at_end(ret_block);
        if rt.is_void_or_unit() || rt.is_user() {
            self.builder.build_return(None);
        } else {
            let ret_value = self.builder.build_load(ret_value.unwrap(), "");
            self.builder.build_return(Some(&ret_value));
        }

        self.builder.position_at_end(args_block);
        let bb = self.ctx.append_basic_block(v, "start");
        self.builder.build_unconditional_branch(bb);
        self.builder.position_at_end(bb);
        self.locals
            .insert(last_param_info.ident.clone(), arg.into());
        match decl.value {
            TypedValueType::Expr(expr) => {
                let rt = expr.get_ty();
                let value = self.compile_expr(expr);
                let value: BasicValueEnum<'ctx> = value.try_into().unwrap();
                let ret_bb = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap()
                    .get_basic_blocks()[1];
                if let Some(ret_target) = self.ret_target.as_ref() {
                    let value = if rt.is_user() || value.is_pointer_value() {
                        self.builder.build_load(value.into_pointer_value(), "")
                    } else {
                        value
                    };
                    self.builder.build_store(*ret_target, value);
                }
                self.builder.build_unconditional_branch(ret_bb);
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
        let _ = ret_block.move_after(v.get_last_basic_block().unwrap());
    }

    pub fn compile_statement(&mut self, stmnt: TypedStatement) {
        match stmnt {
            TypedStatement::IfBranching(ifbranch) => {
                let TypedIfBranching {
                    cond,
                    true_branch,
                    else_ifs,
                    else_branch,
                } = ifbranch;
                let fun = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let true_block = self.ctx.append_basic_block(fun, "");
                let end_block = self.ctx.append_basic_block(fun, "");
                let cond = match self.compile_expr(*cond) {
                    AnyValueEnum::PointerValue(ptr) => self.builder.build_load(ptr,"").into_int_value(),
                    AnyValueEnum::IntValue(it) => it,
                    _=> unreachable!("it can only ever be a point which means a value read or an expression resulting in a boolean")
                };

                match (else_ifs.is_empty(), else_branch.is_empty()) {
                    (true, true) => {
                        // if then
                        self.builder
                            .build_conditional_branch(cond, true_block, end_block);
                        self.builder.position_at_end(true_block);
                        for stmnt in true_branch {
                            self.compile_statement(stmnt);
                        }
                        self.builder.build_unconditional_branch(end_block);
                    }
                    (true, false) => {
                        //if then else
                        let else_block = self.ctx.append_basic_block(fun, "");
                        self.builder
                            .build_conditional_branch(cond, true_block, else_block);
                        self.builder.position_at_end(true_block);
                        for stmnt in true_branch {
                            self.compile_statement(stmnt);
                        }
                        self.builder.build_unconditional_branch(end_block);
                        self.builder.position_at_end(else_block);
                        for stmnt in else_branch {
                            self.compile_statement(stmnt);
                        }
                        let _ = end_block.move_after(else_block);
                        self.builder.build_unconditional_branch(end_block);
                    }
                    (false, true) => {
                        //if then else if then
                        let cond_blocks = std::iter::repeat_with(|| self.ctx.append_basic_block(fun, ""))
                                .take(else_ifs.len())
                                .collect_vec();

                        self.builder.build_conditional_branch(
                            cond, 
                            true_block, 
                            *cond_blocks.first().unwrap()
                        );
                        self.builder.position_at_end(true_block);
                        for stmnt in true_branch {
                            self.compile_statement(stmnt);
                        }
                        self.builder.position_at_end(true_block);
                        
                        let else_ifs = else_ifs
                            .into_iter()
                            .map(|(cond,stmnts)| {
                                let block = self.ctx.append_basic_block(fun, "");
                                self.builder.position_at_end(block);
                                for stmnt in stmnts {
                                    self.compile_statement(stmnt);
                                }
                                self.builder.build_unconditional_branch(end_block);
                                (cond,block)
                            })
                            .zip(cond_blocks.iter().copied())
                            .map(|((cond,block),cond_block)|{
                                let _ = block.move_after(cond_block);
                                (cond,block,cond_block)
                            }).collect_vec();
                        let blocks = else_ifs.into_iter()
                            .zip(cond_blocks.into_iter().skip(1).chain(once(end_block)))
                            .map(|(it,false_block)|(it.0,it.1,it.2,false_block))
                            .collect_vec();
                        let _ = end_block.move_after(blocks.last().map(|it| it.1).unwrap());
                        for (cond,true_block,cond_block,false_block) in blocks {
                            self.builder.position_at_end(cond_block);
                            let cond = match self.compile_expr(*cond) {
                                AnyValueEnum::PointerValue(p) => {
                                    self.builder.build_load(p, "").into_int_value()
                                }
                                AnyValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            };
                            self.builder.build_conditional_branch(cond, true_block, false_block);
                        }
                    }
                    (false, false) => {
                        
                        let cond_blocks = std::iter::repeat_with(|| self.ctx.append_basic_block(fun, ""))
                                .take(else_ifs.len())
                                .collect_vec();

                        self.builder.build_conditional_branch(
                            cond, 
                            true_block, 
                            *cond_blocks.first().unwrap()
                        );
                        self.builder.position_at_end(true_block);
                        for stmnt in true_branch {
                            self.compile_statement(stmnt);
                        }
                        self.builder.build_unconditional_branch(end_block);
                        let else_block = self.ctx.append_basic_block(fun, "");
                        let else_ifs = else_ifs
                            .into_iter()
                            .map(|(cond,stmnts)| {
                                let block = self.ctx.append_basic_block(fun, "");
                                self.builder.position_at_end(block);
                                for stmnt in stmnts {
                                    self.compile_statement(stmnt);
                                }
                                self.builder.build_unconditional_branch(end_block);
                                (cond,block)
                            })
                            .zip(cond_blocks.iter().copied())
                            .map(|((cond,block),cond_block)|{
                                let _ = block.move_after(cond_block);
                                (cond,block,cond_block)
                            }).collect_vec();
                        let blocks = else_ifs.into_iter()
                            .zip(cond_blocks.into_iter().skip(1).chain(once(else_block)))
                            .map(|(it,false_block)|(it.0,it.1,it.2,false_block))
                            .collect_vec();
                        let _ = else_block.move_after(blocks.last().map(|it| it.2).unwrap());
                        for (cond,true_block,cond_block,false_block) in blocks {
                            self.builder.position_at_end(cond_block);
                            let cond = match self.compile_expr(*cond) {
                                AnyValueEnum::PointerValue(p) => {
                                    self.builder.build_load(p, "").into_int_value()
                                }
                                AnyValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            };
                            self.builder.build_conditional_branch(cond, true_block, false_block);
                        }
                        self.builder.position_at_end(else_block);
                        for stmnt in else_branch {
                            self.compile_statement(stmnt);
                        }
                        self.builder.build_unconditional_branch(end_block);
                        let _ = end_block.move_after(else_block);
                    }
                }
                #[cfg(debug_assertions)]
                let _ = self.module.print_to_file("./debug.llvm");
                self.builder.position_at_end(end_block);
            }
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
                let ret_bb = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap()
                    .get_basic_blocks()[1];
                if let TypedExpr::UnitLiteral = expr {
                    self.builder.build_unconditional_branch(ret_bb);
                } else {
                    let value = self.compile_expr(expr);
                    let value: BasicValueEnum<'ctx> = value.try_into().unwrap();
                    if let Some(ret_target) = self.ret_target.as_ref() {
                        self.builder.build_store(*ret_target, value);
                    }
                    self.builder.build_unconditional_branch(ret_bb);
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
                    let rty = self.type_resolver.resolve_type_as_basic(ty.clone());
                    let pvalue = self.builder.build_alloca(rty, &ident);
                    let result = self.compile_expr(expr);
                    let result = if ty.is_user() && result.is_pointer_value() {
                        self.builder
                            .build_load(result.into_pointer_value(), "")
                            .as_any_value_enum()
                    } else {
                        result
                    };
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
                            self.ditypes[&ty.to_string()],
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
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.llvm");
        match expr {
            TypedExpr::BoolLiteral(value, _loc) =>{
                if value {
                    self.ctx.bool_type().const_int(1, false).as_any_value_enum()
                } else {
                    self.ctx.bool_type().const_zero().as_any_value_enum()
                }
            }
            TypedExpr::IfExpr(expr) => {
                let rt = expr.get_ty();
                let ty = self.type_resolver.resolve_type_as_basic(expr.get_ty());
                let TypedIfExpr {
                    cond,
                    true_branch,
                    else_ifs,
                    else_branch,
                    loc,
                } = expr;
                let fun = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let root_cond = match self.compile_expr(*cond) {
                    AnyValueEnum::PointerValue(p) => {
                        self.builder.build_load(p, "").into_int_value()
                    }
                    AnyValueEnum::IntValue(i) => i,
                    _ => unreachable!(),
                };
                let result_block = self.ctx.append_basic_block(fun, "");
                let then_block = self.ctx.append_basic_block(fun, "");
                let else_block = self.ctx.append_basic_block(fun, "");
                if else_ifs.is_empty() {
                    self.builder
                        .build_conditional_branch(root_cond, then_block, else_block);
                    self.builder.position_at_end(then_block);
                    for stmnt in true_branch.0 {
                        self.compile_statement(stmnt);
                    }
                    let true_value = convert_to_basic_value(self.compile_expr(*true_branch.1));
                    let true_value = if !rt.is_user() && true_value.is_pointer_value() {
                        self.builder.build_load(true_value.into_pointer_value(), "")
                    } else {
                        true_value
                    };
                    self.builder.position_at_end(else_block);
                    for stmnt in else_branch.0 {
                        self.compile_statement(stmnt);
                    }
                    let else_value = convert_to_basic_value(self.compile_expr(*else_branch.1));
                    let else_value = if !rt.is_user() && else_value.is_pointer_value() {
                        self.builder.build_load(else_value.into_pointer_value(), "")
                    } else {
                        else_value
                    };
                    self.builder.position_at_end(result_block);
                    let phi = self.builder.build_phi(ty, "");
                    phi.add_incoming(&[(&true_value, then_block), (&else_value, else_block)]);
                    let _ = result_block.move_after(else_block);
                    phi.as_any_value_enum()
                } else {
                    let cond_blocks =
                        std::iter::repeat_with(|| self.ctx.append_basic_block(fun, ""))
                            .take(else_ifs.len())
                            .collect_vec();
                    self.builder.build_conditional_branch(
                        root_cond,
                        then_block,
                        *cond_blocks.first().unwrap(),
                    );
                    self.builder.position_at_end(result_block);
                    let phi = self.builder.build_phi(ty, "");
                    self.builder.position_at_end(then_block);
                    for stmnt in true_branch.0 {
                        self.compile_statement(stmnt);
                    }
                    let true_value = convert_to_basic_value(self.compile_expr(*true_branch.1));
                    let true_value = if !rt.is_user() && true_value.is_pointer_value() {
                        self.builder.build_load(true_value.into_pointer_value(), "")
                    } else {
                        true_value
                    };
                    self.builder.build_unconditional_branch(result_block);
                    phi.add_incoming(&[(&true_value, then_block)]);
                    let else_ifs = else_ifs
                        .into_iter()
                        .map(|(cond, stmnts, result)| {
                            let block = self.ctx.append_basic_block(fun, "");
                            self.builder.position_at_end(block);
                            for stmnt in stmnts {
                                self.compile_statement(stmnt);
                            }
                            let result = convert_to_basic_value(self.compile_expr(*result));
                            let result = if !rt.is_user() && result.is_pointer_value() {
                                self.builder.build_load(result.into_pointer_value(), "")
                            } else {
                                result
                            };
                            phi.add_incoming(&[(
                                &result,
                                self.builder.get_insert_block().unwrap(),
                            )]);
                            self.builder.build_unconditional_branch(result_block);
                            (cond, block)
                        })
                        .zip(cond_blocks.iter().copied())
                        .map(|((cond, block), cond_block)| {
                            let _ = block.move_after(cond_block);
                            (cond, block, cond_block)
                        })
                        .collect_vec();
                    let _ = else_block.move_after(else_ifs.last().unwrap().1);
                    for ((cond, true_block, cond_block), false_block) in else_ifs
                        .into_iter()
                        .zip(cond_blocks.into_iter().skip(1).chain(once(else_block)))
                    {
                        self.builder.position_at_end(cond_block);
                        let cond = match self.compile_expr(*cond) {
                            AnyValueEnum::PointerValue(p) => {
                                self.builder.build_load(p, "").into_int_value()
                            }
                            AnyValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        };
                        self.builder
                            .build_conditional_branch(cond, true_block, false_block);
                    }
                    let _ = result_block.move_after(else_block);
                    self.builder.position_at_end(else_block);
                    for stmnt in else_branch.0 {
                        self.compile_statement(stmnt);
                    }
                    let else_value = convert_to_basic_value(self.compile_expr(*else_branch.1));
                    self.builder.build_unconditional_branch(result_block);
                    let else_value = if !rt.is_user() && else_value.is_pointer_value() {
                        self.builder.build_load(else_value.into_pointer_value(), "")
                    } else {
                        else_value
                    };
                    phi.add_incoming(&[(&else_value, self.builder.get_insert_block().unwrap())]);
                    self.builder.position_at_end(result_block);
                    phi.as_any_value_enum()
                }
            }
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
                    "==" => {
                        todo!("equality");
                    }
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
                let arg = if arg.is_pointer_value()
                    && !arg
                        .into_pointer_value()
                        .get_type()
                        .get_element_type()
                        .is_struct_type()
                {
                    //there has to be a better way to do this.
                    self.builder.build_load(arg.into_pointer_value(), "")
                } else {
                    arg
                };
                let value_t = value.get_ty();
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
                            AnyTypeEnum::StructType(_) => {
                                let target_fun =
                                    self.builder.build_struct_gep(target, 0, "").unwrap();
                                let target_fun = self.builder.build_load(target_fun, "");
                                let ty = self
                                    .type_resolver
                                    .resolve_type_as_function(&value_t)
                                    .ptr_type(AddressSpace::default());
                                let target_fun = self
                                    .builder
                                    .build_bitcast(target_fun, ty, "")
                                    .into_pointer_value();
                                let target_fun: CallableValue = target_fun.try_into().unwrap();
                                if rt.is_user() {
                                    let result = self.builder.build_alloca(
                                        self.type_resolver.resolve_type_as_basic(rt),
                                        "",
                                    );
                                    self.builder.build_call(
                                        target_fun,
                                        &[target.into(), result.into(), arg.into()],
                                        "",
                                    );
                                    result.as_any_value_enum()
                                } else {
                                    self.builder
                                        .build_call(target_fun, &[target.into(), arg.into()], "")
                                        .as_any_value_enum()
                                }
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
                let TypedExpr::ValueRead(ident,_, _) = *value else { unreachable!("not a function name?") };
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

            TypedExpr::ValueRead(ident, _, _) => self
                .locals
                .get(&ident)
                .map(|val| val.as_any_value_enum())
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
            TypedExpr::StructConstruction(con) => {
                let target_t = self.ctx.get_struct_type(&con.ident).unwrap();
                let out = self.builder.build_alloca(target_t, "");

                let ResolvedTypeDeclaration::Struct(def) = self.known_types.get(&con.ident).unwrap().clone() else { unreachable!() };
                let order = con.fields.into_iter().map(|(field, expr)| {
                    (
                        expr,
                        def.fields
                            .iter()
                            .find_position(|it| &it.name == &field)
                            .unwrap()
                            .0,
                    )
                });
                for ((value, loc), offest) in order {
                    let target_gep = self
                        .builder
                        .build_struct_gep(out, offest as u32, "")
                        .unwrap();
                    if let Some(dibuilder) = &self.dibuilder {
                        if let Some(scope) = &self.difunction {
                            dibuilder.create_debug_location(
                                self.ctx,
                                loc.0 as u32,
                                loc.1 as u32,
                                scope.as_debug_info_scope(),
                                None,
                            );
                        }
                    }
                    let result = convert_to_basic_value(self.compile_expr(value));
                    let result = if result.is_pointer_value()
                        && !result
                            .get_type()
                            .into_pointer_type()
                            .get_element_type()
                            .is_struct_type()
                    {
                        self.builder.build_load(result.into_pointer_value(), "")
                    } else {
                        result
                    };
                    self.builder.build_store(target_gep, result);
                }
                out.as_any_value_enum()
            }
            TypedExpr::MemeberRead(read) => {
                #[allow(unused)] //all fields will be needed when member functions are added
                let TypedMemberRead {
                    target,
                    member,
                    offset,
                    ty,
                    loc,
                } = read;
                let target_result = self.compile_expr(*target);
                if let Some(offset) = offset {
                    let gep = self
                        .builder
                        .build_struct_gep((target_result).into_pointer_value(), offset as _, "")
                        .unwrap();
                    self.builder.build_load(gep, "").as_any_value_enum()
                } else {
                    todo!("member functions")
                }
            }
            TypedExpr::ArrayLiteral { .. } => todo!(),
            TypedExpr::ListLiteral { .. } => todo!(),
            TypedExpr::TupleLiteral { .. } => todo!(),
            TypedExpr::ErrorNode => unreachable!(),
        }
    }

    fn add_struct_di(&mut self, def: &StructDefinition) -> bool {
        let Some(dibuilder) = &self.dibuilder else {unreachable!()};
        let Some(file) = &self.difile else {unreachable!()};
        let fields = def
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.ty.clone(), field.loc))
            .collect_vec();
        if fields
            .iter()
            .any(|(_, field, _)| !self.ditypes.contains_key(&field.to_string()))
        {
            return false;
        }
        let strct = self.ctx.get_struct_type(&def.ident).unwrap();
        let info: Vec<(DIType<'ctx>, u64)> =
            fields.into_iter().fold(Vec::new(), |mut out, field| {
                let ditype = self.ditypes[&field.1.to_string()];
                let last = out.last().map_or(0, |(_, it)| *it);
                let size = ditype.get_size_in_bits();
                let ty = dibuilder.create_member_type(
                    file.as_debug_info_scope(),
                    &field.0,
                    *file,
                    field.2 .0 as u32,
                    size,
                    ditype.get_align_in_bits(),
                    last,
                    DIFlags::PUBLIC,
                    ditype,
                );
                out.push((ty.as_type(), last + size));
                out
            });
        let size = 0;
        let Some(discope) = &self.difile else {unreachable!()};
        let difields = info.into_iter().map(|(it, _)| it).collect_vec();

        let di_struct = dibuilder.create_struct_type(
            discope.as_debug_info_scope(),
            &def.ident,
            *discope,
            def.loc.0 as u32,
            size,
            self.target_info.get_preferred_alignment(&strct),
            DIFlags::PUBLIC,
            None,
            &difields,
            0,
            None,
            "",
        );
        if let Some(di_placeholder) = self.ditypes.get(&def.ident) {
            todo!("replace the placeholder somehow");
        } else {
            self.ditypes.insert(def.ident.clone(), di_struct.as_type());
        }
        true
    }
    pub fn compile_decl(&mut self, decl: TypedDeclaration) -> Module<'ctx> {
        match decl {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(data) => {
                self.compile_function(data);
            }
            TypedDeclaration::TypeDefinition(def) => match def {
                crate::typed_ast::ResolvedTypeDeclaration::Struct(def) => {
                    if !def.generics.is_empty() {
                        return self.module.clone();
                    }
                    let strct = self.ctx.get_struct_type(&def.ident).unwrap();
                    self.known_types.insert(
                        def.ident.clone(),
                        ResolvedTypeDeclaration::Struct(def.clone()),
                    );
                    let fields = def
                        .fields
                        .iter()
                        .map(|field| {
                            (
                                field.name.clone(),
                                self.type_resolver.resolve_type_as_basic(field.ty.clone()),
                            )
                        })
                        .collect_vec();
                    let fields_no_name = fields.iter().map(|(_, it)| it.clone()).collect_vec();
                    strct.set_body(&fields_no_name, false);
                    if let Some(dibuilder) = &self.dibuilder {
                        if !self.add_struct_di(&def) {
                            self.needsdi.push(ResolvedTypeDeclaration::Struct(def));
                        }
                    }
                }
            },
        }
        if let Some(dibuilder) = &mut self.dibuilder {
            dibuilder.finalize()
        }
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.llvm");
        self.module.clone()
    }

    fn create_define(&mut self, decl: &TypedDeclaration) {
        match decl {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(decl) => {
                if decl.ty.is_function() {
                    let fun = self.create_curry_list(decl);
                    self.known_functions.insert(dbg!(decl.ident.clone()), fun);
                }
            }
            TypedDeclaration::TypeDefinition(def) => match def {
                crate::typed_ast::ResolvedTypeDeclaration::Struct(decl) => {
                    if decl.generics.len() != 0 {
                        return;
                    }
                    let _strct = self.ctx.opaque_struct_type(&decl.ident);
                }
            },
        }
    }

    fn create_curry_list(&mut self, decl: &TypedValueDeclaration) -> GlobalValue<'ctx> {
        let TypedValueDeclaration {
            ident, args, ty, ..
        } = decl;
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
            let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
            if rt.is_user() {
                let rt = self
                    .type_resolver
                    .resolve_type_as_basic(rt.as_ref().clone())
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum();
                self.ctx
                    .void_type()
                    .fn_type(&[curry_placeholder.into(), rt.into(), arg_t.into()], false)
            } else {
                let rt = self
                    .type_resolver
                    .resolve_type_as_basic(rt.as_ref().clone());
                rt.fn_type(&[curry_placeholder.into(), arg_t.into()], false)
            }
        };
        let v = self.module.add_function(&ident, fun_t, None);
        if rt.is_user() {
            let sret_id = Attribute::get_named_enum_kind_id("sret");
            let attr = self
                .ctx
                .create_type_attribute(sret_id, self.type_resolver.resolve_type_as_any(*rt));
            v.add_attribute(inkwell::attributes::AttributeLoc::Param(1), attr);
        }
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
            let bb = self.ctx.append_basic_block(*curr, "");
            self.builder.position_at_end(bb);
            let ret_t = self.ctx.struct_type(&curried_args[..=(idx + 1)], false);
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
        mut ast: crate::typed_ast::TypedModuleDeclaration,
    ) -> Module<'ctx> {
        if self.dibuilder.is_some() {
            let debug_metadata_version = self.ctx.i32_type().const_int(3, false);
            self.module.add_basic_value_flag(
                "Debug Info Version",
                inkwell::module::FlagBehavior::Warning,
                debug_metadata_version,
            )
        }

        ast.declarations.sort_by(|a, b| match (a, b) {
            (TypedDeclaration::Value(_), TypedDeclaration::Value(_)) => std::cmp::Ordering::Equal,
            (_, TypedDeclaration::Value(_)) => std::cmp::Ordering::Less,
            (TypedDeclaration::Value(_), _) => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        });

        for decl in &ast.declarations {
            self.create_define(decl);
        }
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.llvm");
        for decl in ast.declarations {
            self.compile_decl(decl);
        }
        let mut prev_len = self.needsdi.len();
        while self.needsdi.len() > 0 {
            let mut to_remove = Vec::new();
            for (idx, def) in self.needsdi.clone().iter().enumerate() {
                let result = match def {
                    ResolvedTypeDeclaration::Struct(def) => self.add_struct_di(def),
                    _ => unreachable!(),
                };
                if result {
                    to_remove.push(idx);
                }
            }
            for (offset, to_remove) in to_remove.into_iter().enumerate() {
                self.needsdi.remove(to_remove + offset);
            }
            if prev_len == self.needsdi.len() {
                panic!("di is impossible")
            }
            prev_len = self.needsdi.len();
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
            file.declarations.iter().find_map(|decl| {
                if let TypedDeclaration::Value(decl) = decl {
                    if (decl.ident.ends_with("::main")
                        && decl.ty
                            == ResolvedType::Function {
                                arg: types::UNIT.boxed(),
                                returns: types::UNIT.boxed(),
                            })
                    {
                        Some(decl.ident.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
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
            let difile = dibulder.create_file("builtin", "");
            let ptr_t = self.ctx.i8_type().ptr_type(AddressSpace::default());
            let ptr_size = self.target_info.get_bit_size(&ptr_t);
            let ptr_align = self.target_info.get_preferred_alignment(&ptr_t);
            let curry_t = self.ctx.struct_type(
                &[self.ctx.i8_type().ptr_type(AddressSpace::default()).into()],
                false,
            );
            let curry_t_size = self.target_info.get_bit_size(&curry_t);
            let curry_t_align = self.target_info.get_preferred_alignment(&curry_t);
            let int8_di = dibulder
                .create_basic_type("int8", 8, 0, DIFlags::PUBLIC)
                .unwrap();
            let int8_ptr_di = dibulder.create_pointer_type(
                "<NEXT_PTR>",
                int8_di.as_type(),
                8,
                0,
                AddressSpace::default(),
            );
            self.ditypes = [
                ("int8", int8_di.as_type()),
                (
                    "int16",
                    dibulder
                        .create_basic_type("int16", 16, 0, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type(),
                ),
                (
                    "int32",
                    dibulder
                        .create_basic_type("int32", 32, 0, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type(),
                ),
                (
                    "int64",
                    dibulder
                        .create_basic_type("int64", 64, 0, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type(),
                ),
                (
                    "float32",
                    dibulder
                        .create_basic_type("float32", 32, 0, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type(),
                ),
                (
                    "float64",
                    dibulder
                        .create_basic_type("float64", 64, 0, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type(),
                ),
                ("<Function>", {
                    let pointee = dibulder.create_struct_type(
                        compile_unit.as_debug_info_scope(),
                        "<Function_internal>",
                        difile,
                        0,
                        curry_t_size,
                        curry_t_align,
                        DIFlags::PUBLIC,
                        None,
                        &[int8_ptr_di.as_type()],
                        0,
                        None,
                        "",
                    );
                    dibulder
                        .create_pointer_type(
                            "<Function>",
                            pointee.as_type(),
                            ptr_size,
                            ptr_align,
                            AddressSpace::default(),
                        )
                        .as_type()
                }),
            ]
            .into_iter()
            .map(|(a, b)| (a.to_string(), b))
            .collect();
            self.dibuilder = Some(dibulder);
            self.compile_unit = Some(compile_unit);
        }

        for file in ast {
            self.current_module = file.name.clone() + ".fb";
            if is_debug {
                let Some(dibuilder) = &self.dibuilder else { unreachable!() };
                let difile = dibuilder.create_file(&file.name, "");
                self.difile = Some(difile);
            }
            self.compile_module(file);
            self.difile = None
        }

        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.llvm");
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
                            .fn_type(
                                &[
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
                                        .into(),
                                    self.ctx.struct_type(&[], false).into(),
                                ],
                                false,
                            )
                            .ptr_type(AddressSpace::default()),
                        "",
                    )
                    .into_pointer_value();
                let main: CallableValue = main.try_into().unwrap();
                self.builder.build_call(
                    main,
                    &[
                        gs.as_basic_value_enum().into(),
                        self.ctx.const_struct(&[], false).into(),
                    ],
                    "",
                );
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

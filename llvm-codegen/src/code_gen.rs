use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;

use compiler::ast::EnumVariant;
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
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, PointerType, StructType};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue, IntValue,
    PhiValue, PointerValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use itertools::Itertools;

use crate::type_resolver::TypeResolver;
use compiler::typed_ast::{
    collect_args, ResolvedTypeDeclaration, StructDefinition, TypedArgDeclaration, TypedBinaryOpCall, TypedBlock, TypedDeclaration, TypedDestructure, TypedExpr, TypedFnCall, TypedIf, TypedIfExpr, TypedMatch, TypedMatchArm, TypedMemberRead, TypedPattern, TypedStatement, TypedTopLevelValue, TypedValueDeclaration, TypedValueType
};
use compiler::types::{self, ResolvedType};
use multimap::MultiMap;

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    pub(crate) module: Module<'ctx>,
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
    enum_discrims: HashMap<String, Vec<String>>,
    curry_ty: StructType<'ctx>,
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
            .map(|(name, _a)| {
                let ty = module.get_global(&name).unwrap_or_else(|| {
                    module.add_global(
                        ctx.struct_type(&[ctx.ptr_type(AddressSpace::default()).into()], false),
                        None,
                        &name,
                    )
                });
                (name, ty)
            })
            .collect();
        let curry_ty = ctx.struct_type(&[ctx.ptr_type(AddressSpace::default()).into()], false);
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
            enum_discrims: HashMap::new(),
            dibuilder: None,
            compile_unit: None,
            difile: None,
            difunction: None,
            dilocals: HashMap::new(), // curried_locals : HashMap::new(),
            target_info,
            ditypes: HashMap::new(),
            needsdi: Vec::new(),
            curry_ty,
        }
    }

    fn compile_arg(&mut self, arg: &TypedArgDeclaration, value: BasicValueEnum<'ctx>) {
        match arg {
            TypedArgDeclaration::Discard { .. } => {
                //todo! check if drop/whatever I will call it is needed
            }
            TypedArgDeclaration::Simple { loc, ident, ty } => {
                let actual_ty = self.type_resolver.resolve_arg_type(ty);
                let arg = self
                    .builder
                    .build_alloca(actual_ty, ident.as_str())
                    .unwrap();
                let value = self.value_or_load(ty.clone(), value);
                self.builder.build_store(arg, value).unwrap();
                self.locals.insert(ident.clone(), arg);
            }
            TypedArgDeclaration::DestructureTuple(contents, ty, loc) => {
                let ptr = value.into_pointer_value();
                let ty = self
                    .type_resolver
                    .resolve_type_as_basic(ty.clone())
                    .into_struct_type();
                for (idx, arg) in contents.iter().enumerate() {
                    let gep = self
                        .builder
                        .build_struct_gep(ty, ptr, idx as u32, "")
                        .unwrap();
                    self.compile_arg(arg, gep.as_basic_value_enum());
                }
            }
            TypedArgDeclaration::DestructureStruct {
                loc,
                struct_ident,
                fields,
                renamed_fields,
            } => {
                todo!("todo destructuring struct args")
            }
            TypedArgDeclaration::Discard { loc, ty } | TypedArgDeclaration::Unit { loc, ty } => {
                //TODO debug info for these args.
            }
        }
    }

    fn compile_function(&mut self, decl: TypedTopLevelValue) {
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.ll");
        let v = self.incomplete_functions.get(&decl.ident).unwrap().clone();

        if let Some(dibuilder) = &self.dibuilder {
            let Some(difile) = self.difile.as_ref() else {
                unreachable!()
            };
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
            let _loc = dibuilder.create_debug_location(
                self.ctx,
                decl.loc.0.try_into().unwrap(),
                decl.loc.1.try_into().unwrap(),
                fun_scope.as_debug_info_scope(),
                None,
            );
            // self.builder.set_current_debug_location(loc);
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
                    .build_alloca(self.type_resolver.resolve_type_as_basic(rt.clone()), "ret")
                    .unwrap(),
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
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            );
            let curried_args = curried_args.into_iter().map(|it| it.into()).collect_vec();
            let first_arg = v.get_first_param().unwrap().into_pointer_value();
            let actual_ty = self.ctx.struct_type(&curried_args, false);
            self.builder
                .build_bit_cast(first_arg, self.ctx.ptr_type(AddressSpace::default()), "")
                .unwrap()
        };
        let actual_ty = {
            let mut curried_args = curried_args.clone();
            curried_args.insert(
                0,
                self.ctx
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            );
            self.ctx.struct_type(&curried_args, false)
        };
        for (idx, arg) in decl.args.iter().enumerate().take(decl.args.len() - 1) {
            let gep = self
                .builder
                .build_struct_gep(
                    actual_ty,
                    first_arg.into_pointer_value(),
                    idx as u32 + 1,
                    "",
                )
                .unwrap();
            self.compile_arg(arg, gep.as_basic_value_enum());
            if let Some(fnscope) = &self.difunction {
                let Some(dibuilder) = &self.dibuilder else {
                    unreachable!()
                };
                let Some(file) = &self.difile else {
                    unreachable!()
                };
                let ty = &decl.ty.as_c_function().0[idx];
                let ty = self.ditypes[&ty.to_string()];
                let local = dibuilder.create_parameter_variable(
                    fnscope.as_debug_info_scope(),
                    &arg.get_ident(),
                    idx as u32 + 1,
                    file.clone(),
                    decl.loc.0.try_into().unwrap(),
                    ty,
                    true,
                    DIFlags::TYPE_PASS_BY_VALUE,
                );
                let diloc = dibuilder.create_debug_location(
                    self.ctx,
                    arg.get_loc().0.try_into().unwrap(),
                    arg.get_loc().1.try_into().unwrap(),
                    fnscope.as_debug_info_scope(),
                    None,
                );
                // dibuilder.insert_declare_at_end(arg, Some(local), None, diloc, args_block);
                self.dilocals.insert(arg.get_ident(), local);
            }
        }
        let last_param = v.get_last_param().unwrap();
        let last_param_info = decl.args.last().unwrap();
        self.compile_arg(last_param_info, last_param);
        if let Some(fnscope) = &self.difunction {
            let Some(dibuilder) = &self.dibuilder else {
                unreachable!()
            };
            let Some(file) = &self.difile else {
                unreachable!()
            };
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
                &last_param_info.get_ident(),
                decl.args.len() as u32 + 1,
                file.clone(),
                decl.loc.0.try_into().unwrap(),
                ty,
                true,
                DIFlags::TYPE_PASS_BY_VALUE,
            );
            let diloc = dibuilder.create_debug_location(
                self.ctx,
                last_param_info.get_loc().0.try_into().unwrap(),
                last_param_info.get_loc().1.try_into().unwrap(),
                fnscope.as_debug_info_scope(),
                None,
            );
            // dibuilder.insert_declare_at_end(arg, Some(local), None, diloc, args_block);
            self.dilocals.insert(last_param_info.get_ident(), local);
        }

        let ret_block = self.ctx.append_basic_block(v, "ret"); //this is what will be used to return
        self.builder.position_at_end(ret_block);
        if rt.is_void_or_unit() || rt.is_user() {
            self.builder.build_return(None).unwrap();
        } else {
            let rt = self.type_resolver.resolve_type_as_basic(rt);
            let ret_value = self.builder.build_load(rt, ret_value.unwrap(), "").unwrap();
            self.builder.build_return(Some(&ret_value)).unwrap();
        }

        self.builder.position_at_end(args_block);
        let bb = self.ctx.append_basic_block(v, "start");
        self.builder.build_unconditional_branch(bb).unwrap();
        self.builder.position_at_end(bb);
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
                        let rt = self.type_resolver.resolve_type_as_basic(rt);
                        self.builder
                            .build_load(rt, value.into_pointer_value(), "")
                            .unwrap()
                    } else {
                        value
                    };
                    self.builder.build_store(*ret_target, value).unwrap();
                }
                self.builder.build_unconditional_branch(ret_bb).unwrap();
            }
            TypedValueType::Function(body) => {
                for expr in body {
                    self.compile_statement(expr);
                }
            }
            TypedValueType::Err => unreachable!("how did an error kind get here"),
            TypedValueType::External => unreachable!("The external linkage should not be compiled"),
        }
        self.difunction = None;
        self.dilocals.clear();
        let _ = ret_block.move_after(v.get_last_basic_block().unwrap());
    }

    fn compile_destructure(
        &mut self,
        expr_ty: ResolvedType,
        value: BasicValueEnum<'ctx>,
        pat: TypedPattern,
    ) {
        match pat {
            TypedPattern::EnumVariant { .. } => {
                todo!("enum variant destructure. should be type checked.")
            }

            TypedPattern::Const(_, _) => {
                println!("invalid code.");
            }
            TypedPattern::Read(name, ty, loc) => {
                let value = self.value_or_load(ty.clone(), value);
                let ty = self.type_resolver.resolve_type_as_basic(ty);
                let target = self.builder.build_alloca(ty, &name).unwrap();
                self.builder.build_store(target, value).unwrap();
                if let Some(fnscope) = &self.difunction {
                    let Some(dibuilder) = &self.dibuilder else {
                        unreachable!()
                    };
                    let Some(file) = &self.difile else {
                        unreachable!()
                    };
                    let diloc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        fnscope.as_debug_info_scope(),
                        None,
                    );
                    let local = dibuilder.create_auto_variable(
                        fnscope.as_debug_info_scope(),
                        &name,
                        file.clone(),
                        loc.0.try_into().unwrap(),
                        self.ditypes[&ty.to_string()],
                        false,
                        DIFlags::ZERO,
                        0,
                    );
                    self.dilocals.insert(name.clone(), local);
                    dibuilder.insert_declare_at_end(
                        target,
                        Some(local),
                        None,
                        diloc,
                        self.builder.get_insert_block().unwrap(),
                    );
                }

                self.locals.insert(name, target);
            }
            TypedPattern::Err => println!("errors shouldn't be here"),
            TypedPattern::Destructure(TypedDestructure::Unit) | TypedPattern::Default => (),
            TypedPattern::Or(_, _) => todo!("or patterns aren't allowed in here"),
            TypedPattern::Destructure(TypedDestructure::Tuple(patterns)) => {
                let tuple_ty = self.type_resolver.resolve_type_as_basic(expr_ty.clone());
                let ResolvedType::Tuple { underlining, .. } = expr_ty else {
                    unreachable!()
                };
                for (idx, (pat, expr_ty)) in patterns.into_iter().zip(underlining).enumerate() {
                    let value = self
                        .builder
                        .build_struct_gep(tuple_ty, value.into_pointer_value(), idx as _, "")
                        .unwrap();
                    self.compile_destructure(expr_ty, value.as_basic_value_enum(), pat);
                }
            }
            TypedPattern::Destructure(TypedDestructure::Struct { fields }) => todo!(),
        }
    }

    pub fn compile_statement(&mut self, stmnt: TypedStatement) {
        match stmnt {
            TypedStatement::IfBranching(ifbranch) => {
                let TypedIf {
                    cond,
                    true_branch,
                    else_branch,
                    ..
                } = ifbranch;
                let parent_block = self
                .builder
                .get_insert_block()
                .unwrap();
                let fun = 
                    parent_block
                    .get_parent()
                    .unwrap();
                let true_block = self.ctx.append_basic_block(fun, "");
                let end_block = self.ctx.append_basic_block(fun, "");
                let cond = match self.compile_expr(*cond) {
                    AnyValueEnum::PointerValue(ptr) => self.builder.build_load(self.ctx.bool_type(), ptr,"").unwrap().into_int_value(),
                    AnyValueEnum::IntValue(it) => it,
                    _=> unreachable!("it can only ever be a point which means a value read or an expression resulting in a boolean")
                };
                //build the true branch.
                self.builder.position_at_end(true_block);
                for stmnt in true_branch.statements {
                    self.compile_statement(stmnt);
                }
                if let Some(true_expr) = true_branch.implicit_ret {
                    let _ = self.compile_expr(*true_expr);
                }
                self.builder.build_unconditional_branch(end_block);
                self.builder.position_at_end(parent_block);//reset to parent block to build the conditional
                if let Some(else_branch) = else_branch {
                    //if then else
                    let else_block = self.ctx.append_basic_block(fun, "");
                    self.builder
                        .build_conditional_branch(cond, true_block, else_block)
                        .unwrap();

                    self.builder.build_unconditional_branch(end_block).unwrap();
                    self.builder.position_at_end(else_block);
                    for stmnt in else_branch.statements {
                        self.compile_statement(stmnt);
                    }
                    if let Some(else_expr) = else_branch.implicit_ret {
                        let _ = self.compile_expr(*else_expr);
                    }
                    let _ = end_block.move_after(else_block);
                    self.builder.build_unconditional_branch(end_block).unwrap();
                } else {
                    
                    self.builder
                        .build_conditional_branch(cond, true_block, end_block);
                }

                #[cfg(debug_assertions)]
                let _ = self.module.print_to_file("./debug.ll");
                self.builder.position_at_end(end_block);
            }
            TypedStatement::Return(expr, loc) => {
                if let Some(dibuilder) = &self.dibuilder {
                    let Some(difun) = &self.difunction else {
                        unreachable!()
                    };
                    let _loc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        difun.as_debug_info_scope(),
                        None,
                    );
                    // self.builder.set_current_debug_location(loc);
                }
                let ret_bb = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap()
                    .get_basic_blocks()[1];
                if let TypedExpr::UnitLiteral = expr {
                    self.builder.build_unconditional_branch(ret_bb).unwrap();
                } else {
                    let expr_ty = expr.get_ty();
                    let value = self.compile_expr(expr);

                    let value = self.value_or_load(expr_ty, value.try_into().unwrap());
                    if let Some(ret_target) = self.ret_target.as_ref() {
                        self.builder.build_store(*ret_target, value).unwrap();
                    }
                    self.builder.build_unconditional_branch(ret_bb).unwrap();
                }
            }

            TypedStatement::FnCall(data) => {
                if let Some(dibuilder) = &self.dibuilder {
                    let Some(difun) = &self.difunction else {
                        unreachable!()
                    };
                    let _loc = dibuilder.create_debug_location(
                        self.ctx,
                        data.loc.0.try_into().unwrap(),
                        data.loc.1.try_into().unwrap(),
                        difun.as_debug_info_scope(),
                        None,
                    );
                    // self.builder.set_current_debug_location(loc);
                }
                self.compile_expr(TypedExpr::FnCall(data));
            }

            TypedStatement::Declaration(TypedValueDeclaration {
                target,
                ty,
                value,
                loc,
                ..
            }) => {
                if let TypedValueType::Expr(expr) = value {
                    let expr = convert_to_basic_value(self.compile_expr(expr));
                    self.compile_destructure(ty, expr, target);
                } else {
                    todo!("unsure here?")
                }
            }
            TypedStatement::Match(match_) => {
                self.compile_match(match_);
            }
            TypedStatement::Discard(expr, _) => {
                self.compile_expr(expr);
            }
            _ => todo!(),
        }
    }

    fn compile_block(&mut self, block : TypedBlock) -> AnyValueEnum<'ctx> {
        let TypedBlock {
            statements,
            implicit_ret,
            ret_ty:_,
        } = block;

        for stmnt in statements {
            self.compile_statement(stmnt);
        }
        if let Some(ret) = implicit_ret {
            self.compile_expr(*ret)
        } else {
            self.ctx.i8_type().const_zero().into() // in theory this should never matter. could be any value.
        }
    }

    pub fn compile_expr(&mut self, expr: TypedExpr) -> AnyValueEnum<'ctx> {
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.ll");
        match expr {
            TypedExpr::BoolLiteral(value, _loc) => {
                if value {
                    self.ctx.bool_type().const_int(1, false).as_any_value_enum()
                } else {
                    self.ctx.bool_type().const_zero().as_any_value_enum()
                }
            }
            TypedExpr::IfExpr(expr) => {
                let TypedIf {
                    cond,
                    true_branch,
                    else_branch,
                    loc: _,
                    result_ty:rt
                } = expr;
                let ty = self.type_resolver.resolve_type_as_basic(rt.clone());
                
                let fun = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let root_cond = match self.compile_expr(*cond) {
                    AnyValueEnum::PointerValue(p) => self
                        .builder
                        .build_load(self.ctx.bool_type(), p, "")
                        .unwrap()
                        .into_int_value(),
                    AnyValueEnum::IntValue(i) => i,
                    _ => unreachable!(),
                };
                let result_block = self.ctx.append_basic_block(fun, "");
                let then_block = self.ctx.append_basic_block(fun, "");
                let else_block = self.ctx.append_basic_block(fun, "");
                
                self.builder
                    .build_conditional_branch(root_cond, then_block, else_block)
                    .unwrap();
                self.builder.position_at_end(then_block);
                
                let true_value = self.compile_block(true_branch);
                let true_value = convert_to_basic_value(true_value);
                let true_value = if !rt.is_user() && true_value.is_pointer_value() {
                    self.builder
                        .build_load(ty, true_value.into_pointer_value(), "")
                        .unwrap()
                } else {
                    true_value
                };
                self.builder.position_at_end(else_block);
                let else_value = self.compile_block(else_branch.expect("if expressions require an else"));
                let else_value = convert_to_basic_value(else_value);
                let else_value = if !rt.is_user() && else_value.is_pointer_value() {
                    self.builder
                        .build_load(ty, else_value.into_pointer_value(), "")
                        .unwrap()
                } else {
                    else_value
                };
                self.builder.position_at_end(result_block);
                let phi = self.builder.build_phi(ty, "").unwrap();
                phi.add_incoming(&[(&true_value, then_block), (&else_value, else_block)]);
                let _ = result_block.move_after(else_block);
                phi.as_any_value_enum()
                
            }
            TypedExpr::BinaryOpCall(TypedBinaryOpCall {
                operator,
                lhs,
                rhs,
                loc,
                ..
            }) => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if lhs_t.is_user() || rhs_t.is_user() {
                    unimplemented!("user defined operators not supported yet")
                }
                if let Some(dibuilder) = &self.dibuilder {
                    let _loc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        self.difunction.as_ref().unwrap().as_debug_info_scope(),
                        None,
                    );
                    // self.builder.set_current_debug_location(loc);
                }
                match operator.as_str() {
                    "&&" => {
                        let result = self.builder.build_alloca(self.ctx.bool_type(), "").unwrap();
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t, lhs);
                        let fun = self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap();
                        let lhs_false = self.ctx.append_basic_block(fun, "");
                        let else_block = self.ctx.append_basic_block(fun, "");
                        let continue_block = self.ctx.append_basic_block(fun, "");
                        self.builder
                            .build_conditional_branch(lhs.into_int_value(), else_block, lhs_false)
                            .unwrap();
                        self.builder.position_at_end(lhs_false);
                        self.builder
                            .build_store(result, self.ctx.bool_type().const_zero())
                            .unwrap();
                        self.builder
                            .build_unconditional_branch(continue_block)
                            .unwrap();
                        self.builder.position_at_end(else_block);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t, rhs);
                        self.builder.build_store(result, rhs).unwrap();
                        self.builder
                            .build_unconditional_branch(continue_block)
                            .unwrap();
                        self.builder.position_at_end(continue_block);
                        let result = self
                            .builder
                            .build_load(self.ctx.bool_type(), result, "")
                            .unwrap();
                        result.as_any_value_enum()
                    }

                    "||" => {
                        let result = self.builder.build_alloca(self.ctx.bool_type(), "").unwrap();
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t, lhs);
                        let fun = self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap();
                        let lhs_true = self.ctx.append_basic_block(fun, "");
                        let else_block = self.ctx.append_basic_block(fun, "");
                        let continue_block = self.ctx.append_basic_block(fun, "");
                        self.builder
                            .build_conditional_branch(lhs.into_int_value(), lhs_true, else_block)
                            .unwrap();
                        self.builder.position_at_end(lhs_true);
                        self.builder
                            .build_store(result, self.ctx.bool_type().const_int(1, false))
                            .unwrap();
                        self.builder
                            .build_unconditional_branch(continue_block)
                            .unwrap();
                        self.builder.position_at_end(else_block);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t, rhs);
                        self.builder.build_store(result, rhs).unwrap();
                        self.builder
                            .build_unconditional_branch(continue_block)
                            .unwrap();
                        self.builder.position_at_end(continue_block);
                        let result = self
                            .builder
                            .build_load(self.ctx.bool_type(), result, "")
                            .unwrap();
                        result.as_any_value_enum()
                    }

                    "==" => {
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t.clone(), lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t.clone(), rhs);
                        if lhs_t.is_user() || rhs_t.is_user() {
                            panic!("comparing user types is not supported");
                        }
                        if lhs_t == types::BOOL || lhs_t.is_int() {
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        } else {
                            //this should have warned.
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OEQ,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        }
                    }
                    "!=" => {
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t.clone(), lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t.clone(), rhs);
                        if lhs_t.is_user() || rhs_t.is_user() {
                            panic!("comparing user types is not supported");
                        }
                        if lhs_t == types::BOOL || lhs_t.is_int() {
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::NE,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        } else {
                            // this should have warned
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::UNE,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        }
                    }
                    "<=" => {
                        if lhs_t.is_user() || rhs_t.is_user() {
                            panic!("comparing user types is not supported");
                        }
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t.clone(), lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t.clone(), rhs);

                        if lhs_t.is_int() || rhs_t.is_int() {
                            let lhs_s = if let ResolvedType::Int { signed, .. } = lhs_t {
                                signed
                            } else {
                                false
                            };
                            let rhs_s = if let ResolvedType::Int { signed, .. } = rhs_t {
                                signed
                            } else {
                                false
                            };
                            self.builder
                                .build_int_compare(
                                    if lhs_s || rhs_s {
                                        IntPredicate::SLE
                                    } else {
                                        IntPredicate::ULE
                                    },
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        } else {
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OLE,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        }
                    }
                    "<" => {
                        if lhs_t.is_user() || rhs_t.is_user() {
                            panic!("comparing user types is not supported");
                        }
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t.clone(), lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t.clone(), rhs);

                        if lhs_t.is_int() || rhs_t.is_int() {
                            let lhs_s = if let ResolvedType::Int { signed, .. } = lhs_t {
                                signed
                            } else {
                                false
                            };
                            let rhs_s = if let ResolvedType::Int { signed, .. } = rhs_t {
                                signed
                            } else {
                                false
                            };
                            self.builder
                                .build_int_compare(
                                    if lhs_s || rhs_s {
                                        IntPredicate::SLT
                                    } else {
                                        IntPredicate::ULE
                                    },
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        } else {
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OLT,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        }
                    }
                    ">=" => {
                        if lhs_t.is_user() || rhs_t.is_user() {
                            panic!("comparing user types is not supported");
                        }
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t.clone(), lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t.clone(), rhs);

                        if lhs_t.is_int() || rhs_t.is_int() {
                            let lhs_s = if let ResolvedType::Int { signed, .. } = lhs_t {
                                signed
                            } else {
                                false
                            };
                            let rhs_s = if let ResolvedType::Int { signed, .. } = rhs_t {
                                signed
                            } else {
                                false
                            };
                            self.builder
                                .build_int_compare(
                                    if lhs_s || rhs_s {
                                        IntPredicate::SGE
                                    } else {
                                        IntPredicate::ULE
                                    },
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        } else {
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OGE,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        }
                    }
                    ">" => {
                        if lhs_t.is_user() || rhs_t.is_user() {
                            panic!("comparing user types is not supported");
                        }
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t.clone(), lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t.clone(), rhs);

                        if lhs_t.is_int() || rhs_t.is_int() {
                            let lhs_s = if let ResolvedType::Int { signed, .. } = lhs_t {
                                signed
                            } else {
                                false
                            };
                            let rhs_s = if let ResolvedType::Int { signed, .. } = rhs_t {
                                signed
                            } else {
                                false
                            };
                            self.builder
                                .build_int_compare(
                                    if lhs_s || rhs_s {
                                        IntPredicate::SGT
                                    } else {
                                        IntPredicate::ULE
                                    },
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        } else {
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OGT,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_any_value_enum()
                        }
                    }
                    "+" => {
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t, lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t, rhs);
                        match (lhs, rhs) {
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                self.builder
                                    .build_float_add(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                                let rhs = self
                                    .builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_add(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                let lhs = self
                                    .builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_add(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                                .builder
                                .build_int_add(lhs, rhs, "")
                                .unwrap()
                                .as_any_value_enum(),
                            _ => unimplemented!("Operation is not currently supported."),
                        }
                    }
                    "-" => {
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t, lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t, rhs);
                        match (lhs, rhs) {
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                self.builder
                                    .build_float_sub(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                                let rhs = self
                                    .builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_sub(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                let lhs = self
                                    .builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_sub(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                                .builder
                                .build_int_sub(lhs, rhs, "")
                                .unwrap()
                                .as_any_value_enum(),
                            _ => unimplemented!("Operation is not currently supported."),
                        }
                    }
                    "*" => {
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t, lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t, rhs);
                        match (lhs, rhs) {
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                self.builder
                                    .build_float_mul(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                                let rhs = self
                                    .builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_mul(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                let lhs = self
                                    .builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_mul(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                                .builder
                                .build_int_mul(lhs, rhs, "")
                                .unwrap()
                                .as_any_value_enum(),
                            _ => unimplemented!("Operation is not currently supported."),
                        }
                    }
                    "/" => {
                        let lhs = convert_to_basic_value(self.compile_expr(*lhs));
                        let lhs = self.value_or_load(lhs_t, lhs);
                        let rhs = convert_to_basic_value(self.compile_expr(*rhs));
                        let rhs = self.value_or_load(rhs_t, rhs);
                        match (lhs, rhs) {
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                self.builder
                                    .build_float_div(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                                let rhs = self
                                    .builder
                                    .build_signed_int_to_float(rhs, lhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_div(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                                let lhs = self
                                    .builder
                                    .build_signed_int_to_float(lhs, rhs.get_type(), "")
                                    .unwrap();
                                self.builder
                                    .build_float_div(lhs, rhs, "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                                .builder
                                .build_int_signed_div(lhs, rhs, "")
                                .unwrap()
                                .as_any_value_enum(),
                            _ => unimplemented!("Operation is not currently supported."),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            TypedExpr::UnaryOpCall { .. } => todo!(),
            TypedExpr::FnCall(TypedFnCall {
                value,
                arg,
                rt,
                arg_t: fn_arg_t,
                loc,
                is_extern,
            }) => {
                let Some(arg) = arg else {
                    unimplemented!(
                        "implicit calling not implemented. should be caught by type checker."
                    )
                };
                if is_extern {
                    let TypedExpr::ValueRead(ident, _, _) = *value else {
                        unreachable!(
                            "this shouldn't happen.  should be handled by type checking phase."
                        )
                    };
                    let value = self
                        .known_functions
                        .get(&ident)
                        .expect("undeclared global? should be handled at type checking.")
                        .clone();
                    let args: Vec<BasicValueEnum> = if let ResolvedType::Tuple {
                        underlining: _,
                        loc: _,
                    } = fn_arg_t
                    {
                        let TypedExpr::TupleLiteral { contents, loc: _ } = *arg else {
                            unreachable!("shouldn't be a tuple?")
                        };
                        contents
                            .into_iter()
                            .map(|expr| self.compile_expr(expr).try_into().unwrap())
                            .collect()
                    } else {
                        vec![self.compile_expr(*arg).try_into().unwrap()]
                    };
                    let args = args.into_iter().map(|arg| arg.into()).collect_vec();
                    return self
                        .builder
                        .build_call(value.as_any_value_enum().into_function_value(), &args, "")
                        .unwrap()
                        .as_any_value_enum();
                }
                let arg_t = self.type_resolver.resolve_arg_type(&fn_arg_t);
                let arg = self.compile_expr(*arg);
                let arg: BasicValueEnum = arg.try_into().unwrap();
                let arg = if arg.is_pointer_value() && !arg_t.is_pointer_type()
                // && !arg
                //     .into_pointer_value()
                //     .get_type()
                //     .get_element_type()
                //     .is_struct_type()
                // need to figure out replacement for this type
                {
                    //there has to be a better way to do this.
                    self.builder
                        .build_load(arg_t, arg.into_pointer_value(), "")
                        .unwrap()
                } else {
                    arg
                };
                dbg!(arg_t);
                let value_t = value.get_ty();
                let value = self.compile_expr(*value);
                if let Some(dibuilder) = &self.dibuilder {
                    let _loc = dibuilder.create_debug_location(
                        self.ctx,
                        loc.0.try_into().unwrap(),
                        loc.1.try_into().unwrap(),
                        self.difunction.as_ref().unwrap().as_debug_info_scope(),
                        None,
                    );
                    // self.builder.set_current_debug_location(loc);
                }

                let target = self
                    .builder
                    .build_struct_gep(self.curry_ty, value.into_pointer_value(), 0, "")
                    .unwrap();
                let target = self
                    .builder
                    .build_load(
                        self.ctx.i8_type().ptr_type(AddressSpace::default()),
                        target,
                        "",
                    )
                    .unwrap()
                    .into_pointer_value();
                let value: BasicValueEnum = value.try_into().unwrap();
                let fn_t = self
                    .type_resolver
                    .resolve_type_as_function(&fn_arg_t.fn_ty(&rt));
                if rt.is_user() {
                    let result = self
                        .builder
                        .build_alloca(self.type_resolver.resolve_type_as_basic(rt), "")
                        .unwrap();
                    self.builder
                        .build_indirect_call(
                            fn_t,
                            target,
                            &[value.into(), result.into(), arg.into()],
                            "",
                        )
                        .unwrap();
                    result.as_any_value_enum()
                } else {
                    self.builder
                        .build_indirect_call(fn_t, target, &[value.into(), arg.into()], "")
                        .unwrap()
                        .as_any_value_enum()
                }
                /*
                match value {
                    AnyValueEnum::PointerValue(target) => {
                        match target.get_type().get_element_type() {
                            AnyTypeEnum::StructType(_) => {
                                let target_fun =
                                    self.builder.build_struct_gep(target, 0, "").unwrap();
                                let target_fun = self.builder.build_load(target_fun, "").unwrap();
                                let ty = self
                                    .type_resolver
                                    .resolve_type_as_function(&value_t)
                                    .ptr_type(AddressSpace::default());
                                let target_fun = self
                                    .builder
                                    .build_bitcast(target_fun, ty, "")
                                    .unwrap()
                                    .into_pointer_value();
                                let target_fun: CallableValue = target_fun.try_into().unwrap();
                                if rt.is_user() {
                                    let result = self
                                        .builder
                                        .build_alloca(
                                            self.type_resolver.resolve_type_as_basic(rt),
                                            "",
                                        )
                                        .unwrap();
                                    self.builder
                                        .build_call(
                                            target_fun,
                                            &[target.into(), result.into(), arg.into()],
                                            "",
                                        )
                                        .unwrap();
                                    result.as_any_value_enum()
                                } else {
                                    #[cfg(debug_assertions)]
                                    let _ = self.module.print_to_file("./debug.ll");
                                    self.builder
                                        .build_call(target_fun, &[target.into(), arg.into()], "")
                                        .unwrap()
                                        .as_any_value_enum()
                                }
                            }
                            AnyTypeEnum::PointerType(ptr) => {
                                if !ptr.get_element_type().is_struct_type() {
                                    unreachable!()
                                };
                                let strct_t = ptr.get_element_type().into_struct_type();
                                let target = self
                                    .builder
                                    .build_load(target, "")
                                    .unwrap()
                                    .into_pointer_value();
                                let target_fun =
                                    self.builder.build_struct_gep(target, 0, "").unwrap();
                                let target_fun = self.builder.build_load(target_fun, "").unwrap();
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
                                    .unwrap()
                                    .into_pointer_value();
                                let target_fun: CallableValue = target_fun.try_into().unwrap();
                                self.builder
                                    .build_call(target_fun, &[target.into(), arg.into()], "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            AnyTypeEnum::FunctionType(_) => {
                                let target: CallableValue = target.try_into().unwrap();
                                self.builder
                                    .build_call(target, &[arg.into()], "")
                                    .unwrap()
                                    .as_any_value_enum()
                            }
                            _ => {
                                #[cfg(debug_assertions)]
                                let _ = self.module.print_to_file("./error.ll");
                                unreachable!();
                            }
                        }
                    }
                    AnyValueEnum::FunctionValue(target) => {
                        let _expect_ty = target.get_type().get_param_types();
                        self.builder
                            .build_call(target, &[arg.into()], "")
                            .unwrap()
                            .as_any_value_enum()
                    }
                    _ => {
                        #[cfg(debug_assertions)]
                        let _ = self.module.print_to_file("./error.ll");
                        unreachable!();
                    }
                }
                */
            }
            //
            // TypedExpr::FnCall(TypedFnCall { value, loc, .. }) => {
            //     //this should only ever be a named value?
            //     if let Some(dibuilder) = &self.dibuilder {
            //         let _loc = dibuilder.create_debug_location(
            //             self.ctx,
            //             loc.0.try_into().unwrap(),
            //             loc.1.try_into().unwrap(),
            //             self.difunction.as_ref().unwrap().as_debug_info_scope(),
            //             None,
            //         );
            //         // self.builder.set_current_debug_location(loc);
            //     }
            //     let TypedExpr::ValueRead(ident, _, _) = *value else {
            //         unreachable!("not a function name?")
            //     };
            //     let Some(gv) = self.known_functions.get(&ident) else {
            //         unreachable!("function not found")
            //     };
            //     let fun = self
            //         .builder
            //         .build_struct_gep(gv.as_pointer_value(), 0, "")
            //         .unwrap();
            //     let fun: CallableValue = fun.try_into().unwrap();
            //     self.builder
            //         .build_call(fun, &[gv.as_pointer_value().into()], "")
            //         .unwrap()
            //         .as_any_value_enum()
            // }
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
                let v = unsafe { ty.const_float_from_string(&value) };
                v.as_any_value_enum()
            }
            TypedExpr::StringLiteral(value) => {
                let cs = self.ctx.const_string(value.as_bytes(), false);
                let gs = self.module.add_global(cs.get_type(), None, "");
                gs.set_initializer(&cs);
                gs.set_constant(true);
                let str_t = cs.get_type();
                // let str_t = self.type_resolver.resolve_type_as_basic(types::STR);
                let ptr = unsafe {
                    gs.as_pointer_value().const_in_bounds_gep(
                        str_t,
                        &[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_zero(),
                        ],
                    )
                };
                let ptr_end = unsafe {
                    gs.as_pointer_value().const_in_bounds_gep(
                        str_t,
                        &[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_int(value.len() as u64, false),
                        ],
                    )
                };
                let ty = self
                    .type_resolver
                    .resolve_type_as_basic(types::STR)
                    .into_struct_type();
                let p = self.builder.build_alloca(ty, "").unwrap();
                self.builder
                    .build_store(p, ty.const_named_struct(&[ptr.into(), ptr_end.into()]))
                    .unwrap();
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
                let out = self.builder.build_alloca(target_t, "").unwrap();
                #[allow(irrefutable_let_patterns)]
                let ResolvedTypeDeclaration::Struct(def) =
                    self.known_types.get(&con.ident).unwrap().clone()
                else {
                    unreachable!()
                };
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
                        .build_struct_gep(target_t, out, offest as u32, "")
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
                    // && !result
                    //     .get_type()
                    //     .into_pointer_type()
                    //     .get_element_type()
                    //     .is_struct_type()
                    // TODO! need a replacement
                    {
                        self.builder
                            .build_load(target_t, result.into_pointer_value(), "")
                            .unwrap()
                    } else {
                        result
                    };
                    self.builder.build_store(target_gep, result).unwrap();
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
                let target_t = self.type_resolver.resolve_type_as_basic(ty);
                let target_result = self.compile_expr(*target);
                if let Some(offset) = offset {
                    self.builder
                        .build_struct_gep(
                            target_t,
                            (target_result).into_pointer_value(),
                            offset as _,
                            "",
                        )
                        .unwrap()
                        .as_any_value_enum()
                } else {
                    todo!("member functions")
                }
            }
            TypedExpr::Match(match_) => self.compile_match(match_),
            TypedExpr::ArrayLiteral {
                contents,
                underlining,
            } => {
                let arr_ty = self
                    .type_resolver
                    .resolve_type_as_basic(underlining)
                    .array_type(contents.len() as u32);
                let arr = self.builder.build_alloca(arr_ty, "").unwrap();
                for (idx, expr) in contents.into_iter().enumerate() {
                    let ele = convert_to_basic_value(self.compile_expr(expr));
                    let loc = unsafe {
                        self.builder
                            .build_in_bounds_gep(
                                arr_ty,
                                arr,
                                &[
                                    self.ctx.i32_type().const_zero(),
                                    self.ctx.i32_type().const_int(idx as u64, false),
                                ],
                                "",
                            )
                            .unwrap()
                    };
                    self.builder.build_store(loc, ele).unwrap();
                }
                arr.as_any_value_enum()
            }
            TypedExpr::ListLiteral { contents } => todo!(),
            TypedExpr::TupleLiteral { contents, .. } => {
                let tys = contents.iter().map(|it| it.get_ty()).collect();
                let ty = self
                    .type_resolver
                    .resolve_type_as_basic(ResolvedType::Tuple {
                        underlining: tys,
                        loc: (0, 0),
                    });
                let tuple = self.builder.build_alloca(ty, "").unwrap();
                for (idx, expr) in contents.into_iter().enumerate() {
                    let ele = convert_to_basic_value(self.compile_expr(expr));
                    let loc = self
                        .builder
                        .build_struct_gep(ty, tuple, idx as u32, "")
                        .unwrap();
                    self.builder.build_store(loc, ele);
                }
                tuple.as_any_value_enum()
            }
            TypedExpr::ErrorNode => unreachable!(),
        }
    }

    fn add_struct_di(&mut self, def: &StructDefinition) -> bool {
        let Some(dibuilder) = &self.dibuilder else {
            unreachable!()
        };
        let Some(file) = &self.difile else {
            unreachable!()
        };
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
        let Some(discope) = &self.difile else {
            unreachable!()
        };
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
        if let Some(_di_placeholder) = self.ditypes.get(&def.ident) {
            todo!("replace the placeholder somehow");
        } else {
            self.ditypes.insert(def.ident.clone(), di_struct.as_type());
        }
        true
    }
    pub fn compile_decl(&mut self, decl: TypedDeclaration) -> Module<'ctx> {
        match decl {
            TypedDeclaration::Value(data) => {
                self.compile_function(data);
            }
            TypedDeclaration::TypeDefinition(def) => match def {
                compiler::typed_ast::ResolvedTypeDeclaration::Enum(enum_) => {
                    if !enum_.generics.is_none() {
                        return self.module.clone();
                    }
                }
                compiler::typed_ast::ResolvedTypeDeclaration::Struct(def) => {
                    if !def.generics.is_none() {
                        return self.module.clone();
                    }
                    let strct = self.ctx.get_struct_type(&def.ident).unwrap();
                }
                _ => todo!(),
            },
        }
        if let Some(dibuilder) = &mut self.dibuilder {
            dibuilder.finalize()
        }
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.ll");
        self.module.clone()
    }

    pub(crate) fn create_define(&mut self, decl: &TypedDeclaration) {
        match decl {
            TypedDeclaration::Value(decl) => {
                if let Some(abi) = &decl.abi {
                    match abi.identifier.as_str() {
                        "C" => {
                            // let (args,ret) = decl.ty.as_c_function();
                            if let ResolvedType::Function {
                                arg,
                                returns: ret,
                                loc: _,
                            } = &decl.ty
                            {
                                let args = if let ResolvedType::Tuple {
                                    underlining,
                                    loc: _,
                                } = arg.as_ref()
                                {
                                    underlining.clone()
                                } else {
                                    vec![arg.as_ref().clone()]
                                };
                                let ret = self
                                    .type_resolver
                                    .resolve_type_as_basic(ret.as_ref().clone());
                                let args = args
                                    .into_iter()
                                    .map(|it| self.type_resolver.resolve_type_as_basic(it).into())
                                    .collect_vec();
                                let fun = self.module.add_function(
                                    &decl.ident,
                                    ret.fn_type(&args, false),
                                    Some(inkwell::module::Linkage::External),
                                );

                                self.known_functions
                                    .insert(decl.ident.clone(), fun.as_global_value());
                            } else {
                                let ty = self.type_resolver.resolve_type_as_basic(decl.ty.clone());
                                let gs = self.module.add_global(ty, None, &decl.ident);
                                self.known_values
                                    .insert(decl.ident.clone(), gs.as_basic_value_enum());
                            }
                        }
                        "intrinsic" => {
                            () // do nothing here as this is handled in the code gen itself.  eg `let (+) : int32 -> int32 -> int32` which should output an add signed instruction
                        }
                        _ => {
                            println!("unknown abi {}", abi.identifier)
                        }
                    }
                } else if decl.ty.is_function() && !decl.args.is_empty() {
                    let fun = self.create_curry_list(decl);
                    self.known_functions.insert(decl.ident.clone(), fun);
                } else if decl.ty.is_function() {
                    let TypedValueType::Expr(expr) = &decl.value else {
                        unreachable!()
                    };
                    let ty = if let TypedExpr::ValueRead(name, _, _) = expr {
                        self.ctx.struct_type(
                            &[self.ctx.i8_type().ptr_type(AddressSpace::default()).into()],
                            false,
                        )
                    } else if let TypedExpr::FnCall(fun) = expr {
                        let fields = self.fold_arg_ty(fun);
                        let fields = [self.ctx.i8_type().ptr_type(AddressSpace::default()).into()]
                            .into_iter()
                            .chain(fields)
                            .collect_vec();
                        self.ctx.struct_type(&fields, false)
                    } else {
                        todo!("const other expressions?");
                    };
                    let value = self.module.add_global(ty, None, &decl.ident);
                    value.set_initializer(&ty.const_zero());
                    self.known_values
                        .insert(decl.ident.clone(), value.as_basic_value_enum());
                } else {
                    let ty = self.type_resolver.resolve_type_as_basic(decl.ty.clone());
                    let value = self.module.add_global(ty, None, &decl.ident);
                    self.known_values
                        .insert(decl.ident.clone(), value.as_basic_value_enum());
                    todo!("compile time values?")
                }
            }
            TypedDeclaration::TypeDefinition(def) => match def {
                compiler::typed_ast::ResolvedTypeDeclaration::Struct(decl) => {
                    dbg!(&decl.ident);
                    if decl.generics.is_some() {
                        return;
                    }
                    let strct = self.ctx.opaque_struct_type(&decl.ident);
                    self.known_types.insert(
                        decl.ident.clone(),
                        ResolvedTypeDeclaration::Struct(decl.clone()),
                    );
                    let fields = decl
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
                    if let Some(_dibuilder) = &self.dibuilder {
                        if !self.add_struct_di(decl) {
                            self.needsdi
                                .push(ResolvedTypeDeclaration::Struct(decl.clone()));
                        }
                    }
                }
                compiler::typed_ast::ResolvedTypeDeclaration::Enum(enum_) => {
                    let i32_t = self.type_resolver.resolve_type_as_basic(types::INT32);
                    let i8_t = self.type_resolver.resolve_type_as_basic(types::INT8);

                    if enum_.generics.is_some() {
                        return;
                    }
                    let enum_struct = self.ctx.opaque_struct_type(&enum_.ident);
                    let mut enum_discrims = Vec::new();
                    for variant in &enum_.values {
                        match variant {
                            compiler::ast::EnumVariant::Unit { ident, .. }
                            | compiler::ast::EnumVariant::Tuple { ident, .. }
                            | compiler::ast::EnumVariant::Struct { ident, .. } => {
                                enum_discrims.push(ident.clone());
                                let _variant = self
                                    .ctx
                                    .opaque_struct_type(&format!("{}::{}", &enum_.ident, ident));
                            }
                        }
                    }
                    self.known_types.insert(
                        enum_.ident.clone(),
                        ResolvedTypeDeclaration::Enum(enum_.clone()),
                    );
                    self.enum_discrims
                        .insert(enum_.ident.clone(), enum_discrims);
                    let mut max_size = 0;
                    for variant in &enum_.values {
                        let variant_struct = self
                            .ctx
                            .get_struct_type(&format!("{}::{}", &enum_.ident, &variant.get_ident()))
                            .unwrap();
                        match variant {
                            EnumVariant::Unit { .. } => variant_struct.set_body(&[], false),
                            EnumVariant::Tuple { ty, .. } => variant_struct.set_body(
                                &[i8_t, self.type_resolver.resolve_type_as_basic(ty.clone())],
                                false,
                            ),
                            EnumVariant::Struct { fields, .. } => {
                                let fields = fields
                                    .iter()
                                    .map(|field| {
                                        (
                                            field.name.clone(),
                                            self.type_resolver
                                                .resolve_type_as_basic(field.ty.clone()),
                                        )
                                    })
                                    .collect_vec();
                                let fields_no_name =
                                    fields.iter().map(|(_, it)| it.clone()).collect_vec();
                                let sub_struct = self.ctx.struct_type(&fields_no_name, false);
                                variant_struct.set_body(&[i8_t, sub_struct.into()], false)
                            }
                        };
                        max_size = max_size.max(self.target_info.get_abi_size(&variant_struct));
                    }
                    enum_struct.set_body(
                        &[
                            i8_t,
                            i32_t
                                .array_type(
                                    (max_size / 4 + if max_size % 4 == 0 { 0 } else { 1 }) as u32,
                                )
                                .into(),
                        ],
                        false,
                    );
                }
                _ => todo!(),
            },
        }
    }

    fn create_curry_list(&mut self, decl: &TypedTopLevelValue) -> GlobalValue<'ctx> {
        let TypedTopLevelValue {
            ident, args, ty, ..
        } = decl;
        let curry_placeholder = self.ctx.ptr_type(AddressSpace::default());
        let mut result_ty = ty.clone();
        let mut curried_args = Vec::with_capacity(args.len());
        let gs = self.module.add_global(
            self.ctx
                .struct_type(&[self.ctx.ptr_type(AddressSpace::default()).into()], false),
            None,
            &ident,
        );
        // generate needed supporting functions.
        let args_curry_functions = args
            .iter()
            .rev()
            .take(args.len() - 1)
            .map(|_| {
                let ResolvedType::Function {
                    arg: arg_t,
                    returns,
                    loc: _,
                } = result_ty.clone()
                else {
                    unreachable!()
                };
                let arg_t = self.type_resolver.resolve_arg_type(&arg_t);
                let fun_t =
                    curry_placeholder.fn_type(&[curry_placeholder.into(), arg_t.into()], false);
                result_ty = match *returns {
                    ResolvedType::Pointer { underlining }
                        if matches!(underlining.as_ref(), ResolvedType::Function { .. }) =>
                    {
                        *underlining
                    }
                    _ => *returns,
                };
                let fun = self.module.add_function(&ident, fun_t, None);
                curried_args.push(arg_t);
                fun
            })
            .collect_vec();

        let ResolvedType::Function {
            arg: arg_t,
            returns: rt,
            loc: _,
        } = result_ty
        else {
            unreachable!()
        };
        let fun_t = if rt.as_ref() == &ResolvedType::Void || rt.as_ref() == &ResolvedType::Unit {
            let rt = self.ctx.void_type();
            let arg_t = self.type_resolver.resolve_arg_type(&arg_t);
            rt.fn_type(&[curry_placeholder.into(), arg_t.into()], false)
        } else {
            let arg_t = self.type_resolver.resolve_arg_type(&arg_t);
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
            let first_t = self.ctx.struct_type(&curried_args[..(idx + 1)], false);
            let ret_t = self.ctx.struct_type(&curried_args[..=(idx + 1)], false);
            let ret = self.builder.build_malloc(ret_t, "ret").unwrap();
            let next_fn_ptr = next.as_global_value().as_pointer_value();
            let next_ptr = self.builder.build_struct_gep(ret_t, ret, 0, "").unwrap();
            self.builder.build_store(next_ptr, next_fn_ptr).unwrap();
            let expected = self.ctx.struct_type(&curried_args[..=idx], false);
            let first_elem = curr.get_first_param().unwrap();
            let first_elem = self
                .builder
                .build_bit_cast(
                    first_elem,
                    self.ctx.ptr_type(AddressSpace::default()),
                    "curried",
                )
                .unwrap()
                .into_pointer_value();

            //copy the elements from old to new
            for idx in 0..idx {
                let element = self
                    .builder
                    .build_struct_gep(first_t, first_elem, idx as u32 + 1, "")
                    .unwrap();
                let element = self
                    .builder
                    .build_load(curried_args[idx], element, "")
                    .unwrap();
                let target = self
                    .builder
                    .build_struct_gep(ret_t, ret, idx as u32 + 1, "")
                    .unwrap();
                self.builder.build_store(target, element).unwrap();
            }
            let target = self
                .builder
                .build_struct_gep(ret_t, ret, idx as u32 + 1, "")
                .unwrap();
            self.builder
                .build_store(target, curr.get_last_param().unwrap())
                .unwrap();
            let ret = self
                .builder
                .build_bit_cast(ret, curry_placeholder, "")
                .unwrap();
            self.builder.build_return(Some(&ret)).unwrap();
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
        mut ast: compiler::typed_ast::TypedModuleDeclaration,
    ) -> (Module<'ctx>, Vec<(TypedTopLevelValue, GlobalValue<'ctx>)>) {
        if self.dibuilder.is_some() {
            let debug_metadata_version = self.ctx.i32_type().const_int(3, false);
            self.module.add_basic_value_flag(
                "Debug Info Version",
                inkwell::module::FlagBehavior::Warning,
                debug_metadata_version,
            )
        }

        let mut ast = dbg!(ast);
        ast.declarations.sort_by_key(|it| match it {
            TypedDeclaration::TypeDefinition(_) => 0,
            TypedDeclaration::Value(_) => 1,
            _ => 2,
        });

        for decl in &ast.declarations {
            self.create_define(decl);
        }
        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.ll");

        let (global_curries, declarations) = {
            let mut split = ast
                .declarations
                .into_iter()
                .into_group_map_by(|decl| match decl {
                    TypedDeclaration::Value(decl) => decl.args.is_empty(),
                    TypedDeclaration::TypeDefinition(_) => false,
                });
            let global_curries = if let Some((_, curries)) = split.remove_entry(&true) {
                curries
            } else {
                Vec::new()
            };
            let others = if let Some((_, others)) = split.remove_entry(&false) {
                others
            } else {
                Vec::new()
            };
            (global_curries, others)
        };

        for decl in declarations.into_iter().filter(|it| match it {
            TypedDeclaration::Value(TypedTopLevelValue { value, .. })
                if value == &TypedValueType::External =>
            {
                false
            }
            _ => true,
        }) {
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
        let globals = global_curries
            .into_iter()
            .map(|value| {
                let TypedDeclaration::Value(value) = value else {
                    unreachable!()
                };
                let gv = self.module.get_global(&value.ident).unwrap();
                (value, gv)
            })
            .collect();
        (self.module.clone(), globals)
    }

    pub(crate) fn replace_module(&mut self, new_module: Module<'ctx>) -> Module<'ctx> {
        std::mem::replace(&mut self.module, new_module)
    }

    pub fn compile_program(
        mut self,
        ast: compiler::typed_ast::ProgramTyped,
        is_lib: bool,
        is_debug: bool,
    ) -> Module<'ctx> {
        if self.module.get_global("()").is_none() {
            self.module
                .add_global(self.ctx.struct_type(&[], false), None, "()");
        }
        let main_name = ast.iter().find_map(|file| {
            file.declarations.iter().find_map(|decl| {
                if let TypedDeclaration::Value(decl) = decl {
                    if (decl.ident.ends_with("::main")
                        && decl.ty
                            == ResolvedType::Function {
                                arg: Box::new(types::UNIT),
                                returns: Box::new(types::UNIT),
                                loc: (0, 0),
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
        let mut globals_to_be_init = Vec::new();
        for file in ast {
            self.current_module = file.name.clone() + ".fb";
            if is_debug {
                let Some(dibuilder) = &self.dibuilder else {
                    unreachable!()
                };
                let difile = dibuilder.create_file(&file.name, "");
                self.difile = Some(difile);
            }
            let (_, values) = self.compile_module(file);
            globals_to_be_init.extend(values);
            self.difile = None
        }

        #[cfg(debug_assertions)]
        let _ = self.module.print_to_file("./debug.ll");
        if !is_lib {
            if let Some(main_name) = main_name {
                let entry = self.module.add_function(
                    "main",
                    self.ctx.void_type().fn_type(&[], false),
                    None,
                );
                let bb = self.ctx.append_basic_block(entry, "");
                self.builder.position_at_end(bb);
                for (value, gv) in globals_to_be_init {
                    let TypedValueType::Expr(expr) = value.value else {
                        unreachable!()
                    };
                    let TypedExpr::FnCall(fun) = &expr else {
                        unreachable!()
                    };
                    let fields = self.fold_arg_ty(fun);
                    let fields = [self.ctx.ptr_type(AddressSpace::default()).into()]
                        .into_iter()
                        .chain(fields)
                        .collect_vec();
                    let ty = self.ctx.struct_type(&fields, false);
                    let ptr_value = self.compile_expr(expr).into_pointer_value();
                    let value = self.builder.build_load(ty, ptr_value, "").unwrap();
                    self.builder.build_store(gv.as_pointer_value(), value);
                    self.builder.build_free(ptr_value);
                }
                let gs = self.module.get_global(&main_name).unwrap();
                let main = self
                    .builder
                    .build_load(
                        self.ctx.ptr_type(AddressSpace::default()),
                        gs.as_pointer_value(),
                        "main",
                    )
                    .unwrap()
                    .into_pointer_value();
                let main_t = self
                    .type_resolver
                    .resolve_type_as_function(&types::UNIT.fn_ty(&types::UNIT));
                self.builder
                    .build_indirect_call(
                        main_t,
                        main,
                        &[
                            gs.as_basic_value_enum().into(),
                            self.module
                                .get_global("()")
                                .unwrap()
                                .as_pointer_value()
                                .into(),
                        ],
                        "",
                    )
                    .unwrap();
                self.builder.build_return(None).unwrap();
            } else {
                panic!("could not find suitable main");
            }
        }
        self.module
    }

    fn compile_match(&mut self, match_: TypedMatch) -> AnyValueEnum<'ctx> {
        let rt = match_.get_ty();
        let TypedMatch { loc, on, mut arms } = match_;
        let current_block = self.builder.get_insert_block().unwrap();

        if let Some(dibuilder) = &self.dibuilder {
            let Some(scope) = &self.difunction else {
                unreachable!()
            };
            let _diloc = dibuilder.create_debug_location(
                self.ctx,
                loc.0 as _,
                loc.1 as _,
                scope.as_debug_info_scope(),
                None,
            );
            // self.builder.set_current_debug_location(diloc);
        }
        let cond_ty = on.get_ty();
        if cond_ty == types::STR {
            unimplemented!("need to implement for strings and enums");
        }
        let on = convert_to_basic_value(self.compile_expr(*on));
        let fun = current_block.get_parent().unwrap();
        let ret_block = self.ctx.append_basic_block(fun, "");
        let cond_blocks = std::iter::once(current_block)
            .chain(std::iter::repeat_with(|| {
                self.ctx.append_basic_block(fun, "")
            }))
            .take(arms.len())
            .collect_vec();
        let values = arms
            .into_iter()
            .zip(&cond_blocks)
            .zip(
                cond_blocks
                    .iter()
                    .skip(1)
                    .chain(std::iter::once(&ret_block)),
            )
            .map(|((arm, cond_block), next_block)| {
                self.compile_arm(arm, fun, &on, &cond_ty, *cond_block, ret_block, *next_block)
            })
            .collect_vec();

        self.builder.position_at_end(ret_block);
        ret_block.move_after(fun.get_last_basic_block().unwrap());
        if !rt.is_void_or_unit() {
            let ty = self.type_resolver.resolve_type_as_basic(rt.clone());
            let phi = self.builder.build_phi(ty, "").unwrap();
            for (block, value) in values {
                let value = value.unwrap();
                let pos = block.get_last_instruction().unwrap();
                self.builder.position_before(&pos);
                let value = self.value_or_load(rt.clone(), value);

                phi.add_incoming(&[(&value, block)]);
            }
            self.builder.position_at_end(ret_block);
            phi.as_any_value_enum()
        } else {
            self.module
                .get_global("()")
                .unwrap_or_else(|| {
                    self.module
                        .add_global(self.ctx.struct_type(&[], false), None, "()")
                })
                .as_pointer_value()
                .as_any_value_enum()
        }
    }

    fn compile_pattern_simple(
        &mut self,
        pat: TypedPattern,
        cond_v: &BasicValueEnum<'ctx>,
        cond_ty: &ResolvedType,
    ) -> IntValue<'ctx> {
        match pat {
            TypedPattern::EnumVariant { variant, .. } if pat.is_simple() => {
                let variant_short = if let Some((_,short)) = variant.rsplit_once("::") {
                    dbg!(short)
                } else {
                    &variant
                };
                let ResolvedType::User { name, .. } = cond_ty else { unreachable!() };
                let discrim = dbg!(dbg!(&self.enum_discrims).get(dbg!(name)).unwrap()).iter().position(|it| it==variant_short).unwrap();
                let value = self.builder.build_struct_gep(self.ctx.struct_type(&[self.ctx.i8_type().into()], false), cond_v.into_pointer_value(), 0, "$discrim").unwrap();
                let value = self.builder.build_load(self.ctx.i8_type(),value,"").unwrap();
                self.builder.build_int_compare(IntPredicate::EQ, value.into_int_value(), self.ctx.i8_type().const_int(discrim as _, false), "").unwrap()
            }
            TypedPattern::Default => self.ctx.bool_type().const_int(1, false),
            TypedPattern::Or(lhs, rhs) => {
                let lhs = self.compile_pattern_simple(*lhs, cond_v, cond_ty);
                let rhs = self.compile_pattern_simple(*rhs, cond_v, cond_ty);
                self.builder.build_or(lhs,rhs,"").unwrap()
            },
            TypedPattern::Const(val, ty) if ty.is_int() => {
                let cond_v = self.value_or_load(cond_ty.clone(), *cond_v).into_int_value();
                let ty = self.type_resolver.resolve_type_as_basic(ty).into_int_type();
                let val = ty.const_int_from_string(&val, inkwell::types::StringRadix::Decimal).unwrap();
                self.builder.build_int_compare(IntPredicate::EQ, cond_v, val, "").unwrap()
            },
            TypedPattern::Const(val, ty) if ty.is_float() => {
                let cond_v = self.value_or_load(cond_ty.clone(), *cond_v).into_float_value();
                let ty = self.type_resolver.resolve_type_as_basic(ty).into_float_type();
                let val = unsafe { ty.const_float_from_string(&val) };
                self.builder.build_float_compare(FloatPredicate::UEQ, cond_v, val, "").unwrap()
            },
            TypedPattern::Destructure(TypedDestructure::Tuple(pats)) => {
                let ResolvedType::Tuple { underlining, .. } = cond_ty else { unreachable!() };
                let tuple_ty = self.type_resolver.resolve_type_as_basic(cond_ty.clone());
                pats.into_iter()
                .zip(underlining)
                .enumerate()
                .map(|(idx,(pat,ty))| {
                    let cond_v = self.builder.build_struct_gep(tuple_ty, cond_v.into_pointer_value(), idx as _, "").unwrap().as_basic_value_enum();
                    self.compile_pattern_simple(pat, &cond_v, ty)
                })
                .collect_vec()
                .into_iter()
                .reduce(|accum,value| {
                    self.builder.build_and(accum,value,"").unwrap()
                })
                .unwrap()
            }
            TypedPattern::Destructure(TypedDestructure::Unit) => self.ctx.bool_type().const_int(1, false),
            _ => panic!("non simple pattern trying to be evalulated as a simple pattern (eg could be a value read or a non-simple destructure) {pat:#?}"),
        }
    }

    fn compile_complex_pattern(
        &mut self,
        pat: TypedPattern,
        fun: FunctionValue<'ctx>,
        cond_v: &BasicValueEnum<'ctx>,
        cond_ty: &ResolvedType,
        curr_block: BasicBlock<'ctx>,
        next_block: BasicBlock<'ctx>,
        bindings_block: BasicBlock<'ctx>,
        bindings_phi: &mut HashMap<String, PhiValue<'ctx>>,
        bindings_to_make: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> BasicBlock<'ctx> {
        if pat.is_simple() {
            let value = self.compile_pattern_simple(pat, cond_v, cond_ty);
            for (name, value) in bindings_to_make {
                let phi = bindings_phi[name];
                phi.add_incoming(&[(value, curr_block)]);
            }
            self.builder
                .build_conditional_branch(value, bindings_block, next_block);
            return curr_block;
        }

        match pat {
            TypedPattern::EnumVariant {
                variant,
                pattern: Some(pat),
                ty,
                ..
            } => {
                let variant_short = if let Some((_, short)) = variant.rsplit_once("::") {
                    dbg!(short)
                } else {
                    &variant
                };
                let success_block = self.ctx.append_basic_block(fun, dbg!(&variant));
                let _ = success_block.move_after(curr_block);
                let ResolvedType::User { name, .. } = cond_ty else {
                    unreachable!()
                };
                let discrim = dbg!(dbg!(&self.enum_discrims).get(dbg!(name)).unwrap())
                    .iter()
                    .position(|it| it == variant_short)
                    .unwrap();
                let value = self
                    .builder
                    .build_struct_gep(
                        self.ctx.struct_type(&[self.ctx.i8_type().into()], false),
                        cond_v.into_pointer_value(),
                        0,
                        "$discrim",
                    )
                    .unwrap();
                let value = self
                    .builder
                    .build_load(self.ctx.i8_type(), value, "")
                    .unwrap();
                let right_variant = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        value.into_int_value(),
                        self.ctx.i8_type().const_int(discrim as _, false),
                        "",
                    )
                    .unwrap();
                let _ =
                    self.builder
                        .build_conditional_branch(right_variant, success_block, next_block);
                self.builder.position_at_end(success_block);
                let ResolvedType::Dependent { actual, ident, .. } = ty else {
                    unreachable!()
                };
                let variant_data_type = self.ctx.get_struct_type(&ident).unwrap();
                let value = self
                    .builder
                    .build_struct_gep(variant_data_type, cond_v.into_pointer_value(), 1, "")
                    .unwrap();
                self.compile_complex_pattern(
                    *pat,
                    fun,
                    &value.into(),
                    &actual,
                    success_block,
                    next_block,
                    bindings_block,
                    bindings_phi,
                    bindings_to_make,
                )
            }
            TypedPattern::EnumVariant { .. } => unreachable!(),
            TypedPattern::Destructure(TypedDestructure::Tuple(conds)) => {
                let ResolvedType::Tuple { underlining, .. } = cond_ty else {
                    unreachable!()
                };
                let tuple_ty = self.type_resolver.resolve_type_as_basic(cond_ty.clone());
                let mut conds = conds
                    .into_iter()
                    .zip(underlining)
                    .enumerate()
                    .map(|(idx, (cond, ty))| (cond, idx, ty))
                    .into_group_map_by(|(cond, _, _)| cond.is_simple());
                let simple = if let Some((_, simple_conds)) = conds.remove_entry(&true) {
                    simple_conds
                        .into_iter()
                        .map(|(pat, idx, ty)| {
                            let value = self
                                .builder
                                .build_struct_gep(
                                    tuple_ty,
                                    cond_v.into_pointer_value(),
                                    idx as _,
                                    "",
                                )
                                .unwrap();
                            self.compile_pattern_simple(pat, &value.as_basic_value_enum(), ty)
                        })
                        .collect_vec()
                        .into_iter()
                        .reduce(|accum, next| self.builder.build_and(accum, next, "").unwrap())
                        .expect("if there is no simple conditions then how did we end up here?")
                } else {
                    self.ctx.bool_type().const_int(1, false)
                };
                let new_block = self.ctx.append_basic_block(fun, "complex");
                new_block.move_after(curr_block);
                self.builder
                    .build_conditional_branch(simple, new_block, next_block);
                self.builder.position_at_end(new_block);
                let mut curr_block = new_block;
                let mut complex = conds
                    .remove_entry(&false)
                    .map(|(_, a)| a)
                    .unwrap_or_else(Vec::new);
                complex.retain(|(pat, idx, ty)| {
                    let value = self
                        .builder
                        .build_struct_gep(tuple_ty, cond_v.into_pointer_value(), *idx as _, "")
                        .unwrap();
                    if let TypedPattern::Read(name, _, _) = pat {
                        bindings_to_make.insert(name.clone(), value.as_basic_value_enum());
                        false
                    } else {
                        true
                    }
                });
                // complex.sort_by_key(|(pat, _, _)| {
                //     match pat {
                //         TypedPattern::Read(_, _, _) => 0,
                //         TypedPattern::Or(_, _) => 2,
                //         // not much should be here as most else will be classed as simple.
                //         _ => 1,
                //     }
                // });
                if complex.is_empty() {
                    for (name, value) in bindings_to_make {
                        let phi = bindings_phi[name];
                        phi.add_incoming(&[(value, curr_block)]);
                    }
                    self.builder.build_unconditional_branch(bindings_block);
                } else {
                    for (pat, idx, ty) in complex {
                        let value = self
                            .builder
                            .build_struct_gep(tuple_ty, cond_v.into_pointer_value(), idx as _, "")
                            .unwrap();
                        let pat = dbg!(pat);
                        if let TypedPattern::Read(name, _, _) = pat {
                            bindings_to_make.insert(name, value.as_basic_value_enum());
                        } else {
                            curr_block = self.compile_complex_pattern(
                                pat,
                                fun,
                                &value.as_basic_value_enum(),
                                ty,
                                curr_block,
                                next_block,
                                bindings_block,
                                bindings_phi,
                                bindings_to_make,
                            );
                        }
                    }
                }
                curr_block
            }
            TypedPattern::Destructure(_) => todo!("other kinds of destructure"),
            TypedPattern::Or(lhs, rhs) => {
                let rhs_block = self.ctx.append_basic_block(fun, "or");
                rhs_block.move_after(curr_block);
                let mut lhs_bindings = bindings_to_make.clone();
                let curr_block = self.compile_complex_pattern(
                    *lhs,
                    fun,
                    cond_v,
                    cond_ty,
                    curr_block,
                    rhs_block,
                    bindings_block,
                    bindings_phi,
                    &mut lhs_bindings,
                );
                for (name, value) in lhs_bindings {
                    bindings_phi[&name].add_incoming(&[(&value, curr_block)]);
                }
                self.builder.build_unconditional_branch(bindings_block);
                self.builder.position_at_end(rhs_block);
                let curr_block = self.compile_complex_pattern(
                    *rhs,
                    fun,
                    cond_v,
                    cond_ty,
                    curr_block,
                    next_block,
                    bindings_block,
                    bindings_phi,
                    bindings_to_make,
                );
                for (name, value) in bindings_to_make {
                    bindings_phi[name].add_incoming(&[(value, curr_block)]);
                }
                self.builder.build_unconditional_branch(bindings_block);
                curr_block
            }
            TypedPattern::Read(name, _, _) => {
                bindings_phi[&name].add_incoming(&[(cond_v, curr_block)]);
                for (name, value) in bindings_to_make {
                    bindings_phi[name].add_incoming(&[(value, curr_block)]);
                }
                self.builder.build_unconditional_branch(next_block);
                curr_block
            }
            TypedPattern::Default => {
                for (name, value) in bindings_to_make {
                    bindings_phi[name].add_incoming(&[(value, curr_block)]);
                }
                self.builder.build_unconditional_branch(next_block);
                curr_block
            }
            // handled by the simple case
            TypedPattern::Const(_, _) => curr_block,
            TypedPattern::Err => unreachable!(),
        }
    }

    fn compile_pattern(
        &mut self,
        pat: TypedPattern,
        fun: FunctionValue<'ctx>,
        cond_v: &BasicValueEnum<'ctx>,
        cond_ty: &ResolvedType,
        curr_block: BasicBlock<'ctx>,
        next_block: BasicBlock<'ctx>,
        bindings_block: BasicBlock<'ctx>,
        bindings_phi: &mut HashMap<String, PhiValue<'ctx>>,
    ) {
        match pat {
            TypedPattern::Default => {
                self.builder.build_unconditional_branch(bindings_block);
            }
            TypedPattern::Const(_, _) => {
                let cond = self.compile_pattern_simple(pat, cond_v, cond_ty);
                self.builder
                    .build_conditional_branch(cond, bindings_block, next_block);
            }
            TypedPattern::Read(name, ty, _) => {
                //todo debug info
                let phi = bindings_phi.entry(name.clone()).or_insert_with(|| {
                    self.builder.position_at_end(bindings_block);
                    let phi = self
                        .builder
                        .build_phi(self.ctx.i8_type().ptr_type(AddressSpace::default()), &name)
                        .unwrap();
                    self.builder.position_at_end(curr_block);
                    phi
                });
                phi.add_incoming(&[(&cond_v.into_pointer_value(), curr_block)]);
            }
            TypedPattern::Err => unreachable!(),
            TypedPattern::Or(lhs, rhs) => {
                let rhs_block = self.ctx.append_basic_block(fun, "or");
                rhs_block.move_after(curr_block);
                self.compile_pattern(
                    *lhs,
                    fun,
                    cond_v,
                    cond_ty,
                    curr_block,
                    rhs_block,
                    bindings_block,
                    bindings_phi,
                );
                self.builder.position_at_end(rhs_block);
                self.compile_pattern(
                    *rhs,
                    fun,
                    cond_v,
                    cond_ty,
                    rhs_block,
                    next_block,
                    bindings_block,
                    bindings_phi,
                );
            }
            TypedPattern::Destructure(TypedDestructure::Tuple(tuple))
                if tuple
                    .iter()
                    .all(|pat| pat.is_simple() || matches!(pat, TypedPattern::Read(_, _, _))) =>
            {
                let mut conds = Vec::with_capacity(tuple.len());
                let ResolvedType::Tuple { underlining, .. } = cond_ty else {
                    unreachable!()
                };
                let tuple_ty = self.type_resolver.resolve_type_as_basic(cond_ty.clone());
                for (idx, (pat, cond_ty)) in tuple.into_iter().zip(underlining).enumerate() {
                    let cond_v = self
                        .builder
                        .build_struct_gep(tuple_ty, cond_v.into_pointer_value(), idx as _, "")
                        .unwrap();
                    if pat.is_simple() {
                        conds.push(self.compile_pattern_simple(
                            pat,
                            &cond_v.as_basic_value_enum(),
                            cond_ty,
                        ))
                    } else {
                        self.compile_pattern(
                            pat,
                            fun,
                            &cond_v.as_basic_value_enum(),
                            cond_ty,
                            curr_block,
                            next_block,
                            bindings_block,
                            bindings_phi,
                        );
                    }
                }
                match conds.len() {
                    0 => self.builder.build_unconditional_branch(bindings_block),
                    1 => self.builder.build_conditional_branch(
                        conds.pop().unwrap(),
                        bindings_block,
                        next_block,
                    ),
                    _ => {
                        let fin = conds
                            .into_iter()
                            .reduce(|accum, cond| self.builder.build_and(accum, cond, "").unwrap())
                            .unwrap();
                        self.builder
                            .build_conditional_branch(fin, bindings_block, next_block)
                    }
                }
                .unwrap();
            }

            _ => {
                let mut bindings = HashMap::new();
                self.compile_complex_pattern(
                    pat,
                    fun,
                    cond_v,
                    cond_ty,
                    curr_block,
                    next_block,
                    bindings_block,
                    bindings_phi,
                    &mut bindings,
                );
            }
        }
    }

    fn compile_arm(
        &mut self,
        arm: TypedMatchArm,
        fun: FunctionValue<'ctx>,
        cond_v: &BasicValueEnum<'ctx>,
        cond_ty: &ResolvedType,
        cond_block: BasicBlock<'ctx>,
        ret_block: BasicBlock<'ctx>,
        next_block: BasicBlock<'ctx>,
    ) -> (BasicBlock<'ctx>, Option<BasicValueEnum<'ctx>>) {
        let TypedMatchArm {
            loc,
            cond,
            block,
            ret,
        } = arm;

        let arm_block = self.ctx.append_basic_block(fun, "arm");
        arm_block.move_after(cond_block);
        self.builder.position_at_end(cond_block);
        if cond.is_simple() && cond != TypedPattern::Default {
            let cond = self.compile_pattern_simple(cond, cond_v, cond_ty);
            self.builder
                .build_conditional_branch(cond, arm_block, next_block);
        } else if cond == TypedPattern::Default {
            self.builder.build_unconditional_branch(arm_block);
        } else {
            let bindings_block = self.ctx.append_basic_block(fun, "arm.bindings");
            bindings_block.move_after(cond_block);
            self.builder.position_at_end(bindings_block);
            let mut bindings_phi = cond
                .get_idents_with_types()
                .into_iter()
                .map(|(name, ty)| {
                    let ty = self.type_resolver.resolve_type_as_basic(ty);
                    let phi = self
                        .builder
                        .build_phi(ty.ptr_type(AddressSpace::default()), &name)
                        .unwrap();
                    (name, phi)
                })
                .collect();
            self.builder.build_unconditional_branch(arm_block).unwrap();
            self.builder.position_at_end(cond_block);
            self.compile_pattern(
                cond,
                fun,
                cond_v,
                cond_ty,
                cond_block,
                next_block,
                bindings_block,
                &mut bindings_phi,
            );
            self.known_values.extend(
                bindings_phi
                    .into_iter()
                    .map(|(name, phi)| (name, phi.as_basic_value())),
            );
        }
        self.builder.position_at_end(arm_block);
        for stmnt in block {
            self.compile_statement(stmnt);
        }
        let ret = ret
            .map(|ret| self.compile_expr(*ret))
            .map(convert_to_basic_value);

        self.builder.build_unconditional_branch(ret_block);
        (arm_block, ret)
    }

    fn value_or_load(
        &mut self,
        expr_ty: ResolvedType,
        value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        if value.is_pointer_value() {
            let ty = self.type_resolver.resolve_type_as_basic(expr_ty);
            self.builder
                .build_load(ty, value.into_pointer_value(), "")
                .unwrap()
        } else {
            value
        }
    }
    fn fold_arg_ty(&mut self, fun: &TypedFnCall) -> Vec<BasicTypeEnum<'ctx>> {
        let TypedFnCall {
            value, arg, arg_t, ..
        } = fun;
        let arg_t = self.type_resolver.resolve_type_as_basic(arg_t.clone());
        if let TypedExpr::FnCall(fun) = value.as_ref() {
            let mut out = self.fold_arg_ty(fun);
            out.push(arg_t);
            out
        } else {
            vec![arg_t]
        }
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

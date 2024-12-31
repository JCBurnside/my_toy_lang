use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use itertools::Itertools;

use crate::{
    ast as untyped_ast,
    types::{self, ResolvedType},
};

pub(crate) mod ast;

pub(crate) struct Context {
    next_unknown_id: usize,
    next_expr_id: usize,
    dependency_tree: HashMap<String, Vec<String>>,
    known_types: HashMap<String, ResolvedType>,
    known_struct_fields: HashMap<(String, String), ResolvedType>,
    _known_generic_types: HashMap<String, untyped_ast::TypeDefinition>,
    known_ops: HashMap<String, Vec<ResolvedType>>,
    known_values: HashMap<String, ResolvedType>,
    known_locals: HashMap<String, ResolvedType>,
    expr_ty: HashMap<usize, ResolvedType>,
    equations: HashMap<usize, ResolvedType>, //? unsure of type here yet.
}
impl Context {
    pub(crate) fn inference(
        &mut self,
        ast: untyped_ast::ModuleDeclaration,
    ) -> ast::ModuleDeclaration {
        self.known_types.extend(ast.get_types());
        let mut ast = self.assign_ids_module(ast);
        self.try_to_infer(&mut ast);
        self.apply_equations(&mut ast);
        ast
    }

    pub(crate) fn new(
        dependency_tree: HashMap<String, Vec<String>>,
        known_types: HashMap<String, ResolvedType>,
        known_struct_fields: HashMap<(String, String), ResolvedType>,
        known_ops: HashMap<String, Vec<ResolvedType>>,
        known_generic_types: HashMap<String, untyped_ast::TypeDefinition>,
    ) -> Self {
        Self {
            next_unknown_id: 0,
            next_expr_id: 0,
            dependency_tree,
            known_types,
            known_struct_fields,
            _known_generic_types: known_generic_types,
            known_ops,
            known_values: HashMap::new(),
            known_locals: HashMap::new(),
            expr_ty: HashMap::new(),
            equations: HashMap::new(),
        }
    }

    fn get_next_expr_id(&mut self) -> usize {
        let id = self.next_expr_id;
        self.next_expr_id += 1;
        id
    }

    fn get_next_type_id(&mut self) -> ResolvedType {
        let id = self.next_unknown_id;
        self.next_unknown_id += 1;
        ResolvedType::Unknown(id)
    }

    // phase 1.  determine certain types or assign unknown id.
    pub(crate) fn assign_ids_module(
        &mut self,
        module: untyped_ast::ModuleDeclaration,
    ) -> ast::ModuleDeclaration {
        let untyped_ast::ModuleDeclaration {
            loc,
            name,
            declarations,
        } = module;

        ast::ModuleDeclaration {
            loc: loc.unwrap_or_default(),
            name,
            decls: declarations
                .into_iter()
                .map(|decl| self.assign_ids_top_level(decl))
                .collect(),
        }
    }
    pub(crate) fn assign_ids_top_level(
        &mut self,
        decl: untyped_ast::TopLevelDeclaration,
    ) -> ast::TopLevelDeclaration {
        match decl {
            untyped_ast::TopLevelDeclaration::TypeDefinition(ty) => {
                ast::TopLevelDeclaration::Type(ty)
            }
            untyped_ast::TopLevelDeclaration::Mod(_) => todo!(),
            untyped_ast::TopLevelDeclaration::Value(untyped_ast::TopLevelValue {
                loc,
                is_op,
                ident,
                args,
                ty,
                value,
                generics,
                abi,
            }) => {
                // ast::Declaration::Value(self.assign_ids_value_decl(v))
                let ty = ty.unwrap_or_else(|| self.get_next_type_id());
                let id = self.get_next_expr_id();
                self.known_values.insert(ident.clone(), ty.clone());
                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(idx, arg)| {
                        let expected = if !ty.is_function() {
                            None
                        } else {
                            Some(ty.get_nth_arg(idx))
                        };
                        self.assign_ids_arg(arg, expected.as_ref())
                    })
                    .collect();
                let value = match value {
                    untyped_ast::ValueType::Expr(expr) => {
                        ast::ValueType::Expr(self.assign_ids_expr(expr, None))
                    }
                    untyped_ast::ValueType::Function(stmnts) => ast::ValueType::Function(
                        stmnts
                            .statements
                            .into_iter()
                            .map(|stmnt| self.assign_ids_stmnt(stmnt))
                            .collect(),
                    ),
                    untyped_ast::ValueType::External => ast::ValueType::External,
                };
                ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                    loc,
                    is_op,
                    ident,
                    args,
                    ty,
                    value,
                    generics,
                    abi,
                    id,
                })
            }
        }
    }
    pub(crate) fn assign_ids_arg(
        &mut self,
        arg: untyped_ast::ArgDeclaration,
        expected_ty: Option<&ResolvedType>,
    ) -> ast::ArgDeclaration {
        match arg {
            untyped_ast::ArgDeclaration::Simple { loc, ident, ty } => {
                let ty = ty
                    .or(expected_ty.cloned())
                    .unwrap_or_else(|| self.get_next_type_id());
                let id = self.get_next_expr_id();
                self.known_locals.insert(ident.clone(), ty.clone());
                ast::ArgDeclaration::Simple { loc, ident, ty, id }
            }
            untyped_ast::ArgDeclaration::DestructureStruct {
                loc,
                struct_ident,
                fields,
                renamed_fields,
            } => {
                ast::ArgDeclaration::DestructureStruct {
                    loc,
                    struct_ident,
                    fields,
                    renamed_fields,
                }; // nothing to rea
                todo!("not sure how to handle this as of yet.");
            }
            untyped_ast::ArgDeclaration::DestructureTuple(contents, ty, loc) => {
                let ty = ty
                    .or(expected_ty.cloned())
                    .unwrap_or_else(|| self.get_next_type_id());
                let contents = if let ResolvedType::Tuple { underlining, .. } = &ty {
                    if contents.len() != underlining.len() {
                        // TODO! generate destructuring error.
                    }
                    underlining
                        .iter()
                        .zip(contents)
                        .map(|(ty, arg)| self.assign_ids_arg(arg, Some(ty)))
                        .collect()
                } else {
                    contents
                        .into_iter()
                        .map(|arg| self.assign_ids_arg(arg, None))
                        .collect()
                };
                ast::ArgDeclaration::DestructureTuple(contents, ty, loc)
            }
            untyped_ast::ArgDeclaration::Discard { loc, ty } => {
                //TODO! generate error of unable to deduce type.
                ast::ArgDeclaration::Discard {
                    loc,
                    ty: ty.or(expected_ty.cloned()).unwrap_or(types::ERROR),
                }
            }
            untyped_ast::ArgDeclaration::Unit { loc, ty } => {
                if let Some(ty) = &ty {
                    if ty != &types::UNIT {
                        //TODO! generate type error.
                    }
                }
                ast::ArgDeclaration::Unit {
                    loc,
                    ty: ty.unwrap_or(types::UNIT),
                }
            }
        }
    }

    pub(crate) fn assign_ids_value_decl(
        &mut self,
        val: untyped_ast::ValueDeclaration,
    ) -> ast::ValueDeclaration {
        let untyped_ast::ValueDeclaration {
            loc,
            is_op,
            target,
            args,
            ty,
            value,
            generictypes,
            abi,
        } = val;
        let decl_ty = ty.unwrap_or_else(|| self.get_next_type_id());
        let target = self.assign_ids_pattern(target, &decl_ty);
        let id = self.get_next_expr_id();

        self.known_values.extend(target.get_idents_with_types());
        let args = args
            .into_iter()
            .enumerate()
            .map(|(idx, arg)| {
                let expected = if !decl_ty.is_function() {
                    None
                } else {
                    Some(decl_ty.get_nth_arg(idx))
                };
                self.assign_ids_arg(arg, expected.as_ref())
            })
            .collect_vec();
        let value = match value {
            untyped_ast::ValueType::Expr(expr) => {
                ast::ValueType::Expr(self.assign_ids_expr(expr, None))
            }
            untyped_ast::ValueType::Function(stmnts) => ast::ValueType::Function(
                stmnts
                    .statements.into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect(),
            ),
            untyped_ast::ValueType::External => ast::ValueType::External,
        };
        ast::ValueDeclaration {
            loc,
            is_op,
            target,
            args,
            ty: decl_ty,
            value,
            generics: generictypes,
            abi,
            id,
        }
    }

    fn assign_ids_block(&mut self, untyped_ast::Block{ statements, implicit_ret} : untyped_ast::Block, expected: Option<ResolvedType>) -> ast::Block {
        let id = self.get_next_expr_id();
        ast::Block {
            statements : statements.into_iter().map(|stmnt| self.assign_ids_stmnt(stmnt)).collect(),
            implicit_ret : implicit_ret.map(|expr| self.assign_ids_expr(*expr,expected.clone()).into()),
            id,
            ret_ty : expected
        }
    }

    fn assign_ids_stmnt(&mut self, stmnt: untyped_ast::Statement) -> ast::Statement {
        match stmnt {
            untyped_ast::Statement::Declaration(decl) => {
                let decl = self.assign_ids_value_decl(decl);

                self.known_locals
                    .extend(decl.target.get_idents_with_types());
                ast::Statement::Declaration(decl)
            }
            untyped_ast::Statement::Return(ret, loc) => {
                ast::Statement::Return(self.assign_ids_expr(ret, None), loc)
            }
            untyped_ast::Statement::FnCall(call) => {
                let rt = self.get_next_type_id();
                ast::Statement::FnCall(self.assign_ids_call(call, rt))
            }
            untyped_ast::Statement::Pipe(_) => unimplemented!(),
            untyped_ast::Statement::IfStatement(if_) => {
                
                let untyped_ast::If {
                    cond,
                    true_branch,
                    else_branch,
                    loc,
                } = if_;
                let cond = self.assign_ids_expr(*cond, Some(types::BOOL));
                let true_branch = self.assign_ids_block(true_branch,Some(types::UNIT));
                
                let else_branch = else_branch.map(|block| self.assign_ids_block(block,Some(types::UNIT)));
                ast::Statement::IfStatement(ast::If {
                    cond: cond.into(),
                    true_branch,
                    else_branch,
                    loc,
                    result:types::UNIT,
                    id : usize::MAX,
                })
                
            }
            untyped_ast::Statement::Match(match_) => {
                ast::Statement::Match(self.assign_ids_match(match_))
            }
            untyped_ast::Statement::Expr(expr) => ast::Statement::Expr(self.assign_ids_expr(expr,None)),
            untyped_ast::Statement::Error => ast::Statement::Error,
        }
    }

    fn assign_ids_match(&mut self, match_: untyped_ast::Match) -> ast::Match {
        let id = self.get_next_expr_id();
        let untyped_ast::Match { loc, on, arms } = match_;
        let on = self.assign_ids_expr(*on, None);
        let on_t = on.get_retty(self);
        let arms = arms
            .into_iter()
            .map(|arm| {
                let untyped_ast::MatchArm {
                    block:untyped_ast::Block{
                        statements:block,
                        implicit_ret:ret,
                    },
                    cond,
                    loc,
                } = arm;
                let block = block
                    .into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect();
                let cond = self.assign_ids_pattern(cond, &on_t);
                let ret = ret.map(|ret| self.assign_ids_expr(*ret, None).into());
                ast::MatchArm {
                    block,
                    ret,
                    cond,
                    loc,
                }
            })
            .collect();
        ast::Match {
            loc,
            on: on.into(),
            arms,
            id,
        }
    }
    fn assign_ids_pattern(
        &mut self,
        pattern: untyped_ast::Pattern,
        on_t: &ResolvedType,
    ) -> ast::Pattern {
        match pattern {
            untyped_ast::Pattern::Default => ast::Pattern::Default,
            untyped_ast::Pattern::ConstNumber(num) => ast::Pattern::ConstNumber(num, on_t.clone()),
            untyped_ast::Pattern::ConstStr(s) => ast::Pattern::ConstStr(s),
            untyped_ast::Pattern::ConstChar(s) => ast::Pattern::ConstChar(s),
            untyped_ast::Pattern::ConstBool(b) => ast::Pattern::ConstBool(b),
            untyped_ast::Pattern::Read(name, loc) => ast::Pattern::Read {
                ident: name,
                loc,
                ty: on_t.clone(),
                id: self.get_next_expr_id(),
            },
            untyped_ast::Pattern::Destructure(destruction) => {
                let inner = match destruction {
                    untyped_ast::PatternDestructure::Struct { base_ty, fields } => {
                        let base_ty = base_ty
                            .map(|base_ty| ResolvedType::User {
                                name: base_ty,
                                generics: Vec::new(),
                                loc: (0, 0),
                            })
                            .unwrap_or(on_t.clone());
                        ast::DestructurePattern::Struct {
                            base_ty,
                            fields: fields
                                .into_iter()
                                .map(|(name, pat)| {
                                    let on_ty = self.get_next_type_id();
                                    (name, self.assign_ids_pattern(pat, &on_ty))
                                })
                                .collect(),
                        }
                    }
                    untyped_ast::PatternDestructure::Tuple(values) => {
                        let id = self.get_next_expr_id();
                        if let ResolvedType::Tuple { underlining, .. } = on_t {
                            let values = values
                                .into_iter()
                                .zip(underlining)
                                .map(|(value, on_t)| self.assign_ids_pattern(value, on_t))
                                .collect();
                            ast::DestructurePattern::Tuple(values, on_t.clone(), id)
                        } else {
                            let values = values
                                .into_iter()
                                .map(|value| {
                                    let on_t = self.get_next_type_id();
                                    self.assign_ids_pattern(value, &on_t)
                                })
                                .collect();
                            ast::DestructurePattern::Tuple(values, on_t.clone(), id)
                        }
                    }
                    untyped_ast::PatternDestructure::Unit => ast::DestructurePattern::Unit,
                };
                ast::Pattern::Destructure(inner)
            }
            untyped_ast::Pattern::EnumVariant {
                ty,
                variant,
                pattern,
                loc,
            } => {
                let variant = if let Some(base) = ty {
                    format!("{base}::{variant}")
                } else {
                    variant
                };
                let ty = self.get_next_type_id();
                let pattern = pattern.map(|pat| self.assign_ids_pattern(*pat, &ty).into());
                ast::Pattern::EnumVariant {
                    ty,
                    variant,
                    pattern,
                    loc,
                }
            }
            untyped_ast::Pattern::Error => ast::Pattern::Err,
            untyped_ast::Pattern::Or(lhs, rhs) => ast::Pattern::Or(
                self.assign_ids_pattern(*lhs, on_t).into(),
                self.assign_ids_pattern(*rhs, on_t).into(),
            ),
        }
    }
    fn assign_ids_call(
        &mut self,
        call: untyped_ast::FnCall,
        result_t: ResolvedType,
    ) -> ast::FnCall {
        let untyped_ast::FnCall {
            loc,
            value,
            arg: Some(arg),
        } = call
        else {
            unreachable!()
        };
        let arg = self.assign_ids_expr(*arg, None);
        let arg_t = arg.get_retty(self).into();
        let value = self.assign_ids_expr(
            *value,
            Some(ResolvedType::Function {
                arg: arg_t,
                returns: result_t.clone().into(),
                loc,
            }),
        );
        ast::FnCall {
            loc,
            value: value.into(),
            arg: arg.into(),
            id: self.get_next_expr_id(),
            returns: result_t,
        }
    }
    fn assign_ids_expr(
        &mut self,
        expr: untyped_ast::Expr,
        expected: Option<ResolvedType>,
    ) -> ast::Expr {
        match expr {
            untyped_ast::Expr::Error => ast::Expr::Error(self.get_next_expr_id()),
            untyped_ast::Expr::NumericLiteral { value } => {
                let ty = if let Some(expected) = expected {
                    if (expected.is_int() && !value.contains('.')) || expected.is_float() {
                        expected
                    } else {
                        self.get_next_type_id()
                    }
                } else {
                    types::NUMBER
                };
                ast::Expr::NumericLiteral {
                    value,
                    id: self.get_next_expr_id(),
                    ty,
                }
            }
            untyped_ast::Expr::StringLiteral(s) => ast::Expr::StringLiteral(s),
            untyped_ast::Expr::CharLiteral(c) => ast::Expr::CharLiteral(c),
            untyped_ast::Expr::UnitLiteral => ast::Expr::UnitLiteral,
            untyped_ast::Expr::Compose { lhs: _, rhs: _ } => todo!(),
            untyped_ast::Expr::BinaryOpCall(op) => {
                let untyped_ast::BinaryOpCall {
                    loc,
                    lhs,
                    rhs,
                    operator,
                } = op;
                let id = self.get_next_expr_id();
                let lhs = self.assign_ids_expr(*lhs, None);
                let rhs = self.assign_ids_expr(*rhs, None);
                ast::Expr::BinaryOpCall(ast::BinaryOpCall {
                    loc,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                    operator,
                    id,
                    result: self.get_next_type_id(),
                })
            }
            untyped_ast::Expr::UnaryOpCall(_) => todo!(),
            untyped_ast::Expr::FnCall(call) => {
                let rt = self.get_next_type_id();
                ast::Expr::FnCall(self.assign_ids_call(call, rt))
            }
            untyped_ast::Expr::ValueRead(value, loc) => {
                let expr_id = self.get_next_expr_id();

                ast::Expr::ValueRead(value, loc, expr_id)
            }
            untyped_ast::Expr::ArrayLiteral { contents, loc } => {
                let id = self.get_next_expr_id();
                ast::Expr::ArrayLiteral {
                    contents: contents
                        .into_iter()
                        .map(|expr| self.assign_ids_expr(expr, None))
                        .collect(),
                    loc,
                    id,
                }
            }
            untyped_ast::Expr::ListLiteral {
                contents: _,
                loc: _,
            } => todo!(),
            untyped_ast::Expr::TupleLiteral { contents, loc } => {
                let id = self.get_next_expr_id();
                let contents = if let Some(ResolvedType::Tuple {
                    underlining,
                    loc: _,
                }) = expected
                {
                    contents
                        .into_iter()
                        .zip(underlining)
                        .map(|(expr, ty)| self.assign_ids_expr(expr, Some(ty)))
                        .collect()
                } else {
                    contents
                        .into_iter()
                        .map(|expr| self.assign_ids_expr(expr, None))
                        .collect()
                };
                ast::Expr::TupleLiteral { contents, loc, id }
            }
            untyped_ast::Expr::StructConstruction(_) => todo!(),
            untyped_ast::Expr::BoolLiteral(value, loc) => {
                ast::Expr::BoolLiteral(value, loc, self.get_next_expr_id())
            }
            untyped_ast::Expr::If(if_) => {
                let id = self.get_next_expr_id();
                let result = self.get_next_type_id();
                let untyped_ast::If {
                    cond,
                    true_branch,
                    else_branch,
                    loc,
                } = if_;
                let cond = self.assign_ids_expr(*cond, Some(types::BOOL)).into();
                
                let ty = self.get_next_type_id();
                let true_branch = self.assign_ids_block(true_branch, Some(ty));
                let else_branch = self.assign_ids_block(else_branch.unwrap(), dbg!(true_branch.ret_ty.clone()));
                ast::Expr::If(ast::If {
                    cond,
                    true_branch,
                    else_branch:dbg!(else_branch).into(),
                    loc,
                    id,
                    result,
                })
            }
            untyped_ast::Expr::Match(match_) => ast::Expr::Match(self.assign_ids_match(match_)),
        }
    }
    fn try_to_infer(&mut self, module: &mut ast::ModuleDeclaration) {
        let ast::ModuleDeclaration {
            loc: _,
            name: _,
            decls,
        } = module;
        let items = self.dependency_tree.keys().cloned().collect();
        let order = sort_on_tree(items, &self.dependency_tree);
        
        decls.sort_by_key(|decl| order.iter().position(|name| name == &decl.get_ident()));
        for decl in decls {
            self.known_locals.clear();
            match decl {
                ast::TopLevelDeclaration::Value(value) => {
                    for arg in &value.args {
                        self.infer_arg(arg)
                    }
                    match &mut value.value {
                        ast::ValueType::Expr(e) => {
                            let mut ty = if value.ty.is_unknown() {
                                None
                            } else {
                                let ty = value.ty.remove_args(value.args.len());
                                Some(ty)
                            };
                            let actual = self.get_actual_type(e, ty.clone(), &mut ty);
                            if value.ty.is_unknown() {
                                value.ty = value
                                    .args
                                    .iter()
                                    .map(|arg| arg.get_ty())
                                    .rev()
                                    .fold(actual, |ty, arg| arg.fn_ty(&ty));

                                //not sure if this the correct way to handle this.
                            }
                        }
                        ast::ValueType::Function(stmnts) => {
                            let mut ty = if value.ty.is_unknown() {
                                None
                            } else {
                                Some(value.ty.remove_args(value.args.len()))
                            };
                            for stmnt in stmnts {
                                self.infer_stmnt(stmnt, &mut ty);
                            }
                            if value.ty.is_unknown() {
                                let fun = value
                                    .args
                                    .iter()
                                    .map(|arg| arg.get_ty())
                                    .reduce(|ty, arg| ty.fn_ty(&arg));
                                value.ty = fun.unwrap().fn_ty(&ty.unwrap_or(types::ERROR));
                                //not sure if this the correct way to handle this.
                            }
                        }
                        ast::ValueType::External => (),
                    }
                    self.known_values
                        .insert(value.ident.clone(), value.ty.clone());
                }
                ast::TopLevelDeclaration::Type(_) => (), //don't think there is anything to be done here.
            }
        }
    }

    fn infer_arg(&mut self, arg: &ast::ArgDeclaration) {
        match arg {
            ast::ArgDeclaration::Simple { ident, ty, .. } => {
                self.known_locals.insert(ident.clone(), ty.clone());
            }
            ast::ArgDeclaration::DestructureTuple(contents, _, _) => {
                for arg in contents {
                    self.infer_arg(arg);
                }
            }
            ast::ArgDeclaration::DestructureStruct {
                loc: _,
                struct_ident,
                fields,
                renamed_fields,
            } => {
                for field in fields {
                    let ty = if let Some(ty) = self
                        .known_struct_fields
                        .get(&(struct_ident.clone(), field.clone()))
                    {
                        ty.clone()
                    } else {
                        types::ERROR
                    };
                    self.known_locals.insert(field.clone(), ty);
                }
                for (old_name, new_name) in renamed_fields {
                    if new_name != "_" {
                        let ty = if let Some(ty) = self
                            .known_struct_fields
                            .get(&(struct_ident.clone(), old_name.clone()))
                        {
                            ty.clone()
                        } else {
                            types::ERROR
                        };
                        self.known_locals.insert(new_name.clone(), ty);
                    }
                }
            }
            ast::ArgDeclaration::Discard { loc, ty } => (),
            ast::ArgDeclaration::Unit { .. } => (),
        }
    }

    fn infer_decl(&mut self, value: &mut ast::ValueDeclaration) {
        for arg in &value.args {
            self.infer_arg(arg);
        }
        match &mut value.value {
            ast::ValueType::Expr(e) => {
                let mut ty = if value.ty.is_unknown() {
                    None
                } else {
                    let ty = value.ty.remove_args(value.args.len());
                    Some(ty)
                };

                let actual = self.get_actual_type(e, ty.clone(), &mut ty);
                if value.ty.is_unknown() {
                    value.ty = value
                        .args
                        .iter()
                        .map(|arg| arg.get_ty())
                        .rev()
                        .fold(actual, |ty, arg| arg.fn_ty(&ty));

                    //not sure if this the correct way to handle this.
                }
                self.add_equation_of_pattern(&mut value.target, &value.ty);
            }
            ast::ValueType::Function(stmnts) => {
                let mut ty = if value.ty.is_unknown() {
                    None
                } else {
                    Some(value.ty.remove_args(value.args.len()))
                };
                for stmnt in stmnts {
                    self.infer_stmnt(stmnt, &mut ty);
                }
                if value.ty.is_unknown() {
                    let fun = value
                        .args
                        .iter()
                        .map(|arg| arg.get_ty())
                        .reduce(|ty, arg| ty.fn_ty(&arg));
                    value.ty = fun.unwrap().fn_ty(&ty.unwrap_or(types::ERROR)); //not sure if this the correct way to handle this.
                }
            }
            ast::ValueType::External => (),
        }
    }

    fn get_actual_type(
        &mut self,
        expr: &mut ast::Expr,
        expected: Option<ResolvedType>,
        fun_ret_ty: &mut Option<ResolvedType>,
    ) -> ResolvedType {
        match expr {
            ast::Expr::TupleLiteral {
                contents,
                loc: _,
                id: _,
            } => {
                let contents = if let Some(ResolvedType::Tuple {
                    underlining,
                    loc: _,
                }) = expected
                {
                    contents
                        .iter_mut()
                        .zip(underlining)
                        .map(|(expr, ty)| self.get_actual_type(expr, Some(ty), fun_ret_ty))
                        .collect()
                } else {
                    contents
                        .iter_mut()
                        .map(|expr| self.get_actual_type(expr, None, fun_ret_ty))
                        .collect()
                };
                ResolvedType::Tuple {
                    underlining: contents,
                    loc: (0, 0),
                }
            }
            ast::Expr::Error(_) => ResolvedType::Error,
            ast::Expr::NumericLiteral {
                value: _,
                id: _,
                ty,
            } => {
                if let Some(ety) = expected {
                    let is_ety_valid = ety.is_float() || ety.is_int() || ety.is_unknown();
                    let is_replacable =
                        ty == &ResolvedType::Number || ty.is_error() || ty.is_unknown();
                    *ty = if is_ety_valid && is_replacable {
                        ety
                    } else {
                        ResolvedType::Error
                    };
                    ty.clone()
                } else {
                    ty.clone()
                }
            }
            ast::Expr::StringLiteral(_) => ResolvedType::Str,
            ast::Expr::CharLiteral(_) => ResolvedType::Char,
            ast::Expr::UnitLiteral => ResolvedType::Unit,
            ast::Expr::BinaryOpCall(ast::BinaryOpCall {
                loc: _,
                lhs,
                rhs,
                operator,
                id: _,
                result,
            }) => {
                let lhs_ty = self.get_actual_type(lhs, None, fun_ret_ty);
                let rhs_ty = self.get_actual_type(rhs, None, fun_ret_ty);
                let args = [lhs_ty, rhs_ty];
                let out = if let Some(possiblities) = self.known_ops.get(operator) {
                    if let Some(ety) = expected {
                        if let Some(result) =
                            possiblities.iter().find(|ty| ty.check_function(&args))
                        {
                            let ResolvedType::Function {
                                arg: lhs_ty,
                                returns: temp,
                                loc: _,
                            } = result
                            else {
                                unreachable!()
                            };
                            let ResolvedType::Function {
                                arg: rhs_ty,
                                returns: result,
                                loc: _,
                            } = temp.as_ref()
                            else {
                                unreachable!()
                            };

                            self.expr_ty
                                .insert(lhs.get_expr_id(), lhs_ty.as_ref().clone());
                            self.expr_ty
                                .insert(rhs.get_expr_id(), rhs_ty.as_ref().clone());
                            if let ResolvedType::Unknown(id) = ety {
                                let result = result.as_ref().clone();
                                self.equations.insert(id, result.clone());
                                result
                            } else if result.as_ref() == &ety {
                                ety
                            } else {
                                types::ERROR
                            }
                        } else {
                            types::ERROR
                        }
                    } else {
                        if let Some(result) =
                            possiblities.iter().find(|ty| ty.check_function(&args))
                        {
                            let ResolvedType::Function {
                                arg: lhs_ty,
                                returns: temp,
                                loc: _,
                            } = result
                            else {
                                unreachable!()
                            };
                            let ResolvedType::Function {
                                arg: rhs_ty,
                                returns: result,
                                loc: _,
                            } = temp.as_ref()
                            else {
                                unreachable!()
                            };

                            self.expr_ty
                                .insert(lhs.get_expr_id(), lhs_ty.as_ref().clone());
                            self.expr_ty
                                .insert(rhs.get_expr_id(), rhs_ty.as_ref().clone());

                            result.as_ref().clone()
                        } else {
                            types::ERROR
                        }
                    }
                } else {
                    types::ERROR
                };
                *result = out.clone();
                out
            }
            ast::Expr::FnCall(fncall) => self.infer_call(fncall, fun_ret_ty),
            ast::Expr::ValueRead(ident, _, _) => {
                if let Some(ty) = dbg!(self.known_locals.get_mut(*dbg!(&ident))) {
                    if let Some(ety) = expected {
                        if let ResolvedType::Unknown(id) = ty {
                            if let Some(new_ty) = self.equations.get(id) {
                                *ty = new_ty.clone();
                            } else {
                                self.equations.insert(*id, ety);
                            }
                        }
                    }
                    ty.clone()
                } else if let Some(ty) = self
                    .known_values
                    .get(ident)
                    .or_else(|| self.known_types.get(ident))
                {
                    if let Some(ety) = expected {
                        if &ety != ty {
                            types::ERROR
                        } else {
                            ety
                        }
                    } else {
                        ty.clone()
                    }
                } else {
                    ResolvedType::Error
                }
            }
            ast::Expr::ArrayLiteral {
                contents,
                loc: _,
                id: _,
            } => {
                let ety = if let Some(types::ResolvedType::Array { underlining, .. }) = &expected {
                    Some(underlining.as_ref().clone())
                } else {
                    None
                };
                let result_tys = contents
                    .iter_mut()
                    .map(|elem| self.get_actual_type(elem, ety.clone(), fun_ret_ty))
                    .collect_vec();
                let underlining = result_tys
                    .into_iter()
                    .reduce(|accum, actual| {
                        let out = if accum == actual || actual.is_generic() || actual.is_unknown() {
                            accum
                        } else if accum == types::ERROR || accum.is_unknown() {
                            actual
                        } else {
                            types::ERROR
                        };
                        out
                    })
                    .expect("no empty array literals");


                if dbg!(&underlining) != &types::ERROR {
                    for elem in contents.iter_mut() {
                        self.get_actual_type(elem, Some(underlining.clone()), fun_ret_ty);
                    }
                }

                let out = types::ResolvedType::Array {
                    underlining: underlining.into(),
                    size: contents.len(),
                };
                // self.known_values.insert(*id, out.clone());
                // TODO? not sure if I use expr ids at this point
                out
            }
            ast::Expr::ListLiteral {
                contents,
                loc: _,
                id: _,
            } => {
                let ety = if let Some(types::ResolvedType::Array { underlining, .. }) = &expected {
                    Some(underlining.as_ref().clone())
                } else {
                    None
                };
                let result_tys = contents
                    .iter_mut()
                    .map(|elem| self.get_actual_type(elem, ety.clone(), fun_ret_ty));
                let _underlining = result_tys
                    .reduce(|accum, actual| {
                        if accum == actual || actual.is_generic() {
                            accum
                        } else {
                            ResolvedType::Error
                        }
                    })
                    .expect("no empty array literals");
                todo!("list type?")
            }
            ast::Expr::StructConstruction(strct) => {
                for (name, (field, _)) in &mut strct.fields {
                    if let Some(ety) = self
                        .known_struct_fields
                        .get(&(strct.ident.clone(), name.clone()))
                        .cloned()
                    {
                        self.get_actual_type(field, Some(ety), fun_ret_ty);
                    }
                }
                self.known_types
                    .get(&strct.ident)
                    .cloned()
                    .unwrap_or(ResolvedType::Error)
            }
            ast::Expr::BoolLiteral(_, _, _) => ResolvedType::Bool,
            ast::Expr::If(ast::If {
                cond,
                true_branch,
                else_branch,
                loc: _,
                id: _,
                result,
            }) => {
                self.get_actual_type(cond, Some(types::BOOL), fun_ret_ty);
                
                self.infer_block(true_branch,fun_ret_ty, expected);
                let mut ety = true_branch.ret_ty.clone().unwrap_or(types::ERROR);
                
                self.infer_block(else_branch.as_mut().unwrap(), fun_ret_ty, Some(dbg!(ety.clone())));

                *result = ety.clone();
                dbg!(ety)
            }
            ast::Expr::Match(match_) => self.handle_match(match_, expected, fun_ret_ty),
        }
    }

    fn handle_match(
        &mut self,
        match_: &mut ast::Match,
        e_ty: Option<ResolvedType>,
        fun_ret_ty: &mut Option<ResolvedType>,
    ) -> ResolvedType {
        let ast::Match {
            loc: _,
            on,
            arms,
            id: _,
        } = match_;
        let on_ty = self.get_actual_type(on, None, fun_ret_ty);
        let mut ety = e_ty.unwrap_or(types::ERROR);
        for (
            idx,
            ast::MatchArm {
                block,
                ret,
                cond,
                loc: _,
            },
        ) in arms.iter_mut().enumerate()
        {
            self.add_equation_of_pattern(cond, &on_ty);
            for stmnt in block {
                self.infer_stmnt(stmnt, fun_ret_ty);
            }
            if let Some(ret) = ret {
                if ety == types::ERROR || ety.is_unknown() {
                    ety = self.get_actual_type(ret, None, fun_ret_ty);
                } else {
                    self.get_actual_type(ret, Some(ety.clone()), fun_ret_ty);
                }
            }
            if let ast::Pattern::EnumVariant { ty, .. } = cond {
            }
        }
        if !ety.is_error() {
            for arm in arms {
                self.add_equation_to_arm(arm, &ety)
            }
        }
        ety
    }

    fn add_equation_of_pattern(&mut self, pattern: &mut ast::Pattern, on_ty: &ResolvedType) {
        match pattern {
            ast::Pattern::Read { ident, loc, ty, id } => {
                //todo? check if ty is unknown first
                *ty = on_ty.clone();
                self.expr_ty.insert(*id, on_ty.clone());
                self.known_locals.insert(ident.clone(), on_ty.clone());
            }
            ast::Pattern::ConstNumber(_num, ty) => {
                //TODO? check for non-number types.
                *ty = on_ty.clone();
            }

            ast::Pattern::Destructure(d) => {
                
                match d {
                    ast::DestructurePattern::Tuple(contents, ty, id) => {
                        if let ResolvedType::Tuple {
                            underlining,
                            loc: _,
                        } = on_ty
                        {
                            self.expr_ty.insert(*id, on_ty.clone());
                            if let ResolvedType::Unknown(idx) = ty {
                                self.equations.insert(*idx, on_ty.clone());
                            } else if ty != on_ty {
                                // TODO! error reporting
                                *ty = types::ERROR;
                            }
                            for (pat, ty) in contents.iter_mut().zip(underlining) {
                                self.add_equation_of_pattern(pat, ty);
                            }
                        } else {
                            *ty = types::ERROR
                            // TODO! error reporting.
                        }
                    }
                    ast::DestructurePattern::Struct { base_ty, fields } => {
                        todo!("need to work on destructuring structures");
                    }
                    ast::DestructurePattern::Unit => (),
                }
            }
            ast::Pattern::Or(lhs, rhs) => {
                self.add_equation_of_pattern(lhs.as_mut(), on_ty);
                self.add_equation_of_pattern(rhs.as_mut(), on_ty);
            }
            ast::Pattern::EnumVariant {
                ty: v_ty,
                variant,
                pattern,
                loc,
            } => {

                let variant = if let ResolvedType::User { name, .. } = v_ty {
                    format!("{}::{}", name, variant)
                } else {
                    // TODO! handle dependent types.
                    // TODO! Fall back to on_ty
                    variant.clone()
                };

                let sub_ty = self.known_types.get(&variant).cloned();
                if let Some(ResolvedType::Dependent { base, .. }) = &sub_ty {
                    if let ResolvedType::Unknown(id) = on_ty {
                        self.equations.insert(*id, base.as_ref().clone());
                    }
                }
                match (pattern, sub_ty) {
                    (Some(pat), Some(ty @ ResolvedType::Dependent { .. })) => {
                        
                        let unwrapped = if let ResolvedType::Dependent { actual, .. } = &ty {
                            actual
                        } else {
                            unreachable!()
                        };
                        self.add_equation_of_pattern(pat, unwrapped.as_ref());
                        if let ResolvedType::Unknown(idx) = v_ty {
                            self.equations.insert(*idx, ty.clone());
                        }
                    }
                    (None, Some(ref ty @ ResolvedType::Dependent { ref actual, ..})) if matches!(actual.as_ref(), ResolvedType::Void) =>{
                        if let ResolvedType::Unknown(idx) = v_ty {
                            self.equations.insert(*idx, ty.clone());
                        }
                    }
                    _ => (), //TODO! handle error reporting
                }
            }
            _ => (),
        }
    }

    fn add_equation_to_arm(&mut self, arm: &mut ast::MatchArm, ty: &ResolvedType) {
        if let Some(ret) = &mut arm.ret {
            if let ResolvedType::Unknown(id) = ret.get_retty(self) {
                self.equations.insert(id, ty.clone());
            }
            self.expr_ty.insert(ret.get_expr_id(), ty.clone());
        }
    }

    fn infer_stmnt(&mut self, stmnt: &mut ast::Statement, fun_ret_ty: &mut Option<ResolvedType>) {
        match stmnt {
            ast::Statement::Error => (),
            ast::Statement::Declaration(value) => {
                self.infer_decl(value);
                self.known_locals
                    .extend(value.target.get_idents_with_types());
            }
            ast::Statement::Return(expr, _) => {
                let ret = self.get_actual_type(expr, fun_ret_ty.clone(), fun_ret_ty);
                if fun_ret_ty.is_none()
                    || fun_ret_ty.as_ref().map(|ty| ty == &types::ERROR).unwrap()
                {
                    *fun_ret_ty = Some(ret)
                }
            }
            ast::Statement::FnCall(fncall) => {
                self.infer_call(fncall, fun_ret_ty);
            }
            ast::Statement::IfStatement(ast::If {
                cond,
                true_branch,
                else_branch,
                loc: _,
                id:_,
                result:_
            }) => {
                self.get_actual_type(cond, Some(types::BOOL), fun_ret_ty);
                self.infer_block(true_branch, fun_ret_ty, None);
                if let Some(else_branch) = else_branch.as_mut() {
                    self.infer_block(else_branch, fun_ret_ty, None);
                }
            }
            ast::Statement::Match(match_) => {
                self.handle_match(match_, None, fun_ret_ty);
            }
            ast::Statement::Expr(expr) => {
                self.get_actual_type(expr, None, fun_ret_ty);
            }
        };
    }

    fn infer_call(
        &mut self,
        fncall: &mut ast::FnCall,
        fun_ret_ty: &mut Option<ResolvedType>,
    ) -> ResolvedType {
        let ast::FnCall {
            value,
            arg,
            returns,
            ..
        } = dbg!(fncall);
        if let ResolvedType::Function {
            arg: arg_t,
            returns: return_t,
            loc: _,
        } = dbg!(self.get_actual_type(value.as_mut(), None, fun_ret_ty))
        {

            println!("successful got the type for the value");
            self.get_actual_type(
                arg.as_mut(),
                if arg_t.is_generic() || arg_t.is_unknown() || arg_t.is_error() {
                    None
                } else {
                    Some(*arg_t)
                },
                fun_ret_ty,
            );
            *returns = return_t.as_ref().clone();
            *return_t
        } else {
            println!("getting the value was unsuccessful");
            ResolvedType::Error
        }
    }

    fn apply_equations(&mut self, module: &mut ast::ModuleDeclaration) {
        
        self.apply_substutions(module);
        let ast::ModuleDeclaration {
            loc: _,
            name: _,
            decls,
        } = module;
        let mut equations = dbg!(self.equations.clone()).into_iter().collect_vec();
        for (id,ty) in &self.equations {
            for equation in &mut equations {
                if id == &equation.0 {
                    continue;
                }
                equation.1.replace_unknown_with(*id,ty.clone());
            }
        }
        
        for decl in decls {
            match decl {
                ast::TopLevelDeclaration::Value(v) => {
                    dbg!(&v.ty);
                    for (id, ty) in &equations {
                        v.ty.replace_unknown_with(*id, ty.clone());
                        dbg!(&v.ty);
                        for arg in &mut v.args {
                            arg.replace_unknown_with(*id, ty.clone());
                        }
                        match &mut v.value {
                            ast::ValueType::Expr(expr) => {
                                self.apply_equation_expr(expr, *id, ty.clone());
                            }
                            ast::ValueType::Function(stmnts) => {
                                for stmnt in stmnts {
                                    if let Some(rt) =
                                        self.apply_equation_stmnt(stmnt, *id, ty.clone())
                                    {
                                        //todo?
                                    }
                                }
                            }
                            ast::ValueType::External => (),
                        }
                    }
                }
                ast::TopLevelDeclaration::Type(_) => (), //Nothing to do.
            }
        }
    }

    fn apply_equation_decl(&self, decl: &mut ast::ValueDeclaration, id: usize, ty: ResolvedType) {
        let ast::ValueDeclaration {
            loc: _,
            is_op: _,
            target,
            args,
            ty: v_ty,
            value,
            generics: _,
            abi: _,
            id: _,
        } = decl;
        v_ty.replace_unknown_with(id, ty.clone());
        self.apply_equation_pattern(target, id, ty.clone());
        for arg in args.iter_mut() {
            arg.replace_unknown_with(id, ty.clone());
        }
        match value {
            ast::ValueType::Expr(expr) => {
                self.apply_equation_expr(expr, id, ty);
            }
            ast::ValueType::Function(stmnts) => {
                for stmnt in stmnts {
                    if let Some(rt) = self.apply_equation_stmnt(stmnt, id, ty.clone()) {
                        if v_ty.is_unknown() || v_ty.is_error() {
                            *v_ty = rt;
                        }
                    }
                }
            }
            ast::ValueType::External => (),
        }
    }

    fn apply_substutions(&mut self, ast: &mut ast::ModuleDeclaration) {
        for (id, sub) in self.expr_ty.clone() {
            let sub = (&id, &sub);
            for decl in &mut ast.decls {
                match decl {
                    ast::TopLevelDeclaration::Value(v) => {
                        for arg in &mut v.args {
                            self.apply_substution_arg(sub, arg);
                        }
                        match &mut v.value {
                            ast::ValueType::Expr(expr) => self.apply_substution_expr(sub, expr),
                            ast::ValueType::Function(stmnts) => {
                                for stmnt in stmnts {
                                    self.apply_substution_statement(sub, stmnt);
                                }
                            }
                            ast::ValueType::External => (),
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    fn apply_substution_arg(
        &mut self,
        sub: (&usize, &ResolvedType),
        arg: &mut ast::ArgDeclaration,
    ) -> bool {
        match arg {
            ast::ArgDeclaration::Simple {
                loc: _,
                ident,
                ty,
                id,
            } if id == sub.0 => {
                *ty = sub.1.clone();
                true
            }
            ast::ArgDeclaration::DestructureTuple(contents, ty, _) => {
                let changed = contents.iter_mut().fold(false, |accum, arg| {
                    self.apply_substution_arg(sub, arg) || accum
                });
                if changed {
                    if let ResolvedType::Unknown(ty_id) = ty {
                        self.equations.insert(
                            *ty_id,
                            ResolvedType::Tuple {
                                underlining: contents
                                    .iter()
                                    .map(ast::ArgDeclaration::get_ty)
                                    .collect(),
                                loc: (0, 0),
                            },
                        );
                    }
                }
                changed
            }
            _ => false,
        }
    }

    fn apply_substution_decl(
        &mut self,
        sub: (&usize, &ResolvedType),
        decl: &mut ast::ValueDeclaration,
    ) {
        let ast::ValueDeclaration {
            value,
            args,
            target,
            ..
        } = decl;
        self.apply_substution_pattern(target, sub);
        for arg in args {
            self.apply_substution_arg(sub, arg);
        }

        match value {
            ast::ValueType::Expr(expr) => self.apply_substution_expr(sub, expr),
            ast::ValueType::Function(stmnts) => {
                for stmnt in stmnts {
                    self.apply_substution_statement(sub, stmnt);
                }
            }
            ast::ValueType::External => (),
        }
    }

    fn apply_substution_statement(
        &mut self,
        sub: (&usize, &ResolvedType),
        stmnt: &mut ast::Statement,
    ) {
        match stmnt {
            ast::Statement::Declaration(v) => self.apply_substution_decl(sub, v),
            ast::Statement::FnCall(call) => self.apply_substution_fncall(sub, call),
            ast::Statement::IfStatement(ast::If {
                cond,
                true_branch,
                else_branch,
                ..
            }) => {
                self.apply_substution_expr(sub, cond.as_mut());
                for stmnt in &mut true_branch.statements {
                    self.apply_substution_statement(sub, stmnt);
                }
                if let Some(ret) = true_branch.implicit_ret.as_mut() {
                    self.apply_substution_expr(sub, ret.as_mut());
                }
                
                if let Some(else_branch) = else_branch.as_mut() {

                    for stmnt in &mut else_branch.statements {
                        self.apply_substution_statement(sub, stmnt);
                    }
                    if let Some(ret) = else_branch.implicit_ret.as_mut() {
                        self.apply_substution_expr(sub, ret.as_mut());
                    }
                }
            }
            ast::Statement::Match(match_) => self.apply_substution_match(sub, match_),
            ast::Statement::Expr(expr)
            | ast::Statement::Return(expr, _) => self.apply_substution_expr(sub, expr),
            ast::Statement::Error => (),
        }
    }

    fn apply_substution_expr(&mut self, (id, ty): (&usize, &ResolvedType), expr: &mut ast::Expr) {
        match expr {
            ast::Expr::NumericLiteral {
                value: _,
                id: eid,
                ty: nty,
            } if eid == id => *nty = ty.clone(),
            ast::Expr::BinaryOpCall(ast::BinaryOpCall {
                lhs,
                rhs,
                id: opid,
                result,
                ..
            }) => {
                if opid == id {
                    *result = ty.clone()
                } else {
                    self.apply_substution_expr((id, ty), lhs.as_mut());
                    self.apply_substution_expr((id, ty), rhs.as_mut());
                }
            }
            ast::Expr::FnCall(call) => self.apply_substution_fncall((id, ty), call),
            ast::Expr::If(ast::If {
                cond,
                true_branch,
                else_branch,
                id: ifid,
                result,
                ..
            }) => {
                if ifid == id {
                    *result = ty.clone();
                } else {
                    self.apply_substution_expr((id, ty), cond.as_mut());
                    for stmnt in &mut true_branch.statements {
                        self.apply_substution_statement((id, ty), stmnt);
                    }
                    self.apply_substution_expr((id, ty), true_branch.implicit_ret.as_mut().unwrap());

                    let else_branch = else_branch.as_mut().unwrap();

                    for stmnt in &mut else_branch.statements {
                        self.apply_substution_statement((id, ty), stmnt);
                    }
                    if let Some(ret) = else_branch.implicit_ret.as_mut() {
                        self.apply_substution_expr((id, ty), ret.as_mut());
                    }
                }
            }
            ast::Expr::Match(match_) => self.apply_substution_match((id, ty), match_),
            _ => (),
        }
    }

    fn apply_substution_match(&mut self, sub: (&usize, &ResolvedType), match_: &mut ast::Match) {
        let ast::Match { on, id, arms, .. } = match_;
        if id != sub.0 {
            self.apply_substution_expr(sub, on.as_mut());
            for ast::MatchArm {
                block, ret, cond, ..
            } in arms
            {
                self.apply_substution_pattern(cond, sub);
                for stmnt in block {
                    self.apply_substution_statement(sub, stmnt);
                }
                if let Some(ret) = ret {
                    self.apply_substution_expr(sub, ret.as_mut())
                }
            }
        }
    }

    fn apply_substution_fncall(&mut self, sub: (&usize, &ResolvedType), call: &mut ast::FnCall) {
        let ast::FnCall {
            value,
            arg,
            id,
            returns,
            ..
        } = call;
        if id == sub.0 {
            *returns = sub.1.clone();
        } else {
            self.apply_substution_expr(sub, value.as_mut());
            self.apply_substution_expr(sub, arg.as_mut());
        }
    }

    fn apply_equation_stmnt(
        &self,
        stmnt: &mut ast::Statement,
        id: usize,
        ty: ResolvedType,
    ) -> Option<ResolvedType> {
        match stmnt {
            ast::Statement::Declaration(decl) => {
                self.apply_equation_decl(decl, id, ty);
                None
            }
            ast::Statement::Return(expr, _) => Some(self.apply_equation_expr(expr, id, ty)),
            ast::Statement::FnCall(fncall) => {
                self.apply_equation_fncall(fncall, id, ty);
                None
            }
            ast::Statement::IfStatement(if_) => {
                let ast::If {
                    cond,
                    true_branch,
                    else_branch,
                    loc: _,
                    id:self_id,
                    result
                } = if_;
                if id == *self_id {
                    *result = ty.clone();
                }
                self.apply_equation_expr(cond.as_mut(), id, ty.clone());
                for stmnt in &mut true_branch.statements {
                    self.apply_equation_stmnt(stmnt, id, ty.clone());
                }
                if let Some(ret) = true_branch.implicit_ret.as_mut() {
                    self.apply_equation_expr(ret.as_mut(),id,ty.clone());
                }
                if let Some(else_branch) = else_branch.as_mut() {

                    for stmnt in &mut else_branch.statements {
                        self.apply_equation_stmnt(stmnt, id, ty.clone());
                    }
                    if let Some(ret) = else_branch.implicit_ret.as_mut() {
                        self.apply_equation_expr(ret.as_mut(),id,ty.clone());
                    }
                }
                None
            }
            ast::Statement::Match(match_) => {
                self.apply_equation_match(match_, id, ty);
                None
            }
            ast::Statement::Expr(expr) => {
                self.apply_equation_expr(expr,id,ty);
                None
            }
            ast::Statement::Error => None, //nothing to do for errors
        }
    }

    fn apply_equation_fncall(
        &self,
        fncall: &mut ast::FnCall,
        id: usize,
        ty: ResolvedType,
    ) -> ResolvedType {
        let ast::FnCall {
            loc: _,
            value,
            arg,
            id: _,
            returns,
        } = fncall;
        let fn_expr = self.apply_equation_expr(value, id, ty.clone());
        self.apply_equation_expr(arg, id, ty.clone());
        if let ResolvedType::Function {
            arg: _,
            returns: fn_ret,
            loc: _,
        } = fn_expr
        {
            if returns.is_unknown() || returns.is_error() {
                *returns = fn_ret.as_ref().clone();
            }
        }
        returns.replace_unknown_with(id, ty);
        returns.clone()
    }

    fn apply_equation_match(
        &self,
        match_: &mut ast::Match,
        id: usize,
        ty: ResolvedType,
    ) -> ResolvedType {
        let ast::Match {
            loc: _,
            on,
            arms,
            id: _,
        } = match_;
        self.apply_equation_expr(on.as_mut(), id, ty.clone());
        let mut ret_ty = types::ERROR;
        for ast::MatchArm {
            block,
            ret,
            cond, //TODO! for DU patterns and binding patterns
            loc: _,
        } in arms
        {
            self.apply_equation_pattern(cond, id, ty.clone());
            for stmnt in block {
                self.apply_equation_stmnt(stmnt, id, ty.clone());
            }
            if let Some(ret) = ret.as_mut() {
                ret_ty = self.apply_equation_expr(ret, id, ty.clone());
            } else if !ret_ty.is_error() {
                *ret = Some(ast::Expr::Error(0).into())
            }
        }
        ret_ty
    }

    fn apply_equation_pattern(&self, pat: &mut ast::Pattern, id: usize, new_ty: ResolvedType) {
        match pat {
            ast::Pattern::Read { ty, .. } | ast::Pattern::ConstNumber(_, ty) => {
                ty.replace_unknown_with(id, new_ty)
            }
            ast::Pattern::Destructure(d) => match d {
                ast::DestructurePattern::Struct { base_ty, fields } => {
                    for field in fields.values_mut() {
                        self.apply_equation_pattern(field, id, new_ty.clone());
                    }
                }
                ast::DestructurePattern::Tuple(patterns, ty, _) => {
                    ty.replace_unknown_with(id, new_ty.clone());
                    for pat in patterns {
                        self.apply_equation_pattern(pat, id, new_ty.clone());
                    }
                }
                ast::DestructurePattern::Unit => (),
            },
            ast::Pattern::Or(lhs, rhs) => {
                self.apply_equation_pattern(lhs.as_mut(), id, new_ty.clone());
                self.apply_equation_pattern(rhs.as_mut(), id, new_ty);
            }
            ast::Pattern::EnumVariant { ty, pattern, .. } => {
                ty.replace_unknown_with(id, new_ty.clone());
                if let Some(pat) = pattern {
                    self.apply_equation_pattern(pat.as_mut(), id, new_ty);
                }
            }
            _ => (),
        }
    }

    fn replace_one_level(&self, expr: &mut ast::Expr, ty: ResolvedType) {
        match expr {
            ast::Expr::NumericLiteral {
                value: _,
                id: _,
                ty: l_ty,
            } => {
                assert!(ty.is_int() || ty == types::NUMBER);
                *l_ty = ty;
            }
            ast::Expr::BinaryOpCall(binop) => binop.result = ty,
            ast::Expr::FnCall(call) => call.returns = ty,
            ast::Expr::If(if_) => if_.result = ty,
            ast::Expr::Match(_) => todo!(),
            ast::Expr::ValueRead(_, _, _) => (), //todo? handle where this could solve a value?
            _ => (),
        }
    }

    fn apply_equation_expr(
        &self,
        expr: &mut ast::Expr,
        id: usize,
        ty: ResolvedType,
    ) -> ResolvedType {
        match expr {
            ast::Expr::TupleLiteral {
                contents,
                loc: _,
                id: eid,
            } => {
                if id == *eid {
                    let ResolvedType::Tuple {
                        underlining,
                        loc: _,
                    } = &ty
                    else {
                        return types::ERROR;
                    };
                    //TODO? do I need to do something here?
                    ty
                } else {
                    ResolvedType::Tuple {
                        underlining: contents
                            .iter_mut()
                            .map(|expr| self.apply_equation_expr(expr, id, ty.clone()))
                            .collect(),
                        loc: (0, 0),
                    }
                }
            }
            ast::Expr::NumericLiteral {
                value: _,
                id: _,
                ty: l_ty,
            } => {
                l_ty.replace_unknown_with(id, ty);
                if l_ty.is_unknown() {
                    types::NUMBER
                } else {
                    l_ty.clone()
                }
            }

            ast::Expr::BinaryOpCall(ast::BinaryOpCall {
                loc: _,
                lhs,
                rhs,
                operator,
                id: _,
                result,
            }) => {
                let lhs_t = self.apply_equation_expr(lhs.as_mut(), id, ty.clone());
                let rhs_t = self.apply_equation_expr(rhs.as_mut(), id, ty.clone());
                let args = [lhs_t, rhs_t];
                *result = if let Some(possiblities) = self.known_ops.get(operator) {
                    if let Some(result) = possiblities.iter().find(|ty| ty.check_function(&args)) {
                        let ResolvedType::Function {
                            arg: lhs_t,
                            returns,
                            loc: _,
                        } = result
                        else {
                            unreachable!()
                        };
                        let ResolvedType::Function {
                            arg: rhs_t,
                            returns,
                            loc: _,
                        } = returns.as_ref()
                        else {
                            unreachable!()
                        };
                        self.replace_one_level(lhs.as_mut(), lhs_t.as_ref().clone());
                        self.replace_one_level(rhs.as_mut(), rhs_t.as_ref().clone());
                        returns.as_ref().clone()
                    } else {
                        types::ERROR
                    }
                } else {
                    types::ERROR
                };
                result.clone()
            }
            ast::Expr::FnCall(fncall) => self.apply_equation_fncall(fncall, id, ty),
            ast::Expr::ArrayLiteral {
                contents,
                loc: _,
                id: _,
            } => {
                let result_tys = contents
                    .iter_mut()
                    .map(|expr| self.apply_equation_expr(expr, id, ty.clone()))
                    .collect_vec();

                let underlining = if let Some(first_ty) = result_tys
                    .iter()
                    .find(|it| !it.is_error() && !it.is_unknown())
                {
                    for expr in contents.iter_mut() {
                        self.replace_one_level(expr, first_ty.clone());
                    }
                    first_ty.clone()
                } else {
                    types::ERROR
                };

                ResolvedType::Array {
                    underlining: underlining.into(),
                    size: contents.len(),
                }
            }
            ast::Expr::ListLiteral {
                contents,
                loc: _,
                id: _,
            } => {
                let underlining = contents.iter_mut().fold(types::UNIT, |_, expr| {
                    self.apply_equation_expr(expr, id, ty.clone())
                });
                ResolvedType::User {
                    name: "List".to_string(),
                    generics: vec![underlining],
                    loc: (0, 0),
                }
            }
            ast::Expr::StructConstruction(_) => todo!(),
            ast::Expr::If(ast::If {
                cond,
                true_branch,
                else_branch,
                loc: _,
                id: _,
                result,
            }) => {
                self.apply_equation_expr(cond.as_mut(), id, ty.clone());
                true_branch.ret_ty.as_mut().unwrap().replace_unknown_with(id, ty.clone());
                for stmnt in &mut true_branch.statements {
                    self.apply_equation_stmnt(stmnt, id, ty.clone());
                }
                let true_result = self.apply_equation_expr(true_branch.implicit_ret.as_mut().unwrap().as_mut(), id, ty.clone());
                if result.is_unknown() || result.is_error() {
                    *result = true_result;
                }
                let else_branch = dbg!(else_branch.as_mut().unwrap());
                
                else_branch.ret_ty.as_mut().unwrap().replace_unknown_with(id, ty.clone());
                for stmnt in &mut else_branch.statements {
                    self.apply_equation_stmnt(stmnt, id, ty.clone());
                }
                let else_result = self.apply_equation_expr(else_branch.implicit_ret.as_mut().unwrap().as_mut(), id, ty.clone());
                if result.is_unknown() || result.is_error() {
                    *result = else_result;
                }
                result.replace_unknown_with(id, ty);
                result.clone()
            }
            ast::Expr::Match(match_) => self.apply_equation_match(match_, id, ty),
            ast::Expr::BoolLiteral(_, _, _) => types::BOOL,
            ast::Expr::StringLiteral(_) => types::STR,
            ast::Expr::CharLiteral(_) => types::CHAR,
            ast::Expr::ValueRead(v, _, _) => self
                .known_locals
                .get(v)
                .or_else(|| self.known_values.get(v))
                .cloned()
                .unwrap_or(types::ERROR),
            ast::Expr::UnitLiteral => types::UNIT,
            ast::Expr::Error(_) => types::ERROR, //nothing to do
        }
    }

    fn apply_substution_pattern(
        &self,
        cond: &mut ast::Pattern,
        (eid, new_ty): (&usize, &ResolvedType),
    ) {
        match cond {
            ast::Pattern::Read { id, ty, .. } if id == eid => *ty = new_ty.clone(),
            ast::Pattern::Destructure(d) => match d {
                ast::DestructurePattern::Tuple(patterns, ty, id) => {
                    for pat in patterns {
                        self.apply_substution_pattern(pat, (eid, new_ty));
                        if id == eid {
                            *ty = new_ty.clone()
                        }
                    }
                }
                ast::DestructurePattern::Struct { base_ty, fields } => {
                    for field in fields.values_mut() {
                        self.apply_substution_pattern(field, (eid, new_ty))
                    }
                }
                ast::DestructurePattern::Unit => (),
            },
            ast::Pattern::Err => (),
            ast::Pattern::Or(lhs, rhs) => {
                self.apply_substution_pattern(lhs.as_mut(), (eid, new_ty));
                self.apply_substution_pattern(rhs.as_mut(), (eid, new_ty));
            }
            ast::Pattern::EnumVariant {
                ty, pattern, loc, ..
            } => {
                if let Some(pat) = pattern {
                    self.apply_substution_pattern(pat.as_mut(), (eid, new_ty));
                }
            }
            _ => (),
        }
    }

fn infer_block(&mut self, block: &mut ast::Block, fun_ret_ty:&mut Option<ResolvedType>, expected: Option<ResolvedType>) {
    let ast::Block {
        statements,
        implicit_ret,
        id:_,
        ret_ty,
    } = block;
    let statements = dbg!(statements);
    let implicit_ret = dbg!(implicit_ret);
    for stmnt in statements {
        self.infer_stmnt(stmnt, fun_ret_ty);
    }
    if let Some(ret) = implicit_ret.as_mut() {
        let ty = dbg!(self.get_actual_type(dbg!(ret.as_mut()), dbg!(expected), fun_ret_ty));
        if let Some(ret_ty) = ret_ty.as_mut() {
            if let ResolvedType::Unknown(idx) = *ret_ty {
                println!("[line:1987]replacing ret_ty {} with {}", idx,ty.to_string());
                *ret_ty = ty.clone();
                self.equations.insert(idx, dbg!(ty));
            } else if ret_ty == &types::ERROR {
                println!("[line:1990]replacing ret_ty {} with {}", ret_ty.to_string(),ty.to_string());
            
                *ret_ty = ty;
            }
            
        } else {
            println!("[line:1991]replacing ret_ty {} with {}", ret_ty.as_ref().map(|it|it.to_string()).unwrap_or("None".into()),ty.to_string());
            ret_ty.replace(ty);
        }
    } else {
        if expected.is_some() {
            println!("Expected type but no implicit ret. possibly warn and check for unconditional branching");
            implicit_ret.replace(ast::Expr::Error(usize::MAX).into());
        }
    }
}

    
}

fn sort_on_tree(src: Vec<String>, dependencies: &HashMap<String, Vec<String>>) -> Vec<String> {
    let mut sorted = Vec::with_capacity(src.len());
    let mut visited = HashSet::with_capacity(src.len());
    for item in src {
        visit(item, &mut visited, &mut sorted, dependencies);
    }
    sorted
}

fn visit(
    item: String,
    visited: &mut HashSet<String>,
    sorted: &mut Vec<String>,
    dependencies: &HashMap<String, Vec<String>>,
) {
    if !visited.contains(&item) {
        visited.insert(item.clone());
        if let Some(deps) = dependencies.get(&item) {
            for dep in deps {
                visit(dep.clone(), visited, sorted, dependencies)
            }
        }
        sorted.push(item)
    } else {
        if !sorted.contains(&item) {
            panic!("cylic");
        }
    }
}

#[cfg(test)]
impl Context {
    fn reset(&mut self) {
        self.next_expr_id = 0;
        self.next_unknown_id = 0;
    }
}
#[cfg(test)]
mod tests {
    use crate::{
        inference::ast::TopLevelValue,
        parser::file,
        types::{self, ResolvedType},
    };
    use pretty_assertions::assert_eq;
    use std::collections::HashMap;

    #[test]
    fn phase1() {
        // let foo (a:int32) = a
        let ast = super::untyped_ast::ModuleDeclaration {
            loc: None,
            name: "foo".to_string(),
            declarations: vec![super::untyped_ast::TopLevelDeclaration::Value(
                super::untyped_ast::TopLevelValue {
                    loc: (0, 0),
                    is_op: false,
                    ident: "foo".to_owned(),
                    args: vec![super::untyped_ast::ArgDeclaration::Simple {
                        loc: (0, 0),
                        ident: "a".to_string(),
                        ty: Some(types::INT32),
                    }],
                    ty: None,
                    value: super::untyped_ast::ValueType::Expr(
                        super::untyped_ast::Expr::ValueRead("a".to_string(), (0, 0)),
                    ),
                    generics: None,
                    abi: None,
                },
            )],
        };
        let mut ctx = super::Context::new(
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        assert_eq!(
            ctx.assign_ids_module(ast),
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::TopLevelDeclaration::Value(
                    super::ast::TopLevelValue {
                        loc: (0, 0),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration::Simple {
                            loc: (0, 0),
                            ident: "a".to_string(),
                            ty: types::INT32,
                            id: 1
                        }],
                        ty: types::ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::ValueRead(
                            "a".to_string(),
                            (0, 0),
                            2
                        )),
                        generics: None,
                        abi: None,
                        id: 0
                    }
                )]
            },
            "super simple.  `let foo (a:int32) = a"
        );
        ctx.reset();
        let ast =
            file("foo", r"let foo a : bool -> int32 = if a then 0 else 1;");
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::TopLevelDeclaration::Value(
                    super::ast::TopLevelValue {
                        loc: (4,7),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration::Simple {
                            loc: (8,9),
                            ident: "a".to_string(),
                            ty: types::BOOL,
                            id: 1
                        }],
                        ty: types::BOOL.fn_ty(&types::INT32),
                        value: super::ast::ValueType::Expr(super::ast::Expr::If(
                            super::ast::If {
                                cond: super::ast::Expr::ValueRead("a".to_string(), (31,32), 3)
                                    .into(),
                                true_branch: super::ast::Block{
                                    statements:Vec::new(),
                                    implicit_ret:Some(super::ast::Expr::NumericLiteral {
                                        value: "0".to_string(),
                                        id: 5,
                                        ty: ResolvedType::Unknown(2),
                                    }.into()),
                                    id:4,
                                    ret_ty:ResolvedType::Unknown(1).into(),
                                }.into(),
                                else_branch: super::ast::Block {
                                    statements:Vec::new(),
                                    implicit_ret:Some(super::ast::Expr::NumericLiteral {
                                        value: "1".to_string(),
                                        id:7,
                                        ty: ResolvedType::Unknown(3),
                                    }.into()),
                                    id:6,
                                    ret_ty:ResolvedType::Unknown(1).into(),
                                }
                                .into(),
                                loc: (28,30),
                                id: 2,
                                result: ResolvedType::Unknown(0),
                            }
                        )),
                        generics: None,
                        abi: None,
                        id: 0
                    }
                )]
            },
            ctx.assign_ids_module(ast),
            "with if expr",
        );
        ctx.reset();
        let ast = file("foo",
            r"
let foo a = match a   where
    | 0 -> 'a',
    | 1 -> 'b',
    | 2 -> 'c',
    | _ -> 'd',
",
        );
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::TopLevelDeclaration::Value(
                    super::ast::TopLevelValue {
                        loc: (5,8),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration::Simple {
                            loc: (9,10),
                            ident: "a".to_string(),
                            ty: types::ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: types::ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::Match(
                            super::ast::Match {
                                loc: (13,18),
                                on: super::ast::Expr::ValueRead("a".to_string(), (19,20), 3).into(),
                                arms: vec![
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("a".to_string()).into()
                                        ),
                                        cond: super::ast::Pattern::ConstNumber(
                                            "0".to_string(),
                                            ResolvedType::Unknown(2),
                                        ),
                                        loc: (33,34)
                                    },
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("b".to_string()).into()
                                        ),
                                        cond: super::ast::Pattern::ConstNumber(
                                            "1".to_string(),
                                            ResolvedType::Unknown(2),
                                        ),
                                        loc: (49,50)
                                    },
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("c".to_string()).into()
                                        ),
                                        cond: super::ast::Pattern::ConstNumber(
                                            "2".to_string(),
                                            ResolvedType::Unknown(2),
                                        ),
                                        loc: (65,66)
                                    },
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("d".to_string()).into()
                                        ),
                                        cond: super::ast::Pattern::Default,
                                        loc: (81,82)
                                    },
                                ],
                                id: 2
                            }
                        )),
                        generics: None,
                        abi: None,
                        id: 0,
                    }
                )]
            },
            ctx.assign_ids_module(ast),
        );
        ctx.reset();
        let ast = file("foo","let foo a = a == 3;");
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::TopLevelDeclaration::Value(
                    super::ast::TopLevelValue {
                        loc: (4,7),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration::Simple {
                            loc: (8,9),
                            ident: "a".to_string(),
                            ty: ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::BinaryOpCall(
                            super::ast::BinaryOpCall {
                                loc: (14,16),
                                lhs: super::ast::Expr::ValueRead("a".to_string(), (12,13), 3)
                                    .into(),
                                rhs: super::ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                    id: 4,
                                    ty: types::NUMBER,
                                }
                                .into(),
                                operator: "==".to_string(),
                                id: 2,
                                result: types::ResolvedType::Unknown(2),
                            }
                        )),
                        generics: None,
                        abi: None,
                        id: 0
                    }
                )]
            },
            ctx.assign_ids_module(ast),
            "binary op"
        );
    }


    #[test]
    fn finale() {
        const SRC: &'static str = r#" 


let annotated_arg (x:int32) = [x,1,2,3];

let complex x =
    print_int32 x;
    return if x == 0 then [x,0,0,0] else annotated_arg x;
"#;
        // should result in the same as
        /*
        for<T> simple x : T -> T = x

        let annotated_arg x : int32 -> [int32;4] = [x,1,2,3]

        let complex x : int32 -> [int32;4] =
            print_int32 x;
            if x == 0 then
                [0,0,0,0]
            else
                annoated_arg x
        */

        let ast = file("foo",SRC);
        let dtree=ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut ctx = super::Context::new(
            dtree,
            [("print_int32".to_string(), types::INT32.fn_ty(&types::UNIT))].into(),
            HashMap::new(),
            [(
                "==".to_string(),
                vec![types::INT32.fn_ty(&types::INT32.fn_ty(&types::BOOL))],
            )]
            .into(),
            HashMap::new(),
        );
        let mut ast = ctx.assign_ids_module(ast);
        ctx.try_to_infer(&mut ast);
        dbg!(&ctx.equations);
        ctx.apply_equations(&mut ast);
        ast.decls.sort_by_key(|it| it.get_ident());
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![
                    super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                        loc: (8,21),
                        is_op: false,
                        ident: "annotated_arg".to_string(),
                        args: vec![super::ast::ArgDeclaration::Simple {
                            loc: (23,24),
                            ident: "x".to_string(),
                            ty: types::INT32,
                            id: 1
                        },],
                        ty: ResolvedType::Function {
                            arg: types::INT32.into(),
                            returns: ResolvedType::Array {
                                underlining: types::INT32.into(),
                                size: 4
                            }
                            .into(),
                            loc: (0, 0)
                        },
                        value: super::ast::ValueType::Expr(super::ast::Expr::ArrayLiteral {
                            contents: vec![
                                super::ast::Expr::ValueRead("x".to_string(), (35,36), 3),
                                super::ast::Expr::NumericLiteral {
                                    value: "1".to_string(),
                                    id: 4,
                                    ty: types::INT32
                                },
                                super::ast::Expr::NumericLiteral {
                                    value: "2".to_string(),
                                    id: 5,
                                    ty: types::INT32
                                },
                                super::ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                    id: 6,
                                    ty: types::INT32
                                },
                            ],
                            loc: (34,43),
                            id: 2
                        }),
                        generics: None,
                        abi: None,
                        id: 0
                    }),
                    super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                        loc: (50,57),
                        is_op: false,
                        ident: "complex".to_string(),
                        args: vec![super::ast::ArgDeclaration::Simple {
                            loc: (58,59),
                            ident: "x".to_string(),
                            ty: types::INT32,
                            id: 8
                        },],
                        ty: types::INT32.fn_ty(&ResolvedType::Array {
                            underlining: types::INT32.into(),
                            size: 4
                        }),
                        value: super::ast::ValueType::Function(vec![
                            super::ast::Statement::Expr(super::ast::Expr::FnCall(super::ast::FnCall {
                                loc: (66, 77),
                                value: super::ast::Expr::ValueRead(
                                    "print_int32".to_string(),
                                    (66, 77),
                                    10
                                )
                                .into(),
                                arg: super::ast::Expr::ValueRead("x".to_string(), (78,79), 9)
                                    .into(),
                                id: 11,
                                returns: types::UNIT
                            })),
                            super::ast::Statement::Return(
                                super::ast::Expr::If(super::ast::If {
                                    loc: (92,94),
                                    cond: super::ast::Expr::BinaryOpCall(
                                        super::ast::BinaryOpCall {
                                            loc: (97,99),
                                            lhs: super::ast::Expr::ValueRead(
                                                "x".to_string(),
                                                (95,96),
                                                14
                                            )
                                            .into(),
                                            rhs: super::ast::Expr::NumericLiteral {
                                                value: "0".to_string(),
                                                id: 15,
                                                ty: types::INT32
                                            }
                                            .into(),
                                            operator: "==".to_string(),
                                            id: 13,
                                            result: types::BOOL
                                        }
                                    )
                                    .into(),
                                    true_branch: super::ast::Block{
                                        statements:Vec::new(),
                                        implicit_ret:Some(super::ast::Expr::ArrayLiteral {
                                            contents: vec![
                                                super::ast::Expr::ValueRead(
                                                    "x".to_string(),
                                                    (108,109),
                                                    18
                                                ),
                                                super::ast::Expr::NumericLiteral {
                                                    value: "0".to_string(),
                                                    id: 19,
                                                    ty: types::INT32
                                                },
                                                super::ast::Expr::NumericLiteral {
                                                    value: "0".to_string(),
                                                    id: 20,
                                                    ty: types::INT32
                                                },
                                                super::ast::Expr::NumericLiteral {
                                                    value: "0".to_string(),
                                                    id: 21,
                                                    ty: types::INT32
                                                },
                                            ],
                                            loc: (107,116),
                                            id: 17
                                        }
                                        .into()),
                                        id:16,
                                        ret_ty:Some(ResolvedType::Array{
                                            underlining:types::INT32.into(),
                                            size:4,
                                        })
                                    },
                                    else_branch: super::ast::Block {
                                        statements:Vec::new(),
                                        implicit_ret:Some(super::ast::Expr::FnCall(super::ast::FnCall {
                                            loc: (122,135),
                                            value: super::ast::Expr::ValueRead(
                                                "annotated_arg".to_string(),
                                                (122,135),
                                                24
                                            )
                                            .into(),
                                            arg: super::ast::Expr::ValueRead(
                                                "x".to_string(),
                                                (136,137),
                                                23
                                            )
                                            .into(),
                                            id: 25,
                                            returns: ResolvedType::Array {
                                                underlining: types::INT32.into(),
                                                size: 4
                                            }
                                        })
                                        .into()),
                                        id:22,
                                        ret_ty:Some(ResolvedType::Array{
                                            underlining:types::INT32.into(),
                                            size:4,
                                        })
                                    }.into(),
                                    id: 12,
                                    result: ResolvedType::Array {
                                        underlining: types::INT32.into(),
                                        size: 4
                                    }
                                }),
                                (85,91)
                            )
                        ]),
                        generics: None,
                        abi: None,
                        id: 7
                    }),
                ]
            },
            ast,
        )
    }

    #[test]
    fn type_bindings() {
        const SRC: &'static str = "
let int_unit _ : int32 -> () = ();

let unit_int _ : () -> int16 = 0;

let int_int x : int32 -> int32 = x;

let unit_unit _ : () -> () = ();
";

        let module = file("test",SRC);

        let dtree = [
            ("int_unit".to_string(), Vec::new()),
            ("unit_int".to_string(), Vec::new()),
            ("int_int".to_string(), Vec::new()),
            ("unit_unit".to_string(), Vec::new()),
        ]
        .into();

        let mut ctx = super::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );

        let mut module = ctx.inference(module);

        module
            .decls
            .sort_by_key(super::ast::TopLevelDeclaration::get_ident);
        let [int_int, int_unit, unit_int, unit_unit] = &module.decls[..] else {
            unreachable!()
        };
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (76,83),
                is_op: false,
                ident: "int_int".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (84,85),
                    ident: "x".to_string(),
                    ty: types::INT32,
                    id: 4
                }],
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (94,96)
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::ValueRead(
                    "x".to_string(),
                    (105,106),
                    5
                )),
                generics: None,
                abi: None,
                id: 3
            }),
            int_int,
            "int_int"
        );
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (5,13),
                is_op: false,
                ident: "int_unit".to_string(),
                args: vec![super::ast::ArgDeclaration::Discard {
                    loc: (14,15),
                    ty: types::INT32,
                }],
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::UNIT.into(),
                    loc: (24,26),
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
                id: 0
            }),
            int_unit,
            "int_unit"
        );
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (41,49),
                is_op: false,
                ident: "unit_int".to_string(),
                args: vec![super::ast::ArgDeclaration::Discard {
                    loc: (50,51),
                    ty: types::UNIT,
                }],
                ty: ResolvedType::Function {
                    arg: types::UNIT.into(),
                    returns: types::INT16.into(),
                    loc: (57,59)
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::NumericLiteral {
                    value: "0".to_string(),
                    id: 2,
                    ty: types::INT16
                }),
                generics: None,
                abi: None,
                id: 1
            }),
            unit_int,
            "unit_int"
        );
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(TopLevelValue {
                loc: (113,122),
                is_op: false,
                ident: "unit_unit".to_string(),
                args: vec![super::ast::ArgDeclaration::Discard {
                    loc: (123,124),
                    ty: types::UNIT,
                }],
                ty: ResolvedType::Function {
                    arg: types::UNIT.into(),
                    returns: types::UNIT.into(),
                    loc: (130,132)
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
                id: 6
            }),
            unit_unit,
            "unit_unit"
        );
    }

    #[test]
    fn if_expr() {
        const SRC: &'static str = "
let if_expr a b : bool -> int32 -> int32 = if a then b else 0;
";

        let module = file("",SRC);
        let mut ctx = super::Context::new(
            [("if_expr".to_string(), Vec::new())].into(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = ctx.inference(module);

        let [if_expr] = &module.decls[..] else {
            unreachable!()
        };
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (5,12),
                is_op: false,
                ident: "if_expr".to_string(),
                args: vec![
                    super::ast::ArgDeclaration::Simple {
                        loc: (13,14),
                        ident: "a".to_string(),
                        ty: types::BOOL,
                        id: 1,
                    },
                    super::ast::ArgDeclaration::Simple {
                        loc: (15,16),
                        ident: "b".to_string(),
                        ty: types::INT32,
                        id: 2,
                    },
                ],
                ty: ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (33,35)
                    }
                    .into(),
                    loc: (24,26)
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::If(super::ast::If {
                    cond: super::ast::Expr::ValueRead("a".to_string(), (47,48), 4).into(),
                    true_branch: super::ast::Block {
                        statements:Vec::new(),
                        implicit_ret:Some(super::ast::Expr::ValueRead("b".to_string(), (54,55), 6).into()),
                        id:5,
                        ret_ty:types::INT32.into(),
                    },
                    else_branch: super::ast::Block {
                        statements:Vec::new(),
                        implicit_ret:Some(super::ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                            id: 8,
                            ty: types::INT32
                        }
                        .into()),
                        id:7,
                        ret_ty:types::INT32.into()
                    }.into(),
                    loc: (44,46),
                    id: 3,
                    result: types::INT32
                })),
                generics: None,
                abi: None,
                id: 0,
            }),
            if_expr,
        )
    }

    #[test]
    fn  returns() {
        const SRC: &'static str = "
let returns a : bool -> int32 =
    if a then
        return 0;
    return 1;
";

        let module = file("",SRC);
        let mut ctx = super::Context::new(
            [("returns".to_string(), Vec::new())].into(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = ctx.inference(module);

        let [returns] = &module.decls[..] else {
            unreachable!()
        };
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (5,12),
                is_op: false,
                ident: "returns".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (13, 14),
                    ident: "a".to_string(),
                    ty: types::BOOL,
                    id: 1,
                },],
                ty: ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: types::INT32.into(),
                    loc: (22,24)
                },
                value: super::ast::ValueType::Function(vec![
                    super::ast::Statement::IfStatement(super::ast::If {
                        cond: super::ast::Expr::ValueRead("a".to_string(), (40,41), 2).into(),
                        true_branch: super::ast::Block {
                            statements:vec![super::ast::Statement::Return(
                                super::ast::Expr::NumericLiteral {
                                    value: "0".to_string(),
                                    id: 4,
                                    ty: types::INT32
                                },
                                (55,61)
                            )],
                            implicit_ret:None,
                            id:3,
                            ret_ty:Some(types::UNIT),
                        },
                        else_branch: None,
                        loc: (37,39),
                        id:usize::MAX,
                        result:types::UNIT,
                    }),
                    super::ast::Statement::Return(
                        super::ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                            id: 5,
                            ty: types::INT32
                        },
                        (69, 75)
                    )
                ]),
                generics: None,
                abi: None,
                id: 0
            }),
            returns,
        );
    }
    #[test]
    fn tuples() {
        const SRC: &'static str = "
let produce (a:int32) = (a,a);
#! in theory this could be `let consume = fst` but let's ignore that case for now
let consume a = fst a;
";
        let predefined = [(
            "fst".to_string(),
            ResolvedType::Tuple {
                underlining: vec![types::INT32, types::INT32],
                loc: (0, 0),
            }
            .fn_ty(&types::INT32),
        )]
        .into();

        let ast = file("",SRC);
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut ctx = super::Context::new(
            dtree,
            predefined,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );

        let mut ast = ctx.inference(ast);
        ast.decls
            .sort_by_key(super::ast::TopLevelDeclaration::get_ident);
        let [consume, produce] = &ast.decls[..] else {
            unreachable!("more than the declared functions?")
        };
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (118,125),
                is_op: false,
                ident: "consume".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (126,127),
                    ident: "a".to_string(),
                    ty: ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32,],
                        loc: (0, 0)
                    },
                    id: 6
                }],
                ty: ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32,],
                    loc: (0, 0)
                }
                .fn_ty(&types::INT32),
                value: super::ast::ValueType::Expr(super::ast::Expr::FnCall(super::ast::FnCall {
                    loc: (130,133),
                    value: super::ast::Expr::ValueRead("fst".to_string(), (130,133), 8).into(),
                    arg: super::ast::Expr::ValueRead("a".to_string(), (134,135), 7).into(),
                    id: 9,
                    returns: types::INT32,
                })),
                generics: None,
                abi: None,
                id: 5
            }),
            consume,
            "as arg"
        );

        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (5,12),
                is_op: false,
                ident: "produce".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (14,15),
                    ident: "a".to_string(),
                    ty: types::INT32,
                    id: 1
                }],
                ty: types::INT32.fn_ty(&ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32,],
                    loc: (0, 0)
                }),
                id: 0,
                generics: None,
                abi: None,
                value: super::ast::ValueType::Expr(super::ast::Expr::TupleLiteral {
                    contents: vec![
                        super::ast::Expr::ValueRead("a".to_string(), (26,27), 3),
                        super::ast::Expr::ValueRead("a".to_string(), (28,29), 4),
                    ],
                    id: 2,
                    loc: (25,30),
                })
            }),
            produce,
            "produces"
        );
    }

    #[test]
    fn array() {
        const SRC: &'static str = "
let f (a:[int32;5]) = ();

let main _ : () -> () =
    f [1,2,3,4,5];
    return ();
";

        let ast = file("",SRC);
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_ctx = super::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let mut ast = inference_ctx.inference(ast);
        ast.decls.sort_by_key(|decl| decl.get_ident());
        let [f, main] = &ast.decls[..] else {
            unreachable!()
        };
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (5,6),
                is_op: false,
                ident: "f".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (8,9),
                    ident: "a".to_string(),
                    ty: ResolvedType::Array {
                        underlining: types::INT32.into(),
                        size: 5
                    },
                    id: 1,
                },],
                ty: ResolvedType::Array {
                    underlining: types::INT32.into(),
                    size: 5
                }
                .fn_ty(&types::UNIT),
                value: super::ast::ValueType::Expr(super::ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
                id: 0
            }),
            f,
            "the function"
        );
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (32,36),
                is_op: false,
                ident: "main".to_string(),
                args: vec![super::ast::ArgDeclaration::Discard {
                    loc: (37, 38),
                    ty: types::UNIT,
                },],
                ty: types::UNIT.fn_ty(&types::UNIT),
                value: super::ast::ValueType::Function(vec![
                    super::ast::Statement::Expr(super::ast::Expr::FnCall(super::ast::FnCall {
                        loc: (56,57),
                        value: super::ast::Expr::ValueRead("f".to_string(), (56,57), 9).into(),
                        arg: super::ast::Expr::ArrayLiteral {
                            contents: vec![
                                super::ast::Expr::NumericLiteral {
                                    value: "1".to_string(),
                                    id: 4,
                                    ty: types::INT32
                                },
                                super::ast::Expr::NumericLiteral {
                                    value: "2".to_string(),
                                    id: 5,
                                    ty: types::INT32
                                },
                                super::ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                    id: 6,
                                    ty: types::INT32
                                },
                                super::ast::Expr::NumericLiteral {
                                    value: "4".to_string(),
                                    id: 7,
                                    ty: types::INT32
                                },
                                super::ast::Expr::NumericLiteral {
                                    value: "5".to_string(),
                                    id: 8,
                                    ty: types::INT32
                                },
                            ],
                            loc: (58,69),
                            id: 3
                        }
                        .into(),
                        id: 10,
                        returns: types::UNIT
                    })),
                    super::ast::Statement::Return(super::ast::Expr::UnitLiteral, (75,81))
                ]),
                generics: None,
                abi: None,
                id: 2
            }),
            main,
            "main"
        );
    }

    #[test]
    fn patterns() {
        const SRC: &'static str = "
let ors (a:int32) = match a where
    | 1 | 2 | 3 -> 0,
    | a -> a,
let tuples (v:(int32,int32)) = match v where
    | (a,0) -> a,
    | (1,b) -> b,
    | _ -> 0,
    ";
        let ast = file("",SRC);
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_ctx = super::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let mut ast = inference_ctx.inference(ast);
        ast.decls.sort_by_key(|decl| decl.get_ident());
        let [ors, tuples] = &ast.decls[..] else {
            unreachable!()
        };
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (5,8),
                is_op: false,
                ident: "ors".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (10,11),
                    ident: "a".to_string(),
                    ty: types::INT32,
                    id: 1
                }],
                ty: types::INT32.fn_ty(&types::INT32),
                value: super::ast::ValueType::Expr(super::ast::Expr::Match(super::ast::Match {
                    loc: (21,26),
                    on: super::ast::Expr::ValueRead("a".to_string(), (27,28), 3).into(),
                    arms: vec![
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                super::ast::Expr::NumericLiteral {
                                    value: "0".to_string(),
                                    id: 4,
                                    ty: types::INT32
                                }
                                .into()
                            ),
                            cond: super::ast::Pattern::Or(
                                super::ast::Pattern::ConstNumber("1".to_string(), types::INT32)
                                    .into(),
                                super::ast::Pattern::Or(
                                    super::ast::Pattern::ConstNumber("2".to_string(), types::INT32)
                                        .into(),
                                    super::ast::Pattern::ConstNumber("3".to_string(), types::INT32)
                                        .into(),
                                )
                                .into()
                            ),
                            loc: (39,40),
                        },
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                super::ast::Expr::ValueRead("a".to_string(), (68,69), 6).into()
                            ),
                            cond: super::ast::Pattern::Read {
                                ident: "a".to_string(),
                                loc: (63,64),
                                ty: types::INT32,
                                id: 5
                            },
                            loc: (61,62),
                        }
                    ],
                    id: 2
                })),
                generics: None,
                abi: None,
                id: 0
            }),
            ors
        );

        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (75,81),
                is_op: false,
                ident: "tuples".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (83,84),
                    ident: "v".to_string(),
                    ty: ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32,],
                        loc: (0, 0)
                    },
                    id: 8
                }],
                ty: ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32,],
                    loc: (0, 0)
                }
                .fn_ty(&types::INT32),
                value: super::ast::ValueType::Expr(super::ast::Expr::Match(super::ast::Match {
                    loc: (102,107),
                    on: super::ast::Expr::ValueRead("v".to_string(), (108,109), 10).into(),
                    arms: vec![
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                super::ast::Expr::ValueRead("a".to_string(), (131,132), 13).into()
                            ),
                            cond: super::ast::Pattern::Destructure(
                                super::ast::DestructurePattern::Tuple(
                                    vec![
                                        super::ast::Pattern::Read {
                                            ident: "a".to_string(),
                                            loc: (123,124),
                                            ty: types::INT32,
                                            id: 12
                                        },
                                        super::ast::Pattern::ConstNumber(
                                            "0".to_string(),
                                            types::INT32
                                        ),
                                    ],
                                    ResolvedType::Tuple {
                                        underlining: vec![types::INT32, types::INT32,],
                                        loc: (0, 0)
                                    },
                                    11
                                )
                            ),
                            loc: (120,121)
                        },
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                super::ast::Expr::ValueRead("b".to_string(), (149,150), 16).into()
                            ),
                            cond: super::ast::Pattern::Destructure(
                                super::ast::DestructurePattern::Tuple(
                                    vec![
                                        super::ast::Pattern::ConstNumber(
                                            "1".to_string(),
                                            types::INT32
                                        ),
                                        super::ast::Pattern::Read {
                                            ident: "b".to_string(),
                                            loc: (143,144),
                                            ty: types::INT32,
                                            id: 15
                                        },
                                    ],
                                    ResolvedType::Tuple {
                                        underlining: vec![types::INT32, types::INT32,],
                                        loc: (0, 0)
                                    },
                                    14
                                )
                            ),
                            loc: (138,139)
                        },
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(
                                super::ast::Expr::NumericLiteral {
                                    value: "0".to_string(),
                                    id: 17,
                                    ty: types::INT32
                                }
                                .into()
                            ),
                            cond: super::ast::Pattern::Default,
                            loc: (156,157)
                        }
                    ],
                    id: 9,
                })),
                generics: None,
                abi: None,
                id: 7
            }),
            tuples,
            "tuples"
        );
    }
    #[test]
    fn destructureing_statement() {
        let ast = file("",
            "
let a (v:(int32,int32)) =
    let (x,y) = v;
    return ();
",
        );
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_ctx = crate::inference::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let ast = inference_ctx.inference(ast);
        let [a] = &ast.decls[..] else { unreachable!() };
        use super::ast;
        assert_eq!(
            &ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (5,6),
                is_op: false,
                ident: "a".to_string(),
                args: vec![ast::ArgDeclaration::Simple {
                    ident: "v".to_string(),
                    ty: ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32,],
                        loc: (0, 0)
                    },
                    loc: (8,9),
                    id: 1,
                }],
                ty: ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32,],
                    loc: (0, 0)
                }
                .fn_ty(&types::UNIT),
                value: ast::ValueType::Function(vec![
                    ast::Statement::Declaration(ast::ValueDeclaration {
                        loc: (35,40),
                        is_op: false,
                        target: ast::Pattern::Destructure(ast::DestructurePattern::Tuple(
                            vec![
                                ast::Pattern::Read {
                                    ident: "x".to_string(),
                                    ty: types::INT32,
                                    loc: (36,37),
                                    id: 3
                                },
                                ast::Pattern::Read {
                                    ident: "y".to_string(),
                                    ty: types::INT32,
                                    loc: (38,39),
                                    id: 4
                                },
                            ],
                            ResolvedType::Tuple {
                                underlining: vec![types::INT32, types::INT32,],
                                loc: (0, 0)
                            },
                            2
                        )),
                        args: Vec::new(),
                        value: ast::ValueType::Expr(ast::Expr::ValueRead(
                            "v".to_string(),
                            (43,44),
                            6
                        )),
                        ty: ResolvedType::Tuple {
                            underlining: vec![types::INT32, types::INT32,],
                            loc: (0, 0)
                        },
                        generics: None,
                        abi: None,
                        id: 5
                    }),
                    ast::Statement::Return(ast::Expr::UnitLiteral, (50,56))
                ]),
                generics: None,
                abi: None,
                id: 0
            }),
            a
        );
    }

    #[test]
    fn enum_patterns() {
        let ast = file("",
            "
enum IP = | V4 (int8,int8,int8,int8) | V6 (int8,int8,int8,int8,int8,int8)

let do_something a = match a where
| IP::V4 (127,0,0,1) -> (),
| IP::V4 a -> (),
| IP::V6 _ -> (),
",
        );

        let mut ctx = super::Context::new(
            [].into(),
            HashMap::new(),
            [].into(),
            [].into(),
            [].into(),
        );
        let ast = ctx.inference(ast);
        let [_enum_, func] = &ast.decls[..] else {
            panic!("too much? too little?")
        };
        assert_eq!(
            &super::ast::TopLevelDeclaration::Value(super::ast::TopLevelValue {
                loc: (80,92),
                is_op: false,
                ident: "do_something".to_string(),
                args: vec![super::ast::ArgDeclaration::Simple {
                    loc: (93,94),
                    ident: "a".to_string(),
                    ty: ResolvedType::User {
                        name: "IP".to_string(),
                        generics: Vec::new(),
                        loc: (0, 0),
                    },
                    id: 1
                },],
                ty: ResolvedType::User {
                    name: "IP".to_string(),
                    generics: Vec::new(),
                    loc: (0, 0),
                }
                .fn_ty(&types::UNIT),
                value: super::ast::ValueType::Expr(super::ast::Expr::Match(super::ast::Match {
                    loc: (97,102),
                    on: super::ast::Expr::ValueRead("a".to_string(), (103,104), 3).into(),
                    arms: vec![
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(super::ast::Expr::UnitLiteral.into()),
                            cond: super::ast::Pattern::EnumVariant {
                                ty: ResolvedType::Dependent {
                                    base: ResolvedType::User {
                                        name: "IP".into(),
                                        generics: Vec::new(),
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    ident: "IP::V4".into(),
                                    generics: Vec::new(),
                                    actual: ResolvedType::Tuple {
                                        underlining: vec![
                                            types::INT8;4
                                        ],
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    loc: (0, 0)
                                },
                                variant: "IP::V4".to_string(),
                                // (127, 0, 0, 1)
                                pattern: Some(
                                    super::ast::Pattern::Destructure(
                                        super::ast::DestructurePattern::Tuple(
                                            vec![
                                                super::ast::Pattern::ConstNumber(
                                                    "127".to_string(),
                                                    types::INT8
                                                ),
                                                super::ast::Pattern::ConstNumber(
                                                    "0".to_string(),
                                                    types::INT8
                                                ),
                                                super::ast::Pattern::ConstNumber(
                                                    "0".to_string(),
                                                    types::INT8
                                                ),
                                                super::ast::Pattern::ConstNumber(
                                                    "1".to_string(),
                                                    types::INT8
                                                ),
                                            ], ResolvedType::Tuple {
                                                    underlining: vec![
                                                        types::INT8;4
                                                    ],
                                                    loc: (0, 0)
                                                },
                                            4
                                        )
                                    )
                                    .into()
                                ),
                                loc: (113,119),
                            },
                            loc: (111,112),
                        },
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(super::ast::Expr::UnitLiteral.into()),
                            cond: super::ast::Pattern::EnumVariant {
                                ty: ResolvedType::Dependent {
                                    base: ResolvedType::User {
                                        name: "IP".into(),
                                        generics: Vec::new(),
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    ident: "IP::V4".into(),
                                    generics: Vec::new(),
                                    actual: ResolvedType::Tuple {
                                        underlining: vec![
                                            types::INT8,
                                            types::INT8,
                                            types::INT8,
                                            types::INT8,
                                        ],
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    loc: (0, 0)
                                },
                                variant: "IP::V4".into(),
                                pattern: Some(
                                    super::ast::Pattern::Read {
                                        ident: "a".into(),
                                        loc: (148,149),
                                        ty:  ResolvedType::Tuple {
                                                underlining: vec![
                                                    types::INT8;4
                                                ],
                                                loc: (0, 0)
                                            },
                                        id: 5
                                    }
                                    .into()
                                ),
                                loc: (141,147)
                            },
                            loc: (139,140),
                        },
                        super::ast::MatchArm {
                            block: Vec::new(),
                            ret: Some(super::ast::Expr::UnitLiteral.into()),
                            cond: super::ast::Pattern::EnumVariant {
                                ty: ResolvedType::Dependent {
                                    base: ResolvedType::User {
                                        name: "IP".into(),
                                        generics: Vec::new(),
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    ident: "IP::V6".into(),
                                    generics: Vec::new(),
                                    actual: ResolvedType::Tuple {
                                        underlining: vec![
                                            types::INT8,
                                            types::INT8,
                                            types::INT8,
                                            types::INT8,
                                            types::INT8,
                                            types::INT8,
                                        ],
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    loc: (0, 0)
                                },
                                variant: "IP::V6".into(),
                                pattern: Some(super::ast::Pattern::Default.into()),
                                loc: (159,165)
                            },
                            loc: (157,158)
                        }
                    ],
                    id: 2
                })),
                generics: None,
                abi: None,
                id: 0,
            }),
            func,
        );

    }

    #[test]
    #[ignore = "for debugging only"]
    fn debug() {

        const SRC: &'static str = r#"
enum Testing = | One (int8,int8) | Two;
let fun test = match test where
| Testing::One (0,1) -> 0,
| Testing::One (1|0,a) -> a,
| Testing::One _ -> 1,
| Testing::Two -> 2,
"#; 
        let mut ast = file("",SRC);
        ast.canonialize(vec!["v".into()]);
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_ctx = crate::inference::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let ast = inference_ctx.inference(ast);
        let [_ty, fun] = &ast.decls[..] else {
            unreachable!()
        };
        let expected = super::ast::TopLevelDeclaration::Value(
            super::ast::TopLevelValue {
                loc: (
                    2,
                    4,
                ),
                is_op: false,
                ident: "fun".into(),
                args: vec![
                    super::ast::ArgDeclaration::Simple {
                        loc: (
                            2,
                            8,
                        ),
                        ident: "test".into(),
                        ty: ResolvedType::User {
                            name: "Testing".into(),
                            generics: Vec::new(),
                            loc: (
                                1,
                                5,
                            ),
                        },
                        id: 1,
                    },
                ],
                ty: ResolvedType::Function {
                    arg: ResolvedType::User {
                        name: "Testing".into(),
                        generics: Vec::new(),
                        loc: (
                            1,
                            5,
                        ),
                    }.into(),
                    returns: types::INT8.into(),
                    loc: (
                        0,
                        0,
                    ),
                },
                value: super::ast::ValueType::Expr(
                    super::ast::Expr::Match(
                        super::ast::Match {
                            loc: (
                                2,
                                15,
                            ),
                            on: super::ast::Expr::ValueRead(
                                "test".into(),
                                (
                                    2,
                                    21,
                                ),
                                3,
                            ).into(),
                            arms: vec![
                                super::ast::MatchArm {
                                    block: Vec::new(),
                                    ret: Some(
                                        super::ast::Expr::NumericLiteral {
                                            value: "0".into(),
                                            id: 5,
                                            ty: types::INT8,
                                        }.into(),
                                    ),
                                    cond: super::ast::Pattern::EnumVariant {
                                        ty: ResolvedType::Dependent {
                                            base: ResolvedType::User {
                                                name: "Testing".into(),
                                                generics: Vec::new(),
                                                loc: (
                                                    1,
                                                    5,
                                                ),
                                            }.into(),
                                            ident: "Testing::One".into(),
                                            actual: ResolvedType::Tuple {
                                                underlining: vec![
                                                    types::INT8;2
                                                ],
                                                loc: (
                                                    1,
                                                    21,
                                                ),
                                            }.into(),
                                            generics: Vec::new(),
                                            loc: (
                                                1,
                                                17,
                                            ),
                                        },
                                        variant: "Testing::One".into(),
                                        pattern: Some(
                                            super::ast::Pattern::Destructure(
                                                super::ast::DestructurePattern::Tuple(
                                                    vec![
                                                        super::ast::Pattern::ConstNumber(
                                                            "0".into(),
                                                            types::INT8,
                                                        ),
                                                        super::ast::Pattern::ConstNumber(
                                                            "1".into(),
                                                            types::INT8,
                                                        ),
                                                    ],
                                                    ResolvedType::Tuple {
                                                        underlining: vec![
                                                            types::INT8;2
                                                        ],
                                                        loc: (
                                                            1,
                                                            21,
                                                        ),
                                                    },
                                                    4,
                                                ),
                                            ).into(),
                                        ),
                                        loc: (
                                            3,
                                            2,
                                        ),
                                    },
                                    loc: (
                                        3,
                                        2,
                                    ),
                                },
                                super::ast::MatchArm {
                                    block: Vec::new(),
                                    ret: Some(
                                        super::ast::Expr::ValueRead(
                                            "a".into(),
                                            (
                                                4,
                                                26,
                                            ),
                                            8,
                                        ).into(),
                                    ),
                                    cond: super::ast::Pattern::EnumVariant {
                                        ty: ResolvedType::Dependent {
                                            base: ResolvedType::User {
                                                name: "Testing".into(),
                                                generics: Vec::new(),
                                                loc: (
                                                    1,
                                                    5,
                                                ),
                                            }.into(),
                                            ident: "Testing::One".into(),
                                            actual: ResolvedType::Tuple {
                                                underlining: vec![
                                                    types::INT8;2
                                                ],
                                                loc: (
                                                    1,
                                                    21,
                                                ),
                                            }.into(),
                                            generics: Vec::new(),
                                            loc: (
                                                1,
                                                17,
                                            ),
                                        },
                                        variant: "Testing::One".into(),
                                        pattern: Some(
                                            super::ast::Pattern::Destructure(
                                                super::ast::DestructurePattern::Tuple(
                                                    vec![
                                                        super::ast::Pattern::Or(
                                                            super::ast::Pattern::ConstNumber(
                                                                "1".into(),
                                                                types::INT8,
                                                            ).into(),
                                                            super::ast::Pattern::ConstNumber(
                                                                "0".into(),
                                                                types::INT8,
                                                            ).into(),
                                                        ),
                                                        super::ast::Pattern::Read {
                                                            ident: "a".into(),
                                                            loc: (
                                                                4,
                                                                20,
                                                            ),
                                                            ty: types::INT8,
                                                            id: 7,
                                                        },
                                                    ],
                                                    ResolvedType::Tuple {
                                                        underlining: vec![
                                                            types::INT8;2
                                                        ],
                                                        loc: (
                                                            1,
                                                            21,
                                                        ),
                                                    },
                                                    6,
                                                ),
                                            ).into(),
                                        ),
                                        loc: (
                                            4,
                                            2,
                                        ),
                                    },
                                    loc: (
                                        4,
                                        2,
                                    ),
                                },
                                super::ast::MatchArm {
                                    block: Vec::new(),
                                    ret: Some(
                                        super::ast::Expr::NumericLiteral {
                                            value: "1".into(),
                                            id: 9,
                                            ty: types::INT8,
                                        }.into(),
                                    ),
                                    cond: super::ast::Pattern::EnumVariant {
                                        ty: ResolvedType::Dependent {
                                            base: ResolvedType::User {
                                                name: "Testing".into(),
                                                generics: Vec::new(),
                                                loc: (
                                                    1,
                                                    5,
                                                ),
                                            }.into(),
                                            ident: "Testing::One".into(),
                                            actual: ResolvedType::Tuple {
                                                underlining: vec![
                                                    types::INT8;2
                                                ],
                                                loc: (
                                                    1,
                                                    21,
                                                ),
                                            }.into(),
                                            generics: Vec::new(),
                                            loc: (
                                                1,
                                                17,
                                            ),
                                        },
                                        variant: "Testing::One".into(),
                                        pattern: Some(
                                            super::ast::Pattern::Default.into(),
                                        ),
                                        loc: (
                                            5,
                                            2,
                                        ),
                                    },
                                    loc: (
                                        5,
                                        2,
                                    ),
                                },
                                super::ast::MatchArm {
                                    block: Vec::new(),
                                    ret: Some(
                                        super::ast::Expr::NumericLiteral {
                                            value: "2".into(),
                                            id: 10,
                                            ty: types::INT8,
                                        }.into(),
                                    ),
                                    cond: super::ast::Pattern::EnumVariant {
                                        ty: ResolvedType::Dependent {
                                            base: ResolvedType::User {
                                                name: "Testing".into(),
                                                generics: Vec::new(),
                                                loc: (
                                                    1,
                                                    5,
                                                ),
                                            }.into(),
                                            ident: "Testing::Two".into(),
                                            actual: ResolvedType::Void.into(),
                                            generics: Vec::new(),
                                            loc: (
                                                1,
                                                35,
                                            ),
                                        },
                                        variant: "Testing::Two".into(),
                                        pattern: None,
                                        loc: (
                                            6,
                                            2,
                                        ),
                                    },
                                    loc: (
                                        6,
                                        2,
                                    ),
                                },
                            ],
                            id: 2,
                        },
                    ),
                ),
                generics: None,
                abi: None,
                id: 0,
            },
        );  
        assert_eq!(
            fun,
            &expected
        );
    }
}

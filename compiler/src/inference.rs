use std::{cmp::Ordering, collections::HashMap};

use itertools::Itertools;

use crate::{
    ast as untyped_ast,
    types::{self, ResolvedType},
    util::ExtraUtilFunctions,
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
    expr_ty : HashMap<usize, ResolvedType>,
    equations: HashMap<usize, ResolvedType>, //? unsure of type here yet.
}
impl Context {
    pub(crate) fn inference(
        &mut self,
        ast: untyped_ast::ModuleDeclaration,
    ) -> ast::ModuleDeclaration {
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
        decl: untyped_ast::Declaration,
    ) -> ast::Declaration {
        match decl {
            untyped_ast::Declaration::TypeDefinition(ty) => ast::Declaration::Type(ty),
            untyped_ast::Declaration::Mod(_) => todo!(),
            untyped_ast::Declaration::Value(v) => {
                ast::Declaration::Value(self.assign_ids_value_decl(v))
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
            ident,
            args,
            ty,
            value,
            generictypes,
            abi,
        } = val;

        let id = self.get_next_expr_id();
        let decl_ty = ty.unwrap_or_else(|| self.get_next_type_id());
        self.known_values.insert(ident.clone(), decl_ty.clone());
        let args = args
            .into_iter()
            .enumerate()
            .map(|(idx, arg)| {
                let untyped_ast::ArgDeclaration { loc, ident, ty } = arg;
                let ty = ty.unwrap_or_else(|| {
                    if decl_ty.is_unknown() {
                        self.get_next_type_id()
                    } else {
                        decl_ty.get_nth_arg(idx)
                    }
                });
                let id = self.get_next_expr_id();
                self.known_locals.insert(ident.clone(), ty.clone());
                ast::ArgDeclaration { loc, ident, ty, id }
            })
            .collect_vec();
        let value = match value {
            untyped_ast::ValueType::Expr(expr) => {
                ast::ValueType::Expr(self.assign_ids_expr(expr, None))
            }
            untyped_ast::ValueType::Function(stmnts) => ast::ValueType::Function(
                stmnts
                    .into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect(),
            ),
            untyped_ast::ValueType::External => ast::ValueType::External,
        };
        ast::ValueDeclaration {
            loc,
            is_op,
            ident,
            args,
            ty: decl_ty,
            value,
            generics: generictypes,
            abi,
            id,
        }
    }

    fn assign_ids_stmnt(&mut self, stmnt: untyped_ast::Statement) -> ast::Statement {
        match stmnt {
            untyped_ast::Statement::Declaration(decl) => {
                let decl = self.assign_ids_value_decl(decl);
                self.known_locals
                    .insert(decl.ident.clone(), decl.ty.clone());
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
                let untyped_ast::IfBranching {
                    cond,
                    true_branch,
                    else_ifs,
                    else_branch,
                    loc,
                } = if_;
                let cond = self.assign_ids_expr(*cond, Some(types::BOOL));
                let true_branch = true_branch
                    .into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect();
                let else_ifs = else_ifs
                    .into_iter()
                    .map(|(cond, branch)| {
                        let cond = self.assign_ids_expr(*cond, Some(types::BOOL));
                        let branch = branch
                            .into_iter()
                            .map(|stmnt| self.assign_ids_stmnt(stmnt))
                            .collect();
                        (cond.boxed(), branch)
                    })
                    .collect();
                let else_branch = else_branch
                    .into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect();
                ast::Statement::IfStatement(ast::IfBranching {
                    cond: cond.boxed(),
                    true_branch,
                    else_ifs,
                    else_branch,
                    loc,
                })
            }
            untyped_ast::Statement::Match(match_) => {
                ast::Statement::Match(self.assign_ids_match(match_))
            }
            untyped_ast::Statement::Error => ast::Statement::Error,
        }
    }

    fn assign_ids_match(&mut self, match_: untyped_ast::Match) -> ast::Match {
        let id = self.get_next_expr_id();
        let untyped_ast::Match { loc, on, arms } = match_;
        let on = self.assign_ids_expr(*on, None);
        let arms = arms
            .into_iter()
            .map(|arm| {
                let untyped_ast::MatchArm {
                    block,
                    ret,
                    cond,
                    loc,
                } = arm;
                let block = block
                    .into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect();
                let ret = ret.map(|ret| self.assign_ids_expr(*ret, None).boxed());
                ast::MatchArm {
                    block,
                    ret,
                    cond: (cond, self.get_next_expr_id()),
                    loc,
                }
            })
            .collect();
        ast::Match {
            loc,
            on: on.boxed(),
            arms,
            id,
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
        let arg_t = arg.get_retty(self).boxed();
        let value = self.assign_ids_expr(
            *value,
            Some(ResolvedType::Function {
                arg: arg_t,
                returns: result_t.clone().boxed(),
                loc
            }),
        );
        ast::FnCall {
            loc,
            value: value.boxed(),
            arg: arg.boxed(),
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
            untyped_ast::Expr::Compose { lhs:_, rhs:_ } => todo!(),
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
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
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
            untyped_ast::Expr::ListLiteral { contents:_, loc:_ } => todo!(),
            untyped_ast::Expr::TupleLiteral { contents:_, loc:_ } => todo!(),
            untyped_ast::Expr::StructConstruction(_) => todo!(),
            untyped_ast::Expr::BoolLiteral(value, loc) => {
                ast::Expr::BoolLiteral(value, loc, self.get_next_expr_id())
            }
            untyped_ast::Expr::If(if_) => {
                let id = self.get_next_expr_id();
                let result = self.get_next_type_id();
                let untyped_ast::IfExpr {
                    cond,
                    true_branch,
                    else_ifs,
                    else_branch,
                    loc,
                } = if_;
                let cond = self.assign_ids_expr(*cond, Some(types::BOOL)).boxed();
                let (true_block, true_ret) = true_branch;
                let true_block = true_block
                    .into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect_vec();
                let true_ret = self.assign_ids_expr(*true_ret, None).boxed();
                let else_ifs = else_ifs
                    .into_iter()
                    .map(|(cond, block, ret)| {
                        let cond = self.assign_ids_expr(*cond, Some(types::BOOL)).boxed();
                        let block = block
                            .into_iter()
                            .map(|stmnt| self.assign_ids_stmnt(stmnt))
                            .collect_vec();
                        let ret = self.assign_ids_expr(*ret, None).boxed();
                        (cond, block, ret)
                    })
                    .collect_vec();
                let (else_block, else_ret) = else_branch;
                let else_block = else_block
                    .into_iter()
                    .map(|stmnt| self.assign_ids_stmnt(stmnt))
                    .collect_vec();
                let else_ret = self.assign_ids_expr(*else_ret, None).boxed();
                ast::Expr::If(ast::IfExpr {
                    cond,
                    true_branch: (true_block, true_ret),
                    else_ifs,
                    else_branch: (else_block, else_ret),
                    loc,
                    id,
                    result,
                })
            }
            untyped_ast::Expr::Match(match_) => ast::Expr::Match(self.assign_ids_match(match_)),
        }
    }
    fn try_to_infer(&mut self, module: &mut ast::ModuleDeclaration) {
        let ast::ModuleDeclaration { loc: _, name: _, decls } = module;
        let order = self.dependency_tree.iter().sorted_by(|(lhs_name,lhs_depends), (rhs_name,rhs_depends)| {
            match (lhs_depends.contains(*rhs_name), rhs_depends.contains(*lhs_name)) {
                (true,true) => {
                    if decls.iter().find(|decl| &decl.get_ident() == *lhs_name).map_or(false, ast::Declaration::has_ty) {
                        Ordering::Less//left has explict type so it can come first
                    } else if decls.iter().find(|decl| &decl.get_ident() == *rhs_name).map_or(false, ast::Declaration::has_ty) {
                        Ordering::Greater//right has explict type so it can come first
                    } else {
                        todo!("remove this case.  both lhs and rhs. means they depend on each other.")
                    }
                }
                (false, true) => Ordering::Less,//right depends on left thus should appear after.
                (true, false) => Ordering::Greater,//left depends on right thus should appear after.
                (false,false) => Ordering::Equal,//neither depend on each other.
            }
        })
        .map(|(a,_)|a)
        .cloned()
        .collect_vec();
        decls.sort_unstable_by_key(|decl| order.iter().position(|name| name == &decl.get_ident()));
        for decl in decls {
            self.known_locals.clear();
            match decl {
                ast::Declaration::Value(value) => {
                    self.infer_decl(value);
                    self.known_values
                        .insert(value.ident.clone(), value.ty.clone());
                }
                ast::Declaration::Type(_) => (), //don't think there is anything to be done here.
            }
        }
    }

    fn infer_decl(&mut self, value :&mut ast::ValueDeclaration) {
        for arg in &value.args {
            self.known_locals.insert(arg.ident.clone(), arg.ty.clone());
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
                    value.ty =  value
                            .args
                            .iter()
                            .map(|arg| arg.ty.clone())
                            .rev()
                            .fold(actual,|ty, arg| arg.fn_ty(&ty));
        
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
                        .map(|arg| arg.ty.clone())
                        .reduce(|ty, arg| ty.fn_ty(&arg));
                    value.ty = fun.unwrap().fn_ty(&ty.unwrap()); //not sure if this the correct way to handle this.
                }
            },
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
            ast::Expr::Error(_) => ResolvedType::Error,
            ast::Expr::NumericLiteral { value: _, id: _, ty } => {
                if let Some(ety) = expected {
                    let is_ety_valid = ety.is_float() || ety.is_int();
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
                loc:_,
                lhs,
                rhs,
                operator,
                id:_,
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
                                arg:lhs_ty,
                                returns: temp,
                                loc:_
                            } = result else { unreachable!()};
                            let ResolvedType::Function { 
                                arg:rhs_ty,
                                returns: result,
                                loc:_
                            } = temp.as_ref() else { unreachable!()};
                            
                            self.expr_ty.insert(lhs.get_expr_id(),lhs_ty.as_ref().clone());
                            self.expr_ty.insert(rhs.get_expr_id(), rhs_ty.as_ref().clone());
                            if let ResolvedType::Unknown(id) = ety{
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
                                arg:lhs_ty,
                                returns: temp,
                                loc:_
                            } = result else { unreachable!()};
                            let ResolvedType::Function { 
                                arg:rhs_ty,
                                returns: result,
                                loc:_
                            } = temp.as_ref() else { unreachable!()};
                            
                            self.expr_ty.insert(lhs.get_expr_id(),lhs_ty.as_ref().clone());
                            self.expr_ty.insert(rhs.get_expr_id(), rhs_ty.as_ref().clone());
                            
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
                if let Some(ty) = self.known_locals.get_mut(ident) {
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
            ast::Expr::ArrayLiteral { contents, loc: _, id:_ } => {
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
                if underlining != types::ERROR {
                    for elem in contents.iter_mut() {
                        self.get_actual_type(elem, Some(underlining.clone()), fun_ret_ty);
                    }
                }

                let out = types::ResolvedType::Array {
                    underlining: underlining.boxed(),
                    size: contents.len(),
                };
                // self.known_values.insert(*id, out.clone());
                // TODO? not sure if I use expr ids at this point
                out
            }
            ast::Expr::ListLiteral { contents, loc:_, id:_ } => {
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
            ast::Expr::If(ast::IfExpr {
                cond,
                true_branch,
                else_ifs,
                else_branch,
                loc:_,
                id:_,
                result,
            }) => {
                self.get_actual_type(cond, Some(types::BOOL), fun_ret_ty);
                let (true_block, true_ret) = true_branch;
                for stmnt in true_block {
                    self.infer_stmnt(stmnt, fun_ret_ty);
                }
                let mut ety = self.get_actual_type(true_ret, expected.clone(), fun_ret_ty);
                for (cond, block, ret) in else_ifs {
                    self.get_actual_type(cond, Some(types::BOOL), fun_ret_ty);
                    for stmnt in block {
                        self.infer_stmnt(stmnt, fun_ret_ty);
                    }
                    if ety == types::ERROR {
                        ety = self.get_actual_type(ret, expected.clone(), fun_ret_ty);
                    } else {
                        self.get_actual_type(ret, Some(ety.clone()), fun_ret_ty);
                    }
                }
                let (block, ret) = else_branch;
                for stmnt in block {
                    self.infer_stmnt(stmnt, fun_ret_ty);
                }
                if ety == types::ERROR {
                    ety = self.get_actual_type(ret, expected, fun_ret_ty);
                } else {
                    self.get_actual_type(ret, Some(ety.clone()), fun_ret_ty);
                }

                *result = ety.clone();
                ety
            }
            ast::Expr::Match(match_) => self.handle_match(match_, expected, fun_ret_ty),
        }
    }

    fn handle_match(
        &mut self,
        match_: &mut ast::Match,
        e_ty : Option<ResolvedType>,
        fun_ret_ty: &mut Option<ResolvedType>,
    ) -> ResolvedType {
        let ast::Match { loc:_, on, arms, id:_ } = match_;
        let _on_ty = self.get_actual_type(on, None, fun_ret_ty);
        let mut ety = e_ty.unwrap_or(types::ERROR);
        for ast::MatchArm {
            block,
            ret,
            cond:_,
            loc:_,
        } in arms.iter_mut()
        {
            for stmnt in block {
                self.infer_stmnt(stmnt, fun_ret_ty);
            }
            if let Some(ret) = ret {
                if ety == types::ERROR || ety.is_unknown() || ety == types::NUMBER {
                    ety = self.get_actual_type(ret, None, fun_ret_ty);
                } else {
                    self.get_actual_type(ret, Some(ety.clone()), fun_ret_ty);
                }
            }
        }
        if !ety.is_error() {
            for arm in arms {
                self.add_equation_to_arm(arm, &ety)
            }
        }
        ety
    }

    fn add_equation_to_arm(&mut self, arm:&mut ast::MatchArm, ty : &ResolvedType) {
        if let Some(ret) = &mut arm.ret {
            if let ResolvedType::Unknown(id) = ret.get_retty(self) {
                self.equations.insert(id, ty.clone());
            }
            self.expr_ty.insert(ret.get_expr_id(),ty.clone());
        }
    }

    fn infer_stmnt(&mut self, stmnt: &mut ast::Statement, fun_ret_ty: &mut Option<ResolvedType>) {
        match stmnt {
            ast::Statement::Error => (),
            ast::Statement::Declaration(value) => {
                self.infer_decl(value);
                self.known_locals.insert(value.ident.clone(),value.ty.clone());
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
            ast::Statement::IfStatement(ast::IfBranching {
                cond,
                true_branch,
                else_ifs,
                else_branch,
                loc:_,
            }) => {
                self.get_actual_type(cond, Some(types::BOOL), fun_ret_ty);
                for stmnt in true_branch {
                    self.infer_stmnt(stmnt, fun_ret_ty);
                }
                for (cond, block) in else_ifs {
                    self.get_actual_type(cond, Some(types::BOOL), fun_ret_ty);
                    for stmnt in block {
                        self.infer_stmnt(stmnt, fun_ret_ty);
                    }
                }
                for stmnt in else_branch {
                    self.infer_stmnt(stmnt, fun_ret_ty);
                }
            }
            ast::Statement::Match(match_) => {
                self.handle_match(match_, None, fun_ret_ty);
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
        } = fncall;
        if let ResolvedType::Function {
            arg: arg_t,
            returns: return_t,
            loc:_
        } = self.get_actual_type(value.as_mut(), None, fun_ret_ty)
        {
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
            ResolvedType::Error
        }
    }

    fn apply_equations(&self, module: &mut ast::ModuleDeclaration) {
        self.apply_substutions(module);
        let ast::ModuleDeclaration { loc:_, name:_, decls } = module;
        let mut equations = self.equations.clone().into_iter().collect_vec();
        equations.sort_unstable_by(|(lhs_id, lhs_ty), (rhs_id, rhs_ty)| {
            match (
                lhs_ty.contains_unknown(*rhs_id),
                rhs_ty.contains_unknown(*lhs_id),
            ) {
                (true, true) => todo!("handle circular equations?"),
                (false, false) => Ordering::Equal,
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
            }
        });
        for decl in decls {
            match decl {
                ast::Declaration::Value(v) => {
                    for (id, ty) in &equations {
                        self.apply_equation_decl(v, *id, ty.clone());
                    }
                }
                ast::Declaration::Type(_) => (), //Nothing to do.
            }
        }
    }

    fn apply_equation_decl(&self, decl: &mut ast::ValueDeclaration, id: usize, ty: ResolvedType) {
        let ast::ValueDeclaration {
            loc : _,
            is_op : _,
            ident: _,
            args,
            ty: v_ty,
            value,
            generics : _,
            abi: _,
            id: _,
        } = decl;
        v_ty.replace_unkown_with(id, ty.clone());
        for arg in args.iter_mut() {
            arg.ty.replace_unkown_with(id, ty.clone());
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
            ast::ValueType::External=>(),
        }
    }

    fn apply_substutions(&self, ast : &mut ast::ModuleDeclaration) {
        for sub in &self.expr_ty {
            for decl in &mut ast.decls {
                match decl {
                    ast::Declaration::Value(v) => self.apply_substution_decl(sub, v),
                    _ => (),
                }
            } 
        }
    }

    fn apply_substution_decl(&self, sub:(&usize,&ResolvedType), decl : &mut ast::ValueDeclaration) {
        let ast::ValueDeclaration{ value, args, .. } = decl;
        
        for arg in args {
            if arg.id == *sub.0 {
                arg.ty = sub.1.clone()
            }
        }

        match value {
            ast::ValueType::Expr(expr) => self.apply_substution_expr(sub, expr),
            ast::ValueType::Function(stmnts) => {
                for stmnt in stmnts {
                    self.apply_substution_statement(sub, stmnt);
                }
            },
            ast::ValueType::External=>()
        }
    }

    fn apply_substution_statement(&self,sub:(&usize,&ResolvedType), stmnt : &mut ast::Statement) {
        match stmnt {
            ast::Statement::Declaration(v) => self.apply_substution_decl(sub, v),
            ast::Statement::FnCall(call) => self.apply_substution_fncall(sub, call),
            ast::Statement::IfStatement(ast::IfBranching{ cond, true_branch, else_ifs, else_branch, .. }) => {
                self.apply_substution_expr(sub, cond.as_mut());
                for stmnt in true_branch {
                    self.apply_substution_statement(sub, stmnt);
                }
                for (cond, block) in else_ifs {
                    self.apply_substution_expr(sub, cond.as_mut());
                    for stmnt in block {
                        self.apply_substution_statement(sub, stmnt);
                    }
                }
                for stmnt in else_branch {
                    self.apply_substution_statement(sub, stmnt);
                }
            }
            ast::Statement::Match(match_) => self.apply_substution_match(sub, match_),
            ast::Statement::Return(expr,_) => self.apply_substution_expr(sub, expr),
            ast::Statement::Error => (),
        }
    }

    fn apply_substution_expr(&self, (id,ty): (&usize, &ResolvedType), expr : &mut ast::Expr) {
        match expr {
            ast::Expr::NumericLiteral { value:_, id:eid, ty:nty } if eid == id => *nty=ty.clone(), 
            ast::Expr::BinaryOpCall(ast::BinaryOpCall{lhs,rhs, id:opid, result,..}) => {
                if opid ==id {
                    *result = ty.clone()
                } else {
                    self.apply_substution_expr((id,ty), lhs.as_mut());
                    self.apply_substution_expr((id,ty), rhs.as_mut());
                }
            },
            ast::Expr::FnCall(call) => self.apply_substution_fncall((id,ty), call),
            ast::Expr::If(ast::IfExpr{ cond, true_branch, else_ifs, else_branch, id:ifid, result, .. }) => {
                if ifid == id {
                    *result = ty.clone();
                } else {
                    self.apply_substution_expr((id,ty), cond.as_mut());
                    for stmnt in &mut true_branch.0 {
                        self.apply_substution_statement((id,ty), stmnt);
                    }
                    self.apply_substution_expr((id,ty), true_branch.1.as_mut());

                    for (cond,block,ret) in else_ifs {
                        self.apply_substution_expr((id,ty), cond.as_mut());
                        for stmnt in block{
                            self.apply_substution_statement((id,ty), stmnt);
                        }
                        self.apply_substution_expr((id,ty), ret.as_mut());
                    }

                    for stmnt in &mut else_branch.0 {
                        self.apply_substution_statement((id,ty), stmnt);
                    }
                    self.apply_substution_expr((id,ty), else_branch.1.as_mut());
                }
            },
            ast::Expr::Match(match_) => self.apply_substution_match((id,ty), match_),
            _ => (),
        }
    }

    fn apply_substution_match(&self, sub:(&usize, &ResolvedType), match_: &mut ast::Match) {
        let ast::Match {on,id,arms, .. } = match_;
        if id != sub.0 {
            self.apply_substution_expr(sub, on.as_mut());
            for ast::MatchArm {block, ret, ..} in arms {
                for stmnt in block{
                    self.apply_substution_statement(sub, stmnt);
                }
                if let Some(ret) = ret {
                    self.apply_substution_expr(sub, ret.as_mut())
                }
            }
        }

    }

    fn apply_substution_fncall(&self, sub:(&usize,&ResolvedType), call : &mut ast::FnCall) {
        let ast::FnCall{ value, arg, id, returns, ..} = call;
        if id == sub.0 {
            *returns = sub.1.clone();
        } else {
            self.apply_substution_expr(sub,value.as_mut());
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
                let ast::IfBranching {
                    cond,
                    true_branch,
                    else_ifs,
                    else_branch,
                    loc:_,
                } = if_;
                self.apply_equation_expr(cond.as_mut(), id, ty.clone());
                for stmnt in true_branch {
                    self.apply_equation_stmnt(stmnt, id, ty.clone());
                }

                for (cond, block) in else_ifs.iter_mut() {
                    self.apply_equation_expr(cond.as_mut(), id, ty.clone());
                    for stmnt in block {
                        self.apply_equation_stmnt(stmnt, id, ty.clone());
                    }
                }
                for stmnt in else_branch {
                    self.apply_equation_stmnt(stmnt, id, ty.clone());
                }
                None
            }
            ast::Statement::Match(match_) => {
                self.apply_equation_match(match_, id, ty);
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
            loc:_,
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
            loc:_
        } = fn_expr
        {
            if returns.is_unknown() || returns.is_error() {
                *returns = fn_ret.as_ref().clone();
            }
        }
        returns.replace_unkown_with(id, ty);
        returns.clone()
    }

    fn apply_equation_match(
        &self,
        match_: &mut ast::Match,
        id: usize,
        ty: ResolvedType,
    ) -> ResolvedType {
        let ast::Match {
            loc:_,
            on,
            arms,
            id: _,
        } = match_;
        self.apply_equation_expr(on.as_mut(), id, ty.clone());
        let mut ret_ty = types::ERROR;
        for ast::MatchArm {
            block,
            ret,
            cond: _,//TODO! for DU patterns and binding patterns
            loc : _,
        } in arms
        {
            for stmnt in block {
                self.apply_equation_stmnt(stmnt, id, ty.clone());
            }
            if let Some(ret) = ret.as_mut() {
                ret_ty = self.apply_equation_expr(ret, id, ty.clone());
            } else if !ret_ty.is_error() {
                *ret = Some(ast::Expr::Error(0).boxed())
            }
        }
        ret_ty
    }

    fn replace_one_level(&self, expr: &mut ast::Expr, ty: ResolvedType) {
        match expr {
            ast::Expr::NumericLiteral {
                value:_,
                id:_,
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
            ast::Expr::NumericLiteral {
                value:_,
                id: _,
                ty: l_ty,
            } => {
                l_ty.replace_unkown_with(id, ty);
                if l_ty.is_unknown() {
                    types::NUMBER
                } else {
                    l_ty.clone()
                }
            }

            ast::Expr::BinaryOpCall(ast::BinaryOpCall {
                loc:_,
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
                            loc:_
                        } = result
                        else {
                            unreachable!()
                        };
                        let ResolvedType::Function {
                            arg: rhs_t,
                            returns,
                            loc:_
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
                    underlining: underlining.boxed(),
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
                    loc:(0,0)
                }
            }
            ast::Expr::StructConstruction(_) => todo!(),
            ast::Expr::If(ast::IfExpr {
                cond,
                true_branch,
                else_ifs,
                else_branch,
                loc: _,
                id: _,
                result,
            }) => {
                self.apply_equation_expr(cond.as_mut(), id, ty.clone());
                for stmnt in &mut true_branch.0 {
                    self.apply_equation_stmnt(stmnt, id, ty.clone());
                }
                let true_result = self.apply_equation_expr(true_branch.1.as_mut(), id, ty.clone());
                if result.is_unknown() || result.is_error() {
                    *result = true_result;
                }
                for (cond, block, ret) in else_ifs.iter_mut() {
                    self.apply_equation_expr(cond.as_mut(), id, ty.clone());
                    for stmnt in block {
                        self.apply_equation_stmnt(stmnt, id, ty.clone());
                    }
                    let block_result = self.apply_equation_expr(ret.as_mut(), id, ty.clone());
                    if result.is_unknown() || result.is_error() {
                        *result = block_result;
                    }
                }
                for stmnt in &mut else_branch.0 {
                    self.apply_equation_stmnt(stmnt, id, ty.clone());
                }
                let else_result = self.apply_equation_expr(else_branch.1.as_mut(), id, ty.clone());
                if result.is_unknown() || result.is_error() {
                    *result = else_result;
                }
                result.replace_unkown_with(id, ty);
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
    use std::collections::HashMap;

    use crate::{
        inference::ast::ValueDeclaration,
        types::{self, ResolvedType},
        util::ExtraUtilFunctions,
    };

    #[test]
    fn phase1() {
        // let foo (a:int32) = a
        let ast = super::untyped_ast::ModuleDeclaration {
            loc: None,
            name: "foo".to_string(),
            declarations: vec![super::untyped_ast::Declaration::Value(
                super::untyped_ast::ValueDeclaration {
                    loc: (0, 0),
                    is_op: false,
                    ident: "foo".to_string(),
                    args: vec![super::untyped_ast::ArgDeclaration {
                        loc: (0, 0),
                        ident: "a".to_string(),
                        ty: Some(types::INT32),
                    }],
                    ty: None,
                    value: super::untyped_ast::ValueType::Expr(
                        super::untyped_ast::Expr::ValueRead("a".to_string(), (0, 0)),
                    ),
                    generictypes: None,
                    abi:None
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
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (0, 0),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration {
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
                        abi:None,
                        id: 0
                    }
                )]
            },
            "super simple.  `let foo (a:int32) = a"
        );
        ctx.reset();
        let ast =
            crate::Parser::from_source(r"let foo a = if a then 0 else 1").module("foo".to_string());
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (0, 4),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (0, 8),
                            ident: "a".to_string(),
                            ty: ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::If(
                            super::ast::IfExpr {
                                cond: super::ast::Expr::ValueRead("a".to_string(), (0, 15), 3)
                                    .boxed(),
                                true_branch: (
                                    Vec::new(),
                                    super::ast::Expr::NumericLiteral {
                                        value: "0".to_string(),
                                        id: 4,
                                        ty: types::NUMBER,
                                    }
                                    .boxed()
                                ),
                                else_ifs: Vec::new(),
                                else_branch: (
                                    Vec::new(),
                                    super::ast::Expr::NumericLiteral {
                                        value: "1".to_string(),
                                        id: 5,
                                        ty: types::NUMBER,
                                    }
                                    .boxed()
                                ),
                                loc: (0, 12),
                                id: 2,
                                result: ResolvedType::Unknown(2),
                            }
                        )),
                        generics: None,
                        abi:None,
                        id: 0
                    }
                )]
            },
            ctx.assign_ids_module(ast.ast),
            "with if expr",
        );
        ctx.reset();
        let ast = crate::Parser::from_source(
            r"
let foo a = match a where
    | 0 -> 'a',
    | 1 -> 'b',
    | 2 -> 'c',
    | _ -> 'd',
",
        )
        .module("foo".to_string());
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (1, 4),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (1, 8),
                            ident: "a".to_string(),
                            ty: types::ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: types::ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::Match(
                            super::ast::Match {
                                loc: (1, 12),
                                on: super::ast::Expr::ValueRead("a".to_string(), (1, 18), 3)
                                    .boxed(),
                                arms: vec![
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("a".to_string()).boxed()
                                        ),
                                        cond: (
                                            super::untyped_ast::Pattern::ConstNumber(
                                                "0".to_string()
                                            ),
                                            4
                                        ),
                                        loc: (2, 6)
                                    },
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("b".to_string()).boxed()
                                        ),
                                        cond: (
                                            super::untyped_ast::Pattern::ConstNumber(
                                                "1".to_string()
                                            ),
                                            5
                                        ),
                                        loc: (3, 6)
                                    },
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("c".to_string()).boxed()
                                        ),
                                        cond: (
                                            super::untyped_ast::Pattern::ConstNumber(
                                                "2".to_string()
                                            ),
                                            6
                                        ),
                                        loc: (4, 6)
                                    },
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("d".to_string()).boxed()
                                        ),
                                        cond: (super::untyped_ast::Pattern::Default, 7),
                                        loc: (5, 6)
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
            ctx.assign_ids_module(ast.ast),
        );
        ctx.reset();
        let ast = crate::Parser::from_source("let foo a = a == 3").module("foo".to_string());
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (0, 4),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (0, 8),
                            ident: "a".to_string(),
                            ty: ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::BinaryOpCall(
                            super::ast::BinaryOpCall {
                                loc: (0, 14),
                                lhs: super::ast::Expr::ValueRead("a".to_string(), (0, 12), 3)
                                    .boxed(),
                                rhs: super::ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                    id: 4,
                                    ty: types::NUMBER,
                                }
                                .boxed(),
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
            ctx.assign_ids_module(ast.ast),
            "binary op"
        );
    }

    #[test]
    fn generic_tying() {
        const SRC: &'static str = r#"
for<T> let foo x y : T -> T -> () = ()
"#;
        let ast = crate::Parser::from_source(SRC).module("foo".to_string());
        let mut ctx = super::Context::new(
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );

        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (1, 11),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![
                            super::ast::ArgDeclaration {
                                loc: (1, 15),
                                ident: "x".to_string(),
                                ty: ResolvedType::Generic {
                                    name: "T".to_string(),
                                    loc:(1,21)
                                },
                                id: 1
                            },
                            super::ast::ArgDeclaration {
                                loc: (1, 17),
                                ident: "y".to_string(),
                                ty: ResolvedType::Generic {
                                    name: "T".to_string(),
                                    loc:(1,26)
                                },
                                id: 2
                            },
                        ],
                        ty: ResolvedType::Function {
                            arg:ResolvedType::Generic {
                                name: "T".to_string(),
                                loc:(1,21)
                            }.boxed(),
                            returns : ResolvedType::Function {
                                arg: ResolvedType::Generic {
                                    name: "T".to_string(),
                                    loc:(1,26)
                                }.boxed(),
                                returns: types::UNIT.boxed(),
                                loc:(1,28)
                            }.boxed(),
                            loc:(1,23)
                        },
                        value: super::ast::ValueType::Expr(super::ast::Expr::UnitLiteral),
                        generics: Some(crate::ast::GenericsDecl { 
                            for_loc: (1,0),
                            decls: vec![
                                ((1,4),"T".to_string())
                            ] 
                        }),
                        abi: None,
                        id: 0
                    }
                )]
            },
            ctx.assign_ids_module(ast.ast),
        )
    }

    #[test]
    fn finale() {
        const SRC: &'static str = r#" 


let annotated_arg (x:int32) = [x,1,2,3]

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

        let ast = crate::Parser::from_source(SRC).module("foo".to_string());
        let mut ctx = super::Context::new(
            [
                ("simple".to_string(), Vec::new()),
                ("annotated_arg".to_string(), Vec::new()),
                (
                    "complex".to_string(),
                    vec!["print_int32".to_string(), "annoated_arg".to_string()],
                ),
            ]
            .into(),
            [("print_int32".to_string(), types::INT32.fn_ty(&types::UNIT))].into(),
            HashMap::new(),
            [(
                "==".to_string(),
                vec![types::INT32.fn_ty(&types::INT32.fn_ty(&types::BOOL))],
            )]
            .into(),
            HashMap::new(),
        );
        let mut ast = ctx.assign_ids_module(ast.ast);
        ctx.try_to_infer(&mut ast);
        ctx.apply_equations(&mut ast);
        ast.decls.sort_by_key(|it| it.get_ident());
        assert_eq!(
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![
                    super::ast::Declaration::Value(super::ast::ValueDeclaration {
                        loc: (3, 4),
                        is_op: false,
                        ident: "annotated_arg".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (3, 19),
                            ident: "x".to_string(),
                            ty: types::INT32,
                            id: 1
                        },],
                        ty: ResolvedType::Function {
                            arg:types::INT32.boxed(),
                            returns: ResolvedType::Array {
                                underlining: types::INT32.boxed(),
                                size: 4
                            }.boxed(),
                            loc:(0,0)
                        },
                        value: super::ast::ValueType::Expr(super::ast::Expr::ArrayLiteral {
                            contents: vec![
                                super::ast::Expr::ValueRead("x".to_string(), (3, 31), 3),
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
                            loc: (3, 30),
                            id: 2
                        }),
                        generics: None,
                        abi: None,
                        id: 0
                    }),
                    super::ast::Declaration::Value(super::ast::ValueDeclaration {
                        loc: (5, 4),
                        is_op: false,
                        ident: "complex".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (5, 12),
                            ident: "x".to_string(),
                            ty: types::INT32,
                            id: 8
                        },],
                        ty: types::INT32.fn_ty(&ResolvedType::Array {
                            underlining: types::INT32.boxed(),
                            size: 4
                        }),
                        value: super::ast::ValueType::Function(vec![
                            super::ast::Statement::FnCall(super::ast::FnCall {
                                loc: (6, 4),
                                value: super::ast::Expr::ValueRead(
                                    "print_int32".to_string(),
                                    (6, 4),
                                    10
                                )
                                .boxed(),
                                arg: super::ast::Expr::ValueRead("x".to_string(), (6, 16), 9)
                                    .boxed(),
                                id: 11,
                                returns: types::UNIT
                            }),
                            super::ast::Statement::Return(
                                super::ast::Expr::If(super::ast::IfExpr {
                                    cond: super::ast::Expr::BinaryOpCall(
                                        super::ast::BinaryOpCall {
                                            loc: (7, 16),
                                            lhs: super::ast::Expr::ValueRead(
                                                "x".to_string(),
                                                (7, 14),
                                                14
                                            )
                                            .boxed(),
                                            rhs: super::ast::Expr::NumericLiteral {
                                                value: "0".to_string(),
                                                id: 15,
                                                ty: types::INT32
                                            }
                                            .boxed(),
                                            operator: "==".to_string(),
                                            id: 13,
                                            result: types::BOOL
                                        }
                                    )
                                    .boxed(),
                                    true_branch: (
                                        Vec::new(),
                                        super::ast::Expr::ArrayLiteral {
                                            contents: vec![
                                                super::ast::Expr::ValueRead(
                                                    "x".to_string(),
                                                    (7, 27),
                                                    17
                                                ),
                                                super::ast::Expr::NumericLiteral {
                                                    value: "0".to_string(),
                                                    id: 18,
                                                    ty: types::INT32
                                                },
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
                                            ],
                                            loc: (7, 26),
                                            id: 16
                                        }
                                        .boxed()
                                    ),
                                    else_ifs: Vec::new(),
                                    else_branch: (
                                        Vec::new(),
                                        super::ast::Expr::FnCall(super::ast::FnCall {
                                            loc: (7, 41),
                                            value: super::ast::Expr::ValueRead(
                                                "annotated_arg".to_string(),
                                                (7, 41),
                                                22
                                            )
                                            .boxed(),
                                            arg: super::ast::Expr::ValueRead(
                                                "x".to_string(),
                                                (7, 55),
                                                21
                                            )
                                            .boxed(),
                                            id: 23,
                                            returns: ResolvedType::Array {
                                                underlining: types::INT32.boxed(),
                                                size: 4
                                            }
                                        })
                                        .boxed()
                                    ),
                                    loc: (7, 11),
                                    id: 12,
                                    result: ResolvedType::Array {
                                        underlining: types::INT32.boxed(),
                                        size: 4
                                    }
                                }),
                                (7, 4)
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
let int_unit _ : int32 -> () = ()

let unit_int _ : () -> int16 = 0

let int_int x : int32 -> int32 = x

let unit_unit _ : () -> () = ()
";

        let module = crate::parser::Parser::from_source(SRC).module("test".to_string());

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

        let mut module = ctx.inference(module.ast);

        module.decls.sort_by_key(super::ast::Declaration::get_ident);
        let [
            int_int,
            int_unit,
            unit_int,
            unit_unit
        ] = &module.decls[..] else { unreachable!()};
        assert_eq!(
            &super::ast::Declaration::Value(ValueDeclaration {
                loc: (5,4),
                is_op: false,
                ident: "int_int".to_string(),
                args: vec![
                    super::ast::ArgDeclaration{
                        loc: (5,12),
                        ident: "x".to_string(),
                        ty: types::INT32,
                        id: 6
                    }
                ],
                ty: ResolvedType::Function{
                    arg:types::INT32.boxed(),
                    returns:types::INT32.boxed(),
                    loc:(5, 22)
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::ValueRead("x".to_string(), (5,33), 7)),
                generics: None,
                abi: None,
                id: 5
            }),
            int_int,
            "int_int"
        );
        assert_eq!(
            &super::ast::Declaration::Value(ValueDeclaration {
                loc: (1,4),
                is_op: false,
                ident: "int_unit".to_string(),
                args: vec![
                    super::ast::ArgDeclaration{
                        loc: (1,13),
                        ident: "_".to_string(),
                        ty: types::INT32,
                        id: 1
                    }
                ],
                ty: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::UNIT.boxed(),
                    loc:(1,23),
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
            &super::ast::Declaration::Value(ValueDeclaration {
                loc: (3,4),
                is_op: false,
                ident: "unit_int".to_string(),
                args: vec![
                    super::ast::ArgDeclaration{
                        loc: (3,13),
                        ident: "_".to_string(),
                        ty: types::UNIT,
                        id: 3
                    }
                ],
                ty: ResolvedType::Function {
                    arg: types::UNIT.boxed(),
                    returns: types::INT16.boxed(),
                    loc: (3, 20) 
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::NumericLiteral { value: "0".to_string(), id: 4, ty: types::INT16 }),
                generics: None,
                abi: None,
                id: 2
            }),
            unit_int,
            "unit_int"
        );
        assert_eq!(
            &super::ast::Declaration::Value(ValueDeclaration {
                loc: (7,4),
                is_op: false,
                ident: "unit_unit".to_string(),
                args: vec![
                    super::ast::ArgDeclaration{
                        loc: (7,14),
                        ident: "_".to_string(),
                        ty: types::UNIT,
                        id: 9
                    }
                ],
                ty: ResolvedType::Function {
                    arg:types::UNIT.boxed(),
                    returns:types::UNIT.boxed(),
                    loc:(7, 21)
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
                id: 8
            }),
            unit_unit,
            "unit_unit"
        );
    }

    #[test]
    fn if_expr() {
        const SRC : &'static str = "
let if_expr a b : bool -> int32 -> int32 = if a then b else 0
";

        let module = crate::Parser::from_source(SRC).module("".to_string());
        let mut ctx = super::Context::new(
            [("if_expr".to_string(),Vec::new())].into(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new()
        );
        let module = ctx.inference(module.ast);

        let [if_expr] = &module.decls[..] else { unreachable!() };
        assert_eq!(
            &super::ast::Declaration::Value(super::ast::ValueDeclaration{
                loc:(1,4),
                is_op:false,
                ident : "if_expr".to_string(),
                args: vec![
                    super::ast::ArgDeclaration {
                        loc: (1,12),
                        ident: "a".to_string(),
                        ty: types::BOOL,
                        id: 1,
                    },
                    super::ast::ArgDeclaration {
                        loc: (1,14),
                        ident: "b".to_string(),
                        ty: types::INT32,
                        id: 2,
                    },
                ],
                ty:ResolvedType::Function { 
                    arg: types::BOOL.boxed(),
                    returns:ResolvedType::Function {
                        arg:types::INT32.boxed(),
                        returns:types::INT32.boxed(),
                        loc:(1, 32)
                    }.boxed(),
                    loc:(1, 23)
                },
                value: super::ast::ValueType::Expr(super::ast::Expr::If(super::ast::IfExpr {
                    cond: super::ast::Expr::ValueRead("a".to_string(), (1,46), 4).boxed(),
                    true_branch: (
                        Vec::new(),
                        super::ast::Expr::ValueRead("b".to_string(), (1,53), 5).boxed()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        Vec::new(),
                        super::ast::Expr::NumericLiteral { value: "0".to_string(), id: 6, ty: types::INT32 }.boxed()
                    ),
                    loc: (1,43),
                    id: 3,
                    result: types::INT32 
                })),
                generics:None,
                abi: None,
                id:0,
            }),
            if_expr,
        )
    }

    #[test]
    fn returns() {
        const SRC : &'static str = "
let returns a : bool -> int32 =
    if a then
        return 0;
    return 1;
";

        let module = crate::Parser::from_source(SRC).module("".to_string());
        let mut ctx = super::Context::new(
            [("returns".to_string(),Vec::new())].into(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new()
        );
        let module = ctx.inference(module.ast);

        let [returns] = &module.decls[..] else {unreachable!()};
        assert_eq!(
            &super::ast::Declaration::Value(super::ast::ValueDeclaration{
                loc:(1,4),
                is_op:false,
                ident : "returns".to_string(),
                args: vec![
                    super::ast::ArgDeclaration {
                        loc: (1,12),
                        ident: "a".to_string(),
                        ty: types::BOOL,
                        id: 1,
                    },
                ],
                ty:ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: types::INT32.boxed(),
                    loc: (1,21) 
                },
                value:super::ast::ValueType::Function(vec![
                    super::ast::Statement::IfStatement(super::ast::IfBranching{
                        cond: super::ast::Expr::ValueRead("a".to_string(), (2,7), 2).boxed(),
                        true_branch: vec![
                            super::ast::Statement::Return(
                                super::ast::Expr::NumericLiteral { value: "0".to_string(), id: 3, ty: types::INT32 },
                                (3,8)
                            )
                        ],
                        else_ifs: Vec::new(),
                        else_branch: Vec::new(),
                        loc: (2,4)
                    }),
                    super::ast::Statement::Return(
                        super::ast::Expr::NumericLiteral { value: "1".to_string(), id: 4, ty: types::INT32 },
                        (4,4)
                    )
                ]),
                generics:None,
                abi: None,
                id:0
            }),
            returns,
        );
    }
}

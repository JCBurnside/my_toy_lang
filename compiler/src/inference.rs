use std::collections::HashMap;

use bimap::BiMap;
use itertools::Itertools;

use crate::{
    ast as untyped_ast,
    types::{self, ResolvedType},
    util::ExtraUtilFunctions,
};

mod ast;

pub(crate) struct Context {
    next_unknown_id: usize,
    next_expr_id: usize,
    // dependency_tree: HashMap<String, Vec<String>>,
    known_types: HashMap<String, ResolvedType>,
    known_struct_fields: HashMap<(String, String), ResolvedType>,
    known_generic_types: HashMap<String, untyped_ast::TypeDefinition>,
    known_ops: HashMap<String, Vec<ResolvedType>>,
    known_values: BiMap<usize, ResolvedType>,
    known_locals: HashMap<String, ResolvedType>,
    equations: HashMap<usize, ResolvedType>, //? unsure of type here yet.
}
impl Context {
    pub(crate) fn new(
        // dependency_tree: HashMap<String, Vec<String>>,
        known_types: HashMap<String, ResolvedType>,
        known_struct_fields: HashMap<(String, String), ResolvedType>,
        known_ops: HashMap<String, Vec<ResolvedType>>,
        known_generic_types: HashMap<String, untyped_ast::TypeDefinition>,
    ) -> Self {
        Self {
            next_unknown_id: 0,
            next_expr_id: 0,
            // dependency_tree,
            known_types,
            known_struct_fields,
            known_generic_types,
            known_ops,
            known_values: BiMap::new(),
            known_locals: HashMap::new(),
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
            untyped_ast::Declaration::TypeDefinition(_) => todo!(),
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
        } = val;

        let id = self.get_next_expr_id();
        let decl_ty = ty.unwrap_or_else(|| self.get_next_type_id());
        self.known_values.insert(id, decl_ty.clone());
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
                self.known_values.insert(id, ty.clone());
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
        };
        ast::ValueDeclaration {
            loc,
            is_op,
            ident,
            args,
            ty: decl_ty,
            value,
            generics: generictypes,
            id,
        }
    }

    fn assign_ids_stmnt(&mut self, stmnt: untyped_ast::Statement) -> ast::Statement {
        match stmnt {
            untyped_ast::Statement::Declaration(decl) => {
                let decl = self.assign_ids_value_decl(decl);
                self.known_values.insert(decl.id, decl.ty.clone());
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
                    self.get_next_type_id()
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
            untyped_ast::Expr::Compose { lhs, rhs } => todo!(),
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
            untyped_ast::Expr::ListLiteral { contents, loc } => todo!(),
            untyped_ast::Expr::TupleLiteral { contents, loc } => todo!(),
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
    fn try_to_infer(&mut self, module: ast::ModuleDeclaration) {}

    fn get_actual_type(
        &mut self,
        expr: &mut ast::Expr,
        expected: Option<ResolvedType>,
    ) -> ResolvedType {
        match expr {
            ast::Expr::Error(_) => ResolvedType::Error,
            ast::Expr::NumericLiteral { value, id, ty } => {
                if let Some(ety) = expected {
                    *ty = if (ety.is_float() || ety.is_int()) && ty == &ResolvedType::Number
                        || &ety == ty
                    {
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
            ast::Expr::BinaryOpCall(_) => todo!(),
            ast::Expr::FnCall(ast::FnCall { value, arg, .. }) => {
                if let ResolvedType::Function {
                    arg: arg_t,
                    returns,
                } = self.get_actual_type(value.as_mut(), None)
                {
                    self.get_actual_type(arg.as_mut(), Some(arg_t.as_ref().clone()));
                    *returns
                } else {
                    ResolvedType::Error
                }
            }
            ast::Expr::ValueRead(ident, _, _) => {
                if let Some(ty) = self
                .known_locals
                .get_mut(ident) {
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
                }
                else {
                    ResolvedType::Error
                }
            },
            ast::Expr::ArrayLiteral { contents, loc, id } => todo!(),
            ast::Expr::ListLiteral { contents, loc, id } => todo!(),
            ast::Expr::StructConstruction(strct) => {
                for (name,(field,_)) in &mut strct.fields {
                    if let Some(ety) = self.known_struct_fields.get(&(strct.ident.clone(),name.clone())).cloned() {
                        self.get_actual_type(field, Some(ety));
                    }
                }
                self.known_types.get(&strct.ident).cloned().unwrap_or(ResolvedType::Error)
            }
            ast::Expr::BoolLiteral(_, _, _) => ResolvedType::Bool,
            ast::Expr::If(_) => todo!(),
            ast::Expr::Match(_) => todo!(),
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
                    generictypes: Vec::new(),
                },
            )],
        };
        let mut ctx = super::Context::new(
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
                        generics: Vec::new(),
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
            ctx.assign_ids_module(ast),
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (0, 5),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (0, 9),
                            ident: "a".to_string(),
                            ty: ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::If(
                            super::ast::IfExpr {
                                cond: super::ast::Expr::ValueRead("a".to_string(), (0, 16), 3)
                                    .boxed(),
                                true_branch: (
                                    Vec::new(),
                                    super::ast::Expr::NumericLiteral {
                                        value: "0".to_string(),
                                        id: 4,
                                        ty: ResolvedType::Unknown(3),
                                    }
                                    .boxed()
                                ),
                                else_ifs: Vec::new(),
                                else_branch: (
                                    Vec::new(),
                                    super::ast::Expr::NumericLiteral {
                                        value: "1".to_string(),
                                        id: 5,
                                        ty: ResolvedType::Unknown(4),
                                    }
                                    .boxed()
                                ),
                                loc: (0, 13),
                                id: 2,
                                result: ResolvedType::Unknown(2),
                            }
                        )),
                        generics: Vec::new(),
                        id: 0
                    }
                )]
            },
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
            ctx.assign_ids_module(ast),
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (1, 5),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (1, 9),
                            ident: "a".to_string(),
                            ty: types::ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: types::ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::Match(
                            super::ast::Match {
                                loc: (1, 13),
                                on: super::ast::Expr::ValueRead("a".to_string(), (1, 19), 3)
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
                                        loc: (2, 7)
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
                                        loc: (3, 7)
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
                                        loc: (4, 7)
                                    },
                                    super::ast::MatchArm {
                                        block: Vec::new(),
                                        ret: Some(
                                            super::ast::Expr::CharLiteral("d".to_string()).boxed()
                                        ),
                                        cond: (super::untyped_ast::Pattern::Default, 7),
                                        loc: (5, 7)
                                    },
                                ],
                                id: 2
                            }
                        )),
                        generics: Vec::new(),
                        id: 0,
                    }
                )]
            }
        );
        ctx.reset();
        let ast = crate::Parser::from_source("let foo a = a == 3").module("foo".to_string());
        assert_eq!(
            ctx.assign_ids_module(ast),
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (0, 5),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![super::ast::ArgDeclaration {
                            loc: (0, 9),
                            ident: "a".to_string(),
                            ty: ResolvedType::Unknown(1),
                            id: 1
                        }],
                        ty: ResolvedType::Unknown(0),
                        value: super::ast::ValueType::Expr(super::ast::Expr::BinaryOpCall(
                            super::ast::BinaryOpCall {
                                loc: (0, 15),
                                lhs: super::ast::Expr::ValueRead("a".to_string(), (0, 13), 3)
                                    .boxed(),
                                rhs: super::ast::Expr::NumericLiteral {
                                    value: "3".to_string(),
                                    id: 4,
                                    ty: types::ResolvedType::Unknown(2),
                                }
                                .boxed(),
                                operator: "==".to_string(),
                                id: 2,
                                result: types::ResolvedType::Unknown(3),
                            }
                        )),
                        generics: Vec::new(),
                        id: 0
                    }
                )]
            },
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
        );

        assert_eq!(
            ctx.assign_ids_module(ast),
            super::ast::ModuleDeclaration {
                loc: (0, 0),
                name: "foo".to_string(),
                decls: vec![super::ast::Declaration::Value(
                    super::ast::ValueDeclaration {
                        loc: (1,12),
                        is_op: false,
                        ident: "foo".to_string(),
                        args: vec![
                            super::ast::ArgDeclaration {
                                loc: (1,16),
                                ident: "x".to_string(),
                                ty: ResolvedType::Generic { name: "T".to_string() },
                                id: 1
                            },
                            super::ast::ArgDeclaration {
                                loc: (1,18),
                                ident: "y".to_string(),
                                ty: ResolvedType::Generic { name: "T".to_string() },
                                id: 2
                            },
                        ],
                        ty: ResolvedType::Generic { name: "T".to_string() }.fn_ty(&ResolvedType::Generic { name: "T".to_string() }.fn_ty(&ResolvedType::Unit)),
                        value: super::ast::ValueType::Expr(super::ast::Expr::UnitLiteral),
                        generics: vec!["T".to_string()],
                        id: 0
                    }
                )]
            }
        )
    }


    #[test]
    fn finale() {
        const SRC : &'static str = 
r#" 
let simple x = x

let annotated_arg (x:int32) = [x,1,2,3]

let complex x =
    print_int32 x;
    if x == 0 then 
        [0,0,0,0]
    else
        annotated_arg x
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
            HashMap::new(),
            HashMap::new(),
            [("==".to_string(), vec![types::INT32.fn_ty(&types::INT32.fn_ty(&types::BOOL))])].into(),
            HashMap::new(),
        );
        let ast = ctx.assign_ids_module(ast);

    }

}

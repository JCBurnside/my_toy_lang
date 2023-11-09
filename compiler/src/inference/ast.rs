use std::collections::HashMap;

use crate::{
    ast::{EnumDeclation, Pattern, StructDefinition, TypeDefinition},
    types::{self, ResolvedType},
    util::ExtraUtilFunctions,
};

#[derive(PartialEq, Debug)]
pub(crate) struct ModuleDeclaration {
    pub(crate) loc: crate::Location,
    pub(crate) name: String,
    pub(crate) decls: Vec<Declaration>,
}

#[derive(PartialEq, Debug)]
pub(crate) enum Declaration {
    Value(ValueDeclaration),
    Type(TypeDefinition),
}

#[derive(PartialEq, Debug)]
pub(crate) struct ValueDeclaration {
    pub(crate) loc: crate::Location,
    pub(crate) is_op: bool,
    pub(crate) ident: String,
    pub(crate) args: Vec<ArgDeclaration>,
    pub(crate) ty: ResolvedType,
    pub(crate) value: ValueType,
    pub(crate) generics: Vec<String>,
    pub(crate) id: usize,
}

#[derive(PartialEq, Debug)]
pub(crate) struct ArgDeclaration {
    pub(crate) loc: crate::Location,
    pub(crate) ident: String,
    pub(crate) ty: ResolvedType,
    pub(crate) id: usize,
}

#[derive(PartialEq, Debug)]
pub(crate) enum ValueType {
    Expr(Expr),
    Function(Vec<Statement>),
}

#[derive(PartialEq, Debug)]
pub(crate) enum Statement {
    Error,
    Declaration(ValueDeclaration),
    Return(Expr, crate::Location),
    FnCall(FnCall),
    IfStatement(IfBranching),
    Match(Match),
}

#[derive(PartialEq, Debug)]
pub(crate) struct IfBranching {
    pub(crate) cond: Box<Expr>,
    pub(crate) true_branch: Vec<Statement>,
    pub(crate) else_ifs: Vec<(Box<Expr>, Vec<Statement>)>,
    pub(crate) else_branch: Vec<Statement>,
    pub(crate) loc: crate::Location,
}

#[derive(PartialEq, Debug)]
pub(crate) struct Match {
    pub(crate) loc: crate::Location,
    pub(crate) on: Box<Expr>,
    pub(crate) arms: Vec<MatchArm>,
    pub(crate) id: usize,
}

#[derive(PartialEq, Debug)]
pub(crate) struct MatchArm {
    pub(crate) block: Vec<Statement>,
    pub(crate) ret: Option<Box<Expr>>,
    pub(crate) cond: (Pattern, usize),
    pub(crate) loc: crate::Location,
}

#[derive(PartialEq, Debug)]
pub(crate) struct FnCall {
    pub(crate) loc: crate::Location,
    pub(crate) value: Box<Expr>,
    pub(crate) arg: Box<Expr>,
    pub(crate) id: usize,
    pub(crate) returns: ResolvedType,
}

#[derive(PartialEq, Debug)]
pub(crate) enum Expr {
    Error(usize),
    NumericLiteral {
        value: String,
        id: usize,
        ty: ResolvedType,
    },
    StringLiteral(String),
    CharLiteral(String),
    UnitLiteral,
    BinaryOpCall(BinaryOpCall),
    FnCall(FnCall),
    ValueRead(String, crate::Location, usize),
    ArrayLiteral {
        contents: Vec<Expr>,
        loc: crate::Location,
        id: usize,
    },
    ListLiteral {
        contents: Vec<Expr>,
        loc: crate::Location,
        id: usize,
    },
    StructConstruction(StructConstruction),
    BoolLiteral(bool, crate::Location, usize),
    If(IfExpr),
    Match(Match),
}
impl Expr {
    pub(crate) fn get_retty(&self, ctx: &mut crate::inference::Context) -> ResolvedType {
        match self {
            Expr::Error(_) => ResolvedType::Error,
            Expr::NumericLiteral { ty, .. } => ty.clone(),
            Expr::StringLiteral(_) => types::STR,
            Expr::CharLiteral(_) => types::CHAR,
            Expr::UnitLiteral => types::UNIT,
            Expr::BinaryOpCall(bin) => bin.result.clone(),
            Expr::FnCall(call) => call.returns.clone(),
            Expr::ValueRead(ident, _, _) => {
                let ty = ctx.known_types.get(ident).map(|ty| ty.clone());
                ty.unwrap_or_else(|| ctx.get_next_type_id())
            }
            Expr::ArrayLiteral { contents, .. } => {
                if contents.is_empty() {
                    ResolvedType::Array {
                        underlining: ctx.get_next_type_id().boxed(),
                        size: 0,
                    }
                } else {
                    ResolvedType::Array {
                        underlining: contents.first().unwrap().get_retty(ctx).boxed(),
                        size: contents.len(),
                    }
                }
            }
            Expr::ListLiteral { contents, loc, id } => {
                todo!()
            }
            Expr::StructConstruction(strct) => ResolvedType::User {
                name: strct.ident.clone(),
                generics: strct.generics.clone(),
            },
            Expr::BoolLiteral(_, _, _) => types::BOOL,
            Expr::If(if_) => {
                todo!()
            }
            Expr::Match(_) => todo!(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub(crate) struct BinaryOpCall {
    pub(crate) loc: crate::Location,
    pub(crate) lhs: Box<Expr>,
    pub(crate) rhs: Box<Expr>,
    pub(crate) operator: String,
    pub(crate) id: usize,
    pub(crate) result: ResolvedType,
}

#[derive(PartialEq, Debug)]
pub(crate) struct StructConstruction {
    pub(crate) loc: crate::Location,
    pub(crate) fields: HashMap<String, (Expr, crate::Location)>,
    pub(crate) generics: Vec<ResolvedType>,
    pub(crate) ident: String,
    pub(crate) id: usize,
    pub(crate) result: ResolvedType,
}

#[derive(PartialEq, Debug)]
pub(crate) struct IfExpr {
    pub(crate) cond: Box<Expr>,
    pub(crate) true_branch: (Vec<Statement>, Box<Expr>),
    pub(crate) else_ifs: Vec<(Box<Expr>, Vec<Statement>, Box<Expr>)>,
    pub(crate) else_branch: (Vec<Statement>, Box<Expr>),
    pub(crate) loc: crate::Location,
    pub(crate) id: usize,
    pub(crate) result: ResolvedType,
}

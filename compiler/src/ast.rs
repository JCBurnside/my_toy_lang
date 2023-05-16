use std::{collections::{HashMap, HashSet}, iter::once, rc::Rc};

use inkwell::{
    context::Context,
};
use itertools::Itertools;

use crate::{
    util::ExtraUtilFunctions, 
    types::{self, ResolvedType, TypeResolver, resolve_from_name}
};

type File = ModuleDeclaration;
type Program = Vec<File>;

struct ModuleDeclaration {
    name : Option<String>,
    declarations : Vec<Declaration>
}

enum Declaration {
    Mod(ModuleDeclaration),
    Value(ValueDeclaration)
}

enum ValueType {
    Expr(Expr),
    Function(Vec<Statement>)
}

struct ValueDeclaration {
    name : String,
    arg : Vec<String>,
    ty : ResolvedType,
    value : ValueType,//need to figure out this. as value would be expr here but function would be a list of statements
}

enum Statement {
    Declaration(ValueDeclaration),
    Return(Expr),
    FnCall(FnCall),
    BinaryOpCall(BinaryOpCall),
    UnaryOpCall(UnaryOpCall)
}

struct UnaryOpCall { operand : Box<Expr>, operator : String }

struct BinaryOpCall { lhs : Box<Expr>, rhs: Box<Expr>, operator : String }

struct FnCall {
    value : Box<Expr>,
    arg : Option<Box<Expr>>,
}

enum Expr {
    /// any integer or floating point value
    NumericLiteral { value : String, ty : ResolvedType },
    /// "hello world!"
    StringLiteral { value : String, },
    /// `'a'`
    CharLiteral { value : String, },
    /// ()
    UnitLiteral,
    /// `a + b`. come on everyone should know this one
    BinaryOpCall(BinaryOpCall),
    /// NOT IMPLEMENTED YET
    UnaryOpCall(UnaryOpCall),
    /// eg `foo bar` (composed of two [`ValueRead`]s in this example)
    FnCall(FnCall),
    /// basically an ident on it's own
    ValueRead { name:String },
    /// NOT IMPLEMENTED YET
    /// defined like [ expr, expr, expr ]
    /// type [T;N]
    ArrayLiteral { contents : Vec<Expr> },
    /// NOT IMPLEMENTED YET
    /// defined like [| expr, expr, expr |]
    /// type [|T|]
    ListLiteral { contents : Vec<Expr> },
}

// #[derive(PartialEq, Debug, Clone)]
// #[allow(unused)]
// pub enum Expr {
//     BinaryOpCall {
//         ident: String,
//         lhs: Box<Expr>,
//         rhs: Box<Expr>,
//     },
//     UnaryOpCall {
//         ident: String,
//         operand: Box<Expr>,
//     },
//     FnCall {
//         value: Box<Expr>,//ideally this should be of FnCall | Value.  checking needed
//         arg: Option<Box<Expr>>,
//     },
//     Return {
//         expr: Box<Expr>,
//     },
//     Block {
//         sub: Vec<Expr>,
//     },
//     Literal {
//         value: String,
//         ty: TypeName,
//     },
//     ValueRead {
//         ident : String,
//     },
//     Declaration {
//         is_op: bool,
//         ident: String,
//         ty: Option<TypeName>,
//         args: Option<Vec<(String, Option<TypeName>)>>,
//         value: Box<Expr>,
//         generictypes: Vec<String>
//     },
//     UnitLiteral,
// }

#[derive(PartialEq, Debug, Clone)]
pub enum TypeName {
    FnType(Rc<TypeName>, Rc<TypeName>),
    ValueType(String),
}
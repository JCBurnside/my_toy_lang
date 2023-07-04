use std::{collections::HashSet, num::NonZeroU8};

use crate::types::ResolvedType;

pub(crate) type File = ModuleDeclaration;
pub(crate) type Program = Vec<File>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ModuleDeclaration {
    pub(crate) loc: Option<crate::Location>,
    pub(crate) name: Option<String>,
    pub(crate) declarations: Vec<Declaration>,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum Declaration {
    Mod(ModuleDeclaration),
    Value(ValueDeclaration),
    /// TODO
    TypeDefinition(TypeDefinition),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TypeDefinition {
    Alias(String, ResolvedType),
    Enum(String, Vec<EnumVariant>, crate::Location),
    Struct(String, StructDefinition, crate::Location),
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct StructDefinition {
    pub(crate) values: Vec<(String, ResolvedType)>,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum EnumVariant {
    Unit(crate::Location),
    Tuple(Vec<ResolvedType>, crate::Location),
    Struct(StructDefinition, crate::Location),
}

#[derive(PartialEq, Eq, Debug)]
pub enum ValueType {
    Expr(Expr),
    Function(Vec<Statement>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub(crate) struct ArgDeclation {
    pub(crate) loc: crate::Location,
    pub(crate) ident: String,
    // ty : Option<ResolvedType>,// TODO
}

#[derive(PartialEq, Eq, Debug)]
pub struct ValueDeclaration {
    pub(crate) loc: crate::Location, //should be location of the ident.
    pub(crate) is_op: bool,
    pub(crate) ident: String,
    pub(crate) args: Vec<ArgDeclation>,
    pub(crate) ty: Option<ResolvedType>,
    pub(crate) value: ValueType, //need to figure out this. as value would be expr here but function would be a list of statements
    pub(crate) generictypes: HashSet<String>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    Declaration(ValueDeclaration),
    Return(Expr, crate::Location),
    FnCall(FnCall),
    Pipe(Pipe),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Pipe {
    ///if you need to spread more than an u8's worth of values... wtf are you doing? what would even return that many values as a tuple?  
    pub(crate) expansion: NonZeroU8,
    pub(crate) lhs: Box<Expr>,
    /// THIS HAS A RESTRICTION OF MUST RETURN A FUNCTION
    pub(crate) rhs: Box<Expr>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct UnaryOpCall {
    pub(crate) operand: Box<Expr>,
    pub(crate) operator: String,
}

#[derive(PartialEq, Eq, Debug)]
pub struct BinaryOpCall {
    pub(crate) loc: crate::Location,
    pub(crate) lhs: Box<Expr>,
    pub(crate) rhs: Box<Expr>,
    pub(crate) operator: String,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FnCall {
    pub(crate) loc: crate::Location,
    pub(crate) value: Box<Expr>,
    pub(crate) arg: Option<Box<Expr>>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expr {
    /// any integer or floating point value
    NumericLiteral { value: String, ty: ResolvedType },
    /// "hello world!"
    StringLiteral(String),
    /// `'a'`
    CharLiteral(String),
    /// ()
    UnitLiteral,
    /// a >>> b
    Compose { lhs: Box<Expr>, rhs: Box<Expr> },
    /// `a + b`. come on everyone should know this one
    BinaryOpCall(BinaryOpCall),
    /// NOT IMPLEMENTED YET
    UnaryOpCall(UnaryOpCall),
    /// eg `foo bar` (composed of two [`ValueRead`]s in this example)
    FnCall(FnCall),
    /// basically an ident on it's own
    ValueRead(String),
    /// NOT IMPLEMENTED YET
    /// defined like [ expr, expr, expr, ... ]
    /// type [T;N]
    ArrayLiteral {
        contents: Vec<Expr>,
        loc: crate::Location,
    },
    /// NOT IMPLEMENTED YET
    /// defined like [| expr, expr, expr, ... |]
    /// type [|T|]
    ListLiteral {
        contents: Vec<Expr>,
        loc: crate::Location,
    },
    /// NOT IMPLEMENTED YET
    /// defined like (expr, expr,...)
    /// typed as (T, U, V, ...)
    /// not recommended above 3 values
    TupleLiteral {
        contents: Vec<Expr>,
        loc: crate::Location,
    },
}

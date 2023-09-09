use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU8,
};

use crate::types::ResolvedType;
#[allow(unused)]
pub(crate) type File = ModuleDeclaration;
#[allow(unused)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum TypeDefinition {
    Alias(String, ResolvedType),
    Enum(EnumDeclation),
    Struct(StructDefinition),
}

impl TypeDefinition {
    pub(crate) fn get_ident(&self) -> String {
        match self {
            TypeDefinition::Alias(name, _) => name.clone(),
            TypeDefinition::Enum(_) => todo!(),
            TypeDefinition::Struct(strct) => strct.ident.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub(crate) struct EnumDeclation {
    pub(crate) ident: String,
    pub(crate) generics: Vec<String>,
    pub(crate) values: Vec<EnumVariant>,
    pub(crate) loc: crate::Location,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub(crate) enum EnumVariant {
    Unit(String, crate::Location),
    Tuple(String, Vec<ResolvedType>, crate::Location),
    Struct(String, StructDefinition, crate::Location),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub(crate) struct StructDefinition {
    pub(crate) ident: String,
    pub(crate) generics: Vec<String>,
    pub(crate) values: Vec<FieldDecl>,
    pub(crate) loc: crate::Location,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub(crate) struct FieldDecl {
    pub(crate) name: String,
    pub(crate) ty: ResolvedType,
    pub(crate) loc: crate::Location,
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
    pub(crate) generictypes: Vec<String>,
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
    pub(crate) loc: crate::Location,
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
pub struct StructConstruction {
    pub(crate) loc: crate::Location,
    pub(crate) fields: HashMap<String, (Expr, crate::Location)>,
    pub(crate) generics: Vec<ResolvedType>,
    pub(crate) ident: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expr {
    Error,
    /// any integer or floating point value
    NumericLiteral {
        value: String,
        ty: ResolvedType,
    },
    /// "hello world!"
    StringLiteral(String),
    /// `'a'`
    CharLiteral(String),
    /// ()
    UnitLiteral,
    /// a >>> b
    Compose {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
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

    StructConstruction(StructConstruction),
}

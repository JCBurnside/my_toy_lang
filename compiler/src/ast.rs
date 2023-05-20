use std::{collections::HashSet, num::NonZeroU8, rc::Rc};

use crate::types::ResolvedType;

pub(crate) type File = ModuleDeclaration;
pub(crate) type Program = Vec<File>;

pub(crate) struct ModuleDeclaration {
    pub(crate) name: Option<String>,
    pub(crate) declarations: Vec<Declaration>,
}

pub(crate) enum Declaration {
    Mod(ModuleDeclaration),
    Value(ValueDeclaration),
    /// TODO
    TypeDefinition(TypeDefinition),
}

pub(crate) enum TypeDefinition {
    Alias(String, ResolvedType),
    Enum(String, Vec<EnumVariant>),
    Struct(String, StructDefinition),
}

pub(crate) struct StructDefinition {
    pub(crate) values: Vec<(String, ResolvedType)>,
}

pub(crate) enum EnumVariant {
    Unit,
    Tuple(Vec<ResolvedType>),
    Struct(StructDefinition),
}

#[derive(PartialEq, Eq, Debug)]
pub enum ValueType {
    Expr(Expr),
    Function(Vec<Statement>),
}

#[derive(PartialEq, Eq, Debug)]
pub struct ValueDeclaration {
    pub(crate) is_op: bool,
    pub(crate) ident: String,
    pub(crate) args: Vec<String>, //todo convert to HashMap<String,Option<ResolvedType>> eventually
    pub(crate) ty: Option<ResolvedType>,
    pub(crate) value: ValueType, //need to figure out this. as value would be expr here but function would be a list of statements
    pub(crate) generictypes: HashSet<String>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    Declaration(ValueDeclaration),
    Return(Expr),
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
    pub(crate) lhs: Box<Expr>,
    pub(crate) rhs: Box<Expr>,
    pub(crate) operator: String,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FnCall {
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
    ArrayLiteral { contents: Vec<Expr> },
    /// NOT IMPLEMENTED YET
    /// defined like [| expr, expr, expr, ... |]
    /// type [|T|]
    ListLiteral { contents: Vec<Expr> },
    /// NOT IMPLEMENTED YET
    /// defined like (expr, expr,...)
    /// typed as (T, U, V, ...)
    /// not recommended above 3 values
    TupleLiteral { contents: Vec<Expr> },
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypeName {
    FnType(Rc<TypeName>, Rc<TypeName>),
    ValueType(String),
}

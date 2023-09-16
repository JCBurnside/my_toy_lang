use std::{collections::HashMap, num::NonZeroU8};

use itertools::Itertools;

use crate::types::ResolvedType;
#[allow(unused)]
pub(crate) type File = ModuleDeclaration;
#[allow(unused)]
pub(crate) type Program = Vec<File>;
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ModuleDeclaration {
    pub(crate) loc: Option<crate::Location>,
    pub(crate) name: String,
    pub(crate) declarations: Vec<Declaration>,
}

impl ModuleDeclaration {
    pub fn canonialize(&mut self, mut path: Vec<String>) {
        path.push(self.name.clone());
        let to_remove = self
            .declarations
            .iter()
            .enumerate()
            .filter_map(|(pos, it)| match it {
                Declaration::TypeDefinition(decl) => match decl {
                    TypeDefinition::Alias(_, _) => Some(pos),
                    _ => None,
                },
                _ => None,
            })
            .collect::<Vec<_>>();
        for (offset, idx) in to_remove.into_iter().enumerate() {
            let Declaration::TypeDefinition(TypeDefinition::Alias(new,old)) = self.declarations.remove(idx-offset) else { unreachable!() };
            for decl in &mut self.declarations {
                decl.replace(&new, &old.to_string());
            }
        }
        for i in 0..self.declarations.len() {
            let (old_name, new_name) = match &mut self.declarations[i] {
                Declaration::Mod(m) => {
                    m.canonialize(path.clone());
                    continue;
                }
                Declaration::Value(v) => {
                    // todo check if externed.  if so no work needed.
                    let old = v.ident.clone();
                    v.ident = path.iter().cloned().join("::") + "::" + &v.ident;
                    (old, v.ident.clone())
                }
                Declaration::TypeDefinition(def) => {
                    let old = def.get_ident();
                    let new = path.iter().cloned().join("::") + "::" + &old;
                    def.replace(&old, &new);
                    (old, new)
                }
            };

            for decl in &mut self.declarations {
                decl.replace(&old_name, &new_name);
            }
        }
        let mut holder = Vec::with_capacity(self.declarations.capacity());
        std::mem::swap(&mut holder, &mut self.declarations);
        for decl in holder {
            match decl {
                Declaration::Mod(m) => self.declarations.extend(m.declarations),
                _ => self.declarations.push(decl),
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum Declaration {
    #[allow(unused)] //TODO submoduling
    Mod(ModuleDeclaration),
    Value(ValueDeclaration),
    /// TODO
    TypeDefinition(TypeDefinition),
}

impl Declaration {
    pub fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Mod(_) => (), //do nothing as super mod alias should not leak into submodules
            Self::TypeDefinition(def) => def.replace(nice_name, actual),
            Self::Value(decl) => decl.replace(nice_name, actual),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum TypeDefinition {
    Alias(String, ResolvedType),
    #[allow(unused)]
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

    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Alias(_, ty) => *ty = ty.replace(nice_name, actual),
            Self::Enum(_) => todo!(),
            Self::Struct(strct) => {
                if strct.ident == nice_name {
                    strct.ident = actual.to_string();
                }
                for field in &mut strct.values {
                    field.ty = field.ty.replace(nice_name, actual);
                }
            }
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
#[allow(unused)]
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
    pub(crate) value: ValueType,
    pub(crate) generictypes: Vec<String>,
}
impl ValueDeclaration {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        if let Some(ty) = self.ty.as_mut() {
            *ty = ty.replace(nice_name, actual);
        }
        self.value.replace(nice_name, actual);
    }
}
#[derive(PartialEq, Eq, Debug)]
pub enum ValueType {
    Expr(Expr),
    Function(Vec<Statement>),
}
impl ValueType {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            ValueType::Expr(expr) => expr.replace(nice_name, actual),
            ValueType::Function(stmnts) => stmnts
                .iter_mut()
                .for_each(|it| it.replace(nice_name, actual)),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    Declaration(ValueDeclaration),
    Return(Expr, crate::Location),
    FnCall(FnCall),
    Pipe(Pipe),
    IfStatement(IfBranching),
    Error,
}
impl Statement {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Statement::Declaration(decl) => decl.replace(nice_name, actual),
            Statement::Return(expr, _) => expr.replace(nice_name, actual),
            Statement::FnCall(fncall) => fncall.replace(nice_name, actual),
            Statement::Pipe(_) => todo!(),
            Statement::IfStatement(ifbranches) => ifbranches.replace(nice_name, actual),
            Statement::Error => (),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct IfBranching {
    pub(crate) cond: Box<Expr>,
    pub(crate) true_branch: Vec<Statement>,
    pub(crate) else_ifs: Vec<(Box<Expr>, Vec<Statement>)>,
    pub(crate) else_branch: Vec<Statement>,
}

impl IfBranching {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        self.cond.replace(nice_name, actual);
        self.true_branch
            .iter_mut()
            .for_each(|stmt| stmt.replace(nice_name, actual));
        self.else_ifs.iter_mut().for_each(|(cond, block)| {
            cond.replace(nice_name, actual);
            block
                .iter_mut()
                .for_each(|stmnt| stmnt.replace(nice_name, actual));
        });
        self.else_branch
            .iter_mut()
            .for_each(|stmt| stmt.replace(nice_name, actual));
    }
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
impl BinaryOpCall {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        self.lhs.replace(nice_name, actual);
        self.rhs.replace(nice_name, actual);
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct FnCall {
    pub(crate) loc: crate::Location,
    pub(crate) value: Box<Expr>,
    pub(crate) arg: Option<Box<Expr>>,
}
impl FnCall {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        self.value.replace(nice_name, actual);
        self.arg.as_mut().map(|it| it.replace(nice_name, actual));
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct StructConstruction {
    pub(crate) loc: crate::Location,
    pub(crate) fields: HashMap<String, (Expr, crate::Location)>,
    pub(crate) generics: Vec<ResolvedType>,
    pub(crate) ident: String,
}
impl StructConstruction {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        if self.ident == nice_name {
            self.ident = actual.to_string();
        }
        for (it, _) in self.fields.values_mut() {
            it.replace(nice_name, actual);
        }
        for it in &mut self.generics {
            *it = it.replace(nice_name, actual);
        }
    }
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
    ValueRead(String, crate::Location),

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
    BoolLiteral(bool,crate::Location),
    If(IfExpr),
}
impl Expr {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Expr::ValueRead(read, _) if read == nice_name => {
                *read = actual.to_string();
            }
            Expr::BinaryOpCall(bin) => bin.replace(nice_name, actual),
            Expr::UnaryOpCall(_) => todo!("unary ops are not implemented yet"),
            Expr::FnCall(call) => call.replace(nice_name, actual),
            Expr::ListLiteral { contents, .. }
            | Expr::TupleLiteral { contents, .. }
            | Expr::ArrayLiteral { contents, .. } => contents
                .iter_mut()
                .for_each(|it| it.replace(nice_name, actual)),
            Expr::StructConstruction(con) => con.replace(nice_name, actual),
            Expr::Compose { lhs, rhs } => {
                lhs.replace(nice_name, actual);
                rhs.replace(nice_name, actual);
            }
            _ => (),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct IfExpr {
    pub(crate) cond: Box<Expr>,
    pub(crate) true_branch: (Vec<Statement>, Box<Expr>),
    pub(crate) else_ifs: Vec<(Box<Expr>, Vec<Statement>, Box<Expr>)>,
    pub(crate) else_branch: (Vec<Statement>, Box<Expr>),
    pub(crate) loc: crate::Location,
}

impl IfExpr {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        self.cond.replace(nice_name, actual);
        self.true_branch
            .0
            .iter_mut()
            .for_each(|stmt| stmt.replace(nice_name, actual));
        self.true_branch.1.replace(nice_name, actual);
        self.else_ifs.iter_mut().for_each(|(cond, block, ret)| {
            cond.replace(nice_name, actual);
            block
                .iter_mut()
                .for_each(|stmnt| stmnt.replace(nice_name, actual));
            ret.replace(nice_name, actual);
        });
        self.else_branch
            .0
            .iter_mut()
            .for_each(|stmt| stmt.replace(nice_name, actual));
        self.else_branch.1.replace(nice_name, actual);
    }
}

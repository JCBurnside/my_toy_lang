use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU8,
};

use itertools::Itertools;

use crate::types::ResolvedType;
#[allow(unused)]
pub type File = ModuleDeclaration;
#[allow(unused)]
pub type Program = Vec<File>;
#[derive(Debug, PartialEq)]
pub struct ModuleDeclaration {
    pub loc: Option<crate::Location>,
    pub name: String,
    pub declarations: Vec<Declaration>,
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
            let Declaration::TypeDefinition(TypeDefinition::Alias(new, old)) =
                self.declarations.remove(idx - offset)
            else {
                unreachable!()
            };
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
    pub fn get_dependencies(&self) -> HashMap<String, HashSet<String>> {
        self.declarations
            .iter()
            .flat_map(|it| it.get_dependencies())
            .collect()
    }
}

#[derive(PartialEq, Debug)]
pub(crate) enum Declaration {
    TypeDefinition(TypeDefinition),

    #[allow(unused)] //TODO submoduling
    Mod(ModuleDeclaration),
    Value(ValueDeclaration),
}

impl Declaration {
    pub(crate) fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Mod(_) => (), //do nothing as super mod alias should not leak into submodules
            Self::TypeDefinition(def) => def.replace(nice_name, actual),
            Self::Value(decl) => decl.replace(nice_name, actual),
        }
    }

    fn get_dependencies(&self) -> HashMap<String, HashSet<String>> {
        match self {
            Declaration::Mod(m) => m.get_dependencies(),
            Declaration::Value(v) => [(v.ident.clone(), v.get_dependencies())].into(),
            Declaration::TypeDefinition(_) => [].into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition {
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

#[derive(PartialEq, Debug, Clone)]
pub struct EnumDeclation {
    pub ident: String,
    pub generics: Vec<String>,
    pub values: Vec<EnumVariant>,
    pub loc: crate::Location,
}

#[derive(PartialEq, Debug, Clone)]
#[allow(unused)]
pub enum EnumVariant {
    Unit(String, crate::Location),
    Tuple(String, Vec<ResolvedType>, crate::Location),
    Struct(String, StructDefinition, crate::Location),
}

#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub ident: String,
    pub generics: Vec<String>,
    pub values: Vec<FieldDecl>,
    pub loc: crate::Location,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FieldDecl {
    pub name: String,
    pub ty: ResolvedType,
    pub loc: crate::Location,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct ArgDeclaration {
    pub(crate) loc: crate::Location,
    pub(crate) ident: String,
    pub(crate) ty: Option<ResolvedType>, 
}

#[derive(PartialEq, Debug)]
pub struct ValueDeclaration {
    pub(crate) loc: crate::Location, //should be location of the ident.
    pub(crate) is_op: bool,
    pub(crate) ident: String,
    pub(crate) args: Vec<ArgDeclaration>,
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

    fn get_dependencies(&self) -> HashSet<String> {
        let mut output = HashSet::new();
        if let Some(ty) = &self.ty {
            output.extend(ty.get_all_types());
        }
        for arg in &self.args {
            if let Some(ty) = &arg.ty {
                let mut tys = ty.get_all_types();
                output.extend(tys);
            }
        }
        let args = self.args.iter().map(|arg| arg.ident.clone()).collect_vec();
        let dependencies = self.value.get_dependencies(args);
        output.extend(dependencies);
        output
    }
}
#[derive(PartialEq, Debug)]
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

    fn get_dependencies(&self, known_values: Vec<String>) -> HashSet<String> {
        match self {
            ValueType::Expr(expr) => expr.get_dependencies(known_values),
            ValueType::Function(stmnts) => {
                stmnts
                    .iter()
                    .fold(
                        (known_values, HashSet::new()),
                        |(mut known_values, mut dependencies), stmnt| {
                            dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                            if let Statement::Declaration(decl) = &stmnt {
                                known_values.push(decl.ident.clone())
                            }
                            (known_values, dependencies)
                        },
                    )
                    .1
            }
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Declaration(ValueDeclaration),
    Return(Expr, crate::Location),
    FnCall(FnCall),
    Pipe(Pipe),
    IfStatement(IfBranching),
    Match(Match),
    Error,
}
impl Statement {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Declaration(decl) => decl.replace(nice_name, actual),
            Self::Return(expr, _) => expr.replace(nice_name, actual),
            Self::FnCall(fncall) => fncall.replace(nice_name, actual),
            Self::Pipe(_) => todo!(),
            Self::IfStatement(ifbranches) => ifbranches.replace(nice_name, actual),
            Self::Match(match_) => match_.replace(nice_name, actual),
            Self::Error => (),
        }
    }

    pub fn get_loc(&self) -> crate::Location {
        match self {
            Self::Declaration(decl) => decl.loc,
            Self::Return(_, loc) => *loc,
            Self::FnCall(call) => call.loc,
            Self::Pipe(_) => todo!(),
            Self::IfStatement(if_) => if_.loc,
            Self::Match(match_) => match_.loc,
            Self::Error => (0, 0),
        }
    }

    fn get_dependencies(&self, known_values: Vec<String>) -> HashSet<String> {
        match self {
            Statement::Declaration(decl) => decl.get_dependencies(),
            Statement::Return(expr, _) => expr.get_dependencies(known_values),
            Statement::FnCall(fncall) => fncall.get_dependencies(known_values),
            Statement::Pipe(_) => todo!(),
            Statement::IfStatement(if_) => {
                let mut dependencies = if_.cond.get_dependencies(known_values.clone());
                {
                    let mut known_values = known_values.clone();
                    for stmnt in &if_.true_branch {
                        dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                        if let Statement::Declaration(decl) = stmnt {
                            known_values.push(decl.ident.clone());
                        }
                    }
                }
                for elif in &if_.else_ifs {
                    dependencies.extend(elif.0.get_dependencies(known_values.clone()));
                    {
                        let mut known_values = known_values.clone();
                        for stmnt in &elif.1 {
                            dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                            if let Statement::Declaration(decl) = stmnt {
                                known_values.push(decl.ident.clone());
                            }
                        }
                    }
                }
                {
                    let mut known_values = known_values.clone();
                    for stmnt in &if_.else_branch {
                        dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                        if let Statement::Declaration(decl) = stmnt {
                            known_values.push(decl.ident.clone());
                        }
                    }
                }
                dependencies
            }
            Statement::Match(match_) => {
                let mut dependencies = match_.on.get_dependencies(known_values.clone());
                for arm in &match_.arms {
                    {
                        let mut known_values = known_values.clone();
                        for stmnt in &arm.block {
                            dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                            if let Statement::Declaration(decl) = stmnt {
                                known_values.push(decl.ident.clone());
                            }
                        }
                        if let Some(ret) = &arm.ret {
                            dependencies.extend(ret.get_dependencies(known_values));
                        }
                    }
                }
                dependencies
            }
            Statement::Error => todo!(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct IfBranching {
    pub cond: Box<Expr>,
    pub true_branch: Vec<Statement>,
    pub else_ifs: Vec<(Box<Expr>, Vec<Statement>)>,
    pub else_branch: Vec<Statement>,
    pub loc: crate::Location,
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

#[derive(PartialEq, Debug)]
pub struct Pipe {
    ///if you need to spread more than an u8's worth of values... wtf are you doing? what would even return that many values as a tuple?  
    pub(crate) expansion: NonZeroU8,
    pub(crate) lhs: Box<Expr>,
    /// THIS HAS A RESTRICTION OF MUST RETURN A FUNCTION
    pub(crate) rhs: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct UnaryOpCall {
    pub(crate) loc: crate::Location,
    pub(crate) operand: Box<Expr>,
    pub(crate) operator: String,
}

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
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

    fn get_dependencies(&self, known_values: Vec<String>) -> HashSet<String> {
        let mut dependencies = if let Some(arg) = &self.arg {
            arg.get_dependencies(known_values.clone())
        } else {
            HashSet::new()
        };
        dependencies.extend(self.value.get_dependencies(known_values));
        dependencies
    }
}

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
pub enum Expr {
    Error,
    /// any integer or floating point value
    NumericLiteral {
        value: String,
    },
    /// "hello world!"
    StringLiteral(String),
    /// `'a'`
    CharLiteral(String),
    /// ()
    UnitLiteral,
    /// a >>> b
    /// can probably remove as compose can defined as
    /// let compose a b c = a c |> b
    /// let (>>>) = compose
    /// in theory should resolve to `(T0 -> T1) -> (T1 -> T2) -> T0 -> T2`
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
    /// type [|T|] or List<T>
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
    BoolLiteral(bool, crate::Location),
    If(IfExpr),
    Match(Match),
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
            Expr::If(ifexpr) => ifexpr.replace(nice_name, actual),
            Expr::Match(match_) => match_.replace(nice_name, actual),
            _ => (),
        }
    }

    pub(crate) fn is_function_call(&self) -> bool {
        matches!(self, Expr::FnCall(_))
    }

    fn get_dependencies(&self, known_values: Vec<String>) -> HashSet<String> {
        match self {
            Expr::NumericLiteral { .. }
            | Expr::StringLiteral(_)
            | Expr::CharLiteral(_)
            | Expr::UnitLiteral
            | Expr::BoolLiteral(_, _)
            | Expr::Error => HashSet::new(),
            Expr::Compose { .. } => todo!("compose"),
            Expr::BinaryOpCall(binop) => {
                let mut dependencies = binop.lhs.get_dependencies(known_values.clone());
                dependencies.extend(binop.rhs.get_dependencies(known_values.clone()));
                dependencies.insert(binop.operator.clone());
                dependencies
            }
            Expr::UnaryOpCall(_) => todo!(),
            Expr::FnCall(fncall) => fncall.get_dependencies(known_values),
            Expr::ValueRead(a, _) => {
                if !known_values.contains(a) {
                    [a.clone()].into()
                } else {
                    HashSet::new()
                }
            }
            Expr::ArrayLiteral { contents, .. }
            | Expr::ListLiteral { contents, .. }
            | Expr::TupleLiteral { contents, .. } => contents
                .iter()
                .flat_map(|it| it.get_dependencies(known_values.clone()))
                .collect(),
            Expr::StructConstruction(strctcon) => {
                let mut dependencies: HashSet<_> = strctcon
                    .fields
                    .iter()
                    .map(|it| &it.1 .0)
                    .flat_map(|it| it.get_dependencies(known_values.clone()))
                    .collect();
                dependencies.insert(strctcon.ident.clone());
                dependencies
            }
            Expr::If(if_) => {
                let mut dependencies = if_.cond.get_dependencies(known_values.clone());
                {
                    let mut known_values = known_values.clone();
                    for stmnt in &if_.true_branch.0 {
                        dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                        if let Statement::Declaration(decl) = stmnt {
                            known_values.push(decl.ident.clone());
                        }
                    }
                    dependencies.extend(if_.true_branch.1.get_dependencies(known_values));
                }
                for elif in &if_.else_ifs {
                    dependencies.extend(elif.0.get_dependencies(known_values.clone()));
                    {
                        let mut known_values = known_values.clone();
                        for stmnt in &elif.1 {
                            dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                            if let Statement::Declaration(decl) = stmnt {
                                known_values.push(decl.ident.clone());
                            }
                        }
                        dependencies.extend(elif.2.get_dependencies(known_values));
                    }
                }
                {
                    let mut known_values = known_values.clone();
                    for stmnt in &if_.else_branch.0 {
                        dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                        if let Statement::Declaration(decl) = stmnt {
                            known_values.push(decl.ident.clone());
                        }
                    }
                    dependencies.extend(if_.else_branch.1.get_dependencies(known_values));
                }
                dependencies
            }
            Expr::Match(match_) => {
                let mut dependencies = match_.on.get_dependencies(known_values.clone());
                for arm in &match_.arms {
                    {
                        let mut known_values = known_values.clone();
                        for stmnt in &arm.block {
                            dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                            if let Statement::Declaration(decl) = stmnt {
                                known_values.push(decl.ident.clone());
                            }
                        }
                        if let Some(ret) = &arm.ret {
                            dependencies.extend(ret.get_dependencies(known_values));
                        }
                    }
                }
                dependencies
            }
        }
    }
}

#[derive(PartialEq, Debug)]
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

#[derive(Debug, PartialEq)]
pub struct Match {
    pub(crate) loc: crate::Location,
    pub(crate) on: Box<Expr>,
    pub(crate) arms: Vec<MatchArm>,
}
impl Match {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        self.on.replace(nice_name, actual);
        for arm in &mut self.arms {
            arm.replace(nice_name, actual);
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct MatchArm {
    pub(crate) block: Vec<Statement>,
    pub(crate) ret: Option<Box<Expr>>,
    pub(crate) cond: Pattern,
    pub(crate) loc: crate::Location,
}
impl MatchArm {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        for stmnt in &mut self.block {
            stmnt.replace(nice_name, actual);
        }
        self.ret.as_mut().map(|it| it.replace(nice_name, actual));

        //TODO! allowing for named consts in patterns and handling replacing them with cannon names.
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Pattern {
    Default,
    ConstNumber(String), // todo! conditional branch
    ConstStr(String),
    ConstChar(String),
    ConstBool(bool), //... this is one is odd but gonna support it anyway and eventaully warn with suggestion to convert to if
    Read(String),    // todo! conditional branch
                     // todo! variant patterns.
}

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
    pub declarations: Vec<TopLevelDeclaration>,
}

impl ModuleDeclaration {
    pub fn canonialize(&mut self, mut path: Vec<String>) {
        path.push(self.name.clone());
        let to_remove = self
            .declarations
            .iter()
            .enumerate()
            .filter_map(|(pos, it)| match it {
                TopLevelDeclaration::TypeDefinition(decl) => match decl {
                    TypeDefinition::Alias(_, _) => Some(pos),
                    _ => None,
                },
                _ => None,
            })
            .collect::<Vec<_>>();
        for (offset, idx) in to_remove.into_iter().enumerate() {
            let TopLevelDeclaration::TypeDefinition(TypeDefinition::Alias(new, old)) =
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
                TopLevelDeclaration::Mod(m) => {
                    m.canonialize(path.clone());
                    continue;
                }
                TopLevelDeclaration::Value(v) => {
                    if v.value == ValueType::External {
                        continue;
                    }
                    let old = v.ident.clone();
                    v.ident = path.iter().cloned().join("::") + "::" + &old;
                    (old, v.ident.clone())
                }
                TopLevelDeclaration::TypeDefinition(def) => {
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
                TopLevelDeclaration::Mod(m) => self.declarations.extend(m.declarations),
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

    pub fn get_types(&self) -> HashMap<String, ResolvedType> {
        self.declarations
            .iter()
            .flat_map(|decl| match decl {
                TopLevelDeclaration::Value(_) => [].into(),
                TopLevelDeclaration::Mod(m) => m.get_types(),
                TopLevelDeclaration::TypeDefinition(TypeDefinition::Alias(new, old)) => {
                    [(new.clone(), old.clone())].into()
                }
                TopLevelDeclaration::TypeDefinition(TypeDefinition::Struct(s)) => [(
                    s.ident.clone(),
                    ResolvedType::User {
                        name: s.ident.clone(),
                        generics: s
                            .generics
                            .as_ref()
                            .map(|generics| {
                                generics
                                    .decls
                                    .iter()
                                    .map(|(loc, name)| ResolvedType::Generic {
                                        name: name.clone(),
                                        loc: *loc,
                                    })
                                    .collect()
                            })
                            .unwrap_or_default(),
                        loc: s.loc,
                    },
                )]
                .into(),
                TopLevelDeclaration::TypeDefinition(TypeDefinition::Enum(e)) => {
                    let base_ty = ResolvedType::User {
                        name: e.ident.clone(),
                        generics: e
                            .generics
                            .as_ref()
                            .map(|generics| {
                                generics
                                    .decls
                                    .iter()
                                    .map(|(loc, name)| ResolvedType::Generic {
                                        name: name.clone(),
                                        loc: *loc,
                                    })
                                    .collect()
                            })
                            .unwrap_or_default(),
                        loc: e.loc,
                    };
                    let mut out = vec![(e.ident.clone(), base_ty.clone())];

                    for variant in &e.values {
                        let ident = variant.get_ident();
                        let new_ident = format!("{}::{}", &e.ident, ident);
                        let generics = e
                        .generics
                        .as_ref()
                        .map(|generics| {
                            generics
                                .decls
                                .iter()
                                .map(|(loc, name)| ResolvedType::Generic {
                                    name: name.clone(),
                                    loc: *loc,
                                })
                                .collect()
                        })
                        .unwrap_or_default();
                        match variant {
                            EnumVariant::Unit { ident:_, loc } => out.push((
                                new_ident.clone(),
                                ResolvedType::Dependent {
                                    base: base_ty.clone().into(),
                                    ident: new_ident.clone(),
                                    actual: ResolvedType::Void.into(),
                                    generics,
                                    loc: *loc,
                                },
                            )),
                            EnumVariant::Tuple { ident:_, ty, loc } => out.push((
                                new_ident.clone(),
                                ResolvedType::Dependent {
                                    base: base_ty.clone().into(),
                                    ident: new_ident,
                                    actual: ty.clone().into(),
                                    generics,
                                    loc: *loc,
                                },
                            )),
                            EnumVariant::Struct {
                                ident:_, loc, ..
                            } => {
                                
                                out.push((
                                    new_ident.clone(),
                                    ResolvedType::Dependent {
                                        base: base_ty.clone().into(),
                                        ident: new_ident.clone(),
                                        actual: ResolvedType::User {
                                            name: new_ident,
                                            generics:generics.clone(),
                                            loc: *loc,
                                        }
                                        .into(),
                                        generics,
                                        loc: *loc,
                                    },
                                ))
                            }
                        }
                    }
                    HashMap::from_iter(out)
                }
            })
            .collect()
    }
}

#[derive(PartialEq, Debug)]
pub enum TopLevelDeclaration {
    TypeDefinition(TypeDefinition),

    #[allow(unused)] //TODO submoduling
    Mod(ModuleDeclaration),
    Value(TopLevelValue),
}

impl TopLevelDeclaration {
    pub(crate) fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Mod(_) => (), //do nothing as super mod alias should not leak into submodules
            Self::TypeDefinition(def) => def.replace(nice_name, actual),
            Self::Value(decl) => decl.replace(nice_name, actual),
        }
    }

    fn get_dependencies(&self) -> HashMap<String, HashSet<String>> {
        match self {
            TopLevelDeclaration::Mod(m) => m.get_dependencies(),
            TopLevelDeclaration::Value(v) => [(v.ident.clone(), v.get_dependencies())].into(),
            TopLevelDeclaration::TypeDefinition(_) => [].into(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct TopLevelValue {
    pub loc: crate::Location,
    pub is_op: bool,
    pub ident: String,
    pub args: Vec<ArgDeclaration>,
    pub ty: Option<ResolvedType>,
    pub value: ValueType,
    pub generics: Option<GenericsDecl>,
    pub abi: Option<Abi>,
}

impl TopLevelValue {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        if let Some(ty) = self.ty.as_mut() {
            *ty = ty.replace(nice_name, actual);
        }
        self.value.replace(nice_name, actual);
        for arg in &mut self.args {
            arg.replace(nice_name, actual);
        }
    }

    fn get_dependencies(&self) -> HashSet<String> {
        let mut values = self
            .args
            .iter()
            .flat_map(ArgDeclaration::get_idents)
            .collect_vec();
        values.push(self.ident.clone());
        self.value.get_dependencies(values)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct GenericsDecl {
    pub for_loc: crate::Location,
    pub decls: Vec<(crate::Location, String)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition {
    Alias(String, ResolvedType),
    #[allow(unused)]
    Enum(EnumDeclaration),
    Struct(StructDefinition),
}

impl TypeDefinition {
    pub(crate) fn get_ident(&self) -> String {
        match self {
            TypeDefinition::Alias(name, _) => name.clone(),
            TypeDefinition::Enum(enum_) => enum_.ident.clone(),
            TypeDefinition::Struct(strct) => strct.ident.clone(),
        }
    }

    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Alias(_, ty) => *ty = ty.replace(nice_name, actual),
            Self::Enum(enum_) => {
                if enum_.ident == nice_name {
                    enum_.ident = actual.to_string();
                }
                if let Some(generics) = &enum_.generics {
                    if generics
                        .decls
                        .iter()
                        .find(|(_, ident)| ident == nice_name)
                        .is_some()
                    {
                        return;
                    }
                }
                for variant in &mut enum_.values {
                    match variant {
                        EnumVariant::Unit { .. } => (),
                        EnumVariant::Tuple { ty, .. } => *ty = ty.replace(nice_name, actual),
                        EnumVariant::Struct { fields, .. } => {
                            for field in fields {
                                field.ty = field.ty.replace(nice_name, actual);
                            }
                        }
                    }
                }
            }
            Self::Struct(strct) => {
                if strct.ident == nice_name {
                    strct.ident = actual.to_string();
                }
                if let Some(generics) = &strct.generics {
                    if generics
                        .decls
                        .iter()
                        .find(|(_, ident)| ident == nice_name)
                        .is_some()
                    {
                        return;
                    }
                }
                for field in &mut strct.values {
                    field.ty = field.ty.replace(nice_name, actual);
                }
            }
        }
    }
}
/// {generics} enum {ident} = {values}
#[derive(PartialEq, Debug, Clone)]
pub struct EnumDeclaration {
    pub ident: String,
    pub generics: Option<GenericsDecl>,
    pub values: Vec<EnumVariant>,
    pub loc: crate::Location,
}

#[derive(PartialEq, Debug, Clone)]
#[allow(unused)]
pub enum EnumVariant {
    /// | {ident}
    Unit { ident: String, loc: crate::Location },
    /// | {ident} ({ty*})
    Tuple {
        ident: String,
        ty: ResolvedType,
        loc: crate::Location,
    },
    /// | {ident} {{ {feilddecl*} }}
    Struct {
        ident: String,
        fields: Vec<FieldDecl>,
        loc: crate::Location,
    },
}

impl EnumVariant {
    pub fn get_ident(&self) -> String {
        match self {
            Self::Struct { ident, .. } | Self::Unit { ident, .. } | Self::Tuple { ident, .. } => {
                ident.clone()
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct StructDefinition {
    pub ident: String,
    pub generics: Option<GenericsDecl>,
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
pub enum ArgDeclaration {
    Simple {
        loc: crate::Location,
        ident: String,
        ty: Option<ResolvedType>,
    },
    DestructureTuple(Vec<ArgDeclaration>, Option<ResolvedType>, crate::Location),
    DestructureStruct {
        loc: crate::Location,
        struct_ident: String,
        fields: Vec<String>,
        renamed_fields: HashMap<String, String>,
    },
    Discard {
        loc: crate::Location,
        ty: Option<ResolvedType>,
    },
    Unit {
        loc: crate::Location,
        ty: Option<ResolvedType>,
    },
}

impl ArgDeclaration {
    pub(crate) fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Discard { ty, .. } | Self::Unit { ty, .. } | Self::Simple { ty, .. } => {
                *ty = ty.as_ref().map(|ty| ty.replace(nice_name, actual))
            }
            Self::DestructureTuple(subs, ty, _) => {
                *ty = ty.as_ref().map(|ty| ty.replace(nice_name, actual));
                for sub_arg in subs {
                    sub_arg.replace(nice_name, actual);
                }
            }
            Self::DestructureStruct { struct_ident, .. } if struct_ident == nice_name => {
                *struct_ident = actual.to_string();
            }
            Self::DestructureStruct { .. } => (),
        }
    }

    pub(crate) fn apply_generic(&mut self, generic: &str) {
        match self {
            Self::Discard { ty, .. } | Self::Unit { ty, .. } | Self::Simple { ty, .. } => {
                if let Some(ty) = ty {
                    *ty = ty.clone().replace_user_with_generic(generic);
                }
            }
            Self::DestructureTuple(contents, ty, _) => {
                if let Some(ty) = ty {
                    *ty = ty.clone().replace_user_with_generic(generic);
                }
                contents.iter_mut().for_each(|it| it.apply_generic(generic))
            }
            _ => (),
        }
    }
    fn get_idents(&self) -> Vec<String> {
        match self {
            Self::Simple { ident, .. } => vec![ident.clone()],
            Self::DestructureStruct {
                fields,
                renamed_fields,
                ..
            } => renamed_fields
                .values()
                .cloned()
                .chain(fields.iter().cloned())
                .collect(),
            Self::DestructureTuple(decls, _, _) => {
                decls.iter().flat_map(Self::get_idents).collect()
            }
            Self::Discard { .. } | Self::Unit { .. } => Vec::new(),
        }
    }
    fn get_types(&self) -> HashSet<String> {
        match self {
            Self::DestructureTuple(idents, ty, _) => {
                let mut out: HashSet<String> = idents.iter().flat_map(Self::get_types).collect();
                if let Some(ty) = ty {
                    out.extend(ty.get_all_types());
                }
                out
            }
            Self::DestructureStruct { struct_ident, .. } => [struct_ident.clone()].into(),
            Self::Discard { ty, .. } | Self::Unit { ty, .. } | Self::Simple { ty, .. } => {
                if let Some(ty) = ty {
                    ty.get_all_types()
                } else {
                    HashSet::new()
                }
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Abi {
    pub loc: crate::Location,
    pub identifier: String,
}

#[derive(PartialEq, Debug)]
pub struct ValueDeclaration {
    pub loc: crate::Location, //should be location of the ident.
    pub is_op: bool,
    pub target: Pattern,
    pub args: Vec<ArgDeclaration>,
    pub ty: Option<ResolvedType>,
    pub value: ValueType,
    pub generictypes: Option<GenericsDecl>,
    pub abi: Option<Abi>,
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
            match arg {
                ArgDeclaration::Simple { ty, .. } | ArgDeclaration::DestructureTuple(_, ty, _)
                    if ty.is_some() =>
                {
                    let Some(ty) = ty else { unreachable!() };
                    let tys = ty.get_all_types();
                    output.extend(tys);
                }

                _ => (),
            }
        }
        let args = self
            .args
            .iter()
            .flat_map(ArgDeclaration::get_idents)
            .collect_vec();
        let dependencies = self.value.get_dependencies(args);
        output.extend(dependencies);
        output
    }
}
#[derive(PartialEq, Debug)]
pub enum ValueType {
    Expr(Expr),
    Function(Vec<Statement>),
    External,
}
impl ValueType {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            Self::Expr(expr) => expr.replace(nice_name, actual),
            Self::Function(stmnts) => stmnts
                .iter_mut()
                .for_each(|it| it.replace(nice_name, actual)),
            Self::External => (),
        }
    }

    fn get_dependencies(&self, known_values: Vec<String>) -> HashSet<String> {
        match self {
            Self::Expr(expr) => expr.get_dependencies(known_values),
            Self::Function(stmnts) => {
                stmnts
                    .iter()
                    .fold(
                        (known_values, HashSet::new()),
                        |(mut known_values, mut dependencies), stmnt| {
                            dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                            if let Statement::Declaration(decl) = &stmnt {
                                known_values.extend(decl.target.get_idents());
                            }
                            (known_values, dependencies)
                        },
                    )
                    .1
            }
            Self::External => HashSet::new(),
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
                            known_values.extend(decl.target.get_idents());
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
                                known_values.extend(decl.target.get_idents());
                            }
                        }
                    }
                }
                {
                    let mut known_values = known_values.clone();
                    for stmnt in &if_.else_branch {
                        dependencies.extend(stmnt.get_dependencies(known_values.clone()));
                        if let Statement::Declaration(decl) = stmnt {
                            known_values.extend(decl.target.get_idents());
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
                                known_values.extend(decl.target.get_idents());
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
// TODO! spread pipes
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
    pub loc: crate::Location,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: String,
}
impl BinaryOpCall {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        self.lhs.replace(nice_name, actual);
        self.rhs.replace(nice_name, actual);
    }
}

#[derive(PartialEq, Debug)]
pub struct FnCall {
    pub loc: crate::Location,
    pub value: Box<Expr>,
    pub arg: Option<Box<Expr>>,
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
    pub loc: crate::Location,
    pub fields: HashMap<String, (Expr, crate::Location)>,
    pub generics: Vec<ResolvedType>,
    pub ident: String,
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

    pub fn is_function_call(&self) -> bool {
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
                            known_values.extend(decl.target.get_idents());
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
                                known_values.extend(decl.target.get_idents());
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
                            known_values.extend(decl.target.get_idents());
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
                                known_values.extend(decl.target.get_idents());
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
    pub cond: Box<Expr>,
    pub true_branch: (Vec<Statement>, Box<Expr>),
    pub else_ifs: Vec<(Box<Expr>, Vec<Statement>, Box<Expr>)>,
    pub else_branch: (Vec<Statement>, Box<Expr>),
    pub loc: crate::Location,
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
    pub loc: crate::Location,
    pub on: Box<Expr>,
    pub arms: Vec<MatchArm>,
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
pub struct MatchArm {
    pub block: Vec<Statement>,
    pub ret: Option<Box<Expr>>,
    pub cond: Pattern,
    pub loc: crate::Location,
}
impl MatchArm {
    fn replace(&mut self, nice_name: &str, actual: &str) {
        self.cond.replace(nice_name,actual);
        let names = self.cond.get_idents();
        if names.contains(nice_name) {
            return;
        }
        for stmnt in &mut self.block {
            stmnt.replace(nice_name, actual);
        }
        self.ret.as_mut().map(|it| it.replace(nice_name, actual));

        //TODO! allowing for named consts in patterns and handling replacing them with cannon names.
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Default,
    ConstNumber(String), // todo! conditional branch
    ConstStr(String),
    ConstChar(String),
    ConstBool(bool), //... this is one is odd but gonna support it anyway and eventaully warn with suggestion to convert to if
    Read(String, crate::Location),
    Destructure(PatternDestructure),
    EnumVariant {
        ty: Option<String>,
        variant: String,
        pattern: Option<Box<Self>>,
        loc: crate::Location,
    },
    // maybe this one day?
    // {name} @ {subpatterns}
    // Binding(String /*name*/,Box<Self>/*sub_patterns*/,crate::locations)
    Error,
    Or(Box<Self>, Box<Self>), /*
                                  | 0 | 1 | 2 -> Combo(
                                                      ConstNumber(0),
                                                      Combo(
                                                          ConstNumber(1),
                                                          ConstNumber(2)
                                                      )
                                                  )
                              */
                              // todo! conditional branch
                              // todo! variant patterns.
}

impl Pattern {
    fn get_idents(&self) -> HashSet<String> {
        match self {
            Self::Read(name, _) => [name.clone()].into(),
            Self::Destructure(d) => d.get_idents(),
            Self::Or(lhs, rhs) => {
                let mut idents = lhs.get_idents();
                idents.extend(rhs.get_idents());
                idents
            }
            _ => HashSet::new(),
        }
    }
    
    fn replace(&mut self, nice_name: &str, actual: &str) {
        match self {
            
            Self::Destructure(destructure) => match destructure {
                PatternDestructure::Struct { base_ty, .. } => {
                    if let Some(ty) = base_ty {
                        if ty == nice_name {
                            *ty = actual.into();
                        }
                    }
                },
                PatternDestructure::Tuple(pats) => {
                    for pat in pats {
                        pat.replace(nice_name,actual)
                    }
                },
                PatternDestructure::Unit => (),
            },
            Self::EnumVariant { ty, variant, pattern, .. } => {
                if let Some(ty) = ty {
                    if ty == nice_name {
                        *ty = actual.into();
                    }
                } else {
                    if variant == nice_name {
                        *variant = actual.into();
                    }
                }
                if let Some(pat) = pattern {
                    pat.replace(nice_name,actual);
                }
            },
            
            Self::Or(lhs, rhs) => {
                lhs.replace(nice_name,actual);
                rhs.replace(nice_name,actual);
            }
            _=>()
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum PatternDestructure {
    Struct {
        base_ty: Option<String>,
        fields: HashMap<String, Pattern>,
    },
    Tuple(Vec<Pattern>),
    Unit,
}
impl PatternDestructure {
    fn get_idents(&self) -> HashSet<String> {
        match self {
            Self::Tuple(pats) => pats.iter().flat_map(Pattern::get_idents).collect(),
            Self::Struct { .. } => todo!("struct destructuring"),
            Self::Unit => HashSet::new(),
        }
    }
}

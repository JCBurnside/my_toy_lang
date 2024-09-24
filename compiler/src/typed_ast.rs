use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU8,
};
use thiserror::Error;

use crate::{
    inference::ast::{self, Expr, TopLevelDeclaration},
    types::{self, FloatWidth, IntWidth, ResolvedType},
};

pub type FileTyped = TypedModuleDeclaration;
pub type ProgramTyped = Vec<FileTyped>;
#[derive(Debug, PartialEq, Clone)]
pub struct TypedModuleDeclaration {
    pub loc: crate::Location,
    pub name: String,
    pub declarations: Vec<TypedDeclaration>,
}

impl TypedModuleDeclaration {
    pub(crate) fn from(
        module: ast::ModuleDeclaration,
        fwd_declares: &HashMap<String, ResolvedType>,
        operators: &HashMap<String, Vec<ResolvedType>>,
    ) -> Self {
        let ast::ModuleDeclaration { loc, name, decls } = module;
        let mut fwd_declares = fwd_declares.clone();
        fwd_declares.extend(decls.iter().filter_map(|it| {
            match it {
                TopLevelDeclaration::Type(decl) => match decl {
                    ast::TypeDefinition::Alias(name, value) => Some((name.clone(), value.clone())),
                    ast::TypeDefinition::Enum(enum_) => Some((
                        name.clone(),
                        ResolvedType::User {
                            name: enum_.ident.clone(),
                            generics: enum_
                                .generics
                                .clone()
                                .map(ResolvedGenericsDecl::from)
                                .map(|g| g.decls.into_iter().map(|(_, ty)| ty).collect())
                                .unwrap_or_else(Vec::new),
                            loc: enum_.loc,
                        },
                    )),
                    ast::TypeDefinition::Struct(strct) => Some((
                        strct.ident.clone(),
                        ResolvedType::User {
                            name: strct.ident.clone(),
                            generics: strct
                                .generics
                                .clone()
                                .map(ResolvedGenericsDecl::from)
                                .map(|g| g.decls.into_iter().map(|(_, ty)| ty).collect())
                                .unwrap_or_else(Vec::new),
                            loc: strct.loc,
                        },
                    )),
                },
                TopLevelDeclaration::Value(decl) if decl.is_op == false => {
                    Some((decl.ident.clone(), decl.ty.clone()))
                }
                _ => None,
            }
        }));
        let externs = decls
            .iter()
            .filter_map(|decl| match decl {
                ast::TopLevelDeclaration::Value(decl) if decl.value == ast::ValueType::External => {
                    Some((decl.ident.clone(), decl.ty.clone()))
                }
                _ => None,
            })
            .collect();
        let mut types: HashMap<String, ResolvedTypeDeclaration> = HashMap::new();
        // decls
        //     .iter()
        //     .filter_map::<(String, ast::TypeDefinition), _>(|it| match it {

        //         ast::TopLevelDeclaration::Type(def) => Some((def.get_ident(), def.clone())),
        //         _ => None,
        //     })
        //     .collect();
        for def in decls
            .iter()
            .filter(|it| matches!(it, ast::TopLevelDeclaration::Type(_)))
        {
            let ast::TopLevelDeclaration::Type(def) = def else {
                unreachable!()
            };
            match def {
                ast::TypeDefinition::Alias(_, _resolved_type) => unreachable!(), //should be resolved at cannonizing
                ast::TypeDefinition::Enum(enum_declaration) => {
                    let base =
                        ResolvedTypeDeclaration::try_from(def.clone(), &fwd_declares).unwrap();
                    let generics = if let ResolvedTypeDeclaration::Enum(e) = &base {
                        e.generics.clone()
                    } else {
                        unreachable!()
                    };
                    for variant in &enum_declaration.values {
                        match variant {
                            crate::ast::EnumVariant::Unit { ident, loc } => {
                                types.insert(
                                    format!("{}::{}", &enum_declaration.ident, &ident),
                                    ResolvedTypeDeclaration::Dependent {
                                        base: base.clone().into(),
                                        actual: ResolvedTypeDeclaration::Alias(
                                            ident.clone(),
                                            ResolvedType::Void,
                                        )
                                        .into(),
                                    },
                                );
                            }
                            crate::ast::EnumVariant::Tuple { ident, ty, loc } => {
                                types.insert(
                                    format!("{}::{}", &enum_declaration.ident, &ident),
                                    ResolvedTypeDeclaration::Dependent {
                                        base: base.clone().into(),
                                        actual: ResolvedTypeDeclaration::Alias(
                                            ident.clone(),
                                            ty.clone(),
                                        )
                                        .into(),
                                    },
                                );
                            }
                            crate::ast::EnumVariant::Struct { ident, fields, loc } => {
                                types.insert(
                                    format!("{}::{}", &enum_declaration.ident, &ident),
                                    ResolvedTypeDeclaration::Dependent {
                                        base: base.clone().into(),
                                        actual: ResolvedTypeDeclaration::Struct(StructDefinition {
                                            ident: ident.clone(),
                                            generics: generics.clone(),
                                            fields: fields.clone(),
                                            loc: *loc,
                                        })
                                        .into(),
                                    },
                                );
                            }
                        }
                    }
                    types.insert(enum_declaration.ident.clone(), base);
                }
                ast::TypeDefinition::Struct(struct_definition) => {
                    types.insert(
                        struct_definition.ident.clone(),
                        ResolvedTypeDeclaration::try_from(def.clone(), &fwd_declares).unwrap(),
                    );
                }
            }
        }
        Self {
            loc,
            name,
            declarations: decls
                .into_iter()
                .map(|decl| {
                    TypedDeclaration::try_from(decl, &externs, &fwd_declares, operators, &types)
                })
                .filter_map(|decl: Result<TypedDeclaration, TypingError>| match decl {
                    Ok(decl) => Some(decl),
                    Err(e) => {
                        println!("{:?}", e);
                        None
                    }
                })
                .collect(),
        }
    }

    pub fn lower_generics(&mut self, fwd_declares: &HashMap<String, TypedDeclaration>) {
        let mut fwd_decl = fwd_declares.clone();
        fwd_decl.extend(
            self.declarations
                .clone()
                .into_iter()
                .map(|it| (it.get_ident(), it)),
        );
        let mut context = LoweringContext {
            globals: fwd_decl,
            generated_generics: HashMap::new(),
            functions: HashMap::new(),
            args: Vec::new(),
        };
        self.declarations
            .iter_mut()
            .filter(|it| it.get_generics().is_empty())
            .for_each(|it| it.lower_generics(&mut context));
        self.declarations
            .extend(context.generated_generics.into_iter().map(|(_, it)| it));
        // println!("{:?}", self.declarations)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ResolvedGenericsDecl {
    pub for_loc: crate::Location,
    pub decls: Vec<(crate::Location, ResolvedType)>,
}

impl ResolvedGenericsDecl {
    fn from(other: ast::GenericsDecl) -> Self {
        let ast::GenericsDecl { for_loc, decls } = other;
        Self {
            for_loc,
            decls: decls
                .into_iter()
                .map(|(loc, name)| (loc, ResolvedType::Generic { name, loc }))
                .collect(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedDeclaration {
    Value(TypedTopLevelValue),
    TypeDefinition(ResolvedTypeDeclaration),
}

impl TypedDeclaration {
    pub(crate) fn get_ident(&self) -> String {
        match self {
            Self::Value(v) => v.ident.clone(),
            Self::TypeDefinition(decl) => decl.get_ident(),
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Self::Value(v) => v.ty.is_generic(),
            Self::TypeDefinition(decl) => decl.is_generic(),
        }
    }

    pub(crate) fn try_from(
        data: ast::TopLevelDeclaration,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_ops: &HashMap<String, Vec<ResolvedType>>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        match data {
            TopLevelDeclaration::Value(decl) => Ok(Self::Value(TypedTopLevelValue::try_from(
                decl,
                known_externs,
                known_values,
                known_ops,
                known_types,
            )?)),
            TopLevelDeclaration::Type(define) => Ok(Self::TypeDefinition(
                ResolvedTypeDeclaration::try_from(define, known_values)?,
            )),
        }
    }

    pub(crate) fn replace_types(
        &mut self,
        types: &[(String, ResolvedType)],
        context: &mut LoweringContext,
    ) {
        match self {
            TypedDeclaration::Value(value) => value.replace_types(types),
            TypedDeclaration::TypeDefinition(strct) => strct.replace_types(types, context),
        };
    }

    pub(crate) fn get_generics(&self) -> Vec<String> {
        match self {
            TypedDeclaration::Value(v) => v
                .generics
                .as_ref()
                .map(|g| {
                    g.decls
                        .iter()
                        .map(|(_, it)| {
                            let ResolvedType::Generic { name, .. } = it else {
                                unreachable!()
                            };
                            name.clone()
                        })
                        .collect()
                })
                .unwrap_or_else(Vec::new),
            TypedDeclaration::TypeDefinition(define) => define.get_generics(),
        }
    }

    pub(crate) fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            TypedDeclaration::Value(value) => value.lower_generics(context),
            TypedDeclaration::TypeDefinition(def) => def.lower_generics(context),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedTopLevelValue {
    pub loc: crate::Location,
    pub is_op: bool,
    pub ident: String,
    pub args: Vec<TypedArgDeclaration>,
    pub ty: ResolvedType,
    pub value: TypedValueType,
    pub generics: Option<ResolvedGenericsDecl>,
    pub abi: Option<crate::ast::Abi>,
}

impl TypedTopLevelValue {
    fn try_from(
        value: ast::TopLevelValue,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        _known_ops: &HashMap<String, Vec<ResolvedType>>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        let ast::TopLevelValue {
            loc,
            is_op,
            ident,
            args,
            ty,
            value,
            generics,
            abi,
            id: _,
        } = value;
        let mut known_values = known_values.clone();
        let args = args
            .into_iter()
            .map(TypedArgDeclaration::from)
            .collect_vec();
        known_values.extend(
            args.iter()
                .flat_map(|arg| arg.get_idents_with_types(known_types)),
        );
        let value = match TypedValueType::try_from(value, known_externs, &known_values, known_types)
        {
            Ok(value) => value,
            Err(e) => {
                println!("{:?}", e);
                TypedValueType::Err
            }
        };
        if let Some(abi) = &abi {
            if abi.identifier.as_str() == "C" {
                if let ResolvedType::Function {
                    arg: _, returns, ..
                } = &ty
                {
                    if returns.is_function() {
                        // TODO verify it's a valid c-function.
                    }
                }
            }
        }
        Ok(Self {
            loc,
            is_op,
            ident,
            args,
            ty,
            value,
            generics: generics.map(ResolvedGenericsDecl::from),
            abi,
        })
    }

    pub(crate) fn replace_types(&mut self, types: &[(String, ResolvedType)]) {
        for arg in &mut self.args {
            arg.replace_types(types);
        }
        self.ty = types.iter().fold(self.ty.clone(), |ty, (name, new_ty)| {
            ty.replace_generic(name, new_ty.clone())
        });
        match &mut self.value {
            TypedValueType::Function(stmnts) => stmnts
                .into_iter()
                .for_each(|stmnt| stmnt.replace_types(types)),
            TypedValueType::Expr(expr) => expr.replace_types(types),
            TypedValueType::External | TypedValueType::Err => (),
        }
    }
    pub(crate) fn lower_generics(&mut self, context: &mut LoweringContext) {
        self.ty.lower_generics(context);
        match &mut self.value {
            TypedValueType::Expr(expr) => expr.lower_generics(context),
            TypedValueType::Function(stmnts) => {
                for stmnt in stmnts {
                    context.args.clear();
                    stmnt.lower_generics(context);
                }
            }
            TypedValueType::Err => (),
            TypedValueType::External => (),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[non_exhaustive]
pub enum ResolvedTypeDeclaration {
    Alias(String, ResolvedType),
    Enum(TypedEnumDeclaration),
    Struct(StructDefinition),
    Dependent { base: Box<Self>, actual: Box<Self> },
}

impl Into<ResolvedType> for &ResolvedTypeDeclaration {
    fn into(self) -> ResolvedType {
        match self {
            ResolvedTypeDeclaration::Alias(_, resolved_type) => resolved_type.clone(),
            ResolvedTypeDeclaration::Enum(TypedEnumDeclaration {
                ident,
                loc,
                generics,
                ..
            })
            | ResolvedTypeDeclaration::Struct(StructDefinition {
                ident,
                loc,
                generics,
                ..
            }) => ResolvedType::User {
                name: ident.clone(),
                generics: generics
                    .as_ref()
                    .map(|generics| generics.decls.iter().map(|(_, it)| it).cloned().collect())
                    .unwrap_or_default(),
                loc: *loc,
            },
            ResolvedTypeDeclaration::Dependent { base, actual } => actual.as_ref().into(),
        }
    }
}

impl ResolvedTypeDeclaration {
    fn try_from(
        origin: ast::TypeDefinition,
        known_types: &HashMap<String, ResolvedType>,
    ) -> Result<Self, TypingError> {
        match origin {
            // ast::TypeDefinition::Alias(new, old) => Ok(Self::Alias(new, old)),
            ast::TypeDefinition::Enum(enum_) => Ok(ResolvedTypeDeclaration::Enum(enum_.into())),
            ast::TypeDefinition::Struct(strct) => Ok(ResolvedTypeDeclaration::Struct(
                StructDefinition::try_from(strct, known_types)?,
            )),
            ast::TypeDefinition::Alias(_, _) => unreachable!(), //there should be no aliases left after canonializing
        }
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            // ResolvedTypeDeclaration::Alias(_, old) => match old {
            //     ResolvedType::User { name, generics } if !generics.is_empty() => {
            //         let resolved_name = name.clone()
            //             + "<"
            //             + &generics.iter().map(ResolvedType::to_string).join(",")
            //             + ">";
            //         if !context.generated_generics.contains_key(&resolved_name) {
            //             let mut target = context.globals.get(name).unwrap().clone();
            //             let zipped = target
            //                 .get_generics()
            //                 .into_iter()
            //                 .zip(generics.iter().cloned())
            //                 .collect_vec();
            //             target.replace_types(&zipped, context);
            //             target.lower_generics(context);
            //             let _ = context.generated_generics.insert(resolved_name, target);
            //         }
            //     }
            //     _ => (),
            // },
            ResolvedTypeDeclaration::Dependent { base, actual } => {
                base.lower_generics(context); //gonna leave this here just in case.  should be covered by the other branches but better safe than sorry
                actual.lower_generics(context);
            }
            ResolvedTypeDeclaration::Alias(_, ty) => {
                ty.lower_generics(context);
            }
            ResolvedTypeDeclaration::Enum(enum_) => enum_.lower_generics(context),
            ResolvedTypeDeclaration::Struct(stct) => stct.lower_generics(context),
        }
    }

    fn get_ident(&self) -> String {
        match self {
            ResolvedTypeDeclaration::Alias(ident, _) => ident.clone(),
            ResolvedTypeDeclaration::Dependent { actual, .. } => actual.get_ident(),
            ResolvedTypeDeclaration::Enum(enum_) => enum_.ident.clone(),
            ResolvedTypeDeclaration::Struct(strct) => strct.ident.clone(),
        }
    }

    fn get_generics(&self) -> Vec<String> {
        match self {
            //todo! generic aliases
            ResolvedTypeDeclaration::Alias(_, _) => Vec::new(),
            ResolvedTypeDeclaration::Dependent { actual, .. } => actual.get_generics(),
            ResolvedTypeDeclaration::Enum(enum_) => enum_
                .generics
                .as_ref()
                .map(|g| {
                    g.decls
                        .iter()
                        .map(|(_, name)| {
                            let ResolvedType::Generic { name, .. } = name else {
                                unreachable!()
                            };
                            name.clone()
                        })
                        .collect()
                })
                .unwrap_or_else(Vec::new),
            ResolvedTypeDeclaration::Struct(strct) => strct
                .generics
                .as_ref()
                .map(|g| {
                    g.decls
                        .iter()
                        .map(|(_, name)| {
                            let ResolvedType::Generic { name, .. } = name else {
                                unreachable!()
                            };
                            name.clone()
                        })
                        .collect()
                })
                .unwrap_or_else(Vec::new),
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)], context: &mut LoweringContext) {
        match self {
            ResolvedTypeDeclaration::Alias(_, _ty) => (), // do i need to do something here?
            ResolvedTypeDeclaration::Dependent { base, actual } => {
                base.replace_types(types, context);
                actual.replace_types(types, context);
            }
            ResolvedTypeDeclaration::Enum(enum_) => enum_.replace_types(types, context),
            ResolvedTypeDeclaration::Struct(strct) => strct.replace_types(types, context),
        }
    }

    fn is_generic(&self) -> bool {
        match self {
            Self::Alias(_, _) => false, //todo! generic aliases
            Self::Dependent { base, .. } => base.is_generic(),
            Self::Enum(enum_) => enum_.generics.is_some(),
            Self::Struct(strct) => strct.generics.is_some(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedEnumDeclaration {
    pub ident: String,
    pub generics: Option<ResolvedGenericsDecl>,
    pub values: Vec<crate::ast::EnumVariant>,
    pub loc: crate::Location,
}
impl TypedEnumDeclaration {
    fn lower_generics(&mut self, context: &mut LoweringContext) {
        if self.generics.is_none() {
            for value in &mut self.values {
                match value {
                    crate::ast::EnumVariant::Unit { .. } => (),
                    crate::ast::EnumVariant::Tuple { ty, .. } => ty.lower_generics(context),
                    crate::ast::EnumVariant::Struct { fields, .. } => {
                        for field in fields {
                            field.ty.lower_generics(context);
                        }
                    }
                }
            }
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)], context: &mut LoweringContext) {
        if self.generics.is_none() {
            return;
        }

        let generics = self
            .generics
            .as_ref()
            .unwrap()
            .decls
            .iter()
            .cloned()
            .filter(|(_, it)| match it {
                ResolvedType::Generic { name, .. } => !types.iter().map(|(n, _)| n).contains(name),
                _ => true,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            generics.len(),
            0,
            "generic not completed.  should not be reached!"
        );
        self.generics = None;
        self.ident = format!(
            "{}<{}>",
            self.ident,
            types.iter().map(|(_, it)| it.to_string()).join(",")
        );
        for value in &mut self.values {
            match value {
                crate::ast::EnumVariant::Unit { .. } => (),
                crate::ast::EnumVariant::Tuple { ty, .. } => {
                    *ty = types.iter().fold(ty.clone(), |old_ty, (name, new_ty)| {
                        old_ty.replace_generic(name, new_ty.clone())
                    });
                    ty.lower_generics(context);
                }
                crate::ast::EnumVariant::Struct { fields, .. } => {
                    for field in fields {
                        field.ty = types.iter().fold(field.ty.clone(), |old_ty, ty| {
                            old_ty.replace_generic(&ty.0, ty.1.clone())
                        });
                        field.ty.lower_generics(context);
                    }
                }
            }
        }
    }
}

impl From<crate::ast::EnumDeclaration> for TypedEnumDeclaration {
    fn from(data: crate::ast::EnumDeclaration) -> Self {
        let crate::ast::EnumDeclaration {
            ident,
            generics,
            mut values,
            loc,
        } = data;
        if let Some(generics) = &generics {
            for (_, generic) in &generics.decls {
                values.iter_mut().for_each(|variant| match variant {
                    crate::ast::EnumVariant::Unit { .. } => (),
                    crate::ast::EnumVariant::Tuple { ty, .. } => {
                        *ty = ty.clone().replace_user_with_generic(generic)
                    }
                    crate::ast::EnumVariant::Struct { fields, .. } => {
                        for field in fields {
                            field.ty = field.ty.clone().replace_user_with_generic(&generic);
                        }
                    }
                })
            }
        }
        Self {
            ident,
            generics: generics.map(ResolvedGenericsDecl::from),
            values,
            loc,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDefinition {
    pub ident: String,
    pub generics: Option<ResolvedGenericsDecl>,
    pub fields: Vec<crate::ast::FieldDecl>,
    pub loc: crate::Location,
}

impl StructDefinition {
    pub(crate) fn try_from(
        data: crate::ast::StructDefinition,
        _known_types: &HashMap<String, ResolvedType>,
    ) -> Result<Self, TypingError> {
        let crate::ast::StructDefinition {
            ident,
            generics,
            mut values,
            loc,
        } = data;

        if let Some(generics) = &generics {
            for (_, generic) in &generics.decls {
                values.iter_mut().for_each(|field| {
                    field.ty = field.ty.clone().replace_user_with_generic(&generic);
                })
            }
        }

        Ok(Self {
            ident,
            generics: generics.map(ResolvedGenericsDecl::from),
            fields: values,
            loc,
        })
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        if self.generics.is_none() {
            for field in &mut self.fields {
                field.ty.lower_generics(context)
            }
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)], context: &mut LoweringContext) {
        if self.generics.is_none() {
            return;
        }

        let generics = self
            .generics
            .as_ref()
            .unwrap()
            .decls
            .iter()
            .cloned()
            .filter(|(_, it)| match it {
                ResolvedType::Generic { name, .. } => !types.iter().map(|(n, _)| n).contains(name),
                _ => true,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            generics.len(),
            0,
            "generic not completed.  should not be reached!"
        );
        self.generics = None;
        self.ident = format!(
            "{}<{}>",
            self.ident,
            types.iter().map(|(_, it)| it.to_string()).join(",")
        );
        for field in &mut self.fields {
            field.ty = types.iter().fold(field.ty.clone(), |old_ty, ty| {
                old_ty.replace_generic(&ty.0, ty.1.clone())
            });
            field.ty.lower_generics(context);
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedValueDeclaration {
    pub loc: crate::Location,
    pub is_op: bool,
    pub target: TypedPattern,
    pub args: Vec<TypedArgDeclaration>,
    pub value: TypedValueType,
    pub ty: ResolvedType,
    pub generictypes: Option<ResolvedGenericsDecl>,
    pub abi: Option<crate::ast::Abi>,
    pub is_curried: bool,
}
pub fn collect_args(t: &ResolvedType) -> Vec<ResolvedType> {
    if let ResolvedType::Function { arg, returns, .. } = t {
        [arg.as_ref().clone()]
            .into_iter()
            .chain(collect_args(&returns))
            .collect()
    } else {
        vec![]
    }
}
impl TypedValueDeclaration {
    fn replace_types(&mut self, replaced: &[(String, ResolvedType)]) {
        for arg in &mut self.args {
            arg.replace_types(replaced);
        }
        self.ty = replaced.iter().fold(self.ty.clone(), |ty, (name, new_ty)| {
            ty.replace_generic(name, new_ty.clone())
        });
        match &mut self.value {
            TypedValueType::Expr(expr) => expr.replace_types(replaced),
            TypedValueType::Function(stmnts) => stmnts
                .into_iter()
                .for_each(|stmnt| stmnt.replace_types(&replaced)),
            TypedValueType::External | TypedValueType::Err => (),
        }
    }

    pub(crate) fn try_from(
        data: ast::ValueDeclaration,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        let ast::ValueDeclaration {
            loc,
            is_op,
            target,
            args,
            ty,
            value,
            generics,
            abi,
            id: _,
        } = data;
        let mut known_values = known_values.clone();
        let args = args
            .into_iter()
            .map(TypedArgDeclaration::from)
            .collect_vec();
        known_values.extend(
            args.iter()
                .flat_map(|arg| arg.get_idents_with_types(known_types)),
        );
        let value = match TypedValueType::try_from(value, known_externs, &known_values, known_types)
        {
            Ok(value) => value,
            Err(e) => {
                println!("{:?}", e);
                TypedValueType::Err
            }
        };
        let target = TypedPattern::from(target, &ty, known_types);
        if let Some(abi) = &abi {
            if abi.identifier.as_str() == "C" {
                if let ResolvedType::Function {
                    arg: _, returns, ..
                } = &ty
                {
                    if returns.is_function() {
                        // TODO verify it's a valid c-function.
                    }
                }
            }
        }
        Ok(Self {
            loc,
            is_op,
            target,
            args,
            is_curried: false,
            ty,
            value,
            generictypes: generics.map(ResolvedGenericsDecl::from),
            abi,
        })
    }
    fn lower_generics(&mut self, context: &mut LoweringContext) {
        self.ty.lower_generics(context);
        match &mut self.value {
            TypedValueType::Expr(expr) => expr.lower_generics(context),
            TypedValueType::Function(stmnts) => {
                for stmnt in stmnts {
                    context.args.clear();
                    stmnt.lower_generics(context);
                }
            }
            TypedValueType::Err => (),
            TypedValueType::External => (),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedArgDeclaration {
    Simple {
        loc: crate::Location,
        ident: String,
        ty: ResolvedType,
    },
    DestructureTuple(Vec<TypedArgDeclaration>, ResolvedType, crate::Location),
    DestructureStruct {
        loc: crate::Location,
        struct_ident: String,
        fields: Vec<String>,
        renamed_fields: HashMap<String, String>,
    },
    Discard {
        loc: crate::Location,
        ty: ResolvedType,
    },
    Unit {
        loc: crate::Location,
        ty: ResolvedType,
    },
}

impl From<crate::inference::ast::ArgDeclaration> for TypedArgDeclaration {
    fn from(value: crate::inference::ast::ArgDeclaration) -> Self {
        match value {
            ast::ArgDeclaration::Simple {
                loc,
                ident,
                ty,
                id: _,
            } => Self::Simple { loc, ident, ty },
            ast::ArgDeclaration::DestructureTuple(contents, ty, loc) => {
                Self::DestructureTuple(contents.into_iter().map(|it| it.into()).collect(), ty, loc)
            }
            ast::ArgDeclaration::DestructureStruct {
                loc,
                struct_ident,
                fields,
                renamed_fields,
            } => Self::DestructureStruct {
                loc,
                struct_ident,
                fields,
                renamed_fields,
            },
            ast::ArgDeclaration::Discard { loc, ty } => Self::Discard { loc, ty },
            ast::ArgDeclaration::Unit { loc, ty } => Self::Unit { loc, ty },
        }
    }
}

impl TypedArgDeclaration {
    pub fn get_loc(&self) -> crate::Location {
        let (Self::Simple { loc, .. }
        | Self::DestructureStruct { loc, .. }
        | Self::DestructureTuple(_, _, loc)
        | Self::Discard { loc, .. }
        | Self::Unit { loc, .. }) = self;
        *loc
    }
    pub fn get_ident(&self) -> String {
        match self {
            Self::Discard { .. } => "_".to_string(),
            Self::DestructureTuple(_, _, _) => "".to_string(),
            Self::Unit { .. } => "()".to_string(),
            Self::DestructureStruct { .. } => "<struct>".to_string(),
            Self::Simple { ident, .. } => ident.clone(),
        }
    }

    fn get_idents_with_types(
        &self,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> HashMap<String, ResolvedType> {
        match self {
            // TODO! enum destructure.
            Self::Unit { .. } | Self::Discard { .. } => HashMap::new(),
            Self::DestructureTuple(contents, _, _) => contents
                .iter()
                .flat_map(|it| it.get_idents_with_types(known_types))
                .collect(),
            Self::DestructureStruct {
                loc: _,
                struct_ident,
                fields,
                renamed_fields,
            } => {
                let error_struct = StructDefinition {
                    ident: "<error>".to_string(),
                    generics: None,
                    fields: Vec::new(),
                    loc: (0, 0),
                };
                let struct_ty = if let Some(typ) = known_types.get(struct_ident) {
                    if let ResolvedTypeDeclaration::Struct(typ) = typ {
                        typ
                    } else {
                        &error_struct
                    }
                } else {
                    &error_struct
                };
                let mut out: HashMap<_, _> = fields
                    .iter()
                    .map(|ident| {
                        (
                            ident.clone(),
                            struct_ty
                                .fields
                                .iter()
                                .find_map(|field| {
                                    if &field.name == ident {
                                        Some(field.ty.clone())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or(types::ERROR),
                        )
                    })
                    .collect();
                for (old, new) in renamed_fields {
                    if new != "_" {
                        out.insert(
                            new.clone(),
                            struct_ty
                                .fields
                                .iter()
                                .find_map(|field| {
                                    if &field.name == old {
                                        Some(field.ty.clone())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or(types::ERROR),
                        );
                    }
                }
                out
            }
            Self::Simple { loc: _, ident, ty } => [(ident.clone(), ty.clone())].into(),
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)]) {
        match self {
            TypedArgDeclaration::Simple { ty, .. }
            | TypedArgDeclaration::DestructureTuple(_, ty, _)
            | TypedArgDeclaration::Unit { ty, .. }
            | TypedArgDeclaration::Discard { ty, .. } => {
                *ty = types.iter().fold(ty.clone(), |accum, (old, new)| {
                    accum.replace_generic(old, new.clone())
                })
            }

            TypedArgDeclaration::DestructureStruct {
                loc,
                struct_ident,
                fields,
                renamed_fields,
            } => (),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedValueType {
    Expr(TypedExpr),
    Function(Vec<TypedStatement>),
    Err,
    External,
}

impl TypedValueType {
    pub(crate) fn try_from(
        data: ast::ValueType,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        match data {
            ast::ValueType::Expr(expr) => {
                TypedExpr::try_from(expr, known_externs, known_values, known_types, Vec::new())
                    .map(|expr| Self::Expr(expr))
            }
            ast::ValueType::Function(stmnts) => {
                let mut output = Vec::with_capacity(stmnts.len());
                let mut known_values = known_values.clone();
                for stmnt in stmnts {
                    match TypedStatement::try_from(stmnt, known_externs, &known_values, known_types)
                    {
                        Ok(stmnt) => {
                            if let TypedStatement::Declaration(data) = &stmnt {
                                known_values.extend(data.target.get_idents_with_types());
                            }
                            output.push(stmnt);
                        }
                        Err(e) => {
                            println!("{:?}", e);
                            output.push(TypedStatement::Error);
                        }
                    }
                }
                if !output
                    .iter()
                    .filter_map(|stmnt| match stmnt {
                        TypedStatement::Return(value, _) => {
                            let rt = value.get_ty();
                            if rt != ResolvedType::Error {
                                Some(rt)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    })
                    .all_equal()
                {
                    println!("Not all returns match in type");
                    Err(TypingError::ReturnTypeMismatch)
                } else {
                    Ok(TypedValueType::Function(output))
                }
            }
            ast::ValueType::External => Ok(TypedValueType::External),
        }
    }
    #[deprecated = "need to examine if this can be removed"]
    #[allow(unused)]
    fn get_ty(&self) -> ResolvedType {
        match self {
            Self::Expr(expr) => expr.get_ty(),
            Self::Function(fun) => fun
                .iter()
                .filter_map(|stmnt| match stmnt {
                    TypedStatement::Return(expr, _) => Some(expr.get_ty()),
                    _ => None,
                })
                .fold(
                    fun.last()
                        .map(|last| last.get_ty())
                        .unwrap_or(ResolvedType::Error),
                    |acc, curr| {
                        if acc != ResolvedType::Error && curr == acc {
                            acc
                        } else {
                            ResolvedType::Error
                        }
                    },
                ),
            Self::Err => types::ERROR,
            Self::External => types::ERROR,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStatement {
    Declaration(TypedValueDeclaration),
    Return(TypedExpr, crate::Location),
    FnCall(TypedFnCall),
    Pipe(TypedPipe),
    IfBranching(TypedIfBranching),
    Discard(TypedExpr, crate::Location),
    Match(TypedMatch),
    Error,
}

impl TypedStatement {
    pub(crate) fn try_from(
        statement: ast::Statement,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        match statement {
            ast::Statement::Declaration(data) => Ok(
                match TypedValueDeclaration::try_from(
                    data,
                    known_externs,
                    known_values,
                    known_types,
                ) {
                    Ok(d) => Self::Declaration(d),
                    Err(e) => {
                        println!("{:?}", e);
                        Self::Error
                    }
                },
            ),
            ast::Statement::Return(value, loc) => Ok(Self::Return(
                TypedExpr::try_from(value, known_externs, known_values, known_types, Vec::new())?,
                loc,
            )),
            ast::Statement::FnCall(data) => Ok(Self::FnCall(TypedFnCall::try_from(
                data,
                known_externs,
                known_values,
                known_types,
            )?)),
            ast::Statement::IfStatement(ifstmnt) => Ok(Self::IfBranching(
                TypedIfBranching::try_from(ifstmnt, known_externs, known_values, known_types),
            )),
            ast::Statement::Match(match_) => Ok(Self::Match(TypedMatch::as_statement(
                match_,
                known_externs,
                known_values,
                known_types,
            ))),
            ast::Statement::Error => Ok(Self::Error),
        }
    }
    fn replace_types(&mut self, replaced: &[(String, ResolvedType)]) {
        match self {
            Self::Declaration(data) => data.replace_types(replaced),
            Self::FnCall(data) => data.replace_types(replaced),
            Self::Return(data, _) => data.replace_types(replaced),
            Self::IfBranching(data) => data.replace_types(replaced),
            _ => (),
        }
    }

    fn replace_type(&mut self, name: &str, new_ty: &ResolvedType) {
        self.replace_types(&[(name.to_string(), new_ty.clone())])
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            Self::Declaration(decl) => match &mut decl.value {
                TypedValueType::Expr(expr) => {
                    expr.lower_generics(context);
                    decl.ty = expr.get_ty();
                }
                TypedValueType::Function(_) => {
                    todo!("how to handle this one :/ function inside function");
                }
                TypedValueType::External | TypedValueType::Err => (),
            },
            Self::Discard(expr, _) | Self::Return(expr, _) => expr.lower_generics(context),
            Self::FnCall(call) => call.lower_generics(context),
            Self::Pipe(_) => todo!(),
            Self::IfBranching(ifstmnt) => ifstmnt.lower_generics(context),
            Self::Match(match_) => match_.lower_generics(context),
            Self::Error => todo!(),
        }
    }

    fn get_ty(&self) -> ResolvedType {
        match self {
            Self::Declaration(decl) => decl.ty.clone(),
            Self::Return(expr, _) => expr.get_ty(),
            Self::FnCall(call) => call.rt.clone(),
            Self::Pipe(_) => todo!(),
            Self::Discard(_, _) | Self::Match(_) | Self::IfBranching(_) => types::UNIT,
            Self::Error => todo!(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedIfBranching {
    pub cond: Box<TypedExpr>,
    pub true_branch: Vec<TypedStatement>,
    pub else_ifs: Vec<(Box<TypedExpr>, Vec<TypedStatement>)>,
    pub else_branch: Vec<TypedStatement>,
    pub loc: crate::Location,
}

impl TypedIfBranching {
    fn try_from(
        value: ast::IfBranching,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Self {
        let ast::IfBranching {
            cond,
            true_branch,
            else_ifs,
            else_branch,
            loc,
        } = value;
        let cond = match TypedExpr::try_from(
            *cond,
            known_externs,
            known_values,
            known_types,
            Vec::new(),
        ) {
            Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
            Ok(cond) => {
                let loc = cond.get_loc().unwrap_or_default();
                println!(
                    "condition must be a boolean expresion but got:{} at line:{}, col:{}",
                    cond.get_ty().to_string(),
                    loc.0,
                    loc.1
                );
                TypedExpr::ErrorNode
            }
            Err(e) => {
                println!("{e:?}");
                TypedExpr::ErrorNode
            }
        }
        .into();

        let true_branch = {
            let mut output = Vec::with_capacity(true_branch.len());
            let mut known_values = known_values.clone();
            for stmnt in true_branch {
                match TypedStatement::try_from(stmnt, known_externs, &known_values, known_types) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            known_values.extend(data.target.get_idents_with_types());
                        }
                        output.push(stmnt);
                    }
                    Err(e) => {
                        println!("{:?}", e);
                        output.push(TypedStatement::Error);
                    }
                }
            }
            output
        };

        let else_ifs = else_ifs
            .into_iter()
            .map(|(cond, stmnts)| {
                let cond = match TypedExpr::try_from(
                    *cond,
                    known_externs,
                    known_values,
                    known_types,
                    Vec::new(),
                ) {
                    Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
                    Ok(cond) => {
                        let loc = cond.get_loc().unwrap_or_default();
                        println!(
                            "condition must be a boolean expresion at line:{}, col:{}",
                            loc.0, loc.1
                        );
                        TypedExpr::ErrorNode
                    }
                    Err(e) => {
                        println!("{e:?}");
                        TypedExpr::ErrorNode
                    }
                };
                let mut block_known_values = known_values.clone();
                let block = {
                    let mut block = Vec::new();
                    for stmnt in stmnts {
                        match TypedStatement::try_from(
                            stmnt,
                            known_externs,
                            &block_known_values,
                            known_types,
                        ) {
                            Ok(stmnt) => {
                                if let TypedStatement::Declaration(data) = &stmnt {
                                    block_known_values.extend(data.target.get_idents_with_types());
                                }
                                block.push(stmnt);
                            }
                            Err(e) => {
                                println!("{:?}", e);
                                block.push(TypedStatement::Error);
                            }
                        }
                    }
                    block
                };
                (cond.into(), block)
            })
            .collect();

        let else_branch = {
            let mut else_block_known_values = known_values.clone();
            let mut block = Vec::new();
            for stmnt in else_branch {
                match TypedStatement::try_from(
                    stmnt,
                    known_externs,
                    &else_block_known_values,
                    known_types,
                ) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            else_block_known_values.extend(data.target.get_idents_with_types());
                        }
                        block.push(stmnt);
                    }
                    Err(e) => {
                        println!("{:?}", e);
                        block.push(TypedStatement::Error);
                    }
                }
            }
            block
        };

        Self {
            cond,
            true_branch,
            else_ifs,
            else_branch,
            loc,
        }
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        self.cond.lower_generics(context);
        for (cond, block) in &mut self.else_ifs {
            cond.lower_generics(context);
            block
                .iter_mut()
                .for_each(|stmnt| stmnt.lower_generics(context));
        }
        self.else_branch
            .iter_mut()
            .for_each(|stmnt| stmnt.lower_generics(context));
    }

    fn replace_types(&mut self, replaced: &[(String, ResolvedType)]) {
        for (name, new_ty) in replaced {
            self.cond.replace_type(name, new_ty);
            self.true_branch
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty));
            for (cond, block) in &mut self.else_ifs {
                cond.replace_type(name, new_ty);
                block
                    .iter_mut()
                    .for_each(|it| it.replace_type(name, new_ty));
            }
            self.else_branch
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty));
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedPipe {
    ///if you need to spread more than an u8's worth of values... wtf are you doing? what would even return that many values as a tuple?  
    pub expansion: NonZeroU8,
    pub lhs: Box<TypedExpr>,
    /// THIS HAS A RESTRICTION OF MUST RETURN A FUNCTION
    pub rhs: Box<TypedExpr>,
    /// should match the final return type as [`TypedPipe::rhs`]
    pub rt: ResolvedType,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedFnCall {
    pub loc: crate::Location,
    pub value: Box<TypedExpr>,
    pub arg: Option<Box<TypedExpr>>,
    pub rt: ResolvedType,
    pub arg_t: ResolvedType,
    pub is_extern: bool,
}

impl TypedFnCall {
    pub(crate) fn try_from(
        data: ast::FnCall,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        let ast::FnCall {
            loc,
            value,
            arg,
            id: _,
            returns,
        } = data;
        let value = ok_or_err_node(TypedExpr::try_from(
            *value,
            known_externs,
            known_values,
            known_types,
            Vec::new(),
        ));
        let is_extern = if let TypedExpr::ValueRead(name, _, _) = &value {
            known_externs.contains_key(name)
        } else {
            false
        };

        let arg =
            match TypedExpr::try_from(*arg, known_externs, known_values, known_types, Vec::new()) {
                Ok(arg) => arg,
                Err(e) => {
                    println!("{:?}", e);
                    TypedExpr::ErrorNode
                }
            };

        if value != TypedExpr::ErrorNode {
            let arg_t = arg.get_ty();
            let ty = strip_pointers(&value.get_ty());
            let ResolvedType::Function { arg, .. } = ty else {
                return Err(TypingError::FnNotDeclared);
            };
            if !arg.is_generic() && *arg != arg_t {
                return Err(TypingError::ArgTypeMismatch);
            }
        };
        Ok(Self {
            loc,
            value: value.into(),
            arg_t: arg.get_ty(),
            arg: Some(arg.into()),
            rt: returns,
            is_extern,
        })
    }

    fn replace_types(&mut self, replaced: &[(String, ResolvedType)]) {
        self.value.replace_types(replaced);
        self.arg.as_mut().map(|arg| arg.replace_types(replaced));
        self.rt = replaced.iter().fold(self.rt.clone(), |ty, (name, new_ty)| {
            ty.replace_generic(name, new_ty.clone())
        });
        self.arg_t = replaced
            .iter()
            .fold(self.arg_t.clone(), |ty, (name, new_ty)| {
                ty.replace_generic(name, new_ty.clone())
            });
    }

    fn replace_type(&mut self, name: &str, new_ty: &ResolvedType) {
        self.value.replace_type(name, new_ty);
        self.arg.as_mut().map(|arg| arg.replace_type(name, new_ty));
        self.rt = self.rt.replace_generic(name, new_ty.clone());
        self.arg_t = self.arg_t.replace_generic(name, new_ty.clone());
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        let Self {
            value,
            arg,
            rt,
            arg_t,
            ..
        } = self;
        let arg = arg.as_mut().unwrap();
        let mut old_args = Vec::new();
        std::mem::swap(&mut old_args, &mut context.args);
        arg.lower_generics(context);
        *arg_t = arg.get_ty();
        old_args.push(arg_t.clone());
        std::mem::swap(&mut old_args, &mut context.args);
        value.as_mut().lower_generics(context);
        let ResolvedType::Function { returns, .. } = value.get_ty() else {
            unreachable!()
        };
        *rt = *returns;
        rt.lower_generics(context);
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedStructConstruction {
    pub loc: crate::Location,
    pub fields: HashMap<String, (TypedExpr, crate::Location)>,
    pub generics: Vec<ResolvedType>,
    pub ident: String,
}

impl TypedStructConstruction {
    fn from(
        data: ast::StructConstruction,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        let ast::StructConstruction {
            loc,
            fields,
            generics,
            ident,
            id: _,
            result: _,
        } = data;
        let mut new_fields = HashMap::new();
        let declaration = match known_types.get(&ident) {
            Some(ResolvedTypeDeclaration::Struct(decl)) => decl,
            Some(ResolvedTypeDeclaration::Dependent { actual, .. }) => {
                if let ResolvedTypeDeclaration::Struct(decl) = actual.as_ref() {
                    decl
                } else {
                    println!("not a struct declaration");
                    return Err(TypingError::UnknownType);
                }
            }
            Some(_) => {
                println!("not a struct declaration");
                return Err(TypingError::UnknownType);
            }
            None => {
                println!("Type not found in scope");
                return Err(TypingError::UnknownType);
            }
        };
        for (name, (field, loc)) in fields {
            if new_fields.contains_key(&name) {
                println!("duplicate field {}", name);
                new_fields.insert("_".to_string(), (TypedExpr::ErrorNode, loc));
                continue;
            }
            let field = match TypedExpr::try_from(
                field,
                known_externs,
                known_values,
                known_types,
                Vec::new(),
            ) {
                Ok(expr) => expr,
                Err(e) => {
                    println!("{:?}", e);
                    TypedExpr::ErrorNode
                }
            };
            if field.get_ty() != ResolvedType::Error {
                if let Some(old_field) = declaration.fields.iter().find(|it| it.name == name) {
                    if old_field.ty == field.get_ty() || old_field.ty.is_generic() {
                        new_fields.insert(name, (field, loc));
                    } else {
                        println!(
                            "type mismatch.  expected {} but got {} at line {} column {}",
                            old_field.ty.to_string(),
                            field.get_ty().to_string(),
                            loc.0,
                            loc.1
                        );
                        new_fields.insert(name, (TypedExpr::ErrorNode, loc));
                    }
                } else {
                    println!("{} doesn't exist on type {}", name, ident);
                    new_fields.insert("_".to_string(), (TypedExpr::ErrorNode, loc));
                }
            } else {
                new_fields.insert(name, (TypedExpr::ErrorNode, loc));
            }
        }
        Ok(Self {
            loc,
            fields: new_fields,
            generics,
            ident,
        })
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        if !self.generics.is_empty() {
            self.ident = format!(
                "{}<{}>",
                self.ident,
                self.generics.iter().map(|it| it.to_string()).join(",")
            );
            self.generics.clear();
        }
        self.fields
            .values_mut()
            .for_each(|(v, _)| v.lower_generics(context))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypedExpr {
    /// Integers
    IntegerLiteral {
        value: String,
        size: types::IntWidth,
    },
    /// Floats
    FloatLiteral {
        value: String,
        size: types::FloatWidth,
    },
    /// Strings
    StringLiteral(String),
    /// chars`s
    CharLiteral(String),
    /// ()
    UnitLiteral,
    /// `a + b`
    BinaryOpCall(TypedBinaryOpCall),

    /// `a.b`
    MemeberRead(TypedMemberRead),

    /// `!a`
    UnaryOpCall(TypedUnaryOpCall),
    /// `foo bar`
    FnCall(TypedFnCall),
    /// basically an ident on it's own
    /// (ident,type)
    ValueRead(String, ResolvedType, crate::Location),
    ArrayLiteral {
        contents: Vec<TypedExpr>,
        underlining: ResolvedType,
    },
    /// NOT IMPLEMENTED YET
    /// defined like [| expr, expr, expr, ... |]
    /// type [|T|]
    ListLiteral {
        contents: Vec<TypedExpr>,
    },
    /// NOT IMPLEMENTED YET
    /// defined like (expr, expr,...)
    /// typed as (T, U, V, ...)
    /// not recommended above 3 values
    TupleLiteral {
        contents: Vec<TypedExpr>,
        loc: crate::Location,
    },

    StructConstruction(TypedStructConstruction),

    IfExpr(TypedIfExpr),

    BoolLiteral(bool, crate::Location),

    Match(TypedMatch),
    // This is used to allow to continue type checking.  should never naturally generate.
    ErrorNode,
}
impl TypedExpr {
    pub(crate) fn try_from(
        value: ast::Expr,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
        already_processed_args: Vec<ResolvedType>,
    ) -> Result<Self, TypingError> {
        match value {
            Expr::TupleLiteral {
                contents,
                loc,
                id: _,
            } => Ok(Self::TupleLiteral {
                contents: contents
                    .into_iter()
                    .map(|expr| {
                        Self::try_from(expr, known_externs, known_values, known_types, Vec::new())
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                loc,
            }),
            Expr::NumericLiteral { value, id: _, ty } => {
                if ty.is_int() && value.contains('.') {
                    Err(TypingError::ArgTypeMismatch)
                } else if let ResolvedType::Int { signed: _, width } = &ty {
                    Ok(Self::IntegerLiteral {
                        value,
                        size: *width,
                    })
                } else if let ResolvedType::Float { width } = ty {
                    Ok(Self::FloatLiteral { value, size: width })
                } else {
                    Err(TypingError::UnknownType)
                }
            }
            Expr::StringLiteral(value) => Ok(Self::StringLiteral(value)),
            Expr::CharLiteral(value) => Ok(Self::CharLiteral(value)),
            Expr::UnitLiteral => Ok(Self::UnitLiteral),
            // Expr::Compose { .. } => todo!(),
            Expr::BinaryOpCall(data) if data.operator == "." => {
                Ok(Self::MemeberRead(TypedMemberRead::try_from(
                    data,
                    known_externs,
                    known_values,
                    known_types,
                    already_processed_args,
                )?))
            }
            Expr::BinaryOpCall(data) => Ok(Self::BinaryOpCall(TypedBinaryOpCall::try_from(
                data,
                known_externs,
                known_values,
                known_types,
            )?)),
            // Expr::UnaryOpCall(_) => todo!(), //TODO! unary ops
            Expr::FnCall(data) => Ok(Self::FnCall(TypedFnCall::try_from(
                data,
                known_externs,
                known_values,
                known_types,
            )?)),
            Expr::ValueRead(value, loc, _) => {
                if !known_values.contains_key(&value) {
                    Err(TypingError::UnknownType)
                } else {
                    let ty = known_values[&value].clone();
                    Ok(Self::ValueRead(value, ty, loc))
                }
            }
            Expr::ArrayLiteral { contents, .. } => {
                let contents = contents
                    .into_iter()
                    .map(|value| {
                        TypedExpr::try_from(
                            value,
                            known_externs,
                            known_values,
                            known_types,
                            Vec::new(),
                        )
                    })
                    .map(|value| match value {
                        Ok(value) => value,
                        Err(e) => {
                            println!("{:?}", e);
                            TypedExpr::ErrorNode
                        }
                    })
                    .collect_vec();

                if !contents
                    .iter()
                    .filter_map(|value| {
                        if value == &TypedExpr::ErrorNode {
                            None
                        } else {
                            Some(value.get_ty())
                        }
                    })
                    .all_equal()
                {
                    Err(TypingError::ArgTypeMismatch)
                } else {
                    let underlining = contents
                        .first()
                        .map(|it| it.get_ty())
                        .unwrap_or(types::ERROR);
                    Ok(Self::ArrayLiteral {
                        contents,
                        underlining,
                    })
                }
            }
            #[allow(unused)]
            Expr::ListLiteral {
                contents,
                loc,
                id: _,
            } => todo!(),
            Expr::StructConstruction(strct) => Ok(Self::StructConstruction(
                TypedStructConstruction::from(strct, known_externs, known_values, known_types)?,
            )),
            Expr::If(ifexpr) => Ok(Self::IfExpr(TypedIfExpr::from(
                ifexpr,
                known_externs,
                known_values,
                known_types,
            ))),
            Expr::BoolLiteral(value, loc, _) => Ok(Self::BoolLiteral(value, loc)),
            Expr::Match(match_) => Ok(Self::Match(TypedMatch::from(
                match_,
                known_externs,
                known_values,
                known_types,
            ))),
            Expr::Error(_) => Ok(Self::ErrorNode),
        }
    }

    pub(crate) fn get_loc(&self) -> Option<crate::Location> {
        match self {
            Self::IntegerLiteral { value: _, size: _ } => None,
            Self::FloatLiteral { value: _, size: _ } => None,
            Self::StringLiteral(_) => None,
            Self::CharLiteral(_) => None,
            Self::UnitLiteral => None,
            Self::BinaryOpCall(bin) => Some(bin.loc),
            Self::MemeberRead(read) => Some(read.loc),
            Self::UnaryOpCall(_) => todo!(),
            Self::FnCall(call) => Some(call.loc),
            Self::BoolLiteral(_, loc) | Self::ValueRead(_, _, loc) => Some(*loc),
            Self::ArrayLiteral {
                contents: _,
                underlining: _,
            } => None,
            Self::ListLiteral { contents: _ } => None,
            Self::TupleLiteral { contents: _, loc } => Some(*loc),
            Self::StructConstruction(con) => Some(con.loc),
            Self::IfExpr(ifexpr) => Some(ifexpr.loc),
            Self::Match(match_) => Some(match_.loc),
            Self::ErrorNode => None,
        }
    }

    pub fn get_ty(&self) -> ResolvedType {
        match self {
            Self::BoolLiteral(_, _) => types::BOOL,
            Self::IntegerLiteral { size, .. } => ResolvedType::Int {
                signed: true,
                width: size.clone(),
            },
            Self::FloatLiteral { size, .. } => ResolvedType::Float {
                width: size.clone(),
            },
            Self::StringLiteral(_) => types::STR,
            Self::CharLiteral(_) => types::CHAR,
            Self::UnitLiteral => types::UNIT,
            Self::BinaryOpCall(data) => data.rt.clone(),
            Self::UnaryOpCall(data) => data.rt.clone(),
            Self::FnCall(data) => data.rt.clone(),
            Self::ValueRead(_, ty, _) => ty.clone(),
            Self::ArrayLiteral {
                contents,
                underlining,
            } => ResolvedType::Array {
                underlining: underlining.clone().into(),
                size: contents.len(),
            },
            Self::ListLiteral { .. } => todo!(),
            Self::TupleLiteral { contents, loc: _ } => ResolvedType::Tuple {
                underlining: contents.iter().map(Self::get_ty).collect(),
                loc: (0, 0),
            },
            Self::StructConstruction(strct) => ResolvedType::User {
                name: strct.ident.clone(),
                generics: strct.generics.clone(),
                loc: strct.loc,
            },
            Self::IfExpr(ifexpr) => {
                let expected_ty = ifexpr.true_branch.1.get_ty();
                for (_, _, returned) in &ifexpr.else_ifs {
                    if expected_ty != returned.get_ty() {
                        return ResolvedType::Error;
                    }
                }
                if ifexpr.else_branch.1.get_ty() != expected_ty {
                    ResolvedType::Error
                } else {
                    expected_ty
                }
            }
            Self::MemeberRead(member_read) => member_read.get_ty(),
            Self::Match(match_) => match_.get_ty(),
            Self::ErrorNode => types::ERROR,
        }
    }

    fn replace_type(&mut self, name: &str, new_ty: &ResolvedType) {
        match self {
            Self::BinaryOpCall(data) => data.replace_type(name, new_ty),
            Self::UnaryOpCall(_) => todo!(),
            Self::FnCall(data) => data.replace_type(name, new_ty),
            Self::ValueRead(_, ty, _) => *ty = ty.replace_generic(name, new_ty.clone()),
            Self::ArrayLiteral {
                contents,
                underlining,
            } => {
                if let Some(ty) = contents.iter_mut().fold(None, |accum, it| {
                    it.replace_type(name, new_ty);
                    if accum.is_none() {
                        Some(it.get_ty())
                    } else {
                        accum
                    }
                }) {
                    *underlining = ty;
                }
            }
            Self::ListLiteral { contents } => contents
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty)),
            Self::TupleLiteral { contents, loc: _ } => contents
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty)),

            Self::IfExpr(ifexpr) => {
                ifexpr.replace_type(name, new_ty);
            }
            _ => (),
        }
    }

    fn replace_types(&mut self, replaced: &[(String, ResolvedType)]) {
        for (name, new_ty) in replaced {
            self.replace_type(name, new_ty);
        }
    }

    // pub(crate) fn lower_generics(
    //     &self,
    //     known_values: &HashMap<String, TypedValueDeclaration>,
    // ) -> Option<(String,TypedValueDeclaration,Vec<(String,ResolvedType)>)> {
    //     let Self::FnCall(_) = self else { return None };
    //     let base_fn = self.get_base_fn_call(known_values);
    //     match base_fn {
    //         Some(fun) if fun.generictypes.is_empty() => None,
    //         None => None,
    //         Some(fun) => {
    //             let args = self.collect_args(known_values);
    //             let fun_t = fun.ty.clone();
    //             let (fun_t, mut replaced) = map_types_to_args(fun_t, args);
    //             replaced.reverse();
    //             if fun_t.is_generic() {
    //                 None
    //             } else {
    //                 let mut fun = fun.clone();
    //                 let ident = fun.ident.clone();
    //                 fun.ident =
    //                     fun.ident + "_" + &replaced.iter().map(|(_,it)| it).map(ResolvedType::to_string).join("_");
    //                 fun.ty = fun_t;
    //                 fun = fun.replace_types(&replaced);
    //                 fun.generictypes.clear();
    //                 Some((ident,fun,replaced))
    //             }
    //         }
    //     }
    // }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            Self::BoolLiteral(_, _) => (),
            Self::FnCall(data) => {
                data.lower_generics(context);
            }
            Self::ValueRead(ident, ty, _) if ty.is_function() => {
                let is_generic = ty.is_generic();
                ty.lower_generics(context);
                if !is_generic {
                    return;
                }
                let args = context.args.iter().cloned().collect();
                context.args = Vec::new(); //moving the arg count out due to this is the value of the function call
                let (fun_t, mut replaced) = map_types_to_args(ty.clone(), args);
                replaced.dedup_by_key(|it| it.0.clone()); // should do nothing but I am being overly cautious
                let new_name = format!(
                    "{}<{}>",
                    ident,
                    replaced.iter().map(|(_, it)| it.to_string()).join(",")
                );
                if !context.generated_generics.contains_key(&new_name) {
                    match context.globals.get(ident) {
                        Some(original) => {
                            modify_declaration(original.clone(), &replaced, context, &new_name);
                        }
                        None => todo!("non global generics not supported yet."),
                    }
                }
                *ident = new_name;
                *ty = fun_t;
            }
            Self::ArrayLiteral {
                contents,
                underlining: _,
            }
            | Self::ListLiteral { contents }
            | Self::TupleLiteral { contents, loc: _ } => {
                let mut old_args = Vec::new();
                std::mem::swap(&mut old_args, &mut context.args);
                for value in contents {
                    value.lower_generics(context);
                }
                std::mem::swap(&mut old_args, &mut context.args);
            }
            Self::MemeberRead(read) => read.lower_generics(context),
            Self::IntegerLiteral { .. }
            | Self::FloatLiteral { .. }
            | Self::StringLiteral(_)
            | Self::CharLiteral(_)
            | Self::UnitLiteral => (),
            Self::StructConstruction(strct) => strct.lower_generics(context),
            Self::BinaryOpCall(data) => data.lower_generics(context),
            Self::UnaryOpCall(_data) => todo!("unary op type lowering"),
            Self::ValueRead(_, ty, _) => ty.lower_generics(context),
            Self::IfExpr(data) => data.lower_generics(context),
            Self::Match(match_) => match_.lower_generics(context),
            Self::ErrorNode => (),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedIfExpr {
    pub cond: Box<TypedExpr>,
    pub true_branch: (Vec<TypedStatement>, Box<TypedExpr>),
    pub else_ifs: Vec<(Box<TypedExpr>, Vec<TypedStatement>, Box<TypedExpr>)>,
    pub else_branch: (Vec<TypedStatement>, Box<TypedExpr>),
    pub loc: crate::Location,
}

impl TypedIfExpr {
    pub(crate) fn from(
        value: ast::IfExpr,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Self {
        let ast::IfExpr {
            cond,
            true_branch,
            else_ifs,
            else_branch,
            loc,
            id: _,
            result: _,
        } = value;
        let cond = match TypedExpr::try_from(
            *cond,
            known_externs,
            known_values,
            known_types,
            Vec::new(),
        ) {
            Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
            Ok(cond) => {
                let loc = cond.get_loc().unwrap_or_default();
                println!(
                    "condition must be a boolean but got:{} \nexpresion at line:{}, col:{}",
                    cond.get_ty().to_string(),
                    loc.0,
                    loc.1
                );
                TypedExpr::ErrorNode
            }
            Err(e) => {
                println!("{e:?}");
                TypedExpr::ErrorNode
            }
        }
        .into();

        let mut true_block_known_values = known_values.clone();
        let true_branch_block = {
            let mut block = Vec::new();
            for stmnt in true_branch.0 {
                match TypedStatement::try_from(
                    stmnt,
                    known_externs,
                    &true_block_known_values,
                    known_types,
                ) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            true_block_known_values.extend(data.target.get_idents_with_types());
                        }
                        block.push(stmnt);
                    }
                    Err(e) => {
                        println!("{:?}", e);
                        block.push(TypedStatement::Error);
                    }
                }
            }
            block
        };
        let mut expected_ty = ResolvedType::Error;
        let true_branch_ret = match TypedExpr::try_from(
            *true_branch.1,
            known_externs,
            &true_block_known_values,
            known_types,
            Vec::new(),
        ) {
            Ok(ret) => {
                expected_ty = ret.get_ty();
                ret
            }
            Err(e) => {
                println!("{e:?}");
                TypedExpr::ErrorNode
            }
        };

        let else_ifs = else_ifs.into_iter().map(|(cond, stmnts,ret)| {
            let cond = match TypedExpr::try_from(*cond, known_externs, known_values, known_types, Vec::new()){
                Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
                Ok(cond) => {
                    let loc = cond.get_loc().unwrap_or_default();
                    println!("condition must be a boolean expresion at line:{}, col:{}", loc.0, loc.1);
                    TypedExpr::ErrorNode
                },
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            };
            let mut block_known_values = known_values.clone();
            let block = {
                let mut block = Vec::new();
                for stmnt in stmnts {
                    match TypedStatement::try_from(stmnt, known_externs, &block_known_values, known_types) {
                        Ok(stmnt) => {
                            if let TypedStatement::Declaration(data) = &stmnt {
                                block_known_values.extend(data.target.get_idents_with_types());
                            }
                            block.push(stmnt);
                        }
                        Err(e) => {
                            println!("{:?}", e);
                            block.push(TypedStatement::Error);
                        }
                    }
                }
                block
            };
            let ret = match TypedExpr::try_from(*ret, known_externs, &block_known_values, known_types,Vec::new()){
                Ok(ret) => {
                    if expected_ty == ResolvedType::Error {
                        expected_ty = ret.get_ty();
                    } else if ret.get_ty() != expected_ty {
                        let loc = ret.get_loc().unwrap_or_default();
                        println!(
                            "all branches of an if expression must match in return type.\n  expected: {},\n actual: {} \n at line:{}, col :{}",
                            expected_ty.to_string(),
                            ret.get_ty().to_string(),
                            loc.0,
                            loc.1
                        );
                    }
                    ret
                },
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            };

            (cond.into(),block,ret.into())
        }).collect();

        let mut else_block_known_values = known_values.clone();
        let else_branch_block = {
            let mut block = Vec::new();
            for stmnt in else_branch.0 {
                match TypedStatement::try_from(
                    stmnt,
                    known_externs,
                    &else_block_known_values,
                    known_types,
                ) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            else_block_known_values.extend(data.target.get_idents_with_types());
                        }
                        block.push(stmnt);
                    }
                    Err(e) => {
                        println!("{:?}", e);
                        block.push(TypedStatement::Error);
                    }
                }
            }
            block
        };
        let else_branch_ret = match TypedExpr::try_from(
            *else_branch.1,
            known_externs,
            &else_block_known_values,
            known_types,
            Vec::new(),
        ) {
            Ok(ret) => {
                if expected_ty == ResolvedType::Error {
                    #[allow(unused_assignments)] //here to supress a warning for now.
                    {
                        //hopefully this only happens if there is only if/else and the if branch doesn't have a valid expression
                        expected_ty = ret.get_ty();
                    }
                    // not sure what to report here tbh.
                } else if ret.get_ty() != expected_ty {
                    let loc = ret.get_loc().unwrap_or_default();
                    println!(
                        "all branches of an if expression must match in return type.\n  expected: {},\n actual: {} \n at line:{}, col :{}",
                        expected_ty.to_string(),
                        ret.get_ty().to_string(),
                        loc.0,
                        loc.1
                    );
                }
                ret
            }
            Err(e) => {
                println!("{e:?}");
                TypedExpr::ErrorNode
            }
        };
        Self {
            cond,
            true_branch: (true_branch_block, true_branch_ret.into()),
            else_ifs,
            else_branch: (else_branch_block, else_branch_ret.into()),
            loc,
        }
    }

    fn replace_type(&mut self, name: &str, new_ty: &ResolvedType) {
        self.cond.replace_type(name, new_ty);
        for (cond, block, ret) in &mut self.else_ifs {
            cond.replace_type(name, new_ty);
            block
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty));
            ret.replace_type(name, new_ty);
        }
        self.else_branch
            .0
            .iter_mut()
            .for_each(|it| it.replace_type(name, new_ty));
        self.else_branch.1.replace_type(name, new_ty);
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        self.cond.lower_generics(context);
        for (cond, block, ret) in &mut self.else_ifs {
            cond.lower_generics(context);
            block.iter_mut().for_each(|it| it.lower_generics(context));
            ret.lower_generics(context);
        }
        self.else_branch
            .0
            .iter_mut()
            .for_each(|it| it.lower_generics(context));
        self.else_branch.1.lower_generics(context);
    }

    pub fn get_ty(&self) -> ResolvedType {
        self.true_branch.1.get_ty()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedMemberRead {
    pub target: Box<TypedExpr>,
    pub member: String,
    pub offset: Option<usize>,
    pub ty: ResolvedType,
    pub loc: crate::Location,
}
impl TypedMemberRead {
    pub(crate) fn try_from(
        value: ast::BinaryOpCall,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
        already_processed_args: Vec<ResolvedType>,
    ) -> Result<Self, TypingError> {
        let ast::BinaryOpCall { loc, lhs, rhs, .. } = value;
        let ast::Expr::ValueRead(member, _, _) = *rhs else {
            return Err(TypingError::MemberMustBeIdent);
        };
        let value = TypedExpr::try_from(
            *lhs,
            known_externs,
            known_values,
            known_types,
            already_processed_args,
        )?;
        let ty = value.get_ty();

        if ty == ResolvedType::Error {
            return Err(TypingError::UnknownType);
        }
        let ResolvedType::User { generics, .. } = ty.clone() else {
            unreachable!()
        };
        if !generics.is_empty() {
            // we will have to do checking after lowering.  will do as part of lowering.
            Ok(Self {
                target: value.into(),
                member,
                offset: None,
                ty: ResolvedType::Error,
                loc,
            })
        } else if let Some(strct) = known_types.get(&ty.to_string()) {
            let def = match strct {
                ResolvedTypeDeclaration::Struct(def) => def,
                ResolvedTypeDeclaration::Dependent { actual, .. } => {
                    if let ResolvedTypeDeclaration::Struct(def) = actual.as_ref() {
                        def
                    } else {
                        println!("not a struct declaration");
                        return Err(TypingError::UnknownType);
                    }
                }
                _ => {
                    println!("not a struct declaration");
                    return Err(TypingError::UnknownType);
                }
            };
            let offset = def.fields.iter().position(|it| it.name == member);
            let ty = def
                .fields
                .iter()
                .find_map(|it| {
                    if it.name == member {
                        Some(it.ty.clone())
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    known_values
                        .get(&format!("{}::{}", def.ident, member))
                        .cloned()
                })
                .unwrap_or(ResolvedType::Error);
            Ok(Self {
                target: value.into(),
                offset,
                member,
                ty,
                loc,
            })
        } else {
            Err(TypingError::UnknownType)
        }
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        if let ResolvedType::User { generics, .. } = self.target.get_ty() {
            if generics.is_empty() {
                return;
            }
        } else {
            return;
        }
        self.target.lower_generics(context);
        let Some(strct) = context
            .generated_generics
            .get(&(self.target.get_ty().to_string()))
        else {
            self.ty = ResolvedType::Error;
            return;
        };
        let TypedDeclaration::TypeDefinition(ResolvedTypeDeclaration::Struct(def)) = strct else {
            unreachable!("how are you accessing a member not on a struct")
        };
        self.offset = def.fields.iter().position(|it| it.name == self.member);
        self.ty = def
            .fields
            .iter()
            .find_map(|it| {
                if it.name == self.member {
                    Some(it.ty.clone())
                } else {
                    None
                }
            })
            .or_else(|| {
                context
                    .functions
                    .get(&format!("{}::{}", def.ident, self.member))
                    .cloned()
            })
            .unwrap_or(ResolvedType::Error);
    }

    fn get_ty(&self) -> ResolvedType {
        self.ty.clone()
    }
}

#[derive(Clone)]
pub(crate) struct LoweringContext {
    pub(crate) globals: HashMap<String, TypedDeclaration>,
    pub(crate) generated_generics: HashMap<String, TypedDeclaration>,
    pub(crate) functions: HashMap<String, ResolvedType>,
    pub(crate) args: Vec<ResolvedType>,
}

pub fn map_types_to_args(
    fun: ResolvedType,
    mut args: Vec<ResolvedType>,
) -> (ResolvedType, Vec<(String, ResolvedType)>) {
    if args.is_empty() || !fun.is_generic() {
        return (fun, vec![]);
    }
    let ResolvedType::Function { arg, returns, loc } = &fun else {
        unreachable!()
    };
    let arg_t = args.pop().unwrap();
    if let ResolvedType::Generic { name, .. } = arg.as_ref() {
        let fun = returns.replace_generic(name, arg_t.clone());
        let (fun, mut replaced) = map_types_to_args(fun, args);
        replaced.push((name.clone(), arg_t.clone()));
        (
            ResolvedType::Function {
                arg: arg_t.into(),
                returns: fun.into(),
                loc: *loc,
            },
            replaced,
        )
    } else {
        let (returns, replaced) = map_types_to_args(returns.as_ref().clone(), args);
        (
            ResolvedType::Function {
                arg: arg_t.into(),
                returns: returns.into(),
                loc: (0, 0),
            },
            replaced,
        )
    }
}

fn modify_declaration(
    mut to_lower: TypedDeclaration,
    replaced: &[(String, ResolvedType)],
    context: &mut LoweringContext,
    new_name: &String,
) {
    to_lower.replace_types(&replaced, context);
    if let TypedDeclaration::Value(to_lower) = &mut to_lower {
        match &mut to_lower.value {
            TypedValueType::Expr(expr) => expr.lower_generics(context),
            TypedValueType::Function(stmnts) => {
                for stmnt in stmnts {
                    context.args.clear();
                    stmnt.lower_generics(context);
                }
            }
            TypedValueType::External | TypedValueType::Err => (),
        }
        to_lower
            .generics
            .as_mut()
            .unwrap()
            .decls
            .retain(|(_, g)| match g {
                ResolvedType::Generic { name, .. } => !replaced.iter().any(|(r, _)| r == name),
                _ => false,
            });
        if to_lower.generics.as_ref().unwrap().decls.is_empty() {
            to_lower.generics = None;
        }
    }
    match &mut to_lower {
        TypedDeclaration::Value(data) => data.ident = new_name.clone(),
        TypedDeclaration::TypeDefinition(_) => todo!(),
    }

    context
        .generated_generics
        .insert(new_name.clone(), to_lower);
}

fn ok_or_err_node(it: Result<TypedExpr, TypingError>) -> TypedExpr {
    match it {
        Ok(expr) => expr,
        Err(e) => {
            println!("{:?}", e);
            TypedExpr::ErrorNode
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedBinaryOpCall {
    pub loc: crate::Location,
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
    pub operator: String,
    pub rt: ResolvedType,
}

impl TypedBinaryOpCall {
    fn lower_generics(&mut self, context: &mut LoweringContext) {
        let Self { lhs, rhs, .. } = self;
        let mut old_args = Vec::new();
        std::mem::swap(&mut context.args, &mut old_args);
        lhs.lower_generics(context);
        rhs.lower_generics(context);
        self.reeval_rt();
        std::mem::swap(&mut context.args, &mut old_args);
    }

    fn replace_type(&mut self, name: &str, new_ty: &ResolvedType) {
        let Self { lhs, rhs, .. } = self;
        lhs.replace_type(name, new_ty);
        rhs.replace_type(name, new_ty);
        self.reeval_rt();
    }

    fn reeval_rt(&mut self) {
        let Self {
            lhs,
            rhs,
            operator,
            rt,
            ..
        } = self;
        match operator.as_str() {
            "&&" | "||" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if lhs_t == types::BOOL && rhs_t == types::BOOL {
                    *rt = types::BOOL;
                } else {
                    println!("operator not supported");
                    *rt = types::ERROR;
                }
            }

            "==" | "!=" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if lhs_t == rhs_t && (lhs_t.is_int() || lhs_t.is_float() || lhs_t == types::BOOL) {
                    if lhs_t.is_float() {
                        println!("WARNING:comparing two floats for equality")
                    }
                    *rt = types::BOOL;
                } else {
                    println!("op not supported");
                    *rt = types::ERROR;
                }
            }
            "<=" | "<" | ">=" | ">" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if lhs_t == rhs_t && (lhs_t.is_int() || lhs_t.is_float() || lhs_t == types::BOOL) {
                    *rt = types::BOOL;
                } else {
                    println!("op not supported");
                    *rt = types::ERROR;
                }
            }
            "*" | "+" | "/" | "-" => {
                // TODO: need to add support for overloading this.
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if (lhs_t.is_float() || lhs_t.is_int() || lhs_t.is_generic())
                    && (rhs_t.is_float() || rhs_t.is_int() || rhs_t.is_generic())
                {
                    *rt = match (lhs_t, rhs_t) {
                        (lhs, rhs) if lhs.is_float() && rhs.is_float() => {
                            let ResolvedType::Float { width: lhs_w } = lhs else {
                                unreachable!()
                            };
                            let ResolvedType::Float { width: rhs_w } = rhs else {
                                unreachable!()
                            };
                            ResolvedType::Float {
                                width: lhs_w.max(rhs_w),
                            }
                        }
                        (lhs, rhs) if lhs.is_float() || rhs.is_generic() => lhs,
                        (lhs, rhs) if rhs.is_float() || lhs.is_generic() => rhs,
                        (lhs, rhs) => {
                            let ResolvedType::Int {
                                signed: lhs_signed,
                                width: lhs_w,
                            } = lhs
                            else {
                                unreachable!()
                            };
                            let ResolvedType::Int {
                                signed: rhs_signed,
                                width: rhs_w,
                            } = lhs
                            else {
                                unreachable!()
                            };
                            if lhs_signed && !rhs_signed {
                                lhs
                            } else if rhs_signed && !lhs_signed {
                                rhs
                            } else {
                                ResolvedType::Int {
                                    signed: lhs_signed,
                                    width: lhs_w.max(rhs_w),
                                }
                            }
                        }
                    }
                } else {
                    println!("operation not supported");
                    self.rt = ResolvedType::Error;
                }
            }
            "**" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if (lhs_t.is_float() || lhs_t.is_int() || lhs_t.is_generic())
                    && (rhs_t.is_float() || rhs_t.is_int() || rhs_t.is_generic())
                {
                    *rt = match (lhs_t, rhs_t) {
                        (lhs, rhs) if lhs.is_float() && rhs.is_float() => {
                            let ResolvedType::Float { width: lhs } = lhs else {
                                unreachable!()
                            };
                            let ResolvedType::Float { width: rhs } = rhs else {
                                unreachable!()
                            };
                            ResolvedType::Float {
                                width: lhs.max(rhs),
                            }
                        }
                        (lhs, _) if lhs.is_float() => lhs,
                        (_, rhs) if rhs.is_float() => rhs,
                        (lhs, rhs) => {
                            let ResolvedType::Int {
                                signed: _lhs_signed,
                                width: lhs_w,
                            } = lhs
                            else {
                                unreachable!()
                            };
                            let ResolvedType::Int {
                                signed: _rhs_signed,
                                width: rhs_w,
                            } = rhs
                            else {
                                unreachable!()
                            };
                            let max = lhs_w.max(rhs_w);
                            ResolvedType::Float {
                                width: match max {
                                    IntWidth::SixtyFour => FloatWidth::SixtyFour,
                                    _ => FloatWidth::ThirtyTwo,
                                },
                            }
                        }
                    }
                } else {
                    println!("operation not supported");
                    *rt = ResolvedType::Error;
                }
            }
            _ => {
                println!("operation not supported");
                *rt = ResolvedType::Error;
            }
        }
    }

    pub(crate) fn try_from(
        value: ast::BinaryOpCall,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Result<Self, TypingError> {
        let ast::BinaryOpCall {
            loc,
            lhs,
            rhs,
            operator,
            id: _,
            result: _,
        } = value;
        let lhs =
            match TypedExpr::try_from(*lhs, known_externs, known_values, known_types, Vec::new()) {
                Ok(lhs) => lhs,
                Err(e) => {
                    println!("{:?}", e);
                    TypedExpr::ErrorNode
                }
            };
        let rhs =
            match TypedExpr::try_from(*rhs, known_externs, known_values, known_types, Vec::new()) {
                Ok(rhs) => rhs,
                Err(e) => {
                    println!("{:?}", e);
                    TypedExpr::ErrorNode
                }
            };
        match operator.as_str() {
            "&&" | "||" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if lhs_t == types::BOOL && rhs_t == types::BOOL {
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: types::BOOL,
                    })
                } else if lhs_t.is_generic()
                    || lhs_t == ResolvedType::Error
                    || rhs_t.is_generic()
                    || rhs_t == ResolvedType::Error
                {
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: types::ERROR,
                    })
                } else {
                    println!("operator not supported");
                    Err(TypingError::OpNotSupported)
                }
            }
            "==" | "!=" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if lhs_t == rhs_t && (lhs_t.is_int() || lhs_t.is_float() || lhs_t == types::BOOL) {
                    if lhs_t.is_float() {
                        println!("WARNING:comparing two floats for equality")
                    }
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: types::BOOL,
                    })
                } else if lhs_t.is_generic()
                    || lhs_t == ResolvedType::Error
                    || rhs_t.is_generic()
                    || rhs_t == ResolvedType::Error
                {
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: types::ERROR,
                    })
                } else {
                    println!("op not supported");
                    Err(TypingError::OpNotSupported)
                }
            }
            "<=" | "<" | ">=" | ">" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if lhs_t == rhs_t && (lhs_t.is_int() || lhs_t.is_float() || lhs_t == types::BOOL) {
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: types::BOOL,
                    })
                } else if lhs_t.is_generic()
                    || lhs_t == ResolvedType::Error
                    || rhs_t.is_generic()
                    || rhs_t == ResolvedType::Error
                {
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: types::ERROR,
                    })
                } else {
                    println!("op not supported");
                    Err(TypingError::OpNotSupported)
                }
            }
            "*" | "+" | "/" | "-" => {
                // TODO: need to add support for overloading this.
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if (lhs_t.is_float()
                    || lhs_t.is_int()
                    || lhs_t.is_generic()
                    || lhs_t == types::ERROR)
                    && (rhs_t.is_float()
                        || rhs_t.is_int()
                        || rhs_t.is_generic()
                        || rhs_t == types::ERROR)
                {
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: match (lhs_t, rhs_t) {
                            (ResolvedType::Error, _) | (_, ResolvedType::Error) => types::ERROR,
                            (lhs, rhs) if lhs.is_float() && rhs.is_float() => {
                                let ResolvedType::Float { width: lhs_w } = lhs else {
                                    unreachable!()
                                };
                                let ResolvedType::Float { width: rhs_w } = rhs else {
                                    unreachable!()
                                };
                                ResolvedType::Float {
                                    width: lhs_w.max(rhs_w),
                                }
                            }
                            (lhs, rhs) if lhs.is_float() || rhs.is_generic() => lhs,
                            (lhs, rhs) if rhs.is_float() || lhs.is_generic() => rhs,
                            (lhs, rhs) => {
                                let ResolvedType::Int {
                                    signed: lhs_signed,
                                    width: lhs_w,
                                } = lhs
                                else {
                                    unreachable!()
                                };
                                let ResolvedType::Int {
                                    signed: rhs_signed,
                                    width: rhs_w,
                                } = lhs
                                else {
                                    unreachable!()
                                };
                                if lhs_signed && !rhs_signed {
                                    lhs
                                } else if rhs_signed && !lhs_signed {
                                    rhs
                                } else {
                                    ResolvedType::Int {
                                        signed: lhs_signed,
                                        width: lhs_w.max(rhs_w),
                                    }
                                }
                            }
                        },
                    })
                } else {
                    Err(TypingError::OpNotSupported)
                }
            }
            "**" => {
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if (lhs_t.is_float()
                    || lhs_t.is_int()
                    || lhs_t.is_generic()
                    || lhs_t == types::ERROR)
                    && (rhs_t.is_float()
                        || rhs_t.is_int()
                        || rhs_t.is_generic()
                        || rhs_t == types::ERROR)
                {
                    Ok(Self {
                        loc,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                        operator,
                        rt: match (lhs_t, rhs_t) {
                            (ResolvedType::Error, _) | (_, ResolvedType::Error) => types::ERROR,
                            (lhs, rhs) if lhs.is_float() && rhs.is_float() => {
                                let ResolvedType::Float { width: lhs } = lhs else {
                                    unreachable!()
                                };
                                let ResolvedType::Float { width: rhs } = rhs else {
                                    unreachable!()
                                };
                                ResolvedType::Float {
                                    width: lhs.max(rhs),
                                }
                            }
                            (lhs, _) if lhs.is_float() => lhs,
                            (_, rhs) if rhs.is_float() => rhs,
                            (lhs, rhs) => {
                                let ResolvedType::Int {
                                    signed: _lhs_signed,
                                    width: lhs_w,
                                } = lhs
                                else {
                                    unreachable!()
                                };
                                let ResolvedType::Int {
                                    signed: _rhs_signed,
                                    width: rhs_w,
                                } = rhs
                                else {
                                    unreachable!()
                                };
                                let max = lhs_w.max(rhs_w);
                                ResolvedType::Float {
                                    width: match max {
                                        IntWidth::SixtyFour => FloatWidth::SixtyFour,
                                        _ => FloatWidth::ThirtyTwo,
                                    },
                                }
                            }
                        },
                    })
                } else {
                    Err(TypingError::OpNotSupported)
                }
            }
            _ => Err(TypingError::OpNotSupported),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedUnaryOpCall {
    pub operand: Box<TypedExpr>,
    pub operator: String,
    pub rt: ResolvedType,
}

fn strip_pointers(ty: &ResolvedType) -> ResolvedType {
    if let ResolvedType::Function { arg, returns, loc } = ty {
        let arg = match arg.as_ref() {
            ResolvedType::Pointer { underlining } if underlining.is_function() => {
                strip_pointers(underlining.as_ref()).into()
            }
            _ => arg.clone(),
        };
        let returns = match returns.as_ref() {
            ResolvedType::Pointer { underlining } if underlining.is_function() => {
                strip_pointers(underlining.as_ref()).into()
            }
            _ => returns.clone(),
        };
        ResolvedType::Function {
            arg,
            returns,
            loc: *loc,
        }
    } else {
        ty.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedMatch {
    pub loc: crate::Location,
    pub on: Box<TypedExpr>,
    pub arms: Vec<TypedMatchArm>,
}

impl TypedMatch {
    fn from(
        value: ast::Match,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Self {
        let ast::Match {
            loc,
            on,
            arms,
            id: _,
        } = value;
        let on: Box<_> =
            match TypedExpr::try_from(*on, known_externs, known_values, known_types, Vec::new()) {
                Ok(it) => it,
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            }
            .into();
        let mut new_arms = Vec::with_capacity(arms.len());
        for arm in arms {
            let arm =
                TypedMatchArm::from(arm, known_externs, known_values, known_types, &on.get_ty());
            new_arms.push(arm);
        }
        Self {
            loc,
            on,
            arms: new_arms,
        }
    }

    fn as_statement(
        value: ast::Match,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Self {
        let ast::Match {
            loc,
            on,
            arms,
            id: _,
        } = value;
        let on: Box<_> =
            match TypedExpr::try_from(*on, known_externs, known_values, known_types, Vec::new()) {
                Ok(it) => it,
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            }
            .into();
        let mut new_arms = Vec::with_capacity(arms.len());
        for arm in arms {
            let arm = TypedMatchArm::as_statement(
                arm,
                known_externs,
                known_values,
                known_types,
                &on.get_ty(),
            );
            new_arms.push(arm);
        }
        Self {
            loc,
            on,
            arms: new_arms,
        }
    }

    pub fn get_ty(&self) -> ResolvedType {
        if self
            .arms
            .iter()
            .map(|it| it.ret.as_ref().map(|it| it.get_ty()))
            .all_equal()
        {
            self.arms
                .first()
                .and_then(|it| it.ret.as_ref().map(|it| it.get_ty()))
                .unwrap_or(types::UNIT)
        } else {
            let types = self
                .arms
                .iter()
                .map(|it| it.ret.as_ref().map(|it| it.get_ty()).unwrap_or(types::UNIT))
                .filter(ResolvedType::is_error)
                .counts();
            let most_common = types
                .iter()
                .max_by_key(|(_, it)| *it)
                .map(|(it, _)| it)
                .cloned()
                .unwrap();
            if !most_common.is_void_or_unit() {
                for arm in &self.arms {
                    if arm.ret.as_ref().map(|it| it.get_ty()).as_ref() != Some(&most_common) {
                        let loc = arm
                            .ret
                            .as_ref()
                            .and_then(|it| it.get_loc())
                            .unwrap_or(arm.loc);
                        println!(
                            "expected {}, but got {} at line:{}, col:{}",
                            most_common.to_string(),
                            arm.ret
                                .as_ref()
                                .map(|it| it.get_ty())
                                .unwrap_or(types::UNIT)
                                .to_string(),
                            loc.0,
                            loc.1
                        );
                    }
                }
            }
            most_common
        }
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        let Self { on, arms, .. } = self;
        on.lower_generics(context);
        for arm in arms {
            arm.lower_generics(context)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedMatchArm {
    pub loc: crate::Location,
    pub cond: TypedPattern,
    pub block: Vec<TypedStatement>,
    pub ret: Option<Box<TypedExpr>>,
}

impl TypedMatchArm {
    fn from(
        value: ast::MatchArm,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
        expected_comp: &ResolvedType,
    ) -> Self {
        let ast::MatchArm {
            block,
            ret,
            cond,
            loc,
        } = value;
        let mut known_values = known_values.clone();
        let mut new_block = Vec::with_capacity(block.len());
        let cond = TypedPattern::from(cond, expected_comp, known_types);
        known_values.extend(cond.get_idents_with_types());
        for stmnt in block {
            match TypedStatement::try_from(stmnt, known_externs, &known_values, known_types) {
                Ok(stmnt) => {
                    if let TypedStatement::Declaration(v) = &stmnt {
                        known_values.extend(v.target.get_idents_with_types());
                    }
                    new_block.push(stmnt);
                }
                Err(e) => {
                    println!("{e:?}");
                    new_block.push(TypedStatement::Error);
                }
            }
        }
        let ret = ret.map(|ret| {
            match TypedExpr::try_from(*ret, known_externs, &known_values, known_types, Vec::new()) {
                Ok(ret) => ret,
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            }
            .into()
        });
        Self {
            loc,
            cond,
            block: new_block,
            ret,
        }
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        let Self { block, ret, .. } = self;
        for stmnt in block {
            stmnt.lower_generics(context);
        }
        if let Some(ret) = ret.as_mut() {
            ret.lower_generics(context);
        }
    }

    fn as_statement(
        arm: ast::MatchArm,
        known_externs: &HashMap<String, ResolvedType>,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
        expected_comp: &ResolvedType,
    ) -> TypedMatchArm {
        let ast::MatchArm {
            block,
            ret,
            cond,
            loc,
        } = arm;
        let mut known_values = known_values.clone();
        let mut new_block = Vec::with_capacity(block.len());
        let cond = TypedPattern::from(cond, expected_comp, known_types);
        known_values.extend(cond.get_idents_with_types());
        for stmnt in block {
            match TypedStatement::try_from(stmnt, known_externs, &known_values, known_types) {
                Ok(stmnt) => {
                    if let TypedStatement::Declaration(v) = &stmnt {
                        known_values.extend(v.target.get_idents_with_types());
                    }
                    new_block.push(stmnt);
                }
                Err(e) => {
                    println!("{e:?}");
                    new_block.push(TypedStatement::Error);
                }
            }
        }
        if let Some(ret) = ret {
            let ret = match TypedExpr::try_from(
                *ret,
                known_externs,
                &known_values,
                known_types,
                Vec::new(),
            ) {
                Ok(ret) => ret,
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            };
            if ret != TypedExpr::UnitLiteral {
                let loc = ret.get_loc().unwrap_or_default();
                new_block.push(TypedStatement::Discard(ret, loc));
            }
        }

        Self {
            loc,
            cond,
            block: new_block,
            ret: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedPattern {
    Const(String, ResolvedType),
    Read(String, ResolvedType, crate::Location),
    Err,
    Default,
    Or(Box<Self>, Box<Self>),
    Destructure(TypedDestructure),
    EnumVariant {
        ty: ResolvedType,
        variant: String,
        pattern: Option<Box<Self>>,
        loc: crate::Location,
    },
}

impl TypedPattern {
    fn from(
        value: ast::Pattern,
        expected_type: &ResolvedType,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Self {
        match dbg!(value) {
            ast::Pattern::EnumVariant {
                ty,
                variant,
                pattern,
                loc,
            } => {
                if let Some(ResolvedTypeDeclaration::Dependent { base, actual }) =
                    known_types.get(&variant)
                {
                    let base: ResolvedType = base.as_ref().into();
                    if &base != expected_type {
                        println!("Err incorrect enum matched");
                        return Self::Err;
                    }
                    Self::EnumVariant {
                        ty,
                        variant,
                        pattern: pattern.map(|pat| {
                            Self::from(*pat, &actual.as_ref().into(), known_types).into()
                        }),
                        loc,
                    }
                } else {
                    Self::Err
                }
            }
            ast::Pattern::Default => Self::Default,
            ast::Pattern::ConstNumber(n, ty) if &ty == expected_type => {
                Self::Const(n, expected_type.clone())
            }
            ast::Pattern::ConstNumber(n, ty) => {
                println!(
                    "trying to match against a number (\"{n}\") when {} is expected",
                    expected_type.to_string()
                );
                Self::Const(n, types::ERROR)
            }
            ast::Pattern::ConstStr(s) if expected_type == &types::STR => Self::Const(s, types::STR),
            ast::Pattern::ConstChar(c) if expected_type == &types::CHAR => {
                Self::Const(c, types::CHAR)
            }
            ast::Pattern::ConstBool(b) if expected_type == &types::BOOL => {
                Self::Const(b.to_string(), types::BOOL)
            }
            ast::Pattern::ConstStr(s) => {
                println!(
                    "trying to match against a string when {} is expected",
                    expected_type.to_string()
                );
                Self::Const(s, types::ERROR)
            }
            ast::Pattern::ConstChar(s) => {
                println!(
                    "trying to match against a char when {} is expected",
                    expected_type.to_string()
                );
                Self::Const(s, types::ERROR)
            }
            ast::Pattern::ConstBool(b) => {
                println!(
                    "trying to match against a bool when {} is expected",
                    expected_type.to_string()
                );
                Self::Const(b.to_string(), types::ERROR)
            }
            ast::Pattern::Read {
                ident,
                loc,
                ty,
                id: _,
            } => {
                if &ty != expected_type {
                    println!("this should report a diagnostic");
                    Self::Err
                } else {
                    Self::Read(ident, ty, loc)
                }
            }
            ast::Pattern::Destructure(destruct) => {
                Self::Destructure(TypedDestructure::from(destruct, expected_type, known_types))
            }
            ast::Pattern::Or(lhs, rhs) =>
            //TODO! diagnostics if not all binds are in both patterns.
            {
                Self::Or(
                    Self::from(*lhs, expected_type, known_types).into(),
                    Self::from(*rhs, expected_type, known_types).into(),
                )
            }
            ast::Pattern::Err => Self::Err,
        }
    }

    pub fn get_binds(&self) -> HashSet<String> {
        match self {
            Self::Default | Self::Err | Self::Const(_, _) => HashSet::new(),
            Self::Destructure(d) => d.get_binds(),
            Self::Read(name, _, _) => [name.clone()].into(),
            Self::Or(lhs, rhs) => {
                let lhs = lhs.get_binds();
                lhs
            }
            Self::EnumVariant { pattern, .. } => pattern
                .as_ref()
                .map(Box::as_ref)
                .map(Self::get_binds)
                .unwrap_or_default(),
        }
    }

    pub fn get_idents_with_types(&self) -> HashMap<String, ResolvedType> {
        match self {
            Self::Default | Self::Err | Self::Const(_, _) => HashMap::new(),
            Self::Destructure(d) => d.get_idents_with_types(),
            Self::Or(lhs, rhs) => {
                let mut lhs = lhs.get_idents_with_types();
                let rhs = rhs.get_idents_with_types();
                if lhs == rhs {
                    lhs
                } else {
                    lhs.insert("<error>".to_string(), types::ERROR);
                    lhs
                }
            }
            Self::Read(ident, ty, _) => [(ident.clone(), ty.clone())].into(),
            Self::EnumVariant { pattern, .. } => pattern
                .as_ref()
                .map(Box::as_ref)
                .map(Self::get_idents_with_types)
                .unwrap_or_default(),
        }
    }
    pub fn is_simple(&self) -> bool {
        match self {
            Self::Default => true,
            Self::Const(_, ty) => ty.is_int() || ty.is_float(),
            Self::Or(lhs, rhs) => lhs.is_simple() && rhs.is_simple(),
            Self::Destructure(d) => d.is_simple(),
            Self::Read(_, _, _) | Self::Err => false,
            Self::EnumVariant { pattern, .. } => pattern
                .as_ref()
                .map(Box::as_ref)
                .map(|it| matches!(it,Self::Default))
                .unwrap_or(true),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedDestructure {
    Tuple(Vec<TypedPattern>),
    Struct {
        fields: HashMap<String, TypedPattern>,
    },
    Unit,
}

impl TypedDestructure {
    fn get_idents_with_types(&self) -> HashMap<String, ResolvedType> {
        match self {
            TypedDestructure::Tuple(contents) => contents
                .iter()
                .flat_map(TypedPattern::get_idents_with_types)
                .collect(),
            TypedDestructure::Struct { fields } => {
                todo!("not yet supporting field destructuring on match arms.")
            }
            TypedDestructure::Unit => HashMap::new(),
        }
    }

    fn from(
        destructure: ast::DestructurePattern,
        expected_ty: &ResolvedType,
        known_types: &HashMap<String, ResolvedTypeDeclaration>,
    ) -> Self {
        match destructure {
            ast::DestructurePattern::Struct { base_ty, fields } => todo!(
                "need to expand structure destructuring
            "
            ),
            ast::DestructurePattern::Tuple(patterns, ty, _) => {
                let mut patterns = if let ResolvedType::Tuple { underlining, .. } = expected_ty {
                    patterns
                        .into_iter()
                        .zip(underlining)
                        .map(|(pat, ty)| TypedPattern::from(pat, ty, known_types))
                        .collect()
                } else {
                    patterns
                        .into_iter()
                        .map(|pat| TypedPattern::from(pat, &types::ERROR, known_types))
                        .collect()
                };
                if &ty != expected_ty {
                    // todo! report diagnostic
                }
                Self::Tuple(patterns)
            }
            ast::DestructurePattern::Unit if expected_ty == &types::UNIT => Self::Unit,
            ast::DestructurePattern::Unit => Self::Tuple(vec![TypedPattern::Err]),
        }
    }

    fn get_binds(&self) -> HashSet<String> {
        match self {
            Self::Struct { fields } => todo!(),
            Self::Tuple(patterns) => patterns.iter().flat_map(TypedPattern::get_binds).collect(),
            Self::Unit => HashSet::new(),
        }
    }

    fn is_simple(&self) -> bool {
        match self {
            TypedDestructure::Tuple(pats) => pats.iter().all(TypedPattern::is_simple),
            TypedDestructure::Struct { fields } => todo!(),
            TypedDestructure::Unit => true,
        }
    }
}

#[derive(Error, Debug)]
#[allow(unused)]
pub enum TypingError {
    #[error("Doesn't match return")]
    ReturnTypeMismatch,
    #[error("Function isn't in scope")]
    FnNotDeclared,
    #[error("")]
    BlockTypeMismatch,
    #[error("the member must be an ident")]
    MemberMustBeIdent,
    #[error("the operation is not supported")]
    OpNotSupported, //temp.
    #[error("type is not known")]
    UnknownType,
    #[error("Type weirdness")]
    DoubleTyped,
    #[error("Type mismatched")]
    ArgTypeMismatch,
    #[error("Abi constraint violated.")]
    AbiError,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::TypedArgDeclaration;
    use super::TypedExpr;
    use crate::inference::ast;
    use crate::parser::Parser;
    use crate::typed_ast::TypedDestructure;
    use crate::typed_ast::TypedPattern;
    use crate::typed_ast::TypedTopLevelValue;
    use crate::typed_ast::{
        ResolvedGenericsDecl, ResolvedTypeDeclaration, StructDefinition, TypedBinaryOpCall,
        TypedDeclaration, TypedFnCall, TypedIfBranching, TypedIfExpr, TypedMatch, TypedMatchArm,
        TypedModuleDeclaration, TypedStatement, TypedValueDeclaration, TypedValueType,
    };
    use crate::types::{self, ResolvedType};
    use pretty_assertions::assert_eq;
    lazy_static::lazy_static! {
        static ref PREDEFINED_VALUES : HashMap<String,ResolvedType> = {
            let mut out = HashMap::new();
            out.insert("foo".to_string(), ResolvedType::Function { arg: types::INT32.into(), returns: types::INT32.into(), loc:(0,0) });
            out.insert("bar".to_string(), types::INT32);
            out
        };
    }

    #[test]
    #[ignore = "for debugging only"]
    fn debugging() {
        const SRC: &'static str = r#"
let b value : (int32,(int32,int32)) -> int32 = match value where
    | (0, (0,b) | (b,0) ) | (b,_) -> b,
    | _ -> 0,
"#;
        let parser = Parser::from_source(SRC);
        let mut module = parser.module("foo".to_string()).ast;
        module.canonialize(vec!["P".to_string()]);
        let dependency_graph = module.get_dependencies();
        let dependency_tree = dependency_graph
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_context = crate::inference::Context::new(
            dependency_tree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = inference_context.inference(module);

        let mut module = TypedModuleDeclaration::from(module, &HashMap::new(), &HashMap::new());
        module.lower_generics(&HashMap::new());
        println!("{:#?}", module)
    }

    #[test]
    fn expr_convert() {
        use crate::inference::ast::{self, Expr};
        assert_eq!(
            TypedExpr::try_from(
                Expr::NumericLiteral {
                    value: "1".to_string(),
                    ty: types::INT32,
                    id: 0
                },
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::IntegerLiteral {
                value: "1".to_string(),
                size: types::IntWidth::ThirtyTwo
            },
            "ints"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::NumericLiteral {
                    value: "1.0".to_string(),
                    ty: types::FLOAT32,
                    id: 0
                },
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::FloatLiteral {
                value: "1.0".to_string(),
                size: types::FloatWidth::ThirtyTwo
            },
            "floats"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::StringLiteral("merp".to_string()),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::StringLiteral("merp".to_string()),
            "strings"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::CharLiteral("a".to_string()),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::CharLiteral("a".to_string()),
            "chars"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::UnitLiteral,
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::UnitLiteral,
            "()"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::BinaryOpCall(ast::BinaryOpCall {
                    loc: (0, 0),
                    lhs: Expr::NumericLiteral {
                        value: "1".to_string(),
                        ty: types::INT32,
                        id: 0
                    }
                    .into(),
                    rhs: Expr::NumericLiteral {
                        value: "2".to_string(),
                        ty: types::INT32,
                        id: 0
                    }
                    .into(),
                    operator: "+".to_string(),
                    result: types::INT32,
                    id: 0
                }),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::BinaryOpCall(super::TypedBinaryOpCall {
                loc: (0, 0),
                lhs: TypedExpr::IntegerLiteral {
                    value: "1".to_string(),
                    size: types::IntWidth::ThirtyTwo
                }
                .into(),
                rhs: TypedExpr::IntegerLiteral {
                    value: "2".to_string(),
                    size: types::IntWidth::ThirtyTwo
                }
                .into(),
                operator: "+".to_string(),
                rt: types::INT32
            }),
            "1 + 2"
        );

        // TODO : Unary ops.  not yet implemented at parse level.

        assert_eq!(
            TypedExpr::try_from(
                Expr::ValueRead("bar".to_string(), (0, 0), 0),
                &HashMap::new(),
                &PREDEFINED_VALUES,
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::ValueRead("bar".to_string(), types::INT32, (0, 0)),
            "reading a value"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::FnCall(ast::FnCall {
                    loc: (0, 0),
                    value: Expr::ValueRead("foo".to_string(), (0, 0), 0).into(),
                    arg: Expr::ValueRead("bar".to_string(), (0, 0), 0).into(),
                    id: 0,
                    returns: types::INT32,
                }),
                &HashMap::new(),
                &PREDEFINED_VALUES,
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::FnCall(super::TypedFnCall {
                loc: (0, 0),
                value: TypedExpr::ValueRead(
                    "foo".to_string(),
                    ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (0, 0)
                    },
                    (0, 0)
                )
                .into(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32, (0, 0)).into()),
                arg_t: types::INT32,
                rt: types::INT32,
                is_extern: false,
            }),
            "foo bar"
        );
    }

    #[test]
    fn decl_body_convert() {
        use super::TypedValueType;
        use crate::inference::ast::{self, ValueType};
        assert_eq!(
            TypedValueType::try_from(
                ValueType::Expr(ast::Expr::NumericLiteral {
                    value: "1".to_string(),
                    ty: types::INT32,
                    id: 0
                }),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
            )
            .expect(""),
            TypedValueType::Expr(TypedExpr::IntegerLiteral {
                value: "1".to_string(),
                size: types::IntWidth::ThirtyTwo
            }),
            "expr"
        );

        assert_eq!(
            TypedValueType::try_from(
                ValueType::Function(vec![ast::Statement::Return(
                    ast::Expr::NumericLiteral {
                        value: "1".to_string(),
                        ty: types::INT32,
                        id: 0
                    },
                    (0, 0)
                )]),
                &HashMap::new(),
                &PREDEFINED_VALUES,
                &HashMap::new(),
            )
            .expect(""),
            TypedValueType::Function(vec![super::TypedStatement::Return(
                super::TypedExpr::IntegerLiteral {
                    value: "1".to_string(),
                    size: types::IntWidth::ThirtyTwo
                },
                (0, 0)
            )]),
            "function body"
        );
    }

    #[test]
    fn statement_convert() {
        use super::{TypedStatement, TypedValueDeclaration, TypedValueType};
        use crate::inference::ast::{self, Statement};
        assert_eq!(
            TypedStatement::try_from(
                Statement::Declaration(ast::ValueDeclaration {
                    loc: (0, 0),
                    is_op: false,
                    target: ast::Pattern::Read {
                        ident: "foo".to_string(),
                        loc: (0, 0),
                        ty: types::INT32,
                        id: 0
                    },
                    args: Vec::new(),
                    ty: types::INT32,
                    value: ast::ValueType::Expr(ast::Expr::NumericLiteral {
                        value: "1".to_string(),
                        ty: types::INT32,
                        id: 1
                    }),
                    generics: None,
                    abi: None,
                    id: 0
                }),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
            )
            .expect(""),
            TypedStatement::Declaration(TypedValueDeclaration {
                loc: (0, 0),
                is_op: false,
                target: TypedPattern::Read("foo".to_string(), types::INT32, (0, 0)),
                args: Vec::new(),
                value: TypedValueType::Expr(TypedExpr::IntegerLiteral {
                    value: "1".to_string(),
                    size: types::IntWidth::ThirtyTwo
                }),
                ty: types::INT32,
                generictypes: None,
                abi: None,
                is_curried: false,
            }),
            "decl statement"
        );

        assert_eq!(
            TypedStatement::try_from(
                Statement::Return(ast::Expr::ValueRead("bar".to_string(), (0, 0), 0), (0, 0)),
                &HashMap::new(),
                &PREDEFINED_VALUES,
                &HashMap::new(),
            )
            .expect(""),
            TypedStatement::Return(
                TypedExpr::ValueRead("bar".to_string(), types::INT32, (0, 0)),
                (0, 0)
            ),
            "return"
        );

        assert_eq!(
            TypedStatement::try_from(
                Statement::FnCall(ast::FnCall {
                    loc: (0, 0),
                    value: ast::Expr::ValueRead("foo".to_string(), (0, 0), 0).into(),
                    arg: ast::Expr::ValueRead("bar".to_string(), (0, 0), 0).into(),
                    returns: types::INT32,
                    id: 0
                }),
                &HashMap::new(),
                &PREDEFINED_VALUES,
                &HashMap::new(),
            )
            .expect(""),
            TypedStatement::FnCall(super::TypedFnCall {
                loc: (0, 0),
                value: TypedExpr::ValueRead(
                    "foo".to_string(),
                    ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (0, 0)
                    },
                    (0, 0)
                )
                .into(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32, (0, 0)).into()),
                arg_t: types::INT32,
                rt: types::INT32,
                is_extern: false,
            }),
            "foo bar"
        );

        // TODO: Pipe.  not implemented at parser level
    }

    #[test]
    fn decl_convert() {
        use super::TypedArgDeclaration;
        use super::TypedDeclaration;
        assert_eq!(
            TypedDeclaration::try_from(
                ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                    loc: (0, 0),
                    is_op: false,
                    ident: "test".to_string(),
                    args: vec![ast::ArgDeclaration::Simple {
                        ident: "a".to_string(),
                        loc: (0, 0),
                        ty: types::INT32,
                        id: 0
                    }],
                    ty: ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (0, 0)
                    },
                    value: ast::ValueType::Function(vec![ast::Statement::Return(
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                            ty: types::INT32,
                            id: 0
                        },
                        (0, 0)
                    )]),
                    generics: None,
                    abi: None,
                    id: 0
                }),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
            )
            .expect(""),
            TypedDeclaration::Value(super::TypedTopLevelValue {
                loc: (0, 0),
                is_op: false,
                ident: "test".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    ident: "a".to_string(),
                    loc: (0, 0),
                    ty: types::INT32,
                }],
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0)
                },
                value: super::TypedValueType::Function(vec![super::TypedStatement::Return(
                    super::TypedExpr::IntegerLiteral {
                        value: "1".to_string(),
                        size: types::IntWidth::ThirtyTwo
                    },
                    (0, 0)
                )]),
                generics: None,
                abi: None,
            }),
            r#"let test a : int32 -> int32 = 
    return 1"#
        );
        // TODO : mod and type definition.
    }

    #[test]
    fn generic_use() {
        use crate::parser::Parser;
        let parser = Parser::from_source(
            r#"
for<T> let test a : T -> T = a

let main _ : () -> () =
    let x : int32 = 3;
    test x;
"#,
            // test 3; this will be an inference error of "Unable to determine type of a NumericLiteral"
        );
        let module = parser.module("test".to_string()).ast;
        // module.canonialize(vec!["test".to_string()]);
        let dtree = module.get_dependencies();
        let dependency_tree = dtree
            .clone()
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_context = crate::inference::Context::new(
            dependency_tree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = inference_context.inference(module);

        let mut module =
            super::TypedModuleDeclaration::from(module, &HashMap::new(), &HashMap::new());
        module.declarations.sort_by_key(TypedDeclaration::get_ident);
        let [main, generic] = &module.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 11),
                is_op: false,
                ident: "test".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    loc: (1, 16),
                    ident: "a".to_string(),
                    ty: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (1, 20)
                    }
                }],
                value: TypedValueType::Expr(TypedExpr::ValueRead(
                    "a".to_string(),
                    ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (1, 20)
                    },
                    (1, 29)
                )),
                ty: ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (1, 20),
                    }
                    .into(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (1, 25)
                    }
                    .into(),
                    loc: (0, 0)
                },
                generics: Some(ResolvedGenericsDecl {
                    for_loc: (1, 0),
                    decls: vec![(
                        (1, 4),
                        ResolvedType::Generic {
                            name: "T".to_string(),
                            loc: (1, 4)
                        }
                    )],
                }),
                abi: None,
            }),
            generic,
            "generic"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (3, 4),
                is_op: false,
                ident: "main".to_string(),
                args: vec![TypedArgDeclaration::Discard {
                    loc: (3, 9),
                    ty: types::UNIT,
                }],
                value: TypedValueType::Function(vec![
                    TypedStatement::Declaration(TypedValueDeclaration {
                        loc: (4, 8),
                        is_op: false,
                        target: TypedPattern::Read("x".to_string(), types::INT32, (4, 8)),
                        args: Vec::new(),
                        value: TypedValueType::Expr(TypedExpr::IntegerLiteral {
                            value: "3".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        }),
                        ty: types::INT32,
                        generictypes: None,
                        abi: None,
                        is_curried: false,
                    }),
                    TypedStatement::FnCall(TypedFnCall {
                        loc: (5, 4),
                        value: TypedExpr::ValueRead(
                            "test".to_string(),
                            ResolvedType::Function {
                                arg: ResolvedType::Generic {
                                    name: "T".to_string(),
                                    loc: (1, 20)
                                }
                                .into(),
                                returns: ResolvedType::Generic {
                                    name: "T".to_string(),
                                    loc: (1, 25)
                                }
                                .into(),
                                loc: (0, 0),
                            },
                            (5, 4)
                        )
                        .into(),
                        arg: Some(
                            TypedExpr::ValueRead("x".to_string(), types::INT32, (5, 9)).into()
                        ),
                        rt: ResolvedType::Generic {
                            name: "T".to_string(),
                            loc: (1, 25)
                        },
                        arg_t: types::INT32,
                        is_extern: false,
                    })
                ]),
                ty: ResolvedType::Function {
                    arg: types::UNIT.into(),
                    returns: types::UNIT.into(),
                    loc: (0, 0)
                },
                generics: None,
                abi: None,
            }),
            main,
            "main"
        )
    }

    #[test]
    fn structs() {
        use crate::Parser;
        let parser = Parser::from_source(
            r"
for<T,U> type Tuple = {
    first : T,
    second : U,
}

let first a : Tuple<int32,float64> -> int32 =
    return 0
",
        );

        let module = parser.module("test".to_string()).ast;
        // module.canonialize(vec!["test".to_string()]);
        let dtree = module.get_dependencies();

        let dependency_tree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_context = crate::inference::Context::new(
            dependency_tree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = inference_context.inference(module);

        let mut module = TypedModuleDeclaration::from(module, &HashMap::new(), &HashMap::new());
        let [strct, _] = &module.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            &TypedDeclaration::TypeDefinition(crate::typed_ast::ResolvedTypeDeclaration::Struct(
                StructDefinition {
                    ident: "Tuple".to_string(),
                    generics: Some(ResolvedGenericsDecl {
                        for_loc: (1, 0),
                        decls: vec![
                            (
                                (1, 4),
                                ResolvedType::Generic {
                                    name: "T".to_string(),
                                    loc: (1, 4),
                                },
                            ),
                            (
                                (1, 6),
                                ResolvedType::Generic {
                                    name: "U".to_string(),
                                    loc: (1, 6)
                                }
                            )
                        ]
                    }),
                    fields: vec![
                        crate::ast::FieldDecl {
                            name: "first".to_string(),
                            ty: ResolvedType::Generic {
                                name: "T".to_string(),
                                loc: (2, 12),
                            },
                            loc: (2, 4)
                        },
                        crate::ast::FieldDecl {
                            name: "second".to_string(),
                            ty: ResolvedType::Generic {
                                name: "U".to_string(),
                                loc: (3, 13)
                            },
                            loc: (3, 4)
                        }
                    ],
                    loc: (1, 14)
                }
            )),
            strct,
            "pre-lowering struct",
        );

        module.lower_generics(&HashMap::new());

        let [_unlowered, fun, generated_strct] = &module.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            fun,
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (6, 4),
                is_op: false,
                ident: "first".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    loc: (6, 10),
                    ident: "a".to_string(),
                    ty: ResolvedType::User {
                        name: "Tuple".to_string(),
                        generics: vec![types::INT32, types::FLOAT64],
                        loc: (0, 0)
                    },
                }],
                value: TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::IntegerLiteral {
                        value: "0".to_string(),
                        size: types::IntWidth::ThirtyTwo
                    },
                    (7, 4)
                )]),
                ty: ResolvedType::Function {
                    arg: ResolvedType::User {
                        name: "Tuple<int32,float64>".to_string(),
                        generics: Vec::new(),
                        loc: (7, 14)
                    }
                    .into(),
                    returns: types::INT32.into(),
                    loc: (7, 36)
                },
                generics: None,
                abi: None,
            }),
            "post lowering function"
        );

        assert_eq!(
            &TypedDeclaration::TypeDefinition(ResolvedTypeDeclaration::Struct(StructDefinition {
                ident: "Tuple<int32,float64>".to_string(),
                generics: None,
                fields: vec![
                    crate::ast::FieldDecl {
                        name: "first".to_string(),
                        ty: types::INT32,
                        loc: (2, 4)
                    },
                    crate::ast::FieldDecl {
                        name: "second".to_string(),
                        ty: types::FLOAT64,
                        loc: (3, 4)
                    }
                ],
                loc: (1, 14)
            })),
            generated_strct,
            "generated"
        )
    }

    #[test]
    fn generic_lowering() {
        use crate::parser::Parser;
        use crate::TokenStream;
        let parser = Parser::from_stream(TokenStream::from_source(
            r#"
for<T> let test a : T -> T =
    return a;

let main x : int32 -> int32 =
    test x;
    return 0;
"#,
        ));
        let module = parser.module("test.fb".to_string()).ast;
        // module.canonialize(vec!["test".to_string()]);
        let dtree = module.get_dependencies();

        let dependency_tree = dtree
            .clone()
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_context = crate::inference::Context::new(
            dependency_tree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = inference_context.inference(module);

        let mut module = TypedModuleDeclaration::from(module, &HashMap::new(), &HashMap::new());
        module.lower_generics(&HashMap::new());
        let [generic, main, generated] = &module.declarations[..] else {
            unreachable!("should have three when done")
        };
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 11),
                is_op: false,
                ident: "test".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    ident: "a".to_string(),
                    loc: (1, 16),
                    ty: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (1, 20)
                    },
                }],
                value: TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::ValueRead(
                        "a".to_string(),
                        ResolvedType::Generic {
                            name: "T".to_string(),
                            loc: (1, 20)
                        },
                        (2, 11)
                    ),
                    (2, 4)
                )]),
                ty: ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (1, 20),
                    }
                    .into(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (1, 25),
                    }
                    .into(),
                    loc: (1, 23),
                },
                generics: Some(ResolvedGenericsDecl {
                    for_loc: (1, 0),
                    decls: vec![(
                        (1, 4),
                        ResolvedType::Generic {
                            name: "T".to_string(),
                            loc: (1, 4)
                        }
                    )],
                }),
                abi: None,
            }),
            generic,
            "generic should be untouched"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (4, 4),
                is_op: false,
                ident: "main".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    ident: "x".to_string(),
                    loc: (4, 9),
                    ty: types::INT32,
                }],
                value: TypedValueType::Function(vec![
                    TypedStatement::FnCall(TypedFnCall {
                        loc: (5, 4),
                        value: TypedExpr::ValueRead(
                            "test<int32>".to_string(),
                            ResolvedType::Function {
                                arg: types::INT32.into(),
                                returns: types::INT32.into(),
                                loc: (5, 4)
                            },
                            (5, 4)
                        )
                        .into(),
                        arg: Some(
                            TypedExpr::ValueRead("x".to_string(), types::INT32, (5, 9)).into()
                        ),
                        rt: types::INT32,
                        arg_t: types::INT32,
                        is_extern: false,
                    }),
                    TypedStatement::Return(
                        TypedExpr::IntegerLiteral {
                            value: "0".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        },
                        (6, 4)
                    )
                ]),
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (6, 20)
                },
                generics: None,
                abi: None,
            }),
            main,
            "main should have the value read changed"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 11),
                is_op: false,
                ident: "test<int32>".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    ident: "a".to_string(),
                    loc: (1, 16),
                    ty: types::INT32,
                }],
                value: crate::typed_ast::TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::ValueRead("a".to_string(), types::INT32, (2, 11)),
                    (2, 4)
                )]),
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0)
                },
                generics: None,
                abi: None,
            }),
            generated,
            "this should be generated"
        )
    }

    #[test]
    fn control_flow_if() {
        let parser = Parser::from_source(
            r#"
let expr_with_statement a : bool -> int32 = if a then
        foo 3;
        0
    else
        foo 4;
        1

let statement_with_else_if a b : bool -> bool -> int32 =
    if a then
        return 0;
    else if b then
        return 1;
    else
        return 2;
"#,
        );
        let module = parser.module("test.fb".to_string()).ast;
        // module.canonialize(vec!["test".to_string()]);//don't really need to do this for tests.
        let dtree = module.get_dependencies();
        let dependency_tree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let fwd_decls: HashMap<_, _> =
            [("foo".to_string(), types::INT32.fn_ty(&types::INT32))].into();
        let mut inference_context = crate::inference::Context::new(
            dependency_tree,
            fwd_decls.clone(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = inference_context.inference(module);

        let mut module = TypedModuleDeclaration::from(module, &fwd_decls, &HashMap::new());
        module.declarations.sort_by_key(TypedDeclaration::get_ident);
        let [expr, stmnt] = &module.declarations[..] else {
            unreachable!("more than two?")
        };
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "expr_with_statement".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    loc: (1, 24),
                    ident: "a".to_string(),
                    ty: types::BOOL,
                }],
                value: TypedValueType::Expr(TypedExpr::IfExpr(TypedIfExpr {
                    cond: TypedExpr::ValueRead("a".to_string(), types::BOOL, (1, 47)).into(),
                    true_branch: (
                        vec![TypedStatement::FnCall(TypedFnCall {
                            loc: (2, 8),
                            value: TypedExpr::ValueRead(
                                "foo".to_string(),
                                ResolvedType::Function {
                                    arg: types::INT32.into(),
                                    returns: types::INT32.into(),
                                    loc: (0, 0)
                                },
                                (2, 8)
                            )
                            .into(),
                            arg: Some(
                                TypedExpr::IntegerLiteral {
                                    value: "3".to_string(),
                                    size: types::IntWidth::ThirtyTwo
                                }
                                .into()
                            ),
                            rt: types::INT32,
                            arg_t: types::INT32,
                            is_extern: false,
                        })],
                        TypedExpr::IntegerLiteral {
                            value: "0".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        }
                        .into()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        vec![TypedStatement::FnCall(TypedFnCall {
                            loc: (5, 8),
                            value: TypedExpr::ValueRead(
                                "foo".to_string(),
                                ResolvedType::Function {
                                    arg: types::INT32.into(),
                                    returns: types::INT32.into(),
                                    loc: (0, 0)
                                },
                                (5, 8)
                            )
                            .into(),
                            arg: Some(
                                TypedExpr::IntegerLiteral {
                                    value: "4".to_string(),
                                    size: types::IntWidth::ThirtyTwo
                                }
                                .into()
                            ),
                            rt: types::INT32,
                            arg_t: types::INT32,
                            is_extern: false,
                        })],
                        TypedExpr::IntegerLiteral {
                            value: "1".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        }
                        .into()
                    ),
                    loc: (1, 44)
                })),
                ty: ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0)
                },
                generics: None,
                abi: None,
            }),
            expr,
            "expression if"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (8, 4),
                is_op: false,
                ident: "statement_with_else_if".to_string(),
                args: vec![
                    TypedArgDeclaration::Simple {
                        loc: (8, 27),
                        ident: "a".to_string(),
                        ty: types::BOOL,
                    },
                    TypedArgDeclaration::Simple {
                        loc: (8, 29),
                        ident: "b".to_string(),
                        ty: types::BOOL,
                    },
                ],
                ty: ResolvedType::Function {
                    arg: types::BOOL.into(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.into(),
                        returns: types::INT32.into(),
                        loc: (0, 0)
                    }
                    .into(),
                    loc: (0, 0)
                },
                value: TypedValueType::Function(vec![TypedStatement::IfBranching(
                    TypedIfBranching {
                        cond: TypedExpr::ValueRead("a".to_string(), types::BOOL, (9, 7)).into(),
                        true_branch: vec![TypedStatement::Return(
                            TypedExpr::IntegerLiteral {
                                value: "0".to_string(),
                                size: types::IntWidth::ThirtyTwo
                            },
                            (10, 8)
                        )],
                        else_ifs: vec![(
                            TypedExpr::ValueRead("b".to_string(), types::BOOL, (11, 12)).into(),
                            vec![TypedStatement::Return(
                                TypedExpr::IntegerLiteral {
                                    value: "1".to_string(),
                                    size: types::IntWidth::ThirtyTwo
                                },
                                (12, 8)
                            )]
                        )],
                        else_branch: vec![TypedStatement::Return(
                            TypedExpr::IntegerLiteral {
                                value: "2".to_string(),
                                size: types::IntWidth::ThirtyTwo
                            },
                            (14, 8)
                        )],
                        loc: (9, 4)
                    }
                )]),
                generics: None,
                abi: None,
            }),
            stmnt,
            "statement if"
        )
    }
    #[test]
    fn control_flow_match() {
        let module = Parser::from_source(
            "
let simple_expr a fun : int32 -> (int32 -> int32) -> int32 = match fun a where
| 1 -> 0,
| 2 -> 3,
| a -> a*3,

let nest_in_call a fun : int32 -> (int32 -> int32) -> int32 = fun (match a where
| 1 -> 0,
| 2 -> 3,
| _ -> a*3,
)

let as_statement a b : int32 -> int32 -> () =
    match a where
    | 1 -> 
        match b where
        | 1 -> foo 1,
        | 2 -> 
            foo 3;
        | 3 -> (),
    | 2 -> (),
",
        )
        .module("test".to_string())
        .ast;

        // module.canonialize(vec!["test".to_string()]);
        let dtree = module.get_dependencies();
        let dependency_tree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let fwd_decls: HashMap<_, _> =
            [("foo".to_string(), types::INT32.fn_ty(&types::UNIT))].into();
        let ops: HashMap<_, _> = [(
            "*".to_string(),
            vec![types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32))],
        )]
        .into();
        let mut inference_context = crate::inference::Context::new(
            dependency_tree,
            fwd_decls.clone(),
            HashMap::new(),
            ops.clone(),
            HashMap::new(),
        );
        let module = inference_context.inference(module);

        let mut module = TypedModuleDeclaration::from(module, &fwd_decls, &ops);
        module.declarations.sort_by_key(TypedDeclaration::get_ident);
        let [statement, nest_in_call, simple] = &module.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "simple_expr".to_string(),
                args: vec![
                    TypedArgDeclaration::Simple {
                        loc: (1, 16),
                        ident: "a".to_string(),
                        ty: types::INT32,
                    },
                    TypedArgDeclaration::Simple {
                        loc: (1, 18),
                        ident: "fun".to_string(),
                        ty: types::INT32.fn_ty(&types::INT32),
                    },
                ],
                value: TypedValueType::Expr(TypedExpr::Match(TypedMatch {
                    loc: (1, 61),
                    on: TypedExpr::FnCall(TypedFnCall {
                        loc: (1, 67),
                        value: TypedExpr::ValueRead(
                            "fun".to_string(),
                            ResolvedType::Function {
                                arg: types::INT32.into(),
                                returns: types::INT32.into(),
                                loc: (0, 0)
                            },
                            (1, 67)
                        )
                        .into(),
                        arg: Some(
                            TypedExpr::ValueRead("a".to_string(), types::INT32, (1, 71)).into()
                        ),
                        rt: types::INT32,
                        arg_t: types::INT32,
                        is_extern: false,
                    })
                    .into(),
                    arms: vec![
                        TypedMatchArm {
                            loc: (2, 2),
                            cond: crate::typed_ast::TypedPattern::Const(
                                "1".to_string(),
                                types::INT32
                            ),
                            block: Vec::new(),
                            ret: Some(
                                TypedExpr::IntegerLiteral {
                                    value: "0".to_string(),
                                    size: types::IntWidth::ThirtyTwo
                                }
                                .into()
                            )
                        },
                        TypedMatchArm {
                            loc: (3, 2),
                            cond: crate::typed_ast::TypedPattern::Const(
                                "2".to_string(),
                                types::INT32
                            ),
                            block: Vec::new(),
                            ret: Some(
                                TypedExpr::IntegerLiteral {
                                    value: "3".to_string(),
                                    size: types::IntWidth::ThirtyTwo
                                }
                                .into()
                            )
                        },
                        TypedMatchArm {
                            loc: (4, 2),
                            cond: crate::typed_ast::TypedPattern::Read(
                                "a".to_string(),
                                types::INT32,
                                (4, 2)
                            ),
                            block: Vec::new(),
                            ret: Some(
                                TypedExpr::BinaryOpCall(TypedBinaryOpCall {
                                    loc: (4, 8),
                                    lhs: TypedExpr::ValueRead(
                                        "a".to_string(),
                                        types::INT32,
                                        (4, 7)
                                    )
                                    .into(),
                                    rhs: TypedExpr::IntegerLiteral {
                                        value: "3".to_string(),
                                        size: types::IntWidth::ThirtyTwo
                                    }
                                    .into(),
                                    operator: "*".to_string(),
                                    rt: types::INT32
                                })
                                .into()
                            )
                        },
                    ]
                })),
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: ResolvedType::Function {
                        arg: ResolvedType::Function {
                            arg: types::INT32.into(),
                            returns: types::INT32.into(),
                            loc: (0, 0)
                        }
                        .into(),
                        returns: types::INT32.into(),
                        loc: (0, 0)
                    }
                    .into(),
                    loc: (0, 0)
                },
                generics: None,
                abi: None,
            }),
            simple,
            "simple"
        );

        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (6, 4),
                is_op: false,
                ident: "nest_in_call".to_string(),
                args: vec![
                    TypedArgDeclaration::Simple {
                        loc: (6, 17),
                        ident: "a".to_string(),
                        ty: types::INT32,
                    },
                    TypedArgDeclaration::Simple {
                        loc: (6, 19),
                        ident: "fun".to_string(),
                        ty: types::INT32.fn_ty(&types::INT32),
                    },
                ],
                value: TypedValueType::Expr(TypedExpr::FnCall(TypedFnCall {
                    loc: (6, 62),
                    value: TypedExpr::ValueRead(
                        "fun".to_string(),
                        ResolvedType::Function {
                            arg: types::INT32.into(),
                            returns: types::INT32.into(),
                            loc: (0, 0)
                        },
                        (6, 62)
                    )
                    .into(),
                    arg: Some(
                        TypedExpr::Match(TypedMatch {
                            loc: (6, 67),
                            on: TypedExpr::ValueRead("a".to_string(), types::INT32, (6, 73)).into(),
                            arms: vec![
                                TypedMatchArm {
                                    loc: (7, 2),
                                    cond: crate::typed_ast::TypedPattern::Const(
                                        "1".to_string(),
                                        types::INT32
                                    ),
                                    block: Vec::new(),
                                    ret: Some(
                                        TypedExpr::IntegerLiteral {
                                            value: "0".to_string(),
                                            size: types::IntWidth::ThirtyTwo
                                        }
                                        .into()
                                    )
                                },
                                TypedMatchArm {
                                    loc: (8, 2),
                                    cond: crate::typed_ast::TypedPattern::Const(
                                        "2".to_string(),
                                        types::INT32
                                    ),
                                    block: Vec::new(),
                                    ret: Some(
                                        TypedExpr::IntegerLiteral {
                                            value: "3".to_string(),
                                            size: types::IntWidth::ThirtyTwo
                                        }
                                        .into()
                                    )
                                },
                                TypedMatchArm {
                                    loc: (9, 2),
                                    cond: crate::typed_ast::TypedPattern::Default,
                                    block: Vec::new(),
                                    ret: Some(
                                        TypedExpr::BinaryOpCall(TypedBinaryOpCall {
                                            loc: (9, 8),
                                            lhs: TypedExpr::ValueRead(
                                                "a".to_string(),
                                                types::INT32,
                                                (9, 7)
                                            )
                                            .into(),
                                            rhs: TypedExpr::IntegerLiteral {
                                                value: "3".to_string(),
                                                size: types::IntWidth::ThirtyTwo
                                            }
                                            .into(),
                                            operator: "*".to_string(),
                                            rt: types::INT32
                                        })
                                        .into()
                                    )
                                },
                            ]
                        })
                        .into()
                    ),
                    rt: types::INT32,
                    arg_t: types::INT32,
                    is_extern: false,
                })),
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: ResolvedType::Function {
                        arg: ResolvedType::Function {
                            arg: types::INT32.into(),
                            returns: types::INT32.into(),
                            loc: (0, 0)
                        }
                        .into(),
                        returns: types::INT32.into(),
                        loc: (0, 0)
                    }
                    .into(),
                    loc: (0, 0)
                },
                generics: None,
                abi: None,
            }),
            nest_in_call,
            "nested in a call"
        );

        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (12, 4),
                is_op: false,
                ident: "as_statement".to_string(),
                args: vec![
                    TypedArgDeclaration::Simple {
                        loc: (12, 17),
                        ident: "a".to_string(),
                        ty: types::INT32,
                    },
                    TypedArgDeclaration::Simple {
                        loc: (12, 19),
                        ident: "b".to_string(),
                        ty: types::INT32,
                    },
                ],
                value: TypedValueType::Function(vec![TypedStatement::Match(TypedMatch {
                    loc: (13, 4),
                    on: TypedExpr::ValueRead("a".to_string(), types::INT32, (13, 10)).into(),
                    arms: vec![
                        TypedMatchArm {
                            loc: (14, 6),
                            cond: crate::typed_ast::TypedPattern::Const(
                                "1".to_string(),
                                types::INT32
                            ),
                            block: vec![TypedStatement::Match(TypedMatch {
                                loc: (15, 8),
                                on: TypedExpr::ValueRead("b".to_string(), types::INT32, (15, 14))
                                    .into(),
                                arms: vec![
                                    TypedMatchArm {
                                        loc: (16, 10),
                                        cond: crate::typed_ast::TypedPattern::Const(
                                            "1".to_string(),
                                            types::INT32
                                        ),
                                        block: vec![TypedStatement::Discard(
                                            TypedExpr::FnCall(TypedFnCall {
                                                loc: (16, 15),
                                                value: TypedExpr::ValueRead(
                                                    "foo".to_string(),
                                                    ResolvedType::Function {
                                                        arg: types::INT32.into(),
                                                        returns: types::UNIT.into(),
                                                        loc: (0, 0)
                                                    },
                                                    (16, 15)
                                                )
                                                .into(),
                                                arg: Some(
                                                    TypedExpr::IntegerLiteral {
                                                        value: "1".to_string(),
                                                        size: types::IntWidth::ThirtyTwo
                                                    }
                                                    .into()
                                                ),
                                                rt: types::UNIT,
                                                arg_t: types::INT32,
                                                is_extern: false,
                                            }),
                                            (16, 15)
                                        )],
                                        ret: None
                                    },
                                    TypedMatchArm {
                                        loc: (17, 10),
                                        cond: crate::typed_ast::TypedPattern::Const(
                                            "2".to_string(),
                                            types::INT32
                                        ),
                                        block: vec![TypedStatement::FnCall(TypedFnCall {
                                            loc: (18, 12),
                                            value: TypedExpr::ValueRead(
                                                "foo".to_string(),
                                                ResolvedType::Function {
                                                    arg: types::INT32.into(),
                                                    returns: types::UNIT.into(),
                                                    loc: (0, 0)
                                                },
                                                (18, 12)
                                            )
                                            .into(),
                                            arg: Some(
                                                TypedExpr::IntegerLiteral {
                                                    value: "3".to_string(),
                                                    size: types::IntWidth::ThirtyTwo
                                                }
                                                .into()
                                            ),
                                            rt: types::UNIT,
                                            arg_t: types::INT32,
                                            is_extern: false,
                                        })],
                                        ret: None
                                    },
                                    TypedMatchArm {
                                        loc: (19, 10),
                                        cond: crate::typed_ast::TypedPattern::Const(
                                            "3".to_string(),
                                            types::INT32
                                        ),
                                        block: Vec::new(),
                                        ret: None
                                    }
                                ]
                            })],
                            ret: None
                        },
                        TypedMatchArm {
                            loc: (20, 6),
                            cond: crate::typed_ast::TypedPattern::Const(
                                "2".to_string(),
                                types::INT32
                            ),
                            block: Vec::new(),
                            ret: None
                        }
                    ]
                })]),
                ty: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::UNIT.into(),
                        loc: (0, 0)
                    }
                    .into(),
                    loc: (0, 0)
                },
                generics: None,
                abi: None,
            }),
            statement,
            "in a statement and nested in itself"
        )
    }
    #[test]
    fn arrays() {
        let module = Parser::from_source(
            "
let simple _ : () -> [int32;5] = [5,4,3,2,1]

let should_fail _ : () -> [int32;5] = [1,2,3,4]

let not_so_simple a : int32 -> [int32;4] =
    return [
        a,
        bar,
        if a == 0 then 3 else 4,
        foo a
    ];
",
        )
        .module("foo".to_string())
        .ast;
        let dtree = module.get_dependencies();
        let dependency_tree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();

        let mut inference_context = crate::inference::Context::new(
            dependency_tree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let module = inference_context.inference(module);

        let mut module = TypedModuleDeclaration::from(module, &HashMap::new(), &HashMap::new());
        module.declarations.sort_by_key(TypedDeclaration::get_ident);

        let [_not_so_simple, should_fail, simple] = &module.declarations[..] else {
            unreachable!("not three declarations?")
        };

        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "simple".to_string(),
                args: vec![TypedArgDeclaration::Discard {
                    loc: (1, 11),
                    ty: types::UNIT
                }],
                value: TypedValueType::Expr(TypedExpr::ArrayLiteral {
                    contents: vec![
                        TypedExpr::IntegerLiteral {
                            value: "5".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        },
                        TypedExpr::IntegerLiteral {
                            value: "4".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        },
                        TypedExpr::IntegerLiteral {
                            value: "3".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        },
                        TypedExpr::IntegerLiteral {
                            value: "2".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        },
                        TypedExpr::IntegerLiteral {
                            value: "1".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        },
                    ],
                    underlining: types::INT32,
                }),
                ty: ResolvedType::Function {
                    arg: types::UNIT.into(),
                    returns: ResolvedType::Array {
                        underlining: types::INT32.into(),
                        size: 5
                    }
                    .into(),
                    loc: (0, 0)
                },
                generics: None,
                abi: None,
            }),
            simple,
            "simple"
        );
        println!("{should_fail:?}");

        const USAGE: &'static str = "
let f (a:[int32;5]) = ();

let main _ : () -> () =
    f [1,2,3,4,5];
    return ();
";

        let ast = crate::Parser::from_source(USAGE).module(String::new()).ast;
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_ctx = crate::inference::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let ast = inference_ctx.inference(ast);
        let mut ast = TypedModuleDeclaration::from(ast, &HashMap::new(), &HashMap::new());
        ast.declarations.sort_by_key(|decl| decl.get_ident());
        let [f, main] = &ast.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "f".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    loc: (1, 7),
                    ident: "a".to_string(),
                    ty: ResolvedType::Array {
                        underlining: types::INT32.into(),
                        size: 5
                    },
                },],
                ty: ResolvedType::Array {
                    underlining: types::INT32.into(),
                    size: 5
                }
                .fn_ty(&types::UNIT),
                value: TypedValueType::Expr(TypedExpr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            f,
            "the function"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (3, 4),
                is_op: false,
                ident: "main".to_string(),
                args: vec![TypedArgDeclaration::Discard {
                    loc: (3, 9),
                    ty: types::UNIT,
                },],
                ty: types::UNIT.fn_ty(&types::UNIT),
                value: TypedValueType::Function(vec![
                    TypedStatement::FnCall(TypedFnCall {
                        loc: (4, 4),
                        value: TypedExpr::ValueRead(
                            "f".to_string(),
                            ResolvedType::Array {
                                underlining: types::INT32.into(),
                                size: 5
                            }
                            .fn_ty(&types::UNIT),
                            (4, 4)
                        )
                        .into(),
                        arg: Some(
                            TypedExpr::ArrayLiteral {
                                contents: vec![
                                    TypedExpr::IntegerLiteral {
                                        value: "1".to_string(),
                                        size: types::IntWidth::ThirtyTwo
                                    },
                                    TypedExpr::IntegerLiteral {
                                        value: "2".to_string(),
                                        size: types::IntWidth::ThirtyTwo
                                    },
                                    TypedExpr::IntegerLiteral {
                                        value: "3".to_string(),
                                        size: types::IntWidth::ThirtyTwo
                                    },
                                    TypedExpr::IntegerLiteral {
                                        value: "4".to_string(),
                                        size: types::IntWidth::ThirtyTwo
                                    },
                                    TypedExpr::IntegerLiteral {
                                        value: "5".to_string(),
                                        size: types::IntWidth::ThirtyTwo
                                    },
                                ],
                                underlining: types::INT32,
                            }
                            .into()
                        ),
                        rt: types::UNIT,
                        arg_t: ResolvedType::Array {
                            underlining: types::INT32.into(),
                            size: 5
                        },
                        is_extern: false,
                    }),
                    TypedStatement::Return(TypedExpr::UnitLiteral, (5, 4))
                ]),
                generics: None,
                abi: None,
            }),
            main,
            "main"
        );
    }
    #[test]
    fn destructuring_statement() {
        let ast = Parser::from_source(
            "
let a (v:(int32,int32)) =
    let (x,y) = v;
    return ();",
        )
        .module("".to_string())
        .ast;
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_ctx = crate::inference::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let ast = inference_ctx.inference(ast);
        let mut ast = TypedModuleDeclaration::from(ast, &HashMap::new(), &HashMap::new());
        let [a] = &ast.declarations[..] else {
            unreachable!()
        };
        assert_eq!(
            &TypedDeclaration::Value(TypedTopLevelValue {
                loc: (1, 4),
                is_op: false,
                ident: "a".to_string(),
                args: vec![TypedArgDeclaration::Simple {
                    ident: "v".to_string(),
                    ty: ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32,],
                        loc: (0, 0)
                    },
                    loc: (1, 7)
                }],
                ty: ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32,],
                    loc: (0, 0)
                }
                .fn_ty(&types::UNIT),
                value: TypedValueType::Function(vec![
                    TypedStatement::Declaration(TypedValueDeclaration {
                        loc: (2, 8),
                        is_op: false,
                        is_curried: false,
                        target: TypedPattern::Destructure(TypedDestructure::Tuple(vec![
                            TypedPattern::Read("x".to_string(), types::INT32, (2, 9)),
                            TypedPattern::Read("y".to_string(), types::INT32, (2, 11)),
                        ])),
                        args: Vec::new(),
                        value: TypedValueType::Expr(TypedExpr::ValueRead(
                            "v".to_string(),
                            ResolvedType::Tuple {
                                underlining: vec![types::INT32, types::INT32,],
                                loc: (0, 0)
                            },
                            (2, 16)
                        )),
                        ty: ResolvedType::Tuple {
                            underlining: vec![types::INT32, types::INT32,],
                            loc: (0, 0)
                        },
                        generictypes: None,
                        abi: None,
                    }),
                    TypedStatement::Return(TypedExpr::UnitLiteral, (3, 4))
                ]),
                generics: None,
                abi: None,
            }),
            a
        )
    }
    #[test]
    fn enum_variants() {
        const SRC: &'static str = r#"
enum Test = | A (int8,int8) | B
let fun a = match a where
| Test::A ((a,0) | (0,a)) -> a,
| Test::A (a,b) -> 1,
| Test::B -> 0,
"#;
        let ast = Parser::from_source(SRC).module("".to_string()).ast;
        let dtree = ast.get_dependencies();
        let dtree = dtree
            .into_iter()
            .map(|(key, value)| (key, value.into_iter().collect()))
            .collect();
        let mut inference_ctx = crate::inference::Context::new(
            dtree,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
        let ast = inference_ctx.inference(ast);

        println!("{:#?}", ast.decls[1]);
        let ast = TypedModuleDeclaration::from(ast, &HashMap::new(), &HashMap::new());
        let [ty, fun] = &ast.declarations[..] else {
            unreachable!()
        };

        assert_eq!(
            ty,
            &super::TypedDeclaration::TypeDefinition(super::ResolvedTypeDeclaration::Enum(
                super::TypedEnumDeclaration {
                    ident: "Test".into(),
                    generics: None,
                    values: vec![
                        crate::ast::EnumVariant::Tuple {
                            ident: "A".into(),
                            ty: ResolvedType::Tuple {
                                underlining: vec![types::INT8; 2],
                                loc: (0, 0)
                            },
                            loc: (1, 14)
                        },
                        crate::ast::EnumVariant::Unit {
                            ident: "B".into(),
                            loc: (1, 30)
                        }
                    ],
                    loc: (1, 5),
                }
            ))
        );

        assert_eq!(
            fun,
            &super::TypedDeclaration::Value(super::TypedTopLevelValue {
                loc: (2, 4),
                is_op: false,
                ident: "fun".into(),
                args: vec![TypedArgDeclaration::Simple {
                    loc: (2, 8),
                    ident: "a".into(),
                    ty: ResolvedType::User {
                        name: "Test".into(),
                        generics: Vec::new(),
                        loc: (1, 5)
                    },
                }],
                ty: ResolvedType::User {
                    name: "Test".into(),
                    generics: Vec::new(),
                    loc: (1, 5)
                }
                .fn_ty(&types::INT8),
                value: TypedValueType::Expr(super::TypedExpr::Match(super::TypedMatch {
                    loc: (2, 12),
                    on: super::TypedExpr::ValueRead(
                        "a".into(),
                        ResolvedType::User {
                            name: "Test".into(),
                            generics: Vec::new(),
                            loc: (1, 5)
                        },
                        (2, 18)
                    )
                    .into(),
                    arms: vec![
                        super::TypedMatchArm {
                            loc: (3, 2),
                            cond: TypedPattern::EnumVariant {
                                ty: ResolvedType::Dependent {
                                    base: ResolvedType::User {
                                        name: "Test".into(),
                                        generics: Vec::new(),
                                        loc: (1, 5)
                                    }
                                    .into(),
                                    actual: ResolvedType::Tuple {
                                        underlining: vec![types::INT8, types::INT8],
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    generics: Vec::new(),
                                    ident: "Test::A".into(),
                                    loc: (0, 0)
                                },
                                variant: "Test::A".into(),
                                pattern: Some(
                                    TypedPattern::Or(
                                        TypedPattern::Destructure(TypedDestructure::Tuple(vec![
                                            TypedPattern::Read("a".into(), types::INT8, (3, 12)),
                                            TypedPattern::Const("0".into(), types::INT8),
                                        ]))
                                        .into(),
                                        TypedPattern::Destructure(TypedDestructure::Tuple(vec![
                                            TypedPattern::Const("0".into(), types::INT8),
                                            TypedPattern::Read("a".into(), types::INT8, (3, 22)),
                                        ]))
                                        .into()
                                    )
                                    .into()
                                ),
                                loc: (3, 2)
                            },
                            block: Vec::new(),
                            ret: Some(
                                super::TypedExpr::ValueRead("a".into(), types::INT8, (3, 29))
                                    .into()
                            )
                        },
                        super::TypedMatchArm {
                            loc: (4, 2),
                            cond: TypedPattern::EnumVariant {
                                ty: ResolvedType::Dependent {
                                    base: ResolvedType::User {
                                        name: "Test".into(),
                                        generics: Vec::new(),
                                        loc: (1, 5)
                                    }
                                    .into(),
                                    actual: ResolvedType::Tuple {
                                        underlining: vec![types::INT8, types::INT8],
                                        loc: (0, 0)
                                    }
                                    .into(),
                                    generics: Vec::new(),
                                    ident: "Test::A".into(),
                                    loc: (0, 0)
                                },
                                variant: "Test::A".into(),
                                pattern: Some(
                                    super::TypedPattern::Destructure(TypedDestructure::Tuple(vec![
                                        TypedPattern::Read("a".into(), types::INT8, (4, 11)),
                                        TypedPattern::Read("b".into(), types::INT8, (4, 13)),
                                    ])).into()
                                ),
                                loc: (4, 2)
                            },
                            block: Vec::new(),
                            ret: Some(super::TypedExpr::IntegerLiteral { value: "1".into(), size: types::IntWidth::Eight }.into())
                        },
                        super::TypedMatchArm {
                            loc:(5,2),
                            cond: TypedPattern::EnumVariant { 
                                ty: ResolvedType::Dependent {
                                    base: ResolvedType::User {
                                        name: "Test".into(),
                                        generics: Vec::new(),
                                        loc: (1, 5)
                                    }
                                    .into(),
                                    actual: ResolvedType::Void.into(),
                                    generics: Vec::new(),
                                    ident: "Test::B".into(),
                                    loc: (0, 0)
                                }, variant: "Test::B".into(), 
                                pattern: None, 
                                loc: (5,2) 
                                
                            },
                            block:Vec::new(),
                            ret: Some(super::TypedExpr::IntegerLiteral { value: "0".into(), size: types::IntWidth::Eight }.into())
                        }
                    ]
                })),
                generics: None,
                abi: None
            })
        );
    }
}

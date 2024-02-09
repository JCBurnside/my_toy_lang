use itertools::Itertools;
use std::{collections::HashMap, num::NonZeroU8};

use crate::{
    ast::{self, ArgDeclation, Declaration, Expr},
    types::{self, FloatWidth, IntWidth, ResolvedType},
    util::ExtraUtilFunctions,
};

pub type FileTyped = TypedModuleDeclaration;
pub type ProgramTyped = Vec<FileTyped>;
#[derive(Debug, PartialEq, Clone)]
pub struct TypedModuleDeclaration {
    pub(crate) loc: Option<crate::Location>,
    pub(crate) name: String,
    pub(crate) declarations: Vec<TypedDeclaration>,
}

impl TypedModuleDeclaration {
    pub(crate) fn from(
        module: ast::ModuleDeclaration,
        fwd_declares: &HashMap<String, ResolvedType>,
    ) -> Self {
        let ast::ModuleDeclaration {
            loc,
            name,
            declarations,
        } = module;
        let mut fwd_declares = fwd_declares.clone();
        fwd_declares.extend(declarations.iter().filter_map(|it| {
            match it {
                Declaration::Value(v) => Some((v.ident.clone(), v.ty.clone().unwrap())),
                Declaration::TypeDefinition(decl) => match decl {
                    ast::TypeDefinition::Alias(name, value) => Some((name.clone(), value.clone())),
                    ast::TypeDefinition::Enum(_) => todo!(),
                    ast::TypeDefinition::Struct(strct) => Some((
                        strct.ident.clone(),
                        ResolvedType::User {
                            name: strct.ident.clone(),
                            generics: strct
                                .generics
                                .clone()
                                .map(ResolvedGenericsDecl::from)
                                .map(|g| g.decls.into_iter().map(|(_,ty)| ty).collect())
                                .unwrap_or_else(Vec::new),
                            loc:strct.loc
                        },
                    )),
                },
                _ => None,
            }
        }));

        let types: HashMap<String, ast::TypeDefinition> = declarations
            .iter()
            .filter_map::<(String, ast::TypeDefinition), _>(|it| match it {
                ast::Declaration::TypeDefinition(def) => Some((def.get_ident(), def.clone())),
                _ => None,
            })
            .collect();

        Self {
            loc,
            name,
            declarations: declarations
                .into_iter()
                .map(|decl| TypedDeclaration::try_from(decl, &fwd_declares, &types))
                .filter_map(|decl| match decl {
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
    }
}

#[derive(Debug,PartialEq,Clone)]
pub struct ResolvedGenericsDecl {
    for_loc : crate::Location,
    decls : Vec<(crate::Location,ResolvedType)>
}

impl ResolvedGenericsDecl {
    fn from(
        other : ast::GenericsDecl
    ) -> Self {
        let ast::GenericsDecl { for_loc, decls} = other;
        Self {
            for_loc,
            decls: decls.into_iter().map(|(loc,name)| {
                (
                    loc,
                    ResolvedType::Generic { name, loc }
                )
            }).collect()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedDeclaration {
    Mod(TypedModuleDeclaration),
    Value(TypedValueDeclaration),
    TypeDefinition(ResolvedTypeDeclaration),
}

impl TypedDeclaration {
    fn get_ident(&self) -> String {
        match self {
            Self::Mod(module) => module.name.clone(),
            Self::Value(v) => v.ident.clone(),
            Self::TypeDefinition(decl) => decl.get_ident(),
        }
    }

    pub(crate) fn try_from(
        data: ast::Declaration,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Result<Self, TypingError> {
        match data {
            Declaration::Mod(module) => Ok(Self::Mod(TypedModuleDeclaration::from(
                module,
                known_values,
            ))),
            Declaration::Value(decl) => Ok(Self::Value(TypedValueDeclaration::try_from(
                decl,
                known_values,
                known_types,
            )?)),
            Declaration::TypeDefinition(define) => Ok(Self::TypeDefinition(
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
            TypedDeclaration::Mod(_) => unreachable!(),
            TypedDeclaration::Value(value) => value.replace_types(types),
            TypedDeclaration::TypeDefinition(strct) => strct.replace_types(types, context),
        };
    }

    pub(crate) fn get_generics(&self) -> Vec<String> {
        match self {
            TypedDeclaration::Mod(_) => Vec::new(),
            TypedDeclaration::Value(v) => v.generictypes.as_ref().map(|g| g.decls.iter().map(|(_,it)|{
                let ResolvedType::Generic { name, .. } = it else {unreachable!()};
                name.clone()
            }).collect()).unwrap_or_else(Vec::new),
            TypedDeclaration::TypeDefinition(define) => define.get_generics(),
        }
    }

    pub(crate) fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(value) => value.lower_generics(context),
            TypedDeclaration::TypeDefinition(def) => def.lower_generics(context),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResolvedTypeDeclaration {
    // Alias(String, ResolvedType),
    // Enum(String, Vec<TypedEnumVariant>, crate::Location),
    Struct(StructDefinition),
}
impl ResolvedTypeDeclaration {
    fn try_from(
        origin: ast::TypeDefinition,
        known_types: &HashMap<String, ResolvedType>,
    ) -> Result<Self, TypingError> {
        match origin {
            // ast::TypeDefinition::Alias(new, old) => Ok(Self::Alias(new, old)),
            ast::TypeDefinition::Enum(_) => todo!(),
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
            ResolvedTypeDeclaration::Struct(stct) => stct.lower_generics(context),
        }
    }

    fn get_ident(&self) -> String {
        match self {
            ResolvedTypeDeclaration::Struct(strct) => strct.ident.clone(),
        }
    }

    fn get_generics(&self) -> Vec<String> {
        match self {
            ResolvedTypeDeclaration::Struct(strct) => strct.generics
            .as_ref()
            .map(|g| 
                g.decls.iter()
                .map(|(_,name)|{
                    let ResolvedType::Generic { name, .. } = name else { unreachable!() };
                    name.clone()
                }
                )
                .collect()
            )
            .unwrap_or_else(Vec::new),
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)], context: &mut LoweringContext) {
        match self {
            ResolvedTypeDeclaration::Struct(strct) => strct.replace_types(types, context),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDefinition {
    pub(crate) ident: String,
    pub(crate) generics: Option<ResolvedGenericsDecl>,
    pub(crate) fields: Vec<ast::FieldDecl>,
    pub(crate) loc: crate::Location,
}

impl StructDefinition {
    pub(crate) fn try_from(
        data: ast::StructDefinition,
        _known_types: &HashMap<String, ResolvedType>,
    ) -> Result<Self, TypingError> {
        let ast::StructDefinition {
            ident,
            generics,
            mut values,
            loc,
        } = data;
        if let Some(generics) = &generics {
            for (_,generic) in &generics.decls {
                values.iter_mut().for_each(|field| {
                    field.ty = field.ty.clone().replace_user_with_generic(&generic);
                })
            }
        }

        Ok(Self {
            ident,
            generics:generics.map(ResolvedGenericsDecl::from),
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
        
        let generics = 
            self.generics.as_ref().unwrap().decls
            .iter()
            .cloned()
            .filter(|(_,it)| match it {
                ResolvedType::Generic { name, .. }  => !types.iter().map(|(n,_)| n).contains(name),
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
    pub(crate) loc: crate::Location,
    pub(crate) is_op: bool,
    pub(crate) ident: String,
    pub(crate) args: Vec<ArgDeclation>,
    pub(crate) value: TypedValueType,
    pub(crate) ty: ResolvedType,
    pub(crate) generictypes: Option<ResolvedGenericsDecl>,
    pub(crate) is_curried: bool,
}
pub(crate) fn collect_args(t: &ResolvedType) -> Vec<ResolvedType> {
    if let ResolvedType::Function { arg, returns } = t {
        [arg.as_ref().clone()]
            .into_iter()
            .chain(collect_args(&returns).into_iter())
            .collect()
    } else {
        vec![]
    }
}
impl TypedValueDeclaration {
    fn replace_types(&mut self, replaced: &[(String, ResolvedType)]) {
        self.ty = replaced.iter().fold(self.ty.clone(), |ty, (name, new_ty)| {
            ty.replace_generic(name, new_ty.clone())
        });
        match &mut self.value {
            TypedValueType::Expr(expr) => expr.replace_types(replaced),
            TypedValueType::Function(stmnts) => stmnts
                .into_iter()
                .for_each(|expr| expr.replace_types(&replaced)),
            TypedValueType::Err => (),
        }
    }

    pub(crate) fn try_from(
        data: ast::ValueDeclaration,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Result<Self, TypingError> {
        let ast::ValueDeclaration {
            loc,
            is_op,
            ident,
            args,
            ty,
            value,
            generictypes,
        } = data;
        let Some(ty) = ty else { todo!("type inference") };
        let mut known_values = known_values.clone();
        known_values.extend(
            args.iter()
                .map(|it| it.ident.clone())
                .zip(collect_args(&ty).into_iter()),
        );
        let value = match TypedValueType::try_from(value, &known_values, known_types) {
            Ok(value) => value,
            Err(e) => {
                println!("{:?}", e);
                TypedValueType::Err
            }
        };
        let is_curried = if let ResolvedType::Function { .. } = &ty {
            args.len() < collect_args(&ty).len()
        } else {
            false
        };
        Ok(Self {
            loc,
            is_op,
            ident,
            args,
            is_curried,
            ty: if is_curried { value.get_ty() } else { ty },
            value,
            generictypes : generictypes.map(ResolvedGenericsDecl::from),
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
            TypedValueType::Err => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedValueType {
    Expr(TypedExpr),
    Function(Vec<TypedStatement>),
    Err,
}

impl TypedValueType {
    pub(crate) fn try_from(
        data: ast::ValueType,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Result<Self, TypingError> {
        match data {
            ast::ValueType::Expr(expr) => {
                TypedExpr::try_from(expr, known_values, known_types, Vec::new())
                    .map(|expr| Self::Expr(expr))
            }
            ast::ValueType::Function(stmnts) => {
                let mut output = Vec::with_capacity(stmnts.len());
                let mut known_values = known_values.clone();
                for stmnt in stmnts {
                    match TypedStatement::try_from(stmnt, &known_values, known_types) {
                        Ok(stmnt) => {
                            if let TypedStatement::Declaration(data) = &stmnt {
                                let _ = known_values
                                    .entry(data.ident.clone())
                                    .and_modify(|it| {
                                        *it = data.ty.clone();
                                    })
                                    .or_insert(data.ty.clone());
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
        }
    }

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
            Self::Err => todo!(),
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
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Result<Self, TypingError> {
        match statement {
            ast::Statement::Declaration(data) => Ok(
                match TypedValueDeclaration::try_from(data, known_values, known_types) {
                    Ok(d) => Self::Declaration(d),
                    Err(e) => {
                        println!("{:?}", e);
                        Self::Error
                    }
                },
            ),
            ast::Statement::Return(value, loc) => Ok(Self::Return(
                TypedExpr::try_from(value, known_values, known_types, Vec::new())?,
                loc,
            )),
            ast::Statement::FnCall(data) => Ok(Self::FnCall(TypedFnCall::try_from(
                data,
                known_values,
                known_types,
            )?)),
            ast::Statement::Pipe(_) => todo!(),
            ast::Statement::IfStatement(ifstmnt) => Ok(Self::IfBranching(
                TypedIfBranching::try_from(ifstmnt, known_values, known_types),
            )),
            ast::Statement::Match(match_) => Ok(Self::Match(TypedMatch::as_statement(
                match_,
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
                TypedValueType::Err => (),
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
    pub(crate) cond: Box<TypedExpr>,
    pub(crate) true_branch: Vec<TypedStatement>,
    pub(crate) else_ifs: Vec<(Box<TypedExpr>, Vec<TypedStatement>)>,
    pub(crate) else_branch: Vec<TypedStatement>,
    pub(crate) loc: crate::Location,
}

impl TypedIfBranching {
    fn try_from(
        value: ast::IfBranching,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Self {
        let ast::IfBranching {
            cond,
            true_branch,
            else_ifs,
            else_branch,
            loc,
        } = value;
        let cond = match TypedExpr::try_from(*cond, known_values, known_types, Vec::new()) {
            Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
            Ok(cond) => {
                let loc = cond.get_loc();
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
        .boxed();

        let true_branch = {
            let mut output = Vec::with_capacity(true_branch.len());
            let mut known_values = known_values.clone();
            for stmnt in true_branch {
                match TypedStatement::try_from(stmnt, &known_values, known_types) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            let _ = known_values
                                .entry(data.ident.clone())
                                .and_modify(|it| {
                                    *it = data.ty.clone();
                                })
                                .or_insert(data.ty.clone());
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
                let cond = match TypedExpr::try_from(*cond, known_values, known_types, Vec::new()) {
                    Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
                    Ok(cond) => {
                        let loc = cond.get_loc();
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
                        match TypedStatement::try_from(stmnt, &block_known_values, known_types) {
                            Ok(stmnt) => {
                                if let TypedStatement::Declaration(data) = &stmnt {
                                    let _ = block_known_values
                                        .entry(data.ident.clone())
                                        .and_modify(|it| {
                                            *it = data.ty.clone();
                                        })
                                        .or_insert(data.ty.clone());
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
                (cond.boxed(), block)
            })
            .collect();

        let else_branch = {
            let mut else_block_known_values = known_values.clone();
            let mut block = Vec::new();
            for stmnt in else_branch {
                match TypedStatement::try_from(stmnt, &else_block_known_values, known_types) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            let _ = else_block_known_values
                                .entry(data.ident.clone())
                                .and_modify(|it| {
                                    *it = data.ty.clone();
                                })
                                .or_insert(data.ty.clone());
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
    pub(crate) expansion: NonZeroU8,
    pub(crate) lhs: Box<TypedExpr>,
    /// THIS HAS A RESTRICTION OF MUST RETURN A FUNCTION
    pub(crate) rhs: Box<TypedExpr>,
    /// should match the final return type as [`TypedPipe::rhs`]
    pub(crate) rt: ResolvedType,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedFnCall {
    pub(crate) loc: crate::Location,
    pub(crate) value: Box<TypedExpr>,
    pub(crate) arg: Option<Box<TypedExpr>>,
    pub(crate) rt: ResolvedType,
    pub(crate) arg_t: ResolvedType,
}

impl TypedFnCall {
    pub(crate) fn try_from(
        data: ast::FnCall,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Result<Self, TypingError> {
        let ast::FnCall { loc, value, arg } = data;
        let value = ok_or_err_node(TypedExpr::try_from(
            *value,
            known_values,
            known_types,
            Vec::new(),
        ));

        let arg =
            arg.map(
                |arg| match TypedExpr::try_from(*arg, known_values, known_types, Vec::new()) {
                    Ok(arg) => arg,
                    Err(e) => {
                        println!("{:?}", e);
                        TypedExpr::ErrorNode
                    }
                },
            );

        if value != TypedExpr::ErrorNode {
            let arg_t = arg.as_ref().map(|arg| arg.get_ty());
            let ty = strip_pointers(&value.get_ty());
            let ResolvedType::Function { arg, .. } = ty else { return Err(TypingError::FnNotDeclared) };
            if !arg.is_generic() && Some(*arg) != arg_t {
                return Err(TypingError::ArgTypeMismatch);
            }
        }
        let rt = if value == TypedExpr::ErrorNode {
            ResolvedType::Error
        } else {
            match value.get_ty() {
                ResolvedType::Function { returns, .. } => *returns,
                ResolvedType::Pointer { underlining } if underlining.is_function() => {
                    let ResolvedType::Function { returns, .. } = *underlining else { unreachable!() };
                    *returns
                }
                _ => ResolvedType::Error,
            }
        };
        Ok(Self {
            loc,
            value: value.boxed(),
            arg_t: arg.as_ref().map(|arg| arg.get_ty()).unwrap_or(types::UNIT),
            arg: arg.map(Box::new),
            rt,
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
        let returns = match value.get_ty() {
            ResolvedType::Function { returns, .. } => returns,
            _ => unreachable!(),
        };
        *rt = *returns;
        rt.lower_generics(context);
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedStructConstruction {
    pub(crate) loc: crate::Location,
    pub(crate) fields: HashMap<String, (TypedExpr, crate::Location)>,
    pub(crate) generics: Vec<ResolvedType>,
    pub(crate) ident: String,
}

impl TypedStructConstruction {
    fn from(
        data: ast::StructConstruction,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Result<Self, TypingError> {
        let ast::StructConstruction {
            loc,
            fields,
            generics,
            ident,
        } = data;
        let mut new_fields = HashMap::new();
        let declaration = match known_types.get(&ident) {
            Some(ast::TypeDefinition::Struct(decl)) => decl,
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
            let field = match TypedExpr::try_from(field, known_values, known_types, Vec::new()) {
                Ok(expr) => expr,
                Err(e) => {
                    println!("{:?}", e);
                    TypedExpr::ErrorNode
                }
            };
            if field.get_ty() != ResolvedType::Error {
                if let Some(old_field) = declaration.values.iter().find(|it| it.name == name) {
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
    /// NOT IMPLEMENTED YET
    /// defined like [ expr, expr, expr, ... ]
    /// type [T;N]
    ArrayLiteral {
        contents: Vec<TypedExpr>,
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
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
        already_processed_args: Vec<ResolvedType>,
    ) -> Result<Self, TypingError> {
        match value {
            Expr::NumericLiteral { value, ty } if matches!(ty, ResolvedType::Int { .. }) => {
                let ResolvedType::Int { width, ..} = ty else { unreachable!() };
                Ok(Self::IntegerLiteral { value, size: width })
            }
            Expr::NumericLiteral { value, ty } if matches!(ty, ResolvedType::Float { .. }) => {
                let ResolvedType::Float { width} = ty else { unreachable!() };

                Ok(Self::FloatLiteral { value, size: width })
            }
            Expr::NumericLiteral { .. } => unreachable!(),
            Expr::StringLiteral(value) => Ok(Self::StringLiteral(value)),
            Expr::CharLiteral(value) => Ok(Self::CharLiteral(value)),
            Expr::UnitLiteral => Ok(Self::UnitLiteral),
            Expr::Compose { .. } => todo!(),
            Expr::BinaryOpCall(data) if data.operator == "." => Ok(Self::MemeberRead(
                TypedMemberRead::try_from(data, known_values, known_types, already_processed_args)?,
            )),
            Expr::BinaryOpCall(data) => Ok(Self::BinaryOpCall(TypedBinaryOpCall::try_from(
                data,
                known_values,
                known_types,
            )?)),
            Expr::UnaryOpCall(_) => todo!(),
            Expr::FnCall(data) => Ok(Self::FnCall(TypedFnCall::try_from(
                data,
                known_values,
                known_types,
            )?)),
            Expr::ValueRead(value, loc) => {
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
                    .map(|value| TypedExpr::try_from(value, known_values, known_types, Vec::new()))
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
                    Ok(Self::ArrayLiteral { contents })
                }
            }
            #[allow(unused)]
            Expr::ListLiteral { contents, loc } => todo!(),
            #[allow(unused)]
            Expr::TupleLiteral { contents, loc } => todo!(),
            Expr::StructConstruction(strct) => Ok(Self::StructConstruction(
                TypedStructConstruction::from(strct, known_values, known_types)?,
            )),
            Expr::If(ifexpr) => Ok(Self::IfExpr(TypedIfExpr::from(
                ifexpr,
                known_values,
                known_types,
            ))),
            Expr::BoolLiteral(value, loc) => Ok(Self::BoolLiteral(value, loc)),
            Expr::Match(match_) => Ok(Self::Match(TypedMatch::from(
                match_,
                known_values,
                known_types,
            ))),
            Expr::Error => Ok(Self::ErrorNode),
        }
    }

    pub(crate) fn get_loc(&self) -> crate::Location {
        match self {
            Self::IntegerLiteral { value:_, size: _ } => todo!(),
            Self::FloatLiteral { value: _, size:_ } => todo!(),
            Self::StringLiteral(_) => todo!(),
            Self::CharLiteral(_) => todo!(),
            Self::UnitLiteral => todo!(),
            Self::BinaryOpCall(bin) => bin.loc,
            Self::MemeberRead(read) => read.loc,
            Self::UnaryOpCall(_) => todo!(),
            Self::FnCall(call) => call.loc,
            Self::BoolLiteral(_, loc) | Self::ValueRead(_, _, loc) => *loc,
            Self::ArrayLiteral { contents: _ } => todo!(),
            Self::ListLiteral { contents: _ } => todo!(),
            Self::TupleLiteral { contents: _ } => todo!(),
            Self::StructConstruction(con) => con.loc,
            Self::IfExpr(ifexpr) => ifexpr.loc,
            Self::Match(match_) => match_.loc,
            Self::ErrorNode => todo!(),
        }
    }

    pub(crate) fn get_ty(&self) -> ResolvedType {
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
            Self::ArrayLiteral { contents } => ResolvedType::Array {
                underlining: contents.first().unwrap().get_ty().boxed(),
                size: contents.len(),
            },
            Self::ListLiteral { .. } | Self::TupleLiteral { .. } => todo!(),
            Self::StructConstruction(strct) => ResolvedType::User {
                name: strct.ident.clone(),
                generics: strct.generics.clone(),
                loc:strct.loc,
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
            Self::ArrayLiteral { contents } => contents
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty)),
            Self::ListLiteral { contents } => contents
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty)),
            Self::TupleLiteral { contents } => contents
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
            Self::ArrayLiteral { contents }
            | Self::ListLiteral { contents }
            | Self::TupleLiteral { contents } => {
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
    pub(crate) cond: Box<TypedExpr>,
    pub(crate) true_branch: (Vec<TypedStatement>, Box<TypedExpr>),
    pub(crate) else_ifs: Vec<(Box<TypedExpr>, Vec<TypedStatement>, Box<TypedExpr>)>,
    pub(crate) else_branch: (Vec<TypedStatement>, Box<TypedExpr>),
    pub(crate) loc: crate::Location,
}

impl TypedIfExpr {
    pub(crate) fn from(
        value: ast::IfExpr,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Self {
        let ast::IfExpr {
            cond,
            true_branch,
            else_ifs,
            else_branch,
            loc,
        } = value;
        let cond = match TypedExpr::try_from(*cond, known_values, known_types, Vec::new()) {
            Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
            Ok(cond) => {
                let loc = cond.get_loc();
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
        .boxed();

        let mut true_block_known_values = known_values.clone();
        let true_branch_block = {
            let mut block = Vec::new();
            for stmnt in true_branch.0 {
                match TypedStatement::try_from(stmnt, &true_block_known_values, known_types) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            let _ = true_block_known_values
                                .entry(data.ident.clone())
                                .and_modify(|it| {
                                    *it = data.ty.clone();
                                })
                                .or_insert(data.ty.clone());
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
            let cond = match TypedExpr::try_from(*cond, known_values, known_types, Vec::new()){
                Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
                Ok(cond) => {
                    let loc = cond.get_loc();
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
                    match TypedStatement::try_from(stmnt, &block_known_values, known_types) {
                        Ok(stmnt) => {
                            if let TypedStatement::Declaration(data) = &stmnt {
                                let _ = block_known_values
                                    .entry(data.ident.clone())
                                    .and_modify(|it| {
                                        *it = data.ty.clone();
                                    })
                                    .or_insert(data.ty.clone());
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
            let ret = match TypedExpr::try_from(*ret, &block_known_values, known_types,Vec::new()){
                Ok(ret) => {
                    if expected_ty == ResolvedType::Error {
                        expected_ty = ret.get_ty();
                    } else if ret.get_ty() != expected_ty {
                        let loc = ret.get_loc();
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

            (cond.boxed(),block,ret.boxed())
        }).collect();

        let mut else_block_known_values = known_values.clone();
        let else_branch_block = {
            let mut block = Vec::new();
            for stmnt in else_branch.0 {
                match TypedStatement::try_from(stmnt, &else_block_known_values, known_types) {
                    Ok(stmnt) => {
                        if let TypedStatement::Declaration(data) = &stmnt {
                            let _ = else_block_known_values
                                .entry(data.ident.clone())
                                .and_modify(|it| {
                                    *it = data.ty.clone();
                                })
                                .or_insert(data.ty.clone());
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
            &else_block_known_values,
            known_types,
            Vec::new(),
        ) {
            Ok(ret) => {
                if expected_ty == ResolvedType::Error {
                    #[allow(unused_assignments)]//here to supress a warning for now.
                    {//hopefully this only happens if there is only if/else and the if branch doesn't have a valid expression
                    expected_ty = ret.get_ty();
                    }
                    // not sure what to report here tbh.
                } else if ret.get_ty() != expected_ty {
                    let loc = ret.get_loc();
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
            true_branch: (true_branch_block, true_branch_ret.boxed()),
            else_ifs,
            else_branch: (else_branch_block, else_branch_ret.boxed()),
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

    pub(crate) fn get_ty(&self) -> ResolvedType {
        self.true_branch.1.get_ty()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedMemberRead {
    pub(crate) target: Box<TypedExpr>,
    pub(crate) member: String,
    pub(crate) offset: Option<usize>,
    pub(crate) ty: ResolvedType,
    pub(crate) loc: crate::Location,
}
impl TypedMemberRead {
    pub(crate) fn try_from(
        value: ast::BinaryOpCall,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
        already_processed_args: Vec<ResolvedType>,
    ) -> Result<Self, TypingError> {
        let ast::BinaryOpCall { loc, lhs, rhs, .. } = value;
        let ast::Expr::ValueRead(member, _) = *rhs else { return Err(TypingError::MemberMustBeIdent) };
        let value = TypedExpr::try_from(*lhs, known_values, known_types, already_processed_args)?;
        let ty = value.get_ty();

        if ty == ResolvedType::Error {
            return Err(TypingError::UnknownType);
        }
        let ResolvedType::User { generics, .. } = ty.clone() else { unreachable!() };
        if !generics.is_empty() {
            // we will have to do checking after lowering.  will do as part of lowering.
            Ok(Self {
                target: value.boxed(),
                member,
                offset: None,
                ty: ResolvedType::Error,
                loc,
            })
        } else if let Some(strct) = known_types.get(&ty.to_string()) {
            let ast::TypeDefinition::Struct(def) = strct else { unreachable!("how are you accessing a member not on a struct")};
            let offset = def.values.iter().position(|it| it.name == member);
            let ty = def
                .values
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
                target: value.boxed(),
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
        let Some(strct) = context.generated_generics.get(&(self.target.get_ty().to_string())) else {
            self.ty = ResolvedType::Error;
            return;
        };
        let TypedDeclaration::TypeDefinition(ResolvedTypeDeclaration::Struct(def)) = strct else { unreachable!("how are you accessing a member not on a struct")};
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
    let ResolvedType::Function { arg, returns } = &fun else { unreachable!() };
    let arg_t = args.pop().unwrap();
    if let ResolvedType::Generic { name, .. } = arg.as_ref() {
        let fun = returns.replace_generic(name, arg_t.clone());
        let (fun, mut replaced) = map_types_to_args(fun, args);
        replaced.push((name.clone(), arg_t.clone()));
        (
            ResolvedType::Function {
                arg: arg_t.boxed(),
                returns: fun.boxed(),
            },
            replaced,
        )
    } else {
        let (returns, replaced) = map_types_to_args(returns.as_ref().clone(), args);
        (
            ResolvedType::Function {
                arg: arg_t.boxed(),
                returns: returns.boxed(),
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
            TypedValueType::Err => (),
        }
        to_lower
            .generictypes
            .as_mut()
            .unwrap()
            .decls
            .retain(|(_,g)| match g {
                ResolvedType::Generic { name, .. } =>!replaced.iter().any(|(r, _)| r == name),
                _=>false
            })
    }
    match &mut to_lower {
        TypedDeclaration::Mod(_) => unreachable!(),
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
    pub(crate) loc: crate::Location,
    pub(crate) lhs: Box<TypedExpr>,
    pub(crate) rhs: Box<TypedExpr>,
    pub(crate) operator: String,
    pub(crate) rt: ResolvedType,
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
                            let ResolvedType::Float { width : lhs_w } = lhs else { unreachable!() };
                            let ResolvedType::Float { width : rhs_w } = rhs else { unreachable!() };
                            ResolvedType::Float {
                                width: lhs_w.max(rhs_w),
                            }
                        }
                        (lhs, rhs) if lhs.is_float() || rhs.is_generic() => lhs,
                        (lhs, rhs) if rhs.is_float() || lhs.is_generic() => rhs,
                        (lhs, rhs) => {
                            let ResolvedType::Int { signed : lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                            let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = lhs else { unreachable!() };
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
                            let ResolvedType::Float { width : lhs } = lhs else { unreachable!() };
                            let ResolvedType::Float { width : rhs } = rhs else { unreachable!() };
                            ResolvedType::Float {
                                width: lhs.max(rhs),
                            }
                        }
                        (lhs, _) if lhs.is_float() => lhs,
                        (_, rhs) if rhs.is_float() => rhs,
                        (lhs, rhs) => {
                            let ResolvedType::Int { signed : _lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                            let ResolvedType::Int { signed : _rhs_signed, width : rhs_w } = rhs else { unreachable!() };
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
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Result<Self, TypingError> {
        let ast::BinaryOpCall {
            loc,
            lhs,
            rhs,
            operator,
        } = value;
        let lhs = match TypedExpr::try_from(*lhs, known_values, known_types, Vec::new()) {
            Ok(lhs) => lhs,
            Err(e) => {
                println!("{:?}", e);
                TypedExpr::ErrorNode
            }
        };
        let rhs = match TypedExpr::try_from(*rhs, known_values, known_types, Vec::new()) {
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                        operator,
                        rt: match (lhs_t, rhs_t) {
                            (ResolvedType::Error, _) | (_, ResolvedType::Error) => types::ERROR,
                            (lhs, rhs) if lhs.is_float() && rhs.is_float() => {
                                let ResolvedType::Float { width : lhs_w } = lhs else { unreachable!() };
                                let ResolvedType::Float { width : rhs_w } = rhs else { unreachable!() };
                                ResolvedType::Float {
                                    width: lhs_w.max(rhs_w),
                                }
                            }
                            (lhs, rhs) if lhs.is_float() || rhs.is_generic() => lhs,
                            (lhs, rhs) if rhs.is_float() || lhs.is_generic() => rhs,
                            (lhs, rhs) => {
                                let ResolvedType::Int { signed : lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                                let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = lhs else { unreachable!() };
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
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                        operator,
                        rt: match (lhs_t, rhs_t) {
                            (ResolvedType::Error, _) | (_, ResolvedType::Error) => types::ERROR,
                            (lhs, rhs) if lhs.is_float() && rhs.is_float() => {
                                let ResolvedType::Float { width : lhs } = lhs else { unreachable!() };
                                let ResolvedType::Float { width : rhs } = rhs else { unreachable!() };
                                ResolvedType::Float {
                                    width: lhs.max(rhs),
                                }
                            }
                            (lhs, _) if lhs.is_float() => lhs,
                            (_, rhs) if rhs.is_float() => rhs,
                            (lhs, rhs) => {
                                let ResolvedType::Int { signed : _lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                                let ResolvedType::Int { signed : _rhs_signed, width : rhs_w } = rhs else { unreachable!() };
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
    pub(crate) operand: Box<TypedExpr>,
    pub(crate) operator: String,
    pub(crate) rt: ResolvedType,
}

fn strip_pointers(ty: &ResolvedType) -> ResolvedType {
    if let ResolvedType::Function { arg, returns } = ty {
        let arg = match arg.as_ref() {
            ResolvedType::Pointer { underlining } if underlining.is_function() => {
                strip_pointers(underlining.as_ref()).boxed()
            }
            _ => arg.clone(),
        };
        let returns = match returns.as_ref() {
            ResolvedType::Pointer { underlining } if underlining.is_function() => {
                strip_pointers(underlining.as_ref()).boxed()
            }
            _ => returns.clone(),
        };
        ResolvedType::Function { arg, returns }
    } else {
        ty.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedMatch {
    pub(crate) loc: crate::Location,
    pub(crate) on: Box<TypedExpr>,
    pub(crate) arms: Vec<TypedMatchArm>,
}

impl TypedMatch {
    fn from(
        value: ast::Match,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Self {
        let ast::Match { loc, on, arms } = value;
        let on = match TypedExpr::try_from(*on, known_values, known_types, Vec::new()) {
            Ok(it) => it,
            Err(e) => {
                println!("{e:?}");
                TypedExpr::ErrorNode
            }
        }
        .boxed();
        let mut new_arms = Vec::with_capacity(arms.len());
        for arm in arms {
            let arm = TypedMatchArm::from(arm, known_values, known_types, &on.get_ty());
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
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
    ) -> Self {
        let ast::Match { loc, on, arms } = value;
        let on = match TypedExpr::try_from(*on, known_values, known_types, Vec::new()) {
            Ok(it) => it,
            Err(e) => {
                println!("{e:?}");
                TypedExpr::ErrorNode
            }
        }
        .boxed();
        let mut new_arms = Vec::with_capacity(arms.len());
        for arm in arms {
            let arm = TypedMatchArm::as_statement(arm, known_values, known_types, &on.get_ty());
            new_arms.push(arm);
        }
        Self {
            loc,
            on,
            arms: new_arms,
        }
    }

    pub(crate) fn get_ty(&self) -> ResolvedType {
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
                            .map(|it| it.get_loc())
                            .unwrap_or_else(|| arm.loc);
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
    pub(crate) loc: crate::Location,
    pub(crate) cond: TypedPattern,
    pub(crate) block: Vec<TypedStatement>,
    pub(crate) ret: Option<Box<TypedExpr>>,
}

impl TypedMatchArm {
    fn from(
        value: ast::MatchArm,
        known_values: &HashMap<String, ResolvedType>,
        known_types: &HashMap<String, ast::TypeDefinition>,
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
        let cond = TypedPattern::from(cond, expected_comp);
        if let TypedPattern::Read(name, ty) = &cond {
            known_values.insert(name.clone(), ty.clone());
        }
        for stmnt in block {
            match TypedStatement::try_from(stmnt, &known_values, known_types) {
                Ok(stmnt) => {
                    if let TypedStatement::Declaration(v) = &stmnt {
                        known_values.insert(v.ident.clone(), v.ty.clone());
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
            match TypedExpr::try_from(*ret, &known_values, known_types, Vec::new()) {
                Ok(ret) => ret,
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            }
            .boxed()
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

    fn as_statement(arm: ast::MatchArm, known_values: &HashMap<String, ResolvedType>, known_types: &HashMap<String, ast::TypeDefinition>, expected_comp: &ResolvedType) -> TypedMatchArm {
        let ast::MatchArm {
            block,
            ret,
            cond,
            loc,
        } = arm;
        let mut known_values = known_values.clone();
        let mut new_block = Vec::with_capacity(block.len());
        let cond = TypedPattern::from(cond, expected_comp);
        if let TypedPattern::Read(name, ty) = &cond {
            known_values.insert(name.clone(), ty.clone());
        }
        for stmnt in block {
            match TypedStatement::try_from(stmnt, &known_values, known_types) {
                Ok(stmnt) => {
                    if let TypedStatement::Declaration(v) = &stmnt {
                        known_values.insert(v.ident.clone(), v.ty.clone());
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
            let ret = match TypedExpr::try_from(*ret, &known_values, known_types, Vec::new()) {
                Ok(ret) => ret,
                Err(e) => {
                    println!("{e:?}");
                    TypedExpr::ErrorNode
                }
            };
            if ret != TypedExpr::UnitLiteral {
                let loc = ret.get_loc();
                new_block.push(TypedStatement::Discard(ret, loc));
            }
        }

        Self {
            loc,
            cond,
            block: new_block,
            ret:None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedPattern {
    Const(String, ResolvedType),
    Read(String, ResolvedType),
    Default,
}

impl TypedPattern {
    fn from(value: ast::Pattern, expected_type: &ResolvedType) -> Self {
        match value {
            ast::Pattern::Default => Self::Default,
            ast::Pattern::ConstNumber(n) if expected_type.is_int() || expected_type.is_float() => {
                Self::Const(n, expected_type.clone())
            }
            ast::Pattern::ConstNumber(n) => {
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
                    "trying to match against a char when {} is expected",
                    expected_type.to_string()
                );
                Self::Const(b.to_string(), types::ERROR)
            }
            ast::Pattern::Read(a) => Self::Read(a, expected_type.clone()),
        }
    }
}

#[derive(Debug)]
#[allow(unused)]
pub enum TypingError {
    ReturnTypeMismatch,
    FnNotDeclared,
    BlockTypeMismatch,
    MemberMustBeIdent,
    OpNotSupported, //temp.
    UnknownType,
    DoubleTyped,
    ArgTypeMismatch,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::TypedExpr;
    use crate::ast::{self, ArgDeclation};
    use crate::parser::Parser;
    use crate::typed_ast::{
        ResolvedGenericsDecl, ResolvedTypeDeclaration, StructDefinition, TypedBinaryOpCall, TypedDeclaration, TypedFnCall, TypedIfBranching, TypedIfExpr, TypedMatch, TypedMatchArm, TypedModuleDeclaration, TypedStatement, TypedValueDeclaration, TypedValueType
    };
    use crate::types::{self, ResolvedType};
    use crate::util::ExtraUtilFunctions;
    lazy_static::lazy_static! {
        static ref PREDEFINED_VALUES : HashMap<String,ResolvedType> = {
            let mut out = HashMap::new();
            out.insert("foo".to_string(), ResolvedType::Function { arg: types::INT32.boxed(), returns: types::INT32.boxed() });
            out.insert("bar".to_string(), types::INT32);
            out
        };
    }

    #[test]
    #[ignore = "for debugging only"]
    fn debugging() {
        const SRC: &'static str = r#"
for<T> type Generic = {
    a : T
}

for<T> let id a : T -> T = a

let new_generic a : int32 -> Generic<int32> = Generic<int32> {a:a}

let main _ : () -> () =
    let r : Generic<int32> = new_generic 3
    id r.a
"#;
        let parser = Parser::from_source(SRC);
        let mut module = parser.module("foo".to_string());
        module.canonialize(vec!["P".to_string()]);
        let mut module = TypedModuleDeclaration::from(module, &HashMap::new());
        module.lower_generics(&HashMap::new());
        println!("{:?}", module)
    }

    #[test]
    fn expr_convert() {
        use crate::ast::{self, Expr};
        assert_eq!(
            TypedExpr::try_from(
                Expr::NumericLiteral {
                    value: "1".to_string(),
                    ty: types::INT32,
                },
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
                    ty: types::FLOAT32
                },
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
                        ty: types::INT32
                    }
                    .boxed(),
                    rhs: Expr::NumericLiteral {
                        value: "2".to_string(),
                        ty: types::INT32
                    }
                    .boxed(),
                    operator: "+".to_string()
                }),
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
                .boxed(),
                rhs: TypedExpr::IntegerLiteral {
                    value: "2".to_string(),
                    size: types::IntWidth::ThirtyTwo
                }
                .boxed(),
                operator: "+".to_string(),
                rt: types::INT32
            }),
            "1 + 2"
        );

        // TODO : Unary ops.  not yet implemented at parse level.

        assert_eq!(
            TypedExpr::try_from(
                Expr::ValueRead("bar".to_string(), (0, 0)),
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
                    value: Expr::ValueRead("foo".to_string(), (0, 0)).boxed(),
                    arg: Some(Expr::ValueRead("bar".to_string(), (0, 0)).boxed()),
                }),
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
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    },
                    (0, 0)
                )
                .boxed(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32, (0, 0)).boxed()),
                arg_t: types::INT32,
                rt: types::INT32
            }),
            "foo bar"
        );
    }

    #[test]
    fn decl_body_convert() {
        use super::TypedValueType;
        use crate::ast::{self, ValueType};
        assert_eq!(
            TypedValueType::try_from(
                ValueType::Expr(ast::Expr::NumericLiteral {
                    value: "1".to_string(),
                    ty: types::INT32
                }),
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
                        ty: types::INT32
                    },
                    (0, 0)
                )]),
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
        use crate::ast::{self, Statement};
        assert_eq!(
            TypedStatement::try_from(
                Statement::Declaration(ast::ValueDeclaration {
                    loc: (0, 0),
                    is_op: false,
                    ident: "foo".to_string(),
                    args: Vec::new(),
                    ty: Some(types::INT32),
                    value: ast::ValueType::Expr(ast::Expr::NumericLiteral {
                        value: "1".to_string(),
                        ty: types::INT32
                    }),
                    generictypes: None
                }),
                &HashMap::new(),
                &HashMap::new(),
            )
            .expect(""),
            TypedStatement::Declaration(TypedValueDeclaration {
                loc: (0, 0),
                is_op: false,
                ident: "foo".to_string(),
                args: Vec::new(),
                value: TypedValueType::Expr(TypedExpr::IntegerLiteral {
                    value: "1".to_string(),
                    size: types::IntWidth::ThirtyTwo
                }),
                ty: types::INT32,
                generictypes: None,
                is_curried: false,
            }),
            "decl statement"
        );

        assert_eq!(
            TypedStatement::try_from(
                Statement::Return(ast::Expr::ValueRead("bar".to_string(), (0, 0)), (0, 0)),
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
                    value: ast::Expr::ValueRead("foo".to_string(), (0, 0)).boxed(),
                    arg: Some(ast::Expr::ValueRead("bar".to_string(), (0, 0)).boxed()),
                }),
                &PREDEFINED_VALUES,
                &HashMap::new(),
            )
            .expect(""),
            TypedStatement::FnCall(super::TypedFnCall {
                loc: (0, 0),
                value: TypedExpr::ValueRead(
                    "foo".to_string(),
                    ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    },
                    (0, 0)
                )
                .boxed(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32, (0, 0)).boxed()),
                arg_t: types::INT32,
                rt: types::INT32
            }),
            "foo bar"
        );

        // TODO: Pipe.  not implemented at parser level
    }

    #[test]
    fn decl_convert() {
        use super::TypedDeclaration;
        use crate::ast;

        assert_eq!(
            TypedDeclaration::try_from(
                ast::Declaration::Value(ast::ValueDeclaration {
                    loc: (0, 0),
                    is_op: false,
                    ident: "test".to_string(),
                    args: vec![ArgDeclation {
                        ident: "a".to_string(),
                        loc: (0, 0)
                    }],
                    ty: Some(ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    }),
                    value: ast::ValueType::Function(vec![ast::Statement::Return(
                        ast::Expr::NumericLiteral {
                            value: "1".to_string(),
                            ty: types::INT32
                        },
                        (0, 0)
                    )]),
                    generictypes: None,
                }),
                &HashMap::new(),
                &HashMap::new(),
            )
            .expect(""),
            TypedDeclaration::Value(super::TypedValueDeclaration {
                loc: (0, 0),
                is_op: false,
                ident: "test".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (0, 0)
                }],
                ty: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                },
                value: super::TypedValueType::Function(vec![super::TypedStatement::Return(
                    super::TypedExpr::IntegerLiteral {
                        value: "1".to_string(),
                        size: types::IntWidth::ThirtyTwo
                    },
                    (0, 0)
                )]),
                generictypes: None,
                is_curried: false
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
    test 3;
"#,
        );
        let module = parser.module("test".to_string());
        let module = super::TypedModuleDeclaration::from(module, &HashMap::new());
        let [generic, main] = &module.declarations[..] else { unreachable!() };
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 11),
                is_op: false,
                ident: "test".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (1, 16)
                }],
                value: TypedValueType::Expr(TypedExpr::ValueRead(
                    "a".to_string(),
                    ResolvedType::Generic {
                        name: "T".to_string(),
                        loc:(1,20)
                    },
                    (1, 29)
                )),
                ty: ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc:(1,20),
                    }
                    .boxed(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc:(1,25)
                    }
                    .boxed()
                },
                generictypes: Some(ResolvedGenericsDecl {
                    for_loc : (1,0),
                    decls: vec![
                        (
                            (1,4),
                            ResolvedType::Generic { 
                                name:"T".to_string(),
                                loc:(1,4) 
                            }
                        )
                    ],
                }),
                is_curried: false,
            }),
            generic,
            "generic"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (3, 4),
                is_op: false,
                ident: "main".to_string(),
                args: vec![ArgDeclation {
                    ident: "_".to_string(),
                    loc: (3, 9)
                }],
                value: TypedValueType::Function(vec![TypedStatement::FnCall(TypedFnCall {
                    loc: (4, 4),
                    value: TypedExpr::ValueRead(
                        "test".to_string(),
                        ResolvedType::Function {
                            arg: ResolvedType::Generic {
                                name: "T".to_string(),
                                loc:(1,20)
                            }
                            .boxed(),
                            returns: ResolvedType::Generic {
                                name: "T".to_string(),
                                loc:(1,25)
                            }
                            .boxed()
                        },
                        (4, 4)
                    )
                    .boxed(),
                    arg: Some(
                        TypedExpr::IntegerLiteral {
                            value: "3".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        }
                        .boxed()
                    ),
                    rt: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc:(1,25)
                    },
                    arg_t: types::INT32
                })]),
                ty: ResolvedType::Function {
                    arg: types::UNIT.boxed(),
                    returns: types::UNIT.boxed()
                },
                generictypes: None,
                is_curried: false,
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

        let module = parser.module("test.fb".to_string());
        let mut module = TypedModuleDeclaration::from(module, &HashMap::new());
        let [strct, _] = &module.declarations[..] else { unreachable!() };
        assert_eq!(
            strct,
            &TypedDeclaration::TypeDefinition(crate::typed_ast::ResolvedTypeDeclaration::Struct(
                StructDefinition {
                    ident: "Tuple".to_string(),
                    generics: Some(ResolvedGenericsDecl {
                        for_loc : (0,0),
                        decls:vec![
                            (
                                (0,4), 
                                ResolvedType::Generic{ 
                                    name: "T".to_string(),
                                    loc:(0,4),
                                },

                            ),
                            (
                                (0,6), 
                                ResolvedType::Generic{ 
                                    name: "U".to_string(),
                                    loc:(0,6)
                                }
                            )
                        ]
                    }),
                    fields: vec![
                        crate::ast::FieldDecl {
                            name: "first".to_string(),
                            ty: ResolvedType::Generic {
                                name: "T".to_string(),
                                loc:(2,12),
                            },
                            loc: (2, 5)
                        },
                        crate::ast::FieldDecl {
                            name: "second".to_string(),
                            ty: ResolvedType::Generic {
                                name: "U".to_string(),
                                loc:(3,13)
                            },
                            loc: (3, 5)
                        }
                    ],
                    loc: (1, 15)
                }
            )),
            "pre-lowering struct",
        );

        module.lower_generics(&HashMap::new());

        let [_unlowered, fun, generated_strct] = &module.declarations[..] else { unreachable!() };
        assert_eq!(
            fun,
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (6, 4),
                is_op: false,
                ident: "first".to_string(),
                args: vec![ArgDeclation {
                    loc: (6, 10),
                    ident: "a".to_string(),
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
                        loc:(7,14)
                    }
                    .boxed(),
                    returns: types::INT32.boxed()
                },
                generictypes: None,
                is_curried: false,
            }),
            "post lowering function"
        );

        assert_eq!(
            generated_strct,
            &TypedDeclaration::TypeDefinition(ResolvedTypeDeclaration::Struct(StructDefinition {
                ident: "Tuple<int32,float64>".to_string(),
                generics: None,
                fields: vec![
                    crate::ast::FieldDecl {
                        name: "first".to_string(),
                        ty: types::INT32,
                        loc: (2, 5)
                    },
                    crate::ast::FieldDecl {
                        name: "second".to_string(),
                        ty: types::FLOAT64,
                        loc: (3, 5)
                    }
                ],
                loc: (1, 15)
            }))
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

let main _ : int32 -> int32 =
    test 3;
    return 0;
"#,
        ));
        let module = parser.module("test".to_string());
        let mut module = TypedModuleDeclaration::from(module, &HashMap::new());
        module.lower_generics(&HashMap::new());
        let [generic, main, generated] = &module.declarations[..] else { unreachable!("should have three when done")};
        assert_eq!(
            generic,
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 11),
                is_op: false,
                ident: "test".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (1, 16)
                }],
                value: TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::ValueRead(
                        "a".to_string(),
                        ResolvedType::Generic {
                            name: "T".to_string(),
                            loc:(1,20)
                        },
                        (2, 11)
                    ),
                    (2, 4)
                )]),
                ty: ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc:(1,20),
                    }
                    .boxed(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string(),
                        loc:(1,25),
                    }
                    .boxed()
                },
                generictypes: Some(ResolvedGenericsDecl {
                    for_loc:(1,0),
                    decls:vec![
                        (
                            (1,4), 
                            ResolvedType::Generic{ 
                                name: "T".to_string(),
                                loc:(1,4)
                            }
                        )
                    ],
                }),
                is_curried: false,
            }),
            "generic should be untouched"
        );
        assert_eq!(
            main,
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (4, 4),
                is_op: false,
                ident: "main".to_string(),
                args: vec![ArgDeclation {
                    ident: "_".to_string(),
                    loc: (4, 9)
                }],
                value: TypedValueType::Function(vec![
                    TypedStatement::FnCall(TypedFnCall {
                        loc: (5, 4),
                        value: TypedExpr::ValueRead(
                            "test<int32>".to_string(),
                            ResolvedType::Function {
                                arg: types::INT32.boxed(),
                                returns: types::INT32.boxed()
                            },
                            (5, 4)
                        )
                        .boxed(),
                        arg: Some(
                            TypedExpr::IntegerLiteral {
                                value: "3".to_string(),
                                size: types::IntWidth::ThirtyTwo
                            }
                            .boxed()
                        ),
                        rt: types::INT32,
                        arg_t: types::INT32
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
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                },
                generictypes: None,
                is_curried: false,
            }),
            "main should have the value read changed"
        );
        assert_eq!(
            generated,
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 11),
                is_op: false,
                ident: "test<int32>".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (1, 16)
                }],
                value: crate::typed_ast::TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::ValueRead("a".to_string(), types::INT32, (2, 11)),
                    (2, 4)
                )]),
                ty: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                },
                generictypes: None,
                is_curried: false,
            }),
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
        let mut module = parser.module("test".to_string());
        module.canonialize(vec!["Tests".to_string()]);
        let module = TypedModuleDeclaration::from(module, &PREDEFINED_VALUES);
        let [expr,stmnt] = &module.declarations[..] else {unreachable!("more than two?") };
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 4),
                is_op: false,
                ident: "Tests::test::expr_with_statement".to_string(),
                args: vec![crate::ast::ArgDeclation {
                    loc: (1, 24),
                    ident: "a".to_string()
                }],
                value: TypedValueType::Expr(TypedExpr::IfExpr(TypedIfExpr {
                    cond: TypedExpr::ValueRead("a".to_string(), types::BOOL, (1, 47)).boxed(),
                    true_branch: (
                        vec![TypedStatement::FnCall(TypedFnCall {
                            loc: (2, 8),
                            value: TypedExpr::ValueRead(
                                "foo".to_string(),
                                ResolvedType::Function {
                                    arg: types::INT32.boxed(),
                                    returns: types::INT32.boxed()
                                },
                                (2, 8)
                            )
                            .boxed(),
                            arg: Some(
                                TypedExpr::IntegerLiteral {
                                    value: "3".to_string(),
                                    size: types::IntWidth::ThirtyTwo
                                }
                                .boxed()
                            ),
                            rt: types::INT32,
                            arg_t: types::INT32,
                        })],
                        TypedExpr::IntegerLiteral {
                            value: "0".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        }
                        .boxed()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        vec![TypedStatement::FnCall(TypedFnCall {
                            loc: (5, 8),
                            value: TypedExpr::ValueRead(
                                "foo".to_string(),
                                ResolvedType::Function {
                                    arg: types::INT32.boxed(),
                                    returns: types::INT32.boxed()
                                },
                                (5, 8)
                            )
                            .boxed(),
                            arg: Some(
                                TypedExpr::IntegerLiteral {
                                    value: "4".to_string(),
                                    size: types::IntWidth::ThirtyTwo
                                }
                                .boxed()
                            ),
                            rt: types::INT32,
                            arg_t: types::INT32,
                        })],
                        TypedExpr::IntegerLiteral {
                            value: "1".to_string(),
                            size: types::IntWidth::ThirtyTwo
                        }
                        .boxed()
                    ),
                    loc: (1, 44)
                })),
                ty: ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: types::INT32.boxed()
                },
                generictypes: None,
                is_curried: false,
            }),
            expr,
            "expression if"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (8, 4),
                is_op: false,
                ident: "Tests::test::statement_with_else_if".to_string(),
                args: vec![
                    crate::ast::ArgDeclation {
                        loc: (8, 27),
                        ident: "a".to_string()
                    },
                    crate::ast::ArgDeclation {
                        loc: (8, 29),
                        ident: "b".to_string()
                    },
                ],
                ty: ResolvedType::Function {
                    arg: types::BOOL.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::BOOL.boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed(),
                },
                value: TypedValueType::Function(vec![TypedStatement::IfBranching(
                    TypedIfBranching {
                        cond: TypedExpr::ValueRead("a".to_string(), types::BOOL, (9, 7)).boxed(),
                        true_branch: vec![TypedStatement::Return(
                            TypedExpr::IntegerLiteral {
                                value: "0".to_string(),
                                size: types::IntWidth::ThirtyTwo
                            },
                            (10, 8)
                        )],
                        else_ifs: vec![(
                            TypedExpr::ValueRead("b".to_string(), types::BOOL, (11, 12)).boxed(),
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
                generictypes: None,
                is_curried: false,
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
        .module("test".to_string());

        let module = TypedModuleDeclaration::from(module, &PREDEFINED_VALUES);

        let [simple, nest_in_call,statement] = &module.declarations[..] else { unreachable!() };
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 4),
                is_op: false,
                ident: "simple_expr".to_string(),
                args: vec![
                    ast::ArgDeclation {
                        loc: (1, 16),
                        ident: "a".to_string()
                    },
                    ast::ArgDeclation {
                        loc: (1, 18),
                        ident: "fun".to_string()
                    },
                ],
                value: TypedValueType::Expr(TypedExpr::Match(TypedMatch {
                    loc: (1, 61),
                    on: TypedExpr::FnCall(TypedFnCall {
                        loc: (1, 71),
                        value: TypedExpr::ValueRead(
                            "fun".to_string(),
                            ResolvedType::Function {
                                arg: types::INT32.boxed(),
                                returns: types::INT32.boxed()
                            },
                            (1, 67)
                        )
                        .boxed(),
                        arg: Some(
                            TypedExpr::ValueRead("a".to_string(), types::INT32, (1, 71)).boxed()
                        ),
                        rt: types::INT32,
                        arg_t: types::INT32,
                    })
                    .boxed(),
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
                                .boxed()
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
                                .boxed()
                            )
                        },
                        TypedMatchArm {
                            loc: (4, 2),
                            cond: crate::typed_ast::TypedPattern::Read(
                                "a".to_string(),
                                types::INT32
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
                                    .boxed(),
                                    rhs: TypedExpr::IntegerLiteral {
                                        value: "3".to_string(),
                                        size: types::IntWidth::ThirtyTwo
                                    }
                                    .boxed(),
                                    operator: "*".to_string(),
                                    rt: types::INT32
                                })
                                .boxed()
                            )
                        },
                    ]
                })),
                ty: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: ResolvedType::Function {
                        arg: ResolvedType::Function {
                            arg: types::INT32.boxed(),
                            returns: types::INT32.boxed()
                        }
                        .boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed()
                },
                generictypes: None,
                is_curried: false,
            }),
            simple,
            "simple"
        );

        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (6, 4),
                is_op: false,
                ident: "nest_in_call".to_string(),
                args: vec![
                    ast::ArgDeclation {
                        loc: (6, 17),
                        ident: "a".to_string()
                    },
                    ast::ArgDeclation {
                        loc: (6, 19),
                        ident: "fun".to_string()
                    },
                ],
                value: TypedValueType::Expr(TypedExpr::FnCall(TypedFnCall {
                    loc: (6, 67),
                    value: TypedExpr::ValueRead(
                        "fun".to_string(),
                        ResolvedType::Function {
                            arg: types::INT32.boxed(),
                            returns: types::INT32.boxed()
                        },
                        (6, 62)
                    )
                    .boxed(),
                    arg: Some(
                        TypedExpr::Match(TypedMatch {
                            loc: (6, 67),
                            on: TypedExpr::ValueRead("a".to_string(), types::INT32, (6, 73))
                                .boxed(),
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
                                        .boxed()
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
                                        .boxed()
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
                                            .boxed(),
                                            rhs: TypedExpr::IntegerLiteral {
                                                value: "3".to_string(),
                                                size: types::IntWidth::ThirtyTwo
                                            }
                                            .boxed(),
                                            operator: "*".to_string(),
                                            rt: types::INT32
                                        })
                                        .boxed()
                                    )
                                },
                            ]
                        })
                        .boxed()
                    ),
                    rt: types::INT32,
                    arg_t: types::INT32
                })),
                ty: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: ResolvedType::Function {
                        arg: ResolvedType::Function {
                            arg: types::INT32.boxed(),
                            returns: types::INT32.boxed()
                        }
                        .boxed(),
                        returns: types::INT32.boxed()
                    }
                    .boxed()
                },
                generictypes: None,
                is_curried: false,
            }),
            nest_in_call,
            "nested in a call"
        );

        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (12, 4),
                is_op: false,
                ident: "as_statement".to_string(),
                args: vec![
                    ast::ArgDeclation {
                        loc: (12, 17),
                        ident: "a".to_string()
                    },
                    ast::ArgDeclation {
                        loc: (12, 19),
                        ident: "b".to_string()
                    },
                ],
                value: TypedValueType::Function(vec![TypedStatement::Match(TypedMatch {
                    loc: (13, 4),
                    on: TypedExpr::ValueRead("a".to_string(), types::INT32, (13, 10)).boxed(),
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
                                    .boxed(),
                                arms: vec![
                                    TypedMatchArm {
                                        loc: (16, 10),
                                        cond: crate::typed_ast::TypedPattern::Const(
                                            "1".to_string(),
                                            types::INT32
                                        ),
                                        block: vec![
                                            TypedStatement::Discard(TypedExpr::FnCall(TypedFnCall {
                                                loc: (16, 15),
                                                value: TypedExpr::ValueRead(
                                                    "foo".to_string(),
                                                    ResolvedType::Function {
                                                        arg: types::INT32.boxed(),
                                                        returns: types::INT32.boxed()
                                                    },
                                                    (16, 15)
                                                )
                                                .boxed(),
                                                arg: Some(
                                                    TypedExpr::IntegerLiteral {
                                                        value: "1".to_string(),
                                                        size: types::IntWidth::ThirtyTwo
                                                    }
                                                    .boxed()
                                                ),
                                                rt: types::INT32,
                                                arg_t: types::INT32,
                                            }),(16,15))
                                        ],
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
                                                    arg: types::INT32.boxed(),
                                                    returns: types::INT32.boxed()
                                                },
                                                (18, 12)
                                            )
                                            .boxed(),
                                            arg: Some(
                                                TypedExpr::IntegerLiteral {
                                                    value: "3".to_string(),
                                                    size: types::IntWidth::ThirtyTwo
                                                }
                                                .boxed()
                                            ),
                                            rt: types::INT32,
                                            arg_t: types::INT32,
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
                    arg: types::INT32.boxed(),
                    returns: ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::UNIT.boxed()
                    }
                    .boxed(),
                },
                generictypes: None,
                is_curried: false,
            }),
            statement,
            "in a statement and nested in itself"
        )
    }
}

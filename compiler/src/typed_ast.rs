use itertools::Itertools;
use std::{collections::HashMap, num::NonZeroU8};

use crate::{
    ast::{self, ArgDeclation, Declaration},
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
                                .iter()
                                .map(|it| ResolvedType::Generic { name: it.clone() })
                                .collect(),
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
            TypedDeclaration::Value(v) => v.generictypes.clone(),
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
            ResolvedTypeDeclaration::Struct(strct) => strct.generics.clone(),
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)], context: &mut LoweringContext) {
        match self {
            ResolvedTypeDeclaration::Struct(strct) => strct.replace_types(types, context),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructDefinition {
    pub(crate) ident: String,
    pub(crate) generics: Vec<String>,
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
        for generic in &generics {
            values.iter_mut().for_each(|field| {
                field.ty = field.ty.clone().replace_user_with_generic(&generic);
            })
        }

        Ok(Self {
            ident,
            generics,
            fields: values,
            loc,
        })
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        if self.generics.is_empty() {
            for field in &mut self.fields {
                field.ty.lower_generics(context)
            }
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)], context: &mut LoweringContext) {
        if self.generics.len() == 0 {
            return;
        }
        self.generics = self
            .generics
            .iter()
            .cloned()
            .filter(|name| !types.iter().map(|(n, _)| n).contains(name))
            .collect();
        assert_eq!(
            self.generics.len(),
            0,
            "generic not completed.  should not be reached!"
        );
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
    pub(crate) generictypes: Vec<String>,
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
            generictypes,
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
            Self::Return(expr, _) => expr.lower_generics(context),
            Self::FnCall(call) => call.lower_generics(context),
            Self::Pipe(_) => todo!(),
            Self::IfBranching(ifstmnt) => ifstmnt.lower_generics(context),
            Self::Error => todo!(),
        }
    }

    fn get_ty(&self) -> ResolvedType {
        match self {
            Self::Declaration(decl) => decl.ty.clone(),
            Self::Return(expr, _) => expr.get_ty(),
            Self::FnCall(call) => call.rt.clone(),
            Self::Pipe(_) => todo!(),
            Self::IfBranching(_) => todo!(),
            Self::Error => todo!(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedIfBranching {
    cond: Box<TypedExpr>,
    true_branch: Vec<TypedStatement>,
    else_ifs: Vec<(Box<TypedExpr>, Vec<TypedStatement>)>,
    else_branch: Vec<TypedStatement>,
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
        } = value;
        let cond = match TypedExpr::try_from(*cond, known_values, known_types, Vec::new()) {
            Ok(cond) if cond.get_ty() == ResolvedType::Bool => cond,
            Ok(cond) => {
                let loc = cond.get_loc();
                println!(
                    "condition must be a boolean expresion but got:{} at line:{}, col:{}",
                    cond.get_ty().to_string(),loc.0, loc.1
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
        use ast::Expr;
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
            Expr::ValueRead(value,loc) => {
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
            Expr::Error => Ok(Self::ErrorNode),
        }
    }

    pub(crate) fn get_loc(&self) -> crate::Location {
        match self {
            Self::IntegerLiteral { value, size } => todo!(),
            Self::FloatLiteral { value, size } => todo!(),
            Self::StringLiteral(_) => todo!(),
            Self::CharLiteral(_) => todo!(),
            Self::UnitLiteral => todo!(),
            Self::BinaryOpCall(bin) => bin.loc,
            Self::MemeberRead(read) => read.loc,
            Self::UnaryOpCall(_) => todo!(),
            Self::FnCall(call) => call.loc,
            Self::ValueRead(_, _, loc) => *loc,
            Self::ArrayLiteral { contents } => todo!(),
            Self::ListLiteral { contents } => todo!(),
            Self::TupleLiteral { contents } => todo!(),
            Self::StructConstruction(con) => con.loc,
            Self::IfExpr(ifexpr) => ifexpr.loc,
            Self::ErrorNode => todo!(),
        }
    }

    pub(crate) fn get_ty(&self) -> ResolvedType {
        match self {
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
            Self::ErrorNode => types::ERROR,
            Self::StructConstruction(strct) => ResolvedType::User {
                name: strct.ident.clone(),
                generics: strct.generics.clone(),
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
            Self::ErrorNode => (),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedIfExpr {
    cond: Box<TypedExpr>,
    true_branch: (Vec<TypedStatement>, Box<TypedExpr>),
    else_ifs: Vec<(Box<TypedExpr>, Vec<TypedStatement>, Box<TypedExpr>)>,
    else_branch: (Vec<TypedStatement>, Box<TypedExpr>),
    loc: crate::Location,
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
                    cond.get_ty().to_string(),loc.0, loc.1
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
                    //hopefully this only happens if there is only if/else and the if branch doesn't have a valid expression
                    expected_ty = ret.get_ty();
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
    if let ResolvedType::Generic { name } = arg.as_ref() {
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
            .retain(|g| !replaced.iter().any(|(r, _)| r == g))
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
                            let ResolvedType::Int { signed : lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                            let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = rhs else { unreachable!() };
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
            "." => {
                todo!()
            }
            "*" | "+" | "/" | "-" => {
                // TODO: need to add support for overloading this.
                let lhs_t = lhs.get_ty();
                let rhs_t = rhs.get_ty();
                if (lhs_t.is_float() || lhs_t.is_int() || lhs_t.is_generic())
                    && (rhs_t.is_float() || rhs_t.is_int() || rhs_t.is_generic())
                {
                    Ok(Self {
                        loc,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                        operator,
                        rt: match (lhs_t, rhs_t) {
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
                if (lhs_t.is_float() || lhs_t.is_int() || lhs_t.is_generic())
                    && (rhs_t.is_float() || rhs_t.is_int() || rhs_t.is_generic())
                {
                    Ok(Self {
                        loc,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                        operator,
                        rt: match (lhs_t, rhs_t) {
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
                                let ResolvedType::Int { signed : lhs_signed, width : lhs_w } = lhs else { unreachable!() };
                                let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = rhs else { unreachable!() };
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
    use crate::ast::ArgDeclation;
    use crate::parser::Parser;
    use crate::typed_ast::{
        ResolvedTypeDeclaration, StructDefinition, TypedDeclaration, TypedFnCall, TypedIfExpr,
        TypedModuleDeclaration, TypedStatement, TypedValueDeclaration, TypedValueType, TypedIfBranching,
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
                Expr::ValueRead("bar".to_string(),(0,0)),
                &PREDEFINED_VALUES,
                &HashMap::new(),
                Vec::new()
            )
            .expect(""),
            TypedExpr::ValueRead("bar".to_string(), types::INT32,(0,0)),
            "reading a value"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::FnCall(ast::FnCall {
                    loc: (0, 0),
                    value: Expr::ValueRead("foo".to_string(),(0,0)).boxed(),
                    arg: Some(Expr::ValueRead("bar".to_string(),(0,0)).boxed()),
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
                    (0,0)
                )
                .boxed(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32, (0,0)).boxed()),
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
                    generictypes: Vec::new()
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
                generictypes: Vec::new(),
                is_curried: false,
            }),
            "decl statement"
        );

        assert_eq!(
            TypedStatement::try_from(
                Statement::Return(ast::Expr::ValueRead("bar".to_string(),(0,0)), (0, 0)),
                &PREDEFINED_VALUES,
                &HashMap::new(),
            )
            .expect(""),
            TypedStatement::Return(
                TypedExpr::ValueRead("bar".to_string(), types::INT32, (0,0)),
                (0, 0)
            ),
            "return"
        );

        assert_eq!(
            TypedStatement::try_from(
                Statement::FnCall(ast::FnCall {
                    loc: (0, 0),
                    value: ast::Expr::ValueRead("foo".to_string(),(0,0)).boxed(),
                    arg: Some(ast::Expr::ValueRead("bar".to_string(),(0,0)).boxed()),
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
                    (0,0)
                )
                .boxed(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32,(0,0)).boxed()),
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
                    generictypes: Vec::new(),
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
                generictypes: Vec::new(),
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
    test 3
"#,
        );
        let module = parser.module("test".to_string());
        let module = super::TypedModuleDeclaration::from(module, &HashMap::new());
        let [generic, main] = &module.declarations[..] else { unreachable!() };
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 12),
                is_op: false,
                ident: "test".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (1, 17)
                }],
                value: TypedValueType::Expr(TypedExpr::ValueRead(
                    "a".to_string(),
                    ResolvedType::Generic {
                        name: "T".to_string()
                    },
                    (1,30)
                )),
                ty: ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string()
                    }
                    .boxed(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string()
                    }
                    .boxed()
                },
                generictypes: vec!["T".to_string()],
                is_curried: false,
            }),
            generic,
            "generic"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (3, 5),
                is_op: false,
                ident: "main".to_string(),
                args: vec![ArgDeclation {
                    ident: "_".to_string(),
                    loc: (3, 10)
                }],
                value: TypedValueType::Function(vec![TypedStatement::FnCall(TypedFnCall {
                    loc: (4, 5),
                    value: TypedExpr::ValueRead(
                        "test".to_string(),
                        ResolvedType::Function {
                            arg: ResolvedType::Generic {
                                name: "T".to_string()
                            }
                            .boxed(),
                            returns: ResolvedType::Generic {
                                name: "T".to_string()
                            }
                            .boxed()
                        },
                        (4,5)
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
                        name: "T".to_string()
                    },
                    arg_t: types::INT32
                })]),
                ty: ResolvedType::Function {
                    arg: types::UNIT.boxed(),
                    returns: types::UNIT.boxed()
                },
                generictypes: Vec::new(),
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
                    generics: vec!["T".to_string(), "U".to_string()],
                    fields: vec![
                        crate::ast::FieldDecl {
                            name: "first".to_string(),
                            ty: ResolvedType::Generic {
                                name: "T".to_string()
                            },
                            loc: (2, 5)
                        },
                        crate::ast::FieldDecl {
                            name: "second".to_string(),
                            ty: ResolvedType::Generic {
                                name: "U".to_string()
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
                loc: (6, 5),
                is_op: false,
                ident: "first".to_string(),
                args: vec![ArgDeclation {
                    loc: (6, 11),
                    ident: "a".to_string(),
                }],
                value: TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::IntegerLiteral {
                        value: "0".to_string(),
                        size: types::IntWidth::ThirtyTwo
                    },
                    (7, 5)
                )]),
                ty: ResolvedType::Function {
                    arg: ResolvedType::User {
                        name: "Tuple<int32,float64>".to_string(),
                        generics: Vec::new()
                    }
                    .boxed(),
                    returns: types::INT32.boxed()
                },
                generictypes: Vec::new(),
                is_curried: false,
            }),
            "post lowering function"
        );

        assert_eq!(
            generated_strct,
            &TypedDeclaration::TypeDefinition(ResolvedTypeDeclaration::Struct(StructDefinition {
                ident: "Tuple<int32,float64>".to_string(),
                generics: vec![],
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
    return a

let main _ : int32 -> int32 =
    test 3
    return 0
"#,
        ));
        let module = parser.module("test".to_string());
        let mut module = TypedModuleDeclaration::from(module, &HashMap::new());
        module.lower_generics(&HashMap::new());
        let [generic, main, generated] = &module.declarations[..] else { unreachable!("should have three when done")};
        assert_eq!(
            generic,
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 12),
                is_op: false,
                ident: "test".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (1, 17)
                }],
                value: TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::ValueRead(
                        "a".to_string(),
                        ResolvedType::Generic {
                            name: "T".to_string()
                        },
                        (2,12)
                    ),
                    (2, 5)
                )]),
                ty: ResolvedType::Function {
                    arg: ResolvedType::Generic {
                        name: "T".to_string()
                    }
                    .boxed(),
                    returns: ResolvedType::Generic {
                        name: "T".to_string()
                    }
                    .boxed()
                },
                generictypes: vec!["T".to_string()],
                is_curried: false,
            }),
            "generic should be untouched"
        );
        assert_eq!(
            main,
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (4, 5),
                is_op: false,
                ident: "main".to_string(),
                args: vec![ArgDeclation {
                    ident: "_".to_string(),
                    loc: (4, 10)
                }],
                value: TypedValueType::Function(vec![
                    TypedStatement::FnCall(TypedFnCall {
                        loc: (5, 5),
                        value: TypedExpr::ValueRead(
                            "test<int32>".to_string(),
                            ResolvedType::Function {
                                arg: types::INT32.boxed(),
                                returns: types::INT32.boxed()
                            },
                            (5,5)
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
                        (6, 5)
                    )
                ]),
                ty: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                },
                generictypes: Vec::new(),
                is_curried: false,
            }),
            "main should have the value read changed"
        );
        assert_eq!(
            generated,
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 12),
                is_op: false,
                ident: "test<int32>".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (1, 17)
                }],
                value: crate::typed_ast::TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::ValueRead("a".to_string(), types::INT32,(2,12)),
                    (2, 5)
                )]),
                ty: ResolvedType::Function {
                    arg: types::INT32.boxed(),
                    returns: types::INT32.boxed()
                },
                generictypes: Vec::new(),
                is_curried: false,
            }),
            "this should be generated"
        )
    }

    #[test]
    fn control_flow() {
        let parser = Parser::from_source(
            r#"
let expr_with_statement a : bool -> int32 = if a then
        foo 3
        0
    else
        foo 4
        1

let statement_with_else_if a b : bool -> bool -> int32 =
    if a then
        return 0
    else if b then
        return 1
    else
        return 2
"#,
        );
        let mut module = parser.module("test".to_string());
        module.canonialize(vec!["Tests".to_string()]);
        let module = TypedModuleDeclaration::from(module, &PREDEFINED_VALUES);
        let [expr,stmnt] = &module.declarations[..] else {unreachable!("more than two?") };
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (1, 5),
                is_op: false,
                ident: "Tests::test::expr_with_statement".to_string(),
                args: vec![crate::ast::ArgDeclation {
                    loc: (1, 25),
                    ident: "a".to_string()
                }],
                value: TypedValueType::Expr(TypedExpr::IfExpr(TypedIfExpr {
                    cond: TypedExpr::ValueRead("a".to_string(), types::BOOL,(1,48)).boxed(),
                    true_branch: (
                        vec![
                            TypedStatement::FnCall(TypedFnCall {
                                loc: (2,9),
                                value: TypedExpr::ValueRead(
                                    "foo".to_string(), 
                                    ResolvedType::Function { 
                                        arg: types::INT32.boxed(),
                                        returns: types::INT32.boxed()
                                    },
                                    (2,9)
                                ).boxed(),
                                arg: Some(TypedExpr::IntegerLiteral { value: "3".to_string(), size: types::IntWidth::ThirtyTwo }.boxed()),
                                rt: types::INT32,
                                arg_t: types::INT32,
                            })
                        ],
                        TypedExpr::IntegerLiteral { value: "0".to_string(), size: types::IntWidth::ThirtyTwo }.boxed()
                    ),
                    else_ifs: Vec::new(),
                    else_branch: (
                        vec![
                            TypedStatement::FnCall(TypedFnCall {
                                loc: (5,9),
                                value: TypedExpr::ValueRead(
                                    "foo".to_string(), 
                                    ResolvedType::Function { 
                                        arg: types::INT32.boxed(),
                                        returns: types::INT32.boxed()
                                    },
                                    (5,9)
                                ).boxed(),
                                arg: Some(TypedExpr::IntegerLiteral { value: "4".to_string(), size: types::IntWidth::ThirtyTwo }.boxed()),
                                rt: types::INT32,
                                arg_t: types::INT32,
                            })
                        ],
                        TypedExpr::IntegerLiteral { value: "1".to_string(), size: types::IntWidth::ThirtyTwo }.boxed()
                    ),
                    loc: (1,46)
                })),
                ty: ResolvedType::Function { arg: types::BOOL.boxed(), returns: types::INT32.boxed() },
                generictypes: Vec::new(),
                is_curried: false,
            }),
            expr,
            "expression if"
        );
        assert_eq!(
            &TypedDeclaration::Value(TypedValueDeclaration {
                loc: (8, 5),
                is_op: false,
                ident: "Tests::test::statement_with_else_if".to_string(),
                args: vec![
                    crate::ast::ArgDeclation {
                        loc: (8, 28),
                        ident: "a".to_string()
                    },
                    crate::ast::ArgDeclation {
                        loc: (8, 30),
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
                value: TypedValueType::Function(vec![
                    TypedStatement::IfBranching(TypedIfBranching {
                    cond: TypedExpr::ValueRead("a".to_string(),types::BOOL,(9,8)).boxed(),
                    true_branch: vec![TypedStatement::Return(
                        TypedExpr::IntegerLiteral {
                            value:"0".to_string(), 
                            size: types::IntWidth::ThirtyTwo 
                        },
                        (10, 9)
                    )],
                    else_ifs: vec![(
                        TypedExpr::ValueRead("b".to_string(),types::BOOL,(11,13)).boxed(),
                        vec![TypedStatement::Return(
                            TypedExpr::IntegerLiteral {
                                value:"1".to_string(), 
                                size: types::IntWidth::ThirtyTwo 
                            },
                            (12, 9)
                        )]
                    )],
                    else_branch: vec![TypedStatement::Return(
                        TypedExpr::IntegerLiteral {
                            value:"2".to_string(), 
                            size: types::IntWidth::ThirtyTwo 
                        },
                        (14, 9)
                    )]
                })]),
                generictypes: Vec::new(),
                is_curried : false,
            }),
            stmnt,
            "statement if"
        )
    }
}

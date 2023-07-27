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
    pub(crate) name: Option<String>,
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
        Self {
            loc,
            name,
            declarations: declarations
                .into_iter()
                .map(|decl| TypedDeclaration::try_from(decl, &fwd_declares))
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
            locals: HashMap::new(),
            curried_locals: HashMap::new(),
            generated_generics: HashMap::new(),
            args: Vec::new(),
        };
        self.declarations
            .iter_mut()
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
            Self::Mod(module) => module.name.as_ref().unwrap().clone(),
            Self::Value(v) => v.ident.clone(),
            Self::TypeDefinition(_) => todo!(),
        }
    }

    pub(crate) fn try_from(
        data: ast::Declaration,
        known_values: &HashMap<String, ResolvedType>,
    ) -> Result<Self, TypingError> {
        match data {
            Declaration::Mod(module) => Ok(Self::Mod(TypedModuleDeclaration::from(
                module,
                known_values,
            ))),
            Declaration::Value(decl) => Ok(Self::Value(TypedValueDeclaration::try_from(
                decl,
                known_values,
            )?)),
            Declaration::TypeDefinition(_) => todo!(),
        }
    }

    fn replace_types(&mut self, types: &[(String, ResolvedType)]) {
        match self {
            TypedDeclaration::Mod(_) => unreachable!(),
            TypedDeclaration::Value(value) => value.replace_types(types),
            TypedDeclaration::TypeDefinition(_) => todo!(),
        };
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            TypedDeclaration::Mod(_) => todo!(),
            TypedDeclaration::Value(value) => value.lower_generics(context),
            TypedDeclaration::TypeDefinition(def) => def.lower_generics(context),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ResolvedTypeDeclaration {
    Alias(String, ResolvedType),
    // Enum(String, Vec<TypedEnumVariant>, crate::Location),
    Struct(StructDefinition),
}
impl ResolvedTypeDeclaration {
    fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            ResolvedTypeDeclaration::Alias(_, old) => match old {
                ResolvedType::User { name, generics } if !generics.is_empty() => {
                    let resolved_name = name.clone() + "<" + &generics.iter().map(ResolvedType::to_string).join(",") + ">";
                    if !context.generated_generics.contains_key(&resolved_name) {
                        todo!()
                    }
                },
                _ => ()
            },
            ResolvedTypeDeclaration::Struct(_) => todo!(),
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
        known_types: &HashMap<String, ResolvedType>,
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
        let value = match TypedValueType::try_from(value, &known_values) {
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
    ) -> Result<Self, TypingError> {
        match data {
            ast::ValueType::Expr(expr) => {
                TypedExpr::try_from(expr, known_values, Vec::new()).map(|expr| Self::Expr(expr))
            }
            ast::ValueType::Function(stmnts) => {
                let mut output = Vec::with_capacity(stmnts.len());
                let mut known_values = known_values.clone();
                for stmnt in stmnts {
                    match TypedStatement::try_from(stmnt, &known_values) {
                        Ok(stmnt) => {
                            if let TypedStatement::Declaration(data) = &stmnt {
                                let mut e = known_values
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
    Error,
}

impl TypedStatement {
    pub(crate) fn get_loc(&self) -> crate::Location {
        match self {
            TypedStatement::Declaration(decl) => decl.loc,
            TypedStatement::Return(_, loc) => *loc,
            TypedStatement::FnCall(call) => call.loc,
            TypedStatement::Pipe(_) => todo!(),
            TypedStatement::Error => todo!(),
        }
    }
    fn replace_types(&mut self, replaced: &[(String, ResolvedType)]) {
        match self {
            Self::Declaration(data) => data.replace_types(replaced),
            Self::FnCall(data) => data.replace_types(replaced),
            Self::Return(data, _) => data.replace_types(replaced),
            _ => (),
        }
    }

    pub(crate) fn try_from(
        statement: ast::Statement,
        known_values: &HashMap<String, ResolvedType>,
    ) -> Result<Self, TypingError> {
        match statement {
            ast::Statement::Declaration(data) => {
                Ok(match TypedValueDeclaration::try_from(data, known_values) {
                    Ok(d) => Self::Declaration(d),
                    Err(e) => {
                        println!("{:?}", e);
                        Self::Error
                    }
                })
            }
            ast::Statement::Return(value, loc) => Ok(Self::Return(
                TypedExpr::try_from(value, known_values, Vec::new())?,
                loc,
            )),
            ast::Statement::FnCall(data) => {
                Ok(Self::FnCall(TypedFnCall::try_from(data, known_values)?))
            }
            ast::Statement::Pipe(_) => todo!(),
            ast::Statement::StructConstruction(_) => todo!("Convert construction structs")
        }
    }

    fn lower_generics(&mut self, context: &mut LoweringContext) {
        match self {
            Self::Declaration(decl) => match &mut decl.value {
                TypedValueType::Expr(expr) => expr.lower_generics(context),
                TypedValueType::Function(_) => {
                    todo!("how to handle this one :/ function inside function");
                }
                TypedValueType::Err => (),
            },
            Self::Return(expr, _) => expr.lower_generics(context),
            Self::FnCall(call) => call.lower_generics(context),
            Self::Pipe(_) => todo!(),
            Self::Error => todo!(),
        }
    }

    fn get_ty(&self) -> ResolvedType {
        match self {
            Self::Declaration(decl) => decl.ty.clone(),
            Self::Return(expr, _) => expr.get_ty(),
            Self::FnCall(call) => call.rt.clone(),
            Self::Pipe(_) => todo!(),
            Self::Error => todo!(),
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
    /// should match the final return type as [`rhs`]
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
    ) -> Result<Self, TypingError> {
        let ast::FnCall { loc, value, arg } = data;
        let value = ok_or_err_node(TypedExpr::try_from(*value, known_values, Vec::new()));

        let arg = arg.map(
            |arg| match TypedExpr::try_from(*arg, known_values, Vec::new()) {
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
    /// `!a`
    UnaryOpCall(TypedUnaryOpCall),
    /// `foo bar`
    FnCall(TypedFnCall),
    /// basically an ident on it's own
    /// (ident,type)
    ValueRead(String, ResolvedType),
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

    // This is used to allow to continue type checking.  should never naturally generate.
    ErrorNode,
}

impl TypedExpr {
    pub(crate) fn try_from(
        value: ast::Expr,
        known_values: &HashMap<String, ResolvedType>,
        already_processed_args: Vec<ResolvedType>,
    ) -> Result<Self, TypingError> {
        use ast::Expr;
        match value {
            Expr::NumericLiteral { value, ty } if matches!(ty, ResolvedType::Int { .. }) => {
                let ResolvedType::Int { width, ..} = ty else { unreachable!() };
                Ok(Self::IntegerLiteral { value, size: width })
            }
            Expr::NumericLiteral { value, ty } if matches!(ty, ResolvedType::Float { width }) => {
                let ResolvedType::Float { width} = ty else { unreachable!() };

                Ok(Self::FloatLiteral { value, size: width })
            }
            Expr::NumericLiteral { .. } => unreachable!(),
            Expr::StringLiteral(value) => Ok(Self::StringLiteral(value)),
            Expr::CharLiteral(value) => Ok(Self::CharLiteral(value)),
            Expr::UnitLiteral => Ok(Self::UnitLiteral),
            Expr::Compose { lhs, rhs } => todo!(),
            Expr::BinaryOpCall(data) => Ok(Self::BinaryOpCall(TypedBinaryOpCall::try_from(
                data,
                known_values,
            )?)),
            Expr::UnaryOpCall(_) => todo!(),
            Expr::FnCall(data) => Ok(Self::FnCall(TypedFnCall::try_from(data, known_values)?)),
            Expr::ValueRead(value) => {
                if !known_values.contains_key(&value) {
                    Err(TypingError::UnknownType)
                } else {
                    let ty = known_values[&value].clone();
                    Ok(Self::ValueRead(value, ty))
                }
            }
            Expr::ArrayLiteral { contents, loc } => {
                let contents = contents
                    .into_iter()
                    .map(|value| TypedExpr::try_from(value, known_values, Vec::new()))
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
            Expr::ListLiteral { contents, loc } => todo!(),
            Expr::TupleLiteral { contents, loc } => todo!(),
            Expr::StructConstruction(_) => todo!("convert struct construction"),
        }
    }

    fn replace_value_name(
        &mut self,
        old_name: &str,
        new_name: &String,
        replaced: &Vec<(String, ResolvedType)>,
    ) {
        match self {
            Self::BinaryOpCall(b) => {
                b.lhs.replace_value_name(old_name, new_name, replaced);
                b.rhs.replace_value_name(old_name, new_name, replaced);
            }
            Self::UnaryOpCall(u) => u.operand.replace_value_name(old_name, new_name, replaced),
            Self::FnCall(call) => {
                match call.arg.as_mut() {
                    Some(arg) => arg.replace_value_name(old_name, new_name, replaced),
                    None => (),
                }
                call.value.replace_value_name(old_name, new_name, replaced);
            }
            Self::ValueRead(ident, ty) if *ident == old_name => {
                *ident = new_name.clone();
                replaced
                    .iter()
                    .for_each(|(name, new_ty)| *ty = ty.replace_generic(&name, new_ty.clone()))
            }
            Self::ArrayLiteral { contents }
            | Self::ListLiteral { contents }
            | Self::TupleLiteral { contents } => {
                for expr in contents.iter_mut() {
                    expr.replace_value_name(old_name, new_name, replaced);
                }
            }
            Self::ErrorNode => todo!(),
            _ => (),
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
            Self::ValueRead(_, ty) => ty.clone(),
            Self::ArrayLiteral { contents } => ResolvedType::Array {
                underlying: contents.first().unwrap().get_ty().boxed(),
                size: contents.len(),
            },
            Self::ListLiteral { contents } => todo!(),
            Self::TupleLiteral { contents } => todo!(),
            Self::ErrorNode => types::ERROR,
        }
    }

    fn collect_args(
        &self,
        known_values: &HashMap<String, TypedValueDeclaration>,
    ) -> Vec<ResolvedType> {
        if let Self::FnCall(TypedFnCall { value, arg_t, .. }) = self {
            let mut args = value.collect_args(known_values);
            args.push(arg_t.clone());
            args
        } else if let Self::ValueRead(ident, _) = self {
            match known_values.get(ident) {
                Some(decl) => match &decl.value {
                    TypedValueType::Expr(expr) if matches!(expr, TypedExpr::FnCall(_)) => {
                        expr.collect_args(known_values)
                    }
                    _ => vec![],
                },
                None => vec![],
            }
        } else {
            vec![]
        }
    }

    fn replace_type(&mut self, name: &str, new_ty: &ResolvedType) {
        match self {
            Self::BinaryOpCall(data) => data.replace_type(name, new_ty),
            Self::UnaryOpCall(_) => todo!(),
            Self::FnCall(data) => data.replace_type(name, new_ty),
            Self::ValueRead(_, ty) => *ty = ty.replace_generic(name, new_ty.clone()),
            Self::ArrayLiteral { contents } => contents
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty)),
            Self::ListLiteral { contents } => contents
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty)),
            Self::TupleLiteral { contents } => contents
                .iter_mut()
                .for_each(|it| it.replace_type(name, new_ty)),
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
            Self::ValueRead(ident, ty) if ty.is_function() && ty.is_generic() => {
                let args = context.args.iter().cloned().collect();
                context.args = Vec::new(); //moving the arg count out due to this is the value of the function call
                let (fun_t, mut replaced) = map_types_to_args(ty.clone(), args);
                replaced.dedup_by_key(|it| it.0.clone()); // should do nothing but I am being overly cautious
                let new_name =
                    ident.clone() + "_" + &replaced.iter().map(|(_, ty)| ty.to_string()).join("_");
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
            Self::IntegerLiteral { .. }
            | Self::FloatLiteral { .. }
            | Self::StringLiteral(_)
            | Self::CharLiteral(_)
            | Self::UnitLiteral => (),
            Self::BinaryOpCall(data) => data.lower_generics(context),
            Self::UnaryOpCall(data) => todo!("unary op type lowering"),
            Self::ValueRead(_, _) => (),
            Self::ErrorNode => (),
        }
    }
}

struct LoweringContext {
    globals: HashMap<String, TypedDeclaration>,
    locals: HashMap<String, TypedValueDeclaration>,
    curried_locals: HashMap<String, TypedExpr>,
    generated_generics: HashMap<String, TypedDeclaration>,
    args: Vec<ResolvedType>,
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
    to_lower.replace_types(&replaced);
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
        let Self {
            lhs,
            rhs,
            operator,
            rt,
            ..
        } = self;
        let mut old_args = Vec::new();
        std::mem::swap(&mut context.args, &mut old_args);
        lhs.lower_generics(context);
        rhs.lower_generics(context);
        self.reeval_rt();
    }

    fn replace_type(&mut self, name: &str, new_ty: &ResolvedType) {
        let Self {
            lhs,
            rhs,
            operator,
            rt,
            ..
        } = self;
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
                            let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = lhs else { unreachable!() };
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
    ) -> Result<Self, TypingError> {
        let ast::BinaryOpCall {
            loc,
            lhs,
            rhs,
            operator,
        } = value;
        let lhs = match TypedExpr::try_from(*lhs, known_values, Vec::new()) {
            Ok(lhs) => lhs,
            Err(e) => {
                println!("{:?}", e);
                TypedExpr::ErrorNode
            }
        };
        let rhs = match TypedExpr::try_from(*rhs, known_values, Vec::new()) {
            Ok(rhs) => rhs,
            Err(e) => {
                println!("{:?}", e);
                TypedExpr::ErrorNode
            }
        };
        match operator.as_str() {
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
                                let ResolvedType::Int { signed : rhs_signed, width : rhs_w } = lhs else { unreachable!() };
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
    OpNotSupported, //temp.
    UnknownType,
    DoubleTyped,
    ArgTypeMismatch,
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::TypedExpr;
    use crate::ast::ArgDeclation;
    use crate::typed_ast::{
        StructDefinition, TypedDeclaration, TypedFnCall, TypedModuleDeclaration, TypedStatement,
        TypedValueDeclaration, TypedValueType,
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
    fn expr_convert() {
        use crate::ast::{self, Expr};
        assert_eq!(
            TypedExpr::try_from(
                Expr::NumericLiteral {
                    value: "1".to_string(),
                    ty: types::INT32,
                },
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
                Vec::new()
            )
            .expect(""),
            TypedExpr::CharLiteral("a".to_string()),
            "chars"
        );

        assert_eq!(
            TypedExpr::try_from(Expr::UnitLiteral, &HashMap::new(), Vec::new()).expect(""),
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
                Expr::ValueRead("bar".to_string()),
                &PREDEFINED_VALUES,
                Vec::new()
            )
            .expect(""),
            TypedExpr::ValueRead("bar".to_string(), types::INT32),
            "reading a value"
        );

        assert_eq!(
            TypedExpr::try_from(
                Expr::FnCall(ast::FnCall {
                    loc: (0, 0),
                    value: Expr::ValueRead("foo".to_string()).boxed(),
                    arg: Some(Expr::ValueRead("bar".to_string()).boxed()),
                }),
                &PREDEFINED_VALUES,
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
                    }
                )
                .boxed(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32).boxed()),
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
                &HashMap::new()
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
                &PREDEFINED_VALUES
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
                &HashMap::new()
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
                Statement::Return(ast::Expr::ValueRead("bar".to_string()), (0, 0)),
                &PREDEFINED_VALUES
            )
            .expect(""),
            TypedStatement::Return(
                TypedExpr::ValueRead("bar".to_string(), types::INT32),
                (0, 0)
            ),
            "return"
        );

        assert_eq!(
            TypedStatement::try_from(
                Statement::FnCall(ast::FnCall {
                    loc: (0, 0),
                    value: ast::Expr::ValueRead("foo".to_string()).boxed(),
                    arg: Some(ast::Expr::ValueRead("bar".to_string()).boxed()),
                }),
                &PREDEFINED_VALUES
            )
            .expect(""),
            TypedStatement::FnCall(super::TypedFnCall {
                loc: (0, 0),
                value: TypedExpr::ValueRead(
                    "foo".to_string(),
                    ResolvedType::Function {
                        arg: types::INT32.boxed(),
                        returns: types::INT32.boxed()
                    }
                )
                .boxed(),
                arg: Some(TypedExpr::ValueRead("bar".to_string(), types::INT32).boxed()),
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
                &HashMap::new()
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
            generic,
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
                    }
                ),),
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
            "generic"
        );
        assert_eq!(
            main,
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
                        }
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
            "main"
        )
    }

    #[test]
    fn structs() {
        use crate::Parser;
        let parser = Parser::from_source(
            r"
for<T,U> type Tuple = {
    first : T
    second : U
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
                            loc: (1, 5)
                        },
                        crate::ast::FieldDecl {
                            name: "second".to_string(),
                            ty: ResolvedType::Generic {
                                name: "U".to_string()
                            },
                            loc: (2, 5)
                        }
                    ],
                    loc: (0, 15)
                }
            )),
            "pre-lowering struct",
        );

        module.lower_generics(&HashMap::new());
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
                        }
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
                            "test_int32".to_string(),
                            ResolvedType::Function {
                                arg: types::INT32.boxed(),
                                returns: types::INT32.boxed()
                            }
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
                ident: "test_int32".to_string(),
                args: vec![ArgDeclation {
                    ident: "a".to_string(),
                    loc: (1, 17)
                }],
                value: crate::typed_ast::TypedValueType::Function(vec![TypedStatement::Return(
                    TypedExpr::ValueRead("a".to_string(), types::INT32),
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
}

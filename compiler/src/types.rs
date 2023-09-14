use std::{collections::HashMap, mem::size_of};

use crate::{typed_ast, util::ExtraUtilFunctions};
use inkwell::{
    context::Context,
    debug_info::{DIFile, DIFlags, DIFlagsConstants, DISubroutineType, DIType, DebugInfoBuilder},
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    AddressSpace,
};

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum IntWidth {
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
}

impl IntWidth {
    pub fn from_str(s: &str) -> Self {
        match s.as_ref() {
            "8" => Self::Eight,
            "16" => Self::Sixteen,
            "32" => Self::ThirtyTwo,
            "64" => Self::SixtyFour,
            _ => unimplemented!("custom width ints not supported currently"),
        }
    }

    fn as_bits(&self) -> u64 {
        match self {
            Self::Eight => 8,
            Self::Sixteen => 16,
            Self::ThirtyTwo => 32,
            Self::SixtyFour => 64,
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Eight => "8".to_string(),
            Self::Sixteen => "16".to_string(),
            Self::ThirtyTwo => "32".to_string(),
            Self::SixtyFour => "64".to_string(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum FloatWidth {
    ThirtyTwo,
    SixtyFour,
}
impl FloatWidth {
    pub fn from_str(s: &str) -> Self {
        match s.as_ref() {
            "32" => Self::ThirtyTwo,
            "64" => Self::SixtyFour,
            _ => unimplemented!("custom width floats not supported"),
        }
    }

    fn as_bits(&self) -> u64 {
        match self {
            Self::ThirtyTwo => 32,
            Self::SixtyFour => 64,
        }
    }

    pub(crate) fn to_string(&self) -> String {
        match self {
            Self::ThirtyTwo => "32".to_string(),
            Self::SixtyFour => "64".to_string(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum ResolvedType {
    Int {
        signed: bool,
        width: IntWidth,
    },
    Float {
        width: FloatWidth,
    },
    Char,
    Str,
    //probably going to be implimented as a null checked pointer (at time of creation).
    Ref {
        underlining: Box<ResolvedType>,
    },
    Pointer {
        underlining: Box<ResolvedType>,
    },
    Unit,
    //used as rt only
    Void,
    Slice {
        underlining: Box<ResolvedType>,
    },
    Function {
        arg: Box<ResolvedType>,
        returns: Box<ResolvedType>,
    },
    User {
        name: String,
        generics: Vec<ResolvedType>,
    },

    Array {
        underlining: Box<ResolvedType>,
        size: usize,
    },
    // ForwardUser{name:String},
    Alias {
        actual: Box<ResolvedType>,
    },
    Generic {
        name: String,
    },
    Error,
}

impl ResolvedType {
    pub fn is_void_or_unit(&self) -> bool {
        match self {
            Self::Void | Self::Unit => true,
            _ => false,
        }
    }

    pub fn is_user(&self) -> bool {
        matches!(self, Self::User { .. })
    }

    pub fn as_c_function(&self) -> (Vec<Self>, Self) {
        // (args, return type)
        match self {
            Self::Function { arg, returns } => {
                let (mut args, rt) = returns.as_c_function();
                args.push(arg.as_ref().clone());
                (args, rt)
            }
            _ => (Vec::new(), self.clone()),
        }
    }

    pub fn replace_generic(&self, name: &str, new_ty: Self) -> Self {
        match self {
            Self::Generic { name: old_name } if old_name == name => new_ty,
            Self::Function { arg, returns } => Self::Function {
                arg: Box::new(arg.replace_generic(name, new_ty.clone())),
                returns: Box::new(returns.replace_generic(name, new_ty)),
            },
            _ => self.clone(),
        }
    }

    fn find_first_generic_arg(&self) -> String {
        match self {
            Self::Function { arg, returns } => {
                if arg.is_generic() {
                    arg.find_first_generic_arg()
                } else {
                    returns.find_first_generic_arg()
                }
            }
            Self::Generic { name } => name.clone(),
            _ => unreachable!(),
        }
    }

    pub fn replace_first_generic(self, new_ty: Self) -> Self {
        let name = self.find_first_generic_arg();
        self.replace_generic(&name, new_ty)
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Self::Function { arg, returns } => arg.is_generic() || returns.is_generic(),
            Self::Alias { actual } => actual.is_generic(),
            Self::Slice { underlining }
            | Self::Pointer { underlining }
            | Self::Ref { underlining } => underlining.is_generic(),
            Self::Generic { .. } => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function { .. } => true,
            Self::Alias { actual } => actual.is_function(),
            Self::Pointer { underlining } | Self::Ref { underlining } => underlining.is_function(),
            _ => false,
        }
    }

    pub(crate) fn replace_user_with_generic(self, target_name: &str) -> Self {
        match self {
            ResolvedType::Ref { underlining } => Self::Ref {
                underlining: underlining.replace_user_with_generic(target_name).boxed(),
            },
            ResolvedType::Pointer { underlining } => Self::Ref {
                underlining: underlining.replace_user_with_generic(target_name).boxed(),
            },
            ResolvedType::Slice { underlining } => Self::Slice {
                underlining: underlining.replace_user_with_generic(target_name).boxed(),
            },
            ResolvedType::Function { arg, returns } => Self::Function {
                arg: arg.replace_user_with_generic(target_name).boxed(),
                returns: returns.replace_user_with_generic(target_name).boxed(),
            },
            ResolvedType::Array {
                underlining: underlying,
                size,
            } => Self::Array {
                underlining: underlying.replace_user_with_generic(target_name).boxed(),
                size,
            },
            ResolvedType::User { name, .. } if &name == target_name => {
                ResolvedType::Generic { name }
            }
            _ => self,
        }
    }

    pub(crate) fn lower_generics(&mut self, context: &mut typed_ast::LoweringContext) {
        match self {
            ResolvedType::Int { .. }
            | ResolvedType::Float { .. }
            | ResolvedType::Char
            | ResolvedType::Str
            | ResolvedType::Unit
            | ResolvedType::Generic { .. }
            | ResolvedType::Void => (),
            ResolvedType::Pointer { underlining }
            | ResolvedType::Slice { underlining }
            | ResolvedType::Array { underlining, .. }
            | ResolvedType::Alias {
                actual: underlining,
            }
            | ResolvedType::Ref { underlining } => underlining.as_mut().lower_generics(context),
            ResolvedType::Function { arg, returns } => {
                arg.as_mut().lower_generics(context);
                returns.as_mut().lower_generics(context);
            }
            ResolvedType::User { name, generics } => {
                if generics.iter().any(ResolvedType::is_generic) || generics.len() == 0 {
                    return;
                }
                let new_name = format!(
                    "{}<{}>",
                    name.clone(),
                    generics.iter().map(ResolvedType::to_string).join(",")
                );
                if !context.generated_generics.contains_key(&new_name) {
                    let mut target = context.globals.get(name).unwrap().clone();
                    let zipped = target
                        .get_generics()
                        .into_iter()
                        .zip(generics.iter().cloned())
                        .collect_vec();
                    target.replace_types(&zipped, context);
                    target.lower_generics(context);
                    let _ = context.generated_generics.insert(new_name.clone(), target);
                }
                *name = new_name;
                *generics = Vec::new();
            }
            ResolvedType::Error => (),
        }
    }

    pub fn replace(&self, nice_name: &str, actual: &str) -> Self {
        match self {
            Self::User { name, generics } => {
                if name == nice_name {
                    Self::User {
                        name: actual.to_string(),
                        generics: generics.clone(),
                    }
                } else {
                    Self::User {
                        name: name.clone(),
                        generics: generics
                            .iter()
                            .map(|it| it.replace(nice_name, actual))
                            .collect(),
                    }
                }
            }
            Self::Function { arg, returns } => Self::Function {
                arg: arg.replace(nice_name, actual).boxed(),
                returns: returns.replace(nice_name, actual).boxed(),
            },
            _ => self.clone(),
        }
    }

    pub(crate) fn remove_args(&self,arg:usize) -> Self {
        if arg == 0 {
            self.clone()
        } else {
            let Self::Function { returns, .. } = self else { unreachable!() };
            returns.remove_args(arg-1)
        }
    }
}

impl ToString for ResolvedType {
    fn to_string(&self) -> String {
        match self {
            ResolvedType::Alias { actual } => actual.to_string(),
            ResolvedType::Char => "char".to_string(),
            ResolvedType::Float { width } => "float".to_string() + &width.to_string(),
            ResolvedType::Function { arg, returns } => {
                let arg = if arg.is_function() {
                    format!("({})", arg.to_string())
                } else {
                    arg.to_string()
                };
                format!("{}->{}", arg, returns.to_string())
            }
            ResolvedType::Generic { .. } => {
                unreachable!("why are you trying to serialize a generic type?")
            }
            ResolvedType::Int {
                signed: false,
                width,
            } => "uint".to_string() + &width.to_string(),
            ResolvedType::Int {
                signed: true,
                width,
            } => "int".to_string() + &width.to_string(),
            ResolvedType::Pointer { underlining } => "p_".to_string() + &underlining.to_string(),
            ResolvedType::Ref { .. } | ResolvedType::Slice { .. } => {
                todo!("how to serialize this")
            }
            ResolvedType::Str => "str".to_string(),
            ResolvedType::Unit => "()".to_string(),
            ResolvedType::Void => "".to_string(),
            ResolvedType::User { name, generics } => {
                if generics.len() > 0 {
                    format!(
                        "{}<{}>",
                        name,
                        &generics.iter().map(Self::to_string).join(",")
                    )
                } else {
                    name.clone()
                }
            }
            ResolvedType::Array {
                underlining: underlying,
                size,
            } => {
                format!("[{};{}]", underlying.to_string(), size)
            }
            ResolvedType::Error => "<ERROR>".to_string(),
        }
    }
}
#[allow(unused)]
mod consts {
    use super::*;
    pub const ERROR: ResolvedType = ResolvedType::Error;
    pub const INT8: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::Eight,
    };
    pub const INT16: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::Sixteen,
    };
    pub const INT32: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::ThirtyTwo,
    };
    pub const INT64: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::SixtyFour,
    };
    pub const UINT8: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::Eight,
    };
    pub const UINT16: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::Sixteen,
    };
    pub const UINT32: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::ThirtyTwo,
    };
    pub const UINT64: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::SixtyFour,
    };
    pub const FLOAT32: ResolvedType = ResolvedType::Float {
        width: FloatWidth::ThirtyTwo,
    };
    pub const FLOAT64: ResolvedType = ResolvedType::Float {
        width: FloatWidth::SixtyFour,
    };
    pub const STR: ResolvedType = ResolvedType::Str;
    pub const CHAR: ResolvedType = ResolvedType::Char;
    pub const UNIT: ResolvedType = ResolvedType::Unit;
}
pub use consts::*;
use itertools::Itertools;

#[derive(Clone)]
pub struct TypeResolver<'ctx> {
    known: HashMap<ResolvedType, AnyTypeEnum<'ctx>>,
    ctx: &'ctx Context,
    ditypes: HashMap<ResolvedType, DIType<'ctx>>,
}

impl<'ctx> TypeResolver<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        let unit = ctx.const_struct(&[], false);
        let unit_t = unit.get_type();
        let char_t = ctx.i8_type();
        let str_t = ctx.opaque_struct_type("str");
        let char_ptr_t = char_t.ptr_type(AddressSpace::default());
        str_t.set_body(&[char_ptr_t.into(), char_ptr_t.into()], false);
        let i8_t = ctx.i8_type();
        let i16_t = ctx.i16_type();
        let i32_t = ctx.i32_type();
        let i64_t = ctx.i64_type();
        let f32_t = ctx.f32_type();
        let f64_t = ctx.f64_type();
        let known = {
            let mut out = HashMap::new();
            out.insert(ResolvedType::Void, ctx.void_type().as_any_type_enum());
            out.insert(ResolvedType::Char, char_t.as_any_type_enum());
            out.insert(ResolvedType::Unit, unit_t.as_any_type_enum());
            out.insert(ResolvedType::Str, str_t.as_any_type_enum());
            out.insert(
                ResolvedType::Float {
                    width: FloatWidth::ThirtyTwo,
                },
                f32_t.as_any_type_enum(),
            );
            out.insert(
                ResolvedType::Float {
                    width: FloatWidth::SixtyFour,
                },
                f64_t.as_any_type_enum(),
            );
            out.insert(INT8, i8_t.as_any_type_enum());
            out.insert(INT16, i16_t.as_any_type_enum());
            out.insert(INT32, i32_t.as_any_type_enum());
            out.insert(INT64, i64_t.as_any_type_enum());
            out.insert(UINT8, i8_t.as_any_type_enum());
            out.insert(UINT16, i16_t.as_any_type_enum());
            out.insert(UINT32, i32_t.as_any_type_enum());
            out.insert(UINT64, i64_t.as_any_type_enum());
            out
        };
        Self {
            known,
            ctx,
            ditypes: HashMap::new(),
        }
    }

    pub fn has_type(&self, ty: &ResolvedType) -> bool {
        self.known.contains_key(ty)
    }

    // pub fn insert_many_user_types(&mut self,types:Vec<(String,Vec<ResolvedType>)>) -> Vec<Result<AnyTypeEnum,Vec<InsertionError>>> {
    //     let mut out = types.drain_filter(|(name,))
    //     let names = types.iter().map(|(name,_)| name).collect_vec();
    //     let mut handle_last = types.drain_filter(|(body,))
    // }

    // pub fn insert_user_type(&mut self,name:String,body:Vec<ResolvedType>) -> Result<AnyTypeEnum,Vec<InsertionError>> {

    //     if self.known.contains_key(&ResolvedType::User { name : name.clone() }){
    //         return Err(vec![InsertionError::Redeclaration]);
    //     }
    //     let body : Vec<Result<BasicTypeEnum>> = body.into_iter().map(|ty| {
    //         self.resolve_type_basic(ty).ok_or(InsertionError)
    //     }).collect();
    //     let user = self.ctx.opaque_struct_type(&name);
    //     let body : Vec<_> = body.into_iter().map(Option::unwrap).collect();
    //     user.set_body(&body, false);
    //     self.known.insert(ResolvedType::User { name },user.as_any_type_enum()).ok_or(InsertionError::UnknownType)
    // }

    fn resolve_type(&mut self, ty: ResolvedType) {
        if self.has_type(&ty) {
            return;
        }
        match dbg!(&ty) {
            ResolvedType::Ref { ref underlining } | ResolvedType::Pointer { ref underlining } => {
                if let ResolvedType::Function { .. } = underlining.as_ref() {
                    let result = self
                        .resolve_type_as_any(underlining.as_ref().clone())
                        .into_function_type()
                        .ptr_type(AddressSpace::default())
                        .as_any_type_enum();
                    self.known.insert(ty, result);
                } else {
                    let result = self
                        .resolve_type_as_basic(underlining.as_ref().clone())
                        .ptr_type(AddressSpace::default())
                        .as_any_type_enum();
                    self.known.insert(ty, result);
                }
            }
            ResolvedType::Slice { ref underlining } => {
                let underlining = self
                    .resolve_type_as_basic(underlining.as_ref().clone())
                    .ptr_type(AddressSpace::default());
                self.known.insert(
                    ty,
                    self.ctx
                        .struct_type(&[underlining.into(), underlining.into()], false)
                        .as_any_type_enum(),
                );
            }
            ResolvedType::Function { .. } => {
                let r = self
                    .ctx
                    .struct_type(
                        &[self.resolve_type_as_basic(ResolvedType::Pointer {
                            underlining: INT8.boxed(),
                        })],
                        false,
                    )
                    .as_any_type_enum();
                self.known.insert(ty, r);
            }
            ResolvedType::User { name, generics }
                if generics
                    .iter()
                    .map(ResolvedType::is_generic)
                    .all(|it| it == false)
                    || generics.is_empty() =>
            {
                let new_name = if generics.is_empty() {
                    name.clone()
                } else {
                    format!(
                        "{}<{}>",
                        name,
                        generics.iter().map(ResolvedType::to_string).join(",")
                    )
                };
                match self.ctx.get_struct_type(&new_name) {
                    None => {
                        let strct = self.ctx.opaque_struct_type(&new_name);
                        self.known.insert(ty.clone(), strct.as_any_type_enum());
                    }
                    Some(strct) => {
                        self.known.insert(ty.clone(), strct.as_any_type_enum());
                    }
                }
            }
            // ResolvedType::ForwardUser { name } => todo!(),
            ResolvedType::Alias { actual } => self.resolve_type(actual.as_ref().clone()),
            ResolvedType::Unit => (),
            ResolvedType::Generic { .. } =>
            /* not sure what I need to do here yet */
            {
                ()
            }

            _ => unimplemented!(),
        }
    }

    pub fn resolve_arg_type(&mut self, ty: &ResolvedType) -> BasicTypeEnum<'ctx> {
        self.resolve_type(ty.clone());
        if let ResolvedType::Function { .. } = ty {
            let i8_ptr = self
                .known
                .get(&INT8)
                .unwrap()
                .into_int_type()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum();
            self.ctx
                .struct_type(&[i8_ptr.into()], false)
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum()
        } else if ty == &ResolvedType::Str || ty.is_user() {
            self.resolve_type_as_basic(ty.clone())
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum()
        } else {
            self.resolve_type_as_basic(ty.clone())
        }
    }

    pub fn resolve_type_as_function(&mut self, ty: &ResolvedType) -> FunctionType<'ctx> {
        let ResolvedType::Function { arg, returns } = ty else { unreachable!("trying to make a non function type into a function") };
        let arg = self.resolve_arg_type(&arg);
        let curry_holder = self
            .ctx
            .struct_type(
                &[self.ctx.i8_type().ptr_type(AddressSpace::default()).into()],
                false,
            )
            .ptr_type(AddressSpace::default());
        if returns.is_void_or_unit() {
            return self
                .ctx
                .void_type()
                .fn_type(&[curry_holder.into(), arg.into()], false);
        }

        if returns.is_user() {
            return self.ctx
                .void_type()
                .fn_type(&[
                    curry_holder.into(),
                    self.resolve_type_as_basic(ResolvedType::Pointer { underlining: returns.clone() }).into(),
                    arg.into()
                ], false);
        }
        let rt = if returns.is_function() {
            curry_holder.as_basic_type_enum()
        } else {
            self.resolve_type_as_basic(returns.as_ref().clone())
        };
        rt.fn_type(&[curry_holder.into(), arg.into()], false)
    }

    pub fn resolve_type_as_basic(&mut self, ty: ResolvedType) -> BasicTypeEnum<'ctx> {
        self.resolve_type(ty.clone());
        self.known
            .get(&ty)
            .map(|ty| match ty {
                AnyTypeEnum::ArrayType(ty) => ty.as_basic_type_enum(),
                AnyTypeEnum::FloatType(ty) => ty.as_basic_type_enum(),
                AnyTypeEnum::FunctionType(ty) => {
                    ty.ptr_type(AddressSpace::default()).as_basic_type_enum()
                }
                AnyTypeEnum::IntType(ty) => ty.as_basic_type_enum(),
                AnyTypeEnum::PointerType(ty) => ty.as_basic_type_enum(),
                AnyTypeEnum::StructType(ty) => ty.as_basic_type_enum(),
                AnyTypeEnum::VectorType(ty) => ty.as_basic_type_enum(),
                AnyTypeEnum::VoidType(_) => unreachable!(),
            })
            .unwrap()
    }

    pub fn resolve_type_as_any(&mut self, ty: ResolvedType) -> AnyTypeEnum<'ctx> {
        self.resolve_type(ty.clone());
        *self.known.get(&ty).unwrap()
    }
}

impl ResolvedType {
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float { .. })
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int { .. })
    }
}

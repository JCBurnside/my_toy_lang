use std::collections::{HashMap, HashSet};

use crate::{ast::TypeName, util::ExtraUtilFunctions};
use inkwell::{
    context::Context,
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum},
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
    },

    Array {
        underlying : Box<ResolvedType>,
        size : usize,
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
    pub fn replace_generic(self, name: &str, new_ty: Self) -> Self {
        match self {
            Self::Generic { name: old_name } if old_name == name => new_ty,
            Self::Function { arg, returns } => Self::Function {
                arg: Box::new(arg.replace_generic(name, new_ty.clone())),
                returns: Box::new(returns.replace_generic(name, new_ty)),
            },
            _ => self,
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

    pub(crate) fn to_string(&self) -> String {
        match self {
            ResolvedType::Alias { actual } => actual.to_string(),
            ResolvedType::Char => "char".to_string(),
            ResolvedType::Float { width } => "float".to_string() + &width.to_string(),
            ResolvedType::Function { arg, returns } => todo!("how to serialize this"),
            ResolvedType::Generic { name } => {
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
            ResolvedType::Ref { underlining } | ResolvedType::Slice { underlining } => {
                todo!("how to serialize this")
            }
            ResolvedType::Str => "str".to_string(),
            ResolvedType::Unit => "()".to_string(),
            ResolvedType::Void => "".to_string(),
            ResolvedType::User { name } => name.clone(),
            ResolvedType::Array { underlying, size } => format!("[{};{}]",underlying.to_string(),size),
            ResolvedType::Error => "<ERROR>".to_string(),
        }
    }
}

#[allow(unused)]
mod consts {
    use super::*;
    pub const ERROR : ResolvedType = ResolvedType::Error;
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
pub fn resolve_from_name(name: TypeName, known_generics: &HashSet<String>) -> ResolvedType {
    match name {
        TypeName::FnType(arg, rt) => {
            let arg = match arg.as_ref() {
                TypeName::ValueType(_) => {
                    resolve_from_name(arg.as_ref().clone(), known_generics).boxed()
                }
                TypeName::FnType(_, _) => ResolvedType::Pointer {
                    underlining: resolve_from_name(arg.as_ref().clone(), known_generics).boxed(),
                }
                .boxed(),
            };
            let returns = match rt.as_ref() {
                TypeName::ValueType(v) if v == "()" => ResolvedType::Void.boxed(),
                TypeName::ValueType(_) => {
                    resolve_from_name(rt.as_ref().clone(), known_generics).boxed()
                }
                TypeName::FnType(_, _) => ResolvedType::Pointer {
                    underlining: resolve_from_name(rt.as_ref().clone(), known_generics).boxed(),
                }
                .boxed(),
            };
            ResolvedType::Function { arg, returns }
        }
        TypeName::ValueType(ty) => {
            if ty.starts_with("int") {
                ResolvedType::Int {
                    signed: true,
                    width: IntWidth::from_str(&ty[3..]),
                }
            } else if ty.starts_with("uint") {
                ResolvedType::Int {
                    signed: false,
                    width: IntWidth::from_str(&ty[4..]),
                }
            } else if ty.starts_with("float") {
                ResolvedType::Float {
                    width: FloatWidth::from_str(&ty[5..]),
                }
            } else if ty == "char" {
                ResolvedType::Char
            } else if ty == "str" {
                ResolvedType::Str
            } else if ty == "()" {
                ResolvedType::Unit
            } else if known_generics.contains(&ty) {
                ResolvedType::Generic { name: ty }
            } else {
                ResolvedType::User { name: ty }
            }
        }
    }
}

#[derive(Clone)]
pub struct TypeResolver<'ctx> {
    known: HashMap<ResolvedType, AnyTypeEnum<'ctx>>,
    known_generics: HashMap<String, ResolvedType>,
    ctx: &'ctx Context,
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
            known_generics: HashMap::new(),
            ctx,
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
                    let result = self.resolve_type_as_any(underlining.as_ref().clone());
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
            ResolvedType::Function {
                ref arg,
                ref returns,
            } => {
                let arg = self.resolve_type_as_basic(arg.as_ref().clone());
                let arg = if arg.is_struct_type() {
                    arg.into_struct_type()
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum()
                } else {
                    arg
                };
                if returns.as_ref() == &ResolvedType::Unit
                    || returns.as_ref() == &ResolvedType::Void
                {
                    self.known.insert(
                        ty,
                        self.ctx
                            .void_type()
                            .fn_type(&[arg.into()], false)
                            .as_any_type_enum(),
                    );
                } else {
                    let rt = self.resolve_type_as_basic(returns.as_ref().clone());
                    self.known
                        .insert(ty, rt.fn_type(&[arg.into()], false).as_any_type_enum());
                }
            }
            ResolvedType::User { name } => todo!(),
            // ResolvedType::ForwardUser { name } => todo!(),
            ResolvedType::Alias { actual } => todo!(),
            ResolvedType::Unit => (),
            ResolvedType::Generic { .. } =>
            /* not sure what I need to do here yet */
            {
                ()
            }
            _ => unimplemented!(),
        }
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
                AnyTypeEnum::StructType(ty) => dbg!(ty).as_basic_type_enum(),
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

enum InsertionError {
    UnknownType(String),
    Recursive,
    Circlular(String),
    Redeclaration,
}
impl ResolvedType {
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float { .. })
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int { .. })
    }
}

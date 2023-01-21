use std::{collections::HashMap,vec::Vec};

use inkwell::{
    types::{AnyType,IntType, StructType, AnyTypeEnum, FloatType, BasicTypeEnum, BasicMetadataTypeEnum, BasicType, FunctionType},
    context::Context, 
    values::StructValue, AddressSpace
};
use itertools::Itertools;
use multimap::MultiMap;

use crate::{ast::TypeName, util::ExtraUtilFunctions};

#[derive(Hash,PartialEq, Eq,Clone, Copy, Debug)]
pub enum IntWidth {
    Eight,
    Sixteen,
    ThrityTwo,
    SixtyFour,
}

impl IntWidth {
    pub fn from_str(s:&str) -> Self {
        match s.as_ref() {
            "8" => Self::Eight,
            "16" => Self::Sixteen,
            "32" => Self::ThrityTwo,
            "64" => Self::SixtyFour,
            _ => unimplemented!("custom width ints not supported currently")
        }
    }
}


#[derive(Hash,PartialEq, Eq,Clone, Copy, Debug)]
pub enum FloatWidth {
    ThrityTwo,
    SixtyFour,
}
impl FloatWidth {
    pub fn from_str(s:&str) -> Self {
        match s.as_ref() {
            "32" => Self::ThrityTwo,
            "64" => Self::SixtyFour,
            _ => unimplemented!("custom width floats not supported")
        }
    }
}

#[derive(Hash,PartialEq, Eq, Clone, Debug)]
pub enum ResolvedType {
    Int{signed:bool,width:IntWidth},
    Float{width:FloatWidth},
    Char,
    Str,
    //probably going to be implimented as a null checked pointer (at time of creation).
    Ref{underlining:Box<ResolvedType>},
    Pointer{underlining:Box<ResolvedType>},
    Unit,
    Slice{underlining:Box<ResolvedType>},
    Function{arg:Box<ResolvedType>,returns:Box<ResolvedType>},
    User{name:String},
    // ForwardUser{name:String},
    Alias{actual:Box<ResolvedType>},
    Generic{name:String},
}

impl ResolvedType {
    pub fn replace_generic(self, name:&str, new_ty : Self) -> Self {
        match self {
            Self::Generic { name: old_name } if old_name == name => new_ty,
            Self::Function { arg, returns } => Self::Function { arg: Box::new(arg.replace_generic(name, new_ty.clone())), returns: Box::new(returns.replace_generic(name, new_ty)) },
            _ => self
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Self::Function { arg, returns } => arg.is_generic() || returns.is_generic(),
            Self::Alias { actual } => actual.is_generic(),
            Self::Slice { underlining } 
          | Self::Pointer { underlining } 
          | Self::Ref { underlining } 
                => underlining.is_generic(),
            Self::Generic { .. } => true,
            _ => false,
        }
    }
}

#[allow(unused)]
mod consts {
    use super::*;
    pub const INT8 : ResolvedType = ResolvedType::Int { signed: true, width: IntWidth::Eight };
    pub const INT16 : ResolvedType = ResolvedType::Int { signed: true, width: IntWidth::Sixteen };
    pub const INT32 : ResolvedType = ResolvedType::Int { signed: true, width: IntWidth::ThrityTwo };
    pub const INT64 : ResolvedType = ResolvedType::Int { signed: true, width: IntWidth::SixtyFour };
    pub const UINT8 : ResolvedType = ResolvedType::Int { signed: false, width: IntWidth::Eight };
    pub const UINT16 : ResolvedType = ResolvedType::Int { signed: false, width: IntWidth::Sixteen };
    pub const UINT32 : ResolvedType = ResolvedType::Int { signed: false, width: IntWidth::ThrityTwo };
    pub const UINT64 : ResolvedType = ResolvedType::Int { signed: false, width: IntWidth::SixtyFour };
    pub const FLOAT32 : ResolvedType = ResolvedType::Float { width: FloatWidth::ThrityTwo };
    pub const FLOAT64 : ResolvedType = ResolvedType::Float { width: FloatWidth::SixtyFour };
    pub const STR : ResolvedType = ResolvedType::Str;
    pub const CHAR : ResolvedType = ResolvedType::Char;
    pub const UNIT : ResolvedType = ResolvedType::Unit;
}
pub use consts::*;
pub fn resolve_from_name(name:TypeName) -> ResolvedType {
    match name {
        TypeName::FnType(arg, rt) => {
            let arg = match arg.as_ref() {
                TypeName::ValueType(_)=>resolve_from_name(arg.as_ref().clone()).boxed(),
                TypeName::FnType(_, _) => ResolvedType::Pointer { underlining: resolve_from_name(arg.as_ref().clone()).boxed() }.boxed()
            };
            let returns = match rt.as_ref() {
                TypeName::ValueType(_) => resolve_from_name(rt.as_ref().clone()).boxed(),
                TypeName::FnType(_, _) => ResolvedType::Pointer { underlining: resolve_from_name(rt.as_ref().clone()).boxed() }.boxed()
            };
            ResolvedType::Function { arg, returns }
        },
        TypeName::ValueType(ty) => {
            if ty.starts_with("int") {
                ResolvedType::Int { signed: true, width: IntWidth::from_str(&ty[3..]) }
            } else if ty.starts_with("uint") {
                ResolvedType::Int { signed: false, width: IntWidth::from_str(&ty[4..]) }
            } else if ty.starts_with("float") {
                ResolvedType::Float { width: FloatWidth::from_str(&ty[5..]) }
            } else if ty == "char" {
                ResolvedType::Char
            } else if ty == "str" {
                ResolvedType::Str
            } else if ty == "()" {
                ResolvedType::Unit
            } else {
                ResolvedType::User { name: ty }
            }
        },
    }
}

pub struct TypeResolver<'ctx> {
    known : HashMap<ResolvedType,AnyTypeEnum<'ctx>>,
    ctx : &'ctx Context,
}

impl <'ctx> TypeResolver<'ctx> {
    pub fn new(ctx:&'ctx Context) -> Self {
        let unit = ctx.const_struct(&[], false);
        let unit_t = unit.get_type();
        let char_t = ctx.i8_type(); 
        let str_t = ctx.opaque_struct_type("str");
        let char_ptr_t = char_t.ptr_type(AddressSpace::Generic);
        str_t.set_body(&[char_ptr_t.into(),char_ptr_t.into()], false);
        let i8_t = ctx.i8_type();
        let i16_t = ctx.i16_type();
        let i32_t = ctx.i32_type();
        let i64_t = ctx.i64_type();
        let f32_t = ctx.f32_type();
        let f64_t = ctx.f64_type();
        let known = {
            let mut out = HashMap::new();
            out.insert(ResolvedType::Char, char_t.as_any_type_enum());
            out.insert(ResolvedType::Unit, unit_t.as_any_type_enum());
            out.insert(ResolvedType::Str, str_t.as_any_type_enum());
            out.insert(ResolvedType::Float { width:FloatWidth::ThrityTwo }, f32_t.as_any_type_enum());
            out.insert(ResolvedType::Float { width:FloatWidth::SixtyFour }, f64_t.as_any_type_enum());
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
        }
    }

    pub fn has_type(&self, ty : &ResolvedType)-> bool {
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
    fn resolve_type(&mut self,ty : ResolvedType) {
        if self.has_type(&ty) { return }
        match ty {
            ResolvedType::Ref { ref underlining } 
            | ResolvedType::Pointer { ref underlining } => {
                if let ResolvedType::Function { .. } = underlining.as_ref() {
                    let result = self.resolve_type_as_any(underlining.as_ref().clone()).into_function_type().ptr_type(AddressSpace::Generic).as_any_type_enum();
                    self.known.insert(ty,result);
                } else {
                    let result = self.resolve_type_as_any(underlining.as_ref().clone());
                    self.known.insert(ty, result);
                }
            },
            ResolvedType::Slice { ref underlining } => {
                let underlining = self.resolve_type_as_basic(underlining.as_ref().clone()).ptr_type(AddressSpace::Generic);
                self.known.insert(ty, self.ctx.struct_type(&[underlining.into(),underlining.into()], false).as_any_type_enum());
            },
            ResolvedType::Function { ref arg, ref returns } => {
                let rt = self.resolve_type_as_basic(returns.as_ref().clone());
                let arg = self.resolve_type_as_basic(arg.as_ref().clone());
                self.known.insert(ty, rt.fn_type(&[arg.into()], false).as_any_type_enum());
            },
            ResolvedType::User { name } => todo!(),
            // ResolvedType::ForwardUser { name } => todo!(),
            ResolvedType::Alias { actual } => todo!(),
            _=> unimplemented!()
        }
    }

    pub fn resolve_type_as_basic(&mut self,ty : ResolvedType) -> BasicTypeEnum<'ctx> {
        self.resolve_type(ty.clone());
        self.known.get(&ty).map(|ty| match ty {
            AnyTypeEnum::ArrayType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::FloatType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::FunctionType(ty) => ty.ptr_type(AddressSpace::Generic).as_basic_type_enum(),
            AnyTypeEnum::IntType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::PointerType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::StructType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::VectorType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::VoidType(_) => unreachable!(),
        }).unwrap()
    }

    pub fn resolve_type_as_any(&mut self, ty : ResolvedType) -> AnyTypeEnum<'ctx> {
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
        matches!(self,Self::Float { .. })
    }

    pub fn is_int(&self) -> bool {
        matches!(self,Self::Int{..})
    }
}

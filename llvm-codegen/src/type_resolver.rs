use std::collections::HashMap;

use compiler::types::ResolvedType;
use inkwell::{
    context::Context,
    debug_info::DIType,
    targets::TargetData,
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    AddressSpace,
};

pub struct TypeResolver<'ctx> {
    known: HashMap<ResolvedType, AnyTypeEnum<'ctx>>,
    ctx: &'ctx Context,
    ditypes: HashMap<ResolvedType, DIType<'ctx>>,
    target_data: TargetData,
}

use compiler::types::*;
use itertools::Itertools;

impl<'ctx> TypeResolver<'ctx> {
    pub fn new(ctx: &'ctx Context, target_data: TargetData) -> Self {
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
            out.insert(FLOAT32, f32_t.as_any_type_enum());
            out.insert(FLOAT64, f64_t.as_any_type_enum());
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
            target_data,
        }
    }

    pub fn get_size_in_bits(&mut self, ty: &ResolvedType) -> u64 {
        let ty = self.resolve_type_as_any(ty.clone());
        self.target_data.get_store_size(&ty)
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
        match &ty {
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
                            underlining: Box::new(INT8),
                        })],
                        false,
                    )
                    .as_any_type_enum();
                self.known.insert(ty, r);
            }
            ResolvedType::User {
                name,
                generics,
                loc: _,
            } if generics
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
            ResolvedType::Bool => {
                self.known
                    .insert(BOOL, self.ctx.bool_type().as_any_type_enum());
            }
            // ResolvedType::ForwardUser { name } => todo!(),
            ResolvedType::Alias { actual, loc: _ } => self.resolve_type(actual.as_ref().clone()),
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
        } else if ty == &ResolvedType::Unit {
            self.resolve_type_as_basic(ResolvedType::Pointer {
                underlining: Box::new(UNIT),
            })
        } else {
            self.resolve_type_as_basic(ty.clone())
        }
    }

    pub fn resolve_type_as_function(&mut self, ty: &ResolvedType) -> FunctionType<'ctx> {
        let ResolvedType::Function {
            arg,
            returns,
            loc: _,
        } = ty
        else {
            unreachable!("trying to make a non function type into a function")
        };
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
            return self.ctx.void_type().fn_type(
                &[
                    curry_holder.into(),
                    self.resolve_type_as_basic(ResolvedType::Pointer {
                        underlining: returns.clone(),
                    })
                    .into(),
                    arg.into(),
                ],
                false,
            );
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

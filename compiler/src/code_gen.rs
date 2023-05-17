// use std::cell::RefCell;
// use std::collections::HashMap;
// use std::convert::TryInto;
// use std::iter::once;
// use std::rc::Rc;

// use inkwell::AddressSpace;
// use inkwell::builder::Builder;
// use inkwell::context::Context;
// use inkwell::module::Module;
// use inkwell::types::{BasicTypeEnum, BasicType, AnyTypeEnum};
// use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue, CallableValue};

// use itertools::{Itertools, Either};

// use multimap::MultiMap;

// use crate::typed_ast::TypedExpr;
// use crate::types::{TypeResolver, ResolvedType};

// pub struct CodeGen<'ctx> {
//     ctx: &'ctx Context,
//     module: Module<'ctx>,
//     builder: Builder<'ctx>,
//     type_resolver : TypeResolver<'ctx>,
//     known_functions: HashMap<String, FunctionValue<'ctx>>,
//     _known_ops: MultiMap<String, FunctionValue<'ctx>>,
//     known_values: HashMap<String, BasicValueEnum<'ctx>>,
//     locals : HashMap<String, PointerValue<'ctx>>,
//     incomplete_generics : HashMap<String, Box<dyn FnMut(ResolvedType)->Either<FunctionValue<'ctx>,TypedExpr> + 'ctx>>,
//     // curried_locals : HashMap<String, (PointerValue<'ctx>,ResolvedType)>,
// }

// impl<'ctx> CodeGen<'ctx> {
//     pub fn with_module(
//         ctx: &'ctx Context,
//         module: Module<'ctx>,
//         mut type_resolver : TypeResolver<'ctx>,
//         seed_functions : HashMap<String,ResolvedType>,
//         known_values : HashMap<String, BasicValueEnum<'ctx>>,
//         seed_ops : MultiMap<String,FunctionValue<'ctx>>
//     ) -> Self {
//         let builder = ctx.create_builder();
//         let known_functions = seed_functions
//             .into_iter()
//             .map(|(name,ty)| {
//                 let ty= module
//                     .get_function(&name)
//                     .unwrap_or_else(||
//                         module.add_function(
//                             &name,
//                             type_resolver.resolve_type_as_any(ty).into_function_type(),
//                             None
//                         )
//                     );
//                 (name,ty)
//             })
//             .collect();

//         Self {
//             ctx,
//             module,
//             builder,
//             type_resolver,
//             known_functions,
//             known_values,
//             _known_ops : seed_ops,
//             locals : HashMap::new(),
//             incomplete_generics : HashMap::new(),
//             // curried_locals : HashMap::new(),
//         }
//     }

//     pub fn compile_expr(&mut self, expr: TypedExpr) -> AnyValueEnum<'ctx> {
//         match expr {
//             TypedExpr::BinaryOpCall {
//                 ident,
//                 lhs,
//                 rhs,
//                 ..
//             } => {
//                 let lhs: BasicValueEnum = self.compile_expr(*lhs).try_into().unwrap();
//                 let rhs: BasicValueEnum = self.compile_expr(*rhs).try_into().unwrap();
//                 match ident.as_str() {
//                     "+"  => match (lhs,rhs) {
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_add(lhs, rhs, "").as_any_value_enum(),
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => {
//                             let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
//                             self.builder.build_float_add(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
//                             let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
//                             self.builder.build_float_add(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_add(lhs, rhs, "").as_any_value_enum(),
//                         _=> unimplemented!("Operation is not currently supported.")
//                     },
//                     "-"  => match (lhs,rhs) {
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_sub(lhs, rhs, "").as_any_value_enum(),
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => {
//                             let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
//                             self.builder.build_float_sub(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
//                             let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
//                             self.builder.build_float_sub(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_sub(lhs, rhs, "").as_any_value_enum(),
//                         _=> unimplemented!("Operation is not currently supported.")
//                     },
//                     "*"  => match (lhs,rhs) {
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_mul(lhs, rhs, "").as_any_value_enum(),
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => {
//                             let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
//                             self.builder.build_float_mul(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
//                             let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
//                             self.builder.build_float_mul(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_mul(lhs, rhs, "").as_any_value_enum(),
//                         _=> unimplemented!("Operation is not currently supported.")
//                     },
//                     "/"  => match (lhs,rhs) {
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::FloatValue(rhs)) => self.builder.build_float_div(lhs, rhs, "").as_any_value_enum(),
//                         (BasicValueEnum::FloatValue(lhs),BasicValueEnum::IntValue(rhs)) => {
//                             let rhs = self.builder.build_signed_int_to_float(rhs, lhs.get_type(), "");
//                             self.builder.build_float_div(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::FloatValue(rhs)) => {
//                             let lhs = self.builder.build_signed_int_to_float(lhs, rhs.get_type(), "");
//                             self.builder.build_float_div(lhs, rhs, "").as_any_value_enum()
//                         }
//                         (BasicValueEnum::IntValue(lhs),BasicValueEnum::IntValue(rhs)) => self.builder.build_int_signed_div(lhs, rhs, "").as_any_value_enum(),
//                         _=> unimplemented!("Operation is not currently supported.")
//                     },
//                     _ => unreachable!(),
//                 }
//             }
//             TypedExpr::UnaryOpCall { .. } => todo!(),
//             TypedExpr::FnCall { value, arg:Some(arg), rt, .. } => {
//                 let arg_t = arg.get_rt();
//                 let arg_t = self.type_resolver.resolve_type_as_basic(arg_t);
//                 let arg : BasicValueEnum = self.compile_expr(*arg).try_into().unwrap();
//                 match self.compile_expr(*value) {
//                     AnyValueEnum::PointerValue(target) => match target.get_type().get_element_type() {
//                             AnyTypeEnum::StructType(strct_t) =>{
//                             let target = self.builder.build_struct_gep(target, 0, "").unwrap();
//                             let ty = if let ResolvedType::Function { .. } = rt {
//                                 let mut fields = strct_t.get_field_types();
//                                 fields.push(arg_t);
//                                 self.ctx.struct_type(&fields,false).as_basic_type_enum()
//                             } else {
//                                 self.type_resolver.resolve_type_as_basic(rt)
//                             };
//                             let target_fun = self.builder.build_bitcast(target, ty.fn_type(&[target.get_type().into(),arg_t.into()], false).ptr_type(AddressSpace::default()), "").into_pointer_value();
//                             let target_fun : CallableValue = target_fun.try_into().unwrap();
//                             self.builder.build_call(target_fun,&[target.into(),arg.into()],"").as_any_value_enum()
//                         }
//                         AnyTypeEnum::FunctionType(_) => {
//                             let target : CallableValue = target.try_into().unwrap();
//                             self.builder.build_call(target, &[arg.into()], "").as_any_value_enum()
//                         }
//                         _ => unreachable!()
//                     },
//                     AnyValueEnum::FunctionValue(target) => {
//                         self.builder.build_call(target,&[arg.into()], "").as_any_value_enum()
//                     }
//                     _=>unreachable!()
//                 }

//                 // match *value {
//                 //     TypedExpr::FnCall { value, arg : Some(arg), rt } => {
//                 //         let value = self.compile_expr(*value);
//                 //         let value = convert_to_basic_value(value);
//                 //         let fun_t = self.type_resolver.resolve_type_as_basic(rt).fn_type(&[value.get_type().into(),self.type_resolver.resolve_type_as_basic(arg.get_rt()).into()], false);
//                 //         let value = value.into_pointer_value();
//                 //         let fun = self.builder.build_struct_gep(value, 0, "fn").unwrap();
//                 //         let fun = self.builder.build_bitcast(fun, fun_t.ptr_type(AddressSpace::Generic), "").into_pointer_value();
//                 //         let fun : CallableValue = fun.try_into().unwrap();
//                 //         let arg = convert_to_basic_value(self.compile_expr(*arg));
//                 //         let ret = self.builder.build_call(fun,&[value.into(),arg.into()],"");

//                 //         ret.as_any_value_enum()
//                 //     },
//                 //     TypedExpr::ValueRead { ident,.. } => {
//                 //         let Some(fun) : Option<CallableValue> = self.known_functions.get(&ident)
//                 //         .map(|fun|(*fun).into())
//                 //         .or_else(|| self.curried_locals.get(&ident).map(
//                 //             |(curry,fun_t)| {
//                 //                 let fun = self.builder.build_struct_gep(*curry, 0, "").unwrap();
//                 //                 // let fun = self.builder.build_bitcast(fun, (*fun_t).ptr_type(AddressSpace::Generic), "").into_pointer_value();
//                 //                 fun.try_into().unwrap()
//                 //             }
//                 //         ))  else { unreachable!("Function not found??") };

//                 //         todo!()
//                 //     }
//                 //     _ => unimplemented!("funciton value not supported")
//                 // }
//             }
//             TypedExpr::FnCall { value, .. } => {
//                 //this should only ever be a named value?
//                 let TypedExpr::ValueRead { ident,.. } = *value else { unreachable!("not a function name?") };
//                 let Some(fun)= self.known_functions.get(&ident) else { unreachable!("function not found") };

//                 self.builder.build_call(*fun,&[],"").as_any_value_enum()
//             }
//             TypedExpr::Return { expr, .. } => {
//                 if let TypedExpr::UnitLiteral = expr.as_ref() {
//                     self.builder
//                         .build_return(None)
//                         .as_any_value_enum()
//                         .try_into()
//                         .unwrap()
//                 } else {
//                     let sub = self.compile_expr(*expr);
//                     self.builder
//                         .build_return(Some(&convert_to_basic_value(sub)))
//                         .as_any_value_enum()
//                         .try_into()
//                         .unwrap()
//                 }
//             }
//             TypedExpr::Literal { rt, value } => match self.type_resolver.resolve_type_as_basic(rt.clone()) {
//                 BasicTypeEnum::StructType(ty) => {
//                     let cs = self.ctx.const_string(value.as_bytes(), false);
//                     let gs = self.module.add_global(cs.get_type(), None, "");
//                     gs.set_initializer(&cs);
//                     gs.set_constant(true);
//                     let ptr = unsafe {
//                         gs.as_pointer_value().const_in_bounds_gep(&[
//                             self.ctx.i32_type().const_zero(),
//                             self.ctx.i32_type().const_zero(),
//                         ])
//                     };
//                     let ptr_end = unsafe {
//                         gs.as_pointer_value().const_in_bounds_gep(&[
//                             self.ctx.i32_type().const_zero(),
//                             self.ctx.i32_type().const_int(value.len() as u64, false),
//                         ])
//                     };
//                     let p = self.builder.build_alloca(ty, "");
//                     self.builder
//                         .build_store(p, ty.const_named_struct(&[ptr.into(), ptr_end.into()]));
//                     p.as_any_value_enum()
//                 }
//                 BasicTypeEnum::IntType(ty) => {
//                     // lazy_static::lazy_static! {
//                     //     static ref CHAR_REGEX : Regex = Regex::new("")
//                     // }

//                     let v = ty.const_int(value.parse().unwrap(), false);
//                     v.as_any_value_enum()
//                 }
//                 _ => unreachable!(),
//             },
//             TypedExpr::Block { sub, .. } => {
//                 // TODO : Maybe insert more basic blocks as you go down in scope?  could be helpful for life time management
//                 sub.into_iter().fold(None,|_,expr| {

//                     Some(self.compile_expr(expr))
//                 }).unwrap()
//             }
//             TypedExpr::Declaration {
//                 ident,
//                 ty,
//                 args,
//                 value,
//                 ..
//             } => {
//                 match & ty {
//                     crate::types::ResolvedType::Function { .. } => {

//                         let mut result_ty = ty.clone();
//                         let mut curried_args = Vec::with_capacity(args.len() - 1);

//                         // generate needed supporting functions.
//                         let args_curry_functions = args.iter().take(args.len()-1).map(|_| {
//                             let ResolvedType::Function { arg:arg_t, returns } = result_ty.clone() else { unreachable!() };
//                             let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
//                             let mut rt_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::default()).into()];
//                             rt_types.extend(curried_args.iter());
//                             let arg0_types = rt_types.clone();
//                             rt_types.push(arg_t);
//                             let fun_t = if curried_args.len() == 0 {
//                                 self.ctx.struct_type(&rt_types, false).ptr_type(AddressSpace::default()).fn_type(&[arg_t.into()], false)
//                             } else {
//                                 let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::default());
//                                 self.ctx.struct_type(&rt_types, false).ptr_type(AddressSpace::default()).fn_type(&[arg0_t.into(),arg_t.into()], false)
//                             };
//                             curried_args.push(arg_t);
//                             result_ty = match *returns{
//                                 ResolvedType::Pointer { underlining } if let ResolvedType::Function { .. } = underlining.as_ref() => *underlining,
//                                 _ => *returns
//                             };
//                             self.module.add_function(&ident, fun_t, None)
//                         }).collect_vec();

//                         let ResolvedType::Function { arg:arg_t, returns:rt } = result_ty else { unreachable!() };
//                         let fun_t = if rt.as_ref() == &ResolvedType::Void {
//                             let rt = self.ctx.void_type();
//                             let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
//                             if curried_args.len() == 0 {
//                                 rt.fn_type(&[arg_t.into()], false)
//                             } else {
//                                 let mut arg0_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::default()).into()];
//                                 arg0_types.extend(curried_args.iter());
//                                 let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::default());
//                                 rt.fn_type(&[arg0_t.into(),arg_t.into()], false)
//                             }
//                         } else {
//                             let rt = self.type_resolver.resolve_type_as_basic(*rt);
//                             let arg_t = self.type_resolver.resolve_type_as_basic(*arg_t);
//                             if curried_args.len() == 0 {
//                                 rt.fn_type(&[arg_t.into()], false)
//                             } else {
//                                 let mut arg0_types = vec![self.ctx.i8_type().ptr_type(AddressSpace::default()).into()];
//                                 arg0_types.extend(curried_args.iter());
//                                 let arg0_t = self.ctx.struct_type(&arg0_types, false).ptr_type(AddressSpace::default());
//                                 rt.fn_type(&[arg0_t.into(),arg_t.into()], false)
//                             }
//                         };
//                         let v = self.module.add_function(&ident, fun_t, None);
//                         self.known_functions.insert(ident.clone(), v);
//                         let bb = self.ctx.append_basic_block(v, "");
//                         self.builder.position_at_end(bb);

//                         for (idx,arg_name) in args.iter().enumerate().take(args.len()-1) {
//                             let arg = self.builder.build_alloca(curried_args[idx], arg_name.as_str());
//                             let gep = unsafe { self.builder.build_in_bounds_gep(v.get_first_param().unwrap().into_pointer_value(), &[self.ctx.i32_type().const_zero(),self.ctx.i32_type().const_int((idx+1) as u64, false)], "") };
//                             let value = self.builder.build_load(gep,"");
//                             self.builder.build_store(arg,value);
//                             self.locals.insert(arg_name.clone(), arg.into());
//                         }

//                         let last_param = v.get_last_param().unwrap();
//                         let arg = self.builder.build_alloca(last_param.get_type(), args.last().unwrap());
//                         self.builder.build_store(arg,last_param);
//                         self.locals.insert(args.last().cloned().unwrap(), arg.into());
//                         self.compile_expr(*value);

//                         for (curr,next) in args_curry_functions.iter().chain(once(&v)).tuple_windows() {

//                             let bb = self.ctx.append_basic_block(*curr, "");
//                             self.builder.position_at_end(bb);
//                             let ret_t = curr.get_type().get_return_type().unwrap().into_pointer_type();
//                             let ret = self.builder.build_malloc(ret_t.get_element_type().into_struct_type(), "ret_ptr").unwrap();
//                             let next_fn_ptr = self.builder.build_bitcast(next.as_global_value().as_pointer_value(), self.ctx.i8_type().ptr_type(AddressSpace::default()), "");
//                             let next_ptr = self.builder.build_struct_gep(ret, 0, "").unwrap();
//                             self.builder.build_store(next_ptr, next_fn_ptr);
//                             let last = ret_t.get_element_type().into_struct_type().get_field_types().len() - 1;
//                             let new_element = self.builder.build_struct_gep(ret,last as u32, "").unwrap();
//                             self.builder.build_store(new_element,curr.get_last_param().unwrap());

//                             if let Some(BasicValueEnum::PointerValue(ptr)) = curr.get_first_param()
//                             && let AnyTypeEnum::StructType(strct) = ptr.get_type().get_element_type()
//                             && !strct.is_opaque() {
//                                 let arg0 = unsafe { self.builder.build_in_bounds_gep(ptr, &[self.ctx.i32_type().const_zero()], "") };
//                                 // let arg0 = self.builder.build_load(arg0, "").into_struct_value();
//                                 for idx in 1..strct.get_field_types().len() {
//                                     let ele = self.builder.build_struct_gep(arg0, idx as _, "").unwrap();
//                                     let ele = self.builder.build_load(ele,"");
//                                     let ele_out = self.builder.build_struct_gep(ret, idx as u32, "").unwrap();
//                                     self.builder.build_store(ele_out, ele);
//                                 }
//                             }
//                             self.builder.build_return(Some(&ret));
//                         }
//                         v.as_any_value_enum()
//                     }
//                     _ => {
//                         todo!()
//                     }
//                 }
//             }
//             TypedExpr::ValueRead(ident,_) => {
//                 self.locals.get(&ident)
//                 .map(|val| {
//                     self.builder.build_load(*val,"").as_any_value_enum()
//                 })
//                 .or(self.known_values.get(&ident).map(|val| val.as_any_value_enum()))
//                 .or(self.known_functions.get(&ident).map(|fun| fun.as_any_value_enum())).unwrap()
//             },
//             TypedExpr::UnitLiteral => todo!(),
//         }
//     }

//     pub fn compile_program<T : IntoIterator<Item = TypedExpr>>(self, ast: T) -> Module<'ctx> {
//         let this = Rc::new(RefCell::new(self));
//         for expr in ast {
//             // match expr {
//             //     TypedExpr::Declaration { is_op, ident, ty, args, value, curried, generic : true } => {
//             //         let args = args.clone();
//             //         let ident_clone = ident.clone();
//             //         let this_clone = this.clone();
//             //         let gen_fun = Box::new(move |new_ty:ResolvedType|{
//             //             let replaced_ty = ty.clone().replace_first_generic(new_ty.clone());
//             //             if replaced_ty.is_generic() {
//             //                 Either::Right(TypedExpr::Declaration { is_op, ident: ident.clone() + "_" + &new_ty.to_string(), ty: replaced_ty, args : args.clone(), value : value.clone(), curried, generic: true })
//             //             } else {
//             //                 Either::Left(this_clone.borrow_mut().compile_expr(TypedExpr::Declaration { is_op, ident : ident.clone() + "_" + &new_ty.to_string(), ty: replaced_ty, args:args.clone(), value : value.clone(), curried, generic:false }).into_function_value())
//             //             }
//             //         }) as Box<dyn FnMut(ResolvedType)-> Either<FunctionValue<'ctx>, TypedExpr>>;
//             //         this.borrow_mut().incomplete_generics.entry(ident_clone).insert_entry(gen_fun);
//             //     },
//             //     _ => { this.borrow_mut().compile_expr(expr); },
//             // }
//             this.borrow_mut().compile_expr(expr);
//         }
//         let module = this.borrow().module.clone();
//         module
//     }
// }

// fn convert_to_basic_value<'ctx>(value: AnyValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
//     match value {
//         AnyValueEnum::ArrayValue(v) => BasicValueEnum::ArrayValue(v),
//         AnyValueEnum::IntValue(v) => BasicValueEnum::IntValue(v),
//         AnyValueEnum::FloatValue(v) => BasicValueEnum::FloatValue(v),
//         AnyValueEnum::FunctionValue(v) => {
//             v.as_global_value().as_pointer_value().as_basic_value_enum()
//         }
//         AnyValueEnum::PointerValue(v) => BasicValueEnum::PointerValue(v),
//         AnyValueEnum::StructValue(v) => BasicValueEnum::StructValue(v),
//         AnyValueEnum::VectorValue(v) => BasicValueEnum::VectorValue(v),
//         AnyValueEnum::InstructionValue(_) |
//         AnyValueEnum::MetadataValue(_) |
//         AnyValueEnum::PhiValue(_) => unimplemented!(),
//     }
// }

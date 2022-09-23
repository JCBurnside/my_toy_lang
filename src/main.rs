#![feature(stmt_expr_attributes)]
#![feature(iter_advance_by)]
#![feature(drain_filter)]
#![feature(let_chains)]
use std::collections::HashMap;

mod ast;
mod code_gen;
mod lexer;
mod parser;
mod tokens;
mod util;


use ast::{resolve_type, Expr, ResolvedType, TypeName, TypedExpr};
use code_gen::CodeGen;
use lexer::TokenStream;
use parser::Parser;

use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValue, FunctionValue};
use inkwell::AddressSpace;

use itertools::Itertools;

// fn add_std_lib_ops<'ctx>(ctx:&'ctx Context,types : &mut HashMap<String,BasicTypeEnum<'ctx>>) -> (HashMap<String,Vec<FunctionType<'ctx>>>,HashMap<String,Vec<FunctionValue<'ctx>>>) {
//     let out = HashMap::new();

//     out
// }

#[repr(C)]
pub struct MyStr {
    start: *const i8,
    end: *const i8,
}

fn main() {
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    // if !inkwell::support::load_library_permanently("./std/target/debug/langstd.lib") {
    //     println!("couldn't load lib");
    //     return;
    // }
    let ctx = Context::create();
    let module = ctx.create_module("JIT");

    let mut known_declarations: HashMap<String, Box<dyn AnyValue>> = HashMap::new();
    let mut known_functions: HashMap<String, FunctionValue<'_>> = HashMap::new();
    let mut known_function_types: HashMap<String, FunctionType<'_>> = HashMap::new();
    let mut known_types: HashMap<String, BasicTypeEnum> = HashMap::new();
    known_types.insert("int8".to_string(), ctx.i8_type().as_basic_type_enum());
    known_types.insert("int16".to_string(), ctx.i16_type().as_basic_type_enum());
    known_types.insert("int32".to_string(), ctx.i32_type().as_basic_type_enum());
    known_types.insert("int64".to_string(), ctx.i64_type().as_basic_type_enum());
    known_types.insert("float32".to_string(), ctx.f32_type().as_basic_type_enum());
    known_types.insert("float64".to_string(), ctx.f64_type().as_basic_type_enum());
    known_types.insert("char".to_string(), ctx.i8_type().as_basic_type_enum());
    let str_t = ctx.opaque_struct_type("str");
    str_t.set_body(
        &[
            known_types["char"]
                .into_int_type()
                .ptr_type(AddressSpace::Generic)
                .into(), //start
            known_types["char"]
                .into_int_type()
                .ptr_type(AddressSpace::Generic)
                .into(), //end
        ],
        false,
    );
    known_types.insert("str".to_string(), str_t.as_basic_type_enum());
    let unit = ctx.const_struct(&[], false);
    let unit_t = unit.get_type();
    known_types.insert("unit".to_string(), unit_t.as_basic_type_enum());
    let put_int32_fn_t = unit_t.fn_type(&[known_types["int32"].into()], false);
    let put_int32_fn = module.add_function("put_int32", put_int32_fn_t, None);
    known_function_types.insert("put_int32".to_string(), put_int32_fn_t);
    let print_fn_t = unit_t.fn_type(
        &[known_types["str"].ptr_type(AddressSpace::Generic).into()],
        false,
    );
    let print = "print_str".to_string();
    let print_fn = module.add_function(&print, print_fn_t, None);
    known_function_types.insert(print.clone(), print_fn_t);
    // known_functions.insert(print.clone(), print_fn);
    let put_char_fn_t = unit_t.fn_type(&[known_types["char"].into()], false);
    let put_char_fn = module.add_function("put_char", put_char_fn_t, None);
    known_function_types.insert("put_char".to_string(), put_char_fn_t);
    // known_functions.insert("put_char".to_string(), put_char_fn);
    // let mut file = String::new();
    // std::io::stdin().read_line(&mut file).unwrap();
    let contents = include_str!("../samples/simple_print_test.foo");
    let tokens = TokenStream::from_source(&contents);
    let mut parser = Parser::from_stream(tokens);

    let _tokens_ = TokenStream::from_source(contents)
        .map(|(a, _)| a)
        .collect_vec();
    let mut ast: Vec<Expr> = Vec::new();
    while parser.has_next() {
        let expr = parser.next_expr().expect("Parser error");
        match &expr {
            Expr::Declaration {
                is_op,
                ident,
                ty,
                args,
                value,
            } => {
                match (ty, args) {
                    (Some(_), Some(args)) if args.iter().any(|(_, it)| it.is_some()) => {
                        panic!("doubled typed");
                    }
                    (Some(TypeName::ValueType(_)), Some(_)) => panic!("Arg count mismatch"), //should be no args.  will need to change when fn keyword is introduced.
                    (None, Some(args)) if args.iter().any(|(_, it)| it.is_none()) => {
                        panic!("some type in the function is not known")
                    }
                    (Some(TypeName::FnType(_, _)), Some(_)) => {
                        let ty = if let ResolvedType::Fn(ty) =
                            resolve_type(ty.as_ref().unwrap().clone(), &known_types, &ctx).unwrap()
                        {
                            ty
                        } else {
                            unreachable!()
                        };
                        known_function_types.insert(ident.clone(), ty);
                    }
                    (None, Some(args)) => {
                        // let args = args.iter()
                        //     .map(|(_,ty)| ty.unwrap())
                        //     .map(|ty| resolve_type(ty, &known_types, &ctx).unwrap())
                        //     .collect_vec();
                        // let rt = if let Expr::Block { sub } = value.as_ref() {
                        //     let sub: Vec<TypedExpr> = sub
                        //         .iter()
                        //         .filter(|expr| matches!(expr,Expr::Return { .. }))
                        //         .map(|expr| match expr {
                        //             Expr::BinaryOpCall { ident, .. } |
                        //             Expr::UnaryOpCall { ident, .. } |
                        //             Expr::FnCall { ident, .. } => known_function_types[ident],

                        //         })
                        //         .collect::<Result<Vec<_>, _>>().unwrap();

                        //     let last = sub.last().unwrap().get_rt(types);

                        //     if valid {
                        //         last
                        //     } else {
                        //         panic!("could not determine return type");
                        //     }
                        // }
                        todo!("For now indiviual arg typing and return type inference is planned but not supported.");
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unreachable!(),
        }
        ast.push(expr);
    }
    let ast = ast
        .into_iter()
        .map(|expr| TypedExpr::try_from(&ctx, &known_types, &known_function_types, expr))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let code_gen = CodeGen::with_module(&ctx, known_functions, known_types, HashMap::new(), module);
    let module = code_gen.compile_program(&ast);
    if let Err(err) = module.verify() {
        println!("{}",err);
    }
    #[cfg(debug_assertions)]
    module.print_to_file("./target/out.txt").unwrap();
    let jit = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .expect("could not create jit engine");
    jit.add_global_mapping(&print_fn, langstd::print as usize);
    jit.add_global_mapping(&put_int32_fn, langstd::put_int32 as usize);
    let r = unsafe {
        jit.get_function::<unsafe extern "C" fn(i32) -> i32>("main")
            .unwrap()
            .call(0)
    };
    println!("result {:?}", r);
}

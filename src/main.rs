#![feature(stmt_expr_attributes)]
#![feature(iter_advance_by)]
#![feature(drain_filter)]
#![feature(let_chains)]
#![feature(generic_arg_infer)]
use std::collections::HashMap;

mod ast;
mod code_gen;
mod langstd;
mod lexer;
mod parser;
mod tokens;
mod util;
mod types;

use ast::{Expr, TypeName, TypedExpr};
use code_gen::CodeGen;
use lexer::TokenStream;
use parser::Parser;
use types::ResolvedType;
use types::TypeResolver;

use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target};

use multimap::MultiMap;

use crate::types::resolve_from_name;
use crate::util::ExtraUtilFunctions;

fn main() {
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    // if !inkwell::support::load_library_permanently("./std/target/debug/langstd.lib") {
    //     println!("couldn't load lib");
    //     return;
    // }
    let ctx = Context::create();
    let std_mod = ctx.create_module("std");
    let mut type_resolver = TypeResolver::new(&ctx);
    let print_fn = type_resolver.resolve_type_as_any(ResolvedType::Function { arg: types::STR.boxed(), returns: types::UNIT.boxed() }).into_function_type();
    let print_fn = std_mod.add_function("print_str", print_fn, None);
    let put_int32_fn = type_resolver.resolve_type_as_any(ResolvedType::Function { arg: types::INT32.boxed(), returns: types::UNIT.boxed() }).into_function_type();
    let put_int32_fn = std_mod.add_function("put_int32", put_int32_fn, None);
    
    let module = ctx.create_module("JIT");

    // known_functions.insert("put_char".to_string(), put_char_fn);
    // let mut file = String::new();
    // std::io::stdin().read_line(&mut file).unwrap();
    let contents = include_str!("../samples/simple_print_test.foo");
    let tokens = TokenStream::from_source(&contents);
    let mut parser = Parser::from_stream(tokens);

    let mut ast: Vec<Expr> = Vec::new();
    #[allow(unused_mut)]
    let mut top_level_fns = HashMap::<String,ResolvedType>::new();
    while parser.has_next() {
        let expr = parser.next_expr().expect("Parser error");
        match &expr {
            #[allow(unused)]
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
                        if top_level_fns.insert(ident.clone(), resolve_from_name(ty.as_ref().cloned().unwrap())).is_some() {
                            panic!("two functions with same name!");
                        }
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
        .map(|expr| {
            let functions = top_level_fns.clone();
            TypedExpr::try_from(&ctx, &type_resolver, functions, expr)
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let code_gen = CodeGen::with_module(
        &ctx,
        module,
        type_resolver,
        top_level_fns,
        HashMap::new(),
        MultiMap::new()
    );
    let module = code_gen.compile_program(&ast);
    if let Err(err) = module.verify() {
        println!("{}", err);
    }
    #[cfg(debug_assertions)]
    module.print_to_file("./target/out.txt").unwrap();
    module.link_in_module(std_mod).unwrap();
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

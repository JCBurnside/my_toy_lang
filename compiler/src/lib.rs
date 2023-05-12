 #![feature(stmt_expr_attributes)]
#![feature(iter_advance_by)]
#![feature(drain_filter)]
#![feature(let_chains)]
#![feature(generic_arg_infer)]
#![feature(if_let_guard)]
#![feature(extend_one)]
#![feature(entry_insert)]
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::path::PathBuf;

pub mod ast;
// mod langstd;
mod lexer;
mod parser;
mod tokens;
mod util;
pub mod types;
mod code_gen;

use ast::{Expr, TypeName, TypedExpr};
use code_gen::CodeGen;
use inkwell::module::Module;
use itertools::Itertools;
use lexer::TokenStream;
use parser::Parser;
use types::{ResolvedType, TypeResolver};

use inkwell::context::Context;
use multimap::MultiMap;



pub fn from_file<'ctx>(file : &PathBuf, ctx : &'ctx Context,mut fwd_declarations : HashMap<String, ResolvedType>) -> Result<Module<'ctx>,Vec<Box<dyn Display>>> {
    
    // TODO: I would like to make this work for now I will read the whole file to a string then 
    // let file = File::open(file).map_err(Box::new).map_err(|err| vec![err as Box<dyn Display>])?;
    // let file = BufReader::new(file);
    // let lex = TokenStream::from_iter(file.chars().map(|r| r.unwrap()));
    let contents = std::fs::read_to_string(file).map_err(Box::new).map_err(|err| vec![err as Box<dyn Display>])?;
    let strm = TokenStream::from_source(&contents);
    let mut parser = Parser::from_stream(strm);
    let module = ctx.create_module(file.file_stem().unwrap().to_str().unwrap());
    let file_name = file.file_name().unwrap();
    module.set_source_file_name(file_name.to_str().unwrap());
    let mut type_resolver = TypeResolver::new(ctx);
    let code_gen = CodeGen::with_module(
        &ctx,
        module,
        type_resolver.clone(),
        fwd_declarations.clone(),
        HashMap::new(),
        MultiMap::new()
    );
    let mut ast = Vec::new();
    let mut errors = Vec::new();
    while parser.has_next() {
        match parser.next_expr() {
            Ok(expr) => {
                match &expr {
                    #[allow(unused)]
                    Expr::Declaration {
                        is_op,
                        ident,
                        ty,
                        args,
                        value,
                        generictypes,
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
                                if !is_op {
                                    fwd_declarations.entry(ident.clone()).insert_entry(types::resolve_from_name(ty.as_ref().unwrap().clone(), &generictypes.iter().cloned().collect()));
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
                                todo!("For now individual arg typing and return type inference is planned but not supported.");
                            }
                            _ => unimplemented!(),
                        }
                        
                    }
                    _ => unreachable!(),
                }
                ast.push(expr);
            },
            Err(err) => errors.push(err),
        }
    }
    let ast = dbg!(ast)
        .into_iter()
        .map(|expr| {
            let functions = fwd_declarations.clone();
            TypedExpr::try_from(&ctx, type_resolver.clone(), functions, &HashSet::new(), expr)
        })
        .map_ok(|(ast,mut generics)| {
            generics.extend_one(ast);
            generics
        })
        .flatten_ok()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    
    let module = code_gen.compile_program(ast);
    Ok(module)
}

// fn main() {
//     Target::initialize_native(&InitializationConfig::default()).unwrap();
//     // if !inkwell::support::load_library_permanently("./std/target/debug/langstd.lib") {
//     //     println!("couldn't load lib");
//     //     return;
//     // }
//     let ctx = Context::create();
//     let std_mod = ctx.create_module("std");
// let mut type_resolver = TypeResolver::new(&ctx);
//     let print_fn = type_resolver.resolve_type_as_any(ResolvedType::Function { arg: types::STR.boxed(), returns: types::UNIT.boxed() }).into_function_type();
//     #[allow(unused)]
//     let print_fn = std_mod.add_function("print_str", print_fn, None);
//     let put_int32_fn = type_resolver.resolve_type_as_any(ResolvedType::Function { arg: types::INT32.boxed(), returns: types::UNIT.boxed() }).into_function_type();
//     #[allow(unused)]
//     let put_int32_fn = std_mod.add_function("put_int32", put_int32_fn, None);
    
//     let module = ctx.create_module("JIT");

//     // known_functions.insert("put_char".to_string(), put_char_fn);
//     // let mut file = String::new();
//     // std::io::stdin().read_line(&mut file).unwrap();
//     let contents = include_str!("../../samples/multi_arg_fun.foo");
//     let tokens = TokenStream::from_source(&contents);
//     let mut parser = Parser::from_stream(tokens);

//     let mut ast: Vec<Expr> = Vec::new();
//     #[allow(unused_mut)]
//     let mut top_level_fns = HashMap::<String,ResolvedType>::new();
//     top_level_fns.insert("print_str".to_string(), ResolvedType::Function { arg:types::STR.boxed(), returns: types::UNIT.boxed() });
//     top_level_fns.insert("put_int32".to_string(), ResolvedType::Function { arg: types::INT32.boxed(), returns: types::UNIT.boxed() });
//     while parser.has_next() {
//         let expr = parser.next_expr().expect("Parser error");
//         match &expr {
//             #[allow(unused)]
//             Expr::Declaration {
//                 is_op,
//                 ident,
//                 ty,
//                 args,
//                 value,
//             } => {
//                 match (ty, args) {
//                     (Some(_), Some(args)) if args.iter().any(|(_, it)| it.is_some()) => {
//                         panic!("doubled typed");
//                     }
//                     (Some(TypeName::ValueType(_)), Some(_)) => panic!("Arg count mismatch"), //should be no args.  will need to change when fn keyword is introduced.
//                     (None, Some(args)) if args.iter().any(|(_, it)| it.is_none()) => {
//                         panic!("some type in the function is not known")
//                     }
//                     (Some(TypeName::FnType(_, _)), Some(_)) => {
//                         // if top_level_fns.insert(ident.clone(), resolve_from_name(ty.as_ref().cloned().unwrap())).is_some() {
//                         //     panic!("two functions with same name!");
//                         // }
//                     }
//                     (None, Some(args)) => {
//                         // let args = args.iter()
//                         //     .map(|(_,ty)| ty.unwrap())
//                         //     .map(|ty| resolve_type(ty, &known_types, &ctx).unwrap())
//                         //     .collect_vec();
//                         // let rt = if let Expr::Block { sub } = value.as_ref() {
//                         //     let sub: Vec<TypedExpr> = sub
//                         //         .iter()
//                         //         .filter(|expr| matches!(expr,Expr::Return { .. }))
//                         //         .map(|expr| match expr {
//                         //             Expr::BinaryOpCall { ident, .. } |
//                         //             Expr::UnaryOpCall { ident, .. } |
//                         //             Expr::FnCall { ident, .. } => known_function_types[ident],

//                         //         })
//                         //         .collect::<Result<Vec<_>, _>>().unwrap();

//                         //     let last = sub.last().unwrap().get_rt(types);

//                         //     if valid {
//                         //         last
//                         //     } else {
//                         //         panic!("could not determine return type");
//                         //     }
//                         // }
//                         todo!("For now individual arg typing and return type inference is planned but not supported.");
//                     }
//                     _ => unimplemented!(),
//                 }
//             }
//             _ => unreachable!(),
//         }
//         ast.push(expr);
//     }
//     let ast = ast
//         .into_iter()
//         .map(|expr| {
//             let functions = top_level_fns.clone();
//             TypedExpr::try_from(&ctx, &type_resolver, functions, expr)
//         })
//         .map_ok(|(ast,mut generics)| {
//             generics.extend_one(ast);
//             generics
//         })
//         .flatten_ok()
//         .collect::<Result<Vec<_>, _>>()
//         .unwrap();
//     let code_gen = CodeGen::with_module(
//         &ctx,
//         module,
//         type_resolver,
//         top_level_fns,
//         HashMap::new(),
//         MultiMap::new()
//     );
//     let module = code_gen.compile_program(ast);
//     if let Err(err) = module.verify() {
//         println!("{}", err);
//     }
//     #[cfg(debug_assertions)]
//     module.print_to_file("./target/out.txt").unwrap();
//     module.link_in_module(std_mod).unwrap();
//     #[allow(non_snake_case)]
//     let (DIbuider,_DIunit) = module.create_debug_info_builder(
//         false,
//         DWARFSourceLanguage::Haskell, 
//         "testing", 
//         std::env::current_dir().unwrap().to_str().unwrap(), 
//         "", 
//         false, 
//         "", 
//         0, 
//         "", 
//         DWARFEmissionKind::Full, 
//         0,
//         false, 
//         false, 
//         "", 
//         ""
//     );
//     DIbuider.finalize();
    
//     // let jit = module
//     //     .create_jit_execution_engine(inkwell::OptimizationLevel::None)
//     //     .expect("could not create jit engine");
//     // jit.add_global_mapping(&print_fn, langstd::print as usize);
//     // jit.add_global_mapping(&put_int32_fn, langstd::put_int32 as usize);
//     // let r = unsafe {
//     //     jit.get_function::<unsafe extern "C" fn(i32) -> i32>("main")
//     //         .unwrap()
//     //         .call(0)
//     // };
//     // println!("result {:?}", r);
// }

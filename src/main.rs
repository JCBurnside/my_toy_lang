#![feature(stmt_expr_attributes)]
#![feature(iter_advance_by)]
#![feature(drain_filter)]
#![feature(let_chains)]
#![cfg_attr(test, allow(dead_code))]
use std::collections::HashMap;
use std::io::Write;

use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{AnyType, BasicTypeEnum, BasicType};
use inkwell::AddressSpace;
use inkwell::values::{AnyValue, FunctionValue};
use itertools::Itertools;
use lexer::TokenStream;
use parser::Parser;

mod util;
mod ast;
mod code_gen;
mod lexer;
mod parser;
mod tokens;

#[repr(C)]
struct MyStr {
    start: *const i8,
    end: *const i8,
}

#[no_mangle]
unsafe extern "C" fn print(arg0: MyStr) {
    let mut lock = std::io::stdout().lock();
    let mut curr = arg0.start;
    while curr != arg0.end {
        lock.write(&[*curr as u8]).unwrap();
        curr = curr.add(1);
    }
}

#[used]
static KNOWN_FUNCTIONS: [unsafe extern "C" fn(MyStr); 1] = [print];

fn main() {
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let ctx = Context::create();
    let module = ctx.create_module("JIT");

    let mut known_declarations : HashMap<String, Box<dyn AnyValue>> = HashMap::new();
    let mut known_functions : HashMap<String,FunctionValue> = HashMap::new();
    let mut known_types : HashMap<String,BasicTypeEnum> = HashMap::new();
    known_types.insert("int8".to_string(), ctx.i8_type().as_basic_type_enum());
    known_types.insert("int16".to_string(), ctx.i16_type().as_basic_type_enum());
    known_types.insert("int32".to_string(), ctx.i32_type().as_basic_type_enum());
    known_types.insert("int64".to_string(), ctx.i64_type().as_basic_type_enum());
    known_types.insert("float32".to_string(), ctx.f32_type().as_basic_type_enum());
    known_types.insert("float64".to_string(), ctx.f64_type().as_basic_type_enum());
    let str_t = ctx.struct_type(
        &[
            known_types["int8"]
                .into_int_type()
                .ptr_type(AddressSpace::Generic)
                .into(), //start
            known_types["int8"]
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

    let mut declarations = HashMap::new();
    let print_fn_t = ctx
        .void_type()
        .fn_type(&[known_types["str"].into_struct_type().into()], false);
    let _print_fn = module.add_function("print", print_fn_t, None);
    let print = "print".to_string();
    declarations.insert(print.clone(), print_fn_t.as_any_type_enum());
    module.add_function(&print, print_fn_t, None);



    let mut file = String::new();
    std::io::stdin().read_line(&mut file).unwrap();
    let contents = r#"let foo _ : ( int32 -> int32 ) -> int32 =
    return 0"#;
    let tokens = TokenStream::from_source(&contents);
    let tokens = tokens.collect_vec();
    println!("{:?}", tokens);
    // let mut parser = Parser::from_stream(tokens);
    // match parser.next_expr() {
    //     Ok(ast::Expr::Declaration { ty, .. }) => println!("function parsed as {:?}",ty),
    //     Ok(other) => println!("welp I got {:?}", other),
    //     Err(err) => println!("Something went wrong. {:?}", err),
    // };
    let jit = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .expect("could not create jit engine");
}

#![feature(stmt_expr_attributes)]
#![feature(iter_advance_by)]
#![feature(drain_filter)]
#![feature(let_chains)]
#![cfg_attr(test, allow(dead_code))]
use std::collections::HashMap;
use std::io::Write;


use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::targets::{InitializationConfig,Target};
use inkwell::types::AnyType;
use lexer::TokenStream;
use parser::Parser;


mod ast;
mod lexer;
mod parser;
mod tokens;
mod code_gen;

#[repr(C)]
struct MyStr {
    start : *const i8,
    end : *const i8,
}

#[no_mangle]
unsafe extern "C" fn print(arg0:MyStr) {
    let mut lock = std::io::stdout().lock();
    let mut curr = arg0.start;
    while curr != arg0.end {
        lock.write(&[*curr as u8]).unwrap();
        curr = curr.add(1);
    }
}

#[used]
static KNOWN_FUNCTIONS : [unsafe extern "C" fn(MyStr);1] = [print];

fn main() {
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let ctx = Context::create();
    let module = ctx.create_module("JIT");

    let mut known_types = HashMap::new();
    known_types.insert("int8".to_string(), ctx.i8_type().as_any_type_enum());
    known_types.insert("int16".to_string(), ctx.i16_type().as_any_type_enum());
    known_types.insert("int32".to_string(), ctx.i32_type().as_any_type_enum());
    known_types.insert("int64".to_string(), ctx.i64_type().as_any_type_enum());
    known_types.insert("float32".to_string(), ctx.f32_type().as_any_type_enum());
    known_types.insert("float64".to_string(), ctx.f64_type().as_any_type_enum());
    let str_t = ctx.struct_type(&[
        known_types["int8"].into_int_type().ptr_type(AddressSpace::Generic).into(),//start
        known_types["int8"].into_int_type().ptr_type(AddressSpace::Generic).into()//end
    ],
    false);
    known_types.insert("str".to_string(), str_t.as_any_type_enum());
    
    let mut declarations = HashMap::new();
    let print_fn_t = ctx.void_type().fn_type(&[known_types["str"].into_struct_type().into()], false);
    let print_fn = module.add_function("print", print_fn_t, None);
    let print = "print".to_string();
    declarations.insert(print.clone(),print_fn_t.as_any_type_enum());
    module.add_function(&print, print_fn_t, None);
    
    
    let mut file = String::new();
    std::io::stdin().read_line(&mut file).unwrap();
    let contents = std::fs::read_to_string(file).unwrap();
    let tokens = TokenStream::from_source(&contents);
    let parser = Parser::from_stream(tokens);
    
    let jit = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .expect("could not create jit engine");
}

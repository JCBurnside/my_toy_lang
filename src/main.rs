#![feature(stmt_expr_attributes)]
#![feature(iter_advance_by)]
#![feature(drain_filter)]
#![feature(let_chains)]
#![cfg_attr(test, allow(dead_code))]
use inkwell::context::Context;

mod ast;
mod lexer;
mod parser;
mod tokens;
fn main() {
    let ctx = Context::create();
    let module = ctx.create_module("JIT");
    let _jit = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .expect("could not create jit engine");
}

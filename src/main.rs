#![feature(stmt_expr_attributes)]
#![feature(drain_filter)]
#![feature(iter_advance_by)]
use inkwell::context::Context;

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

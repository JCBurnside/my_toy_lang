use inkwell::context::Context;

mod tokens;
mod lexer;
fn main() {
    let ctx = Context::create();
    let module = ctx.create_module("JIT");
    
}

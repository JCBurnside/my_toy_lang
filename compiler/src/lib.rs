use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::path::PathBuf;

pub mod ast;
// mod langstd;
mod code_gen;
mod lexer;
mod parser;
mod tokens;
pub mod typed_ast;
pub mod types;
mod util;

use code_gen::CodeGen;
use typed_ast::{TypedDeclaration, TypedModuleDeclaration};
// use code_gen::CodeGen;
use inkwell::module::Module;
use itertools::Itertools;
use lexer::TokenStream;
use parser::Parser;
use types::{ResolvedType, TypeResolver};

use inkwell::context::Context;
use multimap::MultiMap;
type Location = (usize, usize);
pub fn from_file<'ctx>(
    file: &PathBuf,
    ctx: &'ctx Context,
    fwd_declarations: HashMap<String, ResolvedType>,
    is_debug: bool,
) -> Result<Module<'ctx>, Vec<Box<dyn Display>>> {
    // TODO: I would like to make this work for now I will read the whole file to a string then
    // let file = File::open(file).map_err(Box::new).map_err(|err| vec![err as Box<dyn Display>])?;
    // let file = BufReader::new(file);
    // let lex = TokenStream::from_iter(file.chars().map(|r| r.unwrap()));
    let contents = std::fs::read_to_string(file)
        .map_err(Box::new)
        .map_err(|err| vec![err as Box<dyn Display>])?;
    let strm = TokenStream::from_source(&contents);
    let parser = Parser::from_stream(strm);
    let module = ctx.create_module(file.file_stem().unwrap().to_str().unwrap());
    let file_name = file.file_stem().unwrap();
    module.set_source_file_name(file_name.to_str().unwrap());
    let type_resolver = TypeResolver::new(ctx);
    let mut code_gen = CodeGen::with_module(
        &ctx,
        module,
        type_resolver.clone(),
        fwd_declarations.clone(),
        HashMap::new(),
        MultiMap::new(),
    );
    let ast = parser.module(file_name.to_str().unwrap().to_string());
    let mut ast = TypedModuleDeclaration::from(ast, &fwd_declarations); //TODO: foward declare std lib
    ast.lower_generics(&HashMap::new());
    ast.declarations.retain(|it| match it {
        TypedDeclaration::Value(it) => it.generictypes.is_empty(),
        _ => true,
    });
    let module = code_gen.compile_program(vec![ast], false, is_debug);
    Ok(module)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use inkwell::{
        context::Context,
        targets::{InitializationConfig, Target},
    };
    use multimap::MultiMap;

    use crate::types::TypeResolver;

    #[test]
    fn integration() {
        const SRC: &'static str = r#"
for<T> let id a : T -> T = a

let use_fn fun a : (int32 -> int32) -> int32 -> int32 =
    return (fun a)

let main _ : () -> () =
    use_fn id 3
"#;
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let ctx = Context::create();
        let type_resolver = TypeResolver::new(&ctx);

        let parser = crate::parser::Parser::from_source(SRC);
        let ast = parser.module("test".to_string());
        let mut ast = crate::typed_ast::TypedModuleDeclaration::from(ast, &HashMap::new());
        ast.lower_generics(&HashMap::new());
        let code_gen = crate::code_gen::CodeGen::with_module(
            &ctx,
            ctx.create_module(""),
            type_resolver,
            HashMap::new(),
            HashMap::new(),
            MultiMap::new(),
        );
        let module = code_gen.compile_program(vec![ast], true, true);
        let llvm = module.print_to_string();
        println!("{:?}", llvm.to_string())
    }
}

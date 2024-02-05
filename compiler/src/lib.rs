use std::collections::HashMap;
use std::fmt::Display;
use std::path::PathBuf;

pub mod ast;
// mod langstd;
#[cfg(feature = "full")]
mod code_gen;
mod lexer;
mod parser;
mod tokens;
pub mod typed_ast;
pub mod types;
mod util;

use typed_ast::{ResolvedTypeDeclaration, TypedDeclaration, TypedModuleDeclaration};
// use code_gen::CodeGen;


use lexer::TokenStream;
use parser::Parser;
use types::ResolvedType;
#[cfg(feature="full")] 
use types::TypeResolver;
use multimap::MultiMap;
type Location = (usize, usize);

pub fn get_untyped_ast(input:&str,file_name:&str) -> ast::ModuleDeclaration{
    let ts = TokenStream::from_source(input);
    Parser::from_stream(ts).module(file_name.to_string())
} 

#[cfg(feature="full")]
use inkwell::{context::Context, module::Module};

#[cfg(feature = "full")]
pub fn from_file<'ctx>(
    file: &PathBuf,
    ctx: &'ctx Context,
    fwd_declarations: HashMap<String, ResolvedType>,
    is_debug: bool,
    project_name: String,
) -> Result<Module<'ctx>, Vec<Box<dyn Display>>> {
    
    use code_gen::CodeGen;
    use inkwell::{
        targets::{CodeModel, Target, TargetMachine},
    };
    // TODO: I would like to make this work for now I will read the whole file to a string then
    // let file = File::open(file).map_err(Box::new).map_err(|err| vec![err as Box<dyn Display>])?;
    // let file = BufReader::new(file);
    // let lex = TokenStream::from_iter(file.chars().map(|r| r.unwrap()));

    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            TargetMachine::get_host_cpu_name().to_str().unwrap(),
            &TargetMachine::get_host_cpu_features().to_str().unwrap(),
            inkwell::OptimizationLevel::None,
            inkwell::targets::RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    // file contents
    let contents = std::fs::read_to_string(file)
        .map_err(Box::new)
        .map_err(|err| vec![err as Box<dyn Display>])?;
    // lexer
    let strm = TokenStream::from_source(&contents);
    // parser
    let parser = Parser::from_stream(strm);
    let module = ctx.create_module(file.file_stem().unwrap().to_str().unwrap());
    // set for the llvm module
    let type_resolver = TypeResolver::new(ctx, target_machine.get_target_data());
    let file_name = file.file_stem().unwrap();
    module.set_source_file_name(file_name.to_str().unwrap());
    // target specifics eg usize/isize and prefered alignments.  also used to get di

    let code_gen = CodeGen::with_module(
        &ctx,
        module,
        type_resolver,
        fwd_declarations.clone(),
        HashMap::new(),
        MultiMap::new(),
        target_machine.get_target_data(),
    );

    let mut ast = parser.module(file_name.to_str().unwrap().to_string());
    ast.canonialize(vec![project_name]);
    let mut ast = TypedModuleDeclaration::from(ast, &fwd_declarations,); //TODO: foward declare std lib

    ast.lower_generics(&HashMap::new());
    ast.declarations.retain(|it| match it {
        TypedDeclaration::Value(it) => it.generictypes.is_none(),
        TypedDeclaration::TypeDefinition(it) => match it {
            ResolvedTypeDeclaration::Struct(strct) => strct.generics.is_none(),
            _ => true,
        },
        _ => true,
    });

    let module = code_gen.compile_program(vec![ast], false, is_debug);
    Ok(module)
}

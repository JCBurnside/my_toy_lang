use std::collections::HashMap;
use std::fmt::Display;
use std::path::PathBuf;

pub mod ast;
// mod langstd;
mod inference;
mod lexer;
mod parser;
mod tokens;
pub mod typed_ast;
pub mod types;
mod util;

use itertools::Itertools;

use typed_ast::{ResolvedTypeDeclaration, TypedDeclaration, TypedModuleDeclaration};



use lexer::TokenStream;
use parser::Parser;
use types::ResolvedType;
type Location = (usize, usize);


pub fn get_untyped_ast(input:&str,file_name:&str) -> ast::ModuleDeclaration{
    let ts = TokenStream::from_source(input);
    Parser::from_stream(ts).module(file_name.to_string())
} 

pub fn get_ast(input:&str, file_name:&str) -> typed_ast::TypedModuleDeclaration {
    let ts = TokenStream::from_source(input);
    let module = Parser::from_stream(ts).module(file_name.to_string());
    // TODO! get prelude.
    let dep_tree= module.get_dependencies().into_iter().map(|(k,v)| (k,v.into_iter().collect())).collect();
    let ops : HashMap<_,_> = [
        ("+".to_string(), vec![
            types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32))
        ])
    ].into();
    let mut infer_context = inference::Context::new(dep_tree, HashMap::new(), HashMap::new(), ops.clone(), HashMap::new());
    let ast = infer_context.inference(module);
    TypedModuleDeclaration::from(ast, &HashMap::new(), &ops)
}

pub fn from_file<'ctx>(
    file: &PathBuf,
    fwd_declarations: HashMap<String, ResolvedType>,
    is_debug: bool,
    project_name: String,
) -> Result<TypedModuleDeclaration, Vec<Box<dyn Display>>> {
    
    
    // TODO: I would like to make this work for now I will read the whole file to a string then
    // let file = File::open(file).map_err(Box::new).map_err(|err| vec![err as Box<dyn Display>])?;
    // let file = BufReader::new(file);
    // let lex = TokenStream::from_iter(file.chars().map(|r| r.unwrap()));

    let file_name = file.file_stem().unwrap();
    // target specifics eg usize/isize and prefered alignments.  also used to get di

    // let code_gen = CodeGen::with_module(
    //     &ctx,
    //     module,
    //     type_resolver,
    //     fwd_declarations.clone(),
    //     HashMap::new(),
    //     MultiMap::new(),
    //     target_machine.get_target_data(),
    // );
 // file contents
    let contents = std::fs::read_to_string(file)
        .map_err(Box::new)
        .map_err(|err| vec![err as Box<dyn Display>])?;
    // lexer
    let strm = TokenStream::from_source(&contents);
    // parser
    let parser = Parser::from_stream(strm);


    let mut ast = parser.module(file_name.to_str().unwrap().to_string());
    ast.canonialize(vec![project_name]);
    let dependency_graph = ast.get_dependencies();
    let dependency_tree = dependency_graph
        .into_iter()
        .map(|(key, value)| (key, value.into_iter().collect()))
        .collect();
    let mut inference_context = inference::Context::new(
        dependency_tree,
        fwd_declarations.clone(),
        HashMap::new(),
        HashMap::new(),
        HashMap::new(),
    );
    let ast = inference_context.inference(ast);
    let mut ast =
        TypedModuleDeclaration::from(ast, &fwd_declarations, &HashMap::new()); //TODO: foward declare std lib


    ast.lower_generics(&HashMap::new());

    Ok(ast)
}

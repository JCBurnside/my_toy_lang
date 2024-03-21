use std::collections::HashMap;
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
use thiserror::Error;

use typed_ast::TypedModuleDeclaration;

#[derive(Error, Debug)]
#[error(transparent)]
pub enum Error {
    ParseError(#[from] parser::ParseError),
    Io(#[from] std::io::Error),
}

#[derive(Error, Debug)]
#[error(transparent)]
pub enum Warning {
    Parsing(#[from] parser::Warning),
}

#[derive(Error, Debug)]
#[error(transparent)]
pub enum WarningAndError {
    Error(#[from] Error),
    Warning(#[from] Warning),
}

use lexer::TokenStream;
use parser::{Parser, ParserReturns};
use types::ResolvedType;
type Location = (usize, usize);

pub fn get_untyped_ast(
    input: &str,
    file_name: &str,
) -> (ast::ModuleDeclaration, Vec<WarningAndError>) {
    let ts = TokenStream::from_source(input);
    let ParserReturns {
        ast,
        loc: _,
        warnings,
        errors,
    } = Parser::from_stream(ts).module(file_name.to_string());
    let warningsanderrors = warnings
        .into_iter()
        .map(|w| Warning::from(w).into())
        .chain(errors.into_iter().map(|e| Error::from(e).into()))
        .collect();
    (ast, warningsanderrors)
}

pub fn get_ast(input: &str, file_name: &str) -> typed_ast::TypedModuleDeclaration {
    let ts = TokenStream::from_source(input);
    let ParserReturns {
        ast: module,
        loc: _,
        warnings: _,
        errors: _,
    } = Parser::from_stream(ts).module(file_name.to_string());
    // TODO! better report errors.
    // TODO! get prelude.
    let dep_tree = module
        .get_dependencies()
        .into_iter()
        .map(|(k, v)| (k, v.into_iter().collect()))
        .collect();
    let ops: HashMap<_, _> = [(
        "+".to_string(),
        vec![types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32))],
    )]
    .into();
    let mut infer_context = inference::Context::new(
        dep_tree,
        HashMap::new(),
        HashMap::new(),
        ops.clone(),
        HashMap::new(),
    );
    let ast = infer_context.inference(module);
    TypedModuleDeclaration::from(ast, &HashMap::new(), &ops)
}

pub fn from_file<'ctx>(
    file: &PathBuf,
    fwd_declarations: HashMap<String, ResolvedType>,
    fwd_ops : HashMap<String,Vec<ResolvedType>>,
    project_name: String,
) -> (Result<TypedModuleDeclaration, Vec<Error>>, Vec<Warning>) {
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
    let contents = match std::fs::read_to_string(file).map_err(Error::from) {
        Ok(contents) => contents,
        Err(e) => return (Err(vec![e]), Vec::new()),
    };
    // lexer
    let strm = TokenStream::from_source(&contents);
    // parser
    let parser = Parser::from_stream(strm);

    let ParserReturns {
        mut ast,
        loc: _,
        warnings,
        errors,
    } = parser.module(file_name.to_str().unwrap().to_string());
    let warnings = warnings.into_iter().map(Warning::from).collect_vec();
    let errors = errors.into_iter().map(Error::from).collect_vec();
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
        fwd_ops.clone(),
        HashMap::new(),
    );
    let ast = inference_context.inference(ast);
    
    let mut ast = TypedModuleDeclaration::from(ast, &fwd_declarations, &fwd_ops); //TODO: foward declare std lib
    // #[cfg(debug_assertions)]
    // println!("{:?}", ast.declarations);
    use std::io::Write;
    ast.lower_generics(&HashMap::new());
    (
        if errors.is_empty() {
            Ok(ast)
        } else {
            Err(errors)
        },
        warnings,
    )
}

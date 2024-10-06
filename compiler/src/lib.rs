use std::path::PathBuf;
use std::{cmp::Ordering, collections::HashMap};

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

use typed_ast::{
    TypedDeclaration, TypedExpr, TypedFnCall, TypedModuleDeclaration, TypedTopLevelValue,
    TypedValueType,
};

#[derive(Error, Debug)]
#[error(transparent)]
pub enum Error {
    // ParseError(#[from] parser::ParseError),
    Io(#[from] std::io::Error),
}

#[derive(Error, Debug)]
#[error(transparent)]
pub enum Warning {
    // Parsing(#[from] parser::Warning),
}

#[derive(Error, Debug)]
#[error(transparent)]
pub enum WarningAndError {
    Error(#[from] Error),
    Warning(#[from] Warning),
}

// use lexer::TokenStream;
// use parser::{Parser, ParserReturns};
use types::ResolvedType;
type Location = (usize, usize);

pub fn get_untyped_ast(
    input: &str,
    file_name: &str,
) -> (ast::ModuleDeclaration, Vec<WarningAndError>) {
    // let ts = TokenStream::from_source(input);
    // let ParserReturns {
    //     ast,
    //     loc: _,
    //     warnings,
    //     errors,
    // } = Parser::from_stream(ts).module(file_name.to_string());
    // let warningsanderrors = warnings
    //     .into_iter()
    //     .map(|w| Warning::from(w).into())
    //     .chain(errors.into_iter().map(|e| Error::from(e).into()))
    //     .collect();
    // (ast, warningsanderrors)
    todo!("fix parsing.")
}

pub fn get_ast(input: &str, file_name: &str) -> typed_ast::TypedModuleDeclaration {
    todo!("fix parsing.")
    // let ts = TokenStream::from_source(input);
    // let ParserReturns {
    //     ast: module,
    //     loc: _,
    //     warnings: _,
    //     errors: _,
    // } = Parser::from_stream(ts).module(file_name.to_string());
    // // TODO! better report errors.
    // // TODO! get prelude.
    // let dep_tree = module
    //     .get_dependencies()
    //     .into_iter()
    //     .map(|(k, v)| (k, v.into_iter().collect()))
    //     .collect();
    // let ops: HashMap<_, _> = [(
    //     "+".to_string(),
    //     vec![types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32))],
    // )]
    // .into();
    // let mut infer_context = inference::Context::new(
    //     dep_tree,
    //     HashMap::new(),
    //     HashMap::new(),
    //     ops.clone(),
    //     HashMap::new(),
    // );
    // let ast = infer_context.inference(module);
    // TypedModuleDeclaration::from(ast, &HashMap::new(), &ops)
}
fn unfold_global_curries(
    mut ast: TypedModuleDeclaration,
    external_globals: HashMap<String, TypedExpr>,
    dtree: HashMap<String, Vec<String>>,
) -> TypedModuleDeclaration {
    let decls = &mut ast.declarations;
    let order = dtree
        .into_iter()
        .sorted_by(|(lhs_name, lhs_depends), (rhs_name, rhs_depends)| {
            match (
                lhs_depends.contains(rhs_name),
                rhs_depends.contains(lhs_name),
            ) {
                (true, true) => {
                    todo!("remove this case.  both lhs and rhs. means they depend on each other.")
                }
                (false, true) => Ordering::Less, //right depends on left thus should appear after.
                (true, false) => Ordering::Greater, //left depends on right thus should appear after.
                (false, false) => Ordering::Equal,  //neither depend on each other.
            }
        })
        .map(|(a, _)| a)
        .collect_vec();
    let mut values = external_globals;
    values.extend(
        decls
            .iter()
            .filter(|decl| {
                if let TypedDeclaration::Value(TypedTopLevelValue { args, .. }) = decl {
                    args.is_empty()
                } else {
                    false
                }
            })
            .map(|decl| {
                let TypedDeclaration::Value(decl) = decl else {
                    unreachable!()
                };
                let TypedValueType::Expr(expr) = &decl.value else {
                    unreachable!()
                };
                (decl.ident.clone(), expr.clone())
            }),
    );
    for decl in decls
        .iter_mut()
        .filter(|decl| {
            if let TypedDeclaration::Value(TypedTopLevelValue { args, .. }) = decl {
                args.is_empty()
            } else {
                false
            }
        })
        .sorted_by_cached_key(|decl| order.iter().position(|name| name == &decl.get_ident()))
    {
        let TypedDeclaration::Value(decl) = decl else {
            unreachable!()
        };
        let TypedValueType::Expr(expr) = &mut decl.value else {
            unreachable!()
        };
        replace_values(expr, &values);
        values.insert(decl.ident.clone(), expr.clone());
    }
    ast
}

fn replace_values(expr: &mut TypedExpr, values: &HashMap<String, TypedExpr>) {
    match expr {
        TypedExpr::FnCall(call) if call.is_extern => {
            call.arg
                .as_mut()
                .map(|arg| replace_values(arg.as_mut(), values));
        }
        TypedExpr::FnCall(call) => {
            replace_values(call.value.as_mut(), values);
            call.arg
                .as_mut()
                .map(|expr| replace_values(expr.as_mut(), values));
        }
        TypedExpr::ValueRead(name, _, _) => {
            if let Some(new_expr) = values.get(name) {
                *expr = new_expr.clone();
            }
        }
        TypedExpr::TupleLiteral { contents, .. }
        | TypedExpr::ListLiteral { contents }
        | TypedExpr::ArrayLiteral { contents, .. } => {
            for expr in contents {
                replace_values(expr, values);
            }
        }
        TypedExpr::BinaryOpCall(biop) => {
            replace_values(&mut biop.lhs, values);
            replace_values(&mut biop.rhs, values);
        }
        _ => (),
    }
}

pub fn from_file<'ctx>(
    file: &PathBuf,
    fwd_declarations: HashMap<String, ResolvedType>,
    fwd_ops: HashMap<String, Vec<ResolvedType>>,
    project_name: String,
) -> (Result<TypedModuleDeclaration, Vec<Error>>, Vec<Warning>) {
    /* 
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
    let dependency_tree: HashMap<_, _> = dependency_graph
        .into_iter()
        .map(|(key, value)| (key, value.into_iter().collect()))
        .collect();
    let mut known_types = fwd_declarations.clone();

    known_types.extend(ast.get_types());
    let mut inference_context = inference::Context::new(
        dependency_tree.clone(),
        known_types,
        HashMap::new(),
        fwd_ops.clone(),
        HashMap::new(),
    );
    let ast = inference_context.inference(ast);
    let mut ast = TypedModuleDeclaration::from(ast, &fwd_declarations, &fwd_ops); //TODO: foward declare std lib
    ast.lower_generics(&HashMap::new());
    let ast = unfold_global_curries(ast, HashMap::new(), dependency_tree);
    (
        if errors.is_empty() {
            Ok(ast)
        } else {
            Err(errors)
        },
        warnings,
    )
    */
    todo!("fix parsing.")
}

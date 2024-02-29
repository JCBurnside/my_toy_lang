use std::collections::HashMap;

use clap::Parser;
use inkwell::{
    context::Context,
    targets::{InitializationConfig, Target},
    types::BasicType,
    OptimizationLevel,
};

use compiler::types::{self, ResolvedType};

mod cli_args;
mod jitstd;

fn main() {
    let args = cli_args::Arguments::parse();
    if !args.run && !args.output_llvm {
        println!("output of files not supported yet.");
        return;
    }

    if !args.file.exists() {
        println!(
            "input file {} does not exist",
            args.file.as_path().to_string_lossy()
        );
        return;
    }

    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let mut fwd_decl = HashMap::new();
    fwd_decl.insert(
        "print_str".to_owned(),
        ResolvedType::Function {
            arg: Box::new(types::STR),
            returns: Box::new(ResolvedType::Void),
            loc: (0, 0),
        },
    );

    let program = compiler::from_file(&args.file, fwd_decl.clone(), args.debug, "jit".to_string());

    match program {
        Err(errors) => {
            for err in errors {
                println!("{}", err)
            }
        }
        Ok(ast) => {
            if args.run {
                let mut jit = llvm_codegen::create_jit_runtime();
                jit.add_declarations(ast.declarations);
                // TODO: Jit redirects.
                unsafe {
                    jit.run_function::<unsafe extern "C" fn()>("main", ());
                }
            } else if args.output_llvm {
                llvm_codegen::compile_file(ast, args.file, args.out_file, fwd_decl)
            }
        }
    }
}

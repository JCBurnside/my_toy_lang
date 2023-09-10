use std::{collections::HashMap, fs::File, io::BufReader, path::PathBuf};

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
    let ctx = Context::create();
    let mut fwd_decl = HashMap::new();
    fwd_decl.insert(
        "print_str".to_owned(),
        ResolvedType::Function {
            arg: Box::new(types::STR),
            returns: Box::new(ResolvedType::Void),
        },
    );

    let program = compiler::from_file(&args.file, &ctx, fwd_decl, args.debug, "jit".to_string());
    match program {
        Err(errors) => {
            for err in errors {
                println!("{}", err)
            }
        }
        Ok(module) => {
            if args.run {
                let jit = module
                    .create_jit_execution_engine(OptimizationLevel::None)
                    .unwrap();
                let Some(str_t) = ctx.get_struct_type("str") else { unreachable!("how do you not have the basic str type????")};
                let jitstd = ctx.create_module("jitstd");
                let fun = jitstd::add_printstr(&ctx, &jitstd, str_t.as_basic_type_enum());
                jit.add_global_mapping(&fun, jitstd::print_str as usize);
                jit.add_module(&jitstd).unwrap();
                jitstd.print_to_file("./jitstd.llvm").unwrap();
                // TODO: Jit redirects.
                unsafe {
                    jit.get_function::<unsafe extern "C" fn()>("main")
                        .unwrap()
                        .call();
                }
            } else if args.output_llvm {
                if let Some(output) = args.out_file {
                    module.print_to_file(output).unwrap();
                } else {
                    let llvm = module.print_to_string();
                    println!("{}", llvm)
                }
            }
        }
    }
}

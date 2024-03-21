use std::collections::HashMap;

use clap::Parser;

use compiler::types::{self, ResolvedType};

use std::io::Write;

mod cli_args;

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
    let mut fwd_decl = HashMap::new();
    fwd_decl.insert(
        "print_str".to_owned(),
        ResolvedType::Function {
            arg: Box::new(types::STR),
            returns: Box::new(ResolvedType::Void),
            loc: (0, 0),
        },
    );

    let (program, _warnings) = compiler::from_file(&args.file, fwd_decl.clone(), [
        (
            "+".to_string(),
            vec![
                types::FLOAT64.fn_ty(&types::FLOAT64.fn_ty(&types::FLOAT64)), // float64 -> float64 -> float64
                types::FLOAT32.fn_ty(&types::FLOAT32.fn_ty(&types::FLOAT32)), // float32 -> float32 -> float32
                types::INT64.fn_ty(&types::INT64.fn_ty(&types::INT64)), // int64 -> int64 -> int64
                types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32)), // int32 -> int32 -> int32
                types::INT16.fn_ty(&types::INT16.fn_ty(&types::INT16)), // int16 -> int16 -> int16
                types::INT8.fn_ty(&types::INT8.fn_ty(&types::INT8)), // int8 -> int8 -> int8
            ]
        ),
        (
            "-".to_string(),
            vec![
                types::FLOAT64.fn_ty(&types::FLOAT64.fn_ty(&types::FLOAT64)), // float64 -> float64 -> float64
                types::FLOAT32.fn_ty(&types::FLOAT32.fn_ty(&types::FLOAT32)), // float32 -> float32 -> float32
                types::INT64.fn_ty(&types::INT64.fn_ty(&types::INT64)), // int64 -> int64 -> int64
                types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32)), // int32 -> int32 -> int32
                types::INT16.fn_ty(&types::INT16.fn_ty(&types::INT16)), // int16 -> int16 -> int16
                types::INT8.fn_ty(&types::INT8.fn_ty(&types::INT8)), // int8 -> int8 -> int8
            ]
        ),
        (
            "*".to_string(),
            vec![
                types::FLOAT64.fn_ty(&types::FLOAT64.fn_ty(&types::FLOAT64)), // float64 -> float64 -> float64
                types::FLOAT32.fn_ty(&types::FLOAT32.fn_ty(&types::FLOAT32)), // float32 -> float32 -> float32
                types::INT64.fn_ty(&types::INT64.fn_ty(&types::INT64)), // int64 -> int64 -> int64
                types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32)), // int32 -> int32 -> int32
                types::INT16.fn_ty(&types::INT16.fn_ty(&types::INT16)), // int16 -> int16 -> int16
                types::INT8.fn_ty(&types::INT8.fn_ty(&types::INT8)), // int8 -> int8 -> int8
            ]
        ),
        (
            "/".to_string(),
            vec![
                types::FLOAT64.fn_ty(&types::FLOAT64.fn_ty(&types::FLOAT64)), // float64 -> float64 -> float64
                types::FLOAT32.fn_ty(&types::FLOAT32.fn_ty(&types::FLOAT32)), // float32 -> float32 -> float32
                types::INT64.fn_ty(&types::INT64.fn_ty(&types::INT64)), // int64 -> int64 -> int64
                types::INT32.fn_ty(&types::INT32.fn_ty(&types::INT32)), // int32 -> int32 -> int32
                types::INT16.fn_ty(&types::INT16.fn_ty(&types::INT16)), // int16 -> int16 -> int16
                types::INT8.fn_ty(&types::INT8.fn_ty(&types::INT8)), // int8 -> int8 -> int8
            ]
        ),
        (
            "&&".to_string(),
            vec![
                types::BOOL.fn_ty(&types::BOOL.fn_ty(&types::BOOL)),
            ]
        ),
        (
            "||".to_string(),
            vec![
                types::BOOL.fn_ty(&types::BOOL.fn_ty(&types::BOOL)),
            ]
        ),
        (
            "==".to_string(),
            vec![
                types::FLOAT64.fn_ty(&types::FLOAT64.fn_ty(&types::BOOL)), // float64 -> float64 -> bool
                types::FLOAT32.fn_ty(&types::FLOAT32.fn_ty(&types::BOOL)), // float32 -> float32 -> bool
                types::INT64.fn_ty(&types::INT64.fn_ty(&types::BOOL)), // int64 -> int64 -> bool
                types::INT32.fn_ty(&types::INT32.fn_ty(&types::BOOL)), // int32 -> int32 -> bool
                types::INT16.fn_ty(&types::INT16.fn_ty(&types::BOOL)), // int16 -> int16 -> bool
                types::INT8.fn_ty(&types::INT8.fn_ty(&types::BOOL)), // int8 -> int8 -> bool
                types::BOOL.fn_ty(&types::BOOL.fn_ty(&types::BOOL)), // bool -> bool -> bool
            ]
        ),
        (
            "!=".to_string(),
            vec![
                types::FLOAT64.fn_ty(&types::FLOAT64.fn_ty(&types::BOOL)), // float64 -> float64 -> bool
                types::FLOAT32.fn_ty(&types::FLOAT32.fn_ty(&types::BOOL)), // float32 -> float32 -> bool
                types::INT64.fn_ty(&types::INT64.fn_ty(&types::BOOL)), // int64 -> int64 -> bool
                types::INT32.fn_ty(&types::INT32.fn_ty(&types::BOOL)), // int32 -> int32 -> bool
                types::INT16.fn_ty(&types::INT16.fn_ty(&types::BOOL)), // int16 -> int16 -> bool
                types::INT8.fn_ty(&types::INT8.fn_ty(&types::BOOL)), // int8 -> int8 -> bool
                types::BOOL.fn_ty(&types::BOOL.fn_ty(&types::BOOL)), // bool -> bool -> bool
            ]
        ),
    ].into(),"jit".to_string());

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
                unsafe {
                    jit.run_function::<unsafe extern "C" fn()>("main", ());
                }
            } else if args.output_llvm {
                llvm_codegen::compile_file(ast, args.file, args.out_file, fwd_decl)
            }
        }
    }
}

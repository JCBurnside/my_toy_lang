mod code_gen;
mod type_resolver;
use std::{
    cell::OnceCell, collections::HashMap, hash::Hash, mem::MaybeUninit, path::PathBuf,
    process::Output, sync::OnceLock,
};

use code_gen::*;
use compiler::{typed_ast::TypedDeclaration, types::ResolvedType};
use inkwell::{
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module,
    object_file::ObjectFile,
    targets::{CodeModel, InitializationConfig, Target, TargetMachine},
};
use multimap::MultiMap;
use type_resolver::TypeResolver;

//TODO! move doing the forward declares into here.
pub fn compile_file(
    mut ast: compiler::typed_ast::FileTyped,
    file: PathBuf,
    write_llvm_to: Option<PathBuf>,
    fwd_decls: HashMap<String, ResolvedType>,
) {
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    ast.declarations.retain(|decl| !decl.is_generic());
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
    let ctx = Context::create();
    let module = ctx.create_module(file.file_stem().unwrap().to_str().unwrap());
    // set for the llvm module
    let type_resolver = TypeResolver::new(&ctx, target_machine.get_target_data());
    let code_gen = CodeGen::with_module(
        &ctx,
        module,
        type_resolver,
        fwd_decls,
        HashMap::new(),
        MultiMap::new(),
        target_machine.get_target_data(),
    );
    let module = code_gen.compile_program(vec![ast], true, false);
    if let Some(path) = write_llvm_to {
        module.print_to_file(path).unwrap()
    }
    //TODO! handle how to output built files?
}

self_cell::self_cell! {

    pub struct JitEngine {
        owner : Context,
        #[not_covariant]
        dependent : BoundItems,

    }
}

struct BoundItems<'ctx> {
    generator: CodeGen<'ctx>,
    engine: ExecutionEngine<'ctx>,
    modules: Vec<Module<'ctx>>, //to garuntee they don't get dropped early.
}

pub fn create_jit_runtime() -> JitEngine {
    Target::initialize_native(&InitializationConfig::default()).unwrap();
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
    let ctx = Context::create();
    JitEngine::new(ctx, |ctx| {
        let module = ctx.create_module("");
        // set for the llvm module
        let type_resolver = TypeResolver::new(&ctx, target_machine.get_target_data());
        let ee = module.create_execution_engine().unwrap();
        let gen = CodeGen::with_module(
            ctx,
            module,
            type_resolver,
            HashMap::new(),
            HashMap::new(),
            MultiMap::new(),
            target_machine.get_target_data(),
        );
        BoundItems {
            generator: gen,
            engine: ee,
            modules: Vec::new(),
        }
    })
}

pub trait TupleCall {
    type Args;
    type Output;
    unsafe fn call_tuple(&self, args: Self::Args) -> Self::Output;
}

macro_rules! impl_unsafe_fn {
    (@recurse $_:ident $( , $rest:ident )*) => {
        impl_unsafe_fn!($( $rest ),*);
    };

    (@recurse) => {};

    ($first:ident) => {
        impl<Output,  $first > TupleCall for JitFunction<'_,unsafe extern "C" fn($first) -> Output> {
            type Args = $first;
            type Output = Output;
            #[allow(non_snake_case)]
            unsafe fn call_tuple(&self,$first:$first) -> Output {
                self.call($first)
            }
        }
        impl<Output> TupleCall for JitFunction<'_,unsafe extern "C" fn() -> Output> {
            type Args=();
            type Output=Output;
            unsafe fn call_tuple(&self, ():()) -> Output {
                self.call()
            }
        }
    };

    ($( $param:ident ),*) => {
        impl<Output, $( $param ),*> TupleCall for JitFunction<'_,unsafe extern "C" fn($( $param ),*) -> Output> {
            type Args = ( $($param),* );
            type Output = Output;
            #[allow(non_snake_case)]
            unsafe fn call_tuple(&self,($($param),*):($($param),*)) -> Output {
                self.call($($param),*)
            }
        }

        impl_unsafe_fn!(@recurse $( $param ),*);
    };
}

impl_unsafe_fn!(A, B, C, D, E, F, G, H, I, J, K, L, M);

impl JitEngine {
    pub fn add_declarations(&mut self, mut decls: Vec<TypedDeclaration>) {
        decls.sort_by(|a, b| match (a, b) {
            (TypedDeclaration::Value(_), TypedDeclaration::Value(_)) => std::cmp::Ordering::Equal,
            (_, TypedDeclaration::Value(_)) => std::cmp::Ordering::Less,
            (TypedDeclaration::Value(_), _) => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        });
        self.with_dependent_mut(
            |ctx,
             BoundItems {
                 generator: gen,
                 modules,
                 ..
             }| {
                let new_module = ctx.create_module("");
                modules.push(gen.replace_module(new_module));
                for decl in &decls {
                    gen.create_define(decl);
                }

                #[cfg(debug_assertions)]
                let _ = gen.module.print_to_file("./debug.ll");

                for decl in decls {
                    gen.compile_decl(decl);
                }
            },
        )
    }
    /// must be mut as it changes the code gen's current module since once added to the generator it is invalid to add to the module.
    pub unsafe fn run_function<'this, 'a, F: UnsafeFunctionPointer>(
        &'this mut self,
        name: &str,
        args: <JitFunction<'a, F> as TupleCall>::Args,
    ) -> <JitFunction<'a, F> as TupleCall>::Output
    where
        JitFunction<'a, F>: TupleCall,
        'this: 'a,
    {
        self.with_dependent_mut(
            move |ctx,
                  BoundItems {
                      generator,
                      engine,
                      modules,
                  }| {
                let new_module = ctx.create_module("");
                let module = generator.replace_module(new_module);
                engine.add_module(&module).unwrap();
                modules.push(module);
                let fun = engine.get_function::<F>(name).unwrap();
                fun.call_tuple(args)
            },
        )
    }
}

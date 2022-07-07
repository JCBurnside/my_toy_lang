use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::{context::Context, module::Linkage};
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{FunctionValue, AnyValueEnum};

use crate::ast::{Expr, TypeName, try_map_type};

struct CodeGen<'ctx, 'module> {
    ctx: &'ctx Context,
    module: &'module Module<'ctx>,
    ast: Vec<Expr>,
    known_types: HashMap<String, AnyTypeEnum<'ctx>>,
    declarations: HashMap<String, AnyTypeEnum<'ctx>>,
}

impl<'ctx, 'module> CodeGen<'ctx, 'module> {
    pub fn new(
        ctx: &'ctx Context,
        module: &'module Module<'ctx>,
        ast: Vec<Expr>,
        known_types: HashMap<String, AnyTypeEnum<'ctx>>,
        declarations: HashMap<String, AnyTypeEnum<'ctx>>,
    ) -> Self {
        Self {
            ctx,
            module,
            ast,
            known_types,
            declarations,
        }
    }

    pub fn compile_program(self) -> Result<Vec<Box<dyn inkwell::values::AnyValue<'ctx>>>, &'static str> {
        let builder = self.ctx.create_builder();
        self.ast.iter().map(|expr| self.compile_expr(&builder, expr)).collect()
    }

    fn compile_expr(&self,builder:&Builder,expr:&Expr) -> Result<Box<dyn inkwell::values::AnyValue<'ctx>>,&'static str> {
        todo!()
    }

    fn compile_function(&mut self,builder:&Builder) -> Result<FunctionValue<'ctx>,&'static str> {
        if let Some(Expr::Declaration { ident, ty, is_op, args, value }) = self.ast.pop() {
            match ty.and_then(|ty| try_map_type(ty, &self.known_types)) {
                Some(ty) => {
                    if self.declarations.contains_key(&ident) {
                        return Err("Already declared");
                    }
                    self.declarations.insert(ident.clone(), ty.clone());
                    if !ty.is_function_type() {
                        Err("expected function")
                    } else {
                        let ty = ty.into_function_type();
                        let fun = self.module.add_function(&ident, ty, Some(Linkage::Private));
                        let block = self.ctx.append_basic_block(fun, &(ident + "_entry"));
                        builder.position_at_end(block);
                        self.compile_expr(builder, value.as_ref())?;
                        Ok(fun)
                    }
                }
                None => Err("Some type is not known")
            }
        } else {
            Err("Top Level not a declaration")
        }
    }
}

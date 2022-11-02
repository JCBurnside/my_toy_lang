use inkwell::{context::Context, module::Module, values::FunctionValue};

use multimap::MultiMap;

use crate::types::{self,TypeResolver};

pub fn get_std_ops<'ctx>(
    ctx: &'ctx Context,
    type_resolver: &mut TypeResolver<'ctx>,
) -> (Module<'ctx>, MultiMap<String, FunctionValue<'ctx>>) {
    let module = ctx.create_module("std::ops");
    let mut functions = MultiMap::new();
    functions.extend(promote_ops(ctx, type_resolver, &module));
    functions.extend(addition_ops(ctx, type_resolver, &module));
    functions.extend(subtraction_ops(ctx, type_resolver, &module));
    (module, functions)
}

fn promote_ops<'ctx>(
    ctx: &'ctx Context,
    type_resolver: &mut TypeResolver<'ctx>,
    module: &Module<'ctx>,
) -> MultiMap<String, FunctionValue<'ctx>> {
    const NAME: &'static str = "implicit_promote";
    let name = NAME.to_string();
    let mut out: MultiMap<String, FunctionValue<'ctx>> = MultiMap::new();
    let builder = ctx.create_builder();
    out.insert_many(name, []);
    for (lower, upper) in [
        (type_resolver.resolve_type_as_basic(types::INT8), type_resolver.resolve_type_as_basic(types::INT16)),
        (type_resolver.resolve_type_as_basic(types::INT8), type_resolver.resolve_type_as_basic(types::INT32)),
        (type_resolver.resolve_type_as_basic(types::INT8), type_resolver.resolve_type_as_basic(types::INT64)),
        (type_resolver.resolve_type_as_basic(types::INT16), type_resolver.resolve_type_as_basic(types::INT32)),
        (type_resolver.resolve_type_as_basic(types::INT16), type_resolver.resolve_type_as_basic(types::INT64)),
        (type_resolver.resolve_type_as_basic(types::INT32), type_resolver.resolve_type_as_basic(types::INT64)),
    ] {
        let (arg, rt) = (lower.into_int_type(), upper.into_int_type());
        let fun = module.add_function(NAME, rt.fn_type(&[arg.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let arg = fun.get_first_param().unwrap().into_int_value();
        builder.position_at_end(bb);
        let out_val = builder.build_int_s_extend(arg, rt, "");
        builder.build_return(Some(&out_val));
        out.get_vec_mut(NAME).unwrap().push(fun);
    }
    let arg = type_resolver.resolve_type_as_basic(types::FLOAT32).into_float_type();
    let rt = type_resolver.resolve_type_as_basic(types::FLOAT64).into_float_type();
    let fun = module.add_function(NAME, rt.fn_type(&[arg.into()], false), None);
    let bb = ctx.append_basic_block(fun, "");
    let arg = fun.get_first_param().unwrap().into_float_value();
    builder.position_at_end(bb);
    let out_val = builder.build_float_ext(arg, rt, "");
    builder.build_return(Some(&out_val));
    out.get_vec_mut(NAME).unwrap().push(fun);
    out
}

fn addition_ops<'ctx>(
    ctx: &'ctx Context,
    type_resolver: &mut TypeResolver<'ctx>,
    module: &Module<'ctx>,
) -> MultiMap<String, FunctionValue<'ctx>> {
    const NAME: &'static str = "addition";
    let name = NAME.to_string();
    let mut out: MultiMap<String, FunctionValue<'ctx>> = MultiMap::new();
    let builder = ctx.create_builder();
    out.insert_many(name, []);
    for ty in [
        type_resolver.resolve_type_as_basic(types::INT8),
        type_resolver.resolve_type_as_basic(types::INT16),
        type_resolver.resolve_type_as_basic(types::INT32),
        type_resolver.resolve_type_as_basic(types::INT64),
    ] {
        let ty = ty.into_int_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_int_value();
        let rhs = fun.get_nth_param(1).unwrap().into_int_value();
        builder.position_at_end(bb);
        let out_val = builder.build_int_add(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.get_vec_mut(NAME).unwrap().push(fun);
    }

    for ty in [type_resolver.resolve_type_as_basic(types::FLOAT32), type_resolver.resolve_type_as_basic(types::FLOAT64)] {
        let ty = ty.into_float_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_float_value();
        let rhs = fun.get_nth_param(1).unwrap().into_float_value();
        builder.position_at_end(bb);
        let out_val = builder.build_float_add(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.get_vec_mut(NAME).unwrap().push(fun);
    }

    out
}

fn subtraction_ops<'ctx>(
    ctx: &'ctx Context,
    type_resolver: &mut TypeResolver<'ctx>,
    module: &Module<'ctx>,
) -> MultiMap<String, FunctionValue<'ctx>> {
    const NAME: &'static str = "subtraction";
    let name = NAME.to_string();
    let mut out: MultiMap<String, FunctionValue<'ctx>> = MultiMap::new();
    let builder = ctx.create_builder();
    for ty in [
        type_resolver.resolve_type_as_basic(types::INT8),
        type_resolver.resolve_type_as_basic(types::INT16),
        type_resolver.resolve_type_as_basic(types::INT32),
        type_resolver.resolve_type_as_basic(types::INT64),
    ] {
        let ty = ty.into_int_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_int_value();
        let rhs = fun.get_nth_param(1).unwrap().into_int_value();
        builder.position_at_end(bb);
        let out_val = builder.build_int_sub(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.entry(name.clone())
            .or_insert_vec(Vec::with_capacity(4))
            .push(fun);
    }
    for ty in [type_resolver.resolve_type_as_basic(types::FLOAT32), type_resolver.resolve_type_as_basic(types::FLOAT64)] {
        let ty = ty.into_float_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_float_value();
        let rhs = fun.get_nth_param(1).unwrap().into_float_value();
        builder.position_at_end(bb);
        let out_val = builder.build_float_sub(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.get_vec_mut(NAME).unwrap().push(fun);
    }

    out
}

fn multiplication_ops<'ctx>(
    ctx: &'ctx Context,
    type_resolver: &mut TypeResolver<'ctx>,
    module: &Module<'ctx>,
) -> MultiMap<String, FunctionValue<'ctx>> {
    const NAME: &'static str = "multiplication";
    let name = NAME.to_string();
    let mut out: MultiMap<String, FunctionValue<'ctx>> = MultiMap::new();
    let builder = ctx.create_builder();
    for ty in [
        type_resolver.resolve_type_as_basic(types::INT8),
        type_resolver.resolve_type_as_basic(types::INT16),
        type_resolver.resolve_type_as_basic(types::INT32),
        type_resolver.resolve_type_as_basic(types::INT64),
    ] {
        let ty = ty.into_int_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_int_value();
        let rhs = fun.get_nth_param(1).unwrap().into_int_value();
        builder.position_at_end(bb);
        let out_val = builder.build_int_mul(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.entry(name.clone())
            .or_insert_vec(Vec::with_capacity(4))
            .push(fun);
    }

    for ty in [type_resolver.resolve_type_as_basic(types::FLOAT32), type_resolver.resolve_type_as_basic(types::FLOAT64)] {
        let ty = ty.into_float_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_float_value();
        let rhs = fun.get_nth_param(1).unwrap().into_float_value();
        builder.position_at_end(bb);
        let out_val = builder.build_float_add(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.get_vec_mut(NAME).unwrap().push(fun);
    }
    out
}

fn division_ops<'ctx>(
    ctx: &'ctx Context,
    type_resolver: &mut TypeResolver<'ctx>,
    module: &Module<'ctx>,
) -> MultiMap<String, FunctionValue<'ctx>> {
    const NAME: &'static str = "division";
    let name = NAME.to_string();
    let mut out: MultiMap<String, FunctionValue<'ctx>> = MultiMap::new();
    let builder = ctx.create_builder();
    for ty in [
        type_resolver.resolve_type_as_basic(types::INT8),
        type_resolver.resolve_type_as_basic(types::INT16),
        type_resolver.resolve_type_as_basic(types::INT32),
        type_resolver.resolve_type_as_basic(types::INT64),
    ] {
        let ty = ty.into_int_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_int_value();
        let rhs = fun.get_nth_param(1).unwrap().into_int_value();
        builder.position_at_end(bb);
        let out_val = builder.build_int_signed_div(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.entry(name.clone())
            .or_insert_vec(Vec::with_capacity(4))
            .push(fun);
    }

    for ty in [type_resolver.resolve_type_as_basic(types::FLOAT32), type_resolver.resolve_type_as_basic(types::FLOAT64)] {
        let ty = ty.into_float_type();
        let fun = module.add_function(NAME, ty.fn_type(&[ty.into(), ty.into()], false), None);
        let bb = ctx.append_basic_block(fun, "");
        let lhs = fun.get_first_param().unwrap().into_float_value();
        let rhs = fun.get_nth_param(1).unwrap().into_float_value();
        builder.position_at_end(bb);
        let out_val = builder.build_float_div(lhs, rhs, "");
        builder.build_return(Some(&out_val));
        out.get_vec_mut(NAME).unwrap().push(fun);
    }

    out
}

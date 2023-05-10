use inkwell::{context::Context, module::Module, types::{PointerType, BasicTypeEnum, BasicType}, IntPredicate, AddressSpace};

pub fn add_printstr<'ctx>(ctx:&'ctx Context, module : &Module<'ctx>, str_t : BasicTypeEnum<'ctx>) {

    let putchar = module.add_function("putchar", ctx.i32_type().fn_type(&[ctx.i8_type().into()], false), None);
    let fun_ty = ctx.void_type().fn_type(&[str_t.ptr_type(AddressSpace::default()).into()], false);
    let builder = ctx.create_builder();
    let fun = module.add_function("print_str", fun_ty, None);
    let bb = ctx.append_basic_block(fun, "");
    builder.position_at_end(bb);
    // let local_str = builder.build_alloca(str_t, "");
    // let param_str = fun.get_first_param().unwrap();
    // let param_str = builder.build_load(param_str.into_pointer_value(),"");
    // builder.build_store(local_str,param_str);
    let start = builder.build_alloca(str_t.into_struct_type().get_field_type_at_index(0).unwrap(), "start");
    let start_inner = builder.build_struct_gep(fun.get_first_param().unwrap().into_pointer_value(), 0, "").unwrap();
    let start_inner = builder.build_load(start_inner, "");
    builder.build_store(start,start_inner);
    let end = builder.build_alloca(str_t.into_struct_type().get_field_type_at_index(1).unwrap(), "end");
    let end_inner = builder.build_struct_gep(fun.get_first_param().unwrap().into_pointer_value(), 1, "").unwrap();
    let end_inner = builder.build_load(end_inner, "");
    builder.build_store(end,end_inner);
    let loop_start = ctx.append_basic_block(fun, "");
    let loop_point = ctx.append_basic_block(fun, "");
    let exit_point = ctx.append_basic_block(fun, "exit");
    builder.build_unconditional_branch(loop_start);
    builder.position_at_end(loop_start);
    let start_load = builder.build_load(start,"").into_pointer_value();
    let end_load = builder.build_load(end,"").into_pointer_value();
    let cmp = builder.build_int_compare(IntPredicate::NE, start_load, end_load, "");
    builder.build_conditional_branch(cmp, loop_point, exit_point);
    builder.position_at_end(loop_point);
    let val = builder.build_load(builder.build_load(start,"").into_pointer_value(), "");
    builder.build_call(putchar,&[val.into()],"");
    let start_load = builder.build_load(start,"").into_pointer_value();
    let new_start = unsafe { builder.build_in_bounds_gep(start_load, &[ctx.i8_type().const_int(1, false)], "") };
    builder.build_store(start,new_start);
    builder.build_unconditional_branch(loop_start);
    builder.position_at_end(exit_point);
    builder.build_return(None);
}
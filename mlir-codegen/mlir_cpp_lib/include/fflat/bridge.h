#pragma once
#include "rust/cxx.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"
#include <memory>
namespace ast {
struct Expr;
struct Statement;
struct ModuleDecl;
struct TopLevelDeclaration;
struct TopLevelValue;
struct TypeDeclaration;
struct StructDef;
struct EnumDef;
} // namespace ast

namespace fflat {
struct Builder {
    Builder();
    ::mlir::MLIRContext ctx;
    ::mlir::OpBuilder builder;
    ::mlir::OwningOpRef<::mlir::ModuleOp> mod;
    void add_module(::ast::ModuleDecl const& mod);
    
    void dump() ;
    void write_file(rust::String path);
    rust::String write_string();
};

::std::unique_ptr<Builder> make_builder();

} // namespace fflat
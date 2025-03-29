#include "FflatMlir/dialect.h"
#include "FflatMlir/fflat_types.h"
#include "FflatMlir/FflatOpsDialect.cpp.inc"
#include "FflatMlir/FflatOpsTypes.h.inc"
#include "mlir/IR/AsmState.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"
#include "fflat/bridge.h"


void mlir::fflat::FflatDialect::initialize() {
    addOperations<
    #define GET_OP_LIST
    #include "FflatMlir/FflatOps.cpp.inc"
    >();

    addTypes<
    mlir::fflat::types::FunctionType,
    mlir::fflat::types::StructType,
    mlir::fflat::types::TupleType
    >();
}

void mlir::fflat::FflatDialect::printType(
    ::mlir::Type ty,
    ::mlir::DialectAsmPrinter& printer
) const {
    if(mlir::isa<mlir::fflat::types::FunctionType>(ty)) {
        auto fun = mlir::cast<mlir::fflat::types::FunctionType>(ty);
        fun.print(printer);
    } else if(mlir::isa<mlir::fflat::types::TupleType>(ty)) {
        auto tuple = mlir::cast<mlir::fflat::types::TupleType>(ty);
        tuple.print(printer);
    }
}

::mlir::Type mlir::fflat::FflatDialect::parseType(::mlir::DialectAsmParser& parser) const {
    ::mlir::Type out;
    if(mlir::fflat::types::FunctionType::parse(parser,out)) {
        return out;
    }
    return mlir::Type();
}

void fflat::test() {
    mlir::registerAsmPrinterCLOptions();
    mlir::registerMLIRContextCLOptions();
    auto ctx = mlir::MLIRContext();
    ctx.getOrLoadDialect<mlir::fflat::FflatDialect>();
    auto builder = mlir::OpBuilder(&ctx);
    auto mod = mlir::ModuleOp::create(builder.getUnknownLoc());
    auto ty = mlir::fflat::types::FunctionType::get(builder.getI8Type(), builder.getI8Type());
    builder.setInsertionPointToEnd(mod.getBody());
    auto _ = builder.create<mlir::fflat::TopLevelValue>(builder.getUnknownLoc(),"test", ty);
    if(llvm::failed(mod.verify()) || !mod) {
        ::llvm::errs()<<"not able to verify";
    } else {
        mod.dump();
    }
}


#define GET_OP_CLASSES
#include "FflatMlir/FflatOps.cpp.inc"
#include "FflatMlir/dialect.h"
#include "FflatMlir/FflatOpsDialect.cpp.inc"
#include "FflatMlir/types.h"
#include "FflatMlir/FflatOpsTypes.h.inc"
void mlir::fflat::FflatDialect::initialize() {
    addOperations<
    #define GET_OP_LIST
    #include "FflatMlir/FflatOps.cpp.inc"
    >();

    addTypes<
    mlir::fflat::FunctionType
    >();
}

void foo() {}


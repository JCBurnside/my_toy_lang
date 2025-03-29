#include "fflat/bridge.h"
#include "mlir-codegen/src/bridge.rs.h"
#include "FflatMlir/dialect.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/OwningOpRef.h"
#include <fstream>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <stdexcept>
#include <system_error>

std::unique_ptr<fflat::Builder> fflat::make_builder() {
    return std::make_unique<fflat::Builder>();
}

fflat::Builder::Builder() : ctx(), builder(&ctx) {
    ctx.getOrLoadDialect<mlir::fflat::FflatDialect>();
    mod = mlir::ModuleOp::create(builder.getUnknownLoc());
}

using namespace fflat;

void Builder::dump() {
    mod->dump();
}

void Builder::write_file(rust::String path_rs) {
    auto path = static_cast<std::string>(path_rs);
    std::error_code ec; 
    ::llvm::raw_fd_ostream fstream(path,ec);
    if(ec) {
        std::cerr << ec.message();
        return;
    }
    mod->print(fstream);
}

rust::String Builder::write_string() {
    std::string out;
    llvm::raw_string_ostream writer(out);
    mod->print(writer);
    return out;
}
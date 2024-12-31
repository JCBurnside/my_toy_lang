#include "FflatMlir/types.h"

bool mlir::fflat::operator==(const FunctionTypeStorage & lhs, const FunctionTypeStorage::KeyTy & rhs) {
    return lhs.arg == std::get<0>(rhs)
    && lhs.ret == std::get<1>(rhs);
}
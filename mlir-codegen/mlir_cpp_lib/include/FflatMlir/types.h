#pragma once
#include "mlir/IR/Types.h"
#include "mlir/IR/TypeSupport.h"
#include <llvm-18/llvm/ADT/Hashing.h>
#include <llvm-18/llvm/ADT/StringRef.h>
#include <tuple>
#include <utility>
namespace mlir::fflat {
// enum class FunctionMutability {
//     Default,
//     Mut,
//     Once,
// };
class FunctionType;
struct FunctionTypeStorage : ::mlir::TypeStorage {
    using KeyTy = std::pair<::mlir::Type,::mlir::Type>;
    FunctionTypeStorage(::mlir::Type arg, ::mlir::Type ret) : arg(arg), ret(ret) {}

    ::mlir::Type arg;
    ::mlir::Type ret;
    // [[maybe_unused]]
    // FunctionMutability _safety = FunctionMutability::Default;

    static ::llvm::hash_code hashKey(const KeyTy& key) {
        return llvm::hash_value(key);
    }

    static KeyTy getKey(::mlir::Type arg, ::mlir::Type ret/*, [[maybe_unused]] FunctionMutability _safety = FunctionMutability::Default */) {
        return std::make_pair(arg, ret);
    }

    static FunctionTypeStorage* construct(
        ::mlir::TypeStorageAllocator& allocator,
        KeyTy key
    ) {

        return new (allocator.allocate<FunctionTypeStorage>()) FunctionTypeStorage(
            key.first,
            key.second
        );
    }
};
bool operator==(FunctionTypeStorage const&, FunctionTypeStorage::KeyTy const&);

class FunctionType : public ::mlir::Type::TypeBase<FunctionType, ::mlir::Type, FunctionTypeStorage> {
public:
    using Base::Base;

    static FunctionType get(::mlir::Type arg, ::mlir::Type ret) {
        auto ctx = arg.getContext();

        return Base::get(ctx, arg, ret);
    }

    ::mlir::Type getArg() const {
        return getImpl()->arg;
    }

    ::mlir::Type getRet() const {
        return getImpl()->ret;
    }

    static constexpr ::llvm::StringLiteral name = "func";
};
}
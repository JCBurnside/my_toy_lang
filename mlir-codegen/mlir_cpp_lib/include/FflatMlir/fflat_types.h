#pragma once
#include "mlir/IR/Types.h"
#include "mlir/IR/TypeSupport.h"
#include <llvm/ADT/Hashing.h>
#include <llvm/ADT/StringRef.h>
#include <tuple>
#include <utility>

#include <mlir/IR/Dialect.h>
#include <mlir/IR/DialectImplementation.h>
namespace mlir::fflat::types {
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

    void print(::mlir::DialectAsmPrinter& printer) const;
    static bool parse(::mlir::DialectAsmParser& parser, ::mlir::Type& out);
};


class StructType;
struct StructTypeStorage : ::mlir::TypeStorage {
    using KeyTy = ::llvm::ArrayRef<::mlir::Type>;

    ::llvm::ArrayRef<::mlir::Type> elements;

    StructTypeStorage(::llvm::ArrayRef<::mlir::Type> elements) : elements(elements){}

    bool operator==(KeyTy const& key) const { return key == elements; }

    static ::llvm::hash_code hashKey(KeyTy const& key) {
        return ::llvm::hash_value(key);
    }

    static KeyTy getKey(::llvm::ArrayRef<::mlir::Type> elements) {
        return KeyTy(elements);
    }

    static StructTypeStorage* construct(::mlir::TypeStorageAllocator& allocator, KeyTy const& key) {
        auto elements = allocator.copyInto(key);
        return new (allocator.allocate<StructTypeStorage>()) StructTypeStorage(
            elements
        );
    }
};
class StructType : public ::mlir::Type::TypeBase<StructType, ::mlir::Type, StructTypeStorage>{
public:
    using Base::Base;

    using Base::get;

    ::llvm::ArrayRef<::mlir::Type> getElements() const{
        return getImpl()->elements;
    }

    std::size_t getNumElements() const {
        return getElements().size();
    }
    static constexpr ::llvm::StringLiteral name = "struct";
};

struct TupleType;
struct TupleTypeStorage : ::mlir::TypeStorage {
    using KeyTy = ::llvm::ArrayRef<::mlir::Type>;
    ::llvm::ArrayRef<::mlir::Type> elements;
    TupleTypeStorage(::llvm::ArrayRef<::mlir::Type> elements) : elements(elements) {}

    static ::llvm::hash_code hashKey(KeyTy const& key) {
        return llvm::hash_value(key);
    } 

    static KeyTy getKey(::llvm::ArrayRef<::mlir::Type> elements) {
        return KeyTy(elements);
    }

    static TupleTypeStorage* construct(::mlir::TypeStorageAllocator& alloc, KeyTy key) {
        auto elements = alloc.copyInto(key);

        return new (alloc.allocate<TupleTypeStorage>()) TupleTypeStorage(
            elements
        );
    }
};
bool operator==(TupleTypeStorage const& lhs, TupleTypeStorage::KeyTy rhs);

struct TupleType : ::mlir::Type::TypeBase<TupleType,::mlir::Type,TupleTypeStorage> {
    using Base::Base;

    static TupleType get(::llvm::ArrayRef<::mlir::Type> key){
        assert(key.size()>0 && "there must be at least one element in a tuple");
        auto ctx = key.front().getContext();

        return Base::get(ctx, key);
    }

    ::llvm::ArrayRef<::mlir::Type> getTypes() const {
        return getImpl()->elements;
    }

    std::size_t getNumElements() const {
        return getTypes().size();
    }

    void print(::mlir::DialectAsmPrinter& printer) const;

    static bool parse(::mlir::DialectAsmParser& parser, ::mlir::Type& out);
    static constexpr ::llvm::StringLiteral name = "tuple";
};

// struct DependentType;
// struct DependentTypeStorage : ::mlir::TypeStorage {
//     using KeyTy = std::pair<::llvm::StringRef, ::llvm::StringRef>;

//     ::llvm::StringRef base;
//     ::llvm::StringRef ident;

//     DependentTypeStorage(::llvm::StringRef base, ::llvm::StringRef ident) : base(base), ident(ident) {}
    
//     static ::llvm::hash_code hashKey(const KeyTy& key) {
//         return llvm::hash_value(key);
//     }

//     static KeyTy getKey(::llvm::StringRef base, ::llvm::StringRef ident) {
//         return std::make_pair(base,ident);
//     }

//     static DependentTypeStorage* construct(
//         ::mlir::TypeStorageAllocator& alloc,
//         KeyTy key
//     ) {
//         auto base = alloc.copyInto(std::get<0>(key));
//         auto ident = alloc.copyInto(std::get<1>(key));

//         return new (alloc.allocate<DependentTypeStorage>()) DependentTypeStorage (
//             base,
//             ident
//         );
//     }
// };

// bool operator==(DependentTypeStorage const& lhs, DependentTypeStorage::KeyTy rhs);

// struct DependentType : ::mlir::Type::TypeBase<DependentType,::mlir::Type,DependentTypeStorage> {

// };

    bool is_fflat_type(::mlir::Type ty);
}
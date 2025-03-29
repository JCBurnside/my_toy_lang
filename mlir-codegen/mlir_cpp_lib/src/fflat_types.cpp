#include "FflatMlir/fflat_types.h"

bool mlir::fflat::types::operator==(const FunctionTypeStorage & lhs, const FunctionTypeStorage::KeyTy & rhs) {
    return lhs.arg == std::get<0>(rhs)
    && lhs.ret == std::get<1>(rhs);
}

bool mlir::fflat::types::operator==(TupleTypeStorage const &lhs, TupleTypeStorage::KeyTy rhs)
{
    return lhs.elements == rhs;
}

// bool mlir::fflat::operator==(DependentTypeStorage const &lhs, DependentTypeStorage::KeyTy rhs)
// {
//     return lhs.base == std::get<0>(key)
//     && lhs.ident == std::get<1>(key);
// }

void mlir::fflat::types::FunctionType::print(::mlir::DialectAsmPrinter &printer) const
{
        printer<<"func<"
        << getArg()
        << " -> "
        << getRet()
        << '>'
        ;
}
bool mlir::fflat::types::FunctionType::parse(::mlir::DialectAsmParser &parser, ::mlir::Type &out)
{

    if(parser.parseKeyword("func") || parser.parseLess()) {return false;}
    ::mlir::Type arg;
    if(parser.parseType(arg)) { return false; }
    auto arg_loc = parser.getCurrentLocation();
    if(!is_fflat_type(arg)) {
        parser.emitError(arg_loc, "arg must be a fflat compatible type but got")
            << arg;
        out = nullptr;
        return true;
    }

    if(parser.parseArrow()) { 
        return true; 
    }
    ::mlir::Type ret;
    auto ret_loc = parser.getCurrentLocation();
    if(!is_fflat_type(ret)) {
        parser.emitError(ret_loc, "element must be a fflat compatible type but got")
            << ret;
        out = nullptr;
        return true;
    }
    if(parser.parseGreater()) {
        return false;
    }
    out = FunctionType::get(arg,ret);
    return true;
}
void mlir::fflat::types::TupleType::print(::mlir::DialectAsmPrinter &printer) const
{
    printer << "tuple[";
    llvm::interleaveComma(getTypes(),printer);
    printer << "]";
}

bool mlir::fflat::types::TupleType::parse(::mlir::DialectAsmParser &parser, ::mlir::Type &out)
{
    if(parser.parseKeyword("tuple") || parser.parseLSquare()) { return false; }
    ::llvm::SmallVector<::mlir::Type,1> elements;

    do {
        auto loc = parser.getCurrentLocation();
        ::mlir::Type element;
        if(parser.parseType(element)) {return false; }
        if(!is_fflat_type(element)) {
            parser.emitError(loc, "Element type for tuple must be a valid fflat type but got: ") << element;
            return false;
        }
        elements.push_back(element);
    } while(succeeded(parser.parseOptionalComma()));

    if(parser.parseRSquare()) { return false; }
    out= TupleType::get(elements);
    return true;
}
bool mlir::fflat::types::is_fflat_type(::mlir::Type ty)
{
    return 
        mlir::isa<
            mlir::IntegerType,
            mlir::Float32Type,
            mlir::Float64Type,
            mlir::IndexType,
            mlir::fflat::types::TupleType,
            mlir::fflat::types::FunctionType
        >(ty)
        ;
}
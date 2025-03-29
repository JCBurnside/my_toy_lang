#include "FflatMlir/dialect.h"
#include "FflatMlir/fflat_types.h"

void mlir::fflat::TopLevelValue::build(::mlir::OpBuilder &builder, ::mlir::OperationState &state, StringRef ident, Type ty) {
    mlir::OpBuilder::InsertionGuard g(builder);

    state.addAttribute(
        SymbolTable::getSymbolAttrName(),
        builder.getStringAttr(ident)
    );

    state.addAttribute(
        getTyAttrName(state.name),
        ::mlir::TypeAttr::get(ty)
    );
}
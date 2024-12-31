add_mlir_dialect(
    FflatOps
    fflat
)

add_mlir_doc(FflatDialect FflatDialect FflatOps/ -gen-dialect-doc)
add_mlir_doc(FflatOps FflatOps FflatOps/ -gen-op-doc)
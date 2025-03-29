add_mlir_dialect_library(
    FflatMlir
    lib.cpp
    builder.cpp
    fflat_types.cpp
    ops.cpp
    
    DEPENDS
    MLIRFflatOpsIncGen

    LINK_LIBS PUBLIC
    MLIRIR
    MLIRInferTypeOpInterface
    MLIRFuncDialect

)
add_dependencies(FflatMlir bridge_headers)
target_link_libraries(FflatMlir
PUBLIC
libMLIRAnalysis.a
libMLIRFunctionInterfaces.a
libMLIRIR.a
libMLIRParser.a
libMLIRSideEffectInterfaces.a
libMLIRTransforms.a)
install(
    TARGETS FflatMlir
)
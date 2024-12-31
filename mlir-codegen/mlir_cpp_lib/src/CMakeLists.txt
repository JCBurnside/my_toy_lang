add_mlir_dialect_library(
    FflatMlir
    lib.cpp
    builder.cpp
    types.cpp
    
    DEPENDS
    MLIRFflatOpsIncGen

    LINK_LIBS PUBLIC
    MLIRIR
    MLIRInferTypeOpInterface
    MLIRFuncDialect

)
add_dependencies(FflatMlir bridge_headers)
target_link_libraries(FflatMlir
PRIVATE
MLIRAnalysis
MLIRFunctionInterfaces
MLIRIR
MLIRParser
MLIRSideEffectInterfaces
MLIRTransforms)
install(
    TARGETS FflatMlir
)
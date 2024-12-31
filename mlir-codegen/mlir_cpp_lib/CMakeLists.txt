cmake_minimum_required(VERSION 3.11)
project(MlirCodegenBridge VERSION 0.1.0 LANGUAGES C CXX)
option(FOCUS "enable tools if this project is the focus for a better ide experience")
if(FOCUS)
    message(INFO "project is focused.")
    find_program(CXXBRIDGE cxxbridge)
    if(CXXBRIDGE)
        message(INFO "cxxbridge being used.  you should only see this if you are focusing on this project in an ide (eg vsc). if not please use cargo build for building.")
        make_directory(${CMAKE_BINARY_DIR}/include/mlir-codegen/src)
        make_directory(${CMAKE_BINARY_DIR}/include/rust)
        add_custom_target(bridge_headers ALL
            cxxbridge ${CMAKE_CURRENT_SOURCE_DIR}/../src/bridge.rs > ${CMAKE_CURRENT_BINARY_DIR}/include/mlir-codegen/src/bridge.rs.cc
            COMMAND cxxbridge --header ${CMAKE_CURRENT_SOURCE_DIR}/../src/bridge.rs > ${CMAKE_CURRENT_BINARY_DIR}/include/mlir-codegen/src/bridge.rs.h
            COMMAND cxxbridge --header > ${CMAKE_CURRENT_BINARY_DIR}/include/rust/cxx.h
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/../src/bridge.rs
        )
        include_directories(${CMAKE_CURRENT_BINARY_DIR}/include/)
    else()
        message(FATAL_ERROR "please install cxxbridge.  if you have cargo installed (if you are working on this you probably do) then it's as simple as `cargo install cxxbridge-cmd")
    endif()
else()
    message("project is not focused")
    message("${CMAKE_CXX_INCLUDE_DIRS}")
    add_custom_target(bridge_headers)#empty target so that it works no matter
endif()

find_package(LLVM CONFIG REQUIRED)
find_package(MLIR CONFIG REQUIRED)
include(AddLLVM)
list(APPEND CMAKE_MODULE_PATH "/usr/lib/llvm-18/lib/cmake/mlir")
include(TableGen)
include(AddMLIR)
include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${MLIR_INCLUDE_DIRS})
include_directories(${PROJECT_BINARY_DIR}/include)
include_directories(include)
add_subdirectory(include)
add_subdirectory(src)
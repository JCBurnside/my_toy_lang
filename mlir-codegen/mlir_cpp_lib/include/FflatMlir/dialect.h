#pragma once


#include "mlir/Bytecode/BytecodeOpInterface.h"
#include "mlir/IR/Dialect.h"
#include "mlir/IR/SymbolTable.h"
#include "mlir/Interfaces/CallInterfaces.h"
#include "mlir/Interfaces/FunctionInterfaces.h"
#include "mlir/Interfaces/SideEffectInterfaces.h"
#include "mlir/IR/Attributes.h"
#include "mlir/IR/AttributeSupport.h"
/// Include the auto-generated header file containing the declaration of the toy
/// dialect.
#include "FflatMlir/FflatOpsDialect.h.inc"

/// Include the auto-generated header file containing the declarations of the
/// toy operations.
#define GET_OP_CLASSES
#include "FflatMlir/FflatOps.h.inc"

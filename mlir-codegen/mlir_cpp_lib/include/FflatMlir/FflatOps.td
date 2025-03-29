#ifndef FFLAT_DIALECT_TD
#define FFLAT_DIALECT_TD
include "mlir/IR/OpBase.td"

include "mlir/Interfaces/SideEffectInterfaces.td"
include "mlir/IR/AttrTypeBase.td"

def FflatDialect : Dialect {
    let name = "fflat";

    let summary = "this is used for the fflat lang";

    let description =[{
        this dialect is for use with the fflat compiler.
    }];

    let cppNamespace = "mlir::fflat";

    let useDefaultTypePrinterParser = 1;
}


def AnyFloatAttr : TypedAttrBase<AnyFloat, "FloatAttr",
    CPred<"::llvm::isa<::mlir::FloatAttr>($_self)">,
    "float attribute"
>{
    let returnType = [{ ::llvm::APFloat }];
}

class FflatAttr<string name, string attrMnuemonic, list<Trait> traits=[]> 
: AttrDef<FflatDialect, name, traits> {
    let mnemonic = attrMnuemonic;
}

def ExternCAttr : FflatAttr<"ExternC","externc"> {
    let summary = "used on fuctions to indicate c linkage";
    let description = [{
        This used by function types to indicate it's an externed item being called
        declared; 
    }];
}

def FflatFunctionType : DialectType<FflatDialect, CPred<"::mlir::isa<::mlir::fflat::types::FunctionType>($_self)">, "Fflat function type.">;
def StructType : DialectType<FflatDialect, CPred<"::mlir::isa<::mlir::fflat::types::StructType>($_self)">, "Fflat structure type.">;
def TupleType : DialectType<FflatDialect, CPred<"::mlir::isa<::mlir::fflat::types::TupleType>($_self)">, "Tuple Type">;
def EnumType : DialectType<FflatDialect, CPred<"::mlir::isa<::mlir::fflat::types::EnumType>($_self)">, "Fflat enum/discriminated union type">;
def DependentType : DialectType<FflatDialect, CPred<"::mlir::isa<:mlir::fflat::types::DependentType>($_self)">, "Fflat dependent type.">;

def FflatType : AnyTypeOf<[
    I8,
    I16,
    I32,
    I64,
    Index,
    AnyFloat,
    TupleType,
    FflatFunctionType,
    StructType,
]>;

class FflatOp<string mneumonic,list<Trait> traits =[]> : Op<FflatDialect,mneumonic,traits>;

def TopLevelValue : FflatOp<"value.top"> {
    let arguments = (ins 
        TypeAttrOf<FflatType>:$ty,
        SymbolNameAttr : $ident
    );

    let builders = [
        OpBuilder<(ins "StringRef":$ident, "Type":$ty)>
    ];

    let assemblyFormat = "$ident `:` $ty attr-dict";
}



def ConstantFloatOp : FflatOp<"const.float", [Pure]> {
    let arguments = (ins AnyAttrOf<[
        F32Attr,
        F64Attr
    ]>:$value);
    let results = (outs AnyFloat);

    let builders = [
        // OpBuilder<(ins "::mlir::FloatAttr":$value), [{
        //     build($_builder, $_state, value.getType(), value);
        // }]>,
        // OpBuilder<(ins "double":$value)>,
        // OpBuilder<(ins "float":$value)>
    ];
}

def RetOp : FflatOp<"ret",[Pure]> {
    let arguments = (ins FflatType:$value);
}

def DestructureTuple : FflatOp<"dest.tuple"> {
    let arguments = (ins TupleType:$value);

    let results = (outs Variadic<FflatType>);
}


#endif 
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

// def FflatFunctionType : DialectType<FflatDialect, CPred<"$_self.isa<FunctionType>()">, "Fflat function type.">;
def FflatType : AnyTypeOf<[
    I8,
    // FflatFunctionType,
]>;

class FflatOp<string mneumonic,list<Trait> traits =[]> : Op<FflatDialect,mneumonic,traits>;
def ConstantFloatOp : FflatOp<"const.float", [Pure]> {
    let arguments = (ins AnyFloatAttr:$value);
    let results = (outs AnyFloat);

    let builders = [
        OpBuilder<(ins "::mlir::FloatAttr":$value), [{
            build($_builder, $_state, value.getType(), value);
        }]>,
        OpBuilder<(ins "double":$value)>,
        OpBuilder<(ins "float":$value)>
    ];
}

def RetOp : FflatOp<"ret",[Pure]> {
    let arguments = (ins FflatType:$value);
}

#endif 
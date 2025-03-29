use compiler::typed_ast;
macro_rules! alias {
    ($new:ident = $old:ty) => {
        #[repr(transparent)]
        struct $new($old);
        impl From<$old> for $new {
            fn from(value:$old) -> Self {
                Self(value)
            }
        }
    };
}
alias!(Expr = typed_ast::TypedExpr);
alias!(Statement = typed_ast::TypedStatement);
alias!(ModuleDecl = typed_ast::TypedModuleDeclaration);
alias!(TopLevelDeclaration = typed_ast::TypedTopLevelValue);
alias!(StructDef = typed_ast::StructDefinition);
alias!(TypeDeclaration = typed_ast::ResolvedTypeDeclaration);
alias!(EnumDef = typed_ast::TypedEnumDeclaration);
alias!(TopLevelValue = typed_ast::TypedTopLevelValue);
#[cxx::bridge]
mod ffi {
    #[namespace = "ast"]
    extern "Rust" {
        type Expr;
        type Statement;
        type ModuleDecl;
        type TopLevelDeclaration;
        type TopLevelValue;
        type TypeDeclaration;
        type StructDef;
        type EnumDef;
    }
    #[namespace = "fflat"]
    unsafe extern "C++" {
        include!("fflat/bridge.h");
        fn test();
        pub type Builder;
        pub fn make_builder() -> UniquePtr<Builder>; 
        pub fn dump(self:Pin<&mut Builder>);
        pub fn write_file(self:Pin<&mut Builder>,path:String);
        pub fn write_string(self:Pin<&mut Builder>) -> String;
    }
}

pub(crate) use ffi::*;
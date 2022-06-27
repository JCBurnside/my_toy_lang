#[derive(PartialEq, Debug)]
#[allow(unused)]
pub enum Expr {
    BinaryOpCall {
        ident: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnaryOpCall {
        ident: String,
        operand: Box<Expr>,
    },
    FnCall {
        ident: String,
        args: Vec<Expr>,
    },
    Return {
        expr: Box<Expr>,
    },
    Block {
        sub: Vec<Expr>,
    },
    Literal {
        value: String,
        ty: TypeName,
    },
    Declaration {
        is_op: bool,
        ident: String,
        ty: Option<TypeName>,
        args: Option<Vec<(String, Option<TypeName>)>>,
        value: Box<Expr>,
    },
}

#[derive(PartialEq, Debug)]
pub enum TypeName {
    FnType(Box<TypeName>, Box<TypeName>),
    ValueType(String),
}

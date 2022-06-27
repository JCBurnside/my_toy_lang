#[derive(PartialEq, Debug)]
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
        ty: Type,
    },
    Declaration {
        is_op: bool,
        ident: String,
        ty: Option<Type>,
        args: Option<Vec<(String, Option<Type>)>>,
        value: Box<Expr>,
    },
}

#[derive(PartialEq, Debug)]
pub enum Type {
    FnType(Box<Type>, Box<Type>),
    ValueType(String),
}

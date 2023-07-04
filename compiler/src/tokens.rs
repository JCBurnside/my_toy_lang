#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Let,
    Return,
    Arrow,      // ->
    GroupOpen,  // (
    GroupClose, // )
    CurlOpen,   // {
    CurlClose,  // }
    Colon,
    Ident(String),
    For, // used for generic declaration and maybe composition in a later version
    Type,
    Enum,
    // Fn, //to be added latter to distinguish between value and function with 0 args

    //literals
    Integer(bool, String),
    FloatingPoint(bool, String),
    StringLiteral(String),
    CharLiteral(String),
    #[allow(unused)] //todo implement.
    Compose,
    Op(String),
    //meta tokens
    NewLine, //needed due to how fn args are parsed.  largely ignored
    BeginBlock,
    EndBlock,
    EoF,

    Error(&'static str /*reason*/),
}

impl Token {
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::EoF)
    }
}

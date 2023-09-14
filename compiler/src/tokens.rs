#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Let,
    Return,
    Arrow,      // ->
    GroupOpen,  // (
    GroupClose, // )
    CurlOpen,   // {
    CurlClose,  // }
    Comma,      //,
    Colon,
    Ident(String),
    For, // used for generic declaration and maybe composition in a later version
    Type,
    Enum,
    If,
    Then,
    Else,

    Match,

    /// will be used for both pattern match with (Match)[Token::Match] and used in restrictions
    Where,

    // Fn, //to be added latter to distinguish between value and function with 0 args.  reality will be syntax sugar for functions with auto unit args

    //literals
    Integer(bool, String),
    FloatingPoint(bool, String),
    StringLiteral(String),
    CharLiteral(String),
    #[allow(unused)] //todo implement.
    Compose,
    // |, >, <, !, @,  $, =, &, +, -, \, /, *, ^, .
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

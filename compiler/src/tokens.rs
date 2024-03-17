#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Let,
    Return,
    Arrow,        // ->
    GroupOpen,    // (
    GroupClose,   // )
    CurlOpen,     // {
    CurlClose,    // }
    BracketOpen,  // [
    BracketClose, //]
    Comma,        //,
    Colon,
    Ident(String),
    For, // used for generic declaration and maybe composition in a later version
    Type,
    Enum,
    If,
    Then,
    Else,
    True,
    False,

    Match,

    /// will be used for both pattern match with (Match)[Token::Match] and used in restrictions
    Where,

    // Fn, //to be added latter to distinguish between value and function with 0 args.  reality will be syntax sugar for functions with auto unit args

    //literals
    Integer(bool, String),
    FloatingPoint(bool, String),
    StringLiteral(String),
    CharLiteral(String),
    ArrayOpen,
    ArrayClose,

    #[allow(unused)] //todo implement.
    Compose,
    // |, >, <, !, @,  $, =, &, +, -, \, /, *, ^, .
    Op(String),
    //meta tokens
    //TODO! attribute indicatator
    Extern,
    Seq, // ;
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

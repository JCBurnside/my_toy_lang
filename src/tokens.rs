#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Let,
    Return,
    Arrow,      // ->
    GroupOpen,  // (
    GroupClose, // )
    Colon,
    Ident(String),
    // Fn, //to be added latter to distinquish between value and function with 0 args

    //literals
    Integer(bool, String),
    FloatingPoint(bool, String),
    StringLiteral(String),
    CharLiteral(String),

    Op(String),
    //meta tokens
    NewLine,    //needed due to how fn args are parsed.  largerly ignored
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

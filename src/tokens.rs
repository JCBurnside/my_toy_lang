#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Let,
    Return,
    Ident(String),
    Equals,
    // Fn, //to be added latter to distinquish between value and function with 0 args

    //literals
    Integer(String),
    FloatingPoint(String),
    StringLiteral(String),
    CharLiteral(String),

    Op(String),
    //meta tokens
    Arrow,      // ->
    GroupOpen,  // (
    GroupClose, // )
    Colon,
    BeginBlock,
    EndBlock,
    EoF,
    IndentError(usize),
}

impl Token {
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::EoF)
    }
}

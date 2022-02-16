use std::str::FromStr;

pub enum Token {
    Let,
    Ident(String),
    Equals,
    // Fn, //to be added latter to distinquish between value and function with 0 args

    //literals 
    Ingeger(i128),
    FloatingPoint(f64),
    StirngLiteral(String),
    CharLiteral(char),

    Op(String),
    //meta tokens
    BeginBlock,
    EndBlock,
    EoF
}

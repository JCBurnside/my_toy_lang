use std::borrow::Cow;
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token<'a> {
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
    Integer(String),
    FloatingPoint(String),
    StringLiteral(String),
    CharLiteral(String),

    #[allow(unused)] //todo implement.
    Compose,
    // |, >, <, !, @,  $, =, &, +, -, \, /, *, ^, .
    Op(&'a str),
    //meta tokens
    //TODO! attribute indicatator
    Extern,
    Seq, // ;
    BeginBlock,
    EndBlock,
    EoF,
    Scope,

    Error(&'static str /*reason*/),
}

impl Token<'_> {
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::EoF)
    }
}

impl winnow::stream::ContainsToken<Token<'_>> for Token<'_> {
    #[inline(always)]
    fn contains_token(&self, token: Token) -> bool {
        self == &token
    }
}

impl winnow::stream::ContainsToken<Token<'_>> for (Token<'_>,std::ops::Range<usize>) {
    #[inline(always)]
    fn contains_token(&self, token: Token<'_>) -> bool {
        self.0 == token
    }
}

impl winnow::stream::ContainsToken<Token<'_>> for &'_ [Token<'_>] {
    #[inline]
    fn contains_token(&self, token: Token<'_>) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token<'_>> for &'_ [Token<'_>; LEN] {
    #[inline]
    fn contains_token(&self, token: Token<'_>) -> bool {
        self.iter().any(|t| t == &token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token<'_>> for [Token<'_>; LEN] {
    #[inline]
    fn contains_token(&self, token: Token<'_>) -> bool {
        self.iter().any(|t| t == &token)
    }
}
impl winnow::stream::ContainsToken<Token<'_>> for &'_ [(Token<'_>,std::ops::Range<usize>)] {
    #[inline]
    fn contains_token(&self, token: Token<'_>) -> bool {
        self.iter().any(|t| t.0 == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token<'_>> for &'_ [(Token<'_>,std::ops::Range<usize>); LEN] {
    #[inline]
    fn contains_token(&self, token: Token<'_>) -> bool {
        self.iter().any(|t| t.0 == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token<'_>> for [(Token<'_>,std::ops::Range<usize>); LEN] {
    #[inline]
    fn contains_token(&self, token: Token<'_>) -> bool {
        self.iter().any(|t| t.0 == token)
    }
}

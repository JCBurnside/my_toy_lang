use std::{iter::Peekable, str::Chars, ops::DerefMut};

pub struct LexErr {
    pub error : &'static str,
    pub location : usize
}
impl LexErr{
    fn new(error:&'static str) -> Self {
        Self { error, location : 0 }
    }
    fn with_pos(error:&'static str, location : usize) -> Self {
        Self { error, location }
    }
}

struct Lexer<'str> {
    source : &'str str,
    chars : Box<Peekable<Chars<'str>>>,
    pos : usize,
    indent_levels : Vec<u32>,
    should_begin : bool

}
use crate::tokens::Token;
type LexResult = std::result::Result<Token,LexErr>;
impl <'str> Lexer<'str> {
    pub fn new(source : &'str str) -> Self {
        Self { source, chars : Box::new(source.chars().peekable()), pos : 0, indent_levels:vec![],should_begin:false }
    }

    pub fn lex(&mut self) -> LexResult {
        let chars = self.chars.deref_mut();
        let src = self.source;
        let mut pos = self.pos;
        let last_block = self.indent_levels.last().copied().unwrap_or_default();
        let mut whitespace_count = 0u32;
        loop {
            
        }
    }
}
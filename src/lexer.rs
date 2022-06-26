use std::{str::Chars, iter::Peekable};
struct Lexer<I> {
    source_stream :I,
    curr_pos : usize,
    whitespaces : Vec<usize>,
    end_blocks_to_gen : usize,
    start_line : bool,
}

macro_rules! operators {
    () => {
        '|' | '>' | '<' | '!' | '@' |  '$' | '=' | '&' | '+' | '-' | '\\' | '/' | '*' 
    };
}

use itertools::Itertools;

use crate::tokens::Token;
impl <I:Iterator<Item=char> + Clone> Lexer<Peekable<I>> {
    #[allow(dead_code)]
    pub fn new<II:IntoIterator<IntoIter = I>>(source: II) -> Self {
        Self {
            source_stream: source.into_iter().peekable(),
            curr_pos : 0,
            whitespaces : vec![],
            end_blocks_to_gen : 0,
            start_line : false,
        }
    }

    pub fn lex(&mut self) -> (Token, usize) {
        if self.end_blocks_to_gen > 0 {
            self.end_blocks_to_gen -= 1;
            return (Token::EndBlock,self.curr_pos);
        }
        if self.source_stream.peek() == None {
            return (Token::EoF,self.curr_pos);
        }

        if self.source_stream.peek() == Some(&'\n') {
            self.start_line = true;
            self.source_stream.advance_by(1).unwrap();
            return self.lex();
        }

        let (ws,count) = self.source_stream
            .peeking_take_while(|c| c != &'\n' && c.is_ascii_whitespace())
            .map(|c| match c {
                ' ' => 1,
                '\t' => 4,
                _ => 0,
            })
            .fold((0usize,0usize),|(sum,count),curr| (sum + curr,count + 1));
        self.curr_pos += count;
        if self.start_line {
            self.start_line = false;
            let ends = self.whitespaces.drain_filter(|x| *x<ws).count();
            return if ends == 0 { 
                self.whitespaces.push(ws);
                (Token::BeginBlock,self.curr_pos) 
            } else {
                self.end_blocks_to_gen = ends; 
                self.lex() 
            }
        }
        if let Some(c) = self.source_stream.next() {
            if c == '\n' {
                self.start_line = true;
                return self.lex()
            }
            
            
            self.curr_pos+=1;
            match c {
                '(' => (Token::GroupOpen,self.curr_pos),
                ')' => (Token::GroupClose,self.curr_pos),
                '\'' => {
                    let mut prev = '\0';
                    let inside = self.source_stream.peeking_take_while(|c| 
                        if prev == '\\' {
                            prev = *c;
                            true
                        } else {
                            prev = *c;
                            c != &'\n' && c != &'\''
                        }
                    ).collect::<String>();
                    self.curr_pos += inside.len();
                    if prev == '\n' {
                        (Token::Error("unclosed litteral"),self.curr_pos)
                    } else {
                        self.source_stream.advance_by(1).unwrap();
                        (Token::CharLiteral(inside),self.curr_pos)
                    }
                }
                '"' => {
                    let mut prev = '\0';
                    let inside = self.source_stream.peeking_take_while(|c| 
                        if prev == '\\' {
                            prev = *c;
                            true
                        } else {
                            prev = *c;
                            c != &'\n' && c != &'"'
                        }
                    ).collect::<String>();
                    self.curr_pos += inside.len();
                    if prev == '\n' {
                        (Token::Error("unclosed litteral"),self.curr_pos)
                    } else {
                        self.source_stream.advance_by(1).unwrap();
                        (Token::StringLiteral(inside),self.curr_pos)
                    }
                }

                '-' if self.source_stream.peek() == Some(&'>') && self.source_stream.clone().nth(2).map_or(true,|c| !c.is_whitespace()) => {
                    self.curr_pos += 1;
                    self.source_stream.next();
                    (Token::Arrow,self.curr_pos)
                }

                '-' | '+' if self.source_stream.clone().next().map_or(false, |c| c.is_numeric()||c=='.') => {
                    let inside : String = self.source_stream.peeking_take_while(|c| !c.is_whitespace()).collect();
                    self.curr_pos+=inside.len();
                    if inside.contains('.') {
                        (Token::FloatingPoint(c == '-',inside),self.curr_pos)
                    } else {
                        (Token::Integer(c=='-', inside), self.curr_pos)
                    }
                }

                operators!() => {
                    let inside : String = self.source_stream.clone().take_while(|c| matches!(c,operators!())).collect();
                    self.source_stream.nth(inside.len());
                    self.curr_pos += inside.len();
                    let inside = c.to_string() + &inside;
                    (Token::Op(inside),self.curr_pos)
                }

                'l' if self.source_stream.clone().take_while(|c| !c.is_whitespace()).collect::<String>() == "et" => {
                    self.source_stream.advance_by(2).unwrap();
                    self.curr_pos += 2;
                    (Token::Let,self.curr_pos)
                }
                'r' if self.source_stream.clone().take_while(|c| !c.is_whitespace()).collect::<String>() == "eturn" => {
                    self.source_stream.advance_by(5).unwrap();
                    self.curr_pos += 5;
                    (Token::Return,self.curr_pos)
                }
                ':' => (Token::Colon,self.curr_pos),
                c=> {
                    let inner : String = self.source_stream.peeking_take_while(|c| !c.is_whitespace()).collect();
                    self.curr_pos+= inner.len();
                    let inner = c.to_string() + &inner;
                    if inner.chars().all(|c| c.is_numeric()) {
                        (Token::Integer(false, inner),self.curr_pos)
                    } else if inner.chars().all(|c| c.is_numeric() || c == '.') {
                        (Token::FloatingPoint(false, inner),self.curr_pos,)
                    } 
                    else {
                        (Token::Ident(inner),self.curr_pos)
                    }
                }
            }
        } else {
            (Token::Error("Unkown Error"),self.curr_pos)
        }

    }
}

pub struct TokenStream<I> {
    lexer: Lexer<I>,
    ended: bool,
}

impl <'src> TokenStream<Peekable<Chars<'src>>> {
    #[allow(dead_code)]
    pub fn from_source(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src.chars()),
            ended: false,
        }
    }
}
impl <I:Iterator<Item=char> + Clone> TokenStream<Peekable<I>> {
    #[allow(dead_code)]
    pub fn from_iter<II:IntoIterator<IntoIter = I>>(src:II) -> Self {
        Self { lexer: Lexer::new(src.into_iter()), ended: false }
    }

}

impl <T:Iterator<Item=char> + Clone> Iterator for TokenStream<Peekable<T>> {
    type Item = (Token, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            None
        } else {
            let (token, span) = self.lexer.lex();
            if token.is_eof() {
                self.ended = true;
            }
            Some((token, span))
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::tokens::Token;

    use super::TokenStream;
    use itertools::Itertools;
    #[test]
    fn single_tokens() {
        assert_eq!(
            TokenStream::from_source("let")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::Let, Token::EoF],
            "let token"
        );
        assert_eq!(
            TokenStream::from_source(":").map(|(a, _)| a).collect_vec(),
            [Token::Colon, Token::EoF],
            "colon"
        );
        assert_eq!(
            TokenStream::from_source("1").map(|(a, _)| a).collect_vec(),
            vec![Token::Integer(false,"1".to_owned()), Token::EoF],
            "int token"
        );
        assert_eq!(
            TokenStream::from_source("1.0")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::FloatingPoint(false,"1.0".to_owned()), Token::EoF],
            "float token"
        );
        assert_eq!(
            TokenStream::from_source("'l'")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::CharLiteral("l".to_owned()), Token::EoF],
            "char token"
        );
        assert_eq!(
            TokenStream::from_source("\"let\"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::StringLiteral("let".to_owned()), Token::EoF],
            "string token"
        );
        assert_eq!(
            TokenStream::from_source("foo")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::Ident("foo".to_owned()), Token::EoF],
            "ident token"
        );
        assert_eq!(
            TokenStream::from_source("!=").map(|(a, _)| a).collect_vec(),
            vec![Token::Op("!=".to_owned()), Token::EoF],
            "op token"
        );

        assert_eq!(
            TokenStream::from_source("return")
                .map(|(a, _)| a)
                .collect_vec(),
            [Token::Return, Token::EoF],
            "return token"
        );

        assert_eq!(
            TokenStream::from_source("(")
                .map(|(a,_)| a)
                .collect_vec(),
            [Token::GroupOpen,Token::EoF],
            "open group token"
        );

        assert_eq!(
            TokenStream::from_source(")")
                .map(|(a,_)|a)
                .collect_vec(),
            [Token::GroupClose, Token::EoF],
            "close group token"
        );

        assert_eq!(
            TokenStream::from_source("->")
                .map(|(a,_)| a)
                .collect_vec(),
            [Token::Arrow,Token::EoF],
            "Arrow token"
        );

        assert_eq!(
            TokenStream::from_source("\n\t")
                .map(|(a,_)|a)
                .collect_vec(),
            [Token::BeginBlock,Token::EoF],
            "Begin block"
        )
    }

    #[test]
    fn singled_out() {
        assert_eq!(
            TokenStream::from_source("\n\tlet")
                .map(|(a,_)|dbg!(a))
                .collect_vec(),
            [Token::BeginBlock,Token::Let,Token::EoF],
            "Begin block"
        )
    }

    #[test]
    fn literals_edge_cases() {
        assert_eq!(
            TokenStream::from_source("\"foo bar \\\"baz\"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![
                Token::StringLiteral("foo bar \\\"baz".to_owned()),
                Token::EoF
            ]
        );
        assert_eq!(
            TokenStream::from_source("\"\"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::StringLiteral("".to_owned()), Token::EoF]
        );
        assert_eq!(
            TokenStream::from_source("\" \"")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::StringLiteral(" ".to_owned()), Token::EoF]
        );

        assert_eq!(
            TokenStream::from_source("' '")
                .map(|(a, _)| a)
                .collect_vec(),
            vec![Token::CharLiteral(" ".to_owned()), Token::EoF]
        );
    }

    #[test]
    fn token_chain() {
        assert_eq!(
            TokenStream::from_source(
                r#"let foo =
    return 1.0

let bar : int32 = 1

let baz = "foo bar baz"

let group_test arg : ( int32 -> int32 ) -> int32
"#
            )
            .map(|(a, _)| dbg!(a))
            .collect_vec(),
            #[rustfmt::skip]
            [
                Token::Let,Token::Ident("foo".to_owned()),Token::Op("=".to_owned()),
                Token::BeginBlock,
                    Token::Return, Token::FloatingPoint(false,"1.0".to_owned()),
                Token::EndBlock,
                Token::Let, Token::Ident("bar".to_owned()), Token::Colon, Token::Ident("int32".to_owned()), Token::Op("=".to_owned()), Token::Integer(false,"1".to_owned()),
                Token::Let, Token::Ident("baz".to_owned()), Token::Op("=".to_owned()),  Token::StringLiteral("\"foo bar baz\"".to_owned()),
                Token::Let, Token::Ident("group_test".to_owned()), Token::Ident("arg".to_owned()), Token::Colon, Token::GroupOpen, Token::Ident("int32".to_owned()), Token::Arrow, Token::Ident("int32".to_owned()), Token::GroupClose, Token::Arrow, Token::Ident("int32".to_owned()),
                Token::EoF,
            ]
        )
    }
}

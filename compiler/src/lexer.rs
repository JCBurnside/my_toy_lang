use std::{iter::Peekable, str::Chars};
#[derive(Clone)]
struct Lexer<I: Clone> {
    source_stream: I,
    curr_col: usize,
    curr_line: usize,
    whitespaces: Vec<usize>,
    end_blocks_to_gen: usize,
    start_line: bool,
}

macro_rules! operators {
    () => {
        '|' | '>' | '<' | '!' | '@' |  '$' | '=' | '&' | '+' | '-' | '\\' | '/' | '*' | '^' | '.'
    };
}

use itertools::Itertools;

use crate::tokens::Token;
impl<I: Iterator<Item = char> + Clone> Lexer<Peekable<I>> {
    pub fn new<II: IntoIterator<IntoIter = I>>(source: II) -> Self {
        Self {
            source_stream: source.into_iter().peekable(),
            curr_col: 0,
            curr_line: 0,
            whitespaces: vec![],
            end_blocks_to_gen: 0,
            start_line: false,
        }
    }

    pub fn lex(&mut self) -> (Token, crate::Location) {
        if self.end_blocks_to_gen > 0 {
            self.end_blocks_to_gen -= 1;
            return (Token::EndBlock, (self.curr_line, self.curr_col));
        }
        if self.source_stream.peek() == None {
            return (Token::EoF, (self.curr_line, self.curr_col));
        }

        if self.source_stream.peek() == Some(&'\n') {
            self.start_line = true;
            self.source_stream.next().unwrap();
            self.curr_col = 0;
            self.curr_line += 1;
            return self.lex();
        }

        let (ws, count) = self
            .source_stream
            .peeking_take_while(|c| c != &'\n' && c.is_ascii_whitespace())
            .map(|c| match c {
                ' ' => 1,
                '\t' => 4,
                _ => 0,
            })
            .fold((0usize, 0usize), |(sum, count), curr| {
                (sum + curr, count + 1)
            });
        self.curr_col += count;
        if self.start_line {
            self.start_line = false;
            if self.whitespaces.last().unwrap_or(&0) == &ws {
                return self.lex();
            }
            let ends = self.whitespaces.iter().filter(|x| *x > &ws).count();
            self.whitespaces.retain(|x| *x <= ws);
            return if ends == 0 && ws > 0 {
                self.whitespaces.push(ws);

                (Token::BeginBlock, (self.curr_line, self.curr_col))
            } else {
                self.end_blocks_to_gen = ends;
                self.lex()
            };
        }
        if let Some(c) = self.source_stream.next() {
            let start_col = self.curr_col;
            self.curr_col += 1;
            if c == '\n' {
                self.start_line = true;
                self.curr_col = 0;
                self.curr_line += 1;
                return self.lex();
            }

            match c {
                ';' => (Token::Seq, (self.curr_line, start_col)),
                ',' => (Token::Comma, (self.curr_line, start_col)),
                '(' => (Token::GroupOpen, (self.curr_line, start_col)),
                ')' => (Token::GroupClose, (self.curr_line, start_col)),
                '{' => (Token::CurlOpen, (self.curr_line, start_col)),
                '}' => (Token::CurlClose, (self.curr_line, start_col)),
                '\'' => {
                    let mut prev = '\0';
                    let inside = self
                        .source_stream
                        .peeking_take_while(|c| {
                            if prev == '\\' {
                                prev = *c;
                                true
                            } else {
                                prev = *c;
                                c != &'\n' && c != &'\''
                            }
                        })
                        .collect::<String>();
                    self.curr_col += inside.len();
                    if prev == '\n' {
                        (
                            Token::Error("unclosed literal"),
                            (self.curr_line, self.curr_col),
                        )
                    } else {
                        self.source_stream.next();
                        self.curr_col += 1;
                        (Token::CharLiteral(inside), (self.curr_line, start_col))
                    }
                }
                '"' => {
                    let mut prev = '\0';
                    let inside = self
                        .source_stream
                        .peeking_take_while(|c| {
                            if prev == '\\' {
                                prev = *c;
                                true
                            } else {
                                prev = *c;
                                c != &'\n' && c != &'"'
                            }
                        })
                        .collect::<String>();
                    self.curr_col += inside.len();
                    if prev == '\n' {
                        (
                            Token::Error("unclosed litteral"),
                            (self.curr_line, start_col),
                        )
                    } else {
                        self.source_stream.next();
                        self.curr_col += 1;
                        (Token::StringLiteral(inside), (self.curr_line, start_col))
                    }
                }

                '-' if self.source_stream.peek() == Some(&'>')
                    && self
                        .source_stream
                        .clone()
                        .nth(2)
                        .map_or(true, |c| !matches!(c, operators!())) =>
                {
                    self.curr_col += 1;
                    self.source_stream.next();
                    (Token::Arrow, (self.curr_line, start_col))
                }

                '-' | '+'
                    if self
                        .source_stream
                        .clone()
                        .next()
                        .map_or(false, |c| c.is_numeric() || c == '.') =>
                {
                    let inside: String = self
                        .source_stream
                        .peeking_take_while(|c| !c.is_whitespace())
                        .collect();
                    self.curr_col += inside.len();
                    if inside.contains('.') {
                        (
                            Token::FloatingPoint(c == '-', inside),
                            (self.curr_line, start),
                        )
                    } else {
                        (Token::Integer(c == '-', inside), (self.curr_line, start_col))
                    }
                }
                c if c.is_numeric() && self.source_stream.peek() == Some(&'.') => {
                    let inner: String = self
                        .source_stream
                        .clone()
                        .take_while(|c| (c.is_numeric()) || c == &'_' || c == &'.')
                        .collect();
                    for _ in 0..inner.len() {
                        self.source_stream.next();
                    }
                    self.curr_col += inner.len();
                    let inner = c.to_string() + &inner;
                    (Token::FloatingPoint(false, inner), (self.curr_line, start_col))
                }
                operators!() => {
                    let inside: String = self
                        .source_stream
                        .peeking_take_while(|c| matches!(c, operators!()))
                        .collect();
                    self.curr_col += inside.len();
                    let inside = c.to_string() + &inside;
                    (Token::Op(inside), (self.curr_line, start_col))
                }
                'f' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "or" =>
                {
                    // self.source_stream.advance_by(2).unwrap();
                    self.source_stream.next();
                    self.source_stream.next();
                    self.curr_col += 2;
                    (Token::For, (self.curr_line, start_col))
                }

                't' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "rue" =>
                {
                    for _ in 0..3 {
                        let _ = self.source_stream.next();
                    }
                    self.curr_col += 3;
                    (Token::True, (self.curr_line, start_col))
                }
                'f' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "alse" =>
                {
                    for _ in 0..4 {
                        let _ = self.source_stream.next();
                    }
                    let curr_col = self.curr_col;
                    self.curr_col += 4;
                    (Token::False, (self.curr_line, start_col))
                }
                'm' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "atch" =>
                {
                    for _ in 0..4 {
                        let _ = self.source_stream.next();
                    }
                    self.curr_col += 4;
                    (Token::Match, (self.curr_line, start_col))
                }

                'w' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "here" =>
                {
                    for _ in 0..4 {
                        let _ = self.source_stream.next();
                    }
                    self.curr_col += 4;
                    (Token::Where, (self.curr_line, start_col))
                }

                'i' if self.source_stream.peek() == Some(&'f') => {
                    let _ = self.source_stream.next();
                    self.curr_col += 1;
                    (Token::If, (self.curr_line, start_col))
                }

                't' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "hen" =>
                {
                    let _ = self.source_stream.next();
                    let _ = self.source_stream.next();
                    let _ = self.source_stream.next();
                    self.curr_col += 3;
                    (Token::Then, (self.curr_line, start_col))
                }

                'e' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "lse" =>
                {
                    let _ = self.source_stream.next();
                    let _ = self.source_stream.next();
                    let _ = self.source_stream.next();
                    self.curr_col += 3;
                    (Token::Else, (self.curr_line, start_col))
                }

                'l' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "et" =>
                {
                    self.source_stream.next();
                    self.source_stream.next();
                    self.curr_col += 2;
                    (Token::Let, (self.curr_line, start_col))
                }
                'r' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "eturn" =>
                {
                    // self.source_stream.advance_by(5).unwrap();
                    for _ in 0..5 {
                        self.source_stream.next();
                    }
                    self.curr_col += 5;
                    (Token::Return, (self.curr_line, start_col))
                }
                't' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "ype" =>
                {
                    for _ in 0..3 {
                        self.source_stream.next();
                    }
                    self.curr_col += 3;
                    (Token::Type, (self.curr_line, start_col))
                }
                'e' if self
                    .source_stream
                    .clone()
                    .take_while(ident_char)
                    .collect::<String>()
                    == "num" =>
                {
                    for _ in 0..3 {
                        self.source_stream.next();
                    }
                    self.curr_col += 3;
                    (Token::Enum, (self.curr_line, start_col))
                }
                ':' => (Token::Colon, (self.curr_line, start_col)),
                c => {
                    let inner: String = self
                        .source_stream
                        .clone()
                        .take_while(|c| (c.is_alphanumeric()) || c == &'_')
                        .collect();
                    // self.source_stream.advance_by(,inner.len()).unwrap();
                    for _ in 0..inner.len() {
                        self.source_stream.next();
                    }
                    self.curr_col += inner.len();
                    let inner = c.to_string() + &inner;
                    if inner.chars().all(|c| c.is_numeric()) {
                        (Token::Integer(false, inner), (self.curr_line, start_col))
                    } else if inner.chars().all(|c| c.is_numeric() || c == '.') {
                        (Token::FloatingPoint(false, inner), (self.curr_line, start_col))
                    } else {
                        (Token::Ident(inner), (self.curr_line, start_col))
                    }
                }
            }
        } else {
            (
                Token::Error("Unknown Lexer Error"),
                (self.curr_line, start_col),
            )
        }
    }
}

fn ident_char(c: &char) -> bool {
    c.is_alphanumeric() || c == &'_'
}

#[derive(Clone)]a
pub struct TokenStream<I: Clone> {
    lexer: Lexer<I>,
    ended: bool,
}

impl<'src> TokenStream<Peekable<Chars<'src>>> {
    #[allow(dead_code)]
    pub fn from_source(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src.chars()),
            ended: false,
        }
    }
}
impl<I: Iterator<Item = char> + Clone> TokenStream<Peekable<I>> {
    #[allow(dead_code)]
    pub fn from_iter<II: IntoIterator<IntoIter = I>>(src: II) -> Self {
        Self {
            lexer: Lexer::new(src.into_iter()),
            ended: false,
        }
    }
}

impl<T: Iterator<Item = char> + Clone> Iterator for TokenStream<Peekable<T>> {
    type Item = (Token, crate::Location);

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
    #[ignore = "for debugging only"]
    fn debugging() {
        let tokens = TokenStream::from_source(
            "
let match_expr_with_block x : int32 -> int32 = match x where
| 1 ->
    let a : int32 = 2;
    a*3
| 2 ->
    2
| a -> a/2,
",
        )
        .map(fst)
        .collect_vec();
        println!("{:?}", tokens);
    }

    #[test]
    #[rustfmt::skip]
    fn explicit_generics() {
        use Token::*;
        assert_eq!(
            TokenStream::from_source(
                r#"for <T,U> let foo bar baz : T -> U -> int8 =
    bar + baz
"#
            )
            .map(fst)
            .collect_vec(),
            [
                For, Op("<".to_owned()), Ident("T".to_owned()), Comma, Ident("U".to_owned()), Op(">".to_owned()), Let, Ident("foo".to_owned()), Ident("bar".to_owned()), Ident("baz".to_owned()), Colon, Ident("T".to_owned()), Arrow, Ident("U".to_owned()), Arrow, Ident("int8".to_owned()), Op("=".to_owned()), 
                BeginBlock,
                    Ident("bar".to_owned()), Op("+".to_owned()), Ident("baz".to_owned()), 
                EoF
            ]
        )
    }

    #[test]
    #[rustfmt::skip]
    fn complex_type() {
        use Token::*;
        assert_eq!(
            TokenStream::from_source("Foo<Bar<int32->(),int32>>")
                .map(fst)
                .collect_vec(),
            [
                Ident("Foo".to_string()),
                Op("<".to_string()),
                    Ident("Bar".to_string()),
                    Op("<".to_string()),
                        Ident("int32".to_string()),
                        Arrow,
                        GroupOpen,
                        GroupClose,
                        Comma,
                        Ident("int32".to_string()),
                Op(">>".to_string()),
                EoF
            ]
        );
    }

    fn fst<T, U>((it, _): (T, U)) -> T {
        it
    }
    #[test]
    fn single_tokens() {
        assert_eq!(
            TokenStream::from_source(";").map(fst).collect_vec(),
            [Token::Seq, Token::EoF],
            "seq"
        );
        assert_eq!(
            TokenStream::from_source("let").map(fst).collect_vec(),
            [Token::Let, Token::EoF],
            "let token"
        );
        assert_eq!(
            TokenStream::from_source(":").map(fst).collect_vec(),
            [Token::Colon, Token::EoF],
            "colon"
        );
        assert_eq!(
            TokenStream::from_source("1").map(fst).collect_vec(),
            [Token::Integer(false, "1".to_owned()), Token::EoF],
            "int token"
        );
        assert_eq!(
            TokenStream::from_source("1.0").map(fst).collect_vec(),
            [Token::FloatingPoint(false, "1.0".to_owned()), Token::EoF],
            "float token"
        );
        assert_eq!(
            TokenStream::from_source("'l'").map(fst).collect_vec(),
            [Token::CharLiteral("l".to_owned()), Token::EoF],
            "char token"
        );
        assert_eq!(
            TokenStream::from_source("\"let\"").map(fst).collect_vec(),
            [Token::StringLiteral("let".to_owned()), Token::EoF],
            "string token"
        );
        assert_eq!(
            TokenStream::from_source("foo").map(fst).collect_vec(),
            vec![Token::Ident("foo".to_owned()), Token::EoF],
            "ident token"
        );
        assert_eq!(
            TokenStream::from_source("!=").map(fst).collect_vec(),
            vec![Token::Op("!=".to_owned()), Token::EoF],
            "op token"
        );

        assert_eq!(
            TokenStream::from_source("return").map(fst).collect_vec(),
            [Token::Return, Token::EoF],
            "return token"
        );

        assert_eq!(
            TokenStream::from_source("(").map(fst).collect_vec(),
            [Token::GroupOpen, Token::EoF],
            "open group token"
        );

        assert_eq!(
            TokenStream::from_source(")").map(fst).collect_vec(),
            [Token::GroupClose, Token::EoF],
            "close group token"
        );

        assert_eq!(
            TokenStream::from_source("->").map(fst).collect_vec(),
            [Token::Arrow, Token::EoF],
            "Arrow token"
        );

        assert_eq!(
            TokenStream::from_source("\n\t").map(fst).collect_vec(),
            [Token::BeginBlock, Token::EoF],
            "Begin block"
        );

        assert_eq!(
            TokenStream::from_source("type").map(fst).collect_vec(),
            [Token::Type, Token::EoF],
            "Type"
        );
        assert_eq!(
            TokenStream::from_source("enum").map(fst).collect_vec(),
            [Token::Enum, Token::EoF],
            "enum"
        );
        assert_eq!(
            TokenStream::from_source("{").map(fst).collect_vec(),
            [Token::CurlOpen, Token::EoF],
            "Open Curl"
        );
        assert_eq!(
            TokenStream::from_source("}").map(fst).collect_vec(),
            [Token::CurlClose, Token::EoF],
            "Close Curl"
        );
        assert_eq!(
            TokenStream::from_source(",").map(fst).collect_vec(),
            [Token::Comma, Token::EoF],
            "comma"
        );

        assert_eq!(
            TokenStream::from_source("match").map(fst).collect_vec(),
            [Token::Match, Token::EoF],
            "match"
        );
        assert_eq!(
            TokenStream::from_source("if").map(fst).collect_vec(),
            [Token::If, Token::EoF],
            "if"
        );
        assert_eq!(
            TokenStream::from_source("then").map(fst).collect_vec(),
            [Token::Then, Token::EoF],
            "then"
        );
        assert_eq!(
            TokenStream::from_source("else").map(fst).collect_vec(),
            [Token::Else, Token::EoF],
            "else"
        );

        assert_eq!(
            TokenStream::from_source("true")
                .map(|(a, _)| a)
                .collect_vec(),
            [Token::True, Token::EoF],
            "true"
        );
        assert_eq!(
            TokenStream::from_source("false")
                .map(|(a, _)| a)
                .collect_vec(),
            [Token::False, Token::EoF],
            "false"
        )
    }

    #[test]
    fn literals_edge_cases() {
        assert_eq!(
            TokenStream::from_source("\"foo bar \\\"baz\"")
                .map(fst)
                .collect_vec(),
            [
                Token::StringLiteral("foo bar \\\"baz".to_owned()),
                Token::EoF
            ]
        );
        assert_eq!(
            TokenStream::from_source("\"\"").map(fst).collect_vec(),
            [Token::StringLiteral("".to_owned()), Token::EoF]
        );
        assert_eq!(
            TokenStream::from_source("\" \"").map(fst).collect_vec(),
            [Token::StringLiteral(" ".to_owned()), Token::EoF]
        );

        assert_eq!(
            TokenStream::from_source("' '").map(fst).collect_vec(),
            [Token::CharLiteral(" ".to_owned()), Token::EoF]
        );
    }

    #[test]
    #[rustfmt::skip]
    fn generic_test() {
        use Token::*;
        assert_eq!(
            TokenStream::from_source(
r#"let foo : int32 =
    return 5;
"#,
            )
            .map(fst)
            .collect_vec(),
            [
                Let, Ident("foo".to_string()), Colon, Ident("int32".to_string()), Op("=".to_string()),
                BeginBlock,
                    Return, Integer(false, "5".to_string()),Seq,
                EoF
            ]
        )
    }

    #[test]
    #[rustfmt::skip]
    fn token_chain() {
        use Token::*;
        assert_eq!(
            TokenStream::from_source(
                r#"let depth1 =
    let depth2 =
        'c'
    return 1.0;

let bar : int32 = 1

let baz = "foo bar baz"

let group_test arg : ( int32 -> int32 ) -> int32
"#
            )
            .map(fst)
            .collect_vec(),
            [
                Let, Ident("depth1".to_owned()), Op("=".to_owned()), 
                BeginBlock,
                    Let, Ident("depth2".to_owned()), Op("=".to_owned()), 
                    BeginBlock,
                        CharLiteral("c".to_owned()), 
                    EndBlock,
                    Return, FloatingPoint(false,"1.0".to_owned()), Seq, 
                    
                EndBlock,
                Let, Ident("bar".to_owned()), Colon, Ident("int32".to_owned()), Op("=".to_owned()), Integer(false,"1".to_owned()),
                
                Let, Ident("baz".to_owned()), Op("=".to_owned()),  StringLiteral("foo bar baz".to_owned()),  
                
                Let, Ident("group_test".to_owned()), Ident("arg".to_owned()), Colon, GroupOpen, Ident("int32".to_owned()), Arrow, Ident("int32".to_owned()), GroupClose, Arrow, Ident("int32".to_owned()), 
                EoF,
            ]
        )
    }

    #[test]
    #[rustfmt::skip]
    fn from_file() {
        const SRC: &'static str = include_str!("../../samples/test.fb");
        use Token::*;
        assert_eq!(
            TokenStream::from_source(SRC).map(fst).collect_vec(),
            [
                Let, Ident("foo".to_owned()), Colon, Ident("int32".to_owned()), Op("=".to_owned()), Integer(false, "3".to_owned()),
                
                Let, Ident("bar".to_owned()), Ident("quz".to_owned()), Colon, Ident("int32".to_owned()), Arrow, Ident("int32".to_owned()), Op("=".to_owned()),
                BeginBlock,
                    Let, Ident("baz".to_owned()), Colon, Ident("str".to_owned()), Op("=".to_owned()), StringLiteral("merp \\\" yes".to_owned()),Seq,
                    Return, Integer(false, "2".to_owned()),Seq,
                EndBlock,
                
                Let, Op("^^".to_owned()), Ident("lhs".to_owned()), Ident("rhs".to_owned()), Colon, Ident("int32".to_owned()), Arrow, Ident("int32".to_owned()), Arrow, Ident("int32".to_owned()), Op("=".to_owned()),
                BeginBlock,
                    Ident("bar".to_string()), Ident("foo".to_owned()),Seq,
                    Return, Integer(false, "1".to_owned()),Seq,
                EoF
            ]
        )
    }

    #[test]
    #[rustfmt::skip]
    fn chain_fn_calls() {
        const SRC: &'static str = r#"
let main _ : int32 -> int32 = 
    put_int32 100;
    print_str "v";
    return 32;
"#;

        use Token::*;
        assert_eq!(
            TokenStream::from_source(SRC).map(fst).collect_vec(),
            [
            
            Let, Ident("main".to_owned()), Ident("_".to_owned()), Colon, Ident("int32".to_owned()), Arrow, Ident("int32".to_owned()), Op("=".to_owned()),
            BeginBlock,
                Ident("put_int32".to_owned()), Integer(false,"100".to_owned()),Seq,
                Ident("print_str".to_owned()), StringLiteral("v".to_owned()),Seq,
                Return, Integer(false, "32".to_owned()),Seq,
            EoF
            ]
        )
    }

    #[test]
    #[rustfmt::skip]
    fn types() {
        const SRC : &'static str = r#"
type Foo = {
    a : int32,
}

enum Bar =
| A
| B int32
| C { a:int32, }"#;
        use Token::*;
        assert_eq!(
            TokenStream::from_source(SRC).map(|(a,_)| a).collect_vec(),
            [
            
            Type, Ident("Foo".to_string()), Op("=".to_string()), CurlOpen, 
            BeginBlock,
                Ident("a".to_owned()), Colon, Ident("int32".to_string()), Comma, 
            EndBlock,
            CurlClose,
            
            Enum, Ident("Bar".to_string()), Op("=".to_string()), 
            Op("|".to_string()), Ident("A".to_string()), 
            Op("|".to_string()), Ident("B".to_string()), Ident("int32".to_string()), 
            Op("|".to_string()), Ident("C".to_string()), CurlOpen, Ident("a".to_owned()), Colon, Ident("int32".to_string()), Comma, CurlClose, 
            EoF
            ]
        )
    }

    #[test]
    #[rustfmt::skip]
    fn control_flow() {
        use Token::*;
        const SRC_MATCH: &'static str = r#"
match x where
| Foo::Y bar -> ()
| Foo::Z { a, b } -> ()
"#;
        assert_eq!(
            TokenStream::from_source(SRC_MATCH)
                .map(fst)
                .collect_vec(),
            [
                
                Match,Ident("x".to_string()),Where,
                Op("|".to_string()),Ident("Foo".to_string()),Colon,Colon,Ident("Y".to_string()),Ident("bar".to_string()),Arrow,GroupOpen,GroupClose,
                Op("|".to_string()),Ident("Foo".to_string()),Colon,Colon,Ident("Z".to_string()),CurlOpen,Ident("a".to_string()),Comma,Ident("b".to_string()),CurlClose,Arrow,GroupOpen,GroupClose,
                EoF
            ],
            "Match"
        );

        const SRC_IF: &'static str = r#"
if cond then
    a
else
    b
"#;
        assert_eq!(
            TokenStream::from_source(SRC_IF)
                .map(fst)
                .collect_vec(),
            [
                
                If,
                Ident("cond".to_string()),
                Then,
                
                BeginBlock,
                Ident("a".to_string()),
                
                EndBlock,
                Else,
                
                BeginBlock,
                Ident("b".to_string()),
                
                EoF
            ],
            "If then else multiline"
        );
        assert_eq!(
            TokenStream::from_source("if cond then a else b")
                .map(fst)
                .collect_vec(),
            [
                If,
                Ident("cond".to_string()),
                Then,
                Ident("a".to_string()),
                Else,
                Ident("b".to_string()),
                EoF
            ]
        );
    }
}

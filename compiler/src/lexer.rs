use std::borrow::Cow;

use winnow::{
    ascii, combinator,
    error::{AddContext, ContextError, ParserError, StrContext},
    stream::Stream,
    token, Located, PResult, Parser, Stateful,
};

use crate::tokens;

#[derive(Debug)]
struct State<'input> {
    prefix: Cow<'input, str>,
}

type WithState<'input> = Stateful<Located<&'input str>, State<'input>>;

fn lex_all<'a>(input: &mut WithState<'a>) -> PResult<Vec<(tokens::Token<'a>, std::ops::Range<usize>)>> {
    combinator::repeat(0.., lex).parse_next(input)
}

fn ignore_blank_lines(input: &mut WithState<'_>) -> PResult<()> {
    let mut checkpoint = input.checkpoint();
    let _ = ascii::space0(input)?;
    combinator::opt(comment).parse_next(input)?;
    while let Some(_) = combinator::opt(ascii::line_ending).parse_next(input)? {
        checkpoint = input.checkpoint();
        ascii::space0(input)?;
        combinator::opt(comment).parse_next(input)?;
    }
    input.reset(&checkpoint);
    Ok(())
}

fn lex<'a>(input: &mut WithState<'a>) -> PResult<(tokens::Token<'a>, std::ops::Range<usize>)> {
    combinator::opt(comment).parse_next(input)?;
    if let Some(_) = combinator::opt(ascii::line_ending).parse_next(input)? {
        let _ = ignore_blank_lines(input);
        let (new_prefix, span) = ascii::space0.with_span().parse_next(input)?;
        if new_prefix.starts_with(input.state.prefix.as_ref()) {
            if new_prefix.len() > input.state.prefix.len() {
                input.state.prefix = new_prefix.into();
                return Ok((tokens::Token::BeginBlock, span));
            }
        } else if input.state.prefix.starts_with(new_prefix) {
            input.state.prefix = new_prefix.into();
            return Ok((tokens::Token::EndBlock, span));
        } else {
            // todo! more accurate errors.
            return Err(winnow::error::ErrMode::Cut(
                ContextError::new().add_context(
                    input,
                    &input.checkpoint(),
                    StrContext::Expected("tabbing/space error.".into()),
                ),
            ));
        }
    }

    let char_lit = char_lit.map(tokens::Token::CharLiteral);
    let string_lit = string_lit.map(tokens::Token::StringLiteral);
    
    
    combinator::alt((
        //idents and literals
        combinator::alt((
            (combinator::opt(combinator::alt((
                '+',
                '-',
            ))),ascii::digit1).map(|(sign,value) : (_,&str)| if Some('-') == sign {
                tokens::Token::Integer(format!("-{value}"))
            } else {
                tokens::Token::Integer(value.into())
            }),//todo translate this to unary op + numeric literal.
            (combinator::opt(combinator::alt((
                '+',
                '-',
            ))),ascii::digit1,'.',ascii::digit0).map(|(sign,whole,_, part) : (_,&str, _,&str)| if Some('-') == sign {
                tokens::Token::FloatingPoint(format!("-{whole}.{part}"))
            } else {
                tokens::Token::FloatingPoint(format!("{whole}.{part}"))
            }),
            op.map(tokens::Token::Op),
            ident.map(tokens::Token::Ident),
            char_lit,
            string_lit,
        )),
        // keywords
        combinator::alt((
            "for".value(tokens::Token::For),
            "let".value(tokens::Token::Let),
            "true".value(tokens::Token::True),
            "false".value(tokens::Token::False),
            "if".value(tokens::Token::If),
            "then".value(tokens::Token::Then),
            "else".value(tokens::Token::Else),
            "enum".value(tokens::Token::Enum),
            "type".value(tokens::Token::Type),
            "where".value(tokens::Token::Where),
            "return".value(tokens::Token::Return),
            "match".value(tokens::Token::Match),
            // "implements".value(tokens::Token::Implements), //TODO! implements blocks
            // "mod".value(tokens::Token::Mod), // TODO! submoduling
        )),
        // special symbols
        combinator::alt((
            // grouping
            '('.value(tokens::Token::GroupOpen),
            ')'.value(tokens::Token::GroupClose),
            '['.value(tokens::Token::BracketOpen),
            ']'.value(tokens::Token::BracketClose),
            '{'.value(tokens::Token::CurlOpen),
            '}'.value(tokens::Token::CurlClose),
            // other
            ','.value(tokens::Token::Comma),
            ':'.value(tokens::Token::Colon),
            ';'.value(tokens::Token::Seq),
            // extra special.  this one will condintionally decay to an op token in any context other than a type.
            "->".value(tokens::Token::Arrow),
        )),
        
    ))
    .with_span()
    .parse_next(input)
}


fn char_lit(input: &mut WithState<'_>) -> PResult<String> {
    combinator::delimited(
        '\'',
        escaper('\''),
        combinator::preceded(combinator::not('\\'), '\''),
    )
    .parse_next(input)
}

fn string_lit(input: &mut WithState<'_>) -> PResult<String> {
    combinator::trace(
        "string_lit",
        combinator::delimited(
            '"',
            escaper('"'),
            /* combinator::preceded(combinator::not('\\'), */
            combinator::cut_err('"') //)
                .context(winnow::error::StrContext::Label("string terminator".into())),
        ),
    )
    .parse_next(input)
}

fn escaper<'a, E: ParserError<WithState<'a>>>(surronding: char) -> impl Parser<WithState<'a>, String, E> {
    ascii::escaped_transform::<_, _, _, _, String>(
        token::take_till(1.., ['\n', '\r', '\\', surronding]),
        '\\',
        combinator::alt((
            "n".value("\n"),
            //todo ocatal and hex escapes.
            "t".value("\t"),
            "\\".value("\\"),
            "\"".value("\""),
            "\'".value("\'"),
            "r".value("\r"), //don't expect this to be common.
                             //more escapes?
        )),
    )
}

const KEYWORDS: &[&'static str] = &[
    "for",
    "let",
    "true",
    "false",
    "type",
    "mod",
    "implements",
    "enum",
    "return",
    "where",
];

fn simple_ident<'input>(input: &mut WithState<'input>) -> PResult<Cow<'input, str>> {
    token::take_while(1.., |c: char| c.is_alphanumeric() || c == '_')
        .verify(|s: &str| !s.starts_with(char::is_numeric))
        .context(winnow::error::StrContext::Expected(
            "indentifers can't start with numbers".into(),
        ))
        .verify(|s| !KEYWORDS.contains(s))
        .context(winnow::error::StrContext::Expected(
            "identifiers can't be a keyword".into(),
        ))
        .map(Into::into)
        .parse_next(input)
}

fn ident<'input>(input: &mut WithState<'input>) -> PResult<String> {
    combinator::separated_foldl1(simple_ident.map(Into::into), "::", |a, _, b| {
        format!("{a}::{b}")
    })
    .parse_next(input)
}



fn op<'input>(input: &mut WithState<'input>) -> PResult<&'input str> {
    token::take_while(
        1..,
        [
            '|', '>', '<', '!', '@', '=', '&', '+', '-', '\\', '/', '*', '^', '.',
        ],
    )
    .parse_next(input)
}

fn comment<I>(input: &mut I) -> PResult<()>
where
    I: winnow::stream::Stream
        + winnow::stream::Compare<&'static str>
        + winnow::stream::FindSlice<(char, char)>
        + winnow::stream::StreamIsPartial,
    <I as winnow::stream::Stream>::Token: Clone + winnow::stream::AsChar,
{
    combinator::alt((line_comment, multiline_comment)).parse_next(input)
}

fn line_comment<I>(input: &mut I) -> PResult<()>
where
    I: winnow::stream::Stream
        + winnow::stream::Compare<&'static str>
        + winnow::stream::FindSlice<(char, char)>
        + winnow::stream::StreamIsPartial,
    <I as winnow::stream::Stream>::Token: Clone + winnow::stream::AsChar,
{
    combinator::preceded("#!", ascii::till_line_ending)
        .void()
        .parse_next(input)?;
    ascii::line_ending.void().parse_next(input)
}

fn multiline_comment<I>(input: &mut I) -> PResult<()>
where
    I: winnow::stream::Stream
        + winnow::stream::Compare<&'static str>
        + winnow::stream::FindSlice<(char, char)>
        + winnow::stream::StreamIsPartial,
    <I as winnow::stream::Stream>::Token: Clone + winnow::stream::AsChar,
{
    combinator::trace(
        "multiline comment",
        combinator::preceded(
            "#/",
            combinator::repeat_till::<_, _, (), _, _, _, _>(
                0..,
                combinator::alt((multiline_comment, token::any.void())),
                "/#",
            ),
        ),
    )
    .void()
    .parse_next(input)
}

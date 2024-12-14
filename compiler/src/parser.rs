use crate::{ast, tokens::Token, types};
use std::borrow::Cow;
use winnow::{
    ascii, combinator,
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext},
    stream::{AsChar, Located, Location, Stream as WStream, StreamIsPartial},
    token, PResult, Parser, Stateful,
};

type Stream<'input> = Stateful<Located<StrippedParser<'input>>,OuterState<'input>>;

#[derive(Default,Clone,Debug)]
struct State {
    in_line_comment:bool,
    multiline_comment_depth:usize,//depth to allow for nesting multiline comments
    changed_multiline_comment_status:bool,
    in_str_lit:bool,
    in_char_lit:bool,
    escaping:bool,
}

#[derive(Default,Debug)]
struct OuterState<'input> {

    prefix: Cow<'input, str>,
    just_consumed_line:bool,
    multi_line_expr:bool,
}

#[derive(Clone)]
struct StrippedParser<'input> {
    state : State,
    src : &'input str,
}

impl StrippedParser<'_> {
    fn is_empty(&self) -> bool {
        self.src.is_empty()
    }
}

impl std::fmt::Debug for StrippedParser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.src.fmt(f)

    }
}

#[derive(Clone)]
struct StrippedParserCheckpoint<'a> {
    inner : <&'a str as WStream>::Checkpoint,
}

impl std::fmt::Debug for StrippedParserCheckpoint<'_> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl winnow::stream::Offset for StrippedParserCheckpoint<'_> {
    fn offset_from(&self, other:&Self) -> usize {
        self.inner.offset_from(&other.inner)
    }
}

#[derive(Clone)]
struct StrippedParserIterator<'a> {
    base : StrippedParser<'a>,
}

impl<'a> std::iter::Iterator for StrippedParserIterator<'a> {
    type Item =<StrippedParser<'a> as WStream>::Token;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.base.next_token()
    }
}

impl<'a> WStream for StrippedParser<'a> {
    type Token = <&'a str as WStream>::Token;

    type Slice=<&'a str as WStream>::Slice;

    type IterOffsets=std::iter::Enumerate<StrippedParserIterator<'a>>;

    type Checkpoint= StrippedParserCheckpoint<'a>;

    fn iter_offsets(&self) -> Self::IterOffsets {
        StrippedParserIterator { base:self.clone() }.enumerate()
    }

    fn eof_offset(&self) -> usize {
        self.src.eof_offset()
    }

    fn next_token(&mut self) -> Option<Self::Token> {
        let token= self.src.next_token()?;
        let out = match token {
            '\n' if self.state.in_line_comment => {
                self.state.in_line_comment = false;
                Some('\n')
            },
            '"' if !self.state.in_str_lit && !(self.state.in_line_comment || self.state.multiline_comment_depth>0) => {
                self.state.in_str_lit = true;
                Some('"')
            },
            '"' if !self.state.escaping && !(self.state.in_line_comment || self.state.multiline_comment_depth>0) => {
                self.state.in_str_lit = false;
                Some('"')
            },
            '\'' if !self.state.in_char_lit && !(self.state.in_line_comment || self.state.multiline_comment_depth>0) =>{
                self.state.in_char_lit = true;
                Some('\'')
            },
            '\'' if !self.state.escaping && !(self.state.in_line_comment || self.state.multiline_comment_depth>0) => {
                self.state.in_char_lit = false;
                Some('\'')
            },
            '\\' if (self.state.in_char_lit || self.state.in_str_lit) && !(self.state.in_line_comment || self.state.multiline_comment_depth>0)=> {
                self.state.escaping = !self.state.escaping;
                Some('\\')
            }
            '#' if !self.state.in_str_lit && !self.state.in_char_lit => {
                if Some('/') == self.src.peek_token().map(|(_,it)| it) {
                    self.state.multiline_comment_depth+=1;
                    self.state.changed_multiline_comment_status = true;
                    Some(' ')
                } else if self.state.changed_multiline_comment_status {
                    self.state.changed_multiline_comment_status =false;
                    Some(' ')
                } else if Some('!') == self.src.peek_token().map(|(_,it)| it) {
                    self.state.in_line_comment = true;
                    Some(' ')
                } else {
                    Some('#')
                }
            }
            '/' if self.state.multiline_comment_depth>0 => {
                if Some('#') == self.src.peek_token().map(|(_,it)| it) {
                    self.state.changed_multiline_comment_status = true;
                    self.state.multiline_comment_depth -= 1;
                    Some(' ')
                } else if self.state.changed_multiline_comment_status {
                    self.state.changed_multiline_comment_status = false;
                    Some(' ')
                } else {
                    Some('/')
                }
            }
            _ if self.state.in_line_comment || self.state.multiline_comment_depth > 0 => Some(' '),
            _ => {
                self.state.escaping=false;

                Some(token)
            },
        };
        // println!("token {token:?} state {:?} out {out:?}", &self.state);
        out 
    }

    fn offset_for<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Token) -> bool {
        for (o,c) in self.iter_offsets() {
            if predicate(c) {
                return Some(o);
            }
        }
        None
    }

    fn offset_at(&self, tokens: usize) -> Result<usize, winnow::error::Needed> {
        self.src.offset_at(tokens)
    }

    fn next_slice(&mut self, offset: usize) -> Self::Slice {

        self.src.next_slice(offset)
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        StrippedParserCheckpoint { inner:self.src.checkpoint() }
    }

    fn reset(&mut self, checkpoint: &Self::Checkpoint) {
        self.src.reset(&checkpoint.inner)
    }

    fn raw(&self) -> &dyn std::fmt::Debug {
        self
    }
}



impl winnow::stream::Offset<Self> for StrippedParser<'_> {
    fn offset_from(&self, other:&Self) ->usize {
        self.src.offset_from(&other.src.checkpoint())
    }
}

impl<'a> winnow::stream::Offset<StrippedParserCheckpoint<'a>> for StrippedParser<'a> {
    fn offset_from(&self, checkpoint:&StrippedParserCheckpoint<'a>) -> usize {
        self.src.offset_from(&checkpoint.inner)
    }
}

impl<'a,T> winnow::stream::Compare<T> for StrippedParser<'a> 
where &'a str : winnow::stream::Compare<T> {
    fn compare(&self, t: T) -> winnow::stream::CompareResult {
        self.src.compare(t)
    }
}

impl<'a> winnow::stream::StreamIsPartial for StrippedParser<'a>{
    type PartialState = <&'a str as winnow::stream::StreamIsPartial>::PartialState;

    fn complete(&mut self) -> Self::PartialState {
        self.src.complete()
    }

    fn restore_partial(&mut self, state: Self::PartialState) {
        self.src.restore_partial(state)
    }

    fn is_partial_supported() -> bool {
        <Located<&'a str> as winnow::stream::StreamIsPartial>::is_partial_supported()
    }
}


impl winnow::stream::AsBStr for StrippedParser<'_> {
    fn as_bstr(&self) -> &[u8] {
        self.src.as_bstr()
    }
}

pub(crate) fn file(file_name:&str, src :&str) ->ast::ModuleDeclaration{
    let src = Stream {
        input: Located::new(StrippedParser{
            src,
            state:Default::default(),
        }),
        state: Default::default(),
    };

    let decls = combinator::delimited(ignore_blank_lines, top_level_block,(ascii::space0,ignore_blank_lines)).parse(src).unwrap();

    ast::ModuleDeclaration{ loc: None, name: file_name.into(), declarations: decls }
}

fn parens< I, O, E>(
    parser: impl Parser<I, O, E>
) -> impl Parser<I, O, E> 
where I : winnow::stream::Stream<Token=char> + winnow::stream::StreamIsPartial + winnow::stream::Compare<char>,
      E : ParserError<I> 
{
    combinator::delimited(
        (ascii::space0,'(',ascii::space0),
        parser,
        (ascii::space0,')'),
        
    )
}
// (name,weight,right associative)
const PRECIDENCE: [(&'static str, usize, bool); 12] = [
    (".", usize::MAX, true),
    ("**", 7, false),
    ("*", 5, true),
    ("/", 5, true),
    ("+", 4, true),
    ("-", 4, true),
    ("<", 2, true),
    ("<=", 2, true),
    (">", 2, true),
    (">=", 2, true),
    ("&&", 1, true),
    ("||", 1, true),
];

enum ShuntingYardOptions {
    Expr(ast::Expr),
    Op((String, crate::Location)),
}

fn expect_line(input : &mut Stream<'_>) -> PResult<()> {
    combinator::trace("expect line",|input : &mut Stream<'_>| {

        if input.state.just_consumed_line {
            return Ok(());
        }
        let _ = ascii::space0(input)?;
        let _ = ascii::line_ending(input)?;
        input.state.just_consumed_line = true;
        Ok(())
    }).parse_next(input)
}

fn reset_line(input:&mut Stream<'_>) -> PResult<()> {
    combinator::trace("reset line",|input : &mut Stream<'_>| {
        input.state.just_consumed_line=false;
        Ok(())
    }).parse_next(input)
}

fn ignore_blank_lines(input: &mut Stream<'_>) -> PResult<()> {
    combinator::trace("blank lines",|input : &mut Stream<'_>| {

        let mut checkpoint = input.checkpoint();
        let _ = ascii::space0(input)?;
        while let Some(_) = combinator::opt(ascii::line_ending).parse_next(input)? {
            checkpoint = input.checkpoint();
            ascii::space0(input)?;
        }
        input.reset(&checkpoint);
        Ok(())
    }).parse_next(input)
}


fn char_lit(input: &mut Stream<'_>) -> PResult<String> {
    combinator::delimited(
        '\'',
        escaper('\''),
        combinator::preceded(combinator::not('\\'), '\''),
    )
    .parse_next(input)
}

fn string_lit(input: &mut Stream<'_>) -> PResult<String> {
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

fn escaper<'a, E: ParserError<Stream<'a>>>(surronding: char) -> impl Parser<Stream<'a>, String, E> {
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


fn simple_ident<'input>(input: &mut Stream<'input>) -> PResult<Cow<'input, str>> {
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

fn ident(input:&mut Stream<'_>) -> PResult<String> {
    combinator::separated_foldl1(
        simple_ident.map(Into::into),
        (ascii::space0,"::",ascii::space0),
        |left,_,right| format!("{left}::{right}")
    ).parse_next(input)
}

fn top_level_block(input: &mut Stream<'_>) -> PResult<Vec<ast::TopLevelDeclaration>> {
    ignore_blank_lines(input)?;
    let old_prefix = input.state.prefix.clone();
    let checkpoint = input.checkpoint();
    let new_prefix = ascii::space0(input)?;
    input.reset(&checkpoint);
    if !new_prefix.starts_with(old_prefix.as_ref()) {
        Ok(Vec::new())
    } else {
    
        input.state.prefix = new_prefix.into();
        let result = combinator::repeat(0..,combinator::preceded((ignore_blank_lines,ascii::space0.verify(|it:&str| it == new_prefix)), top_level_decl)).parse_next(input);
        input.state.prefix = old_prefix;
        result
    }
}




fn top_level_decl(input: &mut Stream<'_>) -> PResult<ast::TopLevelDeclaration> {
    

    let result = combinator::alt((
        mod_decl.map(ast::TopLevelDeclaration::Mod),
        top_level_value.map(ast::TopLevelDeclaration::Value),
        type_decl.map(ast::TopLevelDeclaration::TypeDefinition),
    ))
    .parse_next(input)?;
    
    Ok(result)
}


fn type_decl(input: &mut Stream<'_>) -> PResult<ast::TypeDefinition> {
    combinator::trace("type declaration", |input: &mut Stream<'_>| {
        let generics = combinator::opt(generics).parse_next(input)?;
        
        combinator::alt((
            combinator::preceded(
                (ascii::space0,"type",ascii::space1),
                combinator::cut_err(struct_or_alias_def(generics.clone())),
            ),
            enum_decl(generics).map(ast::TypeDefinition::Enum),
        ))
        .parse_next(input)
    })
    .map(|mut decl| {
        decl.bind_generics();
        decl
    })
    .parse_next(input)
}

const EQ_OP : Token<'static> = Token::Op("=");

fn enum_decl<'input>(
    generics: Option<ast::GenericsDecl>,
) -> impl Parser<Stream<'input>, ast::EnumDeclaration, ContextError> {
    combinator::trace("enum declaration", move |input: &mut Stream<'_>| {
        let (ident, loc) =
            combinator::preceded(
                (ascii::space0,"enum",ascii::space1), 
            simple_ident.with_span())
                .parse_next(input)?;
        let values = combinator::preceded(
            (ascii::space0,'=',ascii::space0),
            combinator::repeat(
                1..,
                combinator::preceded(
                    (ascii::space0,'|', ascii::space0),
                    |input: &mut Stream<'_>| {
                        let (ident, loc) = simple_ident.with_span().parse_next(input)?;
                        let _ = ascii::space0(input)?;
                        let loc = (loc.start, loc.end);
                        let struct_ident = ident.clone();
                        let tuple_ident = ident.clone();
                        combinator::alt((
                            struct_body.map(move |fields| ast::EnumVariant::Struct {
                                ident: struct_ident.clone().into(),
                                fields,
                                loc,
                            }),
                            type_.map(move |ty| ast::EnumVariant::Tuple {
                                ident: tuple_ident.clone().into(),
                                ty,
                                loc,
                            }),
                            combinator::empty.value(ast::EnumVariant::Unit {
                                ident: ident.into(),
                                loc,
                            }),
                        ))
                        .parse_next(input)
                    },
                ),
            ),
        )
        .parse_next(input)?;
        Ok(ast::EnumDeclaration {
            ident: ident.into(),
            generics: generics.clone(),
            values,
            loc: (loc.start, loc.end),
        })
    })
}



fn struct_body(input: &mut Stream<'_>) -> PResult<Vec<ast::FieldDecl>> {
    combinator::delimited(
        (ascii::space0,'{',ascii::multispace0),
        combinator::separated(
            0..,
            combinator::separated_pair(
                simple_ident.with_span(),
                (ascii::multispace0,':',ascii::multispace0),
                type_
            ).map(|((ident, loc), ty)| ast::FieldDecl {
                name: ident.into(),
                ty,
                loc: (loc.start, loc.end),
            }),
            (ascii::multispace0, ',',ascii::multispace0),
        ),
        (ascii::multispace0,combinator::opt((',',ascii::multispace0)),'}'),
    )
    .parse_next(input)
}

fn struct_or_alias_def<'input>(
    generics: Option<ast::GenericsDecl>,
) -> impl Parser<Stream<'input>, ast::TypeDefinition, ContextError> {
    combinator::trace(
        "struct or alias",
        move |input: &mut Stream<'_>| -> PResult<ast::TypeDefinition> {
            let (ident, loc) = simple_ident.with_span().parse_next(input)?;

            let _ = (ascii::space0,'=',ascii::space0).void().parse_next(input)?;
            let result = combinator::alt((
                combinator::trace("struct", struct_body).map(|fields| {
                    ast::TypeDefinition::Struct(ast::StructDefinition {
                        ident: ident.clone().into(),
                        generics: generics.clone(),
                        values: fields,
                        loc: (loc.start, loc.end),
                    })
                }),
                combinator::trace("alias", type_)
                    .verify(|_| generics.is_none())
                    .context(winnow::error::StrContext::Label(
                        "generic aliases are not yet implemented",
                    ))
                    .map(|ty| ast::TypeDefinition::Alias(ident.clone().into(), ty)),
            ))
            .parse_next(input)?;
            Ok(result)
        },
    )
}


fn op<'input>(input: &mut Stream<'input>) -> PResult<Cow<'input,str>> {
    token::take_while(
        1..,
        [
            '|', '>', '<', '!', '@', '=', '&', '+', '-', '\\', '/', '*', '^', '.',
        ],
    )
    .map(Into::into)
    .parse_next(input)
}

fn mod_decl<'input>(input: &mut Stream<'input>) -> PResult<ast::ModuleDeclaration> {
    //TODO! 
    combinator::fail.context(StrContext::Label("submoules not yet supported")).parse_next(input)
}

fn unit(input:&mut Stream<'_>) -> PResult<()> {
    (
        '(',
        ascii::space0,
        ')'    
    ).void()
    .parse_next(input)
}

fn block(input: &mut Stream) -> PResult<ast::Block> {
combinator::trace("block", |input: &mut Stream<'_>|{
        // let _ = (ascii::space0,ascii::line_ending).void().parse_next(input)?;
        let old_prefix = input.state.prefix.clone();
        let checkpoint= input.checkpoint();
        let new_prefix = ascii::space0(input)?;
        if !new_prefix.starts_with(old_prefix.as_ref()) {
            return Err(ErrMode::Cut(ContextError::new().add_context(input, &checkpoint, StrContext::Expected(winnow::error::StrContextValue::Description("expected the body to be an ident deeper")))))
        }
        input.reset(&checkpoint);
        input.state.prefix=new_prefix.into();
        let result = (
            combinator::repeat(
                0..,
                combinator::preceded(
                    ignore_blank_lines,
                    combinator::delimited(
                        combinator::trace("prefix",(new_prefix,reset_line)),
                        combinator::trace(format!("block statement prefix {:?}",new_prefix),combinator::repeat(1..,statement)),
                        combinator::trace("ending",combinator::alt((expect_line,(ascii::space0,combinator::eof).void())))
                     )
                )
            ).map(|statements : Vec<Vec<_>>| {
                    statements.into_iter().flat_map(Vec::into_iter).collect()
            })
            ,
            combinator::opt(combinator::preceded((ignore_blank_lines,new_prefix,reset_line),expr).map(Into::into))
        ).map(|(statements,implicit_ret)| ast::Block { statements, implicit_ret })
        .parse_next(input);
        input.state.prefix=old_prefix;
        result
    }
).parse_next(input)
}

fn comma(input:&mut Stream<'_>) -> PResult<()> {
    (ascii::space0,',',ascii::space0).void().parse_next(input)
}

fn type_<'input>(input: &mut Stream<'input>) -> PResult<types::ResolvedType> {
    let result = combinator::trace(
        "type",
        combinator::alt((
            "int8".value(types::INT8),
            "int16".value(types::INT16),
            "int32".value(types::INT32),
            "int64".value(types::INT64),
            "float32".value(types::FLOAT32),
            "float64".value(types::FLOAT64),
            unit.value(types::UNIT),
            "str".value(types::STR),
            "char".value(types::CHAR),
            "bool".value(types::BOOL),
            (ident,combinator::opt(combinator::delimited((ascii::space0,'<',ascii::space0), combinator::separated(0.., type_, comma),(ascii::space0,'>',ascii::space0)))).with_span().map(|((ident,generics),span)| {
                types::ResolvedType::User{
                    name: ident.into(),
                    generics:generics.unwrap_or_default(),
                    loc: (span.start,span.end),
                }
            }),
            //array
            combinator::delimited(
                (ascii::space0, '[', ascii::space0),
                combinator::separated_pair(
                    type_,
                    (ascii::space0, ';', ascii::space0),
                    ascii::dec_uint,
                ),
                (ascii::space0, ']', ascii::space0),
            )
            .map(|(underlining, count)| types::ResolvedType::Array {
                underlining: underlining.into(),
                size: count,
            }),
            combinator::delimited(
                (ascii::space0, '[', ascii::space0),
                type_,
                (ascii::space0, ']', ascii::space0),
            )
            .map(|underlining| types::ResolvedType::Slice {
                underlining: underlining.into(),
            }),
            combinator::preceded((ascii::space0, '&', ascii::space0), type_).map(|underlining| {
                types::ResolvedType::Ref {
                    underlining: underlining.into(),
                }
            }),
            parens(combinator::separated(
                2..,
                type_,
                (ascii::space0, ',', ascii::space0),
            ))
            .with_span()
            .map(|(underlining, loc)| types::ResolvedType::Tuple {
                underlining,
                loc: (loc.start, loc.end),
            }),
            parens(type_),
        )),
    )
    .parse_next(input)?;
    if let Some((loc, ret)) = combinator::trace(
        "type (function)",
        combinator::opt((
            combinator::delimited(ascii::space0, "->".span(), ascii::space0),
            type_,
        )),
    )
    .parse_next(input)?
    {
        Ok(types::ResolvedType::Function {
            arg: result.into(),
            returns: ret.into(),
            loc: (loc.start, loc.end),
        })
    } else {
        Ok(result)
    }
}

fn arg<'input>(input: &mut Stream<'input>) -> PResult<ast::ArgDeclaration> {
    let apply_ty = combinator::delimited(
        ('(', ascii::space0),
        combinator::separated_pair(arg, (ascii::space0, ':', ascii::space0), type_),
        (ascii::space0, ')'),
    )
    .map(|(mut arg, real_ty)| {
        match &mut arg {
            ast::ArgDeclaration::Simple { ty, .. }
            | ast::ArgDeclaration::DestructureTuple(_, ty, _)
            | ast::ArgDeclaration::Discard { ty, .. }
            | ast::ArgDeclaration::Unit { ty, .. } => {
                ty.replace(real_ty);
            }
            ast::ArgDeclaration::DestructureStruct {
                loc,
                struct_ident,
                fields,
                renamed_fields,
            } => todo!("not a common case.  (StructType {{ ... }} : StructType)"),
        }
        arg
    });
    let unit = ('(', ascii::space0, ')')
        .with_span()
        .map(|((_, _space, _), loc)| {
            //todo generate warnings??
            ast::ArgDeclaration::Unit {
                loc: (loc.start, loc.end),
                ty: None,
            }
        });
    combinator::alt((
        unit,
        apply_ty,
        "_".span().map(|loc| ast::ArgDeclaration::Discard {
            loc: (loc.start, loc.end),
            ty: None,
        }),
        combinator::delimited(('(', ascii::space0), arg, (ascii::space0, ')')),
        combinator::delimited(
            ('(', ascii::space0),
            combinator::separated(1.., arg, (ascii::space0, ',', ascii::space0)),
            (ascii::space0, ')'),
        )
        .with_span()
        .map(|(args, loc)| ast::ArgDeclaration::DestructureTuple(args, None, (loc.start, loc.end))),
        simple_ident
            .with_span()
            .map(|(ident, loc)| ast::ArgDeclaration::Simple {
                loc: (loc.start, loc.end),
                ident: ident.into(),
                ty: None,
            }),
        // todo! other destrctures
    ))
    .parse_next(input)
}

fn top_level_value(input: &mut Stream<'_>) -> PResult<ast::TopLevelValue> {
    let generics = combinator::opt(generics).parse_next(input)?;
    let abi = combinator::opt(
        combinator::preceded(
            ("extern", ascii::space1),
            combinator::alt((
                r#""C""#.span().map(|span| ast::Abi {
                    loc: (span.start, span.end),
                    identifier: "C".into(),
                }),
                r#""intrinsic""#.span().map(|span| ast::Abi {
                    loc: (span.start, span.end),
                    identifier: "interinsic".into(),
                }),
            )),
        )
        .verify(|_| generics.is_none())
        .context(StrContext::Expected(
            "can not combine an abi with generics".into(),
        )),
    )
    .parse_next(input)?;

    (ascii::multispace0, "let").void().parse_next(input)?;
    ascii::space1(input)?;
    let ((ident, loc), is_op) = combinator::alt((
        parens(op).with_span().map(|it| (it, true)),
        simple_ident.with_span().map(|it| (it, false)),
    ))
    .parse_next(input)?;
    let args = combinator::opt(combinator::preceded(
        ascii::space1,
        combinator::separated(1.., arg, ascii::space1),
    ))
    .parse_next(input)?
    .unwrap_or_default();
    let ty = combinator::opt(combinator::preceded(
        (ascii::space0, ':', ascii::space0),
        type_,
    ))
    .parse_next(input)?;
    if let Some(_) = combinator::opt((ascii::space0, '=').void()).parse_next(input)? {
        let mut value = ast::TopLevelValue {
            loc: (loc.start, loc.end),
            is_op,
            ident: ident.into(),
            args,
            ty,
            value: combinator::trace(
                "value",
                combinator::alt((
                    combinator::preceded((ascii::space0, ascii::line_ending), block)
                        .map(ast::ValueType::Function),
                    combinator::delimited(ascii::space0, expr, |input:&mut Stream<'_>| {
                        if input.state.multi_line_expr {
                            input.state.multi_line_expr=false;
                            Ok(())
                        } else {
                            (ascii::space0,';').void().parse_next(input)
                        }
                    })
                    .map(ast::ValueType::Expr),
                )),
            )
            .parse_next(input)?,
            generics,
            abi,
        };
        value.bind_generics();
        Ok(value)
    } else {
        (ascii::space0, ';')
            .void()
            .verify(|_| abi.is_some() && ty.is_some() && generics.is_none())
            .parse_next(input)?;
        Ok(ast::TopLevelValue {
            loc: (loc.start, loc.end),
            is_op,
            ident: ident.into(),
            args,
            ty,
            value: ast::ValueType::External,
            generics,
            abi,
        })
    }
}

fn generics<'input>(input: &mut Stream<'input>) -> PResult<ast::GenericsDecl> {
    let for_loc = "for".span().parse_next(input)?;
    let decls = combinator::delimited(
        (ascii::space0, '<'),
        combinator::separated(
            1..,
            combinator::delimited(
                ascii::space0,
                simple_ident
                    .with_span()
                    .map(|(a, b)| ((b.start, b.end), a.into())),
                ascii::space0,
            ),
            ',',
        ),
        (ascii::space0, '>'),
    )
    .parse_next(input)?;
    Ok(ast::GenericsDecl {
        for_loc: (for_loc.start, for_loc.end),
        decls,
    })
}

fn if_statement(input :&mut Stream<'_>) -> PResult<ast::If> {
    let prefix = input.state.prefix.clone();
    combinator::trace("if",winnow::combinator::seq! { ast::If{
        loc:"if".span().map(|loc| (loc.start,loc.end)),
        cond : combinator::delimited(ascii::multispace1,combinator::cut_err(expr).map(Into::into),(ascii::multispace1,"then")),
        true_branch : combinator::alt((
            combinator::preceded((ascii::space0,ascii::line_ending),block),
            expr.map(|expr| ast::Block { statements:Vec::new(), implicit_ret:Some(expr.into())})
        )),
        _:ignore_blank_lines,
        else_branch : combinator::opt(
            combinator::preceded(
                combinator::preceded(&*prefix, "else"),
                combinator::alt((
                    combinator::preceded(ascii::space1,if_statement).map(|if_|ast::Block { statements : vec![ast::Statement::IfStatement(if_)], implicit_ret:None}),
                    combinator::preceded((ascii::space0,ascii::line_ending),block),
                    expr.map(|expr| ast::Block { statements:Vec::new(), implicit_ret:Some(expr.into())})
                ))
            )
        ),
    }
    }).parse_next(input)
}

fn statement(input: &mut Stream<'_>) -> PResult<ast::Statement> {
    let declaration = combinator::preceded(
        ("let", ascii::space1),
        combinator::cut_err((
            pattern
                .verify(|pat| {
                    !matches!(
                        pat,
                        ast::Pattern::Or(_, _) 
                        | ast::Pattern::EnumVariant { .. } 
                        | ast::Pattern::ConstNumber(_)
                        | ast::Pattern::ConstStr(_)
                        | ast::Pattern::ConstChar(_)
                        | ast::Pattern::ConstBool(_)
                        
                    )
                })
                .with_span()
                .map(|(pat, loc)| ((loc.start, loc.end), pat)),
            combinator::opt(combinator::preceded(
                (ascii::space0, ':', ascii::space0),
                type_,
            )),
            combinator::preceded(
                (ascii::space0, "=", ascii::space0),
                combinator::alt((
                    combinator::preceded((ascii::space0, ascii::line_ending), block)
                        .map(ast::ValueType::Function),
                    combinator::terminated(expr,(ascii::space0,';')).map(ast::ValueType::Expr),
                )),
            ),
        )),
    )
    .map(|((loc, target), ty, value)| ast::ValueDeclaration {
        loc,
        is_op: false,
        target,
        args: Vec::new(),
        ty,
        value,
        generictypes: None,
        abi: None,
    });
    let multiline_context = input.state.multi_line_expr;
    let result = combinator::trace(
        "statement",
            combinator::alt((
                
                declaration.map(ast::Statement::Declaration),
                match_.map(ast::Statement::Match),
                if_statement.map(ast::Statement::IfStatement),
                combinator::terminated(combinator::alt((

                    ("return".span(),
                    combinator::cut_err(combinator::preceded(ascii::space1, expr)),
                
                    ).map(|(loc, expr)| ast::Statement::Return(expr.into(), (loc.start, loc.end))),
                    expr.map(ast::Statement::Expr),
                
                    )),
                    (ascii::multispace0,';')
                ),
                combinator::fail.context(StrContext::Label("Invalid statement"))//this should never be reached???
            )),
    )
    .parse_next(input);
    input.state.multi_line_expr=multiline_context;
    // ignore_blank_lines(input)?;
    result
}



fn simple_expr(input:&mut Stream<'_>) -> PResult<ast::Expr> {

    
    if input.is_empty() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }

    let if_ = combinator::trace("if",winnow::combinator::seq! { ast::If{
        loc:"if".span().map(|loc| (loc.start,loc.end)),
        cond : combinator::delimited(ascii::multispace1,expr.map(Into::into),(ascii::multispace1,"then")),
        true_branch : combinator::alt((
            combinator::preceded((ascii::space0,ascii::line_ending,|input:&mut Stream<'_>| {
                input.state.multi_line_expr=true;
                Ok(())
            }),block),
            expr.map(|expr| ast::Block { statements:Vec::new(), implicit_ret:Some(expr.into())})
        )),
        _:(ascii::multispace0,"else"),//TODO! need to check indent level or if inline then space preceded.
        else_branch : combinator::alt((
            combinator::preceded((ascii::space0,ascii::line_ending,|input:&mut Stream<'_>| {
                input.state.multi_line_expr=true;
                Ok(())
            }),block),
            expr.map(|expr| ast::Block { statements:Vec::new(), implicit_ret:Some(expr.into())})
        )).map(Into::into),
    }
    });
    let struct_con = (
        type_.with_span(),
        combinator::delimited(
            (ascii::space0, '{', ascii::multispace0),
            combinator::separated(
                0..,
                combinator::alt((
                    combinator::separated_pair(
                        simple_ident,
                        (ascii::space0, ':', ascii::space0),
                        expr,
                    )
                    .map(|(ident, expr)| {
                        let loc = expr.get_loc();
                        (ident, (expr, loc))
                    }),
                    simple_ident.with_span().map(|(ident, loc)| {
                        (
                            ident.clone(),
                            (
                                ast::Expr::ValueRead(ident.into(), (loc.start, loc.end)),
                                (loc.start, loc.end),
                            ),
                        )
                    }),
                )),
                (ascii::multispace0, ',', ascii::multispace0),
            ),
            (ascii::space0, '}', ascii::multispace0),
        ),
    )
        .map(|((ty, loc), fields): (_, Vec<_>)| {
            if let types::ResolvedType::User { name, generics, .. } = ty {
                ast::Expr::StructConstruction(ast::StructConstruction {
                    loc: (loc.start, loc.end),
                    fields: fields
                        .into_iter()
                        .map(|(ident, value)| (ident.into(), value))
                        .collect(),
                    generics,
                    ident: name,
                })
            } else {
                ast::Expr::Error
            }
        });

        ascii::space0(input)?;
        combinator::trace(
            "simple expr",
            combinator::alt((
                //axioms
                "()".map(|_| ast::Expr::UnitLiteral),
                "true"
                    .span()
                    .map(|loc| ast::Expr::BoolLiteral(true, (loc.start, loc.end))),
                "false"
                    .span()
                    .map(|loc| ast::Expr::BoolLiteral(false, (loc.start, loc.end))),
                combinator::trace("struct construction",struct_con),
                ascii::dec_int.map(|it: i128| ast::Expr::NumericLiteral {
                    value: it.to_string(),
                }),
                ascii::float.map(|it: f64| ast::Expr::NumericLiteral {
                    value: it.to_string(),
                }),
                string_lit.map(ast::Expr::StringLiteral),
                char_lit.map(ast::Expr::CharLiteral),
                // todo! operator read.  important for passing operators to functions
                // parens(op).map(ast::Expr::OpRead)
                //dependents.
                parens(expr),
                //tuple
                combinator::trace("tuple",parens(combinator::separated(
                    1..,
                    expr,
                    (ascii::multispace0, ',', ascii::multispace0),
                )))
                .with_span()
                .map(|(contents, loc)| ast::Expr::TupleLiteral {
                    contents,
                    loc: (loc.start, loc.end),
                }),
                //array
                combinator::trace("array construction",combinator::delimited(
                    ('[', ascii::multispace0),
                    combinator::cut_err(combinator::separated(
                        1..,
                        expr,
                        (ascii::multispace0, ',', ascii::multispace0),
                    )),
                    (ascii::multispace0, ']'),
                )).with_span()
                .map(|(contents, loc)| ast::Expr::ArrayLiteral {
                    contents,
                    loc: (loc.start, loc.end),
                }),
                if_.map(ast::Expr::If),
                match_.map(ast::Expr::Match),
                ident
                    .with_span()
                    .map(|(name, loc)| ast::Expr::ValueRead(name.into(), (loc.start, loc.end))),
                
                // combinator::separated(expr,combinator::delimited(ascii::multispace0,op,ascii::multispace0))
            )),
        ).parse_next(input)
}

fn expr(input: &mut Stream<'_>) -> PResult<ast::Expr> {
    combinator::trace("expr",|input:&mut Stream<'_>| {
    

    let out = simple_expr(input)?;
    let out = if let Some(args) = combinator::opt(combinator::preceded(
        ascii::multispace1,
        combinator::separated::<_, _, Vec<_>, _, _, _, _>(1.., simple_expr, ascii::multispace1),
    ))
    .parse_next(input)?
    {
        std::iter::once(out)
            .chain(args)
            .reduce(|value, arg| {
                let loc = value.get_loc();
                ast::Expr::FnCall(ast::FnCall {
                    value: value.into(),
                    arg: Some(arg.into()),
                    loc,
                })
            })
            .unwrap()
    } else {
        out
    };
    if let Some(op_ident) = combinator::opt(combinator::preceded(
        ascii::space0,
        op.with_span()
            .map(|(op, loc)| (op.to_string(), (loc.start, loc.end))),
    ))
    .parse_next(input)?
    {
        let mut output = vec![ShuntingYardOptions::Expr(out)];
        let mut op_stack = vec![op_ident];
        let op = combinator::delimited(ascii::space0, op.with_span(), ascii::space0)
            .map(|(op, loc)| (op.to_string(), (loc.start, loc.end)))
            .map(ShuntingYardOptions::Op);
        let mut opts = combinator::opt(combinator::preceded(
            ascii::space0,
            combinator::alt((simple_expr.map(ShuntingYardOptions::Expr), op)),
        ));

        //todo! move to just before inference for
        while let Some(opts) = opts.parse_next(input)? {
            match opts {
                ShuntingYardOptions::Expr(_) => {
                    output.push(opts);
                }
                ShuntingYardOptions::Op((op_ident, loc)) => {
                    let (prec, left) = PRECIDENCE
                        .iter()
                        .find_map(|(op, weight, assc)| {
                            if op == &op_ident {
                                Some((*weight, *assc))
                            } else {
                                None
                            }
                        })
                        .unwrap_or((1, false));
                    if op_stack.is_empty() {
                        op_stack.push((op_ident, loc));
                        continue;
                    }
                    while let Some(op_back) = op_stack.last() {
                        let back_prec = PRECIDENCE
                            .iter()
                            .find_map(|(op, weight, _)| {
                                if op == &op_back.0 {
                                    Some(*weight)
                                } else {
                                    None
                                }
                            })
                            .unwrap_or(1);
                        if back_prec > prec || (back_prec == prec && left) {
                            let Some(op_back) = op_stack.pop() else {
                                unreachable!()
                            };
                            output.push(ShuntingYardOptions::Op(op_back));
                        } else {
                            op_stack.push((op_ident.clone(), loc));
                            break;
                        }
                    }
                    if op_stack.last().is_none() {
                        op_stack.push((op_ident, loc));
                    }
                }
            }
        }
        output.extend(op_stack.into_iter().rev().map(ShuntingYardOptions::Op));
        let mut final_expr = Vec::new();
        for expr in output {
            match expr {
                ShuntingYardOptions::Expr(expr) => final_expr.push(expr),
                ShuntingYardOptions::Op((op, loc)) => {
                    let Some(rhs) = final_expr.pop() else {
                        unreachable!()
                    };
                    let Some(lhs) = final_expr.pop() else {
                        unreachable!()
                    };
                    final_expr.push(ast::Expr::BinaryOpCall(ast::BinaryOpCall {
                        loc,
                        operator: op,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                    }));
                }
            }
        }
        if final_expr.len() == 1 {
            Ok(final_expr.pop().unwrap())
        } else {
            Err(ErrMode::Cut(ContextError::new().add_context(
                input,
                &input.checkpoint(),
                winnow::error::StrContext::Label("Incomplete binary op expression"),
            )))
        }
    } else {
        Ok(out)
    }
    }).parse_next(input)
}

fn match_(input: &mut Stream<'_>) -> PResult<ast::Match> {
    let loc = "match"
        .span()
        .map(|loc| (loc.start, loc.end))
        .parse_next(input)?;
    let on = combinator::delimited(ascii::multispace1, expr, (ascii::multispace1, "where"))
        .parse_next(input)?;
    let _spaces = ascii::space0(input)?;
    let arms = if ascii::line_ending::<_, ErrMode<ContextError>>(input).is_ok() {
        input.state.multi_line_expr=true;
        // on another line.
        let prefix = input.state.prefix.clone();
        let checkpoint = input.checkpoint();
        let new_prefix = ascii::space0(input)?;
        input.reset(&checkpoint);
        input.state.prefix = new_prefix.into();
        let arms = combinator::repeat(1.., combinator::preceded(new_prefix, arm))
            .parse_next(input)?;
        input.state.prefix = prefix;
        arms
    } else {
        // on same line.
        todo!()
    };
    Ok(ast::Match {
        loc,
        on: on.into(),
        arms,
    })
}

fn arm(input: &mut Stream<'_>) -> PResult<ast::MatchArm> {
    combinator::trace("match arm",|input: &mut Stream<'_>| {
    
    let (loc, _, pat) = ("|".span(), ascii::space0.void(), pattern).parse_next(input)?;
    let block = combinator::preceded(
        (ascii::space0, "->", ascii::space0),
        combinator::alt((
            combinator::preceded((ascii::space0, ascii::line_ending), block),
            combinator::terminated(
                expr,
                (ascii::space0, ',', ascii::space0, ascii::line_ending),
            )
            .map(|expr| ast::Block {
                statements: Vec::new(),
                implicit_ret: Some(expr.into()),
            }),
        )),
    )
    .parse_next(input)?;
    Ok(ast::MatchArm {
        block,
        cond: pat,
        loc: (loc.start, loc.end),
    })
    }).parse_next(input)
}

fn pattern(input: &mut Stream<'_>) -> PResult<ast::Pattern> {
    combinator::trace("pattern", |input: &mut Stream<'_>| {
        let base = combinator::alt((
            // axioms
            "_".map(|_| ast::Pattern::Default),
            ascii::dec_int.map(|it: i128| ast::Pattern::ConstNumber(it.to_string())),
            ascii::float.map(|it: f64| ast::Pattern::ConstNumber(it.to_string())),
            "true".map(|_| ast::Pattern::ConstBool(true)),
            "false".map(|_| ast::Pattern::ConstBool(false)),
            string_lit.map(ast::Pattern::ConstStr),
            char_lit.map(ast::Pattern::ConstChar),
            combinator::separated_pair(
                ident.with_span(),
                ascii::space1,
                pattern
            ).map(|((ident,span),pat)| {
                let (ty, variant) = if let Some((ty,variant)) = ident.rsplit_once("::") {
                    (Some(ty.into()),variant.into())
                } else {
                    (None,ident)
                };
                ast::Pattern::EnumVariant { ty, variant, pattern: Some(pat.into()), loc: (span.start,span.end) }
            }),
            // todo struct destructureing
            (
                ident,
                combinator::delimited(
                    (ascii::space0, '{', ascii::multispace0),
                    combinator::separated(
                        0..,
                        combinator::alt((
                            combinator::separated_pair(
                                simple_ident.map(Into::into),
                                (ascii::space0, ':', ascii::space0),
                                combinator::cut_err(pattern),
                            ),
                            simple_ident.with_span().map(|(ident, span)| {
                                (
                                    ident.clone().into(),
                                    ast::Pattern::Read(ident.into(), (span.start, span.end)),
                                )
                            }),
                        )),
                        (ascii::multispace0, ',', ascii::multispace0),
                    ),
                    (ascii::multispace0, '}'),
                ),
            )
                .map(|(ident, fields)| {
                    ast::Pattern::Destructure(ast::PatternDestructure::Struct {
                        base_ty: Some(ident),
                        fields,
                    })
                }),
            ident.with_span().verify(|(it,_)| it.contains("::")).map(|(ident,span)| {
                let Some((ty,variant)) = ident.rsplit_once("::") else {unreachable!()};
                ast::Pattern::EnumVariant { ty: Some(ty.into()), variant: variant.into(), pattern:None, loc: (span.start,span.end) }
            }),
            simple_ident
                .with_span()
                .map(|(ident, loc)| ast::Pattern::Read(ident.into(), (loc.start, loc.end))),
            // more complex
            parens(combinator::separated(
                2..,
                pattern,
                (ascii::space0, ',', ascii::space0),
            ))
            .map(|contents| ast::Pattern::Destructure(ast::PatternDestructure::Tuple(contents))),
            parens(pattern),
        ))
        .parse_next(input)?;
        if let Some(rhs) = combinator::opt(combinator::preceded(
            (ascii::space0, "|", ascii::space0),
            pattern,
        ))
        .parse_next(input)?
        {
            Ok(ast::Pattern::Or(base.into(), rhs.into()))
        } else {
            Ok(base)
        }
    })
    .parse_next(input)
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
    "if",
    "then",
    "else",
];
#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use pretty_assertions::assert_eq;
    use winnow::error::InputError;

    fn from_source(src: &'static str) -> Stream<'static> {
        Stateful {
            input:Located::new(StrippedParser {
                src,
                
                state: Default::default(),
            }),
            
            state: Default::default(), 
        }
    }

    #[test]
    fn if_statements() {
        let mut src = from_source(r#"
if a then
    return 0;"#);
        ignore_blank_lines(&mut src).unwrap();
        assert_eq!(
            Ok(ast::If { 
                loc: (1,3),
                cond: ast::Expr::ValueRead("a".into(), (4,5)).into(), 
                true_branch: ast::Block {
                    statements:vec![
                        ast::Statement::Return(ast::Expr::NumericLiteral { value: "0".into() }, (15,21)),
                    ],
                    implicit_ret:None,
                }, 
                else_branch: None,
            }),
            if_statement(&mut src),
        )
    }

    #[test]
    #[ignore = "for debuging small test cases"]
    fn debugging() {
        let mut src = from_source(r#"if a then 0 else 1;"#);
        
        let x = expr(&mut src);
        println!("{x:#?}");
        // println!("{src}");
        assert!(false);
    }

    #[test] 
    fn type_parsing() {
        use types::ResolvedType;
        let mut src = from_source("int64");
        assert_eq!(type_(&mut src).unwrap(), types::INT64);
        let mut src = from_source("int32");
        assert_eq!(type_(&mut src).unwrap(), types::INT32);
        let mut src = from_source("int16");
        assert_eq!(type_(&mut src).unwrap(), types::INT16);
        let mut src = from_source("int8");
        assert_eq!(type_(&mut src).unwrap(), types::INT8);
        let mut src = from_source("str");
        assert_eq!(type_(&mut src).unwrap(), types::STR);
        let mut src = from_source("char");
        assert_eq!(type_(&mut src).unwrap(), types::CHAR);
        let mut src = from_source("float64");
        assert_eq!(type_(&mut src).unwrap(), types::FLOAT64);
        let mut src = from_source("float32");
        assert_eq!(type_(&mut src).unwrap(), types::FLOAT32);
        let mut src = from_source("()");
        assert_eq!(type_(&mut src).unwrap(), types::UNIT);
        let mut src = from_source("[int32;5]");
        assert_eq!(
            type_(&mut src).unwrap(),
            ResolvedType::Array {
                underlining: types::INT32.into(),
                size: 5
            }
        );

        let mut src = from_source("(int32,int32)");
        assert_eq!(
            type_(&mut src),
            Ok(ResolvedType::Tuple {
                underlining: vec![types::INT32, types::INT32],
                loc: (0, 0)
            })
        );

        //fuctions
        let mut src = from_source("int32->int32");
        assert_eq!(
            type_(&mut src).unwrap(),
            ResolvedType::Function {
                arg: types::INT32.into(),
                returns: types::INT32.into(),
                loc: (0, 0),
            }
        );
        let mut src = from_source("int32->int32->int32");
        assert_eq!(
            type_(&mut src).unwrap(),
            ResolvedType::Function {
                arg: types::INT32.into(),
                returns: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0),
                }
                .into(),
                loc: (0, 0),
            }
        );
        let mut src = from_source("int32->(int32->int32)");
        assert_eq!(
            type_(&mut src).unwrap(),
            ResolvedType::Function {
                arg: types::INT32.into(),
                returns: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0),
                }
                .into(),
                loc: (0, 0),
            }
        );
        let mut src = from_source("(int32->int32)->int32");
        assert_eq!(
            type_(&mut src).unwrap(),
            ResolvedType::Function {
                arg: ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (0, 0),
                }
                .into(),
                returns: types::INT32.into(),
                loc: (0, 0),
            }
        );
        let mut src = from_source("Generic<int32>");
        assert_eq!(
            type_(&mut src).unwrap(),
            ResolvedType::User{ 
                name: "Generic".into(), 
                generics: vec![types::INT32], 
                loc: (0,0),
            }
        )
    }

    #[test]
    fn expressions() {
        let mut src = from_source("1");
        assert_eq!(
            ast::Expr::NumericLiteral { value: "1".into() },
            expr(&mut src).unwrap()
        );
        let mut src = from_source("a");

        let read_a = expr(&mut src).unwrap();
        assert_eq!(ast::Expr::ValueRead("a".into(), (0, 1)), read_a);

        let mut src = from_source("(a)");
        assert_eq!(
            expr(&mut src).unwrap(),
            ast::Expr::ValueRead("a".into(), (1, 2)),
            "`(a)` should be the same `a`"
        );

        let mut src = from_source(r#""merp\"\\""#);
        assert_eq!(
            r#"merp"\"#,
            string_lit(&mut src).unwrap(),
            "string literal with escapes"
        );

        let mut src = from_source("a b c");
        assert_eq!(
            expr(&mut src),
            Ok(ast::Expr::FnCall(ast::FnCall{ 
                loc: (0,1), 
                value: ast::Expr::FnCall(ast::FnCall{ 
                    loc: (0,1), 
                    value: ast::Expr::ValueRead("a".into(), (0,1)).into(), 
                    arg: Some(ast::Expr::ValueRead("b".into(), (2,3)).into())
                }).into(), 
                arg: Some(ast::Expr::ValueRead("c".into(), (4,5)).into()) 
            }))
        );


        let mut src = from_source("a (b c)");
        assert_eq!(
            expr(&mut src),
            Ok(ast::Expr::FnCall(ast::FnCall{ 
                loc: (0,1), 
                value: ast::Expr::ValueRead("a".into(), (0,1)).into(), 
                arg: Some(ast::Expr::FnCall(ast::FnCall{ 
                    loc: (3,4),
                    value: (ast::Expr::ValueRead("b".into(), (3,4)).into()),
                    arg: Some(ast::Expr::ValueRead("c".into(), (5,6)).into()) 
                }).into()), 
            }))
        );
    }
    #[test]
    fn ops() {
        use ast::*;
        const SRC_S: &'static str = "100 + 100 * foo * ( 10 - 1 )";
        let mut parser = from_source(SRC_S);

        assert_eq!(
            Expr::BinaryOpCall(BinaryOpCall {
                loc: (4, 5),
                lhs: Expr::NumericLiteral {
                    value: "100".to_string(),
                }
                .into(),
                rhs: Expr::BinaryOpCall(BinaryOpCall {
                    loc: (16, 17),
                    lhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (10, 11),
                        lhs: Expr::NumericLiteral {
                            value: "100".to_string(),
                        }
                        .into(),
                        rhs: Expr::ValueRead("foo".to_string(), (12, 15)).into(),
                        operator: "*".to_string()
                    })
                    .into(),
                    rhs: Expr::BinaryOpCall(BinaryOpCall {
                        loc: (23, 24),
                        lhs: Expr::NumericLiteral {
                            value: "10".to_string(),
                        }
                        .into(),
                        rhs: Expr::NumericLiteral {
                            value: "1".to_string(),
                        }
                        .into(),
                        operator: "-".to_string()
                    })
                    .into(),
                    operator: "*".to_string()
                })
                .into(),
                operator: "+".to_string()
            }),
            expr(&mut parser).unwrap()
        );
        const SRC: &'static str = r#"let main _ =
    print_int32 (100 + 100);
    return 0;
"#;
        let mut parser = from_source(SRC);
        assert_eq!(
            TopLevelValue {
                loc: (4, 8),
                is_op: false,
                ident: "main".to_owned(),
                ty: None,
                args: vec![ast::ArgDeclaration::Discard {
                    ty: None,
                    loc: (9, 10)
                }],
                value: ValueType::Function(Block {
                    statements: vec![
                        Statement::Expr(Expr::FnCall(FnCall {
                            loc: (17, 28),
                            value: Expr::ValueRead("print_int32".to_owned(), (17, 28)).into(),
                            arg: Some(
                                Expr::BinaryOpCall(BinaryOpCall {
                                    loc: (34, 35),
                                    operator: "+".to_owned(),
                                    lhs: Expr::NumericLiteral {
                                        value: "100".to_owned(),
                                    }
                                    .into(),
                                    rhs: Expr::NumericLiteral {
                                        value: "100".to_owned(),
                                    }
                                    .into()
                                })
                                .into()
                            )
                        })),
                        Statement::Return(
                            Expr::NumericLiteral {
                                value: "0".to_owned(),
                            },
                            (46, 52)
                        )
                    ],
                    implicit_ret: None
                }),
                generics: None,
                abi: None,
            },
            top_level_value(&mut parser).unwrap()
        );
        //a b . 2 + c d . -
        const SRC_MEMBER_ACCESS: &'static str = "a.b + 2 - c.d";
        let mut parser = from_source(SRC_MEMBER_ACCESS);
        assert_eq!(
            ast::Expr::BinaryOpCall(BinaryOpCall {
                loc: (8, 9),
                lhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                    loc: (4, 5),
                    lhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                        loc: (1, 2),
                        lhs: ast::Expr::ValueRead("a".to_string(), (0, 1)).into(),
                        rhs: ast::Expr::ValueRead("b".to_string(), (2, 3)).into(),
                        operator: ".".to_string()
                    })
                    .into(),
                    rhs: ast::Expr::NumericLiteral {
                        value: "2".to_string(),
                    }
                    .into(),
                    operator: "+".to_string()
                })
                .into(),
                rhs: ast::Expr::BinaryOpCall(BinaryOpCall {
                    loc: (11, 12),
                    lhs: ast::Expr::ValueRead("c".to_string(), (10, 11)).into(),
                    rhs: ast::Expr::ValueRead("d".to_string(), (12, 13)).into(),
                    operator: ".".to_string()
                })
                .into(),
                operator: "-".to_string()
            }),
            expr(&mut parser).unwrap(),
        );

        let mut parser = from_source("(foo bar) && (baz quz)");
        assert_eq!(
            ast::Expr::BinaryOpCall(BinaryOpCall {
                loc: (10, 12),
                lhs: ast::Expr::FnCall(FnCall {
                    loc: (1, 4),
                    value: ast::Expr::ValueRead("foo".to_string(), (1, 4)).into(),
                    arg: Some(ast::Expr::ValueRead("bar".to_string(), (5, 8)).into())
                })
                .into(),
                rhs: ast::Expr::FnCall(FnCall {
                    loc: (14, 17),
                    value: ast::Expr::ValueRead("baz".to_string(), (14, 17)).into(),
                    arg: Some(ast::Expr::ValueRead("quz".to_string(), (18, 21)).into())
                })
                .into(),
                operator: "&&".to_string()
            }),
            expr(&mut parser).unwrap(),
            "(foo bar) && (baz quz)"
        );
    }

    #[test]
    fn block_() {
        let mut src = from_source(
            r"
    let x = 0;
    x",
        );
        ignore_blank_lines(&mut src).unwrap();
        assert_eq!(
            block(&mut src).unwrap(),
            ast::Block {
                statements: vec![ast::Statement::Declaration(ast::ValueDeclaration {
                    loc: (9, 10),
                    is_op: false,
                    target: ast::Pattern::Read("x".into(), (9, 10)),
                    args: Vec::new(),
                    ty: None,
                    value: ast::ValueType::Expr(ast::Expr::NumericLiteral { value: "0".into() }),
                    generictypes: None,
                    abi: None
                }),],
                implicit_ret: Some(ast::Expr::ValueRead("x".into(), (20, 21)).into())
            }
        )
    }

    #[test]
    fn statements() {
        const SRC: &'static str = include_str!("../../samples/test.fb");
        let mut input = from_source(SRC);
        assert_eq!(
            ast::TopLevelValue {
                loc: (4, 7),
                is_op: false,
                ident: "foo".to_owned(),
                ty: Some(types::INT32),
                args: Vec::new(),
                value: ast::ValueType::Expr(ast::Expr::NumericLiteral {
                    value: "3".to_owned(),
                }),
                generics: None,
                abi: None,
            },
            top_level_value(&mut input).unwrap(),
            "simple declaration"
        );
        ignore_blank_lines(&mut input).unwrap();
        assert_eq!(
            ast::TopLevelValue {
                loc: (26, 29),
                is_op: false,
                ident: "bar".to_owned(),
                ty: Some(types::ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (2, 20)
                }),
                args: vec![ast::ArgDeclaration::Simple {
                    ident: "quz".to_string(),
                    loc: (30, 33),
                    ty: None,
                }],
                value: ast::ValueType::Function(ast::Block {
                    statements: vec![
                        ast::Statement::Declaration(ast::ValueDeclaration {
                            loc: (62, 65),
                            is_op: false,
                            target: ast::Pattern::Read("baz".to_owned(), (62, 65)),
                            ty: Some(types::STR),
                            args: Vec::new(),
                            value: ast::ValueType::Expr(ast::Expr::StringLiteral(
                                r#"merp " yes"#.to_string()
                            )),
                            generictypes: None,
                            abi: None,
                        }),
                        ast::Statement::Return(
                            ast::Expr::NumericLiteral {
                                value: "2".to_owned(),
                            },
                            (93, 99)
                        )
                    ],
                    implicit_ret: None
                }),
                generics: None,
                abi: None,
            },
            top_level_value(&mut input).unwrap(),
            "declaration with block"
        );
        ignore_blank_lines(&mut input).unwrap();
        assert_eq!(
            ast::TopLevelValue {
                loc: (108, 112),
                is_op: true,

                ident: "^^".to_owned(),
                ty: Some(types::ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::ResolvedType::Function {
                        arg: types::INT32.into(),
                        returns: types::INT32.into(),
                        loc: (6, 32)
                    }
                    .into(),
                    loc: (6, 23)
                }),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        ident: "lhs".to_string(),
                        loc: (113, 116),
                        ty: None,
                    },
                    ast::ArgDeclaration::Simple {
                        ident: "rhs".to_string(),
                        loc: (117, 120),
                        ty: None,
                    },
                ],
                value: ast::ValueType::Function(ast::Block {
                    statements: vec![
                        ast::Statement::Expr(ast::Expr::FnCall(ast::FnCall {
                            loc: (153, 156),
                            value: ast::Expr::ValueRead("bar".to_string(), (153, 156)).into(),
                            arg: Some(ast::Expr::ValueRead("foo".to_string(), (157, 160)).into()),
                        })),
                        ast::Statement::Return(
                            ast::Expr::NumericLiteral {
                                value: "1".to_owned(),
                            },
                            (166, 172)
                        )
                    ],
                    implicit_ret: None,
                }),
                generics: None,
                abi: None,
            },
            top_level_value(&mut input).unwrap(),
            "operator declaration w/ function call",
        );
    }

    #[test]
    fn fn_chain() {
        const SRC: &'static str = r#"let main _ : int32 -> int32 = 
    put_int32 100;
    print_str "v";
    return 32;
"#;
        let mut parser = from_source(SRC);

        assert_eq!(
            ast::TopLevelValue {
                loc: (4, 8),
                is_op: false,

                ident: "main".to_owned(),
                ty: Some(types::ResolvedType::Function {
                    arg: types::INT32.into(),
                    returns: types::INT32.into(),
                    loc: (19, 21)
                }),
                args: vec![ast::ArgDeclaration::Discard {
                    ty: None,
                    loc: (9, 10)
                }],
                value: ast::ValueType::Function(ast::Block {
                    statements: vec![
                        ast::Statement::Expr(ast::Expr::FnCall(ast::FnCall {
                            loc: (35, 44),
                            value: ast::Expr::ValueRead("put_int32".to_string(), (35, 44)).into(),
                            arg: Some(
                                ast::Expr::NumericLiteral {
                                    value: "100".to_owned(),
                                }
                                .into()
                            )
                        })),
                        ast::Statement::Expr(ast::Expr::FnCall(ast::FnCall {
                            loc: (54, 63),
                            value: ast::Expr::ValueRead("print_str".to_string(), (54, 63)).into(),
                            arg: Some(ast::Expr::StringLiteral("v".to_string()).into())
                        })),
                        ast::Statement::Return(
                            ast::Expr::NumericLiteral {
                                value: "32".to_string(),
                            },
                            (73, 79)
                        )
                    ],
                    implicit_ret: None
                }),
                generics: None,
                abi: None,
            },
            top_level_value(&mut parser).unwrap(),
        );
    }

    #[test]
    fn generics() {
        let mut parser = from_source("for<T> let test a : T -> T = a;");
        assert_eq!(
            ast::TopLevelValue {
                loc: (11, 15),
                is_op: false,
                ident: "test".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    loc: (16, 17),
                    ident: "a".to_string(),
                    ty: None,
                }],
                ty: Some(types::ResolvedType::Function {
                    arg: types::ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (20, 21)
                    }
                    .into(),
                    returns: types::ResolvedType::Generic {
                        name: "T".to_string(),
                        loc: (25, 26)
                    }
                    .into(),
                    loc: (22, 24)
                }),
                value: ast::ValueType::Expr(ast::Expr::ValueRead("a".to_string(), (29, 30))),
                generics: Some(ast::GenericsDecl {
                    for_loc: (0, 3),
                    decls: [((4, 5), "T".to_string())].into_iter().collect(),
                }),
                abi: None,
            },
            top_level_value(&mut parser).unwrap(),
        )
    }

    #[test]
    fn struct_decl() {
        const SRC: &'static str = r#"type Foo = {
    a : int32
}
for<T,U> type Tuple = {
    first : T,
    second : U
}"#;
        let mut parser = from_source(SRC);
        assert_eq!(
            ast::TopLevelDeclaration::TypeDefinition(ast::TypeDefinition::Struct(
                ast::StructDefinition {
                    ident: "Foo".to_string(),
                    generics: None,
                    values: vec![ast::FieldDecl {
                        name: "a".to_string(),
                        ty: types::INT32,
                        loc: (17, 18),
                    }],
                    loc: (5, 8)
                },
            )),
            top_level_decl(&mut parser).unwrap(),
            "basic"
        );
        ignore_blank_lines(&mut parser).unwrap();
        assert_eq!(
            ast::TopLevelDeclaration::TypeDefinition(ast::TypeDefinition::Struct(
                ast::StructDefinition {
                    ident: "Tuple".to_string(),
                    generics: Some(ast::GenericsDecl {
                        for_loc: (29, 32),
                        decls: vec![((33, 34), "T".to_string()), ((35, 36), "U".to_string())],
                    }),
                    values: vec![
                        ast::FieldDecl {
                            name: "first".to_string(),
                            ty: types::ResolvedType::Generic {
                                name: "T".to_string(),
                                loc: (65, 66),
                            },
                            loc: (57, 62)
                        },
                        ast::FieldDecl {
                            name: "second".to_string(),
                            ty: types::ResolvedType::Generic {
                                name: "U".to_string(),
                                loc: (81, 82)
                            },
                            loc: (72, 78)
                        },
                    ],
                    loc: (43, 48)
                }
            )),
            top_level_decl(&mut parser).unwrap(),
            "generic"
        )
    }

    #[test]
    fn generic_use_types() {
        const SRC: &'static str = r"let foo a b : Bar<int32> -> Baz<int32,float64> -> int32 =
    return 0; 
";
        let mut parser = from_source(SRC);
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (4, 7),
                is_op: false,
                ident: "foo".to_owned(),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        loc: (8, 9),
                        ident: "a".to_string(),
                        ty: None,
                    },
                    ast::ArgDeclaration::Simple {
                        loc: (10, 11),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: Some(types::ResolvedType::Function {
                    arg: types::ResolvedType::User {
                        name: "Bar".to_string(),
                        generics: vec![types::INT32],
                        loc: (14, 25)
                    }
                    .into(),
                    returns: types::ResolvedType::Function {
                        arg: types::ResolvedType::User {
                            name: "Baz".to_string(),
                            generics: vec![types::INT32, types::FLOAT64],
                            loc: (28, 47)
                        }
                        .into(),
                        returns: types::INT32.into(),
                        loc: (47, 49)
                    }
                    .into(),
                    loc: (25, 27)
                }),
                value: ast::ValueType::Function(ast::Block {
                    statements: vec![ast::Statement::Return(
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        },
                        (62, 68)
                    )],
                    implicit_ret: None
                }),
                generics: None,
                abi: None,
            }),
            top_level_decl(&mut parser).unwrap()
        );
    }

    #[test]
    fn struct_construction() {
        const SRC: &'static str = "Foo { a : 0 }";
        let mut parser = from_source(SRC);
        assert_eq!(
            ast::Expr::StructConstruction(ast::StructConstruction {
                loc: (0, 3),
                fields: HashMap::from([(
                    "a".to_string(),
                    (
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        },
                        (0, 0)
                    )
                )]),
                generics: Vec::new(),
                ident: "Foo".to_string()
            }),
            expr(&mut parser).unwrap()
        );
        let mut parser = from_source("Generic<int32>{ a:0 }");
        assert_eq!(
            ast::Expr::StructConstruction(ast::StructConstruction {
                loc: (0, 14),
                fields: HashMap::from([(
                    "a".to_string(),
                    (
                        ast::Expr::NumericLiteral {
                            value: "0".to_string(),
                        },
                        (0, 0)
                    )
                )]),
                generics: vec![types::INT32],
                ident: "Generic".to_string()
            }),
            expr(&mut parser).unwrap()
        )
    }

    #[test]
    fn abi() {
        const SRC: &'static str = r#"extern "C" let putchar : int32 -> int32;
extern "C" let ex (a:int32) b = a + b;
"#;
        let mut parser = from_source(SRC);
        let putchar = top_level_decl(&mut parser).unwrap();

        ignore_blank_lines(&mut parser).unwrap();
        let ex = top_level_decl(&mut parser).unwrap();
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (15, 22),
                is_op: false,
                ident: "putchar".to_owned(),
                args: Vec::new(),
                ty: Some(types::INT32.fn_ty(&types::INT32)),
                value: ast::ValueType::External,
                generics: None,
                abi: Some(ast::Abi {
                    loc: (7, 10),
                    identifier: "C".to_string(),
                }),
            }),
            putchar,
            "input function"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (56, 58),
                is_op: false,
                ident: "ex".to_owned(),
                args: vec![
                    ast::ArgDeclaration::Simple {
                        loc: (60, 61),
                        ident: "a".to_string(),
                        ty: Some(types::INT32),
                    },
                    ast::ArgDeclaration::Simple {
                        loc: (69, 70),
                        ident: "b".to_string(),
                        ty: None,
                    },
                ],
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::BinaryOpCall(ast::BinaryOpCall {
                    loc: (75, 76),
                    lhs: ast::Expr::ValueRead("a".to_string(), (73, 74)).into(),
                    rhs: ast::Expr::ValueRead("b".to_string(), (77, 78)).into(),
                    operator: "+".to_string()
                })),
                generics: None,
                abi: Some(ast::Abi {
                    loc: (48, 51),
                    identifier: "C".to_string(),
                }),
            }),
            ex,
            "output function."
        )
    }

    #[test]
    fn tuples() {
        const SRC: &'static str = "let ty _ : (int32,int32)->int32 = 0;

let cons a : int32 -> (int32,int32) = (a,0);
";
        let mut src = from_source(SRC);
        let module = top_level_block(&mut src).unwrap();
        let [ty, cons] = &module[..] else {
            unreachable!()
        };
        assert_eq!(
            &ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (4, 6),
                is_op: false,
                ident: "ty".to_owned(),
                args: vec![ast::ArgDeclaration::Discard {
                    ty: None,
                    loc: (7, 8)
                }],
                ty: Some(
                    types::ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32],
                        loc: (0, 0)
                    }
                    .fn_ty(&types::INT32)
                ),
                value: ast::ValueType::Expr(ast::Expr::NumericLiteral {
                    value: "0".to_string()
                }),
                generics: None,
                abi: None,
            }),
            ty
        );

        assert_eq!(
            &ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (42, 46),
                is_op: false,
                ident: "cons".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    loc: (47, 48),
                    ident: "a".to_string(),
                    ty: None
                }],
                ty: Some(types::INT32.fn_ty(&types::ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32],
                    loc: (0, 0)
                })),
                value: ast::ValueType::Expr(ast::Expr::TupleLiteral {
                    contents: vec![
                        ast::Expr::ValueRead("a".to_string(), (77, 78)),
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        }
                    ],
                    loc: (76, 81)
                }),
                generics: None,
                abi: None
            }),
            cons
        );
    }

    #[test]
    fn match_patterns() {
        let mut src = from_source("a");
        let pattern_ = pattern(&mut src).unwrap();
        assert_eq!(ast::Pattern::Read("a".to_string(), (0, 1)), pattern_, "a");
        let mut src = from_source("_");
        let pattern_ = pattern(&mut src).unwrap();
        assert_eq!(ast::Pattern::Default, pattern_, "_");
        let mut src = from_source("(a,b)");
        assert_eq!(
            ast::Pattern::Destructure(ast::PatternDestructure::Tuple(vec![
                ast::Pattern::Read("a".to_string(), (1, 2)),
                ast::Pattern::Read("b".to_string(), (3, 4)),
            ])),
            pattern(&mut src).unwrap(),
            "destruct tuple"
        );
        let mut src = from_source("0 | 1");
        assert_eq!(
            ast::Pattern::Or(
                ast::Pattern::ConstNumber("0".to_string()).into(),
                ast::Pattern::ConstNumber("1".to_string()).into(),
            ),
            pattern(&mut src).unwrap(),
            "or (0 or 1)"
        );
        let mut src = from_source("0 | 1 | 2");
        assert_eq!(
            ast::Pattern::Or(
                ast::Pattern::ConstNumber("0".to_string()).into(),
                ast::Pattern::Or(
                    ast::Pattern::ConstNumber("1".to_string()).into(),
                    ast::Pattern::ConstNumber("2".to_string()).into(),
                )
                .into()
            ),
            pattern(&mut src).unwrap(),
            "or (0 or 1 or 2)"
        );
        let mut src = from_source("(0 | 1,b)");
        assert_eq!(
            ast::Pattern::Destructure(ast::PatternDestructure::Tuple(vec![
                ast::Pattern::Or(
                    ast::Pattern::ConstNumber("0".to_string()).into(),
                    ast::Pattern::ConstNumber("1".to_string()).into(),
                ),
                ast::Pattern::Read("b".to_string(), (7, 8)),
            ])),
            pattern(&mut src).unwrap(),
            "destruct tuple"
        );
    }

    #[test]
    fn arg_types() {
        const SRC: &'static str = "
let simple a = ();
let decon_arg (a,b) = ();
let discard _ = ();
let unit () = ();
let annotated_arg_tuple ((x,y):(int32,int32)) = ();
";

        let mut src = from_source(SRC);
        let simple = top_level_decl(&mut src).unwrap();
        let decon = top_level_decl(&mut src).unwrap();
        let discard = top_level_decl(&mut src).unwrap();
        let unit = top_level_decl(&mut src).unwrap();
        let annotated_decon = top_level_decl(&mut src).unwrap();
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (5, 11),
                is_op: false,
                ident: "simple".to_owned(),
                args: vec![ast::ArgDeclaration::Simple {
                    loc: (12, 13),
                    ident: "a".to_string(),
                    ty: None,
                },],
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            simple,
            "let simple a = ();"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (24, 33),
                is_op: false,
                ident: "decon_arg".to_owned(),
                args: vec![ast::ArgDeclaration::DestructureTuple(
                    vec![
                        ast::ArgDeclaration::Simple {
                            loc: (35, 36),
                            ident: "a".to_string(),
                            ty: None,
                        },
                        ast::ArgDeclaration::Simple {
                            loc: (37, 38),
                            ident: "b".to_string(),
                            ty: None,
                        },
                    ],
                    None,
                    (34, 39)
                ),],
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            decon,
            "let decon_arg (a,b) = ()"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (50, 57),
                is_op: false,
                ident: "discard".to_owned(),
                args: vec![ast::ArgDeclaration::Discard {
                    loc: (58, 59),
                    ty: None,
                },],
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            discard,
            "let discard _ = ();"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (70, 74),
                is_op: false,
                ident: "unit".to_owned(),
                args: vec![ast::ArgDeclaration::Unit {
                    loc: (75, 77),
                    ty: None,
                },],
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            unit,
            "let unit () = ();"
        );
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (88, 107),
                is_op: false,
                ident: "annotated_arg_tuple".to_owned(),
                args: vec![ast::ArgDeclaration::DestructureTuple(
                    vec![
                        ast::ArgDeclaration::Simple {
                            loc: (110, 111),
                            ident: "x".to_string(),
                            ty: None,
                        },
                        ast::ArgDeclaration::Simple {
                            loc: (112, 113),
                            ident: "y".to_string(),
                            ty: None,
                        },
                    ],
                    Some(types::ResolvedType::Tuple {
                        underlining: vec![types::INT32, types::INT32,],
                        loc: (115, 128)
                    }),
                    (109, 114)
                ),],
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::UnitLiteral),
                generics: None,
                abi: None,
            }),
            annotated_decon,
            "let annotated_arg_tuple ((x,y):(int32,int32)) = ();"
        );
    }

    #[test]
    fn destructuring_statement() {
        let mut parser = from_source(
r#"let (x,y) = v;
let ((x,y),z) = v;
let (x,y,z) = v;
let (x,y):(int32,int32) = v;
"#,
        );
        assert_eq!(
            ast::Statement::Declaration(ast::ValueDeclaration {
                loc: (4,9),
                is_op: false,
                target: ast::Pattern::Destructure(ast::PatternDestructure::Tuple(vec![
                    ast::Pattern::Read("x".to_string(), (5, 6)),
                    ast::Pattern::Read("y".to_string(), (7, 8)),
                ])),
                args: Vec::new(),
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (12, 13))),
                generictypes: None,
                abi: None,
            }),
            statement(&mut parser).unwrap()
        );
        ascii::line_ending::<_, ContextError>(&mut parser);
        assert_eq!(
            ast::Statement::Declaration(ast::ValueDeclaration {
                loc: (19, 28),
                is_op: false,
                target: ast::Pattern::Destructure(ast::PatternDestructure::Tuple(vec![
                    ast::Pattern::Destructure(ast::PatternDestructure::Tuple(vec![
                        ast::Pattern::Read("x".to_string(), (21, 22)),
                        ast::Pattern::Read("y".to_string(), (23, 24)),
                    ])),
                    ast::Pattern::Read("z".to_string(), (26, 27))
                ])),
                args: Vec::new(),
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (31, 32))),
                generictypes: None,
                abi: None,
            }),
            statement(&mut parser).unwrap()
        );

        ascii::line_ending::<_, ContextError>(&mut parser);
        assert_eq!(
            ast::Statement::Declaration(ast::ValueDeclaration {
                loc: (38, 45),
                is_op: false,
                target: ast::Pattern::Destructure(ast::PatternDestructure::Tuple(vec![
                    ast::Pattern::Read("x".to_string(), (39, 40)),
                    ast::Pattern::Read("y".to_string(), (41, 42)),
                    ast::Pattern::Read("z".to_string(), (43, 44)),
                ])),
                args: Vec::new(),
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (48, 49))),
                generictypes: None,
                abi: None,
            }),
            statement(&mut parser).unwrap()
        );
        ascii::line_ending::<_, ContextError>(&mut parser);
        assert_eq!(
            ast::Statement::Declaration(ast::ValueDeclaration {
                loc: (55, 60),
                is_op: false,
                target: ast::Pattern::Destructure(ast::PatternDestructure::Tuple(vec![
                    ast::Pattern::Read("x".to_string(), (56, 57)),
                    ast::Pattern::Read("y".to_string(), (58, 59)),
                ])),
                args: Vec::new(),
                ty: Some(types::ResolvedType::Tuple {
                    underlining: vec![types::INT32, types::INT32],
                    loc: (4, 10),
                }),
                value: ast::ValueType::Expr(ast::Expr::ValueRead("v".to_string(), (77, 78))),
                generictypes: None,
                abi: None,
            }),
            statement(&mut parser).unwrap()
        );
    }

    #[test]
    fn arrays() {
        const SRC: &'static str = r#"let arr = [0,0,0,0];
"#;
        let mut arr = from_source(SRC);
        let arr = top_level_decl(&mut arr).unwrap();
        assert_eq!(
            ast::TopLevelDeclaration::Value(ast::TopLevelValue {
                loc: (4, 7),
                is_op: false,
                ident: "arr".to_owned(),
                args: Vec::new(),
                ty: None,
                value: ast::ValueType::Expr(ast::Expr::ArrayLiteral {
                    contents: vec![
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                        ast::Expr::NumericLiteral {
                            value: "0".to_string()
                        },
                    ],
                    loc: (10, 19)
                }),
                generics: None,
                abi: None,
            }),
            arr,
            "arrays"
        )
    }
    #[test]
    fn enums() {
        const SRC: &'static str = "
enum Basic = | None | AnInt int32 | Struct { a: int32 }
for<T> enum Option = | Some T | None
for<T,E> enum Result = | Ok T | Err E
";
        let mut parser = from_source(SRC);
        ignore_blank_lines(&mut parser);
        let basic = top_level_decl(&mut parser).unwrap();
        assert_eq!(
            ast::TopLevelDeclaration::TypeDefinition(ast::TypeDefinition::Enum(
                ast::EnumDeclaration {
                    ident: "Basic".to_string(),
                    generics: None,
                    values: vec![
                        ast::EnumVariant::Unit {
                            ident: "None".to_string(),
                            loc: (16, 20)
                        },
                        ast::EnumVariant::Tuple {
                            ident: "AnInt".to_string(),
                            loc: (23, 28),
                            ty: types::INT32
                        },
                        ast::EnumVariant::Struct {
                            ident: "Struct".to_string(),
                            fields: vec![ast::FieldDecl {
                                name: "a".to_string(),
                                ty: types::INT32,
                                loc: (46, 47)
                            }],
                            loc: (37, 43)
                        }
                    ],
                    loc: (6, 11),
                }
            )),
            basic,
            "basic: enum Basic = | None | AnInt int32 | Struct {{ a: int32 }}"
        );
        ignore_blank_lines(&mut parser);
        let option = top_level_decl(&mut parser).unwrap();
        assert_eq!(
            ast::TopLevelDeclaration::TypeDefinition(ast::TypeDefinition::Enum(
                ast::EnumDeclaration {
                    ident: "Option".to_string(),
                    generics: Some(ast::GenericsDecl {
                        for_loc: (57, 60),
                        decls: vec![((61, 62), "T".to_string())]
                    }),
                    values: vec![
                        ast::EnumVariant::Tuple {
                            ident: "Some".to_string(),
                            ty: types::ResolvedType::Generic {
                                name: "T".to_string(),
                                loc: (85, 86)
                            },
                            loc: (80, 84)
                        },
                        ast::EnumVariant::Unit {
                            ident: "None".to_string(),
                            loc: (89, 93)
                        }
                    ],
                    loc: (69, 75)
                }
            )),
            option,
            "option: for<T> enum Option = | Some T | None"
        );

        ignore_blank_lines(&mut parser);
        let result = top_level_decl(&mut parser).unwrap();
        assert_eq!(
            ast::TopLevelDeclaration::TypeDefinition(ast::TypeDefinition::Enum(
                ast::EnumDeclaration {
                    ident: "Result".to_string(),
                    generics: Some(ast::GenericsDecl {
                        for_loc: (94, 97),
                        decls: vec![((98, 99), "T".to_string()), ((100, 101), "E".to_string()),]
                    }),
                    values: vec![
                        ast::EnumVariant::Tuple {
                            ident: "Ok".to_string(),
                            ty: types::ResolvedType::Generic {
                                name: "T".to_string(),
                                loc: (122, 123)
                            },
                            loc: (119, 121)
                        },
                        ast::EnumVariant::Tuple {
                            ident: "Err".to_string(),
                            ty: types::ResolvedType::Generic {
                                name: "E".to_string(),
                                loc: (130, 131)
                            },
                            loc: (126, 129)
                        },
                    ],
                    loc: (108, 114)
                }
            )),
            result,
            "result: for<T,E> enum Result = | Ok T | Err E"
        );
        ignore_blank_lines(&mut parser);
        assert!(combinator::eof::<_, ContextError>(&mut parser).is_ok());
    }

    #[test]
    fn enum_patterns() {
        const SRC: &'static str = r#"
match a where
| Enum::Complex { a: 0, b } -> b,
| Enum::Simple (0 | 1) -> 0,
| Simple a -> a,
"#;
        // | Enum::Complex c -> c.a // TODO! struct access.
        let mut src = from_source(SRC);
        ignore_blank_lines(&mut src);
        let ast = match_(&mut src).unwrap();
        assert_eq!(
            ast::Match {
                loc: (1, 6),
                on: ast::Expr::ValueRead("a".to_string(), (7, 8)).into(),
                arms: vec![
                    ast::MatchArm {
                        block: ast::Block {
                            statements: Vec::new(),
                            implicit_ret: Some(
                                ast::Expr::ValueRead("b".to_string(), (46, 47)).into()
                            ),
                        },
                        cond: ast::Pattern::Destructure(ast::PatternDestructure::Struct {
                            base_ty: Some("Enum::Complex".into()),
                            fields: [
                                ("a".to_string(), ast::Pattern::ConstNumber("0".to_string())),
                                (
                                    "b".to_string(),
                                    ast::Pattern::Read("b".to_string(), (39, 40))
                                ),
                            ]
                            .into(),
                        }),
                        loc: (15, 16)
                    },
                    ast::MatchArm {
                        block: ast::Block {
                            statements: Vec::new(),
                            implicit_ret: Some(
                                ast::Expr::NumericLiteral {
                                    value: "0".to_string()
                                }
                                .into()
                            )
                        },
                        cond: ast::Pattern::EnumVariant {
                            ty: Some("Enum".to_string()),
                            variant: "Simple".to_string(),
                            pattern: Some(
                                ast::Pattern::Or(
                                    ast::Pattern::ConstNumber("0".to_string()).into(),
                                    ast::Pattern::ConstNumber("1".to_string()).into(),
                                )
                                .into()
                            ),
                            loc: (51, 63)
                        },
                        loc: (49, 50),
                    },
                    ast::MatchArm {
                        block: ast::Block {
                            statements: Vec::new(),
                            implicit_ret: Some(
                                ast::Expr::ValueRead("a".to_string(), (92, 93)).into()
                            ),
                        },
                        cond: ast::Pattern::EnumVariant {
                            ty: None,
                            variant: "Simple".to_string(),
                            pattern: Some(ast::Pattern::Read("a".to_string(), (87, 88)).into()),
                            loc: (80, 86),
                        },
                        loc: (78, 79),
                    },
                ],
            },
            ast,
            "match"
        );
    }

    #[test]
    fn comments() {
        let mut src = from_source(r"#/ unnested #/ nested 
        /# unnested /#");
        assert!(ascii::space1::<_,ContextError>(&mut src).is_ok());
        assert!(src.is_empty());
        let mut src = from_source("a #/ comment /# b");
        assert_eq!(
            expr(&mut src),
            Ok(ast::Expr::FnCall(ast::FnCall{ 
                loc:(0,1), 
                value:ast::Expr::ValueRead("a".into(),(0,1)).into(), 
                arg: Some(ast::Expr::ValueRead("b".into(), (16,17)).into())
            }))
        );
        assert!(src.is_empty());
        let mut src = from_source("a b #! as;ldkfj;aslkdf 
()");
        assert_eq!(
            expr(&mut src),
            Ok(ast::Expr::FnCall(ast::FnCall{ 
                loc:(0,1), 
                value: ast::Expr::FnCall(ast::FnCall { 
                    value:ast::Expr::ValueRead("a".into(),(0,1)).into(), 
                    arg: Some(ast::Expr::ValueRead("b".into(), (2,3)).into()),
                    loc:(0,1),
                }).into(),
                arg:Some(ast::Expr::UnitLiteral.into()),
            }))
        );
        assert!(src.is_empty());
    }
}

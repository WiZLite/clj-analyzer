use std::{array::IntoIter, path::Iter};

use nom::{
    branch::{alt, permutation},
    bytes::complete::{tag, take_till, take_till1, take_until},
    character::complete::{
        alpha1, alphanumeric1, char, digit0, digit1, line_ending, multispace0, multispace1, one_of,
    },
    combinator::{eof, map_res, not, opt},
    error::ParseError,
    multi::{self, count, many0, many1, separated_list0},
    sequence::delimited,
    AsChar, IResult, InputTakeAtPosition, Offset,
};
use nom_locate::{position, LocatedSpan};

use crate::Span;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum NumberLiteralValue {
    Integer(i64),
    Real(f64),
}

#[derive(PartialEq, Debug)]
pub struct AST<'a> {
    pub pos: Span<'a>,
    pub body: ASTBody<'a>,
}

impl<'a> AST<'a> {
    pub fn fragment(&self) -> &'a str {
        self.pos.fragment()
    }
    pub fn column(&self) -> usize {
        self.pos.get_column()
    }
    pub fn line(&self) -> u32 {
        self.pos.location_line()
    }
}

#[derive(PartialEq, Debug)]
pub enum ASTBody<'a> {
    Symbol { ns: Option<&'a str>, name: &'a str },
    Keyword { ns: Option<&'a str>, name: &'a str },
    NumberLiteral(NumberLiteralValue),
    StringLiteral(&'a str),
    Nil,
    List(Vec<AST<'a>>),
    Vector(Vec<AST<'a>>),
    Set(Vec<AST<'a>>),
    Map(Vec<(AST<'a>, AST<'a>)>),
    AnonymousFn(Vec<AST<'a>>),
    Quote(Box<AST<'a>>),
    SyntaxQuote(Box<AST<'a>>),
    UnQuote(Box<AST<'a>>),
    MetaData(Vec<AST<'a>>),
    EOF, // Not necessary but maybe useful for analysis
    Root(Vec<AST<'a>>),
}

fn get_span<'a>(input: &'a str, from: Span, to: Span) -> Span<'a> {
    let from_offset = from.location_offset();
    let to_offset = to.location_offset();
    let fragment = &input[0..(to_offset - from_offset)];
    unsafe { Span::new_from_raw_offset(from.location_offset(), from.location_line(), fragment, ()) }
}

fn parse_number(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, sign) = opt(one_of("+-"))(s)?;
    let (s, int) = digit1(s)?;
    let (s, dot) = opt(char('.'))(s)?;
    if let Some(_) = dot {
        let (s, frac) = digit1(s)?;
        let sign: f64 = if sign.unwrap_or('+') == '+' {
            1.0
        } else {
            -1.0
        };
        let integer: f64 = int.parse::<f64>().unwrap();
        let fraction: f64 = frac.parse::<f64>().unwrap() / 10f64.powf(frac.len() as f64);
        let (s, to) = position(s)?;
        Ok((
            s,
            AST {
                pos: get_span(&input, from, to),
                body: ASTBody::NumberLiteral(NumberLiteralValue::Real(
                    (integer + fraction).copysign(sign),
                )),
            },
        ))
    } else {
        let sign: i64 = if sign.unwrap_or('+') == '+' { 1 } else { -1 };
        let integer: i64 = int.parse::<i64>().unwrap();
        let (s, to) = position(s)?;
        Ok((
            s,
            AST {
                pos: get_span(&input, from, to),
                body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(sign * integer)),
            },
        ))
    }
}

#[derive(Debug)]
struct StringLiteralNode<'a> {
    pub position: Span<'a>,
    pub text: &'a str,
}

fn comma(s: Span) -> IResult<Span, ()> {
    let (s, _) = char(',')(s)?;
    Ok((s, ()))
}

fn multispace(s: Span) -> IResult<Span, ()> {
    let (s, _) = multispace1(s)?;
    Ok((s, ()))
}

fn comment(s: Span) -> IResult<Span, ()> {
    let (s, _) = char(';')(s)?;
    let (s, _) = take_till(|c: char| c == '\r' || c == '\n')(s)?;
    let (s, _) = alt((eof, line_ending))(s)?;
    Ok((s, ()))
}

fn skip0(s: Span) -> IResult<Span, ()> {
    let (s, _) = many0(alt((comma, multispace, comment)))(s)?;
    Ok((s, ()))
}

fn skip1(s: Span) -> IResult<Span, ()> {
    let (s, _) = many1(alt((comma, multispace, comment)))(s)?;
    Ok((s, ()))
}

const CHARS_ALLOWED_IN_KEYWORD_AND_SYMBOL: [char; 7] = ['*', '+', '!', '-', '_', '?', '.'];

fn parse_word(s: Span) -> IResult<Span, Span> {
    take_till1(|x: char| !x.is_alphanumeric() && !CHARS_ALLOWED_IN_KEYWORD_AND_SYMBOL.contains(&x))(
        s,
    )
}

fn parse_symbol(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    not(digit1)(s)?;
    let (s, first) = parse_word(s)?;
    let (s, after_slash) = opt(permutation((char('/'), parse_word)))(s)?;
    let (s, to) = position(s)?;
    if let Some((_, second)) = after_slash {
        Ok((
            s,
            AST {
                pos: get_span(&input, from, to),
                body: ASTBody::Symbol {
                    ns: Some(first.fragment()),
                    name: second.fragment(),
                },
            },
        ))
    } else {
        Ok((
            s,
            AST {
                pos: get_span(&input, from, to),
                body: ASTBody::Symbol {
                    ns: None,
                    name: first.fragment(),
                },
            },
        ))
    }
}

fn parse_keyword(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = char(':')(s)?;
    let (s, second_colon) = opt(char(':'))(s)?;
    if second_colon.is_some() {
        let (s, first) = parse_word(s)?;
        let (s, after_slash) = opt(permutation((char('/'), parse_word)))(s)?;
        let (s, to) = position(s)?;
        if let Some((_, second)) = after_slash {
            Ok((
                s,
                AST {
                    pos: get_span(&input, from, to),
                    body: ASTBody::Keyword {
                        ns: Some(first.fragment()),
                        name: second.fragment(),
                    },
                },
            ))
        } else {
            Ok((
                s,
                AST {
                    pos: get_span(&input, from, to),
                    body: ASTBody::Keyword {
                        ns: Some(""),
                        name: first.fragment(),
                    },
                },
            ))
        }
    } else {
        let (s, name) = parse_word(s)?;
        let (s, to) = position(s)?;
        Ok((
            s,
            AST {
                pos: get_span(&input, from, to),
                body: ASTBody::Keyword {
                    ns: None,
                    name: name.fragment(),
                },
            },
        ))
    }
}

fn parse_string(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = char('"')(s)?;
    let (s, text) = alpha1(s)?;
    let (s, _) = char('"')(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::StringLiteral(text.fragment()),
        },
    ))
}

pub fn parse_forms(input: Span) -> IResult<Span, Vec<AST>> {
    let (s, forms) = separated_list0(skip1, parse_form)(input)?;
    Ok((s, forms))
}

pub fn parse_source(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = skip0(s)?;
    let (s, mut forms) = parse_forms(s)?;
    let (s, _) = skip0(s)?;
    unsafe {
        let offset = input.len();
        let line = input.lines().count() as u32;
        let eof = forms.push(AST {
            pos: Span::new_from_raw_offset(offset, line, "", ()),
            body: ASTBody::EOF,
        });
        let (s, to) = position(s)?;
        Ok((
            s,
            AST {
                pos: get_span(&input, from, to),
                body: ASTBody::Root(forms),
            },
        ))
    }
}

fn parse_list(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, forms) = delimited(char('('), delimited(skip0, parse_forms, skip0), char(')'))(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::List(forms),
        },
    ))
}

fn parse_vector(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, forms) = delimited(char('['), delimited(skip0, parse_forms, skip0), char(']'))(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::Vector(forms),
        },
    ))
}

fn parse_set(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = char('#')(s)?;
    let (s, forms) = delimited(char('{'), delimited(skip0, parse_forms, skip0), char('}'))(s)?;
    let (s, to) = position(input)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::Set(forms),
        },
    ))
}

fn parse_anonymous_fn(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, forms) = delimited(tag("#("), delimited(skip0, parse_forms, skip0), char(')'))(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::AnonymousFn(forms),
        },
    ))
}

fn parse_map(input: Span) -> IResult<Span, AST> {
    fn parse_kv(s: Span) -> IResult<Span, (AST, AST)> {
        let (s, (k, _, v)) = permutation((parse_form, skip1, parse_form))(s)?;
        Ok((s, (k, v)))
    }
    let (s, from) = position(input)?;

    let (s, kvs) = delimited(
        char('{'),
        delimited(skip0, separated_list0(skip1, parse_kv), skip0),
        char('}'),
    )(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::Map(kvs),
        },
    ))
}

fn parse_quote(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = char('\'')(s)?;
    let (s, form) = parse_form(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::Quote(Box::new(form)),
        },
    ))
}

fn parse_unquote(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = char('~')(s)?;
    let (s, form) = parse_form(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::UnQuote(Box::new(form)),
        },
    ))
}

fn parse_syntax_quote(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = char('`')(s)?;
    let (s, form) = parse_form(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::SyntaxQuote(Box::new(form)),
        },
    ))
}

fn parse_nil(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, _) = tag("nil")(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::Nil,
        },
    ))
}

fn parse_metadata(input: Span) -> IResult<Span, AST> {
    let (s, from) = position(input)?;
    let (s, data) = many1(delimited(
        char('^'),
        alt((parse_string, parse_keyword, parse_symbol, parse_map)),
        skip1,
    ))(s)?;
    let (s, to) = position(s)?;
    Ok((
        s,
        AST {
            pos: get_span(&input, from, to),
            body: ASTBody::MetaData(data),
        },
    ))
}

fn parse_form(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    alt((
        parse_list,
        parse_nil,
        parse_symbol,
        parse_keyword,
        parse_number,
        parse_string,
        parse_vector,
        parse_set,
        parse_anonymous_fn,
        parse_map,
    ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::borrow::Borrow;

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse_string("\"hello\"".into()).unwrap().1.body,
            ASTBody::StringLiteral("hello")
        )
    }
    #[test]
    fn test_parse_number() {
        unsafe {
            assert_eq!(
                parse_number("10".into()).unwrap().1,
                AST {
                    pos: Span::new_from_raw_offset(0, 1, "10", ()),
                    body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(10))
                }
            );
        }
        assert_eq!(
            parse_number("3.14".into()).unwrap().1.body,
            ASTBody::NumberLiteral(NumberLiteralValue::Real(3.14))
        );
    }
    #[test]
    fn parse_keyword_test() {
        unsafe {
            let (rest, matched) = parse_keyword(":key-one?  aaa".into()).unwrap();
            assert_eq!(
                matched,
                AST {
                    pos: Span::new_from_raw_offset(0, 1, ":key-one?", ()),
                    body: ASTBody::Keyword {
                        ns: None,
                        name: "key-one?"
                    }
                }
            );
            assert_eq!(rest.fragment(), &"  aaa");
            let (rest, matched) = parse_keyword(":key_.2!\n".into()).unwrap();
            assert_eq!(
                matched,
                AST {
                    pos: Span::new_from_raw_offset(0, 1, ":key_.2!", ()),
                    body: ASTBody::Keyword {
                        ns: None,
                        name: "key_.2!"
                    }
                }
            );
            assert_eq!(rest.fragment(), &"\n");

            assert_eq!(
                parse_keyword("::hoge".into()).unwrap().1.body,
                ASTBody::Keyword {
                    ns: Some(""),
                    name: "hoge"
                }
            );
            assert_eq!(
                parse_keyword("::r/get".into()).unwrap().1.body,
                ASTBody::Keyword {
                    ns: Some("r"),
                    name: "get"
                }
            );
        }
    }
    #[test]
    fn parse_symbol_test() {
        unsafe {
            let result = parse_symbol("1symbol".into());
            assert!(result.is_err());

            let (rest, matched) = parse_symbol("symbol  aaa".into()).unwrap();
            assert_eq!(
                matched,
                AST {
                    pos: Span::new_from_raw_offset(0, 1, "symbol", ()),
                    body: ASTBody::Symbol {
                        ns: None,
                        name: "symbol"
                    }
                }
            );
            assert_eq!(
                parse_symbol("ns/name".into()).unwrap().1.body,
                ASTBody::Symbol {
                    ns: Some("ns"),
                    name: "name"
                }
            );
        }
    }
    #[test]
    fn test_parse_forms() {
        unsafe {
            assert_eq!(
                parse_vector("[  ]   ".into()).unwrap(),
                (
                    Span::new_from_raw_offset(4, 1, "   ", ()),
                    AST {
                        pos: Span::new_from_raw_offset(0, 1, "[  ]", ()),
                        body: ASTBody::Vector(vec![])
                    }
                )
            );
            assert_eq!(
                parse_vector("[ 1 ,,]".into()).unwrap().1.body,
                ASTBody::Vector(vec![AST {
                    pos: Span::new_from_raw_offset(2, 1, "1", ()),
                    body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                }])
            );
            assert_eq!(
                parse_set("#{1, 2, 3}".into()).unwrap().1.body,
                ASTBody::Set(vec![
                    AST {
                        pos: Span::new_from_raw_offset(2, 1, "1", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(5, 1, "2", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(2))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(8, 1, "3", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(3))
                    }
                ])
            );
            assert_eq!(
                parse_list("(1, 2 \n \"hello\")".into()).unwrap().1.body,
                ASTBody::List(vec![
                    AST {
                        pos: Span::new_from_raw_offset(1, 1, "1", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(4, 1, "2", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(2))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(8, 2, "\"hello\"", ()),
                        body: ASTBody::StringLiteral("hello")
                    }
                ])
            );
            assert_eq!(
                parse_anonymous_fn("#(+ a b)".into()).unwrap().1.body,
                ASTBody::AnonymousFn(vec![
                    AST {
                        pos: Span::new_from_raw_offset(2, 1, "+", ()),
                        body: ASTBody::Symbol {
                            ns: None,
                            name: "+"
                        }
                    },
                    AST {
                        pos: Span::new_from_raw_offset(4, 1, "a", ()),
                        body: ASTBody::Symbol {
                            ns: None,
                            name: "a"
                        }
                    },
                    AST {
                        pos: Span::new_from_raw_offset(6, 1, "b", ()),
                        body: ASTBody::Symbol {
                            ns: None,
                            name: "b"
                        }
                    }
                ])
            );
        }
    }
    #[test]
    fn parse_map_test() {
        unsafe {
            assert_eq!(
                parse_map("{\n:a 1 \n :b \"text\" }".into()).unwrap().1.body,
                ASTBody::Map(vec![
                    (
                        AST {
                            pos: Span::new_from_raw_offset(2, 2, ":a", ()),
                            body: ASTBody::Keyword {
                                ns: None,
                                name: "a"
                            }
                        },
                        AST {
                            pos: Span::new_from_raw_offset(5, 2, "1", ()),
                            body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                        }
                    ),
                    (
                        AST {
                            pos: Span::new_from_raw_offset(9, 3, ":b", ()),
                            body: ASTBody::Keyword {
                                ns: None,
                                name: "b"
                            }
                        },
                        AST {
                            pos: Span::new_from_raw_offset(12, 3, "\"text\"", ()),
                            body: ASTBody::StringLiteral("text")
                        }
                    )
                ])
            );
        }
    }
    #[test]
    fn parse_quote_test() {
        let result = parse_quote("'(+ a 1)".into()).unwrap();
        assert!(match result.1.body {
            ASTBody::Quote(quoted_form) => {
                match quoted_form.body {
                    ASTBody::List(_) => true,
                    _ => false,
                }
            }
            _ => false,
        })
    }
    #[test]
    fn parse_unquote_test() {
        let result = parse_unquote("~value".into()).unwrap();
        assert!(match result.1.body {
            ASTBody::UnQuote(quoted_form) => {
                match quoted_form.body {
                    ASTBody::Symbol { ns, name } => true,
                    _ => false,
                }
            }
            _ => false,
        })
    }
    #[test]
    fn parse_syntax_quote_test() {
        let result = parse_syntax_quote("`(+ a 1)".into()).unwrap();
        assert!(match result.1.body {
            ASTBody::SyntaxQuote(quoted_form) => {
                match quoted_form.body {
                    ASTBody::List(_) => true,
                    _ => false,
                }
            }
            _ => false,
        })
    }
    #[test]
    fn parse_nil_test() {
        let result = parse_form("[a nil]".into()).unwrap();
        assert!(match result.1.body {
            ASTBody::Vector(forms) => {
                unsafe {
                    assert_eq!(
                        forms,
                        [
                            AST {
                                pos: Span::new_from_raw_offset(1, 1, "a", ()),
                                body: ASTBody::Symbol {
                                    ns: None,
                                    name: "a"
                                }
                            },
                            AST {
                                pos: Span::new_from_raw_offset(3, 1, "nil", ()),
                                body: ASTBody::Nil
                            },
                        ]
                    )
                }
                true
            }
            _ => {
                println!("result is not vector");
                false
            }
        })
    }
    #[test]
    fn comment_test() {
        let forms = match parse_source("; comment \n (def a ;; comment \r\n 10)".into())
            .unwrap()
            .1
            .body
        {
            ASTBody::Root(forms) => forms,
            _ => panic!(),
        };
        let list = forms.first().unwrap();
        if let ASTBody::List(forms) = &list.body {
            unsafe {
                assert_eq!(
                    *forms,
                    vec![
                        AST {
                            pos: Span::new_from_raw_offset(13, 2, "def", ()),
                            body: ASTBody::Symbol {
                                ns: None,
                                name: "def"
                            }
                        },
                        AST {
                            pos: Span::new_from_raw_offset(17, 2, "a", ()),
                            body: ASTBody::Symbol {
                                ns: None,
                                name: "a"
                            }
                        },
                        AST {
                            pos: Span::new_from_raw_offset(33, 3, "10", ()),
                            body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(10))
                        }
                    ]
                );
            }
        } else {
            assert!(false);
        }
    }
    #[test]
    fn parse_metadata_test() {
        let (s, meta) = parse_metadata("^:keyword ^\"string\" ^{:map 1} ^symbol ".into()).unwrap();
        assert_eq!("", s.to_string());
        unsafe {
            assert_eq!(
                meta.body,
                ASTBody::MetaData(vec![
                    AST {
                        pos: Span::new_from_raw_offset(1, 1, ":keyword", ()),
                        body: ASTBody::Keyword {
                            ns: None,
                            name: "keyword",
                        },
                    },
                    AST {
                        pos: Span::new_from_raw_offset(11, 1, "\"string\"", ()),
                        body: ASTBody::StringLiteral("string"),
                    },
                    AST {
                        pos: Span::new_from_raw_offset(21, 1, "{:map 1}", ()),
                        body: ASTBody::Map(vec![(
                            AST {
                                pos: Span::new_from_raw_offset(22, 1, ":map", ()),
                                body: ASTBody::Keyword {
                                    ns: None,
                                    name: "map",
                                },
                            },
                            AST {
                                pos: Span::new_from_raw_offset(27, 1, "1", ()),
                                body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1)),
                            },
                        )]),
                    },
                    AST {
                        pos: Span::new_from_raw_offset(31, 1, "symbol", ()),
                        body: ASTBody::Symbol {
                            ns: None,
                            name: "symbol",
                        }
                    }
                ]),
            )
        }
    }
}

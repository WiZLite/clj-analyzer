use nom::{
    branch::{alt, permutation},
    bytes::complete::{tag, take_till, take_till1, take_until},
    character::complete::{
        alpha1, alphanumeric1, char, digit0, digit1, multispace0, multispace1, one_of,
    },
    combinator::{map_res, not, opt},
    error::ParseError,
    multi::{self, count, many0, many1, separated_list0},
    sequence::delimited,
    AsChar, IResult, InputTakeAtPosition,
};
use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;

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

#[derive(PartialEq, Debug)]
pub enum ASTBody<'a> {
    Symbol { ns: Option<&'a str>, name: &'a str },
    Keyword { ns: Option<&'a str>, name: &'a str },
    NumberLiteral(NumberLiteralValue),
    StringLiteral(&'a str),
    List(Vec<AST<'a>>),
    Vector(Vec<AST<'a>>),
    Set(Vec<AST<'a>>),
    Map(Vec<(AST<'a>, AST<'a>)>),
    AnonymousFn(Vec<AST<'a>>),
}

fn parse_number(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
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
        Ok((
            s,
            AST {
                pos,
                body: ASTBody::NumberLiteral(NumberLiteralValue::Real(
                    (integer + fraction).copysign(sign),
                )),
            },
        ))
    } else {
        let sign: i64 = if sign.unwrap_or('+') == '+' { 1 } else { -1 };
        let integer: i64 = int.parse::<i64>().unwrap();
        Ok((
            s,
            AST {
                pos,
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

fn separator0(s: Span) -> IResult<Span, ()> {
    let (s, _) = many0(alt((comma, multispace)))(s)?;
    Ok((s, ()))
}

fn separator1(s: Span) -> IResult<Span, ()> {
    let (s, _) = many1(alt((comma, multispace)))(s)?;
    Ok((s, ()))
}

const CHARS_ALLOWED_IN_KEYWORD_AND_SYMBOL: [char; 7] = ['*', '+', '!', '-', '_', '?', '.'];

fn parse_word(s: Span) -> IResult<Span, Span> {
    take_till1(|x: char| !x.is_alphanumeric() && !CHARS_ALLOWED_IN_KEYWORD_AND_SYMBOL.contains(&x))(
        s,
    )
}

fn parse_symbol(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    not(digit1)(s)?;
    let (s, first) = parse_word(s)?;
    let (s, after_slash) = opt(permutation((char('/'), parse_word)))(s)?;
    if let Some((_, second)) = after_slash {
        Ok((
            s,
            AST {
                pos,
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
                pos,
                body: ASTBody::Symbol {
                    ns: None,
                    name: first.fragment(),
                },
            },
        ))
    }
}

fn parse_keyword(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    let (s, _) = char(':')(s)?;
    let (s, second_colon) = opt(char(':'))(s)?;
    if second_colon.is_some() {
        let (s, first) = parse_word(s)?;
        let (s, after_slash) = opt(permutation((char('/'), parse_word)))(s)?;
        if let Some((_, second)) = after_slash {
            Ok((
                s,
                AST {
                    pos,
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
                    pos,
                    body: ASTBody::Keyword {
                        ns: Some(""),
                        name: first.fragment(),
                    },
                },
            ))
        }
    } else {
        let (s, name) = parse_word(s)?;
        Ok((
            s,
            AST {
                pos,
                body: ASTBody::Keyword {
                    ns: None,
                    name: name.fragment(),
                },
            },
        ))
    }
}

fn parse_string(s: Span) -> IResult<Span, AST> {
    let (s, _) = char('"')(s)?;
    let (s, pos) = position(s)?;
    let (s, text) = alpha1(s)?;
    let (s, _) = char('"')(s)?;
    Ok((
        s,
        AST {
            pos,
            body: ASTBody::StringLiteral(text.fragment()),
        },
    ))
}

fn parse_forms(s: Span) -> IResult<Span, Vec<AST>> {
    let (s, forms) = separated_list0(separator1, parse_form)(s)?;
    Ok((s, forms))
}

fn parse_list(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    let (s, forms) = delimited(
        permutation((char('('), separator0)),
        parse_forms,
        permutation((char(')'), separator0)),
    )(s)?;
    Ok((
        s,
        AST {
            pos,
            body: ASTBody::List(forms),
        },
    ))
}

fn parse_vector(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    let (s, forms) = delimited(
        permutation((char('['), separator0)),
        parse_forms,
        permutation((char(']'), separator0)),
    )(s)?;
    Ok((
        s,
        AST {
            pos,
            body: ASTBody::Vector(forms),
        },
    ))
}

fn parse_set(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    let (s, _) = char('#')(s)?;
    let (s, forms) = delimited(
        permutation((char('{'), separator0)),
        parse_forms,
        permutation((char('}'), separator0)),
    )(s)?;
    Ok((
        s,
        AST {
            pos,
            body: ASTBody::Set(forms),
        },
    ))
}

fn parse_anonymous_fn(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    let (s, _) = char('#')(s)?;
    let (s, forms) = delimited(
        permutation((char('('), separator0)),
        parse_forms,
        permutation((char(')'), separator0)),
    )(s)?;
    Ok((
        s,
        AST {
            pos,
            body: ASTBody::AnonymousFn(forms),
        },
    ))
}

fn parse_map(s: Span) -> IResult<Span, AST> {
    fn parse_kv(s: Span) -> IResult<Span, (AST, AST)> {
        let (s, (k, _, v)) = permutation((parse_form, separator1, parse_form))(s)?;
        Ok((s, (k, v)))
    }
    let (s, pos) = position(s)?;
    let (s, kvs) = delimited(
        permutation((char('{'), separator0)),
        separated_list0(separator1, parse_kv),
        permutation((char('}'), separator0)),
    )(s)?;
    Ok((
        s,
        AST {
            pos,
            body: ASTBody::Map(kvs),
        },
    ))
}

fn parse_form(s: Span) -> IResult<Span, AST> {
    let (s, pos) = position(s)?;
    alt((
        parse_list,
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
        assert_eq!(
            parse_number("10".into()).unwrap().1.body,
            ASTBody::NumberLiteral(NumberLiteralValue::Integer(10))
        );
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
                    pos: Span::new_from_raw_offset(0, 1, "", ()),
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
                    pos: Span::new_from_raw_offset(0, 1, "", ()),
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
                    pos: Span::new_from_raw_offset(0, 1, "", ()),
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
                parse_vector("[ 1 ,,]".into()).unwrap().1.body,
                ASTBody::Vector(vec![AST {
                    pos: Span::new_from_raw_offset(2, 1, "", ()),
                    body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                }])
            );
            assert_eq!(
                parse_set("#{1, 2, 3}".into()).unwrap().1.body,
                ASTBody::Set(vec![
                    AST {
                        pos: Span::new_from_raw_offset(2, 1, "", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(5, 1, "", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(2))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(8, 1, "", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(3))
                    }
                ])
            );
            assert_eq!(
                parse_list("(1, 2 \n \"hello\")".into()).unwrap().1.body,
                ASTBody::List(vec![
                    AST {
                        pos: Span::new_from_raw_offset(1, 1, "", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(4, 1, "", ()),
                        body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(2))
                    },
                    AST {
                        pos: Span::new_from_raw_offset(9, 2, "", ()),
                        body: ASTBody::StringLiteral("hello")
                    }
                ])
            );
            assert_eq!(
                parse_anonymous_fn("#(+ a b)".into()).unwrap().1.body,
                ASTBody::AnonymousFn(vec![
                    AST {
                        pos: Span::new_from_raw_offset(2, 1, "", ()),
                        body: ASTBody::Symbol { ns: None, name: "+" }
                    },
                    AST {
                        pos: Span::new_from_raw_offset(4, 1, "", ()),
                        body: ASTBody::Symbol { ns: None, name: "a" }
                    },
                    AST {
                        pos: Span::new_from_raw_offset(6, 1, "", ()),
                        body: ASTBody::Symbol { ns: None, name: "b" }
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
                            pos: Span::new_from_raw_offset(2, 2, "", ()),
                            body: ASTBody::Keyword {
                                ns: None,
                                name: "a"
                            }
                        },
                        AST {
                            pos: Span::new_from_raw_offset(5, 2, "", ()),
                            body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(1))
                        }
                    ),
                    (
                        AST {
                            pos: Span::new_from_raw_offset(9, 3, "", ()),
                            body: ASTBody::Keyword {
                                ns: None,
                                name: "b"
                            }
                        },
                        AST {
                            pos: Span::new_from_raw_offset(13, 3, "", ()),
                            body: ASTBody::StringLiteral("text")
                        }
                    )
                ])
            );
        }
    }
}

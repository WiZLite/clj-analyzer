use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, digit0, digit1, one_of},
    combinator::opt,
    IResult,
};
use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum NumberLiteralValue {
    Integer(i64),
    Real(f64),
}

pub struct AST<'a> {
    pub pos: Span<'a>,
    pub body: ASTBody<'a>
}

#[derive(PartialEq, Debug)]
pub enum ASTBody<'a> {
    NumberLiteral(NumberLiteralValue),
    StringLiteral(&'a str)
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
                body: ASTBody::NumberLiteral(NumberLiteralValue::Real((integer + fraction).copysign(sign)))
            }
        ))
    } else {
        let sign: i64 = if sign.unwrap_or('+') == '+' { 1 } else { -1 };
        let integer: i64 = int.parse::<i64>().unwrap();
        Ok((
            s,
            AST {
                pos,
                body: ASTBody::NumberLiteral(NumberLiteralValue::Integer(sign * integer))
            }
        ))
    }
}

#[derive(Debug)]
struct StringLiteralNode<'a> {
    pub position: Span<'a>,
    pub text: &'a str,
}

fn parse_string(s: Span) -> IResult<Span, AST> {
    let (s, _) = tag("\"")(s)?;
    let (s, pos) = position(s)?;
    let (s, text) = alpha1(s)?;
    let (s, _) = tag("\"")(s)?;
    Ok((
        s,
        AST {
            pos,
            body: ASTBody::StringLiteral(text.fragment())
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
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
}

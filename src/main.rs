#![allow(unused)]

mod parser;
use nom::{IResult, bytes::complete::{take_until, tag}};
use nom_locate::{LocatedSpan, position};
use clap::Parser;

pub type Span<'a> = LocatedSpan<&'a str>;

struct Token<'a> {
    pub position: Span<'a>,
    pub foo: &'a str,
    pub bar: &'a str,
}

fn parse_foobar(s: Span) -> IResult<Span, Token> {
    let (s, _) = take_until("foo")(s)?;
    let (s, pos) = position(s)?;
    let (s, foo) = tag("foo")(s)?;
    let (s, bar) = tag("bar")(s)?;

    dbg!(pos);
    dbg!(foo);
    dbg!(bar);

    Ok((
        s,
        Token {
            position: pos,
            foo: foo.fragment(),
            bar: bar.fragment(),
        },
    ))
}


/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    /// The path to the file to read
    path: std::path::PathBuf,
}

// fn main() {
//     let args = Cli::parse();
//     let path = &args.path;
//     let content = std::fs::read_to_string(path).expect("could not read file");
//     let mut line_index = 0;
//     for line in content.lines() {
//         line_index += 1;
//         if let Some(column_index) = line.find("hoge") {
//             println!("{}:{}:{}: warning: the word 'hoge' is prohibited for political reason😂", path.display(), line_index, column_index);
//         }
//     }
// }


fn main () {
    let input = Span::new("Lorem ipsum \n foobar aa");
    let output = parse_foobar(input);
    let position = output.unwrap().1.position;
    assert_eq!(position.location_offset(), 14);
    assert_eq!(position.location_line(), 2);
    assert_eq!(position.fragment(), &"");
    assert_eq!(position.get_column(), 2);
}

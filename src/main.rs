#![allow(unused)]

mod analyzer;
mod parser;
mod rules;
use analyzer::Analysis;
use clap::Parser;
use nom::{
    bytes::complete::{tag, take_until},
    IResult,
};
use nom_locate::{position, LocatedSpan};
use parser::AST;
use rules::{get_syntax_rules, SyntaxRule};

#[derive(clap::Subcommand)]
enum SubCommand {
    Lint { path: std::path::PathBuf },
}

#[derive(clap::Parser)]
struct Cli {
    #[command(subcommand)]
    sub_command: SubCommand,
}

fn main() {
    let args = Cli::parse();
    let sub_command = &args.sub_command;
    match sub_command {
        SubCommand::Lint { path } => {
            let content = std::fs::read_to_string(path).expect("could not read file");
            // TODO: handle syntax error
            let (_, root) = parser::parse_source(LocatedSpan::new(&content)).unwrap();
            let syntax_rules = get_syntax_rules(edn_rs::Edn::Nil);
            analyzer::visit_ast_with_analyzing(analyzer::VisitArgs {
                filename: path.to_str().unwrap(),
                ast: &root,
                on_visit: |ast, anlysis| {
                    for syntax_rule in &syntax_rules {
                        syntax_rule.on_visit(ast, &|target_ast, severity, message| {
                            let location = Location::from(target_ast.pos);

                            println!(
                                "{}:{}:{}: {}: {}",
                                path.display(),
                                location.from_line,
                                location.from_col,
                                severity,
                                message,
                            )
                        })
                    }
                },
                on_scope_end: |_, _| {},
                on_analysis_end: |_| {},
            })
        }
    }
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub from_line: u16,
    pub from_col: u16,
    pub to_line: u16,
    pub to_col: u16,
}

impl<'a> From<Span<'a>> for Location {
    fn from(span: Span<'a>) -> Self {
        let lines = span.lines().count() as u32;
        Location {
            from_line: span.location_line() as u16,
            from_col: (span.get_column() - 1) as u16, // Convert index to starting from 0
            to_line: (span.location_line() + lines - 1) as u16,
            to_col: span.lines().last().unwrap().len() as u16,
        }
    }
}

#![allow(unused)]

mod analyzer;
mod parser;
mod rules;
use clap::Parser;
use nom::{
    bytes::complete::{tag, take_until},
    IResult,
};
use nom_locate::{position, LocatedSpan};
use rules::{get_rules, LintRule};

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
            let (_, root) = parser::parse_forms(LocatedSpan::new(&content)).unwrap();
            let rules = get_rules(edn_rs::Edn::Nil);
            for ast in root {
                analyzer::visit_ast(&ast, &|ast| {
                    for rule in &rules {
                        if rule.predicate(ast) {
                            let message = rule.get_message(ast);
                            println!(
                                "{}:{}:{}: {}: {}",
                                path.display(),
                                message.location.line,
                                message.location.column,
                                message.level,
                                message.message,
                            )
                        }
                    }
                })
            }
        }
    }
}

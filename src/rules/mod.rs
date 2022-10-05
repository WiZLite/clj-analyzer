use std::fmt::Display;

use crate::{
    parser::{self, ASTBody},
    Location, Span,
};
mod readable_condition;
use edn_rs;

pub enum Severity {
    Info,
    Warning,
    Error,
}

impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Info => write!(f, "info"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
        }
    }
}

pub trait SyntaxRule {
    fn new(config: edn_rs::Edn) -> Box<Self>;
    fn on_visit(&self, ast: &parser::AST, emit_message: &impl Fn(&parser::AST, Severity, &str) -> ());
}

pub fn get_syntax_rules(config: edn_rs::Edn) -> Vec<Box<impl SyntaxRule>> {
    vec![readable_condition::ReadableCondition::new(config)]
}

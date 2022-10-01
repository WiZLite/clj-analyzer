use std::fmt::Display;

use crate::parser::{self, ASTBody};
use edn_rs;

pub enum LintLevel {
    Info,
    Warning,
    Error
}

impl Display for LintLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LintLevel::Info => write!(f, "info"),
            LintLevel::Warning => write!(f, "warning"),
            LintLevel::Error => write!(f, "error")
        }
    }
}

pub trait LintRule {
    fn rule_name() -> String;
    fn new(config: edn_rs::Edn) -> Box<Self>;
    fn predicate(&self, ast: &parser::AST) -> bool;
    fn get_message(&self, ast: &parser::AST) -> (LintLevel, String);
}

pub fn get_rules(config: edn_rs::Edn) -> Vec<Box<impl LintRule>> {
    vec![]
}
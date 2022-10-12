use std::error::Error;

use crate::{parser::AST, Location};

#[derive(Debug)]
pub struct SyntaxError {
    pub location: Location,
    pub message: String,
}

impl SyntaxError {
    pub fn from_ast_and_message(ast: &AST, message: &str) -> Self {
        SyntaxError {
            location: Location::from(ast.pos),
            message: message.to_string(),
        }
    }
}

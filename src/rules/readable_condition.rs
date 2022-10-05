use crate::parser::{self, AST, ASTBody};

use super::{Severity, SyntaxRule};

pub(super) struct ReadableCondition {}
impl SyntaxRule for ReadableCondition {
    fn new(config: edn_rs::Edn) -> Box<Self> {
        Box::new(Self {})
    }

    fn on_visit(&self, ast: &parser::AST, emit_message: &impl Fn(&parser::AST, Severity, &str) -> ()) {
        if let ASTBody::List(forms) = &ast.body {
            if let Some(first) = forms.get(0) {
                if let Some(second) = forms.get(1) {
                    if let ASTBody::Symbol { ns, name: "if-not" } = &first.body {
                        if let ASTBody::List(forms) = &second.body {
                            if let Some(first_form) = forms.first() {
                                if let ASTBody::Symbol { ns, name: "and" | "or" } = first_form.body {
                                    emit_message(first_form, Severity::Warning, "if-not with condition using and/or is hard to read")
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

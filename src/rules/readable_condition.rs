use crate::parser::{self, ASTBody};

use super::{LintRule, LintLevel};

pub(super) struct ReadableCondition {}
impl LintRule for ReadableCondition {
    fn rule_name() -> String {
        return "readable-condition".to_string()
    }

    fn new(config: edn_rs::Edn) -> Box<Self> {
        Box::new(Self {})
    }

    fn predicate(&self, ast: &parser::AST) -> bool {
        if let ASTBody::List(forms) = &ast.body {
            if let Some(first) = forms.get(0) {
                if let Some(second) = forms.get(1) {
                    if let ASTBody::Symbol { ns, name } = first.body {
                        if name == "if-not" {
                            if let ASTBody::List(forms) = &second.body {
                                if let Some(first_form) = forms.first() {
                                    if let ASTBody::Symbol { ns, name } = first_form.body {
                                        return name == "and" || name == "or";
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        false
    }

    fn get_message(&self, ast: &parser::AST) -> (LintLevel, String) {
        (LintLevel::Warning, "if-not with condition using and/or is hard to read".to_string())
    }
}

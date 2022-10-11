use std::cell::RefCell;

use crate::{parser::{self, AST, ASTBody}, analyzer::Analysis};

use super::{Severity, SyntaxRule};

pub(super) struct RedundantLet {}
impl SyntaxRule for RedundantLet {
    fn new(config: edn_rs::Edn) -> Box<Self> {
        Box::new(Self {})
    }

    fn on_visit(&self, ast: &parser::AST, emit_message: &impl Fn(&parser::AST, Severity, &str) -> ()) {
      // FIXME:
      let analysis = Analysis::new();
        if let ASTBody::List(forms) = &ast.body {
            if let Some(first) = forms.get(0) {
                if let Some(second) = forms.get(1) {
                    if let ASTBody::Symbol { ns, name: "let" } = &first.body {
                      let mut has_cond_form_in_ancestor = false;
                      let mut has_let_ancestor = false;
                      for ancestor in analysis.ascend(first) {
                        if let ASTBody::List(forms) = &ancestor.body {
                          if let Some(symbol_ast) = forms.get(0) {
                            match symbol_ast.body {
                              ASTBody::Symbol { ns, name: "let" } => {
                                break;
                              },
                              ASTBody::Symbol { ns, name: "if" | "when" | "if-let" | "when-let" } => {
                                has_cond_form_in_ancestor = true;
                              },
                              _ => continue,
                            }
                          }
                        }
                      }
                      if !has_cond_form_in_ancestor {
                        emit_message(first, Severity::Warning, "Redundant let expression.");
                      }
                    }
                }
            }
        }
    }
}

#[test]
fn test_redundant_let() {
  let emitted = RefCell::new(false);
}
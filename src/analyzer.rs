use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use crate::parser::ASTBody;
use crate::parser::AST;
use crate::Location;
use crate::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct NamespaceDef<'a> {
    location: Span<'a>,
    name: &'a str,
    filename: &'a str,
}

#[derive(Debug)]
pub struct Analysis<'a> {
    pub namespace_definitions: HashMap<&'a str, NamespaceDef<'a>>,
}

impl<'a> Analysis<'a> {
    pub fn new() -> Self {
        Analysis {
            namespace_definitions: HashMap::new(),
        }
    }
}

pub fn visit_ast<'a>(filename: &str, ast: &'a AST<'a>, effect: &impl Fn(&'a AST) -> ()) {
    match &ast.body {
        ASTBody::Symbol { ns, name } => effect(ast),
        ASTBody::Keyword { ns, name } => effect(&ast),
        ASTBody::NumberLiteral(_) => effect(&ast),
        ASTBody::StringLiteral(_) => effect(&ast),
        ASTBody::List(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(filename, form, effect)
            }
        }
        ASTBody::Vector(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(filename, form, effect)
            }
        }
        ASTBody::Set(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(filename, form, effect);
            }
        }
        ASTBody::Map(forms) => {
            effect(&ast);
            for (k, v) in forms {
                visit_ast(filename, k, effect);
                visit_ast(filename, v, effect);
            }
        }
        ASTBody::AnonymousFn(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(filename, form, effect)
            }
        }
        ASTBody::Quote(form) => {
            effect(&ast);
            visit_ast(filename, form, effect);
        }
        ASTBody::SyntaxQuote(form) => {
            effect(&ast);
            visit_ast(filename, form, effect);
        }
        ASTBody::UnQuote(_) => {
            effect(&ast);
            visit_ast(filename, ast, effect);
        }
    };
}

type AnalysisCell<'a> = Rc<RefCell<Analysis<'a>>>;

#[rustfmt::skip]
fn analyze_ns_definitions<'a>(filename: &'a str, ast: &AST<'a>, analysis: AnalysisCell<'a>) {
    let forms = if let ASTBody::List(forms) = &ast.body { forms } else { return; };
    let first = if let Some(first) = forms.get(0) { first } else { return; };
    let second = if let Some(second) = forms.get(1) { second } else { return; };
    if let ASTBody::Symbol { ns, name: "ns" } = first.body {
        if let ASTBody::Symbol { ns, name: ns_name } = second.body {
            println!("inserting {}", ns_name);
            analysis.borrow_mut().namespace_definitions.insert(
                ns_name,
                NamespaceDef {
                    location: ast.pos.into(),
                    name: ns_name,
                    filename,
                },
            );
        }
    }
}

pub fn visit_ast_with_analyzing<'a>(
    filename: &'a str,
    ast: &'a AST,
    effect: &impl Fn(&AST, AnalysisCell) -> (),
    on_analysis_end: impl FnOnce(&Analysis) -> (),
) {
    let mut analysis = Rc::new(RefCell::new(Analysis::new()));
    visit_ast(filename, ast, &|ast| {
        analyze_ns_definitions(filename, ast, analysis.clone());
        effect(ast, analysis.clone());
    });
    let result = analysis.borrow();
    on_analysis_end(result.deref());
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_forms;
    use pretty_assertions::assert_eq;

    use super::*;

    fn do_nothing(ast: &AST, analysis: AnalysisCell) {}

    #[test]
    fn ananalyze_ns_definitions_test() {
        let (_, root) = parse_forms(
            "(ns clj-analizer.core
(:require [clojure.core :as core]))"
                .into(),
        )
        .unwrap();
        let found = RefCell::new(false);
        for ast in root {
            visit_ast_with_analyzing(
                "sample".into(),
                &ast,
                &|ast, analysis| {
                    if (!*found.borrow()) {
                        if analysis
                            .borrow()
                            .namespace_definitions
                            .contains_key("clj-analizer.core")
                        {
                            *found.borrow_mut() = true;
                        }
                    }
                },
                |_| {},
            );
        }
        assert!(*found.borrow());
    }
}

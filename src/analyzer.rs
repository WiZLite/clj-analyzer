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

#[derive(Debug, PartialEq, Eq)]
pub struct VarDefinition<'a> {
    location: Span<'a>,
    name: &'a str,
    filename: &'a str,
    defined_by: &'a str,
}

#[derive(Debug)]
pub struct AnalysisContext<'a> {
    current_ns: &'a str,
}

#[derive(Debug)]
pub struct Analysis<'a> {
    pub namespace_definitions: HashMap<&'a str, NamespaceDef<'a>>,
    pub var_definitions: HashMap<(&'a str, &'a str) /* ns and name */, VarDefinition<'a>>,
    pub context: Rc<RefCell<AnalysisContext<'a>>>,
}

impl<'a, 'temp> Analysis<'a> {
    pub fn new() -> Self {
        Analysis {
            namespace_definitions: HashMap::new(),
            var_definitions: HashMap::new(),
            context: Rc::new(RefCell::new(AnalysisContext { current_ns: "" })),
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
        ASTBody::UnQuote(form) => {
            effect(&ast);
            visit_ast(filename, form, effect);
        }
        ASTBody::EOF => {
            effect(&ast);
        }
        ASTBody::Root(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(filename, form, effect)
            }
        }
        ASTBody::Nil => effect(&ast),
        ASTBody::MetaData(_) => effect(&ast),
    };
}

type AnalysisCell<'a> = Rc<RefCell<Analysis<'a>>>;

#[rustfmt::skip]
fn analyze_ns_definitions<'a, 'temp>(filename: &'a str, ast: &AST<'a>, analysis: AnalysisCell<'a>) {
    let forms = if let ASTBody::List(forms) = &ast.body { forms } else { return; };
    let first = if let Some(first) = forms.get(0) { first } else { return; };
    let second = if let Some(second) = forms.get(1) { second } else { return; };
    if let ASTBody::Symbol { ns, name: "ns" } = first.body {
        if let ASTBody::Symbol { ns, name: ns_name } = second.body {
            analysis.borrow_mut().namespace_definitions.insert(
                ns_name,
                NamespaceDef {
                    location: second.pos.into(),
                    name: ns_name,
                    filename,
                },
            );
            *analysis.borrow_mut().context.borrow_mut() = AnalysisContext {
                current_ns: ns_name
            }
        }
    }
}

#[rustfmt::skip]
fn analyze_var_definitions<'a, 'temp>(filename: &'a str, ast: &AST<'a>, analysis: AnalysisCell<'a>) {
    let forms = if let ASTBody::List(forms) = &ast.body { forms } else { return; };
    let first = if let Some(first) = forms.get(0) { first } else { return; };
    let second = if let Some(second) = forms.get(1) { second } else { return; };
    let current_ns = analysis.borrow().context.borrow().current_ns;
    if let ASTBody::Symbol { ns, name: defined_by } = first.body {
        // TODO: make configurable
        if !defined_by.starts_with("def") { return; }
        if let ASTBody::Symbol { ns: qualified_ns, name: varname } = second.body {
            analysis.borrow_mut().var_definitions.insert(
                (qualified_ns.unwrap_or(current_ns), varname),
                VarDefinition {
                    location: second.pos.into(),
                    name: varname,
                    filename,
                    defined_by
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
        analyze_var_definitions(filename, ast, analysis.clone());
        effect(ast, analysis.clone());
    });
    let result = analysis.borrow();
    on_analysis_end(result.deref());
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_source;
    use pretty_assertions::assert_eq;

    use super::*;

    fn do_nothing(ast: &AST, analysis: AnalysisCell) {}

    #[test]
    fn ananalyze_ns_definitions_test() {
        let (_, root) = parse_source(
            "(ns clj-analyzer.core
(:require [clojure.core :as core]))"
                .into(),
        )
        .unwrap();
        visit_ast_with_analyzing("sample.clj".into(), &root, &do_nothing, |a| {
            let definition = a.namespace_definitions.get("clj-analyzer.core").unwrap();
            assert_eq!(definition.filename, "sample.clj");
            assert_eq!(definition.name, "clj-analyzer.core");
            assert_eq!(a.context.borrow().current_ns, "clj-analyzer.core");
        });
    }
    #[test]
    fn analyze_var_definition_test() {
        let (_, root) = parse_source("(ns test.core) (def a 10) (defrecord b (+ 1 2))".into()).unwrap();
        visit_ast_with_analyzing("src/sample.clj".into(), &root, &do_nothing, |a| {
            let a_def = a.var_definitions.get(&("test.core", "a")).unwrap();
            assert_eq!(a_def.name, "a");
            assert_eq!(a_def.filename, "src/sample.clj");
            assert_eq!(a_def.defined_by, "def");

            let b_def = a.var_definitions.get(&("test.core", "b")).unwrap();
            assert_eq!(b_def.name, "b");
            assert_eq!(b_def.filename, "src/sample.clj");
            assert_eq!(b_def.defined_by, "defrecord");
        });
    }
}

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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Binding<'a> {
    Normal {
        bound_by: &'a AST<'a>,
        value: &'a AST<'a>,
    },
    Destructing {
        bound_to: &'a AST<'a>,
        key: &'a str,
        map: &'a AST<'a>,
    },
}

#[derive(Debug)]
pub struct AnalysisContext<'a> {
    current_ns: &'a str,
    env: Vec<HashMap<&'a str, Binding<'a>>>,
}

impl<'a> AnalysisContext<'a> {
    pub fn in_ns(&mut self, ns: &'a str) {
        self.current_ns = ns;
    }
    pub fn pop_env(&mut self) {
        self.env.pop();
    }
    pub fn push_env(&mut self, scope_env: HashMap<&'a str, Binding<'a>>) {
        self.env.push(scope_env);
    }
    pub fn bind_var(&mut self, name: &'a str, ast: Binding<'a>) {
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name, ast);
        }
    }
    pub fn find_var(&self, name: &str) -> Option<Binding> {
        self.env
            .iter()
            .rev()
            .map(|scope| scope.get(name).map_or(None, |x| Some(*x)))
            .find(|x| x.is_some())
            .unwrap_or(None)
    }
}

#[derive(Debug)]
pub struct Analysis<'a> {
    pub namespace_definitions: HashMap<&'a str, NamespaceDef<'a>>,
    pub var_definitions: HashMap<(&'a str, &'a str) /* ns and name */, VarDefinition<'a>>,
    pub context: Rc<RefCell<AnalysisContext<'a>>>,
}

impl<'a> Analysis<'a> {
    pub fn new() -> Self {
        Analysis {
            namespace_definitions: HashMap::new(),
            var_definitions: HashMap::new(),
            context: Rc::new(RefCell::new(AnalysisContext {
                current_ns: "",
                env: Vec::new(),
            })),
        }
    }
}

type AnalysisCell<'a> = Rc<RefCell<Analysis<'a>>>;

#[rustfmt::skip]
fn analyze_var_definitions<'a>(filename: &'a str, ast: &AST<'a>, analysis: AnalysisCell<'a>) {
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

// Returns if there is any bindings
// TODO: handle syntax errors.
#[rustfmt::skip]
fn analyze_let_bindings<'a>(filename: &'a str, ast: &'a AST<'a>, analysis: AnalysisCell<'a>) -> bool {
    let forms = if let ASTBody::List(forms) = &ast.body { forms } else { return false; };
    let first = if let Some(first) = forms.get(0) { first } else { return false; };
    let second = if let Some(second) = forms.get(1) { second } else { return false; };
    if let ASTBody::Symbol { ns, name: "let" | "if-let" | "when-let", } = &first.body {
        if let ASTBody::Vector(forms) = &second.body {
            if forms.len() % 2 != 0 {
                println!(
                    "let bindings must have even number of forms. {}",
                    ast.fragment()
                );
                return false;
            }

            let new_scope = HashMap::new();
            analysis
                .borrow_mut()
                .context
                .borrow_mut()
                .push_env(new_scope);

            for binding in forms.chunks(2) {
                match &binding[0].body {
                    ASTBody::Symbol { ns, name } => {
                        if let Some(ns_name) = ns {
                            println!("Syntax error. Invalid var name {}/{}", ns.unwrap(), name);
                        } else {
                            analysis.borrow_mut().context.borrow_mut().bind_var(name, Binding::Normal{ bound_by: &binding[0], value: &binding[1]});
                        }
                    },
                    ASTBody::Map(kvs) => {
                        for (k, v) in kvs {
                            match k.body {
                                // ex) [{binded_symbol :key} values]
                                ASTBody::Symbol { ns, name: bind_name } => {
                                    if let ASTBody::Keyword { ns, name: key } = v.body {
                                        analysis.borrow_mut().context.borrow_mut().bind_var(bind_name, Binding::Destructing { bound_to:k, key, map: &binding[1] });
                                    } else {
                                        println!("{}, Syntax error. Expect keyword but found {}", v.pos, v.fragment())
                                    }
                                },
                                // ex) [{:keys [:a :b :c]} values]
                                ASTBody::Keyword { ns, name: "keys" } => {
                                    if let ASTBody::Vector(keys) = &v.body {
                                        for key in keys {
                                            if let ASTBody::Keyword { ns, name: bind_name } = key.body {
                                                analysis.borrow_mut().context.borrow_mut().bind_var(bind_name, Binding::Destructing { bound_to: key, key: bind_name, map: &binding[1] });
                                            } else {
                                                println!("{} Syntax error. Expect keyword but found {}",key.pos, key.fragment())
                                            }
                                        }
                                    } else {
                                        println!("{} Syntax error. Expect vector but found {}", v.pos, v.fragment())
                                    }
                                },
                                // ex) [{:as context ...}]
                                ASTBody::Keyword { ns, name: "as" } => {
                                    if let ASTBody::Symbol { ns, name: bind_name } = v.body {
                                        analysis.borrow_mut().context.borrow_mut().bind_var(bind_name, Binding::Normal{ bound_by: v, value: &binding[1]});
                                    } else {
                                        println!("{} Syntax error. Expect keyword but found {}", v.pos, v.fragment())
                                    }
                                }
                                _ => {
                                    println!("{} Syntax error! Invalid binding {}", v.pos, k.fragment())
                                }
                            }
                        }
                    }
                    _ => {
                        println!("Syntax error! Expect symbol or map but found {}", ast.fragment());
                        return false;
                    }
                }
            }
            return true;
        }
    }
    false
}

#[rustfmt::skip]
fn analyze_ns_definitions<'a>(filename: &'a str, ast: &AST<'a>, analysis: AnalysisCell<'a>) {
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
            analysis.borrow_mut().context.borrow_mut().in_ns(ns_name);
        }
    }
}

pub fn _visit_ast_with_analyzing<'a>(
    filename: &'a str,
    ast: &'a AST<'a>,
    effect: &impl Fn(&'a AST) -> (),
    analysis: AnalysisCell<'a>,
) {
    match &ast.body {
        ASTBody::Symbol { ns, name } => effect(ast),
        ASTBody::Keyword { ns, name } => effect(&ast),
        ASTBody::NumberLiteral(_) => effect(&ast),
        ASTBody::StringLiteral(_) => effect(&ast),
        ASTBody::List(forms) => {
            analyze_ns_definitions(filename, ast, analysis.clone());
            analyze_var_definitions(filename, ast, analysis.clone());
            let is_scope = analyze_let_bindings(filename, ast, analysis.clone());

            effect(&ast);
            for form in forms {
                _visit_ast_with_analyzing(filename, form, effect, analysis.clone())
            }

            if is_scope {
                analysis.borrow_mut().context.borrow_mut().pop_env();
            }
        }
        ASTBody::Vector(forms) => {
            effect(&ast);
            for form in forms {
                _visit_ast_with_analyzing(filename, form, effect, analysis.clone())
            }
        }
        ASTBody::Set(forms) => {
            effect(&ast);
            for form in forms {
                _visit_ast_with_analyzing(filename, form, effect, analysis.clone());
            }
        }
        ASTBody::Map(forms) => {
            effect(&ast);
            for (k, v) in forms {
                _visit_ast_with_analyzing(filename, k, effect, analysis.clone());
                _visit_ast_with_analyzing(filename, v, effect, analysis.clone());
            }
        }
        ASTBody::AnonymousFn(forms) => {
            effect(&ast);
            for form in forms {
                _visit_ast_with_analyzing(filename, form, effect, analysis.clone())
            }
        }
        ASTBody::Quote(form) => {
            effect(&ast);
            _visit_ast_with_analyzing(filename, form, effect, analysis.clone());
        }
        ASTBody::SyntaxQuote(form) => {
            effect(&ast);
            _visit_ast_with_analyzing(filename, form, effect, analysis.clone());
        }
        ASTBody::UnQuote(form) => {
            effect(&ast);
            _visit_ast_with_analyzing(filename, form, effect, analysis.clone());
        }
        ASTBody::EOF => {
            effect(&ast);
        }
        ASTBody::Root(forms) => {
            effect(&ast);
            for form in forms {
                _visit_ast_with_analyzing(filename, form, effect, analysis.clone())
            }
        }
        ASTBody::Nil => effect(&ast),
        ASTBody::MetaData(_) => effect(&ast),
    };
}

pub fn visit_ast_with_analyzing<'a>(
    filename: &'a str,
    ast: &'a AST,
    effect: &impl Fn(&AST, AnalysisCell) -> (),
    on_analysis_end: impl FnOnce(&Analysis) -> (),
) {
    let mut analysis = Rc::new(RefCell::new(Analysis::new()));
    _visit_ast_with_analyzing(
        filename,
        ast,
        &|ast| {
            effect(ast, analysis.clone());
        },
        analysis.clone(),
    );
    let result = analysis.borrow();
    on_analysis_end(result.deref());
}

#[cfg(test)]
mod tests {
    use crate::parser::*;
    use pretty_assertions::assert_eq;

    use super::*;

    fn do_nothing(ast: &AST, analysis: AnalysisCell) {}
    fn do_nothing_on_end(analysis: &Analysis) {}

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
        let (_, root) =
            parse_source("(ns test.core) (def a 10) (defrecord b (+ 1 2))".into()).unwrap();
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
    #[test]
    fn analyze_let_binding_simple_case_test() {
        let source = "(let [a 1
                            b \"hello\"]
                        (+ a 1)
                        (not b))";
        let (_, root) = parse_source(source.into()).unwrap();
        let mut first_a_visited = RefCell::new(false);
        let mut second_a_visited = RefCell::new(false);
        let mut first_b_visited = RefCell::new(false);
        let mut second_b_visited = RefCell::new(false);
        visit_ast_with_analyzing(
            "src/sample.clj",
            &root,
            &|ast, analysis| match ast.body {
                ASTBody::Symbol { ns, name: "a" } => {
                    if !*first_a_visited.borrow() {
                        *first_a_visited.borrow_mut() = true;
                        return;
                    } else {
                        assert!(if let Some(binded) =
                            analysis.borrow().context.borrow().find_var("a")
                        {
                            unsafe {
                                assert_eq!(
                                    binded,
                                    Binding::Normal {
                                        bound_by: &AST {
                                            pos: Span::new_from_raw_offset(6, 1, "a", ()),
                                            body: ASTBody::Symbol {
                                                ns: None,
                                                name: "a"
                                            }
                                        },
                                        value: &AST {
                                            pos: Span::new_from_raw_offset(8, 1, "1", ()),
                                            body: ASTBody::NumberLiteral(
                                                NumberLiteralValue::Integer(1)
                                            ),
                                        }
                                    }
                                )
                            }
                            true
                        } else {
                            false
                        });
                        *second_a_visited.borrow_mut() = true;
                    }
                }
                ASTBody::Symbol { ns, name: "b" } => {
                    if !*first_b_visited.borrow() {
                        *first_b_visited.borrow_mut() = true;
                        return;
                    } else {
                        assert!(if let Some(binded) =
                            analysis.borrow().context.borrow().find_var("b")
                        {
                            unsafe {
                                assert_eq!(
                                    binded,
                                    Binding::Normal {
                                        bound_by: &AST {
                                            pos: Span::new_from_raw_offset(38, 2, "b", ()),
                                            body: ASTBody::Symbol {
                                                ns: None,
                                                name: "b"
                                            }
                                        },
                                        value: &AST {
                                            pos: Span::new_from_raw_offset(40, 2, "\"hello\"", ()),
                                            body: ASTBody::StringLiteral("hello"),
                                        }
                                    }
                                )
                            }
                            true
                        } else {
                            false
                        });
                        *second_b_visited.borrow_mut() = true;
                    }
                }
                _ => (),
            },
            do_nothing_on_end,
        );
        assert!(*first_a_visited.borrow());
        assert!(*second_a_visited.borrow());
        assert!(*first_b_visited.borrow());
        assert!(*second_b_visited.borrow());
    }
    #[test]
    fn analyze_let_bindings_destructing_case_test() {
        let source = "(when-let [{:as a b :key :keys [:c :d]} values]
                        (str a b c d))";
        let (s, root) = parse_source(source.into()).unwrap();
        let mut first_a_visited = RefCell::new(false);
        let mut second_a_visited = RefCell::new(false);
        let mut first_b_visited = RefCell::new(false);
        let mut second_b_visited = RefCell::new(false);
        let mut c_visited = RefCell::new(false);
        let mut d_visited = RefCell::new(false);

        unsafe {
            let values_ast = AST {
                pos: Span::new_from_raw_offset(40, 1, "values", ()),
                body: ASTBody::Symbol {
                    ns: None,
                    name: "values",
                },
            };
            visit_ast_with_analyzing(
                "src/sample.clj",
                &root,
                &|ast, analysis| match ast.body {
                    ASTBody::Symbol { ns, name: "a" } => {
                        if !*first_a_visited.borrow() {
                            *first_a_visited.borrow_mut() = true;
                            return;
                        } else {
                            assert!(if let Some(binded) =
                                analysis.borrow().context.borrow().find_var("a")
                            {
                                unsafe {
                                    assert_eq!(
                                        binded,
                                        Binding::Normal {
                                            bound_by: &AST {
                                                pos: Span::new_from_raw_offset(16, 1, "a", ()),
                                                body: ASTBody::Symbol {
                                                    ns: None,
                                                    name: "a"
                                                }
                                            },
                                            value: &values_ast
                                        },
                                    )
                                }
                                true
                            } else {
                                false
                            });
                            *second_a_visited.borrow_mut() = true;
                        }
                    }
                    ASTBody::Symbol { ns, name: "b" } => {
                        if !*first_b_visited.borrow() {
                            *first_b_visited.borrow_mut() = true;
                            return;
                        } else {
                            assert!(if let Some(binded) =
                                analysis.borrow().context.borrow().find_var("b")
                            {
                                assert_eq!(
                                    binded,
                                    Binding::Destructing {
                                        bound_to: &AST {
                                            pos: Span::new_from_raw_offset(18, 1, "b", ()),
                                            body: ASTBody::Symbol {
                                                ns: None,
                                                name: "b"
                                            }
                                        },
                                        key: "key",
                                        map: &values_ast
                                    }
                                );
                                true
                            } else {
                                false
                            });
                            *second_b_visited.borrow_mut() = true;
                        }
                    }
                    ASTBody::Symbol { ns, name: "c" } => {
                        assert!(if let Some(binded) =
                            analysis.borrow().context.borrow().find_var("c")
                        {
                            assert_eq!(
                                binded,
                                Binding::Destructing {
                                    bound_to: &AST {
                                        pos: Span::new_from_raw_offset(32, 1, ":c", ()),
                                        body: ASTBody::Keyword {
                                            ns: None,
                                            name: "c"
                                        }
                                    },
                                    key: "c",
                                    map: &values_ast
                                }
                            );
                            true
                        } else {
                            false
                        });
                        *c_visited.borrow_mut() = true;
                    }
                    ASTBody::Symbol { ns, name: "d" } => {
                        assert!(if let Some(binded) =
                            analysis.borrow().context.borrow().find_var("d")
                        {
                            assert_eq!(
                                binded,
                                Binding::Destructing {
                                    bound_to: &AST {
                                        pos: Span::new_from_raw_offset(35, 1, ":d", ()),
                                        body: ASTBody::Keyword {
                                            ns: None,
                                            name: "d"
                                        }
                                    },
                                    key: "d",
                                    map: &values_ast
                                }
                            );
                            true
                        } else {
                            false
                        });
                        *d_visited.borrow_mut() = true;
                    }
                    _ => (),
                },
                do_nothing_on_end,
            );
            assert!(*first_a_visited.borrow());
            assert!(*second_a_visited.borrow());
            assert!(*first_b_visited.borrow());
            assert!(*second_b_visited.borrow());
            assert!(*c_visited.borrow());
            assert!(*d_visited.borrow());
        }
    }
}

use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;
use std::collections::HashSet;
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

#[derive(Debug, Clone, Copy)]
pub struct Binding<'a> {
    namespace: &'a str,
    name: &'a str,
    filename: &'a str,
    defined_by: &'a str,
    is_private: bool,
    bound_to: &'a AST<'a>,
    kind: BindingKind<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BindingKind<'a> {
    Normal { value: &'a AST<'a> },
    Destructing { key: &'a AST<'a>, map: &'a AST<'a> },
}

impl<'a> std::hash::Hash for Binding<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.namespace.hash(state);
        self.name.hash(state);
        self.bound_to.pos.hash(state);
    }
}

impl<'a> std::cmp::PartialEq for Binding<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.filename == other.filename && self.bound_to.pos == other.bound_to.pos
    }
}

impl<'a> std::cmp::Eq for Binding<'a> {}

pub enum Variable {}

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
    pub fn bind_var(&mut self, name: &'a str, binding: Binding<'a>) {
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name, binding);
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
    pub bindings: HashSet<Binding<'a>>,
    pub usages: HashMap<&'a AST<'a>, Binding<'a>>,
    pub context: Rc<RefCell<AnalysisContext<'a>>>,
}

impl<'a> Analysis<'a> {
    pub fn new() -> Self {
        Analysis {
            namespace_definitions: HashMap::new(),
            bindings: HashSet::new(),
            usages: HashMap::new(),
            context: Rc::new(RefCell::new(AnalysisContext {
                current_ns: "",
                env: Vec::new(),
            })),
        }
    }
}

type AnalysisCell<'a> = Rc<RefCell<Analysis<'a>>>;

#[rustfmt::skip]
fn analyze_var_definitions<'a>(filename: &'a str, ast: &'a AST<'a>, analysis: AnalysisCell<'a>) {
    let forms = if let ASTBody::List(forms) = &ast.body { forms } else { return; };
    let first = if let Some(first) = forms.get(0) { first } else { return; };
    let second = if let Some(second) = forms.get(1) { second } else { return; };
    let third = if let Some(third) = forms.get(2) { third } else { return; };
    let current_ns = analysis.borrow().context.borrow().current_ns;
    if let ASTBody::Symbol { ns, name: defined_by } = first.body {
        // TODO: make configurable
        if !defined_by.starts_with("def") { return; }
        if let ASTBody::Symbol { ns: qualified_ns, name: varname } = second.body {
            analysis.borrow_mut().bindings.insert(
                Binding {
                    filename,
                    defined_by,
                    name: varname,
                    namespace: qualified_ns.unwrap_or(current_ns),
                    is_private: true, // TODO: implement
                    bound_to: second,
                    kind: BindingKind::Normal { value: third }
                }
            );
        }
    }
}

// Returns if there is any bindings
// TODO: handle syntax errors.
#[rustfmt::skip]
fn analyze_let_bindings<'a>(filename: &'a str, ast: &'a AST<'a>, analysis: AnalysisCell<'a>) -> bool {
    let current_ns = analysis.borrow().context.borrow().current_ns;
    let forms = if let ASTBody::List(forms) = &ast.body { forms } else { return false; };
    let first = if let Some(first) = forms.get(0) { first } else { return false; };
    let second = if let Some(second) = forms.get(1) { second } else { return false; };
    if let ASTBody::Symbol { ns, name: defined_by } = &first.body {
        if *defined_by != "let" && *defined_by != "if-let" && *defined_by != "when-let" {
            return false;
        }
        if let ASTBody::Vector(forms) = &second.body {
            if forms.len() % 2 != 0 {
                println!("let bindings must have even number of forms. {}", ast.fragment());
                return false;
            }
    
            let new_scope = HashMap::new();
            analysis.borrow_mut().context.borrow_mut().push_env(new_scope);

            for binding in forms.chunks(2) {
                match &binding[0].body {
                    ASTBody::Symbol { ns, name } => {
                        if let Some(ns_name) = ns {
                            println!("Syntax error. Invalid var name {}/{}", ns.unwrap(), name);
                            return false;
                        } else {
                            let binding = Binding {
                                filename,
                                defined_by,
                                name,
                                namespace: current_ns,
                                is_private: true,
                                bound_to: &binding[0],
                                kind: BindingKind::Normal { value: &binding[1] }
                            };
                            analysis.borrow_mut().bindings.insert(binding);
                            analysis.borrow_mut().context.borrow_mut().bind_var(name, binding);
                        }
                    },
                    ASTBody::Map(kvs) => {
                        for (k, v) in kvs {
                            match k.body {
                                // ex) [{binded_symbol :key} values]
                                ASTBody::Symbol { ns, name: bind_name } => {
                                    let binding = Binding {
                                        filename,
                                        defined_by,
                                        name: bind_name,
                                        namespace: current_ns,
                                        is_private: true,
                                        bound_to: k,
                                        kind: BindingKind::Destructing { key: v, map: &binding[1] }
                                    };
                                    analysis.borrow_mut().bindings.insert(binding);
                                    analysis.borrow_mut().context.borrow_mut().bind_var(bind_name, binding);
                                },
                                // ex) [{:keys [:a :b :c]} values]
                                ASTBody::Keyword { ns, name: "keys" } => {
                                    if let ASTBody::Vector(keys) = &v.body {
                                        for key in keys {
                                            if let ASTBody::Keyword { ns, name: bind_name } = key.body {
                                                let binding = Binding {
                                                    filename,
                                                    defined_by,
                                                    name: bind_name,
                                                    namespace: current_ns,
                                                    is_private: true,
                                                    bound_to: key,
                                                    kind: BindingKind::Destructing { key, map: &binding[1] }
                                                };
                                                analysis.borrow_mut().bindings.insert(binding);
                                                analysis.borrow_mut().context.borrow_mut().bind_var(bind_name, binding);
                                            } else {
                                                println!("{} Syntax error. Expect keyword but found {}",key.pos, key.fragment());
                                                return false;
                                            }
                                        }
                                    } else {
                                        println!("{} Syntax error. Expect vector but found {}", v.pos, v.fragment());
                                        return false;
                                    }
                                },
                                // ex) [{:as context ...}]
                                ASTBody::Keyword { ns, name: "as" } => {
                                    if let ASTBody::Symbol { ns, name: bind_name } = v.body {
                                        let binding = Binding {
                                            filename,
                                            defined_by,
                                            name: bind_name,
                                            namespace: current_ns,
                                            is_private: true,
                                            bound_to: v,
                                            kind: BindingKind::Normal { value: &binding[1] }
                                        };
                                        analysis.borrow_mut().bindings.insert(binding);
                                        analysis.borrow_mut().context.borrow_mut().bind_var(bind_name, binding);
                                    } else {
                                        println!("{} Syntax error. Expect keyword but found {}", v.pos, v.fragment());
                                        return false;
                                    }
                                }
                                _ => {
                                    println!("{} Syntax error! Invalid binding {}", v.pos, k.fragment());
                                    return false;
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
        }
    }
    true
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
    on_visit: &impl Fn(&AST, &Analysis) -> (),
    on_scope_end: &impl Fn(&AST, &Analysis) -> (),
    analysis_cell: AnalysisCell<'a>,
) {
    match &ast.body {
        ASTBody::List(forms) => {
            analyze_ns_definitions(filename, ast, analysis_cell.clone());
            analyze_var_definitions(filename, ast, analysis_cell.clone());
            let is_scope = analyze_let_bindings(filename, ast, analysis_cell.clone());

            on_visit(ast, &analysis_cell.clone().borrow());

            for form in forms {
                _visit_ast_with_analyzing(
                    filename,
                    form,
                    on_visit,
                    on_scope_end,
                    analysis_cell.clone(),
                )
            }

            if is_scope {
                analysis_cell.borrow_mut().context.borrow_mut().pop_env();
            }
        }
        ASTBody::Root(forms)
        | ASTBody::List(forms)
        | ASTBody::Vector(forms)
        | ASTBody::Set(forms)
        | ASTBody::AnonymousFn(forms) => {
            on_visit(ast, analysis_cell.borrow().deref());
            for form in forms {
                _visit_ast_with_analyzing(
                    filename,
                    form,
                    on_visit,
                    on_scope_end,
                    analysis_cell.clone(),
                )
            }
        }
        ASTBody::Map(forms) => {
            on_visit(ast, analysis_cell.borrow().deref());
            for (k, v) in forms {
                _visit_ast_with_analyzing(
                    filename,
                    k,
                    on_visit,
                    on_scope_end,
                    analysis_cell.clone(),
                );
                _visit_ast_with_analyzing(
                    filename,
                    v,
                    on_visit,
                    on_scope_end,
                    analysis_cell.clone(),
                );
            }
        }
        ASTBody::Quote(form) | ASTBody::SyntaxQuote(form) | ASTBody::UnQuote(form) => {
            on_visit(ast, analysis_cell.borrow().deref());
            _visit_ast_with_analyzing(
                filename,
                form,
                on_visit,
                on_scope_end,
                analysis_cell.clone(),
            );
        }
        _ => {
            on_visit(ast, analysis_cell.borrow().deref());
        }
    };
}

pub struct VisitArgs<'a, V, S, A>
where
    V: Fn(&AST, &Analysis) -> (),
    S: Fn(&AST, &Analysis) -> (),
    A: Fn(&Analysis) -> (),
{
    pub filename: &'a str,
    pub ast: &'a AST<'a>,
    pub on_visit: V,
    pub on_scope_end: S,
    pub on_analysis_end: A,
}

pub fn visit_ast_with_analyzing<'a, V, S, A>(arg: VisitArgs<'a, V, S, A>)
where
    V: Fn(&AST, &Analysis) -> (),
    S: Fn(&AST, &Analysis) -> (),
    A: Fn(&Analysis) -> (),
{
    let mut analysis = Rc::new(RefCell::new(Analysis::new()));
    _visit_ast_with_analyzing(
        arg.filename,
        arg.ast,
        &arg.on_visit,
        &arg.on_scope_end,
        analysis.clone(),
    );
    let result = analysis.borrow();
    (arg.on_analysis_end)(result.deref());
}

#[cfg(test)]
mod tests {
    use crate::parser::*;
    use edn_rs::Map;
    use pretty_assertions::assert_eq;

    use super::*;

    fn do_nothing(ast: &AST, analysis: &Analysis) {}
    fn do_nothing_on_end(analysis: &Analysis) {}

    #[test]
    fn ananalyze_ns_definitions_test() {
        let (_, root) = parse_source(
            "(ns clj-analyzer.core
(:require [clojure.core :as core]))"
                .into(),
        )
        .unwrap();
        visit_ast_with_analyzing(VisitArgs {
            filename: "sample.clj",
            ast: &root,
            on_visit: do_nothing,
            on_scope_end: do_nothing,
            on_analysis_end: |a| {
                let definition = a.namespace_definitions.get("clj-analyzer.core").unwrap();
                assert_eq!(definition.filename, "sample.clj");
                assert_eq!(definition.name, "clj-analyzer.core");
                assert_eq!(a.context.borrow().current_ns, "clj-analyzer.core");
            },
        })
    }
    #[test] // Check if bound variables found in scope like 'let'
    fn analyze_var_definition_test() {
        let (_, root) =
            parse_source("(ns test.core) (def a 10) (defrecord b (+ 1 2))".into()).unwrap();
        visit_ast_with_analyzing(VisitArgs {
            filename: "src/sample.clj",
            ast: &root,
            on_visit: do_nothing,
            on_scope_end: do_nothing,
            on_analysis_end: |a: &Analysis| {
                let a_def = a.bindings.iter().find(|x| x.name == "a");
                assert!(a_def.is_some());
                let a_def = a_def.unwrap();
                assert_eq!(a_def.filename, "src/sample.clj");
                assert_eq!(a_def.defined_by, "def");
                assert_eq!(a_def.is_private, true);
                if let BindingKind::Normal { value: value_ast } = a_def.kind {
                    if let ASTBody::NumberLiteral(val) = &value_ast.body {
                        assert_eq!(*val, NumberLiteralValue::Integer(10))
                    } else {
                        println!("value bound to a is not number");
                        assert!(false);
                    }
                } else {
                    println!("symbol a is not Normal Binding");
                    assert!(false);
                }

                let b_def = a.bindings.iter().find(|x| x.name == "b");
                assert!(b_def.is_some());
                let b_def = b_def.unwrap();
                assert_eq!(b_def.name, "b");
                assert_eq!(b_def.defined_by, "defrecord");
            },
        });
    }
    #[test]
    fn analyze_let_binding_simple_case_test() {
        let source = "(let [a 1
                            b \"hello\"]
                        (+ a 1)
                        (not b))
                        end)";
        let (_, root) = parse_source(source.into()).unwrap();
        let mut first_a_visited = RefCell::new(false);
        let mut second_a_visited = RefCell::new(false);
        let mut first_b_visited = RefCell::new(false);
        let mut second_b_visited = RefCell::new(false);
        let mut end_visited = RefCell::new(false);
        visit_ast_with_analyzing(VisitArgs {
            filename: "src/sample.clj",
            ast: &root,
            on_visit: |ast, analysis| match ast.body {
                ASTBody::Symbol { ns, name: "a" } => {
                    if !*first_a_visited.borrow() {
                        *first_a_visited.borrow_mut() = true;
                        return;
                    } else {
                        if let Some(binded) = analysis.context.borrow().find_var("a") {
                            assert_eq!(binded.name, "a");
                            assert_eq!(binded.defined_by, "let");
                            if let BindingKind::Normal { value } = binded.kind {
                                if let ASTBody::NumberLiteral(n) = &value.body {
                                    assert_eq!(*n, NumberLiteralValue::Integer(1));
                                } else {
                                    println!("value's body should be ASTBody::NumberLiteral");
                                    assert!(false);
                                }
                            } else {
                                println!("kind should be BindingKind::Normal");
                                assert!(false);
                            }
                        } else {
                            println!("Cannot resolve 'a'");
                            assert!(false);
                        };
                        *second_a_visited.borrow_mut() = true;
                    }
                }
                ASTBody::Symbol { ns, name: "b" } => {
                    if !*first_b_visited.borrow() {
                        *first_b_visited.borrow_mut() = true;
                        return;
                    } else {
                        if let Some(binded) = analysis.context.borrow().find_var("b") {
                            assert_eq!(binded.name, "b");
                            assert_eq!(binded.defined_by, "let");
                            if let BindingKind::Normal { value } = binded.kind {
                                if let ASTBody::StringLiteral(s) = &value.body {
                                    assert_eq!(*s, "hello");
                                } else {
                                    println!("value's body should be ASTBody::NumberLiteral");
                                    assert!(false);
                                }
                            } else {
                                println!("kind should be BindingKind::Normal");
                                assert!(false);
                            }
                        } else {
                            println!("Cannot resolve 'b'");
                            assert!(false);
                        }
                        *second_b_visited.borrow_mut() = true;
                    }
                }
                ASTBody::Symbol { ns, name: "end" } => {
                    *end_visited.borrow_mut() = true;
                    assert!(analysis.context.borrow().find_var("a").is_none());
                    assert!(analysis.context.borrow().find_var("b").is_none());
                }
                _ => (),
            },
            on_analysis_end: do_nothing_on_end,
            on_scope_end: do_nothing,
        });
        assert!(*first_a_visited.borrow());
        assert!(*second_a_visited.borrow());
        assert!(*first_b_visited.borrow());
        assert!(*second_b_visited.borrow());
        assert!(*end_visited.borrow());
    }
    #[test]
    fn analyze_let_bindings_destructing_case_test() {
        let source = "(when-let [{:as a b :key :keys [:c :d]} values]
                        (str a b c d))
                        end";
        let (s, root) = parse_source(source.into()).unwrap();
        let mut first_a_visited = RefCell::new(false);
        let mut second_a_visited = RefCell::new(false);
        let mut first_b_visited = RefCell::new(false);
        let mut second_b_visited = RefCell::new(false);
        let mut c_visited = RefCell::new(false);
        let mut d_visited = RefCell::new(false);
        let mut end_visited = RefCell::new(false);

        unsafe {
            let values_ast = AST {
                pos: Span::new_from_raw_offset(40, 1, "values", ()),
                body: ASTBody::Symbol {
                    ns: None,
                    name: "values",
                },
            };
            visit_ast_with_analyzing(VisitArgs {
                filename: "src/sample.clj",
                ast: &root,
                on_visit: |ast, analysis| match ast.body {
                    ASTBody::Symbol { ns, name: "a" } => {
                        if !*first_a_visited.borrow() {
                            *first_a_visited.borrow_mut() = true;
                            return;
                        } else {
                            if let Some(binded) = analysis.context.borrow().find_var("a") {
                                assert!(binded.is_private);
                                assert_eq!(binded.name, "a");
                                assert_eq!(binded.defined_by, "when-let");
                                if let BindingKind::Destructing { key, map } = binded.kind {
                                    if let ASTBody::Symbol { ns, name } = key.body {
                                        assert_eq!(name, "a");
                                    } else {
                                        println!("a should be destructed by symbol 'a'");
                                        assert!(false);
                                    }
                                    if let ASTBody::Symbol { ns, name } = map.body {
                                        assert_eq!(name, "values");
                                    } else {
                                        println!("destructing target should be symbol 'values'");
                                        assert!(false);
                                    }
                                } else {
                                    println!("'a' should be destructed binding");
                                }
                            } else {
                                println!("Cannot resolve a");
                                assert!(false);
                            }
                            *second_a_visited.borrow_mut() = true;
                        }
                    }
                    ASTBody::Symbol { ns, name: "b" } => {
                        if !*first_b_visited.borrow() {
                            *first_b_visited.borrow_mut() = true;
                            return;
                        } else {
                            if let Some(binded) = analysis.context.borrow().find_var("b") {
                                assert!(binded.is_private);
                                assert_eq!(binded.name, "b");
                                assert_eq!(binded.defined_by, "when-let");
                                if let BindingKind::Destructing { key, map } = binded.kind {
                                    if let ASTBody::Keyword { ns, name } = key.body {
                                        assert_eq!(name, "key");
                                    } else {
                                        println!("b should be destructed by keyword ':key'");
                                        assert!(false);
                                    }
                                    if let ASTBody::Symbol { ns, name } = map.body {
                                        assert_eq!(name, "values");
                                    } else {
                                        println!("destructing target should be symbol 'values'");
                                        assert!(false);
                                    }
                                } else {
                                    println!("'b' should be destructed binding");
                                }
                            } else {
                                println!("Cannot resolve a");
                                assert!(false);
                            }
                            *second_b_visited.borrow_mut() = true;
                        }
                    }
                    ASTBody::Symbol { ns, name: "c" } => {
                        if let Some(binded) = analysis.context.borrow().find_var("c") {
                            assert!(binded.is_private);
                            assert_eq!(binded.name, "c");
                            assert_eq!(binded.defined_by, "when-let");
                            if let BindingKind::Destructing { key, map } = binded.kind {
                                if let ASTBody::Keyword { ns, name } = key.body {
                                    assert_eq!(name, "c");
                                } else {
                                    println!("c should be destructed by keyword ':c'");
                                    assert!(false);
                                }
                                if let ASTBody::Symbol { ns, name } = map.body {
                                    assert_eq!(name, "values");
                                } else {
                                    println!("destructing target should be symbol 'values'");
                                    assert!(false);
                                }
                            } else {
                                println!("'c' should be destructed binding");
                            }
                        } else {
                            println!("Cannot resolve c");
                            assert!(false);
                        }
                        *c_visited.borrow_mut() = true;
                    }
                    ASTBody::Symbol { ns, name: "d" } => {
                        if let Some(binded) = analysis.context.borrow().find_var("d") {
                            assert!(binded.is_private);
                            assert_eq!(binded.name, "d");
                            assert_eq!(binded.defined_by, "when-let");
                            if let BindingKind::Destructing { key, map } = binded.kind {
                                if let ASTBody::Keyword { ns, name } = key.body {
                                    assert_eq!(name, "d");
                                } else {
                                    println!("d should be destructed by keyword ':d'");
                                    assert!(false);
                                }
                                if let ASTBody::Symbol { ns, name } = map.body {
                                    assert_eq!(name, "values");
                                } else {
                                    println!("destructing target should be symbol 'values'");
                                    assert!(false);
                                }
                            } else {
                                println!("'d' should be destructed binding");
                            }
                        } else {
                            println!("Cannot resolve d");
                            assert!(false);
                        }
                        *d_visited.borrow_mut() = true;
                    }
                    ASTBody::Symbol { ns, name: "end" } => {
                        *end_visited.borrow_mut() = true;
                        assert!(analysis.context.borrow().find_var("a").is_none());
                        assert!(analysis.context.borrow().find_var("b").is_none());
                        assert!(analysis.context.borrow().find_var("c").is_none());
                        assert!(analysis.context.borrow().find_var("d").is_none());
                    }
                    _ => (),
                },
                on_analysis_end: do_nothing_on_end,
                on_scope_end: do_nothing,
            });
            assert!(*first_a_visited.borrow());
            assert!(*second_a_visited.borrow());
            assert!(*first_b_visited.borrow());
            assert!(*second_b_visited.borrow());
            assert!(*c_visited.borrow());
            assert!(*d_visited.borrow());
            assert!(*end_visited.borrow());
        }
    }
}

use std::{
    borrow::BorrowMut,
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::{parser::AST, Span};

#[derive(Debug, PartialEq, Eq)]
pub struct NamespaceDef<'a> {
    pub location: Span<'a>,
    pub name: &'a str,
    pub filename: &'a str,
}

#[derive(Debug, Clone, Copy)]
pub struct Binding<'a> {
    pub namespace: &'a str,
    pub name: &'a str,
    pub filename: &'a str,
    pub defined_by: &'a str,
    pub is_private: bool,
    pub bound_to: &'a AST<'a>,
    pub kind: BindingKind<'a>,
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

#[derive(Debug)]
pub struct Analysis<'a> {
    pub namespace_definitions: HashMap<&'a str, NamespaceDef<'a>>,
    pub bindings: HashSet<Binding<'a>>,
    pub usages: HashMap<&'a AST<'a>, Binding<'a>>,
    context: Rc<RefCell<AnalysisContext<'a>>>,
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
    // ctx_ is original naming convention,
    // which indicates that the method will mutate or get 'context'
    // which can have various value depending on where it is called.
    pub(super) fn ctx_in_ns(&mut self, ns: &'a str) {
        self.context.deref().borrow_mut().current_ns = ns;
    }
    pub(super) fn ctx_get_current_ns(&self) -> &'a str {
        self.context.deref().borrow_mut().current_ns
    }
    pub(super) fn ctx_pop_env(&mut self) {
        self.context.deref().borrow_mut().env.pop();
    }
    pub(super) fn ctx_push_env(&mut self, scope_env: HashMap<&'a str, Binding<'a>>) {
        self.context.deref().borrow_mut().env.push(scope_env);
    }
    pub(super) fn ctx_bind_var(&mut self, name: &'a str, binding: Binding<'a>) {
        if let Some(scope) = self.context.deref().borrow_mut().env.last_mut() {
            scope.insert(name, binding);
        }
    }
    pub(crate) fn ctx_find_var(&self, name: &str) -> Option<Binding> {
        self.context
            .deref()
            .borrow_mut()
            .env
            .iter()
            .rev()
            .map(|scope| scope.get(name).map_or(None, |x| Some(*x)))
            .find(|x| x.is_some())
            .unwrap_or(None)
    }
}

pub type AnalysisCell<'a> = Rc<RefCell<Analysis<'a>>>;

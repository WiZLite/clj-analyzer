use super::SemanticRule;

pub(super) struct UnusedLocalVatiables {}

impl SemanticRule for UnusedLocalVatiables {
    fn new(config: edn_rs::Edn) -> Box<Self> {
        Box::new(UnusedLocalVatiables {})
    }

    fn on_scope_end(
        &self,
        scope_ast: &crate::parser::AST,
        analysis: &crate::analyzer::Analysis,
        emit_message: &impl Fn(&crate::parser::AST, super::Severity, &str) -> (),
    ) {
    }
}

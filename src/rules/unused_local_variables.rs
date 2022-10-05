use super::SemanticRule;

pub(super) struct UnusedLocalVatiables {}

impl SemanticRule for UnusedLocalVatiables {
    fn new(config: edn_rs::Edn) -> Box<Self> {
        Box::new(UnusedLocalVatiables {})
    }

    fn on_scope_end(analysis: &crate::analyzer::Analysis) {
        todo!()
    }
}

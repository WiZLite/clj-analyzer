use crate::parser::ASTBody;
use crate::parser::AST;

pub fn visit_ast(ast: &AST, effect: &impl Fn(&AST) -> ()) {
    match &ast.body {
        ASTBody::Symbol { ns, name } => effect(&ast),
        ASTBody::Keyword { ns, name } => effect(&ast),
        ASTBody::NumberLiteral(_) => effect(&ast),
        ASTBody::StringLiteral(_) => effect(&ast),
        ASTBody::List(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(form, effect)
            }
        }
        ASTBody::Vector(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(form, effect)
            }
        }
        ASTBody::Set(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(form, effect);
            }
        }
        ASTBody::Map(forms) => {
            effect(&ast);
            for (k, v) in forms {
                visit_ast(k, effect);
                visit_ast(v, effect);
            }
        }
        ASTBody::AnonymousFn(forms) => {
            effect(&ast);
            for form in forms {
                visit_ast(form, effect)
            }
        }
        ASTBody::Quote(form) => {
            effect(&ast);
            visit_ast(form, effect);
        },
        ASTBody::SyntaxQuote(form) => {
            effect(&ast);
            visit_ast(form, effect);
        }
        ASTBody::UnQuote(_) => {
            effect(&ast);
            visit_ast(ast, effect);
        }
    };
}

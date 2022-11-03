use yaipl::{
    frontend::{
        parser::{ast::Ast, ParseError, Parser},
        semantic_analysis::AnnotatedAst,
    },
    utils::interner::Interner,
};

fn compile(src: &str) -> Result<Ast, ParseError> {
    let interner = Interner::new();
    Parser::new(src, &interner)
        .parse_root_module()
        .map(|root| Ast::new(root, interner))
}

#[test]
fn assert_compile() {}
#[test]
fn assert_compile_faliure() {}

use crate::evaluator::SymbolTable;

use super::parser::ast::Ast;
pub struct AnnotatedAst {
    pub ast: Ast,
    pub table: SymbolTable,
}

impl AnnotatedAst {
    fn new(ast: Ast) -> Self {
        Self {
            ast,
            table: SymbolTable::new(),
        }
    }
}
// struct VaribleRenamer {}

// impl VaribleRenamer {
//     // pub fn rename(&mut self, module);
// }

// impl VaribleRenamer {
//     fn new() -> Self {
//         Self {
//             table: SymbolTable::new(),
//         }
//     }
// }

impl Ast {
    pub fn annotate(self) -> AnnotatedAst {
        let mut symbols = SymbolTable::new();
        symbols.regester_top_level(&self.root);

        AnnotatedAst {
            ast: self,
            table: symbols,
        }
    }
}

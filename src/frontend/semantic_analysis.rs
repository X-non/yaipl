use std::collections::HashMap;

use crate::utils::interner::branded::Identifier;

use super::parser::ast::{Ast, FnDecl, ItemKind, Module};
pub struct AnnotatedAst {
    pub ast: Ast,
    pub table: SymbolTable,
}

impl AnnotatedAst {
    fn unannotated(ast: Ast) -> Self {
        Self {
            ast,
            table: SymbolTable::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DefPlace;
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    String,
    Float,
    Int,
    Unit,
}

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    Type { def: DefPlace, kind: Type },
    Variable { def: DefPlace, is_def: bool },
    Func { def: FnDecl },
}

#[derive(Debug)]
pub struct SymbolTable {
    table: HashMap<Identifier, SymbolEntry>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub(crate) fn regester_top_level(&mut self, module: &Module) {
        for item in &module.items {
            self.regester_item(item);
        }
    }

    fn regester_item(&mut self, item: &ItemKind) {
        match item {
            ItemKind::FnDecl(decl @ FnDecl { name, .. }) => {
                let decl = SymbolEntry::Func { def: decl.clone() };
                self.table.insert(*name, decl);
            }
        }
    }

    pub fn get(&self, ident: Identifier) -> Option<&SymbolEntry> {
        self.table.get(&ident)
    }
}

impl Ast {
    pub fn annotate(self) -> AnnotatedAst {
        let mut unannotated = AnnotatedAst::unannotated(self);

        unannotated.table.regester_top_level(&unannotated.ast.root);
        unannotated
    }
}

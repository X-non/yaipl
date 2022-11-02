use std::collections::HashMap;

use crate::utils::interner::Ident;

use super::parser::ast::{Ast, FnDecl, Item, Module};
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

#[derive(Debug)]
pub struct DefPlace;
#[derive(Debug)]
pub enum Type {
    Bool,
}

#[derive(Debug)]
pub enum SymbolEntry {
    Type { def: DefPlace, kind: Type },
    Variable { def: DefPlace, is_def: bool },
    Func { def: FnDecl },
}

#[derive(Debug)]
pub struct SymbolTable {
    table: HashMap<Ident, SymbolEntry>,
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

    fn regester_item(&mut self, item: &Item) {
        match item {
            Item::FnDecl(decl @ FnDecl { name, .. }) => {
                let decl = SymbolEntry::Func { def: decl.clone() };
                self.table.insert(*name, decl);
            }
        }
    }

    pub fn get(&self, ident: Ident) -> Option<&SymbolEntry> {
        self.table.get(&ident)
    }
}

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

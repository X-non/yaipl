mod evaluatable;
pub use self::evaluatable::Evaluatable;
use std::collections::HashMap;

use crate::{
    frontend::parser::ast::{Block, FnDecl, Item, Module},
    utils::interner::{Ident, Interner},
};

pub fn interpret(root: Module, idents: Interner) {
    Interpreter::new(root, idents).run()
}

#[derive(Debug)]
pub enum RuntimeError {}

#[derive(Debug)]
struct DefPlace;
#[derive(Debug)]
struct TypeKind;

#[derive(Debug)]
enum SymbolEntry {
    Type { def: DefPlace, kind: TypeKind },
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

    fn get(&self, ident: Ident) -> Option<&SymbolEntry> {
        self.table.get(&ident)
    }
}

#[allow(dead_code)]
pub struct Interpreter {
    root: Module,
    idents: Interner,
    symbol_table: SymbolTable,
}

impl Interpreter {
    fn new(root: Module, idents: Interner) -> Self {
        // Self { root, idents , };
        todo!()
    }

    fn run(&mut self) {
        // let items = &self.root.items;
        let main_fn = self.lookup_str("main").unwrap();
        if let SymbolEntry::Func { def: decl } = main_fn {
            // self.interpret(&decl.block);
        } else {
            panic!("No main function")
        }
    }

    fn lookup(&self, ident: Ident) -> Option<&SymbolEntry> {
        self.symbol_table.get(ident)
    }

    fn lookup_str(&self, name: &str) -> Option<&SymbolEntry> {
        let ident = self.idents.get_ident(name)?;
        self.lookup(ident)
    }

    fn interpret_block(&self, block: &Block) -> Result<(), RuntimeError> {
        for stmt in block.stmts() {}
        Ok(())
    }

    fn evaluate<I>(&mut self, bla: I) -> Result<I::Value, I::Error>
    where
        I: Evaluatable,
    {
        bla.eval(self)
    }
}

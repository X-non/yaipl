use std::ops::Range;

use crate::utils::interner::{Ident, Interner};

pub struct Ast {
    pub root: Module,
    pub interner: Interner,
}

impl Ast {
    pub fn new(root: Module, interner: Interner) -> Self {
        Self { root, interner }
    }
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(u64),
    Float(f64),
    String(Range<usize>),
    Variable(Ident),
}

#[derive(Debug, Clone)]
pub struct IfBranch {
    pub condition: Expr,
    pub block: Block,
}
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn stmts(&self) -> &[Stmt] {
        self.stmts.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct IfBranchSet {
    pub if_branch: IfBranch,
    pub else_if_branches: Vec<IfBranch>,
    pub else_block: Option<Block>,
}

#[derive(Debug)]
pub enum Item {
    FnDecl(FnDecl),
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name: Ident,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct VaribleDecl {
    pub name: Ident,
    pub intializer: Expr,
}
#[derive(Debug, Clone)]
pub enum Stmt {
    If(IfBranchSet),
    Block(Block),
    VaribleDecl(VaribleDecl),
}

impl Stmt {
    pub(crate) fn needed_semi_colon(&self) -> bool {
        match self {
            Stmt::VaribleDecl(_) => true,
            _ => false,
        }
    }
}

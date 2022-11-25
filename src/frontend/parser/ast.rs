use std::ops::Range;

use crate::utils::{
    interner::{
        branded::{Ident, Identifier, StrLiteral},
        Interned, Interner,
    },
    smallvec::SmallVec,
};
pub struct NodeId(u32);

impl NodeId {
    pub const DUMMY: Self = NodeId(u32::MAX);
}

#[derive(Debug)]
pub struct Ast {
    pub root: Module,
    pub identifiers: Interner<Ident>,
    pub strings: Interner<StrLiteral>,
}

impl Ast {
    pub fn new(root: Module, identifiers: Interner<Ident>, strings: Interner<StrLiteral>) -> Self {
        Self {
            root,
            identifiers,
            strings,
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct FnArguments {
    pub arguments: SmallVec<Expr, 5>,
}

impl FnArguments {
    pub fn new(arguments: SmallVec<Expr, 5>) -> Self {
        Self { arguments }
    }
    pub fn empty() -> Self {
        Self {
            arguments: Default::default(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct FnCall {
    pub callee: Expr,
    pub arguments: FnArguments,
}
#[derive(Debug, Clone)]
pub enum Expr {
    Integer(u64),
    Float(f64),
    Bool(bool),
    String(Interned<StrLiteral>),
    FnCall(Box<FnCall>),
    Variable(Identifier),
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
    pub name: Identifier,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct VaribleDecl {
    pub name: Identifier,
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

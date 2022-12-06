use std::{ops::Range, rc::Rc};

use crate::{
    frontend::lexer::TokenKind,
    utils::{
        interner::{
            branded::{Ident, Identifier, StrLiteral},
            Interned, Interner,
        },
        smallvec::SmallVec,
    },
};
pub struct NodeId(u32);

impl NodeId {
    pub const DUMMY: Self = NodeId(u32::MAX);
}

#[derive(Debug)]
pub struct Ast {
    pub root: Module,
    pub identifiers: Rc<Interner<Ident>>,
    pub strings: Rc<Interner<StrLiteral>>,
}

impl Ast {
    pub fn new(root: Module, identifiers: Interner<Ident>, strings: Interner<StrLiteral>) -> Self {
        Self {
            root,
            identifiers: Rc::new(identifiers),
            strings: Rc::new(strings),
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
    Binary(Binary),
    String(Interned<StrLiteral>),
    FnCall(Box<FnCall>),
    Variable(Identifier),
    UnaryMinus(Box<Expr>),
}
#[derive(Debug, Clone)]
pub struct Binary {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
        }
    }
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        let op = match value {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Star => Self::Mul,
            TokenKind::Slash => Self::Div,
            _ => return Err(()),
        };
        Ok(op)
    }
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
    Expr(Expr),
}

impl Stmt {
    pub(crate) fn needs_semi_colon(&self) -> bool {
        match self {
            Stmt::If(_) | Stmt::Block(_) => false,
            _ => true,
        }
    }
}

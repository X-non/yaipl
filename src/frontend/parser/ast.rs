use std::{ops::Range, rc::Rc};

use crate::{
    frontend::{lexer::TokenKind, span::Span},
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
    pub items: Vec<ItemKind>,
}

#[derive(Debug, Clone)]
pub struct FnArguments {
    pub arguments: SmallVec<ExprKind, 5>,
}

impl FnArguments {
    pub fn new(arguments: SmallVec<ExprKind, 5>) -> Self {
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
    pub callee: ExprKind,
    pub arguments: FnArguments,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Integer(u64),
    Float(f64),
    Bool(bool),
    Binary(Binary),
    String(Interned<StrLiteral>),
    FnCall(Box<FnCall>),
    Variable(Identifier),
    UnaryMinus(Box<ExprKind>),
}
#[derive(Debug, Clone)]
pub struct Binary {
    pub op: BinaryOp,
    pub lhs: Box<ExprKind>,
    pub rhs: Box<ExprKind>,
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
    pub condition: ExprKind,
    pub block: Block,
}
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<StmtKind>,
}

impl Block {
    pub fn stmts(&self) -> &[StmtKind] {
        self.stmts.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct IfBranchSet {
    pub if_branch: IfBranch,
    pub else_if_branches: Vec<IfBranch>,
    pub else_block: Option<Block>,
}
#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
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
    pub intializer: ExprKind,
}
#[derive(Debug, Clone)]
struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}
#[derive(Debug, Clone)]
pub enum StmtKind {
    If(IfBranchSet),
    Block(Block),
    VaribleDecl(VaribleDecl),
    Expr(ExprKind),
}

impl StmtKind {
    pub(crate) fn needs_semi_colon(&self) -> bool {
        match self {
            StmtKind::If(_) | StmtKind::Block(_) => false,
            _ => true,
        }
    }
}

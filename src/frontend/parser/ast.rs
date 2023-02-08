use std::rc::Rc;

use crate::{
    frontend::{lexer::TokenKind, span::Span},
    utils::interner::{
        branded::{Ident, Identifier, StrLiteral},
        Interned, Interner,
    },
};

use super::ParsedList;

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
    pub fn new(
        root: Module,
        identifiers: Rc<Interner<Ident>>,
        strings: Rc<Interner<StrLiteral>>,
    ) -> Self {
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
    pub span: Option<Span>,
    pub arguments: Vec<Expr>,
}

impl FnArguments {
    pub fn new(span: Span, arguments: Vec<Expr>) -> Self {
        Self {
            span: Some(span),
            arguments,
        }
    }
    pub fn empty() -> Self {
        Self {
            span: None,
            arguments: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub callee: Expr,
    pub arguments: FnArguments,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub assignee: Expr,
    pub rhs: Expr,
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
    UnaryMinus(Box<Expr>),
}
#[derive(Debug, Clone)]
pub struct Binary {
    pub op_span: Span,
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn span(&self) -> Span {
        self.lhs.span.combine(self.rhs.span)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,

    And,
    Or,
    Xor,

    Equals,
    NotEquals,

    LessThan,
    LessThanOrEqual,

    GreaterThan,
    GreaterThanOrEqual,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",

            BinaryOp::And => "and",
            BinaryOp::Or => "or",
            BinaryOp::Xor => "xor",
            BinaryOp::Equals => "==",
            BinaryOp::NotEquals => "!=",
            BinaryOp::LessThan => "<",
            BinaryOp::LessThanOrEqual => "<=",
            BinaryOp::GreaterThan => ">",
            BinaryOp::GreaterThanOrEqual => ">=",
        };

        write!(f, "{symbol}")
    }
}

impl TryFrom<&TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
        let op = match value {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Star => Self::Mul,
            TokenKind::Slash => Self::Div,
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            TokenKind::Xor => Self::Xor,
            TokenKind::DoubleEquals => Self::Equals,
            TokenKind::BangEquals => Self::NotEquals,
            TokenKind::LessThan => Self::LessThan,
            TokenKind::LessThanOrEqual => Self::LessThanOrEqual,
            TokenKind::GreaterThan => Self::GreaterThan,
            TokenKind::GreaterThanOrEqual => Self::GreaterThanOrEqual,
            _ => return Err(()),
        };
        Ok(op)
    }
}
impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

#[derive(Debug, Clone)]
pub struct BlockWithCondition {
    pub span: Span,
    pub condition: Expr,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn stmts(&self) -> &[Stmt] {
        self.stmts.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct IfBranchSet {
    pub if_branch: BlockWithCondition,
    pub else_if_branches: Vec<BlockWithCondition>,
    pub else_block: Option<Block>,
}

impl IfBranchSet {
    pub fn span(&self) -> Span {
        let span = self.if_branch.span;
        if let Some(else_block) = &self.else_block {
            return span.combine(else_block.span);
        }

        if let Some(last_else_if) = self.else_if_branches.last() {
            return span.combine(last_else_if.span);
        }

        span
    }
}
#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ItemKind {
    FnDecl(Rc<FnDecl>),
}
#[derive(Debug, Clone)]
pub struct FnParameter {
    pub name: Identifier,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnParameters {
    pub span: Span,
    pub parameters: Vec<FnParameter>,
}
impl From<ParsedList<FnParameter>> for FnParameters {
    fn from(list: ParsedList<FnParameter>) -> Self {
        debug_assert!(list.start.is_open_paren());
        debug_assert!(list.end.is_closed_paren());
        let span = list.start.span.combine(list.end.span);
        let parameters = list.elements;
        Self { span, parameters }
    }
}
#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name_span: Span,
    pub name: Identifier,
    pub block: Block,
    pub parameters: FnParameters,
}

#[derive(Debug, Clone)]
pub struct VaribleDecl {
    pub name_span: Span,
    pub name: Identifier,
    pub intializer: Expr,
}
#[derive(Debug, Clone)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}
#[derive(Debug, Clone)]
pub enum StmtKind {
    If(IfBranchSet),
    Block(Block),
    Return(Expr),
    VaribleDecl(VaribleDecl),
    Assignment(Box<Assignment>),
    Expr(Expr),
    WhileLoop(WhileLoop),
}
#[derive(Debug, Clone)]
pub struct WhileLoop {
    pub condition: Expr,
    pub block: Block,
}
impl StmtKind {
    pub(crate) fn needs_semi_colon(&self) -> bool {
        match self {
            StmtKind::If(_) | StmtKind::Block(_) | StmtKind::WhileLoop(_) => false,
            _ => true,
        }
    }
}

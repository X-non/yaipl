use std::ops::Range;

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Expr {
    Integer(u64),
    Float(f64),
    String(Range<usize>),
}

#[derive(Debug)]
pub struct IfBranch {
    pub condition: Expr,
    pub block: Block,
}
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct IfBranchSet {
    pub if_branch: IfBranch,
    pub else_if_branches: Vec<IfBranch>,
    pub else_block: Option<Block>,
}
#[derive(Debug)]
pub enum Item {
    If(IfBranchSet),
}

#[derive(Debug)]
pub struct VaribleDecl {
    pub name: Range<usize>,
    pub intializer: Expr,
}
#[derive(Debug)]
pub enum Stmt {
    If(IfBranchSet),
    Block(Block),
    VaribleDecl(VaribleDecl),
}

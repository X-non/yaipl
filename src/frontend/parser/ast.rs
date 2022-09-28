#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Expr {
    Number(u64),
}

#[derive(Debug)]
pub struct IfBranch {
    pub condition: Expr,
    pub block: Block,
}
#[derive(Debug)]
pub struct Block;

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

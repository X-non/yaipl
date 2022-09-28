pub struct Module {
    pub items: Vec<Item>,
}

pub enum Expr {
    TODO,
}

pub struct IfBranch {
    pub condition: Expr,
    pub block: Block,
}

pub struct Block;
pub struct IfBranchSet {
    pub if_branch: IfBranch,
    pub else_if_branches: Vec<IfBranch>,
    pub else_block: Option<Block>,
}

pub enum Item {
    If(IfBranchSet),
}

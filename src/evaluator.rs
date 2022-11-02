mod evaluatable;
pub use self::evaluatable::Evaluatable;

use crate::{
    frontend::{
        parser::ast::{Block, Expr, IfBranchSet, Module, Stmt},
        semantic_analysis::{SymbolEntry, SymbolTable, Type},
    },
    utils::interner::{branded::Ident, Interned, Interner},
};

pub fn evaluate(root: Module, idents: Interner<Ident>) -> Result<(), RuntimeError> {
    Interpreter::new(root, idents).run()
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeMismatch { expected: Type, fount: Type },
    UndefVar(Interned<Ident>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    True,
    False,
    String,
}

#[allow(dead_code)]
pub struct Interpreter {
    root: Module,
    idents: Interner<Ident>,
    symbol_table: SymbolTable,
}

impl Interpreter {
    fn new(root: Module, idents: Interner<Ident>) -> Self {
        // Self { root, idents , };
        todo!()
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        // let items = &self.root.items;
        let main_fn = self.lookup_str("main").unwrap();
        if let SymbolEntry::Func { def: decl } = main_fn {
            decl.block.evaluate(self)?;
        } else {
            panic!("No main function")
        }

        Ok(())
    }

    fn lookup(&self, ident: Interned<Ident>) -> Option<&SymbolEntry> {
        self.symbol_table.get(ident)
    }

    fn lookup_str(&self, name: &str) -> Option<&SymbolEntry> {
        let ident = self.idents.get_ident(name)?;
        self.lookup(ident)
    }
}

impl Evaluatable for Block {
    fn evaluate(&self, context: &Interpreter) -> Result<Self::Value, RuntimeError> {
        for stmt in self.stmts() {
            stmt.evaluate(context)?
        }
        Ok(())
    }
}
impl Evaluatable for Stmt {
    fn evaluate(&self, context: &Interpreter) -> Result<Self::Value, RuntimeError> {
        match self {
            Stmt::If(set) => set.evaluate(context)?,
            Stmt::Block(block) => block.evaluate(context)?,
            Stmt::VaribleDecl(decl) => println!("{decl:?}"),
        }
        Ok(())
    }
}

impl Evaluatable for IfBranchSet {
    fn evaluate(&self, context: &Interpreter) -> Result<Self::Value, RuntimeError> {
        if self.if_branch.condition.evaluate(context)? == RuntimeValue::True {
            self.if_branch.block.evaluate(context)?;
        }
        for branch in &self.else_if_branches {
            if branch.condition.evaluate(context)? == RuntimeValue::True {
                self.if_branch.block.evaluate(context)?;
                return Ok(());
            }
        }
        if let Some(else_block) = &self.else_block {
            else_block.evaluate(context)?;
        }
        Ok(())
    }
}

impl Evaluatable for Expr {
    type Value = RuntimeValue;

    fn evaluate(&self, context: &Interpreter) -> Result<Self::Value, RuntimeError> {
        match self {
            &Expr::Integer(int) => todo!(),
            &Expr::Float(float) => todo!(),
            Expr::String(text) => todo!(),
            &Expr::Variable(name) => {
                match context.lookup(name).ok_or(RuntimeError::UndefVar(name))? {
                    SymbolEntry::Type { def, kind } => todo!(),
                    SymbolEntry::Func { def } => todo!(),
                    SymbolEntry::Variable { def, is_def } => {
                        if !*is_def {
                            return Err(RuntimeError::UndefVar(name));
                        }
                        Ok(todo!())
                    }
                }
            }
        }
    }
}

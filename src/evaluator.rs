mod evaluatable;
pub use self::evaluatable::Evaluatable;

use crate::{
    frontend::{
        parser::ast::{Block, Expr, IfBranch, IfBranchSet, Module, Stmt},
        semantic_analysis::{AnnotatedAst, SymbolEntry, SymbolTable, Type},
    },
    utils::interner::{branded::Ident, Interned, Interner},
};

pub fn evaluate(ast: AnnotatedAst) -> Result<(), RuntimeError> {
    Interpreter::new(ast).run()
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

impl RuntimeValue {
    fn assert_type(&self, ty: Type) -> Result<(), RuntimeError> {
        if self.ty() == ty {
            Ok(())
        } else {
            Err(RuntimeError::TypeMismatch {
                expected: ty,
                fount: self.ty(),
            })
        }
    }
    const fn ty(&self) -> Type {
        match self {
            RuntimeValue::True | RuntimeValue::False => Type::Bool,
            RuntimeValue::String => Type::String,
        }
    }
}

#[allow(dead_code)]
pub struct Interpreter {
    root: Module,
    idents: Interner<Ident>,
    symbol_table: SymbolTable,
}

impl Interpreter {
    fn new(ast: AnnotatedAst) -> Self {
        Self {
            root: ast.ast.root,
            idents: ast.ast.interner,
            symbol_table: ast.table,
        }
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
        self.if_branch.evaluate(context)?;

        for branch in &self.else_if_branches {
            branch.evaluate(context)?;
        }
        if let Some(else_block) = &self.else_block {
            else_block.evaluate(context)?;
        }
        Ok(())
    }
}

impl Evaluatable for IfBranch {
    fn evaluate(&self, context: &Interpreter) -> Result<(), RuntimeError> {
        let conditional = self.condition.evaluate(context)?;
        conditional.assert_type(Type::Bool)?;
        if conditional == RuntimeValue::True {
            self.block.evaluate(context)?;
        }
        Ok(())
    }

    type Error = RuntimeError;

    type Value = ();
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

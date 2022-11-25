mod evaluatable;
use std::collections::{hash_map::Entry, HashMap};

pub use self::evaluatable::Evaluatable;

use crate::{
    frontend::{
        parser::ast::{Block, Expr, IfBranch, IfBranchSet, Module, Stmt},
        semantic_analysis::{AnnotatedAst, SymbolEntry, SymbolTable, Type},
    },
    utils::interner::{
        branded::{Ident, Identifier, StrLiteral},
        Interned, Interner,
    },
};

pub fn evaluate(ast: AnnotatedAst) -> Result<(), RuntimeError> {
    Interpreter::new(ast).run()
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeMismatch { expected: Type, fount: Type },
    Undefined(Interned<Ident>),
    Overflow,
    Shadowed(Interned<Ident>),
    Undeclared(Interned<Ident>),
    CantCall(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    True,
    False,
    String(String),
    Float(f64),
    Int(i64),
}

impl TryFrom<u64> for RuntimeValue {
    type Error = RuntimeError;

    fn try_from(v: u64) -> Result<Self, Self::Error> {
        match v.try_into() {
            Ok(v) => Ok(Self::Int(v)),
            Err(_) => Err(RuntimeError::Overflow),
        }
    }
}

impl From<f64> for RuntimeValue {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}
impl From<bool> for RuntimeValue {
    fn from(value: bool) -> Self {
        match value {
            true => Self::True,
            false => Self::False,
        }
    }
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
            RuntimeValue::String(_) => Type::String,
            RuntimeValue::Float(_) => Type::Float,
            RuntimeValue::Int(_) => Type::Int,
        }
    }
}
struct Enviroment {
    map: HashMap<Identifier, Option<RuntimeValue>>,
}
impl Enviroment {
    fn new() -> Enviroment {
        Self {
            map: Default::default(),
        }
    }
}
#[allow(dead_code)]
pub struct Interpreter {
    root: Module,
    idents: Interner<Ident>,
    strings: Interner<StrLiteral>,
    symbol_table: SymbolTable,
    enviroment: Enviroment,
}

impl Interpreter {
    fn new(ast: AnnotatedAst) -> Self {
        Self {
            root: ast.ast.root,
            idents: ast.ast.identifiers,
            symbol_table: ast.table,
            enviroment: Enviroment::new(),
            strings: ast.ast.strings,
        }
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        // let items = &self.root.items;
        let main_fn = self.lookup_str("main").unwrap().clone();
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

    fn set(&mut self, name: Identifier, value: RuntimeValue) -> Result<(), RuntimeError> {
        let mut entry = match self.enviroment.map.entry(name) {
            Entry::Occupied(entry) => entry,
            Entry::Vacant(_) => return Err(RuntimeError::Undefined(name)),
        };

        entry.insert(Some(value));
        Ok(())
    }

    fn define(&mut self, name: Identifier) -> Result<&mut Option<RuntimeValue>, RuntimeError> {
        match self.enviroment.map.entry(name) {
            Entry::Occupied(_) => Err(RuntimeError::Shadowed(name)),
            Entry::Vacant(entry) => Ok(entry.insert(None)),
        }
    }
    fn def_set(&mut self, name: Identifier, value: RuntimeValue) -> Result<(), RuntimeError> {
        *self.define(name)? = Some(value);
        Ok(())
    }
}

impl Evaluatable for Block {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
        for stmt in self.stmts() {
            stmt.evaluate(context)?
        }
        Ok(())
    }
}
impl Evaluatable for Stmt {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
        match self {
            Stmt::If(set) => set.evaluate(context)?,
            Stmt::Block(block) => block.evaluate(context)?,
            Stmt::VaribleDecl(decl) => {
                eprintln!("{decl:?}");
                let initalizer = decl.intializer.evaluate(context)?;
                context.def_set(decl.name, initalizer)?;
            }
        }
        Ok(())
    }
}

impl Evaluatable for IfBranchSet {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
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
    fn evaluate(&self, context: &mut Interpreter) -> Result<(), RuntimeError> {
        let conditional = self.condition.evaluate(context)?;
        conditional.assert_type(Type::Bool)?;
        if conditional == RuntimeValue::True {
            self.block.evaluate(context)?;
        }
        Ok(())
    }
}

impl Evaluatable for Expr {
    type Value = RuntimeValue;

    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
        match self {
            &Expr::Integer(int) => Ok(int.try_into()?),
            &Expr::Float(float) => Ok(float.into()),
            &Expr::Bool(v) => Ok(v.into()),
            &Expr::String(text) => Ok(RuntimeValue::String(
                context.strings.lookup(text).to_string(),
            )),
            &Expr::Variable(name) => Ok(context
                .enviroment
                .map
                .get(&name)
                .ok_or(RuntimeError::Undeclared(name))? // not in enviorment
                .as_ref()
                .ok_or(RuntimeError::Undefined(name))? // varible in enviorment but not set
                .clone()),
            Expr::FnCall(call) => {
                //TODO FIX print hack;
                if let Expr::Variable(ident) = call.callee {
                    let callee_name = context.idents.lookup(ident);
                    if callee_name == "print" {
                        let formated: Result<Vec<_>, _> = call
                            .arguments
                            .arguments
                            .iter()
                            .map(|e| e.evaluate(context).map(|expr| format!("{:?}", expr)))
                            .collect();
                    }
                }
                return Err(RuntimeError::CantCall(call.callee.clone()));
            }
        }
    }
}

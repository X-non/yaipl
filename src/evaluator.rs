mod builtin;
mod evaluatable;
mod io_adaptor;
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Display,
};

pub use self::evaluatable::Evaluatable;
use self::io_adaptor::{IoAdaptor, StdIOAdaptor};

use crate::{
    frontend::{
        parser::ast::{Block, Expr, FnCall, IfBranch, IfBranchSet, Module, Stmt},
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
    Unit,
    True,
    False,
    String(String),
    Float(f64),
    Int(i64),
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Unit => write!(f, "Unit"),
            RuntimeValue::True => write!(f, "true"),
            RuntimeValue::False => write!(f, "false"),
            RuntimeValue::String(text) => write!(f, "{}", text),
            RuntimeValue::Float(val) => write!(f, "{}", val),
            RuntimeValue::Int(val) => write!(f, "{}", val),
        }
    }
}

impl From<()> for RuntimeValue {
    fn from(_: ()) -> Self {
        Self::Unit
    }
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
            RuntimeValue::Unit => Type::Unit,
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
    io_adaptor: Box<dyn IoAdaptor>,
    symbol_table: SymbolTable,
    enviroment: Enviroment,
}

impl Interpreter {
    fn new(ast: AnnotatedAst) -> Self {
        Self::with_io_adaptor(ast, Box::new(StdIOAdaptor::new()))
    }
    fn with_io_adaptor(ast: AnnotatedAst, io_adaptor: Box<dyn IoAdaptor>) -> Self {
        Self {
            root: ast.ast.root,
            idents: ast.ast.identifiers,
            symbol_table: ast.table,
            enviroment: Enviroment::new(),
            strings: ast.ast.strings,
            io_adaptor,
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
            Expr::FnCall(call) => call.evaluate(context),
        }
    }
}
impl Evaluatable for FnCall {
    type Value = RuntimeValue;

    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, Self::Error> {
        //TODO FIX print hack;
        if let Expr::Variable(ident) = self.callee {
            let callee_name = context.idents.lookup(ident);
            if callee_name == "print" {
                return builtin::print(&self.arguments, context).map(Into::into);
            } else {
                todo!("function lookup and call");
            }
        }
        return Err(RuntimeError::CantCall(self.callee.clone()));
    }
}

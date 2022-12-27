mod builtin;
mod evaluatable;
mod io_adaptor;
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Display,
    iter::once,
    rc::Rc,
};

pub use self::evaluatable::Evaluatable;
use self::io_adaptor::{IoAdaptor, StdIOAdaptor};

use crate::{
    frontend::{
        parser::ast::{
            BinaryOp, Block, BlockWithCondition, Expr, ExprKind, FnCall, IfBranchSet, Module, Stmt,
            StmtKind,
        },
        semantic_analysis::{AnnotatedAst, SymbolEntry, SymbolTable, Type},
        span::Span,
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
    BinaryTypeMissmatch(BinaryOp, Type, Type),
    TypeMismatch { expected: Type, fount: Type },
    Uninitalized(Interned<Ident>),
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
#[derive(Debug, Clone)]
struct Variable {
    decl_span: Span,
    kind: VariableKind,
}

impl Variable {
    pub fn assign(&mut self, value: RuntimeValue) {
        self.kind = VariableKind::Initialized(value);
    }
}

#[derive(Debug, Clone)]
enum VariableKind {
    Initialized(RuntimeValue),
    Uninitialized,
}
struct EnviormentFrame {
    map: HashMap<Identifier, Variable>,
}
impl EnviormentFrame {
    fn new() -> EnviormentFrame {
        Self {
            map: Default::default(),
        }
    }

    fn get(&self, name: Identifier) -> Option<&Variable> {
        self.map.get(&name)
    }

    fn get_mut(&mut self, name: Identifier) -> Option<&mut Variable> {
        self.map.get_mut(&name)
    }

    fn define_variable(&mut self, name: Identifier, variable_span: Span) -> Option<&mut Variable> {
        match self.map.entry(name) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => Some(entry.insert(Variable {
                decl_span: variable_span,
                kind: VariableKind::Uninitialized,
            })),
        }
    }
    fn set(&mut self, name: Identifier, value: RuntimeValue) -> Result<(), RuntimeError> {
        let a = self.get_mut(name).ok_or(RuntimeError::Undeclared(name))?;
        a.assign(value);
        Ok(())
    }
}
struct Enviorments {
    global: EnviormentFrame,
    stack: Vec<EnviormentFrame>,
}
#[allow(dead_code)]
impl Enviorments {
    fn new() -> Self {
        Self {
            stack: vec![EnviormentFrame::new()],
            global: EnviormentFrame::new(),
        }
    }
    pub fn scope_enter(&mut self) {
        self.stack.push(EnviormentFrame::new())
    }

    pub fn scope_exit(&mut self) {
        self.stack
            .pop()
            .expect("[Internal Iterpreter Error]: tried to pop the global enviorment");
    }
    fn resolve_iter_mut(&mut self) -> impl Iterator<Item = &mut EnviormentFrame> {
        self.stack.iter_mut().chain(once(&mut self.global))
    }
    fn resolve_iter(&self) -> impl Iterator<Item = &EnviormentFrame> {
        self.stack.iter().rev().chain(once(&self.global))
    }
    fn current_frame_mut(&mut self) -> &mut EnviormentFrame {
        self.stack.last_mut().unwrap_or(&mut self.global)
    }
    fn current_frame(&self) -> &EnviormentFrame {
        self.stack.last().unwrap_or(&self.global)
    }
    fn resolve(&self, name: Identifier) -> Result<RuntimeValue, RuntimeError> {
        self.resolve_iter()
            .find_map(|frame| frame.get(name))
            .ok_or(RuntimeError::Undeclared(name))
            .and_then(|variable| match &variable.kind {
                VariableKind::Initialized(value) => Ok(value.clone()),
                VariableKind::Uninitialized => Err(RuntimeError::Uninitalized(name)),
            })
    }
    fn define(&mut self, name: Identifier, variable_span: Span) -> Result<(), RuntimeError> {
        self.current_frame_mut()
            .define_variable(name, variable_span)
            .ok_or(RuntimeError::Shadowed(name))?;
        Ok(())
    }
    fn set(&mut self, name: Identifier, value: RuntimeValue) -> Result<(), RuntimeError> {
        self.current_frame_mut().set(name, value)
    }
}

#[allow(dead_code)]
pub struct Interpreter {
    root: Module,
    idents: Rc<Interner<Ident>>,
    strings: Rc<Interner<StrLiteral>>,
    io_adaptor: Box<dyn IoAdaptor>,
    symbol_table: SymbolTable,
    enviroment: Enviorments,
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
            enviroment: Enviorments::new(),
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
}

impl Evaluatable for Block {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
        context.enviroment.scope_enter();
        for stmt in self.stmts() {
            stmt.evaluate(context)?
        }
        context.enviroment.scope_exit();
        Ok(())
    }
}
impl Evaluatable for Stmt {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
        match &self.kind {
            StmtKind::If(set) => set.evaluate(context)?,
            StmtKind::Block(block) => block.evaluate(context)?,
            StmtKind::VaribleDecl(decl) => {
                let initalizer = decl.intializer.evaluate(context)?;
                context.enviroment.define(decl.name, decl.name_span)?;
                context.enviroment.set(decl.name, initalizer)?;
            }
            StmtKind::Expr(expr) => {
                expr.evaluate(context)?;
            }
            StmtKind::WhileLoop(while_loop) => loop {
                let conditon = while_loop.condition.evaluate(context)?;
                conditon.assert_type(Type::Bool)?;
                if conditon == RuntimeValue::False {
                    break;
                }
                while_loop.block.evaluate(context)?;
            },
            StmtKind::Assignment(_) => todo!(),
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

impl Evaluatable for BlockWithCondition {
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
        match &self.kind {
            ExprKind::Integer(int) => Ok((*int).try_into()?),
            ExprKind::Float(float) => Ok((*float).into()),
            ExprKind::Bool(v) => Ok((*v).into()),
            ExprKind::String(text) => Ok(RuntimeValue::String(
                context.strings.lookup(*text).to_string(),
            )),
            ExprKind::Variable(name) => context.enviroment.resolve(*name),
            ExprKind::FnCall(call) => call.evaluate(context),
            ExprKind::Binary(binary) => {
                let lhs = (&*binary.lhs).evaluate(context)?;
                let rhs = (&*binary.rhs).evaluate(context)?;
                let result = match binary.op {
                    BinaryOp::Add => match (lhs, rhs) {
                        (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => {
                            RuntimeValue::Int(lhs + rhs)
                        }
                        (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => {
                            RuntimeValue::Float(lhs + rhs)
                        }
                        (lhs, rhs) => {
                            return Err(RuntimeError::BinaryTypeMissmatch(
                                binary.op,
                                lhs.ty(),
                                rhs.ty(),
                            ))
                        }
                    },
                    BinaryOp::Sub => match (lhs, rhs) {
                        (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => {
                            RuntimeValue::Int(lhs + rhs)
                        }
                        (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => {
                            RuntimeValue::Float(lhs + rhs)
                        }
                        (lhs, rhs) => {
                            return Err(RuntimeError::BinaryTypeMissmatch(
                                binary.op,
                                lhs.ty(),
                                rhs.ty(),
                            ))
                        }
                    },
                    BinaryOp::Mul => match (lhs, rhs) {
                        (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => {
                            RuntimeValue::Int(lhs + rhs)
                        }
                        (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => {
                            RuntimeValue::Float(lhs + rhs)
                        }
                        (lhs, rhs) => {
                            return Err(RuntimeError::BinaryTypeMissmatch(
                                binary.op,
                                lhs.ty(),
                                rhs.ty(),
                            ))
                        }
                    },
                    BinaryOp::Div => match (lhs, rhs) {
                        (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => {
                            RuntimeValue::Int(lhs + rhs)
                        }

                        (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => {
                            RuntimeValue::Float(lhs + rhs)
                        }

                        (lhs, rhs) => {
                            return Err(RuntimeError::BinaryTypeMissmatch(
                                binary.op,
                                lhs.ty(),
                                rhs.ty(),
                            ))
                        }
                    },
                };
                Ok(result)
            }
            ExprKind::UnaryMinus(_) => todo!(),
        }
    }
}
impl Evaluatable for FnCall {
    type Value = RuntimeValue;

    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, Self::Error> {
        //FIXME print hack;
        if let ExprKind::Variable(ident) = self.callee.kind {
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

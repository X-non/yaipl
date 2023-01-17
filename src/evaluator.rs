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
            Binary, BinaryOp, Block, BlockWithCondition, Expr, ExprKind, FnCall, IfBranchSet,
            Module, Stmt, StmtKind,
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
    CantAssign(Expr),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RuntimeValue {
    Unit,
    Bool(bool),
    String(String),
    Float(f64),
    Int(i64),
}

impl RuntimeValue {
    const TRUE: Self = Self::Bool(true);
    const FALSE: Self = Self::Bool(false);

    pub fn same_type_as(&self, rhs: &Self) -> bool {
        self.ty() == rhs.ty()
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Unit => write!(f, "Unit"),
            RuntimeValue::Bool(val) => write!(f, "{}", val),
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
        Self::Bool(value)
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
            RuntimeValue::Bool(_) => Type::Bool,
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
    fn get(&self, name: Identifier) -> Result<RuntimeValue, RuntimeError> {
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
    fn resolve_mut(&mut self, name: Identifier) -> Option<&mut Variable> {
        self.resolve_iter_mut()
            .find_map(|frame| frame.get_mut(name))
    }
    fn set(&mut self, name: Identifier, value: RuntimeValue) -> Result<(), RuntimeError> {
        self.resolve_mut(name)
            .ok_or(RuntimeError::Undeclared(name))?
            .assign(value);

        Ok(())
    }
}

#[allow(dead_code)]
pub struct Interpreter {
    root: Module,
    idents: Rc<Interner<Ident>>,
    strings: Rc<Interner<StrLiteral>>,
    io_adaptor: Box<dyn IoAdaptor>,
    symbol_table: SymbolTable,
    enviroments: Enviorments,
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
            enviroments: Enviorments::new(),
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
        context.enviroments.scope_enter();
        for stmt in self.stmts() {
            stmt.evaluate(context)?
        }
        context.enviroments.scope_exit();
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
                context.enviroments.define(decl.name, decl.name_span)?;
                context.enviroments.set(decl.name, initalizer)?;
            }
            StmtKind::Expr(expr) => {
                expr.evaluate(context)?;
            }
            StmtKind::WhileLoop(while_loop) => loop {
                let conditon = while_loop.condition.evaluate(context)?;
                conditon.assert_type(Type::Bool)?;
                if conditon == RuntimeValue::FALSE {
                    break;
                }
                while_loop.block.evaluate(context)?;
            },
            StmtKind::Assignment(assignment) => {
                let ExprKind::Variable(name) = assignment.assignee.kind  else {
                    return Err(RuntimeError::CantAssign(assignment.assignee.clone()));
                };
                let value = assignment.rhs.evaluate(context)?;
                context.enviroments.set(name, value)?;
            }
        }
        Ok(())
    }
}

impl Evaluatable for IfBranchSet {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
        if let DidExecute::Yes = self.if_branch.evaluate(context)? {
            return Ok(());
        }

        for branch in &self.else_if_branches {
            if let DidExecute::Yes = branch.evaluate(context)? {
                return Ok(());
            }
        }
        if let Some(else_block) = &self.else_block {
            else_block.evaluate(context)?;
        }
        Ok(())
    }
}

pub enum DidExecute {
    Yes,
    No,
}
impl Evaluatable for BlockWithCondition {
    type Value = DidExecute;
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, RuntimeError> {
        let conditional = self.condition.evaluate(context)?;
        conditional.assert_type(Type::Bool)?;
        if conditional == RuntimeValue::TRUE {
            self.block.evaluate(context)?;
            Ok(DidExecute::Yes)
        } else {
            Ok(DidExecute::No)
        }
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
            ExprKind::Variable(name) => context.enviroments.get(*name),
            ExprKind::FnCall(call) => call.evaluate(context),
            ExprKind::Binary(binary) => binary.evaluate(context),
            ExprKind::UnaryMinus(_) => todo!(),
        }
    }
}

macro_rules! delagate {
    ($lhs:expr,$rhs:expr, $op:tt, $(RuntimeValue::$kind:ident),+) => {
        match ($lhs, $rhs) {
            $(
                (RuntimeValue::$kind(lhs), RuntimeValue::$kind(rhs))=> Some(RuntimeValue::$kind(lhs $op rhs)),
            )+
            _ => None,
        }
    };
}

impl Evaluatable for Binary {
    type Value = RuntimeValue;
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, Self::Error> {
        let lhs = (&*self.lhs).evaluate(context)?;
        let rhs = (&*self.rhs).evaluate(context)?;
        let lhs_ty = lhs.ty();
        let rhs_ty = rhs.ty();

        let result = match self.op {
            BinaryOp::Add => delagate!(lhs, rhs, +, RuntimeValue::Int, RuntimeValue::Float),
            BinaryOp::Sub => delagate!(lhs, rhs, -, RuntimeValue::Int, RuntimeValue::Float),
            BinaryOp::Mul => delagate!(lhs, rhs, *, RuntimeValue::Int, RuntimeValue::Float),
            BinaryOp::Div => delagate!(lhs, rhs, /, RuntimeValue::Int, RuntimeValue::Float),

            BinaryOp::And => delagate!(lhs, rhs, &&, RuntimeValue::Bool),
            BinaryOp::Or => delagate!(lhs, rhs, ||, RuntimeValue::Bool),
            BinaryOp::Xor => delagate!(lhs, rhs, ^, RuntimeValue::Bool),

            BinaryOp::Equals => lhs
                .same_type_as(&rhs)
                .then(move || RuntimeValue::Bool(lhs == rhs)),
            BinaryOp::NotEquals => lhs
                .same_type_as(&rhs)
                .then(move || RuntimeValue::Bool(lhs != rhs)),

            BinaryOp::LessThan => lhs
                .same_type_as(&rhs)
                .then(move || RuntimeValue::Bool(lhs < rhs)),
            BinaryOp::LessThanOrEqual => lhs
                .same_type_as(&rhs)
                .then(move || RuntimeValue::Bool(lhs <= rhs)),
            BinaryOp::GreaterThan => lhs
                .same_type_as(&rhs)
                .then(move || RuntimeValue::Bool(lhs > rhs)),
            BinaryOp::GreaterThanOrEqual => lhs
                .same_type_as(&rhs)
                .then(move || RuntimeValue::Bool(lhs >= rhs)),
        };
        result.ok_or_else(|| RuntimeError::BinaryTypeMissmatch(self.op, lhs_ty, rhs_ty))
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

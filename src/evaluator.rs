mod builtin;
mod evaluatable;
mod rt;

mod io_adaptor;
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use self::builtin::{BuiltinFunction, builtin_functions};
pub use self::evaluatable::Evaluatable;

use crate::{
    frontend::{
        parser::ast::{
            Binary, BinaryOp, Block, BlockWithCondition, Expr, ExprKind, FnArguments, FnCall,
            IfBranchSet, ItemKind, Module, Stmt, StmtKind,
        },
        semantic_analysis::AnnotatedAst,
        span::Span,
    },
    utils::interner::{
        branded::{Ident, Identifier, StrLiteral}, Interner,
    },
};

pub fn evaluate(ast: AnnotatedAst) -> Result<(), rt::Error> {
    Interpreter::new(ast).run()
}

#[derive(Debug, Clone)]
struct Variable {
    decl_span: Option<Span>,
    kind: VariableKind,
}

impl Variable {
    pub fn assign(&mut self, value: rt::Value) {
        self.kind = VariableKind::Initialized(value);
    }
    pub fn try_get_initialized(&self) -> Option<rt::Value> {
        match &self.kind {
            VariableKind::Initialized(value) => Some(value.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum VariableKind {
    Initialized(rt::Value),
    Uninitialized,
}
struct Enviorment {
    map: HashMap<Identifier, Variable>,
    parent: Option<SharedFrame>,
}
impl Enviorment {
    fn new() -> Enviorment {
        Self {
            map: Default::default(),
            parent: None,
        }
    }

    fn get(&self, name: Identifier) -> Option<rt::Value> {
        self.map
            .get(&name)
            .and_then(Variable::try_get_initialized)
            .or_else(|| {
                let parent_frame = self.parent()?.as_ref().borrow();
                parent_frame.get(name)
            })
    }

    fn assign(&mut self, name: Identifier, value: rt::Value) -> rt::Result<()> {
        if let Some(var) = self.map.get_mut(&name) {
            var.assign(value);
            return Ok(());
        }

        let Some(parent) = &self.parent else {
            return Err(rt::Error::Undeclared(name));
        };

        parent.borrow_mut().assign(name, value)
    }

    fn define(&mut self, name: Identifier, variable_span: Option<Span>) -> Option<&mut Variable> {
        match self.map.entry(name) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => Some(entry.insert(Variable {
                decl_span: variable_span,
                kind: VariableKind::Uninitialized,
            })),
        }
    }

    pub fn new_shared() -> SharedFrame {
        Rc::new(RefCell::new(Self::new()))
    }

    pub fn shared_with_parent(parent: Rc<RefCell<Enviorment>>) -> Rc<RefCell<Enviorment>> {
        let mut new = Self::new();
        new.parent = Some(parent);
        Rc::new(RefCell::new(new))
    }

    pub fn parent(&self) -> Option<&Rc<RefCell<Enviorment>>> {
        self.parent.as_ref()
    }
}
type SharedFrame = Rc<RefCell<Enviorment>>;

#[allow(dead_code)]
pub struct Interpreter {
    root: Module,
    idents: Rc<Interner<Ident>>,
    strings: Rc<Interner<StrLiteral>>,
    
    global_env: SharedFrame,
    current_env: SharedFrame,
    return_value: Option<rt::Value>,
}

impl Interpreter {
    fn new(ast: AnnotatedAst) -> Self {
        let global_env = Enviorment::new_shared();
        let mut interpreter = Self {
            root: ast.ast.root,
            idents: ast.ast.identifiers,

            strings: ast.ast.strings,
            current_env: Rc::clone(&global_env),
            global_env,
            return_value: None,
        };
        for func in builtin_functions(){
            let name = interpreter.idents.intern(func.name());
            interpreter.define(name, None).expect("[Internal Interpreter Error] Defining builtin functions shouldn't fail");
            interpreter.assign(name, rt::FnObject::Builtin(func).into()).expect("[Internal Interpreter Error] Defining builtin functions shouldn't fail");
        }

        interpreter
    }

    fn run(&mut self) -> Result<(), rt::Error> {
        let decls: Vec<_> = self
            .root
            .items
            .iter()
            .filter_map(|item| {
                #[allow(irrefutable_let_patterns)]
                if let ItemKind::FnDecl(decl) = &item.kind {
                    Some((decl.clone(), item.span))
                } else {
                    None
                }
            })
            .collect();
        for (decl, span) in decls {
            self.define(decl.name, Some(span))?;
            self.assign(decl.name, rt::FnObject::Yaipl(decl).into())?;
        }
        let main = self.idents.get_ident("main").ok_or(rt::Error::NoMainFunc)?;

        if let rt::Value::FnObject(fn_obj) = self.get(main).unwrap() {
            fn_obj.call(&FnArguments::empty(), self)?;
        } else {
            return Err(rt::Error::NoMainFunc);
        }
        Ok(())
    }

    pub fn scope_enter(&mut self) {
        let old_top = Rc::clone(&self.current_env);
        self.current_env = Enviorment::shared_with_parent(old_top);
    }

    pub fn scope_exit(&mut self) {
        let parent_of_current = self
            .current_env
            .as_ref()
            .borrow()
            .parent()
            .expect("[Internal Iterpreter Error]: tried to pop the parent enviorment")
            .clone();
        self.current_env = parent_of_current;
    }

    fn get(&self, name: Identifier) -> Result<rt::Value, rt::Error> {
        self.current_env
            .as_ref()
            .borrow()
            .get(name)
            .ok_or(rt::Error::Undeclared(name))
    }

    fn define(&mut self, name: Identifier, variable_span: Option<Span>) -> Result<(), rt::Error> {
        self.current_env
            .borrow_mut()
            .define(name, variable_span)
            .ok_or(rt::Error::Shadowed(name))?;
        Ok(())
    }

    fn assign(&mut self, name: Identifier, value: rt::Value) -> Result<(), rt::Error> {
        self.current_env.borrow_mut().assign(name, value)
    }

    fn set_return_value(&mut self, return_value: rt::Value) {
        self.return_value = Some(return_value);
    }

    fn has_return_value(&self) -> bool {
        self.return_value.is_some()
    }
}

impl Evaluatable for Block {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, rt::Error> {
        context.scope_enter();
        for stmt in self.stmts() {
            stmt.evaluate(context)?;
            if context.has_return_value() {
                break;
            }
        }
        context.scope_exit();
        Ok(())
    }
}
impl Evaluatable for Stmt {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, rt::Error> {
        match &self.kind {
            StmtKind::If(set) => {
                set.evaluate(context)?;
                if context.has_return_value() {
                    return Ok(());
                }
            }
            StmtKind::Block(block) => {
                block.evaluate(context)?;
                if context.has_return_value() {
                    return Ok(());
                }
            }
            StmtKind::VaribleDecl(decl) => {
                let initalizer = decl.intializer.evaluate(context)?;
                context.define(decl.name, Some(decl.name_span))?;
                context.assign(decl.name, initalizer)?;
            }
            StmtKind::Expr(expr) => {
                expr.evaluate(context)?;
            }
            StmtKind::WhileLoop(while_loop) => loop {
                let conditon = while_loop.condition.evaluate(context)?;
                conditon.assert_type(rt::Type::Bool)?;
                if conditon == rt::Value::FALSE {
                    break;
                }
                while_loop.block.evaluate(context)?;
                if context.has_return_value() {
                    return Ok(());
                }
            },
            StmtKind::Assignment(assignment) => {
                let ExprKind::Variable(name) = assignment.assignee.kind  else {
                    return Err(rt::Error::CantAssign(assignment.assignee.clone()));
                };
                let value = assignment.rhs.evaluate(context)?;
                context.assign(name, value)?;
            }
            StmtKind::Return(expr) => {
                let return_value = expr.evaluate(context)?;
                context.set_return_value(return_value);
                return Ok(());
            }
        }
        Ok(())
    }
}

impl Evaluatable for IfBranchSet {
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, rt::Error> {
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
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, rt::Error> {
        let conditional = self.condition.evaluate(context)?;
        conditional.assert_type(rt::Type::Bool)?;
        if conditional == rt::Value::TRUE {
            self.block.evaluate(context)?;
            Ok(DidExecute::Yes)
        } else {
            Ok(DidExecute::No)
        }
    }
}

impl Evaluatable for Expr {
    type Value = rt::Value;

    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, rt::Error> {
        match &self.kind {
            ExprKind::Integer(int) => Ok((*int).try_into()?),
            ExprKind::Float(float) => Ok((*float).into()),
            ExprKind::Bool(v) => Ok((*v).into()),
            ExprKind::String(text) => {
                Ok(rt::Value::String(context.strings.lookup(*text).to_string()))
            }
            ExprKind::Variable(name) => context.get(*name),
            ExprKind::FnCall(call) => call.evaluate(context),
            ExprKind::Binary(binary) => binary.evaluate(context),
            ExprKind::UnaryMinus(_) => todo!(),
        }
    }
}

macro_rules! delagate {
    ($lhs:expr,$rhs:expr, $op:tt, $(rt::Value::$kind:ident),+) => {
        match ($lhs, $rhs) {
            $(
                (rt::Value::$kind(lhs), rt::Value::$kind(rhs))=> Some(rt::Value::$kind(lhs $op rhs)),
            )+
            _ => None,
        }
    };
}

impl Evaluatable for Binary {
    type Value = rt::Value;
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, Self::Error> {
        let lhs = self.lhs.evaluate(context)?;
        let rhs = self.rhs.evaluate(context)?;
        let lhs_ty = lhs.ty();
        let rhs_ty = rhs.ty();

        let result = match self.op {
            BinaryOp::Add => {
                delagate!(lhs, rhs, +, rt::Value::Int, rt::Value::Float)
            }
            BinaryOp::Sub => {
                delagate!(lhs, rhs, -, rt::Value::Int, rt::Value::Float)
            }
            BinaryOp::Mul => {
                delagate!(lhs, rhs, *, rt::Value::Int, rt::Value::Float)
            }
            BinaryOp::Div => {
                delagate!(lhs, rhs, /, rt::Value::Int, rt::Value::Float)
            }

            BinaryOp::And => delagate!(lhs, rhs, &&, rt::Value::Bool),
            BinaryOp::Or => delagate!(lhs, rhs, ||, rt::Value::Bool),
            BinaryOp::Xor => delagate!(lhs, rhs, ^, rt::Value::Bool),

            BinaryOp::Equals => lhs
                .same_type_as(&rhs)
                .then(move || rt::Value::Bool(lhs == rhs)),
            BinaryOp::NotEquals => lhs
                .same_type_as(&rhs)
                .then(move || rt::Value::Bool(lhs != rhs)),

            BinaryOp::LessThan => lhs
                .same_type_as(&rhs)
                .then(move || rt::Value::Bool(lhs < rhs)),
            BinaryOp::LessThanOrEqual => lhs
                .same_type_as(&rhs)
                .then(move || rt::Value::Bool(lhs <= rhs)),
            BinaryOp::GreaterThan => lhs
                .same_type_as(&rhs)
                .then(move || rt::Value::Bool(lhs > rhs)),
            BinaryOp::GreaterThanOrEqual => lhs
                .same_type_as(&rhs)
                .then(move || rt::Value::Bool(lhs >= rhs)),
        };
        result.ok_or_else(|| rt::Error::BinaryTypeMissmatch(self.op, lhs_ty, rhs_ty))
    }
}

impl Evaluatable for FnCall {
    type Value = rt::Value;

    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, Self::Error> {
        let ExprKind::Variable(ident) = self.callee.kind else {
            return Err(rt::Error::CantCall(self.callee.clone()));
        };

        let rt::Value::FnObject(function) = context.get(ident)? else { 
            return Err(rt::Error::CantCall(self.callee.clone()))
        };

        function.call(&self.arguments, context)
    }
}

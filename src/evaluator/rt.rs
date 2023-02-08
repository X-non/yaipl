use std::{fmt::Display, mem, rc::Rc};

use crate::{
    frontend::parser::ast::{BinaryOp, Expr, FnArguments, FnDecl},
    utils::interner::branded::Identifier,
};

use super::{
    builtin::{self, BuiltinFunction},
    Enviorment, Evaluatable, Interpreter,
};
pub type Result<T = Value> = std::result::Result<T, Error>;
#[derive(Debug)]
pub enum Arity {
    Fixed(usize),
    Variable,
}
#[derive(Debug)]
pub enum Error {
    BinaryTypeMissmatch(BinaryOp, Type, Type),
    TypeMismatch { expected: Type, fount: Type },
    Uninitalized(Identifier),
    Overflow,
    Shadowed(Identifier),
    Undeclared(Identifier),
    CantCall(Expr),
    CantAssign(Expr),
    ArityError { expected: usize, found: usize },
    NoMainFunc,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    String,
    Float,
    Int,
    Unit,
    Func,
}
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Unit,
    Bool(bool),
    String(String),
    Float(f64),
    Int(i64),
    FnObject(FnObject),
}

impl From<FnObject> for Value {
    fn from(v: FnObject) -> Self {
        Self::FnObject(v)
    }
}

impl From<String> for Value {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

#[derive(Debug, Clone)]
pub enum FnObject {
    Yaipl(Rc<FnDecl>),
    Builtin(builtin::Function),
}

impl FnObject {
    pub fn arity(&self) -> Arity {
        match self {
            FnObject::Yaipl(fn_obj) => Arity::Fixed(fn_obj.parameters.parameters.len()),
            FnObject::Builtin(b) => b.arity(),
        }
    }
    pub(crate) fn call(&self, arguments: &FnArguments, context: &mut Interpreter) -> Result {
        if let Arity::Fixed(arity) = self.arity() {
            if arity != arguments.arguments.len() {
                return Err(Error::ArityError {
                    expected: arity,
                    found: arguments.arguments.len(),
                });
            }
        };
        match self {
            FnObject::Yaipl(fn_obj) => {
                let args: Result<Vec<_>> = arguments
                    .arguments
                    .iter()
                    .map(|expr| expr.evaluate(context))
                    .collect();

                let parameters = fn_obj.parameters.parameters.iter();

                let function_scope = Enviorment::shared_with_parent(context.global_env.clone());
                let old_scope = mem::replace(&mut context.current_env, function_scope);

                for (argument, param) in args?.into_iter().zip(parameters) {
                    context.define(param.name, Some(param.span)).expect(
                        "[Internal Interpreter Error] Defining a new parameter should never fail",
                    );
                    context.assign(param.name, argument).expect(
                        "[Internal Interpreter Error] Defining a new parameter should never fail",
                    );
                }

                fn_obj.block.evaluate(context)?;

                context.current_env = old_scope;
                let return_value = context.return_value.take().unwrap_or(Value::Unit);
                Ok(return_value)
            }
            FnObject::Builtin(b) => b.call(arguments, context),
        }
    }
}

impl Display for FnObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FnObject::Yaipl(decl) => write!(f, "<func {:?}>", decl.name), //todo resolve the namenn
            FnObject::Builtin(func) => write!(f, "<builtin func {}>", func.name()),
        }
    }
}

impl PartialEq for FnObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Builtin(lhs), Self::Builtin(rhs)) => lhs == rhs,
            (Self::Yaipl(lhs), Self::Yaipl(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}

impl PartialOrd for FnObject {
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl Value {
    pub const TRUE: Self = Self::Bool(true);
    pub const FALSE: Self = Self::Bool(false);

    pub fn same_type_as(&self, rhs: &Self) -> bool {
        self.ty() == rhs.ty()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "Unit"),
            Value::Bool(val) => write!(f, "{}", val),
            Value::String(text) => write!(f, "{}", text),
            Value::Float(val) => write!(f, "{}", val),
            Value::Int(val) => write!(f, "{}", val),
            Value::FnObject(fn_obj) => write!(f, "{}", fn_obj),
        }
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl TryFrom<u64> for Value {
    type Error = Error;

    fn try_from(v: u64) -> Result {
        match v.try_into() {
            Ok(v) => Ok(Self::Int(v)),
            Err(_) => Err(Error::Overflow),
        }
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl Value {
    pub fn assert_type(&self, ty: Type) -> Result<()> {
        if self.ty() == ty {
            Ok(())
        } else {
            Err(Error::TypeMismatch {
                expected: ty,
                fount: self.ty(),
            })
        }
    }
    pub const fn ty(&self) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::String(_) => Type::String,
            Value::Float(_) => Type::Float,
            Value::Int(_) => Type::Int,
            Value::Unit => Type::Unit,
            Value::FnObject(_) => Type::Func,
        }
    }
}

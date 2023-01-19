use std::fmt::Display;

use crate::{
    frontend::parser::ast::{BinaryOp, Expr},
    utils::interner::{branded::Identifier, Interned},
};

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
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    String,
    Float,
    Int,
    Unit,
}
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Unit,
    Bool(bool),
    String(String),
    Float(f64),
    Int(i64),
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

    fn try_from(v: u64) -> Result<Self, Self::Error> {
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
    pub fn assert_type(&self, ty: Type) -> Result<(), Error> {
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
        }
    }
}

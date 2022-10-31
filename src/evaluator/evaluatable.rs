use super::{Interpreter, RuntimeError};

pub trait Evaluatable
where
    Self::Error: Into<RuntimeError>,
{
    type Error;
    type Value;
    fn eval(&self, interpreter: &mut Interpreter) -> Result<Self::Value, Self::Error>;
}

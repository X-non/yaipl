use super::{Interpreter, RuntimeError};

pub trait Evaluatable
where
    Self::Error: Into<RuntimeError>,
{
    type Error = RuntimeError;
    type Value = ();
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, Self::Error>;
}

use super::{rt, Interpreter};

pub trait Evaluatable
where
    Self::Error: Into<rt::Error>,
{
    type Error = rt::Error;
    type Value = ();
    fn evaluate(&self, context: &mut Interpreter) -> Result<Self::Value, Self::Error>;
}

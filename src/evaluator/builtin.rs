use crate::frontend::parser::ast::FnArguments;

use super::{Evaluatable, Interpreter, RuntimeError};

pub fn print(arguments: &FnArguments, context: &mut Interpreter) -> Result<(), RuntimeError> {
    let formated: Result<Vec<String>, _> = arguments
        .arguments
        .iter()
        .map(|e| e.evaluate(context).map(|expr| format!("{:?}", expr)))
        .collect();
    let formated = formated?.join(" ");
    println!("{}", formated);
    Ok(())
}

use crate::frontend::parser::ast::FnArguments;

use super::{Evaluatable, Interpreter, RuntimeError};

pub fn print(arguments: &FnArguments, context: &mut Interpreter) -> Result<(), RuntimeError> {
    let formated: Result<Vec<String>, _> = arguments
        .arguments
        .iter()
        .map(|e| e.evaluate(context).map(|value| format!("{}", value)))
        .collect();
    let formated = formated?.join(" ");

    (writeln!(context.io_adaptor, "{}", formated)).unwrap();
    context.io_adaptor.flush().unwrap();
    Ok(())
}

use crate::frontend::parser::ast::FnArguments;

use super::{rt, Evaluatable, Interpreter};
pub trait BuiltinFn {
    const NAME: &'static str;
    type Value;
    type Error;
    fn call(arguments: &FnArguments, context: &mut Interpreter)
        -> Result<Self::Value, Self::Error>;
}

pub struct PrintLine;

impl BuiltinFn for PrintLine {
    const NAME: &'static str = "println";

    type Value = ();

    type Error = rt::Error;

    fn call(
        arguments: &FnArguments,
        context: &mut Interpreter,
    ) -> Result<Self::Value, Self::Error> {
        let context: &mut Interpreter = context;
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
}

fn print_line(arguments: &FnArguments, context: &mut Interpreter) -> Result<(), rt::Error> {
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

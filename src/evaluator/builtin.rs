use std::{collections::HashMap, io::stdin};

use enum_dispatch::enum_dispatch;

use crate::frontend::parser::ast::FnArguments;

use super::{rt, Evaluatable, Interpreter};
pub fn builtin_functions() -> HashMap<&'static str, Function> {
    [ReadLine.into(), PrintLine.into()]
        .into_iter()
        .map(|func: Function| (func.name(), func))
        .collect()
}
#[enum_dispatch(BuiltinFunction)]
pub enum Function {
    PrintLine,
    ReadLine,
}

#[enum_dispatch]
pub trait BuiltinFunction {
    fn name(&self) -> &'static str;

    fn call(
        &self,
        arguments: &FnArguments,
        context: &mut Interpreter,
    ) -> Result<rt::Value, rt::Error>;
}
pub struct ReadLine;

impl BuiltinFunction for ReadLine {
    fn name(&self) -> &'static str {
        todo!()
    }

    fn call(
        &self,
        arguments: &FnArguments,
        context: &mut Interpreter,
    ) -> Result<rt::Value, rt::Error> {
        let arity = arguments.arguments.len();
        if arity != 0 {
            return Err(rt::Error::ArityError {
                found: arity,
                expected: 0,
            });
        }

        let mut string = String::new();

        stdin().read_line(&mut string).unwrap();
        let trimmed = string.trim_end_matches("\r\n").trim_end_matches("\n");
        string.truncate(trimmed.len());
        Ok(string.into())
    }
}

pub struct PrintLine;

impl BuiltinFunction for PrintLine {
    fn name(&self) -> &'static str {
        "println"
    }
    fn call(
        &self,
        arguments: &FnArguments,
        context: &mut Interpreter,
    ) -> Result<rt::Value, rt::Error> {
        let context: &mut Interpreter = context;
        let formated: Result<Vec<String>, _> = arguments
            .arguments
            .iter()
            .map(|e| e.evaluate(context).map(|value| format!("{}", value)))
            .collect();
        let formated = formated?.join(" ");

        println!("{}", formated);
        Ok(rt::Value::Unit)
    }
}

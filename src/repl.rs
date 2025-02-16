use crate::environment::EnvironmentPtr;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use std::io::{stdout, Stdin, Write};

const PROMPT: &str = ">> ";

pub fn start(stdin: Stdin) {
    let env = EnvironmentPtr::new();
    let macro_env = EnvironmentPtr::new();

    loop {
        let mut buffer = String::new();

        print!("{}", PROMPT);
        // print! doesn't flush to stdout if it doesn't get \n, so do it manually
        stdout().flush().unwrap();

        stdin.read_line(&mut buffer).expect("Failed to read input");

        match execute(buffer, &env, &macro_env) {
            Ok(obj) => match obj {
                Object::Null => continue,
                other => println!("{}", other.inspect()),
            },
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}

pub fn execute(
    code: String,
    env: &EnvironmentPtr,
    macro_env: &EnvironmentPtr,
) -> Result<Object, String> {
    let lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();
    if !parser.errors.is_empty() {
        return Err(format!(
            "Whoops parser errors:\n {}",
            parser
                .errors
                .iter()
                .map(|e| format!("\t{}", e))
                .collect::<Vec<String>>()
                .join("\n")
        ));
    }

    let evaluator = Evaluator::new();
    evaluator.define_macros(&mut program, macro_env);
    let expanded_program = evaluator.expand_macros(program, macro_env);
    let evaluated = evaluator.eval(expanded_program, env);
    Ok(evaluated)
}

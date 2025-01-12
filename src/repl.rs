use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use std::io::{stdout, Stdin, Write};

const PROMPT: &str = ">> ";

pub fn start(stdin: Stdin) {
    let mut env = Environment::new();
    let mut macro_env = Environment::new();

    loop {
        let mut buffer = String::new();

        print!("{}", PROMPT);
        // print! doesn't flush to stdout if it doesn't get \n, so do it manually
        stdout().flush().unwrap();

        stdin.read_line(&mut buffer).expect("Failed to read input");

        let lexer = Lexer::new(buffer);
        let mut parser = Parser::new(lexer);
        let mut program = parser.parse_program();
        if parser.errors.len() != 0 {
            println!(
                "Whoops parser errors:\n {}",
                parser
                    .errors
                    .iter()
                    .map(|e| format!("\t{}", e))
                    .collect::<Vec<String>>()
                    .join("\n")
            );
            continue;
        }

        let evaluator = Evaluator::new();
        evaluator.define_macros(&mut program, &mut macro_env);
        let expanded_program = evaluator.expand_macros(program, &mut macro_env);
        let evaluated = evaluator.eval(expanded_program, &mut env);
        match evaluated {
            Object::Null => continue,
            _ => println!("{}", evaluated.inspect()),
        }
    }
}

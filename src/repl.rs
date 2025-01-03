use crate::environment::Environment;
use crate::evaluator::eval;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use std::io::{stdout, Stdin, Write};

const PROMPT: &str = ">> ";

pub fn start(stdin: Stdin) {
    let mut env = Environment::new();

    loop {
        let mut buffer = String::new();

        print!("{}", PROMPT);
        // print! doesn't flush to stdout if it doesn't get \n, so do it manually
        stdout().flush().unwrap();

        stdin.read_line(&mut buffer).expect("Failed to read input");

        let lexer = Lexer::new(buffer);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
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

        let evaluated = eval(program, &mut env);
        match evaluated {
            Object::Null => continue,
            _ => println!("{}", evaluated.inspect()),
        }
    }
}

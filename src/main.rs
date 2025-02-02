use environment::EnvironmentPtr;
use object::Object;
use std::{env, fs, io};

mod ast;
mod builtins;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        let file_path = &args[1];
        if let Ok(content) = fs::read_to_string(file_path) {
            return execute(content);
        } else {
            println!("The Monkey file {} was not found", file_path);
            return;
        }
    }
    println!("This is the Monkey programming language!");
    println!("feel free to type in commands");

    repl::start(io::stdin());
}

fn execute(content: String) {
    let env = EnvironmentPtr::new();
    let macro_env = EnvironmentPtr::new();
    match repl::execute(content, &env, &macro_env) {
        Ok(obj) => {
            if !matches!(obj, Object::Null) {
                println!("{}", obj.inspect());
            }
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}

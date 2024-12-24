use std::io;

mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    println!("This is the Monkey programming language!");
    println!("feel free to type in commands");

    repl::start(io::stdin());
}

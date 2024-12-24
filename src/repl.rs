use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::{stdout, Stdin, Write};

const PROMPT: &str = ">> ";

pub fn start(stdin: Stdin) {
    loop {
        let mut buffer = String::new();

        print!("{}", PROMPT);
        // print! doesn't flush to stdout if it doesn't get \n, so do it manually
        stdout().flush().unwrap();

        stdin.read_line(&mut buffer).expect("Failed to read input");

        let mut lexer = Lexer::new(buffer);
        let mut tok = lexer.next_token();
        while tok.token_type != TokenType::Eof {
            println!("{:?}", tok);
            tok = lexer.next_token();
        }
    }
}

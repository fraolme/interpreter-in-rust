use crate::token::{Token, TokenType};

struct Lexer {
    input: String,
    position: usize,      // points to current char
    read_position: usize, // points after current char
    ch: char,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };

        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        let tok = match self.ch {
            '=' => Token::new(TokenType::Assign, self.ch),
            ';' => Token::new(TokenType::SemiColon, self.ch),
            '(' => Token::new(TokenType::Lparen, self.ch),
            ')' => Token::new(TokenType::Rparen, self.ch),
            ',' => Token::new(TokenType::Comma, self.ch),
            '+' => Token::new(TokenType::Plus, self.ch),
            '{' => Token::new(TokenType::Lbrace, self.ch),
            '}' => Token::new(TokenType::Rbrace, self.ch),
            '\0' => Token {
                token_type: TokenType::Eof,
                literal: String::from(""),
            },
            _ => Token::new(TokenType::Illegal, self.ch),
        };

        self.read_char();
        tok
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");

        let tests: Vec<(TokenType, &str)> = vec![
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::Lparen, "("),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Rbrace, "}"),
            (TokenType::Comma, ","),
            (TokenType::SemiColon, ";"),
            (TokenType::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (index, (expected_type, expected_literal)) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            let tok_type = tok.token_type;
            let tok_literal = tok.literal;

            assert_eq!(
                &tok_type, expected_type,
                "tests[{index}] - tokentype wrong. expected={expected_type}, got={tok_type}"
            );

            assert_eq!(
                tok_literal,
                expected_literal.to_string(),
                "tests[{index}] - literal wrong. expected={expected_literal}, got={tok_literal}"
            );
        }
    }
}

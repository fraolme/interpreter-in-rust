use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,      // points to current char
    read_position: usize, // points after current char
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
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

    pub fn next_token(&mut self) -> Token {
        let mut pointer_moved = false;

        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        token_type: TokenType::Eq,
                        literal: String::from("=="),
                    }
                } else {
                    Token::new(TokenType::Assign, self.ch)
                }
            }
            ';' => Token::new(TokenType::SemiColon, self.ch),
            '(' => Token::new(TokenType::Lparen, self.ch),
            ')' => Token::new(TokenType::Rparen, self.ch),
            ',' => Token::new(TokenType::Comma, self.ch),
            '+' => Token::new(TokenType::Plus, self.ch),
            '-' => Token::new(TokenType::Minus, self.ch),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        token_type: TokenType::NotEq,
                        literal: String::from("!="),
                    }
                } else {
                    Token::new(TokenType::Bang, self.ch)
                }
            }
            '/' => Token::new(TokenType::Slash, self.ch),
            '*' => Token::new(TokenType::Asterisk, self.ch),
            '<' => Token::new(TokenType::LessThan, self.ch),
            '>' => Token::new(TokenType::GreaterThan, self.ch),
            '{' => Token::new(TokenType::Lbrace, self.ch),
            '}' => Token::new(TokenType::Rbrace, self.ch),
            '[' => Token::new(TokenType::Lbracket, self.ch),
            ']' => Token::new(TokenType::Rbracket, self.ch),
            ':' => Token::new(TokenType::Colon, self.ch),
            '\0' => Token {
                token_type: TokenType::Eof,
                literal: String::from(""),
            },
            '"' => {
                self.read_char();
                let string_val = self.read_string();
                Token {
                    token_type: TokenType::String,
                    literal: string_val,
                }
            }
            _ => {
                if Lexer::is_letter(self.ch) {
                    pointer_moved = true;
                    let identifier = self.read_identifier();
                    let token_type = TokenType::lookup_ident(&identifier);
                    Token {
                        token_type: token_type,
                        literal: identifier,
                    }
                } else if self.ch.is_ascii_digit() {
                    pointer_moved = true;
                    Token {
                        token_type: TokenType::Int,
                        literal: self.read_number(),
                    }
                } else {
                    Token::new(TokenType::Illegal, self.ch)
                }
            }
        };

        if !pointer_moved {
            self.read_char();
        }

        tok
    }

    fn is_letter(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while Lexer::is_letter(self.ch) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let position = self.position;
        while self.ch != '"' && self.ch != '\0' {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from(
            r#"
          let five = 5;
          let ten = 10;

          let add = fn(x, y) {
            x + y;
          };

          let result = add(five, ten);
          !-/*5;
          5 < 10 > 5;

          if (5 < 10) {
            return true;
          } else {
            return false;
          }

          10 == 10;
          10 != 9;
          "foobar"
          "foo bar"
          ""
          [1, 2];
          {"foo": "bar"}
        "#,
        );

        let tests: Vec<(TokenType, &str)> = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::SemiColon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::SemiColon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::SemiColon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::SemiColon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::Rparen, ")"),
            (TokenType::SemiColon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::SemiColon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LessThan, "<"),
            (TokenType::Int, "10"),
            (TokenType::GreaterThan, ">"),
            (TokenType::Int, "5"),
            (TokenType::SemiColon, ";"),
            (TokenType::If, "if"),
            (TokenType::Lparen, "("),
            (TokenType::Int, "5"),
            (TokenType::LessThan, "<"),
            (TokenType::Int, "10"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::SemiColon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::SemiColon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::SemiColon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::SemiColon, ";"),
            (TokenType::String, "foobar"),
            (TokenType::String, "foo bar"),
            (TokenType::String, ""),
            (TokenType::Lbracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::Rbracket, "]"),
            (TokenType::SemiColon, ";"),
            (TokenType::Lbrace, "{"),
            (TokenType::String, "foo"),
            (TokenType::Colon, ":"),
            (TokenType::String, "bar"),
            (TokenType::Rbrace, "}"),
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

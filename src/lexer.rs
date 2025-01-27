use crate::token::Token;

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
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            ';' => Token::SemiColon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            ':' => Token::Colon,
            '\0' => Token::Eof,
            '"' => {
                self.read_char();
                let string_val = self.read_string();
                Token::String(string_val)
            }
            _ => {
                if Lexer::is_letter(self.ch) {
                    pointer_moved = true;
                    let identifier = self.read_identifier();
                    Token::lookup_ident(&identifier)
                } else if self.ch.is_ascii_digit() {
                    pointer_moved = true;
                    Token::Int(self.read_number())
                } else {
                    Token::Illegal(self.ch.to_string())
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
          macro(x, y) {x + y; };
        "#,
        );

        let tests: Vec<(Token, &str)> = vec![
            (Token::Let, "let"),
            (Token::Ident("x".to_string()), "five"),
            (Token::Assign, "="),
            (Token::Int("5".to_string()), "5"),
            (Token::SemiColon, ";"),
            (Token::Let, "let"),
            (Token::Ident("x".to_string()), "ten"),
            (Token::Assign, "="),
            (Token::Int("10".to_string()), "10"),
            (Token::SemiColon, ";"),
            (Token::Let, "let"),
            (Token::Ident("add".to_string()), "add"),
            (Token::Assign, "="),
            (Token::Function, "fn"),
            (Token::Lparen, "("),
            (Token::Ident("x".to_string()), "x"),
            (Token::Comma, ","),
            (Token::Ident("y".to_string()), "y"),
            (Token::Rparen, ")"),
            (Token::Lbrace, "{"),
            (Token::Ident("x".to_string()), "x"),
            (Token::Plus, "+"),
            (Token::Ident("y".to_string()), "y"),
            (Token::SemiColon, ";"),
            (Token::Rbrace, "}"),
            (Token::SemiColon, ";"),
            (Token::Let, "let"),
            (Token::Ident("result".to_string()), "result"),
            (Token::Assign, "="),
            (Token::Ident("add".to_string()), "add"),
            (Token::Lparen, "("),
            (Token::Ident("five".to_string()), "five"),
            (Token::Comma, ","),
            (Token::Ident("ten".to_string()), "ten"),
            (Token::Rparen, ")"),
            (Token::SemiColon, ";"),
            (Token::Bang, "!"),
            (Token::Minus, "-"),
            (Token::Slash, "/"),
            (Token::Asterisk, "*"),
            (Token::Int("5".to_string()), "5"),
            (Token::SemiColon, ";"),
            (Token::Int("5".to_string()), "5"),
            (Token::LessThan, "<"),
            (Token::Int("10".to_string()), "10"),
            (Token::GreaterThan, ">"),
            (Token::Int("5".to_string()), "5"),
            (Token::SemiColon, ";"),
            (Token::If, "if"),
            (Token::Lparen, "("),
            (Token::Int("5".to_string()), "5"),
            (Token::LessThan, "<"),
            (Token::Int("10".to_string()), "10"),
            (Token::Rparen, ")"),
            (Token::Lbrace, "{"),
            (Token::Return, "return"),
            (Token::True, "true"),
            (Token::SemiColon, ";"),
            (Token::Rbrace, "}"),
            (Token::Else, "else"),
            (Token::Lbrace, "{"),
            (Token::Return, "return"),
            (Token::False, "false"),
            (Token::SemiColon, ";"),
            (Token::Rbrace, "}"),
            (Token::Int("10".to_string()), "10"),
            (Token::Eq, "=="),
            (Token::Int("10".to_string()), "10"),
            (Token::SemiColon, ";"),
            (Token::Int("10".to_string()), "10"),
            (Token::NotEq, "!="),
            (Token::Int("9".to_string()), "9"),
            (Token::SemiColon, ";"),
            (Token::String("foobar".to_string()), "foobar"),
            (Token::String("foo bar".to_string()), "foo bar"),
            (Token::String("".to_string()), ""),
            (Token::Lbracket, "["),
            (Token::Int("1".to_string()), "1"),
            (Token::Comma, ","),
            (Token::Int("2".to_string()), "2"),
            (Token::Rbracket, "]"),
            (Token::SemiColon, ";"),
            (Token::Lbrace, "{"),
            (Token::String("foo".to_string()), "foo"),
            (Token::Colon, ":"),
            (Token::String("bar".to_string()), "bar"),
            (Token::Rbrace, "}"),
            (Token::Macro, "macro"),
            (Token::Lparen, "("),
            (Token::Ident("x".to_string()), "x"),
            (Token::Comma, ","),
            (Token::Ident("y".to_string()), "y"),
            (Token::Rparen, ")"),
            (Token::Lbrace, "{"),
            (Token::Ident("x".to_string()), "x"),
            (Token::Plus, "+"),
            (Token::Ident("y".to_string()), "y"),
            (Token::SemiColon, ";"),
            (Token::Rbrace, "}"),
            (Token::SemiColon, ";"),
            (Token::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (index, (expected_token, expected_literal)) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            let tok_type = tok.token_type();
            let tok_literal = tok.literal();
            let expected_type = expected_token.token_type();

            assert_eq!(
                tok_type, expected_type,
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

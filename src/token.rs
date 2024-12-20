use std::fmt;

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, ch: char) -> Self {
        Self {
            token_type,
            literal: String::from(ch),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,
    Ident, // identifier
    Int,
    Assign,
    Plus,
    Comma,
    SemiColon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    // keywords
    Function,
    Let,
}

impl TokenType {

    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            _ => TokenType::Ident,
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str_val = match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Eof => "EOF",
            TokenType::Ident => "IDENT",
            TokenType::Int => "INT",
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Comma => ",",
            TokenType::SemiColon => ";",
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
        };

        write!(f, "{}", str_val)
    }
}

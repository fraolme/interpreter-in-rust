use std::fmt;

//Default is useful when we use mem::take for ownership change
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, PartialEq, Eq, Default, Copy, Clone, Hash)]
pub enum TokenType {
    Illegal,
    #[default]
    Eof, // the default value of TokenType,
    Ident, // identifier
    Int,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Comma,
    SemiColon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Eq,
    NotEq,
    String,
    Lbracket,
    Rbracket,
    Colon,
    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl TokenType {
    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
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
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::LessThan => "<",
            TokenType::GreaterThan => ">",
            TokenType::Comma => ",",
            TokenType::SemiColon => ";",
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Eq => "==",
            TokenType::NotEq => "!=",
            TokenType::String => "STRING",
            TokenType::Lbracket => "[",
            TokenType::Rbracket => "]",
            TokenType::Colon => ":",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
        };

        write!(f, "{}", str_val)
    }
}

#[derive(Debug, PartialEq, Eq, Default, Clone, Hash)]
pub enum Token {
    Illegal(String),
    #[default]
    Eof, // the default value of Token,
    Ident(String), // identifier
    Int(String),
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
    String(String),
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
    Macro,
}

impl Token {
    pub fn lookup_ident(ident: &str) -> Self {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "macro" => Token::Macro,
            _ => Token::Ident(ident.to_string()),
        }
    }

    pub fn token_type(&self) -> String {
        let token_type = match self {
            Token::Illegal(_) => "ILLEGAL",
            Token::Eof => "EOF",
            Token::Ident(_) => "IDENT",
            Token::Int(_) => "INT",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::LessThan => "<",
            Token::GreaterThan => ">",
            Token::Comma => ",",
            Token::SemiColon => ";",
            Token::Lparen => "(",
            Token::Rparen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::String(_) => "STRING",
            Token::Lbracket => "[",
            Token::Rbracket => "]",
            Token::Colon => ":",
            Token::Function => "FUNCTION",
            Token::Let => "LET",
            Token::True => "true",
            Token::False => "false",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Macro => "macro",
        };

        String::from(token_type)
    }

    pub fn literal(&self) -> &str {
        match self {
            Token::Illegal(val) => val,
            Token::Eof => "",
            Token::Ident(val) => val,
            Token::Int(val) => val,
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::LessThan => "<",
            Token::GreaterThan => ">",
            Token::Comma => ",",
            Token::SemiColon => ";",
            Token::Lparen => "(",
            Token::Rparen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::String(val) => val,
            Token::Lbracket => "[",
            Token::Rbracket => "]",
            Token::Colon => ":",
            Token::Function => "fn",
            Token::Let => "let",
            Token::True => "true",
            Token::False => "false",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Macro => "macro",
        }
    }
}

use crate::token::Token;
use std::fmt;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn node_type(&self) -> NodeType;
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeType {
    Program,
    Let,
    Identifier,
    Return,
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            NodeType::Program => "program",
            NodeType::Let => "let",
            NodeType::Identifier => "identifier",
            NodeType::Return => "return",
        };

        write!(f, "{}", val)
    }
}
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(let_statement) => let_statement.token_literal(),
            Statement::Return(return_statement) => return_statement.token_literal(),
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Statement::Let(let_statement) => let_statement.node_type(), 
            Statement::Return(return_statement) => return_statement.node_type(),
        }
    }
}

pub enum Expression {
    Ident(Identifier),
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(identifier) => identifier.token_literal()
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Expression::Ident(identifier) => identifier.node_type()
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }

    fn node_type(&self) -> NodeType {
        NodeType::Program
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    //TODO: skip expressions for now
    //value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Let
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Identifier
    }
}

pub struct ReturnStatement {
    pub token: Token,
    //TODO: parse the expression part later
    //pub return_value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Return
    }
}

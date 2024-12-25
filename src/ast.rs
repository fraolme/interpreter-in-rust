use crate::token::Token;
use std::fmt;
use std::fmt::Write;

pub trait Node: fmt::Display {
    fn token_literal(&self) -> &str;
    fn node_type(&self) -> NodeType;
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeType {
    Program,
    Let,
    Identifier,
    Return,
    ExpressionStatement,
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            NodeType::Program => "program",
            NodeType::Let => "let",
            NodeType::Identifier => "identifier",
            NodeType::Return => "return",
            NodeType::ExpressionStatement => "expression_statement",
        };

        write!(f, "{}", val)
    }
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(let_stmt) => let_stmt.token_literal(),
            Statement::Return(return_stmt) => return_stmt.token_literal(),
            Statement::Expression(expr_stmt) => expr_stmt.token_literal(),
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Statement::Let(let_stmt) => let_stmt.node_type(),
            Statement::Return(return_stmt) => return_stmt.node_type(),
            Statement::Expression(expr_stmt) => expr_stmt.node_type(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(let_stmt) => write!(f, "{}", let_stmt),
            Statement::Return(return_stmt) => write!(f, "{}", return_stmt),
            Statement::Expression(expr_stmt) => write!(f, "{}", expr_stmt),
        }
    }
}

pub enum Expression {
    Ident(Identifier),
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(identifier) => identifier.token_literal(),
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Expression::Ident(identifier) => identifier.node_type(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(identifier) => write!(f, "{}", identifier),
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        for stmt in &self.statements {
            write!(&mut buffer, "{}", stmt).unwrap();
        }

        write!(f, "{}", buffer)
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

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        write!(&mut buffer, "{} ", self.token_literal()).unwrap();
        write!(&mut buffer, "{} = ", self.name).unwrap();
        //TODO: add the value part
        //write!(&mut buffer, "{};", self.value);

        write!(f, "{}", buffer)
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

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
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

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        write!(&mut buffer, "{} ", self.token_literal()).unwrap();
        //TODO: add the return_value part
        //write!(&mut buffer, "{};", self.name).unwrap();
        write!(f, "{}", buffer)
    }
}

// for this kind of statements -> x + 10;
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::ExpressionStatement
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

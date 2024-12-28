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
    IntegerLiteral,
    PrefixExpression,
    InfixExpression,
    BooleanLiteral,
    IfExpression,
    BlockStatement,
    FunctionLiteral,
    CallExpression,
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            NodeType::Program => "program",
            NodeType::Let => "let",
            NodeType::Identifier => "identifier",
            NodeType::Return => "return",
            NodeType::ExpressionStatement => "expression_statement",
            NodeType::IntegerLiteral => "integer",
            NodeType::PrefixExpression => "prefix",
            NodeType::InfixExpression => "infix",
            NodeType::BooleanLiteral => "boolean",
            NodeType::IfExpression => "if",
            NodeType::BlockStatement => "block_statement",
            NodeType::FunctionLiteral => "function_literal",
            NodeType::CallExpression => "function_call",
        };

        write!(f, "{}", val)
    }
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(let_stmt) => let_stmt.token_literal(),
            Statement::Return(return_stmt) => return_stmt.token_literal(),
            Statement::Expression(expr_stmt) => expr_stmt.token_literal(),
            Statement::Block(block) => block.token_literal(),
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Statement::Let(let_stmt) => let_stmt.node_type(),
            Statement::Return(return_stmt) => return_stmt.node_type(),
            Statement::Expression(expr_stmt) => expr_stmt.node_type(),
            Statement::Block(block) => block.node_type(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(let_stmt) => write!(f, "{}", let_stmt),
            Statement::Return(return_stmt) => write!(f, "{}", return_stmt),
            Statement::Expression(expr_stmt) => write!(f, "{}", expr_stmt),
            Statement::Block(block_stmt) => write!(f, "{}", block_stmt),
        }
    }
}

pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Boolean(BooleanLiteral),
    If(Box<IfExpression>),
    Func(FunctionLiteral),
    Call(Box<CallExpression>),
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(identifier) => identifier.token_literal(),
            Expression::Int(int_literal) => int_literal.token_literal(),
            Expression::Prefix(prefix) => prefix.token_literal(),
            Expression::Infix(infix) => infix.token_literal(),
            Expression::Boolean(bol_lit) => bol_lit.token_literal(),
            Expression::If(if_exp) => if_exp.token_literal(),
            Expression::Func(func) => func.token_literal(),
            Expression::Call(call) => call.token_literal(),
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Expression::Ident(identifier) => identifier.node_type(),
            Expression::Int(int_literal) => int_literal.node_type(),
            Expression::Prefix(prefix) => prefix.node_type(),
            Expression::Infix(infix) => infix.node_type(),
            Expression::Boolean(bol_lit) => bol_lit.node_type(),
            Expression::If(if_exp) => if_exp.node_type(),
            Expression::Func(func) => func.node_type(),
            Expression::Call(call) => call.node_type(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(identifier) => write!(f, "{}", identifier),
            Expression::Int(int_literal) => write!(f, "{}", int_literal),
            Expression::Prefix(prefix) => write!(f, "{}", prefix),
            Expression::Infix(infix) => write!(f, "{}", infix),
            Expression::Boolean(bol_lit) => write!(f, "{}", bol_lit),
            Expression::If(if_exp) => write!(f, "{}", if_exp),
            Expression::Func(func) => write!(f, "{}", func),
            Expression::Call(call) => write!(f, "{}", call),
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

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::IntegerLiteral
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::PrefixExpression
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::InfixExpression
    }
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::BooleanLiteral
    }
}

impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct IfExpression {
    pub token: Token, // if token
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::IfExpression
    }
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        write!(&mut buffer, "if {} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(&mut buffer, "else {}", alt)?;
        }

        write!(f, "{}", buffer)
    }
}

pub struct BlockStatement {
    pub token: Token, // { token
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::BlockStatement
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        for stmt in &self.statements {
            write!(&mut buffer, "{}", stmt)?;
        }

        write!(f, "{}", buffer)
    }
}

pub struct FunctionLiteral {
    pub token: Token, // fn token
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::FunctionLiteral
    }
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({}) {}", self.token_literal(), params, self.body)
    }
}

pub struct CallExpression {
    pub token: Token,              // the ( token
    pub function: Box<Expression>, // identifier or function literal
    pub arguments: Vec<Expression>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::CallExpression
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let args = self
            .arguments
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({})", self.function, args)
    }
}

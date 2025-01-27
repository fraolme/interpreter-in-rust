use crate::token::Token;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;
use std::hash::{Hash, Hasher};

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
    StringLiteral,
    ArrayLiteral,
    IndexExpression,
    HashLiteral,
    MacroLiteral,
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
            NodeType::StringLiteral => "string",
            NodeType::ArrayLiteral => "array",
            NodeType::IndexExpression => "index",
            NodeType::HashLiteral => "hash",
            NodeType::MacroLiteral => "macro",
        };

        write!(f, "{}", val)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Boolean(BooleanLiteral),
    If(Box<IfExpression>),
    Func(FunctionLiteral),
    Call(Box<CallExpression>),
    String(StringLiteral),
    Array(ArrayLiteral),
    Index(Box<IndexExpression>),
    Hash(HashLiteral),
    Macro(MacroLiteral),
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
            Expression::String(sl) => sl.token_literal(),
            Expression::Array(arr) => arr.token_literal(),
            Expression::Index(index) => index.token_literal(),
            Expression::Hash(hash) => hash.token_literal(),
            Expression::Macro(mac) => mac.token_literal(),
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
            Expression::String(sl) => sl.node_type(),
            Expression::Array(arr) => arr.node_type(),
            Expression::Index(index) => index.node_type(),
            Expression::Hash(hash) => hash.node_type(),
            Expression::Macro(mac) => mac.node_type(),
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
            Expression::String(sl) => write!(f, "{}", sl),
            Expression::Array(arr) => write!(f, "{}", arr),
            Expression::Index(index) => write!(f, "{}", index),
            Expression::Hash(hash) => write!(f, "{}", hash),
            Expression::Macro(mac) => write!(f, "{}", mac),
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LetStatement {
    pub token: Token,
    pub name: Expression, // identifier type
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal()
    }

    fn node_type(&self) -> NodeType {
        NodeType::Let
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        write!(&mut buffer, "{} ", self.token_literal())?;
        write!(&mut buffer, "{} = ", self.name)?;
        write!(&mut buffer, "{};", self.value)?;

        write!(f, "{}", buffer)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal()
    }

    fn node_type(&self) -> NodeType {
        NodeType::Return
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        write!(&mut buffer, "{} ", self.token_literal()).unwrap();
        write!(&mut buffer, "{};", self.return_value).unwrap();
        write!(f, "{}", buffer)
    }
}

// for this kind of statements -> x + 10;
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IfExpression {
    pub token: Token, // if token
    pub condition: Box<Expression>,
    pub consequence: Statement, // block statement type
    pub alternative: Option<Statement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BlockStatement {
    pub token: Token, // { token
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionLiteral {
    pub token: Token,                // fn token
    pub parameters: Vec<Expression>, // identifier type
    pub body: Box<Statement>,        // block statement type
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CallExpression {
    pub token: Token,              // the ( token
    pub function: Box<Expression>, // identifier or function literal
    pub arguments: Vec<Expression>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal()
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal()
    }

    fn node_type(&self) -> NodeType {
        NodeType::StringLiteral
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrayLiteral {
    pub token: Token, // [
    pub elements: Vec<Expression>,
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal()
    }

    fn node_type(&self) -> NodeType {
        NodeType::ArrayLiteral
    }
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IndexExpression {
    pub token: Token, //[
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Node for IndexExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal()
    }

    fn node_type(&self) -> NodeType {
        NodeType::IndexExpression
    }
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct HashLiteral {
    pub token: Token, // {
    pub pairs: HashMap<Expression, Expression>,
}

impl Node for HashLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal()
    }

    fn node_type(&self) -> NodeType {
        NodeType::HashLiteral
    }
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs
                .iter()
                .map(|(k, v)| k.to_string() + ":" + &v.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

// implement a custom Hash Trait since hashmap doesn't implement Hash trait
// by ignoring the hashmap during hash creation
impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.hash(state);
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct MacroLiteral {
    pub token: Token,                // macro
    pub parameters: Vec<Expression>, // identifiers
    pub body: Box<Statement>,
}

impl Node for MacroLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal()
    }

    fn node_type(&self) -> NodeType {
        NodeType::MacroLiteral
    }
}

impl fmt::Display for MacroLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "macro ({}) {{ {} }}",
            self.parameters
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        )
    }
}

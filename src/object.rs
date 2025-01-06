use crate::ast::{BlockStatement, Identifier};
use crate::environment::Environment;
use std::fmt;

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function(FunctionObject),
    String(String),
    Builtin(BuiltinFunction),
    Array(Vec<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        format!("{}", self)
    }

    pub fn get_type(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN",
            Object::Error(_) => "ERROR",
            Object::Function(_) => "FUNCTION",
            Object::String(_) => "STRING",
            Object::Builtin(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Error(message) => write!(f, "ERROR: {}", message),
            Object::Function(func) => write!(f, "{}", func),
            Object::String(st) => write!(f, "{}", st),
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Array(arr) => write!(
                f,
                "[{}]",
                arr.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone)]
pub struct FunctionObject {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl fmt::Display for FunctionObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn ({}) {{\n {} \n}}",
            self.parameters
                .iter()
                .map(|p| p.value.clone())
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        )
    }
}

type BuiltinFunction = fn(Vec<Object>) -> Object;

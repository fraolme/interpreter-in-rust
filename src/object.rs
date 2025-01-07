use crate::ast::{BlockStatement, Identifier};
use crate::environment::Environment;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Clone, PartialEq, Eq)]
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
    Hash(HashMap<Object, Object>),
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
            Object::Hash(_) => "HASH",
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
            Object::Hash(hash) => write!(
                f,
                "{{{}}}",
                hash.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Object::Integer(val) => val.hash(state),
            Object::Boolean(val) => val.hash(state),
            Object::String(val) => val.hash(state),
            _ => panic!(
                "This type can't be used as a key for a hashmap, got={}",
                self.get_type()
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::hash::DefaultHasher;

    #[test]
    fn test_string_hash_key() {
        let hello1_hash = calculate_hash(Object::String("Hello World".to_string()));
        let hello2_hash = calculate_hash(Object::String("Hello World".to_string()));

        let diff1_hash = calculate_hash(Object::String("My name is John".to_string()));
        let diff2_hash = calculate_hash(Object::String("My name is John".to_string()));

        assert_eq!(
            hello1_hash, hello2_hash,
            "strings with same content have different hash keys"
        );

        assert_eq!(
            diff1_hash, diff2_hash,
            "strings with same content have different hash keys"
        );

        assert_ne!(
            hello1_hash, diff1_hash,
            "strings with different content have same hash key"
        );
    }

    #[test]
    #[should_panic]
    fn test_using_null_as_hash_key() {
        calculate_hash(Object::Null);
    }

    // helper
    fn calculate_hash(val: Object) -> u64 {
        let mut hasher = DefaultHasher::new();
        val.hash(&mut hasher);
        hasher.finish()
    }
}

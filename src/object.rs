#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "null".to_string(),
        }
    }
}

use crate::object::Object;
use std::collections::HashMap;
use std::{rc::Rc, cell::RefCell};

#[derive(PartialEq, Eq, Clone)]
struct Environment {
    store: HashMap<String, Object>,
    outer: Option<EnvironmentPtr>,
}

impl Environment {
    fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    fn new_enclosed(outer: EnvironmentPtr) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    fn get(&self, name: &str) -> Option<Object> {
        let result = self.store.get(name);
        if let Some(result) = result {
            return Some(result.clone())
        } else if self.outer.is_some() {
            let result = self.outer.as_ref().unwrap().get(name);
            if let Some(result) = result {
                return Some(result.clone())
            }
        }

        None
    }

    fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct EnvironmentPtr(Rc<RefCell<Environment>>);

impl EnvironmentPtr {
    pub fn new() -> Self {
        EnvironmentPtr(Rc::new(RefCell::new(Environment::new())))
    }

    pub fn new_enclosed(outer: EnvironmentPtr) -> Self {
        EnvironmentPtr(Rc::new(RefCell::new(Environment::new_enclosed(outer))))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.0.borrow().get(name).clone()
    }

    pub fn set(&self, name: &str, val: Object) {
        self.0.borrow_mut().set(name, val)
    }
}

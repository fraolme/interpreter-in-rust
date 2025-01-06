use crate::object::Object;

pub fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(val) => Object::Integer(val.len() as i64),
        Object::Array(arr) => Object::Integer(arr.len() as i64),
        _ => Object::Error(format!(
            "Argument to 'len' not supported, got {}",
            &args[0].get_type()
        )),
    }
}

pub fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(val) => {
            if val.len() > 0 {
                Object::String(val.chars().nth(0).unwrap().to_string())
            } else {
                Object::Null
            }
        }
        Object::Array(arr) => {
            if arr.len() > 0 {
                arr[0].clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Error(format!(
            "Argument to 'first' not supported, got {}",
            &args[0].get_type()
        )),
    }
}

pub fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(val) => {
            if val.len() > 0 {
                Object::String(val.chars().nth(val.len() - 1).unwrap().to_string())
            } else {
                Object::Null
            }
        }
        Object::Array(arr) => {
            if arr.len() > 0 {
                arr[arr.len() - 1].clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Error(format!(
            "Argument to 'last' not supported, got {}",
            &args[0].get_type()
        )),
    }
}

pub fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(val) => {
            if val.len() > 0 {
                Object::String(val[1..].to_string())
            } else {
                Object::Null
            }
        }
        Object::Array(arr) => {
            if arr.len() > 0 {
                Object::Array(arr[1..].to_vec())
            } else {
                Object::Null
            }
        }
        _ => Object::Error(format!(
            "Argument to 'rest' not supported, got {}",
            &args[0].get_type()
        )),
    }
}

pub fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "Wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(arr) => {
            let mut new_arr = arr.to_vec();
            new_arr.push(args[1].clone());
            Object::Array(new_arr)
        }
        _ => Object::Error(format!(
            "Argument to 'push' must be ARRAY, got {}",
            &args[0].get_type()
        )),
    }
}

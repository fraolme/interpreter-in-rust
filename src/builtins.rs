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
        _ => Object::Error(format!(
            "Argument to 'len' not supported, got {}",
            &args[0].get_type()
        )),
    }
}

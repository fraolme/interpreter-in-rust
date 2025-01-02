use crate::ast::*;
use crate::object::Object;

pub fn eval(program: Program) -> Object {
    let mut result = Object::Null;
    for statement in program.statements {
        result = eval_statement(statement);
    }

    result
}

fn eval_statement(stmt: Statement) -> Object {
    match stmt {
        Statement::Expression(expr_stmt) => eval_expression(expr_stmt.expression),
        _ => panic!("Unsupported statement type"),
    }
}

fn eval_expression(expr: Expression) -> Object {
    match expr {
        Expression::Int(intv) => Object::Integer(intv.value),
        Expression::Boolean(boolv) => Object::Boolean(boolv.value),
        Expression::Prefix(prefix) => {
            let right = eval_expression(*prefix.right);
            return eval_prefix_expression(&prefix.operator, right);
        }
        _ => panic!("Unsupported expression type"),
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Null,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(val) => {
            if val {
                Object::Boolean(false)
            } else {
                Object::Boolean(true)
            }
        }
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false), // all other values are taken as true
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(intv) => Object::Integer(intv * -1),
        _ => Object::Null,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<(&str, i64)> = vec![("5", 5), ("10", 10), ("-5", -5), ("-10", -10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: Vec<(&str, bool)> = vec![("true", true), ("false", false)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests: Vec<(&str, bool)> = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    // helpers
    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        return eval(program);
    }

    fn test_integer_object(obj: Object, expected: i64) {
        if let Object::Integer(val) = obj {
            assert_eq!(
                val, expected,
                "object has a wrong value. got={}, want={}",
                val, expected
            );
        } else {
            panic!("object is not integer. got={:?}", obj);
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        if let Object::Boolean(val) = obj {
            assert_eq!(
                val, expected,
                "object has a wrong value. got={}, want={}",
                val, expected
            );
        } else {
            panic!("object is not boolean. got={:?}", obj);
        }
    }
}

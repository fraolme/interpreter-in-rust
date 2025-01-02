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
        Expression::Infix(infix) => {
            let left = eval_expression(*infix.left);
            let right = eval_expression(*infix.right);
            return eval_infix_expression(&infix.operator, left, right);
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

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left_val), Object::Integer(right_val)) => {
            eval_integer_infix_expression(operator, left_val, right_val)
        }
        (Object::Boolean(left_val), Object::Boolean(right_val)) => {
            eval_boolean_infix_expression(operator, left_val, right_val)
        }
        (Object::Null, Object::Null) => Object::Boolean(true),
        _ => {
            // different type return false for == and != while it returns null for other types
            if (operator == "==" || operator == "!=") {
                Object::Boolean(false)
            } else {
                Object::Null
            }
        }
    }
}

fn eval_integer_infix_expression(operator: &str, left_val: i64, right_val: i64) -> Object {
    match operator {
        "+" => Object::Integer(left_val + right_val),
        "*" => Object::Integer(left_val * right_val),
        "-" => Object::Integer(left_val - right_val),
        "/" => Object::Integer(left_val / right_val),
        "<" => Object::Boolean(left_val < right_val),
        ">" => Object::Boolean(left_val > right_val),
        "==" => Object::Boolean(left_val == right_val),
        "!=" => Object::Boolean(left_val != right_val),
        _ => Object::Null,
    }
}

fn eval_boolean_infix_expression(operator: &str, left_val: bool, right_val: bool) -> Object {
    match operator {
        "==" => Object::Boolean(left_val == right_val),
        "!=" => Object::Boolean(left_val != right_val),
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
        let tests: Vec<(&str, i64)> = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: Vec<(&str, bool)> = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

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

use crate::ast::*;
use crate::environment::Environment;
use crate::object::{FunctionObject, Object};

pub fn eval(program: Program, env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for stmt in program.statements {
        result = eval_statement(stmt, env);
        if let Object::ReturnValue(obj) = result {
            return *obj;
        } else if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_statement(stmt: Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::Expression(expr_stmt) => eval_expression(expr_stmt.expression, env),
        Statement::Block(block_stmt) => eval_block_statement(block_stmt, env),
        Statement::Return(ret_stmt) => {
            let val = eval_expression(ret_stmt.return_value, env);
            if let Object::Error(_) = val {
                val
            } else {
                Object::ReturnValue(Box::new(val))
            }
        }
        Statement::Let(let_stmt) => {
            let val = eval_expression(let_stmt.value, env);
            if let Object::Error(_) = val {
                return val;
            }
            env.set(&let_stmt.name.value, val);
            return Object::Null;
        }
    }
}

fn eval_expression(expr: Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::Int(intv) => Object::Integer(intv.value),
        Expression::Boolean(boolv) => Object::Boolean(boolv.value),
        Expression::Prefix(prefix) => {
            let right = eval_expression(*prefix.right, env);
            if let Object::Error(_) = right {
                right
            } else {
                eval_prefix_expression(&prefix.operator, right)
            }
        }
        Expression::Infix(infix) => {
            let left = eval_expression(*infix.left, env);
            if let Object::Error(_) = left {
                return left;
            }
            let right = eval_expression(*infix.right, env);
            if let Object::Error(_) = right {
                return right;
            }
            return eval_infix_expression(&infix.operator, left, right);
        }
        Expression::If(if_expr) => eval_if_expression(*if_expr, env),
        Expression::Ident(ident) => eval_identifier(ident, env),
        Expression::Func(func_lit) => Object::Function(FunctionObject {
            parameters: func_lit.parameters,
            body: func_lit.body,
            env: env.clone(),
        }),
        Expression::Call(call_expr) => {
            let function = eval_expression(*call_expr.function, env);
            if let Object::Error(_) = function {
                function
            } else {
                let args = eval_expressions(call_expr.arguments, env);
                if args.len() == 1 {
                    if let Object::Error(_) = args[0] {
                        return args[0].clone();
                    }
                }

                apply_function(function, args)
            }
        }
        Expression::String(sl) => Object::String(sl.value),
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!(
            "unknown operator: {}{}",
            operator,
            right.get_type()
        )),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(val) => Object::Boolean(!val),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false), // all other values are taken as true
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(intv) => Object::Integer(intv * -1),
        _ => Object::Error(format!("unknown operator: -{}", right.get_type())),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let left_type = left.get_type().to_string();
    let right_type = right.get_type().to_string();

    match (left, right) {
        (Object::Integer(left_val), Object::Integer(right_val)) => {
            eval_integer_infix_expression(operator, left_val, right_val)
        }
        (Object::Boolean(left_val), Object::Boolean(right_val)) => {
            eval_boolean_infix_expression(operator, left_val, right_val)
        }
        (Object::Null, Object::Null) => Object::Boolean(true),
        (Object::String(left_val), Object::String(right_val)) => {
            eval_string_infix_expression(operator, left_val, right_val)
        }
        _ => {
            // different type return false for == and != while it returns null for other types
            if operator == "==" || operator == "!=" {
                Object::Boolean(false)
            } else if left_type != right_type {
                Object::Error(format!(
                    "type mismatch: {} {} {}",
                    left_type, operator, right_type
                ))
            } else {
                Object::Error(format!(
                    "unknown operator: {} {} {}",
                    left_type, operator, right_type
                ))
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
        _ => Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_boolean_infix_expression(operator: &str, left_val: bool, right_val: bool) -> Object {
    match operator {
        "==" => Object::Boolean(left_val == right_val),
        "!=" => Object::Boolean(left_val != right_val),
        _ => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator)),
    }
}

fn eval_if_expression(if_expr: IfExpression, env: &mut Environment) -> Object {
    let condition = eval_expression(*if_expr.condition, env);
    if let Object::Error(_) = condition {
        return condition;
    }
    let cond_val = match condition {
        Object::Boolean(bool_val) => bool_val,
        Object::Null => false,
        _ => true,
    };

    if cond_val {
        return eval_statement(Statement::Block(if_expr.consequence), env);
    } else if if_expr.alternative.is_some() {
        return eval_statement(Statement::Block(if_expr.alternative.unwrap()), env);
    } else {
        return Object::Null;
    }
}

fn eval_block_statement(block_statement: BlockStatement, env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for stmt in block_statement.statements {
        result = eval_statement(stmt, env);
        // bubble it up without unwraping the return value to handle return inside nested if
        if let Object::ReturnValue(_) = result {
            return result;
        } else if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_identifier(ident: Identifier, env: &mut Environment) -> Object {
    let val = env.get(&ident.value);
    if val.is_none() {
        Object::Error(format!("identifier not found: {}", ident.value))
    } else {
        val.unwrap().clone()
    }
}

fn eval_expressions(exps: Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut result = vec![];

    for exp in exps {
        let evaluated = eval_expression(exp, env);
        if let Object::Error(_) = evaluated {
            return vec![evaluated];
        }
        result.push(evaluated)
    }

    result
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    if let Object::Function(func_obj) = func {
        let FunctionObject {
            body,
            env,
            parameters,
        } = func_obj;
        if args.len() != parameters.len() {
            return Object::Error(format!(
                "wrong number of arguments passed. expected={}, got={}",
                parameters.len(),
                args.len()
            ));
        }
        let mut extended_env = extend_function_env(parameters, args, env);
        let evaluated = eval_statement(Statement::Block(body), &mut extended_env);
        if let Object::ReturnValue(wrapped_obj) = evaluated {
            *wrapped_obj
        } else {
            evaluated
        }
    } else {
        Object::Error(format!("not a function: {}", func.get_type()))
    }
}

fn extend_function_env(
    parameters: Vec<Identifier>,
    args: Vec<Object>,
    env: Environment,
) -> Environment {
    let mut env = Environment::new_enclosed(env);

    for i in 0..parameters.len() {
        env.set(&parameters[i].value, args[i].clone());
    }

    env
}

fn eval_string_infix_expression(operator: &str, left_val: String, right_val: String) -> Object {
    match operator {
        "+" => Object::String(left_val + &right_val),
        "==" => Object::Boolean(left_val == right_val),
        "!=" => Object::Boolean(left_val != right_val),
        ">" => Object::Boolean(left_val > right_val),
        "<" => Object::Boolean(left_val < right_val),
        _ => Object::Error(format!("unknown operator: STRING {} STRING", operator)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    enum Expected {
        Int64(i64),
        Boolean(bool),
        Null,
    }

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

    #[test]
    fn test_if_else_expressions() {
        let tests: Vec<(&str, Expected)> = vec![
            ("if (true) { 10 }", Expected::Int64(10)),
            ("if (false) { 10 }", Expected::Null),
            ("if (1) { 10 }", Expected::Int64(10)),
            ("if (1 < 2) { 10 }", Expected::Int64(10)),
            ("if (1 > 2) { 10 }", Expected::Null),
            ("if (1 > 2) { 10 } else { 20 }", Expected::Int64(20)),
            ("if (1 < 2) { 10 } else { 20 }", Expected::Int64(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Expected::Int64(val) => test_integer_object(evaluated, val),
                Expected::Null => test_null_object(evaluated),
                _ => (),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&str, i64)> = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                r#"
                if (10 > 1) {
                    if(10 > 1) {
                        return 10;
                    }
                }

                return 1;
            "#,
                10,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r#"
                if (10 > 1) {
                    if(10 > 1) {
                        true + false;
                        return 100;
                    }

                    return 1;
                }
                return 2;
            "#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
            (
                r#" "Hello" - "World" "#,
                "unknown operator: STRING - STRING",
            ),
        ];

        for (input, expected_msg) in tests {
            let evaluated = test_eval(input);
            if let Object::Error(msg) = evaluated {
                assert_eq!(
                    msg, expected_msg,
                    "wrong error message. expected ={}, got={}",
                    expected_msg, msg
                );
            } else {
                panic!("no error object returned. got={}", evaluated);
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            test_integer_object(test_eval(input), expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) {x + 2;};";
        let evaluated = test_eval(input);
        if let Object::Function(func) = evaluated {
            assert_eq!(
                func.parameters.len(),
                1,
                "function has wrong parameters. parameters={}",
                func.parameters.len()
            );
            assert_eq!(
                func.parameters[0].to_string(),
                "x",
                "parameter is not 'x'. got={}",
                func.parameters[0]
            );
            assert_eq!(
                func.body.to_string(),
                "(x + 2)",
                "body is not {}. got={}",
                "(x + 2)",
                func.body
            );
        } else {
            panic!("object is not Function. got={}", evaluated);
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            test_integer_object(test_eval(input), expected);
        }
    }

    #[test]
    fn test_closures() {
        let input = r#"
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(3);
        "#;
        test_integer_object(test_eval(input), 5);
    }

    #[test]
    fn test_string_literal() {
        let input = r#" "Hello World" "#;
        let evaluated = test_eval(input);
        if let Object::String(val) = evaluated {
            assert_eq!(val, "Hello World", "String has wrong value. got={}", val);
        } else {
            panic!("object is not string. got={}", evaluated);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#" "Hello" + " " + "World!" "#;
        let evaluated = test_eval(input);
        if let Object::String(val) = evaluated {
            assert_eq!(val, "Hello World!", "String has wrong value. got={}", val);
        } else {
            panic!("object is not string. got={}", evaluated);
        }
    }

    // helpers
    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();

        return eval(program, &mut env);
    }

    fn test_integer_object(obj: Object, expected: i64) {
        if let Object::Integer(val) = obj {
            assert_eq!(
                val, expected,
                "object has a wrong value. got={}, want={}",
                val, expected
            );
        } else {
            panic!("object is not integer. got={}", obj);
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
            panic!("object is not boolean. got={}", obj);
        }
    }

    fn test_null_object(obj: Object) {
        if let Object::Null = obj {
        } else {
            panic!("object is not null. got={}", obj);
        }
    }
}

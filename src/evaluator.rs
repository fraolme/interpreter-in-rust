use crate::ast::*;
use crate::builtins;
use crate::environment::Environment;
use crate::object::{FunctionObject, Object};
use std::collections::HashMap;

pub struct Evaluator {
    builtins: HashMap<String, Object>,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut builtins_map = HashMap::new();
        builtins_map.insert("len".to_string(), Object::Builtin(builtins::len));
        builtins_map.insert("first".to_string(), Object::Builtin(builtins::first));
        builtins_map.insert("last".to_string(), Object::Builtin(builtins::last));
        builtins_map.insert("rest".to_string(), Object::Builtin(builtins::rest));
        builtins_map.insert("push".to_string(), Object::Builtin(builtins::push));
        Self {
            builtins: builtins_map,
        }
    }

    pub fn eval(&self, program: Program, env: &mut Environment) -> Object {
        let mut result = Object::Null;
        for stmt in program.statements {
            result = self.eval_statement(stmt, env);
            if let Object::ReturnValue(obj) = result {
                return *obj;
            } else if let Object::Error(_) = result {
                return result;
            }
        }
        result
    }

    fn eval_statement(&self, stmt: Statement, env: &mut Environment) -> Object {
        match stmt {
            Statement::Expression(expr_stmt) => self.eval_expression(expr_stmt.expression, env),
            Statement::Block(block_stmt) => self.eval_block_statement(block_stmt, env),
            Statement::Return(ret_stmt) => {
                let val = self.eval_expression(ret_stmt.return_value, env);
                if let Object::Error(_) = val {
                    val
                } else {
                    Object::ReturnValue(Box::new(val))
                }
            }
            Statement::Let(let_stmt) => {
                let val = self.eval_expression(let_stmt.value, env);
                if let Object::Error(_) = val {
                    return val;
                }
                env.set(&let_stmt.name.value, val);
                return Object::Null;
            }
        }
    }

    fn eval_expression(&self, expr: Expression, env: &mut Environment) -> Object {
        match expr {
            Expression::Int(intv) => Object::Integer(intv.value),
            Expression::Boolean(boolv) => Object::Boolean(boolv.value),
            Expression::Prefix(prefix) => {
                let right = self.eval_expression(*prefix.right, env);
                if let Object::Error(_) = right {
                    right
                } else {
                    self.eval_prefix_expression(&prefix.operator, right)
                }
            }
            Expression::Infix(infix) => {
                let left = self.eval_expression(*infix.left, env);
                if let Object::Error(_) = left {
                    return left;
                }
                let right = self.eval_expression(*infix.right, env);
                if let Object::Error(_) = right {
                    return right;
                }
                return self.eval_infix_expression(&infix.operator, left, right);
            }
            Expression::If(if_expr) => self.eval_if_expression(*if_expr, env),
            Expression::Ident(ident) => self.eval_identifier(ident, env),
            Expression::Func(func_lit) => Object::Function(FunctionObject {
                parameters: func_lit.parameters,
                body: func_lit.body,
                env: env.clone(),
            }),
            Expression::Call(call_expr) => {
                let function = self.eval_expression(*call_expr.function, env);
                if let Object::Error(_) = function {
                    function
                } else {
                    let args = self.eval_expressions(call_expr.arguments, env);
                    if args.len() == 1 {
                        if let Object::Error(_) = args[0] {
                            return args[0].clone();
                        }
                    }

                    self.apply_function(function, args)
                }
            }
            Expression::String(sl) => Object::String(sl.value),
            Expression::Array(arr) => {
                let arr_elts = self.eval_expressions(arr.elements, env);
                if arr_elts.len() == 1 {
                    if let Object::Error(_) = &arr_elts[0] {
                        return arr_elts[0].clone();
                    }
                }
                Object::Array(arr_elts)
            }
            Expression::Index(index_exp) => {
                let left = self.eval_expression(*index_exp.left, env);
                if let Object::Error(_) = left {
                    return left;
                }
                let index = self.eval_expression(*index_exp.index, env);
                if let Object::Error(_) = index {
                    return index;
                }
                self.eval_index_expression(left, index)
            }
            Expression::Hash(hash) => self.eval_hash_literal(hash, env),
        }
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator_expression(right),
            _ => Object::Error(format!(
                "unknown operator: {}{}",
                operator,
                right.get_type()
            )),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(val) => Object::Boolean(!val),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false), // all other values are taken as true
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(intv) => Object::Integer(intv * -1),
            _ => Object::Error(format!("unknown operator: -{}", right.get_type())),
        }
    }

    fn eval_infix_expression(&self, operator: &str, left: Object, right: Object) -> Object {
        let left_type = left.get_type().to_string();
        let right_type = right.get_type().to_string();

        match (left, right) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                self.eval_integer_infix_expression(operator, left_val, right_val)
            }
            (Object::Boolean(left_val), Object::Boolean(right_val)) => {
                self.eval_boolean_infix_expression(operator, left_val, right_val)
            }
            (Object::Null, Object::Null) => Object::Boolean(true),
            (Object::String(left_val), Object::String(right_val)) => {
                self.eval_string_infix_expression(operator, left_val, right_val)
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

    fn eval_integer_infix_expression(
        &self,
        operator: &str,
        left_val: i64,
        right_val: i64,
    ) -> Object {
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

    fn eval_boolean_infix_expression(
        &self,
        operator: &str,
        left_val: bool,
        right_val: bool,
    ) -> Object {
        match operator {
            "==" => Object::Boolean(left_val == right_val),
            "!=" => Object::Boolean(left_val != right_val),
            _ => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator)),
        }
    }

    fn eval_if_expression(&self, if_expr: IfExpression, env: &mut Environment) -> Object {
        let condition = self.eval_expression(*if_expr.condition, env);
        if let Object::Error(_) = condition {
            return condition;
        }
        let cond_val = match condition {
            Object::Boolean(bool_val) => bool_val,
            Object::Null => false,
            _ => true,
        };

        if cond_val {
            return self.eval_statement(Statement::Block(if_expr.consequence), env);
        } else if if_expr.alternative.is_some() {
            return self.eval_statement(Statement::Block(if_expr.alternative.unwrap()), env);
        } else {
            return Object::Null;
        }
    }

    fn eval_block_statement(
        &self,
        block_statement: BlockStatement,
        env: &mut Environment,
    ) -> Object {
        let mut result = Object::Null;
        for stmt in block_statement.statements {
            result = self.eval_statement(stmt, env);
            // bubble it up without unwraping the return value to handle return inside nested if
            if let Object::ReturnValue(_) = result {
                return result;
            } else if let Object::Error(_) = result {
                return result;
            }
        }
        result
    }

    fn eval_identifier(&self, ident: Identifier, env: &mut Environment) -> Object {
        if let Some(val) = env.get(&ident.value) {
            val.clone()
        } else if let Some(built_in) = self.builtins.get(&ident.value) {
            built_in.clone()
        } else {
            Object::Error(format!("identifier not found: {}", ident.value))
        }
    }

    fn eval_expressions(&self, exps: Vec<Expression>, env: &mut Environment) -> Vec<Object> {
        let mut result = vec![];

        for exp in exps {
            let evaluated = self.eval_expression(exp, env);
            if let Object::Error(_) = evaluated {
                return vec![evaluated];
            }
            result.push(evaluated)
        }

        result
    }

    fn apply_function(&self, func: Object, args: Vec<Object>) -> Object {
        match func {
            Object::Function(func_obj) => {
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
                let mut extended_env = self.extend_function_env(parameters, args, env);
                let evaluated = self.eval_statement(Statement::Block(body), &mut extended_env);
                if let Object::ReturnValue(wrapped_obj) = evaluated {
                    *wrapped_obj
                } else {
                    evaluated
                }
            }
            Object::Builtin(fn_pointer) => fn_pointer(args),
            _ => Object::Error(format!("not a function: {}", func.get_type())),
        }
    }

    fn extend_function_env(
        &self,
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

    fn eval_string_infix_expression(
        &self,
        operator: &str,
        left_val: String,
        right_val: String,
    ) -> Object {
        match operator {
            "+" => Object::String(left_val + &right_val),
            "==" => Object::Boolean(left_val == right_val),
            "!=" => Object::Boolean(left_val != right_val),
            ">" => Object::Boolean(left_val > right_val),
            "<" => Object::Boolean(left_val < right_val),
            _ => Object::Error(format!("unknown operator: STRING {} STRING", operator)),
        }
    }

    fn eval_index_expression(&self, left: Object, index: Object) -> Object {
        let left_type = left.get_type().to_string();
        let index_type = index.get_type().to_string();
        match (left, &index) {
            (Object::Array(arr), Object::Integer(intv)) => {
                self.eval_array_index_expression(arr, *intv)
            }
            (Object::Hash(hash), _) => self.eval_hash_index_expression(hash, index),
            _ => Object::Error(format!(
                "index operator not supported: {}[{}]",
                left_type, index_type
            )),
        }
    }

    fn eval_array_index_expression(&self, arr: Vec<Object>, index: i64) -> Object {
        if index < 0 || index as usize >= arr.len() {
            Object::Null
        } else {
            arr[index as usize].clone()
        }
    }

    fn eval_hash_literal(&self, hash: HashLiteral, env: &mut Environment) -> Object {
        let mut map = HashMap::new();
        for (key, value) in hash.pairs {
            let key_obj = self.eval_expression(key, env);
            if let Object::Error(_) = key_obj {
                return key_obj;
            } else if let Object::Integer(_) | Object::Boolean(_) | Object::String(_) = key_obj {
                let val_obj = self.eval_expression(value, env);
                if let Object::Error(_) = val_obj {
                    return val_obj;
                }
                map.insert(key_obj, val_obj);
            } else {
                return Object::Error(format!(
                    "This type can't be used as a key for a hashmap, got={}",
                    key_obj.get_type()
                ));
            }
        }

        Object::Hash(map)
    }

    fn eval_hash_index_expression(&self, hash: HashMap<Object, Object>, key: Object) -> Object {
        if let Object::Integer(_) | Object::Boolean(_) | Object::String(_) = key {
            let val = hash.get(&key);
            if val.is_none() {
                return Object::Null;
            } else {
                return val.unwrap().clone();
            }
        } else {
            return Object::Error(format!(
                "This type can't be used as a key for a hashmap, got={}",
                key.get_type()
            ));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    enum Expected {
        Int64(i64),
        Null,
        String(String),
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
            (
                r#"{"name": "Monkey"}[fn(x) { x }]"#,
                "This type can't be used as a key for a hashmap, got=FUNCTION",
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

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, Expected::Int64(0)),
            (r#"len("four")"#, Expected::Int64(4)),
            (r#"len("hello world")"#, Expected::Int64(11)),
            (
                r#"len(1)"#,
                Expected::String("Argument to 'len' not supported, got INTEGER".to_string()),
            ),
            (
                r#"len("one", "two")"#,
                Expected::String("Wrong number of arguments. got=2, want=1".to_string()),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Expected::Int64(val) => test_integer_object(evaluated, val),
                Expected::String(val) => {
                    if let Object::Error(msg) = evaluated {
                        assert_eq!(
                            msg, val,
                            "wrong error message. expected={}, got={}",
                            val, msg
                        );
                    } else {
                        panic!("object is not Error. got={}", evaluated);
                    }
                }
                _ => panic!("unhandled expected value"),
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        if let Object::Array(arr) = evaluated {
            assert_eq!(
                arr.len(),
                3,
                "array has wrong num of elements. got={}",
                arr.len()
            );
            test_integer_object(arr[0].clone(), 1);
            test_integer_object(arr[1].clone(), 4);
            test_integer_object(arr[2].clone(), 6);
        } else {
            panic!("object is not Array. got={}", evaluated);
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Expected::Int64(1)),
            ("[1, 2, 3][1]", Expected::Int64(2)),
            ("[1, 2, 3][2]", Expected::Int64(3)),
            ("let i = 0; [1][i]", Expected::Int64(1)),
            ("[1, 2, 3][1 + 1]", Expected::Int64(3)),
            ("let myArray = [1, 2, 3]; myArray[2]", Expected::Int64(3)),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Expected::Int64(2),
            ),
            ("[1, 2, 3][3]", Expected::Null),
            ("[1, 2, 3][-1]", Expected::Null),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Expected::Int64(v) => test_integer_object(evaluated, v),
                Expected::Null => test_null_object(evaluated),
                _ => panic!("expected value is wrong"),
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6/2,
                4: 4,
                true: 5,
                false: 6,
            }
        "#;
        let evaluated = test_eval(input);
        if let Object::Hash(hash) = evaluated {
            let mut expected = HashMap::new();
            expected.insert(Object::String("one".to_string()), 1);
            expected.insert(Object::String("two".to_string()), 2);
            expected.insert(Object::String("three".to_string()), 3);
            expected.insert(Object::Integer(4), 4);
            expected.insert(Object::Boolean(true), 5);
            expected.insert(Object::Boolean(false), 6);

            assert_eq!(
                hash.len(),
                expected.len(),
                "Hash has wrong num of pairs. got={}",
                hash.len()
            );

            for (expected_key, expected_value) in &expected {
                let hash_val = hash.get(expected_key);
                assert!(hash_val.is_some(), "no pair for given key in map");
                test_integer_object(hash_val.unwrap().clone(), *expected_value);
            }
        } else {
            panic!("eval didn't return hash. got={}", evaluated);
        }
    }

    fn test_hash_index_expression() {
        let tests = vec![
            (r#"{"foo": 5}["foo"]"#, Expected::Int64(5)),
            (r#"{"foo": 5}["bar"]"#, Expected::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Expected::Int64(5)),
            (r#"{}["foo"]"#, Expected::Null),
            (r#"{true: 5}[true]"#, Expected::Int64(5)),
            (r#"{false: 5}[false]"#, Expected::Int64(5)),
            (r#"{5: 5}[5]"#, Expected::Int64(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Expected::Int64(val) => test_integer_object(evaluated, val),
                Expected::Null => test_null_object(evaluated),
                _ => panic!("unexpected value"),
            }
        }
    }

    // helpers
    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();
        let evaluator = Evaluator::new();

        return evaluator.eval(program, &mut env);
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

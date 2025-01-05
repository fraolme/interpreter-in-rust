use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::{collections::HashMap, mem};

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

#[derive(PartialEq, Eq, Copy, Clone, PartialOrd, Debug)]
enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedence_map: HashMap<TokenType, Precedence>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut precedence_map: HashMap<TokenType, Precedence> = HashMap::new();
        precedence_map.insert(TokenType::Eq, Precedence::Equals);
        precedence_map.insert(TokenType::NotEq, Precedence::Equals);
        precedence_map.insert(TokenType::LessThan, Precedence::LessGreater);
        precedence_map.insert(TokenType::GreaterThan, Precedence::LessGreater);
        precedence_map.insert(TokenType::Plus, Precedence::Sum);
        precedence_map.insert(TokenType::Minus, Precedence::Sum);
        precedence_map.insert(TokenType::Slash, Precedence::Product);
        precedence_map.insert(TokenType::Asterisk, Precedence::Product);
        precedence_map.insert(TokenType::Lparen, Precedence::Call);

        let mut prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        prefix_parse_fns.insert(TokenType::Ident, Parser::parse_identifier);
        prefix_parse_fns.insert(TokenType::Int, Parser::parse_integer_literal);
        prefix_parse_fns.insert(TokenType::Bang, Parser::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::Minus, Parser::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::True, Parser::parse_boolean);
        prefix_parse_fns.insert(TokenType::False, Parser::parse_boolean);
        prefix_parse_fns.insert(TokenType::Lparen, Parser::parse_grouped_expression);
        prefix_parse_fns.insert(TokenType::If, Parser::parse_if_expression);
        prefix_parse_fns.insert(TokenType::Function, Parser::parse_function_literal);
        prefix_parse_fns.insert(TokenType::String, Parser::parse_string_literal);

        let mut infix_parse_fns: HashMap<TokenType, InfixParseFn> = HashMap::new();
        infix_parse_fns.insert(TokenType::Plus, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Minus, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Slash, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Asterisk, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Eq, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::NotEq, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::LessThan, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::GreaterThan, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Lparen, Parser::parse_call_expression);

        Parser {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns,
            infix_parse_fns,
            precedence_map,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token.token_type != TokenType::Eof {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let identifier_value = self.cur_token.literal.clone();
        let identifier_token = mem::take(&mut self.cur_token);

        let identifier = Identifier {
            token: identifier_token,
            value: identifier_value,
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            token: let_token,
            name: identifier,
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let return_token = mem::take(&mut self.cur_token);

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement {
            token: return_token,
            return_value,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let cur_token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        // semicolon is optional, like if 5 + 5 is typed to the REPL
        if self.peek_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            token: cur_token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        if let Some(prefix_fn) = self.prefix_parse_fns.get(&self.cur_token.token_type) {
            let mut left_exp = prefix_fn(self)?;

            while !self.peek_token_is(TokenType::SemiColon) && precedence < self.peek_precedence() {
                let peek_token_type = self.peek_token.token_type.clone();
                if !self.infix_parse_fns.contains_key(&peek_token_type) {
                    return Some(left_exp);
                }

                self.next_token();
                let infix_fn = self.infix_parse_fns.get(&peek_token_type).unwrap();
                left_exp = infix_fn(self, left_exp)?;
            }

            Some(left_exp)
        } else {
            self.errors.push(format!(
                "no prefix parse function for {} found",
                &self.cur_token.token_type
            ));

            None
        }
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let identifier_value = self.cur_token.literal.clone();
        let identifier_token = mem::take(&mut self.cur_token);

        Some(Expression::Ident(Identifier {
            token: identifier_token,
            value: identifier_value,
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let num_val: i64 = self.cur_token.literal.parse().ok()?;

        Some(Expression::Int(IntegerLiteral {
            token: mem::take(&mut self.cur_token),
            value: num_val,
        }))
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        let val = self.cur_token_is(TokenType::True);
        Some(Expression::Boolean(BooleanLiteral {
            token: mem::take(&mut self.cur_token),
            value: val,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.cur_token.literal.to_string();
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let right_expr = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(Box::new(PrefixExpression {
            token: cur_token,
            operator: operator,
            right: Box::new(right_expr),
        })))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.cur_token.literal.to_string();
        let precedence = self.cur_precedence();
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(Box::new(InfixExpression {
            token: cur_token,
            left: Box::new(left),
            operator: operator,
            right: Box::new(right),
        })))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        Some(exp)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statement();
        let mut alternative: Option<BlockStatement> = None;

        if self.peek_token_is(TokenType::Else) {
            self.next_token();
            if !self.expect_peek(TokenType::Lbrace) {
                return None;
            }

            alternative = Some(self.parse_block_statement());
        }

        return Some(Expression::If(Box::new(IfExpression {
            token: cur_token,
            condition: Box::new(condition),
            consequence,
            alternative,
        })));
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let mut stmts = vec![];
        while !self.cur_token_is(TokenType::Rbrace) && !self.cur_token_is(TokenType::Eof) {
            let stmt = self.parse_statement();
            if let Some(st) = stmt {
                stmts.push(st);
            }
            self.next_token();
        }

        BlockStatement {
            token: cur_token,
            statements: stmts,
        }
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }
        let params = self.parse_function_parameters()?;
        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Expression::Func(FunctionLiteral {
            token: cur_token,
            parameters: params,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.peek_token_is(TokenType::Rparen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let literal = self.cur_token.literal.clone();
        let ident = Identifier {
            token: mem::take(&mut self.cur_token),
            value: literal,
        };
        identifiers.push(ident);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            let literal = self.cur_token.literal.clone();
            let ident = Identifier {
                token: mem::take(&mut self.cur_token),
                value: literal,
            };
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        let arguments = self.parse_call_arguments()?;

        Some(Expression::Call(Box::new(CallExpression {
            token: cur_token,
            function: Box::new(function),
            arguments,
        })))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args: Vec<Expression> = vec![];
        if self.peek_token_is(TokenType::Rparen) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        let arg = self.parse_expression(Precedence::Lowest)?;
        args.push(arg);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let arg = self.parse_expression(Precedence::Lowest)?;
            args.push(arg);
        }

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        Some(args)
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        let value = cur_token.literal.to_string();
        Some(Expression::String(StringLiteral {
            token: cur_token,
            value,
        }))
    }

    // helper functions
    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let message = format!(
            "expected next token to be {}, got {} instead",
            token_type, self.peek_token.token_type
        );
        self.errors.push(message);
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedence_map.get(&self.peek_token.token_type) {
            *precedence
        } else {
            Precedence::Lowest
        }
    }

    fn cur_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedence_map.get(&self.cur_token.token_type) {
            *precedence
        } else {
            Precedence::Lowest
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::fmt::Write;

    enum Expected {
        Int64(i64),
        Str(String),
        Boolean(bool),
    }

    #[test]
    fn test_let_statements() {
        let tests: Vec<(&str, &str, Expected)> = vec![
            ("let x = 5;", "x", Expected::Int64(5)),
            ("let y = true;", "y", Expected::Boolean(true)),
            ("let foobar = y;", "foobar", Expected::Str("y".to_string())),
        ];

        for (input, expected_ident, expected_val) in tests {
            let lexer = Lexer::new(String::from(input));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_let_statement(&program.statements[0], expected_ident, expected_val);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&str, Expected)> = vec![
            ("return 5;", Expected::Int64(5)),
            ("return true;", Expected::Boolean(true)),
            ("return y;", Expected::Str("y".to_string())),
        ];

        for (input, expected_val) in tests {
            let lexer = Lexer::new(String::from(input));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_node_type(program.statements[0].node_type(), NodeType::Return);
            if let Statement::Return(ret_stmt) = &program.statements[0] {
                test_literal_expression(&ret_stmt.return_value, expected_val);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        let stmt_len = program.statements.len();
        assert_eq!(
            stmt_len, 1,
            "program statements length is wrong, got={stmt_len}"
        );

        let stmt = &program.statements[0];
        let node_type = stmt.node_type();
        assert_eq!(
            node_type,
            NodeType::ExpressionStatement,
            "program.statements[0] is not ast::ExpressionStatement. got = {node_type}"
        );

        if let Statement::Expression(ident_expr) = stmt {
            let node_type = ident_expr.expression.node_type();
            assert_eq!(
                node_type,
                NodeType::Identifier,
                "exp not ast::Identifier. got {node_type}"
            );

            if let Expression::Ident(ident) = &ident_expr.expression {
                let ident_val = &ident.value;
                let ident_token_literal = ident.token_literal();
                assert_eq!(
                    ident_val, "foobar",
                    "ident.value not foobar, got={ident_val}"
                );
                assert_eq!(
                    ident_token_literal, "foobar",
                    "ident.token_literal() not foobar, got={ident_token_literal}"
                );
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't have enough statements. got={}",
            program.statements.len()
        );
        assert_eq!(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
            "program.statements[0] is not NodeType::ExpressionSttaement. got={}",
            program.statements[0].node_type()
        );

        if let Statement::Expression(expr_stmt) = &program.statements[0] {
            assert_eq!(
                expr_stmt.expression.node_type(),
                NodeType::IntegerLiteral,
                "exp nit ast::IntegerLiteral. got={}",
                expr_stmt.expression.node_type()
            );

            if let Expression::Int(int_literal) = &expr_stmt.expression {
                assert_eq!(
                    int_literal.value, 5,
                    "int_literal.value not {}. got={}",
                    5, int_literal.value
                );
                assert_eq!(
                    int_literal.token_literal(),
                    "5",
                    "int_literal.token_literal not {}. got={}",
                    "5",
                    int_literal.token_literal()
                );
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests: Vec<(&str, &str, Expected)> = vec![
            ("!5;", "!", Expected::Int64(5)),
            ("-15;", "-", Expected::Int64(15)),
            ("!true;", "!", Expected::Boolean(true)),
            ("!false;", "!", Expected::Boolean(false)),
        ];

        for (input, operator, expected) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_node_type(
                program.statements[0].node_type(),
                NodeType::ExpressionStatement,
            );

            if let Statement::Expression(expr_stmt) = &program.statements[0] {
                test_node_type(expr_stmt.expression.node_type(), NodeType::PrefixExpression);

                if let Expression::Prefix(exp) = &expr_stmt.expression {
                    assert_eq!(
                        exp.operator, operator,
                        "exp.operator is not {}. got={}",
                        operator, exp.operator
                    );

                    test_literal_expression(exp.right.as_ref(), expected);
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests: Vec<(&str, Expected, &str, Expected)> = vec![
            ("5+5;", Expected::Int64(5), "+", Expected::Int64(5)),
            ("5-5;", Expected::Int64(5), "-", Expected::Int64(5)),
            ("5*5;", Expected::Int64(5), "*", Expected::Int64(5)),
            ("5/5;", Expected::Int64(5), "/", Expected::Int64(5)),
            ("5>5;", Expected::Int64(5), ">", Expected::Int64(5)),
            ("5<5;", Expected::Int64(5), "<", Expected::Int64(5)),
            ("5==5;", Expected::Int64(5), "==", Expected::Int64(5)),
            ("5!=5;", Expected::Int64(5), "!=", Expected::Int64(5)),
            (
                "true == true",
                Expected::Boolean(true),
                "==",
                Expected::Boolean(true),
            ),
            (
                "true != false",
                Expected::Boolean(true),
                "!=",
                Expected::Boolean(false),
            ),
            (
                "false == false",
                Expected::Boolean(false),
                "==",
                Expected::Boolean(false),
            ),
        ];

        for (input, left_val, operator, right_val) in infix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_node_type(
                program.statements[0].node_type(),
                NodeType::ExpressionStatement,
            );

            if let Statement::Expression(expr_stmt) = &program.statements[0] {
                test_infix_expression(&expr_stmt.expression, left_val, operator, right_val);
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 != 3 > 4", "((5 > 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.to_string();
            assert_eq!(actual, expected, "expected={}, got={}", expected, actual);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(expr_stmt) = &program.statements[0] {
            test_node_type(expr_stmt.expression.node_type(), NodeType::IfExpression);

            if let Expression::If(if_expr) = &expr_stmt.expression {
                test_infix_expression(
                    &if_expr.condition,
                    Expected::Str("x".to_string()),
                    "<",
                    Expected::Str("y".to_string()),
                );

                test_statements_len(if_expr.consequence.statements.len(), 1);
                test_node_type(
                    if_expr.consequence.statements[0].node_type(),
                    NodeType::ExpressionStatement,
                );

                if let Statement::Expression(consq) = &if_expr.consequence.statements[0] {
                    test_node_type(consq.expression.node_type(), NodeType::Identifier);
                    test_identifier(&consq.expression, "x");
                }

                assert!(
                    if_expr.alternative.is_none(),
                    "if_expr.alternative was not None. got={}",
                    if_expr.alternative.as_ref().unwrap()
                );
            }
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(expr_stmt) = &program.statements[0] {
            test_node_type(expr_stmt.expression.node_type(), NodeType::IfExpression);

            if let Expression::If(if_expr) = &expr_stmt.expression {
                test_infix_expression(
                    &if_expr.condition,
                    Expected::Str("x".to_string()),
                    "<",
                    Expected::Str("y".to_string()),
                );

                test_statements_len(if_expr.consequence.statements.len(), 1);
                test_node_type(
                    if_expr.consequence.statements[0].node_type(),
                    NodeType::ExpressionStatement,
                );

                if let Statement::Expression(consq) = &if_expr.consequence.statements[0] {
                    test_node_type(consq.expression.node_type(), NodeType::Identifier);
                    test_identifier(&consq.expression, "x");
                }

                assert!(
                    if_expr.alternative.is_some(),
                    "if_expr.alternative is not Some. got=None"
                );
                let alternative = if_expr.alternative.as_ref().unwrap();
                test_statements_len(alternative.statements.len(), 1);
                test_node_type(
                    alternative.statements[0].node_type(),
                    NodeType::ExpressionStatement,
                );

                if let Statement::Expression(alt) = &alternative.statements[0] {
                    test_node_type(alt.expression.node_type(), NodeType::Identifier);
                    test_identifier(&alt.expression, "y");
                }
            }
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::FunctionLiteral);

            if let Expression::Func(func) = &stmt.expression {
                test_statements_len(func.parameters.len(), 2);

                assert_eq!(func.parameters[0].token_literal(), "x");
                assert_eq!(func.parameters[1].token_literal(), "y");

                test_statements_len(func.body.statements.len(), 1);
                test_node_type(
                    func.body.statements[0].node_type(),
                    NodeType::ExpressionStatement,
                );

                if let Statement::Expression(expr) = &func.body.statements[0] {
                    test_infix_expression(
                        &expr.expression,
                        Expected::Str("x".to_string()),
                        "+",
                        Expected::Str("y".to_string()),
                    );
                }
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::CallExpression);

            if let Expression::Call(call) = &stmt.expression {
                test_identifier(&call.function, "add");

                test_statements_len(call.arguments.len(), 3);
                test_literal_expression(&call.arguments[0], Expected::Int64(1));
                test_infix_expression(
                    &call.arguments[1],
                    Expected::Int64(2),
                    "*",
                    Expected::Int64(3),
                );
                test_infix_expression(
                    &call.arguments[2],
                    Expected::Int64(4),
                    "+",
                    Expected::Int64(5),
                );
            }
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#" "Hello world"; "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::StringLiteral);
            if let Expression::String(sl) = &stmt.expression {
                assert_eq!(
                    sl.value, "Hello world",
                    "value not {}. got={}",
                    "Hello world", sl.value
                );
            }
        }
    }

    // helper methods
    fn check_parser_errors(parser: &Parser) {
        let error_len = parser.errors.len();

        if error_len == 0 {
            return;
        }

        let mut buffer = String::new();

        write!(&mut buffer, "parser has {} errors \n", error_len).unwrap();
        for msg in &parser.errors {
            write!(&mut buffer, "parser error: {} \n", msg).unwrap();
        }

        assert_eq!(error_len, 0, "{}", buffer);
    }

    fn test_integer_literal(int_exp: &Expression, val: i64) {
        test_node_type(int_exp.node_type(), NodeType::IntegerLiteral);

        if let Expression::Int(integ) = int_exp {
            assert_eq!(
                integ.value, val,
                "integ.value not {}. got={}",
                val, integ.value
            );

            assert_eq!(
                integ.token_literal(),
                val.to_string(),
                "integ.token_literal() not {}. got={}",
                val,
                integ.token_literal()
            );
        }
    }

    fn test_boolean_literal(bol_exp: &Expression, val: bool) {
        test_node_type(bol_exp.node_type(), NodeType::BooleanLiteral);

        if let Expression::Boolean(bol) = bol_exp {
            assert_eq!(bol.value, val, "bol.value not {}. got={}", val, bol.value);

            assert_eq!(
                bol.token_literal(),
                val.to_string(),
                "bol.token_literal() not {}. got={}",
                val,
                bol.token_literal()
            );
        }
    }

    fn test_identifier(exp: &Expression, val: &str) {
        test_node_type(exp.node_type(), NodeType::Identifier);
        if let Expression::Ident(ident) = exp {
            assert_eq!(
                ident.value, val,
                "ident.value not {}. got={}",
                val, ident.value
            );
            assert_eq!(
                ident.token_literal(),
                val,
                "ident.token_literal() not {}. got={}",
                val,
                ident.token_literal()
            );
        }
    }

    fn test_literal_expression(exp: &Expression, expected: Expected) {
        match expected {
            Expected::Int64(val) => test_integer_literal(exp, val),
            Expected::Str(val) => test_identifier(exp, &val),
            Expected::Boolean(val) => test_boolean_literal(exp, val),
        }
    }

    fn test_infix_expression(exp: &Expression, left: Expected, op: &str, right: Expected) {
        test_node_type(exp.node_type(), NodeType::InfixExpression);
        if let Expression::Infix(expr) = exp {
            test_literal_expression(&expr.left, left);
            assert_eq!(
                expr.operator, op,
                "exp.operatir is not {}. got={}",
                op, expr.operator
            );
            test_literal_expression(&expr.right, right);
        }
    }

    fn test_statements_len(stmt_len: usize, expected_len: usize) {
        assert_eq!(
            stmt_len, expected_len,
            "program.statements does not contain {} statements. got={}",
            expected_len, stmt_len
        );
    }

    fn test_node_type(node_type: NodeType, expected_node_type: NodeType) {
        assert_eq!(
            node_type, expected_node_type,
            "node is not of type {}. got={}",
            expected_node_type, node_type
        );
    }

    fn test_let_statement(stmt: &Statement, name: &str, value: Expected) {
        test_node_type(stmt.node_type(), NodeType::Let);
        if let Statement::Let(let_stmt) = stmt {
            assert_eq!(
                let_stmt.name.value, name,
                "let_stmt.name.value not {}. got={}",
                name, let_stmt.name.value
            );

            assert_eq!(
                let_stmt.name.token_literal(),
                name,
                "let_stmt.name.token_literal() not {} got={}",
                name,
                let_stmt.name.token_literal()
            );

            test_literal_expression(&let_stmt.value, value);
        }
    }
}
